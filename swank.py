##### :: swank.lisp
###
### This file defines the "Swank" TCP server for Emacs to talk to. The
### code in this file is purely portable "Common Lisp". We do require a
### smattering of non-portable functions in order to write the server,
### so we have defined them in `swank-backend.lisp' and implemented
### them separately for each Lisp implementation. These extensions are
### available to us here via the `SWANK-BACKEND' package.

#### defpackage swank
import os
import sys
import re
import ast
import select

from collections import defaultdict, UserDict

import cl

from cl import *
from pergamum import *
from more_ast import *

from cl import _servile as servile, _keyword as keyword, import_, _find_symbol0 as find_symbol0, _find_symbol_or_fail as find_symbol_or_fail, _intern0 as intern0, _defaulted as defaulted, _defaulted_to_var as defaulted_to_var

import swank_backend
import swank_python  # the thing patches swank_backend, to avoid indirection
from swank_backend import *

from swank_rpc import *

from swank_arglists import *
#### in-package swank

### Top-level variables, constants, macros: swank.lisp:74
cl_package      = find_package("CL")
keyword_package = find_package("KEYWORD")

setq("_canonical_package_nicknames_",
     [keyword("common-lisp-user"), keyword("cl-user")])
"Canonical package names to use instead of shortest name/nickname."

setq("_auto_abbreviate_dotted_packages_", t)
"Abbreviate dotted package names to their last component if T."

default_server_port = 4005
"The default TCP port for the server (when started manually)."

setq("_swank_debug_p_", t)
"When true, print extra debugging information."

### SLDB customized pprint dispatch table: swank.lisp:95
##
## CLHS 22.1.3.4, and CLHS 22.1.3.6 do not specify *PRINT-LENGTH* to
## affect the printing of strings and bit-vectors.
##
## We use a customized pprint dispatch table to do it for us.

setq("_sldb_string_length_", nil)
setq("_sldb_bitvector_length_", nil)

setq("_sldb_pprint_dispatch_table_",
     # XXX: ???
     nil)

setq("_sldb_printer_bindings_",
     [(intern0("_print_pretty_"),          t),
      (intern0("_print_level_"),           4),
      (intern0("_print_length_"),          10),
      (intern0("_print_circle_"),          t),
      (intern0("_print_readably_"),        nil),
      (intern0("_print_pprint_dispatch_"), symbol_value("_sldb_pprint_dispatch_table_")),
      (intern0("_print_gensym_"),          t),
      (intern0("_print_base_"),            10),
      (intern0("_print_radix_"),           nil),
      (intern0("_print_array_"),           t),
      (intern0("_print_lines_"),           nil),
      (intern0("_print_escape_"),          t),
      (intern0("_print_right_margin_"),    65),
      (intern0("_sldb_bitvector_length_"), 25),
      (intern0("_sldb_string_length_"),    50)])
"A set of printer variables used in the debugger."

setq("_backtrace_pprint_dispatch_table_",
     # XXX: ???
     nil)

setq("_backtrace_printer_bindings_",
     [(intern0("_print_pretty_"),          t),
      (intern0("_print_readably_"),        nil),
      (intern0("_print_level_"),           4),
      (intern0("_print_length_"),          5),
      (intern0("_print_lines_"),           1),
      (intern0("_print_right_margin_"),    200),
      (intern0("_print_pprint_dispatch_"), symbol_value("_backtrace_pprint_dispatch_table_"))])
"Pretter settings for printing backtraces."

setq("_default_worker_thread_bindings_", nil)
"""An alist to initialize dynamic variables in worker threads.
The list has the form ((VAR . VALUE) ...).  Each variable VAR will be
bound to the corresponding VALUE."""

def call_with_bindings(alist, fun):
        """Call FUN with variables bound according to ALIST.
ALIST is a list of the form ((VAR . VAL) ...)."""
        check_type(alist, (or_, list, (member_, nil)))
        if not alist:
                return fun()
        else:
                # (let* ((rlist (reverse alist))
                #        (vars (mapcar #'car rlist))
                #        (vals (mapcar #'cdr rlist)))
                #   (progv vars vals
                #    (funcall fun)))
                alist.reverse()
                vars = mapcar(lambda x: x[0], alist)
                vals = mapcar(lambda x: x[1], alist)
                return progv(vars, vals, fun)

with_bindings = call_with_bindings #### Note: was: defmacro with-bindings

## The `DEFSLIMEFUN' macro defines a function that Emacs can call via
## RPC.
#### defmacro defslimefun
##  see <http://www.franz.com/support/documentation/6.2/doc/pages/variables/compiler/s_cltl1-compile-file-toplevel-compatibility-p_s.htm>
"A DEFUN for functions that Emacs can call by RPC."

def missing_arg():
        """A function that the compiler knows will never to return a value.
You can use (MISSING-ARG) as the initform for defstruct slots that
must always be supplied. This way the :TYPE slot option need not
include some arbitrary initial value like NIL."""
        error("A required &KEY or &OPTIONAL argument was not supplied.")

### Hooks: swank.lisp:213
def add_hook(name, function):
        "Add FUNCTION to the list of values on PLACE."
        if not functionp(function):
                error(TypeError, "ADD-HOOK: second argument must be a function, was %s.", function)
        symbol_value(name).append(function)

def run_hook(funs, *args, **keys):
        "Call each of FUNCTIONS with ARGUMENTS."
        for f in funs:
                f(*args, **keys)

setq("_new_connection_hook_",    [])
"""This hook is run each time a connection is established.
The connection structure is given as the argument.
Backend code should treat the connection structure as opaque."""

setq("_connection_closed_hook_", [])
"""This hook is run when a connection is closed.
The connection as passed as an argument.
Backend code should treat the connection structure as opaque."""

setq("_pre_reply_hook_",         [])
"Hook run (without arguments) immediately before replying to an RPC."

# Issue AFTER-INIT-HOOK-NEVER-ACTIVATED
setq("_after_init_hook_",        [])
"Hook run after user init files are loaded."

### Connections: swank.lisp:245
##
## Connection structures represent the network connections between
## Emacs and Lisp. Each has a socket stream, a set of user I/O
## streams that redirect to Emacs, and optionally a second socket
## used solely to pipe user-output to Emacs (an optimization).  This
## is also the place where we keep everything that needs to be
## freed/closed/killed when we disconnect.

class connection():
        def __init__(self, socket, socket_io, communication_style, coding_system, serve_requests, cleanup):
                # The listening socket. (usually closed)
                self.socket                     = socket
                # Character I/O stream of socket connection.  Read-only to avoid
                # race conditions during initialization.
                self.socket_io                  = socket_io
                # Optional dedicated output socket (backending `user-output' slot).
                # Has a slot so that it can be closed with the connection.
                self.dedicated_output           = nil
                # Streams that can be used for user interaction, with requests
                # redirected to Emacs.
                self.user_input                 = nil
                self.user_output                = nil
                self.user_io                    = nil
                # Bindings used for this connection (usually streams)
                self.env                        = nil
                # A stream that we use for *trace-output*; if nil, we user user-output.
                self.trace_output               = nil
                # A stream where we send REPL results.
                self.repl_results               = nil
                # In multithreaded systems we delegate certain tasks to specific
                # threads. The `reader-thread' is responsible for reading network
                # requests from Emacs and sending them to the `control-thread'; the
                # `control-thread' is responsible for dispatching requests to the
                # threads that should handle them; the `repl-thread' is the one
                # that evaluates REPL expressions. The control thread dispatches
                # all REPL evaluations to the REPL thread and for other requests it
                # spawns new threads.
                self.reader_thread              = None
                self.control_thread             = None
                self.repl_thread                = None
                self.auto_flush_thread          = None
                # Callback functions:
                # (SERVE-REQUESTS <this-connection>) serves all pending requests
                # from Emacs.
                self.serve_requests             = serve_requests
                # (CLEANUP <this-connection>) is called when the connection is
                # closed.
                self.cleanup                    = cleanup
                # Cache of macro-indentation information that has been sent to Emacs.
                # This is used for preparing deltas to update Emacs's knowledge.
                # Maps: symbol -> indentation-specification
                self.indentation_cache          = dict()
                # The list of packages represented in the cache:
                self.indentation_cache_packages = []
                # The communication style used.
                self.communication_style        = communication_style
                # The coding system for network streams.
                self.coding_system              = coding_system
                # The SIGINT handler we should restore when the connection is
                # closed.
                self.saved_sigint_handler       = None

def print_connection(conn, stream, depth):
        return print_unreadable_object(conn, stream, type = t, identity = t)

setq("_connections_",      [])
"List of all active connections, with the most recent at the front."

setq("_emacs_connection_", nil)
"The connection to Emacs currently in use."

def default_connection():
        """Return the 'default' Emacs connection.
This connection can be used to talk with Emacs when no specific
connection is in use, i.e. *EMACS-CONNECTION* is NIL.

The default connection is defined (quite arbitrarily) as the most
recently established one."""
        return symbol_value("_connections_")[0]

def make_connection(socket, stream, style, coding_system):
        serve, cleanup = ((spawn_threads_for_connection, cleanup_connection_threads) if style is keyword("spawn") else
                          (install_sigio_handler, deinstall_sigio_handler) if style is keyword("sigio") else
                          (install_fd_handler, deinstall_fd_handler) if style is keyword("fd-handler") else
                          (simple_serve_requests, nil))
        conn = connection(socket = socket,
                          socket_io = stream,
                          communication_style = style,
                          coding_system = coding_system,
                          serve_requests = serve,
                          cleanup = cleanup)
        run_hook(symbol_value("_new_connection_hook_"), conn)
        symbol_value("_connections_").append(conn)
        return conn

def connection_external_format(conn):
        return ignore_errors(
                lambda:
                        stream_external_format(conn.socket_io))

def ping(tag):
        return tag

def safe_backtrace():
        return ignore_errors(
                lambda:
                        call_with_debugging_environment(
                        lambda:
                                backtrace(0, nil)))

class swank_error(error_):
        "Condition which carries a backtrace."
        def __init__(self, backtrace = None, condition = None):
                self.backtrace, self.condition = backtrace, condition
        def __str__(self):
                return str(self.condition)

def make_swank_error(condition, backtrace = None):
        return swank_error(condition = condition,
                           backtrace = (safe_backtrace() if backtrace is None else
                                        backtrace))

setq("_debug_on_swank_protocol_error_", nil)
"""When non-nil invoke the system debugger on errors that were
signalled during decoding/encoding the wire protocol.  Do not set this
to T unless you want to debug swank internals."""

def with_swank_error_handler(conn, body):
        "Close the connection on internal `swank-error's."
        def handler_case_body():
                return handler_bind(
                        body,
                        (swank_error,
                         (lambda condition:
                           (symbol_value("_debug_on_swank_protocol_error_") and
                                        invoke_default_debugger(condition)))))
        return handler_case(handler_case_body,
                            (swank_error,
                             (lambda condition:
                               close_connection(conn,
                                                condition.condition,
                                                condition.backtrace))))

def with_panic_handler(conn, body):
        "Close the connection on unhandled `serious-condition's."
        return handler_bind(body,
                            (serious_condition,
                             (lambda condition:
                               close_connection(conn,
                                                condition,
                                                safe_backtrace()))))

def notify_backend_of_connection(conn):
        return emacs_connected()
add_hook("_new_connection_hook_", notify_backend_of_connection)

### Utilities: swank.lisp:406
### Logging: swank.lisp:409

t_, nil_, or_, some_, quote_ = mapcar(lambda s: find_symbol_or_fail(s, "CL"),
                                      ["T", "NIL", "OR", "SOME", "QUOTE"])

setq("_swank_io_package_", lret(make_package("SWANK-IO-PACKAGE"), # MAKE-PACKAGE is due to ignore_python = True
                                lambda package: import_([t_, nil_, quote_], package)))

setq("_log_events_", nil)
setq("_log_output_", nil)

def init_log_output():
        if not symbol_value("_log_output_"):
                setq("_log_output_", real_output_stream(symbol_value("_error_output_")))

add_hook("_after_init_hook_", init_log_output)

def real_input_stream(x):
        return typecase(x,
                        (synonym_stream, lambda:
                                 real_input_stream(symbol_value(synonym_stream_symbol(x)))),
                        (two_way_stream, lambda:
                                 real_input_stream(two_way_stream_input_stream(x))),
                        (t,              lambda:
                                 x))

def real_output_stream(x):
        return typecase(x,
                        (synonym_stream, lambda:
                                 real_output_stream(symbol_value(synonym_stream_symbol(x)))),
                        (two_way_stream, lambda:
                                 real_output_stream(two_way_stream_output_stream(x))),
                        (t,              lambda:
                                 x))

setq("_event_history_",        make_list(40))
"A ring buffer to record events for better error messages."
setq("_event_history_index_",  0)
setq("_enable_event_history_", t)

def log_event(format_string, *args):
        """Write a message to *terminal-io* when *log-events* is non-nil.
Useful for low level debugging."""
        def wsios_body():
                with progv(_print_readably_ = nil,
                           _print_pretty_   = nil,
                           _package_        = symbol_value("_swank_io_package_")):
                        if symbol_value("_enable_event_history_"):
                                symbol_value("_event_history_")[symbol_value("_event_history_index_")] = format(nil, format_string, *args)
                                setq("_event_history_index_",
                                     (symbol_value("_event_history_index_") + 1) % len(symbol_value("_event_history_")))
                                if symbol_value("_log_events_"):
                                        write_string(escape_non_ascii(format(nil, format_string, *args)), # XXX: was (format nil "~?" format-string args)
                                                     symbol_value("_log_output_"))
                                        force_output(symbol_value("_log_output_"))
        with_standard_io_syntax(wsios_body)

def event_history_to_list():
        "Return the list of events (older events first)."
        arr, idx = symbol_value("_event_history_"), symbol_value("_event_history_index_")
        return arr[idx:] + arr[:idx]

def clear_event_history():
        arr = symbol_value("_event_history_")
        for i in range(len(arr)):
                arr[i] = nil
        setq("_event_history_index_", 0)

def dump_event_history(stream):
        mapc(lambda e: dump_event(e, stream), event_history_to_list())

def dump_event(event, stream):
        if   stringp(event): write_string(event, stream)
        elif not event:      pass
        else:                write_string(escape_non_ascii(format(nil, "Unexpected event: %s\n", event)),
                                          stream)

def escape_non_ascii(string):
        "Return a string like STRING but with non-ascii chars escaped."
        return (string if ascii_string_p(string) else
                with_output_to_string(lambda out:
                                              mapc(lambda c: (write_string(c, out) if ascii_char_p(c) else
                                                              format(out, r"x%04x", ord(c))),
                                                   string)))

def ascii_string_p(o):
        return stringp(o) and every(ascii_char_p, o)

def ascii_char_p(o):
        return ord(o) <= 127

### Helper macros: swank.lisp:502

def destructure_case(x, *clauses):
        """Dispatch VALUE to one of PATTERNS.
A cross between `case' and `destructuring-bind'.
The pattern syntax is:
  ((HEAD . ARGS) . BODY)
The list of patterns is searched for a HEAD `eq' to the car of
VALUE. If one is found, the BODY is executed with ARGS bound to the
corresponding values in the CDR of VALUE."""
        # format(t, "D/C: %s\n", x)
        op, body = x[0], x[1:]
        # format(t, "clauses:\n")
        # for c in clauses:
        #         format(t, "    %s\n", c)
        for struc, action in clauses:
                cop, cbody = struc[0], struc[1:]
                if cop is t or cop is op or (setp(cop) and op in cop):
                        return action(*body)
                # else:
                #         format(t, "%s is not %s, but %s == %s: %s\n", 
                #                cop, op, cop, op, cop == op)
        else:
                error("DESTRUCTURE-CASE failed: %s", x)

## If true execute interrupts, otherwise queue them.
## Note: `with-connection' binds *pending-slime-interrupts*.
# setq("_slime_interrupts_enabled_", <unbound>) 

#### defmacro with-interrupts-enabled% -- inlined into with/out_slime_interrupts

def with_slime_interrupts(body):
        check_slime_interrupts()
        with progv(_slime_interrupts_enabled_ = t):
                ret = body()
        check_slime_interrupts()
        return ret

def without_slime_interrupts(body):
        with progv(_slime_interrupts_enabled_ = nil):
                return body()

#### invoke-or-queue-interrupt

def with_io_redirection(conn, body):
        return with_bindings(conn.env, body)

def with_connection(conn, body):
        assert(typep(conn, connection))
        if symbol_value("_emacs_connection_") is conn:
                return body()
        else:
                with progv(_emacs_connection_ = conn,
                           _pending_slime_interrupts_ = []):
                        return without_slime_interrupts(
                                lambda: with_swank_error_handler(
                                        conn,
                                        lambda: with_io_redirection(
                                                conn,
                                                lambda: call_with_debugger_hook(
                                                        swank_debugger_hook,
                                                        body))))

@block
def call_with_retry_restart(thunk, msg = "Retry"):
        def body():
                with_simple_restart("RETRY", msg,
                                    lambda: return_from(call_with_retry_restart, thunk()))
        loop(body)

with_retry_restart = call_with_retry_restart # Was: defmacro

#### with-struct*
#### do-symbols*
# UNUSABLE define-special

### Misc: swank.lisp:624
def use_threads_p():
        return symbol_value("_emacs_connection_").communication_style is keyword("spawn")

def current_thread_id():
        return thread_id(current_thread())

#### ensure-list -- implemented in cl.py, proxied in pergamum.py

### Symbols: swank.lisp:637
#### symbol-status
"""Returns one of 

  :INTERNAL  if the symbol is _present_ in PACKAGE as an _internal_ symbol,

  :EXTERNAL  if the symbol is _present_ in PACKAGE as an _external_ symbol,

  :INHERITED if the symbol is _inherited_ by PACKAGE through USE-PACKAGE,
             but is not _present_ in PACKAGE,

  or NIL     if SYMBOL is not _accessible_ in PACKAGE.


Be aware not to get confused with :INTERNAL and how \"internal
symbols\" are defined in the spec; there is a slight mismatch of
definition with the Spec and what's commonly meant when talking
about internal symbols most times. As the spec says:

  In a package P, a symbol S is
  
     _accessible_  if S is either _present_ in P itself or was
                   inherited from another package Q (which implies
                   that S is _external_ in Q.)
  
        You can check that with: (AND (SYMBOL-STATUS S P) T)
  
  
     _present_     if either P is the /home package/ of S or S has been
                   imported into P or exported from P by IMPORT, or
                   EXPORT respectively.
  
                   Or more simply, if S is not _inherited_.
  
        You can check that with: (LET ((STATUS (SYMBOL-STATUS S P)))
                                   (AND STATUS 
                                        (NOT (EQ STATUS :INHERITED))))
  
  
     _external_    if S is going to be inherited into any package that
                   /uses/ P by means of USE-PACKAGE, MAKE-PACKAGE, or
                   DEFPACKAGE.
  
                   Note that _external_ implies _present_, since to
                   make a symbol _external_, you'd have to use EXPORT
                   which will automatically make the symbol _present_.
  
        You can check that with: (EQ (SYMBOL-STATUS S P) :EXTERNAL)
  
  
     _internal_    if S is _accessible_ but not _external_.

        You can check that with: (LET ((STATUS (SYMBOL-STATUS S P)))
                                   (AND STATUS 
                                        (NOT (EQ STATUS :EXTERNAL))))
  

        Notice that this is *different* to
                                 (EQ (SYMBOL-STATUS S P) :INTERNAL)
        because what the spec considers _internal_ is split up into two
        explicit pieces: :INTERNAL, and :INHERITED; just as, for instance,
        CL:FIND-SYMBOL does. 

        The rationale is that most times when you speak about \"internal\"
        symbols, you're actually not including the symbols inherited 
        from other packages, but only about the symbols directly specific
        to the package in question.
"""
#### symbol-external-p
"""True if SYMBOL is external in PACKAGE.
If PACKAGE is not specified, the home package of SYMBOL is used."""
#### classify-symbol
"""Returns a list of classifiers that classify SYMBOL according to its
underneath objects (e.g. :BOUNDP if SYMBOL constitutes a special
variable.) The list may contain the following classification
keywords: :BOUNDP, :FBOUNDP, :CONSTANT, :GENERIC-FUNCTION,
:TYPESPEC, :CLASS, :MACRO, :SPECIAL-OPERATOR, and/or :PACKAGE"""
#### symbol-classification-string
"""Return a string in the form -f-c---- where each letter stands for
boundp fboundp generic-function class macro special-operator package"""

### TCP Server: swank.lisp:769
#### defvar *use-dedicated-output-stream*         # implementation in partus.py
#### defvar *dedicated-output-stream-port*        # implementation in partus.py
#### defvar *communication-style*                 # implementation in partus.py
#### defvar *dont-close*                          # implementation in partus.py
#### defvar *dedicated-output-stream-buffering*   # implementation in partus.py
#### defvar *coding-system*                       # implementation in partus.py
#### defvar *listener-sockets*                    # implementation in partus.py
#### start-server                                 # implementation in partus.py
#### create-server                                # implementation in partus.py
#### find-external-format-or-lose                 # implementation in partus.py
#### defparameter *loopback-interface*            # implementation in partus.py
#### setup-server                                 # implementation in partus.py
#### stop-server                                  # implementation in partus.py
#### restart-server                               # implementation in partus.py
#### accept-connections                           # implementation in partus.py
#### authenticate-client                          # implementation in partus.py     
#### slime-secret                                 # implementation in partus.py
#### serve-requests                               # implementation in partus.py
#### announce-server-port                         # implementation in partus.py
#### simple-announce-function                     # implementation in partus.py

def open_streams(conn):
        """Return the 5 streams for IO redirection:
DEDICATED-OUTPUT INPUT OUTPUT IO REPL-RESULTS"""
        input_fn = lambda: with_connection(conn,
                                           lambda: with_simple_restart("ABORT-READ", "Abort reading input from Emacs.",
                                                                       read_user_input_from_emacs))
        dedicated_output = when(symbol_value("_swank_debug_p_"),
                                lambda: open_dedicated_output_stream(conn.socket_io))
        in_ = make_input_stream(input_fn)
        out = dedicated_output or make_output_stream(make_output_function(conn))
        io = make_two_way_stream(in_, out)
        repl_results = make_output_stream_for_target(conn, keyword("repl-result"))
        if conn.communication_style is keyword("spawn"):
                conn.auto_flush_thread = spawn(lambda: auto_flush_loop(out),
                                               name = "auto-flush-thread")
        return dedicated_output, in_, out, io, repl_results

def make_output_function(conn):
        "Create function to send user output to Emacs."
        i, tag, l = 0, 0, 0
        def set_i_tag_l(x): nonlocal i, tag, l; i, tag, l = x
        return (lambda string:
                        with_connection(conn,
                                        set_i_tag_l(send_user_output(string, i, tag, l))))

setq("_maximum_pipelined_output_chunks_", 50)
setq("_maximum_pipelined_output_length_", 80 * 20 * 5)

def send_user_output(string, pcount, tag, plength):
        # send output with flow control
        if (pcount  > symbol_value("_maximum_pipelined_output_chunks_") or
            plength > symbol_value("_maximum_pipelined_output_length_")):
                tag = (tag + 1) % 1000
                send_to_emacs([keyword("ping"), current_thread_id(), tag])
                with_simple_restart("ABORT", "Abort sending output to Emacs.",
                                    lambda: wait_for_event([keyword("emacs-pong"), tag]))
                pcount, plength = 0
        send_to_emacs([keyword("write-string"), string])
        return pcount + 1, tag, plength + len(string)

def make_output_function_for_target(conn, target):
        "Create a function to send user output to a specific TARGET in Emacs."
        return (lambda string:
                        with_connection(conn,
                                        lambda: with_simple_restart("ABORT", "Abort sending output to Emacs.",
                                                                    lambda: send_to_emacs([keyword("write-string", string, target)]))))

def make_output_stream_for_target(conn, target):
        "Create a stream that sends output to a specific TARGET in Emacs."
        return make_output_stream(make_output_function_for_target(conn, target))

def open_dedicated_output_stream(socket_io):
        """Open a dedicated output connection to the Emacs on SOCKET-IO.
Return an output stream suitable for writing program output.

This is an optimized way for Lisp to deliver output to Emacs."""
        socket = create_socket(symbol_value("_loopback_interface_"),
                               symbol_value("_dedicated_output_stream_port_"))
        try:
                port = local_port(socket)
                encode_message([keyword("open-dedicated-output-stream"), port], socket_io)
                dedicated = accept_connection(socket,
                                              external_format = ignore_errors(
                                lambda:
                                        string(stream_external_format(socket_io))) or "default", # was: keyword("default")
                                              buffering = symbol_value("_dedicated_output_stream_buffering_"),
                                              timeout = 30)
                close_socket(socket)
                socket = None
                return dedicated
        finally:
                if socket:
                        close_socket(socket)

### Event Decoding/Encoding: swank.lisp:1008

def decode_message(stream):
        "Read an S-expression from STREAM using the SLIME protocol."
        log_event("decode_message\n")
        return without_slime_interrupts(
                lambda: handler_bind(
                        lambda: handler_case(
                                lambda: read_message(stream,
                                                     symbol_value("_swank_io_package_")),
                                (swank_reader_error,
                                 lambda c: [keyword("reader-error"), c.packet, c.cause])),
                        (error_,
                         lambda c: error(make_swank_error(c)))))

def encode_message(message, stream):
        "Write an S-expression to STREAM using the SLIME protocol."
        log_event("encode_message\n")
        return without_slime_interrupts(
                lambda:
                        handler_bind(lambda: write_message(message,
                                                           symbol_value("_swank_io_package_"),
                                                           stream),
                                     (error_,
                                      lambda c: error(make_swank_error(c)))))

### Event Processing: swank.lisp:1028
setq("_sldb_quit_restart_", nil)
"The restart that will be invoked when the user calls sldb-quit."

def with_top_level_restart(conn, restart_fn, body):
        def restart_case_body():
                with progv(_sldb_quit_restart_ = find_restart("ABORT")):
                        return body()
        return with_connection(
                conn,
                lambda: restart_case(restart_case_body,
                                     abort = ((lambda v = None:
                                                       force_user_output() and restart_fn()),
                                              dict(report = "Return to SLIME's top level."))))

## Aka REPL-LOOP.  Yeah, really.
def handle_requests(conn, timeout = nil):
        """Read and process :emacs-rex requests.
The processing is done in the extent of the toplevel restart."""
        # The point is that this dualism hurts.
        # REPL-LOOP-HANDLE-REQUESTS would've been a better name.
        @block
        def tag_body():
                # was:
                # (tagbody
                #   start
                #   (with-top-level-restart (connection (go start))
                #     (process-requests timeout)))
                @block
                def inner():
                        return_from(tag_body,
                                    with_top_level_restart(conn,
                                                           lambda: return_from(inner, nil),
                                                           lambda: process_requests(timeout)))
                while True:
                        inner()
        with_connection(
                conn,
                lambda: (process_requests(timeout) if symbol_value("_sldb_quit_restart_") else
                         tag_body()))

## Aka HANDLE-REQUESTS, aka REPL-LOOP.  Yeah, really.
@block
def process_requests(timeout):
        "Read and process requests from Emacs."
        def body():
                event, timeoutp = wait_for_event([or_,
                                                  [keyword("emacs-rex"), ],        # XXX: (:emacs-rex . _)
                                                  [keyword("emacs-channel-send")]]) # XXX: (:emacs-channel-send . _)
                if timeoutp:
                        return_from(process_requests, nil)
                destructure_case(
                        event,
                        ([keyword("emacs-rex")],
                          eval_for_emacs),
                        ([keyword("emacs-channel-send")],
                          lambda channel, selector, *args:
                                  channel_send(channel, selector, args)))
        return loop(body)

def current_socket_io():
        return symbol_value("_emacs_connection_").socket_io

def close_connection(c, condition, backtrace):
        with progv(_debugger_hook_ = nil):
                log_event("close-connection %s ...\n", condition)
        format(symbol_value("_log_output_"), "\n;; swank:close_connection: %s\n", condition)
        cleanup = c.cleanup
        if cleanup:
                cleanup(c)
        close(c.socket_io)
        if c.dedicated_output:
                close(c.dedicated_output)
        setq("_connections_", remove(c, symbol_value("_connections_")))
        run_hook(symbol_value("_connection_closed_hook_"), c)
        if condition and True: # XXX: was (not (typep condition 'end-of-file))
                finish_output(symbol_value("_log_output_"))
                format(symbol_value("_log_output_"), "\n;; Event history start:\n")
                dump_event_history(symbol_value("_log_output_"))
                format(symbol_value("_log_output_"),
                       """;; Event history end.
;; Backtrace:
%s
;; Connection to Emacs lost.
;;  condition: %s
;;  type: %s
;;  encoding: %s vs. %s
;;  style: %s dedicated: %s
""",
                       backtrace,
                       escape_non_ascii(safe_condition_message(condition)),
                       type_of(condition),
                       c.coding_system,
                       connection_external_format(c),
                       c.communication_style,
                       symbol_value("_use_dedicated_output_stream_"))
                finish_output(symbol_value("_log_output_"))
        log_event("close-connection %s ... done.\n", condition)

### Thread based communication: swank.lisp:1107
setq("_active_threads_", [])

def read_loop(conn):
        input_stream, control_thread = conn.socket_io, conn.control_thread
        with_swank_error_handler(conn,
                                 lambda:
                                         loop(lambda: send(control_thread, decode_message(input_stream))))

# Aka CONTROL-THREAD, really.
def dispatch_loop(conn):
        with progv(_emacs_connection_ = conn):
                with_panic_handler(conn,
                                   lambda:
                                           loop(lambda: dispatch_event(receive()[0]))) # WARNING: multiple values!

setq("_auto_flush_interval_", 0.5)

@block
def auto_flush_loop(stream):
        def body():
                if not (open_stream_p(stream) and output_stream_p(stream)):
                        return_from(auto_flush_loop, nil)
                ## Use an IO timeout to avoid deadlocks
                ## on the stream we're flushing.
                call_with_io_timeout(
                        lambda: finish_output(stream),
                        seconds = 0.1)
                sleep(symbol_value("_auto_flush_interval_"))
        loop(body)

def find_repl_thread(conn):
        if not use_threads_p():
                return current_thread()
        else:
                thread = conn.repl_thread
                if not thread:
                        pass
                elif thread_alive_p(thread):
                        return thread
                else:
                        conn.repl_thread = spawn_repl_thread(conn, "new-repl-thread")
                        return conn.repl_thread

def find_worker_thread(id):
        if id is t:
                return first(symbol_value("_active_threads_"))
        elif id is keyword("repl-thread"):
                return find_repl_thread(symbol_value("_emacs_connection_"))
        elif integerp(id):
                return find_thread(id)
        else:
                error(TypeError, "FIND-WORKER-THREAD: id must be one of: T, :REPL-THREAD or a fixnum, was: %s" % id)

def interrupt_worker_thread(id):
        thread = (find_worker_thread(id) or
                  find_repl_thread(symbol_value("_emacs_connection_")) or
                  # FIXME: to something better here
                  spawn(lambda: None, name = "ephemeral"))
        log_event("interrupt_worker_thread: %s %s\n", id, thread)
        assert(thread)
        if use_threads_p():
                interrupt_thread(thread,
                                 lambda:
                                         ## safely interrupt THREAD
                                         invoke_or_queue_interrupt(simple_break))
        else:
                simple_break()

def thread_for_evaluation(id):
        "Find or create a thread to evaluate the next request."
        c = symbol_value("_emacs_connection_")
        if id is t:
                return (spawn_worker_thread(c) if use_threads_p else
                        current_thread())
        elif id is keyword("repl-thread"):
                return find_repl_thread(c)
        elif integerp(id):
                return find_thread(id)
        else:
                error(TypeError, "THREAD-FOR-EVALUATION: id must be one of: T, :REPL-THREAD or a fixnum, was: %s" % id)

def worker_thread(conn):
        "XXX: heresy!  This is not a true REPLica!"
        with_bindings(symbol_value("_default_worker_thread_bindings_"),
                      lambda:
                              with_top_level_restart(
                        conn, nil,
                        lambda:
                                eval_for_emacs(*wait_for_event([keyword("emacs-rex"),
                                                                # XXX: was: :emacs-rex . _
                                                                ])[0][1:])))

def spawn_worker_thread(conn):
        return spawn(lambda: worker_thread(conn), name = "worker")

def spawn_repl_thread(conn, name):
        return spawn(lambda:
                             with_bindings(symbol_value("_default_worker_thread_bindings_"),
                                           lambda: repl_loop(conn)),
                     name = name)

# Aka CONTROL-THREAD iteration, really.
def dispatch_event(event):
        "Handle an event triggered either by Emacs or within Lisp."
        log_event("dispatch_event: %s\n", event)
        def emacs_rex(form, package, thread_id, id):
                thread = thread_for_evaluation(thread_id) # A worker thread, in most cases.
                if thread:
                        symbol_value("_active_threads_").append(thread)
                        return send_event(thread,
                                          [keyword("emacs-rex"), form, package, id])
                else:
                        return encode_message([keyword("invalid-rpc"), id,
                                               format(nil, "Thread not found: %s", thread_id)],
                                              current_socket_io())
        def return_(thread, *args, **keys):
                tail = member(thread, symbol_value("_active_threads_"))
                setq("_active_threads_",
                     ldiff(symbol_value("_active_threads_"), tail) +
                     rest(tail))
                return encode_message([keyword("return")] + list(args), current_socket_io())
        destructure_case(
                event,
                ([keyword("emacs-rex")],
                 emacs_rex),
                ([keyword("return")],
                 return_),
                ([keyword("emacs-interrupt")],
                 lambda thread_id:
                         interrup_worker_thread(thread_id)),
                ([set([keyword("write-string"),
                       keyword("debug"),
                       keyword("debug-condition"),
                       keyword("debug-activate"),
                       keyword("debug-return"),
                       keyword("channel-send"),
                       keyword("presentation-start"),
                       keyword("presentation-end"),
                       keyword("new-package"),
                       keyword("new-features"),
                       keyword("ed"),
                       keyword("indentation-update"),
                       keyword("eval"),
                       keyword("eval-no-wait"),
                       keyword("background-message"),
                       keyword("inspect"),
                       keyword("ping"),
                       keyword("y-or-n-p"),
                       keyword("read-from-minibuffer"),
                       keyword("read-string"),
                       keyword("read-aborted"),
                       ])],
                 lambda *_, **__:
                         encode_message(event, current_socket_io())),
                ([set([keyword("emacs-pong"),
                       keyword("emacs-return"),
                       keyword("emacs-return-string")])],
                 lambda thread_id, *args, **keys:
                         send_event(find_thread(thread_id),
                                    [first(event)] + args)), # XXX: keys? linearise?
                ([keyword("emacs-channel-send")],
                 lambda channel_id, msg:
                         let(find_channel(channel_id),
                             lambda ch:
                                     send_event(channel_thread(ch),
                                                [keyword("emacs-channel-send"), ch, msg]))),
                ([keyword("reader-error")],
                 lambda packet, condition:
                         encode_message([keyword("reader-error"), packet,
                                         safe_condition_message(condition)],
                                        current_socket_io())))

setq("_event_queue_",     [])
setq("_events_enqueued_", 0)

def send_event(thread, event):
        log_event("send-event: %s %s\n", thread, event)
        if use_threads_p:
                send(thread, event)
        else:
                symbol_value("_event_queue_").append(event)
                setq("_events_enqueued_",
                     (symbol_value("_events_enqueued_") + 1) % most_positive_fixnum)

def send_to_emacs(event):
        "Send EVENT to Emacs."
        # log_event("send-to-emacs: %s %s\n", event)
        if use_threads_p():
                send (symbol_value ("_emacs_connection_").control_thread, event)
        else:
                dispatch_event (event)

def wait_for_event(pattern, timeout = nil):
        """Scan the event queue for PATTERN and return the event.
If TIMEOUT is 'nil wait until a matching event is enqued.
If TIMEOUT is 't only scan the queue without waiting.
The second return value is t if the timeout expired before a matching
event was found."""
        # Warning: returns multiple values!
        log_event("wait_for_event: %s %s\n", pattern, timeout)
        ret = without_slime_interrupts(
                lambda: (receive_if(lambda e: event_match_p(e, pattern), timeout) if use_threads_p() else
                         wait_for_event_event_loop(pattern, timeout)))
        # here("returning %s" % (ret,))
        return ret

@block
def wait_for_event_event_loop(pattern, timeout):
        # Warning: not tested (which is somewhat irrelevant, because use_threads_p() -> T)
        if timeout and timeout is not t:
                error(simple_type_error, "WAIT-FOR-EVENT-LOOP: timeout must be NIL or T, was: %s.", timeout)
        def body():
                cl._backtrace()
                check_slime_interrupts()
                event = poll_for_event(pattern)
                if event:
                        return_from(wait_for_event_event_loop, first(event))
                events_enqueued = symbol_value("_events_enqueued_")
                ready = wait_for_input([current_socket_io()], timeout)
                if timeout and not ready:
                        return_from(wait_for_event_event_loop, (nil, t))
                elif (events_enqueued != symbol_value("_events_enqueued_") or
                      ready is keyword("interrupt")):
                        # rescan event queue, interrupts may enqueue new events
                        pass
                else:
                        assert(ready == [current_socket_io()])
                        dispatch_event(decode_message(current_socket_io()))
        loop(body)

def poll_for_event(pattern):
        tail = member_if(lambda e: event_match_p(e, pattern),
                         symbol_value("_event_queue_"))
        if tail:
                setq("_event_queue_",
                     ldiff(symbol_value("_event_queue_"), tail) +
                     rest(tail))
                return tail

# FIXME: Make this use SWANK-MATCH.
def event_match_p(event, pattern):
        if (keywordp(pattern) or numberp(pattern) or stringp(pattern) or
            pattern is t or pattern is nil):
                # here("matching ev %s against pat %s: %s" % (event, pattern, event == pattern))
                return event == pattern
        elif symbolp(pattern):
                # here("matching ev %s against pat %s" % (event, pattern))
                return t # Ostensibly suspicious, but it's the original logic..
        elif listp(pattern):
                # here("matching ev %s against pat %s" % (event, pattern))
                f = pattern[0] # XXX: symbols or strings?
                # here("hit LIST: %s vs. %s of type %s" % (pattern, event[0], type_of(event[0])))
                if f is or_:
                        # here("hit OR")
                        return some(lambda p: event_match_p(event, p), rest(pattern))
                else:
                        return (listp(event) and
                                event_match_p(first(event), first(pattern)) and
                                True # XXX: priority override!
                                # event_match_p(rest(event), rest(pattern))
                                )
        else:
                error("Invalid pattern: %s.", pattern)

def spawn_threads_for_connection(conn):
        conn.control_thread = spawn(lambda: control_thread(conn),
                                    name = "control-thread")
        return conn

def control_thread(conn):
        conn.control_thread = current_thread()
        conn.reader_thread  = spawn(lambda: read_loop(conn),
                                    name = "reader-thread")
        dispatch_loop(conn)

def cleanup_connection_threads(conn):
        threads = [conn.repl_thread,
                   conn.reader_thread,
                   conn.control_thread,
                   conn.auto_flush_thread]
        for thread in threads:
                here("killing %s", thread)
                if (thread and
                    thread_alive_p(thread) and
                    thread is not current_thread()):
                        kill_thread(thread)

def repl_loop(conn):
        handle_requests(conn)

### Signal driven IO: swank.lisp:1333
#### install-sigio-handler
#### defvar *io-interupt-level*
#### process-io-interrupt
#### deinstall-sigio-handler

### SERVE-EVENT based IO: swank.lisp:1354
#### install-fd-handler
#### dispatch-interrupt-event
#### deinstall-fd-handler

### Simple sequential IO: swank.lisp:1377
#### simple-serve-requests
## this is signalled when our custom stream thinks the end-of-file is reached.
## (not when the end-of-file on the socket is reached)
#### define-condition end-of-repl-input
#### simple-repl
#### make-repl-input-stream
#### repl-input-stream-read
#### read-non-blocking

### IO to Emacs: swank.lisp:1447
##
## This code handles redirection of the standard I/O streams
## (`*standard-output*', etc) into Emacs. The `connection' structure
## contains the appropriate streams, so all we have to do is make the
## right bindings.
##
### Global I/O redirection framework: swank.lisp:1454
##
## Optionally, the top-level global bindings of the standard streams
## can be assigned to be redirected to Emacs. When Emacs connects we
## redirect the streams into the connection, and they keep going into
## that connection even if more are established. If the connection
## handling the streams closes then another is chosen, or if there
## are no connections then we revert to the original (real) streams.
##
## It is slightly tricky to assign the global values of standard
## streams because they are often shadowed by dynamic bindings. We
## solve this problem by introducing an extra indirection via synonym
## streams, so that *STANDARD-INPUT* is a synonym stream to
## *CURRENT-STANDARD-INPUT*, etc. We never shadow the "current"
## variables, so they can always be assigned to affect a global
## change.
##

setq("_globally_redirect_io_", nil)
"When non-nil globally redirect all standard streams to Emacs."

### Global redirection setup: swank.lisp:1474

setq("_saved_global_streams_", [])
"""A plist to save and restore redirected stream objects.
E.g. the value for '*STANDARD-OUTPUT* holds the stream object
for _standard_output_ before we install our redirection."""

def setup_stream_indirection(stream_var, stream = None):
        """Setup redirection scaffolding for a global stream variable.
Supposing (for example) STREAM-VAR is *STANDARD-INPUT*, this macro:

1. Saves the value of *STANDARD-INPUT* in `*SAVED-GLOBAL-STREAMS*'.

2. Creates *CURRENT-STANDAR-INPUT*, initially with the same value as
*STANDARD-INPUT*.

3. Assigns *STANDARD-INPUT* to a synonym stream pointing to
*CURRENT-STANDARD-INPUT*.

This has the effect of making *CURRENT-STANDARD-INPUT* contain the
effective global value for *STANDARD-INPUT*. This way we can assign
the effective global value even when *STANDARD-INPUT* is shadowed by a
dynamic binding."""
        current_stream_var = prefixed_var("#:current", stream_var)
        stream = stream or symbol_value(stream_var)
        # Save the real stream value for the future.
        symbol_value("_saved_global_streams_")[stream_var] = stream
        # Define a new variable for the effective stream.
        # This can be reassigned.
        # XXX: proclaim `(special ,current_stream_var)
        setq(current_stream_var, stream)
        # Assign the real binding as a synonym for the current one.
        stream = make_synonym_stream(current_stream_var)
        setq(stream_var, stream)
        set_default_initial_binding(stream_var, [quote_, stream])

def prefixed_var(prefix, variable_symbol):
        "(PREFIXED_VAR \"FOO\" '*BAR*) => SWANK::*FOO-BAR*"
        basename = subseq(symbol_name(variable_symbol), 1)
        return intern_(format(nil, "_%s_%s", string(prefix), basename), # "*~A-~A"
                       keyword("swank"))

# Issue SPECIAL-BINDINGS-ARE-PACKAGE-LESS
setq("_standard_output_streams_",
     ["_standard_output_", "_error_output_", "_trace_output_"])
"The symbols naming standard output streams."

# Issue SPECIAL-BINDINGS-ARE-PACKAGE-LESS
setq("_standard_input_streams_",
     ["_standard_input_"])
"The symbols naming standard input streams."

# Issue SPECIAL-BINDINGS-ARE-PACKAGE-LESS
setq("_standard_io_streams_",
     ["_debug_io_", "_query_io_", "_terminal_io_"])
"The symbols naming standard io streams."

def init_global_stream_redirection():
        if symbol_value("_globally_redirect_io_"):
                if symbol_value("_saved_global_streams_"):
                        warn("Streams already redirected.")
                else:
                        mapc(setup_stream_indirection,
                             append(symbol_value("_standard_output_streams_"),
                                    symbol_value("_standard_input_streams_"),
                                    symbol_value("_standard_io_streams_")))

add_hook("_after_init_hook_", init_global_stream_redirection)

def globally_redirect_io_to_connection(connection):
        """Set the standard I/O streams to redirect to CONNECTION.
Assigns _CURRENT_<STREAM>_ for all standard streams."""
        for o in symbol_value("_standard_output_streams_"):
                setq(prefixed_var("#:current", o),
                     connection.user_output)
        # FIXME: If we redirect standard input to Emacs then we get the
        # regular Lisp top_level trying to read from our REPL.
        #
        # Perhaps the ideal would be for the real top_level to run in a
        # thread with local bindings for all the standard streams. Failing
        # that we probably would like to inhibit it from reading while
        # Emacs is connected.
        #
        # Meanwhile we just leave _standard_input_ alone.
        #+NIL
        #(dolist (i _standard_input_streams_)
        #  (set (prefixed_var '#:current i)
        #       (connection.user_input connection)))
        for io in symbol_value("_standard_io_streams_"):
                setq(prefixed_var("#:current", io),
                     connection.user_io)

def revert_global_io_redirection():
        "Set *CURRENT-<STREAM>* to *REAL-<STREAM>* for all standard streams."
        for stream_var in append(symbol_value("_standard_output_streams_"),
                                 symbol_value("_standard_input_streams_"),
                                 symbol_value("_standard_io_streams_")):
                setq(prefixed_var("#:current", stream_var),
                     symbol_value("_saved_global_streams_")[stream_var])

### Global redirection hooks: swank.lisp:1570

setq("_global_stdio_connection_", nil)
"""The connection to which standard I/O streams are globally redirected.
NIL if streams are not globally redirected."""

def maybe_redirect_global_io(connection):
        "Consider globally redirecting to CONNECTION."
        if (symbol_value("_globally_redirect_io_") and
            null(symbol_value("_global_stdio_connection_")) and
            connection.user_io):
                setq("_global_stdio_connection_", connection)
                globally_redirect_io_to_connection(connection)

def update_redirection_after_close(closed_connection):
        "Update redirection after a connection closes."
        check_type(closed_connection, connection)
        if symbol_value("_global_stdio_connection_") is closed_connection:
                if default_connection() and symbol_value("_global_stdio_connection_"):
                        # Redirect to another connection.
                        globally_redirect_io_to_connection(default_connection())
                else:
                        # No more connections, revert to the real streams.
                        revert_global_io_redirection()
                        setq("global_stdio_connection", nil)

add_hook("_connection_closed_hook_", update_redirection_after_close) # XXX: was late-bound

### Redirection during requests: swank.lisp:1596
##
## We always redirect the standard streams to Emacs while evaluating
## an RPC. This is done with simple dynamic bindings.

def create_repl(target):
        assert(target is nil)
        conn = symbol_value("_emacs_connection_")
        initialize_streams_for_connection(conn)
        # cl = find_package("CL")
        # def sym(x): return find_symbol_or_fail(x, cl)
        # Issue SPECIAL-BINDINGS-ARE-PACKAGE-LESS
        conn.env = [("_standard_output_", conn.user_output),
                    ("_standard_input_",  conn.user_input),
                    ("_trace_output_",    conn.trace_output or conn.user_output),
                    ("_error_output_",    conn.user_output),
                    ("_debug_io_",        conn.user_io),
                    ("_query_io_",        conn.user_io),
                    ("_terminal_io_",     conn.user_io),
                    ]
        maybe_redirect_global_io(conn)
        if use_threads_p():
                conn.repl_thread = spawn_repl_thread(conn, "repl-thread")
        return [package_name(symbol_value("_package_")),
                package_string_for_prompt(symbol_value("_package_"))]

def initialize_streams_for_connection(connection):
        c = connection
        c.dedicated_output, c.user_input, c.user_output, c.user_io, c.repl_results = open_streams(connection)
        return c

### Channels: swank.lisp:1631

setq("_channels_", [])
setq("_channel_counter_", 0)

#### class channel
#### initialize-instance channel
#### print-object channel
#### find-channel
#### channel-send
#### defmacro define-channel-method
#### send-to-remote-channel
#### class listener-channel
#### create-listener
#### initial-channel-bindings
#### spawn-listener-thread
#### define-channel-method :eval listener-channel
#### make-listener-output-stream
#### make-listener-input-stream
#### input-available-p

setq("_slime_features_", [])
"The feature list that has been sent to Emacs."

def send_oob_to_emacs(object):
        send_to_emacs(object)

def force_user_output():
        force_output(symbol_value("_emacs_connection_").user_io)

add_hook("_pre_reply_hook_", force_user_output)

def clear_user_input():
        clear_input(symbol_value("_emacs_connection_").user_input)

setq("_tag_counter_", 0)

def make_tag():
        # (mod (1+ *tag-counter*) (expt 2 22))
        setq("_tag_counter_", (symbol_value("_tag_counter_") + 1) % (1 << 22))

#### read-user-input-from-emacs
#### y-or-n-p-in-emacs
"Like y-or-n-p, but ask in the Emacs minibuffer."
#### read-from-minibuffer-in-emacs
"""Ask user a question in Emacs' minibuffer. Returns \"\" when user
entered nothing, returns NIL when user pressed C-g."""
#### process-form-for-emacs
"""Returns a string which emacs will read as equivalent to
FORM. FORM can contain lists, strings, characters, symbols and
numbers.

Characters are converted emacs' ?<char> notaion, strings are left
as they are (except for espacing any nested \" chars, numbers are
printed in base 10 and symbols are printed as their symbol-name
converted to lower case."""
#### eval-in-emacs
"""Eval FORM in Emacs.
`slime-enable-evaluate-in-emacs' should be set to T on the Emacs side."""

setq("_swank_wire_protocol_version_", "2011-09-28")
"The version of the swank/slime communication protocol."

def connection_info():
        """Return a key-value list of the form: 
\(&key PID STYLE LISP-IMPLEMENTATION MACHINE FEATURES PACKAGE VERSION)
PID: is the process-id of Lisp process (or nil, depending on the STYLE)
STYLE: the communication style
LISP-IMPLEMENTATION: a list (&key TYPE NAME VERSION)
FEATURES: a list of keywords
PACKAGE: a list (&key NAME PROMPT)
VERSION: the protocol version"""
        c = symbol_value("_emacs_connection_")
        setq("_slime_features_", symbol_value("_features_"))
        p = symbol_value("_package_")
        r = [keyword("pid"),                 getpid(),
             keyword("style"),               c.communication_style,
             keyword("encoding"),            [keyword("coding-system"),   c.coding_system,
                                              keyword("external-format"), princ_to_string(connection_external_format(c))],
             keyword("lisp-implementation"), [keyword("type"),    lisp_implementation_type(),
                                              keyword("name"),    lisp_implementation_type_name(),
                                              keyword("version"), lisp_implementation_version(),
                                              keyword("program"), lisp_implementation_program()],
             keyword("machine"),             [keyword("instance"), machine_instance(),
                                              keyword("type"),     machine_type(),
                                              keyword("version"),  machine_version()],
             keyword("features"),            features_for_emacs(),
             keyword("modules"),             symbol_value("_modules_"),
             keyword("package"),             [keyword("name"),     package_name(p),
                                              keyword("prompt"),   package_string_for_prompt(p)],
             keyword("version"),             symbol_value("_swank_wire_protocol_version_"),
             ]
        return r

#### defslimefun io-speed-test
#### debug-on-swank-error
#### (setf debug-on-swank-error)
#### defslimefun toggle-debug-on-swank-error

### Reading and printing: swank.lisp:1902
#### define-special *buffer-package*
"""Package corresponding to slime-buffer-package.  

EVAL-FOR-EMACS binds *buffer-package*.  Strings originating from a slime
buffer are best read in this package.  See also FROM-STRING and TO-STRING."""

def with_buffer_syntax(body, package = nil):
        """Execute BODY with appropriate *package* and *readtable* bindings.

This should be used for code that is conceptionally executed in an
Emacs buffer."""
        # (let ((*package* (if package 
        #                      (guess-buffer-package package) 
        #                      *buffer-package*)))
        #   ;; Don't shadow *readtable* unnecessarily because that prevents
        #   ;; the user from assigning to it.
        #   (if (eq *readtable* *buffer-readtable*)
        #       (call-with-syntax-hooks fun)
        #       (let ((*readtable* *buffer-readtable*))
        #         (call-with-syntax-hooks fun))))
        with progv(_package_ = (guess_buffer_package(package) if package else
                                symbol_value("_buffer_package_"))):
                # Don't shadow *readtable* unnecessarily because that prevents
                # the user from assigning to it.
                if symbol_value("_readtable_") is symbol_value("_buffer_readtable_"):
                        return call_with_syntax_hooks(body)
                else:
                        with progv(_readtable_ = symbol_value("_buffer_readtable_")):
                                return call_with_syntax_hooks(body)

def without_printing_errors(object, stream, body, msg = "<<error printing object>>"):
        "Catches errors during evaluation of BODY and prints MSG instead."
        def handler():
                if stream and object:
                        return print_unreadable_object(object, stream, lambda: fprintf(stream, msg),
                                                       type = t, identity = t)
                elif stream:
                        return write_string(msg, stream)
                elif object:
                        return with_output_to_string(
                                lambda s:
                                        print_unreadable_object(object, s, lambda: fprintf(stream, msg),
                                                                type = t, identity = t))
                else:
                        return msg
        return handler_case(body,
                            (error_,
                             handler))

def to_string(object):
        """Write OBJECT in the *BUFFER-PACKAGE*.
The result may not be readable. Handles problems with PRINT-OBJECT methods
gracefully."""
        def body():
                with progv(_print_readably_ = nil):
                        return without_printing_errors(object, nil,
                                                       lambda: prin1_to_string(object))
        return with_buffer_syntax(body)

def from_string(string):
        "Read string in the *BUFFER-PACKAGE*"
        def body():
                with progv(_read_suppress_ = nil):
                        return read_from_string(string) # XXX: was (values (read-from-string string))
        return with_buffer_syntax(body)

def parse_string(string, package):
        "Read STRING in PACKAGE."
        def body():
                with progv(_read_suppress_ = nil):
                        return read_from_string(string)
        return with_buffer_syntax(body)

## FIXME: deal with #\| etc.  hard to do portably.
def tokenize_symbol(string):
        """STRING is interpreted as the string representation of a symbol
and is tokenized accordingly. The result is returned in three
values: The package identifier part, the actual symbol identifier
part, and a flag if the STRING represents a symbol that is
internal to the package identifier part. (Notice that the flag is
also true with an empty package identifier part, as the STRING is
considered to represent a symbol internal to some current package.)"""
        package = let(position(":", string),
                     lambda pos: subseq(string, 0, pos) if pos else nil)
        symbol = let(position(":", string, from_end = t),
                     lambda pos: subseq(string, pos + 1) if pos else string)
        internp = count(":", string) != 1
        return values(symbol, package, internp)

def tokenize_symbol_thoroughly(string) -> values(str, str, bool):
        "This version of TOKENIZE-SYMBOL handles escape characters."
        package = nil
        token = ""
        backslash = nil
        vertical = nil
        internp = nil
        for char in string:
                if backslash:
                        token += char
                        backslash = nil
                elif char == "\\": # Quotes next character, even within |...|
                        backslash = t
                elif char == "":
                        vertical = not vertical
                elif vertical:
                        token += char
                elif char == ":":
                        if package and internp:
                                return values(nil, nil, nil)
                        elif package:
                                internp = t
                        else:
                                package = token
                                token = ""
                else:
                        token += casify_char(char)
        return (values(token, package, (not package) or internp) if not vertical else
                values(nil, nil, nil))

def untokenize_symbol(package_name, internal_p, symbol_name):
        """The inverse of TOKENIZE-SYMBOL.

  (untokenize-symbol \"quux\" nil \"foo\") ==> \"quux:foo\"
  (untokenize-symbol \"quux\" t \"foo\")   ==> \"quux::foo\"
  (untokenize-symbol nil nil \"foo\")    ==> \"foo\"
"""
        return (symbol_name                          if not package_name else
                cat(package_name, "::", symbol_name) if internal_p       else
                cat(package_name, ":",  symbol_name))

def casify_char(char):
        "Convert CHAR accoring to readtable-case."
        return ecase(symbol_value(readtable_case(symbol_value("_readtable_"))),
                     (keyword("preserve"), lambda: char),
                     (keyword("upcase"),   lambda: char_upcase(char)),
                     (keyword("downcase"), lambda: char_downcase(char)),
                     (keyword("invert"),   lambda: (char_downcase(char)
                                                    if upper_case_p(char) else
                                                    char_upcase(char))))

def find_symbol_with_status(symbol_name, status, package = None) -> values(symbol, bool):
        package = defaulted_to_var(package, "_package_")
        symbol, flag = find_symbol(symbol_name, package)
        return ((symbol, flag) if (flag and flag is status) else
                (nil, nil))

def parse_symbol(string, package = None):
        """Find the symbol named STRING.
Return the symbol and a flag indicating whether the symbols was found."""
        package = defaulted_to_var(package, "_package_")
        sname, pname, internalp = tokenize_symbol_thoroughly(string)
        if sname:
                package = (keyword_package     if pname == "" else
                           find_package(pname) if pname       else
                           package)
                if package:
                        symbol, flag = (find_symbol(sname, package) if internalp else
                                        find_symbol_with_status(sname, keyword("external"), package))
                        return values(symbol, flag, sname, package)
                else:
                        return values(nil, nil, nil, nil)
        else:
                return values(nil, nil, nil, nil)

def parse_symbol_or_lose(string, package = None):
        package = defaulted_to_var(package, "_package_")
        symbol, status = parse_symbol(string, package)[:2]
        return (values(symbol, status) if status else
                error("Unknown symbol: %s [in %s]", string, package))

def parse_package(string):
        """Find the package named STRING.
Return the package or nil."""
        # STRING comes usually from a (in-package STRING) form.
        def body():
                with progv(_package_ = symbol_value("_swank_io_package_")):
                        return find_package(read_from_string(string))
        return ignore_errors(body)

def unparse_name(string):
        "Print the name STRING according to the current printer settings."
        # this is intended for package or symbol names
        return subseq(prin1_to_string(make_symbol(string)), 2)

def guess_package(string):
        """Guess which package corresponds to STRING.
Return nil if no package matches."""
        if string:
                return (find_package(string) or
                        parse_package(string) or
                        nil)
                        ## Was:
                        # (if (find #\! string)           ; for SBCL
                        #     (guess-package (substitute #\- #\! string))

setq("_readtable_alist_", default_readtable_alist())
"An alist mapping package names to readtables."

def guess_buffer_readtable(name):
        package = guess_package(name)
        return ((package and
                 rest(assoc(package_name(package), symbol_value("_readtable_alist_"),
                            test = string_equal))) or
                symbol_value("_readtable_"))

### Evaluation: swank.lisp:2106
setq("_pending_continuations_", [])
"List of continuations for Emacs. (thread local)"

def guess_buffer_package(string):
        """Return a package for STRING. 
Fall back to the the current if no such package exists."""
        return ((string and guess_package(string)) or
                symbol_value("_package_"))

def eval_for_emacs(form, buffer_package, id):
        """Bind *BUFFER-PACKAGE* to BUFFER-PACKAGE and evaluate FORM.
Return the result to the continuation ID.
Errors are trapped and invoke our debugger."""
        # (let (ok result condition)
        #   (unwind-protect
        #        (let ((*buffer-package* (guess-buffer-package buffer-package))
        #              (*buffer-readtable* (guess-buffer-readtable buffer-package))
        #              (*pending-continuations* (cons id *pending-continuations*)))
        #          (check-type *buffer-package* package)
        #          ;; APPLY would be cleaner than EVAL.
        #          ;; (setq result (apply (car form) (cdr form)))
        #          (handler-bind ((t (lambda (c) (setf condition c))))
        #            (setq result (with-slime-interrupts (eval form))))
        #          (run-hook *pre-reply-hook*)
        #          (setq ok t))
        #     (send-to-emacs `(:return ,(current-thread)
        #                              ,(if ok
        #                                   `(:ok ,result)
        #                                   `(:abort ,(prin1-to-string condition)))
        #                              ,id)))))
        ok, result, condition = None, None, None
        def set_result(x):            nonlocal result;    result = x
        def set_condition(x):         nonlocal condition; condition = x
        try:
                with progv(_buffer_package_ = guess_buffer_package(buffer_package),
                           _buffer_readtable_ = guess_buffer_readtable(buffer_package),
                           _pending_continuations_ = [id] + symbol_value("_pending_continuations_")):
                        check_type(symbol_value("_buffer_package_"), package)
                        def with_slime_interrupts_body():
                                try:
                                        return nth_value(0, eval_(form))
                                except SystemExit:
                                        setq("_shutdown_", t)
                        handler_bind(lambda: set_result(with_slime_interrupts(with_slime_interrupts_body)),
                                     (error_,
                                      set_condition))
                        run_hook(boundp("_pre_reply_hook_") and symbol_value("_pre_reply_hook_"))
                        ok = True
        finally:
                send_to_emacs([keyword("return"),
                               current_thread(),
                               ([keyword("ok"), result]
                                if ok else
                                [keyword("abort"), prin1_to_string(condition)]),
                               id])

setq("_echo_area_prefix_", "=> ")
"A prefix that `format-values-for-echo-area' should use."

#### format-values-for-echo-area
#### macro values-to-string
#### interactive-eval
#### eval-and-grab-output

def eval_region(string):
        # Called from CONTROL-THREAD..(FIND-THREAD-FOR-EVALUATION == WORKER-THREAD)..(LISTENER-EVAL == REPL-EVAL)
        """Evaluate STRING.
Return the results of the last form as a list and as secondary value the 
last form."""
        @block
        def body(stream):
                less, vals = nil, nil
                def loop_body():
                        nonlocal less, vals
                        form = read(stream, nil, stream)
                        if form is stream:
                                finish_output()
                                return_from(body, values(vals, less))
                        less = form
                        vals = multiple_value_list(eval_(form))
                        finish_output()
                loop(loop_body)
        return with_input_from_string(string, body)

@block
def eval_region_python(string):
        # Called from CONTROL-THREAD..(FIND-THREAD-FOR-EVALUATION == WORKER-THREAD)..(LISTENER-EVAL == REPL-EVAL)
        """Evaluate STRING in Python mode (without doing a CL:_CALLIFY).
Return the results of the last form as a list and as secondary value the
last form."""
        forms = more_ast.extract_ast(string).body[::-1] # Reverse, to facilitate .pop()
        less, vals = nil, nil
        def loop_body():
                nonlocal less, vals
                if not forms:
                        finish_output()
                        return_from(eval_region_python, values(vals, less))
                form = forms.pop()
                vals = multiple_value_list(cl._eval_python(form))
                finish_output()
        loop(loop_body)

#### interactive-eval-region
#### re-evaluate-defvar

setq("_swank_pprint_bindings_", [(intern0("_print_pretty_"),   t),
                                 (intern0("_print_level_"),    nil),
                                 (intern0("_print_length_"),   nil),
                                 (intern0("_print_circle_"),   t),
                                 (intern0("_print_gensym_"),   t),
                                 (intern0("_print_readably_"), nil)])
"""A list of variables bindings during pretty printing.
Used by pprint-eval."""

#### swank-pprint
"Bind some printer variables and pretty print each object in VALUES."
#### pprint-eval
#### set-package
"""Set *package* to the package named NAME.
Return the full package-name and the string to use in the prompt."""

### Listener eval: swank.lisp:2241

# inversion: listener_eval, repl_eval are a bit lower..
def track_package(fn):
        orig_pkg = symbol_value("_package_")
        try:
                return fn()
        finally:
                cur_pkg = symbol_value("_package_")
                if orig_pkg is not cur_pkg:
                        send_to_emacs([keyword("new-package"), package_name(cur_pkg),
                                       package_string_for_prompt(cur_pkg)])

def send_repl_results_to_emacs(values):
        finish_output()
        here("returning %s", values)
        if not values:
                send_to_emacs([keyword("write-string"), "; No value", keyword("repl-result"),])
        else:
                mapc(lambda v: send_to_emacs(
                                [keyword("write-string"), prin1_to_string(v) + "\n", keyword("repl-result")]),
                     values)

setq("_send_repl_results_function_", send_repl_results_to_emacs)

defvar("_evaluator_mode_", keyword("python"))

def lisp_mode_p():
        return symbol_value("_evaluator_mode_") is keyword("lisp")
def python_mode_p():
        return symbol_value("_evaluator_mode_") is keyword("python")

def lisp_mode(report = t):
        setq("_evaluator_mode_", keyword("lisp"))
        if report:
                write_line("Evaluator is now in (lisp-mode)")

def python_mode(report = t):
        setq("_evaluator_mode_", keyword("python"))
        if report:
                write_line("Evaluator is now in Python(\"mode\")")

def repl_eval(string):
        # clear_user_input()
        def track_package_body():
                values, form = ecase(symbol_value("_evaluator_mode_"),
                                     (keyword("lisp"),   lambda: eval_region),
                                     (keyword("python"), lambda: eval_region_python))(string)
                # (setq *** **  ** *  * (car values)
                #       /// //  // /  / values
                #       +++ ++  ++ +  + last-form)
                symbol_value("_send_repl_results_function_")(values)
        with_retry_restart(lambda: track_package(track_package_body),
                           msg = "Retry SLIME REPL evaluation request.")

setq("_listener_eval_function_", repl_eval)

def listener_eval(string):
        # CONTROL-THREAD has this..
        return symbol_value("_listener_eval_function_")(string)
# end-of-inversion

def cat(*strings):
        "Concatenate all arguments and make the result a string."
        return with_output_to_string(
                lambda out:
                        mapc(lambda s: (write_string(s, out) if stringp(s) or charp(s) else
                                        error(simple_type_error, "CAT accepts only strings and characters.")),
                             strings))

def truncate_string(string, width, ellipsis = nil):
        len = len(string)
        if len < width:
                return string
        elif ellipsis:
                return cat(string[0:width], ellipsis)
        else:
                return string[0:width]

@block
def call__truncated_output_to_string(length, function, ellipsis = ".."):
        """Call FUNCTION with a new stream, return the output written to the stream.
If FUNCTION tries to write more than LENGTH characters, it will be
aborted and return immediately with the output written so far."""
        ###
        ### XXX: this will bomb out outside ASCII.
        ###
        buffer = bytearray([0]) * (length + len(ellipsis))
        fill_pointer = 0
        def write_output(string):
                nonlocal fill_pointer
                free = length - fill_pointer
                count = min(free, len(string))
                replace(buffer, string.encode("ascii"), start1 = fill_pointer, end2 = count)
                fill_pointer += count
                if len(string) > free:
                        replace(buffer, ellipsis.encode("ascii"), start1 = fill_pointer)
                        return_from(call__truncated_output_to_string, buffer.decode("ascii"))
        stream = make_output_stream(write_output)
        function(stream)
        finish_output(stream)
        return subseq(buffer, 0, fill_pointer).decode("ascii")

def with_string_stream(body, length = nil, bindings = nil):
        if not (length or bindings):
                return with_output_to_string(body)
        elif not bindings:
                return call__truncated_output_to_string(length, body)
        else:
                return with_bindings(bindings,
                                     lambda: call__truncated_output_to_string(length, body))

def to_line(object, width = 75):
        "Print OBJECT to a single line. Return the string."
        # Ought to be a lot simpler, no?
        return without_printing_errors(object, stream,
                                       lambda: with_string_stream(lambda stream:
                                                                          write(object, stream = stream, right_margin = width, lines = 1),
                                                                  length = width))

# (defun escape-string (string stream &key length (map '((#\" . "\\\"")
#                                                        (#\\ . "\\\\"))))
#   "Write STRING to STREAM surronded by double-quotes.
# LENGTH -- if non-nil truncate output after LENGTH chars.
# MAP -- rewrite the chars in STRING according to this alist."
#   (let ((limit (or length array-dimension-limit)))
#     (write-char #\" stream)
#     (loop for c across string 
#           for i from 0 do
#           (when (= i limit)
#             (write-string "..." stream)
#             (return))
#           (let ((probe (assoc c map)))
#             (cond (probe (write-string (cdr probe) stream))
#                   (t (write-char c stream)))))
#     (write-char #\" stream)))

def package_string_for_prompt(package):
        "Return the shortest nickname (or canonical name) of PACKAGE."
        return unparse_name(canonical_package_nickname(package) or
                            auto_abbreviated_package_name(package) or
                            shortest_package_nickname(package))

def canonical_package_nickname(package):
        "Return the canonical package nickname, if any, of PACKAGE."
        name = gethash(package_name(package), symbol_value("_canonical_package_nicknames_"))[0]
        if name:
                return string(name)

def auto_abbreviated_package_name(package):
        "XXX: stub"
        return package.name
# (defun auto-abbreviated-package-name (package)
#   "Return an abbreviated 'name' for PACKAGE. 

# N.B. this is not an actual package name or nickname."
#   (when *auto-abbreviate-dotted-packages*
#     (loop with package-name = (package-name package)
#           with offset = nil
#           do (let ((last-dot-pos (position #\. package-name :end offset :from-end t)))
#                (unless last-dot-pos
#                  (return nil))
#                ;; If a dot chunk contains only numbers, that chunk most
#                ;; likely represents a version number; so we collect the
#                ;; next chunks, too, until we find one with meat.
#                (let ((name (subseq package-name (1+ last-dot-pos) offset)))
#                  (if (notevery #'digit-char-p name)
#                      (return (subseq package-name (1+ last-dot-pos)))
#                      (setq offset last-dot-pos)))))))

def shortest_package_nickname(package):
        "XXX: stub"
        return package.name
# (defun shortest-package-nickname (package)
#   "Return the shortest nickname of PACKAGE."
#   (loop for name in (cons (package-name package) (package-nicknames package))
#         for shortest = name then (if (< (length name) (length shortest))
#                                    name
#                                    shortest)
#               finally (return shortest)))

# (defslimefun ed-in-emacs (&optional what)
#   "Edit WHAT in Emacs.

# WHAT can be:
#   A pathname or a string,
#   A list (PATHNAME-OR-STRING &key LINE COLUMN POSITION),
#   A function name (symbol or cons),
#   NIL. "
#   (flet ((canonicalize-filename (filename)
#            (pathname-to-filename (or (probe-file filename) filename))))
#     (let ((target 
#            (etypecase what
#              (null nil)
#              ((or string pathname) 
#               `(:filename ,(canonicalize-filename what)))
#              ((cons (or string pathname) *)
#               `(:filename ,(canonicalize-filename (car what)) ,@(cdr what)))
#              ((or symbol cons)
#               `(:function-name ,(prin1-to-string what))))))
#       (cond (*emacs-connection* (send-oob-to-emacs `(:ed ,target)))
#             ((default-connection)
#              (with-connection ((default-connection))
#                (send-oob-to-emacs `(:ed ,target))))
#             (t (error "No connection"))))))

def inspect_in_emacs(what, wait = None):
        """Inspect WHAT in Emacs. If WAIT is true (default NIL) blocks until the
inspector has been closed in Emacs."""
        def send_it():
                tag, thread = ((make_tag(), current_thread_id()) if wait else
                               (nil, nil))
                def body():
                        reset_inspector()
                        send_oob_to_emacs([keyword("inspect"), inspect_object(what),
                                           thread,
                                           tag])
                if wait:
                        return wait_for_event([keyword("emacs-return"), tag, result])
        if symbol_value("_emacs_connection_"):
                send_it()
        elif default_connection():
                with_connection(default_connection(),
                                send_it)
        return what

def value_for_editing(form):
        """Return a readable value of FORM for editing in Emacs.
FORM is expected, but not required, to be SETF'able."""
        # FIXME: Can we check FORM for setfability? -luke (12/Mar/2005)
        value = nth_value(0, eval_(read_from_string(form)))
        with progv(_print_length_ = nil):
                return prin1_to_string(value)

def commit_edited_value(form, value):
        """Set the value of a setf'able FORM to VALUE.
FORM and VALUE are both strings from Emacs."""
        ## We've got a SETF problem here..
        not_implemented()
#         def body():
#                 with_buffer_syntax(
#                         lambda:
# #                         (eval `(setf ,(read-from-string form)
# #                               ,(read-from-string (concatenate 'string "`" value))))
#                         )
#         return t

def background_message(format_string, *args):
        """Display a message in Emacs' echo area.

Use this function for informative messages only.  The message may even
be dropped, if we are too busy with other things."""
        if symbol_value("_emacs_connection_"):
                send_to_emacs([keyword("background-message"),
                               format(nil, format_string, *args)])

## This is only used by the test suite.
#### sleep-for

### Debugger: swank.lisp:2474

def invoke_slime_debugger(condition):
        """Sends a message to Emacs declaring that the debugger has been entered,
then waits to handle further requests from Emacs. Eventually returns
after Emacs causes a restart to be invoked."""
        without_slime_interrupts(
                lambda: (debug_in_emacs(condition) if symbol_value("_emacs_connection_") else
                         when_let(default_connection(),
                                  lambda conn:
                                          with_connection(conn,
                                                          lambda: debug_in_emacs(condition)))))

class invoke_default_debugger_condition(condition):
        pass

def swank_debugger_hook(condition, hook):
        "Debugger function for binding *DEBUGGER-HOOK*."
        handler_case(lambda: call_with_debugger_hook(swank_debugger_hook,
                                                     lambda: invoke_slime_debugger(condition)),
                     (invoke_default_debugger_condition,
                      lambda _: invoke_slime_debugger(condition)))

def invoke_default_debugger(condition):
        call_with_debugger_hook(nil,
                                lambda: invoke_debugger(condition))

setq("_global_debugger_", t)
"Non-nil means the Swank debugger hook will be installed globally."

def install_debugger(conn):
        if symbol_value("_global_debugger_"):
                install_debugger_globally(swank_debugger_hook)
add_hook("_new_connection_hook_", install_debugger)

### Debugger loop: swank.lisp:2510
##
## These variables are dynamically bound during debugging.
##
setq("_swank-debugger-condition_", nil)
"The condition being debugged."

setq("_sldb_level_",               0)
"The current level of recursive debugging."

setq("_sldb_initial_frames_",      20)
"The initial number of backtrace frames to send to Emacs."

setq("_sldb_restarts_",            [])
"The list of currenlty active restarts."

setq("_sldb_stepping_p_",          nil)
"True during execution of a step command."

def debug_in_emacs(condition):
        # (let ((*swank-debugger-condition* condition)
        #       (*sldb-restarts* (compute-restarts condition))
        #       (*sldb-quit-restart* (and *sldb-quit-restart*
        #                                 (find-restart *sldb-quit-restart*)))
        #       (*package* (or (and (boundp '*buffer-package*)
        #                           (symbol-value '*buffer-package*))
        #                      *package*))
        #       (*sldb-level* (1+ *sldb-level*))
        #       (*sldb-stepping-p* nil))
        #   (force-user-output)
        #   (call-with-debugging-environment
        #    (lambda ()
        #      ;; We used to have (WITH-BINDING *SLDB-PRINTER-BINDINGS* ...)
        #      ;; here, but that truncated the result of an eval-in-frame.
        #      (sldb-loop *sldb-level*)))))
        with progv(_swank_debugger_condition_ = condition,
                   _sldb_restarts_            = compute_restarts(condition),
                   _sldb_quit_restart_        = symbol_value("_sldb_quit_restart_") and find_restart(symbol_value("_sldb_quit_restart_")),
                   _package_                  = ((boundp("_buffer_package_") and
                                                  symbol_value("_buffer_package_")) or
                                                 symbol_value("_package_")),
                   _sldb_level_               = 1 + symbol_value("_sldb_level_"),
                   _sldb_stepping_p_          = None):
                force_user_output()
                ## We used to have (WITH-BINDING *SLDB-PRINTER-BINDINGS* ...)
                ## here, but that truncated the result of an eval-in-frame.
                call_with_debugging_environment(lambda: sldb_loop(symbol_value("_sldb_level_")))

@block
def sldb_loop(level):
        try:
                while True:
                        def with_simple_restart_body():
                                send_to_emacs([keyword("debug"), current_thread_id(), level] +
                                              # was wrapped into (with-bindings *sldb-printer-bindings*)
                                              debugger_info_for_emacs(0, symbol_value("_sldb_initial_frames_")))
                                send_to_emacs([keyword("debug-activate"), current_thread_id(), level, None])
                                while True:
                                        def handler_case_body():
                                                evt, _ = wait_for_event([or_,
                                                                         [keyword("emacs-rex")],
                                                                         [keyword("sldb-return", level + 1)]])
                                                if evt[0] is keyword("emacs-rex"):
                                                        eval_for_emacs(*evt[1:])
                                                elif evt[0] is keyword("sldb-return"):
                                                        return_from(sldb_loop, None)
                                        handler_case(handler_case_body,
                                                     (sldb_condition,
                                                      lambda c: handle_sldb_condition(c)))
                        with_simple_restart("ABORT", ("Return to sldb level %d.", level),
                                            with_simple_restart_body)
        finally:
                send_to_emacs([keyword("debug-return"),
                               current_thread_id(),
                               level,
                               symbol_value("_sldb_stepping_p_")])
                # clean event-queue
                wait_for_event([keyword("sldb-return"), level + 1],
                               t)
                if level > 1:
                        send_event(current_thread(),
                                   [keyword("sldb-return"), level])

def handle_sldb_condition(condition):
        """Handle an internal debugger condition.
Rather than recursively debug the debugger (a dangerous idea!), these
conditions are simply reported."""
        real_condition = condition.original_condition
        send_to_emacs([keyword("debug-condition"), current_thread_id(),
                       princ_to_string(real_condition)])

setq("_sldb_condition_printer_", format_sldb_condition)
"Function called to print a condition to an SLDB buffer."

def safe_condition_message(condition):
        with progv(_print_pretty_ = t,
                   _print_right_margin_ = 65):
                return handler_case(lambda: symbol_value("_sldb_condition_printer_")(condition),
                                    # Beware of recursive errors in printing, so only use the condition
                                    # if it is printable itself:
                                    (error_,
                                     lambda cond:
                                             format(nil, "Unable to display error condition: %s.",
                                                    ignore_errors(
                                                lambda: princ_to_string(cond)))))

def debugger_condition_for_emacs():
        condition = symbol_value("_swank_debugger_condition_")
        return [safe_condition_message(condition),
                format(nil, "   [Condition of type %s]", type_of(condition).__name__),
                condition_extras(condition)]

def format_restarts_for_emacs():
        """Return a list of restarts for *swank-debugger-condition* in a
format suitable for Emacs."""
        with progv(_print_right_margin_ = most_positive_fixnum):
                return mapcar(lambda restart:
                                      [("*" if restart is symbol_value("_sldb_quit_restart_") else
                                        "") + restart_name(restart),
                                       with_output_to_string(
                                        lambda stream:
                                                without_printing_errors(restart, stream,
                                                                        lambda: princ(restart, stream),
                                                                        msg = "<<error printing restart>>"))],
                              symbol_value("_sldb_restarts_"))

### SLDB entry points: swank.lisp:2614

def sldb_break_with_default_debugger(dont_unwind):
        "Invoke the default debugger."
        if dont_unwind:
                invoke_default_debugger(symbol_value("_swank_debugger_condition_"))
        else:
                signal(invoke_default_debugger)

def backtrace(start, end):
        """Return a list ((I FRAME PLIST) ...) of frames from START to END.

I is an integer, and can be used to reference the corresponding frame
from Emacs; FRAME is a string representation of an implementation's
frame."""
        return mapcar(lambda i, frame: [i, frame_to_string(frame)] + ([keyword("restartable"), True]
                                                                      if frame_restartable_p(frame) else
                                                                      []),
                      *zip(*enumerate(compute_backtrace(start, end), start)))

def frame_to_string(frame):
        return with_string_stream(lambda stream:
                                          # XXX: There was no align-related pretty-printing : -)
                                          handler_case(lambda: print_frame(frame, stream, align = 17, handle_overflow = t),
                                                       (error_,
                                                        lambda _: format(stream, "[error printing frame]"))),
                                  length = ((symbol_value("_print_lines_")        or 1) *
                                            (symbol_value("_print_right_margin_") or 100)),
                                  bindings = symbol_value("_backtrace_printer_bindings_"))

def debugger_info_for_emacs(start = 0, end = None):
        """Return debugger state, with stack frames from START to END.
The result is a list:
  (condition ({restart}*) ({stack-frame}*) (cont*))
where
  condition   ::= (description type [extra])
  restart     ::= (name description)
  stack-frame ::= (number description [plist])
  extra       ::= (:references and other random things)
  cont        ::= continutation
  plist       ::= (:restartable {nil | t | :unknown})

condition---a pair of strings: message, and type.  If show-source is
not nil it is a frame number for which the source should be displayed.

restart---a pair of strings: restart name, and description.

stack-frame---a number from zero (the top), and a printed
representation of the frame's call.

continutation---the id of a pending Emacs continuation.

Below is an example return value. In this case the condition was a
division by zero (multi-line description), and only one frame is being
fetched (start=0, end=1).

 ((\"Arithmetic error DIVISION-BY-ZERO signalled.
Operation was KERNEL::DIVISION, operands (1 0).\"
   \"[Condition of type DIVISION-BY-ZERO]\")
  ((\"ABORT\" \"Return to Slime toplevel.\")
   (\"ABORT\" \"Return to Top-Level.\"))
  ((0 \"(KERNEL::INTEGER-/-INTEGER 1 0)\" (:restartable nil)))
  (4))"""
        return [debugger_condition_for_emacs(),
                format_restarts_for_emacs(),
                backtrace(start, end),
                symbol_value("_pending_continuations_")]

def nth_restart(index):
        return nth(index, symbol_value("_sldb_restarts_"))

def invoke_nth_restart(index):
        restart = nth_restart(index)
        if restart:
                return invoke_restart_interactively(restart)

def sldb_abort():
        here("restarts: %s", symbol_value("_sldb_restarts_"))
        # Issue RESTART-NAMING-STRING-VS-SYMBOL
        return invoke_restart(find("ABORT", # XXX: Was: (find 'abort *sldb-restarts* :key #'restart-name)
                                   symbol_value("_sldb_restarts_"),
                                   key = restart_name))

def sldb_continue():
        return continue_()

def coerce_to_condition(datum, args):
        return etypecase(datum,
                         (string, lambda: make_condition(find_symbol_or_fail("SIMPLE-ERROR"),
                                                         format_control = datum,
                                                         format_arguments = args)),
                         (symbol, lambda: make_condition(datum, *args)))

def simple_break(datum = "Interrupt from Emacs", *args):
        return with_simple_restart("CONTINUE", "Continue from break.",
                lambda: invoke_slime_debugger(coerce_to_condition(datum, args)))

def throw_to_toplevel():
        """Invoke the ABORT-REQUEST restart abort an RPC from Emacs.
If we are not evaluating an RPC then ABORT instead."""
        restart = (symbol_value("_sldb_quit_restart_") and
                   find_restart(symbol_value("_sldb_quit_restart_")))
        return (invoke_restart(restart) if restart else
                format(nil, "Restart not active [%s]", symbol_value("_sldb_quit_restart_")))

def invoke_nth_restart_for_emacs(sldb_level, n):
        """Invoke the Nth available restart.
SLDB-LEVEL is the debug level when the request was made. If this
has changed, ignore the request."""
        if sldb_level == symbol_value("_sldb_level_"):
                return invoke_nth_restart(n)

#### wrap-sldb-vars
#   `(let ((*sldb-level* ,*sldb-level*))
#     ,form)

def eval_string_in_frame(string, index):
        return values_to_string(
                eval_in_frame(wrap_sldb_vars(from_string(string)),
                              index))

def pprint_eval_string_in_frame(string, index):
        return swank_pprint(
                multiple_value_list(
                        eval_in_frame(
                                wrap_sldb_vars(from_string(string)),
                                index)))

def frame_locals_and_catch_tags(index):
        """Return a list (LOCALS TAGS) for vars and catch tags in the frame INDEX.
LOCALS is a list of the form ((&key NAME ID VALUE) ...).
TAGS has is a list of strings."""
        # Swankr:
        # return [mapcar(lambda local_name: [keyword("name"), local_name,
        #                                    keyword("id"), 0,
        #                                    keyword("value"), handler_bind(lambda: print_to_string(frame_local_value(frame, local_name)),
        #                                                                    (error_,
        #                                                                     lambda c: "Error printing object: %s." % c))],
        #                ordered_frame_locals(frame)),
        #         []]
        return [frame_locals_for_emacs(index),
                mapcar(to_string, frame_catch_tags(index))]

def frame_locals_for_emacs(index):
        return with_bindings(
                symbol_value("_backtrace_printer_bindings_"),
                lambda: mapcar(lambda var:
                                       destructuring_bind_keys(
                                plist_hash_table(var),
                                lambda name = nil, id = nil, value = nil:
                                        # XXX: this will present variable names as strings,
                                        #      that is, not symbols, which is somewhat ugly...
                                        [keyword("name"),  prin1_to_string(name),
                                         keyword("id"),    id,
                                         keyword("value"), to_line(value)]),
                               frame_locals(index)))

def sldb_disassemble(index):
        def body(stream):
                with progv(_standard_output_ = stream):
                        disassemble_frame(index)
        return with_output_to_string(body)

def sldb_return_from_frame(index, string):
        form = from_string(form)
        return to_string(multiple_value_list(return_from_frame(index, form)))

def sldb_break(name):
        return with_buffer_syntax(
                lambda: sldb_break_at_start(read_from_string(name)))

#### macro define-stepper-function
#### define-stepper-function sldb-step sldb-step-into
#### define-stepper-function sldb-next sldb-step-next
#### define-stepper-function sldb-out  sldb-step-out

def toggle_break_on_signals():
        setq("_break_on_signals_", not symbol_value("_break_on_signals_"))
        return format(nil, "*break-on-signals* = %s" % (symbol_value("_break_on_signals_"),))

def sldb_print_condition():
        return princ_to_string(symbol_value("_swank_debugger_condition_"))

### Compilation Commands: swank.lisp:2785

class compilation_result(servile):
        """
(defstruct (:compilation-result
             (:type list) :named)
  notes
  (successp nil :type boolean)
  (duration 0.0 :type float)
  (loadp nil :type boolean)
  (faslfile nil :type (or null string)))
"""
        pass

measure_time_interval = clocking
"""Call FUN and return the first return value and the elapsed time.
The time is measured in seconds."""

def make_compiler_note(condition):
        "Make a compiler note data structure from a compiler-condition."
        return [keyword("message"),    message(condition),
                keyword("severity"),   severity(condition),
                keyword("location"),   location(condition),
                keyword("references"), references(condition)
                ] + let(source_context(condition),
                        lambda s: ([keyword("source-context"), s] if s else
                                   []))

def collect_notes(function):
        notes = []
        result, seconds = handler_bind(
                lambda: measure_time_interval(
                        lambda:
                                # To report location of error-signaling toplevel forms
                                # for errors in EVAL-WHEN or during macroexpansion.
                        restart_case(lambda: multiple_value_list(function()),
                                     abort = (lambda: [nil],
                                              dict(report = "Abort compilation.")))),
                # XXX -- condition type matching
                (compiler_condition,
                 lambda c: notes.append(make_compiler_note(c))))
        def destructuring_bind_body(success, loadp = nil, faslfile = nil):
                faslfile = (nil                            if faslfile is nil else
                            pathname_to_filename(faslfile) if stringp(faslfile) else
                            error(TypeError, "Bad compiler note: :FASLFILE ought to be either NIL or a pathname."))
                return make_compilation_result(notes = reverse(notes),
                                               duration = seconds,
                                               successp = not not successp,
                                               loadp = not not loadp,
                                               faslfile = faslfile)
        return destructuring_bind(result, destructuring_bind_body)

def compile_file_for_emacs(slime_connection, sldb_state, filename, loadp, *args):
        "XXX: not in compliance"
        """Compile FILENAME and, when LOAD-P, load the result.
Record compiler notes signalled as `compiler-condition's."""
        filename.co, time = clocking(lambda: compile(file_as_string(filename), filename, "exec"))
        if loadp:
                load_file(slime_connection, sldb_state, filename)
        return [keyword("compilation-result"), [], True, time, substitute(loadp), filename]

setq("_fasl_pathname_function_", nil)
"In non-nil, use this function to compute the name for fasl-files."

def pathname_as_directory(pathname):
        return (pathname_directory(pathname) +
                ([file_namestring(pathname)] if pathname_name(pathname) else []))

def compile_file_output(file, directory):
        return make_pathname(directory = pathname_as_directory(directory),
                             defaults = compile_file_pathname(file))

#### fasl-pathname

def compile_string_for_emacs(string, buffer, position, filename, policy):
        """Compile STRING (exerpted from BUFFER at POSITION).
Record compiler notes signalled as `compiler-condition's."""
        ## Swankr:
        # line_offset = char_offset = col_offset = None
        # for pos in position:
        #         if pos[0] is keyword("position"):
        #                 char_offset = pos[1]
        #         elif pos[0] is keyword("line"):
        #                 line_offset = pos[1]
        #                 char_offset = pos[2]
        #         else:
        #                 warning("unknown content in pos %s" % pos)
        # def frob(refs):
        #         not_implemented("frob")
        # def transform_srcrefs(s):
        #         not_implemented("transform_srcrefs")
        # time = None
        # def with_restarts_body():
        #         nonlocal time
        #         exprs = None
        #         def clocking_body():
        #                 nonlocal exprs
        #                 exprs = ast.parse(string)
        #                 return eval(transform_srcrefs(exprs),
        #                             globals = ...)
        #         val, time = clocking(clocking_body)
        #         return val
        # with_restarts(with_restarts_body)
        # return [keyword("compilation-result"), [], True, time, False, False]
        offset = assoc(keyword("position"), position)[1]
        def body():
                with progv(_compile_print_ = t,
                           _compile_verbose = nil):
                        return swank_compile_string(string,
                                                    buffer = buffer,
                                                    position = offset,
                                                    filename = filename,
                                                    policy = policy)
        return with_buffer_syntax(
                lambda: collect_notes(body))

def compile_multiple_strings_for_emacs(strings, policy):
        """Compile STRINGS (exerpted from BUFFER at POSITION).
Record compiler notes signalled as `compiler-condition's."""
        def iter(string, buffer, package, position, filename):
                def body():
                        with progv(_compile_print_ = t,
                                   _compile_verbose = nil):
                                return swank_compile_string(string,
                                                            buffer = buffer,
                                                            position = position,
                                                            filename = filename,
                                                            policy = policy)
                return collect_notes(lambda: with_buffer_syntax(package,
                                                                body))
        return [ iter(*strs) for strs in strings ]

def file_newer_p(new_file, old_file):
        "Returns true if NEW-FILE is newer than OLD-FILE."
        return file_write_date(new_file) > file_write_date(old_file)

def requires_compile_p(source_file):
        fasl_file = probe_file(compile_file_pathname(source_file))
        return ((not fasl_file) or
                file_newer_p(source_file, fasl_file))

def compile_file_if_needed(filename, loadp):
        pathname = filename_to_pathname(filename)
        return (compile_file_for_emacs(pathname, loadp) if requires_compile_p(pathname) else
                collect_notes(
                        lambda: ((not loadp) or
                                 load(compile_file_pathname(pathname)))))

### Loading: swank.lisp:2925
def load_file(filename):
        return to_string(load(filename_to_pathname(filename)))

### swank-require: swank.lisp:2931
def swank_require(modules, filename = None):
        "Load the module MODULE."
        for module in ensure_list(modules):
                if not member(str(module), symbol_value("_modules_")):
                        # Issue REQUIRE-NOT-IMPLEMENTED
                        # require(module, (filename_to_pathname(filename) if filename else
                        #                  module_filename(module)))
                        pass
        return symbol_value("_modules_")

# setq("_find_module_", find_module) # See just a little below.

def module_filename(module):
        "Return the filename for the module MODULE."
        return (symbol_value("_find_module_")(module) or
                error("Can't locate module: %s", module))

### Simple *find-module* function: swank.lisp:2952

def merged_directory(dirname, defaults):
        return os.path.join(defaults, dirname)

setq("_load_path_", [])
"A list of directories to search for modules."

def module_canditates(name, dir):
        return [compile_file_pathname(os.path.join(dir, name)),
                os.path.join(dir, name) + ".py"]

def find_module(module):
        name = string_downcase(string(module))
        return some(lambda dir: some(probe_file, module_candidates(name, dir)),
                    symbol_value("_load_path_"))

setq("_find_module_", find_module)
"""Pluggable function to locate modules.
The function receives a module name as argument and should return
the filename of the module (or nil if the file doesn't exist)."""

### Macroexpansion: swank.lisp:2973
#### defvar *macroexpand-printer-bindings*
#### apply-macro-expander
#### swank-macroexpand-1
#### swank-macroexpand
#### swank-macroexpand-all
#### swank-compiler-macroexpand-1
#### swank-compiler-macroexpand
#### swank-expand-1
#### swank-expand
#### expand-1
#### expand
#### expand-repeatedly
#### swank-format-string-expand

def disassemble_form(form):
        def body(stream):
                with progv(_standard_output_ = stream,
                           _print_readably_ = nil):
                        # XXX: does it really do what it name suggests?
                        # This EVAL thing is highly suspicious..
                        disassemble(nth_value(0, eval_(read_from_string(form))))
        return with_buffer_syntax(
                lambda: with_output_to_string(body))

### Simple completion: swank.lisp:3034

def simple_completions(prefix, package):
        "Return a list of completions for the string PREFIX."
        ## Swankr:
        # def literal2rx(string):
        #         return re.sub("([.\\|()[{^$*+?])", "\\\\\\1", string)
        # def grep(regex, strings):
        #         expr = re.compile(regex)
        #         return [ x for x in strings if re.search(expr, x) ]
        # matches = apropos("^%s" % literal2rx(prefix))
        # nmatches = len(matches)
        # if not matches:
        #         return [[], ""]
        # else:
        #         longest = sorted(matches, key = len)[0]
        #         while (cl._without_condition_system(
        #                         lambda: len(grep("^%s" % literal2rx(longest), matches)))
        #                < nmatches):
        #                 longest = longest[:-1]
        #         return [matches, longest]
        strings = all_completions(prefix, package)
        return [strings, longest_common_prefix(strings)]

def all_completions(prefix, package):
        name, pname, intern = tokenize_symbol(prefix)
        extern = pname and not intern
        pkg = (find_package("KEYWORD")       if pname == "" else
               guess_buffer_package(package) if not pname else
               guess_package(pname))
        test = lambda sym: prefix_match_p(name, symbol_name(sym))
        syms = pkg and matching_symbols(pkg, extern, test)
        return format_completion_set(mapcar(unparse_symbol, syms), intern, pname)

def matching_symbols(package, external, test):
        test = (lambda s: symbol_external_p(s) and test(s)) if external else test
        result = []
        do_symbols(package,
                   lambda s: test(s) and result.append(s))
        return remove_duplicates(result)

def unparse_symbol(symbol):
        with progv(_print_case_ = case(readtable_case(symbol_value("_readtable_")),
                                       (keyword("downcase"), keyword("upcase")),
                                       (t,                   keyword("downcase")))):
                return unparse_name(symbol_name(symbol))

def prefix_match_p(prefix, string):
        "Return true if PREFIX is a prefix of STRING."
        return not mismatch(prefix, string,
                            end2 = min(len(string), len(prefix)),
                            test = char_equal)

def longest_common_prefix(strings):
        "Return the longest string that is a common prefix of STRINGS."
        if not strings:
                return ""
        else:
                def common_prefix(s1, s2):
                        diff_pos = mismatch(s1, s2)
                        return (subseq(s1, 0, diff_pos) if diff_pos else
                                s1)
                return reduce(common_prefix, strings)

def format_completion_set():
        """Format a set of completion strings.
Returns a list of completions with package qualifiers if needed."""
        return mapcar(lambda string: untokenize_symbol(package_name, internal_p, string),
                      sort(strings, string_less))

### Simple arglist display: swank.lisp:3090
def operator_arglist(name, package):
        return ignore_errors(
                lambda: let(arglist(parse_symbol(name, guess_buffer_package(package))),
                            lambda args:
                                    (nil if args is keyword("not-available") else
                                     princ_to_string([name] + args))))

### Documentation: swank.lisp:3099
def apropos_list_for_emacs(name, external_only = nil, case_sensitive = nil, package = nil):
        """Make an apropos search for Emacs.
The result is a list of property lists."""
        package = package and (parse_package(package) or
                               error("No such package: %s", package))
        # The MAPCAN will filter all uninteresting symbols, i.e. those
        # who cannot be meaningfully described.
        return mapcan(listify(briefly_describe_symbol_for_emacs),
                      sort(remove_duplicates(apropos_symbols(name,
                                                             external_only,
                                                             case_sensitive,
                                                             package)),
                           present_symbols_before_p))

def briefly_describe_symbol_for_emacs(symbol):
        """Return a property list describing SYMBOL.
Like `describe-symbol-for-emacs' but with at most one line per item."""
        def first_line(string):
                pos = position("\n", string)
                return string if not pos else subseq(string, 0, pos)
        desc = map_if(stringp, first_line,
                      describe_symbol_for_emacs(symbol))
        if desc:
                return [ keyword("designator"), to_string(symbol) ] + desc

def map_if(test, fn, *lists):
        """Like (mapcar FN . LISTS) but only call FN on objects satisfying TEST.
Example:
\(map-if #'oddp #'- '(1 2 3 4 5)) => (-1 2 -3 4 -5)"""
        return mapcar(lambda x: fn(x) if test(x) else x,
                      *lists)

def listify(f):
        """Return a function like F, but which returns any non-null value
wrapped in a list."""
        # XXX: how is this supposed to deal with empty strings etc. ?
        def body(x):
                y = f(x)
                return y and [y]
        return body

#### present-symbol-vefore-p
"""Return true if X belongs before Y in a printed summary of symbols.
Sorted alphabetically by package name and then symbol name, except
that symbols accessible in the current package go first."""

#### make-apropos-matcher
#### apropos-symbols
#### call-with-describe-settings
#### defmacro with-describe-settings
#### describe-to-string
#### describe-symbol
#### describe-function
#### describe-definition-for-emacs
#### documentation-symbol

### Package Commands: swank.lisp:3224
def list_all_package_names(nicknames = nil):
        return mapcar(unparse_name,
                      mapcan(package_names, list_all_packages()) if nicknames else
                      mapcar(package_name,  list_all_packages()))

### Tracing: swank.lisp:3235
def tracedp(fspec):
        return member(fspec, nth_value(0, eval_([find_symbol_or_fail("TRACE")])))

def swank_toggle_trace(spec_string):
        spec = from_string(spec_string)
        if consp(spec):
                return toggle_trace(spec)
        elif tracedp(spec):
                eval_([find_symbol_or_fail("UNTRACE"), spec])
                return format(nil, "%s is now untraced.", spec)
        else:
                eval_([find_symbol_or_fail("TRACE"), spec])
                return format(nil, "%s is now traced.", spec)

def untrace_all():
        return untrace()

def redirect_trace_output(target):
        symbol_value("_emacs_connection_").trace_output = make_output_stream_for_target(
                symbol_value("_emacs_connection_"), target)
        return nil

### Undefing: swank.lisp:3261
def undefine_function(fname_string):
        fname = from_string(fname_string)
        return format(nil, "%s", fmakunbound(fname))

def unintern_symbol(name, package):
        pkg = guess_package(package)
        if not pkg:
                return format(nil, "No such package: %s", package)
        else:
                sym, found = parse_symbol(name, pkg)[:2]
                if not found:
                        return format(nil, "%s not in package %s", name, package)
                else:
                        unintern(sym, pkg)
                        return format(nil, "Uninterned symbol: %s", sym)

### Profiling: swank.lisp:3279
def profiledp(fspec):
        return member(fspec, profiled_functions)

def toggle_profile_fdefinition(fname_string):
        fname = from_string(fname_string)
        if profiledp(fname):
                unprofile(fname)
                return format(nil, "%s is now unprofiled.", fname)
        else:
                profile(fname)
                return format(nil, "%s is now profiled.", fname)

def profile_by_substring(substring, package):
        count = 0
        def maybe_profile(symbol):
                def body():
                        nonlocal count
                        profile(symbol)
                        count += 1
                if (fboundp(symbol) and
                    not profiledp(symbol) and
                    search(substring, symbol_name(symbol), test = string_equal)):
                        handler_case(body,
                                     (error_,
                                      lambda condition: warn("%s", condition)))
        if package:
                do_symbols(maybe_profile, parse_package(package))
        else:
                do_all_symbols(maybe_profile, parse_package(package))
        return format(nil, "%s functions now profiled", count)

### Source Locations: swank.lisp:3312
def find_definition_for_thing(thing):
        return find_source_location(thing)

def find_source_location_for_emacs(spec):
        return find_source_location(value_spec_ref(spec))

def value_spec_ref(spec):
        return destructure_case(
                spec,
                ([keyword("string")],
                 lambda string, package:
                         nth_value(0, eval_(read_from_string(string)))),
                ([keyword("inspector")],
                 inspector_nth_part),
                ([keyword("sldb")],
                 inspector_frame_var))

def find_definitions_for_emacs(name) -> [(str, str)]:
        """Return a list ((DSPEC LOCATION) ...) of definitions for NAME.
DSPEC is a string and LOCATION a source location. NAME is a string."""

        if lisp_mode_p():
                symbol, found = with_buffer_syntax(lambda:
                                                           parse_symbol(name)[:2])
                if found:
                        return mapcar(xref_elisp, find_definitions(symbol))
        else:
                pass

## Generic function so contribs can extend it.
#### defgeneric xref-doit
#### :method type thing
#### macrolet define-xref-action
#### define-xref-action :calls
#### define-xref-action :calls-who
#### define-xref-action :references
#### define-xref-action :binds
#### define-xref-action :macroexpands
#### define-xref-action :specializes
#### define-xref-action :callers
#### define-xref-action :callees

def xref(type, name):
        sexp, error = ignore_errors(lambda: from_string(name))
        if not error:
                xrefs = xref_doit(type, sexp)
                return (keyword("not-implemented") if xrefs is keyword("not-implemented") else
                        mapcar(xref_elisp, xrefs))

def xrefs(types, name):
        acc = []
        for type in types:
                xrefs = xref(type, name)
                if (xrefs is not keyword("not-implemented") and
                    xrefs):
                        acc.append([type] + xrefs)
        return acc

def xref_elisp(xref):
        name, loc = xref
        return [to_string(name), loc]

### Lazy lists: swank.lisp:3377
class lcons(collections.UserList):
        def __init__(self, car, cdr):
                self.car, self._cdr = (car, the(function_, cdr))
                self.forced = nil

def lconsp(x):       return isinstance(x, lcons)

def lcons_star(car, cdr, *more):
        return (lcons(car, cdr) if not more else
                lcons(car, lambda: lcons(cdr, more[0], more[1:])))

def lcons_cdr(lcons):
        if lcons.forced:
                return lcons._cdr
        else:
                value = lcons._cdr()
                lcons.forced = t
                lcons._cdr = value
                return value

def llist_range(llist, start, end):
        return llist_take(llist_skip(llist, start), end - start)

def llist_skip(lcons, index):
        i, l = 0, lcons
        while i != index and l:
                i += 1
                l = lcons_cdr(l)
        return l

def llist_take(lcons, count):
        result = []
        i, l = 0, lcons
        while (i != index and l):
                result.append(l.car)
                i += 1
                l = lcons_cdr(l)
        result.reverse()
        return result

def iline(label, value):
        return [keyword("line"), label, value]

### Inspecting: swank.lisp:3423

defvar("_inspector_verbose_",    nil)

defvar("_inspector_printer_bindings_",
       [("_print_lines_",        1),
        ("_print_right_margin_", 75),
        ("_print_pretty_",       t),
        ("_print_readably_",     nil)])

defvar("_inspector_verbose_printer_bindings_",
       [("_print_escape_",       t),
        ("_print_circle_",       t),
        ("_print_array_",        nil)])

class inspector_state():
        pass
class istate(inspector_state):
        def __init__(self, object = None, verbose = None, previous = None):
                self.parts  = []
                self.actions = []
                self.metadata_plist = []
                self.content = nil
                (self.object,
                 self.next, self.previous,
                 self.verbose) = (object,
                                  nil, previous,
                                  defaulted_to_var(previous, "_inspector_verbose_"))

def make_istate(**keys):
        return istate(**keys)

defvar("_istate_",               nil)
defvar("_inspector_history_")

def reset_inspector():
        setq("_istate_", nil)
        setq("_inspector_history_", [])

def init_inspector(string):
        def with_retry_restart_body():
                reset_inspector()
                return inspect_object(nth_value(0, eval_(read_from_string(string))))
        return with_buffer_syntax(
                lambda: with_retry_restart(with_retry_restart_body,
                                           msg = "retry SLIME inspection request"))

def ensure_istate_metadata(o, indicator, default):
        istate = symbol_value("_istate_")
        assert(istate.object is o)
        data = getf(istate.metadata_plist, indicator, default)
        setf_getf(istate.metadata_plist, indicator, data)
        return data

def inspect_object(o):
        prev = symbol_value("_istate_")
        istate = make_istate(object = o, previous = prev,
                             verbose = (prev.verbose if prev else
                                        symbol_value("_inspector_verbose_")))
        setq("_istate_", istate)
        istate.content = emacs_inspect__istate(istate)
        if not find(o, symbol_value("_inspector_history_")):
                symbol_value("_inspector_history_").append(o)
        previous = istate.previous
        if previous:
                previous.next = istate
        return istate__elisp(istate)

def emacs_inspect__istate(istate):
        return with_bindings(
                symbol_value("_inspector_verbose_printer_bindings_" if istate.verbose else
                             "_inspector_printer_bindings_"),
                lambda: emacs_inspect(istate.object))

def istate__elisp(istate):
        return [keyword("title"),   prepare_title(istate),
                keyword("id"),      assign_index(istate.ibject, istate.parts),
                keyword("content"), prepare_range(istate, 0, 500)]


def prepare_title(istate):
        return (with_bindings(
                        symbol_value("_inspector_verbose_printer_bindings_"),
                        lambda: to_string(istate.object))
                if istate.verbose else
                with_string_stream(
                        lambda stream: print_unreadable_object(istate.object, stream,
                                                               lambda _: nil,
                                                               type = t, identity = t),
                        length = 200,
                        bindings = symbol_value("_inspector_printer_bindings_")))

def prepare_range(istate, start, end):
        range = content_range(istate.content, start, end)
        ps = mapcan(lambda part: prepare_part(part, istate), range)
        return [ps,
                (start + len(ps)) if len(ps) < (end - start) else
                (end + 1000),
                start, end]

def prepare_part(part, istate):
        return etypecase(
                part,
                (string_, [part]),
                (list,    destructure_case(
                                part,
                                ([keyword("newline")],
                                 lambda: ["\n"]),
                                ([keyword("value")],
                                 lambda obj, str = nil:
                                         [value_part(obj, str, istate.parts)]),
                                ([keyword("label")],
                                 lambda *strs:
                                         [[keyword("label"),
                                           "".join(mapcar(string, strs))]]),
                                ([keyword("action")],
                                 lambda label, lambda_, refreshp = t:
                                         [action_part(label, lambda_, refreshp,
                                                      istate.actions)]),
                                ([keyword("line")],
                                 lambda label, value:
                                         [princ_to_string(label), ": ",
                                          value_part(value, nil, istate.parts),
                                          "\n"]))))

def value_part(object, string, parts):
        return [keyword("value"),
                string or print_part_to_string(object),
                assign_index(object, parts)]

def action_part(label, lambda_, refreshp, actions):
        return [keyword("action"), label, assign_index([lambda_, refreshp], actions)]

def assign_index(object, vector):
        index = fill_pointer(vector)
        vector.append(object)
        return index

def print_part_to_string(value):
        with progv(_print_readably_ = nil):
                string = to_line(value)
                pos = position(value, symbol_value("_inspector_history_"))
                return (format(nil, "@%d=%s", pos, string) if pos else
                        string)

def content_rante(list_, start, end):
        return typecase(list_,
                        (list,  lambda: subseq(list_, start, min(len(list_), end))),
                        (lcons, lambda: llist_range(list, start, end)))

def inspector_nth_part(index):
        return symbol_value("_istate_").parts[index]

def inspect_nth_part(index):
        return with_buffer_syntax(
                lambda:
                        inspect_object(inspector_nth_part(index)))

def inspector_range(from_, to):
        return prepare_range(symbol_value("_istate_"), from_, to)

def inspector_call_nth_action(index, *args):
        fun, refreshp = symbol_value("_istate_").actions[index]
        fun(*args)
        return (inspector_reinspect() if refreshp else
                #tell emacs that we don't want to refresh the inspector buffer
                nil)

def inspector_pop():
        """Inspect the previous object.
Return nil if there's no previous object."""
        if symbol_value("_istate_").previous:
                setq("_istate_", symbol_value("_istate_").previous)
                return istate_elisp(symbol_value("_istate_"))
        else:
                return nil

def inspector_next():
        "Inspect the next element in the history of inspected objects.."
        if symbol_value("_istate_").next:
                setq("_istate_", symbol_value("_istate_").next)
                return istate_elisp(symbol_value("_istate_"))
        else:
                return nil

################################# implementation effort paused here...

#### inspector-reinspect
#### inspector-toggle-verbose
#### inspector-eval
#### inspector-history
def quit_inspector(slime_connection, sldb_state):
        reset_inspector(slime_connection)
        return False
#### describe-inspectee
#### pprint-inspector-part
#### inspect-in-frame
def inspect_current_condition(slime_connection, sldb_state):
        "XXX: diff"
        reset_inspector(slime_connection)
        return inspect_object(slime_connection, sldb_state.condition)

def inspect_frame_var(frame, var):
        ## Was:
        # frame = sldb_state.frames[index] # XXX: was [index + 1]
        # varname = ordered_frame_locals(frame)[var]
        # return inspect_object(slime_connection, frame_local_value(frame, varname))
        def body():
                reset_inspector()
                return inspect_object(frame_var_value(frame, var))
        return with_buffer_syntax(body)

###     ...   : Lists: swank.lisp:3660

# @defmethod
# def emacs_inspect(o: list):
#         return inspect_list(o)

# @defmethod
# def emacs_inspect(o: tuple):
#         return (inspect_cons_list(o) if tuplep(o[1]) else
#                 inspect_tuple(o))

# (defun inspect-cons (cons)
#   (label-value-line* 
#    ('car (car cons))
#    ('cdr (cdr cons))))

# (defun inspect-list (list)
#   (multiple-value-bind (length tail) (safe-length list)
#     (flet ((frob (title list)
#              (list* title '(:newline) (inspect-list-aux list))))
#       (cond ((not length)
#              (frob "A circular list:"
#                    (cons (car list)
#                          (ldiff (cdr list) list))))
#             ((not tail)
#              (frob "A proper list:" list))
#             (t
#              (frob "An improper list:" list))))))

# (defun inspect-list-aux (list)
#   (loop for i from 0  for rest on list  while (consp rest)  append 
#         (if (listp (cdr rest))
#             (label-value-line i (car rest))
#             (label-value-line* (i (car rest)) (:tail (cdr rest))))))

# (defun safe-length (list)
#   "Similar to `list-length', but avoid errors on improper lists.
# Return two values: the length of the list and the last cdr.
# Return NIL if LIST is circular."
#   (do ((n 0 (+ n 2))                    ;Counter.
#        (fast list (cddr fast))          ;Fast pointer: leaps by 2.
#        (slow list (cdr slow)))          ;Slow pointer: leaps by 1.
#       (nil)
#     (cond ((null fast) (return (values n nil)))
#           ((not (consp fast)) (return (values n fast)))
#           ((null (cdr fast)) (return (values (1+ n) (cdr fast))))
#           ((and (eq fast slow) (> n 0)) (return nil))
#           ((not (consp (cdr fast))) (return (values (1+ n) (cdr fast)))))))

##### Hashtables

#### hash-table-to-alist
def hash_table_to_alist(ht):
        return cl._hash_table_alist(ht)

# @defmethod
# def emacs_inspect(ht: dict):
#         return label_value_line_star(
#                                      )
# (defmethod emacs-inspect ((ht hash-table))
#   (append
#    (label-value-line*
#     ("Count" (hash-table-count ht))
#     ("Size" (hash-table-size ht))
#     ("Test" (hash-table-test ht))
#     ("Rehash size" (hash-table-rehash-size ht))
#     ("Rehash threshold" (hash-table-rehash-threshold ht)))
#    (let ((weakness (hash-table-weakness ht)))
#      (when weakness
#        (label-value-line "Weakness:" weakness)))
#    (unless (zerop (hash-table-count ht))
#      `((:action "[clear hashtable]" 
#                 ,(lambda () (clrhash ht))) (:newline)
#        "Contents: " (:newline)))
#    (let ((content (hash-table-to-alist ht)))
#      (cond ((every (lambda (x) (typep (first x) '(or string symbol))) content)
#             (setf content (sort content 'string< :key #'first)))
#            ((every (lambda (x) (typep (first x) 'number)) content)
#             (setf content (sort content '< :key #'first))))
#      (loop for (key . value) in content appending
#            `((:value ,key) " = " (:value ,value)
#              " " (:action "[remove entry]"
#                           ,(let ((key key))
#                                 (lambda () (remhash key ht))))
#              (:newline))))))

##### Arrays

# @defmethod
# def emacs_inspect(char: list):
# (defmethod emacs-inspect ((array array))
#   (lcons*
#    (iline "Dimensions" (array-dimensions array))
#    (iline "Element type" (array-element-type array))
#    (iline "Total size" (array-total-size array))
#    (iline "Adjustable" (adjustable-array-p array))
#    (iline "Fill pointer" (if (array-has-fill-pointer-p array)
#                              (fill-pointer array)))
#    "Contents:" '(:newline)
#    (labels ((k (i max)
#               (cond ((= i max) '())
#                     (t (lcons (iline i (row-major-aref array i))
#                               (k (1+ i) max))))))
#      (k 0 (array-total-size array)))))

##### Chars

# @defmethod
# def emacs_inspect(char: str):
# (defmethod emacs-inspect ((char character))
#   (append 
#    (label-value-line*
#     ("Char code" (char-code char))
#     ("Lower cased" (char-downcase char))
#     ("Upper cased" (char-upcase char)))
#    (if (get-macro-character char)
#        `("In the current readtable (" 
#          (:value ,*readtable*) ") it is a macro character: "
#          (:value ,(get-macro-character char))))))

### Thread listing: swank.lisp:3771
### Class browser: swank.lisp:3825
### Automatically synchronized state: swank.lisp:3847
##
## Here we add hooks to push updates of relevant information to
## Emacs.

## *FEATURES*
def sync_features_to_emacs():
        "Update Emacs if any relevant Lisp state has changed."
        # FIXME: *slime-features* should be connection-local
        if symbol_value("_features_") != symbol_value("_slime_features_"):
                setq("_slime_features_", symbol_value("_features_"))
                send_to_emacs([keyword("new-features"), features_for_emacs()])

def features_for_emacs():
        "Return `*slime-features*' in a format suitable to send it to Emacs."
        return symbol_value("_slime_features_")

add_hook("_pre_reply_hook_", sync_features_to_emacs)

### Indentation of macros: swank.lisp:3868
#### clean-arglist
#### well-formed-list-p
#### print-indentation-lossage
# add_hook("_pre_reply_hook_", sync_indentation_to_emacs)

def before_init(version, load_path):
        # (pushnew :swank *features*)
        setq("_swank_wire_protocol_version_", version)
        setq("_load_path_",                   load_path)
        warn_unimplemented_interfaces()

def init():
        run_hook("_after_init_hook")
### swank.lisp ends here:4043

##*
##* Python-level globals
##*
partus_version = "2011-09-28"

debug = True

def swank_ast_name(x):
        return ast_name(x) if debug else ast_attribute(ast_name("swank"), x)
