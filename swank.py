import os
import sys
import re
import ast
import select

from collections import defaultdict, UserDict

from cl import *
from pergamum import *
from more_ast import *

from cl import _servile as servile, _keyword as keyword, _import, _find_symbol0, _find_symbol_or_fail, _intern0 as intern0

import swank_backend
import swank_python  # the thing patches swank_backend, to avoid indirection
from swank_backend import *

from swank_rpc import *

##*
##* SLDB state.
##*
# makeSldbState <- function(condition, level, id) {
#   calls <- rev(sys.calls())[-1]
#   frames <- rev(sys.frames())[-1]
#   restarts <- rev(computeRestarts(condition))[-1]
#   ret <- list(condition=condition, level=level, id=id, restarts=restarts, calls=calls, frames=frames)
#   class(ret) <- c("sldbState", class(ret))
#   ret
# }
class SldbState(servile): pass

def make_sldb_state(condition, level, id):
        frames = frames_upward_from(this_frame())
        # debug_printf("frames: %s", frames)
        return SldbState(frames = frames,
                         restarts = [],
                         condition = condition,
                         level = level,
                         id = id)

### Top-level variables, constants, macros: swank.lisp:74
cl_package      = find_package("CL")
keyword_package = find_package("KEYWORD")

setq("_canonical_package_nicknames_", [keyword("common-lisp-user"), keyword("cl-user")])

setq("_auto_abbreviate_dotted_packages_", t)

default_server_port = 4005

setq("_swank_debug_p_", t)

### SLDB customized pprint dispatch table: swank.lisp:95
setq("_sldb_string_length_", nil)
setq("_sldb_bitvector_length_", nil)

setq("_sldb_pprint_dispatch_table_",
     # XXX: ???
     None)

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

setq("_backtrace_pprint_dispatch_table_",
     # XXX: ???
     None)

setq("_backtrace_printer_bindings_",
     [(intern0("_print_pretty_"),          t),
      (intern0("_print_readably_"),        nil),
      (intern0("_print_level_"),           4),
      (intern0("_print_length_"),          5),
      (intern0("_print_lines_"),           1),
      (intern0("_print_right_margin_"),    200),
      (intern0("_print_pprint_dispatch_"), symbol_value("_backtrace_pprint_dispatch_table_"))])

setq("_default_worker_thread_bindings_", nil)

def call_with_bindings(alist, fun):
        if not alist:
                return fun()
        else:
                # (let* ((rlist (reverse alist))
                #        (vars (mapcar #'car rlist))
                #        (vals (mapcar #'cdr rlist)))
                #   (progv vars vals
                #    (funcall fun)))
                alist.reverse()
                vars = mapcar(first, alist)
                vals = mapcar(rest, alist)
                progv(vars, vals, fun)

with_bindings = call_with_bindings            #### Note: was: defmacro with-bindings

#### defmacro defslimefun

def missing_arg():
        error("A required &KEY or &OPTIONAL argument was not supplied.")

### Hooks: swank.lisp:213
def add_hook(name, function):
        if not functionp(function):
                error(TypeError, "ADD-HOOK: second argument must be a function, was %s.", function)
        symbol_value(name).append(function)

def run_hook(funs, *args, **keys):
        for f in funs:
                f(*args, **keys)

setq("_new_connection_hook_",    [])
setq("_connection_closed_hook_", [])
setq("_pre_reply_hook_",         [])
setq("_after_init_hook_",        [])

### Connections: swank.lisp:245
class connection():
        def __init__(self, socket, socket_io, communication_style, coding_system, serve_requests, cleanup):
                self.socket                     = socket
                self.socket_io                  = socket_io
                self.communication_style        = communication_style
                self.coding_system              = coding_system
                self.serve_requests             = serve_requests
                self.cleanup                    = cleanup
                #
                self.dedicated_output           = nil
                self.user_input                 = nil
                self.user_output                = nil
                self.user_io                    = nil
                self.env                        = None
                self.trace_output               = nil
                self.repl_results               = nil
                self.reader_thread              = None
                self.control_thread             = None
                self.repl_thread                = None
                self.autoflush_thread           = None
                self.cleanup                    = nil
                self.indentation_cache          = dict()
                self.indentation_cache_packages = []
                self.communication_style        = None
                self.coding_system              = None
                self.saved_sigint_handler       = None

def print_connection(conn, stream, depth):
        return print_unreadable_object(conn, stream, type = t, identity = t)

setq("_connections_",      [])
setq("_emacs_connection_", None)

def default_connection():
        return env._connections_[0]

def make_connection(socket, stream, style, coding_system):
        serve, cleanup = ((spawn_threads_for_connection, cleanup_connection_threads) if style is keyword("spawn") else
                          (install_sigio_handler, deinstall_sigio_handler) if style is keyword("sigio") else
                          (install_fd_handler, deinstall_fd_handler) if style is keyword("fd-handler") else
                          (simple_serve_requests, None))
        conn = connection(socket = socket,
                          socket_io = stream,
                          communication_style = style,
                          coding_system = coding_system,
                          serve_requests = serve,
                          cleanup = cleanup)
        run_hook(symbol_value("_new_connection_hook_"), conn)
        symbol_value("_connections_").append(conn)
        return conn

def connection_external_format(connection):
        return ignore_errors(lambda:
                                     stream_external_format(connection.socket_io))

def ping(tag):
        return tag

def safe_backtrace():
        return ignore_errors(lambda:
                                     call_with_debugging_environment(lambda:
                                                                             backtrace(0, nil)))

class swank_error(Exception):
        "Condition which carries a backtrace."
        def __init__(self, backtrace = None, condition = None):
                self.backtrace, self.condition = backtrace, condition
        def __str__(self):
                return str(self.condition)

def make_swank_error(condition, backtrace = safe_backtrace()):
        return swank_error(condition = condition, backtrace = backtrace)

setq("_debug_on_swank_protocol_error_", None)

def with_swank_error_handler(connection, body):
        def handler_case_body():
                return handler_bind(
                        body,
                        swank_error = (lambda condition:
                                               symbol_value("_debug_on_swank_protocol_error_") and
                                       invoke_default_debugger(condition)))
        return handler_case(handler_case_body,
                            swank_error = (lambda condition:
                                                   close_connection(connection,
                                                                    condition.condition,
                                                                    condition.backtrace)))

def with_panic_handler(connection, body):
        return handler_bind(body,
                            Exception = (lambda condition:
                                                 close_connection(connection,
                                                                  condition,
                                                                  safe_backtrace())))

def notify_backend_of_connection(connection):
        return emacs_connected()

add_hook("_new_connection_hook_", notify_backend_of_connection)

### Utilities: swank.lisp:406
### Logging: swank.lisp:409
setq("_swank_io_package_", lret(make_package("SWANK_IO_PACKAGE"),
                                # curry(_import, mapcar(_find_symbol_or_fail, ["t", "nil", "quote"]))
                                lambda package: _import(mapcar(_find_symbol_or_fail, ["T", "NIL", "QUOTE"]),
                                                        package)))
setq("_log_events_",       nil)
setq("_log_output_",       nil)

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
setq("_event_history_index_",  0)
setq("_enable_event_history_", t)

def log_event(format_string, *args):
        def wsios_body():
                with env.let(_print_readably_ = nil,
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
        format(t, "D/C: %s\n", x)
        op, body = x[0], x[1:]
        format(t, "clauses:\n")
        for c in clauses:
                format(t, "    %s\n", c)
        for struc, action in clauses:
                cop, cbody = struc[0], struc[1:]
                if cop is t or cop is op:
                        return action(*body)
                else:
                        format(t, "%s is not %s, but %s == %s: %s\n", 
                               cop, op, cop, op, cop == op)
        else:
                error("DESTRUCTURE-CASE failed: %s", x)

# setq("_slime_interrupts_enabled_", <unbound>) 

def check_slime_interrupts():
        if boundp("_pending_slime_interrupts_") and env._pending_slime_interrupts_:
                _pending_slime_interrupts_.pop()()
                return True

def with_slime_interrupts(body):
        check_slime_interrupts()
        with env.let(_slime_interrupts_enabled_ = True):
                ret = body()
        check_slime_interrupts()
        return ret

def without_slime_interrupts(body):
        with env.let(_slime_interrupts_enabled_ = False):
                return body()

#### invoke-or-queue-interrupt

def with_io_redirection(connection, body):
        return with_bindings(connection.env, body)

def with_connection(connection, body):
        if symbol_value("_emacs_connection_") is connection:
                return body()
        else:
                with env.let(_emacs_connection_ = connection,
                         _pending_slime_interrupts_ = []):
                        without_slime_interrupts(
                                lambda: with_swank_error_handler(
                                        connection,
                                        lambda: with_io_redirection(
                                                connection,
                                                lambda:
                                                        call_with_debugger_hook(
                                                        swank_debugger_hook,
                                                        body))))

#### call-with-retry-restart
#### macro with-retry-restart
#### with-struct*
#### do-symbols*
# UNUSABLE define-special
### Misc: swank.lisp:624
def use_threads_p():
        return symbol_value("_emacs_connection_").communication_style is keyword("spawn")

def current_thread_id():
        return thread_id(current_thread())

####   ensure-list
### Symbols: swank.lisp:637
### TCP Server: swank.lisp:769
# implementation in partus.py
### Event Decoding/Encoding: swank.lisp:1008

def decode_message(stream):
        "Read an S-expression from STREAM using the SLIME protocol."
        log_event("decode_message\n")
        return without_slime_interrupts(
                lambda:
                        handler_bind(
                        lambda:
                                handler_case(
                                lambda: read_message(stream,
                                                     symbol_value("_swank_io_package_")),
                                swank_reader_error =
                                lambda c: [keyword("reader-error"), c.packet, c.cause]),
                        Exception = lambda c: error(make_swank_error(c))))

def encode_message(message, stream):
        "Write an S-expression to STREAM using the SLIME protocol."
        log_event("encode_message\n")
        return without_slime_interrupts(
                lambda:
                        handler_bind(lambda: write_message(message,
                                                           symbol_value("_swank_io_package_"),
                                                           stream),
                                     Exception = lambda c: error(make_swank_error(c))))

### Event Processing: swank.lisp:1028
setq("_sldb_quit_restart_", None)

def with_top_level_restart(connection, k, body):
        def restart_case_body():
                with env.let(_sldb_quit_restart_ = find_restart("ABORT")):
                        return body()
        return with_connection(
                connection,
                lambda: restart_case(restart_case_body,
                                     abort = ((lambda v = None:
                                                       force_user_output() and k()),
                                              dict(report = "Return to SLIME's top level."))))

def handle_requests(connection, timeout = None):
        def tag_body():
                start
                with_top_level_restart(connection,
                                       lambda: go(start),
                                       lambda: process_requests(timeout))
        with_connection(
                connection,
                lambda: (process_requests(timeout) if symbol_value("_sldb_quit_restart_") else
                         tag_body()))

@block
def process_requests(timeout):
        def body():
                event, timeoutp = wait_for_event(find_symbol0("or"),
                                                 [keyword("emacs-rex"), ],        # XXX: (:emacs-rex . _)
                                                 [keyword("emacs-channel-send")]) # XXX: (:emacs-channel-send . _)
                if timeoutp:
                        return_from(process_requests, None)
                destructure_case(
                        event,
                        ([keyword("emacs-rex"),
                          eval_for_emacs]),
                        ([keyword("emacs-channel-send"),
                          lambda channel, selector, *args:
                                  channel_send(channel, selector, args)]))
        return loop(body)

def current_socket_io():
        return symbol_value("_emacs_connection_").socket_io

#### close-connection

### Thread based communication: swank.lisp:1107
setq("_active_threads_", [])

def read_loop(connection):
        input_stream, control_thread = connection.socket_io, connection.control_thread
        with_swank_error_handler(connection,
                                 lambda:
                                         loop(lambda: send(control_thread, decode_message(input_stream))))

def dispatch_loop(connection):
        with env.let(_emacs_connection_ = connection):
                with_panic_handler(connection,
                                   lambda:
                                           loop(lambda: dispatch_event(receive()[0]))) # WARNING: multiple values!

setq("_auto_flush_interval_", 0.5)

@block
def auto_flush_loop(stream):
        def body():
                if not (open_stream_p(stream) and output_stream_p(stream)):
                        return_from(auto_flush_loop, nil)
                call_with_io_timeout(
                        lambda: finish_output(stream),
                        seconds = 0.1)
                sleep(symbol_value("_auto_flush_interval_"))
        loop(body)

def find_repl_thread(connection):
        if not use_threads_p():
                return current_thread()
        else:
                thread = connection_repl_thread
                if not thread:
                        pass
                elif thread_alive_p(thread):
                        return thread
                else:
                        connection.repl_thread = spawn_repl_thread(connection, "new-repl-thread")
                        return connection.repl_thread

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
                  ## FIXME: to something better here
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

def spawn_worker_thread(connection):
        return spawn(lambda:
                             with_bindings(
                        symbol_value("_default_worker_thread_bindings_"),
                        lambda:
                                with_top_level_restart(
                                connection, nil,
                                lambda:
                                        eval_for_emacs(*wait_for_event([keyword("emacs_rex"),
                                                                        # XXX: was: :emacs-rex . _
                                                                        ])[1:]))),
                     name = "worker")

def spawn_repl_thread(connection, name):
        return spawn(lambda:
                             with_bindings(symbol_value("_default_worker_thread_bindings_"),
                                           lambda: repl_loop(connection)),
                     name = name)

def dispatch_event(event):
        log_event("dispatch_event: %s\n", event)
        def emacs_rex(form, package, thread_id, id):
                thread = thread_for_evaluation(thread_id)
                if thread:
                        symbol_value("_active_threads_").append(thread)
                        send_event(thread, [keyword("emacs-rex"), form, package, id])
                else:
                        encode_message([keyword("invalid-rpc"), id,
                                        format(nil, "Thread not found: %s", thread_id)],
                                       current_socket_io())
        def return_(thread, *args, **keys):
                tail = member(thread, symbol_value("_active_threads_"))
                setq("_active_threads_",
                     ldiff(symbol_value("_active_threads_"), tail) +
                     rest(tail))
                encode_message([keyword("return")] + args, current_socket_io())
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
                         letf(find_channel(channel_id),
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
                send (thread, event)
        else:
                symbol_value("_event_queue_").append(event)
                setq("_events_enqueued_",
                     (symbol_value("_events_enqueued_") + 1) % most_positive_fixnum)

def send_to_emacs(event):
        # log_event("send-to-emacs: %s %s\n", event)
        if use_threads_p():
                send (symbol_value ("_emacs_connection_").control_thread, event)
        else:
                dispatch_event (event)

def wait_for_event(pattern, timeout = None):
        log_event("wait_for_event: %s %s\n", pattern, timeout)
        without_slime_interrupts(
                lambda: (receive_if(lambda e: event_match_p(e, pattern), timeout)[0] # WARNING: multiple values!
                         if use_threads_p() else
                         wait_for_event_event_loop(pattern, timeout)))

@block
def wait_for_event_event_loop(pattern, timeout):
        assert((not timeout) or timeout is t)
        def body():
                check_slime_interrupts()
                event = poll_for_event(pattern)
                if event:
                        return_from(wait_for_event_event_loop, first(event))
                events_enqueued = symbol_value("_events_enqueued_")
                ready = wait_for_input([current_socket_io(), timeout])
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

or_, some_ = _find_symbol0("or", "CL"), _find_symbol0("some", "CL")
def event_match_p(event, pattern):
        if (keywordp(pattern) or numberp(pattern) or stringp(pattern) or
            pattern is t or pattern is nil):
                return event == pattern
        elif symbolp(pattern):
                return t
        elif listp(pattern):
                f = pattern[0] # XXX: symbols or strings?
                if f is or_:
                        return some(lambda p: event_match_p(event, p), rest(pattern))
                else:
                        return (listp(event) and
                                event_match_p(first(event), first(pattern)) and
                                event_match_p(rest(event), rest(pattern)))
        else:
                error("Invalid pattern: %s.", pattern)

def spawn_threads_for_connection(connection):
        connection.control_thread = spawn(lambda: control_thread(connection),
                                          name = "control-thread")
        return connection

def control_thread(connection):
        connection.control_thread = current_thread()
        connection.reader_thread  = spawn(lambda: read_loop(connection),
                                          name = "reader-thread")
        dispatch_loop(connection)

def cleanup_connection_threads(connection):
        threads = [connection.repl_thread,
                   connection.reader_thread,
                   connection.control_thread,
                   connection.auto_flush_thread]
        for thread in threads:
                if (thread and
                    thread_alive_p(thread) and
                    thread is not current_thread()):
                        kill_thread(thread)

def repl_loop(connection):
        handle_requests(connection)

### Signal driven IO: swank.lisp:1333
### SERVE-EVENT based IO: swank.lisp:1354
### Simple sequential IO: swank.lisp:1377
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
### Global redirection setup: swank.lisp:1474
### Global redirection hooks: swank.lisp:1570
### Redirection during requests: swank.lisp:1596
### Channels: swank.lisp:1631
setq("_channels_",      [])
setq("_channel_counter_", 0)

#### class channel
#### initialize-instance channel
#### print-object channel
#### find-channel
#### channel-send
#### defmacro define-channel-method
#### send-to-remote-channel
#### class listener-channel
#### initial-channel-bindings
#### spawn-listener-thread
#### define-channel-method :eval listener-channel
#### make-listener-output-stream
#### make-listener-input-stream
#### input_available_p

setq("_slime_features_", nil)

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
#### read-from-minibuffer-in-emacs
#### process-form-for-emacs
#### eval-in-emacs

setq("_swank_wire_protocol_version_", nil)

#### defslimefun connection-info
#### defslimefun io-speed-test
#### debug-on-swank-error
#### (setf debug-on-swank-error)
#### defslimefun toggle-debug-on-swank-error
### Reading and printing: swank.lisp:1902
# (define-special *buffer-package*     
#     "Package corresponding to slime-buffer-package.  
#
# EVAL-FOR-EMACS binds *buffer-package*.  Strings originating from a slime
# buffer are best read in this package.  See also FROM-STRING and TO-STRING.")

# (defun call-with-buffer-syntax (package fun)
#   (let ((*package* (if package 
#                        (guess-buffer-package package) 
#                        *buffer-package*)))
#     ;; Don't shadow *readtable* unnecessarily because that prevents
#     ;; the user from assigning to it.
#     (if (eq *readtable* *buffer-readtable*)
#         (call-with-syntax-hooks fun)
#         (let ((*readtable* *buffer-readtable*))
#           (call-with-syntax-hooks fun)))))
def with_buffer_syntax(package, body):
        with env.let(_package_ = guess_buffer_package(package) if package else env._buffer_package_):
                return call_with_syntax_hooks(body)

def without_printing_errors(object, stream, body, msg = "<<error printing object>>"):
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
                            Exception = handler)

#### to-string
#### from-string
#### parse-string
#### tokenize-symbol
#### tokenize-symbol-thoroughly
#### untokenize-symbol
#### casify-char
#### find-symbol-with-status
#### parse-symbol
#### parse-symbol-or-lose
#### parse-package
#### unparse-name
#### guess-package
# UNUSABLE: *readtable-alist*
# UNUSABLE: guess-buffer-readtable

##*
##* Actual implementation.
##*
# writeSexpToString <- function(obj) {
#   writeSexpToStringLoop <- function(obj) {
#     switch(typeof(obj),
#            "character"={ string <- paste(string, "\"", gsub("([\"\\])", "\\\\\\1", obj), "\"", sep="") },
#            "list"={ string <- paste(string, "(", sep="")
#                     max <- length(obj)
#                     if(max > 0) {
#                       for(i in 1:max) {
#                         string <- paste(string, writeSexpToString(obj[[i]]), sep="")
#                         if(i != max) {
#                           string <- paste(string, " ", sep="")
#                         }
#                       }
#                     }
#                     string <- paste(string, ")", sep="") },
#            "symbol"={ string <- paste(string, as.character(obj), sep="") },
#            "logical"={ string <- if(obj) { paste(string, "t", sep="") } else { paste(string, "nil", sep="") }},
#            "double"={ string <- paste(string, as.character(obj), sep="") },
#            "integer"={ string <- paste(string, as.character(obj), sep="") },
#            stop(paste("can't write object ", obj, sep="")))
#     string
#   }
#   string <- ""
#   writeSexpToStringLoop(obj)
# }
obj2lisp_xform = {
        False : "nil",
        None  : "nil",
        True  : "t",
        }
def write_sexp_to_string(obj):
        # debug_printf("write_sexp_to_string: %s", obj)
        def do_write_sexp_to_string(obj):
                string = ""
                def write_sexp_to_string_loop(obj):
                        nonlocal string
                        if listp(obj) or tuplep(obj):
                                string += '('
                                max = len(obj)
                                if max:
                                        for i in range(0, max):
                                                string += do_write_sexp_to_string(obj[i])
                                                if i != (max - 1):
                                                        string += " "
                                string += ')'
                        elif symbolp(obj) or integerp(obj) or floatp(obj):
                                string += str(obj)
                        elif obj in obj2lisp_xform:
                                string += obj2lisp_xform[obj]
                        elif type(obj).__name__ == 'builtin_function_or_method':
                                string += '"#<builtin %s 0x%x>"' % (obj.__name__, id(obj))
                        elif stringp(obj):
                                string += r'"%s"' % re.sub(r'(["\\])', r'\\\\1', obj)
                        else:
                                raise Exception("can't write object %s" % obj)
                        return string
                return write_sexp_to_string_loop(obj)
        ret = do_write_sexp_to_string(obj)
        # debug_printf("===> %s", ret)
        return ret

### Evaluation: swank.lisp:2106
setq("_pending_continuations_", [])

#### guess-buffer-package

# (defun eval-for-emacs (form buffer-package id)
#   "Bind *BUFFER-PACKAGE* to BUFFER-PACKAGE and evaluate FORM.
# Return the result to the continuation ID.
# Errors are trapped and invoke our debugger."
#   (let (ok result condition)
#     (unwind-protect
#          (let ((*buffer-package* (guess-buffer-package buffer-package))
#                (*pending-continuations* (cons id *pending-continuations*)))
#            (check-type *buffer-package* package)
#            ;; APPLY would be cleaner than EVAL.
#            ;; (setq result (apply (car form) (cdr form)))
#            (handler-bind ((t (lambda (c) (setf condition c))))
#              (setq result (with-slime-interrupts (eval form))))
#            (run-hook *pre-reply-hook*)
#            (setq ok t))
#       (send-to-emacs `(:return ,(current-thread)
#                                ,(if ok
#                                     `(:ok ,result)
#                                     `(:abort ,(prin1-to-string condition)))
#                                ,id)))))
def eval_for_emacs(form, buffer_package, id):
        ok, result, condition = None, None, None
        def set_result(x):    nonlocal result;    result = x
        def set_condition(x): nonlocal condition; condition = x
        try:
                with env.let(_buffer_package_ = guess_buffer_package(),
                             _pending_continuations_ = [id] + env._pending_continuations_):
                        check_type(_buffer_package_, package)
                        def with_slime_interrupts_body():
                                return eval(form)
                        handler_bind(lambda: set_result(with_slime_interrupts(with_slime_interrupts_body)),
                                     Exception = set_condition)
                        run_hook(boundp("_pre_reply_hook_") and symbol_value(env._pre_reply_hook_))
                        ok = True
        finally:
                send_to_emacs([keyword('return'),
                               current_thread(),
                               ([keyword('ok'), result]
                                if ok else
                                [keyword('abort'), condition]),
                               id])

# XXX: :emacs-rex processing (EVAL-FOR-EMACS)  was done by this one
def _eval_for_emacs(slime_connection, sldb_state, form, pkg, thread, id, level = 0):
        ok = False
        value = nil
        condition = nil
        output = make_string_output_stream()
        try:
                def send_abort(cond, mesg, *args):
                        nonlocal condition
                        writeurn_output(output)
                        condition = cond
                        # debug_printf("ERROR:" + mesg, *args)
                        raise cond
                def with_calling_handlers_body():
                        nonlocal ok, value
                        try:
                                expr = callify(form)
                                call = ast.fix_missing_locations(ast_module(
                                                [# ast_import_from("partus", ["*"]),
                                                 ast_assign_var("", ast_funcall(swank_ast_name("set_value"), expr)),
                                                 ]))
                        except Exception as cond:
                                send_abort(cond, "failed to callify: %s", cond)
                        # debug_printf("==========( COMPILE-AST\n%s\n", pp_ast_as_code(expr))
                        try:
                                code = compile(call, '', 'exec')
                        except Exception as cond:
                                send_abort(cond, "failed to compile: %s", cond)
                        # debug_printf("executing..")
                        with_output_redirection(lambda: exec(code, env.python_user.__dict__), file = output)
                        value = get_value()
                        string = writeurn_output(output)
                        # debug_printf("return value: %s", value)
                        # debug_printf("output:\n%s\n===== EOF =====\n", string)
                        ok = True
                with env.let(id = id):
                        handler_bind(with_calling_handlers_body,
                                     error = lambda cond: error_handler(cond, sldb_state, output = output))
        finally:
                send_to_emacs(slime_connection, [keyword('return'),
                                                 ([keyword('ok'), value]
                                                  if ok else
                                                  [keyword('abort'), condition]),
                                                 id])
def lisp_name_ast(x):
        def rec(x):
                return (ast_name(x[0])
                        if len(x) == 1 else
                        ast_attribute(rec(x[:-1]), x[-1]))
        return rec(remove_if_not(identity, pythonise_lisp_name(x).split(":")))

def constantp(x):
        return type(x) in set([str, int])
obj2ast_xform = {
        False : ast_name("False"),
        None  : ast_name("None"),
        True  : ast_name("True"),
        str   : ast_string,
        int   : ast_num,
        }

def callify(form, quoted = False):
        # debug_printf("CALLIFY %s", form)
        if listp(form):
                if quoted or (form[0] is _find_symbol0('quote')):
                        return (ast_list(mapcar(lambda x: callify(x, quoted = True), form[1]))
                                if listp(form[1]) else
                                callify(form[1], quoted = True))
                else:
                        return ast_funcall(lisp_name_ast(symbol_name(form[0])),
                                           *list(map(callify, form[1:])))
        elif symbolp(form):
                return (ast_funcall(swank_ast_name("read_symbol"), str(form)) if quoted or keywordp(form) else
                        ast_name(symbol_name(form)))
        elif constantp(form):
                return obj2ast_xform[type(form)](form)
        elif form in obj2ast_xform:
                return obj2ast_xform[form]
        else:
                error("Unable to convert form %s", form)
# <<< eval-for-emacs

setq("_echo_area_prefix_", "=> ")
#### format-values-for-echo-area
#### macro values-to-string
#### interactive-eval
#### eval-and-grab-output
# >>> eval-region
___expr___ = None
def set_value(value):
        global ___expr___
        ___expr___ = value
def get_value():
        return ___expr___

def writeurn_output(output):
        string = get_output_stream_string(output)
        close(output)
        if len(string):
                send_to_emacs(symbol_value("_emacs_connection_"), [keyword('write-string'), string])
                # send_to_emacs(env.slime_connection, [keyword('write-string'), "\n"])
        return string

def error_handler(c, sldb_state, output = None):
        global condition
        condition = c
        format(sys.stderr, "EE %s, sldb_state: %s", c, sldb_state)
        if output:
                writeurn_output(output)
        new_sldb_state = make_sldb_state(c, 0 if not sldb_state else sldb_state.level + 1, env.id)
        with env.let(sldb_state = new_sldb_state):
                # debug_printf("===( e-ha %s, new_sldb_state: %s", c, new_sldb_state)
                def with_restarts_body():
                        return sldb_loop(symbol_value("_emacs_connection_"), new_sldb_state, env.id)
                with_restarts(with_restarts_body,
                              abort = "return to sldb level %s" % str(new_sldb_state.level))

def eval_region(string):
        # string = re.sub(r"#\.\(swank:lookup-presented-object([^)]*)\)", r"(lookup-presented-object \\1))", string)
        form = None
        def eval_stage(name, fn):
                try:
                        return fn()
                except Exception as cond:
                        # debug_printf("===( LISTENER %s: %s, sldb state: %s", name, cond, sldb_state)
                        error_handler(cond, env.sldb_state)
                        return None
        ast_ = eval_stage("PARSE", lambda: ast.parse(string))
        if ast_ and ast_.body:
                exprp = typep(ast_.body[0], ast.Expr)
                if exprp:
                        ast_.body[0] = ast_assign_var("", ast_funcall(swank_ast_name("set_value"), ast_.body[0].value))
                co = eval_stage("COMPILE", lambda: compile(ast.fix_missing_locations(ast_), "", 'exec'))
                eval_stage("EXEC", lambda: exec(co, env.python_user.__dict__))
                return [keyword("values")] + [str(___expr___)] if (ast_.body and exprp) else []
        else:
                return [keyword("values")]
#### interactive-eval-region
#### re-evaluate-defvar

setq("_swank_pprint_bindings_", [(intern0("_print_pretty_"),   t),
                                 (intern0("_print_level_"),    nil),
                                 (intern0("_print_length_"),   nil),
                                 (intern0("_print_circle_"),   t),
                                 (intern0("_print_gensym_"),   t),
                                 (intern0("_print_readably_"), nil)])

#### swank-pprint
#### pprint-eval
#### set-package

# `swank:connection-info` <- function (slimeConnection, sldbState) {
#   list(quote(`:pid`), Sys.getpid(),
#        quote(`:package`), list(quote(`:name`), "R", quote(`:prompt`), "R> "),
#        quote(`:lisp-implementation`), list(quote(`:type`), "R",
#                                            quote(`:name`), "R",
#                                            quote(`:version`), paste(R.version$major, R.version$minor, sep=".")))
# }
def connection_info(slime_connection, sldb_state):
        return [keyword("pid"),                 getpid(),
                ## TODO: current package
                keyword("package"),             [keyword("name"), "python",
                                                  keyword("prompt"), "python>"],
                keyword("version"),             partus_version,
                keyword("lisp-implementation"), [keyword("type"), "python",
                                                  keyword("name"), "python",
                                                  keyword("version"), "%d.%d.%d" % sys.version_info[:3]]]

# `swank:swank-require` <- function (slimeConnection, sldbState, contribs) {
#   for(contrib in contribs) {
#     filename <- sprintf("%s/%s.R", swankrPath, as.character(contrib))
#     if(file.exists(filename)) {
#       source(filename)
#     }
#   }
#   list()
# }
def swank_require(slime_connection, sldb_state, contribs):
        for contrib in contribs:
                filename = "%s/%s.py" % (env.partus_path, str(contrib))
                if probe_file(filename):
                        load_file(filename)
        return []

# `swank:create-repl` <- function(slimeConnection, sldbState, env, ...) {
#   list("R", "R")
# }
def create_repl(slime_connection, sldb_state, env, *args):
        return ["python", "python"]

# `swank:autodoc` <- function(slimeConnection, sldbState, rawForm, ...) {
#   "No Arglist Information"
# }
def autodoc(slime_connection, sldb_state, raw_form, *args):
        return "No Arglist Information"

# `swank:operator-arglist` <- function(slimeConnection, sldbState, op, package) {
#   list()
# }
def operator_arglist(slime_connection, sldb_state, op, package):
        return []

# `swank:throw-to-toplevel` <- function(slimeConnection, sldbState) {
#   condition <- simpleCondition("Throw to toplevel")
#   class(condition) <- c("swankTopLevel", class(condition))
#   signalCondition(condition)
# }
class SwankTopLevel(Exception):
        pass
def throw_to_toplevel(slime_connection, sldb_state):
        condition = SwankTopLevel("Throw to toplevel")
        raise condition

# `swank:backtrace` <- function(slimeConnection, sldbState, from=0, to=NULL) {
#   calls <- sldbState$calls
#   if(is.null(to)) to <- length(calls)
#   from <- from+1
#   calls <- lapply(calls[from:to],
#                   { frameNumber <- from-1;
#                     function (x) {
#                       ret <- list(frameNumber, paste(format(x), sep="", collapse=" "))
#                       frameNumber <<- 1+frameNumber
#                       ret
#                     }
#                   })
# }
def backtrace(slime_connection, sldb_state, from_ = 0, to = None):
        frames = sldb_state.frames
        longest = max(mapcar(lambda f: len(fun_filename(frame_fun(f))), frames))
        return list(enumerate(map(pp_frame,
                                  frames[from_:to or len(frames)]), # XXX: was [from_ + 1:to or len(frames)]
                              from_))

# (defun debugger-condition-for-emacs ()
#   (list (safe-condition-message *swank-debugger-condition*)
#         (format nil "   [Condition of type ~S]"
#                 (type-of *swank-debugger-condition*))
#         (condition-extras *swank-debugger-condition*)))
def debugger_condition_for_emacs():
        return [safe_condition_message(env._swank_debugger_condition_),
                format(nil, "   [Condition of type %s]",
                       type_of(env._swank_debugger_condition_).__name__),
                condition_extras(env._swank_debugger_condition_)]

def format_restarts_for_emacs():
        # let ((*print-right-margin* most-positive-fixnum))
        return mapcar(lambda restart:
                              [("*" if restart is env._sldb_quit_restart_ else
                                "") + restart_name(restart),
                               with_output_to_string(
                                without_printing_errors(restart, stream,
                                                        lambda: princ(restart, stream),
                                                        msg = "<<error printing restart>>"))],
                      env._sldb_restarts_)

# `swank:invoke-nth-restart-for-emacs` <- function(slimeConnection, sldbState, level, n) {
#   if(sldbState$level == level) {
#     invokeRestart(sldbState$restarts[[n+1]])
#   }
# }
def invoke_nth_restart_for_emacs(slime_connection, sldb_state, level, n):
        if sldb_state.level == level:
                return invoke_restart(sldb_state.restarts[n+1])

# prin1ToString <- function(val) {
#   paste(deparse(val, backtick=TRUE, control=c("delayPromises", "keepNA")),
#         sep="", collapse="\n")
# }
def prin1_to_string(val):
        return "\n".join(deparse(val)) # FIXME


# `swank:eval-string-in-frame` <- function(slimeConnection, sldbState, string, index) {
#   frame <- sldbState$frames[[1+index]]
#   withRetryRestart("retry SLIME interactive evaluation request",
#                    value <- eval(parse(text=string), envir=frame))
#   printToString(value)
# }
def eval_string_in_frame(slime_connection, sldb_state, string, index):
        frame = sldb_state.frames[index] # XXX: was [index + 1]
        value = None
        def with_retry_restart_body():
                nonlocal value
                value = eval_in_frame(parse(string),
                                      env = frame)
        with_retry_restart(with_retry_restart_body,
                           msg = "retry SLIME interactive evaluation request")
        return print_to_string(value)

def frame_locals_and_catch_tags(index):
        return [frame_locals_for_emacs(index),
                mapcar(to_string, frame_catch_tags(index))]
        # frame = sldb_state.frames[index] # XXX: was [index + 1]
        # return [mapcar(lambda local_name: [keyword('name'), local_name,
        #                                    keyword('id'), 0,
        #                                    keyword('value'), handler_bind(lambda: print_to_string(frame_local_value(frame, local_name)),
        #                                                                    Exception = lambda c: "Error printing object: %s." % c)],
        #                ordered_frame_locals(frame)),
        #         []]

def frame_locals_for_emacs(index):
        # with-bindings *backtrace-printer-bindings*
        return mapcar(lambda var: destructuring_bind(var,
                                                     lambda name = "", id = "", value = "":
                                                             [keyword("name"),  prin1_to_string(name),
                                                              keyword("id"),    id,
                                                              keyword("value"), to_line(value)]),
                      frame_locals(index))
        

# `swank:simple-completions` <- function(slimeConnection, sldbState, prefix, package) {
#   literal2rx <- function(string) {
#     ## list of ERE metacharacters from ?regexp
#     gsub("([.\\|()[{^$*+?])", "\\\\\\1", string)
#   }
#   matches <- apropos(sprintf("^%s", literal2rx(prefix)), ignore.case=FALSE)
#   nmatches <- length(matches)
#   if(nmatches == 0) {
#     list(list(), "")
#   } else {
#     longest <- matches[order(nchar(matches))][1]
#     while(length(grep(sprintf("^%s", literal2rx(longest)), matches)) < nmatches) {
#       longest <- substr(longest, 1, nchar(longest)-1)
#     }
#     list(as.list(matches), longest)
#   }
# }
def simple_completions(slime_connection, sldb_state, prefix, package):
        def literal2rx(string):
                return re.sub("([.\\|()[{^$*+?])", "\\\\\\1", string)
        def grep(regex, strings):
                expr = re.compile(regex)
                return [ x for x in strings if re.search(expr, x) ]
        matches = apropos("^%s" % literal2rx(prefix))
        nmatches = len(matches)
        if not matches:
                return [[], ""]
        else:
                longest = sorted(matches, key = len)[0]
                while len(grep("^%s" % literal2rx(longest), matches)) < nmatches:
                        longest = longest[:-1]
                return [matches, longest]

# `swank:compile-string-for-emacs` <- function(slimeConnection, sldbState, string, buffer, position, filename, policy) {
#   lineOffset <- charOffset <- colOffset <- NULL
#   for(pos in position) {
#     switch(as.character(pos[[1]]),
#            `:position` = {charOffset <- pos[[2]]},
#            `:line` = {lineOffset <- pos[[2]]; colOffset <- pos[[3]]},
#            warning("unknown content in pos", pos))
#   }
#   frob <- function(refs) {
#     lapply(refs,
#            function(x)
#            srcref(attr(x,"srcfile"),
#                   c(x[1]+lineOffset-1, ifelse(x[1]==1, x[2]+colOffset-1, x[2]),
#                     x[3]+lineOffset-1, ifelse(x[3]==1, x[4]+colOffset-1, x[4]),
#                     ifelse(x[1]==1, x[5]+colOffset-1, x[5]),
#                     ifelse(x[3]==1, x[6]+colOffset-1, x[6]))))
#   }
#   transformSrcrefs <- function(s) {
#     srcrefs <- attr(s, "srcref")
#     attribs <- attributes(s)
#     new <- 
#       switch(mode(s),
#              "call"=as.call(lapply(s, transformSrcrefs)),
#              "expression"=as.expression(lapply(s, transformSrcrefs)),
#              s)
#     attributes(new) <- attribs
#     if(!is.null(attr(s, "srcref"))) {
#       attr(new, "srcref") <- frob(srcrefs)
#     }
#     new
#   }
#   withRestarts({
#     times <- system.time({
#       exprs <- parse(text=string, srcfile=srcfile(filename))
#       eval(transformSrcrefs(exprs), envir = globalenv()) })},
#                abort="abort compilation")
#   list(quote(`:compilation-result`), list(), TRUE, times[3], FALSE, FALSE)
# }
def compile_string_for_emacs(slime_connection, sldb_state, string, buffer, position, filename, policy):
        line_offset = char_offset = col_offset = None
        for pos in position:
                if pos[0] is keyword('position'):
                        char_offset = pos[1]
                elif pos[0] is keyword('line'):
                        line_offset = pos[1]
                        char_offset = pos[2]
                else:
                        warning("unknown content in pos %s" % pos)
        def frob(refs):
                not_implemented("frob")
        def transform_srcrefs(s):
                not_implemented("transform_srcrefs")
        time = None
        def with_restarts_body():
                nonlocal time
                exprs = None
                def clocking_body():
                        nonlocal exprs
                        exprs = ast.parse(string)
                        return eval(transform_srcrefs(exprs),
                                    globals = ...)
                val, time = clocking(clocking_body)
                return val
        with_restarts(with_restarts_body)
        return [keyword('compilation-result'), [], True, time, False, False]

# withRetryRestart <- function(description, expr) {
#   call <- substitute(expr)
#   retry <- TRUE
#   while(retry) {
#     retry <- FALSE
#     withRestarts(eval.parent(call),
#                  retry=list(description=description,
#                    handler=function() retry <<- TRUE))
#   }
# }
def with_retry_restart(fn, msg = "Retry"):
        retry = True
        while retry:
                retry = False
                def handler_body():
                        nonlocal retry
                        retry = True
                with_restarts(fn,
                              retry = { 'description': msg,
                                        'handler':     handler_body })

### Listener eval: swank.lisp:2241
def listener_eval(slime_connection, sldb_state, string):
        return env._listener_eval_function_(string)

def send_repl_results_to_emacs(values):
        finish_output()
        if not values:
                send_to_emacs([keyword("write-string"), "; No value", keyword("repl-result"),])
                mapc(lambda v: send_to_emacs(
                                [keyword("write-string"), prin1_to_string(v) + "\n", keyword("repl-result")]),
                     values)

setq("_send_repl_results_to_emacs_", send_repl_results_to_emacs)

def repl_eval(string):
        # clear_user_input()
        def track_package_body():
                values, form = eval_region(string)
                # (setq *** **  ** *  * (car values)
                #       /// //  // /  / values
                #       +++ ++  ++ +  + last-form)
                env._send_repl_results_function_(values)
        with_retry_restart(lambda: track_package(track_package_body),
                           msg = "Retry SLIME REPL evaluation request.")

setq("_listener_eval_function_", repl_eval)

def track_package(fn):
        p = _package_()
        try:
                return fn()
        finally:
                if p is not _package_():
                        send_to_emacs([keyword("new-package"), package_name(_package_()),
                                       package_string_for_prompt(_package_())])

#### cat
#### truncate-string
#### call/truncated-output-to-string
#### with-string-stream
#### to-line
#### escape-string
#### package-string-for-prompt
#### canonical-package-nickname
#### auto-abbreviated-package-name
#### shortest-package-nickname
#### ed-in-emacs
#### inspect-in-emacs
#### value-for-editing
#### commit-edited-value
#### background-message
# UNUSABLE: sleep-for

### Debugger: swank.lisp:2474
def invoke_slime_debugger(condition):
        """Sends a message to Emacs declaring that the debugger has been entered,
then waits to handle further requests from Emacs. Eventually returns
after Emacs causes a restart to be invoked."""
        without_slime_interrupts(
                lambda: (debug_in_emacs(condition) if symbol_value("_emacs_connection_") else
                         when_let(default_connection(),
                                  lambda connection:
                                          with_connection(connection,
                                                          lambda: debug_in_emacs(condition)))))

class invoke_default_debugger_condition(BaseException):
        pass

def swank_debugger_hook(condition, hook):
        handler_case(lambda: call_with_debugger_hook(swank_debugger_hook,
                                                     lambda: invoke_slime_debugger(condition)),
                     invoke_default_debugger_condition = lambda _: invoke_slime_debugger(condition))

def invoke_default_debugger(condition):
        call_with_debugger_hook(nil,
                                lambda: invoke_debugger(condition))

setq("_global_debugger_", t)

def install_debugger(connection):
        if symbol_value("_global_debugger_"):
                install_debugger_globally(swank_debugger_hook)

add_hook("_new_connection_hook_", install_debugger)

### Debugger loop: swank.lisp:2510
##
## These variables are dynamically bound during debugging.
##
setq("_swank-debugger-condition_", None) # (defvar *swank-debugger-condition* nil "The condition being debugged.")                           
setq("_sldb_level_",               0)    # (defvar *sldb-level*               0   "The current level of recursive debugging.")               
setq("_sldb_initial_frames_",      20)   # (defvar *sldb-initial-frames*      20  "The initial number of backtrace frames to send to Emacs.")
setq("_sldb_restarts_",            [])   # (defvar *sldb-restarts*            nil "The list of currenlty active restarts.")                  
setq("_sldb_stepping_p_",          None) # (defvar *sldb-stepping-p*          nil "True during execution of a step command.")

# (defun debug-in-emacs (condition)
#   (let ((*swank-debugger-condition* condition)
#         (*sldb-restarts* (compute-restarts condition))
#         (*sldb-quit-restart* (and *sldb-quit-restart*
#                                   (find-restart *sldb-quit-restart*)))
#         (*package* (or (and (boundp '*buffer-package*)
#                             (symbol-value '*buffer-package*))
#                        *package*))
#         (*sldb-level* (1+ *sldb-level*))
#         (*sldb-stepping-p* nil))
#     (force-user-output)
#     (call-with-debugging-environment
#      (lambda ()
#        ;; We used to have (WITH-BINDING *SLDB-PRINTER-BINDINGS* ...)
#        ;; here, but that truncated the result of an eval-in-frame.
#        (sldb-loop *sldb-level*)))))
def debug_in_emacs(condition):
        with env.let(_swank_debugger_condition_ = condition,
                     _sldb_restarts_            = compute_restarts(condition),
                     _sldb_quit_restart_        = env._sldb_quit_restart_ and find_restart(env._sldb_quit_restart_),
                     _package_                  = ((boundp("_buffer_package_") and
                                                    symbol_value("_buffer_package_")) or
                                                   symbol_value("_package_")),
                     _sldb_level_               = 1 + env._sldb_level_,
                     _sldb_stepping_p_          = None):
                force_user_output()
                ## We used to have (WITH-BINDING *SLDB-PRINTER-BINDINGS* ...)
                ## here, but that truncated the result of an eval-in-frame.
                call_with_debugging_environment(lambda: sldb_loop(env._sldb_level_))

@block
def sldb_loop(level):
        assert(symbol_value("_emacs_connection_"))
        try:
                while True:
                        def with_simple_restart_body():
                                send_to_emacs([keyword("debug"), current_thread_id(), level] +
                                              # was wrapped into (with-bindings *sldb-printer-bindings*)
                                              debugger_info_for_emacs(0, env._sldb_initial_frames_))
                                send_to_emacs([keyword(debug-activate), current_thread_id(), level, None])
                                while True:
                                        def handler_case_body():
                                                evt = wait_for_event(symbol_value("_emacs_connection_"),
                                                                     ["or",
                                                                      [keyword("emacs-rex")],
                                                                      [keyword("sldb-return", level + 1)]])
                                                if evt[0] is keyword("emacs-rex"):
                                                        eval_for_emacs(*evt[1:])
                                                elif evt[0] is keyword("sldb-return"):
                                                        return_from("sldb_loop", None)
                                        handler_case(handler_case_body,
                                                     SLDB_CONDITION = lambda c: handle_sldb_condition(c))
                        with_simple_restart("ABORT", "Return to sldb level %d." % level,
                                            with_simple_restart_body)
        finally:
                send_to_emacs([keyword("debug-return"),
                               current_thread_id(),
                               level,
                               env._sldb_stepping_p_])
                wait_for_event(symbol_value("_emacs_connection_"),
                               [keyword("sldb-return"), level + 1],
                               True)                   # clean event-queue
                if level > 1:
                        send_event(symbol_value("_emacs_connection_"),
                                   current_thread(), [keyword("sldb-return"), level])

#### handle-sldb-condition
#### defvar *sldb-condition-printer*
#### safe-condition-message
#### debugger-condition-for-emacs
#### format-restarts-for-emacs

### SLDB entry points: swank.lisp:2614
def sldb_break_with_default_debugger(dont_unwind):
        if dont_unwind:
                invoke_default_debugger(env._swank_debugger_condition_)
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
        with_string_stream(lambda stream:
                                   handler_case(lambda: print_frame(frame, stream),
                                                Error = lambda _: format(stream, "[error printing frame]")),
                           length = ((symbol_value("_print_lines_")        or 1) *
                                     (symbol_value("_print_right_margin_") or 100)),
                           bindings = symbol_value("_print_right_margin_"))

# (defslimefun debugger-info-for-emacs (start end)
#   "Return debugger state, with stack frames from START to END.
# The result is a list:
#   (condition ({restart}*) ({stack-frame}*) (cont*))
# where
#   condition   ::= (description type [extra])
#   restart     ::= (name description)
#   stack-frame ::= (number description [plist])
#   extra       ::= (:references and other random things)
#   cont        ::= continutation
#   plist       ::= (:restartable {nil | t | :unknown})
#
# condition---a pair of strings: message, and type.  If show-source is
# not nil it is a frame number for which the source should be displayed.
#
# restart---a pair of strings: restart name, and description.
#
# stack-frame---a number from zero (the top), and a printed
# representation of the frame's call.
#
# continutation---the id of a pending Emacs continuation.
#
# Below is an example return value. In this case the condition was a
# division by zero (multi-line description), and only one frame is being
# fetched (start=0, end=1).
#
#  ((\"Arithmetic error DIVISION-BY-ZERO signalled.
# Operation was KERNEL::DIVISION, operands (1 0).\"
#    \"[Condition of type DIVISION-BY-ZERO]\")
#   ((\"ABORT\" \"Return to Slime toplevel.\")
#    (\"ABORT\" \"Return to Top-Level.\"))
#   ((0 \"(KERNEL::INTEGER-/-INTEGER 1 0)\" (:restartable nil)))
#   (4))"
#   (list (debugger-condition-for-emacs)
#         (format-restarts-for-emacs)
#         (backtrace start end)
#         *pending-continuations*))
def debugger_info_for_emacs(slime_connection, sldb_state, from_ = 0, to = None):
        return [debugger_condition_for_emacs(),
                format_restart_for_emacs(),
                backtrace(from_, to),
                env._pending_continuations_]

#### nth-restart
#### invoke-nth-restart
#### sldb-abort
#### sldb-continue
#### coerce-to-condition
#### simple-break
#### throw-to-toplevel
#### invoke-nth-restart-for-emacs
#### wrap-sldb-vars
#### eval-string-in-frame
#### pprint-eval-string-in-frame
#### frame-locals-and-catch-tags
#### frame-locals-for-emacs
#### sldb-disassemble
#### sldb-return-from-frame
#### sldb-break
#### macro define-stepper-function
#### define-stepper-function sldb-step sldb-step-into
#### define-stepper-function sldb-next sldb-step-next
#### define-stepper-function sldb-out  sldb-step-out
#### toggle-break-on-signals
#### sdlb-print-condition

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
#### measure-time-interval -- clocking
#### make-compiler-note
#### collect-notes
def compile_file_for_emacs(slime_connection, sldb_state, filename, loadp, *args):
        "XXX: not in compliance"
        filename.co, time = clocking(lambda: compile(file_as_string(filename), filename, 'exec'))
        if loadp:
                load_file(slime_connection, sldb_state, filename)
        return [keyword('compilation-result'), [], True, time, substitute(loadp), filename]
setq("_fasl_pathname_function_", None)
#### pathname-as-directory
#### compile-file-output
#### fasl-pathname
#### compile-string-for-emacs
#### compile-multiple-strings-for-emacs
#### file-newer-p
#### requires-compile-p
#### compile-file-if-needed
### Loading: swank.lisp:2925
def load_file(slime_connection, sldb_state, filename):
        "XXX: not in compliance"
        exec(compile(file_as_string(filename), filename, 'exec'))
        return True
### swank-require: swank.lisp:2931
### Simple *find-module* function: swank.lisp:2952
### Macroexpansion: swank.lisp:2973
### Simple completion: swank.lisp:3034
### Simple arglist display: swank.lisp:3090
### Documentation: swank.lisp:3099
### Package Commands: swank.lisp:3224
### Tracing: swank.lisp:3235
### Undefing: swank.lisp:3261
### Profiling: swank.lisp:3279
### Source Locations: swank.lisp:3312
### Lazy lists: swank.lisp:3377
### Inspecting: swank.lisp:3423
setq("_inspector_verbose_",                     None)
# setq("_inspector_printer_bindings_",            [])
# setq("_inspector_verbose_printer_bindings_",    [])
#### defstruct inspector-state
#### defstruct istate
setq("_inspector_history_",                     None)
# setq("_istate_",                         <unbound>)
def reset_inspector(slime_connection):
        slime_connection.istate = InspectorState(parts = [])
        slime_connection.inspector_history = list()
def init_inspector(slime_connection, sldb_state, string):
        "XXX: diff?"
        value = None
        def with_retry_restart_body():
                nonlocal value
                reset_inspector(slime_connection)
                value = inspect_object(slime_connection,
                                       eval(string)) # FIXME envir
                pass
        with_retry_restart(with_retry_restart_body,
                           msg = "retry SLIME inspection request")
        return value
#### ensure-istate-metadata
#### inspect-object
#### emacs-inspect/istate
#### prepare-title
#### prepare-range
#### prepare-part
#### value-part
#### action-part
#### assign-index
#### print-part-to-string
#### content-range
def inspector_nth_part(slime_connection, sldb_state, index):
        return slime_connection.istate.parts[index]
def inspect_nth_part(slime_connection, sldb_state, index):
        object = inspector_nth_part(slime_connection, sldb_state, index)
        return inspect_object(slime_connection, object)
#### inspector-range
#### inspector-call-nth-action
def inspector_pop(slime_connection, sldb_state):
        "XXX: diff"
        if slime_connection.istate.previous:
                slime_connection.istate = slime_connection.istate.previous
                return istate_to_elisp(slime_connection.istate)
        else:
                return False
def inspector_next(slime_connection, sldb_state):
        "XXX: diff"
        if slime_connection.istate.next:
                slime_connection.istate = slime_connection.istate.next
                return istate_to_elisp(slime_connection.istate)
        else:
                return False
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

def inspect_frame_var(slime_connection, sldb_state, frame, var):
        "XXX: diff"
        reset_inspector(slime_connection)
        frame = sldb_state.frames[index] # XXX: was [index + 1]
        varname = ordered_frame_locals(frame)[var]
        return inspect_object(slime_connection, frame_local_value(frame, varname))
###     ...   : Lists: swank.lisp:3660
###     ...   : Hashtables: swank.lisp:3705
###     ...   : Arrays: swank.lisp:3741
###     ...   : Chars: swank.lisp:3758
### Thread listing: swank.lisp:3771
### Class browser: swank.lisp:3825
### Automatically synchronized state: swank.lisp:3847
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
