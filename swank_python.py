import os
import inspect
import socket
import threading

import cl

from cl import env, identity, setq, symbol_value, progv, boundp, t, nil, format, find, member_if, remove_if_not, constantly, loop, ldiff, rest, first
from cl import block, return_from, handler_bind, signal, make_condition, write_line, princ_to_string, stream, mapcar
from cl import _top_frame, _frame_fun, _fun_info
from cl import _keyword as keyword

from pergamum import slotting, here, when_let, if_let

from swank_backend import defimplementation
from swank_backend import check_slime_interrupts

def not_implemented():
        raise NotImplemented("Not implemented: %s.", inspect.stack()[1][3].upper())

##### :: swank-sbcl.lisp
### Requires the SB-INTROSPECT contrib.

### Administrivia
# (in-package :swank-backend)

# (eval-when (:compile-toplevel :load-toplevel :execute)
#   (require 'sb-bsd-sockets)
#   (require 'sb-introspect)
#   (require 'sb-posix)
#   (require 'sb-cltl2))

# (declaim (optimize (debug 2) 
#                    (sb-c::insert-step-conditions 0)
#                    (sb-c::insert-debug-catch 0)
#                    (sb-c::merge-tail-calls 2)))

# (import-from :sb-gray *gray-stream-symbols* :swank-backend)

### backwards compability tests

# (eval-when (:compile-toplevel :load-toplevel :execute)
#   ;; Generate a form suitable for testing for stepper support (0.9.17)
#   ;; with #+.
#   (defun sbcl-with-new-stepper-p ()
#     (with-symbol 'enable-stepping 'sb-impl))
def sbcl_with_new_stepper_p():
        return nil

#   ;; Ditto for weak hash-tables
#   (defun sbcl-with-weak-hash-tables ()
#     (with-symbol 'hash-table-weakness 'sb-ext))
def sbcl_with_weak_hash_tables():
        return nil

#   ;; And for xref support (1.0.1)
#   (defun sbcl-with-xref-p ()
#     (with-symbol 'who-calls 'sb-introspect))
def sbcl_with_xref_p():
        return nil

#   ;; ... for restart-frame support (1.0.2)
#   (defun sbcl-with-restart-frame ()
#     (with-symbol 'frame-has-debug-tag-p 'sb-debug)))
def sbcl_with_restart_frame():
        return nil

### swank-mop
# (import-swank-mop-symbols :sb-mop '(:slot-definition-documentation))
# (defun swank-mop:slot-definition-documentation (slot)
#   (sb-pcl::documentation slot t))

### Connection info
@defimplementation
def lisp_implementation_type_name():
        return "CPython"

@defimplementation
def getpid():
        return os.getpid()

### TCP Server
@defimplementation
def preferred_communication_style():
        return keyword("spawn")

def resolve_hostname(name):
        return socket.gethostbyname(name)

@defimplementation
def create_socket(address, port):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.bind((address, port))
        socket_ = sock.makefile(mode = "r")
        socket_.sock = sock
        return socket_

@defimplementation
def local_port(socket):
        return socket.sock.getsockname()[1]

@defimplementation
def close_socket(socket):
        socket.sock.close()

@defimplementation
def accept_connection(socket, external_format = "utf-8", buffering = "full", timeout = nil):
        # XXX: socket buffering ought to be honored
        socket.sock.listen(0)
        client_sock, _ = socket.sock.accept()
        client_socket = client_sock.makefile(mode = "rw", encoding = external_format)
        client_socket.sock = client_sock
        return client_socket

#-win32
#### install-sigint-handler

setq("_sigio_handlers_", [])
"List of (key . fn) pairs to be called on SIGIO."

#### sigio-handler
#### set-sigio-handler
#### enable-sigio-on-fd
# def add_sigio_handler(socket, fn):			pass
# def remove_sigio_handlers(socket):			pass
# def add_fd_handler(socket, fn):			pass
# def remove_fd_handlers(socket):			pass
# def socket_fd(socket_stream):				pass

@defimplementation
def command_line_args():
        return sys.argv

# def dup(fd):						pass

#### defvar *wait-for-input-called* <unbound>

@defimplementation
def wait_for_input(streams, timeout = nil):
        if timeout and timeout is not t:
                error(simple_type_error, "WAIT-FOR-INPUT: timeout must be NIL or T, was: %s.", timeout)
        if boundp("_wait_for_input_called_"):
                 setq("_wait_for_input_called_", t)
        @block
        def _wait_for_input():
                with progv(_wait_for_input_called_ = nil):
                        def body():
                                ready = remove_if_not(input_ready_p, streams)
                                if ready:
                                        return_from(_wait_for_input, ready)
                                if timeout:
                                        return_from(_wait_for_input, nil)
                                if check_slime_interrupts():
                                        return_from(_wait_for_input, keyword("interrupt"))
                                if symbol_value("_wait_for_input_called_"):
                                        return_from(_wait_for_input, keyword("interrupt"))
                                sleep(0.2)
                        loop(body)
        return _wait_for_input()

#-win32
def input_ready_p(stream):
        # (let ((c (read-char-no-hang stream nil :eof)))
        #   (etypecase c
        #     (character (unread-char c stream) t)
        #     (null nil)
        #     ((member :eof) t))))
        return cl._coerce_to_stream(stream).readable() # XXX!

#+win32
# (progn
#   (defun input-ready-p (stream)
#     (or (has-buffered-input-p stream)
#         (handle-listen (sockint::fd->handle 
#                         (sb-impl::fd-stream-fd stream)))))
#
#   (defun has-buffered-input-p (stream)
#     (let ((ibuf (sb-impl::fd-stream-ibuf stream)))
#       (/= (sb-impl::buffer-head ibuf)
#           (sb-impl::buffer-tail ibuf))))
#
#   (sb-alien:define-alien-routine ("WSACreateEvent" wsa-create-event)
#       sb-win32:handle)
# 
#   (sb-alien:define-alien-routine ("WSACloseEvent" wsa-close-event)
#       sb-alien:int 
#     (event sb-win32:handle))
#  
#   (defconstant +fd-read+ #.(ash 1 0))
#   (defconstant +fd-close+ #.(ash 1 5))
#  
#   (sb-alien:define-alien-routine ("WSAEventSelect" wsa-event-select)
#       sb-alien:int 
#     (fd sb-alien:int) 
#     (handle sb-win32:handle)
#     (mask sb-alien:long))
#
#   (sb-alien:load-shared-object "kernel32.dll")
#   (sb-alien:define-alien-routine ("WaitForSingleObjectEx" 
#                                   wait-for-single-object-ex)
#       sb-alien:int
#     (event sb-win32:handle)
#     (milliseconds sb-alien:long)
#     (alertable sb-alien:int))
#
#   ;; see SB-WIN32:HANDLE-LISTEN
#   (defun handle-listen (handle)
#     (sb-alien:with-alien ((avail sb-win32:dword)
#                           (buf (array char #.sb-win32::input-record-size)))
#       (unless (zerop (sb-win32:peek-named-pipe handle nil 0 nil 
#                                                (sb-alien:alien-sap
#                                                 (sb-alien:addr avail))
#                                                nil))
#         (return-from handle-listen (plusp avail)))
#
#       (unless (zerop (sb-win32:peek-console-input handle
#                                                   (sb-alien:alien-sap buf)
#                                                   sb-win32::input-record-size 
#                                                   (sb-alien:alien-sap 
#                                                    (sb-alien:addr avail))))
#         (return-from handle-listen (plusp avail))))
#
#     (let ((event (wsa-create-event)))
#       (wsa-event-select handle event (logior +fd-read+ +fd-close+))
#       (let ((val (wait-for-single-object-ex event 0 0)))
#         (wsa-close-event event)
#         (unless (= val -1)
#           (return-from handle-listen (zerop val)))))
#
#     nil)
#
#   )

#### defvar *external-format-to-coding-system*
# '((:iso-8859-1 
#    "latin-1" "latin-1-unix" "iso-latin-1-unix" 
#    "iso-8859-1" "iso-8859-1-unix")
#   (:utf-8 "utf-8" "utf-8-unix")
#   (:euc-jp "euc-jp" "euc-jp-unix")
#   (:us-ascii "us-ascii" "us-ascii-unix")))

## C.f. R.M.Kreuter in <20536.1219412774@progn.net> on sbcl-general, 2008-08-22.
#### defvar *physical-pathname-host*

# def filename_to_pathname(filename):			pass
# def find_external_format(coding_system):		pass

#### make-socket-io-stream
#### accept

### Support for SBCL syntax
## SBCL's source code is riddled with #! reader macros.  Also symbols
## containing `!' have special meaning.  We have to work long and
## hard to be able to read the source.  To deal with #! reader
## macros, we use a special readtable.  The special symbols are
## converted by a condition handler.
#### feature-in-list-p
#### shebang-reader
#### defvar *shebang-readtable*
#### shebang-readtable
#### sbcl-package-p
#### sbcl-source-file-p
#### guess-readtable-for-filename
#### defvar *debootstrap-packages*
#### call-with-debootstrapping
#### defmacro with-debootstrapping

# def call_with_syntax_hooks(fn):			pass
# def default_readtable_alist():			pass

### Utilities

# def arglist(name):					pass

@defimplementation
def function_name(function):
        return function.__name__

#### defmethod declaration-arglist ((decl-identifier (eql 'optimize)))
#### defmethod type-specifier-arglist :around (typespec-operator)

#### defvar *buffer-name*
#### defvar *buffer-tmpfile*
#### defvar *buffer-offset*
#### defvar *buffer-substring*
#### defvar *previous-compiler-condition*
"Used to detect duplicates."

#### handle-notification-condition
#### signal-compiler-condition
#### real-condition
#### condition-references
#### compiler-note-location
#### compiling-from-buffer-p
#### compiling-from-file-p
#### compiling-from-generated-code-p
#### locate-compiler-note
#### brief-compiler-message-for-emacs
#### compiler-error-context
#### compiler-source-path
# def call_with_compilation_hooks(func):		pass

#### defvar *trap-load-time-warnings*
#### defun compiler-policy
#### defun (setf compiler-policy)
#### defmacro with-compiler-policy
# def swank_compile_file(input_file,
#                        output_file,
#                        load_p,
#                        external_format,
#                        policy = nil):			pass

### compile-string
## We copy the string to a temporary file in order to get adequate
## semantics for :COMPILE-TOPLEVEL and :LOAD-TOPLEVEL EVAL-WHEN forms
## which the previous approach using
##     (compile nil `(lambda () ,(read-from-string string)))
## did not provide.

# (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
#
# (sb-alien:define-alien-routine (#-win32 "tempnam" #+win32 "_tempnam" tempnam)
#     sb-alien:c-string
#   (dir sb-alien:c-string)
#   (prefix sb-alien:c-string))

#### temp-file-name
# def swank_compile_string(string,
#                          buffer = nil,
#                          position = nil,
#                          filename = nil,
#                          policy = nil):		pass

### Definitions
#### defparameter *definition-types*
#### defun definition-specifier
#### defun make-dspec
# def find_definitions(object):				pass
# def find_source_location(object):			pass

#### categorize-definition-source
#### definition-source-for-emacs
#### source-file-position
#### source-hint-snippet
#### function-source-location
# def describe_symbol_for_emacs(symbol):		pass
# def describe_definition(name, type):			pass

#+#.(swank-backend::sbcl-with-xref-p)
### progn
#### defmacro defxref
#### defxref who-calls
#### defxref who-binds
#### defxref who-sets
#### defxref who-references
#### defxref who-macroexpands
#### #+#.(swank-backend:with-symbol 'who-specializes-directly 'sb-introspect)
#### defxref who-specializes
### end-of-progn

#### source-location-for-xref-data
# def list_callers(function_name):			pass
# def list_callees(function_name):			pass

#### sanitize-xrefs
#### ignored-xref-function-names
#### function-dspec

### macroexpansion
# def macroexpand_all(form):				pass

### Debugging
## Notice that SB-EXT:*INVOKE-DEBUGGER-HOOK* is slightly stronger
## than just a hook into BREAK. In particular, it'll make
## (LET ((*DEBUGGER-HOOK* NIL)) ..error..) drop into SLDB rather
## than the native debugger. That should probably be considered a
## feature.

#### make-invoke-debugger-hook
#### set-break-hook
#### call-with-break-hook
# def install_debugger_globally(function):		pass

# def condition_extras(condition):			pass # That is to say: not many.
        # (cond #+#.(swank-backend::sbcl-with-new-stepper-p)
        #       ((typep condition 'sb-impl::step-form-condition)
        #        `((:show-frame-source 0)))
        #       ((typep condition 'sb-int:reference-condition)
        #        (let ((refs (sb-int:reference-condition-references condition)))
        #          (if refs
        #              `((:references ,(externalize-reference refs))))))))def condition_extras(cond)

#### externalize-reference
#### defvar *sldb-stack-top* <unbound>

@defimplementation
def call_with_debugging_environment(debugger_loop_fn):
        # Note that the notion of the "top frame" in CL culture
        # is opposite to its Python counterpart.
        with env.let(_sldb_stack_top_ = (_top_frame()
                                         if symbol_value("_debug_swank_backend_") or not boundp("_stack_top_hint_") else
                                         env._stack_top_hint_),
                     _stack_top_hint_ = nil):
                handler_bind(lambda: debugger_loop_fn(),
                             debug_condition = lambda condition: signal((make_condition(sldb_condition,
                                                                                        original_condition = condition))))
#+#.(swank-backend::sbcl-with-new-stepper-p)
### progn
# def activate_stepping(frame_number):			pass
# def sldb_stepper_condition_p(condition):		pass
# def sldb_step_into():					pass
# def sldb_step_next():					pass
# def sldb_step_out():					pass
### end-progn

# def call_with_debugger_hook(hook, fun):		pass

def nth_frame(index):
        frame = symbol_value("_sldb_stack_top_")
        i = index
        while i:
                frame = cl._next_frame(frame)
                i -= 1
        return frame

@defimplementation
def compute_backtrace(start, end = nil):
        """Return a list of frames starting with frame number START and
continuing to frame number END or, if END is nil, the last frame on the
stack."""
        nth_f = nth_frame(start)
        return cl._frames_upward_from(nth_f)[:(end - start - 1) if end else None]

@defimplementation
def print_frame(frame, stream):
        return cl._print_frame(frame, stream)

# def frame_restartable_p(frame):			pass # That is to say: not very.
# def frame_call(frame_number):				pass

### Code-location -> source-location translation

## If debug-block info is avaibale, we determine the file position of
## the source-path for a code-location.  If the code was compiled
## with C-c C-c, we have to search the position in the source string.
## If there's no debug-block info, we return the (less precise)
## source-location of the corresponding function.

#### code-location-source-location

## FIXME: The naming policy of source-location functions is a bit
## fuzzy: we have FUNCTION-SOURCE-LOCATION which returns the
## source-location for a function, and we also have FILE-SOURCE-LOCATION &co
## which returns the source location for a _code-location_.
##
## Maybe these should be named code-location-file-source-location,
## etc, turned into generic functions, or something. In the very
## least the names should indicate the main entry point vs. helper
## status.

#### file-source-location
#### fallback-source-location
#### lisp-source-location
#### emacs-buffer-source-location
#### source-file-source-location
#### code-location-debug-source-name
#### code-location-debug-source-created
#### code-location-debug-fun-fun
#### code-location-has-debug-block-info-p
#### stream-source-position
#### string-source-position

## source-path-file-position and friends are in swank-source-path-parser

@defimplementation
def frame_source_location(n):
        ## Swankr:
        # fun = _frame_fun(sldb_state.frames[n]) # XXX: was [n + 1]
        # name, _, srcfile, line, nlines = _fun_info(fun)[:5]
        # if not srcfile:
        #         return [keyword("error"), "no srcfile"]
        # else:
        #         return [keyword("location"),
        #                 [keyword("file"), srcfile],
        #                 [keyword("line"), line, line + nlines],
        #                 find_symbol0("nil")]
        return converting_errors_to_error_location(
                lambda: code_location_source_location(
                        frame_code_location(nth_frame(index))))

def frame_debug_vars(frame):
        "Return a vector of debug-variables in frame."
        return cl._frame_locals(frame)

def debug_var_value(var, frame, location):
        ## Was:
        # (ecase (sb-di:debug-var-validity var location)
        #   (:valid (sb-di:debug-var-value var frame))
        #   ((:invalid :unknown) ':<not-available>))
        ## However, lack of inlining and optimisations in Python make this a non-issue.
        return cl._frame_locals(frame)[var]

def debug_var_info(var):
        "Introduced by SBCL 1.0.49.76."
        # (let ((s (find-symbol "DEBUG-VAR-INFO" :sb-di)))
        #   (when (and s (fboundp s))
        #    (funcall s var)))
        pass

@defimplementation
def frame_locals(index):
        #     (let* ((frame (nth-frame index))
        #      (loc (sb-di:frame-code-location frame))
        #      (vars (frame-debug-vars frame))
        #      ;; Since SBCL 1.0.49.76 PREPROCESS-FOR-EVAL understands SB-DEBUG::MORE
        #      ;; specially.
        #      (more-name (or (find-symbol "MORE" :sb-debug) 'more))
        #      (more-context nil)
        #      (more-count nil)
        #      (more-id 0))
        # (when vars
        #   (let ((locals
        #           (loop for v across vars
        #                 do (when (eq (sb-di:debug-var-symbol v) more-name)
        #                      (incf more-id))
        #                    (case (debug-var-info v)
        #                      (:more-context
        #                       (setf more-context (debug-var-value v frame loc)))
        #                      (:more-count
        #                       (setf more-count (debug-var-value v frame loc))))
        #                 collect
        #                    (list :name (sb-di:debug-var-symbol v)
        #                          :id (sb-di:debug-var-id v)
        #                          :value (debug-var-value v frame loc)))))
        #     (when (and more-context more-count)
        #       (setf locals (append locals
        #                            (list
        #                             (list :name more-name
        #                                   :id more-id
        #                                   :value (multiple-value-list
        #                                           (sb-c:%more-arg-values more-context
        #                                                                  0 more-count)))))))
        #     locals)))
        frame = nth_frame(index)
        loc = nil                                           # XXX: was (sb-di:frame-code-location frame)
        vars = frame_debug_vars(frame)
        return mapcar(lambda kv: ["name",  kv[0],
                                  "id",    0,      # XXX: was (sb-di:debug-var-id v)
                                  "value", kv[1]], # XXX: depended upon loc
                      vars.items())

# @defimplementation
# def frame_var_value(frame, var):
#         pass

# def frame_catch_tags(index):			pass
# def eval_in_frame(form, index):		pass

#+#.(swank-backend::sbcl-with-restart-frame)
### progn
# def return_from_frame(index, form):		pass
# def restart_frame(index):			pass
### end-progn

## FIXME: this implementation doesn't unwind the stack before
## re-invoking the function, but it's better than no implementation at
## all.

#-#.(swank-backend::sbcl-with-restart-frame)
### progn
#### sb-debug-catch-tag-p
# def return_from_frame(index, form):		pass
# def restart_frame(index):			pass
### end-progn

### reference-conditions

@defimplementation
def format_sldb_condition(condition):
        return princ_to_string(condition)

### Profiling
# def profile(fname):					pass
# def unprofile(fname):					pass
# def unprofile_all():					pass
# def profile_report():					pass
# def profile_reset():					pass
# def profiled_functions():				pass
# def profile_package(package, callers_p, methods):	pass

### Inspector
#### defmethod emacs-inspect ((o t))
#### defmethod emacs-inspect ((o function))
#### defmethod emacs-inspect ((o sb-kernel:code-component))
#### defmethod emacs-inspect ((o sb-ext:weak-pointer))
#### defmethod emacs-inspect ((o sb-kernel:fdefn))
#### defmethod emacs-inspect :around ((o generic-function))

### Multiprocessing
#+(and sb-thread
       #.(swank-backend:with-symbol "THREAD-NAME" "SB-THREAD"))
### progn
# Editor's note: assuming the above condition as true.

setq("_thread_id_counter_", 0)
# see below def make_lock(): setq("_thread_id_counter_lock_", make_lock(name = "thread id counter lock"))

def next_thread_id():
        return call_with_lock_held(
                symbol_value("_thread_id_counter_lock_"),
                lambda:
                        setq("_thread_id_counter_", symbol_value("_thread_id_counter_") + 1))

setq("_thread_id_map_", dict())

## This should be a thread -> id map but as weak keys are not
## supported it is id -> map instead.
# see below def make_lock():
# setq("_thread_id_map_lock_", make_lock(name = "thread id map lock"))

@defimplementation
def spawn(fn, name = "<unnamed-thread>"):
        thread = cl._without_condition_system(
                lambda: threading.Thread(name = name))
        thread.run = lambda: cl._enable_pytracer() and fn()
        thread.start()
        return thread

@defimplementation
def thread_id(thread):
        # (block thread-id
        #   (sb-thread:with-mutex (*thread-id-map-lock*)
        #     (loop for id being the hash-key in *thread-id-map*
        #           using (hash-value thread-pointer)
        #           do
        #           (let ((maybe-thread (sb-ext:weak-pointer-value thread-pointer)))
        #             (cond ((null maybe-thread)
        #                    ;; the value is gc'd, remove it manually
        #                    (remhash id *thread-id-map*))
        #                   ((eq thread maybe-thread)
        #                    (return-from thread-id id)))))
        #     ;; lazy numbering
        #     (let ((id (next-thread-id)))
        #       (setf (gethash id *thread-id-map*) (sb-ext:make-weak-pointer thread))
        #       id))))
        @block
        def body():
                for id, thread_pointer in symbol_value("_thread_id_map_").items():
                        maybe_thread = weak_pointer_value(thread_pointer)
                        if not maybe_thread:
                                del symbol_value("_thread_id_map_")[id]
                        elif thread is maybe_thread:
                                return_from(body, id)
                # lazy numbering
                id = next_thread_id()
                symbol_value("_thread_id_map_")[id] = make_weak_pointer(thread)
                return id
        return call_with_lock_held(symbol_value("_thread_id_map_lock_"),
                                   body)

@defimplementation
def find_thread(id):
        idmap = symbol_value("_thread_id_map_")
        def purge():
                del idmap[id]
        return call_with_lock_held(
                symbol_value("_thread_id_map_lock_"),
                lambda:
                        when_let(idmap[id] if id in idmap else None,
                                 lambda thread_pointer:
                                         if_let(weak_pointer_value(thread_pointer),
                                                identity,
                                                purge))) #..the thread pointer.

@defimplementation
def thread_name(thread):
        return thread.name

@defimplementation
def thread_status(thread):
        return "Running" if thread_alive_p(thread) else "Stopped"

@defimplementation
def make_lock(name = nil):
        return threading.Lock()

setq("_thread_id_counter_lock_", make_lock(name = "thread id counter lock"))
## This should be a thread -> id map but as weak keys are not
## supported it is id -> map instead.
setq("_thread_id_map_lock_", make_lock(name = "thread id map lock"))

@defimplementation
def call_with_lock_held(lock, function):
        try:
                # format (t, "||| -> acquiring %s\n", lock); cl.backtrace()
                lock.acquire()
                return function()
        finally:
                # format (t, "||| <- releasing %s\n", lock); cl.backtrace()
                lock.release()

@defimplementation
def current_thread():
        return threading.current_thread()

@defimplementation
def all_threads():
        return threading.enumerate()

# def interrupt_thread(thread, fn):			pass
# (defimplementation interrupt-thread (thread fn)
#   (sb-thread:interrupt-thread thread fn))
#
# def kill_thread(thread):				pass
# (defimplementation kill-thread (thread)
#   (sb-thread:terminate-thread thread))

@defimplementation
def thread_alive_p(x):
        return x.is_alive()

setq("_mailbox_lock_", threading.Lock()) # (sb-thread:make-mutex :name "mailbox lock")
setq("_mailboxes_", [])

class _mailbox():
        def __init__(self, thread):
                self.thread = thread
                self.mutex = threading.Lock()
                self.waitqueue = cl._without_condition_system(lambda: threading.Condition(self.mutex))
                self.queue = []

def mailbox(thread):
        def body():
                mboxes = symbol_value("_mailboxes_")
                mbox = find(thread, mboxes, key = slotting("thread"))
                if mbox:
                        return mbox
                else:
                        mbox = _mailbox(thread)
                        mboxes.append(mbox)
                        return mbox
        return call_with_lock_held(symbol_value("_mailbox_lock_"), body)

@defimplementation
def send(thread, message):
        mbox = mailbox(thread)
        def body():
                mbox.queue.append(message)
                # here("to thread (%s): %s -> <QUEUE %x> %s" % (thread_name(thread).upper(),
                #                                               message, id(mbox.queue), mbox.queue,))
                mbox.waitqueue.notify_all()
        return call_with_lock_held(mbox.mutex,
                                   body)

#-sb-lutex
@defimplementation
def condition_timed_wait(waitqueue, mutex, timeout):
        # (handler-case
        #     (let ((*break-on-signals* nil))
        #       (sb-sys:with-deadline (:seconds timeout :override t)
        #         (sb-thread:condition-wait waitqueue mutex) t))
        #   (sb-ext:timeout ()
        #     nil)))
        waitqueue.wait(timeout)

## FIXME: with-timeout doesn't work properly on Darwin
#+sb-lutex
# (defun condition-timed-wait (waitqueue mutex timeout)
#   (declare (ignore timeout))
#   (sb-thread:condition-wait waitqueue mutex))

@defimplementation
def receive_if(test, timeout = nil):
        mbox = mailbox(current_thread())
        mutex, waitq = mbox.mutex, mbox.waitqueue
        assert(not timeout or timeout is t)
        @block
        def _receive_if():
                busywait_reported = nil
                def body():
                        check_slime_interrupts()
                        def lockbody():
                                nonlocal busywait_reported
                                q = mbox.queue
                                tail = member_if(test, q)
                                if tail:
                                        mbox.queue = ldiff(q, tail) + rest(tail)
                                        # here("returning " + str(first(tail)))
                                        return_from(_receive_if,
                                                    (first(tail), None))
                                # elif q:
                                #         here("unmatched events: %s" % (mbox.queue,))
                                if timeout is t:
                                        return_from(_receive_if,
                                                    (None, True))
                                if not busywait_reported:
                                        # here("polling <QUEUE %x>" % id(mbox.queue))
                                        busywait_reported = t
                                condition_timed_wait(waitq, mutex, 0.2)
                        call_with_lock_held(mutex, lockbody)
                here("checking for events on <QUEUE %x>.." % id(mbox.queue))
                loop(body)
        ret = _receive_if()
        # cl._backtrace()
        return ret
### end-of-progn

@defimplementation
def quit_lisp():
        exit()

### Trace implementations
##In SBCL, we have:
## (trace <name>)
## (trace :methods '<name>) #to trace all methods of the gf <name>
## (trace (method <name> <qualifier>? (<specializer>+)))
## <name> can be a normal name or a (setf name)

#### toggle-trace-aux
#### process-fspec
# def toggle_trace(spec):				pass

### Weak datastructures
# def make_weak_key_hash_table(*args, **keys):		pass
# def make_weak_value_hash_table(*args, **keys):	pass
# def hash_table_weakness(hashtable):			pass

#-win32
# def save_image(filename,
#                restart_function = constantly(t)):	pass

#+unix
### progn
#### sb-alien:define-alien-routine ("execv" sys-execv) sb-alien:int
#### execv
#### runtime-pathname
# def exec_image(image_file, args):			pass
### end-of-progn

# def make_fd_stream(fd, external_format):		pass
# def call_with_io_timeout(function, seconds = nil):	pass
# (handler-case
#       (sb-sys:with-deadline (:seconds seconds)
#         (funcall function))
#     (sb-sys:deadline-timeout ()
#       nil))

#-win32
# def background_save_image(filename,
#                           restart_function = constantly(t),
#                           completion_function = constantly(t)):
#         pass

# Issue DEINIT-LOG-OUPUT-NOT-DONE-PROBABLY-CRITICAL
#### deinit-log-output
#### pushnew 'deinit-log-output sb-ext:*save-hooks*

##### :: end of swank-sbcl.lisp
################################################################################

################################################################################
##### :: swank-gray.lisp

class gray_stream(stream):
        pass
class gray_input_stream(gray_stream):
        def __init__(self, read):
                def gray_input_stream_read(n):
                        return read(n)
                self.read = gray_input_stream_read
class gray_output_stream(gray_stream):
        def __init__(self, write):
                def gray_output_stream_write(string):
                        return write(string)
                self.write = gray_output_stream_write

@defimplementation
def make_output_stream(write_string):
        return gray_output_stream(write_string)

# def swank_gray:make_input_stream(read_string):	pass
@defimplementation
def make_input_stream(read_string):
        return gray_input_stream(read_string)

##### :: end of swank-gray.lisp
################################################################################

################################################################################
##### :: <elsewhere>.lisp

make_weak_pointer  = identity
weak_pointer_value = identity

##### :: end of <elsewhere>.lisp
################################################################################
