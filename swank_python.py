import os
import inspect
import socket
import threading

import cl

from cl import env, setq, symbol_value, t, nil, format, find
from cl import block, return_from, handler_bind, signal, make_condition
from cl import _top_frame, _frame_fun, _fun_info
from cl import _keyword

from pergamum import slotting

from swank_backend import defimplementation

def not_implemented():
        raise NotImplemented("Not implemented: %s.", inspect.stack()[1][3].upper())

@defimplementation
def create_socket(address, port):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.bind((address, port))
        socket_ = sock.makefile()
        socket_.sock = sock
        return socket_

@defimplementation
def local_port(socket):
        return socket.sock.getsockname()[1]

@defimplementation
def close_socket(socket):
        socket.sock.close()

@defimplementation
def accept_connection(socket, external_format = "utf-8", buffering = "full", timeout = None):
        # XXX: socket buffering ought to be honored
        socket.sock.listen(0)
        client_sock, _ = socket.sock.accept()
        client_socket = client_sock.makefile(encoding = external_format)
        client_socket.sock = client_sock
        return client_socket

# def add_sigio_handler(socket, fn):			pass
# def remove_sigio_handlers(socket):			pass
# def add_fd_handler(socket, fn):			pass
# def remove_fd_handlers(socket):			pass

@defimplementation
def preferred_communication_style():
        return _keyword("spawn")

# def set_stream_timeout(stream, timeout):		pass

@defimplementation
def emacs_connected():
        """Hook called when the first connection from Emacs is established.
Called from the INIT-FN of the socket server that accepts the
connection.

This is intended for setting up extra context, e.g. to discover
that the calling thread is the one that interacts with Emacs."""
        return None

@defimplementation
def getpid():
        return os.getpid()

# def install_sigint_handler(function):			pass
# def call_with_user_break_handler(handler, function):	pass

@defimplementation
def quit_lisp(slime_connection, sldb_state):
        exit()

# def lisp_implementation_type_name():			pass
# def lisp_implementation_program():			pass
# def socket_fd(socket_stream):				pass
# def make_fd_stream(fd, external_format):		pass
# def dup(fd):						pass
# def exec_image(image_file, args):			pass
# def command_line_args():				pass
# def filename_to_pathname(filename):			pass
# def pathname_to_filename(pathname):			pass

@defimplementation
def default_directory(slime_connection, sldb_state):
        return os.getcwd()

@defimplementation
def set_default_directory(slime_connection, sldb_state, directory):
        os.chdir(directory)
        return default_directory(slime_connection, sldb_state)

# def call_with_syntax_hooks(fn):			pass
# def default_readtable_alist():			pass
# def call_with_compilation_hooks(func):		pass
# def swank_compile_string(string,
#                          buffer = None,
#                          position = None,
#                          filename = None,
#                          policy = None):		pass
# def swank_compile_file(input_file,
#                        output_file,
#                        load_p,
#                        external_format,
#                        policy = None):		pass

@defimplementation
def find_external_format(coding_system):
        return (coding_system
                if coding_system in ['utf-8'] else
                None)

# def guess_external_format(pathname):			pass
# def make_output_stream(write_string):			pass
# def make_input_stream(read_string):			pass
# def arglist(name):					pass

@defimplementation
def function_name(function):
        return function.__name__

# def valid_function_name_p(form):			pass
# def macroexpand_all(form):				pass
# def compiler_macroexpand_1(form, env = None):		pass
# def compiler_macroexpand(form, env = None):		pass
# def format_string_expand(control_string):		pass
# def describe_symbol_for_emacs(symbol):		pass
# def describe_definition(name, type):			pass
# def install_debugger_globally(function):		pass

# setq("_sldb_stack_top_", None)

@defimplementation
def call_with_debugging_environment(debugger_loop_fn):
        with env.let(_sldb_stack_top_ = (_top_frame()
                                         if symbol_value("_debug_swank_backend_") or not env.boundp("_stack_top_hint_") else
                                         env._stack_top_hint_),
                     _stack_top_hint_ = None):
                handler_bind(lambda: debugger_loop_fn(),
                             debug_condition = lambda condition: signal((make_condition(sldb_condition,
                                                                                        original_condition = condition))))

# def call_with_debugger_hook(hook, fun):		pass
# def compute_backtrace(start, end):			pass
# def print_frame(frame, stream):			pass
# def frame_restartable_p(frame):			pass

@defimplementation
def frame_source_location(n):
        fun = _frame_fun(sldb_state.frames[n]) # XXX: was [n + 1]
        name, _, srcfile, line, nlines = _fun_info(fun)[:5]
        if not srcfile:
                return [_keyword('error'), "no srcfile"]
        else:
                return [_keyword('location'),
                        [_keyword('file'), srcfile],
                        [_keyword('line'), line, line + nlines],
                        find_symbol0('nil')]

# def frame_catch_tags(frame_number):			pass
# def frame_locals(frame_number):			pass
# def frame_var_value(frame_number, var_id):		pass
# def disassemble_frame(frame_number):			pass
# def eval_in_frame(form, frame_number):		pass
# def frame_call(frame_number):				pass
# def return_from_frame(frame_number, form):		pass
# def restart_frame(frame_number):			pass
# def format_sldb_condition(condition):			pass

# (defimplementation condition-extras (condition)
#   (cond #+#.(swank-backend::sbcl-with-new-stepper-p)
#         ((typep condition 'sb-impl::step-form-condition)
#          `((:show-frame-source 0)))
#         ((typep condition 'sb-int:reference-condition)
#          (let ((refs (sb-int:reference-condition-references condition)))
#            (if refs
#                `((:references ,(externalize-reference refs))))))))def condition_extras(cond)
@defimplementation
def condition_extras(condition):
        return []

# def gdb_initial_commands():				pass
# def activate_stepping(frame_number):			pass
# def sldb_break_on_return(frame_number):		pass
# def sldb_break_at_start(symbol):			pass
# def sldb_stepper_condition_p(condition):		pass
# def sldb_step_into():					pass
# def sldb_step_next():					pass
# def sldb_step_out():					pass
# def find_definitions(object):				pass
# def find_source_location(object):			pass
# def buffer_first_change(filename):			pass
# def who_calls(function_name):				pass
# def calls_who(function_name):				pass
# def who_references(variable_name):			pass
# def who_binds(variable_name):				pass
# def who_sets(variable_name):				pass
# def who_macroexpands(macro_name):			pass
# def who_specializes(class_name):			pass
# def list_callers(function_name):			pass
# def list_callees(function_name):			pass
# def profile(fname):					pass
# def profiled_functions():				pass
# def unprofile(fname):					pass
# def unprofile_all():					pass
# def profile_report():					pass
# def profile_reset():					pass
# def profile_package(package, callers_p, methods):	pass
# def eval_context(object):				pass
# def describe_primitive_type(object):			pass
# def initialize_multiprocessing(continuation):		pass

@defimplementation
def spawn(fn, name = "<unnamed-thread>"):
        thread = threading.Thread(name = name)
        thread.run = lambda: cl.enable_pytracer() and fn()
        thread.start()
        return thread

# (progn
#   (defvar *thread-id-counter* 0)
#
#   (defvar *thread-id-counter-lock*
#     (sb-thread:make-mutex :name "thread id counter lock"))
#
#   (defun next-thread-id ()
#     (sb-thread:with-mutex (*thread-id-counter-lock*)
#       (incf *thread-id-counter*)))
#
#   (defparameter *thread-id-map* (make-hash-table))
#
#   ;; This should be a thread -> id map but as weak keys are not
#   ;; supported it is id -> map instead.
#   (defvar *thread-id-map-lock*
#     (sb-thread:make-mutex :name "thread id map lock"))
#
#   (defimplementation spawn (fn &key name)
#     (sb-thread:make-thread fn :name name))
#
#   (defimplementation thread-id (thread)
#     (block thread-id
#       (sb-thread:with-mutex (*thread-id-map-lock*)
#         (loop for id being the hash-key in *thread-id-map*
#               using (hash-value thread-pointer)
#               do
#               (let ((maybe-thread (sb-ext:weak-pointer-value thread-pointer)))
#                 (cond ((null maybe-thread)
#                        ;; the value is gc'd, remove it manually
#                        (remhash id *thread-id-map*))
#                       ((eq thread maybe-thread)
#                        (return-from thread-id id)))))
#         ;; lazy numbering
#         (let ((id (next-thread-id)))
#           (setf (gethash id *thread-id-map*) (sb-ext:make-weak-pointer thread))
#           id))))
#
#   (defimplementation find-thread (id)
#     (sb-thread:with-mutex (*thread-id-map-lock*)
#       (let ((thread-pointer (gethash id *thread-id-map*)))
#         (if thread-pointer
#             (let ((maybe-thread (sb-ext:weak-pointer-value thread-pointer)))
#               (if maybe-thread
#                   maybe-thread
#                   ;; the value is gc'd, remove it manually
#                   (progn
#                     (remhash id *thread-id-map*)
#                     nil)))
#             nil))))
#
#   (defimplementation thread-name (thread)
#     ;; sometimes the name is not a string (e.g. NIL)
#     (princ-to-string (sb-thread:thread-name thread)))
#
#   (defimplementation thread-status (thread)
#     (if (sb-thread:thread-alive-p thread)
#         "Running"
#         "Stopped"))
# 
#   (defimplementation make-lock (&key name)
#     (sb-thread:make-mutex :name name))
#
#   (defimplementation call-with-lock-held (lock function)
#     (declare (type function function))
#     (sb-thread:with-recursive-lock (lock) (funcall function)))
#
#   (defimplementation current-thread ()
#     sb-thread:*current-thread*)
#
#   (defimplementation all-threads ()
#     (sb-thread:list-all-threads))
#
#   (defimplementation interrupt-thread (thread fn)
#     (sb-thread:interrupt-thread thread fn))
#
#   (defimplementation kill-thread (thread)
#     (sb-thread:terminate-thread thread))
#
#   (defimplementation thread-alive-p (thread)
#     (sb-thread:thread-alive-p thread))
#
#   (defvar *mailbox-lock* (sb-thread:make-mutex :name "mailbox lock"))
#   (defvar *mailboxes* (list))
#   (declaim (type list *mailboxes*))
#
#   (defstruct (mailbox (:conc-name mailbox.))
#     thread
#     (mutex (sb-thread:make-mutex))
#     (waitqueue  (sb-thread:make-waitqueue))
#     (queue '() :type list))
#
#   (defun mailbox (thread)
#     "Return THREAD's mailbox."
#     (sb-thread:with-mutex (*mailbox-lock*)
#       (or (find thread *mailboxes* :key #'mailbox.thread)
#           (let ((mb (make-mailbox :thread thread)))
#             (push mb *mailboxes*)
#             mb))))
#
#   (defimplementation send (thread message)
#     (let* ((mbox (mailbox thread))
#            (mutex (mailbox.mutex mbox)))
#       (sb-thread:with-mutex (mutex)
#         (setf (mailbox.queue mbox)
#               (nconc (mailbox.queue mbox) (list message)))
#         (sb-thread:condition-broadcast (mailbox.waitqueue mbox)))))
#   #-sb-lutex
#   (defun condition-timed-wait (waitqueue mutex timeout)
#     (handler-case 
#         (let ((*break-on-signals* nil))
#           (sb-sys:with-deadline (:seconds timeout :override t)
#             (sb-thread:condition-wait waitqueue mutex) t))
#       (sb-ext:timeout ()
#         nil)))
#
#   ;; FIXME: with-timeout doesn't work properly on Darwin
#   #+sb-lutex
#   (defun condition-timed-wait (waitqueue mutex timeout)
#     (declare (ignore timeout))
#     (sb-thread:condition-wait waitqueue mutex))
# 
#   (defimplementation receive-if (test &optional timeout)
#     (let* ((mbox (mailbox (current-thread)))
#            (mutex (mailbox.mutex mbox))
#            (waitq (mailbox.waitqueue mbox)))
#       (assert (or (not timeout) (eq timeout t)))
#       (loop
#        (check-slime-interrupts)
#        (sb-thread:with-mutex (mutex)
#          (let* ((q (mailbox.queue mbox))
#                 (tail (member-if test q)))
#            (when tail 
#              (setf (mailbox.queue mbox) (nconc (ldiff q tail) (cdr tail)))
#              (return (car tail))))
#          (when (eq timeout t) (return (values nil t)))
#          (condition-timed-wait waitq mutex 0.2)))))
#   )
# def thread_id(thread):				pass
# def find_thread(id):					pass

@defimplementation
def thread_name(thread):
        return thread.name

# def thread_status(thread):				pass
# def thread_attributes(thread):			pass

@defimplementation
def make_lock(name = None):
        return threading.Lock()

@defimplementation
def call_with_lock_held(lock, function):
        try:
                lock.acquire()
                return function()
        finally:
                lock.release()

@defimplementation
def current_thread():
        return threading.current_thread()

@defimplementation
def all_threads():
        return threading.enumerate()

@defimplementation
def thread_alive_p(x):
        return x.is_alive()

# def interrupt_thread(thread, fn):			pass
# def kill_thread(thread):				pass

setq("_mailbox_lock_", threading.Lock()) # (sb-thread:make-mutex :name "mailbox lock")
setq("_mailboxes_", [])

class _mailbox():
        def __init__(self, thread):
                self.thread = thread
                self.mutex = threading.Lock()
                self.waitqueue = threading.Condition(self.mutex)
                self.queue = []

def mailbox(thread):
        def body():
                mboxes = symbol_value("_mailboxes_")
                mbox = find(thread, mboxes, key = slotting(thread))
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
        return call_with_lock_held(mbox.mutex,
                                   lambda: mbox.queue.append(message))

@defimplementation
  # #-sb-lutex
  # (defun condition-timed-wait (waitqueue mutex timeout)
  #   (handler-case 
  #       (let ((*break-on-signals* nil))
  #         (sb-sys:with-deadline (:seconds timeout :override t)
  #           (sb-thread:condition-wait waitqueue mutex) t))
  #     (sb-ext:timeout ()
  #       nil)))

  # ;; FIXME: with-timeout doesn't work properly on Darwin
  # #+sb-lutex
  # (defun condition-timed-wait (waitqueue mutex timeout)
  #   (declare (ignore timeout))
  #   (sb-thread:condition-wait waitqueue mutex))
def condition_timed_wait(waitqueue, mutex, timeout):
        threading.condition_wait(waitqueue, mutex)

@defimplementation 
@block
def receive_if(test, timeout = None):
        mbox = mailbox(current_thread())
        mutex, waitq = mbox.mutex, mbox.waitqueue
        assert(not timeout or timeout is t)
        def body():
                check_slime_interrupts()
                def lockbody():
                        q = mbox.queue
                        tail = member_if(test, q)
                        if tail:
                                mbox.queue = ldiff(q, tail) + rest(tail)
                                return_from(receive_if,
                                            (first(tail), None))
                        if timeout is t:
                                return_from(receive_if,
                                            (None, True))
                        condition_timed_wait(waitq, mutex, 0.2)
                call_with_lock_held(lockbody)
        loop(body)

# def receive(timeout = None):				pass
# def receive_if(predicate, timeout = None):		pass
# def set_default_initial_binding(var, form):		pass
# def wait_for_input(streams, timeout = None):		pass
# def toggle_trace(spec):				pass
# def make_weak_key_hash_table(*args, **keys):		pass
# def make_weak_value_hash_table(*args, **keys):	pass
# def hash_table_weakness(hashtable):			pass
# def character_completion_set(prefix, matchp):		pass
# def save_image(filename,
#                restart_function = constantly(t)):	pass
# def background_save_image(filename,
#                           restart_function = constantly(t),
#                           completion_function = constantly(t)):
#         pass
# def codepoint_length(string):				pass
# def call_with_io_timeout(function, seconds = None):	pass
