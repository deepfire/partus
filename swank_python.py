import os
import inspect
import socket
import threading

import cl

from cl import env, handler_bind, symbol_value, signal, make_condition, t, nil, format
from cl import _top_frame, _frame_fun, _fun_info
from cl import _keyword

from swank_backend import defimplementation

def not_implemented():
        raise NotImplemented("Not implemented: %s.", inspect.stack()[1][3].upper())

@defimplementation
def create_socket(address, port):
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.bind((address, port))
        return sock

@defimplementation
def local_port(sock):
        return sock.getsockname()[1]

@defimplementation
def close_socket(socket):
        socket.close()

@defimplementation
def accept_connection(socket, external_format = "utf-8", buffering = "full", timeout = None):
        # XXX: socket external format and buffering ought to be honored
        socket.listen(0)
        client, _ = socket.accept()
        return client

# def add_sigio_handler():		pass
# def remove_sigio_handlers():		pass
# def add_fd_handler():			pass
# def remove_fd_handlers():		pass

@defimplementation
def preferred_communication_style():
        return _keyword("spawn")

# def set_stream_timeout():		pass
# def emacs_connected():			pass

@defimplementation
def getpid():
        return os.getpid()

# def install_sigint_handler():		pass
# def call_with_user_break_handler():	pass

@defimplementation
def quit_lisp(slime_connection, sldb_state):
        exit()

# def lisp_implementation_type_name():	pass
# def lisp_implementation_program():	pass
# def socket_fd():			pass
# def make_fd_stream():			pass
# def dup():				pass
# def exec_image():			pass
# def command_line_args():		pass
# def filename_to_pathname():		pass
# def pathname_to_filename():		pass

@defimplementation
def default_directory(slime_connection, sldb_state):
        return os.getcwd()

@defimplementation
def set_default_directory(slime_connection, sldb_state, directory):
        os.chdir(directory)
        return default_directory(slime_connection, sldb_state)

# def call_with_syntax_hooks():		pass
# def default_readtable_alist():		pass
# def call_with_compilation_hooks():	pass
# def swank_compile_string():		pass
# def swank_compile_file():		pass
# def find_external_format():		pass
# def guess_external_format():		pass
# def make_output_stream():		pass
# def make_input_stream():		pass
# def arglist():				pass
# def function_name():			pass
# def valid_function_name_p():		pass
# def macroexpand_all():			pass
# def compiler_macroexpand_1():		pass
# def compiler_macroexpand():		pass
# def format_string_expand():		pass
# def describe_symbol_for_emacs():	pass
# def describe_definition():		pass
# def install_debugger_globally():	pass

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

# def call_with_debugger_hook():		pass
# def compute_backtrace():		pass
# def print_frame():			pass
# def frame_restartable_p():		pass

@defimplementation
def frame_source_location(slime_connection, sldb_state, n):
        fun = _frame_fun(sldb_state.frames[n]) # XXX: was [n + 1]
        name, _, srcfile, line, nlines = _fun_info(fun)[:5]
        if not srcfile:
                return [_keyword('error'), "no srcfile"]
        else:
                return [_keyword('location'),
                        [_keyword('file'), srcfile],
                        [_keyword('line'), line, line + nlines],
                        find_symbol0('nil')]

# def frame_catch_tags():			pass
# def frame_locals():			pass
# def frame_var_value():			pass
# def disassemble_frame():		pass
# def eval_in_frame():			pass
# def frame_call():			pass
# def return_from_frame():		pass
# def restart_frame():			pass
# def format_sldb_condition():		pass

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

# def gdb_initial_commands():		pass
# def activate_stepping():		pass
# def sldb_break_on_return():		pass
# def sldb_break_at_start():		pass
# def sldb_stepper_condition_p():		pass
# def sldb_step_into():			pass
# def sldb_step_next():			pass
# def sldb_step_out():			pass
# def find_definitions():			pass
# def find_source_location():		pass

@defimplementation
def buffer_first_change(slime_connection, sldb_state, filename):
        return

# def who_calls():			pass
# def calls_who():			pass
# def who_references():			pass
# def who_binds():			pass
# def who_sets():				pass
# def who_macroexpands():			pass
# def who_specializes():			pass
# def list_callers():			pass
# def list_callees():			pass
# def profile():				pass
# def profiled_functions():		pass
# def unprofile():			pass
# def unprofile_all():			pass
# def profile_report():			pass
# def profile_reset():			pass
# def profile_package():			pass
# def eval_context():			pass
# def describe_primitive_type():		pass
# def initialize_multiprocessing():	pass
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
# def thread_id():			pass
# def find_thread():			pass
# def thread_name():			pass
# def thread_status():			pass
# def thread_attributes():		pass
# def make_lock():			pass
# def call_with_lock_held():		pass
# def current_thread():			pass
# def all_threads():			pass
# def thread_alive_p():			pass
# def interrupt_thread():			pass
# def kill_thread():			pass
# def send():				pass
# def receive():				pass
# def receive_if():			pass
# def set_default_initial_binding():	pass
# def wait_for_input():			pass
# def toggle_trace():			pass
# def make_weak_key_hash_table():		pass
# def make_weak_value_hash_table():	pass
# def hash_table_weakness():		pass
# def character_completion_set():		pass
# def save_image():			pass
# def background_save_image():		pass
# def codepoint_length():			pass
# def call_with_io_timeout():		pass
