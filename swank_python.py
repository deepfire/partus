import os
import inspect
import socket
import threading

import cl

from cl import env, identity, setq, symbol_value, progv, boundp, t, nil, format, find, member_if, remove_if_not, constantly, loop, ldiff, rest, first
from cl import block, return_from, handler_bind, signal, make_condition, write_line, princ_to_string, stream
from cl import _top_frame, _frame_fun, _fun_info
from cl import _keyword

from pergamum import slotting, here

from swank_backend import defimplementation
from swank_backend import check_slime_interrupts

def not_implemented():
        raise NotImplemented("Not implemented: %s.", inspect.stack()[1][3].upper())

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
#                          buffer = nil,
#                          position = nil,
#                          filename = nil,
#                          policy = nil):		pass
# def swank_compile_file(input_file,
#                        output_file,
#                        load_p,
#                        external_format,
#                        policy = nil):		pass

@defimplementation
def find_external_format(coding_system):
        return (coding_system
                if coding_system in ['utf-8'] else
                None)

# def guess_external_format(pathname):			pass

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

# def swank_gray:make_output_stream(write_string):	pass
@defimplementation
def make_output_stream(write_string):
        return gray_output_stream(write_string)

# def swank_gray:make_input_stream(read_string):	pass
@defimplementation
def make_input_stream(read_string):
        return gray_input_stream(read_string)

# def arglist(name):					pass

@defimplementation
def function_name(function):
        return function.__name__

# def valid_function_name_p(form):			pass
# def macroexpand_all(form):				pass
# def compiler_macroexpand_1(form, env = nil):		pass
# def compiler_macroexpand(form, env = nil):		pass
# def format_string_expand(control_string):		pass
# def describe_symbol_for_emacs(symbol):		pass
# def describe_definition(name, type):			pass
# def install_debugger_globally(function):		pass

# setq("_sldb_stack_top_", None)

@defimplementation
def call_with_debugging_environment(debugger_loop_fn):
        with env.let(_sldb_stack_top_ = (_top_frame()
                                         if symbol_value("_debug_swank_backend_") or not boundp("_stack_top_hint_") else
                                         env._stack_top_hint_),
                     _stack_top_hint_ = nil):
                handler_bind(lambda: debugger_loop_fn(),
                             debug_condition = lambda condition: signal((make_condition(sldb_condition,
                                                                                        original_condition = condition))))

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
        return [nth_f] + cl._frames_upward_from(nth_f)[:(end - start - 1) if end else None]
                

@defimplementation
def print_frame(frame, stream):
        return cl._print_frame(frame, stream)

@defimplementation
def frame_restartable_p(frame):
        return nil

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

@defimplementation
def format_sldb_condition(condition):
        return princ_to_string(condition)

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
        thread = cl._without_condition_system(
                lambda: threading.Thread(name = name))
        thread.run = lambda: cl._enable_pytracer() and fn()
        thread.start()
        return thread

setq("_thread_id_counter_", 0)
# see below def make_lock(): setq("_thread_id_counter_lock_", make_lock(name = "thread id counter lock"))

def next_thread_id():
        return call_with_lock_held(
                symbol_value("_thread_id_counter_lock_"),
                lambda:
                        setq("_thread_id_counter_", symbol_value("_thread_id_counter_") + 1))

setq("_thread_id_map_", dict())
# see below def make_lock(): setq("_thread_id_map_lock_", make_lock(name = "thread id map lock"))

make_weak_pointer  = identity
weak_pointer_value = identity

@defimplementation
def thread_id(thread):
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
#   (defimplementation interrupt-thread (thread fn)
#     (sb-thread:interrupt-thread thread fn))
#
#   (defimplementation kill-thread (thread)
#     (sb-thread:terminate-thread thread))

@defimplementation
def thread_name(thread):
        return thread.name

@defimplementation
def thread_status(thread):
        return "Running" if thread_alive_p(thread) else "Stopped"

# def thread_attributes(thread):			pass

@defimplementation
def make_lock(name = nil):
        return threading.Lock()

setq("_thread_id_counter_lock_", make_lock(name = "thread id counter lock"))
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
                self.waitqueue = cl._without_condition_system(lambda: threading.Condition(self.mutex))
                self.queue = []

def mailbox(thread):
        def body():
                mboxes = symbol_value("_mailboxes_")
                mbox = find(thread, mboxes, key = slotting('thread'))
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
                # here("%s -> %x %s" % (message, id(mbox), mbox.queue,))
                mbox.waitqueue.notify_all()
        return call_with_lock_held(mbox.mutex,
                                   body)

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
        waitqueue.wait(timeout)

@defimplementation
def receive(timeout = nil):
        return receive_if(constantly(t), timeout)

@defimplementation
def receive_if(test, timeout = nil):
        mbox = mailbox(current_thread())
        mutex, waitq = mbox.mutex, mbox.waitqueue
        assert(not timeout or timeout is t)
        @block
        def _receive_if():
                def body():
                        check_slime_interrupts()
                        def lockbody():
                                q = mbox.queue
                                tail = member_if(test, q)
                                if tail:
                                        mbox.queue = ldiff(q, tail) + rest(tail)
                                        return_from(_receive_if,
                                                    (first(tail), None))
                                if timeout is t:
                                        return_from(_receive_if,
                                                    (None, True))
                                condition_timed_wait(waitq, mutex, 0.2)
                        call_with_lock_held(mutex, lockbody)
                loop(body)
        ret = _receive_if()
        # here("returning " + str(ret))
        # cl._backtrace()
        return ret

# def set_default_initial_binding(var, form):		pass

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

def input_ready_p(stream):
        return cl._coerce_to_stream(stream).readable() # XXX!
# #-win32
# (defun input-ready-p (stream)
#   (let ((c (read-char-no-hang stream nil :eof)))
#     (etypecase c
#       (character (unread-char c stream) t)
#       (null nil)
#       ((member :eof) t))))

# #+win32
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

@defimplementation
def codepoint_length(string):
        return len(string)

# def call_with_io_timeout(function, seconds = nil):	pass
