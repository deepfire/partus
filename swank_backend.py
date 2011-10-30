import sys
import more_ast
import inspect

from cl import setq, nil, t, boundp, symbol_value, mapcar, remove, warn, _not_implemented, env, _keyword, functionp, error, write_line, _report_condition#, format
from cl import lisp_implementation_type, probe_file

setq("_debug_swank_backend_",      nil)
"""If this is true, backends should not catch errors but enter the
debugger where appropriate. Also, they should not perform backtrace
magic but really show every frame including SWANK related ones."""

setq("_interface_functions_",      [])
"""The names of all interface functions."""

setq("_unimplemented_interfaces_", [])
"""List of interface functions that are not implemented.
DEFINTERFACE adds to this list and DEFIMPLEMENTATION removes."""

def definterface(*args, **keys):
        """Define an interface function for the backend to implement.
A function is defined with NAME, ARGS, and DOCUMENTATION.  This
function first looks for a function to call in NAME's property list
that is indicated by 'IMPLEMENTATION; failing that, it looks for a
function indicated by 'DEFAULT. If neither is present, an error is
signaled.

If a DEFAULT-BODY is supplied, then a function with the same body and
ARGS will be added to NAME's property list as the property indicated
by 'DEFAULT.

Backends implement these functions using DEFIMPLEMENTATION."""
        if not keys and args and len(args) == 1 and functionp(args[0]):
                fn, name = args[0], args[0].__name__
                symbol_value("_interface_functions_").append(name)
                fn_body_ast = more_ast.extract_ast(inspect.getsource(fn)).body[0].body
                if not fn_body_ast:
                        error("DEFINTERFACE %s: function has no body.", name)
                if fn_body_ast and len(fn_body_ast) == 1 and more_ast.ast_pass_p(fn_body_ast[0]):
                        symbol_value("_unimplemented_interfaces_").append(name)
                        return lambda *_, **__: _not_implemented(name)
                else:
                        return fn
        else:
                error("DEFINTERFACE decorator does not accept arguments.")

def defimplementation(fn):
        setq("_unimplemented_interfaces_", remove(fn.__name__, symbol_value("_unimplemented_interfaces_")))
        sys.modules['swank_backend'].__dict__[fn.__name__] = fn
        return fn

def warn_unimplemented_interfaces():
        with env.let("_print_pretty_", t):
                warn("These Swank interfaces are unimplemented:\n%s",
                     ", ".join(mapcar(lambda x: x.upper(),
                                      sorted(symbol_value("_unimplemented_interfaces_")))))

#### import-to-swank-mop
#### import-swank-mop-symbols
setq("_gray_stream_symbols_", [_keyword("fundamental-character-output-stream"),
                               _keyword("stream-write-char"),
                               _keyword("stream-write-string"),
                               _keyword("stream-fresh-line"),
                               _keyword("stream-force-output"),
                               _keyword("stream-finish-output"),
                               _keyword("fundamental-character-input-stream"),
                               _keyword("stream-read-char"),
                               _keyword("stream-peek-char"),
                               _keyword("stream-read-line"),
                               ## STREAM-FILE-POSITION is not available on all implementations, or
                               ## partially under a different name.
                               # :stream-file-posiion
                               _keyword("stream-listen"),
                               _keyword("stream-unread-char"),
                               _keyword("stream-clear-input"),
                               _keyword("stream-line-column"),
                               _keyword("stream-read-char-no-hang"),
                               ## STREAM-LINE-LENGTH is an extension to gray streams that's apparently
                               ## supported by CMUCL, OpenMCL, SBCL and SCL.
                               #+(or cmu openmcl sbcl scl)
                               _keyword("stream-line-length")])
#### import-from
### Utilities
#### with-struct
#### when-let
#### with-symbol

# XXX: arglists are not enforced here, if body is PASS, and in such a case
#      they are meaningless..

### Partus extensions
#     Anything using anything not in the __builtins__ module is good here.

### Original Slime backend definitions
@definterface
def create_socket():                    pass
@definterface
def local_port():                       pass
@definterface
def close_socket():                     pass
@definterface
def accept_connection():                pass
@definterface
def add_sigio_handler():		pass
@definterface
def remove_sigio_handlers():		pass
@definterface
def add_fd_handler():			pass
@definterface
def remove_fd_handlers():		pass
@definterface
def preferred_communication_style():	pass
@definterface
def set_stream_timeout():		pass
#### define-condition network-error
@definterface
def emacs_connected():			pass
#### defconstant +sigint+
@definterface
def getpid():				pass
@definterface
def install_sigint_handler():		pass
@definterface
def call_with_user_break_handler():	pass
@definterface
def quit_lisp():                        pass

@definterface
def lisp_implementation_type_name():
        "Return a short name for the Lisp implementation."
        return lisp_implementation_type()

@definterface
def lisp_implementation_program():
        "Return the argv[0] of the running Lisp process, or NIL."
        file = sys.argv[0]
        if file and probe_file(file):
                return file # XXX: was: (namestring (truename file))

@definterface
def socket_fd():			pass
@definterface
def make_fd_stream():			pass
@definterface
def dup():				pass
@definterface
def exec_image():			pass
@definterface
def command_line_args():		pass
@definterface
def filename_to_pathname():		pass
@definterface
def pathname_to_filename():		pass
@definterface
def default_directory():                pass
@definterface
def set_default_directory():            pass
@definterface
def call_with_syntax_hooks():		pass
@definterface
def default_readtable_alist():		pass
@definterface
def call_with_compilation_hooks():	pass
#### defmacro with-compilation-hooks
@definterface
def swank_compile_string():		pass
@definterface
def swank_compile_file():		pass
#### deftype severity
#### define-condition compiler-condition
@definterface
def find_external_format():		pass
@definterface
def guess_external_format():		pass
#### %search-coding
@definterface
def make_output_stream():		pass
@definterface
def make_input_stream():		pass
@definterface
def arglist():				pass
#### defgeneric declaration-arglist
#### defgeneric type-specifier-arglist
@definterface
def function_name():			pass
@definterface
def valid_function_name_p():		pass
@definterface
def macroexpand_all():			pass
@definterface
def compiler_macroexpand_1():		pass
@definterface
def compiler_macroexpand():		pass
@definterface
def format_string_expand():		pass
@definterface
def describe_symbol_for_emacs():	pass
@definterface
def describe_definition():		pass

@definterface
def install_debugger_globally(function):
        """Install FUNCTION as the debugger for all threads/processes. This
usually involves setting *DEBUGGER-HOOK* and, if the implementation
permits, hooking into BREAK as well."""
        setq("_debugger_hook_", function)

@definterface
def call_with_debugging_environment():  pass

@definterface
def call_with_debugger_hook(function, body):
        with env.let(_debugger_hook_ = lambda cond, hook:
                             (write_line("Debugger hook caught:") and
                              _report_condition(cond, backtrace = t) and
                              function(cond, hook))):
                return body()

#### define-condition sldb-condition
@definterface
def compute_backtrace():		pass
@definterface
def print_frame():			pass
@definterface
def frame_restartable_p():		pass
@definterface
def frame_source_location():            pass
@definterface
def frame_catch_tags():			pass
@definterface
def frame_locals():			pass
@definterface
def frame_var_value():			pass
@definterface
def disassemble_frame():		pass
@definterface
def eval_in_frame():			pass
@definterface
def frame_call():			pass
@definterface
def return_from_frame():		pass
@definterface
def restart_frame():			pass
@definterface
def format_sldb_condition():		pass
@definterface
def condition_extras():                 pass
@definterface
def gdb_initial_commands():		pass
@definterface
def activate_stepping():		pass
@definterface
def sldb_break_on_return():		pass
@definterface
def sldb_break_at_start():		pass
@definterface
def sldb_stepper_condition_p():		pass
@definterface
def sldb_step_into():			pass
@definterface
def sldb_step_next():			pass
@definterface
def sldb_step_out():			pass
#### defstruct location
#### defstruct error
#### defstruct file
#### defstruct buffer
#### defstruct etags-file
#### defstruct position
#### defstruct tag
#### defmacro converting-errors-to-error-location
#### make-error-location
@definterface
def find_definitions():			pass
@definterface
def find_source_location():		pass
@definterface
def buffer_first_change():              pass
@definterface
def who_calls():			pass
@definterface
def calls_who():			pass
@definterface
def who_references():			pass
@definterface
def who_binds():			pass
@definterface
def who_sets():				pass
@definterface
def who_macroexpands():			pass
@definterface
def who_specializes():			pass
@definterface
def list_callers():			pass
@definterface
def list_callees():			pass
@definterface
def profile():				pass
@definterface
def profiled_functions():		pass
@definterface
def unprofile():			pass
@definterface
def unprofile_all():			pass
@definterface
def profile_report():			pass
@definterface
def profile_reset():			pass
@definterface
def profile_package():			pass
#### defgeneric emacs-inspect
#### defmethod emacs-inspect ((object t))
@definterface
def eval_context():			pass
#### label-value-line
#### defmacro label-value-line*
@definterface
def describe_primitive_type():		pass

@definterface
def initialize_multiprocessing(fn):
        return fn()

@definterface
def spawn():				pass
@definterface
def thread_id():			pass
@definterface
def find_thread():			pass
@definterface
def thread_name():			pass
@definterface
def thread_status():			pass
@definterface
def thread_attributes():		pass
@definterface
def make_lock():			pass
@definterface
def call_with_lock_held():		pass
@definterface
def current_thread():			pass
@definterface
def all_threads():			pass
@definterface
def thread_alive_p():			pass
@definterface
def interrupt_thread():			pass
@definterface
def kill_thread():			pass
@definterface
def send():				pass
@definterface
def receive():				pass
@definterface
def receive_if():			pass
@definterface
def set_default_initial_binding():	pass

## List of delayed interrupts.
## This should only have thread-local bindings, so no init form.
#### defvar *pending-slime-interrupts* UNBOUND

def check_slime_interrupts():
        """Execute pending interrupts if any.
This should be called periodically in operations which
can take a long time to complete.
Return a boolean indicating whether any interrupts was processed."""
        if (boundp("_pending_slime_interrupts_") and
            symbol_value("_pending_slime_interrupts_")):
                symbol_value("_pending_slime_interrupts_").pop()
                return True

setq("_interrupt_queued_handler_", nil)

@definterface
def wait_for_input():			pass
#### wait-for-streams
#### wait-for-one-stream
#### stream-readable-p
@definterface
def toggle_trace():			pass
@definterface
def make_weak_key_hash_table():		pass
@definterface
def make_weak_value_hash_table():	pass
@definterface
def hash_table_weakness():		pass
@definterface
#### defparameter *type-specifier-arglists*
def character_completion_set():		pass
@definterface
def save_image():			pass
@definterface
def background_save_image():		pass
@definterface
def codepoint_length():			pass
@definterface
def call_with_io_timeout():		pass
