import sys
import more_ast
import inspect

from cl import nil, t, _keyword as keyword, functionp, constantly
from cl import typep, defstruct
from cl import mapcar, remove, member
from cl import setq, boundp, progv, symbol_value, block
from cl import return_from, handler_bind, condition, error, error_, _report_condition, warn, _not_implemented
from cl import lisp_implementation_type, write_line, probe_file

##### :: swank-backend.lisp

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
        sys.modules["swank_backend"].__dict__[fn.__name__] = fn
        return fn

def warn_unimplemented_interfaces():
        with progv("_print_pretty_", t):
                warn("These Swank interfaces are unimplemented:\n%s",
                     ", ".join(mapcar(lambda x: x.upper(),
                                      sorted(symbol_value("_unimplemented_interfaces_")))))

#### import-to-swank-mop
#### import-swank-mop-symbols

setq("_gray_stream_symbols_", [keyword("fundamental-character-output-stream"),
                               keyword("stream-write-char"),
                               keyword("stream-write-string"),
                               keyword("stream-fresh-line"),
                               keyword("stream-force-output"),
                               keyword("stream-finish-output"),
                               keyword("fundamental-character-input-stream"),
                               keyword("stream-read-char"),
                               keyword("stream-peek-char"),
                               keyword("stream-read-line"),
                               ## STREAM-FILE-POSITION is not available on all implementations, or
                               ## partially under a different name.
                               # :stream-file-posiion
                               keyword("stream-listen"),
                               keyword("stream-unread-char"),
                               keyword("stream-clear-input"),
                               keyword("stream-line-column"),
                               keyword("stream-read-char-no-hang"),
                               ## STREAM-LINE-LENGTH is an extension to gray streams that's apparently
                               ## supported by CMUCL, OpenMCL, SBCL and SCL.
                               #+(or cmu openmcl sbcl scl)
                               keyword("stream-line-length")])

#### import-from

### Utilities
#### with-struct
#### when-let
#### with-symbol

# XXX: arglists are not enforced here, if body is PASS, in which case
#      they are meaningless anyway..

### Partus extensions ???
#     Anything using anything not in the __builtins__ module is good here.

### TCP server

@definterface
def create_socket():                    pass
"Create a listening TCP socket on interface HOST and port PORT ."

@definterface
def local_port():                       pass
"Return the local port number of SOCKET."

@definterface
def close_socket():                     pass
"Close the socket SOCKET."

@definterface
def accept_connection():                pass
"""Accept a client connection on the listening socket SOCKET.  
Return a stream for the new connection."""

@definterface
def add_sigio_handler():		pass
"Call FN whenever SOCKET is readable."

@definterface
def remove_sigio_handlers():		pass
"Remove all sigio handlers for SOCKET."

@definterface
def add_fd_handler():			pass
"Call FN when Lisp is waiting for input and SOCKET is readable."

@definterface
def remove_fd_handlers():		pass
"Remove all fd-handlers for SOCKET."

@definterface
def preferred_communication_style():	pass
"Return one of the symbols :spawn, :sigio, :fd-handler, or NIL."

@definterface
def set_stream_timeout(stream, timeout):
        """Set the 'stream 'timeout.  The timeout is either the real number
specifying the timeout in seconds or 'nil for no timeout."""
        return nil

## Base condition for networking errors.
#### define-condition network-error

@definterface
def emacs_connected():
        """Hook called when the first connection from Emacs is established.
Called from the INIT-FN of the socket server that accepts the
connection.

This is intended for setting up extra context, e.g. to discover
that the calling thread is the one that interacts with Emacs."""
        return nil

#### defconstant +sigint+ 2

@definterface
def getpid():				pass
"Return the (Unix) process ID of this superior Lisp."

@definterface
def install_sigint_handler(function):
        """Call FUNCTION on SIGINT (instead of invoking the debugger).
Return old signal handler."""
        return nil

@definterface
def call_with_user_break_handler(handler, function):
        "Install the break handler HANDLER while executing FUNCTION."
        old_handler = install_sigint_handler(handler)
        try:
                return function()
        finally:
                install_sigin_handler(old_handler)

@definterface
def quit_lisp():                        pass
"Exit the current lisp image."

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
"Return the file descriptor for SOCKET-STREAM."

@definterface
def make_fd_stream():			pass
"Create a character stream for the file descriptor FD."

@definterface
def dup():				pass
"""Duplicate a file descriptor.
If the syscall fails, signal a condition.
See dup(2)."""

@definterface
def exec_image():			pass
"""Replace the current process with a new process image.
The new image is created by loading the previously dumped
core file IMAGE-FILE.
ARGS is a list of strings passed as arguments to
the new image.
This is thin wrapper around exec(3)."""

@definterface
def command_line_args():
        "Return a list of strings as passed by the OS."
        return nil

## pathnames are sooo useless

@definterface
def filename_to_pathname(filename):
        """Return a pathname for FILENAME.
A filename in Emacs may for example contain asterisks which should not
be translated to wildcards."""
        return filename # Was: (parse-namestring filename)

@definterface
def pathname_to_filename(pathname):
        "Return the filename for PATHNAME."
        return pathname # Was: (namestring pathname)

@definterface
def default_directory():
        "Return the default directory."
        return os.getcwd()

@definterface
def set_default_directory(directory):
        """Set the default directory.
This is used to resolve filenames without directory component."""
        os.chdir(directory)
        return default_directory()

@definterface
def call_with_syntax_hooks(fn):
        "Call FN with hooks to handle special syntax."
        return fn()

@definterface
def default_readtable_alist():
        "Return a suitable initial value for SWANK:*READTABLE-ALIST*."
        return []

@definterface
def call_with_compilation_hooks():	pass
"Call FUNC with hooks to record compiler conditions."

# defmacro
def with_compilation_hooks(body, *args, **keys):
        "Execute BODY as in CALL-WITH-COMPILATION-HOOKS."
        return body()

@definterface
def swank_compile_string():		pass
"""Compile source from STRING.
During compilation, compiler conditions must be trapped and
resignalled as COMPILER-CONDITIONs.

If supplied, BUFFER and POSITION specify the source location in Emacs.

Additionally, if POSITION is supplied, it must be added to source
positions reported in compiler conditions.

If FILENAME is specified it may be used by certain implementations to
rebind *DEFAULT-PATHNAME-DEFAULTS* which may improve the recording of
source information.

If POLICY is supplied, and non-NIL, it may be used by certain
implementations to compile with optimization qualities of its
value.

Should return T on successful compilation, NIL otherwise.
"""

@definterface
def swank_compile_file():		pass
"""Compile INPUT-FILE signalling COMPILE-CONDITIONs.
If LOAD-P is true, load the file after compilation.
EXTERNAL-FORMAT is a value returned by find-external-format or
:default.

If POLICY is supplied, and non-NIL, it may be used by certain
implementations to compile with optimization qualities of its
value.

Should return OUTPUT-TRUENAME, WARNINGS-P and FAILURE-p
like `compile-file'"""

#### deftype severity

## Base condition type for compiler errors, warnings and notes.
#### define-condition compiler-condition

@definterface
def find_external_format(coding_system):
        """Return a \"external file format designator\" for CODING-SYSTEM.
CODING-SYSTEM is Emacs-style coding system name (a string),
e.g. \"latin-1-unix\"."""
        # (if (equal coding-system "iso-latin-1-unix")
        #     :default
        #     nil)
        return (coding_system
                if coding_system in ["utf-8"] else
                None)

@definterface
def guess_external_format():		pass
        # Look for a Emacs-style -*- coding: ... -*- or Local Variable: section.
        # (with-open-file (s pathname :if-does-not-exist nil
        #                    :external-format (or (find-external-format "latin-1-unix")
        #                                         :default))
        #   (if s 
        #       (or (let* ((line (read-line s nil))
        #                  (p (search "-*-" line)))
        #             (when p
        #               (let* ((start (+ p (length "-*-")))
        #                      (end (search "-*-" line :start2 start)))
        #                 (when end
        #                   (%search-coding line start end)))))
        #           (let* ((len (file-length s))
        #                  (buf (make-string (min len 3000))))
        #             (file-position s (- len (length buf)))
        #             (read-sequence buf s)
        #             (let ((start (search "Local Variables:" buf :from-end t))
        #                   (end (search "End:" buf :from-end t)))
        #               (and start end (< start end)
        #                    (%search-coding buf start end))))))))
"""Detect the external format for the file with name pathname.
Return nil if the file contains no special markers."""

# (defun %search-coding (str start end)
#   (let ((p (search "coding:" str :start2 start :end2 end)))
#     (when p
#       (incf p (length "coding:"))
#       (loop while (and (< p end)
#                        (member (aref str p) '(#\space #\tab)))
#             do (incf p))
#       (let ((end (position-if (lambda (c) (find c '(#\space #\tab #\newline)))
#                               str :start p)))
#         (find-external-format (subseq str p end))))))

### Streams
@definterface
def make_output_stream():		pass
"""Return a new character output stream.
The stream calls WRITE-STRING when output is ready."""

@definterface
def make_input_stream():		pass
"""Return a new character input stream.
The stream calls READ-STRING when input is needed."""

### Documentation

@definterface
def arglist(name):
        """Return the lambda list for the symbol NAME. NAME can also be
a lisp function object, on lisps which support this.

The result can be a list or the :not-available keyword if the
arglist cannot be determined."""
        return keyword("not-available")

#### defgeneric declaration-arglist
"""Return the argument list of the declaration specifier belonging to the
declaration identifier DECL-IDENTIFIER. If the arglist cannot be determined,
the keyword :NOT-AVAILABLE is returned.

The different SWANK backends can specialize this generic function to
include implementation-dependend declaration specifiers, or to provide
additional information on the specifiers defined in ANSI Common Lisp."""
#### :method (decl-identifier)

#### defgeneric type-specifier-arglist
"""Return the argument list of the type specifier belonging to
TYPESPEC-OPERATOR.. If the arglist cannot be determined, the keyword
:NOT-AVAILABLE is returned.

The different SWANK backends can specialize this generic function to
include implementation-dependend declaration specifiers, or to provide
additional information on the specifiers defined in ANSI Common Lisp."""
#### :method (typespec-operator)

@definterface
def function_name():
        """Return the name of the function object FUNCTION.

The result is either a symbol, a list, or NIL if no function name is available."""
        return nil

@definterface
def valid_function_name_p(form):
        """Is FORM syntactically valid to name a function?
   If true, FBOUNDP should not signal a type-error for FORM."""
        ## Was:
        # (or (symbolp form)
        #     (and (consp form) (length=2 form)
        #          (eq (first form) 'setf) (symbolp (second form))))
        return (stringp(form) or
                symbolp(form))

@definterface
def macroexpand_all():			pass # XXX: originally implemented
"""Recursively expand all macros in FORM.
Return the resulting form."""

@definterface
def compiler_macroexpand_1():		pass # XXX: originally implemented
"""Call the compiler-macro for form.
If FORM is a function call for which a compiler-macro has been
defined, invoke the expander function using *macroexpand-hook* and
return the results and T.  Otherwise, return the original form and
NIL."""

@definterface
def compiler_macroexpand():		pass # XXX: originally implemented
"Repetitively call `compiler-macroexpand-1'."

@definterface
def format_string_expand():		pass # XXX: originally implemented
"Expand the format string CONTROL-STRING."

@definterface
def describe_symbol_for_emacs():	pass
"""Return a property list describing SYMBOL.

The property list has an entry for each interesting aspect of the
symbol. The recognised keys are:

  :VARIABLE :FUNCTION :SETF :SPECIAL-OPERATOR :MACRO :COMPILER-MACRO
  :TYPE :CLASS :ALIEN-TYPE :ALIEN-STRUCT :ALIEN-UNION :ALIEN-ENUM

The value of each property is the corresponding documentation string,
or :NOT-DOCUMENTED. It is legal to include keys not listed here (but
slime-print-apropos in Emacs must know about them).

Properties should be included if and only if they are applicable to
the symbol. For example, only (and all) fbound symbols should include
the :FUNCTION property.

Example:
\(describe-symbol-for-emacs 'vector)
  => (:CLASS :NOT-DOCUMENTED
      :TYPE :NOT-DOCUMENTED
      :FUNCTION \"Constructs a simple-vector from the given objects.\")"""

@definterface
def describe_definition():		pass
"""Describe the definition NAME of TYPE.
TYPE can be any value returned by DESCRIBE-SYMBOL-FOR-EMACS.

Return a documentation string, or NIL if none is available."""

@definterface
def install_debugger_globally(function):
        """Install FUNCTION as the debugger for all threads/processes. This
usually involves setting *DEBUGGER-HOOK* and, if the implementation
permits, hooking into BREAK as well."""
        setq("_debugger_hook_", function)
        return function

@definterface
def call_with_debugging_environment():  pass
"""Call DEBUGGER-LOOP-FN in a suitable debugging environment.

This function is called recursively at each debug level to invoke the
debugger loop. The purpose is to setup any necessary environment for
other debugger callbacks that will be called within the debugger loop.

For example, this is a reasonable place to compute a backtrace, switch
to safe reader/printer settings, and so on."""

@definterface
def call_with_debugger_hook(function, body):
        """Call FUN and use HOOK as debugger hook. HOOK can be NIL.

HOOK should be called for both BREAK and INVOKE-DEBUGGER."""
        with progv(_debugger_hook_ = lambda cond, hook:
                           (write_line("Debugger hook caught:") and
                            _report_condition(cond, backtrace = t) and
                            function(cond, hook))):
                return body()

class sldb_condition(condition):
        """Wrapper for conditions that should not be debugged.

When a condition arises from the internals of the debugger, it is not
desirable to debug it -- we'd risk entering an endless loop trying to
debug the debugger! Instead, such conditions can be reported to the
user without (re)entering the debugger by wrapping them as
`sldb-condition's."""
        def __init__(self, original_condition):
                self.original_condition = original_condition
        def __str__(self):
                return format(nil, "Condition in debugger code: %s", self.original_condition)

## The following functions in this section are supposed to be called
## within the dynamic contour of CALL-WITH-DEBUGGING-ENVIRONMENT only.

@definterface
def compute_backtrace():		pass
"""Returns a backtrace of the condition currently being debugged,
that is an ordered list consisting of frames. ``Ordered list''
means that an integer I can be mapped back to the i-th frame of this
backtrace.

START and END are zero-based indices constraining the number of frames
returned. Frame zero is defined as the frame which invoked the
debugger. If END is nil, return the frames from START to the end of
the stack."""

@definterface
def print_frame():			pass
"Print frame to stream."

@definterface
def frame_restartable_p(frame):
        """Is the frame FRAME restartable?.
Return T if `restart-frame' can safely be called on the frame."""
        return nil

@definterface
def frame_source_location():            pass
"Return the source location for the frame associated to FRAME-NUMBER."

@definterface
def frame_catch_tags(frame_number):
        """Return a list of catch tags for being printed in a debugger stack
frame."""
        return []

@definterface
def frame_locals():			pass
"""Return a list of ((&key NAME ID VALUE) ...) where each element of
the list represents a local variable in the stack frame associated to
FRAME-NUMBER.

NAME, a symbol; the name of the local variable.

ID, an integer; used as primary key for the local variable, unique
relatively to the frame under operation.

value, an object; the value of the local variable."""

@definterface
def frame_var_value():			pass
"""Return the value of the local variable associated to VAR-ID
relatively to the frame associated to FRAME-NUMBER."""

@definterface
def disassemble_frame():		pass
"""Disassemble the code for the FRAME-NUMBER.
The output should be written to standard output.
FRAME-NUMBER is a non-negative integer."""

@definterface
def eval_in_frame():			pass
"""Evaluate a Lisp form in the lexical context of a stack frame
in the debugger.

FRAME-NUMBER must be a positive integer with 0 indicating the
frame which invoked the debugger.

The return value is the result of evaulating FORM in the
appropriate context."""

@definterface
def frame_call():			pass
"Return a string representing a call to the entry point of a frame."

@definterface
def return_from_frame():		pass
"""Unwind the stack to the frame FRAME-NUMBER and return the value(s)
produced by evaluating FORM in the frame context to its caller.

Execute any clean-up code from unwind-protect forms above the frame
during unwinding.

Return a string describing the error if it's not possible to return
from the frame."""

@definterface
def restart_frame():			pass
"""Restart execution of the frame FRAME-NUMBER with the same arguments
as it was called originally."""

@definterface
def format_sldb_condition(condition):
        "Format a condition for display in SLDB."
        return princ_to_string(condition)

@definterface
def condition_extras(condition):
        """Return a list of extra for the debugger.
The allowed elements are of the form:
  (:SHOW-FRAME-SOURCE frame-number)
  (:REFERENCES &rest refs)
"""
        return []

@definterface
def gdb_initial_commands():
        """List of gdb commands supposed to be executed first for the
   ATTACH-GDB restart."""
        return []

@definterface
def activate_stepping():		pass
"Prepare the frame FRAME-NUMBER for stepping."

@definterface
def sldb_break_on_return():		pass
"Set a breakpoint in the frame FRAME-NUMBER."

@definterface
def sldb_break_at_start():		pass
"Set a breakpoint on the beginning of the function for SYMBOL."

@definterface
def sldb_stepper_condition_p(condition):
        """Return true if SLDB was invoked due to a single-stepping condition,
false otherwise. """
        return nil

@definterface
def sldb_step_into():			pass
"Step into the current single-stepper form."

@definterface
def sldb_step_next():			pass
"Step to the next form in the current function."

@definterface
def sldb_step_out():			pass
"""Stop single-stepping temporarily, but resume it once the current function
returns."""

### Definition finding

## Was: defstruct (location (:type list))

def make_location(buffer, position, hints = nil):
        return [keyword("location"),
                buffer,
                position,
                ## Hints is a property list optionally containing:
                ##   :snippet SOURCE-TEXT
                ##     This is a snippet of the actual source text at the start of
                ##     the definition, which could be used in a text search.
                hints]

#### defstruct error

## Valid content for BUFFER slot
#### defstruct file
#### defstruct buffer
#### defstruct etags-file

## Valid content for POSITIONS slot
#### defstruct position
#### defstruct tag

@block
def converting_errors_to_error_location(body):
        "Catches errors during BODY and converts them to an error location."
        return handler_bind(body,
                            (error_,
                             lambda e: (nil if symbol_value("_debug_swank_backend_") else
                                            return_from(converting_errors_to_error_location,
                                                        make_error_location(e)))))

def make_error_location(datum, *args):
        if typep(datum, condition):
                return [keyword("error"), "Error: %s" % (datum,)]
        elif symbolp(datum):
                return [keyword("error"), "Error: %s" % make_condition(datum, *args)]
        else:
                assert(stringp(datum))
                return [keyword("error"), "Error: %s" % format(nil, datum, *args)]

@definterface
def find_definitions():			pass
"""Return a list ((DSPEC LOCATION) ...) for NAME's definitions.

NAME is a \"definition specifier\".

DSPEC is a \"definition specifier\" describing the
definition, e.g., FOO or (METHOD FOO (STRING NUMBER)) or
\(DEFVAR FOO).

LOCATION is the source location for the definition."""

@definterface
def find_source_location(object):
        """Returns the source location of OBJECT, or NIL.

That is the source location of the underlying datastructure of
OBJECT. E.g. on a STANDARD-OBJECT, the source location of the
respective DEFCLASS definition is returned, on a STRUCTURE-CLASS the
respective DEFSTRUCT definition, and so on."""
        ## This returns one source location and not a list of locations. It's
        ## supposed to return the location of the DEFGENERIC definition on
        ## #'SOME-GENERIC-FUNCTION.
        return make_error_location("FIND-DEFINITIONS is not yet implemented on this implementation.")

@definterface
def buffer_first_change():
        "Called for effect the first time FILENAME's buffer is modified."
        return nil

### XREF

@definterface
def who_calls():
        """Return the call sites of FUNCTION-NAME (a symbol).
The results is a list ((DSPEC LOCATION) ...)."""
        return keyword("not-implemented")

@definterface
def calls_who():
        """Return the call sites of FUNCTION-NAME (a symbol).
The results is a list ((DSPEC LOCATION) ...)."""
        return keyword("not-implemented")

@definterface
def who_references():
        """Return the locations where VARIABLE-NAME (a symbol) is referenced.
See WHO-CALLS for a description of the return value."""
        return keyword("not-implemented")

@definterface
def who_binds():
        """Return the locations where VARIABLE-NAME (a symbol) is bound.
See WHO-CALLS for a description of the return value."""
        return keyword("not-implemented")

@definterface
def who_sets():
        """Return the locations where VARIABLE-NAME (a symbol) is set.
See WHO-CALLS for a description of the return value."""
        return keyword("not-implemented")

@definterface
def who_macroexpands():
        """Return the locations where MACRO-NAME (a symbol) is expanded.
See WHO-CALLS for a description of the return value."""
        return keyword("not-implemented")

@definterface
def who_specializes():
        """Return the locations where CLASS-NAME (a symbol) is specialized.
See WHO-CALLS for a description of the return value."""
        return keyword("not-implemented")

### Simpler variants.

@definterface
def list_callers():			pass
"""List the callers of FUNCTION-NAME.
This function is like WHO-CALLS except that it is expected to use
lower-level means. Whereas WHO-CALLS is usually implemented with
special compiler support, LIST-CALLERS is usually implemented by
groveling for constants in function objects throughout the heap.

The return value is as for WHO-CALLS."""

@definterface
def list_callees():			pass
"""List the functions called by FUNCTION-NAME.
See LIST-CALLERS for a description of the return value."""

### Profiling

### The following functions define a minimal profiling interface.

@definterface
def profile():				pass
"Marks symbol FNAME for profiling."

@definterface
def profiled_functions():		pass
"Returns a list of profiled functions."

@definterface
def unprofile():			pass
"Marks symbol FNAME as not profiled."

@definterface
def unprofile_all():
        "Marks all currently profiled functions as not profiled."
        for f in profiled_functions():
                unprofile(f)

@definterface
def profile_report():			pass
"Prints profile report."

@definterface
def profile_reset():			pass
"Resets profile counters."

@definterface
def profile_package():			pass
"""Wrap profiling code around all functions in PACKAGE.  If a function
is already profiled, then unprofile and reprofile (useful to notice
function redefinition.)

If CALLERS-P is T names have counts of the most common calling
functions recorded.

When called with arguments :METHODS T, profile all methods of all
generic functions having names in the given package.  Generic functions
themselves, that is, their dispatch functions, are left alone."""

### Inspector

#### defgeneric emacs-inspect
"""Explain to Emacs how to inspect OBJECT.

Returns a list specifying how to render the object for inspection.

Every element of the list must be either a string, which will be
inserted into the buffer as is, or a list of the form:

 (:value object &optional format) - Render an inspectable
 object. If format is provided it must be a string and will be
 rendered in place of the value, otherwise use princ-to-string.

 (:newline) - Render a \\n

 (:action label lambda &key (refresh t)) - Render LABEL (a text
 string) which when clicked will call LAMBDA. If REFRESH is
 non-NIL the currently inspected object will be re-inspected
 after calling the lambda.
"""

#### defmethod emacs-inspect ((object t))
"""Generic method for inspecting any kind of object.

Since we don't know how to deal with OBJECT we simply dump the
output of CL:DESCRIBE."""

@definterface
def eval_context(object):
        "Return a list of bindings corresponding to OBJECT's slots."
        return []

#### label-value-line
"""Create a control list which prints \"LABEL: VALUE\" in the inspector.
If NEWLINE is non-NIL a `(:newline)' is added to the result."""

#### defmacro label-value-line*

@definterface
def describe_primitive_type():
        "Return a string describing the primitive type of object."
        return "N/A"

### Multithreading
##
## The default implementations are sufficient for non-multiprocessing
## implementations.
@definterface
def initialize_multiprocessing(continuation):
        """Initialize multiprocessing, if necessary and then invoke CONTINUATION.

Depending on the impleimentaion, this function may never return."""
        return continuation()

@definterface
def spawn():				pass
"Create a new thread to call FN."

@definterface
def thread_id(thread):
        """Return an Emacs-parsable object to identify THREAD.

Ids should be comparable with equal, i.e.:
 (equal (thread-id <t1>) (thread-id <t2>)) <==> (eq <t1> <t2>)"""
        return thread

@definterface
def find_thread(id):
        """Return the thread for ID.
ID should be an id previously obtained with THREAD-ID.
Can return nil if the thread no longer exists."""
        return current_thread()

@definterface
def thread_name(thread):
        """Return the name of THREAD.
Thread names are short strings meaningful to the user. They do not
have to be unique."""
        return "The One True Thread"

@definterface
def thread_status(thread):
        "Return a string describing THREAD's state."
        return ""

@definterface
def thread_attributes(thread):
        "Return a plist of implementation-dependent attributes for THREAD"
        return []

@definterface
def make_lock(name = nil):
        """Make a lock for thread synchronization.
Only one thread may hold the lock (via CALL-WITH-LOCK-HELD) at a time
but that thread may hold it more than once."""
        return keyword("null-lock")

@definterface
def call_with_lock_held(lock, function):
        "Call FUNCTION with LOCK held, queueing if necessary."
        return function()

@definterface
def current_thread():
        "Return the currently executing thread."
        return 0

@definterface
def all_threads():
        "Return a fresh list of all threads."
        return []

@definterface
def thread_alive_p(thread):
        "Test if THREAD is termintated."
        # Issue: THREAD-ALIVE-P-DEFAULT-IMPLEMENTATION-POSSIBLE-BUG
        return member(thread, all_threads())

@definterface
def interrupt_thread(thread, fn):	pass
"Cause THREAD to execute FN."

@definterface
def kill_thread(thread):
        """Terminate THREAD immediately.
Don't execute unwind-protected sections, don't raise conditions.
(Do not return  go, do not collect $200.)"""
        return nil

@definterface
def send(thread, object):		pass
"Send OBJECT to thread THREAD."

@definterface
def receive(timeout = nil):
        "Return the next message from current thread's mailbox."
        return receive_if(constantly(t), timeout)

@definterface
def receive_if():			pass
"Return the first message satisfiying PREDICATE."

@definterface
def set_default_initial_binding(var, form):
        """Initialize special variable VAR by default with FORM.

Some implementations initialize certain variables in each newly
created thread.  This function sets the form which is used to produce
the initial value."""
        return setq(var, eval_(form))

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
"""Function to call on queued interrupts.
Interrupts get queued when an interrupt occurs while interrupt
handling is disabled.

Backends can use this function to abort slow operations."""

@definterface
def wait_for_input(streams, timeout = nil):
        """Wait for input on a list of streams.  Return those that are ready.
STREAMS is a list of streams
TIMEOUT nil, t, or real number. If TIMEOUT is t, return
those streams which are ready immediately, without waiting.
If TIMEOUT is a number and no streams is ready after TIMEOUT seconds,
return nil.

Return :interrupt if an interrupt occurs while waiting."""
        assert(member(timeout, [t, nil]))
        # (cond #+(or)
        #       ((null (cdr streams)) 
        #        (wait-for-one-stream (car streams) timeout))
        #       (t
        #        (wait-for-streams streams timeout)))
        return wait_for_streams(streams, timeout)


@block
def wait_for_streams(streams, timeout):
        def body():
                if check_slime_interrupts():
                        return_from(wait_for_streams, keyword("interrupt"))
                ready = remove_if_not(stream_readable_p, streams)
                if ready:
                        return_from(wait_for_streams, ready)
                if timeout:
                        return_from(wait_for_streams, nil)
                sleep(0.1)
        loop(body)

## Note: Usually we can't interrupt PEEK-CHAR cleanly.
def wait_for_one_stream(stream, timeout):
        return ecase(timeout,
                     (nil,
                      lambda: cond((check_slime_interrupts(),
                                    lambda: keyword("interrupt")),
                                   (t,
                                    prognf(lambda: peek_char(nil, stream),
                                           lambda: [stream])))),
                     (t,
                      lambda: letf(read_char_no_hang(stream, nil, nil),
                                   lambda c: cond((c,
                                                   prognf(lambda: unread_char(c, stream),
                                                          lambda: [stream])),
                                                  (t,
                                                   [])))))

def stream_readable_p(stream):
        c = read_char_no_hang(stream, nil, keyword("eof"))
        if not c:
                return nil
        elif c is keyword("eof"):
                return t
        else:
                unread_char(c, stream)
                return t

@definterface
def toggle_trace():			pass
"""Toggle tracing of the function(s) given with SPEC.
SPEC can be:
 (setf NAME)                            ; a setf function
 (:defmethod NAME QUALIFIER... (SPECIALIZER...)) ; a specific method
 (:defgeneric NAME)                     ; a generic function with all methods
 (:call CALLER CALLEE)                  ; trace calls from CALLER to CALLEE.
 (:labels TOPLEVEL LOCAL) 
 (:flet TOPLEVEL LOCAL) """

### Weak datastructures

@definterface
def make_weak_key_hash_table(*args):
        "Like MAKE-HASH-TABLE, but weak w.r.t. the keys."
        return make_hash_table(*args)

@definterface
def make_weak_value_hash_table(*args):
        "Like MAKE-HASH-TABLE, but weak w.r.t. the values."
        return make_hash_table(*args)

@definterface
def hash_table_weakness():
        "Return nil or one of :key :value :key-or-value :key-and-value"
        return nil

### Character names

@definterface
def character_completion_set(prefix, matchp):
        "Return a list of names of characters that match PREFIX."
        # Handle the standard and semi-standard characters.
        return [ x
                 for x in ["Newline" "Space" "Tab" "Page" "Rubout"
                           "Linefeed" "Return" "Backspace" ]
                 if matchp(prefix, name) ]

#### defparameter *type-specifier-arglists*

### Heap dumps

@definterface
def save_image():			pass
"""Save a heap image to the file FILENAME.
RESTART-FUNCTION, if non-nil, should be called when the image is loaded."""

@definterface
def background_save_image():		pass
"""Request saving a heap image to the file FILENAME.
RESTART-FUNCTION, if non-nil, should be called when the image is loaded.
COMPLETION-FUNCTION, if non-nil, should be called after saving the image."""

### Codepoint length

@definterface
def codepoint_length(string):
        """Return the number of codepoints in STRING.
With some Lisps, like cmucl, LENGTH returns the number of UTF-16 code
units, but other Lisps return the number of codepoints. The slime
protocol wants string lengths in terms of codepoints."""
        return len(string)

### Timeouts

@definterface
def call_with_io_timeout(function, seconds):
        "Calls function with the specified IO timeout."
        return function()
