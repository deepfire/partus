import inspect

import cl

from cl       import *
from pergamum import *

from cl import _keyword as keyword, _intern0 as intern0

import sb_c

from sb_c import debug_source_from, debug_source_namestring # XXX

###
### Conditions
###
class debug_condition(condition):
        """All DEBUG-CONDITIONs inherit from this type. These are serious conditions
that must be handled, but they are not programmer errors."""
        pass

class no_debug_blocks(debug_condition):
        "The DEBUG-FUN has no DEBUG-BLOCK information."
        pass

class no_debug_vars(debug_condition):
        "The DEBUG-FUN has no DEBUG-VAR information."
        pass

class lambda_list_unavailable(debug_condition):
        "The DEBUG-FUN has no lambda list since argument DEBUG-VARs are unavailable."
        pass

class invalid_value(debug_condition):
        pass

class ambiguous_var_name(debug_condition):
        pass

#### errors and DEBUG-SIGNAL

### The debug-internals code tries to signal all programmer errors as
### subtypes of DEBUG-ERROR. There are calls to ERROR signalling
### SIMPLE-ERRORs, but these dummy checks in the code and shouldn't
### come up.
###
### While under development, this code also signals errors in code
### branches that remain unimplemented.

class debug_error(error_):
        """All programmer errors from using the interface for building debugging
tools inherit from this type."""
        pass

class unhandled_debug_condition(debug_error):
        def __init__(self, condition = None):
                assert(condition)
                self.condition = condition
        def __str__(self):
                return "\nunhandled DEBUG-CONDITION:\n%s" % (self.condition,)

class unknown_code_location(debug_error):
        pass

class unknown_debug_var(debug_error):
        pass

# class invalid_control_stack_pointer(debug_error):
#         pass

# class frame_fun_mismatch(debug_error):
#         pass

### This signals debug-conditions. If they go unhandled, then signal
### an UNHANDLED-DEBUG-CONDITION error.
###
### ??? Get SIGNAL in the right package!
def debug_signal(datum, *arguments):
        condition = make_condition(datum, *arguments)
        signal(condition)
        error(unhandled_debug_condition, condition = condition)

#### structures
####
#### Most of these structures model information stored in internal
#### data structures created by the compiler. Whenever comments
#### preface an object or type with "compiler", they refer to the
#### internal compiler thing, not to the object or type with the same
#### name in the "SB-DI" package.

#### DEBUG-VARs

### These exist for caching data stored in packed binary form in
### compiler DEBUG-FUNs.
debug_var = defstruct("debug_var",
                      ## the name of the variable
                      "symbol",
                      ## a unique integer identification relative to other variables with the same
                      ## symbol
                      "id",
                      ## Does the variable always have a valid value?
                      "alive_p")

def debug_var_id(debug_var):
        """Return the integer that makes DEBUG-VAR's name and package unique
with respect to other DEBUG-VARs in the same function."""
        return dvar.id

#### frames

#### These represent call frames on the stack.
# (defstruct (frame (:constructor nil)
#                   (:copier nil))
#   ## the next frame up, or NIL when top frame
#   (up nil :type (or frame null))
#   ## the previous frame down, or NIL when the bottom frame. Before
#   ## computing the next frame down, this slot holds the frame pointer
#   ## to the control stack for the given frame. This lets us get the
#   ## next frame down and the return-pc for that frame.
#   (%down :unparsed :type (or frame (member nil :unparsed)))
#   ## the DEBUG-FUN for the function whose call this frame represents
#   (debug-fun nil :type debug-fun)
#   ## the CODE-LOCATION where the frame's DEBUG-FUN will continue
#   ## running when program execution returns to this frame. If someone
#   ## interrupted this frame, the result could be an unknown
#   ## CODE-LOCATION.
#   (code-location nil :type code-location)
#   ## an a-list of catch-tags to code-locations
#   (%catches :unparsed :type (or list (member :unparsed)))
#   ## pointer to frame on control stack (unexported)
#   pointer
#   ## This is the frame's number for prompt printing. Top is zero.
#   (number 0 :type index))

#### DEBUG-FUNs

### These exist for caching data stored in packed binary form in
### compiler DEBUG-FUNs. *COMPILED-DEBUG-FUNS* maps a SB!C::DEBUG-FUN
### to a DEBUG-FUN. There should only be one DEBUG-FUN in existence
### for any function# that is, all CODE-LOCATIONs and other objects
### that reference DEBUG-FUNs point to unique objects. This is
### due to the overhead in cached information.
# (defstruct (debug-fun (:constructor nil)
#                       (:copier nil))
#   ## some representation of the function arguments. See
#   ## DEBUG-FUN-LAMBDA-LIST.
#   ## NOTE: must parse vars before parsing arg list stuff.
#   (%lambda-list :unparsed)
#   ## cached DEBUG-VARS information (unexported).
#   ## These are sorted by their name.
#   (%debug-vars :unparsed :type (or simple-vector null (member :unparsed)))
#   ## cached debug-block information. This is NIL when we have tried to
#   ## parse the packed binary info, but none is available.
#   (blocks :unparsed :type (or simple-vector null (member :unparsed)))
#   ## the actual function if available
#   (%function :unparsed :type (or null function (member :unparsed))))

#### DEBUG-BLOCKs

### These exist for caching data stored in packed binary form in compiler
### DEBUG-BLOCKs.
# (defstruct (debug-block (:constructor nil)
#                         (:copier nil))
#   ## Code-locations where execution continues after this block.
#   (successors nil :type list)
#   ## This indicates whether the block is a special glob of code shared
#   ## by various functions and tucked away elsewhere in a component.
#   ## This kind of block has no start code-location. This slot is in
#   ## all debug-blocks since it is an exported interface.
#   (elsewhere-p nil :type boolean))

def debug_block_successors(db):
        """Return the list of possible code-locations where execution may continue
when the basic-block represented by debug-block completes its execution."""
        not_implemented()

def debug_block_elsewhere_p(db):
        "Return whether debug-block represents elsewhere code."
        not_implemented()

#### breakpoints

### This is an internal structure that manages information about a
### breakpoint locations. See *COMPONENT-BREAKPOINT-OFFSETS*.
# (defstruct (breakpoint-data (:constructor make-breakpoint-data
#                                           (component offset))
#                             (:copier nil))
#   ## This is the component in which the breakpoint lies.
#   component
#   ## This is the byte offset into the component.
#   (offset nil :type index)
#   ## The original instruction replaced by the breakpoint.
#   (instruction nil :type (or null sb!vm::word))
#   ## A list of user breakpoints at this location.
#   (breakpoints nil :type list))

# (defstruct (breakpoint (:constructor %make-breakpoint
#                                      (hook-fun what kind %info))
#                        (:copier nil))
#   ## This is the function invoked when execution encounters the
#   ## breakpoint. It takes a frame, the breakpoint, and optionally a
#   ## list of values. Values are supplied for :FUN-END breakpoints as
#   ## values to return for the function containing the breakpoint.
#   ## :FUN-END breakpoint hook functions also take a cookie argument.
#   ## See the COOKIE-FUN slot.
#   (hook-fun (required-arg) :type function)
#   ## CODE-LOCATION or DEBUG-FUN
#   (what nil :type (or code-location debug-fun))
#   ## :CODE-LOCATION, :FUN-START, or :FUN-END for that kind
#   ## of breakpoint. :UNKNOWN-RETURN-PARTNER if this is the partner of
#   ## a :code-location breakpoint at an :UNKNOWN-RETURN code-location.
#   (kind nil :type (member :code-location :fun-start :fun-end
#                           :unknown-return-partner))
#   ## Status helps the user and the implementation.
#   (status :inactive :type (member :active :inactive :deleted))
#   ## This is a backpointer to a breakpoint-data.
#   (internal-data nil :type (or null breakpoint-data))
#   ## With code-locations whose type is :UNKNOWN-RETURN, there are
#   ## really two breakpoints: one at the multiple-value entry point,
#   ## and one at the single-value entry point. This slot holds the
#   ## breakpoint for the other one, or NIL if this isn't at an
#   ## :UNKNOWN-RETURN code location.
#   (unknown-return-partner nil :type (or null breakpoint))
#   ## :FUN-END breakpoints use a breakpoint at the :FUN-START
#   ## to establish the end breakpoint upon function entry. We do this
#   ## by frobbing the LRA to jump to a special piece of code that
#   ## breaks and provides the return values for the returnee. This slot
#   ## points to the start breakpoint, so we can activate, deactivate,
#   ## and delete it.
#   (start-helper nil :type (or null breakpoint))
#   ## This is a hook users supply to get a dynamically unique cookie
#   ## for identifying :FUN-END breakpoint executions. That is, if
#   ## there is one :FUN-END breakpoint, but there may be multiple
#   ## pending calls of its function on the stack. This function takes
#   ## the cookie, and the hook function takes the cookie too.
#   (cookie-fun nil :type (or null function))
#   ## This slot users can set with whatever information they find useful.
#   %info)

#### CODE-LOCATIONs

code_location = defstruct("code_location",
                          ## the DEBUG-FUN containing this CODE-LOCATION
                          "debug_fun",       # nil :type debug-fun
                          ## This is initially :UNSURE. Upon first trying to access an
                          ## :UNPARSED slot, if the data is unavailable, then this becomes T,
                          ## and the code-location is unknown. If the data is available, this
                          ## becomes NIL, a known location. We can't use a separate type
                          ## code-location for this since we must return code-locations before
                          ## we can tell whether they're known or unknown. For example, when
                          ## parsing the stack, we don't want to unpack all the variables and
                          ## blocks just to make frames.
                          "unknown_p",       # :unsure :type (member t nil :unsure)
                          ## the DEBUG-BLOCK containing CODE-LOCATION. XXX Possibly toss this
                          ## out and just find it in the blocks cache in DEBUG-FUN.
                          "debug_block",     # :unparsed :type (or debug-block (member :unparsed))
                          ## This is the number of forms processed by the compiler or loader
                          ## before the top level form containing this code-location.
                          "tlf_offset",      # :unparsed :type (or index (member :unparsed))
                          ## This is the depth-first number of the node that begins
                          ## code-location within its top level form.
                          "form_number",     # :unparsed :type (or index (member :unparsed))

                          ## Our cargo-cult imitation..
                          "lineno",)

#### DEBUG-SOURCEs

### Return the number of top level forms processed by the compiler
### before compiling this source. If this source is uncompiled, this
### is zero. This may be zero even if the source is compiled since the
### first form in the first file compiled in one compilation, for
### example, must have a root number of zero -- the compiler saw no
### other top level forms before it.
def debug_source_root_number(debug_source):
        return sb_c.debug_source_source_root(debug_source)

###
### Globals
###
setq("_stack_top_hint_", nil)

###
### Code
###
def top_frame():
        return cl._caller_frame()

## Flush all of the frames above FRAME, and renumber all the frames
## below FRAME.
#### defun flush-frames-above
#### defun find-saved-frame-down

def frame_down(f):
        # BOOT: CL:ERROR
        return cl._next_frame(f)

## Get the old FP or return PC out of FRAME. STACK-SLOT is the
## standard save location offset on the stack. LOC is the saved
## SC-OFFSET describing the main location.
#### defun get-context-value
#### defun (setf get-context-value)
#### defun foreign-function-backtrace-name

## This returns a frame for the one existing in time immediately
## prior to the frame referenced by current-fp. This is current-fp's
## caller or the next frame down the control stack. If there is no
## down frame, this returns NIL for the bottom of the stack. UP-FRAME
## is the up link for the resulting frame object, and it is null when
## we call this to get the top of the stack.
##
## The current frame contains the pointer to the temporally previous
## frame we want, and the current frame contains the pc at which we
## will continue executing upon returning to that previous frame.
##
## Note: Sometimes LRA is actually a fixnum. This happens when lisp
## calls into C. In this case, the code object is stored on the stack
## after the LRA, and the LRA is the word offset.

#### defun compute-calling-frame
#### defun nth-interrupt-context
#### defun find-escaped-frame
#### defun find-pc-from-assembly-fun

## Find the code object corresponding to the object represented by
## bits and return it. We assume bogus functions correspond to the
## undefined-function.
#### defun code-object-from-bits

### frame utilities

## This returns a COMPILED-DEBUG-FUN for COMPONENT and PC. We fetch the
## SB!C::DEBUG-INFO and run down its FUN-MAP to get a
## SB!C::COMPILED-DEBUG-FUN from the PC. The result only needs to
## reference the COMPONENT, for function constants, and the
## SB!C::COMPILED-DEBUG-FUN.

#### defun debug-fun-from-pc

## This returns a code-location for the COMPILED-DEBUG-FUN,
## DEBUG-FUN, and the pc into its code vector. If we stopped at a
## breakpoint, find the CODE-LOCATION for that breakpoint. Otherwise,
## make an :UNSURE code location, so it can be filled in when we
## figure out what is going on.

#### defun code-location-from-pc

def frame_catches():
        # Return an alist mapping catch tags to CODE-LOCATIONs. These are
        # CODE-LOCATIONs at which execution would continue with frame as the
        # top frame if someone threw to the corresponding tag.
        return not_implemented()

## Modify the value of the OLD-TAG catches in FRAME to NEW-TAG
#### defun replace-frame-catch-tag

### operations on DEBUG-FUNs

## Execute the forms in a context with BLOCK-VAR bound to each
## DEBUG-BLOCK in DEBUG-FUN successively. Result is an optional
## form to execute for return values, and DO-DEBUG-FUN-BLOCKS
## returns nil if there is no result form. This signals a
## NO-DEBUG-BLOCKS condition when the DEBUG-FUN lacks
## DEBUG-BLOCK information.
#### defmacro do-debug-fun-blocks

## Execute body in a context with VAR bound to each DEBUG-VAR in
## DEBUG-FUN. This returns the value of executing result (defaults to
## nil). This may iterate over only some of DEBUG-FUN's variables or
## none depending on debug policy# for example, possibly the
## compilation only preserved argument information.
#### defmacro do-debug-fun-vars

## Return the object of type FUNCTION associated with the DEBUG-FUN,
## or NIL if the function is unavailable or is non-existent as a user
## callable function object.
#### defun debug-fun-fun

## Return the name of the function represented by DEBUG-FUN. This may
## be a string or a cons# do not assume it is a symbol.
#### defun debug-fun-name

## Return a DEBUG-FUN that represents debug information for FUN.
#### fun-debug-fun

def find_stepped_frame():
        # Return the frame that triggered a single-step condition. Used to
        # provide a *STACK-TOP-HINT*.
        return not_implemented()

def frame_code_location(f):
        return code_location(## the DEBUG-FUN containing this CODE-LOCATION
                             # "debug_fun",       # nil :type debug-fun
                             debug_fun = f.f_code,
                             #
                             ## This is initially :UNSURE. Upon first trying to access an
                             ## :UNPARSED slot, if the data is unavailable, then this becomes T,
                             ## and the code-location is unknown. If the data is available, this
                             ## becomes NIL, a known location. We can't use a separate type
                             ## code-location for this since we must return code-locations before
                             ## we can tell whether they're known or unknown. For example, when
                             ## parsing the stack, we don't want to unpack all the variables and
                             ## blocks just to make frames.
                             # "unknown_p",       # :unsure :type (member t nil :unsure)
                             unknown_p   = keyword("unsure"),
                             #
                             ## the DEBUG-BLOCK containing CODE-LOCATION. XXX Possibly toss this
                             ## out and just find it in the blocks cache in DEBUG-FUN.
                             # "debug_block",     # :unparsed :type (or debug-block (member :unparsed))
                             debug_block = keyword("unparsed"),
                             #
                             ## This is the number of forms processed by the compiler or loader
                             ## before the top level form containing this code-location.
                             # "tlf_offset",      # :unparsed :type (or index (member :unparsed))
                             tlf_offset  = keyword("unparsed"),
                             #
                             ## This is the depth-first number of the node that begins
                             ## code-location within its top level form.
                             # "form_number",     # :unparsed :type (or index (member :unparsed))
                             form_number = keyword("unparsed"),
                             #
                             # ..our measly substitute..
                             lineno      = f.f_lineno)

###
def code_location_toplevel_form_offset(l):
        # Returns the number of top level forms before the one containing
        # CODE-LOCATION as seen by the compiler in some compilation unit. (A
        # compilation unit is not necessarily a single file, see the section
        # on debug-sources.)
        return not_implemented()

def code_location_form_number(l):
        # Return the number of the form corresponding to CODE-LOCATION. The
        # form number is derived by a walking the subforms of a top level
        # form in depth-first order.
        return not_implemented()

def form_number_translations(tlf):
        # This code produces and uses what we call source-paths. A
        # source-path is a list whose first element is a form number as
        # returned by CODE-LOCATION-FORM-NUMBER and whose last element is a
        # top level form number as returned by
        # CODE-LOCATION-TOPLEVEL-FORM-NUMBER. The elements from the last to
        # the first, exclusively, are the numbered subforms into which to
        # descend. For example:
        #    (defun foo (x)
        #      (let ((a (aref x 3)))
        #     (cons a 3)))
        # The call to AREF in this example is form number 5. Assuming this
        # DEFUN is the 11'th top level form, the source-path for the AREF
        # call is as follows:
        #    (5 1 0 1 3 11)
        # Given the DEFUN, 3 gets you the LET, 1 gets you the bindings, 0
        # gets the first binding, and 1 gets the AREF form.
        #
        # This returns a table mapping form numbers to source-paths. A
        # source-path indicates a descent into the TOPLEVEL-FORM form,
        # going directly to the subform corressponding to the form number.
        #
        # The vector elements are in the same format as the compiler's
        # NODE-SOURCE-PATH; that is, the first element is the form number and
        # the last is the TOPLEVEL-FORM number.
        return not_implemented()

def code_location_source_form(l):
        return not_implemented()

def maybe_block_start_location(l):
        # If LOC is an unknown location, then try to find the block start
        # location. Used by source printing to some information instead of
        # none for the user.
        return not_implemented()

def code_location_debug_fun(l):
        return l.debug_fun

def code_location_debug_source(l):
        # (let ((info (compiled-debug-fun-debug-info
        #                (code-location-debug-fun code-location))))
        #     (or (sb!c::debug-info-source info)
        #         (debug-signal 'no-debug-blocks :debug-fun
        #                       (code-location-debug-fun code-location))))
        # return inspect.getsource(l.debug_fun)
        filename = l.debug_fun.co_filename
        exists_p = probe_file(filename)
        namestring, created, compiled = ((filename,
                                          file_write_date(filename),
                                          file_write_date(filename)) if exists_p else
                                         # no backing file...
                                         # XXX: really ought to deal with this..
                                         (nil,
                                          nil,
                                          get_universal_time()))
        return sb_c.debug_source(
                namestring = filename,
                created = created,
                source_root = 0,
                start_positions = [],
                form = nil,
                function = l.debug_fun,
                compiled = compiled,
                # Mind this (in swank_python.py, code_location_source_location()):
                # getf(plist, keyword("emacs-buffer"))
                plist = [])

def code_location_debug_block(basic_code_location):
        # Return the DEBUG-BLOCK containing code-location if it is available.
        # Some debug policies inhibit debug-block information, and if none
        # is available, then this signals a NO-DEBUG-BLOCKS condition.
        ## XXX: ought to be:
        # block = basic_code_location.debug_block
        # return (etypecase(basic_code_location,
        #                   (compiled_code_location,
        #                    lambda: compute_compiled_code_location_debug_block(basic_code_location)))
        #         if block is keyword("unparsed") else
        #         block)
        ## -- we're identity, instead
        return basic_code_location

###
def debug_fun_debug_vars(df):
        return not_implemented()

###
def debug_var_validity(var, frame):
        return keyword("valid") # options: "invalid", "unknown"

def debug_var_value(var, frame):
        return cl._frame_locals(frame)[var]

def debug_var_info(var):
        return nil # XXX: not_implemented() would be more fair?

def debug_var_symbol(var):
        return intern0(the(string, var)) # XXX: not_implemented() would be more fair?

def debug_var_id(var):
        return 0 # XXX: not_implemented() would be more fair..

def preprocess_for_eval():
        return not_implemented()

def indirect_value_cell_p():
        return not_implemented()

def disassemble_memory():
        return not_implemented()

def disassemble_code_component():
        return not_implemented()

def align():
        return not_implemented()

# def ():
#        return not_implemented()
##############################
