import inspect

import cl

from cl       import *
from pergamum import *

from cl import _keyword as keyword, _intern0 as intern0, _here as here

import ast
import more_ast

import sb_c

from sb_c import debug_source_from, debug_source_namestring # XXX

# more or less src/code/debug-int.lisp

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

def debug_var_symbol(dvar):
        return intern0(the(string, dvar)) # XXX: not_implemented() would be more fair?

def debug_var_id(dvar):
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
### for any function; that is, all CODE-LOCATIONs and other objects
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

class code_location():
        def __init__(self, debug_fun, unknown_p, debug_block, tlf_offset, form_number,
                     lineno, source, tlf):
                ## the DEBUG-FUN containing this CODE-LOCATION
                (self.debug_fun,
                 ## This is initially :UNSURE. Upon first trying to access an
                 ## :UNPARSED slot, if the data is unavailable, then this becomes T,
                 ## and the code-location is unknown. If the data is available, this
                 ## becomes NIL, a known location. We can't use a separate type
                 ## code-location for this since we must return code-locations before
                 ## we can tell whether they're known or unknown. For example, when
                 ## parsing the stack, we don't want to unpack all the variables and
                 ## blocks just to make frames.
                 self.unknown_p,
                 ## the DEBUG-BLOCK containing CODE-LOCATION. XXX Possibly toss this
                 ## out and just find it in the blocks cache in DEBUG-FUN.
                 self.debug_block,
                 ## This is the number of forms processed by the compiler or loader
                 ## before the top level form containing this code-location.
                 self.tlf_offset,
                 ## This is the depth-first number of the node that begins
                 ## code-location within its top level form.
                 self.form_number,
                 ## Our cargo-cult imitation..
                 self.lineno,
                 self.source,
                 self.tlf) = (debug_fun,
                              unknown_p,
                              debug_block,
                              tlf_offset,
                              form_number,
                              lineno,
                              source,
                              tlf)

def code_location_debug_fun(l): return l.debug_fun

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

### Return the kind of the function, which is one of :OPTIONAL,
### :EXTERNAL, :TOPLEVEL, :CLEANUP, or NIL.
#### debug-fun-kind (debug-fun)

### Is there any variable information for DEBUG-FUN?
#### debug-var-info-available (debug-fun)

### Return a list of DEBUG-VARs in DEBUG-FUN having the same name
### and package as SYMBOL. If SYMBOL is uninterned, then this returns
### a list of DEBUG-VARs without package names and with the same name
### as symbol. The result of this function is limited to the
### availability of variable information in DEBUG-FUN# for
### example, possibly DEBUG-FUN only knows about its arguments.
#### debug-fun-symbol-vars (debug-fun symbol)

### Return a list of DEBUG-VARs in DEBUG-FUN whose names contain
### NAME-PREFIX-STRING as an initial substring. The result of this
### function is limited to the availability of variable information in
### debug-fun# for example, possibly debug-fun only knows
### about its arguments.
#### ambiguous-debug-vars (debug-fun name-prefix-string)

### This returns a position in VARIABLES for one containing NAME as an
### initial substring. END is the length of VARIABLES if supplied.
#### find-var (name variables &optional end)

### Return a list representing the lambda-list for DEBUG-FUN. The
### list has the following structure:
###   (required-var1 required-var2
###    ...
###    (:optional var3 suppliedp-var4)
###    (:optional var5)
###    ...
###    (:rest var6) (:rest var7)
###    ...
###    (:keyword keyword-symbol var8 suppliedp-var9)
###    (:keyword keyword-symbol var10)
###    ...
###   )
### Each VARi is a DEBUG-VAR# however it may be the symbol :DELETED if
### it is unreferenced in DEBUG-FUN. This signals a
### LAMBDA-LIST-UNAVAILABLE condition when there is no argument list
### information.
#### debug-fun-lambda-list (debug-fun)

### Note: If this has to compute the lambda list, it caches it in DEBUG-FUN.
#### compiled-debug-fun-lambda-list

### COMPILED-DEBUG-FUN-LAMBDA-LIST calls this when a
### COMPILED-DEBUG-FUN has no lambda list information cached. It
### returns the lambda list as the first value and whether there was
### any argument information as the second value. Therefore,
### (VALUES NIL T) means there were no arguments, but (VALUES NIL NIL)
### means there was no argument information.
#### parse-compiled-debug-fun-lambda-list (debug-fun)

### This is used in COMPILED-DEBUG-FUN-LAMBDA-LIST.
#### compiled-debug-fun-lambda-list-var (args i vars)
#### compiled-debug-fun-debug-info (debug-fun)

### unpacking variable and basic block data
#### defvar *parsing-buffer*
#### defvar *other-parsing-buffer*

### PARSE-DEBUG-BLOCKS and PARSE-DEBUG-VARS
### use this to unpack binary encoded information. It returns the
### values returned by the last form in body.
###
### This binds buffer-var to *parsing-buffer*, makes sure it starts at
### element zero, and makes sure if we unwind, we nil out any set
### elements for GC purposes.
###
### This also binds other-var to *other-parsing-buffer* when it is
### supplied, making sure it starts at element zero and that we nil
### out any elements if we unwind.
###
### This defines the local macro RESULT that takes a buffer, copies
### its elements to a resulting simple-vector, nil's out elements, and
### restarts the buffer at element zero. RESULT returns the
### simple-vector.

### The argument is a debug internals structure. This returns the
### DEBUG-BLOCKs for DEBUG-FUN, regardless of whether we have unpacked
### them yet. It signals a NO-DEBUG-BLOCKS condition if it can't
### return the blocks.
#### debug-fun-debug-blocks (debug-fun)

### Return a SIMPLE-VECTOR of DEBUG-BLOCKs or NIL. NIL indicates there
### was no basic block information.
#### parse-debug-blocks (debug-fun)

### This does some of the work of PARSE-DEBUG-BLOCKS.
#### parse-compiled-debug-blocks (debug-fun)

### The argument is a debug internals structure. This returns NIL if
### there is no variable information. It returns an empty
### simple-vector if there were no locals in the function. Otherwise
### it returns a SIMPLE-VECTOR of DEBUG-VARs.
def debug_fun_debug_vars(debug_fun):
        return not_implemented()

### VARS is the parsed variables for a minimal debug function. We need
### to assign names of the form ARG-NNN. We must pad with leading
### zeros, since the arguments must be in alphabetical order.
#### assign_minimal_var_names (vars)

### Parse the packed representation of DEBUG-VARs from
### DEBUG-FUN's SB!C::COMPILED-DEBUG-FUN, returning a vector
### of DEBUG-VARs, or NIL if there was no information to parse.
#### parse-compiled-debug-vars (debug-fun)

#### CODE-LOCATIONs

### If we're sure of whether code-location is known, return T or NIL.
### If we're :UNSURE, then try to fill in the code-location's slots.
### This determines whether there is any debug-block information, and
### if code-location is known.
###
### ??? IF this conses closures every time it's called, then break off the
### :UNSURE part to get the HANDLER-CASE into another function.
def code_location_unknown_p(basic_code_location):
        def try_fill_in():
                basic_code_location.unknown_p = handler_case(
                        lambda:
                                not fill_in_code_location(basic_code_location),
                        (no_debug_blocks,
                         lambda _: t))
                return basic_code_location.unknown_p
        return ecase(basic_code_location.unknown_p,
                     ([t, True],    t),
                     ([nil, False], nil),
                     (keyword("unsure"),
                      try_fill_in()))

### Return the DEBUG-BLOCK containing code-location if it is available.
### Some debug policies inhibit debug-block information, and if none
### is available, then this signals a NO-DEBUG-BLOCKS condition.
def code_location_debug_block(basic_code_location):
        ## XXX: ought to be:
        # block = basic_code_location.debug_block
        # return (etypecase(basic_code_location,
        #                   (compiled_code_location,
        #                    lambda: compute_compiled_code_location_debug_block(basic_code_location)))
        #         if block is keyword("unparsed") else
        #         block)
        ## -- we're identity, instead
        return basic_code_location

### Store and return BASIC-CODE-LOCATION's debug-block. We determines
### the correct one using the code-location's pc. We use
### DEBUG-FUN-DEBUG-BLOCKS to return the cached block information
### or signal a NO-DEBUG-BLOCKS condition. The blocks are sorted by
### their first code-location's pc, in ascending order. Therefore, as
### soon as we find a block that starts with a pc greater than
### basic-code-location's pc, we know the previous block contains the
### pc. If we get to the last block, then the code-location is either
### in the second to last block or the last block, and we have to be
### careful in determining this since the last block could be code at
### the end of the function. We have to check for the last block being
### code first in order to see how to compare the code-location's pc.
#### compute-compiled-code-location-debug-block

### Return the CODE-LOCATION's DEBUG-SOURCE.
def code_location_debug_source(code_location):
        # (let ((info (compiled-debug-fun-debug-info
        #                (code-location-debug-fun code-location))))
        #     (or (sb!c::debug-info-source info)
        #         (debug-signal 'no-debug-blocks :debug-fun
        #                       (code-location-debug-fun code-location))))
        # return inspect.getsource(l.debug_fun)
        ## form:
        ##  c:
        ##   - sb-di::preprocess-for-eval :: form -> loc -> (frame -> t)
        ##   - sb-di::form-number-translations :: form -> integer (TLF #) -> { integer (TLF #) -> list }
        ##
        ## frame (debug-int.lisp):
        ##  a:
        ##   - up
        ##   -*%down
        ##   -*debug-fun
        ##   -*code-location
        ##   -*%catches
        ##   - pointer
        ##   - number
        ##  c:
        ##   - sb-di:top-frame :: IO -> frame
        ##
        ## cloc (debug-int.lisp):
        ##  a:
        ##   -*debug-fun
        ##   - %unknown-p
        ##   -+%debug-block
        ##       sb-di::code-location-debug-block :: cloc -(via cache/parse)-> dblock
        ##   -+%tlf-offset
        ##       sb-di::code-location-toplevel-form-offset :: cloc -(via cache/parse)-> integer (TLF #)
        ##   -+%form-number
        ##       sb-di::code-location-form-number :: cloc -(via cache/parse)-> integer (form #)
        ##  c:
        ##   - sb-di::code-location-debug-source :: cloc -(via dinfo (via dfun))-> dsource
        ##   - sb-debug::maybe-block-start-location :: cloc -> cloc            # only identity is practical for us
        ##   - sb-debug::code-location-source-form :: cloc -> integer -> form  # no surprises here
        ##
        ## dfun (debug-int.lisp (don't confuse with SB!C::DEBUG-FUN)):
        ##  a:
        ##   - %lambda-list
        ##   - %debug-vars
        ##   - blocks
        ##   - %function
        ##  c:
        ##   - dinfo
        ##  
        ## dvar (debug-int.lisp):
        ##  a:
        ##   -*symbol
        ##   -*id
        ##   -*alive-p
        ##  c:
        ##   - sb-di:debug-var-value
        ##   - sb-di::debug-var-validity :: dvar -> cloc -> (member :valid :invalid :unknown)
        ##
        ## dblock (debug-int.lisp):
        ##  a:
        ##   - successors
        ##   - elsewhere-p
        ##
        ## compiled-dfun (debug-info.lisp):
        ##  a:
        ##   - name
        ##   - kind
        ##   - vars
        ##   - blocks
        ##   - tlf-number
        ##   - arguments
        ##   - returns
        ##   - return-pc
        ##   - old-fp
        ##   - nfp
        ##   - start-pc
        ##   - elsewhere-pc
        ##  c:
        ##   - 
        ##
        ## dsource (debug-info.lisp):
        ##  a:
        ##   -*namestring
        ##   -*created
        ##   - source-root
        ##   - start-positions
        ##   - form
        ##   - function
        ##   - compiled
        ##   -*plist
        ##  c:
        ##   - dinfo ???
        ##   - sb-di:debug-source-from :: dsource -> (member :file :lisp)
        ## 
        ## dinfo (debug-info.lisp):
        ##  a:
        ##   - name
        ##   - source
        ##  c:
        ## 
        ##
        co = code_location.debug_fun
        namestring = co.co_filename
        _namestring_ast, timestamp = __namestring_asts__[(namestring, 0)]
        exists_p = _namestring_ast
        # XXX: it's very unclear, how do we track the compilation timestamp..
        namestring, created, compiled, form = (
                (namestring,
                 timestamp,
                 timestamp,
                 nil) if exists_p else
                # no backing file...
                # XXX: really ought to deal with this..
                (nil,
                 nil,
                 get_universal_time(),
                 ast))
        return sb_c.debug_source(
                # When the DEBUG-SOURCE describes a file, the file's namestring.
                # Otherwise, NIL.
                namestring = namestring,
                # the universal time that the source was written, or NIL if
                # unavailable
                created = created,
                # the source path root number of the first form read from this
                # source (i.e. the total number of forms converted previously in
                # this compilation).  (Note: this will always be 0 so long as the
                # SOURCE-INFO structure has exactly one FILE-INFO.)
                source_root = 0,
                # The FILE-POSITIONs of the truly top level forms read from this
                # file (if applicable). The vector element type will be chosen to
                # hold the largest element.
                start_positions = [],
                # For functions processed by EVAL (including EVAL-WHEN and LOAD on
                # a source file), the source form.
                form = nil,
                # This is the function whose source is the form.
                function = code_location.debug_fun,
                # the universal time that the source was compiled
                compiled = compiled,
                # Additional information from (WITH-COMPILATION-UNIT (:SOURCE-PLIST ...))
                #
                # XXX: Mind this (in swank_python.py, code_location_source_location()):
                # getf(plist, keyword("emacs-buffer"))
                plist = [])

def _make_cloc(co, lineno):
        return code_location(## the DEBUG-FUN containing this CODE-LOCATION
                             # "debug_fun",       # nil :type debug-fun
                             debug_fun = co,      # a code object.. which is really closer to a debug block
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
                             lineno      = lineno,
                             source      = keyword("unparsed"),
                             tlf         = keyword("unparsed")

                             # Issue FRAME-CLOC-AND-RELATED-MAPS-MUST-BE-WEAK
)
(__frame_clocs__, _frame_cloc) = cl._make_timestamping_cache(
        lambda frame: _make_cloc(cl._frame_fun(frame),
                                 cl._frame_lineno(frame) - 1))

frame_code_location = _frame_cloc

# This is blatant overcaching.  Period.
#  ..all of it in the name of uniformity.
#
# Issue SOURCE-LINEMAP-AST-CACHES-NEED-TIMESTAMP-DISCIPLINE
def __namestring_source(namestring):
        if probe_file(namestring):
                return cl._file_as_string(namestring)
(__namestring_source__, _namestring_source) = cl._make_timestamping_cache(__namestring_source)
def __code_source(co):
        source, presentp = cl._evaluated_code_source(co)
        if presentp:
                return source
(__code_source__, _code_source) = cl._make_timestamping_cache(__code_source)

# A kind word of warning: linemap counts from zero, like Dijkstra!
def __namestring_linemap(namestring):
        return cl._if_let(_namestring_source(namestring),
                          string_line_offsets)
(__namestring_linemap__, _namestring_linemap) = cl._make_timestamping_cache(__namestring_linemap)
def __code_linemap(co):
        source_linemap = _namestring_linemap(co.co_filename)
        if source_linemap:
                return source_linemap
        # Source file missing, the only other place where we could've had
        # the source is EVAL's cache:
        return cl._if_let(_code_source(co),
                          string_line_offsets)
(__code_linemap__, _code_linemap) = cl._make_timestamping_cache(__code_linemap)

def __namestring_ast(namestring):
        return cl._if_let(_namestring_source(namestring),
                          more_ast.extract_ast)
(__namestring_asts__, _namestring_ast) = cl._make_timestamping_cache(__namestring_ast)

def __code_ast(co):
        source_ast = _namestring_ast(co.co_filename)
        if source_ast:
                return source_ast
        # Source file missing, the only other place where we could've had
        # the source is EVAL's cache:
        return cl._if_let(_code_source(co),
                          more_ast.extract_ast)
(__code_asts__, _code_ast) = cl._make_timestamping_cache(__code_ast)
###
### XXX: the above begs for structure

###
def code_location_toplevel_form_offset(code_location):
        # Returns the number of top level forms before the one containing
        # CODE-LOCATION as seen by the compiler in some compilation unit. (A
        # compilation unit is not necessarily a single file, see the section
        # on debug-sources.)
        if code_location_unknown_p(code_location):
                error(unknown_code_location, code_location = code_location)
        tlf_offset = code_location.tlf_offset
        return (tlf_offset                     if tlf_offset is not keyword("unparsed")    else
                error("unknown code location") if not fill_in_code_location(code_location) else
                code_location.tlf_offset)

def code_location_form_number(code_location):
        # Return the number of the form corresponding to CODE-LOCATION. The
        # form number is derived by a walking the subforms of a top level
        # form in depth-first order.
        if code_location_unknown_p(code_location):
                error(unknown_code_location, code_location = code_location)
        form_num = code_location.form_number
        return (form_num                       if form_num is not keyword("unparsed")      else
                error("unknown code location") if not fill_in_code_location(code_location) else
                code_location.form_number)

def code_location_kind(code_location):
        # Return the kind of CODE-LOCATION, one of:
        #  :INTERPRETED, :UNKNOWN-RETURN, :KNOWN-RETURN, :INTERNAL-ERROR,
        #  :NON-LOCAL-EXIT, :BLOCK-START, :CALL-SITE, :SINGLE-VALUE-RETURN,
        #  :NON-LOCAL-ENTRY
        not_implemented()

def compiled_code_location_live_set(code_location):
        # This returns CODE-LOCATION's live-set if it is available. If
        # there is no debug-block information, this returns NIL.
        not_implemented()

def code_location_equal(l1, l2):
        # true if OBJ1 and OBJ2 are the same place in the code
        not_implemented()

def sub_compiled_code_location_equal(l1, l2):
        # (= (compiled-code-location-pc obj1)
        #    (compiled-code-location-pc obj2))
        not_implemented()

def fill_in_code_location(cloc) -> t:
        # Fill in CODE-LOCATION's :UNPARSED slots, returning T or NIL
        # depending on whether the code-location was known in its
        # DEBUG-FUN's debug-block information. This may signal a
        # NO-DEBUG-BLOCKS condition due to DEBUG-FUN-DEBUG-BLOCKS, and
        # it assumes the %UNKNOWN-P slot is already set or going to be set.
        check_type(cloc, code_location)
        debug_fun = cloc.debug_fun
        ## Was:
        # blocks = debug_fun_debug_blocks(debug_fun)
        # for i, block in enumerate(blocks):
        #         locations = compiled_debug_block_code_locations(block)
        #         for i, loc in enumerate(locations):
        #                 if sub_compiled_code_location_equal(cloc, loc):
        #                         (cloc.debug_block,
        #                          cloc.tlf_offset,
        #                          cloc.form_number,
        #                          cloc.live_set,
        #                          cloc.kind,
        #                          cloc.step_info) = (loc.debug_block,
        #                                             loc.tlf_offset,
        #                                             loc.form_number,
        #                                             loc.live_set,
        #                                             loc.kind,
        #                                             loc.step_info)
        #                          return t
        def find_toplevel_for_lineno(tlfs, lineno):
                prev = nil
                for i, tlf in enumerate(tlfs):
                        if tlf.lineno > lineno:
                                return min(0, i - 1), prev
                        prev = tlf
                return nil, nil
        def tlf_find_first_node_for_lineno(form, lineno):
                def rec(form):
                        if form.lineno >= lineno:
                                return form
                        else:
                                return find_if(rec, sorted(more_ast.ast_children(form),
                                                           key = slotting("lineno")))
                return rec(form)
        lineno = cloc.lineno
        module_ast = the(ast.Module, _code_ast(debug_fun))
        cloc.source = _code_source(debug_fun)
        (cloc.tlf_offset,
         cloc.tlf) = find_toplevel_for_lineno(module_ast.body, lineno)
        if not cloc.tlf:
                error("Code location points past end-of-file.")
        cloc.form_number = 0 # It's borderline impossible to infer this from just the lineno.
                             # Yes, you know whom the warm gratitude ought to be extended to.
                             # XXX: Nevertheless, we could be much more precise with FORM-NUMBER.
                             #      The only nagging question -- what for?
        return t

### operations on DEBUG-BLOCKs
#### do-debug-block-locations
#### debug-block-fun-name
#### debug-block-code-locations

### operations on debug variables

def debug_var_symbol_name(debug_var):
        return symbol_name(debug_var_symbol(debug_var))

### FIXME: Make sure that this isn't called anywhere that it wouldn't
### be acceptable to have NIL returned, or that it's only called on
### DEBUG-VARs whose symbols have non-NIL packages.
#### debug-var-package-name (debug-var)

### Return the value stored for DEBUG-VAR in frame, or if the value is
### not :VALID, then signal an INVALID-VALUE error.
#### debug-var-valid-value (debug-var frame)

### Returns the value stored for DEBUG-VAR in frame. The value may be
### invalid. This is SETFable.
def debug_var_value(var, frame):
        return cl._frame_locals(frame)[var]

def set_debug_var_value(var, frame, new_value):
        cl._frame_locals(frame)[var] = new_value
        return new_value

### This returns what is stored for the variable represented by
### DEBUG-VAR relative to the FRAME. This may be an indirect value
### cell if the variable is both closed over and set.
#### access-compiled-debug-var-slot (debug-var frame)

### a helper function for working with possibly-invalid values:
### Do (%MAKE-LISP-OBJ VAL) only if the value looks valid.
###
### (Such values can arise in registers on machines with conservative
### GC, and might also arise in debug variable locations when
### those variables are invalid.)
#### make-lisp-obj (val &optional (errorp t))

#### sub-access-debug-var-slot (fp sc-offset &optional escaped)
  ## NOTE: The long-float support in here is obviously decayed.  When
  ## the x86oid and non-x86oid versions of this function were unified,
  ## the behavior of long-floats was preserved, which only served to
  ## highlight its brokenness.

### This stores value as the value of DEBUG-VAR in FRAME. In the
### COMPILED-DEBUG-VAR case, access the current value to determine if
### it is an indirect value cell. This occurs when the variable is
### both closed over and set.
#### %set-debug-var-value (debug-var frame new-value)

### This stores VALUE for the variable represented by debug-var
### relative to the frame. This assumes the location directly contains
### the variable's value# that is, there is no indirect value cell
### currently there in case the variable is both closed over and set.
#### set-compiled-debug-var-slot (debug-var frame value)

#### sub-set-debug-var-slot (fp sc-offset value &optional escaped)
  ## Like sub-access-debug-var-slot, this is the unification of two
  ## divergent copy-pasted functions.  The astute reviewer will notice
  ## that long-floats are messed up here as well, that x86oids
  ## apparently don't support accessing float values that are in
  ## registers, and that non-x86oids store the real part of a float
  ## for both the real and imaginary parts of a complex on the stack
  ## (but not in registers, oddly enough).  Some research has
  ## indicated that the different forms of THE used for validating the
  ## type of complex float components between x86oid and non-x86oid
  ## systems are only significant in the case of using a non-complex
  ## number as input (as the non-x86oid case effectively converts
  ## non-complex numbers to complex ones and the x86oid case will
  ## error out).  That said, the error message from entering a value
  ## of the wrong type will be slightly easier to understand on x86oid
  ## systems.

### The method for setting and accessing COMPILED-DEBUG-VAR values use
### this to determine if the value stored is the actual value or an
### indirection cell.
def indirect_value_cell_p(x):
        return not_implemented()

### Return three values reflecting the validity of DEBUG-VAR's value
### at BASIC-CODE-LOCATION:
###   :VALID    The value is known to be available.
###   :INVALID  The value is known to be unavailable.
###   :UNKNOWN  The value's availability is unknown.
###
### If the variable is always alive, then it is valid. If the
### code-location is unknown, then the variable's validity is
### :unknown. Once we've called CODE-LOCATION-UNKNOWN-P, we know the
### live-set information has been cached in the code-location.
def debug_var_validity(var, frame):
        return keyword("valid") # options: "invalid", "unknown"

def debug_var_info(var):
        return nil # XXX: not_implemented() would be more fair?

### This is the method for DEBUG-VAR-VALIDITY for COMPILED-DEBUG-VARs.
### For safety, make sure basic-code-location is what we think.
#### compiled-debug-var-validity (debug-var basic-code-location)

#### sources

### This code produces and uses what we call source-paths. A
### source-path is a list whose first element is a form number as
### returned by CODE-LOCATION-FORM-NUMBER and whose last element is a
### top level form number as returned by
### CODE-LOCATION-TOPLEVEL-FORM-NUMBER. The elements from the last to
### the first, exclusively, are the numbered subforms into which to
### descend. For example:
###    (defun foo (x)
###      (let ((a (aref x 3)))
###     (cons a 3)))
### The call to AREF in this example is form number 5. Assuming this
### DEFUN is the 11'th top level form, the source-path for the AREF
### call is as follows:
###    (5 1 0 1 3 11)
### Given the DEFUN, 3 gets you the LET, 1 gets you the bindings, 0
### gets the first binding, and 1 gets the AREF form.

### This returns a table mapping form numbers to source-paths. A
### source-path indicates a descent into the TOPLEVEL-FORM form,
### going directly to the subform corressponding to the form number.
###
### The vector elements are in the same format as the compiler's
### NODE-SOURCE-PATH# that is, the first element is the form number and
### the last is the TOPLEVEL-FORM number.
def form_number_translations(form, tlf_number):
        
        return not_implemented()

#### PREPROCESS-FOR-EVAL

### Return a function of one argument that evaluates form in the
### lexical context of the BASIC-CODE-LOCATION LOC, or signal a
### NO-DEBUG-VARS condition when the LOC's DEBUG-FUN has no
### DEBUG-VAR information available.
###
### The returned function takes the frame to get values from as its
### argument, and it returns the values of FORM. The returned function
### can signal the following conditions: INVALID-VALUE,
### AMBIGUOUS-VAR-NAME, and FRAME-FUN-MISMATCH.
def preprocess_for_eval(form, loc):
        not_implemented()

### EVAL-IN-FRAME

def eval_in_frame(frame, form):
        """Evaluate FORM in the lexical context of FRAME's current code location,
returning the results of the evaluation."""
        return (preprocess_for_eval(form, frame_code_location(frame)))(frame)

#### breakpoints

#### user-visible interface

### Create and return a breakpoint. When program execution encounters
### the breakpoint, the system calls HOOK-FUN. HOOK-FUN takes the
### current frame for the function in which the program is running and
### the breakpoint object.
###
### WHAT and KIND determine where in a function the system invokes
### HOOK-FUN. WHAT is either a code-location or a DEBUG-FUN. KIND is
### one of :CODE-LOCATION, :FUN-START, or :FUN-END. Since the starts
### and ends of functions may not have code-locations representing
### them, designate these places by supplying WHAT as a DEBUG-FUN and
### KIND indicating the :FUN-START or :FUN-END. When WHAT is a
### DEBUG-FUN and kind is :FUN-END, then HOOK-FUN must take two
### additional arguments, a list of values returned by the function
### and a FUN-END-COOKIE.
###
### INFO is information supplied by and used by the user.
###
### FUN-END-COOKIE is a function. To implement :FUN-END
### breakpoints, the system uses starter breakpoints to establish the
### :FUN-END breakpoint for each invocation of the function. Upon
### each entry, the system creates a unique cookie to identify the
### invocation, and when the user supplies a function for this
### argument, the system invokes it on the frame and the cookie. The
### system later invokes the :FUN-END breakpoint hook on the same
### cookie. The user may save the cookie for comparison in the hook
### function.
###
### Signal an error if WHAT is an unknown code-location.
#### make-breakpoint (hook-fun what &key (kind :code-location) info fun-end-cookie)

### These are unique objects created upon entry into a function by a
### :FUN-END breakpoint's starter hook. These are only created
### when users supply :FUN-END-COOKIE to MAKE-BREAKPOINT. Also,
### the :FUN-END breakpoint's hook is called on the same cookie
### when it is created.
#### defstruct fun-end-cookie    bogus-ira debug-fun
### This maps bogus-lra-components to cookies, so that
### HANDLE-FUN-END-BREAKPOINT can find the appropriate cookie for the
### breakpoint hook.
#### defvar *fun-end-cookies*
### This returns a hook function for the start helper breakpoint
### associated with a :FUN-END breakpoint. The returned function
### makes a fake LRA that all returns go through, and this piece of
### fake code actually breaks. Upon return from the break, the code
### provides the returnee with any values. Since the returned function
### effectively activates FUN-END-BPT on each entry to DEBUG-FUN's
### function, we must establish breakpoint-data about FUN-END-BPT.
#### fun-end-starter-hook (starter-bpt debug-fun)
### This takes a FUN-END-COOKIE and a frame, and it returns
### whether the cookie is still valid. A cookie becomes invalid when
### the frame that established the cookie has exited. Sometimes cookie
### holders are unaware of cookie invalidation because their
### :FUN-END breakpoint hooks didn't run due to THROW'ing.
###
### This takes a frame as an efficiency hack since the user probably
### has a frame object in hand when using this routine, and it saves
### repeated parsing of the stack and consing when asking whether a
### series of cookies is valid.
#### fun-end-cookie-valid-p (frame cookie)

#### ACTIVATE-BREAKPOINT

### Cause the system to invoke the breakpoint's hook function until
### the next call to DEACTIVATE-BREAKPOINT or DELETE-BREAKPOINT. The
### system invokes breakpoint hook functions in the opposite order
### that you activate them.
#### activate-breakpoint (breakpoint)
#### activate-compiled-code-location-breakpoint (breakpoint)
#### activate-compiled-fun-start-breakpoint (breakpoint)
#### sub-activate-breakpoint (breakpoint data)

#### DEACTIVATE-BREAKPOINT

### Stop the system from invoking the breakpoint's hook function.
#### deactivate-breakpoint (breakpoint)
#### deactivate-compiled-breakpoint (breakpoint)

#### BREAKPOINT-INFO

### Return the user-maintained info associated with breakpoint. This
### is SETF'able.
#### breakpoint-info (breakpoint)
#### %set-breakpoint-info (breakpoint value)

#### BREAKPOINT-ACTIVE-P and DELETE-BREAKPOINT
#### breakpoint-active-p (breakpoint)
### Free system storage and remove computational overhead associated
### with breakpoint. After calling this, breakpoint is completely
### impotent and can never become active again.
#### delete-breakpoint (breakpoint)

#### C call out stubs

### This actually installs the break instruction in the component. It
### returns the overwritten bits. You must call this in a context in
### which GC is disabled, so that Lisp doesn't move objects around
### that C is pointing to.
#### sb!alien:define-alien-routine "breakpoint_install" sb!alien:unsigned-int code-obj pc-offset
### This removes the break instruction and replaces the original
### instruction. You must call this in a context in which GC is disabled
### so Lisp doesn't move objects around that C is pointing to.
#### sb!alien:define-alien-routine "breakpoint_remove" sb!alien:void code-obj pc-offset old-inst
#### sb!alien:define-alien-routine "breakpoint_do_displaced_inst" sb!alien:void scp orig-inst

#### breakpoint handlers (layer between C and exported interface)

### This maps components to a mapping of offsets to BREAKPOINT-DATAs.
#### defvar *component-breakpoint-offsets*

### This returns the BREAKPOINT-DATA object associated with component cross
### offset. If none exists, this makes one, installs it, and returns it.
#### breakpoint-data (component offset &optional (create t))
### We use this when there are no longer any active breakpoints
### corresponding to DATA.
#### delete-breakpoint-data (data)
  ## Again, this looks brittle. Is there no danger of being interrupted
  ## here?

### The C handler for interrupts calls this when it has a
### debugging-tool break instruction. This does *not* handle all
### breaks# for example, it does not handle breaks for internal
### errors.
#### handle-breakpoint (offset component signal-context)

### This holds breakpoint-datas while invoking the breakpoint hooks
### associated with that particular component and location. While they
### are executing, if we hit the location again, we ignore the
### breakpoint to avoid infinite recursion. fun-end breakpoints
### must work differently since the breakpoint-data is unique for each
### invocation.
#### defvar *executing-breakpoint-hooks*

### This handles code-location and DEBUG-FUN :FUN-START
### breakpoints.
#### handle-breakpoint-aux (breakpoints data offset component signal-context)
#### invoke-breakpoint-hooks (breakpoints signal-context)
#### signal-context-frame (signal-context)
#### handle-fun-end-breakpoint (offset component context)
### Either HANDLE-BREAKPOINT calls this for :FUN-END breakpoints
### [old C code] or HANDLE-FUN-END-BREAKPOINT calls this directly
### [new C code].
#### handle-fun-end-breakpoint-aux (breakpoints data signal-context)
  ## FIXME: This looks brittle: what if we are interrupted somewhere
  ## here? ...or do we have interrupts disabled here?
#### get-fun-end-breakpoint-values (scp)

#### MAKE-BOGUS-LRA (used for :FUN-END breakpoints)
#### defconstant bogus-lra-constants
#### defconstant known-return-p-slot

### Make a bogus LRA object that signals a breakpoint trap when
### returned to. If the breakpoint trap handler returns, REAL-LRA is
### returned to. Three values are returned: the bogus LRA object, the
### code component it is part of, and the PC offset for the trap
### instruction.
#### make-bogus-lra (real-lra &optional known-return-p)

#### miscellaneous

### This appears here because it cannot go with the DEBUG-FUN
### interface since DO-DEBUG-BLOCK-LOCATIONS isn't defined until after
### the DEBUG-FUN routines.

### Return a code-location before the body of a function and after all
### the arguments are in place# or if that location can't be
### determined due to a lack of debug information, return NIL.
#### debug-fun-start-location (debug-fun)

#### Single-stepping

### The single-stepper works by inserting conditional trap instructions
### into the generated code (see src/compiler/*/call.lisp), currently:
###
###   1) Before the code generated for a function call that was
###      translated to a VOP
###   2) Just before the call instruction for a full call
###
### In both cases, the trap will only be executed if stepping has been
### enabled, in which case it'll ultimately be handled by
### HANDLE-SINGLE-STEP-TRAP, which will either signal a stepping condition,
### or replace the function that's about to be called with a wrapper
### which will signal the condition.
#### handle-single-step-trap (kind callee-register-offset)
#### defvar *step-frame*
#### handle-single-step-before-trap (context)

### This function will replace the fdefn / function that was in the
### register at CALLEE-REGISTER-OFFSET with a wrapper function. To
### ensure that the full call will use the wrapper instead of the
### original, conditional trap must be emitted before the fdefn /
### function is converted into a raw address.

#### handle-single-step-around-trap (context callee-register-offset)
### Given a signal context, fetch the step-info that's been stored in
### the debug info at the trap point.
#### single-step-info-from-context (context)

def find_stepped_frame():
        # Return the frame that triggered a single-step condition. Used to
        # provide a *STACK-TOP-HINT*.
        return not_implemented()

#################################### END OF debug-int.lisp #####################

def code_location_source_form(l):
        "XXX: elsewhere"
        return not_implemented()

def maybe_block_start_location(l):
        "XXX: elsewhere"
        # If LOC is an unknown location, then try to find the block start
        # location. Used by source printing to some information instead of
        # none for the user.
        return not_implemented()

def disassemble_memory():
        "XXX: elsewhere"
        return not_implemented()

def disassemble_code_component():
        "XXX: elsewhere"
        return not_implemented()

def align():
        "XXX: elsewhere"
        return not_implemented()
