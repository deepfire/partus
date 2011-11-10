import inspect

import cl

from cl       import *
from pergamum import *

from cl import _keyword as keyword, _intern0 as intern0

###
### Conditions
###
class debug_condition(condition):
        """All DEBUG-CONDITIONs inherit from this type. These are serious conditions
that must be handled, but they are not programmer errors."""
        pass

class no_debug_blocks(debug_condition):
        pass

###
### Classes
###
class code_location(servile):
        pass

class debug_fun(servile):
        pass

class debug_var(servile):
        pass

###
### Globals
###
setq("_stack_top_hint_", nil)


###
### Code
###
def top_frame():
        return cl._caller_frame()

def frame_down(f):
        # BOOT: CL:ERROR
        return cl._next_frame(f)

def frame_catches():
        # Return an alist mapping catch tags to CODE-LOCATIONs. These are
        # CODE-LOCATIONs at which execution would continue with frame as the
        # top frame if someone threw to the corresponding tag.
        return not_implemented()

def find_stepped_frame():
        # Return the frame that triggered a single-step condition. Used to
        # provide a *STACK-TOP-HINT*.
        return not_implemented()

def frame_code_location(f):
        return code_location(debug_fun = f.f_code,   # the DEBUG-FUN containing this CODE-LOCATION
                             #
                             # The stuff we don't have:
                             #
                             # This is the number of forms processed by the compiler or loader
                             # before the top level form containing this code-location.
                             # tlf-offset            
                             #
                             # This is the depth-first number of the node that begins
                             # code-location within its top level form.
                             # form-number           # 
                             lineno    = f.f_lineno) # ..a measly substitute..

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
        return sb_c.debug_source(
                namestring = l.debug_fun.co_filename,
                created = file_write_date(l.debug_fun.co_filename),
                source = 0,
                start_positions = [],
                form = nil,
                function = l.debug_fun.
                compiled = file_write_date(l.debug_fun.co_filename),
                plist = [])

def code_location_debug_block(l):
        # the DEBUG-BLOCK containing CODE-LOCATION. XXX Possibly toss this
        # out and just find it in the blocks cache in DEBUG-FUN.
        return not_implemented()

###
def debug_source_plist(ds):      return ds.plist        # Mind this: getf(plist, keyword("emacs-buffer"))
def debug_source_from(ds):       return keyword("file") # XXX: Mind C-c C-c !
def debug_source_namestring(ds): return ds.namestring
def debug_source_created(ds):    return ds.created

def debug_source_name(ds):
        return not_implemented()

###
def debug_fun_fun(df):
        return not_implemented()

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
