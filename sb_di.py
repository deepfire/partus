import cl

from cl       import *
from pergamum import *

class debug_condition(condition):
        pass

class no_debug_blocks(debug_condition):
        pass

setq("_stack_top_hint_", nil)

def top_frame():
        return cl._caller_frame()

def frame_down(f):
        return cl._next_frame(f)

def frame_code_location(f):
        not_implemented()

def frame_catches():
        not_implemented()

def find_stepped_frame():
        not_implemented()

class code_location(servile):
        pass

def code_location_toplevel_form_offset(l):
        not_implemented()

def code_location_form_number(l):
        not_implemented()

def form_number_translations(tlf):
        not_implemented()

def code_location_source_form(l):
        not_implemented()

def maybe_block_start_location(l):
        not_implemented()

def code_location_debug_fun(l):
        not_implemented()

def code_location_debug_source(l):
        not_implemented()

def code_location_debug_block(l):
        not_implemented()

class debug_source(servile):
        pass

def debug_source_plist(ds):
        not_implemented()

def debug_source_from(ds):
        not_implemented()

def debug_source_namestring(ds):
        not_implemented()

def debug_source_created(ds):
        not_implemented()

def debug_source_name(ds):
        not_implemented()

class debug_fun(servile):
        pass

def debug_fun_fun(df):
        not_implemented()

def debug_fun_debug_vars(df):
        not_implemented()

class debug_var(servile):
        pass

def debug_var_validity(var, frame):
        return keyword("valid") # options: "invalid", "unknown"

def debug_var_value(var, frame):
        return cl._frame_locals(frame)[var]

def debug_var_info(var):
        return nil

def debug_var_symbol():
        not_implemented()

def debug_var_id():
        not_implemented()

def preprocess_for_eval():
        not_implemented()

def indirect_value_cell_p():
        not_implemented()

def disassemble_memory():
        not_implemented()

def disassemble_code_component():
        not_implemented()

def align():
        not_implemented()

# def ():
#        not_implemented()
###################
