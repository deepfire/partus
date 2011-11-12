import cl

from cl import *
from cl import _keyword as keyword, _intern0 as intern0, _defaulted_to_var as defaulted_to_var

defvar("_source_plist_", [])

# There is one per compiled file and one per function compiled at
# toplevel or loaded from source.
debug_source = defstruct("debug_source",
                         # (This is one of those structures where IWBNI we had multiple
                         # inheritance.  The first four slots describe compilation of a
                         # file, the fifth and sixth compilation of a form processed by
                         # EVAL, and the seventh and eigth all compilation units; and these
                         # are orthogonal concerns that can combine independently.)

                         # When the DEBUG-SOURCE describes a file, the file's namestring.
                         # Otherwise, NIL.
                         "namestring",      # nil :type (or null string)
                         # the universal time that the source was written, or NIL if
                         # unavailable
                         "created",         # nil :type (or unsigned-byte null)
                         # the source path root number of the first form read from this
                         # source (i.e. the total number of forms converted previously in
                         # this compilation).  (Note: this will always be 0 so long as the
                         # SOURCE-INFO structure has exactly one FILE-INFO.)
                         "source_root",     # 0   :type index
                         # The FILE-POSITIONs of the truly top level forms read from this
                         # file (if applicable). The vector element type will be chosen to
                         # hold the largest element.
                         "start_positions", # nil :type (or (simple-array * (*)) null)
                         # For functions processed by EVAL (including EVAL-WHEN and LOAD on
                         # a source file), the source form.
                         "form",            # nil :type list
                         # This is the function whose source is the form.
                         "function",        # nil
                         # the universal time that the source was compiled
                         "compiled",        # (missing-arg)
                         # Additional information from (WITH-COMPILATION-UNIT (:SOURCE-PLIST ...))
                         "plist",)          # :type unsigned-byte

def debug_source_namestring(ds): return ds.namestring
def debug_source_created(ds):    return ds.created
def debug_source_plist(ds):      return ds.plist

def debug_source_from(ds):
        # Note, this is absent in SBCL 1.0.50
        return keyword("file" if source_namestring_looks_real_p(ds.namestring) else
                       "lisp")

def source_namestring_looks_real_p(namestring):
        return (len(namestring) > 3 and
                (namestring[0] != "<") and
                namestring[-3:] == ".py")
