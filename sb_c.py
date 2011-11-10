import cl

from cl import *
from cl import _defaulted_to_var as defaulted_to_var

defvar("_source_plist_", [])

# There is one per compiled file and one per function compiled at
# toplevel or loaded from source.
class debug_source:
  # (This is one of those structures where IWBNI we had multiple
  # inheritance.  The first four slots describe compilation of a
  # file, the fifth and sixth compilation of a form processed by
  # EVAL, and the seventh and eigth all compilation units; and these
  # are orthogonal concerns that can combine independently.)
  def __init__(self,
               namestring = nil, created = nil, source_root = 0,
               start_positions = nil,
               form = nil, function = nil,
               compiled = None,
               plist = None):
          # When the DEBUG-SOURCE describes a file, the file's namestring.
          # Otherwise, NIL.
          self.namestring = namestring              # nil :type (or null string)
          # the universal time that the source was written, or NIL if
          # unavailable
          self.created = created                    # nil :type (or unsigned-byte null)
          # the source path root number of the first form read from this
          # source (i.e. the total number of forms converted previously in
          # this compilation).  (Note: this will always be 0 so long as the
          # SOURCE-INFO structure has exactly one FILE-INFO.)
          self.source_root = source_root            # 0   :type index
          # The FILE-POSITIONs of the truly top level forms read from this
          # file (if applicable). The vector element type will be chosen to
          # hold the largest element.
          self.start_positions = start_positions    # nil :type (or (simple-array * (*)) null)

          # For functions processed by EVAL (including EVAL-WHEN and LOAD on
          # a source file), the source form.
          self.form = form                          # nil :type list
          # This is the function whose source is the form.
          self.function = function                  # nil

          # the universal time that the source was compiled
          self.compiled = compiled or missing_arg() # :type unsigned-byte
          # Additional information from (WITH-COMPILATION-UNIT (:SOURCE-PLIST ...))
          self.plist = defaulted_to_var(plist, "_source_plist_")

def debug_source_namestring(ds): return ds.namestring
def debug_source_created(ds):    return ds.created
def debug_source_plist(ds):      return ds.plist
