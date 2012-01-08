###
### Zero-requirement functionality
###
def lisp_symbol_name_python_name(x):
        def sub(cs):
                acc = []
                for c in cs:
                        acc.append("_" if c in "-*&" else c)
                return acc
        ret = "".join(sub(x)).lower()
        return ret

def python_name_lisp_symbol_name(x):
        "Heuristic to undo the effect of _lisp_symbol_name_python_name()."
        def sub(cs):
                if len(cs) > 1:
                        starred = cs[0]  == cs[-1] == "_" # *very-nice*
                        anded   = cs[0]  == "_" != cs[-1] # &something; This #\& heuristic might bite us quite sensibly..
                        tailed  = cs[-1] == "_" != cs[0]  # something-in-conflict
                pre, post, start, end = (("*", "*", 1, len(cs) - 1) if starred else
                                         ("&", "",  1, None)        if anded   else
                                         ("",  "",  0, len(cs) - 1) if tailed  else
                                         ("",  "",  0, None))
                return (pre +
                        "".join("-" if c == "_" else c for c in cs[start:end]) +
                        post)
        ret = sub(x).upper()
        return ret

def setf_global(value, name, globals):
        globals[name] = value
        return value

def global_(name, globals):
        return ((globals[name], True) if name in globals else
                (None,          False))
