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

def make_object_like_python_function(x, function):
        x.__code__ = function.__code__
        return x

def make_object_like_python_class(x, cls):
        x.__doc__          = cls.__doc__
        x.__subclasshook__ = cls.__subclasshook__
        return x

def frost_def(o, symbol, slot, globals):
        setattr(symbol, slot, o)
        setf_global(symbol, lisp_symbol_name_python_name(symbol.name), globals)
        return symbol

##
## Pythonese execution tracing: for HANDLER-BIND.
##
import sys

__tracer_hooks__   = dict() # allowed keys: "call", "line", "return", "exception", "c_call", "c_return", "c_exception"
def set_tracer_hook(type, fn):        __tracer_hooks__[type] = fn
def     tracer_hook(type):     return __tracer_hooks__[type] if type in __tracer_hooks__ else None

def pytracer(frame, event, arg):
        method = tracer_hook(event)
        if method:
                method(arg, frame)
        return pytracer

def pytracer_enabled_p(): return sys.gettrace() is pytracer
def enable_pytracer():    sys.settrace(_pytracer)
def disable_pytracer():   sys.settrace(None)
