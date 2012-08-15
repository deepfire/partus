###
### Zero-requirement functionality
###
def lisp_symbol_name_python_name(x):
        def sub(cs):
                acc = []
                for c in cs:
                        acc.append("_" if c in "-*:&%" else c)
                return acc
        ret = "".join(sub(x)).lower()
        return ret

def full_symbol_name_python_name(x):
        return ("#" if not x.package              else
                "" if x.package.name == "KEYWORD" else
                x.package.name) + ":" + x.name

def lisp_symbol_name_python_type_name(x):
        return lisp_symbol_name_python_name(x) + "_t"

common_ands = { "WHOLE", "OPTIONAL", "REST", "BODY", "KEY", "ALLOW-OTHER-KEYS" }
def python_name_lisp_symbol_name(x):
        """Heuristic to (not quite) undo the effect of _lisp_symbol_name_python_name().
Irreversibles: %."""
        def sub(cs):
                if len(cs) > 1:
                        starred       = cs[0]  == cs[-1] == "_"                                   # *very-nice*
                        anded         = cs[0]  == "_" != cs[-1] and cs[1:].upper() in common_ands # &something
                        maybe_keyword = cs[-1] != cs[0] == "_" != cs[1]                           # :something
                        tailed        = cs[-1] == "_" != cs[0]                                    # something-in-conflict
                else:
                        starred = anded = maybe_keyword = tailed = False
                pre, post, start, end = (("*", "*", 1, len(cs) - 1) if starred       else
                                         ("&", "",  1, None)        if anded         else
                                         (":", "",  1, None)        if maybe_keyword else
                                         ("",  "",  0, len(cs) - 1) if tailed        else
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

def has_global(name, globals):
        return name in globals

def make_object_like_python_function(x, function):
        x.__code__ = function.__code__
        return x

def make_object_like_python_class(x, cls):
        x.__doc__          = cls.__doc__
        x.__subclasshook__ = cls.__subclasshook__
        return x

def frost_def(o, symbol, slot, globals):
        setattr(symbol, slot, o)
        setf_global(symbol, full_symbol_name_python_name(symbol), globals)
        return symbol

def raise_exception(cond):
        raise cond

import sys

def find_module(name):
        return sys.modules[name] if name in sys.modules else None

##
## Pythonese execution tracing: for HANDLER-BIND.
##
__tracer_hooks__   = dict() # allowed keys: "call", "line", "return", "exception", "c_call", "c_return", "c_exception"
def set_tracer_hook(type, fn):        __tracer_hooks__[type] = fn
def     tracer_hook(type):     return __tracer_hooks__[type] if type in __tracer_hooks__ else None

def pytracer(frame, event, arg):
        method = tracer_hook(event)
        if method:
                method(arg, frame)
        return pytracer

def pytracer_enabled_p(): return sys.gettrace() is pytracer
def enable_pytracer():    sys.settrace(pytracer)
def disable_pytracer():   sys.settrace(None)

##
## Research stuff..
##
def _dump_thread_state():
        def body():
                import ctypes
                from binascii import hexlify
                from ctypes import c_uint, c_char, c_ulong, POINTER, cast, pythonapi
                def dump(obj):
                        for i, x in _py.enumerate(hexlify(_memoryview(obj)).decode()):
                                _py.print(x, end='')
                                if i and not (i + 1)%8:
                                        _py.print(" ", end='')
                                if i and not (i + 1)%32:
                                        _py.print("")
                class PyThreadState(ctypes.Structure):
                        _fields_ = [("next",               c_ulong),
                                    ("interp",             c_ulong),

                                    ("frame",              c_ulong),
                                    ("recursion_depth",    c_uint),
                                    ("overflowed",         c_char),

                                    ("recursion_critical", c_char),

                                    ("pad0_", c_char),
                                    ("pad1_", c_char),

                                    ("tracing",            c_uint),
                                    ("use_tracing",        c_uint),

                                    ("c_profilefunc",      c_ulong),
                                    ("c_tracefunc",        c_ulong),
                                    ("c_profileobj",       c_ulong),
                                    ("c_traceobj",         c_ulong),

                                    ("curexc_type",        c_ulong),
                                    ("curexc_value",       c_ulong),
                                    ("curexc_traceback",   c_ulong),

                                    ("exc_type",           c_ulong),
                                    ("exc_value",          c_ulong),
                                    ("exc_traceback",      c_ulong),

                                    ("dict",               c_ulong),

                                    ("tick_counter",       c_uint),

                                    ("gilstate_counter",   c_uint),

                                    ("async_exc",          c_ulong),
                                    ("thread_id",          c_ulong)]
                pythonapi.PyThreadState_Get.restype = PyThreadState
                o = pythonapi.PyThreadState_Get()

                _py.print("o: %s, id: {%x}" % (o, _py.id(o)))
                _py.print(dump(o))
                for slot, _ in _py.type(o)._fields_:
                        val = _py.getattr(o, slot)
                        _py.print(("%25s: " + ("%x" if integerp(val) else "%s")) % (slot, val))
        _without_condition_system(body,
                                  reason = "_dump_thread_state")
