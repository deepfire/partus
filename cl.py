###
### Some surfacial Common Lisp compatibility.
###
import re
import os
import io
import _io
import ast
import imp
import sys
import time
import types
import socket
import inspect
import builtins
import platform
import functools
import threading
import collections

from functools import reduce
from neutrality import stringp, _write_string

###
### Ring 0.
###
def identity(x):
        return x

def let(*values_and_body):
        values, body = values_and_body[:-1], values_and_body[-1]
        return body(*values)

def progn(*body):
        for b in body[:-1]:
                b()
        return body[-1]()

def _prognf(*body):
        return lambda: progn(*body)

most_positive_fixnum = 67108864

def defstruct(name, *slots):
        return collections.namedtuple(name, slots)

def string_upcase(x):     return x.upper()
def string_downcase(x):   return x.lower()
def string_capitalize(x): return x.capitalize()

__core_symbol_names__ = [
        "QUOTE",
        "AND", "OR", "MEMBER", "EQL",
        "ABORT", "CONTINUE", "BREAK",
        ]
__more_symbol_names__ = [
        "SOME", "EVERY",
]

_case_attribute_map = dict(UPCASE     = string_upcase,
                           DOWNCASE   = string_downcase,
                           CAPITALIZE = string_capitalize,
                           PRESERVE   = identity)
def _case_xform(type, s):
        return _case_attribute_map[type.name](s)

###
### Ring 1.
###
def _alist_plist(xs):
        return append(*xs)

def _plist_alist(xs):
        acc = []
        for i in range(0, len(xs), 2):
                acc.append((xs[i], xs[i + 1]))
        return acc

class _cache(collections.UserDict):
        def __init__(self, filler):
                self.filler = filler
                self.data = dict()
        def __getitem__(self, key):
                check_type(key, tuple)
                key, access_timestamp = key
                if key[0] not in self.data:
                        self.data[key[0]] = self.filler(key)
                return self.data[key[0]]
        def __setitem__(self, key, value):
                error("Direct cache writes are not allowed.")

def _defaulted(x, value):
        return x if x is not None else value

def _defaulted_to_var(x, variable):
        return _defaulted(x, symbol_value(variable))

def _read_case_xformed(x):
        return _case_xform(_symbol_value("_READ_CASE_"), x)

def _coerce_to_symbol_name(x):
        return (x.name                if symbolp(x) else
                _read_case_xformed(x) if stringp(x) else
                error(simple_type_error, "%s cannot be coerced to string.", x))

def _astp(x):                                return typep(x, ast.AST)
def _ast_num(n):                             return ast.Num(n = the(int, n))
def _ast_alias(name):                        return ast.alias(name = the(str, name), asname = None)
def _ast_keyword(name, value):               return ast.keyword(arg = the(str, name), value = the(ast.expr, value))
def _ast_Expr(node):                         return ast.Expr(value = the(ast.expr, node))
def _ast_rw(writep):                         return (ast.Store() if writep else ast.Load())
def _ast_string(s):                          return ast.Str(s = the(str, s))
def _ast_name(name, writep = False):         return ast.Name(id = the(str, name), ctx = _ast_rw(writep))
def _ast_attribute(x, name, writep = False): return ast.Attribute(attr = name, value = x, ctx = _ast_rw(writep))
def _ast_index(of, index, writep = False):   return ast.Subscript(value = of, slice = ast.Index(value = index), ctx = _ast_rw(writep))
def _ast_maybe_normalise_string(x):          return (_ast_string(x) if stringp(x) else x)
def _ast_list(xs):
        assert every(_astp, the(list, xs))
        return ast.List(elts = xs, ctx = ast.Load())
def _ast_funcall(name, *args, **keys):
        if not every(lambda x: stringp(x) or _astp(x) or x is None, args):
                error("AST-FUNCALL: %s: improper arglist %s", name, str(args))
        return ast.Call(func = (_ast_name(name) if stringp(name) else name),
                        args = mapcar(_ast_maybe_normalise_string, args),
                        keywords = _maphash(_ast_keyword, keys),
                        starargs = None,
                        kwargs = None)
def _ast_import_from(module_name, names):
        assert every(stringp, the(list, names))
        return ast.ImportFrom(module = the(str, module_name), names = mapcar(_ast_alias, names), level = 0)
def _ast_module(body):
        assert every(_astp, the(list, body))
        return ast.Module(body = body, lineno = 0)

###
### Basis
###
##
## modules/packages
##
def _load_code_object_as_module(name, x, filename = "", builtins = None):
        check_type(x, type(_load_code_object_as_module.__code__))
        mod = imp.new_module(name)
        mod.__filename__ = filename
        if builtins:
                mod.__dict__["__builtins__"] = builtins
        sys.modules[name] = mod
        exec(x, mod.__dict__, mod.__dict__)
        return mod

def _load_text_as_module(name, text, filename = "", builtins = None):
        return _load_code_object_as_module(name, compile(text, filename, "exec"),
                                           filename = filename, builtins = builtins)

def _reregister_module_as_package(mod, parent_package = None):
        # this line might need to be performed before exec()
        mod.__path__ = (parent_package.__path__ if parent_package else []) + [ mod.name.split(".")[-1] ]
        if parent_package:
                dotpos = mod.name.rindex(".")
                assert (dotpos)
                postdot_name = mod.name[dotpos + 1:]
                setattr(parent_package, postdot_name, mod)
                parent_package.__children__.add(mod)
                mod.__parent__ = parent_package
        if packagep:
                mod.__children__ = set([])

##
## frames
##
# >>> dir(f)
# ["__class__", "__delattr__", "__doc__", "__eq__", "__format__",
# "__ge__", "__getattribute__", "__gt__", "__hash__", "__init__",
# "__le__", "__lt__", "__ne__", "__new__", "__reduce__",
# "__reduce_ex__", "__repr__", "__setattr__", "__sizeof__", "__str__",
# "__subclasshook__", "f_back", "f_builtins", "f_code", "f_globals",
# "f_lasti", "f_lineno", "f_locals", "f_trace"]
# >>> dir(f.f_code)
# ["__class__", "__delattr__", "__doc__", "__eq__", "__format__",
# "__ge__", "__getattribute__", "__gt__", "__hash__", "__init__",
# "__le__", "__lt__", "__ne__", "__new__", "__reduce__",
# "__reduce_ex__", "__repr__", "__setattr__", "__sizeof__", "__str__",
# "__subclasshook__", "co_argcount", "co_cellvars", "co_code",
# "co_consts", "co_filename", "co_firstlineno", "co_flags",
# "co_freevars", "co_kwonlyargcount", "co_lnotab", "co_name",
# "co_names", "co_nlocals", "co_stacksize", "co_varnames"]
def _example_frame():
        "cellvars: closed over non-globals;  varnames: bound"
        def xceptor(xceptor_arg):
                "names: globals;  varnames: args + otherbind;  locals: len(varnames)"
                try:
                        error("This is xceptor talking: %s.", xceptor_arg)
                except Exception as cond:
                        return _this_frame()
        def midder(midder_arg):
                "freevars: non-global-free;  varnames: args + otherbind;  locals: ..."
                midder_stack_var = 0
                return xceptor(midder_arg + midder_stack_var)
        def outer():
                "freevars: non-global-free;  varnames: args + otherbind"
                outer_stack_var = 3
                return midder(outer_stack_var)
        return outer()

def _all_threads_frames():
        return sys._current_frames()

def _this_frame():
        return sys._getframe(1)

_frame = type(_this_frame())

def _framep(x):
        return typep(x, _frame)

def _next_frame(f):
        return f.f_back if f.f_back else error("Frame \"%s\" is the last frame.", _pp_frame(f, lineno = True))

def _caller_frame(caller_relative = 0):
        return sys._getframe(caller_relative + 2)

def _caller_name(n = 0):
        return _fun_name(_frame_fun(sys._getframe(n + 2)))

def _exception_frame():
        return sys.exc_info()[2].tb_frame

def _frames_upward_from(f = None, n = -1):
        "Semantics of N are slightly confusing, but the implementation is so simple.."
        f = _caller_frame() if f is None else the(_frame, f)
        acc = [f]
        while f.f_back and n:
                f, n = f.f_back, n - 1
                acc.append(f)
        return acc

def _top_frame():
        return _caller_frame()

def _frame_info(f):
        "Return frame (function, lineno, locals, globals, builtins)."
        return (f.f_code,
                f.f_lineno,
                f.f_locals,
                f.f_globals,
                f.f_builtins,
                )

def _frame_fun(f):               return f.f_code
def _frame_lineno(f):            return f.f_lineno
def _frame_locals(f):            return f.f_locals
def _frame_globals(f):           return f.f_globals
def _frame_local_value(f, name): return f.f_locals[name]

### XXX: this is the price of Pythonic pain
__ordered_frame_locals__ = dict()
def _frame_ordered_locals(f):
        global __ordered_frame_locals__
        if f not in __ordered_frame_locals__:
                __ordered_frame_locals__[f] = list(f.f_locals.keys())
        return __ordered_frame_locals__[f]

def _fun_info(f):
        "Return function (name, params, filename, lineno, nlines)."
        return (f.co_name or "<unknown-name>",
                f.co_varnames[:f.co_argcount], # parameters
                f.co_filename or "<unknown-file>",
                f.co_firstlineno,
                1 + max(f.co_lnotab or [0]),        # lines
                f.co_varnames[f.co_argcount:], # non-parameter bound locals
                f.co_freevars,
                )
def _fun_name(f):       return f.co_name
def _fun_filename(f):   return f.co_filename
def _fun_bytecode(f):   return f.co_code
def _fun_constants(f):  return f.co_consts

def _print_function_arglist(f):
        argspec = inspect.getargspec(f)
        return ", ".join(argspec.args +
                         (["*" + argspec.varargs]   if argspec.varargs  else []) +
                         (["**" + argspec.keywords] if argspec.keywords else []))

def _pp_frame(f, align = None, handle_overflow = None, lineno = None):
        fun = _frame_fun(f)
        fun_name, fun_params, filename = _fun_info(fun)[:3]
        align = ((align or 10) if handle_overflow else
                 _defaulted(align, 0))
        return ("%s%s %s(%s)" % (filename + ("" if align else ":") + (" " * (align - (len(filename) % align if align else 0))),
                                 ("%d:" % _frame_lineno(f)) if lineno else "",
                                 fun_name, ", ".join(fun_params)))

def _print_frame(f, stream = None, **keys):
        write_string(_pp_frame(f, **keys), _defaulted_to_var(stream, "_debug_io_"))

def _print_frames(fs, stream = None):
        mapc(lambda i, f: format(_defaulted_to_var(stream, "_debug_io_"), "%2d: %s\n" % (i, _pp_frame(f, lineno = True))),
             *zip(*enumerate(fs)))

def _backtrace(x = -1, stream = None):
        _print_frames(_frames_upward_from(_this_frame())[1:x],
                      _defaulted_to_var(stream, "_debug_io_"))

def _pp_frame_chain(xs):
        return "..".join(mapcar(lambda f: _fun_name(_frame_fun(f)), xs))

def _here(note = None, *args, callers = 5, stream = None, default_stream = sys.stderr):
        frames = _frames_upward_from(_caller_frame(), callers - 1)
        names = _pp_frame_chain(reversed(frames))
        string = (""           if not note else
                  " - " + note if not args else
                  (note % args))
        return write_line("    (%s)  %s:\n      %s" % (threading.current_thread().name.upper(),
                                                       names, string),
                          _defaulted(stream, default_stream))

# Study was done by the means of:
# print("\n".join(map(lambda f:
#                             "== def %s\n%s\n" %
#                     (fun_name(f),
#                      "\n  ".join(map(lambda s: s + ": " + str(getattr(f, s)),
#                                      ["co_argcount",
#                                       "co_cellvars",
#                                       "co_names",
#                                       "co_varnames",
#                                       "co_freevars",
#                                       "co_nlocals"]))),
#                     ffuns)))

# == def xceptor
# co_argcount: 1
#   co_cellvars: ()
#   co_names: ("error", "Exception", "this_frame")
#   co_varnames: ("xceptor_arg", "cond")
#   co_freevars: ()
#   co_nlocals: 2

# == def midder
# co_argcount: 1
#   co_cellvars: ()
#   co_names: ()
#   co_varnames: ("midder_arg", "midder_stack_var")
#   co_freevars: ("xceptor",)
#   co_nlocals: 2

# == def outer
# co_argcount: 0
#   co_cellvars: ()
#   co_names: ()
#   co_varnames: ("outer_stack_var",)
#   co_freevars: ("midder",)
#   co_nlocals: 1

# == def example_frame
# co_argcount: 0
#   co_cellvars: ("xceptor", "midder")
#   co_names: ()
#   co_varnames: ("outer",)
#   co_freevars: ()
#   co_nlocals: 1

# == def <module>
# co_argcount: 0
#   co_cellvars: ()
#   co_names: ("example_frame", "f")
#   co_varnames: ()
#   co_freevars: ()
#   co_nlocals: 0

# More info:
# sys.call_tracing()
# p = Pdb(self.completekey, self.stdin, self.stdout)
# p.prompt = "(%s) " % self.prompt.strip()
# print >>self.stdout, "ENTERING RECURSIVE DEBUGGER"
# sys.call_tracing(p.run, (arg, globals, locals))
# print >>self.stdout, "LEAVING RECURSIVE DEBUGGER"
# sys.settrace(self.trace_dispatch)
# self.lastcmd = p.lastcmd

##
## Condition: not_implemented
##
condition = BaseException
error_    = Exception

def _conditionp(x):
        return typep(x, condition)

class simple_condition(condition):
        def __init__(self, format_control, *format_arguments):
                self.format_control, self.format_arguments = format_control, format_arguments
        def __str__(self):
                return self.format_control % tuple(self.format_arguments)
        def __repr__(self):
                return self.__str__()

class warning(condition): pass

class simple_warning(simple_condition, warning): pass

class _not_implemented_error(error_):
        def __init__(*args):
                self, name = args[0], args[1]
                self.name = name
        def __str__(self):
                return "Not implemented: " + self.name.upper()
        def __repr__(self):
                return self.__str__()

def _not_implemented(x = None):
        error(_not_implemented_error,
              x if x is not None else
              _caller_name())

##
## Pergamum 0
##
def _if_let(cond, consequent, antecedent = lambda: None):
        x = cond() if isinstance(cond, function_) else cond
        return consequent(x) if x else antecedent()

def _when_let(cond, consequent):
        x = cond() if isinstance(cond, function_) else cond
        return consequent(x) if x else None

def _lret(value, body):
        body(value)
        return value

_curry = functools.partial

def _compose(f, g):
        return lambda *args, **keys: f(g(*args, **keys))

def _tuplep(x):       return type(x) is tuple
def _frozensetp(o):   return type(o) is frozenset
def _setp(o):         return type(o) is set or _frozensetp(o)

def _ensure_list(x):
        return x if listp(x) else [x]

def _mapset(f, xs):
        acc = set()
        for x in xs:
                acc.add(f(x))
        return acc

def _mapsetn(f, xs):
        acc = set()
        for x in xs:
                acc |= f(x)
        return acc

def _mapcar_star(f, xs):
        return [ f(*x) for x in xs ]

def _slotting(x):             return lambda y: getattr(y, x, None)

def _updated_dict(to, from_):
        to.update(from_)
        return to

def _stream_as_string(stream):
        return stream.read()

def _file_as_string(filename):
        with open(filename, "r") as f:
                return _stream_as_string(f)

##
## Lesser non-CL tools
##
def ___(str, expr):
        write_string("%s: %s" % (str, expr))
        return expr

class _servile():
        def __repr__(self):
                return "#%s(%s)" % (type(self).__name__,
                                    ", ".join(_maphash(lambda k, v: "%s = %s" % (k, v),
                                                       self.__dict__)))
        def __init__(self, **keys):
                self.__dict__.update(keys)

##
## Symbols
##
__gensym_counter__ = 0
def gensym(x = "G"):
        global __gensym_counter__
        __gensym_counter__ += 1
        return make_symbol(x + str(__gensym_counter__))

##
## Basic
##
__iff__ = { True:  lambda x, _: x,
            False: lambda _, y: y }
def iff(val, consequent, antecedent):
        "This restores sanity."
        return __iff__[not not val](consequent, antecedent)()

def constantp(x):
        return type(x) in set([str, int])

def loop(body):
        while True:
                body()

def eq(x, y):
        return x is y

def eql(x, y):
        ## Python is really cute:
        # >>> 256 is (255 + 1)
        # True
        # >>> 257 is (256 + 1)
        # False
        return (x is y) if not isinstance(x, int) else x == y

def equal(x, y):
        return x == y

def destructuring_bind(val, body):
        return body(*tuple(val))

def _destructuring_bind_keys(val, body):
        return body(**val)

def when(test, body):
        if test:
                return body() if isinstance(body, function_) else body
def cond(*clauses):
        for (test, body) in clauses:
                if test() if isinstance(test, function_) else test:
                        return body() if isinstance(body, function_) else body
def case(val, *clauses):
        for (cval, body) in clauses:
                if val == cval or (cval is True) or (cval is t):
                        return body() if isinstance(body, function_) else body

def ecase(val, *clauses):
        for (cval, body) in clauses:
                if val == cval:
                        return body() if isinstance(body, function_) else body
        error("%s fell through ECASE expression. Wanted one of %s.", val, mapcar(first, clauses))

def every(fn, xs):
        for x in xs:
                if not fn(x): return False
        return True

def some(fn, xs):
        for x in xs:
                if fn(x): return True
        return False

def none(fn, xs):
        for x in xs:
                if fn(x): return False
        return True

##
## Types
##
class type_error(error_):
        pass

class simple_type_error(simple_condition, type_error):
        pass

type_  = builtins.type     # Should we shadow org.python.type?
stream = _io._IOBase

def find_class(x, errorp = True):
        check_type(x, symbol)
        return (x.value if typep(x.value, type_) else
                nil     if not errorp            else
                error("There is no class named %s.", x))

def type_of(x):
        return type(x)

# __type_predicate_map__ is declared after the package system is initialised
def _check_complex_type(type, x):
        zero, test, element_test = __type_predicate_map__[type[0]]
        return (zero if len(type) is 1 else
                test(lambda elem_type: element_test(x, elem_type),
                     type[1:]))

def typep(x, type):
        return (isinstance(x, type)          if isinstance(type, type_)                      else
                _check_complex_type(type, x) if (listp(type) and
                                                 type and type[0] in __type_predicate_map__) else
                error(simple_type_error, "%s is not a valid type specifier.", type))

def subtypep(sub, super):
        return issubclass(sub, super)

def the(type, x):
        return (x if typep(x, type) else
                error(simple_type_error, "The value %s is not of type %s.", x, type))

def check_type(x, type):
        the(type, x)

def typecase(val, *clauses):
        for (ctype, body) in clauses:
                if (ctype is t) or (ctype is True) or typep(val, ctype):
                        return body() if isinstance(body, function_) else body

def etypecase(val, *clauses):
        for (ctype, body) in clauses:
                if (ctype is t) or (ctype is True) or typep(val, ctype):
                        return body() if isinstance(body, function_) else body
        else:
                error(simple_type_error, "%s fell through ETYPECASE expression. Wanted one of (%s).",
                      val, ", ".join(mapcar(lambda c: c[0].__name__, clauses)))

def coerce(x, type):
        if type(x) is type:
                return x
        elif type is list:
                return list(x)
        elif type is set:
                return set(x)
        elif type is dict:
                return dict.fromkeys(x)

##
## Type predicates
##
__function_types__ = frozenset([types.BuiltinFunctionType,
                                types.BuiltinMethodType,
                                types.FunctionType,
                                types.LambdaType,
                                types.MethodType])

function_ = types.FunctionType.__mro__[0]
integer   = int

def function(name):
        pyname, module = _lisp_symbol_python_addr(name)
        return the(function_, _lisp_symbol_python_value(name))

def functionp(o):     return isinstance(o, function_)
def integerp(o):      return type(o) is int
def floatp(o):        return type(o) is float
def complexp(o):      return type(o) is complex
def numberp(o):       return type(o) in frozenset([float, int, complex])
def listp(o):         return type(o) is list
def boolp(o):         return type(o) is bool
def sequencep(x):     return getattr(type(x), "__len__", None) is not None
def hash_table_p(o):  return type(o) is dict

##
## Predicates
##
def null(x):          return not x
def evenp(x):         return not (x % 2)
def oddp(x):          return not not (x % 2)
def zerop(x):         return x == 0
def plusp(x):         return x > 0
def minusp(x):        return x < 0

##
## Multiple values
##
def values(*xs):
        return xs

def nth_value(n, xs):
        return nth(n, xs)

def multiple_value_bind(values_form, body):
        return body(*values_form)

def multiple_value_list(values_form):
        return list(values_form)

def multiple_values_list(list):
        return tuple(list)

def multiple_value_call(function, *values_forms):
        return function(*(append(*values_forms)))

##
## Conses
##
def cons(x, y):       return (x, y)
def consp(o):         return type(o) is tuple and len(o) is 2
def atom(o):          return type(o) is not tuple
def car(x):           return x[0]
def cdr(x):           return x[1]
def cadr(x):          return x[1][0]

def copy_list(x):
        return list(the(list, x))

##
## Functions
##
def complement(f):
        return lambda x: not f(x)

def constantly (x):
        return lambda *args: x

def prog1(val, body):
        body()
        return val

##
## Sequences
##
def getf(xs, key):
        for i, x in enumerate(xs):
                if not i%2 and x == key:
                        return xs[i + 1]
        else:
                return nil

def assoc(x, xs, test = equal):
        for k, v in xs:
                if test(x, k):
                        return v

def aref(xs, *indices):
        r = xs
        for i in indices:
                r = r[i]
        return r
def first(xs):        return xs[0]   # don't confuse with car/cdr
def rest(xs):         return xs[1:]  # !!!

def nth(n, xs):       return xs[n] if n < len(xs) else nil

def subseq(xs, start, end = None):
        return xs[start:end]

def make_list(size, initial_element = None):
        # horribly inefficient, but that's what we have..
        return mapcar(constantly(initial_element), range(size))

def append(*xs): return reduce(lambda x, y: x + y, xs) if (xs and xs[0]) else []

def mapcar(f, *xs):
        return [ f(*x) for x in zip(*xs) ]

def mapcan(f, *xs):
        return reduce(append, [ f(*x) for x in zip(*xs) ]) if (xs and xs[0]) else []

def mapc(f, *xs):
        for x in zip(*xs):
                f(*x)
        return xs[0]

__allowed__ = frozenset([set, frozenset, tuple, list, bytes, bytearray, str])
def _maprestype(x):
        type = type_of(x)
        return type if type in __allowed__ else list

def remove_if(f, xs, key = identity):
        if isinstance(xs, dict):
                return              { k:x for k, x in xs.items() if not f(k, key(x))}
        else:
                return _maprestype(xs) (x for x    in xs         if not f(key(x)))

def remove_if_not(f, xs, key = identity):
        if isinstance(xs, dict):
                return              { k:x for k, x in xs.items() if f(k, key(x))}
        else:
                return _maprestype(xs) (x for x    in xs         if f(key(x)))

def remove(elt, xs, test = eq, key = identity):
        if isinstance(xs, dict):
                return              { k:x for k, x in xs.items() if test(elt, key(x))}
        else:
                return _maprestype(xs) (x for x    in xs         if test(elt, key(x)))

def find_if(p, xs, key = identity, start = 0, end = None, from_end = None):
        end = end or len(xs)
        if start or end:
                seq = zip(xs, range(len(xs)))
                if from_end:
                        seq = reversed(list(seq))
                for (x, i) in seq:
                        if (start <= i < end) and p(key(x)):
                                return x
        else:
                if from_end:
                        xs = reversed(xs)
                for x in xs:
                        if p(key(x)):
                                return x

def find(elt, xs, **keys):
        return find_if(lambda x: x == elt, xs, **keys)

def member_if(test, xs):
        "XXX: not terribly compliant."
        for i, x in enumerate(xs):
                if test(x):
                        return xs[i:]

def member(x, xs):
        "XXX: not terribly compliant."
        return member_if(lambda y: y == x, xs)

def position_if(p, xs, key = identity, start = 0, end = None, from_end = None):
        end = end or len(xs)
        if start or end:
                seq = zip(xs, range(len(xs)))
                if from_end:
                        seq = reversed(list(seq))
                for (x, i) in seq:
                        if (start <= i < end) and p(key(x)):
                                return i
        else:
                i, increment, seq = ((end - 1, -1, reversed(xs))
                                     if from_end else
                                     (      0,  1, xs))
                for x in seq:
                        if p(key(x)):
                                return i
                        i += increment

def position_if_not(p, xs, key = identity, start = 0, end = None, from_end = None):
        return position_if(complement(p), xs, key = key, start = start, end = end, from_end = from_end)

def position(elt, xs, **keys):
        return position_if(lambda x: x == elt, xs, **keys)

def count(elt, xs, key = identity, start = 0):
        c = 0
        for (x, i) in zip(xs, range(len(xs))):
                if (i >= start) and key(x) == elt:
                        c += 1
        return c

def count_if(p, xs, key = identity, start = 0):
        c = 0
        for (x, i) in zip(xs, range(len(xs))):
                if (i >= start) and p(key(x)):
                        c += 1
        return c

sort = sorted

def replace(sequence_1, sequence_2, start1 = 0, start2 = 0, end1 = None, end2 = None):
        """Destructively modifies sequence-1 by replacing the elements
of subsequence-1 bounded by start1 and end1 with the elements of
subsequence-2 bounded by start2 and end2. """
        # XXX: this will bomb out when designated subsequence of sequence_2 is
        #      shorter than that of sequence_1, which is quite fine by CL:REPLACE:
        # 
        # "If these subsequences are not of the same length, then the
        #  shorter length determines how many elements are copied; the
        #  extra elements near the end of the longer subsequence are not
        #  involved in the operation."
        sequence_1[start1:end1] = sequence_2[start2:end2]
        return sequence_1

# XXX: This is geared at cons-style lists, and so is fucking costly
# for imperative lists.
def tailp(object, list):
        """If OBJECT is the same as some tail of LIST, TAILP returns
true; otherwise, it returns false."""
        if len(object) > len(list):
                return None
        else:
                list_start = len(list) - len(object)
                return list[list_start:] == object

# XXX: This is geared at cons-style lists, and so is fucking costly
# for imperative lists.
def ldiff(object, list_):
        """If OBJECT is the same as some tail of LIST, LDIFF returns a
fresh list of the elements of LIST that precede OBJECT in the
list structure of LIST; otherwise, it returns a copy[2] of
LIST."""
        if len(object) > len(list_):
                return list(list_)
        else:
                list_start = len(list_) - len(object)
                if list_[list_start:] == object:
                        return list_[:list_start]
                else:
                        return list(list_)

##
## Strings
##
def string_equal(xs, ys):            return xs == ys
def string_greater(xs, ys):          return xs > ys
def string_greater_or_equal(xs, ys): return xs >= ys
def string_less(xs, ys):             return xs < ys
def string_less_or_equal(xs, ys):    return xs <= ys

def string_right_trim(cs, s):
        return s.rstrip("".join(cs))

def string_left_trim(cs, s):
        return s.lstrip("".join(cs))

def string_trim(cs, s):
        return s.strip("".join(cs))

def with_output_to_string(f):
        x = make_string_output_stream()
        try:
                f(x)
                return get_output_stream_string(x)
        finally:
                close(x)

def with_input_from_string(s, f):
        x = make_string_input_stream(s)
        try:
                return f(x)
        finally:
                close(x)

##
## Sets
##
def union(x, y):
        return x | y

def intersection(x, y):
        return x & y

##
## Dicts
##
def gethash(key, dict):
        inp = key in dict
        return (dict.get(key) if inp else None), key in dict

# Issue INCONSISTENT-HASH-TABLE-FUNCTION-NAMING
def _maphash(f, dict):
        return [ f(k, v) for k, v in dict.items() ]

def _remap_hash_table(f, xs):
        return { k: f(k, v) for k, v in xs.items() }

def _map_into_hash(f, xs, key = identity):
        acc = dict()
        for x in xs:
                k, v = f(key(x))
                acc[k] = v
        return acc

##
## Non-local control transfers
##
def unwind_protect(form, fn):
        "For the times, when statements won't do."
        try:
                return form()
        finally:
                fn()

# WARNING: non-specific try/except clauses and BaseException handlers break this!
class __catcher_throw__(condition):
        def __init__(self, ball, value, reenable_pytracer = False):
                self.ball, self.value, self.reenable_pytracer = ball, value, reenable_pytracer

def catch(ball, body):
        "This seeks the stack like mad, like the real one."
        check_type(ball, symbol)
        try:
                return body()
        except __catcher_throw__ as ct:
                # format(t, "catcher %s, ball %s -> %s", ct.ball, ball, "caught" if ct.ball is ball else "missed")
                if ct.ball is ball:
                        if ct.reenable_pytracer:
                                _enable_pytracer()
                        return ct.value
                else:
                        raise

def throw(ball, value):
        "Stack this seeks, like mad, like the real one."
        check_type(ball, symbol)
        raise __catcher_throw__(ball = ball, value = value, reenable_pytracer = boundp("_signalling_frame_"))

def __block__(fn):
        "An easy decorator-styled interface for block establishment."
        nonce = gensym("BLOCK")
        ret = (lambda *args, **keys:
                       catch(nonce,
                             lambda: fn(*args, **keys)))
        setattr(ret, "ball", nonce)
        return ret

def block(nonce_or_fn, body = None):
        """A lexically-bound counterpart to CATCH/THROW.
Note, how, in this form, it is almost a synonym to CATCH/THROW -- the lexical aspect
of nonce-ing is to be handled manually."""
        if not body: # Assuming we were called as a decorator..
                return __block__(nonce_or_fn)
        else:
                return catch(nonce_or_fn, body)

def return_from(nonce, value):
        nonce = (nonce if not isinstance(nonce, function_) else
                 (getattr(nonce, "ball", None) or
                  error("RETURN-FROM was handed a %s, but it is not cooperating in the __BLOCK__ nonce passing syntax.", nonce)))
        throw(nonce, value)

##
## Dynamic scope
##
__global_scope__ = dict()

class thread_local_storage(threading.local):
        def __init__(self):
                self.dynamic_scope = []

__tls__ = thread_local_storage()

def _boundp(name):
        name = _coerce_to_symbol_name(name)
        for scope in reversed(__tls__.dynamic_scope):
                if name in scope:
                        return t
        if name in __global_scope__:
                return t

def _find_dynamic_frame(name):
        for scope in reversed(__tls__.dynamic_scope):
                if name in scope:
                        return scope
        if name in __global_scope__:
                return __global_scope__

def _symbol_value(name):
        frame = _find_dynamic_frame(name)
        return (frame[name] if frame else
                error(AttributeError, "Unbound variable: %s." % name))

def _coerce_cluster_keys_to_symbol_names(dict):
        return { _coerce_to_symbol_name(var):val for var, val in dict.items() }

def boundp(symbol):
        return _boundp(_coerce_to_symbol_name(symbol))

def symbol_value(symbol):
        return (_symbol_value(_coerce_to_symbol_name(symbol)) if stringp(symbol) else
                symbol.value                                  if symbolp(symbol) else
                error(simple_type_error, "SYMBOL-VALUE accepts either strings or symbols, not '%s'.",
                      symbol))

def setq(name, value):
        name = _coerce_to_symbol_name(name)
        frame = (_find_dynamic_frame(name) or
                 (__tls__.dynamic_scope[-1] if __tls__.dynamic_scope else
                  __global_scope__))
        frame[name] = value
        return value

# defvar(name, value, documentation = nil):
# defparameter(name, value, documentation = nil):

class _env_cluster(object):
        def __init__(self, cluster):
                self.cluster = cluster
        def __enter__(self):
                __tls__.dynamic_scope.append(_coerce_cluster_keys_to_symbol_names(self.cluster))
        def __exit__(self, t, v, tb):
                __tls__.dynamic_scope.pop()

class _dynamic_scope(object):
        "Courtesy of Jason Orendorff."
        def let(self, **keys):
                return _env_cluster(keys)
        def maybe_let(self, p, **keys):
                return _env_cluster(keys) if p else None
        def __getattr__(self, name):
                return symbol_value(name)
        def __setattr__(self, name, value):
                error(AttributeError, "Use SETQ to set special globals.")

__dynamic_scope__ = _dynamic_scope()
env = __dynamic_scope__             # shortcut..

def progv(vars = None, vals = None, body = None, **cluster):
        """Two usage modes:
progv([\"foovar\", \"barvar\"],
      [3.14, 2.71],
      lambda: body())

with progv(foovar = 3.14,
           barvar = 2.71):
      body()

..with the latter being lighter on the stack frame usage."""
        if body:
                with _env_cluster(_map_into_hash(lambda vv: (_coerce_to_symbol_name(vv[0]), vv[1]),
                                                 zip(vars, vals))):
                        return body()
        else:
                return _env_cluster(_coerce_cluster_keys_to_symbol_names(cluster))

##
## Package system
##
__packages__         = dict()
__builtins_package__ = None
__keyword_package__  = None
__modular_noise__    = None

class package_error(error_):
        pass

class simple_package_error(simple_condition, package_error):
        pass

def symbol_conflict_error(op, obj, pkg, x, y):
        error(simple_package_error, "%s %s causes name-conflicts in %s between the following symbols: %s, %s." %
              (op, obj, pkg, x, y))

def _symbol_accessible_in(x, package):
        return (x.name in package.accessible and
                package.accessible[x.name] is x)

def symbols_not_accessible_error(package, syms):
        def pp_sym_or_string(x):
                return "\"%s\"" % x if stringp(x) else _print_nonkeyword_symbol(x)
        error(simple_package_error, "These symbols are not accessible in the %s package: (%s).",
              package_name(package), ", ".join(mapcar(pp_sym_or_string, syms)))

def _use_package_symbols(dest, src, syms):
        assert(packagep(dest) and packagep(src) and hash_table_p(syms))
        conflict_set = _mapset(_slotting("name"), syms.values()) & set(dest.accessible.keys())
        for name in conflict_set:
                if syms[name] is not dest.accessible[name]:
                        symbol_conflict_error("USE-PACKAGE", src, dest, syms[name], dest.accessible[name])
        ## no conflicts anymore? go on..
        for name, sym in syms.items():
                dest.inherited[sym].add(src)
                if name not in dest.accessible: # Addition of this conditional is important for package use loops.
                        dest.accessible[name] = sym
                        # if dest.name == "SWANK" and src.name == "INSPECTOR":
                        #         debug_printf("merging %s into %s: test: %s", s, dest, _read_symbol(_print_nonkeyword_symbol(s)))
                if dest.module and name not in dest.module.__dict__:
                        dest.module.__dict__[name] = sym.value

def use_package(dest, src):
        dest, src = _coerce_to_package(dest), _coerce_to_package(src)
        symhash = _map_into_hash(lambda x: (x.name, x), src.external)
        _use_package_symbols(dest, src, symhash)
        src.packages_using.add(dest)
        dest.used_packages.add(src)

def package_used_by_list(package):
        package = _coerce_to_package(package)
        return package.packages_using

def _lisp_symbol_name_python_name(x):
        def _sub(cs):
                acc = ""
                for c in cs:
                        acc += "_" if c in "-*" else c
                return acc
        ret = _sub(x).lower()
        # debug_printf("==> Python(Lisp %s) == %s", x, ret)
        return ret

def _lisp_symbol_python_name(sym):
        return _lisp_symbol_name_python_name(sym.name)

def _lisp_symbol_python_names(sym):
        return (_lisp_symbol_name_python_name(sym.name),
                _lisp_symbol_name_python_name(sym.package.name))

def _find_module(name, if_does_not_exist = "error"):
        return (gethash(name, sys.modules)[0] or
                ecase(if_does_not_exist,
                      ("continue",
                       None),
                      ("error",
                       lambda: error(simple_package_error, "The name %s does not designate any package.",
                                     name))))

def _lisp_symbol_python_addr(sym):
        symname, packname = _lisp_symbol_python_names(sym)
        return symname, _find_module(packname)

def _lisp_symbol_python_value(sym):
        name, module = _lisp_symbol_python_addr(sym)
        value, presentp = gethash(name, module.__dict__)
        return (value if presentp else
                error(simple_package_error, "This name is not accessible in the '%s' module: '%s'.",
                      module.__name__, name))

def _lisp_symbol_ast(sym, current_package):
        symname, packname = _lisp_symbol_python_names(sym)
        return (_ast_name(symname) if _symbol_accessible_in(sym, current_package) else
                _ast_index(_ast_attribute(_ast_index(_ast_attribute(_ast_name("sys"), "modules"), _ast_string(packname)),
                                          "__dict__"),
                           _ast_string(symname)))

class package(collections.UserDict):
        def __repr__ (self):
                return "#<PACKAGE \"%s\">" % self.name
        def __bool__(self):
                return True
        def __hash__(self):
                return hash(id(self))
        def __init__(self, name, use = [], filename = "",
                     ignore_python = False, python_exports = True, boot = False):
                self.name = string(name)

                self.own         = set()                        # sym
                self.imported    = set()                        # sym
              # self.present     = own + imported
                self.inherited   = collections.defaultdict(set) # sym -> set(pkg) ## _mapsetn(_slotting("external"), used_packages) -> source_package
                self.accessible  = dict()                       # str -> sym      ## accessible = present + inherited
                self.external    = set()                        # sym             ## subset of accessible
              # self.internal    = accessible - external

                modname = _lisp_symbol_name_python_name(name)
                self.module = (_find_module(modname, if_does_not_exist = "continue") or
                               _load_text_as_module(modname, "", filename = filename))
                # Issue _CCOERCE_TO_PACKAGE-WEIRD-DOUBLE-UNDERSCORE-NAMING-BUG
                coercer = (_ccoerce_to_package if boot else
                           _coerce_to_package)
                self.used_packages  = set(mapcar(lambda x: coercer(x, if_null = "error"),
                                                 use))
                self.packages_using = set()
                assert(every(packagep, self.used_packages))
                mapc(_curry(use_package, self), self.used_packages)

                ## Import the corresponding python dictionary.  Intern depends on
                if not ignore_python:
                        moddict = dict(self.module.__dict__)
                        explicit_exports = set(moddict["__all__"] if "__all__" in moddict else
                                               [])
                        for (key, value) in moddict.items():
                                ## intern the python symbol, when it is known not to be inherited
                                if key not in self.accessible:
                                        s = _intern0(key, self)
                                        s.value = value
                                        if functionp(value):
                                                s.function = value
                                ## export symbols, according to the python model
                                if (python_exports and key[0] != "_" and
                                    ((not explicit_exports) or
                                     key in explicit_exports)):
                                        self.external.add(self.accessible[key])
                ## Hit the street.
                self.data          = self.accessible
                __packages__[name] = self
def packagep(x):     return typep(x, package)
def package_name(x): return x.name

def make_package(name, nicknames = [], use = []):
        "XXX: NICKNAMES are ignored."
        return package(string(name), ignore_python = True, use = [])

def _find_package(name, errorp = True):
        return (__packages__.get(name) if name in __packages__ else
                nil                    if not errorp           else
                error("Package with name '%s' does not exist.", name))
def find_package(name, errorp = False):
        return _find_package(_coerce_to_symbol_name(name), errorp)

# Issue _CCOERCE_TO_PACKAGE-WEIRD-DOUBLE-UNDERSCORE-NAMING-BUG
def _ccoerce_to_package(x, if_null = "current", **args):
        return (x                         if packagep(x)                      else
                symbol_value("_package_") if (not x) and if_null == "current" else
                _find_package(x)          if stringp(x) or symbolp(x)         else
                error(simple_type_error, "CCOERCE-TO-PACKAGE accepts only package designators -- packages, strings or symbols, was given '%s' of type %s.",
                      x, type_of(x)))
def _coerce_to_package(x, if_null = "current"):
        return (x                         if packagep(x)                      else
                symbol_value("_package_") if (not x) and if_null == "current" else
                find_package(x, True)     if stringp(x) or symbolp(x)         else
                error(simple_type_error, "COERCE-TO-PACKAGE accepts only package designators -- packages, strings or symbols, was given '%s' of type %s.",
                      x, type_of(x)))

def defpackage(name, use = [], export = []):
        p = package(name, use = use)
        for symname in export:
                _not_implemented("DEFPACKAGE: :EXPORT keyword") # XXX: populate the for-INTERN-time-export set of names
        return p

def in_package(name):
        setq("_package_", _coerce_to_package(name))

class symbol():
        def __str__(self):
                return _print_symbol(self)
        def __repr__(self):
                return str(self)
        def __init__(self, name):
                self.name, self.package, self.value, self.function = name, None, None, None
        def __hash__(self):
                return hash(self.name) ^ (hash(self.package.name) if self.package else 0)
        def __bool__(self):
                return self is not nil
def symbolp(x):                      return typep(x, symbol)
def keywordp(x):                     return symbolp(x) and symbol_package(x) is __keyword_package__
def symbol_name(x):                  return x.name.lower()
def symbol_package(x):               return x.package
def coerce_to_symbol(s_or_n, package = None):
        return intern(s_or_n, _coerce_to_package(package))

def make_symbol(name):
        return symbol(name)

def symbol_relation(x, p):
        "NOTE: here we trust that X belongs to P, when it's a symbol."
        s = (p.accessible.get(x) if x in p.accessible else None) if stringp(x) else x
        if s is not None:
                return (_keyword("inherited") if s.name in p.inherited else
                        _keyword("external")  if s in p.external else
                        _keyword("internal"))

def _find_symbol(x, package):
        s = package.accessible.get(x) if x in package.accessible else None
        if s is not None:
                # format(t, "FIND-SYMBOL:%s, %s -> %s, %s\n", 
                #        x, package, s, symbol_relation(s, p))
                return s, symbol_relation(s, package)
        else:
                return None, None
def find_symbol(x, package = None):
        return _find_symbol(x, _coerce_to_package(package))
def _find_symbol0(x, package = None): return find_symbol(x, package)[0]

def _find_symbol_or_fail(x, package = None):
        p = _coerce_to_package(package)
        sym, foundp = find_symbol(x, p)
        return (sym if foundp else
                symbols_not_accessible_error(p, [x]))

def _intern(x, package = None):
        p = _coerce_to_package(package)
        s = (p.accessible.get(x) if x in p.accessible else None) if stringp(x) else x
        if not (s is not None or stringp(x)):
                error("Attempted to intern object >%s< of type %s into %s.", x, type(x), p)
        if s:
                # debug_printf("Found >%s< in %s.", s, p)
                return s, p
        else:
                s = symbol(x)
                p.own.add(s)
                p.accessible[x], s.package = s, p
                # debug_printf("Interned >%s< into %s.", s, p)
                if p is __keyword_package__:
                        # CLHS 11.1.2.3.1 Interning a Symbol in the KEYWORD Package
                        p.external.add(s)
                        s.value = s
                return s, None
def intern(x, package = None):
        s, found_in_package = _intern(x, package)
        return s, (symbol_relation(s, found_in_package) if found_in_package else
                   None)
def _intern0(x, package = None): return intern(x, package)[0]

# requires that __keyword_package__ is set, otherwise _intern will fail with _COERCE_TO_PACKAGE
def _keyword(s, upcase = True):
        return _intern((s.upper() if upcase else s), __keyword_package__)[0]

def import_(symbols, package = None, populate_module = True):
        p = _coerce_to_package(package)
        symbols = _ensure_list(symbols)
        module = _find_module(_lisp_symbol_name_python_name(package_name(p)),
                              if_does_not_exist = "continue")
        for s in symbols:
                ps = p.get(s.name) if s.name in p else None
                if ps is not None: # conflict
                        symbol_conflict_error("IMPORT", s, p, s, ps)
                else:
                        p.imported.add(s)
                        p.accessible[s.name] = s
                        if module:
                                # Issue SYMBOL-VALUES-NOT-SYNCHRONISED-WITH-PYTHON-MODULES
                                python_name = _lisp_symbol_name_python_name(s.name)
                                module.__dict__[python_name] = s.value
        return True

def export(symbols, package = None):
        symbols, package = _ensure_list(symbols), _coerce_to_package(package)
        assert(every(symbolp, symbols))
        symdict = _map_into_hash(lambda x: (x.name, x), symbols)
        for user in package.packages_using:
                _use_package_symbols(user, package, symdict)
        # No conflicts?  Alright, we can proceed..
        symset = set(symdict.values())
        for_interning = symset & set(package.inherited)
        for sym in for_interning:
                del package.inherited[sym]
                self.internal.add(sym)
        package.external |= symset
        return True

def string(x):
        return (x              if stringp(x) else
                symbol_name(x) if symbolp(x) else
                error(simple_type_error, "%s cannot be coerced to string.", x))

def _init_condition_system():
        _enable_pytracer() ## enable HANDLER-BIND and RESTART-BIND

def _without_condition_system(body):
        if _pytracer_enabled_p():
                try:
                        _disable_pytracer()
                        return body()
                finally:
                        _enable_pytracer()
        else:
                return body()

def _condition_system_enabled_p():
        return (_pytracer_enabled_p() and
                _tracer_hook("exception") is __cl_condition_handler__)

def _init_package_system_0():
        # debug_printf("   --  -- [ package system init..")
        global __packages__
        global __builtins_package__
        global __keyword_package__
        global __modular_noise__
        global t, nil
        __packages__ = dict()
        __builtins_package__ = package("BUILTINS", boot = True)
        __keyword_package__ = package("KEYWORD", ignore_python = True, boot = True)
        __modular_noise__ = frozenset(_load_text_as_module("", "").__dict__)
        cl = package("CL", use = ["BUILTINS"], boot = True)
        intern(".", cl)

        t                  = _intern0("T", cl)       # Nothing much works without these..
        nil                = _intern0("NIL", cl)
        t.value, nil.value = t, nil     # Self-evaluation.
        export([t, nil] + mapcar(lambda n: _intern0(n, cl),
                                 __core_symbol_names__ +
                                 __more_symbol_names__),
               cl)
        def pythonise_core_symbol_name(x):
                return x.lower() + "_"
        for sym in __core_symbol_names__:
                sys.modules['cl'].__dict__[pythonise_core_symbol_name(sym)] = _find_symbol_or_fail(sym, cl)
        package("COMMON-LISP-USER", use = ["CL", "BUILTINS"], boot = True)
_init_package_system_0() ########### _keyword(), quote_, and_, or_, abort_, continue_, break_ are now available

def _init_reader_0():
        "SETQ, SYMBOL_VALUE, LET and BOUNDP (anything calling _COERCE_TO_SYMBOL_NAME) need this to mangle names."
        __global_scope__["_READ_CASE_"] = _keyword("upcase", upcase = True)
_init_reader_0()         ########### _coerce_to_symbol_name() is now available

def _init_package_system_1():
        # Ought to declare it all on the top level.
        in_package("COMMON-LISP-USER")
        setq("_features_", [])
        setq("_modules_",  [])

_init_package_system_1()

__type_predicate_map__ = { _keyword("or"):     (nil, some,  typep),
                           or_:                (nil, some,  typep),
                           _keyword("and"):    (t,   every, typep),
                           and_:               (t,   every, typep),
                           _keyword("member"): (nil, some,  eql),
                           member_:            (nil, some,  eql),
                           _keyword("eql"):    (nil, some,  eql),
                           eql_:               (nil, some,  eql),
                           }

##
## T/NIL-dependent stuff
##
def defvar(name, value, documentation = nil):
        "XXX: documentation, declaring as special"
        if not boundp(name):
                setq(name, value)
        return name

def defparameter(name, value, documentation = nil):
        "XXX: documentation, declaring as special"
        setq(name, value)
        return name

##
## Pretty-printing
##
def print_unreadable_object(object, stream, body, identity = None, type = None):
        write_string("#<", stream)
        if type:
                format(stream, "%s ", type_of(object).__name__)
        body()
        if identity:
                format(stream, " {%x}", id(object))
        write_string(">", stream)

__standard_pprint_dispatch__ = dict() # XXX: this is crap!
__standard_readtable__       = dict() # XXX: this is crap!

__standard_io_syntax__ = dict(_package_               = find_package("COMMON-LISP-USER"),
                              _print_array_           = t,
                              _print_base_            = 10,
                              _print_case_            = _keyword("upcase"),
                              _print_circle_          = nil,
                              _print_escape_          = t,
                              _print_gensym_          = t,
                              _print_length_          = nil,
                              _print_level_           = nil,
                              _print_lines_           = nil,
                              _print_miser_width_     = nil,
                              _print_pprint_dispatch_ = __standard_pprint_dispatch__,
                              _print_pretty_          = t,
                              _print_radix_           = nil,
                              _print_readably_        = nil,
                              _print_right_margin_    = nil,
                              _read_base_                 = 10,
                              _read_default_float_format_ = "single-float",
                              _read_eval_                 = t,
                              _read_suppress_             = nil,
                              _readtable_                 = __standard_readtable__)

def with_standard_io_syntax(body):
        """Within the dynamic extent of the BODY of forms, all reader/printer
control variables, including any implementation-defined ones not
specified by this standard, are bound to values that produce standard
READ/PRINT behavior. The values for the variables specified by this
standard are listed in the next figure.

Variable                     Value                               
*package*                    The CL-USER package                 
*print-array*                t                                   
*print-base*                 10                                  
*print-case*                 :upcase                             
*print-circle*               nil                                 
*print-escape*               t                                   
*print-gensym*               t                                   
*print-length*               nil                                 
*print-level*                nil                                 
*print-lines*                nil                                 
*print-miser-width*          nil                                 
*print-pprint-dispatch*      The standard pprint dispatch table  
*print-pretty*               nil                                 
*print-radix*                nil                                 
*print-readably*             t                                   
*print-right-margin*         nil                                 
*read-base*                  10                                  
*read-default-float-format*  single-float                        
*read-eval*                  t                                   
*read-suppress*              nil                                 
*readtable*                  The standard readtable
"""
        with progv(**__standard_io_syntax__):
                return body()

setq("_print_array_",           __standard_io_syntax__["_print_array_"])
"""Controls the format in which arrays are printed. If it is false,
the contents of arrays other than strings are never printed. Instead,
arrays are printed in a concise form using #< that gives enough
information for the user to be able to identify the array, but does
not include the entire array contents. If it is true, non-string
arrays are printed using #(...), #*, or #nA syntax."""

setq("_print_base_",            __standard_io_syntax__["_print_base_"])
"""*PRINT-BASE* and *PRINT-RADIX* control the printing of
rationals. The value of *PRINT-BASE* is called the current output
base.

The value of *PRINT-BASE* is the radix in which the printer will print
rationals. For radices above 10, letters of the alphabet are used to
represent digits above 9."""

setq("_print_case_",            __standard_io_syntax__["_print_case_"])
"""The value of *PRINT-CASE* controls the case (upper, lower, or
mixed) in which to print any uppercase characters in the names of
symbols when vertical-bar syntax is not used.

*PRINT-CASE* has an effect at all times when the value of
*PRINT-ESCAPE* is false. *PRINT-CASE* also has an effect when the
value of *PRINT-ESCAPE* is true unless inside an escape context
(i.e., unless between vertical-bars or after a slash)."""

setq("_print_circle_",          __standard_io_syntax__["_print_circle_"])
"""Controls the attempt to detect circularity and sharing in an object
being printed.

If false, the printing process merely proceeds by recursive descent
without attempting to detect circularity and sharing.

If true, the printer will endeavor to detect cycles and sharing in the
structure to be printed, and to use #n= and #n# syntax to indicate the
circularities or shared components.

If true, a user-defined PRINT-OBJECT method can print objects to the
supplied stream using WRITE, PRIN1, PRINC, or FORMAT and expect
circularities and sharing to be detected and printed using the #n#
syntax. If a user-defined PRINT-OBJECT method prints to a stream other
than the one that was supplied, then circularity detection starts over
for that stream.

Note that implementations should not use #n# notation when the Lisp
reader would automatically assure sharing without it (e.g., as happens
with interned symbols)."""

setq("_print_escape_",          __standard_io_syntax__["_print_escape_"])
"""If false, escape characters and package prefixes are not output
when an expression is printed.

If true, an attempt is made to print an expression in such a way that
it can be READ again to produce an equal expression. (This is only a
guideline; not a requirement. See *PRINT-READABLY*.)

For more specific details of how the value of *PRINT-ESCAPE* affects
the printing of certain types, see Section 22.1.3 (Default
Print-Object Methods)."""

setq("_print_gensym_",          __standard_io_syntax__["_print_gensym_"])
"""Controls whether the prefix ``#:'' is printed before apparently
uninterned symbols. The prefix is printed before such symbols if and
only if the value of *PRINT-GENSYM* is true."""

setq("_print_length_",          __standard_io_syntax__["_print_length_"])
"""*PRINT-LENGTH* controls how many elements at a given level are
printed. If it is false, there is no limit to the number of components
printed. Otherwise, it is an integer indicating the maximum number of
elements of an object to be printed. If exceeded, the printer will
print ``...'' in place of the other elements. In the case of a dotted
list, if the list contains exactly as many elements as the value of
*PRINT-LENGTH*, the terminating atom is printed rather than printing
``...''.

*PRINT-LEVEL* and *PRINT-LENGTH* affect the printing of an any object
printed with a list-like syntax. They do not affect the printing of
symbols, strings, and bit vectors."""

setq("_print_level_",           __standard_io_syntax__["_print_level_"])
"""*PRINT-LEVEL* controls how many levels deep a nested object will
print. If it is false, then no control is exercised. Otherwise, it is
an integer indicating the maximum level to be printed. An object to be
printed is at level 0; its components (as of a list or vector) are at
level 1; and so on. If an object to be recursively printed has
components and is at a level equal to or greater than the value of
*PRINT-LEVEL*, then the object is printed as ``#''.

*PRINT-LEVEL* and *PRINT-LENGTH* affect the printing of an any object
printed with a list-like syntax. They do not affect the printing of
symbols, strings, and bit vectors."""

setq("_print_lines_",           __standard_io_syntax__["_print_lines_"])
"""When the value of *PRINT-LINES* is other than NIL, it is a limit on
the number of output lines produced when something is pretty
printed. If an attempt is made to go beyond that many lines, ``..'' is
printed at the end of the last line followed by all of the suffixes
(closing delimiters) that are pending to be printed."""

setq("_print_miser_width_",     __standard_io_syntax__["_print_miser_width_"])
"""If it is not NIL, the pretty printer switches to a compact style of
output (called miser style) whenever the width available for printing
a substructure is less than or equal to this many ems."""

setq("_print_pprint_dispatch_", __standard_io_syntax__["_print_pprint_dispatch_"])
"""The PPRINT dispatch table which currently controls the pretty printer.

Initial value is implementation-dependent, but the initial entries all
use a special class of priorities that have the property that they are
less than every priority that can be specified using
SET-PPRINT-DISPATCH, so that the initial contents of any entry can be
overridden."""

setq("_print_pretty_",          __standard_io_syntax__["_print_pretty_"])
"""Controls whether the Lisp printer calls the pretty printer.

If it is false, the pretty printer is not used and a minimum of
whitespace[1] is output when printing an expression.

If it is true, the pretty printer is used, and the Lisp printer will
endeavor to insert extra whitespace[1] where appropriate to make
expressions more readable.

*PRINT-PRETTY* has an effect even when the value of *PRINT-ESCAPE* is
false."""

setq("_print_radix_",           __standard_io_syntax__["_print_radix_"])
"""*PRINT-BASE* and *PRINT-RADIX* control the printing of
rationals. The value of *PRINT-BASE* is called the current output
base.

If the value of *PRINT-RADIX* is true, the printer will print a radix
specifier to indicate the radix in which it is printing a rational
number. The radix specifier is always printed using lowercase
letters. If *PRINT-BASE* is 2, 8, or 16, then the radix specifier used
is #b, #o, or #x, respectively. For integers, base ten is indicated by
a trailing decimal point instead of a leading radix specifier; for
ratios, #10r is used."""

setq("_print_readably_",        __standard_io_syntax__["_print_readably_"])
"""If *PRINT-READABLY* is true, some special rules for printing
objects go into effect. Specifically, printing any object O1 produces
a printed representation that, when seen by the Lisp reader while the
standard readtable is in effect, will produce an object O2 that is
similar to O1. The printed representation produced might or might not
be the same as the printed representation produced when
*PRINT-READABLY* is false. If printing an object readably is not
possible, an error of type print-not-readable is signaled rather than
using a syntax (e.g., the ``#<'' syntax) that would not be readable by
the same implementation. If the value of some other printer control
variable is such that these requirements would be violated, the value
of that other variable is ignored.

Specifically, if *PRINT-READABLY* is true, printing proceeds as if
*PRINT-ESCAPE*, *PRINT-ARRAY*, and *PRINT-GENSYM* were also true, and
as if *PRINT-LENGTH*, *PRINT-LEVEL*, AND *PRINT-LINES* were false.

If *PRINT-READABLY* is false, the normal rules for printing and the
normal interpretations of other printer control variables are in
effect.

Individual methods for PRINT-OBJECT, including user-defined methods,
are responsible for implementing these requirements.

If *READ-EVAL* is false and *PRINT-READABLY* is true, any such method
that would output a reference to the ``#.'' reader macro will either
output something else or will signal an error (as described above)."""

setq("_print_right_margin_",    __standard_io_syntax__["_print_right_margin_"])
"""If it is non-NIL, it specifies the right margin (as integer number
of ems) to use when the pretty printer is making layout decisions.

If it is NIL, the right margin is taken to be the maximum line length
such that output can be displayed without wraparound or truncation. If
this cannot be determined, an implementation-dependent value is
used."""

setq("_read_base_",                 __standard_io_syntax__["_read_base_"])
"""."""

setq("_read_default_float_format_", __standard_io_syntax__["_read_default_float_format_"])
"""."""

setq("_read_eval_",                 __standard_io_syntax__["_read_eval_"])
"""."""

setq("_read_suppress_",             __standard_io_syntax__["_read_suppress_"])
"""."""

setq("_readtable_",                 __standard_io_syntax__["_readtable_"])
"""."""

def _print_symbol(s, escape = None, gensym = None, case = None, package = None, readably = None):
        # Specifically, if *PRINT-READABLY* is true, printing proceeds as if
        # *PRINT-ESCAPE*, *PRINT-ARRAY*, and *PRINT-GENSYM* were also true, and
        # as if *PRINT-LENGTH*, *PRINT-LEVEL*, AND *PRINT-LINES* were false.
        #
        # If *PRINT-READABLY* is false, the normal rules for printing and the
        # normal interpretations of other printer control variables are in
        # effect.
        #
        # Individual methods for PRINT-OBJECT, including user-defined methods,
        # are responsible for implementing these requirements.
        package  = _defaulted_to_var(package,  "_package_")
        if not packagep(package):
                _here("------------------------------------------------------------\npackage is a %s: %s" % (type_of(package), package,))
        readably = _defaulted_to_var(readably, "_print_readably_")
        escape   = _defaulted_to_var(escape,   "_print_escape_") if not readably else t
        case     = _defaulted_to_var(case,     "_print_case_")   if not readably else t
        gensym   = _defaulted_to_var(gensym,   "_print_gensym_") if not readably else t
        # Because the #: syntax does not intern the following symbol, it is
        # necessary to use circular-list syntax if *PRINT-CIRCLE* is true and
        # the same uninterned symbol appears several times in an expression to
        # be printed. For example, the result of
        #
        # (let ((x (make-symbol "FOO"))) (list x x))
        #
        # would be printed as (#:FOO #:FOO) if *PRINT-CIRCLE* were
        # false, but as (#1=#:FOO #1#) if *PRINT-CIRCLE* were true.
        return ((""                       if not escape                        else
                 ":"                      if s.package is __keyword_package__  else
                 ""                       if _symbol_accessible_in(s, package) else
                 ("#:" if gensym else "") if not s.package                     else
                 (s.package.name + (":"
                                    if s in s.package.external else
                                    "::"))) +
                _case_xform(case, s.name))

def _print_string(x, escape = None, readably = None):
        """The characters of the string are output in order. If printer escaping
is enabled, a double-quote is output before and after, and all
double-quotes and single escapes are preceded by backslash. The
printing of strings is not affected by *PRINT-ARRAY*. Only the active
elements of the string are printed."""
        # XXX: "active elements of the string"
        # Issue ADJUSTABLE-CHARACTER-VECTORS-NOT-IMPLEMENTED
        readably = _defaulted_to_var(readably, "_print_readably_")
        escape   = _defaulted_to_var(escape,   "_print_escape_") if not readably else t
        return (x if not escape else
                ("\"" + _without_condition_system(
                                lambda: re.sub(r"([\"\\])", r"\\\1", x)) +
                 "\""))

def _print_function(x):
        return with_output_to_string(
                lambda s: print_unreadable_object(
                        x, s,
                        lambda: format(s, "%s (%s)", x.__name__, _print_function_arglist(x)),
                        identity = t, type = t))

def _print_unreadable_compound(x):
        return with_output_to_string(
                lambda s: print_unreadable_object(
                        x, s,
                        lambda: format(s, "%d elements", len(x)),
                        identity = t, type = t))

def _print_unreadable(x):
        return with_output_to_string(
                lambda stream: print_unreadable_object(
                        x, stream,
                        lambda: nil,
                        identity = t, type = t))

def write_to_string(object,
                    array = None,
                    base = None,
                    case = None,
                    circle = None,
                    escape = None,
                    gensym = None,
                    length = None,
                    level = None,
                    lines = None,
                    miser_width = None,
                    pprint_dispatch = None,
                    pretty = None,
                    radix = None,
                    readably = None,
                    right_margin = None):
        "XXX: does not conform!"
        array           = _defaulted_to_var(array,           "_print_array_")
        base            = _defaulted_to_var(base,            "_print_base_")
        case            = _defaulted_to_var(case,            "_print_case_")
        circle          = _defaulted_to_var(circle,          "_print_circle_")
        escape          = _defaulted_to_var(escape,          "_print_escape_")
        gensym          = _defaulted_to_var(gensym,          "_print_gensym_")
        length          = _defaulted_to_var(length,          "_print_length_")
        level           = _defaulted_to_var(level,           "_print_level_")
        lines           = _defaulted_to_var(lines,           "_print_lines_")
        miser_width     = _defaulted_to_var(miser_width,     "_print_miser_width_")
        pprint_dispatch = _defaulted_to_var(pprint_dispatch, "_print_pprint_dispatch_")
        pretty          = _defaulted_to_var(pretty,          "_print_pretty_")
        radix           = _defaulted_to_var(radix,           "_print_radix_")
        readably        = _defaulted_to_var(readably,        "_print_readably_")
        right_margin    = _defaulted_to_var(right_margin,    "_print_right_margin_")
        # assert(True
        #        and array is t
        #        and base is 10
        #        # case is keyword("upcase")
        #        and circle is nil
        #        # and escape is t !
        #        # and gensym is t
        #        and length is nil
        #        and level is nil
        #        and lines is nil
        #        and miser_width is nil
        #        and pretty is nil
        #        and pprint_dispatch is __standard_pprint_dispatch__
        #        and radix is nil
        #        # and readably is nil !
        #        # and right_margin is nil !
        #        )
        obj2lisp_xform = {
                False : "nil",
                None  : "nil",
                True  : "t",
        }
        def do_write_to_string(object):
                string = ""
                def write_to_string_loop(object):
                        nonlocal string
                        if listp(object) or _tuplep(object):
                                string += "("
                                max = len(object)
                                if max:
                                        for i in range(0, max):
                                                string += do_write_to_string(object[i])
                                                if i != (max - 1):
                                                        string += " "
                                string += ")"
                        elif symbolp(object):
                                # Honors *PACKAGE*, *PRINT-CASE*, *PRINT-ESCAPE*, *PRINT-GENSYM*, *PRINT-READABLY*.
                                # XXX: in particular, *PRINT-ESCAPE* is honored only partially.
                                string += _print_symbol(object)
                        elif integerp(object) or floatp(object):
                                string += str(object)
                        elif object.__hash__ and object in obj2lisp_xform:
                                string += obj2lisp_xform[object]
                        elif type(object).__name__ == "builtin_function_or_method":
                                string += "\"#<BUILTIN-FUNCTION-OR-METHOD %s 0x%x>\"" % (object.__name__, id(object))
                        elif stringp(object):
                                # Honors *PRINT-ESCAPE* and *PRINT-READABLY*.
                                string += _print_string(object)
                        elif hash_table_p(object) or _setp(object):
                                # Honors *PRINT-ESCAPE* and *PRINT-READABLY*.
                                string += _print_unreadable_compound(object)
                        elif functionp(object):
                                string += _print_function(object)
                        elif (not escape) and typep(object, [or_, restart, condition]):
                                string += str(object)
                        else:
                                string += _print_unreadable(object)
                                # error("Can't write object %s", object)
                        return string
                return write_to_string_loop(object)
        ret = do_write_to_string(object)
        # debug_printf("===> %s", ret)
        return ret

def prin1_to_string(object): return write_to_string(object, escape = t)
def princ_to_string(object): return write_to_string(object, escape = nil, readably = nil)

def write(object, stream = t, **args):
        """WRITE is the general entry point to the Lisp printer. For each
explicitly supplied keyword parameter named in the next figure, the
corresponding printer control variable is dynamically bound to its
value while printing goes on; for each keyword parameter in the next
figure that is not explicitly supplied, the value of the corresponding
printer control variable is the same as it was at the time write was
invoked. Once the appropriate bindings are established, the object is
output by the Lisp printer."""
        write_string(write_to_string(object, **args), stream)
        return object

def prin1(object, stream = t):
        """PRIN1 produces output suitable for input to READ. It binds *PRINT-ESCAPE* to true."""
        return write(object, stream = stream, escape = t)

def princ(object, stream = t):
        """PRINC is just like PRIN1 except that the output has no escape characters.
It binds *PRINT-ESCAPE* to false and *PRINT-READABLY* to false.
The general rule is that output from PRINC is intended to look good to people, 
while output from PRIN1 is intended to be acceptable to READ."""
        return write(object, stream = stream, escape = nil, readably = nil)

def print_(object, stream = t):
        """PRINT is just like PRIN1 except that the printed representation of object
is preceded by a newline and followed by a space."""
        terpri(stream)
        prin1(object, stream)
        write_char(" ", stream)
        return object

def pprint(object, stream = t):
        """PPRINT is just like PRINT except that the trailing space is omitted
and object is printed with the *PRINT-PRETTY* flag non-NIL to produce pretty output."""
        terpri(stream)
        write(object, stream = stream, escape = t, pretty = t)
        return object

def format(destination, control_string, *args):
        """FORMAT produces formatted output by outputting the characters
of CONTROL-STRING and observing that a tilde introduces a
directive. The character after the tilde, possibly preceded by prefix
parameters and modifiers, specifies what kind of formatting is
desired. Most directives use one or more elements of ARGS to create
their output.

If DESTINATION is a string, a stream, or T, then the result is
nil. Otherwise, the result is a string containing the `output.'

FORMAT is useful for producing nicely formatted text, producing
good-looking messages, and so on. FORMAT can generate and return a
string or output to destination.

For details on how the CONTROL-STRING is interpreted, see Section 22.3
(Formatted Output)."""
        string = control_string % args
        if  streamp(destination) or listp(destination) or destination is t:
                # XXX: python strings are immutable, so lists will serve as adjustable arrays..
                # Issue ADJUSTABLE-CHARACTER-VECTORS-NOT-IMPLEMENTED
                write_string(string, destination)
                return nil
        else:
                return string

##
## Reader
##
setq("_read_case_", _keyword("upcase"))

def parse_integer(xs, junk_allowed = nil, radix = 10):
        l = len(xs)
        def hexcharp(x): return x.isdigit() or x in ["a", "b", "c", "d", "e", "f"]
        (test, xform) = ((str.isdigit, identity)      if radix == 10 else
                         (hexcharp,    float.fromhex) if radix == 16 else
                         _not_implemented("PARSE-INTEGER only implemented for radices 10 and 16."))
        for end in range(0, l):
                if not test(xs[end]):
                        if junk_allowed:
                                end -= 1
                                break
                        else:
                                error("Junk in string \"%s\".", xs)
        return int(xform(xs[:(end + 1)]))

def _read_symbol(x, package = None, case = _keyword("upcase")):
        # debug_printf("_read_symbol >%s<, x[0]: >%s<", x, x[0])
        name, p = ((x[1:], __keyword_package__)
                   if x[0] == ":" else
                   let(x.find(":"),
                       lambda index:
                               (_if_let(find_package(x[0:index].upper()),
                                        lambda p:
                                                (x[index + 1:], p),
                                        lambda:
                                                error("Package \"%s\" doesn't exist, while reading symbol \"%s\".",
                                                      x[0:index].upper(), x))
                                if index != -1 else
                                (x, _coerce_to_package(package)))))
        return _intern0(_case_xform(case, name), p)

end_of_file = EOFError

@block
def read_from_string(string, eof_error_p = True, eof_value = nil,
                     start = 0, end = None, preserve_whitespace = None):
        "Does not conform."
        # _here("from \"%s\"" % string)
        pos, end = start, (end or len(string))
        def handle_short_read_if(test):
                # _here("< %s" % (test,))
                if test:
                        (error(end_of_file, "end of file on %s" % (make_string_input_stream(string),)) if eof_error_p else
                         return_from(read_from_string, eof_value))
        def read():
                skip_whitespace()
                char = string[pos]
                # _here("> \"%s\", by \"%s\"" % (string[pos:], char))
                if   char == "(":  obj = read_list()
                elif char == "\"": obj = read_string()
                elif char == "'":  obj = read_quote()
                else:
                        handle_short_read_if(pos > end)
                        obj = read_number_or_symbol()
                        if obj == _find_symbol0("."):
                                error("Consing dot not implemented")
                # _here("< %s" % (obj,))
                return obj
        def skip_whitespace():
                nonlocal pos
                while string[pos] in frozenset([" ", "\t", "\n"]):
                        pos += 1
        def read_list():
                nonlocal pos
                ret = []
                pos += 1
                while True:
                        skip_whitespace()
                        char = string[pos]
                        if char == ")":
                                pos += 1
                                break
                        else:
                                obj = read()
                                if not listp(obj) and obj is _find_symbol0("."):
                                        error("Consing dot not implemented")
                                ret += [obj]
                # _here("< %s" % (ret,))
                return ret
        def read_string():
                nonlocal pos
                ret = ""
                def add_char(c):
                        nonlocal ret
                        ret += c
                while True:
                        pos += 1
                        char = string[pos]
                        if char == "\"":
                                pos += 1
                                break
                        elif char == "\\":
                                pos += 1
                                char2 = string[pos]
                                if   char2 == "\"": add_char(char2)
                                elif char2 == "\\": add_char(char2)
                                else:
                                        error("READ-FROM-STRING: unrecognized escape character \"%s\".", char2)
                        else:
                                add_char(char)
                # _here("< %s" % (ret,))
                return ret
        def read_number_or_symbol():
                token = read_token()
                handle_short_read_if(not token)
                if _without_condition_system(lambda: re.match("^[0-9]+$", token)):
                        ret = int(token)
                elif _without_condition_system(lambda: re.match("^[0-9]+\\.[0-9]+$", token)):
                        ret = float(token)
                else:
                        ret = _read_symbol(token)
                        # debug_printf("-- interned %s as %s", token, name)
                        # if name is t:
                        #         ret = True
                        # elif name is nil:
                        #         ret = False
                        # else:
                        #         ret = name
                # _here("< %s" % ret)
                return ret
        def read_token():
                nonlocal pos
                token = ""
                # _here(">> ..%s..%s" % (pos, end))
                while True:
                        if pos >= end:
                                break
                        char = string[pos]
                        if char in set([" ", "\t", "\n", "(", ")", "\"", "'"]):
                                break
                        else:
                                token += char
                                pos += 1
                # _here("< %s" % token)
                return token
        ret = handler_case(read,
                           (IndexError,
                            lambda c: handle_short_read_if(True)))
        # _here("lastly %s" % (ret,))
        return ret

def read_char(stream = sys.stdin, eof_error_p = True, eof_value = nil, recursivep = nil):
        ret = the(_io._IOBase, stream).read(1)
        return (ret       if ret             else
                eof_value if not eof_error_p else
                error(end_of_file, "end of file on %s" % (stream,)))

def unread_char(x, stream = sys.stdin):
        "XXX: conformance"
        # I've found out I don't really undestand how UNREAD-CHAR is supposed to work..
        posn = file_position(stream)
        if posn == 0:
                error("Nothing to unread.")
        else:
                stream.seek(posn - 1)

@block
def read(stream = sys.stdin, eof_error_p = True, eof_value = nil, preserve_whitespace = None, recursivep = nil):
        "Does not conform."
        def read_inner():
                skip_whitespace()
                char = read_char(stream)
                unread_char(char, stream)
                # _here("> \"%s\", by \"%s\"" % (string[pos:], char))
                if   char == "(":  obj = read_list()
                elif char == "\"": obj = read_string()
                elif char == "'":  obj = read_quote()
                else:
                        # handle_short_read_if(pos > end)
                        obj = read_number_or_symbol()
                        if obj == _find_symbol0("."):
                                error("Consing dot not implemented")
                # _here("< %s" % (obj,))
                return obj
        def skip_whitespace():
                while True:
                        c = read_char(stream, nil, nil)
                        if c not in frozenset([" ", "\t", "\n"]):
                                if c is not nil:
                                        unread_char(c, stream)
                                return
        def read_list():
                ret = []
                c = read_char(stream) # it's a #\(
                while True:
                        skip_whitespace()
                        char = read_char(stream)
                        if char == ")":
                                break
                        else:
                                unread_char(char, stream)
                                obj = read_inner()
                                if not listp(obj) and obj is _find_symbol0("."):
                                        error("Consing dot not implemented")
                                ret += [obj]
                # _here("< %s" % (ret,))
                return ret
        def read_string():
                ret = ""
                read_char(stream) # seek the opening double-quote
                while True:
                        char = read_char(stream)
                        if char == "\"":
                                break
                        elif char == "\\":
                                char2 = read_char(stream)
                                ret += (char2 if char2 in set(["\"", "\\"]) else
                                        error("READ-FROM-STRING: unrecognized escape character \"%s\".", char2))
                        else:
                                ret += char
                # _here("< %s" % (ret,))
                return ret
        def read_number_or_symbol():
                token = read_token()
                if _without_condition_system(lambda: re.match("^[0-9]+$", token)):
                        ret = int(token)
                elif _without_condition_system(lambda: re.match("^[0-9]+\\.[0-9]+$", token)):
                        ret = float(token)
                else:
                        ret = _read_symbol(token)
                        # debug_printf("-- interned %s as %s", token, name)
                # _here("< %s" % ret)
                return ret
        def read_token():
                token = ""
                # _here(">> ..%s..%s" % (pos, end))
                while True:
                        char = read_char(stream, nil, nil)
                        if char in set([nil, " ", "\t", "\n", "(", ")", "\"", "'"]):
                                if char is not nil:
                                        unread_char(char, stream)
                                break
                        else:
                                token += char
                # _here("< %s" % token)
                return token
        ret = handler_case(read_inner,
                           (end_of_file,
                            lambda c: error(c) if eof_error_p else
                                      return_from(read, eof_value)))
        # _here("lastly %s" % (ret,))
        return ret

##
## Files
##
def probe_file(pathname):
        "No, no real pathnames, just namestrings.."
        assert(stringp(pathname))
        return _without_condition_system(
                lambda: os.path.exists(pathname))

def namestring(pathname):
        return pathname

def truename(pathname):
        "XXX: does not conform."
        return pathname

def file_write_date(pathspec):
        """Returns a universal time representing the time
at which the file specified by PATHSPEC was last written
(or created), or returns NIL if such a time cannot be determined. """
        # XXX: doesn't conform terribly well:
        # 1. NIL isn't returned if the time cannot be determined: python will,
        #    in most likelihood, raise an error.
        # 2. (from CLHS) Exceptional Situations:
        # An error of type FILE-ERROR is signaled if pathspec is wild.
        # An error of type FILE-ERROR is signaled if the file system
        # cannot perform the requested operation.
        #
        # Issue UNIVERSAL-TIME-COARSE-GRANULARITY
        # os.path.getmtime() returns microseconds..
        _here("filename: %s", pathspec)
        return int(os.path.getmtime(pathspec))

##
## Streams
##
def open_stream_p(x):
        return not the(stream, x).closed

def input_stream_p(x):
        return open_stream_p(x) and x.readable()

def output_stream_p(x):
        return open_stream_p(x) and x.writable()

class two_way_stream(stream):
        def __init__(self, input, output):
                self.input, self.output  = input, output
        def read(self, amount):
                return self.input.read(amount)
        def write(self, data):
                return self.output.write(data)
        def flush(self):
                return self.output.flush()
        def close(self):
                self.output.close()
                self.input.close()
        def readable(self): return True
        def writable(self): return True

def make_two_way_stream(input, output):   return two_way_stream(input, output)
def two_way_stream_input_stream(stream):  return stream.input
def two_way_stream_output_stream(stream): return stream.output

setq("_standard_input_",  sys.stdin)
setq("_standard_output_", sys.stdout)
setq("_error_output_",    sys.stderr)
setq("_debug_io_",        make_two_way_stream(symbol_value("_standard_input_"), symbol_value("_standard_output_")))
setq("_query_io_",        make_two_way_stream(symbol_value("_standard_input_"), symbol_value("_standard_output_")))

class broadcast_stream(stream):
        def __init__(self, *streams):
                self.streams  = streams
        def write(self, data):
                for component in self.streams:
                        component.write(data)
        def flush(self):
                for component in self.streams:
                        component.flush()
        def readable(self): return False
        def writable(self): return True

def make_broadcast_stream(*streams):  return broadcast_stream(*streams)
def broadcast_stream_streams(stream): return stream.streams

class synonym_stream(stream):
        def __init__(self, symbol):
                self.symbol  = symbol
        def stream():
                return symbol_value(self.symbol)
        def read(self, amount):
                return stream().read(amount)
        def write(self, data):
                return stream().write(data)
        def flush(self):
                return stream().flush()
        def readable(self): return stream.readable()
        def writable(self): return stream.writable()

def make_synonym_stream(symbol):   return synonym_stream(symbol)
def synonym_stream_symbol(stream): return stream.symbol

def streamp(x):
        return typep(x, stream)

def stream_external_format(stream):
        return _keyword(stream.encoding)

def _coerce_to_stream(x):
        return (x                                 if streamp(x) else
                symbol_value("_standard_output_") if x is t else
                error("%s cannot be coerced to a stream.", x))

class stream_type_error(simple_condition, io.UnsupportedOperation):
        pass

def write_char(c, stream = t):
        write_string(c, stream)
        return c

def terpri(stream = t):
        write_string("\n", stream)

def write_string(string, stream = t):
        if stream is not nil:
                def handler():
                        try:
                                return _write_string(string, _coerce_to_stream(stream))
                        except io.UnsupportedOperation as cond:
                                error(stream_type_error, "%s is not an %s stream: \"%s\".",
                                      stream, ("output" if cond.args[0] == "not writable" else
                                               "adequate"),
                                      cond.args[0])
                _without_condition_system(handler)
        return string

def write_line(string, stream = t):
        return write_string(string + "\n", stream)

def make_string_output_stream():
        return io.StringIO()

def get_output_stream_string(x):
        return x.getvalue()

def make_string_input_stream(x):
        return io.StringIO(x)

def close(x):
        x.close()

def file_position(x):
        return x.seek(0, 1)

def finish_output(stream = t):
        stream is not nil and _coerce_to_stream(stream).flush()

def force_output(*args, **keys):
        finish_output(*args, **keys)

##
## Pythonese execution tracing: for HANDLER-BIND.
##
__tracer_hooks__   = dict() # allowed keys: "call", "line", "return", "exception", "c_call", "c_return", "c_exception"
def _set_tracer_hook(type, fn):        __tracer_hooks__[type] = fn
def     _tracer_hook(type):     return __tracer_hooks__.get(type) if type in __tracer_hooks__ else None

def _pytracer(frame, event, arg):
        method = _tracer_hook(event)
        if method:
                method(arg, frame)
        return _pytracer

def _pytracer_enabled_p(): return sys.gettrace() is _pytracer
def _enable_pytracer():    sys.settrace(_pytracer); return True
def _disable_pytracer():   sys.settrace(None);      return True

def _set_condition_handler(fn):
        _set_tracer_hook("exception", fn)
        return True

##
## Condition system
##
setq("__handler_clusters__", [])

def make_condition(datum, *args, default_type = error_, **keys):
        """
It's a slightly weird interpretation of MAKE-CONDITION, as the latter
only accepts symbols as DATUM, while this one doesn't accept symbols
at all.
"""
        # format(t, "stringp: %s\nclassp: %s\nBaseException-p: %s\n",
        #        stringp(datum),
        #        typep(datum, type_of(condition)),
        #        typep(datum, condition))
        cond = (default_type(datum % args) if stringp(datum) else
                datum(*args, **keys)       if typep(datum, type_of(condition)) else
                datum                      if typep(datum, condition) else
                error(simple_type_error, "The first argument to MAKE-CONDITION must either a string, a condition type or a condition, was: %s, of type %s.",
                      datum, type_of(datum)))
        # format(t, "made %s %s %s\n", datum, args, keys)
        # format(t, "    %s\n", cond)
        return cond

setq("_presignal_hook_", nil)
setq("_prehandler_hook_", nil)
setq("_debugger_hook_",  nil)

def signal(cond):
        # _here("Signalling: %s", cond)
        for cluster in reversed(env.__handler_clusters__):
                # format(t, "Analysing cluster %s for %s.\n", cluster, type_of(cond))
                for type, handler in cluster:
                        if not stringp(type):
                                # _here("Trying: %s -> %s", type, typep(cond, type))
                                if typep(cond, type):
                                        hook = symbol_value("_prehandler_hook_")
                                        if hook:
                                                frame = assoc("__frame__", cluster)
                                                assert(frame)
                                                hook(cond, frame, hook)
                                        handler(cond)
                                        # _here("...continuing handling of %s, refugees: |%s|",
                                        #       cond,
                                        #       _pp_frame_chain(
                                        #                 reversed(_frames_upward_from(
                                        #                                 assoc("__frame__", cluster),
                                        #                                 15))))
        return nil

def error(datum, *args, **keys):
        "With all said and done, this ought to jump right into __CL_CONDITION_HANDLER__."
        raise make_condition(datum, *args, **keys)

def warn(datum, *args, **keys):
        cond = make_condition(datum, *args, default_type = simple_warning, **keys)
        signal(cond)
        format(symbol_value("_error_output_"), "%s", cond)
        return nil

def invoke_debugger(cond):
        "XXX: non-compliant: doesn't actually invoke the debugger."
        debugger_hook = symbol_value("_debugger_hook_")
        if debugger_hook:
                with env.let(_debugger_hook_ = nil):
                        debugger_hook(cond, debugger_hook)
        error(BaseError, "INVOKE-DEBUGGER fell through.")

__main_thread__ = threading.current_thread()
def _report_condition(cond, stream = None, backtrace = None):
        stream = _defaulted_to_var(stream, "_debug_io_")
        format(stream, "%sondition of type %s: %s\n",
               (("In thread \"%s\": c" % threading.current_thread().name)
                if threading.current_thread() is not __main_thread__ else 
                "C"),
               type(cond), cond)
        if backtrace:
                _backtrace(-1, stream)
        return t

def _maybe_reporting_conditions_on_hook(p, hook, body, backtrace = None):
        if p:
                old_hook_value = symbol_value(hook)
                def wrapped_hook(cond, hook_value):
                        "Let's honor the old hook."
                        _report_condition(cond,
                                          stream = symbol_value("_debug_io_"),
                                          backtrace = backtrace)
                        if old_hook_value:
                                old_hook_value(cond, old_hook_value)
                with env.maybe_let(p, **{_coerce_to_symbol_name(hook): wrapped_hook}):
                        return body()
        else:
                return body()

def _dump_thread_state():
        def body():
                import ctypes
                from binascii import hexlify
                from ctypes import c_uint, c_char, c_ulong, POINTER, cast, pythonapi
                def dump(obj):
                        for i, x in enumerate(hexlify(memoryview(obj)).decode()):
                                print(x, end='')
                                if i and not (i + 1)%8:
                                        print(" ", end='')
                                if i and not (i + 1)%32:
                                        print("")
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

                print("o: %s, id: {%x}" % (o, id(o)))
                print(dump(o))
                for slot, _ in type(o)._fields_:
                        val = getattr(o, slot)
                        type_ = type(val)
                        print(("%25s: " + ("%x" if type_ is int else "%s")) % (slot, val))
        _without_condition_system(body)

__not_even_conditions__ = frozenset([SystemExit, __catcher_throw__])
"A set of condition types which are entirely ignored by the condition system."

def __cl_condition_handler__(condspec, frame):
        def continuation():
                type, raw_cond, traceback = condspec
                # print_frames(frames_upward_from(frame))
                if type_of(raw_cond) not in __not_even_conditions__:
                        def _maybe_upgrade_condition(cond):
                                "Fix up the shit routinely being passed around."
                                return (cond if typep(cond, condition) else
                                        condspec[0](*([cond] if stringp(cond) else
                                                      cond)))
                                       # typecase(cond,
                                       #          (BaseException, lambda: cond),
                                       #          (str,       lambda: error_(cond)))
                        cond = _maybe_upgrade_condition(raw_cond)
                        if cond is not raw_cond:
                                _here("Condition Upgrader: %s(%s) -> %s(%s)",
                                      prin1_to_string(raw_cond), type_of(raw_cond),
                                      prin1_to_string(cond), type_of(cond),
                                      callers = 15)
                        with env.let(_traceback_ = traceback,
                                     _signalling_frame_ = frame): # These bindings are the deviation from the CL standard.
                                presignal_hook = symbol_value("_presignal_hook_")
                                if presignal_hook:
                                        with env.let(_presignal_hook_ = nil):
                                                presignal_hook(cond, presignal_hook)
                                signal(cond)
                                debugger_hook = symbol_value("_debugger_hook_")
                                if debugger_hook:
                                        with env.let(_debugger_hook_ = nil):
                                                debugger_hook(cond, debugger_hook)
                        return cond
                else:
                        return raw_cond
        with progv(_stack_top_hint_ = _caller_frame(caller_relative = 1)):
                cond = sys.call_tracing(continuation, tuple())
        if type_of(cond) not in __not_even_conditions__:
                _here("In thread '%s': unhandled condition : %s",
                      threading.current_thread().name, prin1_to_string(cond),
                      callers = 15)
        # At this point, the Python condition handler kicks in,
        # and the stack gets unwound for the first time.
        #
        # ..too bad, we've already called all HANDLER-BIND-bound
        # condition handlers.
        # If we've hit any HANDLER-CASE-bound handlers, then we won't
        # even reach this point, as the stack is already unwound.
_set_condition_handler(__cl_condition_handler__)

def handler_bind(fn, *handlers, no_error = identity):
        "Works like real HANDLER-BIND, when the conditions are right.  Ha."
        value = None

        # this is:
        #     pytracer_enabled_p() and condition_handler_active_p()
        # ..inlined for speed.
        if _pytracer_enabled_p() and "exception" in __tracer_hooks__ and __tracer_hooks__["exception"] is __cl_condition_handler__:
                for type, _ in handlers:
                        if not (typep(type, type_) and subtypep(type, condition)):
                                error(simple_type_error, "While establishing handler: '%s' does not designate a known condition type.", type)
                with env.let(__handler_clusters__ = env.__handler_clusters__ +
                             [handlers + (("__frame__", _this_frame()),)]):
                        return no_error(fn())
        else:
                # old world case..
                # format(t, "crap FAIL: pep %s, exhook is cch: %s",
                #        _pytracer_enabled_p(), __tracer_hooks__.get("exception") is __cl_condition_handler__)
                if len(handlers) > 1:
                        error("HANDLER-BIND: was asked to establish %d handlers, but cannot establish more than one in 'dumb' mode.",
                              len(handlers))
                condition_type_name, handler = handlers.popitem()
                try:
                        value = fn()
                except find_class(condition_type_name) as cond:
                        return handler(cond)
                finally:
                        return no_error(value)

def handler_case(body, *handlers, no_error = identity):
        "Works like real HANDLER-CASE, when the conditions are right.  Ha."
        nonce            = gensym("HANDLER-CASE")
        wrapped_handlers = _mapcar_star(lambda type, handler:
                                                (type, lambda cond: return_from(nonce, handler(cond))),
                                        handlers)
        return catch(nonce,
                     lambda: handler_bind(body, *wrapped_handlers, no_error = no_error))

def ignore_errors(body):
        return handler_case(body,
                            (Exception,
                             lambda _: None))

##
## Restarts
##
class restart(_servile):
        def __str__(self):
                # XXX: must conform by honoring *PRINT-ESCAPE*:
                # http://www.lispworks.com/documentation/lw51/CLHS/Body/m_rst_ca.htm#restart-case
                return (with_output_to_string(lambda stream: self.report_function(stream)) if self.report_function else
                        self.__repr__())
        pass
# RESTART-BIND executes the body of forms in a dynamic environment where
# restarts with the given names are in effect.

# If a name is nil, it indicates an anonymous restart; if a name is a
# non-NIL symbol, it indicates a named restart.

# The function, interactive-function, and report-function are
# unconditionally evaluated in the current lexical and dynamic
# environment prior to evaluation of the body. Each of these forms must
# evaluate to a function.

# If INVOKE-RESTART is done on that restart, the function which resulted
# from evaluating function is called, in the dynamic environment of the
# INVOKE-RESTART, with the arguments given to INVOKE-RESTART. The
# function may either perform a non-local transfer of control or may
# return normally.


# If the restart is invoked interactively from the debugger (using
# invoke-restart-interactively), the arguments are defaulted by calling
# the function which resulted from evaluating interactive-function. That
# function may optionally prompt interactively on query I/O, and should
# return a list of arguments to be used by invoke-restart-interactively
# when invoking the restart.

# If a restart is invoked interactively but no interactive-function is
# used, then an argument list of nil is used. In that case, the function
# must be compatible with an empty argument list.

# If the restart is presented interactively (e.g., by the debugger), the
# presentation is done by calling the function which resulted from
# evaluating report-function. This function must be a function of one
# argument, a stream. It is expected to print a description of the
# action that the restart takes to that stream. This function is called
# any time the restart is printed while *print-escape* is nil.

# restart_bind(body,
#              name = ((lambda *args: 1),
#                      dict(interactive_function = lambda: compute_invoke_restart_interactively_args(),
#                           report_function      = lambda stream: print_restart_summary(stream),
#                           test_function        = lambda cond: visible_p(cond))))
setq("__restart_clusters__", [])

def _restartp(x):
        return typep(x, restart)

def restart_name(x):
        return x.name

def _specs_restarts_args(restart_specs):
        # format (t, "_s_r: %s", restart_specs)
        restarts_args = dict()
        for name, spec in restart_specs.items():
                function, options = ((spec[0], spec[1]) if _tuplep(spec) else
                                     (spec, dict()))
                restarts_args[name.upper()] = _updated_dict(options, dict(function = function)) # XXX: name mangling!
        return restarts_args

##
# XXX: :TEST-FUNCTION is currently IGNORED!
##
def _restart_bind(body, restarts_args):
        with env.let(__restart_clusters__ = env.__restart_clusters__ + [_remap_hash_table(lambda _, restart_args: restart(**restart_args), restarts_args)]):
                return body()

def restart_bind(body, **restart_specs):
        return _restart_bind(body, _specs_restarts_args(restart_specs))

__valid_restart_options__ = frozenset(["interactive", "report", "test", "function"])
def _restart_case(body, **restarts_args):
        def validate_restart_options(options):
                unknown = set(options.keys()) - __valid_restart_options__
                return t if not unknown else error(simple_type_error, "Acceptable restart options are: (%s), not (%s)",
                                                   " ".join(__valid_restart_options__), " ".join(options.keys()))
        nonce = gensym("RESTART-CASE")
        wrapped_restarts_args = {
                restart_name: let(restart_args["function"],
                                  restart_args["interactive"] if "interactive" in restart_args else nil,
                                  restart_args["report"]      if "report"      in restart_args else nil,
                                  lambda function, interactive, report:
                                          (validate_restart_options(restart_args) and
                                           _updated_dict(restart_args,
                                                         dict(name                 = restart_name,
                                                              function             =
                                                              lambda *args, **keys:
                                                                      return_from(nonce, function(*args, **keys)),
                                                              interactive_function =
                                                              (interactive                  if functionp(interactive) else
                                                               lambda: []                   if null(interactive) else
                                                               error(":INTERACTIVE argument to RESTART-CASE must be either a function or NIL.")),
                                                              report_function      =
                                                              (report                       if functionp(report) else
                                                               _curry(write_string, report) if stringp(report) else
                                                               nil                          if null(report) else
                                                               error(":REPORT argument to RESTART-CASE must be either a function, a string or NIL."))))))
                for restart_name, restart_args in restarts_args.items () }
        return catch(nonce,
                     lambda: _restart_bind(body, wrapped_restarts_args))

def restart_case(body, **restart_specs):
        return _restart_case(body, **_specs_restarts_args(restart_specs))

def with_simple_restart(name, format_control_and_arguments, body):
        """
WITH-SIMPLE-RESTART establishes a restart.

If the restart designated by NAME is not invoked while executing
FORMS, all values returned by the last of FORMS are returned. If the
restart designated by NAME is invoked, control is transferred to
WITH-SIMPLE-RESTART, which returns two values, NIL and T.

If name is NIL, an anonymous restart is established.

The FORMAT-CONTROL and FORMAT-ARGUMENTS are used report the restart.
"""
        description = (format_control_and_arguments if stringp(format_control_and_arguments) else
                       format(nil, format_control_and_arguments[0], *format_control_and_arguments[1:]))
        return restart_case(body, **{ name: (lambda: None,
                                             dict(report = lambda stream: format(stream, "%s", description))) })

def restart_condition_association_check(cond, restart):
        """
When CONDITION is non-NIL, only those restarts are considered that are
either explicitly associated with that condition, or not associated
with any condition; that is, the excluded restarts are those that are
associated with a non-empty set of conditions of which the given
condition is not an element. If condition is NIL, all restarts are
considered.
"""
        return (not cond or
                "associated_conditions" not in restart.__dict__ or
                cond in restart.associated_conditions)

def find_restart(identifier, condition = None):
        """
FIND-RESTART searches for a particular restart in the current dynamic
environment.

When CONDITION is non-NIL, only those restarts are considered that are
either explicitly associated with that condition, or not associated
with any condition; that is, the excluded restarts are those that are
associated with a non-empty set of conditions of which the given
condition is not an element. If condition is NIL, all restarts are
considered.

If IDENTIFIER is a symbol, then the innermost (most recently
established) applicable restart with that name is returned. nil is
returned if no such restart is found.

If IDENTIFIER is a currently active restart, then it is
returned. Otherwise, NIL is returned.
"""
        if _restartp(identifier):
                return find_restart(restart_name(identifier)) is identifier
        else:
                for cluster in reversed(env.__restart_clusters__):
                        # format(t, "Analysing cluster %s for \"%s\".", cluster, name)
                        restart = cluster[identifier] if identifier in cluster else None
                        if restart and restart_condition_association_check(condition, restart):
                                return restart

def compute_restarts(condition = None):
        """
COMPUTE-RESTARTS uses the dynamic state of the program to compute a
list of the restarts which are currently active.

The resulting list is ordered so that the innermost (more-recently
established) restarts are nearer the head of the list.

When CONDITION is non-NIL, only those restarts are considered that are
either explicitly associated with that condition, or not associated
with any condition; that is, the excluded restarts are those that are
associated with a non-empty set of conditions of which the given
condition is not an element. If condition is NIL, all restarts are
considered.

COMPUTE-RESTARTS returns all applicable restarts, including anonymous
ones, even if some of them have the same name as others and would
therefore not be found by FIND-RESTART when given a symbol argument.

Implementations are permitted, but not required, to return distinct
lists from repeated calls to COMPUTE-RESTARTS while in the same
dynamic environment. The consequences are undefined if the list
returned by COMPUTE-RESTARTS is every modified.
"""
        restarts = list()
        for cluster in reversed(env.__restart_clusters__):
                # format(t, "Analysing cluster %s for \"%s\".", cluster, name)
                restarts.extend(remove_if_not(_curry(restart_condition_association_check, condition), cluster.values())
                                if condition else
                                cluster.values())
        return restarts

def invoke_restart(restart, *args, **keys):
        """
Calls the function associated with RESTART, passing arguments to
it. Restart must be valid in the current dynamic environment.
"""
        
        assert(stringp(restart) or _restartp(restart))
        restart = restart if _restartp(restart) else find_restart(restart)
        return restart.function(*args, **keys)

def invoke_restart_interactively(restart):
        """
INVOKE-RESTART-INTERACTIVELY calls the function associated with
RESTART, prompting for any necessary arguments. If RESTART is a name,
it must be valid in the current dynamic environment.

INVOKE-RESTART-INTERACTIVELY prompts for arguments by executing the
code provided in the :INTERACTIVE KEYWORD to RESTART-CASE or
:INTERACTIVE-FUNCTION keyword to RESTART-BIND.

If no such options have been supplied in the corresponding
RESTART-BIND or RESTART-CASE, then the consequences are undefined if
the restart takes required arguments. If the arguments are optional,
an argument list of nil is used.

Once the arguments have been determined, INVOKE-RESTART-INTERACTIVELY
executes the following:

 (apply #'invoke-restart restart arguments)
"""
        assert(stringp(restart) or _restartp(restart))
        restart = restart if _restartp(restart) else find_restart(restart)
        return invoke_restart(restart, *restart.interactive_function())

##
## Interactivity
##
def describe(x, stream = t):
        stream = _coerce_to_stream(stream)
        write_line("Object \"%s\" of type %s:" % (x, type_of(x)), stream)
        for attr, val in (x.__dict__ if hasattr(x, "__dict__") else
                          { k: getattr(x, k) for k in dir(x)}).items():
                write_line("%25s: %s" % (attr, ignore_errors(lambda: str(val))), stream)

##
## Modules
##
setq("_module_provider_functions_", [])

def _module_filename(module):
        return "%s/%s.py" % (env.partus_path, _coerce_to_symbol_name(module))

def load(pathspec, verbose = None, print = None,
         if_does_not_exist = t,
         external_format = "default"):
        "XXX: not in compliance"
        verbose = _defaulted_to_var(verbose, "_load_verbose_")
        print   = _defaulted_to_var(verbose, "_load_print_")
        filename = pathspec
        exec(compile(_file_as_string(filename), filename, "exec"))
        return True

def require(name, pathnames = None):
        "XXX: not terribly compliant either"
        name = _coerce_to_symbol_name(name)
        filename = pathnames[0] if pathnames else _module_filename(name)
        if probe_file(filename):
                _not_implemented()
        else:
                error("Don't know how to REQUIRE %s.", name.upper())

##
## Environment
##
def get_universal_time():
        # Issue UNIVERSAL-TIME-COARSE-GRANULARITY
        # time.time() returns microseconds..
        return int(time.time())

def sleep(x):
        return time.sleep(x)

def user_homedir_pathname():
        return os.path.expanduser("~")

def lisp_implementation_type():    return "CPython"
def lisp_implementation_version(): return sys.version

def machine_instance():            return socket.gethostname()
def machine_type():                return _without_condition_system(lambda: platform.machine())
def machine_version():             return "Unknown"

##
## The EVAL
##
def _make_eval_context():
        val = None
        def set(x):
                nonlocal val
                val = x
        def get():
                return val
        return get, set
__evget__, __evset__ = _make_eval_context()

__eval_source_cache__ = dict() # :: code_object -> string

def _coerce_to_expr(x):
        return (x.value if typep(x, ast.Expr) else
                x)

def _eval_python(expr_or_stmt):
        "In AST form, naturally."
        package = symbol_value("_package_")
        exprp = typep(the(ast.AST, expr_or_stmt), [or_, ast.expr, ast.Expr])
        call = ast.fix_missing_locations(_ast_module(
                        [_ast_import_from("cl", ["__evset__", "_read_symbol"]),
                         _ast_Expr(_ast_funcall(_ast_name("__evset__"), _coerce_to_expr(expr_or_stmt)))]
                        if exprp else
                        [expr_or_stmt]))
        code = handler_case(lambda: compile(call, "", "exec"),
                            (error_,
                             lambda cond:
                                     error("EVAL: error while trying to compile <%s>: %s",
                                           more_ast.pp_ast_as_code(expr_or_stmt), cond)))
        if boundp("_source_for_eval_"):
                __eval_source_cache__[code] = symbol_value("_source_for_eval_")
        # write_line(">>> EVAL: %s" % (more_ast.pp_ast_as_code(expr),))
        exec(code, _find_module(_lisp_symbol_name_python_name(package_name(package))).__dict__)
        values = (__evget__() if exprp else
                  tuple())
        return values if _tuplep(values) else (values,)

def _callify(form, package = None, quoted = False):
        package = _defaulted_to_var(package, "_package_")
        obj2ast_xform = {
                False : _ast_name("False"),
                None  : _ast_name("None"),
                True  : _ast_name("True"),
                str   : _ast_string,
                int   : _ast_num,
                }
        if listp(form):
                if quoted or (form[0] is _find_symbol0("QUOTE")):
                        return (_ast_list(mapcar(lambda x: _callify(x, package, True), form[1]))
                                if listp(form[1]) else
                                _callify(form[1], package, True))
                else:
                        return _ast_funcall(_lisp_symbol_ast(form[0], package),
                                            *list(map(lambda x: _callify(x, package), form[1:])))
        elif symbolp(form):
                return (_ast_funcall("_read_symbol",
                                     _ast_string(form.name), _ast_string(form.package.name))
                        if quoted or keywordp(form) else
                        _lisp_symbol_ast(form, package))
        elif constantp(form):
                return obj2ast_xform[type(form)](form)
        elif form in obj2ast_xform:
                return obj2ast_xform[form]
        else:
                error("Unable to convert form %s", form)

def eval_(form):
        package = symbol_value("_package_")
        return _eval_python(_callify(form, package))

###
### Init
###
def _init():
        "Initialise the Common Lisp compatibility layer."
        _init_condition_system()
        return t

###
### Missing stuff
###
# def peek_char(peek_type, stream = nil, eof_error_p = True, eof_value = None, recursive_p = None):
#         return "a"
#
# def read_sequence(sequence, stream, start = 0, end = None):
#         return 0
#
# class _deadline_timeout(condition)
# def _with_deadline(timeout, body)
