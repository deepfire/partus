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

def char_upcase(x):       return x.upper()
def char_downcase(x):     return x.lower()
def upper_case_p(x):      return x.isupper()
def lower_case_p(x):      return x.islower()

__core_symbol_names__ = [
        "QUOTE",
        "AND", "OR", "MEMBER", "EQL", "SATISFIES",
        "ABORT", "CONTINUE", "BREAK",
        "LIST",
        "_KEY", "_REST", "_BODY", "_ALLOW_OTHER_KEYS", "_WHOLE",
        # Heresy!
        "TUPLE", "PARTUPLE", "VARITUPLE", "MAYBE", "CLASS", "CLASS_EQ", "LAMBDA_LIST"
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
def _1arg(*args):
        return args[0]

def _0arg(*args):
        return args[1]

def _narg(n, *args):
        return args[n]

def _alist_plist(xs):
        return append(*xs)

def _plist_alist(xs):
        acc = []
        for i in range(0, len(xs), 2):
                acc.append((xs[i], xs[i + 1]))
        return acc

def _hash_table_alist(xs):
        return xs.items()

def _alist_hash_table(xs):
        return dict(xs)

class _cache(collections.UserDict):
        def __init__(self, filler):
                self.filler = filler
                self.data = dict()
        def __getitem__(self, key):
                check_type(key, tuple)
                key, access_timestamp = key
                if key not in self.data:
                        res = self.filler(key)
                        if res is None: # Allow the filler to refuse.
                                return
                        self.data[key] = res
                return self.data[key]
        def __setitem__(self, key, value):
                error("Direct cache writes are not allowed.")

def _make_timestamping_cache(map_computer):
        cache = _cache(lambda x:
                              let(map_computer(x),
                                  lambda y: ((y, get_universal_time()) if x else
                                             None)))
        def cache_getter(x):
                res = cache[(x, 0)]
                return res[0] if res is not None else None
        return cache, cache_getter

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

def _astp(x):        return typep(x, ast.AST)
def _ast_rw(writep): return (ast.Store() if writep else ast.Load())

### literals
def _ast_num(n):
        return ast.Num(n = the(int, n))
def _ast_bool(n):
        return ast.Bool(n = the(int, n))
def _ast_string(s):
        return ast.Str(s = the(str, s))
def _ast_set(xs,   writep = False):
        return ast.Set(elts   = the((list_, ast.AST), xs), ctx = _ast_rw(writep))
def _ast_list(xs,  writep = False):
        return ast.List(elts  = the((list_, ast.AST), xs), ctx = _ast_rw(writep))
def _ast_tuple(xs, writep = False):
        return ast.Tuple(elts = the((list_, ast.AST), xs), ctx = _ast_rw(writep))

############################### recurse? AST-ifier
__astifier_map__ = { str:             (False, _ast_string),
                     int:             (False, _ast_num),
                     bool:            (False, _ast_num),
                     type(None):      (False, lambda x: _ast_name("None")),
                     list:            (True,  _ast_list),
                     tuple:           (True,  _ast_tuple),
                     set:             (True,  _ast_set),
                     ## symbol: see below
                     }
def _register_astifier_for_type(type, recurse, astifier):
        "Please, list the added astifiers above."
        __astifier_map__[type] = (recurse, astifier)

def _astifiable_p(x):
        return type(x) in __astifier_map__

def _try_astify_constant(x):
        if _astp(x):
                return x, True
        (rec, astifier), astifiable = gethash(type_of(x), __astifier_map__,
                                              ((nil, nil), nil))
        return (astifier(mapcar(lambda x: _astify_constant(x), x) if rec else
                         x), True) if astifiable else (None, None)

def _astify_constant(x):
        ast, successp = _try_astify_constant(x)
        return (ast if successp else
                error("Cannot convert value %s to AST.  Is it a literal?",
                      prin1_to_string(x)))

def _coerce_to_ast(x):
        return _astify_constant(x) if not _astp(x) else x

### expressions
def _ast_alias(name):                        return ast.alias(name = the(str, name), asname = None)
def _ast_keyword(name, value):               return ast.keyword(arg = the(str, name), value = the(ast.expr, value))
def _ast_name(name, writep = False):         return ast.Name(id = the(str, name), ctx = _ast_rw(writep))
def _ast_attribute(x, name, writep = False): return ast.Attribute(attr = name, value = x, ctx = _ast_rw(writep))
def _ast_index(of, index, writep = False):   return ast.Subscript(value = of, slice = ast.Index(value = index), ctx = _ast_rw(writep))
def _ast_maybe_normalise_string(x):          return (_ast_string(x) if stringp(x) else x)

def _ast_funcall(name, args = [], keys = {}, starargs = None, kwargs = None):
        check_type(args, (list_, (or_, ast.AST, type(None), (satisfies_, _astifiable_p))))
        return ast.Call(func = (_ast_name(name) if stringp(name) else name),
                        args = mapcar(_coerce_to_ast, args),
                        keywords = _maphash(_ast_keyword, keys),
                        starargs = starargs or None,
                        kwargs = kwargs or None)

### statements
def _ast_Expr(node):
        return ast.Expr(value = the(ast.expr, node))

def _ast_module(body, lineno = 0):
        return ast.Module(body = the((list_, ast.AST), body),
                          lineno = lineno)

def _ast_import(*names):
        return ast.Import(names = mapcar(ast_alias, the((list_, str), names)))
def _ast_import_from(module_name, names):
        return ast.ImportFrom(module = the(str, module_name),
                              names = mapcar(_ast_alias, the((list_, str), names)),
                              level = 0)

def _ast_assign(to, value):
        return ast.Assign(targets = the((list_, ast.AST), to),
                          value = the(ast.AST, value))
def _ast_return(node):
        return ast.Return(value = the(ast.AST, node))

### lambda lists
# arguments = (arg* args, identifier? vararg, expr? varargannotation,
#              arg* kwonlyargs, identifier? kwarg,
#              expr? kwargannotation, expr* defaults,

#              expr* kw_defaults)
# arg = (identifier arg, expr? annotation)
# keyword = (identifier arg, expr value)
def _argspec_nfixargs(paramspec):
        return len(paramspec.args) - len(paramspec.defaults or []) # ILTW Python implementors think..

def _argspec_lambda_spec(spec):
        # args, varargs, varkw, defaults, kwonlyargs, kwonlydefaults, annotations
        nfixargs = _argspec_nfixargs(spec)
        return (spec.args[:nfixargs],
                list(zip(spec.args[nfixargs:],
                         mapcar(_astify_constant, spec.defaults or []))),
                spec.varargs,
                list(zip(spec.kwonlyargs,
                     mapcar(_astify_constant, spec.kwonlydefaults or []))),
                spec.varkw)
def _lambda_spec_arguments(lambda_list_spec):
        fixed, optional, args, keyword, keys = lambda_list_spec
        return ast.arguments(args        = mapcar(lambda x: ast.arg(x, None),
                                                  fixed + mapcar(lambda x: x[0], optional)),
                             defaults    = mapcar(lambda x: x[1], optional),
                             vararg      = args,
                             kwonlyargs  = mapcar(lambda x: ast.arg(x, None),
                                                  mapcar(lambda x: x[0], keyword)),
                             kw_defaults = mapcar(lambda x: x[1], keyword),
                             kwarg       = keys,
                             varargannotation = None,
                             kwargannotation  = None)
def _ast_functiondef(name, lambda_list_spec, body):
        fixed, optional, args, keyword, keys = lambda_list_spec
        return ast.FunctionDef(
                name = the(string_, name),
                args = _lambda_spec_arguments(lambda_list_spec),
                lineno = 0,
                decorator_list = [],
                returns = None,
                body = etypecase(body,
                                 ((list_, ast.AST),
                                  body),
                                 (function_,
                                  lambda: 
                                  body(*mapcar(_ast_name, fixed),
                                       **_map_into_hash(lambda x: (x, _ast_name),
                                                        (list(optional) + list(keyword) +
                                                         ([args] if args else []) +
                                                         ([keys] if keys else [])))))))

###
### Basis
###
##
## modules/packages
##
def _load_code_object_as_module(name, co, filename = "", builtins = None, globals_ = None, locals_ = None, register = True):
        check_type(co, type(_load_code_object_as_module.__code__))
        mod = imp.new_module(name)
        mod.__filename__ = filename
        if builtins:
                mod.__dict__["__builtins__"] = builtins
        if register:
                sys.modules[name] = mod
        globals_ = _defaulted(globals_, mod.__dict__)
        locals_  = _defaulted(locals_, mod.__dict__)
        exec(co,
             globals_,
             locals_)
        return mod, globals_, locals_

def _load_text_as_module(name, text, filename = "", **keys):
        return _load_code_object_as_module(name, compile(text, filename, "exec"),
                                           filename = filename, **keys)[0]

def _reregister_module_as_package(mod, parent_package = None):
        # this line might need to be performed before exec()
        mod.__path__ = (parent_package.__path__ if parent_package else []) + [ mod.name.split(".")[-1] ]
        if parent_package:
                dotpos = mod.name.rindex(".")
                assert(dotpos)
                postdot_name = mod.name[dotpos + 1:]
                setattr(parent_package, postdot_name, mod)
                parent_package.__children__.add(mod)
                mod.__parent__ = parent_package
        if packagep:
                mod.__children__ = set([])

def _compile_and_load(*body, modname = "", filename = "", lineno = 0, **keys):
        return _load_code_object_as_module(
                modname,
                compile(ast.fix_missing_locations(_ast_module(list(body), lineno = lineno)), filename, "exec"),
                register = nil,
                filename = filename,
                **keys)

def _ast_compiled_name(name, *body, **keys):
        mod, globals, locals = _compile_and_load(*body, **keys)
        return locals[name]

##
## frames
##
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

def _frames_calling(f = None, n = -1):
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

# Issue FRAME-CODE-OBJECT-IS-NOT-FUN
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
def _fun_name(f):        return f.co_name
def _fun_filename(f):    return f.co_filename
def _fun_firstlineno(f): return f.co_firstlineno
def _fun_bytecode(f):    return f.co_code
def _fun_constants(f):   return f.co_consts

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
        _print_frames(_frames_calling(_this_frame())[1:x],
                      _defaulted_to_var(stream, "_debug_io_"))

def _pp_frame_chain(xs, source_location = None, all_pretty = None, print_fun_line = None):
        def _pp_frame_in_chain(f, pretty = None):
                fun = _frame_fun(f)
                return format(nil, *(("%s",
                                      _fun_name(fun))
                                     if not pretty else
                                     ("%s%s@%s:%d",
                                      _fun_name(fun),
                                      (":" + str(_frame_lineno(f) - _fun_firstlineno(fun))) if print_fun_line else "",
                                      _fun_filename(fun),
                                      _frame_lineno(f))))
        return ("..".join(mapcar(lambda f: _pp_frame_in_chain(f, t), xs) if all_pretty else
                          (mapcar(lambda f: _pp_frame_in_chain(f), xs[:-1]) +
                           [_pp_frame_in_chain(xs[-1], t)])))

def _pp_chain_of_frame(x, callers = 5, *args, **keys):
        fs = _frames_calling(x, callers)
        fs.reverse()
        return _pp_frame_chain(fs, *args, **keys)

def _here(note = None, *args, callers = 5, stream = None, default_stream = sys.stderr, frame = None, print_fun_line = None, all_pretty = None):
        return _debug_printf("    (%s)  %s:\n      %s" % (threading.current_thread().name.upper(),
                                                          _pp_chain_of_frame(_defaulted(frame, _caller_frame()),
                                                                             callers = callers - 1,
                                                                             print_fun_line = print_fun_line,
                                                                             all_pretty = all_pretty),
                                                          (""           if not note else
                                                           " - " + note if not args else
                                                           (note % args))),
                            # _defaulted(stream, default_stream)
                             )

def _fprintf(stream, format_control, *format_args):
        try:
                return _write_string(format_control % format_args, stream)
        except UnicodeEncodeError:
                return _write_string((format_control % format_args).encode("utf-8"), stream)

def _debug_printf(format_control, *format_args):
        _fprintf(sys.stderr, format_control + "\n", *format_args)

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
# Study was done by the means of:
# print("\n".join((lambda listattr:
#                   map(lambda f:
#                        "== co %s\n  %s\n== def %s\n  %s\n" %
#                        (f, listattr(f), cl._fun_name(cl._frame_fun(f)), listattr(cl._frame_fun(f))),
#                        cl._frames_calling(cl._example_frame())))
#                 (lambda x: "\n  ".join(map(lambda s: s + ": " + str(getattr(x, s)),
#                                            cl.remove_if(lambda attr: "__" in attr or "builtins" in attr or "locals" in attr or "globals" in attr,
#                                                         dir(x)))))))

# == co <frame object at 0x2381de0>
#   f_back: <frame object at 0x2381c00>
#   f_code: <code object xceptor at 0x277a4f8, file "cl.py", line 199>
#   f_lasti: 59
#   f_lineno: 204
#   f_trace: None
# == def xceptor
#   co_argcount: 1
#   co_cellvars: ()
#   co_code: b'y\x11\x00t\x00\x00d\x01\x00|\x00\x00\x83\x02\x00\x01Wn,\x00\x04t\x01\x00k\n\x00r?\x00\x01}\x01\x00\x01z\x0c\x00t\x02\x00\x83\x00\x00SWYd\x02\x00d\x02\x00}\x01\x00~\x01\x00Xn\x01\x00Xd\x02\x00S'
#   co_consts: ('names: globals;  varnames: args + otherbind;  locals: len(varnames)', 'This is xceptor talking: %s.', None)
#   co_filename: cl.py
#   co_firstlineno: 199
#   co_flags: 83
#   co_freevars: ()
#   co_kwonlyargcount: 0
#   co_lnotab: b'\x00\x02\x03\x01\x11\x01\x12\x01'
#   co_name: xceptor
#   co_names: ('error', 'Exception', '_this_frame')
#   co_stacksize: 16
#   co_varnames: ('xceptor_arg', 'cond')

# == co <frame object at 0x2381c00>
#   f_back: <frame object at 0x1fa8480>
#   f_code: <code object midder at 0x277a580, file "cl.py", line 205>
#   f_lasti: 19
#   f_lineno: 208
#   f_trace: None
# == def midder
#   co_argcount: 1
#   co_cellvars: ()
#   co_code: b'd\x01\x00}\x01\x00\x88\x00\x00|\x00\x00|\x01\x00\x17\x83\x01\x00S'
#   co_consts: ('freevars: non-global-free;  varnames: args + otherbind;  locals: ...', 0)
#   co_filename: cl.py
#   co_firstlineno: 205
#   co_flags: 19
#   co_freevars: ('xceptor',)
#   co_kwonlyargcount: 0
#   co_lnotab: b'\x00\x02\x06\x01'
#   co_name: midder
#   co_names: ()
#   co_stacksize: 3
#   co_varnames: ('midder_arg', 'midder_stack_var')

# == co <frame object at 0x1fa8480>
#   f_back: <frame object at 0x27ce6c0>
#   f_code: <code object outer at 0x277a608, file "cl.py", line 209>
#   f_lasti: 15
#   f_lineno: 212
#   f_trace: None
# == def outer
#   co_argcount: 0
#   co_cellvars: ()
#   co_code: b'd\x01\x00}\x00\x00\x88\x00\x00|\x00\x00\x83\x01\x00S'
#   co_consts: ('freevars: non-global-free;  varnames: args + otherbind', 3)
#   co_filename: cl.py
#   co_firstlineno: 209
#   co_flags: 19
#   co_freevars: ('midder',)
#   co_kwonlyargcount: 0
#   co_lnotab: b'\x00\x02\x06\x01'
#   co_name: outer
#   co_names: ()
#   co_stacksize: 2
#   co_varnames: ('outer_stack_var',)

# == co <frame object at 0x27ce6c0>
#   f_back: <frame object at 0x27f3030>
#   f_code: <code object _example_frame at 0x277a690, file "cl.py", line 197>
#   f_lasti: 45
#   f_lineno: 213
#   f_trace: None
# == def _example_frame
#   co_argcount: 0
#   co_cellvars: ('xceptor', 'midder')
#   co_code: b'd\x01\x00\x84\x00\x00\x89\x00\x00\x87\x00\x00f\x01\x00d\x02\x00\x86\x00\x00\x89\x01\x00\x87\x01\x00f\x01\x00d\x03\x00\x86\x00\x00}\x00\x00|\x00\x00\x83\x00\x00S'
#   co_consts: ('cellvars: closed over non-globals;  varnames: bound', <code object xceptor at 0x277a4f8, file "cl.py", line 199>, <code object midder at 0x277a580, file "cl.py", line 205>, <code object outer at 0x277a608, file "cl.py", line 209>)
#   co_filename: cl.py
#   co_firstlineno: 197
#   co_flags: 3
#   co_freevars: ()
#   co_kwonlyargcount: 0
#   co_lnotab: b'\x00\x02\t\x06\x0f\x04\x0f\x04'
#   co_name: _example_frame
#   co_names: ()
#   co_stacksize: 2
#   co_varnames: ('outer',)

# == co <frame object at 0x27f3030>
#   f_back: <frame object at 0x2388fd0>
#   f_code: <code object <lambda> at 0x278de00, file "<stdin>", line 1>
#   f_lasti: 36
#   f_lineno: 5
#   f_trace: None
# == def <lambda>
#   co_argcount: 1
#   co_cellvars: ('listattr',)
#   co_code: b't\x00\x00\x87\x00\x00f\x01\x00d\x01\x00\x86\x00\x00t\x01\x00j\x02\x00t\x01\x00j\x03\x00\x83\x00\x00\x83\x01\x00\x83\x02\x00S'
#   co_consts: (None, <code object <lambda> at 0x278d0b8, file "<stdin>", line 2>)
#   co_filename: <stdin>
#   co_firstlineno: 1
#   co_flags: 3
#   co_freevars: ()
#   co_kwonlyargcount: 0
#   co_lnotab: b'\x00\x01\x0f\x03'
#   co_name: <lambda>
#   co_names: ('map', 'cl', '_frames_calling', '_example_frame')
#   co_stacksize: 4
#   co_varnames: ('listattr',)

# == co <frame object at 0x2388fd0>
#   f_back: None
#   f_code: <code object <module> at 0x220f7a0, file "<stdin>", line 1>
#   f_lasti: 24
#   f_lineno: 6
#   f_trace: None
# == def <module>
#   co_argcount: 0
#   co_cellvars: ()
#   co_code: b'e\x00\x00d\x00\x00j\x01\x00d\x01\x00\x84\x00\x00d\x02\x00\x84\x00\x00\x83\x01\x00\x83\x01\x00\x83\x01\x00Fd\x03\x00S'
#   co_consts: ('\n', <code object <lambda> at 0x278de00, file "<stdin>", line 1>, <code object <lambda> at 0x220f2d8, file "<stdin>", line 6>, None)
#   co_filename: <stdin>
#   co_firstlineno: 1
#   co_flags: 64
#   co_freevars: ()
#   co_kwonlyargcount: 0
#   co_lnotab: b'\x0f\x05'
#   co_name: <module>
#   co_names: ('print', 'join')
#   co_stacksize: 4
#   co_varnames: ()

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
condition         = BaseException
error_            = Exception
serious_condition = Exception

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
def _if_let(x, consequent, antecedent = lambda: None):
        return consequent(x) if x else antecedent()

def _when_let(x, consequent):
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

def _prefix_suffix_if(f, xs, key = identity):
        for i, x in enumerate(xs):
                if not f(key(x)):
                        return xs[:i], xs[i:]
        return xs, []

def _prefix_suffix_if_not(f, xs, key = identity):
        return _prefix_suffix_if(lambda x: not f(x), xs, key = key)

##
## Lesser non-CL tools
##
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
                if ((val == cval or (cval is True) or (cval is t)) if not isinstance(cval, list) else
                    val in cval):
                        return body() if isinstance(body, function_) else body

def ecase(val, *clauses):
        for (cval, body) in clauses:
                if ((val == cval) if not isinstance(cval, list) else
                    val in cval):
                        return body() if isinstance(body, function_) else body
        error("%s fell through ECASE expression. Wanted one of %s.", val, mapcar(first, clauses))

def every(fn, *xss):
        for xs in zip(*xss):
                if not fn(*xs): return False
        return True

def _some_not(fn, *xss):
        for xs in zip(*xss):
                if not fn(*xs): return True
        return False

def some(fn, *xss):
        for xs in zip(*xss):
                if fn(*xs): return True
        return False

def none(fn, *xss):
        for xs in zip(xss):
                if fn(*xs): return False
        return True

##
## Types
##
class type_error(error_):
        pass

class simple_type_error(simple_condition, type_error):
        pass

type_   = builtins.type    # Should we shadow org.python.type?
stream_ = stream = _io._IOBase
string_ = str

def find_class(x, errorp = True):
        check_type(x, symbol)
        return (x.value if typep(x.value, type_) else
                nil     if not errorp            else
                error("There is no class named %s.", x))

def type_of(x):
        return type(x)

def _of_type(x):
        return lambda y: typep(y, x)

def _every_typep(xs, type):
        for x in xs:
                if not typep(x, type): return False
        return True

def _invalid_type_specifier(x):
        error(simple_type_error, "%s is not a valid type specifier.", x)

# __type_predicate_map__ is declared after the package system is initialised
def _check_complex_type(x, type):
        fast_test, zero, test, element_test = __type_predicate_map__[type[0]]
        return (let(fast_test(x, type),
                    lambda ret: (ret if ret is not None else
                                 _invalid_type_specifier(type)))          if fast_test      else
                (zero if zero is not None else
                 error("Type specifier %s requires arguments.", type[0])) if len(type) is 1 else
                test(lambda elem_type: element_test(x, elem_type),
                     type[1:]))

def typep(x, type):
        return (isinstance(x, type)          if isinstance(type, type_)                      else
                t                            if type is t                                    else
                _check_complex_type(x, type) if (_tuplep(type) and
                                                 type and type[0] in __type_predicate_map__) else
                _invalid_type_specifier(type))

def subtypep(sub, super):
        return (issubclass(sub, super)              if super is not t                 else
                _not_implemented("complex type relatioships: %s vs. %s.",
                                 sub, super)        if _tuplep(sub) or _tuplep(super) else
                error("%s is not a type specifier") if not (typep(sub, (or_, type_, (eql_, t))) and
                                                            typep(sub, (or_, type_, (eql_, t)))) else
                sub is super or super is t)

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
def car(x):           return x[0]   if x  else nil
def cdr(x):           return x[1:]  if x  else nil
def first(xs):        return xs[0]  if xs else nil
def rest(xs):         return xs[1:] if xs else nil
def nth(n, xs):       return xs[n] if n < len(xs) else nil

def copy_list(x):
        return list(the(list, x))

def pop(xs):
        if xs:
                x, xs[0:1] = xs[0], []
                return x
        else:
                return nil

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
def stable_sort(xs, predicate):
        return sorted(xs, key = functools.cmp_to_key(predicate))

def vector_push(vec, x):
        "XXX: compliance"
        vec.append(x)
        return vec

def vector_push_extend(vec, x):
        "XXX: compliance"
        vec.append(x)
        return vec

def getf(xs, key, default = None):
        for i, x in enumerate(xs):
                if not i%2 and x == key:
                        return xs[i + 1]
        else:
                return _defaulted(default, nil)

def setf_getf(xs, key, value):
        for i, x in enumerate(xs):
                if not i%2 and x == key:
                        xs[i + 1] = value
                        return xs
        else:
                return [key, value] + xs

def assoc(x, xs, test = equal):
        for k, v in xs:
                if test(x, k):
                        return v

def aref(xs, *indices):
        r = xs
        for i in indices:
                r = r[i]
        return r

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

def memq(item, list):
        "Return tail of LIST beginning with first element EQ to ITEM."
        # List views?
        for i, x in enumerate(xs):
                if x is elt:
                        return xs[i:]
        return []

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
def gethash(key, dict, default = None):
        inp = key in dict
        return (dict.get(key) if inp else default), key in dict

# Issue INCONSISTENT-HASH-TABLE-FUNCTION-NAMING
def _maphash(f, dict) -> list:
        return [ f(k, v) for k, v in dict.items() ]

def _remap_hash_table(f, xs: dict) -> dict:
        return { k: f(k, v) for k, v in xs.items() }

def _map_into_hash(f, xs,
                   key_test = lambda k: k is not None,
                   value_test = lambda _: True) -> dict:
        acc = dict()
        for x in xs:
                k, v = f(x)
                if key_test(k) and value_test(v):
                        acc[k] = v
        return acc

def _map_into_hash_star(f, xs,
                        key_test = lambda k: k is not None,
                        value_test = lambda _: t) -> dict:
        acc = dict()
        for x in xs:
                k, v = f(*x)
                if key_test(k) and value_test(v):
                        acc[k] = v
        return acc

def _map_hash_table(f, hash_table, **keys) -> dict:
        return _map_into_hash_star(f, hash_table.items(), **keys)

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
                                _enable_pytracer(reason = "ball caught")
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
                        acc += "_" if c in "-*&" else c
                return acc
        ret = _sub(x).lower()
        # debug_printf("==> Python(Lisp %s) == %s", x, ret)
        return ret

def coerce(type, x):
        return (x if isinstance(x, type) else
                case(type,
                     (str,  "".join(x)),
                     (dict, dict.fromkeys(x)),
                     (t,    type(x))))

def _python_name_lisp_symbol_name(x):
        "Heuristic to undo the effect of _lisp_symbol_name_python_name()."
        def _sub(cs):
                starred = len(cs) > 1 and (cs[0] == cs[-1] == "_") # *very-nice*
                anded   = len(cs) > 1 and (cs[0] == "_" != cs[-1]) # &something    # This #\& heuristic might bite us quite well..
                pre, post, start, end = (("*", "*", 1, len(cs) - 1) if starred else
                                         ("&", "",  1, None)        if anded   else
                                         ("",  "",  0, None))
                return (pre +
                        coerce(string_,
                               ("-" if c == "_" else c for c in cs[start:end])) +
                        post)
        ret = _sub(x).upper()
        # debug_printf("==> (Lisp (Python %s)) == %s", x, ret)
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

def fboundp(x):
        return hasattr(the(symbol, x), "__call__")

def function(name):
        pyname, module = _lisp_symbol_python_addr(name)
        return the(function_, _lisp_symbol_python_value(name))

def symbol_function(x):
        check_type(x, symbol)
        func, fboundp = gethash("function", x.__dict__)
        if not fboundp:
                error("The function %s is undefined.", x)
        return func

def defun(name, function):
        the(symbol, name).function = function
        return name

class symbol():
        def __str__(self):
                return _print_symbol(self)
        def __repr__(self):
                return str(self)
        def __init__(self, name):
                self.name, self.package, self.value, self.function = name, None, None, None
        def __hash__(self):
                return hash(self.name) ^ (hash(self.package.name) if self.package else 0)
        def __call__(self, *args, **keys):
                return symbol_function(self)(*args, **keys)
        def __bool__(self):
                return self is not nil

_register_astifier_for_type(symbol, False, (lambda sym:
                                             _ast_funcall("_find_symbol_or_fail",
                                                          [symbol_name(sym)])))

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
                ps, accessible = gethash(s.name, p.accessible)
                if ps is s:
                        continue
                elif accessible: # conflict
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

def _without_condition_system(body, reason = ""):
        if _pytracer_enabled_p():
                try:
                        _disable_pytracer(reason = reason)
                        return body()
                finally:
                        _enable_pytracer(reason = "%s done" % reason)
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
        # secondary
        global star
        star = _intern0("*", cl)
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

def setf_fdefinition(symbol_name, function):
        symbol_name = string(symbol_name)
        symbol, therep = gethash(symbol_name, globals())
        if not therep:
                globals()[symbol_name] = symbol = _intern0(symbol_name)
        symbol.function = function
        symbol.__name__        = symbol_name
        symbol.__annotations__ = {}
        symbol.__code__        = function.__code__
        return function

def defun(f):
        symbol_name = f.__name__
        setf_fdefinition(symbol_name, f)
        return globals()[symbol_name] # guaranteed to exist at this point

@defun
def maybe_(x, type):
        return x is None or x is nil or typep(x, type[1])
assert(symbolp(maybe_))

@defun
def list_(x, type):
        return isinstance(x, list) and _every_typep(x, type[1])

@defun
def satisfies_(x, type):
        return type[1](x)

@defun
def eql_(x, type):
        return eql(x, type[1])

@defun
def tuple_(x, type):
        return (_tuplep(x)              and
                len(x) == len(type) - 1 and
                every(typep, x, type[1:]))

@defun
def partuple_(x, type):
        return (_tuplep(x)              and
                len(x) >= len(type) - 1 and
                every(typep, x, type[1:]))

__variseq__ = (tuple_, (eql_, maybe_), t) # Meta-type, heh..
@defun
def varituple_(x, type):
        # correctness enforcement over speed?
        fixed_t, maybes_t = _prefix_suffix_if_not(_of_type(__variseq__), type[1:])
        if not every(_of_type(__variseq__), maybes_t):
                return None # fail
        fixlen = len(fixed_t)
        return (len(x) >= fixlen                  and
                every(typep, x[:fixlen], fixed_t) and
                _every_typep(x[fixlen:], (or_,) + tuple(t[1] for t in maybes_t)))

__type_predicate_map__ = {
        or_:            (nil,          nil, some,  typep),
        and_:           (nil,          t,   every, typep),
        member_:        (nil,          nil, some,  eql),
        eql_:           (eql_,         nil, nil, nil),
        satisfies_:     (satisfies_,   nil, nil, nil),
        maybe_:         (maybe_,       nil, nil, nil),
        # XXX: this is a small lie: this is not a cons-list
        # ..but neither CL has a type specifier like this and the others, that follow..
        list_:          (list_,        nil, nil, nil),
        tuple_:         (tuple_,       nil, nil, nil),
        partuple_:      (partuple_,    nil, nil, nil),
        varituple_:     (varituple_,   nil, nil, nil),
        }

##
## T/NIL-dependent stuff
##
def defvar(name, value = None, documentation = nil):
        "XXX: documentation, declaring as special"
        if not boundp(name) and value is not None:
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

class readtable(collections.UserDict):
        def __init__(self, case = _keyword("upcase")):
                self.case = the((member_, _keyword("upcase"), _keyword("downcase"), _keyword("preserve"), _keyword("invert")),
                                case)
                self.dict = dict()

def readtablep(x):     return typep(x, readtable)
def readtable_case(x): return the(readtable, x).case

def copy_readtable(x):
        check_type(x, readtable)
        new = readtable(case = readtable_case(x))
        new.dict = dict(x.dict)
        return new

__standard_pprint_dispatch__ = dict() # XXX: this is crap!
__standard_readtable__       = readtable() # XXX: this is crap!

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
                                lambda: re.sub(r"([\"\\])", r"\\\1", x),
                                reason = "re.sub") +
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
                        elif object is False or object is None or object is True:
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
                        elif (not escape) and typep(object, (or_, restart, condition)):
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
                if _without_condition_system(lambda: re.match("^[0-9]+$", token),
                                             reason = "re.match"):
                        ret = int(token)
                elif _without_condition_system(lambda: re.match("^[0-9]+\\.[0-9]+$", token),
                                               reason = "re.match"):
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

def read_line(stream = None, eof_error_p = True, eof_value = nil):
        stream = _defaulted_to_var(stream, "_standard_input_")
        return handler_case(lambda: stream.readline(),
                            (error_,
                             lambda c: error(end_of_file, "end of file on %s" % (stream,))))

def read_char(stream = None, eof_error_p = True, eof_value = nil, recursivep = nil):
        stream = _defaulted_to_var(stream, "_standard_input_")
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

def peek_char(peek_type = nil, input_stream = None, eof_error_p = nil, eof_value = nil, recursive_p = nil):
        """PEEK-CHAR obtains the next character in INPUT-STREAM without actually reading it, thus leaving the character to be read at a later time. It can also be used to skip over and discard intervening characters in the INPUT-STREAM until a particular character is found.

If PEEK-TYPE is not supplied or NIL, PEEK-CHAR returns the next character to be read from INPUT-STREAM, without actually removing it from INPUT-STREAM. The next time input is done from INPUT-STREAM, the character will still be there. If PEEK-TYPE is T, then PEEK-CHAR skips over whitespace[2] characters, but not comments, and then performs the peeking operation on the next character. The last character examined, the one that starts an object, is not removed from INPUT-STREAM. If PEEK-TYPE is a character, then PEEK-CHAR skips over input characters until a character that is CHAR= to that character is found; that character is left in INPUT-STREAM.

If an end of file[2] occurs and EOF-ERROR-P is false, EOF-VALUE is returned.

If RECURSIVE-P is true, this call is expected to be embedded in a higher-level call to READ or a similar function used by the Lisp reader.

When INPUT-STREAM is an echo stream, characters that are only peeked at are not echoed. In the case that PEEK-TYPE is not NIL, the characters that are passed by PEEK-CHAR are treated as if by READ-CHAR, and so are echoed unless they have been marked otherwise by UNREAD-CHAR."""
        criterion = (lambda _: t                if peek_type is nil                           else
                     lambda c: c not in " \t\n" if peek_type is t                             else
                     lambda c: c == peek_type   if stringp(peek_type) and len(peek_type) == 1 else
                     error("Invalid peek-type: '%s'.", peek_type))
        stream = _defaulted(input_stream, symbol_value("_standard_input_"))
        while True:
                char = read_char(stream, eof_error_p, eof_value, recursive_p)
                if criterion(char):
                        unread_char(char, stream)
                        return char

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
                if _without_condition_system(lambda: re.match("^[0-9]+$", token),
                                             reason = "re.match"):
                        ret = int(token)
                elif _without_condition_system(lambda: re.match("^[0-9]+\\.[0-9]+$", token),
                                               reason = "re.match"):
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
                lambda: os.path.exists(pathname),
                reason = "os.path.exists")

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
                _without_condition_system(handler,
                                          reason = "_write_string")
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

def setf_file_position(x, posn):
        return x.seek(posn)

def finish_output(stream = t):
        check_type(stream, (or_, stream_, (member_, t, nil)))
        (stream is not nil) and _coerce_to_stream(stream).flush()

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
def _enable_pytracer(reason = "", report = None):
        sys.settrace(_pytracer); report and _debug_printf("_ENABLE (%s)", reason);  return True
def _disable_pytracer(reason = "", report = None):
        sys.settrace(None);      report and _debug_printf("_DISABLE (%s)", reason); return True

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

def _report_handling_handover(cond, frame, hook):
        format(sys.stderr, "Handing over handling of %s to frame %s\n",
               prin1_to_string(cond), _pp_chain_of_frame(frame, callers = 25))

def signal(cond):
        for cluster in reversed(env.__handler_clusters__):
                for type, handler in cluster:
                        if not stringp(type):
                                if typep(cond, type):
                                        hook = symbol_value("_prehandler_hook_")
                                        if hook:
                                                frame = assoc("__frame__", cluster)
                                                assert(frame)
                                                hook(cond, frame, hook)
                                        handler(cond)
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
        _without_condition_system(body,
                                  reason = "_dump_thread_state")

__not_even_conditions__ = frozenset([SystemExit, __catcher_throw__])
"A set of condition types which are entirely ignored by the condition system."

def __cl_condition_handler__(condspec, frame):
        def continuation():
                type, raw_cond, traceback = condspec
                # _print_frames(_frames_calling(frame))
                if type_of(raw_cond) not in __not_even_conditions__:
                        def _maybe_upgrade_condition(cond):
                                "Fix up the shit routinely being passed around."
                                return ((cond, False) if typep(cond, condition) else
                                        (condspec[0](*([cond] if not sequencep(cond) or stringp(cond) else
                                                       cond)), True))
                                       # typecase(cond,
                                       #          (BaseException, lambda: cond),
                                       #          (str,       lambda: error_(cond)))
                        cond, upgradedp = _maybe_upgrade_condition(raw_cond)
                        if upgradedp:
                                _here("Condition Upgrader: %s(%s) -> %s(%s)",
                                      prin1_to_string(raw_cond), type_of(raw_cond),
                                      prin1_to_string(cond), type_of(cond),
                                      callers = 45, frame = symbol_value("_stack_top_hint_"))
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
                is_not_ball = type_of(cond) is not __catcher_throw__
                _here("In thread '%s': unhandled condition : %s%s",
                      threading.current_thread().name, prin1_to_string(cond),
                      ("\n; Disabling CL condition system." if is_not_ball else
                       ""),
                      callers = 15)
                if is_not_ball:
                        _disable_pytracer(reason = "unhandled condition")
        ## Issue UNHANDLED-CONDITIONS-NOT-REALLY
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
                             [handlers + (("__frame__", _caller_frame()),)]):
                        return no_error(fn())
        else:
                # old world case..
                # format(t, "crap FAIL: pep %s, exhook is cch: %s",
                #        _pytracer_enabled_p(), __tracer_hooks__.get("exception") is __cl_condition_handler__)
                if len(handlers) > 1:
                        error("HANDLER-BIND: was asked to establish %d handlers, but cannot establish more than one in 'dumb' mode.",
                              len(handlers))
                condition_type_name, handler = handlers[-1]
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
        with env.let(__restart_clusters__ = (env.__restart_clusters__ +
                                             [_remap_hash_table(lambda _, restart_args: restart(**restart_args), restarts_args)])):
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
def describe(x, stream = t, show_hidden = nil):
        stream = _coerce_to_stream(stream)
        write_line("Object \"%s\" of type %s:" % (x, type_of(x)), stream)
        for attr, val in (x.__dict__ if hasattr(x, "__dict__") else
                          { k: getattr(x, k) for k in dir(x)}).items():
                if show_hidden or "__" not in attr:
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
def machine_type():                return _without_condition_system(lambda: platform.machine(),
                                                                    reason = "platform.machine")
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

def _evaluated_code_source(co):
        return gethash(co, __eval_source_cache__)

def _coerce_to_expr(x):
        return (x.value if typep(x, ast.Expr) else
                x)

def _eval_python(expr_or_stmt):
        "In AST form, naturally."
        package = symbol_value("_package_")
        exprp = typep(the(ast.AST, expr_or_stmt), (or_, ast.expr, ast.Expr))
        call = ast.fix_missing_locations(_ast_module(
                        [_ast_import_from("cl", ["__evset__", "_read_symbol"]),
                         _ast_Expr(_ast_funcall(_ast_name("__evset__"), [_coerce_to_expr(expr_or_stmt)]))]
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
        def callify_call(sym, args):
                func = function(the(symbol, sym))
                paramspec = inspect.getfullargspec(func)
                nfix = _argspec_nfixargs(paramspec)
                _here("func: %s -> %s, paramspec: %s", sym, func, paramspec)
                _here("nfix: %s", nfix)
                _here("args: %s", args)
                _here("nkeys: %s", len(args) - nfix)
                if oddp(len(args) - nfix):
                        error("odd number of &KEY arguments")
                allow_other_keys = paramspec.varkw is not None
                fixnames, keynames = (paramspec.args[0:nfix],
                                      set(paramspec.args[nfix:] + paramspec.kwonlyargs))
                fixargs = args[0:nfix]
                keyargs = ({ _lisp_symbol_python_name(k):v
                             for k, v in _plist_alist(args[nfix:]) })
                if not allow_other_keys:
                        for k in keyargs.keys():
                                if k not in keynames:
                                        error("unknown &KEY argument: %s", k)
                return _ast_funcall(
                        _lisp_symbol_ast(sym, package),
                        mapcar(lambda x: _callify(x, package),
                               args),
                        _map_hash_table(
                               lambda k, x: (k, _callify(x, package)),
                                      keyargs))
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
                        return callify_call(form[0], form[1:])
        elif symbolp(form):
                return (_ast_funcall("_read_symbol", [_ast_string(form.name),
                                                      _ast_string(form.package.name)])
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

def _compile_list(body):
        

# Unregistered Issue C-J-COULD-BE-EXTENDED-TO-FOLLOW-M-J-WITHIN-COMMENTS
##
## An attempt at CLOS imitation
##
class standard_object():
        pass

def make_instance(class, **keys):
        "XXX: compliance?"
        return class(***keys)

class standard_method(standard_object):
        def __init__(self, function = None, **key):
                if not functionp(function):
                        # When the method metaobject is created with MAKE-INSTANCE,
                        # the method function must be the value of the :FUNCTION
                        # initialization argument.
                        error("MAKE-INSTANCE for method metaobjects requires the :FUNCTION keyword to name a function.")
                self.function = function
        def __call__(self, gfun_args, next_methods):
                return self.function(gfun_args, next_methods)

def _generic_function_p(x): return functionp(x) and hasattr(x, "__methods__")
def _method_p(x):           return functionp(x) and hasattr(x, "__specializers__")
def _specializerp(x):       return ((x is t)        or
                                    typep(x, (or_, type_, (tuple_, (eql_, eql_), t))))

def _get_generic_fun_info(generic_function):
        return values(len(generic_function.__lambda_list__[0]), # nreq
                      nil,
                      [],
                      len(generic_function.__lambda_list__[3]),
                      generic_function.__lambda_list__)

def generic_function_name(x):    return x.__name__
def generic_function_methods(x): return x.__methods__.values()
# def generic_function_(x):       return x.____

__method_combinations__ = dict()
def _find_method_combination(name):
        combin, therep = gethash(name, __method_combinations__)
        if not therep:
                error("Undefined method combination: %s", name)
        return combin

def _qualifier_pattern_p(x):
        return _not_implemented()

def define_method_combination(name, method_group_specifiers, body,
                              arguments = None, generic_function = None):
        """Syntax:

define-method-combination name [[short-form-option]]

=> name

define-method-combination name lambda-list (method-group-specifier*) [(:arguments . args-lambda-list)] [(:generic-function generic-function-symbol)] [[declaration* | documentation]] form*

=> name

short-form-option::= :documentation documentation |  
                     :identity-with-one-argument identity-with-one-argument | 
                     :operator operator 

method-group-specifier::= (name {qualifier-pattern+ | predicate} [[long-form-option]]) 

long-form-option::= :description description | 
                    :order order | 
                    :required required-p 

Arguments and Values:

args-lambda-list---a define-method-combination arguments lambda list.

declaration---a declare expression; not evaluated.

description---a format control.

documentation---a string; not evaluated.

forms---an implicit progn that must compute and return the form that specifies how the methods are combined, that is, the effective method.

generic-function-symbol---a symbol.

identity-with-one-argument---a generalized boolean.

lambda-list---ordinary lambda list.

name---a symbol. Non-keyword, non-nil symbols are usually used.

operator---an operator. Name and operator are often the same symbol. This is the default, but it is not required.

order---:most-specific-first or :most-specific-last; evaluated.

predicate---a symbol that names a function of one argument that returns a generalized boolean.

qualifier-pattern---a list, or the symbol *.

required-p---a generalized boolean.

Description:

The macro DEFINE-METHOD-COMBINATION is used to define new types of
method combination.

There are two forms of DEFINE-METHOD-COMBINATION. The short form is a
simple facility for the cases that are expected to be most commonly
needed. The long form is more powerful but more verbose. It resembles
DEFMACRO in that the body is an expression, usually using backquote,
that computes a form. Thus arbitrary control structures can be
implemented. The long form also allows arbitrary processing of method
qualifiers.

Short Form

    The short form syntax of DEFINE-METHOD-COMBINATION is recognized
    when the second subform is a non-NIL symbol or is not
    present. When the short form is used, name is defined as a type of
    method combination that produces a Lisp form (operator method-call
    method-call ...). The operator is a symbol that can be the name of
    a function, macro, or special operator. The operator can be
    supplied by a keyword option; it defaults to name.

    Keyword options for the short form are the following:

        The :DOCUMENTATION option is used to document the
        METHOD-COMBINATION type; see description of long form below.

        The :IDENTITY-WITH-ONE-ARGUMENT option enables an optimization
        when its value is true (the default is false). If there is
        exactly one applicable method and it is a primary method, that
        method serves as the effective method and operator is not
        called. This optimization avoids the need to create a new
        effective method and avoids the overhead of a function
        call. This option is designed to be used with operators such
        as PROGN, AND, +, and MAX.

        The :OPERATOR option specifies the name of the operator. The
        operator argument is a symbol that can be the name of a
        function, macro, or special form.

    These types of method combination require exactly one qualifier
    per method. An error is signaled if there are applicable methods
    with no qualifiers or with qualifiers that are not supported by
    the method combination type.

    A method combination procedure defined in this way recognizes two
    roles for methods. A method whose one qualifier is the symbol
    naming this type of method combination is defined to be a primary
    method. At least one primary method must be applicable or an error
    is signaled. A method with :AROUND as its one qualifier is an
    auxiliary method that behaves the same as an around method in
    standard method combination. The function CALL-NEXT-METHOD can
    only be used in around methods; it cannot be used in primary
    methods defined by the short form of the DEFINE-METHOD-COMBINATION
    macro.

    A method combination procedure defined in this way accepts an
    optional argument named order, which defaults
    to :MOST-SPECIFIC-FIRST. A value of :MOST-SPECIFIC-LAST reverses
    the order of the primary methods without affecting the order of
    the auxiliary methods.

    The short form automatically includes error checking and support
    for around methods.

    For a discussion of built-in method combination types, see Section
    7.6.6.4 (Built-in Method Combination Types).

Long form (snipped)

    The long form syntax of DEFINE-METHOD-COMBINATION is recognized
    when the second subform is a list.

    The LAMBDA-LIST receives any arguments provided after the name of
    the method combination type in the :METHOD-COMBINATION option to
    DEFGENERIC.

    A list of method group specifiers follows. Each specifier selects
    a subset of the applicable methods to play a particular role,
    either by matching their qualifiers against some patterns or by
    testing their qualifiers with a predicate. These method group
    specifiers define all method qualifiers that can be used with this
    type of method combination.

    The car of each METHOD-GROUP-SPECIFIER is a symbol which names a
    variable. During the execution of the forms in the body of
    DEFINE-METHOD-COMBINATION, this variable is bound to a list of the
    methods in the method group. The methods in this list occur in the
    order specified by the :order option.

    If QUALIFIER-PATTERN is a symbol it must be *. A method matches a
    qualifier-pattern if the method's list of qualifiers is equal to
    the QUALIFIER-PATTERN (except that the symbol * in a
    qualifier-pattern matches anything). Thus a qualifier-pattern can
    be one of the following: the empty list, which matches unqualified
    methods; the symbol *, which matches all methods; a true list,
    which matches methods with the same number of qualifiers as the
    length of the list when each qualifier matches the corresponding
    list element; or a dotted list that ends in the symbol * (the *
    matches any number of additional qualifiers).

    Each applicable method is tested against the qualifier-patterns
    and predicates in left-to-right order. As soon as a
    qualifier-pattern matches or a predicate returns true, the method
    becomes a member of the corresponding method group and no further
    tests are made. Thus if a method could be a member of more than
    one method group, it joins only the first such group. If a method
    group has more than one qualifier-pattern, a method need only
    satisfy one of the qualifier-patterns to be a member of the group.

    The name of a predicate function can appear instead of
    qualifier-patterns in a method group specifier. The predicate is
    called for each method that has not been assigned to an earlier
    method group; it is called with one argument, the method's
    qualifier list. The predicate should return true if the method is
    to be a member of the method group. A predicate can be
    distinguished from a qualifier-pattern because it is a symbol
    other than nil or *.

    If there is an applicable method that does not fall into any
    method group, the function invalid-method-error is called.

    Method group specifiers can have keyword options following the
    qualifier patterns or predicate. Keyword options can be
    distinguished from additional qualifier patterns because they are
    neither lists nor the symbol *. The keyword options are as
    follows:

        The :DESCRIPTION option is used to provide a description of
        the role of methods in the method group. Programming
        environment tools use (APPLY #'FORMAT STREAM FORMAT-CONTROL
        (METHOD-QUALIFIERS METHOD)) to print this description, which
        is expected to be concise. This keyword option allows the
        description of a method qualifier to be defined in the same
        module that defines the meaning of the method qualifier. In
        most cases, FORMAT-CONTROL will not contain any format
        directives, but they are available for generality. If
        :DESCRIPTION is not supplied, a default description is
        generated based on the variable name and the qualifier
        patterns and on whether this method group includes the
        unqualified methods.

        The :ORDER option specifies the order of methods. The order
        argument is a form that evaluates to :MOST-SPECIFIC-FIRST or
        :MOST-SPECIFIC-LAST. If it evaluates to any other value, an
        error is signaled. If :ORDER is not supplied, it defaults to
        :MOST-SPECIFIC-FIRST.

        The :REQUIRED option specifies whether at least one method in
        this method group is required. If its value is true and the
        method group is empty (that is, no applicable methods match
        the qualifier patterns or satisfy the predicate), an error is
        signaled. If :REQUIRED is not supplied, it defaults to NIL.

    The use of method group specifiers provides a convenient syntax to
    select methods, to divide them among the possible roles, and to
    perform the necessary error checking. It is possible to perform
    further filtering of methods in the body forms by using normal
    list-processing operations and the functions METHOD-QUALIFIERS and
    INVALID-METHOD-ERROR. It is permissible to use SETQ on the
    variables named in the method group specifiers and to bind
    additional variables. It is also possible to bypass the method
    group specifier mechanism and do everything in the body
    forms. This is accomplished by writing a single method group with
    * as its only qualifier-pattern; the variable is then bound to a
    list of all of the applicable methods, in MOST-SPECIFIC-FIRST
    order.

    The BODY forms compute and return the form that specifies how the
    methods are combined, that is, the effective method. The effective
    method is evaluated in the null lexical environment augmented with
    a local macro definition for CALL-METHOD and with bindings named
    by symbols not accessible from the COMMON-LISP-USER package. Given
    a method object in one of the lists produced by the method group
    specifiers and a list of next methods, CALL-METHOD will invoke the
    method such that CALL-NEXT-METHOD has available the next methods.

    When an effective method has no effect other than to call a single
    method, some implementations employ an optimization that uses the
    single method directly as the effective method, thus avoiding the
    need to create a new effective method. This optimization is active
    when the effective method form consists entirely of an invocation
    of the CALL-METHOD macro whose first subform is a method object
    and whose second subform is NIL or unsupplied. Each
    DEFINE-METHOD-COMBINATION body is responsible for stripping off
    redundant invocations of PROGN, AND, MULTIPLE-VALUE-PROG1, and the
    like, if this optimization is desired.

    The list (:ARGUMENTS . LAMBDA-LIST) can appear before any
    declarations or documentation string. This form is useful when the
    method combination type performs some specific behavior as part of
    the combined method and that behavior needs access to the
    arguments to the generic function. Each parameter variable defined
    by lambda-list is bound to a form that can be inserted into the
    effective method. When this form is evaluated during execution of
    the effective method, its value is the corresponding argument to
    the generic function; the consequences of using such a form as the
    place in a setf form are undefined. Argument correspondence is
    computed by dividing the :arguments lambda-list and the generic
    function lambda-list into three sections: the required parameters,
    the optional parameters, and the keyword and rest parameters. The
    arguments supplied to the generic function for a particular call
    are also divided into three sections; the required arguments
    section contains as many arguments as the generic function has
    required parameters, the optional arguments section contains as
    many arguments as the generic function has optional parameters,
    and the keyword/rest arguments section contains the remaining
    arguments. Each parameter in the required and optional sections of
    the :arguments lambda-list accesses the argument at the same
    position in the corresponding section of the arguments. If the
    section of the :arguments lambda-list is shorter, extra arguments
    are ignored. If the section of the :arguments lambda-list is
    longer, excess required parameters are bound to forms that
    evaluate to nil and excess optional parameters are bound to their
    initforms. The keyword parameters and rest parameters in the
    :arguments lambda-list access the keyword/rest section of the
    arguments. If the :arguments lambda-list contains &key, it behaves
    as if it also contained &allow-other-keys.

    In addition, &whole var can be placed first in the :arguments
    lambda-list. It causes var to be bound to a form that evaluates to
    a list of all of the arguments supplied to the generic
    function. This is different from &rest because it accesses all of
    the arguments, not just the keyword/rest arguments.

    Erroneous conditions detected by the body should be reported with
    method-combination-error or invalid-method-error; these functions
    add any necessary contextual information to the error message and
    will signal the appropriate error.

    The body forms are evaluated inside of the bindings created by the
    lambda list and method group specifiers. Declarations at the head
    of the body are positioned directly inside of bindings created by
    the lambda list and outside of the bindings of the method group
    variables. Thus method group variables cannot be declared in this
    way. locally may be used around the body, however.

    Within the body forms, generic-function-symbol is bound to the
    generic function object.

    Documentation is attached as a documentation string to name (as
    kind method-combination) and to the method combination object.

    Note that two methods with identical specializers, but with
    different qualifiers, are not ordered by the algorithm described
    in Step 2 of the method selection and combination process
    described in Section 7.6.6 (Method Selection and
    Combination). Normally the two methods play different roles in the
    effective method because they have different qualifiers, and no
    matter how they are ordered in the result of Step 2, the effective
    method is the same. If the two methods play the same role and
    their order matters, an error is signaled. This happens as part of
    the qualifier pattern matching in define-method-combination.

If a DEFINE-METHOD-COMBINATION form appears as a top level form, the
compiler must make the method combination name be recognized as a
valid method combination name in subsequent defgeneric forms. However,
the method combination is executed no earlier than when the
DEFINE-METHOD-COMBINATION form is executed, and possibly as late as
the time that generic functions that use the method combination are
executed."""
        ## define_method_combination(name, method_group_specifiers, body,
        #                            arguments = None, generic_function = None)
        check_type(method_group_specifiers,
                  (list_,
                   (varituple_,
                    symbol,     # group name
                    (or_, (list_, (or_, star, list)),
                          function),
                    # the rest is actually a plist, but we cannot (yet) describe it
                    # in terms of a type.
                    (maybe_, (tuple_,
                              (eql_, keyword("description")),
                              str)),
                    (maybe_, (tuple_,
                              (eql_, keyword("order")),
                              (member_,
                               keyword("most-specific-first"),
                               keyword("most-specific-last"))))
                    (maybe_, (tuple_,
                              (eql_, keyword("required")),
                              (member_, t, nil))))))
        ### VARI-BIND, anyone?
        # def vari_bind(x, body):
        #         # don't lambda lists actually rule this, hands down?
        #         posnal, named = argspec_value_varivals(inspect.getfullargspec(body), x)
        #         return body()
        method_group = defstruct("method_group",
                                 "name",
                                 "qualifier_spec"
                                 "description",
                                 "most_specific_first",
                                 "required")
        groups = dict()
        for mgspec in method_group_specifiers:
                name, qualifier_spec = mgspec[:2]
                options = mgspec[2:]
                options_dict = _map_into_hash_star(lambda keyword, v: (symbol_name(keyword), v), options)
                (lambda DESCRIPTION = "Method group %s.",
                        REQUIRED = nil,
                        ORDER = keyword("most_specific_first"):
                        groups.update({name: method_group(name, qualifier_spec,
                                                          DESCRIPTION,
                                                          ORDER is keyword("most_specific_first"),
                                                          REQUIRED)}))(**options_dict)
        def method_combination(applicable_methods, *args, **keys):
               # The LAMBDA-LIST receives any arguments provided after the name of
               # the method combination type in the :METHOD-COMBINATION option to
               # DEFGENERIC.
               ## .. this means, that the EMF-computing form must be evaluated in
               ## the context, where the values bound by the lambda list are available.
               def method_qualifiers_match_pattern_p(qualifiers, pattern):
                       return (t if pattern is star else
                               qualifiers == pattern)
               grouped_methods = defaultdict(list)
               for method in applicable_methods:
                       qualifiers = method_qualifiers(method)
                       for group in groups.values():
                               qualifier_spec = group.qualifier_spec
                               ## qualifier_spec:
                               # (or_, (list_, (or_, star, list)),
                               #       function),
                               if ((listp(qualifier_spec) and
                                    some(curry(method_qualifiers_match_pattern_p, qualifiers),
                                               qualifier_spec))
                                   # must be an fbound symbol, as per the above TYPEP
                                   or qualifier_spec(qualifiers)):
                                       grouped_methods[group.name].append(method)
                       else:
                               ## XXX: ought to be a call to INVALID-METHOD-ERROR
                               error("Applicable method %s with qualifiers %s does not fall into any method group.",
                                     method, qualifiers)
               for group in groups.values():
                       if group.required and not grouped_methods[group.name]:
                               error("Method group %s requires at least one method.", group.name)
                       if not group.most_specific_first:
                               grouped_methods[group.name].reverse()
               # The effective method is evaluated in the null lexical
               # environment augmented with a local macro definition
               # for CALL-METHOD and with bindings named by symbols not
               # accessible from the COMMON-LISP-USER package.
               ## So: must bind group names and CALL-METHOD, which, in turn
               ## must bind CALL-NEXT-METHOD and NEXT-METHOD-P.  I presume.
               # BODY must, therefore, return some kind of an AST representation?
               # I guess, well, that we could play the CL games with that.
               # Yes, I'm thinking of real macros..
               # Maybe just arg it?
               body_args = dict(grouped_methods)
               def call_method(method, next_methods = None):
                       next_methods = _defaulted(next_methods, [])
                       # sounds like we ought to look up the compiled METHOD-LAMBDA?
                       # Given a method object in one of the lists produced
                       # by the method group specifiers and a list of next
                       # methods, CALL-METHOD will invoke the method such that
                       # CALL-NEXT-METHOD has available the next methods.
                       method_lambda
               body_args.update({ "call_method": })
               return body(**body_args)
        method_combination.__name__                    = the(symbol, name)
        method_combination.__method_group_specifiers__ = method_group_specifiers
        # Unregistered Issue SPECIAL-CASE-(APPLY #'FORMAT STREAM FORMAT-CONTROL (METHOD-QUALIFIERS METHOD))-NOT-IMPLEMENTED
        # 
        return method_combination

def make_method_lambda(generic_function, method, lambda_expression, environment):
# gfun -> method -> sexp -> env -> (sexp -> function)
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject.

The METHOD argument is a (possibly uninitialized) method metaobject.

The LAMBDA-EXPRESSION argument is a lambda expression.

The ENVIRONMENT argument is the same as the &environment argument to
macro expansion functions.

Values: This generic function returns two values. The first is a
lambda expression, the second is a list of initialization arguments
and values.

Purpose: This generic function is called to produce a lambda
expression which can itself be used to produce a method function for a
method and generic function with the specified classes. The generic
function and method the method function will be used with are not
required to be the given ones. Moreover, the METHOD metaobject may be
uninitialized.

Either the function COMPILE, the special form FUNCTION or the function
COERCE must be used to convert the lambda expression to a method
function. The method function itself can be applied to arguments with
APPLY or FUNCALL.

When a method is actually called by an effective method, its first
argument will be a list of the arguments to the generic function. Its
remaining arguments will be all but the first argument passed to
CALL-METHOD. By default, all method functions must accept two
arguments: the list of arguments to the generic function and the list
of next methods.

For a given generic function and method class, the applicable methods
on MAKE-METHOD-LAMBDA and COMPUTE-EFFECTIVE-METHOD must be consistent
in the following way: each use of CALL-METHOD returned by the method
on COMPUTE-EFFECTIVE-METHOD must have the same number of arguments,
and the method lambda returned by the method on MAKE-METHOD-LAMBDA
must accept a corresponding number of arguments.

Note that the system-supplied implementation of CALL-NEXT-METHOD is
not required to handle extra arguments to the method function. Users
who define additional arguments to the method function must either
redefine or forego CALL-NEXT-METHOD. (See the example below.)

When the method metaobject is created with MAKE-INSTANCE, the method
function must be the value of the :FUNCTION initialization
argument. The additional initialization arguments, returned as the
second value of this generic function, must also be passed in this
call to MAKE-INSTANCE."""
        _not_implemented()
        "Return an expression compileable (by whom? compute-effective-method?)
        down to a function, accepting (gf-arglist &rest (subseq c-m-args 1)),
        responsible to invoke the method and ."
        # (defmacro call-method (method &rest c-m-args)
        #   (apply method.function
        #          gf-arglist (subseq c-m-args 1)))

def compute_effective_method(generic_function, combin, applicable_methods):
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject.

The METHOD-COMBINATION argument is a method combination metaobject.

The METHODS argument is a list of method metaobjects.

Values: This generic function returns two values. The first is an
effective method, the second is a list of effective method options.

Purpose: This generic function is called to determine the effective
method from a sorted list of method metaobjects.

An effective method is a form that describes how the applicable
methods are to be combined. Inside of effective method forms are
CALL-METHOD forms which indicate that a particular method is to be
called. The arguments to the CALL-METHOD form indicate exactly how the
method function of the method should be called. (See
MAKE-METHOD-LAMBDA for more details about method functions.)

An effective method option has the same interpretation and syntax as
either the :ARGUMENTS or the :GENERIC-FUNCTION option in the long form
of DEFINE-METHOD-COMBINATION.

More information about the form and interpretation of effective
methods and effective method options can be found under the
description of the DEFINE-METHOD-COMBINATION macro in the CLOS
specification.

This generic function can be called by the user or the
implementation. It is called by discriminating functions whenever a
sorted list of applicable methods must be converted to an effective
method."""
        _not_implemented()

def compute_applicable_methods_using_classes(generic_function, classes):
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject.

The CLASSES argument is a list of class metaobjects.

Values: This generic function returns two values. The first is a
possibly empty list of method metaobjects. The second is either true
or false.

Purpose: This generic function is called to attempt to determine the
method applicability of a generic function given only the classes of
the required arguments.

If it is possible to completely determine the ordered list of
applicable methods based only on the supplied classes, this generic
function returns that list as its first value and true as its second
value. The returned list of method metaobjects is sorted by precedence
order, the most specific method coming first. If no methods are
applicable to arguments with the specified classes, the empty list and
true are returned.

If it is not possible to completely determine the ordered list of
applicable methods based only on the supplied classes, this generic
function returns an unspecified first value and false as its second
value.

When a generic function is invoked, the discriminating function must
determine the ordered list of methods applicable to the
arguments. Depending on the generic function and the arguments, this
is done in one of three ways: using a memoized value; calling
COMPUTE-APPLICABLE-METHODS-USING-CLASSES; or calling
COMPUTE-APPLICABLE-METHODS. (Refer to the description of
COMPUTE-DISCRIMINATING-FUNCTION for the details of this process.)

The following consistency relationship between
COMPUTE-APPLICABLE-METHODS-USING-CLASSES and
COMPUTE-APPLICABLE-METHODS must be maintained: for any given generic
function and set of arguments, if
COMPUTE-APPLICABLE-METHODS-USING-CLASSES returns a second value of
true, the first value must be equal to the value that would be
returned by a corresponding call to COMPUTE-APPLICABLE-METHODS. The
results are undefined if a portable method on either of these generic
functions causes this consistency to be violated.

The list returned by this generic function will not be mutated by the
implementation. The results are undefined if a portable program
mutates the list returned by this generic function."""
        return _compute_applicable_methods_using_types(generic_function,
                                                       _types_from_args(generic_function,
                                                                        classes,
                                                                        class_eq_))

def _types_from_args(generic_function, arguments, type_modifier = None):
        nreq, applyp, metatypes, nkeys, arg_info = _get_generic_fun_info(generic_function)
        # (declare (ignore applyp metatypes nkeys))
        types_rev = []
        for i in range(nreq):
                if not arguments:
                        error_need_at_least_n_args(generic_function_name(generic_function),
                                                   nreq)
                        arg = arguments.pop()
                        types_rev.append([type_modifier, arg] if type_modifier else
                                         arg)
        return values(types_rev, arg_info)

def _arg_info_precedence(arg_info: "lambda list, actually.."):
        return range(len(arg_info[0]))

def _compute_applicable_methods_using_types(generic_function, types_):
        definite_p, possibly_applicable_methods = t, []
        # Not safe against method list modifications by another thread!
        for method in generic_function_methods(generic_function):
                specls = method_specializers(method) # Was: if (consp method)
                types = list(types_)
                possibly_applicable_p, applicable_p = t, t
                for specl in specls:
                        _here("specl: %s", specl)
                        (specl_applicable_p,
                         specl_possibly_applicable_p) = specializer_applicable_using_type_p(specl, pop(types))
                        if not specl_applicable_p:
                                applicable_p = nil
                        if not specl_possibly_applicable_p:
                                possibly_applicable_p = nil
                                break
                if possibly_applicable_p:
                        if not applicable_p: definite_p = nil
                        possibly_applicable_methods[0:0] = [method]
                nreq, applyp, metatypes, nkeys, arg_info = _get_generic_fun_info(generic_function)
                # (declare (ignore nreq applyp metatypes nkeys))
                precedence = _arg_info_precedence(arg_info)
                return values(_sort_applicable_methods(precedence,
                                                       reversed(possibly_applicable_methods),
                                                       types),
                              definite_p)

def _type_from_specializer(specl):
        if specl is t:
                return t
        elif _tuplep(specl):
                if not member(car(specl), [class_, class_eq_, eql_]): # protoype_
                        error("%s is not a legal specializer type.", specl)
                return specl
        elif specializerp(specl): # Was a little bit more involved.
                return specializer_type(specl)
        else:
                error("%s is neither a type nor a specializer.", specl)

def specializer_applicable_using_type_p(specl, type):
        specl = _type_from_specializer(specl)
        if specl is t:
                return values(t, t)
        ## This is used by C-A-M-U-T and GENERATE-DISCRIMINATION-NET-INTERNAL,
        ## and has only what they need.
        return (values(nil, t) if atom(type) or car(type) is t else
                case(car(type),
                     # (and    (saut-and specl type)),
                     # (not    (saut-not specl type)),
                     # (class,      saut_class(specl, type)),
                     # (prototype  (saut-prototype specl type)),
                     (class_eq_,   lambda: saut_class_eq(specl, type)),
                     # (class-eq   (saut-class-eq specl type)),
                     # (eql    (saut-eql specl type)),
                     (t,       lambda: error("%s cannot handle the second argument %s.",
                                             "specializer-applicable-using-type-p",
                                             type))))

def saut_class_eq(specl, type):
       if car(specl) is eql_:
               return values(nil, type_of(specl[1]) is type[1])
       else:
               pred = case(car(specl),
                           (class_eq_, lambda: specl[1] is type[1]),
                           (class_,    lambda: (specl[1] is type[1] or
                                                memq(specl[1], cpl_or_nil(type[1])))))
               return values(pred, pred)

def _sort_applicable_methods(precedence, methods, types):
        def sorter(class1, class2, index):
                class_ = types[index] # Was: (type-class (nth index types))
                cpl = class_.__mro__  # Was: ..dependent on boot state
                return (class1 if memq(class2, memq(class1, cpl)) else # XXX: our MEMQ is horribly inefficient!
                        class2)
        return _sort_methods(methods,
                             precedence,
                             sorter)

def _sort_methods(methods, precedence, compare_classes_function):
        def sorter(method1, method2):
                for index in precedence:
                        specl1 = nth(index, method_specializers(method1)) # XXX: Was (if (listp method1)
                        specl2 = nth(index, method_specializers(method2)) # XXX: Was (if (listp method2)
                        order  = _order_specializers(specl1, specl2, index, compare_classes_function)
                        if order:
                                return order is specl1
        return stable_sort(methods, sorter)

def _order_specializers(specl1, specl2, index, compare_classes_function):
        type1 = specializer_type(specl1) # Was: (if (eq **boot-state** 'complete)
        type2 = specializer_type(specl2) # Was: (if (eq **boot-state** 'complete)
        return ([]     if specl1 is specl1 else
                specl2 if atom(type1)      else # is t?
                specl1 if atom(type2)      else # is t?
                case(car(type1),
                     (type_, lambda: case(car(type2),
                                          (type_, compare_classes_function(specl1, specl2, index)),
                                          (t, specl2))),
                     # (prototype (case (car type2)
                     #             (class (funcall compare-classes-function
                     #                             specl1 specl2 index))
                     #             (t specl2)))
                     # (class-eq (case (car type2)
                     #             (eql specl2)
                     #             ;; FIXME: This says that all CLASS-EQ
                     #             ;; specializers are equally specific, which
                     #             ;; is fair enough because only one CLASS-EQ
                     #             ;; specializer can ever be appliable.  If
                     #             ;; ORDER-SPECIALIZERS should only ever be
                     #             ;; called on specializers from applicable
                     #             ;; methods, we could replace this with a BUG.
                     #             (class-eq nil)
                     #             (class type1)))
                     (eql_,  lambda: case(car(type2),
                                          # similarly
                                          (eql_, []),
                                          (t, specl1)))))

def compute_applicable_methods(generic_function, arguments):
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject.

The ARGUMENTS argument is a list of objects.

Values: This generic function returns a possibly empty list of method
metaobjects.

Purpose: This generic function determines the method applicability of
a generic function given a list of required ARGUMENTS. The returned
list of method metaobjects is sorted by precedence order with the most
specific method appearing first. If no methods are applicable to the
supplied arguments the empty list is returned.

When a generic function is invoked, the discriminating function must
determine the ordered list of methods applicable to the
arguments. Depending on the generic function and the arguments, this
is done in one of three ways: using a memoized value; calling
COMPUTE-APPLICABLE-METHODS-USING-CLASSES; or calling
COMPUTE-APPLICABLE-METHODS. (Refer to the description of
COMPUTE-DISCRIMINATING-FUNCTION for the details of this process.)

The arguments argument is permitted to contain more elements than the
generic function accepts required arguments; in these cases the extra
arguments will be ignored. An error is signaled if arguments contains
fewer elements than the generic function accepts required arguments.

The list returned by this generic function will not be mutated by the
implementation. The results are undefined if a portable program
mutates the list returned by this generic function."""
        return _compute_applicable_methods_using_types(generic_function,
                                                       _types_from_args(generic_function,
                                                                        arguments,
                                                                        eql_))

__sealed_classes__ = set([object,
                          int, bool, float, complex,
                          str, tuple, bytes,
                          list, bytearray,
                          set, frozenset,
                          dict,
                          function_,
                          stream,
                          BaseException, Exception] +
                         mapcar(type_of,
                                [None,           # NoneType
                                 Ellipsis,       # ellipsis
                                 NotImplemented, # NotImplementedType
                                 int,            # type
                                 "".find,        # builtin_function_or_method
                                 ast,            # module
                                 sys.stdin,      # _io.TextIOWrapper
                                 car.__code__,   # code object
                                 _this_frame(),  # frame
                                 ]))

def _class_sealed_p(x):
        return x in __sealed_classes__

## A sealed metaclass?
def _seal_class(x):
        _not_implemented()
        # How do we forbid class precedence list modification?
        __sealed_classes__.add(x)

def compute_discriminating_function(generic_function) -> (lambda *args, **keys: None):
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject.

Values: The value returned by this generic function is a function.

Purpose: This generic function is called to determine the
discriminating function for a generic function. When a generic
function is called, the installed discriminating function is called
with the full set of arguments received by the generic function, and
must implement the behavior of calling the generic function:
determining the ordered set of applicable methods, determining the
effective method, and running the effective method.

To determine the ordered set of applicable methods, the discriminating
function first calls COMPUTE-APPLICABLE-METHODS-USING-CLASSES. If
COMPUTE-APPLICABLE-METHODS-USING-CLASSES returns a second value of
false, the discriminating function then calls
COMPUTE-APPLICABLE-METHODS.

When COMPUTE-APPLICABLE-METHODS-USING-CLASSES returns a second value
of true, the discriminating function is permitted to memoize the first
returned value as follows. The discriminating function may reuse the
list of applicable methods without calling
COMPUTE-APPLICABLE-METHODS-USING-CLASSES again provided that:

    (i) the generic function is being called again with required
        arguments which are instances of the same classes,
    (ii) the generic function has not been reinitialized,
    (iii) no method has been added to or removed from the
          generic function,
    (iv) for all the specializers of all the generic function's
         methods which are classes, their class precedence lists
         have not changed and
    (v) for any such memoized value, the class precedence list of
        the class of each of the required arguments has not changed.

Determination of the effective method is done by calling
COMPUTE-EFFECTIVE-METHOD. When the effective method is run, each
method's function is called, and receives as arguments: (i) a list of
the arguments to the generic function, and (ii) whatever other
arguments are specified in the call-method form indicating that the
method should be called. (See MAKE-METHOD-LAMBDA for more information
about how method functions are called.)

The generic function COMPUTE-DISCRIMINATING-FUNCTION is called, and
its result installed, by ADD-METHOD, REMOVE-METHOD,
INITIALIZE-INSTANCE and REINITIALIZE-INSTANCE."""
        return _do_compute_discriminating_function(
                 generic_function.__name__,
                 generic_function.__lambda_list__,
                 generic_function.__applicable_method_cache__,
                 generic_function.__code__.co_filename,
                 generic_function.__code__.co_lineno)

def error_need_at_least_n_args(function, n):
        error("The function %s requires at least %d arguments.", function, n)

def _do_compute_discriminating_function(function_name,
                                        lambda_list,
                                        applicable_method_cache,
                                        filename,
                                        lineno) -> (lambda *args, **keys: None):
        fixed, optional, args, keyword, keys = lambda_list
        nfixed = len(fixed)
        def dfun_compute_applicable_methods(generic_function, args):
                if len(args) < nfixed:
                        error_need_at_least_n_args(function_name, nfixed)
                dispatch_args      = args[:nfixed]
                dispatch_arg_types = tuple(type(x) for x in dispatch_args)
                # The discriminating function may reuse the
                # list of applicable methods without calling
                # COMPUTE-APPLICABLE-METHODS-USING-CLASSES again provided that:
                # (iv) for all the specializers of all the generic function's
                #      methods which are classes, their class precedence lists
                #      have not changed and
                # XXX: not_implemented()
                # Issue COMPUTE-DISCRIMINATING-FUNCTION-REQUIREMENT-4-UNCLEAR-NOT-IMPLEMENTED
                # (v) for any such memoized value, the class precedence list of
                #     the class of each of the required arguments has not changed.
                unsealed_classes = set(x for x in dispatch_arg_types if not _class_sealed_p(x))
                applicable_method_cache_key = dispatch_arg_types + reduce(lambda acc, x: acc + x.__mro__,
                                                                          sorted(unsealed_classes, key = lambda type: type.__name__),
                                                                          tuple())
                # We pay the high price of (iv) and (v), because we can't hook
                # into the Python's object system.
                applicable, hit = gethash(applicable_method_cache_key, applicable_method_cache)
                if hit:
                        # The discriminating function may reuse the
                        # list of applicable methods without calling
                        # COMPUTE-APPLICABLE-METHODS-USING-CLASSES again provided that:
                        # (i) the generic function is being called again with required
                        #     arguments which are instances of the same classes,
                        return applicable
                _here("gf: %s, ll: %s", generic_function, generic_function.__lambda_list__)
                methods, okayp = compute_applicable_methods_using_classes(generic_function,
                                                                          dispatch_arg_types)
                if okayp:
                        applicable_method_cache[applicable_method_cache_key] = methods
                        return methods
                else:
                        return compute_applicable_methods(generic_function,
                                                          dispatch_args)
        ## compute_discriminating_function(generic_function, function_name, lambda_list,
        ##                                 fixed, optional, args, keyword, keys,
        ##                                 nfixed):
        new_dfun_ast = _ast_functiondef(
            function_name,
            lambda_list,
            # How do we access methods themselves?
            [_ast_return(
                 _ast_funcall(_ast_funcall("compute_effective_method",
                                           [_ast_name(function_name),
                                            None, # method combination
                                            _ast_funcall("dfun_compute_applicable_methods",
                                                         [_ast_name(function_name),
                                                          mapcar(_ast_name, fixed)])]),
                              mapcar(_ast_name, fixed + mapcar(car, optional)),
                              _map_into_hash_star(lambda key, default: (key, _ast_name(default)),
                                                   keyword),
                              starargs = _ast_name(args) if args else None,
                              kwargs   = _ast_name(keys) if keys else None))])
        if True:
                import more_ast # Shall we concede, and import it all?
                format(t, "; generic function '%s':\n%s",
                       function_name, more_ast.pp_ast_as_code(new_dfun_ast))
        env = dict(compute_effective_method        = compute_effective_method,
                   _find_symbol_or_fail            = _find_symbol_or_fail,
                   dfun_compute_applicable_methods = dfun_compute_applicable_methods)
        return _ast_compiled_name(
                    function_name,
                    new_dfun_ast,
                    filename = _defaulted(filename, ""),
                    lineno   = lineno,
                    globals_ = env,
                    locals_  = env)

def ensure_generic_function_using_class(generic_function, function_name,
                                        argument_precedence_order = None,
                                        declarations = None,
                                        documentation = None,
                                        generic_function_class = None,
                                        lambda_list = None,
                                        method_class = None,
                                        method_combination = None,
                                        name = nil,
                                        # incompatible..
                                        filename = None,
                                        lineno = None,
                                        **keys):
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject or NIL.

The FUNCTION-NAME argument is a symbol or a list of the form (SETF
SYMBOL).

The :GENERIC-FUNCTION-CLASS argument is a class metaobject or a class
name. If it is not supplied, it defaults to the class named
STANDARD-GENERIC-FUNCTION. If a class name is supplied, it is
interpreted as the class with that name. If a class name is supplied,
but there is no such class, an error is signaled.

For the interpretation of additional keyword arguments, see
``Initialization of Generic Function Metaobjects''.

Values: The result is a generic function metaobject.

Purpose: The generic function ENSURE-GENERIC-FUNCTION-USING-CLASS is
called to define or modify the definition of a globally named generic
function. It is called by the ENSURE-GENERIC-FUNCTION function. It can
also be called directly.

The first step performed by this generic function is to compute the
set of initialization arguments which will be used to create or
reinitialize the globally named generic function. These initialization
arguments are computed from the full set of keyword arguments received
by this generic function as follows:

    The :GENERIC-FUNCTION-CLASS argument is not included in the
    initialization arguments.

    If the :METHOD-CLASS argument was received by this generic
    function, it is converted into a class metaobject. This is done by
    looking up the class name with FIND-CLASS. If there is no such
    class, an error is signalled.

    All other keyword arguments are included directly in the
    initialization arguments.

If the GENERIC-FUNCTION argument is NIL, an instance of the class
specified by the :GENERIC-FUNCTION-CLASS argument is created by
calling MAKE-INSTANCE with the previously computed initialization
arguments. The function name FUNCTION-NAME is set to name the generic
function. The newly created generic function metaobject is returned.

If the class of the GENERIC-FUNCTION argument is not the same as the
class specified by the :GENERIC-FUNCTION-CLASS argument, an error is
signaled.

Otherwise the generic function GENERIC-FUNCTION is redefined by
calling the REINITIALIZE-INSTANCE generic function with
GENERIC-FUNCTION and the initialization arguments. The
GENERIC-FUNCTION argument is then returned.

Unless there is a specific note to the contrary, then during
reinitialization, if an initialization argument is not supplied, the
previously stored value is left unchanged.

The :ARGUMENT-PRECEDENCE-ORDER argument is a list of symbols.

An error is signaled if this argument appears but the :LAMBDA-LIST
argument does not appear. An error is signaled if this value is not a
proper list or if this value is not a permutation of the symbols from
the required arguments part of the :LAMBDA-LIST initialization
argument.

When the generic function is being initialized or reinitialized, and
this argument is not supplied, but the :LAMBDA-LIST argument is
supplied, this value defaults to the symbols from the required
arguments part of the :LAMBDA-LIST argument, in the order they appear
in that argument. If neither argument is supplied, neither are
initialized (see the description of :LAMBDA-LIST.)

The :DECLARATIONS argument is a list of declarations.

An error is signaled if this value is not a proper list or if each of
its elements is not a legal declaration.

When the generic function is being initialized, and this argument is
not supplied, it defaults to the empty list.

The :DOCUMENTATION argument is a string or NIL.

An error is signaled if this value is not a string or NIL.

If the generic function is being initialized, this argument defaults
to NIL.

The :LAMBDA-LIST argument is a lambda list.

An error is signaled if this value is not a proper generic function
lambda list.

When the generic function is being initialized, and this argument is
not supplied, the generic function's lambda list is not
initialized. The lambda list will be initialized later, either when
the first method is added to the generic function, or a later
reinitialization of the generic function.

The :METHOD-COMBINATION argument is a method combination metaobject.

The :METHOD-CLASS argument is a class metaobject.

An error is signaled if this value is not a subclass of the class
METHOD.

When the generic function is being initialized, and this argument is
not supplied, it defaults to the class STANDARD-METHOD.

The :NAME argument is an object.

If the generic function is being initialized, this argument defaults
to NIL."""
        lambda_list = _defaulted(lambda_list, ([], [], nil, [], nil))
        fixed, optional, args, keyword, keys = lambda_list
        if some(lambda x: x[1] is not None, list(optional) + list(keyword)):
                error("Generic function arglist cannot specify default parameter values.")
        if (argument_precedence_order or declarations or
            generic_function_class or method_class or method_combination):
                error("This is not CLOS.  Yet.  (Read: ARGUMENT-PRECEDENCE-ORDER, DECLARE, ENVIRONMENT, GENERIC-FUNCTION-CLASS, METHOD-CLASS and METHOD-COMBINATION keyword options are not supported.)")
        gfun, presentp = gethash(function_name, globals())
        applicable_method_cache = dict()
        if (not presentp or                       # New generic function?..
            lambda_list != gfun.__lambda_list__): # ..or an incompatible redefinition?
                new_gfun = _do_compute_discriminating_function(function_name,
                                                               lambda_list,
                                                               applicable_method_cache,
                                                               filename,
                                                               _defaulted(lineno, 0))
                # new_gfun.__applicable_method_cache__ = ..see below
                new_gfun.__methods__            = dict() # keyed by type specifier tuples... busted?
                new_gfun.__lambda_list__        = lambda_list
                globals()[function_name] = gfun = new_gfun
        else:
                gfun.__code__.co_filename    = filename
                gfun.__code__.co_firstlineno = lineno
        gfun.__doc__ = _defaulted(documentation, gfun.__doc__)
        # The discriminating function may reuse the
        # list of applicable methods without calling
        # COMPUTE-APPLICABLE-METHODS-USING-CLASSES again provided that:
        # (ii) the generic function has not been reinitialized,
        gfun.__applicable_method_cache__ = applicable_method_cache # (list_, type_) -> list;  busted on every defmethod invocation
        return gfun

def generic_function_argument_precedence_order(x): return _not_implemented()
def generic_function_declarations(x):              return _not_implemented()
def generic_function_lambda_list(x):               return x.__lambda_list__
def generic_function_method_combination(x):        return _not_implemented()
def generic_function_method_class(x):              return _not_implemented()
def generic_function_name(x):                      return x.__name__

def ensure_generic_function(function_name, **keys):
        """Arguments:

The FUNCTION-NAME argument is a symbol or a list of the form (SETF
SYMBOL).

Some of the keyword arguments accepted by this function are actually
processed by ENSURE-GENERIC-FUNCTION-USING-CLASS, others are processed
during initialization of the generic function metaobject (as described
in the section called ``Initialization of Generic Function
Metaobjects'').

Values: The result is a generic function metaobject.

Purpose: This function is called to define a globally named generic
function or to specify or modify options and declarations that pertain
to a globally named generic function as a whole. It can be called by
the user or the implementation.

It is the functional equivalent of DEFGENERIC, and is called by the
expansion of the DEFGENERIC and DEFMETHOD macros.

The behavior of this function is actually implemented by the generic
function ENSURE-GENERIC-FUNCTION-USING-CLASS. When
ENSURE-GENERIC-FUNCTION is called, it immediately calls
ENSURE-GENERIC-FUNCTION-USING-CLASS and returns that result as its
own.

The first argument to ENSURE-GENERIC-FUNCTION-USING-CLASS is computed
as follows:

    If FUNCTION-NAME names a non-generic function, a macro, or a
    special form, an error is signaled.

    If FUNCTION-NAME names a generic function, that generic function
    metaobject is used.

    Otherwise, NIL is used.

The second argument is FUNCTION-NAME. The remaining arguments are the
complete set of keyword arguments received by
ENSURE-GENERIC-FUNCTION."""
        x, definedp = gethash(the(str, function_name), globals(), nil)
        if functionp(x) and not _generic_function_p(x):
                error("%s already names an ordinary function.", function_name)
        return ensure_generic_function_using_class(x, function_name, **keys)

def defgeneric(fn):
        return ensure_generic_function(fn.__name__,
                                       documentation = fn.__doc__,
                                       lambda_list   = _argspec_lambda_spec(inspect.getfullargspec(fn)),
                                       filename      = fn.__code__.co_filename,
                                       lineno        = fn.__code__.co_firstlineno)

def add_method(generic_function, method):
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject.

The METHOD argument is a method metaobject.

Values: The GENERIC-FUNCTION argument is returned.

Purpose: This generic function associates an unattached method with a
generic function.

An error is signaled if the lambda list of the method is not congruent
with the lambda list of the generic function. An error is also
signaled if the method is already associated with some other generic
function.

If the given method agrees with an existing method of the generic
function on parameter specializers and qualifiers, the existing method
is removed by calling REMOVE-METHOD before the new method is
added. See the section of the CLOS Specification called ``Agreement on
Parameter Specializers and Qualifiers'' for a definition of agreement
in this context.

Associating the method with the generic function then proceeds in four
steps: (i) add METHOD to the set returned by GENERIC-FUNCTION-METHODS
and arrange for METHOD-GENERIC-FUNCTION to return
GENERIC-FUNCTION; (ii) call ADD-DIRECT-METHOD for each of the method's
specializers; (iii) call COMPUTE-DISCRIMINATING-FUNCTION and install
its result with SET-FUNCALLABLE-INSTANCE-FUNCTION; and (iv) update the
dependents of the generic function.

The generic function ADD-METHOD can be called by the user or the
implementation."""
        # The discriminating function may reuse the
        # list of applicable methods without calling
        # COMPUTE-APPLICABLE-METHODS-USING-CLASSES again provided that:
        # (iii) no method has been added to or removed from the
        #       generic function,
        # XXX: validate GF
        lambda_list = _argspec_lambda_spec(inspect.getfullargspec(the(function_, method)))
        fixed, optional, args, keyword, keys = lambda_list
        method.__qualifiers__ = []
        method.__lambda_list__ = lambda_list
        method.__specializers__ = tuple(make_method_specializers(
                        mapcar(lambda name: gethash(name, method.__annotations__, t)[0],
                               fixed)))
        method.__slot_definition__ = None
        generic_function.__methods__[method.__specializers__] = method
        generic_function.__applicable_method_cache__ = dict()
        return generic_function

def method_qualifiers(x):      return x.__qualifiers___
def method_lambda_list(x):     return x.__lambda_list__
def method_specializers(x):    return x.__specializers__
def method_function(x):        return x
def method_slot_definition(x): return x.__slot_definition__
def method_documentation(x):   return x.__doc__

def make_method_specializers(specializers):
        def parse(name):
                return (# name                                                    if specializerp(name) else
                        name                                                      if name is t          else
                                                                  # ..special-case, since T isn't a type..
                        name                                                      if typep(name, type_) else
                                                                  # Was: ((symbolp name) `(find-class ',name))
                        ecase(car(name),
                              (eql_,      lambda: intern_eql_specializer(name[1])),
                              (class_eq_, lambda: class_eq_specializer(name[1]))) if _tuplep(name)      else
                        ## Was: FIXME: Document CLASS-EQ specializers.
                        error("%s is not a valid parameter specializer name.", name))
        return mapcar(parse, specializers)

def remove_method(generic_function, method):
        """Arguments:

The GENERIC-FUNCTION argument is a generic function metaobject.

The METHOD argument is a method metaobject.

Values: The GENERIC-FUNCTION argument is returned.

Purpose: This generic function breaks the association between a
generic function and one of its methods.

No error is signaled if the method is not among the methods of the
generic function.

Breaking the association between the method and the generic function
proceeds in four steps: (i) remove method from the set returned by
GENERIC-FUNCTION-METHODS and arrange for METHOD-GENERIC-FUNCTION to
return NIL; (ii) call REMOVE-DIRECT-METHOD for each of the method's
specializers; (iii) call COMPUTE-DISCRIMINATING-FUNCTION and install
its result with SET-FUNCALLABLE-INSTANCE-FUNCTION; and (iv) update the
dependents of the generic function.

The generic function REMOVE-METHOD can be called by the user or the
implementation."""
        # The discriminating function may reuse the
        # list of applicable methods without calling
        # COMPUTE-APPLICABLE-METHODS-USING-CLASSES again provided that:
        # (iii) no method has been added to or removed from the
        #       generic function,
        generic_function.__applicable_method_cache__ = dict()
        return generic_function

def defmethod(fn):
        """defmethod function-name {method-qualifier}* specialized-lambda-list [[declaration* | documentation]] form*

=> new-method

function-name::= {symbol | (setf symbol)}

method-qualifier::= non-list

specialized-lambda-list::= ({var | (var parameter-specializer-name)}* 
                            [&optional {var | (var [initform [supplied-p-parameter] ])}*] 
                            [&rest var] 
                            [&key{var | ({var | (keywordvar)} [initform [supplied-p-parameter] ])}*
                                 [&allow-other-keys] ] 
                            [&aux {var | (var [initform] )}*] ) 
parameter-specializer-name::= symbol | (eql eql-specializer-form)

Arguments and Values:

declaration---a declare expression; not evaluated.

documentation---a string; not evaluated.

var---a variable name.

eql-specializer-form---a form.

Form---a form.

Initform---a form.

Supplied-p-parameter---variable name.

new-method---the new method object.

Description:

The macro DEFMETHOD defines a method on a generic function.

If (FBOUNDP FUNCTION-NAME) is NIL, a generic function is created with
default values for the argument precedence order (each argument is
more specific than the arguments to its right in the argument list),
for the generic function class (the class STANDARD-GENERIC-FUNCTION),
for the method class (the class STANDARD-METHOD), and for the method
combination type (the standard method combination type). The lambda
list of the generic function is congruent with the lambda list of the
method being defined; if the DEFMETHOD form mentions keyword
arguments, the lambda list of the generic function will mention
..... key (but no keyword arguments). If FUNCTION-NAME names an
ordinary function, a macro, or a special operator, an error is
signaled.

If a generic function is currently named by FUNCTION-NAME, the lambda
list of the method must be congruent with the lambda list of the
generic function. If this condition does not hold, an error is
signaled. For a definition of congruence in this context, see Section
7.6.4 (Congruent Lambda-lists for all Methods of a Generic Function).

Each METHOD-QUALIFIER argument is an object that is used by method
combination to identify the given method. The method combination type
might further restrict what a method qualifier can be. The standard
method combination type allows for unqualified methods and methods
whose sole qualifier is one of the keywords :BEFORE, :AFTER,
or :AROUND.

The SPECIALIZED-LAMBDA-LIST argument is like an ordinary lambda list
except that the names of required parameters can be replaced by
specialized parameters. A specialized parameter is a list of the
form (VAR PARAMETER-SPECIALIZER-NAME). Only required parameters can be
specialized. If PARAMETER-SPECIALIZER-NAME is a symbol it names a
class; if it is a list, it is of the form (EQL
EQL-SPECIALIZER-FORM). The parameter specializer name (EQL
EQL-SPECIALIZER-FORM) indicates that the corresponding argument must
be EQL to the object that is the value of EQL-SPECIALIZER-FORM for the
method to be applicable. The EQL-SPECIALIZER-FORM is evaluated at the
time that the expansion of the DEFMETHOD macro is evaluated. If no
parameter specializer name is specified for a given required
parameter, the parameter specializer defaults to the class t. For
further discussion, see Section 7.6.2 (Introduction to Methods).

The FORM arguments specify the method body. The body of the method is
enclosed in an implicit block. If FUNCTION-NAME is a symbol, this
block bears the same name as the generic function. If FUNCTION-NAME is
a list of the form (SETF SYMBOL), the name of the block is symbol.

The class of the method object that is created is that given by the
method class option of the generic function on which the method is
defined.

If the generic function already has a method that agrees with the
method being defined on parameter specializers and qualifiers,
DEFMETHOD replaces the existing method with the one now being
defined. For a definition of agreement in this context. see Section
7.6.3 (Agreement on Parameter Specializers and Qualifiers).

The parameter specializers are derived from the parameter specializer
names as described in Section 7.6.2 (Introduction to Methods).

The expansion of the DEFMETHOD macro ``refers to'' each specialized
parameter (see the description of ignore within the description of
declare). This includes parameters that have an explicit parameter
specializer name of T. This means that a compiler warning does not
occur if the body of the method does not refer to a specialized
parameter, while a warning might occur if the body of the method does
not refer to an unspecialized parameter. For this reason, a parameter
that specializes on T is not quite synonymous with an unspecialized
parameter in this context.

Declarations at the head of the method body that apply to the method's
lambda variables are treated as bound declarations whose scope is the
same as the corresponding bindings.

Declarations at the head of the method body that apply to the
functional bindings of CALL-NEXT-METHOD or NEXT-METHOD-P apply to
references to those functions within the method body forms. Any outer
bindings of the function names CALL-NEXT-METHOD and NEXT-METHOD-P, and
declarations associated with such bindings are shadowed[2] within the
method body forms.

The scope of free declarations at the head of the method body is the
entire method body, which includes any implicit local function
definitions but excludes initialization forms for the lambda
variables.

DEFMETHOD is not required to perform any COMPILE-TIME side effects. In
particular, the methods are not installed for invocation during
compilation. An implementation may choose to store information about
the generic function for the purposes of COMPILE-TIME
error-checking (such as checking the number of arguments on calls, or
noting that a definition for the function name has been seen).

Documentation is attached as a documentation string to the method
object."""
## 7.6.2 Introduction to Methods
#
# Methods define the class-specific or identity-specific behavior and
# operations of a generic function.
#
# A method object is associated with code that implements the method's
# behavior, a sequence of parameter specializers that specify when the
# given method is applicable, a lambda list, and a sequence of
# qualifiers that are used by the method combination facility to
# distinguish among methods.
#
# A method object is not a function and cannot be invoked as a
# function. Various mechanisms in the object system take a method
# object and invoke its method function, as is the case when a generic
# function is invoked. When this occurs it is said that the method is
# invoked or called.
#
# A method-defining form contains the code that is to be run when the
# arguments to the generic function cause the method that it defines
# to be invoked. When a method-defining form is evaluated, a method
# object is created and one of four actions is taken:
#
# * If a generic function of the given name already exists and if a
#   method object already exists that agrees with the new one on
#   parameter specializers and qualifiers, the new method object
#   replaces the old one. For a definition of one method agreeing with
#   another on parameter specializers and qualifiers, see Section
#   7.6.3 (Agreement on Parameter Specializers and Qualifiers).
#
# * If a generic function of the given name already exists and if
#   there is no method object that agrees with the new one on
#   parameter specializers and qualifiers, the existing generic
#   function object is modified to contain the new method object.
#
# * If the given name names an ordinary function, a macro, or a
#   special operator, an error is signaled.
#
# * Otherwise a generic function is created with the method specified
#   by the method-defining form.
#
# If the lambda list of a new method is not congruent with the lambda
# list of the generic function, an error is signaled. If a
# method-defining operator that cannot specify generic function
# options creates a new generic function, a lambda list for that
# generic function is derived from the lambda list of the method in
# the method-defining form in such a way as to be congruent with
# it. For a discussion of congruence, see Section 7.6.4 (Congruent
# Lambda-lists for all Methods of a Generic Function).
#
# Each method has a specialized lambda list, which determines when
# that method can be applied. A specialized lambda list is like an
# ordinary lambda list except that a specialized parameter may occur
# instead of the name of a required parameter. A specialized parameter
# is a list (variable-name parameter-specializer-name), where
# parameter-specializer-name is one of the following:
#
# a symbol
#
#     denotes a parameter specializer which is the class named by that
#     symbol.
#
# a class
#
#     denotes a parameter specializer which is the class itself.
#
# (eql form)
#
#     denotes a parameter specializer which satisfies the type
#     specifier (eql object), where object is the result of evaluating
#     form. The form form is evaluated in the lexical environment in
#     which the method-defining form is evaluated. Note that form is
#     evaluated only once, at the time the method is defined, not each
#     time the generic function is called.
#
# Parameter specializer names are used in macros intended as the
# user-level interface (defmethod), while parameter specializers are
# used in the functional interface.
#
# Only required parameters may be specialized, and there must be a
# parameter specializer for each required parameter. For notational
# simplicity, if some required parameter in a specialized lambda list
# in a method-defining form is simply a variable name, its parameter
# specializer defaults to the class t.
#
# Given a generic function and a set of arguments, an applicable
# method is a method for that generic function whose parameter
# specializers are satisfied by their corresponding arguments. The
# following definition specifies what it means for a method to be
# applicable and for an argument to satisfy a parameter specializer.
#
# Let <A1, ..., An> be the required arguments to a generic function in
# order. Let <P1, ..., Pn> be the parameter specializers corresponding
# to the required parameters of the method M in order. The method M is
# applicable when each Ai is of the type specified by the type
# specifier Pi. Because every valid parameter specializer is also a
# valid type specifier, the function typep can be used during method
# selection to determine whether an argument satisfies a parameter
# specializer.
#
# A method all of whose parameter specializers are the class t is
# called a default method; it is always applicable but may be shadowed
# by a more specific method.
#
# Methods can have qualifiers, which give the method combination
# procedure a way to distinguish among methods. A method that has one
# or more qualifiers is called a qualified method. A method with no
# qualifiers is called an unqualified method. A qualifier is any
# non-list. The qualifiers defined by the standardized method
# combination types are symbols.
#
# In this specification, the terms ``primary method'' and ``auxiliary
# method'' are used to partition methods within a method combination
# type according to their intended use. In standard method
# combination, primary methods are unqualified methods and auxiliary
# methods are methods with a single qualifier that is one of :around,
# :before, or :after. Methods with these qualifiers are called around
# methods, before methods, and after methods, respectively. When a
# method combination type is defined using the short form of
# define-method-combination, primary methods are methods qualified
# with the name of the type of method combination, and auxiliary
# methods have the qualifier :around. Thus the terms ``primary
# method'' and ``auxiliary method'' have only a relative definition
# within a given method combination type.
#
## 7.6.3 Agreement on Parameter Specializers and Qualifiers
#
# Two methods are said to agree with each other on parameter
# specializers and qualifiers if the following conditions hold:
#
# 1. Both methods have the same number of required parameters. Suppose
# the parameter specializers of the two methods are P1,1...P1,n and
# P2,1...P2,n.
#
# 2. For each 1<=i<=n, P1,i agrees with P2,i. The parameter
# specializer P1,i agrees with P2,i if P1,i and P2,i are the same
# class or if P1,i=(eql object1), P2,i=(eql object2), and (eql object1
# object2). Otherwise P1,i and P2,i do not agree.
#
# 3. The two lists of qualifiers are the same under equal.
#
## 7.6.4 Congruent Lambda-lists for all Methods of a Generic Function
#
# These rules define the congruence of a set of lambda lists,
# including the lambda list of each method for a given generic
# function and the lambda list specified for the generic function
# itself, if given.
#
# 1. Each lambda list must have the same number of required
# parameters.
#
# 2. Each lambda list must have the same number of optional
# parameters. Each method can supply its own default for an optional
# parameter.
#
# 3. If any lambda list mentions &rest or &key, each lambda list must
# mention one or both of them.
#
# 4. If the generic function lambda list mentions &key, each method
# must accept all of the keyword names mentioned after &key, either by
# accepting them explicitly, by specifying &allow-other-keys, or by
# specifying &rest but not &key. Each method can accept additional
# keyword arguments of its own. The checking of the validity of
# keyword names is done in the generic function, not in each method. A
# method is invoked as if the keyword argument pair whose name is
# :allow-other-keys and whose value is true were supplied, though no
# such argument pair will be passed.
#
# 5. The use of &allow-other-keys need not be consistent across lambda
# lists. If &allow-other-keys is mentioned in the lambda list of any
# applicable method or of the generic function, any keyword arguments
# may be mentioned in the call to the generic function.
#
# 6. The use of &aux need not be consistent across methods.
#
# If a method-defining operator that cannot specify generic function
# options creates a generic function, and if the lambda list for the
# method mentions keyword arguments, the lambda list of the generic
# function will mention &key (but no keyword arguments).
        gfun, definedp = gethash(fn.__name__, globals())
        if not definedp:
                gfun = ensure_generic_function(fn.__name__)
        methfun_lambda, methfun_args = make_method_lambda(gfun, class_prototype(class_of()),
                                                          fn, <env>)
        method = make_instance(standard_method,
                               function = _not_implemented("somehow compile", methfun_lambda),
                               **methfun_args)
        add_method(gfun, method)
        return method

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
# def read_sequence(sequence, stream, start = 0, end = None):
#         return 0
#
# class _deadline_timeout(condition)
# def _with_deadline(timeout, body)
