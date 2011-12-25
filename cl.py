###
### Ring 0.
###
def _python_builtins_dictionary():
        return _builtins.getattr(__builtins__, "__dict__", __builtins__)

def _setf_python_builtins_dictionary(value):
        global __builtins__
        if hasattr(__builtins__, "__dict__"):
                vars(__builtins__)["__dict__"] = value
                # __builtins__.__dict__ = value
        else:
                __builtins__ = value
        return value

import builtins    as _builtins
import collections as _collections

class _dictator(_collections.UserDict):
        def __hasattr__(self, name): return name in self.data
        def __getattr__(self, name): return self.data[name]
        def __setitem__(self, *_):   raise  self.data["Exception"]("Dictator.")
        def __delitem__(self, *_):   raise  self.data["Exception"]("Dictator.")
        def __setattr__(self, *_):   raise  self.data["Exception"]("Dictator.")
        def __init__(self, dict):
                self.__dict__.update(data = dict)

_python = _dictator(_python_builtins_dictionary())
_setf_python_builtins_dictionary(_python.dict(_python_builtins_dictionary()))

# Obtained by the means of: list(sorted(sys.modules["__main__"].__builtins__.__dict__.keys()))
_python_builtin_names = ['ArithmeticError', 'AssertionError', 'AttributeError',
                         'BaseException', 'BufferError', 'BytesWarning',
                         'DeprecationWarning',
                         'EOFError', 'Ellipsis', 'EnvironmentError', 'Exception',
                         ############ 'False' cannot be deleted
                         'False', 'FloatingPointError', 'FutureWarning',
                         'GeneratorExit',
                         'IOError', 'ImportError', 'ImportWarning', 'IndentationError', 'IndexError',
                         'KeyError', 'KeyboardInterrupt',
                         'LookupError',
                         'MemoryError',
                         ############ 'None' cannot be deleted
                         'NameError', 'None', 'NotImplemented', 'NotImplementedError',
                         'OSError', 'OverflowError',
                         'PendingDeprecationWarning',
                         'ReferenceError', 'ResourceWarning', 'RuntimeError', 'RuntimeWarning',
                         'StopIteration', 'SyntaxError', 'SyntaxWarning', 'SystemError', 'SystemExit',
                         ############ 'True' cannot be deleted
                         'TabError', 'True', 'TypeError',
                         'UnboundLocalError', 'UnicodeDecodeError', 'UnicodeEncodeError', 'UnicodeError', 'UnicodeTranslateError', 'UnicodeWarning', 'UserWarning',
                         'ValueError',
                         'Warning',
                         'ZeroDivisionError',
                         ###### These are both sufficiently important and _-prefixed.
                         '_', # '__build_class__', '__debug__', '__doc__', '__import__', '__name__', '__package__',
                         'abs', 'all', 'any', 'ascii',
                         'bin', 'bool', 'bytearray', 'bytes',
                         'callable', 'chr', 'classmethod', 'compile', 'complex', 'copyright', 'credits',
                         'delattr', 'dict', 'dir', 'divmod',
                         'enumerate', 'eval', 'exec', 'exit',
                         'filter', 'float', 'format', 'frozenset',
                         'getattr', 'globals',
                         'hasattr', 'hash', 'help', 'hex',
                         'id', 'input', 'int', 'isinstance', 'issubclass', 'iter',
                         'len', 'license', 'list', 'locals',
                         'map', 'max', 'memoryview', 'min',
                         'next',
                         'object', 'oct', 'open', 'ord',
                         'pow', 'print', 'property',
                         'quit',
                         'range', 'repr', 'reversed', 'round',
                         'set', 'setattr', 'slice', 'sorted', 'staticmethod', 'str', 'sum', 'super',
                         'tuple', 'type',
                         'vars',
                         'zip']
### Clean up the namespace.
def _distance_oneself_from_python():
        for name in _python_builtin_names:
                del _python_builtins_dictionary()[name]

_distance_oneself_from_python()
### What's left:
## o  None, True, False -- notably are not AST-level, but still are invincible.
## o  __(build_class | debug | doc | import | name | package)__
## o  _python_builtins_dictionary, _python_builtin_names
## o  _python

###
### Some surfacial Common Lisp compatibility.
###
import re          as _re
import os          as _os
import io          as _io
import _io         as __io
import ast         as _ast
import imp         as _imp
import sys         as _sys
import math        as _math
import time        as _time
import types       as _types
import socket      as _socket
import inspect     as _inspect
import platform    as _platform
import functools   as _functools
import linecache   as _linecache
import threading   as _threading
import collections as _collections

import neutrality  as _neutrality

from builtins import isinstance as _isinstance, len as _len, repr as _repr
from builtins import zip as _zip, range as _range, reversed as _reversed, enumerate as _enumerate
from builtins import id as _id, hasattr as _hasattr, getattr as _getattr, setattr as _setattr

###
### Types
###
## Naming policy:
##
## Two aspects are at play: the type aspect and the function aspect.
##
## In Python the type name has only one value associated with it, and
## this value works both as a type specifier, and as its constructor
## function -- everything in single value namespace.
##
## In CL this isn't so, and the symbol can have a function associated
## with it, at the same time it can be interepreted as a type
## specifier on its own, and still it might have no value attached to
## it (this is, in fact, the common case).
##
## To be able to correctly reflect that picture, we need to introduce
## an artificial split into the single python namespace.  So:
##
##  - 
## 
function_   = _types.FunctionType.__mro__[0] # clashes with #'CL:FUNCTION
integer     = _builtins.int
float       = _builtins.float
complex     = _builtins.complex
string_     = _builtins.str                  # clashes with #'CL:STRING
boolean     = _builtins.bool
hash_table  = _builtins.dict
stream_     = __io._IOBase                   # clashes with keyword argument names
_type       = _builtins.type
_list       = _builtins.list
_tuple      = _builtins.tuple
_str        = _builtins.str
_bytes      = _builtins.bytes
_bytearray  = _builtins.bytearray
_set        = _builtins.set
_frozenset  = _builtins.frozenset
_NoneType   = _type(None)

###
### Constants
###
pi                   = _math.pi
most_positive_fixnum = 67108864

def identity(x):
        return x

def type_of(x):
        return _type(x)

def let(*values_and_body):
        values, body = values_and_body[:-1], values_and_body[-1]
        return body(*values)

def progn(*body):
        for b in body[:-1]:
                b()
        return body[-1]()

def _prognf(*body):
        return lambda: progn(*body)

reduce = _functools.reduce

def defstruct(name, *slots):
        return _collections.namedtuple(name, slots)

def _cold_constantp(form):
        # Coldness:
        #  - slow handling of constant variables
        #  - no handling of DEFCONSTANT-introduced variables
        #  - additional constant forms
        return (_isinstance(form, (integer, float, complex, string_)) or
                (type_of(form).__name__ == "symbol" and
                 ((form.package.name == "KEYWORD") or
                  (form.package.name == "COMMON-LISP" and form.name in ["T", "NIL"]))) or
                (_tuplep(form)                         and
                 _len(form) == 2              and
                 type_of(form[0]).__name__ == "symbol" and
                 form.package.name == "COMMON-LISP"    and
                 form.name in ["QUOTE"]))
constantp = _cold_constantp

def string_upcase(x):     return x.upper()
def string_downcase(x):   return x.lower()
def string_capitalize(x): return x.capitalize()

def char_upcase(x):       return x.upper()
def char_downcase(x):     return x.lower()
def upper_case_p(x):      return x.isupper()
def lower_case_p(x):      return x.islower()

def gethash(key, dict, default = None):
        therep = key in dict
        return (dict.get(key) if therep else default), therep

__core_symbol_names__ = [
        "QUOTE",
        "AND", "OR", "MEMBER", "EQL", "SATISFIES",
        "ABORT", "CONTINUE", "BREAK",
        "LIST",
        "_OPTIONAL", "_REST", "_KEY", "_BODY", "_ALLOW_OTHER_KEYS", "_WHOLE",
        "_RESTKEY", # pythonism
        # Heresy!  Extensions!
        "TUPLE", "PARTUPLE", "VARITUPLE", "MAYBE", "CLASS", "CLASS_EQ", "LAMBDA_LIST"
        ]
__more_symbol_names__ = [
        "SOME", "EVERY",
]

_case_attribute_map = _dict(UPCASE     = string_upcase,
                            DOWNCASE   = string_downcase,
                            CAPITALIZE = string_capitalize,
                            PRESERVE   = identity)
def _case_xform(type, s):
        return _case_attribute_map[type.name](s)

# Issue GLOBALS-SPECIFIED-TO-REFER-TO-THE-CONTAINING-MODULE-NOT-THE-CALLING-ONE
def _setf_global(value, name):
        _builtins.globals()[name] = value
        return value

def _global(name):
        return gethash(name, _builtins.globals())

###
### Cold boot
###
def _cold_print_symbol(s, **keys):
        return s.package.name + ":" + s.name
_print_symbol = _cold_print_symbol

def _cold_error(datum, *args):
        raise (_builtins.Exception(datum % args) if stringp(datum) else
               datum(*args))
error = _cold_error
def _cold_null(x): return not x
null = _cold_null
def _cold_format(destination, control_string, *args):
        string = control_string % args
        if not destination:
                return string
        else:
                _write_string(string, sys.stderr if destination is t else destination)
format = _cold_format
def _cold_princ_to_string(x):
        return _repr(x)
princ_to_string = _cold_princ_to_string
# Unregistered Issue PACKAGE-INIT-MUST-TAKE-COLD-SYMBOL-VALUES-INTO-ACCOUNT
def _cold_probe_file(pathname):
        assert(stringp(pathname))
        return os.path.exists(the(string_, pathname))
probe_file = _cold_probe_file
def _cold_merge_pathnames(pathname, default_pathname = None, default_version = None):
        """merge-pathnames pathname &optional default-pathname default-version

=> merged-pathname

Arguments and Values:

PATHNAME---a pathname designator.

DEFAULT-PATHNAME---a pathname designator. The default is the value of *DEFAULT-PATHNAME-DEFAULTS*.

DEFAULT-VERSION---a valid pathname version. The default is :NEWEST.

MERGED-PATHNAME---a pathname.

Description:

Constructs a pathname from PATHNAME by filling in any unsupplied
components with the corresponding values from DEFAULT-PATHNAME and
DEFAULT-VERSION.

Defaulting of pathname components is done by filling in components
taken from another pathname.  This is especially useful for cases such
as a program that has an input file and an output file.  Unspecified
components of the output pathname will come from the input pathname,
except that the type should not default to the type of the input
pathname but rather to the appropriate default type for output from
the program; for example, see the function COMPILE-FILE-PATHNAME.

If no version is supplied, DEFAULT-VERSION is used.  If DEFAULT-VERSION
is NIL, the version component will remain unchanged.

If PATHNAME explicitly specifies a host and not a device, and if the
host component of DEFAULT-PATHNAME matches the host component of
pathname, then the device is taken from the DEFAULT-PATHNAME;
otherwise the device will be the default file device for that host.
If PATHNAME does not specify a host, device, directory, name, or type,
each such component is copied from DEFAULT-PATHNAME.  If PATHNAME does
not specify a name, then the version, if not provided, will come from
DEFAULT-PATHNAME, just like the other components.  If PATHNAME does
specify a name, then the version is not affected by DEFAULT-PATHNAME.
If this process leaves the version missing, the DEFAULT-VERSION is
used.  If the host's file name syntax provides a way to input a
version without a name or type, the user can let the name and type
default but supply a version different from the one in
DEFAULT-PATHNAME.

If PATHNAME is a stream, PATHNAME effectively becomes (PATHNAME
PATHNAME).  MERGE-PATHNAMES can be used on either an open or a closed
stream.

If PATHNAME is a pathname it represents the name used to open the
file.  This may be, but is not required to be, the actual name of the
file.

MERGE-PATHNAMES recognizes a logical pathname namestring when
DEFAULT-PATHNAME is a logical pathname, or when the namestring begins
with the name of a defined logical host followed by a colon.  In the
first of these two cases, the host portion of the logical pathname
namestring and its following colon are optional.

MERGE-PATHNAMES returns a logical pathname if and only if its first
argument is a logical pathname, or its first argument is a logical
pathname namestring with an explicit host, or its first argument does
not specify a host and the DEFAULT-PATHNAME is a logical pathname.

Pathname merging treats a relative directory
specially. If (pathname-directory pathname) is a list whose car
is :RELATIVE, and (pathname-directory default-pathname) is a list,
then the merged directory is the value of

 (append (pathname-directory default-pathname)
         (cdr  ;remove :relative from the front
           (pathname-directory pathname)))

except that if the resulting list contains a string or :WILD
immediately followed by :BACK, both of them are removed.  This removal
of redundant :BACK keywords is repeated as many times as
possible.  If (pathname-directory default-pathname) is not a list
or (pathname-directory pathname) is not a list whose car is :RELATIVE,
the merged directory is (or (pathname-directory
pathname) (pathname-directory default-pathname))

MERGE-PATHNAMES maps customary case in PATHNAME into customary case in
the output pathname.

Notes:

The net effect is that if just a name is supplied, the host, device,
directory, and type will come from DEFAULT-PATHNAME, but the version
will come from DEFAULT-VERSION.  If nothing or just a directory is
supplied, the name, type, and version will come from DEFAULT-PATHNAME
together."""
        # * (merge-pathnames "/a" "b")     # -NAME superseded by X
        # #P"/a"
        # * (merge-pathnames "a" "b")
        # #P"a"
        # * (merge-pathnames "a/a" "/b")   # when Y specifies the whole path, X takes over
        # #P"/a/a"
        # * (merge-pathnames "/a" "b/b")
        # #P"/a"
        # * (merge-pathnames "/a/" "b")    # non-conflicting components are merged
        # #P"/a/b"
        # * (merge-pathnames "b" "a/")
        # #P"a/b"
        # * (merge-pathnames "a/" "b")
        # #P"a/b"
        # * (merge-pathnames "b" "a/")
        # #P"a/b"
        # * (merge-pathnames "/a/" "/b/")
        # #P"/a/"
        ## Unregistered Issue MERGE-PATHNAMES-WEIRDLY-IMPLEMENTED
        # * (merge-pathnames "a/" "b/")
        # #P"b/a/"
        # * (merge-pathnames "a/a" "b/b")
        # P"b/a/a"
        _not_implemented() # Gave up for the while.
        default_pathname = _defaulted(default_pathname, os.getcwd() + os.sep)
        dir_supplied_p = os.sep in pathname
        name_supplied_p = pathname and pathname[-1] != os.sep
        dir_defaulted_p = os.sep in default_pathname
        net_effect_if = name_supplied_p and not dir_supplied_p # Unregistered Issue COMPLIANCE-MERGE-PATHNAMES-SIMPLIFICATION
        if net_effect_if:
                return os.path.join((default_pathname[:position(os.sep, default_pathname, from_end = True) + 1] if dir_defaulted_p else ""),
                                    pathname)
        elif not name_supplied_p:
                pass
        return os.path.join(x, y)

###
### Ring 1.
###
def _0arg(*args):
        return args[0]

def _1arg(*args):
        return args[1]

def _narg(n, *args):
        return args[n]

def _alist_plist(xs):
        return append(*xs)

def _plist_alist(xs):
        acc = []
        for i in _range(0, _len(xs), 2):
                acc.append((xs[i], xs[i + 1]))
        return acc

def _hash_table_alist(xs):
        return xs.items()

def _alist_hash_table(xs):
        return _dict(xs)

class _cache(collections.UserDict):
        def __init__(self, filler):
                self.filler = filler
                self.data = _dict()
        def __getitem__(self, key):
                check_type(key, _tuple)
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

def _specifiedp(x):
        return x is not None

def _defaulted(x, value, type = None):
        if type is not None:
                check_type(x, type) # Not a macro, so cannot access the actual defaulted name..
        return x if x is not None else value

def _defaulted_to_var(x, variable, type = None):
        return _defaulted(x, symbol_value(variable), type = type)

def _only_specified_keys(**keys):
        return _dict(((k, v) for k, v in keys if _specifiedp(k)))

def _read_case_xformed(x):
        return _case_xform(_symbol_value("_READ_CASE_"), x)

def _coerce_to_symbol_name(x):
        return (x.name                if symbolp(x) else
                _read_case_xformed(x) if stringp(x) else
                error(simple_type_error, "%s cannot be coerced to string.", x))

###
### AST basis.
###
def _astp(x):        return typep(x, ast.AST)

def _coerce_to_ast_type(type):
        return typecase(type,
                        (_type,   lambda: (type if subtypep(type, ast.AST) else
                                           error("Provided type %s is not a proper subtype of ast.AST.", type))),
                        (string_, lambda: (ast.__dict__[type] if type in ast.__dict__ else
                                           error("Unknown AST type '%s'.", type))),
                        (t,       lambda: error("Invalid AST type specifier: %s, %s, %s.", type, _type, typep(type, _type))))

def _text_ast(text):
        return _builtins.compile(text, "", 'exec', flags=ast.PyCF_ONLY_AST).body

def _function_ast(fn):
        fn_ast = _text_ast(inspect.getsource(fn))[0]
        return fn_ast.args, fn_ast.body

def _function_body_pass_p(fn):
        fn_body_ast = _function_ast(fn)[1]
        return _len(fn_body_ast) == 1 and typep(fn_body_ast[0], ast.Pass)

### literals
def _ast_num(n):
        return ast.Num(n = the(integer, n))
def _ast_bool(n):
        return ast.Bool(n = the(integer, n))
def _ast_string(s):
        return ast.Str(s = the(string_, s))
def _ast_set(xs,   writep = False):
        return ast.Set(elts   = the((list_, ast.AST), xs), ctx = _ast_rw(writep))
def _ast_list(xs,  writep = False):
        return ast.List(elts  = the((list_, ast.AST), xs), ctx = _ast_rw(writep))
def _ast_tuple(xs, writep = False):
        return ast.Tuple(elts = the((list_, ast.AST), xs), ctx = _ast_rw(writep))

############################### recurse? AST-ifier
__astifier_map__ = { string:          (False, _ast_string),
                     integer:         (False, _ast_num),
                     boolean:         (False, _ast_num),
                     _NoneType:       (False, lambda x: _ast_name("None")),
                     _list:           (True,  _ast_list),
                     _tuple:          (True,  _ast_tuple),
                     _set:            (True,  _ast_set),
                     ## symbol: see below
                     }
def _register_astifier_for_type(type, recurse, astifier):
        "Please, list the added astifiers above."
        __astifier_map__[type] = (recurse, astifier)

def _astifiable_p(x):
        return _type(x) in __astifier_map__

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
def _ast_alias(name):                        return ast.alias(name = the(string_, name), asname = None)
def _ast_keyword(name, value):               return ast.keyword(arg = the(string_, name), value = the(ast.expr, value))

def _ast_rw(writep):                         return (ast.Store() if writep else ast.Load())
def _ast_name(name, writep = False):         return ast.Name(id = the(string_, name), ctx = _ast_rw(writep))
def _ast_attribute(x, name, writep = False): return ast.Attribute(attr = name, value = x, ctx = _ast_rw(writep))
def _ast_index(of, index, writep = False):   return ast.Subscript(value = of, slice = ast.Index(value = index), ctx = _ast_rw(writep))
def _ast_maybe_normalise_string(x):          return (_ast_string(x) if stringp(x) else x)

def _ast_funcall(name, args = [], keys = {}, starargs = None, kwargs = None):
        check_type(args, (list_, (or_, ast.AST, _NoneType, (satisfies_, _astifiable_p))))
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
        return ast.ImportFrom(module = the(string_, module_name),
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
def _function_lambda_list(fn, astify_defaults = True):
        return _argspec_lambda_spec(inspect.getfullargspec(fn), astify_defaults = astify_defaults)

def _argspec_nfixargs(paramspec):
        return _len(paramspec.args) - _len(paramspec.defaults or []) # ILTW Python implementors think..

def _argspec_lambda_spec(spec, astify_defaults = True):
        # args, varargs, varkw, defaults, kwonlyargs, kwonlydefaults, annotations
        nfixargs = _argspec_nfixargs(spec)
        default_xform = _astify_constant if astify_defaults else identity
        return (spec.args[:nfixargs],
                _list(_zip(spec.args[nfixargs:],
                           mapcar(default_xform, spec.defaults or []))),
                spec.varargs,
                _list(_zip(spec.kwonlyargs,
                      mapcar(default_xform, spec.kwonlydefaults or []))),
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
                                                        (_list(optional) + _list(keyword) +
                                                         ([args] if args else []) +
                                                         ([keys] if keys else [])))))))
###
### Basis
###
##
## modules/packages
##
def _load_code_object_as_module(name, co, filename = "", builtins = None, globals_ = None, locals_ = None, register = True):
        check_type(co, _type(_load_code_object_as_module.__code__))
        mod = imp.new_module(name)
        mod.__filename__ = filename
        if builtins:
                mod.__dict__["__builtins__"] = builtins
        if register:
                sys.modules[name] = mod
        globals_ = _defaulted(globals_, mod.__dict__)
        locals_  = _defaulted(locals_, mod.__dict__)
        _builtins.exec(co,
                       globals_,
                       locals_)
        return mod, globals_, locals_

def _load_text_as_module(name, text, filename = "", **keys):
        return _load_code_object_as_module(name, _builtins.compile(text, filename, "exec"),
                                           filename = filename, **keys)[0]

def _reregister_module_as_package(mod, parent_package = None):
        # this line might need to be performed before exec()
        mod.__path__ = (parent_package.__path__ if parent_package else []) + [ mod.name.split(".")[-1] ]
        if parent_package:
                dotpos = mod.name.rindex(".")
                assert(dotpos)
                postdot_name = mod.name[dotpos + 1:]
                _setattr(parent_package, postdot_name, mod)
                parent_package.__children__.add(mod)
                mod.__parent__ = parent_package
        if packagep:
                mod.__children__ = _set()

def _compile_and_load(*body, modname = "", filename = "", lineno = 0, **keys):
        return _load_code_object_as_module(
                modname,
                _builtins.compile(ast.fix_missing_locations(_ast_module(list(body), lineno = lineno)), filename, "exec"),
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

_frame = _type(_this_frame())

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
__ordered_frame_locals__ = _dict()
def _frame_ordered_locals(f):
        global __ordered_frame_locals__
        if f not in __ordered_frame_locals__:
                __ordered_frame_locals__[f] = _list(f.f_locals.keys())
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
        return ("%s%s %s(%s)" % (filename + ("" if align else ":") + (" " * (align - (_len(filename) % align if align else 0))),
                                 ("%d:" % _frame_lineno(f)) if lineno else "",
                                 fun_name, ", ".join(fun_params)))

def _print_frame(f, stream = None, **keys):
        write_string(_pp_frame(f, **keys), _defaulted_to_var(stream, "_DEBUG_IO_"))

def _print_frames(fs, stream = None):
        mapc(lambda i, f: format(_defaulted_to_var(stream, "_DEBUG_IO_"), "%2d: %s\n" % (i, _pp_frame(f, lineno = True))),
             *zip(*enumerate(fs)))

def _backtrace(x = -1, stream = None):
        _print_frames(_frames_calling(_this_frame())[1:x],
                      _defaulted_to_var(stream, "_DEBUG_IO_"))

def _pp_frame_chain(xs, source_location = None, all_pretty = None, print_fun_line = None):
        def _pp_frame_in_chain(f, pretty = None):
                fun = _frame_fun(f)
                return format(nil, *(("%s",
                                      _fun_name(fun))
                                     if not pretty else
                                     ("%s%s@%s:%d",
                                      _fun_name(fun),
                                      (":" + _str(_frame_lineno(f) - _fun_firstlineno(fun))) if print_fun_line else "",
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

def _locals_printf(locals, *local_names):
        # Unregistered Issue NEWLINE-COMMA-SEPARATION-NOT-PRETTY
        _fprintf(sys.stderr, ", ".join((("%s: %%s" % x) if stringp(x) else "%s")
                                       for x in local_names) + "\n",
                 *((locals[x] if stringp(x) else "\n") for x in local_names))

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
                "names: globals;  varnames: args + otherbind;  locals: _len(varnames)"
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
#                 (lambda x: "\n  ".join(map(lambda s: s + ": " + _str(getattr(x, s)),
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
#   co_consts: ('names: globals;  varnames: args + otherbind;  locals: _len(varnames)', 'This is xceptor talking: %s.', None)
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
                return self.format_control % _tuple(self.format_arguments)
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

def _tuplep(x):     return _isinstance(x, _tuple)
def _frozensetp(o): return _isinstance(o, _frozenset)
def _setp(o):       return _isinstance(o, (_set, _frozenset))
def _nonep(o):      return o is None

def _ensure_list(x):
        return x if _listp(x) else [x]
def _ensure_car(x):
        return x[0] if _hasattr(x, "__len__") else x
def _ensure_cons(x, default = None):
        return x if _tuplep(x) and _len(x) == 2 else (x, default)

def _mapset(f, xs):
        acc = _set()
        for x in xs:
                acc.add(f(x))
        return acc

def _mapsetn(f, xs):
        acc = _set()
        for x in xs:
                acc |= f(x)
        return acc

def _mapseparaten(f, xs):
        s0, s1 = _set(), _set()
        for s0r, s1r in (f(x) for x in xs):
                s0 |= s0r; s1 |= s1r
        return s0, s1

def _separate(n, f, xs):
        ss = _tuple(_set() for _ in _range(n))
        for rss in (f(x) for x in xs):
                for s, rs in _zip(ss, rss):
                        s |= rs
        return ss

__combiners__ = { set: set.add, list: list.append }
def _recombine(spec, f, xss):
        accs  = _tuple(f() for f in spec)
        combs = _tuple(__combiners__[_type(a)] for a in accs)
        for xs in xss:
                for acc, comb, reselt in _zip(accs, combs, f(xs)):
                        comb(acc, reselt)
        return accs
def _recombine_star(spec, f, *xss):
        accs  = _tuple(f() for f in spec)
        combs = _tuple(__combiners__[_type(a)] for a in accs)
        for xs in _zip(*xss):
                for acc, comb, reselt in _zip(accs, combs, f(*xs)):
                        comb(acc, reselt)
        return accs

def _mapcar_star(f, xs):
        return [ f(*x) for x in xs ]

def _slotting(x):             return lambda y: _getattr(y, x, None)
def _slot_of(x):              return lambda y: _getattr(x, y, None)
def _slot_equal(slot, val):   return lambda y: _getattr(y, slot, None) == val

def _indexing(*is_):          return lambda y: aref(y, *is_)
def _index_of(xs):            return lambda *is_: aref(xs, *is_)
def _index_equal(index, val): return lambda y: y[index] == val

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

def _defwith(name, enter, exit, **initargs):
        initargs.update(_dict(__enter__ = enter,
                              __exit__  = exit))
        return _type(name, (object,), initargs)

##
## Lesser non-CL tools
##
class _servile():
        def __repr__(self):
                return "#%s(%s)" % (_type(self).__name__,
                                    ", ".join(_maphash(lambda k, v: "%s = %s" % (k, v),
                                                       self.__dict__)))
        def __init__(self, **keys):
                self.__dict__.update(keys)

##
## Symbols
##
__gensym_counter__ = 0
def _gensymname(x = "N"):
        # Unregistered Issue GENSYM-NOT-THREAD-SAFE
        global __gensym_counter__
        __gensym_counter__ += 1
        return x + _str(__gensym_counter__)
def gensym(x = "G"):
        return make_symbol(_gensymname(x))

def _init_gen(self, n = 1, x = "G", gen = gensym):
        if zerop(n):
                error("WITH-GEN: we are very very much against this, please stop doing it!")
        self.gen, self.x, self.n = gen, x, n
_gen = _defwith("_gen",
                lambda self: (self.gen(self.x) if self.n == 1 else
                              _tuple(self.gen(self.x) for i in _range(self.n))),
                lambda *_: None,
                __init__ = _init_gen)

def _gensyms(**initargs):     return _get(gen = gensym,      **initargs)
def _gensymnames(**initargs): return _get(gen = _gensymname, **initargs)

##
## Basic
##
__iff__ = { True:  lambda x, _: x,
            False: lambda _, y: y }
def iff(val, consequent, antecedent):
        "This restores sanity."
        return __iff__[not not val](consequent, antecedent)()

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
        return (x is y) if not _isinstance(x, integer) else x == y

def equal(x, y):
        return x == y

def destructuring_bind(val, body):
        return body(*val)

def _destructuring_bind_keys(val, body):
        return body(**val)

def when(test, body):
        if test:
                return body() if _isinstance(body, function_) else body
def cond(*clauses):
        for (test, body) in clauses:
                if test() if _isinstance(test, function_) else test:
                        return body() if _isinstance(body, function_) else body
def case(val, *clauses):
        for (cval, body) in clauses:
                if ((val == cval or (cval is True) or (cval is t)) if not _isinstance(cval, _list) else
                    val in cval):
                        return body() if _isinstance(body, function_) else body

def ecase(val, *clauses):
        for (cval, body) in clauses:
                if ((val == cval) if not _isinstance(cval, _list) else
                    val in cval):
                        return body() if _isinstance(body, function_) else body
        error("%s fell through ECASE expression. Wanted one of %s.", val, mapcar(first, clauses))

def _infinite(x):
        while True:
                yield x

def _seek(n, iterator):
        for i in range(n):
                next(iterator, nil)

def _from(n, xs):
        iterator = iter(xs)
        for i in range(n):
                next(iterator, nil)
        for x in iterator:
                yield x

def every(fn, *xss, start = 0):
        for xs in _from(start, zip(*xss)):
                if not fn(*xs): return False
        return (xs or True) if "xs" in _builtins.locals() else True

def notevery(fn, *xss, start = 0):
        for xs in _from(start, zip(*xss)):
                ret = fn(*xs)
                if not ret: return ret or True
        return False

def some(fn, *xss, start = 0):
        for xs in _from(start, zip(*xss)):
                ret = fn(*xs)
                if ret: return ret or True
        return False

def notany(fn, *xss, start = 0):
        for xs in _from(start, zip(*xss)):
                if fn(*xs): return False
        return (xs or True) if "xs" in _builtins.locals() else True

def _xorf(x, y):
        return (x or y) and not (x and y)

def _nxorf(x, y):
        return (x and y) or not (x or y)

##
## Types
##
class type_error(error_):
        pass

class simple_type_error(simple_condition, type_error):
        pass

def find_class(x, errorp = True):
        check_type(x, symbol)
        return (x.value if typep(x.value, _type) else
                nil     if not errorp            else
                error("There is no class named %s.", x))

def _of_type(x):
        return lambda y: typep(y, x)

def _not_of_type(x):
        return lambda y: not typep(y, x)

def _type_specifier_complex_p(x):
        """Determines, whether a type specifier X constitutes a
complex type specifier."""
        return _tuplep(x)

def _invalid_type_specifier_error(x, complete_type = None):
        error(simple_type_error, "%s is not a valid type specifier%s.",
              x, ("" if not complete_type else
                  (" (within type specifier %s)" % (complete_type,))))

# __type_predicate_map__ is declared after the package system is initialised
def _complex_type_mismatch(x, type):
        ctype_test = __type_predicate_map__[type[0]]
        ret = ctype_test(x, type)
        return (ret if not (_tuplep(ret) and ret[2]) else
                _invalid_type_specifier_error(ret[1], complete_type = type))

def _type_mismatch(x, type):
        """Determine, whether X does not belong to TYPE, and if so,
return a triple, specifying the specific parts of X and TYPE being in
disagreement and, as a third element, a boolean, denoting whether the
type specifier was malformed.  Otherwise, when X is of TYPE, a
negative boolean value is returned."""
        return (((not _isinstance(x, type)) and
                 (x, type, False))              if _isinstance(type, _type)   else
                nil                             if type is t                           else
                _complex_type_mismatch(x, type) if (_tuplep(type) and type and
                                                    type[0] in __type_predicate_map__) else
                _invalid_type_specifier_error(type))

def typep(x, type):
        return not _type_mismatch(x, type)

def subtypep(sub, super):
        return (issubclass(sub, super)                         if super is not t                            else
                _not_implemented("complex type relatioships: %s vs. %s.",
                                 sub, super)                   if _tuplep(sub) or _tuplep(super)            else
                error("%s is not a valid type specifier", sub) if not (typep(sub, (or_, _type, (eql_, t))) and
                                                                       typep(sub, (or_, _type, (eql_, t)))) else
                sub is super or super is t)

def the(type, x):
        mismatch = _type_mismatch(x, type)
        return (x if not mismatch else
                error(simple_type_error, "The value %s is not of type %s%s.",
                      x, type, ("" if (not _type_specifier_complex_p(type)) or type is mismatch[1] else
                                (", specifically, the value %s is not of type %s" % (princ_to_string(mismatch[0]), mismatch[1])))))

def check_type(x, type):
        the(type, x)

def typecase(val, *clauses):
        for (ctype, body) in clauses:
                if (ctype is t) or (ctype is True) or typep(val, ctype):
                        return body() if _isinstance(body, function_) else body

def etypecase(val, *clauses):
        for (ctype, body) in clauses:
                if (ctype is t) or (ctype is True) or typep(val, ctype):
                        return body() if _isinstance(body, function_) else body
        else:
                error(simple_type_error, "%s fell through ETYPECASE expression. Wanted one of (%s).",
                      val, ", ".join(mapcar(lambda c: c[0].__name__, clauses)))

##
## Type predicates
##
def functionp(o):     return _isinstance(o, function_)
def integerp(o):      return _isinstance(o, integer)
def floatp(o):        return _isinstance(o, float)
def complexp(o):      return _isinstance(o, complex)
def numberp(o):       return _isinstance(o, (integer, float, complex))
def _listp(o):        return _isinstance(o, _list)
def _boolp(o):        return _isinstance(o, boolean)
def sequencep(x):     return _getattr(_type(x), "__len__", None) is not None
def hash_table_p(o):  return _isinstance(o, hash_table)

##
## Predicates
##
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
        return _list(values_form)

def multiple_values_list(list):
        return _tuple(list)

def multiple_value_call(function, *values_forms):
        return function(*(append(*values_forms)))

##
## Conses
##
def cons(x, y):       return (x, y)
def consp(o):         return _isinstance(o, _tuple) and _len(o) is 2
def atom(o):          return not _isinstance(o, _tuple)
def car(x):           return x[0]   if x  else nil
def cdr(x):           return x[1:]  if x  else nil
def first(xs):        return xs[0]  if xs else nil
def second(xs):       return xs[1]  if _len(xs) > 1 else nil
def third(xs):        return xs[2]  if _len(xs) > 2 else nil
def rest(xs):         return xs[1:] if xs else nil
def nth(n, xs):       return xs[n] if n < _len(xs) else nil

def copy_list(x):
        return _list(the(_list, x))

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

__allowed__ = _frozenset([string_, _set, _frozenset, _tuple, _list, _bytes, _bytearray])
def _maprestype(x):
        type = type_of(x)
        return type if type in __allowed__ else _list

def remove_if(f, xs, key = identity):
        if _isinstance(xs, _dict):
                return              { k:x for k, x in xs.items() if not f(k, key(x))}
        else:
                return _maprestype(xs) (x for x    in xs         if not f(key(x)))

def remove_if_not(f, xs, key = identity):
        if _isinstance(xs, _dict):
                return              { k:x for k, x in xs.items() if f(k, key(x))}
        else:
                return _maprestype(xs) (x for x    in xs         if f(key(x)))

def remove(elt, xs, test = eql, key = identity):
        if _isinstance(xs, _dict):
                return              { k:x for k, x in xs.items() if not test(elt, key(x))}
        else:
                return _maprestype(xs) (x for x    in xs         if not test(elt, key(x)))

def find_if(p, xs, key = identity, start = 0, end = None, from_end = None):
        # Unregistered Issue FIND-IF-NOT-ITERATOR-FRIENDLY
        end = end or _len(xs)
        if start or end:
                seq = zip(xs, _range(_len(xs)))
                if from_end:
                        seq = _reversed(_list(seq))
                for (x, i) in seq:
                        if (start <= i < end) and p(key(x)):
                                return x
        else:
                if from_end:
                        xs = reversed(xs)
                for x in xs:
                        if p(key(x)):
                                return x

def find_if_not(p, xs, key = identity, start = 0, end = None, from_end = None):
        return find_if(complement(p), xs, key = key, start = start, end = end, from_end = from_end)

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
        end = end or _len(xs)
        if start or end:
                seq = _zip(xs, _range(_len(xs)))
                if from_end:
                        seq = _reversed(_list(seq))
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
        for (x, i) in _zip(xs, _range(_len(xs))):
                if (i >= start) and key(x) == elt:
                        c += 1
        return c

def count_if(p, xs, key = identity, start = 0):
        c = 0
        for (x, i) in _zip(xs, _range(_len(xs))):
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
        if _len(object) > _len(list):
                return None
        else:
                list_start = _len(list) - _len(object)
                return list[list_start:] == object

# XXX: This is geared at cons-style lists, and so is fucking costly
# for imperative lists.
def ldiff(object, list_):
        """If OBJECT is the same as some tail of LIST, LDIFF returns a
fresh list of the elements of LIST that precede OBJECT in the
list structure of LIST; otherwise, it returns a copy[2] of
LIST."""
        if _len(object) > _len(list_):
                return _list(list_)
        else:
                list_start = _len(list_) - _len(object)
                if list_[list_start:] == object:
                        return list_[:list_start]
                else:
                        return _list(list_)

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
# Issue INCONSISTENT-HASH-TABLE-FUNCTION-NAMING
def _maphash(f, dict) -> _list:
        return [ f(k, v) for k, v in dict.items() ]

def _remap_hash_table(f, xs: _dict) -> _dict:
        return { k: f(k, v) for k, v in xs.items() }

def _map_into_hash(f, xs,
                   key_test = lambda k: k is not None,
                   value_test = lambda _: True) -> _dict:
        acc = _dict()
        for x in xs:
                k, v = f(x)
                if key_test(k) and value_test(v):
                        acc[k] = v
        return acc

def _map_into_hash_star(f, xs,
                        key_test = lambda k: k is not None,
                        value_test = lambda _: t) -> _dict:
        acc = _dict()
        for x in xs:
                k, v = f(*x)
                if key_test(k) and value_test(v):
                        acc[k] = v
        return acc

def _map_hash_table(f, hash_table, **keys) -> _dict:
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
        _setattr(ret, "ball", nonce)
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
        nonce = (nonce if not _isinstance(nonce, function_) else
                 (_getattr(nonce, "ball", None) or
                  error("RETURN-FROM was handed a %s, but it is not cooperating in the __BLOCK__ nonce passing syntax.", nonce)))
        throw(nonce, value)

##
## Dynamic scope
##
__global_scope__ = _dict()

class thread_local_storage(threading.local):
        def __init__(self):
                self.dynamic_scope = []

__tls__ = thread_local_storage()

# The symmetry invariance is _IMPORTANT_, as you probably can imagine!
def _dynamic_scope_push(scope):
        __tls__.dynamic_scope.append(scope)
def _dynamic_scope_pop():
        __tls__.dynamic_scope.pop()

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
                _dynamic_scope_push(_coerce_cluster_keys_to_symbol_names(self.cluster))
        def __exit__(self, t, v, tb):
                _dynamic_scope_pop()

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
__packages__         = _dict()
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
        conflict_set = _mapset(_slotting("name"), syms.values()) & _set(dest.accessible.keys())
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
        ## Unregistered Issue COMPLIANCE-COERCE
        return (x if _isinstance(x, type) else
                case(type,
                     (string_,    "".join(x)),
                     (hash_table, _dict.fromkeys(x)),
                     (t,          _type(x))))

def _python_name_lisp_symbol_name(x):
        "Heuristic to undo the effect of _lisp_symbol_name_python_name()."
        def _sub(cs):
                starred = _len(cs) > 1 and (cs[0] == cs[-1] == "_") # *very-nice*
                anded   = _len(cs) > 1 and (cs[0] == "_" != cs[-1]) # &something    # This #\& heuristic might bite us quite well..
                pre, post, start, end = (("*", "*", 1, _len(cs) - 1) if starred else
                                         ("&", "",  1, None)                  if anded   else
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
                return hash(_id(self))
        def __init__(self, name, use = [], filename = "",
                     ignore_python = False, python_exports = True, boot = False):
                self.name = string(name)

                self.own         = _set()                        # sym
                self.imported    = _set()                        # sym
              # self.present     = own + imported
                self.inherited   = collections.defaultdict(_set) # sym -> _set(pkg) ## _mapsetn(_slotting("external"), used_packages) -> source_package
                self.accessible  = _dict()                       # str -> sym       ## accessible = present + inherited
                self.external    = _set()                        # sym              ## subset of accessible
              # self.internal    = accessible - external

                modname = _lisp_symbol_name_python_name(name)
                self.module = (_find_module(modname, if_does_not_exist = "continue") or
                               _load_text_as_module(modname, "", filename = filename))
                # Issue _CCOERCE_TO_PACKAGE-WEIRD-DOUBLE-UNDERSCORE-NAMING-BUG
                coercer = (_ccoerce_to_package if boot else
                           _coerce_to_package)
                self.used_packages  = _set(mapcar(lambda x: coercer(x, if_null = "error"),
                                                  use))
                self.packages_using = _set()
                assert(every(packagep, self.used_packages))
                mapc(_curry(use_package, self), self.used_packages)

                ## Import the corresponding python dictionary.  Intern depends on
                if not ignore_python:
                        moddict = _dict(self.module.__dict__)
                        explicit_exports = _set(moddict["__all__"] if "__all__" in moddict else
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
        return (x                          if packagep(x)                      else
                _symbol_value("_PACKAGE_") if (not x) and if_null == "current" else
                _find_package(x)           if stringp(x) or symbolp(x)         else
                error(simple_type_error, "CCOERCE-TO-PACKAGE accepts only package designators -- packages, strings or symbols, was given '%s' of type %s.",
                      x, type_of(x)))
def _coerce_to_package(x, if_null = "current"):
        return (x                          if packagep(x)                      else
                _symbol_value("_PACKAGE_") if (not x) and if_null == "current" else
                find_package(x, True)      if stringp(x) or symbolp(x)         else
                error(simple_type_error, "COERCE-TO-PACKAGE accepts only package designators -- packages, strings or symbols, was given '%s' of type %s.",
                      x, type_of(x)))

def defpackage(name, use = [], export = []):
        p = package(name, use = use)
        for symname in export:
                _not_implemented("DEFPACKAGE: :EXPORT keyword") # XXX: populate the for-INTERN-time-export set of names
        return p

def in_package(name):
        setq("_PACKAGE_", _coerce_to_package(name))

###
### CL namespaces
###
## Some factology:
##
# * (defmacro yay (x) `(a))
# ; in: DEFMACRO YAY
# ;     (LET* ((X (CAR (CDR #:WHOLE628))))
# ;       (BLOCK YAY '(A)))
# ; 
# ; caught STYLE-WARNING:
# ;   The variable X is defined but never used.
# ; 
# ; compilation unit finished
# ;   caught 1 STYLE-WARNING condition
#
# YAY
# * (fboundp 'yay)
#
# T
# * (symbol-function 'yay)
#
# #<CLOSURE (LAMBDA (&REST SB-C::ARGS)) {1002CA2269}>
# * (fdefinition 'yay)
#
# #<CLOSURE (LAMBDA (&REST SB-C::ARGS)) {1002CA2269}>
# * (function yay)
#
# #<CLOSURE (LAMBDA (&REST SB-C::ARGS)) {1002CA2269}>
#
# * (setf (symbol-function 'yay) (lambda (z) z))
#
# #<FUNCTION (LAMBDA (Z)) {1002CC0099}>
# * (symbol-function 'yay)
#
# #<FUNCTION (LAMBDA (Z)) {1002CC0099}>
# * (fdefinition 'yay)
#
# #<FUNCTION (LAMBDA (Z)) {1002CC0099}>
# * (setf (fdefinition 'yay) (lambda (z) z))
#
# #<FUNCTION (LAMBDA (Z)) {1002CE8F79}>
# * (macroexpand-1 '(yay 1))
#
# (A)
# T

def fboundp(name):
        """fboundp name => generalized-boolean

Pronunciation:

[,ef'bandpee]

Arguments and Values:

NAME---a function name.

GENERALIZED-BOOLEAN---a generalized boolean.

Description:

Returns true if NAME is fbound; otherwise, returns false."""
        return t if (the(symbol, name).function or
                     symbol.macro_function) else nil

def fmakunbound(name):
        """fmakunbound name => name

Pronunciation:

[,ef'makuhn,band] or [,ef'maykuhn,band]

Arguments and Values:

NAME---a function name.

Description:

Removes the function or macro definition, if any, of NAME in the
global environment."""
        (the(symbol, name).function,
         symbol.macro_function) = nil, nil
        return name

def function(name):
        """function name => function

Arguments and Values:

NAME---a function name or lambda expression.

FUNCTION---a function object.

Description:

The value of FUNCTION is the functional value of NAME in the current
lexical environment.

If NAME is a function name, the functional definition of that name is
that established by the innermost lexically enclosing FLET, LABELS, or
MACROLET form, if there is one.  Otherwise the global functional
definition of the function name is returned.

If NAME is a lambda expression, then a lexical closure is returned.
In situations where a closure over the same set of bindings might be
produced more than once, the various resulting closures might or might
not be EQ.

It is an error to use FUNCTION on a function name that does not denote
a function in the lexical environment in which the FUNCTION form
appears.  Specifically, it is an error to use FUNCTION on a symbol
that denotes a macro or special form.  An implementation may choose
not to signal this error for performance reasons, but implementations
are forbidden from defining the failure to signal an error as a useful
behavior."""
        # Unregistered Issue COMPLIANCE-NAMESPACING-FUNCTIONS
        if special_operator_p(the(symbol, name)) or macro_function_p(name):
                error("Runtime Issue SYMBOL-%s-DENOTES-A-MACRO-OR-SPECIAL-FORM", name)
        maybe_fn = _lisp_symbol_python_value(name)
        if not maybe_fn or not _isinstance(maybe_fn, function_):
                error("Runtime Issue SYMBOL-%s-DOES-NOT-DENOTE-A-FUNCTION-IN-LEXICAL-ENVIRONMENT", name)
        return maybe_fn

class _cold_undefined_function(error_):
        def __init__(self, fname):
                self.name = fname
        def __str__(self):
                return "The function %s is undefined." % (self.name,)
        def __repr__(self):
                return self.__str__()
undefined_function = _cold_undefined_function

def symbol_function(symbol_):
        """symbol-function symbol => contents

(setf (symbol-function symbol) new-contents)

Arguments and Values:

SYMBOL---a symbol.

CONTENTS--- If the SYMBOL is globally defined as a macro or a special
operator, an object of implementation-dependent nature and identity is
returned.  If the SYMBOL is not globally defined as either a macro or
a special operator, and if the SYMBOL is fbound, a function object is
returned.

NEW-CONTENTS---a function.

Description:

Accesses the SYMBOL's function cell.

Affected By:

DEFUN

Exceptional Situations:

Should signal an error of type TYPE-ERROR if SYMBOL is not a symbol.

Should signal UNDEFINED-FUNCTION if SYMBOL is not fbound and an
attempt is made to read its definition. (No such error is signaled on
an attempt to write its definition.)"""
        return (the(symbol_, symbol).primitive_function or
                symbol.macro_function                   or
                the(function_, symbol.function)         or
                error(undefined_function, symbol))

def macro_function(symbol_, environment = None):
        """macro-function symbol &optional environment => function

(setf (macro-function symbol &optional environment) new-function)

Arguments and Values:

SYMBOL---a symbol.

ENVIRONMENT---an environment object.

FUNCTION---a macro function or NIL.

NEW-FUNCTION---a macro function.

Description:

Determines whether SYMBOL has a function definition as a macro in the
specified environment.

If so, the macro expansion function, a function of two arguments, is
returned.  If SYMBOL has no function definition in the lexical
environment ENVIRONMENT, or its definition is not a macro,
MACRO-FUNCTION returns NIL.

It is possible for both MACRO-FUNCTION and SPECIAL-OPERATOR-P to
return true of SYMBOL.  The macro definition must be available for use
by programs that understand only the standard Common Lisp special
forms.

Affected By:

(SETF MACRO-FUNCTION), DEFMACRO, and MACROLET.

Exceptional Situations:

The consequences are undefined if ENVIRONMENT is non-NIL in a use of
SETF of MACRO-FUNCTION."""
        ## Unregistered Issue COMPLIANCE-MACRO-FUNCTION-PROVIDED-ENVIRONMENT-IGNORED
        _nonep(environment) or _not_implemented("query of environments other than global")
        return the((or_, function_, null_), the(symbol, symbol_).macro_function)

def setf_macro_function(new_function, symbol, environment = None):
        "<See documentation for MACRO-FUNCTION>"
        if symbol.function:
                style_warn("%s is being redefined as a macro when it was previously defined to be a function.",
                           symbol)
                symbol.function = nil
        _warn_possible_redefinition(gethash("macro_function", symbol.__dict__)[0], _intern0(defmacro_))
        symbol.macro_function = new_function
        return new_function

# Research Issue CL-DIFFERENCE-BETWEEN-SETF-SYMBOL-FUNCTION-SETF-FDEFINITION-AND-DEFUN
# ..because, this far, DEFMACRO wins over SETF-FDEFINITION -- only DEFUN is necessary to switch namespace..
def setf_fdefinition(function_name, new_definition):
        """fdefinition function-name => definition

(setf (fdefinition function-name) new-definition)

Arguments and Values:

FUNCTION-NAME---a function name. In the non-SETF case, the name must be fbound in the global environment.

DEFINITION---Current global function definition named by FUNCTION-NAME.

NEW-DEFINITION---a function.

Description:

FDEFINITION accesses the current global function definition named by
FUNCTION-NAME.  The definition may be a function or may be an object
representing a special form or macro.  The value returned by
FDEFINITION when FBOUNDP returns true but the FUNCTION-NAME denotes a
macro or special form is not well-defined, but FDEFINITION does not
signal an error."""
        namestring = string(function_name)
        # Issue GLOBALS-SPECIFIED-TO-REFER-TO-THE-CONTAINING-MODULE-NOT-THE-CALLING-ONE
        symbol, therep = _global(namestring)
        if not therep:
                symbol = _intern0(namestring)
                _setf_global(namestring, symbol)
        symbol.function = new_definition
        return new_definition

def special_operator_p(symbol):
        """Syntax:

special-operator-p symbol => generalized-boolean

Arguments and Values:

SYMBOL---a symbol.

GENERALIZED-BOOLEAN---a generalized boolean.

Description:

Returns true if SYMBOL is a special operator; otherwise, returns
false.

Exceptional Situations:

Should signal TYPE-ERROR if its argument is not a symbol."""
        

def _warn_incompatible_function_redefinition(symbol, tons, fromns):
        style_warn("%s is being redefined as a %s when it was previously defined to be a %s.",
                           symbol, tons, fromns)
def _install_function_definition(function):
        return _do_install_function_definition(_intern0(function.__name__), function)
def _do_install_function_definition(x, function):
        if the(symbol, x).function:
                _warn_incompatible_function_redefinition(x, "function", "macro")
        else:
                _warn_possible_redefinition(x.function, the(symbol, defun_))
        x.function = value
        _make_object_like_python_function(x, function)
        return value

def _cold_defun(f):
        symbol_name = f.__name__
        setf_fdefinition(symbol_name, f)
        # Issue GLOBALS-SPECIFIED-TO-REFER-TO-THE-CONTAINING-MODULE-NOT-THE-CALLING-ONE
        return _global(symbol_name)[0] # guaranteed to exist at this point
defun = _cold_defun

class symbol():
        def __str__(self):
                return _print_symbol(self)
        def __repr__(self):
                return _str(self)
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
                error("Attempted to intern object >%s< of type %s into %s.", x, _type(x), p)
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

### handi-tools
def _intern0(x, package = None): return intern(the(string_, x),         package)[0]
# requires that __keyword_package__ is set, otherwise _intern will fail with _COERCE_TO_PACKAGE
def _keyword(s, upcase = True):
        return _intern((s.upper() if upcase else s), __keyword_package__)[0]
def _i(x):                       return intern(the(string_, x).upper(), None)[0]
_k = _keyword

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
        symset = _set(symdict.values())
        for_interning = symset & _set(package.inherited)
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
        __packages__ = _dict()
        __builtins_package__ = package("BUILTINS", boot = True)
        __keyword_package__ = package("KEYWORD", ignore_python = True, boot = True)
        __modular_noise__ = _frozenset(_load_text_as_module("", "").__dict__)
        cl = package("CL", use = ["BUILTINS"], boot = True)
        intern(".", cl)

        t                  = _intern0("T", cl)       # Nothing much works without these..
        nil                = _intern0("NIL", cl)
        t.value, nil.value = t, nil     # Self-evaluation.
        nil.__contains__   = lambda _: False
        nil.__getitem__    = lambda _, __: nil
        nil.__length__     = lambda _: 0
        nil.__iter__       = lambda _: None
        nil.__reversed__   = lambda _: None
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
        in_package("CL")
        setq("_FEATURES_", [])
        setq("_MODULES_",  [])
_init_package_system_1()

def _init_package_system_2():
        "Is called once CL is loaded completely."
        in_package("COMMON-LISP-USER")

###
### Symbol-related thaw
###

def constantp(form, environment = None):
        """constantp form &optional environment => generalized-boolean

Arguments and Values:

FORM---a form.

environment---an environment object. The default is nil.

GENERALIZED-BOOLEAN---a generalized boolean.

Description:

Returns true if FORM can be determined by the implementation to be a
constant form in the indicated ENVIRONMENT;  otherwise, it returns
false indicating either that the form is not a constant form or that
it cannot be determined whether or not FORM is a constant form.

The following kinds of forms are considered constant forms:

* Self-evaluating objects (such as numbers, characters, and the
  various kinds of arrays) are always considered constant forms and
  must be recognized as such by CONSTANTP.

* Constant variables, such as keywords, symbols defined by Common Lisp
  as constant (such as NIL, T, and PI), and symbols declared as
  constant by the user in the indicated ENVIRONMENT using DEFCONSTANT
  are always considered constant forms and must be recognized as such
  by CONSTANTP.

* QUOTE forms are always considered constant forms and must be
  recognized as such by CONSTANTP.

* An implementation is permitted, but not required, to detect
  additional constant forms.  If it does, it is also permitted, but
  not required, to make use of information in the
  ENVIRONMENT.  Examples of constant forms for which CONSTANTP might or
  might not return true are: (SQRT PI), (+ 3 2), (LENGTH '(A B C)),
  and (LET ((X 7)) (ZEROP X)).

If an implementation chooses to make use of the environment
information, such actions as expanding macros or performing function
inlining are permitted to be used, but not required; however,
expanding compiler macros is not permitted.

Affected By:

The state of the global environment (e.g., which symbols have been
declared to be the names of constant variables)."""
        return (_isinstance(form, (integer, float, complex, string_)) or
                keywordp(form) or form in [t, nil, pi]                 or
                (_tuplep(form) and _len(form) == 2 and form[0] is quote_))

def null(x):
        return x is nil

def defvar(name, value = None, documentation = nil):
        "XXX: documentation, declaring as special"
        if not boundp(name) and value is not None:
                setq(name, value)
        return name

def defparameter(name, value, documentation = nil):
        "XXX: documentation, declaring as special"
        setq(name, value)
        return name

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
###
##
#

###
### Earlified streaming
###

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
###
##
#
__type_predicate_map__ = _dict()

## Remember, we return type mismatches in these predicates!
# Unregistered Issue NONE-VALUE-SAFETY
def _some_type_mismatch(type, xs):
        "Determines, whether some member of XS mismatches TYPE."
        return some(_type_mismatch, xs, _infinite(type))

@defun
def or_(x, type):
        return ((x, type, False) if _len(type) is 1 else
                let(mapcar(_type_mismatch, _infinite(x), _from(1, type)),
                    lambda mismatches:
                            (some(lambda m: m and m[2] and m, mismatches) or
                            (every(identity, mismatches) and (x, type, False)))))

@defun
def and_(x, type):
        return (nil       if _len(type) is 1 else
                some(_type_mismatch, _infinite(x), _from(1, type)))

@defun
def not_(x, type):
        return ((x, type, True) if _len(type) is not 2 else
                let(_type_mismatch(x, type[1]),
                    lambda m: ((x, type, False) if not m      else
                               m                if m and m[2] else
                               nil)))

@defun
def member_(x, type):
        return ((x not in _from(1, type)) and
                (x, type, False))

@defun
def maybe_(x, type):
        return ((x, type, True)  if _len(type) is not 2 else
                let(_type_mismatch(x, type[1]),
                    lambda m: (nil if not m      else
                               m   if ((m and m[2]) or
                                       not (x is nil or x is None)) else
                               nil)))

@defun
def list_(x, type):
        return ((x, type, True)  if _len(type) is not 2       else
                (x, type, False) if not _isinstance(x, _list) else
                some(_type_mismatch, x, _infinite(type[1])))

@defun
def satisfies_(x, type):
        return ((x, type, True) if ((_len(type) is not 2) or
                                    not functionp(type[1])) else
                ((not type[1](x)) and
                 (x, type, False)))

@defun
def eql_(x, type):
        return ((x, type, True) if _len(type) is not 2 else
                ((not eql(x, type[1])) and
                 (x, type, False)))

@defun
def tuple_(x, type):
        return ((x, type, False) if not (_tuplep(x) and _len(x) == _len(type) - 1) else
                some(_type_mismatch, x, _from(1, type)))
# Unregistered Issue TEACHABLE-TYPE-CHECKING-PRACTICE-AND-TOOL-CONSTRUCTION

@defun
def partuple_(x, type):
        return ((x, type, False) if not (_tuplep(x) and _len(x) >= _len(type) - 1) else
                some(_type_mismatch, x, type[1:]))

__variseq__ = (tuple_, (eql_, maybe_), t) # Meta-type, heh..
@defun
def varituple_(x, type):
        # correctness enforcement over speed?
        fixed_t, maybes_t = _prefix_suffix_if_not(_of_type(__variseq__), type[1:])
        if not every(_of_type(__variseq__), maybes_t):
                return (x, type, True)   # fail
        fixlen = _len(fixed_t)
        return ((x, type) if _len(x) < fixlen else
                some(_type_mismatch, x[:fixlen], fixed_t) or
                some(_type_mismatch, x[fixlen:], _infinite((or_,) + _tuple(t[1] for t in maybes_t))))

@defun
def lambda_list_(x, type):
        if type:
                return (x, type, True) # fail
        return typep(x, (tuple_,
                         (list_, _str),
                         (list_, _str),
                         (maybe_, _str),
                         (list_, _str),
                         (maybe_, _str)))

def deftype(name, test):
        "XXX: should analyse the lambda list of TEST.."
        __type_predicate_map__[name] = test
        return name

## Remember, the used predicates must return type mismatches!
deftype(or_,          or_)
deftype(and_,         and_)
deftype(not_,         not_)
deftype(member_,      member_)
deftype(eql_,         eql_)
deftype(satisfies_,   satisfies_)
# XXX: this is a small lie: this is not a cons-list
# ..but neither CL has a type specifier like this and the others, that follow..
deftype(list_,        list_)
deftype(maybe_,       maybe_)
deftype(tuple_,       tuple_)
deftype(partuple_,    partuple_)
deftype(varituple_,   varituple_)
# deftype(lambda_list_, lambda_list_)

###
### Rich AST
###
## 1. Free variables.
##
## - except for special cases, it (the set of FV) is (mapsetn #'_ir_free_vars (ir-walkable-fields o))
## - walkable fields are:
##   - fields annotated as being of type __ast_walkable_field_types__
##   - fields, for which there is a "walk" declaration
## - special cases are:
##   - Name:                { self.id }
##   - Lambda:              (+ (free self.args) (- (free self.body) (binds self.args)))
##   - *Comp|GeneratiorExp: It could be as simple as:
##                              (+ (free self.generators) (- (free self.elt) (binds self.generators)))
##                          ..however, it's more complex than that: succeeding generators are evaluated
##                          in a lexical environment extended by bindings established by previous generators:
##                          (m-v-bind (free bound)
##                              (labels ((gch-free (xs acc-binds)
##                                         (if xs
##                                             (m-v-bind (cfree finbound)
##                                                 (gch-free (rest xs) (+ acc-binds (binds (first xs))))
##                                               (values (+ (- (free (first xs)) acc-binds) cfree)
##                                                       finbound))
##                                             (values nil acc-binds))))
##                                (gch-free self.generators ()))
##                            (values free bound))
##                          NOTE: it was a neat exercise, and it much helped to shape the thought
##                          process documented below.
##   - arguments:     free: (+ (free self.args) (free self.varargannotation) (free self.defaults)
##                             (free self.kwonlyargs) (free self.kwargannotation) (free self.kwdefaults))
## Intercession: if we decide to go with the upward out-bounded-ness protocol,
##               when do we stop propagation?  The details, so far apparent, are:
##               - default to upward propagation
##               - customize at parents "owning" the bindings
##               - must cooperate with free var computation, for proper free cancellation
##                 - single, shared pass?
##               - Q: whether all bindings affect free vars the same way?
##                 - honest FunctionDef/Lambda/comprehension/With bindings vs. *Assign/For
##                   - the owner is calculated much the same way, modulo global/nonlocal
##                   - the relevance of the possibility of actual un"bound"ed-ness..
##                     ..patchable by the means of _builtins.locals()? : -D
##                   - we're bound (ha) to be overly-optimistic about "bound"ed-ness, as,
##                     due to the lack of CFA, we must be.
##### The thought process stopped here:
##   - FunctionDef:         (+ (refs self.decorators) (refs self.args) (refs self.annotations)
##                             (- (free self.body) (binds self.args)))
##   - ClassDef:            (+ (refs self.decorators) (refs self.classargs)
##                             (- (free self.body) (binds self.classargs)))
##   - comprehension:       (+ (refs self.iter) (refs self.ifs) <see below>)
##                          binds are:
##                          - outward, not for "self"
##                          - only for Names being direct children of target, List, Tuple and Starred;
##                          the rest are deemed refs;
##                          - possibly mutating structure.
##                          It is, therefore, concluded, that comprehensions must not be processed
##                          separately, but rather in part of parent *Comp/Generator processing.
##   - *Assign:             Neatly solved by upward out-boundedness protocol. 
##   - Import*:             Same problem as *Assign (almost: can only rebind names, not mutate structure).
##   - Global/Nonlocal:     Related to the same problem as *Assign.
##   - For:                 (+ (refs self.target <see the above complication in comprehension>)
##                             (refs self.iter) (- (+ (free self.body) (free self.orelse))
##                                                 (binds self.target <again, above complication>)))
##                          Same problem as *Assign.
##   - With:                only (trivial) complication is potential None-ness of optional_vars
##   - TryExcept/ExceptHandler:    much the same situation as with *Comp|GeneratiorExp/comprehension,
##                          and much the same solution
##
_ast_info = defstruct("_ast_info",
                      "fields",     # each field is _dict(name, type, walk, [default])
                      "bound_free")
__ast_walkable_field_types__ = _set([ast.stmt, (list_, ast.expr), (maybe_, ast.expr),
                                     ast.expr, (list_, ast.stmt)])
__ast_infos__ = _dict()
def _find_ast_info(type):
        return __ast_infos__[_coerce_to_ast_type(type)]
def _ast_fields(ast): return _find_ast_info(type(ast)).fields

def _ast_ensure_stmt(x):
        return x if typep(x, ast.stmt) else ast.Expr(the(ast.AST, x))

defvar("_BOUND_FREE_RECURSOR_")
def _bound_free_recursor():
        return symbol_value("_BOUND_FREE_RECURSOR_")

def _ast_bound_free(astxs):
        def ast_rec(astxs):
                def bound_free(ast):
                        info = _find_ast_info(type_of(ast))
                        return info.bound_free(*mapcar(_slot_of(ast), _type(ast)._fields))
                return _separate(3, bound_free, remove(None, _ensure_list(astxs)))
        with progv(_BOUND_FREE_RECURSOR_ = ast_rec):
                return ast_rec(the((or_, ast.AST, (list_, ast.AST)),
                                   astxs))

def _atree_bound_free(atreexs):
        def atree_rec(atreexs):
                def bound_free(atree):
                        type = _coerce_to_ast_type(atree[0])
                        info = _find_ast_info(type)
                        return info.bound_free(*atree[1:])
                return _separate(3, bound_free, remove(None, _ensure_list(atreexs)))
        with progv(_BOUND_FREE_RECURSOR_ = atree_rec):
                return atree_rec(the((or_, _tuple, (list_, _tuple)),
                                     atreexs))

def _atree_bound(atree): return _atree_bound_free(atree)[0]
def _atree_free(atree):  return _atree_bound_free(atree)[1]
def _atree_xtnls(atree): return _atree_bound_free(atree)[2]

def defast(fn):
        ### generic tools
        def parse_defbody_ast(names, asts, valid_declarations = _dict()):
                def _ast_call_to_name_p(name, x):
                        return (typep(x, ast.Expr)            and
                                typep(x.value, ast.Call)      and
                                typep(x.value.func, ast.Name) and
                                x.value.func.id == name)
                def extract_sexp(ast_):
                        import more_ast
                        return (x.id                                       if _isinstance(ast_, ast.Name)  else
                                x.n                                        if _isinstance(ast_, ast.Num)   else
                                _tuple(extract_sexp(x) for x in ast_.elts) if _isinstance(ast_, ast.Tuple) else
                                error("Invalid sexp: %s.", more_ast.pp_ast_as_code(ast_)))
                def ensure_valid_declarations(decls):
                        # Unregistered Issue ENSURE-VALID-DECLARATION-SUGGESTS-FASTER-CONVERGENCE-TO-METASTRUCTURE
                        def fail():
                                import more_ast
                                err("invalid declaration form: %s", more_ast.pp_ast_as_code(decls))
                        def ensure_valid_declaration(decl):
                                typep(decl, ast.Tuple) and decl.elts and typep(decl.elts[0], ast.Name) or fail()
                                decl_name = decl.elts[0].id
                                if decl_name not in valid_declarations:
                                        err("unknown declaration: %s", decl_name.upper())
                                n_decl_args = valid_declarations[decl_name]
                                if _len(decl.elts) < 1 + n_decl_args + 1:
                                        err("invalid declaration %s: no parameter names specified", decl_name.upper())
                                every(_of_type(ast.Name), decl.elts[1 + n_decl_args:]) or fail()
                                decl_param_names = _tuple(x.id for x in decl.elts[1 + n_decl_args:])
                                unknown_param_names = _set(decl_param_names) - _set(names)
                                if unknown_param_names:
                                        err("invalid declaration %s: invalid parameter names: %s",
                                            decl_name.upper(), ", ".join(x.upper() for x in unknown_param_names))
                                return (decl_name,
                                        _tuple(extract_sexp(x) for x in decl.elts[1:1 + n_decl_args]),
                                        decl_param_names)
                        not (decls.keywords or decls.starargs or decls.kwargs) or fail()
                        return mapcar(ensure_valid_declaration, decls.args)
                content, _ = _prefix_suffix_if(_not_of_type(ast.Pass), asts)
                documentation, body = ((content[0], content[1:]) if content and stringp(content[0]) else
                                       (nil, content))
                declarations, body = _prefix_suffix_if(_curry(_ast_call_to_name_p, "declare"), body)
                return body, documentation, mapcan(lambda dexcall: ensure_valid_declarations(dexcall.value),
                                                   declarations)
        def group_declarations(valid_declspecs, decls):
                def _declaration_names(x): return _set(x[1 + valid_declspecs[x[0]]:])
                return { name: _set(d[0:2] for d in decls
                                   if name in d[2:])
                         for name in _mapsetn(_declaration_names, decls) } # INDEXING..
        def declaredp(grouped_decls, x, as_):
                return x in grouped_decls and (as_,) in grouped_decls
        def lambda_list_names(lambda_list, remove_optional = t):
                (fixed, optional, args, keyword, keys) = lambda_list
                xform = car if remove_optional else identity
                return (_tuple(fixed) +
                        _tuple(xform(x) for x in optional) + (_tuple() if not args else (args,)) +
                        _tuple(xform(x) for x in keyword)  + (_tuple() if not keys else (keys,)))
        ### end-of-generic-tools
        def err(format_control, *format_args):
                error(("In DEFAST %s: " % fn.__name__) + format_control + ".", *format_args)
        def validate_defast_name(name):
                if not name.startswith("_ast_"):
                        err("the AST name must be prefixed with \"_ast_\"")
                name = name[5:]
                ast_type, therep = gethash(name, ast.__dict__)
                if not therep:
                        err("'%s' does not denote a known AST type", name)
                return name, ast_type
        name, ast_type = validate_defast_name(fn.__name__)
        def validate_defast_lambda_list(ast_type, lambda_list, annotations):
                (fixed, optional, args, keyword, keys) = lambda_list
                if args or keyword or keys:
                        err("only fixed and optional arguments are allowed")
                ast_field_names = fixed + mapcar(car, optional)
                ast_field_names_with_defaults = fixed + optional
                ast_field_types = mapcar(lambda name: annotations[name], ast_field_names)
                if _len(ast_field_types) != _len(ast_type._fields):
                        err("the amount of provided type specifiers (%d) does not match the AST _fields: %s",
                            _len(ast_field_types), ast_type._fields)
                type = (or_, _tuple, _type)
                if not every(_of_type(type), ast_field_types):
                        err("the AST field type specifiers must be of type %s", type)
                for i, (fname, ast_fname) in enumerate(zip(ast_field_names, ast_type._fields)):
                        if fname != ast_fname:
                                err("the provided name for the %d'th field (%s) does not match its actual name (%s), expected field names: %s",
                                    i, fname, ast_fname, ast_type._fields)
                return ast_field_types
        def arglist_field_infos(parameters, nfix, with_defaults, ast_field_types):
                fields = _odict()
                def process_ast_field_arglist_entry(name, type, default, fixed = t):
                        walkp = (type in __ast_walkable_field_types__ or
                                 declaredp(grouped_decls, p, "walk"))
                        fields[p] = (_dict(name = name, type = type, walk = walkp) if fixed else
                                     _dict(name = name, type = type, walk = walkp, default = default))
                for p, type, defaulted in zip(parameters[:nfix], ast_field_types[:nfix], with_defaults[:nfix]):
                        process_ast_field_arglist_entry(p, type, None,         fixed = t)
                for p, type, defaulted in zip(parameters[nfix:], ast_field_types[nfix:], with_defaults[nfix:]):
                        process_ast_field_arglist_entry(p, type, defaulted[1], fixed = nil)
                return fields
        def body_methods(arguments_ast, fields, body_ast):
                def make_default_bound_free(name):
                        # compile down to:
                        # return _separate(3, _ast_bound_free, [<fields>])
                        # ..where <fields> is [ f["name"] for f in fields if f["walk"] ]
                        return ast.FunctionDef(
                                name, arguments_ast,
                                [ast.Return(
                                 _ast_funcall("_separate",
                                              [ 3,
                                                _ast_funcall("_bound_free_recursor", []),
                                                # ast.Name("_ast_bound_free", ast.Load()),
                                                ast.List([ ast.Name(f["name"], ast.Load())
                                                           for f in fields.values()
                                                           if f["walk"] ],
                                                         ast.Load()) ]))],
                                [], None)
                valid_methods = [("bound_free", make_default_bound_free)]
                def fail(x):
                        import more_ast
                        err("definition body may only contain definitions of %s methods, encountered: %s",
                            (", ".join([x.upper() for x, _ in
                                        valid_methods[:-1]]) +
                             (" and " if _len(valid_methods) > 1 else "") +
                             valid_methods[-1][0].upper()),
                             x if stringp(x) else more_ast.pp_ast_as_code(x))
                not_fdefn = find_if_not(_of_type(ast.FunctionDef), body_ast)
                if not_fdefn:
                        fail(not_fdefn)
                specified_method_names = { x.name:x for x in body_ast }
                invalid_methods = _set(specified_method_names) - _mapset(_indexing(0), valid_methods)
                if invalid_methods:
                        fail(invalid_methods.pop().upper())
                def process(method_name, default_maker):
                        "Return a validated and normalised named method body 'return'-wise."
                        x = find(method_name, body_ast, key = _slotting("name"))
                        method_name = "_ast_%s_%s" % (name, method_name)
                        if x:
                                x.name, x.args = method_name, arguments_ast # Splice in the common arglist.
                                if _len(x.body) > 1:
                                        if not typep(x.body[-1], ast.Return):
                                                err("multi-line methods must use an explicit return statement")
                                elif not(typep(x.body[0], ast.Return)):
                                        x.body[0] = ast.Return(x.body[0].value)
                        import more_ast
                        # _debug_printf("AST for %s.%s:\n%s", name, method_name, more_ast.pp_ast_as_code(x or default_maker(method_name)))
                        return _ast_compiled_name(method_name, x or default_maker(method_name),
                                                  locals_ = _builtins.locals(), globals_ = _builtins.globals())
                return (process(*mspec) for mspec in valid_methods)
        lambda_list = (fixed, optional, args, keyword, keys) = _function_lambda_list(fn, astify_defaults = nil)
        ast_field_types = validate_defast_lambda_list(ast_type, lambda_list, fn.__annotations__)
        parameters, with_defaults = (lambda_list_names(lambda_list),
                                     lambda_list_names(lambda_list, remove_optional = nil))
        args_ast, body_ast = _function_ast(fn)
        valid_declspecs = _dict(walk  = 0)
        body, documentation, declarations = parse_defbody_ast(parameters, body_ast,
                                                              valid_declarations = valid_declspecs)
        grouped_decls = group_declarations(valid_declspecs, declarations)
        fields = arglist_field_infos(parameters, _len(fixed), with_defaults, ast_field_types)
        [bound_free] = body_methods(args_ast, fields, remove_if(_of_type(ast.Pass), body))
        # _debug_printf("bound_free for %s is %s", name, bound_free)
        __ast_infos__[ast_type] = _ast_info(fields     = fields,
                                            bound_free = bound_free)

###
### ATrees (low-level IR)
###
def _atree_ast(tree):
        """Flip an atree to its AST geminae.

An "atree" is a tree, where every element is one of the following:
 - an astifiable literal (according to _try_astify_constant/__astifier_map__),
   but not a tuple;
 - a tuple of length > 0, with the following structure:
   - the 0'th element is a string, naming a class in the "ast" module
   - the rest of the elements are atrees.

The set of all atrees enjoys an isomorphism relationship to the set of
all AST-trees .. except for the case of tuples."""
        def unknown_ast_type_error(x, node):
                error("Unknown AST type %s in atree node %s.", x, node)
        def argument_count_error(min, max, given, control, *args):
                error("%s requires between %d and %d arguments, but %d were given.", (control % args), min, max, given)
        def argument_type_error(name, expected_type, defacto_value, control, *args):
                error("The argument \"%s\" of %s must be of type %s, but was a %s.  Tree: %s.",
                      name, (control % args), expected_type, princ_to_string(defacto_value), tree)
        def astify_known(type, args):
                ast_type = ast.__dict__[type]
                info = _find_ast_info(ast_type)
                fields, finfos = _recombine((_list, _list), identity, _list(info.fields.items()))
                positional, optional = _prefix_suffix_if(lambda x: "default" in x, finfos)
                nfixed, defacto = _len(positional), _len(args)
                max = nfixed + _len(optional)
                if not (nfixed <= defacto <= max):
                        argument_count_error(nfixed, max, defacto, "AST type %s", type)
                effective_args = args + mapcar(_indexing("default"), optional[defacto - nfixed:])
                assert(_len(effective_args) == max)
                for val, name, finfo in _zip(effective_args, fields, finfos):
                        subtype = finfo["type"]
                        if not typep(val, subtype):
                                argument_type_error(name, subtype, val, "AST node %s", repr(type))
                return ast_type(*effective_args)
        ret =  (tree
                if typep(tree, (or_, string_, integer, (eql_, None))) else
                mapcar(_atree_ast, tree)
                if _listp(tree)                  else
                _try_astify_constant(tree)[0]
                if not _tuplep(tree)            else
                error("The atree nodes cannot be zero-length.")
                if not tree                     else
                error("The CAR of an atree must be a string, not %s.", tree[0])
                if not stringp(tree[0])         else
                unknown_ast_type_error(tree[0], tree) if tree[0] not in ast.__dict__  else
                (astify_known(tree[0], mapcar(_atree_ast, _from(1, tree)))))
        return ret

# mod = Module(stmt* body)
#     | Interactive(stmt* body)
#     | Expression(expr body)
@defast
def _ast_Module(body: (list_, ast.stmt)): pass
@defast
def _ast_Interactive(body: (list_, ast.stmt)): pass
@defast
def _ast_Expression(body: ast.expr): pass
# stmt = FunctionDef(identifier name,
#                    arguments args,
#                    stmt* body,
#                    expr* decorator_list,
#                    expr? returns)
# >>> a = 0
# >>> def damage():
# ...         a = 42
# ...         def inner():
# ...                 if 2:
# ...                         a = 1
# ...                 if 0:
# ...                         global a
# ...                 # elif 0: # errs: SyntaxError: name 'a' is nonlocal and global
# ...                 #         nonlocal a
# ...         inner()
# ... 
# <stdin>:7: SyntaxWarning: name 'a' is assigned to before global declaration
# >>> damage()
# >>> print(a)
# 1
# >>> def damage():
# ...         a = 42
# ...         def inner(a):
# ...                 if 2:
# ...                         a = 1
# ...                 if 0:
# ...                         global a
# ...                 # elif 0: # errs: SyntaxError: name 'a' is nonlocal and global
# ...                 #         nonlocal a
# ...         inner(1)
# ... 
# <stdin>:7: SyntaxWarning: name 'a' is assigned to before global declaration
#   File "<stdin>", line 3
# SyntaxError: name 'a' is parameter and global
@defast
def _ast_FunctionDef(name:            string_,
                     args:            ast.arguments,
                     body:           (list_, ast.stmt),
                     decorator_list: (list_, ast.expr) = _list(),
                     returns:        (maybe_, ast.expr) = None):
        def bound_free():
                ((args_b, args_f, _),
                 (body_b, body_f, body_x),
                 (_,      deco_f, _),
                 (_,      retn_f, _)) = mapcar(_bound_free_recursor(), [args, body, decorator_list, returns])
                body_bound = _set([name]) | args_b | (body_b - body_x)
                body_free = body_f - body_bound
                body_xtnl_writes = body_b & body_x
                free = args_f | body_free | deco_f | retn_f
                return (body_xtnl_writes, # names declared global/nonlocal and assigned to
                        free,
                        _set())            # these do not escape..
#       | ClassDef(identifier name,
# 		   expr* bases,
# 		   keyword* keywords,
# 		   expr? starargs,
# 		   expr? kwargs,
# 		   stmt* body,
# 		   expr* decorator_list)
@defast
def _ast_ClassDef(name:            string_,
                  bases:          (list_, ast.expr),
                  keywords:       (list_, ast.keyword),
                  starargs:       (maybe_, ast.expr),
                  kwargs:         (maybe_, ast.expr),
                  body:           (list_, ast.stmt),
                  decorator_list: (list_, ast.expr)):
        def bound_free():
                ((base_b, base_f, _),
                 (keyw_b, keyw_f, _),
                 (star_b, star_f, _),
                 (karg_b, karg_f, _),
                 (body_b, body_f, body_x),
                 (deco_b, deco_f, _)) = mapcar(_bound_free_recursor(), [bases, keywords, starargs, kwargs, body, decorator_list])
                # Unregistered Issue CLASS-BINDINGS-UNCLEAR
                body_bound = body_b - body_x
                body_free = body_f - body_bound
                body_xtnl_writes = body_b & body_x
                free = base_f | keyw_f | star_f | karg_f | body_free | deco_f
                return (body_xtnl_writes, # names declared global/nonlocal and assigned to
                        free,
                        _set())            # these do not escape..
#       | Return(expr? value)
@defast
def _ast_Return(value: (maybe_, ast.expr)): pass
#       | Delete(expr* targets)
@defast
def _ast_Delete(targets: (list_, ast.expr)): pass
        # targets do ref, in this case!
#       | Assign(expr* targets, expr value)
@defast
def _ast_Assign(targets: (list_, ast.expr),
                value:    ast.expr):
        def bound_free():
                ((targ_b, targ_f, _),
                 (_,      valu_f, _)) = mapcar(_bound_free_recursor(), [targets, value])
                return (targ_b,
                        targ_f | valu_f,
                        _set())
#       | AugAssign(expr target, operator op, expr value)
@defast
def _ast_AugAssign(target: ast.expr,
                   op:     ast.operator,
                   value:  ast.expr):
        def bound_free():
                ((targ_b, targ_f, _),
                 (_,      valu_f, _)) = mapcar(_bound_free_recursor(), [target, value])
                return (targ_b,
                        targ_f | valu_f,
                        _set())

def _ast_body_bound_free(body, more_bound = _set()):
        body_b, body_f, body_x = _bound_free_recursor()(body)
        bound = more_bound | (body_b - body_x)
        return (bound,
                body_f - bound,
                body_b & body_x)

#       | For(expr target, expr iter, stmt* body, stmt* orelse)
@defast
def _ast_For(target:  ast.expr,
             iter:    ast.expr,
             body:   (list_, ast.stmt),
             orelse: (list_, ast.stmt)):
        def bound_free():
                ((targ_b, targ_f, _),
                 (_,      iter_f, _)) = mapcar(_bound_free_recursor(), [target, iter])
                # Unregistered Issue HOLE-ORELSE-CAN-USE-BODY-BINDINGS
                (bound, free, xtnls) = _separate(3, _ast_body_bound_free, [body, orelse])
                return (bound,
                        targ_f | iter_f | free,
                        xtnls)

#       | While(expr test, stmt* body, stmt* orelse)
@defast
def _ast_While(test:    ast.expr,
               body:   (list_, ast.stmt),
               orelse: (list_, ast.stmt)):
        def bound_free():
                ((_, test_f, _)) = _bound_free_recursor()(test)
                # Unregistered Issue HOLE-ORELSE-CAN-USE-BODY-BINDINGS
                (bound, free, xtnls) = _separate(3, _ast_body_bound_free, [body, orelse])
                return (bound,
                        free | test_f,
                        xtnls)
#       | If(expr test, stmt* body, stmt* orelse)
@defast
def _ast_If(test:    ast.expr,
            body:   (list_, ast.stmt),
            orelse: (list_, ast.stmt)):
        def bound_free():
                ((_, test_f, _)) = _bound_free_recursor()(test)
                # Unregistered Issue HOLE-ORELSE-CAN-USE-BODY-BINDINGS
                (bound, free, xtnls) = _separate(3, _ast_body_bound_free, [body, orelse])
                return (bound,
                        free | test_f,
                        xtnls)
#       | With(expr context_expr, expr? optional_vars, stmt* body)
@defast
def _ast_With(context_expr:   ast.expr,
              optional_vars: (maybe_, ast.expr),
              body:          (list_, ast.stmt)):
        def bound_free():
                ((_,      ctxt_f, _),
                 (optl_b, optl_f, _)) = mapcar(_bound_free_recursor(), [context_expr, optional_vars])
                body_bound, body_free, body_xtnls = _ast_body_bound_free(body, optl_b)
                return (body_bound,
                        ctxt_f | optl_f | body_free,
                        body_xtnls)
#       | Raise(expr? exc, expr? cause)
@defast
def _ast_Raise(exc:   (maybe_, ast.expr),
               cause: (maybe_, ast.expr)): pass
#       | TryExcept(stmt* body, excepthandler* handlers, stmt* orelse)
@defast
def _ast_TryExcept(body:     (list_, ast.stmt),
                   handlers: (list_, ast.excepthandler),
                   orelse:   (list_, ast.stmt)):
        # Unregistered Issue HOLE-ORELSE-CAN-USE-BODY-BINDINGS
        def bound_free(): _separate(3, _ast_body_bound_free, [body, handlers, orelse])
#       | TryFinally(stmt* body, stmt* finalbody)
@defast
def _ast_TryFinally(body:      (list_, ast.stmt),
                    finalbody: (list_, ast.stmt)):
        # Unregistered Issue HOLE-ORELSE-CAN-USE-BODY-BINDINGS
        def bound_free(): _separate(3, _ast_body_bound_free, [body, handlers, orelse])
#       | Assert(expr test, expr? msg)
@defast
def _ast_Assert(test: ast.expr,
                msg:  ast.expr = None): pass
#       | Import(alias* names)
@defast
def _ast_Import(names: (list_, ast.alias)):
        declare((walk, names))
#       | ImportFrom(identifier? module, alias* names, int? level)
@defast
def _ast_ImportFrom(module: (maybe_, string_),
                    names:  (list_, ast.alias),
                    level:  (maybe_, integer)):
        def bound_free():
                return (_bound_free_recursor()(names)[0],
                        _set([module] if module else []),
                        _set())
#       | Global(identifier* names)
@defast
def _ast_Global(names: (list_, string_)):
        def bound_free(): (_set(), _set(), _set(names))
#       | Nonlocal(identifier* names)
@defast
def _ast_Nonlocal(names: (list_, string_)):
        def bound_free(): (_set(), _set(), _set(names))
#       | Expr(expr value)
@defast
def _ast_Expr(value: ast.expr): pass
#       | Pass | Break | Continue
@defast
def _ast_Pass(): pass
@defast
def _ast_Break(): pass
@defast
def _ast_Continue(): pass
# expr = BoolOp(boolop op, expr* values)
@defast
def _ast_BoolOp(op:      ast.boolop,
                values: (list_, ast.expr)): pass
#      | BinOp(expr left, operator op, expr right)
@defast
def _ast_BinOp(left:  ast.expr,
               op:    ast.operator,
               right: ast.expr): pass
#      | UnaryOp(unaryop op, expr operand)
@defast
def _ast_UnaryOp(op:      ast.unaryop,
                 operand: ast.expr): pass
#      | Lambda(arguments args, expr body)
@defast
def _ast_Lambda(args: ast.arguments,
                body: ast.expr):
        def bound_free():
                ((args_b, args_f, _),
                 (_,      body_f, _)) = mapcar(_bound_free_recursor(), [args, body])
                body_free = body_f - args_b
                free = args_f | body_free
                return (_set(),
                        free,
                        _set())
#      | IfExp(expr test, expr body, expr orelse)
@defast
def _ast_IfExp(test:   ast.expr,
               body:   ast.expr,
               orelse: ast.expr): pass
#      | Dict(expr* keys, expr* values)
@defast
def _ast_Dict(keys:   (list_, ast.expr),
              values: (list_, ast.expr)): pass
#      | Set(expr* elts)
@defast
def _ast_Set(elts: (list_, ast.expr)): pass
#      | ListComp(expr elt, comprehension* generators)

def _ast_gchain_bound_free(xs, acc_binds):
        if xs:
                g_binds, g_free, _ = _bound_free_recursor()(xs[0])
                finbound, cfree = _ast_gchain_bound_free(xs[1:], acc_binds | g_binds)
                return finbound, (g_free - acc_binds) | cfree
        else:
                return acc_binds, _set()

def _ast_comprehension_bound_free(exprs, generators):
        gchain_bound, gchain_free = _ast_gchain_bound_free(generators, _set())
        _, exprs_f, _ = _separate(3, _bound_free_recursor(), exprs)
        return (_set(),
                gchain_free | (exprs_f - gchain_bound),
                _set())

@defast
def _ast_ListComp(elt:         ast.expr,
                  generators: (list_, ast.comprehension)):
        def bound_free(): _ast_comprehension_bound_free([elt], generators)
#      | SetComp(expr elt, comprehension* generators)
@defast
def _ast_SetComp(elt:         ast.expr,
                 generators: (list_, ast.comprehension)):
        def bound_free(): _ast_comprehension_bound_free([elt], generators)
#      | DictComp(expr key, expr value, comprehension* generators)
@defast
def _ast_DictComp(key:        ast.expr,
                  value:      ast.expr,
                  generators: (list_, ast.comprehension)):
        def bound_free(): _ast_comprehension_bound_free([key, value], generators)
#      | GeneratorExp(expr elt, comprehension* generators)
@defast
def _ast_GeneratorExp(elt:         ast.expr,
                      generators: (list_, ast.comprehension)):
        def bound_free(): _ast_comprehension_bound_free([elt], generators)
#      | Yield(expr? value)
@defast
def _ast_Yield(value: (maybe_, ast.expr) = None): pass
#      | Compare(expr left, cmpop* ops, expr* comparators)
@defast
def _ast_Compare(left:         ast.expr,
                 ops:         (list_, ast.cmpop),
                 comparators: (list_, ast.expr)): pass
#      | Call(expr func, expr* args, keyword* keywords, expr? starargs, expr? kwargs)
@defast
def _ast_Call(func:      ast.expr,
              args:     (list_, ast.expr),
              keywords: (list_, ast.keyword),
              starargs: (maybe_, ast.expr) = None,
              kwargs:   (maybe_, ast.expr) = None):
        def bound_free(): _separate(3, _bound_free_recursor(),
                                    [func, args, keywords, starargs, kwargs])
#      | Num(object n) -- a number as a PyObject.
@defast
def _ast_Num(n: int): pass
#      | Str(string s) -- need to specify raw, unicode, etc?
@defast
def _ast_Str(s: str): pass
#      | Bytes(string s)
@defast
def _ast_Bytes(s: str): pass
#      | Ellipsis
@defast
def _ast_Ellipsis(): pass
#      | Attribute(expr value, identifier attr, expr_context ctx)
@defast
def _ast_Attribute(value: ast.expr,
                   attr:  string_,
                   ctx:   ast.expr_context): pass
#      | Subscript(expr value, slice slice, expr_context ctx)
@defast
def _ast_Subscript(value: ast.expr,
                   slice: ast.slice,
                   ctx:   ast.expr_context):
        declare((walk, slice))
#      | Starred(expr value, expr_context ctx)
@defast
def _ast_Starred(value: ast.expr,
                 ctx:   ast.expr_context): pass
#      | Name(identifier id, expr_context ctx)
@defast
def _ast_Name(id:  string_,
              ctx: ast.expr_context):
        def bound_free(): ((_set(), _set([id])) if typep(ctx, (or_, ast.Load, ast.AugLoad, ast.Param)) else
                           (_set([id]), _set()))
#      | List(expr* elts, expr_context ctx)
@defast
def _ast_List(elts: (list_, ast.expr),
              ctx:   ast.expr_context): pass
#      | Tuple(expr* elts, expr_context ctx)
@defast
def _ast_Tuple(elts: (list_, ast.expr),
               ctx:   ast.expr_context): pass
# expr_context = Load | Store | Del | AugLoad | AugStore | Param
@defast
def _ast_Load(): pass
@defast
def _ast_Store(): pass
@defast
def _ast_AugLoad(): pass
@defast
def _ast_AugStore(): pass
@defast
def _ast_Param(): pass
# slice = Slice(expr? lower, expr? upper, expr? step)
@defast
def _ast_Slice(lower: (maybe_, ast.expr) = None,
               upper: (maybe_, ast.expr) = None,
               step:  (maybe_, ast.expr) = None): pass
#       | ExtSlice(slice* dims)
@defast
def _ast_ExtSlice(dims: (list_, ast.slice)):
        declare((walk, dims))
#       | Index(expr value)
@defast
def _ast_Index(value: ast.expr): pass
# boolop = And | Or
@defast
def _ast_And(): pass
@defast
def _ast_Or(): pass
# operator = Add | Sub | Mult | Div | Mod | Pow | LShift | RShift | BitOr | BitXor | BitAnd | FloorDiv
@defast
def _ast_Add(): pass
@defast
def _ast_Sub(): pass
@defast
def _ast_Mult(): pass
@defast
def _ast_Div(): pass
@defast
def _ast_Mod(): pass
@defast
def _ast_Pow(): pass
@defast
def _ast_LShift(): pass
@defast
def _ast_RShift(): pass
@defast
def _ast_BitOr(): pass
@defast
def _ast_BitXor(): pass
@defast
def _ast_BitAnd(): pass
@defast
def _ast_FloorDiv(): pass
# unaryop = Invert | Not | UAdd | USub
@defast
def _ast_Invert(): pass
@defast
def _ast_Not(): pass
@defast
def _ast_UAdd(): pass
@defast
def _ast_USub(): pass
# cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
@defast
def _ast_Eq(): pass
@defast
def _ast_NotEq(): pass
@defast
def _ast_Lt(): pass
@defast
def _ast_LtE(): pass
@defast
def _ast_Gt(): pass
@defast
def _ast_GtE(): pass
@defast
def _ast_Is(): pass
@defast
def _ast_IsNot(): pass
@defast
def _ast_In(): pass
@defast
def _ast_NotIn(): pass
# comprehension = (expr target, expr iter, expr* ifs)
@defast
def _ast_comprehension(target: ast.expr,
                       iter:   ast.expr,
                       ifs:   (list_, ast.expr)):
        def bound_free():
                ((_,      targ_f, _),
                 (iter_b, iter_f, _),
                 (_,      iffs_f, _)) = mapcar(_bound_free_recursor(), [target, iter, ifs])
                return (iter_b,
                        ((targ_f | iffs_f) - iter_b) | iter_f,
                        _set())
# excepthandler = ExceptHandler(expr? type, identifier? name, stmt* body)
@defast
def _ast_ExceptHandler(type: (maybe_, ast.expr),
                       name: (maybe_, str),
                       body: (list_, ast.stmt)):
        def bound_free():
                (_,      type_f, _) = _bound_free_recursor()(type)
                (bound, free, xtnls) = _ast_body_bound_free(body, _set([name] if name is not None else []))
                return (bound,
                        type_f | free,
                        xtnls)
# arguments = (arg* args, identifier? vararg, expr? varargannotation,
#              arg* kwonlyargs, identifier? kwarg,
#              expr? kwargannotation, expr* defaults,
#              expr* kw_defaults)
@defast
### These MAYBEs suggest a remapping facility.
def _ast_arguments(args:             (list_, ast.arg),
                   vararg:           (maybe_, str),
                   varargannotation: (maybe_, ast.expr),
                   kwonlyargs:       (list_, ast.arg),
                   kwarg:            (maybe_, str),
                   kwargannotation:  (maybe_, ast.expr),
                   defaults:         (list_, ast.expr),
                   kw_defaults:      (list_, ast.expr)):
        def bound_free():
                arg_bound, arg_free, _ = _separate(3, _bound_free_recursor(), [args, kwonlyargs])
                arg_bound |= _set(x for x in [vararg, kwarg]
                                  if x is not None)
                _, other_free, _ = _separate(3, _bound_free_recursor(), [varargannotation, kwargannotation, defaults, kw_defaults])
                return (arg_bound,
                        arg_free | other_free,
                        _set())
# arg = (identifier arg, expr? annotation)
@defast
def _ast_arg(arg:         string_,
             annotation: (maybe_, ast.expr) = None):
        def bound_free(): (_set([arg]),
                           _bound_free_recursor()(annotation)[1],
                           _set())
# keyword = (identifier arg, expr value)
@defast
def _ast_keyword(arg:   string_,
                 value: ast.expr):
        def bound_free(): (_set([] if _nonep(asname) else [asname]),
                           _bound_free_recursor()(value),
                           _set())
# alias = (identifier name, identifier? asname)
@defast
def _ast_alias(name:    string_,
               asname: (maybe_, string_) = None):
        def bound_free(): (_set([] if _nonep(asname) else [asname]),
                           _set(),
                           _set())
#####
####
###
##
#

def _anode_expression_p(x):
        return _tuplep(x) and issubclass(ast.__dict__(x[0]), ast.expr)

###
### A rudimentary Lisp -> Python compiler
###
__primitive_form_compilers__ = _dict()
def defprimitive(fn):
        name = fn.__name__
        fn.__name__ = "compile_" + fn.__name__
        sym, presentp = _global(name)
        if not presentp or not symbolp(sym):
                sym = _intern0(name.upper())
                _setf_global(name, sym)
        __primitive_form_compilers__[sym] = fn
        return sym # pass through
def _find_primitive(x):
        return gethash(x, __primitive_form_compilers__)

###
### Thunking is defined as code movement, introduced by the need for
### statement sequencing in AST nodes lowerable to IR incapable of sequencing.
### It is a kind of situation perceived to be rare for target IRs.
###
### From the standpoint of thunking, There are two kinds of expressions:
###  - thunk acceptors -- high-level IRs lowering to IRs "capable" of storing
###                       named functions with an arbitrary amount of subforms.
###    In short, anything lowering to:
###      - Module, Interactive
###      - FunctionDef, ClassDef
###      - For, While, If, With
###      - TryExcept, TryFinally, ExceptHandler
###  - thunk requestors -- high-level IRs with implicit PROGN (including PROGN itself),
###                        lowering to "incapable" IRs.
###
### Code movement introduces a lexical scope difference, the relevance of which is
###  in its effect on the free variables of the moved expression.
### This difference is twofold, in the context of Python:
###  - expression-wise de-scoping, where the difference is introduced by the
###    containing lambda expressions
###  - statement-wise scope change, with the differences introduced by assignment
###    statements (including destructuring)
### The second kind of difference can be avoided entirely, by placing the thunks,
###  generated by an expression, immediately before the statement containing the
###  expression.
###

def _compile_lispy_lambda_list(context, list_, allow_defaults = None):
        if not _tuplep(list_):
                error("In %s: lambda list must be a tuple.", list_)
        def valid_parameter_specifier_p(x): return stringp(x) or (symbolp(x) and not keywordp(x))
        test, failure_message = ((lambda x: valid_parameter_specifier_p(x) or (_tuplep(x) and _len(x) == 2 and
                                                              valid_parameter_specifier_p(x[0])),
                                 "In %s: lambda lists can only contain strings, non-keyword symbols and two-element lists, with said argument specifiers as first elements: %s.")
                                 if allow_defaults else
                                 (valid_parameter_specifier_p, "In %s: lambda list must consist of strings and non-keyword symbols: %s."))
        ### 0. locate lambda list keywords
        lambda_words = [_optional_, _rest_, _key_, _restkey_]
        optpos,  restpos,  keypos,  restkeypos = lambda_posns = mapcar(lambda x: position(x, list_), lambda_words)
        ### 1. ensure proper order of provided lambda list keywords
        optposp, restposp, keyposp, restkeyposp = mapcar(complement(_nonep), lambda_posns)
        def test_lambda_list_word_order():
                toptpos     = optpos or 0
                trestpos    = restpos or toptpos
                tkeypos     = keypos or trestpos
                trestkeypos = restkeypos or tkeypos
                if not toptpos <= trestpos <= tkeypos <= trestkeypos:
                        error("In %s: %s, %s, %s and %s must appear in that order in the lambda list, when specified.",
                              context, *lambda_words)
        test_lambda_list_word_order()
        # _locals_printf(_builtins.locals(),
        #                "optpos",  "restpos",  "keypos",  "restkeypos",
        #                "optposp", "restposp", "keyposp", "restkeyposp",
        #               "toptpos", "trestpos", "tkeypos", "trestkeypos")
        ### 2. ensure correct amount of names for provided lambda list keywords
        if (restposp and keyposp and (keypos - restpos != 1) or
            restposp and (not keyposp) and restkeyposp and (restkeypos - restpos != 1) or
            restkeyposp and (_len(list_) - restkeypos != 2)):
                error("In %s: found garbage instead of a lambda list: %s", context, list_)
        ### 3. compute argument specifier sets, as determined by provided lambda list keywords
        restkey = restkeyposp and list_[restkeypos + 1] or None
        _keys = _list(list_[keypos + 1:restkeypos or None]) if keypos else _tuple()
        keys, keydefs = (_list(_ensure_car(x)      for x in _keys),
                         _list(cdr(car(x)) or None for x in _keys))
        rest = restposp and list_[restpos + 1] or None
        _optional = _list(list_[optpos + 1:restpos or keypos or restkeypos or None]) if optposp else []
        optional, optdefs = (_list(_ensure_car(x)      for x in _optional),
                             _list(cdr(car(x)) or None for x in _optional))
        fixed = _list(list_[0:optpos or restpos or keypos or restkeypos or None])
        total = fixed + optional + (rest,) + keys + (restkey,) if restkey else _list()
        ### 4. validate syntax of the provided individual argument specifiers
        if not every(valid_parameter_specifier_p, total):
                error(failure_message, context, list_)
        ### 5. check for duplicate lambda list specifiers
        if _len(total) != _len(_set(total)):
                error("In %s: duplicate parameter names in lambda list: %s.", context, list_)
        ### 6. lower
        return ("arguments",
                mapcar(lambda x: ("arg", string(x)), fixed + optional),
                rest and string(rest), None,
                mapcar(lambda x: ("arg", string(x)), keys),
                restkey and string(restkey), None,
                optdefs,
                keydefs), (fixed, optional, rest, keys, restkey, total)

###
### Tuple intermediate IR
###
## A pro/val tuple:
## - prologue
## - value, can only contain ast.expr's
##
def _tuplerator(pve):
        for x in pve[0]:
                yield x
        if pve[1] is not None:
                yield pve[1]
def _tuple_empty_p(pve):      return not (pve[0] or pve[1])
def _tuple_expression_p(pve): return not (pve[0])
def _tuple_bound(pve):        return _mapsetn(_atree_bound, _tuplerator(the((tuple_, _list, _tuple), pve)))
def _tuple_free(pve):         return _mapsetn(_atree_free,  _tuplerator(the((tuple_, _list, _tuple), pve)))
def _tuple_xtnls(pve):        return _mapsetn(_atree_xtnls, _tuplerator(the((tuple_, _list, _tuple), pve)))

## Should, probably, be bound by the compiler itself.
defvar("_COMPILER_TOPLEVEL_P_", t)
defvar("_COMPILER_DEF_",        nil)
defvar("_COMPILER_QUOTE_",      nil)
defvar("_COMPILER_TAILP_",      nil)

defvar("_COMPILER_DEBUG_P_",    nil)

def _debug_compiler(value = t):
        setq("_COMPILER_DEBUG_P_", value)
def _debugging_compiler_p():
        return symbol_value("_COMPILER_DEBUG_P_")

__compiler_form_record__ = collections.defaultdict(lambda: 0)
__compiler_form_record_threshold__ = 5
def _compiler_track_compiled_form(form):
        cur = __compiler_form_record__[_id(form)]
        if cur > __compiler_form_record_threshold__:
                error("Apparent non-termination while compiling %s (happened over %d times).",
                      form, __compiler_form_record_threshold__)
        __compiler_form_record__[_id(form)] += 1

class _compiler_def(_servile):
        pass

def _compiling_def():     return symbol_value("_COMPILER_DEF_")
def _tail_position_p():   return symbol_value("_COMPILER_TAILP_")
def _compiling_quote_p(): return symbol_value("_COMPILER_QUOTE_")

def _compiler_report_context():
        _here("def %s\n      tailp: %s\n      quote: %s", *mapcar(symbol_value, ["_COMPILER_DEF_",
                                                                                 "_COMPILER_TAILP_",
                                                                                 "_COMPILER_QUOTE_"]))

_tail_position       = _defwith("_tail_position",
                                   lambda *_: _dynamic_scope_push(_dict(_COMPILER_TAILP_ = t)),
                                   lambda *_: _dynamic_scope_pop())

_maybe_tail_position = _defwith("_maybe_tail_position", # This is just a documentation feature.
                                   lambda *_: None,
                                   lambda *_: None)

_no_tail_position    = _defwith("_no_tail_position",
                                   lambda *_: _dynamic_scope_push(_dict(_COMPILER_TAILP_ = nil)),
                                   lambda *_: _dynamic_scope_pop())

_compiler_debug         = _defwith("_compiler_debug",
                                   lambda *_: _dynamic_scope_push(_dict(_COMPILER_DEBUG_P_ = t)),
                                   lambda *_: _dynamic_scope_pop())

#### Issue stack:
## Tail position optimisations
### LET optimisation
## Atree bfx queries
## Lisp-level bound/free
## Quote processing
## FUNCALL order of evaluation (closed?)
## is the value generally side-effect-free?

###                                      (QUAQUOTE)    <-
###                                      (COMMA)       <-

###                                      (SYMBOL)      <-
###                                      (SETF VALUES) <-
###                                      (RETURN)      <-
###                                      (QUOTE)       <-
###                 | (empty: SYMBOL) <- (PROGN)       <-
###      PROGN, RETURN, QUOTE, SYMBOL <- (DEF)         <-
### LAMBDA | LAMBDA, PROGN, SETQ, LET <- (LET)         <-
###            | SYMBOL, LET, FUNCALL <- (FUNCALL)     <-
###                          LET, DEF <- (FLET)        <-
###              PROGN | FLET, SYMBOL <- (LAMBDA)      <-
###                FLET, DEF, FUNCALL <- (LABELS)      <-
###                 PROGN | LET, LET* <- (LET*)        <-

@defprimitive # Critical-issue-free.
def symbol_(name):
        check_type(name, (or_, str, symbol))
        return ([],
                ("Name", string(name), ("Load",)))

def _compile_name(name, ctx = "Load"):
        check_type(name, (or_, str, symbol, (tuple_, (eql_, symbol_), (or_, str, symbol))))
        if _tuplep(name) and ctx != "Load":
                error("COMPILE-NAME: only 'Load' context possible while lowering (SYMBOL ..) forms.")
        return (compile_(name)[1] if _tuplep(name) else
                ("Name", string(name), ctx))

@defprimitive # Critical-issue-free.
def setq_(name, value):
        pro, val = compile_(value)
        return (pro + [("Assign", [_compile_name(name, "Store")])],
                _compile_name(name, "Load"))

@defprimitive # Critical-issue-free.
def setf_values_(names, values):
        # Unregistered Issue ORTHOGONALISE-TYPING-OF-THE-SEQUENCE-KIND-AND-STRUCTURE
        check_type(names, _tuple)
        pro, val = compile_(values)
        return (pro + [("Assign", [ _compile_name(x, "Store") for x in names ],
                        val)],
                ("Tuple", mapcar(_compile_name, names), ("Load",)))

@defprimitive # Critical-issue-free.
def return_(x):
        with _tail_position():
                pro, val = compile_(x)
                return (pro + [("Return", val)],
                        None)

@defprimitive # Issue-free, per se.
def quote_(x):
        with progv(_COMPILER_QUOTE_ = t):
                return compile_(x)

@defprimitive # imp?
def quaquote_(x):
        with progv(_COMPILER_QUOTE_ = t):
                return compile_(x)

@defprimitive # imp?
def comma_(x):
        with progv():
                return compile_(x)

def _compiler_prepend(pro, tuple):
        return (pro + tuple[0],
                tuple[1])

@defprimitive # Critical-issue-free.
def progn_(*body):
        if not body:
                return ([],
                        compile_((symbol_, "nil")))
        pro, ntotal = [], _len(body)
        with _no_tail_position():
                for spro, val in (compile_(x) for x in body[:-1]):
                        pro.extend(spro)
                        pro.append(("Expr", val))
        with _maybe_tail_position():
                return _compiler_prepend(pro,
                                         compile_(body[-1]))
        ## Not sure the stuff below still makes sense.  Still, am afraid to erase it.
        # lowered_body = mapcan(compile_, body)
        # (( body_bound_vars,  body_free_vars,  body_xtnls),
        #  (thunk_bound_vars, thunk_free_vars, thunk_xtnls)) = mapcar(_ast_bound_free, [lowered_body, thunks])
        # must_thunk = _len(lowered_body) > 1 or thunks
        # scope_mutation = (body_bound_vars or thunk_bound_vars or body_xtnls or thunk_xtnls)
        # if not (must_thunk or scope_mutation):
        #         return lowered_body

@defprimitive
def if_(test, consequent, antecedent = nil):
        with _no_tail_position():
                lo_test = pro_test, val_test = compile_(test)
        lo_cons, lo_ante = mapcar(compile_, [consequent, antecedent])
        ((pro_cons, val_cons),
         (pro_ante, val_ante)) = lo_cons, lo_ante
        cons_expr_p, ante_expr_p = mapcar(_tuple_expression_p, [lo_cons, lo_ante])
        if _all([cons_expr_p, ante_expr_p]):
                return (pro_test,
                        ("IfExp", val_test, val_cons, val_ante))
        else:
                with _gensymnames(x = "IF-BRANCH", n = 2) as name_cons, name_ante:
                        cons_branch = cons_val if cons_expr_p else (symbol_, name_cons)
                        ante_branch = ante_val if ante_expr_p else (symbol_, name_ante)
                        return compile_((flet_, ((_tuple() if cons_expr_p else ((name_cons, _tuple()) + pro_cons + (val_cons,),),) +
                                                 (_tuple() if ante_expr_p else ((name_ante, _tuple()) + pro_ante + (val_cons,),),)),
                                         (if_, test, cons_branch, ante_branch)))
# 1. I'd rather much separate:
#    - named lambda compilation
#        def thunk():
#                def named(<lambda-list>):
#                        <body>
#                return named
#        thunk()
#    - installation of such named lambdas as global function definitions
#        emit a decorator? install_fdefinition
@defprimitive
def def_(name, lambda_list, *body, decorators = []):
        # This is NOT a Lisp form, but rather an acknowledgement of the
        # need to represent a building block from the underlying system.
        "A function definition with python-style lambda list (but homoiconic lisp-style representation)."
        cdef = _compiler_def(name   = name,
                             parent = _compiling_def())
        toplevelp = symbol_value("_COMPILER_TOPLEVEL_P_")
        with progv(_COMPILER_DEF_        = cdef,
                   _COMPILER_TOPLEVEL_P_ = nil):
                check_type(name, (or_, string_, (and_, symbol, (not_, (satisfies_, keywordp)))))
                def try_compile():
                        # Unregistered Issue COMPLIANCE-REAL-DEFAULT-VALUES
                        compiled_lambda_list, (fixed, optional, rest, keys, restkey,
                                               total) = _compile_lispy_lambda_list("DEF %s" % name, lambda_list)
                        with _tail_position():
                                # Unregistered Issue COMPILATION-SHOULD-TRACK-SCOPES
                                pve = body_ret, _ = compile_((return_, (progn_, ) + body))
                        # body_exprp = _tuple_expression_p(preliminary_body_pve) # Why we'd need that, again?
                        # Unregistered Issue CRUDE-SPECIAL-CASE-FOR-BOUND-FREE
                        deco_vals = []
                        for pro_deco, val_deco in (compile_(d) for d in decorators):
                                if pro_deco:
                                        error("in DEF %s: decorators must lower to python expressions.", name)
                                
                        return ([("FunctionDef", string(name), compiled_lambda_list,
                                                 body_ret,
                                                 deco_vals)],
                                compile_((quote_, (symbol_, string(name))))[1])
                ## Xtnls feedback loop stabilisation scheme.
                ##
                ## This looks fairly ridiculous, but that reality for you:
                ##  - it's impossible to know externals before compilation
                ##    - determined by walking the resulting atree
                ##  - you need to know externals before compilation
                ##    - at least one optimisation (LET) depends on this
                ##
                ## Quietly hoped to be the only parameter requiring such beforehand knowledge.
                xtnls_guess, xtnls_actual = None, _set()
                while xtnls_guess != xtnls_actual:
                        cdef.xtnls = xtnls_guess = xtnls_actual
                        result = try_compile()
                        xtnls_actual = _tuple_xtnls(result)
                return result

def _install_macro_definition(fn):
        name = fn.__name__
        # Unregistered Issue COMPLIANCE-GLOBAL-DEFINITIONS-ONLY-IN-CURRENT-PACKAGE
        sym = _intern0(name)
        _setf_symbol_macro_function(sym, fn)
        return sym

@defprimitive
def defmacro(name, lambda_list, *body):
        # 1. Transformation
        #   - compile_((def_, name, lambda_list) + body)
        # 2. Compile-time side-effect (see CLHS)
        #   - obtain the function for compiled code
        #   - _setf_symbol_macro_function()
        return compile_((def_, name, lambda_list) + body,
                        decorators = [(_defmacro,)])

@defprimitive
def let_(bindings, *body):
        # Potential optimisations:
        #  - better tail position detection: non-local-transfer-of-control-free and ending with RETURN.
        #  - even when not in the tail position, but the bound names are not:
        #    - xtnls
        #    - free in some other local expression
        #    - falls out, sort of.. (see below)
        if not (_tuplep(bindings) and
                _every(_of_type((or_, symbol, (tuple_, symbol, t))))):
                error("LET: malformed bindings: %s.", bindings)
        # Unregistered Issue PRIMITIVE-DECLARATIONS
        bindings_thru_defaulting = _tuple(_ensure_cons(b, nil) for b in bindings)
        names, values = _recombine((_list, _list), identity, bindings_thru_defaulting)
        compiled_value_pves = mapcar(compile_, values)
        ## A great optimisation, but mutation can affect:
        ##  - scope of called outside functions
        ##    - cannot optimize if body could jump to local code depending on mutated locals
        ##  - xtnls
        ##    - cannot optimize if bindings contain xtnls of the current DEF_
        ##      - which ones must be, therefore, determined before the DEF_'s body is compiled
        ## Now, all was implemented, except ATREE bound/free/xtnls queries.
        ## But possibly, just possibly, I've missed another requirement, so playing it safe for now.
        ##
        ## This optimisation is, currently, tactically broken, but for another reason: order of evaluation.
        # names = _mapset(ensure_car, bindings)
        # if _tail_position_p() and not ((_mapsetn(_atree_free, body) - _set(names)) or
        #                                 (_compiling_def().xtnls & _set(names))):
        #         with _no_tail_position():
        #                 # Consciously discarding the values returned by (SETF VALUES)
        #                 bind_pro, _ = compile_((setf_values_,
        #                                         [ car(x) for x in bindings_thru_defaulting ],
        #                                         ("tuple",) + _tuple(cdr(x) for x in bindings_thru_defaulting)))
        #         # Unregistered Issue COMPILATION-SHOULD-TRACK-SCOPES
        #         body_pro, body_val = compile_((progn_,) + body)
        #         return (bind_pro + body_pro,
        #                 body_val)
        if every(_tuple_expression_p, compiled_value_pves):
                return compile_(((lambda_, binding_thru_defaulting) + body,))
        else:
                last_non_expr_posn = position_if_not(_tuple_expression_p, compiled_value_pves, from_end = t)
                n_nonexprs = last_non_expr_posn + 1
                temp_names = [ _gensym("LET") for i in _range(_len(bindings)) ]
                # Unregistered Issue PYTHON-CANNOT-CONCATENATE-ITERATORS-FULL-OF-FAIL
                return compile_((progn_,) +
                                _tuple((setq_, n, v) for n, v in _zip(temp_names, values[:n_nonexprs])) +
                                ((let_, _tuple(_zip(names[:n_nonexprs], temp_names)),
                                  ((lambda_, _tuple(_zip(names, temp_names + values[n_nonexprs:]))) + body,)),))

@defprimitive
def unwind_protect_(form, *unwind_body):
        if not unwind_body:
                return compile_(form)
        temp_name = gensym("PROTECTED-FORM-VALUE")
        pro_form, val_form = compile_((setq_, temp_name, form))
        pro_unwind, val_unwind = compile_((progn,) + unwind_body)
        return ([("TryFinally",
                  # It's the SETQ's value we're discarding here, which is known to be safe -- a name reference.
                  pro_form, # Unregistered Issue COMPILER-VALUE-DISCARDABILITY-POLICY
                  # ..in contrast, here, barring analysis, we have no idea about discardability of val_unwind
                  pro_unwind + [("Expr", val_unwind)])],
                (symbol_, temp_name))

@defprimitive # /Seems/ alright.
def funcall_(func, *args):
        # Unregistered Issue IMPROVEMENT-FUNCALL-COULD-VALIDATE-CALLS-OF-KNOWNS
        if stringp(func): # Unregistered Issue ENUMERATE-COMPUTATIONS-RELIANT-ON-STRING-FUNCALL
                          # - quote_ compilation in compile_
                func_pro, func_val = ([],
                                      ("Name", func, ("Load",)))
        else:
                with _no_tail_position():
                        func_pro, func_val = compile_(func)
        with _no_tail_position():
                arg_pves = mapcar(compile_, args)
        if every(_tuple_expression_p, arg_pves):
                return (func_pro,
                        ("Call", func_val, mapcar(second, arg_pves), []))
        else:
                with _gensyms(n = _len(args), x = "FUNCALL-ARG") as temp_names:
                        if _tuple_expression_p((func_pro, func_val)):
                                func_binding, func_exp = (_tuple(),
                                                          func)
                        else:
                                func_name = _gensym("FUNCALL-FUNCNAME")
                                func_binding, func_exp = (((func_name, func),),
                                                          (symbol_, func_name))
                        return compile_((let_, func_binding + _tuple(_zip(temp_names, args)),
                                         (funcall_, func_exp) + _tuple()))

@defprimitive # Critical issue-free.
def flet_(bindings, *body):
        # Unregistered Issue ORTHOGONALISE-TYPING-OF-THE-SEQUENCE-KIND-AND-STRUCTURE
        # Unregistered Issue LAMBDA-LIST-TYPE-NEEDED
        # Unregistered Issue SINGLE-NAMESPACE
        if not _every(_of_type((partuple_, symbol, _tuple))):
                error("FLET: malformed bindings: %s.", bindings)
        return compile_((let_, _tuple((name, (fdefinition_, (def_, _gensym(string(name)), lambda_list) + fbody))
                                      for name, lambda_list, *fbody in bindings)) +
                         body)

@defprimitive # Critical-issue-free.
def lambda_(lambda_list, *body):
        # Unregistered Issue COMPLIANCE-REAL-DEFAULT-VALUES
        # Unregistered Issue COMPILATION-SHOULD-TRACK-SCOPES
        # Unregistered Issue SHOULD-HAVE-A-BETTER-WAY-TO-COMPUTE-EXPRESSIBILITY
        # Unregistered Issue EMPLOY-THUNKING-TO-REMAIN-AN-EXPRESSION
        preliminary_body_pve = compile_((progn_), + body)
        body_exprp = _tuple_expression_p(preliminary_body_pve)
        if body_exprp:
                compiled_arguments, _ = _compile_lispy_lambda_list("LAMBDA", lambda_list, allow_defaults = t)
                return ([],
                        ("Lambda", compiled_arguments, preliminary_body_pve[1]))
        else:
                func_name = _gensymname("LET-BODY-")
                return compile_((flet_, ((func_name, lambda_list) + body,),
                                 (symbol_, func_name)))

@defprimitive # Critical-issue-free, per se, but depends on DEF_.
def labels_(bindings, *body):
        # Unregistered Issue ORTHOGONALISE-TYPING-OF-THE-SEQUENCE-KIND-AND-STRUCTURE
        # Unregistered Issue LAMBDA-LIST-TYPE-NEEDED
        # Unregistered Issue SINGLE-NAMESPACE
        if not _every(_of_type((partuple_, symbol, _tuple))):
                error("LABELS: malformed bindings: %s.", bindings)
        temp_name = _gensym("LABELS")
        return compile_((flet, ((temp_name, _tuple(),
                                 _tuple((def_, name, lambda_list, body)
                                        for name, lambda_list, *body in bindings) +
                                 body)),
                         (funcall_, temp_name)))

## Good news: our LET* will be honest:
# >>> def let0():
# ...         def val0_body1():
# ...                 print("val0")
# ...                 val1()
# ...         def body0():
# ...                 def val1():
# ...                         print("val1")
# ...                 val0_body1()
# ...         body0()
# ...
# >>> let0()
# val0
# Traceback (most recent call last):
#   File "<stdin>", line 1, in <module>
#   File "<stdin>", line 9, in let0
#   File "<stdin>", line 8, in body0
#   File "<stdin>", line 4, in val0_body1
# NameError: global name 'val1' is not defined
@defprimitive
def let__(bindings, *body): # Critical-issue-free.
        if not (_tuplep(bindings) and
                _every(_of_type((or_, symbol, (tuple_, symbol, t))))):
                error("LET*: malformed bindings: %s.", bindings)
        # Unregistered Issue PRIMITIVE-DECLARATIONS
        if not bindings:
                return compile_((progn_,) + body)
        else:
                return compile_((let_, bindings[:1],
                                 (let__, bindings[1:]) + body))

## Honest DEFUN, with real keyword arguments, is out of scope for now.
# @defprimitive
# def defun(name, lambda_list, *body):
#         def compile_lambda_list(x):
#                 return ("arguments",
#                         )
#         return ("FunctionDef", name, compile_lambda_list(lambda_list),
#                 mapcar(compile_, body),
#                 []) # no decorators and no return annotation

# How is it do be determined, that a form must be passed through?
# - directly AST-ifiable (in terms of _astify_constant)
# - atrees
# ..but what about detecting invalid forms?
#
# Also: how do we represent fucking tuples?
# Also: should we track form paths?
def macroexpand_1(form):
        # SYMBOL-MACRO-FUNCTION is what forced us to require the package system.
        return ((form, nil) if not _tuplep(form) else
                _if_let((form and _symbol_macro_function(form[0])[0]),
                        lambda expander:
                                (expander(*form[1:]), t),
                        lambda:
                                (form, nil)))

def macroexpand(form):
        def do_macroexpand(form, expanded):
                expansion, expanded_again = macroexpand_1(form)
                return (do_macroexpand(expansion, t) if expanded_again else
                        (form, expanded))
        return do_macroexpand(form, nil)

# Unregistered Issue DEBUG-SCAFFOLDING
if probe_file("/home/deepfire/.partus-debug-compiler"):
        _debug_compiler()
# Urgent Issue COMPILER-MACRO-SYSTEM
def compile_(form):
        # - tail position tracking
        # - scopes
        # - symbols not terribly clear
        # - proper quote processing
        if _debugging_compiler_p():
                _compiler_track_compiled_form(form)
                _debug_printf(";;; compiling: %s", form)
                _compiler_report_context()
        def lower(x):
                # NOTE: we are going to splice unquoting processing here, as we must be able
                # to work in READ-less environment.
                if _debugging_compiler_p():
                        _debug_printf(";;; lowering: %s", x)
                if _tuplep(x):
                        if not x:
                                return lower((symbol_, "nil"))
                        if _compiling_quote_p():
                                # And so, let the rampant special-casing begin..
                                if x[0] is symbol_:
                                        _len(x) > 2 and error("Invalid SYMBOL pseudo-form: %s.", x)
                                        return _find_primitive(symbol_)[0](x[1])
                                else:
                                        return _find_primitive(funcall_)[0](("tuple",) + x)
                        else:
                                if symbolp(x[0]):
                                        # Urgent Issue COMPILER-MACRO-SYSTEM
                                        compiler, primitivep = _find_primitive(the(symbol, x[0]))
                                        if primitivep:
                                                # Unregistered Issue COMPILE-CANNOT-EVEN-MENTION-KWARGS
                                                return compiler(*x[1:])
                                        form, expanded = macroexpand(x)
                                        if expanded:
                                                return lower(form)
                                        # basic function call
                                        return lower((funcall_,) + form)
                                elif (_tuplep(x[0]) and x[0] and x[0][0] is lambda_):
                                        return lower((funcall_,) + x)
                                elif stringp(x[0]): # basic function call
                                        return lower((funcall_,) + x)
                                else:
                                        error("Invalid form: %s.", princ_to_string(form))
                elif symbolp(x):
                        if _compiling_quote_p():
                                return lower(string(x))
                        else:
                                return lower((symbol_, string(x)))
                else:
                        # NOTE: we don't care about quoting here, as constants are self-evaluating.
                        ast, astifiedp = _try_astify_constant(x) # NOTE: this allows to directly pass through ASTs.
                        if astifiedp:
                                # ..in turn, this requires the atree astifier to directly pass through ASTs,
                                # or, alternatively (and more desirably), we could call _try_atreeify_constant.
                                return ([],
                                        x,
                                        [])
                        else:
                                error("UnASTifiable non-symbol/tuple %s.", princ_to_string(x))
        pve = lower(form)
        if _debugging_compiler_p():
                _debug_printf(";;; compilation atree output for %s:\n;;;\n;;; Prologue\n;;;\n%s\n;;;\n;;; Value\n;;;\n%s",
                              form, *pve)
        return pve

# getsource
#   getsourcelines
#     findsource
#       file = fn.__code__.co_filename
#       sourcefile = getsourcefile = f(fn.__code__.co_filename)
#       file = sourcefile or file
#       module = getmodule()
#       linecache.getlines(file)
#     getblock
#       <boring>
__def_sources__ = collections.OrderedDict()
__def_sources__[""] = "" # placeholder
__def_sources_filename__ = "<lisp>"
def _lisp_add_def(name, source):
        if name in __def_sources__:
                del __def_sources__[name]
        __def_sources__[name] = source
        total = "\n".join(__def_sources__.values())
        linecache.cache[__def_sources_filename__] = _len(total), _int(time.time()), total.split("\n"), __def_sources_filename__

def lisp(body):
        def _intern_astsexp(x):
                return (x.n                                        if _isinstance(x, ast.Num)   else
                        x.s                                        if _isinstance(x, ast.Str)   else
                        _read_symbol(x.id)                         if _isinstance(x, ast.Name)  else
                        _tuple(_intern_astsexp(e) for e in x)      if _isinstance(x, _list)     else
                        _tuple(_intern_astsexp(e) for e in x.elts) if _isinstance(x, ast.Tuple) else
                        _intern_astsexp(x.value)                   if _isinstance(x, ast.Expr)  else
                        error("LISP: don't know how to intern value %s of type %s.", x, type_of(x)))
        args_ast, body_ast = _function_ast(body)
        if _len(body_ast) > 1:
                error("In LISP %s: toplevel definitions are just that: toplevel definitions. "
                      "No more than one toplevel form is allowed per definition.")
        form = _intern_astsexp(body_ast[0])
        __def_allowed_toplevels__ = _set([def_])
        if form[0] not in __def_allowed_toplevels__:
                error("In LISP %s: only toplevels in %s are allowed.",
                      form[0], __def_allowed_toplevels__)
        pve = pro, val = compile_(form)
        # Unregistered Issue SHOULD-REALLY-BE-HONEST-WITH-VALUE-IN-LISP
        stmts = mapcar(_compose(_ast_ensure_stmt, _atree_ast),
                       remove_if(_nonep, _tuplerator(pve)))
        if _debugging_compiler_p():
                import more_ast
                _debug_printf(";;; compilation Python output for %s:\n;;;\n%s\n",
                              form, "".join(more_ast.pp_ast_as_code(x) for x in stmts))
        return _ast_compiled_name(body.__name__,
                                  *stmts,
                                  globals_ = _builtins.globals(),
                                  locals_  = _builtins.locals())

@lisp
def fdefinition(name):
        (def_, fdefinition, (name,),
         (if_, (stringp, name),
          (_global, name),
          (symbol_function, (the, symbol, name))))

@lisp
def stringp(x):
        (def_, stringp, (x,),
         ("isinstance", x, str)) # Unregistered Issue REDUCE-BUILTINS-USAGE

##
## Pretty-printing
##
def print_unreadable_object(object, stream, body, identity = None, type = None):
        write_string("#<", stream)
        if type:
                format(stream, "%s ", type_of(object).__name__)
        body()
        if identity:
                format(stream, " {%x}", _id(object))
        write_string(">", stream)

class readtable(collections.UserDict):
        def __init__(self, case = _keyword("upcase")):
                self.case = the((member_, _keyword("upcase"), _keyword("downcase"), _keyword("preserve"), _keyword("invert")),
                                case)
                self.dict = _dict()

def readtablep(x):     return typep(x, readtable)
def readtable_case(x): return the(readtable, x).case

def copy_readtable(x):
        check_type(x, readtable)
        new = readtable(case = readtable_case(x))
        new.dict = _dict(x.dict)
        return new

__standard_pprint_dispatch__ = _dict() # XXX: this is crap!
__standard_readtable__       = readtable() # XXX: this is crap!

__standard_io_syntax__ = _dict(_PACKAGE_               = find_package("COMMON-LISP-USER"),
                               _PRINT_ARRAY_           = t,
                               _PRINT_BASE_            = 10,
                               _PRINT_CASE_            = _keyword("upcase"),
                               _PRINT_CIRCLE_          = nil,
                               _PRINT_ESCAPE_          = t,
                               _PRINT_GENSYM_          = t,
                               _PRINT_LENGTH_          = nil,
                               _PRINT_LEVEL_           = nil,
                               _PRINT_LINES_           = nil,
                               _PRINT_MISER_WIDTH_     = nil,
                               _PRINT_PPRINT_DISPATCH_ = __standard_pprint_dispatch__,
                               _PRINT_PRETTY_          = t,
                               _PRINT_RADIX_           = nil,
                               _PRINT_READABLY_        = nil,
                               _PRINT_RIGHT_MARGIN_    = nil,
                               _READ_BASE_                 = 10,
                               _READ_DEFAULT_FLOAT_FORMAT_ = "single-float",
                               _READ_EVAL_                 = t,
                               _READ_SUPPRESS_             = nil,
                               _READTABLE_                 = __standard_readtable__)

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

setq("_PRINT_ARRAY_",           __standard_io_syntax__["_PRINT_ARRAY_"])
"""Controls the format in which arrays are printed. If it is false,
the contents of arrays other than strings are never printed. Instead,
arrays are printed in a concise form using #< that gives enough
information for the user to be able to identify the array, but does
not include the entire array contents. If it is true, non-string
arrays are printed using #(...), #*, or #nA syntax."""

setq("_PRINT_BASE_",            __standard_io_syntax__["_PRINT_BASE_"])
"""*PRINT-BASE* and *PRINT-RADIX* control the printing of
rationals. The value of *PRINT-BASE* is called the current output
base.

The value of *PRINT-BASE* is the radix in which the printer will print
rationals. For radices above 10, letters of the alphabet are used to
represent digits above 9."""

setq("_PRINT_CASE_",            __standard_io_syntax__["_PRINT_CASE_"])
"""The value of *PRINT-CASE* controls the case (upper, lower, or
mixed) in which to print any uppercase characters in the names of
symbols when vertical-bar syntax is not used.

*PRINT-CASE* has an effect at all times when the value of
*PRINT-ESCAPE* is false. *PRINT-CASE* also has an effect when the
value of *PRINT-ESCAPE* is true unless inside an escape context
(i.e., unless between vertical-bars or after a slash)."""

setq("_PRINT_CIRCLE_",          __standard_io_syntax__["_PRINT_CIRCLE_"])
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

setq("_PRINT_ESCAPE_",          __standard_io_syntax__["_PRINT_ESCAPE_"])
"""If false, escape characters and package prefixes are not output
when an expression is printed.

If true, an attempt is made to print an expression in such a way that
it can be READ again to produce an equal expression. (This is only a
guideline; not a requirement. See *PRINT-READABLY*.)

For more specific details of how the value of *PRINT-ESCAPE* affects
the printing of certain types, see Section 22.1.3 (Default
Print-Object Methods)."""

setq("_PRINT_GENSYM_",          __standard_io_syntax__["_PRINT_GENSYM_"])
"""Controls whether the prefix ``#:'' is printed before apparently
uninterned symbols. The prefix is printed before such symbols if and
only if the value of *PRINT-GENSYM* is true."""

setq("_PRINT_LENGTH_",          __standard_io_syntax__["_PRINT_LENGTH_"])
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

setq("_PRINT_LEVEL_",           __standard_io_syntax__["_PRINT_LEVEL_"])
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

setq("_PRINT_LINES_",           __standard_io_syntax__["_PRINT_LINES_"])
"""When the value of *PRINT-LINES* is other than NIL, it is a limit on
the number of output lines produced when something is pretty
printed. If an attempt is made to go beyond that many lines, ``..'' is
printed at the end of the last line followed by all of the suffixes
(closing delimiters) that are pending to be printed."""

setq("_PRINT_MISER_WIDTH_",     __standard_io_syntax__["_PRINT_MISER_WIDTH_"])
"""If it is not NIL, the pretty printer switches to a compact style of
output (called miser style) whenever the width available for printing
a substructure is less than or equal to this many ems."""

setq("_PRINT_PPRINT_DISPATCH_", __standard_io_syntax__["_PRINT_PPRINT_DISPATCH_"])
"""The PPRINT dispatch table which currently controls the pretty printer.

Initial value is implementation-dependent, but the initial entries all
use a special class of priorities that have the property that they are
less than every priority that can be specified using
SET-PPRINT-DISPATCH, so that the initial contents of any entry can be
overridden."""

setq("_PRINT_PRETTY_",          __standard_io_syntax__["_PRINT_PRETTY_"])
"""Controls whether the Lisp printer calls the pretty printer.

If it is false, the pretty printer is not used and a minimum of
whitespace[1] is output when printing an expression.

If it is true, the pretty printer is used, and the Lisp printer will
endeavor to insert extra whitespace[1] where appropriate to make
expressions more readable.

*PRINT-PRETTY* has an effect even when the value of *PRINT-ESCAPE* is
false."""

setq("_PRINT_RADIX_",           __standard_io_syntax__["_PRINT_RADIX_"])
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

setq("_PRINT_READABLY_",        __standard_io_syntax__["_PRINT_READABLY_"])
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

setq("_PRINT_RIGHT_MARGIN_",    __standard_io_syntax__["_PRINT_RIGHT_MARGIN_"])
"""If it is non-NIL, it specifies the right margin (as integer number
of ems) to use when the pretty printer is making layout decisions.

If it is NIL, the right margin is taken to be the maximum line length
such that output can be displayed without wraparound or truncation. If
this cannot be determined, an implementation-dependent value is
used."""

setq("_READ_BASE_",                 __standard_io_syntax__["_READ_BASE_"])
"""."""

setq("_READ_DEFAULT_FLOAT_FORMAT_", __standard_io_syntax__["_READ_DEFAULT_FLOAT_FORMAT_"])
"""."""

setq("_READ_EVAL_",                 __standard_io_syntax__["_READ_EVAL_"])
"""."""

setq("_READ_SUPPRESS_",             __standard_io_syntax__["_READ_SUPPRESS_"])
"""."""

setq("_READTABLE_",                 __standard_io_syntax__["_READTABLE_"])
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
        package  = _defaulted_to_var(package,  "_PACKAGE_")
        if not packagep(package):
                _here("------------------------------------------------------------\npackage is a %s: %s" % (type_of(package), package,))
        readably = _defaulted_to_var(readably, "_PRINT_READABLY_")
        escape   = _defaulted_to_var(escape,   "_PRINT_ESCAPE_") if not readably else t
        case     = _defaulted_to_var(case,     "_PRINT_CASE_")   if not readably else t
        gensym   = _defaulted_to_var(gensym,   "_PRINT_GENSYM_") if not readably else t
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
        readably = _defaulted_to_var(readably, "_PRINT_READABLY_")
        escape   = _defaulted_to_var(escape,   "_PRINT_ESCAPE_") if not readably else t
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
                        lambda: format(s, "%d elements", _len(x)),
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
        array           = _defaulted_to_var(array,           "_PRINT_ARRAY_")
        base            = _defaulted_to_var(base,            "_PRINT_BASE_")
        case            = _defaulted_to_var(case,            "_PRINT_CASE_")
        circle          = _defaulted_to_var(circle,          "_PRINT_CIRCLE_")
        escape          = _defaulted_to_var(escape,          "_PRINT_ESCAPE_")
        gensym          = _defaulted_to_var(gensym,          "_PRINT_GENSYM_")
        length          = _defaulted_to_var(length,          "_PRINT_LENGTH_")
        level           = _defaulted_to_var(level,           "_PRINT_LEVEL_")
        lines           = _defaulted_to_var(lines,           "_PRINT_LINES_")
        miser_width     = _defaulted_to_var(miser_width,     "_PRINT_MISER_WIDTH_")
        pprint_dispatch = _defaulted_to_var(pprint_dispatch, "_PRINT_PPRINT_DISPATCH_")
        pretty          = _defaulted_to_var(pretty,          "_PRINT_PRETTY_")
        radix           = _defaulted_to_var(radix,           "_PRINT_RADIX_")
        readably        = _defaulted_to_var(readably,        "_PRINT_READABLY_")
        right_margin    = _defaulted_to_var(right_margin,    "_PRINT_RIGHT_MARGIN_")
        # assert(True
        #        and array is t
        #        and base is 10
        #        # case is _keyword("upcase")
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
                        if _listp(object) or _tuplep(object):
                                string += "("
                                max = _len(object)
                                if max:
                                        for i in _range(0, max):
                                                string += do_write_to_string(object[i])
                                                if i != (max - 1):
                                                        string += " "
                                string += ")"
                        elif symbolp(object):
                                # Honors *PACKAGE*, *PRINT-CASE*, *PRINT-ESCAPE*, *PRINT-GENSYM*, *PRINT-READABLY*.
                                # XXX: in particular, *PRINT-ESCAPE* is honored only partially.
                                string += _print_symbol(object)
                        elif integerp(object) or floatp(object):
                                string += _str(object)
                        elif object is False or object is None or object is True:
                                string += obj2lisp_xform[object]
                        elif _type(object).__name__ == "builtin_function_or_method":
                                string += "\"#<BUILTIN-FUNCTION-OR-METHOD %s 0x%x>\"" % (object.__name__, _id(object))
                        elif stringp(object):
                                # Honors *PRINT-ESCAPE* and *PRINT-READABLY*.
                                string += _print_string(object)
                        elif hash_table_p(object) or _setp(object):
                                # Honors *PRINT-ESCAPE* and *PRINT-READABLY*.
                                string += _print_unreadable_compound(object)
                        elif functionp(object):
                                string += _print_function(object)
                        elif (not escape) and typep(object, (or_, restart, condition)):
                                string += _str(object)
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

##
## Reader
##
setq("_READ_CASE_", _keyword("upcase"))

def parse_integer(xs, junk_allowed = nil, radix = 10):
        l = _len(xs)
        def hexcharp(x): return x.isdigit() or x in ["a", "b", "c", "d", "e", "f"]
        (test, xform) = ((_str.isdigit, identity)      if radix == 10 else
                         (hexcharp,    _builtins.float.fromhex) if radix == 16 else
                         _not_implemented("PARSE-INTEGER only implemented for radices 10 and 16."))
        for end in _range(0, l):
                if not test(xs[end]):
                        if junk_allowed:
                                end -= 1
                                break
                        else:
                                error("Junk in string \"%s\".", xs)
        return _int(xform(xs[:(end + 1)]))

end_of_file = EOFError

@block
def read_from_string(string, eof_error_p = True, eof_value = nil,
                     start = 0, end = None, preserve_whitespace = None):
        "Does not conform."
        # _here("from \"%s\"" % string)
        pos, end = start, (end or _len(string))
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
                while string[pos] in _frozenset([" ", "\t", "\n"]):
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
                                if not _listp(obj) and obj is _find_symbol0("."):
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
                        ret = _int(token)
                elif _without_condition_system(lambda: re.match("^[0-9]+\\.[0-9]+$", token),
                                               reason = "re.match"):
                        ret = _builtins.float(token)
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
                        if char in _set([" ", "\t", "\n", "(", ")", "\"", "'"]):
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
        stream = _defaulted_to_var(stream, "_STANDARD_INPUT_")
        return handler_case(lambda: stream.readline(),
                            (error_,
                             lambda c: error(end_of_file, "end of file on %s" % (stream,))))

def read_char(stream = None, eof_error_p = True, eof_value = nil, recursivep = nil):
        stream = _defaulted_to_var(stream, "_STANDARD_INPUT_")
        ret = the(stream_, stream).read(1)
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
        criterion = (lambda _: t                if peek_type is nil                                     else
                     lambda c: c not in " \t\n" if peek_type is t                                       else
                     lambda c: c == peek_type   if stringp(peek_type) and _len(peek_type) == 1 else
                     error("Invalid peek-type: '%s'.", peek_type))
        stream = _defaulted(input_stream, symbol_value("_STANDARD_INPUT_"))
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
                        if c not in _frozenset([" ", "\t", "\n"]):
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
                                if not _listp(obj) and obj is _find_symbol0("."):
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
                                ret += (char2 if char2 in _set(["\"", "\\"]) else
                                        error("READ-FROM-STRING: unrecognized escape character \"%s\".", char2))
                        else:
                                ret += char
                # _here("< %s" % (ret,))
                return ret
        def read_number_or_symbol():
                token = read_token()
                if _without_condition_system(lambda: re.match("^[0-9]+$", token),
                                             reason = "re.match"):
                        ret = _int(token)
                elif _without_condition_system(lambda: re.match("^[0-9]+\\.[0-9]+$", token),
                                               reason = "re.match"):
                        ret = _builtins.float(token)
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
                        if char in _set([nil, " ", "\t", "\n", "(", ")", "\"", "'"]):
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
        return _int(os.path.getmtime(pathspec))

##
## Streams
##
def open_stream_p(x):
        return not the(stream_, x).closed

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

setq("_STANDARD_INPUT_",  sys.stdin)
setq("_STANDARD_OUTPUT_", sys.stdout)
setq("_ERROR_OUTPUT_",    sys.stderr)
setq("_DEBUG_IO_",        make_two_way_stream(symbol_value("_STANDARD_INPUT_"), symbol_value("_STANDARD_OUTPUT_")))
setq("_QUERY_IO_",        make_two_way_stream(symbol_value("_STANDARD_INPUT_"), symbol_value("_STANDARD_OUTPUT_")))

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
                symbol_value("_STANDARD_OUTPUT_") if x is t else
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
        if  streamp(destination) or _listp(destination) or destination is t:
                # XXX: python strings are immutable, so lists will serve as adjustable arrays..
                # Issue ADJUSTABLE-CHARACTER-VECTORS-NOT-IMPLEMENTED
                write_string(string, destination)
                return nil
        else:
                return string

def write_line(string, stream = t):
        return write_string(string + "\n", stream)

def finish_output(stream = t):
        check_type(stream, (or_, stream_, (member_, t, nil)))
        (stream is not nil) and _coerce_to_stream(stream).flush()

def force_output(*args, **keys):
        finish_output(*args, **keys)

##
## Pythonese execution tracing: for HANDLER-BIND.
##
__tracer_hooks__   = _dict() # allowed keys: "call", "line", "return", "exception", "c_call", "c_return", "c_exception"
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

###
### Condition system -related thaw
###
def error(datum, *args, **keys):
        "With all said and done, this ought to jump right into __CL_CONDITION_HANDLER__."
        raise make_condition(datum, *args, **keys)
###
##
#

setq("_PRESIGNAL_HOOK_", nil)
setq("_PREHANDLER_HOOK_", nil)
setq("_DEBUGGER_HOOK_",  nil)

def _report_handling_handover(cond, frame, hook):
        format(sys.stderr, "Handing over handling of %s to frame %s\n",
               prin1_to_string(cond), _pp_chain_of_frame(frame, callers = 25))

def signal(cond):
        for cluster in reversed(env.__handler_clusters__):
                for type, handler in cluster:
                        if not stringp(type):
                                if typep(cond, type):
                                        hook = symbol_value("_PREHANDLER_HOOK_")
                                        if hook:
                                                frame = assoc("__frame__", cluster)
                                                assert(frame)
                                                hook(cond, frame, hook)
                                        handler(cond)
        return nil

def warn(datum, *args, **keys):
        cond = make_condition(datum, *args, default_type = simple_warning, **keys)
        signal(cond)
        format(symbol_value("_ERROR_OUTPUT_"), "%s", cond)
        return nil

def invoke_debugger(cond):
        "XXX: non-compliant: doesn't actually invoke the debugger."
        debugger_hook = symbol_value("_DEBUGGER_HOOK_")
        if debugger_hook:
                with env.let(_debugger_hook_ = nil):
                        debugger_hook(cond, debugger_hook)
        error(BaseError, "INVOKE-DEBUGGER fell through.")

__main_thread__ = threading.current_thread()
def _report_condition(cond, stream = None, backtrace = None):
        stream = _defaulted_to_var(stream, "_DEBUG_IO_")
        format(stream, "%sondition of type %s: %s\n",
               (("In thread \"%s\": c" % threading.current_thread().name)
                if threading.current_thread() is not __main_thread__ else
                "C"),
               _type(cond), cond)
        if backtrace:
                _backtrace(-1, stream)
        return t

def _maybe_reporting_conditions_on_hook(p, hook, body, backtrace = None):
        if p:
                old_hook_value = symbol_value(hook)
                def wrapped_hook(cond, hook_value):
                        "Let's honor the old hook."
                        _report_condition(cond,
                                          stream = symbol_value("_DEBUG_IO_"),
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
                                _builtins.print(x, end='')
                                if i and not (i + 1)%8:
                                        _builtins.print(" ", end='')
                                if i and not (i + 1)%32:
                                        _builtins.print("")
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

                _builtins.print("o: %s, id: {%x}" % (o, _id(o)))
                _builtins.print(dump(o))
                for slot, _ in _type(o)._fields_:
                        val = _getattr(o, slot)
                        _builtins.print(("%25s: " + ("%x" if integerp(val) else "%s")) % (slot, val))
        _without_condition_system(body,
                                  reason = "_dump_thread_state")

__not_even_conditions__ = _frozenset([SystemExit, __catcher_throw__])
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
                                      callers = 45, frame = symbol_value("_STACK_TOP_HINT_"))
                        with env.let(_traceback_ = traceback,
                                     _signalling_frame_ = frame): # These bindings are the deviation from the CL standard.
                                presignal_hook = symbol_value("_PRESIGNAL_HOOK_")
                                if presignal_hook:
                                        with env.let(_PRESIGNAL_HOOK_ = nil):
                                                presignal_hook(cond, presignal_hook)
                                signal(cond)
                                debugger_hook = symbol_value("_DEBUGGER_HOOK_")
                                if debugger_hook:
                                        with env.let(_DEBUGGER_HOOK_ = nil):
                                                debugger_hook(cond, debugger_hook)
                        return cond
                else:
                        return raw_cond
        with progv(_STACK_TOP_HINT_ = _caller_frame(caller_relative = 1)):
                cond = sys.call_tracing(continuation, _tuple())
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
                        if not (typep(type, _type) and subtypep(type, condition)):
                                error(simple_type_error, "While establishing handler: '%s' does not designate a known condition type.", type)
                with env.let(__handler_clusters__ = env.__handler_clusters__ +
                             [handlers + (("__frame__", _caller_frame()),)]):
                        return no_error(fn())
        else:
                # old world case..
                # format(t, "crap FAIL: pep %s, exhook is cch: %s",
                #        _pytracer_enabled_p(), __tracer_hooks__.get("exception") is __cl_condition_handler__)
                if _len(handlers) > 1:
                        error("HANDLER-BIND: was asked to establish %d handlers, but cannot establish more than one in 'dumb' mode.",
                              _len(handlers))
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
#                      _dict(interactive_function = lambda: compute_invoke_restart_interactively_args(),
#                            report_function      = lambda stream: print_restart_summary(stream),
#                            test_function        = lambda cond: visible_p(cond))))
setq("__restart_clusters__", [])

def _restartp(x):
        return typep(x, restart)

def restart_name(x):
        return x.name

def _specs_restarts_args(restart_specs):
        # format (t, "_s_r: %s", restart_specs)
        restarts_args = _dict()
        for name, spec in restart_specs.items():
                function, options = ((spec[0], spec[1]) if _tuplep(spec) else
                                     (spec, _dict()))
                restarts_args[name.upper()] = _updated_dict(options, _dict(function = function)) # XXX: name mangling!
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

__valid_restart_options__ = _frozenset(["interactive", "report", "test", "function"])
def _restart_case(body, **restarts_args):
        def validate_restart_options(options):
                unknown = _set(options.keys()) - __valid_restart_options__
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
                                                         _dict(name                 = restart_name,
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
                                             _dict(report = lambda stream: format(stream, "%s", description))) })

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
        restarts = _list()
        for cluster in _reversed(env.__restart_clusters__):
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
        for attr, val in (x.__dict__ if _hasattr(x, "__dict__") else
                          { k: _getattr(x, k) for k in dir(x)}).items():
                if show_hidden or "__" not in attr:
                        write_line("%25s: %s" % (attr, ignore_errors(lambda: _str(val))), stream)

##
## Modules
##
setq("_MODULE_PROVIDER_FUNCTIONS_", [])

def _module_filename(module):
        return "%s/%s.py" % (env.partus_path, _coerce_to_symbol_name(module))

def load(pathspec, verbose = None, print = None,
         if_does_not_exist = t,
         external_format = "default"):
        "XXX: not in compliance"
        verbose = _defaulted_to_var(verbose, "_LOAD_VERBOSE_")
        print   = _defaulted_to_var(verbose, "_LOAD_PRINT_")
        filename = pathspec
        _builtins.exec(_builtins.compile(_file_as_string(filename), filename, "exec"))
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
        return _int(time.time())

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

__eval_source_cache__ = _dict() # :: code_object -> string

def _evaluated_code_source(co):
        return gethash(co, __eval_source_cache__)

def _coerce_to_expr(x):
        return (x.value if typep(x, ast.Expr) else
                x)

def _eval_python(expr_or_stmt):
        "In AST form, naturally."
        package = symbol_value("_PACKAGE_")
        exprp = typep(the(ast.AST, expr_or_stmt), (or_, ast.expr, ast.Expr))
        call = ast.fix_missing_locations(_ast_module(
                        [_ast_import_from("cl", ["__evset__", "_read_symbol"]),
                         _ast_Expr(_ast_funcall(_ast_name("__evset__"), [_coerce_to_expr(expr_or_stmt)]))]
                        if exprp else
                        [expr_or_stmt]))
        code = handler_case(lambda: _builtins.compile(call, "", "exec"),
                            (error_,
                             lambda cond:
                                     error("EVAL: error while trying to compile <%s>: %s",
                                           more_ast.pp_ast_as_code(expr_or_stmt), cond)))
        if boundp("_source_for_eval_"):
                __eval_source_cache__[code] = symbol_value("_SOURCE_FOR_EVAL_")
        # write_line(">>> EVAL: %s" % (more_ast.pp_ast_as_code(expr),))
        _builtins.exec(code, _find_module(_lisp_symbol_name_python_name(package_name(package))).__dict__)
        values = (__evget__() if exprp else
                  _tuple())
        return values if _tuplep(values) else (values,)

def _callify(form, package = None, quoted = False):
        package = _defaulted_to_var(package, "_PACKAGE_")
        def callify_call(sym, args):
                func = function(the(symbol, sym))
                paramspec = inspect.getfullargspec(func)
                nfix = _argspec_nfixargs(paramspec)
                _here("func: %s -> %s, paramspec: %s", sym, func, paramspec)
                _here("nfix: %s", nfix)
                _here("args: %s", args)
                _here("nkeys: %s", _len(args) - nfix)
                if oddp(_len(args) - nfix):
                        error("odd number of &KEY arguments")
                allow_other_keys = paramspec.varkw is not None
                fixnames, keynames = (paramspec.args[0:nfix],
                                      _set(paramspec.args[nfix:] + paramspec.kwonlyargs))
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
                False   : _ast_name("False"),
                None    : _ast_name("None"),
                True    : _ast_name("True"),
                string_ : _ast_string,
                integer : _ast_num,
                }
        if _listp(form):
                if quoted or (form[0] is _find_symbol0("QUOTE")):
                        return (_ast_list(mapcar(lambda x: _callify(x, package, True), form[1]))
                                if _listp(form[1]) else
                                _callify(form[1], package, True))
                else:
                        return callify_call(form[0], form[1:])
        elif symbolp(form):
                return (_ast_funcall("_read_symbol", [_ast_string(form.name),
                                                      _ast_string(form.package.name)])
                        if quoted or keywordp(form) else
                        _lisp_symbol_ast(form, package))
        elif constantp(form):
                return obj2ast_xform[_type(form)](form)
        elif form in obj2ast_xform:
                return obj2ast_xform[form]
        else:
                error("Unable to convert form %s", form)

def eval_(form):
        package = symbol_value("_PACKAGE_")
        return _eval_python(_callify(form, package))

def _valid_declaration_p(x):
        return nil

# Unregistered Issue C-J-COULD-BE-EXTENDED-TO-FOLLOW-M-J-WITHIN-COMMENTS
##
## An attempt at CLOS imitation
##
def class_of(x):
        return _getattr(x, "__class__")

class standard_object():
        def __init__(self, **initargs):
                super().__init__(**initargs)
                initialize_instance(self, **initargs)

def make_instance(class_, **keys):
        "XXX: compliance?"
        return class_(**keys)

def slot_boundp(object, slot):            return _hasattr(object, slot)
def slot_makunbound(object, slot):        del object.__dir__[slot]
def slot_value(object, slot):             return _getattr(object, slot)
def setf_slot_value(object, slot, value): return _setattr(object, slot, value)

def initialize_instance(instance, **initargs):
        """Called by MAKE-INSTANCE to initialize a newly created INSTANCE. The
generic function is called with the new INSTANCE and the defaulted
initialization argument list.

The system-supplied primary method on INITIALIZE-INSTANCE initializes
the slots of the instance with values according to the INITARGS and
the :INITFORM forms of the slots. It does this by calling the generic
function SHARED-INITIALIZE with the following arguments: the instance,
T (this indicates that all slots for which no initialization arguments
are provided should be initialized according to their :INITFORM
forms), and the INITARGS.

Programmers can define methods for INITIALIZE-INSTANCE to specify
actions to be taken when an instance is initialized. If only after
methods are defined, they will be run after the system-supplied
primary method for initialization and therefore will not interfere
with the default behavior of INITIALIZE-INSTANCE."""
        # Unregistered Issue COMPLIANCE-SPEC-UNCLEAR-ON-NEED-FOR-INITARG-VALIDITY-CHECK
        shared_initialize(instance, t, **initargs)
        return instance

def reinitialize_instance(instance, **initargs):
        """The generic function REINITIALIZE-INSTANCE can be used to
change the values of local slots of an INSTANCE according to
INITARGS. This generic function can be called by users.

The system-supplied primary method for REINITIALIZE-INSTANCE checks
the validity of INITARGS and signals an error if an initarg is
supplied that is not declared as valid. The method then calls the
generic function SHARED-INITIALIZE with the following arguments: the
INSTANCE, NIL (which means no slots should be initialized according to
their initforms), and the INITARGS it received."""
        _not_implemented("check of validity of INITARGS")
        shared_initialize(instance, nil, **initargs)
        return instance

def shared_initialize(instance, slot_names, **initargs):
        """shared-initialize instance slot-names &rest initargs &key &allow-other-keys => instance

Method Signatures:

shared-initialize (instance standard-object) slot-names &rest initargs

Arguments and Values:

instance---an object.

slot-names---a list or t.

initargs---a list of keyword/value pairs (of initialization argument names and values).

Description:

The generic function SHARED-INITIALIZE is used to fill the slots of an
instance using INITARGS and :INITFORM forms. It is called when an
instance is created, when an instance is re-initialized, when an
instance is updated to conform to a redefined class, and when an
instance is updated to conform to a different class. The generic
function SHARED-INITIALIZE is called by the system-supplied primary
method for INITIALIZE-INSTANCE, REINITIALIZE-INSTANCE,
UPDATE-INSTANCE-FOR-REDEFINED-CLASS, and
UPDATE-INSTANCE-FOR-DIFFERENT-CLASS.

The generic function SHARED-INITIALIZE takes the following arguments:
the INSTANCE to be initialized, a specification of a set of SLOT-NAMES
accessible in that INSTANCE, and any number of INITARGS. The arguments
after the first two must form an initialization argument list. The
system-supplied primary method on SHARED-INITIALIZE initializes the
slots with values according to the INITARGS and supplied :INITFORM
forms. SLOT-NAMES indicates which slots should be initialized
according to their :INITFORM forms if no initargs are provided for
those slots.

The system-supplied primary method behaves as follows, regardless of
whether the slots are local or shared:

    If an initarg in the initialization argument list specifies a
    value for that slot, that value is stored into the slot, even if a
    value has already been stored in the slot before the method is
    run.

    Any slots indicated by SLOT-NAMES that are still unbound at this
    point are initialized according to their :INITFORM forms. For any
    such slot that has an :INITFORM form, that form is evaluated in
    the lexical environment of its defining DEFCLASS form and the
    result is stored into the slot. For example, if a before method
    stores a value in the slot, the :INITFORM form will not be used to
    supply a value for the slot.

    The rules mentioned in Section 7.1.4 (Rules for Initialization
    Arguments) are obeyed.

The SLOTS-NAMES argument specifies the slots that are to be
initialized according to their :INITFORM forms if no initialization
arguments apply. It can be a list of slot names, which specifies the
set of those slot names; or it can be the symbol T, which specifies
the set of all of the slots.

7.1.4 Rules for Initialization Arguments

The :INITARG slot option may be specified more than once for a given
slot.

The following rules specify when initialization arguments may be
multiply defined:

* A given initialization argument can be used to initialize more than
  one slot if the same initialization argument name appears in more
  than one :INITARG slot option.

* A given initialization argument name can appear in the lambda list
  of more than one initialization method.

* A given initialization argument name can appear both in an :INITARG
  slot option and in the lambda list of an initialization method.

If two or more initialization arguments that initialize the same slot
are given in the arguments to MAKE-INSTANCE, the leftmost of these
initialization arguments in the initialization argument list supplies
the value, even if the initialization arguments have different names.

If two or more different initialization arguments that initialize the
same slot have default values and none is given explicitly in the
arguments to MAKE-INSTANCE, the initialization argument that appears
in a :DEFAULT-INITARGS class option in the most specific of the
classes supplies the value. If a single :DEFAULT-INITARGS class option
specifies two or more initialization arguments that initialize the
same slot and none is given explicitly in the arguments to
MAKE-INSTANCE, the leftmost in the :DEFAULT-INITARGS class option
supplies the value, and the values of the remaining default value
forms are ignored.

Initialization arguments given explicitly in the arguments to
MAKE-INSTANCE appear to the left of defaulted initialization
arguments. Suppose that the classes C1 and C2 supply the values of
defaulted initialization arguments for different slots, and suppose
that C1 is more specific than C2; then the defaulted initialization
argument whose value is supplied by C1 is to the left of the defaulted
initialization argument whose value is supplied by C2 in the defaulted
initialization argument list. If a single :DEFAULT-INITARGS class
option supplies the values of initialization arguments for two
different slots, the initialization argument whose value is specified
farther to the left in the :DEFAULT-INITARGS class option appears
farther to the left in the defaulted initialization argument list.

If a slot has both an :INITFORM form and an :INITARG slot option, and
the initialization argument is defaulted using :DEFAULT-INITARGS or is
supplied to MAKE-INSTANCE, the captured :INITFORM form is neither used
nor evaluated."""
        # Unregistered Issue COMPLIANCE-INITFORM-MECHANISM-NOT-AVAILABLE
        # Unregistered Issue COMPLIANCE-MULTIPLY-DEFINED-INITARGS-NOT-SUPPORTED
        # Unregistered Issue COMPLIANCE-INDIRECT-SLOT-INITARG-RELATIONSHIPS-NOT-SUPPORTED
        # Unregistered Issue COMPLIANCE-DEFAULT-INITARGS-NOT-SUPPORTED
        instance.__dict__.update(initargs)
        return instance

class method(standard_object):
        "All methods are of this type."

class funcallable_standard_class(standard_object):
        "All funcallable instances are of this type."
        def __call__(self, *args, **keys):
                return self.function(*args, **keys)

class generic_function(funcallable_standard_class):
        "All generic functions are of this type."
        def __init__(self, **initargs): # Simulate a :BEFORE method.
                self.__dependents__ = _set()
                super().__init__(**initargs)

# Dependent Maintenance Protocol
#
# It is convenient for portable metaobjects to be able to memoize
# information about other metaobjects, portable or otherwise. Because
# class and generic function metaobjects can be reinitialized, and
# generic function metaobjects can be modified by adding and removing
# methods, a means must be provided to update this memoized
# information.
#
# The dependent maintenance protocol supports this by providing a way
# to register an object which should be notified whenever a class or
# generic function is modified. An object which has been registered
# this way is called a dependent of the class or generic function
# metaobject. The dependents of class and generic function metaobjects
# are maintained with ADD-DEPENDENT and REMOVE-DEPENDENT. The
# dependents of a class or generic function metaobject can be accessed
# with MAP-DEPENDENTS. Dependents are notified about a modification by
# calling UPDATE-DEPENDENT. (See the specification of UPDATE-DEPENDENT
# for detailed description of the circumstances under which it is
# called.)
#
# To prevent conflicts between two portable programs, or between
# portable programs and the implementation, portable code must not
# register metaobjects themselves as dependents. Instead, portable
# programs which need to record a metaobject as a dependent, should
# encapsulate that metaobject in some other kind of object, and record
# that object as the dependent. The results are undefined if this
# restriction is violated.
#
# This example shows a general facility for encapsulating metaobjects
# before recording them as dependents. The facility defines a basic
# kind of encapsulating object: an UPDATER. Specializations of the
# basic class can be defined with appropriate special updating
# behavior. In this way, information about the updating required is
# associated with each updater rather than with the metaobject being
# updated.
#
# Updaters are used to encapsulate any metaobject which requires
# updating when a given class or generic function is modified. The
# function RECORD-UPDATER is called to both create an UPDATER and add
# it to the dependents of the class or generic function. Methods on
# the generic function UPDATE-DEPENDENT, specialized to the specific
# class of UPDATER do the appropriate update work.
#
# (defclass updater ()
#      ((dependent :initarg :dependent :reader dependent)))
#
# (defun record-updater (class dependee dependent &rest initargs)
#   (let ((updater (apply #'make-instance class :dependent dependent initargs)))
#     (add-dependent dependee updater)
#     updater))
#
# A FLUSH-CACHE-UPDATER simply flushes the cache of the dependent when
# it is updated.
#
# (defclass flush-cache-updater (updater) ())
#
# (defmethod update-dependent (dependee (updater flush-cache-updater) &rest args)
#   (declare (ignore args))
#   (flush-cache (dependent updater)))

def add_dependent(metaobject, dependent):
        """add-dependent metaobject dependent

Arguments:

The METAOBJECT argument is a class or generic function metaobject.

The DEPENDENT argument is an object.

Values:

The value returned by this generic function is unspecified.

Purpose:

This generic function adds DEPENDENT to the dependents of
METAOBJECT. If DEPENDENT is already in the set of dependents it is not
added again (no error is signaled).

The generic function MAP-DEPENDENTS can be called to access the set of
dependents of a class or generic function. The generic function
REMOVE-DEPENDENT can be called to remove an object from the set of
dependents of a class or generic function. The effect of calling
ADD-DEPENDENT or REMOVE-DEPENDENT while a call to MAP-DEPENDENTS on
the same class or generic function is in progress is unspecified.

The situations in which ADD-DEPENDENT is called are not specified."""
        metaobject.__dependents__.add(dependent)

def remove_dependent(metaobject, dependent):
        """remove-dependent metaobject dependent

Arguments:

The METAOBJECT argument is a class or generic function metaobject.

The DEPENDENT argument is an object.

Values:

The value returned by this generic function is unspecified.

Purpose:

This generic function removes DEPENDENT from the dependents of
METAOBJECT. If DEPENDENT is not one of the dependents of metaobject,
no error is signaled.

The generic function MAP-DEPENDENTS can be called to access the set of
dependents of a class or generic function. The generic function
ADD-DEPENDENT can be called to add an object from the set of
dependents of a class or generic function. The effect of calling
ADD-DEPENDENT or REMOVE-DEPENDENT while a call to MAP-DEPENDENTS on
the same class or generic function is in progress is unspecified.

The situations in which REMOVE-DEPENDENT is called are not specified."""
        if dependent in metaobject.__dependents__:
                metaobject.__dependents__.remove(dependent)

def map_dependents(metaobject, function):
        """map-dependents metaobject function

Arguments:

The METAOBJECT argument is a class or generic function metaobject.

The FUNCTION argument is a function which accepts one argument.

Values:

The value returned is unspecified.

Purpose:

This generic function applies FUNCTION to each of the dependents of
METAOBJECT. The order in which the dependents are processed is not
specified, but function is applied to each dependent once and only
once. If, during the mapping, ADD-DEPENDENT or REMOVE-DEPENDENT is
called to alter the dependents of METAOBJECT, it is not specified
whether the newly added or removed dependent will have function
applied to it."""
        mapc(function, the(metaobject).__dependents__)

def update_dependent(metaobject, dependent, **initargs):
        """update-dependent metaobject dependent &rest initargs

Arguments:

The METAOBJECT argument is a class or generic function metaobject. It
is the metaobject being reinitialized or otherwise modified.

The DEPENDENT argument is an object. It is the dependent being
updated.

The INITARGS argument is a list of the initialization arguments for
the metaobject redefinition.

Values:

The value returned by UPDATE-DEPENDENT is unspecified.

Purpose:

This generic function is called to update a dependent of METAOBJECT.

When a class or a generic function is reinitialized each of its
dependents is updated. The INITARGS argument to UPDATE-DEPENDENT is
the set of initialization arguments received by REINITIALIZE-INSTANCE.

When a method is added to a generic function, each of the generic
function's dependents is updated. The INITARGS argument is a list of
two elements: the symbol ADD-METHOD, and the method that was added.

When a method is removed from a generic function, each of the generic
function's dependents is updated. The INITARGS argument is a list of
two elements: the symbol REMOVE-METHOD, and the method that was
removed.

In each case, MAP-DEPENDENTS is used to call UPDATE-DEPENDENT on each
of the dependents. So, for example, the update of a generic function's
dependents when a method is added could be performed by the following
code:

  (map-dependents generic-function
                  #'(lambda (dep)
                      (update-dependent generic-function
                                        dep
                                        'add-method
                                        new-method)))
"""
        # Unregistered Issue COMPLIANCE-UPDATE-DEPENDENT-DOES-NOT-REALLY-DO-ANYTHING
        pass

class method_combination():
        "All method combinations are of this type."

class standard_method(method):
        def __init__(self, **initargs):
                super().__init__(**initargs)
                _standard_method_shared_initialize(method, **initargs)
        def __call__(self, gfun_args, next_methods):
                return self.function(gfun_args, next_methods)

class standard_generic_function(generic_function):
        def __init__(self, **initargs):
                super().__init__(**initargs)
                _standard_generic_function_shared_initialize(self, **initargs)
        # def __call__ ..is installed during EMF computation, with the proper arglist.

def _update_generic_function_and_dependents(generic_function, **initargs):
        set_funcallable_instance_function(generic_function,
                                          compute_discriminating_function(generic_function))
        map_dependents(generic_function,
                       lambda dep: update_dependent(generic_function, dep, **initargs))

def _standard_generic_function_shared_initialize(generic_function,
                                                 argument_precedence_order = None,
                                                 declarations = None,
                                                 documentation = None,
                                                 lambda_list = None,
                                                 method_combination = None,
                                                 method_class = None,
                                                 name = None,
                                                 # extensions
                                                 filename = None,
                                                 lineno = None):
        """Initialization of Generic Function Metaobjects

A generic function metaobject can be created by calling
MAKE-INSTANCE. The initialization arguments establish the definition
of the generic function. A generic function metaobject can be
redefined by calling REINITIALIZE-INSTANCE. Some classes of generic
function metaobject do not support redefinition; in these cases,
REINITIALIZE-INSTANCE signals an error.

Initialization of a generic function metaobject must be done by
calling MAKE-INSTANCE and allowing it to call
INITIALIZE-INSTANCE. Reinitialization of a GENERIC-FUNCTION metaobject
must be done by calling REINITIALIZE-INSTANCE. Portable programs must
not call INITIALIZE-INSTANCE directly to initialize a generic function
metaobject. Portable programs must not call SHARED-INITIALIZE directly
to initialize or reinitialize a generic function metaobject. Portable
programs must not call CHANGE-CLASS to change the class of any generic
function metaobject or to turn a non-generic-function object into a
generic function metaobject.

Since metaobject classes may not be redefined, no behavior is
specified for the result of calls to
UPDATE-INSTANCE-FOR-REDEFINED-CLASS on generic function
metaobjects. Since the class of a generic function metaobject may not
be changed, no behavior is specified for the results of calls to
UPDATE-INSTANCE-FOR-DIFFERENT-CLASS on generic function metaobjects.

During initialization or reinitialization, each initialization
argument is checked for errors and then associated with the generic
function metaobject. The value can then be accessed by calling the
appropriate accessor as shown in Table 3.

This section begins with a description of the error checking and
processing of each initialization argument. This is followed by a
table showing the generic functions that can be used to access the
stored initialization arguments. The section ends with a set of
restrictions on portable methods affecting generic function metaobject
initialization and reinitialization.

In these descriptions, the phrase ``this argument defaults to value''
means that when that initialization argument is not supplied,
initialization or reinitialization is performed as if value had been
supplied. For some initialization arguments this could be done by the
use of default initialization arguments, but whether it is done this
way is not specified. Implementations are free to define default
initialization arguments for specified generic function metaobject
classes. Portable programs are free to define default initialization
arguments for portable subclasses of the class GENERIC-FUNCTION.

Unless there is a specific note to the contrary, then during
reinitialization, if an initialization argument is not supplied, the
previously stored value is left unchanged.

    The :ARGUMENT-PRECEDENCE-ORDER argument is a list of symbols.

    An error is signaled if this argument appears but the :LAMBDA-LIST
    argument does not appear. An error is signaled if this value is
    not a proper list or if this value is not a permutation of the
    symbols from the required arguments part of the :LAMBDA-LIST
    initialization argument.

    When the generic function is being initialized or reinitialized,
    and this argument is not supplied, but the :LAMBDA-LIST argument
    is supplied, this value defaults to the symbols from the required
    arguments part of the :LAMBDA-LIST argument, in the order they
    appear in that argument. If neither argument is supplied, neither
    are initialized (see the description of :LAMBDA-LIST.)

    The :DECLARATIONS argument is a list of declarations.

    An error is signaled if this value is not a proper list or if each
    of its elements is not a legal declaration.

    When the generic function is being initialized, and this argument
    is not supplied, it defaults to the empty list.

    The :DOCUMENTATION argument is a string or NIL.

    An error is signaled if this value is not a string or NIL.

    If the generic function is being initialized, this argument
    defaults to NIL.

    The :LAMBDA-LIST argument is a lambda list.

    An error is signaled if this value is not a proper generic
    function lambda list.

    When the generic function is being initialized, and this argument
    is not supplied, the generic function's lambda list is not
    initialized. The lambda list will be initialized later, either
    when the first method is added to the generic function, or a later
    reinitialization of the generic function.

    The :METHOD-COMBINATION argument is a method combination
    metaobject.

    The :METHOD-CLASS argument is a class metaobject.

    An error is signaled if this value is not a subclass of the class
    METHOD.

    When the generic function is being initialized, and this argument
    is not supplied, it defaults to the class STANDARD-METHOD.

    The :NAME argument is an object.

    If the generic function is being initialized, this argument
    defaults to NIL.

After the processing and defaulting of initialization arguments
described above, the value of each initialization argument is
associated with the generic function metaobject. These values can then
be accessed by calling the corresponding generic function. The
correspondences are as follows:

Table 2: Initialization arguments and accessors for generic function metaobjects. 

Initialization Argument		Generic Function
--------------------------------------------------------------------------
:argument-precedence-order 	generic-function-argument-precedence-order
:declarations 			generic-function-declarations
:documentation 			documentation
:lambda-list 			generic-function-lambda-list
:method-combination 		generic-function-method-combination
:method-class 			generic-function-method-class
:name 				generic-function-name

Methods:

It is not specified which methods provide the initialization and
reinitialization behavior described above. Instead, the information
needed to allow portable programs to specialize this behavior is
presented as a set of restrictions on the methods a portable program
can define. The model is that portable initialization methods have
access to the generic function metaobject when either all or none of
the specified initialization has taken effect."""
        # Unregistered Issue COMPLIANCE-METHOD-CLASS-ARGUMENT-TYPE-CHECK-NOT-PRECISE-ENOUGH
        if _specifiedp(argument_precedence_order):
                if not _specifiedp(lambda_list):
                        error("MAKE-INSTANCE STANDARD-GENERIC-FUNCTION: :ARGUMENT-PRECEDENCE-ORDER "
                              "was provided, but :LAMBDA-LIST was not.")
                elif not (_listp(argument) and
                          _set(argument_precedence_order) == _set(lambda_list[0])):
                        error("MAKE-INSTANCE STANDARD-GENERIC-FUNCTION: :ARGUMENT-PRECEDENCE-ORDER, "
                              "when specified, must be a permutation of fixed arguments in :LAMBDA-LIST.")
                generic_function.argument_precedence_order = _tuple(argument_precedence_order)
        elif _specifiedp(lambda_list):
                generic_function.argument_precedence_order = _tuple(lambda_list[0])
        generic_function.declarations        = _tuple(_defaulted(declarations, _list(),
                                                                 type = (list_,
                                                                         (satisfies_, _valid_declaration_p))))
        generic_function.documentation       = _defaulted(documentation, nil,
                                              type = (or_, string_, (eql_, nil)))
        if _specifiedp(lambda_list):
                # XXX: _not_implemented("lambda-list validation")
                generic_function.lambda_list = lambda_list
        generic_function.method_combination  = _defaulted(method_combination, standard_method_combination,
                                                          type = method_combination)
        generic_function.method_class        = _defaulted(method_class, standard_method,
                                                          type = _type) # method metaclass
        generic_function.name                = _defaulted(name, nil)
        # The discriminating function may reuse the
        # list of applicable methods without calling
        # COMPUTE-APPLICABLE-METHODS-USING-CLASSES again provided that:
        # (ii) the generic function has not been reinitialized,
        generic_function.__applicable_method_cache__ = _dict() # (list_, _type) -> _list
        filename, lineno = (_defaulted(filename, "<unknown>"),
                            _defaulted(lineno,   0))
        _update_generic_function_and_dependents(
                generic_function,
                **_only_specified_keys(argument_precedence_order = argument_precedence_order,
                                       declarations = declarations,
                                       documentation = documentation,
                                       lambda_list = lambda_list,
                                       method_combination = method_combination,
                                       method_class = method_class,
                                       name = name,
                                       # extensions
                                       filename = filename,
                                       lineno = lineno))
        # Simulate a python function (XXX: factor):
        generic_function.__doc__ = documentation
        generic_function.__code__.co_filename    = filename
        generic_function.__code__.co_firstlineno = lineno
        return generic_function

def generic_function_argument_precedence_order(x): return x.argument_precedence_order
def generic_function_declarations(x):              return x.declarations
def generic_function_lambda_list(x):               return x.lambda_list
def generic_function_method_combination(x):        return x.method_combination
def generic_function_method_class(x):              return x.method_class
def generic_function_name(x):                      return x.name

def generic_function_p(x): return functionp(x) and _hasattr(x, "__methods__")  # XXX: CL+
def method_p(x):           return functionp(x) and _hasattr(x, "specializers") # XXX: CL+
def _specializerp(x):       return ((x is t)        or
                                    typep(x, (or_, _type, (_tuple, (eql_, eql_), t))))

def _get_generic_fun_info(generic_function):
        return values(_len(generic_function.lambda_list[0]), # nreq
                      nil,
                      [],
                      _len(generic_function.lambda_list[3]),
                      generic_function.lambda_list)

def generic_function_methods(x):                   return x.__methods__.values()

__method_combinations__ = _dict()

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
    order specified by the :ORDER option.

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
    by LAMBDA-LIST is bound to a form that can be inserted into the
    effective method. When this form is evaluated during execution of
    the effective method, its value is the corresponding argument to
    the generic function; the consequences of using such a form as the
    place in a setf form are undefined. Argument correspondence is
    computed by dividing the :ARGUMENTS LAMBDA-LIST and the generic
    function LAMBDA-LIST into three sections: the required parameters,
    the optional parameters, and the keyword and rest parameters. The
    arguments supplied to the generic function for a particular call
    are also divided into three sections; the required arguments
    section contains as many arguments as the generic function has
    required parameters, the optional arguments section contains as
    many arguments as the generic function has optional parameters,
    and the keyword/rest arguments section contains the remaining
    arguments. Each parameter in the required and optional sections of
    the :ARGUMENTS LAMBDA-LIST accesses the argument at the same
    position in the corresponding section of the arguments. If the
    section of the :ARGUMENTS LAMBDA-LIST is shorter, extra arguments
    are ignored. If the section of the :ARGUMENTS LAMBDA-LIST is
    longer, excess required parameters are bound to forms that
    evaluate to NIL and excess optional parameters are bound to their
    initforms. The keyword parameters and rest parameters in the
    :ARGUMENTS LAMBDA-LIST access the keyword/rest section of the
    arguments. If the :ARGUMENTS LAMBDA-LIST contains &key, it behaves
    as if it also contained &allow-other-keys.

    In addition, &whole var can be placed first in the :ARGUMENTS
    LAMBDA-LIST. It causes var to be bound to a form that evaluates to
    a list of all of the arguments supplied to the generic
    function. This is different from &rest because it accesses all of
    the arguments, not just the keyword/rest arguments.

    Erroneous conditions detected by the body should be reported with
    METHOD-COMBINATION-ERROR or INVALID-METHOD-ERROR; these functions
    add any necessary contextual information to the error message and
    will signal the appropriate error.

    The BODY forms are evaluated inside of the bindings created by the
    lambda list and method group specifiers. Declarations at the head
    of the BODY are positioned directly inside of bindings created by
    the lambda list and outside of the bindings of the method group
    variables. Thus method group variables cannot be declared in this
    way. LOCALLY may be used around the BODY, however.

    Within the BODY forms, GENERIC-FUNCTION-SYMBOL is bound to the
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
        # Unregistered Issue COMPLIANCE-ARGUMENTS-LAMBDA-LIST-NOT-IMPLEMENTED
        # Unregistered Issue COMPLIANCE-ERRORNEOUS-CONDITION-REPORTING
        # Unregistered Issue COMPLIANCE-SPECIAL-CASE-(APPLY #'FORMAT STREAM FORMAT-CONTROL (METHOD-QUALIFIERS METHOD))-NOT-IMPLEMENTED
        # Unregistered Issue PERFORMANCE-SINGLE-APPLICABLE-METHOD-OPTIMISATION
        check_type(method_group_specifiers,
                  (list_,
                   (varituple_,
                    symbol,       # group name
                    (or_, (list_, (or_, _tuple, (eql_, star))), # We're off the spec a little here,
                                                                # but it's a minor syntactic issue.
                          function_),
                    # the rest is actually a plist, but we cannot (yet) describe it
                    # in terms of a type.
                    (maybe_, (tuple_,
                              (eql_, _keyword("description")),
                              string_)),
                    (maybe_, (tuple_,
                              (eql_, _keyword("order")),
                              (member_,
                               _keyword("most-specific-first"),
                               _keyword("most-specific-last")))),
                    (maybe_, (tuple_,
                              (eql_, _keyword("required")),
                              (member_, t, nil))))))
        # check_type(arguments, (maybe_, lambda_list_))
        check_type(arguments, (maybe_, (tuple_,
                                        (list_, string_),
                                        (list_, string_),
                                        (maybe_, string_),
                                        (list_, string_),
                                        (maybe_, string_))))
        check_type(generic_function, (maybe_, symbol))
        ### VARI-BIND, anyone?
        # def vari_bind(x, body):
        #         # don't lambda lists actually rule this, hands down?
        #         posnal, named = argspec_value_varivals(inspect.getfullargspec(body), x)
        #         return body()
        method_group = defstruct("method_group",
                                 "name",
                                 "qualifier_spec",
                                 "description",
                                 "most_specific_first",
                                 "required")
        groups = _dict()
        for mgspec in method_group_specifiers:
                gname, qualifier_spec = mgspec[:2]
                options = mgspec[2:]
                options_dict = _map_into_hash_star(lambda keyword, v: (symbol_name(keyword), v), options)
                (lambda description = "Method group %s.",
                        required = nil,
                        order = _keyword("most_specific_first"):
                        groups.update({gname: method_group(gname,
                                                           qualifier_spec,
                                                           description,
                                                           order is _keyword("most_specific_first"),
                                                           required)}))(**options_dict)
        def method_combination(applicable_methods, *args, **keys):
               # The LAMBDA-LIST receives any arguments provided after the name of
               # the method combination type in the :METHOD-COMBINATION option to
               # DEFGENERIC.
               # /citation
               #
               # The BODY forms are evaluated inside of the bindings created by the
               # lambda list and method group specifiers.
               # /citation
               #
               # The effective method is evaluated in the null lexical environment
               # augmented with a local macro definition for CALL-METHOD and with
               # bindings named by symbols not accessible from the COMMON-LISP-USER
               # package.
               # /citation
               #
               # Within the BODY forms, GENERIC-FUNCTION-SYMBOL is bound to the
               # generic function object.
               # /citation
               def method_qualifiers_match_pattern_p(qualifiers, pattern):
                       return (t if pattern is star else
                               qualifiers == pattern)
               grouped_methods = defaultdict(list)
               for method in applicable_methods:
                       qualifiers = method_qualifiers(method)
                       for group in groups.values():
                               qualifier_spec = group.qualifier_spec
                               ## qualifier_spec:
                               # (or_, (list_, (or_, star, _list)),
                               #       function),
                               if ((_listp(qualifier_spec) and
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
               ## So: must bind group names and CALL-METHOD, which, in turn
               ## must bind CALL-NEXT-METHOD and NEXT-METHOD-P.  I presume.
               # BODY must, therefore, return some kind of an AST representation?
               # I guess, well, that we could play the CL games with that.
               # Yes, I'm thinking of real macros..
               # Maybe just arg it?
               body_args = _dict(grouped_methods)
               def call_method(method, next_methods = None):
                       next_methods = _defaulted(next_methods, [])
                       # sounds like we ought to look up the compiled METHOD-LAMBDA?
                       # Given a method object in one of the lists produced
                       # by the method group specifiers and a list of next
                       # methods, CALL-METHOD will invoke the method such that
                       # CALL-NEXT-METHOD has available the next methods.
                       method_lambda
               body_args.update({ "call_method": call_method })
               return body(**body_args)
        method_combination.name                        = the(symbol, name)
        method_combination.__method_group_specifiers__ = method_group_specifiers
        return method_combination

# 7.6.6.2 Standard Method Combination
#
# Standard method combination is supported by the class
# standard-generic-function. It is used if no other type of method
# combination is specified or if the built-in method combination type
# standard is specified.
#
# Primary methods define the main action of the effective method, while
# auxiliary methods modify that action in one of three ways. A primary
# method has no method qualifiers.
#
# An auxiliary method is a method whose qualifier is :before, :after,
# or :around. Standard method combination allows no more than one
# qualifier per method; if a method definition specifies more than one
# qualifier per method, an error is signaled.
#
# * A before method has the keyword :before as its only qualifier. A
#   before method specifies code that is to be run before any primary
#   methods.
#
# * An after method has the keyword :after as its only qualifier. An
#   after method specifies code that is to be run after primary methods.
#
# * An around method has the keyword :around as its only qualifier. An
#   around method specifies code that is to be run instead of other
#   applicable methods, but which might contain explicit code which
#   calls some of those shadowed methods (via call-next-method).
#
# The semantics of standard method combination is as follows:
#
# * If there are any around methods, the most specific around method is
#   called. It supplies the value or values of the generic function.
#
# * Inside the body of an around method, call-next-method can be used to
#   call the next method. When the next method returns, the around
#   method can execute more code, perhaps based on the returned value or
#   values. The generic function no-next-method is invoked if
#   call-next-method is used and there is no applicable method to
#   call. The function next-method-p may be used to determine whether a
#   next method exists.
#
# * If an around method invokes call-next-method, the next most specific
#   around method is called, if one is applicable. If there are no
#   around methods or if call-next-method is called by the least
#   specific around method, the other methods are called as follows:
#
#     -- All the before methods are called, in most-specific-first
#        order. Their values are ignored. An error is signaled if
#        call-next-method is used in a before method.
#
#     -- The most specific primary method is called. Inside the body of
#        a primary method, call-next-method may be used to call the next
#        most specific primary method. When that method returns, the
#        previous primary method can execute more code, perhaps based on
#        the returned value or values. The generic function
#        no-next-method is invoked if call-next-method is used and there
#        are no more applicable primary methods. The function
#        next-method-p may be used to determine whether a next method
#        exists. If call-next-method is not used, only the most specific
#        primary method is called.
#
#     -- All the after methods are called in most-specific-last
#        order. Their values are ignored. An error is signaled if
#        call-next-method is used in an after method.
#
# * If no around methods were invoked, the most specific primary method
#   supplies the value or values returned by the generic function. The
#   value or values returned by the invocation of call-next-method in
#   the least specific around method are those returned by the most
#   specific primary method.
#
# In standard method combination, if there is an applicable method but
# no applicable primary method, an error is signaled.
#
# The before methods are run in most-specific-first order while the
# after methods are run in least-specific-first order. The design
# rationale for this difference can be illustrated with an
# example. Suppose class C1 modifies the behavior of its superclass, C2,
# by adding before methods and after methods. Whether the behavior of
# the class C2 is defined directly by methods on C2 or is inherited from
# its superclasses does not affect the relative order of invocation of
# methods on instances of the class C1. Class C1's before method runs
# before all of class C2's methods. Class C1's after method runs after
# all of class C2's methods.
#
# By contrast, all around methods run before any other methods run. Thus
# a less specific around method runs before a more specific primary
# method.
#
# If only primary methods are used and if call-next-method is not used,
# only the most specific method is invoked; that is, more specific
# methods shadow more general ones.
standard_method_combination = define_method_combination(
        _i("STANDARD"),
        [(_i("around"),  [(_keyword("around"),)]),
         (_i("before"),  [(_keyword("before"),)]),
         (_i("primary"), [tuple()],
                         (_keyword("required"), t)),
         (_i("after"),   [(_keyword("after"),)],
                         (_keyword("order"),    _keyword("most-specific-last")))],
        lambda: # "around", "before", "primary" and "after" are bound "magically",
                # to avoid duplication.
                [])

def make_method_lambda(generic_function, method, lambda_expression, environment):
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
        """Return an expression compileable (by whom? compute-effective-method?)
        down to a function, accepting (gf-arglist &rest (subseq c-m-args 1)),
        responsible to invoke the method and ."""
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
        for i in _range(nreq):
                if not arguments:
                        error_need_at_least_n_args(generic_function_name(generic_function),
                                                   nreq)
                        arg = arguments.pop()
                        types_rev.append([type_modifier, arg] if type_modifier else
                                         arg)
        return values(types_rev, arg_info)

def _arg_info_precedence(arg_info: "lambda list, actually.."):
        return _range(_len(arg_info[0]))

def _compute_applicable_methods_using_types(generic_function, types_):
        definite_p, possibly_applicable_methods = t, []
        # Not safe against method list modifications by another thread!
        for method in generic_function_methods(generic_function):
                specls = method_specializers(method) # Was: if (consp method)
                types = _list(types_)
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
                                                       _reversed(possibly_applicable_methods),
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
                     (class_eq_,   lambda: _saut_class_eq(specl, type)),
                     # (class-eq   (saut-class-eq specl type)),
                     # (eql    (saut-eql specl type)),
                     (t,       lambda: error("%s cannot handle the second argument %s.",
                                             "specializer-applicable-using-type-p",
                                             type))))

def _saut_class_eq(specl, type):
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
                     (_type, lambda: case(car(type2),
                                          (_type, compare_classes_function(specl1, specl2, index)),
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

def error_need_at_least_n_args(function, n):
        error("The function %s requires at least %d arguments.", function, n)

__sealed_classes__ = _set([object,
                           integer, boolean, float, complex,
                           string_, _tuple, _bytes,
                           _list, _bytearray,
                           _set, _frozenset,
                           hash_table,
                           function_,
                           stream_,
                           BaseException, Exception] +
                          mapcar(type_of,
                                 [None,           # NoneType
                                  Ellipsis,       # ellipsis
                                  NotImplemented, # NotImplementedType
                                  integer,        # type
                                  "".find,        # builtin_function_or_method
                                  ast,            # module
                                  sys.stdin,      # __io.TextIOWrapper
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

def compute_discriminating_function(generic_function):
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
        (function_name,
         lambda_list,
         applicable_method_cache,
         filename,
         lineno) = (generic_function.name,
                    generic_function.lambda_list,
                    generic_function.__applicable_method_cache__,
                    generic_function.__code__.co_filename,
                    generic_function.__code__.co_lineno)
        fixed, optional, args, keyword, keys = lambda_list
        nfixed = _len(fixed)
        def dfun_compute_applicable_methods(generic_function, args):
                if _len(args) < nfixed:
                        error_need_at_least_n_args(function_name, nfixed)
                dispatch_args      = args[:nfixed]
                dispatch_arg_types = _tuple(_type(x) for x in dispatch_args)
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
                unsealed_classes = _set(x for x in dispatch_arg_types if not _class_sealed_p(x))
                applicable_method_cache_key = dispatch_arg_types + reduce(lambda acc, x: acc + x.__mro__,
                                                                          _sorted(unsealed_classes, key = lambda type: type.__name__),
                                                                          _tuple())
                # We ought to pay the high price of (iv) and (v), because we can't hook
                # into the Python's object system.
                applicable, hit = gethash(applicable_method_cache_key, applicable_method_cache)
                if hit:
                        # The discriminating function may reuse the
                        # list of applicable methods without calling
                        # COMPUTE-APPLICABLE-METHODS-USING-CLASSES again provided that:
                        # (i) the generic function is being called again with required
                        #     arguments which are instances of the same classes,
                        return applicable
                _here("gf: %s, ll: %s", generic_function, generic_function.lambda_list)
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
        env = _dict(compute_effective_method        = compute_effective_method,
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
                                        generic_function_class = standard_generic_function,
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
GENERIC-FUNCTION argument is then returned."""
        # Unregistered Issue COMPLIANCE-SETF-LIST-NAMES-NOT-SUPPORTED
        # Unregistered Issue COMPLIANCE-GENERIC-FUNCTION-CLASS-AS-NAME-NOT-SUPPORTED
        # Unregistered Issue COMPLIANCE-GENERIC-FUNCTION-REINITIALIZE-INSTANCE-SURROGATE-CALLED
        ###
        ### First step, "compute the set of initialization arguments":
        if gfun:
                # DEFGENERIC (CLHS) documentation speaks so about method removal/addition:
                ## The effect of the DEFGENERIC macro is as if the following three steps
                ## were performed: first, methods defined by previous DEFGENERIC forms
                ## are removed; second, ENSURE-GENERIC-FUNCTION is called; and finally,
                ## methods specified by the current DEFGENERIC form are added to the
                ## generic function.
                ## /citation
                # ..however, in the documentation of ENSURE-GENERIC-FUNCTION (AMOP):
                ## The behavior of this function is actually implemented by the generic
                ## function ENSURE-GENERIC-FUNCTION-USING-CLASS. When ENSURE-GENERIC-FUNCTION
                ## is called, it immediately calls ENSURE-GENERIC-FUNCTION-USING-CLASS and
                ## returns that result as its own.
                # ..and so, we decide that AMOP trumps CLHS.
                mapc(curry(remove_method, gfun), generic_function_methods(gfun))
        if lambda_list:
                fixed, optional, args, keyword, keys = lambda_list
                if some(lambda x: x[1] is not None, _list(optional) + _list(keyword)):
                        error("Generic function arglist cannot specify default parameter values.")
        initargs = _only_specified_keys(
                argument_precedence_order = argument_precedence_order,
                declarations              = declarations,
                documentation             = documentation,
                lambda_list               = lambda_list,
                method_class              = method_class,
                method_combination        = method_combination,
                name                      = name,
                # incompatible..
                filename                  = filename,
                lineno                    = lineno)
        initargs.update(keys)
        ###
        ### Second step:
        if not generic_function:
                # If the GENERIC-FUNCTION argument is NIL, an instance of the class
                # specified by the :GENERIC-FUNCTION-CLASS argument is created by
                # calling MAKE-INSTANCE with the previously computed initialization arguments.
                # The function name FUNCTION-NAME is set to name the generic function.
                generic_function = make_instance(generic_function_class, **initargs)
                # _standard_generic_function_shared_initialize is called by s-g-f.__init__
                _setf_global(function_name, generic_function)
        else:
                if class_of(generic_function) is not generic_function_class:
                        # If the class of the GENERIC-FUNCTION argument is not the same as the
                        # class specified by the :GENERIC-FUNCTION-CLASS argument, an error is
                        # signaled.
                        error("ENSURE-GENERIC-FUNCTION-USING-CLASS: ")
                # Otherwise the generic function GENERIC-FUNCTION is redefined by
                # calling the REINITIALIZE-INSTANCE generic function with
                # GENERIC-FUNCTION and the initialization arguments. The
                # GENERIC-FUNCTION argument is then returned.
                # reinitialize_instance(generic_function, **initargs) # does not do much, beyond __dict__ update
                _standard_generic_function_shared_initialize(generic_function, **initargs)
        return generic_function

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
        # Issue GLOBALS-SPECIFIED-TO-REFER-TO-THE-CONTAINING-MODULE-NOT-THE-CALLING-ONE
        maybe_gfun, therep = _defaulted(_global(the(string_, function_name)), nil)
        if functionp(maybe_gfun) and not generic_function_p(maybe_gfun):
                error("%s already names an ordinary function.", function_name)
        return ensure_generic_function_using_class(maybe_gfun, function_name, **keys)

def defgeneric(_ = None,
               argument_precedence_order = _keyword("most-specific-first"),
               documentation = None,
               method_combination = standard_method_combination,
               generic_function_class = standard_generic_function,
               method_class = standard_method):
# Unregistered Issue: COMPLIANCE-DEFGENERIC-METHOD-DESCRIPTIONS-UNIMPLEMENTABLE
        """defgeneric function-name gf-lambda-list [[option | {method-description}*]]

=> new-generic

option::= (:argument-precedence-order parameter-name+) |
          (declare gf-declaration+) |
          (:documentation gf-documentation) |
          (:method-combination method-combination method-combination-argument*) | 
          (:generic-function-class generic-function-class) |
          (:method-class method-class)

method-description::= (:method method-qualifier* specialized-lambda-list [[declaration* | documentation]] form*) 

Arguments and Values:

FUNCTION-NAME---a function name.

GENERIC-FUNCTION-CLASS---a non-NIL symbol naming a class.

GF-DECLARATION---an optimize declaration specifier; other declaration specifiers are not permitted.

GF-DOCUMENTATION---a string; not evaluated.

GF-LAMBDA-LIST---a generic function lambda list.

METHOD-CLASS---a non-NIL symbol naming a class.

METHOD-COMBINATION-ARGUMENT---an object.

METHOD-COMBINATION-NAME---a symbol naming a method combination type.

METHOD-QUALIFIERS, SPECIALIZED-LAMBDA-LIST, DECLARATIONS, DOCUMENTATION, FORMS---as per DEFMETHOD.

NEW-GENERIC---the generic function object.

PARAMETER-NAME---a symbol that names a required parameter in the
LAMBDA-LIST. (If the :ARGUMENT-PRECEDENCE-ORDER option is specified,
each required parameter in the LAMBDA-LIST must be used exactly once
as a PARAMETER-NAME.)

Description:

The macro DEFGENERIC is used to define a generic function or to
specify options and declarations that pertain to a generic function as
a whole.

If FUNCTION-NAME is a list it must be of the form (SETF SYMBOL). If
(FBOUNDP FUNCTION-NAME) is false, a new generic function is
created. If (FDEFINITION FUNCTION-NAME) is a generic function, that
generic function is modified. If FUNCTION-NAME names an ordinary
function, a macro, or a special operator, an error is signaled.

The effect of the DEFGENERIC macro is as if the following three steps
were performed: first, methods defined by previous DEFGENERIC forms
are removed; second, ENSURE-GENERIC-FUNCTION is called; and finally,
methods specified by the current DEFGENERIC form are added to the
generic function.

Each METHOD-DESCRIPTION defines a method on the generic function. The
lambda list of each method must be congruent with the lambda list
specified by the GF-LAMBDA-LIST option. If no method descriptions are
specified and a generic function of the same name does not already
exist, a generic function with no methods is created.

The GF-LAMBDA-LIST argument of defgeneric specifies the shape of
lambda lists for the methods on this generic function. All methods on
the resulting generic function must have lambda lists that are
congruent with this shape. If a DEFGENERIC form is evaluated and some
methods for that generic function have lambda lists that are not
congruent with that given in the DEFGENERIC form, an error is
signaled. For further details on method congruence, see Section 7.6.4
(Congruent Lambda-lists for all Methods of a Generic Function).

The generic function passes to the method all the argument values
passed to it, and only those; default values are not supported. Note
that optional and keyword arguments in method definitions, however,
can have default initial value forms and can use SUPPLIED-P
parameters.

The following options are provided. Except as otherwise noted, a given
option may occur only once.

    The :argument-precedence-order option is used to specify the order
    in which the required arguments in a call to the generic function
    are tested for specificity when selecting a particular
    method. Each required argument, as specified in the gf-lambda-list
    argument, must be included exactly once as a parameter-name so
    that the full and unambiguous precedence order is supplied. If
    this condition is not met, an error is signaled.

    The declare option is used to specify declarations that pertain to
    the generic function.

    An optimize declaration specifier is allowed. It specifies whether
    method selection should be optimized for speed or space, but it
    has no effect on methods. To control how a method is optimized, an
    optimize declaration must be placed directly in the defmethod form
    or method description. The optimization qualities speed and space
    are the only qualities this standard requires, but an
    implementation can extend the object system to recognize other
    qualities. A simple implementation that has only one method
    selection technique and ignores optimize declaration specifiers is
    valid.

    The special, ftype, function, inline, notinline, and declaration
    declarations are not permitted. Individual implementations can
    extend the declare option to support additional declarations. If
    an implementation notices a declaration specifier that it does not
    support and that has not been proclaimed as a non-standard
    declaration identifier name in a declaration proclamation, it
    should issue a warning.

    The declare option may be specified more than once. The effect is
    the same as if the lists of declaration specifiers had been
    appended together into a single list and specified as a single
    declare option.

    The :documentation argument is a documentation string to be
    attached to the generic function object, and to be attached with
    kind function to the function-name.

    The :generic-function-class option may be used to specify that the
    generic function is to have a different class than the default
    provided by the system (the class standard-generic-function). The
    class-name argument is the name of a class that can be the class
    of a generic function. If function-name specifies an existing
    generic function that has a different value for the
    :generic-function-class argument and the new generic function
    class is compatible with the old, change-class is called to change
    the class of the generic function; otherwise an error is signaled.

    The :method-class option is used to specify that all methods on
    this generic function are to have a different class from the
    default provided by the system (the class standard-method). The
    class-name argument is the name of a class that is capable of
    being the class of a method.

    The :method-combination option is followed by a symbol that names
    a type of method combination. The arguments (if any) that follow
    that symbol depend on the type of method combination. Note that
    the standard method combination type does not support any
    arguments. However, all types of method combination defined by the
    short form of define-method-combination accept an optional
    argument named order, defaulting to :most-specific-first, where a
    value of :most-specific-last reverses the order of the primary
    methods without affecting the order of the auxiliary methods.

The method-description arguments define methods that will be
associated with the generic function. The method-qualifier and
specialized-lambda-list arguments in a method description are the same
as for defmethod.

The form arguments specify the method body. The body of the method is
enclosed in an implicit block. If function-name is a symbol, this
block bears the same name as the generic function. If function-name is
a list of the form (setf symbol), the name of the block is symbol.

Implementations can extend defgeneric to include other options. It is
required that an implementation signal an error if it observes an
option that is not implemented locally.

defgeneric is not required to perform any compile-time side
effects. In particular, the methods are not installed for invocation
during compilation. An implementation may choose to store information
about the generic function for the purposes of compile-time
error-checking (such as checking the number of arguments on calls, or
noting that a definition for the function name has been seen)."""
        if _ is not None:
                error("DEFGENERIC must be used be as a decorator call.")
                # The rationale is that the gfun arglist is precious, and
                # the decorator is the only place to have a sane arglist.
        if documentation is not None:
                error("DEFGENERIC :DOCUMENTATION is provided through the docstring instead.")
        def do_defgeneric(fn):
                # option::= (:argument-precedence-order parameter-name+) |
                #           (declare gf-declaration+) |
                #           (:documentation gf-documentation) |
                #           (:method-combination method-combination method-combination-argument*) |
                #           (:generic-function-class generic-function-class) |
                #           (:method-class method-class)
                return ensure_generic_function(fn.__name__,
                                               argument_precedence_order = argument_precedence_order,
                                               documentation             = fn.__doc__,
                                               method_combination        = method_combination,
                                               generic_function_class    = generic_function_class,
                                               lambda_list               = _function_lambda_list(fn),
                                               method_class              = method_class,
                                               #
                                               filename      = fn.__code__.co_filename,
                                               lineno        = fn.__code__.co_firstlineno)
        return do_defgeneric

def _method_agrees_with_qualifiers_specializers(method, qualifiers, specializers):
        """7.6.3 Agreement on Parameter Specializers and Qualifiers

Two methods are said to agree with each other on parameter
specializers and qualifiers if the following conditions hold:

1. Both methods have the same number of required parameters. Suppose
the parameter specializers of the two methods are P1,1...P1,n and
P2,1...P2,n.

2. For each 1<=i<=n, P1,i agrees with P2,i. The parameter specializer
P1,i agrees with P2,i if P1,i and P2,i are the same class or if
P1,i=(eql object1), P2,i=(eql object2), and (eql object1
object2). Otherwise P1,i and P2,i do not agree.

3. The two lists of qualifiers are the same under equal."""
        lambda_list = method_lambda_list(method)
        return (_len(lambda_list[0]) == _len(specializers)       and
                every(lambda ms, s: ((ms is s) or
                                     (_listp(ms) and _listp(s) and
                                      _len(ms) == _len(s) == 2 and
                                      ms[0] == s[0] == eql_    and
                                      eql(ms[1], s[1]))),
                      method_specializers(method), specializers) and
                equal(method_qualifiers(method), qualifiers))

def _generic_function_lambda_list_incongruent_with_method_list_p(generic_function_lambda_list,
                                                                 method_lambda_list):
        """7.6.4 Congruent Lambda-lists for all Methods of a Generic Function

These rules define the congruence of a set of lambda lists, including
the lambda list of each method for a given generic function and the
lambda list specified for the generic function itself, if given.

1. Each lambda list must have the same number of required parameters.

2. Each lambda list must have the same number of optional
parameters. Each method can supply its own default for an optional
parameter.

3. If any lambda list mentions &rest or &key, each lambda list must
mention one or both of them.

4. If the generic function lambda list mentions &key, each method must
accept all of the keyword names mentioned after &key, either by
accepting them explicitly, by specifying &allow-other-keys, or by
specifying &rest but not &key. Each method can accept additional
keyword arguments of its own. The checking of the validity of keyword
names is done in the generic function, not in each method. A method is
invoked as if the keyword argument pair whose name is
:allow-other-keys and whose value is true were supplied, though no
such argument pair will be passed.

5. The use of &allow-other-keys need not be consistent across lambda
lists. If &allow-other-keys is mentioned in the lambda list of any
applicable method or of the generic function, any keyword arguments
may be mentioned in the call to the generic function.

6. The use of &aux need not be consistent across methods.

If a method-defining operator that cannot specify generic function
options creates a generic function, and if the lambda list for the
method mentions keyword arguments, the lambda list of the generic
function will mention &key (but no keyword arguments)."""
# Unregistered Issue COMPLIANCE-SPEC-UNCLEAR-LAST-PASSAGE-LAMBDA-LIST-CONGRUENCE
        gf_fixed, gf_optional, gf_args, gf_keyword, gf_keys = generic_function_lambda_list
        m_fixed,  m_optional,  m_args,  m_keyword,  m_keys  = method_lambda_list
        return ((_len(gf_fixed)    != _len(m_fixed) and
                 "the method has %s required arguments than the generic function" %
                 ("more" if _len(m_fixed) > _len(gf_fixed) else "less"))                                      or
                (_len(gf_optional) != _len(m_optional) and
                 "the method has %s optional arguments than the generic function" %
                 ("more" if _len(m_fixed) > _len(gf_fixed) else "less"))                                      or
                (_xorf(gf_args, m_args) and
                 "but the method and generic function differ in whether they accept &REST or &KEY arguments") or
                # XXX: #3 compliance -- still looks fishy
                (_xorf(gf_keyword or gf_keys,
                       m_keyword  or m_keys) and
                 "but the method and generic function differ in whether they accept &REST or &KEY arguments") or
                (((not gf_keyword) or
                  m_keys           or
                  not (_set(gf_keyword) - _set(m_keyword))) and
                 "but the method does not accept each of the &KEY arguments %s" % _tuple([gf_keyword])))

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
        # Unregistered Issue COMPLIANCE-UNCLEAR-METHOD-LAMBDA-LIST-SPECIALIZERS-INCLUSION
        congruence_error = _generic_function_lambda_list_incongruent_with_method_list_p(
                generic_function_lambda_list(generic_function),
                method_lambda_list(method))
        if congruence_error:
                error("attempt to add the method %s to the generic function %s; but %s.",
                      method, generic_function, congruence_error)
        if slot_boundp(method, "__generic_function__") and method.__generic_function__:
                error("ADD-METHOD called to add %s, when it was already attached to %s.",
                      method, method.__generic_function__)
        old_method = find_if(lambda m: _method_agrees_with_qualifiers_specializers(m,
                                                                                   method_qualifiers(method),
                                                                                   method_specializers(method)),
                             generic_function_methods(generic_function))
        if old_method:
                remove_method(generic_function, old_method)
        generic_function.__methods__[method.specializers] = method
        method.__generic_function__ = generic_function
        for s in method_specializers(method):
                add_direct_method(s, method)
        _update_generic_function_and_dependents(generic_function, add_method = method)
        return generic_function

def set_funcallable_instance_function(funcallable_instance, function):
        """set-funcallable-instance-function funcallable-instance function

Arguments:

The FUNCALLABLE-INSTANCE argument is a funcallable instance (it must
have been returned by ALLOCATE-INSTANCE (FUNCALLABLE-STANDARD-CLASS)).

The FUNCTION argument is a function.

Values:

The value returned by this function is unspecified.

Purpose:

This function is called to set or to change the function of a
funcallable instance. After SET-FUNCALLABLE-INSTANCE-FUNCTION is
called, any subsequent calls to FUNCALLABLE-INSTANCE will run the new
FUNCTION."""
        # XXX: better to:
        # 1. override __call__ with a properly-arglisted thing
        # 2. pass through __code__ and maybe others
        funcallable_instance.function = function

def add_direct_method(specializer, method):
        """This generic function is called to maintain a set of
backpointers from a SPECIALIZER to the set of methods specialized to
it. If METHOD is already in the set, it is not added again (no error
is signaled).

This set can be accessed as a list by calling the generic function
SPECIALIZER-DIRECT-METHODS. Methods are removed from the set by
REMOVE-DIRECT-METHOD.

The generic function ADD-DIRECT-METHOD is called by ADD-METHOD
whenever a method is added to a generic function. It is called once
for each of the specializers of the METHOD. Note that in cases where a
specializer appears more than once in the specializers of a METHOD,
this generic function will be called more than once with the same
specializer as argument.

The results are undefined if the SPECIALIZER argument is not one of
the specializers of the METHOD argument."""
        _not_implemented("maintain a set of backpointers from a SPECIALIZER to the set of methods specialized to it")

def _standard_method_shared_initialize(method,
                                       qualifiers = None,
                                       lambda_list = None,
                                       specializers = None,
                                       function = None,
                                       documentation = None,
                                       slot_definition = None,
                                       # extensions
                                       filename = None,
                                       lineno = None):
        """Initialization of Method Metaobjects

A method metaobject can be created by calling MAKE-INSTANCE. The
initialization arguments establish the definition of the METHOD. A
method metaobject cannot be redefined; calling REINITIALIZE-INSTANCE
signals an error.

Initialization of a METHOD metaobject must be done by calling
MAKE-INSTANCE and allowing it to call INITIALIZE-INSTANCE. Portable
programs must not call INITIALIZE-INSTANCE directly to initialize a
method metaoject. Portable programs must not call shared-initialize
directly to initialize a method metaobject. Portable programs must not
call CHANGE-CLASS to change the class of any method metaobject or to
turn a non-method object into a method metaobject.

Since metaobject classes may not be redefined, no behavior is
specified for the result of calls to
UPDATE-INSTANCE-FOR-REDEFINED-CLASS on method metaobjects. Since the
class of a method metaobject cannot be changed, no behavior is
specified for the result of calls to
UPDATE-INSTANCE-FOR-DIFFERENT-CLASS on method metaobjects.

During initialization, each initialization argument is checked for
errors and then associated with the METHOD metaobject. The value can
then be accessed by calling the appropriate accessor as shown in Table
4.

This section begins with a description of the error checking and
processing of each initialization argument. This is followed by a
table showing the generic functions that can be used to access the
stored initialization arguments. The section ends with a set of
restrictions on portable methods affecting method metaobject
initialization.

In these descriptions, the phrase ``this argument defaults to value''
means that when that initialization argument is not supplied,
initialization is performed as if value had been supplied. For some
initialization arguments this could be done by the use of default
initialization arguments, but whether it is done this way is not
specified. Implementations are free to define default initialization
arguments for specified method metaobject classes. Portable programs
are free to define default initialization arguments for portable
subclasses of the class method.

    The :QUALIFIERS argument is a list of method qualifiers. An error
    is signaled if this value is not a proper list, or if any element
    of the list is not a non-null atom. This argument defaults to the
    empty list.

    The :LAMBDA-LIST argument is the unspecialized lambda list of the
    method. An error is signaled if this value is not a proper lambda
    list. If this value is not supplied, an error is signaled.

    The :SPECIALIZERS argument is a list of the specializer
    metaobjects for the METHOD. An error is signaled if this value is
    not a proper list, or if the length of the list differs from the
    number of required arguments in the :LAMBDA-LIST argument, or if
    any element of the list is not a specializer metaobject. If this
    value is not supplied, an error is signaled.

    The :FUNCTION argument is a method function. It must be compatible
    with the methods on COMPUTE-EFFECTIVE-METHOD defined for this
    class of method and generic function with which it will be
    used. That is, it must accept the same number of arguments as all
    uses of CALL-METHOD that will call it supply. (See
    COMPUTE-EFFECTIVE-METHOD for more information.) An error is
    signaled if this argument is not supplied.

    When the METHOD being initialized is an instance of a subclass of
    STANDARD-ACCESSOR-METHOD, the :SLOT-DEFINITION initialization
    argument must be provided. Its value is the direct slot definition
    metaobject which defines this accessor method. An error is
    signaled if the value is not an instance of a subclass of
    DIRECT-SLOT-DEFINITION.

    The :documentation argument is a string or NIL. An error is
    signaled if this value is not a string or NIL. This argument
    defaults to NIL.

After the processing and defaulting of initialization arguments
described above, the value of each initialization argument is
associated with the method metaobject. These values can then be
accessed by calling the corresponding generic function. The
correspondences are as follows:"""
        method.qualifiers = _defaulted(qualifiers, [],
                                       type = (list_, (and_, symbol, (not_, (eql_, nil)))))
        if not _specifiedp(lambda_list):
                error("SHARED-INITIALIZE STANDARD-METHOD: :LAMBDA-LIST must be supplied.")
        # Unregistered Issue COMPLIANCE-STANDARD-METHOD-SHARED-INITIALIZE-LAMBDA-LIST-VALIDATION-NOT-IMPLEMENTED
        method.lambda_list = lambda_list
        if not _specifiedp(specializers):
                error("SHARED-INITIALIZE STANDARD-METHOD: :SPECIALIZERS must be supplied.")
        # Unregistered Issue COMPLIANCE-STANDARD-METHOD-SHARED-INITIALIZE-SPECIALIZER-VALIDATION-NOT-IMPLEMENTED
        #  o  (list_, method_specializer)
        #  o  length == len(lambda_list[0])
        method.specializers = specializers
        if not _specifiedp(function):
                error("SHARED-INITIALIZE STANDARD-METHOD: :FUNCTION must be supplied.")
        method.function = function
        # Unregistered Issue COMPLIANCE-STANDARD-METHOD-SHARED-INITIALIZE-SLOT-DEFINITION-OPTION-NOT-IMPLEMENTED
        ## Later:
        # if typep(method, standard_accessor_method):
        #         if not _specifiedp(slot_definition):
        #                 error("SHARED-INITIALIZE STANDARD-METHOD: :SLOT-DEFINITION must be supplied.")
        #         if not typep(slot_definition, direct_slot_definition):
        #                 error("SHARED-INITIALIZE STANDARD-METHOD: the supplied value of :SLOT-DEFINITION must be an instance of a subclass of DIRECT-SLOT-DEFINITION.")
        method.documentation = _defaulted(documentation, nil,
                                          type = (or_, string_, (eql_, nil)))
        return method

def method_qualifiers(x):       return x.qualifiers
def method_lambda_list(x):      return x.lambda_list
def method_specializers(x):     return x.specializers
def method_function(x):         return x
def method_slot_definition(x):  return x.__slot_definition__
def method_documentation(x):    return x.__doc__

def method_generic_function(x): return x.__generic_function__

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
        del generic_function.__methods__[method.specializers]
        method.__generic_function__ = nil
        for s in method_specializers(method):
                remove_direct_method(s, method)
        _update_generic_function_and_dependents(generic_function, remove_method = method)
        return generic_function

def remove_direct_method(specializer, method):
        """remove-direct-method specializer method

Arguments:

The specializer argument is a specializer metaobject.

The method argument is a method metaobject.

Values:

The value returned by remove-direct-method is unspecified.

Purpose:

This generic function is called to maintain a set of backpointers from
a SPECIALIZER to the set of methods specialized to it. If METHOD is in
the set it is removed. If it is not, no error is signaled.

This set can be accessed as a list by calling the generic function
SPECIALIZER-DIRECT-METHODS. Methods are added to the set by
ADD-DIRECT-METHOD.

The generic function REMOVE-DIRECT-METHOD is called by REMOVE-METHOD
whenever a method is removed from a generic function. It is called
once for each of the specializers of the method. Note that in cases
where a specializer appears more than once in the specializers of a
method, this generic function will be called more than once with the
same specializer as argument.

The results are undefined if the specializer argument is not one of
the specializers of the method argument."""
        _not_implemented()

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
        # Issue GLOBALS-SPECIFIED-TO-REFER-TO-THE-CONTAINING-MODULE-NOT-THE-CALLING-ONE
        generic_function, definedp = gethash(fn.__name__, _builtins.globals())
        fixed, optional, args, keyword, keys = lambda_list = _function_lambda_list(fn)
        if not definedp:
                generic_function = ensure_generic_function(fn.__name__,
                                                           lambda_list = lambda_list,
                                                           # the rest is defaulted
                                                           )
        method_class = generic_function_method_class(generic_function)
        methfun_lambda, methfun_args = make_method_lambda(generic_function,
                                                          class_prototype(method_class),
                                                          fn, ___env___)
        method = make_instance(
                method_class,
                qualifiers = [], # XXX
                lambda_list = lambda_list,
                specializers = _tuple(_make_method_specializers(
                                      mapcar(lambda name: gethash(name, method.__annotations__, t)[0],
                                             fixed))),
                function = _not_implemented("somehow compile", methfun_lambda)
                **methfun_args)
        add_method(generic_function, method)
        return method

def _make_method_specializers(specializers):
        def parse(name):
                return (# name                                                    if specializerp(name) else
                        name                                                      if name is t          else
                                                                  # ..special-case, since T isn't a type..
                        name                                                      if typep(name, _type) else
                                                                  # Was: ((symbolp name) `(find-class ',name))
                        ecase(car(name),
                              (eql_,      lambda: intern_eql_specializer(name[1])),
                              (class_eq_, lambda: class_eq_specializer(name[1]))) if _tuplep(name)      else
                        ## Was: FIXME: Document CLASS-EQ specializers.
                        error("%s is not a valid parameter specializer name.", name))
        return mapcar(parse, specializers)

###
### Init
###
_init_package_system_2()
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
