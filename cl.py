
# Builtins management

def _python_builtins_dictionary():
        import builtins    as _builtins
        return _builtins.getattr(__builtins__, "__dict__", __builtins__)

import collections as _collections

class _dictator(_collections.UserDict):
        def __hasattr__(self, name): return name in self.data
        def __getattr__(self, name): return self.data[name]
        def __setitem__(self, *_):   raise  self.data["Exception"]("Dictator.")
        def __delitem__(self, *_):   raise  self.data["Exception"]("Dictator.")
        def __setattr__(self, *_):   raise  self.data["Exception"]("Dictator.")
        def __init__(self, dict):
                self.__dict__.update(data = dict)

_py = _dictator(_python_builtins_dictionary())

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
        ## Unfortunately, the effect is global, and, seemingly,
        ## cannot be constrained to a single module.
        for name in _python_builtin_names:
                del _python_builtins_dictionary()[name]

### What's supposed to be left (but, actually, much more is):
## o  None, True, False -- notably are not AST-level, but still are invincible.
## o  __(build_class | debug | doc | import | name | package)__
## o  _python_builtins_dictionary, _python_builtin_names
## o  _python

# Imports

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
import hashlib     as _hashlib
import inspect     as _inspect
import operator    as _operator
import platform    as _platform
import functools   as _functools
import linecache   as _linecache
import threading   as _threading
import collections as _collections

import neutrality  as _neutrality
import frost       as _frost

# Unspecific Wave 0

def _defaulted(x, value, type = None):
        if x is not None and type is not None:
                check_type(x, type) # Not a macro, so cannot access the actual defaulted name..
        return x if x is not None else value

def _defaulted_to_var(x, variable, type = None):
        return x if x is not None else _defaulted(x, _symbol_value(variable), type = type)

def _specifiedp(x):
        return x is not None

def _only_specified_keys(**keys):
        return _py.dict(((k, v) for k, v in keys.items()
                        if _specifiedp(k)))

def _defaulted_keys(**keys):
        return _py.dict((key, (default if value is None else value))
                        for key, (value, default) in keys.items())

# Boot messaging

def _fprintf(stream, format_control, *format_args):
        try:
                _neutrality._write_string(format_control % format_args, stream)
        except _py.UnicodeEncodeError:
                _neutrality._write_string((format_control % format_args).encode("utf-8"), stream)

def _debug_printf(format_control, *format_args):
        _fprintf(_sys.stderr, format_control + "\n", *format_args)

def _debug_printf_if(condition, format_control, *format_args):
        if condition:
                _debug_printf(format_control, *format_args)

# First-class namespaces

class _namespace(_collections.UserDict):
        def __str__(self):
                return "#<NAMESPACE %s>" % (_py.repr(self.name),)
        def __init__(self, name, data_constructor = _py.dict):
                self.name, self.data, self.properties = name, data_constructor(), _collections.defaultdict(_py.dict)
        def __getitem__(self, x):               return self.data.__getitem__(x)
        def __hasitem__(self, x):               return self.data.__hasitem__(x)
        def names(self):                        return _py.set(self.data.keys())
        def intersect(self, with_):             return [x for x in with_ if x in self.data] if _py.len(self) > _py.len(with_) else [x for x in self.data if x in with_]
        def has(self, name):                    return name in self.data
        def get(self, name):                    return self.data[name]
        def access(self, name, default = None): return (default, None) if name not in self.data else (self.data[name], True)
        def set(self, value, name):             self.data[name] = value; return value
        def grow(self, name, **keys):           self.data[name] = _namespace_type_and_constructor(name, **keys); self.setf_property(True, name, "NAMESPACEP")
        def properties(self, name):             return self.properties[name]
        def has_property(self, name, pname):    return pname in self.properties[name]
        def property(self, name, pname, default = None):
                cell = self.properties[name]
                return cell[pname] if pname in cell else default
        def setf_property(self, value, name, pname):
                self.properties[name] = value
                return value
_namespace_type_and_constructor = _namespace
_namespace = _namespace_type_and_constructor("")

# Meta-boot

## 1. trivial enumeration for later DEFUN/DEFCLASS
__boot_defunned__, __boot_defclassed__ = _py.set(),  _py.set()
def boot_defun(fn):     __boot_defunned__.add(fn);    return fn
def boot_defclass(cls): __boot_defclassed__.add(cls); return cls

## 2. tagged switchables
_namespace.grow("boot", data_constructor = lambda: _collections.defaultdict(_py.set))

def boot(set, boot, on_unboot = None):
        def definer(orig):
                def unboot():
                        _frost.setf_global(orig, orig.__name__, _py.globals())
                        if on_unboot:
                                on_unboot()
                def linkage(*args, **keys):
                        return boot(orig, *args, **keys)
                boot.unboot = unboot
                boot.name = orig.__name__
                _namespace["boot"][set].add(boot)
                return linkage
        return definer

def _unboot_set(set):
        for x in _py.sorted(_namespace["boot"][set], key = lambda x: x.name):
                if not _py.hasattr(x, "unboot"):
                        error("In UNBOOT-SET \"%s\": %s has no 'unboot' attribute.", set, x)
                x.unboot()
        del _namespace["boot"][set]
        _debug_printf("; unbooted function set %s, remaining boot sets: %s", _py.repr(set), ", ".join(_namespace["boot"].keys()))

def _interpret_toplevel_value(name_or_obj, objness_predicate):
        def stringp(x): return _py.isinstance(x, _py.str)
        name, obj = ((name_or_obj.__name__, name_or_obj) if objness_predicate(name_or_obj)               else
                     (name_or_obj, None)                 if stringp(name_or_obj) or symbolp(name_or_obj) else
                     error("Bad cold object definition: %s", name_or_obj))
        ####### Thought paused here:
        # ..delay symbol computation!
        sym, pyname = ((_intern(_frost.python_name_lisp_symbol_name(name))[0], name)  if _py.isinstance(name, _py.str) else
                       (name, _frost.lisp_symbol_name_python_name(symbol_name(name))) if symbolp(name)                 else
                       error("In cold definition of %s: bad name %s for a cold object.", name, _py.repr(name)))
        return obj, sym, pyname

class _storyteller(_collections.UserDict):
        def __init__(self):           self.__dict__.update(_py.dict(__call__ = lambda self, x: self.advance(x),
                                                                    data     = _py.dict()))
        def __setattr__(self, _, __): raise _py.Exception("\n; The Storyteller defies this intercession.")
        def advance(self, x):
                self.data[x if _py.isinstance(x, _py.str) else
                          x.__name__] = True
                return x
        def narrated(self, x):        return x in self.data
        def call(self, x, control, *args, hard = False):
                if x in self.data:
                        return True
                if hard:
                        raise _py.Exception(("\n; The Storyteller quietly said: 'twas too early to mention \"%s\" " % x) + control % args)
                else:
                        warn(("too early to mention \"%s\" " % (x,)) + control, *args)
        def conclude(self):
                _debug_printf("; The Storyteller proclaimed a conclusion, which also was a new beginning.")
                self.__dict__.update(_py.dict(__call__ = lambda *_, **__: True))
_storyteller = _storyteller()
story = _storyteller.advance

# Cold types

_cold_class_type       = _py.type
_cold_condition_type   = _py.BaseException
_cold_error_type       = _py.Exception
_cold_hash_table_type  = _py.dict
_cold_stream_type      = __io._IOBase
_cold_file_stream_type = __io.TextIOWrapper
_cold_function_type    = _types.FunctionType.__mro__[0]
_cold_tuple_type       = _py.tuple
_cold_string_type      = _py.str
_cold_list_type        = _py.list
def _cold_simple_error(format, *args): raise _cold_error_type(format % args)
def _cold_typep(x, type):
        return _py.isinstance(x, (type             if _py.isinstance(x, _py.type)             else
                                  type.python_type if _py.isinstance(x, (symbol if _py.isinstance(x, _py.type) else
                                                                         symbol.python_type)) else
                                  _cold_simple_error("%s is neither a python type, nor a symbol.",
                                                     x.__repr__())))
def _cold_the(type, x):
        if typep(x, type):
                return x
        else:
                raise _cold_simple_error("%s is not a %s.", x.__repr__(), type)
def _cold_check_type(x, type):
        the(type, x)
typep      = _cold_typep
the        = _cold_the
check_type = _cold_check_type

# As-of-yet -homeless type predicates..

@boot_defun
def stringp(x):        return _py.isinstance(x, _cold_string_type)
@boot("symbol", lambda _, o: (_py.isinstance(o, _cold_function_type) or
                              _py.isinstance(o, symbol) and o.function))
@boot_defun ## Unregistered Issue COMPLIANCE-EVALUATION-MODEL-FUNCTIONP
def functionp(o):      return (_py.isinstance(o, _cold_function_type) or
                               _py.isinstance(o, symbol.python_type) and o.function)

def _symbol_type_specifier_p(x):
        return _py.hasattr(x, "python_type")

def _python_type_p(x): return _py.isinstance(o, _cold_class_type)

@boot_defun
def type_of(x):
        return _py.type(x)

# Boot listery and consery (beware model issues)

def _tuplep(x):
        return _py.isinstance(x, _cold_tuple_type)

@boot_defun
def atom(x):        return not _tuplep(x) or x == ()
@boot_defun # Unregistered Issue LIST-CONSNESS
def consp(x):       return _tuplep(x) and x != ()
@boot_defun # Unregistered Issue LIST-CONSNESS
def listp(x):       return x is nil or _tuplep(x) ## covers x == ()
@boot_defun # Unregistered Issue LIST-CONSNESS
def list(*xs):      return xs
@boot_defun # Unregistered Issue LIST-CONSNESS
def append(*xs):    return _py.sum(xs, xs[0].__class__()) or _py.tuple()
# def append(*xs): return reduce(lambda x, y: x + y, xs) if (xs and xs[0]) else []
@boot_defun
def cons(car, cdr): return (x, y)
@boot_defun
def car(x):         return x[0] if x else nil
@boot_defun
def first(x):       return x[0] if x else nil

# Unspecific Wave 1

@boot_defun
def identity(x):  return x

@boot_defun
def make_hash_table(default_constructor = None):
        return (_collections.defaultdict(default_constructor) if default_constructor else
                _py.dict())

@boot_defun
def gethash(key, dict, default = None):
        therep = key in dict
        return (dict[key] if therep else default), therep

@boot_defun
def length(x):
        return _py.len(the(string, x))

def _map_into_hash(f, xs,
                   key_test = lambda k: k is not None,
                   value_test = lambda _: True) -> _py.dict:
        acc = _py.dict()
        for x in xs:
                k, v = f(x)
                if key_test(k) and value_test(v):
                        acc[k] = v
        return acc

# Boot dynamic scope

__global_scope__ = make_hash_table()

class _thread_local_storage(_threading.local):
        def __init__(self):
                self.dynamic_scope = []

__tls__ = _thread_local_storage()

# The symmetry invariance is _IMPORTANT_, as you probably can imagine!
def _dynamic_scope_push(scope):
        __tls__.dynamic_scope.append(scope)
def _dynamic_scope_pop():
        __tls__.dynamic_scope.pop()

def _find_dynamic_frame(name):
        for scope in reversed(__tls__.dynamic_scope):
                if name in scope:
                        return scope
        if name in __global_scope__:
                return __global_scope__

def _dynamic_frame_for_set(name, force_toplevel = None):
        return (__global_scope__ if force_toplevel else
                (_find_dynamic_frame(name) or
                 (__tls__.dynamic_scope[-1] if __tls__.dynamic_scope else
                  __global_scope__)))

def _symbol_value(name):
        frame = _find_dynamic_frame(name)
        return (frame[name] if frame else
                error(AttributeError, "Unbound variable: %s." % name))

def _do_bind_symbol_pyname(symbol, globals):
        pyname = _frost.lisp_symbol_name_python_name(symbol_name(symbol))
        _frost.setf_global(symbol, pyname, globals)

def _bind_symbol_pyname(symbol_, globals = None):
        _do_bind_symbol_pyname(_boot_check_type(symbolp, symbol_), globals = _defaulted(globals, _py.globals()))

def _intern_and_bind_pynames(*names, globals = None):
        globals = _defaulted(globals, _py.globals())
        for name in names:
                _bind_symbol_pyname(_intern(name)[0], globals)

def _boot_symbolicate_global_dynamic_scope():
        def upgrade_scope(xs):
                kvs = _py.list(xs.items())
                for k, v in kvs:
                        del xs[k]
                        sym = _intern_in_package(k, __cl)[0]
                        xs[sym] = v
                        _do_bind_symbol_pyname(sym, _py.globals())
        assert not __tls__.dynamic_scope
        upgrade_scope(__global_scope__)

def _do_set(name, value, force_toplevel):
        _dynamic_frame_for_set(name, force_toplevel = force_toplevel)[name] = value

@boot("symbol",
      lambda _string_set, name, value, force_toplevel = None:
      _string_set(name, value, force_toplevel = force_toplevel, symbolicp = False),
      on_unboot = _boot_symbolicate_global_dynamic_scope)
def _string_set(symbol_name, value, force_toplevel = None, symbolicp = True):
        stringp(symbol_name) or error("The first argument to %%STRING-SET must be a string, was: %s.", symbol_name.__repr__())
        name = _intern(symbol_name)[0] if symbolicp else symbol_name
        _do_set(name, value, force_toplevel)
        symbolicp and _bind_symbol_pyname(name)
        return value

@boot("typep", lambda _, __, ___: error("A violent faecal odour hung in the air.."))
@boot_defun
def set(symbol_, value, *_, force_toplevel = False):
        _do_set(the(symbol, symbol_), value, force_toplevel)
        return value

@boot("symbol", lambda _, name: _find_dynamic_frame(_boot_check_type(stringp, name)) and t)
@boot_defun
def boundp(symbol_):
        # Unregistered Issue COMPLIANCE-BOUNDP-ACCEPTS-STRINGS
        return _find_dynamic_frame(the(symbol, symbol_)) and t

# Boot conditions: WARN, ERROR

def _conditionp(x):
        return _py.isinstance(x, _cold_condition_type)

@boot("typep", lambda _, datum, *args, default_type = None, **keys:
              _py.Exception(datum % args) if stringp(datum) else
              (datum if not (args or keys) else
               error("Bad, bad evil is rising.  Now go and kill everybody.")) if _conditionp(datum) else
              datum(*args, **keys))
def _coerce_to_condition(datum, *args, default_type = None, **keys):
        type_specifier = _defaulted(default_type, error) if stringp(datum) else datum

        type = (type_specifier             if typep(type_specifier, _cold_class_type)                              else
                None                       if _conditionp(type_specifier)                                          else
                type_specifier.python_type if symbolp(type_specifier) and _symbol_type_specifier_p(type_specifier) else
                error(simple_type_error, "Cannot coerce %s to a condition.", _py.repr(datum)))
        cond = (datum              if type is None   else # Already a condition.
                type(datum % args) if stringp(datum) else
                type(*args, **keys))
        return cond

@boot("typep", lambda _, datum, *args, **keys:
              _debug_printf("COLD WARNING: " + datum, *args, **keys))
@boot_defun
def warn(control, *args, **keys):
        condition = _coerce_to_condition(control, *args, **keys)
        check_type(condition, warning)
        signal(condition)
        badness = _poor_man_etypecase(condition,
                                      (style_warning, style_warning),
                                      (warning,       warning))
        format(_symbol_value(_error_output_), "%s: %s\n", symbol_name(badness), condition)
        return nil

# @boot(lambda error, datum, *args, **keys: _frost.raise_exception(_coerce_to_condition(datum, *args, **keys)))
@boot_defun
def error(datum, *args, **keys):
        ## Shouldn't we ditch Python compat entirely, doing instead
        ## the typical SIGNAL/INVOKE-DEBUGGER thing?
        raise _coerce_to_condition(datum, *args, **keys)

def _boot_check_type(pred, x):
        return x if pred(x) else error("A violent faecal odour hung in the air..")

# Package system conditions

def _package_not_found_error(x):
        error("The name \"%s\" does not designate any package.", x)

def _symbol_conflict_error(op, obj, pkg, x, y):
        error(simple_package_error, "%s %s causes name-conflicts in %s between the following symbols: %s, %s." %
              (op, obj, pkg, x, y))

def _symbols_not_accessible_error(package, syms):
        def pp_sym_or_string(x):
                return "\"%s\"" % x if stringp(x) else _print_nonkeyword_symbol(x)
        error(simple_package_error, "These symbols are not accessible in the %s package: (%s).",
              package_name(package), ", ".join(mapcar(pp_sym_or_string, syms)))

# Package system classes

_namespace.grow("PACKAGES")

@boot_defclass
class package(_collections.UserDict):
        def __repr__ (self): return "#<PACKAGE \"%s\">" % self.name # Cold PRINT-UNREADABLE-OBJECT
        def __bool__(self):  return True                            # Non-false even if empty.
        def __hash__(self):  return hash(_py.id(self))
        def __init__(self, name, use = [], nicknames = [], internal_symbols = 10, external_symbols = 10,
                     filename = "", ignore_python = False, python_exports = True, boot = False):
                internal_symbols = external_symbols = "IGNORE"
                ## DEPENDENCY: USE-PACKAGE
                ## DEPENDENCY: INTERN
                def validate_requested_package_names(name, nicknames):
                        # Unregistered Issue COMPLIANCE-PACKAGE-REDEFINITION
                        name = "IGNORE"
                        nickname_conflicts = _namespace["PACKAGES"].intersect(nicknames)
                        for n_c in nickname_conflicts:
                                p = _namespace["PACKAGES"][n_c]
                                if p.name == n_c: error("\"%s\" is a package name, so it cannot be a nickname for \"%s\".", n_c, name)
                                else:             error("\"%s\" is already a nickname for \"%s\".", n_c, p.name)
                def setup_package_usage(p, used):
                        ## Issue _CCOERCE_TO_PACKAGE-WEIRD-DOUBLE-UNDERSCORE-NAMING-BUG
                        # coercer = (_ccoerce_to_package if boot else
                        #            _coerce_to_package)
                        p.used_packages  = _py.set(find_package(x) or _package_not_found_error(x)
                                                   for x in used)
                        p.packages_using = _py.set()
                        if p.used_packages and _storyteller.call("use_package", "using %s into %s", used, p):
                                for u_p in p.used_packages:
                                        assert _py.isinstance(u_p, _py.type(p))
                                        use_package(p, u_p)
                ## __init__()
                assert stringp(name)
                self.name = name
                self.nicknames = nicknames

                validate_requested_package_names(name, nicknames)

                self.own         = _py.set()                         # sym
                self.imported    = _py.set()                         # sym
              # self.present     = own + imported
                self.inherited   = _collections.defaultdict(_py.set) # sym -> _py.set(pkg) ## _mapsetn(_slotting("external"), used_packages) -> source_package
                self.accessible  = make_hash_table()                 # str -> sym          ## accessible = present + inherited
                self.external    = _py.set()                         # sym                 ## subset of accessible
              # self.internal    = accessible - external

                setup_package_usage(self, use)

                ## Hit the street.
                self.data          = self.accessible
                _namespace["PACKAGES"].set(self, name)
                for nick in nicknames:
                        _namespace["PACKAGES"].set(self, nick)

@boot("symbol", lambda _, name, **keys: package(name, **keys))
@boot_defun
def make_package(name, **keys):
        return package.python_type(name, **keys)

@boot("symbol", lambda _, x: _py.isinstance(x, package))
@boot_defun
def packagep(x): return _py.isinstance(x, package.python_type)

@boot_defun
def package_name(x): return x.name

@boot_defun
def find_package(name):
        return (name if packagep(name) else
                _namespace["PACKAGES"].access(string(name))[0] or nil)

@boot_defun
def package_used_by_list(package):
        p = _coerce_to_package(package)
        return p.packages_using if p else _package_not_found_error(package)

@boot_defclass
class symbol(): # Turned to a symbol, during the package system bootstrap.
        def __str__(self):
                return _print_symbol(self)
        def __repr__(self):
                return _py.str(self)
        def __init__(self, name):
                (self.name, self.package,
                 (self.function,
                  self.macro_function,
                  self.compiler_macro_function,
                  self.symbol_macro_expansion,
                  self.known)) = name, None, (None, nil, nil, None, nil)
        def __hash__(self):
                return hash(self.name) ^ (hash(self.package.name) if self.package else 0)
        def __call__(self, *args, **keys):
                function = self.function ## Well, it's an inlined SYMBOL-FUNCTION..
                if not _py.isinstance(function, _cold_function_type):
                        error("While calling %s: SYMBOL-FUNCTION returned a non-callable object of type %s.",
                              self, _py.type(function))
                return function(*args, **keys)
        def __bool__(self):
                return self is not nil

@boot("symbol", lambda _, name, **keys: symbol(name))
@boot_defun
def make_symbol(name, **keys):
        return symbol.python_type(name, **keys)

@boot("symbol", lambda _, x: _py.isinstance(x, symbol))
@boot_defun
def symbolp(x):  return _py.isinstance(x, symbol.python_type)

@boot_defun
def keywordp(x): return symbolp(x) and symbol_package(x) is __keyword

@boot_defun
def symbol_name(x):            return x.name
@boot_defun
def symbol_package(x):         return x.package
@boot_defun # Unregistered Issue COMPLIANCE-SYMBOL-VALUE
def symbol_value(symbol_):     return _symbol_value(the(symbol, symbol_))
def _symbol_function(symbol):  return (symbol.known          or
                                       symbol.macro_function or
                                       symbol.function       or
                                       _debug_printf("no fun: %s", symbol) or
                                       error(undefined_function, symbol))

@boot_defun
def string(x):
        ## NOTE: These type check branches can be in bootstrap order or in usage frequency order!
        return (x      if stringp(x) else
                x.name if symbolp(x) else
                error("%s must have been either a string or a symbol.", x))

def _do_find_symbol(str, package):
        return gethash(str, package.accessible, None)[0]

def _find_symbol_or_fail(x, package = None):
        sym = _do_find_symbol(x, _coerce_to_package(package))
        return (sym if sym is not None else
                _symbols_not_accessible_error(p, [x]))

def _symbol_relation(x, p):
        "NOTE: here we trust that X belongs to P, when it's a symbol."
        s = gethash(x, p.accessible, None)[0] if stringp(x) else x
        if s is not None:
                return _keyword("INHERITED" if s.name in p.inherited else
                                "EXTERNAL"  if s      in p.external  else
                                "INTERNAL")

def _find_symbol(str, package):
        s = _do_find_symbol(str, package)
        return ((s, _symbol_relation(s, package)) if s is not None else
                (None, None))

def _symbol_accessible_in(x, package):
        return (x.name in package.accessible and
                package.accessible[x.name] is x)

@boot_defun
def find_symbol(str, package = None):
        return _find_symbol(str, _coerce_to_package(package))

@boot("print", lambda _, s, **__:
              (("#"            if not s.package                               else
                ""             if s.package is __keyword or s.package is __cl else
                s.package.name) + (""  if s.package is __cl                                                         else
                                   ":" if (not s.package or s.name in s.package.external or s.package is __keyword) else
                                   "::") + s.name))
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
        package  = _defaulted_to_var(package,  _package_)
        if not packagep(package):
                _here("------------------------------------------------------------\npackage is a %s: %s" % (type_of(package), package,))
        readably = _defaulted_to_var(readably, _print_readably_)
        escape   = _defaulted_to_var(escape,   _print_escape_) if not readably else t
        case     = _defaulted_to_var(case,     _print_case_)   if not readably else _keyword("UPCASE")
        gensym   = _defaulted_to_var(gensym,   _print_gensym_) if not readably else t
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
                 ":"                      if s.package is __keyword            else
                 ""                       if _symbol_accessible_in(s, package) else
                 ("#:" if gensym else "") if not s.package                     else
                 (s.package.name + (":"
                                    if s in s.package.external else
                                    "::"))) +
                _case_xform(case, s.name))

def _core_package_init():
        global __cl, __keyword
        __cl      = make_package("COMMON-LISP", nicknames = ["CL"])
        __keyword = make_package("KEYWORD")

_core_package_init()

def _do_intern_symbol(s, p):
        p.own.add(s)
        p.accessible[s.name], s.package = s, p
        if p is __keyword: # CLHS 11.1.2.3.1 Interning a Symbol in the KEYWORD Package
                p.external.add(s)
        return s

def _cold_make_nil():
        nil = symbol.__new__(symbol)
        (nil.name,
         nil.package,
         nil.function,
         nil.macro_function,
         nil.compiler_macro_function,
         nil.symbol_macro_expansion,
         nil.known) = "NIL", __cl, nil, nil, nil, None, nil
        return _do_intern_symbol(nil, __cl)

NIL = nil = _cold_make_nil()

@boot_defun
def null(x): return x is nil or x == ()

# Package system core

def _intern_in_package(x, p):
        s, presentp = (error("X must be a string: %s.", _py.repr(x)) if not _py.isinstance(x, _py.str) else
                       (p.accessible.get(x), True)                   if x in p.accessible              else
                       (None,                False))
        if not presentp:
                s = _do_intern_symbol(make_symbol(x), p)
        return s, presentp

def _coerce_to_package(x, if_null = "current"):
        return (find_package(x)                                              if stringp(x) or symbolp(x) or packagep(x) else
                (_symbol_value(_package_) if if_null == "current" else
                 _package_not_found_error(x))                                if (not x)                                 else
                error(simple_type_error, "COERCE-TO-PACKAGE accepts only package designators -- packages, strings or symbols, was given '%s' of type %s.",
                      x, type_of(x)))

@boot("symbol", lambda _intern, x, package = None:
              _intern(x, package or __cl))
def _intern(x, package = None):
        "A version of INTERN, that does not compute the relationship between SYMBOL and designated PACKAGE."
        return _intern_in_package(x, find_package(package) if package else
                                     _symbol_value(_package_))

def _keyword(s, upcase = True):
        return _intern((s.upper() if upcase else s),
                       __keyword)[0]

def _use_package_symbols(dest, src, syms):
        conflict_set = { x.name for x in syms.values() } & _py.set(dest.accessible.keys())
        for name in conflict_set:
                if syms[name] is not dest.accessible[name]:
                        _symbol_conflict_error("USE-PACKAGE", src, dest, syms[name], dest.accessible[name])
        ## no conflicts anymore? go on..
        for name, sym in syms.items():
                dest.inherited[sym].add(src)
                if name not in dest.accessible: # Addition of this conditional is important for package use loops.
                        dest.accessible[name] = sym
                        # if dest.name == "SWANK" and src.name == "INSPECTOR":
                        #         debug_printf("merging %s into %s: test: %s", s, dest, _read_symbol(_print_nonkeyword_symbol(s)))

@story
@boot_defun
def use_package(dest, src):
        dest, src = _coerce_to_package(dest), _coerce_to_package(src)
        symhash = _map_into_hash(lambda x: (x.name, x), src.external)
        _use_package_symbols(dest, src, symhash)
        src.packages_using.add(dest)
        dest.used_packages.add(src)

@boot_defun
def intern(x, package = None):
        package = _coerce_to_package(package)
        s, found_in_package = _intern(x, package)
        return s, (_symbol_relation(s, package) if found_in_package else
                   None)

@boot_defun
def defpackage(name, use = [], export = []):
        p = make_package(name, use = use)
        for symname in export:
                _not_implemented("DEFPACKAGE: :EXPORT keyword") # XXX: populate the for-INTERN-time-export set of names
        return p

@boot_defun
def in_package(name):
        _string_set("*PACKAGE*", _coerce_to_package(name), force_toplevel = t)

@boot_defun
def export(symbols, package = None):
        symbols, package = symbols if _py.isinstance(symbols, _py.list) else [symbols], _coerce_to_package(package)
        assert(all(symbolp(x)
                   for x in symbols))
        symdict = _map_into_hash(lambda x: (x.name, x), symbols)
        for user in package.packages_using:
                _use_package_symbols(user, package, symdict)
        # No conflicts?  Alright, we can proceed..
        symset = _py.set(symdict.values())
        for_interning = symset & _py.set(package.inherited)
        for sym in for_interning:
                del package.inherited[sym]
                self.internal.add(sym)
        package.external |= symset
        return True

@boot_defun
def import_(symbols, package = None, populate_module = True):
        p = _coerce_to_package(package)
        symbols = _ensure_list(symbols)
        module = _find_module(_frost.lisp_symbol_name_python_name(package_name(p)),
                              if_does_not_exist = "continue")
        for s in symbols:
                ps, accessible = gethash(s.name, p.accessible)
                if ps is s:
                        continue
                elif accessible: # conflict
                        _symbol_conflict_error("IMPORT", s, p, s, ps)
                else:
                        p.imported.add(s)
                        p.accessible[s.name] = s
                        if module:
                                _not_implemented("Namespace merging.")
                                # Issue SYMBOL-VALUES-NOT-SYNCHRONISED-WITH-PYTHON-MODULES
                                # python_name = _frost.lisp_symbol_name_python_name(s.name)
                                # module.__dict__[python_name] = ???
        return t

# Package system init

def _init_package_system_0():
        global __packages__
        global __keyword
        global t, T, symbol, make_symbol, make_package
        __core_symbol_names__ = [
                "QUOTE",
                "ABORT", ("CONTINUE", "continue_"), ("BREAK", "break_"),
                "&OPTIONAL", "&REST", "&KEY", "&BODY", "&ALLOW-OTHER-KEYS", "&WHOLE",
                "&RESTKEY", # pythonism
                ]
        __more_symbol_names__ = [
                "SOME", "EVERY", "LOCALLY", "MACROLET", "SYMBOL_MACROLET"
                ]
        __packages__ = make_hash_table()
        T = t              = _intern("T", __cl)[0]     # Nothing much works without this.
        nil.__contains__   = lambda _: False
        nil.__getitem__    = lambda _, __: nil
        nil.__length__     = lambda _: 0
        nil.__iter__       = lambda _: None
        nil.__reversed__   = lambda _: None
        export([t, nil] + [_intern(n[0] if _tuplep(n) else n, __cl)[0]
                           for n in __core_symbol_names__ + __more_symbol_names__],
               __cl)
        for spec in __core_symbol_names__ + __more_symbol_names__:
                lisp_name, python_name = (spec, _frost.lisp_symbol_name_python_name(spec)) if stringp(spec) else spec
                _frost.setf_global(_find_symbol_or_fail(lisp_name, __cl), python_name, _py.globals())
                # Unregistered Issue PACKAGE-SYSTEM-INIT-SHOULD-USE-GLOBAL-SETTER-INSTEAD-OF-CUSTOM-HACKERY
        # secondary
        global star
        star = _intern("*", __cl)[0]
        package("COMMON-LISP-USER", use = [__cl], boot = True)
        __global_scope__["*PACKAGE*"] = __cl # COLD-SETQ
        symbol = _frost.frost_def(symbol,  _intern("SYMBOL")[0],  "python_type", _py.globals())
        @boot_defun# (_do_intern_symbol(symbol.python_type("MAKE-SYMBOL"), __cl))
        def make_symbol(name):
                return symbol.python_type(name)
        _frost.frost_def(package, _intern("PACKAGE")[0], "python_type", _py.globals())
        @boot_defun
        def make_package(name, nicknames = [], use = []):
                if nicknames:
                        _not_implemented("In MAKE-PACKAGE %s: package nicknames are ignored.", _py.repr(name))
                return package.python_type(string(name), ignore_python = True, use = [])

_init_package_system_0()

_unboot_set("symbol")
# _unboot_set("print") # This can turn 4.8s of debug printing into 30+s

# GENSYM

__gensym_counter__ = 0

def _gensymname(x = "N"):
        # Unregistered Issue GENSYM-NOT-THREAD-SAFE
        global __gensym_counter__
        __gensym_counter__ += 1
        return x + _py.str(__gensym_counter__)

@boot_defun
def gensym(x = "G"):
        return make_symbol(_gensymname(x))

# Dynamic scope

class _env_cluster(object):
        def __init__(self, cluster):
                self.cluster = cluster
        def __enter__(self):
                _dynamic_scope_push(self.cluster)
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
                error(AttributeError, "Use SET to set special globals.")

__dynamic_scope__ = _dynamic_scope()
env = __dynamic_scope__             # shortcut..

@boot_defun
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
                with _env_cluster(_py.dict(_py.zip(vars, vals))):
                        return body()
        else:
                return _env_cluster(vars if hash_table_p(vars) else
                                    cluster)

# CATCH, THROW, BLOCK, RETURN-FROM

@boot_defun
def unwind_protect(form, fn):
        "For the times, when statements won't do."
        try:
                return form()
        finally:
                fn()

# WARNING: non-specific try/except clauses and BaseException handlers break this!
class __catcher_throw__(_cold_condition_type):
        def __init__(self, ball, value, reenable_pytracer = nil):
                self.ball, self.value, self.reenable_pytracer = ball, value, reenable_pytracer
        def __str__(self):
                return "@<ball %s>" % (self.ball,)

def _do_catch(ball, body):
        "This seeks the stack like mad, like the real one."
        if not _py.isinstance(ball, symbol.python_type):
                error("In %DO-CATCH: first argument must be a symbol, was: %s.", _py.repr(ball))
        try:
                return body()
        except __catcher_throw__ as ct:
                # format(t, "catcher %s, ball %s -> %s", ct.ball, ball, "caught" if ct.ball is ball else "missed")
                if ct.ball is ball:
                        if ct.reenable_pytracer:
                                _frost.enable_pytracer()
                        return ct.value
                else:
                        raise
@boot_defun
def catch(ball, body):
        return _do_catch(ball, body)

@boot_defun
def _throw(ball, value):
        "Stack this seeks, like mad, like the real one."
        check_type(ball, symbol)
        raise __catcher_throw__(ball = ball, value = value, reenable_pytracer = boundp(_signalling_frame_))

def __block__(fn):
        "An easy decorator-styled interface for block establishment."
        nonce = gensym("BLOCK")
        ret = (lambda *args, **keys:
                       _do_catch(nonce,
                                 lambda: fn(*args, **keys)))
        _py.setattr(ret, "ball", nonce)
        return ret

@boot_defun
def block(nonce_or_fn, body = None):
        """A lexically-bound counterpart to CATCH/THROW.
Note, how, in this form, it is almost a synonym to CATCH/THROW -- the lexical aspect
of nonce-ing is to be handled manually."""
        if not body: # Assuming we were called as a decorator..
                return __block__(nonce_or_fn)
        else:
                return catch(nonce_or_fn, body)

@boot_defun
def return_from(nonce, value):
        nonce = ((_py.getattr(nonce, "ball", None) or
                  error("RETURN-FROM was handed a function %s, but it is not cooperating in the "
                        "__BLOCK__ nonce passing syntax.", nonce)) if functionp(nonce) else
                 ## This can mean either the @defun-ned function, or absent a function definition, the symbol itself.
                 (_py.getattr(nonce.function, "ball", nonce))      if symbolp(nonce)   else
                 nonce                                             if stringp(nonce)   else
                 error("In RETURN-FROM: nonce must either be a string, or a function designator;  was: %s.", _py.repr(nonce)))
        _throw(nonce, value)

# Condition system: SIGNAL

## standard globals:
_string_set("*DEBUGGER-HOOK*",         nil)
_string_set("*INVOKE-DEBUGGER-HOOK*",  nil)

## non-standard:
_string_set("*HANDLER-CLUSTERS*", [])
_string_set("*PRESIGNAL-HOOK*",   nil)
_string_set("*PREHANDLER-HOOK*",  nil)

def _set_condition_handler(fn):
        _frost.set_tracer_hook("exception", fn)

@boot_defun
def signal(cond):
        for cluster in reversed(_symbol_value(_handler_clusters_)):
                for type, handler in cluster:
                        if not stringp(type):
                                if typep(cond, type):
                                        hook = _symbol_value(_prehandler_hook_)
                                        if hook:
                                                frame = assoc("__frame__", cluster)
                                                assert(frame)
                                                hook(cond, frame, hook)
                                        handler(cond)
        return nil

def _run_hook(variable, condition):
        old_hook = symbol_value(variable)
        if old_hook:
                with progv({ variable: nil }):
                        old_hook(condition, old_hook)

# Stab at INVOKE-DEBUGGER

def _flush_standard_output_streams():
        _warn_not_implemented()

def _funcall_with_debug_io_syntax(function, *args, **keys):
        _warn_not_implemented()
        return function(*args, **keys)

_intern_and_bind_pynames("*DEBUG-CONDITION*", "*DEBUG-RESTARTS*", "*NESTED-DEBUG-CONDITION*")

def _show_restarts(restarts, stream):
        _warn_not_implemented()

def _invoke_debugger(condition):
        ## SBCL is being careful to not handle STEP-CONDITION here..
        with progv({_debug_condition_: condition,
                    _debug_restarts_: compute_restarts(condition),
                    _nested_debug_condition_: nil }):
                def error_handler_body(condition):
                        _string_set("*NESTED-DEBUG-CONDITION*", condition)
                        ndc_type = type_of(condition)
                        format(_symbol_value(_error_output_),
                               "\nA %s was caught when trying to print %s when "
                               "entering the debugger. Printing was aborted and the "
                               "%s was stored in %s.\n",
                               ndc_type, _debug_condition_, ndc_type, _nested_debug_condition_)
                        if typep(condition, cell_error):
                                format(_symbol_value(_error_output_),
                                       "\n(CELL-ERROR-NAME %s) = %s\n",
                                       _nested_debug_condition_, cell_error_name(condition))
                handler_case(lambda: _print_debugger_invocation_reason(condition,
                                                                       _symbol_value(_error_output_)),
                             (error, error_handler_body))
                try:
                        pass
                finally:
                        with progv({ _standard_output_: _symbol_value(_standard_output_),
                                     _error_output_:    _symbol_value(_debug_io_) }):
                                format(_symbol_value(_debug_io_), "\nType HELP for debugger help, or (VPCL:QUIT) to exit from VPCL.\n\n")
                                _show_restarts(_symbol_value(_debug_restarts_), _symbol_value(_debug_io_))
                                _internal_debug()

@boot_defun
def invoke_debugger(condition):
        "XXX: non-compliant: doesn't actually invoke the debugger."
        _run_hook(_invoke_debugger_hook_, condition)
        _run_hook(_debugger_hook_, condition)
        if not (packagep(_symbol_value(_package_)) and
                package_name(_symbol_value(_package_))):
                _string_set("*PACKAGE*", find_package("CL-USER"))
                format(_symbol_value(_error_output_),
                       "The value of %s was not an undeleted PACKAGE. It has been reset to %s.",
                       _package_, _symbol_value(_package_))
        _flush_standard_output_streams()
        return _funcall_with_debug_io_syntax(_invoke_debugger, condition)

# Type predicates

def integerp(o):      return _py.isinstance(o, _py.int)
def floatp(o):        return _py.isinstance(o, _py.float)
def complexp(o):      return _py.isinstance(o, _py.complex)
def numberp(o):       return _py.isinstance(o, (_py.int, _py.float, _py.complex))
def hash_table_p(o):  return _py.isinstance(o, _cold_hash_table_type)
def _listp(o):        return _py.isinstance(o, _cold_list_type)
def _boolp(o):        return _py.isinstance(o, _py.bool)
def sequencep(x):     return _py.getattr(_py.type(x), "__len__", None) is not None

# Types mappable to python

def _define_python_type_map(symbol_or_name, type):
        not (stringp(symbol_or_name) or symbolp(symbol_or_name)) and \
            error("In DEFINE-PYTHON-TYPE-MAP: first argument must be either a string or a symbol, was: %s.", _py.repr(symbol_or_name))
        not _py.isinstance(type, _py.type(_py.str)) and \
            error("In DEFINE-PYTHON-TYPE-MAP: second argument must be a Python type, was: %s.", _py.repr(type))
        symbol = (symbol_or_name if symbolp(symbol_or_name) else
                  _intern(symbol_or_name)[0])
        _frost.setf_global(symbol, _frost.lisp_symbol_name_python_name(symbol.name),
                           globals = _py.globals())
        symbol.python_type = type
        return symbol

_define_python_type_map("INTEGER",           _py.int)
_define_python_type_map("FLOAT",             _py.float)
_define_python_type_map("COMPLEX",           _py.complex)

_define_python_type_map("STRING",            _py.str)
_define_python_type_map("HASH-TABLE",        _cold_hash_table_type)

_define_python_type_map("FUNCTION",          _cold_function_type)

_define_python_type_map("STREAM",            _cold_stream_type)

_define_python_type_map("CLASS",             _py.type) # Ha.

_define_python_type_map("CONDITION",         _py.BaseException)
_define_python_type_map("ERROR",             _py.Exception)
_define_python_type_map("SERIOUS-CONDITION", _py.Exception)
_define_python_type_map("END-OF-FILE",       _py.EOFError)

## non-standard type names
_define_python_type_map("FILE-STREAM", _cold_file_stream_type)
_define_python_type_map("PYBOOL",      _py.bool)
_define_python_type_map("PYLIST",      _py.list)
_define_python_type_map("PYTUPLE",     _py.tuple)
_define_python_type_map("PYBYTES",     _py.bytes)
_define_python_type_map("PYBYTEARRAY", _py.bytearray)
_define_python_type_map("PYSET",       _py.set)
_define_python_type_map("PYFROZENSET", _py.frozenset)

# Complex type specifier machinery: %TYPE-MISMATCH, @DEFTYPE, TYPEP

def _type_specifier_complex_p(x):
        """Determines, whether a type specifier X constitutes a
complex type specifier."""
        return _tuplep(x)

def _invalid_type_specifier_error(x, complete_type = None):
        error(simple_type_error, "%s is not a valid type specifier%s.",
              x, ("" if not complete_type else
                  (" (within type specifier %s)" % (complete_type,))))

def _complex_type_mismatch(x, type):
        ret = type[0].type_predicate(x, type)
        if _tuplep(ret) and _py.len(ret) != 3:
                error("Type matcher for %s returned an invalid value: %s.", type[0], _py.repr(ret))
        return (ret if not (_tuplep(ret) and ret[2]) else
                _invalid_type_specifier_error(ret[1], complete_type = type))

def _type_mismatch(x, type):
        """Determine, whether X does not belong to TYPE, and if so,
return a triple, specifying the specific parts of X and TYPE being in
disagreement and, as a third element, a boolean, denoting whether the
type specifier was malformed.  Otherwise, when X is of TYPE, a
negative boolean value is returned."""
        return (((not _py.isinstance(x, type)) and
                 (x, type, False))                    if _py.isinstance(type, _py.type)     else
                nil                                   if type is t                                else
                (((not _py.isinstance(x, type.python_type)) and
                  (x, type, False))                           if _py.hasattr(type, "python_type")    else
                 _complex_type_mismatch(x, _py.tuple([type])) if _py.hasattr(type, "type_predicate") else
                 _invalid_type_specifier_error(type)) if symbolp(type)                            else
                _complex_type_mismatch(x, type)       if (_tuplep(type) and type and
                                                          _py.hasattr(type[0], "type_predicate")) else
                _invalid_type_specifier_error(type))

@boot_defun
def typep(x, type):
        return not _type_mismatch(x, type)

@boot_defun
def deftype(type_name_or_fn):
        def do_deftype(fn, type_name = type_name_or_fn):
                symbol = _intern(type_name)[0]
                symbol.type_predicate = fn
                return symbol
        return (do_deftype(type_name_or_fn, type_name = _frost.python_name_lisp_symbol_name(type_name_or_fn.__name__)) if functionp(type_name_or_fn) else
                do_deftype                                                                                             if stringp(type_name_or_fn)   else
                error("In DEFTYPE: argument must be either a function or a string, was: %s.",
                      _py.repr(symbol_name_or_fn)))

@boot_defun
def the(type, x):
        mismatch = _type_mismatch(x, type)
        return (x if not mismatch else
                error(simple_type_error, "The value %s is not of type %s%s.",
                      x, type, ("" if (not _type_specifier_complex_p(type)) or type is mismatch[1] else
                                (", specifically, the value %s is not of type %s" % (princ_to_string(mismatch[0]), mismatch[1])))))

@boot_defun
def check_type(x, type):
        the(type, x)

def _of_type(x):
        return lambda y: typep(y, x)

def _not_of_type(x):
        return lambda y: not typep(y, x)
        return some(_type_mismatch, xs, _infinite(type))

# Complex type definitions

@deftype
def boolean(x, type):
        return ((x, type, True)  if _py.len(type) is not 1 else
                (x, type, False) if x not in [t, nil]      else
                nil)

@deftype
def null(x, type):
        return ((x, type, True)  if _py.len(type) is not 1 else
                (x, type, False) if x is not nil           else
                nil)

@deftype
def keyword(x, type):
        return ((x, type, True)  if _py.len(type) is not 1 else
                (x, type, False) if not keywordp(x)        else
                nil)

@deftype("OR")
def or_(x, type):
        return ((x, type, False) if _py.len(type) is 1 else
                _poor_man_let(mapcar(_type_mismatch, [x] * (_py.len(type) - 1), type[1:]),
                              lambda mismatches:
                                      (some(lambda m: m and m[2] and m, mismatches) or
                                       (every(identity, mismatches) and (x, type, False)))))

@deftype("AND")
def and_(x, type):
        return (nil       if _py.len(type) is 1 else
                some(_type_mismatch, [x] * (_py.len(type) - 1), type[1:]))

@deftype("NOT")
def not_(x, type):
        return ((x, type, True) if _py.len(type) is not 2 else
                _poor_man_let(_type_mismatch(x, type[1]),
                              lambda m: ((x, type, False) if not m      else
                                         m                if m and m[2] else
                                         nil)))

@deftype
def member(x, type):
        return ((x not in type[1:]) and
                (x, type, False))

@deftype
def satisfies(x, type):
        return ((x, type, True) if ((_py.len(type) is not 2) or
                                    not functionp(type[1])) else
                ((not type[1](x)) and
                 (x, type, False)))

@deftype
def eql(x, type):
        return ((x, type, True) if _py.len(type) is not 2 else
                ((not eql(x, type[1])) and
                 (x, type, False)))

@deftype
def unsigned_byte(x, type):
        return (((x, type, False) if not integerp(x) or minusp(x) else nil)                        if _py.len(type) is 1 else
                ((x, type, False) if not integerp(x) or minusp(x) or (x >= 1 << type[1]) else nil) if _py.len(type) is 2 else
                (x, type, True))

## Non-standard
@deftype
def pytypename(x, type):
        return ((x, type, True)  if _py.len(type) is not 1                      else
                (x, type, False) if not (symbolp(x) and _symbol_python_type(x)) else
                nil)

@deftype
def maybe(x, type):
        return ((x, type, True)  if _py.len(type) is not 2 else
                _poor_man_let(_type_mismatch(x, type[1]),
                              lambda m: (nil if not m      else
                                         m   if ((m and m[2]) or
                                                 not (x is nil or x is None)) else
                                         nil)))

@deftype
def pylist(x, type):
        return ((x, type, True)  if _py.len(type) is not 2       else
                (x, type, False) if not _py.isinstance(x, _py.list) else
                some(_type_mismatch, x, _infinite(type[1])))

@deftype
def pytuple(x, type):
        return ((x, type, False) if not (_tuplep(x) and _py.len(x) == _py.len(type) - 1) else
                some(_type_mismatch, x, type[1:]))
# Unregistered Issue TEACHABLE-TYPE-CHECKING-PRACTICE-AND-TOOL-CONSTRUCTION

@deftype
def partuple(x, type):
        return ((x, type, False) if not (_tuplep(x) and _py.len(x) >= _py.len(type) - 1) else
                some(_type_mismatch, x, type[1:]))

__variseq__ = (pytuple, (eql, maybe), t) # Meta-type, heh..
@deftype
def varituple(x, type):
        # correctness enforcement over speed?
        fixed_t, maybes_t = _prefix_suffix_if_not(_of_type(__variseq__), type[1:])
        if not every(_of_type(__variseq__), maybes_t):
                return (x, type, True)   # fail
        fixlen = _py.len(fixed_t)
        return ((x, type) if _py.len(x) < fixlen else
                some(_type_mismatch, x[:fixlen], fixed_t) or
                some(_type_mismatch, x[fixlen:], _infinite((or_,) + _py.tuple(t[1] for t in maybes_t))))

@deftype
def lambda_list(x, type):
        if type:
                return (x, type, True) # fail
        return typep(x, (pytuple,
                         (pylist, string),
                         (pylist, string),
                         (maybe,  string),
                         (pylist, string),
                         (maybe,  string)))

def _eql_type_specifier_p(x): return _tuplep(x) and _py.len(x) is 2 and x[0] is eql

_unboot_set("typep")

# Type relationships, rudimentary

def subtypep(sub, super):
        def coerce_to_python_type(x):
                return (x             if _py.isinstance(x, _cold_class_type)   else
                        x.python_type if symbolp(x) else
                        error("In SUBTYPEP: arguments must be valid type designators, but %s wasn't one.", _py.repr(x)))
        def do_subclass_check(sub, super):
                return _py.issubclass(coerce_to_python_type(sub),
                                      coerce_to_python_type(super))
        return (do_subclass_check(sub, super)                  if super is not t                            else
                _not_implemented("complex type relatioships: %s vs. %s.",
                                 sub, super)                   if _tuplep(sub) or _tuplep(super)            else
                error("%s is not a valid type specifier", sub) if not (typep(sub, (or_, _py.type, (eql, t))) and
                                                                       typep(sub, (or_, _py.type, (eql, t)))) else
                sub is super or super is t)

# Toplevel definitions: @DEFUN and @DEFCLASS

doit = False
def _make_cold_definer(definer_name, predicate, slot, preprocess, mimicry):
        def cold_definer(name_or_obj):
                obj, sym, name = _interpret_toplevel_value(name_or_obj, predicate)
                def do_cold_def(o):
                        # symbol = (_intern(_defaulted(name, _frost.python_name_lisp_symbol_name(o.__name__)))[0]
                        #           if stringp(name) else
                        #           name if symbolp(name) else
                        #           error("In %s: bad name %s for a cold object.", definer_name))
                        o = preprocess(o)
                        _frost.frost_def(o, sym, slot, _py.globals())
                        mimicry(sym, o)
                        return sym
                return (do_cold_def(obj) if obj                                          else
                        do_cold_def      if stringp(name_or_obj) or symbolp(name_or_obj) else
                        error("In %s: argument must be either satisfy %s or be a string;  was: %s.",
                              definer_name, predicate, _py.repr(name_or_obj)))
        cold_definer.__name__ = definer_name
        return cold_definer

del boot_defun
del boot_defclass

defun            = _cold_defun    = _make_cold_definer("%COLD-DEFUN",    functionp,
                                                       "function",    identity, _frost.make_object_like_python_function)
defclass         = _cold_defclass = _make_cold_definer("%COLD-DEFCLASS", lambda x: _py.isinstance(x, _py.type),
                                                       "python_type", identity,  _frost.make_object_like_python_class)
defun_with_block = _cold_defun_with_block = _make_cold_definer("%COLD-DEFUN-WITH-BLOCK", functionp,
                                                               "function", __block__, _frost.make_object_like_python_function)

for fn  in __boot_defunned__:   _frost.setf_global(defun(fn),     fn.__name__,  _py.globals())
for cls in __boot_defclassed__: _frost.setf_global(defclass(cls), cls.__name__, _py.globals())
doit = True

# Delayed class definitions

def _attrify_args(self, locals, *names):
        for name in names:
                setattr(self, name, locals[name])

@defclass
class nil():
        @classmethod
        def __instancecheck__(_, __): return False # This is an empty type

@defclass
class t():
        @classmethod
        def __instancecheck__(_, __): return True  # This is the absolute sum type

@defclass
class simple_condition(condition.python_type):
        def __init__(self, format_control, format_arguments):
                _attrify_args(self, locals(), "format_control", "format_arguments")
                # _debug_printf("About to signal a simple condition of type %s:\n%s", _py.type(self), self)
        def __str__(self):
                try:
                        return self.format_control % (1,).__class__(self.format_arguments)
                ## Unregistered Issue PROBABLE-PYTHON-BUG-PY-IS-NONE
                except self.__class__.__mro__[-2] as x: # Workaround for the above issue..
                        return "Failed to format into %s args %s." % (self.format_control.__repr__(),
                                                                      self.format_arguments.__repr__())
        def __repr__(self):
                return self.__str__()

@defclass
class simple_error(simple_condition.python_type, error.python_type):
        pass
@defclass
class package_error(error.python_type):
        pass
@defclass
class simple_package_error(simple_error.python_type, package_error.python_type):
        pass

# Rudimentary multiple values

#     The implemented version of NTH-VALUES is a soft one, which doesn't fail on values not
#     participating in the M-V frame protocol.

_intern_and_bind_pynames("%MV-MARKER")

def _values_frame(*xs):
        return (_mv_marker,) + xs

def _values_frame_p(x):
        return _tuplep(x) and x[0] is _mv_marker

def _values_frame_project(n, values_form):
        return ((nil if n > _py.len(values_form) - 2 else
                 values_form[n + 1])
                if _values_frame_p(values_form) else
                (nil if n else values_form))

# Early object system

@defun
def find_class(x, errorp = t):
        _not_implemented()

@defun
def make_instance(class_or_name, **initargs):
        return (class_or_name             if _py.isinstance(class_or_name, _cold_class_type) else
                class_or_name.python_type if symbolp(class_or_name)                          else
                error("In MAKE-INSTANCE %s: first argument must be a class specifier.", class_or_name))(**initargs)

@defun
def _make_missing_method(cls, name):
        return lambda *_, **_k: error("Missing method %s in class %%s." % name.upper(), cls)

# PRINT-UNREADABLE-OBJECT, sort of

def print_unreadable_object(object, stream, body, identity = None, type = None):
        write_string("#<", stream)
        if type:
                format(stream, "%s ", type_of(object).__name__)
        body()
        if identity:
                format(stream, " {%x}", _py.id(object))
        write_string(">", stream)

# Readtable and WITH-STANDARD-IO-SYNTAX

@defclass
class readtable(_collections.UserDict):
        def __init__(self, case = _keyword("upcase")):
                self.case = the((member, _keyword("upcase"), _keyword("downcase"), _keyword("preserve"), _keyword("invert")),
                                case)
                self.data = make_hash_table()

def readtablep(x):     return typep(x, readtable)
def readtable_case(x): return the(readtable, x).case

def copy_readtable(x):
        check_type(x, readtable)
        new = readtable(case = readtable_case(x))
        new.dict = make_hash_table()
        return new

__standard_pprint_dispatch__ = make_hash_table() # XXX: this is crap!
__standard_readtable__       = make_instance(readtable) # XXX: this is crap!

_intern_and_bind_pynames("*PRINT-ARRAY*", "*PRINT-BASE*", "*PRINT-CASE*", "*PRINT-CIRCLE*",
                         "*PRINT-ESCAPE*", "*PRINT-GENSYM*", "*PRINT-LENGTH*", "*PRINT-LEVEL*",
                         "*PRINT-LINES*", "*PRINT-MISER-WIDTH*", "*PRINT-PPRINT-DISPATCH*",
                         "*PRINT-PRETTY*", "*PRINT-RADIX*", "*PRINT-READABLY*", "*PRINT-RIGHT-MARGIN*",
                         "*READ-BASE*", "*READ-DEFAULT-FLOAT-FORMAT*", "*READ-EVAL*",
                         "*READ-SUPPRESS*",
                         "*READTABLE*")
__standard_io_syntax__ = _py.dict({_package_               : find_package("COMMON-LISP-USER"),
                                   _print_array_           : t,
                                   _print_base_            : 10,
                                   _print_case_            : _keyword("UPCASE"),
                                   _print_circle_          : nil,
                                   _print_escape_          : t,
                                   _print_gensym_          : t,
                                   _print_length_          : nil,
                                   _print_level_           : nil,
                                   _print_lines_           : nil,
                                   _print_miser_width_     : nil,
                                   _print_pprint_dispatch_ : __standard_pprint_dispatch__,
                                   _print_pretty_          : t,
                                   _print_radix_           : nil,
                                   _print_readably_        : nil,
                                   _print_right_margin_    : nil,
                                   _read_base_                 : 10,
                                   _read_default_float_format_ : "single-float",
                                   _read_eval_                 : t,
                                   _read_suppress_             : nil,
                                   _readtable_                 : __standard_readtable__})

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

def _set_settable_standard_globals():
        _string_set("*READ-CASE*", _keyword("UPCASE"))
        _string_set("*FEATURES*",  [])
        _string_set("*MODULES*",   [])
        _string_set("*STANDARD-INPUT*",  _sys.stdin)
        _string_set("*STANDARD-OUTPUT*", _sys.stdout)
        _string_set("*ERROR-OUTPUT*",    _sys.stderr)
        _string_set("*PRINT-ARRAY*",           __standard_io_syntax__[_print_array_])
        _string_set("*PRINT-BASE*",            __standard_io_syntax__[_print_base_])
        _string_set("*PRINT-CASE*",            __standard_io_syntax__[_print_case_])
        _string_set("*PRINT-CIRCLE*",          __standard_io_syntax__[_print_circle_])
        _string_set("*PRINT-GENSYM*",          __standard_io_syntax__[_print_gensym_])
        _string_set("*PRINT-ESCAPE*",          __standard_io_syntax__[_print_escape_])
        _string_set("*PRINT-LENGTH*",          __standard_io_syntax__[_print_length_])
        _string_set("*PRINT-LEVEL*",           __standard_io_syntax__[_print_level_])
        _string_set("*PRINT-LINES*",           __standard_io_syntax__[_print_lines_])
        _string_set("*PRINT-MISER-WIDTH*",     __standard_io_syntax__[_print_miser_width_])
        _string_set("*PRINT-PPRINT-DISPATCH*", __standard_io_syntax__[_print_pprint_dispatch_])
        _string_set("*PRINT-PRETTY*",          __standard_io_syntax__[_print_pretty_])
        _string_set("*PRINT-RADIX*",           __standard_io_syntax__[_print_radix_])
        _string_set("*PRINT-READABLY*",        __standard_io_syntax__[_print_readably_])
        _string_set("*PRINT-RIGHT-MARGIN*",    __standard_io_syntax__[_print_right_margin_])
        _string_set("*READ-BASE*",                 __standard_io_syntax__[_read_base_])
        _string_set("*READ-DEFAULT-FLOAT-FORMAT*", __standard_io_syntax__[_read_default_float_format_])
        _string_set("*READ-EVAL*",                 __standard_io_syntax__[_read_eval_])
        _string_set("*READ-SUPPRESS*",             __standard_io_syntax__[_read_suppress_])
        _string_set("*READTABLE*",                 __standard_io_syntax__[_readtable_])

_set_settable_standard_globals()

# Derived names:  %NoneType, REDUCE, SORT, %CURRY, STRINGP, %CLASSP, %NONEP etc.

_NoneType         = _py.type(None)

pi                = _math.pi
reduce            = _functools.reduce
sort              = _py.sorted
_curry            = _functools.partial

stringp           = _neutrality.stringp
_write_string     = _neutrality._write_string

def _classp(x):     return _py.isinstance(x, _py.type)
def _frozensetp(o): return _py.isinstance(o, _py.frozenset)
def _setp(o):       return _py.isinstance(o, (_py.set, _py.frozenset))
def _nonep(o):      return o is None

# Constants

most_positive_fixnum = 67108864

def _poor_man_let(*values_and_body):
        values, body = values_and_body[:-1], values_and_body[-1]
        return body(*values)

def _poor_man_defstruct(name, *slots):
        return _collections.namedtuple(name, slots)

def _poor_man_when(test, body):
        if test:
                return body() if functionp(body) else body

def _poor_man_case(val, *clauses):
        for (cval, body) in clauses:
                if ((val == cval or (cval is True) or (cval is t)) if not _py.isinstance(cval, _py.list) else
                    val in cval):
                        return body() if functionp(body) else body

def _poor_man_ecase(val, *clauses):
        for (cval, body) in clauses:
                if ((val == cval) if not _py.isinstance(cval, _py.list) else
                    val in cval):
                        return body() if functionp(body) else body
        error("%s fell through ECASE expression. Wanted one of %s.", val, mapcar(first, clauses))

def _poor_man_typecase(val, *clauses):
        for (ctype, body) in clauses:
                if (ctype is t) or (ctype is True) or typep(val, ctype):
                        return body() if functionp(body) else body

def _poor_man_etypecase(val, *clauses):
        for (ctype, body) in clauses:
                if (ctype is t) or (ctype is True) or typep(val, ctype):
                        return body() if functionp(body) else body
        else:
                error(simple_type_error, "%s fell through ETYPECASE expression. Wanted one of (%s).",
                      val, ", ".join(mapcar(lambda c: c[0].__name__, clauses)))

def _cold_constantp(form):
        # Coldness:
        #  - slow handling of constant variables
        #  - no handling of DEFCONSTANT-introduced variables
        #  - additional constant forms
        return (_py.isinstance(form, (_py.int, _py.float, _py.complex, _py.str)) or
                (type_of(form).__name__ == "symbol" and
                 ((form.package.name == "KEYWORD") or
                  (form.package.name == "COMMON-LISP" and form.name in ["T", "NIL", "PI"]))) or
                (_tuplep(form)                          and
                 _py.len(form) == 2                        and
                 type_of(form[0]).__name__ == "symbol" and
                 form.package.name == "COMMON-LISP"     and
                 form.name in ["QUOTE"]))
constantp = _cold_constantp

# Basic string/char functions and %CASE-XFORM

@defun
def string_upcase(x):     return x.upper()
@defun
def string_downcase(x):   return x.lower()
@defun
def string_capitalize(x): return x.capitalize()

@defun
def char_upcase(x):       return x.upper()
@defun
def char_downcase(x):     return x.lower()
@defun
def upper_case_p(x):      return x.isupper()
@defun
def lower_case_p(x):      return x.islower()

_case_attribute_map = _py.dict(UPCASE     = string_upcase,
                               DOWNCASE   = string_downcase,
                               CAPITALIZE = string_capitalize,
                               PRESERVE   = identity)
def _case_xform(type, s):
        if not (symbolp(type) and type.package.name == "KEYWORD"):
                error("In CASE-XFORM: case specifier must be a keyword, was a %s: %s.", _py.type(type), _print_symbol(type))
        return _case_attribute_map[type.name](s)

# Possibly dangling cold boot code

#     I wonder if this boot state infrastructure is a good idea:
#     - it tangles the flow of things (?)

def _global(x):
        """This is important due to the single namespace, and the
consequent shadowing of various specifiers."""
        return _frost.global_(x, _py.globals())[0]

def _cold_format(destination, control_string, *args):
        string = control_string % args
        if not destination:
                return string
        else:
                _write_string(string, _sys.stderr if destination is t else destination)
format = _cold_format
def _cold_princ_to_string(x):
        return _py.repr(x)
princ_to_string = _cold_princ_to_string
# Unregistered Issue PACKAGE-INIT-MUST-TAKE-COLD-SYMBOL-VALUES-INTO-ACCOUNT
def _cold_probe_file(pathname):
        assert(stringp(pathname))
        return _os.path.exists(the(string, pathname))
probe_file = _cold_probe_file

# Python module compilation

def _load_code_object_as_module(name, co, filename = "", builtins = None, globals = None, locals = None, register = True):
        check_type(co, _py.type(_load_code_object_as_module.__code__))
        mod = _imp.new_module(name)
        mod.__filename__ = filename
        if builtins:
                mod.__dict__["__builtins__"] = builtins
        if register:
                _sys.modules[name] = mod
        globals = _defaulted(globals, mod.__dict__)
        locals  = _defaulted(locals, mod.__dict__)
        _py.exec(co, globals, locals)
        return mod, globals, locals

def _load_text_as_module(name, text, filename = "", **keys):
        return _load_code_object_as_module(name, _py.compile(text, filename, "exec"),
                                           filename = filename, **keys)[0]

def _reregister_module_as_package(mod, parent_package = None):
        # this line might need to be performed before exec()
        mod.__path__ = (parent_package.__path__ if parent_package else []) + [ mod.name.split(".")[-1] ]
        if parent_package:
                dotpos = mod.name.rindex(".")
                assert dotpos
                postdot_name = mod.name[dotpos + 1:]
                _py.setattr(parent_package, postdot_name, mod)
                parent_package.__children__.add(mod)
                mod.__parent__ = parent_package
        if packagep:
                mod.__children__ = _py.set()

def _py_compile_and_load(*body, modname = "", filename = "", lineno = 0, **keys):
        return _load_code_object_as_module(
                modname,
                _py.compile(_ast.fix_missing_locations(_ast_module(_py.list(body), lineno = lineno)), filename, "exec"),
                register = nil,
                filename = filename,
                **keys)

def _ast_compiled_name(name, *body, **keys):
        mod, globals, locals = _py_compile_and_load(*body, **keys)
        return locals[name]

# Python frames

def _all_threads_frames():
        return _sys._current_frames()

def _this_frame():
        return _sys._getframe(1)

_frame = _py.type(_this_frame())

def _framep(x):
        return typep(x, _frame)

def _next_frame(f):
        return f.f_back if f.f_back else error("Frame \"%s\" is the last frame.", _pp_frame(f, lineno = True))

def _caller_frame(caller_relative = 0):
        return _sys._getframe(caller_relative + 2)

def _frames_calling(f = None, n = -1):
        "Semantics of N are slightly confusing, but the implementation is so simple.."
        f = _caller_frame() if f is None else the(_frame, f)
        acc = [f]
        while f.f_back and n:
                f, n = f.f_back, n - 1
                acc.append(f)
        return acc

def _caller_name(n = 0):
        return _fun_name(_frame_fun(_sys._getframe(n + 2)))

def _exception_frame():
        return _sys.exc_info()[2].tb_frame

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
__ordered_frame_locals__ = _py.dict()
def _frame_ordered_locals(f):
        global __ordered_frame_locals__
        if f not in __ordered_frame_locals__:
                __ordered_frame_locals__[f] = _py.list(f.f_locals.keys())
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

# Frame pretty-printing

def _frame_fun_name(f):          return f.f_code.co_name

def _print_function_arglist(f):
        argspec = _inspect.getargspec(f)
        return ", ".join(argspec.args +
                         (["*" + argspec.varargs]   if argspec.varargs  else []) +
                         (["**" + argspec.keywords] if argspec.keywords else []))

def _pp_frame(f, align = None, handle_overflow = None, lineno = None):
        fun = _frame_fun(f)
        fun_name, fun_params, filename = _fun_info(fun)[:3]
        align = ((align or 10) if handle_overflow else
                 _defaulted(align, 0))
        return ("%s%s %s(%s)" % (filename + ("" if align else ":") + (" " * (align - (_py.len(filename) % align if align else 0))),
                                 ("%d:" % _frame_lineno(f)) if lineno else "",
                                 fun_name, ", ".join(fun_params)))

def _print_frame(f, stream = None, **keys):
        write_string(_pp_frame(f, **keys), _defaulted_to_var(stream, _debug_io_))

def _print_frames(fs, stream = None):
        mapc(lambda i, f: format(_defaulted_to_var(stream, _debug_io_), "%2d: %s\n" % (i, _pp_frame(f, lineno = True))),
             *_py.zip(*_py.enumerate(fs)))

def _backtrace(x = -1, stream = None):
        _print_frames(_frames_calling(_this_frame())[1:x],
                      _defaulted_to_var(stream, _debug_io_))

def _pp_frame_chain(xs, source_location = None, all_pretty = None, print_fun_line = None):
        def _pp_frame_in_chain(f, pretty = None):
                fun = _frame_fun(f)
                return format(nil, *(("%s",
                                      _fun_name(fun))
                                     if not pretty else
                                     ("%s%s@%s:%d",
                                      _fun_name(fun),
                                      (":" + _py.str(_frame_lineno(f) - _fun_firstlineno(fun))) if print_fun_line else "",
                                      _fun_filename(fun),
                                      _frame_lineno(f))))
        return ("..".join(mapcar(lambda f: _pp_frame_in_chain(f, t), xs) if all_pretty else
                          (mapcar(lambda f: _pp_frame_in_chain(f), xs[:-1]) +
                           [_pp_frame_in_chain(xs[-1], t)])))

def _pp_chain_of_frame(x, callers = 5, *args, **keys):
        fs = _frames_calling(x, callers)
        fs.reverse()
        return _pp_frame_chain(fs, *args, **keys)

def _escape_percent(x):
        return x.replace("%", "%%")

# Higher-level debug trace functions

# lf = open("/home/deepfire/lf", "w")
def _frame_chain_hash(f, ignore_callers = _py.set(["<lambda>"])):
        "Return an MD5 digest of the caller name chain, with callers listed in IGNORE-CALLERS omitted."
        def f_digestible(f):
                name = f.f_code.co_name
                return name.encode() if name not in ignore_callers else b''
        fchain = _frames_calling(f)[1:]
        retv = reduce((lambda acc, f:
                               acc.update(f_digestible(f)) or acc),
                      fchain, _hashlib.new("md5")).hexdigest()
        # _fprintf(lf, "%s %s\n", mapcar(f_str, _py.reversed(chain)), r)
        return retv

def _frame_id(f):
        return _hashlib.new("md5", ("%x" % id(f)).encode()).hexdigest()

def _here(note = None, *args, callers = 5, stream = None, default_stream = _sys.stderr, frame = None, print_fun_line = None, all_pretty = None):
        def _do_format(x, args):
                try:
                        return x % (_py.tuple(args))
                except _cold_error_type as cond:
                        return "#<error formatting %s into %s: %s>" % (args.__repr__(), note.__repr__(), cond)
        def format_args():
                return (""           if not note else
                        " - " + note if not args else
                        # Unregistered Issue IDEA-MAPXFORM-IF
                        _do_format(note, args))
        return _debug_printf("    (%s)  %s:\n      %s",
                             _threading.current_thread().name.upper(),
                             _pp_chain_of_frame(_defaulted(frame, _caller_frame()),
                                                callers = callers - 1,
                                                print_fun_line = print_fun_line,
                                                all_pretty = all_pretty),
                             _without_condition_system(format_args),
                             # _defaulted(stream, default_stream)
                             )

def _locals_printf(locals, *local_names):
        # Unregistered Issue NEWLINE-COMMA-SEPARATION-NOT-PRETTY
        _fprintf(_sys.stderr, ", ".join((("%s: %%s" % x) if stringp(x) else "%s")
                                        for x in local_names) + "\n",
                 *((locals[x] if stringp(x) else "\n") for x in local_names))

# Raw data of frame research

# ## Unregistered Issue PAREDIT-MUST-BE-TAUGHT-ABOUT-COMMENTS-WITHIN-BABEL-BLOCKS

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
                "names: globals;  varnames: args + otherbind;  locals: _py.len(varnames)"
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
#                 (lambda x: "\n  ".join(map(lambda s: s + ": " + _py.str(getattr(x, s)),
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
#   co_code: b'y\x11\x00t\x00\x00d\x01\x00|\x00\x00\x83\x02\x00\x01Wn,\x00\x04t\x01\x00k\n\x00r?\x00\x01\x01\x00\x01z\x0c\x00t\x02\x00\x83\x00\x00SWYd\x02\x00d\x02\x00\x01\x00~\x01\x00Xn\x01\x00Xd\x02\x00S'
#   co_consts: ('names: globals;  varnames: args + otherbind;  locals: _py.len(varnames)', 'This is xceptor talking: %s.', None)
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
#   co_code: b'd\x01\x00\x01\x00\x88\x00\x00|\x00\x00|\x01\x00\x17\x83\x01\x00S'
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
#   co_code: b'd\x01\x00\x00\x00\x88\x00\x00|\x00\x00\x83\x01\x00S'
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
#   co_code: b'd\x01\x00\x84\x00\x00\x89\x00\x00\x87\x00\x00f\x01\x00d\x02\x00\x86\x00\x00\x89\x01\x00\x87\x01\x00f\x01\x00d\x03\x00\x86\x00\x00\x00\x00|\x00\x00\x83\x00\x00S'
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

# Alist/plist extensions

def _alist_plist(xs):
        return append(*xs)

def _plist_alist(xs):
        acc = []
        for i in _py.range(0, _py.len(xs), 2):
                acc.append((xs[i], xs[i + 1]))
        return acc

def _plist_keys(xs):        return xs[::2]
def _plist_values(xs):      return xs[1::2]
def _plist_keys_values(xs): return xs[::2], xs[1::2]

def _hash_table_alist(xs):
        return xs.items()

def _alist_hash_table(xs):
        return _py.dict(xs)

# %CACHE

class _cache(_collections.UserDict):
        def __init__(self, filler):
                self.filler = filler
                self.data = _py.dict()
        def __getitem__(self, key):
                check_type(key, pytuple)
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
                              _poor_man_let(map_computer(x),
                                            lambda y: ((y, get_universal_time()) if x else
                                                       None)))
        def cache_getter(x):
                res = cache[(x, 0)]
                return res[0] if res is not None else None
        return cache, cache_getter

def _read_case_xformed(x):
        return _case_xform(_symbol_value(_read_case_), x)

# Pergamum 0

def _if_let(x, consequent, antecedent = lambda: None):
        return consequent(x) if x else antecedent()

def _when_let(x, consequent):
        return consequent(x) if x else None

def _lret(value, body):
        body(value)
        return value

def _compose(f, g):
        return lambda *args, **keys: f(g(*args, **keys))

def _ensure_list(x):
        return x if _listp(x) else [x]
def _ensure_car(x):
        return x[0] if _tuplep(x) else x
def _ensure_cons(x, default = None):
        return x if _tuplep(x) and _py.len(x) == 2 else (x, default)

def _mapset(f, xs):
        acc = _py.set()
        for x in xs:
                acc.add(f(x))
        return acc

def _mapsetn(f, xs):
        acc = _py.set()
        for x in xs:
                acc |= f(x)
        return acc

def _mapseparaten(f, xs):
        s0, s1 = _py.set(), _py.set()
        for s0r, s1r in (f(x) for x in xs):
                s0 |= s0r; s1 |= s1r
        return s0, s1

def _separate(n, f, xs):
        ss = _py.tuple(_py.set() for _ in _py.range(n))
        for rss in (f(x) for x in xs):
                for s, rs in _py.zip(ss, rss):
                        s |= rs
        return ss

__combiners__ = { _py.set: _py.set.add, _py.list: _py.list.append }
def _recombine(spec, f, xss):
        accs  = _py.tuple(f() for f in spec)
        combs = _py.tuple(__combiners__[_py.type(a)] for a in accs)
        for xs in xss:
                for acc, comb, reselt in _py.zip(accs, combs, f(xs)):
                        comb(acc, reselt)
        return accs
def _recombine_star(spec, f, *xss):
        accs  = _py.tuple(f() for f in spec)
        combs = _py.tuple(__combiners__[_py.type(a)] for a in accs)
        for xs in _py.zip(*xss):
                for acc, comb, reselt in _py.zip(accs, combs, f(*xs)):
                        comb(acc, reselt)
        return accs

def _mapcar_star(f, xs):
        return [ f(*x) for x in xs ]

def _slotting(x):             return lambda y: _py.getattr(y, x, None)
def _slot_of(x):              return lambda y: _py.getattr(x, y, None)
def _slot_equal(slot, val):   return lambda y: _py.getattr(y, slot, None) == val

def _indexing(*is_):          return lambda y: aref(y, *is_)
def _index_of(xs):            return lambda *is_: aref(xs, *is_)
def _index_equal(index, val): return lambda y: y[index] == val

def _updated_dict(to, from_):
        to.update(from_)
        return to

def _prefix_suffix_if(f, xs, key = identity):
        for i, x in _py.enumerate(xs):
                if not f(key(x)):
                        return xs[:i], xs[i:]
        return xs, []

def _prefix_suffix_if_not(f, xs, key = identity):
        return _prefix_suffix_if(lambda x: not f(x), xs, key = key)

def _defwith(name, enter, exit, **initargs):
        initargs.update(_py.dict(__enter__ = enter,
                                 __exit__  = exit))
        return _py.type(name, (object,), initargs)

def _lookup(scope, name):
        if not scope: return nil, nil
        frame, rest = scope
        for cell_name, value in frame:
                if cell_name is name:
                        return value, t
        return _lookup(name, rest)

def _defscope(name, varname, **initargs):
        def make_frame(bindings):
                return _py.tuple(bindings.items())
        def push_frame(**bindings):
                # This dictionary<->alist-ing is a waste.
                return _dynamic_scope_push({ varname: (make_frame(bindings), _symbol_value(var)) })
        def pop_frame(*_): _dynamic_scope_pop()
        return _defwith(name, push_frame, pop_frame, lookup = _lookup, **initargs)

# Lesser non-CL tools

class _withless():
        @staticmethod
        def __init__(): pass
        @staticmethod
        def __enter__(): pass
        @staticmethod
        def __exit__(*_): pass

class _servile():
        def __repr__(self):
                return "#%s(%s)" % (_py.type(self).__name__,
                                    ", ".join(_maphash(lambda k, v: "%s = %s" % (k, v),
                                                       self.__dict__)))
        def __init__(self, **keys):
                self.__dict__.update(keys)

def _gen(n = 1, x = "G", gen = gensym):
        if zerop(n):
                error("_GEN: we are very very much against this, please stop doing it!")
        return (_py.tuple(gen(x)
                for i in _py.range(n)))
def _gensyms(**initargs):     return _gen(gen = gensym,      **initargs)
def _gensymnames(**initargs): return _gen(gen = _gensymname, **initargs)

# Basic functions

@defun
def eq(x, y):
        return x is y

@defun
def eql(x, y):
        ## EQL is needed by the compiler.
        ## Python is really cute:
        # >>> 256 is (255 + 1)
        # True
        # >>> 257 is (256 + 1)
        # False
        return (x is y) if not _py.isinstance(x, _py.int) else x == y

@defun
def equal(x, y):
        return x == y

def _infinite(x):
        while True:
                yield x

def _seek(n, iterator):
        for i in range(n):
                _py.next(iterator, nil)

def _from(n, xs):
        iterator = iter(xs)
        for i in range(n):
                next(iterator, nil)
        for x in iterator:
                yield x

_termination_marker = gensym()
def _take(n, xs):
        iterator = iter(xs)
        for i in range(n):
                elt = next(iterator, _termination_marker)
                if elt is not _termination_marker:
                        yield elt

@defun
def every(fn, *xss, start = 0):
        for xs in _from(start, _py.zip(*xss)):
                if not fn(*xs): return nil
        return (xs or t) if "xs" in _py.locals() else t

@defun
def notevery(fn, *xss, start = 0):
        for xs in _from(start, _py.zip(*xss)):
                ret = fn(*xs)
                if not ret: return ret or t
        return nil

@defun
def some(fn, *xss, start = 0):
        for xs in _from(start, _py.zip(*xss)):
                ret = fn(*xs)
                if ret: return ret or t
        return nil

@defun
def notany(fn, *xss, start = 0):
        for xs in _from(start, _py.zip(*xss)):
                if fn(*xs): return nil
        return (xs or t) if "xs" in _py.locals() else t

def _xorf(x, y):
        return (x or y) and not (x and y)

def _nxorf(x, y):
        return (x and y) or not (x or y)

# Predicates

@defun
def evenp(x):         return not (x % 2)
@defun
def oddp(x):          return not not (x % 2)
@defun
def zerop(x):         return x == 0
@defun
def plusp(x):         return x > 0
@defun
def minusp(x):        return x < 0

# Conses

@defun
def cdr(x):           return x[1:]  if x  else nil
@defun
def second(xs):       return xs[1]  if _py.len(xs) > 1 else nil
@defun
def third(xs):        return xs[2]  if _py.len(xs) > 2 else nil
@defun
def rest(xs):         return xs[1:] if xs else nil
@defun
def nth(n, xs):       return xs[n] if n < _py.len(xs) else nil

@defun
def copy_list(x):
        _not_implemented()

@defun
def pop(xs):
        if xs:
                x, xs[0:1] = xs[0], []
                return x
        else:
                return nil

# Functions

@defun
def complement(f):
        return lambda x: not f(x)

@defun
def constantly (x):
        return lambda *args: x

# Sequences

@defun
def stable_sort(xs, predicate):
        return sorted(xs, key = _functools.cmp_to_key(predicate))

@defun
def vector_push(vec, x):
        "XXX: compliance"
        vec.append(x)
        return vec

@defun
def vector_push_extend(vec, x):
        "XXX: compliance"
        vec.append(x)
        return vec

@defun
def getf(xs, key, default = None):
        for i, x in _py.enumerate(xs):
                if not i%2 and x == key:
                        return xs[i + 1]
        else:
                return _defaulted(default, nil)

@defun
def setf_getf(value, xs, key):
        for i, x in _py.enumerate(xs):
                if not i%2 and x == key:
                        xs[i + 1] = value
                        return xs
        else:
                return [key, value] + xs

@defun
def assoc(x, xs, test = equal):
        for k, v in xs:
                if test(x, k):
                        return v

@defun
def aref(xs, *indices):
        r = xs
        for i in indices:
                r = r[i]
        return r

@defun
def subseq(xs, start, end = None):
        return xs[start:end]

@defun
def make_list(size, initial_element = None):
        # horribly inefficient, but that's what we have..
        return mapcar(constantly(initial_element), range(size))

@defun
def mapcar(f, *xs):
        return [ f(*x) for x in _py.zip(*xs) ]

@defun
def mapcan(f, *xs):
        return reduce(append, [ f(*x) for x in _py.zip(*xs) ]) if (xs and xs[0]) else []

@defun
def mapc(f, *xs):
        for x in _py.zip(*xs):
                f(*x)
        return xs[0]

__allowed__ = _py.frozenset([_py.str, _py.set, _py.frozenset, _py.tuple, _py.list, _py.bytes, _py.bytearray])
def _maprestype(x):
        type = type_of(x)
        return type if type in __allowed__ else _py.list

@defun
def remove_if(f, xs, key = identity):
        if hash_table_p(xs):
                return              { k:x for k, x in xs.items() if not f(k, key(x))}
        else:
                return _maprestype(xs) (x for x    in xs         if not f(key(x)))

@defun
def remove_if_not(f, xs, key = identity):
        if hash_table_p(xs):
                return              { k:x for k, x in xs.items() if f(k, key(x))}
        else:
                return _maprestype(xs) (x for x    in xs         if f(key(x)))

@defun
def remove(elt, xs, test = eql, key = identity):
        if hash_table_p(xs):
                return              { k:x for k, x in xs.items() if not test(elt, key(x))}
        else:
                return _maprestype(xs) (x for x    in xs         if not test(elt, key(x)))

@defun
def find_if(p, xs, key = identity, start = 0, end = None, from_end = None):
        # Unregistered Issue FIND-IF-NOT-ITERATOR-FRIENDLY
        end = end or _py.len(xs)
        if start or end:
                seq = _py.zip(xs, _py.range(_py.len(xs)))
                if from_end:
                        seq = _py.reversed(_py.list(seq))
                for (x, i) in seq:
                        if (start <= i < end) and p(key(x)):
                                return x
        else:
                if from_end:
                        xs = reversed(xs)
                for x in xs:
                        if p(key(x)):
                                return x

@defun
def find_if_not(p, xs, key = identity, start = 0, end = None, from_end = None):
        return find_if(complement(p), xs, key = key, start = start, end = end, from_end = from_end)

@defun
def find(elt, xs, **keys):
        return find_if(lambda x: x == elt, xs, **keys)

@defun
def memq(item, list):
        "Return tail of LIST beginning with first element EQ to ITEM."
        # List views?
        for i, x in _py.enumerate(xs):
                if x is elt:
                        return xs[i:]
        return []

@defun
def member_if(test, xs):
        "XXX: not terribly compliant."
        for i, x in _py.enumerate(xs):
                if test(x):
                        return xs[i:]

@defun
def member(x, xs):
        "XXX: not terribly compliant."
        return member_if(lambda y: y == x, xs)

@defun
def position_if(p, xs, key = identity, start = 0, end = None, from_end = None):
        end = end or _py.len(xs)
        if start or end:
                seq = _py.zip(xs, _py.range(_py.len(xs)))
                if from_end:
                        seq = _py.reversed(_py.list(seq))
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

@defun
def position_if_not(p, xs, key = identity, start = 0, end = None, from_end = None):
        return position_if(complement(p), xs, key = key, start = start, end = end, from_end = from_end)

@defun
def position(elt, xs, **keys):
        return position_if(lambda x: x == elt, xs, **keys)

@defun
def count(elt, xs, key = identity, start = 0):
        c = 0
        for (x, i) in _py.zip(xs, _py.range(_py.len(xs))):
                if (i >= start) and key(x) == elt:
                        c += 1
        return c

@defun
def count_if(p, xs, key = identity, start = 0):
        c = 0
        for (x, i) in _py.zip(xs, _py.range(_py.len(xs))):
                if (i >= start) and p(key(x)):
                        c += 1
        return c

@defun
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
@defun
def tailp(object, list):
        """If OBJECT is the same as some tail of LIST, TAILP returns
true; otherwise, it returns false."""
        if _py.len(object) > _py.len(list):
                return None
        else:
                list_start = _py.len(list) - _py.len(object)
                return list[list_start:] == object

# XXX: This is geared at cons-style lists, and so is fucking costly
# for imperative lists.
@defun
def ldiff(object, list_):
        """If OBJECT is the same as some tail of LIST, LDIFF returns a
fresh list of the elements of LIST that precede OBJECT in the
list structure of LIST; otherwise, it returns a copy[2] of
LIST."""
        if _py.len(object) > _py.len(list_):
                return _py.list(list_)
        else:
                list_start = _py.len(list_) - _py.len(object)
                if list_[list_start:] == object:
                        return list_[:list_start]
                else:
                        return _py.list(list_)

def _intersperse(x, xs):
        """Return a sequence of elements, with X inserted between every two
adjacent elements of XS."""
        acc = []
        if xs:
                for ix in xs[:-1]:
                        acc.append(ix)
                        acc.append(x)
                acc.append(xs[-1])
        return acc

# String functions

@defun
def string_equal(xs, ys):            return xs == ys
@defun
def string_greater(xs, ys):          return xs > ys
@defun
def string_greater_or_equal(xs, ys): return xs >= ys
@defun
def string_less(xs, ys):             return xs < ys
@defun
def string_less_or_equal(xs, ys):    return xs <= ys

@defun
def string_right_trim(cs, s):
        return s.rstrip("".join(cs))

@defun
def string_left_trim(cs, s):
        return s.lstrip("".join(cs))

@defun
def string_trim(cs, s):
        return s.strip("".join(cs))

# Sets

@defun
def union(x, y):
        return x | y

@defun
def intersection(x, y):
        return x & y

# Dicts

# Issue INCONSISTENT-HASH-TABLE-FUNCTION-NAMING
def _dictappend(*dicts):
        acc = _py.dict()
        for dict in dicts:
                acc.update(dict)
        return acc

def _dict_select_keys(dict, *keys):
        acc = _py.dict()
        for k in keys:
                if k in dict:
                        acc[k] = dict[k]
        return acc

def _maphash(f, dict) -> _py.list:
        return [ f(k, v) for k, v in dict.items() ]

def _remap_hash_table(f, xs: _py.dict) -> _py.dict:
        return { k: f(k, v) for k, v in xs.items() }

def _map_into_hash_star(f, xs,
                        key_test = lambda k: k is not None,
                        value_test = lambda _: t) -> _py.dict:
        acc = make_hash_table()
        for x in xs:
                k, v = f(*x)
                if key_test(k) and value_test(v):
                        acc[k] = v
        return acc

def _map_hash_table(f, hash_table, **keys) -> _py.dict:
        return _map_into_hash_star(f, hash_table.items(), **keys)

def _symbol_known(symbol_):
        return symbol_.known
def _symbol_python_type(symbol_, if_not_a_type = "error"):
        return (symbol_.python_type                                  if _py.hasattr(the(symbol, symbol_), "python_type") else
                nil                                                                        if if_not_a_type == "continue" else
                error("In %%SYMBOL-TYPE %s: symbol does not designate a known type.", symbol) if if_not_a_type == "error" else
                error("In %%SYMBOL-TYPE: the :IF-NOT-A-TYPE keyword argument must be one of ('error, 'continue')."))
def _symbol_type_predicate(symbol):
        return symbol.type_predicate if _py.hasattr(the(symbol, symbol_), "type_predicate") else nil

# Lisp packages/symbols vs. python modules/names

def _lisp_symbol_python_name(sym):
        return _frost.lisp_symbol_name_python_name(sym.name)

def _lisp_symbol_python_names(sym):
        return (_frost.lisp_symbol_name_python_name(sym.name),
                _frost.lisp_symbol_name_python_name(sym.package.name))

def _find_module(name, if_does_not_exist = "error"):
        return (_frost.find_module(name) or
                _poor_man_ecase(if_does_not_exist,
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

# Functions

def _variable_kind(name):
        check_type(name, symbol)
        # (ecase kind
        #   (:special "a special variable")
        #   (:macro "a symbol macro")
        #   (:constant "a constant variable")
        #   (:global "a global variable")
        #   (:unknown "an undefined variable")
        #   (:alien "an alien variable"))
        _not_implemented()

@defun
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

@defun
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
         symbol.macro_function,
         symbol.compiler_macro_function) = nil, nil, nil
        return name

## @defun def function was moved lower, due to dependency on @defun and CL:T

@defun
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
        return _symbol_function(the(symbol, symbol_))

@defun
def macro_function(symbol_, environment = nil):
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
        b_or_res = (the(_lexenv, environment).lookup_func_kind(macro, symbol_, nil) if environment else
                    the(symbol, symbol_).macro_function)
        if b_or_res and _bindingp(b_or_res) and _tuplep(b_or_res.value):
               lambda_list, body = b_or_res.value
               b_or_res = the(function, _compile_in_lexenv(nil,
                                                           (lambda_, lambda_list) + body,
                                                           environment))
        return the((or_, function, null),
                   (b_or_res.value if _bindingp(b_or_res) else b_or_res) or nil)

@defun
def compiler_macro_function(symbol_, environment = nil, check_shadow = t):
        """compiler-macro-function symbol &optional environment => function"""
        shadow = check_shadow and environment and environment.funcscope_binds_p(symbol_)
        return (the((or_, function, null),
                     the(symbol, symbol_).compiler_macro_function) if not shadow else
                nil)

@defun
def setf_compiler_macro_function(new_function, symbol, environment = nil):
        "<See documentation for COMPILER-MACRO-FUNCTION>"
        ## Ensure compliance.
        check_type(environment, null)
        symbol.compiler_macro_function = the(function, new_function)
        return new_function

def _symbol_macro_expander(symbol_, environment = None):
        ## -> (-> expansion) | None
        lexical = environment and the(_lexenv, environment).lookup_var_kind(symbol_macro, symbol)
        expansion = (the(symbol, symbol_).symbol_macro_expansion if not lexical else
                     lexical.value)
        return (lambda: expansion) if expansion is not None else None

def _style_warn(control, *args):
        warn(simple_style_warning, control, *args)

def _warn_incompatible_function_redefinition(symbol, tons, fromns):
        _style_warn("%s is being redefined as a %s when it was previously defined to be a %s.", symbol, tons, fromns)

def _warn_possible_redefinition(x, type):
        if x:
                _style_warn("In %s: %s is being redefined.", type, x.name)

@defun
def setf_macro_function(new_function, symbol, environment = nil):
        "<See documentation for MACRO-FUNCTION>"
        ## Ensure compliance.
        check_type(environment, null)
        if symbol.function:
                _warn_incompatible_function_redefinition(symbol, "macro", "function")
                symbol.function = nil
        _warn_possible_redefinition(symbol.macro_function, defmacro)
        symbol.macro_function = the(function, new_function)
        return new_function

@defun
def setf_fdefinition(new_definition, function_name):
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
        the(symbol, function_name).function = new_definition
        return new_definition

@defun
def special_operator_p(symbol_):
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
        return t if _find_known(symbol_) else nil

# * (do-external-symbols (s :common-lisp) (when (special-operator-p s) (print s)))
#  TAGBODY
#  GO
#
#  IF
#  PROGN
#
#  BLOCK
#  RETURN-FROM
#
#  CATCH
#  THROW
#
#  UNWIND-PROTECT
#
#  NIL
#  THE
#  QUOTE
#  FUNCTION
#  LOAD-TIME-VALUE
#
#  LABELS
#  LET
#  LET*
#  FLET
#  MACROLET
#  SYMBOL-MACROLET
#
#  PROGV
#  SETQ
#
#  LOCALLY
#  EVAL-WHEN
#
#  MULTIPLE-VALUE-CALL
#  MULTIPLE-VALUE-PROG1

def _do_set_function_definition(function, x):
        if the(symbol, x).macro_function:
                _warn_incompatible_function_redefinition(x, "function", "macro")
        else:
                _warn_possible_redefinition(x.function, the(symbol, defun))
        x.function, x.macro_function = function, nil
        function and _frost.make_object_like_python_function(x, function)
        return x

_intern_and_bind_pynames("DEFMACRO")
def _do_set_macro_definition(function, x):
        if the(symbol, x).function:
                _warn_incompatible_function_redefinition(x, "macro", "function")
        else:
                _warn_possible_redefinition(x.macro_function, the(symbol, defmacro))
        x.function, x.macro_function = nil, function
        function and _frost.make_object_like_python_function(x, function)
        return x

@defun(intern("_set_function_definition")[0])
def _set_function_definition(function):
        return _do_set_function_definition(function, intern(function.__name__)[0])

@defun(intern("_set_macro_definition")[0])
def _set_macro_definition(function):
        return _do_set_macro_definition(function, intern(function.__name__)[0])

# Condition system disabling

def _without_condition_system(body, reason = ""):
        if _frost.pytracer_enabled_p():
                try:
                        _frost.disable_pytracer()
                        return body()
                finally:
                        _frost.enable_pytracer()
        else:
                return body()

# Condition system init

def _init_condition_system():
        _frost.enable_pytracer() ## enable HANDLER-BIND and RESTART-BIND

def _condition_system_enabled_p():
        return (_frost.pytracer_enabled_p() and
                _frost.tracer_hook("exception") is __cl_condition_handler__)

def _init_package_system_2():
        "Is called once CL is loaded completely."
        in_package("COMMON-LISP-USER")

_load_toplevel, _compile_toplevel, _execute = mapcar(_keyword, ["LOAD-TOPLEVEL",
                                                                "COMPILE-TOPLEVEL",
                                                                "EXECUTE"])

_init_condition_system()

# Rudimentary character type

@defclass
class base_char(): pass

# Early-earlified streaming

@defun
def streamp(x):                     return typep(x, stream)

def _file_stream_p(x):              return typep(x, file_stream)

@defun
def with_open_stream(stream, fn):
        try:
                return fn(stream)
        finally:
                close(stream)

@defun
def open(pathname, direction = _keyword("INPUT"), element_type = base_char,
         if_exists = _keyword("ERROR"), if_does_not_exist = _keyword("ERROR"),
         external_format = _keyword("DEFAULT")):
        """Return a stream which reads from or writes to FILENAME.
Defined keywords:
 :DIRECTION - one of :INPUT, :OUTPUT, :IO, or :PROBE
 :ELEMENT-TYPE - the type of object to read or write, default BASE-CHAR
 :IF-EXISTS - one of :ERROR, :NEW-VERSION, :RENAME, :RENAME-AND-DELETE,
                     :OVERWRITE, :APPEND, :SUPERSEDE or NIL
 :IF-DOES-NOT-EXIST - one of :ERROR, :CREATE or NIL"""
        ## Unregistered Issue COMPLIANCE-OPEN-PROBE-OUTPUT-DIRECTION
        ## Unregistered Issue COMPLIANCE-OPEN-ELEMENT-TYPE
        ## Unregistered Issue COMPLIANCE-OPEN-IF-EXISTS
        ## Unregistered Issue COMPLIANCE-OPEN-IF-DOES-NOT-EXIST
        return _py.open(namestring(pathname),
                        _poor_man_ecase(direction,
                                        (_keyword("INPUT"),  lambda: "r"),
                                        (_keyword("OUTPUT"), lambda: "w"),
                                        (_keyword("IO"),     lambda: "rw"),
                                        (_keyword("PROBE"),  lambda: _not_implemented("direction :PROBE"))))

@defun
def with_open_file(pathname, body, direction = _keyword("INPUT"), element_type = base_char,
                   if_exists = _keyword("ERROR"), if_does_not_exist = _keyword("ERROR"),
                   external_format = _keyword("DEFAULT")):
        return with_open_stream(open(pathname, direction, element_type, if_exists, if_does_not_exist, external_format),
                                body)

@defun
def probe_file(x):
        x = pathname(x)
        return _without_condition_system(
                lambda: truename(x) if _os.path.exists(namestring(x)) else nil,
                reason = "os.path.exists")

@defun
def truename(x):
        # Unregistered Issue COMPLIANCE-TRUENAME
        x = pathname(x)
        return namestring(x)

@defun
def file_length(stream):
        f = namestring(pathname(stream))
        return _os.path.getsize(f)

@defun
def file_write_date(pathspec):
        """Returns a universal time representing the time
at which the file specified by PATHSPEC was last written
(or created), or returns NIL if such a time cannot be determined. """
        f = namestring(pathname(stream))
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
        return _py.int(_os.path.getmtime(f))

def _file_name(x):
        return _values_frame_project(0, parse_namestring(the(file_stream, x).name))

# Stream types and functions

def open_stream_p(x):
        return not the(stream, x).closed

def input_stream_p(x):
        return open_stream_p(x) and x.readable()

def output_stream_p(x):
        return open_stream_p(x) and x.writable()

@defclass
class two_way_stream(stream.python_type):
        def __init__(self, input, output):
                self.input, self.output  = input, output
        def read(self, amount):
                return self.input.read(amount)
        def write(self, data):
                return self.output.write(data)
        def flush(self):
                return self.output.flush()
        def close(self):
                ## Unregistered Issue PROBABLE-PYTHON-BUG-TWO-WAY-STREAM-CLOSE-GETS-CALLED-WITHOUT-REASON
                # Repro?  Pretty strange as it is..
                #  1. Comment out the _debug_printf below.
                #  2. Uncomment the 'raise' line immediately below %STRING-SET calls.
                _debug_printf("For the love of all things living: about to close the funky streams!")
                self.output.close()
                self.input.close()
        def readable(self): return t
        def writable(self): return t

def make_two_way_stream(input, output):   return two_way_stream.python_type(input, output)
def two_way_stream_input_stream(stream):  return stream.input
def two_way_stream_output_stream(stream): return stream.output

_string_set("*DEBUG-IO*", make_two_way_stream(_symbol_value(_standard_input_), _symbol_value(_standard_output_)))
_string_set("*QUERY-IO*", make_two_way_stream(_symbol_value(_standard_input_), _symbol_value(_standard_output_)))
# raise simple_condition.python_type("Boo %s.", 2)

@defclass
class broadcast_stream(stream.python_type):
        def __init__(self, *streams):
                self.streams  = streams
        def write(self, data):
                for component in self.streams:
                        component.write(data)
        def flush(self):
                for component in self.streams:
                        component.flush()
        def readable(self): return nil
        def writable(self): return t

def make_broadcast_stream(*streams):  return broadcast_stream.python_type(*streams)
def broadcast_stream_streams(stream): return stream.streams

@defclass
class synonym_stream(stream.python_type):
        def __init__(self, symbol):
                self.symbol  = symbol
        def stream():
                return symbol_value(self.symbol)
        def read(self, amount):
                return self.stream().read(amount)
        def write(self, data):
                return self.stream().write(data)
        def flush(self):
                return self.stream().flush()
        def readable(self): return self.stream.readable()
        def writable(self): return self.stream.writable()

def make_synonym_stream(symbol):   return synonym_stream.python_type(symbol)
def synonym_stream_symbol(stream): return stream.symbol

def _coerce_to_stream(x):
        return (x                                if streamp(x) else
                _symbol_value(_standard_output_) if x is t else
                error("%s cannot be coerced to a stream.", x))

# Stream output functions and FORMAT

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
                        except _io.UnsupportedOperation as cond:
                                error(stream_type_error, "%s is not an %s stream: \"%s\".",
                                      stream, ("output" if cond.args[0] == "not writable" else
                                               "adequate"),
                                      cond.args[0])
                _without_condition_system(handler,
                                          reason = "_write_string")
        return string

def write_line(string, stream = t):
        return write_string(string + "\n", stream)

def finish_output(stream = t):
        check_type(stream, (or_, stream, (member, t, nil)))
        (stream is not nil) and _coerce_to_stream(stream).flush()

def force_output(*args, **keys):
        finish_output(*args, **keys)

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

# Earlified streaming

@defun
def stream_external_format(stream): return _keyword(stream.encoding)

@defun
def make_string_output_stream():
        return _io.StringIO()

@defun
def with_output_to_string(f):
        x = make_string_output_stream()
        try:
                f(x)
                return get_output_stream_string(x)
        finally:
                close(x)

@defun
def with_input_from_string(s, f):
        x = make_string_input_stream(s)
        try:
                return f(x)
        finally:
                close(x)

@defun
def get_output_stream_string(x):
        return x.getvalue()

@defun
def make_string_input_stream(x):
        return _io.StringIO(x)

@defun
def close(x):
        x.close()

@defun
def file_position(x):
        return x.seek(0, 1)

@defun
def setf_file_position(posn, x):
        return x.seek(posn)

def _stream_as_string(stream):
        return stream.read()

def _file_as_string(filename):
        with _py.open(filename, "r") as f:
                return _stream_as_string(f)

# Pathnames

#     Relevant sections:

#     - 19.2.2.1.2 Case in Pathname Components          :: http://clhs.lisp.se/Body/19_bbab.htm
#     - 19.2.2.1.2.1 Local Case in Pathname Components  :: http://clhs.lisp.se/Body/19_bbaba.htm
#     - 19.2.2.1.2.2 Common Case in Pathname Components :: http://clhs.lisp.se/Body/19_bbabb.htm

def _namestring_components(x):
        dirname, basename = _os.path.split(x)
        return _if_let(position(".", basename, from_end = t),
                       lambda dotpos: (dirname or nil, basename[:dotpos], basename[dotpos + 1:]),
                       lambda:        (dirname or nil, basename or nil,   nil))

@defclass
class pathname_host():
        def parse(self, x):
                ## Unregistered Issue COMPLIANCE-NAMESTRING-UNPARSING-NOT-REALLY-IMPLEMENTED
                dirname, basename, type = _namestring_components(the(string, x))
                directory = dirname.split(_os.sep) if dirname else nil
                return pathname.python_type(host      = _system_pathname_host,
                                            device    = nil,
                                            directory = directory and (([_keyword("ABSOLUTE")] + directory[1:]) if directory[0] == "" else
                                                                       ([_keyword("RELATIVE")] + directory)),
                                            name      = basename,
                                            type      = type,
                                            version   = _keyword("NEWEST") if x else nil)
        def unparse(self, x):
                return ((x.device or "") +
                        (_os.sep                                   if x.directory and x.directory[0] is _keyword("ABSOLUTE") else "") +
                        ((_os.sep.join(x.directory[1:]) + _os.sep) if x.directory                                            else "") +
                        ("*"    if x.name is _keyword("WILD") else
                         x.name if x.name is not nil          else
                         "") +
                        (("." + ("*" if x.type is _keyword("WILD") else x.type)) if x.type is not nil else ""))
        def local_case(self, x): return self.localise_case(x)
        def common_case(self, x):
                return (self.customiser     if x.isupper() else
                        self.anticustomiser if x.islower() else
                        identity)(x)
        def apply_case(self, case, x):
                return (self.local_case(x)  if case is _keyword("LOCAL")  else
                        self.common_case(x) if case is _keyword("COMMON") else
                        error("Invalid case transform specifier: %s.  Must be one of either %s or %s.",
                              case, _keyword("LOCAL"), _keyword("COMMON")))

@defclass
class unix_host(pathname_host.python_type):
        localise_case              = identity
        customiser, anticustomiser = _py.str.lower, _py.str.upper

@defclass
class windows_host(pathname_host.python_type):
        localise_case              = identity
        customiser, anticustomiser = _py.str.lower, _py.str.upper

_system_pathname_host = make_instance(windows_host if _platform.system() == 'Windows' else
                                      unix_host)

_intern_and_bind_pynames("*DEFAULT-PATHNAME-DEFAULTS*")

@defclass
class pathname():
        def __init__(self, *args, host, device, directory, name, type, version):
                assert not args
                (self.host, self.device, self.directory, self.name, self.type, self.version) = host, device, directory, name, type, version
        def __str__(self):
                return "#P\"%s\"" % _py.repr(namestring(self))[1:-1]
        def __repr__(self):
                return self.__str__()

@defun
def pathnamep(x): return _py.isinstance(x, pathname.python_type)

## Unregistered Issue COMPLIANCE-HOST-TYPE-WRONG
@defun
def make_pathname(*args, host = None, device = None, directory = None, name = None, type = None, version = None,
                  default = None, case = _keyword("LOCAL")):
        assert not args
        default = default or pathname.python_type(**_defaulted_keys(
                        host = pathname_host(_symbol_value(_default_pathname_defaults_)),
                        device = nil, directory = nil, name = nil, type = nil, version = nil))
        effective_host = _defaulted(host, default.host)
        supplied_pathname = _py.dict(
                (k, effective_host.apply_case(case, v) if stringp(v) else v)
                for k, v in _only_specified_keys(host = host, device = device, directory = directory, name = name, type = type, version = version).items())
        ## Unregistered Issue RESEARCH-COMPLIANCE-MAKE-PATHNAME-CANONICALISATION
        return merge_pathnames(supplied_pathname, default)

@defun
def parse_namestring(thing, host = nil, default_pathname = None, *args, start = 0, end = nil, junk_allowed = nil):
        assert not args
        if junk_allowed:
                _not_implemented("%s", _keyword("JUNK-ALLOWED")) ## Unregistered Issue COMPLIANCE-PARSE-NAMESTRING-JUNK-ALLOWED-NOT-IMPLEMENTED
        if streamp(thing):
                thing = pathname(thing)
        if pathnamep(thing):
                return (_values_frame(thing, start) if not (host or thing.host) or host is thing.host else
                        error("The specified host %s does not match pathname's host %s.", host, thing.host))
        ## It is a string.
        check_type(thing, string)
        # Unregistered Issue COMPLIANCE-LOGICAL-PATHNAMES-NOT-IMPLEMENTED
        ## NI: If HOST is a logical host then THING is parsed as a
        ##     logical pathname namestring on the HOST.
        ## NI: If HOST is NIL and THING is a syntactically valid
        ##     logical pathname namestring containing an explicit
        ##     host, then it is parsed as a logical pathname
        ##     namestring.
        default_pathname = _defaulted_to_var(default_pathname, _default_pathname_defaults_)
        ## NI: If HOST is NIL, DEFAULT-PATHNAME is a logical pathname,
        ##     and THING is a syntactically valid logical pathname
        ##     namestring without an explicit host, then it is parsed
        ##     as a logical pathname namestring on the host that is
        ##     the host component of DEFAULT-PATHNAME.
        ##
        ## Now the haha part: "Otherwise, the parsing of thing is
        ## implementation-defined."
        ## We choose to follow SBCL.  Sort of.
        effective_host = (host or default_pathname.host)
        if not effective_host:
                error("Can't parse the namestring for an unspecified host: either %s or a %s specifying a pathname host must be provided.",
                      _keyword("HOST"), _keyword("DEFAULT-PATHNAME"))
        return _values_frame(effective_host.parse(subseq(thing, start, end) if start or end else thing),
                             start)

@defun
def namestring(x):
        p = pathname(x)
        if not p.host:
                error("Can't determine the namestring for pathnames with no host:\n  %s", p)
        ## Unregistered Issue COMPLIANCE-NAMESTRING-UNPARSING-NOT-REALLY-IMPLEMENTED
        return p.host.unparse(p)

@defun
def pathname(x):
        """pathname pathspec => pathname

Arguments and Values:

pathspec---a pathname designator.

pathname---a pathname.

Description:

Returns the pathname denoted by PATHSPEC.

If the PATHSPEC designator is a stream, the stream can be either open
or closed; in both cases, the PATHNAME returned corresponds to the
filename used to open the file.  PATHNAME returns the same pathname for
a file stream after it is closed as it did when it was open.

If the PATHSPEC designator is a file stream created by opening a
logical pathname, a logical pathname is returned."""
        return (x                                             if pathnamep(x)          else
                _values_frame_project(0, parse_namestring(x)) if stringp(x)            else
                _file_name(x)                                 if typep(x, file_stream) else
                error("PATHNAME only accepts pathnames, namestrings and file streams, was given: %s.", x))

@defun
def pathname_directory(x):
        # Unregistered Issue PORTABILITY-PATHNAME
        absp = the(string, x).startswith(_os.sep)
        return ([_keyword("absolute" if absp else "relative")] +
                # Reject the integer interpretation of booleans.
                _namestring_components(x)[0].split(_os.sep)[1 if absp else 0])

@defun
def pathname_name(x): return _namestring_components(x)[1]
@defun
def pathname_type(x): return _namestring_components(x)[2]

def _init_pathnames():
        _string_set("*DEFAULT-PATHNAME-DEFAULTS*",
                    _values_frame_project(0, parse_namestring(_os.getcwd() + "/",
                                                              host = _system_pathname_host,
                                                              default_pathname = t)))
        # T is a junk marker, but avoid a bootstrap loop

_init_pathnames()

# Sub-standard pathname functions

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
        default_pathname = _defaulted(default_pathname, _os.getcwd() + _os.sep)
        dir_supplied_p = _os.sep in pathname
        name_supplied_p = pathname and pathname[-1] != _os.sep
        dir_defaulted_p = _os.sep in default_pathname
        net_effect_if = name_supplied_p and not dir_supplied_p # Unregistered Issue COMPLIANCE-MERGE-PATHNAMES-SIMPLIFICATION
        if net_effect_if:
                return _os.path.join((default_pathname[:position(_os.sep, default_pathname, from_end = t) + 1] if dir_defaulted_p else ""),
                                    pathname)
        elif not name_supplied_p:
                pass
        return _os.path.join(x, y)

# Cold printer

def _print_string(x, escape = None, readably = None):
        """The characters of the string are output in order. If printer escaping
is enabled, a double-quote is output before and after, and all
double-quotes and single escapes are preceded by backslash. The
printing of strings is not affected by *PRINT-ARRAY*. Only the active
elements of the string are printed."""
        # XXX: "active elements of the string"
        # Issue ADJUSTABLE-CHARACTER-VECTORS-NOT-IMPLEMENTED
        readably = _defaulted_to_var(readably, _print_readably_)
        escape   = _defaulted_to_var(escape,   _print_escape_) if not readably else t
        return (x if not escape else
                ("\"" + _without_condition_system(
                                lambda: _re.sub(r"([\"\\])", r"\\\1", x),
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
                        lambda: format(s, "%d elements", _py.len(x)),
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
        array           = _defaulted_to_var(array,           _print_array_)
        base            = _defaulted_to_var(base,            _print_base_)
        case            = _defaulted_to_var(case,            _print_case_)
        circle          = _defaulted_to_var(circle,          _print_circle_)
        escape          = _defaulted_to_var(escape,          _print_escape_)
        gensym          = _defaulted_to_var(gensym,          _print_gensym_)
        length          = _defaulted_to_var(length,          _print_length_)
        level           = _defaulted_to_var(level,           _print_level_)
        lines           = _defaulted_to_var(lines,           _print_lines_)
        miser_width     = _defaulted_to_var(miser_width,     _print_miser_width_)
        pprint_dispatch = _defaulted_to_var(pprint_dispatch, _print_pprint_dispatch_)
        pretty          = _defaulted_to_var(pretty,          _print_pretty_)
        radix           = _defaulted_to_var(radix,           _print_radix_)
        readably        = _defaulted_to_var(readably,        _print_readably_)
        right_margin    = _defaulted_to_var(right_margin,    _print_right_margin_)
        # assert(t
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
                                max = _py.len(object)
                                if max:
                                        for i in _py.range(0, max):
                                                string += do_write_to_string(object[i])
                                                if i != (max - 1):
                                                        string += " "
                                string += ")"
                        elif symbolp(object):
                                # Honors *PACKAGE*, *PRINT-CASE*, *PRINT-ESCAPE*, *PRINT-GENSYM*, *PRINT-READABLY*.
                                # XXX: in particular, *PRINT-ESCAPE* is honored only partially.
                                string += _print_symbol(object)
                        elif integerp(object) or floatp(object):
                                string += _py.str(object)
                        elif object is False or object is None or object is True:
                                string += obj2lisp_xform[object]
                        elif _py.type(object).__name__ == "builtin_function_or_method":
                                string += "\"#<BUILTIN-FUNCTION-OR-METHOD %s 0x%x>\"" % (object.__name__, _py.id(object))
                        elif stringp(object):
                                # Honors *PRINT-ESCAPE* and *PRINT-READABLY*.
                                string += _print_string(object)
                        elif hash_table_p(object) or _setp(object):
                                # Honors *PRINT-ESCAPE* and *PRINT-READABLY*.
                                string += _print_unreadable_compound(object)
                        elif functionp(object):
                                string += _print_function(object)
                        elif (not escape) and typep(object, (or_, restart, condition)):
                                string += _py.str(object)
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

# Cold reader

_string_set("*READ-CASE*", _keyword("upcase"))

def parse_integer(xs, junk_allowed = nil, radix = 10):
        l = _py.len(xs)
        def hexcharp(x): return x.isdigit() or x in ["a", "b", "c", "d", "e", "f"]
        (test, xform) = ((_str.isdigit, identity)      if radix == 10 else
                         (hexcharp,    _py.float.fromhex) if radix == 16 else
                         _not_implemented("PARSE-INTEGER only implemented for radices 10 and 16."))
        for end in _py.range(0, l):
                if not test(xs[end]):
                        if junk_allowed:
                                end -= 1
                                break
                        else:
                                error("Junk in string \"%s\".", xs)
        return _int(xform(xs[:(end + 1)]))

def _read_symbol(x, package = None, case = None):
        # debug_printf("_read_symbol >%s<, x[0]: >%s<", x, x[0])
        case = _defaulted_to_var(case, _read_case_)
        name, p = ((x[1:], __keyword)
                   if x[0] == ":" else
                   _poor_man_let(x.find(":"),
                                 lambda index:
                                         (_if_let(find_package(x[0:index].upper()),
                                                  lambda p:
                                                          (x[index + 1:], p),
                                                  lambda:
                                                          error("Package \"%s\" doesn't exist, while reading symbol \"%s\".",
                                                                x[0:index].upper(), x))
                                          if index != -1 else
                                          (x, _coerce_to_package(package)))))
        return _intern(_case_xform(case, name), p)[0]

@__block__
def _cold_read_from_string(string, eof_error_p = t, eof_value = nil,
                           start = 0, end = None, preserve_whitespace = None):
        "Does not conform."
        # _here("from \"%s\"" % string)
        pos, end = start, (end or _py.len(string))
        def handle_short_read_if(test):
                # _here("< %s" % (test,))
                if test:
                        (error(end_of_file, "end of file on %s" % (make_string_input_stream(string),)) if eof_error_p else
                         return_from(_cold_read_from_string, eof_value))
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
                        if obj == _find_symbol(".", __cl)[0]:
                                error("Consing dot not implemented")
                # _here("< %s" % (obj,))
                return obj
        def skip_whitespace():
                nonlocal pos
                while string[pos] in _py.frozenset([" ", "\t", "\n"]):
                        pos += 1
        def read_list():
                nonlocal pos
                ret = []
                pos += 1
                while t:
                        skip_whitespace()
                        char = string[pos]
                        if char == ")":
                                pos += 1
                                break
                        else:
                                obj = read()
                                if not _listp(obj) and obj is _find_symbol(".", __cl)[0]:
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
                while t:
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
                if _without_condition_system(lambda: _re.match("^[0-9]+$", token),
                                             reason = "re.match"):
                        ret = _py.int(token)
                elif _without_condition_system(lambda: _re.match("^[0-9]+\\.[0-9]+$", token),
                                               reason = "re.match"):
                        ret = _py.float(token)
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
                while t:
                        if pos >= end:
                                break
                        char = string[pos]
                        if char in _py.set([" ", "\t", "\n", "(", ")", "\"", "'"]):
                                break
                        else:
                                token += char
                                pos += 1
                # _here("< %s" % token)
                return token
        ret = handler_case(read,
                           (IndexError,
                            lambda c: handle_short_read_if(t)))
        # _here("lastly %s" % (ret,))
        return ret
read_from_string = _cold_read_from_string

def read_line(stream = None, eof_error_p = t, eof_value = nil):
        stream = _defaulted_to_var(stream, _standard_input_)
        return handler_case(lambda: stream.readline(),
                            (error,
                             lambda c: error(end_of_file, "end of file on %s" % (stream,))))

def read_char(stream = None, eof_error_p = t, eof_value = nil, recursivep = nil):
        stream = _defaulted_to_var(stream, _standard_input_)
        ret = the(_global("stream"), stream).read(1)
        return (ret       if ret             else
                eof_value if not eof_error_p else
                error(end_of_file, "end of file on %s" % (stream,)))

def unread_char(x, stream = _sys.stdin):
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
                     lambda c: c == peek_type   if stringp(peek_type) and _py.len(peek_type) == 1 else
                     error("Invalid peek-type: '%s'.", peek_type))
        stream = _defaulted(input_stream, _symbol_value(_standard_input_))
        while t:
                char = read_char(stream, eof_error_p, eof_value, recursive_p)
                if criterion(char):
                        unread_char(char, stream)
                        return char

@__block__
def _cold_read(stream = _sys.stdin, eof_error_p = t, eof_value = nil, preserve_whitespace = None, recursivep = nil):
        ## Has not even a remote chance of conforming.
        def read_char_maybe_eof(): return read_char(stream, nil, nil)
        def read_inner():
                skip_whitespace()
                char = read_char(stream)
                unread_char(char, stream)
                # _here("> \"%s\", by \"%s\"" % (string[pos:], char))
                if   char == _py.chr(40):  obj = read_list() # Org is a bit too picky
                elif char == "\"":         obj = read_string()
                elif char == "'":          obj = read_quote()
                else:
                        # handle_short_read_if(pos > end)
                        obj = read_number_or_symbol()
                        if obj == _find_symbol(".", __cl)[0]:
                                error("Consing dot not implemented")
                        # _here("< %s" % (obj,))
                return obj
        def skip_until_eol():
                c = read_char_maybe_eof()
                while c and c != "\n":
                        c = read_char_maybe_eof()
        def skip_whitespace():
                while t:
                        c = read_char_maybe_eof()
                        if c == ";":
                                skip_until_eol()
                        elif c not in _py.frozenset([" ", "\t", "\n"]):
                                if c is not nil:
                                        unread_char(c, stream)
                                return
        def read_list():
                ret = []
                c = read_char(stream) # it's a #\(
                while t:
                        skip_whitespace()
                        char = read_char(stream)
                        if char == ")":
                                break
                        else:
                                unread_char(char, stream)
                                obj = read_inner()
                                if not _listp(obj) and obj is _find_symbol(".", __cl)[0]:
                                        error("Consing dot not implemented")
                                ret += [obj]
                # _here("< %s" % (ret,))
                return _py.tuple(ret)
        def read_string():
                ret = ""
                read_char(stream) # seek the opening double-quote
                while t:
                        char = read_char(stream)
                        if char == "\"":
                                break
                        elif char == "\\":
                                char2 = read_char(stream)
                                ret += (char2 if char2 in _py.set(["\"", "\\"]) else
                                        error("READ-FROM-STRING: unrecognized escape character \"%s\".", char2))
                        else:
                                ret += char
                # _here("< %s" % (ret,))
                return ret
        def read_number_or_symbol():
                token = read_token()
                if _without_condition_system(lambda: _re.match("^[0-9]+$", token),
                                             reason = "re.match"):
                        ret = _py.int(token)
                elif _without_condition_system(lambda: _re.match("^[0-9]+\\.[0-9]+$", token),
                                               reason = "re.match"):
                        ret = _py.float(token)
                else:
                        ret = _read_symbol(token)
                        # debug_printf("-- interned %s as %s", token, name)
                # _here("< %s" % ret)
                return ret
        def read_token():
                token = ""
                # _here(">> ..%s..%s" % (pos, end))
                while t:
                        char = read_char_maybe_eof()
                        if char in _py.set([nil, " ", "\t", "\n", "(", ")", "\"", "'"]):
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
                                      return_from(_cold_read, eof_value)))
        # _here("lastly %s" % (ret,))
        return ret
read = _cold_read

# Condition system

def _conditionp(x):
        return typep(x, condition)

def make_condition(type, *args, **keys):
        check_type(type, symbol)
        if not (_py.hasattr(type, "python_type") and
                _conditionp(type.python_type)):
                error("In MAKE-CONDITION: %s does not designate a condition type.", type)
        return type.python_type(*args, **keys)

def _report_handling_handover(cond, frame, hook):
        format(_sys.stderr, "Handing over handling of %s to frame %s\n",
               prin1_to_string(cond), _pp_chain_of_frame(frame, callers = 25))

__main_thread__ = _threading.current_thread()
def _report_condition(cond, stream = None, backtrace = None):
        stream = _defaulted_to_var(stream, _debug_io_)
        format(stream, "%sondition of type %s: %s\n",
               (("In thread \"%s\": c" % _threading.current_thread().name)
                if _threading.current_thread() is not __main_thread__ else
                "C"),
               _py.type(cond), cond)
        if backtrace:
                _backtrace(-1, stream)
        return t

def _maybe_reporting_conditions_on_hook(p, hook, body, backtrace = None):
        if p:
                old_hook_value = symbol_value(hook)
                def wrapped_hook(cond, hook_value):
                        "Let's honor the old hook."
                        _report_condition(cond,
                                          stream = _symbol_value(_debug_io_),
                                          backtrace = backtrace)
                        if old_hook_value:
                                old_hook_value(cond, old_hook_value)
                with env.maybe_let(p, **{string(hook): wrapped_hook}):
                        return body()
        else:
                return body()

__not_even_conditions__ = _py.frozenset([_py.GeneratorExit, _py.SystemExit, __catcher_throw__])
"A set of condition types which are entirely ignored by the condition system."

_intern_and_bind_pynames("*STACK-TOP-HINT*", "*TRACEBACK*", "*SIGNALLING-FRAME*")

def __cl_condition_handler__(condspec, frame):
        backtrace_printed = nil
        def continuation():
                nonlocal backtrace_printed
                type, raw_cond, traceback = condspec
                # _print_frames(_frames_calling(frame))
                def _maybe_upgrade_condition(cond):
                        "Fix up the shit routinely being passed around."
                        return ((cond, nil) if typep(cond, condition) else
                                (condspec[0](*([cond] if not sequencep(cond) or stringp(cond) else
                                               cond)), t))
                        # _poor_man_typecase(cond,
                        #                    (BaseException, lambda: cond),
                        #                    (str,       lambda: error_(cond)))
                cond, upgradedp = _maybe_upgrade_condition(raw_cond)
                if type_of(cond) not in __not_even_conditions__:
                        # _debug_printf("signalling %s", cond)
                        if upgradedp:
                                _backtrace()
                                backtrace_printed = t
                                _here("Condition Upgrader: %s of-type %s -> %s of-type %s",
                                      prin1_to_string(raw_cond), type_of(raw_cond),
                                      prin1_to_string(cond), type_of(cond),
                                      callers = 45, frame = _symbol_value(_stack_top_hint_))
                        with progv({_traceback_: traceback,
                                    _signalling_frame_: frame}): # These bindings are the deviation from the CL standard.
                                presignal_hook = _symbol_value(_presignal_hook_)
                                if presignal_hook:
                                        with progv({_presignal_hook_: nil}):
                                                presignal_hook(cond, presignal_hook)
                                signal(cond)
                                debugger_hook = _symbol_value(_debugger_hook_)
                                if debugger_hook:
                                        with progv({_debugger_hook_: nil}):
                                                debugger_hook(cond, debugger_hook)
                return cond
        signalling_frame = _caller_frame(caller_relative = 1)
        with progv({_stack_top_hint_: signalling_frame}):
                cond = _sys.call_tracing(continuation, _py.tuple())
        if type_of(cond) not in __not_even_conditions__:
                is_not_ball = type_of(cond) is not __catcher_throw__
                _here("In thread '%s': unhandled condition of type %s: %s%s",
                      _threading.current_thread().name, type_of(cond), princ_to_string(cond),
                      ("\n; Disabling CL condition system." if is_not_ball else
                       ""),
                      callers = 15, frame = signalling_frame)
                if not backtrace_printed:
                        _backtrace()
                if is_not_ball:
                        _frost.disable_pytracer()
                        try:
                                invoke_debugger(cond)
                        except error.python_type as debugger_cond:
                                _debug_printf("Failed to enter the debugger:\n%s\nHave a nice day!", debugger_cond)
                                _sys.stderr.flush()
                                _py.exit()
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
        #     _frost.pytracer_enabled_p() and condition_handler_active_p()
        # ..inlined for speed.
        if _frost.pytracer_enabled_p() and _frost.tracer_hook("exception") is __cl_condition_handler__:
                # Unregistered Issue HANDLER-BIND-CHECK-ABSENT
                with progv({_handler_clusters_: (_symbol_value(_handler_clusters_) +
                                                 [handlers + (("__frame__", _caller_frame()),)])}):
                        return no_error(fn())
        else:
                # old world case..
                # format(t, "crap FAIL: pep %s, exhook is cch: %s",
                #        _frost.pytracer_enabled_p(), __tracer_hooks__.get("exception") is __cl_condition_handler__)
                if _py.len(handlers) > 1:
                        error("HANDLER-BIND: was asked to establish %d handlers, but cannot establish more than one in 'dumb' mode.",
                              _py.len(handlers))
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

# Built-in condition types

@defclass
class warning(condition.python_type):                                              pass
@defclass
class simple_warning(simple_condition.python_type, warning.python_type):           pass

@defclass
class style_warning(warning.python_type):                                          pass
@defclass
class simple_style_warning(simple_warning.python_type, style_warning.python_type): pass
@defun
def simple_style_warning(format_control, *format_arguments):
        warn(simple_style_warning, format_control = format_control, format_arguments = format_arguments)

@defclass
class type_error(error.python_type):                                               pass
@defclass
class simple_type_error(simple_error.python_type, type_error.python_type):         pass
@defun
def simple_type_error(format_control, *format_arguments):
        error(simple_type_error, format_control = format_control, format_arguments = format_arguments)

@defclass
class program_error(error.python_type):                                            pass
@defclass
class simple_program_error(simple_error.python_type, program_error.python_type):   pass
@defun
def simple_program_error(format_control, *format_arguments):
        error(simple_program_error, format_control = format_control, format_arguments = format_arguments)

@defclass
class undefined_function(error.python_type):
        def __init__(self, fname):
                self.name = fname
        def __str__(self):
                return "The function %s is undefined." % (self.name,)
        def __repr__(self):
                return self.__str__()

@defclass
class _not_implemented_condition(condition.python_type):
        def __init__(*args):
                self, name = args[0], args[1]
                self.name = name
        def __str__(self):
                return "Not implemented: " + self.name.upper()
        def __repr__(self):
                return self.__str__()
@defclass
class _not_implemented_error(_not_implemented_condition.python_type, error.python_type): pass
@defclass
class _not_implemented_warning(_not_implemented_condition.python_type, warning.python_type): pass

def _not_implemented(x = None):
        error(_not_implemented_error,
              x if x is not None else
              _caller_name())

def _warn_not_implemented(x = None):
        warn(_not_implemented_warning,
              x if x is not None else
              _caller_name())

# Restarts

@defclass
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
#                      _py.dict(interactive_function = lambda: compute_invoke_restart_interactively_args(),
#                               report_function      = lambda stream: print_restart_summary(stream),
#                               test_function        = lambda cond: visible_p(cond))))
_string_set("*RESTART-CLUSTERS*", [])

def _restartp(x):
        return typep(x, restart)

def restart_name(x):
        return x.name

def _specs_restarts_args(restart_specs):
        # format (t, "_s_r: %s", restart_specs)
        restarts_args = make_hash_table()
        for name, spec in restart_specs.items():
                function, options = ((spec[0], spec[1]) if _tuplep(spec) else
                                     (spec, make_hash_table()))
                restarts_args[name.upper()] = _updated_dict(options, _py.dict(function = function)) # XXX: name mangling!
        return restarts_args

##
# XXX: :TEST-FUNCTION is currently IGNORED!
##
def _restart_bind(body, restarts_args):
        with progv({_restart_clusters_: (_symbol_value(_restart_clusters_) +
                                           [_remap_hash_table(lambda _, restart_args: make_instance(restart, **restart_args), restarts_args)])}):
                return body()

def restart_bind(body, **restart_specs):
        return _restart_bind(body, _specs_restarts_args(restart_specs))

__valid_restart_options__ = _py.frozenset(["interactive", "report", "test", "function"])
def _restart_case(body, **restarts_args):
        def validate_restart_options(options):
                unknown = _py.set(options.keys()) - __valid_restart_options__
                return t if not unknown else error(simple_type_error, "Acceptable restart options are: (%s), not (%s)",
                                                   " ".join(__valid_restart_options__), " ".join(options.keys()))
        nonce = gensym("RESTART-CASE")
        wrapped_restarts_args = {
                restart_name: _poor_man_let(restart_args["function"],
                                  restart_args["interactive"] if "interactive" in restart_args else nil,
                                  restart_args["report"]      if "report"      in restart_args else nil,
                                  lambda function, interactive, report:
                                          (validate_restart_options(restart_args) and
                                           _updated_dict(restart_args,
                                                         _py.dict(name                 = restart_name,
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
                                             _py.dict(report = lambda stream: format(stream, "%s", description))) })

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
                for cluster in reversed(_symbol_value(_restart_clusters_)):
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
        restarts = _py.list()
        for cluster in _py.reversed(_symbol_value(_restart_clusters_)):
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

# DEFBODY

def _defbody_parse_ast(names, asts, valid_declarations = make_hash_table()):
        """Given a list of defined parameter NAMES, a list of statement ASTS and a
table of VALID-DECLARATIONS, return the body, documentation and declarations if any."""
        def _ast_call_to_name_p(name, x):
                return (typep(x, _ast.Expr)            and
                        typep(x.value, _ast.Call)      and
                        typep(x.value.func, _ast.Name) and
                        x.value.func.id == name)
        def ensure_valid_declarations(decls):
                # Unregistered Issue ENSURE-VALID-DECLARATION-SUGGESTS-FASTER-CONVERGENCE-TO-METASTRUCTURE
                def fail():
                        import more_ast
                        err("invalid declaration form: %s", more_ast.pp_ast_as_code(decls))
                def ensure_valid_declaration(decl):
                        typep(decl, _ast.Tuple) and decl.elts and typep(decl.elts[0], _ast.Name) or fail()
                        decl_name = decl.elts[0].id
                        if decl_name not in valid_declarations:
                                err("unknown declaration: %s", decl_name.upper())
                        n_decl_args = valid_declarations[decl_name]
                        if _py.len(decl.elts) < 1 + n_decl_args + 1:
                                err("invalid declaration %s: no parameter names specified", decl_name.upper())
                        every(_of_type(_ast.Name), decl.elts[1 + n_decl_args:]) or fail()
                        decl_param_names = _py.tuple(x.id for x in decl.elts[1 + n_decl_args:])
                        unknown_param_names = _py.set(decl_param_names) - _py.set(names)
                        if unknown_param_names:
                                err("invalid declaration %s: invalid parameter names: %s",
                                    decl_name.upper(), ", ".join(x.upper() for x in unknown_param_names))
                        return (decl_name,
                                _py.tuple(extract_sexp(x) for x in decl.elts[1:1 + n_decl_args]),
                                decl_param_names)
                not (decls.keywords or decls.starargs or decls.kwargs) or fail()
                return mapcar(ensure_valid_declaration, decls.args)
        def group_declarations(valid_declspecs, decls):
                def _declaration_names(x): return _py.set(x[1 + valid_declspecs[x[0]]:])
                return { name: _py.set(d[0:2] for d in decls
                                       if name in d[2:])
                         for name in _mapsetn(_declaration_names, decls) } # INDEXING..
        content, _ = _prefix_suffix_if(_not_of_type(_ast.Pass), asts)
        documentation, body = ((content[0].value.s, content[1:]) if (len(content) > 1 and
                                                                     typep(content[0], _ast.Expr) and
                                                                     typep(content[0].value, _ast.Str)) else
                               (nil, content))
        declarations, body = _prefix_suffix_if(_curry(_ast_call_to_name_p, "declare"), body)
        return body, documentation, group_declarations(valid_declarations,
                                                       mapcan(lambda dexcall: ensure_valid_declarations(dexcall.value),
                                                              declarations))

def _defbody_methods(desc, body_ast, method_name_fn, method_specs, arguments_ast = None):
        method_specs = _py.list((mspec if _tuplep(mspec) else
                                 (mspec, _defbody_make_required_method_error(desc))) for mspec in method_specs)
        def fail(x):
                import more_ast
                error("In %s: definition body may only contain definitions of %s methods, encountered: %s, an object of type %s", desc,
                      (", ".join([x.upper() for x, _ in method_specs[:-1]]) +
                       (" and " if _py.len(method_specs) > 1 else "") +
                       (method_specs[-1][0].upper() if method_specs else "")),
                      x if stringp(x) else more_ast.pp_ast_as_code(x), type_of(x))
        def process(method_name, default_maker):
                "Return a validated and normalised named method body 'return'-wise."
                x = find(method_name, body_ast, key = _slotting("name"))
                method_name = method_name_fn(method_name)
                if x:
                        x.name, x.args = method_name, _defaulted(arguments_ast, x.args)
                        ## recursively analyse RETURN-ality
                        def massage_tail(x, multiline):
                                if multiline:
                                        if hasattr(x.body[-1], "body"):
                                                massage_tail(x.body[-1], True)
                                        elif not typep(x.body[-1], _ast.Return):
                                                error("In %s: multi-line methods must include an explicit terminating Return statement", desc)
                                elif not(typep(x.body[0], _ast.Return)):
                                        if _py.hasattr(x.body[0], "value"):
                                                x.body[0] = _ast.Return(x.body[0].value)
                                        elif _py.hasattr(x.body[0], "body"):
                                                massage_tail(x.body[0], _py.len(x.body[0].body) > 1)
                                        else:
                                                error("In %s: tail-positioned form %s has no redeeming qualities what so ever.",
                                                      desc, type_of(x.body[0]))
                        massage_tail(x, _py.len(x.body) > 1)
                return _ast_compiled_name(method_name, x or default_maker(method_name),
                                          locals = _py.locals(), globals = _py.globals())
        ##
        body_ast = remove_if(_of_type(_ast.Pass), body_ast) # Remove noise.
        non_fdefns = find_if_not(_of_type(_ast.FunctionDef), body_ast)
        if non_fdefns:
                fail(non_fdefns)
        specified_method_names = { x.name:x for x in body_ast }
        invalid_methods = _py.set(specified_method_names) - _mapset(_indexing(0), method_specs)
        if invalid_methods:
                fail(invalid_methods.pop().upper())
        return (process(*mspec) for mspec in method_specs)

def _defbody_make_required_method_error(desc):
        return lambda method_name: error("In %s: missing method %s.", desc, _py.str(method_name).upper())

# AST basics

def _astp(x):        return typep(x, _ast.AST)

def _coerce_to_ast_type(type):
        return _poor_man_typecase(type,
                                  (_py.type, lambda: (type if subtypep(type, _ast.AST) else
                                                      error("Provided type %s is not a proper subtype of _ast.AST.", type))),
                                  (string,   lambda: (_ast.__dict__[type] if type in _ast.__dict__ else
                                                      error("Unknown AST type '%s'.", type))),
                                  (t,        lambda: error("Invalid AST type specifier: %s, %s, %s.", type, _py.type, typep(type, _py.type))))

def _text_ast(text):
        return _py.compile(text, "", 'exec', flags = _ast.PyCF_ONLY_AST).body

def _function_ast(fn):
        fn_ast = _text_ast(_without_condition_system(lambda: _inspect.getsource(fn)))[0]
        return fn_ast.args, fn_ast.body

def _function_body_pass_p(fn):
        fn_body_ast = _function_ast(fn)[1]
        return _py.len(fn_body_ast) == 1 and typep(fn_body_ast[0], _ast.Pass)

### literals
def _ast_num(n):
        return _ast.Num(n = the(integer, n))
def _ast_bool(n):
        return _ast.Bool(n = the(integer, n))
def _ast_string(s):
        return _ast.Str(s = the(string, s))
def _ast_set(xs,   writep = nil):
        return _ast.Set(elts   = the((pylist, _ast.AST), xs), ctx = _ast_rw(writep))
def _ast_list(xs,  writep = nil):
        return _ast.List(elts  = the((pylist, _ast.AST), xs), ctx = _ast_rw(writep))
def _ast_tuple(xs, writep = nil):
        return _ast.Tuple(elts = the((pylist, _ast.AST), xs), ctx = _ast_rw(writep))

################################# recurse? AST-ifier
__astifier_map__ = { _py.str:     (nil, _ast_string),
                     _py.int:     (nil, _ast_num),
                     _py.bool:    (nil, _ast_num),
                     _NoneType:   (nil, lambda x: _ast_name("None")),
                     _py.list:    (t,   _ast_list),
                     _py.tuple:   (t,   _ast_tuple),
                     _py.set:     (t,   _ast_set),
                     ## symbol: see below
                     }
def _register_astifier_for_type(type, recurse, astifier):
        "Please, list the added astifiers above."
        __astifier_map__[type] = (recurse, astifier)

def _astifiable_p(x):
        return _py.type(x) in __astifier_map__

def _try_astify_constant(x):
        if _astp(x):
                return x, t
        (rec, astifier), astifiable = gethash(type_of(x), __astifier_map__,
                                              ((nil, nil), nil))
        return (astifier(mapcar(lambda x: _astify_constant(x), x) if rec else
                         x), t) if astifiable else (None, None)

def _astify_constant(x):
        ast, successp = _try_astify_constant(x)
        return (ast if successp else
                error("Cannot convert value %s to _AST.  Is it a literal?",
                      prin1_to_string(x)))

def _coerce_to_ast(x):
        return _astify_constant(x) if not _astp(x) else x

### expressions
def _ast_alias(name):                        return _ast.alias(name = the(string, name), asname = None)
def _ast_keyword(name, value):               return _ast.keyword(arg = the(string, name), value = the(_ast.expr, value))

def _ast_rw(writep):                         return (_ast.Store() if writep else _ast.Load())
def _ast_name(name, writep = nil):           return _ast.Name(id = the(string, name), ctx = _ast_rw(writep))
def _ast_attribute(x, name, writep = nil):   return _ast.Attribute(attr = name, value = x, ctx = _ast_rw(writep))
def _ast_attribute_chain(xs, writep = nil):  return reduce((lambda acc, attr: _ast_attribute(acc, attr, writep)),
                                                           xs[1:],
                                                           _ast_name(xs[0], writep))
def _ast_index(of, index, writep = nil):     return _ast.Subscript(value = of, slice = _ast.Index(value = index), ctx = _ast_rw(writep))
def _ast_maybe_normalise_string(x):          return (_ast_string(x) if stringp(x) else x)

def _ast_funcall(name, args = [], keys = {}, starargs = None, kwargs = None):
        check_type(args, (pylist, (or_, _ast.AST, _NoneType, (satisfies, _astifiable_p))))
        return _ast.Call(func = (_ast_name(name) if stringp(name) else name),
                        args = mapcar(_coerce_to_ast, args),
                        keywords = _maphash(_ast_keyword, keys),
                        starargs = starargs or None,
                        kwargs = kwargs or None)

### statements
def _ast_Expr(node):
        return _ast.Expr(value = the(_ast.expr, node))

def _ast_module(body, lineno = 0):
        return _ast.Module(body = the((pylist, _ast.AST), body),
                          lineno = lineno)

def _ast_import(*names):
        return _ast.Import(names = mapcar(ast_alias, the((pylist, string), names)))
def _ast_import_from(module_name, names):
        return _ast.ImportFrom(module = the(string, module_name),
                              names = mapcar(_ast_alias, the((pylist, string), names)),
                              level = 0)

def _ast_assign(to, value):
        return _ast.Assign(targets = the((pylist, _ast.AST), to),
                          value = the(_ast.AST, value))
def _ast_return(node):
        return _ast.Return(value = the(_ast.AST, node))

# A very primitive AST-friendly lambda list mechanism + FunctionDef node creator

# arguments = (arg* args, identifier? vararg, expr? varargannotation,
#              arg* kwonlyargs, identifier? kwarg,
#              expr? kwargannotation, expr* defaults,

#              expr* kw_defaults)
# arg = (identifier arg, expr? annotation)
# keyword = (identifier arg, expr value)
def _function_lambda_list(fn, astify_defaults = t):
        "Returns: FIXPARMS, OPTIONAL-WITH-DEFAULTS, VARARGS, KEYS-WITH-DEFAULTS, KWARGS."
        return _argspec_lambda_spec(_inspect.getfullargspec(fn), astify_defaults = astify_defaults)

def _argspec_nfixargs(paramspec):
        return _py.len(paramspec.args) - _py.len(paramspec.defaults or []) # ILTW Python implementors think..

def _argspec_lambda_spec(spec, astify_defaults = t):
        # args, varargs, varkw, defaults, kwonlyargs, kwonlydefaults, annotations
        nfixargs = _argspec_nfixargs(spec)
        default_xform = _astify_constant if astify_defaults else identity
        return (spec.args[:nfixargs],
                _py.list(_py.zip(spec.args[nfixargs:],
                                 mapcar(default_xform, spec.defaults or []))),
                spec.varargs,
                _py.list(_py.zip(spec.kwonlyargs,
                         mapcar(default_xform, spec.kwonlydefaults or []))),
                spec.varkw)
def _lambda_spec_arguments(lambda_list_spec):
        fixed, optional, args, keyword, keys = lambda_list_spec
        return _ast.arguments(args        = mapcar(lambda x: _ast.arg(x, None),
                                                  fixed + mapcar(lambda x: x[0], optional)),
                             defaults    = mapcar(lambda x: x[1], optional),
                             vararg      = args,
                             kwonlyargs  = mapcar(lambda x: _ast.arg(x, None),
                                                  mapcar(lambda x: x[0], keyword)),
                             kw_defaults = mapcar(lambda x: x[1], keyword),
                             kwarg       = keys,
                             varargannotation = None,
                             kwargannotation  = None)

def _ast_functiondef(name, lambda_list_spec, body):
        fixed, optional, args, keyword, keys = lambda_list_spec
        return _ast.FunctionDef(
                name = the(string, name),
                args = _lambda_spec_arguments(lambda_list_spec),
                lineno = 0,
                decorator_list = [],
                returns = None,
                body = _poor_man_etypecase(body,
                                           ((pylist, _ast.AST),
                                            body),
                                           (function,
                                            lambda:
                                                    body(*mapcar(_ast_name, fixed),
                                                          **_map_into_hash(lambda x: (x, _ast_name),
                                                                           (_py.list(optional) + _py.list(keyword) +
                                                                            ([args] if args else []) +
                                                                            ([keys] if keys else [])))))))

# AST -> SEX

def _literal_ast_sex(ast_):
        def fail(sex):
                import more_ast
                error("Invalid sexp: %s.", more_ast.pp_ast_as_code(sex))
        return (ast_.id                                       if _py.isinstance(ast_, _ast.Name)  else
                ast_.n                                        if _py.isinstance(ast_, _ast.Num)   else
                _py.tuple(extract_sexp(x) for x in ast_.elts) if _py.isinstance(ast_, _ast.Tuple) else
                fail(ast_))

def _read_ast(x):
        def rec(x):
                return (x.n                               if _py.isinstance(x, _ast.Num)   else
                        x.s                               if _py.isinstance(x, _ast.Str)   else
                        _intern(_frost.python_name_lisp_symbol_name(x.id))[0]
                                                          if _py.isinstance(x, _ast.Name)  else
                        _py.tuple(rec(e) for e in x)      if _py.isinstance(x, _py.list)   else
                        _py.tuple(rec(e) for e in x.elts) if _py.isinstance(x, _ast.Tuple) else
                        _read_ast(x.value)                if _py.isinstance(x, _ast.Expr)  else
                        error("LISP: don't know how to intern value %s of type %s.", x, type_of(x)))
        with progv(# {_read_case_: _keyword("preserve")}
                   ):
                return rec(x)

# Rich AST definition machinery

_ast_info = _poor_man_defstruct("_ast_info",
                                "type",
                                "fields",     # each field is _py.dict(name, type, walk, [default])
                                "bound_free",
                                "nfixed")
__ast_walkable_field_types__ = _py.set([_ast.stmt, (pylist, _ast.expr), (maybe, _ast.expr),
                                        _ast.expr, (pylist, _ast.stmt)])
__ast_infos__ = make_hash_table()
def _find_ast_info(type):
        return __ast_infos__[_coerce_to_ast_type(type)]
def _ast_info_check_args_type(info, args, atreep = t):
        if len(args) < info.nfixed:
                error("AST type '%s' requires %s %d arguments, but only %d were provided: %s.",
                      info.type.__name__, "exactly" if len(info.fields) == info.nfixed else "at least", info.nfixed,
                      len(args), args)
        def check_arg_type(arg, type):
                def maybe_typespec_p(x): return typep(x, (pytuple, (eql, maybe), t))
                def list_typespec_p(x):  return typep(x, (pytuple, (eql, pylist), t))
                def simple_typespec_p(x):
                        return typep(x, (or_, (member, integer, string),
                                              (pytuple, (eql, maybe), (member, integer, string))))
                def atree_simple_typep(x, type):
                        return (_tuplep(x) and stringp(x[0]) and
                                _find_ast_info(x[0]) and _py.issubclass(_find_ast_info(x[0]).type, type))
                if atreep and not simple_typespec_p(type):
                        maybe_typep, list_typep, type = ((t,   nil, type[1]) if maybe_typespec_p(type) else
                                                         (nil,   t, type[1]) if list_typespec_p(type)  else
                                                         (nil, nil, type))
                        return (maybe_typep                                                       if arg is None else
                                _listp(arg) and every(lambda x: atree_simple_typep(x, type), arg) if list_typep  else
                                atree_simple_typep(arg, type))
                else:
                        return typep(arg, type)
        for i, (field, arg) in _py.enumerate(_py.zip(info.fields.values(), args)):
                if not check_arg_type(arg, field["type"]):
                        error("Argument %d (field %s) of AST '%s' must correspond to type %s, but was an instance of %s, instead: %s.",
                              i, _py.repr(field["name"]), info.type.__name__, field["type"], type_of(arg), _py.repr(arg))
        return t

def _ast_ensure_stmt(x):
        return x if typep(x, _ast.stmt) else _ast.Expr(the(_ast.AST, x))

# AST/Atree bound/free calculation

_intern_and_bind_pynames("*BOUND-FREE-RECURSOR*")

def _bound_free_recursor():
        return _symbol_value(_bound_free_recursor_)

def _ast_bound_free(astxs):
        def ast_rec(astxs):
                def bound_free(ast):
                        info = _find_ast_info(type_of(ast))
                        args = mapcar(_slot_of(ast), _py.type(ast)._fields)
                        _ast_info_check_args_type(info, args, atreep = nil)
                        return info.bound_free(*args)
                return _separate(3, bound_free, remove(None, _ensure_list(astxs)))
        with progv({_bound_free_recursor_: ast_rec}):
                return ast_rec(the((or_, _ast.AST, (pylist, _ast.AST)),
                                   astxs))

def _atree_bound_free(atreexs):
        def atree_rec(atreexs):
                def bound_free(atree):
                        check_type(atree, (partuple, string))
                        type = _coerce_to_ast_type(atree[0])
                        info = _find_ast_info(type)
                        args = atree[1:]
                        _ast_info_check_args_type(info, args, atreep = t)
                        return info.bound_free(*args)
                return _separate(3, bound_free, remove(None, _ensure_list(atreexs)))
        with progv({_bound_free_recursor_: atree_rec}):
                return atree_rec(the((or_, pytuple, (pylist, pytuple)),
                                     atreexs))

def _atree_bound(atree): return _atree_bound_free(atree)[0]
def _atree_free(atree):  return _atree_bound_free(atree)[1]
def _atree_xtnls(atree): return _atree_bound_free(atree)[2]

# @DEFAST

def defast(fn):
        ### generic tools
        def declaredp(grouped_decls, x, as_):
                return x in grouped_decls and (as_,) in grouped_decls
        def lambda_list_names(lambda_list, remove_optional = t):
                (fixed, optional, args, keyword, keys) = lambda_list
                xform = (lambda x: x[0]) if remove_optional else identity
                return (_py.tuple(fixed) +
                        _py.tuple(xform(x) for x in optional) + (_py.tuple() if not args else (args,)) +
                        _py.tuple(xform(x) for x in keyword)  + (_py.tuple() if not keys else (keys,)))
        ### end-of-generic-tools
        def validate_defast_name(name):
                if not name.startswith("_ast_"):
                        error("In DEFAST %s: the AST name must be prefixed with \"_ast_\"", name)
                name = name[5:]
                ast_type, therep = gethash(name, _ast.__dict__)
                if not therep:
                        error("In DEFAST %s: '%s' does not denote a known AST type", name, name)
                return name, ast_type
        name, ast_type = validate_defast_name(fn.__name__)
        def validate_defast_lambda_list(ast_type, lambda_list, annotations):
                (fixed, optional, args, keyword, keys) = lambda_list
                if args or keyword or keys:
                        err("only fixed and optional arguments are allowed")
                ast_field_names = fixed + mapcar((lambda x: x[0]), optional)
                ast_field_names_with_defaults = fixed + optional
                ast_field_types = mapcar(lambda name: annotations[name], ast_field_names)
                if _py.len(ast_field_types) != _py.len(ast_type._fields):
                        error("In DEFAST %s:the amount of provided type specifiers (%d) does not match the AST _fields: %s",
                              name, _py.len(ast_field_types), ast_type._fields)
                type_specifier_type = (or_, pytuple, _py.type, pytypename)
                if not every(_of_type(type_specifier_type), ast_field_types):
                        mismatched = find_if_not(_of_type(type_specifier_type), ast_field_types)
                        error("In DEFAST %s: the AST field type specifiers must be of type %s, found: %s",
                              name, type_specifier_type, mismatched)
                for i, (fname, ast_fname) in _py.enumerate(_py.zip(ast_field_names, ast_type._fields)):
                        if fname != ast_fname:
                                error("In DEFAST %s: the provided name for the %d'th field (%s) does not match its actual name (%s), expected field names: %s",
                                      name, i, fname, ast_fname, ast_type._fields)
                return ast_field_types
        def arglist_field_infos(parameters, nfix, with_defaults, ast_field_types, grouped_decls):
                fields = _without_condition_system(lambda: _collections.OrderedDict())
                def process_ast_field_arglist_entry(name, type, default, fixed = t):
                        walkp = (type in __ast_walkable_field_types__ or
                                 declaredp(grouped_decls, p, "walk"))
                        fields[p] = (_py.dict(name = name, type = type, walk = walkp) if fixed else
                                     _py.dict(name = name, type = type, walk = walkp, default = default))
                for p, type, defaulted in _py.zip(parameters[:nfix], ast_field_types[:nfix], with_defaults[:nfix]):
                        process_ast_field_arglist_entry(p, type, None,         fixed = t)
                for p, type, defaulted in _py.zip(parameters[nfix:], ast_field_types[nfix:], with_defaults[nfix:]):
                        process_ast_field_arglist_entry(p, type, defaulted[1], fixed = nil)
                return fields
        def make_default_bound_free(name, arguments_ast, fields):
                # compile down to:
                # return _separate(3, _ast_bound_free, [<fields>])
                # ..where <fields> is [ f["name"] for f in fields if f["walk"] ]
                return _ast.FunctionDef(
                        name, arguments_ast,
                        [_ast.Return(
                         _ast_funcall("_separate",
                                      [ 3,
                                        _ast_funcall("_bound_free_recursor", []),
                                        # _ast.Name("_ast_bound_free", _ast.Load()),
                                        _ast.List([ _ast.Name(f["name"], _ast.Load())
                                                    for f in fields.values()
                                                    if f["walk"] ],
                                                 _ast.Load()) ]))],
                        [], None)
        lambda_list = (fixed, optional, args, keyword, keys) = _function_lambda_list(fn, astify_defaults = nil)
        ast_field_types = validate_defast_lambda_list(ast_type, lambda_list, fn.__annotations__)
        parameters, with_defaults = (lambda_list_names(lambda_list),
                                     lambda_list_names(lambda_list, remove_optional = nil))
        args_ast, body_ast = _function_ast(fn)
        valid_declspecs = _py.dict(walk  = 0)
        body, documentation, declarations = _defbody_parse_ast(parameters, body_ast,
                                                               valid_declarations = valid_declspecs)
        fields = arglist_field_infos(parameters, _py.len(fixed), with_defaults, ast_field_types, declarations)
        [bound_free] = _defbody_methods("DEFAST " + name, body,
                                        lambda method: "_ast_%s_%s" % (name, method),
                                        [("bound_free", lambda name: make_default_bound_free(name, args_ast, fields))],
                                        arguments_ast = args_ast)
        # _debug_printf("bound_free for %s is %s", name, bound_free)
        __ast_infos__[ast_type] = _ast_info(type       = ast_type,
                                            fields     = fields,
                                            bound_free = bound_free,
                                            nfixed     = _py.len(fixed))

## AST + Symbols
_register_astifier_for_type(symbol.python_type, nil, (lambda sym: _ast_funcall("_find_symbol_or_fail",
                                                                               [symbol_name(sym)])))

# Rich AST definitions

# mod = Module(stmt* body)
#     | Interactive(stmt* body)
#     | Expression(expr body)
@defast
def _ast_Module(body: (pylist, _ast.stmt)): pass
@defast
def _ast_Interactive(body: (pylist, _ast.stmt)): pass
@defast
def _ast_Expression(body: _ast.expr): pass
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
def _ast_FunctionDef(name:            string,
                     args:            _ast.arguments,
                     body:           (pylist, _ast.stmt),
                     decorator_list: (pylist, _ast.expr) = _py.list(),
                     returns:        (maybe,  _ast.expr) = None):
        def bound_free():
                ((args_b, args_f, _),
                 (body_b, body_f, body_x),
                 (_,      deco_f, _),
                 (_,      retn_f, _)) = mapcar(_bound_free_recursor(), [args, body, decorator_list, returns])
                body_bound = _py.set([name]) | args_b | (body_b - body_x)
                body_free = body_f - body_bound
                body_xtnl_writes = body_b & body_x
                free = args_f | body_free | deco_f | retn_f
                return (body_xtnl_writes, # names declared global/nonlocal and assigned to
                        free,
                        _py.set())        # these do not escape..
#       | ClassDef(identifier name,
#                  expr* bases,
#                  keyword* keywords,
#                  expr? starargs,
#                  expr? kwargs,
#                  stmt* body,
#                  expr* decorator_list)
@defast
def _ast_ClassDef(name:            string,
                  bases:          (pylist, _ast.expr),
                  keywords:       (pylist, _ast.keyword),
                  starargs:       (maybe,  _ast.expr),
                  kwargs:         (maybe,  _ast.expr),
                  body:           (pylist, _ast.stmt),
                  decorator_list: (pylist, _ast.expr)):
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
                        _py.set())        # these do not escape..
#       | Return(expr? value)
@defast
def _ast_Return(value: (maybe, _ast.expr)): pass
#       | Delete(expr* targets)
@defast
def _ast_Delete(targets: (pylist, _ast.expr)): pass
        # targets do ref, in this case!
#       | Assign(expr* targets, expr value)
@defast
def _ast_Assign(targets: (pylist, _ast.expr),
                value:    _ast.expr):
        def bound_free():
                ((targ_b, targ_f, _),
                 (_,      valu_f, _)) = mapcar(_bound_free_recursor(), [targets, value])
                return (targ_b,
                        targ_f | valu_f,
                        _py.set())
#       | AugAssign(expr target, operator op, expr value)
@defast
def _ast_AugAssign(target: _ast.expr,
                   op:     _ast.operator,
                   value:  _ast.expr):
        def bound_free():
                ((targ_b, targ_f, _),
                 (_,      valu_f, _)) = mapcar(_bound_free_recursor(), [target, value])
                return (targ_b,
                        targ_f | valu_f,
                        _py.set())

def _ast_body_bound_free(body, more_bound = _py.set()):
        body_b, body_f, body_x = _bound_free_recursor()(body)
        bound = more_bound | (body_b - body_x)
        return (bound,
                body_f - bound,
                body_b & body_x)

#       | For(expr target, expr iter, stmt* body, stmt* orelse)
@defast
def _ast_For(target:  _ast.expr,
             iter:    _ast.expr,
             body:   (pylist, _ast.stmt),
             orelse: (pylist, _ast.stmt)):
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
def _ast_While(test:    _ast.expr,
               body:   (pylist, _ast.stmt),
               orelse: (pylist, _ast.stmt)):
        def bound_free():
                ((_, test_f, _)) = _bound_free_recursor()(test)
                # Unregistered Issue HOLE-ORELSE-CAN-USE-BODY-BINDINGS
                (bound, free, xtnls) = _separate(3, _ast_body_bound_free, [body, orelse])
                return (bound,
                        free | test_f,
                        xtnls)
#       | If(expr test, stmt* body, stmt* orelse)
@defast
def _ast_If(test:    _ast.expr,
            body:   (pylist, _ast.stmt),
            orelse: (pylist, _ast.stmt)):
        def bound_free():
                ((_, test_f, _)) = _bound_free_recursor()(test)
                # Unregistered Issue HOLE-ORELSE-CAN-USE-BODY-BINDINGS
                (bound, free, xtnls) = _separate(3, _ast_body_bound_free, [body, orelse])
                return (bound,
                        free | test_f,
                        xtnls)
#       | With(expr context_expr, expr? optional_vars, stmt* body)
@defast
def _ast_With(context_expr:   _ast.expr,
              optional_vars: (maybe, _ast.expr),
              body:          (pylist, _ast.stmt)):
        def bound_free():
                ((_,      ctxt_f, _),
                 (optl_b, optl_f, _)) = mapcar(_bound_free_recursor(), [context_expr, optional_vars])
                body_bound, body_free, body_xtnls = _ast_body_bound_free(body, optl_b)
                return (body_bound,
                        ctxt_f | optl_f | body_free,
                        body_xtnls)
#       | Raise(expr? exc, expr? cause)
@defast
def _ast_Raise(exc:   (maybe, _ast.expr),
               cause: (maybe, _ast.expr)): pass
#       | TryExcept(stmt* body, excepthandler* handlers, stmt* orelse)
@defast
def _ast_TryExcept(body:     (pylist, _ast.stmt),
                   handlers: (pylist, _ast.excepthandler),
                   orelse:   (pylist, _ast.stmt)):
        # Unregistered Issue HOLE-ORELSE-CAN-USE-BODY-BINDINGS
        def bound_free(): _separate(3, _ast_body_bound_free, [body, handlers, orelse])
#       | TryFinally(stmt* body, stmt* finalbody)
@defast
def _ast_TryFinally(body:      (pylist, _ast.stmt),
                    finalbody: (pylist, _ast.stmt)):
        # Unregistered Issue HOLE-ORELSE-CAN-USE-BODY-BINDINGS
        def bound_free(): _separate(3, _ast_body_bound_free, [body, handlers, orelse])
#       | Assert(expr test, expr? msg)
@defast
def _ast_Assert(test: _ast.expr,
                msg:  _ast.expr = None): pass
#       | Import(alias* names)
@defast
def _ast_Import(names: (pylist, _ast.alias)):
        declare((walk, names))
#       | ImportFrom(identifier? module, alias* names, int? level)
@defast
def _ast_ImportFrom(module: (maybe, string),
                    names:  (pylist, _ast.alias),
                    level:  (maybe, integer)):
        def bound_free():
                return (_bound_free_recursor()(names)[0],
                        _py.set([module] if module else []),
                        _py.set())
#       | Global(identifier* names)
@defast
def _ast_Global(names: (pylist, string)):
        def bound_free(): (_py.set(), _py.set(), _py.set(names))
#       | Nonlocal(identifier* names)
@defast
def _ast_Nonlocal(names: (pylist, string)):
        def bound_free(): (_py.set(), _py.set(), _py.set(names))
#       | Expr(expr value)
@defast
def _ast_Expr(value: _ast.expr): pass
#       | Pass | Break | Continue
@defast
def _ast_Pass(): pass
@defast
def _ast_Break(): pass
@defast
def _ast_Continue(): pass
# expr = BoolOp(boolop op, expr* values)
@defast
def _ast_BoolOp(op:      _ast.boolop,
                values: (pylist, _ast.expr)): pass
#      | BinOp(expr left, operator op, expr right)
@defast
def _ast_BinOp(left:  _ast.expr,
               op:    _ast.operator,
               right: _ast.expr): pass
#      | UnaryOp(unaryop op, expr operand)
@defast
def _ast_UnaryOp(op:      _ast.unaryop,
                 operand: _ast.expr): pass
#      | Lambda(arguments args, expr body)
@defast
def _ast_Lambda(args: _ast.arguments,
                body: _ast.expr):
        def bound_free():
                ((args_b, args_f, _),
                 (_,      body_f, _)) = mapcar(_bound_free_recursor(), [args, body])
                body_free = body_f - args_b
                free = args_f | body_free
                return (_py.set(),
                        free,
                        _py.set())
#      | IfExp(expr test, expr body, expr orelse)
@defast
def _ast_IfExp(test:   _ast.expr,
               body:   _ast.expr,
               orelse: _ast.expr): pass
#      | Dict(expr* keys, expr* values)
@defast
def _ast_Dict(keys:   (pylist, _ast.expr),
              values: (pylist, _ast.expr)): pass
#      | Set(expr* elts)
@defast
def _ast_Set(elts: (pylist, _ast.expr)): pass
#      | ListComp(expr elt, comprehension* generators)

def _ast_gchain_bound_free(xs, acc_binds):
        if xs:
                g_binds, g_free, _ = _bound_free_recursor()(xs[0])
                finbound, cfree = _ast_gchain_bound_free(xs[1:], acc_binds | g_binds)
                return finbound, (g_free - acc_binds) | cfree
        else:
                return acc_binds, _py.set()

def _ast_comprehension_bound_free(exprs, generators):
        gchain_bound, gchain_free = _ast_gchain_bound_free(generators, _py.set())
        _, exprs_f, _ = _separate(3, _bound_free_recursor(), exprs)
        return (_py.set(),
                gchain_free | (exprs_f - gchain_bound),
                _py.set())

@defast
def _ast_ListComp(elt:         _ast.expr,
                  generators: (pylist, _ast.comprehension)):
        def bound_free(): _ast_comprehension_bound_free([elt], generators)
#      | SetComp(expr elt, comprehension* generators)
@defast
def _ast_SetComp(elt:         _ast.expr,
                 generators: (pylist, _ast.comprehension)):
        def bound_free(): _ast_comprehension_bound_free([elt], generators)
#      | DictComp(expr key, expr value, comprehension* generators)
@defast
def _ast_DictComp(key:        _ast.expr,
                  value:      _ast.expr,
                  generators: (pylist, _ast.comprehension)):
        def bound_free(): _ast_comprehension_bound_free([key, value], generators)
#      | GeneratorExp(expr elt, comprehension* generators)
@defast
def _ast_GeneratorExp(elt:         _ast.expr,
                      generators: (pylist, _ast.comprehension)):
        def bound_free(): _ast_comprehension_bound_free([elt], generators)
#      | Yield(expr? value)
@defast
def _ast_Yield(value: (maybe, _ast.expr) = None): pass
#      | Compare(expr left, cmpop* ops, expr* comparators)
@defast
def _ast_Compare(left:         _ast.expr,
                 ops:         (pylist, _ast.cmpop),
                 comparators: (pylist, _ast.expr)): pass
#      | Call(expr func, expr* args, keyword* keywords, expr? starargs, expr? kwargs)
@defast
def _ast_Call(func:      _ast.expr,
              args:     (pylist, _ast.expr),
              keywords: (pylist, _ast.keyword),
              starargs: (maybe, _ast.expr) = None,
              kwargs:   (maybe, _ast.expr) = None):
        def bound_free(): _separate(3, _bound_free_recursor(),
                                    [func, args, keywords, starargs, kwargs])
#      | Num(object n) -- a number as a PyObject.
@defast
def _ast_Num(n: integer): pass
#      | Str(string s) -- need to specify raw, unicode, etc?
@defast
def _ast_Str(s: string): pass
#      | Bytes(string s)
@defast
def _ast_Bytes(s: string): pass
#      | Ellipsis
@defast
def _ast_Ellipsis(): pass
#      | Attribute(expr value, identifier attr, expr_context ctx)
@defast
def _ast_Attribute(value: _ast.expr,
                   attr:  string,
                   ctx:   _ast.expr_context): pass
#      | Subscript(expr value, slice slice, expr_context ctx)
@defast
def _ast_Subscript(value: _ast.expr,
                   slice: _ast.slice,
                   ctx:   _ast.expr_context):
        declare((walk, slice))
#      | Starred(expr value, expr_context ctx)
@defast
def _ast_Starred(value: _ast.expr,
                 ctx:   _ast.expr_context): pass
#      | Name(identifier id, expr_context ctx)
@defast
def _ast_Name(id:  string,
              ctx: _ast.expr_context):
        def bound_free(): ((_py.set(), _py.set([id])) if typep(ctx, (or_, _ast.Load, _ast.AugLoad, _ast.Param)) else
                           (_py.set([id]), _py.set()))
#      | List(expr* elts, expr_context ctx)
@defast
def _ast_List(elts: (pylist, _ast.expr),
              ctx:   _ast.expr_context): pass
#      | Tuple(expr* elts, expr_context ctx)
@defast
def _ast_Tuple(elts: (pylist, _ast.expr),
               ctx:   _ast.expr_context): pass
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
def _ast_Slice(lower: (maybe, _ast.expr) = None,
               upper: (maybe, _ast.expr) = None,
               step:  (maybe, _ast.expr) = None): pass
#       | ExtSlice(slice* dims)
@defast
def _ast_ExtSlice(dims: (pylist, _ast.slice)):
        declare((walk, dims))
#       | Index(expr value)
@defast
def _ast_Index(value: _ast.expr): pass
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
def _ast_comprehension(target: _ast.expr,
                       iter:   _ast.expr,
                       ifs:   (pylist, _ast.expr)):
        def bound_free():
                ((_,      targ_f, _),
                 (iter_b, iter_f, _),
                 (_,      iffs_f, _)) = mapcar(_bound_free_recursor(), [target, iter, ifs])
                return (iter_b,
                        ((targ_f | iffs_f) - iter_b) | iter_f,
                        _py.set())
# excepthandler = ExceptHandler(expr? type, identifier? name, stmt* body)
@defast
def _ast_ExceptHandler(type: (maybe, _ast.expr),
                       name: (maybe, string),
                       body: (pylist, _ast.stmt)):
        def bound_free():
                (_,      type_f, _) = _bound_free_recursor()(type)
                (bound, free, xtnls) = _ast_body_bound_free(body, _py.set([name] if name is not None else []))
                return (bound,
                        type_f | free,
                        xtnls)
# arguments = (arg* args, identifier? vararg, expr? varargannotation,
#              arg* kwonlyargs, identifier? kwarg,
#              expr? kwargannotation, expr* defaults,
#              expr* kw_defaults)
@defast
### These MAYBEs suggest a remapping facility.
def _ast_arguments(args:             (pylist, _ast.arg),
                   vararg:           (maybe, string),
                   varargannotation: (maybe, _ast.expr),
                   kwonlyargs:       (pylist, _ast.arg),
                   kwarg:            (maybe, string),
                   kwargannotation:  (maybe, _ast.expr),
                   defaults:         (pylist, _ast.expr),
                   kw_defaults:      (pylist, _ast.expr)):
        def bound_free():
                arg_bound, arg_free, _ = _separate(3, _bound_free_recursor(), [args, kwonlyargs])
                arg_bound |= _py.set(x for x in [vararg, kwarg]
                                     if x is not None)
                _, other_free, _ = _separate(3, _bound_free_recursor(), [varargannotation, kwargannotation, defaults, kw_defaults])
                return (arg_bound,
                        arg_free | other_free,
                        _py.set())
# arg = (identifier arg, expr? annotation)
@defast
def _ast_arg(arg:         string,
             annotation: (maybe, _ast.expr) = None):
        def bound_free(): (_py.set([arg]),
                           _bound_free_recursor()(annotation)[1],
                           _py.set())
# keyword = (identifier arg, expr value)
@defast
def _ast_keyword(arg:   string,
                 value: _ast.expr):
        def bound_free(): (_py.set([] if _nonep(asname) else [asname]),
                           _bound_free_recursor()(value),
                           _py.set())
# alias = (identifier name, identifier? asname)
@defast
def _ast_alias(name:    string,
               asname: (maybe, string) = None):
        def bound_free(): (_py.set([] if _nonep(asname) else [asname]),
                           _py.set(),
                           _py.set())

# Python value -> Atree

def _try_atreeify_list(xs):
        ret = []
        for x in xs:
                atree, atreeifiedp = _try_atreeify_constant(x)
                if not atreeifiedp:
                        return None, None
                ret.append(atree)
        return ret, t
__atreeifier_map__ = { _py.str:     (nil, lambda x: ("Str", x)),
                       _py.int:     (nil, lambda x: ("Num", x)),
                       _py.bool:    (nil, lambda x: ("Name", ("True" if x else "False"), ("Load",))),
                       _NoneType:   (nil, lambda x: ("Name", "None", ("Load",))),
                       _py.list:    (t,   lambda x: ("List", x, ("Load"))),
                       _py.tuple:   (t,   lambda x: ("Tuple", x, ("Load"))),
                       _py.set:     (t,   lambda x: ("Set", x)),
                       ## symbol: see below
                     }
def _register_atreeifier_for_type(type, recurse, atreeifier):
        "Please, list the added atreeifiers above."
        __atreeifier_map__[type] = (recurse, atreeifier)

def _atreeifiable_p(x):
        type = type_of(x)
        type_recipe, _ = gethash(type, __atreeifier_map__)
        if not type_recipe:
                return nil
        recursep, _ = type_recipe
        if not recursep:
                return t
        return every(_atreeifiable_p, x)

def _try_atreeify_constant(x):
        "It's more efficient to try doing it, than to do a complete check and then to 'try' again."
        (rec, atreeifier), atreeifiablep = gethash(type_of(x), __atreeifier_map__,
                                                   ((nil, nil), nil))
        if not atreeifiablep:
                return None, None
        if rec:
                atree, successp = _try_atreeify_list(x)
                return (atreeifier(atree), t) if successp else (None, None)
        return atreeifier(atree if rec else x), t

def _atreeify_constant(x):
        atree, successp = _try_atreeify_constant(x)
        return (atree if successp else
                error("Cannot atreeify value %s.  Is it a literal?",
                      prin1_to_string(x)))

# Atree -> AST

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
                ast_type = _ast.__dict__[type]
                info = _find_ast_info(ast_type)
                fields, finfos = _recombine((_py.list, _py.list), identity, _py.list(info.fields.items()))
                positional, optional = _prefix_suffix_if(lambda x: "default" in x, finfos)
                nfixed, defacto = _py.len(positional), _py.len(args)
                max = nfixed + _py.len(optional)
                if not (nfixed <= defacto <= max):
                        argument_count_error(nfixed, max, defacto, "AST type %s", type)
                effective_args = args + mapcar(_indexing("default"), optional[defacto - nfixed:])
                assert(_py.len(effective_args) == max)
                for val, name, finfo in _py.zip(effective_args, fields, finfos):
                        subtype = finfo["type"]
                        if not typep(val, subtype):
                                argument_type_error(name, subtype, val, "AST node %s", repr(type))
                return ast_type(*effective_args)
        ret =  (tree
                if typep(tree, (or_, string, integer, (eql, None))) else
                mapcar(_atree_ast, tree)
                if _listp(tree)                  else
                _try_astify_constant(tree)[0]
                if not _tuplep(tree)            else
                error("The atree nodes cannot be zero-length.")
                if not tree                     else
                error("The CAR of an atree must be a string, not %s.", tree[0])
                if not stringp(tree[0])         else
                unknown_ast_type_error(tree[0], tree) if tree[0] not in _ast.__dict__  else
                (astify_known(tree[0], mapcar(_atree_ast, _from(1, tree)))))
        return ret

# Atree tools

def _attr_chain_atree(xs):
        return reduce((lambda acc, attr: ("Attribute", acc, attr, ("Load",))),
                      xs[1:],
                      ("Name", xs[0], ("Load",)))

# Code

__constants__ = _py.dict()

def _do_defconstant(name, value, documentation = nil):
        if name in __constants__:
                error("The constant %s is being redefined (from %s to %s).", name, __constants__[name], value)
        __constants__[name] = value
        return name

_do_defconstant(t,   t)
_do_defconstant(nil, nil)
_do_defconstant(pi,  pi)

def _name_defined_as_constant_p(name):
        return name in __constants__

@defun
def constantp(form):
        ## See also: %COLD-CONSTANTP
        return (_py.isinstance(form, (_py.int, _py.float, _py.complex, _py.str)) or
                keywordp(form) or
                (symbolp(form) and form in __constants__) or
                (_tuplep(form) and _py.len(form) is 2 and form[0] is quote))

# Matcher

#       A large part of work is development of a calling convention.  Multiple values, as a
#       concept, is an important, but basic step in the general direction.

_intern_and_bind_pynames("%NAME", "%MAYBE")

def _maybe_destructure_binding(pat):
        return ((None, pat)               if not typep(pat, _py.dict) else
                _py.tuple(pat.items())[0] if _py.len(pat) == 1        else
                error_bad_pattern(pat))

def _error_bad_pattern(pat):
        raise Exception("Bad pattern: %s." % (pat,))

_intern_and_bind_pynames("%CALL", "%RETURN")

_trace_table = _py.dict()
def _do_tracep(*props):
        # _debug_printf("tracep %s", props)
        return props in _trace_table
def _no_trace(*_):            return False
_tracep = _do_tracep
def _trace(*props):
        global _tracep
        _tracep = _do_tracep
        if props:
                _trace_table[props] = t
def _untrace(*props):
        global _tracep
        if props:
                del _trace_table[props]
        else:
                _tracep = _no_trace

### Default trace settings:
_untrace()

def _trace_printf(tracespec, control, *args):
        if _tracep(*(tracespec                    if _tuplep(tracespec) else
                     (tracespec,)                 if stringp(tracespec) else
                     (tracespec, _caller_name(1)))):
                _debug_printf(control, *(args if not (_py.len(args) == 1 and functionp(args[0])) else
                                         args[0]()))

def _r(x, y, retval, q = "", n = 20, ignore_callers = _py.set(["<lambda>", "complex"])):
        def trace_args():
                frames = _py.reversed(
                        _py.list(_take(n, (f for f in _frames_calling(_caller_frame()) if _frame_fun_name(f) not in ignore_callers))))
                return (":".join(("%s_%s" % (_frame_fun_name(f),
                                             _frame_id(f)[:3]))
                                 for f in frames),
                        q, retval, x, y)
        _trace_printf(_return, "--- %s%3s\n   < %s   %s   %s", trace_args)
        return retval

### .prod
## segment:       coor <- crec <- succ <-
## match:         test <-
## match:         test <-
## typep:         test <-
## lax
#
### .crec
## segment:       coor <-
## match:         equo <-
#
### .comb
## crec:          succ <-

class _matcher():
        @staticmethod
        def bind(value, bound, name, if_exists:{error, replace} = error):
                def error_bound(x):
                        raise Exception("Rebinding %s from %s to %s." % (_py.repr(name), _py.repr(x), _py.repr(value)))
                def error_keyword(name, bad, allowed):
                        raise Exception("Keyword %s must be one of: %s.  Was: %s." % (_py.repr(name), allowed, _py.repr(bad)))
                if   name is None:            return bound
                elif (name not in bound or
                      if_exists is replace):
                        bound = _py.dict(bound)
                        bound.update({name:value}); return bound
                elif if_exists is error:     error_bound(bound[name])
                else:                        error_keyword("if_exists", if_exists, { error, replace })
        @staticmethod
        def succ(bound, res):      return bound, res, None
        @staticmethod
        def fail(bound, exp, pat): return bound, exp, pat
        @staticmethod
        def post(x, mutator):      return (x[0], mutator(x[1]), None) if x[2] is None else x
        ###
        def test(m, test, bound, name, resf:"() -> result", exp, fail_pat, if_exists:{error, replace} = error):
                return _r(test, "", (m.succ(m.bind(exp, bound, name, if_exists = if_exists), resf()) if test else
                                     m.fail(bound, exp, fail_pat)))
        def equo(m, name, exp, x):
                "Apply result binding, if any."
                b, r, f = x
                return ((m.bind(exp, b, name), r, f) if f is None else
                        x) # propagate failure as-is
        def coor(m, l0ret, lR, orig_tuple_p = False):
                l0b, l0r, l0f = l0ret
                if l0f is None: return m.succ(l0b, l0r)
                lRb, lRr, lRf = lR()
                if lRf is None: return m.succ(lRb, lRr)
                return m.fail(l0b, lRr, m.forc(l0f, lRf, orig_tuple_p))
        def crec(m, exp, l0, lR, horisontal = True, orig_tuple_p = False):
                ## Unregistered Issue PYTHON-LACK-OF-RETURN-FROM
                failpat, fx0, fxR, bound0, boundR = None, None, None, None, None
                def try_0():
                        nonlocal bound0, fx0, failpat
                        bound0, fx0, failpat = l0()
                        if failpat is None: return fx0
                def try_R():
                        nonlocal boundR, fxR, failpat
                        boundR, fxR, failpat = lR(bound0)
                        if failpat is None: return fxR
                result = (m.comh if horisontal else
                          m.comr)(try_0, try_R, orig_tuple_p)
                _trace_printf("yield", "+++ YIELD for %s (orig: %s, call: %s->%s):\n%s",
                              lambda: (exp, orig_tuple_p, _caller_name(2), _caller_name(1), result))
                return _r(exp, [fx0, fxR],
                          m.succ(boundR, result) if failpat is None else
                          m.fail(boundR or bound0, fx0 if fx0 is not None else fxR, failpat))
        ###
        def   register_simplex_matcher(m, name, matcher):     m.__simplex_patterns__[name] = matcher
        def   register_complex_matcher(m, name, matcher):     m.__complex_patterns__[name] = matcher
        def deregister_complex_matcher(m, name):          del m.__complex_patterns__[name]
        def simplex_pat_p(m, x): return _tuplep(x) and x and symbolp(x[0]) and x[0] in m.__simplex_patterns__
        def complex_pat_p(m, x): return _tuplep(x) and x and symbolp(x[0]) and x[0] in m.__complex_patterns__
        def simplex(m, bound, name, exp, pat, orifst):
                _trace_printf("simplex", "simplex  %s (call: %s->%s) %x  %10s  %20s\n -[] %s\n -() %s",
                              lambda: (pat[0], _caller_name(2), _caller_name(1), id(exp) ^ id(pat),
                                       name, bound, exp, pat))
                return _r(exp, pat[0],
                          m.__simplex_patterns__[pat[0]](bound, name, exp, pat, orifst))
        def complex(m, bound, name, exp, pat, orifst, aux, limit):
                _trace_printf("complex", "complex  %s (call: %s->%s) %x  %10s  %20s\n -[] %s\n -() %s\n -<> %s  %s  %s",
                              lambda: (pat[0][0], _caller_name(2), _caller_name(1), id(exp) ^ id(pat),
                                       name, bound, exp, pat, orifst, aux, limit))
                return _r(exp, pat[0][0],
                          m.__complex_patterns__[pat[0][0]](bound, name, exp, pat, orifst, aux, limit))
        def __init__(m):
                m.__complex_patterns__, m.__simplex_patterns__ = _py.dict(), _py.dict()
                m.register_complex_matcher(some, m.segment)
                m.register_complex_matcher(_maybe, m.maybe)
                m.register_complex_matcher(or_, m.or_)
        def complex_matcher_not_implemented(m, bound, name, exp, pat, orifst, aux, limit):
                raise Exception("Not yet capable of matching complex patterns of type %s.", pat[0][0])
        def simplex_matcher_not_implemented(m, bound, name, exp, pat, orifst):
                raise Exception("Not yet capable of matching simplex patterns of type %s.", pat[0])
        def identity(m, bound, name, exp, pat, orifst, aux, limit):
                ## Unregistered Issue IDENTITY-IGNORE-MATCHERS-COMPLEX/MATCH-USE-UNCLEAR
                return m.complex(bound, name, exp, pat[0][1:] + pat[1:], orifst, aux, limit)
        def ignore(m, bound, name, exp, pat, orifst, aux, limit):
                return m.match(bound, name, exp, pat[1:], (False, False), aux, limit)
        ###
        def segment(m, bound, name, exp, pat, orifst, aux, limit, end = None):
                def position(x, xs):
                        for i, ix in enumerate(xs):
                                if x == ix: return i
                def constant_pat_p(pat):
                        def nonconstant_pat_p(x): return _tuplep(x) or m.nonliteral_atom_p(x)
                        return not nonconstant_pat_p(_py.tuple(pat.items())[0][1] if typep(pat, _py.dict) else
                                                     pat)
                ## Unregistered Issue PYTHON-DESTRUCTURING-WORSE-THAN-USELESS-DUE-TO-NEEDLESS-COERCION
                seg_pat, rest_pat = pat[0][1:], pat[1:]
                firstp = aux is None
                aux = (seg_pat + ((some,) + seg_pat,)) if aux is None else aux # We'll MATCH against this
                _trace_printf("segment", "segment  %x  %10s  %20s\n -[] %s\n -() %s\n -<> %s  firstp:%s newaux:%s limit:%s",
                              lambda: (id(exp) ^ id(pat), name, bound, exp, pat, orifst, firstp, aux, limit))
                end = (end                        if end is not None                          else
                       position(rest_pat[0], exp) if rest_pat and constant_pat_p(rest_pat[0]) else
                       0)
                if ((end and end > _py.len(exp)) or ## no boundary variant fitted
                    end is None):                   ## a constant pattern was missing
                        return _r(exp, pat, m.fail(bound, exp, pat))
                cut = end if rest_pat else len(exp)
                seg_exp, rest_exp = (exp[0:cut], exp[len(exp) if cut is None else cut:])
                _trace_printf("segment", "segment  seg_exp:%s", seg_exp)
                return _r(exp, pat,
                          m.coor(m.crec(exp,
                                        lambda:
                                                ((lambda seg_bound, seg_r, seg_fail_pat:
                                                          m.test(seg_fail_pat is None, seg_bound, name, (lambda: seg_r), seg_exp, seg_fail_pat,
                                                                 if_exists = replace))
                                                 (*(m.succ(m.bind((), bound, name), m.prod((), orifst[0])) if seg_exp == () else
                                                    m.fail(bound, exp, pat)                                if limit == 0    else
                                                    ## Try biting one more iteration off seg_exp:
                                                    m.match(bound, name,  seg_exp,     aux,  (False,
                                                                                              firstp), aux, (limit - 1 if integerp(limit) else
                                                                                                             None))))),
                                        lambda seg_bound:
                                                m.match(seg_bound, None, rest_exp, rest_pat,  (False, False), None, None),
                                        orig_tuple_p = firstp and orifst[0] and seg_exp != (),
                                        horisontal = t),
                                 lambda: m.segment(   bound, name,      exp,      pat, orifst,   None,  limit,
                                                            end + 1)))
        def maybe(m, bound, name, exp, pat, orifst, aux, limit):
                ## The semantics of aux are painfully unclear here:
                ##  - we need to perform aux pass-through, for any potential surrounding segment match
                ##  - we need a clean slate for this segment..
                ## So, do we need a separate stack for aux here?  Sounds like an in inevitability..
                ## Unregistered Issue SEGMENT-MATCH-USERS-REQUIRE-AUX-DOMAIN-SEPARATION
                return _r(exp, pat,
                          m.segment(bound, name, exp, ((some,) + pat[0][1:],) + pat[1:], orifst, None, 1))
        def or_(m, bound, name, exp, pat, orifst, aux, limit):
                alternatives = pat[0][1:]
                def fail():
                        return m.fail(bound, exp, pat[0])
                def rec(head, tail):
                        return m.coor(m.match(bound, name, exp, (head,) + pat[1:], orifst, None, None),
                                      ## Unregistered Issue MATCHER-OR-FAILED-EXPRESSION-TOO-UNSPECIFIC
                                      lambda: (fail() if not tail else
                                               rec(tail[0], tail[1:])))
                return (fail() if not alternatives else
                        rec(alternatives[0], alternatives[1:]))
        ## About the vzy33c0's idea:
        ## type-driven variable naming is not good enough, because:
        ## 1. type narrows down the case analysis chain (of which there is a lot)
        ## 2. expressions also need typing..
        def match(m, bound, name, exp, pat, orifst, aux, limit):
                def maybe_get0Rname(pat):
                        ## Unregistered Issue PYTHON-DESTRUCTURING-WORSE-THAN-USELESS-DUE-TO-NEEDLESS-COERCION
                        (name, pat0), patR = _maybe_destructure_binding(pat[0]), pat[1:]
                        return (name, pat0, name and m.simplex_pat_p(pat0), patR,
                                (((pat0,) + patR) if name is not None else
                                 pat)) ## Attempt to avoid consing..
                ## I just caught myself feeling so comfortable thinking about life matters,
                ## while staring at a screenful of code.  In "real" life I'd be pressed by
                ## the acute sense of time being wasted..
                atomp, null = not _tuplep(pat), pat == ()
                _trace_printf("match", "       _match  %x  %10s  %20s\n -[] %s\n -() %s\n -<> %s  %s  %s, atomp: %s, null: %s, complexp: %s, simplexp: %s",
                              lambda: (id(exp) ^ id(pat), name, bound, exp, pat, orifst, aux, limit,
                                       atomp, null,
                                       (not (atom or null)) and m.complex_pat_p(pat[0]),
                                       (not (atom or null)) and m.simplex_pat_p(pat[0])))
                return \
                    _r(exp, pat,
                       (m.test((m.atom(exp, pat) if atomp else
                                exp == ()),
                               bound, name, lambda: m.prod(exp, orifst[0]), exp, pat)     if atomp or null        else
                        m.simplex(bound, name, exp,  pat, (_tuplep(exp),
                                                           orifst[1]))              if m.simplex_pat_p(pat) else
                        m.complex(bound, name, (exp,), (pat,), orifst, None, limit) if m.complex_pat_p(pat) else
                        (lambda pat0name, pat0, pat0simplexp, patR, clean_pat:
                                 (m.equo(name, exp,
                                         m.complex(bound, pat0name, exp, clean_pat, orifst, aux, limit))
                                  if m.complex_pat_p(pat0)         else
                                  m.fail(bound, exp, pat)   if not _tuplep(exp) or exp == () else # pat tupleful, exp tupleful
                                  m.equo(name, exp,
                                         m.crec(exp,
                                                lambda:        m.match(bound, pat0name, exp[0],  pat0, (_tuplep(exp[0]),
                                                                                                        orifst[1]),
                                                                       None, None),
                                                (lambda b0und: m.match(b0und, None,     exp[1:], patR, (False, orifst[1]), aux, limit)),
                                                orig_tuple_p = orifst[0],
                                                horisontal = nil))))
                        (*maybe_get0Rname(pat))))

def _match(matcher, exp, pat):
        name, prepped = _maybe_destructure_binding(matcher.preprocess(pat))
        return matcher.match(_py.dict(), name, exp, prepped, (True, False), None, None)

# Functions (in the sense of result of compilation)

_fns = make_hash_table()

@defclass
class _fn():
        ## Omnidefined:
        # name
        # arglist
        # body
        # globalp
        # args_types    - a tuple of types
        #
        ## Optional (can be None):
        # values_types  - T or a list of types
        # effects       - T or NIL
        # affected      - T or NIL
        def __init__(self, name,
                     arglist = None, body = None,
                     globalp = t,
                     args_types = None, values_types = None,
                     effects = None, affected = None):
                check_type(arglist, _py.tuple)
                args_types = _py.tuple(t for _ in arglist)
                check_type(name, symbol)
                if name in _fns:
                        error("Asked to overwrite FN record %s.", name)
                _attrify_args(self, locals(), "name",
                              "arglist", "args_types", "values_types",
                              "effects", "affected")
                self.dependents, self.dependencies = (make_hash_table(default_constructor = _py.set),
                                                      make_hash_table(default_constructor = _py.set))
                if globalp:
                        _fns[name] = self
        def add_dependent(self, reason, depee):
                self.dependents[reason].add(depee)
                depee.dependencies[reason].add(self)
        def map_dependents(self, fn):
                for reason, depees in self.dependents:
                        for d in depees:
                                fn(d, reason)
        def clear_dependencies(self):
                for reason, deps in self.dependencies.items():
                        for d in deps:
                                d.dependents[reason].remove(self)
                                if not d.dependents[reason]:
                                       del d.dependents[reason]
                self.dependencies.clear()

@defun
def _fn(name):
        return _fns.get(name, nil)

_string_set("*COMPILER-FN*", nil)

def _compiling_fn():
        return symbol_value(_compiler_fn_)

def _depend_on(fn_or_name, reason = t):
        fn = fn_or_name if functionp(fn_or_name) else _fn(fn_or_name)
        fn.add_dependent(reason, _compiling_fn())

def _ir_depending_on_function_properties(function_form, body, *prop_test_pairs):
        ## TODO: we should depend for the de-pessimisation sense likewise.
        ## ..or should we?  I think, at least for rechecking of conditions.. which is only possible
        ## in case of full recompilation..
        ##
        ## ..And we didn't even start to consider dependency loops..
        if symbolp(function_form):
                fn = _fn(function_form)
                if fn:
                        prop_vals = []
                        for prop_test in prop_test_pairs:
                                prop, test = (prop_test if _tuplep(prop_test) else
                                              (prop_test, constantly(t)))
                                val = _py.getattr(fn, prop)
                                if val is None or not test(val):
                                        return None
                                prop_vals.append(val)
                        for prop, _ in prop_test_pairs:
                                _depend_on(fn, prop)
                        return body(fn, *prop_vals)
        return None

# Generic pieces

def _parse_body(body, doc_string_allowed = t, toplevel = nil):
        doc = nil
        def doc_string_p(x, remaining_forms):
                return ((error("duplicate doc string %s", x) if doc else t)
                        if stringp(x) and doc_string_allowed and remaining_forms else
                        None)
        def declaration_p(x):
                return _tuplep(x) and x[0] is declare
        decls, forms = [], []
        for i, form in _py.enumerate(body):
               if doc_string_p(form, body[i:]):
                       doc = form
               elif declaration_p(form):
                       decls.append(form)
               else:
                       forms = body[i:]
                       break
        return (forms,
                decls,
                doc)

def _process_decls(decls, vars, fvars):
        _warn_not_implemented()

# Compiler conditions

@defclass
class _compiler_error(error.python_type):
        pass

@defclass
class _simple_compiler_error(simple_condition.python_type, _compiler_error.python_type):
        pass

@defun
def _compiler_error(control, *args):
        return _simple_compiler_error.python_type(control, *args)

# Compilation environment

_string_set("*IN-COMPILATION-UNIT*", nil)
_intern_and_bind_pynames("*ABORTED-COMPILATION-UNIT-COUNT*",
                         "*COMPILER-ERROR-COUNT*",
                         "*COMPILER-WARNINGS-COUNT*",
                         "*COMPILER-STYLE-WARNINGS-COUNT*",
                         "*COMPILER-NOTE-COUNT*",
                         "*UNDEFINED-WARNINGS*",
                         ##
                         "*TOP-COMPILATION-UNIT-P*",
                         "*SYMBOLS*")

def with_compilation_unit(fn, override = nil):
        """Affects compilations that take place within its dynamic extent. It is
intended to be eg. wrapped around the compilation of all files in the same system.

Following options are defined:

  :OVERRIDE Boolean-Form
      One of the effects of this form is to delay undefined warnings until the
      end of the form, instead of giving them at the end of each compilation.
      If OVERRIDE is NIL (the default), then the outermost
      WITH-COMPILATION-UNIT form grabs the undefined warnings. Specifying
      OVERRIDE true causes that form to grab any enclosed warnings, even if it
      is enclosed by another WITH-COMPILATION-UNIT.

Examples:

  ;; Prevent proclamations from the file leaking, and restrict
  ;; SAFETY to 3 -- otherwise uses the current global policy.
  (with-compilation-unit (:policy '(optimize))
    (restrict-compiler-policy 'safety 3)
    (load \"foo.lisp\"))

  ;; Using default policy instead of the current global one,
  ;; except for DEBUG 3.
  (with-compilation-unit (:policy '(optimize debug)
                          :override t)
    (load \"foo.lisp\"))

  ;; Same as if :POLICY had not been specified at all: SAFETY 3
  ;; proclamation leaks out from WITH-COMPILATION-UNIT.
  (with-compilation-unit (:policy nil)
    (declaim (optimize safety))
    (load \"foo.lisp\"))"""
        ## What do we use this for:
        ##
        ## 1. Self-evaluating names fixup.
        ## ...
        ## N-1. Warning/error summaries.
        def summarize_compilation_unit(failurep):
                _warn_not_implemented()
        succeeded_p = nil
        if _symbol_value(_in_compilation_unit_) and not override:
                try:
                        with progv({_top_compilation_unit_p_: nil}):
                                ret = fn()
                        succeeded_p = t
                        return ret
                finally:
                        if not succeeded_p:
                                _string_set("*ABORTED-COMPILATION-UNIT-COUNT*",
                                            _symbol_value(_aborted_compilation_unit_count_) + 1)
        else:
                with progv({_aborted_compilation_unit_count_: 0, # What is this for?
                            _compiler_error_count_: 0,
                            _compiler_warnings_count_: 0,
                            _compiler_style_warnings_count_: 0,
                            _compiler_note_count_: 0,
                            _undefined_warnings_: nil,
                            _in_compilation_unit_: t,
                            ##
                            _top_compilation_unit_p_: t,
                            _symbols_: _py.set(),
                            }):
                        try:
                               ret = fn()
                               succeeded_p = t
                               return ret
                        finally:
                                if not succeeded_p:
                                        _string_set("*ABORTED-COMPILATION-UNIT-COUNT*",
                                                    _symbol_value(_aborted_compilation_unit_count_) + 1)
                                summarize_compilation_unit(not succeeded_p)

def _compilation_note_symbol(x):
        _symbol_value(_symbols_).add(the(symbol, x))
        return t

def _compilation_symbols():
        return _symbol_value(_symbols_)

def _top_compilation_unit_p():
        return _symbol_value(_top_compilation_unit_p_)

def _compilation_unit_prologue():
        "This computes the prologue.  Supposed to be called once, at the end of all processing."
        def symbol_prologue():
                symbols = _compilation_symbols()
                # _debug_printf("prologue: %s", symbols)
                with progv({_symbols_: _py.set()}):
                        # for s in symbols:
                        #         _debug_printf("++++++++++++++++++++++++++++++++++++ emitting init for %s:\n%s\n\n",
                        #                       s, _lower((setq, s, (intern, symbol_name(s), package_name(symbol_package(s))))))
                        ## So, it becomes evident, that we need multiple values.
                        return mapcan((lambda s: _lower((setq, s, (intern, symbol_name(s), (find_package, package_name(symbol_package(s))))))[0]),
                                      symbols)
        return ((symbol_prologue() +
                 [])
                if _top_compilation_unit_p() else
                [])

# Code

_intern_and_bind_pynames("*LEXENV*",
                         "VARIABLE", "CONSTANT", "SPECIAL", "SYMBOL-MACRO", "MACRO", "COMPILER-MACRO", "BLOCK")
                         ## "FUNCTION"

class _nameuse():
        name, kind, type = None, None, None
        def __init__(self, name, kind, type, **attributes):
                _attrify_args(self, locals(), "name", "kind", "type")
def _nameusep(x):
        return _py.isinstance(x, _nameuse)

class _binding():
        value, shadows = None, None
        def __init__(self, value, shadows = nil, **attributes):
                _attrify_args(self, locals(), "value", "shadows")
def _bindingp(x):
        return _py.isinstance(x, _binding)

class _variable(_nameuse):
        def __init__(self, name, kind, type = t, dynamic_extent = nil, **attributes):
                check_type(kind, (member, variable, constant, special, symbol_macro)) # constant | special | symbol-macro | variable
                _nameuse.__init__(self, name, kind, type, **attributes)
                _attrify_args(self, locals(), "dynamic_extent") # t | nil
class _function(_nameuse):
        def __init__(self, name, kind, type = t, **attributes):
                check_type(kind, (member, function, macro, compiler_macro)) # macro | compiler-macro | function
                _nameuse.__init__(self, name, kind, type, **attributes)
class _block(_nameuse):
        def __init__(self, name, **attributes):
                check_type(kind, (eql, block)) # block
                _nameuse.__init__(self, name, block, t, **attributes)

def _nameuse_variablep(x): return _py.isinstance(x, _variable)
def _nameuse_functionp(x): return _py.isinstance(x, _function)
def _nameuse_blockp(x): return _py.isinstance(x, _block)

class _variable_binding(_variable, _binding):
        def __init__(self, name, kind, value, **attributes):
                _variable.__init__(self, name, kind, **attributes)
                _binding.__init__(self, value, **attributes)
class _function_binding(_function, _binding):
        def __init__(self, name, kind, value, **attributes):
                _function.__init__(self, name, kind, **attributes)
                _binding.__init__(self, value, **attributes)
class _block_binding(_block, _binding):
        def __init__(self, name, value, **attributes):
                _block.__init__(self, name, **attributes)
                _binding.__init__(self, value, **attributes)

def _variable_bindingp(x): return _py.isinstance(x, _variable_binding)
def _function_bindingp(x): return _py.isinstance(x, _function_binding)
def _block_bindingp(x):    return _py.isinstance(x, _block_binding)

@defclass(intern("%LEXENV")[0])
class _lexenv():
        """Chains variable and function scope pieces together.  Scope pieces map binding kinds
           to binding sets and bound names to bindings."""
        varscope, funcscope = nil, nil
        def __init__(self, parent = nil,
                     name_varframe = None, name_funcframe = None, name_blockframe = None,
                     kind_varframe = None, kind_funcframe = None, kind_blockframe = None,
                     full_varframe = None, full_funcframe = None, full_blockframe = None):
                # for k, v in self.data.items():
                #         symbolp(k)   or error("Lexenv scope keys must be symbols, found: %s.",    k.__repr__())
                #         _bindingp(k) or error("Lexenv scopt values must be bindings, found: %s.", v.__repr__())
                def complete_name_frame(framespec):
                        res = _collections.defaultdict(_py.set)
                        res["name"] = framespec
                        for binding in framespec.values():
                                res[binding.kind].add(binding)
                        return res
                def complete_kind_frame(framespec):
                        res = _py.dict(framespec)
                        res["names"] = names = _py.dict()
                        for set in framespec.values:
                                for binding in set:
                                        names[binding.name] = binding
                        return res
                def complete_frame(name, kind, full):
                        return (full                       if full else
                                complete_name_frame(name)  if name else
                                complete_kind_frame(frame) if kind else
                                None)
                def compute_scope(parent, sname, frame):
                        parent = (parent if _specifiedp(parent) else
                                  _symbol_value(_lexenv_))
                        pscope = _py.getattr(parent, sname) if parent else nil
                        return (pscope if not frame else
                                ((frame, (nil if parent is nil else
                                          pscope))))
                (self.varscope,
                 self.funcscope,
                 self.blockscope) = (compute_scope(parent,  "varscope",
                                                   complete_frame(name_varframe, kind_varframe, full_varframe)),
                                     compute_scope(parent, "funcscope",
                                                   complete_frame(name_funcframe, kind_funcframe, full_funcframe)),
                                     compute_scope(parent, "blockscope",
                                                   complete_frame(name_funcframe, kind_blockframe, full_blockframe)))
        @staticmethod
        def do_lookup_scope(scope, x, default):
                while scope:
                        if x in scope[0]["name"]:
                                return scope[0]["name"][x]
                        scope = scope[1] # COLD-CDR
                return default
        def lookup_func(self, x, default = None):         return self.do_lookup_scope(self.funcscope, x, default)
        def lookup_var(self, x, default = None):          return self.do_lookup_scope(self.varscope, x, default)
        def lookup_block(self, x, default = None):        return self.do_lookup_scope(self.blockscope, x, default)
        def lookup_scope(self, sname, x, default = None): return self.do_lookup_scope(getattr(self, sname), x, default)
        def funcscope_binds_p(self, x):   return self.lookup_func(x)  is not None
        def varscope_binds_p(self, x):    return self.lookup_var(x)   is not None
        def blockscope_binds_p(self, x):  return self.lookup_block(x) is not None
        def lookup_func_kind(self, kind, x, default = None):
                b = self.do_lookup_scope(self.funcscope, x, None)
                return (b and b.kind is kind and b) or default
        def lookup_var_kind(self, kind, x, default = None):
                b = self.do_lookup_scope(self.varscope, x, None)
                return (b and b.kind is kind and b) or default
def _lexenvp(x):         return _py.isinstance(x, _lexenv.python_type)
def _make_null_lexenv(): return make_instance(_lexenv, parent = nil)
def _make_lexenv(parent = nil, **initial_content):
        ## just a dumb pass-through
        return _lexenv.python_type(parent, **initial_content)

# Quasiquotation

def _convert_quasiquotation(quasiquoted_form):
        """Expand abbreviation of QUASIQUOTED-FORM (in a simple, yet suboptimal way)."""
        def rec(x):
                if atom(x):
                        return (quote, x)
                else:
                        acc = []
                        run = [list]
                        for ix in x:
                                if not _tuplep(ix) or not ix or ix[0] not in [comma, splice]:
                                        run.append(rec(ix))
                                elif len(ix) != 2:
                                        error("In quasi-quoted form %s: bad %s expression %s.", x, ix[0], ix)
                                elif ix[0] is comma:
                                        run.append(ix[1])
                                elif ix[0] is splice:
                                        if not acc:
                                                acc = [append]
                                        acc.append(_py.tuple(run))
                                        acc.append(ix[1])
                                        run = [list]
                        if acc:
                                acc.append(_py.tuple(run))
                        else:
                                acc = run
                        return _py.tuple(remove((list,), acc, test = equal))
        return rec(qq_form[1:])

# DEFKNOWN

_known = _poor_man_defstruct("known",
                             "name",
                             "metasex",
                             "nvalues", "nth_value",
                             "binds",
                             "lower",
                             "effects", "affected",
                             "lower_params")

def _compute_default_metasex(name):
        "Return a default MetaSEX form for a known with NAME."
        return (name, " ", _form)

def defknown(metasex_or_fn, name = None):
        """Define a form 'known' to the compiler.
The NAME is bound, in separate namespaces, to:
   - a LOWER method, which returns either a modified SEX, or a target tuple IR;
   - a BINDS method, which returns a namespace-categorised enumeration of bindings
     established by the form around its body, if any;
   - a MetaSEX form, which, when unspecified, defaults to (<KNOWN-NAME> " " %FORM),
     and serves the following purposes:
     - a pretty-printer specifier
     - structural validation
     - destructuring
     - walking
"""
        def do_defknown(fn, sym, pyname, metasex):
                fn.__name__ = "_lower_" + pyname
                _frost.setf_global(sym, pyname, globals = _py.globals())
                args_ast, body_asts = _function_ast(fn)
                body, documentation, declarations = _defbody_parse_ast(_py.set(), body_asts)
                def meth(name, body, *args):
                        return (name, lambda *_: _py.compile(body % args, "", "exec",
                                                             flags = _ast.PyCF_ONLY_AST).body[0])
                [lower,
                 binds,
                 nvalues, nth_value,
                 effects, affected,
                ] = _defbody_methods("DEFKNOWN " + _py.str(sym), body, lambda method_name: pyname + "_" + method_name,
                                     ["lower",
                                      meth("binds",
                                           """def %s_binds(*_):
                                                      return dict()""", pyname),
                                      meth("nvalues",
                                           """def %s_nvalues(*_):
                                                      error('Known %s unsupported in values context.')""", pyname, sym),
                                      meth("nth_value",
                                           """def %s_nth_value(*_):
                                                      error('Known %s unsupported in values context.')""", pyname, sym),
                                      meth("effects",
                                           """def %s_effects(*_):
                                                      error('Effect analysis unsupported for known %s.')""", pyname, sym),
                                      meth("affected",
                                           """def %s_affected(*_):
                                                      error('Effect analysis unsupported for known %s.')""", pyname, sym)])
                lower_key_args = _function_lambda_list(lower)[3]
                ## Complete, record the deeds.
                sym.known = _known(name = sym,
                                   metasex = metasex,
                                   binds = binds,
                                   nvalues = nvalues, nth_value = nth_value,
                                   lower = lower,
                                   effects = effects, affected = affected,
                                   lower_params = _mapset(_indexing(0), lower_key_args))
                return sym # pass through
        def _defknown(fn, name = name, metasex = metasex_or_fn):
                _, sym, pyname = _interpret_toplevel_value(fn, functionp)
                name    = _defaulted(name, sym, symbol)
                metasex = _defaulted(metasex, _compute_default_metasex(name))
                return do_defknown(fn, name, pyname, metasex)
        return (_defknown(metasex_or_fn, metasex = None) if functionp(metasex_or_fn) else
                _defknown                                if _tuplep(metasex_or_fn)   else
                error("In DEFKNOWN: argument must be either a function or a pretty-printer code tuple, was: %s.",
                      _py.repr(metasex_or_fn)))
def _find_known(x):
        return _symbol_known(the(symbol, x))

# METASEX-MATCHER, METASEX-MATCHER-PP and METASEX-MATCHER-NONSTRICT-PP

#         MetaSEX presents us with an excellent lesson.  Let's try to understand.

_intern_and_bind_pynames("%NAME", "%MAYBE", "%NOTLEAD", "%NOTTAIL",
                         "%NEWLINE", "%INDENT", "%FORM", "%BOUND", "%SYMBOL", "%TYPEP", "%COUNT-SCOPE",
                         "%MAYBE-ONCE", "%ONCE",
                         "%SET-MINUS")

def _form_known(form):
        ## METASEX-MATCHER guts it, due to case analysis
        complex_form_p = _tuplep(form) and symbolp(form[0])
        return complex_form_p and _find_known(form[0])

def _combine_t_or_None(f0, fR, orig_tuple_p):
        f0r = f0()
        if f0r is not None:
                fRr = fR()
                return t

class _metasex_matcher(_matcher):
        def __init__(m):
                _matcher.__init__(m)
                m.register_simplex_matcher(_form,        m.form)
                m.register_simplex_matcher(_bound,       m.form)
                m.register_simplex_matcher(_symbol,      m.symbol)
                m.register_simplex_matcher(_typep,       m.typep)
                m.register_complex_matcher(_newline,     m.ignore)
                m.register_complex_matcher(_indent,      m.ignore)
                m.register_complex_matcher(_notlead,     m.identity)
                m.register_complex_matcher(_nottail,     m.identity)
                m.register_complex_matcher(_count_scope, m.identity)
        def preprocess(m, pat):
                "Expand syntactic sugar."
                def prep_binding(b):
                        k, v = _py.tuple(b.items())[0]
                        return {k: m.preprocess(v)}
                return ((_count_scope, (some,) +
                         m.preprocess(_py.tuple(pat))) if typep(pat, _py.list)       else
                        prep_binding(pat)              if typep(pat, _py.dict)       else
                        (_form,)                       if pat == _form               else
                        (_newline, 0)                  if pat == "\n"                else
                        (_newline, pat)                if integerp(pat)              else
                        (_indent, 1)                   if pat == " "                 else
                        pat                            if not (pat and _tuplep(pat)) else
                        (m.preprocess(pat[0]),) + m.preprocess(pat[1:]))
        @staticmethod
        def prod(x, orig_tuple_p): return ""
        @staticmethod
        def comh(f0, fR, orig_tuple_p): return _combine_t_or_None(f0, fR, orig_tuple_p)
        @staticmethod
        def comr(f0, fR, orig_tuple_p): return _combine_t_or_None(f0, fR, orig_tuple_p)
        @staticmethod
        def forc(x, y, orig_tuple_p):
                return (("OR",) if orig_tuple_p else ()) + (x,) + y
        @staticmethod
        def nonliteral_atom_p(x):
                ## Currently only depended upon by the segment matcher.
                return x == _name
        @staticmethod
        def atom(exp, pat):
                _trace_printf("atom", "%%% atom: e:%s p:%s:  %s (t1:%s, t2:%s), _name: %s, symp(exp): %s, keyp(exp): %s, %s",
                              lambda: (exp, pat, ((symbolp(exp) and not keywordp(exp)) if pat is _name else
                                                  exp is pat                           if symbolp(pat) else
                                                  exp == pat),
                                       pat is _name, symbolp(pat), _name, symbolp(exp), keywordp(exp), _py.type(exp)))
                return _r(exp, pat,
                          ((symbolp(exp) and not keywordp(exp)) if pat is _name else
                           exp is pat                           if symbolp(pat) else
                           exp == pat))
        @staticmethod
        def form_metasex(form):
                ####### Unregistered Issue FORM-METASEX-TOO-RELAXED-ON-ATOMS
                return ((_typep, t)                             if not _tuplep(form)                                      else
                        ()                                      if not form                                               else
                        _find_known(form[0]).metasex            if symbolp(form[0]) and _find_known(form[0])              else
                        (_name, [(_nottail, " "), _form])       if symbolp(form[0])                                       else
                        (_form, "\n", [(_notlead, " "), _form]) if _tuplep(form[0]) and form[0] and form[0][0] is lambda_ else
                        ([(_notlead, " "), _form],))
        def form(m, bound, name, form, pat, orifst):
                prepped = m.preprocess(m.form_metasex(form))
                _trace_printf("form", "=== form for %s:\n    %s", lambda: (_py.repr(form), prepped))
                return _r(form, pat, m.match(bound, name, form, prepped, (_tuplep(form), orifst[1]), None, None))
        def symbol(m, bound, name, form, pat, orifst):
                return m.test(form is pat[1], bound, name, lambda: m.prod(form, orifst),
                              form, pat)
        def typep(m, bound, name, form, pat, orifst):
                return _r(form, pat,
                          m.test(typep(form, pat[1]), bound, name, lambda: m.prod(form, orifst),
                                 form, pat))

_string_set("*PP-BASE-DEPTH*", 0)
_string_set("*PP-DEPTH*", 0)
def _pp_base_depth(): return _symbol_value(_pp_base_depth_)
def _pp_depth():      return _symbol_value(_pp_depth_)

def _combine_pp(f0, fR, orig_tuple_p):
        def orig_tuple_comb(body):
                new_base = _pp_depth() + 1
                with progv({ _pp_base_depth_: new_base }):
                        ret = body(new_base)
                        return None if ret is None else ("(" + ret + ")")
        def body(base):
                f0r = f0()
                if f0r is None: return
                with progv({ _pp_depth_: base + _py.len(f0r.split("\n")[-1]) }):
                        fRr = fR()
                        if fRr is None: return
                        return f0r + fRr
        return (orig_tuple_comb if orig_tuple_p else
                lambda f: f(_pp_base_depth()))(body)

class _metasex_matcher_pp(_metasex_matcher):
        def __init__(m):
                _metasex_matcher.__init__(m)
                m.register_complex_matcher(_newline,     m.newline)
                m.register_complex_matcher(_indent,      m.indent)
                m.register_complex_matcher(_notlead,     m.notlead)
                m.register_complex_matcher(_nottail,     m.nottail)
        @staticmethod
        def prod(x, orig_tuple_p):
                result = _py.str(x) if x or orig_tuple_p else ""
                _trace_printf("yield", "+++ YIELD prod:\n%s", result)
                return result
        @staticmethod
        def comh(f0, fR, orig_tuple_p): return _combine_pp(f0, fR, orig_tuple_p)
        @staticmethod
        def comr(f0, fR, orig_tuple_p): return _combine_pp(f0, fR, orig_tuple_p)
        def newline(m, bound, name, exp, pat, orifst, aux, limit):
                n, tail = pat[0][1], pat[1:]
                new_base = _pp_base_depth() + n
                with progv({ _pp_depth_:      new_base,
                             _pp_base_depth_: new_base}):
                        return m.post(m.match(m.bind(new_base, bound, name), None, exp, tail, orifst, aux, None),
                                      lambda r: "\n" + (" " * new_base) + r)
        def indent(m, bound, name, exp, pat, orifst, aux, limit):
                n, tail = pat[0][1], pat[1:]
                new_depth = _pp_depth() + n
                with progv({ _pp_depth_: new_depth }):
                        return m.post(m.match(m.bind(new_depth, bound, name), None, exp, tail, orifst, aux, None),
                                      lambda r: (" " * n) + r)
        def notlead(m, bound, name, exp, pat, orifst, aux, limit):
                maybe_pat = pat[0][1]
                _trace_printf("notlead", "!!! notlead: first:%s %s %s", orifst[1], pat, exp)
                if orifst[1]:
                        return m.match(bound, name, exp, pat[1:],              orifst, aux, limit)
                ############## act as identity
                return         m.match(bound, name, exp, pat[0][1:] + pat[1:], orifst, aux, limit)
        def nottail(m, bound, name, exp, pat, orifst, aux, limit):
                maybe_pat = pat[0][1]
                before_end = exp == ()
                _trace_printf("notlead", "!!! nottail: before-end:%s %s %s", before_end, pat, exp)
                if before_end:
                        return m.match(bound, name, exp, pat[1:],              orifst, aux, limit)
                ############## act as identity
                return         m.match(bound, name, exp, pat[0][1:] + pat[1:], orifst, aux, limit)

_intern_and_bind_pynames("%LAX")

class _metasex_matcher_nonstrict_pp(_metasex_matcher_pp):
        def __init__(m):
                _metasex_matcher_pp.__init__(m)
                m.register_complex_matcher(_lax, m.lax)
        def lax(m, bound, name, exp, pat, orifst, aux, limit):
                return (m.prod(exp, orifst[0])                if not exp         else
                        ######################### Thought paused here..
                        m.match(bound, name, exp, (([(_lax,)] if _tuplep(exp[0]) else
                                                    (_typep, t)), (_lax,)), (False, False), None, None))
        def crec(m, exp, l0, lR, orig_tuple_p = False):
                ## Unregistered Issue PYTHON-LACK-OF-RETURN-FROM
                #
                ##
                ###
                ### What the HELL does None mean, as a FAILEX value?
                ###
                ##
                #
                failpat, failex, bound0, boundR = None, None, None, None
                def try_0():
                        nonlocal bound0, failex, failpat
                        _, failex, failpat = l0()
                        if failpat is None: return failex
                        return m.match({}, None, exp, ([(_lax,)],) if _tuplep(exp) else (_typep, t),
                                       (None, None), None, None)
                def try_R():
                        nonlocal boundR, failex, failpat
                        _, failex, failpat = lR(bound0)
                        if failpat is None: return failex
                        return m.match({}, None, exp, ([(_lax,)],) if _tuplep(exp) else (_typep, t),
                                       (None, None), None, None)
                result = (m.comh if horisontal else
                          m.comr)(try_0, try_R, orig_tuple_p)
                _trace_printf("yield", "+t+ YIELD for %s:\n%s", exp, result)
                return (m.succ(boundR, result) if failpat is None else
                        m.fail(boundR or bound0, failex, failpat))

_metasex              = _metasex_matcher()
_metasex_pp           = _metasex_matcher_pp()
_metasex_nonstrict_pp = _metasex_matcher_nonstrict_pp()

# _trace("complex")
# _trace("simplex")
# _trace("segment")
# _trace("match")
# _trace("yield")
# _trace("notlead")
# _trace("form")
# _trace("atom")
# _trace(_return, "test")
# _trace(_return, "crec")
# _trace(_return, "comb")
# _trace(_return, "simplex")
# _trace(_return, "complex")
# _trace(_return, "segment")
# _trace(_return, "maybe")
# _trace(_return, "match")
# _trace(_return, "atom")
# _trace(_return, "form")
# _trace(_return, "typep")

def _match_sex(sex, pattern = None):
        return _match(_metasex, sex, _defaulted(pattern, _metasex.form_metasex(sex)))

## WIP: set specifiers (got bored)
_string_set("*SETSPEC-SCOPE*", nil)
_setspec = _defscope("_setspec", _setspec_scope_)

# Pretty-printing

def _pp_sex(sex, strict = t, initial_depth = None):
        ## Unregistered Issue RELAXED-METASEX-PRETTY-PRINTER-MODE-NEEDED
        initial_depth = _defaulted_to_var(initial_depth, _pp_base_depth_)
        with progv({ _pp_depth_:      initial_depth,
                     _pp_base_depth_: initial_depth}):
                _, r, f = _match(_metasex_pp, sex, _metasex.form_metasex(sex))
        if f is not None:
                error("\n=== failed sex: %s\n=== failsubpat: %s\n=== subex: %s", sex, f, r)
        return r or ""

# IR method -based services

def _ir_nvalues(form):
        return (lambda known: (known.nvalues(*form[1:])            if known else
                               None))                (_form_known(form))
def _ir_nth_value(n, form):
        return (lambda known: (known.nth_value(n, form, *form[1:]) if known else
                               (nth_value, n, form)))(_form_known(form))
def _ir_binds(form):
        return (lambda known: (known.binds(*form[1:])              if known else
                               _py.dict()))          (_form_known(form))
def _ir_effects(form):
        return (lambda known: (known.effects(*form[1:])            if known else
                               t))                   (_form_known(form))
def _ir_affected(form):
        return (lambda known: (known.affected(*form[1:])           if known else
                               t))                   (_form_known(form))

# Testing

_results_ = []
def _runtest(fun, bindings, result):
        b, r, f = fun()
        _results_.append((fun, b, r, f))
        return (b == bindings,
                r == result,
                f is None)
def _results():
        for fun, b, r, f in _results_:
                _debug_printf("%15s bound: %s", fun.__name__, b)
                _debug_printf("%15s res: %s", fun.__name__, r)

# _intern_and_bind_pynames("LET", "FIRST", "SECOND", "CAR", "&BODY")
# def just_match():
#         return _match(_metasex,
#                       (let, ((first, ()),
#                              (second, (car,))),
#                         _body),
#                       (let, " ", ({"bindings":[(_notlead, "\n"), (_name, " ", _form)]},),
#                          1, {"body":[(_notlead, "\n"), _form]}))
# bound_good, result_good, nofail = _runtest(just_match,
#                                            { 'bindings': ((first, ()),
#                                                           (second, (car,))),
#                                              'body':     (_body,)},
#                                            t)
# _results()
# assert(nofail)
# assert(bound_good)
# assert(result_good)
# print("; JUST-MATCH: passed")

# def pp():
#         return _match(_metasex_pp,
#                       (let, ((first, ()),
#                              (second, (car,))),
#                         _body),
#                       (let, " ", ({"bindings":[(_notlead, "\n"), (_name, " ", _form)]},),
#                          1, {"body":[(_notlead, "\n"), _form]}))
# bound_good, result_good, nofail = _runtest(pp,
#                                            { 'bindings': ((first, ()),
#                                                           (second, (car,))),
#                                              'body':     (_body,)},
#                                            """(LET ((FIRST ())
#       (SECOND (CAR)))
#   &BODY)""")
# _results()
# assert(nofail)
# assert(bound_good)
# assert(result_good)
# print("; PP: passed")

# def mal_pp():
#         return _match(_metasex_nonstrict_pp,
#                       (let, ((first),
#                              (second, (car,), ())),
#                         _body),
#                       (let, " ", ([(_notlead, "\n"), (_name, " ", _form)],),
#                          1, [(_notlead, "\n"), _form]))
# bound_good, result_good, nofail = _runtest(mal_pp,
#                                            {},
#                                            """(LET ((FIRST ())
#       (SECOND (CAR)))
#   &BODY)""")
# _results()
# assert(nofail)
# assert(bound_good)
# assert(result_good)
# print("; MAL-PP: passed")

# def empty():
#         return _match(_metasex_pp, (), {"whole":()})
# bound_good, result_good, nofail = _runtest(empty,
#                                            { 'whole': () },
#                                            "()")
# _results()
# assert(nofail)
# assert(bound_good)
# assert(result_good)
# print("; EMPTY: passed")

# def empty_cross():
#         return _match(_metasex_pp, (), ({"a":[_name]}, {"b":[(_name,)]},))
# bound_good, result_good, nofail = _runtest(empty_cross,
#                                            { 'a': (), 'b': () },
#                                            "()")
# _results()
# assert(nofail)
# assert(bound_good)
# assert(result_good)
# print("; EMPTY-CROSS: passed")

# def alternates():
#         return _match(_metasex_pp, (1, "a"), {"whole":([(or_, (_typep, _py.int),
#                                                               (_typep, _py.str))],)})
# bound_good, result_good, nofail = _runtest(alternates,
#                                            { 'whole': (1, "a") },
#                                            "(1a)")
# _results()
# assert(nofail)
# assert(bound_good)
# assert(result_good)
# print("; ALTERNATES: passed")

# _intern_and_bind_pynames("PI")
# def simplex():
#         pat = ({'head':[()]}, {'tail':_name})
#         exp = ((), pi)
#         return _match(_metasex_pp, exp, pat)
# bound_good, result_good, nofail = _runtest(simplex,
#                                            { 'head': ((),),
#                                              'tail': pi },
#                                            "(()PI)"
#                                            )
# _results()
# assert(nofail)
# assert(bound_good)
# assert(result_good)
# print("; SIMPLEX: passed")

# def mid_complex():
#         pat = ({"headname":_name},
#                   {"headtupname":(_name,)},
#                            {"varitupseq":[(_name, [_name])]},
#                                                     {"fix1tupseq":[(_name,)]},
#                                                                            {"nameseq":[_name]},
#                                                                                 {"tailname":_name})
#         exp =              (pi,   (pi,),   (pi,), (pi, pi), (pi, pi, pi), (pi,), (pi,), (pi,),   pi, pi, pi)
#         return _match(_metasex_pp, exp, pat)
# bound_good, result_good, nofail = _runtest(mid_complex,
#                                            { 'headname': pi,
#                                              'headtupname': (pi,),
#                                              'varitupseq': ((pi,), (pi, pi), (pi, pi, pi)),
#                                              'fix1tupseq': ((pi,), (pi,), (pi,)),
#                                              'nameseq': (pi, pi),
#                                              'tailname': pi },
#                                            "(PI(PI)(PI)(PIPI)(PIPIPI)(PI)(PI)(PI)PIPIPI)"
#                                            # "(PI (PI) (PI) (PI PI) (PI PI PI) (PI) (PI) (PI) PI PI PI)"
#                                            )
# _results()
# assert(nofail)
# assert(bound_good)
# assert(result_good)
# print("; MID-COMPLEX: passed")

# def simple_maybe():
#         return _match(_metasex_pp, (pi, car, cdr), ({"pi":(_maybe, _name)}, {"car":_name}, (_maybe, {"cdr":_name})))
# bound_good, result_good, nofail = _runtest(simple_maybe,
#                                            { 'pi': (pi,), 'car': car, 'cdr': cdr, },
#                                            "(PICARCDR)")
# _results()
# assert(nofail)
# assert(bound_good)
# assert(result_good)
# print("; SIMPLE-MAYBE: passed")

# Macroexpansion

def _do_macroexpand_1(form, env = nil, compilerp = nil):
        """This handles:
             - macro expansion, and,
           optionally, if COMPILERP is non-NIL:
             - conversion of implicit funcalls to knowns, and
             - compiler macro expansion"""
        ## Unregistered Issue COMPLIANCE-IMPLICIT-FUNCALL-INTERPRETATION
        # SYMBOL-MACRO-FUNCTION is what forced us to require the package system.
        def find_compiler_macroexpander(form):
                ### XXX: we rely on the FUNCALL form to have been validated! (Joys of MetaSEX, yet?)
                b, _, fail = _match_sex(form[1:], ((or_, (quote,    { "global_function_name":  (_typep, symbol)}),
                                                         (function, { "lexical_function_name": (_typep, symbol)})), [_form]))
                global_, lexical_ = (b.get("global_function_name",  nil),
                                     b.get("lexical_function_name", nil))
                return (compiler_macro_function(global_ or lexical_, check_shadow = lexical_) if fail is None else
                        nil)
        def knownifier_and_maybe_compiler_macroexpander(form, known):
                if not known: ## ..then it's a funcall, because all macros
                        xformed = (apply, (function, form[0])) + form[1:] + (nil,)
                        return lambda *_: (find_compiler_macroexpander(xformed) or identity)(xformed)
                else:
                        return (find_compiler_macroexpander(form) if known.name is apply else
                                nil)
        expander, args = (((form and symbolp(form[0]) and
                            (macro_function(form[0], env) or
                             (compilerp and
                              knownifier_and_maybe_compiler_macroexpander(form, _find_known(form[0]))))),
                           form[1:])                              if _tuplep(form) else
                          (_symbol_macro_expander(form, env), ()) if symbolp(form) else ## Notice, how NIL is not expanded.
                          (nil, nil))
        return ((form, nil) if not expander else
                (expander(*args), t))

## Unregistered Issue COMPLIANCE-MACROEXPAND-HOOK-MUST-BE-FUNCALL-BUT-IT-IS-NOT-A-FUNCTION
_string_set("*MACROEXPAND-HOOK*", lambda f, *args, **keys: f(*args, **keys))
_string_set("*ENABLE-MACROEXPAND-HOOK*", t)

def macroexpand_1(form, env = nil, compilerp = nil):
        if symbol_value(_enable_macroexpand_hook_):
                return symbol_value(_macroexpand_hook_)(_do_macroexpand_1, form, env, compilerp = compilerp)
        return _do_macroexpand_1(form, env, compilerp)

def macroexpand(form, env = nil, compilerp = nil):
        def do_macroexpand(form, expanded):
                expansion, expanded_again = macroexpand_1(form, env, compilerp)
                return (do_macroexpand(expansion, t) if expanded_again else
                        (form, expanded))
        return do_macroexpand(form, nil)

_string_set("*MACROEXPANDER-ENV*", nil)       ## This is for regular macro expansion.
_string_set("*MACROEXPANDER-FORM-BINDS*", nil)

def _macroexpander_inner(m, bound, name, form, pat, orifst, compilerp = t):
        # _debug_printf("%%MXER-INNER: -->\n%s", exp)
        ## 1. Expose the knowns and implicit funcalls:
        env = _symbol_value(_macroexpander_env_)
        expanded_form, _ = macroexpand(form, env, compilerp = compilerp)
        ## 2. Compute bindings contributed by this outer form.
        known = _find_known(expanded_form[0]) if _tuplep(expanded_form) else nil
        (symbol_frame,
         mfunc_frame,
         ffunc_frame) = ((_dict_select_keys(_ir_binds(expanded_form), symbol_macro),
                          _dict_select_keys(_ir_binds(expanded_form), macro),
                          _dict_select_keys(_ir_binds(expanded_form), function)) if known else
                         (_py.dict(), _py.dict(), _py.dict()))
        ## 3. Pass the binding extension information down.
        with (progv({_macroexpander_form_binds_: _make_lexenv(env,
                                                              kind_varframe = symbol_frame,
                                                              kind_funcframe = _dictappend(mfunc_frame, ffunc_frame))})
              if symbol_frame or mfunc_frame or ffunc_frame else
              _withless()):
                b, r, f = _metasex_matcher.form(m, bound, name, expanded_form, "ignored", orifst)
                # if not f:
                #         _debug_printf("%%MXER-INNER: <--\n%s", r)
                return b, r, f
##
####
#####
####
###
### The Grand point-of-continuation TODO is here.
###
###  - explore final IR-ification through APPLY-conversion
###    - not necessarily final, due to:
###      - inlining
###      - complex transformations, not possible early, which enable further compiler macro expansion
###    - but, possibly, still final, if we cache macroexpanded/applyified forms of functions
###    - but, even then, complex transformations can recurse, potentially affecting pre-cached expansions
###      - FINALLY - DEPENDENT TRACKING!
###  - explore generic inline-context-change-aware walking (which doesn't affect the walking itself,
###    unlike the case with macroexpansion)
###    - sounds bland, doesn't it?
###
####
#####
####
##
class _macroexpander_matcher(_metasex_matcher):
        def __init__(m):
                ## Unregistered Issue MACROEXPANDER-COMPILERP-STATE-PASSING-TANGLED
                _metasex_matcher.__init__(m)
                m.register_simplex_matcher(_form,  lambda *args: _macroexpander_inner(m, *args, compilerp = t))
                m.register_simplex_matcher(_bound, m.activate_binding_extension)
        def activate_binding_extension(m, bound, name, exp, pat, orifst):
                ## The dynamic scope expansion is unavoidable, because we have to
                ## terminate the binding extension marker.
                with progv({_macroexpander_env_: (_symbol_value(_macroexpander_form_binds_) or
                                                  _symbol_value(_macroexpander_env_)),
                            ## marker scope ends here:
                            _macroexpander_form_binds_: nil}):
                        return _macroexpander_inner(m, bound, name, exp, pat, orifst, compilerp = t)
        @staticmethod
        def prod(x, orig_tuple_p): return x
        @staticmethod
        def comh(f0, fR, orig_tuple_p):
                f0r = f0()
                if f0r is not None:
                        fRr = fR()
                        return _r(f0r, fRr, f0r + fRr)
        @staticmethod
        def comr(f0, fR, orig_tuple_p):
                f0r = f0()
                if f0r is not None:
                        fRr = fR()
                        return _r(f0r, fRr, (f0r,) + fRr)

_macroexpander = _macroexpander_matcher()

def macroexpand_all(form, env = nil, compilerp = t):
        _, res, failpat = _macroexpander_inner(_macroexpander, dict(), None,
                                               form,
                                               nil,  ## The pattern will be discarded out of hand, anyway.
                                               (None, None),
                                               compilerp = t)
        assert(not failpat)
        return res

# DEFMACRO

@_set_macro_definition
# ((intern("DEFMACRO")[0], " ", _name, " ", ([(_notlead, " "), _name],),
#   1, [(_notlead, "\n"), (_bound, _form)]))
def DEFMACRO(name, lambda_list, *body):
        return (eval_when, (_compile_toplevel, _load_toplevel, _execute),
                 (apply, ("cl", "_set_macro_definition"), (lambda_, lambda_list) + body,
                         nil))

# Tuple intermediate IR

#     A tuple of:
#     - prologue
#     - value, can only contain _ast.expr's

# Unregistered Issue DEBUG-SCAFFOLDING
_string_set("*COMPILER-DEBUG-P*",    nil)
def _debug_compiler(value = t):
        _string_set("*COMPILER-DEBUG-P*", value, force_toplevel = t)
def _debugging_compiler():
        return _symbol_value(_compiler_debug_p_)
def _compiler_debug_printf(control, *args):
        if _debugging_compiler():
                justification = _sex_space()
                def fix_string(x): return x.replace("\n", "\n" + justification) if stringp(x) else x
                _debug_printf(justification + fix_string(control), *_py.tuple(fix_string(a) for a in args))

if probe_file("/home/deepfire/.partus-debug-compiler"):
        _debug_compiler()

def _tuplerator(pve):
        for x in pve[0]:
                if x is not None:
                        yield x
        if pve[1] is not None:
                yield pve[1]
def _tuple_empty_p(pve):      return not (pve[0] or pve[1])
def _tuple_expression_p(pve): return not (pve[0])
def _tuple_bound(pve):        return _mapsetn(_atree_bound, _tuplerator(the((pytuple, pylist, pytuple), pve)))
def _tuple_free(pve):         return _mapsetn(_atree_free,  _tuplerator(the((pytuple, pylist, pytuple), pve)))
def _tuple_xtnls(pve):        return _mapsetn(_atree_xtnls, _tuplerator(the((pytuple, pylist, pytuple), pve)))

def _prepend_prologue(pro, tuple):
        return (pro + tuple[0],
                tuple[1])

# Compiler state and miscellanea

## Should, probably, be bound by the compiler itself.
_string_set("*COMPILER-TOPLEVEL-P*", t)
_string_set("*COMPILER-DEF*",        nil)
_string_set("*COMPILER-TAILP*",      nil)

def _sex_space(delta = None, char = " "):
        return char * (_pp_base_depth() + _defaulted(delta, 0))
def _sex_deeper(n, body):
        with progv({ _pp_base_depth_: _pp_base_depth() + n }):
                return body()

__compiler_form_record__ = _collections.defaultdict(lambda: 0)
__compiler_form_record_threshold__ = 5
def _compiler_track_compiled_form(form):
        cur = __compiler_form_record__[_py.id(form)]
        ## Unregistered Issue NONTERMINATION-SAFETY-CHECK-BUGGY
        # if cur > __compiler_form_record_threshold__:
        #         error("Apparent non-termination while compiling %s (happened over %d times).",
        #               form, __compiler_form_record_threshold__)
        __compiler_form_record__[_py.id(form)] += 1

class _compiler_def(_servile):
        pass

def _compiling_def():     return _symbol_value(_compiler_def_)
def _tail_position_p():   return _symbol_value(_compiler_tailp_)

def _compiler_report_context():
        _here("def %s\n      tailp: %s",
              *mapcar(_symbol_value, [_compiler_def_,
                                      _compiler_tailp_]))

_tail_position       = _defwith("_tail_position",
                                   lambda *_: _dynamic_scope_push(_py.dict(_COMPILER_TAILP_ = t)),
                                   lambda *_: _dynamic_scope_pop())

_maybe_tail_position = _defwith("_maybe_tail_position", # This is just a documentation feature.
                                   lambda *_: None,
                                   lambda *_: None)

_no_tail_position    = _defwith("_no_tail_position",
                                   lambda *_: _dynamic_scope_push(_py.dict(_COMPILER_TAILP_ = nil)),
                                   lambda *_: _dynamic_scope_pop())

_compiler_debug         = _defwith("_compiler_debug",
                                   lambda *_: _dynamic_scope_push(_py.dict(_COMPILER_DEBUG_P_ = t)),
                                   lambda *_: _dynamic_scope_pop())

#### Issues:
## Tail position optimisations
## Lisp-level bound/free
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
###              | SYMBOL, LET, APPLY <- (APPLY)       <-
###                          LET, DEF <- (FLET)        <-
###              PROGN | FLET, SYMBOL <- (LAMBDA)      <-
###                  FLET, DEF, APPLY <- (LABELS)      <-
###                 PROGN | LET, LET* <- (LET*)        <-
@defun("NOT")
def not_(x):        return t if x is nil else nil

def _lower_expr(x, fn):
        pro, val = lower(x)
        return (pro, fn(val))

# Out-of-band IR argument passing: %IR-ARGS, %IR

@defknown((_name, "\n", _form, ["\n", ((_typep, _py.str), " ", (_typep, t))],))
def _ir_args():
        def lower(*_): error("Invariant failed: %s is not meant to be lowered.", _ir_args)

def _maybe_ir_args(x):
        "Maybe extract IR-ARGS' parameters, if X is indeed an IR-ARGS node, returning them as third element."
        return ((t, x[1], x[2:]) if _tuplep(x) and x and x[0] is _ir_args else
                (nil, x, _py.tuple()))

def _ir(*ir, **keys):
        "This IR-ARGS constructur is meant to be used to pass extended arguments down."
        known = _find_known(the(symbol, ir[0]))
        invalid_params = _py.set(keys.keys()) - known.lower_params
        if invalid_params:
                error("In IR-ARGS: IR %s accepts parameters in the set %s, whereas following unknowns were passed: %s.",
                      known.name, known.lower_params, invalid_params)
        return (_ir_args, ir) + _py.tuple(_hash_table_alist(keys))

def _ir_args_when(when, ir, **parameters):
        return _ir(*ir, **parameters) if when else ir

# Lambda list facilities

def _ir_prepare_lispy_lambda_list(lambda_list_, context, allow_defaults = None, default_expr = None):
        default_expr = _defaulted(default_expr, (_name, "None"))
        if not _tuplep(lambda_list_):
                error("In %s: lambda list must be a tuple, was: %s.", context, lambda_list_)
        def valid_parameter_specifier_p(x): return typep(x, (and_, symbol, (not_, keyword)))
        test, failure_message = ((lambda x: valid_parameter_specifier_p(x) or (_tuplep(x) and _py.len(x) == 2 and
                                                                               valid_parameter_specifier_p(x[0])),
                                 "In %s: lambda lists can only contain non-keyword symbols and two-element lists, with said argument specifiers as first elements: %s.")
                                 if allow_defaults else
                                 (valid_parameter_specifier_p, "In %s: lambda list must consist of non-keyword symbols: %s.  Default values are forbidden in this context."))
        ### 0. locate lambda list keywords
        lambda_words = [_optional, _rest, _key, _restkey]
        optpos,  restpos,  keypos,  restkeypos = lambda_posns = mapcar(lambda x: position(x, lambda_list_), lambda_words)
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
        # _locals_printf(_py.locals(),
        #                "optpos",  "restpos",  "keypos",  "restkeypos",
        #                "optposp", "restposp", "keyposp", "restkeyposp",
        #                "toptpos", "trestpos", "tkeypos", "trestkeypos")
        ### 2. ensure correct amount of names for provided lambda list keywords
        if (restposp and keyposp and (keypos - restpos != 1) or
            restposp and (not keyposp) and restkeyposp and (restkeypos - restpos != 1) or
            restkeyposp and (_py.len(lambda_list_) - restkeypos != 2)):
                error("In %s: found garbage instead of a lambda list: %s", context, lambda_list_)
        ### 3. compute argument specifier sets, as determined by provided lambda list keywords
        restkey = restkeyposp and lambda_list_[restkeypos + 1] or None
        _keys = _py.list(lambda_list_[keypos + 1:restkeypos or None]) if keypos else _py.tuple()
        keys, keydefs = (_py.list(_ensure_car(x) for x in _keys),
                         _py.list((x[1] if _tuplep(x) else default_expr)
                                  for x in _keys))
        rest = restposp and lambda_list_[restpos + 1] or None
        optional = _py.list(lambda_list_[optpos + 1:restpos or keypos or restkeypos or None]) if optposp else []
        optional, optdefs = (_py.list(_ensure_car(x) for x in optional),
                             _py.list((x[1] if _tuplep(x) else default_expr)
                                      for x in optional))
        fixed = _py.list(lambda_list_[0:_defaulted(optpos, (restpos    if restposp    else
                                                            keypos     if keyposp     else
                                                            restkeypos if restkeyposp else None))])
        if not every(symbolp, fixed):
                error("In %s: fixed arguments must be symbols, but %s wasn't one.", context, find_if_not(symbolp, fixed))
        total = fixed + optional + ([rest] if rest else []) + keys + ([restkey] if restkey else [])
        ### 4. validate syntax of the provided individual argument specifiers
        if not every(valid_parameter_specifier_p, total):
                error(failure_message, context, lambda_list_)
        ### 5. check for duplicate lambda list specifiers
        if _py.len(total) != _py.len(_py.set(total)):
                error("In %s: duplicate parameter names in lambda list: %s.", context, lambda_list_)
        return (total,
                (fixed, optional, rest, keys, restkey),
                (optdefs, keydefs))

def _lower_lispy_lambda_list(context, fixed, optional, rest, keys, restkey, opt_defaults, key_defaults):
        assert _py.len(optional) == _py.len(opt_defaults)
        assert _py.len(keys) == _py.len(key_defaults)
        ((odef_pros, odef_vals),
         (kdef_pros, kdef_vals)) = mapcar(lambda x: _recombine((_py.list, _py.list), _lower, x),
                                          [opt_defaults, key_defaults])
        if odef_pros or kdef_pros:
                _compiler_error("In LAMBDA %s: invariant failed: target lambda lists must have expressible defaults: %s.", context)
        return ("arguments",
                mapcar(lambda x: ("arg", _frost.full_symbol_name_python_name(x)), fixed + optional),
                rest and _frost.full_symbol_name_python_name(rest) or None, None,
                mapcar(lambda x: ("arg", _frost.full_symbol_name_python_name(x)), keys),
                restkey and _frost.full_symbol_name_python_name(restkey) or None, None,
                odef_vals,
                kdef_vals)

# Multiple-value function call dependency support

def _ir_function_form_nvalues(func):
        return _defaulted(
                _ir_depending_on_function_properties(func,
                                                     lambda fn, types: _py.len(types),
                                                     ("value_types", lambda x: x is not t)),
                t)

def _ir_function_form_nth_value_form(n, func, orig_form):
        return _defaulted(
                _ir_depending_on_function_properties(
                        func,
                        lambda fn, types, effs: (nil if n >= _py.len(types) else
                                                 _ir_make_constant(types[n])),
                        ("value_types", lambda x: (x is not t and (n >= _py.len(x) or
                                                                   ## XXX: This is sweet, no?
                                                                   _eql_type_specifier_p(x[n])))),
                        ## Let's not get lulled, though -- this is nothing more than a
                        ## fun optimisation, and not a substitute for proper constant propagation.
                        ("effects",     lambda x: not x)),
                (nth_value, n, orig_form))

# Python calls

def _ir_cl_module_name(name):
        return ("cl", name)

def _ir_cl_module_call(name, *ir_args):
        return (apply, _ir_cl_module_name(name)) + ir_args

# SETQ
#         :PROPERTIES:
#         :K:        [X]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

def _lower_name(name, ctx = "Load"):
        check_type(name, (or_, symbol, (pytuple, (eql, symbol), symbol)))
        if _tuplep(name) and ctx != "Load":
                error("COMPILE-NAME: only 'Load' context possible while lowering (SYMBOL ..) forms.")
        return ("Name", _frost.full_symbol_name_python_name(name[1] if _tuplep(name) else name), (ctx,))

@defknown((intern("SETQ")[0], " ", _name, " ", _form))
def setq():
        def nvalues(_, __):               return 1
        def nth_value(n, orig, _, value): return orig if n is 0 else (progn, orig, nil)
        ## Unregistered Issue COMPLIANCE-ISSUE-SETQ-BINDING
        ## Unregistered Issue COMPLIANCE-SETQ-MULTIPLE-ASSIGNMENTS-UNSUPPORTED
        def lower(name, value):
                # Urgent Issue SETQ-BROKEN-WRT-SPECIAL-VARIABLES
                # Urgent Issue COMPLIANCE-IR-LEVEL-BOUND-FREE-FOR-GLOBAL-NONLOCAL-DECLARATIONS
                pro, val = _lower(value)
                return (pro + [("Assign", [_lower_name(name, "Store")], val)],
                        _lower_name(name))
        def effects(name, value):         return t
        def affected(name, value):        return _ir_affected(value)

# QUOTE
#         :PROPERTIES:
#         :K:        [X]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

@defknown
def quote():
        def nvalues(_):            return 1
        def nth_value(n, orig, _): return orig if n is 0 else nil
        def lower(x):
                # Unregistered Issue COMPLIANCE-QUOTED-LITERALS
                if symbolp(x) and not constantp(x):
                        return nil if x is nil else (ref, x)
                else:
                        atree, successp = _try_atreeify_constant(x)
                        if successp:
                                return ([],
                                        atree)
                        else:
                                return (apply, (function, list)) + _py.tuple((quote, x) for x in x) + (nil,)
        def effects(x):            return nil
        def affected(x):           return nil

# MULTIPLE-VALUE-CALL
#         :PROPERTIES:
#         :IMPL:     [X]
#         :K:        [X]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :CL:       [X]
#         :END:

def _do_multiple_value_call(fn, values_frames):
        return fn(*reduce(lambda acc, f: acc + f[1:], values_frames, []))

@defknown
def multiple_value_call():
        ## We might start considering the argument forms for the values queries,
        ## once we get into the partial evaluation affairs..
        def nvalues(func, *_):
                return _ir_function_form_nvalues(func)
        def nth_value(n, orig, func, *_):
                return _ir_function_form_nth_value_form(n, func, orig)
        def lower(fn, *arg_forms):
                ## We have no choice, but to lower immediately, and by hand.
                ## No further processing, also.  At least as far as is observable now.
                (fn_p, fn_v, fn_n), *arg_pvns = [ _lower(x) + (_gensymname(),) for x in (fn,) + arg_forms ]
                pro = fn_p + [("Assign", [ _lower_name(fn_n, "Store") ], fn_v)]
                for p, v, n in arg_pvns:
                        pro.extend(p)
                        pro.append(("Assign", [_lower_name(n, "Store")], v))
                ## Unregistered Issue SAFETY-VALUES-FRAME-CHECKING
                pro.append(("Call", fn_n, [("Subscript", _lower_name(n),
                                                         ("Slice", ("Num", 1)),
                                                         ("Load",))
                                           for _, __, n in arg_pvns], []))
                return pro
        def effects(fn, *arg_forms):
                return (_py.any(_ir_effects(arg) for arg in arg_forms) or
                        _ir_depending_on_function_properties(func, lambda fn, effects: effects, "effects"))
        def affected(fn, *arg_forms):
                return (_py.any(_ir_affected(arg) for arg in arg_forms) or
                        _ir_depending_on_function_properties(func, lambda fn, affected: affected, "affected"))

# PROGN
#         :PROPERTIES:
#         :K:        [X]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

def _ir_nth_valueify_last_subform(n, form):
        return form[:-1] + ((nth_value, n, form[-1]),)

@defknown((intern("PROGN")[0],
            1, [(_notlead, "\n"), _form]))
def progn():
        def nvalues(*body):            return 1   if not body else _ir_nvalues(body[-1])
        def nth_value(n, orig, *body): return nil if not body else _ir_nth_valueify_last_subform(n, orig)
        def lower(*body):
                if not body:
                        return ([],
                                _lower(nil))
                pro, ntotal = [], _py.len(body)
                with _no_tail_position():
                        for spro, val in (_lower(x) for x in body[:-1]):
                                pro.extend(spro)
                                pro.append(("Expr", val))
                with _maybe_tail_position():
                        return _prepend_prologue(pro,
                                                 _lower(body[-1]))
                ## Not sure the stuff below still makes sense.  Still, am afraid to erase it.
                # lowered_body = mapcan(_lower, body)
                # (( body_bound_vars,  body_free_vars,  body_xtnls),
                #  (thunk_bound_vars, thunk_free_vars, thunk_xtnls)) = mapcar(_ast_bound_free, [lowered_body, thunks])
                # must_thunk = _py.len(lowered_body) > 1 or thunks
                # scope_mutation = (body_bound_vars or thunk_bound_vars or body_xtnls or thunk_xtnls)
                # if not (must_thunk or scope_mutation):
                #         return lowered_body
        def effects(*body):
                return _py.any(_ir_effects(f) for f in body)
        def affected(*body):
                return _py.any(_ir_affected(f) for f in body)

# IF
#         :PROPERTIES:
#         :K:        [X]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

@defknown((intern("IF")[0], " ", _form,
             3, _form,
             (_maybe, "\n", _form)))
def if_():
        def nvalues(test, consequent, antecedent):
                nconseq, nante = _ir_nvalues(consequent), _ir_nvalues(antecedent)
                return (nconseq                 if nconseq == nante                      else
                        _py.max(nconseq, nante) if integerp(nconseq) and integerp(nante) else
                        t)
        def nth_value(n, orig, test, consequent, antecedent):
                vconseq, vante = _ir_nth_value(consequent), _ir_nth_value(antecedent)
                ## Warning: this something, that goes on here, is quite interesting!
                ## Thinking pause: WHY DO WE DO THIS ..and.. WHAT EXACTLY DO WE DO HERE?
                ##  - what: lowering valueness?  ..a good initial approach to understanding..
                return ((nth_value, n, orig) if _ir_effects(consequent) or _ir_effects(antecedent) else
                        vconseq              if (vconseq == vante) and not _ir_effects(test)       else
                        (if_, test, vconseq, vante))
        def lower(test, consequent, antecedent):
                with _no_tail_position():
                        lo_test = pro_test, val_test = _lower(test)
                lo_cons, lo_ante = mapcar(_lower, [consequent, antecedent])
                ((pro_cons, val_cons),
                 (pro_ante, val_ante)) = lo_cons, lo_ante
                cons_expr_p, ante_expr_p = mapcar(_tuple_expression_p, [lo_cons, lo_ante])
                if _py.all([cons_expr_p, ante_expr_p]):
                        _compiler_debug_printf(" -- IF: simple all-expression case")
                        return (pro_test,
                                ("IfExp", val_test, val_cons, val_ante))
                else:
                        _compiler_debug_printf(" -- IF: complex FLET-based case")
                        ## Unregistered Issue FUNCALL-MISSING
                        name_cons, name_ante = _gensyms(x = "IF-BRANCH", n = 2)
                        cons, cons_fdefn = ((consequent,                     ()) if cons_expr_p else
                                            ((apply, (ref, name_cons), nil), ((name_cons, ()) + (consequent,),)))
                        ante, ante_fdefn = ((antecedent,                     ()) if ante_expr_p else
                                            ((apply, (ref, name_ante), nil), ((name_ante, ()) + (antecedent,),)))
                        return (flet, cons_fdefn + ante_fdefn,
                                 (if_, test, cons, ante))
        def effects(test, consequent, antecedent):  return _py.any(_ir_effects(f) for f in [test, consequent, antecedent])
        def affected(test, consequent, antecedent): return _py.any(_ir_affected(f) for f in [test, consequent, antecedent])

# LET
#           :PROPERTIES:
#           :K:        [ ]
#           :VALUES:   [X]
#           :EFFECTS:  [X]
#           :IMPL:     [X]
#           :CL:       [X]
#           :END:

@defknown((intern("LET")[0], " ", ([(_notlead, "\n"), (_name, " ", _form)],),
            1, [(_notlead, "\n"), (_bound, _form)]))
def let():
        def nvalues(bindings, *body):            return 1   if not body else _ir_nvalues(body[-1])
        def nth_value(n, orig, bindings, *body): return nil if not body else _ir_nth_valueify_last_subform(n, orig)
        def binds(bindings, *body):
                bindings = (((b,    None) if symbolp(b) else
                             (b[0], b[1])) for b in bindings)
                return { variable: { _variable_binding(name, variable, None) for name, _ in bindings } }
        def lower(bindings, *body):
                # Unregistered Issue UNIFY-PRETTY-PRINTING-AND-WELL-FORMED-NESS-CHECK
                if not (_tuplep(bindings) and
                        every(_of_type((or_, symbol, (pytuple, symbol, t))))):
                        error("LET: malformed bindings: %s.", bindings)
                # Unregistered Issue PRIMITIVE-DECLARATIONS
                bindings_thru_defaulting = _py.tuple(_ensure_cons(b, nil) for b in bindings)
                names, values = _recombine((_py.list, _py.list), identity, bindings_thru_defaulting)
                compiled_value_pves = mapcar(_lower, values)
                if every(_tuple_expression_p, compiled_value_pves):
                        _compiler_debug_printf(" -- LET: simple all-expression LAMBDA case")
                        form = (apply, _ir(lambda_, (_optional,) + bindings_thru_defaulting, *body,
                                            dont_delay_defaults = t), nil)
                else:
                        _compiler_debug_printf(" -- LET: complex PROGN + SETQ + LET + LAMBDA case")
                        last_non_expr_posn = position_if_not(_tuple_expression_p, compiled_value_pves, from_end = t)
                        n_nonexprs = last_non_expr_posn + 1
                        temp_names = [ gensym("LET-NONEXPR-VAL") for i in _py.range(_py.len(bindings)) ]
                        # Unregistered Issue PYTHON-CANNOT-CONCATENATE-ITERATORS-FULL-OF-FAIL
                        form = ((progn,) +
                                _py.tuple((setq, n, v) for n, v in _py.zip(temp_names, values[:n_nonexprs])) +
                                (_ir(lambda_, (_optional,) + _py.tuple(_py.zip(names, temp_names + values[n_nonexprs:])), *body,
                                     dont_delay_defaults = t),))
                with progv({ _lexenv_: _lexenv(kind_varframe = { variable: { _variable_binding(name, variable, form)
                                                                             for name, form in bindings } }) }):
                        return _lower(form)
        def effects(bindings, *body):
                ## Unregistered Issue LET-EFFECT-COMPUTATION-PESSIMISTIC
                return _py.any(_ir_effects(f) for f in _py.tuple(x[1] for x in bindings) + body)
        def affected(bindings, *body):
                return _py.any(_ir_affected(f) for f in _py.tuple(x[1] for x in bindings) + body)

# FLET
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

@defknown((intern("FLET")[0], " ", ([(_notlead, "\n"), (_name, " ", ([(_notlead, " "), _form],),
                                                         1, [(_notlead, "\n"), _form])],),
            1, [(_notlead, "\n"), (_bound, _form)]))
def flet():
        def nvalues(bindings, *body):            return 1   if not body else _ir_nvalues(body[-1])
        def nth_value(n, orig, bindings, *body): return nil if not body else _ir_nth_valueify_last_subform(n, orig)
        def binds(bindings, *body):
                return { function: { _function_binding(name, function, None) for name, _, *__ in bindings } }
        def lower(bindings, *body):
                # Unregistered Issue COMPLIANCE-LAMBDA-LIST-DIFFERENCE
                # Unregistered Issue ORTHOGONALISE-TYPING-OF-THE-SEQUENCE-KIND-AND-STRUCTURE
                # Unregistered Issue LAMBDA-LIST-TYPE-NEEDED
                if not every(_of_type((partuple, symbol, pytuple)), bindings):
                        error("FLET: malformed bindings: %s.", bindings)
                # Unregistered Issue LEXICAL-CONTEXTS-REQUIRED
                tempnames = [ gensym(string(name)) for name, _, *__ in bindings ]
                form = ((let, _py.tuple((name, (progn,
                                                 (def_, tempname, lambda_list) + _py.tuple(fbody),
                                                 (ref, tempname)))
                                        for tempname, (name, lambda_list, *fbody) in _py.zip(tempnames, bindings))) +
                        body)
                with progv({ _lexenv_: _lexenv(kind_funcframe = { function: { _function_binding(name, function,
                                                                                                _fn.python_type(name))
                                                                              for name, _, *__ in bindings } }) }):
                        return _lower(form)
        def effects(bindings, *body):
                return _py.any(_ir_effects(f) for f in body)
        def affected(bindings, *body):
                return _py.any(_ir_affected(f) for f in body)

# LABELS
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

@defknown((intern("LABELS")[0], " ", ([(_notlead, "\n"), (_name, " ", ([(_notlead, " "), (_bound, _form)],),
                                                           1, [(_notlead, "\n"), (_bound, _form)])],),
            1, [(_notlead, "\n"), (_bound, _form)]))
def labels():
        def nvalues(bindings, *body):            return 1   if not body else _ir_nvalues(body[-1])
        def nth_value(n, orig, bindings, *body): return nil if not body else _ir_nth_valueify_last_subform(n, orig)
        def binds(bindings, *body):
                return { function: { _function_binding(name, function, None) for name, _, *__ in bindings } }
        def lower(bindings, *body):
                # Unregistered Issue COMPLIANCE-LAMBDA-LIST-DIFFERENCE
                # Unregistered Issue ORTHOGONALISE-TYPING-OF-THE-SEQUENCE-KIND-AND-STRUCTURE
                # Unregistered Issue LAMBDA-LIST-TYPE-NEEDED
                if not every(_of_type((partuple, symbol, pytuple))):
                        error("LABELS: malformed bindings: %s.", bindings)
                temp_name = gensym("LABELS")
                _compiler_debug_printf(" -- LABELS: to DEF + FLET")
                return (flet, ((temp_name, _py.tuple(),
                                _py.tuple((def_, name, lambda_list, body)
                                       for name, lambda_list, *body in bindings) +
                                body)),
                         (apply, temp_name, nil))
        def effects(bindings, *body):
                return _py.any(_ir_effects(f) for f in body)
        def affected(bindings, *body):
                return _py.any(_ir_affected(f) for f in body)

# FUNCTION
#         :PROPERTIES:
#         :K:        [X]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

#         Unregistered Issue COMPLIANCE-DEFUN-DEFMACRO-LAMBDA-LAMBDA-LIST.

_intern_and_bind_pynames("SETF")

@defknown((intern("FUNCTION")[0], " ", (or_, _name,
                                             (setf, _name))))
def function():
        ## A purely marker known, for now.
        ## Unregistered Issue COMPLIANCE-FUNCTION-NAMESPACE-SEPARATION
        def nvalues(_):            return 1
        def nth_value(n, orig, _): return orig if n is 0 else nil
        def lower(name):
                binding = symbol_value(_lexenv_).lookup_function(the(symbol, name))
                if not (binding or name.function):
                        simple_style_warning("undefined function: %s", name)
                return (ref, name)
        def effects(name):         return nil
        def affected(name):        return not symbol_value(_lexenv_).funcscope_binds_p(name)
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

# UNWIND-PROTECT
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

@defknown((intern("UNWIND-PROTECT")[0], " ", _form,
            1, [(_notlead, "\n"), _form]))
def unwind_protect():
        def nvalues(form, *unwind_body):            return _ir_nvalues(form)
        def nth_value(n, orig, form, *unwind_body): return (unwind_protect, _ir_nth_value(n, form)) + unwind_body
        def lower(form, *unwind_body):
                if not unwind_body:
                        return _lower(form)
                temp_name = gensym("PROTECTED-FORM-VALUE")
                pro_form, val_form = _lower((setq, temp_name, form))
                pro_unwind, val_unwind = _lower((progn,) + unwind_body)
                return ([("TryFinally",
                          # It's the SETQ's value we're discarding here, which is known to be safe -- a name reference.
                          pro_form, # Unregistered Issue COMPILER-VALUE-DISCARDABILITY-POLICY
                          # ..in contrast, here, barring analysis, we have no idea about discardability of val_unwind
                          pro_unwind + [("Expr", val_unwind)])],
                        _lower((ref, temp_name))[1])
        def effects(form, *unwind_body):
                return _py.any(_ir_effects(f) for f in (form,) + body)
        def affected(form, *unwind_body):
                return _py.any(_ir_affected(f) for f in (form,) + body)

# MACROLET
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

@defknown((intern("MACROLET")[0], " ", ([(_notlead, "\n"), (_name, " ", ([(_notlead, " "), _form],),
                                                             1, [(_notlead, "\n"), _form])],),
            1, [(_notlead, "\n"), (_bound, _form)]))
def macrolet():
        def nvalues(bindings, *body):            return 1   if not body else _ir_nvalues(body[-1])
        def nth_value(n, orig, bindings, *body): return nil if not body else _ir_nth_valueify_last_subform(n, orig)
        def binds(bindings, *body):
                return { macro: { _function_binding(name, macro, (lambda_list, body))
                                  for name, lambda_list, *body in bindings } }
        def lower(bindings, *body):
                ## By the time we get to the lowering stage, all macros have already been expanded.
                return (progn,) + body
        def effects(bindings, *body):
                return _py.any(_ir_effects(f) for f in body)
        def affected(bindings, *body):
                return _py.any(_ir_affected(f) for f in body)

# SYMBOL-MACROLET
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

@defknown((intern("SYMBOL-MACROLET")[0], " ", ([(_notlead, "\n"), (_name, " ", _form)],),
            1, [(_notlead, "\n"), (_bound, _form)]))
def symbol_macrolet():
        def nvalues(bindings, *body):            return 1   if not body else _ir_nvalues(body[-1])
        def nth_value(n, orig, bindings, *body): return nil if not body else _ir_nth_valueify_last_subform(n, orig)
        def binds(bindings, *body):
                return { symbol_macro: { _variable_binding(name, symbol_macro, form) for name, form in bindings } }
        def lower(bindings, *body):
                ## By the time we get to the lowering stage, all macros are already expanded.
                return (progn,) + body
        def effects(bindings, *body):
                return _py.any(_ir_effects(f) for f in body)
        def affected(bindings, *body):
                return _py.any(_ir_affected(f) for f in body)

# BLOCK
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

@defknown((intern("BLOCK")[0], " ", _name, ([(_notlead, "\n"), (_bound, _form)],)))
def block():
        def nvalues(name, *body):            return 1   if not body else _ir_nvalues(body[-1])
        def nth_value(n, orig, name, *body): return nil if not body else _ir_nth_valueify_last_subform(n, orig)
        def binds(name, *body):
                return { block: { _block_binding(name, t) } }
        def lower(name, *body):
                nonce = gensym("BLOCK-" + string(name))
                with progv({ _lexenv_: _lexenv(name_blockframe = { name: _block_binding(name, nonce) }) }):
                        return _lower((catch, (quote, nonce)) + body)
        def effects(name, *body):            return _py.any(_ir_effects(f) for f in body)
        def affected(name, *body):           return _py.any(_ir_affected(f) for f in body)

# RETURN-FROM
#         :PROPERTIES:
#         :K:        [X]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

@defknown((intern("RETURN-FROM")[0], " ", _name, (_maybe, " ", _form)))
def return_from():
        def nvalues(_, value):            return _ir_nvalues(value)
        def nth_value(n, orig, _, value): return _ir_nth_value(n, value)
        def lower(name, value):
                binding = symbol_value(_lexenv_).lookup_block(the(symbol, name))
                if not binding:
                        simple_program_error("return for unknown block: %s", name)
                return (throw, (quote, binding.value), value)
        def effects(_, value):            return _ir_effects(value)
        def affected(_, value):           return _ir_affected(value)

# CATCH
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

@defknown((intern("CATCH")[0], " ", _form, ([(_notlead, "\n"), (_bound, _form)],)))
def catch():
        def nvalues(_, *body):              return 1 if not body else _ir_nvalues(body[-1])
        def nth_value(n, orig, tag, *body): return (_ir_nth_valueify_last_subform(n, orig) if body             else
                                                    (progn, tag, nil)                      if _ir_effects(tag) else
                                                    nil)
        def lower(tag, *body):    return _ir_cl_module_call("_do_catch", tag, (lambda_, ()) + body)
        def effects(tag, *body):  return _ir_effects(tag) or _py.any(_ir_effects(f) for f in body)
        def affected(tag, *body): return _ir_affected(tag) or _py.any(_ir_affected(f) for f in body)

# THROW
#         :PROPERTIES:
#         :K:        [X]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

@defknown((intern("THROW")[0], " ", _form, (_maybe, " ", _form)))
def throw():
        def nvalues(_, value):            return _ir_nvalues(value)
        def nth_value(n, orig, _, value): return ((progn, tag, _ir_nth_value(value)) if _ir_effects(tag) else
                                                  _ir_nth_value(value))
        def lower(tag, value):            return _ir_cl_module_call("_throw", tag, value)
        def effects(tag, value):          return _ir_effects(tag) or _ir_effects(value)
        def affected(tag, value):         return _ir_affected(tag) or _ir_affected(value)

# TAGBODY
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [ ]
#         :CL:       [X]
#         :END:

#         Implementation strategy:

#         - bytecode patching, for function-internal jumps
#         - THROW, plus bytecode patching, for jumps outward of a lexically contained function
#           definition

@defknown((intern("TAGBODY")[0], (["\n", (or_, (_name,), (_bound, _form))],)))
def tagbody():
        def nvalues(*tags_and_forms):            return 1
        def nth_value(_, orig, *tags_and_forms): return (nil if not _py.any(_ir_effects(f) for f in tags_and_forms
                                                                            if not symbolp(f)) else
                                                         (progn, orig, nil))
        def binds(*tags_and_forms):              return { tag: { _tag_binding(name, t) for name in tags_and_forms
                                                                 if symbolp(name) } }
        def lower(*tags_and_forms):
                with progv({ _lexenv_: _lexenv(name_gotagframe = { name: _gotag_binding(name, t)
                                                                   for name in tags_and_forms
                                                                   if symbolp(name) }) }):
                        _lower((progn,) + body)
                return _not_implemented()
        def effects(*tags_and_forms):            return _py.any(_ir_effects(f) for f in tags_and_forms
                                                                if not symbolp(f))
        def affected(*tags_and_forms):           return _py.any(_ir_affected(f) for f in tags_and_forms
                                                                if not symbolp(f))

# GO
#         :PROPERTIES:
#         :K:        [X]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [ ]
#         :CL:       [X]
#         :END:

@defknown((intern("GO")[0], " ", _name))
def go():
        def nvalues(_):            return _ir_nvalues(value)
        def nth_value(n, orig, _): return None
        def lower(name):
                binding = symbol_value(_lexenv_).lookup_gotag(the(symbol, name))
                if not binding:
                        simple_program_error("attempt to GO to nonexistent tag: %s", name)
                return _not_implemented()
        def effects(_):            return nil
        def affected(_):           return nil

# EVAL-WHEN
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [ ]
#         :EFFECTS:  [ ]
#         :IMPL:     [ ]
#         :CL:       [X]
#         :END:

@defknown((intern("EVAL-WHEN")[0], " ", ([(_notlead, " "),
                                          (or_, (_maybe_once, _keyword("COMPILE-TOPLEVEL")),
                                                (_maybe_once, _keyword("LOAD-TOPLEVEL")),
                                                (_maybe_once, _keyword("EXECUTE")))],),
            1, [(_notlead, "\n"), _form]))
def eval_when():
        """eval-when (situation*) form* => result*

Arguments and Values:

situation---One of the symbols :COMPILE-TOPLEVEL, :LOAD-TOPLEVEL,
            :EXECUTE, COMPILE, LOAD, or EVAL.

The use of EVAL, COMPILE, and LOAD is deprecated.

forms---an implicit PROGN.

results---the values of the forms if they are executed, or NIL if they
          are not.

Description:

The body of an EVAL-WHEN form is processed as an implicit PROGN, but
only in the situations listed.

The use of the situations :COMPILE-TOPLEVEL (or COMPILE) and
:LOAD-TOPLEVEL (or LOAD) controls whether and when evaluation occurs
when EVAL-WHEN appears as a top level form in code processed by
COMPILE-FILE. See Section 3.2.3 (File Compilation).

The use of the situation :EXECUTE (or EVAL) controls whether
evaluation occurs for other EVAL-WHEN forms; that is, those that are
not top level forms, or those in code processed by EVAL or COMPILE.
If the :EXECUTE situation is specified in such a form, then the body
forms are processed as an implicit PROGN; otherwise, the EVAL-WHEN
form returns NIL.

EVAL-WHEN normally appears as a top level form, but it is meaningful
for it to appear as a non-top-level form.  However, the compile-time
side effects described in Section 3.2 (Compilation) only take place
when EVAL-WHEN appears as a top level form."""
        def lower(when, *body):
                ### Unregistered Issue DEPRECATED-SYMBOLS-CONSIDERED-INVALID
                ctop, ltop, exec = _parse_eval_when_situations(when)
                ## This handles EVAL-WHEN in non-top-level forms. (EVAL-WHENs in top
                ## level forms are picked off and handled by PROCESS-TOPLEVEL-FORM,
                ## so that they're never seen at this level.)
                _not_implemented()
                return (((progn,) + body) if exec else
                        _lower(nil))

# THE
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [ ]
#         :EFFECTS:  [X]
#         :IMPL:     [ ]
#         :CL:       [X]
#         :END:

@defknown((intern("THE")[0], " ", _form, " ", _form))
def the():
        def nvalues(type, form):            _not_implemented()
        def nth_value(n, orig, type, form): _not_implemented()
        def lower(type, form):              _not_implemented()
        def effects(type, form):            return _ir_effects(form)
        def affected(type, form):           return _ir_affected(form)

# LOAD-TIME-VALUE
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [ ]
#         :EFFECTS:  [ ]
#         :IMPL:     [ ]
#         :CL:       [X]
#         :END:

@defknown((intern("LOAD-TIME-VALUE")[0], " ", _form, (_maybe, " ", (_typep, (member, t, nil)))))
def load_time_value():
        def nvalues(form, read_only_p):            _not_implemented()
        def nth_value(n, orig, form, read_only_p): _not_implemented()
        def lower(form, read_only_p):              _not_implemented()
        def effects(form, read_only_p):            _not_implemented()
        def affected(form, read_only_p):           _not_implemented()

# LET*
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [ ]
#         :EFFECTS:  [ ]
#         :IMPL:     [ ]
#         :CL:       [X]
#         :END:

@defknown((intern("LET*")[0], " ", ([(_notlead, "\n"), (_name, " ", _form)],), ## XXX: wrong SEX!
            1, [(_notlead, "\n"), (_bound, _form)]),
          name = intern("LET*")[0])
def let_():
        def nvalues(bindings, *decls_n_body):            _not_implemented()
        def nth_value(n, orig, bindings, *decls_n_body): _not_implemented()
        def lower(bindings, *decls_n_body):              _not_implemented()
        def effects(bindings, *decls_n_body):            _not_implemented()
        def affected(bindings, *decls_n_body):           _not_implemented()

# PROGV
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [ ]
#         :CL:       [X]
#         :END:

@defknown((intern("PROGV")[0], " ", _form, " ", _form, ([(_notlead, "\n"), (_bound, _form)],)))
def progv():
        def nvalues(_, __, *body):                    return 1 if not body else _ir_nvalues(body[-1])
        def nth_value(n, orig, names, values, *body): return (_ir_nth_valueify_last_subform(n, orig) if body                  else
                                                              (progn, names, values, nil)  if (_ir_effects(names) or
                                                                                               _ir_effects(values)) else
                                                              nil)
        def lower(names, values, *body):
                _not_implemented()
                # ppro, pval = _lower(_ir_cl_module_call("_progv", ???))
                # bpro, bval = _lower((progn, body))
                # ("With", ("Call", ("Name", "progv", ("Load",))), [])
                # return _ir_cl_module_call("_do_catch", tag, (lambda_, ()) + body)
        def effects(names, values, *body):            return _py.any(_ir_effects(f) for f in (names, values) + body)
        def affected(names, values, *body):           return _py.any(_ir_affected(f) for f in (names, values) + body)

# LOCALLY
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [ ]
#         :EFFECTS:  [ ]
#         :IMPL:     [ ]
#         :CL:       [X]
#         :END:

@defknown((intern("LOCALLY")[0], (["\n", (_bound, _form)],)))
def locally():
        def nvalues(*decls_n_body):            _not_implemented()
        def nth_value(n, orig, *decls_n_body): _not_implemented()
        def lower(*decls_n_body):              _not_implemented()
        def effects(*decls_n_body):            _not_implemented()
        def affected(*decls_n_body):           _not_implemented()

# MULTIPLE-VALUE-PROG1
#         :PROPERTIES:
#         :IMPL:     [ ]
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :CL:       [X]
#         :END:

@defknown((intern("MULTIPLE-VALUE-PROG1")[0], " ", _form, (["\n", _form],)))
def multiple_value_prog1():
        def nvalues(first_form, *forms):            return _ir_nvalues(first_form)
        def nth_value(n, orig, first_form, *forms): return (_ir_nth_value(n, first_form) if not _py.any(_ir_effects(f) for f in forms) else
                                                            (lambda sym: ((let, ((sym, _ir_nth_value(n, first_form)))) +
                                                                            forms +
                                                                            (sym,)))(gensym("MV-PROG1-VALUE")))
        def lower(first_form, *forms):              _not_implemented()
        def effects(first_form, *forms):            return _ir_effects(first_form) or _py.any(_ir_effects(f) for f in forms)
        def affected(first_form, *forms):           return _ir_affected(first_form) or _py.any(_ir_affected(f) for f in forms)

# REF
#         :PROPERTIES:
#         :K:        [X]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [ ]
#         :END:

@defknown
def ref():
        def nvalues(_):            return 1
        def nth_value(n, orig, _): return orig if n is 0 else nil
        def lower(name):
                check_type(name, symbol)
                _compilation_note_symbol(name)
                return ([],
                        ("Name", _frost.full_symbol_name_python_name(name), ("Load",)))
        def effects(name):         return nil
        def affected(name):        return not _name_defined_as_constant_p(name)

# NTH-VALUE
#         :PROPERTIES:
#         :IMPL:     [X]
#         :K:        [X]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :CL:       [ ]
#         :END:

@defknown((intern("NTH-VALUE")[0], " ", _form, " ", _form))
def nth_value():
        def nvalues(_, __):    return 1
        def nth_value(n, orig, form_n, form):
                return ((nth_value, n, orig) if not (integerp(n) and integerp(form_n)) else ## Give up.  Too early?
                        nil                  if n != form_n and not _ir_effects(form)  else
                        (progn, form, nil)   if n != form_n                            else
                        _ir_nth_value(n, form)) ## We don't risque unbounded recursion here, so let's further analysis..
        def lower(n, form):    return _ir_cl_module_call("_values_frame_project", n, form)
        def effects(n, form):  return _ir_effects(n) or _ir_effects(form)
        def affected(n, form): return _ir_affected(n) or _ir_affected(form)

# DEF_
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [ ]
#         :END:

# 1. I'd rather much separate:
#    - named lambda compilation
#        def thunk():
#                def named(<lambda-list>):
#                        <body>
#                return named
#        thunk()
#    - installation of such named lambdas as global function definitions
#        emit a decorator? install_fdefinition
@defknown((intern("DEF_")[0], " ", _name, " ", ([(_notlead, " "), _form],),
            1, [(_notlead, "\n"), (_bound, _form)]),
          name = intern("DEF_")[0])
def def_():
        "A function definition with python-style lambda list (but homoiconic lisp-style representation)."
        def nvalues(_):             return 1
        def nth_value(n, orig, *_): return orig if n is 0 else (progn, orig, nil)
        def binds(name, lambda_list, *body, decorators = []):
                total, _, __ = _ir_prepare_lispy_lambda_list(lambda_list, "DEF %s" % name)
                return { variable: { _variable_binding(name, variable, None) for name in total } }
        def lower(name, lambda_list, *body, decorators = []):
                ## Urgent Issue COMPLIANCE-IR-LEVEL-BOUND-FREE-FOR-GLOBAL-NONLOCAL-DECLARATIONS
                # This is NOT a Lisp form, but rather an acknowledgement of the
                # need to represent a building block from the underlying system.
                cdef = _compiler_def(name   = name,
                                     parent = _compiling_def())
                toplevelp = _symbol_value(_compiler_toplevel_p_)
                with progv({_compiler_def_:        cdef,
                            _compiler_toplevel_p_: nil}):
                        check_type(name, (and_, symbol, (not_, keyword)))
                        # check_type(name, (or_, string, (and_, symbol, (not_, (satisfies, keywordp)))))
                        def try_compile():
                                # Unregistered Issue COMPLIANCE-REAL-DEFAULT-VALUES
                                total, args, defaults = _ir_prepare_lispy_lambda_list(lambda_list, "DEF %s" % name)
                                compiled_lambda_list = _lower_lispy_lambda_list("DEF %s" % name, *(args + defaults))
                                with _tail_position():
                                        # Unregistered Issue COMPILATION-SHOULD-TRACK-SCOPES
                                        pve = body_ret, _ = _lower((return_from, name, (progn, ) + body))
                                # body_exprp = _tuple_expression_p(preliminary_body_pve) # Why we'd need that, again?
                                # Unregistered Issue CRUDE-SPECIAL-CASE-FOR-BOUND-FREE
                                deco_vals = []
                                for pro_deco, val_deco in (_lower(d) for d in decorators):
                                        if pro_deco:
                                                error("in DEF %s: decorators must lower to python expressions.", name)
                                        deco_vals.append(val_deco)
                                return ([("FunctionDef", _frost.full_symbol_name_python_name(name), compiled_lambda_list,
                                                         body_ret,
                                                         deco_vals),
                                         # ("Assign", [("Name", full_name, ("Store",))], ("Name", short_name, ("Load",)))
                                         ],
                                        _lower(name)[1])
                        ## Xtnls feedback loop stabilisation scheme.
                        ##
                        ## This looks fairly ridiculous, but this is reality for you:
                        ##  - it's impossible to know externals before compilation
                        ##    - determined by walking the resulting atree
                        ##  - you need to know externals before compilation
                        ##    - at least one optimisation (LET) depends on this
                        ##
                        ## Quietly hoped to be the only parameter requiring such beforehand knowledge.
                        xtnls_guess, xtnls_actual, try_ = None, _py.set(), 0
                        while xtnls_guess != xtnls_actual:
                                cdef.xtnls = xtnls_guess = xtnls_actual
                                _compiler_debug_printf(" -- DEF: try %d", try_)
                                result = try_compile()
                                _compiler_debug_printf(" -- DEF: result\n%s", result)
                                xtnls_actual = _tuple_xtnls(result)
                                try_ += 1
                        return result
        def effects(name):         return t
        def affected(name):        return nil

# LAMBDA
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [ ]
#         :END:

@defknown((intern("LAMBDA")[0], " ", ([(_notlead, " "), _form],),
            1, [(_notlead, "\n"), (_bound, _form)]))
def lambda_():
        def nvalues(*_):            return 1
        def nth_value(n, orig, *_): return orig if n is 0 else nil
        def binds(lambda_list, *body, dont_delay_defaults = nil):
                total, _, __ = _ir_prepare_lispy_lambda_list(lambda_list, "LAMBDA")
                return { variable: { _variable_binding(name, variable, None) for name in total } }
        def lower(lambda_list, *body, dont_delay_defaults = nil):
                # Unregistered Issue COMPLIANCE-LAMBDA-LIST-DIFFERENCE
                # Unregistered Issue COMPLIANCE-REAL-DEFAULT-VALUES
                # Unregistered Issue COMPILATION-SHOULD-TRACK-SCOPES
                # Unregistered Issue SHOULD-HAVE-A-BETTER-WAY-TO-COMPUTE-EXPRESSIBILITY
                # Unregistered Issue EMPLOY-THUNKING-TO-REMAIN-AN-EXPRESSION
                preliminary_body_pve = _lower((progn,) + body)
                body_exprp = _tuple_expression_p(preliminary_body_pve)
                if body_exprp:
                        total, args, defaults = _ir_prepare_lispy_lambda_list(lambda_list, "LAMBDA", allow_defaults = t)
                        (fixed, optional, rest, keys, restkey), (optdefs, keydefs) = args, defaults
                        if not (optional or keys):
                                _compiler_debug_printf(" -- LAMBDA: simple")
                                return ([],
                                        ("Lambda", _lower_lispy_lambda_list("LAMBDA", *(args + defaults)), preliminary_body_pve[1]))
                        elif rest or restkey:
                                _not_implemented("rest/restkey-ful defaulting lambda list")
                        elif dont_delay_defaults:
                                # duplicate code here, but the checking "issue" is not understood well-enough..
                                _compiler_debug_printf(" -- LAMBDA: undelayed defaults")
                                return ([],
                                        ("Lambda", _lower_lispy_lambda_list("LAMBDA", *(args + defaults)), preliminary_body_pve[1]))
                        else:
                                ## Delay evaluation of default values.
                                def defaulting_expr(arg, default):
                                        return (if_, (eq, arg, (_name, "None")),
                                                     default,
                                                     arg)
                                _compiler_debug_printf(" -- LAMBDA: defaulting LAMBDA + LET")
                                return (lambda_, _py.tuple(fixed + [_optional] + optional + keys),
                                         (let, _py.tuple((arg, defaulting_expr(arg, default))
                                                         for arg, default in _py.zip(optional + keys,
                                                                                     optdefs + keydefs))) +
                                           body)

                else:
                        _compiler_debug_printf(" -- LAMBDA: non-expression FLET")
                        func_name = _gensym("LET-BODY-")[0]
                        return (flet, ((func_name, lambda_list) + body,),
                                 (function, func_name))
        def effects(*_):            return nil
        def affected(*_):           return nil

# APPLY
#         :PROPERTIES:
#         :IMPL:     [X]
#         :K:        [X]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :CL:       [ ]
#         :END:

@defknown((intern("APPLY")[0], " ", _form, " ", _form, [" ", _form]))
def apply():
        def nvalues(func, _, *__):            return _ir_function_form_nvalues(func)
        def nth_value(n, orig, func, _, *__): return _ir_function_form_nth_value_form(n, func, orig)
        def lower(func, arg, *args):
                def pycall_p(x): return stringp(x) or (_tuplep(x) and _py.all(stringp(x) for x in x))
                ## Unregistered Issue IMPROVEMENT-APPLY-COULD-VALIDATE-CALLS-OF-KNOWNS
                with _no_tail_position():
                        func_pro, func_val = (([], _attr_chain_atree((func,) if stringp(func) else func))
                                              # Unregistered Issue ENUMERATE-COMPUTATIONS-RELIANT-ON-STRING-APPLY
                                              # - quote_ compilation in lower_
                                              if pycall_p(func) else
                                              _lower(func))
                fixed, rest = (((),                 arg)       if not args                  else
                               ((arg,) + args[:-1], args[-1]))
                ## Note, how the test below is too weak to be useful:
                ## the comparison against a literal NIL is much weaker than a NULL type membership test.
                ## Therefore, the important evolutionary question, is what kind of preparations are
                ## required to make such type analysis viable.
                no_varargs = rest == nil
                with _no_tail_position():
                        arg_pvs = mapcar(_lower, fixed + (rest,))
                if every(_tuple_expression_p, arg_pvs):
                        _compiler_debug_printf(" -- APPLY: simple case")
                        return (func_pro,
                                ("Call", func_val, mapcar(second, arg_pvs[:-1]), [], (arg_pvs[-1] if not no_varargs else
                                                                                      None)))
                else:
                        _compiler_debug_printf(" -- APPLY: complex LET + APPLY")
                        func_binding, func_expr = (((),                     func) if func_pro else
                                                   (lambda func_name:
                                                     (((func_name, func),), (ref, func_name)))(gensym("APPLY-FUNCNAME")))
                        temp_names = _py.tuple(_gensyms(n = _py.len(arg_pvs) - no_varargs, x = "APPLY-ARG"))
                        arg_exprs = temp_names + ((nil,) if no_varargs else ())
                        return (let, func_binding + _py.tuple(_py.zip(temp_names, args)),
                                 (apply, func_expr) + arg_exprs)
        def effects(func, arg, *args):
                return (_py.any(_ir_effects(arg) for arg in (func, arg) + args) or
                        _ir_depending_on_function_properties(func, lambda fn, effects: effects, "effects"))
        def affected(func, arg, *args):
                return (_py.any(_ir_affected(arg) for arg in (func, arg) + args) or
                        _ir_depending_on_function_properties(func, lambda fn, affected: affected, "affected"))

# Known tests

_intern_and_bind_pynames("COND")

def applyification():
        return _macroexpander_inner(_macroexpander, dict(), None,
                                    (cond,),
                                    nil,  ## The pattern will be discarded out of hand, anyway.
                                    (None, None))
bound_good, result_good, nofail = _runtest(applyification,
                                           {},
                                           (apply, (function, cond), nil))
_results()
assert(nofail)
assert(bound_good)
assert(result_good)
print("; APPLYIFICATION: passed")

# Engine and drivers: %LOWER, @LISP and COMPILE

def _dump_form(form):
        _debug_printf("%s\n", "*** " + "\n*** ".join(_pp_sex(form).split("\n")))

# Urgent Issue COMPILER-MACRO-SYSTEM
def _lower(form):
        # - tail position tracking
        # - scopes
        # - symbols not terribly clear
        # - proper quote processing
        if _debugging_compiler():
                _compiler_track_compiled_form(form)
                # _debug_printf(";;; compiling:\n%s", _pp_sex(form))
                # _compiler_report_context()
        def _rec(x):
                ## XXX: what are the side-effects?
                ## NOTE: we are going to splice unquoting processing here, as we must be able
                ## to work in READ-less environment.
                _debug_printf_if(_debugging_compiler(), ";;;%s lowering:\n%s%s", _sex_space(-3, ";"), _sex_space(), _pp_sex(x))
                if listp(x): ## nil or _tuplep
                        def noisep(x): return x in [symbol]
                        def puntedp(x):
                                "Whether the primitive compiler requested recompilation."
                                return x and _tuplep(x) and symbolp(x[0])
                        def maybe_call_primitive_compiler(name, forms, args):
                                known = _find_known(name)
                                if not known:
                                        return nil, nil
                                _debug_printf_if(_debugging_compiler() and not noisep(name),
                                                 "%s>>> %s\n%s%s", _sex_space(), name, _sex_space(), ("\n" + _sex_space()).join(_pp_sex(f) for f in forms))
                                ret = known.lower(*forms, **_alist_hash_table(args))
                                if puntedp(ret):
                                        _debug_printf_if(_debugging_compiler() and not noisep(name),
                                                         "%s===========================\n"
                                                         "%s\n%s-------------------------->\n%s\n"
                                                         "%s...........................",
                                                         _sex_space(),
                                                         _sex_space() + _pp_sex((name,) + forms),
                                                         _sex_space(),
                                                         _sex_space() + _pp_sex(ret),
                                                         _sex_space())
                                        return _sex_deeper(4, lambda: _rec(ret)), t
                                else:
                                        not noisep(name) and _compiler_debug_printf("=== %s done", name)
                                        return ret, t
                        if not x: ## Either an empty list or NIL itself.
                                return _rec((nil,))
                        if symbolp(x[0]):
                                argsp, form, args = _maybe_ir_args(x)
                                # Urgent Issue COMPILER-MACRO-SYSTEM
                                ret, primitivep = maybe_call_primitive_compiler(form[0], form[1:], args)
                                if primitivep:
                                        # Unregistered Issue COMPILE-CANNOT-EVEN-MENTION-KWARGS
                                        return ret
                                form, expanded = macroexpand(x)
                                if expanded:
                                        return _rec(form)
                                # basic function call
                                return _rec((apply,) + form + (nil,))
                        elif (_tuplep(x[0]) and x[0] and x[0][0] is lambda_):
                                return _rec((apply,) + x + (nil,))
                        elif stringp(x[0]): # basic function call
                                return _rec((apply,) + x + (nil,))
                        else:
                                error("Invalid form: %s.", princ_to_string(x))
                elif symbolp(x) and not constantp(x):
                        return _attr_chain_atree(_ir_cl_module_name("nil"))
                else:
                        # NOTE: we don't care about quoting here, as constants are self-evaluating.
                        atree, successp = _try_atreeify_constant(x) # NOTE: this allows to directly pass through ASTs.
                        if successp:
                                # ..in turn, this requires the atree astifier to directly pass through ASTs,
                                # or, alternatively (and more desirably), we could call _try_atreeify_constant.
                                return ([],
                                        atree)
                        else:
                                error("UnASTifiable non-symbol/tuple %s.", princ_to_string(x))

        ## XXX: what about side-effects?
        pv = _rec(form)
        # _debug_printf_if(_debugging_compiler(),
        #                  ";;; compilation atree output for\n%s\n;;;\n;;; Prologue\n;;;\n%s\n;;;\n;;; Value\n;;;\n%s",
        #                  _pp_sex(form), *pv)
        expected_return_type = (pytuple, pylist, (maybe, (partuple, string)))
        if not typep(pv, expected_return_type):
                error("While lowering %s: returned value %s is not TYPEP %s.", form, pv, expected_return_type)
        return pv

def _do_compile(form, lexenv = nil):
        _debug_printf_if(_debugging_compiler(), ";;;%s compiling:\n%s%s", _sex_space(-3, ";"), _sex_space(), _pp_sex(form))
        macroexpanded = macroexpand_all(form, env = lexenv)
        _debug_printf_if(_debugging_compiler(), ";;;%s macroexpanded:\n%s%s", _sex_space(-3, ";"), _sex_space(), _pp_sex(macroexpanded))
        return _lower(macroexpanded)

def function_lambda_expression(function_):
        """function-lambda-expression function

=> LAMBDA-EXPRESSION, CLOSURE-P, NAME

Arguments and Values:

FUNCTION---a function.

LAMBDA-EXPRESSION---a lambda expression or NIL.

CLOSURE-P---a generalized boolean.

NAME---an object.

Description:

Returns information about function as follows:

The primary value, LAMBDA-EXPRESSION, is function's defining lambda
expression, or NIL if the information is not available.  The lambda
expression may have been pre-processed in some ways, but it should
remain a suitable argument to COMPILE or FUNCTION.  Any implementation
may legitimately return NIL as the LAMBDA-EXPRESSION of any FUNCTION.

The secondary value, CLOSURE-P, is NIL if FUNCTION's definition was
enclosed in the null lexical environment or something non-NIL if
FUNCTION's definition might have been enclosed in some non-null
lexical environment.  Any implementation may legitimately return true
as the CLOSURE-P of any function.

The tertiary value, NAME, is the ``name'' of FUNCTION.  The name is
intended for debugging only and is not necessarily one that would be
valid for use as a name in DEFUN or FUNCTION, for example.  By
convention, NIL is used to mean that FUNCTION has no name.  Any
implementation may legitimately return NIL as the name of any
FUNCTION."""
        return _values_frame(*(gethash(slot, the(function, function_).__dict__, default)[0]
                               for slot, default in [("lambda_expression", nil),
                                                     ("closure_p",         t),
                                                     ("name",              nil)]))

##
### What is the status of this?
def _make_compilation_unit():
        unit = _py.dict(conditions = [])
        return unit
_compilation_unit = _defwith("_compilation_unit",
                             lambda *_: _dynamic_scope_push({_compilation_unit_: _make_compilation_unit()}
                                                            if not boundp(_compilation_unit_) else
                                                            make_hash_table()),
                             lambda *_: _dynamic_scope_pop())
def _compilation_unit_set(k, v):
        _symbol_value(_compilation_unit_)[k] = v
def _compilation_unit_get(k):
        return _symbol_value(_compilation_unit_)[k]

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
__def_sources__ = _without_condition_system(lambda: _collections.OrderedDict())
__def_sources__[""] = "" # placeholder
__def_sources_filename__ = "<lisp>"
def _lisp_add_def(name, source):
        if name in __def_sources__:
                del __def_sources__[name]
        __def_sources__[name] = source
        total = "\n".join(__def_sources__.values())
        linecache.cache[__def_sources_filename__] = _py.len(total), _py.int(time.time()), total.split("\n"), __def_sources_filename__


def lisp(body):
        def read_python_toplevel_name(f):
                symbol_name = _frost.python_name_lisp_symbol_name(f.__name__)
                symbol = _intern(symbol_name)[0]
                return symbol, symbol_name, f.__name__
        ## What should it be like?
        ##  - COMPILE-FILE + LOAD
        ##  - COMPILE-TOPLEVEL-FORM + ?
        symbol, symbol_name, _ = read_python_toplevel_name(body)
        args_ast, body_ast = _function_ast(body)
        if _py.len(body_ast) > 1:
                error("In LISP %s: toplevel definitions are just that: toplevel definitions. "
                      "No more than one toplevel form is allowed per definition.", symbol)
        form = _read_ast(body_ast[0])
        __def_allowed_toplevels__ = _py.set([defun, defmacro])
        if form[0] not in __def_allowed_toplevels__:
                error("In LISP %s: only toplevels in %s are allowed.",
                      form[0], __def_allowed_toplevels__)
        name, warnedp, failedp, _ = _compile_toplevel_def_in_lexenv(symbol, form, _make_null_lexenv(),
                                                                    globalp = t,
                                                                    macrop = form[0] is defmacro,
                                                                    lambda_expression = (lambda_,) + form[2:])
        if failedp:
                error("Compilation failed: errors while compiling %s.", name)
        elif warnedp:
                pass # Unregistered Issue LISP-TOPLEVEL-PROCESSOR-IGNORES-WARNINGS
        return name

def compile(name, definition = None):
        # Multiple values would have reduced, if not obviated, the need for such..
        # Research Issue COMPLIANCE-SPECIFIED-FUNCTIONS-RETURNING-ADDITIONAL-VALUES
        return _compile(name, definition)[:3]

def _compile(name, definition = None):
        """compile name &optional definition => FUNCTION, WARNINGS-P, FAILURE-P

Arguments and Values:

NAME---a function name, or NIL.

DEFINITION---a lambda expression or a function.  The default is the
function definition of NAME if it names a function, or the macro
function of name if it names a macro.  The consequences are undefined
if no definition is supplied when the name is NIL.

FUNCTION---the function-name, or a compiled function.

WARNINGS-P---a generalized boolean.

FAILURE-P---a generalized boolean.

Description:

Compiles an interpreted function.

COMPILE produces a compiled function from DEFINITION.  If the
definition is a lambda expression, it is coerced to a function.  If
the DEFINITION is already a compiled function, COMPILE either produces
that function itself (i.e., is an identity operation) or an equivalent
function.

If the NAME is NIL, the resulting compiled function is returned
directly as the primary value.  If a non-NIL NAME is given, then the
resulting compiled function replaces the existing function definition
of NAME and the NAME is returned as the primary value; if NAME is a
symbol that names a macro, its macro function is updated and the NAME
is returned as the primary value.

Literal objects appearing in code processed by the COMPILE function
are neither copied nor coalesced.  The code resulting from the
execution of COMPILE references objects that are EQL to the
corresponding objects in the source code.

COMPILE is permitted, but not required, to establish a handler for
conditions of type ERROR.  For example, the handler might issue a
warning and restart compilation from some implementation-dependent
point in order to let the compilation proceed without manual
intervention.

The secondary value, WARNINGS-P, is false if no conditions of type
ERROR or WARNING were detected by the compiler, and true otherwise.

The tertiary value, FAILURE-P, is false if no conditions of type ERROR
or WARNING (other than STYLE-WARNING) were detected by the compiler,
and true otherwise."""
        ## 1. Elect a name, for future retrieval of the compiled
        ##    function from the resulting module by N-O-W-B-C-L-A-N-T.
        ## 2. Choose a lambda expression, and handle exceptions.
        ## 3. Inevitably, punt to N-O-W-B-C-L-A-N-T.
        if _tuplep(definition):
                lambda_expression = the((partuple, (eql, lambda_), pytuple), definition)
        else:
                fun = definition or macro_function(name) or fdefinition(name)
                _, lambda_expression, _, _ = function_lambda_expression(fun)
                if not definition:
                        # Not much we can do, but return the original function.
                        return fun, nil, nil, nil
        final_name = the(symbol, name) or gensym("compiled_lambda")
        # Must has a name, for two reasons:
        #  - _ast_compiled_name() requires one
        #  - THERE-EXIST lambdas non-expressible in Python
        # Coerce the lambda to a named def, for _ast_compiled_name purposes:
        return _compile_lambda_as_named_toplevel(final_name, lambda_expression, _make_null_lexenv(),
                                                 globalp = not not name,
                                                 macrop = name and not not macro_function(name))

def _compile_in_lexenv(name, lambda_expression, lexenv):
        def _convert_lambda_to_defun(name, lambda_expression):
                return (defun, the(symbol, name)) + lambda_expression[1:]
        final_name = the(symbol, name) or gensym("compiled_lambda")
        return _compile_toplevel_def_in_lexenv(final_name, _convert_lambda_to_defun(final_name, lambda_expression), lexenv,
                                               lambda_expression = lambda_expression)

def _compile_lambda_as_named_toplevel(name, lambda_expression, lexenv, globalp = None, macrop = None):
        "There really is no other way.  Trust me.  Please."
        def _convert_lambda_to_def(name, lambda_expression):
                return (def_, the(symbol, name)) + lambda_expression[1:]
        _, arglist, *body = lambda_expression
        fn = _fn.python_type(name = name,
                             arglist = arglist, body = body,
                             globalp = globalp)
        ## fn.clear_dependencies() -- not needed, for as long as we create functions anew (HAR, um, HAR, I suppose..)
        with progv({ _compiler_fn_: fn }):
                return _compile_toplevel_def_in_lexenv(
                        ## This decorator-passing scheme is fairly archaic -- we would be better served by macros.
                        name, _ir_args_when(globalp, _convert_lambda_to_def(name, lambda_expression),
                                            decorators = [(ref, _set_macro_definition) if macrop else
                                                          (ref, _set_function_definition)]),
                        lexenv,
                        globalp = globalp, macrop = globalp and macrop,
                        lambda_expression = lambda_expression)

def _compile_toplevel_def_in_lexenv(name, form, lexenv, globalp = nil, macrop = nil, lambda_expression = None):
        ## Actually, only DEFUN and DEFMACRO would work at the moment, not DEF_.
        def _in_compilation_unit():
                pro, value = _do_compile(form, lexenv) # We're only interested in the resulting DEF.
                cu_pro = _compilation_unit_prologue()
                final_pv = cu_pro + pro, value
                if _py.len(pro) != 1:   # The FunctionDef and the symbol Assign
                        _debug_printf("*** invariant failed - %s prologue - while compiling form:", ("no" if not pro else
                                                                                                     "long"))
                        _dump_form(form)
                        if pro:
                                _debug_printf("*** prologue:")
                                mapc(_dump_form, pro)
                        assert(_py.len(pro) == 1)
                pro_ast = mapcar(_compose(_ast_ensure_stmt, _atree_ast), _tuplerator(final_pv))
                import more_ast
                more_ast.assign_meaningful_locations(pro_ast)
                if _debugging_compiler():
                        import more_ast
                        _debug_printf("cu_pro: %s\n", cu_pro)
                        _debug_printf(";;; In C-T-D-I-L: Lisp ================\n%s:\n;;; Python ------------->\n%s\n;;; .....................\n",
                                      _pp_sex(form), "\n".join(more_ast.pp_ast_as_code(x, line_numbers = t)
                                                               for x in pro_ast))
                        ############################ This is an excess newline, so it is a bug workaround.
                        ############################ Unregistered Issue PP-AST-AS-CODE-INCONSISTENT-NEWLINES
                        if typep(pro_ast[0], _ast.FunctionDef):
                                _debug_printf("type of ast: %s\ndecorators: %s", type_of(pro_ast[0]), pro_ast[0].decorator_list)
                # Unregistered Issue COMPLIANCE-WITH-COMPILATION-UNIT-WARNINGS-P-FAILURE-P
                acn = _ast_compiled_name(_frost.full_symbol_name_python_name(name),
                                         *pro_ast,
                                         globals = _py.globals(),
                                         locals  = _py.locals())
                sym = the(symbol, acn)
                func = the(function, symbol_function(sym))
                # Unregistered Issue COMPILE-PYSTAGE-ERROR-CHECKING
                # Feed FUNCTION-LAMBDA-EXPRESSION:
                if _specifiedp(lambda_expression):
                        func.lambda_expression = lambda_expression
                func.closure_p         = nil
                func.name              = name # Debug name, as per F-L-E spec.
                warnings, style_warnings, errors = [], [], []
                ## XXX: was too lazy to fix compilation unit stuff, so commented out..
                # for cond in _compilation_unit_get("conditions"):
                #         if typep(cond, error):
                #                 errors.append(cond)
                #         elif typep(cond, style_warning):
                #                 style_warnings.append(cond)
                #         elif typep(cond, warning):
                #                 warnings.append(cond)
                warnedp, failedp = (not not (errors or warnings or style_warnings),
                                    not not (errors or warnings))
                return ((name if globalp else func),
                        warnedp,
                        failedp,
                        pro)
        with progv({_lexenv_: lexenv}):
                return with_compilation_unit(_in_compilation_unit)

# @LISP tests

@lisp
# ((intern("DEFUN")[0], " ", _name, " ", ([(_notlead, " "), _name],),
#    1, [(_notlead, "\n"), (_bound, _form)]))
def DEFUN(name, lambda_list, *body):
        (defmacro, defun, (name, lambda_list, _body, body),
          (quaquote,
            (progn,
              ## SBCL has a :COMPILE-TOPLEVEL part, but it's not very clear what we need in this respect.
              (eval_when, (_load_toplevel, _execute),
                (apply, ("cl", "_set_function_definition"),
                        (lambda_, (comma, lambda_list), (splice, body)),
                        nil)))))

@lisp
def cond(*clauses):
        (defmacro, cond, (_rest, clauses),
         (if_, (not_, clauses),
          nil,
          (let, ((clause, (first, clauses)),
                 (rest, (rest, clauses))),
           (let, ((test, (first, clause)),
                  (body, (rest, clause))),
            (quaquote, (if_, (unquote, test),
                        (progn, (splice, body)),
                        (cond, (splice, rest))))))))

@lisp
def fdefinition(name):
        (defun, fdefinition, (name,),
         (symbol_function, (the, symbol, name)))
describe(fdefinition)

# Source info (rudimentary)

@defclass
class _source_info():
        def __init__(self, pathname):
                self.pathname = pathname

# EVAL (incl. %EVAL-TLF)

_string_set("*EVAL-SOURCE-CONTEXT*", nil)
_string_set("*EVAL-TLF-INDEX*", nil)
_string_set("*EVAL-SOURCE-INFO*", nil)

def _simple_eval(expr, lexenv):
        lam = (lambda_, _py.tuple()) + expr
        fun = _compile_in_lexenv(nil, lam, lexenv)[0]
        # _debug_printf("got: %s", fun)
        return fun()

def _simple_eval_in_lexenv(original_exp, lexenv):
        exp = macroexpand(original_exp, lexenv)
        if symbolp(exp):
                return symbol_value(exp)
        elif _tuplep(exp):
                name = car(exp)
                n_args = _py.len(exp) - 1
                if   name is function:        _not_implemented("%SIMPLE-EVAL-IN-LEXENV FUNCTION case")
                elif name is quote:           return the((pytuple, symbol, t), exp)[1]
                elif name is setq:            _not_implemented("%SIMPLE-EVAL-IN-LEXENV SETQ case")
                elif name is progn:           _not_implemented("%SIMPLE-EVAL-IN-LEXENV PROGN case")
                elif name is eval_when:       _not_implemented("%SIMPLE-EVAL-IN-LEXENV EVAL-WHEN case")
                elif name is locally:         _not_implemented("%SIMPLE-EVAL-IN-LEXENV LOCALLY case")
                elif name is macrolet:        _not_implemented("%SIMPLE-EVAL-IN-LEXENV MACROLET case")
                elif name is symbol_macrolet: _not_implemented("%SIMPLE-EVAL-IN-LEXENV SYMBOL-MACROLET case")
                elif name is if_:             return _eval_in_lexenv((exp[2] if _eval_in_lexenv(exp[1], lexenv) else
                                                                      nil    if _py.len(exp) == 3               else
                                                                      exp[3]), lexenv)
                elif name in [let, let_]:     return _simple_eval(exp, lexenv)
                else:                         return _simple_eval(exp, lexenv) # This directly invoked a function, whenever
                                                                               # it was already in the FNDB.  We just punt.
        else:
                return exp

def _eval_in_lexenv(exp, lexenv):
        return _simple_eval_in_lexenv(exp, lexenv)

def _do_eval(exp, tlf_index, source_info, lexenv):
        with progv({_eval_source_context_: exp,
                    _eval_tlf_index_:      tlf_index,
                    _eval_source_info_:    source_info}):
                return _eval_in_lexenv(exp, lexenv)

def _eval_tlf(original_exp, tlf_index, lexenv = None):
        return _do_eval(original_exp, tlf_index, _symbol_value(_source_info_),
                        _defaulted(lexenv, _make_null_lexenv()))

def eval(original_exp):
        return _do_eval(original_exp, nil, nil, _make_null_lexenv())

# File compiler, as stolen from SBCL (%EVAL-COMPILE-TOPLEVEL / %PROCESS-TOPLEVEL-FORM and COMPILE-FILE)

def _compiler_mumble(control, *args):
        _debug_printf(control, *args)

_string_set("*TOP-LEVEL-FORM-NOTED*", nil)

_string_set("*COMPILE-PRINT*", t)
_string_set("*COMPILE-VERBOSE*", t)
_string_set("*COMPILE-PROGRESS*", nil) # Not CL.

_string_set("*COMPILE-FILE-PATHNAME*", nil)
_string_set("*COMPILE-FILE-TRUENAME*", nil)

_string_set("*COMPILE-OBJECT*", nil)
_string_set("*COMPILE-TOPLEVEL-OBJECT*", nil)

# with_compilation_unit() is above, due to dependency

# name-reserved-by-ansi-p name kind
# summarize-compilation-unit abort-p
# with-compilation-values body
# ir1-optimize-until-done component
# dfo-as-needed component
# ir1-phases component
# %compile-component component
# delete-if-no-entries component
# compile-component component
# clear-constant-info
# clear-ir1-info component
# clear-stuff
# describe-component component *standard-output*
# describe-ir2-component component
# def!struct file-info
# def!struct source-info
# make-file-source-info file external-format
# make-lisp-source-info form parent
# get-toplevelish-file-info source-info
# read-for-compile-file stream position
# get-source-stream info
# close-source-info info
# defmacro do-forms-from-info (forms keys) info
# sub-sub-compile-file info
# find-source-root index info
# convert-and-maybe-compile form path

def _preprocessor_macroexpand_1(form):
        def _macroexpand_1(form, env):
                ## Like MACROEXPAND-1, but takes care not to expand special forms.
                # (if (or (atom form)
                #      (let ((op (car form)))
                #       (not (and (symbolp op) (sb!xc:special-operator-p op)))))
                #  (sb!xc:macroexpand-1 form env)
                #  (values form nil))
                return (macroexpand_1(form, env) if atom(form) or not (symbolp(car(form)) and special_operator_p(car(form)))
                                                 else _values_frame(form, nil))
        ## Macroexpand FORM in the current environment with an error handler.
        ## We only expand one level, so that we retain all the intervening
        ## forms in the source path.
        try:
                return _macroexpand_1(form, _symbol_value(_lexenv_))
        except error.python_type as condition:
                _compiler_error("(during macroexpansion of %s)\n%s", format(nil, "%s", form), condition)

## The %CONVERT-AND-MAYBE-COMPILE function returns whatever LOWER returns.
## The PROCESS-* functions return possibly empty lists of %C-A-M-C return values.
def _process_toplevel_progn(forms, path, compile_time_too):
        return mapcan(lambda f: _process_toplevel_form(f, path, compile_time_too), forms)

def _process_toplevel_locally(body, path, compile_time_too, vars = [], funs = []):
        forms, decls = _parse_body(body, doc_string_allowed = nil, toplevel = t)[:2]
        with progv({_lexenv_: _process_decls(decls, vars, funs)}):
                return _process_toplevel_progn(forms, path, compile_time_too)

_eval_when_ordered_keywords = _compile_toplevel, _load_toplevel, _execute
_eval_when_keywords = _py.set(_eval_when_ordered_keywords)
def _parse_eval_when_situations(situ_form):
        if not (_tuplep(situ_form) and _py.set(situ_form) == _eval_when_keywords):
                error("In EVAL-WHEN: the first form must be a list of following keywords: %s.", _eval_when_ordered_keywords)
        return [x in situ_form for x in _eval_when_ordered_keywords]

# functional-components f
# make-functional-from-toplevel-lambda lambda-expression name path
# %compile lambda-expression *compile-object* name path
# process-toplevel-cold-fset name lambda-expression path

def _note_top_level_form(form, finalp = nil):
        if _symbol_value(_compile_print_):
                if not _symbol_value(_top_level_form_noted_):
                        _compiler_mumble("; compiling %s\n", _pp_sex(form[:min(2, _py.len(form))]))
                        return form
                else:
                        return _symbol_value(_top_level_form_noted_)

## This returns a list of %C-A-M-C retvals, just as PROCESS-* do.
def _eval_compile_toplevel(body, path):
        ## Handle the evaluation the a :COMPILE-TOPLEVEL body during
        ## compilation. Normally just evaluate in the appropriate
        ## environment, but also compile if outputting a CFASL.
        eval_tlf((progn,) + body, source_path_tlf_number(path), _symbol_value(_lexenv_))
        cto = _symbol_value(_compile_toplevel_object_)
        if cto:
                with progv({_compile_object_: cto}):
                        return [_convert_and_maybe_compile((progn,) + body, path)]
        else:
                return []

def _process_toplevel_form(form, path, compile_time_too):
        def funcall_in_macrolet_lexenv(bindings, fn):
                with progv({_lexenv_: _not_implemented()}):
                        return fn()
        def funcall_in_symbol_macrolet_lexenv(bindings, fn):
                with progv({_lexenv_: _not_implemented()}):
                        return fn()
        path = _get_source_path(form) or cons(form, path)
        def default_processor(form):
                with progv({_toplevel_form_noted_: _note_top_level_form(form)}):
                        expanded = _values_frame_project(0, _preprocessor_macroexpand_1(form))
                        if expanded is form:
                                if compile_time_too:
                                        # Done purely for side-effect.
                                        _eval_compile_toplevel((form,), path)
                                return [_convert_and_maybe_compile(form, path)]
                        else:
                                return _process_toplevel_form(expanded, path, compile_time_too)
        if atom(form):
                return default_processor(form)
        else:
                if form[0] in [eval_when, macrolet, symbol_macrolet]:
                        if _py.len(form) < 2:
                                _compiler_error("%s form is too short: %s", form[0], form)
                        special_operator, magic, *body = form
                        if special_operator is eval_when:
                                ## CT, LT, and E here are as in Figure 3-7 of ANSI
                                ## "3.2.3.1 Processing of Top Level Forms".
                                ct, lt, e = _parse_eval_when_situations(magic)
                                new_compile_time_too = ct or (compile_time_too and e)
                                return (_process_toplevel_progn(body, path, new_compile_time_too) if lt                   else
                                        _eval_compile_toplevel(body, path)                        if new_compile_time_too else
                                        [])
                        elif special_operator is macrolet:
                                return funcall_in_macrolet_lexenv(
                                        magic,
                                        lambda funs = [], prepend = []:
                                                (the(null, prepend) or
                                                 _process_toplevel_locally(body, path, compile_time_too)))
                        elif special_operator is symbol_macrolet:
                                return funcall_in_symbol_macrolet_lexenv(
                                        magic,
                                        lambda vars = [], prepend = []:
                                                (the(null, prepend) or
                                                 _process_toplevel_locally(body, path, compile_time_too, vars = vars)))
                elif form[0] is locally:
                        return _process_toplevel_locally(form[1:], path, compile_time_too)
                elif form[0] is progn:
                        return _process_toplevel_progn(form[1:], path, compile_time_too)
                else:
                        return default_processor(form)

def _sub_sub_compile_file():
        pass

def compile_file_pathname(input_file, output_file = None):
        _not_implemented()
        return

def compile_file(input_file, output_file = None,
                 compile_verbose = None,
                 compile_print = None,
                 external_format = None,
                 # Extension:
                 trace_file = None):
        def _compile_file_default_pathname(input_file, output_file = None):
                if not input_file.endswith(".lisp"):
                        error("In COMPILE-FILE %s: must end with .lisp.", _py.repr(input_file))
                return _defaulted(output_file, input_file[:-2] + "fas")
        compile_verbose, compile_print = mapcar(lambda x: _defaulted_to_var(compile_verbose, x), [_compile_verbose_,
                                                                                                  _compile_print_])
        output_file = _compile_file_default_pathname(input_file, output_file = output_file)
        abort_p, warnings_p, failure_p = nil, nil, nil
        with _py.open(input_file, "r") as input:
                form = read(input)
                while form:
                        # However, we need a total module here, not just a single form.
                        # The actual compilation must come as a last pass.
                        ################ Thought process paused here...
                        cfun, errp, warnp = 1
                        failure_p  = failure_p or errp
                        warnings_p = warnings_p or warnp
                        form = read(input)
        with _py.open(output_file, "w") as output:
                _compiler_mumble("; writing %s..\n", output_file)
                _not_implemented()
        return (abort_p,
                warnings_p,
                failure_p)

# LOAD (+ stray stream_type_error)

_string_set("*LOAD-VERBOSE*", nil)
_string_set("*LOAD-PRINT*", nil)

def _load_as_source(stream, verbose = nil, print = nil):
        pathname = _file_name(stream)
        verbose and format(t, "; loading %s\n", _py.repr(pathname))
        def with_abort_restart_body():
                def eval_form(form, index):
                        spref = "; evaluating "
                        print and format(t, spref + "%s\n", _pp_sex(form, initial_depth = length(spref)))
                        def with_continue_restart_body():
                                while t:
                                        def with_retry_restart_body():
                                                results = _eval_tlf(form, index)
                                                results = (results,) if not _tuplep(results) else results
                                                if print:
                                                        format(t, "%s\n", ", ".join(_py.repr(x) for x in results))
                                        with_simple_restart("RETRY", ("Retry EVAL of current toplevel form.",),
                                                            with_retry_restart_body)
                                        return
                        with_simple_restart("CONTINUE", ("Ignore error and continue loading file %s.", _py.repr(pathname)),
                                            with_continue_restart_body)
                ## Unregistered Issue DEBUG-LOAD-FILE-SOURCE-INFO-IGNORED
                def next(): return read(stream, eof_error_p = nil, eof_value = stream)
                form = next()
                if pathname:
                        with progv({ _source_info_: _source_info.python_type(pathname = pathname) }):
                                while form != stream:
                                        eval_form(form, nil)
                                        form = next()
                else:
                        with progv({ _source_info_: nil }):
                                while form != stream:
                                        eval_form(form, nil)
                                        form = next()
        return with_simple_restart("ABORT", ("Abort loading file %s.", _file_name(stream)),
                                   with_abort_restart_body)

_string_set("*FASL-FILE-TYPE*", "vpfas")

@_cold_defun_with_block
def load(pathspec, verbose = None, print = None,
         if_does_not_exist = t,
         external_format = _keyword("default")):
        verbose = _defaulted_to_var(verbose, _load_verbose_)
        print   = _defaulted_to_var(verbose, _load_print_)
        def fasl_header_p(stream, errorp = nil):
                _warn_not_implemented()
                if errorp:
                        error("The file pointed at by stream %s does not contain a FASL file.", stream)
                return nil
        def load_stream(stream, faslp):
                with progv({ _readtable_:     _symbol_value(_readtable_),
                             _package_:       _symbol_value(_package_),
                             _load_pathname_: pathname(stream),
                             _load_truename_: handler_case(lambda: truename(stream),
                                                             (error, lambda _: nil)) }):
                        return_from(load, (_load_as_fasl if faslp else
                                           _load_as_source)(stream, verbose = verbose, print = print))
        ## Case 1: stream.
        if streamp(pathspec):
                return load_stream(pathspec, _stream_faslp(pathspec))
        pathname_ = pathname(pathspec)
        ## Case 2: Open as binary, try to process as a fasl.
        def with_open_stream_body(stream):
                if not stream:
                        return_from(load, nil)
                real = probe_file(stream)
                should_be_fasl_p = real and string_equal(pathname_type(real), _symbol_value(_fasl_file_type_))
                if ((should_be_fasl_p or file_length(stream)) and
                    fasl_header_p(stream, errorp = should_be_fasl_p)):
                        return_from(load, load_stream(stream, t))
        def typeless_pathname_branch():
                nonlocal pathname_
                defaulted_pathname = probe_load_defaults(pathspec)
                if defaulted_pathname:
                        pathname_ = defaulted_pathname
                        return open(pathname_, if_does_not_exist = (_keyword("ERROR") if if_does_not_exist else
                                                                    nil),
                                    element_type = (unsigned_byte, 8))
        with_open_stream((open(pathspec, element_type = (unsigned_byte, 8),
                               if_does_not_exist = nil) or
                          (null(pathname_type(pathspec)) and typeless_pathname_branch()) or
                          (if_does_not_exist and
                           error(simple_file_error, pathname = pathspec,
                                 format_control = "Couldn't load %s: file does not exist.",
                                 format_arguments = [pathspec]))),
                         with_open_stream_body)
        ## Case 3: Open using the gived external format, process as source.
        with_open_file(pathname_,
                       lambda stream: load_stream(stream, nil),
                       external_format = external_format)

@defclass
class stream_type_error(simple_condition.python_type, _io.UnsupportedOperation):
        pass

# LOAD-able things

#     Cold boot complete, now we can LOAD vpcl.lisp.

# _debug_compiler()
load("vpcl.lisp", verbose = t)

# REQUIRE

_string_set("*MODULE-PROVIDER-FUNCTIONS*", [])

def _module_filename(module):
        return "%s/%s.py" % (env.partus_path, string(module))

def require(name, pathnames = None):
        "XXX: not terribly compliant either"
        namestring = string(name)
        filename = pathnames[0] if pathnames else _module_filename(namestring)
        if probe_file(filename):
                _not_implemented()
        else:
                error("Don't know how to REQUIRE %s.", namestring.upper())

# Environment

def get_universal_time():
        # Issue UNIVERSAL-TIME-COARSE-GRANULARITY
        # time.time() returns microseconds..
        return _py.int(_time.time())

def sleep(x):
        return _time.sleep(x)

def user_homedir_pathname():
        return _os.path.expanduser("~")

def lisp_implementation_type():    return "CPython"
def lisp_implementation_version(): return _sys.version

def machine_instance():            return _socket.gethostname()
def machine_type():                return _without_condition_system(lambda: _platform.machine(),
                                                                    reason = "platform.machine")
def machine_version():             return "Unknown"

# DESCRIBE

# def _get_info_value(name, type, env_list = None):
#         def lookup(env_list):
# def _info(class, type, name, env_list = None):
#         info = _type_info_or_lose(class, type)
#         return _get_info_value(name, _type_info_number(info), env_list)
def _describe_function(name, function, stream):
        name = function.__name__ if function else name
        if not (function or (name and fboundp(name))):
                format(stream, "%s names an undefined function.\n", name)
        else:
                if not function and special_operator_p(name):
                        fun, what, lambda_list = symbol_value(name), "a special operator", _not_implemented()
                elif not function and macro_function(name):
                        fun, what, lambda_list = macro_function(name), "a macro", _not_implemented()
                else:
                        fun = function or fdefinition(name)
                        what = "a function"
                               # ("a generic function"      if typep(fun, generic_function) else
                               #  "a compiled function"     if compiled_function_p(fun)     else
                               #  "an interpreted function")
                        lambda_list = (fun.lambda_list if _py.hasattr(fun, "lambda_list") else
                                       None)
                        methods = (# generic_function_methods(fun) if typep(fun, generic_function) else
                                   None)
                        if not function:
                                format(stream, "\n%s names %s:", name, what)
                        if _specifiedp(lambda_list):
                                describe_lambda_list(lambda_list)
                        # describe_documentation(name, intern("FUNCTION")[0], stream)
                        if _specifiedp(methods):
                                format(stream, "\nMethod combination: %s",
                                       generic_function_method_combination(fun))
                                if not methods:
                                        format(stream, "\nNo methods.\n")
                                else:
                                        for m in methods:
                                                format(stream, "\n  %s %s %s",
                                                       name,
                                                       method_qualifiers(m),
                                                       method_specializers(m))
                                                describe_documentation(m, t, stream, nil)
                        # describe_function_source(fun, stream)
                        terpri(stream)
        if not function:
                # when (and (legal-fun-name-p name) (compiler-macro-function name))
                ## ...
                # when (and (consp name) (eq 'setf (car name)) (not (cddr name)))
                ## ... setf expansions
                pass
        if symbolp(name):
                ## (describe-function `(setf ,name) nil stream)
                pass

def _describe_class(name, class_, stream):
        pass

def _describe_python_object(x, stream):
        type = _py.type(x)
        slots = [ x for x in _py.dir(x) if "__" not in x ]
        maxslotnamelen = _py.max(_py.len(x) for x in slots)
        def describe_slot(x, slot):
                value = _py.getattr(x, slot)
                format(stream, "\n  %%%ds: %%s" % maxslotnamelen, slot, _py.repr(value))
        format(stream, "Python type: %s", type)
        for slot in slots:
                describe_slot(x, slot)
        terpri(stream)

def describe_object(o, stream):
        def object_self_string(o):
                if symbolp(o):
                        return _print_symbol(o)
                else:
                        return "#<python object %s>" % (o.__repr__(),)
        def object_type_string(o):
                return _py.type(o).__name__
        def print_standard_describe_header(o, stream):
                format(stream, "%s\n  [%s]\n",
                       object_self_string(o), object_type_string(o))
        def describe_symbol(o, stream):
                ### variable of some kind -- see the _variable_kind() stub
                # var_kind = info(_keyword("variable"), _keyword("kind"), o)
                # var_kind = _variable_kind(o)
                _describe_function(o, nil, stream)
                _describe_class(o, nil, stream)
                ### type specifier
                # type_kind = info(_keyword("type"), _keyword("kind"), o)
                # type_kind = ???
                ## defined   - expander
                ## primitive - translator
                ### optimisation policy
                ### properties
                if not (fboundp(o) or _symbol_type_specifier_p(o)):
                        _describe_python_object(o, stream)
        print_standard_describe_header(o, stream)
        if symbolp(o): describe_symbol(o, stream)
        else:
                _describe_python_object(o, stream)

def describe(object, stream_designator = None):
        "Print a description of OBJECT to STREAM-DESIGNATOR."
        stream_designator = _defaulted(stream_designator, _symbol_value(_standard_output_))
        with progv({_print_right_margin_: 72}):
                describe_object(object, stream_designator)

# Functions: FUNCTION, CONSTANTP

@defun
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
        return (typep(form, (or_, integer, float, complex, string)) or
                keywordp(form) or form in [t, nil, pi]                 or
                (_tuplep(form) and _py.len(form) == 2 and form[0] is quote_))

# STANDARD-OBJECT and basic protocols

# Unregistered Issue C-J-COULD-BE-EXTENDED-TO-FOLLOW-M-J-WITHIN-COMMENTS
def class_of(x):
        return _py.getattr(x, "__class__")

@defclass
class standard_object():
        def __init__(self, **initargs):
                super().__init__() # Unregistered Issue PYTHON-OBJECT-DOES-NOT-ACCEPT-ARGUMENTS-BUT-SEE-SUPER-CONSIDERED-HARMFUL
                initialize_instance(self, **initargs)

def slot_boundp(object, slot):            return _py.hasattr(object, slot)
def slot_makunbound(object, slot):        del object.__dir__[slot]
def slot_value(object, slot):             return _py.getattr(object, slot)
def setf_slot_value(value, object, slot): return _py.setattr(object, slot, value)

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

# Generic functions

@defclass
class method(standard_object.python_type):
        "All methods are of this type."

@defclass
class funcallable_standard_class(standard_object.python_type):
        "All funcallable instances are of this type."
        def __call__(self, *args, **keys):
                return self.function(*args, **keys)

@defclass
class generic_function(funcallable_standard_class.python_type):
        "All generic functions are of this type."
        def __init__(self, **initargs): # Simulate a :BEFORE method.
                self.__dependents__ = _py.set()
                _here("args: %s", initargs)
                super().__init__(**initargs)

# Dependent maintenance protocol

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
        mapc(function, metaobject.__dependents__)

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

# Generic function methods

@defclass
class method_combination():
        "All method combinations are of this type."

@defclass
class standard_method(method.python_type):
        def __init__(self, **initargs):
                super().__init__(**initargs)
                _standard_method_shared_initialize(self, **initargs)
        def __call__(self, gfun_args, next_methods):
                return self.function(gfun_args, next_methods)

@defclass
class standard_generic_function(generic_function.python_type):
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

Initialization Argument         Generic Function
--------------------------------------------------------------------------
:argument-precedence-order      generic-function-argument-precedence-order
:declarations                   generic-function-declarations
:documentation                  documentation
:lambda-list                    generic-function-lambda-list
:method-combination             generic-function-method-combination
:method-class                   generic-function-method-class
:name                           generic-function-name

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
                elif not (_listp(argument_precedence_order) and
                          _py.set(argument_precedence_order) == _py.set(lambda_list[0])):
                        error("MAKE-INSTANCE STANDARD-GENERIC-FUNCTION: :ARGUMENT-PRECEDENCE-ORDER, "
                              "when specified, must be a permutation of fixed arguments in :LAMBDA-LIST.  "
                              "Was: %s;  fixed LAMBDA-LIST args: %s.",
                              _py.repr(argument_precedence_order), lambda_list[0])
                generic_function.argument_precedence_order = _py.tuple(argument_precedence_order)
        elif _specifiedp(lambda_list):
                generic_function.argument_precedence_order = _py.tuple(lambda_list[0])
        generic_function.declarations        = _py.tuple(_defaulted(declarations, _py.list(),
                                                                 type = (pylist,
                                                                         (satisfies, _valid_declaration_p))))
        generic_function.documentation       = _defaulted(documentation, nil,
                                              type = (or_, string, (eql, nil)))
        if _specifiedp(lambda_list):
                # XXX: _not_implemented("lambda-list validation")
                generic_function.lambda_list = lambda_list
        generic_function.method_combination  = _defaulted(method_combination, standard_method_combination.python_type,
                                                          type = _cold_class_type)
        generic_function.method_class        = _defaulted(method_class, standard_method.python_type,
                                                          type = _cold_class_type) # method metaclass
        generic_function.name                = _defaulted(name, nil)
        # The discriminating function may reuse the
        # list of applicable methods without calling
        # COMPUTE-APPLICABLE-METHODS-USING-CLASSES again provided that:
        # (ii) the generic function has not been reinitialized,
        generic_function.__applicable_method_cache__ = make_hash_table() # (list, _type) -> _list
        generic_function.__methods__ = make_hash_table()
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
        # generic_function.__code__.co_filename    = filename
        # generic_function.__code__.co_firstlineno = lineno
        return generic_function

def generic_function_argument_precedence_order(x): return x.argument_precedence_order
def generic_function_declarations(x):              return x.declarations
def generic_function_lambda_list(x):               return x.lambda_list
def generic_function_method_combination(x):        return x.method_combination
def generic_function_method_class(x):              return x.method_class
def generic_function_name(x):                      return x.name

def generic_function_p(x): return functionp(x) and _py.hasattr(x, "__methods__")  # XXX: CL+
def method_p(x):           return functionp(x) and _py.hasattr(x, "specializers") # XXX: CL+
def _specializerp(x):       return ((x is t)        or
                                    typep(x, (or_, _py.type, (pytuple, (eql, eql), t))))

def _get_generic_fun_info(generic_function):
        return (_py.len(generic_function.lambda_list[0]), # nreq
                nil,
                [],
                _py.len(generic_function.lambda_list[3]),
                generic_function.lambda_list)

def generic_function_methods(x):                   return x.__methods__.values()

# DEFINE-METHOD-COMBINATION

__method_combinations__ = make_hash_table()

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
    place in a SETF form are undefined. Argument correspondence is
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
                  (pylist,
                   (varituple,
                    symbol,       # group name
                    (or_, (pylist, (or_, pytuple, (eql, star))), # We're off the spec a little here,
                                                                 # but it's a minor syntactic issue.
                          function),
                    # the rest is actually a plist, but we cannot (yet) describe it
                    # in terms of a type.
                    (maybe, (pytuple,
                             (eql, _keyword("description")),
                             string)),
                    (maybe, (pytuple,
                              (eql, _keyword("order")),
                              (member,
                               _keyword("most-specific-first"),
                               _keyword("most-specific-last")))),
                    (maybe, (pytuple,
                             (eql, _keyword("required")),
                             (member, t, nil))))))
        # check_type(arguments, (maybe, lambda_list_))
        check_type(arguments, (maybe, (pytuple,
                                       (pylist, string),
                                       (pylist, string),
                                       (maybe, string),
                                       (pylist, string),
                                       (maybe, string))))
        check_type(generic_function, (maybe, symbol))
        ### VARI-BIND, anyone?
        # def vari_bind(x, body):
        #         # don't lambda lists actually rule this, hands down?
        #         posnal, named = argspec_value_varivals(_inspect.getfullargspec(body), x)
        #         return body()
        method_group = _poor_man_defstruct("method_group",
                                           "name",
                                           "qualifier_spec",
                                           "description",
                                           "most_specific_first",
                                           "required")
        groups = make_hash_table()
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
                               # (or_, (pylist, (or_, star, _py.list)),
                               #       function),
                               if ((_listp(qualifier_spec) and
                                    some(_curry(method_qualifiers_match_pattern_p, qualifiers),
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
               body_args = _py.dict(grouped_methods)
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

# Standard method combination

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
standard_method_combination = method_combination # Crude XXX
# standard_method_combination = define_method_combination(
#         _i("STANDARD"),
#         [(_i("around"),  [(_keyword("around"),)]),
#          (_i("before"),  [(_keyword("before"),)]),
#          (_i("primary"), [tuple()],
#                          (_keyword("required"), t)),
#          (_i("after"),   [(_keyword("after"),)],
#                          (_keyword("order"),    _keyword("most-specific-last")))],
#         lambda: # "around", "before", "primary" and "after" are bound "magically",
#                 # to avoid duplication.
#                 [])

# MAKE-METHOD-LAMBDA

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

# COMPUTE-EFFECTIVE-METHOD

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
        for i in _py.range(nreq):
                if not arguments:
                        error_need_at_least_n_args(generic_function_name(generic_function),
                                                   nreq)
                        arg = arguments.pop()
                        types_rev.append([type_modifier, arg] if type_modifier else
                                         arg)
        return (types_rev, arg_info)

def _arg_info_precedence(arg_info: "lambda list, actually.."):
        return _py.range(_py.len(arg_info[0]))

def _compute_applicable_methods_using_types(generic_function, types_):
        definite_p, possibly_applicable_methods = t, []
        # Not safe against method list modifications by another thread!
        for method in generic_function_methods(generic_function):
                specls = method_specializers(method) # Was: if (consp method)
                types = _py.list(types_)
                possibly_applicable_p, applicable_p = t, t
                for specl in specls:
                        (_,
                         specl_applicable_p,
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
                return _values_frame(_sort_applicable_methods(precedence,
                                                              _py.reversed(possibly_applicable_methods),
                                                              types),
                                     definite_p)

def _type_from_specializer(specl):
        if specl is t:
                return t
        elif _tuplep(specl):
                if not member(car(specl), [class_, _class_eq, eql]): # protoype_
                        error("%s is not a legal specializer type.", specl)
                return specl
        elif specializerp(specl): # Was a little bit more involved.
                return specializer_type(specl)
        else:
                error("%s is neither a type nor a specializer.", specl)

def specializer_applicable_using_type_p(specl, type):
        specl = _type_from_specializer(specl)
        if specl is t:
                return _values_frame(t, t)
        ## This is used by C-A-M-U-T and GENERATE-DISCRIMINATION-NET-INTERNAL,
        ## and has only what they need.
        return ((nil, t) if atom(type) or car(type) is t else
                _poor_man_case(car(type),
                               # (and    (saut-and specl type)),
                               # (not    (saut-not specl type)),
                               # (class_,     saut_class(specl, type)),
                               # (prototype  (saut-prototype specl type)),
                               (_class_eq,   lambda: _saut_class_eq(specl, type)),
                               # (class-eq   (saut-class-eq specl type)),
                               # (eql    (saut-eql specl type)),
                               (t,       lambda: error("%s cannot handle the second argument %s.",
                                                       "specializer-applicable-using-type-p",
                                                       type))))

def _saut_class_eq(specl, type):
       if car(specl) is eql:
               return (nil, type_of(specl[1]) is type[1])
       else:
               pred = _poor_man_case(car(specl),
                                     (class_eq, lambda: specl[1] is type[1]),
                                     (class_,   lambda: (specl[1] is type[1] or
                                                         memq(specl[1], cpl_or_nil(type[1])))))
               return (pred, pred)

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
                        specl1 = nth(index, method_specializers(method1)) # XXX: Was (if (listp method1) ..)
                        specl2 = nth(index, method_specializers(method2)) # XXX: Was (if (listp method2) ..)
                        order  = _order_specializers(specl1, specl2, index, compare_classes_function)
                        if order:
                                return order is specl1
        return stable_sort(methods, sorter)

def _order_specializers(specl1, specl2, index, compare_classes_function):
        type1 = specializer_type(specl1) # Was: (if (eq **boot-state** 'complete) ..)
        type2 = specializer_type(specl2) # Was: (if (eq **boot-state** 'complete) ..)
        return ([]     if specl1 is specl1 else
                specl2 if atom(type1)      else # is t?
                specl1 if atom(type2)      else # is t?
                _poor_man_case(car(type1),
                               (_py.type, lambda: case(car(type2),
                                                       (_py.type, compare_classes_function(specl1, specl2, index)),
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
                     (eql,  lambda: _poor_man_case(car(type2),
                                                   # similarly
                                                   (eql, []),
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
        return _values_frame_project(0, _compute_applicable_methods_using_types(generic_function,
                                                                                _types_from_args(generic_function,
                                                                                                 arguments,
                                                                                                 eql)))

def error_need_at_least_n_args(function, n):
        error("The function %s requires at least %d arguments.", function, n)

__sealed_classes__ = _py.set([object,
                              integer, boolean, float, complex,
                              string,
                              hash_table,
                              function,
                              stream,
                              pytuple, pybytes, pylist, pybytearray, pyset, pyfrozenset,
                              BaseException, Exception] +
                             mapcar(type_of,
                                    [None,           # NoneType
                                     Ellipsis,       # ellipsis
                                     NotImplemented, # NotImplementedType
                                     integer,        # type
                                     "".find,        # builtin_function_or_method
                                     _ast,           # module
                                     _sys.stdin,     # __io.TextIOWrapper
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
         # filename,
         # lineno
         ) = (generic_function.name,
                    generic_function.lambda_list,
                    generic_function.__applicable_method_cache__,
                    # generic_function.__code__.co_filename,
                    # generic_function.__code__.co_lineno
              )
        if not function_name:
                error("In COMPUTE-DISCRIMINATING-FUNCTION: name is %s.", function_name)
        fixed, optional, args, keyword, keys = lambda_list
        nfixed = _py.len(fixed)
        def dfun_compute_applicable_methods(generic_function, args):
                if _py.len(args) < nfixed:
                        error_need_at_least_n_args(function_name, nfixed)
                dispatch_args      = args[:nfixed]
                dispatch_arg_types = _py.tuple(_py.type(x) for x in dispatch_args)
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
                unsealed_classes = _py.set(x for x in dispatch_arg_types if not _class_sealed_p(x))
                applicable_method_cache_key = dispatch_arg_types + reduce(lambda acc, x: acc + x.__mro__,
                                                                          _sorted(unsealed_classes, key = lambda type: type.__name__),
                                                                          _py.tuple())
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
                _, methods, okayp = compute_applicable_methods_using_classes(generic_function,
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
            string(function_name),
            lambda_list,
            # How do we access methods themselves?
            [_ast_return(
                 _ast_funcall(_ast_funcall("compute_effective_method",
                                           [_ast_name(string(function_name)),
                                            None, # method combination
                                            _ast_funcall("dfun_compute_applicable_methods",
                                                         [_ast_name(string(function_name)),
                                                          mapcar(_ast_name, fixed)])]),
                              mapcar(_ast_name, fixed + mapcar(car, optional)),
                              _map_into_hash_star(lambda key, default: (key, _ast_name(default)),
                                                   keyword),
                              starargs = _ast_name(args) if args else None,
                              kwargs   = _ast_name(keys) if keys else None))])
        if t:
                import more_ast # Shall we concede, and import it all?
                format(t, "; generic function '%s':\n%s",
                       function_name, more_ast.pp_ast_as_code(new_dfun_ast))
        env = _py.dict(compute_effective_method    = compute_effective_method,
                       _find_symbol_or_fail            = _find_symbol_or_fail,
                       dfun_compute_applicable_methods = dfun_compute_applicable_methods)
        return _ast_compiled_name(
                    string(function_name),
                    new_dfun_ast,
                    filename = "" # _defaulted(filename, "")
                    ,
                    lineno   = 0 # lineno
                    ,
                    globals  = env,
                    locals   = env)

# ENSURE-GENERIC-FUNCTION

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
                                        globals = None,
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
        if generic_function:
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
                mapc(_curry(remove_method, generic_function),
                     generic_function_methods(generic_function))
        if lambda_list:
                fixed, optional, args, keyword, kwarg = lambda_list
                if some(lambda x: x[1] is not None, _py.list(optional) + _py.list(keyword)):
                        error("Generic function arglist cannot specify default parameter values.")
        initargs = _only_specified_keys(
                argument_precedence_order = argument_precedence_order,
                declarations              = declarations,
                documentation             = documentation,
                lambda_list               = lambda_list,
                method_class              = method_class,
                method_combination        = method_combination,
                name                      = function_name, # name # Issue RESEARCH-COMPLIANCE-ENSURE-GENERIC-FUNCTION-USING-CLASS-NAME-ARGUMENT
                # incompatible..
                filename                  = filename,
                lineno                    = lineno)
        initargs.update(keys)
        _here("args: %s", initargs)
        ###
        ### Second step:
        if not generic_function:
                # If the GENERIC-FUNCTION argument is NIL, an instance of the class
                # specified by the :GENERIC-FUNCTION-CLASS argument is created by
                # calling MAKE-INSTANCE with the previously computed initialization arguments.
                # The function name FUNCTION-NAME is set to name the generic function.
                generic_function = make_instance(generic_function_class, **initargs)
                # _standard_generic_function_shared_initialize is called by s-g-f.__init__
                _frost.setf_global(generic_function, function_name, globals = _defaulted(globals, _py.globals()))
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

def ensure_generic_function(function_name, globals = None, **keys):
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
        maybe_gfun, therep = _defaulted(_frost.global_(the(symbol, function_name),
                                                       _defaulted(globals, _py.globals())), nil)
        if functionp(maybe_gfun) and not generic_function_p(maybe_gfun):
                error("%s already names an ordinary function.", function_name)
        return ensure_generic_function_using_class(maybe_gfun, function_name, **keys)

def defgeneric(_ = None,
               argument_precedence_order = None,
               documentation = None,
               method_combination = standard_method_combination.python_type,
               generic_function_class = standard_generic_function.python_type,
               method_class = standard_method.python_type):
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
                _, sym, __ = _interpret_toplevel_value(fn, functionp)
                return ensure_generic_function(sym,
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
        return (_py.len(lambda_list[0]) == _py.len(specializers)       and
                every(lambda ms, s: ((ms is s) or
                                     (_listp(ms) and _listp(s) and
                                      _py.len(ms) == _py.len(s) == 2 and
                                      ms[0] == s[0] == eql     and
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
        return ((_py.len(gf_fixed)    != _py.len(m_fixed) and
                 "the method has %s required arguments than the generic function" %
                 ("more" if _py.len(m_fixed) > _py.len(gf_fixed) else "less"))                                or
                (_py.len(gf_optional) != _py.len(m_optional) and
                 "the method has %s optional arguments than the generic function" %
                 ("more" if _py.len(m_fixed) > _py.len(gf_fixed) else "less"))                                or
                (_xorf(gf_args, m_args) and
                 "but the method and generic function differ in whether they accept &REST or &KEY arguments") or
                # XXX: #3 compliance -- still looks fishy
                (_xorf(gf_keyword or gf_keys,
                       m_keyword  or m_keys) and
                 "but the method and generic function differ in whether they accept &REST or &KEY arguments") or
                (((not gf_keyword) or
                  m_keys           or
                  not (_py.set(gf_keyword) - _py.set(m_keyword))) and
                 "but the method does not accept each of the &KEY arguments %s" % _py.tuple([gf_keyword])))

# Method addition and removal

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

# METHOD metamethods

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
                                       type = (pylist, (and_, symbol, (not_, (eql, nil)))))
        if not _specifiedp(lambda_list):
                error("SHARED-INITIALIZE STANDARD-METHOD: :LAMBDA-LIST must be supplied.")
        # Unregistered Issue COMPLIANCE-STANDARD-METHOD-SHARED-INITIALIZE-LAMBDA-LIST-VALIDATION-NOT-IMPLEMENTED
        method.lambda_list = lambda_list
        if not _specifiedp(specializers):
                error("SHARED-INITIALIZE STANDARD-METHOD: :SPECIALIZERS must be supplied.")
        # Unregistered Issue COMPLIANCE-STANDARD-METHOD-SHARED-INITIALIZE-SPECIALIZER-VALIDATION-NOT-IMPLEMENTED
        #  o  (pylist, method_specializer)
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
                                          type = (or_, string, (eql, nil)))
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

# DEFMETHOD

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
        generic_function, definedp = _frost.global_(fn.__name__, _py.globals())
        fixed, optional, args, keyword, keys = lambda_list = _function_lambda_list(fn)
        if not definedp:
                _, sym, __ = _interpret_toplevel_value(fn, functionp)
                generic_function = ensure_generic_function(sym,
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
                specializers = _py.tuple(_make_method_specializers(
                                         mapcar(lambda name: gethash(name, method.__annotations__, t)[0],
                                                fixed))),
                function = _not_implemented("somehow compile", methfun_lambda)
                **methfun_args)
        add_method(generic_function, method)
        return method

def _make_method_specializers(specializers):
        def parse(name):
                return (# name                                                    if specializerp(name) else
                        name                                                      if name is t                   else
                                                                  # ..special-case, since T isn't a type..
                        name                                                      if typep(name, _py.type) else
                                                                  # Was: ((symbolp name) `(find-class ',name))
                        _poor_man_ecase(car(name),
                                        (eql,       lambda: intern_eql_specializer(name[1])),
                                        (class_eq_, lambda: class_eq_specializer(name[1]))) if _tuplep(name)      else
                        ## Was: FIXME: Document CLASS-EQ specializers.
                        error("%s is not a valid parameter specializer name.", name))
        return mapcar(parse, specializers)

# Init

_init_package_system_2()
def _init():
        "Initialise the Common Lisp compatibility layer."
        _init_condition_system()
        _string_set("*DEFAULT-PATHNAME-DEFAULTS*", _os.path.getcwd())
        return t

# Evaluation of Python as Lisp (for Partus)

def _make_eval_context():
        val = None
        def set(x):
                nonlocal val
                val = x
        def get():
                return val
        return get, set
__evget__, __evset__ = _make_eval_context()

__eval_source_cache__ = make_hash_table() # :: code_object -> string

def _evaluated_code_source(co):
        return gethash(co, __eval_source_cache__)

def _coerce_to_expr(x):
        return (x.value if typep(x, _ast.Expr) else
                x)

def _eval_python(expr_or_stmt):
        "In AST form, naturally."
        package = _symbol_value(_package_)
        exprp = typep(the(_ast.AST, expr_or_stmt), (or_, _ast.expr, _ast.Expr))
        call = _ast.fix_missing_locations(_ast_module(
                        [_ast_import_from("cl", ["__evset__", "_read_symbol"]),
                         _ast_Expr(_ast_funcall(_ast_name("__evset__"), [_coerce_to_expr(expr_or_stmt)]))]
                        if exprp else
                        [expr_or_stmt]))
        code = handler_case(lambda: _py.compile(call, "", "exec"),
                            (error,
                             lambda cond:
                                     error("EVAL: error while trying to compile <%s>: %s",
                                           more_ast.pp_ast_as_code(expr_or_stmt), cond)))
        if boundp(_source_for_eval_):
                __eval_source_cache__[code] = _symbol_value(_source_for_eval_)
        # write_line(">>> EVAL: %s" % (more_ast.pp_ast_as_code(expr),))
        _py.exec(code, _find_module(_frost.lisp_symbol_name_python_name(package_name(package))).__dict__)
        values = (__evget__() if exprp else
                  _py.tuple())
        return values if _tuplep(values) else (values,)

def _callify(form, package = None, quoted = nil):
        package = _defaulted_to_var(package, _package_)
        def callify_call(sym, args):
                func = function(the(symbol, sym))
                paramspec = _inspect.getfullargspec(func)
                nfix = _argspec_nfixargs(paramspec)
                _here("func: %s -> %s, paramspec: %s", sym, func, paramspec)
                _here("nfix: %s", nfix)
                _here("args: %s", args)
                _here("nkeys: %s", _py.len(args) - nfix)
                if oddp(_py.len(args) - nfix):
                        error("odd number of &KEY arguments")
                allow_other_keys = paramspec.varkw is not None
                fixnames, keynames = (paramspec.args[0:nfix],
                                      _py.set(paramspec.args[nfix:] + paramspec.kwonlyargs))
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
                string  : _ast_string,
                integer : _ast_num,
                }
        if _listp(form):
                if quoted or (form[0] is _find_symbol("QUOTE", __cl)[0]):
                        return (_ast_list(mapcar(lambda x: _callify(x, package, t), form[1]))
                                if _listp(form[1]) else
                                _callify(form[1], package, t))
                else:
                        return callify_call(form[0], form[1:])
        elif symbolp(form):
                return (_ast_funcall("_read_symbol", [_ast_string(form.name),
                                                      _ast_string(form.package.name)])
                        if quoted or keywordp(form) else
                        _lisp_symbol_ast(form, package))
        elif constantp(form):
                return obj2ast_xform[_py.type(form)](form)
        elif form in obj2ast_xform:
                return obj2ast_xform[form]
        else:
                error("Unable to convert form %s", form)

def _valid_declaration_p(x):
        return nil

# Missing stuff

# class _deadline_timeout(condition)
# def _with_deadline(timeout, body)

def read_sequence(sequence, stream, start = 0, end = None):
        _not_implemented()

def get(symbol, indicator, default = None):
        """get symbol indicator &optional default => value

(setf (get symbol indicator &optional default) new-value)

Arguments and Values:

SYMBOL---a symbol.

INDICATOR---an object.

DEFAULT---an object. The default is NIL.

VALUE---if the indicated property exists, the object that is its
        value; otherwise, the specified default.

NEW-VALUE---an object.

Description:

GET finds a property on the property list[2] of SYMBOL whose property
indicator is identical to INDICATOR, and returns its corresponding
property value.  If there are multiple properties[1] with that
property indicator, GET uses the first such property.  If there is no
property with that property indicator, DEFAULT is returned.

SETF of GET may be used to associate a new object with an existing
indicator already on the SYMBOL's property list, or to create a new
assocation if none exists.  If there are multiple properties[1] with
that property indicator, SETF of GET associates the NEW-VALUE with the
first such property.  When a GET form is used as a SETF place, any
default which is supplied is evaluated according to normal
left-to-right evaluation rules, but its value is ignored."""
        _not_implemented()

def setf_get(new_value, symbol, indicator, default = None):
        _not_implemented()

def symbol_plist(symbol):
        _not_implemented()

def setf_symbol_plist(new_value, symbol):
        _not_implemented()

###
### ...
###
# Specification Issue INTERN-RELATIONSHIP-UNDERUSED
## response: sufficiently smart compilers eliminate the efficiency concern,
##           so what is left?
def _intern0(x, package = None): return _intern(the(_py.str, x), package)[0]

# Thoughts on thunking

#
##
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
##
#

# *PRINT/READ-<foo>* docstrings

"""Controls the format in which arrays are printed. If it is false,
the contents of arrays other than strings are never printed. Instead,
arrays are printed in a concise form using #< that gives enough
information for the user to be able to identify the array, but does
not include the entire array contents. If it is true, non-string
arrays are printed using #(...), #*, or #nA syntax."""

"""*PRINT-BASE* and *PRINT-RADIX* control the printing of
rationals. The value of *PRINT-BASE* is called the current output
base.

The value of *PRINT-BASE* is the radix in which the printer will print
rationals. For radices above 10, letters of the alphabet are used to
represent digits above 9."""

"""The value of *PRINT-CASE* controls the case (upper, lower, or
mixed) in which to print any uppercase characters in the names of
symbols when vertical-bar syntax is not used.

*PRINT-CASE* has an effect at all times when the value of
*PRINT-ESCAPE* is false. *PRINT-CASE* also has an effect when the
value of *PRINT-ESCAPE* is true unless inside an escape context
(i.e., unless between vertical-bars or after a slash)."""

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

"""If false, escape characters and package prefixes are not output
when an expression is printed.

If true, an attempt is made to print an expression in such a way that
it can be READ again to produce an equal expression. (This is only a
guideline; not a requirement. See *PRINT-READABLY*.)

For more specific details of how the value of *PRINT-ESCAPE* affects
the printing of certain types, see Section 22.1.3 (Default
Print-Object Methods)."""

"""Controls whether the prefix ``#:'' is printed before apparently
uninterned symbols. The prefix is printed before such symbols if and
only if the value of *PRINT-GENSYM* is true."""

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

"""When the value of *PRINT-LINES* is other than NIL, it is a limit on
the number of output lines produced when something is pretty
printed. If an attempt is made to go beyond that many lines, ``..'' is
printed at the end of the last line followed by all of the suffixes
(closing delimiters) that are pending to be printed."""

"""If it is not NIL, the pretty printer switches to a compact style of
output (called miser style) whenever the width available for printing
a substructure is less than or equal to this many ems."""

"""The PPRINT dispatch table which currently controls the pretty printer.

Initial value is implementation-dependent, but the initial entries all
use a special class of priorities that have the property that they are
less than every priority that can be specified using
SET-PPRINT-DISPATCH, so that the initial contents of any entry can be
overridden."""

"""Controls whether the Lisp printer calls the pretty printer.

If it is false, the pretty printer is not used and a minimum of
whitespace[1] is output when printing an expression.

If it is true, the pretty printer is used, and the Lisp printer will
endeavor to insert extra whitespace[1] where appropriate to make
expressions more readable.

*PRINT-PRETTY* has an effect even when the value of *PRINT-ESCAPE* is
false."""

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

"""If it is non-NIL, it specifies the right margin (as integer number
of ems) to use when the pretty printer is making layout decisions.

If it is NIL, the right margin is taken to be the maximum line length
such that output can be displayed without wraparound or truncation. If
this cannot be determined, an implementation-dependent value is
used."""
