
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
import pdb         as _pdb
import sys         as _sys
import math        as _math
import time        as _time
import trace       as __trace
import types       as _types
import socket      as _socket
import hashlib     as _hashlib
import inspect     as _inspect
import marshal     as _marshal
import operator    as _operator
import platform    as _platform
import functools   as _functools
import itertools   as _itertools
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
        return dict(((k, v) for k, v in keys.items()
                     if _specifiedp(k)))

def _defaulted_keys(**keys):
        return dict((key, (default if value is None else value))
                    for key, (value, default) in keys.items())

# Boot messaging

def _fprintf(stream, format_control, *format_args):
        try:
                _neutrality._write_string(format_control % format_args, stream)
        except UnicodeEncodeError:
                _neutrality._write_string((format_control % format_args).encode("utf-8"), stream)

def _debug_printf(format_control, *format_args):
        _fprintf(_sys.stderr, format_control + "\n", *format_args)

# First-class namespaces

class _namespace(_collections.UserDict):
        def __str__(self):
                return "#<NAMESPACE %s>" % (repr(self.name),)
        def __init__(self, name, data_constructor = dict):
                self.name, self.data, self.properties = name, data_constructor(), _collections.defaultdict(dict)
        def __getitem__(self, x):               return self.data.__getitem__(x)
        def __hasitem__(self, x):               return self.data.__hasitem__(x)
        def names(self):                        return set(self.data.keys())
        def intersect(self, with_):             return [x for x in with_ if x in self.data] if len(self) > len(with_) else [x for x in self.data if x in with_]
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
__boot_defunned__, __boot_defclassed__ = set(),  set()
def boot_defun(fn):     __boot_defunned__.add(fn);    return fn
def boot_defclass(cls): __boot_defclassed__.add(cls); return cls

## 2. tagged switchables
_namespace.grow("boot", data_constructor = lambda: _collections.defaultdict(set))

def boot(set, boot, on_unboot = None):
        def definer(orig):
                def unboot():
                        _frost.setf_global(orig, orig.__name__, globals())
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
        for x in sorted(_namespace["boot"][set], key = lambda x: x.name):
                if not hasattr(x, "unboot"):
                        error("In UNBOOT-SET \"%s\": %s has no 'unboot' attribute.", set, x)
                x.unboot()
        del _namespace["boot"][set]
        _debug_printf("; unbooted function set %s, remaining boot sets: %s", repr(set), ", ".join(_namespace["boot"].keys()))

def _interpret_toplevel_value(name_or_obj, objness_predicate):
        name, obj = ((name_or_obj.__name__, name_or_obj) if objness_predicate(name_or_obj)           else
                     (name_or_obj, None)                 if isinstance(name_or_obj, (str, symbol_t)) else
                     error("Bad cold object definition: %s", name_or_obj))
        ####### Thought paused here:
        # ..delay symbol computation!
        sym, inmod_name = ((_intern(_frost.python_name_lisp_symbol_name(name))[0], name)  if isinstance(name, str)      else
                           (name, _frost.lisp_symbol_name_python_name(symbol_name(name))) if isinstance(name, symbol_t) else
                           error("In cold definition of %s: bad name %s for a cold object.", name, repr(name)))
        return obj, sym, inmod_name

class _storyteller(_collections.UserDict):
        def __init__(self):           self.__dict__.update(dict(__call__ = lambda self, x: self.advance(x),
                                                                    data     = dict()))
        def __setattr__(self, _, __): raise Exception("\n; The Storyteller defies this intercession.")
        def advance(self, x):
                self.data[x if isinstance(x, str) else
                          x.__name__] = True
                return x
        def narrated(self, x):        return x in self.data
        def call(self, x, control, *args, hard = False):
                if x in self.data:
                        return True
                if hard:
                        raise Exception(("\n; The Storyteller quietly said: 'twas too early to mention \"%s\" " % x) + control % args)
                else:
                        warn(("too early to mention \"%s\" " % (x,)) + control, *args)
        def conclude(self):
                _debug_printf("; The Storyteller proclaimed a conclusion, which also was a new beginning.")
                self.__dict__.update(dict(__call__ = lambda *_, **__: True))
_storyteller = _storyteller()
story = _storyteller.advance

# Cold types

_cold_class_type       = type
_cold_condition_type   = BaseException
_cold_error_type       = Exception
_cold_hash_table_type  = dict
_cold_stream_type      = __io._IOBase
_cold_function_type    = _types.FunctionType.__mro__[0]
_cold_tuple_type       = tuple
_cold_string_type      = str
_cold_list_type        = list
def _cold_simple_error(format, *args): raise _cold_error_type(format % args)
def _cold_typep(x, type):
        return isinstance(x, (type             if isinstance(x, type) else
                                  type.python_type if isinstance(x, symbol_t) else
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
def stringp(x):        return isinstance(x, _cold_string_type)
@boot("symbol", lambda _, o: (isinstance(o, _cold_function_type) or
                              isinstance(o, symbol_t) and o.function))
@boot_defun ## Unregistered Issue COMPLIANCE-EVALUATION-MODEL-FUNCTIONP
def functionp(o):      return isinstance(o, _cold_function_type)

def _symbol_type_specifier_p(x):
        return hasattr(x, "python_type")

def _python_type_p(x): return isinstance(o, _cold_class_type)

@boot_defun
def type_of(x):
        return type(x)

# Unspecific Wave 1

@boot_defun
def identity(x):  return x

@boot_defun
def make_hash_table(default_constructor = None):
        return (_collections.defaultdict(default_constructor) if default_constructor else
                dict())

@boot_defun
def gethash(key, dict, default = None):
        therep = key in dict
        return (dict[key] if therep else default), therep

def _map_into_hash(f, xs,
                   key_test = lambda k: k is not None,
                   value_test = lambda _: True) -> dict:
        acc = dict()
        for x in xs:
                k, v = f(x)
                if key_test(k) and value_test(v):
                        acc[k] = v
        return acc

# Boot dynamic scope

__global_scope__ = make_hash_table() ## To be replaced later, by VARDB.

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

def _list_dynamic_frames():
        return __tls__.dynamic_scope

def _dynamic_frame_for_set(name, force_toplevel = None):
        return (__global_scope__ if force_toplevel else
                (_find_dynamic_frame(name) or
                 (__tls__.dynamic_scope[-1] if __tls__.dynamic_scope else
                  __global_scope__)))

def _symbol_value(name):
        frame = _find_dynamic_frame(name)
        return (frame[name] if frame else
                error(AttributeError, "Unbound variable: %s." % name))

def _do_pyimport_symbol(symbol, globals):
        inmod_name = _frost.lisp_symbol_name_python_name(symbol_name(symbol))
        _frost.setf_global(symbol, inmod_name, globals)

def _pyimport_symbol(symbol, globals = None):
        _do_pyimport_symbol(_boot_check_type(symbolp, symbol), globals = _defaulted(globals, _py.globals()))

def _intern_and_bind_names_in_module(*names, globals = None):
        globals = _defaulted(globals, _py.globals())
        for name in names:
                _pyimport_symbol(_intern(name)[0], globals)

def _intern_and_bind_names_in_module_specifically(*name_specs, globals = None):
        globals = _defaulted(globals, _py.globals())
        for pyname, name in name_specs:
                _frost.setf_global(_intern(name)[0], pyname, globals)

def _boot_symbolicate_global_dynamic_scope():
        def upgrade_scope(xs):
                kvs = list(xs.items())
                for k, v in kvs:
                        del xs[k]
                        sym = _intern_in_package(k, __cl)[0]
                        xs[sym] = v
                        _do_pyimport_symbol(sym, globals())
        assert not __tls__.dynamic_scope
        upgrade_scope(__global_scope__)

def _do_set(name, value, force_toplevel):
        _dynamic_frame_for_set(name, force_toplevel = force_toplevel)[name] = value
        return value

@boot("symbol",
      lambda _string_set, name, value, force_toplevel = None:
      _string_set(name, value, force_toplevel = force_toplevel, symbolicp = False),
      on_unboot = _boot_symbolicate_global_dynamic_scope)
def _string_set(symbol_name, value, force_toplevel = None, symbolicp = True):
        isinstance(symbol_name, str) or \
                 error("The first argument to %%STRING-SET must be a string, was: %s.", symbol_name.__repr__())
        name = _intern(symbol_name)[0] if symbolicp else symbol_name
        _do_set(name, value, force_toplevel)
        symbolicp and _pyimport_symbol(name)
        return value

# @boot("typep", lambda _, __, ___: error("A violent faecal odour hung in the air.."))
# @boot_defun
# def set(symbol, value, *_, force_toplevel = False):
#         _do_set(the(symbol_t, symbol), value, force_toplevel)
#         return value

@boot("symbol", lambda _, name: _find_dynamic_frame(_boot_check_type(stringp, name)) and t)
@boot_defun
def boundp(symbol):
        # Unregistered Issue COMPLIANCE-BOUNDP-ACCEPTS-STRINGS
        return _find_dynamic_frame(the(symbol_t, symbol)) and t

# Boot conditions: WARN, ERROR

def _conditionp(x):
        return isinstance(x, _cold_condition_type)

@boot("typep", lambda _, datum, *args, default_type = None, **keys:
              Exception(datum % args) if isinstance(datum, str) else
              (datum if not (args or keys) else
               error("Bad, bad evil is rising.  Now go and kill everybody.")) if _conditionp(datum) else
              datum(*args, **keys))
def _coerce_to_condition(datum, *args, default_type = None, **keys):
        def not_a_condition_specifier_error(x):
                raise Exception("Cannot coerce %s to a condition." % repr(x))
        type_specifier = _defaulted(default_type, error_t) if isinstance(datum, str) else datum

        type_ = (type_specifier             if isinstance(type_specifier, type)                                     else
                 None                       if _conditionp(type_specifier)                                          else
                 type_specifier.python_type if isinstance(type_specifier, symbol_t) and _symbol_type_specifier_p(type_specifier) else
                 not_a_condition_specifier_error(datum))
        cond = (datum              if type_ is None   else # Already a condition.
                type_(datum % args) if isinstance(datum, str) else
                type_(*args, **keys))
        return cond

@boot("typep", lambda _, datum, *args, **keys:
              _debug_printf("COLD WARNING: " + datum, *args, **keys))
@boot_defun
def warn(control, *args, **keys):
        condition = _coerce_to_condition(control, *args, **keys)
        check_type(condition, warning_t)
        signal(condition)
        badness = _poor_man_etypecase(condition,
                                      (style_warning_t, "STYLE-WARNING"),
                                      (warning_t,       "WARNING"))
        format(_symbol_value(_error_output_), "%s: %s\n", badness, condition)
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
        error(simple_package_error_t, "%s %s causes name-conflicts in %s between the following symbols: %s, %s." %
              (op, obj, pkg, x, y))

def _symbols_not_accessible_error(package, syms):
        def pp_sym_or_string(x):
                return "\"%s\"" % x if isinstance(x, str) else _print_nonkeyword_symbol(x)
        error(simple_package_error_t, "These symbols are not accessible in the %s package: (%s).",
              package_name(package), ", ".join((pp_sym_or_string(x) for x in syms)))

# Package system classes

_namespace.grow("PACKAGES")

@boot_defclass
class package_t(_collections.UserDict):
        def __repr__ (self): return "#<PACKAGE \"%s\">" % self.name # Cold PRINT-UNREADABLE-OBJECT
        def __bool__(self):  return True                            # Non-false even if empty.
        def __hash__(self):  return hash(id(self))
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
                        p.used_packages  = set(find_package(x) or _package_not_found_error(x)
                                               for x in used)
                        p.packages_using = set()
                        if p.used_packages and _storyteller.call("use_package", "using %s into %s", used, p):
                                for u_p in p.used_packages:
                                        assert isinstance(u_p, type(p))
                                        use_package(p, u_p)
                ## __init__()
                assert isinstance(name, str)
                self.name = name
                self.nicknames = nicknames

                validate_requested_package_names(name, nicknames)

                self.own         = set()                         # sym
                self.imported    = set()                         # sym
              # self.present     = own + imported
                self.inherited   = _collections.defaultdict(set) # sym -> set(pkg) ## _mapsetn(_slotting("external"), used_packages) -> source_package
                self.accessible  = make_hash_table()             # str -> sym          ## accessible = present + inherited
                self.external    = set()                         # sym                 ## subset of accessible
              # self.internal    = accessible - external

                setup_package_usage(self, use)

                ## Hit the street.
                self.data          = self.accessible
                _namespace["PACKAGES"].set(self, name)
                for nick in nicknames:
                        _namespace["PACKAGES"].set(self, nick)

@boot("symbol", lambda _, name, **keys: package_t(name, **keys))
@boot_defun
def make_package(name, **keys):
        return package_t(name, **keys)

@boot("symbol", lambda _, x: isinstance(x, package_t))
@boot_defun
def packagep(x): return isinstance(x, package_t)

@boot_defun
def package_name(x): return x.name

@boot_defun
def find_package(name):
        return (name if packagep(name) else
                _namespace["PACKAGES"].access(name if isinstance(name, str) else symbol_name(name))[0] or nil)

@boot_defun
def package_used_by_list(package):
        p = _coerce_to_package(package)
        return p.packages_using if p else _package_not_found_error(package)

@boot_defclass
class symbol_t(): # Turned to a symbol, during the package system bootstrap.
        def __str__(self):
                return _print_symbol(self)
        def __repr__(self):
                return str(self)
        def __init__(self, name):
                (self.name, self.package,
                 (self.function,
                  self.macro_function,
                  self.compiler_macro_function,
                  self.symbol_macro_expansion,
                  self.known)) = name, nil, (None, nil, nil, None, nil)
                ## Critically, the compiler must never produce two symbols with the same
                ## package and name.
                self.function_pyname = None
                self.symbol_pyname   = None
        def __bool__(self):
                return self is not nil

@boot("symbol", lambda _, name, **keys: symbol_t(name))
@boot_defun
def make_symbol(name, **keys):
        return symbol_t(name, **keys)

@boot("symbol", lambda _, x: isinstance(x, symbol_t))
@boot_defun
def symbolp(x):  return isinstance(x, symbol_t)

@boot_defun
def keywordp(x): return isinstance(x, symbol_t) and symbol_package(x) is __keyword

@boot_defun
def symbol_name(x):            return x.name
@boot_defun
def symbol_package(x):         return x.package
@boot_defun # Unregistered Issue COMPLIANCE-SYMBOL-VALUE
def symbol_value(symbol):      return _symbol_value(the(symbol_t, symbol))
## Unregistered Issue FDEFINITION-SYMBOL-FUNCTION-AND-COMPILER-GFUNS-NEED-SYNCHRONISATION
def _symbol_function(symbol):  return (symbol.known          or
                                       symbol.macro_function or
                                       symbol.function       or
                                       _debug_printf("no fun: %s", symbol) or
                                       error(undefined_function_t, symbol))

def _do_find_symbol(str, package):
        return gethash(str, package.accessible, None)[0]

def _find_symbol_or_fail(x, package = None):
        sym = _do_find_symbol(x, _coerce_to_package(package))
        return (sym if sym is not None else
                _symbols_not_accessible_error(p, [x]))

def _symbol_relation(x, p):
        "NOTE: here we trust that X belongs to P, when it's a symbol."
        s = gethash(x, p.accessible, None)[0] if isinstance(x, str) else x
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
        nil = symbol_t.__new__(symbol_t)
        (nil.name,
         nil.package,
         nil.function,
         nil.macro_function,
         nil.compiler_macro_function,
         nil.symbol_macro_expansion,
         nil.known) = "NIL", __cl, nil, nil, nil, None, nil
        nil.symbol_pyname, nil.function_pyname = None, None
        return _do_intern_symbol(nil, __cl)

NIL = nil = _cold_make_nil()

@boot_defun
def null(x): return x is nil or x == ()

# Package system core

def _intern_in_package(x, p):
        s, presentp = (error("X must be a string: %s.", repr(x)) if not isinstance(x, str) else
                       (p.accessible.get(x), True)                   if x in p.accessible              else
                       (None,                False))
        if not presentp:
                s = _do_intern_symbol(make_symbol(x), p)
        return s, presentp

def _coerce_to_package(x, if_null = "current"):
        return (find_package(x)                                              if isinstance(x, (str, symbol_t, package_t)) else
                (_symbol_value(_package_) if if_null == "current" else
                 _package_not_found_error(x))                                if (not x)                                   else
                simple_type_error("COERCE-TO-PACKAGE accepts only package designators -- packages, strings or symbols, was given '%s' of type %s.",
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
        conflict_set = { x.name for x in syms.values() } & set(dest.accessible.keys())
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
        symbols, package = symbols if isinstance(symbols, list) else [symbols], _coerce_to_package(package)
        assert(all(isinstance(x, symbol_t)
                   for x in symbols))
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

def _protosymbolicate(x, name, slot):
        sym, _ = _intern(name)
        setattr(sym, slot, x)
        return sym

def _symbolicate(x, name, slot, globals):
        sym = _protosymbolicate(x, name, slot)
        pyname = _frost.lisp_symbol_name_python_name(name)
        _frost.setf_global(sym, pyname, globals)

def _init_package_system_0():
        global __packages__
        global __keyword
        global t, T, make_symbol, make_package
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
        __global_scope__.update({ "T": t, "NIL": nil })
        export([t, nil] + [_intern(n[0] if isinstance(n, tuple) else n, __cl)[0]
                           for n in __core_symbol_names__ + __more_symbol_names__],
               __cl)
        for spec in __core_symbol_names__ + __more_symbol_names__:
                lisp_name, python_name = (spec, _frost.lisp_symbol_name_python_name(spec)) if isinstance(spec, str) else spec
                _frost.setf_global(_find_symbol_or_fail(lisp_name, __cl), python_name, globals())
                # Unregistered Issue PACKAGE-SYSTEM-INIT-SHOULD-USE-GLOBAL-SETTER-INSTEAD-OF-CUSTOM-HACKERY
        # secondary
        package_t("COMMON-LISP-USER", use = [__cl], boot = True)
        __global_scope__["*PACKAGE*"] = __cl # COLD-SETQ
        _protosymbolicate(symbol_t, "SYMBOL", "python_type")
        @boot_defun
        def make_symbol(name):
                return symbol_t(name)
        _protosymbolicate(package_t, "PACKAGE", "python_type")
        @boot_defun
        def make_package(name, nicknames = [], use = []):
                if nicknames:
                        _not_implemented("In MAKE-PACKAGE %s: package nicknames are ignored.", repr(name))
                return package_t(name if isinstance(name, str) else symbol_name(name),
                                 ignore_python = True, use = [])

_init_package_system_0()

_unboot_set("symbol")
# _unboot_set("print") # This can turn 4.8s of debug printing into 30+s

# GENSYM

__gensym_counter__ = 0

def _gensymname(x = "N"):
        # Unregistered Issue GENSYM-NOT-THREAD-SAFE
        global __gensym_counter__
        __gensym_counter__ += 1
        return x + str(__gensym_counter__)

@boot_defun
def gensym(x = "G"):
        # A version adding a name is defined later: GENSYM-TN.
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
                with _env_cluster(dict(zip(vars, vals))):
                        return body()
        else:
                return _env_cluster(vars if hash_table_p(vars) else
                                    cluster)

# CATCH, THROW, BLOCK, RETURN-FROM

# WARNING: non-specific try/except clauses and BaseException handlers break this!
class __catcher_throw__(_cold_condition_type):
        def __init__(self, ball, value, reenable_pytracer = nil):
                self.ball, self.value, self.reenable_pytracer = ball, value, reenable_pytracer
        def __str__(self):
                return "@<ball %s>" % (self.ball,)

def __catch(ball, body):
        "This seeks the stack like mad, like the real one."
        try:
                return body()
        except __catcher_throw__ as ct:
                # format(t, "catcher %s, ball %s -> %s", ct.ball, ball, "caught" if ct.ball is ball else "missed")
                if ct.ball is ball:
                        __catch_maybe_reenable_pytracer(ct)
                        return ct.value
                else:
                        raise

def __catch_maybe_reenable_pytracer(ct):
        if ct.reenable_pytracer:
                _frost.enable_pytracer()

def __throw(ball, value):
        "Stack this seeks, like mad, like the real one."
        raise __catcher_throw__(ball = ball, value = value, reenable_pytracer = boundp(_signalling_frame_))

def __block__(fn):
        "An easy decorator-styled interface for block establishment."
        nonce = gensym("BLOCK-")
        ret = (lambda *args, **keys:
                       __catch(nonce,
                               lambda: fn(*args, **keys)))
        setattr(ret, "ball", nonce)
        return ret

def __block(nonce_or_fn, body = None):
        """A lexically-bound counterpart to CATCH/THROW.
Note, how, in this form, it is almost a synonym to CATCH/THROW -- the lexical aspect
of nonce-ing is to be handled manually."""
        if not body: # Assuming we were called as a decorator..
                return __block__(nonce_or_fn)
        else:
                return __catch(nonce_or_fn, body)

@boot_defun
def __return_from(nonce, value):
        nonce = ((getattr((symbol_function(nonce) if isinstance(nonce, symbol_t) else
                           nonce), "ball", None) or
                  error("RETURN-FROM was handed a function %s, but it is not cooperating in the "
                        "__BLOCK__ nonce passing syntax.", nonce)) if isinstance(nonce, _cold_function_type) else
                 ## This can mean either the @defun-ned function, or absent a function definition, the symbol itself.
                 (getattr(nonce.function, "ball", nonce))          if isinstance(nonce, symbol_p)            else
                 nonce                                             if isinstance(nonce, str)                 else
                 error("In RETURN-FROM: nonce must either be a string, or a function designator;  was: %s.", repr(nonce)))
        __throw(nonce, value)

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
        handler_clusters = _symbol_value(_handler_clusters_)
        for n, cluster in enumerate(reversed(handler_clusters)):
                ## Unregistered Issue CLUSTERS-NOT-PROPERLY-UNWOUND-FOR-HANDLERS
                for type, handler in cluster:
                        if not isinstance(type, str):
                                if isinstance(cond, type):
                                        hook = _symbol_value(_prehandler_hook_)
                                        if hook:
                                                frame = assoc("__frame__", cluster)
                                                assert(frame)
                                                hook(cond, frame, hook)
                                        with progv({ _handler_clusters_: handler_clusters[:-(n + 1)]}):
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

_intern_and_bind_names_in_module("*DEBUG-CONDITION*", "*DEBUG-RESTARTS*", "*NESTED-DEBUG-CONDITION*")

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
                        if isinstance(condition, cell_error_t):
                                format(_symbol_value(_error_output_),
                                       "\n(CELL-ERROR-NAME %s) = %s\n",
                                       _nested_debug_condition_, cell_error_name(condition))
                handler_case(lambda: _print_debugger_invocation_reason(condition,
                                                                       _symbol_value(_error_output_)),
                             (error_t, error_handler_body))
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

def integerp(o):      return isinstance(o, int)
def floatp(o):        return isinstance(o, float)
def complexp(o):      return isinstance(o, complex)
def numberp(o):       return isinstance(o, (int, float, complex))
def hash_table_p(o):  return isinstance(o, _cold_hash_table_type)
def _listp(o):        return isinstance(o, _cold_list_type)
def _boolp(o):        return isinstance(o, bool)
def sequencep(x):     return getattr(type(x), "__len__", None) is not None

# Types mappable to python

def _define_python_type_map(symbol_or_name, type_):
        not isinstance(symbol_or_name, (str, symbol_t)) and \
            error("In DEFINE-PYTHON-TYPE-MAP: first argument must be either a string or a symbol, was: %s.", repr(symbol_or_name))
        not isinstance(type_, type) and \
            error("In DEFINE-PYTHON-TYPE-MAP: second argument must be a Python type, was: %s.", repr(type_))
        symbol = (symbol_or_name if symbolp(symbol_or_name) else
                  _intern(symbol_or_name)[0])
        _protosymbolicate(type_, symbol.name, "python_type")
        _frost.setf_global(type_, _frost.lisp_symbol_name_python_type_name(symbol.name),
                           globals = globals())
        symbol.python_type = type_
        return symbol

_define_python_type_map("INTEGER",           int)
_define_python_type_map("FLOAT",             float)
_define_python_type_map("COMPLEX",           complex)

_define_python_type_map("STRING",            str)
_define_python_type_map("HASH-TABLE",        _cold_hash_table_type)

_define_python_type_map("FUNCTION",          _cold_function_type)

_define_python_type_map("STREAM",            _cold_stream_type)

_define_python_type_map("CLASS",             type) # Ha.

_define_python_type_map("CONDITION",         BaseException)
_define_python_type_map("ERROR",             Exception)
_define_python_type_map("SERIOUS-CONDITION", Exception)
_define_python_type_map("END-OF-FILE",       EOFError)

## non-standard type names
_define_python_type_map("PYBOOL",      bool)
_define_python_type_map("PYLIST",      list)
_define_python_type_map("PYTUPLE",     tuple)
_define_python_type_map("PYBYTES",     bytes)
_define_python_type_map("PYBYTEARRAY", bytearray)
_define_python_type_map("PYSET",       set)
_define_python_type_map("PYFROZENSET", frozenset)

# Complex type specifier machinery: %TYPE-MISMATCH, @DEFTYPE, TYPEP

def _type_specifier_complex_p(x):
        """Determines, whether a type specifier X constitutes a
complex type specifier."""
        return isinstance(x, tuple)

def _invalid_type_specifier_error(x, complete_type = None):
        error("%s is not a valid type specifier%s.",
              x, ("" if not complete_type else
                  (" (within type specifier %s)" % (complete_type,))))

def _complex_type_mismatch(x, type):
        ret = type[0].type_predicate(x, type)
        if isinstance(ret, tuple) and len(ret) != 3:
                error("Type matcher for %s returned an invalid value: %s.", type[0], repr(ret))
        return (ret if not (isinstance(ret, tuple) and ret[2]) else
                _invalid_type_specifier_error(ret[1], complete_type = type))

def _type_mismatch(x, type_):
        """Determine, whether X does not belong to TYPE, and if so,
return a triple, specifying the specific parts of X and TYPE being in
disagreement and, as a third element, a boolean, denoting whether the
type specifier was malformed.  Otherwise, when X is of TYPE, a
negative boolean value is returned."""
        return (((not isinstance(x, type_)) and
                 (x, type_, False))                            if isinstance(type_, type)               else
                nil                                            if type_ is t                            else
                (((not isinstance(x, type_.python_type)) and
                  (x, type_, False))                           if hasattr(type_, "python_type")         else
                 _complex_type_mismatch(x, tuple([type_]))     if hasattr(type_, "type_predicate")      else
                 _invalid_type_specifier_error(type_))         if isinstance(type_, symbol_t)           else
                _complex_type_mismatch(x, type_)               if (isinstance(type_, tuple) and type_ and
                                                                   hasattr(type_[0], "type_predicate")) else
                _invalid_type_specifier_error(type_))

@boot_defun
def typep(x, type):
        return not _type_mismatch(x, type)

def _deftype(type_name_or_fn, globals = None):
        def do_deftype(fn, type_name = type_name_or_fn):
                nonlocal globals
                old_global_name = (type_name_or_fn.__name__ if functionp(type_name_or_fn) else
                                   fn.__name__)
                globals = _defaulted(globals, _py.globals())
                old_global = globals.get(old_global_name, None)
                symbol = _intern(type_name)[0]
                symbol.type_predicate = fn
                _frost.setf_global(symbol, old_global_name + ("" if old_global_name.endswith("_") else "_") + "t",
                                   globals)
                return old_global
        return (do_deftype(type_name_or_fn, type_name = _frost.python_name_lisp_symbol_name(type_name_or_fn.__name__)) if functionp(type_name_or_fn) else
                do_deftype                                                                                             if isinstance(type_name_or_fn, str)   else
                error("In DEFTYPE: argument must be either a function or a string, was: %s.",
                      repr(symbol_name_or_fn)))

@boot_defun
def the(type, x):
        mismatch = _type_mismatch(x, type)
        return (x if not mismatch else
                error(simple_type_error_t,
                      format_control = "The value %s (of type %s) is not of type %s%s.",
                      format_arguments = (x, type_of(x), type,
                                          ("" if (not _type_specifier_complex_p(type)) or type is mismatch[1] else
                                              (", specifically, the value %s is not of type %s" % (princ_to_string(mismatch[0]), mismatch[1]))))))

@boot_defun
def check_type(x, type):
        the(type, x)

def _of_type(x):
        return lambda y: typep(y, x)

def _not_of_type(x):
        return lambda y: not typep(y, x)

# Complex type definitions

@_deftype
def boolean(x, type):
        return ((x, type, True)  if len(type) is not 1 else
                (x, type, False) if x not in [t, nil]      else
                nil)

@_deftype
def null(x, type):
        return ((x, type, True)  if len(type) is not 1 else
                (x, type, False) if x is not nil           else
                nil)

@_deftype
def keyword(x, type):
        return ((x, type, True)  if len(type) is not 1 else
                (x, type, False) if not keywordp(x)        else
                nil)

@_deftype("OR")
def or_(x, type):
        return ((x, type, False) if len(type) is 1 else
                _poor_man_let(list(_type_mismatch(ix, ty) for ix, ty in zip([x] * (len(type) - 1), type[1:])),
                              lambda mismatches:
                                      (_some_fast(lambda m: m and m[2] and m, mismatches) or
                                       (all(x for x in mismatches) and (x, type, False)))))

@_deftype("AND")
def and_(x, type):
        return (nil       if len(type) is 1 else
                _some_fast(lambda ix: _type_mismatch(x, ix), type[1:]))

@_deftype("NOT")
def not_(x, type):
        return ((x, type, True) if len(type) is not 2 else
                _poor_man_let(_type_mismatch(x, type[1]),
                              lambda m: ((x, type, False) if not m      else
                                         m                if m and m[2] else
                                         nil)))

@_deftype
def member(x, type):
        return ((x not in type[1:]) and
                (x, type, False))

@_deftype
def satisfies(x, type):
        return ((x, type, True) if ((len(type) is not 2) or
                                    not isinstance(type[1], _cold_function_type)) else
                ((not type[1](x)) and
                 (x, type, False)))

@_deftype
def eql(x, type):
        return ((x, type, True) if len(type) is not 2 else
                ((not eql(x, type[1])) and
                 (x, type, False)))

@_deftype
def unsigned_byte(x, type):
        return (((x, type, False) if not isinstance(x, int) or minusp(x) else nil)                        if len(type) is 1 else
                ((x, type, False) if not isinstance(x, int) or minusp(x) or (x >= 1 << type[1]) else nil) if len(type) is 2 else
                (x, type, True))

## Non-standard
@_deftype
def maybe(x, type):
        return ((x, type, True)  if len(type) is not 2 else
                _poor_man_let(_type_mismatch(x, type[1]),
                              lambda m: (nil if not m                         else
                                         m   if ((m and m[2]) or
                                                 not (x is nil or x is None)) else
                                         nil)))

@_deftype
def pylist(x, type):
        return ((x, type, True)  if len(type) is not 2      else
                (x, type, False) if not isinstance(x, list) else
                _some_fast(lambda ix: _type_mismatch(ix, type[1]), x))

@_deftype
def homotuple(x, type):
        return ((x, type, True)  if len(type) is not 2       else
                (x, type, False) if not isinstance(x, tuple) else
                _some_fast(lambda ix: _type_mismatch(ix, type[1]), x))

@_deftype
def pyseq(x, type):
        return ((x, type, True)  if len(type) is not 2               else
                (x, type, False) if not isinstance(x, (list, tuple)) else
                _some_fast(lambda ix: _type_mismatch(ix, type[1]), x))

@_deftype
def cons(x, type):
        return ((x, type, True)                           if len(type) not in (1, 3)                   else
                (x, type, False)                          if not (isinstance(x, list) and len(x) == 2) else
                _some_fast_2(_type_mismatch, x, type[1:]) if len(type) is 3                            else
                nil) 

@_deftype
def pyfixlist(x, type):
        return ((x, type, False) if not (isinstance(x, list) and len(x) == len(type) - 1) else
                _some_fast_2(_type_mismatch, x, type[1:]))

@_deftype
def pytuple(x, type):
        return ((x, type, False) if not (isinstance(x, tuple) and len(x) == len(type) - 1) else
                _some_fast_2(_type_mismatch, x, type[1:]))
# Unregistered Issue TEACHABLE-TYPE-CHECKING-PRACTICE-AND-TOOL-CONSTRUCTION

@_deftype
def partuple(x, type):
        return ((x, type, False) if not (isinstance(x, tuple) and len(x) >= len(type) - 1) else
                _some_fast_2(_type_mismatch, x, type[1:]))

__variseq__ = (pytuple_t, (eql_t, maybe_t), t) # Meta-type, heh..
@_deftype
def varituple(x, type):
        # correctness enforcement over speed?
        fixed_t, maybes_t = _prefix_suffix_if_not(_of_type(__variseq__), type[1:])
        if not all(typep(x, __variseq__) for x in maybes_t):
                return (x, type, True)   # fail
        fixlen = len(fixed_t)
        ctype = (or_t,) + tuple(t[1] for t in maybes_t)
        return ((x, type) if len(x) < fixlen else
                _some_fast_2(_type_mismatch, x[:fixlen], fixed_t) or
                _some_fast(lambda ix: _type_mismatch(ix, ctype), x[fixlen:]))

def _eql_type_specifier_p(x): return isinstance(x, tuple) and len(x) is 2 and x[0] is eql_t

_unboot_set("typep")

# Type relationships, rudimentary

def subtypep(sub, super):
        def coerce_to_python_type(x):
                return (x             if isinstance(x, _cold_class_type)   else
                        x.python_type if isinstance(x, symbol_t)           else
                        error("In SUBTYPEP: arguments must be valid type designators, but %s wasn't one.", repr(x)))
        def do_subclass_check(sub, super):
                return issubclass(coerce_to_python_type(sub),
                                      coerce_to_python_type(super))
        return (do_subclass_check(sub, super)                  if super is not t                                     else
                _not_implemented("complex type relatioships: %s vs. %s.",
                                 sub, super)                   if isinstance(sub, tuple) or isinstance(super, tuple) else
                error("%s is not a valid type specifier", sub) if not (typep(sub, (or_t, type, (eql_t, t))) and
                                                                       typep(sub, (or_t, type, (eql_t, t))))         else
                sub is super or super is t)

# Toplevel definitions: @DEFUN and @DEFCLASS

doit = False
def _make_cold_definer(definer_name, predicate, slot, preprocess, mimicry):
        def cold_definer(name_or_obj):
                obj, sym, name = _interpret_toplevel_value(name_or_obj, predicate)
                def do_cold_def(o):
                        setattr(sym, slot, o)
                        # symbol = (_intern(_defaulted(name, _frost.python_name_lisp_symbol_name(o.__name__)))[0]
                        #           if isinstance(name, str) else
                        #           name if symbolp(name) else
                        #           error("In %s: bad name %s for a cold object.", definer_name))
                        o = preprocess(o)
                        mimicry(sym, o)
                        return o
                return (do_cold_def(obj) if obj                                      else
                        do_cold_def      if isinstance(name_or_obj, (str, symbol_t)) else
                        error("In %s: argument must be either satisfy %s or be a string;  was: %s.",
                              definer_name, predicate, repr(name_or_obj)))
        cold_definer.__name__ = definer_name
        return cold_definer

del boot_defun
del boot_defclass

defun            = _cold_defun    = _make_cold_definer("%COLD-DEFUN",    functionp,
                                                       "function",    identity, _frost.make_object_like_python_function)
defclass         = _cold_defclass = _make_cold_definer("%COLD-DEFCLASS", lambda x: isinstance(x, type),
                                                       "python_type", identity,  _frost.make_object_like_python_class)
defun_with_block = _cold_defun_with_block = _make_cold_definer("%COLD-DEFUN-WITH-BLOCK", functionp,
                                                               "function", __block__, _frost.make_object_like_python_function)
for fn  in __boot_defunned__:   _frost.setf_global(defun(fn),     fn.__name__,  globals())
for cls in __boot_defclassed__: _frost.setf_global(defclass(cls), cls.__name__, globals())
doit = True

# Delayed class definitions

@defclass
class nil():
        @classmethod
        def __instancecheck__(_, __): return False # This is an empty type
_symbolicate(nil, "NIL", "python_type", globals())

@defclass
class t():
        @classmethod
        def __instancecheck__(_, __): return True  # This is the absolute sum type
_symbolicate(t, "T", "python_type", globals())

def _attrify_args(self, locals, *names):
        for name in names:
                setattr(self, name, locals[name])

@defclass
class simple_condition_t(condition_t):
        def __init__(self, format_control, format_arguments):
                _attrify_args(self, locals(), "format_control", "format_arguments")
                # _debug_printf("About to signal a simple condition of type %s:\n%s", type(self), self)
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
class simple_error_t(simple_condition_t, error_t):
        pass
@defclass
class package_error_t(error_t):
        pass
@defclass
class simple_package_error_t(simple_error_t, package_error_t):
        pass

# Rudimentary multiple values

#     The implemented version of NTH-VALUES is a soft one, which doesn't fail on values not
#     participating in the M-V frame protocol.

_intern_and_bind_names_in_module("%MV-MARKER")

def _values_frame(*xs):
        return (_mv_marker,) + xs

def _values_frame_p(x):
        return isinstance(x, tuple) and x[0] is _mv_marker

def _values_frame_values(x):
        return x[1:]

def _values_frame_project(n, values_form):
        return ((nil if n > len(values_form) - 2 else
                 values_form[n + 1])
                if _values_frame_p(values_form) else
                (nil if n else values_form))

# Early object system

@defun
def find_class(x, errorp = t):
        _not_implemented()

@defun
def make_instance(class_or_name, **initargs):
        return (class_or_name             if isinstance(class_or_name, _cold_class_type) else
                class_or_name.python_type if isinstance(class_or_name, symbol_t)         else
                error("In MAKE-INSTANCE %s: first argument must be a class specifier.", class_or_name))(**initargs)

def _make_missing_method(cls, name):
        return lambda *_, **_k: error("Missing method %s in class %%s." % name.upper(), cls)

# PRINT-UNREADABLE-OBJECT, sort of

def print_unreadable_object(object, stream, body, identity = None, type = None):
        write_string("#<", stream)
        if type:
                format(stream, "%s ", type_of(object).__name__)
        body()
        if identity:
                format(stream, " {%x}", id(object))
        write_string(">", stream)

# Readtable and WITH-STANDARD-IO-SYNTAX

@defclass
class readtable_t(_collections.UserDict):
        def __init__(self, case = _keyword("upcase")):
                self.case = the((member_t, _keyword("upcase"), _keyword("downcase"), _keyword("preserve"), _keyword("invert")),
                                case)
                self.data = make_hash_table()

def readtablep(x):     return isinstance(x, readtable_t)
def readtable_case(x): return the(readtable_t, x).case

def copy_readtable(x):
        check_type(x, readtable_t)
        new = readtable(case = readtable_case(x))
        new.dict = make_hash_table()
        return new

__standard_pprint_dispatch__ = make_hash_table()          # XXX: this is crap!
__standard_readtable__       = make_instance(readtable_t) # XXX: this is crap!

_intern_and_bind_names_in_module("*PRINT-ARRAY*", "*PRINT-BASE*", "*PRINT-CASE*", "*PRINT-CIRCLE*",
                                 "*PRINT-ESCAPE*", "*PRINT-GENSYM*", "*PRINT-LENGTH*", "*PRINT-LEVEL*",
                                 "*PRINT-LINES*", "*PRINT-MISER-WIDTH*", "*PRINT-PPRINT-DISPATCH*",
                                 "*PRINT-PRETTY*", "*PRINT-RADIX*", "*PRINT-READABLY*", "*PRINT-RIGHT-MARGIN*",
                                 "*READ-BASE*", "*READ-DEFAULT-FLOAT-FORMAT*", "*READ-EVAL*",
                                 "*READ-SUPPRESS*",
                                 "*READTABLE*")
__standard_io_syntax__ = dict({_package_               : find_package("COMMON-LISP-USER"),
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

_NoneType         = type(None)

reduce            = _functools.reduce
_repeat           = _itertools.repeat
sort              = sorted
_curry            = _functools.partial

stringp           = _neutrality.stringp
_write_string     = _neutrality._write_string

def _classp(x):     return isinstance(x, type)
def _frozensetp(o): return isinstance(o, frozenset)
def _setp(o):       return isinstance(o, (set, frozenset))
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
                return body() if isinstance(body, _cold_function_type) else body

def _poor_man_case(val, *clauses):
        for (cval, body) in clauses:
                if ((val == cval or (cval is True) or (cval is t)) if not isinstance(cval, list) else
                    val in cval):
                        return body() if isinstance(body, _cold_function_type) else body

def _poor_man_ecase(val, *clauses):
        for (cval, body) in clauses:
                if ((val == cval) if not isinstance(cval, list) else
                    val in cval):
                        return body() if isinstance(body, _cold_function_type) else body
        error("%s fell through ECASE expression. Wanted one of %s.", val, [ x[0] for x in clauses ])

def _poor_man_typecase(val, *clauses):
        for (ctype, body) in clauses:
                if (ctype is t) or (ctype is True) or typep(val, ctype):
                        return body() if isinstance(body, _cold_function_type) else body

def _poor_man_etypecase(val, *clauses):
        for (ctype, body) in clauses:
                if (ctype is t) or (ctype is True) or typep(val, ctype):
                        return body() if isinstance(body, _cold_function_type) else body
        else:
                simple_type_error("%s fell through ETYPECASE expression. Wanted one of (%s).",
                                  val, ", ".join((c[0].__name__ for c in clauses)))

def _cold_constantp(form):
        # Coldness:
        #  - slow handling of constant variables
        #  - no handling of DEFCONSTANT-introduced variables
        #  - additional constant forms
        return (isinstance(form, (int, float, complex, str)) or
                (type_of(form).__name__ == "symbol" and
                 ((form.package.name == "KEYWORD") or
                  (form.package.name == "COMMON-LISP" and form.name in ["T", "NIL"]))) or
                (isinstance(form, list) and
                 (len(form) == 2                        and
                  type_of(form[0]).__name__ == "symbol" and
                  form.package.name == "COMMON-LISP"    and
                  form.name in ["QUOTE"])))
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

_case_attribute_map = dict(UPCASE     = string_upcase,
                               DOWNCASE   = string_downcase,
                               CAPITALIZE = string_capitalize,
                               PRESERVE   = identity)
def _case_xform(type_, s):
        if not (isinstance(type_, symbol_t) and type_.package.name == "KEYWORD"):
                error("In CASE-XFORM: case specifier must be a keyword, was a %s: %s.", type(type_), _print_symbol(type_))
        return _case_attribute_map[type_.name](s)

# Possibly dangling cold boot code

#     I wonder if this boot state infrastructure is a good idea:
#     - it tangles the flow of things (?)

def _global(x):
        """This is important due to the single namespace, and the
consequent shadowing of various specifiers."""
        return _frost.global_(x, globals())[0]

def _cold_format(destination, control_string, *args):
        string = control_string % args
        if not destination:
                return string
        else:
                _write_string(string, _sys.stderr if destination is t else destination)
format = _cold_format
def _cold_princ_to_string(x):
        return repr(x)
princ_to_string = _cold_princ_to_string
# Unregistered Issue PACKAGE-INIT-MUST-TAKE-COLD-SYMBOL-VALUES-INTO-ACCOUNT
def _cold_probe_file(pathname):
        assert(isinstance(pathname, str))
        return _os.path.exists(the(string_t, pathname))
probe_file = _cold_probe_file

# Python module compilation

def _load_code_object_as_module(name, co, filename = "", builtins = None, globals = None, locals = None, register = True):
        check_type(co, type(_load_code_object_as_module.__code__))
        mod = _imp.new_module(name)
        mod.__filename__ = filename
        if builtins:
                mod.__dict__["__builtins__"] = builtins
        if register:
                _sys.modules[name] = mod
        globals = _defaulted(globals, mod.__dict__)
        locals  = _defaulted(locals, mod.__dict__)
        exec(co, globals, locals)
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
                setattr(parent_package, postdot_name, mod)
                parent_package.__children__.add(mod)
                mod.__parent__ = parent_package
        if packagep:
                mod.__children__ = set()

def _py_compile_and_load(*body, modname = "", filename = "", lineno = 0, **keys):
        return _load_code_object_as_module(
                modname,
                _py.compile(_ast.fix_missing_locations(_ast_module(list(body), lineno = lineno)), filename, "exec"),
                register = nil,
                filename = filename,
                **keys)

def _ast_compiled_name(name, *body, function = nil, **keys):
        mod, globals, locals = _py_compile_and_load(*body, **keys)
        return locals[function or name]

# Python frames

def _all_threads_frames():
        return _sys._current_frames()

def _this_frame():
        return _sys._getframe(1)

_frame = type(_this_frame())

def _framep(x):
        return isinstance(x, _frame)

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

# Frame pretty-printing

def _frame_fun_name(f):          return f.f_code.co_name

def _print_function_arglist(f):
        argspec = _inspect.getargspec(f)
        return ", ".join(argspec.args +
                         (["*" + argspec.varargs]   if argspec.varargs  else []) +
                         (["**" + argspec.keywords] if argspec.keywords else []))

def _pp_frame(f, align = None, handle_overflow = None, lineno = None, frame_id = None):
        fun = _frame_fun(f)
        fun_name, fun_params, filename = _fun_info(fun)[:3]
        align = ((align or 10) if handle_overflow else
                 _defaulted(align, 0))
        return ("%s%s%s %s(%s)" % (((_frame_id(f)[:4] + " ") if frame_id else ""),
                                   filename + ("" if align else ":") + (" " * (align - (len(filename) % align if align else 0))),
                                   ("%d:" % _frame_lineno(f)) if lineno else "",
                                   fun_name, ", ".join(fun_params)))

def _print_frame(f, stream = None, **keys):
        write_string(_pp_frame(f, **keys), _defaulted_to_var(stream, _debug_io_))

def _print_frames(fs, stream = None, frame_ids = None):
        for i, f in enumerate(fs):
                format(_defaulted_to_var(stream, _debug_io_), "%2d: %s\n" %
                       (i, _pp_frame(f, lineno = True, frame_id = frame_ids)))

def _backtrace(x = -1, stream = None, frame = None, frame_ids = None, offset = 0):
        _print_frames(_frames_calling(_defaulted(frame, _this_frame()))[1 + offset:x],
                      _defaulted_to_var(stream, _debug_io_),
                      frame_ids = frame_ids)

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
        return ("..".join((_pp_frame_in_chain(f, t) for f in xs) if all_pretty else
                          ([_pp_frame_in_chain(f) for f in xs[:-1]] +
                           [_pp_frame_in_chain(xs[-1], t)])))

def _pp_chain_of_frame(x, callers = 5, *args, **keys):
        fs = _frames_calling(x, callers)
        fs.reverse()
        return _pp_frame_chain(fs, *args, **keys)

def _escape_percent(x):
        return x.replace("%", "%%")

# Higher-level debug trace functions

# lf = open("/home/deepfire/lf", "w")
def _frame_chain_hash(f, ignore_callers = set(["<lambda>"])):
        "Return an MD5 digest of the caller name chain, with callers listed in IGNORE-CALLERS omitted."
        def f_digestible(f):
                name = f.f_code.co_name
                return name.encode() if name not in ignore_callers else b''
        fchain = _frames_calling(f)[1:]
        retv = reduce((lambda acc, f:
                               acc.update(f_digestible(f)) or acc),
                      fchain, _hashlib.new("md5")).hexdigest()
        # _fprintf(lf, "%s %s\n", [ f_str(x) for x in reversed(chain) ], r)
        return retv

def _frame_id(f):
        return _hashlib.new("md5", ("%x" % id(f)).encode()).hexdigest()

def _here(note = None, *args, callers = 5, stream = None, default_stream = _sys.stderr, frame = None, print_fun_line = None, all_pretty = None, offset = 0):
        def _do_format(x, args):
                try:
                        return x % (tuple(args))
                except _cold_error_type as cond:
                        return "#<error formatting %s into %s: %s>" % (args.__repr__(), note.__repr__(), cond)
        def format_args():
                return (""           if not note else
                        " - " + note if not args else
                        # Unregistered Issue IDEA-MAPXFORM-IF
                        _do_format(note, args))
        return _debug_printf("    (%s)  %s:\n      %s",
                             _threading.current_thread().name.upper(),
                             _pp_chain_of_frame(_defaulted(frame, _caller_frame(offset)),
                                                callers = callers - 1,
                                                print_fun_line = print_fun_line,
                                                all_pretty = all_pretty),
                             _without_condition_system(format_args),
                             # _defaulted(stream, default_stream)
                             )

def _locals_printf(locals, *local_names):
        # Unregistered Issue NEWLINE-COMMA-SEPARATION-NOT-PRETTY
        _fprintf(_sys.stderr, ", ".join((("%s: %%s" % x) if isinstance(x, str) else "%s")
                                        for x in local_names) + "\n",
                 *((locals[x] if isinstance(x, str) else "\n") for x in local_names))

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
#   co_code: b'y\x11\x00t\x00\x00d\x01\x00|\x00\x00\x83\x02\x00\x01Wn,\x00\x04t\x01\x00k\n\x00r?\x00\x01\x01\x00\x01z\x0c\x00t\x02\x00\x83\x00\x00SWYd\x02\x00d\x02\x00\x01\x00~\x01\x00Xn\x01\x00Xd\x02\x00S'
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

def _plist_alist(xs):
        acc = []
        for i in range(0, len(xs), 2):
                acc.append((xs[i], xs[i + 1]))
        return acc

def _plist_keys(xs):        return xs[::2]
def _plist_values(xs):      return xs[1::2]
def _plist_keys_values(xs): return xs[::2], xs[1::2]

def _alist_hash_table(xs):
        return dict(xs)

# %CACHE

class _cache(_collections.UserDict):
        def __init__(self, filler):
                self.filler = filler
                self.data = dict()
        def __getitem__(self, key):
                check_type(key, pytuple_t)
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
        return x[0] if isinstance(x, list) else x
def _ensure_cons(x, default = None):
        return x if isinstance(x, list) and len(x) == 2 else [x, default]

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

def _mapseparaten(f, xs):
        s0, s1 = set(), set()
        for s0r, s1r in (f(x) for x in xs):
                s0 |= s0r; s1 |= s1r
        return s0, s1

def _separate(n, f, xs):
        ss = tuple(set() for _ in range(n))
        for rss in (f(x) for x in xs):
                for s, rs in zip(ss, rss):
                        s |= rs
        return ss

__combiners__ = { set: set.add, list: list.append }
def _recombine(spec, f, xss):
        accs  = tuple(f() for f in spec)
        combs = tuple(__combiners__[type(a)] for a in accs)
        for xs in xss:
                for acc, comb, reselt in zip(accs, combs, f(xs)):
                        comb(acc, reselt)
        return accs
def _recombine_star(spec, f, *xss):
        accs  = tuple(f() for f in spec)
        combs = tuple(__combiners__[type(a)] for a in accs)
        for xs in zip(*xss):
                for acc, comb, reselt in zip(accs, combs, f(*xs)):
                        comb(acc, reselt)
        return accs

def _slotting(x):             return lambda y: getattr(y, x, None)
def _slot_of(x):              return lambda y: getattr(x, y, None)
def _slot_equal(slot, val):   return lambda y: getattr(y, slot, None) == val

def _updated_dict(to, from_):
        to.update(from_)
        return to

def _prefix_suffix_if(f, xs, key = identity):
        for i, x in enumerate(xs):
                if not f(key(x)):
                        return xs[:i], xs[i:]
        return xs, []

def _prefix_suffix_if_not(f, xs, key = identity):
        return _prefix_suffix_if(lambda x: not f(x), xs, key = key)

def _defwith(name, enter, exit, **initargs):
        initargs.update(dict(__enter__ = enter,
                                 __exit__  = exit))
        return type(name, (object,), initargs)

def _lookup(scope, name):
        if not scope: return nil, nil
        frame, rest = scope
        for cell_name, value in frame:
                if cell_name is name:
                        return value, t
        return _lookup(name, rest)

def _defscope(name, varname, **initargs):
        def make_frame(bindings):
                return tuple(bindings.items())
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
                return "#%s(%s)" % (type(self).__name__,
                                    ", ".join(_maphash(lambda k, v: "%s = %s" % (k, v),
                                                       self.__dict__)))
        def __init__(self, **keys):
                self.__dict__.update(keys)

def _gen(n = 1, x = "G", gen = gensym):
        if zerop(n):
                error("_GEN: we are very very much against this, please stop doing it!")
        return tuple(gen(x)
               for i in range(n))
def _gensyms(**initargs):     return _gen(gen = gensym,      **initargs)
def _gensymnames(**initargs): return _gen(gen = _gensymname, **initargs)

# Testing

#         Used by quasiquotation, MetaSEX and others.

_results_ = []
def _runtest(fn, input, expected, printer = str):
        result = fn(input)
        name = fn.__name__.upper().replace("_", "-")
        if result != expected:
                _debug_printf("; %30s:  FAILED\n;  input:\n%s\n;  expected:\n%s\n;  actual:\n%s",
                              name, printer(input), printer(expected), printer(result))
        _results_.append((fn, result))
        successp = result == expected
        if successp:
                print("; %30s:  ok" % name)
        return successp and result

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
        return (x is y) if not isinstance(x, int) else x == y

@defun
def equal(x, y):
        return x == y

def _seek(n, iterator):
        for i in range(n):
                next(iterator, nil)

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

def _some_fast(fn, xs):
        for x in xs:
                ret = fn(x)
                if ret: return ret or t
        return nil

def _some_fast_2(fn, xs, ys):
        for x, y in zip(xs, ys):
                ret = fn(x, y)
                if ret: return ret or t
        return nil

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
def aref(xs, *indices):
        r = xs
        for i in indices:
                r = r[i]
        return r

__allowed__ = frozenset([str, set, frozenset, tuple, list, bytes, bytearray])
def _maprestype(x):
        type = type_of(x)
        return type if type in __allowed__ else list

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
        acc = dict()
        for dict in dicts:
                acc.update(dict)
        return acc

def _dict_select_keys(dict_, *keys):
        acc = dict()
        for k in keys:
                if k in dict_:
                        acc[k] = dict_[k]
        return acc

def _maphash(f, dict) -> list:
        return [ f(k, v) for k, v in dict.items() ]

def _remap_hash_table(f, xs: dict) -> dict:
        return { k: f(k, v) for k, v in xs.items() }

def _map_into_hash_star(f, xs,
                        key_test = lambda k: k is not None,
                        value_test = lambda _: t) -> dict:
        acc = make_hash_table()
        for x in xs:
                k, v = f(*x)
                if key_test(k) and value_test(v):
                        acc[k] = v
        return acc

def _map_hash_table(f, hash_table, **keys) -> dict:
        return _map_into_hash_star(f, hash_table.items(), **keys)

def _symbol_known(symbol):
        return symbol.known
def _symbol_python_type(symbol, if_not_a_type = "error"):
        return (symbol.python_type                                   if hasattr(the(symbol_t, symbol), "python_type") else
                nil                                                                        if if_not_a_type == "continue" else
                error("In %%SYMBOL-TYPE %s: symbol does not designate a known type.", symbol) if if_not_a_type == "error" else
                error("In %%SYMBOL-TYPE: the :IF-NOT-A-TYPE keyword argument must be one of ('error, 'continue')."))
def _symbol_type_predicate(symbol):
        return symbol.type_predicate if hasattr(the(symbol_t, symbol), "type_predicate") else nil

# Complex arguments

def _extract_keywords(xs, keys_allowed = t):
        if len(xs) % 2:
                error("Odd number of arguments in keyword section: %s", xs)
        names = xs[0::2]
        if not all(isinstance(x, symbol_t) for x in names):
                error("Non-symbol(s) in keyword position(s): %s", [ x for x in names if not isinstance(x, symbol_t) ])
        bad_keys = ([] if keys_allowed is t else
                    [ x for x in names if x not in keys_allowed ])
        if bad_keys:
                error("Unexpected keywords - %s, where %s were expected.", bad_keys, keys_allowed)
        return dict(zip(names, xs[1::2]))

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
                                 lambda: error(simple_package_error_t, "The name %s does not designate any package.",
                                               name))))

def _lisp_symbol_python_addr(sym):
        symname, packname = _lisp_symbol_python_names(sym)
        return symname, _find_module(packname)

def _lisp_symbol_python_value(sym):
        name, module = _lisp_symbol_python_addr(sym)
        value, presentp = gethash(name, module.__dict__)
        return (value if presentp else
                error(simple_package_error_t, "This name is not accessible in the '%s' module: '%s'.",
                      module.__name__, name))

def _lisp_symbol_ast(sym, current_package):
        symname, packname = _lisp_symbol_python_names(sym)
        return (_ast_name(symname) if _symbol_accessible_in(sym, current_package) else
                _ast_index(_ast_attribute(_ast_index(_ast_attribute(_ast_name("sys"), "modules"), _ast_string(packname)),
                                          "__dict__"),
                           _ast_string(symname)))

# Functions

def _variable_kind(name):
        check_type(name, symbol_t)
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
        return t if (the(symbol_t, name).function or
                     name.macro_function) else nil

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
        (the(symbol_t, name).function,
         name.macro_function,
         name.compiler_macro_function) = nil, nil, nil
        return name

## @defun def function was moved lower, due to dependency on @defun and CL:T

@defun
def symbol_function(symbol):
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
        ## Unregistered Issue FDEFINITION-SYMBOL-FUNCTION-AND-COMPILER-GFUNS-NEED-SYNCHRONISATION
        return _symbol_function(the(symbol_t, symbol))

@defun
def macro_function(symbol, environment = nil):
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
        b_or_res = (the(_lexenv, environment).lookup_func_kind(macro, symbol, nil) if environment else
                    nil) or the(symbol_t, symbol).macro_function
        if b_or_res and _bindingp(b_or_res) and isinstance(b_or_res.value, tuple):
               lambda_list, body = b_or_res.value
               _not_implemented("compilation of lambda expression form of macro function")
               # b_or_res = the(function_t, _compile_in_lexenv(nil,
               #                                             (lambda_, lambda_list) + body,
               #                                             environment))
        return the((or_t, function_t, null_t),
                   (b_or_res.value if _bindingp(b_or_res) else b_or_res) or nil)

@defun
def compiler_macro_function(symbol, environment = nil, check_shadow = t):
        """compiler-macro-function symbol &optional environment => function"""
        shadow = check_shadow and environment and environment.funcscope_binds_p(symbol)
        return (the((or_t, function_t, null_t),
                     the(symbol_t, symbol).compiler_macro_function) if not shadow else
                nil)

@defun
def setf_compiler_macro_function(new_function, symbol, environment = nil):
        "<See documentation for COMPILER-MACRO-FUNCTION>"
        ## Ensure compliance.
        check_type(environment, null_t)
        symbol.compiler_macro_function = the(function_t, new_function)
        return new_function

def _symbol_macro_expander(sym, environment = None):
        ## -> (-> expansion) | None
        lexical = environment and the(_lexenv, environment).lookup_var_kind(symbol_macro, symbol)
        expansion = (the(symbol_t, sym).symbol_macro_expansion if not lexical else
                     lexical.value)
        return (lambda: expansion) if expansion is not None else None

def _style_warn(control, *args):
        warn(simple_style_warning_t, format_control = control, format_arguments = args)

def _warn_incompatible_redefinition(symbol, tons, fromns):
        _style_warn("%s is being redefined as a %s when it was previously defined to be a %s.", symbol, tons, fromns)

def _warn_possible_redefinition(x, type):
        if x:
                _style_warn("In %s: %s is being redefined.", type, x)

@defun
def setf_macro_function(new_function, symbol, environment = nil):
        "<See documentation for MACRO-FUNCTION>"
        ## Ensure compliance.
        check_type(environment, null_t)
        if symbol.function:
                _warn_incompatible_redefinition(symbol, "macro", "function")
                symbol.function = nil
        _warn_possible_redefinition(symbol.macro_function, defmacro)
        symbol.macro_function = the(function_t, new_function)
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
        the(symbol_t, function_name).function = new_definition
        _compiler_defun(function_name, nil, check_redefinition = nil)
        return new_definition

@defun
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
        return t if _find_known(symbol) else nil

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

_intern_and_bind_names_in_module("DEFMACRO")

# Namespace separation.

_compiler_safe_namespace_separation = t

def _ensure_function_pyname(symbol):
        if the(symbol_t, symbol).function_pyname is not None:
                return symbol.function_pyname
        symbol.function_pyname = (_gensymname("FUN_" + str(symbol) + "-") if _compiler_safe_namespace_separation else
                                  str(symbol))
        return symbol.function_pyname
def _ensure_symbol_pyname(symbol):
        if the(symbol_t, symbol).symbol_pyname is not None:
                return symbol.symbol_pyname
        symbol.symbol_pyname = (_gensymname("SYM_" + str(symbol) + "-") if _compiler_safe_namespace_separation else
                                str(symbol))
        return symbol.symbol_pyname

def _ensure_variable_pyname(x):
        return _frost.full_symbol_name_python_name(x)

def _get_function_pyname(symbol):
        if the(symbol_t, symbol).function_pyname is None:
                error("Function %s has no mapping to a python name.", symbol)
        return symbol.function_pyname
def _get_symbol_pyname(symbol):
        if the(symbol_t, symbol).symbol_pyname is None:
                error("Symbol %s has no mapping to a python name.", symbol)
        return symbol.symbol_pyname

def _set_function_definition(globals, x, lambda_expression = None, check_redefinition = nil):
        lambda_expression = _defaulted(lambda_expression, [lambda_, [nil, nil]])
        identity_redef = _compiler_defun(x, lambda_expression, check_redefinition = check_redefinition)
        def do_set_function_definition(function):
                if not identity_redef and function:
                        x.function, x.macro_function = function, nil
                        _frost.make_object_like_python_function(x, function)
                        globals["__" + function.__name__.rstrip("_")] = function.name = x
                        globals[_ensure_function_pyname(x)] = function
                return x
        return do_set_function_definition

def _set_macro_definition(globals, x, lambda_expression):
        identity_redef = _compiler_defmacro(x, lambda_expression)
        def do_set_macro_definition(function):
                if not identity_redef and function:
                        x.function, x.macro_function = nil, function
                        _frost.make_object_like_python_function(x, function)
                        function.name = x
                        globals[_ensure_function_pyname(x)] = function
                return x
        return do_set_macro_definition

# Essential system-level functions

def _getenv(var):
        return _without_condition_system(lambda: _os.getenv(var))

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

_load_toplevel, _compile_toplevel, _execute = [ _keyword(x) for x in [ "LOAD-TOPLEVEL",
                                                                       "COMPILE-TOPLEVEL",
                                                                       "EXECUTE" ] ]

if not _getenv("CL_NO_CONDITION_SYSTEM"):
        _init_condition_system()

# Rudimentary character type

@defclass
class base_char_t(): pass

# Early-earlified streaming

@defun
def streamp(x):                     return isinstance(x, stream_t)

def _file_stream_p(x):              return isinstance(x, (__io._TextIOBase, __io._BufferedIOBase))

@defun
def with_open_stream(stream, fn):
        try:
                return fn(stream)
        finally:
                close(stream)

@defun
def open(pathname, direction = _keyword("INPUT"), element_type = base_char_t,
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
def with_open_file(pathname, body, direction = _keyword("INPUT"), element_type = base_char_t,
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
        f = namestring(pathname(pathspec))
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
        return int(_os.path.getmtime(f))

def _file_stream_name(x):
        return _values_frame_project(0, parse_namestring(x.name))

# Stream types and functions

def open_stream_p(x):
        return not the(stream_t, x).closed

def input_stream_p(x):
        return open_stream_p(x) and x.readable()

def output_stream_p(x):
        return open_stream_p(x) and x.writable()

@defclass
class two_way_stream_t(stream_t):
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

def make_two_way_stream(input, output):   return two_way_stream_t(input, output)
def two_way_stream_input_stream(stream):  return stream.input
def two_way_stream_output_stream(stream): return stream.output

_string_set("*DEBUG-IO*", make_two_way_stream(_symbol_value(_standard_input_), _symbol_value(_standard_output_)))
_string_set("*QUERY-IO*", make_two_way_stream(_symbol_value(_standard_input_), _symbol_value(_standard_output_)))
# raise simple_condition("Boo %s.", 2)

@defclass
class broadcast_stream_t(stream_t):
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

def make_broadcast_stream(*streams):  return broadcast_stream_t(*streams)
def broadcast_stream_streams(stream): return stream.streams

@defclass
class synonym_stream_t(stream_t):
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

def make_synonym_stream(symbol):   return synonym_stream_t(symbol)
def synonym_stream_symbol(stream): return stream.symbol

def _coerce_to_stream(x):
        return (x                                if streamp(x) else
                _symbol_value(_standard_output_) if x is t else
                error("%s cannot be coerced to a stream.", x))

# Stream output functions and FORMAT

def write_char(c, stream = t):
        write_string(c, stream)
        return c

@defun
def terpri(stream = t):
        write_string("\n", stream)

def write_string(string, stream = t):
        if stream is not nil:
                def handler():
                        try:
                                return _write_string(string, _coerce_to_stream(stream))
                        except _io.UnsupportedOperation as cond:
                                error(stream_type_error_t, "%s is not an %s stream: \"%s\".",
                                      stream, ("output" if cond.args[0] == "not writable" else
                                               "adequate"),
                                      cond.args[0])
                _without_condition_system(handler,
                                          reason = "_write_string")
        return string

def write_line(string, stream = t):
        return write_string(string + "\n", stream)

def finish_output(stream = t):
        check_type(stream, (or_t, stream_t, (member_t, t, nil)))
        (stream is not nil) and _coerce_to_stream(stream).flush()

def force_output(*args, **keys):
        finish_output(*args, **keys)

@defun
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
        posn = basename.rfind(".")
        return _if_let(posn if posn >= 0 else nil,
                       lambda dotpos: (dirname or nil, basename[:dotpos], basename[dotpos + 1:]),
                       lambda:        (dirname or nil, basename or nil,   nil))

@defclass
class pathname_host_t():
        def parse(self, x):
                ## Unregistered Issue COMPLIANCE-NAMESTRING-UNPARSING-NOT-REALLY-IMPLEMENTED
                dirname, basename, type = _namestring_components(the(string_t, x))
                directory = dirname.split(_os.sep) if dirname else nil
                return pathname_t(host      = _system_pathname_host,
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
class unix_host_t(pathname_host_t):
        localise_case              = identity
        customiser, anticustomiser = str.lower, str.upper

@defclass
class windows_host_t(pathname_host_t):
        localise_case              = identity
        customiser, anticustomiser = str.lower, str.upper

_system_pathname_host = make_instance(windows_host_t if _platform.system() == 'Windows' else
                                      unix_host_t)

_intern_and_bind_names_in_module("*DEFAULT-PATHNAME-DEFAULTS*")

@defclass
class pathname_t():
        def __init__(self, *args, host, device, directory, name, type, version):
                assert not args
                (self.host, self.device, self.directory, self.name, self.type, self.version) = host, device, directory, name, type, version
        def __str__(self):
                return "#P\"%s\"" % repr(namestring(self))[1:-1]
        def __repr__(self):
                return self.__str__()

@defun
def pathnamep(x): return isinstance(x, pathname_t)

## Unregistered Issue COMPLIANCE-HOST-TYPE-WRONG
@defun
def make_pathname(*args, host = None, device = None, directory = None, name = None, type = None, version = None,
                  default = None, case = _keyword("LOCAL")):
        assert not args
        default = default or pathname_t(**_defaulted_keys(
                        host = pathname_host(_symbol_value(_default_pathname_defaults_)),
                        device = nil, directory = nil, name = nil, type = nil, version = nil))
        effective_host = _defaulted(host, default.host)
        supplied_pathname = dict(
                (k, effective_host.apply_case(case, v) if isinstance(v, str) else v)
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
        check_type(thing, string_t)
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
        return (x                                             if pathnamep(x)      else
                _values_frame_project(0, parse_namestring(x)) if isinstance(x, str)        else
                _file_stream_name(x)                          if _file_stream_p(x) else
                error("PATHNAME only accepts pathnames, namestrings and file streams, was given: %s.", x))

@defun
def pathname_directory(x):
        # Unregistered Issue PORTABILITY-PATHNAME
        absp = the(string_t, x).startswith(_os.sep)
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
                posn = default_pathname.rfind(_os.sep)
                return _os.path.join((default_pathname[:posn + 1] if dir_defaulted_p else ""),
                                    pathname)
        elif not name_supplied_p:
                pass
        return _os.path.join(x, y)

# Cons <-> vector xform

def _vectorise(x):
        res = []
        while x:
                res.append(x[0] if not consp(x[0]) else
                           _vectorise(x[0]))
                x = x[1]
        return res

def _vectorise_linear(x):
        res = []
        while x:
                res.append(x[0])
                x = x[1]
        return res

def _consify(xs):
        return _consify_linear(( (_consify(x) if isinstance(x, (list, tuple)) else x)
                                 for x in xs )) if isinstance(xs, (list, tuple)) else xs

def _consify_star(*xs):
        return _consify(xs)

def _consify_linear(xs, last_cdr = nil):
        if consp(xs):
                error("Asked to consify a CONS:\n%s", xs)
        return reduce(lambda acc, x: [x, acc],
                      reversed(tuple(xs)),
                      last_cdr)

def _pp_consly(x, dispatch = dict()):
        if consp(x):
                acc = []
                ptr = x
                while ptr:
                        acc.append(_pp_consly(ptr[0], dispatch = dispatch))
                        ptr = ptr[1]
                return "(" + " ".join(acc) + ")"
        return (repr              if isinstance(x, str)  else
                dispatch[type(x)] if type(x) in dispatch else str)(x)

def _xmap_to_vector(f, *xss):
        not xss and error("Invalid number of arguments: %d", 1)
        if len(xss) is 1:
                acc, xs = [], xss[0]
                while xs:
                        acc.append(f(xs[0]))
                        xs = xs[1]
                return acc
        else:
                not_implemented("%XMAP-TO-VECTOR: multiple-list case")

def _xmap_to_conses(f, *xss):
        not xss and error("Invalid number of arguments: %d", 1)
        if len(xss) is 1:
                xs, lim = xss[0], len(xss[0])
                if not xs:
                        return nil
                acc = ptr = [f(xs[0]), nil]
                i = 1
                while i < lim:
                        ptr[1] = ptr = [f(xs[i]), nil]
                        i += 1
                return acc
        else:
                not_implemented("%XMAP-TO-CONSES: multiple-list case")

# Sequences
def _compute_predicate(key, elt, test = eql, test_not = nil):
        if test and test_not:
                error("Incomprehensible simultaneous specification of :TEST and :TEST-NOT.")
        test = test or test_not
        return ((lambda x: test(elt, x))
                if key is identity else
                (lambda x: test(elt, key(x))))

## Uncomment in emergency:
#
# __key_map__ = { "key":      _key_,
#                 "start":    _start,
#                 "end":      _end,
#                 "from_end": _from_end,
#                 "test":     _test,
#                 "test_not": _test_not,
#                 }
# def _read_keys(keys):
#         return { __key_map__[k]: v for k, v in keys }

# COPY-SEQ
# ELT
# FILL
# MAKE-SEQUENCE

@defun
def subseq(xs, start, end = nil):
        def error_bad_indices(start, end, actual):
                error("The bounding indices %d and %s are bad for a sequence of length %d",
                      start, end, actual)
        if not (isinstance(start, int) and start > -1):
                error("Invalid sequence index: %s", start)
        if listp(xs):
                i = 0
                if end is not nil:
                        while i != start:
                                if xs is nil:
                                        error_bad_indices(start, end, i)
                                xs = xs[1]
                                i += 1
                        if end is start:
                                return nil
                        if xs is nil:
                                error_bad_indices(start, end, i)
                        ret = ptr = [xs[0], nil]
                        while True:
                                i += 1
                                if i == end:
                                        return ret
                                xs = xs[1]
                                if xs is nil:
                                        error_bad_indices(start, end, i)
                                ptr[1] = [xs[0], nil]
                                ptr = ptr[1]
                else:
                        ptr = xs
                        while i != start:
                                if ptr is nil:
                                        error_bad_indices(start, end, i)
                                ptr = ptr[1]
                                i += 1
                        return ptr
        else:
                _not_implemented("Non-cons case of SUBSEQ.")

_key_, _start, _end, _from_end, _test, _test_not = [ _keyword(x) for x in
                                                     ["KEY", "START", "END", "FROM-END", "TEST", "TEST-NOT" ] ]

# MAP
# MAP-INTO
# REDUCE

@defun
def count(elt, xs, *rest):
        keys = _extract_keywords(rest, [_key_, _start, _end, _from_end, _test, _test_not])
        key, start, end, from_end, test, test_not = [ keys.get(k, df) for k, df
                                                      in [ (_key_,     identity),
                                                           (_start,    0),
                                                           (_end,      nil),
                                                           (_from_end, nil),
                                                           (_test,     nil),
                                                           (_test_not, nil) ] ]
        _not_implemented()

@defun
def count_if(p, xs, *rest):
        keys = _extract_keywords(rest, [_key_, _start, _end, _from_end])
        key, start, end, from_end = [ keys.get(k, df) for k, df
                                      in [ (_key_,     identity),
                                           (_start,    0),
                                           (_end,      nil),
                                           (_from_end, nil) ] ]
        _not_implemented()

@defun
def count_if_not(p, xs, *rest):
        keys = _extract_keywords(rest, [_key_, _start, _end, _from_end])
        key, start, end, from_end = [ keys.get(k, df) for k, df
                                      in [ (_key_,     identity),
                                           (_start,    0),
                                           (_end,      nil),
                                           (_from_end, nil) ] ]
        _not_implemented()

@defun
def length(x):
        if listp(x):
                len = 0
                while isinstance(x, list):
                        len += 1
                        x = x[1]
                return len
        else:
                _not_implemented("LENGTH: non-list case")

# REVERSE

@defun
def nreverse(xs):
        if xs is nil:
                return xs
        oldl, oldcdr, newcdr = xs, xs[1], nil
        while oldcdr is not nil:
                oldl[1] = newcdr
                newcdr = oldl
                oldl = oldcdr
                oldcdr = oldcdr[1]
        oldl[1] = newcdr
        return oldl

# SORT
# STABLE-SORT

def _find_if(pred, xs, keys):
        key, start, end, from_end = [ keys.get(k, df) for k, df
                                      in [ (_key_,     identity),
                                           (_start,    0),
                                           (_end,      nil),
                                           (_from_end, nil) ] ]
        endp = end is not nil
        if from_end:
                _not_implemented(":FROM-END")
        the_test = pred if not key else lambda x: pred(key(x))
        if listp(xs):
                i, ptr = start, nthcdr(start, xs) if start else xs
                while ptr:
                        if endp and i >= end: ## Hoist?
                                return nil
                        if the_test(ptr[0]):
                                return ptr[0]
                        i, ptr = i + 1, ptr[1]
                return nil
        else:
                _not_implemented("FIND-IF: non-list case")

@defun
def find(elt, xs, *rest):
        keys = _extract_keywords(rest, [_key_, _start, _end, _from_end, _test, _test_not])
        key, test, test_not = [ keys.get(k, df) for k, df
                                in [ (_key_,     identity),
                                     (_test,     eql),
                                     (_test_not, nil) ] ]
        if _key_     in keys: del keys[_key_]
        if _test     in keys: del keys[_test]
        if _test_not in keys: del keys[_test_not]
        return _find_if(_compute_predicate(key, elt, test = test, test_not = test_not),
                        xs, keys)

@defun
def find_if(p, xs, *rest):
        keys = _extract_keywords(rest, [_key_, _start, _end, _from_end])
        return _find_if(p, xs, keys)


@defun
def find_if_not(p, xs, *rest):
        keys = _extract_keywords(rest, [_key_, _start, _end, _from_end])
        return _find_if(lambda x: not p(x), xs, keys)

def _position_if(pred, xs, keys):
        key, start, end, from_end = [ keys.get(k, df) for k, df
                                      in [ (_key_,     identity),
                                           (_start,    0),
                                           (_end,      nil),
                                           (_from_end, nil) ] ]
        endp = end is not nil
        if from_end:
                _not_implemented(":FROM-END")
        the_test = pred if not key else lambda x: pred(key(x))
        if listp(xs):
                i, ptr = start, nthcdr(start, xs) if start else xs
                while ptr:
                        if endp and i >= end: ## Hoist?
                                return nil
                        if the_test(ptr[0]):
                                return i
                        i, ptr = i + 1, ptr[1]
                return nil
        else:
                _not_implemented("POSITION-IF: non-list case")

@defun
def position(elt, xs, *rest):
        keys = _extract_keywords(rest, [_key_, _start, _end, _from_end, _test, _test_not])
        key, test, test_not = [ keys.get(k, df) for k, df
                                in [ (_key_,     identity),
                                     (_test,     eql),
                                     (_test_not, nil) ] ]
        if _key_     in keys: del keys[_key_]
        if _test     in keys: del keys[_test]
        if _test_not in keys: del keys[_test_not]
        return _position_if(_compute_predicate(key, elt, test = test, test_not = test_not),
                            xs, keys)

@defun
def position_if(p, xs, *rest):
        keys = _extract_keywords(rest, [_key_, _start, _end, _from_end])
        return _position_if(p, xs, keys)

@defun
def position_if_not(p, xs, *rest):
        keys = _extract_keywords(rest, [_key_, _start, _end, _from_end])
        return _position_if(lambda x: not p(x), xs, keys)

# SEARCH
# MISMATCH

@defun
def replace(sequence_1, sequence_2, *rest):
        """Destructively modifies sequence-1 by replacing the elements
of subsequence-1 bounded by start1 and end1 with the elements of
subsequence-2 bounded by start2 and end2. """
        keys = _extract_keywords(rest, [_start1, _start2, _end1, _end2])
        start1, start2, end1, end2 = [ keys.get(k, df) for k, df
                                       in [ (_start1,    nil),
                                            (_start2,    nil),
                                            (_end2,      nil),
                                            (_end2,      nil) ] ]
        _not_implemented()

# SUBSTITUTE
# SUBSTITUTE-IF
# SUBSTITUTE-IF-NOT
# NSUBSTITUTE
# NSUBSTITUTE-IF
# NSUBSTITUTE-IF-NOT
# CONCATENATE
# MERGE

@defun
def remove(elt, xs, *rest):
        keys = _extract_keywords(rest, [_key_, _start, _end, _from_end, _test, _test_not, _count])
        key, start, end, from_end, test, test_not, count = [ keys.get(k, df) for k, df
                                                             in [ (_key_,     identity),
                                                                  (_start,    0),
                                                                  (_end,      nil),
                                                                  (_from_end, nil),
                                                                  (_test,     nil),
                                                                  (_test_not, nil),
                                                                  (_count,    nil) ] ]
        _not_implemented()

@defun
def remove_if(f, xs, *rest):
        keys = _extract_keywords(rest, [_key_, _start, _end, _from_end, _count])
        key, start, end, from_end, count = [ keys.get(k, df) for k, df
                                             in [ (_key_,     identity),
                                                  (_start,    0),
                                                  (_end,      nil),
                                                  (_from_end, nil),
                                                  (_count,    nil) ] ]
        _not_implemented()

@defun
def remove_if_not(f, xs, *rest):
        keys = _extract_keywords(rest, [_key_, _start, _end, _from_end, _count])
        key, start, end, from_end, count = [ keys.get(k, df) for k, df
                                             in [ (_key_,     identity),
                                                  (_start,    0),
                                                  (_end,      nil),
                                                  (_from_end, nil),
                                                  (_count,    nil) ] ]
        _not_implemented()

# REMOVE-DUPLICATES
# DELETE-DUPLICATES

# Conses

_initial_element = _keyword("INITIAL-ELEMENT")

@defun
def cons(x, y):     return [x, y]

@defun
def consp(x):       return isinstance(x, list) and len(x) is 2
def _consp_fast(x): return isinstance(x, list)

@defun
def atom(x):        return not isinstance(x, list) or len(x) != 2

# RPLACA
# RPLACD

@defun
def car(x):         return x[0] if x else nil

@defun
def cdr(x):         return x[1] if x else nil

# CAAR
# CADR
# CDAR
# CDDR
# CAAAR
# CAADR
# CADAR
# CADDR
# CDAAR
# CDADR
# CDDAR
# CDDDR
# CAAAAR
# CAAADR
# CAADAR
# CAADDR
# CADAAR
# CADADR
# CADDAR
# CADDDR
# CDAAAR
# CDAADR
# CDADAR
# CDADDR
# CDDAAR
# CDDADR
# CDDDAR
# CDDDDR
# COPY-TREE
# SUBLIS
# NSUBLIS
# SUBST
# SUBST-IF
# SUBST-IF-NOT
# NSUBST
# NSUBST-IF
# NSUBST-IF-NOT
# TREE-EQUAL

def _copy_list_with_lastcdr(x, cdr):
        if not x:
                return cdr
        ret = ptr = [x[0], cdr]
        while True:
                x = x[1]
                if not x:
                        return ret
                ptr[1] = ptr = [x[0], cdr]

@defun
def copy_list(xs):
        return _copy_list_with_lastcdr(xs, nil)

@defun("LIST")
def list_(*xs):     return _consify_linear(xs)

@defun("LIST*")
def list__(*xs):
        return _consify_linear(_itertools.islice(xs, 0, len(xs) - 1), last_cdr = xs[-1])
# LIST-LENGTH

@defun
def listp(x):       return x is nil or isinstance(x, list) and len(x) is 2

@defun("MAKE-LIST")
def make_list(length, *rest):
        elt = _extract_keywords(rest, [_initial_element]).get(_initial_element, nil)
        acc = nil
        for i in range(length):
                acc = [elt, acc]
        return acc

# PUSH
# POP
# FIRST

@defun
def second(x):
        return nil if not x or not x[1] else x[1][0]

# THIRD
# FOURTH
# FIFTH
# SIXTH
# SEVENTH
# EIGHTH
# NINTH
# TENTH
# NTH
# ENDP
# NULL

@defun
def nconc(*xs):
        head = nil
        for x in xs:
                if not x:
                        continue
                if not head:
                        head = ptr = x
                        continue
                last(ptr)[1] = x
                ptr = x
        return head

@defun
def append(*xs):
        if not xs:
                return nil
        return _copy_list_with_lastcdr(xs[0], append(*xs[1:]))

# REVAPPEND
# NRECONC
# BUTLAST
# NBUTLAST

@defun
def last(x):
        if not x:
               return nil
        while True:
                lastx = x
                x = x[1]
                if not x:
                        return lastx

@defun
def ldiff(object, list_):
        """If OBJECT is the same as some tail of LIST, LDIFF returns a
fresh list of the elements of LIST that precede OBJECT in the
list structure of LIST; otherwise, it returns a copy[2] of
LIST."""
        _not_implemented()

@defun
def tailp(object, list):
        """If OBJECT is the same as some tail of LIST, TAILP returns
true; otherwise, it returns false."""
        _not_implemented()

@defun
def nthcdr(n, xs):
        while n and xs:
                n, xs = n - 1, xs[1]
        return xs

# REST

@defun
def member(x, xs):
        keys = _extract_keywords(rest, [_key_, _test, _test_not])
        key, test, test_not = [ keys.get(k, df) for k, df
                                in [ (_key_,     identity),
                                     (_test,     nil),
                                     (_test_not, nil) ] ]
        _not_implemented()

@defun
def member_if(test, xs):
        key = _extract_keywords(rest, [_key_]).get(_key_, identity)
        _not_implemented()

@defun
def member_if_not(test, xs):
        key = _extract_keywords(rest, [_key_]).get(_key_, identity)
        _not_implemented()

@defun
def mapc(f, *xs):
         _not_implemented()

@defun
def mapcar(f, *xss):
        if len(xss) == 1:
                xs = xss[0]
                if not xs:
                        return nil
                car, cdr = xs[0], xs[1]
                acc = ptr = [f(car), nil]
                while cdr:
                        car, cdr = cdr[0], cdr[1]
                        ptr[1] = ptr = [f(car), nil]
                return acc
        else:
                _not_implemented("MAPCAR: multiple-list case")

@defun
def mapcan(f, *xs):
         _not_implemented()

# MAPL
# MAPLIST

@defun
def mapcon(f, *xss):
        if len(xss) == 1:
                xs = xss[0]
                acc = nil
                while xs and not acc:
                        acc = f(xs)
                        xs = xs[1]
                ptr = acc
                while xs:
                        res = f(xs)
                        if res:
                                last(ptr)[1] = res
                                ptr = res
                        xs = xs[1]
                return acc
        else:
                _not_implemented("MAPCON: multiple-list case")

# ACONS

@defun
def assoc(x, xs, *rest):
         _not_implemented()

# ASSOC-IF
# ASSOC-IF-NOT
# COPY-ALIST
# PAIRLIS
# RASSOC
# RASSOC-IF
# RASSOC-IF-NOT
# GET-PROPERTIES

@defun
def getf(xs, key, default = nil):
         _not_implemented()

@defun
def setf_getf(value, xs, key):
         _not_implemented()

# REMF
# INTERSECTION
# NINTERSECTION
# ADJOIN
# PUSHNEW
# SET-DIFFERENCE
# NSET-DIFFERENCE
# SET-EXCLUSIVE-OR
# NSET-EXCLUSIVE-OR
# SUBSETP
# UNION
# NUNION

# Data function part of 5.3

# Function APPLY

# Accessor FDEFINITION

# Function FBOUNDP

# Function FMAKUNBOUND

# Function FUNCALL

# Function FUNCTION-LAMBDA-EXPRESSION

# Function FUNCTIONP

# Function COMPILED-FUNCTION-P

# Function NOT

# Function EQ

# Function EQL

# Function EQUAL

# Function EQUALP

# Function IDENTITY

# Function COMPLEMENT

# Function CONSTANTLY

@defun
def every(fn, xs, *xss):
        if len(xss) == 1:
                while xs:
                        if not fn(xs[0]):
                                 return nil
                        xs = xs[1]
                return t
        else:
                not_implemented("EVERY: multiple-list case")

@defun
def some(fn, xs, *xss):
        if len(xss) == 1:
                while xs:
                        if fn(xs[0]):
                                 return t
                        xs = xs[1]
                return nil
        else:
                not_implemented("SOME: multiple-list case")

@defun
def notevery(fn, xs, *xss):
        if len(xss) == 1:
                while xs:
                        if not fn(xs[0]):
                                 return t
                        xs = xs[1]
                return nil
        else:
                not_implemented("NOTEVERY: multiple-list case")

@defun
def notany(fn, xs, *xss):
        if len(xss) == 1:
                while xs:
                        if fn(xs[0]):
                                 return nil
                        xs = xs[1]
                return t
        else:
                not_implemented("NOTANY: multiple-list case")

# Function VALUES-LIST

# Function GET-SETF-EXPANSION

# Arrays

@defun
def vector(*xs):    return [len(xs), t] + list(xs)

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
                        if _listp(object):
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
                        elif isinstance(object, int) or floatp(object):
                                string += str(object)
                        elif object is False or object is None or object is True:
                                string += obj2lisp_xform[object]
                        elif type(object).__name__ == "builtin_function_or_method":
                                string += "\"#<BUILTIN-FUNCTION-OR-METHOD %s 0x%x>\"" % (object.__name__, id(object))
                        elif isinstance(object, str):
                                # Honors *PRINT-ESCAPE* and *PRINT-READABLY*.
                                string += _print_string(object)
                        elif hash_table_p(object) or _setp(object):
                                # Honors *PRINT-ESCAPE* and *PRINT-READABLY*.
                                string += _print_unreadable_compound(object)
                        elif functionp(object):
                                string += _print_function(object)
                        elif (not escape) and isinstance(object, (restart_t, condition_t)):
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

# Quasiquotation

## From CLHS 2.4.6:
##
## The backquote syntax can be summarized formally as follows.
##
## * `basic is the same as 'basic, that is, (quote basic), for any expression basic that is not a list or a general vector.
##
## * `,form is the same as form, for any form, provided that the representation of form does not begin with at-sign or dot.
## (A similar caveat holds for all occurrences of a form after a comma.)
##
## * `,@form has undefined consequences.
##
## * `(x1 x2 x3 ... xn . atom) may be interpreted to mean
##
## (append [ x1] [ x2] [ x3] ... [ xn] (quote atom))
##
##         where the brackets are used to indicate a transformation of an xj as follows:
##
##              -- [form] is interpreted as (list `form), which contains a backquoted form that must then be further
##              interpreted.
##
##              -- [,form] is interpreted as (list form).
##
##              -- [,@form] is interpreted as form.
##
## * `(x1 x2 x3 ... xn) may be interpreted to mean the same as the backquoted form `(x1 x2 x3 ... xn . nil), thereby
## reducing it to the previous case.
##
## * `(x1 x2 x3 ... xn . ,form) may be interpreted to mean
##
## (append [ x1] [ x2] [ x3] ... [ xn] form)
##
##         where the brackets indicate a transformation of an xj as described above.
##
## * `(x1 x2 x3 ... xn . ,@form) has undefined consequences.
##
## * `#(x1 x2 x3 ... xn) may be interpreted to mean (apply #'vector `(x1 x2 x3 ... xn)).
##
## Anywhere ``,@'' may be used, the syntax ``,.'' may be used instead to indicate that it is permissible to operate
## destructively on the list structure produced by the form following the ``,.'' (in effect, to use nconc instead of
## append).
##
## If the backquote syntax is nested, the innermost backquoted form should be expanded first. This means that if several
## commas occur in a row, the leftmost one belongs to the innermost backquote.

__list, __append = intern("LIST")[0], intern("APPEND")[0]
_intern_and_bind_names_in_module("QUOTE", "QUASIQUOTE", "COMMA", "SPLICE")

_string_set("*READER-TRACE-QQEXPANSION*",        nil)

## Unregistered Issue COMPLIANCE-BACKQUOTE-EXPANSION-DOTTED-LIST-HANDLING

def _expand_quasiquotation(form):
        """Expand quasiquotation abbreviations in FORM (in a simple, yet suboptimal way)."""
        def malform(x): error("Invalid %s form: %s.", x[0], x)
        def process_form(x):
                return (x                                         if atom(x)            else
                        (process_qq(x[1][0]) if length(x) is 2 else
                         malform(x))                              if x[0] is quasiquote else
                        mapcar(process_form, x))
        def process_qq(x):
                if atom(x):
                        return list_(quote, x)
                else:
                        acc = [__append]
                        ptr = x
                        while ptr:
                                xi = ptr[0]
                                if atom(xi):
                                        acc.append(list_(__list, list_(quote, xi)))
                                elif xi[0] in (comma, splice):
                                        length(xi) is 2 or malform(xi)
                                        acc.append(list_(__list, xi[1][0]) if xi[0] is comma else
                                                   xi[1][0]) # second
                                else:
                                        if xi[0] is quasiquote:
                                                length(xi) is 2 or malform(xi)
                                                nxi = process_qq(xi[1][0])
                                                xi = nxi
                                        acc.append(list_(__list, process_qq(xi)))
                                ptr = ptr[1]
                        ## Simplify an obvious case of APPEND having only LIST subforms.
                        if all((consp(x) and x[0] is __list)
                               for x in acc[1:]):
                                new = (__list,) + tuple(x[1][0] for x in acc[1:])
                                acc = new
                        return _consify_linear(acc)
        result = process_form(form)
        if symbol_value(_reader_trace_qqexpansion_):
                if form != result:
                        _debug_printf(";;;%s quasiquotation expanded to:\n%s%s",
                                      _sex_space(-3, ";"), _sex_space(), _pp(result))
                else:
                        _debug_printf(";;;%s quasiquotation had no effect", _sex_space(-3, ";"))
        return result

def _run_tests_quasiquotation():
        def quasiquotation_simple(x): return _expand_quasiquotation(x)
        def quasiquotation_nested(x): return _expand_quasiquotation(x)
        assert(_runtest(quasiquotation_simple,
                        ## `(1 ,2 3 ,@4 5 (,6 ,@7) ,@8 ,@9)
                        list_(quasiquote, list_(1, list_(comma, 2), 3, list_(splice, 4), 5,
                                          list_(list_(comma, 6), list_(splice, 7)),
                                          list_(splice, 8), list_(splice, 9))),
                        ## (append (list (quote 1)) (list 2) (list (quote 3)) 4 (list (quote 5))
                        ##         (list (append (list 6) 7)) 8 9)
                        list_(__append, list_(__list, list_(quote, 1)), list_(__list, 2),
                              list_(__list, list_(quote, 3)), 4,
                              list_(__list, list_(quote, 5)), list_(__list, list_(__append, list_(__list, 6), 7)),
                              8, 9),
                        printer = _pp_consly))
        
        assert(_runtest(quasiquotation_nested,
                        ## `(a ,b ,@c `(d ,,e ,@f ,@,g)) -- numbers don't do, as CONSTANTP is used for simplification.
                        list_(quasiquote,
                              list_(1, list_(comma, 2), list_(splice, 3),
                                    list_(quasiquote, list_(4, list_(comma, list_(comma, 5)),
                                          list_(splice, 6), list_(splice, list_(comma, 7)))))),
                        list_(__append,
                              list_(__list, list_(quote, 1)), list_(__list, 2), 3,
                              ## The first pass ought to be:
                              ## (__append, (__list, (quote, 4)), (__list, (comma, 5)), 6, (comma, 7))
                              list_(__list, list_(__list,
                                                  list_(quote, __append),
                                                  list_(__list, list_(quote, __list), list_(__list, list_(quote, quote), list_(quote, 4))),
                                                  list_(__list, list_(quote, __list), 5),
                                                  list_(quote, 6),
                                                  7))),
                        printer = _pp_consly))

if _getenv("CL_RUN_TESTS"):
        _run_tests_quasiquotation()

# Cold reader

_string_set("*READ-CASE*", _keyword("upcase"))

def parse_integer(xs, junk_allowed = nil, radix = 10):
        l = len(xs)
        def hexcharp(x): return x.isdigit() or x in ["a", "b", "c", "d", "e", "f"]
        (test, xform) = ((_str.isdigit, identity)      if radix == 10 else
                         (hexcharp,    float.fromhex) if radix == 16 else
                         _not_implemented("PARSE-INTEGER only implemented for radices 10 and 16."))
        for end in range(0, l):
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

def read_line(stream = None, eof_error_p = t, eof_value = nil):
        stream = _defaulted_to_var(stream, _standard_input_)
        return handler_case(lambda: stream.readline(),
                            (error_t,
                             lambda c: error(end_of_file_t, "end of file on %s" % (stream,))))

def read_char(stream = None, eof_error_p = t, eof_value = nil, recursivep = nil):
        stream = _defaulted_to_var(stream, _standard_input_)
        ret = the(_global("stream_t"), stream).read(1)
        return (ret       if ret             else
                eof_value if not eof_error_p else
                error(end_of_file_t, "end of file on %s" % (stream,)))

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
                     lambda c: c == peek_type   if isinstance(peek_type, str) and len(peek_type) == 1 else
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
                # char = read_char_maybe_eof()
                # if char is nil:
                #         if not eof_error_p:
                #                 __return_from(_cold_read, eof_value)
                #         else:
                #                 read_char(stream)
                unread_char(char, stream)
                # _here("> \"%s\", by \"%s\"" % (string[pos:], char))
                if   char == chr(40):  obj = read_list() # Org is a bit too picky
                elif char == "\"":     obj = read_string()
                elif char == "'":
                        read_char(stream)
                        obj = list_(quote, read_inner())
                elif char == "`":
                        read_char(stream)
                        obj = list_(quasiquote, read_list())
                elif char == ",":
                        ## This is a simplified take, but it'll do for bootstrapping purposes.
                        read_char(stream);
                        char = read_char(stream)
                        if char == "@":
                                obj = list_(splice, read_inner())
                        else:
                                unread_char(char, stream)
                                obj = list_(comma, read_inner())
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
                        elif c not in frozenset([" ", "\t", "\n"]):
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
                                ret.append(obj)
                # _here("< %s" % (ret,))
                return _consify_linear(tuple(ret)) ## Beacon DEBUG-RELATED-SLOWDOWN
        def read_string():
                ret = ""
                read_char(stream) # seek the opening double-quote
                while t:
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
                if _without_condition_system(lambda: _re.match("^[0-9]+$", token),
                                             reason = "re.match"):
                        ret = int(token)
                elif _without_condition_system(lambda: _re.match("^[0-9]+\\.[0-9]+$", token),
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
                while t:
                        char = read_char_maybe_eof()
                        if char in set([nil, " ", "\t", "\n", "(", ")", "\"", "'"]):
                                if char is not nil:
                                        unread_char(char, stream)
                                break
                        else:
                                token += char
                # _here("< %s" % token)
                return token
        ret = handler_case(read_inner,
                           (end_of_file_t,
                            lambda c: error(c) if eof_error_p else
                                      __return_from(_cold_read, eof_value)))
        # _here("lastly %s" % (ret,))
        return _expand_quasiquotation(ret)
read = _cold_read

def _cold_read_from_string(string, eof_error_p = t, eof_value = nil,
                           start = 0, end = None, preserve_whitespace = None):
        stream = io.StringIO(string)
        try:
                return _cold_read(stream, eof_error_p = eof_error_p, eof_value = eof_value,
                                  start = start, end = end, preserve_whitespace = preserve_whitespace)
        finally:
                close(stream)

read_from_string = _cold_read_from_string

# Condition system

def _conditionp(x):
        return isinstance(x, condition_t)

def make_condition(type, *args, **keys):
        check_type(type, symbol_t)
        if not (hasattr(type, "python_type") and
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
                                          stream = _symbol_value(_debug_io_),
                                          backtrace = backtrace)
                        if old_hook_value:
                                old_hook_value(cond, old_hook_value)
                with env.maybe_let(p, **{hook if isinstance(hook, str) else symbol_name(hook): wrapped_hook}):
                        return body()
        else:
                return body()

__not_even_conditions__ = frozenset([GeneratorExit, SystemExit, __catcher_throw__])
"A set of condition types which are entirely ignored by the condition system."

_intern_and_bind_names_in_module("*STACK-TOP-HINT*", "*TRACEBACK*", "*SIGNALLING-FRAME*")

def __cl_condition_handler__(condspec, frame):
        backtrace_printed = nil
        def continuation():
                nonlocal backtrace_printed
                type, raw_cond, traceback = condspec
                # _print_frames(_frames_calling(frame))
                def _maybe_upgrade_condition(cond):
                        "Fix up the shit routinely being passed around."
                        return ((cond, nil) if isinstance(cond, condition_t) else
                                (condspec[0](*([cond] if not sequencep(cond) or isinstance(cond, str) else
                                               cond)), t))
                        # _poor_man_typecase(cond,
                        #                    (BaseException, lambda: cond),
                        #                    (str,       lambda: error_(cond)))
                cond, upgradedp = _maybe_upgrade_condition(raw_cond)
                if type_of(cond) not in __not_even_conditions__ and isinstance(cond, condition_t):
                        # _debug_printf("signalling %s", cond)
                        if upgradedp:
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
                cond = _sys.call_tracing(continuation, ())
        if type_of(cond) not in __not_even_conditions__:
                if isinstance(cond, condition_t):
                        try:
                                repr_str = princ_to_string(cond)
                        except Exception as sub_cond:
                                _debug_printf("While printing condition, another condition was raised: %s", repr(sub_cond))
                                # _backtrace(frame = _exception_frame())
                                repr_str = "#<error printing condition>"
                        _here("In thread '%s': unhandled condition of type %s: %s%s",
                              _threading.current_thread().name, type_of(cond), repr_str,
                              "\n; Disabling CL condition system.",
                              callers = 15, frame = signalling_frame)
                else:
                        _debug_printf("In thread %s: a non-condition of type %s was raised: %s",
                                      _threading.current_thread().name, type_of(cond), repr(cond))
                if not backtrace_printed:
                        _backtrace()
                _frost.disable_pytracer()
                try:
                        invoke_debugger(cond)
                except error_t as debugger_cond:
                        _debug_printf("Failed to enter the debugger:\n%s\nHave a nice day!", debugger_cond)
                        _sys.stderr.flush()
                        exit()
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
        nonce            = gensym("HANDLER-CASE-")
        wrapped_handlers = [ (ty_ha[0], lambda cond: __return_from(nonce, ty_ha[1](cond)))
                             for ty_ha in handlers ]
        return __catch(nonce,
                       lambda: handler_bind(body, *wrapped_handlers, no_error = no_error))

def ignore_errors(body):
        return handler_case(body,
                            (Exception,
                             lambda _: None))

# Built-in condition types

@defclass
class warning_t(condition_t):                                              pass
@defclass
class simple_warning_t(simple_condition_t, warning_t):                     pass

@defclass
class style_warning_t(warning_t):                                          pass
@defclass
class simple_style_warning_t(simple_warning_t, style_warning_t):           pass
@defun
def simple_style_warning(format_control, *format_arguments):
        warn(simple_style_warning_t, format_control = format_control, format_arguments = format_arguments)

@defclass
class type_error_t(error_t):                                               pass
@defclass
class simple_type_error_t(simple_error_t, type_error_t):                   pass
@defun
def simple_type_error(format_control, *format_arguments):
        error(simple_type_error_t, format_control = format_control, format_arguments = format_arguments)

@defclass
class program_error_t(error_t):                                            pass
@defclass
class simple_program_error_t(simple_error_t, program_error_t):             pass
@defun
def simple_program_error(format_control, *format_arguments):
        error(simple_program_error_t, format_control = format_control, format_arguments = format_arguments)

@defclass
class undefined_function_t(error_t):
        def __init__(self, fname):
                self.name = fname
        def __str__(self):
                return "The function %s is undefined." % (self.name,)
        def __repr__(self):
                return self.__str__()

@defclass
class _not_implemented_condition(condition_t):
        def __init__(*args):
                self, name = args[0], args[1]
                self.name = name
        def __str__(self):
                return "Not implemented: " + self.name.upper()
        def __repr__(self):
                return self.__str__()
@defclass
class _not_implemented_error(_not_implemented_condition, error_t):     pass
@defclass
class _not_implemented_warning(_not_implemented_condition, warning_t): pass

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
class restart_t(_servile):
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
#                               report_function      = lambda stream: print_restart_summary(stream),
#                               test_function        = lambda cond: visible_p(cond))))
_string_set("*RESTART-CLUSTERS*", [])

def _restartp(x):
        return isinstance(x, restart_t)

def restart_name(x):
        return x.name

def _specs_restarts_args(restart_specs):
        # format (t, "_s_r: %s", restart_specs)
        restarts_args = make_hash_table()
        for name, spec in restart_specs.items():
                function, options = ((spec[0], spec[1]) if isinstance(spec, tuple) else
                                     (spec, make_hash_table()))
                restarts_args[name.upper()] = _updated_dict(options, dict(function = function)) # XXX: name mangling!
        return restarts_args

##
# XXX: :TEST-FUNCTION is currently IGNORED!
##
def _restart_bind(body, restarts_args):
        with progv({_restart_clusters_: (_symbol_value(_restart_clusters_) +
                                           [_remap_hash_table(lambda _, restart_args: make_instance(restart_t, **restart_args), restarts_args)])}):
                return body()

def restart_bind(body, **restart_specs):
        return _restart_bind(body, _specs_restarts_args(restart_specs))

__valid_restart_options__ = frozenset(["interactive", "report", "test", "function"])
def _restart_case(body, **restarts_args):
        def validate_restart_options(options):
                unknown = set(options.keys()) - __valid_restart_options__
                return t if not unknown else simple_type_error("Acceptable restart options are: (%s), not (%s)",
                                                               " ".join(__valid_restart_options__), " ".join(options.keys()))
        nonce = gensym("RESTART-CASE-")
        wrapped_restarts_args = {
                restart_name: _poor_man_let(restart_args["function"],
                                  restart_args["interactive"] if "interactive" in restart_args else nil,
                                  restart_args["report"]      if "report"      in restart_args else nil,
                                  lambda function, interactive, report:
                                          (validate_restart_options(restart_args) and
                                           _updated_dict(restart_args,
                                                         dict(name                 = restart_name,
                                                                  function             =
                                                                  lambda *args, **keys:
                                                                          __return_from(nonce, function(*args, **keys)),
                                                                  interactive_function =
                                                                  (interactive                  if functionp(interactive) else
                                                                   lambda: []                   if null(interactive) else
                                                                   error(":INTERACTIVE argument to RESTART-CASE must be either a function or NIL.")),
                                                                  report_function      =
                                                                  (report                       if functionp(report) else
                                                                   _curry(write_string, report) if isinstance(report, str) else
                                                                   nil                          if null(report) else
                                                                   error(":REPORT argument to RESTART-CASE must be either a function, a string or NIL."))))))
                for restart_name, restart_args in restarts_args.items () }
        return __catch(nonce,
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
        description = (format_control_and_arguments if isinstance(format_control_and_arguments, str) else
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
        restarts = list()
        for cluster in reversed(_symbol_value(_restart_clusters_)):
                # format(t, "Analysing cluster %s for \"%s\".", cluster, name)
                restarts.extend([ x for x in cluster.values()
                                  if restart_condition_association_check(condition, x) ]
                                if condition else
                                cluster.values())
        return restarts

def invoke_restart(restart, *args, **keys):
        """
Calls the function associated with RESTART, passing arguments to
it. Restart must be valid in the current dynamic environment.
"""
        assert(isinstance(restart, str) or _restartp(restart))
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
        assert(isinstance(restart, str) or _restartp(restart))
        restart = restart if _restartp(restart) else find_restart(restart)
        return invoke_restart(restart, *restart.interactive_function())

# DEFBODY

#     This is used by @defast and @defknown.

def _defbody_parse_ast(names, asts, valid_declarations = make_hash_table()):
        """Given a list of defined parameter NAMES, a list of statement ASTS and a
table of VALID-DECLARATIONS, return the body, documentation and declarations if any."""
        def _ast_call_to_name_p(name, x):
                return (isinstance(x, _ast.Expr)            and
                        isinstance(x.value, _ast.Call)      and
                        isinstance(x.value.func, _ast.Name) and
                        x.value.func.id == name)
        def ensure_valid_declarations(decls):
                # Unregistered Issue ENSURE-VALID-DECLARATION-SUGGESTS-FASTER-CONVERGENCE-TO-METASTRUCTURE
                def fail():
                        import more_ast
                        err("invalid declaration form: %s", more_ast.pp_ast_as_code(decls))
                def ensure_valid_declaration(decl):
                        isinstance(decl, _ast.Tuple) and decl.elts and isinstance(decl.elts[0], _ast.Name) or fail()
                        decl_name = decl.elts[0].id
                        if decl_name not in valid_declarations:
                                err("unknown declaration: %s", decl_name.upper())
                        n_decl_args = valid_declarations[decl_name]
                        if len(decl.elts) < 1 + n_decl_args + 1:
                                err("invalid declaration %s: no parameter names specified", decl_name.upper())
                        all(isinstance(x, _ast.Name) for x in decl.elts[1 + n_decl_args:]) or fail()
                        decl_param_names = tuple(x.id for x in decl.elts[1 + n_decl_args:])
                        unknown_param_names = set(decl_param_names) - set(names)
                        if unknown_param_names:
                                err("invalid declaration %s: invalid parameter names: %s",
                                    decl_name.upper(), ", ".join(x.upper() for x in unknown_param_names))
                        return (decl_name,
                                tuple(extract_sexp(x) for x in decl.elts[1:1 + n_decl_args]),
                                decl_param_names)
                not (decls.keywords or decls.starargs or decls.kwargs) or fail()
                return [ ensure_valid_declaration(x) for x in decls.args ]
        def group_declarations(valid_declspecs, decls):
                def _declaration_names(x): return set(x[1 + valid_declspecs[x[0]]:])
                return { name: set(d[0:2] for d in decls
                                   if name in d[2:])
                         for name in _mapsetn(_declaration_names, decls) } # INDEXING..
        content, _ = _prefix_suffix_if(_not_of_type(_ast.Pass), asts)
        documentation, body = ((content[0].value.s, content[1:]) if (len(content) > 1 and
                                                                     isinstance(content[0], _ast.Expr) and
                                                                     isinstance(content[0].value, _ast.Str)) else
                               (nil, content))
        declarations, body = _prefix_suffix_if(_curry(_ast_call_to_name_p, "declare"), body)
        return body, documentation, group_declarations(valid_declarations,
                                                       reduce(lambda acc, dexcall:
                                                                      acc + ensure_valid_declarations(dexcall.value),
                                                              declarations, []))

def _defbody_methods(desc, body_ast, method_name_fn, method_specs, arguments_ast = None):
        method_specs = list((mspec if isinstance(mspec, tuple) else
                            (mspec, _defbody_make_required_method_error(desc))) for mspec in method_specs)
        def fail(x):
                import more_ast
                error("In %s: definition body may only contain definitions of %s methods, encountered: %s, an object of type %s", desc,
                      (", ".join([x.upper() for x, _ in method_specs[:-1]]) +
                       (" and " if len(method_specs) > 1 else "") +
                       (method_specs[-1][0].upper() if method_specs else "")),
                      x if isinstance(x, str) else more_ast.pp_ast_as_code(x), type_of(x))
        def process(method_name, default_maker):
                "Return a validated and normalised named method body 'return'-wise."
                xs = [ x for x in body_ast if x.name is method_name ]
                x = xs[0] if xs else nil
                method_name = method_name_fn(method_name)
                if x:
                        x.name, x.args = method_name, _defaulted(arguments_ast, x.args)
                        ## recursively analyse RETURN-ality
                        def massage_tail(x, multiline):
                                if multiline:
                                        if hasattr(x.body[-1], "body"):
                                                massage_tail(x.body[-1], True)
                                        elif not isinstance(x.body[-1], _ast.Return):
                                                error("In %s: multi-line methods must include an explicit terminating Return statement", desc)
                                elif not(isinstance(x.body[0], _ast.Return)):
                                        if hasattr(x.body[0], "value"):
                                                x.body[0] = _ast.Return(x.body[0].value)
                                        elif hasattr(x.body[0], "body"):
                                                massage_tail(x.body[0], len(x.body[0].body) > 1)
                                        else:
                                                error("In %s: tail-positioned form %s has no redeeming qualities what so ever.",
                                                      desc, type_of(x.body[0]))
                        massage_tail(x, len(x.body) > 1)
                return _ast_compiled_name(method_name, x or default_maker(method_name),
                                          locals = locals(), globals = globals())
        ##
        body_ast = [ x for x in body_ast if not typep(x, _ast.Pass )] # Remove noise.
        non_fdefns = [ x for x in body_ast if not typep(x, _ast.FunctionDef) ]
        if non_fdefns:
                fail(non_fdefns[0])
        specified_method_names = { x.name:x for x in body_ast }
        invalid_methods = set(specified_method_names) - _mapset(lambda x: x[0], method_specs)
        if invalid_methods:
                fail(invalid_methods.pop().upper())
        return (process(*mspec) for mspec in method_specs)

def _defbody_make_required_method_error(desc):
        return lambda method_name: error("In %s: missing method %s.", desc, str(method_name).upper())

# AST toolkit

def _astp(x):        return isinstance(x, _ast.AST)

def _coerce_to_ast_type(type_):
        return ((type_ if subtypep(type_, _ast.AST) else error("Provided type %s is not a proper subtype of _ast.AST", type_))
                if isinstance(type_, type) else
                (_ast.__dict__[type_] if type_ in _ast.__dict__ else error("Unknown AST type '%s'.", type_))
                if isinstance(type_, str)  else
                error("Invalid AST type specifier: %s, %s, %s.", type_, type, isinstance(type_, type)))

def _text_ast(text):
        return _py.compile(text, "", 'exec', flags = _ast.PyCF_ONLY_AST).body

def _function_ast(fn):
        fn_ast = _text_ast(_without_condition_system(lambda: _inspect.getsource(fn)))[0]
        return fn_ast.args, fn_ast.body

def _function_body_pass_p(fn):
        fn_body_ast = _function_ast(fn)[1]
        return len(fn_body_ast) == 1 and isinstance(fn_body_ast[0], _ast.Pass)

### literals
def _ast_num(n):
        return _ast.Num(n = the(integer_t, n))
def _ast_bool(n):
        return _ast.Bool(n = the(integer_t, n))
def _ast_string(s):
        return _ast.Str(s = the(string_t, s))
def _ast_set(xs,   writep = nil):
        return _ast.Set(elts   = the((pylist_t, _ast.AST), xs), ctx = _ast_rw(writep))
def _ast_list(xs,  writep = nil):
        return _ast.List(elts  = the((pylist_t, _ast.AST), xs), ctx = _ast_rw(writep))
def _ast_tuple(xs, writep = nil):
        return _ast.Tuple(elts = the((pylist_t, _ast.AST), xs), ctx = _ast_rw(writep))

################################# recurse? AST-ifier
__astifier_map__ = { str:       (nil, _ast_string),
                     int:       (nil, _ast_num),
                     bool:      (nil, _ast_num),
                     _NoneType: (nil, lambda x: _ast_name("None")),
                     list:      (t,   _ast_list),
                     tuple:     (t,   _ast_tuple),
                     set:       (t,   _ast_set),
                     ## symbol: see below
                     }
def _register_astifier_for_type(type, recurse, astifier):
        "Please, list the added astifiers above."
        __astifier_map__[type] = (recurse, astifier)

def _unregister_astifier_for_type(type):
        del __astifier_map__[type]

def _astifiable_p(x):
        return type(x) in __astifier_map__

def _try_astify_constant(x):
        if _astp(x):
                return x, t
        (rec, astifier), astifiable = gethash(type_of(x), __astifier_map__,
                                              ((nil, nil), nil))
        return (astifier([ _astify_constant(x) for x in x ] if rec else
                         x), t) if astifiable else (None, None)

def _astify_constant(x):
        ast, successp = _try_astify_constant(x)
        return (ast if successp else
                error("Cannot convert value %s to _AST.  Is it a literal?",
                      prin1_to_string(x)))

def _coerce_to_ast(x):
        return _astify_constant(x) if not _astp(x) else x

### expressions
def __ast_alias(name):                       return _ast.alias(name = the(string_t, name), asname = None)
def _ast_keyword(name, value):               return _ast.keyword(arg = the(string_t, name), value = the(_ast.expr, value))

def _ast_rw(writep):                         return (_ast.Store() if writep else _ast.Load())
def _ast_name(name, writep = nil):           return _ast.Name(id = the(string_t, name), ctx = _ast_rw(writep))
def _ast_attribute(x, name, writep = nil):   return _ast.Attribute(attr = name, value = x, ctx = _ast_rw(writep))
def _ast_attribute_chain(xs, writep = nil):  return reduce((lambda acc, attr: _ast_attribute(acc, attr, writep)),
                                                           xs[1:],
                                                           _ast_name(xs[0], writep))
def _ast_index(of, index, writep = nil):     return _ast.Subscript(value = of, slice = _ast.Index(value = index), ctx = _ast_rw(writep))
def _ast_maybe_normalise_string(x):          return (_ast_string(x) if isinstance(x, str) else x)

def _ast_funcall(name, args = [], keys = {}, starargs = None, kwargs = None):
        check_type(args, (pylist_t, (or_t, _ast.AST, _NoneType, (satisfies_t, _astifiable_p))))
        return _ast.Call(func = (_ast_name(name) if isinstance(name, str) else name),
                        args = [ _coerce_to_ast(x) for x in args ],
                        keywords = _maphash(_ast_keyword, keys),
                        starargs = starargs or None,
                        kwargs = kwargs or None)

def _ast_and(*args):
        return _ast.BoolOp(_ast.And(), list(args))

def _ast_or(*args):
        return _ast.BoolOp(_ast.Or(), list(args))

### statements
def _ast_Expr(node):
        return _ast.Expr(value = the(_ast.expr, node))

def _ast_module(body, lineno = 0):
        return _ast.Module(body = the((pylist_t, _ast.AST), body),
                          lineno = lineno)

def _ast_import(*names):
        return _ast.Import(names = [ __ast_alias(x) for x in the((homotuple_t, string_t), names) ])
def _ast_import_from(module_name, names):
        return _ast.ImportFrom(module = the(string_t, module_name),
                              names = [ __ast_alias for x in the((pylist_t, string_t), names) ],
                              level = 0)

def _ast_assign(to, value):
        return _ast.Assign(targets = the((pylist_t, _ast.AST), to),
                          value = the(_ast.AST, value))
def _ast_return(node):
        return _ast.Return(value = the(_ast.AST, node))

## ast.FunctionDef tools
#
# arguments = (arg* args, identifier? vararg, expr? varargannotation,
#              arg* kwonlyargs, identifier? kwarg,
#              expr? kwargannotation, expr* defaults,
#              expr* kw_defaults)
#
# arg = (identifier arg, expr? annotation)
#
# keyword = (identifier arg, expr value)

def _argspec_nfixargs(paramspec):
        return len(paramspec.args) - len(paramspec.defaults or []) # ILTW Python implementors think..

def _argspec_lambda_spec(spec, astify_defaults = t):
        # args, varargs, varkw, defaults, kwonlyargs, kwonlydefaults, annotations
        nfixargs = _argspec_nfixargs(spec)
        default_xform = _astify_constant if astify_defaults else identity
        return (spec.args[:nfixargs],
                list(zip(spec.args[nfixargs:],
                         [ default_xform(x) for x in spec.defaults or [] ])),
                spec.varargs,
                list(zip(spec.kwonlyargs,
                         [ default_xform(x) for x in spec.kwonlydefaults or [] ])),
                spec.varkw)

def _function_lambda_list(fn, astify_defaults = t):
        "Returns: FIXPARMS, OPTIONAL-WITH-DEFAULTS, VARARGS, KEYS-WITH-DEFAULTS, KWARGS."
        return _argspec_lambda_spec(_inspect.getfullargspec(fn), astify_defaults = astify_defaults)

def _lambda_spec_arguments(lambda_list_spec):
        fixed, optional, args, keyword, keys = lambda_list_spec
        return _ast.arguments(args        = [ _ast.arg(x, None)
                                              for x in fixed + [ x[0] for x in optional ] ],
                              defaults    = [ x[1] for x in optional ],
                              vararg      = args,
                              kwonlyargs  = [ _ast.arg(x, None)
                                              for x in [ x[0] for x in keyword ] ],
                              kw_defaults = [ x[1] for x in keyword ],
                              kwarg       = keys,
                              varargannotation = None,
                              kwargannotation  = None)

def _ast_functiondef(name, lambda_list_spec, body):
        fixed, optional, args, keyword, keys = lambda_list_spec
        return _ast.FunctionDef(
                name = the(string_t, name),
                args = _lambda_spec_arguments(lambda_list_spec),
                lineno = 0,
                decorator_list = [],
                returns = None,
                body = _poor_man_etypecase(body,
                                           ((pylist_t, _ast.AST),
                                            body),
                                           (function_t,
                                            lambda:
                                                    body(*tuple(_ast_name(x) for x in fixed),
                                                          **_map_into_hash(lambda x: (x, _ast_name),
                                                                           (list(optional) + list(keyword) +
                                                                            ([args] if args else []) +
                                                                            ([keys] if keys else [])))))))

def _ast_defun_fixed(name, names, *body):
        return _ast_functiondef(name, (names, [], None, [], None),
                                list(body))

# AST interning: %READ-AST, %READ-PYTHON-TOPLEVEL-AS-LISP

def _literal_ast_sex(ast_):
        def fail(sex):
                import more_ast
                error("Invalid sexp: %s.", more_ast.pp_ast_as_code(sex))
        return (ast_.id                                                if isinstance(ast_, _ast.Name)  else
                ast_.n                                                 if isinstance(ast_, _ast.Num)   else
                _consify_linear([ extract_sexp(x) for x in ast_.elts ]) if isinstance(ast_, _ast.Tuple) else
                fail(ast_))

def _read_ast(x):
        def rec(x):
                def read_symbol(x):
                        lisp_name = _frost.python_name_lisp_symbol_name(x)
                        name, keywordp = (lisp_name, nil) if lisp_name[0] != ":" else (lisp_name[1:], t)
                        package = symbol_value(_package_) if not keywordp else __keyword
                        return _intern(name, package)[0]
                return (x.n                                      if isinstance(x, _ast.Num)   else
                        x.s                                      if isinstance(x, _ast.Str)   else
                        read_symbol(x.id)                        if isinstance(x, _ast.Name)  else
                        _consify_linear([rec(e) for e in x])      if isinstance(x, list)       else
                        _consify_linear([rec(e) for e in x.elts]) if isinstance(x, _ast.Tuple) else
                        _read_ast(x.value)                       if isinstance(x, _ast.Expr)  else
                        error("LISP: don't know how to intern value %s of type %s.", x, type_of(x)))
        with progv(# {_read_case_: _keyword("preserve")}
                   ):
                return _expand_quasiquotation(rec(x))

def _read_python_toplevel_as_lisp(fn, allowed_toplevels = { "DEFUN", "DEFMACRO" }):
        def read_python_toplevel_name(f):
                symbol_name = _frost.python_name_lisp_symbol_name(f.__name__)
                symbol = _intern(symbol_name)[0]
                return symbol, symbol_name, f.__name__
        name, sym_name, pyname = read_python_toplevel_name(fn)
        _frost.setf_global(name, pyname.lower(),
                           globals())
        args_ast, body_ast = _function_ast(fn)
        if len(body_ast) > 1:
                error("In LISP %s: toplevel definitions are just that: toplevel definitions. "
                      "No more than one toplevel form is allowed per definition.", name)
        form = _read_ast(body_ast[0])
        if not (symbolp(form[0]) and symbol_name(form[0]) in allowed_toplevels):
                error("In LISP %s: only toplevels in %s are allowed.",
                      repr(form[0]), __def_allowed_toplevels__)
        return name, form

# Rich AST definition machinery

_ast_info = _poor_man_defstruct("_ast_info",
                                "type",
                                "fields",     # each field is dict(name, type, walk, [default])
                                "bound_free",
                                "nfixed")
__ast_walkable_field_types__ = set([_ast.stmt, (pylist_t, _ast.expr), (maybe_t, _ast.expr),
                                    _ast.expr, (pylist_t, _ast.stmt)])
__ast_infos__         = make_hash_table()
def  _find_ast_info(type):     return __ast_infos__[_coerce_to_ast_type(type)]
def __find_ast_info(type):     return __ast_infos__[type]
def _ast_info_check_args_type(info, args, atreep = t):
        if len(args) < info.nfixed:
                error("AST type '%s' requires %s %d arguments, but only %d were provided: %s.",
                      info.type.__name__, "exactly" if len(info.fields) == info.nfixed else "at least", info.nfixed,
                      len(args), args)
        def check_arg_type(arg, type):
                def maybe_typespec_p(x): return isinstance(x, tuple) and len(x) is 2 and x[0] is maybe_t
                def list_typespec_p(x):  return isinstance(x, tuple) and len(x) is 2 and x[0] is pylist_t
                def simple_typespec_p(x):
                        return (x is integer_t or
                                x is string_t or
                                isinstance(x, tuple) and len(x) is 2 and x[0] is maybe_t and
                                (x[1] is integer_t or x[1] is string_t))
                ## The eternal beauty sleeps..
                # def maybe_typespec_p(x): return typep(x, (pytuple_t, (eql_t, maybe_t), t))
                # def list_typespec_p(x):  return typep(x, (pytuple_t, (eql_t, pylist_t), t))
                # def simple_typespec_p(x):
                #         return typep(x, (or_t, (member_t, integer_t, string_t),
                #                                (pytuple_t, (eql_t, maybe_t), (member_t, integer_t, string_t))))
                def atree_simple_typep(x, type):
                        ast_info = isinstance(x, tuple) and isinstance(x[0], str) and _find_ast_info(x[0])
                        return ast_info and issubclass(ast_info.type, type)
                if atreep and not simple_typespec_p(type):
                        maybe_typep, list_typep, type = ((t,   nil, type[1]) if maybe_typespec_p(type) else
                                                         (nil,   t, type[1]) if list_typespec_p(type)  else
                                                         (nil, nil, type))
                        return (maybe_typep                                                         if arg is None else
                                isinstance(arg, list) and all(check_arg_type(x, type) for x in arg) if list_typep  else
                                atree_simple_typep(arg, type))
                else:
                        return typep(arg, type)
        for i, (field, arg) in enumerate(zip(info.fields.values(), args)):
                if not check_arg_type(arg, field["type"]):
                        error("Argument %d (field %s) of AST '%s' must correspond to type %s, but was an instance of %s, instead: %s.",
                              i, repr(field["name"]), info.type.__name__, field["type"], type_of(arg), repr(arg))
        return t

def _ast_ensure_stmt(x):
        return x if isinstance(x, _ast.stmt) else _ast.Expr(the(_ast.AST, x))

# AST/Atree bound/free calculation

_intern_and_bind_names_in_module("*BOUND-FREE-RECURSOR*")

def _bound_free_recursor():
        return _symbol_value(_bound_free_recursor_)

__atrees_validate__ = nil

def _ast_bound_free(astxs):
        def ast_rec(astxs):
                def bound_free(ast):
                        info = __find_ast_info(type_of(ast))
                        args = [ _slot_of(ast)(x) for x in type(ast)._fields ]
                        if __atrees_validate__:
                                _ast_info_check_args_type(info, args, atreep = nil)
                        return info.bound_free(*args)
                return _separate(3, bound_free, [ x for x in _ensure_list(astxs) if x is not None ])
        with progv({_bound_free_recursor_: ast_rec}):
                return ast_rec(the((or_t, _ast.AST, (pylist_t, _ast.AST)),
                                   astxs))

def _atree_validate(atree):
        if isinstance(atree, (str, int, _NoneType)):
                return
        if not (isinstance(atree, tuple) and atree and isinstance(atree[0], str)):
                error("Invalid atree: %s", atree)
        kind, args = atree[0], atree[1:]
        info = _find_ast_info(kind)
        _ast_info_check_args_type(info, args, atreep = t)
        for xxs in args:
                for xs in _ensure_list(xxs):
                        _atree_validate(xs)

def _atree_bound_free(atreexs):
        def atree_rec(atreexs):
                def bound_free(atree):
                        check_type(atree, (partuple_t, string_t))
                        info = _find_ast_info(atree[0])
                        args = atree[1:]
                        _ast_info_check_args_type(info, args, atreep = t)
                        return info.bound_free(*args)
                return _separate(3, bound_free, [ x for x in _ensure_list(atreexs) if x is not None ])
        with progv({_bound_free_recursor_: atree_rec}):
                return atree_rec(the((or_t, pytuple_t, (pylist_t, pytuple_t)),
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
                return (tuple(fixed) +
                        tuple(xform(x) for x in optional) + (() if not args else (args,)) +
                        tuple(xform(x) for x in keyword)  + (() if not keys else (keys,)))
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
                ast_field_names = fixed + [ x[0] for x in optional ]
                ast_field_names_with_defaults = fixed + optional
                ast_field_types = [ annotations[name] for name in ast_field_names ]
                if len(ast_field_types) != len(ast_type._fields):
                        error("In DEFAST %s:the amount of provided type specifiers (%d) does not match the AST _fields: %s",
                              name, len(ast_field_types), ast_type._fields)
                type_specifier_type = (or_t, pytuple_t, type)
                if not all(typep(x, type_specifier_type) for x in ast_field_types):
                        mismatched = [ x for x in ast_field_types
                                       if not typep(x, type_specifier_type) ]
                        error("In DEFAST %s: the AST field type specifiers must be of type %s, found: %s",
                              name, type_specifier_type, ", ".join(str(x) for x in mismatched))
                for i, (fname, ast_fname) in enumerate(zip(ast_field_names, ast_type._fields)):
                        if fname != ast_fname:
                                error("In DEFAST %s: the provided name for the %d'th field (%s) does not match its actual name (%s), expected field names: %s",
                                      name, i, fname, ast_fname, ast_type._fields)
                return ast_field_types
        def arglist_field_infos(parameters, nfix, with_defaults, ast_field_types, grouped_decls):
                fields = _without_condition_system(lambda: _collections.OrderedDict())
                def process_ast_field_arglist_entry(name, type, default, fixed = t):
                        walkp = (type in __ast_walkable_field_types__ or
                                 declaredp(grouped_decls, p, "walk"))
                        fields[p] = (dict(name = name, type = type, walk = walkp) if fixed else
                                     dict(name = name, type = type, walk = walkp, default = default))
                for p, type, defaulted in zip(parameters[:nfix], ast_field_types[:nfix], with_defaults[:nfix]):
                        process_ast_field_arglist_entry(p, type, None,         fixed = t)
                for p, type, defaulted in zip(parameters[nfix:], ast_field_types[nfix:], with_defaults[nfix:]):
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
        valid_declspecs = dict(walk  = 0)
        body, documentation, declarations = _defbody_parse_ast(parameters, body_ast,
                                                               valid_declarations = valid_declspecs)
        fields = arglist_field_infos(parameters, len(fixed), with_defaults, ast_field_types, declarations)
        [bound_free] = _defbody_methods("DEFAST " + name, body,
                                        lambda method: "_ast_%s_%s" % (name, method),
                                        [("bound_free", lambda name: make_default_bound_free(name, args_ast, fields))],
                                        arguments_ast = args_ast)
        # _debug_printf("bound_free for %s is %s", name, bound_free)
        __ast_infos__[ast_type] = _ast_info(type       = ast_type,
                                            fields     = fields,
                                            bound_free = bound_free,
                                            nfixed     = len(fixed))

## AST + Symbols
_register_astifier_for_type(symbol_t, nil, (lambda sym: _ast_funcall("_find_symbol_or_fail", [symbol_name(sym)])))

# Definitions

# mod = Module(stmt* body)
#     | Interactive(stmt* body)
#     | Expression(expr body)
@defast
def _ast_Module(body: (pylist_t, _ast.stmt)): pass
@defast
def _ast_Interactive(body: (pylist_t, _ast.stmt)): pass
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
def _ast_FunctionDef(name:            string_t,
                     args:            _ast.arguments,
                     body:           (pylist_t, _ast.stmt),
                     decorator_list: (pylist_t, _ast.expr) = list(),
                     returns:        (maybe_t,  _ast.expr) = None):
        def bound_free():
                ((args_b, args_f, _),
                 (body_b, body_f, body_x),
                 (_,      deco_f, _),
                 (_,      retn_f, _)) = [ _bound_free_recursor()(x)
                                          for x in [args, body, decorator_list, returns] ]
                body_bound = set([name]) | args_b | (body_b - body_x)
                body_free = body_f - body_bound
                body_xtnl_writes = body_b & body_x
                free = args_f | body_free | deco_f | retn_f
                return (body_xtnl_writes, # names declared global/nonlocal and assigned to
                        free,
                        set())        # these do not escape..
#       | ClassDef(identifier name,
# 		   expr* bases,
# 		   keyword* keywords,
# 		   expr? starargs,
# 		   expr? kwargs,
# 		   stmt* body,
# 		   expr* decorator_list)
@defast
def _ast_ClassDef(name:            string_t,
                  bases:          (pylist_t, _ast.expr),
                  keywords:       (pylist_t, _ast.keyword),
                  starargs:       (maybe_t,  _ast.expr),
                  kwargs:         (maybe_t,  _ast.expr),
                  body:           (pylist_t, _ast.stmt),
                  decorator_list: (pylist_t, _ast.expr)):
        def bound_free():
                ((base_b, base_f, _),
                 (keyw_b, keyw_f, _),
                 (star_b, star_f, _),
                 (karg_b, karg_f, _),
                 (body_b, body_f, body_x),
                 (deco_b, deco_f, _)) = [ _bound_free_recursor()(x)
                                          for x in [bases, keywords, starargs, kwargs, body, decorator_list] ]
                # Unregistered Issue CLASS-BINDINGS-UNCLEAR
                body_bound = body_b - body_x
                body_free = body_f - body_bound
                body_xtnl_writes = body_b & body_x
                free = base_f | keyw_f | star_f | karg_f | body_free | deco_f
                return (body_xtnl_writes, # names declared global/nonlocal and assigned to
                        free,
                        set())        # these do not escape..
#       | Return(expr? value)
@defast
def _ast_Return(value: (maybe_t, _ast.expr)): pass
#       | Delete(expr* targets)
@defast
def _ast_Delete(targets: (pylist_t, _ast.expr)): pass
        # targets do ref, in this case!
#       | Assign(expr* targets, expr value)
@defast
def _ast_Assign(targets: (pylist_t, _ast.expr),
                value:    _ast.expr):
        def bound_free():
                ((targ_b, targ_f, _),
                 (_,      valu_f, _)) = [ _bound_free_recursor()(x) for x in [targets, value] ]
                return (targ_b,
                        targ_f | valu_f,
                        set())
#       | AugAssign(expr target, operator op, expr value)
@defast
def _ast_AugAssign(target: _ast.expr,
                   op:     _ast.operator,
                   value:  _ast.expr):
        def bound_free():
                ((targ_b, targ_f, _),
                 (_,      valu_f, _)) = [ _bound_free_recursor()(x) for x in [target, value] ]
                return (targ_b,
                        targ_f | valu_f,
                        set())

def _ast_body_bound_free(body, more_bound = set()):
        body_b, body_f, body_x = _bound_free_recursor()(body)
        bound = more_bound | (body_b - body_x)
        return (bound,
                body_f - bound,
                body_b & body_x)

#       | For(expr target, expr iter, stmt* body, stmt* orelse)
@defast
def _ast_For(target:  _ast.expr,
             iter:    _ast.expr,
             body:   (pylist_t, _ast.stmt),
             orelse: (pylist_t, _ast.stmt)):
        def bound_free():
                ((targ_b, targ_f, _),
                 (_,      iter_f, _)) = [ _bound_free_recursor()(x) for x in [target, iter] ]
                # Unregistered Issue HOLE-ORELSE-CAN-USE-BODY-BINDINGS
                (bound, free, xtnls) = _separate(3, _ast_body_bound_free, [body, orelse])
                return (bound,
                        targ_f | iter_f | free,
                        xtnls)

#       | While(expr test, stmt* body, stmt* orelse)
@defast
def _ast_While(test:    _ast.expr,
               body:   (pylist_t, _ast.stmt),
               orelse: (pylist_t, _ast.stmt)):
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
            body:   (pylist_t, _ast.stmt),
            orelse: (pylist_t, _ast.stmt)):
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
              optional_vars: (maybe_t, _ast.expr),
              body:          (pylist_t, _ast.stmt)):
        def bound_free():
                ((_,      ctxt_f, _),
                 (optl_b, optl_f, _)) = [ _bound_free_recursor()(x) for x in [context_expr, optional_vars] ]
                body_bound, body_free, body_xtnls = _ast_body_bound_free(body, optl_b)
                return (body_bound,
                        ctxt_f | optl_f | body_free,
                        body_xtnls)
#       | Raise(expr? exc, expr? cause)
@defast
def _ast_Raise(exc:   (maybe_t, _ast.expr),
               cause: (maybe_t, _ast.expr)): pass
#       | TryExcept(stmt* body, excepthandler* handlers, stmt* orelse)
@defast
def _ast_TryExcept(body:     (pylist_t, _ast.stmt),
                   handlers: (pylist_t, _ast.excepthandler),
                   orelse:   (pylist_t, _ast.stmt)):
        # Unregistered Issue HOLE-ORELSE-CAN-USE-BODY-BINDINGS
        def bound_free(): _separate(3, _ast_body_bound_free, [body, handlers, orelse])
#       | TryFinally(stmt* body, stmt* finalbody)
@defast
def _ast_TryFinally(body:      (pylist_t, _ast.stmt),
                    finalbody: (pylist_t, _ast.stmt)):
        # Unregistered Issue HOLE-ORELSE-CAN-USE-BODY-BINDINGS
        def bound_free(): _separate(3, _ast_body_bound_free, [body, handlers, orelse])
#       | Assert(expr test, expr? msg)
@defast
def _ast_Assert(test: _ast.expr,
                msg:  _ast.expr = None): pass
#       | Import(alias* names)
@defast
def _ast_Import(names: (pylist_t, _ast.alias)):
        declare((walk, names))
#       | ImportFrom(identifier? module, alias* names, int? level)
@defast
def _ast_ImportFrom(module: (maybe_t, string_t),
                    names:  (pylist_t, _ast.alias),
                    level:  (maybe_t, integer_t)):
        def bound_free():
                return (_bound_free_recursor()(names)[0],
                        set([module] if module else []),
                        set())
#       | Global(identifier* names)
@defast
def _ast_Global(names: (pylist_t, string_t)):
        def bound_free(): (set(), set(), set(names))
#       | Nonlocal(identifier* names)
@defast
def _ast_Nonlocal(names: (pylist_t, string_t)):
        def bound_free(): (set(), set(), set(names))
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
                values: (pylist_t, _ast.expr)): pass
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
                 (_,      body_f, _)) = [ _bound_free_recursor()(x) for x in [args, body] ]
                body_free = body_f - args_b
                free = args_f | body_free
                return (set(),
                        free,
                        set())
#      | IfExp(expr test, expr body, expr orelse)
@defast
def _ast_IfExp(test:   _ast.expr,
               body:   _ast.expr,
               orelse: _ast.expr): pass
#      | Dict(expr* keys, expr* values)
@defast
def _ast_Dict(keys:   (pylist_t, _ast.expr),
              values: (pylist_t, _ast.expr)): pass
#      | Set(expr* elts)
@defast
def _ast_Set(elts: (pylist_t, _ast.expr)): pass
#      | ListComp(expr elt, comprehension* generators)

def _ast_gchain_bound_free(xs, acc_binds):
        if xs:
                g_binds, g_free, _ = _bound_free_recursor()(xs[0])
                finbound, cfree = _ast_gchain_bound_free(xs[1:], acc_binds | g_binds)
                return finbound, (g_free - acc_binds) | cfree
        else:
                return acc_binds, set()

def _ast_comprehension_bound_free(exprs, generators):
        gchain_bound, gchain_free = _ast_gchain_bound_free(generators, set())
        _, exprs_f, _ = _separate(3, _bound_free_recursor(), exprs)
        return (set(),
                gchain_free | (exprs_f - gchain_bound),
                set())

@defast
def _ast_ListComp(elt:         _ast.expr,
                  generators: (pylist_t, _ast.comprehension)):
        def bound_free(): _ast_comprehension_bound_free([elt], generators)
#      | SetComp(expr elt, comprehension* generators)
@defast
def _ast_SetComp(elt:         _ast.expr,
                 generators: (pylist_t, _ast.comprehension)):
        def bound_free(): _ast_comprehension_bound_free([elt], generators)
#      | DictComp(expr key, expr value, comprehension* generators)
@defast
def _ast_DictComp(key:        _ast.expr,
                  value:      _ast.expr,
                  generators: (pylist_t, _ast.comprehension)):
        def bound_free(): _ast_comprehension_bound_free([key, value], generators)
#      | GeneratorExp(expr elt, comprehension* generators)
@defast
def _ast_GeneratorExp(elt:         _ast.expr,
                      generators: (pylist_t, _ast.comprehension)):
        def bound_free(): _ast_comprehension_bound_free([elt], generators)
#      | Yield(expr? value)
@defast
def _ast_Yield(value: (maybe_t, _ast.expr) = None): pass
#      | Compare(expr left, cmpop* ops, expr* comparators)
@defast
def _ast_Compare(left:         _ast.expr,
                 ops:         (pylist_t, _ast.cmpop),
                 comparators: (pylist_t, _ast.expr)): pass
#      | Call(expr func, expr* args, keyword* keywords, expr? starargs, expr? kwargs)
@defast
def _ast_Call(func:      _ast.expr,
              args:     (pylist_t, _ast.expr),
              keywords: (pylist_t, _ast.keyword),
              starargs: (maybe_t, _ast.expr) = None,
              kwargs:   (maybe_t, _ast.expr) = None):
        def bound_free(): _separate(3, _bound_free_recursor(),
                                    [func, args, keywords, starargs, kwargs])
#      | Num(object n) -- a number as a PyObject.
@defast
def _ast_Num(n: integer_t): pass
#      | Str(string s) -- need to specify raw, unicode, etc?
@defast
def _ast_Str(s: string_t): pass
#      | Bytes(string s)
@defast
def _ast_Bytes(s: string_t): pass
#      | Ellipsis
@defast
def _ast_Ellipsis(): pass
#      | Attribute(expr value, identifier attr, expr_context ctx)
@defast
def _ast_Attribute(value: _ast.expr,
                   attr:  string_t,
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
def _ast_Name(id:  string_t,
              ctx: _ast.expr_context):
        def bound_free(): ((set(), set([id])) if isinstance(ctx, (_ast.Load, _ast.AugLoad, _ast.Param)) else
                           (set([id]), set()))
#      | List(expr* elts, expr_context ctx)
@defast
def _ast_List(elts: (pylist_t, _ast.expr),
              ctx:   _ast.expr_context): pass
#      | Tuple(expr* elts, expr_context ctx)
@defast
def _ast_Tuple(elts: (pylist_t, _ast.expr),
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
def _ast_Slice(lower: (maybe_t, _ast.expr) = None,
               upper: (maybe_t, _ast.expr) = None,
               step:  (maybe_t, _ast.expr) = None): pass
#       | ExtSlice(slice* dims)
@defast
def _ast_ExtSlice(dims: (pylist_t, _ast.slice)):
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
                       ifs:   (pylist_t, _ast.expr)):
        def bound_free():
                ((_,      targ_f, _),
                 (iter_b, iter_f, _),
                 (_,      iffs_f, _)) = [ _bound_free_recursor()(x) for x in [target, iter, ifs] ]
                return (iter_b,
                        ((targ_f | iffs_f) - iter_b) | iter_f,
                        set())
# excepthandler = ExceptHandler(expr? type, identifier? name, stmt* body)
@defast
def _ast_ExceptHandler(type: (maybe_t, _ast.expr),
                       name: (maybe_t, string_t),
                       body: (pylist_t, _ast.stmt)):
        def bound_free():
                (_,      type_f, _) = _bound_free_recursor()(type)
                (bound, free, xtnls) = _ast_body_bound_free(body, set([name] if name is not None else []))
                return (bound,
                        type_f | free,
                        xtnls)
# arguments = (arg* args, identifier? vararg, expr? varargannotation,
#              arg* kwonlyargs, identifier? kwarg,
#              expr? kwargannotation, expr* defaults,
#              expr* kw_defaults)
@defast
### These MAYBEs suggest a remapping facility.
def _ast_arguments(args:             (pylist_t, _ast.arg),
                   vararg:           (maybe_t, string_t),
                   varargannotation: (maybe_t, _ast.expr),
                   kwonlyargs:       (pylist_t, _ast.arg),
                   kwarg:            (maybe_t, string_t),
                   kwargannotation:  (maybe_t, _ast.expr),
                   defaults:         (pylist_t, _ast.expr),
                   kw_defaults:      (pylist_t, _ast.expr)):
        def bound_free():
                arg_bound, arg_free, _ = _separate(3, _bound_free_recursor(), [args, kwonlyargs])
                arg_bound |= set(x for x in [vararg, kwarg]
                                     if x is not None)
                _, other_free, _ = _separate(3, _bound_free_recursor(), [varargannotation, kwargannotation, defaults, kw_defaults])
                return (arg_bound,
                        arg_free | other_free,
                        set())
# arg = (identifier arg, expr? annotation)
@defast
def _ast_arg(arg:         string_t,
             annotation: (maybe_t, _ast.expr) = None):
        def bound_free(): (set([arg]),
                           _bound_free_recursor()(annotation)[1],
                           set())
# keyword = (identifier arg, expr value)
@defast
def _ast_keyword(arg:   string_t,
                 value: _ast.expr):
        def bound_free(): (set([] if _nonep(asname) else [asname]),
                           _bound_free_recursor()(value),
                           set())
# alias = (identifier name, identifier? asname)
@defast
def _ast_alias(name:    string_t,
               asname: (maybe_t, string_t) = None):
        def bound_free(): (set([] if _nonep(asname) else [asname]),
                           set(),
                           set())

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
                info = __find_ast_info(ast_type)
                fields, finfos, positional, optional = [], [], [], []
                for f, i in info.fields.items():
                        fields.append(f); finfos.append(i)
                        # (positional if hasattr(i, "default") else
                        #  optional).append(i)
                positional, optional = _prefix_suffix_if(lambda x: "default" in x, finfos)
                nfixed, defacto = len(positional), len(args)
                max = nfixed + len(optional)
                if not (nfixed <= defacto <= max):
                        argument_count_error(nfixed, max, defacto, "AST type %s", type)
                effective_args = args + [ x["default"] for x in optional[defacto - nfixed:] ]
                # [ x["default"] for x in optional[defacto - nfixed:] ]
                assert(len(effective_args) == max)
                for val, name, finfo in zip(effective_args, fields, finfos):
                        subtype = finfo["type"]
                        if not typep(val, subtype):
                                argument_type_error(name, subtype, val, "AST node %s", repr(type))
                return ast_type(*effective_args)
        ret =  (tree                                                    if isinstance(tree, (str, int, _NoneType)) else
                [ _atree_ast(x) for x in tree ]                         if isinstance(tree, list)                  else
                _try_astify_constant(tree)[0]                           if not isinstance(tree, tuple)             else
                error("The atree nodes cannot be zero-length.")         if not tree                                else
                error("Atree[0] must be a string, not a %s.", tree[0])  if not isinstance(tree[0], str)            else
                unknown_ast_type_error(tree[0], tree)                   if tree[0] not in _ast.__dict__            else
                (astify_known(tree[0], [ _atree_ast(x) for x in tree[1:] ])))
        return ret

# Bindings

_intern_and_bind_names_in_module("SYMBOL",
                                 "VARIABLE", "CONSTANT", "SPECIAL", "SYMBOL-MACRO",
                                 "MACRO", "COMPILER-MACRO", "FUNCTION", "BLOCK", "GOTAG")

class _nameuse():
        name, kind, type = None, None, None
        def __init__(self, name, kind, type, **attributes):
                _attrify_args(self, locals(), "name", "kind", "type")
def _nameusep(x):
        return isinstance(x, _nameuse)

class _binding():
        value, shadows = None, None
        def __init__(self, value, shadows = None, **attributes):
                _attrify_args(self, locals(), "value", "shadows")
        def __repr__(self):
                return "#<bind %s %s: %s>" % (self.kind, self.name, self.value)
def _bindingp(x):
        return isinstance(x, _binding)

class _variable(_nameuse):
        def __init__(self, name, tn, kind, type = t, dynamic_extent = nil, **attributes):
                check_type(kind, (member_t, variable, constant, special, symbol_macro)) # constant | special | symbol-macro | variable
                _nameuse.__init__(self, name, kind, type, **attributes)
                _attrify_args(self, locals(), "tn", "dynamic_extent") # t | nil
class _function(_nameuse):
        def __init__(self, name, tn, kind, type = t, lambda_expression = None, **attributes):
                check_type(kind, (member_t, function, macro, compiler_macro)) # macro | compiler-macro | function
                _nameuse.__init__(self, name, kind, type, **attributes)
                _attrify_args(self, locals(), "tn", "lambda_expression") # None | cons
class _block(_nameuse):
        def __init__(self, name, **attributes):
                _nameuse.__init__(self, name, block, t, **attributes)
class _gotag(_nameuse):
        def __init__(self, name, **attributes):
                _nameuse.__init__(self, name, gotag, t, **attributes)

def _nameuse_variablep(x): return isinstance(x, _variable)
def _nameuse_functionp(x): return isinstance(x, _function)
def _nameuse_blockp(x): return isinstance(x, _block)

class _variable_binding(_variable, _binding):
        def __init__(self, name, tn, kind, value, **attributes):
                ## Variables are not necessarily bound to specific value forms -- function parameters.
                _variable.__init__(self, name, tn, kind, **attributes)
                _binding.__init__(self, value, **attributes)
class _function_binding(_function, _binding):
        def __init__(self, name, tn, kind, value, **attributes):
                _function.__init__(self, name, tn, kind, **attributes)
                _binding.__init__(self, value, **attributes)
class _block_binding(_block, _binding):
        def __init__(self, name, value, **attributes):
                _block.__init__(self, name, **attributes)
                _binding.__init__(self, value, **attributes)
class _gotag_binding(_gotag, _binding):
        def __init__(self, name, value, **attributes):
                _gotag.__init__(self, name, **attributes)
                _binding.__init__(self, value, **attributes)

def _variable_bindingp(x): return isinstance(x, _variable_binding)
def _function_bindingp(x): return isinstance(x, _function_binding)
def _block_bindingp(x):    return isinstance(x, _block_binding)

# Global scope: as seen by the compiler

class _scope(): pass
class _variable_scope(_scope, _collections.UserDict):
        def __hasattr__(self, name): return name in self.data
        def __getattr__(self, name): return self.data[name]
        def __init__(self):
                self.data = dict()
class _function_scope(_scope, _collections.UserDict):
        def __hasattr__(self, name): return name in self.data
        def __getattr__(self, name): return self.data[name]
        def __init__(self):
                self.data = dict()

_variable_scope = _variable_scope()
_function_scope = _function_scope()

def _find_global_variable(name):     return gethash(name, _variable_scope)[0]
def _find_global_function(name):     return gethash(name, _function_scope)[0]
def  _set_global_function(name, x): _function_scope[name] = the(_function, x)
def  _set_global_variable(name, x): _variable_scope[name] = the(_variable, x)

def _compiler_defparameter(name, value):
        x = _variable(the(symbol_t, name), _ensure_variable_pyname(name), variable)
        _variable_scope[name] = x
        if value is not None:
                __global_scope__[name] = value

def _compiler_defvar(name, value):
        if not _find_global_variable(name):
                _compiler_defparameter(name, value)

_intern_and_bind_names_in_module("SETF")
def _compiler_defun(name: symbol_t, lambda_expression: cons_t, check_redefinition = t) -> bool:
        """Manipulate the compiler's idea of a function's definition.
           Return a boolean, which denotes whether the situation is an identity redefinition."""
        check_type(name, (or_t, symbol_t, cons_t))
        oldef = _find_global_function(name)
        if oldef and oldef.lambda_expression == lambda_expression:
                return t
        if check_redefinition and isinstance(name, symbol_t):
                if oldef and oldef.kind is macro:
                        _warn_incompatible_redefinition(name, "function", "macro")
                elif oldef and oldef.kind is function:
                        _warn_possible_redefinition(oldef.name, name)
        _set_global_function(name, _function(name, _ensure_function_pyname(name),
                                             function, lambda_expression = lambda_expression))
        return nil

def _compiler_defmacro(name, lambda_expression, check_redefinition = t):
        "Return a boolean, which denotes whether the situation is an identity redefinition."
        check_type(name, (or_t, symbol_t, list))
        oldef = _find_global_function(name)
        ## Unregistered Issue MACRO-REDEFINITION-REPLACED-BY-ONE-WITH-NONEIFIED-GLOBALS
        if oldef and oldef.lambda_expression == lambda_expression:
                return t
        if check_redefinition and isinstance(name, symbol_t):
                if oldef and oldef.kind is function:
                        _warn_incompatible_redefinition(name, "macro", "function")
                elif oldef and oldef.kind is macro:
                        _warn_possible_redefinition(oldef.name, name)
        _set_global_function(name, _function(name, _ensure_function_pyname(name),
                                             macro, lambda_expression = lambda_expression))
        return nil

def _compiler_defvar_without_actually_defvar(name, value):
        "This is for SETQ-IN-ABSENCE-OF-DEFVAR."
        if value is not None:
                __global_scope__[name] = value

def _global_variable_constant_p(name):
        var = _find_global_variable(name)
        return var and var.kind is constant

## Unregistered Issue CONSTANTNESS-PERSISTENCE
def _compiler_defconstant(name, value):
        assert(value is not None)
        if _global_variable_constant_p(name):
                error("The constant %s is being redefined (from %s to %s).", name, _variable_scope[name].value, value)
        var = _variable(the(symbol_t, name), _ensure_variable_pyname(name), constant, value = value)
        _variable_scope[name] = var

def _check_no_locally_rebound_constants(locals, use = "local variable"):
        constant_rebound = [ x for x in locals if _global_variable_constant_p(x) ]
        if constant_rebound:
                simple_program_error("%s names a defined constant, and cannot be used as a %s.", constant_rebound[0], use)

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
        return (isinstance(form, (int, float, complex, str))                       or
                keywordp(form)                                                     or
                (isinstance(form, symbol_t) and _global_variable_constant_p(form)) or
                (isinstance(form, list) and len(form) is 2 and form[0] is quote))

# Essential functions

@defun("NOT")
def not_(x):        return t if x is nil else nil

# Action: populate compilation environment with pre-defined functions

lambda_ = intern("LAMBDA")[0]
def _populate_compilation_environment_from_package(package):
        for sym in package.own:
                if sym.function:
                        _set_function_definition(globals(), sym,
                                                  lambda_expression = nil, check_redefinition = nil)(sym.function)
                value, presentp = gethash(sym, __global_scope__)
                if presentp:
                        _compiler_defvar(sym, value)

_populate_compilation_environment_from_package(__cl)

_compiler_defconstant(t,   t)
_compiler_defconstant(nil, nil)

EmptyDict = dict()
EmptySet = frozenset()

# Tracing

_trace_table = dict()
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

_untrace()

def _trace_printf(tracespec, control, *args):
        if _tracep(*(tracespec                    if isinstance(tracespec, tuple) else
                     (tracespec,)                 if isinstance(tracespec, str)   else
                     (tracespec, _caller_name(1)))):
                _debug_printf(control, *(args if not (len(args) == 1 and functionp(args[0])) else
                                         args[0]()))

__enable_livelock_detector__           = False
__livelock_detector_frames__           = _collections.defaultdict(lambda: 0)
__frame_livelock_suspicion_threshold__ = 1000
def _trace_frame():
        if not __enable_livelock_detector__:
                return
        f = _caller_frame(0)
        fid = id(f)
        __livelock_detector_frames__[fid] += 1
        if __livelock_detector_frames__[fid] > __frame_livelock_suspicion_threshold__:
                _debug_printf(" *** frame %s exceeded livelock suspicion threshold:", _frame_id(f)[:4])
                _backtrace(frame_ids = True)

def _matcher_pp(x):
        return _pp_consly(x, dispatch = { dict: lambda x: repr(list(x.keys())[0]) + "::" + _pp_consly(list(x.values())[0]) })

def _r(x, y, retval, q = "", n = 20, ignore_callers = set(["<lambda>", "complex", "simplex"]), id_frames = False):
        def pp_frame(f):
                return ("%s_%s" % (_frame_fun_name(f),
                                   _frame_id(f)[:3])) if id_frames else _frame_fun_name(f)
        def trace_args():
                frames = reversed(
                        list(_take(n, (f
                                       for f in _frames_calling(_caller_frame(2))
                                       if _frame_fun_name(f) not in ignore_callers))))
                return (":".join(pp_frame(f) for f in frames),
                        q, "<==%s  %s  %s==>" % (retval[0],
                                                 _matcher_pp(retval[1]),
                                                 _matcher_pp(retval[2])),
                        _matcher_pp(x), _matcher_pp(y))
        _trace_printf(_return, "--- %s%3s\n   %s   %s   %s", trace_args)
        return retval

__enable_matcher_tracing__ = False
_string_set("*MATCHER-DEPTH*", 0)

def _matcher_deeper(x, name = None):
        depth = symbol_value(_matcher_depth_)
        _debug_printf("%s%s    %s", " " * depth, _defaulted(name, _caller_name(1).upper()),
                      ("  -  ".join(_matcher_pp(x) for x in x)) if x else "")
        _dynamic_scope_push({ _matcher_depth_: depth + 1 })

def _make_ml(_, x = None, name = None, **args):
        o = object.__new__(_, **args)
        o.x, o.name = x, name
        return o

_match_level = _defwith("_match_level",
                        lambda self: __enable_matcher_tracing__ and _matcher_deeper(self.x, name = self.name),
                        lambda *_:   __enable_matcher_tracing__ and _dynamic_scope_pop(),
                        __new__ = _make_ml)

# Main part

_intern_and_bind_names_in_module("%NAME", "%MAYBE")
_intern_and_bind_names_in_module("%CALL", "%RETURN")
_intern_and_bind_names_in_module_specifically(
        ("_some",    "%SOME"),
        ("_or",      "%OR"),
        ("_maybe",   "%MAYBE"),
        ("ir_args",  "IR-ARGS"))

def _maybe_destructure_binding(pat):
        return ((None, pat)           if not isinstance(pat, dict) else
                tuple(pat.items())[0] if len(pat) == 1             else
                error_bad_pattern(pat))

def _error_bad_pattern(pat):
        raise Exception("Bad pattern: %s." % (pat,))

class _matcher():
        class matchod():
                def __init__(self, name, method):
                        self.name = name
                        self.method = method
                        self.cache = None
        @staticmethod
        def bind(value, bound, name, if_exists:{error, replace} = error):
                def error_bound(x):
                        raise Exception("Rebinding %s from %s to %s." % (repr(name), repr(x), repr(value)))
                def error_keyword(name, bad, allowed):
                        raise Exception("Keyword %s must be one of: %s.  Was: %s." % (repr(name), allowed, repr(bad)))
                if   name is None:            return bound
                elif (name not in bound or
                      if_exists is replace):
                        bound = dict(bound)
                        bound.update({name:value}); return bound
                elif if_exists is error:     error_bound(bound[name])
                else:                        error_keyword("if_exists", if_exists, { error, replace })
        @staticmethod
        def succ(bound, res):      return bound, res, None
        @staticmethod
        def fail(bound, exp, pat): return bound, exp, pat
        @staticmethod
        def post(x, mutator):      return (x[0], mutator(x[1]), None) if x[2] is None else x
        @staticmethod
        def post_fail(x, pat):
                # if x[2] is not None:
                #         _debug_printf("replacing fail %s ---> %s", x[2], pat)
                return x if x[2] is None else (x[0], x[1], pat)
        ###
        def test(m, test, bound, name, resf:"() -> result", exp, fail_pat,
                 if_exists:{error, replace} = error, comment = None):
                with _match_level([test] + ([comment] if comment else [])):
                        # _debug_printf("test: %s", exp)
                        # _trace_frame()
                        return (m.succ(m.bind(exp, bound, name, if_exists = if_exists), resf()) if test else
                                m.fail(bound, exp, fail_pat))
        def equo(m, name, exp, x):
                "Apply result binding, if any."
                b, r, f = x
                return ((m.bind(exp, b, name), r, f) if f is None else
                        x) # propagate failure as-is
        def crec(m, expat, l0, lR, horisontal = True, originalp = False):
                # _trace_frame()
                ## Unregistered Issue PYTHON-LACK-OF-RETURN-FROM
                b0, bR, fx0, fxR, fp0, fpR  = None, None, None, None, None, None
                def try_0():
                        nonlocal b0, fx0, fp0
                        with _match_level(name = "CAR"):
                                b0, fx0, fp0 = l0()
                        if fp0 is None: return fx0
                def try_R():
                        nonlocal bR, fxR, fpR
                        with _match_level(name = "CDR"):
                                bR, fxR, fpR = lR(b0)
                        if fpR is None: return fxR
                result = (m.comh if horisontal else
                          m.comr)(try_0, try_R, originalp)
                # _trace_printf("yield", "+++ YIELD for %s (orig: %s, call: %s->%s):\n%s",
                #               lambda: (exp, originalp, _caller_name(2), _caller_name(1), result))
                return (m.succ(bR, result)      if fp0 is None and fpR is None else
                        m.fail(*((b0, fx0, fp0) if fp0 is not None             else
                                 (bR, fxR, fpR))))
        ###
        def register_simplex_matcher(m, name, matcher):
                __metasex_words__.add(name)
                m.__simplex_patterns__[name] = m.matchod(name, matcher)
        def register_complex_matcher(m, name, matcher):
                __metasex_words__.add(name)
                m.__complex_patterns__[name] = m.matchod(name, matcher)
        def simplex_pat_p(m, x): return consp(x) and isinstance(x[0], symbol_t) and x[0] in m.__simplex_patterns__
        def complex_pat_p(m, x): return consp(x) and isinstance(x[0], symbol_t) and x[0] in m.__complex_patterns__
        def simplex(m, bound, name, exp, pat, orifst):
                # _trace_frame()
                # _trace_printf("simplex", "simplex  %s (call: %s->%s) %x  %10s  %20s\n -EE %s\n -PP %s",
                #               lambda: (pat[0], _caller_name(2), _caller_name(1), id(exp) ^ id(pat),
                #                        name, bound, exp, pat))
                mtd = m.__simplex_patterns__[pat[0]]
                # def exp_store():
                #         try:
                #                 return mtd.cache[exp]
                #         except TypeError:
                #                 pass
                # store = _without_condition_system(exp_store)
                # if store is not None:
                #         exp_pat_hit = store.get(pat)
                #         if exp_pat_hit:
                #                 return exp_pat_hit
                res = _, __, f = mtd.method(bound, name, exp, pat, orifst)
                # if store is not None and f is None:
                #         store[pat] = res
                return res
                # return m.__simplex_patterns__[pat[0][0]](bound, name, exp, pat, orifst)
        def complex(m, bound, name, exp, pat, orifst, aux, limit):
                # _trace_frame()
                # _trace_printf("complex", "complex  %s (call: %s->%s) %x  %10s  %20s\n -EE %s\n -PP %s\n -OF %s  %s  %s",
                #               lambda: (pat[0][0], _caller_name(2), _caller_name(1), id(exp) ^ id(pat),
                #                        name, bound, exp, pat, orifst, aux, limit))
                mtd = m.__complex_patterns__[pat[0][0]]
                # store = mtd.cache[id(exp)]
                # if store is not None:
                #         exp_pat_hit = store.get(id(pat))
                #         if exp_pat_hit:
                #                 return exp_pat_hit
                res = _, __, f = mtd.method(bound, name, exp, pat, orifst, aux, limit)
                # if store is not None:
                #         store[id(pat)] = res
                return res
                # return m.__complex_patterns__[pat[0][0]](bound, name, exp, pat, orifst, aux, limit)
        def initialise_cache(m):
                for mr in m.__simplex_patterns__.values():
                        mr.cache = _collections.defaultdict(dict)
                for mr in m.__complex_patterns__.values():
                        mr.cache = _collections.defaultdict(dict)
        def __init__(m):
                m.__complex_patterns__, m.__simplex_patterns__ = dict(), dict()
                m.register_complex_matcher(_some, m.segment)
                m.register_complex_matcher(_maybe, m.maybe)
                m.register_complex_matcher(_or, m.or_)
        def per_use_init(m):
                m.initialise_cache()
        def complex_matcher_not_implemented(m, bound, name, exp, pat, orifst, aux, limit):
                raise Exception("Not yet capable of matching complex patterns of type %s.", pat[0][0])
        def simplex_matcher_not_implemented(m, bound, name, exp, pat, orifst):
                raise Exception("Not yet capable of matching simplex patterns of type %s.", pat[0])
        def complex_identity(m, bound, name, exp, pat, orifst, aux, limit):
                with _match_level():
                        # _trace_frame()
                        ## Unregistered Issue IDENTITY-IGNORE-MATCHERS-COMPLEX/MATCH-USE-UNCLEAR
                        return m.complex(bound, name, exp, [pat[0][1][0], pat[1]], orifst, aux, limit)
        def simplex_identity(m, bound, name, exp, pat, orifst):
                with _match_level():
                        # _trace_frame()
                        ## Unregistered Issue IDENTITY-IGNORE-MATCHERS-COMPLEX/MATCH-USE-UNCLEAR
                        return m.simplex(bound, name, exp, pat[1][0], orifst)
        def ignore(m, bound, name, exp, pat, orifst, aux, limit):
                with _match_level():
                        return m.match(bound, name, exp, pat[1], (False, False), aux, limit)
        ###
        # Methods to be implemented:
        # 
        # def forc(f0, fR, originalp)
        # def prod(exp, originalp)
        # def nonliteral_atom_p(exp) -- XXX: abstraction leak!
        ###
        def segment(m, bound, name, exp, pat, orifst, aux, limit, end = None):
                # _trace_frame()
                def posn(x, xs):
                        pos, i = xs, 0
                        while pos:
                                if x == pos[0]: return i
                                pos = pos[1]
                def constant_pat_p(pat):
                        def nonconstant_pat_p(x): return consp(x) or m.nonliteral_atom_p(x)
                        return not nonconstant_pat_p(tuple(pat.items())[0][1] if isinstance(pat, dict) else
                                                     pat)
                ## Unregistered Issue PYTHON-DESTRUCTURING-WORSE-THAN-USELESS-DUE-TO-NEEDLESS-COERCION
                ## 1. Destructure the pattern, deduce the situation.
                seg_pat, rest_pat = pat[0][1], pat[1]
                firstp = aux is None
                ## 2. Memoize the tuple being iteratively matched upon.
                ## Unregistered Issue TRY-SIMPLIFY-OUT-AUX
                aux = append(seg_pat, list_(pat[0])) if aux is None else aux
                _trace_printf("segment", "segment  %x  %10s  %20s\n -EE %s\n -PP %s\n -OF %s  firstp:%s newaux:%s limit:%s",
                              lambda: (id(exp) ^ id(pat), name, bound,
                                       _matcher_pp(exp), _matcher_pp(pat), orifst, firstp, _matcher_pp(aux), limit))
                ## 3. (Re-)establish and check the boundary.
                exlen = length(exp)
                end = (end                    if end is not None                          else
                       posn(rest_pat[0], exp) if rest_pat and constant_pat_p(rest_pat[0]) else
                       0)
                if ((end and end > exlen) or ## All legitimate splits failed.
                    end is None):            ## A constant pattern was missing, likewise.
                        # _debug_printf("   FAILing seg: %s  -at-  %s,   end:%s exlen:%s,    --  %s",
                        #               _matcher_pp(pat), _matcher_pp(exp), end, exlen,
                        #               "(end and end > exlen)" if (end and end > exlen) else
                        #               "end is None"           if end is None           else
                        #               error("INTERNAL ERROR"))
                        return m.fail(bound, exp, pat)
                ## 4. Compute the relevant part of the expression -- success is when this part reduces to ().
                cut = end if rest_pat else exlen
                seg_exp, rest_exp = (subseq(exp, 0, cut),
                                     exp if cut is None else subseq(exp, cut))
                with _match_level([end, seg_exp, cut, rest_exp, pat, exp]):
                        ## 5. Try match at the chosen split -- the rest part first, then the segment part.
                        brf_0 = m.crec([exp, pat],
                                       lambda:
                                               ((lambda seg_bound, seg_r, seg_fail_pat, comment:
                                                         m.test(seg_fail_pat is None, seg_bound, name, (lambda: seg_r), seg_exp, seg_fail_pat,
                                                                if_exists = replace, comment = comment))
                                                (*(## Test for success -- segment piece exhausted.
                                                   m.succ(m.bind(nil, bound, name), m.prod(nil, orifst[0]))
                                                   + ("segment exhausted",)           if seg_exp is nil else
                                                   ## Test specific for bounded matching.
                                                   m.fail(bound, exp, pat)
                                                   + ("segment match limit reached",) if limit == 0     else
                                                   ## Try biting one more iteration off seg_exp:
                                                   m.match(bound, name,  seg_exp,     aux,  (False,
                                                                                             firstp), aux, limit - 1)
                                                   + ("try match car",)))),
                                       lambda seg_bound:
                                               m.match(seg_bound, None, rest_exp, rest_pat,  (False, False), None, -1),
                                       originalp = firstp and orifst[0] and seg_exp is not nil,
                                       horisontal = t)
                        if brf_0[2] is None:
                                return m.succ(brf_0[0], brf_0[1])
                ## 6. Alternate length attempts.
                brf_1 = m.segment(bound, name, exp, pat, orifst, None, limit, end + 1)
                if brf_1[2] is None:
                        return m.succ(brf_1[0], brf_1[1])
                return m.fail(brf_0[0], *((brf_0[1], brf_0[2]) if brf_1[2] == pat else
                                          (brf_1[1], brf_1[2])))
        def maybe(m, bound, name, exp, pat, orifst, aux, limit):
                # _trace_frame()
                ## The semantics of aux are painfully unclear here:
                ##  - we need to perform aux pass-through, for any potential surrounding segment match
                ##  - we need a clean slate for this segment..
                ## So, do we need a separate stack for aux here?  Sounds like an in inevitability..
                ## Unregistered Issue SEGMENT-MATCH-USERS-REQUIRE-AUX-DOMAIN-SEPARATION
                with _match_level():
                        return m.segment(bound, name, exp, [[_some, pat[0][1]], pat[1]], orifst, None, 1)
        def or_(m, bound, name, exp, pat, orifst, aux, limit):
                # _trace_frame()
                alternatives = pat[0][1]
                def fail(): return m.fail(bound, exp, pat[0])
                def rec(current, other_options):
                        with _match_level([[current, pat[1]], exp], name = "OR"):
                                brf_0 = m.match(bound, name, exp, [current, pat[1]], orifst, None, -1)
                        if brf_0[2] is None:
                                return m.succ(brf_0[0], brf_0[1])
                        brf_1 = (fail() if not other_options else
                                 rec(other_options[0], other_options[1]))
                        if brf_1[2] is None:
                                return m.succ(brf_1[0], brf_1[1])
                        return m.fail(brf_0[0], exp, pat)
                return (fail() if not alternatives else
                        m.post_fail(rec(alternatives[0], alternatives[1]),
                                    pat[0]))
        ## About the vzy33c0's idea:
        ## type-driven variable naming is not good enough, because:
        ## 0. mostly is already done
        ## 1. type narrows down the case analysis chain (of which there is a lot)
        ## 2. expressions also need typing..
        def match(m, bound, name, exp, pat, orifst, aux, limit):
                # _trace_frame()
                def maybe_get0Rname(pat):
                        ## Unregistered Issue PYTHON-DESTRUCTURING-WORSE-THAN-USELESS-DUE-TO-NEEDLESS-COERCION
                        (name, pat0), patR = _maybe_destructure_binding(pat[0]), pat[1]
                        return (name, pat0, name and m.simplex_pat_p(pat0), patR,
                                ([pat0, patR] if name is not None else
                                 pat)) ## Attempt to avoid consing..
                ## I just caught myself feeling so comfortable thinking about life matters,
                ## while staring at a screenful of code.  In "real" life I'd be pressed by
                ## the acute sense of time being wasted..
                atomp, null = atom(pat), pat is nil
                def pp_binding(x):
                        return repr(list(x.keys())[0]) + "::" + _pp_consly(list(x.values())[0])
                # _trace_printf("match", "       _match  %x  %10s  %20s\n -EE %s\n -PP %s\n -OF %s  %s  %s, atomp: %s, null: %s, complexp: %s, simplexp: %s",
                #               lambda: (id(exp) ^ id(pat), name, bound, exp, pat, orifst, aux, limit,
                #                        atomp, null,
                #                        (not (atom or null)) and m.complex_pat_p(pat[0]),
                #                        (not (atom or null)) and m.simplex_pat_p(pat[0])))
                with _match_level([exp, pat]):
                 return \
                     (m.test((m.atom(exp, pat) if atomp else
                              exp is nil),
                             bound, name, lambda: m.prod(exp, orifst[0]), exp, pat)         if atomp or null        else
                      m.simplex(bound, name, exp,  pat, (consp(exp),
                                                          orifst[1]))                       if m.simplex_pat_p(pat) else
                      m.complex(bound, name, list_(exp), list_(pat), orifst, None, limit)   if m.complex_pat_p(pat) else
                      (lambda pat0name, pat0, pat0simplexp, patR, clean_pat:
                               (m.equo(name, exp,
                                       m.complex(bound, pat0name, exp, clean_pat, orifst, aux, limit))
                                                                 if m.complex_pat_p(pat0) else
                                m.fail(bound, exp, pat)          if not consp(exp)        else
                                m.equo(name, exp,
                                       m.crec([exp, pat],
                                              lambda:        m.match(bound, pat0name, exp[0], pat0, (listp(exp[0]),
                                                                                                     orifst[1]),
                                                                     None, -1),
                                              (lambda b0und: m.match(b0und, None,     exp[1], patR, (False, orifst[1]), aux, limit)),
                                              originalp = orifst[0],
                                              horisontal = nil))))
                      (*maybe_get0Rname(pat)))

def _match(matcher, exp, pat):
        matcher.per_use_init()
        name, prepped = _maybe_destructure_binding(pat)
        return matcher.match(dict(), name, exp, prepped, (True, False), None, -1)

# Compiler conditions

@defclass
class _compiler_error(error_t):
        pass

@defclass
class _simple_compiler_error(simple_condition_t, _compiler_error):
        pass

@defun
def _compiler_error(control, *args):
        return _simple_compiler_error(control, *args)

# Form parsing

_intern_and_bind_names_in_module("DECLARE")
def _parse_body(body, doc_string_allowed = t):
        doc = nil
        def doc_string_p(x, remaining_forms):
                return ((error("duplicate doc string %s", x) if doc else t)
                        if isinstance(x, str) and doc_string_allowed and remaining_forms else
                        None)
        def declaration_p(x):
                return isinstance(x, tuple) and x[0] is declare
        decls, forms = [], []
        for i, form in enumerate(body):
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

_eval_when_ordered_keywords = _compile_toplevel, _load_toplevel, _execute
_eval_when_keywords = set(_eval_when_ordered_keywords)
def _parse_eval_when_situations(situ_form):
        if not (listp(situ_form) and not (set(_vectorise_linear(situ_form)) - _eval_when_keywords)):
                error("In EVAL-WHEN: the first form must be a list of following keywords: %s.", _eval_when_ordered_keywords)
        return [x in situ_form for x in _eval_when_ordered_keywords]

def _analyse_eval_when_situations(compile_time_too, ct, lt, e):
        "Implement the EVAL-WHEN chart of section #5 of CLHS 3.2.3.1."
        process = lt
        eval = ct or (compile_time_too and e)
        new_compile_time_too = lt and eval
        return new_compile_time_too, process, eval

def _process_decls(decls, vars, fvars):
        _warn_not_implemented()

def _self_evaluating_form_p(x):
        return isinstance(x, (int, str, float)) or x in [t, nil]

# Debugging, tracing and pretty-printing

_compiler_max_mockup_level = 3

_string_set("*COMPILER-TRACE-TOPLEVELS*",          nil)
_string_set("*COMPILER-TRACE-TOPLEVELS-DISASM*",   nil)
_string_set("*COMPILER-TRACE-ENTRY-FORMS*",        nil)
_string_set("*COMPILER-TRACE-MACROEXPANSION*",     nil)
_string_set("*COMPILER-TRACE-TRUE-DEATH-OF-CODE*", nil)

_string_set("*COMPILER-TRACE-FORMS*",              nil)
_string_set("*COMPILER-TRACE-PRIMITIVES*",         nil)
_string_set("*COMPILER-TRACE-SUBFORMS*",           nil)
_string_set("*COMPILER-TRACE-REWRITES*",           nil)
_string_set("*COMPILER-TRACE-CHOICES*",            nil)
_string_set("*COMPILER-TRACE-RESULT*",             nil)
_string_set("*COMPILER-TRACE-PRETTY-FULL*",        nil)

_string_set("*COMPILER-DEBUG-P*",                  nil)
_string_set("*COMPILER-TRAPPED-FUNCTIONS*",        set()) ## Emit a debug entry for those functions.

__known_trace_args__ = {"toplevels", "entry_forms", "macroexpansion",
                        "forms", "subforms", "rewrites", "choices", "result", "pretty_full"}

def _compiler_explain_tracing():
        def control_var_name(x): return "*COMPILER-TRACE-%s*" % x.replace("_", "-").upper()
        _debug_printf(";;  compiler trace config:")
        for var in __known_trace_args__:
                _debug_printf(";;    %s: %s", control_var_name(var), symbol_value(find_symbol(control_var_name(var))[0]))

def _compiler_config_tracing(**keys):
        def control_var_name(x): return "*COMPILER-TRACE-%s*" % x.replace("_", "-").upper()
        for namespec, value in keys.items():
                _string_set(control_var_name(namespec), value)

_string_set("*PP-BASE-DEPTH*", 0)
_string_set("*PP-DEPTH*", 0)
def _pp_base_depth(): return _symbol_value(_pp_base_depth_)
def _pp_depth():      return _symbol_value(_pp_depth_)

def _sex_space(delta = None, char = " "):
        return char * (_pp_base_depth() + _defaulted(delta, 0))
def _sex_deeper(n, body):
        with progv({ _pp_base_depth_: _pp_base_depth() + n }):
                return body()

def _pp(x, **args):
        return (_pp_sex if symbol_value(_compiler_trace_pretty_full_) else _mockup_sex)(x, **args)

## Compiler messages:
## - entry        _lower:rec()                             ;* lowering
##                _debug_printf(";;;%s lowering:\n%s%s", _sex_space(-3, ";"), _sex_space(), _pp_sex(x))
## - part listing _lower:call_known()                       >>> parts
## - rewriting    _lower:call_known()                       ===\n---\n...
## - result       _lower()                                 ;* compilation atree output\n;;; Prologue\n;;; Value

def _compiler_trap_function(name):
        symbol_value(_compiler_trapped_functions_).add(name)

def _compiler_function_trapped_p(name):
        return name in symbol_value(_compiler_trapped_functions_)

def _debug_compiler(value = t):
        _string_set("*COMPILER-DEBUG-P*", value, force_toplevel = t)
def _debugging_compiler():
        return _symbol_value(_compiler_debug_p_)
def _maybe_disable_debugging(self):
        self.was_debugging = _debugging_compiler()
        _debug_compiler(nil)
def _maybe_reenable_debugging(self, *_):
        _debug_compiler(self.was_debugging)
_no_compiler_debugging = _defwith("_no_compiler_debugging", _maybe_disable_debugging, _maybe_reenable_debugging)

_compiler_debug        = _defwith("_compiler_debug",
                                  lambda *_: _dynamic_scope_push({ _compiler_debug_p_: t }),
                                  lambda *_: _dynamic_scope_pop())

def _compiler_debug_printf(control, *args):
        if _debugging_compiler():
                justification = _sex_space()
                def fix_string(x): return x.replace("\n", "\n" + justification) if isinstance(x, str) else x
                _debug_printf(justification + fix_string(control), *tuple(fix_string(a) for a in args))

def _compiler_trace_choice(ir_name, id, choice):
        if symbol_value(_compiler_trace_choices_):
                _debug_printf("%s-- %s %s: %s", _sex_space(), ir_name, _ir_minify(id), choice)

if probe_file("/home/deepfire/.partus-debug-compiler"):
        _debug_compiler()

# General

_intern_and_bind_names_in_module(
        "*COMPILER-ERROR-COUNT*",
        "*COMPILER-WARNINGS-COUNT*",
        "*COMPILER-STYLE-WARNINGS-COUNT*",
        "*COMPILER-NOTE-COUNT*",
        "*UNDEFINED-WARNINGS*",
        ##
        "*UNIT-FUNCTIONS*",
        "*UNIT-SYMBOLS*",
        "*UNIT-GFUNS*",
        "*UNIT-GVARS*",
        ##
        "*TOP-COMPILATION-UNIT-P*")

# Tail position

## Should, probably, be bound by the compiler itself.
_string_set("*COMPILER-TAILP*", nil)

def _tail_position_p(): return _symbol_value(_compiler_tailp_)

_tail_position          = _defwith("_tail_position",
                                   lambda *_: _dynamic_scope_push({ _compiler_tailp_: t }),
                                   lambda *_: _dynamic_scope_pop())
_maybe_tail_position    = _defwith("_maybe_tail_position", # This is just a documentation feature.
                                   lambda *_: None,
                                   lambda *_: None)
_no_tail_position       = _defwith("_no_tail_position",
                                   lambda *_: _dynamic_scope_push({ _compiler_tailp_: nil }),
                                   lambda *_: _dynamic_scope_pop())

# Functions

_string_set("*COMPILER-FN*",     nil)
_string_set("*COMPILER-LAMBDA*", nil)

## Critical Issue COALESCE-FNS-WITH-FUNCTION-SCOPE
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
                check_type(arglist, tuple)
                args_types = tuple(t for _ in arglist)
                check_type(name, symbol_t)
                if name in _fns:
                        error("Asked to overwrite FN record %s.", name)
                _attrify_args(self, locals(), "name",
                              "arglist", "args_types", "values_types",
                              "effects", "affected")
                self.dependents, self.dependencies = (make_hash_table(default_constructor = set),
                                                      make_hash_table(default_constructor = set))
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

def _find_fn(name):
        return _fns.get(name, nil)

def _depend_on(fn_or_name, reason = t):
        fn = fn_or_name if isinstance(fn_or_name, _cold_function_type) else _find_fn(fn_or_name)
        fn.add_dependent(reason, symbol_value(_compiler_fn_))

def _ir_depending_on_function_properties(function_form, body, *prop_test_pairs):
        ## TODO: we should depend for the de-pessimisation sense likewise.
        ## ..or should we?  I think, at least for rechecking of conditions.. which is only possible
        ## in case of full recompilation..
        ##
        ## ..And we didn't even start to consider dependency loops..
        if symbolp(function_form):
                fn = _find_fn(function_form)
                if fn:
                        prop_vals = []
                        for prop_test in prop_test_pairs:
                                prop, test = (prop_test if isinstance(prop_test, tuple) else
                                              (prop_test, constantly(t)))
                                val = getattr(fn, prop)
                                if val is None or not test(val):
                                        return None
                                prop_vals.append(val)
                        for prop, _ in prop_test_pairs:
                                _depend_on(fn, prop)
                        return body(fn, *prop_vals)
        return None

# Symbols

_string_set("*IN-COMPILATION-UNIT*", nil)

def _unit_function(x):
        symbol_value(_unit_functions_).add(x)
        return x
def _unit_symbol(x):
        symbol_value(_unit_symbols_).add(x)
        return x
def _unit_variable_pyname(x):
        return _frost.full_symbol_name_python_name(x)
def _unit_function_pyname(x):
        symbol_value(_unit_functions_).add(x)
        return _ensure_function_pyname(x)
def _unit_symbol_pyname(x):
        symbol_value(_unit_symbols_).add(x)
        return _ensure_symbol_pyname(x)

def _unit_note_gfun_reference(x):
        symbol_value(_unit_gfuns_).add(x)
def _unit_note_gvar_reference(x):
        symbol_value(_unit_gvars_).add(x)

def _compilation_unit_symbols():
        """Return a function names and plain symbols, referred by the current compilation unit."""
        return (symbol_value(_unit_functions_),
                symbol_value(_unit_symbols_),
                symbol_value(_unit_gfuns_),
                symbol_value(_unit_gvars_))

def _compilation_unit_adjoin_symbols(funs, syms, gfuns, gvars):
        symbol_value(_unit_functions_).update(funs)
        symbol_value(_unit_symbols_).update(syms)
        symbol_value(_unit_gfuns_).update(gfuns)
        symbol_value(_unit_gvars_).update(gvars)

_string_set("*COMPILATION-UNIT-ID*", nil)

def with_compilation_unit(fn, override = nil, id = "UNIT-"):
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
                # _warn_not_implemented()
                ...
        succeeded_p = nil
        if _symbol_value(_in_compilation_unit_) and not override:
                try:
                        with progv({_top_compilation_unit_p_: nil}):
                                ret = fn()
                        succeeded_p = t
                        return ret
                finally:
                        ...
        else:
                parent_id = symbol_value(_compilation_unit_id_)
                id = gensym(id)
                with progv({_compiler_error_count_: 0,
                            _compiler_warnings_count_: 0,
                            _compiler_style_warnings_count_: 0,
                            _compiler_note_count_: 0,
                            _undefined_warnings_: nil,
                            ##
                            _unit_functions_: set(),
                            _unit_symbols_: set(),
                            _unit_gfuns_: set(),
                            _unit_gvars_: set(),
                            ##
                            _in_compilation_unit_: t,
                            _top_compilation_unit_p_: t,
                            _compilation_unit_id_: id,
                            }):
                        try:
                                # _debug_printf("############################################ Entered %s%s",
                                #               id, (" parent: %s" % parent_id) if parent_id else "")
                                ret = fn()
                                succeeded_p = t
                                return ret
                        finally:
                                summarize_compilation_unit(not succeeded_p)
                                # _debug_printf("############################################  ..left %s", id)

def _compilation_unit_prologue(funs, syms, gfuns, gvars):
        """Emit a prologue for a standalone unit referring to SYMBOLS."""
        def import_prologue():
                return _emit_ast(p.import_(p.name("cl"))) 
        def symbol_prologue():
                def wrap(x):
                        return _defaulted(x, _consify_star(ref, (quote, ("None",))))
                with progv({ _compiler_trace_pretty_full_: nil,
                             _compiler_trace_forms_:       nil,
                             _compiler_trace_primitives_:  nil,
                             _compiler_trace_choices_:     nil,
                             _compiler_trace_subforms_:    nil,
                             _compiler_trace_rewrites_:    nil,
                             _compiler_trace_result_:      nil }):
                 symbols = sorted(funs | syms, key = str)
                 return _lower(list_(progn,
                                     _ir_cl_module_call(
                                         "_fop_make_symbol_available",
                                         _ir_funcall("globals"),
                                         "COMMON-LISP", "VECTOR", _ensure_function_pyname(__vector),
                                         list_(ref, list_(quote, list_("None"))),
                                         True, False),
                                     _ir_cl_module_call(
                                         "_fop_make_symbols_available",
                                         _ir_funcall("globals"),
                                         _ir_funcall(__vector, *tuple(package_name(symbol_package(sym)) if symbol_package(sym) else (ref, (quote, ("None",)))
                                                                      for sym in symbols )),
                                         _ir_funcall(__vector, *tuple(symbol_name(sym)          for sym in symbols )),
                                         _ir_funcall(__vector, *tuple(wrap(sym.function_pyname) for sym in symbols )),
                                         _ir_funcall(__vector, *tuple(wrap(sym.symbol_pyname)   for sym in symbols )),
                                         _ir_funcall(__vector, *tuple(sym in gfuns              for sym in symbols )),
                                         _ir_funcall(__vector, *tuple(sym in gvars              for sym in symbols )))),
                               ## Beacon LEXENV-CLAMBDA-IS-NIL-HERE
                               lexenv = _make_null_lexenv(nil))
        with _no_compiler_debugging():
                return (import_prologue() +
                        symbol_prologue())

# Code

_intern_and_bind_names_in_module("*LEXENV*", "NULL")

def _coerce_to_lexenv(x):
        return (nil if x is null else
                (x or _symbol_value(_lexenv_)))

def _with_lexenv_frame(frame, fn):
        with progv({ _lexenv_: frame }):
                return fn()

@defclass(intern("%LEXENV")[0])
class _lexenv():
        """Chains variable and function scope pieces together.  Scope pieces map binding kinds
           to binding sets and bound names to bindings."""
        clambda = nil
        varscope, funcscope, blockscope, gotagscope = nil, nil, nil, nil
        varframe, funcframe, blockframe, gotagframe = nil, nil, nil, nil
        def __repr__(self):
                return "#<lexenv vars: %s, funs: %s, blks: %s, gotags: %s>" % (self.varscope,   self.funcscope,
                                                                               self.blockscope, self.gotagscope)
        def __init__(self, clambda, parent = nil,
                     name_varframe = None, name_funcframe = None, name_blockframe = None, name_gotagframe = None,
                     kind_varframe = None, kind_funcframe = None, kind_blockframe = None, kind_gotagframe = None,
                     full_varframe = None, full_funcframe = None, full_blockframe = None, full_gotagframe = None):
                # for k, v in self.data.items():
                #         symbolp(k)   or error("Lexenv scope keys must be symbols, found: %s.",    k.__repr__())
                #         _bindingp(k) or error("Lexenv scopt values must be bindings, found: %s.", v.__repr__())
                def complete_name_frame(framespec):
                        res = _collections.defaultdict(set)
                        res["name"] = framespec
                        for binding in framespec.values():
                                res[binding.kind].add(binding)
                        return res
                def complete_kind_frame(framespec):
                        res = dict(framespec)
                        res["name"] = names = dict()
                        for set_ in framespec.values():
                                for binding in set_:
                                        names[binding.name] = binding
                        return res
                def complete_frame(name, kind, full):
                        return (full                      if full else
                                complete_name_frame(name) if name else
                                complete_kind_frame(kind) if kind else
                                None)
                self.clambda  = clambda ## The compiler lambda.
                self.parent   = _coerce_to_lexenv(parent)
                ((self.varscope,   self.varframe),
                 (self.funcscope,  self.funcframe),
                 (self.blockscope, self.blockframe),
                 (self.gotagscope, self.gotagframe)
                 ) = (self.adjoin_scope(parent,  "varscope",
                                        complete_frame(name_varframe,   kind_varframe,   full_varframe)),
                      self.adjoin_scope(parent, "funcscope",
                                        complete_frame(name_funcframe,  kind_funcframe,  full_funcframe)),
                      self.adjoin_scope(parent, "blockscope",
                                        complete_frame(name_blockframe, kind_blockframe, full_blockframe)),
                      self.adjoin_scope(parent, "gotagscope",
                                        complete_frame(name_gotagframe, kind_gotagframe, full_gotagframe)))
        def adjoin_scope(self, parent_lexenv, sname, frame):
                parent_lexenv = _coerce_to_lexenv(parent_lexenv)
                pscope = getattr(parent_lexenv, sname) if parent_lexenv else nil
                return (pscope if not frame else
                        (frame,
                         (nil if parent_lexenv is nil else pscope),
                         self)
                        ), frame
        @staticmethod
        def merge_frames(f0, f1):
                res = dict(f0)
                for key, map in f1.items():
                        res[key] = (_dictappend(res[key], map) if key in res else
                                    map)
                return res
        # def appended(self, clambda, added):
        #         if not added:
        #                 return self
        #         assert(self.parent is added.parent)
        #         names = [ "var", "func", "block", "gotag" ]
        #         frames = [ self.merge_frames(getattr(self,  name + "frame") or dict(),
        #                                      getattr(added, name + "frame") or dict()) for name in names ]
        #         parent = self.parent
        #         result = _lexenv(clambda)
        #         for name, frame in zip(names, frames):
        #                 setattr(result, name + "frame", frame)
        #                 setattr(result, name + "scope", self.adjoin_scope(parent, name + "scope", frame)[0])
        #         return result
        @staticmethod
        def do_lookup_scope(scope, x, default):
                while scope:
                        frame, rest, lexenv = scope
                        if not isinstance(frame, dict):
                                _debug_printf("bad scope: %s", scope)
                        if x in frame["name"]:
                                return frame["name"][x], lexenv
                        scope = rest # COLD-CDR
                return default, None
        def lookup_func(self, x, default = None):         return self.do_lookup_scope(self.funcscope, x, default)
        def lookup_var(self, x, default = None):          return self.do_lookup_scope(self.varscope, x, default)
        def lookup_block(self, x, default = None):        return self.do_lookup_scope(self.blockscope, x, default)
        def lookup_gotag(self, x, default = None):        return self.do_lookup_scope(self.gotagscope, x, default)
        def funcscope_binds_p(self, x):   return self.lookup_func(x)[0]  is not None
        def varscope_binds_p(self, x):    return self.lookup_var(x)[0]   is not None
        def blockscope_binds_p(self, x):  return self.lookup_block(x)[0] is not None
        def gotagscope_binds_p(self, x):  return self.lookup_gotag(x)[0] is not None
        def lookup_func_kind(self, kind, x, default = None):
                b = self.do_lookup_scope(self.funcscope, x, None)[0]
                return (b and b.kind is kind and b) or default
        def lookup_var_kind(self, kind, x, default = None):
                b = self.do_lookup_scope(self.varscope, x, None)[0]
                return (b and b.kind is kind and b) or default

def _lexenvp(x):                return isinstance(x, _lexenv)
def _make_null_lexenv(clambda): return make_instance(_lexenv, clambda = clambda, parent = null)
def _make_lexenv(clambda, parent = nil, **initial_content):
        """ :PARENT - NULL for a null lexenv, nil for the value of *LEXENV*.
            :{NAME,KIND,FULL}-{VAR,FUNC,BLOCK}FRAME - constituents."""
        return _lexenv(clambda, parent, **initial_content)

def _make_lexenv_varframe(clambda, tns, names, forms = _repeat(None)):
        return _make_lexenv(clambda,
                            kind_varframe  = { variable: { _variable_binding(sym, tn, variable, form)
                                                           for tn, sym, form in zip(tns, names, forms) } })

def _make_lexenv_funcframe(clambda, tns, bindings):
        return _make_lexenv(clambda,
                            kind_funcframe = { function: { _function_binding(sym, tn, function, _fn(sym, clam.lambda_list))
                                                           for tn, (sym, clam) in zip(tns, bindings) } })

# Target environment setup machinery

## Unregistered Issue SEPARATE-COMPILATION-IN-FACE-OF-NAME-MAPS

### Global compiler state carry-over, and module state initialisation.
def _fop_make_symbol_available(globals, package_name, symbol_name,
                               function_pyname, symbol_pyname,
                               gfunp, gvarp):
        symbol = (intern(symbol_name, find_package(package_name))[0] if package_name is not None else
                  make_symbol(symbol_name))
        if function_pyname is not None:
                symbol.function_pyname = function_pyname
                # _debug_printf("   c-t %%U-S-G-F-P %s FUN: %s  - %s, %s %s",
                #               "G" if gfunp else "l", symbol_name, function_pyname, symbol.function, symbol.macro_function)
                if gfunp:
                        globals[function_pyname] = (symbol_function(symbol)
                                                    if symbol.function or symbol.macro_function else
                                                    ## It is a valid situation, when the function is not defined at
                                                    ## the beginning of load-time for a given compilation unit.
                                                    lambda *_, **__: error("Function not defined: %s.", symbol))
        if gvarp:
                if _find_global_variable(symbol):
                        value = symbol_value(symbol)
                        assert(value is not None)
                        globals[_unit_variable_pyname(symbol)] = value
                else:
                        ## Unregistered Issue UNDEFINED-GLOBAL-REFERENCE-ERROR-DETECTION
                        pass # This will fail obscurely.
        if symbol_pyname is not None:
                symbol.symbol_pyname = symbol_pyname
                globals[symbol_pyname] = symbol

def _fop_make_symbols_available(globals, package_names, symbol_names,
                                function_pynames, symbol_pynames,
                                gfunps, gvarps):
        for fop_msa_args in zip(package_names, symbol_names,
                                function_pynames, symbol_pynames,
                                gfunps, gvarps):
                _fop_make_symbol_available(globals, *fop_msa_args)

# Primitive IR

import primitives as p

def _defaulting_expr(name, default):
        return p.if_(p.eq(name, p.name("None")), default, name)

def _var_tn(sym):
        return p.name(_unit_variable_pyname(sym))

def _var_tn_no_unit(sym):
        return p.name(_ensure_variable_pyname(sym))

def _var_tns(syms):
        return [ _var_tn(x) for x in syms ]

def _fun_tn(sym):
        return p.name(_unit_function_pyname(sym))

def _fun_tn_no_unit(sym):
        return p.name(_ensure_function_pyname(sym))

def _gensym_tn(x = "G"):
        sym = gensym(x)
        sym.tn = p.name(_unit_variable_pyname(sym))
        return sym

def _variable_frame_bindings(clambda, names, forms = _repeat(None)):
        tns   = [ p.name(_unit_variable_pyname(sym))  for sym in names ]
        frame = _make_lexenv_varframe(clambda, tns, names, forms)
        return tns, frame

def _function_frame_bindings(clambda, bindings):
        names = list(zip(*bindings))[0]
        tns   = [ p.name(_unit_function_pyname(sym))  for sym in names ]
        frame = _make_lexenv_funcframe(clambda, tns, bindings)
        return tns, frame

__primitiviser_map__ = { str:        (nil, p.string),
                         int:        (nil, p.integer),
                         float:      (nil, p.float_num),
                         ## Note: this relies on the corresponding name to be made available by some means.
                         symbol_t:   (nil, lambda x: p.symbol(_unit_symbol_pyname(x))),
                         bool:       (nil, lambda x: p.name("True" if x else "False"))
                         }

def _primitivisable_p(x):
        type = type_of(x)
        type_recipe, _ = gethash(type, __primitiviser_map__)
        if not type_recipe:
                return nil
        recursep, _ = type_recipe
        if not recursep:
                return t
        return all(_primitivisable_p(x) for x in x)

def _try_primitivise_constant(x):
        "It's more efficient to try doing it, than to do a complete check and then to 'try' again."
        (rec, primitiviser), primitivisablep = gethash(type_of(x), __primitiviser_map__,
                                                       ((nil, nil), nil))
        if not primitivisablep:
                return None, None
        if rec: ## Dead code.
                prim, successp = _try_primitivise_list(x)
                return (primitiviser(prim), t) if successp else (None, None)
        return primitiviser(prim if rec else x), t

def _primitivise_constant(x):
        prim, successp = _try_primitivise_constant(x)
        return (prim if successp else
                error("Cannot primitivise value %s.  Is it a literal?", _pp_consly(x)))

def _ir_funcall(func, *args):
        l, l_ = list_, list__
        return l_(apply, l(function, (l(quote, l(func)) if isinstance(func, str) else
                                      func)),
                  _consify_linear(args, l(l(quote, nil))))

def _ir_cl_module_name(name):
        return list_("cl", name)

def _ir_cl_module_call(name, *ir_args):
        return _ir_funcall(list_(quote, _ir_cl_module_name(name)), *ir_args)

def _ir_lambda_to_def(name, lambda_expression):
        return _ir_args(lambda_, the(symbol_t, name), *_vectorise_linear(lambda_expression[1]),
                        name = name)

# Matcher: MetaSEX preprocessing

_intern_and_bind_names_in_module_specifically(
        ("_newline",                "%NEWLINE"),
        ("_indent",                 "%INDENT"),
        ("_for_matchers_xform",     "%FOR-MATCHERS-XFORM"),
        ("_for_not_matchers_xform", "%FOR-NOT-MATCHERS-XFORM"))

__metasex_words__      = set() ## Populated by _register_*_matcher()
__metasex_leaf_words__ = { _newline, _indent, _for_matchers_xform, _for_not_matchers_xform }

def _metasex_word_p(x):      return isinstance(x, symbol_t) and x in __metasex_words__
def _metasex_leaf_word_p(x): return isinstance(x, symbol_t) and x in __metasex_leaf_words__

def _preprocess_metasex(pat):
        "Expand syntactic sugars."
        def rec(pat):
                def prep_binding(b):
                        k, v = tuple(b.items())[0]
                        return { k: _consify(rec(v)) }
                return ((_count_scope,
                         (_some,) + tuple(rec(x) for x in pat)) if isinstance(pat, list)                   else
                        prep_binding(pat)                       if isinstance(pat, dict)                   else
                        (_form,)                                if pat is _form                            else
                        (_newline, 0)                           if pat == "\n"                             else
                        (_newline, pat)                         if isinstance(pat, int)                    else
                        (_indent, 1)                            if pat == " "                              else
                        pat                                     if not isinstance(pat, tuple) or pat == () else
                        (((identity if _metasex_word_p(pat[0])      else rec)(pat[0]),)
                         + (pat[1:] if _metasex_leaf_word_p(pat[0]) else tuple(rec(x) for x in pat[1:]))))
        # _here("\n ==> %s   -   %s", pat, ret, callers = 15)
        return _consify(rec(pat))

def _strip_metasex_pp(x):
        ret = mapcon(lambda xs:
                             (list_(xs[0])                          if not consp(xs[0])                                 else
                              list_(_strip_metasex_pp(xs[0][1][0])) if xs[0][0] is _count_scope                         else
                              list_(_strip_metasex_pp(xs[0]))       if (not symbolp(xs[0][0])
                                                                        or xs[0][0] not in (_newline, _indent,
                                                                                            _lead, _notlead, _nottail)) else
                               nil),
                      x)
        # _debug_printf("STRIP: %s\n--->\n%s", _pp_consly(x), _pp_consly(ret))
        return ret

# DEFKNOWN

_known = _poor_man_defstruct("known",
                             "name",
                             "metasex",
                             "metasex_pp",
                             "nvalues", "nth_value",
                             "binds",
                             "prologuep",
                             "lower",
                             "effects", "affected",
                             "lower_params")

def _compute_default_known_metasex(name):
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
                _frost.setf_global(sym, pyname, globals = globals())
                args_ast, body_asts = _function_ast(fn)
                body, documentation, declarations = _defbody_parse_ast(set(), body_asts)
                def meth(name, body, *args):
                        return (name, lambda *_: _py.compile(body % args, "", "exec",
                                                             flags = _ast.PyCF_ONLY_AST).body[0])
                [prologuep,
                 lower,
                 binds,
                 nvalues, nth_value,
                 effects, affected,
                ] = _defbody_methods("DEFKNOWN " + str(sym), body, lambda method_name: pyname + "_" + method_name,
                                     ["prologuep",
                                      "lower",
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
                metasex = _preprocess_metasex(metasex)
                sym.known = _known(name = sym,
                                   metasex = _strip_metasex_pp(metasex),
                                   metasex_pp = metasex,
                                   binds = binds,
                                   nvalues = nvalues, nth_value = nth_value,
                                   prologuep = prologuep,
                                   lower = lower,
                                   effects = effects, affected = affected,
                                   lower_params = _mapset(lambda x: x[0], lower_key_args))
                return sym # pass through
        def _defknown(fn, name = name, metasex = metasex_or_fn):
                _, sym, pyname = _interpret_toplevel_value(fn, functionp)
                name    = _defaulted(name, sym, symbol_t)
                metasex = _defaulted(metasex, _compute_default_known_metasex(name))
                return do_defknown(fn, name, pyname, metasex)
        return (_defknown(metasex_or_fn, metasex = None) if functionp(metasex_or_fn) else
                _defknown                                if isinstance(metasex_or_fn, tuple)   else
                error("In DEFKNOWN: argument must be either a function or a pretty-printer code tuple, was: %s.",
                      repr(metasex_or_fn)))
def _find_known(x):
        return _symbol_known(the(symbol_t, x))

# METASEX-MATCHER, METASEX-MATCHER-PP and METASEX-MATCHER-NONSTRICT-PP

#         MetaSEX presents us with an excellent lesson.  Let's try to understand.

def _form_known(form):
        ## METASEX-MATCHER guts it, due to case analysis
        complex_form_p = consp(form) and isinstance(form[0], symbol_t)
        return complex_form_p and _find_known(form[0])

_string_set("*METASEX-PP*", nil)

def _form_metasex(form, pp = nil):
        "Return a normalised MetaSEX for FORM."
        ## Unregistered Issue FORM-METASEX-SHOULD-COMPUTE-METASEX-OF-DEFINED-MACROS
        ## Unregistered Issue FORM-METASEX-TOO-RELAXED-ON-ATOMS
        return (_preprocess_metasex((_typep, t))  if not consp(form)                                        else
                ()                                if not form                                               else
                getattr(_find_known(form[0]),
                        "metasex_pp" if pp else
                        "metasex")                if isinstance(form[0], symbol_t) and _find_known(form[0]) else
                _preprocess_metasex((_form, "\n", [(_notlead, " "), _form]))
                                                  if isinstance(form[0], tuple) and form[0] and form[0][0] is lambda_ else
                _preprocess_metasex(([(_notlead, " "), _form],)))

def _combine_t_or_None(f0, fR, originalp):
        f0r = f0()
        if f0r is not None:
                fRr = fR()
                if fRr is not None:
                        return t

_intern_and_bind_names_in_module_specifically(
        ("_form",        "%FORM"),
        ("_bound",       "%BOUND"),
        ("_symbol",      "%SYMBOL"),
        ("_typep",       "%TYPEP"),
        ("_newline",     "%NEWLINE"),
        ("_indent",      "%INDENT"),
        ("_lead",        "%LEAD"),
        ("_notlead",     "%NOTLEAD"),
        ("_nottail",     "%NOTTAIL"),
        ("_count_scope", "%COUNT-SCOPE"))

class _metasex_matcher(_matcher):
        def __init__(m):
                _matcher.__init__(m)
                m.register_simplex_matcher(_form,        m.form)
                m.register_simplex_matcher(_bound,       m.simplex_identity)
                m.register_simplex_matcher(_symbol,      m.symbol)
                m.register_simplex_matcher(_typep,       m.typep)
                m.register_complex_matcher(_newline,     m.ignore)
                m.register_complex_matcher(_indent,      m.ignore)
                m.register_complex_matcher(_lead,        m.complex_identity)
                m.register_complex_matcher(_notlead,     m.complex_identity)
                m.register_complex_matcher(_nottail,     m.complex_identity)
                m.register_complex_matcher(_count_scope, m.complex_identity)
        @staticmethod
        def prod(x, originalp):      return ""
        @staticmethod
        def comh(f0, fR, originalp): return _combine_t_or_None(f0, fR, originalp)
        @staticmethod
        def comr(f0, fR, originalp): return _combine_t_or_None(f0, fR, originalp)
        @staticmethod
        def nonliteral_atom_p(x):
                ## Currently only depended upon by the segment matcher.
                return x == _name
        @staticmethod
        def atom(exp, pat):
                # _trace_frame()
                with _match_level():
                        symp = isinstance(exp, symbol_t)
                        ## Wanna some fun?  Replace -- not keywordp(exp), with:
                        ## symbol_package(exp) is not __keyword -- Unregistered Issue PYTHON-IS-BUGGY
                        result = ((symp and not keywordp(exp)) if pat is _name else
                                   exp is pat                  if symp         else
                                   exp == pat)
                        # _trace_printf("atom", "%% atom: e:%s/%s/%s p:%s/%s/%s:  %s (t1:%s, t2:%s), _name: %s, symp(exp): %s, keyp(exp): %s, %s",
                        #               lambda: (exp, symbol_name(exp), symbol_package(exp),
                        #                        pat, symbol_name(pat), symbol_package(pat), result,
                        #                        pat is _name, symbolp(pat), _name, symbolp(exp), keywordp(exp), type(exp)))
                        return result
        def process_formpat_arguments(m, form, pat):
                arg_handlers = { _for_matchers_xform:     (lambda arg: m in _vectorise_linear(arg[1]),
                                                           lambda arg: arg[0](form)),
                                 _for_not_matchers_xform: (lambda arg: m not in _vectorise_linear(arg[1]),
                                                           lambda arg: arg[0](form)),
                                 }
                if consp(pat):
                        args = pat[1]
                        # _debug_printf("args of %s: %s", _pp_consly(pat), _pp_consly(args))
                        for argname, argval in _vectorise_linear(args):
                                if argname not in arg_handlers:
                                        error("Invalid FORM argument: %s, pat: %s", argname, pat)
                                arg_applicable_p, arg_handler = arg_handlers[argname]
                                if arg_applicable_p(argval):
                                        ret = arg_handler(argval)
                                        # _debug_printf("\n\nMATCHED/xformed: %s", ret)
                                        return True, ret
                return None, None
        def form(m, bound, name, form, pat, orifst, ignore_args = None):
                with _match_level([form, pat]):
                        # _trace_frame()
                        ## This is actually a filter.
                        # _debug_printf("\n\nFORM -- %s -- %s", form, pat)
                        handled, ret = m.process_formpat_arguments(form, pat) if not ignore_args else (None, None)
                        if handled:
                                return ret
                        form_pat = _form_metasex(form, pp = symbol_value(_metasex_pp_))
                        # _trace_printf("form", "=== form for %s:\n    %s", lambda: (repr(form), form_pat))
                        return m.match(bound, name, form, form_pat, (consp(form), orifst[1]), None, -1)
        def symbol(m, bound, name, form, pat, orifst):
                with _match_level():
                        # _trace_frame()
                        return m.test(form is pat[1][0], bound, name, lambda: m.prod(form, orifst),
                                      form, pat)
        def typep(m, bound, name, form, pat, orifst):
                with _match_level():
                        # _trace_frame()
                        return m.test(typep(form, pat[1][0]), bound, name, lambda: m.prod(form, orifst),
                                      form, pat)

def _combine_pp(f0, fR, originalp):
        def orig_tuple_comb(body):
                new_base = _pp_depth() + 1
                with progv({ _pp_base_depth_: new_base }):
                        ret = body(new_base)
                        return None if ret is None else ("(" + ret + ")")
        def body(base):
                f0r = f0()
                if f0r is None: return
                with progv({ _pp_depth_: base + len(f0r.split("\n")[-1]) }):
                        fRr = fR()
                        if fRr is None: return
                        return f0r + fRr
        return (orig_tuple_comb if originalp else
                lambda f: f(_pp_base_depth()))(body)

class _metasex_matcher_pp(_metasex_matcher):
        def __init__(m):
                _metasex_matcher.__init__(m)
                m.register_complex_matcher(_newline,     m.newline)
                m.register_complex_matcher(_indent,      m.indent)
                m.register_complex_matcher(_lead,        m.lead)
                m.register_complex_matcher(_notlead,     m.notlead)
                m.register_complex_matcher(_nottail,     m.nottail)
        @staticmethod
        def prod(x, originalp):
                # _trace_frame()
                result = (""            if x is nil and (not originalp) else
                          '"' + x + '"' if isinstance(x, str)           else
                          str(x))
                # _trace_printf("yield", "+++ YIELD prod:\n%s", result)
                return result
        @staticmethod
        def comh(f0, fR, originalp): return _combine_pp(f0, fR, originalp)
        @staticmethod
        def comr(f0, fR, originalp): return _combine_pp(f0, fR, originalp)
        def newline(m, bound, name, exp, pat, orifst, aux, limit):
                # _trace_frame()
                n, tail = pat[0][1][0], pat[1]
                new_base = _pp_base_depth() + n
                with progv({ _pp_depth_:      new_base,
                             _pp_base_depth_: new_base }):
                        return m.post(m.match(m.bind(new_base, bound, name), None, exp, tail, orifst, aux, -1),
                                      lambda r: "\n" + (" " * new_base) + r)
        def indent(m, bound, name, exp, pat, orifst, aux, limit):
                # _trace_frame()
                n, tail = pat[0][1][0], pat[1]
                new_depth = _pp_depth() + n
                with progv({ _pp_depth_: new_depth }):
                        return m.post(m.match(m.bind(new_depth, bound, name), None, exp, tail, orifst, aux, -1),
                                      lambda r: (" " * n) + r)
        def lead(m, bound, name, exp, pat, orifst, aux, limit):
                # _trace_frame()
                maybe_pat = pat[0][1][0]
                # _trace_printf("lead", "!!! lead: first:%s %s %s", orifst[1], pat, exp)
                if not orifst[1]:
                        return m.match(bound, name, exp, pat[1],              orifst, aux, limit)
                ############## act as identity
                return         m.match(bound, name, exp, [maybe_pat, pat[1]], orifst, aux, limit)
        def notlead(m, bound, name, exp, pat, orifst, aux, limit):
                # _trace_frame()
                maybe_pat = pat[0][1][0]
                # _trace_printf("notlead", "!!! notlead: first:%s %s %s", orifst[1], pat, exp)
                if orifst[1]:
                        return m.match(bound, name, exp, pat[1],              orifst, aux, limit)
                ############## act as identity
                return         m.match(bound, name, exp, [maybe_pat, pat[1]], orifst, aux, limit)
        def nottail(m, bound, name, exp, pat, orifst, aux, limit):
                # _trace_frame()
                maybe_pat = pat[0][1][0]
                before_end = exp is nil
                # _trace_printf("nottail", "!!! nottail: before-end:%s %s %s", before_end, pat, exp)
                if before_end:
                        return m.match(bound, name, exp, pat[1],              orifst, aux, limit)
                ############## act as identity
                return         m.match(bound, name, exp, [maybe_pat, pat[1]], orifst, aux, limit)

_intern_and_bind_names_in_module_specifically(
        ("_lax",        "%LAX"))

class _metasex_matcher_nonstrict_pp(_metasex_matcher_pp):
        def __init__(m):
                _metasex_matcher_pp.__init__(m)
                m.register_complex_matcher(_lax, m.lax)
        def lax(m, bound, name, exp, pat, orifst, aux, limit):
                return (m.prod(exp, orifst[0])                if not exp         else
                        ######################### Thought paused here..
                        m.match(bound, name, exp, (([(_lax,)] if isinstance(exp[0], tuple) else
                                                    (_typep, t)), (_lax,)), (False, False), None, -1))
        def crec(m, exp, l0, lR, horisontal = True, originalp = False):
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
                        return m.match({}, None, exp, ([(_lax,)],) if listp(exp) else (_typep, t),
                                       (None, None), None, None)
                def try_R():
                        nonlocal boundR, failex, failpat
                        _, failex, failpat = lR(bound0)
                        if failpat is None: return failex
                        return m.match({}, None, exp, ([(_lax,)],) if listp(exp) else (_typep, t),
                                       (None, None), None, None)
                result = (m.comh if horisontal else
                          m.comr)(try_0, try_R, originalp)
                # _trace_printf("yield", "+t+ YIELD for %s:\n%s\nfailpat: %s", exp, result, failpat)
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

def _match_sex(sex, pattern = None, matcher = None):
        matcher = _defaulted(matcher, _metasex)
        matcher.per_use_init()
        return _match(matcher, sex, _defaulted(pattern, _form_metasex(sex)))

## WIP: set specifiers (got bored)
_string_set("*SETSPEC-SCOPE*", nil)
_setspec = _defscope("_setspec", _setspec_scope_)

# Mapping

def _combine_identity(f0, fR):
        f0r = f0()
        if f0r is not None:
                fRr = fR()
                if fRr is not None:
                        return [f0r, fRr]

_string_set("*MAPPER-FN*", nil)

class _metasex_mapper(_metasex_matcher):
        def __init__(m):
                _metasex_matcher.__init__(m)
                m.register_simplex_matcher(_form, m.form)
        @staticmethod
        def prod(x, _):      return x
        @staticmethod
        def comh(f0, fR, _): return _combine_identity(f0, fR)
        @staticmethod
        def comr(f0, fR, _): return _combine_identity(f0, fR)
        def form(m, bound, name, form, pat, orifst, ignore_args = None):
                "Used as a simplex matcher in _macroexpander_matcher."
                # _debug_printf("%%MXER-INNER: -->\n%s", exp)
                ## 1. Expose the knowns and implicit funcalls:
                handled, ret = m.process_formpat_arguments(form, pat) if not ignore_args else (None, None)
                if handled:
                        return m.succ(bound, ret)
                b, r, f = _metasex_matcher.form(m, bound, name, form, pat, orifst, ignore_args = ignore_args)
                if f:
                        return b, r, f
                return b, symbol_value(_mapper_fn_)(r), f

_mapper = _metasex_mapper()

def _map_sex(fn, sex):
        _mapper.per_use_init()
        with progv({ _mapper_fn_: fn }):
                return _match(_mapper, sex, _form_metasex(sex))

# Pretty-printing

def _pp_sex(sex, strict = t, initial_depth = None):
        _metasex_pp.per_use_init()
        ## Unregistered Issue RELAXED-METASEX-PRETTY-PRINTER-MODE-NEEDED
        initial_depth = _defaulted_to_var(initial_depth, _pp_base_depth_)
        with progv({ _pp_depth_:      initial_depth,
                     _pp_base_depth_: initial_depth,
                     _metasex_pp_:    t }): ## Guide the nested %FORM-METASEX invocations.
                _, r, f = _match(_metasex_pp, sex, _form_metasex(sex, pp = t))
        if f is not None:
                error("\n=== failed sex: %s\n=== failpat: %s\n=== failsubpat: %s\n=== subex: %s",
                      _matcher_pp(sex), _matcher_pp(pat), _matcher_pp(f), _matcher_pp(r))
        return r or ""

def _ir_minify(form):
        return ('"%s"' % form if stringp(form)                                else
                str(form)     if symbolp(form) or not form or not consp(form) else
                ("(%s ...)" % _ir_minify(form[0])))

def _mockup_sex(sex, initial_depth = None, max_level = None):
        max_level = _defaulted(max_level, _compiler_max_mockup_level)
        def mock_atom(x):       return '"' + x + '"' if isinstance(x, str) else str(x)
        def mock_complexes(xs, new_level):
                with progv({ _pp_base_depth_: symbol_value(_pp_base_depth_) + 2 }):
                        return ("\n" + _sex_space()).join(rec(x, new_level) for x in xs)
        def rec(sex, level):
                if level > max_level:
                        return "#"
                elif atom(sex):
                        return mock_atom(sex)
                else:
                        car, cdr = sex
                        complex_tail_start = position_if_not(atom, cdr)
                        simple_tail, complex_tail = (subseq(cdr, 0, complex_tail_start),
                                                     subseq(cdr, (complex_tail_start if complex_tail_start is not nil else
                                                                  length(cdr))))
                        if atom(car):
                                return ("(" + " ".join(mock_atom(x)
                                                       for x in [car] + _vectorise_linear(simple_tail)) +
                                        ("" if not complex_tail else
                                         ((("\n  " + _sex_space()) if complex_tail and level < max_level else "") +
                                          (mock_complexes(complex_tail, level + 1) if level < max_level else
                                                         " ..more.."))) +
                                        ")")
                        else:
                                return (mock_complexes(sex, level + 1) if level < max_level else
                                        "..more..")
        initial_depth = _defaulted_to_var(initial_depth, _pp_base_depth_)
        with progv({ _pp_base_depth_: initial_depth }):
                return rec(sex, 0)

# Testing

def _matcher_result_printer(x):
        return ((("%s\n%s\n%s" % (x[0], _pp_consly(x[1]), _pp_consly(x[2])))
                 if len(x) is 3 else
                 ("%s\n%s" % (_pp_consly(x[0]), _pp_consly(x[1]))))
                                                                          if isinstance(x, tuple) else
                _matcher_pp(x)                                            if isinstance(x, dict)  else
                _matcher_pp(x)                                            if consp(x)             else
                str(x))

_intern_and_bind_names_in_module("LET", "FIRST", "SECOND", "CAR", "CDR", "&BODY")
def _run_tests_metasex():
        printer = _matcher_result_printer
        def do_run_test(input, matcher = _metasex_pp):
                return _match_sex(input[0], _preprocess_metasex(input[1]), matcher = matcher)
        def just_match(input):   return do_run_test(input, matcher = _metasex)
        def pp(input):           return do_run_test(input)
        def mal_pp(input):       return do_run_test(input)
        def empty(input):        return do_run_test(input)
        def empty_cross(input):  return do_run_test(input)
        def alternates(input):   return do_run_test(input)
        def simplex(input):      return do_run_test(input)
        def mid_complex(input):  return do_run_test(input)
        def simple_maybe(input): return do_run_test(input)

        ###
        assert _runtest(just_match,
                        (_consify_star(let, ((first, ()),
                                             (second, (__car,))),
                                        _body),
                         (let, " ", ({"bindings":[(_notlead, "\n"), (_name, " ", _form)]},),
                                  1, {"body":[(_notlead, "\n"), _form]})),
                        ({ 'bindings': _consify_star((first, ()),
                                                     (second, (__car,))),
                           'body':     _consify_star(_body) },
                         t,
                         None),
                        printer = printer)

        assert _runtest(pp,
                        (_consify_star(let, ((first, ()),
                                              (second, (__car,))),
                                        _body),
                         (let, " ", ({"bindings":[(_notlead, "\n"), (_name, " ", _form)]},),
                            1, {"body":[(_notlead, "\n"), _form]})),
                        ({ 'bindings': _consify_star((first, ()),
                                                     (second, (__car,))),
                           'body':     _consify_star(_body,)},
                         """(LET ((FIRST NIL)
      (SECOND (CAR)))
  &BODY)""",
                         None),
                        printer = printer)

        # assert _runtest(mal_pp,
        #                 (_consify_star(let, ((first),
        #                                      (second, (__car,), ())),
        #                                 _body),
        #                  (let, " ", ([(_notlead, "\n"), (_name, " ", _form)],),
        #                    1, [(_notlead, "\n"), _form])),
        #                 ({},
        #                  """(LET ((FIRST ())
        #     (SECOND (CAR)))
        # &BODY)""",
        #                  None),
        #                 printer = printer)
        assert _runtest(empty,
                        (nil,
                         {"whole":nil}),
                        ({ 'whole': nil },
                         "NIL",
                         None),
                        printer = printer)

        assert _runtest(empty_cross,
                        (nil,
                         ({"a":[_name]}, {"b":[(_name,)]},)),
                        ({ 'a': nil, 'b': nil },
                         "NIL",
                         None),
                        printer = printer)

        assert _runtest(alternates,
                        (list_(1, "a"),
                         {"whole":([(_or, (_typep, int),
                                          (_typep, str))],)}),
                        ({ 'whole': list_(1, "a") },
                         '(1"a")',
                         None),
                        printer = printer)

        _intern_and_bind_names_in_module("PI")
        assert _runtest(simplex,
                        (list_(nil, pi),
                         ({'head':[nil]}, {'tail':_name})),
                        ({ 'head': list_(nil),
                           'tail': pi },
                         "(NILPI)",
                         None),
                        printer = printer)

        assert _runtest(mid_complex,
                        (list_(pi, list_(pi), list_(pi), list_(pi, pi), list_(pi, pi, pi),
                                     list_(pi), list_(pi), list_(pi), pi, pi, pi),
                         ({"headname":_name},
                          {"headtupname":(_name,)},
                                   {"varitupseq":[(_name, [_name])]},
                                                            {"fix1tupseq":[(_name,)]},
                                                                                   {"nameseq":[_name]},
                                                                                        {"tailname":_name})),
                        ({ 'headname': pi,
                          'headtupname': list_(pi),
                          'varitupseq': _consify_star((pi,), (pi, pi), (pi, pi, pi)),
                          'fix1tupseq': _consify_star((pi,), (pi,), (pi,)),
                          'nameseq': list_(pi, pi),
                          'tailname': pi },
                         "(PI(PI)(PI)(PIPI)(PIPIPI)(PI)(PI)(PI)PIPIPI)",
                         None),
                        # "(PI (PI) (PI) (PI PI) (PI PI PI) (PI) (PI) (PI) PI PI PI)"
                        printer = printer)

        assert _runtest(simple_maybe,
                        (list_(pi, car, cdr),
                         ({"pi":(_maybe, _name)}, {"car":_name}, (_maybe, {"cdr":_name}))),
                        ({ 'pi': list_(pi), 'car': car, 'cdr': cdr, },
                         "(PICARCDR)",
                         None),
                        printer = printer)

if _getenv("CL_RUN_TESTS"):
        _run_tests_metasex()

# Macroexpansion

_intern_and_bind_names_in_module("APPLY", "FUNCALL")

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
                maybe_fnref = form[1][0]
                lookupable = (consp(maybe_fnref) and length(maybe_fnref) == 2 and
                              isinstance(maybe_fnref[1][0], symbol_t))
                global_, lexical_ = ((nil, nil)               if not lookupable             else
                                     (maybe_fnref[1][0], nil) if maybe_fnref[0] is quote    else
                                     (nil, maybe_fnref[1][0]) if maybe_fnref[0] is function else
                                      (nil, nil))
                return (compiler_macro_function(global_ or lexical_, check_shadow = lexical_)
                        if global_ or lexical_ else
                        nil)
        def knownifier_and_maybe_compiler_macroexpander(form, known):
                if not known: ## ..then it's a funcall, because all macros
                        xformed = _ir_funcall(form[0], *_vectorise_linear(form[1]))
                        # _debug_printf("APPLYIFIED\n%s\n->\n%s", _pp_consly(form), _pp_consly(xformed))
                        return lambda *_: (find_compiler_macroexpander(xformed) or identity)(xformed)
                else:
                        return (find_compiler_macroexpander(form) if known.name in (apply, funcall) else
                                nil)
        expander, args = (((form and isinstance(form[0], symbol_t) and
                            (macro_function(form[0], env) or
                             (compilerp and
                              knownifier_and_maybe_compiler_macroexpander(form, _find_known(form[0]))))),
                           form[1])                                if consp(form)                else
                          (_symbol_macro_expander(form, env), nil) if isinstance(form, symbol_t) else ## Notice, how NIL is not expanded.
                          (nil, nil))
        return ((form, nil) if not expander else
                (expander(*_vectorise_linear(args)), t))

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

def _macroexpander_inner(m, bound, name, form, pat, orifst, compilerp = t, ignore_args = None):
        "Used as a simplex matcher in _macroexpander_matcher."
        # _debug_printf("%%MXER-INNER: -->\n%s", exp)
        ## 1. Expose the knowns and implicit funcalls:
        handled, ret = m.process_formpat_arguments(form, pat) if not ignore_args else (None, None)
        if handled:
                return m.succ(bound, ret)
        env = _symbol_value(_macroexpander_env_)
        expanded_form, _ = macroexpand(form, env, compilerp = compilerp)
        ## 2. Compute bindings contributed by this outer form.
        # _debug_printf("\nexpanded\n%s\n->\n%s", _pp_consly(form), _pp_consly(expanded_form))
        known = _find_known(expanded_form[0]) if consp(expanded_form) else nil
        (symbol_frame,
         mfunc_frame,
         ffunc_frame) = ((_dict_select_keys(_ir_binds(expanded_form), symbol_macro),
                          _dict_select_keys(_ir_binds(expanded_form), macro),
                          _dict_select_keys(_ir_binds(expanded_form), function)) if known else
                         (dict(), dict(), dict()))
        ## 3. Pass the binding extension information down.
        with (progv({_macroexpander_form_binds_: _make_lexenv(parent = env,
                                                              kind_varframe = symbol_frame,
                                                              kind_funcframe = _dictappend(mfunc_frame, ffunc_frame))})
              if symbol_frame or mfunc_frame or ffunc_frame else
              _withless()):
                b, r, f = _metasex_matcher.form(m, bound, name, expanded_form, pat, orifst, ignore_args = ignore_args)
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
                        return _macroexpander_inner(m, bound, name, exp, pat, orifst, compilerp = t, ignore_args = t)
        @staticmethod
        def prod(x, originalp): return x
        @staticmethod
        def comh(f0, fR, originalp):
                f0r = f0()
                if f0r is not None:
                        fRr = fR()
                        if fRr is not None:
                                return append(f0r, fRr)
        @staticmethod
        def comr(f0, fR, originalp):
                f0r = f0()
                if f0r is not None:
                        fRr = fR()
                        if fRr is not None:
                                return [f0r, fRr]

_macroexpander = _macroexpander_matcher()

def macroexpand_all(sex, lexenv = nil, compilerp = t):
        _macroexpander.per_use_init()
        with progv({ _macroexpander_env_: _coerce_to_lexenv(lexenv)}):
                _, r, f = _macroexpander_inner(_macroexpander, dict(), None,
                                               sex,
                                               nil,  ## The pattern will be discarded out of hand, anyway.
                                               (None, None),
                                               compilerp = t)
        if f is not None:
                error("\n=== failed sex: %s\n=== failsubpat: %s\n=== subex: %s", sex, f, repr(r))
        return r

# DEFMACRO

_ensure_function_pyname(defmacro) ## This is only needed due to the special definition of DEFMACRO.
@_set_macro_definition(globals(), defmacro, nil)
# ((intern("DEFMACRO")[0], " ", _name, " ", ([(_notlead, " "), _name],),
#   1, [(_notlead, "\n"), (_bound, _form)]))
def DEFMACRO(name, lambda_list, *body):
        l, l_ = list_, list__
        return l(eval_when, l(_compile_toplevel, _load_toplevel, _execute),
                 ## Unregistered Issue MATCH-FAILURE-POINTS-INTO-THE-FAILED-SEX-AND-PATTERN-NOT-AT
                 # (function, (def_, name, lambda_list) + body),
                 (ir_args,
                  l_(lambda_, lambda_list, _consify(body)),
                  l("decorators", [_ir_cl_module_call("_set_macro_definition", _ir_funcall("globals"),
                                                      (quote, name),
                                                      (quote, (name, lambda_list) + body))]),
                  l("name", name)))

# Out-of-band IR argument passing: %IR-ARGS, %IR

@defknown((ir_args, "\n", _form, ["\n", ((_typep, str), " ", (_typep, t))],))
def ir_args():
        def prologuep(known, *_):  return _ir_prologue_p(known)
        def lower(*_):             error("Invariant failed: %s is not meant to be lowered.", ir_args)
        def effects(*ir,  **args): return _ir_effects(ir)
        def affected(*ir, **args): return _ir_affected(ir)

def _destructure_possible_ir_args(x):
        "Maybe extract IR-ARGS' parameters, if X is indeed an IR-ARGS node, returning them as third element."
        return ((t,   x[1][0], x[1][1]) if consp(x) and x[0] is ir_args else
                (nil, x,       nil))

def _ir(*ir, **keys):
        "This IR-ARGS constructur is meant to be used to pass extended arguments down."
        known = _find_known(the(symbol_t, ir[0]))
        invalid_params = set(keys.keys()) - known.lower_params
        if invalid_params:
                error("In IR-ARGS: IR %s accepts parameters in the set %s, whereas following unknowns were passed: %s.",
                      known.name, known.lower_params, invalid_params)
        return list__(ir_args, ir, _consify_linear([ [k, v] for k, v in keys.items() ]))

def _ir_args_when(when, ir, **parameters):
        return _ir(*ir, **parameters) if when else ir

# IR methods

def _ir_nvalues(form):
        return (lambda known: (known.nvalues(*_vectorise_linear(form[1]))            if known else
                               None))                       (_form_known(form))
def _ir_nth_value(n, form):
        return (lambda known: (known.nth_value(n, form, *_vectorise_linear(form[1])) if known else
                               list_(nth_value, n, form)))  (_form_known(form))
def _ir_prologue_p(form):
        return (lambda known: (known.prologuep(*_vectorise_linear(form[1])           if known else
                               nil                                 if isinstance(form, symbol_t) or constantp(form) else
                               error("Prologue queries only defined on known forms, not %s.", repr(form)))))(
                                                             _form_known(form))
def _ir_binds(form):
        # As of early October 2012, used only by macroexpander.
        return (lambda known: (known.binds(*_vectorise_linear(form[1])               if known else
                               dict())))                    (_form_known(form))
def _ir_effects(form):
        return (lambda known: (known.effects(*_vectorise_linear(form[1]))            if known else
                               t))                          (_form_known(form))
def _ir_affected(form):
        return (lambda known: (known.affected(*_vectorise_linear(form[1]))           if known else
                               t))                          (_form_known(form))

def _ir_function_form_nvalues(func):
        return _defaulted(
                _ir_depending_on_function_properties(func,
                                                     lambda fn, types: len(types),
                                                     ("value_types", lambda x: x is not t)),
                t)

def _ir_function_form_nth_value_form(n, func, orig_form):
        return _defaulted(
                _ir_depending_on_function_properties(
                        func,
                        lambda fn, types, effs: (nil if n >= len(types) else
                                                 _ir_make_constant(types[n])),
                        ("value_types", lambda x: (x is not t and (n >= len(x) or
                                                                   ## XXX: This is sweet, no?
                                                                   _eql_type_specifier_p(x[n])))),
                        ## Let's not get lulled, though -- this is nothing more than a
                        ## fun optimisation, and not a substitute for proper constant propagation.
                        ("effects",     lambda x: not x)),
                list_(nth_value, n, orig_form))

def _ir_nth_valueify_last_subform(n, form):
        return append(butlast(form), list_(list_(nth_value, n, lastcar(form))))

# Prologue queries (legacy)

def _ir_body_prologuep(body):
        ## Unregistered Issue BODY-PROLOGUEP-PESSIMISTIC-WRT-DISCARDED-PURE-FORMS
        return len(body) > 1 or len(body) == 1 and _ir_prologue_p(body[0])

# Lambda list lowering

def _ir_prepare_lambda_list(lambda_list, context, allow_defaults = None, default_expr = None):
        ## Critical Issue LAMBDA-LIST-PARSING-BROKEN-WRT-BODY
        default_expr = _defaulted(default_expr, (_name, "None"))
        if not listp(lambda_list):
                error("In %s: lambda list must be a proper list, was: %s.", context, _pp_consly(lambda_list))
        lambda_list = _vectorise_linear(lambda_list)
        def valid_parameter_specifier_p(x): return isinstance(x, symbol_t) and symbol_package(x) is not __keyword
        # test, failure_message = ((lambda x: valid_parameter_specifier_p(x) or (isinstance(x, tuple) and len(x) == 2 and
        #                                                                        valid_parameter_specifier_p(x[0])),
        #                          "In %s: lambda lists can only contain non-keyword symbols and two-element lists, with said argument specifiers as first elements: %s.")
        #                          if allow_defaults else
        #                          (valid_parameter_specifier_p, "In %s: lambda list must consist of non-keyword symbols: %s.  Default values are forbidden in this context."))
        ### 0. locate lambda list keywords
        lambda_words = [_optional, _rest, _body, _key]
        optpos,  restpos,  bodypos,  keypos  = posns = [ (lambda i: x if x >= 0 else None)
                                                         (lambda_list.index(x)) for x in lambda_words ]
        ### 1. ensure proper order of provided lambda list keywords
        optposp, restposp, bodyposp, keyposp = [ x is not None for x in posns ]
        def test_lambda_list_word_order():
                toptpos     = optpos or 0
                trestpos    = restpos or toptpos
                tbodypos    = bodypos or toptpos
                tkeypos     = keypos or trestpos
                if restposp and bodyposp:
                        error("In %s: &BODY and &REST cannot coexist in a single lambda list.")
                if not toptpos <= trestpos <= tkeypos:
                        error("In %s: %s, %s, %s and %s must appear in that order in the lambda list, when specified.",
                              context, *lambda_words)
        test_lambda_list_word_order()
        # _locals_printf(locals(),
        #                "optpos",  "restpos",  "keypos",
        #                "optposp", "restposp", "keyposp",
        #                "toptpos", "trestpos", "tkeypos")
        ### 2. ensure correct amount of names for provided lambda list keywords
        if (restposp or bodyposp) and (keyposp and (keypos - restpos != 1)):
                error("In %s: found garbage instead of a lambda list: %s", context, lambda_list)
        ### 3. compute argument specifier sets, as determined by provided lambda list keywords
        _keys = lambda_list[keypos + 1:] if keypos else ()
        keys, keydefs = (list(_ensure_car(x)
                              for x in _keys),
                         list((default_expr if not consp(x) else second(x))
                              for x in _keys))
        rest_or_body = (lambda_list[restpos + 1] if restposp else
                        lambda_list[bodypos + 1] if bodyposp else
                        None)
        optional = lambda_list[optpos + 1:bodypos or restpos or keypos or None] if optposp else []
        optional, optdefs = (list(_ensure_car(x)
                                  for x in optional),
                             list((default_expr if not consp(x) else second(x))
                                  for x in optional))
        fixed = lambda_list[0:_defaulted(optpos, (restpos    if restposp    else
                                                  bodypos    if bodyposp    else
                                                  keypos     if keyposp     else None))]
        if not all(isinstance(x, symbol_t) for x in fixed):
                error("In %s: fixed arguments must be symbols, but %s wasn't one.", context, [ x for x in fixed
                                                                                               if symbolp(x) ][0])
        total = fixed + optional + ([rest_or_body] if rest_or_body else []) + keys
        ### 4. validate syntax of the provided individual argument specifiers
        if not all(valid_parameter_specifier_p(x) for x in total):
                error(failure_message, context, lambda_list)
        ### 5. check for duplicate lambda list specifiers
        if len(total) != len(set(total)):
                error("In %s: duplicate parameter names in lambda list: %s.", context, lambda_list)
        return (total,
                (fixed, optional, rest_or_body, keys),
                (optdefs, keydefs))

def _lower_lambda_list(context, fixed, optional, rest, keys, opt_defaults, key_defaults):
        assert len(optional) == len(opt_defaults)
        assert len(keys)     == len(key_defaults)
        def to_names(xs): return [ p.name(_unit_variable_pyname(x)) for x in xs ]
        return (to_names(fixed),
                to_names(optional),
                [ _primitivise(x) for x in _defaulted(opt_defaults, _repeat(_consify(ref, (quote, ("None",))))) ],
                p.name(_unit_variable_pyname(rest)) if rest else None,
                to_names(keys),
                [ _primitivise(x) for x in _defaulted(key_defaults, _repeat(_consify(ref, (quote, ("None",))))) ],
                None)

# Toolkit

def _lowered(prim):                   return prim
def _rewritten(form, scope = dict()): return form, the(dict, scope)
def _rewritep(x):                     return isinstance(x, tuple) and isinstance(x[1], dict)

# Overview

#### Issues:
## Tail position optimisations
## Lisp-level bound/free
## is the value generally side-effect-free?

### SETQ                 -> ∅                           |             LEXICAL
###                         +(APPLY,FUNCTION,QUOTE,REF)               GLOBAL
### QUOTE                -> ∅                       |                 NONCONSTANT-SYMBOL
###                         ∅                       |                 CONSTANT
###                         =(APPLY,FUNCTION,QUOTE)                   SEX
### MULTIPLE-VALUE-CALL  -> ∅
### PROGN                -> ∅
### IF                   -> ∅                    |                    EXPR
###                         =(FLET,APPLY,REF,IF)                      NONEXPR-AS-FLET
### LET                  -> APPLY,LAMBDA(e_d_e=t, f_s=f_s)         |  EXPR-BOUND-VALUES
###                         =(PROGN,SETQ,LAMBDA(e_d_e=t, f_s=f_s))    NONEXPR-SETQ-LAMBDA
### FLET                 -> =(LET,PROGN,DEF,FUNCTION)
### LABELS               -> =(FLET,DEF,APPLY) ???
### FUNCTION             -> ∅
### UNWIND-PROTECT       -> =()            |                          NO-UNWIND
###                         SETQ,PROGN,REF                            UNWIND
### MACROLET             -> =(PROGN)
### SYMBOL-MACROLET      -> =(PROGN)
### BLOCK                -> =(PROGN)       |                          NO-RETURN-FROM
###                         =(CATCH,QUOTE)                            HAS-RETURN-FROM
### RETURN-FROM          -> =(THROW,QUOTE)
### CATCH                -> =(APPLY,QUOTE)                                                      ## Via _ir_cl_module_call()
### THROW                -> =(APPLY,QUOTE)                                                      ## Via _ir_cl_module_call()
### TAGBODY              -> --not-implemented--
### GO                   -> --not-implemented--
### EVAL-WHEN            -> ∅     |                                   EXECUTE
###                         PROGN                                     NO-EXECUTE
### THE                  -> --not-implemented--
### LOAD-TIME-VALUE      -> --not-implemented--
### LET*                 -> --not-implemented--
### PROGV                -> --not-implemented--
### LOCALLY              -> --not-implemented--
### MULTIPLE-VALUE-PROG1 -> --not-implemented--
### REF                  -> ∅
### NTH-VALUE            -> =(APPLY,QUOTE)                                                      ## Via _ir_cl_module_call()
### DEF                  -> BLOCK,QUOTE
### LAMBDA               -> PROGN                      |              EXPR-BODY/DEFAULTS-NO-OPTIONAL-NO-KEYS
###                         PROGN                      |              EXPR-BODY/DEFAULTS-EARLY-EVALUATED-OPTIONAL-OR-KEYS
###                         =(LAMBDA,LET,IF,APPLY,REF) |              EXPR-BODY/DEFAULTS-REWIND-DELAYED-DEFAULT-VALUES
###                         =(FLET,FUNCTION)                          !!! NONEXPR-PROGN-DEF-FUNCTION
### APPLY                -> ∅                     |                   EXPR-ARGS
###                         =(LET,APPLY)          |                   NONEXPR-REWIND-AS-LET-APPLY
###                         =(LET,FUNCTION,APPLY)                     NONEXPR-REWIND-AS-LET-FUNCTION-APPLY

# SETQ
#         :PROPERTIES:
#         :K:        [X]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

@defknown((intern("SETQ")[0], " ", (_typep, symbol_t), " ", _form))
def setq():
        def nvalues(_, __):                                               return 1
        def nth_value(n, orig, _, value):                                 return orig if n is 0 else list_(progn, orig, nil)
        def binds(name, value, *_):
                assert(not _)
                return dict()
        ## Unregistered Issue COMPLIANCE-ISSUE-SETQ-BINDING
        ## Unregistered Issue COMPLIANCE-SETQ-MULTIPLE-ASSIGNMENTS-UNSUPPORTED
        def prologuep(*_): return t
        def lower(name, value, *_):
                assert(not _)
                lexical_binding, lexenv = symbol_value(_lexenv_).lookup_var(the(symbol_t, name))
                if not lexical_binding or lexical_binding.kind is special:
                        _compiler_trace_choice(setq, name, "GLOBAL")
                        gvar = _find_global_variable(name)
                        if gvar and gvar.kind is constant:
                                simple_program_error("%s is a constant and thus can't be set.", name)
                        if not gvar and not lexical_binding: # Must be a special, don't complain.
                                simple_style_warning("undefined variable: %s", name)
                                _compiler_defvar_without_actually_defvar(name, value)
                        return _lowered(p.special_setq(p.name(_unit_symbol_pyname(name)), _primitivise(value)))
                _compiler_trace_choice(setq, name, "LEXICAL")
                current_clambda = symbol_value(_compiler_lambda_)
                if current_clambda and current_clambda is not lexenv.clambda:
                        current_clambda.nonlocal_setqs.add(name)
                return _lowered(p.assign(lexical_binding.tn, _primitivise(value)))
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

@defknown((intern("QUOTE")[0], " ", (_form, (_for_matchers_xform, identity, _macroexpander))))
def quote():
        def nvalues(_):            return 1
        def nth_value(n, orig, _): return orig if n is 0 else nil
        def prologuep(_):          return nil
        def lower(x):
                # Unregistered Issue COMPLIANCE-QUOTED-LITERALS
                if isinstance(x, symbol_t) and not constantp(x):
                        _compiler_trace_choice(quote, x, "NONCONSTANT-SYMBOL")
                        return _lowered(p.symbol(_unit_symbol_pyname(x)))
                else:
                        prim, successp = _try_primitivise_constant(x)
                        if successp:
                                _compiler_trace_choice(quote, x, "CONSTANT")
                                return _lowered(prim)
                        else:
                                _compiler_trace_choice(quote, x, "SEX")
                                return _lowered(p.literal_list(*_xmap_to_vector(lambda x: _primitivise((quote, x)), x)))
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
        def prologuep(fn, *arg_forms):
                return _ir_prologue_p(fn) or not not arg_forms
        def lower(fn, *arg_forms):
                ## We have no choice, but to lower immediately, and by hand.
                ## Unregistered Issue SAFETY-VALUES-FRAME-CHECKING
                return _lowered(p.apply(_primitivise(fn),
                                        p.add(*_xmap_to_vector(lambda x: p.slice(_primitivise(x), 1, nil, nil), arg_forms))))
        def effects(fn, *arg_forms):
                return (any(_ir_effects(arg) for arg in arg_forms) or
                        _ir_depending_on_function_properties(func, lambda fn, effects: effects, "effects"))
        def affected(fn, *arg_forms):
                return (any(_ir_affected(arg) for arg in arg_forms) or
                        _ir_depending_on_function_properties(func, lambda fn, affected: affected, "affected"))

# PROGN
#         :PROPERTIES:
#         :K:        [X]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

@defknown((intern("PROGN")[0],
            1, [(_notlead, "\n"), _form]))
def progn():
        def nvalues(*body):            return 1   if not body else _ir_nvalues(body[-1])
        def nth_value(n, orig, *body): return nil if not body else _ir_nth_valueify_last_subform(n, orig)
        def prologuep(*body):          return _ir_body_prologuep(body)
        def lower(*body):
                return _lowered(p.progn(*_xmap_to_vector(_primitivise, body)) if body else
                                _primitivise(nil))
        def effects(*body):            return any(_ir_effects(f) for f in body)
        def affected(*body):           return any(_ir_affected(f) for f in body)

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
                return (nconseq             if nconseq == nante                      else
                        max(nconseq, nante) if integerp(nconseq) and integerp(nante) else
                        t)
        def nth_value(n, orig, test, consequent, antecedent):
                vconseq, vante = _ir_nth_value(consequent), _ir_nth_value(antecedent)
                ## Warning: this something, that goes on here, is quite interesting!
                ## Thinking pause: WHY DO WE DO THIS ..and.. WHAT EXACTLY DO WE DO HERE?
                ##  - what: lowering valueness?  ..a good initial approach to understanding..
                return (list_(nth_value, n, orig) if _ir_effects(consequent) or _ir_effects(antecedent) else
                        vconseq                   if (vconseq == vante) and not _ir_effects(test)       else
                        list_(if_, test, vconseq, vante))
        def prologuep(*tca):
                return any(_ir_prologue_p(x) for x in tca)
        def lower(test, consequent, antecedent):
                return _lowered(p.if_(test, consequent, antecedent))
        def effects(*tca):  return any(_ir_effects(f)  for f in tca)
        def affected(*tca): return any(_ir_affected(f) for f in tca)

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
                bindings = (((b,    None) if isinstance(b, symbol_t) else
                             (b[0], b[1])) for b in bindings)
                return { variable: { _variable_binding(name, _var_tn_no_unit(name), variable, None)
                                     for name, _ in bindings } }
        def prologuep(bindings, *body):
                return not not bindings or _ir_body_prologuep(body)
        def lower(bindings, *body):
                # Unregistered Issue UNIFY-PRETTY-PRINTING-AND-WELL-FORMED-NESS-CHECK
                if not (listp(bindings) and
                        every(lambda x: isinstance(x, symbol_t) or consp(x), bindings)):
                        error("LET: malformed bindings: %s.", bindings)
                # Unregistered Issue PRIMITIVE-DECLARATIONS
                # Normalisation is vain -- EFFECTS, AFFECTED and BINDS all need it..
                # Unregistered Issue NORMALISING-PREMACRO-REQUIRED-FOR-KNOWN-PREPROCESSING
                normalised = _vectorise(bindings)
                _check_no_locally_rebound_constants(x[0] for x in normalised)
                tns, frame = _variable_frame_bindings(symbol_value(_compiler_lambda_), zip(*normalised))
                return _lowered(p.let(((tn, _primitivise(form))
                                       for tn, (_, form) in zip(tns, normalised)),
                                      *(_with_lexenv_frame(frame,
                                                           lambda: _primitivise(x))
                                        for x in body)))
        def effects(bindings, *body):
                ## Unregistered Issue LET-EFFECT-COMPUTATION-PESSIMISTIC
                return any(_ir_effects(f) for f in tuple(x[1] for x in bindings) + body)
        def affected(bindings, *body):
                return any(_ir_affected(f) for f in tuple(x[1] for x in bindings) + body)

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
                return { function: { _function_binding(name, _fun_tn_no_unit(name), function, None)
                                     for name, _, *__ in bindings } }
        def prologuep(bindings, *body):
                return not not bindings or _ir_body_prologuep(body)
        def lower(bindings, *body):
                # Unregistered Issue COMPLIANCE-LAMBDA-LIST-DIFFERENCE
                # Unregistered Issue ORTHOGONALISE-TYPING-OF-THE-SEQUENCE-KIND-AND-STRUCTURE
                # Unregistered Issue LAMBDA-LIST-TYPE-NEEDED
                if not listp(bindings) or some(lambda x: length(x) < 2
                                               or not isinstance(x[0], symbol_t),
                                               or not listp(x[1][0])):
                        error("FLET: malformed bindings: %s.", bindings)
                normalised = _xmap_to_vector(lambda x: [x[0], x[1][0], _vectorise_linear(x[1][1])],
                                             bindings)
                clambdas = [ _compiler_lambda(name, lambda_list)
                             for name, lambda_list, *_ in normalised ]
                tns, frame = _function_frame_bindings(symbol_value(_compiler_lambda_), [ (c.name, c) for c in clambdas ])
                return _lowered(p.flet(((tn, _lower_lambda_list(lam)
                                         ) + tuple(_primitivise(x)  for x in body)
                                        for tn, (name, lam, *body) in zip(tns, normalised)),
                                       *(_with_lexenv_frame(frame,
                                                            lambda: _primitivise(x))
                                         for x in body)))
        def effects(bindings, *body):
                return any(_ir_effects(f) for f in body)
        def affected(bindings, *body):
                return any(_ir_affected(f) for f in body)

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
                return { function: { _function_binding(name, _fun_tn_no_unit(name), function, None)
                                     for name, _, *__ in bindings } }
        def prologuep(bindings, *body):
                return not not bindings or _ir_body_prologuep(body)
        def lower(bindings, *body):
                # Unregistered Issue COMPLIANCE-LAMBDA-LIST-DIFFERENCE
                # Unregistered Issue ORTHOGONALISE-TYPING-OF-THE-SEQUENCE-KIND-AND-STRUCTURE
                # Unregistered Issue LAMBDA-LIST-TYPE-NEEDED
                if not listp(bindings) or some(lambda x: length(x) < 2
                                               or not isinstance(x[0], symbol_t),
                                               or not listp(x[1][0])):
                        error("LABELS: malformed bindings: %s.", bindings)
                normalised = _xmap_to_vector(lambda x: [x[0], x[1][0], _vectorise_linear(x[1][1])],
                                             bindings)
                clambdas = [ _compiler_lambda(name, lambda_list)
                             for name, lambda_list, *_ in normalised ]
                tns, frame = _function_frame_bindings(symbol_value(_compiler_lambda_), [ (c.name, c) for c in clambdas ])
                with progv({ _lexenv_: frame }):
                        return _lowered(p.labels(((tn, _lower_lambda_list(lam))
                                                  + tuple(_primitivise(x)  for x in body)
                                                  for tn, (name, lam, *body) in zip(tns, normalised)),
                                                 *(_primitivise(x)
                                                   for x in body)))
        def effects(bindings, *body):
                return any(_ir_effects(f) for f in body)
        def affected(bindings, *body):
                return any(_ir_affected(f) for f in body)

# FUNCTION
#         :PROPERTIES:
#         :K:        [X]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

#         Unregistered Issue COMPLIANCE-DEFUN-DEFMACRO-LAMBDA-LAMBDA-LIST.

_intern_and_bind_names_in_module("SETF")

@defknown
def function():
        ## Unregistered Issue COMPLIANCE-FUNCTION-NAMESPACE-SEPARATION
        def nvalues(_):            return 1
        def nth_value(n, orig, _): return orig if n is 0 else nil
        def prologuep(_):          return nil
        def lower(name):
                ## (QUOTE ("str"))
                def pycall_p(x): return (consp(name) and name[0] is quote and consp(name[1])
                                         and consp(name[1][0]) and isinstance(name[1][0][0], str))
                if pycall_p(name):
                        ## this is not scoped -- a raw, unchecked call
                        return _lowered(p.prim_attr_chain([ p.name(name[1][0]) ]
                                                          + _xmap_to_vector(p.string, name[1][0])))
                lexical_binding, lexenv = symbol_value(_lexenv_).lookup_func(the(symbol_t, name))
                if not lexical_binding:
                        ## Unregistered Issue FDEFINITION-SYMBOL-FUNCTION-AND-COMPILER-GFUNS-NEED-SYNCHRONISATION
                        if not _find_global_function(name):
                                simple_style_warning("undefined function: %s", name)
                        _unit_note_gfun_reference(name)
                return _lowered(p.name(_unit_function_pyname(name)) if not lexical_binding else
                                lexical_binding.tn)
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
        def nth_value(n, orig, form, *unwind_body): return list__(unwind_protect, _ir_nth_value(n, form),
                                                                  _consify_linear(unwind_body))
        def prologuep(*_):                          return t
        def lower(form, *unwind_body):
                return _lowered(p.unwind_protect(_primitivise(form),
                                                 *(_primitivise(x) for x in unwind_body)))
        def effects(form, *unwind_body):
                return any(_ir_effects(f) for f in (form,) + body)
        def affected(form, *unwind_body):
                return any(_ir_affected(f) for f in (form,) + body)

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
                return { macro: { _function_binding(name, _fun_tn_no_unit(name), macro, list_(lambda_list, body))
                                  for name, lambda_list, *body in _vectorise_linear(bindings) } }
        def prologuep(bindings, *body):          return _ir_body_prologuep(body)
        def lower(bindings, *body):
                ## By the time we get to the lowering stage, all macros have already been expanded.
                return _rewritten(cons(progn, _consify_linear(body)))
        def effects(bindings, *body):
                return any(_ir_effects(f) for f in body)
        def affected(bindings, *body):
                return any(_ir_affected(f) for f in body)

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
                return { symbol_macro: { _variable_binding(name, _var_tn_no_unit(name), symbol_macro, form)
                                         for name, form in _vectorise_linear(bindings) } }
        def prologuep(bindings, *body):          return _ir_body_prologuep(body)
        def lower(bindings, *body):
                normalised = _vectorise_linear(bindings)
                _check_no_locally_rebound_constants([ x[0] for x in normalised ], "symbol macro")
                ## By the time we get to the lowering stage, all macros are already expanded.
                return _rewritten(cons(progn, _consify_linear(body)))
        def effects(bindings, *body):
                return any(_ir_effects(f) for f in body)
        def affected(bindings, *body):
                return any(_ir_affected(f) for f in body)

# BLOCK
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

@defknown((intern("BLOCK")[0], " ", _name,
           [1, (_bound, _form)],))
def block():
        def nvalues(name, *body):            return 1   if not body else _ir_nvalues(body[-1])
        def nth_value(n, orig, name, *body): return nil if not body else _ir_nth_valueify_last_subform(n, orig)
        def binds(name, *body):
                return { block: { _block_binding(name, t) } }
        def prologuep(name, *body):          return _ir_body_prologuep(body)
        def lower(name, *body):
                nonce = gensym("BLOCK-" + symbol_name(name) + "-")
                catch_target = list__(catch, list_(quote, nonce), _consify_linear(body))
                has_return_from = nil
                def update_has_return_from(sex):
                        nonlocal has_return_from
                        if consp(sex) and sex[0] is return_from:
                                has_return_from = t
                _map_sex(update_has_return_from, catch_target)
                if has_return_from:
                        _compiler_trace_choice(quote, name, "HAS-RETURN-FROM")
                        return _rewritten(catch_target,
                                          { _lexenv_: _make_lexenv(name_blockframe = { name: _block_binding(name, nonce) }) })
                else:
                        _compiler_trace_choice(quote, name, "NO-RETURN-FROM")
                        return _rewritten(cons(progn, _consify_linear(body)))
        def effects(name, *body):            return any(_ir_effects(f) for f in body)
        def affected(name, *body):           return any(_ir_affected(f) for f in body)

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
        def prologuep(_, value):          return _ir_prologue_p(value)
        def lower(name, value):
                binding, lexenv = symbol_value(_lexenv_).lookup_block(the(symbol_t, name))
                if not binding:
                        simple_program_error("return for unknown block: %s", name)
                return _rewritten(list_(throw, list_(quote, binding.value), value))
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

@defknown((intern("CATCH")[0], " ", _form,
           1, [(_notlead, "\n"), (_bound, _form)]))
def catch():
        ## Critical Issue CATCH-MULTIPLE-VALUES-NOT-IMPLEMENTED
        def nvalues(_, *body):              return 1 if not body else _not_implemented()
        def nth_value(n, orig, tag, *body): return (_not_implemented()      if body             else
                                                    list_(progn, tag, nil)  if _ir_effects(tag) else
                                                    nil)
        ## Unregistered Issue DOUBT-WHETHER-LAMBDA-CAN-LOWER-PROLOGUESSLY-DUE-TO-C-L-A-N-T
        def prologuep(tag, *body_):         return _ir_prologue_p(tag) or _ir_body_prologuep(body)
        def lower(tag, *body):
                return _lowered(p.catch(_primitivise(tag),
                                        *(_primitivise(x) for x in body)))
        def effects(tag, *body):  return _ir_effects(tag) or any(_ir_effects(f) for f in body)
        def affected(tag, *body): return _ir_affected(tag) or any(_ir_affected(f) for f in body)

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
        def nth_value(n, orig, _, value): return (list_(progn, tag, _ir_nth_value(value)) if _ir_effects(tag) else
                                                  _ir_nth_value(value))
        def prologuep(tag, value):        return _ir_prologue_p(tag) or _ir_prologue_p(value)
        def lower(tag, value):
                return _lowered(p.throw(_primitivise(tag), _primitivise(value)))
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

#         - Henry Baker's '92 "TAGBODY/GO" EMULATED BY "CATCH/THROW"
#         - bytecode patching, for function-internal jumps
#         - THROW, plus bytecode patching, for jumps outward of a lexically contained function
#           definition

_intern_and_bind_names_in_module("NXT-LABEL")

## Unregistered Issue COMPLIANCE-TAGBODY-TAGS-EXEMPT-FROM-MACROEXPANSION
@defknown((intern("TAGBODY")[0], ["\n", (_or, (_name,), (_bound, _form))],))
def tagbody():
        def nvalues(*tags_and_forms):            return 1
        def nth_value(_, orig, *tags_and_forms): return (nil if not any(_ir_effects(f) for f in tags_and_forms
                                                                        if isinstance(f, symbol_t)) else
                                                         list_(progn, orig, nil))
        ## Unregistered Issue TAGBODY-BINDS-METHOD-IMPRECISE-AND-CANNOT-BE-SO-MADE-EASILY
        def binds(*tags_and_forms):              return { tag: t for tag in tags_and_forms
                                                          if isinstance(tag, symbol_t) }
        def prologuep(*tags_and_forms):          return not not tags_and_forms
        def lower(*tags_and_forms):
                (init_tag,
                 go_tag,
                 return_tag) = (gensym(x + "-TAG-") for x in ["INIT", "GO", "RETURN"])
                body         = cons(init_tag, tags_and_forms)
                tags         = remove_if_not(symbolp, body)
                fun_names    = { tag: gensym("TAG-%s-" % symbol_name(tag)) for tag in _vectorise_linear(tags) }
                def lam_(seq):
                        label, body = seq[0], seq[1]
                        if not atom(label):
                                return nil
                        nextl = find_if(atom, body)
                        nlposn = position_if(atom, body)
                        return list_(list__(fun_names[label], nil,
                                            nconc(subseq(body, nlposn),
                                                  list_(_ir_funcall(fun_names[nextl]) if nlposn else
                                                        list_(throw, return_tag, nil)))))
                funs        = mapcon(lam_, body)
                # (mapcon #'(lambda (seq &aux (label (car seq) (s (cdr seq)))      
                #             (when (atom label)                                   
                #               (let ((p (position-if #'atom s)))                  
                #                 `((,(label-to-functionname label) ()             
                #                      ,@(subseq s 0 (or p (length s)))            
                #                      ,(if p `(,(label-to-functionname (elt s p)))
                #                             `(throw ,return-tag nil)))))))
                #         `(,init-tag ,@body))
                l, l_ = list_, list__
                form = l(let, l(l(go_tag, l(apply, l(function, list_), l(quote, nil), l(quote, nil)))),
                         l(let, l_(l(return_tag, l(apply, l(function, list_), l(quote, nil), l(quote, nil))),
                                   _consify_linear(l(name, go_tag) for name in fun_names.values())),
                           l(catch, return_tag,
                             l(labels, funs,
                               l(let, l(l(nxt_label, l(function, funs[0][0]))),
                                 l(protoloop,
                                   l(setq, nxt_label,
                                           l(catch, go_tag, l(apply, nxt_label, l(quote, nil))))))))))
                return _rewritten(form,
                                  { _lexenv_: _make_lexenv(name_gotagframe =
                                                           { tag: _gotag_binding(tag, fun_names[tag])
                                                             for tag in _vectorise_linear(tags[1]) }) })
        def effects(*tags_and_forms):            return some(_ir_effects, remove_if(symbolp, tags_and_forms))
        def affected(*tags_and_forms):           return some(_ir_affected, remove_if(symbolp, tags_and_forms))

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
        def nvalues(_):            return 0
        def nth_value(n, orig, _): return None
        def prologuep(_):          return nil
        def lower(name):
                binding, lexenv = symbol_value(_lexenv_).lookup_gotag(the(symbol_t, name))
                if not binding:
                        simple_program_error("attempt to GO to nonexistent tag: %s", name)
                return _rewritten(list_(throw, binding.value, list_(function, binding.value)))
        def effects(_):            return t
        def affected(_):           return nil

# EVAL-WHEN
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [ ]
#         :EFFECTS:  [ ]
#         :IMPL:     [ ]
#         :CL:       [X]
#         :END:

## Unregistered Issue EVAL-WHEN-LACKING-SPACE-BETWEEN-KEYWORDS-WHEN-PRINTED
@defknown((intern("EVAL-WHEN")[0], " ", ([(_notlead, " "),
                                          (_or, _compile_toplevel,
                                                _load_toplevel,
                                                _execute)],),
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
        def prologuep(when, *body):
                _, __, exec = _parse_eval_when_situations(when)
                return _ir_body_prologuep(body) if exec else nil
        def lower(when, *body):
                ### Unregistered Issue DEPRECATED-SYMBOLS-CONSIDERED-INVALID
                ctop, ltop, exec = _parse_eval_when_situations(when)
                ## This handles EVAL-WHEN in non-top-level forms. (EVAL-WHENs in top
                ## level forms are picked off and handled by PROCESS-TOPLEVEL-FORM,
                ## so that they're never seen at this level.)
                _compiler_trace_choice(eval_when, when, "EXECUTE" if exec else "NO-EXECUTE")
                return _rewritten(cons(progn, body) if exec else
                                  nil)

# THE
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [ ]
#         :EFFECTS:  [X]
#         :IMPL:     [ ]
#         :CL:       [X]
#         :END:

@defknown((intern("THE")[0], " ", _form, " ", _form))
def the_():
        def nvalues(type, form):            _not_implemented()
        def nth_value(n, orig, type, form): _not_implemented()
        def prologuep(type, form):          return _ir_prologue_p(form)
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

@defknown((intern("LOAD-TIME-VALUE")[0], " ", _form, (_maybe, " ", (_typep, (member_t, t, nil)))))
def load_time_value():
        def nvalues(form, read_only_p):            _not_implemented()
        def nth_value(n, orig, form, read_only_p): _not_implemented()
        def prologuep(form, read_only_p):          return _ir_prologue_p(form)
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
        def prologuep(bindings, *decls_n_body):          return not not bindings or _ir_body_prologuep(decls_n_body)
        def lower(bindings, *decls_n_body):              _not_implemented()
        def effects(bindings, *decls_n_body):            _not_implemented()
        def affected(bindings, *decls_n_body):           _not_implemented()

# PROGV
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [X]
#         :END:

@defknown((intern("PROGV")[0], " ", _form, " ", _form, ([(_notlead, "\n"), (_bound, _form)],)))
def progv_():
        def nvalues(_, __, *body):                    return 1 if not body else _ir_nvalues(body[-1])
        def nth_value(n, orig, names, values, *body): return (_ir_nth_valueify_last_subform(n, orig)
                                                              if body                     else
                                                              list_(progn,
                                                                    list_(__list, names),
                                                                    list_(__list, values),
                                                                    nil)
                                                              if (_ir_effects(names)
                                                                  or _ir_effects(values)) else
                                                              nil)
        def prologuep(names, values, *body):          return not not names or _ir_body_prologuep(body)
        def lower(vars, vals, *body):
                return _lowered(p.progv((_primitivise(x) for x in vars), (_primitivise(x) for x in vals),
                                        *(_primitivise(x) for x in body)))
        def effects(names, values, *body):            return any(_ir_effects(f) for f in (names, values) + body)
        def affected(names, values, *body):           return any(_ir_affected(f) for f in (names, values) + body)

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
        def prologuep(*decls_n_body):          return _ir_body_prologuep(decls_n_body)
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
        def nth_value(n, orig, first_form, *forms):
                return (_ir_nth_value(n, first_form) if not any(_ir_effects(f) for f in forms) else
                        (lambda sym: list__(let, list_(list_(sym, _ir_nth_value(n, first_form))),
                                            _consify_pyseq(forms, list_(sym))))
                        (gensym("MV-PROG1-VALUE-")))
        def prologuep(first_form, *forms):          return not not forms or _ir_prologue_p(first_form)
        def lower(first_form, *forms):              _not_implemented()
        def effects(first_form, *forms):            return _ir_effects(first_form) or any(_ir_effects(f) for f in forms)
        def affected(first_form, *forms):           return _ir_affected(first_form) or any(_ir_affected(f) for f in forms)

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
        def prologuep(_):          return nil
        def lower(name):
                def pyref_p(x): return typep(x, (pytuple_t, (eql_t, quote), (homotuple_t, string_t)))
                if pyref_p(name):
                        return _lowered(p.prim_attr_chain([ p.name(name[1][0]) ]
                                                          + [ p.string(x) for x in name[1][1:] ]))
                lexical_binding, lexenv = symbol_value(_lexenv_).lookup_var(the(symbol_t, name))
                if not lexical_binding or lexical_binding.kind is special:
                        gvar = _find_global_variable(name)
                        if not gvar and not lexical_binding: # Don't complain on yet-unknown specials.
                                simple_style_warning("undefined variable: %s", name)
                        _unit_note_gvar_reference(name)
                        ## Note, how this differs from FUNCTION:
                        return _lowered(p.special_ref(p.name(_unit_symbol_pyname(name))))
                if lexenv.clambda is not symbol_value(_compiler_lambda_):
                        symbol_value(_compiler_lambda_).nonlocal_refs.add(name)
                return _lowered(lexical_binding.tn)
        def effects(name):         return nil
        def affected(name):        return not _global_variable_constant_p(name)

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
                return (list_(nth_value, n, orig) if not (integerp(n) and integerp(form_n)) else ## Give up.  Too early?
                        nil                       if n != form_n and not _ir_effects(form)  else
                        list_(progn, form, nil)   if n != form_n                            else
                        _ir_nth_value(n, form)) ## We don't risque unbounded recursion here, so let's further analysis..
        def prologuep(n, form): return _ir_prologue_p(n) or _ir_prologue_p(form)
        def lower(n, form):
                return _lowered(p.funcall(p.impl_ref("_values_frame_project"), _primitivise(n), _primitivise(form)))
        def effects(n, form):   return _ir_effects(n) or _ir_effects(form)
        def affected(n, form):  return _ir_affected(n) or _ir_affected(form)

# PROTOLOOP
#         :PROPERTIES:
#         :IMPL:     [X]
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :CL:       [ ]
#         :END:

@defknown((intern("PROTOLOOP")[0], ["\n", _form]))
def protoloop():
        "This was implemented exclusively for the sake of TAGBODY."
        ## Critical Issue PROTOLOOP-MULTIPLE-VALUES-NOT-IMPLEMENTED
        def nvalues(*_):            return _not_implemented()
        def nth_value(n, *_):       return _not_implemented()
        def prologuep(*_):          return t
        def lower(*body):
                return _lowered(p.loop(*(_primitivise(x) for x in body)))
        def effects(*body):         return any(_ir_effects(x)  for x in body)
        def affected(*body):        return any(_ir_affected(x) for x in body)

# LAMBDA
#         :PROPERTIES:
#         :K:        [ ]
#         :VALUES:   [X]
#         :EFFECTS:  [X]
#         :IMPL:     [X]
#         :CL:       [ ]
#         :END:

class _compiler_lambda():
        __slots__ = ("name", "lambda_list",
                     "fixed", "optional", "rest", "keys", "optdefs", "keydefs",
                     "total_bound",
                     "nonlocal_refs", "nonlocal_setqs")
        def __init__(self, name, lambda_list):
                total, args, defaults = _ir_prepare_lambda_list(lambda_list, "LAMBDA", allow_defaults = t)
                _check_no_locally_rebound_constants(total)
                self.name, self.lambda_list = name, lambda_list
                (self.fixed, self.optional, self.rest, self.keys), (self.optdefs, self.keydefs) = args, defaults
                self.total_bound = self.fixed + self.optional + self.keys + [self.rest] if self.rest else []
                self.nonlocal_refs, self.nonlocal_setqs = set(), set()

@defknown((lambda_, " ", ([(_notlead, " "), _form],),
            1, [(_notlead, "\n"), (_bound, _form)]))
def lambda_():
        def nvalues(*_):            return 1
        def nth_value(n, orig, *_): return orig if n is 0 else nil
        def binds(lambda_list, *body, name = nil, decorators = [], evaluate_defaults_early = nil):
                total, _, __ = _ir_prepare_lambda_list(lambda_list, "LAMBDA")
                return { variable: { _variable_binding(name, _var_tn_no_unit(variable), variable, None)
                                     for name in total } }
        def prologuep(lambda_list, *body):
                total, args, defaults = _ir_prepare_lambda_list(lambda_list, "LAMBDA", allow_defaults = t)
                return len(body) < 2 and any( _ir_body_prologuep(x) for x in body + tuple(defaults[0]) + tuple(defaults[1]))
        def lower(lambda_list, *body, name = nil, decorators = [], evaluate_defaults_early = nil):
                # Unregistered Issue COMPLIANCE-LAMBDA-LIST-DIFFERENCE
                # Unregistered Issue COMPLIANCE-REAL-DEFAULT-VALUES
                # Unregistered Issue COMPILATION-SHOULD-TRACK-SCOPES
                # Unregistered Issue EMPLOY-THUNKING-TO-REMAIN-AN-EXPRESSION
                clambda = _compiler_lambda(name, lambda_list)
                args, defaults = (fixed, optional, rest, keys), (optdefs, keydefs) = \
                    (clambda.fixed, clambda.optional, clambda.rest, clambda.keys), (clambda.optdefs, clambda.keydefs)
                total = clambda.total_bound
                constant_defaults_p = all(constantp(x) for x in optdefs + keydefs)
                must_defer = not (constant_defaults_p or evaluate_defaults_early)
                tns, lexenv = _variable_frame_bindings(clambda, total)
                with progv({ _lexenv_: lexenv,
                             _compiler_lambda_: clambda }):
                        prim_body = [ _primitivise(x) for x in body ]
                nonlocal_decl = ([ p.nonlocal_([ p.name(_unit_variable_pyname(x))
                                                 for x in sorted(clambda.nonlocal_setqs, key = symbol_name) ]) ]
                                 if clambda.nonlocal_setqs else [])
                if not (name or decorators or rest or keys or (must_defer and optional)):
                        ## &rest requires listification, at very least
                        ## &key and &optional require run-time defaulting
                        ## &key require run-time arg parsing
                        ## so, the simple case is this..
                        _compiler_trace_choice(lambda_, lambda_list, "NO-REST-KEYS-FIXED-LAMBDA/NON-DEFERRED-OPTIONALS")
                        return _lowered(p.lambda_(_lower_lambda_list("LAMBDA", *(args + defaults)),
                                                  *(nonlocal_decl +
                                                    prim_body)))
                ## So, full complexity, head-on?
                _compiler_trace_choice(lambda_, lambda_list, "FULL-COMPLEXITY-HEAD-ON")
                need_rest = rest or keys ## &key is processed through parsing of *rest
                gsy_o, gs_r = ([ _gensym_tn("OPT-" + symbol_name(x) + "-") for x in optional ],
                               _gensym_tn("REST-" + symbol_name(rest) + "-") if need_rest else None)
                ns_o, ns_k = _var_tns(optional), _var_tns(keys)
                tn_ht = p.genname("KWHASH")
                nksy_k = [ p.name(_unit_symbol_pyname(_keyword(symbol_name(x)))) for x in keys ]
                [fn_tn], frame = (_function_frame_bindings(clambda, [(name, clambda)]) if name else
                                  ([nil], nil))
                with progv(frame if name else
                           dict()):
                        return _lowered(p.lambda_(
                                        _lower_lambda_list("LAMBDA", *(fixed, gsy_o, gs_r if need_rest else None,
                                                                       [], [], [])),
                                        # We're primed with LET*, but what about nonlocal..
                                        # ..we need to split them and dispatch the pieces..
                                        *(nonlocal_decl
                                          + ( [ p.assert_(p.not_(p.mod(p.funcall(p.blin_ref("len"), p.integer(2))))) ]
                                              if keys else [] )
                                          + [ p.let_(tuple((_var_tn(name),
                                                            p.if_(p.eq(gs.tn, p.name("None")),
                                                                  _primitivise(def_expr),
                                                                  gs.tn))
                                                           for name, gs, def_expr in zip(optional, gsy_o, optdefs))
                                                     + (( p.import_(p.name("pdb"), p.name("cl")),
                                                          p.funcall(p.impl_ref("_without_condition_system"),
                                                                    p.attribute(p.name("pdb"), "set_trace")) )
                                                        if name and _compiler_function_trapped_p(name) else ())
                                                     + (((_var_tn(rest), p.funcall(p.impl_ref("_consify_linear"),
                                                                                   gs_r.tn)),)
                                                        if rest else ())
                                                     + (((tn_ht, p.funcall(p.blin_ref("dict"),
                                                                           p.funcall(p.blin_ref("zip"),
                                                                                     p.slice(the(symbol_t, gs_r).tn, p.integer(0), nil,
                                                                                             p.integer(2)),
                                                                                     p.slice(gs_r.tn, p.integer(1), nil,
                                                                                             p.integer(2))))),)
                                                        if keys else ())
                                                     + tuple((name, p.if_(p.eq(p.index(tn_ht, ksymtn), p.name("None")),
                                                                          _primitivise(expr),
                                                                          p.index(tn_ht, ksymtn)))
                                                             for ksymtn, expr in zip(nksy_k, keydefs)),
                                                     *prim_body,
                                                     headp = t) ]),
                                        name = fn_tn,
                                        decorators = [ _primitivise(x) for x in decorators ]))
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

@defknown((apply, " ", _form, " ", _form, [" ", _form]))
def apply():
        def nvalues(func, _, *__):            return _ir_function_form_nvalues(func)
        def nth_value(n, orig, func, _, *__): return _ir_function_form_nth_value_form(n, func, orig)
        def prologuep(func, arg, *args):      return any(_ir_prologue_p(x) for x in (func, arg) + args)
        def lower(func, arg, *args):
                ## Unregistered Issue IMPROVEMENT-APPLY-COULD-VALIDATE-CALLS-OF-KNOWNS
                fixed, rest = (((),                 arg)       if not args                  else
                               ((arg,) + args[:-1], args[-1]))
                ## Note, how the test below is too weak to be useful:
                ## the comparison against a literal NIL is much weaker than a NULL type membership test.
                ## Therefore, the important evolutionary question, is what kind of preparations are
                ## required to make such type analysis viable.
                if rest is nil or rest == list_(quote, nil):
                        return _lowered(p.funcall(_primitivise(func), *(_primitivise(x) for x in fixed)))
                else:
                        return _lowered(p.apply(_primitivise(func), *((_primitivise(x) for x in fixed)
                                                                      + [ p.funcall(p.impl_ref("_vectorise_linear"),
                                                                                    _primitivise(rest)) ])))
        def effects(func, arg, *args):
                return (any(_ir_effects(arg) for arg in (func, arg) + args) or
                        _ir_depending_on_function_properties(func, lambda fn, effects: effects, "effects"))
        def affected(func, arg, *args):
                return (any(_ir_affected(arg) for arg in (func, arg) + args) or
                        _ir_depending_on_function_properties(func, lambda fn, affected: affected, "affected"))

# Tests

_intern_and_bind_names_in_module("COND")

def _run_tests_known():
        def applyification(input):
                _macroexpander.per_use_init()
                return _macroexpander_inner(_macroexpander, dict(), None,
                                            input, nil,  ## The pattern will be discarded out of hand, anyway.
                                            (None, None))
        assert _runtest(applyification,
                        list_(cond),
                        ({},
                         _consify_star(apply, (function, cond), (quote, nil)),
                         None))

if _getenv("CL_RUN_TESTS"):
        _run_tests_known()

# Core: %PRIMITIVISE, %EMIT-AST, %LOWER and COMPILE

def _dump_form(form):
        _debug_printf("%s\n", "*** " + "\n*** ".join(_pp_sex(form).split("\n")))

# Unregistered Issue COMPILER-MACRO-SYSTEM
def _primitivise(form, lexenv = nil) -> p.prim:
        # - tail position tracking
        # - scopes
        # - symbols not terribly clear
        # - proper quote processing
        def compiler_note_form(x):
                if (symbol_value(_compiler_trace_forms_) and _debugging_compiler() and
                    not isinstance(x, (symbol_t, bool))                            and
                    not (consp(x) and x[0] in [ref, function, quote])):
                        _debug_printf(";;;%s lowering:\n%s%s", _sex_space(-3, ";"), _sex_space(), _pp(x))
        def compiler_note_parts(known_name, xs):
                if symbol_value(_compiler_trace_subforms_) and _debugging_compiler() and known_name is not symbol:
                        _debug_printf("%s>>> %s\n%s%s", _sex_space(), name,
                                      _sex_space(), ("\n" + _sex_space()).join(_pp(f) for f in xs))
        def compiler_note_rewrite(known_name, known_subforms, result_form):
                if symbol_value(_compiler_trace_rewrites_) and _debugging_compiler() and known_name not in (symbol, ref):
                        _debug_printf("%s======================================================\n%s\n"
                                      "%s--------------------- rewrote ------------------------>\n%s\n"
                                      # "%s--------------------- rewrote ------------------------>",
                                      "%s......................................................",
                                      _sex_space(), _sex_space() + _pp((known_name,) + known_subforms),
                                      _sex_space(), _sex_space() + _pp(result_form),
                                      _sex_space())
        def compiler_note_result(form, prim):
                if (symbol_value(_compiler_trace_result_) and _debugging_compiler() and
                    ## Too trivial to take notice
                    not typep(form, (or_t, symbol_t, (cons_t, (member_t, quote, function, ref), t)))):
                        ssp = _sex_space()
                        _debug_printf(";;;\n;;; knowns ->\n;;;\n%s%s\n;;;\n;;; -> primitives\n%s%s",
                                      ssp, _pp(form),
                                      "\n".join(str(x) for x in prim.spills) + ("\n" if prim.spills else ""),
                                      prim)
        def _rec(x):
                ## XXX: what are the side-effects?
                ## NOTE: we are going to splice unquoting processing here, as we must be able
                ## to work in READ-less environment.
                compiler_note_form(x)
                if listp(x): ## nil, () or _tuplep
                        def call_known(known, forms, args):
                                compiler_note_parts(known.name, forms)
                                ret = known.lower(*forms, **_alist_hash_table(args))
                                if _rewritep(ret):
                                        form, scope = ret
                                        compiler_note_rewrite(known.name, forms, form)
                                        if scope:
                                                with progv(scope):
                                                        return _sex_deeper(4, lambda: _rec(form))
                                        else:
                                                return _sex_deeper(4, lambda: _rec(form))
                                else:
                                        return ret
                        if not x: ## Either an empty list or NIL itself.
                                return _rec((ref, nil))
                        if isinstance(x[0], symbol_t):
                                argsp, form, args = _destructure_possible_ir_args(x)
                                # Urgent Issue COMPILER-MACRO-SYSTEM
                                known = _find_known(form[0])
                                if known:
                                        # Unregistered Issue COMPILE-CANNOT-EVEN-MENTION-KWARGS
                                        return call_known(known, form[1:], args)
                                # basic function call
                                ## APPLY-conversion, likewise, is expected to have already happened.
                                # return _rec((apply,) + form + (nil,))
                                error("Invariant failed: no non-known IR node expected at this point.  Saw: %s.", x)
                        elif (consp(x[0]) and x[0] and x[0][0] is lambda_):
                                return _rec((apply,) + x + ((quote, nil),))
                        elif isinstance(x[0], str): # basic function call
                                return _rec((apply,) + x + ((quote, nil),))
                        else:
                                error("Invalid form: %s.", princ_to_string(x))
                elif isinstance(x, symbol_t) and not constantp(x):
                        ## Unregistered Issue SYMBOL-MODEL
                        return _rec((ref, x))
                else:
                        # NOTE: we don't care about quoting here, as constants are self-evaluating.
                        return _primitivise_constant(x)

        ## XXX: what about side-effects?
        with progv({ _lexenv_: _coerce_to_lexenv(lexenv) }):
                prim = the(p.prim, _rec(form))
                compiler_note_result(form, prim)
                return prim

_fixupp = gensym("FIXUPP")

class _name_context_fixer(_ast.NodeTransformer):
        def visit_Name(w, o):
                return (_ast.Name(o.id, _ast.Store()) if symbol_value(_fixupp) else
                        o)
        def visit_Assign(w, o):
                import more_ast
                with progv({ _fixupp: t }):
                        targets = [ w.visit(x)
                                    for x in o.targets ]
                return _ast.Assign(targets = targets,
                                   value = w.visit(o.value))
        def visit_AugAssign(w, o):
                with progv({ _fixupp: t }):
                        target = w.visit(o.target)
                return _ast.AugAssign(target = target,
                                      op = o.op,
                                      value = w.visit(o.value))
        def visit_For(w, o):
                with progv({ _fixupp: t }):
                        target = w.visit(o.target)
                return _ast.For(target = target,
                                iter = w.visit(o.iter),
                                body = [ w.visit(x) for x in o.body ],
                                orelse = [ w.visit(x) for x in o.orelse ])
        def visit_With(w, o):
                with progv({ _fixupp: t }):
                        optional_vars = w.visit(o.optional_vars)
                return _ast.With(context_expr = w.visit(o.context_expr),
                                 optional_vars = optional_vars,
                                 body = [ w.visit(x) for x in o.body ])
        def visit_comprehension(w, o):
                with progv({ _fixupp: t }):
                        target = w.visit(o.target)
                return _ast.comprehension(target = target,
                                          iter = w.visit(o.iter),
                                          ifs = [ w.visit(x) for x in o.ifs ])
        def visit_Subscript(w, o):
                with progv({ _fixupp: nil }):
                        return _ast.Subscript(value = w.visit(o.value),
                                              slice = w.visit(o.slice),
                                              ctx = o.ctx)
        def visit_Attribute(w, o):
                with progv({ _fixupp: nil }):
                        return _ast.Attribute(value = w.visit(o.value),
                                              attr = o.attr,
                                              ctx = o.ctx)

_name_context_fixer = _name_context_fixer()

def _emit_ast(prim) -> [p.stmt]:
        def fixup_written_name_contexts(x):
                import more_ast
                with progv({ _fixupp: nil }):
                        return _name_context_fixer.visit(the(_ast.AST, x))
        xs = p.help_prog([prim])
        return [ fixup_written_name_contexts(x) for x in xs ]

def _lower(form, lexenv = nil):
        "Must be called within %WITH-SYMBOL-UNIT-MAGIC context."
        vectree = _vectorise(form)
        prim = _primitivise(vectree, lexenv = lexenv)
        return _emit_ast(prim)

def _compile(form, lexenv = nil):
        "Same as %LOWER, but also macroexpand.  Requires %WITH-SYMBOL-UNIT-MAGIC context all the same."
        check_type(lexenv, (or_t, null_t, _lexenv))
        macroexpanded = macroexpand_all(form, lexenv = lexenv, compilerp = t)
        if symbol_value(_compiler_trace_macroexpansion_):
                if form != macroexpanded:
                        _debug_printf(";;;%s macroexpanded:\n%s%s",
                                      _sex_space(-3, ";"), _sex_space(), _pp(macroexpanded))
                else:
                        _debug_printf(";;;%s macroexpansion had no effect", _sex_space(-3, ";"))
        if symbol_value(_compiler_trace_entry_forms_):
                _debug_printf(";;;%s compiling:\n%s%s",
                              _sex_space(-3, ";"), _sex_space(), _pp(form))
        return _lower(macroexpanded, lexenv = lexenv)

# Linkage: %ASSEMBLE, %PROCESS-AS-LOADABLE, %LOAD-MODULE-BYTECODE

#
## High-level users of %LOWER   ---   UPDATE !!!
#
## eval, _eval_tlf
##   _do_eval
##     _eval_in_lexenv        <-_
##       _simple_eval_in_lexenv /
##         _simple_eval =-> _compile_in_lexenv
## _read_function_object_ast_compile_load_and_pymport, _compile_lambda_as_named_toplevel (<-= _compile), _compile_in_lexenv (<-= macro_function, _simple_eval)
##   _compile_and_load_function
##     _compile_loadable_unit :: form -> code-object
##       _expand_process_and_lower_in_lexenv =-> _lower
##       _compilation_unit_prologue =-> _lower
## @defknown -> _lower
#

def _with_symbol_unit_magic(body, standalone = nil, id = "UNIT-"):
        "Ensure symbol availability, for the code emitted by BODY, by prepending it with name initialisers."
        def in_compilation_unit():
                stmts = body()
                check_type(stmts, list)
                unit_data = _compilation_unit_symbols()
                return ((_compilation_unit_prologue(*unit_data) if standalone else []) +
                        stmts,) + tuple(unit_data)
        return with_compilation_unit(in_compilation_unit,
                                     override = t, id = id)

def _assemble(ast: [_ast.stmt], form, filename = "") -> "code":
        import more_ast
        more_ast.assign_meaningful_locations(ast)
        if symbol_value(_compiler_trace_toplevels_):
                _debug_printf(";;; Lisp ================\n%s\n;;; Python ------------->\n%s\n;;; .....................\n",
                              _pp(form), "\n".join(more_ast.pp_ast_as_code(x, line_numbers = t)
                                                  for x in ast))
                ############################ This is an excess newline, so it is a bug workaround.
                ############################ Unregistered Issue PP-AST-AS-CODE-INCONSISTENT-NEWLINES
                if isinstance(ast[0], _ast.FunctionDef):
                        _debug_printf("type of ast: %s\ndecorators: %s", type_of(ast[0]), ast[0].decorator_list)
        bytecode = _py.compile(_ast.fix_missing_locations(_ast_module(ast)), filename, "exec")
        if symbol_value(_compiler_trace_toplevels_disasm_):
                _debug_printf(";;; Bytecode ================\n")
                import dis
                def rec(x):
                        dis.dis(x)
                        for sub in x.co_consts:
                                if isinstance(sub, _types.CodeType):
                                        _debug_printf(";;; child code -------------\n")
                                        rec(sub)
                rec(bytecode)
        return bytecode

def _process_as_loadable(processor, form, lexenv = nil, id = "PROCESSED-"):
        stmts, *_ =_with_symbol_unit_magic(lambda: processor(form, lexenv = lexenv),
                                           standalone = t, id = id)
        return _assemble(stmts, form)

def _load_module_bytecode(bytecode, func_name = nil, filename = ""):
        mod, globals, locals = _load_code_object_as_module(filename, bytecode, register = nil)
        if func_name:
                sf = the((or_t, symbol_t, function_t), # globals[_get_function_pyname(name)]
                         mod.__dict__[_get_function_pyname(func_name)])
                func = sf if functionp(sf) else symbol_function(sf)
                # _without_condition_system(_pdb.set_trace) # { k:v for k,v in globals().items() if v is None }
                func.name = func_name # Debug name, as per F-L-E spec.
        else:
                func = nil
        # _debug_printf("; L-M-B globals: %x, content: %s",
        #               id(globals), { k:v for k,v in globals.items() if k != '__builtins__' })
        return func, globals, dict(globals)

# High-level drivers: %PROCESS-TOP-LEVEL, COMPILE-FILE, @LISP, COMPILE, EVAL

def _process_top_level(form, lexenv = nil) -> [_ast.stmt]:
        "A, hopefully, faithful implementation of CLHS 3.2.3.1."
        check_type(lexenv, (or_t, null_t, _lexenv))
        ## Compiler macro expansion, unless disabled by a NOTINLINE declaration, SAME MODE
        ## Macro expansion, SAME MODE
        if symbol_value(_compile_verbose_):
                kind, maybe_name = ((form[0], form[1][0]) if listp(form) and length(form) > 1 else
                                    (form[0], "")         if listp(form) and form             else
                                    (form, ""))
                _debug_printf("; compiling (%s%s%s%s)",
                              kind, " " if len(form) > 1 else "", maybe_name, " ..." if len(form) > 2 else "")
        if symbol_value(_compiler_trace_entry_forms_):
                _debug_printf(";;;%s compiling:\n%s%s",
                              _sex_space(-3, ";"), _sex_space(), _pp(form))
        macroexpanded = macroexpand_all(form, lexenv = lexenv, compilerp = t)
        if symbol_value(_compiler_trace_macroexpansion_):
                if form != macroexpanded:
                        _debug_printf(";;;%s macroexpanded:\n%s%s",
                                      _sex_space(-3, ";"), _sex_space(), _pp(macroexpanded))
                else:
                        _debug_printf(";;;%s macroexpansion had no effect", _sex_space(-3, ";"))
        ## Accumulation of results arranged for the run time:
        run_time_results = []
        def make_processor(skip_subforms, doc_and_decls):
                ## Unregistered Issue TOPLEVEL-PROCESSOR-IGNORES-DECLARATIONS
                def processor(compile_time_too, process, eval, *forms):
                        "Dispatch top-level (by deconstruction) FORMS through REC."
                        relevant = forms[skip_subforms:]
                        body = _parse_body(relevant)[0]
                        for f in body:
                                rec(compile_time_too, process, eval, f)
                return processor
        def process_eval_when(compile_time_too, process, eval, _, situations, *body):
                new_ctt, new_process, new_eval = _analyse_eval_when_situations(compile_time_too,
                                                                               *_parse_eval_when_situations(situations))
                for f in body:
                        rec(new_ctt, process and new_process, new_eval, f)
        def default_processor(compile_time_too, process, eval, *form):
                ## Unregistered Issue TOPLEVEL-PROCESSOR-WACKY-LEXENV-ARGUMENT-HANDLING
                ## This is where LEXENV isn't true, as it's always empty.
                ## ..but re-walking it.. who would care?  CLtL2 environments?
                ## Additional note: this is %PROCESS, split in half, due to cases.
                if process or eval:
                        stmts, *unit_data = _with_symbol_unit_magic(lambda: _lower(form, lexenv = lexenv),
                                                                    id = "PROCESS-TOPLEVEL-")
                if process:
                        run_time_results.extend(stmts)
                        _compilation_unit_adjoin_symbols(*unit_data)
                if eval:
                        bytecode = _assemble(_compilation_unit_prologue(*unit_data) +
                                             stmts,
                                             form)
                        # _debug_printf(";; ..compile-time code object execution")
                        _, broken_globals, good_globals = _load_module_bytecode(bytecode)
                        ## Critical Issue NOW-WTF-IS-THIS-SHIT?!
                        broken_globals.update(good_globals)
                        # _debug_printf("; D-P: globals: %x, content: %s",
                        #               id(globals), { k:v for k,v in globals.items() if k != '__builtins__' })
        actions = {
                progn:           make_processor(skip_subforms = 1, doc_and_decls = nil),
                locally:         make_processor(skip_subforms = 1, doc_and_decls = t),
                macrolet:        make_processor(skip_subforms = 2, doc_and_decls = t),
                symbol_macrolet: make_processor(skip_subforms = 2, doc_and_decls = t),
                eval_when:       process_eval_when,
                }
        def rec(compile_time_too, process, eval, form):
                ## Unregistered Issue TOPLEVEL-PROCESSOR-WACKY-LEXENV-HANDLING
                if not consp(form):
                        return
                actions.get(form[0], default_processor)(compile_time_too, process, eval, *form)
        rec(nil, t, nil, macroexpanded)
        return run_time_results

_string_set("*COMPILE-PRINT*",         t)
_string_set("*COMPILE-VERBOSE*",       t)

_string_set("*COMPILE-FILE-PATHNAME*", nil)
_string_set("*COMPILE-FILE-TRUENAME*", nil)

_string_set("*COMPILE-OBJECT*",          nil)
_string_set("*COMPILE-TOPLEVEL-OBJECT*", nil)

def compile_file(input_file, output_file = nil, trace_file = nil, verbose = None, print = None):
        verbose = _defaulted_to_var(verbose, _compile_verbose_)
        print   = _defaulted_to_var(verbose, _compile_print_)
        if verbose:
                format(t, "; compiling file \"%s\" (written %s):\n", input_file, file_write_date(input_file))
        ## input/output file conformance is bad here..
        abort_p, warnings_p, failure_p = nil, nil, nil
        forms = (progn,)
        with _py.open(input_file, "r") as input:
                def in_compilation_unit():
                        nonlocal trace_file, forms
                        if trace_file:
                                trace_filename = (trace_file if stringp(trace_file) else
                                                  input_file.replace(".lisp", "." + symbol_value(_trace_file_type_)))
                                trace_file = _py.open(trace_filename, "w")
                        try:
                                stmts = []
                                form = read(input, eof_value = input_file, eof_error_p = nil)
                                while form is not input_file:
                                        forms = forms + (form,)
                                        ## Beacon LEXENV-CLAMBDA-IS-NIL-HERE
                                        form_stmts = _process_top_level(form, lexenv = _make_null_lexenv(nil))
                                        stmts.extend(form_stmts)
                                        if trace_file:
                                                trace_file.write(_pp(form))
                                                for stmt in form_stmts:
                                                        trace_file.write(str(stmt))
                                                        trace_file.write("\n")
                                        form = read(input, eof_value = input_file, eof_error_p = nil)
                                return (_compilation_unit_prologue(*_compilation_unit_symbols()) +
                                        stmts)
                        finally:
                                if trace_file:
                                        trace_file.close()
                stmts = with_compilation_unit(in_compilation_unit,
                                              ## Unregistered Issue POSSIBLE-COMPILATION-UNIT-USE-VIOLATION-HERE
                                              override = t, id = "COMPILE-FILE-")
        output_file = output_file or input_file.replace(".lisp", "." + symbol_value(_fasl_file_type_))
        try:
                with _py.open(output_file, "wb") as f:
                        f.write(symbol_value(_fasl_file_magic_))
                        bytecode = _assemble(stmts, forms)
                        _marshal.dump(bytecode, f)
                        return output_file
        finally:
                verbose and format(t, "; %s written\n", output_file)

def _read_function_as_toplevel_compile_and_load(body):
        ## What should it be like?
        ##  - COMPILE-FILE + LOAD
        ##  - COMPILE-TOPLEVEL-FORM + ?
        name, form = _read_python_toplevel_as_lisp(body)
        bytecode = _process_as_loadable(_process_top_level, form, lexenv = _make_null_lexenv(), id = "LISP-")
        function, bad_gls, good_gls = _load_module_bytecode(bytecode, func_name = name, filename = "<lisp core>")
        bad_gls.update(good_gls)      ## Critical Issue NOW-WTF-IS-THIS-SHIT?!
        return function.name

def lisp(function):
        return _read_function_as_toplevel_compile_and_load(function)

def _compile_lambda_as_named_toplevel(name, lambda_expression, lexenv, globalp = None, macrop = None):
        "There really is no other way.  Trust me.  Please."
        form = _ir_args_when(globalp, _ir_lambda_to_def(name, lambda_expression),
                             decorators = [_ir_cl_module_call("_set_macro_definition",
                                                              _ir_funcall("globals"), name, lambda_expression)
                                           if macrop else
                                           _ir_cl_module_call("_set_function_definition",
                                                              _ir_funcall("globals"), name, lambda_expression)])
        bytecode = _process_as_loadable(_linearise_processor(_compile), form, lexenv = lexenv, id = "COMPILED-LAMBDA-")
        function, bad_gls, good_gls = _load_module_bytecode(bytecode, func_name = name, filename = "<lisp core>")
        bad_gls.update(good_gls)      ## Critical Issue NOW-WTF-IS-THIS-SHIT?!
        ## Doesn't this make %READ-FUNCTION-AS-TOPLEVEL-COMPILE-AND-LOAD somewhat of an excess?
        return function

##
def compile(name, definition = None):
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
        if consp(definition):
                lambda_expression = the((cons_t, (eql_t, lambda_), cons_t), definition)
        else:
                fun = definition or macro_function(name) or fdefinition(name)
                _, lambda_expression, _, _ = function_lambda_expression(fun)
                if not definition:
                        # Not much we can do, but return the original function.
                        return fun, nil, nil, nil
        final_name = the(symbol_t, name) or gensym("COMPILED-LAMBDA-")
        # Must has a name, for two reasons:
        #  - _ast_compiled_name() requires one
        #  - THERE-EXIST lambdas non-expressible in Python
        # Coerce the lambda to a named def, for _ast_compiled_name purposes:
        return _compile_lambda_as_named_toplevel(final_name, lambda_expression, _make_null_lexenv(),
                                                 globalp = not not name,
                                                 macrop = name and not not macro_function(name))

def eval(form):
        return compile(nil, (lambda_, (), form))()

# Auxiliary: F-L-X, FDEFINITION

################################################################################

def function_lambda_expression(function):
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
        return _values_frame(*(gethash(slot, the(function_t, function).__dict__, default)[0]
                               for slot, default in [("lambda_expression", nil),
                                                     ("closure_p",         t),
                                                     ("name",              nil)]))

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
        linecache.cache[__def_sources_filename__] = len(total), int(time.time()), total.split("\n"), __def_sources_filename__

def _peek_func_globals(x, desc = "FUNC"):
        func = the(function_t, (x if functionp(x) else
                              symbol_function(x) or macro_function(x)))
        _debug_printf("\n  %s %s (0x%x): globals %x (of type %s)\n%s",
                      desc, func.__name__, id(func),
                      id(func.__globals__), type_of(func.__globals__),
                      { k:v for k,v in func.__globals__.items() if k != '__builtins__' })
        _backtrace(15, frame_ids = t, offset = 1)

@defun
def fdefinition(name):
        ## DEFMACRO expands into this (DEFUN should too)
        return symbol_function(the(symbol_t, name))

# @LISP tests
# _trace("atom")
# _trace(_return, "crec")
# _trace(_return, "maybe")
# _trace("segment")
# _trace(_return, "segment")
# _trace("form")
# _trace(_return, "form")
# _trace("match")
# _trace(_return, "match")
# _debug_compiler()

# @defun(intern("ONEMORE")[0])
# def onemore(x): return x + 1

## Critical Issue EXTREME-INEFFICIENCY-OF-MATCHER
# _pp_sex((defmacro, defun, ("name", lambda_list, _body, "body"),
#           (quasiquote,
#             (progn,
#               (eval_when, (_load_toplevel, _execute),
#                 (apply, (apply, (function, (quote, ("cl", "_set_function_definition"))), (comma, "name"), (quote, nil)),
#                         (lambda_, (comma, lambda_list), (splice, "body")),
#                         (quote, nil))))))
#         # (1, 1, (1, 1, 1, 1),
#         #     (function, (quote, ("cl", "_set_function_definition"))))
#         )

# @lisp
# def DEFUN(name, lambda_list, *body):
#         (defmacro, defun, (name, lambda_list, _body, body),
#           (quasiquote,
#             (progn,
#               (eval_when, (_compile_toplevel,),
#                 ## _compile_and_load_function() expects the compiler-level part of function to be present.
#                 (apply, (function, (quote, ("cl", "_compiler_defun"))),
#                  (quote, (comma, name)), (quote, (lambda_, (comma, lambda_list), (splice, body))),
#                  (quote, nil))),
#               (eval_when, (_load_toplevel, _execute),
#                 (apply, (apply, (function, (quote, ("cl", "_set_function_definition"))),
#                          (quote, (comma, name)), (quote, (lambda_, (comma, lambda_list), (splice, body))),
#                          (quote, nil)),
#                         (progn,
#                           (def_, (comma, name), (comma, lambda_list),
#                            (block, (comma, name),
#                             (splice, body))),
#                           (function, (comma, name))),
#                         (quote, nil))))))

# @lisp
# def cond(*clauses):
#         (defmacro, cond, (_rest, clauses),
#          (if_, (not_, clauses),
#           nil,
#           (let, ((clause, (first, clauses)),
#                  (rest, (rest, clauses))),
#            (let, ((test, (first, clause)),
#                   (body, (rest, clause))),
#             (quasiquote, (if_, (unquote, test),
#                          (progn, (splice, body)),
#                          (cond, (splice, rest))))))))

# LOAD (+ stray stream_type_error)

_string_set("*LOAD-VERBOSE*", t)
_string_set("*LOAD-PRINT*", nil)
_string_set("*SOURCE-INFO*", nil)

def _load_as_source(stream, verbose = nil, print = nil):
        ## This is botched.
        pathname = _file_stream_name(stream)
        verbose and format(t, "; loading %s\n", repr(pathname))
        def with_abort_restart_body():
                def eval_form(form, index):
                        spref = "; evaluating "
                        print and format(t, spref + "%s\n", _pp(form, initial_depth = len(spref)))
                        def with_continue_restart_body():
                                while t:
                                        def with_retry_restart_body():
                                                results = eval(form)
                                                results = ((results,) if not _values_frame_p(results) else
                                                           _values_frame_values(results))
                                                print and format(t, "%s\n", ", ".join(repr(x) for x in results))
                                        return with_simple_restart("RETRY", ("Retry EVAL of current toplevel form.",),
                                                                   with_retry_restart_body)
                        with_simple_restart("CONTINUE", ("Ignore error and continue loading file %s.", repr(pathname)),
                                            with_continue_restart_body)
                ## Unregistered Issue DEBUG-LOAD-FILE-SOURCE-INFO-IGNORED
                def next(): return read(stream, eof_error_p = nil, eof_value = stream)
                form = next()
                if pathname:
                        with progv({ _source_info_: nil # _source_info_type(pathname = pathname)
                                     }):
                                while form != stream:
                                        eval_form(form, nil)
                                        form = next()
                else:
                        with progv({ _source_info_: nil }):
                                while form != stream:
                                        eval_form(form, nil)
                                        form = next()
        return with_simple_restart("ABORT", ("Abort loading file %s.", _file_stream_name(stream)),
                                   with_abort_restart_body)

_string_set("*LOAD-PATHNAME*", nil)
_string_set("*LOAD-TRUENAME*", nil)

_string_set("*FASL-FILE-TYPE*",  "vpfas")
_string_set("*TRACE-FILE-TYPE*", "trace")
_string_set("*FASL-FILE-MAGIC*", ";VPCL FAS\n".encode("utf-8"))

def _fasl_header_p(stream, errorp = nil):
        magic = stream.read(10)
        if magic == symbol_value(_fasl_file_magic_):
                return t
        if errorp:
                error("The file pointed at by stream %s does not contain a FASL file.", stream)
        return nil

def _load_as_fasl(stream, verbose = None, print = None):
        ## The stream is expected to have been seeked past the magic.
        verbose = _defaulted_to_var(verbose, _load_verbose_)
        print   = _defaulted_to_var(verbose, _load_print_)
        filename = truename(stream)
        verbose and format(t, "; loading %s...\n", filename)
        bytecode = _marshal.load(stream)
        _, broken_globals, good_globals = _load_module_bytecode(bytecode, filename = filename)
        ## Critical Issue NOW-WTF-IS-THIS-SHIT?!
        broken_globals.update(good_globals)

@_cold_defun_with_block
def load(pathspec, verbose = None, print = None,
         if_does_not_exist = t,
         external_format = _keyword("default")):
        verbose = _defaulted_to_var(verbose, _load_verbose_)
        print   = _defaulted_to_var(verbose, _load_print_)
        def load_stream(stream, faslp):
                with progv({ _readtable_:     _symbol_value(_readtable_),
                             _package_:       _symbol_value(_package_),
                             _load_pathname_: pathname(stream),
                             _load_truename_: handler_case(lambda: truename(stream),
                                                           (error_t, lambda _: nil)) }):
                        __return_from(load, (_load_as_fasl if faslp else
                                             _load_as_source)(stream, verbose = verbose, print = print))
        ## Case 1: stream.
        if streamp(pathspec):
                return load_stream(pathspec, _fasl_header_p(pathspec))
        pathname_ = pathname(pathspec)
        ## Case 2: Open as binary, try to process as a fasl.
        def with_open_stream_body(stream):
                if not stream:
                        __return_from(load, nil)
                real = probe_file(stream)
                should_be_fasl_p = real and string_equal(pathname_type(real), _symbol_value(_fasl_file_type_))
                if ((should_be_fasl_p or file_length(stream)) and
                    _fasl_header_p(stream, errorp = should_be_fasl_p)):
                        __return_from(load, load_stream(stream, t))
        def typeless_pathname_branch():
                nonlocal pathname_
                defaulted_pathname = probe_load_defaults(pathspec)
                if defaulted_pathname:
                        pathname_ = defaulted_pathname
                        return open(pathname_, if_does_not_exist = (_keyword("ERROR") if if_does_not_exist else
                                                                    nil),
                                    element_type = (unsigned_byte_t, 8))
        with_open_stream(((pathspec                 if streamp(pathspec)                          else
                           _py.open(pathspec, "rb") if stringp(pathspec) and probe_file(pathspec) else
                           nil) or
                          (null(pathname_type(pathspec)) and typeless_pathname_branch()) or
                          (if_does_not_exist and
                           error(simple_file_error_t, pathname = pathspec,
                                 format_control = "Couldn't load %s: file does not exist.",
                                 format_arguments = [pathspec]))),
                         with_open_stream_body)
        ## Case 3: Open using the gived external format, process as source.
        with_open_file(pathname_,
                       lambda stream: load_stream(stream, nil),
                       external_format = external_format)

@defclass
class stream_type_error_t(simple_condition_t, _io.UnsupportedOperation):
        pass

# LOAD-able things

#     Cold boot complete, now we can LOAD vpcl.lisp.

def _configure_recursion_limit(new_limit):
        _debug_printf("; current recursion limit is: %s;  setting it to %s",
                      _sys.getrecursionlimit(), new_limit)
        _sys.setrecursionlimit(new_limit)

_configure_recursion_limit(262144)

_compiler_config_tracing(# toplevels = t,
                         # toplevels_disasm = t,
                         # entry_forms = t,
                         # forms = t,
                         # primitives = t,
                         macroexpansion = t,
                         # true_death_of_code = t,
                         # result = t,
                         # rewrites = t,
                         # choices = t,
                         pretty_full = t
                         )

# _compiler_trap_function(intern("DEFPACKAGE")[0])

# @lisp
# def LOOPTEST():
#         (defun, looptest, (x,),
#           (tagbody,
#             re,
#             (setq, x, (onemore, x)),
#             (if_, (oddp, x),
#                   (go, re),
#                   (format, t, "x: %s\n", x)),
#             (go, re)))

# LOOPTEST(0)

# import cProfile as _cProfile, pstats as _pstats
# _cProfile.runctx("fasl_filename = traced()", globals(), locals(), sort = "cumulative")

def fx():
        _debug_printf("  fname: %s", _caller_frame(11).f_code.co_name)
        _debug_printf(" locals: %s", _caller_frame(11).f_locals)
        _debug_printf("  glsid: %x", id(_caller_frame(11).f_globals))
        _debug_printf("   coid: %x", id(_caller_frame(11).f_code))
        _debug_printf("DEFUN's: %x", id(find_symbol("DEFUN")[0].macro_function.__code__))

# _compiler_trap_function(intern("DEFUN")[0])

if not _getenv("CL_NO_LISP"):
        load(compile_file("vpcl.lisp"))
        load(compile_file("reader.lisp"))

# load(compile_file("reader.lisp"))

# load("vpcl.lisp", verbose = t)

# REQUIRE

_string_set("*MODULE-PROVIDER-FUNCTIONS*", [])

def _module_filename(module):
        return "%s/%s.py" % (env.partus_path, module if stringp(module) else symbol_name(module))

def require(name, pathnames = None):
        "XXX: not terribly compliant either"
        namestring = name if stringp(name) else symbol_name(name)
        filename = pathnames[0] if pathnames else _module_filename(namestring)
        if probe_file(filename):
                _not_implemented()
        else:
                error("Don't know how to REQUIRE %s.", namestring.upper())

# Environment

def get_universal_time():
        # Issue UNIVERSAL-TIME-COARSE-GRANULARITY
        # time.time() returns microseconds..
        return int(_time.time())

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
                               # ("a generic function"      if typep(fun, generic_function_t) else
                               #  "a compiled function"     if compiled_function_p(fun)       else
                               #  "an interpreted function")
                        lambda_list = (fun.lambda_list if hasattr(fun, "lambda_list") else
                                       None)
                        methods = (# generic_function_methods(fun) if typep(fun, generic_function_t) else
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
        type = type_of(x)
        slots = [ x for x in dir(x) if "__" not in x ]
        maxslotnamelen = max(len(x) for x in slots)
        def describe_slot(x, slot):
                value = getattr(x, slot)
                format(stream, "\n  %%%ds: %%s" % maxslotnamelen, slot, repr(value))
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
                return type(o).__name__
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

# STANDARD-OBJECT and basic protocols

# Unregistered Issue C-J-COULD-BE-EXTENDED-TO-FOLLOW-M-J-WITHIN-COMMENTS
def class_of(x):
        return getattr(x, "__class__")

@defclass
class standard_object_t():
        def __init__(self, **initargs):
                super().__init__() # Unregistered Issue PYTHON-OBJECT-DOES-NOT-ACCEPT-ARGUMENTS-BUT-SEE-SUPER-CONSIDERED-HARMFUL
                initialize_instance(self, **initargs)

def slot_boundp(object, slot):            return hasattr(object, slot)
def slot_makunbound(object, slot):        del object.__dir__[slot]
def slot_value(object, slot):             return getattr(object, slot)
def setf_slot_value(value, object, slot): return setattr(object, slot, value)

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
class method_t(standard_object_t):
        "All methods are of this type."

@defclass
class funcallable_standard_class_t(standard_object_t):
        "All funcallable instances are of this type."
        def __call__(self, *args, **keys):
                return self.function(*args, **keys)

@defclass
class generic_function_t(funcallable_standard_class_t):
        "All generic functions are of this type."
        def __init__(self, **initargs): # Simulate a :BEFORE method.
                self.__dependents__ = set()
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
class method_combination_t():
        "All method combinations are of this type."

@defclass
class standard_method_t(method_t):
        def __init__(self, **initargs):
                super().__init__(**initargs)
                _standard_method_shared_initialize(self, **initargs)
        def __call__(self, gfun_args, next_methods):
                return self.function(gfun_args, next_methods)

@defclass
class standard_generic_function_t(generic_function_t):
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
                elif not (_listp(argument_precedence_order) and
                          set(argument_precedence_order) == set(lambda_list[0])):
                        error("MAKE-INSTANCE STANDARD-GENERIC-FUNCTION: :ARGUMENT-PRECEDENCE-ORDER, "
                              "when specified, must be a permutation of fixed arguments in :LAMBDA-LIST.  "
                              "Was: %s;  fixed LAMBDA-LIST args: %s.",
                              repr(argument_precedence_order), lambda_list[0])
                generic_function.argument_precedence_order = tuple(argument_precedence_order)
        elif _specifiedp(lambda_list):
                generic_function.argument_precedence_order = tuple(lambda_list[0])
        generic_function.declarations        = tuple(_defaulted(declarations, nil,
                                                                type = (pylist_t,
                                                                        (satisfies_t, _valid_declaration_p))))
        generic_function.documentation       = _defaulted(documentation, nil,
                                              type = (or_t, string_t, (eql_t, nil)))
        if _specifiedp(lambda_list):
                # XXX: _not_implemented("lambda-list validation")
                generic_function.lambda_list = lambda_list
        generic_function.method_combination  = _defaulted(method_combination, standard_method_combination_t,
                                                          type = _cold_class_type)
        generic_function.method_class        = _defaulted(method_class, standard_method_t,
                                                          type = _cold_class_type) # method metaclass
        generic_function.name                = _defaulted(name, nil)
        # The discriminating function may reuse the
        # list of applicable methods without calling
        # COMPUTE-APPLICABLE-METHODS-USING-CLASSES again provided that:
        # (ii) the generic function has not been reinitialized,
        generic_function.__applicable_method_cache__ = make_hash_table() # (__list, _type) -> list
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

def generic_function_p(x): return functionp(x) and hasattr(x, "__methods__")  # XXX: CL+
def method_p(x):           return functionp(x) and hasattr(x, "specializers") # XXX: CL+
def _specializerp(x):       return ((x is t)        or
                                    typep(x, (or_t, type, (pytuple_t, (eql_t, eql), t))))

def _get_generic_fun_info(generic_function):
        return (len(generic_function.lambda_list[0]), # nreq
                nil,
                [],
                len(generic_function.lambda_list[3]),
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
                  (pylist_t,
                   (varituple_t,
                    symbol_t,       # group name
                    (or_t, (pylist_t, (or_t, pytuple_t, (eql_t, star))), # We're off the spec a little here,
                                                                         # but it's a minor syntactic issue.
                          function_t),
                    # the rest is actually a plist, but we cannot (yet) describe it
                    # in terms of a type.
                    (maybe_t, (pytuple_t,
                               (eql_t, _keyword("description")),
                               string_t)),
                    (maybe_t, (pytuple_t,
                               (eql_t, _keyword("order")),
                               (member_t,
                                _keyword("most-specific-first"),
                                _keyword("most-specific-last")))),
                    (maybe_t, (pytuple_t,
                               (eql_t, _keyword("required")),
                               (member_t, t, nil))))))
        # check_type(arguments, (maybe, lambda_list_))
        check_type(arguments, (maybe_t, (pytuple_t,
                                         (pylist_t, string_t),
                                         (pylist_t, string_t),
                                         (maybe_t, string_t),
                                         (pylist_t, string_t),
                                         (maybe_t, string_t))))
        check_type(generic_function, (maybe_t, symbol_t))
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
                               # (or_t, (pylist_t, (or_t, star, list)),
                               #        function_t),
                               if ((_listp(qualifier_spec) and
                                    any(method_qualifiers_match_pattern_p(qualifiers, x)
                                        for x in qualifier_spec))
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
               body_args = dict(grouped_methods)
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
        method_combination.name                        = the(symbol_t, name)
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
standard_method_combination_t = method_combination_t # Crude XXX
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
        for i in range(nreq):
                if not arguments:
                        error_need_at_least_n_args(generic_function_name(generic_function),
                                                   nreq)
                        arg = arguments.pop()
                        types_rev.append([type_modifier, arg] if type_modifier else
                                         arg)
        return (types_rev, arg_info)

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
                                                              reversed(possibly_applicable_methods),
                                                              types),
                                     definite_p)

def _type_from_specializer(specl):
        if specl is t:
                return t
        elif isinstance(specl, tuple):
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
                               (type, lambda: case(car(type2),
                                                       (type, compare_classes_function(specl1, specl2, index)),
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

__sealed_classes__ = set([object,
                          integer_t, boolean_t, float_t, complex_t,
                          string_t,
                          hash_table_t,
                          function_t,
                          stream_t,
                          pytuple_t, pybytes_t, pylist_t, pybytearray_t, pyset_t, pyfrozenset_t,
                          BaseException, Exception] +
                         [ type_of(x)
                           for x in [None,           # NoneType
                                     Ellipsis,       # ellipsis
                                     NotImplemented, # NotImplementedType
                                     integer_t,      # type
                                     "".find,        # builtin_function_or_method
                                     _ast,           # module
                                     _sys.stdin,     # __io.TextIOWrapper
                                     car.__code__,   # code object
                                     _this_frame(),  # frame
                                     ] ])

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
                                                                          _sorted(unsealed_classes, key = lambda type: type.__name__),
                                                                          ())
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
            symbol_name(function_name),
            lambda_list,
            # How do we access methods themselves?
            [_ast_return(
                 _ast_funcall(_ast_funcall("compute_effective_method",
                                           [_ast_name(symbol_name(function_name)),
                                            None, # method combination
                                            _ast_funcall("dfun_compute_applicable_methods",
                                                         [_ast_name(symbol_name(function_name)),
                                                          [ _ast_name(x) for x in fixed ]])]),
                              [ _ast_name(x) for x in fixed + [ x[0] for x in optional ] ],
                              _map_into_hash_star(lambda key, default: (key, _ast_name(default)),
                                                   keyword),
                              starargs = _ast_name(args) if args else None,
                              kwargs   = _ast_name(keys) if keys else None))])
        if t:
                import more_ast # Shall we concede, and import it all?
                format(t, "; generic function '%s':\n%s",
                       function_name, more_ast.pp_ast_as_code(new_dfun_ast))
        env = dict(compute_effective_method    = compute_effective_method,
                       _find_symbol_or_fail            = _find_symbol_or_fail,
                       dfun_compute_applicable_methods = dfun_compute_applicable_methods)
        return _ast_compiled_name(
                    symbol_name(function_name),
                    new_dfun_ast,
                    filename = "" # _defaulted(filename, "")
                    ,
                    lineno   = 0 # lineno
                    ,
                    function = _function_name(function_name),
                    globals  = env,
                    locals   = env)

# ENSURE-GENERIC-FUNCTION

def ensure_generic_function_using_class(generic_function, function_name,
                                        argument_precedence_order = None,
                                        declarations = None,
                                        documentation = None,
                                        generic_function_class = standard_generic_function_t,
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
                if any(x[1] is not None for x in list(optional) + list(keyword)):
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
                generic_function = make_instance(generic_function_class_t, **initargs)
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
        maybe_gfun, therep = _defaulted(_frost.global_(the(symbol_t, function_name),
                                                       _defaulted(globals, _py.globals())), nil)
        if functionp(maybe_gfun) and not generic_function_p(maybe_gfun):
                error("%s already names an ordinary function.", function_name)
        return ensure_generic_function_using_class(maybe_gfun, function_name, **keys)

def defgeneric(_ = None,
               argument_precedence_order = None,
               documentation = None,
               method_combination = standard_method_combination_t,
               generic_function_class = standard_generic_function_t,
               method_class = standard_method_t):
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
        return (len(lambda_list[0]) == len(specializers)
                and all(((ms is s)
                         or (_listp(ms) and _listp(s)
                             and len(ms) == len(s) == 2
                             and ms[0] == s[0] == eql
                             and eql(ms[1], s[1])))
                        for m, ms in
                        zip(method_specializers(method), specializers))
                and equal(method_qualifiers(method), qualifiers))

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
        return ((len(gf_fixed)    != len(m_fixed) and
                 "the method has %s required arguments than the generic function" %
                 ("more" if len(m_fixed) > len(gf_fixed) else "less"))                                or
                (len(gf_optional) != len(m_optional) and
                 "the method has %s optional arguments than the generic function" %
                 ("more" if len(m_fixed) > len(gf_fixed) else "less"))                                or
                (_xorf(gf_args, m_args) and
                 "but the method and generic function differ in whether they accept &REST or &KEY arguments") or
                # XXX: #3 compliance -- still looks fishy
                (_xorf(gf_keyword or gf_keys,
                       m_keyword  or m_keys) and
                 "but the method and generic function differ in whether they accept &REST or &KEY arguments") or
                (((not gf_keyword) or
                  m_keys           or
                  not (set(gf_keyword) - set(m_keyword))) and
                 "but the method does not accept each of the &KEY arguments %s" % tuple([gf_keyword])))

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
        old_method = [ m for m in generic_function_methods(generic_function)
                       if _method_agrees_with_qualifiers_specializers(m,
                                                                      method_qualifiers(method),
                                                                      method_specializers(method)) ]
        if old_method:
                remove_method(generic_function, old_method[0])
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
                                       type = (pylist_t, (and_t, symbol_t, (not_t, (eql_t, nil)))))
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
                                          type = (or_t, string_t, (eql_t, nil)))
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
        generic_function, definedp = _frost.global_(fn.__name__, globals())
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
                specializers = tuple(_make_method_specializers(
                                         [ gethash(name, method.__annotations__, t)[0]
                                           for name in fixed ])),
                function = _not_implemented("somehow compile", methfun_lambda)
                **methfun_args)
        add_method(generic_function, method)
        return method

def _make_method_specializers(specializers):
        def parse(name):
                return (# name                                    if specializerp(name) else
                        name                                      if name is t                   else
                                                                  # ..special-case, since T isn't a type..
                        name                                      if isinstance(name, type)      else
                                                                  # Was: ((symbolp name) `(find-class ',name))
                        _poor_man_ecase(car(name),
                                        (eql,       lambda: intern_eql_specializer(name[1])),
                                        (class_eq_, lambda: class_eq_specializer(name[1]))) if isinstance(name, tuple)      else
                        ## Was: FIXME: Document CLASS-EQ specializers.
                        error("%s is not a valid parameter specializer name.", name))
        return [ parse(x) for x in specializers ]

# Init

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
        return (x.value if isinstance(x, _ast.Expr) else
                x)

def _eval_python(expr_or_stmt):
        "In AST form, naturally."
        package = _symbol_value(_package_)
        exprp = isinstance(the(_ast.AST, expr_or_stmt), (_ast.expr, _ast.Expr))
        call = _ast.fix_missing_locations(_ast_module(
                        [_ast_import_from("cl", ["__evset__", "_read_symbol"]),
                         _ast_Expr(_ast_funcall(_ast_name("__evset__"), [_coerce_to_expr(expr_or_stmt)]))]
                        if exprp else
                        [expr_or_stmt]))
        code = handler_case(lambda: _py.compile(call, "", "exec"),
                            (error_t,
                             lambda cond:
                                     error("EVAL: error while trying to compile <%s>: %s",
                                           more_ast.pp_ast_as_code(expr_or_stmt), cond)))
        if boundp(_source_for_eval_):
                __eval_source_cache__[code] = _symbol_value(_source_for_eval_)
        # write_line(">>> EVAL: %s" % (more_ast.pp_ast_as_code(expr),))
        exec(code, _find_module(_frost.lisp_symbol_name_python_name(package_name(package))).__dict__)
        values = (__evget__() if exprp else
                  ())
        return values if isinstance(values, tuple) else (values,)

def _callify(form, package = None, quoted = nil):
        package = _defaulted_to_var(package, _package_)
        def callify_call(sym, args):
                func = function(the(symbol_t, sym))
                paramspec = _inspect.getfullargspec(func)
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
                        [ _callify(x, package) for x in args ],
                        _map_hash_table(
                               lambda k, x: (k, _callify(x, package)),
                                      keyargs))
        obj2ast_xform = {
                False     : _ast_name("False"),
                None      : _ast_name("None"),
                True      : _ast_name("True"),
                string_t  : _ast_string,
                integer_t : _ast_num,
                }
        if _listp(form):
                if quoted or (form[0] is _find_symbol("QUOTE", __cl)[0]):
                        return (_ast_list([ _callify(x, package, t) for x in form[1] ])
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
                return obj2ast_xform[type(form)](form)
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
def _intern0(x, package = None): return _intern(the(str, x), package)[0]

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
