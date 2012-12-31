
# Builtins management

def python_builtins_dictionary():
        import builtins    as _builtins
        return _builtins.getattr(__builtins__, "__dict__", __builtins__)

import collections

class dictator(collections.UserDict):
        def __hasattr__(self, name): return name in self.data
        def __getattr__(self, name): return self.data[name]
        def __setitem__(self, *_):   raise  self.data["Exception"]("Dictator.")
        def __delitem__(self, *_):   raise  self.data["Exception"]("Dictator.")
        def __setattr__(self, *_):   raise  self.data["Exception"]("Dictator.")
        def __init__(self, dict):
                self.__dict__.update(data = dict)

py = dictator(python_builtins_dictionary())

# Imports

import re
import os
import io
import _io
import ast
import imp
import pdb
import sys
import math
import time
import trace
import types
import socket
import hashlib
import inspect
import marshal
import builtins
import operator
import platform
import functools
import itertools
import linecache
import threading
import collections

import neutrality
import frost

# Default values for optional/key arguments

def defaulted(x, value, type = None):
        if x is not None and type is not None:
                check_type(x, type) # Not a macro, so cannot access the actual defaulted name..
        return x if x is not None else value

def defaulted_to_var(x, variable, type = None):
        return x if x is not None else defaulted(x, symbol_value(variable), type = type)

def specifiedp(x):
        return x is not None

def only_specified_keys(**keys):
        return dict(((k, v) for k, v in keys.items()
                     if specifiedp(k)))

def defaulted_keys(**keys):
        return dict((key, (default if value is None else value))
                    for key, (value, default) in keys.items())

def validate_function_keys(desc, f, keys):
        argspec = inspect.getfullargspec(f)
        invalid = (nil if argspec.varkw else
                   (set(keys.keys()) - set(argspec.args) - set(argspec.kwonlyargs)))
        if invalid:
                error("Invalid arguments for %s: %s does not expect keyword arguments %s -- the argspec is %s.",
                      desc, f, ", ".join("'%s'" % x for x in invalid), argspec)

# Boot messaging

def fprintf(stream, format_control, *format_args):
        try:
                neutrality.do_write_string(format_control % format_args, stream)
        except UnicodeEncodeError:
                neutrality.do_write_string((format_control % format_args).encode("utf-8"), stream)

def dprintf(format_control, *format_args):
        fprintf(sys.stderr, format_control + "\n", *format_args)

# First-class namespaces

class namespace(collections.UserDict):
        def __str__(self):
                return "#<NAMESPACE %s>" % (repr(self.name),)
        def __init__(self, name, data_constructor = dict):
                self.name, self.data, self.properties = name, data_constructor(), collections.defaultdict(dict)
        def __getitem__(self, x):               return self.data.__getitem__(x)
        def __hasitem__(self, x):               return self.data.__hasitem__(x)
        def names(self):                        return set(self.data.keys())
        def intersect(self, with_):             return [x for x in with_ if x in self.data] if len(self) > len(with_) else [x for x in self.data if x in with_]
        def has(self, name):                    return name in self.data
        def get(self, name):                    return self.data[name]
        def access(self, name, default = None): return (default, None) if name not in self.data else (self.data[name], True)
        def set(self, value, name):             self.data[name] = value; return value
        def grow(self, name, **keys):           self.data[name] = namespace_type_and_constructor(name, **keys); self.setf_property(True, name, "NAMESPACEP")
        def properties(self, name):             return self.properties[name]
        def has_property(self, name, pname):    return pname in self.properties[name]
        def property(self, name, pname, default = None):
                cell = self.properties[name]
                return cell[pname] if pname in cell else default
        def setf_property(self, value, name, pname):
                self.properties[name] = value
                return value
namespace_type_and_constructor = namespace
namespace = namespace_type_and_constructor("")

# Meta-boot

def global_(x, globals = globals()):
        """This is important due to the single namespace, and the
consequent shadowing of various specifiers."""
        return frost.global_(x, globals)[0]

## 1. trivial enumeration for later DEFUN/DEFCLASS
__boot_defunned__, __boot_defclassed__ = set(),  set()
def boot_defun(fn):     __boot_defunned__.add(fn);    return fn
def boot_defclass(cls): __boot_defclassed__.add(cls); return cls

## 2. tagged switchables
namespace.grow("boot", data_constructor = lambda: collections.defaultdict(set))

def boot(set, boot, on_unboot = None):
        def definer(orig):
                def unboot():
                        frost.setf_global(orig, orig.__name__, globals(), force = True)
                        if on_unboot:
                                on_unboot()
                def linkage(*args, **keys):
                        return boot(orig, *args, **keys)
                boot.unboot = unboot
                boot.name = orig.__name__
                namespace["boot"][set].add(boot)
                return linkage
        return definer

def unboot_set(set):
        for x in sorted(namespace["boot"][set], key = lambda x: x.name):
                if not hasattr(x, "unboot"):
                        error("In UNBOOT-SET \"%s\": %s has no 'unboot' attribute.", set, x)
                x.unboot()
        del namespace["boot"][set]
        dprintf("; unbooted function set %s, remaining boot sets: %s", repr(set), ", ".join(namespace["boot"].keys()))

def interpret_toplevel_value(name_or_obj, objness_predicate):
        name, obj = ((name_or_obj.__name__, name_or_obj) if objness_predicate(name_or_obj)           else
                     (name_or_obj, None)                 if isinstance(name_or_obj, (str, symbol_t)) else
                     error("Bad cold object definition: %s", name_or_obj))
        ####### Thought paused here:
        # ..delay symbol computation!
        sym, inmod_name = ((do_intern(frost.python_name_lisp_symbol_name(name))[0], name) if isinstance(name, str)      else
                           (name, frost.lisp_symbol_name_python_name(symbol_name(name)))  if isinstance(name, symbol_t) else
                           error("In cold definition of %s: bad name %s for a cold object.", name, repr(name)))
        return obj, sym, inmod_name

# Cold types

cold_class_type       = type
cold_condition_type   = BaseException
cold_error_type       = Exception
cold_hash_table_type  = dict
cold_stream_type      = _io._IOBase
cold_function_type    = types.FunctionType.__mro__[0]
cold_tuple_type       = tuple
cold_string_type      = str
cold_list_type        = list
def cold_simple_error(format, *args): raise cold_error_type(format % args)
def cold_typep(x, type):
        return isinstance(x, (type             if isinstance(x, type) else
                                  type.python_type if isinstance(x, symbol_t) else
                                  cold_simple_error("%s is neither a python type, nor a symbol.",
                                                    x.__repr__())))
def cold_the(type, x):
        if typep(x, type):
                return x
        else:
                raise cold_simple_error("%s is not a %s.", x.__repr__(), type)
def cold_check_type(x, type):
        the(type, x)
typep      = cold_typep
the        = cold_the
check_type = cold_check_type

# As-of-yet -homeless type predicates..

@boot_defun
def stringp(x):        return isinstance(x, cold_string_type)
@boot("symbol", lambda _, o: (isinstance(o, _cold_function_type) or
                              isinstance(o, symbol_t) and o.function))
@boot_defun ## Unregistered Issue COMPLIANCE-EVALUATION-MODEL-FUNCTIONP
def functionp(o):      return isinstance(o, cold_function_type)

def symbol_type_specifier_p(x):
        return hasattr(x, "python_type")

def python_type_p(x): return isinstance(o, cold_class_type)

@boot_defun
def type_of(x):
        return type(x)

# Unspecific Wave 1

@boot_defun
def identity(x):   return x

@boot_defun
def make_hash_table(default_constructor = None):
        return (collections.defaultdict(default_constructor) if default_constructor else
                dict())

@boot_defun
def gethash(key, dict, default = None):
        therep = key in dict
        return (dict[key] if therep else default), therep

def map_into_hash(f, xs,
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

class thread_local_storage(threading.local):
        def __init__(self):
                self.dynamic_scope = []

__tls__ = thread_local_storage()

# The symmetry invariance is _IMPORTANT_, as you probably can imagine!
def dynamic_scope_push(scope):
        __tls__.dynamic_scope.append(scope)
def dynamic_scope_pop():
        __tls__.dynamic_scope.pop()

def find_dynamic_frame(name):
        for scope in reversed(__tls__.dynamic_scope):
                if name in scope:
                        return scope
        if name in __global_scope__:
                return __global_scope__

def list_dynamic_frames():
        return __tls__.dynamic_scope

def dynamic_frame_for_set(name, force_toplevel = None):
        return (__global_scope__ if force_toplevel else
                (find_dynamic_frame(name) or
                 (__tls__.dynamic_scope[-1] if __tls__.dynamic_scope else
                  __global_scope__)))

def do_symbol_value(name):
        frame = find_dynamic_frame(name)
        return (frame[name] if frame else
                error(AttributeError, "Unbound variable: %s." % name))

def do_pyimport_symbol(symbol, globals, name_xform = frost.lisp_symbol_name_python_name, force = False):
        inmod_name = name_xform(symbol_name(symbol))
        # dprintf("PYMPORT '%s'", inmod_name)
        frost.setf_global(symbol, inmod_name, globals, force = force)

def pyimport_symbol(symbol, globals = None, name_xform = frost.lisp_symbol_name_python_name, force = False):
        do_pyimport_symbol(boot_check_type(symbolp, symbol), defaulted(globals, py.globals()), name_xform, force)

def global_pyname(name):
        if name[0] != "*" != name[-1]:
                error("%%GLOBAL-PYNAME: provided symbol name \"%s\" is not valid for a global variable name.", name)
        return "_%s_" % name[1:-1].replace("-", "_").lower()

def symbol_pyname(name):
        return "_" + name.replace("%", "_").replace("&", "_").replace("-", "_").strip("_%").lower()

def intern_and_bind_symbols(*names, globals = None):
        globals = defaulted(globals, py.globals())
        for name in names:
                pyimport_symbol(intern(name)[0], globals, symbol_pyname)

def intern_and_bind_globals(*names, globals = None):
        globals = defaulted(globals, py.globals())
        for name in names:
                pyimport_symbol(intern(name)[0], globals, global_pyname)

def intern_and_bind_names_in_module_specifically(*name_specs, globals = None):
        globals = defaulted(globals, py.globals())
        for pyname, name in name_specs:
                frost.setf_global(intern(name)[0], pyname, globals)

def boot_symbolicate_global_dynamic_scope():
        def upgrade_scope(xs):
                kvs = list(xs.items())
                for k, v in kvs:
                        del xs[k]
                        sym = intern_in_package(k, __cl)[0]
                        xs[sym] = v
                        do_pyimport_symbol(sym, globals(), force = t)
        assert not __tls__.dynamic_scope
        upgrade_scope(__global_scope__)

def do_set(name, value, force_toplevel):
        dynamic_frame_for_set(name, force_toplevel = force_toplevel)[name] = value
        return value

@boot("symbol",
      lambda string_set, name, value, force_toplevel = None:
      string_set(name, value, force_toplevel = force_toplevel, symbolicp = False),
      on_unboot = boot_symbolicate_global_dynamic_scope)
def string_set(symbol_name, value, force_toplevel = None, symbolicp = True, globals = None):
        isinstance(symbol_name, str) or \
                 error("The first argument to %%STRING-SET must be a string, was: %s.", symbol_name.__repr__())
        name = intern(symbol_name)[0] if symbolicp else symbol_name
        do_set(name, value, force_toplevel)
        symbolicp and pyimport_symbol(name, globals = globals)
        return value

# @boot("typep", lambda _, __, ___: error("A violent faecal odour hung in the air.."))
# @boot_defun
# def set(symbol, value, *_, force_toplevel = False):
#         do_set(the(symbol_t, symbol), value, force_toplevel)
#         return value

@boot("symbol", lambda _, name: find_dynamic_frame(boot_check_type(stringp, name)) and t)
@boot_defun
def boundp(symbol):
        # Unregistered Issue COMPLIANCE-BOUNDP-ACCEPTS-STRINGS
        return find_dynamic_frame(the(symbol_t, symbol)) and t

# Boot conditions: WARN, ERROR

def conditionp(x):
        return isinstance(x, cold_condition_type)

@boot("typep", lambda _, datum, *args, default_type = None, **keys:
              Exception(datum % args) if isinstance(datum, str) else
              (datum if not (args or keys) else
               error("Bad, bad evil is rising.  Now go and kill everybody.")) if conditionp(datum) else
              datum(*args, **keys))
def coerce_to_condition(datum, *args, default_type = None, **keys):
        def not_a_condition_specifier_error(x):
                raise Exception("Cannot coerce %s to a condition." % repr(x))
        type_specifier = defaulted(default_type, error_t) if isinstance(datum, str) else datum

        type_ = (type_specifier             if isinstance(type_specifier, type)                                     else
                 None                       if conditionp(type_specifier)                                          else
                 type_specifier.python_type if isinstance(type_specifier, symbol_t) and symbol_type_specifier_p(type_specifier) else
                 not_a_condition_specifier_error(datum))
        cond = (datum              if type_ is None   else # Already a condition.
                type_(datum % args) if isinstance(datum, str) else
                type_(*args, **keys))
        return cond

@boot("typep", lambda _, datum, *args, **keys:
              dprintf("COLD WARNING: " + datum, *args, **keys))
@boot_defun
def warn(control, *args, **keys):
        condition = coerce_to_condition(control, *args, **keys)
        check_type(condition, warning_t)
        signal(condition)
        badness = poor_man_etypecase(condition,
                                      (style_warning_t, "STYLE-WARNING"),
                                      (warning_t,       "WARNING"))
        format(symbol_value(_error_output_), "%s: %s\n", badness, condition)
        return nil

# @boot(lambda error, datum, *args, **keys: frost.raise_exception(coerce_to_condition(datum, *args, **keys)))
@boot_defun
def error(datum, *args, **keys):
        ## Shouldn't we ditch Python compat entirely, doing instead
        ## the typical SIGNAL/INVOKE-DEBUGGER thing?
        raise coerce_to_condition(datum, *args, **keys)

def boot_check_type(pred, x):
        return x if pred(x) else error("A violent faecal odour hung in the air..")

# Package system conditions

def package_not_found_error(x):
        error("The name \"%s\" does not designate any package.", x)

def symbol_conflict_error(op, obj, pkg, x, y):
        error(simple_package_error_t, "%s %s causes name-conflicts in %s between the following symbols: %s, %s." %
              (op, obj, pkg, x, y))

def symbols_not_accessible_error(package, syms):
        def pp_sym_or_string(x):
                return "\"%s\"" % x if isinstance(x, str) else print_nonkeyword_symbol(x)
        error(simple_package_error_t, "These symbols are not accessible in the %s package: (%s).",
              package_name(package), ", ".join((pp_sym_or_string(x) for x in syms)))

# Package system classes

namespace.grow("PACKAGES")

@boot_defclass
class package_t(collections.UserDict):
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
                        nickname_conflicts = namespace["PACKAGES"].intersect(nicknames)
                        for n_c in nickname_conflicts:
                                p = namespace["PACKAGES"][n_c]
                                if p.name == n_c: error("\"%s\" is a package name, so it cannot be a nickname for \"%s\".", n_c, name)
                                else:             error("\"%s\" is already a nickname for \"%s\".", n_c, p.name)
                def setup_package_usage(p, used):
                        ## Issue CCOERCE_TO_PACKAGE-WEIRD-DOUBLE-UNDERSCORE-NAMING-BUG
                        # coercer = (ccoerce_to_package if boot else
                        #            coerce_to_package)
                        p.used_packages  = set(find_package(x) or package_not_found_error(x)
                                               for x in used)
                        p.packages_using = set()
                        if p.used_packages:
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
                self.inherited   = collections.defaultdict(set) # sym -> set(pkg) ## mapsetn(slotting("external"), used_packages) -> source_package
                self.accessible  = make_hash_table()             # str -> sym          ## accessible = present + inherited
                self.external    = set()                         # sym                 ## subset of accessible
              # self.internal    = accessible - external

                setup_package_usage(self, use)

                ## Hit the street.
                self.data          = self.accessible
                namespace["PACKAGES"].set(self, name)
                for nick in nicknames:
                        namespace["PACKAGES"].set(self, nick)

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
                namespace["PACKAGES"].access(name if isinstance(name, str) else symbol_name(name))[0] or nil)

@boot_defun
def package_used_by_list(package):
        p = coerce_to_package(package)
        return p.packages_using if p else package_not_found_error(package)

@boot_defclass
class symbol_t(): # Turned to a symbol, during the package system bootstrap.
        def __str__(self):
                return print_symbol(self)
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
def symbol_value(symbol):      return do_symbol_value(the(symbol_t, symbol))
## Unregistered Issue FDEFINITION-SYMBOL-FUNCTION-AND-COMPILER-GFUNS-NEED-SYNCHRONISATION
def do_symbol_function(symbol):  return (symbol.known          or
                                         symbol.macro_function or
                                         symbol.function       or
                                         dprintf("no fun: %s", symbol) or
                                         error(undefined_function_t, symbol))

def really_do_find_symbol(str, package):
        return gethash(str, package.accessible, None)[0]

def find_symbol_or_fail(x, package = None):
        sym = really_do_find_symbol(x, coerce_to_package(package))
        return (sym if sym is not None else
                symbols_not_accessible_error(p, [x]))

def symbol_relation(x, p):
        "NOTE: here we trust that X belongs to P, when it's a symbol."
        s = gethash(x, p.accessible, None)[0] if isinstance(x, str) else x
        if s is not None:
                return make_keyword("INHERITED" if s.name in p.inherited else
                                    "EXTERNAL"  if s      in p.external  else
                                    "INTERNAL")

def do_find_symbol(str, package):
        s = really_do_find_symbol(str, package)
        return ((s, symbol_relation(s, package)) if s is not None else
                (None, None))

def symbol_accessible_in(x, package):
        return (x.name in package.accessible and
                package.accessible[x.name] is x)

@boot_defun
def find_symbol(str, package = None):
        return do_find_symbol(str, coerce_to_package(package))

@boot("print", lambda _, s, **__:
              (("#"            if not s.package                               else
                ""             if s.package is __keyword or s.package is __cl else
                s.package.name) + (""  if s.package is __cl                                                         else
                                   ":" if (not s.package or s.name in s.package.external or s.package is __keyword) else
                                   "::") + s.name))
def print_symbol(s, escape = None, gensym = None, case = None, package = None, readably = None):
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
        package  = defaulted_to_var(package,  _package_)
        if not packagep(package):
                here("------------------------------------------------------------\npackage is a %s: %s" % (type_of(package), package,))
        readably = defaulted_to_var(readably, _print_readably_)
        escape   = defaulted_to_var(escape,   _print_escape_) if not readably else t
        case     = defaulted_to_var(case,     _print_case_)   if not readably else make_keyword("UPCASE")
        gensym   = defaulted_to_var(gensym,   _print_gensym_) if not readably else t
        # Because the #: syntax does not intern the following symbol, it is
        # necessary to use circular-list syntax if *PRINT-CIRCLE* is true and
        # the same uninterned symbol appears several times in an expression to
        # be printed. For example, the result of
        #
        # (let ((x (make-symbol "FOO"))) (list x x))
        #
        # would be printed as (#:FOO #:FOO) if *PRINT-CIRCLE* were
        # false, but as (#1=#:FOO #1#) if *PRINT-CIRCLE* were true.
        return ((""                       if not escape                       else
                 ":"                      if s.package is __keyword           else
                 ""                       if symbol_accessible_in(s, package) else
                 ("#:" if gensym else "") if not s.package                    else
                 (s.package.name + (":"
                                    if s in s.package.external else
                                    "::"))) +
                case_xform(case, s.name))

def core_package_init():
        global __cl, __keyword
        __cl      = make_package("COMMON-LISP", nicknames = ["CL"])
        __keyword = make_package("KEYWORD")

core_package_init()

def do_intern_symbol(s, p):
        p.own.add(s)
        p.accessible[s.name], s.package = s, p
        if p is __keyword: # CLHS 11.1.2.3.1 Interning a Symbol in the KEYWORD Package
                p.external.add(s)
        return s

def cold_make_nil():
        nil = symbol_t.__new__(symbol_t)
        (nil.name,
         nil.package,
         nil.function,
         nil.macro_function,
         nil.compiler_macro_function,
         nil.symbol_macro_expansion,
         nil.known) = "NIL", __cl, nil, nil, nil, None, nil
        nil.symbol_pyname, nil.function_pyname = None, None
        return do_intern_symbol(nil, __cl)

NIL = nil = cold_make_nil()

# Package system core

def intern_in_package(x, p):
        s, presentp = (error("X must be a string: %s.", repr(x)) if not isinstance(x, str) else
                       (p.accessible.get(x), True)                   if x in p.accessible              else
                       (None,                False))
        if not presentp:
                s = do_intern_symbol(make_symbol(x), p)
        return s, presentp

def coerce_to_package(x, if_null = "current"):
        return (find_package(x)                                              if isinstance(x, (str, symbol_t, package_t)) else
                (symbol_value(_package_) if if_null == "current" else
                 package_not_found_error(x))                                if (not x)                                   else
                simple_type_error("COERCE-TO-PACKAGE accepts only package designators -- packages, strings or symbols, was given '%s' of type %s.",
                                  x, type_of(x)))

@boot("symbol", lambda intern, x, package = None:
              intern(x, package or __cl))
def do_intern(x, package = None):
        "A version of INTERN, that does not compute the relationship between SYMBOL and designated PACKAGE."
        return intern_in_package(x, find_package(package) if package else
                                     symbol_value(_package_))

def make_keyword(s, upcase = True):
        return do_intern((s.upper() if upcase else s),
                         __keyword)[0]

def use_package_symbols(dest, src, syms):
        conflict_set = { x.name for x in syms.values() } & set(dest.accessible.keys())
        for name in conflict_set:
                if syms[name] is not dest.accessible[name]:
                        symbol_conflict_error("USE-PACKAGE", src, dest, syms[name], dest.accessible[name])
        ## no conflicts anymore? go on..
        for name, sym in syms.items():
                dest.inherited[sym].add(src)
                if name not in dest.accessible: # Addition of this conditional is important for package use loops.
                        dest.accessible[name] = sym
                        # if dest.name == "SWANK" and src.name == "INSPECTOR":
                        #         dprintf("merging %s into %s: test: %s", s, dest, read_symbol(print_nonkeyword_symbol(s)))

@boot_defun
def use_package(dest, src):
        dest, src = coerce_to_package(dest), coerce_to_package(src)
        symhash = map_into_hash(lambda x: (x.name, x), src.external)
        use_package_symbols(dest, src, symhash)
        src.packages_using.add(dest)
        dest.used_packages.add(src)

@boot_defun
def intern(x, package = None):
        package = coerce_to_package(package)
        s, found_in_package = do_intern(x, package)
        return s, (symbol_relation(s, package) if found_in_package else
                   None)

@boot_defun
def defpackage(name, use = [], export = []):
        p = make_package(name, use = use)
        for symname in export:
                not_implemented("DEFPACKAGE: :EXPORT keyword") # XXX: populate the for-INTERN-time-export set of names
        return p

@boot_defun
def in_package(name):
        string_set("*PACKAGE*", coerce_to_package(name), force_toplevel = t)

@boot_defun
def export(symbols, package = None):
        symbols, package = symbols if isinstance(symbols, list) else [symbols], coerce_to_package(package)
        assert(all(isinstance(x, symbol_t)
                   for x in symbols))
        symdict = map_into_hash(lambda x: (x.name, x), symbols)
        for user in package.packages_using:
                use_package_symbols(user, package, symdict)
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
        p = coerce_to_package(package)
        symbols = vectorise_linear(ensure_list(symbols))
        module = find_module(frost.lisp_symbol_name_python_name(package_name(p)),
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
                                not_implemented("Namespace merging.")
                                # Issue SYMBOL-VALUES-NOT-SYNCHRONISED-WITH-PYTHON-MODULES
                                # python_name = frost.lisp_symbol_name_python_name(s.name)
                                # module.__dict__[python_name] = ???
        return t

# Package system init

def protosymbolicate(x, name, slot):
        sym, _ = do_intern(name)
        setattr(sym, slot, x)
        return sym

def symbolicate(x, name, slot, globals):
        sym = protosymbolicate(x, name, slot)
        pyname = frost.lisp_symbol_name_python_name(name)
        frost.setf_global(sym, pyname, globals, force = t)

def init_package_system_0():
        global __packages__
        global t, T, make_symbol, make_package
        __packages__ = make_hash_table()
        T = t              = intern("T", __cl)[0]     # Nothing much works without this.
        nil.__contains__   = lambda _: False
        nil.__getitem__    = lambda _, __: nil
        nil.__length__     = lambda _: 0
        nil.__iter__       = lambda _: None
        nil.__reversed__   = lambda _: None
        __global_scope__.update({ "T": t, "NIL": nil })
        # secondary
        package_t("COMMON-LISP-USER", use = [__cl], boot = True)
        __global_scope__["*PACKAGE*"] = __cl # COLD-SETQ
        protosymbolicate(symbol_t, "SYMBOL", "python_type")
        @boot_defun
        def make_symbol(name):
                return symbol_t(name)
        protosymbolicate(package_t, "PACKAGE", "python_type")
        @boot_defun
        def make_package(name, nicknames = [], use = []):
                if nicknames:
                        not_implemented("In MAKE-PACKAGE %s: package nicknames are ignored.", repr(name))
                return package_t(name if isinstance(name, str) else symbol_name(name),
                                 ignore_python = True, use = [])

init_package_system_0()

unboot_set("symbol")
# unboot_set("print") # This can turn 4.8s of debug printing into 30+s

# GENSYM

__gensym_counter__ = 0

def gensymname(x = "N"):
        # Unregistered Issue GENSYM-NOT-THREAD-SAFE
        global __gensym_counter__
        __gensym_counter__ += 1
        return x + str(__gensym_counter__)

@boot_defun
def gensym(x = "G"):
        # A version adding a name is defined later: GENSYM-TN.
        return make_symbol(gensymname(x))

# Dynamic scope

class progv():
        ## Unregistered Issue PYTHON33-BROKE-NEW-TIME-CLASS-DISPATCH
        # def __new__(cls, args):
        #         return withless() if not args[0] else object.__new__(cls)
        def __init__(self, cluster):
                self.cluster = cluster
        def __enter__(self):
                dynamic_scope_push(self.cluster)
        def __exit__(self, t, v, tb):
                dynamic_scope_pop()

class dynamic_scope():
        "Courtesy of Jason Orendorff."
        def let(self, **keys):
                return progv(keys)
        def maybe_let(self, p, **keys):
                return progv(keys) if p else None
        def __getattr__(self, name):
                return symbol_value(name)
        def __setattr__(self, name, value):
                error(AttributeError, "Use SET to set special globals.")

__dynamic_scope__ = dynamic_scope()
env = __dynamic_scope__             # shortcut..

# CATCH, THROW, BLOCK, RETURN-FROM

# WARNING: non-specific try/except clauses and BaseException handlers break this!
class __catcher_throw__(cold_condition_type):
        def __init__(self, ball, value, reenable_pytracer = nil):
                self.ball, self.value, self.reenable_pytracer = ball, value, reenable_pytracer
        def __str__(self):
                return "@<ball %s>" % (self.ball,)

def catch(ball, body):
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
                frost.enable_pytracer()

def throw(ball, value):
        "Stack this seeks, like mad, like the real one."
        raise __catcher_throw__(ball = ball, value = value, reenable_pytracer = boundp(_signalling_frame_))

def __block__(fn):
        "An easy decorator-styled interface for block establishment."
        nonce = gensym("BLOCK-")
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

@boot_defun
def return_from(nonce, value):
        nonce = ((getattr((symbol_function(nonce) if isinstance(nonce, symbol_t) else
                           nonce), "ball", None) or
                  error("RETURN-FROM was handed a function %s, but it is not cooperating in the "
                        "__BLOCK__ nonce passing syntax.", nonce)) if isinstance(nonce, cold_function_type) else
                 ## This can mean either the @defun-ned function, or absent a function definition, the symbol itself.
                 (getattr(nonce.function, "ball", nonce))          if isinstance(nonce, symbol_p)            else
                 nonce                                             if isinstance(nonce, str)                 else
                 error("In RETURN-FROM: nonce must either be a string, or a function designator;  was: %s.", repr(nonce)))
        throw(nonce, value)

# Condition system: SIGNAL

## standard globals:
string_set("*DEBUGGER-HOOK*",         nil)
string_set("*INVOKE-DEBUGGER-HOOK*",  nil)

## non-standard:
string_set("*HANDLER-CLUSTERS*", [])
string_set("*PRESIGNAL-HOOK*",   nil)
string_set("*PREHANDLER-HOOK*",  nil)

def set_condition_handler(fn):
        frost.set_tracer_hook("exception", fn)

@boot_defun
def signal(cond):
        handler_clusters = symbol_value(_handler_clusters_)
        for n, cluster in enumerate(reversed(handler_clusters)):
                ## Unregistered Issue CLUSTERS-NOT-PROPERLY-UNWOUND-FOR-HANDLERS
                for type, handler in cluster:
                        if not isinstance(type, str):
                                if isinstance(cond, type):
                                        hook = symbol_value(_prehandler_hook_)
                                        if hook:
                                                frame = assoc("__frame__", cluster)
                                                assert(frame)
                                                hook(cond, frame, hook)
                                        with progv({ _handler_clusters_: handler_clusters[:-(n + 1)]}):
                                                handler(cond)
        return nil

def run_hook(variable, condition):
        old_hook = symbol_value(variable)
        if old_hook:
                with progv({ variable: nil }):
                        old_hook(condition, old_hook)

# Stab at INVOKE-DEBUGGER

def flush_standard_output_streams():
        warn_not_implemented()

def funcall_with_debug_io_syntax(function, *args, **keys):
        warn_not_implemented()
        return function(*args, **keys)

intern_and_bind_globals("*DEBUG-CONDITION*", "*DEBUG-RESTARTS*", "*NESTED-DEBUG-CONDITION*")

def show_restarts(restarts, stream):
        warn_not_implemented()

def do_invoke_debugger(condition):
        ## SBCL is being careful to not handle STEP-CONDITION here..
        with progv({_debug_condition_: condition,
                    _debug_restarts_: compute_restarts(condition),
                    _nested_debug_condition_: nil }):
                def error_handler_body(condition):
                        string_set("*NESTED-DEBUG-CONDITION*", condition)
                        ndc_type = type_of(condition)
                        format(symbol_value(_error_output_),
                               "\nA %s was caught when trying to print %s when "
                               "entering the debugger. Printing was aborted and the "
                               "%s was stored in %s.\n",
                               ndc_type, _debug_condition_, ndc_type, _nested_debug_condition_)
                        if isinstance(condition, cell_error_t):
                                format(symbol_value(_error_output_),
                                       "\n(CELL-ERROR-NAME %s) = %s\n",
                                       _nested_debug_condition_, cell_error_name(condition))
                handler_case(lambda: print_debugger_invocation_reason(condition,
                                                                       symbol_value(_error_output_)),
                             (error_t, error_handler_body))
                try:
                        pass
                finally:
                        with progv({ _standard_output_: symbol_value(_standard_output_),
                                     _error_output_:    symbol_value(_debug_io_) }):
                                format(symbol_value(_debug_io_), "\nType HELP for debugger help, or (VPCL:QUIT) to exit from VPCL.\n\n")
                                show_restarts(symbol_value(_debug_restarts_), symbol_value(_debug_io_))
                                internal_debug()

@boot_defun
def invoke_debugger(condition):
        "XXX: non-compliant: doesn't actually invoke the debugger."
        run_hook(_invoke_debugger_hook_, condition)
        run_hook(_debugger_hook_, condition)
        if not (packagep(symbol_value(_package_)) and
                package_name(symbol_value(_package_))):
                string_set("*PACKAGE*", find_package("CL-USER"))
                format(symbol_value(_error_output_),
                       "The value of %s was not an undeleted PACKAGE. It has been reset to %s.",
                       _package_, symbol_value(_package_))
        flush_standard_output_streams()
        return funcall_with_debug_io_syntax(do_invoke_debugger, condition)

# Type predicates

def integerp(o):      return isinstance(o, int)
def floatp(o):        return isinstance(o, float)
def complexp(o):      return isinstance(o, complex)
def numberp(o):       return isinstance(o, (int, float, complex))
def hash_table_p(o):  return isinstance(o, cold_hash_table_type)
def listp(o):        return isinstance(o, cold_list_type)
def boolp(o):        return isinstance(o, bool)
def sequencep(x):     return getattr(type(x), "__len__", None) is not None

# Types mappable to python

def define_python_type_map(symbol_or_name, type_):
        not isinstance(symbol_or_name, (str, symbol_t)) and \
            error("In DEFINE-PYTHON-TYPE-MAP: first argument must be either a string or a symbol, was: %s.", repr(symbol_or_name))
        not isinstance(type_, type) and \
            error("In DEFINE-PYTHON-TYPE-MAP: second argument must be a Python type, was: %s.", repr(type_))
        symbol = (symbol_or_name if symbolp(symbol_or_name) else
                  intern(symbol_or_name)[0])
        protosymbolicate(type_, symbol.name, "python_type")
        frost.setf_global(type_, frost.lisp_symbol_name_python_type_name(symbol.name),
                           globals = globals())
        symbol.python_type = type_
        return symbol

define_python_type_map("INTEGER",           int)
define_python_type_map("FLOAT",             float)
define_python_type_map("COMPLEX",           complex)

define_python_type_map("STRING",            str)
define_python_type_map("HASH-TABLE",        cold_hash_table_type)

define_python_type_map("FUNCTION",          cold_function_type)

define_python_type_map("STREAM",            cold_stream_type)

define_python_type_map("CLASS",             type) # Ha.

define_python_type_map("CONDITION",         BaseException)
define_python_type_map("ERROR",             Exception)
define_python_type_map("SERIOUS-CONDITION", Exception)
define_python_type_map("END-OF-FILE",       EOFError)

## non-standard type names
define_python_type_map("PYBOOL",      bool)
define_python_type_map("PYBYTES",     bytes)
define_python_type_map("PYBYTEARRAY", bytearray)
define_python_type_map("PYSET",       set)
define_python_type_map("PYFROZENSET", frozenset)

# Complex type specifier machinery: %TYPE-MISMATCH, @DEFTYPE, TYPEP

def type_specifier_complex_p(x):
        """Determines, whether a type specifier X constitutes a
complex type specifier."""
        return isinstance(x, tuple)

def invalid_type_specifier_error(x, complete_type = None):
        error("%s is not a valid type specifier%s.",
              x, ("" if not complete_type else
                  (" (within type specifier %s)" % (complete_type,))))

def complex_type_mismatch(x, type):
        ret = type[0].type_predicate(x, type)
        if isinstance(ret, tuple) and len(ret) != 3:
                error("Type matcher for %s returned an invalid value: %s.", type[0], repr(ret))
        return (ret if not (isinstance(ret, tuple) and ret[2]) else
                invalid_type_specifier_error(ret[1], complete_type = type))

def type_mismatch(x, type_):
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
                 complex_type_mismatch(x, tuple([type_]))     if hasattr(type_, "type_predicate")      else
                 invalid_type_specifier_error(type_))         if isinstance(type_, symbol_t)           else
                complex_type_mismatch(x, type_)               if (isinstance(type_, tuple) and type_ and
                                                                   hasattr(type_[0], "type_predicate")) else
                invalid_type_specifier_error(type_))

@boot_defun
def typep(x, type):
        return not type_mismatch(x, type)

def deftype(type_name_or_fn, globals = None):
        def do_deftype(fn, type_name = type_name_or_fn):
                nonlocal globals
                old_global_name = (type_name_or_fn.__name__ if functionp(type_name_or_fn) else
                                   fn.__name__)
                globals = defaulted(globals, py.globals())
                old_global = (global_(old_global_name, globals)
                              or builtins.__dict__.get(old_global_name, None)
                              or None)
                symbol = intern(type_name)[0]
                symbol.type_predicate = fn
                frost.setf_global(symbol, old_global_name + ("" if old_global_name.endswith("_") else "_") + "t",
                                   globals)
                return old_global
        return (do_deftype(type_name_or_fn, type_name = frost.python_name_lisp_symbol_name(type_name_or_fn.__name__)) if functionp(type_name_or_fn) else
                do_deftype                                                                                             if isinstance(type_name_or_fn, str)   else
                error("In DEFTYPE: argument must be either a function or a string, was: %s.",
                      repr(symbol_name_or_fn)))

@boot_defun
def the(type, x):
        mismatch = type_mismatch(x, type)
        return (x if not mismatch else
                error(simple_type_error_t,
                      format_control = "The value %s (of type %s) is not of type %s%s.",
                      format_arguments = (x, type_of(x), type,
                                          ("" if (not type_specifier_complex_p(type)) or type is mismatch[1] else
                                              (", specifically, the value %s is not of type %s" % (princ_to_string(mismatch[0]), mismatch[1]))))))

@boot_defun
def check_type(x, type):
        the(type, x)

def of_type(x):
        return lambda y: typep(y, x)

def not_of_type(x):
        return lambda y: not typep(y, x)

# Complex type definitions

@deftype
def boolean(x, type):
        return ((x, type, True)  if len(type) is not 1 else
                (x, type, False) if x not in [t, nil]      else
                nil)

@deftype
def null(x, type):
        return ((x, type, True)  if len(type) is not 1 else
                (x, type, False) if x is not nil           else
                nil)

@deftype
def keyword(x, type):
        return ((x, type, True)  if len(type) is not 1 else
                (x, type, False) if not keywordp(x)        else
                nil)

@deftype("OR")
def or_(x, type):
        return ((x, type, False) if len(type) is 1 else
                poor_man_let(list(type_mismatch(ix, ty) for ix, ty in zip([x] * (len(type) - 1), type[1:])),
                              lambda mismatches:
                                      (some_fast(lambda m: m and m[2] and m, mismatches) or
                                       (all(mismatches) and (x, type, False)))))

@deftype("AND")
def and_(x, type):
        return (nil       if len(type) is 1 else
                some_fast(lambda ix: type_mismatch(x, ix), type[1:]))

@deftype("NOT")
def not_(x, type):
        return ((x, type, True) if len(type) is not 2 else
                poor_man_let(type_mismatch(x, type[1]),
                              lambda m: ((x, type, False) if not m      else
                                         m                if m and m[2] else
                                         nil)))

@deftype
def member(x, type):
        return ((x not in type[1:]) and
                (x, type, False))

@deftype
def satisfies(x, type):
        return ((x, type, True) if ((len(type) is not 2) or
                                    not isinstance(type[1], cold_function_type)) else
                ((not type[1](x)) and
                 (x, type, False)))

@deftype
def eql(x, type):
        return ((x, type, True) if len(type) is not 2 else
                ((not eql(x, type[1])) and
                 (x, type, False)))

@deftype
def unsigned_byte(x, type):
        return (((x, type, False) if not isinstance(x, int) or minusp(x) else nil)                        if len(type) is 1 else
                ((x, type, False) if not isinstance(x, int) or minusp(x) or (x >= 1 << type[1]) else nil) if len(type) is 2 else
                (x, type, True))

## Non-standard
@deftype
def maybe(x, type):
        return ((x, type, True)  if len(type) is not 2 else
                poor_man_let(type_mismatch(x, type[1]),
                              lambda m: (nil if not m                         else
                                         m   if ((m and m[2]) or
                                                 not (x is nil or x is None)) else
                                         nil)))

@deftype
def pylist(x, type):
        return ((x, type, True)  if len(type) is not 2      else
                (x, type, False) if not isinstance(x, list) else
                some_fast(lambda ix: type_mismatch(ix, type[1]), x))

@deftype
def homotuple(x, type):
        return ((x, type, True)  if len(type) is not 2       else
                (x, type, False) if not isinstance(x, tuple) else
                some_fast(lambda ix: type_mismatch(ix, type[1]), x))

@deftype
def pyseq(x, type):
        return ((x, type, True)  if len(type) is not 2               else
                (x, type, False) if not isinstance(x, (list, tuple)) else
                some_fast(lambda ix: type_mismatch(ix, type[1]), x))

@deftype
def cons(x, type):
        return ((x, type, True)                           if len(type) not in (1, 3)                   else
                (x, type, False)                          if not (isinstance(x, list) and len(x) == 2) else
                some_fast_2(type_mismatch, x, type[1:]) if len(type) is 3                            else
                nil) 

@deftype
def list(x, type):
        return ((x, type, True)                           if len(type) not in (1, 3)                   else
                (x, type, False)                          if not ((isinstance(x, list) and len(x) == 2)
                                                                  or x is nil)                         else
                nil)

@deftype
def pyfixlist(x, type):
        return ((x, type, False) if not (isinstance(x, list) and len(x) == len(type) - 1) else
                some_fast_2(type_mismatch, x, type[1:]))

@deftype
def pytuple(x, type):
        return ((x, type, False) if not (isinstance(x, tuple) and len(x) == len(type) - 1) else
                some_fast_2(type_mismatch, x, type[1:]))

@deftype
def pyanytuple(x, type):
        return ((x, type, False) if not (isinstance(x, tuple)) else
                nil)
# Unregistered Issue TEACHABLE-TYPE-CHECKING-PRACTICE-AND-TOOL-CONSTRUCTION

@deftype
def partuple(x, type):
        return ((x, type, False) if not (isinstance(x, tuple) and len(x) >= len(type) - 1) else
                some_fast_2(type_mismatch, x, type[1:]))

__variseq__ = (pytuple_t, (eql_t, maybe_t), t) # Meta-type, heh..
@deftype
def varituple(x, type):
        # correctness enforcement over speed?
        fixed_t, maybes_t = prefix_suffix_if_not(of_type(__variseq__), type[1:])
        if not all(typep(x, __variseq__) for x in maybes_t):
                return (x, type, True)   # fail
        fixlen = len(fixed_t)
        ctype = (or_t,) + tuple(t[1] for t in maybes_t)
        return ((x, type) if len(x) < fixlen else
                some_fast_2(type_mismatch, x[:fixlen], fixed_t) or
                some_fast(lambda ix: type_mismatch(ix, ctype), x[fixlen:]))

def eql_type_specifier_p(x): return isinstance(x, tuple) and len(x) is 2 and x[0] is eql_t

unboot_set("typep")

# Type relationships, rudimentary

def subtypep(sub, super):
        def coerce_to_python_type(x):
                return (x             if isinstance(x, cold_class_type)   else
                        x.python_type if isinstance(x, symbol_t)           else
                        error("In SUBTYPEP: arguments must be valid type designators, but %s wasn't one.", repr(x)))
        def do_subclass_check(sub, super):
                return issubclass(coerce_to_python_type(sub),
                                      coerce_to_python_type(super))
        return (do_subclass_check(sub, super)                  if super is not t                                     else
                not_implemented("complex type relatioships: %s vs. %s.",
                                 sub, super)                   if isinstance(sub, tuple) or isinstance(super, tuple) else
                error("%s is not a valid type specifier", sub) if not (typep(sub, (or_t, type, (eql_t, t))) and
                                                                       typep(sub, (or_t, type, (eql_t, t))))         else
                sub is super or super is t)

# Toplevel definitions: @DEFUN and @DEFCLASS

doit = False
def make_cold_definer(definer_name, predicate, slot, preprocess, mimicry):
        def cold_definer(name_or_obj):
                obj, sym, name = interpret_toplevel_value(name_or_obj, predicate)
                def do_cold_def(o):
                        setattr(sym, slot, o)
                        # symbol = (intern(defaulted(name, frost.python_name_lisp_symbol_name(o.__name__)))[0]
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

defun            = cold_defun    = make_cold_definer("%COLD-DEFUN",    functionp,
                                                       "function",    identity, frost.make_object_like_python_function)
defclass         = cold_defclass = make_cold_definer("%COLD-DEFCLASS", lambda x: isinstance(x, type),
                                                       "python_type", identity,  frost.make_object_like_python_class)
defun_with_block = cold_defun_with_block = make_cold_definer("%COLD-DEFUN-WITH-BLOCK", functionp,
                                                               "function", __block__, frost.make_object_like_python_function)
for fn  in __boot_defunned__:   frost.setf_global(defun(fn),     fn.__name__,  globals(), force = t)
for cls in __boot_defclassed__: frost.setf_global(defclass(cls), cls.__name__, globals(), force = t)
doit = True

# Delayed class definitions

@defclass
class nil():
        @classmethod
        def __instancecheck__(_, __): return False # This is an empty type
symbolicate(nil, "NIL", "python_type", globals())

@defclass
class t():
        @classmethod
        def __instancecheck__(_, __): return True  # This is the absolute sum type
symbolicate(t, "T", "python_type", globals())

def attrify_args(self, locals, *names):
        for name in names:
                setattr(self, name, locals[name])

@defclass
class simple_condition_t(condition_t):
        def __init__(self, format_control, format_arguments):
                attrify_args(self, locals(), "format_control", "format_arguments")
                # dprintf("About to signal a simple condition of type %s:\n%s", type(self), self)
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

intern_and_bind_symbols("%MV-MARKER")

@defun
def values(*rest):
        return (_mv_marker,) + rest

def values_frame_p(x):
        return isinstance(x, tuple) and x[0] is _mv_marker

def values_frame_values(x):
        return x[1:]

def values_frame_project(n, values_form):
        return ((nil if n > len(values_form) - 2 else
                 values_form[n + 1])
                if values_frame_p(values_form) else
                (nil if n else values_form))

# Early object system

@defun
def find_class(x, errorp = t):
        not_implemented()

@defun
def make_instance(class_or_name, **initargs):
        return (class_or_name             if isinstance(class_or_name, cold_class_type) else
                class_or_name.python_type if isinstance(class_or_name, symbol_t)         else
                error("In MAKE-INSTANCE %s: first argument must be a class specifier.", class_or_name))(**initargs)

def make_missing_method(cls, name):
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
class readtable_t(collections.UserDict):
        def __init__(self, case = make_keyword("upcase")):
                self.case = the((member_t, make_keyword("upcase"), make_keyword("downcase"), make_keyword("preserve"), make_keyword("invert")),
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

intern_and_bind_globals("*PRINT-ARRAY*", "*PRINT-BASE*", "*PRINT-CASE*", "*PRINT-CIRCLE*",
                         "*PRINT-ESCAPE*", "*PRINT-GENSYM*", "*PRINT-LENGTH*", "*PRINT-LEVEL*",
                         "*PRINT-LINES*", "*PRINT-MISER-WIDTH*", "*PRINT-PPRINT-DISPATCH*",
                         "*PRINT-PRETTY*", "*PRINT-RADIX*", "*PRINT-READABLY*", "*PRINT-RIGHT-MARGIN*",
                         "*READ-BASE*", "*READ-DEFAULT-FLOAT-FORMAT*", "*READ-EVAL*",
                         "*READ-SUPPRESS*",
                         "*READTABLE*")
__standard_io_syntax__ = dict({_package_               : find_package("COMMON-LISP-USER"),
                               _print_array_           : t,
                               _print_base_            : 10,
                               _print_case_            : make_keyword("UPCASE"),
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
        with progv(__standard_io_syntax__):
                return body()

def set_settable_standard_globals():
        string_set("*READ-CASE*", make_keyword("UPCASE"))
        string_set("*FEATURES*",  [])
        string_set("*MODULES*",   [])
        string_set("*STANDARD-INPUT*",  sys.stdin)
        string_set("*STANDARD-OUTPUT*", sys.stdout)
        string_set("*ERROR-OUTPUT*",    sys.stderr)
        string_set("*PRINT-ARRAY*",           __standard_io_syntax__[_print_array_])
        string_set("*PRINT-BASE*",            __standard_io_syntax__[_print_base_])
        string_set("*PRINT-CASE*",            __standard_io_syntax__[_print_case_])
        string_set("*PRINT-CIRCLE*",          __standard_io_syntax__[_print_circle_])
        string_set("*PRINT-GENSYM*",          __standard_io_syntax__[_print_gensym_])
        string_set("*PRINT-ESCAPE*",          __standard_io_syntax__[_print_escape_])
        string_set("*PRINT-LENGTH*",          __standard_io_syntax__[_print_length_])
        string_set("*PRINT-LEVEL*",           __standard_io_syntax__[_print_level_])
        string_set("*PRINT-LINES*",           __standard_io_syntax__[_print_lines_])
        string_set("*PRINT-MISER-WIDTH*",     __standard_io_syntax__[_print_miser_width_])
        string_set("*PRINT-PPRINT-DISPATCH*", __standard_io_syntax__[_print_pprint_dispatch_])
        string_set("*PRINT-PRETTY*",          __standard_io_syntax__[_print_pretty_])
        string_set("*PRINT-RADIX*",           __standard_io_syntax__[_print_radix_])
        string_set("*PRINT-READABLY*",        __standard_io_syntax__[_print_readably_])
        string_set("*PRINT-RIGHT-MARGIN*",    __standard_io_syntax__[_print_right_margin_])
        string_set("*READ-BASE*",                 __standard_io_syntax__[_read_base_])
        string_set("*READ-DEFAULT-FLOAT-FORMAT*", __standard_io_syntax__[_read_default_float_format_])
        string_set("*READ-EVAL*",                 __standard_io_syntax__[_read_eval_])
        string_set("*READ-SUPPRESS*",             __standard_io_syntax__[_read_suppress_])
        string_set("*READTABLE*",                 __standard_io_syntax__[_readtable_])

set_settable_standard_globals()

# Derived names:  %NoneType, REDUCE, SORT, %CURRY, STRINGP, %CLASSP, %NONEP etc.

NoneType         = type(None)

reduce            = functools.reduce
repeat            = itertools.repeat
sort              = sorted
curry             = functools.partial

stringp           = neutrality.stringp
do_write_string   = neutrality.do_write_string

def classp(x):     return isinstance(x, type)
def frozensetp(o): return isinstance(o, frozenset)
def setp(o):       return isinstance(o, (set, frozenset))
def nonep(o):      return o is None

# Constants

most_positive_fixnum = 67108864

def poor_man_let(*values_and_body):
        values, body = values_and_body[:-1], values_and_body[-1]
        return body(*values)

def poor_man_defstruct(name, *slots):
        return collections.namedtuple(name, slots)

def poor_man_when(test, body):
        if test:
                return body() if isinstance(body, cold_function_type) else body

def poor_man_case(val, *clauses):
        for (cval, body) in clauses:
                if ((val == cval or (cval is True) or (cval is t)) if not isinstance(cval, list) else
                    val in cval):
                        return body() if isinstance(body, cold_function_type) else body

def poor_man_ecase(val, *clauses):
        for (cval, body) in clauses:
                if ((val == cval) if not isinstance(cval, list) else
                    val in cval):
                        return body() if isinstance(body, cold_function_type) else body
        error("%s fell through ECASE expression. Wanted one of %s.", val, [ x[0] for x in clauses ])

def poor_man_typecase(val, *clauses):
        for (ctype, body) in clauses:
                if (ctype is t) or (ctype is True) or typep(val, ctype):
                        return body() if isinstance(body, cold_function_type) else body

def poor_man_etypecase(val, *clauses):
        for (ctype, body) in clauses:
                if (ctype is t) or (ctype is True) or typep(val, ctype):
                        return body() if isinstance(body, cold_function_type) else body
        else:
                simple_type_error("%s fell through ETYPECASE expression. Wanted one of (%s).",
                                  val, ", ".join((c[0].__name__ for c in clauses)))

def cold_constantp(form):
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
constantp = cold_constantp

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

case_attribute_map = dict(UPCASE     = string_upcase,
                               DOWNCASE   = string_downcase,
                               CAPITALIZE = string_capitalize,
                               PRESERVE   = identity)
def case_xform(type_, s):
        if not (isinstance(type_, symbol_t) and type_.package.name == "KEYWORD"):
                error("In CASE-XFORM: case specifier must be a keyword, was a %s: %s.", type(type_), print_symbol(type_))
        return case_attribute_map[type_.name](s)

# Possibly dangling cold boot code

#     I wonder if this boot state infrastructure is a good idea:
#     - it tangles the flow of things (?)

def cold_format(destination, control_string, *args):
        string = control_string % args
        if not destination:
                return string
        else:
                write_string(string, sys.stderr if destination is t else destination)
format = cold_format
def cold_princ_to_string(x):
        return repr(x)
princ_to_string = cold_princ_to_string
# Unregistered Issue PACKAGE-INIT-MUST-TAKE-COLD-SYMBOL-VALUES-INTO-ACCOUNT
def cold_probe_file(pathname):
        assert(isinstance(pathname, str))
        return os.path.exists(the(string_t, pathname))
probe_file = cold_probe_file

# Python module compilation

def load_code_object_as_module(name, co, filename = "", builtins = None, globals = None, locals = None, register = True):
        check_type(co, type(load_code_object_as_module.__code__))
        mod = imp.new_module(name)
        mod.__filename__ = filename
        if builtins:
                mod.__dict__["__builtins__"] = builtins
        if register:
                sys.modules[name] = mod
        globals = defaulted(globals, mod.__dict__)
        locals  = defaulted(locals, mod.__dict__)
        exec(co, globals, locals)
        return mod, globals, locals

def load_text_as_module(name, text, filename = "", **keys):
        return load_code_object_as_module(name, py.compile(text, filename, "exec"),
                                           filename = filename, **keys)[0]

def reregister_module_as_package(mod, parent_package = None):
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

def py_compile_and_load(*body, modname = "", filename = "", lineno = 0, **keys):
        return load_code_object_as_module(
                modname,
                py.compile(ast.fix_missing_locations(ast_module(list(body), lineno = lineno)), filename, "exec"),
                register = nil,
                filename = filename,
                **keys)

def ast_compiled_name(name, *body, function = nil, **keys):
        mod, globals, locals = py_compile_and_load(*body, **keys)
        return locals[function or name]

# Python frames

def all_threads_frames():
        return sys._current_frames()

def this_frame():
        return sys._getframe(1)

frame = type(this_frame())

def framep(x):
        return isinstance(x, frame)

def next_frame(f):
        return f.f_back if f.f_back else error("Frame \"%s\" is the last frame.", pp_frame(f, lineno = True))

def caller_frame(caller_relative = 0):
        return sys._getframe(caller_relative + 2)

def frames_calling(f = None, n = -1):
        "Semantics of N are slightly confusing, but the implementation is so simple.."
        f = caller_frame() if f is None else the(frame, f)
        acc = [f]
        while f.f_back and n:
                f, n = f.f_back, n - 1
                acc.append(f)
        return acc

def caller_name(n = 0):
        return fun_name(frame_fun(sys._getframe(n + 2)))

def caller_args(n = 0):
        return frame_locals(sys._getframe(n + 2))

def exception_frame():
        return sys.exc_info()[2].tb_frame

def top_frame():
        return caller_frame()

def frame_info(f):
        "Return frame (function, lineno, locals, globals, builtins)."
        return (f.f_code,
                f.f_lineno,
                f.f_locals,
                f.f_globals,
                f.f_builtins,
                )

# Issue FRAME-CODE-OBJECT-IS-NOT-FUN
def frame_fun(f):               return f.f_code
def frame_lineno(f):            return f.f_lineno
def frame_locals(f):            return f.f_locals
def frame_globals(f):           return f.f_globals
def frame_local_value(f, name): return f.f_locals[name]

### XXX: this is the price of Pythonic pain
__ordered_frame_locals__ = dict()
def frame_ordered_locals(f):
        global __ordered_frame_locals__
        if f not in __ordered_frame_locals__:
                __ordered_frame_locals__[f] = list(f.f_locals.keys())
        return __ordered_frame_locals__[f]

def fun_info(f):
        "Return function (name, params, filename, lineno, nlines)."
        return (f.co_name or "<unknown-name>",
                f.co_varnames[:f.co_argcount], # parameters
                f.co_filename or "<unknown-file>",
                f.co_firstlineno,
                1 + max(f.co_lnotab or [0]),        # lines
                f.co_varnames[f.co_argcount:], # non-parameter bound locals
                f.co_freevars,
                )
def fun_name(f):        return f.co_name
def fun_filename(f):    return f.co_filename
def fun_firstlineno(f): return f.co_firstlineno
def fun_bytecode(f):    return f.co_code
def fun_constants(f):   return f.co_consts

# Frame pretty-printing

def frame_fun_name(f):          return f.f_code.co_name

def print_function_arglist(f):
        argspec = inspect.getargspec(f)
        return ", ".join(argspec.args +
                         (["*" + argspec.varargs]   if argspec.varargs  else []) +
                         (["**" + argspec.keywords] if argspec.keywords else []))

def pp_frame(f, align = None, handle_overflow = None, lineno = None, frame_id = None):
        fun = frame_fun(f)
        fun_name, fun_params, filename = fun_info(fun)[:3]
        align = ((align or 10) if handle_overflow else
                 defaulted(align, 0))
        return ("%s%s%s %s(%s)" % (((frame_id(f)[:4] + " ") if frame_id else ""),
                                   filename + ("" if align else ":") + (" " * (align - (len(filename) % align if align else 0))),
                                   ("%d:" % frame_lineno(f)) if lineno else "",
                                   fun_name, ", ".join(fun_params)))

def print_frame(f, stream = None, **keys):
        write_string(pp_frame(f, **keys), defaulted_to_var(stream, _debug_io_))

def print_frames(fs, stream = None, frame_ids = None):
        for i, f in enumerate(fs):
                format(defaulted_to_var(stream, _debug_io_), "%2d: %s\n",
                       i, pp_frame(f, lineno = True, frame_id = frame_ids))

def backtrace(x = -1, stream = None, frame = None, frame_ids = None, offset = 0):
        print_frames(frames_calling(defaulted(frame, this_frame()))[1 + offset:x],
                      defaulted_to_var(stream, _debug_io_),
                      frame_ids = frame_ids)

def pp_frame_chain(xs, source_location = None, all_pretty = None, print_fun_line = None):
        def pp_frame_in_chain(f, pretty = None):
                fun = frame_fun(f)
                return (fun_name(fun) if not pretty else
                        ("%s%s@%s:%d" % (fun_name(fun),
                                         (":" + str(frame_lineno(f) - fun_firstlineno(fun))) if print_fun_line else "",
                                         fun_filename(fun),
                                         frame_lineno(f))))
        return ("..".join((pp_frame_in_chain(f, t) for f in xs) if all_pretty else
                          ([pp_frame_in_chain(f) for f in xs[:-1]] +
                           [pp_frame_in_chain(xs[-1], t)])))

def pp_chain_of_frame(x, callers = 5, *args, **keys):
        fs = frames_calling(x, callers)
        fs.reverse()
        return pp_frame_chain(fs, *args, **keys)

def escape_percent(x):
        return x.replace("%", "%%")

# Higher-level debug trace functions

# lf = open("/home/deepfire/lf", "w")
def frame_chain_hash(f, ignore_callers = set(["<lambda>"])):
        "Return an MD5 digest of the caller name chain, with callers listed in IGNORE-CALLERS omitted."
        def f_digestible(f):
                name = f.f_code.co_name
                return name.encode() if name not in ignore_callers else b''
        fchain = frames_calling(f)[1:]
        retv = reduce((lambda acc, f:
                               acc.update(f_digestible(f)) or acc),
                      fchain, hashlib.new("md5")).hexdigest()
        # fprintf(lf, "%s %s\n", [ f_str(x) for x in reversed(chain) ], r)
        return retv

def frame_id(f):
        return hashlib.new("md5", ("%x" % id(f)).encode()).hexdigest()

def here(note = None, *args, callers = 5, stream = None, default_stream = sys.stderr, frame = None, print_fun_line = None, all_pretty = None, offset = 0):
        def do_format(x, args):
                try:
                        return x % (tuple(args))
                except cold_error_type as cond:
                        return "#<error formatting %s into %s: %s>" % (args.__repr__(), note.__repr__(), cond)
        def format_args():
                return (""           if not note else
                        " - " + note if not args else
                        # Unregistered Issue IDEA-MAPXFORM-IF
                        do_format(note, args))
        return dprintf("    (%s)  %s:\n      %s",
                             threading.current_thread().name.upper(),
                             pp_chain_of_frame(defaulted(frame, caller_frame(offset)),
                                                callers = callers - 1,
                                                print_fun_line = print_fun_line,
                                                all_pretty = all_pretty),
                             without_condition_system(format_args),
                             # defaulted(stream, default_stream)
                             )

def locals_printf(locals, *local_names):
        # Unregistered Issue NEWLINE-COMMA-SEPARATION-NOT-PRETTY
        fprintf(sys.stderr, ", ".join((("%s: %%s" % x) if isinstance(x, str) else "%s")
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
def example_frame():
        "cellvars: closed over non-globals;  varnames: bound"
        def xceptor(xceptor_arg):
                "names: globals;  varnames: args + otherbind;  locals: len(varnames)"
                try:
                        error("This is xceptor talking: %s.", xceptor_arg)
                except Exception as cond:
                        return this_frame()
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
#   f_code: <code object example_frame at 0x277a690, file "cl.py", line 197>
#   f_lasti: 45
#   f_lineno: 213
#   f_trace: None
# == def example_frame
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
#   co_name: example_frame
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

# Alexandria

def alist_hash_table(xs):
        return { x[0]: x[1] for x in vectorise_linear(xs) }

def hash_table_alist(xs):
        return mapcon(lambda kv: [k, [v, nil]],
                      consify_linear(the(dict, xs).items()))

class cache(collections.UserDict):
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

def make_timestamping_cache(map_computer):
        cache = cache(lambda x:
                              poor_man_let(map_computer(x),
                                            lambda y: ((y, get_universal_time()) if x else
                                                       None)))
        def cache_getter(x):
                res = cache[(x, 0)]
                return res[0] if res is not None else None
        return cache, cache_getter

def read_case_xformed(x):
        return case_xform(symbol_value(_read_case_), x)

# Pergamum 0

def if_let(x, consequent, antecedent = lambda: None):
        return consequent(x) if x else antecedent()

def when_let(x, consequent):
        return consequent(x) if x else None

def lret(value, body):
        body(value)
        return value

def compose(f, g):
        return lambda *args, **keys: f(g(*args, **keys))

def mapset(f, xs):
        acc = set()
        for x in xs:
                acc.add(f(x))
        return acc

def mapsetn(f, xs):
        acc = set()
        for x in xs:
                acc |= f(x)
        return acc

def mapseparaten(f, xs):
        s0, s1 = set(), set()
        for s0r, s1r in (f(x) for x in xs):
                s0 |= s0r; s1 |= s1r
        return s0, s1

def separate(n, f, xs):
        ss = tuple(set() for _ in range(n))
        for rss in (f(x) for x in xs):
                for s, rs in zip(ss, rss):
                        s |= rs
        return ss

__combiners__ = { set: set.add, list: list.append }
def recombine(spec, f, xss):
        accs  = tuple(f() for f in spec)
        combs = tuple(__combiners__[type(a)] for a in accs)
        for xs in xss:
                for acc, comb, reselt in zip(accs, combs, f(xs)):
                        comb(acc, reselt)
        return accs
def recombine_star(spec, f, *xss):
        accs  = tuple(f() for f in spec)
        combs = tuple(__combiners__[type(a)] for a in accs)
        for xs in zip(*xss):
                for acc, comb, reselt in zip(accs, combs, f(*xs)):
                        comb(acc, reselt)
        return accs

def slotting(x):             return lambda y: getattr(y, x, None)
def slot_of(x):              return lambda y: getattr(x, y, None)
def slot_equal(slot, val):   return lambda y: getattr(y, slot, None) == val

def updated_dict(to, from_):
        to.update(from_)
        return to

def prefix_suffix_if(f, xs, key = identity):
        for i, x in enumerate(xs):
                if not f(key(x)):
                        return xs[:i], xs[i:]
        return xs, []

def prefix_suffix_if_not(f, xs, key = identity):
        return prefix_suffix_if(lambda x: not f(x), xs, key = key)

def defwith(name, enter, exit, **initargs):
        initargs.update(dict(__enter__ = enter,
                             __exit__  = exit))
        return type(name, (object,), initargs)

def lookup(scope, name):
        if not scope: return nil, nil
        frame, rest = scope
        for cell_name, value in frame:
                if cell_name is name:
                        return value, t
        return lookup(name, rest)

def defscope(name, varname, **initargs):
        def make_frame(bindings):
                return tuple(bindings.items())
        def push_frame(**bindings):
                # This dictionary<->alist-ing is a waste.
                return dynamic_scope_push({ varname: (make_frame(bindings), symbol_value(var)) })
        def pop_frame(*_): dynamic_scope_pop()
        return defwith(name, push_frame, pop_frame, lookup = lookup, **initargs)

# Lesser non-CL tools

class withless():
        @staticmethod
        def __init__(): pass
        @staticmethod
        def __enter__(): pass
        @staticmethod
        def __exit__(*_): pass

class servile():
        def __repr__(self):
                return "#%s(%s)" % (type(self).__name__,
                                    ", ".join(maphash(lambda k, v: "%s = %s" % (k, v),
                                                       self.__dict__)))
        def __init__(self, **keys):
                self.__dict__.update(keys)

def gen(n = 1, x = "G", gen = gensym):
        if zerop(n):
                error("_GEN: we are very very much against this, please stop!")
        return tuple(gen(x)
               for i in range(n))
def gensyms(**initargs):     return gen(gen = gensym,      **initargs)
def gensymnames(**initargs): return gen(gen = gensymname, **initargs)

# Testing

#         Used by quasiquotation, metasex and others.

results_ = []
def runtest(fn_spec, input, expected, printer = str, tabstop = 30,
            known_failure = nil, catch_errors = nil):
        name, fn = ((fn_spec.__name__.upper().replace("_", "-"), fn_spec) if functionp(fn_spec)         else
                    fn_spec                                               if isinstance(fn_spec, tuple) else
                    error("Test function specifier must be either a function, or a tuple of two elements, was: %s", fn_spec))
        pref   = "; %%%d" % tabstop
        caught = nil
        def handler(cond):
                nonlocal caught
                caught = t
                dprintf(pref + "s:  EXCEPTION%s\n;  caught%s:\n%s",
                        name, " (known)" if known_failure else "", " (this is normal)" if known_failure else "",
                        cond)
        result = fn(input) if not catch_errors else handler_case(lambda: fn(input),
                                                                 (Exception, handler))
        if caught:
                return known_failure
        if result != expected:
                dprintf(pref + "s:  FAILED%s\n;  input:\n%s\n;  expected:\n%s\n;  actual:\n%s",
                        name, " (known)" if known_failure else "", printer(input), printer(expected), printer(result))
        results_.append((fn, result))
        successp = result == expected
        if successp:
                dprintf(pref + "s:  ok", name)
        return (successp if not known_failure else
                not successp)

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

def seek(n, iterator):
        for i in range(n):
                next(iterator, nil)

def from_(n, xs):
        iterator = iter(xs)
        for i in range(n):
                next(iterator, nil)
        for x in iterator:
                yield x

termination_marker = gensym()
def take(n, xs):
        iterator = iter(xs)
        for i in range(n):
                elt = next(iterator, termination_marker)
                if elt is not termination_marker:
                        yield elt

def some_fast(fn, xs):
        for x in xs:
                ret = fn(x)
                if ret: return ret or t
        return nil

def some_fast_2(fn, xs, ys):
        for x, y in zip(xs, ys):
                ret = fn(x, y)
                if ret: return ret or t
        return nil

def xorf(x, y):
        return (x or y) and not (x and y)

def nxorf(x, y):
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
        return sorted(xs, key = functools.cmp_to_key(predicate))

@defun
def aref(xs, *indices):
        r = xs
        for i in indices:
                r = r[i]
        return r

__allowed__ = frozenset([str, set, frozenset, tuple, list, bytes, bytearray])
def maprestype(x):
        type = type_of(x)
        return type if type in __allowed__ else list

def intersperse(x, xs):
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
def dictappend(*dicts):
        acc = dict()
        for d in dicts:
                acc.update(d)
        return acc

def dict_select_keys(dict_, *keys):
        acc = dict()
        for k in keys:
                if k in dict_:
                        acc[k] = dict_[k]
        return acc

def maphash(f, dict) -> list:
        return [ f(k, v) for k, v in dict.items() ]

def remap_hash_table(f, xs: dict) -> dict:
        return { k: f(k, v) for k, v in xs.items() }

def map_into_hash_star(f, xs,
                        key_test = lambda k: k is not None,
                        value_test = lambda _: t) -> dict:
        acc = make_hash_table()
        for x in xs:
                k, v = f(*x)
                if key_test(k) and value_test(v):
                        acc[k] = v
        return acc

def map_hash_table(f, hash_table, **keys) -> dict:
        return map_into_hash_star(f, hash_table.items(), **keys)

def symbol_python_type(symbol, if_not_a_type = "error"):
        return (symbol.python_type                                   if hasattr(the(symbol_t, symbol), "python_type") else
                nil                                                                        if if_not_a_type == "continue" else
                error("In %%SYMBOL-TYPE %s: symbol does not designate a known type.", symbol) if if_not_a_type == "error" else
                error("In %%SYMBOL-TYPE: the :IF-NOT-A-TYPE keyword argument must be one of ('error, 'continue')."))
def symbol_type_predicate(symbol):
        return symbol.type_predicate if hasattr(the(symbol_t, symbol), "type_predicate") else nil

# Complex arguments

def extract_keywords(xs, keys_allowed = t):
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

def lisp_symbol_python_name(sym):
        return frost.lisp_symbol_name_python_name(sym.name)

def lisp_symbol_python_names(sym):
        return (frost.lisp_symbol_name_python_name(sym.name),
                frost.lisp_symbol_name_python_name(sym.package.name))

def find_module(name, if_does_not_exist = "error"):
        return (frost.find_module(name) or
                poor_man_ecase(if_does_not_exist,
                                ("continue",
                                 None),
                                ("error",
                                 lambda: error(simple_package_error_t, "The name %s does not designate any package.",
                                               name))))

def lisp_symbol_python_addr(sym):
        symname, packname = lisp_symbol_python_names(sym)
        return symname, find_module(packname)

def lisp_symbol_python_value(sym):
        name, module = lisp_symbol_python_addr(sym)
        value, presentp = gethash(name, module.__dict__)
        return (value if presentp else
                error(simple_package_error_t, "This name is not accessible in the '%s' module: '%s'.",
                      module.__name__, name))

def lisp_symbol_ast(sym, current_package):
        symname, packname = lisp_symbol_python_names(sym)
        return (ast_name(symname) if symbol_accessible_in(sym, current_package) else
                ast_index(ast_attribute(ast_index(ast_attribute(ast_name("sys"), "modules"), ast_string(packname)),
                                          "__dict__"),
                           ast_string(symname)))

# Functions

def variable_kind(name):
        check_type(name, symbol_t)
        # (ecase kind
        #   (:special "a special variable")
        #   (:macro "a symbol macro")
        #   (:constant "a constant variable")
        #   (:global "a global variable")
        #   (:unknown "an undefined variable")
        #   (:alien "an alien variable"))
        not_implemented()

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
        return do_symbol_function(the(symbol_t, symbol))

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
        b_or_res = (the(lexenv_t, environment).lookup_func_kind(_macro, symbol, nil) if environment else
                    nil) or the(symbol_t, symbol).macro_function
        if b_or_res and bindingp(b_or_res) and consp(b_or_res.value):
               lambda_list, body = b_or_res.value
               b_or_res = compile_in_lexenv(list_(_lambda, lambda_list, *body),
                                             lexenv = environment)
        return the((or_t, function_t, null_t),
                   (b_or_res.value if bindingp(b_or_res) else b_or_res) or nil)

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

def symbol_macro_expander(sym, environment = None):
        ## -> (-> expansion) | None
        lexical = environment and the(lexenv_t, environment).lookup_var_kind(_symbol_macro, sym)
        expansion = (the(symbol_t, sym).symbol_macro_expansion if not lexical else
                     lexical.value)
        return (lambda: expansion) if expansion is not None else None

def style_warn(control, *args):
        warn(simple_style_warning_t, format_control = control, format_arguments = args)

def warn_incompatible_redefinition(symbol, tons, fromns):
        style_warn("%s is being redefined as a %s when it was previously defined to be a %s.", symbol, tons, fromns)

def warn_possible_redefinition(x, type):
        if x:
                style_warn("In %s: %s is being redefined.", type, x)

@defun
def setf_macro_function(new_function, symbol, environment = nil):
        "<See documentation for MACRO-FUNCTION>"
        ## Ensure compliance.
        check_type(environment, null_t)
        if symbol.function:
                warn_incompatible_redefinition(symbol, "macro", "function")
                symbol.function = nil
        warn_possible_redefinition(symbol.macro_function, _defmacro)
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
        compiler_defun(function_name, nil, check_redefinition = nil)
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
        return t if find_known(symbol) else nil

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

# Namespace separation.

compiler_safe_namespace_separation = t

def ensure_function_pyname(symbol):
        if the(symbol_t, symbol).function_pyname is not None:
                return symbol.function_pyname
        symbol.function_pyname = (gensymname("FUN_" + str(symbol) + "-") if compiler_safe_namespace_separation else
                                  str(symbol))
        return symbol.function_pyname
def ensure_symbol_pyname(symbol):
        if the(symbol_t, symbol).symbol_pyname is not None:
                return symbol.symbol_pyname
        symbol.symbol_pyname = (gensymname("SYM_" + str(symbol) + "-") if compiler_safe_namespace_separation else
                                str(symbol))
        return symbol.symbol_pyname

def ensure_variable_pyname(x):
        return frost.full_symbol_name_python_name(x)

def get_function_pyname(symbol):
        if the(symbol_t, symbol).function_pyname is None:
                error("Function %s has no mapping to a python name.", symbol)
        return symbol.function_pyname
def get_symbol_pyname(symbol):
        if the(symbol_t, symbol).symbol_pyname is None:
                error("Symbol %s has no mapping to a python name.", symbol)
        return symbol.symbol_pyname

def define_global_sym_for_pyname(globals, pyname, sym):
        globals["__" + pyname[:None if (pyname[:-1].endswith("_")
                                        or not pyname.endswith("_")) else
                               -1]] = sym

def set_function_definition(globals, x, lambda_expression = None, check_redefinition = nil):
        lambda_expression = defaulted(lambda_expression, [_lambda, [nil, nil]])
        identity_redef = compiler_defun(x, lambda_expression, check_redefinition = check_redefinition)
        def do_set_function_definition(function):
                if not identity_redef and function:
                        x.function, x.macro_function = function, nil
                        frost.make_object_like_python_function(x, function)
                        forigname = function.__name__
                        function.name = x
                        ## Unregistered Issue NAMESPACE-POLLUTION-SEEMS-FRIVOLOUS
                        define_global_sym_for_pyname(globals, forigname, x)
                        frost.setf_global(function, ensure_function_pyname(x), globals)
                return function
        return do_set_function_definition

def set_macro_definition(globals, x, lambda_expression):
        identity_redef = compiler_defmacro(x, lambda_expression)
        def do_set_macro_definition(function):
                if not identity_redef and function:
                        x.function, x.macro_function = nil, function
                        frost.make_object_like_python_function(x, function)
                        function.name = x
                        frost.setf_global(function, ensure_function_pyname(x), globals)
                return x
        return do_set_macro_definition

# Essential system-level functions

def getenv(var):
        return without_condition_system(lambda: os.getenv(var))

# Condition system disabling

def without_condition_system(body, reason = ""):
        if frost.pytracer_enabled_p():
                try:
                        frost.disable_pytracer()
                        return body()
                finally:
                        frost.enable_pytracer()
        else:
                return body()

disabled_condition_system = defwith("disabled_condition_system",
                                    lambda _:  frost.disable_pytracer(),
                                    lambda *_: frost.enable_pytracer())

# Condition system init

def init_condition_system():
        frost.enable_pytracer() ## enable HANDLER-BIND and RESTART-BIND

def condition_system_enabled_p():
        return (frost.pytracer_enabled_p() and
                frost.tracer_hook("exception") is __cl_condition_handler__)

(_load_toplevel, _compile_toplevel, _execute) = [ make_keyword(x)  for x in [ "LOAD-TOPLEVEL", "COMPILE-TOPLEVEL", "EXECUTE" ] ]
(_load, _compile, _eval)                      = [ intern(x)[0] for x in [ "LOAD", "COMPILE", "EVAL" ] ]

if not getenv("CL_NO_CONDITION_SYSTEM"):
        ## Has no effect until set_condition_handler(__cl_condition_handler__) is called later.
        init_condition_system()

# Rudimentary character type

@defclass
class base_char_t(): pass

# Early-earlified streaming

@defun
def streamp(x):                     return isinstance(x, stream_t)

def file_stream_p(x):              return isinstance(x, (_io._TextIOBase, _io._BufferedIOBase))

@defun
def with_open_stream(stream, fn):
        try:
                return fn(stream)
        finally:
                close(stream)

@defun
def open(pathname, direction = make_keyword("INPUT"), element_type = base_char_t,
         if_exists = make_keyword("ERROR"), if_does_not_exist = make_keyword("ERROR"),
         external_format = make_keyword("DEFAULT")):
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
        return py.open(namestring(pathname),
                        poor_man_ecase(direction,
                                        (make_keyword("INPUT"),  lambda: "r"),
                                        (make_keyword("OUTPUT"), lambda: "w"),
                                        (make_keyword("IO"),     lambda: "rw"),
                                        (make_keyword("PROBE"),  lambda: not_implemented("direction :PROBE"))))

@defun
def with_open_file(pathname, body, direction = make_keyword("INPUT"), element_type = base_char_t,
                   if_exists = make_keyword("ERROR"), if_does_not_exist = make_keyword("ERROR"),
                   external_format = make_keyword("DEFAULT")):
        return with_open_stream(open(pathname, direction, element_type, if_exists, if_does_not_exist, external_format),
                                body)

@defun
def probe_file(x):
        x = pathname(x)
        return without_condition_system(
                lambda: truename(x) if os.path.exists(namestring(x)) else nil,
                reason = "os.path.exists")

@defun
def truename(x):
        # Unregistered Issue COMPLIANCE-TRUENAME
        x = pathname(x)
        return namestring(x)

@defun
def file_length(stream):
        f = namestring(pathname(stream))
        return os.path.getsize(f)

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
        return int(os.path.getmtime(f))

def file_stream_name(x):
        return values_frame_project(0, parse_namestring(x.name))

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
                #  1. Comment out the dprintf below.
                #  2. Uncomment the 'raise' line immediately below %STRING-SET calls.
                dprintf("For the love of all things living: about to close the funky streams!")
                self.output.close()
                self.input.close()
        def readable(self): return t
        def writable(self): return t

def make_two_way_stream(input, output):   return two_way_stream_t(input, output)
def two_way_stream_input_stream(stream):  return stream.input
def two_way_stream_output_stream(stream): return stream.output

string_set("*DEBUG-IO*", make_two_way_stream(symbol_value(_standard_input_), symbol_value(_standard_output_)))
string_set("*QUERY-IO*", make_two_way_stream(symbol_value(_standard_input_), symbol_value(_standard_output_)))
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

def coerce_to_stream(x):
        return (x                               if streamp(x) else
                symbol_value(_standard_output_) if x is t     else
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
                                return do_write_string(string, coerce_to_stream(stream))
                        except io.UnsupportedOperation as cond:
                                error(stream_type_error_t, "%s is not an %s stream: \"%s\".",
                                      stream, ("output" if cond.args[0] == "not writable" else
                                               "adequate"),
                                      cond.args[0])
                without_condition_system(handler,
                                          reason = "_write_string")
        return string

def write_line(string, stream = t):
        return write_string(string + "\n", stream)

def finish_output(stream = t):
        check_type(stream, (or_t, stream_t, (member_t, t, nil)))
        (stream is not nil) and coerce_to_stream(stream).flush()

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
        if  streamp(destination) or listp(destination) or destination is t:
                # XXX: python strings are immutable, so lists will serve as adjustable arrays..
                # Issue ADJUSTABLE-CHARACTER-VECTORS-NOT-IMPLEMENTED
                write_string(string, destination)
                return nil
        else:
                return string

# Earlified streaming

@defun
def stream_external_format(stream): return make_keyword(stream.encoding)

@defun
def make_string_output_stream():
        return io.StringIO()

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
        return io.StringIO(x)

@defun
def close(x):
        x.close()

@defun
def file_position(x):
        return x.seek(0, 1)

@defun
def setf_file_position(posn, x):
        return x.seek(posn)

def stream_as_string(stream):
        return stream.read()

def file_as_string(filename):
        with py.open(filename, "r") as f:
                return stream_as_string(f)

# Pathnames

#     Relevant sections:

#     - 19.2.2.1.2 Case in Pathname Components          :: http://clhs.lisp.se/Body/19_bbab.htm
#     - 19.2.2.1.2.1 Local Case in Pathname Components  :: http://clhs.lisp.se/Body/19_bbaba.htm
#     - 19.2.2.1.2.2 Common Case in Pathname Components :: http://clhs.lisp.se/Body/19_bbabb.htm

def namestring_components(x):
        dirname, basename = os.path.split(x)
        posn = basename.rfind(".")
        return if_let(posn if posn >= 0 else nil,
                       lambda dotpos: (dirname or nil, basename[:dotpos], basename[dotpos + 1:]),
                       lambda:        (dirname or nil, basename or nil,   nil))

@defclass
class pathname_host_t():
        def parse(self, x):
                ## Unregistered Issue COMPLIANCE-NAMESTRING-UNPARSING-NOT-REALLY-IMPLEMENTED
                dirname, basename, type = namestring_components(the(string_t, x))
                directory = dirname.split(os.sep) if dirname else nil
                return pathname_t(host      = system_pathname_host,
                                  device    = nil,
                                  directory = directory and (([make_keyword("ABSOLUTE")] + directory[1:]) if directory[0] == "" else
                                                             ([make_keyword("RELATIVE")] + directory)),
                                  name      = basename,
                                  type      = type,
                                  version   = make_keyword("NEWEST") if x else nil)
        def unparse(self, x):
                return ((x.device or "") +
                        (os.sep                                   if x.directory and x.directory[0] is make_keyword("ABSOLUTE") else "") +
                        ((os.sep.join(x.directory[1:]) + os.sep) if x.directory                                            else "") +
                        ("*"    if x.name is make_keyword("WILD") else
                         x.name if x.name is not nil          else
                         "") +
                        (("." + ("*" if x.type is make_keyword("WILD") else x.type)) if x.type is not nil else ""))
        def local_case(self, x): return self.localise_case(x)
        def common_case(self, x):
                return (self.customiser     if x.isupper() else
                        self.anticustomiser if x.islower() else
                        identity)(x)
        def apply_case(self, case, x):
                return (self.local_case(x)  if case is make_keyword("LOCAL")  else
                        self.common_case(x) if case is make_keyword("COMMON") else
                        error("Invalid case transform specifier: %s.  Must be one of either %s or %s.",
                              case, make_keyword("LOCAL"), make_keyword("COMMON")))

@defclass
class unix_host_t(pathname_host_t):
        localise_case              = identity
        customiser, anticustomiser = str.lower, str.upper

@defclass
class windows_host_t(pathname_host_t):
        localise_case              = identity
        customiser, anticustomiser = str.lower, str.upper

system_pathname_host = make_instance(windows_host_t if platform.system() == 'Windows' else
                                      unix_host_t)

intern_and_bind_globals("*DEFAULT-PATHNAME-DEFAULTS*")

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
                  default = None, case = make_keyword("LOCAL")):
        assert not args
        default = default or pathname_t(**_defaulted_keys(
                        host = pathname_host(symbol_value(_default_pathname_defaults_)),
                        device = nil, directory = nil, name = nil, type = nil, version = nil))
        effective_host = defaulted(host, default.host)
        supplied_pathname = dict(
                (k, effective_host.apply_case(case, v) if isinstance(v, str) else v)
                for k, v in only_specified_keys(host = host, device = device, directory = directory, name = name, type = type, version = version).items())
        ## Unregistered Issue RESEARCH-COMPLIANCE-MAKE-PATHNAME-CANONICALISATION
        return merge_pathnames(supplied_pathname, default)

@defun
def parse_namestring(thing, host = nil, default_pathname = None, *args, start = 0, end = nil, junk_allowed = nil):
        assert not args
        if junk_allowed:
                not_implemented("%s", make_keyword("JUNK-ALLOWED")) ## Unregistered Issue COMPLIANCE-PARSE-NAMESTRING-JUNK-ALLOWED-NOT-IMPLEMENTED
        if streamp(thing):
                thing = pathname(thing)
        if pathnamep(thing):
                return (values(thing, start) if not (host or thing.host) or host is thing.host else
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
        default_pathname = defaulted_to_var(default_pathname, _default_pathname_defaults_)
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
                      make_keyword("HOST"), make_keyword("DEFAULT-PATHNAME"))
        return values(effective_host.parse(subseq(thing, start, end) if start or end else thing),
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
                values_frame_project(0, parse_namestring(x)) if isinstance(x, str)        else
                file_stream_name(x)                          if file_stream_p(x) else
                error("PATHNAME only accepts pathnames, namestrings and file streams, was given: %s.", x))

@defun
def pathname_directory(x):
        # Unregistered Issue PORTABILITY-PATHNAME
        absp = the(string_t, x).startswith(os.sep)
        return ([make_keyword("absolute" if absp else "relative")] +
                # Reject the integer interpretation of booleans.
                namestring_components(x)[0].split(os.sep)[1 if absp else 0])

@defun
def pathname_name(x): return namestring_components(x)[1]
@defun
def pathname_type(x): return namestring_components(x)[2]

def init_pathnames():
        string_set("*DEFAULT-PATHNAME-DEFAULTS*",
                    values_frame_project(0, parse_namestring(os.getcwd() + "/",
                                                              host = system_pathname_host,
                                                              default_pathname = t)))
        # T is a junk marker, but avoid a bootstrap loop

init_pathnames()

# Sub-standard pathname functions

def cold_merge_pathnames(pathname, default_pathname = None, default_version = None):
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
        not_implemented() # Gave up for the while.
        default_pathname = defaulted(default_pathname, os.getcwd() + os.sep)
        dir_supplied_p = os.sep in pathname
        name_supplied_p = pathname and pathname[-1] != os.sep
        dir_defaulted_p = os.sep in default_pathname
        net_effect_if = name_supplied_p and not dir_supplied_p # Unregistered Issue COMPLIANCE-MERGE-PATHNAMES-SIMPLIFICATION
        if net_effect_if:
                posn = default_pathname.rfind(os.sep)
                return os.path.join((default_pathname[:posn + 1] if dir_defaulted_p else ""),
                                    pathname)
        elif not name_supplied_p:
                pass
        return os.path.join(x, y)

# Cons <-> vector xform

def vectorise(x):
        res = []
        while x:
                res.append(x[0] if not consp(x[0]) else
                           vectorise(x[0]))
                x = x[1]
        return res

def vectorise_to_tuple(x):
        res = ()
        while x:
                res += ((x[0] if not consp(x[0]) else
                         vectorise_to_tuple(x[0])),)
                x = x[1]
        return res

def vectorise_linear(x):
        res = []
        while x:
                res.append(x[0])
                x = x[1]
        return res

def consify(xs):
        return consify_linear(( (consify(x) if isinstance(x, (list, tuple)) else x)
                                 for x in xs )) if isinstance(xs, (list, tuple)) else xs

def consify_star(*xs):
        return consify(xs)

def consify_linear(xs, last_cdr = nil):
        return reduce(lambda acc, x: [x, acc],
                      reversed(tuple(xs)),
                      last_cdr)

def pp_consly(x, dispatch = dict(), str_printer = repr, max_depth = 7):
        if consp(x):
                if max_depth is 0:
                        return "#"
                acc = []
                ptr = x
                while ptr:
                        if not consp(ptr):
                                acc.append(". " + str(ptr))
                                break
                        acc.append(pp_consly(ptr[0], dispatch = dispatch, max_depth = max_depth - 1))
                        ptr = ptr[1]
                return "\x28" + " ".join(acc) + "\x29"
        elif isinstance(x, dict):
                return "{ " + ", ".join("%s: %s" % (k, pp_consly(v, dispatch, str_printer, max_depth))
                                        for k, v in x.items()) + "}"
        return (str_printer       if isinstance(x, str)  else
                dispatch[type(x)] if type(x) in dispatch else str)(x)

def pp_consly_pp_str(x, dispatch = dict()):
        return pp_consly(x, dispatch, str)

def xmap_to_vector(f, xs, *xss):
        if not xss:
                acc = []
                while xs:
                        acc.append(f(xs[0]))
                        xs = xs[1]
                return acc
        else:
                not_implemented("%XMAP-TO-VECTOR: multiple-list case")

def xmap_to_conses(f, xs, *xss):
        if not xss:
                lim = len(xs)
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

def compute_predicate(key, elt, test = eql, test_not = nil):
        if test and test_not:
                error("Incomprehensible simultaneous specification of :TEST and :TEST-NOT.")
        test = test or test_not
        return ((lambda x: test(elt, x))
                if key is identity else
                (lambda x: test(elt, key(x))))

## Uncomment in emergency:
#
# __key_map__ = { "key":      key_,
#                 "start":    start,
#                 "end":      end,
#                 "from_end": from_end,
#                 "test":     test,
#                 "test_not": test_not,
#                 }
# def read_keys(keys):
#         return { __key_map__[k]: v for k, v in keys }

# COPY-SEQ
# ELT
# FILL
# MAKE-SEQUENCE

def error_bad_indices(start, end, actual):
        error("The bounding indices %d and %s are bad for a sequence of length %d",
              start, end, actual)

def copy_list_with_lastcdr(x, cdr):
        if not x:
                return cdr
        ret = ptr = [the(list_t, x)[0], cdr]
        while True:
                x = x[1]
                if not x:
                        return ret
                ptr[1] = ptr = [x[0], cdr]

def copy_list_head_with_lastcdr(xs, n, cdr):
        if not xs:
                return cdr, n is 0
        ret = ptr = [the(list_t, xs)[0], cdr]
        while True:
                n -= 1
                xs, done = xs[1], n is 0
                if not xs or done:
                        return ret, done
                ptr[1] = ptr = [xs[0], cdr]

def copy_list_head_operating_on_cdr(f, n, xs):
        if not xs or n is 0:
                return nil if n else f(xs), n
        ret = ptr = [the(list_t, xs)[0], nil]
        while True:
                n -= 1
                oldxs = xs
                xs = xs[1]
                if not xs or not n:
                        if not n:
                                oldxs[1] = f(xs)
                        return ret, n
                ptr[1] = ptr = [xs[0], nil]

@defun
def subseq(xs, start, end = nil):
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
                not_implemented("Non-cons case of SUBSEQ.")

_key_, _start, _end, _from_end, _test, _test_not, _count = [ make_keyword(x) for x in
                                                             ["KEY", "START", "END", "FROM-END", "TEST", "TEST-NOT", "COUNT" ] ]

# MAP
# MAP-INTO
# REDUCE

@defun
def count(elt, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end, _test, _test_not])
        key, start, end, from_end, test, test_not = [ keys.get(k, df) for k, df
                                                      in [ (_key_,     identity),
                                                           (_start,    0),
                                                           (_end,      nil),
                                                           (_from_end, nil),
                                                           (_test,     nil),
                                                           (_test_not, nil) ] ]
        not_implemented()

@defun
def count_if(p, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end])
        key, start, end, from_end = [ keys.get(k, df) for k, df
                                      in [ (_key_,     identity),
                                           (_start,    0),
                                           (_end,      nil),
                                           (_from_end, nil) ] ]
        not_implemented()

@defun
def count_if_not(p, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end])
        key, start, end, from_end = [ keys.get(k, df) for k, df
                                      in [ (_key_,     identity),
                                           (_start,    0),
                                           (_end,      nil),
                                           (_from_end, nil) ] ]
        not_implemented()

@defun
def length(x):
        if listp(x):
                len = 0
                while isinstance(x, list):
                        len += 1
                        x = x[1]
                return len
        else:
                not_implemented("LENGTH: non-list case on object %s (of type %s)" % (x, type(x).__name__))

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

def do_find_if(pred, xs, keys):
        key, start, end, from_end = [ keys.get(k, df) for k, df
                                      in [ (_key_,     identity),
                                           (_start,    0),
                                           (_end,      nil),
                                           (_from_end, nil) ] ]
        endp = end is not nil
        if endp and end < start:
                error_bad_indices(start, end, length(xs))
        if from_end:
                not_implemented(":FROM-END")
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
                not_implemented("FIND-IF: non-list case")

@defun
def find(elt, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end, _test, _test_not])
        key, test, test_not = [ keys.get(k, df) for k, df
                                in [ (_key_,     identity),
                                     (_test,     eql),
                                     (_test_not, nil) ] ]
        if _key_     in keys: del keys[_key_]
        if _test     in keys: del keys[_test]
        if _test_not in keys: del keys[_test_not]
        return do_find_if(compute_predicate(key, elt, test = test, test_not = test_not),
                          xs, keys)

@defun
def find_if(p, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end])
        return do_find_if(p, xs, keys)

@defun
def find_if_not(p, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end])
        return do_find_if(lambda x: not p(x), xs, keys)

def do_position_if(pred, xs, keys):
        key, start, end, from_end = [ keys.get(k, df) for k, df
                                      in [ (_key_,     identity),
                                           (_start,    0),
                                           (_end,      nil),
                                           (_from_end, nil) ] ]
        endp = end is not nil
        if endp and end < start:
                error_bad_indices(start, end, length(xs))
        if from_end:
                not_implemented(":FROM-END")
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
                not_implemented("POSITION-IF: non-list case")

@defun
def position(elt, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end, _test, _test_not])
        key, test, test_not = [ keys.get(k, df) for k, df
                                in [ (_key_,     identity),
                                     (_test,     eql),
                                     (_test_not, nil) ] ]
        if _key_     in keys: del keys[_key_]
        if _test     in keys: del keys[_test]
        if _test_not in keys: del keys[_test_not]
        return do_position_if(compute_predicate(key, elt, test = test, test_not = test_not),
                             xs, keys)

@defun
def position_if(p, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end])
        return do_position_if(p, xs, keys)

@defun
def position_if_not(p, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end])
        return do_position_if(lambda x: not p(x), xs, keys)

# SEARCH
# MISMATCH

@defun
def replace(sequence_1, sequence_2, *rest):
        """Destructively modifies sequence-1 by replacing the elements
of subsequence-1 bounded by start1 and end1 with the elements of
subsequence-2 bounded by start2 and end2. """
        keys = extract_keywords(rest, [_start1, _start2, _end1, _end2])
        start1, start2, end1, end2 = [ keys.get(k, df) for k, df
                                       in [ (_start1,    nil),
                                            (_start2,    nil),
                                            (_end2,      nil),
                                            (_end2,      nil) ] ]
        not_implemented()

# SUBSTITUTE
# SUBSTITUTE-IF
# SUBSTITUTE-IF-NOT
# NSUBSTITUTE
# NSUBSTITUTE-IF
# NSUBSTITUTE-IF-NOT
# CONCATENATE
# MERGE

def do_remove_if(pred, xs, keys):
        key, start, end, from_end, count = [ keys.get(k, df) for k, df
                                             in [ (_key_,     identity),
                                                  (_start,    0),
                                                  (_end,      nil),
                                                  (_from_end, nil),
                                                  (_count,    nil) ] ]
        endp = end is not nil
        if endp and end < start:
                error_bad_indices(start, end, length(xs))
        if count is not nil:
                not_implemented(":COUNT")
        the_test = pred if not key else lambda x: pred(key(x))
        if listp(xs):
                def continuation(tail):
                        i, rptr = start, tail
                        acc = wptr = [nil, nil]
                        while True:
                                if i == end:
                                        wptr[1] = rptr
                                        return rptr if i == start else acc
                                elif not rptr:
                                        if end is not nil:
                                                error_bad_indices(start, end, i - 1)
                                        if acc == wptr:
                                                acc = nil
                                        else:
                                                oldwptr[1] = nil
                                        return acc
                                if not the_test(rptr[0]):
                                        wptr[0] = rptr[0]
                                        oldwptr = wptr
                                        wptr[1] = wptr = [nil, nil]
                                i, rptr = i + 1, rptr[1]
                ret, remaining = copy_list_head_operating_on_cdr(continuation, start, xs)
                if remaining:
                        error_bad_indices(start, end, start - remaining)
                return ret
        else:
                not_implemented("FIND-IF: non-list case")

@defun
def remove(elt, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end, _test, _test_not, _count])
        key, test, test_not = [ keys.get(k, df) for k, df
                                in [ (_key_,     identity),
                                     (_test,     eql),
                                     (_test_not, nil) ] ]
        if _key_     in keys: del keys[_key_]
        if _test     in keys: del keys[_test]
        if _test_not in keys: del keys[_test_not]
        return do_remove_if(compute_predicate(key, elt, test = test, test_not = test_not),
                            xs, keys)

@defun
def remove_if(p, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end, _count])
        return do_remove_if(p, xs, keys)

@defun
def remove_if_not(p, xs, *rest):
        keys = extract_keywords(rest, [_key_, _start, _end, _from_end, _count])
        return do_remove_if(lambda x: not p(x), xs, keys)

# REMOVE-DUPLICATES
# DELETE-DUPLICATES

# Conses

initial_element = make_keyword("INITIAL-ELEMENT")

@defun
def cons(x, y):     return [x, y]

@defun
def consp(x):       return isinstance(x, list) and len(x) is 2
def consp_fast(x): return isinstance(x, list)

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

@defun
def copy_list(xs):
        return copy_list_with_lastcdr(xs, nil)

@defun("LIST")
def list_(*xs):     return consify_linear(xs)

@defun("LIST*")
def list__(*xs):
        return consify_linear(itertools.islice(xs, 0, len(xs) - 1), last_cdr = xs[-1])
# LIST-LENGTH

@defun
def listp(x):       return x is nil or isinstance(x, list) and len(x) is 2

@defun("MAKE-LIST")
def make_list(length, *rest):
        elt = extract_keywords(rest, [_initial_element]).get(initial_element, nil)
        acc = nil
        for i in range(length):
                acc = [elt, acc]
        return acc

# PUSH
# POP

@defun
def first(x):
        return nil if not x else x[0]

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
        return copy_list_with_lastcdr(the(list_t, xs[0]), (xs[1] if len(xs) is 2 else
                                                            nil   if len(xs) is 1 else
                                                            append(*xs[1:])))

# REVAPPEND
# NRECONC

@defun
def butlast(xs):
        if not consp(xs) or not consp(xs[1]):
                return nil
        ret = wptr = [xs[0], nil]
        while True:
                xs = xs[1]
                if not consp(xs[1]):
                        return ret
                wptr[1] = [xs[0], nil]
                wptr = wptr[1]

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
        not_implemented()

@defun
def tailp(object, list):
        """If OBJECT is the same as some tail of LIST, TAILP returns
true; otherwise, it returns false."""
        not_implemented()

@defun
def nthcdr(n, xs):
        while n and xs:
                n, xs = n - 1, xs[1]
        return xs

# REST

@defun
def member(x, xs):
        keys = extract_keywords(rest, [_key_, test, test_not])
        key, test, test_not = [ keys.get(k, df) for k, df
                                in [ (key_,     identity),
                                     (test,     nil),
                                     (test_not, nil) ] ]
        not_implemented()

@defun
def member_if(test, xs):
        key = extract_keywords(rest, [_key_]).get(key_, identity)
        not_implemented()

@defun
def member_if_not(test, xs):
        key = extract_keywords(rest, [_key_]).get(key_, identity)
        not_implemented()

@defun
def mapc(f, *xs):
         not_implemented()

@defun
def mapcar(f, xs, *xss):
        if not xss:
                if not xs:
                        return nil
                car, cdr = xs[0], xs[1]
                acc = ptr = [f(car), nil]
                while cdr:
                        car, cdr = cdr[0], cdr[1]
                        ptr[1] = ptr = [f(car), nil]
                return acc
        else:
                if not xs or not all(xss): ## This misfires when something is illegally not a list, but a pyfalse -- e.g. 0.
                        return nil
                caar, cars = xs[0], [ x[0] for x in xss ]
                cadr, cdrs = xs[1], [ x[1] for x in xss ]
                acc = ptr = [f(caar, *cars), nil]
                while cadr and all(cdrs):
                        caar, cars = cadr[0], [ x[0] for x in cdrs ]
                        cadr, cdrs = cadr[1], [ x[1] for x in cdrs ]
                        ptr[1] = ptr = [f(caar, *cars), nil]
                return acc

@defun
def mapcan(f, *xs):
         not_implemented()

# MAPL
# MAPLIST

@defun
def mapcon(f, xs, *xss):
        if not xss:
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
                not_implemented("MAPCON: multiple-list case")

# ACONS

@defun
def assoc(x, xs, *rest):
         not_implemented()

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
         not_implemented()

@defun
def setf_getf(value, xs, key):
         not_implemented()

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
        if not xss:
                while xs:
                        if not fn(xs[0]):
                                 return nil
                        xs = xs[1]
                return t
        else:
                not_implemented("EVERY: multiple-list case")

@defun
def some(fn, xs, *xss):
        if not xss:
                while xs:
                        if fn(xs[0]):
                                 return t
                        xs = xs[1]
                return nil
        else:
                not_implemented("SOME: multiple-list case")

@defun
def notevery(fn, xs, *xss):
        if not xss:
                while xs:
                        if not fn(xs[0]):
                                 return t
                        xs = xs[1]
                return nil
        else:
                not_implemented("NOTEVERY: multiple-list case")

@defun
def notany(fn, xs, *xss):
        if not xss:
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

def print_string(x, escape = None, readably = None):
        """The characters of the string are output in order. If printer escaping
is enabled, a double-quote is output before and after, and all
double-quotes and single escapes are preceded by backslash. The
printing of strings is not affected by *PRINT-ARRAY*. Only the active
elements of the string are printed."""
        # XXX: "active elements of the string"
        # Issue ADJUSTABLE-CHARACTER-VECTORS-NOT-IMPLEMENTED
        readably = defaulted_to_var(readably, _print_readably_)
        escape   = defaulted_to_var(escape,   _print_escape_) if not readably else t
        return (x if not escape else
                ("\"" + without_condition_system(
                                lambda: re.sub(r"([\"\\])", r"\\\1", x),
                                reason = "re.sub") +
                 "\""))

def print_function(x):
        return with_output_to_string(
                lambda s: print_unreadable_object(
                        x, s,
                        lambda: format(s, "%s (%s)", x.__name__, print_function_arglist(x)),
                        identity = t, type = t))

def print_unreadable_compound(x):
        return with_output_to_string(
                lambda s: print_unreadable_object(
                        x, s,
                        lambda: format(s, "%d elements", len(x)),
                        identity = t, type = t))

def print_unreadable(x):
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
        array           = defaulted_to_var(array,           _print_array_)
        base            = defaulted_to_var(base,            _print_base_)
        case            = defaulted_to_var(case,            _print_case_)
        circle          = defaulted_to_var(circle,          _print_circle_)
        escape          = defaulted_to_var(escape,          _print_escape_)
        gensym          = defaulted_to_var(gensym,          _print_gensym_)
        length          = defaulted_to_var(length,          _print_length_)
        level           = defaulted_to_var(level,           _print_level_)
        lines           = defaulted_to_var(lines,           _print_lines_)
        miser_width     = defaulted_to_var(miser_width,     _print_miser_width_)
        pprint_dispatch = defaulted_to_var(pprint_dispatch, _print_pprint_dispatch_)
        pretty          = defaulted_to_var(pretty,          _print_pretty_)
        radix           = defaulted_to_var(radix,           _print_radix_)
        readably        = defaulted_to_var(readably,        _print_readably_)
        right_margin    = defaulted_to_var(right_margin,    _print_right_margin_)
        # assert(t
        #        and array is t
        #        and base is 10
        #        # case is make_keyword("upcase")
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
                        if listp(object):
                                string += "\x28"
                                max = len(object)
                                if max:
                                        for i in range(0, max):
                                                string += do_write_to_string(object[i])
                                                if i != (max - 1):
                                                        string += " "
                                string += "\x29"
                        elif symbolp(object):
                                # Honors *PACKAGE*, *PRINT-CASE*, *PRINT-ESCAPE*, *PRINT-GENSYM*, *PRINT-READABLY*.
                                # XXX: in particular, *PRINT-ESCAPE* is honored only partially.
                                string += print_symbol(object)
                        elif isinstance(object, int) or floatp(object):
                                string += str(object)
                        elif object is False or object is None or object is True:
                                string += obj2lisp_xform[object]
                        elif type(object).__name__ == "builtin_function_or_method":
                                string += "\"#<BUILTIN-FUNCTION-OR-METHOD %s 0x%x>\"" % (object.__name__, id(object))
                        elif isinstance(object, str):
                                # Honors *PRINT-ESCAPE* and *PRINT-READABLY*.
                                string += print_string(object)
                        elif hash_table_p(object) or setp(object):
                                # Honors *PRINT-ESCAPE* and *PRINT-READABLY*.
                                string += print_unreadable_compound(object)
                        elif functionp(object):
                                string += print_function(object)
                        elif (not escape) and isinstance(object, (restart_t, condition_t)):
                                string += str(object)
                        else:
                                string += print_unreadable(object)
                                # error("Can't write object %s", object)
                        return string
                return write_to_string_loop(object)
        ret = do_write_to_string(object)
        # dprintf("===> %s", ret)
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

intern_and_bind_symbols("LIST", "APPEND", "QUOTE", "QUASIQUOTE", "COMMA", "SPLICE")

string_set("*READER-TRACE-QQEXPANSION*",        nil)

## Unregistered Issue COMPLIANCE-BACKQUOTE-EXPANSION-DOTTED-LIST-HANDLING

def expand_quasiquotation(form):
        """Expand quasiquotation abbreviations in FORM (in a simple, yet suboptimal way)."""
        def process_atom(x):
                return (x if constantp(x) else
                        list_(_quote, x))
        def process_form(x):
                return ((error("Invalid form: %s - %s not inside a backquote.", x, x[0]) if x[0] is not _quasiquote else # The toplevel must be a QQ.
                         error("Internal reader error: %s is invalid.")                  if len(x) != 2             else
                         process_qq(x[1]))        if isinstance(x, tuple)   else
                        x                         if atom(x)                else
                        mapcar(process_form, x))
        def process_qq(x):
                if atom(x):
                        return process_atom(x)
                else:
                        acc = [_append]
                        ptr = x
                        while ptr:
                                ## Check for a qq marker at the end of an improper list:
                                def do_one(x):
                                        return list_(_list,
                                                     (process_atom(x) if atom(x) else
                                                      process_qq(x)))
                                if isinstance(ptr, tuple):
                                        xi = ptr
                                        if len(xi) != 2 or xi[0] not in (_quasiquote, _comma, _splice):
                                                error("Invalid reader output, bad tuple intermarker: %s", pp_consly(form))
                                        if xi[0] is _splice:
                                                error(",@ after dot in %s", pp_consly(form))
                                        acc.append(process_qq(xi[1]) if xi[0] is _quasiquote else
                                                   xi[1])            #           comma
                                        break
                                xi = ptr[0]
                                if isinstance(xi, tuple):
                                        if len(xi) != 2 or xi[0] not in (_quasiquote, _comma, _splice):
                                                error("Invalid reader output, bad tuple intermarker: %s", pp_consly(form))
                                        acc.append(do_one(process_qq(xi[1])) if xi[0] is _quasiquote else
                                                   list_(_list, xi[1])      if xi[0] is _comma      else
                                                   xi[1])                    #           splice
                                else:
                                        acc.append(do_one(xi))
                                ptr = ptr[1]
                        ## Simplify an obvious case of APPEND having only LIST subforms.
                        if all((consp(x) and x[0] is _list)
                               for x in acc[1:]):
                                new = (_list,) + tuple(x[1][0] for x in acc[1:])
                                acc = new
                        return consify_linear(acc)
        result = process_form(form)
        if symbol_value(_reader_trace_qqexpansion_):
                if form != result:
                        dprintf(";;;%s quasiquotation expanded to:\n%s%s",
                                      sex_space(-3, ";"), sex_space(), pp(result))
                else:
                        dprintf(";;;%s quasiquotation had no effect", sex_space(-3, ";"))
        return result

def run_tests_quasiquotation():
        def quasiquotation_simple(x): return expand_quasiquotation(x)
        def quasiquotation_nested(x): return expand_quasiquotation(x)
        assert(runtest(quasiquotation_simple,
                        ## `(1 ,2 3 ,@4 5 (,6 ,@7) ,@8 ,@9)
                        (_quasiquote, list_(1, (_comma, 2), 3, (_splice, 4), 5,
                                             list_((_comma, 6), (_splice, 7)),
                                             (_splice, 8), (_splice, 9))),
                        ## (append (list (_quote 1)) (list 2) (list (_quote 3)) 4 (list (_quote 5))
                        ##         (list (append (list 6) 7)) 8 9)
                        list_(_append, list_(_list, 1), list_(_list, 2),
                              list_(_list, 3), 4,
                              list_(_list, 5), list_(_list, list_(_append, list_(_list, 6), 7)),
                              8, 9),
                        printer = pp_consly))
        
        assert(runtest(quasiquotation_nested,
                        ## `(a ,b ,@c `(d ,,e ,@f ,@,g)) -- numbers don't do, as CONSTANTP is used for simplification.
                        (_quasiquote,
                         list_(1, (_comma, 2), (_splice, 3),
                               (_quasiquote, list_(4, (_comma, (_comma, 5)),
                                                       (_splice, 6), (_splice, (_comma, 7)))))),
                        list_(_append,
                              list_(_list, 1), list_(_list, 2), 3,
                              ## The first pass ought to be:
                              ## (_append, (_list, (_quote, 4)), (_list, (_comma, 5)), 6, (_comma, 7))
                              list_(_list, list_(_list,
                                                  list_(_quote, _append),
                                                  list_(_list, list_(_quote, _list), 4),
                                                  list_(_list, list_(_quote, _list), 5),
                                                  6,
                                                  7))),
                        printer = pp_consly))

if getenv("CL_RUN_TESTS") != "nil" and getenv("CL_TEST_QQ") != "nil":
        with progv({ _reader_trace_qqexpansion_: nil }):
                run_tests_quasiquotation()

# Cold reader

string_set("*READ-CASE*", make_keyword("upcase"))

def parse_integer(xs, junk_allowed = nil, radix = 10):
        l = len(xs)
        def hexcharp(x): return x.isdigit() or x in ["a", "b", "c", "d", "e", "f"]
        (test, xform) = ((str.isdigit, identity)      if radix == 10 else
                         (hexcharp,    float.fromhex) if radix == 16 else
                         not_implemented("PARSE-INTEGER only implemented for radices 10 and 16."))
        for end in range(0, l):
                if not test(xs[end]):
                        if junk_allowed:
                                end -= 1
                                break
                        else:
                                error("Junk in string \"%s\".", xs)
        return int(xform(xs[:(end + 1)]))

def read_symbol(x, package = None, case = None):
        # dprintf("_read_symbol >%s<, x[0]: >%s<", x, x[0])
        case = defaulted_to_var(case, _read_case_)
        name, p = ((x[1:], __keyword)
                   if x[0] == ":" else
                   poor_man_let(x.find(":"),
                                 lambda index:
                                         (if_let(find_package(x[0:index].upper()),
                                                  lambda p:
                                                          (x[index + 1:], p),
                                                  lambda:
                                                          error("Package \"%s\" doesn't exist, while reading symbol \"%s\".",
                                                                x[0:index].upper(), x))
                                          if index != -1 else
                                          (x, coerce_to_package(package)))))
        return intern(case_xform(case, name), p)[0]

def read_line(stream = None, eof_error_p = t, eof_value = nil):
        stream = defaulted_to_var(stream, _standard_input_)
        return handler_case(lambda: stream.readline(),
                            (error_t,
                             lambda c: error(end_of_file_t, "end of file on %s" % (stream,))))

def read_char(stream = None, eof_error_p = t, eof_value = nil, recursivep = nil):
        stream = defaulted_to_var(stream, _standard_input_)
        ret = stream.read(1)
        return (ret       if ret             else
                eof_value if not eof_error_p else
                error(end_of_file_t, "end of file on %s" % (stream,)))

def unread_char(x, stream = sys.stdin):
        "XXX: conformance"
        # I've found out I don't really understand how UNREAD-CHAR is supposed to work..
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
        stream = defaulted(input_stream, symbol_value(_standard_input_))
        while t:
                char = read_char(stream, eof_error_p, eof_value, recursive_p)
                if criterion(char):
                        unread_char(char, stream)
                        return char

@__block__
def cold_read(stream = sys.stdin, eof_error_p = t, eof_value = nil, preserve_whitespace = None, recursivep = nil):
        ## Has not even a remote chance of conforming.
        def read_char_maybe_eof(): return read_char(stream, nil, nil)
        def read_inner(allow_consing_dot = nil):
                skip_whitespace()
                char = read_char(stream)
                unread_char(char, stream)
                if   char == chr(40):  obj = read_list() # Org is a bit too picky
                elif char == "\"":     obj = read_string()
                elif char == "'":
                        read_char(stream)
                        obj = list_(_quote, read_inner())
                elif char == "`":
                        read_char(stream)
                        obj = (_quasiquote, read_list())
                elif char == ",":
                        ## This is a simplified take, but it'll do for bootstrapping purposes.
                        read_char(stream)
                        char = read_char(stream)
                        if char == "@":
                                obj = (_splice, read_inner())
                        else:
                                unread_char(char, stream)
                                obj = (_comma, read_inner())
                else:
                        # handle_short_read_if(pos > end)
                        obj = read_number_or_symbol()
                        if obj == find_symbol(".", __cl)[0] and not allow_consing_dot:
                                error("Unexpected consing dot in stream %s, position %%s.", stream)
                        # here("< %s" % (obj,))
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
                improper = nil
                c = read_char(stream) # it's a #\(
                while t:
                        skip_whitespace()
                        char = read_char(stream)
                        if char == "\x29":
                                break
                        else:
                                unread_char(char, stream)
                                obj = read_inner(allow_consing_dot = t)
                                if not listp(obj) and obj is find_symbol(".", __cl)[0]:
                                        improper = read_inner()
                                        skip_whitespace()
                                        char = read_char(stream)
                                        if char != "\x29":
                                                error("Unexpected character %s, where a closing paren was expected.",
                                                      repr(char))
                                        break
                                ret.append(obj)
                # here("< %s" % (ret,))
                return consify_linear(tuple(ret), last_cdr = improper) ## Beacon DEBUG-RELATED-SLOWDOWN
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
                # here("< %s" % (ret,))
                return ret
        def read_number_or_symbol():
                token = read_token()
                if without_condition_system(lambda: re.match("^[0-9]+$", token),
                                             reason = "re.match"):
                        ret = int(token)
                elif without_condition_system(lambda: re.match("^[0-9]+\\.[0-9]+$", token),
                                               reason = "re.match"):
                        ret = float(token)
                else:
                        ret = read_symbol(token)
                        # dprintf("-- interned %s as %s", token, name)
                # here("< %s" % ret)
                return ret
        def read_token():
                token = ""
                # here(">> ..%s..%s" % (pos, end))
                while t:
                        char = read_char_maybe_eof()
                        if char in set([nil, " ", "\t", "\n", "\x28", "\x29", "\"", "'"]):
                                if char is not nil:
                                        unread_char(char, stream)
                                break
                        else:
                                token += char
                # here("< %s" % repr(token))
                return token
        ret = handler_case(read_inner,
                           (end_of_file_t,
                            lambda c: error(c) if eof_error_p else
                                      return_from(cold_read, eof_value)))
        # here("lastly %s" % (ret,))
        return expand_quasiquotation(ret)
read = cold_read

def cold_read_from_string(string, eof_error_p = t, eof_value = nil,
                           start = 0, end = None, preserve_whitespace = None):
        stream = io.StringIO(string)
        try:
                return cold_read(stream, eof_error_p = eof_error_p, eof_value = eof_value,
                                  start = start, end = end, preserve_whitespace = preserve_whitespace)
        finally:
                close(stream)

read_from_string = cold_read_from_string

# Condition system

def conditionp(x):
        return isinstance(x, condition_t)

def make_condition(type, *args, **keys):
        check_type(type, symbol_t)
        if not (hasattr(type, "python_type") and
                conditionp(type.python_type)):
                error("In MAKE-CONDITION: %s does not designate a condition type.", type)
        return type.python_type(*args, **keys)

def report_handling_handover(cond, frame, hook):
        format(sys.stderr, "Handing over handling of %s to frame %s\n",
               prin1_to_string(cond), pp_chain_of_frame(frame, callers = 25))

__main_thread__ = threading.current_thread()
def report_condition(cond, stream = None, backtrace = None):
        stream = defaulted_to_var(stream, _debug_io_)
        format(stream, "%sondition of type %s: %s\n",
               (("In thread \"%s\": c" % threading.current_thread().name)
                if threading.current_thread() is not __main_thread__ else
                "C"),
               type(cond), cond)
        if backtrace:
                backtrace(-1, stream)
        return t

def maybe_reporting_conditions_on_hook(p, hook, body, backtrace = None):
        if p:
                old_hook_value = symbol_value(hook)
                def wrapped_hook(cond, hook_value):
                        "Let's honor the old hook."
                        report_condition(cond,
                                          stream = symbol_value(_debug_io_),
                                          backtrace = backtrace)
                        if old_hook_value:
                                old_hook_value(cond, old_hook_value)
                with env.maybe_let(p, **{hook if isinstance(hook, str) else symbol_name(hook): wrapped_hook}):
                        return body()
        else:
                return body()

__not_even_conditions__ = frozenset([GeneratorExit, SystemExit, __catcher_throw__])
"A set of condition types which are entirely ignored by the condition system."

intern_and_bind_globals("*STACK-TOP-HINT*", "*TRACEBACK*", "*SIGNALLING-FRAME*")

def __cl_condition_handler__(condspec, frame):
        backtrace_printed = nil
        def continuation():
                nonlocal backtrace_printed
                type, raw_cond, traceback = condspec
                # print_frames(frames_calling(frame))
                def maybe_upgrade_condition(cond):
                        "Fix up the shit routinely being passed around."
                        return ((cond, nil) if isinstance(cond, condition_t) else
                                (condspec[0](*([cond] if not sequencep(cond) or isinstance(cond, str) else
                                               cond)), t))
                        # poor_man_typecase(cond,
                        #                    (BaseException, lambda: cond),
                        #                    (str,       lambda: error_(cond)))
                cond, upgradedp = maybe_upgrade_condition(raw_cond)
                if type_of(cond) not in __not_even_conditions__ and isinstance(cond, condition_t):
                        # dprintf("signalling %s", cond)
                        if upgradedp:
                                pass
                                # here("Condition Upgrader: %s of-type %s -> %s of-type %s",
                                #       prin1_to_string(raw_cond), type_of(raw_cond),
                                #       prin1_to_string(cond), type_of(cond),
                                #       callers = 45, frame = symbol_value(_stack_top_hint_))
                        with progv({_traceback_: traceback,
                                    _signalling_frame_: frame}):
                                presignal_hook = symbol_value(_presignal_hook_)
                                if presignal_hook:
                                        with progv({_presignal_hook_: nil}):
                                                presignal_hook(cond, presignal_hook)
                                signal(cond)
                                debugger_hook = symbol_value(_debugger_hook_)
                                if debugger_hook:
                                        with progv({_debugger_hook_: nil}):
                                                debugger_hook(cond, debugger_hook)
                return cond
        signalling_frame = caller_frame(caller_relative = 1)
        with progv({_stack_top_hint_: signalling_frame}):
                cond = sys.call_tracing(continuation, ())
        if type_of(cond) not in __not_even_conditions__:
                if isinstance(cond, condition_t):
                        try:
                                repr_str = princ_to_string(cond)
                        except Exception as sub_cond:
                                dprintf("While printing condition, another condition was raised: %s", repr(sub_cond))
                                # backtrace(frame = exception_frame())
                                repr_str = "#<error printing condition>"
                        here("In thread '%s': unhandled condition of type %s:\n\n%s%s",
                              threading.current_thread().name, type_of(cond), repr_str,
                              "\n; Disabling CL condition system.",
                              callers = 15, frame = signalling_frame)
                else:
                        dprintf("In thread %s: a non-condition of type %s was raised: %s",
                                      threading.current_thread().name, type_of(cond), repr(cond))
                if not backtrace_printed:
                        backtrace(offset = 2) ## 2 = [ pytracer, __cl_condition_handler__ ]
                frost.disable_pytracer()
                try:
                        invoke_debugger(cond)
                except error_t as debugger_cond:
                        dprintf("Failed to enter the debugger:\n%s\nHave a nice day!", debugger_cond)
                        sys.stderr.flush()
                        exit()
        ## Issue UNHANDLED-CONDITIONS-NOT-REALLY
        # At this point, the Python condition handler kicks in,
        # and the stack gets unwound for the first time.
        #
        # ..too bad, we've already called all HANDLER-BIND-bound
        # condition handlers.
        # If we've hit any HANDLER-CASE-bound handlers, then we won't
        # even reach this point, as the stack is already unwound.
set_condition_handler(__cl_condition_handler__)

def handler_bind(fn, *handlers, no_error = identity):
        "Works like real HANDLER-BIND, when the conditions are right.  Ha."
        value = None

        # this is:
        #     frost.pytracer_enabled_p() and condition_handler_active_p()
        # ..inlined for speed.
        if frost.pytracer_enabled_p() and frost.tracer_hook("exception") is __cl_condition_handler__:
                # Unregistered Issue HANDLER-BIND-CHECK-ABSENT
                with progv({_handler_clusters_: (symbol_value(_handler_clusters_) +
                                                 [handlers + (("__frame__", caller_frame()),)])}):
                        return no_error(fn())
        else:
                # old world case..
                # format(t, "crap FAIL: pep %s, exhook is cch: %s",
                #        frost.pytracer_enabled_p(), __tracer_hooks__.get("exception") is __cl_condition_handler__)
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
        wrapped_handlers = [ (ty_ha[0], lambda cond: return_from(nonce, ty_ha[1](cond)))
                             for ty_ha in handlers ]
        return catch(nonce,
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
class not_implemented_condition(condition_t):
        def __init__(*args):
                self, name = args[0], args[1]
                self.name = name
        def __str__(self):
                return "Not implemented: " + self.name.upper()
        def __repr__(self):
                return self.__str__()
@defclass
class not_implemented_error(not_implemented_condition, error_t):     pass
@defclass
class not_implemented_warning(not_implemented_condition, warning_t): pass

def not_implemented(x = None):
        error(not_implemented_error,
              x if x is not None else
              caller_name())

def warn_not_implemented(x = None):
        warn(not_implemented_warning,
              x if x is not None else
              caller_name())

# Restarts

@defclass
class restart_t(servile):
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
string_set("*RESTART-CLUSTERS*", [])

def restartp(x):
        return isinstance(x, restart_t)

def restart_name(x):
        return x.name

def specs_restarts_args(restart_specs):
        # format (t, "_s_r: %s", restart_specs)
        restarts_args = make_hash_table()
        for name, spec in restart_specs.items():
                function, options = ((spec[0], spec[1]) if isinstance(spec, tuple) else
                                     (spec, make_hash_table()))
                restarts_args[name.upper()] = updated_dict(options, dict(function = function)) # XXX: name mangling!
        return restarts_args

##
# XXX: :TEST-FUNCTION is currently IGNORED!
##
def restart_bind(body, restarts_args):
        with progv({_restart_clusters_: (symbol_value(_restart_clusters_) +
                                           [_remap_hash_table(lambda _, restart_args: make_instance(restart_t, **restart_args), restarts_args)])}):
                return body()

def restart_bind(body, **restart_specs):
        return restart_bind(body, specs_restarts_args(restart_specs))

__valid_restart_options__ = frozenset(["interactive", "report", "test", "function"])
def restart_case(body, **restarts_args):
        def validate_restart_options(options):
                unknown = set(options.keys()) - __valid_restart_options__
                return t if not unknown else simple_type_error("Acceptable restart options are: (%s), not (%s)",
                                                               " ".join(__valid_restart_options__), " ".join(options.keys()))
        nonce = gensym("RESTART-CASE-")
        wrapped_restarts_args = {
                restart_name: poor_man_let(restart_args["function"],
                                  restart_args["interactive"] if "interactive" in restart_args else nil,
                                  restart_args["report"]      if "report"      in restart_args else nil,
                                  lambda function, interactive, report:
                                          (validate_restart_options(restart_args) and
                                           updated_dict(restart_args,
                                                         dict(name                 = restart_name,
                                                                  function             =
                                                                  lambda *args, **keys:
                                                                          return_from(nonce, function(*args, **keys)),
                                                                  interactive_function =
                                                                  (interactive                  if functionp(interactive) else
                                                                   lambda: []                   if interactive is nil else
                                                                   error(":INTERACTIVE argument to RESTART-CASE must be either a function or NIL.")),
                                                                  report_function      =
                                                                  (report                       if functionp(report) else
                                                                   curry(write_string, report) if isinstance(report, str) else
                                                                   nil                          if report is nil else
                                                                   error(":REPORT argument to RESTART-CASE must be either a function, a string or NIL."))))))
                for restart_name, restart_args in restarts_args.items () }
        return catch(nonce,
                      lambda: restart_bind(body, wrapped_restarts_args))

def restart_case(body, **restart_specs):
        return restart_case(body, **_specs_restarts_args(restart_specs))

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
        if restartp(identifier):
                return find_restart(restart_name(identifier)) is identifier
        else:
                for cluster in reversed(symbol_value(_restart_clusters_)):
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
        for cluster in reversed(symbol_value(_restart_clusters_)):
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
        assert(isinstance(restart, str) or restartp(restart))
        restart = restart if restartp(restart) else find_restart(restart)
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
        assert(isinstance(restart, str) or restartp(restart))
        restart = restart if restartp(restart) else find_restart(restart)
        return invoke_restart(restart, *restart.interactive_function())

# Toolkit

def astp(x):        return isinstance(x, ast.AST)

def coerce_to_ast_type(type_):
        return ((type_ if subtypep(type_, ast.AST) else error("Provided type %s is not a proper subtype of ast.AST", type_))
                if isinstance(type_, type) else
                (ast.__dict__[type_] if type_ in ast.__dict__ else error("Unknown AST type '%s'.", type_))
                if isinstance(type_, str)  else
                error("Invalid AST type specifier: %s, %s, %s.", type_, type, isinstance(type_, type)))

def text_ast(text):
        return py.compile(text, "", 'exec', flags = ast.PyCF_ONLY_AST).body

def function_ast(fn):
        fn_ast = text_ast(without_condition_system(lambda: inspect.getsource(fn)))[0]
        return fn_ast.args, fn_ast.body

def function_body_pass_p(fn):
        fn_body_ast = function_ast(fn)[1]
        return len(fn_body_ast) == 1 and isinstance(fn_body_ast[0], ast.Pass)

### literals
def ast_num(n):
        return ast.Num(n = the(integer_t, n))
def ast_bool(n):
        return ast.Bool(n = the(integer_t, n))
def ast_string(s):
        return ast.Str(s = the(string_t, s))
def ast_set(xs,   writep = nil):
        return ast.Set(elts   = the((pylist_t, ast.AST), xs), ctx = ast_rw(writep))
def ast_list(xs,  writep = nil):
        return ast.List(elts  = the((pylist_t, ast.AST), xs), ctx = ast_rw(writep))
def ast_tuple(xs, writep = nil):
        return ast.Tuple(elts = the((pylist_t, ast.AST), xs), ctx = ast_rw(writep))

__ast_efless_types__                       = { ast.Num, ast.Str, ast.Name, ast.Bytes }
__ast_potentially_efless_recursive_types__ = { ast.Tuple:  lambda x: x.elts,
                                               ast.List:   lambda x: x.elts,
                                               ast.Set:    lambda x: x.elts,
                                               ast.Dict:   lambda x: x.keys + x.values,
                                               }
def ast_efless_p(x):
        "This is fairly conservative."
        ty = type(x)
        return (ty in __ast_efless_types__
                or (ty in __ast_potentially_efless_recursive_types__
                    and all(ast_efless_p(x)
                            for x in __ast_potentially_efless_recursive_types__[ty](x))))

################################# recurse? AST-ifier
__astifier_map__ = { str:       (nil, ast_string),
                     int:       (nil, ast_num),
                     bool:      (nil, ast_num),
                     NoneType:  (nil, lambda x: ast_name("None")),
                     list:      (t,   ast_list),
                     tuple:     (t,   ast_tuple),
                     set:       (t,   ast_set),
                     ## symbol: see below
                     }
def register_astifier_for_type(type, recurse, astifier):
        "Please, list the added astifiers above."
        __astifier_map__[type] = (recurse, astifier)

def unregister_astifier_for_type(type):
        del __astifier_map__[type]

def astifiable_p(x):
        return type(x) in __astifier_map__

def try_astify_constant(x):
        if astp(x):
                return x, t
        (rec, astifier), astifiable = gethash(type_of(x), __astifier_map__,
                                              ((nil, nil), nil))
        return (astifier([ astify_constant(x) for x in x ] if rec else
                         x), t) if astifiable else (None, None)

def astify_constant(x):
        ast, successp = try_astify_constant(x)
        return (ast if successp else
                error("Cannot convert value %s to AST.  Is it a literal?",
                      prin1_to_string(x)))

def coerce_to_ast(x):
        return astify_constant(x) if not astp(x) else x

### expressions
def _ast_alias(name):                       return ast.alias(name = the(string_t, name), asname = None)
def ast_keyword(name, value):               return ast.keyword(arg = the(string_t, name), value = the(ast.expr, value))

def ast_rw(writep):                         return (ast.Store() if writep else ast.Load())
def ast_name(name, writep = nil):           return ast.Name(id = the(string_t, name), ctx = ast_rw(writep))
def ast_attribute(x, name, writep = nil):   return ast.Attribute(attr = name, value = x, ctx = ast_rw(writep))
def ast_attribute_chain(xs, writep = nil):  return reduce((lambda acc, attr: ast_attribute(acc, attr, writep)),
                                                           xs[1:],
                                                           ast_name(xs[0], writep))
def ast_index(of, index, writep = nil):     return ast.Subscript(value = of, slice = ast.Index(value = index), ctx = ast_rw(writep))
def ast_maybe_normalise_string(x):          return (ast_string(x) if isinstance(x, str) else x)

def ast_funcall(name, args = [], keys = {}, starargs = None, kwargs = None):
        check_type(args, (pylist_t, (or_t, ast.AST, NoneType, (satisfies_t, astifiable_p))))
        return ast.Call(func = (ast_name(name) if isinstance(name, str) else name),
                        args = [ coerce_to_ast(x) for x in args ],
                        keywords = maphash(ast_keyword, keys),
                        starargs = starargs or None,
                        kwargs = kwargs or None)

def ast_and(*args):
        return ast.BoolOp(ast.And(), list(args))

def ast_or(*args):
        return ast.BoolOp(ast.Or(), list(args))

### statements
def ast_Expr(node):
        return ast.Expr(value = the(ast.expr, node))

def ast_module(body, lineno = 0):
        return ast.Module(body = the((pylist_t, ast.AST), body),
                          lineno = lineno)

def ast_import(*names):
        return ast.Import(names = [ _ast_alias(x) for x in the((homotuple_t, string_t), names) ])
def ast_import_from(module_name, names):
        return ast.ImportFrom(module = the(string_t, module_name),
                              names = [ _ast_alias(x) for x in the((pylist_t, string_t), names) ],
                              level = 0)

def ast_assign(to, value):
        return ast.Assign(targets = the((pylist_t, ast.AST), to),
                          value = the(ast.AST, value))
def ast_return(node):
        return ast.Return(value = the(ast.AST, node))

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

def argspec_nfixargs(paramspec):
        return len(paramspec.args) - len(paramspec.defaults or []) # ILTW Python implementors think..

def argspec_lambda_spec(spec, astify_defaults = t):
        # args, varargs, varkw, defaults, kwonlyargs, kwonlydefaults, annotations
        nfixargs = argspec_nfixargs(spec)
        default_xform = astify_constant if astify_defaults else identity
        return (spec.args[:nfixargs],
                list(zip(spec.args[nfixargs:],
                         [ default_xform(x) for x in spec.defaults or [] ])),
                spec.varargs,
                list(zip(spec.kwonlyargs,
                         [ default_xform(x) for x in spec.kwonlydefaults or [] ])),
                spec.varkw)

def function_lambda_list(fn, astify_defaults = t):
        "Returns: FIXPARMS, OPTIONAL-WITH-DEFAULTS, VARARGS, KEYS-WITH-DEFAULTS, KWARGS."
        return argspec_lambda_spec(inspect.getfullargspec(fn), astify_defaults = astify_defaults)

def lambda_spec_arguments(lambda_list_spec):
        fixed, optional, args, keyword, keys = lambda_list_spec
        return ast.arguments(args        = [ ast.arg(x, None)
                                              for x in fixed + [ x[0] for x in optional ] ],
                              defaults    = [ x[1] for x in optional ],
                              vararg      = args,
                              kwonlyargs  = [ ast.arg(x, None)
                                              for x in [ x[0] for x in keyword ] ],
                              kw_defaults = [ x[1] for x in keyword ],
                              kwarg       = keys,
                              varargannotation = None,
                              kwargannotation  = None)

def ast_functiondef(name, lambda_list_spec, body):
        fixed, optional, args, keyword, keys = lambda_list_spec
        return ast.FunctionDef(
                name = the(string_t, name),
                args = lambda_spec_arguments(lambda_list_spec),
                lineno = 0,
                decorator_list = [],
                returns = None,
                body = poor_man_etypecase(body,
                                           ((pylist_t, ast.AST),
                                            body),
                                           (function_t,
                                            lambda:
                                                    body(*tuple(ast_name(x) for x in fixed),
                                                          **_map_into_hash(lambda x: (x, ast_name),
                                                                           (list(optional) + list(keyword) +
                                                                            ([args] if args else []) +
                                                                            ([keys] if keys else [])))))))

def ast_defun_fixed(name, names, *body):
        return ast_functiondef(name, (names, [], None, [], None),
                                list(body))

# Interning: %READ-AST, %READ-PYTHON-TOPLEVEL-AS-LISP

def literal_ast_sex(ast_):
        def fail(sex):
                import more_ast
                error("Invalid sexp: %s.", more_ast.pp_ast_as_code(sex))
        return (ast_.id                                                if isinstance(ast_, ast.Name)  else
                ast_.n                                                 if isinstance(ast_, ast.Num)   else
                consify_linear([ extract_sexp(x) for x in ast_.elts ]) if isinstance(ast_, ast.Tuple) else
                fail(ast_))

def read_ast(x):
        def rec(x):
                def read_symbol(x):
                        lisp_name = frost.python_name_lisp_symbol_name(x)
                        name, keywordp = (lisp_name, nil) if lisp_name[0] != ":" else (lisp_name[1:], t)
                        package = symbol_value(_package_) if not keywordp else __keyword
                        return intern(name, package)[0]
                return (x.n                                      if isinstance(x, ast.Num)   else
                        x.s                                      if isinstance(x, ast.Str)   else
                        read_symbol(x.id)                        if isinstance(x, ast.Name)  else
                        consify_linear([rec(e) for e in x])      if isinstance(x, list)       else
                        consify_linear([rec(e) for e in x.elts]) if isinstance(x, ast.Tuple) else
                        read_ast(x.value)                       if isinstance(x, ast.Expr)  else
                        error("LISP: don't know how to intern value %s of type %s.", x, type_of(x)))
        with progv(# {_read_case_: make_keyword("preserve")}
                   {}):
                return expand_quasiquotation(rec(x))

def read_python_toplevel_as_lisp(fn, allowed_toplevels = { "DEFUN", "DEFMACRO" }):
        def read_python_toplevel_name(f):
                symbol_name = frost.python_name_lisp_symbol_name(f.__name__)
                symbol = intern(symbol_name)[0]
                return symbol, symbol_name, f.__name__
        name, sym_name, pyname = read_python_toplevel_name(fn)
        frost.setf_global(name, pyname.lower(),
                           globals())
        args_ast, body_ast = function_ast(fn)
        if len(body_ast) > 1:
                error("In LISP %s: toplevel definitions are just that: toplevel definitions. "
                      "No more than one toplevel form is allowed per definition.", name)
        form = read_ast(body_ast[0])
        if not (symbolp(form[0]) and symbol_name(form[0]) in allowed_toplevels):
                error("In LISP %s: only toplevels in %s are allowed.",
                      repr(form[0]), __def_allowed_toplevels__)
        return name, form

# DEFAST

ast_info = poor_man_defstruct("_ast_info",
                                "type",
                                "fields",     # each field is dict(name, type, walk, [default])
                                "nfixed")
__ast_infos__                = dict()
def find_ast_info(type):     return __ast_infos__[type]

def defast(fn):
        ### generic tools
        def lambda_list_names(lambda_list, remove_optional = t):
                (fixed, optional, args, keyword, keys) = lambda_list
                xform = (lambda x: x[0]) if remove_optional else identity
                return (tuple(fixed) +
                        tuple(xform(x) for x in optional) + (() if not args else (args,)) +
                        tuple(xform(x) for x in keyword)  + (() if not keys else (keys,)))
        ### end-of-generic-tools
        def validate_defast_name(name):
                if not name.startswith("ast_"):
                        error("In DEFAST %s: the AST name must be prefixed with \"ast_\"", name)
                name = name[4:]
                ast_type, therep = gethash(name, ast.__dict__)
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
                        error("In DEFAST %s:the amount of provided type specifiers (%d) does not match the AST fields: %s",
                              name, len(ast_field_types), ast_type._fields)
                type_specifier_type = (or_t, pyanytuple_t, type)
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
        def arglist_field_infos(parameters, nfix, with_defaults, ast_field_types):
                fields = without_condition_system(lambda: collections.OrderedDict())
                def process_ast_field_arglist_entry(name, type, default, fixed = t):
                        fields[p] = (dict(name = name, type = type) if fixed else
                                     dict(name = name, type = type, default = default))
                for p, type, defaulted in zip(parameters[:nfix], ast_field_types[:nfix], with_defaults[:nfix]):
                        process_ast_field_arglist_entry(p, type, None,         fixed = t)
                for p, type, defaulted in zip(parameters[nfix:], ast_field_types[nfix:], with_defaults[nfix:]):
                        process_ast_field_arglist_entry(p, type, defaulted[1], fixed = nil)
                return fields
        lambda_list = (fixed, optional, args, keyword, keys) = function_lambda_list(fn, astify_defaults = nil)
        ast_field_types = validate_defast_lambda_list(ast_type, lambda_list, fn.__annotations__)
        parameters, with_defaults = (lambda_list_names(lambda_list),
                                     lambda_list_names(lambda_list, remove_optional = nil))
        args_ast, body_ast = function_ast(fn)
        fields = arglist_field_infos(parameters, len(fixed), with_defaults, ast_field_types)
        __ast_infos__[ast_type] = ast_info(type       = ast_type,
                                            fields     = fields,
                                            nfixed     = len(fixed))

## AST + Symbols
register_astifier_for_type(symbol_t, nil, (lambda sym: ast_funcall("find_symbol_or_fail", [symbol_name(sym)])))

# Definitions

# mod = Module(stmt* body)
#     | Interactive(stmt* body)
#     | Expression(expr body)
@defast
def ast_Module(body: (pylist_t, ast.stmt)): pass
@defast
def ast_Interactive(body: (pylist_t, ast.stmt)): pass
@defast
def ast_Expression(body: ast.expr): pass
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
def ast_FunctionDef(name:            string_t,
                     args:            ast.arguments,
                     body:           (pylist_t, ast.stmt),
                     decorator_list: (pylist_t, ast.expr) = list(),
                     returns:        (maybe_t,  ast.expr) = None): ...
#       | ClassDef(identifier name,
#                  expr* bases,
#                  keyword* keywords,
#                  expr? starargs,
#                  expr? kwargs,
#                  stmt* body,
#                  expr* decorator_list)
@defast
def ast_ClassDef(name:            string_t,
                  bases:          (pylist_t, ast.expr),
                  keywords:       (pylist_t, ast.keyword),
                  starargs:       (maybe_t,  ast.expr),
                  kwargs:         (maybe_t,  ast.expr),
                  body:           (pylist_t, ast.stmt),
                  decorator_list: (pylist_t, ast.expr)): ...
#       | Return(expr? value)
@defast
def ast_Return(value: (maybe_t, ast.expr)): pass
#       | Delete(expr* targets)
@defast
def ast_Delete(targets: (pylist_t, ast.expr)): pass
        # targets do ref, in this case!
#       | Assign(expr* targets, expr value)
@defast
def ast_Assign(targets: (pylist_t, ast.expr),
                value:    ast.expr): ...
#       | AugAssign(expr target, operator op, expr value)
@defast
def ast_AugAssign(target: ast.expr,
                   op:     ast.operator,
                   value:  ast.expr): ...

#       | For(expr target, expr iter, stmt* body, stmt* orelse)
@defast
def ast_For(target:  ast.expr,
             iter:    ast.expr,
             body:   (pylist_t, ast.stmt),
             orelse: (pylist_t, ast.stmt)): ...

#       | While(expr test, stmt* body, stmt* orelse)
@defast
def ast_While(test:    ast.expr,
               body:   (pylist_t, ast.stmt),
               orelse: (pylist_t, ast.stmt)): ...

#       | If(expr test, stmt* body, stmt* orelse)
@defast
def ast_If(test:    ast.expr,
            body:   (pylist_t, ast.stmt),
            orelse: (pylist_t, ast.stmt)): ...

#       | Raise(expr? exc, expr? cause)
@defast
def ast_Raise(exc:   (maybe_t, ast.expr),
               cause: (maybe_t, ast.expr)): pass

#       | Assert(expr test, expr? msg)
@defast
def ast_Assert(test: ast.expr,
                msg:  ast.expr = None): pass
#       | Import(alias* names)
@defast
def ast_Import(names: (pylist_t, ast.alias)):
        declare((walk, names))
#       | ImportFrom(identifier? module, alias* names, int? level)
@defast
def ast_ImportFrom(module: (maybe_t, string_t),
                    names:  (pylist_t, ast.alias),
                    level:  (maybe_t, integer_t)): ...

#       | Global(identifier* names)
@defast
def ast_Global(names: (pylist_t, string_t)): ...

#       | Nonlocal(identifier* names)
@defast
def ast_Nonlocal(names: (pylist_t, string_t)): ...

#       | Expr(expr value)
@defast
def ast_Expr(value: ast.expr): pass
#       | Pass | Break | Continue
@defast
def ast_Pass(): pass
@defast
def ast_Break(): pass
@defast
def ast_Continue(): pass
# expr = BoolOp(boolop op, expr* values)
@defast
def ast_BoolOp(op:      ast.boolop,
                values: (pylist_t, ast.expr)): pass
#      | BinOp(expr left, operator op, expr right)
@defast
def ast_BinOp(left:  ast.expr,
               op:    ast.operator,
               right: ast.expr): pass
#      | UnaryOp(unaryop op, expr operand)
@defast
def ast_UnaryOp(op:      ast.unaryop,
                 operand: ast.expr): pass
#      | Lambda(arguments args, expr body)
@defast
def ast_Lambda(args: ast.arguments,
                body: ast.expr): ...

#      | IfExp(expr test, expr body, expr orelse)
@defast
def ast_IfExp(test:   ast.expr,
               body:   ast.expr,
               orelse: ast.expr): pass
#      | Dict(expr* keys, expr* values)
@defast
def ast_Dict(keys:   (pylist_t, ast.expr),
              values: (pylist_t, ast.expr)): pass
#      | Set(expr* elts)
@defast
def ast_Set(elts: (pylist_t, ast.expr)): pass
#      | ListComp(expr elt, comprehension* generators)

@defast
def ast_ListComp(elt:         ast.expr,
                  generators: (pylist_t, ast.comprehension)): ...

#      | SetComp(expr elt, comprehension* generators)
@defast
def ast_SetComp(elt:         ast.expr,
                 generators: (pylist_t, ast.comprehension)): ...

#      | DictComp(expr key, expr value, comprehension* generators)
@defast
def ast_DictComp(key:        ast.expr,
                  value:      ast.expr,
                  generators: (pylist_t, ast.comprehension)): ...

#      | GeneratorExp(expr elt, comprehension* generators)
@defast
def ast_GeneratorExp(elt:         ast.expr,
                      generators: (pylist_t, ast.comprehension)): ...

#      | Yield(expr? value)
@defast
def ast_Yield(value: (maybe_t, ast.expr) = None): pass

#      | Compare(expr left, cmpop* ops, expr* comparators)
@defast
def ast_Compare(left:         ast.expr,
                 ops:         (pylist_t, ast.cmpop),
                 comparators: (pylist_t, ast.expr)): pass
#      | Call(expr func, expr* args, keyword* keywords, expr? starargs, expr? kwargs)
@defast
def ast_Call(func:      ast.expr,
              args:     (pylist_t, ast.expr),
              keywords: (pylist_t, ast.keyword),
              starargs: (maybe_t, ast.expr) = None,
              kwargs:   (maybe_t, ast.expr) = None): ...

#      | Num(object n) -- a number as a PyObject.
@defast
def ast_Num(n: (or_t, integer_t, float_t)): pass
#      | Str(string s) -- need to specify raw, unicode, etc?
@defast
def ast_Str(s: string_t): pass
#      | Bytes(string s)
@defast
def ast_Bytes(s: string_t): pass
#      | Ellipsis
@defast
def ast_Ellipsis(): pass
#      | Attribute(expr value, identifier attr, expr_context ctx)
@defast
def ast_Attribute(value: ast.expr,
                   attr:  string_t,
                   ctx:   ast.expr_context): pass
#      | Subscript(expr value, slice slice, expr_context ctx)
@defast
def ast_Subscript(value: ast.expr,
                   slice: ast.slice,
                   ctx:   ast.expr_context):
        declare((walk, slice))
#      | Starred(expr value, expr_context ctx)
@defast
def ast_Starred(value: ast.expr,
                 ctx:   ast.expr_context): pass
#      | Name(identifier id, expr_context ctx)
@defast
def ast_Name(id:  string_t,
              ctx: ast.expr_context): ...

#      | List(expr* elts, expr_context ctx)
@defast
def ast_List(elts: (pylist_t, ast.expr),
              ctx:   ast.expr_context): pass
#      | Tuple(expr* elts, expr_context ctx)
@defast
def ast_Tuple(elts: (pylist_t, ast.expr),
               ctx:   ast.expr_context): pass
# expr_context = Load | Store | Del | AugLoad | AugStore | Param
@defast
def ast_Load(): pass
@defast
def ast_Store(): pass
@defast
def ast_AugLoad(): pass
@defast
def ast_AugStore(): pass
@defast
def ast_Param(): pass
# slice = Slice(expr? lower, expr? upper, expr? step)
@defast
def ast_Slice(lower: (maybe_t, ast.expr) = None,
               upper: (maybe_t, ast.expr) = None,
               step:  (maybe_t, ast.expr) = None): pass
#       | ExtSlice(slice* dims)
@defast
def ast_ExtSlice(dims: (pylist_t, ast.slice)):
        declare((walk, dims))
#       | Index(expr value)
@defast
def ast_Index(value: ast.expr): pass
# boolop = And | Or
@defast
def ast_And(): pass
@defast
def ast_Or(): pass
# operator = Add | Sub | Mult | Div | Mod | Pow | LShift | RShift | BitOr | BitXor | BitAnd | FloorDiv
@defast
def ast_Add(): pass
@defast
def ast_Sub(): pass
@defast
def ast_Mult(): pass
@defast
def ast_Div(): pass
@defast
def ast_Mod(): pass
@defast
def ast_Pow(): pass
@defast
def ast_LShift(): pass
@defast
def ast_RShift(): pass
@defast
def ast_BitOr(): pass
@defast
def ast_BitXor(): pass
@defast
def ast_BitAnd(): pass
@defast
def ast_FloorDiv(): pass
# unaryop = Invert | Not | UAdd | USub
@defast
def ast_Invert(): pass
@defast
def ast_Not(): pass
@defast
def ast_UAdd(): pass
@defast
def ast_USub(): pass
# cmpop = Eq | NotEq | Lt | LtE | Gt | GtE | Is | IsNot | In | NotIn
@defast
def ast_Eq(): pass
@defast
def ast_NotEq(): pass
@defast
def ast_Lt(): pass
@defast
def ast_LtE(): pass
@defast
def ast_Gt(): pass
@defast
def ast_GtE(): pass
@defast
def ast_Is(): pass
@defast
def ast_IsNot(): pass
@defast
def ast_In(): pass
@defast
def ast_NotIn(): pass
# comprehension = (expr target, expr iter, expr* ifs)
@defast
def ast_comprehension(target: ast.expr,
                       iter:   ast.expr,
                       ifs:   (pylist_t, ast.expr)): ...

# excepthandler = ExceptHandler(expr? type, identifier? name, stmt* body)
@defast
def ast_ExceptHandler(type: (maybe_t, ast.expr),
                       name: (maybe_t, string_t),
                       body: (pylist_t, ast.stmt)): ...

# arguments = (arg* args, identifier? vararg, expr? varargannotation,
#              arg* kwonlyargs, identifier? kwarg,
#              expr? kwargannotation, expr* defaults,
#              expr* kw_defaults)
@defast
### These MAYBEs suggest a remapping facility.
def ast_arguments(args:             (pylist_t, ast.arg),
                   vararg:           (maybe_t, string_t),
                   varargannotation: (maybe_t, ast.expr),
                   kwonlyargs:       (pylist_t, ast.arg),
                   kwarg:            (maybe_t, string_t),
                   kwargannotation:  (maybe_t, ast.expr),
                   defaults:         (pylist_t, ast.expr),
                   kw_defaults:      (pylist_t, ast.expr)): ...

# arg = (identifier arg, expr? annotation)
@defast
def ast_arg(arg:         string_t,
             annotation: (maybe_t, ast.expr) = None): ...

# keyword = (identifier arg, expr value)
@defast
def ast_keyword(arg:   string_t,
                 value: ast.expr): ...

# alias = (identifier name, identifier? asname)
@defast
def ast_alias(name:    string_t,
               asname: (maybe_t, string_t) = None): ...

if neutrality.py3minor_atleast(3):
        import ast_since33
else:
        import ast_until33

# Validation: %AST-VALIDATE

__ast_walkable_field_types__ = set([ast.stmt, (pylist_t, ast.expr), (maybe_t, ast.expr),
                                    ast.expr, (pylist_t, ast.stmt)])
def ast_info_check_args_type(info, args):
        if len(args) < info.nfixed:
                error("AST type '%s' requires %s %d arguments, but only %d were provided: %s.",
                      info.type.__name__, "exactly" if len(info.fields) == info.nfixed else "at least", info.nfixed,
                      len(args), args)
        for i, (field, arg) in enumerate(zip(info.fields.values(), args)):
                if not typep(arg, field["type"]):
                        error("Argument %d (field %s) of AST '%s' must correspond to type %s, but was an instance of %s, instead: %s.",
                              i, repr(field["name"]), info.type.__name__, field["type"], type_of(arg), repr(arg))
        return t

def ast_validate(_ast):
        def rec(x, context):
                if isinstance(x, (str, int, float, NoneType)):
                        return
                if not isinstance(x, ast.AST):
                        error("%s %s is invalid: not an AST, where an AST was expected.", context, x)
                args = [ (field, getattr(x, field))
                         for field in x._fields
                         if hasattr(x, field) ]
                ast_info_check_args_type(find_ast_info(type(x)), [ fval for _, fval in args ])
                for field, ixs in args:
                        for ix in ixs if isinstance(ixs, list) else [ixs]:
                                rec(ix, "ast.%s.%s" % (type(x).__name__, field))
        rec(_ast, "root AST")
        return _ast

# Tracing

def matcher_pp(x):
        return pp_consly(x, dispatch = { dict: lambda x: repr(list(x.keys())[0]) + "::" + pp_consly(list(x.values())[0]) })

__running_tests__ = False
__enable_matcher_tracing__ = False
__matcher_tracing_immediate__ = False

string_set("*MATCHER-TRACING*", nil)
string_set("*MATCHER-DEPTH*", 0)
string_set("*MATCHER-PP-STACK*", None)

def matcher_traced():
        return __enable_matcher_tracing__ and symbol_value(_matcher_tracing_)

def setup_emt(self):
        global __enable_matcher_tracing__, __matcher_tracing_immediate__
        self.saved_emt, self.saved_immediate = __enable_matcher_tracing__, __matcher_tracing_immediate__
        __enable_matcher_tracing__ = defaulted(self.new_emt, __enable_matcher_tracing__)
        __matcher_tracing_immediate__ = defaulted(self.immediate, __matcher_tracing_immediate__)

def restore_emt(self):
        global __enable_matcher_tracing__, __matcher_tracing_immediate__
        __enable_matcher_tracing__ = self.saved_emt
        __matcher_tracing_immediate__ = self.saved_immediate

traced_matcher = defwith("traced_matcher",
                         lambda self:        setup_emt(self) or dynamic_scope_push({ _matcher_tracing_: t }),
                         lambda self, *_:  restore_emt(self) or dynamic_scope_pop(),
                         __init__ = (lambda self, new_emt = None, immediate = None:
                                             self.__dict__.update({ "new_emt": new_emt,
                                                                    "immediate": immediate })))

def matcher_print_one_arg(x):
        return ((x[0] + ": " + matcher_pp(x[1])) if isinstance(x, tuple) and len(x) is 2 and isinstance(x[0], str) else
                matcher_pp(x))

def matcher_deeper_deferred(args, name):
        level = [[name] + args]
        symbol_value(_matcher_pp_stack_).append(level)
        dynamic_scope_push({ _matcher_depth_: symbol_value(_matcher_depth_) + 1,
                             _matcher_pp_stack_: level })

def matcher_deeper_immediate(args, name):
        level = [[name] + args]
        depth = symbol_value(_matcher_depth_) + 1
        record = "%s%s    %s" % \
                 (" " * depth, name if name else caller_name().upper(),
                  ("  ".join(matcher_print_one_arg(x) for x in args)) if args else "")
        dprintf("%s", record)
        symbol_value(_matcher_pp_stack_).append(level)
        dynamic_scope_push({ _matcher_depth_: depth,
                             _matcher_pp_stack_: level })

def mrtrace(name, format, *args):
        if matcher_traced():
                symbol_value(_matcher_pp_stack_).append(
                        [(" " * (symbol_value(_matcher_depth_) + 1))
                         + name + "    " + (format % args)])

def r(retval, q = "", n = 20, ignore_callers = set(["<lambda>", "complex", "simplex"]), id_frames = False):
        if matcher_traced():
                depth = symbol_value(_matcher_depth_)
                level = symbol_value(_matcher_pp_stack_)
                name, *args = level[0]
                level[0] = ("%s%s    <==%s    %s==>    %s" %
                            (" " * depth, name if name else caller_name().upper(),
                             # retval[0],
                             matcher_pp(retval[1]),
                             matcher_pp(retval[2]),
                             ("  ".join(matcher_print_one_arg(x) for x in args)) if args else ""))
        return retval

def match_level_concede(desc, *args):
        if not matcher_traced():
                return
        depth = symbol_value(_matcher_depth_)
        level = symbol_value(_matcher_pp_stack_)
        level[0] = ("%s%s:FAIL   %s" %
                    (" " * depth, desc, ("  ".join(matcher_print_one_arg(x) for x in args)) if args else ""))

def make_ml(_, x = [], name = None, **args):
        o = object.__new__(_, **args)
        o.x, o.name = x, name
        return o

match_level_deferred = defwith("match_level_deferred",
                               lambda self: matcher_traced() and matcher_deeper_deferred(self.x, self.name),
                               lambda *_:   matcher_traced() and dynamic_scope_pop(),
                               __new__ = make_ml)

match_level_immediate = defwith("match_level_immediate",
                                  lambda self: matcher_traced() and matcher_deeper_immediate(self.x, self.name),
                                  lambda *_:   matcher_traced() and dynamic_scope_pop(),
                                  __new__ = make_ml)

def match_level(*args, **keys):
        return (match_level_immediate if __matcher_tracing_immediate__ else
                match_level_deferred)(*args, **keys)

def matcher_pp_stack_finalise(*_):
        stack = symbol_value(_matcher_pp_stack_)
        dynamic_scope_pop()
        def rec(x):
                dprintf("%s", x[0])
                for x in x[1:]:
                        rec(x)
        stack and rec(stack[0])

matcher_pp_stack = defwith("_matcher_pp_stack",
                           lambda self: __enable_matcher_tracing__ and dynamic_scope_push({ _matcher_pp_stack_: [] }),
                           lambda *_:   __enable_matcher_tracing__ and matcher_pp_stack_finalise())

# Base class

intern_and_bind_globals("*SEGMENT-ITERATION*")

intern_and_bind_symbols("%SOME", "%MAYBE", "%OR", "%FUNCHER",
                         "IR-ARGS",
                         "LAMBDA")
def segment_iteration():
        return symbol_value(_segment_iteration_)

__funchers__ = dict()

def define_funcher(name, *pattern): __funchers__[name] = xmap_to_conses(preprocess_metasex, pattern)
def funcher_name_p(x):              return isinstance(x, symbol_t) and x in __funchers__
def find_funcher(x):                return __funchers__[x]

def maybe_destructure_binding(pat):
        return ((None, pat)           if not isinstance(pat, dict) else
                tuple(pat.items())[0] if len(pat) == 1             else
                Exception("Bad pattern: %s." % (pat,)))

class matcher():
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
        ###
        def test(m, test, desc, bound, name, exp, resf:"() -> result", fail_pat,
                 if_exists:{error, replace} = error):
                with match_level(([desc + ":", test] if desc else [test])):
                        # dprintf("test: %s", exp)
                        # Unregistered Issue XXX-TEST-UNSURE-WHAT-IS-TO-BE-BOUND-RESF()-OR-EXP
                        res = resf()
                        return r(m.succ(m.bind(exp, bound, name, if_exists = if_exists), res) if test else
                                  m.fail(bound, res, fail_pat))
        def equo(m, name, exp, x):
                "Apply result binding, if any."
                b, r, f = x
                return ((m.bind(exp, b, name), r, f) if f is None else
                        x) # propagate failure as-is
        def crec(m, expat, l0, lR, combine, originalp = False):
                ## Unregistered Issue PYTHON-LACK-OF-RETURN-FROM
                b0, bR, fx0, fxR, fp0, fpR  = None, None, None, None, None, None
                def try_0():
                        nonlocal b0, fx0, fp0
                        b0, fx0, fp0 = l0()
                        if fp0 is None: return fx0
                def try_R():
                        nonlocal bR, fxR, fpR
                        bR, fxR, fpR = lR(b0)
                        if fpR is None: return fxR
                result = combine(try_0, try_R, originalp)
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
                mtd = m.__simplex_patterns__[pat[0]]
                # def exp_store():
                #         try:
                #                 return mtd.cache[exp]
                #         except TypeError:
                #                 pass
                # store = without_condition_system(exp_store)
                # if store is not None:
                #         exp_pat_hit = store.get(pat)
                #         if exp_pat_hit:
                #                 return exp_pat_hit
                # dprintf("mtd.method: %s  exp %s  pat %s", mtd.method, pp_consly(exp), pp_consly(pat))
                res = mtd.method(bound, name, exp, pat, orifst)
                if not isinstance(res, tuple):
                        error("Simplex method %s violated the B-R-F calling convention by yielding %s.", mtd.method, res)
                # if store is not None and f is None:
                #         store[pat] = res
                return res
                # return m.__simplex_patterns__[pat[0][0]](bound, name, exp, pat, orifst)
        def complex(m, bound, name, exp, pat, orifst, aux, limit):
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
                        mr.cache = collections.defaultdict(dict)
                for mr in m.__complex_patterns__.values():
                        mr.cache = collections.defaultdict(dict)
        def __init__(m):
                # without_condition_system(pdb.set_trace)
                # dprintf("__some is: %s", _some)
                #
                ## Yields:
                #
                # (Pdb) dis.disassemble(sys._getframe(11).f_code)
                # 6440           0 LOAD_GLOBAL              0 (without_condition_system) 
                #               3 LOAD_GLOBAL              1 (pdb) 
                #               6 LOAD_ATTR                2 (set_trace) 
                #               9 CALL_FUNCTION            1 
                #              12 POP_TOP              
                #
                # 6441          13 LOAD_GLOBAL              3 (dprintf) 
                #              16 LOAD_CONST               1 ('__some is: %s') 
                #              19 LOAD_GLOBAL              4 (matcher__some) 
                #              22 CALL_FUNCTION            2 
                #              25 POP_TOP              
                m.__simplex_patterns__, m.__complex_patterns__ = dict(), dict()
                m.register_complex_matcher(_some,  m.segment)
                m.register_complex_matcher(_maybe, m.maybe)
                m.register_simplex_matcher(_or,    m.or_)
                m.match_calls = 0
        def complex_matcher_not_implemented(m, bound, name, exp, pat, orifst, aux, limit):
                raise Exception("Not yet capable of matching complex patterns of type %s.", pat[0][0])
        def simplex_matcher_not_implemented(m, bound, name, exp, pat, orifst):
                raise Exception("Not yet capable of matching simplex patterns of type %s.", pat[0])
        def complex_identity(m, bound, name, exp, pat, orifst, aux, limit):
                with match_level():
                        ## Unregistered Issue IDENTITY-IGNORE-MATCHERS-COMPLEX/MATCH-USE-UNCLEAR
                        return r(m.complex(bound, name, exp, [pat[0][1][0], pat[1]], orifst, aux, limit))
        def simplex_identity(m, bound, name, exp, pat, orifst):
                with match_level():
                        ## Unregistered Issue IDENTITY-IGNORE-MATCHERS-COMPLEX/MATCH-USE-UNCLEAR
                        return r(m.simplex(bound, name, exp, pat[1][0], orifst))
        def ignore(m, bound, name, exp, pat, orifst, aux, limit):
                with match_level():
                        return r(m.match(bound, name, exp, pat[1], (False, False), aux, limit))
        ###
        # Methods to be implemented by the specific subclasses:
        # 
        # def prod(exp, originalp)
        ###
        def segment(m, bound, name, exp, pat, orifst, aux, limit, end = None):
                def posn(x, xs):
                        pos, i = xs, 0
                        while pos:
                                if x == pos[0]: return i
                                pos = pos[1]
                def constant_pat_p(pat):
                        ## Should be LISTP?
                        return not consp(tuple(pat.items())[0][1] if isinstance(pat, dict) else
                                         pat)
                ## Unregistered Issue PYTHON-DESTRUCTURING-WORSE-THAN-USELESS-DUE-TO-NEEDLESS-COERCION
                ## 1. Destructure the pattern, deduce the situation.
                seg_pat, rest_pat = pat[0][1], pat[1]
                firstp = aux is None
                ## 2. Memoize the tuple being iteratively matched upon.
                ## Unregistered Issue TRY-SIMPLIFY-OUT-AUX
                origaux, aux = aux, append(seg_pat, list_(pat[0])) if aux is None else aux
                ## 3. (Re-)establish and check the boundary.
                exlen = length(exp)
                end = (end                    if end is not None                          else
                       exlen                  if not rest_pat                             else
                       posn(rest_pat[0], exp) if rest_pat and constant_pat_p(rest_pat[0]) else
                       0)
                if ((end and end > exlen) or ## All legitimate splits failed.
                    end is None):            ## A constant pattern was missing, likewise.
                        # dprintf("   FAILing seg: %s  -at-  %s,   end:%s exlen:%s,    --  %s",
                        #               matcher_pp(pat), matcher_pp(exp), end, exlen,
                        #               "(end and end > exlen)" if (end and end > exlen) else
                        #               "end is None"           if end is None           else
                        #               error("INTERNAL ERROR"))
                        return m.fail(bound, exp, pat)
                ## 4. Compute the relevant part of the expression -- success is when this part reduces to ().
                seg_exp, rest_exp = (subseq(exp, 0, end),
                                     subseq(exp, end))
                trace_args = [("orifst", orifst), seg_exp, end, rest_exp, ("pat", pat), ("exp", exp), ("aux", origaux), ("first", firstp)]
                with match_level(trace_args):
                 with progv({ _segment_iteration_: (0 if firstp else (symbol_value(_segment_iteration_) + 1)) }):
                        ## 5. Try match at the chosen split -- the rest part first, then the segment part.
                        brf_0 = m.crec([exp, pat],
                                       lambda:
                                               ((lambda seg_bound, seg_r, seg_fail_pat, desc:
                                                         m.test(seg_fail_pat is None, desc, seg_bound, name, seg_exp, (lambda: seg_r), seg_fail_pat,
                                                                if_exists = replace))
                                                (*(## Test for success -- segment piece exhausted.
                                                   m.succ(m.bind(nil, bound, name), m.prod(nil, orifst[0]))
                                                   + ("+: segment piece exhausted",)     if seg_exp is nil else
                                                   ## Test specific for bounded matching.
                                                   m.fail(bound, exp, pat)
                                                   + ("-: segment match limit reached",) if limit == 0     else
                                                   ## Try biting one more iteration off seg_exp:
                                                   m.match(bound, name,  seg_exp,     aux,  (False,
                                                                                             firstp), aux, limit - 1)
                                                   + ("try match car",)))),
                                       lambda seg_bound:
                                               m.match(seg_bound, None, rest_exp, rest_pat,  (False, False), None, -1,
                                                       name = "CDR-SEGMATCH"),
                                       m.comh,
                                       originalp = firstp and orifst[0] and seg_exp is not nil)
                        if brf_0[2] is None:
                                return r(m.succ(brf_0[0], brf_0[1]))
                        match_level_concede("SEGMENT", *(trace_args
                                                          + [("brf_0[1]", brf_0[1]), ("brf_0[2]", brf_0[2])]))
                ## 6. Alternate length attempts.
                brf_1 = m.segment(bound, name, exp, pat, orifst, origaux, limit, end + 1)
                if brf_1[2] is None:
                        return m.succ(brf_1[0], brf_1[1])
                # mrtrace("SEGFAIL", "brf_0: %s | %s   brf_1: %s | %s", brf_0[1], brf_0[2], brf_1[1], brf_1[2])
                return m.fail(brf_0[0], *((brf_0[1], brf_0[2]) if True else
                                          (brf_1[1], brf_1[2])))
        def maybe(m, bound, name, exp, pat, orifst, aux, limit):
                ## The semantics of aux are painfully unclear here:
                ##  - we need to perform aux pass-through, for any potential surrounding segment match
                ##  - we need a clean slate for this segment..
                ## So, do we need a separate stack for aux here?  Sounds like an in inevitability..
                ## Unregistered Issue SEGMENT-MATCH-USERS-REQUIRE-AUX-DOMAIN-SEPARATION
                with match_level():
                        return r(m.segment(bound, name, exp, [[_some, pat[0][1]], pat[1]], orifst, None, 1))
        def or_(m, bound, name, exp, pat, orifst):
                alternatives = pat[1]
                def fail():                 return m.fail(bound, exp, pat)
                def post_fail(x, exp, pat): return x if x[2] is None else (x[0], exp, pat)
                def rec(current, other_options):
                        with match_level([[current, pat], exp], name = "OR"):
                                brf_0 = r(m.match(bound, name, exp, current, orifst, None, -1))
                        if brf_0[2] is None:
                                return m.succ(brf_0[0], brf_0[1])
                        brf_1 = (fail() if not other_options else
                                 rec(other_options[0], other_options[1]))
                        if brf_1[2] is None:
                                return m.succ(brf_1[0], brf_1[1])
                        return m.fail(brf_0[0], exp, pat)
                return (fail() if not alternatives else
                        post_fail(rec(alternatives[0], alternatives[1]),
                                  exp, pat))
        ## About the vzy33c0's idea:
        ## type-driven variable naming is not good enough, because:
        ## 0. mostly is already done
        ## 1. type narrows down the case analysis chain (of which there is a lot)
        ## 2. expressions also need typing..
        def match(m, bound, bname, exp, pat, orifst, aux, limit, name = "MATCH"):
                # if __running_tests__:
                #         dprintf("MATCH %s", pp_consly(exp))
                #         dprintf("PATT- %s", pp_consly(pat))
                m.match_calls += 1
                def maybe_get0Rname(pat):
                        ## Unregistered Issue PYTHON-DESTRUCTURING-WORSE-THAN-USELESS-DUE-TO-NEEDLESS-COERCION
                        (bname, pat0), patR = maybe_destructure_binding(pat[0]), pat[1]
                        return (bname, pat0, bname and m.simplex_pat_p(pat0), patR,
                                ([pat0, patR] if bname is not None else
                                 pat)) ## Attempt to avoid consing..
                ## I just caught myself feeling so comfortable thinking about life matters,
                ## while staring at a screenful of code.  In "real" life I'd be pressed by
                ## the acute sense of time being wasted..
                def pp_binding(x):
                        return repr(list(x.keys())[0]) + "::" + pp_consly(list(x.values())[0])
                with match_level([exp, ("pat", pat), ("first", orifst[1])], name = name):
                 return \
                     r(m.test(exp == pat, "atom match", bound, bname, exp, lambda: m.prod(exp, orifst[0]), pat)
                                                                                                 if not consp(pat)       else
                        m.simplex(bound, bname, exp,        pat,        (consp(exp), orifst[1])) if m.simplex_pat_p(pat) else
                        m.complex(bound, bname, list_(exp), list_(pat), orifst, None, limit)     if m.complex_pat_p(pat) else
                        m.fail(bound, exp, pat)                                                  if not listp(exp)       else
                        (lambda pat0name, pat0, pat0simplexp, patR, clean_pat:
                                 (m.equo(bname, exp,
                                         m.complex(bound, pat0name, exp, clean_pat, orifst, aux, limit))
                                                                   if m.complex_pat_p(pat0) else
                                  m.fail(bound, exp, pat)          if not consp(exp)        else
                                  m.equo(bname, exp,
                                         m.crec([exp, pat],
                                                lambda:        m.match(bound, pat0name, exp[0], pat0, (listp(exp[0]),
                                                                                                       orifst[1]),
                                                                       None, -1),
                                                (lambda b0und: m.match(b0und, None,     exp[1], patR, (False, orifst[1]), aux, limit,
                                                                       name = "CDR-MATCH")),
                                                m.comr,
                                                originalp = orifst[0]))))
                        (*maybe_get0Rname(pat)))
        def default(m, exp, pat, name = None, orifst = (True, False)):
                return m.match(dict(), name, exp, pat, orifst, None, -1)

def match(matcher, exp, pat):
        name, prepped = maybe_destructure_binding(pat)
        return matcher.default(exp, prepped, name = name)

# Preprocessing

intern_and_bind_symbols(
        "%FORM",
        "%NEWLINE", "%INDENT",
        "%BINDER", "%BIND", "%BOUND",
        "%LEAD", "%NOTLEAD", "%NOTTAIL",
        "%FOR-MATCHERS-XFORM", "%FOR-NOT-MATCHERS-XFORM")

__metasex_words__          = set() ## Populated by register_*_matcher()
__metasex_pp_words__       = { _newline, _indent, _lead, _notlead, _nottail }
__metasex_bind_words__     = { _binder, _bind, _bound }
__metasex_leaf_words__     = { _form,
                               _newline, _indent,
                               _for_matchers_xform, _for_not_matchers_xform }

def metasex_word_p(x):      return isinstance(x, symbol_t) and x in __metasex_words__
def metasex_pp_word_p(x):   return isinstance(x, symbol_t) and x in __metasex_pp_words__
def metasex_bind_word_p(x): return isinstance(x, symbol_t) and x in __metasex_bind_words__
def metasex_leaf_word_p(x): return isinstance(x, symbol_t) and x in __metasex_leaf_words__

def preprocess_metasex(pat):
        "Expand syntactic sugars."
        def consify_metasex(x):
                return (xmap_to_conses(consify_metasex, x)                if isinstance(x, tuple) else
                        ## %SOME preprocessing is special-cased here, since it is the only reliable place to do it..
                        list__(_some, xmap_to_conses(consify_metasex, x)) if isinstance(x, list)  else
                        { k: consify_metasex(v)
                          for k, v in x.items() }                         if isinstance(x, dict)  else
                        x)
        def rec(x):
                l = list_
                # dprintf("rec  %s", pp_consly(x))
                def prep_binding(b):
                        k, v = tuple(b.items())[0]
                        return { k: rec(v)[0] }
                def prep_linear(xs):
                        return mapcon(lambda con: rec(con[0]), xs)
                return (l(prep_binding(x))             if isinstance(x, dict)                 else
                        l(l(_indent, 1))              if x == " "                            else
                        l(l(_newline, 0))             if x == "\n"                           else
                        l(l(_newline, x))             if isinstance(x, int)                  else
                        l(x)                           if atom(x)                             else
                        (error("Reference to an undefined funcher %s in pattern %s.",
                               x[1][0], pat) if not funcher_name_p(x[1][0]) else
                         find_funcher(x[1][0]))      if x[0] is _funcher                    else
                        l(prep_linear(x)))
        consified = consify_metasex(pat)
        ret = rec(consified)[0]
        # dprintf("Preprocessed\n%s  ->\n%s  ->\n%s", pat, pp_consly(consified), pp_consly(ret))
        return ret

def strip_metasex(form, strip_pp = nil, strip_bind = nil):
        def de_bind(x):
                return (x[1][0]    if x[0] is _bound           else
                        x[1][1][0] if x[0] in (_bind, _binder) else
                        error("Invalid form for binder stripping: %s", pp_consly(x)))
        def rec(xs):
                return mapcon(lambda x: (list_(x[0])                if not consp(x[0])                                  else
                                         list_(strip_metasex(x[0])) if not (symbolp(x[0][0]) or strip_pp or strip_bind) else
                                         nil                        if (strip_pp   and   metasex_pp_word_p(x[0][0]))    else
                                         rec(list_(de_bind(x[0])))  if (strip_bind and metasex_bind_word_p(x[0][0]))    else
                                         list_(rec(x[0]))),
                              xs)
        ret = rec(list_(form))[0]
        # dprintf("STRIP %s: %s\n--->\n%s", "pp" if strip_pp else "bind" if strip_bind else "???", pp_consly(form), pp_consly(ret))
        return ret

# METASEX-MATCHER, METASEX-MATCHER-PP and METASEX-MATCHER-NONSTRICT-PP

#         metasex presents us with an excellent lesson.  Let's try to understand.

string_set("*METASEX-KIND*", "metasex")

def form_metasex(form, kind = "metasex"):
        "Return a normalised metasex for FORM."
        ## Unregistered Issue FORM-METASEX-SHOULD-COMPUTE-METASEX-OF-DEFINED-MACROS
        ## Unregistered Issue FORM-METASEX-TOO-RELAXED-ON-ATOMS
        ## Unregistered Issue FORM-METASEX-STATICALLY-PRECLUDES-MULTILINE-FORMS
        return (preprocess_metasex((_typep, t))    if not consp(form)                                                  else
                getattr(find_known(form[0]), kind)  if isinstance(form[0], symbol_t) and find_known(form[0])           else
                preprocess_metasex((([(_notlead, " "), (_form,)] if kind == "metasex_pp" else
                                      [                  (_form,)]),)))

def form_real(x):
        return (x if not (consp(x) and x[0] is _ir_args) else
                x[1][0])

def combine_t_or_None(f0, fR, originalp):
        f0r = f0()
        if f0r is not None:
                fRr = fR()
                if fRr is not None:
                        return t

def combine_pp(f0, fR, originalp, consdotp):
        def orig_tuple_comb(body):
                new_base = pp_depth() + 1
                with progv({ _pp_base_depth_: new_base }):
                        ret = body(new_base)
                        return None if ret is None else ("\x28" + ret + "\x29")
        def body(base):
                f0r = f0()
                if f0r is None: return
                with progv({ _pp_depth_: base + len(f0r.split("\n")[-1]) }):
                        fRr = fR()
                        if fRr is None: return
                        return f0r + (" . " if consdotp else "") + fRr
        return (orig_tuple_comb if originalp else
                lambda f: f(pp_base_depth()))(body)

def combine_cons(f0, fR, originalp):
        f0r = f0()
        if f0r is not None:
                fRr = fR()
                if fRr is not None:
                        return [f0r, fRr]

def combine_append(f0, fR, originalp):
        f0r = f0()
        if f0r is not None:
                fRr = fR()
                if fRr is not None:
                        return append(f0r, fRr)

intern_and_bind_symbols("%SATISFIES", "%CONS", "%TYPEP")

class metasex_matcher_t(matcher):
        def __init__(m):
                matcher.__init__(m)
                m.register_simplex_matcher(_form,             m.form)
                m.register_simplex_matcher(_satisfies,        m.satisfies)
                m.register_simplex_matcher(_cons,             m.cons)
                m.register_simplex_matcher(_typep,            m.typep)
        @staticmethod
        def prod(x, originalp):      return t
        @staticmethod
        def comh(f0, fR, originalp): return combine_t_or_None(f0, fR, originalp)
        @staticmethod
        def comr(f0, fR, originalp): return combine_t_or_None(f0, fR, originalp)
        @staticmethod
        def comc(f0, fR, originalp): return combine_t_or_None(f0, fR, originalp)
        def process_formpat_arguments(m, form, pat):
                arg_handlers = { _for_matchers_xform:     (lambda arg: find(m, arg[1]),
                                                           lambda arg: arg[0](form)),
                                 _for_not_matchers_xform: (lambda arg: not find(m, arg[1]),
                                                           lambda arg: arg[0](form)),
                                 }
                if consp(pat):
                        args = pat[1]
                        # dprintf("args of %s: %s", pp_consly(pat), pp_consly(args))
                        ptr = args
                        while ptr:
                                argname, argval = ptr[0]
                                if argname not in arg_handlers:
                                        error("Invalid FORM argument: %s, pat: %s", argname, pat)
                                arg_applicable_p, arg_handler = arg_handlers[argname]
                                if arg_applicable_p(argval):
                                        ret = arg_handler(argval)
                                        # dprintf("\n\nMATCHED/xformed: %s ->\n%s", pp_consly(form), pp_consly(ret))
                                        return True, ret
                                ptr = ptr[1]
                return None, None
        def cons(m, bound, name, form, pat, orifst):
                with match_level([form, pat[1]]):
                        return r(m.crec([form, pat],
                                         lambda:    m.match(bound, None, form[0], pat[1][0],    (orifst[0], True),  None, -1),
                                         lambda b0: m.match(b0,    None, form[1], pat[1][1][0], (orifst[0], False), None, -1,
                                                            name = "CDR-CONSMATCH"),
                                         m.comc,
                                         originalp = orifst[0]))
        def form(m, bound, name, form, pat, orifst):
                with match_level([form, pat]):
                        ## This is actually a filter.
                        handled, ret = m.process_formpat_arguments(metasex, form, pat)
                        if handled:
                                return m.succ(bound, ret)
                        form_pat = form_metasex(form, kind = symbol_value(_metasex_kind_))
                        return r(m.match(bound, name, form, form_pat, (consp(form), orifst[1]), None, -1))
        def satisfies(m, bound, name, form, pat, orifst):
                with match_level([form, pat]):
                        return r(m.test(pat[1][0](form), "satisfies", bound, name, form,
                                         lambda: m.prod(form, orifst), pat))
        def typep(m, bound, name, form, pat, orifst):
                with match_level([form, pat[1][0]]):
                        return r(m.test(typep(form, pat[1][0]), "typep", bound, name, form,
                                         lambda: m.prod(form, orifst), pat))

metasex = metasex_matcher_t()

def match_sex(sex, pattern = None, matcher = None):
        matcher = defaulted(matcher, metasex)
        return match(matcher, sex, defaulted(pattern, form_metasex(sex)))

# Mapping

string_set("*MAPPER-FN*", nil)

class metasex_mapper_t(metasex_matcher_t):
        def __init__(m):
                metasex_matcher_t.__init__(m)
                m.register_simplex_matcher(_form, m.form)
        @staticmethod
        def prod(x, _):      return x
        @staticmethod
        def comh(f0, fR, _): return combine_append(f0, fR, nil)
        @staticmethod
        def comr(f0, fR, _): return combine_cons(f0, fR, nil)
        @staticmethod
        def comc(f0, fR, _): return combine_cons(f0, fR, nil)
        def form(m, bound, name, form, pat, orifst):
                # dprintf("MM.FORM %s", pp_consly(form, max_depth = 10))
                def mapper_continuation(xformed):
                        return metasex_matcher_t.form(m, bound, name, xformed, pat, orifst)
                handled, ret = m.process_formpat_arguments(form, pat) if not ignore_args else (None, None)
                if handled:
                        return m.succ(bound, ret)
                return symbol_value(_mapper_fn_)(form, mapper_continuation)

metasex_mapper = metasex_mapper_t()

def map_sex(fn: "Form -> (Form -> ({} Form Bool)) -> ({} Form Bool)",
             sex, matcher = metasex_mapper, start_inside = nil) -> "({} Form Bool)":
        with progv({ _mapper_fn_: fn }):
                return match(matcher, sex, form_metasex(sex) if start_inside else list_(_form))

def xform_ir(fn: "Form -> (Form -> ({} Form Bool)) -> ({} Form Bool)",
              sex, matcher = metasex_mapper, start_inside = nil) -> "Form":
        with matcher_pp_stack():
                _, r, f = map_sex(fn, sex, matcher, start_inside)
        if f is not None:
                error("\n=== failed sex: %s\n=== failsubpat: %s\n=== subex: %s", pp_consly(sex), matcher_pp(f), pp_consly(r))
        return r

# Pretty-printing

intern_and_bind_symbols("%NEWLINE", "%INDENT", "%LEAD", "%NOTLEAD", "%NOTTAIL")

class metasex_matcher_pp_t(metasex_matcher_t):
        def __init__(m):
                metasex_matcher_t.__init__(m)
                m.register_complex_matcher(_newline,     m.newline)
                m.register_complex_matcher(_indent,      m.indent)
                m.register_complex_matcher(_lead,        m.lead)
                m.register_complex_matcher(_notlead,     m.notlead)
                m.register_complex_matcher(_nottail,     m.nottail)
        @staticmethod
        def prod(x, originalp):
                result = (""            if x is nil and (not originalp) else
                          '"' + x + '"' if isinstance(x, str)           else
                          str(x))
                return result
        @staticmethod
        def comh(f0, fR, originalp): return combine_pp(f0, fR, originalp, nil)
        @staticmethod
        def comr(f0, fR, originalp): return combine_pp(f0, fR, originalp, nil)
        @staticmethod
        def comc(f0, fR, originalp): return combine_pp(f0, fR, originalp, t)
        def newline(m, bound, name, exp, pat, orifst, aux, limit):
                n, tail = pat[0][1][0], pat[1]
                new_base = pp_base_depth() + n
                with progv({ _pp_depth_:      new_base,
                             _pp_base_depth_: new_base }):
                        return m.post(m.match(m.bind(new_base, bound, name), None, exp, tail, orifst, aux, -1),
                                      lambda r: "\n" + (" " * new_base) + r)
        def indent(m, bound, name, exp, pat, orifst, aux, limit):
                n, tail = pat[0][1][0], pat[1]
                new_depth = pp_depth() + n
                with progv({ _pp_depth_: new_depth }):
                        return m.post(m.match(m.bind(new_depth, bound, name), None, exp, tail, orifst, aux, -1),
                                      lambda r: (" " * n) + r)
        def lead(m, bound, name, exp, pat, orifst, aux, limit):
                maybe_pat = pat[0][1][0]
                if not orifst[1]:
                        return m.match(bound, name, exp, pat[1],              orifst, aux, limit)
                ############## act as identity
                return         m.match(bound, name, exp, [maybe_pat, pat[1]], orifst, aux, limit)
        def notlead(m, bound, name, exp, pat, orifst, aux, limit):
                maybe_pat = pat[0][1][0]
                mrtrace("NOTLEAD", "lead: %s, %sing %s",
                         orifst[1], "ignor" if orifst[1] else "process", pp_consly(maybe_pat))
                if orifst[1]:
                        return m.match(bound, name, exp, pat[1],              orifst, aux, limit)
                ############## act as identity
                return         m.match(bound, name, exp, [maybe_pat, pat[1]], orifst, aux, limit)
        def nottail(m, bound, name, exp, pat, orifst, aux, limit):
                maybe_pat = pat[0][1][0]
                before_end = exp is nil
                if before_end:
                        return m.match(bound, name, exp, pat[1],              orifst, aux, limit)
                ############## act as identity
                return         m.match(bound, name, exp, [maybe_pat, pat[1]], orifst, aux, limit)

metasex_pp = metasex_matcher_pp_t()

def pp_sex(sex, strict = t, initial_depth = None):
        ## Unregistered Issue RELAXED-METASEX-PRETTY-PRINTER-MODE-NEEDED
        initial_depth = defaulted_to_var(initial_depth, _pp_base_depth_)
        pat = form_metasex(sex, kind = "metasex_pp")
        with progv({ _pp_depth_:         initial_depth,
                     _pp_base_depth_:    initial_depth,
                     _metasex_kind_:     "metasex_pp" }): ## Guide the nested %FORM-METASEX invocations.
                with matcher_pp_stack():
                        _, r, f = match(metasex_pp, sex, pat)
        if f is not None:
                error("\n=== failed sex: %s\n=== failpat: %s\n=== failsubpat: %s\n=== subex: %s",
                      matcher_pp(sex), matcher_pp(pat), matcher_pp(f), matcher_pp(r))
        return r or ""

def ir_minify(form):
        return ('"%s"' % form if stringp(form)                                else
                str(form)     if symbolp(form) or not form or not consp(form) else
                ("(%s ...)" % ir_minify(form[0])))

def mock(sex, initial_depth = None, max_level = None):
        max_level = defaulted(max_level, compiler_max_mockup_level)
        def mock_atom(x):       return '"' + x + '"' if isinstance(x, str) else str(x)
        def mock_complexes(xs, new_level):
                with progv({ _pp_base_depth_: symbol_value(_pp_base_depth_) + 2 }):
                        return ("\n" + sex_space()).join(rec(x, new_level) for x in xs)
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
                                return ("\x28" + " ".join(mock_atom(x)
                                                       for x in [car] + vectorise_linear(simple_tail)) +
                                        ("" if not complex_tail else
                                         ((("\n  " + sex_space()) if complex_tail and level < max_level else "") +
                                          (mock_complexes(complex_tail, level + 1) if level < max_level else
                                                         " ..more.."))) +
                                        "\x29")
                        else:
                                return (mock_complexes(sex, level + 1) if level < max_level else
                                        "..more..")
        initial_depth = defaulted_to_var(initial_depth, _pp_base_depth_)
        with progv({ _pp_base_depth_: initial_depth }):
                return rec(sex, 0)

# Compiler conditions

@defclass
class compiler_error(error_t):
        pass

@defclass
class simple_compiler_error(simple_condition_t, compiler_error):
        pass

@defun
def compiler_error(control, *args):
        return simple_compiler_error(control, *args)

# Form parsing

def namep(x):
        return isinstance(x, symbol_t) and symbol_package(x) is not __keyword

intern_and_bind_symbols("&WHOLE", "&OPTIONAL", "&REST", "&BODY", "&KEY", "&ALLOW-OTHER-KEYS", "&AUX", "&ENVIRONMENT")

__lambda_words__ = { _whole, _optional, _rest, _body, _key, _allow_other_keys, _aux, _environment }

def lambda_word_p(x):
        return isinstance(x, symbol_t) and x in __lambda_words__

intern_and_bind_symbols("DECLARE")

def parse_body(body, doc_string_allowed = t):
        doc = nil
        def doc_string_p(x, remaining_forms):
                return ((error("duplicate doc string %s", x) if doc else t)
                        if isinstance(x, str) and doc_string_allowed and remaining_forms else
                        None)
        def declaration_p(x):
                return isinstance(x, tuple) and x[0] is _declare
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

eval_when_ordered_keywords = _compile_toplevel, _load_toplevel, _execute
eval_when_ordered_legacy_keywords = _compile, _load, _eval
eval_when_keywords = set(eval_when_ordered_keywords) | set(eval_when_ordered_legacy_keywords)
def parse_eval_when_situations(situ_form):
        situ_linear = vectorise_linear(situ_form)
        all(isinstance(x, symbol_t) for x in situ_linear) or error("In EVAL-WHEN: invalid situation form: %s",
                                                                   pp_consly(situ_form))
        if not (listp(situ_form) and not (set(situ_linear) - eval_when_keywords)):
                error("In EVAL-WHEN: the first form must be a list of following keywords: %s.", eval_when_keywords)
        return [ (new    in situ_linear or
                  legacy in situ_linear)
                 for new, legacy in zip(eval_when_ordered_keywords,
                                        eval_when_ordered_legacy_keywords) ]

def analyse_eval_when_situations(compile_time_too, ct, lt, e):
        "Implement the EVAL-WHEN chart of section #5 of CLHS 3.2.3.1."
        process = lt
        eval = ct or (compile_time_too and e)
        new_compile_time_too = lt and eval
        return new_compile_time_too, process, eval

def process_decls(decls, vars, fvars):
        warn_not_implemented()

def self_evaluating_form_p(x):
        return isinstance(x, (int, str, float)) or x in [t, nil]

# Debugging, tracing and pretty-printing

compiler_max_mockup_level = 3

string_set("*COMPILER-TRACE-FORMS*",              nil)
string_set("*COMPILER-TRACE-MACROEXPANDED*",      nil)
string_set("*COMPILER-TRACE-REWRITTEN*",          nil)
string_set("*COMPILER-TRACE-PRIMITIVES*",         nil)
string_set("*COMPILER-TRACE-AST*",                nil)
string_set("*COMPILER-TRACE-MODULE-AST*",         nil)
string_set("*COMPILER-TRACE-BYTECODE*",           nil)

string_set("*COMPILER-TRACE-TOPLEVELS*",          nil)
string_set("*COMPILER-TRACE-COMPILE-TIME-EVAL*",  nil)

string_set("*COMPILER-TRACE-SUBEXPANSION*",       nil)
string_set("*COMPILER-TRACE-SUBREWRITING*",       nil)
string_set("*COMPILER-TRACE-SUBPRIMITIVISATION*", nil)
string_set("*COMPILER-TRACE-SUBASTIFICATION*",    nil)
string_set("*COMPILER-TRACE-INNER-KNOWNS*",       nil)
string_set("*COMPILER-TRACE-KNOWN-CHOICES*",      nil)
string_set("*COMPILER-TRACE-KNOWN-PRIMITIVES*",   nil)

string_set("*COMPILER-TRACE-PRETTY-FULL*",        nil)

string_set("*COMPILER-VALIDATE-AST*",             nil)
string_set("*COMPILER-TRAPPED-FUNCTIONS*",        set()) ## Emit a debug entry for those functions.

compiler_debugless_traceless_frame = { _compiler_trace_forms_:              nil,
                                       _compiler_trace_macroexpanded_:      nil,
                                       _compiler_trace_rewritten_:          nil,
                                       _compiler_trace_primitives_:         nil,
                                       _compiler_trace_ast_:                nil,
                                       _compiler_trace_module_ast_:         nil,
                                       _compiler_trace_bytecode_:           nil,

                                       _compiler_trace_toplevels_:          nil,
                                       _compiler_trace_compile_time_eval_:  nil,

                                       _compiler_trace_subexpansion_:       nil,
                                       _compiler_trace_subrewriting_:       nil,
                                       _compiler_trace_subprimitivisation_: nil,
                                       _compiler_trace_subastification_:    nil,
                                       _compiler_trace_inner_knowns_:       nil,
                                       _compiler_trace_known_choices_:      nil,
                                       _compiler_trace_known_primitives_:   nil,

                                       _compiler_trace_pretty_full_:       nil,
                                       _compiler_validate_ast_:            nil,
                                       _compiler_trapped_functions_:       set() }

__known_trace_args__ = { "forms", "macroexpanded", "rewritten", "primitives", "ast", "module_ast", "bytecode",
                         "toplevels", "compile_time_eval",
                         "subexpansion", "subrewriting", "subprimitivisation", "subastification", "inner_knowns", "known_choices", "known_primitives",
                         "pretty_full" }

def compiler_explain_tracing():
        def control_var_name(x): return "*COMPILER-TRACE-%s*" % x.replace("_", "-").upper()
        dprintf(";;  compiler trace config:")
        for var in __known_trace_args__:
                dprintf(";;    %s: %s", control_var_name(var), symbol_value(find_symbol(control_var_name(var))[0]))

def compiler_dbgconf(**keys):
        def control_var_name(x): return "*%s%s*" % (("COMPILER-TRACE-" if x in __known_trace_args__ else ""),
                                                    x.replace("_", "-").upper())
        for namespec, value in keys.items():
                string_set(control_var_name(namespec), value)

string_set("*PP-BASE-DEPTH*", 0)
string_set("*PP-DEPTH*", 0)
def pp_base_depth(): return symbol_value(_pp_base_depth_)
def pp_depth():      return symbol_value(_pp_depth_)

def sex_space(delta = None, char = " "):
        return char * (pp_base_depth() + defaulted(delta, 0))
def sex_deeper(n, body):
        with progv({ _pp_base_depth_: pp_base_depth() + n }):
                return body()

def pp(x, **args):
        return (pp_sex if symbol_value(_compiler_trace_pretty_full_) else mock)(x, **args)

## Compiler messages:
## - entry        lower:rec()                             ;* lowering
##                dprintf(";;;%s lowering:\n%s%s", sex_space(-3, ";"), sex_space(), pp_sex(x))
## - part listing lower:call_known()                       >>> parts
## - rewriting    lower:call_known()                       ===\n---\n...
## - result       lower()                                 ;* compilation atre_ output\n;;; Prologue\n;;; Value

def compiler_trap_function(name):
        symbol_value(_compiler_trapped_functions_).add(name)

def compiler_function_trapped_p(name):
        return name in symbol_value(_compiler_trapped_functions_)

def compiler_trace_known_choice(ir_name, id, choice):
        if symbol_value(_compiler_trace_known_choices_):
                dprintf("%s-- %s %s: %s", sex_space(), ir_name, ir_minify(id), choice)

# Bindings

intern_and_bind_symbols(
        "SYMBOL",
        "VARIABLE", "CONSTANT", "SPECIAL", "SYMBOL-MACRO",
        "MACRO", "COMPILER-MACRO", "FUNCTION", "BLOCK", "GOTAG")

class nameuse():
        name, kind, type = None, None, None
        def __init__(self, name, kind, type, **attributes):
                attrify_args(self, locals(), "name", "kind", "type")
def nameusep(x):
        return isinstance(x, nameuse)

class binding():
        value, shadows = None, None
        def __init__(self, value, shadows = None, **attributes):
                attrify_args(self, locals(), "value", "shadows")
        def __repr__(self):
                return "#<bind %s %s: %s>" % (self.kind, self.name, self.value)
def bindingp(x):
        return isinstance(x, binding)

class variable(nameuse):
        def __init__(self, name, kind, type = t, dynamic_extent = nil, **attributes):
                check_type(kind, (member_t, _variable, _constant, _special, _symbol_macro)) # CONSTANT | SPECIAL | SYMBOL-MACRO | VARIABLE
                nameuse.__init__(self, name, kind, type, **attributes)
                attrify_args(self, locals(), "dynamic_extent") # t | nil
                self.tn = nil
class function(nameuse):
        def __init__(self, name, kind, type = t, lambda_expression = None, **attributes):
                check_type(kind, (member_t, _function, _macro, _compiler_macro)) # MACRO | COMPILER-MACRO | FUNCTION
                nameuse.__init__(self, name, kind, type, **attributes)
                attrify_args(self, locals(), "lambda_expression") # None | cons
                self.tn = nil
class block(nameuse):
        def __init__(self, name, **attributes):
                nameuse.__init__(self, name, _block, t, **attributes)
class gotag(nameuse):
        def __init__(self, name, **attributes):
                nameuse.__init__(self, name, _gotag, t, **attributes)

def nameuse_variablep(x): return isinstance(x, variable)
def nameuse_functionp(x): return isinstance(x, function)
def nameuse_blockp(x):    return isinstance(x, block)

class variable_binding(variable, binding):
        def __init__(self, name, kind, value, tn = nil, **attributes):
                ## Variables are not necessarily bound to specific value forms -- function parameters.
                variable.__init__(self, name, kind, **attributes)
                binding.__init__(self, value, **attributes)
                if tn:
                        self.tn = tn
        def allocate_tn(self):
                ## This calls onto primitive IR
                self.tn = variable_tn(self.name)
class function_binding(function, binding):
        def __init__(self, name, kind, value, tn = nil, **attributes):
                function.__init__(self, name, kind, **attributes)
                binding.__init__(self, value, **attributes)
                if tn:
                        self.tn = tn
        def allocate_tn(self):
                ## This calls onto primitive IR
                self.tn = function_tn(self.name)
class block_binding(block, binding):
        def __init__(self, name, _, value, **attributes):
                block.__init__(self, name, **attributes)
                binding.__init__(self, value, **attributes)
class gotag_binding(gotag, binding):
        def __init__(self, name, _, value, **attributes):
                gotag.__init__(self, name, **attributes)
                binding.__init__(self, value, **attributes)

def variable_bindingp(x): return isinstance(x, variable_binding)
def function_bindingp(x): return isinstance(x, function_binding)
def block_bindingp(x):    return isinstance(x, block_binding)
def gotag_bindingp(x):    return isinstance(x, gotag_binding)

# Lexenv

intern_and_bind_globals("*LEXENV*")
intern_and_bind_symbols("NULL")
intern_and_bind_symbols("%BOOTSTRAP-NULL-LEXENV")

def with_lexenv(lexenv, fn):
        with progv({ _lexenv_: lexenv }):
                return fn()

@defclass(intern("%LEXENV")[0])
class lexenv_t():
        """Chains variable and function scope pieces together.  Scope pieces map binding kinds
           to binding sets and bound names to bindings."""
        clambda = nil
        varscope, funcscope, blockscope, gotagscope = nil, nil, nil, nil
        varframe, funcframe, blockframe, gotagframe = nil, nil, nil, nil
        def __repr__(self):
                kinds    = [ (name, nil) for name in [ "var", "func", "block", "gotag" ]
                             if getattr(self, name + "frame") ]
                frames   = [ getattr(self, kind + "frame") for kind, _ in kinds ]
                namesets = [ sorted(map(str, frame["name"].keys())) for frame in frames ]
                def present_kind(kind, nameset):
                        return "%s: %s" % (kind[0], ", ".join(nameset))
                return "#<LEXENV  %s  %s>" % ("empty" if not kinds else
                                              "  ".join(map(present_kind, kinds, namesets)),
                                              self.clambda)
        def __init__(self, parent = nil, clambda = None, allocate_tns = nil,
                     name_varframe = None, name_funcframe = None, name_blockframe = None, name_gotagframe = None,
                     kind_varframe = None, kind_funcframe = None, kind_blockframe = None, kind_gotagframe = None,
                     full_varframe = None, full_funcframe = None, full_blockframe = None, full_gotagframe = None):
                # for k, v in self.data.items():
                #         symbolp(k)   or error("Lexenv scope keys must be symbols, found: %s.",    k.__repr__())
                #         bindingp(k) or error("Lexenv scopt values must be bindings, found: %s.", v.__repr__())
                def complete_name_frame(framespec):
                        res = collections.defaultdict(set)
                        res["name"] = framespec
                        for b in framespec.values():
                                check_type(b, binding)
                                res[b.kind].add(b)
                        return res
                def complete_kind_frame(framespec):
                        res = dict(framespec)
                        res["name"] = names = dict()
                        for set_ in framespec.values():
                                for b in set_:
                                        names[b.name] = the(binding, b)
                        return res
                def complete_frame(name, kind, full):
                        return (full                      if full else
                                complete_name_frame(name) if name else
                                complete_kind_frame(kind) if kind else
                                None)
                if parent is not _bootstrap_null_lexenv:
                        self.parent   = coerce_to_lexenv(parent)
                        if not (self.parent or clambda):
                                error("Asked to create a lexenv without provision of either a clambda or a parent lexenv.")
                        self.clambda  = defaulted(clambda, (self.parent.clambda if parent is not _null else
                                                            nil))
                else:
                        parent = nil
                        self.parent, self.clambda = nil, clambda
                ((self.varscope,   self.varframe),
                 (self.funcscope,  self.funcframe),
                 (self.blockscope, self.blockframe),
                 (self.gotagscope, self.gotagframe)
                 ) = (self.adjoin_scope(self.parent,  "varscope",
                                        complete_frame(name_varframe,   kind_varframe,   full_varframe)),
                      self.adjoin_scope(self.parent, "funcscope",
                                        complete_frame(name_funcframe,  kind_funcframe,  full_funcframe)),
                      self.adjoin_scope(self.parent, "blockscope",
                                        complete_frame(name_blockframe, kind_blockframe, full_blockframe)),
                      self.adjoin_scope(self.parent, "gotagscope",
                                        complete_frame(name_gotagframe, kind_gotagframe, full_gotagframe)))
                if allocate_tns:
                        self.allocate_tns()
        def adjoin_scope(self, parent_lexenv, sname, frame):
                pscope = getattr(parent_lexenv, sname) if parent_lexenv else nil
                return (pscope if not frame else
                        (frame,
                         (nil if parent_lexenv is nil else pscope),
                         self)
                        ), frame
        def allocate_tns(self):
                for slot in ["varframe", "funcframe"]:
                        frame = getattr(self, slot)
                        if frame:
                                for binding in frame["name"].values():
                                        binding.allocate_tn()
        @staticmethod
        def merge_frames(f0, f1):
                res = dict(f0)
                for key, map in f1.items():
                        res[key] = (dictappend(res[key], map) if key in res else
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
        #         result = lexenv(clambda)
        #         for name, frame in zip(names, frames):
        #                 setattr(result, name + "frame", frame)
        #                 setattr(result, name + "scope", self.adjoin_scope(parent, name + "scope", frame)[0])
        #         return result
        @staticmethod
        def do_lookup_scope(scope, x, default):
                while scope:
                        frame, rest, lexenv = scope
                        if not isinstance(frame, dict):
                                dprintf("bad scope: %s", scope)
                        if x in frame["name"]:
                                return frame["name"][x], lexenv
                        scope = rest # COLD-CDR
                return default, None
        def print(self):
                dprintf("===  Dumping lexenv:")
                for name, scope in [("var", self.varscope), ("func", self.funcscope), ("block", self.blockscope), ("gotag", self.gotagscope)]:
                        dprintf("=====  Scope:  %s", name)
                        while scope:
                                frame, rest, lexenv = scope
                                dprintf("---  %s", frame)
                                scope = rest # COLD-CDR
        def lookup_var(self, x, default = None):          return self.do_lookup_scope(self.varscope, x, default)
        def lookup_func(self, x, default = None):         return self.do_lookup_scope(self.funcscope, x, default)
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

def lexenvp(x):                return isinstance(x, lexenv_t)
def make_null_lexenv(clambda): return lexenv_t(clambda = clambda, parent = _bootstrap_null_lexenv)
def make_lexenv(parent = nil, clambda = None, **initargs):
        """ :PARENT - NULL for a null lexenv, nil for the value of *LEXENV*.
            :{NAME,KIND,FULL}-{VAR,FUNC,BLOCK}FRAME - constituents."""
        return lexenv_t(parent, clambda = clambda, **initargs)

def coerce_to_lexenv(x):
        return (the_null_lexenv() if x is _null else
                the(lexenv_t, x or symbol_value(_lexenv_)))

__the_null_lexenv__ = make_null_lexenv(nil)
def the_null_lexenv():
        return __the_null_lexenv__

def make_lexenv_varframe(names, parent = nil, clambda = None, forms = repeat(None), allocate_tns = nil):
        return make_lexenv(parent = parent, clambda = clambda, allocate_tns = allocate_tns,
                           kind_varframe  = { _variable: { variable_binding(sym, _variable, form)
                                                           for sym, form in zip(names, forms) } })

def make_lexenv_funcframe(bindings, parent = nil, clambda = None, allocate_tns = nil):
        return make_lexenv(parent = parent, clambda = clambda, allocate_tns = allocate_tns,
                           kind_funcframe = { function: { function_binding(sym, _function, fn(sym, clam.lambda_list))
                                                          for sym, clam in bindings } })

# Code

string_set("*WALKER-LEXENV*", nil)        ## This is for regular macro expansion.
string_set("*WALKER-CLAMBDA*", nil)       ## Should the original lexenv and clambda be merged into *WALKER-ARGS*?
                                          ## Or should we, still better, add argument passing to the matcher?
string_set("*WALKER-ALLOCATE-TNS*", nil)
string_set("*WALKER-BINDER*", nil)
string_set("*WALKER-BINDER-ARGS*", nil)

define_funcher(_lambda,
                " ", ([(_notlead, " "), (_or, (_satisfies, lambda_word_p),
                                          (_bind, _variable, (_satisfies, namep)),
                                          (_bind, _variable, ((_satisfies, namep), " ", (_bound, (_form,)))))],),
                1, [(_notlead, "\n"), (_bound, (_form,))])

## Unregistered Issue LEXENV-WALKER-NOT-REENTRANT->MACROEXPANSION-DAMAGED-GOODS
class lexenv_walker(metasex_mapper_t):
        class binder():
                def __init__(self, args, description):
                        self.args, self.positionally, self.description = args, dict(), description
                def __repr__(self):
                        return "#<BINDER %s>" % (self.description,)
        def __init__(m):
                metasex_mapper_t.__init__(m)
                m.register_simplex_matcher(_binder,        m.establish_binder)
                m.register_simplex_matcher(_bind,          m.add_binding)
                m.register_simplex_matcher(_bound,         m.setup_lexenv)
                m.register_simplex_matcher(_form,          m.form)
        def form(m, bound, name, form, pat, orifst, ignore_args = None):
                handled, ret = m.process_formpat_arguments(form, pat) if not ignore_args else (None, None)
                if handled:
                        return m.succ(bound, ret)
                ## Hmm.. BINDER patterns..
                peek_pat = form_metasex(form_real(form), kind = symbol_value(_metasex_kind_))
                def continuation(form):
                        return metasex_mapper.form(m, bound, name, form, pat, orifst, ignore_args = ignore_args)
                if consp(peek_pat) and peek_pat[0] is _binder:
                        kind = peek_pat[1][0]
                        return kind.known.binder(form, continuation)
                else:
                        return continuation(form)
        def establish_binder(m, bound, name, exp, pat, orifst):
                # def continuation(exp):
                #         return m.match(bound, name, exp, pat[1], orifst, aux, limit)
                # return pat[0][1][0].known.binder(exp, continuation)
                return m.match(bound, name, exp, pat[1][1][0], orifst, None, -1)
        def add_binding(m, bound, name, exp, pat, orifst):
                ## For the sake of proper pre-lexenv maintenance for the value subform of this binding form,
                ## bust all the bindings accumulated from previous unsuccessful matches:
                binder = the(lexenv_walker.binder, symbol_value(_walker_binder_))
                position = segment_iteration()
                if isinstance(binder.args, int):
                        binder.args = position + 1
                for i, (posn, _, __, ___) in tuple(binder.positionally.items()):
                        if posn >= position:
                                del binder.positionally[i]
                kind, bindingpat = pat[1][0], pat[1][1][0]
                ## Recursively process the binding form for its subforms, having the clean pre-lexenv:
                ret = m.default(exp, bindingpat, name = name, orifst = orifst)
                if ret[2] is not None: ## Propagate match failure.  Can we incrementalise the above bustage here?
                        return ret
                ## Success -- extend the pre-lexenv with the binding.
                name, what = (first(exp), second(exp)) if listp(exp) else (exp, nil)
                # dprintf("==== Found binding for %s", name)
                binder.positionally[position] = (position, kind, name, what)
                return ret
        def setup_lexenv(m, bound, name, exp, pat, orifst):
                def further():
                        return m.default(exp, pat[1][0], name = name, orifst = orifst)
                binder = symbol_value(_walker_binder_)
                if not binder.positionally:
                        return further()
                items = list(sorted(binder.positionally.values()))
                kind = items[0][1]
                names, forms = zip(*((x[2], x[3])
                                     for x in items))
                space, bctor = (("name_varframe",   variable_binding) if kind in (_variable, _symbol_macro) else
                                ("name_funcframe",  function_binding) if kind in (_macro, _function)        else
                                ("name_blockframe", block_binding)    if kind is _block                     else
                                ("name_gotagframe", gotag_binding)    if kind is _gotag                     else
                                error("Invalid binding kind %s in binding of %s.", kind, names))
                whats        = (forms if isinstance(binder.args, int) else
                                binder.args)
                with progv({ _walker_lexenv_: make_lexenv(symbol_value(_walker_lexenv_),
                                                          allocate_tns = symbol_value(_walker_allocate_tns_),
                                                          **{ space: { name: bctor(name, kind, what )
                                                                       for name, what in zip(names, whats) } }) }):
                        return further()

lexenv_walker = lexenv_walker()

def walk_with_lexenv(fn: "Form -> (Form -> ({} Form Bool)) -> ({} Form Bool)",
                      sex, lexenv = nil, allocate_tns = nil, matcher = lexenv_walker) -> "Form":
        with progv({ _walker_lexenv_: coerce_to_lexenv(lexenv),
                     _walker_allocate_tns_: allocate_tns,
                     _metasex_kind_:  "metasex_bind" }):
                return xform_ir(fn, sex, matcher = matcher)

def walker_lexenv():
        return symbol_value(_walker_lexenv_)

# Global scope

class scope(): pass
class variable_scope(scope, collections.UserDict):
        def __hasattr__(self, name): return name in self.data
        def __getattr__(self, name): return self.data[name]
        def __init__(self):
                self.data = dict()
class function_scope(scope, collections.UserDict):
        def __hasattr__(self, name): return name in self.data
        def __getattr__(self, name): return self.data[name]
        def __init__(self):
                self.data = dict()

variable_scope = variable_scope()
function_scope = function_scope()

def find_global_variable(name):     return gethash(name, variable_scope)[0]
def find_global_function(name):     return gethash(name, function_scope)[0]
def  set_global_function(name, x): function_scope[name] = the(function, x)
def  set_global_variable(name, x): variable_scope[name] = the(variable, x)

def compiler_defparameter(name, value):
        x = variable(the(symbol_t, name), _variable)
        variable_scope[name] = x
        if value is not None:
                __global_scope__[name] = value

def compiler_defvar(name, value):
        if not find_global_variable(name):
                compiler_defparameter(name, value)

def compiler_defun(name: symbol_t, lambda_expression: cons_t, check_redefinition = t) -> bool:
        """Manipulate the compiler's idea of a function's definition.
           Return a boolean, which denotes whether the situation is an identity redefinition."""
        check_type(name, (or_t, symbol_t, cons_t))
        oldef = find_global_function(name)
        if oldef and oldef.lambda_expression == lambda_expression:
                return t
        if check_redefinition and isinstance(name, symbol_t):
                if oldef and oldef.kind is _macro: ## XXX: WTF does oldef mean as a function object attribute?
                        warn_incompatible_redefinition(name, "function", "macro")
                elif oldef and oldef.kind is _function:
                        warn_possible_redefinition(oldef.name, name)
        set_global_function(name, function(name, _function, lambda_expression = lambda_expression))
        return nil

def compiler_defmacro(name, lambda_expression, check_redefinition = t):
        "Return a boolean, which denotes whether the situation is an identity redefinition."
        check_type(name, (or_t, symbol_t, list))
        oldef = find_global_function(name)
        ## Unregistered Issue MACRO-REDEFINITION-REPLACED-BY-ONE-WITH-NONEIFIED-GLOBALS
        if oldef and oldef.lambda_expression == lambda_expression:
                return t
        if check_redefinition and isinstance(name, symbol_t):
                if oldef and oldef.kind is _function:
                        warn_incompatible_redefinition(name, "macro", "function")
                elif oldef and oldef.kind is _macro: ## XXX: WTF does oldef mean as a function object attribute?
                        warn_possible_redefinition(oldef.name, name)
        set_global_function(name, function(name, _macro, lambda_expression = lambda_expression))
        return nil

def compiler_defvar_without_actually_defvar(name, value):
        "This is for SETQ-IN-ABSENCE-OF-DEFVAR."
        if value is not None:
                __global_scope__[name] = value

def global_variable_constant_p(name):
        var = find_global_variable(name)
        return var and var.kind is _constant

## Unregistered Issue CONSTANTNESS-PERSISTENCE
def compiler_defconstant(name, value):
        assert(value is not None)
        if global_variable_constant_p(name):
                error("The constant %s is being redefined (from %s to %s).", name, variable_scope[name].value, value)
        var = variable(the(symbol_t, name), _constant, value = value)
        variable_scope[name] = var

def check_no_locally_rebound_constants(locals, use = "local variable"):
        constant_rebound = [ x for x in locals if global_variable_constant_p(x) ]
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
                (isinstance(form, symbol_t) and global_variable_constant_p(form)) or
                (isinstance(form, list) and len(form) is 2 and form[0] is _quote))

# Action: populate global scope with pre-defined functions

def populate_compilation_environment_from_package(package):
        for sym in package.own:
                if sym.function:
                        set_function_definition(globals(), sym,
                                                  lambda_expression = nil, check_redefinition = nil)(sym.function)
                value, presentp = gethash(sym, __global_scope__)
                if presentp:
                        compiler_defvar(sym, value)

populate_compilation_environment_from_package(__cl)

compiler_defconstant(t,   t)
compiler_defconstant(nil, nil)

EmptyDict = dict()
EmptySet = frozenset()

# Compiler globals

intern_and_bind_globals(
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

# Dynamic scope -based Tracking of various IR properties (crude)

## Should, probably, be bound by the compiler itself.

def define_state_tracker(name):
        macro = """
string_set("*COMPILER-%sP*", nil)

def _%sp(): return symbol_value(_compiler_%sp_)

_%s          = defwith("_%s",
                          lambda *_: dynamic_scope_push({ compiler_%sp_: t }),
                          lambda *_: dynamic_scope_pop())
no_%s       = defwith("_no_%s",
                          lambda *_: dynamic_scope_push({ compiler_%sp_: nil }),
                          lambda *_: dynamic_scope_pop())
"""
        exec(macro % ((name.upper(),) + (name,) * 8))

define_state_tracker("linear")

# Functions

string_set("*COMPILER-FN*",     nil)
string_set("*COMPILER-LAMBDA*", nil)

## Critical Issue COALESCE-FNS-WITH-FUNCTION-SCOPE
## Critical Issue FIGURE-OUT-WHAT-IS-COMPILE-TIME-AND-WHAT-IS-LOAD-TIME
fns = make_hash_table()

@defclass
class fn():
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
                check_type(arglist, list_t)
                args_types = [t] * length(arglist)
                if the(symbol_t, name): ## Not anonymous LAMBDA?
                        if name in fns:
                                error("Asked to overwrite FN record %s.", name)
                        if globalp:
                                fns[name] = self
                attrify_args(self, locals(), "name",
                              "arglist", "args_types", "values_types",
                              "effects", "affected")
                self.dependents, self.dependencies = (make_hash_table(default_constructor = set),
                                                      make_hash_table(default_constructor = set))
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

def find_fn(name):
        return fns.get(name, nil)

def depend_on(fn_or_name, reason = t):
        fn = fn_or_name if isinstance(fn_or_name, cold_function_type) else find_fn(fn_or_name)
        fn.add_dependent(reason, symbol_value(_compiler_fn_))

def ir_depending_on_function_properties(function_form, body, *prop_test_pairs):
        ## TODO: we should depend for the de-pessimisation sense likewise.
        ## ..or should we?  I think, at least for rechecking of conditions.. which is only possible
        ## in case of full recompilation..
        ##
        ## ..And we didn't even start to consider dependency loops..
        if symbolp(function_form):
                fn = find_fn(function_form)
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
                                depend_on(fn, prop)
                        return body(fn, *prop_vals)
        return None

# Symbols

string_set("*IN-COMPILATION-UNIT*", nil)

def unit_function(x):
        symbol_value(_unit_functions_).add(x)
        return x
def unit_symbol(x):
        symbol_value(_unit_symbols_).add(x)
        return x
def unit_variable_pyname(x):
        return frost.full_symbol_name_python_name(x)
def unit_function_pyname(x):
        symbol_value(_unit_functions_).add(x)
        return ensure_function_pyname(x)
def unit_symbol_pyname(x):
        symbol_value(_unit_symbols_).add(x)
        return ensure_symbol_pyname(x)

def unit_note_gfun_reference(x):
        symbol_value(_unit_gfuns_).add(x)
def unit_note_gvar_reference(x):
        symbol_value(_unit_gvars_).add(x)

def compilation_unit_symbols():
        """Return a function names and plain symbols, referred by the current compilation unit."""
        return (symbol_value(_unit_functions_),
                symbol_value(_unit_symbols_),
                symbol_value(_unit_gfuns_),
                symbol_value(_unit_gvars_))

def compilation_unit_adjoin_symbols(funs, syms, gfuns, gvars):
        symbol_value(_unit_functions_).update(funs)
        symbol_value(_unit_symbols_).update(syms)
        symbol_value(_unit_gfuns_).update(gfuns)
        symbol_value(_unit_gvars_).update(gvars)

string_set("*COMPILATION-UNIT-ID*", nil)

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
                # warn_not_implemented()
                ...
        succeeded_p = nil
        if symbol_value(_in_compilation_unit_) and not override:
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
                                # dprintf("############################################ Entered %s%s",
                                #               id, (" parent: %s" % parent_id) if parent_id else "")
                                ret = fn()
                                succeeded_p = t
                                return ret
                        finally:
                                summarize_compilation_unit(not succeeded_p)
                                # dprintf("############################################  ..left %s", id)

# Setup machinery

## Unregistered Issue SEPARATE-COMPILATION-IN-FACE-OF-NAME-MAPS

### Global compiler state carry-over, and module state initialisation.
def fop_make_symbol_available(globals, package_name, symbol_name,
                               function_pyname, symbol_pyname,
                               gfunp, gvarp):
        symbol = (intern(symbol_name, find_package(package_name))[0] if package_name is not None else
                  make_symbol(symbol_name))
        if function_pyname is not None:
                symbol.function_pyname = function_pyname
                # dprintf("   c-t %%U-S-G-F-P %s FUN: %s  - %s, %s %s",
                #               "G" if gfunp else "l", symbol_name, function_pyname, symbol.function, symbol.macro_function)
                if gfunp:
                        frost.setf_global((symbol_function(symbol)
                                            if symbol.function or symbol.macro_function else
                                            ## It is a valid situation, when the function is not defined at
                                            ## the beginning of load-time for a given compilation unit.
                                            lambda *_, **__: error("Function not defined: %s.", symbol)),
                                           function_pyname, globals)
        if gvarp:
                if find_global_variable(symbol):
                        value = symbol_value(symbol)
                        assert(value is not None)
                        frost.setf_global(value, unit_variable_pyname(symbol), globals)
                else:
                        ## Unregistered Issue UNDEFINED-GLOBAL-REFERENCE-ERROR-DETECTION
                        pass # This will fail obscurely.
        if symbol_pyname is not None:
                symbol.symbol_pyname = symbol_pyname
                frost.setf_global(symbol, symbol_pyname, globals)

def fop_make_symbols_available(globals, package_names, symbol_names,
                                function_pynames, symbol_pynames,
                                gfunps, gvarps):
        for fop_msa_args in zip(package_names[2:], symbol_names[2:],
                                function_pynames[2:], symbol_pynames[2:],
                                gfunps[2:], gvarps[2:]):
                fop_make_symbol_available(globals, *fop_msa_args)

# Call support

def parse_keyword_args(rest):
        acc = dict()
        for k, v in zip(rest[0::2], rest[1::2]):
                if k not in acc:
                        acc[k] = v
        return acc

_allow_other_keys_ = intern("ALLOW-OTHER-KEYS", __keyword)[0]

def validate_keyword_args(allowed_set, keymap):
        if _allow_other_keys_ in keymap and keymap[_allow_other_keys_] is not nil:
                return
        wrong_keys = keymap.keys() - allowed_set - set([_allow_other_keys_])
        if wrong_keys:
                error("Unknown &KEY arguments: %s", ", ".join(str(x) for x in wrong_keys))

# Macroexpansion

intern_and_bind_symbols("DEFMACRO", "EVAL-WHEN")

ensure_function_pyname(_defmacro) ## This is only needed due to the special definition of DEFMACRO.
@set_macro_definition(globals(), _defmacro, nil)
# ((intern("DEFMACRO")[0], " ", (satisfies, namep), " ", ([(notlead, " "), (satisfies, namep)],),
#   1, [(notlead, "\n"), (bound, form)]))
def DEFMACRO(name, lambda_list, *body):
        l, l_ = list_, list__
        return l(_eval_when, l(_compile_toplevel, _load_toplevel, _execute),
                 ## Unregistered Issue MATCH-FAILURE-POINTS-INTO-THE-FAILED-SEX-AND-PATTERN-NOT-AT
                 # (function, (def_, name, lambda_list) + body),
                 l(_ir_args,
                   l_(_lambda, lambda_list, consify_linear(body)),
                   l("decorators", ir_cl_call("set_macro_definition", ir_apply("globals"),
                                               l(_quote, name),
                                               l(_quote, nil # l_(name, lambda_list, consify_linear(body))
                                                 ))),
                   ["name", name]))

def do_macroexpand_1(form, env = nil, compilerp = nil):
        """This handles:
             - macro expansion, and,
           optionally, if COMPILERP is non-NIL:
             - conversion of implicit funcalls to knowns, and
             - compiler macro expansion"""
        ## Unregistered Issue COMPLIANCE-IMPLICIT-FUNCALL-INTERPRETATION
        # SYMBOL-MACRO-FUNCTION is what forced us to require the package system.
        def find_compiler_macroexpander(form):
                ### XXX: we rely on the FUNCALL form to have been validated! (Joys of metasex, yet?)
                maybe_fnref = form[1][0]
                lookupable = (consp(maybe_fnref) and length(maybe_fnref) == 2 and
                              isinstance(maybe_fnref[1][0], symbol_t))
                global_, lexical_ = ((nil, nil)               if not lookupable              else
                                     (maybe_fnref[1][0], nil) if maybe_fnref[0] is _quote    else
                                     (nil, maybe_fnref[1][0]) if maybe_fnref[0] is _function else
                                      (nil, nil))
                return (compiler_macro_function(global_ or lexical_, check_shadow = lexical_)
                        if global_ or lexical_ else
                        nil)
        def knownifier_and_maybe_compiler_macroexpander(form, known):
                if not known: ## ..then it's a funcall, because all macros
                        xformed = ir_funcall(form[0], *vectorise_linear(form[1]))
                        # dprintf("APPLYIFIED\n%s\n->\n%s", pp_consly(form), pp_consly(xformed))
                        return lambda *_: (find_compiler_macroexpander(xformed) or identity)(xformed)
                else:
                        return (find_compiler_macroexpander(form) if known.name in (_apply, _funcall) else
                                nil)
        expander, args = (((form
                            and isinstance(form[0], symbol_t)
                            and (macro_function(form[0], env)
                                 or (compilerp
                                     and knownifier_and_maybe_compiler_macroexpander(form, find_known(form[0]))))),
                           form[1])                               if consp(form)                else
                          (symbol_macro_expander(form, env), nil) if isinstance(form, symbol_t) else ## Notice, how NIL is not expanded.
                          (nil, nil))
        ret = mxed, xformp = ((form, nil) if not expander else
                              (expander(*vectorise_linear(args)), t))
        if (xformp and symbol_value(_compiler_trace_subexpansion_)):
                dprintf(";;; %s - macroexpanded ............%s   -->\n%s\n",
                        "DO-MACROEXPAND-1", pp_consly(form), pp(mxed))
        return ret

## Unregistered Issue COMPLIANCE-MACROEXPAND-HOOK-MUST-BE-FUNCALL-BUT-IT-IS-NOT-A-FUNCTION
string_set("*MACROEXPAND-HOOK*", lambda f, *args, **keys: f(*args, **keys))
string_set("*ENABLE-MACROEXPAND-HOOK*", t)

def macroexpand_1(form, env = nil, compilerp = nil):
        if symbol_value(_enable_macroexpand_hook_):
                return symbol_value(_macroexpand_hook_)(do_macroexpand_1, form, env, compilerp = compilerp)
        return do_macroexpand_1(form, env, compilerp)

def macroexpand(form, env = nil, compilerp = nil):
        def do_macroexpand(form, expanded):
                expansion, expanded_again = macroexpand_1(form, env, compilerp)
                return (do_macroexpand(expansion, t) if expanded_again else
                        (form, expanded))
        return do_macroexpand(form, nil)

string_set("*MACROEXPANDER-ENV*",       nil)
string_set("*MACROEXPANDER-COMPILERP*", nil)

def macroexpander_xform(form, further):
        expanded_form, expandedp = macroexpand(form, walker_lexenv(), compilerp = symbol_value(_macroexpander_compilerp_))
        ret = _, combined, __ = further(expanded_form)
        # if not (isinstance(form, (symbol_t, str, int)) or
        #         consp(form) and form[0] in (_function, _quote, _ref)):
        #         dprintf("\n%s\n%s\nexpanded ->\n%s\ncombined ->\n%s",
        #                 "MACROEX" if expandedp else "INTACT",
        #                 pp_consly(form),
        #                 pp_consly(expanded_form),
        #                 pp_consly(combined))
        return ret

def macroexpand_all(sex, lexenv = nil):
        return walk_with_lexenv(macroexpander_xform, sex, lexenv)

def compiler_macroexpand_all(form, lexenv = nil):
        with progv({ _macroexpander_compilerp_: t }):
                return macroexpand_all(form, lexenv = lexenv)

# Known IR

class known():
        def rewrite(orig, *_):
                return nil, orig
        def binder(exp, continuation):
                with progv({ _walker_binder_: lexenv_walker.binder(0, "Generic binder.") }):
                        # dprintf("== KNOWN.BINDER: establishing a generic one")
                        return continuation(exp)

def compute_default_metasex(name):
        "Return a default metasex form for an IR with NAME."
        return (name, [" ", (_form,)])

def defknown(metasex_or_class, name = None):
        def do_def(cls, sym, pyname, metasex):
                orig = global_(pyname, globals())
                ## Complete, record the deeds.
                metasex = preprocess_metasex(metasex)
                cls.name           = sym
                cls.metasex        = strip_metasex(metasex, strip_pp = t, strip_bind = t)
                cls.metasex_pp     = strip_metasex(metasex,               strip_bind = t)
                cls.metasex_bind   = strip_metasex(metasex, strip_pp = t)
                sym.known          = cls
                define_global_sym_for_pyname(globals(), pyname, sym)
                return orig # pass through -- let the symbol be bound to the name
        def def_(cls, name = name, metasex = metasex_or_class):
                pyname = cls.__name__
                sym     = intern(frost.python_name_lisp_symbol_name(pyname))[0]
                name    = defaulted(name, sym, symbol_t)
                metasex = defaulted(metasex, compute_default_metasex(name))
                return do_def(cls, name, pyname, metasex)
        return (def_(metasex_or_class, metasex = None) if isinstance(metasex_or_class, type)    else
                def_                                   if isinstance(metasex_or_class, tuple)   else
                error("In DEFKNOWN: argument must be either a class or a metasex tuple, was: %s.",
                      repr(metasex_or_class)))

def find_known(x):
        return the(symbol_t, x).known

def form_known(form):
        ## METASEX-MATCHER guts it, due to case analysis
        complex_form_p = consp(form) and isinstance(form[0], symbol_t)
        return complex_form_p and find_known(form[0])

def rewrite_1_keys(form, keys) -> "(Bool ({} Form Bool))":
        ## For IR-ARGS.REWRITE
        if atom(form):
                return nil, form
        known = find_known(form[0])
        if not known:
                error("Unknown form encountered at rewrite stage: %s, form[0]: %s/%x", pp_consly(form), form[0], id(form[0]))
        rewrite_method = known.rewrite
        validate_function_keys(form[0], rewrite_method, keys)
        ret = _, r = rewrite_method(form, *vectorise_linear(form[1]), **keys)
        if (symbol_value(_compiler_trace_subrewriting_)
            and form != r):
                dprintf(";;; %s - rewrote ............\n%s   -->\n%s\n",
                        "REWRITE-1-KEYS", pp(form), pp(r))
        return ret

def rewrite_1(form) -> "(Bool ({} Form Bool))":
        "All non-atom forms are interpreted as knowns at this point."
        # dprintf("%s(%s) calling %s.REWRITE(%s, ...)", caller_name(3), pp_consly(caller_args(3)), form[0], pp_consly(form))
        ## 0. Atoms are not rewritten.
        if atom(form):
                return nil, form
        ## 1. Funcallify LAMBDA calls .. and recursively rewrite all contents.
        if consp(form[0]) and form[0][0] is _lambda:
                ## Applyification of ((LAMBDA ...) ...) forms -- the generic way only works on symbol calls.
                return t, cons(_funcall, form)
        ## 2. Otherwise, find the corresponding known.
        known = find_known(form[0])
        if not known:
                error("Unknown form encountered at rewrite stage: %s, form[0]: %s/%x", pp_consly(form), form[0], id(form[0]))
        ## 3. Check known parameter/argument count correspondence.
        rewrite_method = known.rewrite
        argspec = inspect.getfullargspec(rewrite_method)
        args = vectorise_linear(form[1])
        if not argspec.varargs and len(args) != len(argspec.args) - 1:
                error("Invalid form: %s -- %d arguments provided, but %s.REWRITE expects only %d.",
                      pp_consly(form), len(args), known.__name__.upper(), known.rewrite.__code__.co_argcount - 2)
        ## 4. Invoke the rewrite method.
        ret = _, r = rewrite_method(form, *args)
        if (symbol_value(_compiler_trace_subrewriting_)
            and form != r):
                dprintf(";;; %s - rewrote ............\n%s   -->\n%s\n",
                        "REWRITE-1", pp(form), pp(r))
        return ret

def rewrite(form) -> "({} Form Bool)":
        ## Unregistered issue CONTINUATION-PASSING-MULTIPLIES-STACK-PRESSURE
        def do_rewrite(form):
                xformed_again, reform = rewrite_1(form)
                return (do_rewrite(reform) if xformed_again else
                        reform)
        ret = do_rewrite(form)
        return ret

class rewriter_t(type(lexenv_walker)):
        pass

rewriter = rewriter_t()

def rewriter_xform(form, further: "Form -> ({} Form Bool)") -> "({} Form Bool)":
        ## Optimise: further(form) if atom(form) else rewrite(further, form)
        ## Absolutely want to see it benchmarked : -)
        return further(rewrite(form))

def rewrite_all(sex, lexenv = nil):
        return walk_with_lexenv(rewriter_xform, sex, lexenv = lexenv, matcher = rewriter)

def ir_nvalues(form):
        return (lambda known: (known.nvalues(*vectorise_linear(form[1]))            if known else
                               None))                       (form_known(form))
def ir_nth_value(n, form):
        return (lambda known: (known.nth_value(n, form, *vectorise_linear(form[1])) if known else
                               list_(nth_value, n, form)))  (form_known(form))
def ir_effects(form):
        return (lambda known: (known.effects(*vectorise_linear(form[1]))            if known else
                               t))                          (form_known(form))
def ir_affected(form):
        return (lambda known: (known.affected(*vectorise_linear(form[1]))           if known else
                               t))                          (form_known(form))
def ir_function_form_nvalues(func):
        return defaulted(
                ir_depending_on_function_properties(func,
                                                     lambda fn, types: len(types),
                                                     ("value_types", lambda x: x is not t)),
                t)

def ir_function_form_nth_value_form(n, func, orig_form):
        return defaulted(
                ir_depending_on_function_properties(
                        func,
                        lambda fn, types, effs: (nil if n >= len(types) else
                                                 ir_make_constant(types[n])),
                        ("value_types", lambda x: (x is not t and (n >= len(x) or
                                                                   ## XXX: This is sweet, no?
                                                                   eql_type_specifier_p(x[n])))),
                        ## Let's not get lulled, though -- this is nothing more than a
                        ## fun optimisation, and not a substitute for proper constant propagation.
                        ("effects",     lambda x: not x)),
                list_(nth_value, n, orig_form))

def ir_nth_valueify_last_subform(n, form):
        return append(butlast(form), list_(list_(nth_value, n, lastcar(form))))

# Primitive IR

with disabled_condition_system():
        import primitives as p

def variable_tn(sym):         return p.name(unit_variable_pyname(sym))
def function_tn(sym):         return p.name(unit_function_pyname(sym))
def symbol_tn(sym):           return p.name(unit_symbol_pyname(sym))
def variable_tn_no_unit(sym): return p.name(ensure_variable_pyname(sym))
def function_tn_no_unit(sym): return p.name(ensure_function_pyname(sym))

def gensym_tn(x = "G"):
        sym = gensym(x)
        sym.tn = variable_tn(sym)
        return sym

def make_keyword_tn(name):
        kw = make_keyword(name)
        kw.tn = symbol_tn(kw)
        return kw

__primitiviser_map__ = { str:        (nil, p.string),
                         int:        (nil, p.integer),
                         float:      (nil, p.float_num),
                         ## Note: this relies on the corresponding name to be made available by some means.
                         symbol_t:   (nil, lambda x: p.symbol(unit_symbol_pyname(x))),
                         bool:       (nil, lambda x: p.name("True" if x else "False")),
                         NoneType:   (nil, lambda x: p.name("None")),
                         list:       (t,   lambda xs: p.pylist(*xs)),
                         tuple:      (t,   lambda xs: p.pytuple(*xs)),
                         }

def primitivisable_p(x):
        type = type_of(x)
        type_recipe, _ = gethash(type, __primitiviser_map__)
        if not type_recipe:
                return nil
        recursep, _ = type_recipe
        if not recursep:
                return t
        return all(primitivisable_p(x) for x in x)

def try_primitivise_constant(x):
        "It's more efficient to try doing it, than to do a complete check and then to 'try' again."
        def try_primitivise_list(xs):
                ret = []
                for x in xs:
                        res, successp = try_primitivise_constant(x)
                        if not successp:
                                return None, None
                        ret.append(res)
                return ret, t
        (rec, primitiviser), primitivisablep = gethash(type_of(x), __primitiviser_map__,
                                                       ((nil, nil), nil))
        if not primitivisablep:
                return None, None
        if rec:
                prim, successp = try_primitivise_list(x)
                return (primitiviser(prim), t) if successp else (None, None)
        return primitiviser(prim if rec else x), t

def primitivise_constant(x):
        prim, successp = try_primitivise_constant(x)
        return (prim if successp else
                error("Cannot primitivise value %s (of type %s).  Is it a literal?",
                      pp_consly(x), type(x).__name__))

intern_and_bind_symbols("APPLY", "FUNCALL")

def ir_funcall(func, *args):
        ## Has the virtue of only being available pre-rewrite.
        l, l_ = list_, list__
        return l(_funcall, l(_function, (l(_quote, l(func)) if isinstance(func, str) else
                                         func)),
                 *args)

def ir_apply(func, *args):
        ## Has the virtue of being available even post-rewrite.
        l, l_ = list_, list__
        return l(_apply, l(_function, (l(_quote, l(func)) if isinstance(func, str) else
                                         func)),
                 *(args + (l(_quote, nil),)))

def ir_cl_call(name, *args):
        return ir_apply(list_(_quote, list_("cl", name)), *args)

# Metasex tests

def matcher_result_printer(x):
        return ((("%s\n%s\n%s" % (x[0], pp_consly_pp_str(x[1]), pp_consly(x[2])))
                 if len(x) is 3 else
                 ("%s\n%s" % (pp_consly(x[0]), pp_consly(x[1]))))
                                                                          if isinstance(x, tuple) else
                matcher_pp(x)                                            if isinstance(x, dict)  else
                matcher_pp(x)                                            if consp(x)             else
                str(x))

intern_and_bind_symbols("CAR", "CDR", "LET", "&BODY")
def run_tests_metasex():
        printer = matcher_result_printer
        def do_run_test(input, matcher = metasex_pp):
                with matcher_pp_stack():
                        return match_sex(input[0], preprocess_metasex(input[1]), matcher = matcher)
        def just_match(input):   return do_run_test(input, matcher = metasex)
        def pp(input):           return do_run_test(input)
        def mal_pp(input):       return do_run_test(input)
        def empty(input):        return do_run_test(input)
        def empty_cross(input):  return do_run_test(input)
        def alternates(input):   return do_run_test(input)
        def simplex(input):      return do_run_test(input)
        def mid_complex(input):  return do_run_test(input)
        def simple_maybe(input): return do_run_test(input)
        def proper_fail(input):  return do_run_test(input, matcher = metasex)

        ###
        assert runtest(just_match,
                        (consify_star(_let, ((_car, ()),
                                             (_cdr, (_car,))),
                                        _body),
                         (_let, ({"bindings":[((_satisfies, namep), (_form,))]},),
                           {"body":[(_form,)]})),
                        ({ 'bindings': consify_star((_car, ()),
                                                     (_cdr, (_car,))),
                           'body':     consify_star(_body) },
                         t,
                         None),
                        printer = printer)

        assert runtest(pp,
                        (consify_star(_let, ((_car, ()),
                                             (_cdr, (_car,))),
                                        _body),
                         (_let, " ", ({"bindings":[(_notlead, "\n"), ((_satisfies, namep), " ", (_form,))]},),
                            1, {"body":[(_notlead, "\n"), (_form,)]})),
                        ({ 'bindings': consify_star((_car, ()),
                                                    (_cdr, (_car,))),
                           'body':     consify_star(_body,)},
                         """(LET ((CAR NIL)
      (CDR (CAR)))
  &BODY)""",
                         None),
                        printer = printer)

        assert runtest(empty,
                        (nil,
                         {"whole":nil}),
                        ({ 'whole': nil },
                         "NIL",
                         None),
                        printer = printer)

        assert runtest(empty_cross,
                        (nil,
                         ({"a":[(_satisfies, namep)]}, {"b":[((_satisfies, namep),)]},)),
                        ({ 'a': nil, 'b': nil },
                         "NIL",
                         None),
                        printer = printer)

        assert runtest(alternates,
                        (list_(1, "a"),
                         {"whole":([(_or, (_typep, int),
                                          (_typep, str))],)}),
                        ({ 'whole': list_(1, "a") },
                         '(1"a")',
                         None),
                        printer = printer)

        intern_and_bind_symbols("PI")
        assert runtest(simplex,
                        (list_(nil, _pi),
                         ({'head':[nil]}, {'tail':(_satisfies, namep)})),
                        ({ 'head': list_(nil),
                           'tail': _pi },
                         "(NILPI)",
                         None),
                        printer = printer)

        assert runtest(mid_complex,
                        (list_(_pi, list_(_pi), list_(_pi), list_(_pi, _pi), list_(_pi, _pi, _pi),
                                     list_(_pi), list_(_pi), list_(_pi), _pi, _pi, _pi),
                         ({"headname":(_satisfies, namep)},
                          {"headtupname":((_satisfies, namep),)},
                                   {"varitupseq":[((_satisfies, namep), [(_satisfies, namep)])]},
                                                            {"fix1tupseq":[((_satisfies, namep),)]},
                                                                                   {"nameseq":[(_satisfies, namep)]},
                                                                                        {"tailname":(_satisfies, namep)})),
                        ({ 'headname': _pi,
                          'headtupname': list_(_pi),
                          'varitupseq': consify_star((_pi,), (_pi, _pi), (_pi, _pi, _pi)),
                          'fix1tupseq': consify_star((_pi,), (_pi,), (_pi,)),
                          'nameseq': list_(_pi, _pi),
                          'tailname': _pi },
                         "(PI(PI)(PI)(PIPI)(PIPIPI)(PI)(PI)(PI)PIPIPI)",
                         None),
                        # "(_PI (_PI) (_PI) (_PI PI) (_PI PI _PI) (_PI) (_PI) (_PI) _PI PI _PI)"
                        printer = printer)

        assert runtest(simple_maybe,
                        (list_(_pi, _car, _cdr),
                         ({"_pi":(_maybe, (_satisfies, namep))}, {"car":(_satisfies, namep)}, (_maybe, {"cdr":(_satisfies, namep)}))),
                        ({ '_pi': list_(_pi), 'car': _car, 'cdr': _cdr, },
                         "(PICARCDR)",
                         None),
                        printer = printer)

        # global __enable_matcher_tracing__
        # __enable_matcher_tracing__ = True
        assert runtest(proper_fail,
                        (list_(cons('name', 666)),
                         ([((_typep, str), (_typep, t))],)),
                        ({},
                         666,
                         list_(list_(_typep, t))),
                        printer = printer)

if getenv("CL_RUN_TESTS") != "nil" and getenv("CL_TEST_METASEX") != "nil":
        run_tests_metasex()

# Form-based IR toolkit

def handle_linear_body(xs):
        return (nil   if not xs       else
                xs[0] if len(xs) is 1 else
                list_(_progn, *xs))

def rewrite_linear_body(xs):
        return ((nil, list_(_progn, *xs)) if len(xs) > 1 else
                (t, handle_linear_body(xs)))

def handle_constant_linear_body(xs):
        return (nil    if not xs       else
                xs[-1])

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
### CATCH                -> =(APPLY,QUOTE)                                                      ## Via ir_cl_call()
### THROW                -> =(APPLY,QUOTE)                                                      ## Via ir_cl_call()
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
### NTH-VALUE            -> =(APPLY,QUOTE)                                                      ## Via ir_cl_call()
### DEF                  -> BLOCK,QUOTE
### LAMBDA               -> PROGN                      |              EXPR-BODY/DEFAULTS-NO-OPTIONAL-NO-KEYS
###                         PROGN                      |              EXPR-BODY/DEFAULTS-EARLY-EVALUATED-OPTIONAL-OR-KEYS
###                         =(LAMBDA,LET,IF,APPLY,REF) |              EXPR-BODY/DEFAULTS-REWIND-DELAYED-DEFAULT-VALUES
###                         =(FLET,FUNCTION)                          !!! NONEXPR-PROGN-DEF-FUNCTION
### APPLY                -> ∅                     |                   EXPR-ARGS
###                         =(LET,APPLY)          |                   NONEXPR-REWIND-AS-LET-APPLY
###                         =(LET,FUNCTION,APPLY)                     NONEXPR-REWIND-AS-LET-FUNCTION-APPLY

# Raw python references

def pyref_p(x):
        return (consp(x) and x[0] is _quote and consp(x[1])
                and consp(x[1][0]) and isinstance(x[1][0][0], str))

def primitivise_pyref(x):
        return p.prim_attr_chain([ p.name(x[1][0][0]) ]
                                 + xmap_to_vector(p.string, x[1][0][1]))

# IR argument passing

@defknown((_ir_args, "\n", (_form,), ["\n", (_cons, (_typep, str), (_form, (_for_not_matchers_xform, identity, metasex_pp)))],))
class ir_args(known):
        def rewrite(orig, form, *args):
                ## Effect of IR-ARGS:
                ## 1. Process with inner rewrite, parametrised by IR-ARGS', well, args.
                ## 2. Re-wrap the result with the same IR-ARGS.
                if form[0] is _ir_args:
                        error("Unlikely-to-be-valid IR-ARGS: %s  added to  %s.", pp_consly(consify_linear(args)), pp_consly(form))
                def do_rewrite(form, keys = {}):
                        # dprintf("IR-ARGS.DO-REWRITE:   %s", pp_consly(form))
                        xformed_again, reform = rewrite_1_keys(form, keys)
                        return (do_rewrite(reform) if xformed_again else
                                reform)
                rewritten = do_rewrite(form, dict(args))
                argless_form, subargs_retainer = ((rewritten,       identity) if atom(rewritten) or rewritten[0] is not _ir_args else
                                                  (rewritten[1][0], lambda x: list__(_ir_args, x, rewritten[1][1])))
                rewrapped = subargs_retainer(argless_form)
                # dprintf("IR-ARGS.REWRITE: ---------------\n%s\n%s\n",
                #         pp(orig), pp(rewrapped))
                return nil, rewrapped
                # r = do_rewrite(form, dict(args))[1]
                # output = list_(_ir_args, r, *args) if r != form else orig
                # dprintf("IR-ARGS.REWRITE: ---------------\n%s\n%s\n%s\n%s\n",
                #         pp_consly(orig), pp_consly(form), pp_consly(r), pp_consly(output))
                # return nil, (b, output, f)
                ## Proceed: ignore CONT and custom-trail?
                # return rewrite_method(ir_args_cont, form, *vectorise_linear(form[1]), **{ k: v for k, v in args })
        def lower(*_):                     error("Invariant failed: IR-ARGS is not meant to be lowered.")
        def effects(*ir,  **args):         return ir_effects(ir)
        def affected(*ir, **args):         return ir_affected(ir)

def destructure_possible_ir_args(x):
        "Maybe extract IR-ARGS' parameters, if X is indeed an IR-ARGS node, returning them as third element."
        return ((t,   x[1][0], x[1][1]) if consp(x) and x[0] is _ir_args else
                (nil, x,       nil))

def ir(*ir, **keys):
        "A syntactic sugar for convenient IR-ARGS specification."
        known = find_known(the(symbol_t, ir[0]))
        validate_function_keys("IR %s lower method" % known.name, known.lower, keys)
        return (list__(_ir_args, consify_linear(ir), consify_linear([ [k, v] for k, v in keys.items() ])) if keys else
                consify_linear(ir))

# FUNCALL

@defknown
class funcall(known):
        def rewrite(_, func, *args):
                return t, list__(_apply, func, append(consify_linear(args), list_(list_(_quote, nil))))

# LET*

#       LET* is a funky kind of a rewrite, as its intricate lexenv-estry affects macroexpansion non-trivially,
#       and so, a simple fire-and-forget BINDS method will not work at all.
#       It could have been avoided altogether, if we'd committed a blasphemy of rewriting it into oblivion
#       before MACROEXPAND-1 could have seen it.  But CL exposes macroexpansion, and users will not be amused :-)

_let_ = intern("LET*")[0]

@defknown((_binder, _let_,
           (_let_, " ",  ([(_notlead, "\n"), (_bind, _variable, (_or, (_satisfies, namep), ((_satisfies, namep), " ", (_bound, (_form,)))))],),
            1, [(_notlead, "\n"), (_bound, (_form,))])),
          name = _let_)
class let_(known):
        def rewrite(_, bindings, *body):
                ## I quietly hope that there is no need to handle lexenvs here..
                if not bindings:
                        return t, handle_linear_body(body)
                if not bindings[1]:
                        return t, list_(_let, bindings, *body)
                l = list_
                ## Unregistered Issue LET*-REWRITE-TIME-LEXENV-WOBBLINESS
                return t, l(_let, l(bindings[0]),
                            l(_let_, bindings[1],
                              *body))

# FLET

_flet = intern("FLET")[0]

@defknown((_binder, _flet,
           (_flet, " ", ([(_notlead, "\n"), (_bind, _function,
                                             (_binder, _lambda,
                                              ((_satisfies, namep), (_funcher, _lambda))))],),
            1, [(_notlead, "\n"), (_bound, (_form,))])),
          name = _flet)
class flet(known):
        def binder_args(bindings, *body):
                return xmap_to_vector(lambda b: fn(b[0], b[1][0]), bindings)
        def rewrite(_, bindings, *body):
                def fail(x): error("Bad FLET form: %s", pp_consly(x))
                l = list_
                not (listp(bindings)
                     and all((length(x) >= 2 and
                              listp(second(x)))
                             for x in bindings)) and fail(form)
                return t, (handle_constant_linear_body(body) if all(constantp(x) for x in body) else
                           ## This weak attempt above screams for proper liveness analysis.
                           nil                                if not (bindings or body)          else
                           handle_linear_body(body)          if not bindings                    else
                           l(_funcall,
                                 (lambda rest, tns:
                                          l(_labels, xmap_to_conses(lambda btn:
                                                                            cons(btn[0], btn[1]),
                                                                    zip(tns, bindings)),
                                            l(_labels, xmap_to_conses(lambda btn:
                                                                              l(btn[1][0], l(_rest, rest),
                                                                                l(_apply, l(f_unction, btn[0]), rest)),
                                                                      zip(tns, bindings)),
                                              *body)))
                             (gensym("REST-"),
                              (gensym("FN-" + symbol_name(x[0]) + "-")
                               for x in vectorise_linear(bindings)))))

# LABELS

_labels = intern("LABELS")[0]

@defknown((_binder, _labels,
           (_labels, " ", ([(_notlead, "\n"), (_binder, _lambda,
                                               ((_satisfies, namep), (_funcher, _lambda)))],),
            1, [(_notlead, "\n"), (_form,)])),
          name = _labels)
class labels(known):
        def binder(exp, further):
                def fail(x): error("Bad LABELS form: %s", pp_consly(x))
                form = form_real(exp) ## Filter out any potential IR-ARGS
                length(form) < 2 and fail(form)
                bindings = vectorise_linear(form[1][0])
                not (listp(bindings)
                     and all((length(x) >= 2 and
                              listp(second(x)))
                             for x in bindings)) and fail(form)
                env = symbol_value(_walker_lexenv_)
                ## No need to bind *WALKER-BINDER*, since we contribute to %BIND metasex expressions.
                with progv({ _walker_lexenv_:
                              make_lexenv(env, allocate_tns = symbol_value(_walker_allocate_tns_),
                                          name_funcframe = { name: function_binding(name, _function, fn(name, lam))
                                                             for name, (lam, _) in bindings }),
                             _walker_binder_args_: nil } if bindings else
                           { _walker_binder_args_: nil }):
                        return further(exp)
        def rewrite(_, bindings, *body):
                l = list_
                return t, (handle_constant_linear_body(body) if all(constantp(x) for x in body) else
                           ## This weak attempt above screams for proper liveness analysis.
                           nil                               if not (bindings or body)          else
                           handle_linear_body(body)          if not bindings                    else
                           l(_funcall, ir(_lambda, nil,
                                          ## We rely on that PRIMITIVE layer will allocate a
                                          ## separate function for statements, thereby not wronging the namespace.
                                          *(xmap_to_vector(lambda binding:
                                                                   ir(_lambda, *vectorise_linear(binding[1]),
                                                                      name = binding[0]),
                                                           bindings)
                                            + list(body)))))

# MACROLET

## Unregistered Issue EXTENDED-LAMBDA-LIST-DESTRUCTURING-WRECKS-ALL
_macrolet = intern("MACROLET")[0]

@defknown((_binder, _macrolet,
           (_macrolet, " ", ([(_notlead, "\n"),
                              (_bind, _macro,
                               ((_satisfies, namep), " ",
                                ([(_notlead, " "),
                                  (_or, (_satisfies, lambda_word_p),
                                   (_bind, _variable, (_satisfies, namep)),
                                   (_bind, _variable, ((_satisfies, namep), (_bound, (_form,)))),
                                   (_form,))],),
                                1, [(_notlead, "\n"), (_form,)]))],),
            1, [(_notlead, "\n"), (_bound, (_form,))])),
          name = _macrolet)
class macrolet(known):
        def rewrite(_, bindings, *body):
                return t, handle_linear_body(body)

# SYMBOL-MACROLET

_symbol_macrolet = intern("SYMBOL-MACROLET")[0]

@defknown((_binder, _symbol_macrolet,
           (_symbol_macrolet, " ", ([(_notlead, "\n"), (_bind, _symbol_macro, ((_satisfies, namep), " ", (_form,)))],),
            1, [(_notlead, "\n"), (_bound, (_form,))])),
          name = _symbol_macrolet)
class symbol_macrolet(known):
        def rewrite(_, bindings, *body):
                return t, handle_linear_body(body)

# BLOCK

#       Instead of performing an additional mapping operation, which is quadratic,
#       we could rely on the RETURN-FROM marking its presence.

_block = intern("BLOCK")[0]

@defknown((_binder, _block,
           (_block, " ", (_bind, _block, (_satisfies, namep)),
            [1, (_bound, (_form,))],)),
          name = _block)
class block(known):
        def rewrite(orig, name, *body):
                nonce = gensym("BLOCK-" + symbol_name(name) + "-")
                catch_target = list__(_catch, list_(_quote, nonce), consify_linear(body))
                has_return_from = nil
                def update_has_return_from(sex, further):
                        nonlocal has_return_from
                        if consp(sex) and sex[0] is _return_from:
                                has_return_from = t
                        return further(sex)
                map_sex(update_has_return_from, catch_target)
                ## Unregistered Issue CATCH-22-WHILE-DOING-CONTENT-DEPENDENT-REWRITING
                with progv({ _walker_lexenv_: make_lexenv(symbol_value(_walker_lexenv_),
                                                          name_blockframe = { name: block_binding(name, _block, None) }) }):
                        return t, (catch_target if has_return_from else handle_linear_body(body))

# RETURN-FROM

_return_from = intern("RETURN-FROM")[0]

@defknown((_return_from, " ", (_satisfies, namep), (_maybe, " ", (_form,))))
class return_from(known):
        def rewrite(_, name, *maybe_form):
                binding, _ = symbol_value(_walker_lexenv_).lookup_block(the(symbol_t, name))
                if not binding:
                        simple_program_error("return for unknown block: %s", name)
                return t, list_(_throw, list_(_quote, binding.value), nil if not maybe_form else maybe_form[0])

# TAGBODY

#         Implementation strategy:

#         - Henry Baker's '92 "TAGBODY/GO" EMULATED BY "CATCH/THROW"
#         - bytecode patching, for function-internal jumps
#         - THROW, plus bytecode patching, for jumps outward of a lexically contained function
#           definition

intern_and_bind_symbols("%NXT-LABEL", "TAGBODY")

## Unregistered Issue COMPLIANCE-TAGBODY-TAGS-EXEMPT-FROM-MACROEXPANSION
@defknown(## No need for bindings and bound markers -- all done in the BINDER method.
          (_binder, _tagbody,
           (_tagbody, ["\n", (_form,)])),
          name = _tagbody)
class tagbody(known):
        def binder(exp, further):
                form          = form_real(exp)
                binder        = lexenv_walker.binder(0, "Tagbody.")
                binder.tags   = [ gensym("INIT-TAG-")
                                  ] + [ x for x in vectorise_linear(form[1])
                                        if isinstance(x, symbol_t) ]
                binder.fnames = [ gensym("TAG-%s-" % symbol_name(tag)) for tag in binder.tags ]
                ## No need to bind *WALKER-BINDER*, since we contribute to %BIND metasex expressions.
                with progv(dict([ (_walker_binder_,      binder),
                                  (_walker_binder_args_, nil)] +
                                ([(_walker_lexenv_,
                                   make_lexenv(symbol_value(_walker_lexenv_),
                                               allocate_tns = symbol_value(_walker_allocate_tns_),
                                               name_gotagframe = { name: gotag_binding(name, _variable, fname)
                                                                   for name, fname in zip(binder.tags,
                                                                                          binder.fnames) }))]
                                 if binder.tags else []))):
                        return further(exp)
        def rewrite(orig, *tags_and_forms):
                binder       = symbol_value(_walker_binder_)
                init_tag     = binder.tags[0]
                (go_tag,
                 return_tag) = (gensym(x + "-TAG-") for x in ["GO", "RETURN"])
                body         = cons(init_tag, consify_linear(tags_and_forms))
                fun_names = dict(zip(binder.tags, binder.fnames))
                nxt_label = gensym("NXT-")
                l, l_ = list_, list__
                def lam_(seq):
                        label, body = seq[0], seq[1]
                        if not atom(label):
                                return nil
                        nextl = do_find_if(atom, body, dict())
                        nlposn = do_position_if(atom, body, dict())
                        return l(l_(fun_names[label], nil,
                                    nconc(subseq(body, 0, nlposn),
                                          l(ir_apply(fun_names[nextl]) if nlposn else
                                            l(_throw, return_tag, nil)))))
                funs         = mapcon(lam_, body)
                # (mapcon #'(lambda (seq &aux (label (car seq) (s (cdr seq))))
                #             (when (atom label)                                   
                #               (let ((p (position-if #'atom s)))                  
                #                 `((,(label-to-functionname label) ()             
                #                      ,@(subseq s 0 (or p (length s)))            
                #                      ,(if p `(,(label-to-functionname (elt s p)))
                #                             `(throw ,return-tag nil)))))))
                #         `(,init-tag ,@body))
                form = l(_let, l(l(go_tag, l(_apply, l(_function, _list), l(_quote, nil), l(_quote, nil)))),
                         l(_let, l_(l(return_tag, l(_apply, l(_function, _list), l(_quote, nil), l(_quote, nil))),
                                   consify_linear(l(name, go_tag) for name in fun_names.values())),
                           l(_catch, return_tag,
                             l(_labels, funs,
                               l(_let, l(l(_nxt_label, l(_function, funs[0][0]))),
                                 l(_protoloop,
                                   l(_setq, nxt_label,
                                           l(_catch, go_tag, l(_apply, nxt_label, l(_quote, nil))))))))))
                return t, form

# GO

intern_and_bind_symbols("GO")

@defknown((_go, " ", (_typep, symbol_t)))
class go(known):
        def rewrite(_, name):
                binding, lexenv = symbol_value(_walker_lexenv_).lookup_gotag(the(symbol_t, name))
                if not binding:
                        simple_program_error("attempt to GO to nonexistent tag: %s", name)
                return t, list_(_throw, binding.value, list_(_function, binding.value))

# EVAL-WHEN

intern_and_bind_symbols("PROGN")

## Unregistered Issue EVAL-WHEN-LACKING-SPACE-BETWEEN-KEYWORDS-WHEN-PRINTED
@defknown((intern("EVAL-WHEN")[0], " ", ([(_notlead, " "),
                                         (_or, _compile_toplevel, _load_toplevel, _execute,
                                               _compile, _load, _eval)],),
           1, [(_notlead, "\n"), (_form,)]))
class eval_when(known):
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
        def rewrite(_, when, *body):
                ctop, ltop, exec = parse_eval_when_situations(when)
                ## This handles EVAL-WHEN in non-top-level forms. (EVAL-WHENs in top
                ## level forms are picked off and handled by PROCESS-TOPLEVEL-FORM,
                ## so that they're never seen at this level.)
                compiler_trace_known_choice(_eval_when, when, "EXECUTE" if exec else "NO-EXECUTE")
                return t, (cons(_progn, consify_linear(body)) if exec else
                           nil)

# SETQ

_setq = intern("SETQ")[0]

@defknown((_setq, [" ", (_or, (_satisfies, namep), (_satisfies, pyref_p)),
                   " ", (_form,)]))
class setq(known):
        def rewrite(orig, *args):
                ## Actually a normalisation.. or actually, the hell knows what it is..
                len(args) % 2 and \
                    error("SETQ accepts an even amount of arguments, got: %s", pp_consly(consify_linear(args)))
                not all(pyref_p(x) or isinstance(x, symbol_t)
                        for x in args[::2]) and \
                    error("SETQ arguments at even positions must be symbols, got: %s", pp_consly(consify_linear(args)))
                return ((nil, orig) if len(args) == 2 else
                        (t,   handle_linear_body(list(map(lambda name, value: list_(_setq, name, value),
                                                          args[::2], args[1::2])))))
        def nvalues(_, __):                                               return 1
        def nth_value(n, orig, _, value):                                 return orig if n is 0 else list_(_progn, orig, nil)
        ## Unregistered Issue COMPLIANCE-ISSUE-SETQ-BINDING
        ## Unregistered Issue COMPLIANCE-SETQ-MULTIPLE-ASSIGNMENTS-UNSUPPORTED
        def lower(name, value):
                if pyref_p(name):
                        return p.assign(primitivise_pyref(name), primitivise(value))
                cur_lexenv = symbol_value(_lexenv_)
                lexical_binding, tgt_lexenv = cur_lexenv.lookup_var(the(symbol_t, name))
                if not lexical_binding or lexical_binding.kind is _special:
                        compiler_trace_known_choice(_setq, name, "GLOBAL")
                        gvar = find_global_variable(name)
                        if gvar and gvar.kind is _constant:
                                simple_program_error("%s is a constant and thus can't be set.", name)
                        if not gvar and not lexical_binding: # Must be a special, don't complain.
                                simple_style_warning("undefined variable: %s", name)
                                compiler_defvar_without_actually_defvar(name, value)
                        return p.special_setq(p.name(unit_symbol_pyname(name)), primitivise(value))
                compiler_trace_known_choice(_setq, name, "LEXICAL")
                if cur_lexenv.clambda is not tgt_lexenv.clambda:
                        cur_lexenv.clambda.nonlocal_setqs.add(name)
                return p.assign(lexical_binding.tn, primitivise(value))
        def effects(name, value):         return t
        def affected(name, value):        return ir_affected(value)

# PROGN

@defknown((_progn,
           1, [(_notlead, "\n"), (_form,)]))
class progn(known):
        def rewrite(_, *body):
                rewrote, form = rewrite_linear_body(body)
                return rewrote, form
        def nvalues(*body):            return 1   if not body else ir_nvalues(body[-1])
        def nth_value(n, orig, *body): return nil if not body else ir_nth_valueify_last_subform(n, orig)
        def lower(*body):
                return (p.progn(*(primitivise(x) for x in body)) if body else
                        primitivise(nil))
        def effects(*body):            return any(ir_effects(f) for f in body)
        def affected(*body):           return any(ir_affected(f) for f in body)

# IF

_if = intern("IF")[0]

@defknown((_if, " ", (_form,),
           3, (_form,),
          (_maybe, "\n", (_form,))))
class if_(known):
        def rewrite(orig, test, consequent, *maybe_ante):
                return not maybe_ante, (orig if maybe_ante else
                                        list_(_if, test, consequent, nil))
        def nvalues(test, consequent, antecedent):
                nconseq, nante = ir_nvalues(consequent), ir_nvalues(antecedent)
                return (nconseq             if nconseq == nante                      else
                        max(nconseq, nante) if integerp(nconseq) and integerp(nante) else
                        t)
        def nth_value(n, orig, test, consequent, antecedent):
                vconseq, vante = ir_nth_value(consequent), ir_nth_value(antecedent)
                ## Warning: this something, that goes on here, is quite interesting!
                ## Thinking pause: WHY DO WE DO THIS ..and.. WHAT EXACTLY DO WE DO HERE?
                ##  - what: lowering valueness?  ..a good initial approach to understanding..
                return (list_(_nth_value, n, orig) if ir_effects(consequent) or ir_effects(antecedent) else
                        vconseq                    if (vconseq == vante) and not ir_effects(test)       else
                        list_(_if, test, vconseq, vante))
        def lower(test, consequent, antecedent):
                return p.if_(primitivise(test),
                             primitivise(consequent),
                             primitivise(antecedent))
        def effects(*tca):  return any(ir_effects(f)  for f in tca)
        def affected(*tca): return any(ir_affected(f) for f in tca)

# LET

_let = intern("LET")[0]

@defknown((_binder, _let,
           (_let, " ",  ([(_notlead, "\n"), (_bind, _variable, (_or, (_satisfies, namep), ((_satisfies, namep), " ", (_form,))))],),
            1, [(_notlead, "\n"), (_bound, (_form,))])),
          name = _let)
class let(known):
        def rewrite(orig, bindings, *body):
                names, forms = list(zip(*xmap_to_vector(lambda x: ((first(x), second(x)) if consp(x) else (x, nil)),
                                                               bindings))) or ([], [])
                check_no_locally_rebound_constants(names)
                return (not (body and bindings and every(lambda x: consp(x) and length(x) == 2, bindings)),
                        (list__(_let, mapcar(list_, consify_linear(names), consify_linear(forms)),
                                consify_linear(body))
                         if bindings and body else
                         handle_linear_body(body) if body              else
                         list__(_progn, append(consify_linear(forms), list_(nil)))))
        def nvalues(bindings, *body):            return 1   if not body else ir_nvalues(body[-1])
        def nth_value(n, orig, bindings, *body): return nil if not body else ir_nth_valueify_last_subform(n, orig)
        def lower(bindings, *body):
                # Unregistered Issue PRIMITIVE-DECLARATIONS
                # Unregistered Issue DEAD-CODE-ELIMINATION
                normalised = vectorise_linear(bindings)
                env = make_lexenv_varframe(list(zip(*normalised))[0], allocate_tns = t)
                with progv({ _lexenv_: env }):
                        guts = p.progn(*(primitivise(x) for x in body))
                return p.let(list((env.varframe["name"][name].tn, primitivise(form))
                                  for name, (form, __) in normalised),
                             guts)
        def effects(bindings, *body):
                ## Unregistered Issue LET-EFFECT-COMPUTATION-PESSIMISTIC
                return any(ir_effects(f) for f in (x[1][0] for x in bindings) + body)
        def affected(bindings, *body):
                return any(ir_affected(f) for f in (x[1][0] for x in bindings) + body)

# FUNCTION

intern_and_bind_symbols("SETF", "LOCALLY")

## Unregistered Issue MACROEXPANDABILITY-OF-FUNCTION-SUBFORM-IS-INTERESTING
@defknown((intern("FUNCTION")[0], " ", (_form,)))
class function(known):
        def rewrite(orig, x):
                lambdap = pyref_p(x) or not (consp(x) and x[0] is _lambda)
                return not lambdap, (orig if lambdap else
                                     x) ## Elide the FUNCTION form -- LAMBDA is a known.
                                        ## Unregistered Issue LAMBDA-IS-NOT-A-MACRO
        ## Unregistered Issue COMPLIANCE-FUNCTION-NAMESPACE-SEPARATION
        def nvalues(_):            return 1
        def nth_value(n, orig, _): return orig if n is 0 else nil
        def lower(name):
                ## (QUOTE ("str"))
                if pyref_p(name):
                        return primitivise_pyref(name)
                lexical_binding, lexenv = symbol_value(_lexenv_).lookup_func(the(symbol_t, name))
                if not lexical_binding:
                        ## Unregistered Issue FDEFINITION-SYMBOL-FUNCTION-AND-COMPILER-GFUNS-NEED-SYNCHRONISATION
                        if not find_global_function(name):
                                simple_style_warning("undefined function: %s", name)
                        unit_note_gfun_reference(name)
                return (function_tn(name) if not lexical_binding else
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

@defknown((intern("UNWIND-PROTECT")[0], " ", (_form,),
           1, [(_notlead, "\n"), (_form,)]))
class unwind_protect(known):
        def rewrite(orig, form, *unwind_body):
                return ((t, cont(form)) if not unwind_body or all(constantp(x) for x in unwind_body) else
                        (lambda gs: (t, list_(_let, list_(list_(gs, form)),
                                              *(unwind_body
                                                + (gs,)))))
                        (gensym("UWP-CONSTANT-VALUE-")) if constantp(form) else
                        (nil, orig))
        def nvalues(form, *unwind_body):            return ir_nvalues(form)
        def nth_value(n, orig, form, *unwind_body): return list__(_unwind_protect, ir_nth_value(n, form),
                                                                  consify_linear(unwind_body))
        def lower(form, *unwind_body):
                return p.unwind_protect(primitivise(form),
                                        p.progn(*(primitivise(x) for x in unwind_body)))
        def effects(form, *unwind_body):
                return any(ir_effects(f) for f in (form,) + body)
        def affected(form, *unwind_body):
                return any(ir_affected(f) for f in (form,) + body)

# REF

_ref = intern("REF")[0]

@defknown((_ref, " ", (_or, (_satisfies, namep), (_satisfies, pyref_p))))
class ref(known):
        def rewrite(orig, x):
                pyrefp = pyref_p(x)
                return not pyrefp, (orig if pyrefp else x)
        def nvalues(_):            return 1
        def nth_value(n, orig, _): return orig if n is 0 else nil
        def lower(name):
                if pyref_p(name):
                        return primitivise_pyref(name)
                cur_lexenv = symbol_value(_lexenv_)
                lexical_binding, src_lexenv = cur_lexenv.lookup_var(the(symbol_t, name))
                if not lexical_binding or lexical_binding.kind is _special:
                        gvar = find_global_variable(name)
                        if not gvar and not lexical_binding: # Don't complain on yet-unknown specials.
                                simple_style_warning("undefined variable: %s", name)
                        unit_note_gvar_reference(name)
                        ## Note, how this differs from FUNCTION:
                        return p.special_ref(p.name(unit_symbol_pyname(name)))
                if cur_lexenv.clambda is not src_lexenv.clambda:
                        cur_lexenv.clambda.nonlocal_refs.add(name)
                return lexical_binding.tn
        def effects(name):         return nil
        def affected(name):        return not global_variable_constant_p(name)

# LAMBDA

class compiler_lambda():
        __slots__ = ("name", "lambda_list",
                     "args", "whole", "fixed", "optional", "rest", "keys", "aux",
                     "forms", "optdefs", "keydefs", "auxforms",
                     "total", "total_types", "value_types",
                     "aokp", "keysp",
                     "nonlocal_refs", "nonlocal_setqs")
        def __repr__(self):
                return "#<CLAMBDA %s {%x}>" % (pp_consly(self.lambda_list),
                                               id(self))
        def __init__(self, name, lambda_list):
                total, args, forms, keysp, aokp = ir_parse_lambda_list(lambda_list, "LAMBDA", allow_defaults = t)
                check_no_locally_rebound_constants(total)
                self.name, self.lambda_list = the(symbol_t, name), the(list_t, lambda_list)
                (self.whole, self.fixed, self.optional, self.rest, self.keys, self.aux), \
                    (self.optdefs, self.keydefs, self.auxforms), \
                    self.aokp, self.keysp = args, forms, aokp, keysp
                self.total, self.args, self.forms = total, args, forms
                self.total_types, self.value_types = [t] * len(total), t
                self.nonlocal_refs, self.nonlocal_setqs = set(), set()

def ir_parse_lambda_list(orig_lambda_list, context, allow_defaults = None, macrop = nil):
        ## Critical Issue LAMBDA-LIST-PARSING-BROKEN-WRT-BODY
        if not listp(orig_lambda_list):
                error("In %s: lambda list must be a proper list, was: %s.", context, pp_consly(lambda_list))
        lastcdr = last(orig_lambda_list)
        improper = lastcdr and lastcdr[1] is not nil
        ## Normalise:
        lambda_list = ((vectorise_linear(butlast(orig_lambda_list)) + [lastcdr[0], _rest, lastcdr[1]]) if improper else
                       vectorise_linear(orig_lambda_list))
        if not macrop and improper:
                error("Dotted lambda list provided where one is not allowed: %s", pp_consly(lambda_list))
        ### 0. locate lambda list keywords
        lambda_words = [_whole, _optional, _rest, _body, _key, _allow_other_keys, _aux, _environment]
        wholepos, optpos,  restpos,  bodypos,  keypos, aokpos, auxpos, envpos = posns = \
            [ (lambda_list.index(x) if x in lambda_list else None)
              for x in lambda_words ]
        # dprintf("%s %s,  %s %s,  %s %s,  %s %s,  %s %s,  %s %s,  %s %s,  %s %s",
        #         _whole, _optional, _rest, _body, _key, _allow_other_keys, _aux, _environment,
        #         wholepos, optpos,  restpos,  bodypos,  keypos, aokpos, auxpos, envpos)
        wholeposp, optposp, restposp, bodyposp, keyposp, aokposp, auxposp, envposp = [ x is not None for x in posns ]
        fixedpos  = 0 if not wholeposp else 2
        nauxpos = len(lambda_list)
        rbposp = restposp or bodyposp
        rbpos = (restpos if restposp else
                 bodypos if bodyposp else
                 None)
        ### 1. ensure proper order of provided lambda list keywords
        twholepos   = wholepos or 0
        toptpos     = optpos or twholepos
        trbpos      = rbpos or toptpos
        tkeypos     = keypos or trbpos
        taokpos     = aokpos or tkeypos
        tauxpos     = auxpos or taokpos
        naokpos     = defaulted(auxpos, nauxpos)
        nkeypos     = defaulted(aokpos, naokpos)
        nrbpos      = defaulted(keypos, nkeypos)
        noptpos     = defaulted(rbpos, nrbpos)
        nwholepos   = defaulted(optpos, noptpos)
        nfixpos     = nwholepos
        if wholeposp and not macrop:
                error("In %s: ordinary lambda list does not allow the &WHOLE keyword.", context)
        if restposp and bodyposp:
                error("In %s: &BODY and &REST cannot coexist in a single lambda list.", context)
        if not twholepos <= toptpos <= trbpos <= tkeypos <= taokpos <= tauxpos:
                error("In %s: %s, %s, %s, %s, %s, %s and %s must appear in the lambda list in that precise order, whenever specified.",
                      context, *lambda_words[:-1])
        if (wholeposp and nwholepos - wholepos < 2 or
            rbposp    and nrbpos - rbpos != 2      or
            aokposp   and naokpos - aokpos != 1):
#                 dprintf(
# """(wholeposp %s and wholepos %s - nwholepos %s %s < 2 or
#  rbposp %s   and rbpos %s - nrbpos %s %s != 2      or
#  aokposp %s  and aokpos %s - naokpos %s %s != 0)""",
# wholeposp, wholepos, nwholepos, wholeposp and wholepos - nwholepos,
# rbposp, rbpos, nrbpos, rbposp and rbpos - nrbpos,
# aokposp, aokpos, naokpos, aokposp and aokpos - naokpos)
                error("In %s: found garbage instead of a lambda list: %s", context, pp_consly(orig_lambda_list))
        # locals_printf(locals(),
        #                "optpos",  "restpos",  "keypos",
        #                "optposp", "restposp", "keyposp",
        #                "toptpos", "trestpos", "tkeypos")
        ### 2. compute argument specifier sets, as determined by provided lambda list keywords
        def parse_maybe_defaulted(xs):
                return ([ x[0] if consp(x) else x
                          for x in xs ],
                        [ (second(x) if consp(x) else nil)
                          for x in xs ])
        whole  = lambda_list[wholepos + 1] if wholeposp else None
        fixed = lambda_list[fixedpos:nfixpos]
        optional, optdefs = parse_maybe_defaulted(lambda_list[optpos + 1:noptpos] if optposp else [])
        rest_or_body  = lambda_list[rbpos + 1] if rbposp else None
        keys, keydefs = parse_maybe_defaulted(lambda_list[keypos + 1:nkeypos] if keyposp else [])
        if not all(isinstance(x, symbol_t) for x in fixed):
                viola = [ x for x in fixed if not symbolp(x) ][0] ## find-if
                error("In %s: fixed arguments must be symbols, but %s (of type %s) wasn't one.",
                      context, viola, type(viola).__name__)
        aux, auxforms = parse_maybe_defaulted(lambda_list[auxpos + 1:] if auxposp else [])
        total = (([whole] if wholeposp else [])
                 + fixed
                 + optional
                 + ([rest_or_body] if rbposp else [])
                 + keys
                 + aux)
        ### 3. validate syntax of the provided individual argument specifiers
        bad_paramspecs = [ x for x in total if not namep(x) ]
        if bad_paramspecs:
                error("In %s, lambda list %s: bad parameter specifiers: %s",
                      context, pp_consly(orig_lambda_list), ", ".join(str(x) for x in bad_paramspecs))
        ### 4. check for duplicate lambda list specifiers
        if len(total) != len(set(total)):
                error("In %s: duplicate parameter names in lambda list: %s.", context, orig_lambda_list)
        return (total,
                (whole, fixed, optional, rest_or_body, keys, aux),
                (optdefs, keydefs, auxforms),
                keyposp, aokposp)

def lower_lambda_list(context, fixed, optional, rest, keys, opt_defaults, key_defaults):
        assert len(optional) == len(opt_defaults)
        assert len(keys)     == len(key_defaults)
        def to_names(xs): return [ variable_tn(x) for x in xs ]
        return (to_names(fixed),
                to_names(optional),
                [ primitivise(x) if x is not None else p.name("None")
                  for x in opt_defaults ],
                variable_tn(rest) if rest else None,
                to_names(keys),
                [ primitivise(x) if x is not None else p.name("None")
                  for x in key_defaults ],
                None)

## Welcome to the wonderful world of macros shadowed by a named lambda, within its arglist defaulting forms..
## ..also, to the wonderful world of %IR-ARGS being critically unhandled.
@defknown((_binder, _lambda,
           (_lambda, (_funcher, _lambda))),
          name = _lambda)
class lambda_(known):
        def binder(exp, further, name = nil, decorators = nil):
                form = form_real(exp)
                length(form) < 2 and error("Bad LAMBDA form: %s", pp_consly(form))
                lam = form[1][0]
                not listp(lam) and error("Bad lambda list in LAMBDA form: %s", pp_consly(exp))
                clambda = compiler_lambda(name, lam)
                with progv(dict([ (_walker_binder_, lexenv_walker.binder(0, "Lambda.")),
                                  (_walker_binder_args_, nil),
                                  (_walker_lexenv_,
                                   make_lexenv(symbol_value(_walker_lexenv_), clambda = clambda,
                                               allocate_tns = symbol_value(_walker_allocate_tns_),
                                               name_funcframe = { name: function_binding(name, _function, fn(name, lam)) }))
                                 ])):
                        return further(exp)
        def rewrite(orig, lambda_list, *body, name = nil, decorators = nil):
                # Unregistered Issue COMPLIANCE-MACRO-LAMBDA-LIST-DESTRUCTURING-AND-ENV
                env = symbol_value(_walker_lexenv_)
                clambda = compiler_lambda(name, lambda_list)
                (whole, fixed, optional, rest, keys, aux), (optdefs, keydefs, auxforms) = \
                    args, forms = clambda.args, clambda.forms,
                complexp = not not (optional or rest or clambda.keysp or aux)
                if not complexp: ## Only has &WHOLE and fixed args:
                        with progv({ _walker_lexenv_: make_lexenv_varframe(clambda.total, parent = env) }):
                                return nil, ir(*vectorise_linear(orig),
                                               **dictappend({ "name":       name }       if name       else {},
                                                            { "decorators": decorators } if decorators else {}))
                ## Optimisations
                # constant_forms_p = all(constantp(x) for x in optdefs + keydefs + auxforms)
                opt_gsyms        = [ gensym_tn("OPT-" + symbol_name(x) + "-")
                                     for x in optional ]
                need_rest        = rest or clambda.keysp ## &key is processed through parsing of *rest
                rest_gsym        = ((gensym_tn("REST-" + (symbol_name(rest) if rest else
                                                           "TN") + "-")) if need_rest else
                                    None)
                must_check_keys  = clambda.keysp and not clambda.aokp
                keyset_gsym      = gensym("KEYSET-") if must_check_keys else nil
                key_map_gsym     = gensym_tn("KWHASH") if clambda.keysp else nil
                ksyms            = [ make_keyword_tn(symbol_name(x)) for x in keys ]
                ## 1. validate_keyword_args, parse_keyword_args
                ### This is the biggest victim of IR representation mixing (cons vs. linear-tuple),
                ### all in the name of enrolling python lambda lists on board.
                l, l_, a = list_, list__, append
                return nil, ir(_lambda, a(l(whole) if whole else nil,
                                          consify_linear(fixed),
                                          l_(_optional, consify_linear(opt_gsyms)) if optional else nil,
                                          l(_rest, rest_gsym) if need_rest else nil),
                               l(_let_, a(consify_linear(l(name, l(_if, l(_prim, p.eq, gs.tn, p.name("None")),
                                                                   def_expr,
                                                                   gs))
                                                         for name, gs, def_expr in zip(optional, opt_gsyms, optdefs)),
                                          (l(l(rest, ir_cl_call("consify_linear", rest_gsym)))
                                           if rest else nil),
                                          (l_(l(key_map_gsym, ir_cl_call("parse_keyword_args", rest_gsym)),
                                                consify_linear(l(name, l(_if, l(_prim, p.not_in, ksym.tn, key_map_gsym.tn),
                                                                         def_expr,
                                                                         l(_prim, p.index, key_map_gsym.tn, ksym.tn)))
                                                             for name, ksym, def_expr in zip(keys, ksyms, keydefs)))
                                           if clambda.keysp else nil),
                                          (l(l(keyset_gsym, ir_apply("set", ir_cl_call("vectorise_linear", ir_funcall(_list, *ksyms)))),
                                             l(gensym("DUMMY-"), ir_cl_call("validate_keyword_args", keyset_gsym, key_map_gsym)))
                                           if must_check_keys else nil),
                                          consify_linear(l(name, form)
                                                         for name, form in zip(aux, auxforms))),
                                 *body),
                               **dictappend({ "name":       name }       if name       else {},
                                            { "decorators": decorators } if decorators else {}))
        def nvalues(*_):            return 1
        def nth_value(n, orig, *_): return orig if n is 0 else nil
        def lower(lambda_list, *body, name = nil, decorators = nil):
                clambda = compiler_lambda(name, lambda_list)
                (whole, fixed, optional, rest, keys, aux), (optdefs, keydefs, auxforms) = \
                    args, forms = clambda.args, clambda.forms
                assert not (whole or clambda.keysp or aux) ## &WHOLE is a piece of debt, currently.
                fnname_tn = function_tn(name) if name else None
                ## Things, that are needed:
                ##  - CLAMBDA passing
                nonlocal_decl = ([ p.nonlocal_([ variable_tn(x)
                                                 for x in sorted(clambda.nonlocal_setqs, key = symbol_name) ]) ]
                                 if clambda.nonlocal_setqs else [])
                varframe      = dict((b, variable_binding(b, _variable, None))
                                     for b in clambda.total)
                with progv({ _lexenv_: make_lexenv(symbol_value(_lexenv_), clambda = clambda, allocate_tns = t,
                                                   name_varframe  = varframe,
                                                   name_funcframe = { name: function_binding(name, _function, fn(name, lambda_list)) }) }):
                        return p.lambda_(lower_lambda_list("LAMBDA", *(fixed, optional, rest or None, [],
                                                                       [None] * len(optional), [])),
                                         p.progn(*nonlocal_decl
                                                 + [ primitivise(x) for x in body ]),
                                         name = fnname_tn,
                                         decorators = xmap_to_vector(primitivise, decorators))
        def effects(*_):            return nil
        def affected(*_):           return nil

# PRIM

_prim = intern("PRIM")[0]

@defknown((_prim, " ", (_typep, t), [" ", (_form,)]))
class prim(known):
        def rewrite(orig, prim, *args, **keys):
                return nil, (ir(_prim, prim, *args,
                                **keys) if keys else
                             orig)
        def nvalues(*_):            return 1
        def nth_value(n, orig, *_): return orig if n is 0 else nil
        def lower(prim, *args, **keys):
                return prim(*args, **keys)
        def effects(*_):            return t
        def affected(*_):           return t

# APPLY

@defknown((_apply, " ", (_form,), " ", (_form,), [" ", (_form,)]))
class apply(known):
        def nvalues(func, _, *__):            return ir_function_form_nvalues(func)
        def nth_value(n, orig, func, _, *__): return ir_function_form_nth_value_form(n, func, orig)
        def lower(func, arg, *args):
                ## Unregistered Issue IMPROVEMENT-APPLY-COULD-VALIDATE-CALLS-OF-KNOWNS
                fixed, rest = (((),                 arg)       if not args                  else
                               ((arg,) + args[:-1], args[-1]))
                ## Note, how the test below is too weak to be useful:
                ## the comparison against a literal NIL is much weaker than a NULL type membership test.
                ## Therefore, the important evolutionary question, is what kind of preparations are
                ## required to make such type analysis viable.
                if rest is nil or rest == list_(_quote, nil):
                        return p.funcall(primitivise(func), *(primitivise(x) for x in fixed))
                else:
                        return p.apply(primitivise(func), *(list(primitivise(x) for x in fixed)
                                                             + [ p.funcall(p.impl_ref("vectorise_linear"),
                                                                           primitivise(rest)) ]))
        def effects(func, arg, *args):
                return (any(ir_effects(arg) for arg in (func, arg) + args) or
                        ir_depending_on_function_properties(func, lambda fn, effects: effects, "effects"))
        def affected(func, arg, *args):
                return (any(ir_affected(arg) for arg in (func, arg) + args) or
                        ir_depending_on_function_properties(func, lambda fn, affected: affected, "affected"))

# QUOTE

@defknown((intern("QUOTE")[0], " ", (_form, (_for_not_matchers_xform, identity, metasex_pp))))
class quote(known):
        def nvalues(_):            return 1
        def nth_value(n, orig, _): return orig if n is 0 else nil
        def lower(x):
                # Unregistered Issue COMPLIANCE-QUOTED-LITERALS
                if isinstance(x, symbol_t) and not constantp(x):
                        compiler_trace_known_choice(_quote, x, "NONCONSTANT-SYMBOL")
                        return p.symbol(unit_symbol_pyname(x))
                else:
                        prim, successp = try_primitivise_constant(x)
                        if successp:
                                compiler_trace_known_choice(_quote, x, "CONSTANT")
                                return prim
                        elif consp(x):
                                compiler_trace_known_choice(_quote, x, "SEX")
                                return p.literal_list(*(primitivise(list_(_quote, x)) for x in vectorise_linear(x)))
                        else:
                                error("QUOTE: cannot handle %s: non-primitivisable constant atom.")
        def effects(x):            return nil
        def affected(x):           return nil

# MULTIPLE-VALUE-CALL

def do_multiple_value_call(fn, values_frames):
        return fn(*reduce(lambda acc, f: acc + f[1:], values_frames, []))

_multiple_value_call = intern("MULTIPLE-VALUE-CALL")[0]

@defknown
class multiple_value_call(known):
        ## We might start considering the argument forms for the values queries,
        ## once we get into the partial evaluation affairs..
        def nvalues(func, *_):            return ir_function_form_nvalues(func)
        def nth_value(n, orig, func, *_): return ir_function_form_nth_value_form(n, func, orig)
        def lower(fn, *arg_forms):
                ## We have no choice, but to lower immediately, and by hand.
                ## Unregistered Issue SAFETY-VALUES-FRAME-CHECKING
                return p.apply(primitivise(fn),
                               p.add(*(p.slice(primitivise(x), p.integer(1), nil, nil) for x in arg_forms)))
        def effects(fn, *arg_forms):
                return (any(ir_effects(arg) for arg in arg_forms) or
                        ir_depending_on_function_properties(func, lambda fn, effects: effects, "effects"))
        def affected(fn, *arg_forms):
                return (any(ir_affected(arg) for arg in arg_forms) or
                        ir_depending_on_function_properties(func, lambda fn, affected: affected, "affected"))

# CATCH

_catch = intern("CATCH")[0]

@defknown((_catch, " ", (_form,),
          1, [(_notlead, "\n"), (_form,)]))
class catch(known):
        ## Critical Issue CATCH-MULTIPLE-VALUES-NOT-IMPLEMENTED
        def nvalues(_, *body):              return 1 if not body else not_implemented()
        def nth_value(n, orig, tag, *body): return (not_implemented()        if body             else
                                                    list_(_progn, tag, nil)  if ir_effects(tag) else
                                                    nil)
        ## Unregistered Issue DOUBT-WHETHER-LAMBDA-CAN-LOWER-PROLOGUESSLY-DUE-TO-C-L-A-N-T
        def lower(tag, *body):
                return p.catch(primitivise(tag),
                               p.progn(*(primitivise(x) for x in body)))
        def effects(tag, *body):  return ir_effects(tag) or any(ir_effects(f) for f in body)
        def affected(tag, *body): return ir_affected(tag) or any(ir_affected(f) for f in body)

# THROW

_throw = intern("THROW")[0]

@defknown((_throw, " ", (_form,), (_maybe, " ", (_form,))))
class throw(known):
        def nvalues(_, value):            return ir_nvalues(value)
        def nth_value(n, orig, _, value): return (list_(_progn, tag, ir_nth_value(value)) if ir_effects(tag) else
                                                  ir_nth_value(value))
        def lower(tag, value):
                return p.throw(primitivise(tag), primitivise(value))
        def effects(tag, value):          return ir_effects(tag) or ir_effects(value)
        def affected(tag, value):         return ir_affected(tag) or ir_affected(value)

# NTH-VALUE

@defknown((intern("NTH-VALUE")[0], " ", (_form,), " ", (_form,)))
class nth_value(known):
        def nvalues(_, __):    return 1
        def nth_value(n, orig, form_n, form):
                return (list_(_nth_value, n, orig) if not (integerp(n) and integerp(form_n)) else ## Give up.  Too early?
                        nil                        if n != form_n and not ir_effects(form)   else
                        list_(_progn, form, nil)   if n != form_n                            else
                        ir_nth_value(n, form)) ## We don't risk unbounded recursion here, so let's analyse further..
        def lower(n, form):
                return p.funcall(p.impl_ref("values_frame_project"), primitivise(n), primitivise(form))
        def effects(n, form):   return ir_effects(n) or ir_effects(form)
        def affected(n, form):  return ir_affected(n) or ir_affected(form)

# PROGV

@defknown((intern("PROGV")[0], " ", (_form,), " ", (_form,), ([(_notlead, "\n"), (_form,)],)))
class progv(known):
        def nvalues(_, __, *body):                    return 1 if not body else ir_nvalues(body[-1])
        def nth_value(n, orig, names, values, *body): return (ir_nth_valueify_last_subform(n, orig)
                                                              if body                     else
                                                              list_(_progn,
                                                                    cons(_list, names),
                                                                    cons(_list, values),
                                                                    nil)
                                                              if (ir_effects(names)
                                                                  or ir_effects(values)) else
                                                              nil)
        def lower(vars, vals, *body):
                return p.progv((primitivise(x) for x in vars), (primitivise(x) for x in vals),
                               p.progn(*(primitivise(x) for x in body)))
        def effects(names, values, *body):            return any(ir_effects(f) for f in (names, values) + body)
        def affected(names, values, *body):           return any(ir_affected(f) for f in (names, values) + body)

# PROTOLOOP

_protoloop = intern("PROTOLOOP")[0]

@defknown((_protoloop, ["\n", (_form,)]))
class protoloop(known):
        "This was implemented exclusively for the sake of TAGBODY."
        ## Critical Issue PROTOLOOP-MULTIPLE-VALUES-NOT-IMPLEMENTED
        def nvalues(*_):            return not_implemented()
        def nth_value(n, *_):       return not_implemented()
        def lower(*body):
                return p.loop(p.progn(*(primitivise(x) for x in body)))
        def effects(*body):         return any(ir_effects(x)  for x in body)
        def affected(*body):        return any(ir_affected(x) for x in body)

# THE

@defknown((intern("THE")[0], " ", (_form,), " ", (_form,)))
class the(known):
        def nvalues(type, form):            not_implemented()
        def nth_value(n, orig, type, form): not_implemented()
        def lower(type, form):              not_implemented()
        def effects(type, form):            return ir_effects(form)
        def affected(type, form):           return ir_affected(form)

# LOCALLY

@defknown((intern("LOCALLY")[0], (["\n", (_form,)],)))
class locally(known):
        def nvalues(*decls_n_body):            not_implemented()
        def nth_value(n, orig, *decls_n_body): not_implemented()
        def lower(*decls_n_body):              not_implemented()
        def effects(*decls_n_body):            not_implemented()
        def affected(*decls_n_body):           not_implemented()

# MULTIPLE-VALUE-PROG1

@defknown((intern("MULTIPLE-VALUE-PROG1")[0], " ", (_form,), (["\n", (_form,)],)))
class multiple_value_prog1(known):
        def nvalues(first_form, *forms):            return ir_nvalues(first_form)
        def nth_value(n, orig, first_form, *forms):
                return (ir_nth_value(n, first_form) if not any(ir_effects(f) for f in forms) else
                        (lambda sym: list__(_let, list_(list_(sym, ir_nth_value(n, first_form))),
                                            consify_linear(forms + (sym,))))
                        (gensym("MV-PROG1-VALUE-")))
        def lower(first_form, *forms):              not_implemented()
        def effects(first_form, *forms):            return ir_effects(first_form) or any(ir_effects(f) for f in forms)
        def affected(first_form, *forms):           return ir_affected(first_form) or any(ir_affected(f) for f in forms)

# LOAD-TIME-VALUE

@defknown((intern("LOAD-TIME-VALUE")[0], " ", (_form,), (_maybe, " ", (_typep, (member_t, t, nil)))))
class load_time_value(known):
        def nvalues(form, read_only_p):            not_implemented()
        def nth_value(n, orig, form, read_only_p): not_implemented()
        def lower(form, read_only_p):              not_implemented()
        def effects(form, read_only_p):            not_implemented()
        def affected(form, read_only_p):           not_implemented()

# Tests

def run_tests_known():
        def lexenvful_identity_rewrite(input):
                def identity_rewriter(form, further: "Form -> ({} Form Bool)") -> "({} Form Bool)":
                        return further(form)
                return walk_with_lexenv(identity_rewriter, input, lexenv = _null)
        def applyification(input):
                # Essentially a walk_with_lexenv(macroexpander_xform, sex, lexenv), with *MACROEXPANDER-COMPILERP* bound.
                return compiler_macroexpand_all(input, lexenv = _null)
        l = list_
        assert runtest(lexenvful_identity_rewrite,
                       l(_let, l(l(_car, _cdr))),
                       l(_let, l(l(_car, _cdr))))
        assert runtest(applyification, 
                       l(_let, l(l(_car, _cdr)),
                         l(_lambda, l(_car, _cdr),
                           l(_cdr, _car))),
                       l(_let, l(l(_car, _cdr)),
                         l(_lambda, l(_car, _cdr),
                           l(_funcall, l(_function, _cdr), _car))))

if getenv("CL_RUN_TESTS") != "nil" and getenv("CL_TEST_KNOWN") != "nil":
        with matcher_pp_stack():
                run_tests_known()

# Core: %PRIMITIVISE, %EMIT-AST, %LOWER and COMPILE

def report(macroexpanded = None, known = None, primitive = None, ast = None, bytecode = None,
            desc = "", form_id = None, lexenv = None):
        lexenv  = "%s\n"  % coerce_to_lexenv(lexenv) if  lexenv is not None else ""
        desc    = "%s - " % desc                     if    desc is not None else ""
        form_id = "  %x"  % form_id                  if form_id is not None else ""
        if macroexpanded is not None:
                dprintf(";;; %smacroexpanded ............%s\n%s%s\n",
                              desc, form_id, lexenv, pp(macroexpanded))
        if known is not None:
                dprintf(";;; %sknowns ..............%s\n%s%s\n",
                              desc, form_id, lexenv, pp(known))
        if primitive is not None:
                dprintf(";;; %sprimitives ==========%s\n%s%s\n",
                              desc, form_id, lexenv, primitive)
        if ast:
                import more_ast
                dprintf(";;; %spython ------------->%s\n%s\n",
                              desc, form_id, "\n".join(more_ast.pp_ast_as_code(x, line_numbers = t)
                                                       for x in ast))
        if bytecode:
                dprintf(";;; %sbytecode ************%s\n", desc, form_id)
                import dis
                def rec(x):
                        dis.dis(x)
                        for sub in x.co_consts:
                                if isinstance(sub, types.CodeType):
                                        dprintf(";;; child code -------------\n")
                                        rec(sub)
                rec(bytecode)

# Unregistered Issue COMPILER-MACRO-SYSTEM
def primitivise(form, lexenv = nil) -> p.prim:
        # - tail position tracking
        # - scopes
        # - symbols not terribly clear
        # - proper quote processing
        def compiler_maybe_note_subprimitivisation(x):
                if (symbol_value(_compiler_trace_subprimitivisation_)
                    and not isinstance(x, (symbol_t, bool, int, str))
                    and not (consp(x) and x[0] in [_ref, _function, _quote])):
                        report(known = x, desc = "PRIMITIVISE", lexenv = coerce_to_lexenv(symbol_value(_lexenv_)))
        def compiler_maybe_note_inner(known_name, xs):
                if symbol_value(_compiler_trace_inner_knowns_) and known_name is not _symbol:
                        dprintf("%s>>> %s\n%s%s", sex_space(), name,
                                      sex_space(), ("\n" + sex_space()).join(pp(f) for f in xs))
        def compiler_maybe_note_known_primitives(form, prim):
                if (symbol_value(_compiler_trace_known_primitives_) and
                    ## Too trivial to take notice
                    not typep(form, (or_t, symbol_t, string_t, integer_t, (cons_t, (member_t, _quote, _function, _ref), t)))):
                        ssp = sex_space()
                        dprintf(";;;\n;;; knowns ->\n;;;\n%s%s\n;;;\n;;; -> primitives\n%s",
                                      ssp, pp(form),
                                      prim)
        def rec(x):
                ## XXX: what are the side-effects?
                ## NOTE: we are going to splice unquoting processing here, as we must be able
                ## to work in READ-less environment.
                compiler_maybe_note_subprimitivisation(x)
                if x is nil or isinstance(x, symbol_t) and not constantp(x):
                        ## Unregistered Issue SYMBOL-MODEL
                        return rec(list_(_ref, x))
                elif listp(x):
                        if isinstance(x[0], symbol_t):
                                argsp, form, args = destructure_possible_ir_args(x)
                                # Urgent Issue COMPILER-MACRO-SYSTEM
                                known = find_known(form[0])
                                if not known:
                                        error("Invariant failed: no non-known IR node expected at this point.  Saw: %s.", x)
                                compiler_maybe_note_inner(known.name, form)
                                return known.lower(*vectorise_linear(form[1]), **alist_hash_table(args))
                        else:
                                error("Invalid form: %s.", pp_consly(x))
                else:
                        # NOTE: we don't care about quoting here, as constants are self-evaluating.
                        return primitivise_constant(x)

        ## XXX: what about side-effects?
        with progv({ _lexenv_: coerce_to_lexenv(lexenv) }):
                prim = the(p.prim, rec(form))
                compiler_maybe_note_known_primitives(form, prim)
                return prim

fixupp = gensym("FIXUPP")

class name_context_fixer(ast.NodeTransformer):
        def visit_Name(w, o):
                return (ast.Name(o.id, ast.Store()) if symbol_value(fixupp) else
                        o)
        def visit_Assign(w, o):
                with progv({ fixupp: t }):
                        targets = [ w.visit(x)
                                    for x in o.targets ]
                return ast.Assign(targets = targets,
                                   value = w.visit(o.value))
        def visit_AugAssign(w, o):
                with progv({ fixupp: t }):
                        target = w.visit(o.target)
                return ast.AugAssign(target = target,
                                      op = o.op,
                                      value = w.visit(o.value))
        def visit_For(w, o):
                with progv({ fixupp: t }):
                        target = w.visit(o.target)
                return ast.For(target = target,
                                iter = w.visit(o.iter),
                                body = [ w.visit(x) for x in o.body ],
                                orelse = [ w.visit(x) for x in o.orelse ])
        def visit_With(w, o):
                with progv({ fixupp: t }):
                        optional_vars = w.visit(o.optional_vars)
                return ast.With(context_expr = w.visit(o.context_expr),
                                 optional_vars = optional_vars,
                                 body = [ w.visit(x) for x in o.body ])
        def visit_comprehension(w, o):
                with progv({ fixupp: t }):
                        target = w.visit(o.target)
                return ast.comprehension(target = target,
                                          iter = w.visit(o.iter),
                                          ifs = [ w.visit(x) for x in o.ifs ])
        def visit_Subscript(w, o):
                writep = symbol_value(fixupp)
                with progv({ fixupp: nil }):
                        return ast.Subscript(value = w.visit(o.value),
                                              slice = w.visit(o.slice),
                                              ctx = ast.Store() if writep else o.ctx)
        def visit_Attribute(w, o):
                writep = symbol_value(fixupp)
                with progv({ fixupp: nil }):
                        return ast.Attribute(value = w.visit(o.value),
                                              attr = o.attr,
                                              ctx = ast.Store() if writep else o.ctx)

name_context_fixer = name_context_fixer()

def ast_fixup_written_name_contexts(x):
        with progv({ fixupp: nil }):
                return name_context_fixer.visit(the(ast.AST, x))

def emit_ast(prim) -> [p.stmt]:
        xs = p.help_prog([prim])
        return [ ast_fixup_written_name_contexts(x) for x in xs ]

def lower(form, lexenv = nil):
        "Must be called within %WITH-SYMBOL-UNIT-MAGIC context."
        ## Macroexpanded SEX -> MacIR
        rewritten = rewrite_all(form, lexenv = lexenv)    ## No other high-level entry point to %REWRITE-ALL
        if symbol_value(_compiler_trace_rewritten_):
                report(known = rewritten, form_id = id(form), desc = "%LOWER", lexenv = lexenv)
        ## HIR -> LIR
        prim = primitivise(rewritten, lexenv = lexenv)    ## No other high-level entry point to %PRIMITIVISE.
        if symbol_value(_compiler_trace_primitives_):
                report(primitive = prim, form_id = id(form), desc = "%LOWER", lexenv = lexenv)
        ##
        ## LIR -> target AST
        ast  = emit_ast(prim)
        if symbol_value(_compiler_trace_ast_):            ## No other high-level entry point to %EMIT-AST.
                report(ast = ast, form_id = id(form), desc = "%LOWER")
        return ast

def process_to_ast(form, lexenv = nil):
        "Same as %LOWER, but also macroexpand.  Requires %WITH-SYMBOL-UNIT-MAGIC context all the same."
        check_type(lexenv, (or_t, null_t, lexenv_t))
        if symbol_value(_compiler_trace_forms_):
                dprintf(";;;%s compiling:\n%s%s",
                              sex_space(-3, ";"), sex_space(), pp(form))
        macroexpanded = compiler_macroexpand_all(form, lexenv = lexenv)
        if symbol_value(_compiler_trace_macroexpanded_):
                if form != macroexpanded:
                        report(macroexpanded = macroexpanded, desc = "PROCESS-AST", lexenv = lexenv)
                else:
                        dprintf(";;;%s macroexpansion had no effect", sex_space(-3, ";"))
        return lower(macroexpanded, lexenv = lexenv)

# Linkage: %ASSEMBLE, %PROCESS-AS-LOADABLE, %LOAD-MODULE-BYTECODE

#
## High-level users of %LOWER   ---   UPDATE !!!
#
## eval, eval_tlf
##   do_eval
##     eval_in_lexenv        <-_
##       simple_eval_in_lexenv /
##         simple_eval =-> compile_in_lexenv
## read_function_object_ast_compile_load_and_pymport, compile_in_lexenv (<-= compile), compile_in_lexenv (<-= macro_function, simple_eval)
##   compile_and_load_function
##     compile_loadable_unit :: form -> code-object
##       expand_process_and_lower_in_lexenv =-> lower
##       compilation_unit_prologue =-> lower
## @defknown -> lower
#

_vector = intern("VECTOR")[0]

def compilation_unit_prologue(funs, syms, gfuns, gvars):
        """Emit a prologue for a standalone unit referring to SYMBOLS."""
        def import_prologue():
                return p.help(p.import_(p.name("cl")))[0]
        def symbol_prologue():
                def wrap(x):
                        return defaulted(x, consify_star(_ref, (_quote, ("None",))))
                with progv(compiler_debugless_traceless_frame):
                 symbols = sorted(funs | syms, key = str)
                 prologue = list_(_progn,
                                  ir_cl_call(
                                  "fop_make_symbol_available",
                                  ir_apply("globals"),
                                  "COMMON-LISP", "VECTOR", ensure_function_pyname(_vector),
                                  list_(_ref, list_(_quote, list_("None"))),
                                  True, False),
                                  ir_cl_call(
                                  "fop_make_symbols_available",
                                  ir_apply("globals"),
                                  ir_apply(_vector, *tuple(package_name(symbol_package(sym)) if symbol_package(sym) else consify_star(_ref, (_quote, ("None",)))
                                                             for sym in symbols )),
                                  ir_apply(_vector, *tuple(symbol_name(sym)          for sym in symbols )),
                                  ir_apply(_vector, *tuple(wrap(sym.function_pyname) for sym in symbols )),
                                  ir_apply(_vector, *tuple(wrap(sym.symbol_pyname)   for sym in symbols )),
                                  ir_apply(_vector, *tuple(sym in gfuns              for sym in symbols )),
                                  ir_apply(_vector, *tuple(sym in gvars              for sym in symbols ))))
                 # dprintf("prologue:\n%s", pp_consly(prologue))
                 return lower(prologue,
                               ## Beacon LEXENV-CLAMBDA-IS-NIL-HERE
                               lexenv = the_null_lexenv())
        return (import_prologue() +
                symbol_prologue())

def with_symbol_unit_magic(body, standalone = nil, id = "UNIT-"):
        "Ensure symbol availability, for the code emitted by BODY, by prepending it with name initialisers."
        def in_compilation_unit():
                stmts = body()
                check_type(stmts, list)
                unit_data = compilation_unit_symbols()
                return ((compilation_unit_prologue(*unit_data) if standalone else []) +
                        stmts,) + tuple(unit_data)
        return with_compilation_unit(in_compilation_unit,
                                     override = t, id = id)

def assemble(_ast: [ast.stmt], form: cons_t, filename = "") -> "code":
        import more_ast
        if symbol_value(_compiler_validate_ast_):
                [ ast_validate(a) for a in _ast ]
        more_ast.assign_meaningful_locations(_ast)
        if symbol_value(_compiler_trace_module_ast_):
                report(ast = _ast, form_id = id(form), desc = "%ASSEMBLE")
        bytecode = py.compile(ast.fix_missing_locations(ast_module(_ast)), filename, "exec")
        if symbol_value(_compiler_trace_bytecode_):
                report(bytecode = bytecode, form_id = id(form), desc = "%ASSEMBLE")
        return bytecode

def process_as_loadable(processor, form, lexenv = nil, id = "PROCESSED-"):
        stmts, *_ = with_symbol_unit_magic(lambda: processor(form, lexenv = lexenv),
                                           standalone = t, id = id)
        return assemble(stmts, form)

def load_module_bytecode(bytecode, func_name = nil, filename = ""):
        mod, globals, locals = load_code_object_as_module(filename, bytecode, register = nil)
        if func_name:
                sf = the((or_t, symbol_t, function_t), # globals[get_function_pyname(name)]
                         mod.__dict__[get_function_pyname(func_name)])
                func = sf if functionp(sf) else symbol_function(sf)
                # without_condition_system(pdb.set_trace) # { k:v for k,v in globals().items() if v is None }
                func.name = func_name # Debug name, as per F-L-E spec.
        else:
                func = nil
        # dprintf("; L-M-B globals: %x, content: %s",
        #               id(globals), { k:v for k,v in globals.items() if k != '__builtins__' })
        return func, globals, dict(globals)

# High-level drivers: %PROCESS-TOP-LEVEL, COMPILE-FILE, @LISP, COMPILE, EVAL

def process_top_level(form) -> [ast.stmt]:
        "A, hopefully, faithful implementation of CLHS 3.2.3.1."
        ## Compiler macro expansion, unless disabled by a NOTINLINE declaration, SAME MODE
        ## Macro expansion, SAME MODE
        if symbol_value(_compile_verbose_):
                complex_named_p = (listp(form) and length(form) > 1
                                   and (atom(form[1][0]) or form[1][0][0] is _setf))
                kind, maybe_name = ((form[0], form[1][0]) if complex_named_p      else
                                    (form[0], "")         if listp(form) and form else
                                    (form, ""))
                dprintf("; compiling (%s%s%s%s)",
                              kind, " " if complex_named_p else "", maybe_name, " ..." if length(form) > 2 else "")
        if symbol_value(_compiler_trace_forms_):
                dprintf(";;;%s compiling:\n%s%s",
                              sex_space(-3, ";"), sex_space(), pp(form))
        macroexpanded = compiler_macroexpand_all(form, lexenv = _null)
        if symbol_value(_compiler_trace_macroexpanded_):
                if form != macroexpanded:
                        # dprintf("  MX  %s   --->\n%s", pp_consly(form), pp_consly(macroexpanded))
                        report(macroexpanded = macroexpanded, desc = "PROCESS-TOP-LEVEL", lexenv = _null)
                else:
                        dprintf(";;;%s macroexpansion had no effect", sex_space(-3, ";"))
        ## Note, that at this point, the lexenv is discharged completely.
        ## ..is it?  Macroexpansion is done, so what could it be?
        ##
        ## Accumulation of results arranged for the run time:
        run_time_results = []
        def make_processor(skip_subforms, doc_and_decls):
                ## Unregistered Issue TOPLEVEL-PROCESSOR-IGNORES-DECLARATIONS
                def processor(compile_time_too, process, eval, *forms, toplevel = None):
                        "Dispatch top-level (by deconstruction) FORMS through REC."
                        relevant = forms[skip_subforms:]
                        body = parse_body(relevant)[0]
                        for f in body:
                                rec(compile_time_too, process, eval, f)
                return processor
        def process_eval_when(compile_time_too, process, eval, _, situations, *body, toplevel = None):
                parsed_situations = parse_eval_when_situations(situations)
                new_ctt, new_process, new_eval = analyse_eval_when_situations(compile_time_too, *parsed_situations)
                for f in body:
                        rec(new_ctt, process and new_process, new_eval, f)
        def default_processor(compile_time_too, process, eval, *form, toplevel = None):
                ## Unregistered Issue TOPLEVEL-PROCESSOR-WACKY-LEXENV-ARGUMENT-HANDLING
                ## This is where LEXENV isn't true, as it's always empty.
                ## ..but re-walking it.. who would care?  CLtL2 environments?
                ## Additional note: this is %PROCESS, split in half, due to cases.
                form = consify_linear(form)
                if process or eval:
                        ## Note, how lie wrt. the NULL lexenv -- what about {SYMBOL-,}MACROLET?  See above..
                        stmts, *unit_data = with_symbol_unit_magic(lambda: lower(form, lexenv = _null),
                                                                    id = "PROCESS-TOPLEVEL-")
                if process:
                        if toplevel and symbol_value(_compiler_trace_toplevels_):
                                report(known = form, ast = stmts, desc = "processed TLF")
                        run_time_results.extend(stmts)
                        compilation_unit_adjoin_symbols(*unit_data)
                if eval:
                        if toplevel and symbol_value(_compiler_trace_compile_time_eval_):
                                report(known = form, ast = stmts, desc = "CT eval")
                        bytecode = assemble(compilation_unit_prologue(*unit_data)
                                             + stmts,
                                             form)
                        # dprintf(";; ..compile-time code object execution")
                        _, broken_globals, good_globals = load_module_bytecode(bytecode)
                        ## Critical Issue NOW-WTF-IS-THIS-SHIT?!
                        broken_globals.update(good_globals)
                        # dprintf("; D-P: globals: %x, content: %s",
                        #               id(globals), { k:v for k,v in globals.items() if k != '__builtins__' })
        actions = {
                _progn:           make_processor(skip_subforms = 1, doc_and_decls = nil),
                _locally:         make_processor(skip_subforms = 1, doc_and_decls = t),
                _macrolet:        make_processor(skip_subforms = 2, doc_and_decls = t),
                _symbol_macrolet: make_processor(skip_subforms = 2, doc_and_decls = t),
                _eval_when:       process_eval_when,
                }
        def rec(compile_time_too, process, eval, form):
                ## Unregistered Issue TOPLEVEL-PROCESSOR-WACKY-LEXENV-HANDLING
                if not consp(form):
                        return
                actions.get(form[0], default_processor)(compile_time_too, process, eval, *vectorise_linear(form),
                                                        toplevel = t)
        rec(nil, t, nil, macroexpanded)
        return run_time_results

string_set("*COMPILE-PRINT*",         t)
string_set("*COMPILE-VERBOSE*",       t)

string_set("*COMPILE-FILE-PATHNAME*", nil)
string_set("*COMPILE-FILE-TRUENAME*", nil)

string_set("*COMPILE-OBJECT*",          nil)
string_set("*COMPILE-TOPLEVEL-OBJECT*", nil)

def compile_file(input_file, output_file = nil, trace_file = nil, verbose = None, print = None):
        verbose = defaulted_to_var(verbose, _compile_verbose_)
        print   = defaulted_to_var(verbose, _compile_print_)
        if verbose:
                format(t, "; compiling file \"%s\" (written %s):\n", input_file, file_write_date(input_file))
        ## input/output file conformance is bad here..
        abort_p, warnings_p, failure_p = nil, nil, nil
        forms = list_(_progn)
        with py.open(input_file, "r") as input:
                def in_compilation_unit():
                        nonlocal trace_file, forms
                        if trace_file:
                                trace_filename = (trace_file if stringp(trace_file) else
                                                  input_file.replace(".lisp", "." + symbol_value(_trace_file_type_)))
                                trace_file = py.open(trace_filename, "w")
                        try:
                                stmts = []
                                form = read(input, eof_value = input, eof_error_p = nil)
                                while form is not input:
                                        forms = cons(form, forms)
                                        ## Beacon LEXENV-CLAMBDA-IS-NIL-HERE
                                        form_stmts = process_top_level(form)
                                        stmts.extend(form_stmts)
                                        if trace_file:
                                                trace_file.write(pp(form))
                                                for stmt in form_stmts:
                                                        trace_file.write(str(stmt))
                                                        trace_file.write("\n")
                                        form = read(input, eof_value = input, eof_error_p = nil)
                                return (compilation_unit_prologue(*compilation_unit_symbols()) +
                                        stmts)
                        finally:
                                if trace_file:
                                        trace_file.close()
                stmts = with_compilation_unit(in_compilation_unit,
                                              ## Unregistered Issue POSSIBLE-COMPILATION-UNIT-USE-VIOLATION-HERE
                                              override = t, id = "COMPILE-FILE-")
        output_file = output_file or input_file.replace(".lisp", "." + symbol_value(_fasl_file_type_))
        try:
                with py.open(output_file, "wb") as f:
                        f.write(symbol_value(_fasl_file_magic_))
                        bytecode = assemble(stmts, nreverse(forms))
                        marshal.dump(bytecode, f)
                        return output_file
        finally:
                verbose and format(t, "; %s written\n", output_file)

def read_function_as_toplevel_compile_and_load(body):
        ## What should it be like?
        ##  - COMPILE-FILE + LOAD
        ##  - COMPILE-TOPLEVEL-FORM + ?
        name, form = read_python_toplevel_as_lisp(body)
        bytecode = process_as_loadable(process_top_level, form, id = "LISP-")
        function, bad_gls, good_gls = load_module_bytecode(bytecode, func_name = name, filename = "<lisp core>")
        bad_gls.update(good_gls)      ## Critical Issue NOW-WTF-IS-THIS-SHIT?!
        return function.name

def lisp(function):
        return read_function_as_toplevel_compile_and_load(function)

def compile_in_lexenv(lambda_expression, lexenv = nil, name = None, globalp = None, global_macro_p = None):
        if lexenv and globalp:
                error("In %%COMPILE-IN-LEXENV: the provided non-NULL lexenv conflicts with GLOBALP.")
        form = ir(*vectorise_linear(lambda_expression),
                   name = name,
                   **({ "decorators": list_(ir_cl_call("set_macro_definition",
                                                        ir_apply("globals"), name, lambda_expression)
                                            if global_macro_p else
                                            ir_cl_call("set_function_definition",
                                                        ir_apply("globals"), name, lambda_expression))}
                      if globalp else {}))
        bytecode = process_as_loadable(process_to_ast, form, lexenv = lexenv, id = "COMPILED-LAMBDA-")
        function, bad_gls, good_gls = load_module_bytecode(bytecode, func_name = name, filename = "<lisp core>")
        bad_gls.update(good_gls)      ## Critical Issue NOW-WTF-IS-THIS-SHIT?!
        ## Doesn't this make %READ-FUNCTION-AS-TOPLEVEL-COMPILE-AND-LOAD somewhat of an excess?
        return the(cold_function_type, function)

##

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
        return values(*(gethash(slot, the(function_t, function).__dict__, default)[0]
                        for slot, default in [("lambda_expression", nil),
                                              ("closure_p",         t),
                                              ("name",              nil)]))

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
                lambda_expression = the((cons_t, (eql_t, _lambda), cons_t), definition)
        else:
                fun = definition or macro_function(name) or fdefinition(name)
                _, lambda_expression, _, _ = function_lambda_expression(fun)
                if not definition:
                        # Not much we can do, but return the original function.
                        return fun, nil, nil, nil
        final_name = the(symbol_t, name) or gensym("COMPILED-LAMBDA-")
        # Must has a name, for two reasons:
        #  - ast_compiled_name() requires one
        #  - THERE-EXIST lambdas non-expressible in Python
        # Coerce the lambda to a named def, for ast_compiled_name purposes:
        return compile_in_lexenv(lambda_expression,
                                 lexenv         = the_null_lexenv(),
                                 name           = final_name,
                                 globalp        = not not name,
                                 global_macro_p = name and not not macro_function(name))

def eval(form):
        return compile(nil, list_(_lambda, nil, form))()

def dbgsetup(**keys):
        compiler_dbgconf(pretty_full = t,
                         **keys)

def run_tests_compiler():
        def evaltest(name, form, expected, **keys):
                return runtest((name, eval), form, expected, printer = pp_consly, tabstop = 55, **keys)
        dbgsetup( # forms = t,

                  # subexpansion = t,
                  # macroexpanded = t,

                  # subrewriting = t,
                  # rewritten = t,

                  # subprimitivisation = t,
                  # primitives = t,

                  # compiler_validate_ast = t,
                  # subastification = t,
                  # ast = t,
                  # module_ast = t,

                  # bytecodes = t
                  )
        l = list_
        __cons, _identity, _values = [ intern(x)[0]
                                       for x in ["CONS", "IDENTITY", "VALUES"]]
        __cdr = make_keyword("CDR")
        _cadr, __cadr = intern("CADR")[0], make_keyword("CADR")
        _cddr, __cddr = intern("CDDR")[0], make_keyword("CDDR")
        assert evaltest("CONST-NIL",                nil,                       nil)
        assert evaltest("CONST-T",                  t,                         t)
        assert evaltest("CONST-42",                 42,                        42)
        assert evaltest("CONST-FOO",                "foo",                    "foo")
        assert evaltest("QUOTE-NONCONSTANT-SYMBOL", l(_quote, _car),           _car)
        assert evaltest("QUOTE-CONSTANT",           l(_quote, 42),             42)
        assert evaltest("QUOTE-SEX",                l(_quote, l(_car, 42)),    l(_car, 42))
        assert evaltest("APPLY-SIMPLE",             l(_list, 3.14, "a", 42),   l(3.14, "a", 42))
        #### TODO: QUOTE-UNPRIMITIVISABLE-ERROR-CASE
        ## SETQ/REF/PROGN
        assert evaltest("EMPTY-PROGN",                   l(_progn),                                nil)
        assert evaltest("SIMPLE-PROGN",                  l(_progn, 1, 0),                          0)
        assert evaltest("SETQ-SIMPLE/REF",               l(_progn,
                                                           l(_setq, _car, 42),
                                                           _car),                                  42)
        assert evaltest("SETQ-COMPLEX/REF/APPLY-SIMPLE", l(_progn,
                                                           l(_setq, _car, 42,
                                                             _cdr, 3.14),
                                                           l(_list, _car, _cdr)),                  l(42, 3.14))
        assert evaltest("SETQ/REF-PYREF",                l(_progn,
                                                           l(_setq, l(_quote, l("foo")), "bar"),
                                                           l(_ref, l(_quote, l("foo")))),         "bar")
        assert evaltest("LET/SETQ/REF-LEXICAL",          l(_progn,
                                                           l(_setq, _car, "bar"),
                                                           l(_let, l(_car,
                                                                     l(_cdr, 42),
                                                                     l(_let, 42)),
                                                             l(_setq, _let, 21264),
                                                             l(_list, _car, _cdr, _let))),         l(nil, 42, 21264))
        ## IF
        assert evaltest("IF-TRUE",
                       l(_if, t, t, nil),   t)
        assert evaltest("IF-FALSE",
                       l(_if, nil, nil, t), t)
        assert evaltest("IF-FALSE-SINGLE-BRANCH",
                       l(_if, nil, t),      nil)
        ## LET
        assert evaltest("LET-EMPTY",
                        l(_let, nil),
                        nil)
        assert evaltest("LET-NO-BINDINGS",
                        l(_let, nil, 0),
                        0)
        assert evaltest("LET-NO-BODY",
                        l(_progn,
                          l(_list,
                            l(_let, l(l(_car, l(_setq, _cdr, 42)))),
                            _cdr)),
                        l(nil, 42))
        ## PRIM, IR-ARGS
        assert evaltest("PRIM-DEF-CALL",
                        l(_progn,
                          l(_ir_args,
                            l(_prim, p.lambda_, ([], [], [], None, [], [], None),
                                     p.integer(42)),
                            ["name", p.name("foo")]),
                          l(_prim, p.funcall, p.name("foo"))),
                        42)
        ## FUNCTION
        assert evaltest("FUNCTION/PYREF",
                        l(_function, l(_quote, l("cl", "list_"))),
                        list_)
        ## LAMBDA
        assert evaltest("LAMBDA-SIMPLE-CALL",
                        l(l(_lambda, nil)),
                        nil)
        assert evaltest("LAMBDA-IDENTITY",
                        l(l(_lambda, l(_car), _car), 3.14),
                        3.14)
        assert evaltest("LAMBDA-&OPTIONAL-NIL-DEFAULTED",
                        l(l(_lambda, l(_car, _optional, _cdr), l(_list, _car, _cdr)), 3.14),
                        l(3.14, nil))
        assert evaltest("LAMBDA-&OPTIONAL-DEFAULTED",
                        l(l(_lambda, l(_car, _optional, l(_cdr, 42)), l(_list, _car, _cdr)), 3.14),
                        l(3.14, 42))
        assert evaltest("LAMBDA-&OPTIONAL-PROVIDED",
                        l(l(_lambda, l(_car, _optional, l(_cdr, 42)), l(_list, _car, _cdr)), 3.14, 2.71),
                        l(3.14, 2.71))
        assert evaltest("LAMBDA-&KEY-NIL-DEFAULTED",
                        l(l(_lambda, l(_car, _key, _cdr), l(_list, _car, _cdr)), 3.14),
                        l(3.14, nil))
        assert evaltest("LAMBDA-&KEY-DEFAULTED",
                        l(l(_lambda, l(_car, _key, l(_cdr, 42)), l(_list, _car, _cdr)), 3.14),
                        l(3.14, 42))
        assert evaltest("LAMBDA-&KEY-PROVIDED",
                        l(l(_lambda, l(_car, _key, l(_cdr, 42)), l(_list, _car, _cdr)), 3.14, __cdr, 2.71),
                        l(3.14, 2.71))
        assert evaltest("LAMBDA-&KEY-&ALLOW-OTHER-KEYS-LAMBDA-ALLOWS",
                        l(l(_lambda, l(_car, _key, _allow_other_keys), t), 3.14, __cdr, 2.71),
                        t)
        assert evaltest("LAMBDA-&KEY-:ALLOW-OTHER-KEYS-CALL-ALLOWS",
                        l(l(_lambda, l(_key), t), __cdr, 2.71, _allow_other_keys_, t),
                        t)
        assert evaltest("LAMBDA-&OPTIONAL-&KEY-PROVIDED",
                        l(l(_lambda, l(_car, _optional, _cdr, _key, l(_cadr, 42)), l(_list, _car, _cdr, _cadr)),
                          3.14, 42, __cadr, 2.71),
                        l(3.14, 42, 2.71))
        assert evaltest("LAMBDA-&REST-NIL",
                        l(l(_lambda, l(_car, _rest, _cdr), l(__cons, _car, _cdr)), 2.71),
                        l(2.71))
        assert evaltest("LAMBDA-&REST-SOMETHING",
                        l(l(_lambda, l(_rest, _car), _car), 2.71, 3.14, 42),
                        l(2.71, 3.14, 42))
        assert evaltest("LAMBDA-&REST-&KEY-PROVIDED",
                        l(l(_lambda, l(_car, _rest, _cdr, _key, l(_cadr, 42), l(_cddr, 123)),
                            l(_list, _car, _cdr, _cadr, _cddr)),
                          3.14, __cddr, 2.71),
                        l(3.14, l(__cddr, 2.71), 42, 2.71))
        ## APPLY
        assert evaltest("APPLY-PFUNCALL",
                        l(_apply, l(_function, _identity), 1, l(_quote, nil)),
                        1)
        assert evaltest("APPLY-PAPPLY",
                        l(_apply, l(_function, _identity), l(_list, 1)),
                        1)
        ## MULTIPLE-VALUE-CALL
        assert evaltest("MULTIPLE-VALUE-CALL-SIMPLE",
                        l(_multiple_value_call, l(_function, _list), l(_values, 1, 2, 3)),
                        l(1, 2, 3))
        # dbgsetup( # forms = t,
        #           # macroexpanded = t,
        #           # rewritten = t,
        #           primitives = t,
        #           # module_ast = t,
        #           )
        # assert evaltest("MULTIPLE-VALUE-CALL-COMPLEX",
        #                 l(_multiple_value_call, l(_function, _list), l(_values, 1, 2), 3, l(_values, 4, 5)),
        #                 l(1, 2, 3, 4, 5))
        ## CATCH
        # assert evaltest("",
        #                 l(),
        #                 )
        ## THROW
        # assert evaltest("",
        #                 l(),
        #                 )
        ## UNWIND-PROTECT
        # assert evaltest("",
        #                 l(),
        #                 )
        ## NTH-VALUE
        # assert evaltest("",
        #                 l(),
        #                 )
        ## PROGV
        # assert evaltest("",
        #                 l(),
        #                 )
        ## PROTOLOOP
        # assert evaltest("",
        #                 l(),
        #                 )
        ## THE
        ## LOCALLY
        ## MULTIPLE-VALUE-PROG1
        ## LOAD-TIME-VALUE
        ####
        ## FUNCALL
        # assert evaltest("FUNCALL-PRIM",
        #                 l(_progn,
        #                   l(_ir_args,
        #                     l(_prim, p.lambda_, ([], [], [], None, [], [], None),
        #                              p.integer(42)),
        #                     ["name", p.name("bar")]),
        #                   l(_funcall, l(_function, l(_quote, l("bar"))))),
        #                 42)
        ## LET*
        # assert evaltest("",
        #                 l(),
        #                 )
        ## FLET
        # assert evaltest("",
        #                 l(),
        #                 )
        ## LABELS
        # assert evaltest("",
        #                 l(),
        #                 )
        ## MACROLET
        # assert evaltest("",
        #                 l(),
        #                 )
        ## SYMBOL-MACROLET
        # assert evaltest("",
        #                 l(),
        #                 )
        ## BLOCK/RETURN-FROM
        # assert evaltest("",
        #                 l(),
        #                 )
        ## TAGBODY/GO
        # assert evaltest("",
        #                 l(),
        #                 )
        ## EVAL-WHEN
        # assert evaltest("",
        #                 l(),
        #                 )
        ## if-true, if-false, if-false-one-handed,

if getenv("CL_RUN_TESTS") != "nil" and getenv("CL_TEST_COMPILER") != "nil":
        with matcher_pp_stack():
                run_tests_compiler()
        exit()

# Auxiliary: FDEFINITION

################################################################################

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
__def_sources__ = without_condition_system(lambda: collections.OrderedDict())
__def_sources__[""] = "" # placeholder
__def_sources_filename__ = "<lisp>"
def lisp_add_def(name, source):
        if name in __def_sources__:
                del __def_sources__[name]
        __def_sources__[name] = source
        total = "\n".join(__def_sources__.values())
        linecache.cache[__def_sources_filename__] = len(total), int(time.time()), total.split("\n"), __def_sources_filename__

def peek_func_globals(x, desc = "FUNC"):
        func = the(function_t, (x if functionp(x) else
                              symbol_function(x) or macro_function(x)))
        dprintf("\n  %s %s (0x%x): globals %x (of type %s)\n%s",
                      desc, func.__name__, id(func),
                      id(func.__globals__), type_of(func.__globals__),
                      { k:v for k,v in func.__globals__.items() if k != '__builtins__' })
        backtrace(15, frame_ids = t, offset = 1)

@defun
def fdefinition(name):
        ## DEFMACRO expands into this (DEFUN should too)
        return symbol_function(the(symbol_t, name))

# LOAD (+ stray stream_type_error)

string_set("*LOAD-VERBOSE*", t)
string_set("*LOAD-PRINT*", nil)
string_set("*SOURCE-INFO*", nil)

def load_as_source(stream, verbose = nil, print = nil):
        ## This is botched.
        pathname = file_stream_name(stream)
        verbose and format(t, "; loading %s\n", repr(pathname))
        def with_abort_restart_body():
                def eval_form(form, index):
                        spref = "; evaluating "
                        print and format(t, spref + "%s\n", pp(form, initial_depth = len(spref)))
                        def with_continue_restart_body():
                                while t:
                                        def with_retry_restart_body():
                                                results = eval(form)
                                                results = ((results,) if not values_frame_p(results) else
                                                           values_frame_values(results))
                                                print and format(t, "%s\n", ", ".join(repr(x) for x in results))
                                        return with_simple_restart("RETRY", ("Retry EVAL of current toplevel form.",),
                                                                   with_retry_restart_body)
                        with_simple_restart("CONTINUE", ("Ignore error and continue loading file %s.", repr(pathname)),
                                            with_continue_restart_body)
                ## Unregistered Issue DEBUG-LOAD-FILE-SOURCE-INFO-IGNORED
                def next(): return read(stream, eof_error_p = nil, eof_value = stream)
                form = next()
                if pathname:
                        with progv({ _source_info_: nil # source_info_type(pathname = pathname)
                                     }):
                                while form != stream:
                                        eval_form(form, nil)
                                        form = next()
                else:
                        with progv({ _source_info_: nil }):
                                while form != stream:
                                        eval_form(form, nil)
                                        form = next()
        return with_simple_restart("ABORT", ("Abort loading file %s.", file_stream_name(stream)),
                                   with_abort_restart_body)

string_set("*LOAD-PATHNAME*", nil)
string_set("*LOAD-TRUENAME*", nil)

string_set("*FASL-FILE-TYPE*",  "vpfas")
string_set("*TRACE-FILE-TYPE*", "trace")
string_set("*FASL-FILE-MAGIC*", ";VPCL FAS\n".encode("utf-8"))

def fasl_header_p(stream, errorp = nil):
        magic = stream.read(10)
        if magic == symbol_value(_fasl_file_magic_):
                return t
        if errorp:
                error("The file pointed at by stream %s does not contain a FASL file.", stream)
        return nil

def load_as_fasl(stream, verbose = None, print = None):
        ## The stream is expected to have been seeked past the magic.
        verbose = defaulted_to_var(verbose, _load_verbose_)
        print   = defaulted_to_var(verbose, _load_print_)
        filename = truename(stream)
        verbose and format(t, "; loading %s...\n", filename)
        bytecode = marshal.load(stream)
        _, broken_globals, good_globals = load_module_bytecode(bytecode, filename = filename)
        ## Critical Issue NOW-WTF-IS-THIS-SHIT?!
        broken_globals.update(good_globals)

@cold_defun_with_block
def load(pathspec, verbose = None, print = None,
         if_does_not_exist = t,
         external_format = make_keyword("default")):
        verbose = defaulted_to_var(verbose, _load_verbose_)
        print   = defaulted_to_var(verbose, _load_print_)
        def load_stream(stream, faslp):
                with progv({ _readtable_:     symbol_value(_readtable_),
                             _package_:       symbol_value(_package_),
                             _load_pathname_: pathname(stream),
                             _load_truename_: handler_case(lambda: truename(stream),
                                                           (error_t, lambda _: nil)) }):
                        return_from(load, (load_as_fasl if faslp else
                                           load_as_source)(stream, verbose = verbose, print = print))
        ## Case 1: stream.
        if streamp(pathspec):
                return load_stream(pathspec, fasl_header_p(pathspec))
        pathname_ = pathname(pathspec)
        ## Case 2: Open as binary, try to process as a fasl.
        def with_open_stream_body(stream):
                if not stream:
                        return_from(load, nil)
                real = probe_file(stream)
                should_be_fasl_p = real and string_equal(pathname_type(real), symbol_value(_fasl_file_type_))
                if ((should_be_fasl_p or file_length(stream)) and
                    fasl_header_p(stream, errorp = should_be_fasl_p)):
                        return_from(load, load_stream(stream, t))
        def typeless_pathname_branch():
                nonlocal pathname_
                defaulted_pathname = probe_load_defaults(pathspec)
                if defaulted_pathname:
                        pathname_ = defaulted_pathname
                        return open(pathname_, if_does_not_exist = (keyword("ERROR") if if_does_not_exist else
                                                                    nil),
                                    element_type = (unsigned_byte_t, 8))
        with_open_stream(((pathspec                 if streamp(pathspec)                          else
                           py.open(pathspec, "rb") if stringp(pathspec) and probe_file(pathspec) else
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
class stream_type_error_t(simple_condition_t, io.UnsupportedOperation):
        pass

# LOAD-able things

#     Cold boot complete, now we can LOAD vpcl.lisp.

def configure_recursion_limit(new_limit):
        dprintf("; current recursion limit is: %s;  setting it to %s",
                      sys.getrecursionlimit(), new_limit)
        sys.setrecursionlimit(new_limit)

configure_recursion_limit(262144)
dbgsetup()

# compiler_trap_function(intern("DEFPACKAGE")[0])

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

def fx():
        dprintf("  fname: %s", caller_frame(11).f_code.co_name)
        dprintf(" locals: %s", caller_frame(11).f_locals)
        dprintf("  glsid: %x", id(caller_frame(11).f_globals))
        dprintf("   coid: %x", id(caller_frame(11).f_code))
        dprintf("DEFUN's: %x", id(find_symbol("DEFUN")[0].macro_function.__code__))

# compiler_trap_function(intern("DEFUN")[0])

intern_and_bind_names_in_module_specifically(
        ("_a", "A"),
        ("_b", "B"),
        ("_c", "C"),
        ("_d", "D"),
        ("_e", "E"),
        ("_f", "F"),
        ("_g", "G"),
        ("_h", "H"),
        ("_i", "I"),
        ("_j", "J"),
        )
__running_tests__ = True
# __enable_matcher_tracing__ = True
if not getenv("CL_NO_LISP"):
        with disabled_condition_system():
                import cProfile, pstats
        def compile_vpcl():
                return compile_file("vpcl.lisp")
        def compile_reader():
                # global __enable_matcher_tracing__
                # __enable_matcher_tracing__ = True
                return compile_file("reader.lisp")
        ## Critical Issue EXTREME-INEFFICIENCY-OF-MATCHER
        # def slow_match():
        #         ## 0:15, 1:53, 2:221, 3:1115
        #         ## 0:11, 1:24, 2:49,  3:98
        #         ## 0:10, 1:16, 2:22, 3:28
        #         exp = list_(_lambda, list_(set(),))
        #         with matcher_pp_stack():
        #                 ret = match_sex(exp, list_(form), matcher = metasex)
        #                 dprintf(";;;\n;;; match calls: %d\n;;; expr: %s\n;;;", metasex.match_calls, pp_consly(exp))
        #                 return ret
        #         return pp_sex(exp)
        dbgsetup( # forms = t,

                  # subexpansion = t,
                  # macroexpanded = t,

                  # subrewriting = t,
                  # rewritten = t,

                  # subprimitivisation = t,
                  # primitives = t,

                  # compiler_validate_ast = t,
                  # subastification = t,
                  # ast = t,
                  # module_ast = t,

                  # bytecodes = t
                  )
        load(compile_file("vpcl.lisp"))
        cProfile.runctx("result = compile_reader()", globals(), locals(),
                         sort = "time"
                         # sort = "cumulative"
                         )
        # dprintf("result:\n%s", result)
        # load(compile_file("reader.lisp"))
        load(result)
        exit(1)
        # load(compile_file("reader.lisp"))

# load(compile_file("reader.lisp"))

# load("vpcl.lisp", verbose = t)

# REQUIRE

string_set("*MODULE-PROVIDER-FUNCTIONS*", [])

def module_filename(module):
        return "%s/%s.py" % (env.partus_path, module if stringp(module) else symbol_name(module))

def require(name, pathnames = None):
        "XXX: not terribly compliant either"
        namestring = name if stringp(name) else symbol_name(name)
        filename = pathnames[0] if pathnames else module_filename(namestring)
        if probe_file(filename):
                not_implemented()
        else:
                error("Don't know how to REQUIRE %s.", namestring.upper())

# Environment

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
def machine_type():                return without_condition_system(lambda: platform.machine(),
                                                                    reason = "platform.machine")
def machine_version():             return "Unknown"

# DESCRIBE

# def get_info_value(name, type, env_list = None):
#         def lookup(env_list):
# def info(class, type, name, env_list = None):
#         info = type_info_or_lose(class, type)
#         return get_info_value(name, type_info_number(info), env_list)
def describe_function(name, function, stream):
        name = function.__name__ if function else name
        if not (function or (name and fboundp(name))):
                format(stream, "%s names an undefined function.\n", name)
        else:
                if not function and special_operator_p(name):
                        fun, what, lambda_list = symbol_value(name), "a special operator", not_implemented()
                elif not function and macro_function(name):
                        fun, what, lambda_list = macro_function(name), "a macro", not_implemented()
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
                        if specifiedp(lambda_list):
                                describe_lambda_list(lambda_list)
                        # describe_documentation(name, intern("FUNCTION")[0], stream)
                        if specifiedp(methods):
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

def describe_class(name, class_, stream):
        pass

def describe_python_object(x, stream):
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
                        return print_symbol(o)
                else:
                        return "#<python object %s>" % (o.__repr__(),)
        def object_type_string(o):
                return type(o).__name__
        def print_standard_describe_header(o, stream):
                format(stream, "%s\n  [%s]\n",
                       object_self_string(o), object_type_string(o))
        def describe_symbol(o, stream):
                ### variable of some kind -- see the variable_kind() stub
                # var_kind = info(keyword("variable"), make_keyword("kind"), o)
                # var_kind = variable_kind(o)
                describe_function(o, nil, stream)
                describe_class(o, nil, stream)
                ### type specifier
                # type_kind = info(keyword("type"), make_keyword("kind"), o)
                # type_kind = ???
                ## defined   - expander
                ## primitive - translator
                ### optimisation policy
                ### properties
                if not (fboundp(o) or symbol_type_specifier_p(o)):
                        describe_python_object(o, stream)
        print_standard_describe_header(o, stream)
        if symbolp(o): describe_symbol(o, stream)
        else:
                describe_python_object(o, stream)

def describe(object, stream_designator = None):
        "Print a description of OBJECT to STREAM-DESIGNATOR."
        stream_designator = defaulted(stream_designator, symbol_value(_standard_output_))
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
        not_implemented("check of validity of INITARGS")
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
                here("args: %s", initargs)
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
                standard_method_shared_initialize(self, **initargs)
        def __call__(self, gfun_args, next_methods):
                return self.function(gfun_args, next_methods)

@defclass
class standard_generic_function_t(generic_function_t):
        def __init__(self, **initargs):
                super().__init__(**initargs)
                standard_generic_function_shared_initialize(self, **initargs)
        # def __call__ ..is installed during EMF computation, with the proper arglist.

def update_generic_function_and_dependents(generic_function, **initargs):
        set_funcallable_instance_function(generic_function,
                                          compute_discriminating_function(generic_function))
        map_dependents(generic_function,
                       lambda dep: update_dependent(generic_function, dep, **initargs))

def standard_generic_function_shared_initialize(generic_function,
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
        if specifiedp(argument_precedence_order):
                if not specifiedp(lambda_list):
                        error("MAKE-INSTANCE STANDARD-GENERIC-FUNCTION: :ARGUMENT-PRECEDENCE-ORDER "
                              "was provided, but :LAMBDA-LIST was not.")
                elif not (listp(argument_precedence_order) and
                          set(argument_precedence_order) == set(lambda_list[0])):
                        error("MAKE-INSTANCE STANDARD-GENERIC-FUNCTION: :ARGUMENT-PRECEDENCE-ORDER, "
                              "when specified, must be a permutation of fixed arguments in :LAMBDA-LIST.  "
                              "Was: %s;  fixed LAMBDA-LIST args: %s.",
                              repr(argument_precedence_order), lambda_list[0])
                generic_function.argument_precedence_order = tuple(argument_precedence_order)
        elif specifiedp(lambda_list):
                generic_function.argument_precedence_order = tuple(lambda_list[0])
        generic_function.declarations        = tuple(defaulted(declarations, nil,
                                                                type = (pylist_t,
                                                                        (satisfies_t, valid_declaration_p))))
        generic_function.documentation       = defaulted(documentation, nil,
                                              type = (or_t, string_t, (eql_t, nil)))
        if specifiedp(lambda_list):
                # XXX: not_implemented("lambda-list validation")
                generic_function.lambda_list = lambda_list
        generic_function.method_combination  = defaulted(method_combination, standard_method_combination_t,
                                                          type = cold_class_type)
        generic_function.method_class        = defaulted(method_class, standard_method_t,
                                                          type = cold_class_type) # method metaclass
        generic_function.name                = defaulted(name, nil)
        # The discriminating function may reuse the
        # list of applicable methods without calling
        # COMPUTE-APPLICABLE-METHODS-USING-CLASSES again provided that:
        # (ii) the generic function has not been reinitialized,
        generic_function.__applicable_method_cache__ = make_hash_table() # (_list, type) -> list
        generic_function.__methods__ = make_hash_table()
        filename, lineno = (defaulted(filename, "<unknown>"),
                            defaulted(lineno,   0))
        update_generic_function_and_dependents(
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
def specializerp(x):       return ((x is t)        or
                                    typep(x, (or_t, type, (pytuple_t, (eql_t, eql), t))))

def get_generic_fun_info(generic_function):
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
                               (eql_t, make_keyword("description")),
                               string_t)),
                    (maybe_t, (pytuple_t,
                               (eql_t, make_keyword("order")),
                               (member_t,
                                make_keyword("most-specific-first"),
                                make_keyword("most-specific-last")))),
                    (maybe_t, (pytuple_t,
                               (eql_t, make_keyword("required")),
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
        #         posnal, named = argspec_value_varivals(inspect.getfullargspec(body), x)
        #         return body()
        method_group = poor_man_defstruct("method_group",
                                           "name",
                                           "qualifier_spec",
                                           "description",
                                           "most_specific_first",
                                           "required")
        groups = make_hash_table()
        for mgspec in method_group_specifiers:
                gname, qualifier_spec = mgspec[:2]
                options = mgspec[2:]
                options_dict = map_into_hash_star(lambda keyword, v: (symbol_name(keyword), v), options)
                (lambda description = "Method group %s.",
                        required = nil,
                        order = make_keyword("most_specific_first"):
                        groups.update({gname: method_group(gname,
                                                           qualifier_spec,
                                                           description,
                                                           order is make_keyword("most_specific_first"),
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
                               if ((listp(qualifier_spec) and
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
                       next_methods = defaulted(next_methods, [])
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
#         i("STANDARD"),
#         [(i("around"),  [(keyword("around"),)]),
#          (i("before"),  [(keyword("before"),)]),
#          (i("primary"), [tuple()],
#                          (keyword("required"), t)),
#          (i("after"),   [(keyword("after"),)],
#                          (keyword("order"),    make_keyword("most-specific-last")))],
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
        not_implemented()
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
        not_implemented()

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
        return compute_applicable_methods_using_types(generic_function,
                                                       types_from_args(generic_function,
                                                                        classes,
                                                                        class_eq_))

def types_from_args(generic_function, arguments, type_modifier = None):
        nreq, applyp, metatypes, nkeys, arg_info = get_generic_fun_info(generic_function)
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

def arg_info_precedence(arg_info: "lambda list, actually.."):
        return range(len(arg_info[0]))

def compute_applicable_methods_using_types(generic_function, types_):
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
                nreq, applyp, metatypes, nkeys, arg_info = get_generic_fun_info(generic_function)
                # (declare (ignore nreq applyp metatypes nkeys))
                precedence = arg_info_precedence(arg_info)
                return values(sort_applicable_methods(precedence,
                                                      reversed(possibly_applicable_methods),
                                                      types),
                              definite_p)

def type_from_specializer(specl):
        if specl is t:
                return t
        elif isinstance(specl, tuple):
                if not member(car(specl), [class_, class_eq, eql]): # protoype_
                        error("%s is not a legal specializer type.", specl)
                return specl
        elif specializerp(specl): # Was a little bit more involved.
                return specializer_type(specl)
        else:
                error("%s is neither a type nor a specializer.", specl)

def specializer_applicable_using_type_p(specl, type):
        specl = type_from_specializer(specl)
        if specl is t:
                return values(t, t)
        ## This is used by C-A-M-U-T and GENERATE-DISCRIMINATION-NET-INTERNAL,
        ## and has only what they need.
        return ((nil, t) if atom(type) or car(type) is t else
                poor_man_case(car(type),
                               # (and    (saut-and specl type)),
                               # (not    (saut-not specl type)),
                               # (class_,     saut_class(specl, type)),
                               # (prototype  (saut-prototype specl type)),
                               (class_eq,   lambda: saut_class_eq(specl, type)),
                               # (class-eq   (saut-class-eq specl type)),
                               # (eql    (saut-eql specl type)),
                               (t,       lambda: error("%s cannot handle the second argument %s.",
                                                       "specializer-applicable-using-type-p",
                                                       type))))

def saut_class_eq(specl, type):
       if car(specl) is eql:
               return (nil, type_of(specl[1]) is type[1])
       else:
               pred = poor_man_case(car(specl),
                                     (class_eq, lambda: specl[1] is type[1]),
                                     (class_,   lambda: (specl[1] is type[1] or
                                                         memq(specl[1], cpl_or_nil(type[1])))))
               return (pred, pred)

def sort_applicable_methods(precedence, methods, types):
        def sorter(class1, class2, index):
                class_ = types[index] # Was: (type-class (nth index types))
                cpl = class_.__mro__  # Was: ..dependent on boot state
                return (class1 if memq(class2, memq(class1, cpl)) else # XXX: our MEMQ is horribly inefficient!
                        class2)
        return sort_methods(methods,
                             precedence,
                             sorter)

def sort_methods(methods, precedence, compare_classes_function):
        def sorter(method1, method2):
                for index in precedence:
                        specl1 = nth(index, method_specializers(method1)) # XXX: Was (if (listp method1) ..)
                        specl2 = nth(index, method_specializers(method2)) # XXX: Was (if (listp method2) ..)
                        order  = order_specializers(specl1, specl2, index, compare_classes_function)
                        if order:
                                return order is specl1
        return stable_sort(methods, sorter)

def order_specializers(specl1, specl2, index, compare_classes_function):
        type1 = specializer_type(specl1) # Was: (if (eq **boot-state** 'complete) ..)
        type2 = specializer_type(specl2) # Was: (if (eq **boot-state** 'complete) ..)
        return ([]     if specl1 is specl1 else
                specl2 if atom(type1)      else # is t?
                specl1 if atom(type2)      else # is t?
                poor_man_case(car(type1),
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
                     (eql,  lambda: poor_man_case(car(type2),
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
        return values_project(0, compute_applicable_methods_using_types(generic_function,
                                                                        types_from_args(generic_function,
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
                                     ast,            # module
                                     sys.stdin,      # _io.TextIOWrapper
                                     car.__code__,   # code object
                                     this_frame(),   # frame
                                     ] ])

def class_sealed_p(x):
        return x in __sealed_classes__

## A sealed metaclass?
def seal_class(x):
        not_implemented()
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
                unsealed_classes = set(x for x in dispatch_arg_types if not class_sealed_p(x))
                applicable_method_cache_key = dispatch_arg_types + reduce(lambda acc, x: acc + x.__mro__,
                                                                          sorted(unsealed_classes, key = lambda type: type.__name__),
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
                here("gf: %s, ll: %s", generic_function, generic_function.lambda_list)
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
        new_dfun_ast = ast_functiondef(
            symbol_name(function_name),
            lambda_list,
            # How do we access methods themselves?
            [_ast_return(
                 ast_funcall(ast_funcall("compute_effective_method",
                                           [_ast_name(symbol_name(function_name)),
                                            None, # method combination
                                            ast_funcall("dfun_compute_applicable_methods",
                                                         [_ast_name(symbol_name(function_name)),
                                                          [ ast_name(x) for x in fixed ]])]),
                              [ ast_name(x) for x in fixed + [ x[0] for x in optional ] ],
                              map_into_hash_star(lambda key, default: (key, ast_name(default)),
                                                   keyword),
                              starargs = ast_name(args) if args else None,
                              kwargs   = ast_name(keys) if keys else None))])
        if t:
                import more_ast # Shall we concede, and import it all?
                format(t, "; generic function '%s':\n%s",
                       function_name, more_ast.pp_ast_as_code(new_dfun_ast))
        env = dict(compute_effective_method    = compute_effective_method,
                       find_symbol_or_fail            = find_symbol_or_fail,
                       dfun_compute_applicable_methods = dfun_compute_applicable_methods)
        return ast_compiled_name(
                    symbol_name(function_name),
                    new_dfun_ast,
                    filename = "" # defaulted(filename, "")
                    ,
                    lineno   = 0 # lineno
                    ,
                    function = function_name(function_name),
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
                mapc(curry(remove_method, generic_function),
                     generic_function_methods(generic_function))
        if lambda_list:
                fixed, optional, args, keyword, kwarg = lambda_list
                if any(x[1] is not None for x in list(optional) + list(keyword)):
                        error("Generic function arglist cannot specify default parameter values.")
        initargs = only_specified_keys(
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
        here("args: %s", initargs)
        ###
        ### Second step:
        if not generic_function:
                # If the GENERIC-FUNCTION argument is NIL, an instance of the class
                # specified by the :GENERIC-FUNCTION-CLASS argument is created by
                # calling MAKE-INSTANCE with the previously computed initialization arguments.
                # The function name FUNCTION-NAME is set to name the generic function.
                generic_function = make_instance(generic_function_class_t, **initargs)
                # standard_generic_function_shared_initialize is called by s-g-f.__init__
                frost.setf_global(generic_function, function_name, globals = defaulted(globals, py.globals()))
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
                standard_generic_function_shared_initialize(generic_function, **initargs)
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
        maybe_gfun = defaulted(global_(the(symbol_t, function_name),
                                       defaulted(globals, py.globals())),
                                nil)
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
                _, sym, __ = interpret_toplevel_value(fn, functionp)
                return ensure_generic_function(sym,
                                               argument_precedence_order = argument_precedence_order,
                                               documentation             = fn.__doc__,
                                               method_combination        = method_combination,
                                               generic_function_class    = generic_function_class,
                                               lambda_list               = function_lambda_list(fn),
                                               method_class              = method_class,
                                               #
                                               filename      = fn.__code__.co_filename,
                                               lineno        = fn.__code__.co_firstlineno)
        return do_defgeneric

def method_agrees_with_qualifiers_specializers(method, qualifiers, specializers):
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
                         or (listp(ms) and listp(s)
                             and len(ms) == len(s) == 2
                             and ms[0] == s[0] == eql
                             and eql(ms[1], s[1])))
                        for m, ms in
                        zip(method_specializers(method), specializers))
                and equal(method_qualifiers(method), qualifiers))

def generic_function_lambda_list_incongruent_with_method_list_p(generic_function_lambda_list,
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
                (xorf(gf_args, m_args) and
                 "but the method and generic function differ in whether they accept &REST or &KEY arguments") or
                # XXX: #3 compliance -- still looks fishy
                (xorf(gf_keyword or gf_keys,
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
        congruence_error = generic_function_lambda_list_incongruent_with_method_list_p(
                generic_function_lambda_list(generic_function),
                method_lambda_list(method))
        if congruence_error:
                error("attempt to add the method %s to the generic function %s; but %s.",
                      method, generic_function, congruence_error)
        if slot_boundp(method, "__generic_function__") and method.__generic_function__:
                error("ADD-METHOD called to add %s, when it was already attached to %s.",
                      method, method.__generic_function__)
        old_method = [ m for m in generic_function_methods(generic_function)
                       if method_agrees_with_qualifiers_specializers(m,
                                                                      method_qualifiers(method),
                                                                      method_specializers(method)) ]
        if old_method:
                remove_method(generic_function, old_method[0])
        generic_function.__methods__[method.specializers] = method
        method.__generic_function__ = generic_function
        for s in method_specializers(method):
                add_direct_method(s, method)
        update_generic_function_and_dependents(generic_function, add_method = method)
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
        not_implemented("maintain a set of backpointers from a SPECIALIZER to the set of methods specialized to it")

# METHOD metamethods

def standard_method_shared_initialize(method,
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
        method.qualifiers = defaulted(qualifiers, [],
                                       type = (pylist_t, (and_t, symbol_t, (not_t, (eql_t, nil)))))
        if not specifiedp(lambda_list):
                error("SHARED-INITIALIZE STANDARD-METHOD: :LAMBDA-LIST must be supplied.")
        # Unregistered Issue COMPLIANCE-STANDARD-METHOD-SHARED-INITIALIZE-LAMBDA-LIST-VALIDATION-NOT-IMPLEMENTED
        method.lambda_list = lambda_list
        if not specifiedp(specializers):
                error("SHARED-INITIALIZE STANDARD-METHOD: :SPECIALIZERS must be supplied.")
        # Unregistered Issue COMPLIANCE-STANDARD-METHOD-SHARED-INITIALIZE-SPECIALIZER-VALIDATION-NOT-IMPLEMENTED
        #  o  (pylist, method_specializer)
        #  o  length == len(lambda_list[0])
        method.specializers = specializers
        if not specifiedp(function):
                error("SHARED-INITIALIZE STANDARD-METHOD: :FUNCTION must be supplied.")
        method.function = function
        # Unregistered Issue COMPLIANCE-STANDARD-METHOD-SHARED-INITIALIZE-SLOT-DEFINITION-OPTION-NOT-IMPLEMENTED
        ## Later:
        # if typep(method, standard_accessor_method):
        #         if not specifiedp(slot_definition):
        #                 error("SHARED-INITIALIZE STANDARD-METHOD: :SLOT-DEFINITION must be supplied.")
        #         if not typep(slot_definition, direct_slot_definition):
        #                 error("SHARED-INITIALIZE STANDARD-METHOD: the supplied value of :SLOT-DEFINITION must be an instance of a subclass of DIRECT-SLOT-DEFINITION.")
        method.documentation = defaulted(documentation, nil,
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
        update_generic_function_and_dependents(generic_function, remove_method = method)
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
        not_implemented()

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
        generic_function, definedp = frost.global_(fn.__name__, globals())
        fixed, optional, args, keyword, keys = lambda_list = function_lambda_list(fn)
        if not definedp:
                _, sym, __ = interpret_toplevel_value(fn, functionp)
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
                specializers = tuple(make_method_specializers(
                                         [ gethash(name, method.__annotations__, t)[0]
                                           for name in fixed ])),
                function = not_implemented("somehow compile", methfun_lambda)
                **methfun_args)
        add_method(generic_function, method)
        return method

def make_method_specializers(specializers):
        def parse(name):
                return (# name                                    if specializerp(name) else
                        name                                      if name is t                   else
                                                                  # ..special-case, since T isn't a type..
                        name                                      if isinstance(name, type)      else
                                                                  # Was: ((symbolp name) `(find-class ',name))
                        poor_man_ecase(car(name),
                                        (eql,       lambda: intern_eql_specializer(name[1])),
                                        (class_eq_, lambda: class_eq_specializer(name[1]))) if isinstance(name, tuple)      else
                        ## Was: FIXME: Document CLASS-EQ specializers.
                        error("%s is not a valid parameter specializer name.", name))
        return [ parse(x) for x in specializers ]

# Init

def init():
        "Initialise the Common Lisp compatibility layer."
        init_condition_system()
        string_set("*DEFAULT-PATHNAME-DEFAULTS*", os.path.getcwd())
        return t

# Evaluation of Python as Lisp (for Partus)

def make_eval_context():
        val = None
        def set(x):
                nonlocal val
                val = x
        def get():
                return val
        return get, set
__evget__, __evset__ = make_eval_context()

__eval_source_cache__ = make_hash_table() # :: code_object -> string

def evaluated_code_source(co):
        return gethash(co, __eval_source_cache__)

def coerce_to_expr(x):
        return (x.value if isinstance(x, ast.Expr) else
                x)

def eval_python(expr_or_stmt):
        "In AST form, naturally."
        package = symbol_value(_package_)
        exprp = isinstance(the(ast.AST, expr_or_stmt), (ast.expr, ast.Expr))
        call = ast.fix_missing_locations(ast_module(
                        [_ast_import_from("cl", ["__evset__", "read_symbol"]),
                         ast_Expr(ast_funcall(ast_name("__evset__"), [_coerce_to_expr(expr_or_stmt)]))]
                        if exprp else
                        [expr_or_stmt]))
        code = handler_case(lambda: py.compile(call, "", "exec"),
                            (error_t,
                             lambda cond:
                                     error("EVAL: error while trying to compile <%s>: %s",
                                           more_ast.pp_ast_as_code(expr_or_stmt), cond)))
        if boundp(source_for_eval_):
                __eval_source_cache__[code] = symbol_value(_source_for_eval_)
        # write_line(">>> EVAL: %s" % (more_ast.pp_ast_as_code(expr),))
        exec(code, find_module(frost.lisp_symbol_name_python_name(package_name(package))).__dict__)
        values = (__evget__() if exprp else
                  ())
        return values if isinstance(values, tuple) else (values,)

def callify(form, package = None, quoted = nil):
        package = defaulted_to_var(package, package_)
        def callify_call(sym, args):
                func = function(the(symbol_t, sym))
                paramspec = inspect.getfullargspec(func)
                nfix = argspec_nfixargs(paramspec)
                here("func: %s -> %s, paramspec: %s", sym, func, paramspec)
                here("nfix: %s", nfix)
                here("args: %s", args)
                here("nkeys: %s", len(args) - nfix)
                if oddp(len(args) - nfix):
                        error("odd number of &KEY arguments")
                allow_other_keys = paramspec.varkw is not None
                fixnames, keynames = (paramspec.args[0:nfix],
                                      set(paramspec.args[nfix:] + paramspec.kwonlyargs))
                fixargs = args[0:nfix]
                keyargs = ({ lisp_symbol_python_name(k):v
                             for k, v in zip(args[nfix::2], args[nfix + 1::2]) })
                if not allow_other_keys:
                        for k in keyargs.keys():
                                if k not in keynames:
                                        error("unknown &KEY argument: %s", k)
                return ast_funcall(
                        lisp_symbol_ast(sym, package),
                        [ callify(x, package) for x in args ],
                        map_hash_table(
                               lambda k, x: (k, callify(x, package)),
                                      keyargs))
        obj2ast_xform = {
                False     : ast_name("False"),
                None      : ast_name("None"),
                True      : ast_name("True"),
                string_t  : ast_string,
                integer_t : ast_num,
                }
        if listp(form):
                if quoted or (form[0] is find_symbol("QUOTE", __cl)[0]):
                        return (ast_list([ callify(x, package, t) for x in form[1] ])
                                if listp(form[1]) else
                                callify(form[1], package, t))
                else:
                        return callify_call(form[0], form[1:])
        elif symbolp(form):
                return (ast_funcall("read_symbol", [_ast_string(form.name),
                                                      ast_string(form.package.name)])
                        if quoted or keywordp(form) else
                        lisp_symbol_ast(form, package))
        elif constantp(form):
                return obj2ast_xform[type(form)](form)
        elif form in obj2ast_xform:
                return obj2ast_xform[form]
        else:
                error("Unable to convert form %s", form)

def valid_declaration_p(x):
        return nil

# Missing stuff

# class deadline_timeout(condition)
# def with_deadline(timeout, body)

def read_sequence(sequence, stream, start = 0, end = None):
        not_implemented()

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
        not_implemented()

def setf_get(new_value, symbol, indicator, default = None):
        not_implemented()

def symbol_plist(symbol):
        not_implemented()

def setf_symbol_plist(new_value, symbol):
        not_implemented()

###
### ...
###
# Specification Issue INTERN-RELATIONSHIP-UNDERUSED
## response: sufficiently smart compilers eliminate the efficiency concern,
##           so what is left?
def intern0(x, package = None): return intern(the(str, x), package)[0]

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
