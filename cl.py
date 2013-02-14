
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

pyb = dictator(python_builtins_dictionary())

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

def validate_function_args(desc, f, args):
        argspec = inspect.getfullargspec(f)
        nfixed    = len(argspec.args)
        nrequired = nfixed - (len(argspec.defaults) if argspec.defaults else 0)
        if len(args) < nrequired:
                error("Not enough arguments for %s %s: at least %d are required, but %d were provided -- the argspec is %s, the args were %s.",
                      desc, f, nrequired, len(args), argspec, args)
        if len(args) > nfixed and not argspec.varargs:
                error("Too many arguments for %s %s: at most %d are accepted, but %d were provided -- the argspec is %s, the args were %s.",
                      desc, f, nfixed, len(args), argspec, args)

def validate_function_keys(desc, f, keys):
        argspec = inspect.getfullargspec(f)
        invalid = (nil if argspec.varkw else
                   (set(keys.keys()) - set(argspec.args) - set(argspec.kwonlyargs)))
        if invalid:
                error("Invalid arguments for %s: %s does not expect keyword arguments %s -- the argspec is %s, the keys were %s.",
                      desc, f, ", ".join("'%s'" % x for x in invalid), argspec, keys)

# Boot messaging

def fprintf(stream, format_control, *format_args):
        try:
                neutrality.do_write_string(format_control % format_args, stream)
        except UnicodeEncodeError:
                neutrality.do_write_string((format_control % format_args).encode("utf-8"), stream)

def dprintf(format_control, *format_args, trailing_newline = True):
        fprintf(sys.stderr, format_control + ("\n" if trailing_newline else ""), *format_args)

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
boot_sets = collections.defaultdict(set)

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
                boot_sets[set].add(boot)
                return linkage
        return definer

def unboot_set(set):
        for x in sorted(boot_sets[set], key = lambda x: x.name):
                if not hasattr(x, "unboot"):
                        error("In UNBOOT-SET \"%s\": %s has no 'unboot' attribute.", set, x)
                x.unboot()
        del boot_sets[set]
        # dprintf("; unbooted function set %s, remaining boot sets: %s", repr(set), ", ".join(boot_sets.keys()))

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
def stringp(x):        return t if isinstance(x, cold_string_type) else nil
@boot("symbol", lambda _, o: (isinstance(o, _cold_function_type) or
                              isinstance(o, symbol_t) and o.function))
@boot_defun ## Unregistered Issue COMPLIANCE-EVALUATION-MODEL-FUNCTIONP
def functionp(o):      return t if isinstance(o, cold_function_type) else nil

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

__global_scope__ = dict() ## To be replaced later, by VARDB.

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

def do_pyimport_symbol(symbol, globals, inmod_name = None, name_xform = frost.lisp_symbol_name_python_name, force = False):
        inmod_name = name_xform(symbol_name(symbol)) if inmod_name is None else inmod_name
        # dprintf("PYMPORT '%s'", inmod_name)
        frost.setf_global(symbol, inmod_name, globals, force = force)

def pyimport_symbol(symbol, globals = None, inmod_name = None, name_xform = frost.lisp_symbol_name_python_name, force = False):
        do_pyimport_symbol(boot_check_type(symbolp, symbol), defaulted(globals, pyb.globals()), inmod_name, name_xform, force)

def global_rtname(name):
        if name[0] != "*" != name[-1]:
                error("%%GLOBAL-RTNAME: provided symbol name \"%s\" is not valid for a global variable name.", name)
        return "_%s_" % name[1:-1].replace("-", "_").lower()

def symbol_rtname(name):
        return "_" + name.replace("%", "_").replace("&", "_").replace("-", "_").strip("_%").lower()

def intern_and_bind(*specs, globals = None, gvarp = False):
        globals = defaulted(globals, pyb.globals())
        for namespec in specs:
                rtname, name, value = ((None, namespec, None) if not isinstance(namespec, tuple) else
                                       namespec + (None,)     if len(namespec) is 2              else
                                       namespec)
                pyimport_symbol(intern(name)[0], globals, rtname, global_rtname if gvarp else symbol_rtname)
                if value is not None:
                        do_set(name, value, force_toplevel = t)

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

def do_set(name, value, force_toplevel = True):
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
        do_set(name, value, force_toplevel = force_toplevel)
        symbolicp and pyimport_symbol(name, globals = globals)
        return value

@boot("symbol", lambda _, name: find_dynamic_frame(boot_check_type(stringp, name)) and t)
@boot_defun
def boundp(symbol):
        # Unregistered Issue COMPLIANCE-BOUNDP-ACCEPTS-STRINGS
        return t if find_dynamic_frame(the(symbol_t, symbol)) else nil

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

        type_ = (type_specifier             if isinstance(type_specifier, type)                                    else
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

packages = dict()

@boot_defclass
class package_t(collections.UserDict):
        def __repr__ (self): return "#<PACKAGE \"%s\">" % self.name # Cold PRINT-UNREADABLE-OBJECT
        def __bool__(self):  return True                            # Non-false even if empty.
        def __hash__(self):  return hash(id(self))
        def __init__(self, name, use = [], nicknames = [],
                     filename = "", ignore_python = False, python_exports = True, boot = False):
                ## DEPENDENCY: USE-PACKAGE
                ## DEPENDENCY: INTERN
                def validate_requested_package_names(name, nicknames):
                        if name in packages:
                                error("Refusing to redefine package %s.", name)
                        nickname_conflicts = set(packages) & set(nicknames)
                        for n_c in nickname_conflicts:
                                p = packages[n_c]
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
                self.accessible  = dict()                        # str -> sym          ## accessible = present + inherited
                self.external    = set()                         # sym                 ## subset of accessible
              # self.internal    = accessible - external

                setup_package_usage(self, use)

                ## Hit the street.
                self.data          = self.accessible
                packages[name] = self
                for nick in nicknames:
                        packages[nick] = self

@boot("symbol", lambda _, name, **keys: package_t(name, **keys))
@boot_defun
def make_package(name, **keys):
        return package_t(name, **keys)

@boot("symbol", lambda _, x: isinstance(x, package_t))
@boot_defun
def packagep(x): return t if isinstance(x, package_t) else nil

@boot_defun
def package_name(x): return x.name

@boot_defun
def find_package(name):
        return name if packagep(name) else packages.get(name if isinstance(name, str) else symbol_name(name),
                                                        nil)

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
                  self.setf_function,
                  self.macro_function,
                  self.compiler_macro_function,
                  self.symbol_macro_expansion,
                  self.known)) = name, nil, (nil, nil, nil, nil, None, nil)
                ## Critically, the compiler must never produce two symbols with the same
                ## package and name.
                self.function_rtname      = None
                self.setf_function_rtname = None
                self.symbol_rtname        = None
        def __bool__(self):
                return self is not nil

@boot("symbol", lambda _, name, **keys: symbol_t(name))
@boot_defun
def make_symbol(name, **keys):
        return symbol_t(name, **keys)

@boot("symbol", lambda _, x: isinstance(x, symbol_t))
@boot_defun
def symbolp(x): return t if isinstance(x, symbol_t) else nil

@boot_defun
def keywordp(x): return t if isinstance(x, symbol_t) and symbol_package(x) is __keyword else nil

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
         nil.setf_function,
         nil.macro_function,
         nil.compiler_macro_function,
         nil.symbol_macro_expansion,
         nil.known) = "NIL", __cl, nil, nil, nil, nil, None, nil
        nil.symbol_rtname, nil.function_rtname, nil.setf_function_rtname = None, None, None
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
def do_in_package(name):
        string_set("*PACKAGE*", coerce_to_package(name), force_toplevel = t)
        return nil

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
                                # module.__dict__[python_name] = ?
        return t

# Package system init

def protosymbolicate(x, name, slot):
        sym, _ = do_intern(name)
        setattr(sym, slot, x)
        return sym

def init_package_system_0():
        global __packages__
        global t, T, make_symbol, make_package
        __packages__ = dict()
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
        raise __catcher_throw__(ball = ball, value = value, reenable_pytracer = t)

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
def return_from(nonce, value = nil):
        nonce = ((getattr((symbol_function(nonce) if isinstance(nonce, symbol_t) else
                           nonce), "ball", None) or
                  error("RETURN-FROM was handed a function %s, but it is not cooperating in the "
                        "__BLOCK__ nonce passing syntax.", nonce)) if isinstance(nonce, cold_function_type) else
                 ## This can mean either the @defun-ned function, or absent a function definition, the symbol itself.
                 (getattr(nonce.function, "ball", nonce))          if isinstance(nonce, symbol_t)            else
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

intern_and_bind("*DEBUG-CONDITION*", "*DEBUG-RESTARTS*", "*NESTED-DEBUG-CONDITION*", gvarp = t)

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
                 (x, type_, False))                            if isinstance(type_, type)                else
                nil                                            if type_ is t                             else
                (((not isinstance(x, type_.python_type)) and
                  (x, type_, False))                           if hasattr(type_, "python_type")          else
                 complex_type_mismatch(x, tuple([type_]))      if hasattr(type_, "type_predicate")       else
                 invalid_type_specifier_error(type_))          if isinstance(type_, symbol_t)            else
                complex_type_mismatch(x, type_)                if (isinstance(type_, tuple) and type_ and
                                                                    hasattr(type_[0], "type_predicate")) else
                invalid_type_specifier_error(type_))

@boot_defun
def typep(x, type):
        return nil if type_mismatch(x, type) else t

def deftype(type_name_or_fn, globals = None):
        def do_deftype(fn, type_name = type_name_or_fn):
                nonlocal globals
                old_global_name = (type_name_or_fn.__name__ if functionp(type_name_or_fn) else
                                   fn.__name__)
                globals = defaulted(globals, pyb.globals())
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
        return nil

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
        return (((x, type, False) if not isinstance(x, int) or x < 0 else nil)                        if len(type) is 1 else
                ((x, type, False) if not isinstance(x, int) or x < 0 or (x >= 1 << type[1]) else nil) if len(type) is 2 else
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
def make_cold_definer(definer_name, predicate, slot, preprocess):
        def cold_definer(name_or_obj):
                obj, sym, name = interpret_toplevel_value(name_or_obj, predicate)
                def do_cold_def(o):
                        setattr(sym, slot, o)
                        return preprocess(o)
                return (do_cold_def(obj) if obj                                      else
                        do_cold_def      if isinstance(name_or_obj, (str, symbol_t)) else
                        error("In %s: argument must be either satisfy %s or be a string;  was: %s.",
                              definer_name, predicate, repr(name_or_obj)))
        cold_definer.__name__ = definer_name
        return cold_definer

del boot_defun
del boot_defclass

defun            = cold_defun    = make_cold_definer("%COLD-DEFUN",    functionp, "function",    identity)
defclass         = cold_defclass = make_cold_definer("%COLD-DEFCLASS", lambda x: isinstance(x, type), "python_type", identity)
defun_with_block = cold_defun_with_block = make_cold_definer("%COLD-DEFUN-WITH-BLOCK", functionp, "function", __block__)
for fn  in __boot_defunned__:   frost.setf_global(defun(fn),     fn.__name__,  globals(), force = t)
for cls in __boot_defclassed__: frost.setf_global(defclass(cls), cls.__name__, globals(), force = t)
doit = True

# Delayed class definitions

@defclass
class nil():
        @classmethod
        def __instancecheck__(_, __): return False # This is an empty type
nil = protosymbolicate(nil, "NIL", "python_type")

@defclass
class t():
        @classmethod
        def __instancecheck__(_, __): return True  # This is the absolute sum type
t = protosymbolicate(t, "T", "python_type")

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
class warning_t(condition_t):                                              pass

@defclass
class simple_error_t(simple_condition_t, error_t):
        pass
@defclass
class package_error_t(error_t):
        pass
@defclass
class simple_package_error_t(simple_error_t, package_error_t):
        pass

@defclass
class simple_warning_t(simple_condition_t, warning_t):                     pass

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
              py.caller_name())

def warn_not_implemented(x = None):
        warn(not_implemented_warning,
              x if x is not None else
              py.caller_name())

# Rudimentary multiple values

#     The implemented version of NTH-VALUES is a soft one, which doesn't fail on values not
#     participating in the M-V frame protocol.

intern_and_bind("%MV-MARKER")

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
                self.data = dict()

def readtablep(x):     return isinstance(x, readtable_t)
def readtable_case(x): return the(readtable_t, x).case

def copy_readtable(x):
        check_type(x, readtable_t)
        new = readtable(case = readtable_case(x))
        new.dict = dict()
        return new

__standard_pprint_dispatch__ = dict()        # XXX: this is crap!
__standard_readtable__       = readtable_t() # XXX: this is crap!

intern_and_bind("*PRINT-ARRAY*", "*PRINT-BASE*", "*PRINT-CASE*", "*PRINT-CIRCLE*",
                 "*PRINT-ESCAPE*", "*PRINT-GENSYM*", "*PRINT-LENGTH*", "*PRINT-LEVEL*",
                 "*PRINT-LINES*", "*PRINT-MISER-WIDTH*", "*PRINT-PPRINT-DISPATCH*",
                 "*PRINT-PRETTY*", "*PRINT-RADIX*", "*PRINT-READABLY*", "*PRINT-RIGHT-MARGIN*",
                 "*READ-BASE*", "*READ-DEFAULT-FLOAT-FORMAT*", "*READ-EVAL*",
                 "*READ-SUPPRESS*",
                 "*READTABLE*", gvarp = t)
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
        string_set("*FEATURES*",  nil)
        string_set("*MODULES*",   nil)
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

def locals_printf(locals, *local_names):
        # Unregistered Issue NEWLINE-COMMA-SEPARATION-NOT-PRETTY
        fprintf(sys.stderr, ", ".join((("%s: %%s" % x) if isinstance(x, str) else "%s")
                                        for x in local_names) + "\n",
                 *((locals[x] if isinstance(x, str) else "\n") for x in local_names))

# Alexandria

def alist_hash_table(xs):
        return { x[0]: x[1] for x in vectorise_linear(xs) }

def hash_table_alist(xs):
        return mapcon(lambda kv: [k, [v, nil]],
                      consify_linear(the(dict, xs).items()))

def read_case_xformed(x):
        return case_xform(symbol_value(_read_case_), x)

# Pergamum 0

def if_let(x, consequent, antecedent = lambda: None):
        return consequent(x) if x else antecedent()

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
                                    ", ".join([ "%s = %s" % (k, v) for k, v in self.__dict__.items() ]))
        def __init__(self, **keys):
                self.__dict__.update(keys)

def gen(n = 1, x = "G", gen = gensym):
        if n == 0:
                error("_GEN: we are very very much against this, please stop!")
        return tuple(gen(x)
               for i in range(n))
def gensyms(**initargs):     return gen(gen = gensym,      **initargs)
def gensymnames(**initargs): return gen(gen = gensymname, **initargs)

# Testing

#         Used by quasiquotation, metasex and others.

results_ = []
def runtest(fn_spec, input, expected, printer = str, tabstop = 55,
            known_failure = nil, catch_errors = nil):
        name, fn = ((fn_spec.__name__.upper().replace("_", "-"), fn_spec) if functionp(fn_spec)         else
                    fn_spec                                               if isinstance(fn_spec, tuple) else
                    error("Test function specifier must be either a function, or a tuple of two elements, was: %s", fn_spec))
        pref   = "; %%%d" % tabstop
        caught = nil
        def handler(cond):
                nonlocal caught
                caught = t
                dprintf("EXCEPTION%s\n;  caught%s:\n%s",
                        "  (known)" if known_failure else "", " (this is normal)" if known_failure else "",
                        cond)
        dprintf(pref + "s:  ", name, trailing_newline = nil)
        result = fn(input) if not catch_errors else handler_case(lambda: fn(input),
                                                                 (Exception, handler))
        if caught:
                return known_failure
        if result != expected:
                dprintf("FAILED%s\n;  input:\n%s\n;  expected:\n%s\n;  actual:\n%s",
                        "  (known)" if known_failure else "", printer(input), printer(expected), printer(result))
        results_.append((fn, result))
        successp = result == expected
        if successp:
                dprintf("ok%s", "  (unexpected!)" if known_failure else "")
        return (successp if not known_failure else
                t)

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
        return t if ((x is y) if not isinstance(x, int) else x == y) else nil

def seek(n, iterator):
        for i in range(n):
                next(iterator, nil)

def from_(n, xs):
        iterator = iter(xs)
        for i in range(n):
                next(iterator, nil)
        for x in iterator:
                yield x

termination_marker = object()
def take(n, xs):
        iterator = iter(xs)
        for i in range(n):
                elt = next(iterator, termination_marker)
                if elt is not termination_marker:
                        yield elt

def some_fast(f, xs):
        for x in xs:
                ret = f(x)
                if ret: return ret or t
        return nil

def some_fast_2(f, xs, ys):
        for x, y in zip(xs, ys):
                ret = f(x, y)
                if ret: return ret or t
        return nil

def xorf(x, y):
        return (x or y) and not (x and y)

# Sequences

@defun
def stable_sort(xs, predicate):
        return sorted(xs, key = functools.cmp_to_key(predicate))

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

def map_into_hash_star(f, xs,
                        key_test = lambda k: k is not None,
                        value_test = lambda _: t) -> dict:
        acc = dict()
        for x in xs:
                k, v = f(*x)
                if key_test(k) and value_test(v):
                        acc[k] = v
        return acc

def map_hash_table(f, hash_table, **keys) -> dict:
        return map_into_hash_star(f, hash_table.items(), **keys)

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

# Functions

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
               name = gensym(str(symbol))
               f = list__(_lambda, lambda_list, body)
               b_or_res = compile_in_lexenv(f, name = name,
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

def symbol_macro_expander(sym, environment = None):
        ## -> (-> expansion) | None
        lexical = environment and the(lexenv_t, environment).lookup_var_kind(_symbol_macro, sym)
        expansion = (the(symbol_t, sym).symbol_macro_expansion if not lexical else
                     lexical.value[0])
        return (lambda: expansion) if expansion is not None else None

def style_warn(control, *args):
        warn(simple_style_warning_t, format_control = control, format_arguments = args)

def warn_incompatible_redefinition(x, tons, fromns):
        style_warn("%s is being redefined as a %s when it was previously defined to be a %s.", pp_consly(x), tons, fromns)

def warn_possible_redefinition(type, x):
        if x:
                style_warn("In %s: %s is being redefined.", type, pp_consly(x))

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

def full_symbol_rtname(x):
        return ("#" if not x.package              else
                "" if x.package.name == "KEYWORD" else
                x.package.name) + ":" + x.name

def interpret_function_name(x):
        return (x if isinstance(x, symbol_t) else
                (_setf, x[1][0]))

def interpreted_function_name_symbol(x):
        return (x if isinstance(x, symbol_t) else
                x[1])

def new_function_rtname(name):
        return gensymname("FUN_" + (str(name) if symbolp(name) else
                                    ("SETF_" + str(name[1][0]))) + "-")

def new_symbol_rtname(symbol):
        return gensymname("SYM_" + str(symbol) + "-")

def ensure_function_rtname(name):
        check_type(name, (or_t, symbol_t, cons_t))
        slot, sym = ("function_rtname", name) if isinstance(name, symbol_t) else ("setf_function_rtname", name[1][0])
        if getattr(sym, slot) is not None:
                # dprintf("ensure_function_rtname: slot %s, sym %s, existing name: %s", slot, sym, getattr(sym, slot))
                return getattr(sym, slot)
        setattr(sym, slot, new_function_rtname(name))
        # dprintf("ensure_function_rtname: slot %s, sym %s, new name: %s", slot, sym, getattr(sym, slot))
        return getattr(sym, slot)

def ensure_symbol_rtname(symbol):
        if the(symbol_t, symbol).symbol_rtname is not None:
                return symbol.symbol_rtname
        symbol.symbol_rtname = new_symbol_rtname(symbol)
        return symbol.symbol_rtname

def ensure_variable_rtname(x):
        return full_symbol_rtname(x)

def get_function_rtname(name):
        check_type(name, (or_t, symbol_t, cons_t))
        slot, sym = ("function_rtname", name) if isinstance(name, symbol_t) else ("setf_function_rtname", name[1][0])
        if getattr(sym, slot) is None:
                error("Function %s has no mapping to a python name.", pp_consly(name))
        return getattr(sym, slot)

def get_symbol_rtname(symbol):
        if the(symbol_t, symbol).symbol_rtname is None:
                error("Symbol %s has no mapping to a python name.", symbol)
        return symbol.symbol_rtname

def define_global_sym_for_rtname(globals, rtname, sym):
        globals["__" + rtname[:None if (rtname[:-1].endswith("_")
                                        or not rtname.endswith("_")) else
                               -1]] = sym

def set_function_definition(globals, x, lambda_expression = None, check_redefinition = nil):
        lambda_expression = defaulted(lambda_expression, [_lambda, [nil, nil]])
        identity_redef = compiler_defun(x, lambda_expression, check_redefinition = check_redefinition)
        def do_set_function_definition(function):
                if identity_redef:
                        style_warn("not identically redefining function %s", x)
                if function and not identity_redef:
                        ifname = interpret_function_name(x)
                        setfp = isinstance(ifname, tuple)
                        sym = interpreted_function_name_symbol(ifname)
                        slot = ("setf_function" if setfp else "function")
                        setattr(sym, slot, function)
                        if not setfp:
                                sym.macro_function = nil
                        function.name = x
                        ## Unregistered Issue NAMESPACE-POLLUTION-SEEMS-FRIVOLOUS
                        rtname = ensure_function_rtname(x)
                        # dprintf("S-F-D: %15s %25s %20s %15s%s <- %s", rtname, function, sym, slot, " SETF" * setfp, x)
                        frost.setf_global(function, rtname, globals, force = True)
                return function
        return do_set_function_definition

def set_macro_definition(globals, x, lambda_expression):
        identity_redef = compiler_defmacro(x, lambda_expression)
        def do_set_macro_definition(function):
                if identity_redef:
                        style_warn("not identically redefining macro %s", x)
                if not identity_redef and function:
                        x.function, x.macro_function = nil, function
                        function.name = x
                        frost.setf_global(function, ensure_function_rtname(x), globals, force = True)
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
def streamp(x):                    return t if isinstance(x, stream_t) else nil

def file_stream_p(x):              return t if isinstance(x, (_io._TextIOBase, _io._BufferedIOBase)) else nil

@defun
def with_open_stream(stream, f):
        try:
                return f(stream)
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
        return pyb.open(namestring(pathname),
                         poor_man_ecase(direction,
                                         (make_keyword("INPUT"),  lambda: "r"),
                                         (make_keyword("OUTPUT"), lambda: "w"),
                                         (make_keyword("IO"),     lambda: "rw"),
                                         (make_keyword("PROBE"),  lambda: not_implemented("direction :PROBE"))))

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
        if  streamp(destination) or destination is t:
                # XXX: python strings are immutable, so lists will serve as adjustable arrays..
                # Issue ADJUSTABLE-CHARACTER-VECTORS-NOT-IMPLEMENTED
                write_string(string, destination)
                return nil
        else:
                return string

# Earlified streaming

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
def get_output_stream_string(x):
        return x.getvalue()

@defun
def close(x):
        x.close()

@defun
def file_position(x):
        return x.seek(0, 1)

def stream_as_string(stream):
        return stream.read()

def file_as_string(filename):
        with pyb.open(filename, "r") as f:
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

system_pathname_host = (windows_host_t if platform.system() == 'Windows' else
                        unix_host_t)()

intern_and_bind("*DEFAULT-PATHNAME-DEFAULTS*", gvarp = t)

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
def pathnamep(x): return t if isinstance(x, pathname_t) else nil

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

def pp_consly(x, dispatch = dict(), str_printer = lambda x: '"' + x + '"', max_depth = 7):
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
def consp(x):       return t if isinstance(x, list) and len(x) is 2 else nil

@defun
def atom(x):        return t if not isinstance(x, list) or len(x) != 2 else nil

# RPLACA
# RPLACD

@defun
def car(x):         return x[0] if x else nil

@defun
def cdr(x):         return x[1] if x else nil

@defun
def rest(x):        return x[1] if x else nil

# CAAR
# CADR
# CDAR

@defun
def cddr(x):        return x[1][1] if x and x[1] else nil

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

@defun("LIST")
def list_(*xs):     return consify_linear(xs)

@defun("LIST*")
def list__(*xs):
        return consify_linear(itertools.islice(xs, 0, len(xs) - 1), last_cdr = xs[-1])
# LIST-LENGTH

@defun
def listp(x):       return t if x is nil or isinstance(x, list) and len(x) is 2 else nil

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
def mapc(f, xs, *xss):
        if not xss:
                if not xs:
                        return nil
                car, cdr = xs[0], xs[1]
                f(car)
                while cdr:
                        car, cdr = cdr[0], cdr[1]
                        f(car)
        else:
                if not xs or not all(xss): ## This misfires when something is illegally not a list, but a pyfalse -- e.g. 0.
                        return nil
                caar, cars = xs[0], [ x[0] for x in xss ]
                cadr, cdrs = xs[1], [ x[1] for x in xss ]
                f(caar, *cars)
                while cadr and all(cdrs):
                        caar, cars = cadr[0], [ x[0] for x in cdrs ]
                        cadr, cdrs = cadr[1], [ x[1] for x in cdrs ]
                        f(caar, *cars)
        return nil

@defun
def mapcari(f, xs, fixup = nil):
        """Same as MAPCAR, but:
        1. Accept only one list argument
        2. Process improper lists, by handing the CDR of non-list cons in the 'natural' way,
           yielding either an improper list, or a proper one (if FIXUP is non-NIL) as a result.
        3. F receives two arguments, the second designating whether the improper tail is being handled."""
        if not xs:
                return nil
        car, cdr = xs[0], xs[1]
        acc = ptr = [f(car, nil), nil]
        improperp = nil
        while cdr:
                if not listp(cdr):
                        if fixup:
                                cdr, improperp = [cdr, nil], t
                        else:
                                ptr[1] = f(cdr, t)
                                break
                car, cdr = cdr[0], cdr[1]
                ptr[1] = ptr = [f(car, improperp), nil]
        return acc

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

# Arrays

# Function MAKE-ARRAY

# Function ADJUST-ARRAY

# Function ADJUSTABLE-ARRAY-P

@defun
def aref(x, *indices):
        ## Let's not forget the first two non-elements within arrays.
        return (x[indices[0] + 2] if len(indices) is 1 else
                x                if not indices       else
                aref(x[indices[0]], *indices[1:]))

# Function ARRAY-DIMENSION

# Function ARRAY-DIMENSIONS

# Function ARRAY-ELEMENT-TYPE

# Function ARRAY-HAS-FILL-POINTER-P

# Function ARRAY-DISPLACEMENT

# Function ARRAY-IN-BOUNDS-P

# Function ARRAY-RANK

# Function ARRAY-ROW-MAJOR-INDEX

# Function ARRAY-TOTAL-SIZE

# Function ARRAYP

# Accessor FILL-POINTER

# Accessor ROW-MAJOR-AREF

# Function UPGRADED-ARRAY-ELEMENT-TYPE

# Constant Variable ARRAY-DIMENSION-LIMIT

# Constant Variable ARRAY-RANK-LIMIT

# Constant Variable ARRAY-TOTAL-SIZE-LIMIT

# Function SIMPLE-VECTOR-P

# Accessor SVREF

# Function VECTOR

# Function VECTOR-POP

# Function VECTOR-PUSH, VECTOR-PUSH-EXTEND

# Function VECTORP

# Accessor BIT, SBIT

# Function BIT-AND, BIT-ANDC1, BIT-ANDC2, BIT-EQV, BIT-IOR, BIT-NAND, BIT-NOR, BIT-NOT, BIT-ORC1, BIT-ORC2, BIT-XOR

# Function BIT-VECTOR-P

# Function SIMPLE-BIT-VECTOR-P

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
def every(f, xs, *xss):
        if not xss:
                while xs:
                        if not f(xs[0]):
                                 return nil
                        xs = xs[1]
                return t
        else:
                not_implemented("EVERY: multiple-list case")

@defun
def some(f, xs, *xss):
        if not xss:
                while xs:
                        if f(xs[0]):
                                 return t
                        xs = xs[1]
                return nil
        else:
                not_implemented("SOME: multiple-list case")

@defun
def notevery(f, xs, *xss):
        if not xss:
                while xs:
                        if not f(xs[0]):
                                 return t
                        xs = xs[1]
                return nil
        else:
                not_implemented("NOTEVERY: multiple-list case")

@defun
def notany(f, xs, *xss):
        if not xss:
                while xs:
                        if f(xs[0]):
                                 return nil
                        xs = xs[1]
                return t
        else:
                not_implemented("NOTANY: multiple-list case")

# Function VALUES-LIST

# Function GET-SETF-EXPANSION

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

intern_and_bind(("_cons_", "CONS"), "LIST", ("_list_", "LIST*"), "APPEND", "QUOTE", "QUASIQUOTE", "COMMA", "SPLICE")

string_set("*READER-TRACE-QQEXPANSION*",        nil)

## Unregistered Issue COMPLIANCE-BACKQUOTE-EXPANSION-DOTTED-LIST-HANDLING

def expand_quasiquotation(form):
        "Direct translation of src/quasiquotation.lisp, modulo the python-specific intermarker representation."
        l, l_ = list_, list__
        def len_is_2(x):    return t if len(x) is 2 else error("Invalid intermarker: %s.", x)
        def commap(x):      return isinstance(x, tuple) and len_is_2(x) and x[0] is _comma
        def splicep(x):     return isinstance(x, tuple) and len_is_2(x) and x[0] is _splice
        def quasiquotep(x): return isinstance(x, tuple) and len_is_2(x) and x[0] is _quasiquote
        def simplify_append(x):
                def append_insulated_p(x):
                        return consp(x) and x[0] is _list and consp(x[1]) and x[1][1] is nil
                def try_simplify_list_(xs, tail):
                        return (list_(_cons_, second(xs[0]), tail) if xs[1] is nil else
                                list_(_list_, *([ x[1][0] for x in vectorise_linear(xs) ] + [tail])))
                pieces   = x[1]
                butlast_ = butlast(pieces)
                last_    = last(x)
                lastcar  = last_[0]
                if x[1][1] is nil:
                        ret =  x[1][0]
                elif not every(append_insulated_p, butlast_):
                        ninsulated = position_if_not(append_insulated_p, butlast_)
                        ret = (x if ninsulated == 0 else
                               try_simplify_list_(subseq(butlast_, 0, ninsulated),
                                                  list_(_append,
                                                        *(vectorise_linear(subseq(butlast_, ninsulated))
                                                          + [lastcar]))))
                elif append_insulated_p(lastcar):
                        ret = list_(_list, *[ x[1][0] for x in vectorise_linear(pieces) ])
                else:
                        ret = try_simplify_list_(butlast_, lastcar)
                return ret
        def self_evaluating_p(x):
                return isinstance(x, (int, float, complex, str)) or x in (t, nil)
        def quote_around_intermarkers_and_insulate(x):
                def rec(x):
                        ret = (x             if self_evaluating_p(x) else
                               x[1]          if commap(x)            else
                               x[1]          if splicep(x)           else
                               l(_quote, x)  if atom(x)              else
                               simplify_append(l_(_append, mapcari(lambda x, improperp:
                                                                           (identity            if not improperp else
                                                                            error("Invalid form: %s.", pp_consly(form))
                                                                                                if splicep(x)    else
                                                                            second)(rec(x)),
                                                                   x, fixup = t))))
                        return ret if splicep(x) else l(_list, ret)
                return rec(x)[1][0]
        def rec(x, depth):
                if quasiquotep(x):
                        rec_ret = rec(x[1], depth + 1)
                        qaiai_ret = quote_around_intermarkers_and_insulate(rec_ret)
                        return qaiai_ret
                return (      # if quasiquotep(x)          else
                        ((x[0], rec(x[1], depth - 1)) if depth > 0 else
                         error("Comma outside of a backquote in: %s.", pp_consly(form)))  if commap(x) or splicep(x) else
                        x                                                                 if atom(x)                 else
                        mapcari(lambda x, _: rec(x, depth), x))
        result = rec(form, 0)
        if symbol_value(_reader_trace_qqexpansion_):
                if form != result:
                        dprintf(";;;%s quasiquotation expanded to:\n%s%s",
                                      sex_space(-3, ";"), sex_space(), pp(result))
                else:
                        dprintf(";;;%s quasiquotation had no effect", sex_space(-3, ";"))
        return result

def run_tests_quasiquotation():
        dprintf("; testing the quasiquotation rewriter:")   ## Quirky stuff reference mode ON!  Go go Arachna!
        l, l_ = list_, list__
        a, b, c, d, e, f, g, h, i = (intern("%s" % x)[0] for x in "ABCDEFGHI")
        assert(runtest(("QUASIQUOTATION-IDENTITY", expand_quasiquotation),
                       l(a, l(b), c),
                       l(a, l(b), c),
                       printer = pp_consly))
        assert(runtest(("QUASIQUOTATION-VERY-SIMPLE", expand_quasiquotation),
                       ## `(a `,b)
                       (_quasiquote, l(a, (_comma, b))),
                       ## (list 'a b)
                       l(_list, l(_quote, a), b),
                       printer = pp_consly))
        assert(runtest(("QUASIQUOTATION-SIMPLE", expand_quasiquotation),
                       ## (a `((b) ,(c) ,@(d)))
                       l(a, (_quasiquote, l(l(b), (_comma, l(c)), (_splice, l(d))))),
                       ## (a (append (list '(b)) (list (c)) (d)))  or, equivalently
                       ## (a (list* '(b) (c) (d)))
                       l(a, l(_list_, l(_list, l(_quote, b)), l(c), l(d))),
                       printer = pp_consly))
        assert(runtest(("QUASIQUOTATION-LARGE", expand_quasiquotation),
                       ## `(a ,b c ,@d e (,f ,@g) ,@h ,@i)
                       (_quasiquote, l(a, (_comma, b), c, (_splice, d), e,
                                       l((_comma, f), (_splice, g)),
                                       (_splice, h), (_splice, i))),
                       ## (list* 'a b 'c (append d (list 'e) (list (cons f g)) h i)
                       l(_list_, l(_quote, a), b, l(_quote, c),
                         l(_append, d, l(_list, l(_quote, e)),
                           l(_list, l(_cons_, f, g)), h, i)),
                       printer = pp_consly))
        
        assert(runtest(("QUASIQUOTATION-NESTED-SIMPLE", expand_quasiquotation),
                       ## ``(a)
                       (_quasiquote, (_quasiquote, l(a))),
                       ## (list 'list (list 'quote 'a))
                       ## where inner is: (list 'a)
                       l(_list, l(_quote, _list), l(_list, l(_quote, _quote), l(_quote, a))),
                       printer = pp_consly))
        assert(runtest(("QUASIQUOTATION-NESTED", expand_quasiquotation),
                       ## ``(,a ,,b ,@c ,@,d)
                       (_quasiquote,
                        (_quasiquote,
                         l((_comma, a), (_comma, (_comma, b)), (_splice, c), (_splice, (_comma, d))))),
                       ## (list 'list* 'a b (list 'append 'c d))
                       ## where inner is: (list* a ,b (append c ,d))
                       l(_list, l(_quote, _list_), l(_quote, a), b, l(_list, l(_quote, _append), l(_quote, c), d)),
                       printer = pp_consly))
        assert(runtest(("QUASIQUOTATION-WOVEN", expand_quasiquotation),
                       ## `(a ,b ,@c (d ,(e `(f ,@(g`h)))))
                       (_quasiquote,
                        l(a, (_comma, b), (_splice, c),
                          l(d, (_comma, l(e, (_quasiquote, l(f, (_splice, l(g, (_quasiquote, h)))))))))),
                       ## (list* 'a b (append c (list (list 'd (e (cons 'f (g h)))))))
                       l(_list_, l(_quote, a), b,
                         l(_append, c,
                           l(_list, l(_list, l(_quote, d),
                                      l(e, l(_cons_, l(_quote, f), l(g, l(_quote, h)))))))),
                       printer = pp_consly))
        assert(runtest(("QUASIQUOTATION-IDENTITY-DOTTED", expand_quasiquotation),
                       cons(a, b),
                       cons(a, b),
                       printer = pp_consly))
        assert(runtest(("QUASIQUOTATION-DOTTED", expand_quasiquotation),
                       ## ``(a ,b . ,c)
                       (_quasiquote, l_(a, (_comma, b), (_comma, c))),
                       ## (list* 'a b c)
                       l(_list_, l(_quote, a), b, c),
                       printer = pp_consly))
        assert(runtest(("QUASIQUOTATION-NESTED-DOTTED", expand_quasiquotation),
                       ## ``(a ,,b . ,,c)
                       (_quasiquote,
                        (_quasiquote, l_(a, (_comma, (_comma, b)), (_comma, (_comma, c))))),
                       ## (list 'list* (list 'quote 'a) b  c)
                       ## where the inner is: (list* 'a ,b ,c)
                       l(_list, l(_quote, _list_), l(_list, l(_quote, _quote), l(_quote, a)), b, c),
                       printer = pp_consly))

if getenv("CL_RUN_TESTS") == "t" and getenv("CL_TEST_QQ") == "t":
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
        bufc = nil
        def do_read_char(eof_error_p = t, eof_value = nil):
                nonlocal bufc
                if bufc:
                        ret, bufc = bufc, nil
                        return ret
                else:
                        return read_char(stream, eof_error_p, eof_value)
        def do_unread_char(char):
                nonlocal bufc
                if bufc:
                        error("Attempted to unread more than one character (new %s, already got %s)!", repr(bufc), repr(char))
                bufc = char
        def read_char_maybe_eof(): return do_read_char(nil, nil)
        def read_inner(allow_consing_dot = nil):
                skip_whitespace(eof_error_p = t)
                char = do_read_char()
                do_unread_char(char)
                if   char == chr(40):  obj = read_list() # Org is a bit too picky
                elif char == "\"":     obj = read_string()
                elif char == "#":
                        do_read_char()
                        dchar = do_read_char()
                        obj = read_dispatched(dchar)
                elif char == "'":
                        do_read_char()
                        obj = list_(_quote, read_inner())
                elif char == "`":
                        do_read_char()
                        obj = (_quasiquote, read_list())
                elif char == ",":
                        ## This is a simplified take, but it'll do for bootstrapping purposes.
                        do_read_char()
                        char = do_read_char()
                        if char == "@":
                                obj = (_splice, read_inner())
                        else:
                                do_unread_char(char)
                                obj = (_comma, read_inner())
                else:
                        # handle_short_read_if(pos > end)
                        obj = read_number_or_symbol()
                        if obj == find_symbol(".", __cl)[0] and not allow_consing_dot:
                                error("Unexpected consing dot in stream %s, position %%s.", stream)
                        # here("< %s" % (obj,))
                return obj
        def read_dispatched(char):
                if char == "'":
                        return list_(_function, read_inner())
                if char in ("b", "B"):
                        return int(read_token(), 2)
                if char in ("x", "X"):
                        return int(read_token(), 16)
                if char in ("+", "-"):
                        def feature_eval(expr):
                                features = symbol_value(_features_)
                                def assertex(x):
                                        x or error("Invalid feature expression: %s", pp_consly(expr))
                                expr = vectorise(expr) if consp(expr) else expr
                                def eval_atom(x):
                                        assertex(isinstance(x, symbol_t))
                                        return find(x, features)
                                def eval_or(xs):  return any(rec(x) for x in xs)
                                def eval_and(xs): return all(rec(x) for x in xs)
                                def eval_not(x):  return not rec(x)
                                def rec(x):
                                        return (eval_atom(x)    if atom(x)      else
                                                eval_or(x[1:])  if x[0] is _or  else
                                                eval_and(x[1:]) if x[0] is _and else
                                                eval_not(x[1])  if x[0] is _not else
                                                assertex(atom(x) or x[0] in [_or, _and, _not]))
                        feature_form = read_inner()
                        successp = feature_eval(feature_form)
                        successp = (successp and char == "+") or (not successp and char == "-")
                        with progv({ _read_suppress_: not successp }):
                                form = read_inner()
                        # format(t, "FEX  %s%s  wrt  %s  -->  %s\n", char, pp_consly(feature_form), pp_consly(form), successp)
                        return (form if successp else
                                read_inner()) ## We're obliged to read something.
                else:
                        error("Unknown or invalid dispatch macro character: %s.", princ_to_string(char))
        def skip_until_eol():
                c = read_char_maybe_eof()
                while c and c != "\n":
                        c = read_char_maybe_eof()
        def skip_whitespace(eof_error_p = nil):
                while t:
                        c = do_read_char(eof_error_p, nil)
                        if c == ";":
                                skip_until_eol()
                        elif c not in frozenset([" ", "\t", "\n"]):
                                if c is not nil:
                                        do_unread_char(c)
                                return
        def read_list():
                ret = []
                improper = nil
                c = do_read_char() # it's a #\(
                while t:
                        skip_whitespace()
                        char = do_read_char()
                        if char == "\x29":
                                break
                        else:
                                do_unread_char(char)
                                obj = read_inner(allow_consing_dot = t)
                                if not listp(obj) and obj is find_symbol(".", __cl)[0]:
                                        improper = read_inner()
                                        skip_whitespace()
                                        char = do_read_char()
                                        if char != "\x29":
                                                error("Unexpected character %s, where a closing paren was expected.",
                                                      repr(char))
                                        break
                                ret.append(obj)
                # here("< %s" % (ret,))
                return consify_linear(tuple(ret), last_cdr = improper) ## Beacon DEBUG-RELATED-SLOWDOWN
        def read_string():
                ret = ""
                do_read_char() # seek the opening double-quote
                while t:
                        char = do_read_char()
                        if char == "\"":
                                break
                        elif char == "\\":
                                char2 = do_read_char()
                                ret += (char2 if char2 in set(["\"", "\\"]) else
                                        error("READ-FROM-STRING: unrecognized escape character \"%s\".", char2))
                        else:
                                ret += char
                # here("< %s" % (ret,))
                return ret
        def read_number_or_symbol():
                token = read_token()
                if symbol_value(_read_suppress_):
                        return nil
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
                                        do_unread_char(char)
                                break
                        else:
                                token += char
                # here("< %s" % repr(token))
                return token
        ret = handler_case(read_inner,
                           (end_of_file_t,
                            lambda c: error(c) if eof_error_p else
                                      return_from(cold_read, eof_value)))
        # format(t, "lastly %s\n", pp_consly(ret))
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
                py.backtrace(-1, stream)
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

intern_and_bind("*STACK-TOP-HINT*", "*TRACEBACK*", "*SIGNALLING-FRAME*", gvarp = t)

string_set("*LAST-CHANCE-HANDLER*", nil)

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
        def report_condition(cond):
                if isinstance(cond, condition_t):
                        try:
                                repr_str = princ_to_string(cond)
                        except Exception as sub_cond:
                                dprintf("While printing condition, another condition was raised: %s", repr(sub_cond))
                                # py.backtrace(frame = exception_frame())
                                repr_str = "#<error printing condition>"
                        py.here("In thread '%s': unhandled condition of type %s:\n\n%s",
                                threading.current_thread().name, type_of(cond), repr_str,
                                callers = 15, frame = signalling_frame)
                else:
                        dprintf("In thread %s: a non-condition of type %s was raised: %s",
                                threading.current_thread().name, type_of(cond), repr(cond))
        signalling_frame = py.caller_frame(caller_relative = 1)
        with progv({_stack_top_hint_: signalling_frame}):
                cond = sys.call_tracing(continuation, ())
        if type_of(cond) not in __not_even_conditions__:
                if not isinstance(cond, error_t):
                        return
                report_condition(cond)
                if not backtrace_printed:
                        py.backtrace(offset = 2) ## 2 = [ pytracer, __cl_condition_handler__ ]
                last_chance_handler = symbol_value(_last_chance_handler_)
                if last_chance_handler:
                        last_chance_handler(cond)
                dprintf("; Last chance handler declined, trying to enter the debugger.")
                try:
                        invoke_debugger(cond)
                except error_t as debugger_cond:
                        dprintf("; Failed to enter the debugger due to the following condition:\n%s\n; Disabling the CL condition system.\n; Have a nice day!", debugger_cond)
                        sys.stderr.flush()
                        frost.disable_pytracer()
        ## Issue UNHANDLED-CONDITIONS-NOT-REALLY
        # At this point, the Python condition handler kicks in,
        # and the stack gets unwound for the first time.
        #
        # ..too bad, we've already called all HANDLER-BIND-bound
        # condition handlers.
        # If we've hit any HANDLER-CASE-bound handlers, then we won't
        # even reach this point, as the stack is already unwound.
set_condition_handler(__cl_condition_handler__)

def handler_bind(f, *handlers, no_error = identity):
        "Works like real HANDLER-BIND, when the conditions are right.  Ha."
        value = None

        # this is:
        #     frost.pytracer_enabled_p() and condition_handler_active_p()
        # ..inlined for speed.
        if frost.pytracer_enabled_p() and frost.tracer_hook("exception") is __cl_condition_handler__:
                # Unregistered Issue HANDLER-BIND-CHECK-ABSENT
                with progv({_handler_clusters_: (symbol_value(_handler_clusters_) +
                                                 [handlers + (("__frame__", py.caller_frame()),)])}):
                        return no_error(f())
        else:
                # old world case..
                # format(t, "crap FAIL: pep %s, exhook is cch: %s",
                #        frost.pytracer_enabled_p(), __tracer_hooks__.get("exception") is __cl_condition_handler__)
                if len(handlers) > 1:
                        error("HANDLER-BIND: was asked to establish %d handlers, but cannot establish more than one in 'dumb' mode.",
                              len(handlers))
                condition_type_name, handler = handlers[-1]
                try:
                        value = f()
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
        restarts_args = dict()
        for name, spec in restart_specs.items():
                function, options = ((spec[0], spec[1]) if isinstance(spec, tuple) else
                                     (spec, dict()))
                restarts_args[name.upper()] = updated_dict(options, dict(function = function)) # XXX: name mangling!
        return restarts_args

##
# XXX: :TEST-FUNCTION is currently IGNORED!
##
def do_restart_bind(body, restart_args):
        with progv({_restart_clusters_: (symbol_value(_restart_clusters_) +
                                           [{ name: restart_t(**restart_args)
                                              for name, restart_args in restart_args.items() }])}):
                return body()

def restart_bind(body, **restart_specs):
        return do_restart_bind(body, specs_restarts_args(restart_specs))

__valid_restart_options__ = frozenset(["interactive", "report", "test", "function"])
def do_restart_case(body, **restarts_args):
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
                      lambda: do_restart_bind(body, wrapped_restarts_args))

def restart_case(body, **restart_specs):
        return do_restart_case(body, **specs_restarts_args(restart_specs))

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

def do_setup_emt(emt = None, immediate = None):
        global __enable_matcher_tracing__, __matcher_tracing_immediate__
        __enable_matcher_tracing__ = defaulted(emt, __enable_matcher_tracing__)
        __matcher_tracing_immediate__ = defaulted(immediate, __matcher_tracing_immediate__)

def setup_emt(self):
        self.saved_emt, self.saved_immediate = __enable_matcher_tracing__, __matcher_tracing_immediate__
        do_setup_emt(emt = self.new_emt, immediate = self.immediate)

def restore_emt(self):
        global __enable_matcher_tracing__, __matcher_tracing_immediate__
        __enable_matcher_tracing__ = self.saved_emt
        __matcher_tracing_immediate__ = self.saved_immediate

traced_matcher = defwith("traced_matcher",
                         lambda self:        setup_emt(self) or dynamic_scope_push({ _matcher_tracing_: t }),
                         lambda self, *_:  restore_emt(self) or dynamic_scope_pop(),
                         __init__ = (lambda self, emt = None, immediate = None:
                                             self.__dict__.update({ "new_emt": emt,
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
                 (" " * depth, name if name else py.caller_name().upper(),
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
                            (" " * depth, name if name else py.caller_name().upper(),
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

intern_and_bind("*KINDA-POSITION*", gvarp = t)

intern_and_bind("%SOME", "%MAYBE", "%OR", "%FUNCHER",
                "IR-ARGS",
                "LAMBDA")
def kinda_position():
        return symbol_value(_kinda_position_)

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
                        return Fail
                def try_R():
                        nonlocal bR, fxR, fpR
                        bR, fxR, fpR = lR(b0)
                        if fpR is None: return fxR
                        return Fail
                result = combine(try_0, try_R, originalp)
                # result = None if result is Fail else result
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
                 with progv({ _kinda_position_: (0 if firstp else (symbol_value(_kinda_position_) + 1)) }):
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
                 with progv({ _kinda_position_: 0 if orifst[0] else (symbol_value(_kinda_position_) + 1)}):
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

def matcher_error(sex, r, f, desc = "matching"):
        error("While %s %s: failed to match %s using pattern %s.", desc, pp_consly(sex), pp_consly(r), pp_consly(f))

# Preprocessing

intern_and_bind(
        "%FORM",
        "%BASE", "%NEWLINE", "%FILL", "%NBSP",
        "%BINDER", "%BIND", "%BOUND",
        "%POSCASE", "%LEAD", "%NOTLEAD", "%NOTTAIL",
        "%FOR-MATCHERS-XFORM", "%FOR-NOT-MATCHERS-XFORM", "%FOR-MATCHER-LAYERS-SKIP-ACTION")

__metasex_words__          = set() ## Populated by register_*_matcher()
__metasex_pp_words__       = { _base, _newline, _fill, _nbsp, _poscase, _lead, _notlead, _nottail }
__metasex_bind_words__     = { _binder, _bind, _bound }
__metasex_leaf_words__     = { _form,
                               _base, _newline, _fill, _nbsp,
                               _for_matchers_xform, _for_not_matchers_xform, _for_matcher_layers_skip_action }

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
                def prep_poscase(c):
                        return cons(_poscase, mapcar(lambda x: cons(x[0], rec(x[1])[0]), c[1]))
                def prep_linear(xs):
                        return mapcon(lambda con: rec(con[0]), xs)
                return (l(prep_binding(x))             if isinstance(x, dict)                 else
                        l(l(_nbsp, 1))                 if x == " "                            else
                        l(l(_base))                    if x == "."                            else
                        l(l(_newline, 0))              if x == "\n"                           else
                        l(l(_newline, x))              if isinstance(x, int)                  else
                        l(x)                           if atom(x)                             else
                        (error("Reference to an undefined funcher %s in pattern %s.",
                               x[1][0], pat) if not funcher_name_p(x[1][0]) else
                         find_funcher(x[1][0]))        if x[0] is _funcher                    else
                        l(prep_poscase(x))             if x[0] is _poscase                    else
                        l(prep_linear(x)))
        consified = consify_metasex(pat)
        ret = rec(consified)[0]
        # dprintf("Preprocessed\n%s  ->\n%s  ->\n%s", pat, pp_consly(consified), pp_consly(ret))
        return ret

def rewrite_metasex(form, for_pp = nil, for_bind = nil):
        strip_pp   = not for_pp
        strip_bind = not for_bind
        l = list_
        def de_bind(x):
                return (x[1][0]    if x[0] is _bound           else
                        x[1][1][0] if x[0] in (_bind, _binder) else
                        error("Invalid form for binder rewriting: %s", pp_consly(x)))
        def rec(xs):
                return mapcon(lambda x: (l(x[0])                  if not consp(x[0])                                  else
                                         l(rewrite_metasex(x[0])) if not (symbolp(x[0][0]) or strip_pp or strip_bind) else
                                         nil                      if strip_pp   and   metasex_pp_word_p(x[0][0])      else
                                         l(l(_form))              if for_pp     and x[0][0] is _satisfies             else
                                         rec(l(de_bind(x[0])))    if strip_bind and metasex_bind_word_p(x[0][0])      else
                                         l(rec(x[0]))),
                              xs)
        ret = rec(l(form))[0]
        # dprintf("REWR %s: %s\n--->\n%s", "pp" if for_pp else "bind" if for_bind else "?", pp_consly(form), pp_consly(ret))
        return ret

# METASEX-MATCHER, METASEX-MATCHER-PP and METASEX-MATCHER-NONSTRICT-PP

#         metasex presents us with an excellent lesson.  Let's try to understand.

string_set("*METASEX-KIND*", "metasex")

def form_metasex(form, kind = "metasex"):
        "Return a normalised metasex for FORM."
        ## Unregistered Issue FORM-METASEX-SHOULD-COMPUTE-METASEX-OF-DEFINED-MACROS
        ## Unregistered Issue FORM-METASEX-TOO-RELAXED-ON-ATOMS
        ## Unregistered Issue FORM-METASEX-STATICALLY-PRECLUDES-MULTILINE-FORMS
        return (preprocess_metasex((_typep, t))          if not consp(form)                                    else
                getattr(find_known(form[0]), kind)    if isinstance(form[0], symbol_t) and find_known(form[0]) else
                preprocess_metasex((_cons,
                                    (_form,), (_form,))) if not listp(form[1])                                 else
                preprocess_metasex((([(_notlead, " "), (_form,)] if kind == "metasex_pp" else
                                     [                 (_form,)]),)))

def form_real(x):
        return (x if not (consp(x) and x[0] is _ir_args) else
                x[1][0])

class Fail_t():
        def __str__(_):  return "<Fail>"
        def __repr__(_): return "<Fail>"
        def __bool__(_): return False
Fail = Fail_t()
# Fail = None

def combine_t_or_Fail(f0, fR, originalp):
        f0r = f0()
        if f0r is Fail: return Fail
        fRr = fR()
        if fRr is Fail: return Fail
        return t

def combine_pp(f0, fR, originalp, consdotp):
        def orig_tuple_comb(body):
                new_base = pp_depth() + 1
                # dprintf("==== COMPP-OTCOM new_base (%d) = pp_base_depth() (%d) + n (%d)", new_base, pp_depth(), 1)
                with progv({ _pp_base_depth_: new_base,
                             _pp_depth_     : new_base }):
                        ret = body(new_base)
                        return Fail if ret is Fail else ("\x28" + ret + "\x29")
        def body(base):
                f0r = f0()
                if f0r is Fail: return Fail
                insertion = (" . " if consdotp else "")
                lines = f0r.split("\n")
                new_depth = ((base + len(f0r)) if len(lines) is 1 else len(lines[-1])) + len(insertion)
                # dprintf("==== COMPP-BODY  f0r:      '%s'", f0r)
                # dprintf("==== COMPP-BODY new_depth (%d) = base (%d) + len(f0r.split(RET)[-1]) (%d) + len(insertion) (%d)",
                #         new_depth, base, len(f0r.split("\n")[-1]), len(insertion))
                with progv({ _pp_depth_: new_depth }):
                        fRr = fR()
                        return Fail if fRr is Fail else f0r + insertion + fRr
        return (orig_tuple_comb if originalp else
                lambda f: f(pp_depth()))(body)

def combine_cons(f0, fR, originalp):
        f0r = f0()
        if f0r is Fail: return Fail
        fRr = fR()
        if fRr is Fail: return Fail
        return [f0r, fRr]

def combine_append(f0, fR, originalp):
        f0r = f0()
        if f0r is Fail: return Fail
        fRr = fR()
        if fRr is Fail: return Fail
        return append(f0r, fRr)

intern_and_bind("%SATISFIES", "%CONS", "%TYPEP")

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
        def comh(f0, fR, originalp): return combine_t_or_Fail(f0, fR, originalp)
        @staticmethod
        def comr(f0, fR, originalp): return combine_t_or_Fail(f0, fR, originalp)
        @staticmethod
        def comc(f0, fR, originalp): return combine_t_or_Fail(f0, fR, originalp)
        def process_formpat_arguments(m, layer, form, pat, continuation = None):
                ## Handlers are called with the REST of the matched OP form.
                handlers = { _for_matchers_xform:             (lambda _, *ms:         m in ms,
                                                               lambda action, *_: action(form)),
                             _for_not_matchers_xform:         (lambda _, *ms:     not m in ms,
                                                               lambda action, *_: action(form)),
                             _for_matcher_layers_skip_action: (lambda *mls:   any((m is mr and layer is lr)
                                                                                  for mr, (lr, _) in mls),
                                                               lambda *_: continuation(form)[1])
                             }
                ptr = pat[1]
                while ptr:
                        name, args = ptr[0]
                        if name not in handlers:
                                error("Invalid FORM argument: %s, pat: %s", name, pat)
                        applicable_p, handler = handlers[name]
                        if applicable_p(*vectorise_linear(args)):
                                ret = handler(*vectorise_linear(args))
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
                handled, ret = m.process_formpat_arguments(metasex_mapper, form, pat, continuation = mapper_continuation)
                if handled:
                        return m.succ(bound, ret)
                return symbol_value(_mapper_fn_)(form, mapper_continuation)

metasex_mapper = metasex_mapper_t()

def map_sex(fn: "Form -> (Form -> ({} Form Bool)) -> ({} Form Bool)",
             sex, matcher = metasex_mapper, start_inside = nil, error_if_fail = t) -> "({} Form Bool)":
        with progv({ _mapper_fn_: fn }):
                ret = _, r, f = match(matcher, sex, form_metasex(sex, kind = symbol_value(_metasex_kind_)) if start_inside else list_(_form))
                if f is not None and error_if_fail:
                        matcher_error(sex, r, f, desc = "mapping with %s using %s" % (fn.__name__, type(matcher).__name__))
                return ret

def xform_ir(fn: "Form -> (Form -> ({} Form Bool)) -> ({} Form Bool)",
             sex, matcher = metasex_mapper, start_inside = nil) -> "Form":
        with matcher_pp_stack():
                _, r, f = map_sex(fn, sex, matcher, start_inside)
        if f is not None:
                error("\n=== failed sex: %s\n=== failsubpat: %s\n=== subex: %s", pp_consly(sex), matcher_pp(f), pp_consly(r))
        return r

# Pretty-printing

class metasex_pprinter_t(metasex_matcher_t):
        def __init__(m):
                metasex_matcher_t.__init__(m)
                m.register_complex_matcher(_base,        m.base)
                m.register_complex_matcher(_newline,     m.newline)
                m.register_complex_matcher(_fill,        m.fill)
                m.register_complex_matcher(_nbsp,        m.nbsp)
                m.register_complex_matcher(_poscase,     m.poscase)
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
        def base(m, bound, name, exp, pat, orifst, aux, limit):
                with progv({ _pp_base_depth_: pp_depth() }):
                        return m.match(bound, name, exp, pat[1], orifst, aux, limit)
        def newline(m, bound, name, exp, pat, orifst, aux, limit):
                n, tail = pat[0][1][0], pat[1]
                new_base = pp_base_depth() + n
                # dprintf("==== NEWLINE new_base (%d) = pp_base_depth() (%d) + n (%d)", new_base, pp_base_depth(), n)
                with progv({ _pp_depth_:      new_base,
                             _pp_base_depth_: new_base }):
                        return m.post(m.match(m.bind(new_base, bound, name), None, exp, tail, orifst, aux, -1),
                                      lambda r: "\n" + (" " * new_base) + r)
        def fill(m, bound, name, exp, pat, orifst, aux, limit):
                if pp_depth() >= 50:
                        return m.newline(bound, name, exp, cons(list_(_newline, 0), pat[1]), orifst, aux, limit)
                else:
                        return m.nbsp(bound, name, exp, cons(list_(_nbsp, 1), pat[1]), orifst, aux, limit)
        def nbsp(m, bound, name, exp, pat, orifst, aux, limit):
                n, tail = pat[0][1][0], pat[1]
                new_depth = pp_depth() + n
                # dprintf("==== INDENT new_depth (%d) = pp_depth() (%d) + n (%d)", new_depth, pp_depth(), n)
                with progv({ _pp_depth_: new_depth }):
                        return m.post(m.match(m.bind(new_depth, bound, name), None, exp, tail, orifst, aux, -1),
                                      lambda r: (" " * n) + r)
        def poscase(m, bound, name, exp, pat, orifst, aux, limit):
                ## Unregistered Issue POSCASE-BECAME-UGLY-SINCE-SEGMENT-ITERATION-BECAME-SORTA-POSITION
                segpos = kinda_position()
                for pos, casepat in xmap_to_vector(lambda x: (x[0], x[1][0]), pat[0][1]):
                        if pos in (segpos, t):
                                return m.match(bound, name, exp, [casepat, pat[1]], orifst, aux, limit)
                ## act as identity
                return m.match(bound, name, exp, pat[1],              orifst, aux, limit)
        def lead(m, bound, name, exp, pat, orifst, aux, limit):
                maybe_pat = pat[0][1][0]
                if not orifst[1]:
                        return m.match(bound, name, exp, pat[1],              orifst, aux, limit)
                ############## act as identity
                return         m.match(bound, name, exp, [maybe_pat, pat[1]], orifst, aux, limit)
        def notlead(m, bound, name, exp, pat, orifst, aux, limit):
                maybe_pat = pat[0][1][0]
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

metasex_pprinter = metasex_pprinter_t()

def pp_sex(sex, strict = t, initial_depth = None):
        ## Unregistered Issue RELAXED-METASEX-PRETTY-PRINTER-MODE-NEEDED
        initial_depth = defaulted_to_var(initial_depth, _pp_base_depth_)
        pat = form_metasex(sex, kind = "metasex_pp")
        with progv({ _pp_depth_:         initial_depth,
                     _pp_base_depth_:    initial_depth,
                     _matcher_tracing_:  nil,
                     _metasex_kind_:     "metasex_pp" }): ## Guide the nested %FORM-METASEX invocations.
                with matcher_pp_stack():
                        _, r, f = match(metasex_pprinter, sex, pat)
                        if f is not None:
                                dprintf(";  failed to properly print %s", repr(sex))
                        return r if f is None else pp_consly(sex)

def ir_minify(form):
        return ('"%s"' % form if stringp(form)                                else
                str(form)     if symbolp(form) or not form or not consp(form) else
                ("(%s ...)" % ir_minify(form[0])))

def mock_sex(sex, initial_depth = None, max_level = None):
        ## Unregistered Issue IMPROPER-LISTS-BREAK-MOCK
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

intern_and_bind("&WHOLE", "&OPTIONAL", "&REST", "&BODY", "&KEY", "&ALLOW-OTHER-KEYS", "&AUX", "&ENVIRONMENT")

__lambda_words__ = { _whole, _optional, _rest, _body, _key, _allow_other_keys, _aux, _environment }

def lambda_word_p(x):
        return isinstance(x, symbol_t) and x in __lambda_words__

intern_and_bind("DECLARE")

@defun
def parse_body(body, doc_string_allowed = t):
        doc = nil
        def doc_string_p(x, remaining_forms):
                return ((error("Duplicate doc string %s", x) if doc is not nil else t)
                        if isinstance(x, str) and doc_string_allowed and remaining_forms else
                        None)
        def declaration_p(x):
                return consp(x) and x[0] is _declare
        decls, forms = nil, nil
        ptr = body
        while ptr:
                if not consp(ptr):
                        error("Malformed body: %s.", pp_consly(body))
                form, rest = ptr
                if doc_string_p(form, rest):
                        doc = form
                elif declaration_p(form):
                        decls = cons(form, decls)
                else:
                        forms = ptr
                        break
                ptr = ptr[1]
        return values(forms,
                      nreverse(decls),
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

def further_eval_when_eval(_, __, eval, ct, lt, e):
        return nil, nil, eval and e

def further_eval_when_file_compiler(compile_time_too, _, __, ct, lt, e):
        "Implement the EVAL-WHEN chart of section #5 of CLHS 3.2.3.1."
        process = lt
        eval = ct or (compile_time_too and e)
        new_compile_time_too = lt and eval
        # dprintf("\nA-E-W-S:  CTT CT:LT:E   %s  %s %s %s   ->   NCTT %s, PROC %s, EVAL %s",
        #         compile_time_too, ct, lt, e, new_compile_time_too, process, eval)
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

string_set("*COMPILER-TRACE-PRETTY-FULL*",        t)

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

                                       _compiler_trace_pretty_full_:        t,
                                       _compiler_validate_ast_:             nil,
                                       _compiler_trapped_functions_:        set() }

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
        return (pp_sex if symbol_value(_compiler_trace_pretty_full_) else mock_sex)(x, **args)

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

intern_and_bind(
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
        tn, value, shadows = None, None, None
        def __init__(self, value, shadows = None, **attributes):
                attrify_args(self, locals(), "value", "shadows")
        def __repr__(self):
                return "#<bind %s %s: %s  tn: %s>" % (self.kind, self.name, self.value,
                                                      (defaulted(self.tn, "<not allocated>")))
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
        def allocate_tn(self, globalp = nil):
                ## This calls onto primitive IR
                self.tn = variable_tn(self.name, globalp = globalp)
class function_binding(function, binding):
        def __init__(self, name, kind, value, tn = nil, **attributes):
                function.__init__(self, name, kind, **attributes)
                binding.__init__(self, value, **attributes)
                if tn:
                        self.tn = tn
        def allocate_tn(self, globalp = nil):
                ## This calls onto primitive IR
                self.tn = function_tn(self.name, globalp = globalp)
                # dprintf("  ALLOCATE-TN: %s -> %s", self.name, self.tn)
class block_binding(block, binding):
        def __init__(self, name, _, value, **attributes):
                block.__init__(self, name, **attributes)
                binding.__init__(self, value, **attributes)
class gotag_binding(gotag, binding):
        def __init__(self, name, _, value, **attributes):
                gotag.__init__(self, name, **attributes)
                binding.__init__(self, value, **attributes)

def function_bindingp(x): return isinstance(x, function_binding)
def block_bindingp(x):    return isinstance(x, block_binding)
def gotag_bindingp(x):    return isinstance(x, gotag_binding)

# Lexenv

intern_and_bind("*LEXENV*", gvarp = t)
intern_and_bind("NULL")
intern_and_bind("%BOOTSTRAP-NULL-LEXENV")

def with_lexenv(lexenv, f):
        with progv({ _lexenv_: lexenv }):
                return f()

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
                        self.allocate_tns(globalp = nil)
        def adjoin_scope(self, parent_lexenv, sname, frame):
                pscope = getattr(parent_lexenv, sname) if parent_lexenv else nil
                return (pscope if not frame else
                        (frame,
                         (nil if parent_lexenv is nil else pscope),
                         self)
                        ), frame
        def allocate_tns(self, globalp = nil):
                for slot in ["varframe", "funcframe"]:
                        frame = getattr(self, slot)
                        if frame:
                                for binding in frame["name"].values():
                                        binding.allocate_tn(globalp = globalp)
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
        def lookup_func(self, x, default = None):         return self.do_lookup_scope(self.funcscope, interpret_function_name(x), default)
        def lookup_block(self, x, default = None):        return self.do_lookup_scope(self.blockscope, x, default)
        def lookup_gotag(self, x, default = None):        return self.do_lookup_scope(self.gotagscope, x, default)
        def funcscope_binds_p(self, x):   return self.lookup_func(x)[0]  is not None
        def varscope_binds_p(self, x):    return self.lookup_var(x)[0]   is not None
        def blockscope_binds_p(self, x):  return self.lookup_block(x)[0] is not None
        def gotagscope_binds_p(self, x):  return self.lookup_gotag(x)[0] is not None
        def lookup_func_kind(self, kind, x, default = None):
                b = self.do_lookup_scope(self.funcscope, interpret_function_name(x), None)[0]
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
                [(_lead, 1), (_notlead, "\n"), (_bound, (_form,))])

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
        def establish_binder(m, bound, name, exp, pat, orifst):
                ## Skip over -- it's handled by WALK-WITH-LEXENV itself.
                return m.match(bound, name, exp, pat[1][1][0], orifst, None, -1)
        def add_binding(m, bound, name, exp, pat, orifst):
                ## For the sake of proper pre-lexenv maintenance for the value subform of this binding form,
                ## bust all the bindings accumulated from previous unsuccessful matches:
                binder = the(lexenv_walker.binder, symbol_value(_walker_binder_))
                if not binder:
                        error("While adding a binding from %s: no binder available.", pp_consly(exp))
                position = kinda_position()
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
                name, what = (first(exp), cdr(exp)) if listp(exp) else (exp, nil)
                # dprintf("==== Found binding for %s", name)
                binder.positionally[position] = (position, kind, name, what)
                return ret
        def setup_lexenv(m, bound, name, exp, pat, orifst):
                def further():
                        return m.default(exp, pat[1][0], name = name, orifst = orifst)
                binder = symbol_value(_walker_binder_)
                if not binder:
                        error("While setting up a lexenv for %s: no binder available.", pp_consly(exp))
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
                     sex, lexenv = nil, allocate_tns = nil, matcher = lexenv_walker, bind_after_fn = t) -> "Form":
        with progv({ _walker_lexenv_:         coerce_to_lexenv(lexenv),
                     _walker_allocate_tns_:   allocate_tns,
                     _metasex_kind_:         "metasex_bind" }):
                def fn_then_binder(form, further: "Form -> ({} Form Bool)") -> "({} Form Bool)":
                        def continuation(form) -> "({} Form Bool)":
                                peek_pat = form_metasex(form_real(form), kind = "metasex_bind")
                                if consp(peek_pat) and peek_pat[0] is _binder:
                                        return peek_pat[1][0].known.binder(form, further)
                                else:
                                        return further(form)
                        return fn(form, continuation)
                def binder_then_fn(form, further: "Form -> ({} Form Bool)") -> "({} Form Bool)":
                        def continuation(form) -> "({} Form Bool)":
                                return fn(form, further)
                        peek_pat = form_metasex(form_real(form), kind = "metasex_bind")
                        if consp(peek_pat) and peek_pat[0] is _binder:
                                return peek_pat[1][0].known.binder(form, continuation)
                        else:
                                return continuation(form)
                return xform_ir((    fn_then_binder if bind_after_fn else
                                 binder_then_fn),
                                sex, matcher = matcher)

def walker_lexenv():
        return symbol_value(_walker_lexenv_)

# Global scope

intern_and_bind(("_setf", "SETF"))

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
def find_global_function(name):     return gethash(interpret_function_name(name), function_scope)[0]
def  set_global_function(name, x): function_scope[interpret_function_name(name)] = the(function, x)
def  set_global_variable(name, x): variable_scope[name] = the(variable, x)

def compiler_defparameter(name, value):
        x = variable(the(symbol_t, name), _variable)
        variable_scope[name] = x
        if value is not None:
                __global_scope__[name] = value

def compiler_defvar(name, value):
        if not find_global_variable(name):
                compiler_defparameter(name, value)

def compiler_defun(name, lambda_expression: cons_t, check_redefinition = t) -> bool:
        """Manipulate the compiler's idea of a function's definition.
           Return a boolean, which denotes whether the situation is an identity redefinition."""
        check_type(name, (or_t, symbol_t, cons_t))
        oldef = find_global_function(name)
        if oldef and oldef.lambda_expression == lambda_expression:
                return t
        if check_redefinition:
                if oldef and oldef.kind is _macro: ## XXX: WTF does oldef mean as a function object attribute?
                        warn_incompatible_redefinition(name, "function", "macro")
                elif oldef and oldef.kind is _function:
                        warn_possible_redefinition("DEFUN", name)
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
                        warn_possible_redefinition("DEFMACRO", name)
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
        if (global_variable_constant_p(name)
            and variable_scope[name].value != value):
                error("The constant %s is being redefined (from %s to %s).", name, variable_scope[name].value, value)
        var = variable(the(symbol_t, name), _constant)
        var.value = value
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
        return t if (isinstance(form, (int, float, complex, str))                       or
                     keywordp(form)                                                     or
                     (isinstance(form, symbol_t) and global_variable_constant_p(form))  or
                     (isinstance(form, list) and len(form) is 2 and form[0] is _quote)) else nil

# Action: populate global scope with pre-defined functions

intern_and_bind(("_mult",   "*"),
                ("_add",    "+"),
                ("_sub",    "-"),
                ("_equals", "="),
                ("_gt",     ">"))

@defun(_mult)
def mult(*xs):
        return reduce(operator.mul, xs)

@defun(_add)
def add(*xs):
        return 0 if not xs else reduce(operator.add, xs)

@defun(_sub)
def sub(*xs):
        return (0      if not xs       else
                -xs[0] if len(xs) is 1 else
                reduce(operator.sub, xs))

@defun(_equals)
def equals(x, y):
        return t if x == y else nil

@defun(_gt)
def gt(x, y):
        return t if x > y else nil

@defun
def evenp(x):
        return nil if x % 2 else t

@defun
def plusp(x):
        return t if x > 0 else nil

def post_factum_defun(symbol, function):
        set_function_definition(globals(), symbol,
                                lambda_expression = nil, check_redefinition = nil)(function)

def populate_compilation_environment_from_package(package):
        for sym in package.own:
                if sym.function:
                        post_factum_defun(sym, sym.function)
                value, presentp = gethash(sym, __global_scope__)
                if presentp:
                        compiler_defvar(sym, value)

populate_compilation_environment_from_package(__cl)

compiler_defconstant(t,   t)
compiler_defconstant(nil, nil)

EmptyDict = dict()
EmptySet = frozenset()

# Compiler globals

intern_and_bind(
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
        "*TOP-COMPILATION-UNIT-P*", gvarp = t)

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
fns = dict()

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
                     globalp = nil,
                     args_types = None, values_types = None,
                     effects = None, affected = None):
                check_type(arglist, list_t)
                args_types = [t] * length(arglist)
                if name: ## Not anonymous LAMBDA?
                        ifname = interpret_function_name(the((or_t, symbol_t, cons_t), name))
                        if globalp:
                                if name in fns:
                                        error("Asked to overwrite FN record %s.", pp_consly(name))
                                fns[ifname] = self
                attrify_args(self, locals(), "name",
                              "arglist", "args_types", "values_types",
                              "effects", "affected")
                self.dependents, self.dependencies = (collections.defaultdict(set),
                                                      collections.defaultdict(set))
        def add_dependent(self, reason, depee):
                self.dependents[reason].add(depee)
                depee.dependencies[reason].add(self)
        def map_dependents(self, f):
                for reason, depees in self.dependents:
                        for d in depees:
                                f(d, reason)
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
                                              (prop_test, lambda *_: t))
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
def unit_variable_rtname(x):
        return full_symbol_rtname(x)
def unit_function_rtname(x):
        ifname = interpret_function_name(x)
        symbol_value(_unit_functions_).add(ifname)
        return ensure_function_rtname(x)
def unit_symbol_rtname(x):
        symbol_value(_unit_symbols_).add(x)
        return ensure_symbol_rtname(x)

def unit_note_gfun_reference(x):
        symbol_value(_unit_gfuns_).add(interpret_function_name(x))
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

def with_compilation_unit(f, override = nil, id = "UNIT-"):
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
                                ret = f()
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
                                ret = f()
                                succeeded_p = t
                                return ret
                        finally:
                                summarize_compilation_unit(not succeeded_p)
                                # dprintf("############################################  ..left %s", id)

# Setup machinery

def undefined_function(name):
        error("Function not defined: %s.", name)

# Macroexpansion

intern_and_bind("DEFMACRO", "EVAL-WHEN", "DEFVAR", "IMPL-REF", "IMPL-CALL")

ensure_function_rtname(_defmacro) ## This is only needed due to the special definition of DEFMACRO.
@set_macro_definition(globals(), _defmacro, nil)
# ((intern("DEFMACRO")[0], " ", (_satisfies, namep), " ", ([(_notlead, " "), (_satisfies, _namep)],),
#   [(_lead, 1), (_notlead, "\n"), (_bound, form)]))
def DEFMACRO(name, lambda_list, *body):
        l, l_ = list_, list__
        return l(_eval_when, l(_compile_toplevel, _load_toplevel, _execute),
                 ## Unregistered Issue MATCH-FAILURE-POINTS-INTO-THE-FAILED-SEX-AND-PATTERN-NOT-AT
                 # (function, (def_, name, lambda_list) + body),
                 l(_ir_args,
                   l(_function, l_(_lambda, lambda_list, consify_linear(body))),
                   l("pydecorators", ir_cl_call("set_macro_definition", ir_apply("globals"),
                                                 l(_quote, name),
                                                 l(_quote, l_(_lambda, lambda_list, l(gensym("FAKE-BODY")) # consify_linear(body)
                                                              )))),
                   ["globalp", t],
                   ["name", name]))

@set_macro_definition(globals(), _defvar, nil)
def DEFVAR(name, value = nil, documentation = nil):
        l, l_ = list_, list__
        return l(_eval_when, l(_compile_toplevel, _load_toplevel, _execute),
                 l(_funcall, l(_function, l(_quote, l("cl", "compiler_defvar"))), l(_quote, name), value),
                 nil)

@set_macro_definition(globals(), _impl_ref, nil)
def IMPL_REF(x):
        l, l_ = list_, list__
        if not stringp(x):
                error("In IMPL-REF %s: argument must be a single string.", pp_consly(x))
        return l(_ref, l(_quote, l("cl", x)))

@set_macro_definition(globals(), _impl_call, nil)
def IMPL_CALL(x, *args):
        l, l_ = list_, list__
        if not stringp(x):
                error("In IMPL-CALL: argument must be a single string.", pp_consly(x))
        return l_(_funcall, l(_function, l(_quote, l("cl", x))), consify_linear(args))


class macroexpander_t(type(lexenv_walker)):
        pass

macroexpander = macroexpander_t()

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
        def lambda_call_knownifier(form):
                return lambda *_: list__(_funcall, list_(_function, form[0]), form[1])
        expander, args = (((form
                            and ((macro_function(form[0], env)
                                  or (compilerp
                                      and knownifier_and_maybe_compiler_macroexpander(form, find_known(form[0]))))
                                                              if isinstance(form[0], symbol_t)      else
                                 lambda_call_knownifier(form) if ir_lambda_p(form[0]) and compilerp else
                                 nil)),
                           form[1])                               if consp(form)                else
                          (symbol_macro_expander(form, env), nil) if isinstance(form, symbol_t) else ## Notice, how NIL is not expanded.
                          (nil, nil))
        ret = mxed, xformp = ((form, nil) if not expander else
                              (expander(*vectorise_linear(args)), t))
        if (xformp and symbol_value(_compiler_trace_subexpansion_)):
                dprintf(";;; %s - macroexpanded ............\n%s   -->\n%s\n",
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
        return walk_with_lexenv(macroexpander_xform, sex, lexenv = lexenv, matcher = macroexpander, bind_after_fn = t)

def compiler_macroexpand_all(form, lexenv = nil, desc = "COMPILER-MACROEXPAND-ALL"):
        with progv({ _macroexpander_compilerp_: t }):
                expanded = macroexpand_all(form, lexenv = lexenv)
        if symbol_value(_compiler_trace_macroexpanded_):
                if form != expanded:
                        # dprintf("  MX  %s   --->\n%s", pp_consly(form), pp_consly(macroexpanded))
                        report(expanded, "macroexpanded", desc = desc, lexenv = _null)
                else:
                        dprintf(";;;%s macroexpansion had no effect", sex_space(-3, ";"))
        return expanded

# Known IR

## Known names.
intern_and_bind("IR-ARGS", "FUNCALL", ("_let_", "LET*"),
                "FLET", "LABELS", "MACROLET",
                "SYMBOL-MACROLET", "BLOCK", "RETURN-FROM",
                "TAGBODY", "GO", "EVAL-WHEN",
                "SETQ", "PROGN", "IF",
                "LET", "FUNCTION", "UNWIND-PROTECT",
                "REF", "LAMBDA", "PRIMITIVE",
                "APPLY", "QUOTE", "MULTIPLE-VALUE-CALL",
                "CATCH", "THROW", "NTH-VALUE",
                "PROGV", "PROTOLOOP", "THE",
                "LOCALLY", "MULTIPLE-VALUE-PROG1", "LOAD-TIME-VALUE")

class known():
        def rewrite(mach, orig, *_):
                return nil, orig
        def binder(exp, continuation):
                with progv({ _walker_binder_: lexenv_walker.binder(0, "Generic binder.") }):
                        return continuation(exp)

def compute_default_metasex(name):
        "Return a default metasex form for an IR with NAME."
        return (name, [" ", (_form,)])

def defknown(metasex_or_class, name = None):
        def do_def(cls, sym, rtname, metasex):
                orig = global_(rtname, globals())
                ## Complete, record the deeds.
                metasex = preprocess_metasex(metasex)
                cls.name           = sym
                cls.metasex        = rewrite_metasex(metasex)
                cls.metasex_pp     = rewrite_metasex(metasex, for_pp = t)
                cls.metasex_bind   = rewrite_metasex(metasex, for_bind = t)
                sym.known          = cls
                define_global_sym_for_rtname(globals(), rtname, sym)
                return orig # pass through -- let the symbol be bound to the name
        def def_(cls, name = name, metasex = metasex_or_class):
                rtname = cls.__name__
                sym     = intern(frost.python_name_lisp_symbol_name(rtname))[0]
                name    = defaulted(name, sym, symbol_t)
                metasex = defaulted(metasex, compute_default_metasex(name))
                return do_def(cls, name, rtname, metasex)
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

def rewrite_1(preform) -> "(Bool ({} Form Bool))":
        "All non-atom forms are interpreted as knowns at this point."
        # dprintf("%s(%s) calling %s.REWRITE(%s, ...)", caller_name(3), pp_consly(caller_args(3)), preform[0], pp_consly(preform))
        ## 1. Atoms are not rewritten.
        if atom(preform):
                return nil, preform
        ## 2. Process IR-ARGS
        form, keys = ((preform,       {}) if preform[0] is not _ir_args else
                      (preform[1][0], dict(vectorise_linear(preform[1][1]))))
        if atom(form) and keys:
                error("In a fit of madness, somebody provided keys to an atom: %s", pp_consly(preform))
        ## 3. Find the corresponding known.
        if not symbolp(form[0]):
                error("Invalid form: %s", pp_consly(form))
        known = find_known(form[0])
        if not known:
                error("Unknown form encountered at rewrite stage: %s, form[0]: %s/%x", pp_consly(form), form[0], id(form[0]))
        ## 4. Check known parameter/argument count correspondence.
        machine = symbol_value(_machine_)
        rewrite_method = known.rewrite
        args = vectorise_linear(form[1])
        validate_function_keys(form[0], rewrite_method, keys)
        validate_function_args(form[0], rewrite_method, [machine, form] + args)
        ## 5. Invoke the rewrite method.
        ret = _, r = rewrite_method(machine, form, *args, **keys)
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
        return walk_with_lexenv(rewriter_xform, sex, lexenv = lexenv, matcher = rewriter, bind_after_fn = nil)

def ir_lambda_p(x):
        return consp(x) and x[0] is _lambda

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

def implrefname_p(x):
        return consp(x) and isinstance(x[0], str)

def implref_p(x):
        return (consp(x) and x[0] is _quote and consp(x[1])
                and implrefname_p(x[1][0]))

intern_and_bind("APPLY", "FUNCALL")

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
                                       l(_quote, func)    if implrefname_p(func)   else
                                       func)),
                 *(args + (l(_quote, nil),)))

def ir_cl_call(name, *args):
        return ir_apply(list_(_quote, list_("cl", name)), *args)

_allow_other_keys_ = intern("ALLOW-OTHER-KEYS", __keyword)[0]

intern_and_bind("*MACHINE*", gvarp = t)

with disabled_condition_system():
        import primitives as p

def variable_tn(sym, globalp = nil): return p.name((unit_variable_rtname if globalp else ensure_variable_rtname)(sym))
def function_tn(sym, globalp = nil): return p.name((unit_function_rtname if globalp else new_function_rtname)(sym))
def   symbol_tn(sym):                return p.name(unit_symbol_rtname(sym))

def gensym_tn(x = "G"):
        sym = gensym(x)
        sym.tn = variable_tn(sym)
        return sym

def make_keyword_tn(name):
        kw = make_keyword(name)
        kw.tn = symbol_tn(kw)
        return kw

with disabled_condition_system():
        import py
        import tri
        pymach  = py.py()
        cfgmach = tri.tri()

string_set("*MACHINE*", pymach)

target_machine = defwith("target_machine",
                         lambda self: dynamic_scope_push({ _machine_: self.machine }),
                         lambda *_:   dynamic_scope_pop(),
                         __init__ = (lambda self, machine:
                                             self.__dict__.update({ "machine": machine })))

__primitiviser_map__ = { str:        (nil, p.string),
                         int:        (nil, p.integer),
                         float:      (nil, p.float_num),
                         ## Note: this relies on the corresponding name to be made available by some means.
                         bool:       (nil, lambda x: p.name("True" if x else "False")),
                         NoneType:   (nil, lambda x: p.name("None")),
                         symbol_t:   (nil, lambda x: p.symbol(unit_symbol_rtname(x))),
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

# Metasex tests

def matcher_result_printer(x):
        return ((("%s\n%s\n%s" % (x[0], pp_consly_pp_str(x[1]), pp_consly(x[2])))
                 if len(x) is 3 else
                 ("%s\n%s" % (pp_consly(x[0]), pp_consly(x[1]))))
                                                                          if isinstance(x, tuple) else
                matcher_pp(x)                                            if isinstance(x, dict)  else
                matcher_pp(x)                                            if consp(x)             else
                str(x))

def dbgsetup(**keys):
        compiler_dbgconf(pretty_full = t,
                         **keys)

def no_debug():
        dbgsetup(forms = nil,
                 
                 subexpansion = nil,
                 macroexpanded = nil,
                 
                 subrewriting = nil,
                 rewritten = nil,
                 
                 subprimitivisation = nil,
                 primitives = nil,
                 
                 subastification = nil,
                 module_ast = nil)
        
def summary_debug():
        dbgsetup(forms = t,
                 macroexpanded = t,
                 rewritten = t,
                 primitives = t,
                 module_ast = t)

def full_debug():
        dbgsetup(forms = t,
                 
                 subexpansion = t,
                 macroexpanded = t,
                 
                 subrewriting = t,
                 rewritten = t,
                 
                 subprimitivisation = t,
                 primitives = t,
                 
                 subastification = t,
                 module_ast = t)

intern_and_bind("CAR", "CDR", "LET", "&BODY")
def run_tests_metasex():
        printer = matcher_result_printer
        def do_run_test(input, matcher = metasex_pprinter):
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
        dprintf("; testing the MetaSEX engine:")   ## Quirky stuff reference mode ON!  Go go Arachna!
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

        intern_and_bind("PI")
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

if getenv("CL_RUN_TESTS") == "t" and getenv("CL_TEST_METASEX") == "t":
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
### LABELS               -> =(FLET,DEF,APPLY) ?
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

# IR argument passing

intern_and_bind("AREF", "VECTOR", "INLINE", ## For LABELS
                )

@defknown((_ir_args, "\n", (_form, (_for_matcher_layers_skip_action, (rewriter, metasex_mapper))),
           ["\n", (_cons, (_typep, str), (_form, (_for_not_matchers_xform, identity, metasex_pprinter)))],))
class ir_args(known):
        def primitivise(*_):               error("Invariant failed: IR-ARGS is not meant to be lowered.")
        def effects(*ir,  **args):         return ir_effects(ir)
        def affected(*ir, **args):         return ir_affected(ir)

def destructure_possible_ir_args(x):
        "Maybe extract IR-ARGS' parameters, if X is indeed an IR-ARGS node, returning them as third element."
        return ((t,   x[1][0], x[1][1]) if consp(x) and x[0] is _ir_args else
                (nil, x,       nil))

def ir(*ir, **keys):
        "A syntactic sugar for convenient IR-ARGS specification."
        known = find_known(the(symbol_t, ir[0]))
        if not known:
                error("Unknown form being passed IR arguments: %s", pp_consly(consify_linear(ir)))
        validate_function_keys("IR %s primitivise method" % known.name, known.primitivise, keys)
        return (list__(_ir_args, consify_linear(ir), consify_linear([ [k, v] for k, v in keys.items() ])) if keys else
                consify_linear(ir))

# FUNCALL

@defknown
class funcall(known):
        def rewrite(mach, _, func, *args):
                return t, list__(_apply, func, append(consify_linear(args), list_(list_(_quote, nil))))

# LET*

#       LET* is a funky kind of a rewrite, as its intricate lexenv-estry affects macroexpansion non-trivially,
#       and so, a simple fire-and-forget BINDS method will not work at all.
#       It could have been avoided altogether, if we'd committed a blasphemy of rewriting it into oblivion
#       before MACROEXPAND-1 could have seen it.  But CL exposes macroexpansion, and users will not be amused :-)

@defknown((_binder, _let_,
           (_let_, " ",  ([(_notlead, "\n"), (_bind, _variable, (_or, (_satisfies, namep), ((_satisfies, namep), " ", (_bound, (_form,)))))],),
            [(_lead, 1), (_notlead, "\n"), (_bound, (_form,))])),
          name = _let_)
class let_(known):
        def rewrite(mach, _, bindings, *body):
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
                self.name, self.lambda_list = the((or_t, symbol_t, cons_t), name), the(list_t, lambda_list)
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

def tnify_function_arglist(fname, lexenv, arglist):
        frames = [ getattr(lexenv, x)["name"] for x in ["varframe", "funcframe"]
                   if getattr(lexenv, x) ]
        def tnify(x, error_if_none = True):
                for f in frames:
                        if x in f:
                                return f[x].tn
                if error_if_none:
                        error("While processing arglist of %s: %s is not in the lexenv.", fname, x)
        return (tnify(arglist[0], error_if_none = False),) + tuple(tnify(x) for x in arglist[1:])

def rewrite_lambda(mach, lexenv, clambda, lam, body) -> "conslist of rewritten":
        # Unregistered Issue COMPLIANCE-MACRO-LAMBDA-LIST-DESTRUCTURING-AND-ENV
        (whole, fixed, optional, rest, keys, aux), (optdefs, keydefs, auxforms) = \
            args, forms = clambda.args, clambda.forms,
        complexp = not not (optional or rest or clambda.keysp or aux)
        if not complexp: ## Only has &WHOLE and fixed args:
                return cons(lam, consify_linear(body))
        ## Optimisations
        # constant_forms_p = all(constantp(x) for x in optdefs + keydefs + auxforms)
        opt_gsyms        = [ gensym_tn("OPT-" + symbol_name(x) + "-")
                             for x in optional ]
        need_rest        = rest or optional or clambda.keysp ## &key is processed through parsing of *rest
        rest_gsym        = ((gensym_tn("REST-" + (symbol_name(rest) if rest else
                                                  "TN") + "-")) if need_rest else
                            None)
        nrest_gsym       = gensym("REST-LEN")
        need_nonopt_rest = (rest or clambda.keysp) and optional
        nonopt_rest_gsym = gensym("NONOPT-REST") if need_nonopt_rest else rest_gsym
        must_check_keys  = clambda.keysp and not clambda.aokp
        ### This is the biggest victim of IR representation mixing (cons vs. linear-tuple),
        ### all in the name of enrolling python lambda lists on board.
        l, l_, a = list_, list__, append
        return l(a(l(whole) if whole else nil,
                   consify_linear(fixed),
                   l(_rest, rest_gsym) if need_rest else nil),
                 # l(_funcall, l(_function, l(_quote, l("cl", "dprintf"))),
                 #   ("fixed:%s    optional:%s    rest:%s"
                 #    % (" %s"*len(fixed), " %s"*len(optional), (" %s" if need_rest else ""))),
                 #   *(fixed + opt_gsyms + ([rest_gsym] if need_rest else []))),
                 ## REST argument must be indexable
                 l(_let_, a((l(l(nrest_gsym, mach.vararg_count(rest_gsym))) if optional else nil),
                            consify_linear(l(name, l(_if, l(_primitive, p.lt, i, nrest_gsym),
                                                          l(_primitive, p.index, rest_gsym, i),
                                                          def_expr))
                                           for i, name, gs, def_expr
                                           in zip(range(len(optional)), optional, opt_gsyms, optdefs)),
                            (l(l(nonopt_rest_gsym, mach.vararg_subseq(rest_gsym, len(optional)))) if need_nonopt_rest else nil),
                            (l(l(rest, mach.vector_consifier(nonopt_rest_gsym)))
                             if rest else nil),
                            (mach.keyword_binding_checking(nonopt_rest_gsym, keys, keydefs, must_check_keys = must_check_keys)
                             if clambda.keysp else nil),
                            consify_linear(l(name, form)
                                           for name, form in zip(aux, auxforms))),
                   *body))

def lower_lambda(mach, lexenv, clambda, lam, body, name = nil, pydecorators = nil, self_binding = nil, globalp = nil,
                 function_namespace = nil):
        (whole, fixed, optional, rest, keys, aux), (optdefs, keydefs, auxforms) = \
            args, forms = clambda.args, clambda.forms
        assert not (whole or optional or clambda.keysp or aux) ## &WHOLE is a piece of debt, currently.
        check_type(mach, p.machine)
        fnname_tn = function_tn(name, globalp = globalp) if name else None
        ## Things, that are needed:
        ##  - CLAMBDA passing
        ifname = interpret_function_name(name)
        varframe, funcframe = ((dict((b, variable_binding(b, _variable, None))
                                     for b in clambda.total),
                                dict([(ifname, self_binding)] if self_binding else []))
                               if not function_namespace else
                               (dict(),
                                dictappend(dict((b, function_binding(b, _function, None))
                                                for b in clambda.total),
                                           dict([(ifname, self_binding)] if self_binding else []))))
        ## We've wedged stuff into a single LEXENV, where in fact it is an intricate LET*.
        ## This is probably gonna bite sooner or later.
        fn_lexenv = make_lexenv(lexenv, clambda = clambda, allocate_tns = t,
                                name_varframe  = varframe,
                                name_funcframe = funcframe)
        with progv({ _lexenv_: fn_lexenv }):
                primitivised_body = [ primitivise(mach, x) for x in body ]
                ## _now_, that the body was primitivised, the clambda's nonlocal-SETQs were computed:
                nonlocal_decl = ([ py.nonlocal_(*(variable_tn(x)
                                                  for x in sorted(clambda.nonlocal_setqs, key = symbol_name) )) ]
                                 if clambda.nonlocal_setqs else [])
                # dprintf("emitting LAMBDA primitive, name: %s, ifname: %s, fnname_tn: %s", name, ifname, fnname_tn)
                return p.function(fnname_tn, tnify_function_arglist(name, fn_lexenv, [rest or None] + fixed),
                                  p.progn(*nonlocal_decl
                                          + primitivised_body),
                                  id = pp_consly(name) if name else nil,
                                  pydecorators = xmap_to_vector(primitivise, pydecorators))

@defknown((_binder, _flet,
           (_flet, " ", ([(_notlead, "\n"), (_bind, _function,
                                             (_binder, _function,
                                              ((_satisfies, namep), (_funcher, _lambda))))],),
            [(_lead, 1), (_notlead, "\n"), (_bound, (_form,))])),
          name = _flet)
class flet(known):
        def binder_args(bindings, *body):
                return xmap_to_vector(lambda b: fn(b[0], b[1][0]), bindings)
        def rewrite(mach, orig, bindings, *body):
                ## Unregistered Issue FLET-POTENTIALLY-MISSING-LAMBDA-BINDER-MAGIC
                not (listp(bindings)
                     and every(lambda x: (length(x) >= 2 and
                                          listp(second(x))),
                               bindings)) and error("Bad FLET form: %s", pp_consly(orig))
                l = list_
                lexenv = symbol_value(_walker_lexenv_)
                return nil, (handle_constant_linear_body(body) if all(constantp(x) for x in body) else
                             ## This weak attempt above screams for proper liveness analysis.
                             nil                               if not (bindings or body)          else
                             handle_linear_body(body)          if not bindings                    else
                             l(_flet, mapcar(lambda f: cons(f[0], rewrite_lambda(mach, lexenv,
                                                                                 compiler_lambda(f[0], f[1][0]),
                                                                                 f[1][0], vectorise_linear(f[1][1]))),
                                             bindings),
                                *body))
        def primitivise(mach, bindings, *body):
                lexenv          = symbol_value(_lexenv_)
                names, bindings = list(zip(*xmap_to_vector(lambda b: (b[0], b), bindings)))
                clambdas        = [ compiler_lambda(b[0], b[1][0]) for b in bindings ]
                body_name       = "FLET-" + ("-".join(symbol_name(x) for x in names))
                return p.funcall(lower_lambda(mach, lexenv, compiler_lambda(gensym(body_name + "-"),
                                                                      list_(*names)),
                                              names, body,
                                              name = gensym(body_name),
                                              function_namespace = t),
                                 *[ lower_lambda(mach, lexenv, clambda,
                                                 lambda_list, vectorise_linear(body),
                                                 name = name)
                                    for clambda, (name, (lambda_list, body)) in zip(clambdas, bindings) ])

# LABELS

#         Implementation strategy:
#          - Henry Baker's '92  CIRCULAR ENVIRONMENTS OF "LABELS" EMULATED BY "FLET" ALONE

@defknown((_binder, _labels,
           (_labels, " ", ([(_notlead, "\n"), (_binder, _function,
                                               ((_satisfies, namep), (_funcher, _lambda)))],),
            [(_lead, 1), (_notlead, "\n"), (_form,)])),
          name = _labels)
class labels(known):
        def binder(exp, further):
                form = form_real(exp) ## Filter out any potential IR-ARGS
                def fail(x): error("Bad LABELS form: %s", pp_consly(x))
                length(form) < 2 and fail(form)
                bindings = vectorise_linear(form[1][0])
                all((length(x) >= 2 and
                     listp(second(x)))
                    for x in bindings) or fail(form)
                env    = symbol_value(_walker_lexenv_)
                binder = lexenv_walker.binder(0, "Labels.")
                ## No need to bind *WALKER-BINDER*, since we contribute to %BIND metasex expressions.
                with progv({ _walker_lexenv_:
                              make_lexenv(env, allocate_tns = symbol_value(_walker_allocate_tns_),
                                          name_funcframe = { name: function_binding(name, _function, fn(name, lam))
                                                             for name, (lam, body) in bindings }),
                             _walker_binder_: binder,
                             _walker_binder_args_: nil } if bindings else
                           { _walker_binder_: binder,
                             _walker_binder_args_: nil }):
                        return further(exp)
        def rewrite(mach, _, bindings, *body):
                l = list_
                def henry_baker_labels_using_flet_alone():
                        # (defmacro laybells (fns &body forms)
                        #   (let* ((fnames (mapcar #'car fns))
                        #          (fnvec (gensym))
                        #          (findicies (iota-list (length fns)))
                        #          (fbodies (mapcar #'(lambda (f i)
                        #                              `(,f (&rest a) (apply (svref ,fnvec ,i) ,fnvec a)))
                        #                   fnames findicies))
                        #          (fdecls `(declare (inline ,@fnames)))
                        #          (nfbodies (mapcar #'(lambda (f)
                        #                              `#'(lambda (,fnvec ,@(cadr f))
                        #                                   (flet ,fbodies ,fdecls ,@(cddr f))))
                        #                    fns)))
                        #    `(let ((,fnvec (vector ,@nfbodies)))
                        #       (flet ,fbodies ,fdecls ,@forms))))
                        fnames    = xmap_to_vector(first, bindings)
                        fnvec     = gensym("Y")
                        findicies = range(len(fnames))
                        rest      = gensym()
                        fbodies   = [ l(f, l(_rest, rest), l(_apply, l(_primitive, l(_quote, l("index")), fnvec, i), fnvec, rest))
                                      for f, i in zip(fnames, findicies) ]
                        fdecls    = nil ## l(_declare, cons(_inline, consify_linear(fnames)))
                        nfbodies  = [ l(_function, l(_lambda, cons(fnvec, f[1][0]),
                                                     list__(_flet, consify_linear(fbodies), fdecls,
                                                            f[1][1])))
                                      for f in vectorise_linear(bindings) ]
                        return l(_let, l(l(fnvec, list__(_primitive, l(_quote, l("pylist")), consify_linear(nfbodies)))),
                                  list__(_flet, consify_linear(fbodies),
                                          fdecls,
                                          consify_linear(body)))
                return t, (handle_constant_linear_body(body) if all(constantp(x) for x in body) else
                           ## This weak attempt above screams for proper liveness analysis.
                           nil                               if not (bindings or body)          else
                           handle_linear_body(body)          if not bindings                    else
                           henry_baker_labels_using_flet_alone())

# MACROLET

## Unregistered Issue EXTENDED-LAMBDA-LIST-DESTRUCTURING-WRECKS-ALL
@defknown((_binder, _macrolet,
           (_macrolet, " ", ([(_notlead, "\n"),
                              (_bind, _macro,
                               ((_satisfies, namep), " ",
                                ([(_notlead, " "),
                                  (_or, (_satisfies, lambda_word_p),
                                   (_bind, _variable, (_satisfies, namep)),
                                   (_bind, _variable, ((_satisfies, namep), (_bound, (_form,)))),
                                   (_form,))],),
                                [(_lead, 1), (_notlead, "\n"), (_form,)]))],),
            [(_lead, 1), (_notlead, "\n"), (_bound, (_form,))])),
          name = _macrolet)
class macrolet(known):
        def rewrite(mach, _, bindings, *body):
                return t, handle_linear_body(body)

# SYMBOL-MACROLET

@defknown((_binder, _symbol_macrolet,
           (_symbol_macrolet, " ", ([(_notlead, "\n"), (_bind, _symbol_macro, ((_satisfies, namep), " ", (_form,)))],),
            [(_lead, 1), (_notlead, "\n"), (_bound, (_form,))])),
          name = _symbol_macrolet)
class symbol_macrolet(known):
        def rewrite(mach, _, bindings, *body):
                return t, handle_linear_body(body)

# BLOCK

#       Instead of performing an additional mapping operation, which is quadratic,
#       we could rely on the RETURN-FROM marking its presence.

@defknown((_binder, _block,
           (_block, " ", (_bind, _block, (_satisfies, namep)),
            [(_lead, 1), (_notlead, "\n"), (_bound, (_form,))],)),
          name = _block)
class block(known):
        def binder(exp, continuation):
                ## We could avoid the need for this binder, if we could push a %BOUND pattern
                ## directive around the rewritten CATCH form in REWRITE, figuratively speaking.
                ## Unfortunately, as it stands, rewriting blasts the matcher pattern, and the
                ## essential guts are never even considered.
                if length(exp) < 2 or not isinstance(exp[1][0], symbol_t):
                        error("Bad BLOCK form: %s", pp_consly(exp))
                name = exp[1][0]
                nonce = gensym("BLOCK-" + symbol_name(name) + "-")
                with progv({ _walker_binder_: lexenv_walker.binder(0, "Block."),
                             _walker_lexenv_: make_lexenv(symbol_value(_walker_lexenv_),
                                                          name_blockframe = { name: block_binding(name, _block, nonce) }) }):
                        return continuation(exp)
        def rewrite(mach, orig, name, *body):
                consbody = consify_linear(body)
                block_used = nil
                def compute_block_usage(sex, further):
                        nonlocal block_used
                        if consp(sex) and sex[0] is _return_from and sex[1][0] is name:
                                ## Unregistered Issue EARLY-WALKER-TERMINATION
                                block_used = t
                        return further(sex)
                ## Look ahead (inside, actually):
                with progv({ _metasex_kind_: "metasex" }):
                        map_sex(compute_block_usage, cons(_progn, consbody), )
                binding, _ = symbol_value(_walker_lexenv_).lookup_block(the(symbol_t, name))
                nonce = binding.value
                ## Unregistered Issue CATCH-22-WHILE-DOING-CONTENT-DEPENDENT-REWRITING
                return t, (list__(_catch, list_(_quote, nonce), consbody) if block_used else
                           handle_linear_body(body))

# RETURN-FROM

@defknown((_return_from, " ", (_satisfies, namep), (_maybe, " ", (_form,))))
class return_from(known):
        def rewrite(mach, _, name, *maybe_form):
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
        def rewrite(mach, orig, *tags_and_forms):
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
                               l(_let, l(l(nxt_label, l(_function, funs[0][0]))),
                                 l(_protoloop,
                                   l(_setq, nxt_label,
                                            l(_catch, go_tag, l(_apply, nxt_label, l(_quote, nil))))))))))
                return t, form

# GO

@defknown((_go, " ", (_typep, symbol_t)))
class go(known):
        def rewrite(mach, _, name):
                binding, lexenv = symbol_value(_walker_lexenv_).lookup_gotag(the(symbol_t, name))
                if not binding:
                        simple_program_error("attempt to GO to nonexistent tag: %s", name)
                return t, list_(_throw, binding.value, list_(_function, binding.value))

# EVAL-WHEN

## Unregistered Issue EVAL-WHEN-LACKING-SPACE-BETWEEN-KEYWORDS-WHEN-PRINTED
@defknown((intern("EVAL-WHEN")[0], " ", ([(_notlead, " "),
                                         (_or, _compile_toplevel, _load_toplevel, _execute,
                                               _compile, _load, _eval)],),
           [(_lead, 1), (_notlead, "\n"), (_form,)]))
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
        def rewrite(mach, _, when, *body):
                ctop, ltop, exec = parse_eval_when_situations(when)
                ## This handles EVAL-WHEN in non-top-level forms. (EVAL-WHENs in top
                ## level forms are picked off and handled by PROCESS-TOPLEVEL-FORM,
                ## so that they're never seen at this level.)
                compiler_trace_known_choice(_eval_when, when, "EXECUTE" if exec else "NO-EXECUTE")
                return t, (cons(_progn, consify_linear(body)) if exec else
                           nil)

# SETQ

@defknown((_setq, [(_poscase,
                    (1, " "),
                    (3, 5),
                    (t, "\n")), (_or, (_satisfies, namep), (_satisfies, implref_p)), " ", (_form,)]))
class setq(known):
        def rewrite(mach, orig, *args):
                ## Actually a normalisation.. or actually, the hell knows what it is..
                len(args) % 2 and \
                    error("SETQ accepts an even amount of arguments, got: %s", pp_consly(consify_linear(args)))
                not all(implref_p(x) or isinstance(x, symbol_t)
                        for x in args[::2]) and \
                    error("SETQ arguments at even positions must be symbols, got: %s", pp_consly(consify_linear(args)))
                return ((nil, orig) if len(args) == 2 else
                        (t,   handle_linear_body(list(map(lambda name, value: list_(_setq, name, value),
                                                          args[::2], args[1::2])))))
        def nvalues(_, __):                                               return 1
        def nth_value(n, orig, _, value):                                 return orig if n is 0 else list_(_progn, orig, nil)
        ## Unregistered Issue COMPLIANCE-ISSUE-SETQ-BINDING
        ## Unregistered Issue COMPLIANCE-SETQ-MULTIPLE-ASSIGNMENTS-UNSUPPORTED
        def primitivise(mach, name, value):
                if implref_p(name):
                        return p.assign(mach.primitivise_implref(name), primitivise(mach, value))
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
                        return p.special_setq(p.name(unit_symbol_rtname(name)), primitivise(mach, value))
                compiler_trace_known_choice(_setq, name, "LEXICAL")
                if cur_lexenv.clambda is not tgt_lexenv.clambda:
                        # dprintf("NONLOCAL SETQ of %s from %s by %s", name, tgt_lexenv.clambda, cur_lexenv.clambda)
                        cur_lexenv.clambda.nonlocal_setqs.add(name)
                return p.assign(lexical_binding.tn, primitivise(mach, value))
        def effects(name, value):         return t
        def affected(name, value):        return ir_affected(value)

# PROGN

@defknown((_progn,
           [(_lead, 1), (_notlead, "\n"), (_form,)]))
class progn(known):
        def rewrite(mach, _, *body):
                rewrote, form = rewrite_linear_body(body)
                return rewrote, form
        def nvalues(*body):            return 1   if not body else ir_nvalues(body[-1])
        def nth_value(n, orig, *body): return nil if not body else ir_nth_valueify_last_subform(n, orig)
        def primitivise(mach, *body):
                return (p.progn(*(primitivise(mach, x) for x in body)) if body else
                        primitivise(mach, nil))
        def effects(*body):            return any(ir_effects(f) for f in body)
        def affected(*body):           return any(ir_affected(f) for f in body)

# IF

@defknown((_if, " ", (_form,),
           3, (_form,),
          (_maybe, "\n", (_form,))))
class if_(known):
        def rewrite(mach, orig, test, consequent, *maybe_ante):
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
        def primitivise(mach, test, consequent, antecedent):
                return p.if_(primitivise(mach, test),
                             primitivise(mach, consequent),
                             primitivise(mach, antecedent))
        def effects(*tca):  return any(ir_effects(f)  for f in tca)
        def affected(*tca): return any(ir_affected(f) for f in tca)

# LET

@defknown((_binder, _let,
           (_let, " ",  ([(_notlead, "\n"), (_bind, _variable, (_or, (_satisfies, namep), ((_satisfies, namep), " ", (_form,))))],),
            [(_lead, 1), (_notlead, "\n"), (_bound, (_form,))])),
          name = _let)
class let(known):
        def rewrite(mach, orig, bindings, *body):
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
        def primitivise(mach, bindings, *body):
                # Unregistered Issue PRIMITIVE-DECLARATIONS
                # Unregistered Issue DEAD-CODE-ELIMINATION
                normalised = vectorise_linear(bindings)
                env = make_lexenv_varframe(list(zip(*normalised))[0], allocate_tns = t)
                with progv({ _lexenv_: env }):
                        guts = p.progn(*(primitivise(mach, x) for x in body))
                return p.let(list((env.varframe["name"][name].tn, primitivise(mach, form))
                                  for name, (form, __) in normalised),
                             guts)
        def effects(bindings, *body):
                ## Unregistered Issue LET-EFFECT-COMPUTATION-PESSIMISTIC
                return any(ir_effects(f) for f in (x[1][0] for x in bindings) + body)
        def affected(bindings, *body):
                return any(ir_affected(f) for f in (x[1][0] for x in bindings) + body)

# FUNCTION

## Unregistered Issue MACROEXPANDABILITY-OF-FUNCTION-SUBFORM-IS-INTERESTING
@defknown((_binder, _function,
           (_function, " ", (_or,
                             (_lambda, (_funcher, _lambda)),
                             (_satisfies, namep),
                             (_satisfies, implref_p),
                             (_setf, (_satisfies, namep))))))
class function(known):
        def binder(exp, further, name = nil, pydecorators = nil, globalp = nil):
                form = form_real(exp)
                x = form[1][0]
                if atom(x) or x[0] is not _lambda:
                        return further(exp)
                length(x) < 2 and error("Bad LAMBDA form: %s", pp_consly(form))
                lam = x[1][0]
                not listp(lam) and error("Bad lambda list in LAMBDA form: %s", pp_consly(exp))
                clambda = compiler_lambda(name, lam)
                with progv(dict([ (_walker_binder_, lexenv_walker.binder(0, "Lambda.")),
                                  (_walker_binder_args_, nil),
                                  (_walker_lexenv_,
                                   make_lexenv(symbol_value(_walker_lexenv_), clambda = clambda,
                                               allocate_tns = symbol_value(_walker_allocate_tns_),
                                               ## Unregistered Issue PER-PHASE-FN-ALLOCATION-FOR-SAME-CODE
                                               name_funcframe = { name: function_binding(name, _function, fn(name, lam)) }))
                                 ])):
                        return further(exp)
        def rewrite(mach, orig, x, name = nil, pydecorators = nil, globalp = nil):
                lambdap = ir_lambda_p(x)
                if lambdap:
                        if not (listp(x[1][0]) and listp(x[1][1])):
                                error("Invalid #'LAMBDA form: %s", pp_consly(orig))
                        lambda_list, body = x[1][0], x[1][1]
                return nil, (ir(_function, cons(_lambda, rewrite_lambda(mach, symbol_value(_walker_lexenv_),
                                                                        compiler_lambda(name, lambda_list),
                                                                        lambda_list, vectorise_linear(body))),
                                **dictappend({ "name":         name }         if name         else {},
                                             { "pydecorators": pydecorators } if pydecorators else {},
                                             { "globalp":      globalp }      if globalp      else {}))
                             if lambdap else
                             orig)
        ## Unregistered Issue COMPLIANCE-FUNCTION-NAMESPACE-SEPARATION
        def nvalues(_):            return 1
        def nth_value(n, orig, _): return orig if n is 0 else nil
        def primitivise(mach, x, name = nil, pydecorators = nil, globalp = nil):
                ## (QUOTE ("str"))
                if implref_p(x):
                        return mach.primitivise_implref(x)
                lambdap = ir_lambda_p(x)
                if lambdap:
                        lambda_list, body = x[1][0], x[1][1]
                        return lower_lambda(mach, symbol_value(_lexenv_),
                                            compiler_lambda(name, lambda_list),
                                            lambda_list, vectorise_linear(body),
                                            name = name, pydecorators = pydecorators,
                                            globalp = globalp,
                                            self_binding = function_binding(name, _function, fn(name, lambda_list)))
                lexical_binding, lexenv = symbol_value(_lexenv_).lookup_func(x)
                if not lexical_binding:
                        ## Unregistered Issue FDEFINITION-SYMBOL-FUNCTION-AND-COMPILER-GFUNS-NEED-SYNCHRONISATION
                        if not find_global_function(x):
                                simple_style_warning("undefined function: %s", x)
                        unit_note_gfun_reference(x)
                # dprintf("-=-=-=-  FUNCTION  lexicalp - %s - %s",
                #         lexical_binding, (function_tn(x, globalp = t) if not lexical_binding else
                #                           lexical_binding.tn))
                return (function_tn(x, globalp = t) if not lexical_binding else
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

@defknown((_unwind_protect,
           3, (_form,),
           [(_lead, -2), (_notlead, "\n"), (_form,)]))
class unwind_protect(known):
        def rewrite(mach, orig, form, *unwind_body):
                return ((t, cont(form)) if not unwind_body or all(constantp(x) for x in unwind_body) else
                        (lambda gs: (t, list_(_let, list_(list_(gs, form)),
                                              *(unwind_body
                                                + (gs,)))))
                        (gensym("UWP-CONSTANT-VALUE-")) if constantp(form) else
                        (nil, orig))
        def nvalues(form, *unwind_body):            return ir_nvalues(form)
        def nth_value(n, orig, form, *unwind_body): return list__(_unwind_protect, ir_nth_value(n, form),
                                                                  consify_linear(unwind_body))
        def primitivise(mach, form, *unwind_body):
                return p.unwind_protect(primitivise(mach, form),
                                        p.progn(*(primitivise(mach, x) for x in unwind_body)))
        def effects(form, *unwind_body):
                return any(ir_effects(f) for f in (form,) + body)
        def affected(form, *unwind_body):
                return any(ir_affected(f) for f in (form,) + body)

# REF

@defknown((_ref, " ", (_or, (_satisfies, namep), (_satisfies, implref_p))))
class ref(known):
        def rewrite(mach, orig, x):
                implrefp = implref_p(x)
                return not implrefp, (orig if implrefp else x)
        def nvalues(_):            return 1
        def nth_value(n, orig, _): return orig if n is 0 else nil
        def primitivise(mach, name):
                if implref_p(name):
                        return mach.primitivise_implref(name)
                cur_lexenv = symbol_value(_lexenv_)
                lexical_binding, src_lexenv = cur_lexenv.lookup_var(the(symbol_t, name))
                if not lexical_binding or lexical_binding.kind is _special:
                        gvar = find_global_variable(name)
                        if not gvar and not lexical_binding: # Don't complain on yet-unknown specials.
                                simple_style_warning("undefined variable: %s", name)
                        unit_note_gvar_reference(name)
                        ## Note, how this differs from FUNCTION:
                        return p.special_ref(p.name(unit_symbol_rtname(name)))
                if cur_lexenv.clambda is not src_lexenv.clambda:
                        cur_lexenv.clambda.nonlocal_refs.add(name)
                return lexical_binding.tn
        def effects(name):         return nil
        def affected(name):        return not global_variable_constant_p(name)

# PRIM

@defknown((_primitive, " ", (_or, (_satisfies, p.prim_type_p), (_satisfies, implref_p)), [" ", (_form,)]))
class primitive(known):
        def rewrite(mach, orig, prim, *args, **keys):
                prim, implrefp = ((prim, nil) if not implref_p(prim) else
                                  (p.find_primitive(prim[1][0][0], mach), t))
                return nil, (ir(_primitive, prim, *args, **keys) if keys     else
                             list_(_primitive, prim, *args)      if implrefp else
                             orig)
        def nvalues(*_):            return 1
        def nth_value(n, orig, *_): return orig if n is 0 else nil
        def primitivise(mach, prim, *args, **keys):
                return prim(*(x         if isinstance(x, p.prim)       else
                              (error("Invalid quoting within PRIMITIVE known form: %s", cons(prim, consify_linear(args)))
                               if not consp(x[1]) else
                               x[1][0]) if consp(x) and x[0] is _quote else
                              primitivise(mach, x)
                              for x in args),
                             machine = mach,
                             **keys)
        def effects(*_):            return t
        def affected(*_):           return t

# APPLY

@defknown((_apply, " ", ".", (_form,), (_fill,), (_form,), [(_fill,), (_form,)]))
class apply(known):
        def nvalues(func, _, *__):            return ir_function_form_nvalues(func)
        def nth_value(n, orig, func, _, *__): return ir_function_form_nth_value_form(n, func, orig)
        def primitivise(mach, func, arg, *args):
                ## Unregistered Issue IMPROVEMENT-APPLY-COULD-VALIDATE-CALLS-OF-KNOWNS
                fixed, rest = (((),                 arg)       if not args                  else
                               ((arg,) + args[:-1], args[-1]))
                ## Note, how the test below is too weak to be useful:
                ## the comparison against a literal NIL is much weaker than a NULL type membership test.
                ## Therefore, the important evolutionary question, is what kind of preparations are
                ## required to make such type analysis viable.
                if rest is nil or rest == list_(_quote, nil):
                        return p.funcall(primitivise(mach, func), *(primitivise(mach, x) for x in fixed))
                else:
                        return p.apply(primitivise(mach, func), *(list(primitivise(mach, x) for x in fixed)
                                                             + [ p.funcall(py.impl_ref("vectorise_linear"),
                                                                           primitivise(mach, rest)) ]))
        def effects(func, arg, *args):
                return (any(ir_effects(arg) for arg in (func, arg) + args) or
                        ir_depending_on_function_properties(func, lambda fn, effects: effects, "effects"))
        def affected(func, arg, *args):
                return (any(ir_affected(arg) for arg in (func, arg) + args) or
                        ir_depending_on_function_properties(func, lambda fn, affected: affected, "affected"))

# QUOTE

@defknown((_quote, " ", (_form, (_for_not_matchers_xform, identity, metasex_pprinter))))
class quote(known):
        def nvalues(_):            return 1
        def nth_value(n, orig, _): return orig if n is 0 else nil
        def primitivise(mach, x):
                # Unregistered Issue COMPLIANCE-QUOTED-LITERALS
                if isinstance(x, symbol_t) and not constantp(x):
                        compiler_trace_known_choice(_quote, x, "NONCONSTANT-SYMBOL")
                        return p.symbol(unit_symbol_rtname(x))
                else:
                        prim, successp = try_primitivise_constant(x)
                        if successp:
                                compiler_trace_known_choice(_quote, x, "CONSTANT")
                                return prim
                        elif consp(x):
                                compiler_trace_known_choice(_quote, x, "SEX")
                                return p.literal_list(*(primitivise(mach, list_(_quote, x)) for x in vectorise_linear(x)))
                        else:
                                error("QUOTE: cannot handle %s: non-primitivisable constant atom.")
        def effects(x):            return nil
        def affected(x):           return nil

# MULTIPLE-VALUE-CALL

def do_multiple_value_call(fn, values_frames):
        return fn(*reduce(lambda acc, f: acc + f[1:], values_frames, []))

@defknown
class multiple_value_call(known):
        ## We might start considering the argument forms for the values queries,
        ## once we get into the partial evaluation affairs..
        def nvalues(func, *_):            return ir_function_form_nvalues(func)
        def nth_value(n, orig, func, *_): return ir_function_form_nth_value_form(n, func, orig)
        def primitivise(mach, fn, *arg_forms):
                ## We have no choice, but to lower immediately, and by hand.
                ## Unregistered Issue SAFETY-VALUES-FRAME-CHECKING
                return p.apply(primitivise(mach, fn),
                               p.add(*(py.slice(primitivise(mach, x), p.integer(1), nil, nil) for x in arg_forms)))
        def effects(fn, *arg_forms):
                return (any(ir_effects(arg) for arg in arg_forms) or
                        ir_depending_on_function_properties(func, lambda fn, effects: effects, "effects"))
        def affected(fn, *arg_forms):
                return (any(ir_affected(arg) for arg in arg_forms) or
                        ir_depending_on_function_properties(func, lambda fn, affected: affected, "affected"))

# CATCH

@defknown((_catch, " ", (_form,),
          [(_lead, 1), (_notlead, "\n"), (_form,)]))
class catch(known):
        ## Critical Issue CATCH-MULTIPLE-VALUES-NOT-IMPLEMENTED
        def nvalues(_, *body):              return 1 if not body else not_implemented()
        def nth_value(n, orig, tag, *body): return (not_implemented()        if body             else
                                                    list_(_progn, tag, nil)  if ir_effects(tag) else
                                                    nil)
        ## Unregistered Issue DOUBT-WHETHER-LAMBDA-CAN-LOWER-PROLOGUESSLY-DUE-TO-C-L-A-N-T
        def primitivise(mach, tag, *body):
                return p.catch(primitivise(mach, tag),
                               p.progn(*(primitivise(mach, x) for x in body)))
        def effects(tag, *body):  return ir_effects(tag) or any(ir_effects(f) for f in body)
        def affected(tag, *body): return ir_affected(tag) or any(ir_affected(f) for f in body)

# THROW

@defknown((_throw, " ", (_form,), (_maybe, " ", (_form,))))
class throw(known):
        def nvalues(_, value):            return ir_nvalues(value)
        def nth_value(n, orig, _, value): return (list_(_progn, tag, ir_nth_value(value)) if ir_effects(tag) else
                                                  ir_nth_value(value))
        def primitivise(mach, tag, value):
                return p.throw(primitivise(mach, tag), primitivise(mach, value))
        def effects(tag, value):          return ir_effects(tag) or ir_effects(value)
        def affected(tag, value):         return ir_affected(tag) or ir_affected(value)

# NTH-VALUE

@defknown((_nth_value, " ", (_form,), " ", (_form,)))
class nth_value(known):
        def nvalues(_, __):    return 1
        def nth_value(n, orig, form_n, form):
                return (list_(_nth_value, n, orig) if not (integerp(n) and integerp(form_n)) else ## Give up.  Too early?
                        nil                        if n != form_n and not ir_effects(form)   else
                        list_(_progn, form, nil)   if n != form_n                            else
                        ir_nth_value(n, form)) ## We don't risk unbounded recursion here, so let's analyse further..
        def primitivise(mach, n, form):
                return p.funcall(py.impl_ref("values_frame_project"), primitivise(mach, n), primitivise(mach, form))
        def effects(n, form):   return ir_effects(n) or ir_effects(form)
        def affected(n, form):  return ir_affected(n) or ir_affected(form)

# PROGV

@defknown((_progv, " ", ([(_notlead, " "), (_form,)],), " ", ([(_notlead, " "), (_form,)],),
           [(_lead, 1), (_notlead, "\n"), (_form,)]))
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
        def primitivise(mach, vars, vals, *body):
                return p.progv(tuple(primitivise(mach, x) for x in vars), tuple(primitivise(mach, x) for x in vals),
                               p.progn(*(primitivise(mach, x) for x in body)))
        def effects(names, values, *body):            return any(ir_effects(f) for f in (names, values) + body)
        def affected(names, values, *body):           return any(ir_affected(f) for f in (names, values) + body)

# PROTOLOOP

@defknown((_protoloop,
           [(_lead, 1), (_notlead, "\n"), (_form,)]))
class protoloop(known):
        "This was implemented exclusively for the sake of TAGBODY."
        ## Critical Issue PROTOLOOP-MULTIPLE-VALUES-NOT-IMPLEMENTED
        def nvalues(*_):            return not_implemented()
        def nth_value(n, *_):       return not_implemented()
        def primitivise(mach, *body):
                return p.loop(p.progn(*(primitivise(mach, x) for x in body)))
        def effects(*body):         return any(ir_effects(x)  for x in body)
        def affected(*body):        return any(ir_affected(x) for x in body)

# THE

@defknown((_the, " ", (_form,), " ", (_form,)))
class the(known):
        def nvalues(type, form):            not_implemented()
        def nth_value(n, orig, type, form): not_implemented()
        def primitivise(mach, type, form):  not_implemented()
        def effects(type, form):            return ir_effects(form)
        def affected(type, form):           return ir_affected(form)

# LOCALLY

@defknown((_locally,
           [(_lead, 1), (_notlead, "\n"), (_form,)]))
class locally(known):
        def nvalues(*decls_n_body):            not_implemented()
        def nth_value(n, orig, *decls_n_body): not_implemented()
        def primitivise(mach, *decls_n_body):  not_implemented()
        def effects(*decls_n_body):            not_implemented()
        def affected(*decls_n_body):           not_implemented()

# MULTIPLE-VALUE-PROG1

@defknown((_multiple_value_prog1,
           3, (_form,),
           [(_lead, -2), (_notlead, "\n"), (_form,)]))
class multiple_value_prog1(known):
        def nvalues(first_form, *forms):            return ir_nvalues(first_form)
        def nth_value(n, orig, first_form, *forms):
                return (ir_nth_value(n, first_form) if not any(ir_effects(f) for f in forms) else
                        (lambda sym: list__(_let, list_(list_(sym, ir_nth_value(n, first_form))),
                                            consify_linear(forms + (sym,))))
                        (gensym("MV-PROG1-VALUE-")))
        def primitivise(mach, first_form, *forms):  not_implemented()
        def effects(first_form, *forms):            return ir_effects(first_form) or any(ir_effects(f) for f in forms)
        def affected(first_form, *forms):           return ir_affected(first_form) or any(ir_affected(f) for f in forms)

# LOAD-TIME-VALUE

@defknown((_load_time_value, " ", (_form,), (_maybe, " ", (_typep, (member_t, t, nil)))))
class load_time_value(known):
        def nvalues(form, read_only_p):            not_implemented()
        def nth_value(n, orig, form, read_only_p): not_implemented()
        def primitivise(mach, form, read_only_p):  not_implemented()
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
        dprintf("; testing basis known operations:")   ## Quirky stuff reference mode ON!  Go go Arachna!
        assert runtest(lexenvful_identity_rewrite,
                       l(_let, l(l(_car, _cdr))),
                       l(_let, l(l(_car, _cdr))))
        assert runtest(applyification, 
                       l(_let, l(l(_car, _cdr)),
                         l(_function, l(_lambda, l(_car, _cdr),
                                        l(_cdr, _car)))),
                       l(_let, l(l(_car, _cdr)),
                         l(_function, l(_lambda, l(_car, _cdr),
                                        l(_funcall, l(_function, _cdr), _car)))))

if getenv("CL_RUN_TESTS") == "t" and getenv("CL_TEST_KNOWN") == "t":
        with matcher_pp_stack():
                run_tests_known()

def run_tests_pp():
        def pptest(name, form, expected, **keys):
                ret = runtest((name, pp_sex), form, expected, tabstop = 55, **keys)
                # dprintf("0 2 4 6 8 1012141618202224262830323436384042")
                return ret
        l = list_
        (__cons,
         _identity,
         _symbol_value,
         _values,) = [ intern(x)[0]
                       for x in ["CONS", "IDENTITY", "SYMBOL-VALUE", "VALUES"] ]
        __cdr = make_keyword("CDR")
        _cadr, __cadr = intern("CADR")[0], make_keyword("CADR")
        _cddr, __cddr = intern("CDDR")[0], make_keyword("CDDR")
        dprintf("; testing the pretty-printer:")   ## Quirky stuff reference mode ON!  Go go Arachna!
        assert pptest("INTEGER",               42,                "42")
        assert pptest("FLOAT",                 2.718281828,       "2.718281828")
        assert pptest("STRING",               "xyzzy",            '"xyzzy"')
        assert pptest("NIL",                   l(),               "NIL")
        assert pptest("LIST",                  l(42, "xyzzy"),    '(42 "xyzzy")')
        assert pptest("CONS",                  cons(42, "xyzzy"), '(42 . "xyzzy")')
        assert pptest("IF",
                      l(_if, 0, 1, 2),
"""(IF 0
    1
    2)""")
        assert pptest("IF-NESTED",
                      l(_if, l(_if, 0, 1, 2), l(_if, 0, 1, 2), l(_if, 0, 1, 2)),
"""(IF (IF 0
        1
        2)
    (IF 0
        1
        2)
    (IF 0
        1
        2))""")
        assert pptest("IR-ARGS",
                      l(_ir_args, 42,
                        ["foo", 3.14],
                        l("bar", 2.71)),
"""(IR-ARGS
 42
 ("foo" . 3.14)
 ("bar" . (2.71)))""")
        assert pptest("FUNCALL-NO-ARGS",       l(_funcall, _car),       "(FUNCALL CAR)")
        assert pptest("FUNCALL",               l(_funcall, _car, _car), "(FUNCALL CAR CAR)")
        assert pptest("FUNCALL-IF",
                      l(_funcall, 2.71, l(_if, 0, 1, 2), 42, l(_if, 3, 4, 5), 3.14),
"""(FUNCALL 2.71 (IF 0
                  1
                  2) 42 (IF 3
                            4
                            5) 3.14)""")
        assert pptest("LET*-EMPTY",            l(_let_, nil),           "(LET* NIL)")
        assert pptest("LET*-NO-BINDINGS",
                      l(_let_, l(),
                        42),
"""(LET* NIL
  42)""")
        assert pptest("LET*",
                      l(_let_, l(l(_car, 2.71),
                                 l(_car, 3.14)),
                        42,
                        43),
"""(LET* ((CAR 2.71)
       (CAR 3.14))
  42
  43)""")
        assert pptest("LET*-NESTED",
                      l(_let_, l(l(_car, l(_let_, nil,
                                           42)))),
"""(LET* ((CAR (LET* NIL
              42))))""")
        assert pptest("FLET-NO-BODY",          l(_flet, l(l(_car, nil))), "(FLET ((CAR NIL)))")
        assert pptest("FLET-EMPTY",            l(_flet, nil),             "(FLET NIL)")
        assert pptest("FLET",
                      l(_flet, l(l(_car, l(),
                                   2.71,
                                   2.71),
                                 l(_car, l(_car, _key, _cdr, l(_cddr, l(_if, t,
                                                                             nil,
                                                                             2.71))),
                                   3.14),
                                 l(_car, l(),
                                   2.71)),
                        42,
                        42),
"""(FLET ((CAR NIL
         2.71
         2.71)
       (CAR (CAR &KEY CDR (CDDR (IF T
                                    NIL
                                    2.71)))
         3.14)
       (CAR NIL
         2.71))
  42
  42)""")
        assert pptest("BLOCK",
                      l(_block, 42,
                        nil,
                        nil),
"""(BLOCK 42
  NIL
  NIL)""")
        assert pptest("RETURN-FROM",       l(_return_from, 1, 2), "(RETURN-FROM 1 2)")
        assert pptest("RETURN-FROM-SHORT", l(_return_from, 1),    "(RETURN-FROM 1)")
        assert pptest("EVAL-WHEN",
                      l(_eval_when, l(_compile_toplevel, _load_toplevel, _execute),
                        nil,
                        nil),
"""(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  NIL
  NIL)""")
        assert pptest("SETQ-EMPTY",        l(_setq),  "(SETQ)")
        assert pptest("SETQ",              l(_setq, 0, l(_if, 1, 2, 3), 4, 5, 3.14, 2.71),
"""(SETQ 0 (IF 1
            2
            3)
      4 5
      3.14 2.71)""")
        assert pptest("UNWIND-PROTECT",    l(_unwind_protect, 42, 2.71, 3.14),
"""(UNWIND-PROTECT
    42
  2.71
  3.14)""")
        assert pptest("QUOTE",             l(_quote, 42), "'42",
                      known_failure = t)
        assert pptest("PROGV",
                      l(_progv, l(42, 2.71), l(3.14, 7),
                        nil,
                        nil),
"""(PROGV (42 2.71) (3.14 7)
  NIL
  NIL)""")
        # with traced_matcher(emt = True, immediate = True):

if getenv("CL_RUN_TESTS") == "t" and getenv("CL_TEST_PP") == "t":
        with matcher_pp_stack():
                run_tests_pp()

# Core: %PRIMITIVISE, %EMIT-AST, %LOWER and COMPILE

def report(x, kind, desc = "", form_id = None, lexenv = None):
        lexenv  = "%s\n"  % coerce_to_lexenv(lexenv) if  lexenv is not None else ""
        desc    = "%s - " % desc                     if    desc is not None else ""
        form_id = "  %x"  % form_id                  if form_id is not None else ""
        if   kind == "macroexpanded":
                dprintf(";;; %smacroexpanded ............%s\n%s%s\n",
                              desc, form_id, lexenv, pp(x))
        elif kind == "known":
                dprintf(";;; %sknowns ..............%s\n%s%s\n",
                              desc, form_id, lexenv, pp(x))
        elif kind == "primitive":
                dprintf(";;; %sprimitives ==========%s\n%s%s\n",
                              desc, form_id, lexenv, x)
        elif kind == "ast":
                import more_ast
                dprintf(";;; %spython ------------->%s\n%s\n",
                              desc, form_id, "\n".join(more_ast.pp_ast_as_code(x, line_numbers = t)
                                                       for x in x))
        elif kind == "bytecode":
                dprintf(";;; %sbytecode ************%s\n", desc, form_id)
                import dis
                def rec(x):
                        dis.dis(x)
                        for sub in x.co_consts:
                                if isinstance(sub, types.CodeType):
                                        dprintf(";;; child code -------------\n")
                                        rec(sub)
                rec(x)

# Unregistered Issue COMPILER-MACRO-SYSTEM
def primitivise(mach, form, lexenv = nil) -> p.prim:
        # - tail position tracking
        # - scopes
        # - symbols not terribly clear
        # - proper quote processing
        def compiler_maybe_note_subprimitivisation(x, res):
                if (symbol_value(_compiler_trace_subprimitivisation_)
                    and not isinstance(x, (symbol_t, bool, int, str))
                    and not (consp(x) and x[0] in [_ref, _function, _quote])):
                        report(x, "known", desc = "PRIMITIVISE", lexenv = coerce_to_lexenv(symbol_value(_lexenv_)))
                        dprintf("%s", res)
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
                if x is nil or isinstance(x, symbol_t) and not constantp(x):
                        ## Unregistered Issue SYMBOL-MODEL
                        return rec(list_(_ref, x))
                elif listp(x):
                        if isinstance(x[0], symbol_t):
                                argsp, form, args = destructure_possible_ir_args(x)
                                # Urgent Issue COMPILER-MACRO-SYSTEM
                                known = find_known(form[0])
                                if not known:
                                        error("Invariant failed: no non-known IR node expected at this point.  Saw %s in %s.",
                                              form[0], pp_consly(x))
                                compiler_maybe_note_inner(known.name, form)
                                if not hasattr(known, "primitivise"):
                                        error("It is not KNOWN to me, how to primitivise %s forms.", form[0])
                                res = known.primitivise(mach, *vectorise_linear(form[1]), **alist_hash_table(args))
                                compiler_maybe_note_subprimitivisation(x, res)
                                return res
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

def lower(form: cons_t, lexenv = nil, machine = None) -> "machine-specific":
        "Must be called within %WITH-SYMBOL-UNIT-MAGIC context."
        machine = defaulted_to_var(machine, _machine_)
        with target_machine(machine):
                ## Wide Knowns -> Narrow Knowns
                # with traced_matcher(emt = t, immediate = t):
                rewritten = rewrite_all(form, lexenv = lexenv)  ## The high-level entry to %REWRITE-ALL
                if symbol_value(_compiler_trace_rewritten_):
                        report(rewritten, "known", form_id = id(form), desc = "%LOWER", lexenv = lexenv)
                ## Narrow Knowns -> Primitives
                prim = primitivise(machine, rewritten, lexenv = lexenv)  ## The high-level entry to %PRIMITIVISE
                if symbol_value(_compiler_trace_primitives_):
                        report(prim, "primitive", form_id = id(form), desc = "%LOWER", lexenv = lexenv)
                return machine.lower(prim)

# High-level drivers: %PROCESS-TOP-LEVEL, COMPILE-FILE, @LISP, COMPILE, EVAL

def map_top_level(fn, form, eval_when_computer, compile_time_too = nil, process = t, eval = nil):
        def make_skipping_iterating_processor(skip_subforms, doc_and_decls):
                ## Unregistered Issue TOPLEVEL-PROCESSOR-IGNORES-DECLARATIONS
                def skipping_iterating_processor(subforms, compile_time_too, process, eval):
                        "Ignore some SUBFORMS, and dispatch the rest through REC, as top-levels."
                        relevant = nthcdr(skip_subforms, subforms)
                        body = values_frame_project(0, parse_body(relevant))
                        mapc(lambda f: rec(f, compile_time_too, process, eval),
                             body)
                return skipping_iterating_processor
        def process_eval_when(body, compile_time_too, process, eval):
                situations = body[1][0]
                parsed_situations = parse_eval_when_situations(situations)
                new_ctt, new_process, new_eval = eval_when_computer(compile_time_too, process, eval, *parsed_situations)
                # dprintf("\nPROCESS-EVAL-WHEN: %s %s %s / %s:%s -> %s %s %s\n%s",
                #         compile_time_too, process, eval, situations, parsed_situations,
                #         new_ctt, new_process, new_eval,
                #         pp_sex(body))
                mapc(lambda f: rec(f, new_ctt, process and new_process, new_eval),
                     body[1][1])
        actions = {
                _progn:           make_skipping_iterating_processor(skip_subforms = 1, doc_and_decls = nil),
                _locally:         make_skipping_iterating_processor(skip_subforms = 1, doc_and_decls = t),
                _macrolet:        make_skipping_iterating_processor(skip_subforms = 2, doc_and_decls = t),
                _symbol_macrolet: make_skipping_iterating_processor(skip_subforms = 2, doc_and_decls = t),
                _eval_when:       process_eval_when,
                }
        def rec(form, compile_time_too, process, eval):
                # dprintf("REC: %s", pp_consly(form))
                ## Unregistered Issue TOPLEVEL-PROCESSOR-WACKY-LEXENV-HANDLING
                (actions.get(form[0], fn) if consp(form) and atom(form[0]) else
                 fn) (form, compile_time_too, process, eval)
        rec(form, compile_time_too, process, eval)

def process_top_level(form, machine = None) -> [ast.stmt]:
        "A, hopefully, faithful implementation of CLHS 3.2.3.1."
        machine = defaulted_to_var(machine, _machine_)
        if symbol_value(_compile_verbose_):
                def mini_pp(kind, id, named_p, long_p):
                        return "(%s%s%s%s)" % (kind, " " if named_p else "", id, " ..." if long_p else "")
                def named_p(x):
                        return (listp(x) and length(x) >= 2
                                and (atom(x[1][0]) or x[1][0][0] is _setq))
                nested_eval_when_p = consp(form) and form[0] is _eval_when and consp(form[1]) and consp(form[1][1]) and consp(form[1][1][0]) and consp(form[1][1][0][1])
                kind, maybe_name = (("%s %s " % (form[0], pp_consly(form[1][0])),
                                     mini_pp(form[1][1][0][0], form[1][1][0][1][0] if named_p(form[1][1][0]) else "", named_p(form[1][1][0]), length(form[1][1][0]) > 2))
                                                                   if nested_eval_when_p   else
                                    (form[0], form[1][0])          if named_p(form) else
                                    (form[0], "")                  if consp(form)   else
                                    (form, ""))
                dprintf("; compiling %s", mini_pp(kind, maybe_name, named_p(form), length(form) > (3 if nested_eval_when_p and named_p(form[1][1][0]) else
                                                                                                   2 if named_p(form)                                 else
                                                                                                   1)))
        if symbol_value(_compiler_trace_forms_):
                dprintf(";;;%s compiling:\n%s%s",
                        sex_space(-3, ";"), sex_space(), pp(form))
        ## Macro / compiler macro expansion, (the latter one unless disabled by a NOTINLINE declaration), SAME MODE
        macroexpanded = compiler_macroexpand_all(form, lexenv = _null, desc = "PROCESS-TOP-LEVEL")
        ## Note, that at this point, the lexenv is discharged completely.
        ## ..is it?  Macroexpansion is done, so what could it be?
        ##
        ## Accumulation of results arranged for the run time:
        run_time_results = []
        def file_compiler_processor(form, compile_time_too, process, eval, toplevel = None):
                ## Unregistered Issue TOPLEVEL-PROCESSOR-WACKY-LEXENV-ARGUMENT-HANDLING
                ## This is where LEXENV isn't true, as it's always empty.
                ## ..but re-walking it.. who would care?  CLtL2 environments?
                ## Additional note: this is %PROCESS, split in half, due to cases.
                if process or eval:
                        ## Note, how lie wrt. the NULL lexenv -- what about {SYMBOL-,}MACROLET?  See above..
                        code, *unit_data = with_compilation_unit(lambda: (lower(form, lexenv = _null, machine = machine),
                                                                          ) + compilation_unit_symbols(),
                                                                 override = t, id = "PROCESS-TOPLEVEL-")
                if process:
                        if toplevel and symbol_value(_compiler_trace_toplevels_):
                                report(form, "known", desc = "processed TLF")
                                report(code, "ast", desc = "processed TLF")
                        run_time_results.extend(code)
                        compilation_unit_adjoin_symbols(*unit_data)
                if eval:
                        if toplevel and symbol_value(_compiler_trace_compile_time_eval_):
                                report(form, "known", desc = "CT eval")
                                report(code, "ast", desc = "CT eval")
                        bytecode = machine.assemble(machine.compilation_unit_prologue(*unit_data)
                                                    + code,
                                                    form)
                        # dprintf(";; ..compile-time code object execution")
                        machine.execute_bytecode(bytecode)
        map_top_level(file_compiler_processor, macroexpanded, further_eval_when_file_compiler,
                      compile_time_too = nil,
                      process          = t,
                      eval             = nil)
        return run_time_results

string_set("*COMPILE-PRINT*",         t)
string_set("*COMPILE-VERBOSE*",       t)

string_set("*COMPILE-FILE-PATHNAME*", nil)
string_set("*COMPILE-FILE-TRUENAME*", nil)

string_set("*COMPILE-OBJECT*",          nil)
string_set("*COMPILE-TOPLEVEL-OBJECT*", nil)

def compile_file(input_file, output_file = nil, trace_file = nil, verbose = None, print = None, machine = None):
        verbose = defaulted_to_var(verbose, _compile_verbose_)
        print   = defaulted_to_var(verbose, _compile_print_)
        if verbose:
                format(t, "; compiling file \"%s\" (written %s):\n", input_file, file_write_date(input_file))
        ## input/output file conformance is bad here..
        abort_p, warnings_p, failure_p = nil, nil, nil
        machine = defaulted_to_var(machine, _machine_)
        forms = list_(_progn)
        with pyb.open(input_file, "r") as input:
                def in_compilation_unit():
                        nonlocal trace_file, forms
                        if trace_file:
                                trace_filename = (trace_file if stringp(trace_file) else
                                                  input_file.replace(".lisp", "." + symbol_value(_trace_file_type_)))
                                trace_file = pyb.open(trace_filename, "w")
                        try:
                                code = []
                                form = read(input, eof_value = input, eof_error_p = nil)
                                while form is not input:
                                        forms = cons(form, forms)
                                        ## Beacon LEXENV-CLAMBDA-IS-NIL-HERE
                                        form_code = process_top_level(form, machine = machine)
                                        code.extend(form_code)
                                        if trace_file:
                                                trace_file.write(pp(form))
                                                for stmt in form_code:
                                                        trace_file.write(str(stmt))
                                                        trace_file.write("\n")
                                        form = read(input, eof_value = input, eof_error_p = nil)
                                return (machine.compilation_unit_prologue(*compilation_unit_symbols()) +
                                        code)
                        finally:
                                if trace_file:
                                        trace_file.close()
                code = with_compilation_unit(in_compilation_unit,
                                             ## Unregistered Issue POSSIBLE-COMPILATION-UNIT-USE-VIOLATION-HERE
                                             override = t, id = "COMPILE-FILE-")
        output_file = output_file or input_file.replace(".lisp", "." + symbol_value(_fasl_file_type_))
        try:
                with pyb.open(output_file, "wb") as f:
                        f.write(symbol_value(_fasl_file_magic_))
                        bytecode = machine.assemble(code, nreverse(forms))
                        marshal.dump(bytecode, f)
                        return output_file
        finally:
                verbose and format(t, "; %s written\n", output_file)

def compile_in_lexenv(lambda_expression, lexenv = nil, name = None, globalp = None, global_macro_p = None, macroexpand = t,
                      machine = None):
        if name is None:
                error("In COMPILE-IN-LEXENV: NAME must be provided.")
        check_type(lexenv, (or_t, null_t, lexenv_t))
        machine = defaulted_to_var(machine, _machine_)
        if lexenv and globalp:
                error("In %%COMPILE-IN-LEXENV: the provided non-NULL lexenv conflicts with GLOBALP.")
        form = ir(*[_function, lambda_expression],
                   name = name,
                   globalp = t,  ## This (LOWER-LAMBDA's) globalp is different from COMPILE-IN-LEXENV's globalp.
                   **({ "pydecorators": list_(ir_cl_call("set_macro_definition",
                                                          ir_apply("globals"), name, lambda_expression)
                                              if global_macro_p else
                                              ir_cl_call("set_function_definition",
                                                          ir_apply("globals"), name, lambda_expression))}
                      if globalp else {}))
        if symbol_value(_compiler_trace_forms_):
                dprintf(";;;%s compiling:\n%s%s",
                              sex_space(-3, ";"), sex_space(), pp(form))
        def process_to_code(form, lexenv = None, macroexpand = None):
                return lower(compiler_macroexpand_all(form, lexenv = lexenv, desc = "PROCESS-TO-CODE") if macroexpand else
                             form,
                             lexenv = lexenv,
                             machine = machine)
        code, *unit_data = with_compilation_unit(lambda: (process_to_code(form, lexenv = lexenv, macroexpand = macroexpand),
                                                          ) + compilation_unit_symbols(),
                                                 override = t, id = "COMPILED-LAMBDA-")
        bytecode = machine.assemble(machine.compilation_unit_prologue(*unit_data)
                                    + code, form)
        function = machine.execute_bytecode(bytecode, object_name = name, filename = "<lisp core>")
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

def compile(name, definition = None, macroexpand = t):
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
                                 global_macro_p = name and not not macro_function(name),
                                 macroexpand    = macroexpand)

def eval(form):
        if symbol_value(_compiler_trace_forms_):
                dprintf(";;;%s evaluating:\n%s%s",
                              sex_space(-3, ";"), sex_space(), pp(form))
        ## We need to fully (?) macroexpand, before we can walk the top level form.
        # with traced_matcher(emt = t, immediate = t):
        macroexpanded = compiler_macroexpand_all(form, lexenv = _null, desc = "EVAL")
        result = nil
        def evaluator_processor(form, compile_time_too, process, eval, toplevel = None):
                nonlocal result
                ## Unregistered Issue TOPLEVEL-PROCESSOR-WACKY-LEXENV-ARGUMENT-HANDLING
                ## This is where LEXENV isn't true, as it's always empty.
                ## ..but re-walking it.. who would care?  CLtL2 environments?
                ## Additional note: this is %PROCESS, split in half, due to cases.
                if eval:
                        result = compile(nil, list_(_lambda, nil, form), macroexpand = nil)()
        map_top_level(evaluator_processor, macroexpanded, further_eval_when_eval,
                      compile_time_too = nil,
                      process          = nil,
                      eval             = t)
        return result

def do_exit(status = 0):
        frost.disable_pytracer()
        exit(status)

def repl(prompt = "* "):
        @__block__
        def repl_iteration():
                write_string(prompt)
                finish_output()
                expr = handler_case(read,
                                    (EOFError, lambda _: do_exit()))
                result = eval(expr)
                write_line(pp_sex(result))
                finish_output()
        def repl_error_handler(cond):
                terpri()
                finish_output()
                return_from(repl_iteration)
        while t:
                with progv({ _last_chance_handler_: repl_error_handler }):
                        repl_iteration()

def configure_recursion_limit(new_limit):
        # dprintf("; current recursion limit is: %s;  setting it to %s",
        #               sys.getrecursionlimit(), new_limit)
        sys.setrecursionlimit(new_limit)

configure_recursion_limit(262144)

def run_tests_compiler():
        def do_dbgsetup():
                dbgsetup(forms = t,
                        
                         # subexpansion = t,
                         macroexpanded = t,
                         
                         # subrewriting = t,
                         rewritten = t,
                         
                         # subprimitivisation = t,
                         primitives = t,
                         
                         # subastification = t,
                         # ast = t,
                         module_ast = t,
                         
                         # bytecodes = t
                         )
        def evaltest(name, form, expected, **keys):
                def do_runtest():
                        try:
                                return runtest((name, eval), form, expected, printer = pp_consly, tabstop = 55, **keys)
                        except:
                                pass
                ret = do_runtest()
                if not ret:
                        do_dbgsetup()
                        do_runtest()
                assert ret
        l = list_
        (__cons,
         _identity,
         _symbol_value,
         _values,) = [ intern(x)[0]
                       for x in ["CONS", "IDENTITY", "SYMBOL-VALUE", "VALUES"]]
        __cdr = make_keyword("CDR")
        _cadr, __cadr = intern("CADR")[0], make_keyword("CADR")
        _cddr, __cddr = intern("CDDR")[0], make_keyword("CDDR")
        dprintf("; testing compiler evaluation powers:")   ## Quirky stuff reference mode ON!  Go go the spider Queen!
        # dbgsetup( # forms = t,
        #           # macroexpanded = t,
        #           # subrewriting = t,
        #           # rewritten = t,
        #           # primitives = t,
        #           # module_ast = t,
        #           )
        # with traced_matcher(emt = t, immediate = t):
        evaltest("PRIMITIVE-QUOTATION/ATOM",
                 l(_primitive, p.integer, l(_quote, 42)),
                 42)
        evaltest("PRIMITIVE-OP",
                 l(_primitive, p.not_, 0),
                 True)
        evaltest("PRIMITIVE-QUOTED-OP",
                 l(_primitive, l(_quote, l("not_")), 0),
                 True)
        evaltest("PRIMITIVE-LAMBDA/FUNCALL",
                 l(_primitive, p.funcall,
                   l(_primitive, p.function, None, l(_quote, (None, p.name("x"))),
                     l(_primitive, p.add, p.name("x"), 2)),
                   2),
                 4)
        evaltest("CONST-NIL",                nil,                       nil)
        evaltest("CONST-T",                  t,                         t)
        evaltest("CONST-42",                 42,                        42)
        evaltest("CONST-FOO",                "foo",                    "foo")
        evaltest("QUOTE-NONCONSTANT-SYMBOL", l(_quote, _car),           _car)
        evaltest("QUOTE-CONSTANT",           l(_quote, 42),             42)
        evaltest("QUOTE-SEX",                l(_quote, l(_car, 42)),    l(_car, 42))
        evaltest("APPLY-SIMPLE",             l(_list, 3.14, "a", 42),   l(3.14, "a", 42))
        #### TODO: QUOTE-UNPRIMITIVISABLE-ERROR-CASE
        ## SETQ/REF/PROGN
        evaltest("EMPTY-PROGN",                   l(_progn),                                nil)
        evaltest("SIMPLE-PROGN",                  l(_progn, 1, 0),                          0)
        evaltest("SETQ-SIMPLE/REF",               l(_progn,
                                                    l(_defvar, _car, nil),
                                                    l(_setq, _car, 42),
                                                    _car),                                  42)
        evaltest("SPILL-SETQ",                    l(_progn,
                                                    l(_defvar, _car, nil),
                                                    l(_setq, _car, l(_setq, _car, 42)),
                                                    _car),                                  42)
        evaltest("SETQ-COMPLEX/REF/APPLY-SIMPLE", l(_progn,
                                                    l(_defvar, _car, nil),
                                                    l(_defvar, _cdr, nil),
                                                    l(_setq, _car, 42,
                                                             _cdr, 3.14),
                                                    l(_list, _car, _cdr)),                  l(42, 3.14))
        evaltest("SETQ/REF-IMPLREF",              l(_progn,
                                                    l(_setq, l(_quote, l("foo")), "bar"),
                                                    l(_ref, l(_quote, l("foo")))),         "bar",
                                                  known_failure = t, catch_errors = t)
                                                  ## The above is broken by the new (correct) EVAL model,
                                                  ## and the issue is in Lisp/Pyworld impedance mismatch.
        evaltest("LET/SETQ/REF-LEXICAL",          l(_progn,
                                                    l(_defvar, _car, nil),
                                                    l(_setq, _car, "bar"),
                                                    l(_let, l(_car,
                                                              l(_cdr, 42),
                                                              l(_let, 42)),
                                                      l(_setq, _let, 21264),
                                                      l(_list, _car, _cdr, _let))),         l(nil, 42, 21264))
        ## IF
        evaltest("IF-TRUE",
                l(_if, t, t, nil),   t)
        evaltest("IF-FALSE",
                l(_if, nil, nil, t), t)
        evaltest("IF-FALSE-SINGLE-BRANCH",
                l(_if, nil, t),      nil)
        ## LET
        evaltest("LET-EMPTY",
                 l(_let, nil),
                 nil)
        evaltest("LET-NO-BINDINGS",
                 l(_let, nil, 0),
                 0)
        evaltest("LET-NO-BODY",
                 l(_progn,
                   l(_defvar, _cdr, nil),
                   l(_list,
                     l(_let, l(l(_car, l(_setq, _cdr, 42)))),
                     _cdr)),
                 l(nil, 42))
        ## PRIM, IR-ARGS
        evaltest("PRIM-DEF-CALL",
                 l(_progn,
                   l(_primitive, p.function, p.name("foo"), l(_quote, (None,)),
                     p.integer(42)),
                   l(_primitive, p.funcall, p.name("foo"))),
                 42,
                 known_failure = t, catch_errors = t) ## Same reason as SETQ/REF-IMPLREF
        ## FUNCTION
        evaltest("FUNCTION/IMPLREF",
                 l(_function, l(_quote, l("cl", "list_"))),
                 list_)
        ## LAMBDA
        evaltest("LAMBDA-SIMPLE-CALL",
                 l(l(_lambda, nil)),
                 nil)
        evaltest("LAMBDA-IDENTITY",
                 l(l(_lambda, l(_car), _car), 3.14),
                 3.14)
        evaltest("LAMBDA-&OPTIONAL-NIL-DEFAULTED",
                 l(l(_lambda, l(_car, _optional, _cdr), l(_list, _car, _cdr)), 3.14),
                 l(3.14, nil))
        evaltest("LAMBDA-&OPTIONAL-DEFAULTED",
                 l(l(_lambda, l(_car, _optional, l(_cdr, 42)), l(_list, _car, _cdr)), 3.14),
                 l(3.14, 42))
        evaltest("LAMBDA-&OPTIONAL-PROVIDED",
                 l(l(_lambda, l(_car, _optional, l(_cdr, 42)), l(_list, _car, _cdr)), 3.14, 2.71),
                 l(3.14, 2.71))
        evaltest("LAMBDA-&KEY-NIL-DEFAULTED",
                 l(l(_lambda, l(_car, _key, _cdr), l(_list, _car, _cdr)), 3.14),
                 l(3.14, nil))
        evaltest("LAMBDA-&KEY-DEFAULTED",
                 l(l(_lambda, l(_car, _key, l(_cdr, 42)), l(_list, _car, _cdr)), 3.14),
                 l(3.14, 42))
        evaltest("LAMBDA-&KEY-PROVIDED",
                 l(l(_lambda, l(_car, _key, l(_cdr, 42)), l(_list, _car, _cdr)), 3.14, __cdr, 2.71),
                 l(3.14, 2.71))
        evaltest("LAMBDA-&KEY-&ALLOW-OTHER-KEYS-LAMBDA-ALLOWS",
                 l(l(_lambda, l(_car, _key, _allow_other_keys), t), 3.14, __cdr, 2.71),
                 t)
        evaltest("LAMBDA-&KEY-:ALLOW-OTHER-KEYS-CALL-ALLOWS",
                 l(l(_lambda, l(_key), t), __cdr, 2.71, _allow_other_keys_, t),
                 t)
        evaltest("LAMBDA-&OPTIONAL-&KEY-PROVIDED",
                 l(l(_lambda, l(_car, _optional, _cdr, _key, l(_cadr, 42)), l(_list, _car, _cdr, _cadr)),
                   3.14, 42, __cadr, 2.71),
                 l(3.14, 42, 2.71))
        evaltest("LAMBDA-&REST-NIL",
                 l(l(_lambda, l(_car, _rest, _cdr), l(__cons, _car, _cdr)), 2.71),
                 l(2.71))
        evaltest("LAMBDA-&REST-SOMETHING",
                 l(l(_lambda, l(_rest, _car), _car), 2.71, 3.14, 42),
                 l(2.71, 3.14, 42))
        evaltest("LAMBDA-&REST-&KEY-PROVIDED",
                 l(l(_lambda, l(_car, _rest, _cdr, _key, l(_cadr, 42), l(_cddr, 123)),
                     l(_list, _car, _cdr, _cadr, _cddr)),
                   3.14, __cddr, 2.71),
                 l(3.14, l(__cddr, 2.71), 42, 2.71))
        ## APPLY
        evaltest("APPLY-PFUNCALL",
                 l(_apply, l(_function, _identity), 1, l(_quote, nil)),
                 1)
        evaltest("APPLY-PAPPLY",
                 l(_apply, l(_function, _identity), l(_list, 1)),
                 1)
        ## MULTIPLE-VALUE-CALL
        evaltest("MULTIPLE-VALUE-CALL-SIMPLE",
                 l(_multiple_value_call, l(_function, _list), l(_values, 1, 2, 3)),
                 l(1, 2, 3))
        evaltest("MULTIPLE-VALUE-CALL-COMPLEX",
                 l(_multiple_value_call, l(_function, _list), l(_values, 1, 2), 3, l(_values, 4, 5)),
                 l(1, 2, 3, 4, 5),
                 known_failure = t, catch_errors = t)
        ## FLET
        name = gensym("FNAME")
        evaltest("FLET-SIMPLE",
                 l(_flet, l(l(name, l(_car),
                              l(__cons, 42, _car))),
                   l(name, l(_list, 2.17))),
                 l(42, 2.17))
        name = gensym("FNAME")
        evaltest("FLET-COMPLEX",
                 l(_flet, l(l(name, l(_car, _optional, _cons, _rest, _cdr, _key, l(_cadr, 42), l(_cddr, 123)),
                              l(_list, _car, _cons, _cdr, _cadr, _cddr))),
                   l(name, 3.14, 2.71, __cadr, 2.71)),
                 l(3.14, 2.71, l(__cadr, 2.71), 2.71, 123))
        name = gensym("FNAME")
        evaltest("FLET-SHADOW/SCOPE",
                 l(_flet, l(l(name, l(),
                              42)),
                   l(_list,
                     l(name),
                     l(_flet, l(l(name, l(),
                                  l(_list, 3.14, l(name)))),
                       l(name)),
                     l(name))),
                 l(42, l(3.14, 42), 42))
        ## CATCH/THROW
        evaltest("CATCH/THROW-TRIVIAL",
                 l(_catch, l(_quote, _car),
                   l(_throw, l(_quote, _car), 42),
                   3.14),
                 42)
        name = gensym("FNAME")
        evaltest("CATCH/THROW-FUNCALL",
                 l(_flet, l(l(name, nil,
                              l(_throw, l(_quote, _car), 42))),
                   l(_catch, l(_quote, _car),
                     l(name),
                     3.14)),
                 42)
        ## UNWIND-PROTECT
        evaltest("UNWIND-PROTECT",
                 l(_let, l(l(_car, nil)),
                   l(_list,
                     l(_catch, l(_quote, _cdr),
                       l(_unwind_protect,
                         l(_throw, l(_quote, _cdr), 42),
                         l(_setq, _car, 2.718281828))),
                     _car)),
                 l(42, 2.718281828))
        ## NTH-VALUE
        evaltest("NTH-VALUE-SIMPLE",
                 l(_nth_value, 1, l(_values, 3.14, 42, 2.71)),
                 42)
        name = gensym("FNAME")
        evaltest("NTH-VALUE-FUNCALL",
                 l(_flet, l(l(name, nil,
                              l(_values, 3.14, 42, 2.71))),
                   l(_nth_value, 1, l(name))),
                 42)
        name = gensym("FNAME")
        evaltest("VALUES-MANY-AS-ONE",
                 l(_flet, l(l(name, nil,
                              l(_values, 3.14, 42, 2.71))),
                   l(_identity, l(name))),
                 3.14,
                 known_failure = t)
        ## PROGV
        name = gensym("FNAME")
        evaltest("PROGV",
                 l(_flet, l(l(name, nil,
                              l(_symbol_value, l(_quote, _car)))),
                   l(_progv, l(l(_car, l(_list, l(_quote, _car)))), l(42),
                     l(name))),
                 42)
        ## PROTOLOOP
        acc, counter = [ gensym(x) for x in ["ACC", "COUNTER"] ]
        evaltest("PROTOLOOP/CATCH-THROW",
                 l(_let, l(l(acc, 0),
                           l(counter, 42)),
                   l(_catch, l(_quote, _car),
                     l(_protoloop,
                       l(_if, l(_primitive, p.eq, counter, 0),
                         l(_throw, l(_quote, _car), acc)),
                       l(_setq, acc,     l(_add, acc, 2),
                                counter, l(_sub, counter, 1))))),
                 84)
        ## THE
        ## LOCALLY
        ## MULTIPLE-VALUE-PROG1
        ## LOAD-TIME-VALUE
        ####
        ## FUNCALL
        name = gensym("FNAME")
        evaltest("FUNCALL-PRIM",
                 l(_flet, l(l(name, nil,
                              l(_primitive, p.function, p.name("bar"), l(_quote, (None,)),
                                 p.integer(42)),
                              l(_funcall, l(_function, l(_quote, l("bar")))))),
                   l(name)),
                 42)
        name = gensym("FNAME")
        evaltest("FUNCALL/FLET/FUNCTION",
                 ## Is this pointless by now?
                 l(_flet, l(l(name, l(_car, _key, l(_cdr, 42)),
                              l(_list, _car, _cdr))),
                   l(_funcall, l(_function, name), 3.14, __cdr, 2.71)),
                 l(3.14, 2.71))
        ## LET*
        evaltest("LET*",
                 l(_let, l(l(_car, nil)),
                   l(_list,
                     l(_let_, l(l(_car, l(__cons, nil, _car)),
                                l(_car, l(__cons, nil, _car))),
                       _car),
                     _car)),
                 l(l(nil, nil),
                   nil))
        evaltest("LET*-NO-BINDINGS",
                 l(_let, l(),
                   42),
                 42)
        ## LABELS
        name = gensym("FNAME")
        evaltest("LABELS-SIMPLE",
                 l(_labels, l(l(name, l(_car),
                                l(__cons, 42, _car))),
                   l(name, l(_list, 2.17))),
                 l(42, 2.17))
        name, acc, n, cur = [ gensym(x) for x in [ "FACT", "ACC", "N", "CUR" ] ]
        evaltest("LABELS-REC",
                 l(_labels, l(l(name, l(n, _optional, l(acc, 1), l(cur, n)),
                                l(_if, l(_equals, cur, 0),
                                  acc,
                                  l(name, n, l(_mult, acc, cur), l(_sub, cur, 1))))),
                   l(name, 5)),
                 120)
        ## MACROLET
        name, x = gensym("MACRO"), intern("X")[0]
        evaltest("MACROLET",
                 l(_macrolet, l(l(name, l(x),
                                  l(_list, l(_quote, _list), 42, x, 43))),
                   l(name, 3.14)),
                 l(42, 3.14, 43))
        ## SYMBOL-MACROLET
        name = gensym("SYMAC")
        evaltest("SYMBOL-MACROLET",
                 l(_symbol_macrolet, l(l(name, l(_progn,
                                                 l(intern("FORMAT")[0], intern("*ERROR-OUTPUT*")[0], " side effect, sumairu!  "),
                                                 l(_list, 42)))),
                   name),
                 l(42))
        ## BLOCK/RETURN-FROM
        inner, outer = gensym("INNER"), gensym("OUTER")
        evaltest("BLOCK/RETURN-FROM",
                 l(_block, outer,
                   l(_block, inner,
                     l(_return_from, outer, 42),
                     3.14),
                   2.71),
                 42)
        ## TAGBODY/GO
        a, b, c = [ intern(x)[0] for x in ["A", "B", "C"] ]
        x = intern("X")[0]
        evaltest("TAGBODY/GO",
                 l(_let, l(l(x, 0)),
                   l(_tagbody,
                     l(_setq, x, l(_add, x, 2)),
                     l(_go, b),
                     a,
                     l(_setq, x, l(_add, x, 10)),
                     l(_go, c),
                     b,
                     l(_setq, x, l(_mult, x, 5)),
                     l(_go, a),
                     c,
                     l(_setq, x, l(_sub, x, 20))),
                   x),
                 0)
        ## EVAL-WHEN
        evaltest("EVAL-EVAL-WHEN-COMPILE-LOAD-IGNORED",
                 l(_eval_when, l(_compile_toplevel, _load_toplevel),
                   42),
                 nil)
        evaltest("EVAL-EVAL-WHEN-EXECUTE-EXECUTED",
                 l(_eval_when, l(_execute),
                   42),
                 42)

if getenv("CL_RUN_TESTS") == "t" and getenv("CL_TEST_COMPILER") == "t":
        with matcher_pp_stack():
                run_tests_compiler()

def self_analyze():
        def codep(x): return isinstance(x, type(self_analyze.__code__))
        def co_deps(x):
                deps = set(x.co_names)
                for sub in x.co_consts:
                        if codep(sub):
                                deps |= co_deps(sub)
                return deps
        def fn_deps(x):
                return co_deps(x.__code__)
        def cls_deps(x):
                deps = set()
                for ix in x.__dict__.values():
                        if functionp(ix):
                                deps |= fn_deps(ix)
                return deps
        fdeps = dict()                        ## CALLER --> { CALLEE }
        for name, x in globals().items():
                if functionp(x):
                        fdeps[x] = fn_deps(x)
                if isinstance(x, type):
                        fdeps[x] = cls_deps(x)
        frdeps = collections.defaultdict(set) ## CALLEE --> { CALLERS }
        for f, deps in fdeps.items():
                for dep in deps:
                        frdeps[dep].add(f)
        dprintf("; Total functions: %d", len(fdeps))
        usebs = collections.defaultdict(set)
        for f, rdeps in sorted(frdeps.items()):
                usebs[len(rdeps)].add(f)
        usebs[0] = set(x.__name__ for x in fdeps
                       if x.__name__ not in frdeps)
        dprintf("; Usage breakdown:")
        for n, fs in reversed(sorted(usebs.items())):
                dprintf("; ---- use count %d:", n)
                for f in sorted(fs): dprintf(";     %s", f)

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
        py.backtrace(15, frame_ids = t, offset = 1)

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
                def load_as_source_process_one_form(form, index):
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
                                        load_as_source_process_one_form(form, nil)
                                        form = next()
                else:
                        with progv({ _source_info_: nil }):
                                while form != stream:
                                        load_as_source_process_one_form(form, nil)
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
                should_be_fasl_p = real and pathname_type(real) == symbol_value(_fasl_file_type_)
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
                           pyb.open(pathspec, "rb") if stringp(pathspec) and probe_file(pathspec) else
                           nil) or
                          (pathname_type(pathspec) is nil and typeless_pathname_branch()) or
                          (if_does_not_exist and
                           error(simple_file_error_t, pathname = pathspec,
                                 format_control = "Couldn't load %s: file does not exist.",
                                 format_arguments = [pathspec]))),
                         with_open_stream_body)
        ## Case 3: Open using the gived external format, process as source.
        with_open_file(pathname_,
                       lambda stream: load_stream(stream, nil),
                       external_format = external_format)

for symname, name, fun in [("_macroexpand",  "MACROEXPAND",  macroexpand),
                           ("_load",         "LOAD",         load),
                           ("_compile_file", "COMPILE-FILE", compile_file)]:
        intern_and_bind((symname, name, fun))
        post_factum_defun(globals()[symname], fun)

@defclass
class stream_type_error_t(simple_condition_t, io.UnsupportedOperation):
        pass

# LOAD-able things

def load_rest_of_lisp():
        global __running_tests__, __enable_matcher_tracing__
        __running_tests__ = True
        # __enable_matcher_tracing__ = True
        with disabled_condition_system():
                import cProfile, pstats
        def compile_vpcl():
                return compile_file("vpcl.lisp")
        def compile_reader():
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

                  # validate_ast = t,
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
                # type_kind = ?
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
                py.here("args: %s", initargs)
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
        generic_function.__applicable_method_cache__ = dict() # (_list, type) -> list
        generic_function.__methods__ = dict()
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

__method_combinations__ = dict()

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
        groups = dict()
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
                           for x in [None,            # NoneType
                                     Ellipsis,        # ellipsis
                                     NotImplemented,  # NotImplementedType
                                     integer_t,       # type
                                     "".find,         # builtin_function_or_method
                                     ast,             # module
                                     sys.stdin,       # _io.TextIOWrapper
                                     car.__code__,    # code object
                                     py.this_frame(), # frame
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
                py.here("gf: %s, ll: %s", generic_function, generic_function.lambda_list)
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
        py.here("args: %s", initargs)
        ###
        ### Second step:
        if not generic_function:
                # If the GENERIC-FUNCTION argument is NIL, an instance of the class
                # specified by the :GENERIC-FUNCTION-CLASS argument is created by
                # calling MAKE-INSTANCE with the previously computed initialization arguments.
                # The function name FUNCTION-NAME is set to name the generic function.
                generic_function = generic_function_class_t(**initargs)
                # standard_generic_function_shared_initialize is called by s-g-f.__init__
                frost.setf_global(generic_function, function_name, globals = defaulted(globals, pyb.globals()))
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
                                       defaulted(globals, pyb.globals())),
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
                and method_qualifiers(method) == qualifiers)

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

# Missing stuff

def valid_declaration_p(x):
        return nil

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
