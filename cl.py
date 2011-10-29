###
### Some surfacial Common Lisp compatibility.
###
import re
import os
import io
import _io
import sys
import time
import types
import inspect
import builtins
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

most_positive_fixnum = 67108864

def string_upcase(x):     return x.upper()
def string_downcase(x):   return x.lower()
def string_capitalize(x): return x.capitalize()

_case_attribute_map = dict(UPCASE     = string_upcase,
                           DOWNCASE   = string_downcase,
                           CAPITALIZE = string_capitalize,
                           PRESERVE   = identity)
def _case_xform(type, s):
        return _case_attribute_map[type.name](s)

###
### Ring 1.
###
def _defaulting(x, variable):
        return x if x is not None else symbol_value(variable)

def _read_case_xformed(x):
        return _case_xform(_symbol_value("_READ_CASE_"), x)

def _coerce_to_symbol_name(x):
        return (x.name if symbolp(x) else
                _read_case_xformed(x))

###
### Basis
###
##
## frames
##
# >>> dir(f)
# ['__class__', '__delattr__', '__doc__', '__eq__', '__format__',
# '__ge__', '__getattribute__', '__gt__', '__hash__', '__init__',
# '__le__', '__lt__', '__ne__', '__new__', '__reduce__',
# '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__', '__str__',
# '__subclasshook__', 'f_back', 'f_builtins', 'f_code', 'f_globals',
# 'f_lasti', 'f_lineno', 'f_locals', 'f_trace']
# >>> dir(f.f_code)
# ['__class__', '__delattr__', '__doc__', '__eq__', '__format__',
# '__ge__', '__getattribute__', '__gt__', '__hash__', '__init__',
# '__le__', '__lt__', '__ne__', '__new__', '__reduce__',
# '__reduce_ex__', '__repr__', '__setattr__', '__sizeof__', '__str__',
# '__subclasshook__', 'co_argcount', 'co_cellvars', 'co_code',
# 'co_consts', 'co_filename', 'co_firstlineno', 'co_flags',
# 'co_freevars', 'co_kwonlyargcount', 'co_lnotab', 'co_name',
# 'co_names', 'co_nlocals', 'co_stacksize', 'co_varnames']
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

def _next_frame(f):
        return f.f_back

def _caller_frame(n = 0):
        return sys._getframe(n + 2)

def _caller_name(n = 0):
        return _fun_name(_frame_fun(sys._getframe(n + 2)))

def _exception_frame():
        return sys.exc_info()[2].tb_frame

def _frames_upward_from(f):
        return [f] + (_frames_upward_from(f.f_back) if f.f_back else [])

def _top_frame():
        return _frames_upward_from(_this_frame())[-1]

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

def _pp_frame(f, align = None, lineno = None):
        fun = _frame_fun(f)
        fun_name, fun_params, filename = _fun_info(fun)[:3]
        padding = " " * ((align or len(filename)) - len(filename))
        return "%s:%s %s(%s)" % (padding + filename,
                                  ("%d:" % _frame_lineno(f)) if lineno else "",
                                  fun_name, ", ".join(fun_params))

def _print_frame(f, stream = None):
        write_string(_pp_frame(f), _defaulting(stream, "_debug_io_"))

def _print_frames(fs, stream = None):
        mapc(lambda i, f: format(_defaulting(stream, "_debug_io_"), "%2d: %s\n" % (i, _pp_frame(f, lineno = True))),
             *zip(*enumerate(fs)))

def _backtrace(x = -1, stream = None):
        _print_frames(_frames_upward_from(_this_frame())[1:x],
                      _defaulting(stream, "_debug_io_"))

def _here(note = None, callers = 4):
        names = []
        for i in reversed(range(callers)):
                names.append(_caller_name(i))
        names = "..".join(mapcar(string_upcase, names))
        return write_line("%s: '%s'%s" % (names, threading.current_thread().name, "" if note is None else (" - %s" % note)),
                          sys.stderr)

# Study was done by the means of:
# print("\n".join(map(lambda f:
#                             "== def %s\n%s\n" %
#                     (fun_name(f),
#                      "\n  ".join(map(lambda s: s + ": " + str(getattr(f, s)),
#                                      ['co_argcount',
#                                       'co_cellvars',
#                                       'co_names',
#                                       'co_varnames',
#                                       'co_freevars',
#                                       'co_nlocals']))),
#                     ffuns)))

# == def xceptor
# co_argcount: 1
#   co_cellvars: ()
#   co_names: ('error', 'Exception', 'this_frame')
#   co_varnames: ('xceptor_arg', 'cond')
#   co_freevars: ()
#   co_nlocals: 2

# == def midder
# co_argcount: 1
#   co_cellvars: ()
#   co_names: ()
#   co_varnames: ('midder_arg', 'midder_stack_var')
#   co_freevars: ('xceptor',)
#   co_nlocals: 2

# == def outer
# co_argcount: 0
#   co_cellvars: ()
#   co_names: ()
#   co_varnames: ('outer_stack_var',)
#   co_freevars: ('midder',)
#   co_nlocals: 1

# == def example_frame
# co_argcount: 0
#   co_cellvars: ('xceptor', 'midder')
#   co_names: ()
#   co_varnames: ('outer',)
#   co_freevars: ()
#   co_nlocals: 1

# == def <module>
# co_argcount: 0
#   co_cellvars: ()
#   co_names: ('example_frame', 'f')
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
class simple_condition(BaseException):
        def __init__(self, format_control, *format_arguments):
                self.format_control, self.format_arguments = format_control, format_arguments
        def __str__(self):
                return self.format_control % tuple(self.format_arguments)
        def __repr__(self):
                return self.__str__()

class warning(BaseException): pass

class simple_warning(simple_condition, warning): pass

class _not_implemented_error(Exception):
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
## Non-CL tools
##
def _letf(*values_and_body):
        values, body = values_and_body[:-1], values_and_body[-1]
        return body(*values)

def _if_let(condition, consequent, antecedent = lambda: None):
        x = condition() if functionp(condition) else condition
        return consequent(x) if x else antecedent()

def _when_let(condition, consequent):
        x = condition() if functionp(condition) else condition
        return consequent(x) if x else None

def _lret(value, body):
        body(value)
        return value

_curry = functools.partial

def _compose(f, g):
        return lambda *args, **keys: f(g(*args, **keys))

def _tuplep(x): return type(x) is tuple
def _dictp(o):  return type(o) is dict

def _ensure_list(x):
        return x if listp(x)   else [x]

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

def _slotting(x):             return lambda y: getattr(y, x, None)

def _map_into_hash(f, xs, key = identity):
        acc = dict()
        for x in xs:
                acc[key(x)] = f(x)
        return acc

def _updated_dict(to, from_):
        to.update(from_)
        return to

##
## Lesser non-CL tools
##
def ___(str, expr):
        write_string("%s: %s" % (str, expr))
        return expr

class _servile():
        def __repr__(self):
                return "#%s(%s)" % (type(self).__name__,
                                    ", ".join(maphash(lambda k, v: "%s = %s" % (k, v),
                                                      self.__dict__)))
        def __init__(self, **keys):
                self.__dict__.update(keys)

##
## Symbols
##
__gensym_counter__ = 0
def gensym(x = "G"):
        "Not a real GENSYM, as it returns merely a string."
        global __gensym_counter__
        __gensym_counter__ += 1
        return sys.intern(x + str(__gensym_counter__))

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

def equal(x, y):
        return x == y

def destructuring_bind(val, body):
        return body(*tuple(val))

def when(test, clause):
        if test() if functionp(test) else test:
                return clause
def cond(*clauses):
        for (test, result) in clauses:
                if test() if functionp(test) else test:
                        return result
def case(val, *clauses):
        for (cval, result) in clauses:
                if val == cval or (cval is True):
                        return result

##
## Types
##
class type_error(Exception):
        pass

class simple_type_error(simple_condition, type_error):
        pass

stream = _io._IOBase

def find_class(x):
        "XXX: how to do this?"
        return globals()[name]

def type_of(x):
        return type(x)

def typep(x, super):
        return isinstance(x, super)

def subtypep(sub, super):
        return issubclass(sub, super)

def the(type, x):
        assert(typep(x, type))
        return x

def typecase(val, *clauses):
        for (ctype, result) in clauses:
                if (ctype is t) or (ctype is True) or typep(val, ctype):
                        return result() if functionp(result) else result

def etypecase(val, *clauses):
        for (ctype, result) in clauses:
                if (ctype is t) or (ctype is True) or typep(val, ctype):
                        return result() if functionp(result) else result
        else:
                error(TypeError, "%s fell through ETYPECASE expression. Wanted one of (%s)." %
                      (val, ", ".join(mapcar(lambda c: c[0].__name__, clauses))))

def check_type(x, type):
        if not typep(x, type):
                error(TypeError, "The value %s is not of type %s." %
                      (x, type.__name__))

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
def functionp(o):     return type(o) in __function_types__
def integerp(o):      return type(o) is int
def floatp(o):        return type(o) is float
def complexp(o):      return type(o) is complex
def numberp(o):       return type(o) in frozenset([float, int, complex])
def listp(o):         return type(o) is list
def boolp(o):         return type(o) is bool
def sequencep(x):     return getattr(type(x), '__len__', None) is not None

##
## Predicates
##
def null(x):          return not x
def evenp(x):         return x % 2 == 0
def zerop(x):         return x == 0
def plusp(x):         return x > 0
def minusp(x):        return x < 0

##
## Conses
##
def cons(x, y):       return (x, y)
def consp(o):         return type(o) is tuple and len(o) is 2
def atom(o):          return type(o) is not tuple
def car(x):           return x[0]
def cdr(x):           return x[1]
def cadr(x):          return x[1][0]

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
## Sequences
##
def aref(xs, *indices):
        r = xs
        for i in indices:
                r = r[i]
        return r
def first(xs):        return xs[0]   # don't confuse with car/cdr
def rest(xs):         return xs[1:]  # !!!

def nth_value(n, xs): return xs[n]

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
        sequence_1[start1:end1] = sequence2[start2:end2]
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
def string_right_trim(cs, s):
        "http://www.lispworks.com/documentation/lw50/CLHS/Body/f_stg_tr.htm"
        for i in range(len(s) - 1, 0, -1):
                if s[i] not in cs:
                        return s[0:i+1]
        return ""

def string_left_trim(cs, s):
        "http://www.lispworks.com/documentation/lw50/CLHS/Body/f_stg_tr.htm"
        for i in range(0, len(s) - 1, 1):
                if s[i] not in cs:
                        return s[i:]
        return ""

def string_trim(cs, s):
        "http://www.lispworks.com/documentation/lw50/CLHS/Body/f_stg_tr.htm"
        return string_left_trim(cs, string_right_trim(cs, s))

def with_output_to_string(f):
        x = make_string_output_stream()
        try:
                f(x)
                return get_output_stream_string(x)
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

def maphash(f, dict):
        return [ f(k, v) for k, v in dict.items() ]

def _remap_hash_table(f, xs):
        return { k: f(k, v) for k, v in xs.items() }

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
class __catcher_throw__(BaseException):
        def __str__(self):
                return "The ball escaped!"
        def __init__(self, ball, value, reenable_pytracer = False):
                self.ball, self.value, self.reenable_pytracer = ball, value, reenable_pytracer

def catch(ball, body):
        "This seeks the stack like mad, like the real one."
        ball = sys.intern(ball)
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
        raise __catcher_throw__(ball = ball, value = value, reenable_pytracer = boundp('_signalling_frame_'))

def make_ball(name, nonce):
        return nonce + name + nonce # Shall we do something smarter?

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
                return catch(nonce, body)

def return_from(nonce, value):
        nonce = (nonce if not functionp(nonce) else
                 (getattr(nonce, "ball", None) or
                  error("RETURN-FROM was handed a %s, but it is not cooperating in the __BLOCK__ nonce passing syntax.", nonce)))
        throw(nonce, value)

##
## Package system
##
__packages__        = dict()
__keyword_package__ = None

class package_error(Exception):
        pass

class simple_package_error(simple_condition, package_error):
        pass

def symbol_conflict_error(op, obj, pkg, x, y):
        error(simple_package_error, "%s %s causes name-conflicts in %s between the following symbols: %s, %s." %
              (op, obj, pkg, x, y))

def symbols_not_accessible_error(package, syms):
        def pp_sym_or_string(x):
                return "'%s'" % x if stringp(x) else _print_symbol(x)
        error(simple_package_error, "These symbols are not accessible in the %s package: (%s).",
              package_name(package), ", ".join(mapcar(pp_sym_or_string, syms)))

def _use_package_symbols(dest, src, syms):
        assert(packagep(dest) and packagep(src) and _dictp(syms))
        conflict_set = _mapset(_slotting('name'), syms.values()) & set(dest.accessible.keys())
        for name in conflict_set:
                if syms[name] is not dest.accessible[name]:
                        symbol_conflict_error("USE-PACKAGE", src, dest, syms[name], dest.accessible[name])
        ## no conflicts anymore? go on..
        for name, sym in syms.items():
                dest.inherited[sym].add(src)
                if name not in dest.accessible: # Addition of this conditional is important for package use loops.
                        dest.accessible[name] = sym
                        # if dest.name == "SWANK" and src.name == "INSPECTOR":
                        #         debug_printf("merging %s into %s: test: %s", s, dest, _read_symbol(_print_symbol(s)))
                if dest.module and name not in dest.module.__dict__:
                        dest.module.__dict__[name] = sym.value

def use_package(dest, src):
        "Warning: we're doing a circular package use."
        dest, src = coerce_to_package(dest), coerce_to_package(src)
        symhash = _map_into_hash(identity, src.external, key = _slotting("name"))
        _use_package_symbols(dest, src, symhash)
        src.packages_using.add(dest)
        dest.used_packages.add(src)

def package_used_by_list(package):
        package = coerce_to_package(package)
        return package.packages_using

class package(collections.UserDict):
        def __str__ (self):
                return '#<PACKAGE "%s">' % self.name
        def __bool__(self):
                return True
        def __hash__(self):
                return hash(id(self))
        def __init__(self, name, use = [],
                     ignore_python = False, python_exports = True):
                self.name = string(name)

                self.own         = set()                        # sym
                self.imported    = set()                        # sym
              # self.present     = own + imported
                self.inherited   = collections.defaultdict(set) # sym -> set(pkg) ## _mapsetn(_slotting("external"), used_packages) -> source_package
                self.accessible  = dict()                       # str -> sym      ## accessible = present + inherited
                self.external    = set()                        # sym             ## subset of accessible
              # self.internal    = accessible - external

                modname = name.lower() ## XXX: deal away with this mangling
                self.module = (sys.modules.get(modname) if modname in sys.modules else
                               None)
                self.used_packages  = set(mapcar(lambda x: coerce_to_package(x, if_null = 'error'), use))
                self.packages_using = set()
                mapc(_curry(use_package, self), self.used_packages)

                ## Import the corresponding python dictionary.  Intern depends on
                if not ignore_python:
                        moddict = self.module and dict(self.module.__dict__)
                        if moddict:
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
                                        if (python_exports and key[0] != '_' and
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

def find_package(x):
        return __packages__.get(x) if x in __packages__ else None
def coerce_to_package(x, if_null = 'current'):
        return (x                         if packagep(x) else
                find_package(x)           if stringp(x) else
                symbol_value("_package_") if not x and if_null == 'current' else
                error("Asked to coerce object >%s< of type %s to a package.", x, type(x)))

def defpackage(name, use = [], export = []):
        p = package(name, use = use)
        for symname in export:
                _not_implemented("DEFPACKAGE: :EXPORT keyword") # XXX: populate the for-INTERN-time-export set of names
        return p

def in_package(name):
        setq("_package_", coerce_to_package(name))

def _print_symbol2(x, package = None): return _letf(coerce_to_package(package),
                                                    lambda p: (x.name if x.package and p.accessible[x.name] is x else
                                                               str(x)))
def print_keyword(s):
        return ":%s" % s.name

class symbol():
        def __str__(self):
                return (print_keyword if self.package is __keyword_package__ else _print_symbol)(self)
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
        return intern(s_or_n, coerce_to_package(package))

def _keyword(s, upcase = True):
        return _intern((s.upper() if upcase else s), __keyword_package__)[0]

def symbol_relation(x, p):
        "NOTE: here we trust that X belongs to P, when it's a symbol."
        s = (p.accessible.get(x) if x in p.accessible else None) if stringp(x) else x
        if s is not None:
                return (_keyword("inherited") if s.name in p.inherited else
                        _keyword("external")  if s in p.external else
                        _keyword("internal"))

def find_symbol(x, package = None):
        p = coerce_to_package(package)
        s = p.get(x) if x in p else None
        if s is not None:
                # format(t, "FIND-SYMBOL:%s, %s -> %s, %s\n", 
                #        x, package, s, symbol_relation(s, p))
                return s, symbol_relation(s, p)
        else:
                return None, None
def _find_symbol0(x, package = None): return find_symbol(x, package)[0]

def _find_symbol_or_fail(x, package = None):
        p = coerce_to_package(package)
        sym, foundp = find_symbol(x, p)
        return (sym if foundp else
                symbols_not_accessible_error(p, [x]))

def _intern(x, package = None):
        p = coerce_to_package(package)
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
        return s, symbol_relation(s, found_in_package) if found_in_package else None
def _intern0(x, package = None): return intern(x, package)[0]

def _import(symbols, package = None):
        p = coerce_to_package(package)
        symbols = _ensure_list(symbols)
        format(t, "importing %s into %s\n", symbols, p)
        for s in symbols:
                ps = p.get(s.name) if s.name in p else None
                if ps is not None: # conflict
                        symbol_conflict_error("IMPORT", s, p, s, ps)
                else:
                        p.imported.add(s)
                        p.accessible[s.name] = s
        return True

def export(symbols, package = None):
        symbols, package = _ensure_list(symbols), coerce_to_package(package)
        assert(every(symbolp, symbols))
        symdict = _map_into_hash(identity, symbols, key = _slotting("name"))
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
                error(TypeError, "%s cannot be coerced to string." % x))

def in_package(name):
        return setq("_package_", find_package(string(name)))

def _pythonise_lisp_name(x):
        def _sub(cs):
                acc = ""
                for c in cs:
                        acc += "_" if c in "-*" else c
                return acc
        ret = _sub(x).lower()
        # debug_printf("==> Python(Lisp %s) == %s", x, ret)
        return ret

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

def _init_package_system_0():
        # debug_printf("   --  -- [ package system init..")
        global __packages__
        global __keyword_package__
        global t, nil
        __packages__ = dict()
        __keyword_package__ = package("KEYWORD", ignore_python = True)
        cl = package("CL")
        intern(".", cl)

        t                  = _intern0("T", cl)       # Nothing much works without these..
        nil                = _intern0("NIL", cl)
        t.value, nil.value = t, nil     # Self-evaluation.
        export([t, nil] + mapcar(lambda n: _intern0(n, cl),
                                 ["QUOTE", "OR", "SOME"]),
               cl)
_init_package_system_0()

##
## Dynamic scope
##
__global_scope__ = dict()

class thread_local_storage(threading.local):
        def __init__(self):
                self.dynamic_scope = []

__tls__ = thread_local_storage()

def _init_reader_0():
        "SETQ, LET and BOUNDP all need this to mangle names."
        __global_scope__["_READ_CASE_"] = _keyword("upcase", upcase = True)
_init_reader_0()

def _boundp(name):
        name = _coerce_to_symbol_name(name)
        for scope in reversed(__tls__.dynamic_scope):
                if name in scope:
                        return t
        if name in __global_scope__:
                return t

def _symbol_value(name):
        for scope in reversed(__tls__.dynamic_scope):
                if name in scope:
                        return scope[name]
        if name in __global_scope__:
                return __global_scope__[name]
        error(AttributeError, "Unbound variable: %s." % name)

def _coerce_cluster_keys_to_symbol_names(dict):
        return { _coerce_to_symbol_name(var):val for var, val in dict.items() }

def boundp(symbol):
        return _boundp(_coerce_to_symbol_name(symbol))

def symbol_value(symbol):
        return _symbol_value(_coerce_to_symbol_name(symbol))

def setq(name, value):
        dict = __tls__.dynamic_scope[-1] if __tls__.dynamic_scope else __global_scope__
        # if name == "_scope_":
        #         _write_string("setq(%s -(%s)> %s, %s)" %
        #                       (name, symbol_value("_read_case_"), _case_xform(symbol_value("_read_case_"), name), value),
        #                       sys.stdout)
        dict[_coerce_to_symbol_name(name)] = value
        return value

class env_cluster(object):
        def __init__(self, cluster):
                self.cluster = cluster
        def __enter__(self):
                __tls__.dynamic_scope.append(_coerce_cluster_keys_to_symbol_names(self.cluster))
        def __exit__(self, t, v, tb):
                __tls__.dynamic_scope.pop()

class dynamic_scope(object):
        "Courtesy of Jason Orendorff."
        def let(self, **keys):
                return env_cluster(keys)
        def maybe_let(self, p, **keys):
                return env_cluster(keys) if p else None
        def __getattr__(self, name):
                return symbol_value(name)
        def __setattr__(self, name, value):
                error(AttributeError, "Use SETQ to set special globals.")

__dynamic_scope__ = dynamic_scope()
env = __dynamic_scope__             # shortcut..

def progv(vars = None, vals = None, body = None, **cluster):
        """Two usage modes:
progv(['foovar', 'barvar'],
      [3.14, 2.71],
      lambda: body())

with progv(foovar = 3.14,
           barvar = 2.71):
      body()

..with the latter being lighter on the stack frame usage."""
        if body:
                with env_cluster(_map_into_hash(lambda vv: (_coerce_to_symbol_name(vv[0]), vv[1]),
                                                zip(vars, vals))):
                        return body()
        else:
                return env_cluster(_coerce_cluster_keys_to_symbol_names(cluster))

def _init_package_system_1():
        setq("_package_", package("COMMON_LISP_USER", use = ["CL"]))

_init_package_system_1()

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

__standard_io_syntax__ = dict(_print_array_           = t,
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
                              _print_right_margin_    = nil)

for var, standard_value in __standard_io_syntax__.items():
        setq(var, standard_value)

def _print_symbol(s, gensym = True, case = None):
        return "%s%s%s" % (s.package.name if s.package else
                           ("#" if gensym else ""),
                           (":" if not gensym or s.package else "") if not s.package or (s in s.package.external) else
                           "::",
                           _case_xform(_defaulting(case, "_print_case_"), s.name))

def with_standard_io_syntax(body):
        # XXX: is this true?
        return body()

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
        array           = array           if array           is not None else symbol_value("_print_array_")
        base            = base            if base            is not None else symbol_value("_print_base_")
        case            = case            if case            is not None else symbol_value("_print_case_")
        circle          = circle          if circle          is not None else symbol_value("_print_circle_")
        escape          = escape          if escape          is not None else symbol_value("_print_escape_")
        gensym          = gensym          if gensym          is not None else symbol_value("_print_gensym_")
        length          = length          if length          is not None else symbol_value("_print_length_")
        level           = level           if level           is not None else symbol_value("_print_level_")
        lines           = lines           if lines           is not None else symbol_value("_print_lines_")
        miser_width     = miser_width     if miser_width     is not None else symbol_value("_print_miser_width_")
        pprint_dispatch = pprint_dispatch if pprint_dispatch is not None else symbol_value("_print_pprint_dispatch_")
        pretty          = pretty          if pretty          is not None else symbol_value("_print_pretty_")
        radix           = radix           if radix           is not None else symbol_value("_print_radix_")
        readably        = readably        if readably        is not None else symbol_value("_print_readably_")
        right_margin    = right_margin    if right_margin    is not None else symbol_value("_print_right_margin_")
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
                                string += '('
                                max = len(object)
                                if max:
                                        for i in range(0, max):
                                                string += do_write_to_string(object[i])
                                                if i != (max - 1):
                                                        string += " "
                                string += ')'
                        elif symbolp(object) or integerp(object) or floatp(object):
                                string += str(object)
                        elif object in obj2lisp_xform:
                                string += obj2lisp_xform[object]
                        elif type(object).__name__ == 'builtin_function_or_method':
                                string += '"#<builtin %s 0x%x>"' % (object.__name__, id(object))
                        elif stringp(object):
                                string += '"%s"' % re.sub(r'(["\\])', r'\\\\1', object)
                        else:
                                string += "<%s>" % (object,)
                                # error("Can't write object %s", object)
                        return string
                return write_to_string_loop(object)
        ret = do_write_to_string(object)
        # debug_printf("===> %s", ret)
        return ret

def prin1_to_string(object): return write_to_string(object, escape = t)
def princ_to_string(object): return write_to_string(object, escape = nil, readably = nil)

def write(object, stream = t, **args):
        write_string(write_to_string(object, **args), stream)
        return object

def prin1(object, stream = t): return write(object, stream = stream, escape = t)
def princ(object, stream = t): return write(object, stream = stream, escape = nil, readably = nil)

def print_(object, stream = t):
        terpri(stream)
        prin1(object, stream)
        write_char(" ", stream)
        return object

def pprint(object, stream = t):
        terpri(stream)
        write(object, stream = stream, escape = t, pretty = t)
        return object

def format(stream, format_control, *format_arguments):
        string = format_control % format_arguments
        if  stream is nil:
                return string
        else:
                write_string(string, stream)

##
## Reader
##
setq("_read_case_", _keyword("upcase"))

def parse_integer(xs, junk_allowed = nil, radix = 10):
        l = len(xs)
        def hexcharp(x): return x.isdigit() or x in ['a', 'b', 'c', 'd', 'e', 'f']
        (test, xform) = ((str.isdigit, identity)      if radix == 10 else
                         (hexcharp,    float.fromhex) if radix == 16 else
                         _not_implemented("PARSE-INTEGER only implemented for radices 10 and 16."))
        for end in range(0, l):
                if not test(xs[end]):
                        if junk_allowed:
                                end -= 1
                                break
                        else:
                                error("Junk in string '%s'.", xs)
        return int(xform(xs[:(end + 1)]))

def _read_symbol(x, package = None, case = _keyword("upcase")):
        # debug_printf("_read_symbol >%s<, x[0]: >%s<", x, x[0])
        name, p = ((x[1:], __keyword_package__)
                   if x[0] == ":" else
                   _letf(x.find(":"),
                         lambda index:
                                 (_if_let(find_package(x[0:index].upper()),
                                          lambda p:
                                                  (x[index + 1:], p),
                                          lambda:
                                                  error("Package \"%s\" doesn't exist, while reading symbol \"%s\".",
                                                        x[0:index].upper(), x))
                                  if index != -1 else
                                  (x, coerce_to_package(package)))))
        return _intern0(_case_xform(case, name), p)

@block
def read_from_string(string, eof_error_p = True, eof_value = nil,
                     start = 0, end = None, preserve_whitespace = None):
        "Does not conform."
        # _here("from '%s'" % string)
        # string = re.sub(r"swank\:lookup-presented-object ", r"lookup_presented_object ", string)
        pos, end = start, (end or len(string))
        def handle_short_read_if(test):
                # _here("< %s" % (test,))
                if test:
                        (error("EOF during read") if eof_error_p else
                         return_from(read_from_string, eof_value))
        def read():
                skip_whitespace()
                char = string[pos]
                # _here("> '%s', by '%s'" % (string[pos:], char))
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
                                        error("READ-FROM-STRING: unrecognized escape character '%s'.", char2)
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
        # return read()
        ## XXX: Issue PROBABLE-LIMIT-EXCEEDED -- mystery -- this:
        ret = handler_case(read,
                           IndexError = lambda c: handle_short_read_if(True))
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

##
## Streams
##
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
                                error(stream_type_error, "%s is not an %s stream: '%s'.",
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

def close(x):
        x.close()

def finish_output(stream = t):
        stream is not nil and _coerce_to_stream(stream).flush()

def force_output(*args, **keys):
        finish_output(*args, **keys)

##
## Pythonese execution tracing: for HANDLER-BIND.
##
__tracer_hooks__   = dict() # allowed keys: 'call', 'line', 'return', 'exception', 'c_call', 'c_return', 'c_exception'
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
        _set_tracer_hook('exception', fn)
        return True

##
## Condition system
##
setq("__handler_clusters__", [])

def make_condition(datum, *args, default_type = Exception, **keys):
        """
It's a slightly weird interpretation of MAKE-CONDITION, as the latter
only accepts symbols as DATUM, while this one doesn't accept symbols
at all.
"""
        # format(t, "stringp: %s\nclassp: %s\nBaseException-p: %s\n",
        #        stringp(datum),
        #        typep(datum, type_of(BaseException)),
        #        typep(datum, BaseException))
        cond = (default_type(datum % args) if stringp(datum) else
                datum(*args, **keys)       if typep(datum, type_of(BaseException)) else
                datum                      if typep(datum, BaseException) else
                error(TypeError, "The first argument to MAKE-CONDITION must either a string, a condition type or a condition, was: %s, of type %s.",
                      datum, type_of(datum)))
        # format(t, "made %s %s %s\n", datum, args, keys)
        # format(t, "    %s\n", cond)
        return cond

setq("_presignal_hook_", nil)
setq("_prehandler_hook_", nil)
setq("_debugger_hook_",  nil)

def signal(condition):
        "XXX: this is crippled by inheritance-ignorant exact matching of the condition name."
        # format(t, "Signalling %s", condition)
        name = type_of(condition).__name__
        for cluster in reversed(env.__handler_clusters__):
                # format(t, "Analysing cluster %s for '%s'.", cluster, name)
                if name in cluster:
                        hook = symbol_value("_prehandler_hook_")
                        if hook:
                                frame = cluster['__frame__']
                                hook(condition, frame, hook)
                        cluster[name](condition)
        return nil

def error(datum, *args, **keys):
        "With all said and done, this ought to jump right into __CL_CONDITION_HANDLER__."
        raise make_condition(datum, *args, **keys)

def warn(datum, *args, **keys):
        condition = make_condition(datum, *args, default_type = simple_warning, **keys)
        signal(condition)
        format(symbol_value("_error_output_"), "%s", condition)
        return nil

def invoke_debugger(condition):
        "XXX: non-compliant: doesn't actually invoke the debugger."
        debugger_hook = symbol_value("_debugger_hook_")
        if debugger_hook:
                with env.let(_debugger_hook_ = nil):
                        debugger_hook(cond, debugger_hook)
        error(BaseError, "INVOKE-DEBUGGER fell through.")

__main_thread__ = threading.current_thread()
def _report_condition(condition, stream = None, backtrace = None):
        stream = _defaulting(stream, "_debug_io_")
        format(stream, "%sondition of type %s: %s\n",
               (("In thread '%s': c" % threading.current_thread().name)
                if threading.current_thread() is not __main_thread__ else 
                "C"),
               type(condition), condition)
        if backtrace:
                _backtrace(-1, stream)
        return t

def _maybe_reporting_conditions_on_hook(p, hook, body, backtrace = None):
        if p:
                old_hook_value = symbol_value(hook)
                def wrapped_hook(condition, hook_value):
                        "Let's honor the old hook."
                        _report_condition(condition,
                                          stream = symbol_value("_debug_io_"),
                                          backtrace = backtrace)
                        if old_hook_value:
                                old_hook_value(condition, old_hook_value)
                with env.maybe_let(p, **{_coerce_to_symbol_name(hook): wrapped_hook}):
                        return body()
        else:
                return body()

def __cl_condition_handler__(condspec, frame):
        type, condition, traceback = condspec
        # print_frames(frames_upward_from(frame))
        if not typep(condition, __catcher_throw__): # no need to delay the inevitable
                with env.let(_traceback_ = traceback,
                             _signalling_frame_ = frame): # These bindings are the deviation from the CL standard.
                        presignal_hook = symbol_value("_presignal_hook_")
                        if presignal_hook:
                                with env.let(_presignal_hook_ = nil):
                                        presignal_hook(condition, presignal_hook)
                        signal(condition)
                        debugger_hook = symbol_value("_debugger_hook_")
                        if debugger_hook:
                                with env.let(_debugger_hook_ = nil):
                                        debugger_hook(condition, debugger_hook)
        # At this point, the Python condition handler kicks in,
        # and the stack gets unwound for the first time.
        #
        # ..too bad, we've already called all HANDLER-BIND-bound
        # condition handlers.
        # If we've hit any HANDLER-CASE-bound handlers, then we won't
        # even reach this point, as the stack is already unwound.
_set_condition_handler(__cl_condition_handler__)

def handler_bind(fn, no_error = identity, **handlers):
        "Works like real HANDLER-BIND, when the conditions are right.  Ha."
        value = None

        # this is:
        #     pytracer_enabled_p() and condition_handler_active_p()
        # ..inlined for speed.
        if _pytracer_enabled_p() and 'exception' in __tracer_hooks__ and __tracer_hooks__['exception'] is __cl_condition_handler__:
                ### XXX: This is a temporary shitty workaround for broken FIND-CLASS (oh, yes, Python, thank you again!)
                # resolved = dict()
                # for type, handler in handlers.items():
                #         resolved[resolve_exception_type(type)] = handler
                handlers['__frame__'] = _this_frame()
                with env.let(__handler_clusters__ = env.__handler_clusters__ + [handlers]):
                        # format(t, "crap ok, going on, new __handler_clusters__ = %s", env.__handler_clusters__)
                        return no_error(fn())
        else:
                # old world case..
                # format(t, "crap FAIL: pep %s, exhook is cch: %s",
                #        _pytracer_enabled_p(), __tracer_hooks__.get('exception') is __cl_condition_handler__)
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

def handler_case(body, no_error = identity, **handlers):
        "Works like real HANDLER-CASE, when the conditions are right.  Ha."
        nonce            = gensym("HANDLER-CASE")
        wrapped_handlers = { cond_name: (lambda cond: return_from(nonce, handler(cond)))
                             for cond_name, handler in handlers.items () }
        return catch(nonce,
                     lambda: handler_bind(body, no_error = no_error, **wrapped_handlers))

def ignore_errors(body):
        return handler_case(body,
                            Exception = lambda _: None)

##
## Restarts
##
class restart(_servile):
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
#                           test_function        = lambda condition: visible_p(condition))))
setq("__restart_clusters__", [])

def restartp(x):
        return typep(x, restart)

def restart_name(x):
        return x.name

def _specs_restarts_args(restart_specs):
        # format (t, "_s_r: %s", restart_specs)
        restarts_args = dict()
        for name, spec in restart_specs.items():
                function, options = ((spec[0], spec[1]) if _tuplep(spec) else
                                     spec, dict())
                restarts_args[name] = _updated_dict(options, dict(name = name,
                                                                  function = function))
        return restarts_args

##
# XXX: :TEST-FUNCTION is currently IGNORED!
##
def _restart_bind(body, restarts_args):
        with env.let(__restart_clusters__ = env.__restart_clusters__ + [_remap_hash_table(lambda _, restart_args: restart(**restart_args), restarts_args)]):
                return body()

def restart_bind(body, **restart_specs):
        return _restart_bind(body, _specs_restarts_args(restart_specs))

def _restart_case(body, **restarts_args):
        nonce            = gensym("RESTART-CASE")
        wrapped_restarts_args = {
                restart_name: _letf(restart_args['function'],
                                    lambda function:
                                            _updated_dict(restart_args,
                                                          dict(function =
                                                               lambda *args, **keys: return_from(nonce, function(*args, **keys)))))
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
        return restart_case(body, **{ name: dict(name            = name,
                                                 function        = lambda: None,
                                                 report_function = lambda stream: format(stream, "%s", description)) })

def restart_condition_association_check(condition, restart):
        """
When CONDITION is non-NIL, only those restarts are considered that are
either explicitly associated with that condition, or not associated
with any condition; that is, the excluded restarts are those that are
associated with a non-empty set of conditions of which the given
condition is not an element. If condition is NIL, all restarts are
considered.
"""
        return (not condition or
                "associated_conditions" not in restart.__dict__ or
                condition in restart.associated_conditions)

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
                for cluster in reversed(env.__restart_clusters__):
                        # format(t, "Analysing cluster %s for '%s'.", cluster, name)
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
                # format(t, "Analysing cluster %s for '%s'.", cluster, name)
                restarts.extend(remove_if_not(_curry(restart_condition_association_check, condition), cluster.values())
                                if condition else
                                cluster.values())
        return restarts

def invoke_restart(restart, *args, **keys):
        """
Calls the function associated with RESTART, passing arguments to
it. Restart must be valid in the current dynamic environment.
"""
        assert(stringp(restart) or restartp(restart))
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
        assert(stringp(restart) or restartp(restart))
        restart = restart if restartp(restart) else find_restart(restart)
        return invoke_restart(*restart.interactive_function())

##
## Environment
##
def sleep(x):
        return time.sleep(x)

def user_homedir_pathname():
        return os.path.expanduser("~")

###
### Missing stuff
###
# def peek_char(peek_type, stream = nil, eof_error_p = True, eof_value = None, recursive_p = None):
#         return "a"
#
# def read_sequence(sequence, stream, start = 0, end = None):
#         return 0
