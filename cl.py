###
### Some surfacial Common Lisp compatibility.
###
import io
import sys
import inspect
import builtins
from functools import reduce
from neutrality import stringp, printf, fprintf

## "constants"
t = True
nil = None

most_positive_fixnum = 67108864

## basic
__iff__ = { True:  lambda x, _: x,
            False: lambda _, y: y }
def iff(val, consequent, antecedent):
        "This restores sanity."
        return __iff__[not not val](consequent, antecedent)()

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

## types
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
                if (ctype is True) or typep(val, ctype):
                        return result

def etypecase(val, *clauses):
        for (ctype, result) in clauses:
                if (ctype is True) or typep(val, ctype):
                        return result
        else:
                raise TypeError("%s fell through ETYPECASE expression. Wanted one of (%s)." %
                                (val, ", ".join(mapcar(lambda c: c[0].__name__, clauses))))

def check_type(x, type):
        if not typep(x, type):
                raise TypeError("The value %s is not of type %s." %
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

## type predicates
def functionp(o):     return getattr(o, '__call__', False) and True
def integerp(o):      return type(o) is int
def floatp(o):        return type(o) is float
def listp(o):         return type(o) is list
def boolp(o):         return type(o) is bool
def sequencep(x):     return getattr(type(x), '__len__', None) is not None

## predicates
def null(x):          return not x
def evenp(x):         return x % 2 == 0
def zerop(x):         return x == 0
def plusp(x):         return x > 0
def minusp(x):        return x < 0

## conses
def cons(x, y):       return (x, y)
def consp(o):         return type(o) is tuple and len(o) is 2
def atom(o):          return type(o) is not tuple
def car(x):           return x[0]
def cdr(x):           return x[1]
def cadr(x):          return x[1][0]

## functions
def identity(x):
        return x

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

## sequences
def aref(xs, *indices):
        r = xs
        for i in indices:
                r = r[i]
        return r
def first(xs):        return xs[0]   # don't confuse with car/cdr
def rest(xs):         return xs[1:]  # !!!

def nth_value(n, xs): return xs[n]

def subseq(xs, start, end = None):
        return xs[start:end] if end else  xs[start:]

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

def remove_if(f, xs, key = identity):
        if listp(xs):              return [ x for x in xs if not f(key(x)) ]
        elif typep(xs, set):       return set (x for x in xs if not f(key(x)))
        elif typep(xs, frozenset): return frozenset(x for x in xs if not f(key(x)))
        elif typep(xs, dict):
                acc = dict()
                for x in xs:
                        if not f(key(x)):
                                acc[x] = xs[x]
                return acc
        else:                      return [ x for x in xs if not f(key(x)) ]

def remove_if_not(f, xs, key = identity):
        if listp(xs):              return [ x for x in xs if f(key(x)) ]
        elif typep(xs, set):       return set(x for x in xs if f(key(x)))
        elif typep(xs, frozenset): return frozenset(x for x in xs if f(key(x)))
        elif typep(xs, dict):
                acc = dict()
                for x in xs:
                        if f(key(x)):
                                acc[x] = xs[x]
                return acc
        else:                      return [ x for x in xs if f(key(x)) ]

def remove(elt, xs, test = eq, key = identity):
        if listp(xs):              return [ x for x in xs if not test(elt, key(x)) ]
        elif typep(xs, set):       return set(x for x in xs if not test(elt, key(x)))
        elif typep(xs, frozenset): return frozenset(x for x in xs if not test(elt, key(x)))
        elif typep(xs, dict):
                acc = dict()
                for x in xs:
                        if not test(elt, key(x)):
                                acc[x] = xs[x]
                return acc
        else:                      return [ x for x in xs if not test(elt, key(x)) ]

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

## strings
def format(stream, format_control, *format_arguments):
        string = format_control % format_arguments
        if not stream:
                return string
        elif stream is True:
                printf(string)
        else:
                fprintf(stream, string)

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

def parse_integer(xs, junk_allowed = None):
        if junk_allowed:
                try:
                        return int(xs)
                except:
                        return None
        else:
                return int(xs)

def with_output_to_string(f):
        x = make_string_output_stream()
        try:
                f(x)
                return get_output_stream_string(x)
        finally:
                close(x)

## streams
def make_string_output_stream():
        return io.StringIO()

def get_output_stream_string(x):
        return x.getvalue()

def close(x):
        x.close()

def finish_output(stream = sys.stdout):
        stream.flush()

## sets
def union(x, y):
        return x | y

def intersection(x, y):
        return x & y

## dicts
def gethash(key, dict):
        return dict.get(key), key in dict

## catch/throw
# WARNING: non-specific try-except clauses break this!
class catcher_throw(BaseException):
        def __init__(self, name, value):
                self.name, self.value = name, value

def return_from(name, value):
        raise catcher_throw(name, value)

def block(fn):
        # source = builtins.intern(inspect.getsource(fn))  # for a more adequate nonce generation..
        name = sys.intern(fn.__name__)
        def unlambda(*args, **kwargs):
                try:
                        ret = fn(*args, **kwargs)
                        return ret
                except catcher_throw as ct:
                        if ct.name is name:
                                return ct.value
                        else:
                                raise
        return unlambda

## dynamic scope (XXX: NOT PER-THREAD YET!!!)
__dynamic_binding_clusters__ = []

class env_block(object):
        def __init__(self, kwargs):
                self.kwargs = kwargs
        def __enter__(self):
                __dynamic_binding_clusters__.append(self.kwargs)
        def __exit__(self, t, v, tb):
                __dynamic_binding_clusters__.pop()

class dynamic_scope(object):
        "Courtesy of Jason Orendorff."
        def __getattr__(self, name):
                for scope in reversed(__dynamic_binding_clusters__):
                        if name in scope:
                                return scope[name]
                raise AttributeError("Special %s not bound." % name)
        def let(self, **keys):
                return env_block(keys)
        def boundp(self, name):
                for scope in reversed(__dynamic_binding_clusters__):
                        if name in scope:
                                return True
        def __setattr__(self, name, value):
                raise AttributeError("Env variables can only be set using `with env.let()`.")

def setq(name, value):
        __dynamic_binding_clusters__[-1][name] = value

__cl_top_level_dynamic_scope__ = dict()
class cl_dynamic_scope(dynamic_scope):
        def __init__(self):
                __dynamic_binding_clusters__.append(__cl_top_level_dynamic_scope__)

__dynamic_scope__ = cl_dynamic_scope()
env = __dynamic_scope__             # shortcut..

## Pythonese execution tracing: for HANDLER-BIND.
__tracer_hooks__   = dict() # allowed keys: 'call', 'line', 'return', 'exception', 'c_call', 'c_return', 'c_exception'
def set_tracer_hook(type, fn): __tracer_hooks__[type] = fn
def     tracer_hook(type):     return __tracer_hooks__.get(type)

def pytracer(frame, event, arg):
        method = tracer_hook(event)
        if method:
                method(arg, frame)
        return pytracer

__tracer_enabled__ = False
def pytracer_enabled(): return __tracer_enabled__
def enable_pytracer():  global __tracer_enabled__; sys.settrace(pytracer); __tracer_enabled__ = True
def disable_pytracer(): global __tracer_enabled__; sys.settrace(None);     __tracer_enabled__ = False

__condition_handler__ = None
def condition_handler(cond, frame):
        if __condition_handler__:
                __condition_handler__(cond, frame)

def set_condition_handler(fn):         global __condition_handler__; __condition_handler__ = fn
def     condition_handler_enabled_p(): return __condition_handler__

def condition_handler_active_p():
        return __condition_handler__ and tracer_hook('exception') is condition_handler

def activate_condition_handler(fn):
        set_condition_handler(fn)
        set_tracer_hook('exception', fn)

## debugging
# def debugger_hook(cond, frame):
#         pass
# activate_condition_handler(debugger_hook)

## conditions
setq("__handler_clusters__", [])

def __cl_condition_handler__(cond, frame):
        name = type_of(cond).__name__
        for cluster in reversed(env.__handler_clusters__):
                if functionp(cluster):
                        cluster()
                elif dictp(cluster) and name in cluster:
                        cluster[name](cond)

def unwind_protect(form, fn):
        with env.let(__handler_clusters__ = __handler_clusters__ + [fn]):
                return form()

activate_condition_handler(__cl_condition_handler__)

def error(datum, *args):
        "With all said and done, this ought to jump straight above, into __CL_CONDITION_HANDLER__."
        raise Exception(datum % args) if stringp(datum) else datum(*args)

def handler_case(fn, error = lambda c: None, no_error = identity):
        "Unable to handle things in an ad-hoc manner, unlike actual CL:HANDLER-CASE."
        value = None
        try:
                value = fn()
        except Exception as cond:
                return error(cond)
        finally:
                return no_error(value)

def handler_bind(fn, no_error = identity, **handlers):
        "Unwinds stack, unlike actual CL:HANDLER-BIND.  Also, see HANDLER-CASE docstring above."
        value = None

        # this is:
        #     pytracer_enabled() and condition_handler_active_p()
        # ..inlined for speed.
        if __tracer_enabled__ and __condition_handler__ and __tracer_hooks__.get('exception') is condition_handler:
                ### XXX: This is a temporary shitty workaround for broken FIND-CLASS (oh, yes, Python, thank you again!)
                # resolved = dict()
                # for type, handler in handlers.items():
                #         resolved[resolve_exception_type(type)] = handler
                with env.let(__handler_clusters__ = env.__handler_clusters__ + [handlers]):
                        return no_error(fn())
        else:
                # old world case..
                try:
                        value = fn()
                except Exception as cond:
                        return error(cond)
                finally:
                        return no_error(value)
# (handler-case form
#  (type1 (var1) . body1)
#  (type2 (var2) . body2) ...)

# is approximately equivalent to:
# (block #1=#:g0001
#  (let ((#2=#:g0002 nil))
#      (tagbody
#        (handler-bind ((type1 #'(lambda (temp)
#                                        (setq #1# temp)
#                                        (go #3=#:g0003)))
#                       (type2 #'(lambda (temp)
#                                        (setq #2# temp)
#                                        (go #4=#:g0004))) ...)
#        (return-from #1# form))
#          #3# (return-from #1# (let ((var1 #2#)) . body1))
#          #4# (return-from #1# (let ((var2 #2#)) . body2)) ...)))
