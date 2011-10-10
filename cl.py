###
### Some surfacial Common Lisp compatibility.
###
import io
from functools import reduce
from neutrality import stringp, printf, fprintf

## "constants"
t = True
nil = None

most_positive_fixnum = 67108864

## basic
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

def typecase(val, *clauses):
        for (ctype, result) in clauses:
                if (ctype is True) or typep(val, ctype):
                        return result

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

## types
def subtypep(sub, super):
        return issubclass(sub, super)

def typep(x, super):
        return isinstance(x, super)

def the(type, x):
        assert(typep(x, type))
        return x

def functionp(o):         return getattr(o, '__call__', False) and True
def integerp(o):          return type(o) is int
def floatp(o):            return type(o) is float
def listp(o):             return type(o) is list
def boolp(o):             return type(o) is bool
def sequencep(x):         return getattr(type(x), '__len__', None) is not None

def coerce(x, type):
        if type(x) is type:
                return x
        elif type is list:
                return list(x)
        elif type is set:
                return set(x)
        elif type is dict:
                return dict.fromkeys(x)

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

## sets
def union(x, y):
        return x | y

def intersection(x, y):
        return x & y

## predicates
def null(x):          return not x
def evenp(x):         return x % 2 == 0
def zerop(x):         return x == 0
def plusp(x):         return x > 0
def minusp(x):        return x < 0

## conditions
def error(datum, *args):
        raise Exception(datum % args) if stringp(datum) else datum(*args)

def handler_bind(fn, error = lambda c: None):
        "Doesn't work as an actual CL:HANDLER-BIND."
        try:
                return fn()
        except Exception as cond:
                return error(cond)
