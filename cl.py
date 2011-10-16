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

## secret, non-CL things, without which life can be very, very painful
def ___(str, expr):
        printf("%s: %s" % (str, expr))
        return expr
def _updated_dict(to, from_):
        to.update(from_)
        return to
def _letf(value, body):
        return body(value)
def _tuplep(x): return type(x) is tuple
class _servile():
        def __repr__(self):
                return "#%s(%s)" % (type(self).__name__,
                                    ", ".join(maphash(lambda k, v: "%s = %s" % (k, v),
                                                      self.__dict__)))
        def __init__(self, **keys):
                self.__dict__.update(keys)

## symbols
__gensym_counter__ = 0
def gensym(x = "G"):
        "Not a real GENSYM, as it returns merely a string."
        global __gensym_counter__
        __gensym_counter__ += 1
        return sys.intern(x + str(__gensym_counter__))

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
def print_to_string(x):
        return with_output_to_string(s,
                                     lambda: print(x, file = s, end = ''))

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

def maphash(f, dict):
        return [ f(k, v) for k, v in dict.items() ]

def _remap_hash_table(f, xs):
        return { k: f(k, v) for k, v in xs.items() }

## py-cltl2, if you like..
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

def all_threads_frames():
        return sys._current_frames()

def this_frame():
        return sys._getframe(1)

def exception_frame():
        return sys.exc_info()[2].tb_frame

def frames_upward_from(f):
        return [f] + (frames_upward_from(f.f_back) if f.f_back else [])

def frame_info(f):
        "Return frame (function, lineno, locals, globals, builtins)."
        return (f.f_code,
                f.f_lineno,
                f.f_locals,
                f.f_globals,
                f.f_builtins,
                )

def frame_fun(f):               return f.f_code
def frame_lineno(f):            return f.f_lineno
def frame_locals(f):            return f.f_locals
def frame_globals(f):           return f.f_globals
def frame_local_value(f, name): return f.f_locals[name]

### XXX: this is the price of Pythonic pain
__ordered_frame_locals__ = dict()
def ordered_frame_locals(f):
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
def fun_name(f):       return f.co_name
def fun_filename(f):   return f.co_filename
def fun_bytecode(f):   return f.co_code
def fun_constants(f):  return f.co_consts

def pp_frame(f, align = None):
        fun = frame_fun(f)
        fun_name, fun_params, filename = fun_info(fun)[:3]
        padding = " " * ((align or len(filename)) - len(filename))
        return "%s: %s(%s)" % (padding + filename, fun_name, ", ".join(fun_params))

def print_frame(f):
        print(pp_frame(f))

def print_frames(fs):
        mapc(lambda i, f: print("%2d: %s" % (i, pp_frame(f))), *zip(*enumerate(fs)))

## non-local control transfers
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
                # printf("catcher %s, ball %s -> %s", ct.ball, ball, "caught" if ct.ball is ball else "missed")
                if ct.ball is ball:
                        if ct.reenable_pytracer:
                                enable_pytracer()
                        return ct.value
                else:
                        raise

def throw(ball, value):
        "Stack this seeks, like mad, like the real one."
        raise __catcher_throw__(ball = ball, value = value, reenable_pytracer = env.boundp('_signalling_frame_'))

def make_ball(name, nonce):
        return nonce + name + nonce # Shall we do something smarter?

def __block__(fn):
        "An easy decorator-styled interface for block establishment."
        nonce = gensym("BLOCK")
        ret = (lambda *args, **kwargs:
                       catch(nonce,
                             lambda: fn(*args, **kwargs)))
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
        raise throw(nonce, value)


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
                raise AttributeError("Unbound variable: %s." % name)
        def let(self, **keys):
                return env_block(keys)
        def boundp(self, name):
                for scope in reversed(__dynamic_binding_clusters__):
                        if name in scope:
                                return True
        def __setattr__(self, name, value):
                raise AttributeError("Use SETQ to set special globals.")

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

def pytracer_enabled_p(): return sys.gettrace() is pytracer
def enable_pytracer():    sys.settrace(pytracer)
def disable_pytracer():   sys.settrace(None)

def set_condition_handler(fn):
        set_tracer_hook('exception', fn)

## debugging
# def debugger_hook(cond, frame):
#         pass
# activate_condition_handler(debugger_hook)

## conditions
setq("__handler_clusters__", [])

def signal(condition):
        # printf("Signalling %s", condition)
        name = type_of(condition).__name__
        for cluster in reversed(env.__handler_clusters__):
                # printf("Analysing cluster %s for '%s'.", cluster, name)
                if name in cluster:
                        cluster[name](cond)

def __cl_condition_handler__(cond, frame):
        type, cond, traceback = cond
        # printf("__cl_condition_handler__(%s, %s), line %d", cond, pp_frame(frame), traceback.tb_lineno)
        # print_frames(frames_upward_from(frame))
        with env.let(_traceback_ = traceback,
                     _signalling_frame_ = frame): # These bindings are the deviation from the CL standard.
                signal(cond)
        # At this point, the Python condition handler kicks in,
        # and the stack gets unwound for the first time.
        #
        # ..too bad, we've already called all HANDLER-BIND-bound
        # condition handlers.
        # If we've hit any HANDLER-CASE-bound handlers, then we won't
        # even reach this point, as the stack is already unwound.
set_condition_handler(__cl_condition_handler__)

def error(datum, *args):
        "With all said and done, this ought to jump straight above, into __CL_CONDITION_HANDLER__."
        raise (Exception(datum % args) if stringp(datum) else
               datum(*args)            if functionp(datum) else
               datum)

def handler_bind(fn, no_error = identity, **handlers):
        "Works like real HANDLER-BIND, when the conditions are right.  Ha."
        value = None

        # this is:
        #     pytracer_enabled_p() and condition_handler_active_p()
        # ..inlined for speed.
        if pytracer_enabled_p() and __tracer_hooks__.get('exception') is __cl_condition_handler__:
                ### XXX: This is a temporary shitty workaround for broken FIND-CLASS (oh, yes, Python, thank you again!)
                # resolved = dict()
                # for type, handler in handlers.items():
                #         resolved[resolve_exception_type(type)] = handler
                with env.let(__handler_clusters__ = env.__handler_clusters__ + [handlers]):
                        # printf("crap ok, going on, new __handler_clusters__ = %s", env.__handler_clusters__)
                        return no_error(fn())
        else:
                # old world case..
                # printf("crap FAIL: pep %s, exhook is cch: %s",
                #        pytracer_enabled_p(), __tracer_hooks__.get('exception') is __cl_condition_handler__)
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
        ret = catch(nonce,
                     lambda: handler_bind(body, no_error = no_error, **wrapped_handlers))
        return ret
