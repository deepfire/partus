import sys
import cl
from cl import *

def outer():
        def midder(f):
                catch("midder", f)
                return "midder"
        def inner():
                throw("outer", "inner")
        return catch("outer", lambda: midder(inner))
assert(outer() == "inner")
print("CATCH/THROW: passed")

@block
def outer(yoo):
        @block
        def midder():
                (lambda: return_from(midder, "lambda way!"))()
                return "Middah!"
        return midder()
assert(outer("Yeehaw!") == "lambda way!")
print("RETURN-FROM-MIDDLE: passed")

@block
def outer(yoo):
        @block
        def midder():
                (lambda: return_from(outer, "lambda way!"))()
                return "Middah!"
        return midder()
assert(outer("Yeehaw!") == "lambda way!")
print("RETURN-FROM-OUTER: passed")

class X(Exception): pass
class X1(X):        pass
class X2(X):        pass

def e(mesg, expr): fprintf(sys.stderr, "-%s: %s\n" % (mesg, expr)); return expr

enable_pytracer()

def f():
        def raiser():
                raise X1("HANDLER-CASE: Ugh.")
        return handler_case(raiser,
                            X1 = lambda cond: "Woot!",
                            X2 = lambda cond: "Moot!")
assert(f() == "Woot!")
print("HANDLER-CASE: passed")

def report():
        print(print_frames(frames_upward_from(env._signalling_frame_)))
        return True

@block
def f():
        def raiser():
                raise X1("HANDLER-BIND: Ugh.")
        return handler_bind(raiser,
                            X1 = lambda cond:
                            return_from(f, "Woot: " + pp_frame(env._signalling_frame_)))
assert(f() == "Woot: cl-tests.py: raiser()")
print("HANDLER-BIND: passed")

@block
def f():
        def raiser():
                raise X1("HANDLER-BIND: Ugh.")
        return handler_case(
                lambda: handler_bind(raiser,
                                     X1 = lambda cond:
                                             return_from(f, "Woot: " + pp_frame(env._signalling_frame_))),
                X1 = lambda cond: "Surrounding HANDLER-CASE won!")
assert(f() == "Woot: cl-tests.py: raiser()")
print("HANDLER-CASE-AROUND-HANDLER-BIND: passed")
