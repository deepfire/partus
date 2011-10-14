import sys
import cl
from cl import *

outer = gensym("outer")
@cl.__block__(outer)
def outer(yoo):
        midder = gensym("midder")
        @cl.__block__(midder)
        def midder():
                (lambda: return_from(midder, "lambda way!"))()
                return "Middah!"
        return midder()
assert(outer("Yeehaw!") == "lambda way!")

outer = gensym("outer")
@cl.__block__(outer)
def outer(yoo):
        midder = gensym("midder")
        @cl.__block__(midder)
        def midder():
                (lambda: return_from(outer, "lambda way!"))()
                return "Middah!"
        return midder()
assert(outer("Yeehaw!") == "lambda way!")

class X(Exception): pass
class X1(X):        pass

def e(mesg, expr): fprint(sys.stderr, "-%s\n" % mesg); return expr

def f():
        def raiser():
                raise X1(e("raising", "Ugh."))
        return e("handled", handler_case(e("calling raiser", identity),
                                          X = lambda: e("handling", "Woot!")))

enable_pytracer()
assert(f() == "Woot!")
