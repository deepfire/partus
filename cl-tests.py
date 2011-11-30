import sys
import cl
from cl import *
from cl import _intern0

defpackage("CL-TESTS", use = ["CL", "BUILTINS"])

in_package("CL-TESTS")

assert(typep(1, integer))
assert(typep(1, (member_, 1)))
assert(typep(0x10000000000000000000000000000000, (eql_, 0x10000000000000000000000000000000)))
assert(typep(1, (and_, integer, (member_, 1))))
assert(every(lambda x: typep(x, (or_, str, (member_, 1))),
             [1, "a"]))
assert(typep([1, 2, 3, "a"], (list_, (or_, int, str))))
assert(not typep("1", integer))
assert(not typep(2, (member_, 1)))
assert(not typep(0x10000000000000000000000000000000, (eql_, 0x10000000000000000000000000000001)))
assert(not typep(2, (and_, integer, (member_, 1))))
assert(not every(lambda x: typep(x, (or_, str, (member_, 1))),
                 [1, "a", 2]))
assert(not typep([1, 2, 3, "a", []], (list_, (or_, int, str))))
assert(    typep((1, 2, 3, "a", []), (partuple_, int, int, int)))
assert(not typep((1, 2, "a", []),    (partuple_, int, int, int)))
assert(not typep((1, 2),             (partuple_, int, int, int)))
assert(    typep((1, 2, 3, "a", []),      (varituple_, int, int, int, (maybe_, str), (maybe_, list))))
assert(not typep((1, 2, 3, "a", [], {}),  (varituple_, int, int, int, (maybe_, str), (maybe_, list))))
assert(not typep((1, 2, {}, "a", [], {}), (varituple_, int, int, int, (maybe_, str), (maybe_, list))))
print("TYPEP: passed")

setq("_scope_", 0)
def outer():
        def inner():
                setq("_scope_", 2)
                return symbol_value("_scope_")
        def midder():
                with env.let(_scope_ = 1):
                        return [ symbol_value("_scope_"), inner() ]
        return [ symbol_value("_scope_") ] + midder() + [ symbol_value("_scope_") ]
assert(outer() == [0, 1, 2, 0])
print("DYNAMIC-SCOPE: passed")

# with_standard_io_syntax(lambda: mapcar(lambda x: symbol_value(x), ["_print_escape_", "_print_pretty_", "_print_readably_"]))
# [T, T, NIL]
def with_alternate_io_syntax(thunk):
    with progv(_print_escape_ = 1, _print_pretty_ = 2, _print_readably_ = 3):
        return thunk()
# with_alternate_io_syntax(lambda: mapcar(lambda x: symbol_value(x), ["_print_escape_", "_print_pretty_", "_print_readably_"]))
# [1, 2, 3]

assert(with_alternate_io_syntax(
         lambda: mapcar(lambda x: with_standard_io_syntax(lambda: symbol_value(x)),
                        ["_print_escape_", "_print_pretty_", "_print_readably_"])) ==
       [t, t, nil])
print("WITH-STANDARD-IO-SYNTAX: passed")

def outer():
        mid, out = _intern0("midder"), _intern0("outer")
        def midder(f):
                catch(mid, f)
                return "midder"
        def inner():
                throw(out, "inner")
        return catch(out, lambda: midder(inner))
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

cl._init_condition_system()

@block
def f():
        def raiser():
                raise X1("HANDLER-BIND: Ugh.")
        return handler_bind(raiser,
                            (X1,
                             lambda cond:
                                     return_from(f, "Woot: " + cl._pp_frame(env._signalling_frame_))))
assert(f() == "Woot: cl-tests.py: raiser()")
print("HANDLER-BIND: passed")

def f():
        def raiser():
                raise X1("HANDLER-CASE: Ugh.")
        return handler_case(raiser,
                            (X1,
                             lambda cond: "X1!"),
                            (X,
                             lambda cond: "X!"))
assert(f() == "X1!")
print("HANDLER-CASE: passed")

def report():
        print(print_frames(frames_upward_from(env._signalling_frame_)))
        return True

@block
def f():
        def raiser():
                raise X1("HANDLER-CASE: Ugh.")
        return handler_case(
                lambda: handler_bind(raiser,
                                     (X1,
                                      lambda cond:
                                              return_from(f, "Woot: " + cl._pp_frame(env._signalling_frame_)))),
                (X1,
                 lambda cond: "Surrounding HANDLER-CASE won!"))
assert(f() == "Woot: cl-tests.py: raiser()")
print("HANDLER-CASE-AROUND-HANDLER-BIND: passed")

@block
def f():
        def raiser():
                raise X1("PREHANDLER-HOOK: Ugh.")
        with progv(_prehandler_hook_ = lambda cond, frame, _:
                           return_from(f, str([cond, frame]))):
                return handler_case(
                        lambda: handler_bind(raiser,
                                             (X1,
                                              lambda cond:
                                                      return_from(f, "Woot: " + cl._pp_frame(env._signalling_frame_)))),
                        (X1,
                         lambda cond: "HANDLER-CASE won!"))
assert("[X1('PREHANDLER-HOOK: Ugh.',), <frame object at" in f())
print("PREHANDLER-HOOK: passed")

@block
def f():
        return format(nil, "%s",
                      with_simple_restart(
                        "fooage", ("Yay: %s!", 3.14),
                        lambda: with_simple_restart(
                                "brooage", ("Yay2: %s!", 2.71),
                                lambda: str(mapcar(lambda x: x.name, compute_restarts())))))
assert(f() == "['BROOAGE', 'FOOAGE']")
print("WITH-SIMPLE-RESTARTS/COMPUTE-RESTARTS: passed")

@block
def f():
        def raiser():
                raise X1("HANDLER-BIND: Ugh.")
        def with_restarter(body):
                return restart_bind(body,
                                    RETURN = lambda *args: return_from(f, "Winnage: %s, %s!" % args))
        def finder_invoker(body):
                return handler_bind(body,
                                    (X,
                                     lambda _: invoke_restart("RETURN", 1, 2)))
        return with_restarter(lambda:
                                      finder_invoker(raiser))
assert(f() == 'Winnage: 1, 2!')
print("CONDITIONS-RESTARTS: passed")

assert(probe_file("/etc/passwd") and
       not probe_file("/does/not/exist"))
print("PROBE-FILE: passed")


original_tracer = sys.gettrace()

handler = None
def test_tracer(frame, event, arg):
        if event != "line":
                # cl._here("<%s> %s", event, arg)
                if event == "exception" and handler:
                        handler(arg, frame)
        return test_tracer
sys.settrace(test_tracer)

def pause():
        import sys
        print("Paused!")
        sys.stdin.readline()

@block
def f():
        global handler
        def second_handler(cond, _):
                # cl._here("Handling <%s>, tracer: %s", cond, sys.gettrace())
                return_from(f, "All handled.")
        def first_handler(cond, _):
                # cl._here("Handling <%s>, tracer: %s, info: %s", cond, sys.gettrace(), sys.exc_info())
                def continuation():
                        global handler
                        # cl._dump_thread_state()
                        # pause()
                        handler = second_handler
                        raise error_("Nested condition!")
                # cl._dump_thread_state()
                sys.call_tracing(continuation, tuple())
        handler = first_handler
        exec("{1:2}[3]")
assert(f() == "All handled.")
sys.settrace(original_tracer)
print("NESTED-PYTRACER: passed")

@block
def f():
        def second_handler(cond, _):
                return_from(f, "All handled.")
        def first_handler(cond, _):
                with progv(_debugger_hook_ = second_handler):
                        # write_line(">>> condsys %s, de-ho %s" % 
                        #            (cl._condition_system_enabled_p(),
                        #             cl._print_function(symbol_value("_debugger_hook_"))))
                        # cl._dump_thread_state()
                        error("Nested condition!")
        with progv(_debugger_hook_ = first_handler):
                # write_line(">>> condsys %s, de-ho %s" % 
                #            (cl._condition_system_enabled_p(),
                #             cl._print_function(symbol_value("_debugger_hook_"))))
                # cl._dump_thread_state()
                error("Initial condition!")
assert(f() == "All handled.")
print("NESTED-DEBUGGER-HOOK: passed")

data  = [0, [1], "two"]
printed = mapcar(princ_to_string, data)

readed = mapcar(read_from_string, printed)
assert(data == readed)
print("READ-FROM-STRING: passed")

assert(with_input_from_string(" ".join(printed), lambda s: mapcar(read, [s] * len(data))) ==
       readed)
print("READ: passed")

assert(with_input_from_string("", lambda s: read(s, nil, nil)) ==
       nil)
print("READ-EOF-ERROR-P-NIL: passed")

# Issue CONDITIONS-AT-TOP-LEVEL-CANNOT-BE-HANDLED
# def this_is_handled():
#         ""[1]
# this_is_handled()
# this_is_not_handled()

# Issue DOUBLE-UNHANDLED-FAULT-REPRO
## double fault repro
# def err_twice(f):
#         ""[1]
#         ""[2]
# err_twice(1)
# @err_twice
# def foo():
#         pass

@defgeneric
def gfun(x):
        pass

@defmethod
def gfun(x):
        return "generic: %s" % x

@defmethod
def gfun(x: int):
        return "int: %s" % x

@defmethod
def gfun(x: str):
        return "str: %s" % x

@defmethod
def gfun(x: X):
        return "X: %s" % x

@defmethod
def gfun(x: X1):
        return "X1: %s, %s" % (x, call_next_method())

print("gfun(%s): %s", 1, gfun(1))
print("gfun(%s): %s", "one", gfun("one"))
print("gfun(%s): %s", X(), gfun(X()))
print("gfun(%s): %s", X1(), gfun(X1()))
print("gfun(%s): %s", [], gfun([]))
# print("DEFGENERIC: passed")
