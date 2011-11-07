import sys
import cl
from cl import *

defpackage("CL-TESTS", use = ["CL", "BUILTINS"])

in_package("CL-TESTS")

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
print("WITH-SIMPLE-RESTARTS: passed")

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
                cl._here("Handling <%s>, tracer: %s", cond, sys.gettrace())
                return_from(f, "All handled.")
        def first_handler(cond, _):
                cl._here("Handling <%s>, tracer: %s, info: %s", cond, sys.gettrace(), sys.exc_info())
                print(dir(cond[2]))
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
