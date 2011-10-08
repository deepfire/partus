def not_implemented(mesg):
        raise Exception("ERROR: not implemented: " + mesg)
def with_restarts(fn, **restarts):
        # not_implemented("with_restarts()")
        return fn()
def eval_in_frame(expr, env):
        not_implemented("eval_in_frame()")
        return eval(expr)
def compute_restarts(condition):
        not_implemented("compute_restarts()")
        return []
def invoke_restart(restart):
        not_implemented("invoke_restart()")
def restart_description(restart):
        not_implemented("restart_description()")
        return ""
def with_calling_handlers(fn, error = lambda c: None):
        "HANDLER-BIND"
        # not_implemented("with_calling_handlers()")
        return fn()
def with_visible(fn):
        not_implemented("with_visible()")
        return fn()
def sys_calls():
        not_implemented("sys_calls()")
def sys_frames():
        not_implemented("sys_frames()")
import sys
def with_output_redirection(fn, file = None):
        old_stdout = sys.stdout
        try:
                sys.stdout = file
                ret = fn()
        finally:
                sys.stdout = old_stdout
        return ret
def parse(str):
        not_implemented("parse()")
        return None
def deparse(expr):
        not_implemented("deparse()")
        return None
def ls(env = None):
        not_implemented("ls()")
        return []
def new_env():
        not_implemented("new_env()")
def env_get(name, env = None):
        not_implemented("env_get()")
        return None
def apropos(string):
        not_implemented("apropos()")
        return []
def substitute(expr):
        not_implemented("substitute()")
        return expr
def load_file(filename):
        not_implemented("load_file()")
        return True
#
#
#
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

def functionp(o):         return type(o) is type(functionp)
def integerp(o):          return type(o) is int
def listp(o):             return type(o) is list
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
def first(xs):    return xs[0]   # don't confuse with car/cdr
def rest(xs):     return xs[1:]  # !!!

def subseq(xs, start, end = None):
        return xs[start:end] if end else  xs[start:]

def make_list(size, initial_element = None):
        # horribly inefficient, but that's what we have..
        return mapcar(constantly(initial_element), range(size))

def append(x, y): return x + y

def mapcar(f, xs):
        return [ f(x) for x in xs ]

def mapcan(f, xs):
        return reduce(append, [ f(x) for x in xs ]) if xs else []

def mapc(f, xs):
        for x in xs:
                f(x)
        return xs

def remove_if(f, xs):
        if listp(xs):              return [ x for x in xs if not f(x) ]
        elif typep(xs, set):       return set (x for x in xs if not f(x))
        elif typep(xs, frozenset): return frozenset(x for x in xs if not f(x))
        elif typep(xs, dict):
                acc = dict()
                for x in xs:
                        if not f(x):
                                acc[x] = xs[x]
                return acc
        else:                      return [ x for x in xs if not f(x) ]

def remove_if_not(f, xs):
        if listp(xs):              return [ x for x in xs if f(x) ]
        elif typep(xs, set):       return set(x for x in xs if f(x))
        elif typep(xs, frozenset): return frozenset(x for x in xs if f(x))
        elif typep(xs, dict):
                acc = dict()
                for x in xs:
                        if f(x):
                                acc[x] = xs[x]
                return acc
        else:                      return [ x for x in xs if f(x) ]

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
#
#
#
import time, io

def clocking(fn):
        start = time.time()
        result = fn()
        return (result, int((time.time() - start) * 1000))

def make_string_output_stream():
        return io.StringIO()

def get_output_stream_string(x):
        return x.getvalue()

def close(x):
        x.close()

def with_output_to_string(f):
        x = make_string_output_stream()
        f(x)
        ret = output_stream_string(x)
        close(x)
        return ret

def printf(format_control, *format_args):
        print(format_control % format_args)

def fprintf(stream, format_control, *format_args):
        print(format_control % format_args, file = stream, end = '')

def astp(x):              return typep(x, ast.AST)
#
#
#
import ast

# predicates
def ast_string_p(x):            return typep(x, ast.Str)
def ast_num_p(x):               return typep(x, ast.Num)
def ast_name_p(x):              return typep(x, ast.Name)
def ast_string_equalp(x, s):    return ast_string_p(x) and x.s == s
def ast_assign_p(x, to):        return typep(x, ast.Assign) and to in x.targets
def ast_module_p(x):            return typep(x, ast.Module)
def ast_def_p(x):               return typep(x, ast.FunctionDef)
def ast_import_p(x):            return typep(x, ast.Import)
def ast_import_from_p(x):       return typep(x, ast.ImportFrom)
def ast_import_maybe_from_p(x): return ast_import_p(x) or ast_import_from_p(x)
def ast_expr_p(x):              return typep(x, ast.Expr)
def ast_call_p(x):              return typep(x, ast.Call)
def ast_attribute_p(x):         return typep(x, ast.Attribute)
def ast_keyword_p(x):           return typep(x, ast.keyword)

# top-levels
def ast_module(body):
        assert(listp(body) and all(mapcar(astp, body)))
        return ast.Module(body = body, lineno = 0)

def ast_def(name, *body):
        filtered_body = remove_if(null, body)
        assert(stringp(name) and all(mapcar(astp, filtered_body)))
        return ast.FunctionDef(name = name, decorator_list = [],
                               args = ast.arguments(args = [], defaults = [], kwonlyargs = [], kw_defaults = [],
                                                    vararg = None, varargannotation = None, kwarg = None, kwargannotation = None),
                               body = filtered_body, returns = None)

# expressions
def ast_bytes(bs):
        if not bytesp(bs):
                mesg("ast_bytes(), not an immutable byte vector: %s", str(bs))
        assert(bytesp(bs))
        return ast.Bytes(s = bs)

def ast_string(s):
        if not stringp(s):
                mesg("ast_string(), not a string: %s", str(s))
        assert(stringp(s))
        return ast.Str(s = s)

def ast_num(n):
        assert(integerp(n))
        return ast.Num(n = n)

def ast_rw(writep):
        return ast.Store() if writep else ast.Load()

def ast_name(name, writep = False):
        assert(stringp(name))
        return ast.Name(id = name, ctx = ast_rw(writep))

def ast_alias(name):
        assert(stringp(name))
        return ast.alias(name = name, asname = None)

def ast_list(xs):
        assert(listp(xs) and all(mapcar(astp, xs)))
        return ast.List(elts = xs, ctx = ast.Load())

def ast_tuple(xs, writep = False):
        assert(listp(xs) and all(mapcar(astp, xs)))
        return ast.Tuple(elts = xs, ctx = ast_rw(writep))

def ast_dict(keys, values):
        return ast.Dict(keys = keys, values = values)

def ast_attribute(x, name, writep = False):
        return ast.Attribute(attr = name, value = x, ctx = ast_rw(writep))

def ast_index(of, index, writep = False):
        return ast.Subscript(value = of,
                             slice = ast.Index(value = index),
                             ctx = ast.Store() if writep else ast.Load())

def ast_maybe_normalise_string(x):
        return ast_string(x) if stringp(x) else x

def ast_funcall(name, *args):
        if not (all(mapcar(lambda x: stringp(x) or astp(x) or x is None, args))):
                error("In call to %s: improper arglist %s", name, str(args))
        return ast.Call(func = ast_name(name) if stringp(name) else name,
                        args = mapcar(ast_maybe_normalise_string, args),
                        keywords = [], starargs = None, kwargs = None)

def ast_func_name(x):
        if typep(x, ast.Name):
                return x.id
        elif typep(x, ast.Subscript):
                return ast_func_name(x.value) + "[" + ast_func_name(x.slice.value) + "]"
        elif typep(x, ast.Attribute):
                return ast_func_name(x.value) + "." + x.attr
        else:
                return "<unhandled>"

# statements
def astlist_prog(*body):
        "WARNING: not an actual node, returns a list!"
        return remove_if(null, body) or [ ast.Pass() ]

def ast_return(node):
        assert(astp(node))
        return ast.Return(value = node)

def ast_expr(node):
        assert(astp(node))
        return ast.Expr(value = node)

def ast_expression(node):
        assert(astp(node))
        return ast.Expression(body = node)

def ast_import(*names):
        assert(all(mapcar(stringp, names)))
        return ast.Import(names = mapcar(ast_alias, names))

def ast_import_all_from(name):
        assert(stringp(name))
        return ast.ImportFrom(module = name, names = [ ast.alias(name = '*', asname = None) ], level = 0)

def ast_import_from(module_name, names):
        assert(stringp(module_name))
        assert(listp(names) and all(mapcar(stringp, names)))
        return ast.ImportFrom(module = module_name, names = mapcar(ast_alias, names), level = 0)

def ast_assign(to, value):
        assert(listp(to) and all(mapcar(astp, to)) and astp(value))
        return ast.Assign(value = value, targets = to)

def ast_assign_var(name, value):
        assert(stringp(name) and (integerp(value) or stringp(value) or astp(value)))
        return ast.Assign(value = value, targets = [ ast_name(name, True) ])

def ast_append_var(name, value):
        assert(stringp(name) and (stringp(value) or astp(value)))
        return ast.AugAssign(value = value, target = ast_name(name, True), op = ast.Add())

def ast_when(test, *body):
        return ast.If(test = test, body = remove_if(null, body), orelse = [])

def ast_unless(test, *body):
        return ast.If(test = test, body = [], orelse = remove_if(null, body))

def ast_try_except(body, except_handlers, *else_body):
        return ast.TryExcept(body = remove_if(null, body),
                             handlers = [ ast.ExceptHandler(name = xname,
                                                            type = ast_name(xtype),
                                                            body = remove_if(null, xhandler_body))
                                          for (xtype, xname, xhandler_body) in except_handlers ],
                             orelse = remove_if(null, else_body))

def ast_print(*strings):
        return ast_expr(ast_funcall('print', *strings))

def pp_ast(o):
        "Pretty-print AST O."
        def do_pp_ast_rec(x, name, pspec):
                lstr = [""]
                def lmesg(msg):
                        lstr[0] += msg
                        if msg[-1] == "\n"[0]:
                                print(lstr[0][:-1])
                                lstr[0] = ""
                def pp_prefix(spec):
                        for i in spec:
                                lmesg(" |  " if i else "    ")
                pp_prefix(pspec)
                if name:
                        lmesg("<" + name + ">: ")
                if x is None:
                        lmesg("<None>\n")
                elif stringp(x):
                        lmesg("'" + x + "'\n")
                elif bytesp(x) or integerp(x):
                        lmesg(str(x) + "\n")
                elif sequencep(x) and emptyp(x):
                        lmesg("[]\n")
                else:
                        child_slot_names = type(x)._fields
                        child_slots = [ (k, getattr(x, k)) for k in child_slot_names ]
                        lmesg(type(x).__name__ + "  ")
                        for (k, v) in child_slots:
                                if stringp(v):
                                        lmesg("<" + k + ">: '" + v + "', ")
                        lmesg("\n")
                        child_list_slots = list(reversed(sort([ (k, v) for (k, v) in child_slots if listp(v) ], key = car)))
                        child_list_slots_nr = len(child_list_slots)
                        for (k, v) in child_slots:
                                if not listp(v) and not stringp(v):
                                        do_pp_ast_rec(v, k, pspec + ([True] if child_list_slots_nr > 0 else [False]))
                        for ((k, v), i) in zip(child_list_slots, range(0, child_list_slots_nr)):
                                pp_prefix(pspec)
                                lmesg(" ^[" + k + "]\n")
                                subprefix = pspec + ([True] if i < (child_list_slots_nr - 1) else [False])
                                for sub in v:
                                        do_pp_ast_rec(sub, '', subprefix)
        do_pp_ast_rec(o, '', [])
        return o

#
#
#
class symbol():
        name = None
        def __str__(self):  return self.name
        def __repr__(self): return self.name
        def __init__(self, name):
                self.name = name.upper()
def symbolp(x):     return type(x) is symbol
def symbol_name(x): return x.name.lower()
syms = dict()
def intern(x):
        X = x.upper()
        if X in syms:
                return syms[X]
        else:
                syms[X] = symbol(X)
                return syms[X]
t   = intern("t")
nil = intern("nil")
#
#
#
import traceback
def print_backtrace():
        exc_type, exc_value, _ = sys.exc_info()
        printf("%s: %s", str(exc_type), str(exc_value))
        for line in traceback.format_exc().splitlines():
                printf("%s", line)

def warning(str, *args):
        printf(str, *args)

def case(val, *clauses):
        for (cval, result) in clauses:
                if val == cval or (cval is True):
                        return result

def typecase(val, *clauses):
        for (ctype, result) in clauses:
                if (ctype is True) or typep(val, ctype):
                        return result

def subtypep(sub, super):
        return issubclass(sub, super)

def typep(x, super):
        return isinstance(x, super)

def functionp(o):         return type(o) is type(functionp)
def stringp(o):           return type(o) is str
def integerp(o):          return type(o) is int
def floatp(o):            return type(o) is float
def listp(o):             return type(o) is list
def nonep(o):             return o is None
def minus1p(o):           return o is -1
def bytesp(o):            return type(o) is bytes
def dictp(o):             return type(o) is dict
def frozensetp(o):        return type(o) is frozenset
def setp(o):              return type(o) is set or frozensetp(o)
def tuplep(o):            return type(o) is tuple
def sequencep(x):         return getattr(type(x), '__len__', None) is not None
## conditions
def error(datum, *args):
        raise Exception(datum % args) if stringp(datum) else datum(*args)
#
#
#
import os, sys, socket, select, re

partus_path = os.getcwd()                                           # swankrPath <- getwd() 

# swank <- function(port=4005) {
#  acceptConnections(port, FALSE)
# }
def swank(port = 4005):      accept_connections(port, False)
# startSwank <- function(portFile) {
#  acceptConnections(4005, portFile)
# }
def start_swank(port_file):  accept_connections(4005, port_file)

# acceptConnections <- function(port, portFile) {
#  if(portFile != FALSE) {
#    f <- file(portFile, open="w+")
#    cat(port, file=f)
#    close(f)
#  }
#  s <- socketConnection(host="localhost", server=TRUE, port=port, open="r+b")
#  on.exit(close(s))
#  serve(s)
# }
def accept_connections(port, port_file):
        global s
        if port_file:
                with open(port_file, "rw") as f:
                        print(port, file = f)
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        s.bind(('', port))
        s.listen(0)
        printf("waiting for clients..")
        c, a = s.accept()
        printf("serving connection from %s", a)
        serve(c, c.makefile(mode = "rw"))

# serve <- function(io) {
#  mainLoop(io)
# }
def serve(sock, file):
        printf("serve: sock = %s, file = %s", sock, file)
        main_loop(sock, file)

# mainLoop <- function(io) {
#  slimeConnection <- new.env()
#  slimeConnection$io <- io
#  while(TRUE) {
#    withRestarts(tryCatch(dispatch(slimeConnection, readPacket(io)),
#                          swankTopLevel=function(c) NULL),
#                 abort="return to SLIME's toplevel")
#  }
# }
class servile():
        def __init__(self, **keys):
                self.__dict__.update(keys)

class SlimeConnection(servile): pass

def main_loop(sock, file):
        global slime_connection
        slime_connection = SlimeConnection(sock = sock, file = file, io = sock)
        while True:
                def with_restarts_body():
                        try:
                                dispatch(slime_connection, read_packet(sock, file))
                        except Exception as x: # FIXME
                                # print_backtrace()
                                not_implemented("Unhandled exception at main loop.")
                                swank_top_level = lambda c: None
                with_restarts(with_restarts_body,
                              abort = "return to SLIME's toplevel")

# dispatch <- function(slimeConnection, event, sldbState=NULL) {
#  kind <- event[[1]]
#  if(kind == quote(`:emacs-rex`)) {
#    do.call("emacsRex", c(list(slimeConnection), list(sldbState), event[-1]))
#  }
# }
def dispatch(slime_connection, event, sldb_state = None):
        kind = event[0]
        if kind is intern(':emacs-rex'):
                emacs_rex(*([slime_connection, sldb_state] + event[1:]))

# sendToEmacs <- function(slimeConnection, obj) {
#  io <- slimeConnection$io
#  payload <- writeSexpToString(obj)
#  writeChar(sprintf("%06x", nchar(payload)), io, eos=NULL)
#  writeChar(payload, io, eos=NULL)
#  flush(io)
# }
def send_to_emacs(slime_connection, obj):
        file = slime_connection.file
        payload = write_sexp_to_string(obj)
        print("%06x" % len(payload), file = file, end = '')
        print(payload,               file = file, end = '')
        printf("-> %06x %s", len(payload), payload)
        file.flush()

# callify <- function(form) {
#  ## we implement here the conversion from Lisp S-expression (or list)
#  ## expressions of code into our own, swankr, calling convention,
#  ## with slimeConnection and sldbState as first and second arguments.
#  ## as.call() gets us part of the way, but we need to walk the list
#  ## recursively to mimic CL:EVAL; we need to avoid converting R
#  ## special operators which we are punning (only `quote`, for now)
#  ## into this calling convention.
#  if(is.list(form)) {
#    if(form[[1]] == quote(quote)) {
#      as.call(form)
#    } else {
#      as.call(c(list(form[[1]], quote(slimeConnection), quote(sldbState)), lapply(form[-1], callify)))
#    }
#  } else {
#    form
#  }
# }
def pythonise_lisp_name(x):
        ret = re.sub("[:\\-]", "_", x).lower()
        printf("pythonising Lisp name: %s -> %s", x, ret)
        return ret

def constantp(x):
        return type(x) in set([str, int, float])
constant_xform = {
        False : ast_name("False"),
        None  : ast_name("None"),
        True  : ast_name("True"),
        str   : ast_string,
        int   : ast_num,
        }
def callify(form, quoted = False):
        printf("CALLIFY %s", form)
        if listp(form):
                if quoted or (form[0] is intern('quote')):
                        return (ast_list(mapcar(lambda x: callify(x, quoted = True), form[1]))
                                if listp(form[1]) else
                                callify(form[1], quoted = True))
                else:
                        return ast_funcall(pythonise_lisp_name(symbol_name(form[0])),
                                           ast_name("slime_connection"), ast_name("sldb_state"),
                                           *map(callify, form[1:]))
        elif symbolp(form):
                return (ast_funcall("intern", symbol_name(form))
                        if quoted else
                        ast_name(symbol_name(form)))
        elif constantp(form):
                return constant_xform[type(form)](form)
        elif form in constant_xform:
                return constant_xform[form]
        else:
                error("Unable to convert form %s", form)

# emacsRex <- function(slimeConnection, sldbState, form, pkg, thread, id, level=0) {
#  ok <- FALSE
#  value <- NULL
#  conn <- textConnection(NULL, open="w")
#  condition <- NULL
#  tryCatch({
#    withCallingHandlers({
#      call <- callify(form)
#      capture.output(value <- eval(call), file=conn)
#      string <- paste(textConnectionValue(conn), sep="", collapse="\n")
#      if(nchar(string) > 0) {
#        sendToEmacs(slimeConnection, list(quote(`:write-string`), string))
#        sendToEmacs(slimeConnection, list(quote(`:write-string`), "\n"))
#      }
#      close(conn)
#      ok <- TRUE
#    }, error=function(c) {
#      condition <<- c
#      string <- paste(textConnectionValue(conn), sep="", collapse="\n")
#      if(nchar(string) > 0) {
#        sendToEmacs(slimeConnection, list(quote(`:write-string`), string))
#        sendToEmacs(slimeConnection, list(quote(`:write-string`), "\n"))
#      }
#      close(conn)
#      newSldbState <- makeSldbState(c, if(is.null(sldbState)) 0 else sldbState$level+1, id)
#      withRestarts(sldbLoop(slimeConnection, newSldbState, id), abort=paste("return to sldb level", newSldbState$level)) })},
#    finally=sendToEmacs(slimeConnection, list(quote(`:return`), if(ok) list(quote(`:ok`), value) else list(quote(`:abort`), as.character(condition)), id)))
# }

sldb_state = None
___expr___ = None
def swank_set_value(value):
        global ___expr___
        ___expr___ = value

def emacs_rex(slime_connection, sldb_state, form, pkg, thread, id, level = 0):
        ok = False
        value = intern('nil')
        condition = intern('nil')
        output = make_string_output_stream()
        try:
                def writeurn_output():
                        string = get_output_stream_string(output)
                        close(output)
                        if len(string):
                                send_to_emacs(slime_connection, [intern(':write-string'), string])
                                send_to_emacs(slime_connection, [intern(':write-string'), "\n"])
                        return string
                def give_up(cond, mesg, *args):
                        nonlocal condition
                        writeurn_output()
                        condition = cond
                        fprintf(sys.stderr, "ERROR: " + mesg, *args)
                        raise cond
                def with_calling_handlers_body():
                        nonlocal ok, value
                        try:
                                call = ast.fix_missing_locations(ast_module(
                                                [# ast_import_from("partus", ["*"]),
                                                 ast_assign_var("foo", ast_funcall("swank_set_value", callify(form))),
                                                 ]))
                        except Exception as cond:
                                give_up(cond, "failed to callify: %s", cond)
                        printf("compiling %s", call)
                        pp_ast(call)
                        try:
                                code = compile(call, '', 'exec')
                        except Exception as cond:
                                give_up(cond, "failed to compile: %s", cond)
                        printf("executing..")
                        with_output_redirection(lambda: exec(code), file = output)
                        value = ___expr___
                        string = writeurn_output()
                        printf("return value: %s", ___expr___)
                        printf("output:\n%s\n===== EOF =====\n", string)
                        ok = True
                def error_handler(c):
                        global condition
                        condition = c
                        string = writeurn_output()
                        new_sldb_state = make_sldb_state(c, 0 if not sldb_state else sldb_state.level + 1, id)
                        def with_restarts_body():
                                return sldb_loop(slime_connection, new_sldb_state, id)
                        with_restarts(with_restarts_body,
                                      abort = "return to sldb level %s" % str(new_sldb_state.level))
                with_calling_handlers(with_calling_handlers_body,
                                      error = error_handler)
        finally:
                send_to_emacs(slime_connection, [intern(':return'),
                                                 ([intern(':ok'), value]
                                                  if ok else
                                                  [intern(':abort'), condition]),
                                                 id])

# makeSldbState <- function(condition, level, id) {
#   calls <- rev(sys.calls())[-1]
#   frames <- rev(sys.frames())[-1]
#   restarts <- rev(computeRestarts(condition))[-1]
#   ret <- list(condition=condition, level=level, id=id, restarts=restarts, calls=calls, frames=frames)
#   class(ret) <- c("sldbState", class(ret))
#   ret
# }
class SldbState(servile): pass

def make_sldb_state(condition, level, id):
        return SldbState(calls = reversed(sys_calls()[1:]),
                         frames = reversed(sys_frames()[1:]),
                         restarts = reversed(compute_restarts(condition)[1:]),
                         condition = condition,
                         level = level,
                         id = id)
        
# sldbLoop <- function(slimeConnection, sldbState, id) {
#   tryCatch({
#     io <- slimeConnection$io
#     sendToEmacs(slimeConnection, c(list(quote(`:debug`), id, sldbState$level), `swank:debugger-info-for-emacs`(slimeConnection, sldbState)))
#     sendToEmacs(slimeConnection, list(quote(`:debug-activate`), id, sldbState$level, FALSE))
#     while(TRUE) {
#       dispatch(slimeConnection, readPacket(io), sldbState)
#     }
#   }, finally=sendToEmacs(slimeConnection, c(list(quote(`:debug-return`), id, sldbState$level, FALSE))))
# }
def sldb_loop(slime_connection, sldb_state, id):
        try:
                io = slime_connection.io
                send_to_emacs(slime_connection, [intern(':debug'), id, sldb_state.level] + swank_debugger_info_for_emacs(slime_connection, sldb_state))
                send_to_emacs(slime_connection, [intern(':debug-activate'), id, sldb_state.level, False])
                while True:
                        dispatch(slime_connection, read_packet(slime_connection.sock, slime_connection.file), sldb_state)
        finally:
                send_to_emacs(slime_connection, [intern(':debug-return'), id, sldb_state.level, False])

# readPacket <- function(io) {
#   socketSelect(list(io))
#   header <- readChunk(io, 6)
#   len <- strtoi(header, base=16)
#   payload <- readChunk(io, len)
#   readSexpFromString(payload)
# }
def read_packet(sock, file):
        select.select([sock.fileno()], [], [])
        header = read_chunk(file, 6)
        len = int(header, 16)
        payload = read_chunk(file, len)
        printf("<- %s %s", header, payload)
        return read_sexp_from_string(payload)
                
# readChunk <- function(io, len) {
#   buffer <- readChar(io, len)
#   if(nchar(buffer) != len) {
#     stop("short read in readChunk")
#   }
#   buffer
# }
def read_chunk(file, len_):
        buffer = file.read(len_)
        if len(buffer) != len_:
                raise Exception("short read in read_chunk")
        return buffer

# readSexpFromString <- function(string) {
#   pos <- 1
def read_sexp_from_string(string):
        pos = 0
#   read <- function() {
#     skipWhitespace()
#     char <- substr(string, pos, pos)
#     switch(char,
#            "("=readList(),
#            "\""=readString(),
#            "'"=readQuote(),
#            {
#              if(pos > nchar(string))
#                stop("EOF during read")
#              obj <- readNumberOrSymbol()
#              if(obj == quote(`.`)) {
#                stop("Consing dot not implemented")
#              }
#              obj
#            })
#   }
        def read():
                skip_whitespace()
                char = string[pos]
                # printf("read(#\\%s :: '%s')", char, string[pos + 1:])
                if   char == "(":  obj = read_list()
                elif char == "\"": obj = read_string()
                elif char == "'":  obj = read_quote()
                else:
                        if pos > len(string):
                                error("EOF during read")
                        obj = read_number_or_symbol()
                        if obj == ".":
                                error("Consing dot not implemented")
                # printf("read(): returning %s", obj)
                return obj
#   skipWhitespace <- function() {
#     while(substr(string, pos, pos) %in% c(" ", "\t", "\n")) {
#       pos <<- pos + 1
#     }
#   }
        def skip_whitespace():
                nonlocal pos
                while string[pos] in frozenset([" ", "\t", "\n"]):
                        pos += 1
#   readList <- function() {
#     ret <- list()
#     pos <<- pos + 1
#     while(TRUE) {
#       skipWhitespace()
#       char <- substr(string, pos, pos)
#       if(char == ")") {
#         pos <<- pos + 1
#         break
#       } else {
#         obj <- read()
#         if(length(obj) == 1 && obj == quote(`.`)) {
#           stop("Consing dot not implemented")
#         }
#         ret <- c(ret, list(obj))
#       }
#     }
#     ret
#   }
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
                                if not listp(obj) and obj is intern("."):
                                        error("Consing dot not implemented")
                                ret += [obj]
                # printf("read_list(): returning %s", ret)
                return ret
#   readString <- function() {
#     ret <- ""
#     addChar <- function(c) { ret <<- paste(ret, c, sep="") }
#     while(TRUE) {
#       pos <<- pos + 1
#       char <- substr(string, pos, pos)
#       switch(char,
#              "\""={ pos <<- pos + 1; break },
#              "\\"={ pos <<- pos + 1
#                     char2 <- substr(string, pos, pos)
#                     switch(char2,
#                            "\""=addChar(char2),
#                            "\\"=addChar(char2),
#                            stop("Unrecognized escape character")) },
#              addChar(char))
#     }
#     ret
#   }
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
                                        error("Unrecognized escape character")
                        else:
                                add_char(char)
                # printf("read_string(): returning %s", ret)
                return ret
#   readNumberOrSymbol <- function() {
#     token <- readToken()
#     if(nchar(token)==0) {
#       stop("End of file reading token")
#     } else if(grepl("^[0-9]+$", token)) {
#       strtoi(token)
#     } else if(grepl("^[0-9]+\\.[0-9]+$", token)) {
#       as.double(token)
#     } else {
#       name <- as.name(token)
#       if(name == quote(t)) {
#         TRUE
#       } else if(name == quote(nil)) {
#         FALSE
#       } else {
#         name
#       }
#     }
#   }
        def read_number_or_symbol():
                token = read_token()
                if not token:
                        error("End of file reading token")
                elif re.match("^[0-9]+$", token):
                        ret = int(token)
                elif re.match("^[0-9]+\\.[0-9]+$", token):
                        ret = float(token)
                else:
                        name = intern(token)
                        if name is t:
                                ret = True
                        elif name is nil:
                                ret = False
                        else:
                                ret = name
                # printf("read_number_or_symbol(): returning %s", ret)
                return ret
#   readToken <- function() {
#     token <- ""
#     while(TRUE) {
#       char <- substr(string, pos, pos)
#       if(char == "") {
#         break;
#       } else if(char %in% c(" ", "\n", "\t", "(", ")", "\"", "'")) {
#         break;
#       } else {
#         token <- paste(token, char, sep="")
#         pos <<- pos + 1
#       }
#     }
#     token
#   }
        def read_token():
                nonlocal pos
                token = ""
                while True:
                        char = string[pos]
                        if char == "":
                                break
                        elif char in set([" ", "\t", "\n", "(", ")", "\"", "'"]):
                                break
                        else:
                                token += char
                                pos += 1
                # printf("read_token(): returning %s", token)
                return token
        ret = read()
        printf("got remote SEXP: %s", ret)
        return ret
#   read()
# }

# writeSexpToString <- function(obj) {
#   writeSexpToStringLoop <- function(obj) {
#     switch(typeof(obj),
#            "character"={ string <- paste(string, "\"", gsub("([\"\\])", "\\\\\\1", obj), "\"", sep="") },
#            "list"={ string <- paste(string, "(", sep="")
#                     max <- length(obj)
#                     if(max > 0) {
#                       for(i in 1:max) {
#                         string <- paste(string, writeSexpToString(obj[[i]]), sep="")
#                         if(i != max) {
#                           string <- paste(string, " ", sep="")
#                         }
#                       }
#                     }
#                     string <- paste(string, ")", sep="") },
#            "symbol"={ string <- paste(string, as.character(obj), sep="") },
#            "logical"={ string <- if(obj) { paste(string, "t", sep="") } else { paste(string, "nil", sep="") }},
#            "double"={ string <- paste(string, as.character(obj), sep="") },
#            "integer"={ string <- paste(string, as.character(obj), sep="") },
#            stop(paste("can't write object ", obj, sep="")))
#     string
#   }
#   string <- ""
#   writeSexpToStringLoop(obj)
# }
def write_sexp_to_string(obj):
        printf("write_sexp_to_string: %s", obj)
        string = ""
        def write_sexp_to_string_loop(obj):
                nonlocal string
                if listp(obj):
                        string += '('
                        max = len(obj)
                        if max:
                                for i in range(0, max):
                                        string += write_sexp_to_string(obj[i])
                                        if i != (max - 1):
                                                string += " "
                        string += ')'
                elif symbolp(obj):
                        string += symbol_name(obj)
                elif stringp(obj):
                        string += '"%s"' % re.sub(r'(["\\])', r'\\\\1', obj)
                elif integerp(obj) or floatp(obj):
                        string += str(obj)
                elif nonep(obj):
                        string += "nil"
                else:
                        raise Exception("can't write object %s" % obj)
                return string   
        return write_sexp_to_string_loop(obj)

# prin1ToString <- function(val) {
#   paste(deparse(val, backtick=TRUE, control=c("delayPromises", "keepNA")),
#         sep="", collapse="\n")
# }
def prin1_to_string(val):
        return "\n".join(deparse(val)) # FIXME

# printToString <- function(val) {
#   paste(capture.output(print(val)), sep="", collapse="\n")
# }
def print_to_string(val):
        # I insist, that this is a less stupid way.  Maybe I'm still wrong..
        return with_output_to_string(lambda s: print(val, file = s))

# `swank:connection-info` <- function (slimeConnection, sldbState) {
#   list(quote(`:pid`), Sys.getpid(),
#        quote(`:package`), list(quote(`:name`), "R", quote(`:prompt`), "R> "),
#        quote(`:lisp-implementation`), list(quote(`:type`), "R",
#                                            quote(`:name`), "R",
#                                            quote(`:version`), paste(R.version$major, R.version$minor, sep=".")))
# }
def swank_connection_info(slime_connection, sldb_state):
        return [intern(":pid"),                 os.getpid(),
                intern(":package"),             [intern(":name"), "python",
                                                 intern(":prompt"), "python>"],
                intern(":lisp-implementation"), [intern(":type"), "python",
                                                 intern(":name"), "python",
                                                 intern(":version"), "%d.%d.%d" % sys.version_info[:3]]]

# `swank:swank-require` <- function (slimeConnection, sldbState, contribs) {
#   for(contrib in contribs) {
#     filename <- sprintf("%s/%s.R", swankrPath, as.character(contrib))
#     if(file.exists(filename)) {
#       source(filename)
#     }
#   }
#   list()
# }
def swank_swank_require(slime_connection, sldb_state, contribs):
        for contrib in contribs:
                filename = "%s/%s.py" % (partus_path, str(contrib))
                if os.path.exists(filename):
                        load_file(filename)
        return []

# `swank:create-repl` <- function(slimeConnection, sldbState, env, ...) {
#   list("R", "R")
# }
def swank_create_repl(slime_connection, sldb_state, env, *args):
        return ["python", "python"]

# makeReplResult <- function(value) {
#   string <- printToString(value)
#   list(quote(`:write-string`), string,
#        quote(`:repl-result`))
# }
def make_repl_result(value):
        string = print_to_string(value)
        return [":write-string", string, ":repl-result"]

# makeReplResultFunction <- makeReplResult
make_repl_result_function = make_repl_result

# sendReplResult <- function(slimeConnection, value) {
#   result <- makeReplResultFunction(value)
#   sendToEmacs(slimeConnection, result)
# }
def send_repl_result(slime_connection, value):
        result = make_repl_result_function(value)
        return send_to_emacs(slime_connection, result)

# sendReplResultFunction <- sendReplResult
send_repl_result_function = send_repl_result

# `swank:listener-eval` <- function(slimeConnection, sldbState, string) {
#   ## O how ugly
#   string <- gsub("#\\.\\(swank:lookup-presented-object-or-lose([^)]*)\\)", ".(`swank:lookup-presented-object-or-lose`(slimeConnection, sldbState,\\1))", string)
#   for(expr in parse(text=string)) {
#     expr <- expr
#     ## O maybe this is even uglier
#     lookedup <- do.call("bquote", list(expr))
#     tmp <- withVisible(eval(lookedup, envir = globalenv()))
#     if(tmp$visible) {
#       sendReplResultFunction(slimeConnection, tmp$value)
#     }
#   }
#   list}()
def swank_listener_eval(slime_connection, sldb_state, string):
        string = re.sub(r"#\.\(swank:lookup-presented-object-or-lose([^)]*)\)", r".(`swank:lookup-presented-object-or-lose`(slime_connection, sldb_state,\1))", string)
        for expr in ...:
                pass

# `swank:autodoc` <- function(slimeConnection, sldbState, rawForm, ...) {
#   "No Arglist Information"
# }
def swank_autodoc(slime_connection, sldb_state, raw_form, *args):
        return "No Arglist Information"

# `swank:operator-arglist` <- function(slimeConnection, sldbState, op, package) {
#   list()
# }
def swank_operator_arglist(slime_connection, sldb_state, op, package):
        return []

# `swank:throw-to-toplevel` <- function(slimeConnection, sldbState) {
#   condition <- simpleCondition("Throw to toplevel")
#   class(condition) <- c("swankTopLevel", class(condition))
#   signalCondition(condition)
# }
class SwankTopLevel(Exception):
        pass
def swank_throw_to_toplevel(slime_connection, sldb_state):
        condition = SwankTopLevel("Throw to toplevel")
        raise condition

# `swank:backtrace` <- function(slimeConnection, sldbState, from=0, to=NULL) {
#   calls <- sldbState$calls
#   if(is.null(to)) to <- length(calls)
#   from <- from+1
#   calls <- lapply(calls[from:to],
#                   { frameNumber <- from-1;
#                     function (x) {
#                       ret <- list(frameNumber, paste(format(x), sep="", collapse=" "))
#                       frameNumber <<- 1+frameNumber
#                       ret
#                     }
#                   })
# }
def swank_backtrace(slime_connection, sldb_state, from_ = 0, to = None):
        calls = sldb_state.calls
        if not to:
                to = len(calls)
        from_ += 1
        frame_number = from_ - 1
        def frame_iter(x):
                nonlocal frame_number
                ret = [frame_number, str(x)]
                frame_number += 1
                return ret
        calls = map(frame_iter, calls[from_:to])
        return calls

# computeRestartsForEmacs <- function (sldbState) {
#   lapply(sldbState$restarts,
#          function(x) {
#            ## this is all a little bit internalsy
#            restartName <- x[[1]][[1]]
#            description <- restartDescription(x)
#            list(restartName, if(is.null(description)) restartName else description)
#          })
# }
def compute_restarts_for_emacs(sldb_state):
        def restart_for_emacs(x):
                restart_name = x[0][0]
                description = restart_description(x)
                return [restart_name, restart_name if not restart_name else description]
        return map(restart_for_emacs,
                   sldb_state.restarts)

# `swank:debugger-info-for-emacs` <- function(slimeConnection, sldbState, from=0, to=NULL) {
#   list(list(as.character(sldbState$condition), sprintf("  [%s]", class(sldbState$condition)[[1]]), FALSE),
#        computeRestartsForEmacs(sldbState),
#        `swank:backtrace`(slimeConnection, sldbState, from, to),
#        list(sldbState$id))
# }
def swank_debugger_info_for_emacs(slime_connection, sldb_state, from_ = 0, to = None):
        return [[str(sldb_state.condition), " [%s]" % sldb_state.condition.__type__.__name__, False],
                compute_restarts_for_emacs(sldb_state),
                swank_backtrace(slime_connection, sldb_state, from_, to),
                [sldb_state.id]]

# `swank:invoke-nth-restart-for-emacs` <- function(slimeConnection, sldbState, level, n) {
#   if(sldbState$level == level) {
#     invokeRestart(sldbState$restarts[[n+1]])
#   }
# }
def swank_invoke_nth_restart_for_emacs(slime_connection, sldb_state, level, n):
        if sldb_state.level == level:
                return invoke_restart(sldb_state.restarts[n+1])

# `swank:frame-source-location` <- function(slimeConnection, sldbState, n) {
#   call <- sldbState$calls[[n+1]]
#   srcref <- attr(call, "srcref")
#   srcfile <- attr(srcref, "srcfile")
#   if(is.null(srcfile)) {
#     list(quote(`:error`), "no srcfile")
#   } else {
#     filename <- get("filename", srcfile)
#     ## KLUDGE: what this means is "is the srcfile filename
#     ## absolute?"
#     if(substr(filename, 1, 1) == "/") {
#       file <- filename
#     } else {
#       file <- sprintf("%s/%s", srcfile$wd, filename)
#     }
#     list(quote(`:location`),
#          list(quote(`:file`), file),
#          list(quote(`:line`), srcref[[1]], srcref[[2]]-1),
#          FALSE)
#   }
# }
def swank_frame_source_location(slime_connection, sldb_state, n):
        call = sldb_state.calls[n + 1]
        srcref = call.srcref
        srcfile = call.srcfile
        if not srcfile:
                return [intern(':error'), "no srcfile"]
        else:
                filename = srcfile.name
                file = filename if filename[0] == '/' else ("%s/%s" % (srcfile.wd, filename))
                return [intern(':location'), [intern(':file'), file], [intern(':line'), srcref[0], srcref[1] - 1], intern('nil')]

# `swank:buffer-first-change` <- function(slimeConnection, sldbState, filename) {
#   FALSE
# }
def swank_buffer_first_change(slime_connection, sldb_state, filename):
        return

# `swank:eval-string-in-frame` <- function(slimeConnection, sldbState, string, index) {
#   frame <- sldbState$frames[[1+index]]
#   withRetryRestart("retry SLIME interactive evaluation request",
#                    value <- eval(parse(text=string), envir=frame))
#   printToString(value)
# }
def swank_eval_string_in_frame(slime_connection, sldb_state, string, index):
        frame = sldb_state.frames[index + 1]
        value = None
        def with_retry_restart_body():
                nonlocal value
                value = eval_in_frame(parse(string),
                                      env = frame)
        with_retry_restart(with_retry_restart_body,
                           mesg = "retry SLIME interactive evaluation request")
        return print_to_string_value

# `swank:frame-locals-and-catch-tags` <- function(slimeConnection, sldbState, index) {
#   frame <- sldbState$frames[[1+index]]
#   objs <- ls(envir=frame)
#   list(lapply(objs, function(name) { list(quote(`:name`), name,
#                                           quote(`:id`), 0,
#                                           quote(`:value`),
#                                           tryCatch({
#                                             printToString(eval(parse(text=name), envir=frame))
#                                           }, error=function(c) {
#                                             sprintf("error printing object")
#                                           }))}),
#        list())
# }
def swank_frame_locals_and_catch_tags(slime_connection, sldb_state, index):
        frame = sldb_state.frames[index + 1]
        objs = ls(env = frame)
        return [map(lambda name: [intern(':name'), name,
                                  intern(':id'), 0,
                                  intern(':value'), compute_value()],
                    objs),
                []]

# `swank:simple-completions` <- function(slimeConnection, sldbState, prefix, package) {
#   literal2rx <- function(string) {
#     ## list of ERE metacharacters from ?regexp
#     gsub("([.\\|()[{^$*+?])", "\\\\\\1", string)
#   }
#   matches <- apropos(sprintf("^%s", literal2rx(prefix)), ignore.case=FALSE)
#   nmatches <- length(matches)
#   if(nmatches == 0) {
#     list(list(), "")
#   } else {
#     longest <- matches[order(nchar(matches))][1]
#     while(length(grep(sprintf("^%s", literal2rx(longest)), matches)) < nmatches) {
#       longest <- substr(longest, 1, nchar(longest)-1)
#     }
#     list(as.list(matches), longest)
#   }
# }
def swank_simple_completions(slime_connection, sldb_state, prefix, package):
        def literal2rx(string):
                return re.sub("([.\\|()[{^$*+?])", "\\\\\\1", string)
        def grep(regex, strings):
                expr = re.compile(regex)
                return [ x for x in strings if re.search(expr, x) ]
        matches = apropos("^%s" % literal2rx(prefix))
        nmatches = len(matches)
        if not matches:
                return [[], ""]
        else:
                longest = sorted(matches, key = len)[0]
                while len(grep("^%s" % literal2rx(longest), matches)) < nmatches:
                        longest = longest[:-1]
                return [matches, longest]

# `swank:compile-string-for-emacs` <- function(slimeConnection, sldbState, string, buffer, position, filename, policy) {
#   lineOffset <- charOffset <- colOffset <- NULL
#   for(pos in position) {
#     switch(as.character(pos[[1]]),
#            `:position` = {charOffset <- pos[[2]]},
#            `:line` = {lineOffset <- pos[[2]]; colOffset <- pos[[3]]},
#            warning("unknown content in pos", pos))
#   }
#   frob <- function(refs) {
#     lapply(refs,
#            function(x)
#            srcref(attr(x,"srcfile"),
#                   c(x[1]+lineOffset-1, ifelse(x[1]==1, x[2]+colOffset-1, x[2]),
#                     x[3]+lineOffset-1, ifelse(x[3]==1, x[4]+colOffset-1, x[4]),
#                     ifelse(x[1]==1, x[5]+colOffset-1, x[5]),
#                     ifelse(x[3]==1, x[6]+colOffset-1, x[6]))))
#   }
#   transformSrcrefs <- function(s) {
#     srcrefs <- attr(s, "srcref")
#     attribs <- attributes(s)
#     new <- 
#       switch(mode(s),
#              "call"=as.call(lapply(s, transformSrcrefs)),
#              "expression"=as.expression(lapply(s, transformSrcrefs)),
#              s)
#     attributes(new) <- attribs
#     if(!is.null(attr(s, "srcref"))) {
#       attr(new, "srcref") <- frob(srcrefs)
#     }
#     new
#   }
#   withRestarts({
#     times <- system.time({
#       exprs <- parse(text=string, srcfile=srcfile(filename))
#       eval(transformSrcrefs(exprs), envir = globalenv()) })},
#                abort="abort compilation")
#   list(quote(`:compilation-result`), list(), TRUE, times[3], FALSE, FALSE)
# }
def swank_compile_string_for_emacs(slime_connection, sldb_state, string, buffer, position, filename, policy):
        line_offset = char_offset = col_offset = None
        for pos in position:
                if pos[0] is intern(':position'):
                        char_offset = pos[1]
                elif pos[0] is intern(':line'):
                        line_offset = pos[1]
                        char_offset = pos[2]
                else:
                        warning("unknown content in pos %s" % pos)
        def frob(refs):
                not_implemented("frob")
        def transform_srcrefs(s):
                not_implemented("transform_srcrefs")
        time = None
        def with_restarts_body():
                nonlocal time
                exprs = None
                def clocking_body():
                        nonlocal exprs
                        exprs = ast.parse(string)
                        return eval(transform_srcrefs(exprs),
                                    globals = ...)
                val, time = clocking(clocking_body)
                return val
        with_restarts(with_restarts_body)
        return [intern(':compilation-result'), [], True, time, False, False]

# withRetryRestart <- function(description, expr) {
#   call <- substitute(expr)
#   retry <- TRUE
#   while(retry) {
#     retry <- FALSE
#     withRestarts(eval.parent(call),
#                  retry=list(description=description,
#                    handler=function() retry <<- TRUE))
#   }
# }
def with_retry_restart(fn, mesg = "Retry"):
        retry = True
        while retry:
                retry = False
                def handler_body():
                        nonlocal retry
                        retry = True
                with_restarts(fn,
                              retry = { 'description': mesg,
                                        'handler':     handler_body })
                        

# `swank:interactive-eval` <-  function(slimeConnection, sldbState, string) {
#   withRetryRestart("retry SLIME interactive evaluation request",
#                    tmp <- withVisible(eval(parse(text=string), envir=globalenv())))
#   if(tmp$visible) {
#     prin1ToString(tmp$value)
#   } else {
#     "# invisible value"
#   }
# }
def swank_interactive_eval(slime_connection, sldb_state, string):
        tmp = None
        def with_retry_restart_body():
                nonlocal tmp
                tmp = with_visible(lambda: eval(string)) # FIXME: envir
                pass
        with_retry_restart(with_retry_restart_body,
                           mesg = "retry SLIME interactive evaluation request")
        return prin1_to_string(value) if tmp.visible else "# invisible value"

# `swank:eval-and-grab-output` <- function(slimeConnection, sldbState, string) {
#   withRetryRestart("retry SLIME interactive evaluation request",
#                    { output <-
#                        capture.output(tmp <- withVisible(eval(parse(text=string),
#                                                               envir=globalenv()))) })
#   output <- paste(output, sep="", collapse="\n")
#   if(tmp$visible) {
#     list(output, prin1ToString(value))
#   } else {
#     list(output, "# invisible value")
#   }
# }
def swank_eval_and_grab_output(slime_connection, sldb_state, string):
        output, tmp  = None, None
        def with_retry_restart_body():
                nonlocal output
                def with_captured_output_body():
                        nonlocal tmp
                        tmp = with_visible(lambda: eval(string)) # FIXME: envir
                        pass
                output = with_captured_output(with_captured_output_body)
        with_retry_restart(with_retry_restart_body,
                           mesg = "retry SLIME interactive evaluation request")
        return ["\n".join(output), prin1_to_string(value) if tmp.visible else "# invisible value"]

# `swank:interactive-eval-region` <- function(slimeConnection, sldbState, string) {
#   withRetryRestart("retry SLIME interactive evaluation request",
#                    tmp <- withVisible(eval(parse(text=string), envir=globalenv())))
#   if(tmp$visible) {
#     prin1ToString(value)
#   } else {
#     "# invisible value"
#   }
# }
def swank_interactive_eval_region(slime_connection, sldb_state, string):
        return swank_interactive_eval(slime_connection, sldb_state, string)

# `swank:find-definitions-for-emacs` <- function(slimeConnection, sldbState, string) {
#   if(exists(string, envir = globalenv())) {
#     thing <- get(string, envir = globalenv())
#     if(inherits(thing, "function")) {
#       body <- body(thing)
#       srcref <- attr(body, "srcref")
#       srcfile <- attr(body, "srcfile")
#       if(is.null(srcfile)) {
#         list()
#       } else {
#         filename <- get("filename", srcfile)
#         ## KLUDGE: what this means is "is the srcfile filename
#         ## absolute?"
#         if(substr(filename, 1, 1) == "/") {
#           file <- filename
#         } else {
#           file <- sprintf("%s/%s", srcfile$wd, filename)
#         }
#         list(list(sprintf("function %s", string),
#                   list(quote(`:location`),
#                        list(quote(`:file`), file),
#                        list(quote(`:line`), srcref[[2]][[1]], srcref[[2]][[2]]-1),
#                        list())))
#       }
#     } else {
#       list()
#     }
#   } else {
#     list()
#   }
# }
def swank_find_definitions_for_emacs(slime_connection, sldb_state, string):
        pass

# `swank:value-for-editing` <- function(slimeConnection, sldbState, string) {
#   paste(deparse(eval(parse(text=string), envir = globalenv()), control="all"),
#         collapse="\n", sep="")
# }
def swank_value_for_editing(slime_connection, sldb_state, string):
        pass

# `swank:commit-edited-value` <- function(slimeConnection, sldbState, string, value) {
#   eval(parse(text=sprintf("%s <- %s", string, value)), envir = globalenv())
#   TRUE
# }
def swank_commit_edited_value(slime_connection, sldb_state, string, value):
        pass

# resetInspector <- function(slimeConnection) {
#   assign("istate", list(), envir=slimeConnection)
#   assign("inspectorHistory", NULL, envir=slimeConnection)
# }
def reset_inspector(slime_connection):
        global istate, inspector_history
        istate = []
        inspector_history = dict()

# `swank:init-inspector` <- function(slimeConnection, sldbState, string) {
#   withRetryRestart("retry SLIME inspection request",
#                    { resetInspector(slimeConnection)
#                      value <- inspectObject(slimeConnection, eval(parse(text=string), envir=globalenv()))
#                    })
#   value
# }
def swank_init_inspector(slime_connection, sldb_state, string):
        value = None
        def with_retry_restart_body():
                nonlocal value
                reset_inspector(slime_connection)
                value = inspect_object(slime_connection,
                                       eval(string)) # FIXME envir
                pass
        with_retry_restart(with_retry_restart_body,
                           mesg = "retry SLIME inspection request")
        return value

# inspectObject <- function(slimeConnection, object) {
#   previous <- slimeConnection$istate
#   slimeConnection$istate <- new.env()
#   slimeConnection$istate$object <- object
#   slimeConnection$istate$previous <- previous
#   slimeConnection$istate$content <- emacsInspect(object)
#   if(!(object %in% slimeConnection$inspectorHistory)) {
#     slimeConnection$inspectorHistory <- c(slimeConnection$inspectorHistory, object)
#   }
#   if(!is.null(slimeConnection$istate$previous)) {
#     slimeConnection$istate$previous$`next` <- slimeConnection$istate
#   }
#   istateToElisp(slimeConnection$istate)
# }
def inspect_object(slime_connection, object):
        previous = slime_connection.istate
        slime_connection.istate = new_env() # FIXME
        slime_connection.istate.object = object
        slime_connection.istate.previous = previous
        slime_connection.istate.content = emacs_inspect(object)
        slime_connection.inspector_history.add(object)
        if slime_connection.istate.previous:
                slime_connection.istate.previous.next = slime_connection.istate
        return istate_to_elisp(slime_connection.istate)

# valuePart <- function(istate, object, string) {
#   list(quote(`:value`),
#        if(is.null(string)) printToString(object) else string,
#        assignIndexInParts(object, istate))
# }
def value_part(istate, object, string):
        return [":value",
                string or print_to_string(object),
                assign_index_in_parts(object, istate)]

# preparePart <- function(istate, part) {
#   if(is.character(part)) {
#     list(part)
#   } else {
#     switch(as.character(part[[1]]),
#            `:newline` = list("\n"),
#            `:value` = valuePart(istate, part[[2]], part[[3]]),
#            `:line` = list(printToString(part[[2]]), ": ",
#              valuePart(istate, part[[3]], NULL), "\n"))
#   }
# }
def prepare_part(istate, part):
        if type(part) == str:
                return [part]
        elif part[0] == ":newline":
                return ["\n"]
        elif part[0] == ":value":
                return value_part(istate, part[1], part[2])
        elif part[0] == ":line":
                return [print_to_string(part[1]), ": ",
                        value_part(istate, part[2], NULL), "\n"]
                

# prepareRange <- function(istate, start, end) {
#   range <- istate$content[start+1:min(end+1, length(istate$content))]
#   ps <- NULL
#   for(part in range) {
#     ps <- c(ps, preparePart(istate, part))
#   }
#   list(ps, if(length(ps)<end-start) { start+length(ps) } else { end+1000 },
#        start, end)
# }
def prepare_range(istate, start, end):
        range = istate.content[start:min(end, len(istate.content) - 1)]
        ps = None
        for part in range:
                ps += prepare_part(istate, part)
        return [ps, (start + len(ps)) if len(ps) < end - start else (end + 1000),
                start, end]

# assignIndexInParts <- function(object, istate) {
#   ret <- 1+length(istate$parts)
#   istate$parts <- c(istate$parts, list(object))
#   ret
# }
def assing_index_in_parts(object, istate):
        ret = len(istate.parts) + 1
        istate.parts.append(object)
        return ret

# istateToElisp <- function(istate) {
#   list(quote(`:title`), deparse(istate$object, control="all", nlines=1),
#        quote(`:id`), assignIndexInParts(istate$object, istate),
#        quote(`:content`), prepareRange(istate, 0, 500))
# }
def istate_to_elisp(istate):
        return [":title",   deparse(istate.object),
                ":id",      assign_index_in_parts(istate.object, istate),
                ":content", prepare_range(istate, 0, 500)]

def emacs_inspect(object):
# emacsInspect.list <- function(list) {
#   c(list("a list", list(quote(`:newline`))),
#     mapply(function(name, value) { list(list(quote(`:line`), name, value)) },
#            names(list), list))
# }
        if dictp(object):
                return ["a dict", ":newline"] + [ [":line", name, object[name] ] for name in object ]
# emacsInspect.numeric <- function(numeric) {
#   c(list("a numeric", list(quote(`:newline`))),
#     mapply(function(name, value) { list(list(quote(`:line`), name, value)) },
#            (1:length(numeric)), numeric))
# }
        # elif integerp(object):
        #         return ["a numeric", ":newline"] + 
        else:
# emacsInspect.default <- function(thing) {
#   c(list(paste("a ", class(thing)[[1]], sep=""), list(quote(`:newline`))))
# }
                return ["a %s" % type(object).__name__, ":newline"]



# `swank:quit-inspector` <- function(slimeConnection, sldbState) {
#   resetInspector(slimeConnection)
#   FALSE
# }
def swank_quit_inspector(slime_connection, sldb_state):
        reset_inspector(slime_connection)
        return False

# `swank:inspector-nth-part` <- function(slimeConnection, sldbState, index) {
#   slimeConnection$istate$parts[[index]]
# }
def swank_inspector_nth_part(slime_connection, sldb_state, index):
        return slime_connection.istate.pargs[index]

# `swank:inspect-nth-part` <- function(slimeConnection, sldbState, index) {
#   object <- `swank:inspector-nth-part`(slimeConnection, sldbState, index)
#   inspectObject(slimeConnection, object)
# }
def swank_inspect_nth_part(slime_connection, sldb_state, index):
        object = swank_inspector_nth_parg(slime_connection, sldb_state, index)
        return inspect_object(slime_connection, object)

# `swank:inspector-pop` <- function(slimeConnection, sldbState) {
#   if(!is.null(slimeConnection$istate$previous)) {
#     slimeConnection$istate <- slimeConnection$istate$previous
#     istateToElisp(slimeConnection$istate)
#   } else {
#     FALSE
#   }
# }
def swank_inspector_pop(slime_connection, sldb_state):
        if slime_connection.istate.previous:
                slime_connection.istate = slime_connection.istate.previous
                return istate_to_elisp(slime_connection.istate)
        else:
                return False

# `swank:inspector-next` <- function(slimeConnection, sldbState) {
#   if(!is.null(slimeConnection$istate$`next`)) {
#     slimeConnection$istate <- slimeConnection$istate$`next`
#     istateToElisp(slimeConnection$istate)
#   } else {
#     FALSE
#   }
# }
def swank_inspector_next(slime_connection, sldb_state):
        if slime_connection.istate.next:
                slime_connection.istate = slime_connection.istate.next
                return istate_to_elisp(slime_connection.istate)
        else:
                return False

# `swank:inspector-eval` <- function(slimeConnection, sldbState, string) {
#   expr <- parse(text=string)[[1]]
#   object <- slimeConnection$istate$object
#   if(inherits(object, "list")|inherits(object, "environment")) {
#     substituted <- substituteDirect(expr, object)
#     eval(substituted, envir=globalenv())
#   } else {
#     eval(expr, envir=globalenv())
#   }
# }
def swank_inspector_eval(slime_connection, sldb_state, string):
        pass

# `swank:inspect-current-condition` <- function(slimeConnection, sldbState) {
#   resetInspector(slimeConnection)
#   inspectObject(slimeConnection, sldbState$condition)
# }
def swank_inspect_current_condition(slime_connection, sldb_state):
        reset_inspector(slime_connection)
        return inspect_object(slime_connection, sldb_state.condition)

# `swank:inspect-frame-var` <- function(slimeConnection, sldbState, frame, var) {
#   resetInspector(slimeConnection)
#   frame <- sldbState$frames[[1+frame]]
#   name <- ls(envir=frame)[[1+var]]
#   object <- get(name, envir=frame)
#   inspectObject(slimeConnection, object)
# }
def swank_inspect_frame_var(slime_connection, sldb_state, frame, var):
        reset_inspector(slime_connection)
        frame = sldb_state.frames[frame + 1]
        name = ls(env = frame)[var + 1]
        object = env_get(name, env = frame)
        return inspect_object(slime_connection, object)

# `swank:default-directory` <- function(slimeConnection, sldbState) {
#   getwd()
# }
def swank_default_directory(slime_connection, sldb_state):
        return os.getcwd()

# `swank:set-default-directory` <- function(slimeConnection, sldbState, directory) {
#   setwd(directory)
#   `swank:default-directory`(slimeConnection, sldbState)
# }
def swank_set_default_directory(slime_connection, sldb_state, directory):
        os.chdir(directory)
        return swank_default_directory(slime_connection, sldb_state)

# `swank:load-file` <- function(slimeConnection, sldbState, filename) {
#   source(filename, local=FALSE, keep.source=TRUE)
#   TRUE
# }
def swank_load_file(slime_connection, sldb_state, filename):
        exec(filename.co)
        return True

# `swank:compile-file-for-emacs` <- function(slimeConnection, sldbState, filename, loadp, ...) {
#   times <- system.time(parse(filename, srcfile=srcfile(filename)))
#   if(loadp) {
#     ## KLUDGE: inelegant, but works.  It might be more in the spirit
#     ## of things to keep the result of the parse above around to
#     ## evaluate.
#     `swank:load-file`(slimeConnection, sldbState, filename)
#   }
#   list(quote(`:compilation-result`), list(), TRUE, times[3], substitute(loadp), filename)
# }
def swank_compile_file_for_emacs(slime_connection, sldb_state, filename, loadp, *args):
        import ast
        filename.co, time = clocking(lambda: compile(filename.name, filename.src))
        if loadp:
                swank_load_file(slime_connection, sldb_state, filename)
        return [intern(':compilation-result'), [], True, time, substitute(loadp), filename]

# `swank:quit-lisp` <- function(slimeConnection, sldbState) {
#   quit()
# }
def swank_quit_lisp(slime_connection, sldb_state):
        exit()
