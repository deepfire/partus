import os
import sys
import re
import ast
import select

from cl import *
from pergamum import *
from more_ast import *

###
### Dynamic scope.
###
_stack = []

class _env_block(object):
    def __init__(self, kwargs):
        self.kwargs = kwargs
    def __enter__(self):
        _stack.append(self.kwargs)
    def __exit__(self, t, v, tb):
        _stack.pop()

class _dynamic_scope(object):
    "Courtesy of Jason Orendorff."
    def __getattr__(self, name):
        for scope in reversed(_stack):
            if name in scope:
                return scope[name]
        raise AttributeError("Special %s not bound." % name)
    def let(self, **kwargs):
        return _env_block(kwargs)
    def __setattr__(self, name, value):
        raise AttributeError("Env variables can only be set using `with env.let()`.")

env = _dynamic_scope()

###
### Symbols.
###
class symbol():
        name = None
        def __str__(self):  return self.name
        def __repr__(self): return self.name
        def __init__(self, name):
                self.name = name.upper()
def symbolp(x):     return type(x) is symbol
def keywordp(x):    return symbolp(x) and x.name[0] == ":"
def symbol_name(x): return x.name.lower()
syms = dict()
def intern(x):
        X = x.upper()
        if X in syms:
                return syms[X]
        else:
                syms[X] = symbol(X)
                return syms[X]

###
### Globals.
###
t   = intern("t")
nil = intern("nil")

partus_version = "2011-09-28"

debug = True

###
### Evaluation result.
###
___expr___ = None
def set_value(value):
        global ___expr___
        ___expr___ = value
def get_value():
        return ___expr___

###
### SLDB state.
###
# makeSldbState <- function(condition, level, id) {
#   calls <- rev(sys.calls())[-1]
#   frames <- rev(sys.frames())[-1]
#   restarts <- rev(computeRestarts(condition))[-1]
#   ret <- list(condition=condition, level=level, id=id, restarts=restarts, calls=calls, frames=frames)
#   class(ret) <- c("sldbState", class(ret))
#   ret
# }
class servile():
        def __init__(self, **keys):
                self.__dict__.update(keys)

class SldbState(servile): pass

def make_sldb_state(condition, level, id):
        def unwind_frames(f):
                return [f] + (unwind_frames(f.f_back) if f.f_back else [])
        top_frame = sys.exc_info()[2].tb_frame
        frames = unwind_frames(top_frame)
        # debug_printf("frames: %s", frames)
        return SldbState(frames = frames,
                         restarts = [],
                         condition = condition,
                         level = level,
                         id = id)

###
### Potemkin shit.
###
def not_implemented(mesg):
        raise Exception("ERROR: not implemented: " + mesg)
def with_restarts(fn, **restarts):
        # not_implemented("with_restarts()")
        return fn()

###
### Actual implementation.
###
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
obj2lisp_xform = {
        False : "nil",
        None  : "nil",
        True  : "t",
        }
def write_sexp_to_string(obj):
        debug_printf("write_sexp_to_string: %s", obj)
        def do_write_sexp_to_string(obj):
                string = ""
                def write_sexp_to_string_loop(obj):
                        nonlocal string
                        if listp(obj) or tuplep(obj):
                                string += '('
                                max = len(obj)
                                if max:
                                        for i in range(0, max):
                                                string += do_write_sexp_to_string(obj[i])
                                                if i != (max - 1):
                                                        string += " "
                                string += ')'
                        elif symbolp(obj):
                                string += symbol_name(obj)
                        elif integerp(obj) or floatp(obj):
                                string += str(obj)
                        elif obj in obj2lisp_xform:
                                string += obj2lisp_xform[obj]
                        elif type(obj).__name__ == 'builtin_function_or_method':
                                string += '"#<builtin %s 0x%x>"' % (obj.__name__, id(obj))
                        elif stringp(obj):
                                string += r'"%s"' % re.sub(r'(["\\])', r'\\\\1', obj)
                        else:
                                raise Exception("can't write object %s" % obj)
                        return string
                return write_sexp_to_string_loop(obj)
        ret = do_write_sexp_to_string(obj)
        debug_printf("===> %s", ret)
        return ret

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
        debug_printf("-> %06x %s", len(payload), payload)
        file.flush()

# `swank:connection-info` <- function (slimeConnection, sldbState) {
#   list(quote(`:pid`), Sys.getpid(),
#        quote(`:package`), list(quote(`:name`), "R", quote(`:prompt`), "R> "),
#        quote(`:lisp-implementation`), list(quote(`:type`), "R",
#                                            quote(`:name`), "R",
#                                            quote(`:version`), paste(R.version$major, R.version$minor, sep=".")))
# }
def connection_info(slime_connection, sldb_state):
        return [intern(":pid"),                 os.getpid(),
                ### TODO: current package
                intern(":package"),             [intern(":name"), "python",
                                                 intern(":prompt"), "python>"],
                intern(":version"),             partus_version,
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
def swank_require(slime_connection, sldb_state, contribs):
        for contrib in contribs:
                filename = "%s/%s.py" % (env.partus_path, str(contrib))
                if os.path.exists(filename):
                        load_file(filename)
        return []

# `swank:create-repl` <- function(slimeConnection, sldbState, env, ...) {
#   list("R", "R")
# }
def create_repl(slime_connection, sldb_state, env, *args):
        return ["python", "python"]

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
def writeurn_output(output):
        string = get_output_stream_string(output)
        close(output)
        if len(string):
                send_to_emacs(env.slime_connection, [intern(':write-string'), string])
                # send_to_emacs(env.slime_connection, [intern(':write-string'), "\n"])
        return string

def error_handler(c, sldb_state, output = None):
        global condition
        condition = c
        debug_printf("===( e-ha %s, sldb_state: %s", c, sldb_state)
        if output:
                writeurn_output(output)
        new_sldb_state = make_sldb_state(c, 0 if not sldb_state else sldb_state.level + 1, env.id)
        with env.let(sldb_state = new_sldb_state):
                debug_printf("===( e-ha %s, new_sldb_state: %s", c, new_sldb_state)
                def with_restarts_body():
                        return sldb_loop(env.slime_connection, new_sldb_state, env.id)
                with_restarts(with_restarts_body,
                              abort = "return to sldb level %s" % str(new_sldb_state.level))

def swank_ast_name(x):
        return ast_name(x) if debug else ast_attribute(ast_name("swank"), x)

def listener_eval(slime_connection, sldb_state, string):
        string = re.sub(r"#\.\(swank:lookup-presented-object([^)]*)\)", r"lookup_presented_object(slime_connection, sldb_state,\\1))", string)
        def eval_stage(name, fn):
                try:
                        return fn()
                except Exception as cond:
                        debug_printf("===( LISTENER %s: %s, sldb state: %s", name, cond, sldb_state)
                        error_handler(cond, sldb_state)
                        return None
        ast_ = eval_stage("PARSE", lambda: ast.parse(string))
        if ast_ and ast_.body:
                exprp = typep(ast_.body[0], ast.Expr)
                if exprp:
                        ast_.body[0] = ast_assign_var("", ast_funcall(swank_ast_name("set_value"), ast_.body[0].value))
                co = eval_stage("COMPILE", lambda: compile(ast.fix_missing_locations(ast_), "", 'exec'))
                eval_stage("EXEC", lambda: exec(co, env.python_user.__dict__))
                return [intern(":values")] + [str(___expr___)] if (ast_.body and exprp) else []
        else:
                return [intern(":values")]

# `swank:autodoc` <- function(slimeConnection, sldbState, rawForm, ...) {
#   "No Arglist Information"
# }
def autodoc(slime_connection, sldb_state, raw_form, *args):
        return "No Arglist Information"

# `swank:operator-arglist` <- function(slimeConnection, sldbState, op, package) {
#   list()
# }
def operator_arglist(slime_connection, sldb_state, op, package):
        return []

# `swank:throw-to-toplevel` <- function(slimeConnection, sldbState) {
#   condition <- simpleCondition("Throw to toplevel")
#   class(condition) <- c("swankTopLevel", class(condition))
#   signalCondition(condition)
# }
class SwankTopLevel(Exception):
        pass
def throw_to_toplevel(slime_connection, sldb_state):
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
def backtrace(slime_connection, sldb_state, from_ = 0, to = None):
        frames = sldb_state.frames
        if nonep(to):
                to = len(frames)
        longest = max(map(lambda f: len(f.f_code.co_filename), frames))
        def pp_frame(f):
                co = f.f_code
                filename = co.co_filename or "<unknown-file>"
                padding = " " * (longest - len(filename))
                return "%s: %s(%s)" % (padding + filename, co.co_name, ", ".join(co.co_cellvars))
        return list(enumerate(map(pp_frame, frames[from_ + 1:to]), from_))

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
        return list(map(restart_for_emacs,
                        sldb_state.restarts))

# `swank:debugger-info-for-emacs` <- function(slimeConnection, sldbState, from=0, to=NULL) {
#   list(list(as.character(sldbState$condition), sprintf("  [%s]", class(sldbState$condition)[[1]]), FALSE),
#        computeRestartsForEmacs(sldbState),
#        `swank:backtrace`(slimeConnection, sldbState, from, to),
#        list(sldbState$id))
# }
def debugger_info_for_emacs(slime_connection, sldb_state, from_ = 0, to = None):
        return [[str(sldb_state.condition), " [%s]" % type(sldb_state.condition).__name__, False],
                compute_restarts_for_emacs(sldb_state),
                backtrace(slime_connection, sldb_state, from_, to),
                [sldb_state.id]]

# `swank:invoke-nth-restart-for-emacs` <- function(slimeConnection, sldbState, level, n) {
#   if(sldbState$level == level) {
#     invokeRestart(sldbState$restarts[[n+1]])
#   }
# }
def invoke_nth_restart_for_emacs(slime_connection, sldb_state, level, n):
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
def frame_source_location(slime_connection, sldb_state, n):
        frame = sldb_state.frames[n + 1]
        co = frame.f_code
        def co_nlines(co):
                return 1 + max(co.co_lnotab)
        srcfile, line, nlines, column, name = co.co_filename, co.co_firstlineno, co_nlines(co), 0, co.name
        if not srcfile:
                return [intern(':error'), "no srcfile"]
        else:
                return [intern(':location'),
                        [intern(':file'), srcfile],
                        [intern(':line'), line, srcref[1] - 1],
                        intern('nil')]

# `swank:buffer-first-change` <- function(slimeConnection, sldbState, filename) {
#   FALSE
# }
def buffer_first_change(slime_connection, sldb_state, filename):
        return

# `swank:eval-string-in-frame` <- function(slimeConnection, sldbState, string, index) {
#   frame <- sldbState$frames[[1+index]]
#   withRetryRestart("retry SLIME interactive evaluation request",
#                    value <- eval(parse(text=string), envir=frame))
#   printToString(value)
# }
def eval_string_in_frame(slime_connection, sldb_state, string, index):
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
def make_frame():
        def x(a):
                b = 1
                try:
                        raise Exception("a: %s, b: %s", a, b)
                except Exception as cond:
                        return sys.exc_info()[2]
        def y(c):
                d = c + 1
                return x(d)
        return y(0)

def frame_locals_and_catch_tags(slime_connection, sldb_state, index):
        frame = sldb_state.frames[index + 1]
        return [map_hash_table(lambda name, value: [intern(':name'), name,
                                                    intern(':id'), 0,
                                                    intern(':value'), handler_bind(lambda: print_to_string(value),
                                                                                   error = lambda c: "Error printing object: %s." % c)],
                               frame.f_locals),
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
def simple_completions(slime_connection, sldb_state, prefix, package):
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
def compile_string_for_emacs(slime_connection, sldb_state, string, buffer, position, filename, policy):
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
def interactive_eval(slime_connection, sldb_state, string):
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
def eval_and_grab_output(slime_connection, sldb_state, string):
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
def interactive_eval_region(slime_connection, sldb_state, string):
        return interactive_eval(slime_connection, sldb_state, string)

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
def find_definitions_for_emacs(slime_connection, sldb_state, string):
        pass

# `swank:value-for-editing` <- function(slimeConnection, sldbState, string) {
#   paste(deparse(eval(parse(text=string), envir = globalenv()), control="all"),
#         collapse="\n", sep="")
# }
def value_for_editing(slime_connection, sldb_state, string):
        pass

# `swank:commit-edited-value` <- function(slimeConnection, sldbState, string, value) {
#   eval(parse(text=sprintf("%s <- %s", string, value)), envir = globalenv())
#   TRUE
# }
def commit_edited_value(slime_connection, sldb_state, string, value):
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
def init_inspector(slime_connection, sldb_state, string):
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

# `swank:quit-inspector` <- function(slimeConnection, sldbState) {
#   resetInspector(slimeConnection)
#   FALSE
# }
def quit_inspector(slime_connection, sldb_state):
        reset_inspector(slime_connection)
        return False

# `swank:inspector-nth-part` <- function(slimeConnection, sldbState, index) {
#   slimeConnection$istate$parts[[index]]
# }
def inspector_nth_part(slime_connection, sldb_state, index):
        return slime_connection.istate.pargs[index]

# `swank:inspect-nth-part` <- function(slimeConnection, sldbState, index) {
#   object <- `swank:inspector-nth-part`(slimeConnection, sldbState, index)
#   inspectObject(slimeConnection, object)
# }
def inspect_nth_part(slime_connection, sldb_state, index):
        object = inspector_nth_parg(slime_connection, sldb_state, index)
        return inspect_object(slime_connection, object)

# `swank:inspector-pop` <- function(slimeConnection, sldbState) {
#   if(!is.null(slimeConnection$istate$previous)) {
#     slimeConnection$istate <- slimeConnection$istate$previous
#     istateToElisp(slimeConnection$istate)
#   } else {
#     FALSE
#   }
# }
def inspector_pop(slime_connection, sldb_state):
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
def inspector_next(slime_connection, sldb_state):
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
def inspector_eval(slime_connection, sldb_state, string):
        pass

# `swank:inspect-current-condition` <- function(slimeConnection, sldbState) {
#   resetInspector(slimeConnection)
#   inspectObject(slimeConnection, sldbState$condition)
# }
def inspect_current_condition(slime_connection, sldb_state):
        reset_inspector(slime_connection)
        return inspect_object(slime_connection, sldb_state.condition)

# `swank:inspect-frame-var` <- function(slimeConnection, sldbState, frame, var) {
#   resetInspector(slimeConnection)
#   frame <- sldbState$frames[[1+frame]]
#   name <- ls(envir=frame)[[1+var]]
#   object <- get(name, envir=frame)
#   inspectObject(slimeConnection, object)
# }
def inspect_frame_var(slime_connection, sldb_state, frame, var):
        reset_inspector(slime_connection)
        frame = sldb_state.frames[frame + 1]
        name = ls(env = frame)[var + 1]
        object = env_get(name, env = frame)
        return inspect_object(slime_connection, object)

# `swank:default-directory` <- function(slimeConnection, sldbState) {
#   getwd()
# }
def default_directory(slime_connection, sldb_state):
        return os.getcwd()

# `swank:set-default-directory` <- function(slimeConnection, sldbState, directory) {
#   setwd(directory)
#   `swank:default-directory`(slimeConnection, sldbState)
# }
def set_default_directory(slime_connection, sldb_state, directory):
        os.chdir(directory)
        return default_directory(slime_connection, sldb_state)

# `swank:load-file` <- function(slimeConnection, sldbState, filename) {
#   source(filename, local=FALSE, keep.source=TRUE)
#   TRUE
# }
def load_file(slime_connection, sldb_state, filename):
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
def compile_file_for_emacs(slime_connection, sldb_state, filename, loadp, *args):
        import ast
        filename.co, time = clocking(lambda: compile(filename.name, filename.src))
        if loadp:
                load_file(slime_connection, sldb_state, filename)
        return [intern(':compilation-result'), [], True, time, substitute(loadp), filename]

# `swank:quit-lisp` <- function(slimeConnection, sldbState) {
#   quit()
# }
def quit_lisp(slime_connection, sldb_state):
        exit()

###
### emacs-rex
###
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
        ret = remove_if_not(identity, re.sub("[\\-]", "_", x).lower().split(":"))
        debug_printf("==> Python(Lisp %s) == %s", x, ret)
        return ret

def lisp_name_ast(x):
        def rec(x):
                return (ast_name(x[0])
                        if len(x) == 1 else
                        ast_attribute(rec(x[:-1]), x[-1]))
        return rec(pythonise_lisp_name(x))

def constantp(x):
        return type(x) in set([str, int])
obj2ast_xform = {
        False : ast_name("False"),
        None  : ast_name("None"),
        True  : ast_name("True"),
        str   : ast_string,
        int   : ast_num,
        }

def callify(form, quoted = False):
        debug_printf("CALLIFY %s", form)
        if listp(form):
                if quoted or (form[0] is intern('quote')):
                        return (ast_list(mapcar(lambda x: callify(x, quoted = True), form[1]))
                                if listp(form[1]) else
                                callify(form[1], quoted = True))
                else:
                        return ast_funcall(lisp_name_ast(symbol_name(form[0])),
                                           ast_attribute(swank_ast_name("env"), "slime_connection"),
                                           ast_attribute(swank_ast_name("env"), "sldb_state"),
                                           *map(callify, form[1:]))
        elif symbolp(form):
                return (ast_funcall(swank_ast_name("intern"), symbol_name(form))
                        if quoted or (symbol_name(form)[0] == ":") else
                        ast_name(symbol_name(form)))
        elif constantp(form):
                return obj2ast_xform[type(form)](form)
        elif form in obj2ast_xform:
                return obj2ast_xform[form]
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
def emacs_rex(slime_connection, sldb_state, form, pkg, thread, id, level = 0):
        ok = False
        value = intern('nil')
        condition = intern('nil')
        output = make_string_output_stream()
        try:
                def send_abort(cond, mesg, *args):
                        nonlocal condition
                        writeurn_output(output)
                        condition = cond
                        debug_printf("ERROR:" + mesg, *args)
                        raise cond
                def with_calling_handlers_body():
                        nonlocal ok, value
                        try:
                                expr = callify(form)
                                call = ast.fix_missing_locations(ast_module(
                                                [# ast_import_from("partus", ["*"]),
                                                 ast_assign_var("", ast_funcall(swank_ast_name("set_value"), expr)),
                                                 ]))
                        except Exception as cond:
                                send_abort(cond, "failed to callify: %s", cond)
                        debug_printf("==========( COMPILE-AST\n%s\n", pp_ast_as_code(expr))
                        try:
                                code = compile(call, '', 'exec')
                        except Exception as cond:
                                send_abort(cond, "failed to compile: %s", cond)
                        debug_printf("executing..")
                        with_output_redirection(lambda: exec(code, env.python_user.__dict__), file = output)
                        value = get_value()
                        string = writeurn_output(output)
                        debug_printf("return value: %s", value)
                        # debug_printf("output:\n%s\n===== EOF =====\n", string)
                        ok = True
                with env.let(id = id):
                        handler_bind(with_calling_handlers_body,
                                     error = lambda cond: error_handler(cond, sldb_state, output = output))
        finally:
                send_to_emacs(slime_connection, [intern(':return'),
                                                 ([intern(':ok'), value]
                                                  if ok else
                                                  [intern(':abort'), condition]),
                                                 id])

# dispatch <- function(slimeConnection, event, sldbState=NULL) {
#  kind <- event[[1]]
#  if(kind == quote(`:emacs-rex`)) {
#    do.call("emacsRex", c(list(slimeConnection), list(sldbState), event[-1]))
#  }
# }
def dispatch(slime_connection, event, sldb_state):
        kind = event[0]
        debug_printf("===( DISPATCH, sldb_state: %s", sldb_state)
        if kind is intern(':emacs-rex'):
                emacs_rex(*([slime_connection, sldb_state] + event[1:]))

###
### I/O
###
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
                # debug_printf("read(#\\%s :: '%s')", char, string[pos + 1:])
                if   char == "(":  obj = read_list()
                elif char == "\"": obj = read_string()
                elif char == "'":  obj = read_quote()
                else:
                        if pos > len(string):
                                error("EOF during read")
                        obj = read_number_or_symbol()
                        if obj == ".":
                                error("Consing dot not implemented")
                # debug_printf("read(): returning %s", obj)
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
                # debug_printf("read_list(): returning %s", ret)
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
                # debug_printf("read_string(): returning %s", ret)
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
                # debug_printf("read_number_or_symbol(): returning %s", ret)
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
                # debug_printf("read_token(): returning %s", token)
                return token
        ret = read()
        debug_printf("==============( REMOTE SEXP:\n%s\n", ret)
        return ret
#   read()
# }

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
        debug_printf("<- %s %s", header, payload)
        return read_sexp_from_string(payload)

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
def sldb_loop(slime_connection, sldb_state, id_):
        try:
                io = slime_connection.io
                send_to_emacs(slime_connection, [intern(':debug'), id_, sldb_state.level] + debugger_info_for_emacs(slime_connection, sldb_state))
                send_to_emacs(slime_connection, [intern(':debug-activate'), id_, sldb_state.level, False])
                while True:
                        dispatch(slime_connection, read_packet(slime_connection.sock, slime_connection.file), sldb_state)
        except Exception as cond:
                print_backtrace()
                debug_printf("===( SLDB got X: %s", cond)
        finally:
                send_to_emacs(slime_connection, [intern(':debug-return'), id_, sldb_state.level, False])

###
### Inspection.
###
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
        return with_output_to_string(lambda s: print(val, file = s, end = ''))


# makeReplResult <- function(value) {
#   string <- printToString(value)
#   list(quote(`:write-string`), string,
#        quote(`:repl-result`))
# }
def make_repl_result(value):
        string = print_to_string(value)
        return [intern(":write-string"), string, intern(":repl-result")]

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
        return [intern(":value"),
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
        elif part[0] is intern(":newline"):
                return ["\n"]
        elif part[0] is intern(":value"):
                return value_part(istate, part[1], part[2])
        elif part[0] is intern(":line"):
                return [ print_to_string(part[1]), ": ",
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
def assign_index_in_parts(object, istate):
        ret = len(istate.parts) + 1
        istate.parts.append(object)
        return ret

# istateToElisp <- function(istate) {
#   list(quote(`:title`), deparse(istate$object, control="all", nlines=1),
#        quote(`:id`), assignIndexInParts(istate$object, istate),
#        quote(`:content`), prepareRange(istate, 0, 500))
# }
def istate_to_elisp(istate):
        return [intern(":title"),   deparse(istate.object),
                intern(":id"),      assign_index_in_parts(istate.object, istate),
                intern(":content"), prepare_range(istate, 0, 500)]

def emacs_inspect(object):
# emacsInspect.list <- function(list) {
#   c(list("a list", list(quote(`:newline`))),
#     mapply(function(name, value) { list(list(quote(`:line`), name, value)) },
#            names(list), list))
# }
        if dictp(object):
                return ["a dict", intern(":newline")] + [ [intern(":line"), name, object[name] ] for name in object ]
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
                return ["a %s" % type(object).__name__, intern(":newline")]
