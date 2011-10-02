def not_implemented(mesg):
        raise Exception("ERROR: not implemented: " + mesg)
def with_restarts(fn, **restarts):
        return fn()
def compute_restarts(condition):
        not_implemented("compute_restarts()")
def invoke_restart(restart):
        not_implemented("invoke_restart()")
def restart_description(restart):
        not_implemented("restart_description()")
def with_calling_handlers(fn, error = lambda c: None):
        return fn()
def sys_calls():
        not_implemented("sys_calls()")
def sys_frames():
        not_implemented("sys_frames()")
def with_output_redirection(fn, file = None):
        return fn()
#
#
#
import os, sys, socket, select

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
        s.bind(('', port))
        f = s.makefile(mode = "rw")
        serve(s, f)

# serve <- function(io) {
#  mainLoop(io)
# }
def serve(sock, file):
        main_loop(sock, file)

# mainLoop <- function(io) {
#  slimeConnection <- new.env()
#  slimeConnection$io <- io
#  while(TRUE) {
#    withRestarts(tryCatch(dispatch(slimeConnection, readPacket(io)),
#                          swankTopLevel=function(c) NULL),
#                 abort="return to SLIME's toplevel")
#  }
}
class servile():
        def __init__(self, **keys):
                self.__dict__.update(keys)

class SlimeConnection(servile): pass

def main_loop(sock, file):
        global slime_connection
        slime_connection = SlimeConnection(sock = sock, file = file, io = file)
        while True:
                def with_restarts_body():
                        try:
                                dispatch(slime_connection, read_packet(file))
                        except Exception as x: # FIXME
                                not_implemented("caught " + str(x))
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
        if kind == ':emacs-rex':
                emacsRex(*([slime_connection, sldb_state] + event[:1]))

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
def callify(form):
        if type(form) is list:
                print("CALLIFY %s" % form)
                if form[0] == 'quote':
                        return form
                else:
                        return [form[0], slime_connection, sldb_state] + map(callify, form[1:])
        else:
                return form

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
        value = None
        conn = io.StringIO()
        condition = None
        try:
                def with_calling_handlers_body():
                        call = callify(form)
                        print("executing %s", call)
                        def with_output_redirection_body():
                                nonlocal value
                                value = exec(call)
                        with_output_redirection(with_output_redirection_body,
                                                file = conn)
                        string = "\n".join(map(str, conn.getvalue()))
                        if len(string):
                                send_to_emacs(slime_connection, [':write-string', string])
                                send_to_emacs(slime_connection, [':write-string', "\n"])
                        close(conn)
                        ok = True
                def error_handler(c):
                        global condition
                        condition = c
                        string = "\n".join(map(str, conn.getvalue()))
                        if len(string):
                                send_to_emacs(slime_connection, [':write-string', string])
                                send_to_emacs(slime_connection, [':write-string', "\n"])
                        close(conn)
                        new_sldb_state = make_sldb_state(c, 0 if not sldb_state else sldb_state.level + 1, id)
                        def with_restarts_body():
                                return sldb_loop(slime_connection, new_sldb_state, id)
                        with_restarts(with_restarts_body,
                                      abort = "return to sldb level %s" % str(new_sldb_state.level))
                with_calling_handlers(with_calling_handlers_body,
                                      error = error_handler)
        finally:
                send_to_emacs(slime_connection, [':return',
                                                 ([':ok', value]
                                                  if ok else
                                                  [':abort', str(condition)]),
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
                send_to_emacs(slime_connection, [':debug', id, sldb_state.level] + swank_debugger_info_for_emacs(slime_connection, sldb_state))
                send_to_emacs(slime_connection, [':debug-activate', id, sldb_state.level, False])
                while True:
                        dispatch(slime_connection, read_packet(io), sldb_state)
        finally:
                send_to_emacs(slime_connection, [':debug-return', id, sldb_state.level, False])

# readPacket <- function(io) {
#   socketSelect(list(io))
#   header <- readChunk(io, 6)
#   len <- strtoi(header, base=16)
#   payload <- readChunk(io, len)
#   readSexpFromString(payload)
# }
def read_packet(io):
        socket.select(rlist = [io.fileno()])
        header = read_chunk(io, 6)
        len = int(header, 16)
        payload = read_chunk(io, len)
        return read_sexp_from_string(payload)
                
# readChunk <- function(io, len) {
#   buffer <- readChar(io, len)
#   if(nchar(buffer) != len) {
#     stop("short read in readChunk")
#   }
#   buffer
# }
def read_chunk(io, len):
        buffer = io.read(len)
        if len(buffer) != len:
                raise Exception("short read in read_chunk")
        return

# readSexpFromString <- function(string) {
#   pos <- 1
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
#   skipWhitespace <- function() {
#     while(substr(string, pos, pos) %in% c(" ", "\t", "\n")) {
#       pos <<- pos + 1
#     }
#   }
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
        string = ""
        def write_sexp_to_string_loop(obj):
                nonlocal string
                if type(obj) is list:
                        string += '('
                        max = len(obj)
                        if max:
                                for i in range(0, max):
                                        string += write_sexp_to_string(obj[i])
                                        if i += max - 1:
                                                string += " "
                        string += ')'
                elif type(obj) is str:
                        string += '"%s"' % re.sub(r'(["\\])', r'\\\\1', obj)
                elif type(obj) in [int, float]:
                        string += str(obj)
                else:
                        raise Exception("can't write object %s" % obj)
                return string   
        return write_sexp_to_string_loop(obj)

# prin1ToString <- function(val) {
#   paste(deparse(val, backtick=TRUE, control=c("delayPromises", "keepNA")),
#         sep="", collapse="\n")
# }
def prin1_to_string(val):
        return "\n".join(deparse)

printToString <- function(val) {
  paste(capture.output(print(val)), sep="", collapse="\n")
}

`swank:connection-info` <- function (slimeConnection, sldbState) {
  list(quote(`:pid`), Sys.getpid(),
       quote(`:package`), list(quote(`:name`), "R", quote(`:prompt`), "R> "),
       quote(`:lisp-implementation`), list(quote(`:type`), "R",
                                           quote(`:name`), "R",
                                           quote(`:version`), paste(R.version$major, R.version$minor, sep=".")))
}

`swank:swank-require` <- function (slimeConnection, sldbState, contribs) {
  for(contrib in contribs) {
    filename <- sprintf("%s/%s.R", swankrPath, as.character(contrib))
    if(file.exists(filename)) {
      source(filename)
    }
  }
  list()
}

`swank:create-repl` <- function(slimeConnection, sldbState, env, ...) {
  list("R", "R")
}

makeReplResult <- function(value) {
  string <- printToString(value)
  list(quote(`:write-string`), string,
       quote(`:repl-result`))
}

makeReplResultFunction <- makeReplResult

sendReplResult <- function(slimeConnection, value) {
  result <- makeReplResultFunction(value)
  sendToEmacs(slimeConnection, result)
}

sendReplResultFunction <- sendReplResult

`swank:listener-eval` <- function(slimeConnection, sldbState, string) {
  ## O how ugly
  string <- gsub("#\\.\\(swank:lookup-presented-object-or-lose([^)]*)\\)", ".(`swank:lookup-presented-object-or-lose`(slimeConnection, sldbState,\\1))", string)
  for(expr in parse(text=string)) {
    expr <- expr
    ## O maybe this is even uglier
    lookedup <- do.call("bquote", list(expr))
    tmp <- withVisible(eval(lookedup, envir = globalenv()))
    if(tmp$visible) {
      sendReplResultFunction(slimeConnection, tmp$value)
    }
  }
  list()
}

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
def swank_backtrace(slime_connection, sldb_state, from = 0, to = None):
        calls = sldb_state.calls
        if not to:
                to = len(calls)
        from += 1
        frame_number = from - 1
        def frame_iter(x):
                nonlocal frame_number
                ret = [frame_number, str(x)]
                frame_number += 1
                return ret
        calls = map(frame_iter, calls[from:to])
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
def swank_debugger_info_for_emacs(slime_connection, sldb_state, from = 0, to = None):
        return [[str(sldb_state.condition), " [%s]" % sldb_state.condition.__type__.__name__, False],
                compute_restarts_for_emacs(sldb_state),
                swank_backtrace(slime_connection, sldb_state, from, to),
                [sldb_state.id]]

# `swank:invoke-nth-restart-for-emacs` <- function(slimeConnection, sldbState, level, n) {
#   if(sldbState$level == level) {
#     invokeRestart(sldbState$restarts[[n+1]])
#   }
# }
def swank_invoke_nth_restart_for_emacs(slime_connection, sldb_state, level, n):
        if sldb_state.level = level:
                return invoke_restart(sldb_state.restarts[n+1])

`swank:frame-source-location` <- function(slimeConnection, sldbState, n) {
  call <- sldbState$calls[[n+1]]
  srcref <- attr(call, "srcref")
  srcfile <- attr(srcref, "srcfile")
  if(is.null(srcfile)) {
    list(quote(`:error`), "no srcfile")
  } else {
    filename <- get("filename", srcfile)
    ## KLUDGE: what this means is "is the srcfile filename
    ## absolute?"
    if(substr(filename, 1, 1) == "/") {
      file <- filename
    } else {
      file <- sprintf("%s/%s", srcfile$wd, filename)
    }
    list(quote(`:location`),
         list(quote(`:file`), file),
         list(quote(`:line`), srcref[[1]], srcref[[2]]-1),
         FALSE)
  }
}

`swank:buffer-first-change` <- function(slimeConnection, sldbState, filename) {
  FALSE
}

`swank:eval-string-in-frame` <- function(slimeConnection, sldbState, string, index) {
  frame <- sldbState$frames[[1+index]]
  withRetryRestart("retry SLIME interactive evaluation request",
                   value <- eval(parse(text=string), envir=frame))
  printToString(value)
}

`swank:frame-locals-and-catch-tags` <- function(slimeConnection, sldbState, index) {
  frame <- sldbState$frames[[1+index]]
  objs <- ls(envir=frame)
  list(lapply(objs, function(name) { list(quote(`:name`), name,
                                          quote(`:id`), 0,
                                          quote(`:value`),
                                          tryCatch({
                                            printToString(eval(parse(text=name), envir=frame))
                                          }, error=function(c) {
                                            sprintf("error printing object")
                                          }))}),
       list())
}

`swank:simple-completions` <- function(slimeConnection, sldbState, prefix, package) {
  literal2rx <- function(string) {
    ## list of ERE metacharacters from ?regexp
    gsub("([.\\|()[{^$*+?])", "\\\\\\1", string)
  }
  matches <- apropos(sprintf("^%s", literal2rx(prefix)), ignore.case=FALSE)
  nmatches <- length(matches)
  if(nmatches == 0) {
    list(list(), "")
  } else {
    longest <- matches[order(nchar(matches))][1]
    while(length(grep(sprintf("^%s", literal2rx(longest)), matches)) < nmatches) {
      longest <- substr(longest, 1, nchar(longest)-1)
    }
    list(as.list(matches), longest)
  }
}

`swank:compile-string-for-emacs` <- function(slimeConnection, sldbState, string, buffer, position, filename, policy) {
  lineOffset <- charOffset <- colOffset <- NULL
  for(pos in position) {
    switch(as.character(pos[[1]]),
           `:position` = {charOffset <- pos[[2]]},
           `:line` = {lineOffset <- pos[[2]]; colOffset <- pos[[3]]},
           warning("unknown content in pos", pos))
  }
  frob <- function(refs) {
    lapply(refs,
           function(x)
           srcref(attr(x,"srcfile"),
                  c(x[1]+lineOffset-1, ifelse(x[1]==1, x[2]+colOffset-1, x[2]),
                    x[3]+lineOffset-1, ifelse(x[3]==1, x[4]+colOffset-1, x[4]),
                    ifelse(x[1]==1, x[5]+colOffset-1, x[5]),
                    ifelse(x[3]==1, x[6]+colOffset-1, x[6]))))
  }
  transformSrcrefs <- function(s) {
    srcrefs <- attr(s, "srcref")
    attribs <- attributes(s)
    new <- 
      switch(mode(s),
             "call"=as.call(lapply(s, transformSrcrefs)),
             "expression"=as.expression(lapply(s, transformSrcrefs)),
             s)
    attributes(new) <- attribs
    if(!is.null(attr(s, "srcref"))) {
      attr(new, "srcref") <- frob(srcrefs)
    }
    new
  }
  withRestarts({
    times <- system.time({
      exprs <- parse(text=string, srcfile=srcfile(filename))
      eval(transformSrcrefs(exprs), envir = globalenv()) })},
               abort="abort compilation")
  list(quote(`:compilation-result`), list(), TRUE, times[3], FALSE, FALSE)
}

withRetryRestart <- function(description, expr) {
  call <- substitute(expr)
  retry <- TRUE
  while(retry) {
    retry <- FALSE
    withRestarts(eval.parent(call),
                 retry=list(description=description,
                   handler=function() retry <<- TRUE))
  }
}

`swank:interactive-eval` <-  function(slimeConnection, sldbState, string) {
  withRetryRestart("retry SLIME interactive evaluation request",
                   tmp <- withVisible(eval(parse(text=string), envir=globalenv())))
  if(tmp$visible) {
    prin1ToString(tmp$value)
  } else {
    "# invisible value"
  }
}

`swank:eval-and-grab-output` <- function(slimeConnection, sldbState, string) {
  withRetryRestart("retry SLIME interactive evaluation request",
                   { output <-
                       capture.output(tmp <- withVisible(eval(parse(text=string),
                                                              envir=globalenv()))) })
  output <- paste(output, sep="", collapse="\n")
  if(tmp$visible) {
    list(output, prin1ToString(value))
  } else {
    list(output, "# invisible value")
  }
}

`swank:interactive-eval-region` <- function(slimeConnection, sldbState, string) {
  withRetryRestart("retry SLIME interactive evaluation request",
                   tmp <- withVisible(eval(parse(text=string), envir=globalenv())))
  if(tmp$visible) {
    prin1ToString(value)
  } else {
    "# invisible value"
  }
}

`swank:find-definitions-for-emacs` <- function(slimeConnection, sldbState, string) {
  if(exists(string, envir = globalenv())) {
    thing <- get(string, envir = globalenv())
    if(inherits(thing, "function")) {
      body <- body(thing)
      srcref <- attr(body, "srcref")
      srcfile <- attr(body, "srcfile")
      if(is.null(srcfile)) {
        list()
      } else {
        filename <- get("filename", srcfile)
        ## KLUDGE: what this means is "is the srcfile filename
        ## absolute?"
        if(substr(filename, 1, 1) == "/") {
          file <- filename
        } else {
          file <- sprintf("%s/%s", srcfile$wd, filename)
        }
        list(list(sprintf("function %s", string),
                  list(quote(`:location`),
                       list(quote(`:file`), file),
                       list(quote(`:line`), srcref[[2]][[1]], srcref[[2]][[2]]-1),
                       list())))
      }
    } else {
      list()
    }
  } else {
    list()
  }
}

`swank:value-for-editing` <- function(slimeConnection, sldbState, string) {
  paste(deparse(eval(parse(text=string), envir = globalenv()), control="all"),
        collapse="\n", sep="")
}

`swank:commit-edited-value` <- function(slimeConnection, sldbState, string, value) {
  eval(parse(text=sprintf("%s <- %s", string, value)), envir = globalenv())
  TRUE
}

resetInspector <- function(slimeConnection) {
  assign("istate", list(), envir=slimeConnection)
  assign("inspectorHistory", NULL, envir=slimeConnection)
}

`swank:init-inspector` <- function(slimeConnection, sldbState, string) {
  withRetryRestart("retry SLIME inspection request",
                   { resetInspector(slimeConnection)
                     value <- inspectObject(slimeConnection, eval(parse(text=string), envir=globalenv()))
                   })
  value
}

inspectObject <- function(slimeConnection, object) {
  previous <- slimeConnection$istate
  slimeConnection$istate <- new.env()
  slimeConnection$istate$object <- object
  slimeConnection$istate$previous <- previous
  slimeConnection$istate$content <- emacsInspect(object)
  if(!(object %in% slimeConnection$inspectorHistory)) {
    slimeConnection$inspectorHistory <- c(slimeConnection$inspectorHistory, object)
  }
  if(!is.null(slimeConnection$istate$previous)) {
    slimeConnection$istate$previous$`next` <- slimeConnection$istate
  }
  istateToElisp(slimeConnection$istate)
}

valuePart <- function(istate, object, string) {
  list(quote(`:value`),
       if(is.null(string)) printToString(object) else string,
       assignIndexInParts(object, istate))
}

preparePart <- function(istate, part) {
  if(is.character(part)) {
    list(part)
  } else {
    switch(as.character(part[[1]]),
           `:newline` = list("\n"),
           `:value` = valuePart(istate, part[[2]], part[[3]]),
           `:line` = list(printToString(part[[2]]), ": ",
             valuePart(istate, part[[3]], NULL), "\n"))
  }
}

prepareRange <- function(istate, start, end) {
  range <- istate$content[start+1:min(end+1, length(istate$content))]
  ps <- NULL
  for(part in range) {
    ps <- c(ps, preparePart(istate, part))
  }
  list(ps, if(length(ps)<end-start) { start+length(ps) } else { end+1000 },
       start, end)
}

assignIndexInParts <- function(object, istate) {
  ret <- 1+length(istate$parts)
  istate$parts <- c(istate$parts, list(object))
  ret
}

istateToElisp <- function(istate) {
  list(quote(`:title`), deparse(istate$object, control="all", nlines=1),
       quote(`:id`), assignIndexInParts(istate$object, istate),
       quote(`:content`), prepareRange(istate, 0, 500))
}

emacsInspect <- function(object) {
  UseMethod("emacsInspect")
}

emacsInspect.default <- function(thing) {
  c(list(paste("a ", class(thing)[[1]], sep=""), list(quote(`:newline`))))
}

emacsInspect.list <- function(list) {
  c(list("a list", list(quote(`:newline`))),
    mapply(function(name, value) { list(list(quote(`:line`), name, value)) },
           names(list), list))
}

emacsInspect.numeric <- function(numeric) {
  c(list("a numeric", list(quote(`:newline`))),
    mapply(function(name, value) { list(list(quote(`:line`), name, value)) },
           (1:length(numeric)), numeric))
}

`swank:quit-inspector` <- function(slimeConnection, sldbState) {
  resetInspector(slimeConnection)
  FALSE
}

`swank:inspector-nth-part` <- function(slimeConnection, sldbState, index) {
  slimeConnection$istate$parts[[index]]
}

`swank:inspect-nth-part` <- function(slimeConnection, sldbState, index) {
  object <- `swank:inspector-nth-part`(slimeConnection, sldbState, index)
  inspectObject(slimeConnection, object)
}

`swank:inspector-pop` <- function(slimeConnection, sldbState) {
  if(!is.null(slimeConnection$istate$previous)) {
    slimeConnection$istate <- slimeConnection$istate$previous
    istateToElisp(slimeConnection$istate)
  } else {
    FALSE
  }
}

`swank:inspector-next` <- function(slimeConnection, sldbState) {
  if(!is.null(slimeConnection$istate$`next`)) {
    slimeConnection$istate <- slimeConnection$istate$`next`
    istateToElisp(slimeConnection$istate)
  } else {
    FALSE
  }
}

`swank:inspector-eval` <- function(slimeConnection, sldbState, string) {
  expr <- parse(text=string)[[1]]
  object <- slimeConnection$istate$object
  if(inherits(object, "list")|inherits(object, "environment")) {
    substituted <- substituteDirect(expr, object)
    eval(substituted, envir=globalenv())
  } else {
    eval(expr, envir=globalenv())
  }
}

`swank:inspect-current-condition` <- function(slimeConnection, sldbState) {
  resetInspector(slimeConnection)
  inspectObject(slimeConnection, sldbState$condition)
}

`swank:inspect-frame-var` <- function(slimeConnection, sldbState, frame, var) {
  resetInspector(slimeConnection)
  frame <- sldbState$frames[[1+frame]]
  name <- ls(envir=frame)[[1+var]]
  object <- get(name, envir=frame)
  inspectObject(slimeConnection, object)
}

`swank:default-directory` <- function(slimeConnection, sldbState) {
  getwd()
}

`swank:set-default-directory` <- function(slimeConnection, sldbState, directory) {
  setwd(directory)
  `swank:default-directory`(slimeConnection, sldbState)
}

`swank:load-file` <- function(slimeConnection, sldbState, filename) {
  source(filename, local=FALSE, keep.source=TRUE)
  TRUE
}

`swank:compile-file-for-emacs` <- function(slimeConnection, sldbState, filename, loadp, ...) {
  times <- system.time(parse(filename, srcfile=srcfile(filename)))
  if(loadp) {
    ## KLUDGE: inelegant, but works.  It might be more in the spirit
    ## of things to keep the result of the parse above around to
    ## evaluate.
    `swank:load-file`(slimeConnection, sldbState, filename)
  }
  list(quote(`:compilation-result`), list(), TRUE, times[3], substitute(loadp), filename)
}

`swank:quit-lisp` <- function(slimeConnection, sldbState) {
  quit()
}
