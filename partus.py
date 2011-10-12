import traceback
import io
import os
import sys
import socket
import re

from cl import *
from pergamum import *
from more_ast import *

import swank
from swank import env

###
### Potemkin shit.
###
def not_implemented(mesg):
        raise Exception("ERROR: not implemented: " + mesg)
def with_restarts(fn, **restarts):
        # not_implemented("with_restarts()")
        return fn()

###
### Load-code-object-as-module, the.
###
import imp
def load_code_object_as_module(name, x, parent_package = None, built_ins = None, packagep = None, filename = 'garbage'):
        if not code_object_p(x):
                error('In %s: while loading module "%s", argument is a "%s", not a code object.', 'load_code_object_as_module', name, type(x).__name__)
        mod = imp.new_module(name)
        mod.__filename__ = filename
        if packagep:
                mod.__path__ = (parent_package.__path__ if parent_package else []) + [ names.parse_namestring(name)[-1] ]
        sys.modules[name] = mod
        if built_ins:
                mod.__dict__['__builtins__'] = built_ins
        exec(x, mod.__dict__, mod.__dict__)
        if parent_package:
                dotpos = name.rindex('.')
                assert (dotpos)
                postdot_name = name[dotpos + 1:]
                setattr(parent_package, postdot_name, mod)
                parent_package.__children__.add(mod)
                mod.__parent__ = parent_package
        if packagep:
                mod.__children__ = set([])
        return mod

# swank <- function(port=4005) {
#  acceptConnections(port, FALSE)
# }
def start_swank(port = 4005):      accept_connections(port, False)
# startSwank <- function(portFile) {
#  acceptConnections(4005, portFile)
# }
def start_swank_from_file(port_file):  accept_connections(4005, port_file)

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
        swank.init_package_system()
        debug_printf("waiting for clients..")
        c, a = s.accept()
        debug_printf("serving connection from %s", a)
        serve(c, c.makefile(mode = "rw"))

# serve <- function(io) {
#  mainLoop(io)
# }
def serve(sock, file):
        debug_printf("serve: sock = %s, file = %s", sock, file)
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
        with env.let(slime_connection = SlimeConnection(sock = sock, file = file, io = sock),
                     python_user      = load_code_object_as_module("python_user",
                                                                   compile("import swank; import cl;" + ("from swank import *" if swank.debug else ""),
                                                                           "PY-USER", "exec")),
                     partus_path      = os.getcwd()):
                while True:
                        def with_restarts_body():
                                try:
                                        with env.let(sldb_state = None):
                                                swank.dispatch(env.slime_connection, swank.read_packet(sock, file), sldb_state = env.sldb_state)
                                except Exception as x: # FIXME
                                        # print_backtrace()
                                        not_implemented("Unhandled exception at main loop.")
                                        swank_top_level = lambda c: None
                        with_restarts(with_restarts_body,
                                      abort = "return to SLIME's toplevel")
