import traceback
import io
import os
import sys
import socket
import re

import cl

from cl import *
from cl import _keyword as keyword

from pergamum import *
from more_ast import *

import swank
from swank import *

from swank_backend import *

# Python silently clobbers on FROM x IMPORT *
# 'inspect' unable to get source of funtions entered through REPL:
#   def foo(): pass; getsource(foo)... IOError: source code not available

def _init_swank_packages():
        "Hooked."
        defpackage("PERGAMUM",   use = ["CL"])
        defpackage("MORE-AST",   use = ["CL", "PERGAMUM"])
        defpackage("SWANK",      use = ["CL", "PERGAMUM", "MORE-AST"])
        swank = find_package("SWANK")

        import inspector
        package("INSPECTOR",     use = ["CL", "PERGAMUM", "SWANK"])
        inspector.nil_surrogate = cl._intern0("nil_surrogate", "INSPECTOR")
        # WARNING: circular package use!
        use_package(swank, "INSPECTOR")

        inspector_syms = [
                "inspect_object", "lookup_presented_object"
                ]
        import_(mapcar(lambda s: cl._find_symbol0(s, "INSPECTOR"), inspector_syms),
                "SWANK")

### TCP Server: swank.lisp:769
setq('_use_dedicated_output_stream_',       None)
setq('_dedicated_output_stream_port_',      0)
setq('_communication_style_',               preferred_communication_style())
setq('_dont_close_',                        None)
setq('_dedicated_output_stream_buffering_', ("full"
                                             if symbol_value("_communication_style_") is keyword("spawn") else
                                             "none"))
setq('_coding_system_',                     "utf-8")
setq('_listener_sockets_',                  dict())
setq('_default_server_port_',               4005)

defvar("_shutdown_", nil) # XXX: python-specific invention..
def honoring_death_wish():
        while True:
                if symbol_value("_shutdown_"):
                        sys.exit()
                try:
                        sleep(1)
                except KeyboardInterrupt:
                        sys.exit()

__coding_systems__ = dict([("utf-8-unix", "utf-8")])
def _xlate_coding_system(x):
        return gethash(x, __coding_systems__, x)[0]

def start_server(port_file,
                 style         = symbol_value('_communication_style_'),
                 dont_close    = symbol_value('_dont_close_'),
                 coding_system = symbol_value('_coding_system_')):
        setup_server(0,
                     lambda port: announce_server_port(port_file, port),
                     style, dont_close,
                     _xlate_coding_system(coding_system))
        honoring_death_wish()

def create_server(port          = symbol_value("_default_server_port_"),
                  style         = symbol_value('_communication_style_'),
                  dont_close    = symbol_value('_dont_close_'),
                  coding_system = symbol_value('_coding_system_')):
        setup_server(port,
                     simple_announce_function,
                     style, dont_close,
                     _xlate_coding_system(coding_system))
        honoring_death_wish()

def find_external_format_or_lose(fmt):
        return find_external_format(fmt) or error()

setq('_loopback_interface_',                "127.0.0.1")

def setup_server(port, announce_fn, style, dont_close, coding_system):
        assert(functionp(announce_fn))
        _init_swank_packages()
        init_log_output()
        find_external_format_or_lose(coding_system)
        socket     = create_socket(symbol_value('_loopback_interface_'), port)
        port = local_port(socket)
        announce_fn(port)
        def serve():
                accept_connections(socket, style, dont_close, coding_system)
        def loop_serve():
                while True:
                        ignore_errors(serve)
        if style is keyword('spawn'):
                initialize_multiprocessing(
                        lambda:
                                spawn(lambda: (loop_serve() if dont_close else
                                               serve())))
        elif style is keyword("fd-handler"):
                add_fd_handler(socket, lambda: serve())
        else:
                while symbol_value("_dont_close_"):
                        loop_serve()
        symbol_value("_listener_sockets_")[port] = [style, socket]
        return port

def stop_server(port):
        style, socket = symbol_value("_listener_sockets_")[port]
        if style is keyword("spawn"):
                thread_position = position_if(lambda x: x[1] == "Swank %s" % port, list_threads())
                if thread_position:
                        kill_nth_thread(thread_position - 1)
                        close_socket(socket)
                        # XXX: REMHASH
                        del symbol_value("_listener_sockets_")[port]
        elif style is keyword("fd-handler"):
                remove_fd_handlers(socket)
                close_socket(socket)
                # XXX: REMHASH
                del symbol_value("_listener_sockets_")[port]

def restart_server(port          = symbol_value("_default_server_port_"),
                   style         = symbol_value('_communication_style_'),
                   dont_close    = symbol_value('_dont_close_'),
                   coding_system = symbol_value('_coding_system_')):
        stop_server(port)
        sleep(5)
        create_server(port = port, style = style, dont_close = dont_close, coding_system = coding_system)

def accept_connections(socket, style, dont_close, coding_system):
        ef = find_external_format_or_lose(coding_system)
        try:
                client = accept_connection(socket, external_format = ef)
        finally:
                if not dont_close:
                        close_socket(socket)
        authenticate_client(client)
        serve_requests(make_connection(socket, client, style, coding_system))

def authenticate_client(stream):
        secret = slime_secret()
        if secret:
                set_stream_timeout(stream, 20)
                first_val = decode_message(stream)
                if not (stringp(first_val) and first_val == secret):
                        error("Incoming connection doesn't know the password.")
                set_stream_timeout(stream, None)

def slime_secret():
        secret_path = os.path.join(user_homedir_pathname(), ".slime-secret")
        if probe_file(secret_path):
                with open(secret_path, "r") as f:
                        return f and read_line(f, nil, "")

def serve_requests(conn):
        conn.serve_requests(conn)

def announce_server_port(file, port):
        with open(file, "w+") as s:
                format(s, "%s\n", port)
                finish_output(s)
        simple_announce_function(port)

def simple_announce_function(port):
        if symbol_value("_swank_debug_p_"):
                format(symbol_value("_log_output_"), "\n;; Swank started at port: %d.\n", port)
                force_output(symbol_value("_log_output_"))
