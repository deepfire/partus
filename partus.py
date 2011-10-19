import traceback
import io
import os
import sys
import socket
import re

from cl import *
from cl import _keyword

from pergamum import *
from more_ast import *

import swank
from swank import *

from swank_backend import *

# Python silently clobbers on FROM x IMPORT *
# 'inspect' unable to get source of funtions entered through REPL:
#   def foo(): pass; getsource(foo)... IOError: source code not available

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

### TCP Server: swank.lisp:769
setq('_use_dedicated_output_stream_',       None)
setq('_dedicated_output_stream_port_',      0)
setq('_communication_style_',               preferred_communication_style())
setq('_dont_close_',                        None)
setq('_dedicated_output_stream_buffering_', ("full"
                                             if symbol_value("_communication_style_") is _keyword("spawn") else
                                             "none"))
setq('_coding_system_',                     "utf-8")
setq('_listener_sockets_',                  dict())
setq('_loopback_interface_',                "127.0.0.1")
setq('_default_server_port_',               4005)

def start_server(port_file,
                 style         = symbol_value('_communication_style_'),
                 dont_close    = symbol_value('_dont_close_'),
                 coding_system = symbol_value('_coding_system_')):
        setup_server(0,
                     lambda port: announce_server_port(port_file, port),
                     style, dont_close, coding_system)

def create_server(port          = symbol_value("_default_server_port_"),
                  style         = symbol_value('_communication_style_'),
                  dont_close    = symbol_value('_dont_close_'),
                  coding_system = symbol_value('_coding_system_')):
        setup_server(port, simple_announce_function,
                     style, dont_close, coding_system)

def find_external_format_or_lose(fmt):
        assert(fmt in ['utf-8'])
        return fmt

def setup_server(port, announce_fn, style, dont_close, coding_system):
        assert(functionp(announce_fn))
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
        if style is _keyword('spawn'):
                initialize_multiprocessing(
                        lambda:
                                spawn(lambda: (loop_serve() if dont_close else
                                               serve())))
        elif style is _keyword("fd-handler"):
                add_fd_handler(socket, lambda: serve())
        else:
                while symbol_value("_dont_close_"):
                        loop_serve()
        symbol_value("_listener_sockets_")[port] = [style, socket]
        return port

def stop_server(port):
        style, socket = symbol_value("_listener_sockets_")[port]
        if style is _keyword("spawn"):
                thread_position = position_if(lambda x: x[1] == "Swank %s" % port, list_threads())
                if thread_position:
                        kill_nth_thread(thread_position - 1)
                        close_socket(socket)
                        # XXX: REMHASH
                        del symbol_value("_listener_sockets_")[port]
        elif style is _keyword("fd-handler"):
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
        client = unwind_protect(lambda: accept_connection(socket, external_format = ef),
                                lambda: None if dont_close else close_socket(socket))
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
        if os.path.exists(secret_path):
                with open(secret_path, "r") as f:
                        return f and read_line(f, nil, "")

def serve_requests(connection):
        connection.serve-requests(connection)

def announce_server_port(file, port):
        with open(file, "w") as s:
                format(s, "%s\n", port)
        simple_announce_function(port)

def simple_announce_function(port):
        if symbol_value("_swank_debug_p_"):
                format(symbol_value("_log_output_"), "\n;; Swank started at port: ~%d.\n", port)
                force_output(symbol_value("_log_output_"))

def open_streams(connection):
        """Return the 5 streams for IO redirection:
DEDICATED-OUTPUT INPUT OUTPUT IO REPL-RESULTS"""
        input_fn = lambda: with_connection(connection,
                                           lambda: with_simple_restart("ABORT-READ", "Abort reading input from Emacs.",
                                                                       read_user_input_from_emacs))
        dedicated_output = when(symbol_value("_swank_debug_p_"),
                                lambda: open_dedicated_output_stream(connection.socket_io))
        in_ = make_input_stream(input_fn)
        out = dedicated_output or make_output_stream(make_output_function(connection))
        io = make_two_way_stream(in_, out)
        repl_results = make_output_stream_for_target(connection, _keyword("repl-result"))
        if connection.communication_style is _keyword("spawn"):
                connection.set_auto_flush_thread(spawn(lambda: auto_flush_loop(out),
                                                       name = "auto-flush-thread"))
        return dedicated_output, in_, out, io, repl_results

def make_output_function(connection):
        i, tag, l = 0, 0, 0
        def set_i_tag_l(x): nonlocal i, tag, l; i, tag, l = x
        return (lambda string:
                        with_connection(connection,
                                        set_i_tag_l(send_user_output(string, i, tag, l))))

setq("_maximum_pipelined_output_chunks_", 50)
setq("_maximum_pipelined_output_length_", 80 * 20 * 5)

def send_user_output(string, pcount, tag, plength):
        if (pcount  > symbol_value("_maximum_pipelined_output_chunks_") or
            plength > symbol_value("_maximum_pipelined_output_length_")):
                tag = (tag + 1) % 1000
                send_to_emacs([_keyword("ping"), current_thread_id(), tag])
                with_simple_restart("ABORT", "Abort sending output to Emacs.")
                wait_for_event([_keyword("emacs-pong", tag)])
                pcount, plength = 0
        send_to_emacs([_keyword("write-string"), string])
        return pcount + 1, tag, plength + len(string)

def make_output_function_for_target(connection, target):
        "Create a function to send user output to a specific TARGET in Emacs."
        return (lambda string:
                        with_connection(connection,
                                        lambda: with_simple_restart("ABORT", "Abort sending output to Emacs.",
                                                                    lambda: send_to_emacs([_keyword("write-string", string, target)]))))

def make_output_stream_for_target(connection, target):
        return make_output_stream(make_output_function_for_target(connection, target))

def open_dedicated_output_stream(socket_io):
        """Open a dedicated output connection to the Emacs on SOCKET-IO.
Return an output stream suitable for writing program output.

This is an optimized way for Lisp to deliver output to Emacs."""
        socket = create_socket(symbol_value('_loopback_interface_'),
                               symbol_value('_dedicated_output_stream_port_'))
        try:
                port = local_port(socket)
                encode_message([_keyword("open-dedicated-output-stream"), port], socket_io)
                dedicated = accept_connection(socket,
                                              external_format = ignore_errors(stream_external_format(socket_io)) or "default", # was: _keyword("default")
                                              buffering = symbol_value('_dedicated_output_stream_buffering_'),
                                              timeout = 30)
                close_socket(socket)
                socket = None
                return dedicated
        finally:
                if socket:
                        close_socket(socket)

def main_loop(sock, file):
        with env.let(slime_connection = SlimeConnection(sock = sock, file = file, io = sock),
                     python_user      = load_code_object_as_module("python_user",
                                                                   compile("import swank; import cl;" + ("from swank import *" if swank.debug else ""),
                                                                           "PY-USER", "exec")),
                     partus_path      = default_directory()):
                while True:
                        def with_restarts_body():
                                try:
                                        with env.let(sldb_state = None):
                                                swank.dispatch_event(env.slime_connection, swank.read_packet(sock, file), sldb_state = env.sldb_state)
                                except Exception as x: # FIXME
                                        # print_backtrace()
                                        not_implemented("Unhandled exception at main loop.")
                                        swank_top_level = lambda c: None
                        with_simple_restart("ABORT", "Return to SLIME's toplevel.",
                                            with_restarts_body)
