#!/usr/bin/env python3

import cl
from cl import *

version           = "0.0.1"
vpcl_install_root = "/home/deepfire/src/partus/"
sicl_source_root  = "/home/deepfire/src/sicl/Code/"

def VPCL(file):
        return vpcl_install_root + file

def SICL(file):
        return sicl_source_root + file

def lcf(locator, file):
        def pseudo_compile_file_pathname(x):
                pieces = x.split(".")
                assert pieces[-1] in {"lisp", "lsp"}
                return ".".join(pieces[:-1]) + '.vpfas'
        compiled_file = pseudo_compile_file_pathname(file)
        if not probe_file(locator(compiled_file)) or file_write_date(locator(file)) > file_write_date(locator(compiled_file)):
                compile_file(locator(file))
        load(locator(compiled_file))

# print("This is VPCL %s, a piece of code mockingly approaching to an implementation of ANSI Common Lisp.\n" % version)

for kind, s in [(VPCL, "vpcl.lisp"),
                (VPCL, "src/array.lisp"),
                (VPCL, "src/defstruct.lisp"),
                (VPCL, "src/hash-table.lisp"),
                # (VPCL, "src/package.lisp"),
                (VPCL, "src/setf.lisp"),
                # (SICL, "Reader/float.lisp"),
                # (SICL, "Reader/read.lisp"),
                ]:
        lcf(kind, s)

dprintf("; *PACKAGE* is %s", symbol_value(cl._package_))

repl()
