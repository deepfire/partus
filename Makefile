#PYTHON ?= python3
PYTHON ?= python3
TEST   ?= t
TEST_QQ       ?= nil
TEST_METASEX  ?= nil
TEST_KNOWN    ?= nil
TEST_PP       ?= nil
TEST_COMPILER ?= nil
REPORT ?= nil
DEBUG  ?= nil

RECOMPILE_VPCL ?= nil

DUMP_FORM ?= nil
DUMP_MX   ?= nil
DUMP_RE   ?= nil
DUMP_PRIM ?= nil
DUMP_AST  ?= nil

all: run

vpcl.vpfas: cl.py vpcl.lisp
	$(PYTHON) -c "from cl import *; dbgsetup(forms = $(DUMP_FORM), macroexpanded = $(DUMP_MX), rewritten = $(DUMP_RE), primitives = $(DUMP_PRIM), module_ast = $(DUMP_AST)); compile_file('vpcl.lisp')"

repl: vpcl.vpfas
	$(PYTHON) -ic "from cl import *; load('vpcl.vpfas'); repl()"

test: vpcl.vpfas
	export CL_RUN_TESTS=$(TEST) CL_TEST_QQ=$(TEST_QQ) CL_TEST_METASEX=$(TEST_METASEX) CL_TEST_KNOWN=$(TEST_KNOWN) CL_TEST_PP=$(TEST_PP) CL_TEST_COMPILER=$(TEST_COMPILER); \
	$(PYTHON) -c \
"from cl import *; in_package('CL'); load('vpcl.vpfas'); dbgsetup(forms = $(DUMP_FORM), macroexpanded = $(DUMP_MX), rewritten = $(DUMP_RE), primitives = $(DUMP_PRIM), module_ast = $(DUMP_AST)); load('reader.lisp', verbose = t, print = t)"


clean:
	rm -rf __pycache__

merge:
	git diff HEAD^1 cl.py > d.diff
	patch --merge cl.org d.diff; rm -f d.diff cl.org.orig


## Undermaintained targets
run:	clean
# (lambda c, f, h: format(sys.stdout, 'Passing condition \'%s\' of type \'%s\' to frame:\n%s\n', c, type_of(c), cl._pp_frame(f, lineno = t)))
	$(PYTHON) -c "from cl import *; import cl, partus, sys; setq('_presignal_hook_', (lambda c, h: cl._report_condition(c, backtrace = t)) if $(REPORT) else nil); setq('_prehandler_hook_', (cl._report_handling_handover) if $(REPORT) else nil); setq('_debug_on_swank_protocol_error_', $(DEBUG)); setq('_debug_swank_backend_', $(DEBUG)); partus.create_server()"

swank:
	sbcl --eval '(require :swank)' --eval '(swank:create-server :dont-close t)'

smalltest:
	$(PYTHON) cl-tests.py
