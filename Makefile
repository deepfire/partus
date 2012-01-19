REPORT ?= nil
DEBUG  ?= nil

all: run

run:	clean
# (lambda c, f, h: format(sys.stdout, 'Passing condition \'%s\' of type \'%s\' to frame:\n%s\n', c, type_of(c), cl._pp_frame(f, lineno = t)))
	python3 -c "from cl import *; import cl, partus, sys; setq('_presignal_hook_', (lambda c, h: cl._report_condition(c, backtrace = t)) if $(REPORT) else nil); setq('_prehandler_hook_', (cl._report_handling_handover) if $(REPORT) else nil); setq('_debug_on_swank_protocol_error_', $(DEBUG)); setq('_debug_swank_backend_', $(DEBUG)); partus.create_server()"

swank:
	sbcl --eval '(require :swank)' --eval '(swank:create-server :dont-close t)'

clean:
	rm -rf __pycache__

test:
	python3 -c "from cl import *; in_package('CL'); load('../informatimago/common-lisp/lisp-reader/reader.lisp', verbose =t, print = t)"
smalltest:
	python3 cl-tests.py
