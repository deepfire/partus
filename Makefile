REPORT ?= nil

all: run

run:	clean
# (lambda c, f, h: format(sys.stdout, 'Passing condition \'%s\' of type \'%s\' to frame:\n%s\n', c, type_of(c), cl._pp_frame(f, lineno = t)))
	python3 -c "from cl import *; import cl, partus, sys; setq('_presignal_hook_', (lambda c, h: cl._report_condition(c)) if $(REPORT) else nil); setq('_prehandler_hook_', (lambda *args: write_line('YIPPEE: %s!' % (args,)) and quit()) if $(REPORT) else nil); partus.create_server()"

clean:
	rm -rf __pycache__

test:
	python3 cl-tests.py
