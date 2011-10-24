REPORT ?= nil

all: run

run:	clean
	python3 -c "from cl import *; import cl, partus; cl.setq('_presignal_hook_', cl._report_condition if $(REPORT) else nil); partus.create_server()"

clean:
	rm -rf __pycache__

test:
	python3 cl-tests.py
