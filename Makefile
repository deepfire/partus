REPORT ?= nil

all: run

run:	clean
	python3 -c "from cl import *; import cl, partus; cl._maybe_reporting_conditions_on_hook($(REPORT), '_debugger_hook_', partus.create_server)"

clean:
	rm -rf __pycache__

test:
	python3 cl-tests.py
