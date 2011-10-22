all:
	rm -rf __pycache__
	python3 -c "import partus; partus.create_server()"
