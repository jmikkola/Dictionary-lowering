.PHONY: all test check


all: check test

test:
	python3 -m unittest discover -s interpreter -p '*_test.py'

check:
	mypy main.py interpreter/
