.PHONY: all test check TAGS


all: check test TAGS

test:
	python3 -m unittest discover -s interpreter -p '*_test.py'

check:
	mypy main.py interpreter/

TAGS:
	ctags -e -R .
