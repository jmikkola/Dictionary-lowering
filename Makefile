.PHONY: all test check TAGS pylint pyflakes coverage installdeps watch


all: TAGS check test

test:
	python3 -m unittest discover -s interpreter -p '*_test.py'

check:
	mypy main.py interpreter/

TAGS:
	ctags -e -R .

pylint:
	pylint -E interpreter/

pyflakes:
	pyflakes interpreter/

coverage:
	coverage run -m unittest discover  -s interpreter -p '*_test.py' && coverage html

installdeps:
	python3 -m pip install coverage mypy pylint pyflakes

watch:
	./watch.sh
