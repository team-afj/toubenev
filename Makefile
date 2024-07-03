.PHONY: default
default: all

.PHONY: venv
venv: .venv/touchfile

.venv/touchfile: requirements.txt
	python3 -m venv .venv
	. .venv/bin/activate; python -m pip install -r requirements.txt
	touch .venv/touchfile

.PHONY: all
all: venv
	. .venv/bin/activate; \
	sh run.sh

.PHONY: watch
watch: venv
	. .venv/bin/activate; \
	sh watch.sh
