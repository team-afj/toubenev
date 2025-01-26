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
	time sh run.sh

.PHONY: watch
watch: venv
	. .venv/bin/activate; \
	sh watch.sh

.PHONY: gapi-test
gapi-test: venv
	. .venv/bin/activate; \
	python import_gapi.py

.PHONY: upgrade
upgrade: venv
	. .venv/bin/activate; sed -i '' 's/[~=]=/>=/' requirements.txt
	. .venv/bin/activate; pip install -U -r requirements.txt
	. .venv/bin/activate; pip freeze | sed 's/==/~=/' > requirements.txt

.PHONY: web-dev
web-dev:
	cd web && yarn serve
