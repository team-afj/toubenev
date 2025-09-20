.PHONY: default
default: all

.PHONY: all
all:
	uv run time sh run.sh

.PHONY: watch
watch:
	uv run sh watch.sh

.PHONY: gapi-test
gapi-test:
	uv run python import_gapi.py

.PHONY: web-dev
web-dev:
	cd web && yarn serve
