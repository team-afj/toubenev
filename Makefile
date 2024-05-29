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
	cd planning_bras_croises_v0/Bras\ Croisés\ 2024\ 3ca967a4c110493eb994982dbd5812e9 && \
	python ../../lbc24.py \
	Lieux\ 8c24227c43ae4c7dbe61104e014e4baf_all.csv \
	Type\ de\ Quete\ efe9868d9cca4ecca88371c125c2c90c_all.csv \
	Bénévoles\ 141ab917b31a454c914099491a60ea8c_all.csv \
	Quetes\ cedaeddb132f4374bf1ca862bfa2911e_all.csv
