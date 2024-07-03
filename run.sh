#!/bin/sh

cd planning_bras_croises_v0/data && \
python ../../lbc24.py \
Lieux*.csv \
Type*.csv \
Bénévoles*.csv \
Quetes*.csv
