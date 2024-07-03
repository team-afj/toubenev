#!/bin/sh

fswatch -o lbc24.py | xargs -n1 -I{} sh run.sh
