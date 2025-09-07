#!/bin/bash

./toubenev.exe &
fswatch -o toubenev.exe -l 2 | xargs -L1 bash -c \
  "killall toubenev.exe || true; (./toubenev.exe || true) &"