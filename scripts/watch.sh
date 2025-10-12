#!/bin/sh

./toubenev.exe &

fswatch -o toubenev.exe www/** www -l 1 | xargs -L1 bash -c \
	"killall toubenev.exe || true; \
	(CAQTI_DEBUG_PARAM=true ./toubenev.exe || export TBN_PID=$! || true) &"

echo "Killing toubenev"
killall toubenev.exe
