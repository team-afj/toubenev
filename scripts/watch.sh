#!/bin/sh

./toubenev.exe &

fswatch -o toubenev.exe www/** www -l 1 | xargs -L1 bash -c \
	"killall toubenev.exe || true; \
	(echo "" && CAQTI_DEBUG_PARAM=true ./toubenev.exe || true) &"

echo "Killing toubenev"
killall toubenev.exe
