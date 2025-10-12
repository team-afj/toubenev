#!/bin/sh

opam exec -- dune build @ocaml-index @app --profile=dev --watch --terminal-persistence=preserve &
DUNE_PID=$!

./scripts/watch.sh &
SERVER_PID=$!

stop() {
 echo '\n' Killing servers;
 killall toubenev.exe
 kill ${DUNE_PID}
 kill ${SERVER_PID}
 exit
}
trap stop SIGINT

echo "Wait for watcher kill"

wait ${SERVER_PID}
kill ${DUNE_PID}

