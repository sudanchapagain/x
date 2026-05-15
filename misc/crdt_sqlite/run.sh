#!/usr/bin/env sh

nix-shell -p sqlite.dev --run "mkdir -p bin && clang src/main.c -lsqlite3 -o bin/cr && ./bin/cr"
