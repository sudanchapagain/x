repl.wasm: repl.scm Makefile
	../../pre-inst-env guild compile-wasm -g1 -o $@ $<

run-guile: repl.wasm
	../../pre-inst-env guile --no-auto-compile boot-repl.scm

run-node: repl.wasm
	node boot-repl.js

clean:
	rm -f repl.wasm

.PHONY: clean run-guile run-node
