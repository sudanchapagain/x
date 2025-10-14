hello.wasm: hello.scm
	guild compile-wasm -L . -o $@ $<

serve: hello.wasm
	guile -c '((@ (hoot web-server) serve))'
