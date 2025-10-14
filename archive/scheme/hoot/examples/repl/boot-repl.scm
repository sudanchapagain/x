(use-modules (hoot reflect)
             (wasm parse))

(define wasm (call-with-input-file "repl.wasm" parse-wasm))
(define module (hoot-instantiate wasm))
(define proc (hoot-load module))
(hoot-apply-async proc)
