SUBDIRS = module lib

js_runnerdir = $(pkgdatadir)/$(VERSION)/js-runner
js_runner_DATA = \
  js-runner/await-call.js \
  js-runner/call.js \
  js-runner/load.js \
  js-runner/load-async.js \
  js-runner/load-primitive.js

reflect_jsdir = $(pkgdatadir)/$(VERSION)/reflect-js
reflect_js_DATA = \
  reflect-js/reflect.js

reflect_wasmdir = $(pkgdatadir)/$(VERSION)/reflect-wasm
REFLECT_WAT = \
  reflect-wasm/reflect.wat \
  reflect-wasm/wtf8.wat
reflect_wasm_DATA = $(REFLECT_WAT:%.wat=%.wasm)
CLEANFILES = $(reflect_wasm_DATA)
SUFFIXES = .wat .wasm

# The mkdir is necessary for 'make distcheck' since it's building in a
# different directory than the project root.
.wat.wasm:
	$(AM_V_GEN)$(MKDIR_P) $(dir $@) && \
	  $(top_builddir)/pre-inst-env $(GUILD) assemble-wasm -o "$@" "$<"


$(top_builddir)/reflect-wasm/reflect.wasm: reflect-wasm/reflect.wat module/hoot/stdlib.scm
	$(AM_V_GEN)$(MKDIR_P) $(dir $@) && \
	  $(top_builddir)/pre-inst-env $(GUILD) assemble-wasm \
	    --stdlib="((@ (hoot stdlib) compute-stdlib) #t)" -o "$@" "$<"

TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = $(top_builddir)/pre-inst-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile -L $(top_srcdir)
JS_LOG_COMPILER = $(top_builddir)/js-test
CLEANFILES += $(TESTS:test/%.scm=%.log)

TESTS = 					\
  test/test-assignment.scm			\
  test/test-atomics.scm				\
  test/test-bitwise.scm				\
  test/test-bytevectors.scm			\
  test/test-char-prelude.scm			\
  test/test-complex.scm				\
  test/test-constants.scm			\
  test/test-disassemble.scm			\
  test/test-division.scm			\
  test/test-environments.scm			\
  test/test-eval.scm				\
  test/test-expander.scm			\
  test/test-exceptions.scm			\
  test/test-expander.scm			\
  test/test-ffi.scm				\
  test/test-fibers.scm				\
  test/test-fibers-channels.scm			\
  test/test-fibers-conditions.scm		\
  test/test-fibers-operations.scm		\
  test/test-fibers-scheduler.scm		\
  test/test-fibers-streams.scm			\
  test/test-fibers-timers.scm			\
  test/test-fibers-waiter-queue.scm		\
  test/test-finalization.scm			\
  test/test-flonums.scm				\
  test/test-fluids.scm				\
  test/test-fractions.scm			\
  test/test-hash.scm				\
  test/test-hashtables.scm			\
  test/test-hashtables-doubly-weak.scm		\
  test/test-hashtables-guile.scm		\
  test/test-hashtables-weak-key.scm		\
  test/test-hashtables-weak-value.scm		\
  test/test-inline-wasm.scm			\
  test/test-ints.scm				\
  test/test-keywords.scm			\
  test/test-library-group.scm			\
  test/test-lower-globals.scm			\
  test/test-lower-stringrefs.scm		\
  test/test-lower.scm				\
  test/test-match.scm				\
  test/test-modules.scm				\
  test/test-number-to-string.scm		\
  test/test-numeric.scm                         \
  test/test-pairs.scm				\
  test/test-ports.scm				\
  test/test-procedures.scm			\
  test/test-promises.scm			\
  test/test-prompts.scm				\
  test/test-read.scm				\
  test/test-records.scm				\
  test/test-regexps.scm				\
  test/test-string-to-number.scm		\
  test/test-strings.scm				\
  test/test-symbols.scm				\
  test/test-syntax-objects.scm			\
  test/test-time.scm				\
  test/test-tree-il.scm				\
  test/test-vectors.scm				\
  test/test-vm.scm				\
  test/test-wasm-assembler.scm			\
  test/test-weak-vectors.scm			\
  test/test-write.scm				\
  #

info_TEXINFOS = doc/hoot.texi
doc_hoot_TEXINFOS = doc/hoot.texi 		\
                    doc/apache-2.0.texi

dvi: # Don't build dvi docs
html-local:
	$(GUILE) --no-auto-compile doc/build-html.scm

EXTRA_DIST = 					\
  $(REFLECT_WAT)				\
  $(TESTS)					\
  $(EXTRA_WAT)					\
  $(js_runner_DATA)				\
  $(reflect_js_DATA)				\
  bin/eval.scm					\
  bin/call.scm					\
  bin/compile.scm				\
  bin/generate-char-prelude.scm			\
  bootstrap.sh					\
  guix.scm					\
  COPYING					\
  hoot.png					\
  README.md					\
  test/basic-types.wat				\
  test/basic-types.wasm				\
  test/utils.scm				\
  test/hashtable-utils.scm			\
  test/fixtures/hello				\
  doc/build-html.scm				\
  doc/hoot.css					\
  examples/project-template/hello.js		\
  examples/project-template/hello.scm		\
  examples/project-template/index.html		\
  examples/project-template/Makefile		\
  examples/project-template/manifest.scm	\
  examples/project-template/hello/document.scm	\
  examples/project-template/hello/element.scm	\
  examples/project-template/README.md		\
  #
