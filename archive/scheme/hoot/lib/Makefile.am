SOURCES = 					\
  fibers.scm					\
  fibers/channels.scm				\
  fibers/conditions.scm				\
  fibers/operations.scm				\
  fibers/promises.scm				\
  fibers/scheduler.scm				\
  fibers/streams.scm				\
  fibers/timers.scm				\
  fibers/waiter-queue.scm			\
  guile.scm					\
  hoot/apply.scm				\
  hoot/assoc.scm				\
  hoot/atomics.scm				\
  hoot/bitvectors.scm				\
  hoot/bitwise.scm				\
  hoot/boxes.scm				\
  hoot/bytevectors.scm				\
  hoot/char.scm					\
  hoot/cond-expand.scm				\
  hoot/control.scm				\
  hoot/core-syntax.scm				\
  hoot/core-syntax-helpers.scm			\
  hoot/cross-compilation.scm			\
  hoot/debug.scm				\
  hoot/dynamic-states.scm			\
  hoot/dynamic-wind.scm				\
  hoot/environments.scm				\
  hoot/eq.scm					\
  hoot/equal.scm				\
  hoot/error-handling.scm			\
  hoot/errors.scm				\
  hoot/eval.scm					\
  hoot/exceptions.scm				\
  hoot/expander.scm				\
  hoot/external.scm				\
  hoot/ffi.scm					\
  hoot/finalization.scm				\
  hoot/fluids.scm				\
  hoot/gensym.scm				\
  hoot/hackable.scm				\
  hoot/hashtables.scm				\
  hoot/inline-wasm.scm				\
  hoot/interaction-environment.scm		\
  hoot/keywords.scm				\
  hoot/lists.scm				\
  hoot/match.scm				\
  hoot/modules.scm				\
  hoot/not.scm					\
  hoot/numbers.scm				\
  hoot/pairs.scm				\
  hoot/parameters.scm				\
  hoot/ports.scm				\
  hoot/primitive-eval.scm			\
  hoot/primitives-module.scm			\
  hoot/procedures.scm				\
  hoot/read.scm					\
  hoot/records.scm				\
  hoot/regexps.scm				\
  hoot/strings.scm				\
  hoot/symbols.scm				\
  hoot/syntax.scm				\
  hoot/syntax-objects.scm			\
  hoot/syntax-transformers.scm			\
  hoot/time.scm					\
  hoot/tree-il.scm				\
  hoot/values.scm				\
  hoot/vectors.scm				\
  hoot/weak-refs.scm				\
  hoot/weak-vectors.scm				\
  hoot/write.scm				\
  ice-9/atomic.scm				\
  ice-9/binary-ports.scm			\
  ice-9/control.scm				\
  ice-9/custom-ports.scm			\
  ice-9/exceptions.scm				\
  ice-9/i18n.scm				\
  ice-9/match.scm				\
  ice-9/optargs.scm				\
  ice-9/ports/internal.scm			\
  ice-9/rdelim.scm				\
  ice-9/regex.scm				\
  ice-9/weak-vector.scm				\
  rnrs/bytevectors.scm				\
  rnrs/bytevectors/gnu.scm			\
  scheme/base.scm				\
  scheme/case-lambda.scm			\
  scheme/char.scm				\
  scheme/complex.scm				\
  scheme/cxr.scm				\
  scheme/eval.scm				\
  scheme/file.scm				\
  scheme/inexact.scm				\
  scheme/lazy.scm				\
  scheme/load.scm				\
  scheme/process-context.scm			\
  scheme/r5rs.scm				\
  scheme/read.scm				\
  scheme/repl.scm				\
  scheme/time.scm				\
  scheme/write.scm				\
  srfi/srfi-9.scm				\
  srfi/srfi-9/gnu.scm				\
  srfi/srfi-11.scm				\
  srfi/srfi-14.scm

GENERATED_SOURCES = 				\
  hoot/char-prelude.scm

hoot/char-prelude.scm: $(top_srcdir)/bin/generate-char-prelude.scm
	$(AM_V_GEN)$(MKDIR_P) $(dir $@) && \
	  $(top_builddir)/pre-inst-env $(GUILE) $< > "$@"

CLEANFILES = $(GENERATED_SOURCES)
EXTRA_DIST = $(SOURCES)
moddir=$(pkgdatadir)/$(PACKAGE_VERSION)/lib
nobase_mod_DATA = $(SOURCES) $(GENERATED_SOURCES)
