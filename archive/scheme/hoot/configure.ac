dnl -*- Autoconf -*-

AC_INIT(guile-hoot, 0.6.1)
AC_CONFIG_SRCDIR(module)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([1.12 foreign silent-rules subdir-objects  color-tests parallel-tests -Woverride -Wno-portability])
AM_SILENT_RULES([yes])

AC_CONFIG_FILES([Makefile module/Makefile lib/Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([module/hoot/config.scm])

# Prepare a version of $datadir that does not contain references to
# shell variables.  Snarfed from Guix.
hoot_prefix="`eval echo $prefix | sed -e"s|NONE|/usr/local|g"`"
hoot_datadir="`eval eval echo $datadir | sed -e "s|NONE|$hoot_prefix|g"`"
AC_SUBST([hoot_datadir])

GUILE_PKG([3.0])
GUILE_PROGS

# Find Guile's module source files.
GUILE_LIBRARY_DIR="`$PKG_CONFIG --variable=pkgdatadir guile-$GUILE_EFFECTIVE_VERSION`/$GUILE_EFFECTIVE_VERSION"
AC_SUBST(GUILE_LIBRARY_DIR)

# V8-based JS+Wasm runtimes (optional)
AC_PATH_PROG([NODE], [node])
AC_SUBST([NODE])

AC_PATH_PROG([D8], [d8])
AC_SUBST([D8])

AC_OUTPUT
