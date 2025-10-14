# Hoot example project

This directory serves as an example of a basic Hoot project that you
can use as a starting point for your own project!

It has everything you need:

* A simple Scheme program composed of multiple modules that makes use
  of Web APIs.

* A `manifest.scm` file for creating a development environment with
  `guix shell`

* A `Makefile` for compiling the Scheme source to a WebAssembly
  binary.

* HTML and JavaScript files for loading the WebAssembly binary in a
  web page.

* A simple development web server for testing builds.

There is one wrinkle in this template: The `reflect.wasm`, `reflect.js`,
and `wtf8.wasm` files are symlinks.  If you choose to copy this project
somewhere as a starting point, you'll want to copy over the necessary
files.  If you've installed Hoot to your system via `make install` or
via a package manager, you'll find all these files in Hoot's `share`
directory (such as `/usr/share/guile-hoot`) and can symlink/copy from
there.  We are looking forward to making this step entirely unnecessary
in future releases.

## Building

Building this project requires [GNU Guix](https://guix.gnu.org).

Once you have Guix, the development environment with all necessary
dependencies can be created:

```
guix shell
```

Build the project:

```
make
```

Launch a development web server:

```
make serve
```

To check if the program works, visit https://localhost:8088 in your
web browser.  We recommend using Mozilla Firefox or Google Chrome at
this time.
