> [!NOTE]
> this too was supposed to be a blog post. it was supposed to have lots of examples
> so i had made a repo but i have not touched this in a while so. 

Cross Compiling
===============

Cross-Compiling refers to building software on one platform (the host) that is
intended to run on a different platform (the target). Direct compilation on
the target platform might be infeasible, for example on embedded systems with
limited computing resources.

For example, a microwave oven will have an extremely small computer to read
its keypad and door sensor, provide output to a digital display and speaker,
and to control the microwave for cooking food. This computer is generally not
powerful enough to run a compiler, a filesystem, or a development environment.

Also, a company may wish to support several different versions of an
operating system or to support several different operating systems. By using
a cross compiler, a single build environment can be set up to compile for each
of these targets.

Other cases where cross compilation is required includes bootstrapping a new
platform, compiling native code for emulators of old platforms, etc.

**Things to consider:**

- Instruction set architecture (ISA).
- Application binary interface (ABI).
- Operating system or runtime environment.
- System libraries.

These properties together define the target platform. Compilers represent
this information using a 'target triple'. Example: `x86_64-unknown-linux-gnu`.
This describes:

- `x86_64`: instruction set architecture
- `unknown`: vendor
- `linux`: operating system kernel
- `gnu`: system C library / userland ABI

> You can run `gcc -dumpmachine` or `clang -dumpmachine` to probe your own
> device.

Compiling with different architecture is handled by compilers that support
cross compilation. The vendor specific differences generally do not matter.
For cases where they do matter, you should look into other resources. I do not
have enough knowledge to even suggest you anything.

However, compiling for a different platform requires more than a compiler
binary. The toolchain must also provide the components of the target system,
such as:

- Headers for the target C library.
- Target system libraries.
- Runtime startup objects.
- The correct linker configuration.

These are usually provided through a `sysroot`, which mirrors the filesystem
layout of the target system.



