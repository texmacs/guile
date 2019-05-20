# Lightening

Lightening is a just-in-time code generation library derived from GNU
Lightning, adapted to the purposes of the GNU Guile project.

## Use

```
gcc -flto -O2 -g -o lightening.o -c lightening/lightening.c
gcc -flto -O2 -g -o my-program lightening.o my-program.c
```

See the GNU Lightning manual for more on how to program against
Lightening (much of the details are the same).

## What's the difference with GNU Lightning?

This project is called Lightening because it's lighter-weight than GNU
Lightning.  When you go to generate code at run-time with GNU Lightning,
what happens is that you build up a graph of nodes which GNU Lightning
"optimizes" before finally emitting machine code.  These optimizations
can improve register allocation around call sites.  However they are not
helpful from a Guile perspective, as they get in the way of register
allocation that we need to do; and they actually prevent access to all
the registers that we would like to have.

Guile needs a simple, light-weight code generation library.  The GNU
Lightning architecture-specific backends provide the bulk of this
functionality, and Lightening wraps it all in a lightweight API.

## Supported targets

Lightening can generate code for the x86-64, i686, ARMv7, and AArch64
architectures.  It supports the calling conventions of MS Windows,
GNU/Linux, and Mac OS.

On i686, Lightening requires SSE support.  On ARMv7, we require hardware
floating-point support (the VFP instructions), as well as the UDIV/SDIV
instructions.

Lightening is automatically tested using GitLab's continuous integration
for under the supported architectures, for GNU/Linux; for a list of
recent jobs, see [the CI
page](https://gitlab.com/wingo/lightening/-/jobs).

## Future targets

Lightening has some inherited code from GNU Lightning for MIPS, PPC64,
and s390.  Patches to adapt this code to the Lightening code structure
are quite welcome.

RISC-V support would be fun too.

## Status

Lightening is used in GNU Guile since version 2.9.2 and seems to work
well.
