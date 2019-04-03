# Lightening

Lightening is a just-in-time code generation library derived from GNU
Lightning, adapted to the purposes of the GNU Guile project.

## Use

```
gcc -flto -O2 -g -o jit.o -c jit/jit.c
gcc -flto -O2 -g -o my-program jit.o my-program.c
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

## Status

Only the x86-64 port is currently usable.  I plan to re-enable 32-bit
x86 shortly, and then work on 32-bit and 64-bit ARM.  Other
architectures may come with time, but help is very much appreciated
there.  The test suite is still in progress but will be fairly
comprehensive in terms of API surface.
