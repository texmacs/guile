#!/bin/sh

# This trivial script exists to have a single place where the
# invocation of aclocal is specified.  It is called from both
# autogen.sh and the Makefile.

aclocal -I guile-config
