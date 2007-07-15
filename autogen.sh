#!/bin/sh
# Usage: sh -x ./autogen.sh

set -e

[ -f GUILE-VERSION ] || {
  echo "autogen.sh: run this command only at the top of guile-core."
  exit 1
}

######################################################################
### update infrastructure

autoreconf -i --force --verbose

echo "guile-readline..."
(cd guile-readline && ./autogen.sh)

echo "Now run configure and make."
echo "You must pass the \`--enable-maintainer-mode' option to configure."
