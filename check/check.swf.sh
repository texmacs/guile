#!/bin/sh
./lightning -mvfp=0 $srcdir/`basename $0 | sed -e 's|\.swf$||'`.tst
