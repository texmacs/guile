#!/bin/sh
./lightning -mthumb=0 $srcdir/`basename $0 | sed -e 's|\.arm$||'`.tst
