#!/bin/sh
./lightning -mx87=1 $srcdir/`basename $0 | sed -e 's|\.x87$||'`.tst
