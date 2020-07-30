#!/bin/sh
DOT_X_FILES="alist.x arbiters.x async.x backtrace.x boolean.x chars.x	\
    continuations.x debug.x deprecation.x deprecated.x discouraged.x	\
    dynl.x dynwind.x environments.x eq.x error.x eval.x evalext.x	\
    extensions.x feature.x fluids.x fports.x futures.x gc.x gc-mark.x	\
    gc-segment.x gc-malloc.x gc-card.x goops.x gsubr.x guardians.x	\
    hash.x hashtab.x hooks.x i18n.x init.x ioext.x keywords.x lang.x	\
    list.x load.x macros.x mallocs.x modules.x numbers.x objects.x	\
    objprop.x options.x pairs.x ports.x print.x procprop.x procs.x	\
    properties.x random.x rdelim.x read.x root.x rw.x scmsigs.x		\
    script.x simpos.x smob.x sort.x srcprop.x stackchk.x stacks.x	\
    stime.x strings.x srfi-4.x srfi-13.x srfi-14.x strorder.x		\
    strports.x struct.x symbols.x threads.x throw.x values.x		\
    variable.x vectors.x version.x vports.x weaks.x ramap.x unif.x \
    dynl.x filesys.x posix.x net_db.x socket.x regex-posix.x
    "

for loop in ${DOT_X_FILES}
do
  file=${loop%.*}
  echo "handle ${file}.c"
./guile-snarf -o cmake-build-debug/libguile/${file}.x libguile/${file}.c \
-DHAVE_CONFIG_H -I.. -I.. -I.  -g -O2 -Wall -Wmissing-prototypes -I./cmake-build-debug
done