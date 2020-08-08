#!/bin/sh
set -e
if [ $# != 2 ]
then
  echo "Usage: test_all srcdir dstdir"
  exit 1;
fi
guile_srcdir=$1
guile_dstdir=$2
guile_main=$guile_dstdir/guile
export PATH=$guile_dstdir:$PATH
export GUILE_LOAD_PATH=.:$guile_srcdir:$GUILE_LOAD_PATH
print_test_header() {
  echo "================"
  echo "$1"
}
print_test_footer() {
  echo "$1"
  echo "================"
}
echo "================================"
echo "=         test examples        ="
echo "================================"
cd $guile_dstdir/examples/box
print_test_header "test example box"
./box script.scm
print_test_footer "test example box done"
cd $guile_dstdir/examples/box-dynamic
print_test_header "test example box-dynamic"
$guile_main script.scm
print_test_footer "test example box-dynamic done"
cd $guile_dstdir/examples/box-dynamic-module
print_test_header "test example box-dynamic-module"
for loop in "box-mixed" "box-module"
do
  echo "> run $loop"
  $guile_main $loop-script.scm
done
print_test_footer "test example box-dynamic-module done"
cd $guile_dstdir/examples/box-module
print_test_header "test example box-module"
./box script.scm
print_test_footer "test example box-module done"
cd $guile_dstdir/examples/c_scheme
print_test_header "test example c_scheme"
for loop in 1 2 3
do
  demo_name=demo$loop
  cd $guile_dstdir/examples/c_scheme/$demo_name
  echo "> run $demo_name"
  chmod +x ./$demo_name
  ./$demo_name ./script.scm
done

print_test_footer "test example c_scheme done"

cd $guile_srcdir/examples/modules
print_test_header "test example modules"
$guile_main main
print_test_footer "test example modules done"

cd $guile_srcdir/examples/safe
print_test_header "test example safe"
for loop in "evil" "untrusted"
do
  echo "> run $loop"
 $guile_main -s safe $loop.scm
done
print_test_footer "test example safe done"

cd $guile_srcdir/examples/scripts
print_test_header "test example scripts"
echo "> run fact 5"
$guile_main fact 5
for loop in "hello" "simple-hello.scm"
do
  echo "> run $loop"
 $guile_main $loop
done
print_test_footer "test example scripts done"

echo "==================================="
echo "=         test test-suite         ="
echo "==================================="

cd $guile_dstdir/test-suite/standalone
print_test_header "test test-suite standalone test-asmobs"
export builddir=$guile_dstdir/test-suite/standalone
$guile_main test-asmobs
print_test_footer "test test-suite standalone test-asmobs done"

for loop in test-conversion test-gh test-list test-num2integral test-round test-scm-c-read test-scm-take-locale-symbol \
test-scm-with-guile test-unwind test-with-guile-module
do
print_test_header "test test-suite standalone $loop"
./$loop
print_test_footer "test test-suite standalone $loop done"
done

print_test_header "test test-suite standalone test-require-extension"
sh $guile_srcdir/test-suite/standalone/test-require-extension
print_test_footer "test test-suite standalone test-require-extension done"

print_test_header "test test-suite standalone test-system-cmds"
$guile_main $guile_srcdir/test-suite/standalone/test-system-cmds
print_test_footer "test test-suite standalone test-system-cmds done"

cd $guile_srcdir/test-suite
guile_test=$guile_srcdir/test-suite/guile-test
$guile_main -e main -s $guile_test --test-suite $guile_srcdir/test-suite/tests

echo "all done!!!"