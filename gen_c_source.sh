files="cpp_err_symbols.in cpp_sig_symbols.in"
#awk -f cpp_cnvt.awk < cpp_err_symbols.in
for loop in ${files}
do
  file=${loop%.*}
  echo "generate ${file}.c"
  awk -f ./libguile/cpp_cnvt.awk < ./libguile/${file}.in > ./cmake-build-debug/libguile/${file}.c
done