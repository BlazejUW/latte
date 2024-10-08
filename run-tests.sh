#!/bin/bash -e

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color
OK="${GREEN}OK${NC}"
ERROR="${RED}ERROR - This test should have failed, but didn't!${NC}"

# Run tests
echo "Running tests..."


echo "--- SHOULD FAIL: ---"

# Run all tests from bad/ directory, expecting to fail:
for file in ./lattests/bad/*.lat; do
    echo -n "Running test $file... "
    ./src/Latte/RunCompile $file > /dev/null 2>&1 && echo -e $ERROR && exit 1 || echo -e $OK
done

echo "--- LLVM TESTS: ---"

# Pułapka na EXIT, aby usunąć tymczasowe pliki niezależnie od wyniku skryptu
trap 'rm -f temp_output.ll temp_minified_output.ll temp_minified_expected.ll' EXIT

# # Run all tests from llvm-tests/ directory
# for file in ./lattests/llvm-tests/*.lat; do
#     echo -n "Running test $file... "
#     ./Latte/RunCompile "$file" > temp_output.ll 
#     python3 minify_llvm.py temp_output.ll temp_minified_output.ll
#     python3 minify_llvm.py "${file%.lat}.ll" temp_minified_expected.ll
#     diff -u temp_minified_output.ll temp_minified_expected.ll && echo -e $OK || exit 1
# done

echo "--- EASY TESTS: ---"
# Run all tests from good/ directory
for file in ./lattests/easy_tests/*.lat; do
    echo -n "Running test $file... "
    ./src/Latte/RunCompile $file | llvm-link - -S ./lib/runtime_for_mac.bc | lli | diff - ${file%.lat}.output && echo -e $OK || exit 1
done


echo "--- SHOULD NOT FAIL: ---"
# Run all tests from good/ directory
for file in ./lattests/good/*.lat; do
    echo -n "Running test $file... "
    ./src/Latte/RunCompile $file | llvm-link - -S ./lib/runtime_for_mac.bc | lli | diff - ${file%.lat}.output && echo -e $OK || exit 1
done

# echo "--- STUCENCKIE SHOULD FAIL: ---"
# for file in ./lattests/studenckie/bad/infinite_loop/*.lat; do
#     echo -n "Running test $file... "
#     ./src/Latte/RunCompile $file > /dev/null 2>&1 && echo -e $ERROR && exit 1 || echo -e $OK
# done
# for file in ./lattests/studenckie/bad/semantic/*.lat; do
#     echo -n "Running test $file... "
#     ./src/Latte/RunCompile $file > /dev/null 2>&1 && echo -e $ERROR && exit 1 || echo -e $OK
# done
# for file in ./lattests/studenckie/bad/runtime/*.lat; do
#     echo -n "Running test $file... "
#     ./src/Latte/RunCompile $file > /dev/null 2>&1 && echo -e $ERROR && exit 1 || echo -e $OK
# done

echo "--- STUCENCKIE SHOULD NOT FAIL: ---"
for file in ./lattests/studenckie/good/basic/*.lat; do
    echo -n "Running test $file... "
    ./src/Latte/RunCompile $file | llvm-link - -S ./lib/runtime_for_mac.bc | lli | diff - ${file%.lat}.output && echo -e $OK || exit 1
done

echo "--- END ---"
echo "All tests run - check for failures above."
