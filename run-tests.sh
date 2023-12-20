#!/bin/bash -e

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color
OK="${GREEN}OK${NC}"
ERROR="${RED}ERROR - This test should have failed, but didn't!${NC}"

# Run tests
echo "Running tests..."

# Run all tests from good/ directory
# for file in ./lattests/good/*.lat; do
#     echo -n "Running test $file... "
#     ./Latte/RunCompile $file | diff - ${file%.lat}.out && echo -e $OK || exit 1
# done

echo "--- SHOULD FAIL: ---"

# Run all tests from bad/ directory, expecting to fail:
for file in ./lattests/bad/*.lat; do
    echo -n "Running test $file... "
    ./Latte/RunCompile $file > /dev/null 2>&1 && echo -e $ERROR && exit 1 || echo -e $OK
done

echo "--- END ---"
echo "All tests run - check for failures above."
