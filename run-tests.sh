#!/bin/bash

# function run_warn {
#     set +e +E
#     pass=true
# 
#     diff -u --strip-trailing-cr -- \
#         "./compiler/tests/negative/$1.warn" "./build/tests.warn/$1"
#     if [ "$?" != 0 ] ; then pass=false; fi
# 
#     if ! $pass ; then
# 	echo "[FAILED] $1"
# 	exit 1
#     fi
# 
#     set -e -E
# }

function run_test {
    set +e +E
    pass=true

    "${args[1]}/$1" \
        > "${args[1]}.out/$1" \
        2> "${args[1]}.err/$1"

    diff -u --strip-trailing-cr -- \
        "${args[0]}/$1.out" \
        "${args[1]}.out/$1"
    if [ "$?" != 0 ] ; then pass=false; fi

    diff -u --strip-trailing-cr -- \
        "${args[0]}/$1.err" \
        "${args[1]}.err/$1"
    if [ "$?" != 0 ] ; then pass=false; fi

    if ! $pass ; then
	echo "[FAILED] $1" >&2
	exit 1
    fi

    set -e -E
}

if [ $# != 2 ]; then
    echo 'Usage: run-tests.sh SourceDirectory TargetDirectory'
    exit 1
fi

args=("$@")

# for file in ./build/tests.warn/* ; do
#     if [ -f "$file" ]; then
#         run_warn $(basename "$file")
#     else
#         echo "Skipping test $file ..."
#     fi
# done

for file in ./build/tests/positive/* ; do
    if [ -f "$file" ]; then
        justfile=$(basename "$file")
        echo "Running test $justfile ..."
        run_test "$justfile"
    else
        echo "Skipping test $file ..."
    fi
done

echo 'All tests passed! :)'
