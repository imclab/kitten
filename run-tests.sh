#!/bin/bash

function run_warn {
    set +e +E
    pass=true

    diff -u --strip-trailing-cr -- \
        "./compiler/tests/$1.warn" "./build/tests.warn/$1"
    if [ "$?" != 0 ] ; then pass=false; fi

    if ! $pass ; then
	echo "[FAILED] $1"
	exit 1
    fi

    set -e -E
}

function run_test {
    set +e +E
    pass=true

    "./build/tests/$1" > "./build/tests.out/$1" 2> "./build/tests.err/$1"

    diff -u --strip-trailing-cr -- \
        "./compiler/tests/$1.out" "./build/tests.out/$1"
    if [ "$?" != 0 ] ; then pass=false; fi

    diff -u --strip-trailing-cr -- \
        "./compiler/tests/$1.err" "./build/tests.err/$1"
    if [ "$?" != 0 ] ; then pass=false; fi

    if ! $pass ; then
	echo "[FAILED] $1" >&2
	exit 1
    fi

    set -e -E
}

for file in ./build/tests.warn/* ; do
    if [ -f "$file" ]; then
        run_warn $(basename "$file")
    else
        echo "Skipping test $file ..."
    fi
done

for file in ./build/tests/* ; do
    if [ -f "$file" ]; then
        run_test $(basename "$file")
    else
        echo "Skipping test $file ..."
    fi
done

echo 'All tests passed! :)'
