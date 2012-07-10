#!/bin/bash

function run_warn {
    set +e +E
    pass=true

    diff -u --strip-trailing-cr -- "./test/$1.warn" "./build/test.warn/$1"
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

    echo "Running test $1 ..." >&2
    "./build/test/$1" > "./build/test.out/$1" 2> "./build/test.err/$1"

    diff -u --strip-trailing-cr -- "./test/$1.out" "./build/test.out/$1"
    if [ "$?" != 0 ] ; then pass=false; fi

    diff -u --strip-trailing-cr -- "./test/$1.err" "./build/test.err/$1"
    if [ "$?" != 0 ] ; then pass=false; fi

    if ! $pass ; then
	echo "[FAILED] $1" >&2
	exit 1
    fi

    set -e -E
}

for file in ./build/test.warn/* ; do
    base=$(basename "$file")
    [ -f "./build/test.warn/$base" ] || continue
    run_warn $base
done

for file in ./build/test/* ; do
    base=$(basename "$file")
    [ -f "./build/test/$base" ] || continue
    run_test $base
done

echo 'All tests passed! :)'
