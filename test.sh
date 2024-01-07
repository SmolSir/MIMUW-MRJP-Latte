#!/bin/bash

gcc -c -m32 lib/runtime.c -o lib/runtime.o
make

bad=(
    test/lattests/bad
    # test/mrjp-tests/bad/infinite_loop
    # test/mrjp-tests/bad/runtime
    # test/mrjp-tests/bad/semantic
)

good=(
    test/lattests/good
    # test/lattests/extensions/struct
    # test/lattests/extensions/arrays1
    # test/lattests/extensions/objects1
    # test/lattests/extensions/objects2
    test/mrjp-tests/good/basic
    # test/mrjp-tests/good/hardcore
    # test/mrjp-tests/good/arrays
    # test/mrjp-tests/good/virtual
    # test/mrjp-tests/gr5
)

for directory in ${bad[@]}; do
    for file in $directory/*.lat; do
        echo "Testing $file"
        ./latc_x86 $file
        exit_code=$?
        if [ $exit_code -eq 0 ]; then
            echo "Test $file failed"
            exit $exit_code
        fi
    done
done

for directory in ${good[@]}; do
    for file in $directory/*.lat; do
        echo "Testing $file"
        ./latc_x86 $file
        exit_code=$?
        if [ $exit_code -ne 0 ]; then
            echo "Test $file failed"
            exit $exit_code
        fi

        # compile
        gcc -m32 -z execstack lib/runtime.o ${file%.*}.s -o ${file%.*}

        # run with input if present
        if [ -f ${file%.*}.input ]; then
            ./${file%.*} < ${file%.*}.input > test.out
        else
            ./${file%.*} > test.out
        fi
        diff test.out ${file%.*}.output
        exit_code=$?
        if [ $exit_code -ne 0 ]; then
            echo "Test $file failed: output differs"
            exit $exit_code
        fi
    done
done

echo "All tests passed!"
