#!/bin/bash

make

bad=(
    test/lattests/bad
    test/mrjp-tests/bad/infinite_loop
    # test/mrjp-tests/bad/runtime
    test/mrjp-tests/bad/semantic
)

good=(
    test/lattests/good
    # test/lattests/extensions/struct
    # test/lattests/extensions/arrays1
    # test/lattests/extensions/objects1
    # test/lattests/extensions/objects2
    test/mrjp-tests/good
    test/mrjp-tests/gr5
)

for directory in ${bad[@]}; do
    for file in $directory/*.lat; do
        echo "Testing $file"
        ./latc $file
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
        ./latc $file
        exit_code=$?
        if [ $exit_code -ne 0 ]; then
            echo "Test $file failed"
            exit $exit_code
        fi
    done
done
