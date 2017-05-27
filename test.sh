#!/bin/bash

function compile {
  echo "$1" | stack exec hec > tmp.s
  if [[ $? -ne 0 ]]; then
    echo "Failed to compile $1"
    exit
  fi
  gcc -o tmp.out driver/driver.c tmp.s
  if [[ $? -ne 0 ]]; then
    echo "GCC failed"
    exit
  fi
}

function test {
  expected="$1"
  expr="$2"
  compile "$expr"
  result="$(./tmp.out)"
  if [[ "$result" != "$expected" ]]; then
    echo "Test failed: $expected expected but got $result"
    exit
  fi
}

stack build

test 0 0
test 42 42

rm -f tmp.out tmp.s
echo "All tests passed"
