#!/bin/bash

function compile {
  echo "$1" | stack exec hec > tmp.s
  if [[ $? -ne 0 ]]; then
    echo "Failed to compile $1"
    exit
  fi
  gcc -o tmp.out "driver/$2.c" tmp.s
  if [[ $? -ne 0 ]]; then
    echo "GCC failed"
    exit
  fi
}

function test {
  driver="$1"
  expected="$2"
  expr="$3"
  compile "$expr" "$driver"
  result="$(./tmp.out)"
  if [[ "$result" != "$expected" ]]; then
    echo "Test failed: $expected expected but got $result"
    exit
  fi
}

function testfail {
  driver="$1"
  expr="$2"
  echo "$expr" | stack exec hec >/dev/null 2>&1
  if [[ $? -eq 0 ]]; then
    echo "Should fail to compile, but succeded: $expr"
    exit
  fi
}

stack build

test intfn 0 0
test intfn 42 42
test stringfn abc '"abc"'

test intfn 3 '1+2'
test intfn 3 '1 + 2'
test intfn 10 '1+2+3+4'

testfail stringfn '"abc'
testfail intfn '0abc'
testfail intfn '1+'

rm -f tmp.out tmp.s
echo "All tests passed"
