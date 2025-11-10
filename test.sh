#!/bin/bash

set -e

cargo build

echo

for t in examples/*.tc; do
  echo -n "$t... "
  diff --color <(target/debug/taco $t) $t.out
  echo "OK"
done

echo
echo "All tests passed!"
