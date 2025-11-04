#!/bin/bash

set -e

cargo build

echo

for t in tests/*.ha; do
  echo -n "$t... "
  diff --color <(target/debug/sea-lang $t) $t.out
  echo "OK"
done

echo
echo "All tests passed!"
