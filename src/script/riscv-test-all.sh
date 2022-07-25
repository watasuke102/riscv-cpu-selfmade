#!/usr/bin/env bash

for f in src/riscv/*; do
  bash src/script/test.sh $f
done
