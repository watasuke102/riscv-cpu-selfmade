#!/usr/bin/env bash

old_file=$(sed -ne 's|.*val file.*"\(.*\)"|\1|p' src/test/scala/test.scala)

for f in src/riscv/*; do
  echo $f
  sed -ie "s|\(file.*\)\".*\"|\1\"$f\"|" src/test/scala/test.scala
  make test
done

sed -ie "s|\(file.*\)\".*\"|\1\"$old_file\"|" src/test/scala/test.scala
