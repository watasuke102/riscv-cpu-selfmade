#!/usr/bin/env bash

src=./share/riscv-tests/isa/rv32*i-p-*
dst=./src/riscv
script=$(cd $(dirname $0); pwd)

for f in $src; do
  if [[ ! $f =~ "dump" ]]; then
    name="${f##*/}"
    bash $script/create-hex.sh $f $dst/$name.hex
  fi
done
