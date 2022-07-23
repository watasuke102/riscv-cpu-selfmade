#!/usr/bin/env bash

src=./share/riscv-tests/isa/rv32*i-p-*
dst=./src/riscv

for f in $src; do
  if [[ ! $f =~ "dump" ]]; then
    name="${f##*/}"
    riscv64-unknown-elf-objcopy -O binary $f "$dst/$name.bin"
    od -An -tx1 -w1 -v $dst/$name.bin > $dst/$name.hex   
    rm -f $dst/$name.bin
  fi
done