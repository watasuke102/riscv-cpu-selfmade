#!/usr/bin/env bash

riscv64-unknown-elf-objcopy -O binary $1 tmp 
od -An -tx1 -w1 -v tmp > $2 
rm -f tmp 
