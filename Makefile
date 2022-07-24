.PHONY: test

test:
	sbt "testOnly mycpu.HexTest"

riscv-test:
	bash -e riscv-test-all.sh

make-riscv-test: 
	sudo chown -R vscode:vscode /opt/riscv
	cd /opt/riscv/riscv-tests && autoconf && ./configure --prefix=/workdir && make && make install
	bash src/script/riscv-test-tohex.sh

ctest:
	riscv64-unknown-elf-gcc -march=rv32i -mabi=ilp32 -c -o ctest.o src/c/ctest.c
	riscv64-unknown-elf-ld  -b elf32-littleriscv ctest.o -T src/c/link.ld -o ctest
	bash src/script/create-hex.sh ctest src/hex/ctest.hex
	rm ctest ctest.o
