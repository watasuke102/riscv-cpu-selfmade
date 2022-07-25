.PHONY: test

test:
	sbt "testOnly mycpu.HexTest"

riscv-test:
	bash -e src/script/riscv-test-all.sh

make-riscv-test: 
	sudo chown -R vscode:vscode /opt/riscv
	cd /opt/riscv/riscv-tests && autoconf && ./configure --prefix=/workdir && make && make install
	bash src/script/riscv-test-tohex.sh

ctest: src/c/ctest.c
	mkdir -p build
	riscv64-unknown-elf-gcc -O2 -march=rv32i -mabi=ilp32 -c -o ctest.o src/c/ctest.c
	riscv64-unknown-elf-ld  -b elf32-littleriscv ctest.o -T src/c/link.ld -o build/ctest
	bash src/script/create-hex.sh build/ctest src/hex/ctest.hex
	rm ctest.o
	bash src/script/test.sh src/hex/ctest.hex

dump:
	riscv64-unknown-elf-objdump -b elf32-littleriscv -D build/ctest
