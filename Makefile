.PHONY: test

test:
	sbt "testOnly mycpu.HexTest"

riscv-test:
	bash -e riscv-test-all.sh

make-riscv-test: 
	sudo chown -R vscode:vscode /opt/riscv
	cd /opt/riscv/riscv-tests && autoconf && ./configure --prefix=/workdir && make && make install
	bash tohex.sh
