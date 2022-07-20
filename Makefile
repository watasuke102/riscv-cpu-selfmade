.PHONY: test

test:
	sbt "testOnly mycpu.HexTest"
