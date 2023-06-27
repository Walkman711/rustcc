ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

test:
	cd ${ROOT_DIR}tests
	./test_compiler.sh ../target/debug/rustcc
	cd -
