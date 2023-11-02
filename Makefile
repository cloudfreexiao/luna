.PHONY: test

test:
	zig build test --summary failures -Dversion=skynet_lua

	zig build install-example-zig-function
	zig build install-example-zig-interpreter

docs:
	mkdir -p docs
	zig build-lib -femit-docs=docs src/luna.zig