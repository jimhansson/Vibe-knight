# Makefile for vibe-knight

.PHONY: all run build clean install-deps test

all: build

build:
	sbcl --script build.lisp

run:
	./vibe-knight

clean:
	rm -f vibe-knight

install-deps:
	bash install-cl-sdl2.sh

test:
	@if command -v ros >/dev/null 2>&1; then \
	  ros run --eval '(ql:quickload :prove)' --load src/package.lisp --load src/world.lisp --load src/world-test.lisp --quit; \
	else \
	  sbcl --load ~/quicklisp/setup.lisp --eval '(ql:quickload :prove)' --load src/package.lisp --load src/world.lisp --load src/world-test.lisp --quit; \
	fi
