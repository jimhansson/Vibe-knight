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
	ASDF_OUTPUT_TRANSLATIONS="/:$(PWD)/:/src:$(PWD)/src" sbcl --load ~/quicklisp/setup.lisp --eval '(ql:quickload :prove)' --load src/package.lisp --load src/world-test.lisp --quit
