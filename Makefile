# Makefile for vibe-knight

.PHONY: all run build clean install-deps

all: build

build:
	sbcl --script build.lisp

run:
	./vibe-knight

clean:
	rm -f vibe-knight

install-deps:
	bash install-cl-sdl2.sh
