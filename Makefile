# Makefile for vibe-knight

.PHONY: all run build clean

all: build

build:
	sbcl --script build.lisp

run:
	./vibe-knight

clean:
	rm -f vibe-knight
