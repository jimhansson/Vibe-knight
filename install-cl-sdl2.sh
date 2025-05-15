#!/bin/bash
# install-cl-sdl2.sh
# Script to install cl-sdl2 and its dependencies for Quicklisp

set -e

QL_LOCAL=/home/codespace/quicklisp/local-projects
mkdir -p "$QL_LOCAL"

cd "$QL_LOCAL"

git clone https://github.com/lispgames/cl-sdl2.git || echo "cl-sdl2 already cloned"
git clone https://github.com/lispgames/sdl2-ffi.git || echo "sdl2-ffi already cloned"
git clone https://github.com/lispgames/cffi.git || echo "cffi already cloned"
git clone https://github.com/lispgames/trivial-features.git || echo "trivial-features already cloned"
git clone https://github.com/lispgames/trivial-garbage.git || echo "trivial-garbage already cloned"
git clone https://github.com/lispgames/rtg-math.git || echo "rtg-math already cloned"

echo "cl-sdl2 and dependencies are now in $QL_LOCAL."
echo "You can now run (ql:quickload :cl-sdl2) in your Lisp REPL."
