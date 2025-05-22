#!/bin/bash
# setup-env.sh: Add the tools directory to PATH for this session
# Usage: source ./setup-env.sh

TOOLS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/tools" && pwd)"
export PATH="$PATH:$TOOLS_DIR"
echo "[setup-env.sh] Added $TOOLS_DIR to PATH. You can now use 'git bug' as a git subcommand."
