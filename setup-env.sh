#!/bin/bash
# setup-env.sh: Add the tools directory to PATH and set up a dummy X display for SDL2 in Codespaces
# Usage: source ./setup-env.sh

TOOLS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/tools" && pwd)"
export PATH="$PATH:$TOOLS_DIR"

# Start Xvfb if not already running, and set DISPLAY=:99
if ! pgrep -f "Xvfb :99" > /dev/null; then
  Xvfb :99 -screen 0 1024x768x24 &
  sleep 1
fi
export DISPLAY=:99

echo "[setup-env.sh] Added $TOOLS_DIR to PATH. DISPLAY set to :99 (Xvfb). You can now use 'git bug' and run SDL2 graphics in Codespaces."
