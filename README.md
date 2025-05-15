# vibe-knight

A 2D roguelike game in Common Lisp using SDL2, inspired by Zelda 3.

## Structure
- `src/` – all source code
- `doc/` – documentation
- `vibe-knight.asd` – ASDF system file
- `build.lisp` – script to build an executable

## Build and Run

1. **Install SDL2 and Lisp dependencies:**
   ```bash
   sudo apt-get install libsdl2-dev
   # Install Quicklisp if you don't have it
   # Download and install sdl2 bindings for Common Lisp
   (ql:quickload :sdl2)
   ```

2. **Run the game directly in Lisp:**
   ```lisp
   (asdf:load-system :vibe-knight)
   (vibe-knight:main)
   ```

3. **Build a standalone executable with SBCL:**
   ```bash
   sbcl --script build.lisp
   # This creates an executable named 'vibe-knight' in the project folder
   ./vibe-knight
   ```

## Note
This is a basic structure. Add more logic and graphics as needed!