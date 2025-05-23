# vibe-knight

A 2D roguelike game in Common Lisp using SDL2, inspired by Zelda 3.

**Author:** Jim Hansson

## Structure
- `src/` – all source code
- `doc/` – documentation
- `vibe-knight.asd` – ASDF system file
- `build.lisp` – script to build an executable

## Build and Run

1. **Install SDL2, SBCL, and Lisp dependencies:**
   ```bash
   sudo apt-get install sbcl libsdl2-dev
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

4. **Run tests (no SDL2 required):**
   ```bash
   make test
   ```
   This will run all unit tests, loading all necessary files. SDL2 is not required for running tests.

## Using Roswell for Common Lisp Development

Roswell is a command-line launcher and environment manager for Common Lisp. It makes it easy to install, switch, and run different Lisp implementations, manage scripts, and set up development environments.

### Install Roswell (Linux)

You can install Roswell with:

```sh
curl -L https://raw.githubusercontent.com/roswell/roswell/release/scripts/install-for-ci.sh | sh
```

Or, if available on your system:

```sh
sudo apt-get install roswell
```

Or with Homebrew (Linux or macOS):

```sh
brew install roswell
```

### Basic Usage

- Start a REPL: `ros run`
- Install SBCL: `ros install sbcl`
- Set SBCL as default: `ros use sbcl`
- List available implementations: `ros list`

### Using Roswell with this project

You can use Roswell to run and test this project with your preferred Lisp implementation. For example:

```sh
ros run --load build.lisp
```

Or to run tests:

```sh
ros run --load src/package.lisp --load src/world-test.lisp
```

See [https://roswell.github.io/](https://roswell.github.io/) for more details.

## Issue Tracking with git-bug

This project uses [git-bug](https://github.com/MichaelMure/git-bug) for distributed bug and issue tracking. git-bug stores issues directly in your git repository, so you can work with issues offline and sync them with others via git.

### Install git-bug

Note: git-bug is not the same as Roswell. The .deb file provided here is for git-bug, not Roswell.

If git-bug is not available in your package manager, you can manually install it using the provided .deb file:

```sh
sudo dpkg -i git-bug.deb
```

Or download the latest release from [https://github.com/MichaelMure/git-bug/releases](https://github.com/MichaelMure/git-bug/releases):

```sh
# Example for Linux (x86_64)
wget https://github.com/MichaelMure/git-bug/releases/latest/download/git-bug-linux-amd64
chmod +x git-bug-linux-amd64
sudo mv git-bug-linux-amd64 /usr/local/bin/git-bug
```

Or use Homebrew:
```sh
brew install git-bug
```

### Basic Usage

- Initialize git-bug in your repo:
  ```sh
  git bug init
  ```
- Create a new issue:
  ```sh
  git bug new
  ```
- List issues:
  ```sh
  git bug list
  ```
- Show an issue:
  ```sh
  git bug show <issue-id>
  ```
- Sync issues with remotes:
  ```sh
  git bug push
  git bug pull
  ```

See [https://github.com/MichaelMure/git-bug](https://github.com/MichaelMure/git-bug) for more details and advanced usage.

## Developer Log

Project progress, decisions, and daily notes are tracked in `doc/diary.org` using org-mode. This developer log includes:

- Daily summaries of work completed
- Project milestones and important decisions
- Notes on design, testing, and future plans

To contribute to the log, add a new entry under the appropriate date in `doc/diary.org`. The format uses org-mode headings for year, month, and full date, for example:

```
* 2025
** May
*** 2025-05-16
- Notes about today's work...
```

Review the log to understand the project's history and ongoing development.

## Project Logs and Reflections

- [Development Diary](doc/diary.org)
- [Reflections on AI Programming](doc/reflection-on-ai-programming.org)

## Note
This is a basic structure. Add more logic and graphics as needed!