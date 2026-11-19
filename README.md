# terminal.hx

Floating terminal for Helix + Steel.

Two backends:

* **ghostty** (default when `bin/hx-term` exists): a tiny C helper linked
  against [libghostty-vt](https://lib.ghostty.org/vt/) owns a real pty and the
  VT state, and streams the visible grid to a Steel component.
  Full interactivity: fish/zsh/bash, vim, htop, curses apps, resize, Ctrl-C.
  Colors: the helper resolves SGR and palette colors to RGB and sends style
  runs; default-styled text uses the Helix theme (`ui.text`).
* **pipe** (fallback, pure Steel): piped stdio plus a local line editor.
  No tty, no full-screen apps, but zero native dependencies.

## Build the helper

```sh
make
```

Requires the libghostty-vt headers and library (they ship with the `ghostty`
package; 1.0+ has a stable C ABI).

## Install

```scheme
;; init.scm
(require "terminal/terminal.scm")

(keymap (global) (normal (space (terminal-open))))
```

`space t` in the default config of this dotfiles setup.

## Configuration

```scheme
(terminal-config #:shell "fish")                  ; default: $SHELL, else bash
(terminal-config #:backend 'ghostty)              ; 'auto | 'ghostty | 'pipe
(terminal-config #:helper "/path/to/hx-term")
(terminal-config #:shell "bash" #:args '("--noprofile" "--norc"))
(terminal-config #:scrollback 8000 #:tick-fast 16 #:tick-slow 120)
```

`:terminal-config` reports the current settings.

## Keys (ghostty backend)

| Key | Action |
|---|---|
| Ctrl-4 | close the popup (terminals send this for Ctrl-`\`) |
| everything else | forwarded to the child (Esc included) |

## Keys (pipe backend)

| Key | Action |
|---|---|
| Esc | close |
| Enter | submit line (restarts a dead shell) |
| Up/Down | history |
| PgUp/PgDn | scroll |
| Ctrl-L / Ctrl-U | clear output / input |

## Layout

```
terminal/
  terminal.scm   plugin
  hx-term.c      helper source
  bin/hx-term    helper binary (gitignored, make)
  cog.scm        steel package manifest
  Makefile
```
