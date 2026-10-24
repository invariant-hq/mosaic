# Matrix

A fast, full-featured, modern terminal UI library for OCaml.

## Why Matrix?

- **No dependencies** beyond OCaml's distribution: Unicode data tables and UTF-8 handling are built in.
- **Minimal output**: double-buffered rendering diffs cell changes and writes only what changed. Grid-owned grapheme storage keeps allocations off the hot path.
- **Modular architecture**: small, focused libraries (`matrix.ansi`, `matrix.grid`, `matrix.pty`, `matrix.vte`, and others) that you can use alone or together.
- **Immediate-mode API**: a loop you write. `Matrix.next` returns the next input event, timer, posted message or frame to draw, and you draw your UI in each frame.
- **Alpha blending**: RGBA colors composite for translucent overlays.
- **Unicode**: grapheme clusters, emoji, wide characters and 24-bit color, aligned whichever way the terminal measures text width.
- **Modern terminal protocols**: the kitty keyboard protocol, SGR and X10 mouse tracking, bracketed paste and focus reports, negotiated with the terminal.
- **Safe terminal handling**: raw mode, the alternate screen and the input protocols are restored on exit, even if your code raises an exception.
- **Declarative and imperative APIs**: the mutable `Grid` for performance-critical paths, or the Notty-inspired `Image` DSL for compositional layouts.

## Getting Started

Install via opam:

```bash
opam install matrix
```

Or build the library locally:

```bash
dune build @install
```

Run a demo from the repository's root to confirm everything works:

```bash
dune exec ./packages/matrix/examples/01-rain/main.exe
```

### Hello Terminal

```ocaml
open Matrix

let () =
  Matrix.run @@ fun app ->
  let rec loop keys =
    match Matrix.next app with
    | Stopped | Event (Key { key = Escape; _ }) -> ()
    | Event (Key k) when Input.Keystroke.is_interrupt k -> ()
    | Event (Key _) ->
        Matrix.request_redraw app;
        loop (keys + 1)
    | Frame ->
        Matrix.draw app (fun grid ->
            Grid.draw_text grid ~x:2 ~y:2 ~text:(Printf.sprintf "Keys: %d" keys));
        loop keys
    | Event _ | Timer _ | Posted _ -> loop keys
  in
  loop 0
```

`Matrix.run` takes the terminal for its scope: it switches the terminal to raw
mode, negotiates terminal features, and restores everything on every way out,
an exception included. Inside, `Matrix.next` returns what to do next: an input
event, a timer that came due (`Matrix.at`), a message another thread posted
(`Matrix.post`), or a frame to draw. A frame comes when the program asks for
one (`Matrix.request_redraw`), and when the runtime needs one, as at the start
and after a resize. An idle program costs no wakeups. In raw mode Ctrl+C is a
key, so the program decides what it does; this one quits, as it does at Escape.

`Matrix.next` waits for the terminal with `select(2)`. A program with an event
loop of its own takes the run's steps with `Matrix.step app ~now`, which never
waits, and waits with that loop until one of `Matrix.fds app` is readable or
`Matrix.deadline app` comes. Mosaic's `mosaic.eio` library does it for Eio.

### Display Modes

Matrix has two presentation modes:

- **`Alt`** (the default): the alternate screen, which the run owns whole. The
  shell's screen comes back on exit.
- **`Primary`**: a live region at the bottom of the normal screen, below the
  shell's output and the rows the program adds to it. Those rows stay in the
  terminal's history after the program ends.

Use `Primary` for command-line tools that leave output in the terminal's
history. This program adds a row above its live region and waits for a key:

```ocaml
let () =
  let config = Matrix.config ~mode:`Primary () in
  Matrix.run ~config @@ fun app ->
  let row = Matrix.Grid.create ~width:20 ~height:1 () in
  Matrix.Grid.draw_text row ~x:0 ~y:0 ~text:"Started";
  Matrix.static_write app row;
  let rec loop () =
    match Matrix.next app with
    | Stopped | Event (Key _) -> ()
    | Frame ->
        Matrix.draw app (fun grid ->
            Matrix.Grid.draw_text grid ~x:0 ~y:0 ~text:"Press a key");
        loop ()
    | Event _ | Timer _ | Posted _ -> loop ()
  in
  loop ()
```

## API Overview

Matrix is organized into focused libraries that can be used together or independently:

| Library           | Module            | Purpose                                            |
| ----------------- | ----------------- | -------------------------------------------------- |
| `matrix`          | `Matrix`          | Immediate-mode runtime: a scoped run and its loop  |
| `matrix.image`    | `Matrix_image`    | Declarative Notty-inspired composition DSL         |
| `matrix.grid`     | `Matrix_grid`     | Mutable framebuffer with colors and styles         |
| `matrix.screen`   | `Matrix_screen`   | Double-buffered rendering with ANSI diffing        |
| `matrix.input`    | `Matrix_input`    | Keyboard, mouse, paste and focus event decoding    |
| `matrix.terminal` | `Matrix_terminal` | Terminal modes and capability detection           |
| `matrix.terminfo` | `Matrix_terminfo` | Terminal descriptions from the terminfo database   |
| `matrix.tty`      | `Tty`             | The terminal device and the signals it sends       |
| `matrix.ansi`     | `Ansi`            | ANSI escape sequence generation                    |
| `matrix.text`     | `Matrix_text`     | Unicode text measurement and segmentation          |
| `matrix.charts`   | `Matrix_charts`   | Declarative terminal charts rendered into grids    |
| `matrix.pty`      | `Pty`             | POSIX pseudo-terminal spawning                     |
| `matrix.vte`      | `Vte`             | Virtual terminal emulator for embedding output     |
| `matrix.cli`      | `Matrix_cli`      | Tables and trees for command-line output           |
| `matrix.test`     | `Matrix_test`     | Programs run on an emulated terminal in tests      |

`Matrix` gives the modules of the libraries it uses short names, such as
`Matrix.Grid` and `Matrix.Input`.

API documentation is available in the corresponding `.mli` files under `lib/`.

### Matrix vs Notty

| Aspect              | Matrix                                                                                                                                        | Notty                                                                                                 |
| ------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| Rendering model     | Immediate-mode, double-buffered grid with ANSI diffing; only changed cells emit bytes; built for high FPS.                                    | Declarative images; redraws full image on refresh; simpler, not diff-based.                           |
| Performance tactics | Double-buffered cell diffing, grid-owned grapheme storage, explicit-width output for graphemes.                                                | Allocates per render; no diffing or explicit-width negotiation.                                       |
| Features            | RGBA with alpha blending, OSC8 hyperlinks, hit regions, inline/alt display modes.                                                             | Core fg/bg + styles; compact feature set; no alpha/hyperlinks.                                        |
| Protocols and input | Auto-negotiates Kitty keyboard, SGR/URXVT/X10 mouse, bracketed paste, focus, explicit width; separates capability responses from user events. | Minimal, broadly compatible protocols by design; no capability probing; basic key/mouse/paste events. |
| Unicode handling    | Unicode width tables plus explicit-width negotiation to stay aligned with modern emoji/wide symbols.                                          | Width hints only; no explicit-width negotiation.                                                      |
| API surface         | Mutable `Grid` for hot paths + Notty-inspired `Image` DSL; choose imperative or declarative.                                                  | Compositional image API; no mutable grid for rendering.                                               |
| Runtime and cleanup | Manages raw mode, alt/inline modes, cursor/mouse/paste/focus state, and restores the terminal on exit.                                        | Alt-screen runtime; inline helpers but no managed mode negotiation or runtime loop.                   |

## Acknowledgements

Matrix draws inspiration from several excellent projects:

- [Notty](https://github.com/pqwy/notty) – Declarative terminal graphics for OCaml - our `Image` API is directly inspired by Notty's.
- [OpenTUI](https://github.com/sst/opentui/) – TypeScript library for building terminal user interfaces (TUIs).
- [Rich](https://github.com/Textualize/rich) – Python library for rich text and beautiful formatting in the terminal.

## License

Matrix is licensed under the ISC license. See [LICENSE](../../LICENSE) for details.
