# Matrix

A fast, full-featured, modern terminal UI library for OCaml.

## Why Matrix?

- **Minimal dependencies** – Only depends on `uutf` for UTF-8 decoding. No transitive dependency bloat.
- **High performance** – Designed for 60+ FPS rendering. Zero-allocation frame diffing, efficient glyph pooling, and double-buffered rendering minimize GC pressure.
- **Modular architecture** – Small, focused libraries (`matrix.ansi`, `matrix.grid`, `matrix.pty`, `matrix.vte`, etc.) that you can use independently or combine as needed.
- **Immediate-mode API** – A simple render loop with `on_render`, `on_input`, and `on_resize` callbacks. No framework overhead—just draw your UI each frame.
- **Native alpha blending** – RGBA colors with proper alpha compositing for translucent overlays and smooth visual effects.
- **Full Unicode support** – Grapheme clusters, emoji, wide characters, and 24-bit color with correct alignment across terminal width calculation methods.
- **Modern terminal protocols** – Kitty keyboard (with auto-detection), SGR/X10/URXVT mouse tracking, bracketed paste, and focus reporting—all negotiated automatically.
- **Safe terminal handling** – Raw mode, alternate screen, and input protocols are restored on exit, even if your code raises an exception.
- **Declarative and imperative APIs** – Use the mutable `Grid` for performance-critical paths, or the Notty-inspired `Image` DSL for compositional layouts.
- **Built-in devtools** – Debug overlay for frame timing and FPS, and frame dumps to disk for diagnostics.

## Getting Started

Install via opam:

```bash
opam install matrix
```

Or build the library locally:

```bash
dune build @install
```

Run a demo to confirm everything works:

```bash
dune exec ./examples/01-rain/main.exe
```

### Hello Terminal

```ocaml
open Matrix

let () =
  let app = Matrix.create () in
  let frames = ref 0 in
  Matrix.run app
    ~on_frame:(fun _ ~dt:_ -> incr frames)
    ~on_input:(fun app event ->
      match event with
      | Input.Key { key = Input.Key.Escape; _ } -> Matrix.stop app
      | _ -> ())
    ~on_render:(fun app ->
      let grid = Matrix.grid app in
      Grid.clear grid;
      Grid.draw_text grid ~x:2 ~y:2
        ~text:(Printf.sprintf "Frames: %d" !frames))
```

`Matrix.run` switches the TTY to raw mode (when configured), negotiates
terminal features, and restores everything on exit even if a callback raises.

### Display Modes

Matrix supports two rendering modes:

- **`Alt`** (default) – Full-screen alternate buffer, content restored on exit
- **`Primary`** – Inline on primary screen, grows/shrinks dynamically with full scrollback

Use `Primary` for CLI tools that should leave output in terminal history:

```ocaml
let app = Matrix.create ~mode:`Primary () in
(* Use Matrix.static_print to write persistent output above the TUI *)
```

## API Overview

Matrix is organized into focused libraries that can be used together or independently:

| Library           | Module     | Purpose                                               |
| ----------------- | ---------- | ----------------------------------------------------- |
| `matrix`          | `Matrix`   | Immediate-mode runtime with render loop and callbacks |
| `matrix`          | `Image`    | Declarative Notty-inspired composition DSL            |
| `matrix.grid`     | `Grid`     | Mutable framebuffer with colors and styles            |
| `matrix.screen`   | `Screen`   | Double-buffered rendering with ANSI diffing           |
| `matrix.input`    | `Input`    | Keyboard, mouse, paste, focus event parsing           |
| `matrix.terminal` | `Terminal` | TTY control and capability detection                  |
| `matrix.ansi`     | `Ansi`     | Low-level ANSI escape sequence generation             |
| `matrix.glyph`    | `Glyph`    | Unicode grapheme cluster management                   |
| `matrix.terminfo` | `Terminfo` | Terminal capability database                          |
| `matrix.pty`      | `Pty`      | Pseudo-terminal spawning (POSIX + Windows ConPTY)     |
| `matrix.vte`      | `Vte`      | Virtual terminal emulator for embedding output        |

API documentation is available in the corresponding `.mli` files under `lib/`.

### Matrix vs Notty

| Aspect              | Matrix                                                                                                                                        | Notty                                                                                                 |
| ------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| Rendering model     | Immediate-mode, double-buffered grid with ANSI diffing; only changed cells emit bytes; built for high FPS.                                    | Declarative images; redraws full image on refresh; simpler, not diff-based.                           |
| Performance tactics | Zero-allocation hot loops, glyph pooling, optional render thread, explicit-width output for graphemes.                                        | Allocates per render; no diffing or explicit-width negotiation.                                       |
| Features            | RGBA with alpha blending, OSC8 hyperlinks, hit regions, inline/alt/split display modes, debug overlay, frame dumps.                           | Core fg/bg + styles; compact feature set; no alpha/hyperlinks or built-in diagnostics.                |
| Protocols and input | Auto-negotiates Kitty keyboard, SGR/URXVT/X10 mouse, bracketed paste, focus, explicit width; separates capability responses from user events. | Minimal, broadly compatible protocols by design; no capability probing; basic key/mouse/paste events. |
| Unicode handling    | Unicode width tables plus explicit-width negotiation to stay aligned with modern emoji/wide glyphs.                                           | Width hints only; no explicit-width negotiation.                                                      |
| API surface         | Mutable `Grid` for hot paths + Notty-inspired `Image` DSL; choose imperative or declarative.                                                  | Compositional image API; no mutable grid for rendering.                                               |
| Runtime and cleanup | Manages raw mode, alt/inline/split modes, cursor/mouse/paste/focus state, and restores the terminal on exit.                                  | Alt-screen runtime; inline helpers but no managed mode negotiation or runtime loop.                   |

## Acknowledgements

Matrix draws inspiration from several excellent projects:

- [Notty](https://github.com/pqwy/notty) – Declarative terminal graphics for OCaml - our `Image` API is directly inspired by Notty's.
- [OpenTUI](https://github.com/sst/opentui/) – TypeScript library for building terminal user interfaces (TUIs).
- [Rich](https://github.com/Textualize/rich) – Python library for rich text and beautiful formatting in the terminal.

## License

Matrix is licensed under the ISC license. See [LICENSE](../LICENSE) for details.
