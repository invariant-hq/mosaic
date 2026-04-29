# Mosaic

A terminal UI framework for OCaml.

Mosaic implements [The Elm Architecture](https://guide.elm-lang.org/architecture/)
for terminal applications: state is a typed model, the UI is a pure
function of the model, and side effects are explicit values. Layout is
CSS Flexbox and Grid via [Toffee](../toffee/README.md). Rendering is
handled by [Matrix](../matrix/README.md).

## Quick start

```ocaml
open Mosaic

type msg = Increment | Decrement | Quit

let init () = (0, Cmd.none)

let update msg model =
  match msg with
  | Increment -> (model + 1, Cmd.none)
  | Decrement -> (model - 1, Cmd.none)
  | Quit -> (model, Cmd.quit)

let view model =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [ text (Printf.sprintf "Count: %d" model) ];
      text "Press + / - to change, q to quit";
    ]

let subscriptions _model =
  Sub.on_keys
    [
      (Shortcut.char '+', Increment);
      (Shortcut.char '-', Decrement);
      (Shortcut.char 'q', Quit);
      (Shortcut.escape, Quit);
    ]

let () = run { init; update; view; subscriptions }
```

Install and run:

```bash
opam install mosaic
```

Or build from source:

```bash
dune build @install
dune exec ./mosaic/examples/01-counter/main.exe
```

## Architecture

An application is four functions:

| Function        | Signature                           | Role                        |
| --------------- | ----------------------------------- | --------------------------- |
| `init`          | `unit -> model * msg Cmd.t`         | Initial state and commands  |
| `update`        | `msg -> model -> model * msg Cmd.t` | State transition            |
| `view`          | `model -> msg t`                    | Pure UI projection          |
| `subscriptions` | `model -> msg Sub.t`                | Event sources               |

`Cmd.t` carries side effects: `Cmd.none`, `Cmd.quit`, `Cmd.perform`,
`Cmd.batch`, `Cmd.set_title`, `Cmd.focus`.

`Sub.t` declares event sources: `Sub.on_keys`, `Sub.on_key`, `Sub.on_mouse`,
`Sub.on_paste`, `Sub.on_resize`, `Sub.on_tick`, `Sub.every`, `Sub.on_focus`,
`Sub.on_blur`.

Components compose via `map : ('a -> 'b) -> 'a t -> 'b t`.

## Widgets

| Widget         | Constructor    | Description                                    |
| -------------- | -------------- | ---------------------------------------------- |
| Box            | `box`          | Container with border, background, layout      |
| Text           | `text`         | Styled text with fragments, wrapping           |
| Text input     | `input`        | Single-line input with cursor, placeholder     |
| Textarea       | `textarea`     | Multi-line editor with undo/redo, wrapping      |
| Select         | `select`       | Vertical list with keyboard and mouse          |
| Tab select     | `tab_select`   | Horizontal tab bar with scroll arrows          |
| Table          | `table`        | Columns, headers, row styles, overflow         |
| Slider         | `slider`       | Horizontal/vertical with sub-cell precision    |
| Scroll box     | `scroll_box`   | Scrollable container with scroll bars          |
| Canvas         | `canvas`       | Mutable drawing surface for custom graphics    |
| Code           | `code`         | Syntax-highlighted code display                |
| Markdown       | `markdown`     | Full CommonMark rendering                      |
| Tree           | `tree`         | Hierarchical expand/collapse navigation        |
| Spinner        | `spinner`      | Animated loading indicators                    |
| Progress bar   | `progress_bar` | Horizontal/vertical animated fill              |
| Line number    | `line_number`  | Code gutter with signs and per-line colors     |

Every widget accepts layout properties: `size`, `padding`, `margin`,
`gap`, `flex_direction`, `flex_grow`, `align_items`, `justify_content`,
`grid_template_columns`, and the full set of CSS Flexbox/Grid options.

## Layout

Mosaic uses CSS layout semantics powered by Toffee (a port of
[Taffy](https://github.com/DioxusLabs/taffy)). Flexbox, Grid, and Block
are all supported.

Dimension helpers:

```ocaml
px 10             (* 10 terminal columns/rows *)
pct 50            (* 50% of parent *)
auto              (* automatic sizing *)
padding 2         (* uniform padding *)
gap 1             (* flex/grid gap *)
Grid.fr 1.        (* fractional grid track *)
Grid.minmax ~min ~max  (* range-sized grid track *)
```

## Rich text

Text supports styled fragments with hierarchical style merging:

```ocaml
text ~fragments:[
  bold "Hello ";
  fg Ansi.Color.cyan (bold "world");
  styled (Ansi.Style.make ~italic:true ()) "!";
] ""
```

## Packages

| Package      | Description                               |
| ------------ | ----------------------------------------- |
| `mosaic`     | TEA runtime, widget DSL, layout helpers   |
| `mosaic.ui`  | Lower-level UI: renderer, events, vnodes  |
| `mosaic.mlx` | JSX/MLX compatibility layer               |

## Examples

24 examples and 3 showcase applications. Run any example from the repo root:

```bash
dune exec ./mosaic/examples/<name>/main.exe
```

| Example            | Description                                       |
| ------------------ | ------------------------------------------------- |
| `01-counter`       | TEA basics: model, update, view, subscriptions    |
| `02-text`          | Styled text with colors and formatting            |
| `03-spinner`       | Animated loading indicators                       |
| `04-input`         | Single-line text input with cursor                |
| `05-select`        | Vertical list with keyboard/mouse navigation      |
| `06-tabs`          | Horizontal tab bar with scroll arrows             |
| `07-slider`        | Value sliders with sub-cell precision             |
| `08-table`         | Data tables with columns and styling              |
| `09-canvas`        | Procedural drawing with shapes and braille        |
| `10-code`          | Syntax highlighting with tree-sitter              |
| `11-scrollbox`     | Scrollable content with scroll bars               |
| `12-form`          | Multi-component form with focus management        |
| `13-markdown`      | CommonMark rendering in a scroll box              |
| `14-selection`     | Text selection across elements                    |
| `15-charts`        | Interactive charts with zoom, pan, tooltips       |
| `16-timer`         | Countdown with tick subscriptions                 |
| `17-progress-bar`  | Animated progress bars                            |
| `18-textarea`      | Multi-line editing with word wrap                 |
| `19-tree`          | Hierarchical tree with expand/collapse            |
| `20-async`         | Background operations with `Cmd.perform`          |
| `21-line-number`   | Code gutter with signs and colors                 |
| `22-layout`        | CSS Grid with responsive switching                |
| `23-resize`        | Terminal resize and focus events                  |
| `24-eio`           | Eio-based runtime                                 |
| `x-agent`          | Agent-style primary-screen CLI                    |
| `x-dashboard`      | Component composition with `map`                  |
| `x-syspanel`       | System monitor: CPU, memory, disk, processes      |

## License

ISC. See [LICENSE](../LICENSE).
