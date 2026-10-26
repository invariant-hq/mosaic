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
dune exec ./packages/mosaic/examples/01-counter/main.exe
```

## Architecture

An application is four functions:

| Function        | Signature                           | Role                        |
| --------------- | ----------------------------------- | --------------------------- |
| `init`          | `unit -> model * msg Cmd.t`         | Initial state and commands  |
| `update`        | `msg -> model -> model * msg Cmd.t` | State transition            |
| `view`          | `model -> msg t`                    | Pure UI projection          |
| `subscriptions` | `model -> msg Sub.t`                | Event sources               |

`Cmd.t` carries side effects, started in list order after the update that
returned them: `Cmd.none`, `Cmd.quit`, `Cmd.perform` (work off the loop),
`Cmd.exec` (give the terminal to an editor or a pager), `Cmd.suspend` (stop
the job, as Ctrl+Z in a shell), `Cmd.batch`, `Cmd.set_title`, and
static-output commands.

`Sub.t` declares event sources: `Sub.on_keys`, `Sub.on_key`, `Sub.on_mouse`,
`Sub.on_paste`, `Sub.on_resize`, `Sub.on_tick`, `Sub.every`, `Sub.after`,
`Sub.worker`, `Sub.on_focus`, `Sub.on_blur`. Key, mouse and paste listeners
run after the elements, on the events no element stopped, unless declared
`~phase:Capture`. A timer or a worker lives as long as its subscription is
declared. `Sub.every` fires on a fixed grid of times, once in a turn, with the
number of periods that ended, `Sub.after` fires once after a delay, and a
worker's stop is requested when its id is dropped.

Components compose via `map : ('a -> 'b) -> 'a t -> 'b t`.

## Widgets

| Widget         | Constructor    | Description                                    |
| -------------- | -------------- | ---------------------------------------------- |
| Box            | `box`          | Container with border, background, layout      |
| Modal          | `modal`        | Dialog that makes the rest inert, Escape closes |
| Button         | `button`       | Primary click or Enter/Space activation        |
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
let ten_cells = px 10 (* 10 terminal columns or rows *)
let half = pct 50 (* 50% of the parent *)
let automatic = auto (* sized by the layout *)
let around = padding 2 (* the same padding on every side *)
let spaced = gap 1 (* the gap of a flex or grid container *)
let share = Grid.fr 1. (* a fractional grid track *)
let range =
  (* a grid track between two sizes *)
  Grid.minmax
    ~min:(Toffee.Style.Compact_length.length 10.)
    ~max:(Toffee.Style.Compact_length.fr 1.)
```

## Model-owned interaction

Editors, scroll positions, selections, and focus belong to the model. Semantic
callbacks such as `on_change`, `on_scroll`, `on_submit`, and `on_activate` return
messages directly. `update` accepts a proposed value by storing it, transforms
it, or rejects it by keeping the current value. Raw event callbacks such as
`on_key`, `on_mouse`, and `on_click` return `msg option` to filter events.

```ocaml
type model = { editor : Editor.t; focused : bool; dirty : bool }
type msg = Changed of Editor.t | Focus_editor

let init () =
  ({ editor = Editor.empty; focused = true; dirty = false }, Cmd.none)

let update msg model =
  match msg with
  | Changed editor ->
      let changed = not (String.equal (Editor.text editor) (Editor.text model.editor)) in
      ({ model with editor; dirty = model.dirty || changed }, Cmd.none)
  | Focus_editor -> ({ model with focused = true }, Cmd.none)

let view model =
  input ~state:model.editor ~focused:model.focused
    ~on_focus_request:Focus_editor ~on_change:(fun editor -> Changed editor) ()
```

Editor proposals include caret and selection changes. Always accept the complete
state when appropriate, but compare `Editor.text` before marking a document
modified, resetting submission status, or starting text-dependent work. Cursor
movement shares the existing text and source index.

`~focused` declares the model's accepted focus. On an ordinary left-button press,
`~on_focus_request` sends its message before caret and selection defaults run.
Those defaults proceed only if the application accepts focus on that live node.
Rejecting the request or removing the node leaves its editor unchanged.
`~on_click` remains a release gesture callback. An element with
`~on_focus_request` is also a stop for Tab and Shift+Tab, which deliver the next
or previous stop's request in view order unless a handler prevents the key;
`Cmd.focus_next` and `Cmd.focus_prev` do the same from `update`.

Buttons share that focus contract:

```ocaml
type form_focus = Name_field | Submit_button
type form_msg = Focus of form_focus | Submit

let submit_button focus =
  button ~focused:(focus = Submit_button)
    ~on_focus_request:(Focus Submit_button) ~on_activate:Submit
    ~border:true "Submit"
```

A button activates on a primary press and release inside without dragging, or an
unmodified Enter/Space press while focused. Reported key repeats and releases do
not activate it. Focus adds inverse video to the label. `~disabled:true` dims
the label and suppresses activation and focus requests; custom label colors and
other attributes are preserved. Include buttons in the application's focus traversal order; use input
`on_submit` for form submission so a global Enter shortcut does not intercept
button activation. `Mosaic_mlx.button` takes string-list `~children` as its label.

Selection widgets associate display items with application values:

```ocaml
type page = Overview | Settings
type page_msg = Page_selected of page

let page_picker selected =
  select ~equal:( = ) ~selected ~on_change:(fun page -> Page_selected page)
    [ Select.item Overview "Overview"; Select.item Settings "Settings" ]
```

`select`, `tab_select`, and `table` take `~selected:'a option` and `~equal`.
Construct their entries with `Select.item`, `Tab_select.item`, or
`Table.row value cells`. Item values must be distinct under `equal`. Reordering
keeps selection attached to its value. `None`, or a selected value absent from
the current entries, displays no selection and emits no change. Navigation
proposes a current item value; activation without a selection does nothing.

`Scroll.t` retains positions and symbolic follow/reveal targets. Timer
subscriptions require stable IDs. Key subscriptions run after the focused
widget and see only the keys it did not act on, so `Sub.on_keys [ (Shortcut.char
'q', Quit) ]` does not quit while `q` is typed into an input. A listener
declared `~phase:Capture` runs before the focused widget; call
`Event.Key.prevent_default` there to keep the widget from acting on the key.

## Paint and caching

```ocaml
let view _model = text ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.cyan ()) "Hello"
```

For expensive subtrees, use
`memo ~key:(document_id, revision, theme_version) view`. Keys are immutable,
function-free values containing every dependency of the subtree, including
callback dependencies. The cache lives only while that position is mounted.

## Packages

| Package      | Description                               |
| ------------ | ----------------------------------------- |
| `mosaic`     | TEA runtime, widget DSL, layout helpers   |
| `mosaic.ui`  | Lower-level UI: renderer, events, vnodes  |
| `mosaic.mlx` | JSX/MLX syntax adapter                   |
| `mosaic.eio` | Running from Eio (built with `eio`)      |
| `mosaic.lwt` | Running as an Lwt promise (built with `lwt`) |
| `mosaic.test` | Running a program on an emulated terminal in tests |

## Examples

27 examples and 3 showcase applications, listed with their run commands in
[examples/README.md](examples/README.md).

### Keyboard bindings

Widgets and subscriptions share `Shortcut.t`. Editor and selection-widget
`key_bindings` pair a typed shortcut with a widget action; custom entries
replace defaults for the same shortcut, with the last entry taking precedence.
`Sub.on_keys` uses the same precedence: compose bindings as `defaults @ overrides`.
For example:

```ocaml
type binding_msg = Edit of Editor.t | Select of string

let editor_view editor =
  textarea ~state:editor ~on_change:(fun state -> Edit state)
    ~key_bindings:[ (Shortcut.ctrl 'j', Mosaic_ui.Textarea.Submit) ] ()

let selection_view selected options =
  select ~equal:String.equal ~selected
    ~on_change:(fun value -> Select value)
    ~key_bindings:[ (Shortcut.ctrl 'n', Select.Move_down) ] options
```

Modifiers match exactly, ignoring Caps Lock and Num Lock. Releases do not
trigger bindings; repeats repeat navigation and editing. Keypad equivalents
and terminal-reported base-layout keys use the same matching rules everywhere.

Select measures its labels and descriptions when no size is specified. Use
`max_size` or an explicit height to constrain the visible rows; option changes
invalidate cached widths, while moving the selection reuses them.

## License

ISC. See [LICENSE](../../LICENSE).
