(** Markdown rendering demonstration. *)

open Mosaic

type msg = Quit

let markdown_content =
  {|# Markdown Demo

This demonstrates **Mosaic's** markdown rendering capabilities.

## Text Formatting

- **Bold text** with double asterisks
- *Italic text* with single asterisks
- ***Bold and italic*** with triple asterisks
- `inline code` with backticks
- ~~Strikethrough~~ with double tildes

## Links

- [OCaml website](https://ocaml.org)
- [Mosaic GitHub](https://github.com/spice/mosaic)
- <https://example.com> - autolinks

## Lists

### Unordered Lists

- First item
- Second item
  - Nested item 1
  - Nested item 2
    - Deep nested
- Third item

### Ordered Lists

1. First step
2. Second step
   1. Sub-step A
   2. Sub-step B
3. Third step

## Code Blocks

```ocaml
(* OCaml code *)
let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let () = Printf.printf "5! = %d\n" (factorial 5)
```

```bash
# Shell commands
dune build @all
dune exec my_app
```

## Blockquotes

> This is a blockquote.
> It can span multiple lines.
>
> > Nested blockquotes are supported.
> > They can contain **formatted text**.

## Tables

| Column 1 | Column 2 | Column 3 |
|----------|----------|----------|
| Cell A1  | Cell B1  | Cell C1  |
| Cell A2  | Cell B2  | Cell C2  |
| Cell A3  | Cell B3  | Cell C3  |

## Horizontal Rules

---

## Inline HTML (as text)

<div>HTML tags are rendered as plain text</div>

## Task Lists

- [x] Completed task
- [ ] Incomplete task
- [x] Another completed task

## Complex Example

### A Real-World Code Example

```ocaml
module App = struct
  type model = {
    count : int;
    message : string;
  }

  type msg =
    | Increment
    | Decrement
    | Reset
    | SetMessage of string

  let init () =
    ({ count = 0; message = "Welcome!" }, Cmd.none)

  let update msg model =
    match msg with
    | Increment ->
        ({ model with count = model.count + 1 }, Cmd.none)
    | Decrement ->
        ({ model with count = model.count - 1 }, Cmd.none)
    | Reset ->
        ({ model with count = 0 }, Cmd.none)
    | SetMessage text ->
        ({ model with message = text }, Cmd.none)

  let view model =
    Ui.vstack [
      Ui.text ~style:Style.(fg Green) model.message;
      Ui.text (Printf.sprintf "Count: %d" model.count);
    ]
end
```

> **Note**: This example shows how The Elm Architecture works in Mosaic.
> The `model` holds state, `msg` defines events, and `update` handles
> state transitions.
|}

let init () = ((), Cmd.none)
let update msg () = match msg with Quit -> ((), Cmd.quit)

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let view () =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Header *)
      box ~padding:(padding 1) ~background:header_bg
        [
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            ~size:{ width = pct 100; height = auto }
            [
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Markdown";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Scrollable markdown content *)
      box ~flex_grow:1. ~padding:(padding 1)
        [
          scroll_box ~scroll_y:true ~scroll_x:false
            ~size:{ width = pct 100; height = pct 100 }
            [ markdown markdown_content ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "scroll with mouse wheel  •  q quit" ];
    ]

let subscriptions () =
  Sub.on_keys [ (Shortcut.char 'q', Quit); (Shortcut.escape, Quit) ]

let () = run { init; update; view; subscriptions }
