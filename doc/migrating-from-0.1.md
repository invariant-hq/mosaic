# Migrating from 0.1 to 0.2

This guide is for programs written against 0.1.0 of `mosaic`, `matrix` or
`toffee`. It lists every API that 0.2.0 removes, renames or reshapes, and what
to write instead. It goes package by package, then library by library, and
each section starts with why we made the change.

Most entries are compile errors, so the compiler leads you to them. Each
package ends with a list of behaviour changes: code that compiles unchanged
but acts differently. Read them even when your build is green.

Matrix and Mosaic programs do not run on Windows in 0.2.0. `Matrix.run`, and
so `Mosaic.run`, raises `Failure` there, because the new runtime has no
Windows path yet. `matrix.pty` is POSIX-only too: we removed its ConPTY
backend, which was unsound. We are working on Windows support for a later
release.

Which parts you need depends on what your program uses. A Mosaic application
needs [Packages and libraries](#packages-and-libraries) and [Mosaic](#mosaic).
It also needs the `Ansi.Color` entry of [`matrix.ansi`](#matrixansi). The
[Matrix behaviour changes](#matrix-behaviour-changes) on colors, signals,
Unicode widths and Windows reach it too, through `Mosaic.run`. A program that
drives Matrix itself needs all of [Matrix](#matrix).

In the tables, `Char 'c'` is short for `Char (Uchar.of_char 'c')`.

## What most programs hit

- `Mosaic.run` takes the terminal's options as `~config:(Matrix.config ...)`,
  where it took `~matrix` ([Running a program](#running-a-program)).
- Your model owns every widget's state ([Widget state](#widget-state)). The
  widgets take it like this:

  | Widgets | Labels |
  | --- | --- |
  | `input`, `textarea`, `slider`, `scroll_bar`, `tree` | `~state ~on_change` |
  | `scroll_box` | `~scroll ~on_scroll` |
  | `select`, `tab_select`, `table` | `~equal ~selected ~on_change`, over values of your own type |

- The callbacks that report a widget's own change return `'msg` where they
  returned `'msg option`. They are `on_change`, `on_activate`, `on_submit` and
  `on_scroll`.
- You declare focus with `?focused` and `?on_focus_request`. They replace
  `?id`, `?ref`, `?autofocus`, `?focusable` and `Cmd.focus`, and Tab now moves
  focus ([Focus](#focus)).
- `Event.Key.data` returns a `Matrix.Input.Keystroke.t`. Its `modifier` field
  is now `mods`, and Ctrl+C is `Char 'c'` with `ctrl` set
  ([Events and subscriptions](#events-and-subscriptions)).
- Color constructors such as `Ansi.Color.Red` become values such as
  `Ansi.Color.red` ([`matrix.ansi`](#matrixansi)).
- `Sub.every` and `Sub.on_tick` take an `~id`
  ([Events and subscriptions](#events-and-subscriptions)).
- A widget given no color draws with the theme, in the terminal's own colors.
  In 0.1.0 it used fixed colors ([Colors and themes](#colors-and-themes)).

## Packages and libraries

Two packages and two libraries are gone or moved. Every Mosaic install used to
compile 36 MB of tree-sitter's generated C that Mosaic itself did not use, so
the bindings now live in their own package. In our measurement, a fresh
install went from 90 seconds to 60. `matrix-eio` is gone because any event loop
can now wait on a run's descriptors.

| 0.1.0 | 0.2.0 |
| --- | --- |
| The `tree-sitter` package (`tree-sitter`, `tree-sitter.json`, `tree-sitter.ocaml`) | Its own package at <https://github.com/invariant-hq/ocaml-tree-sitter>, with the same library names. Feed its captures to `Syntax_highlight.of_triples`, or run it in a `Code.Highlighter` ([highlighting.md](highlighting.md)) |
| The `matrix-eio` package, `Matrix_eio.create` | `Mosaic_eio.run` (library `mosaic.eio`, built when `eio` is installed). A Matrix program waits for `Matrix.fds` and `Matrix.deadline` with Eio and calls `Matrix.step` |
| The `matrix.glyph` library (`Glyph`, `Matrix.Glyph`) | `matrix.text` (`Matrix_text`, also `Matrix.Text`) to measure and segment text. `Glyph.t` and `Glyph.Pool` have no replacement: the grid stores graphemes itself |
| `Matrix.Image` | `Matrix_image`, library `matrix.image` |

The main modules of the sublibraries now carry their package's prefix. Their
old names were generic, so another opam package, or a `grid.ml` of your own,
with the same module name could not be linked next to them.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `Grid`, `Screen`, `Input`, `Terminal`, `Terminfo` | `Matrix_grid`, `Matrix_screen`, `Matrix_input`, `Matrix_terminal`, `Matrix_terminfo` |
| `Style`, `Tree`, `Geometry`, `Compute` | `Toffee_style`, `Toffee_tree`, `Toffee_geometry`, `Toffee_compute` |
| `Compute_block`, `Compute_flexbox`, `Compute_grid`, `Compute_helpers` | `Toffee_compute_block`, `Toffee_compute_flexbox`, `Toffee_compute_grid`, `Toffee_compute_helpers` |

Code that goes through `Matrix` or `Toffee` keeps the short names.
`Matrix.Grid`, `Matrix.Screen`, `Matrix.Input` and `Matrix.Terminal` still
work, and so do `Toffee.Style` and `Toffee.Geometry`. `Matrix` also has
`Matrix.Text`, `Matrix.Terminfo` and `Matrix.Tty` now, beside `Matrix.Ansi`.
`Ansi`, `Vte` and `Pty` keep their bare names, and the new `Tty` has one too.

## Mosaic

### Running a program

Matrix no longer calls back into the program, so Mosaic now runs Matrix's loop
itself. `Mosaic.run` takes the terminal's options as a `Matrix.config`, and
the descriptors to run on, where it took a Matrix app:

```ocaml
(* 0.1.0 *)
Mosaic.run ~matrix:(Matrix.create ~mode:`Primary ~target_fps:(Some 30.) ()) app

(* 0.2.0 *)
Mosaic.run ~config:(Matrix.config ~mode:`Primary ~fps:30. ()) app
```

The full signature is `Mosaic.run ?input ?output ?env ?stop ?config
?process_perform app`. The [`matrix`](#matrix-runtime) table shows how the
options of `Matrix.create` map to `Matrix.config`.

An Eio program runs its Mosaic application with `Mosaic_eio.run`. It runs the
work of `Cmd.perform` and `Sub.worker` as fibers of the calling domain, so the
`process_perform` that forked them goes too:

```ocaml
(* 0.1.0 *)
Eio_main.run @@ fun env ->
Eio.Switch.run @@ fun sw ->
let matrix =
  Matrix_eio.create ~sw ~clock:(Eio.Stdenv.clock env) ~stdin:env#stdin
    ~stdout:env#stdout ()
in
Mosaic.run ~matrix ~process_perform app

(* 0.2.0 *)
Eio_main.run @@ fun env -> Mosaic_eio.run (Eio.Stdenv.domain_mgr env) app
```

If you drive the reconciler yourself, two calls change. In 0.1.0 `Mosaic.map`
copied the whole view and broke the memos under it. A map is now applied when
a message arrives, and the messages leave through `deliver`. `render` needs
the viewport width because it picks the branches of `viewport_switch` before
layout.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `Reconciler.create ~container` | `Reconciler.create ~container ~deliver`, where `deliver` receives the messages of the view's handlers, so a view no longer has to be mapped to `unit` |
| `Reconciler.render t vnode` | `Reconciler.render t ~viewport_width view` |

### Views

You build views with `Mosaic`'s elements, and `Mosaic_ui.Vnode` is gone. It
was a second vocabulary for the same tree, so every label was spelled twice,
and its representation was public.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `'msg Mosaic.t`, which was `'msg option Vnode.t` | `'msg Mosaic.t`, abstract: no pattern takes it apart, and a test observes what `Mosaic.Reconciler` mounts |
| `Mosaic_ui.Vnode` and `Vnode.<element>` | `Mosaic.<element>`, with the labels of `Mosaic`'s elements |
| `Vnode.text ~style:row s` (a prebuilt `Toffee.Style.t`) | `Mosaic.text ~layout:row s`. Every element takes `?layout`, which its layout labels override |
| `Vnode.map`, `fragment`, `empty`, `embed`, `px`, `pct`, `auto`, `size`, `gap`, `padding`, `margin`, `inset` | The same names in `Mosaic` |

The MLX forms of `mosaic.mlx` take the same labels as the elements, so every
change in this section and the next applies to them too.

### Focus

Your application now declares which element is focused. In 0.1.0 the widgets
held focus, and it drifted away from what the application thought. A stored
`ref` could also outlive its element, because it fired when the element was
created and never when it was unmounted. Now an element that can take focus
gives the message that asks for it, and your model decides.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `?id`, `?autofocus`, `?focusable` on every element | `?focused` and `?on_focus_request` |
| `?ref` on every element | `?on_size` to read an element's size; `embed` to place a renderable of your own |
| `Cmd.focus id` | Declare the element `~focused:true`. For traversal, `Cmd.focus_next` and `Cmd.focus_prev`, or `Mosaic.Focus.next` and `Mosaic.Focus.prev` over your own identifiers |

```ocaml
(* 0.1.0 *)
input ~id:"name" ~autofocus:true ~value:model.name
  ~on_input:(fun s -> Some (Name_changed s))
  ()

(* 0.2.0 *)
input
  ~focused:(model.focus = Name)
  ~on_focus_request:(Focus Name)
  ~state:model.name
  ~on_change:(fun e -> Name_changed e)
  ()

(* update *)
| Focus f -> ({ model with focus = f }, Cmd.none)
```

Tab and Shift+Tab now move focus. They send the `?on_focus_request` of the
next or previous element in view order. Before, every application restated
the view's order as a list and bound Tab to it, and the list fell out of date
when an element was hidden. A form that moved focus on Tab with its own code
can drop that code.

A listener that gave Tab another meaning now also moves focus, unless it
prevents the key. Bind the key with `Sub.on_keys`, which prevents the keys it
binds, or call `Event.Key.prevent_default`.

```ocaml
(* 0.1.0: Tab did nothing by default *)
Sub.on_key (fun k ->
  match (Event.Key.data k).key with Tab -> Some Next_page | _ -> None)

(* 0.2.0 *)
Sub.on_keys [ (Shortcut.tab, Next_page) ]
```

### Widget state

Your model now owns each widget's state. In 0.1.0 the widgets kept their own,
and the view only passed starting values. A value the application forgot to
pass back drifted away from the model, and a value forced from the view undid
what the user had just typed. Both of our own editing examples had that bug.

Now a widget shows the state the model gives it and proposes the next one
through `on_change`. The callbacks that report a widget's own change return
`'msg` where they returned `'msg option`, since the widget has already decided
there is a change. They are `on_change`, `on_activate`, `on_submit` and
`on_scroll`. Raw event callbacks still return `'msg option`.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `input` and `textarea`'s `?value`, `?cursor`, `?selection`, `?on_input`, `?on_cursor` | `~state:Editor.t ~on_change`. Build with `Editor.of_string` or `Editor.empty`; read with `Editor.text`, `Editor.cursor`, `Editor.selection` |
| `input ?max_length` | `Editor.of_string ?max_length` |
| `textarea ?spans`, `code ?spans` | `?syntax:Code.syntax` ([Other elements](#other-elements)); `textarea ?annotations` for styles the application resolves itself |
| `slider ?value ?on_value_change` | `slider ~state ~on_change` |
| `scroll_bar ?on_change` | `scroll_bar ~state ~scroll_size ~viewport_size ~on_change` |
| `select ?selected_index` over `Select.item` records | `select ~equal ~selected ~on_change` over `Select.item value ?description label` |
| `tab_select ?selected` over `Tab_select.item` records | `tab_select ~equal ~selected ~on_change` over `Tab_select.item value ?description label` |
| `table ?rows ?selected_row` | `table ~equal ~selected ~on_change`, rows built with `Table.row value cells` |
| `tree ?selected_index ?expand_depth ?on_expand` | `tree ~state:Tree.State.t ~on_change`, the state built with `Tree.State.make`; `Tree.item` takes `~id` |
| `scroll_box ?sticky_scroll ?sticky_start ?on_scroll` | `scroll_box ~scroll:Scroll.t ~on_scroll`; `Scroll.follow` is the sticky edge |

`select`, `tab_select` and `table` report the selected value where they
reported its index. An index pointed at the wrong item as soon as the
application reordered or filtered its items. A value stays selected through
both:

```ocaml
(* 0.1.0 *)
select ~selected_index:model.size
  ~on_change:(fun i -> Some (Size_changed i))
  [ { Select.label = "Small"; description = None };
    { Select.label = "Large"; description = None } ]

(* 0.2.0 *)
select ~equal:( = ) ~selected:(Some model.size)
  ~on_change:(fun s -> Size_changed s)
  [ Select.item Small "Small"; Select.item Large "Large" ]
```

A tree's state names its nodes by id:

```ocaml
(* 0.2.0: model.tree starts as Tree.State.make ~expanded:[ "src" ] () *)
let items =
  [ Tree.item ~id:"src" ~children:[ Tree.item ~id:"src/main.ml" "main.ml" ] "src" ]
in
tree ~items ~state:model.tree ~on_change:(fun s -> Tree_changed s) ()
```

A scroll position is a value the model keeps. In 0.1.0 a scroll box had no
label for its position, only one-shot requests that the application had to
retire after use. Mosaic resolves a `Scroll.t` against the real extents when
it draws the view, so there is nothing to write back:

```ocaml
(* 0.1.0 *)
scroll_box ~sticky_scroll:true ~sticky_start:`Bottom lines

(* 0.2.0: model.scroll starts as Scroll.follow `Bottom (Scroll.at 0) *)
scroll_box ~scroll:model.scroll ~on_scroll:(fun s -> Scrolled s) lines
```

### Other elements

Durations are in seconds everywhere now. We renamed `~delta` to `~dt` and
`interval` to `period` so that code written for milliseconds fails to compile.
With the old labels it would have run 1,000 times too slowly.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `canvas ?respect_alpha`, `Canvas.respect_alpha`, `Canvas.set_respect_alpha` | None; `Canvas.set_uchar ~blend:true` blends one cell |
| `Canvas.set_cell ~glyph` | `Canvas.set_uchar ~uchar` |
| `Canvas.draw_line ?glyphs` | `Canvas.draw_line ?symbols` |
| The draw function's `~delta` | `~dt`, still in seconds: `canvas (fun c ~dt -> ...)`. A drawing that moves with `dt` also needs `~live:true` ([behaviour changes](#mosaic-behaviour-changes)) |
| `Syntax_theme` | `Syntax_style` |
| `Markdown.style`, `Markdown.default_style` | `Markdown.Style.t`, `Markdown.Style.default`, built with `Markdown.Style.make` |
| `Spinner.frame_set`'s `interval`, in milliseconds | `period`, in seconds |

`Markdown.style` was a function, and Mosaic compared it by physical equality.
One built in the view highlighted every code block again on every refresh. A
`Markdown.Style.t` is a value, compared by value.

Highlighted code takes its spans as a `Code.syntax`. Highlights are now tied to
the text they were computed for, so a stale result shows plain text instead of
coloring the wrong bytes:

```ocaml
(* 0.1.0 *)
code ~spans:(Syntax_theme.apply theme ~content ranges) content

(* 0.2.0 *)
code ~syntax:(Code.syntax (Syntax_highlight.of_triples ~content ranges)) content
```

Where you called `Syntax_theme.apply` for the spans themselves, call
`Syntax_highlight.to_spans ~style (Syntax_highlight.of_triples ~content ranges)`.

### Colors and themes

In 0.1.0 the widgets drew in fixed colors, and they were unreadable on light
terminals: white text, or an orange keyword at a contrast of 2.2:1 on white.
A widget given no color now draws with the roles of the theme in scope, in the
terminal's own foreground and background.

Text uses the `Plain` role and borders use `Border`. Inputs are drawn on
`Field`, and the focused item of a list on `Cursor`. The calls keep compiling.
To get 0.1.0's look back, pass the color labels that held it:

```ocaml
select ~selected_background:(Ansi.Color.of_rgb 51 68 85)
  ~selected_text_color:(Ansi.Color.of_rgb 255 255 0) ...
box ~border_color:Ansi.Color.white ~focused_border_color:Ansi.Color.bright_cyan ...
input ~text_color:Ansi.Color.white ~cursor_color:Ansi.Color.white ...
code ~syntax:(Code.syntax ~style:Syntax_style.default highlights) ...
markdown ~md_style:Markdown.Style.default ...
```

Or restyle a role for the whole view with one `themed` at its root:

```ocaml
let view model =
  themed
    (Theme.with_style Theme.Cursor
       (Ansi.Style.make ~fg:Ansi.Color.yellow ~bg:(Ansi.Color.of_rgb 51 68 85) ()))
    (box [ ... ])
```

### Text selection

A selection now spans the whole view, and the renderer owns it. You hear it in
one place, with `Sub.on_selection`. It draws with the theme's `Highlight`
role, over each run's own style. In 0.1.0 a selection with no color given
showed nothing at all.

`?selectable` takes a mode where it took a bool. With a bool, an element could
not choose between selecting its text and selecting every cell it draws, as a
terminal does.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `text`'s and `code`'s `?selection_bg`, `?selection_fg`; `input`'s and `textarea`'s `?selection_color`, `?selection_fg` | The theme's `Highlight` role (`themed`) |
| `code ?on_selection` | `Sub.on_selection`, which hears the view's selection as text; `Cmd.copy_selection` copies it |
| `text ~selectable:false`, `code ~selectable:false` | `` ~selectable:`Off ``, which every element takes |
| `text ~selectable:true`, `code ~selectable:true` | Nothing: an element nothing declares for reads `` `Text ``; `` ~selectable:`Text `` inside an element that declares `` `Off `` |

### Events and subscriptions

`Event.Key.data` returns a `Matrix.Input.Keystroke.t`, where it returned an
`Input.Key.event`. In 0.1.0 one key had a different value in each terminal
encoding: Shift+a was `A` in one and `a` with Shift in another. Now a key's
value is the character typed, with Shift applied, in every encoding.
[`matrix.input`](#matrixinput) lists every change. These are the ones you meet
first:

| 0.1.0 | 0.2.0 |
| --- | --- |
| `modifier` | `mods` |
| `associated_text` | `text` |
| `base_key` | `base` |
| A repeat | Sets the `repeat` field |
| A release | Comes only through `Sub.on_key_release` |
| A control character | Its letter with Ctrl: Ctrl+C is `Char 'c'` with `ctrl` |
| `Char` with Shift | The shifted character: Shift+a is `Char 'A'` |

Matching through `Shortcut` is shorter, and it survives these details:

```ocaml
(* 0.1.0 *)
Sub.on_key (fun k ->
  let d = Event.Key.data k in
  if d.modifier.ctrl && d.key = Char (Uchar.of_char 's') then Some Save else None)

(* 0.2.0 *)
Sub.on_keys [ (Shortcut.ctrl 's', Save) ]
```

Subscriptions take an `~id`, which pairs each one with its previous instance.
In 0.1.0 a timer that missed periods replayed every one of them: a 0.1 s timer
sent 100 messages after a 10 s stall. `Sub.every` now fires once and says how
many periods ended. With an id, `Sub.on_tick` also keeps its `dt` per
subscription. Before, an animation that started after an idle stretch took
the whole idle time as its first step.

Events now follow the DOM's order, capture then target then bubble, so the
`_all` subscriptions are gone. In 0.1.0 key subscriptions ran before the
focused element, and a `q` shortcut took the `q` typed into an input.

A mouse release reports `dragged`, which is true only when the pointer left
the pressed cell. Some terminals report motion inside a cell, and that turned
clicks into drags. A press reports its `clicks`, for double and triple clicks.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `Sub.every period (fun () -> msg)` | `Sub.every ~id period (fun n -> msg)`, where `n` is the number of periods that ended, `1` when on time |
| `Sub.on_tick f` | `Sub.on_tick ~id f` |
| `Sub.on_key_all`, `Sub.on_mouse_all`, `Sub.on_paste_all` | `Sub.on_key`, `Sub.on_mouse`, `Sub.on_paste` with `~phase:Sub.Capture` |
| `Event.Mouse.Up`'s `is_dragging` | `dragged`, `true` when the pointer left the pressed cell |
| `Event.Mouse.Drag`'s `is_dragging` | None; it was always `true` |
| A pattern on `Event.Mouse.Down { button }` | `Down` also carries `clicks`: name it or end the pattern with `; _` |

### Mosaic behaviour changes

These compile unchanged and act differently. The ones most likely to hurt come
first.

- An exception raised in a `Cmd.perform` now ends the program. `Mosaic.run`
  raises it again, with its backtrace, once the terminal is given back. In
  0.1.0 it killed only its own thread, and the program ran on without the
  result it was waiting for. Catch the exception inside the perform if the
  program should carry on.
- `Cmd.perform` and `Sub.worker` run on a helper domain by default. A
  CPU-bound perform on the loop's domain added tens to hundreds of
  milliseconds to each key. While the work runs, `Unix.fork` raises, because
  OCaml forbids forking once several domains run. Use `Unix.create_process`,
  which works, or choose another executor with `?process_perform`.
- A `Sub.on_key`, `on_mouse` or `on_paste` listener now sees events that an
  element prevented without stopping them. Listeners run after the elements, and in 0.1.0 they never
  heard a prevented event, so a listener could miss a press with no way to
  know. The DOM works the same way: preventing an event cancels its default
  action and still delivers it. To skip such events, test the event's
  `default_prevented` (`Event.Key.default_prevented`), as `Sub.on_keys` does.
  A focused widget stops the keys it acts on, and only a `~phase:Sub.Capture`
  listener sees those.
- Tab and Shift+Tab move focus to the next or previous element with
  `?on_focus_request`. If your program uses Tab for something else, prevent
  the key in the handler that uses it, or bind it with `Sub.on_keys`
  ([Focus](#focus)).
- A click no longer focuses an element by itself. It sends the element's
  `?on_focus_request` message, and the element is focused once the view
  declares it `~focused:true`. Focus has one owner now, your model, and a
  click asks it like anything else.
- Ctrl+C still quits, but as a default action. An element or listener that
  uses the key and calls `Event.Key.prevent_default` keeps the program
  running, and so does a `Sub.on_keys` binding of it. While text is selected
  in the view, Ctrl+C copies it instead. In 0.1.0, Ctrl+C with a word selected
  quit the program.
- A `canvas` whose drawing moves with `dt` needs `~live:true`, which asks for
  every frame. Frames now come only when something asks for one. Without the
  label, the canvas asks for no frame and moves only when something else
  redraws. In 0.1.0 a canvas that cleared itself asked for a frame on every
  frame, so a program that showed one never went idle.
- Three mistakes that 0.1.0 let pass now raise `Invalid_argument`. Two
  subscriptions that declare the same `~id` raise, because subscriptions are
  matched by id. A border width that is not a whole number of cells raises.
  A tab width below 1 raises too: the lower layers already raised for it,
  while `Text_buffer` clamped it to 1, so one value meant two things.
- A widget given no color draws with the theme's roles in the terminal's own
  colors. Pass its color labels, or restyle the roles with `themed`, to get
  0.1.0's look back ([Colors and themes](#colors-and-themes)).
- A program in primary mode leaves its last frame on the screen when it ends,
  with the cursor below it. A summary or the choice it made stays in the
  scrollback. `Cmd.keep_frame false` erases it as 0.1.0 did.
- `init` runs before the terminal is taken, so an exception it raises leaves
  the terminal untouched. In 0.1.0 an exception in the first subscriptions
  left the terminal raw and on the alternate screen.

### `mosaic.ui`

This section is for programs that drive `Mosaic_ui.Renderer` and its widgets
without the `Mosaic` runtime. In 0.1.0 each widget's `create` repeated every
label of its `Props.make`, two lists of 10 to 25 labels that we kept in step
by hand. Now `create` takes its node's labels, its callbacks, and the
`Props.t` that `Props.make` builds:

```ocaml
(* 0.1.0 *)
Box.create ~parent ~border:true ()

(* 0.2.0 *)
Box.create ~parent (Box.Props.make ~border:true ())
```

`Edit_buffer` became `Editor`, an immutable value. A mutable editor held in
the model made old models share editing state with new ones, so replaying a
model broke. The renderer's dispatch functions take the event's path, fixed
when dispatch starts, as in the DOM. In 0.1.0 the renderer called back into
its host to run the host's listeners first.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `Edit_buffer`, `Edit_buffer.create ?max_length s` | `Editor`, an immutable value: `Editor.of_string ?max_length s` |
| `Text_input` and `Textarea`'s `buffer`, `set_value`, `set_on_input`, `set_on_cursor`, and `Textarea.edit` | `~on_change` in `create`, `~state` in `Props.make`; read with `state`, set a new state with `apply_props`, replace the callback with `set_on_change` |
| `Canvas.create ?respect_alpha`, `Canvas.Props` | None; `Canvas.set_uchar ~blend:true` blends one cell |
| `Code.create ?spans`, `Code.set_spans` | `Code.Props.make ?syntax`, `Code.set_syntax` |
| `Scroll_box`'s `?sticky_scroll`, `?sticky_start`, `set_sticky_scroll`, `set_sticky_start`, `reset_sticky` | `?scroll` and `Scroll_box.set_scroll` with a `Scroll.t` |
| `?scrollbar_props`, `?vertical_bar_props`, `?horizontal_bar_props` | `?scrollbar_style`, `?vertical_bar_style`, `?horizontal_bar_style` |
| `Scroll_accel.tick ~now`, in milliseconds | `Scroll_accel.tick ~seconds` |
| `Tree.set_on_expand` | `Tree.set_on_change` |
| `Text_buffer.create ?width_method`, `width_method`, `set_width_method` | The `~width_method` of `Text_buffer.line_width` and `max_line_width` |
| `Text_buffer.line_spans` | `Text_buffer.styled_text` |
| `Text_surface.display_info` | `Text_surface.layout` |
| `Renderable.set_default_key_handler`, `set_paste_handler` | `Renderable.set_key_behavior`, `set_paste_behavior` |
| `Renderable.line_info`, `set_line_info_provider` | `Renderable.text_rows`, `set_text_rows_provider` |
| `~delta`, documented in milliseconds, of a render function (`Renderable.create ?render`, `set_render`), `set_on_frame`, `Renderer.render_frame` and `Renderer.add_frame_callback` | `~dt`, in seconds |
| `Renderer.create ?glyph_pool`, `Renderer.glyph_pool` | None |
| `Renderer.dispatch_key r k` | `Renderer.dispatch_key r (Renderer.focus_path r) (Event.Key.of_input k)` |
| `Renderer.dispatch_paste r s` | `Renderer.dispatch_paste r (Renderer.focus_path r) (Event.Paste.of_text s)` |
| `Renderer.dispatch_mouse`, `dispatch_scroll` | `Renderer.mouse_path`, then `Renderer.dispatch_mouse` on the path and event it returns |
| `Renderer.add_post_process`, `remove_post_process`, `clear_post_processes` | None |

The renderer owns the selection, which spans every widget, so the selection
calls change with it:

| 0.1.0 | 0.2.0 |
| --- | --- |
| `Selection`, `Renderer.selection`, `Renderable.set_selection`, `unset_selection` | The renderer's own selection: `Renderer.selection_text`, `take_selection_changes`, `clear_selection`; `Renderable.set_selectable` declares what a selection takes of a subtree: `` `Off ``, `` `Text `` or `` `Cells `` |
| `Text.set_selectable t b` | `` Renderable.set_selectable (Text.node t) (Some `Off) `` or `` (Some `Text) `` |
| `Code.set_on_selection` | `Renderer.take_selection_changes` |
| `Text.set_selection_bg`, `set_selection_fg`, `Text_surface.set_selection_bg`, `set_selection_fg` | The theme's `Highlight` role |
| `Text_surface.set_local_selection`, `update_local_selection` | None; the renderer maps the pointer to text |

## Matrix

### `matrix` runtime

Matrix has a new runtime. In 0.1.0 it called your callbacks from inside its
own loop, and the order of those calls was promised only by a comment. Now a
run is a scope that holds the terminal and gives it back however the scope
ends. Inside it, your application pulls each step with `Matrix.next`:

```ocaml
(* 0.1.0 *)
let () =
  let count = ref 0 in
  Matrix.run (Matrix.create ())
    ~on_input:(fun app ev ->
      match ev with
      | Matrix.Input.Key { key = Escape; _ } -> Matrix.close app
      | Matrix.Input.Key _ -> incr count
      | _ -> ())
    ~on_render:(fun app ->
      Matrix.Grid.draw_text (Matrix.grid app) ~x:0 ~y:0
        ~text:(Printf.sprintf "%d keys" !count))

(* 0.2.0 *)
let () =
  Matrix.run @@ fun app ->
  let rec loop count =
    match Matrix.next app with
    | Stopped | Event (Key { key = Escape; _ }) -> ()
    | Event (Key k) when Matrix.Input.Keystroke.is_interrupt k -> ()
    | Event (Key _) ->
        Matrix.request_redraw app;
        loop (count + 1)
    | Frame ->
        Matrix.draw app (fun grid ->
            Matrix.Grid.draw_text grid ~x:0 ~y:0
              ~text:(Printf.sprintf "%d keys" count));
        loop count
    | Event _ | Timer _ | Posted _ -> loop count
  in
  loop 0
```

The options of `create` moved into `Matrix.config`. A few pieces of the old
runtime changed shape for reasons of their own. `Matrix.release` replaces
suspending, a state that every call had to honor. A program with its own event
loop may need to stop a run that has not begun yet, which `Matrix.Stop` allows.
The debug overlay and frame dumps answer to `MATRIX_DEBUG`, so you can turn
them on in a Mosaic program without rebuilding it. `static_write` takes a
grid, which knows its rows, where it took a row count you had to get right
yourself.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `Matrix.create`, `Matrix.attach`, `Matrix.run ~on_render` | `Matrix.run ?input ?output ?env ?stop ?config (fun app -> ...)`, with steps from `Matrix.next` |
| `on_input` | An `Event e` step |
| `on_resize` | An `Event (Resize (cols, rows))` step |
| `on_render` | A `Frame` step that calls `Matrix.draw` |
| `on_frame ~dt` | `Matrix.frame_dt app` |
| The options of `create` and `attach` | `Matrix.config ?mode ?raw_mode ?mouse ?bracketed_paste ?kitty_keyboard ?key_releases ?fps ?explicit_width ?min_tui_height ?caps ?color ()`, given as `run ~config` |
| `?target_fps` | `~fps` |
| `?mouse_enabled:false` | `` ~mouse:`Off ``; `?mouse` takes a mode, not an option |
| `?initial_caps` | `~caps` |
| `?output` | `Matrix.run ~output:fd` |
| `?cursor_visible:true` | `Matrix.set_cursor app ~visible:true` before the first frame |
| `?input_timeout` | A timer: `Matrix.at app (Matrix.now app + ns)`, in integer nanoseconds, cancelled with `Matrix.cancel` |
| `?exit_on_ctrl_c` | Test `Matrix.Input.Keystroke.is_interrupt` on a `Key` event and return from the run |
| `?focus_reporting`, `?respect_alpha`, `?signal_handlers`, `Matrix.install_signal_handlers`, `?resize_debounce`, `?start_idle` | None. Focus reports are always on, each grid call chooses its compositing, and the run takes the signals left at their default action |
| The I/O functions of `attach` (`write_output`, `read_events`, `terminal_size`, ...) | `Matrix.run ?input ?output ?env` on any terminal's descriptors; `Matrix.step`, `Matrix.fds` and `Matrix.deadline` under an event loop of your own; `Matrix_test` in tests |
| `type Matrix.app` | `type 'msg Matrix.app`; another thread calls only `Matrix.post` |
| `Matrix.grid`, `prepare`, `submit`, `run ?primary_required_rows` | `Matrix.draw app ?primary_required_rows (fun grid -> ...)` on a `Frame` step |
| `Matrix.close`, `Matrix.stop` | Return from the function given to `run`, or `Matrix.Stop.request` on the `Matrix.Stop.t` given as `run ~stop` |
| `Matrix.running` | The run is over once a step is `Stopped` |
| `Matrix.start`, `pause`, `request_live`, `drop_live` | Call `Matrix.request_redraw` from each `Frame` while animating, and stop calling it to pause |
| `Matrix.suspend`, `Matrix.resume` | `Matrix.release app f`; `Matrix.stop_job` suspends the job as Ctrl+Z does |
| `?debug_overlay`, `?debug_overlay_corner`, `?debug_overlay_capacity`, `set_debug_overlay`, `toggle_debug_overlay`, `debug_overlay_corner` | `MATRIX_DEBUG=overlay` in the environment, or `Matrix.Debug_overlay.create ?corner ?capacity ()` with `record` and `draw` inside `Matrix.draw` |
| `?frame_dump_every`, `?frame_dump_dir`, `?frame_dump_pattern`, `?frame_dump_hits`, `configure_frame_dump` | `MATRIX_DEBUG=frames=DIR` in the environment, or `Matrix.dump_frame ?hits ?dir ?pattern app` inside `Matrix.draw` |
| `Matrix.static_write app ~rows s` | `Matrix.static_write ?preserve_live_region ?rows app grid`, the rows drawn into a `Grid.t` |
| `Matrix.set_cursor_color app ~r ~g ~b ~a` | `Matrix.set_cursor_color app ~r ~g ~b` |
| `kitty_keyboard`'s `` `Disabled `` | `` `Off `` |
| `kitty_keyboard`'s `` `Enabled flags `` | `` `Auto ``, which pushes the protocol once the terminal answers |

### `matrix.input`

The old parser kept a clock of its own. It typed the rest of a paste as keys
after 5 seconds of silence, and the rest of a long reply past 4,096 bytes. The
new decoder has no clock, and it tells replies from keys by what the terminal
still owes. `Input` now holds `Keystroke`, `Mouse_report`, `Reply` and
`Decoder`, and a key match reads the new record:

```ocaml
(* 0.1.0 *)
| Input.Key { key = Char c; modifier = { ctrl = true; _ }; _ } -> ...

(* 0.2.0, in a run *)
| Event (Key { key = Char c; mods = { ctrl = true; _ }; _ }) -> ...
```

A paste now arrives in pieces, with no size limit. 0.1.0 capped it at 16 MiB.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `Input.t` | `Matrix.event` in a run, `Input.Decoder.event` when decoding |
| `Key` (press, repeat or release) | `Key`, a press or repeat (`repeat` field), and `Key_release` |
| `Mouse`, `Scroll` | `Mouse` of an `Input.Mouse_report.t` |
| `Focus`, `Blur` | `Focus_in`, `Focus_out` |
| `Paste s` | `Paste_start`, `Paste_data s`, `Paste_end` |
| `Clipboard`, `Osc` | `Reply` events, which come only for queries sent with `Matrix.query` |

A key has the same value whichever encoding the terminal uses
([Events and subscriptions](#events-and-subscriptions)). `Input.Keymap` moved
to Mosaic, because matching keys to bindings is Mosaic's job.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `Input.Key.t` | `Input.Keystroke.key` |
| `Unknown n` | `Unidentified n` |
| The media and volume keys (`Media_play`, `Media_next`, `Volume_up`, ...) | `Media Play`, `Media Track_next`, `Media Volume_up`, ... |
| The side-specific modifier keys (`Shift_left`, ...), `Iso_level3_shift` | `Modifier Left_shift`, ..., `Modifier Level3_shift` |
| `KP_*` | The main key with `keypad = true` (`KP_enter` is `Enter`) |
| `Line_feed` | `Char 'j'` with Ctrl |
| A control character | Its letter with Ctrl: Ctrl+C is `Char 'c'` |
| `Char` with Shift | The shifted character: Shift+a is `Char 'A'` |
| `Input.Key.event` | `Input.Keystroke.t`: `modifier` → `mods`, `associated_text` → `text`, `base_key` → `base`, `event_type` → the `repeat` field and the `Key_release` event, `shifted_key` → `Char` itself |
| `Input.Key.modifier`, `no_modifier` | `Input.Keystroke.mods`, `no_mods` |
| `Input.Key.make`, `Key.of_char`, `Input.key`, `char`, `key_event`, `char_event`, `press`, `repeat` | `Input.Keystroke.make ?mods ?text ?base ?keypad ?repeat key` |
| `Input.release` | A `Key_release` event of an `Input.Keystroke.release` record |
| `equal` and the `equal_*` functions | `( = )` |
| The `is_*` predicates, `Input.match_ctrl_char` | A pattern match |
| `Key.pp`, `Key.pp_event`, `Input.pp` | `Keystroke.pp_key`, `Keystroke.pp`, `Matrix.pp_event` or `Decoder.pp_event` |
| `Input.Keymap` | `Mosaic.Shortcut` |

A mouse report is a plain record:

| 0.1.0 | 0.2.0 |
| --- | --- |
| `Input.Mouse.event` | `Input.Mouse_report.t`, a record `{ kind; x; y; shift; alt; ctrl }` |
| `Input.mouse_press`, `mouse_release`, `mouse_motion` | A `Mouse_report.t` record |
| `Button_press` | `Press b` |
| `Button_release` | `Release (Some b)`; a legacy release is `Release None` |
| `Motion` | `Motion (Some b)` or `Motion None` |
| The wheel buttons | `` Scroll `Up `` and the other directions |
| `Button n` | `Extra n` |

A reply decodes as `Reply` only when a query owes it, so the terminal's answers
no longer show up as key presses:

| 0.1.0 | 0.2.0 |
| --- | --- |
| `Input.Caps.event` | `Input.Reply.t` |
| `Device_attributes` | `Primary_attributes` or `Secondary_attributes` |
| `Mode_report` | One `Mode` per mode |
| `Pixel_resolution` | `Text_area_pixels` |
| `Cursor_position (row, col)`, counting from 1 | `Cursor { row; col; page }`, counting from 0 |
| `Xtversion` | `Version` |
| `Kitty_graphics_reply` | `Graphics` |
| `Kitty_keyboard` | `Keyboard_flags` |
| `Input.Parser` | `Input.Decoder` |
| `Parser.feed p buf off len ~now ~on_event ~on_caps` | `Decoder.feed d buf ~off ~len f` |
| `Parser.deadline`, `drain` | Call `Decoder.idle` when the terminal has been silent |
| `Parser.pending` | `Decoder.pending`, a variant |
| Replies decoded whenever they come | A reply decodes as `Reply` only after `Decoder.expect` declares it owed |

### `matrix.terminal`, `matrix.terminfo` and `matrix.tty`

Everything that calls the operating system moved to the new `Tty` module.
`matrix.terminal` used to mix pure protocol facts with calls that link `unix`
and C, so a program that wanted only the facts had to link them too. Finding a
terminfo file also moved to `Tty`. It now searches as ncurses does, and reads
`TERMINFO` and `TERMINFO_DIRS`, which 0.1.0 ignored.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `Terminal.capabilities`'s `term` | Read `TERM` |
| `Terminal.capabilities`'s `rgb` | `colors`: `` `Ansi16 ``, `` `Ansi256 `` or `` `Truecolor `` |
| `Terminal.capabilities`'s `kitty_keyboard`, a `bool` | An `int option`, the flags last reported |
| `Terminal.capabilities`'s `unicode_width` | A `Matrix_text.width_method` |
| A record literal of capabilities | Also gives `dumb`, `eager_wrap`, `lifts_resting_cursor`, `grapheme_mode`, `graphics`, `notification`, `meta_sends_escape`, `multiplexer` and `identity` |
| `Terminal.make ~output ?tty ?initial_caps ()` | `Terminal.make ~output ?tty caps` |
| `Terminal.terminal_info`, `set_terminal_info` | The capabilities' `identity` field |
| `Terminal.apply_capability_event` | `Caps.add`, then `Caps.derive` and `Terminal.set_capabilities` |
| `Terminal.probe` | `Caps.ask`, `add`, `cursor`, `answered` and `next`, which `Matrix.run` drives |
| `Terminal.set_unicode_width` | `Terminal.enable_grapheme_mode` |
| `type Terminal.unicode_width` | `Matrix_text.width_method` |
| `type Terminal.cursor_style` | `Ansi.cursor_style` |
| `Terminal.set_raw`, `restore` | `Tty.raw`, `Tty.restore` |
| `Terminal.size` | `Tty.window`, an option |
| `Terminal.is_tty`, `flush_input` | `Tty.is_tty`, `Tty.flush_input` |
| `Terminal.enable_vt` | `Tty.enable_vt`, whose `Tty.console` goes to `Tty.restore_console` before the program ends |
| `Terminfo.load ?term ()` | `Tty.terminfo name`, or `Matrix_terminfo.of_string` on a file's contents |
| `` `Parse_error `` | `` `Invalid `` |
| `Terminfo.get t cap` and the `'a cap` constructors | `Matrix_terminfo.flag`, `number` and `string` by capability name (`Cursor_position` is `string t "cup"`); `expand` for parameters, `pieces` for delays |

### `matrix.ansi`

`Ansi.Color.t` is abstract, so that a palette color stays a palette color all
the way to the terminal. The constructors become values, and a match on a
color goes through its intent:

```ocaml
(* 0.1.0 *)
Ansi.Style.make ~fg:Ansi.Color.Cyan ~bg:(Ansi.Color.Rgb { r = 30; g = 30; b = 46 }) ()

(* 0.2.0 *)
Ansi.Style.make ~fg:Ansi.Color.cyan ~bg:(Ansi.Color.of_rgb 30 30 46) ()

(* 0.2.0: reading a color *)
match Ansi.Color.intent c with
| Default -> ...
| Indexed i -> ...
| Rgb -> let r, g, b, a = Ansi.Color.to_rgba c in ...
```

`Ansi.render` takes a line of spans. Matrix had three types for a run of text
in one style. `render` also sanitizes the line, so a string
from elsewhere can't move the cursor or restyle what follows:

```ocaml
(* 0.1.0 *)
Ansi.render [ (bold, "Status: "); (Ansi.Style.default, msg) ]

(* 0.2.0 *)
Ansi.render [ Ansi.span ~style:bold "Status: "; Ansi.span msg ]
```

A span's text is written as `Ansi.sanitize` leaves it, so a control function
inside it is not written. Write one apart with `Ansi.literal` or
`Ansi.to_string`.

`Ansi.parse` was a second name for `Ansi.Parser.parse`, and it is gone.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `Black` ... `Bright_white` | `Color.black` ... `Color.bright_white` |
| `Extended i` | `Color.indexed i` |
| `Rgb`, `Rgba` | `Color.of_rgb`, `Color.of_rgba` |
| `Color.default` | `Color.transparent` for paint that lets what is under it show, or `Color.default_fg` and `Color.default_bg` for the terminal's colors |
| `Color.blend ?mode ~src ~dst ()` | `Color.blend ~src ~dst` |
| `Color.downgrade ?level c` | `Color.to_depth depth c` |
| `Color.pack`, `unpack` | `Color.Packed.encode`, `decode` |
| `Ansi.parse` | `Ansi.Parser.parse` |
| `Ansi.to_buffer t buf` | `Buffer.add_string buf (Ansi.to_string t)` |
| `Ansi.csi_u_on`, `csi_u_off` | `Ansi.csi_u_push ~flags:1`, `Ansi.csi_u_pop` |
| The query `Color_scheme_query` | `Color_scheme_report` |
| `Ansi.Parser.token` | Syntax tokens: `Text`, `Control`, `Esc`, `Csi`, `Open`, `Data`, `Close` |
| `Parser.control`, `Parser.sgr_attr` | Read a `Csi` by its `final` with `Parser.Params`; apply SGR with `Style.apply_sgr` |
| `Parser.has_pending p` | `not (Parser.at_boundary p)` |
| `Parser.pending` | None |
| `Parser.feed` returning `unit` | It returns the number of bytes it read; ignore it unless you call `Parser.stop` |
| `Ansi.Writer.make buf` | `Writer.of_bytes buf`, or `Writer.create n` |
| `Writer.pos`, `Writer.slice` | `Writer.len`, `Writer.contents` |
| `Writer.make_counting` | Write into `Writer.create` and read `len` |
| `Sgr_state.update ~fg_r ~fg_g ... ~bg_a` | `Sgr_state.update ~fg ~bg`, taking `Ansi.Color.t` |

### `matrix.grid`, `matrix.screen` and `matrix.image`

The grid stores graphemes itself now, and each drawing call says whether it
blends. A grid-wide flag used to decide that for every call, mixing copying
with compositing. `Image.render` became `Matrix_image.draw`, with the image
first. There were two entry points with opposite argument orders, and
elsewhere in Matrix "render" means writing ANSI while "draw" means writing
cells into a grid.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `Grid.create ?glyph_pool ?respect_alpha` | `Matrix_grid.create ~width ~height ?width_method ()`; `create_like` shares a grid's storage |
| `Grid.glyph_pool`, `is_inline`, `respect_alpha`, `set_respect_alpha` | None. Compositing is chosen per call: `draw_region` blends, `blit_region` copies, `set_uchar ~blend` |
| `Grid.set_cell ~glyph` | `set_uchar` or `set_grapheme` |
| `get_code` | `get_codepoint` |
| `get_glyph` | `get_text`, or `cell_text_length` and `blit_cell_text` |
| `get_fg_r` and the other channel reads | `get_fg` and `get_bg`, with `Ansi.Color.to_rgba_f` |
| `get_background` | `get_bg` |
| `hyperlink_url`, `hyperlink_url_direct` | `get_link`, which returns the URL |
| `Grid.scroll g ~top ~bottom n` | `Grid.shift` of the region with `~dx:0 ~dy:(-n) ~color` |
| `Grid.diff_cells` | `Grid.cells_equal` |
| `Grid.to_ansi ?reset` | `Grid.to_ansi` |
| `line_glyphs`, `default_line_glyphs`, `ascii_line_glyphs`, `draw_line ?glyphs` | `line_symbols`, `default_line_symbols`, `ascii_line_symbols`, `draw_line ?symbols` |
| `Screen.create ?glyph_pool ?respect_alpha ?mouse_enabled` | `Matrix_screen.create ?width_method ?cursor_visible ?explicit_width ()` |
| `Screen.grid`, `hit_grid` | `next_grid`, `next_hit_grid` |
| `Screen.render ?height_limit` | `Screen.render ~viewport` |
| `Screen.render_to_bytes` | `Screen.render_into` with `Ansi.Writer.of_bytes` |
| `set_cursor_visible`, `set_cursor_position`, `clear_cursor_position`, `set_cursor_style`, `set_cursor_color`, `reset_cursor_color`, `cursor_info` | `Matrix_screen.set_cursor` and `cursor`, with the record `{ position; style; blinking; color; visible }`; `position` counts from 0 |
| `Screen.post_process`, `remove_post_process`, `clear_post_processes` | None: draw an effect into `next_grid` before rendering |
| `record_runtime_metrics` and the timing fields of `frame_metrics` | None: time a render with a clock around it |
| `Screen.set_mouse_enabled` | `Matrix_terminal.set_mouse_mode` |
| The type `Hit_grid.rect` | `Matrix_grid.region` |
| `Hit_grid.blit` | `Hit_grid.draw` |
| `Image.string`, `Image.line` | `Matrix_image.text` |
| `Image.render ?hits ?x ?y grid img` | `Matrix_image.draw ?hits ?x ?y img grid` |

### `matrix.text` and `matrix.vte`

`matrix.text` takes over measuring and segmenting text from `matrix.glyph`.
`Vte.feed` returns the replies of the emulated terminal. They used to wait in a
queue that nothing drained, so the queue grew without bound. `Vte.create`
takes a `?palette` because the default colors had two owners: a program's
OSC 11 changed what it was told, and the screen kept showing the old color.

| 0.1.0 | 0.2.0 |
| --- | --- |
| `Glyph.String.measure`, `measure_sub`, `grapheme_count`, `iter_graphemes`, `iter_line_breaks`, `iter_wrap_breaks` | The same names in `Matrix_text` |
| `iter_grapheme_info` | `Matrix_text.iter_units` |
| The width method `` `No_zwj `` | None |
| `Vte.create ?glyph_pool ?respect_alpha` | No such arguments |
| `Vte.create ?default_fg ?default_bg` | `?palette`, for example `{ Ansi.Palette.default with foreground = (r, g, b) }`. A cell written with no color holds `Ansi.Color.default_fg` or `default_bg`, where it held white or black; resolve it through `Vte.palette` |
| `Vte.feed`, `feed_string` and `resize` returning `unit` | They return the replies: `ignore` them, or write `Matrix_input.Reply.encode replies` |
| `Vte.auto_wrap_mode`, `insert_mode`, `cursor_key_mode`, `bracketed_paste_mode`, `origin_mode`, `cursor_visible`, `is_alternate_screen` | The fields of `Vte.modes t` |
| `Vte.set_cursor_pos`, `set_cursor_visible`, `scroll_up`, `scroll_down` | Feed the sequence: `Ansi.cursor_position`, `Ansi.enable Cursor_visible`, `Ansi.scroll_up` |
| `Vte.scrollback_lines t` | `List.init n (fun i -> Vte.text t (i - n))` with `n = Vte.scrollback_size t` |
| `Vte.render_with_scrollback t ~offset dst` | `Vte.blit_view t ~offset dst` |

### `matrix.charts`

We rewrote the charts. The old ones could cut off ticks that left the window,
and rounding could put a braille dot a cell away from the tick of its value.
Hit tests ignored areas and fills. A chart is now an immutable figure of marks, laid out
in a region and then drawn:

```ocaml
(* 0.1.0 *)
let chart =
  Matrix_charts.(empty () |> line ~x:fst ~y:snd points |> with_title "Latency")
in
ignore (Matrix_charts.draw chart grid ~width:60 ~height:15)

(* 0.2.0 *)
let fig =
  Matrix_charts.(
    title "Latency" (layer [ line ~x:fst ~y:snd points; axis ~title:"ms" "y" ]))
in
Matrix_charts.(draw (layout ~width:60 ~height:15 fig) grid ~x:0 ~y:0)
```

| 0.1.0 | 0.2.0 |
| --- | --- |
| `empty () \|> add m`, `make [ ms ]`, the chart-building `line`, `scatter`, `bar`, ... | `layer [ ms ]` |
| `with_title s` | `title s` |
| `with_theme t` | `layout ~theme:t` |
| `with_x_scale`, `with_y_scale` | `scale "x"`, `scale "y"` |
| `with_y2_scale`, `` ~y_axis:`Y2 `` | `rename "y" n` on the marks, and `` axis ~side:`Right n `` |
| `~style` on a mark | `~color:(const c)` and `attrs` |
| `~label` | `~color:(series label)` |
| `~id` | `name` |
| `` ~resolution:`Braille2x4 ``, `` `Block2x2 ``, `` `Wave `` | `` ~dots:`Braille ``, `` `Quadrant ``, `` `Steps `` |
| `scatter ~mode` | `dot ~dots` |
| `bar ~direction` | `bar_y` or `bar_x` |
| `stacked_bar` | Rows of one category under `bar_y ~color:(cat f) ~label` |
| `heatmap` | `raster` |
| `fill_between` | `area ~y2` |
| `shade`, `column_bg` | `rect` |
| `rule` | `hline`, `vline` |
| The type `Mark.ohlc` | `ohlc`, which `candles` takes |
| `Axis`, `Gridlines`, `manual_frame`, `with_axes`, `with_grid`, `with_frame` | `axis ?side ?show ?title ?grid ?format ?spacing ?style ?tick_style ?label_style ?title_style`, `frame`, `margins` |
| A tick count | `?spacing`, or `Scale.linear ~ticks` |
| A border around the region | Drawn by the host: `Matrix_grid.draw_box`, or a Mosaic `box ~border` |
| `Label_format.mmdd_utc`, `hhmmss_utc` | `Scale.time` |
| `Charset`, `Theme.with_charset` | `Theme.t`'s `glyphs` and the presets `Theme.light_glyphs`, `heavy_glyphs`, `rounded_glyphs`, `ascii_glyphs` |
| `Legend` | `legend` and `Layout.legend` |
| `Transform.ema`, `sma`, `gaussian` | `ema`, `sma`, `gaussian` |
| `draw chart grid ~width ~height` | `draw (layout ~width ~height fig) grid ~x ~y` |
| `View.window` records and their functions (`zoom_around`, ...) | A window is a `(min, max)` pair per scale: `View.set`, `View.window`, `View.zoom`, `View.pan`, `View.clamp` |
| `Layout.zoom_view_around_px`, `pan_view_by_px`, `clamp_view` | `Layout.zoom`, `Layout.pan`, `View.clamp` |
| `data_of_px`, `px_of_data` and the category conversions | `Layout.value`, `Layout.position` |
| `hit_test` | `Layout.hit`, whose row indexes the program's own array |
| `Overlay` | The `?overlays` of `draw`, in cells |
| `Sparkline` | A `float list`, newest first, kept in the model (`List.take n (v :: l)`) and drawn with `spark` |
| A sparkline's fixed maximum | `scale "y" (Scale.linear ~domain:(0., max) ())` |

The old `Sparkline` was a mutable buffer that programs kept in an Elm
model. With `~auto_max:true` it kept the largest value ever pushed. `spark`
scales to the largest value of the list the model keeps, so its top falls once
that value leaves the list. Give an explicit domain for a ceiling that never
falls.

### Matrix behaviour changes

These compile unchanged and act differently. The ones most likely to hurt come
first.

- A run raises `Failure` on Windows, and so does `Mosaic.run`, because the
  runtime has no Windows path yet. `matrix.pty` is POSIX-only. We removed its
  ConPTY backend because it was unsound: every read and write treated a custom
  block as a handle, and closing either side of a pty closed both. We are
  working on Windows support for a later release.
- A run draws a frame only when you ask with `Matrix.request_redraw`. It also
  asks for frames itself, at the start, after a resize and after a release.
  0.1.0 drew 30 frames a second, so a program that relied on that
  stops updating its screen. Call `request_redraw` whenever your view changes,
  and again from each `Frame` while something animates. An idle run now costs
  no wakeups.
- Ctrl+C means nothing to Matrix. In raw mode it is a `Key` event like any
  other, and your application decides what it does. To quit on it, test
  `Input.Keystroke.is_interrupt` and return from the run, as the
  [runtime example](#matrix-runtime) does.
- SIGINT, SIGTERM, SIGHUP and SIGQUIT give the terminal back and are raised
  again, so the process ends with the signal's status, 143 for SIGTERM. `run`
  does not return. 0.1.0 called `exit` from handlers of its own, with a wrong
  status, and replaced any handler the program had installed. The run now
  takes a signal only while its action is the default. If you want `run` to
  return, install your own handler first.
- The runtime installs no handler for uncaught exceptions, since that choice
  belongs to the process. The run gives the terminal back and raises the
  exception again.
- Frames are written at the terminal's color depth and follow the user's color
  settings. In 0.1.0 a run wrote 24-bit or 256 colors whatever the user asked,
  and a pipe got escape codes. Now a 16-color terminal gets 16 colors, and an
  output that is not a terminal gets no style. `NO_COLOR` keeps attributes and
  drops colors. The run reads `FORCE_COLOR`, `CLICOLOR_FORCE` and `CLICOLOR`
  too. To write colors whatever the user set, pass
  `` Matrix.config ~color:`Always ``.
- An RGB color reduced to 256 colors maps to entries 16 to 255, so
  `rgb(255,0,0)` is `38;5;196` where it was `38;5;9`. Entries 0 to 15 are the
  user's own palette, and a program does not know their colors.
- A drawn space erases the text under it. The default style has a transparent
  background, so in 0.1.0 drawing `" b"` over `"XYZ"` left `"XbZ"`. To tint
  cells without erasing them, use a translucent `fill_rect`.
- `Grid.blit_region` copies colors as they are, where it blended a translucent
  source. Use `draw_region` to blend.
- `fill_rect` with a transparent color changes nothing, where it cleared the
  text. Use `clear_rect` to clear.
- Cell accessors raise `Invalid_argument` on an index out of bounds. Such an
  index used to read another cell, or memory past the grid's planes.
- The cursor starts hidden in both modes, and the `?cursor_visible` option is
  gone. Your application owns the frame, so the cursor is yours to show with
  `Matrix.set_cursor app ~visible:true`.
- In primary mode, the last frame stays on the screen when the run ends, with
  the cursor below it. `Matrix.keep_frame app false` erases it.
- In primary mode, static rows wrap as ordinary output, where they were cut at
  the right margin. A blank row stays above the live region while the run
  lasts. Drawing the live region at absolute rows lost lines in resize storms
  on tmux, VTE and WezTerm, and lost lines that other processes wrote.
- Text-default pictographs such as ❤ (U+2764) and ⚠ (U+26A0) take one cell
  unless VS16 follows them, as Unicode 18.0 says. Terminals draw them in one
  cell. Matrix counted two, which shifted the rest of the row.
- `Pty.spawn` raises `Unix.Unix_error` when the program cannot run. Before,
  the child exited with status 127, which a program can also return.
- A child spawned with `matrix.pty` starts with default signal actions and only
  its standard descriptors, and both descriptors of `Pty.open_pty` are
  close-on-exec. A child used to inherit the parent's ignored signals and open
  pipes, so a reader never saw the end of its input.
- `Pty.close ~wait:false` keeps the child until `Pty.wait` reaps it. The old
  handle could signal a process id that the system had given to another
  process.
- `Screen.resize` leaves every buffer blank and makes the next render full,
  where it kept the grid's contents. After a resize, the old diff could leave
  stale cells on the screen.
- `Terminal.reset_state` puts back only the modes the handle changed. It used
  to clear the window title and the cursor's color and style for every
  application, even ones that never set them.
- `Vte.cursor_pos` never reports column `cols`. It did while a wrap was
  pending, and `Vte.last_column` now tells that state.

## Toffee

Toffee's geometry is unboxed. `Size`, `Rect` and `Point` are polymorphic, so a
float size was a block pointing at two boxed floats, and reading a layout
allocated. Layouts and measure functions now use `Sizef`, `Pointf` and
`Rectf`, records of plain floats. All 2,084 layout tests give the same results.

| 0.1.0 | 0.2.0 |
| --- | --- |
| A measure function returning `float Size.t` | It returns a `Geometry.Sizef.t`: `Sizef.{ width; height }`, `Sizef.make w h`, `Sizef.zero` |
| `Layout.t`'s `size`, `location`, `border`, `padding`, `margin` and `scrollbar_size`, of the polymorphic `Size`, `Point` and `Rect` types | `Sizef.t`, `Pointf.t` and `Rectf.t`: a pattern names the float module (`let Sizef.{ width; height } = Layout.size l`); `layout.size.width` reads as before |
| `Layout.content_size`, `Layout_output.content_size` | `scrollable_overflow`, a `Scrollable_overflow.t`; for scroll extents, `Layout.scroll_width` and `scroll_height` |
| The float operations of `Size` (`zero`, `add`, `sub`, `max`, `min`, `clamp`, `maybe_add`, ...) | The same names in `Sizef`. `Size.max_concrete`, which took `Float.max`, is `Sizef.max` |
| The float operations of `Rect` (`zero`, `add`, `horizontal_axis_sum`, `vertical_axis_sum`, `sum_axes`, `grid_axis_sum`) | The same names in `Rectf` |
| The float operations of `Point` (`zero`, `add`, `sub`, `mul_scalar`, ...) | The same names in `Pointf` |
| Converting between the two | `Sizef.of_size`, `to_size`, `some`; `Rectf.of_rect`, `to_rect`; `Pointf.of_point`, `to_point` |
| `Size.equal_option eq` | `Size.equal_with (Option.equal eq)` |
| `Available_space.maybe_set` | `Available_space.set_or_self` |
| `Cache_tree.cache_get tree node ~known_dimensions ~available_space ~run_mode`, `cache_store` likewise | `Cache_tree.cache_get tree node inputs`, `cache_store`, with the node's `Layout_input.t` |
| `Cache.get`, `Cache.store` | Take the node's style, then the `Layout_input.t`: `Cache.get cache style inputs` |
| `Cache.compute_cache_slot`, `is_roughly_equal`, `cache_entry`, the fields of `Cache.t` | None; `Cache.t` is abstract |
| A `Toffee_tree.ROUND_TREE` implementation | Adds `is_rounded`, and its `set_final_layout` takes `~x ~y`, the parent's unrounded absolute position |
| A hand-built `Layout_input.t` record | Also sets `known_dimensions_are_definite`, or comes from `Layout_input.make` |
| `Layout.make ~content_size`, `Layout_output.make ~content_size`, `Layout_output.from_sizes_and_baselines` | `~scrollable_overflow`, and a `Scrollable_overflow.t` for `from_sizes_and_baselines` |
| `Compute_helpers.compute_content_size_contribution` | `Toffee_compute_helpers.add_overflow_contribution`, then `finalize_overflow` |

```ocaml
(* 0.1.0 *)
let measure _known _available _node _context _style =
  Toffee.Geometry.Size.{ width = 12.; height = 1. }

(* 0.2.0 *)
let measure _known _available _node _context _style =
  Toffee.Geometry.Sizef.{ width = 12.; height = 1. }
```

The rest of the table matters if you write your own layout algorithm or tree.
`content_size` became `scrollable_overflow`, which may reach before the
origin, as CSS defines it. The old extents could not scroll a `row-reverse`
container at all. The cache keeps up to 24 entries keyed on the node's whole
`Layout_input.t`, where it had nine fixed slots that evicted each other. It
also reads the node's style to know whether the parent's height matters. A
measurement keyed on the parent's width alone answered a query made under
another height. `ROUND_TREE` adds `is_rounded` so that rounding can skip the
subtrees that did not move. Rounding the whole tree on every pass made up 92%
of a keystroke's allocation behind a long Mosaic transcript.

### Toffee behaviour changes

These compile unchanged and act differently. The ones most likely to hurt come
first.

- A flex or grid root without a width, laid out in a definite available width,
  fills that width less its margins. It used to take the width of its content,
  so its children had no width to resolve percentages against. A block root
  already filled the width, and browsers do the same. To keep the content's
  width, give the root a width, or lay it out in
  `Available_space.max_content`.
- When a leaf is measured, its measure function receives the known size of its
  content box, clamped by the leaf's minimum and maximum sizes. It used to
  receive the border box's size. A padded text was then measured at one height
  and laid out at another, so the line after it drew over it. Your measure
  function must give the same size for a known dimension as for that dimension
  given as definite available space.
- A child resolves percentages of a size that its flex container derived from
  the item's content as if that size were unknown, as browsers do. CSS Flexbox
  says such a size is not definite. So `height: 50%` inside an item the
  container does not stretch no longer takes half of it.
- A root with margins is placed at its left and top margins, where it was at
  (0, 0). Chromium places the root's margin box at the origin too.
- A rounded location is the node's rounded position in the root's coordinates
  minus its parent's, as in Taffy. Before, the size and the location were
  rounded from different edges and disagreed, so boxes at fractional offsets
  left gaps or overlapped. They now tile without gaps. A box under a parent at
  a fractional position may move by one cell, and sizes are unchanged.
