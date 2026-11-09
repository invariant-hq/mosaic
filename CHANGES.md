# Changelog

## [Unreleased]

### Mosaic

- Mosaic runs on the Windows console again, as Matrix does (below).
  `Mosaic_eio.run` works there too: the program runs on a domain with no Eio
  loop, and cancelling the calling fiber still stops it. `Mosaic.fds` raises
  `Invalid_argument` on Windows.

### Matrix

Matrix runs on the Windows console again, on Windows 10 1803 or later. The run
reads the console's input records itself, so a resize reaches it, and keys,
mouse reports, pastes and focus changes arrive as the sequences a VT terminal
sends. Ctrl+C and Ctrl+Break reach the run as SIGINT, and closing the console
as SIGHUP. When the run ends, on one of these too, or the process aborts, the
console gets back every mode and code page the run changed.

#### Also new

- `Tty.Signal.wait` and `Tty.Signal.read` wait for and read a holder's input,
  on every system, and `Matrix.Device.unix` is built on them. On a Windows
  console, `read` gives the text the keys carry as UTF-8 and receives a resize
  as SIGWINCH.
- `Tty.raw`, `keys`, `no_echo`, `restore` and `flush_input` work on a Windows
  console.
- `Pty.close` keeps the program's status when it reaps the child, and
  `Pty.wait` or `Pty.poll` then gives it, where they used to raise
  `Invalid_argument`.

#### Changes the compiler won't catch

- On Windows the input is a console or a file. A run whose input is a pipe
  raises `Invalid_argument` at its first wait, and `Matrix.fds` raises
  `Invalid_argument`, since no descriptor stands for the console: drive the run
  with `Matrix.next`, or with `Matrix.run` on a domain of its own.
- `Tty.is_controlling` is `true` for a handle of the process's console on
  Windows, so one run at a time holds it.
- `Tty.restore` blocks SIGTTOU while it sets the settings, so a process in the
  background no longer needs to block it itself.
- `Pty.terminate` and `Pty.kill` on a handle that `Pty.close` has reaped do
  nothing, as on a child that has exited, where they used to raise
  `Invalid_argument`.
- On OCaml 5.4 and 5.5, Ctrl+Break on Windows ends the process even when a
  SIGINT handler is installed (ocaml/ocaml#15267). While a run holds the
  console, Matrix takes it as SIGINT and gives the console back first.

#### Breaking changes

- `Pty.spawn` runs the program under a keeper that leads the pseudo-terminal's
  session, so the program's output outlives its exit on every kernel: FreeBSD
  dropped the output the master had not read when the program, as the
  session's leader, exited (#114). `Pty.pid` is the program's id and process
  group, not a child of the calling process: reap it with `Pty.wait`, or with
  the new `Pty.poll` without blocking, never with `Unix.waitpid`.
  `Pty.terminate`, `kill` and `close` signal the program's process group, so
  the processes it started in it too, as the terminal's interrupt key does;
  the hangup's `SIGHUP` reaches the program on every kernel, with a `SIGCONT`
  if it was stopped. A program on a spawned terminal is a job of its session,
  which a stop signal stops.
- The job of a spawned program ends with it: once the program has exited and
  the master has read its output, the keeper kills what the program left in
  its process group. A process that ignores `SIGHUP` and `SIGTERM` no longer
  outlives `Pty.close`, and a background job no longer outlives the program
  (#114).
- `Pty.close` no longer sends `SIGTERM` behind the hangup's `SIGHUP`: the
  program has about 100 ms to act on the hangup before `SIGKILL`, so a
  shell's trap for `SIGHUP` runs.
- `Pty.spawn` searches for `prog` in the `PATH` of `env`, the program's
  environment, rather than in the calling process's (#114).

#### Fixed

- The keeper passes the program's status to the handle as data instead of
  dying by the program's signal, so macOS no longer files a crash report for
  the calling program when a spawned program crashes.
- The handle no longer waits for the program's process id after reaping the
  keeper, which in a subreaper could reap an unrelated child that had been
  given that id.
- `Pty.close` hangs up the program when another process holds the master
  too: the program receives `SIGHUP` rather than only the `SIGKILL` 100 ms
  later, and on the BSDs and macOS `close` no longer waits for that process
  to read the program's output.
- `Matrix_cli_unix.adapt_std_formatters` keeps the standard formatters
  adapted once the program spawns a domain. From then on, their line ends and
  indents were lost, and the rest of their output bypassed the styling and the
  live region.
- A hyperlink's URI is written percent-encoded, as the OSC 8 specification
  requires: every byte outside `0x21`..`0x7E`, the space included, becomes
  `%XX`, and a `%` is kept. Controls and invalid UTF-8 used to be dropped from
  the URI, which changed the link's target, and spaces and non-ASCII bytes went
  out raw.
- `Matrix_cli.adapt` no longer takes memory without bound for a long or
  unterminated OSC. It keeps a hyperlink's URI up to 2083 bytes, the limit of
  VTE and iTerm2, and a longer one ends the link.
- A line printed in many pieces while a live region shows costs time linear in
  its length. 100k one-byte prints took 5.5 s, and now take 0.03 s.

## [0.2.0] - 2026-11-04

0.2.0 changes most of the API of all three packages, and some programs that
still compile will behave differently. Before you upgrade from 0.1, read
[Migrating from 0.1](doc/migrating-from-0.1.md): it lists every renamed and
removed API, with code before and after. Note that Matrix, and so Mosaic, no
longer runs on Windows in this release.

Each package below starts with what is new. Then come the changes the compiler
won't catch, then the breaking changes and why we made them.

### Mosaic

The biggest change is that your model now owns all interactive state. In 0.1,
widgets kept their own: an input held its text, a tree its open nodes, a scroll
box its offset, and the view only passed starting values. Nothing kept the two
in step. A value the app forgot to pass back drifted away from the model, and a
value forced from the view undid what the user had just typed. Both of our own
editing examples had that bug. Now a widget shows the state you give it and
proposes changes through `on_change`, and your `update` decides what to keep:

```ocaml
(* in the view; model.name is an Editor.t *)
input ~state:model.name ~on_change:(fun e -> Name_edited e) ()

(* in update *)
| Name_edited e -> ({ model with name = e }, Cmd.none)
```

| Widget | The state it takes |
| --- | --- |
| `input`, `textarea` | `Editor.t` |
| `scroll_box` | `Scroll.t` |
| `tree` | `Tree.State.t` |
| `slider`, `scroll_bar` | their value |
| `select`, `tab_select`, `table` | the selected item, as a value of your own type |

Focus works the same way. Your model says which element is focused with
`?focused`. A click, or Tab, asks for focus through `?on_focus_request`. Tab
and Shift+Tab walk the elements in the order they appear, so you no longer keep
a list of them by hand.

Mosaic is also much faster on the thing we care about most, long transcripts.
With `keyed` lists and `memo`, a frame costs as much as what changed in it. In
our benchmark, a keystroke under a 20,000-block transcript takes about 0.3 ms,
the same as under 5,000 blocks.

#### Also new

- Widgets draw with a theme instead of fixed colors. `Mosaic.Theme` names
  roles such as `Plain`, `Muted`, `Accent` and `Border`, and `themed` restyles
  a subtree. The default theme uses the terminal's own palette, so widgets
  stay readable on light terminals, where 0.1's white text did not.
- You can select and copy text anywhere in the view: drag, double- or
  triple-click, extend with Shift and the arrow keys, then press Ctrl+C. Each
  element chooses what a selection takes from it with `?selectable`. By
  default it takes the text. With `` `Cells `` it takes every cell the element
  draws, borders included, the way a terminal selects. To copy as soon as the
  user selects, answer `Sub.on_selection` with `Cmd.copy_selection`.
- `modal` draws a dialog above the whole view and keeps focus, the mouse and
  Tab inside it. Escape sends its `on_close` message.
- Key, mouse and paste events travel from their target up through its parents,
  as in the DOM, and a handler can stop them or prevent their default action.
  `Sub.on_keys` binds shortcuts to messages.
- You can run a program under Eio with `Mosaic_eio.run`, under Lwt with
  `Mosaic_lwt.run`, or under any other event loop with `Mosaic.start` and
  `step`. In tests, `Mosaic_test.run` runs it on an emulated terminal.
- `Cmd.perform` and the new `Sub.worker` run on a helper domain, so slow work
  no longer holds up input. `Sub.after` fires once, which is what you want for
  debouncing.
- Syntax highlighting is pluggable. `Code.Highlighter` wraps a highlighter
  that runs synchronously, on a thread or asynchronously. After an edit, Mosaic
  asks an incremental highlighter only for the lines around the view. See
  [doc/highlighting.md](doc/highlighting.md).
- `Cmd.exec` hands the terminal to an editor or a pager and takes it back
  afterwards, and `Cmd.suspend` stops the program as Ctrl+Z does. Smaller
  commands copy to the clipboard, send a desktop notification, ring the bell
  and switch mouse tracking on or off.
- New elements: `button`, `rich_text`, `diff` for unified and side-by-side
  patches, `picture` for images, and `viewport_switch`, which picks a subtree
  by terminal width. Elements also take `?on_click` and `?on_size`.
- `Mosaic.Element.kind` turns a widget of your own into an element that the
  reconciler diffs like the built-in ones.

#### Changes the compiler won't catch

- Tab and Shift+Tab now move focus. If your program uses Tab for something
  else, call `Event.Key.prevent_default` in the handler that uses it, or bind
  it with `Sub.on_keys`.
- A click no longer focuses an element by itself. It sends the element's
  `?on_focus_request` message, and the element is focused once your view says
  `~focused:true`.
- A widget given no color draws with the theme, in the terminal's own colors.
  To get 0.1's look back, pass the widget its color arguments, or restyle the
  roles once with `themed` at the root.
- Ctrl+C still quits, but now as a default action that a handler can prevent.
  While text is selected, it copies the selection instead.
- Key, mouse and paste listeners now see events that an element prevented, as
  DOM listeners do. Check `default_prevented` on the event to skip them.
  Before, a listener could miss a press and have no way to know.
- In inline mode, the last frame stays on screen when the program ends, so a
  summary or a choice stays in the scrollback. `Cmd.keep_frame false` erases it
  as 0.1 did.
- Frames are drawn only when something asks for one. A `canvas` that animates
  on its own needs `~live:true`; without it, it moves only when something else
  redraws.
- An exception raised in `Cmd.perform` now ends the program, and `Mosaic.run`
  raises it again once the terminal is restored. In 0.1 it killed only its
  thread, and the program carried on without the result it was waiting for.
- `Unix.fork` raises while background work is running, because OCaml forbids
  forking once several domains run. Use `Unix.create_process`.
- `init` runs before the terminal is taken, so an exception it raises leaves
  the terminal alone.
- Two subscriptions with the same `~id`, a border width that isn't a whole
  number of cells, and a tab width below 1 now raise `Invalid_argument`.

#### Breaking changes

The compiler will point you to each of these. The [migration
guide](doc/migrating-from-0.1.md#mosaic) has the full lists and code before
and after.

- Stateful widgets take their state as an argument, as shown in the table
  above, and report changes through `~on_change`. `select`, `tab_select` and
  `table` take items that carry your own values instead of reporting indices,
  so reordering or filtering the items keeps the selection. See [Widget
  state](doc/migrating-from-0.1.md#widget-state).
- Callbacks that report a widget's own change (`on_change`, `on_activate`,
  `on_submit` and `on_scroll`) return `'msg` instead of `'msg option`. The
  widget has already decided there is a change, so there is nothing left to
  filter. Raw event callbacks still return `'msg option`.
- Focus is declared with `?focused` and `?on_focus_request`, which replace
  `?id`, `?ref`, `?autofocus`, `?focusable` and `Cmd.focus`. Focus kept by the
  widgets drifted away from the app, and a stored `ref` could outlive its
  element. Read sizes with `?on_size`, and mount a renderable of your own with
  `embed`. See [Focus](doc/migrating-from-0.1.md#focus).
- `Event.Key.data` returns a `Matrix.Input.Keystroke.t`. In 0.1 one key had a
  different value in each terminal encoding: Shift+a was `A` in one and `a`
  with Shift in another. Now it is `Char 'A'` everywhere, and Ctrl+C is
  `Char 'c'` with `ctrl` set.
- A mouse release reports `dragged` instead of `is_dragging`. It is true only
  when the pointer left the pressed cell, because some terminals report motion
  inside a cell and that turned clicks into drags. A press reports its
  `clicks`, for double and triple clicks.
- `Sub.every` and `Sub.on_tick` take an `~id`, and `Sub.every` passes the
  number of periods that elapsed. In 0.1 a timer that missed periods replayed
  each of them: a 0.1 s timer sent 100 messages after a 10 s stall.
- `Sub.on_key_all`, `on_mouse_all` and `on_paste_all` are gone. Pass
  `~phase:Capture` to `Sub.on_key`, `on_mouse` or `on_paste` instead. Events
  now follow the DOM's capture-then-bubble order, where 0.1 had a different
  rule for each kind of event.
- `Mosaic.run` takes a `Matrix.config` as `?config`, where it took a Matrix
  app as `?matrix`, because Mosaic now runs Matrix's loop itself. Eio programs
  call `Mosaic_eio.run`, which replaces the forty lines of glue each of them
  had to copy. See [Running a
  program](doc/migrating-from-0.1.md#running-a-program).
- `?selectable` takes `` `Off ``, `` `Text `` or `` `Cells `` instead of a
  bool, so an element can choose between selecting its text and selecting
  every cell it draws. `false` becomes `` `Off ``, and `true` can usually go,
  since `` `Text `` is the default. `Text.set_selectable` is gone; call
  `Renderable.set_selectable` on the node.
- The per-widget selection colors and `code ?on_selection` are gone.
  Selections draw with the theme's `Highlight` role, and `Sub.on_selection`
  hears the selection, which now spans every widget.
- The tree-sitter bindings moved to their own package,
  [ocaml-tree-sitter](https://github.com/invariant-hq/ocaml-tree-sitter), and
  `code ?spans` and `textarea ?spans` became `?syntax`. Every Mosaic install
  used to compile 36 MB of generated C that Mosaic didn't use. In our
  measurement, a fresh install went from 90 seconds to 60.
- `Syntax_theme` is now `Syntax_style`. `Markdown.style` is a
  `Markdown.Style.t` value built with `Markdown.Style.make`, where it was a
  function. A style function built in the view made every code block highlight
  again on every refresh.
- Durations are in seconds everywhere. A `canvas` draw function takes `~dt`
  where it took `~delta` in milliseconds, and `Spinner.frame_set` takes
  `period` where it took `interval`. We renamed the labels so that code written
  for milliseconds fails to compile instead of running 1,000 times too slowly.
- On a canvas, `?respect_alpha` gave way to `?blend` on each write, and
  `Canvas.set_cell ~glyph` to `set_uchar ~uchar`.

If you build on `mosaic.ui` directly:

- `Mosaic_ui.Vnode` is gone; build views with `Mosaic`'s elements. There were
  two vocabularies for one tree, and every label was spelled twice. `Mosaic.t`
  is abstract.
- `Mosaic_ui.Edit_buffer` became `Mosaic_ui.Editor`, an immutable value, so
  replayed models no longer share editing state.
- A widget's `create` takes its `Props.t` last instead of repeating every
  label: `Box.create ~parent (Box.Props.make ~border:true ())`.
- `Reconciler.create` takes `~deliver`, `Reconciler.render` takes
  `~viewport_width`, and `Renderer.dispatch_key` takes the focus path and an
  `Event.Key.t`. See [mosaic.ui](doc/migrating-from-0.1.md#mosaicui).

#### Fixed

- A text node that had shrunk didn't grow back when it got its room back.
- Flex children next to each other could paint over each other.
- `scroll_box ~scroll_x:true` didn't scroll horizontally.
- Messages sent from `Cmd.perform` could be lost.
- An empty, focused `input` showed no cursor.

### Matrix

Matrix has a new runtime. In 0.1 you handed `Matrix.run` a set of callbacks,
and it called them from inside its own loop. Now you run the loop.
`Matrix.run` holds the terminal for the length of a function and gives it back
however that function ends. Inside it, you ask `Matrix.next` for the next step
and draw with `Matrix.draw`:

```ocaml
let () =
  Matrix.run @@ fun app ->
  let rec loop count =
    match Matrix.next app with
    | Stopped | Event (Key { key = Escape; _ }) -> ()
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

Because the loop is yours, you can also hand it to another event loop.
`Matrix.step` runs one turn without waiting for input, and `Matrix.fds` and
`Matrix.deadline` tell Eio, Lwt or your own loop what to wait for.
`Matrix.post` sends a message from any thread or domain.

Frames now come only when you ask for one with `Matrix.request_redraw`, so an
idle program doesn't wake up at all. 0.1 drew 30 frames a second whether
anything changed or not. A key's frame is drawn in the same turn that read the
key: in our benchmark, the median time from a key press to the screen is under
0.4 ms.

#### Also new

- `matrix.test` runs a program's own loop on an emulated terminal, with a
  clock that only the test moves. Send input, resize and advance time, then
  read the screen with `Matrix_test.Screen`.
- Inline mode no longer erases the shell's output when the terminal is
  resized, when it runs under tmux, or when another process writes. In our
  tests on five terminal emulators, it lost no lines.
- A key decodes to the same value whichever encoding the terminal uses. Pastes
  arrive in pieces with no size limit, and the terminal's answers to queries no
  longer show up as key presses.
- `Matrix_grid.draw_picture` shows RGBA images, sent as kitty graphics, sixels
  or half blocks, whichever the terminal supports. `Matrix_grid.Lines` joins
  box-drawing lines where they meet.
- `matrix.cli` and `matrix.cli.unix` are for command-line programs that don't
  take over the screen: progress shown below the program's output, task trees
  with progress bars, prompts and tables. They follow `--color`, `NO_COLOR`,
  pipes and CI.
- `Matrix.query_palette` reads the user's palette, so you can pick colors that
  fit their theme.
- `Matrix.run` accepts the descriptors and environment of any terminal, and
  `Matrix.on_device` runs one program per remote session, for example one per
  SSH connection.
- `Matrix.release` lends the terminal to an editor or a shell and takes it
  back, and `Matrix.stop_job` suspends the program as Ctrl+Z does. The
  `Terminal` module adds clipboard, notification and bell commands, and
  `Matrix.set_mouse` switches mouse tracking while the program runs.
- We rewrote `matrix.charts` around figures. Marks such as `line`, `area`,
  `bar_y` and `spark` share named scales, and axes and legends follow from
  those scales. A figure's layout also answers zoom, pan and hit tests.
- Set `MATRIX_DEBUG=overlay` to see frame timings, or `MATRIX_DEBUG=frames=DIR`
  to save every frame, in any Matrix or Mosaic program, without rebuilding it.
- `matrix.text` follows Unicode 18.0 and lays out wrapped text, and
  `Ansi.sanitize` makes untrusted text safe to print. `Vte.feed` returns the
  replies the emulated terminal sends back.

#### Changes the compiler won't catch

- Matrix no longer runs on Windows. `Matrix.run`, and so `Mosaic.run`, raises
  `Failure` there: the new runtime waits on POSIX descriptors and has no
  Windows path yet. `matrix.pty` is POSIX-only too, because its ConPTY backend
  was unsound.
- Frames are drawn only on request. Call `Matrix.request_redraw` whenever your
  view changes, and again from each `Frame` while something animates. A
  program that relied on 0.1's 30 frames a second will stop updating its
  screen.
- Ctrl+C is an ordinary key, and Matrix no longer quits on it. To quit, test
  `Input.Keystroke.is_interrupt` and return from the run. Mosaic programs still
  quit on Ctrl+C by default.
- SIGINT, SIGTERM, SIGHUP and SIGQUIT restore the terminal and then end the
  process with the signal's status, so `run` doesn't return. 0.1 exited with a
  wrong status and replaced handlers the program had installed. Now a handler
  you install first wins.
- The cursor starts hidden in both modes. Show it with
  `Matrix.set_cursor app ~visible:true`.
- Output follows the user's color settings. `NO_COLOR` drops colors, a
  16-color terminal gets 16 colors and a pipe gets no styling at all. Pass
  ``~color:`Always`` to `Matrix.config` to keep colors whatever the user set.
- In inline mode, the last frame stays on screen at exit.
  `Matrix.keep_frame app false` erases it.
- Pictographs such as ❤ and ⚠ take one cell unless a VS16 follows them, as
  terminals draw them. Matrix counted them as two, which shifted the rest of
  the row.
- Drawing a space now erases the text under it, `blit_region` copies colors as
  they are, and a transparent `fill_rect` changes nothing. To tint cells, draw
  a translucent `fill_rect`.
- Matrix installs no handler for uncaught exceptions. The run restores the
  terminal and raises the exception again.

`Pty.spawn`, `Screen.resize`, `Terminal.reset_state` and `Vte.cursor_pos` also
behave differently in smaller ways; the [guide lists
them](doc/migrating-from-0.1.md#matrix-behaviour-changes).

#### Breaking changes

The [migration guide](doc/migrating-from-0.1.md#matrix) has the full lists.

- The callback runtime is gone. `Matrix.create`, `attach`, the `on_*`
  callbacks, `close`, `stop`, `suspend` and `resume` give way to the loop shown
  above, `Matrix.Stop` and `Matrix.release`. The old runtime ran application
  code from inside Matrix's loop, and its call order was promised only by a
  comment. See [the runtime](doc/migrating-from-0.1.md#matrix-runtime).
- The options of `create` moved into `Matrix.config`, given as `run ~config`.
- Input events are `Matrix.event`. `Input.Key` is now `Input.Keystroke`,
  `Input.Mouse` is `Input.Mouse_report`, and `Input.Parser` is
  `Input.Decoder`. A paste arrives as `Paste_start`, `Paste_data` and
  `Paste_end`: 0.1 cut a paste off after 5 seconds of silence and typed the
  rest as keys. `Input.Keymap` moved to Mosaic as `Mosaic.Shortcut`.
- Library modules have a prefix: `Grid` is `Matrix_grid`, `Screen` is
  `Matrix_screen`, and so on. The short names clashed with any other library,
  or file of your own, that had a module with the same name. Inside `Matrix`,
  the short names still work.
- The `matrix-eio` package is gone. Any event loop can now wait on the run's
  descriptors, and Mosaic programs use `Mosaic_eio.run`.
- `matrix.glyph` is gone. Measure and segment text with `matrix.text`.
- `Ansi.Color.t` is abstract, so that a palette color stays a palette color all
  the way to the terminal. Build colors with `Color.of_rgb`, `Color.indexed` or
  named values such as `Color.red`.
- `Ansi.render` takes spans built with `Ansi.span`. It now sanitizes the text,
  so a string from elsewhere can't move the cursor or restyle what follows.
- `Terminal.set_raw`, `restore` and `size` moved to `Tty`, along with
  everything else that calls the operating system, so `matrix.terminal` no
  longer links `unix`.
- `Matrix.Image` is `Matrix_image`, and `Image.render grid img` is
  `Matrix_image.draw img grid`.
- `Matrix.static_write` takes a grid where it took a string and a row count
  that you had to get right yourself.
- The debug overlay and frame dumps are no longer options of the run. Use
  `MATRIX_DEBUG`, or `Matrix.Debug_overlay` and `Matrix.dump_frame`.
- `Matrix_grid.create` takes no pool and no blending flag, and each drawing
  call says whether it blends. The screen's cursor setters became one
  `Matrix_screen.set_cursor`, and `Screen.post_process` is gone.
- `Vte.feed` returns the terminal's replies, which used to pile up in a queue
  that nothing drained. `Vte.create` takes a `?palette` instead of default
  colors.
- `matrix.charts` has the new API described above. A sparkline is a
  `float list` drawn with `spark`.

#### Fixed

- Ctrl+V and Ctrl+O didn't reach the application.
- The runtime used a full CPU core once its input closed.
- A failed setup left the terminal in raw mode.
- A mouse report on the first row of the inline region came in as row -1.
- A combining mark after an ASCII letter was split from the letter.

### Toffee

Toffee's layouts match Chromium's more often. We ported fixes from Taffy and
checked others against Chromium, for percentages, aspect ratios, absolute
positioning, baselines and scroll extents. A `row-reverse` scroll container
can now scroll, for one.

Relayout is also much cheaper, which matters for Mosaic's long transcripts.
Each node caches up to 24 measurements, where 0.1 had nine fixed slots that
kept evicting each other. With nine, appending a line to a 20,000-line log
measured text 500,000 times; with 24, it measures 20,000 times. Rounding skips the subtrees that didn't
move, laying out an unchanged tree returns at once, and reading a layout
allocates nothing.

#### Changes the compiler won't catch

- A flex or grid root without a width fills the available width, less its
  margins, as a block root already did and as browsers do. Before, its
  children had no width to resolve percentages against. To size it to its
  content as before, give it a width, or lay it out in
  `Available_space.max_content`.
- A root with margins sits at its margins instead of at (0, 0).
- Locations are rounded from absolute positions, as in Taffy and browsers, so
  boxes at fractional offsets tile without gaps or overlaps. A box under a
  parent at a fractional position may move by one cell.
- A measure function receives the size of the content box, clamped by the
  leaf's minimum and maximum sizes, where it received the border box. A padded
  text was measured at one height and laid out at another, so the line after
  it drew over it.
- Percentages inside a flex item whose size comes from its content resolve as
  if that size were unknown, as browsers do.

#### Breaking changes

- Geometry is unboxed. Measure functions return a `Sizef.t`, and the fields of
  `Layout.t` are `Sizef.t`, `Pointf.t` and `Rectf.t`, records of plain floats.
  Reading a layout used to allocate a box for every float. The float
  operations moved into those modules too, and `Size.max_concrete` is
  `Sizef.max`.
- `Layout.content_size` is now `Layout.scrollable_overflow`, which can reach
  before the origin, as CSS defines it. For scroll extents, use
  `Layout.scroll_width` and `Layout.scroll_height`.
- Library modules have a prefix (`Toffee_style`, `Toffee_tree` and so on), for
  the same reason as Matrix's. `Toffee.Style` and `Toffee.Geometry` still work.
- `Size.equal_option` and `Available_space.maybe_set` are now
  `Size.equal_with` and `Available_space.set_or_self`.
- If you write your own layout algorithm or tree, the cache takes the node's
  `Layout_input.t` and its style, `Layout_input.t` has a new field, outputs
  carry a `Scrollable_overflow.t`, and `ROUND_TREE` adds `is_rounded`. See
  [Toffee](doc/migrating-from-0.1.md#toffee) in the guide.

#### Fixed

- A grid's auto rows collapsed when they held scroll containers.

## [0.1.0] - 2026-02-26

### Mosaic

Terminal UI framework for OCaml built on Matrix and Toffee. Implements The Elm Architecture (TEA) for declarative, composable terminal applications.

- **The Elm Architecture** – Pure functional Model-View-Update pattern with `init`, `update`, `view`, and `subscriptions`. Commands (`Cmd`) handle side effects; subscriptions (`Sub`) handle external events
- **Flexbox and Grid layout** – Powered by Toffee, define complex layouts with familiar CSS properties like `flex_direction`, `align_items`, `justify_content`, and CSS Grid support
- **Rich widget library** – `box`, `text`, `input`, `textarea`, `select`, `tab_select`, `slider`, `table`, `spinner`, `scroll_box`, `canvas`, `code`, `markdown`, `tree`, `progress_bar`, and `line_number`
- **Rich text** – Styled fragments, word/character wrapping, text selection, and full Unicode support
- **Syntax highlighting** – Tree-sitter-based code highlighting with the `code` widget
- **Event system** – Mouse, keyboard, paste, focus, and resize events with `Sub.on_key`, `Sub.on_mouse`, `Sub.on_paste`, `Sub.on_resize`, `Sub.on_focus`, and `Sub.on_blur`
- **Canvas drawing** – Procedural drawing API with shapes, lines, and braille patterns for charts and visualizations
- **Markdown rendering** – CommonMark rendering with the `markdown` widget
- **Dirty tracking and viewport culling** – Only re-layouts dirty subtrees; scroll containers cull off-screen children

### Matrix

Terminal toolkit for OCaml providing rendering, input, and terminal management.

- **Immediate-mode runtime** – `Matrix.run` with `on_frame`, `on_render`, `on_input`, `on_resize` callbacks, configurable FPS capping, and safe teardown that restores terminal state even on exceptions
- **Two display modes** – Full-screen alternate buffer (`Alt`) or inline on primary screen (`Primary`) with dynamic height and full scrollback
- **Declarative Image API** – Notty-inspired compositional DSL with `hcat`, `vcat`, `overlay`, padding, cropping, and hit regions for mouse interaction
- **Full Unicode support** – Grapheme cluster handling, emoji, wide characters, and configurable width calculation (wcwidth, Unicode tables)
- **Modern terminal protocols** – Kitty keyboard (with auto-detection), SGR/X10/URXVT mouse tracking, bracketed paste, focus reporting—all negotiated automatically
- **Native alpha blending** – RGBA colors with proper alpha compositing for translucent overlays
- **Double-buffered diffing** – Screen module diffs cell changes between frames to emit minimal ANSI output
- **PTY & VTE** – Spawn processes in pseudo-terminals and embed terminal output in your UI; cross-platform with Windows ConPTY support
- **Automatic capability detection** – Two-stage probing (environment heuristics + active queries) detects RGB, Kitty keyboard, sixel, hyperlinks, and Unicode width support without manual configuration
- **Hit testing** – O(1) spatial indexing maps mouse coordinates to UI element IDs for clickable widgets
- **Built-in devtools** – Debug overlay for frame timing/FPS, frame dumps to disk for diagnostics

### Toffee

CSS layout engine for OCaml, ported from [Taffy](https://github.com/DioxusLabs/taffy) (Rust).

- **CSS Grid Level 1** – Full implementation including `grid-template-columns/rows`, `grid-auto-flow`, named lines/areas, `repeat()`, `minmax()`, `fr` units, and auto-placement
- **Flexbox** – Complete algorithm with `flex-direction`, `flex-wrap`, `flex-grow/shrink/basis`, `align-items/self/content`, `justify-content`, `gap`, and `order`
- **Block layout** – Traditional CSS block formatting context with proper margin collapsing
- **Pure OCaml, zero dependencies** – No C stubs, no runtime dependencies; works wherever OCaml 5 runs
- **Arena storage and layout caching** – Nodes stored in a flat arena; layout results cached and invalidated via `mark_dirty`
- **Composable architecture** – Use the high-level `Toffee` tree API or the lower-level `toffee.tree` and `toffee.compute` libraries to plug layout into your own node representation
- **Custom measure functions** – Integrate text shaping, images, or any content with intrinsic sizing via `compute_layout_with_measure`
