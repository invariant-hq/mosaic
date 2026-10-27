open Mosaic_ui

(* ── Ordering assertions ── *)

(* [satisfies] over [int] with the claim built from the bound, so a call
   site stays one line and the failure still shows both numbers. Thread
   [?pos] when the failing line matters more than the test's name. *)

let gt ?pos ?msg ~than n =
  Windtrap.satisfies ?pos ?msg
    ~claim:(Printf.sprintf "greater than %d" than)
    Windtrap.int
    (fun v -> v > than)
    n

let ge ?pos ?msg ~than n =
  Windtrap.satisfies ?pos ?msg
    ~claim:(Printf.sprintf "at least %d" than)
    Windtrap.int
    (fun v -> v >= than)
    n

let lt ?pos ?msg ~than n =
  Windtrap.satisfies ?pos ?msg
    ~claim:(Printf.sprintf "less than %d" than)
    Windtrap.int
    (fun v -> v < than)
    n

let le ?pos ?msg ~than n =
  Windtrap.satisfies ?pos ?msg
    ~claim:(Printf.sprintf "at most %d" than)
    Windtrap.int
    (fun v -> v <= than)
    n

(* ── Renderer Context ── *)

(* A renderer for a test, with the frames its nodes asked for
   ([schedule_count]). Everything goes through the renderer's public
   interface. *)
type ctx = { renderer : Renderer.t; schedule_count : int ref }

(* The renderer of each root, so that the helpers below find a node's
   renderer from the node. A [uid] is unique within one renderer, so roots
   are told apart physically. *)
let renderers : (int, Renderable.t * Renderer.t) Hashtbl.t = Hashtbl.create 16

let rec root_of node =
  match Renderable.parent node with Some p -> root_of p | None -> node

let renderer_of node =
  let root = root_of node in
  match
    List.find_opt
      (fun (r, _) -> r == root)
      (Hashtbl.find_all renderers (Renderable.uid root))
  with
  | Some (_, r) -> r
  | None -> Windtrap.fail "the node is in no test renderer"

(* [register r] lets the helpers below find [r] from its nodes, for a test
   that makes its renderer itself. *)
let register r =
  let root = Renderer.root r in
  Hashtbl.add renderers (Renderable.uid root) (root, r)

let make_ctx ?(width_method = `Unicode) () =
  let schedule_count = ref 0 in
  let renderer =
    Renderer.create ~width_method
      ~request_render:(fun () -> incr schedule_count)
      ~executor:(fun work -> ignore (Thread.create work () : Thread.t))
      ()
  in
  register renderer;
  { renderer; schedule_count }

(* The size of the frames the helpers draw when the test names none. *)
let frame_width = 80
let frame_height = 24

(* The frames that settle [r]: a frame can ask for another, as a widget
   sized by its first layout does. *)
let settle_frames r =
  let rec go n =
    Renderer.render_frame r ~width:frame_width ~height:frame_height ~dt:0.;
    ignore (Renderer.render r : string);
    if n > 1 && Renderer.needs_render r then go (n - 1)
  in
  go 8

(* Draws the frames that leave the renderer clean, and is the number of
   frames asked for so far: a test reads it before an operation and compares
   after, so that an operation that asks for a frame counts once. *)
let clean t =
  settle_frames t.renderer;
  !(t.schedule_count)

(* ── Root and Layout ── *)

let make_root ?style t =
  let root = Renderer.root t.renderer in
  Option.iter (Renderable.set_style root) style;
  root

(* Places [node] at column [x] and row [y], [width] cells wide and
   [height] high, as its style says, and lays it out with a frame. *)
let place node ~x ~y ~width ~height =
  let open Toffee.Style in
  let lpa v = Length_percentage_auto.length (Float.of_int v) in
  Renderable.set_style node
    (Renderable.style node
    |> set_position Position.Absolute
    |> set_inset
         {
           left = lpa x;
           top = lpa y;
           right = Length_percentage_auto.auto;
           bottom = Length_percentage_auto.auto;
         }
    |> set_size { width = Dimension.length (Float.of_int width); height }
    |> set_flex_shrink 0.);
  let r = renderer_of node in
  Renderer.render_frame r ~width:frame_width ~height:frame_height ~dt:0.;
  ignore (Renderer.render r : string)

(* Places [node] at column [x] and row [y] of the frame, [width] by [height]
   cells. *)
let layout_node node ~x ~y ~width ~height =
  place node ~x ~y ~width
    ~height:(Toffee.Style.Dimension.length (Float.of_int height))

(* Places [node] as [layout_node] does, at [width] columns, with the height
   its measure gives. *)
let layout_width node ~x ~y ~width =
  place node ~x ~y ~width ~height:Toffee.Style.Dimension.auto

(* [measured_again node f] is whether the frames after [f ()] measure [node]
   again, as they do when [f] changes what lays it out: the layout tree keeps
   a node's measurement until the node is marked to be laid out again. *)
let measured_again node f =
  match Renderable.measure node with
  | None -> Windtrap.fail "the node has no measure"
  | Some measure ->
      let calls = ref 0 in
      Renderable.set_measure node
        (Some
           (fun ~known_dimensions ~available_space ~style ->
             incr calls;
             measure ~known_dimensions ~available_space ~style));
      let r = renderer_of node in
      settle_frames r;
      calls := 0;
      f ();
      settle_frames r;
      !calls > 0

(* ── Drawing ── *)

(* [render node grid ~dt] draws a frame of [node]'s renderer the size of
   [grid] and copies it into [grid]. *)
let render node grid ~dt =
  let r = renderer_of node in
  Renderer.render_frame r ~width:(Matrix_grid.width grid)
    ~height:(Matrix_grid.height grid) ~dt;
  Matrix_grid.blit ~src:(Matrix_screen.next_grid (Renderer.screen r)) ~dst:grid;
  (* Presenting the frame makes its hit geometry the one events find. *)
  ignore (Renderer.render r : string)

(* ── Box Helpers ── *)

let lp = Toffee.Style.Length_percentage.equal
let one = Toffee.Style.Length_percentage.length 1.
let zero_lp = Toffee.Style.Length_percentage.zero
let border_of node = Toffee.Style.border (Renderable.style node)

(* ── Grid ── *)

let make_grid ?(width = 80) ?(height = 24) ?(width_method = `Unicode) () =
  Matrix_grid.create ~width ~height ~width_method ()

(* ── Input events ── *)

(* Modifiers with the ones held named, for [make_key] and [char_key]. *)
let mods ?(ctrl = false) ?(alt = false) ?(shift = false) ?(super = false)
    ?(hyper = false) ?(meta = false) ?(caps_lock = false) ?(num_lock = false) ()
    : Matrix_input.Keystroke.mods =
  { shift; alt; ctrl; super; hyper; meta; caps_lock; num_lock }

let make_key ?ctrl ?alt ?shift ?super ?hyper ?meta ?caps_lock ?num_lock ?repeat
    ?text ?base ?keypad key =
  Matrix_input.Keystroke.make
    ~mods:(mods ?ctrl ?alt ?shift ?super ?hyper ?meta ?caps_lock ?num_lock ())
    ?repeat ?text ?base ?keypad key

let char_key ?ctrl ?alt ?shift ?super ?hyper ?meta ?repeat ?text ?base c =
  make_key ?ctrl ?alt ?shift ?super ?hyper ?meta ?repeat ?text ?base
    (Char (Uchar.of_char c))

(* Mouse reports as the terminal sends them, for tests that dispatch through
   the renderer. *)
let mouse_report ?(shift = false) ?(alt = false) ?(ctrl = false) ~x ~y kind :
    Matrix_input.Mouse_report.t =
  { kind; x; y; shift; alt; ctrl }

(* Dispatch through the renderer as a host does with no listeners of its
   own: the path is taken, then the event delivered along it. Each returns
   the event, whose flags tell what the tree did with it. *)
let dispatch_key renderer key =
  let event = Event.Key.of_input key in
  Renderer.dispatch_key renderer (Renderer.focus_path renderer) event;
  event

let dispatch_paste renderer text =
  let event = Event.Paste.of_text text in
  Renderer.dispatch_paste renderer (Renderer.focus_path renderer) event;
  event

let dispatch_mouse renderer report =
  let path, event = Renderer.mouse_path renderer report in
  Renderer.dispatch_mouse renderer path event;
  event

(* Events given to a node through its renderer, as a terminal's input
   reaches it: the renderer draws a frame first if one is due, so that its
   hit geometry is current. *)
let settle r =
  if Renderer.needs_render r then (
    Renderer.render_frame r ~width:frame_width ~height:frame_height ~dt:0.;
    ignore (Renderer.render r : string))

let report_button : Event.Mouse.button -> Matrix_input.Mouse_report.button =
  function
  | Left -> Left
  | Middle -> Middle
  | Right -> Right
  | Button n -> Extra n

(* [emit_mouse node event] delivers [event] along the path the renderer takes
   for the pointer at [event]'s cell, which must be over [node]: the report
   of [event]'s kind routes it, and [event] itself is delivered. *)
let emit_mouse node (event : Event.mouse) =
  let r = renderer_of node in
  settle r;
  let x = Event.Mouse.x event and y = Event.Mouse.y event in
  let kind : Matrix_input.Mouse_report.kind =
    match Event.Mouse.kind event with
    | Down { button } -> Press (report_button button)
    | Up { button; _ } -> Release (Some (report_button button))
    | Drag { button; _ } -> Motion (Some (report_button button))
    | Scroll { direction = Scroll_up; _ } -> Scroll `Up
    | Scroll { direction = Scroll_down; _ } -> Scroll `Down
    | Scroll { direction = Scroll_left; _ } -> Scroll `Left
    | Scroll { direction = Scroll_right; _ } -> Scroll `Right
    | Move | Drag_end _ | Drop _ | Over _ | Out -> Motion None
  in
  let modifiers = Event.Mouse.modifiers event in
  let path, _ =
    Renderer.mouse_path r
      {
        kind;
        x;
        y;
        shift = modifiers.shift;
        alt = modifiers.alt;
        ctrl = modifiers.ctrl;
      }
  in
  Renderer.dispatch_mouse r path event

(* [focus_node node] focuses [node] through its renderer, making it focusable
   first if it is not. *)
let focus_node node =
  let r = renderer_of node in
  if not (Renderable.focusable node) then Renderable.set_focusable node true;
  ignore (Renderer.focus r node : bool);
  r

let blur_node node = Renderer.blur (renderer_of node)

(* [emit_key node event] focuses [node] and delivers [event] to it: its
   handlers, its behavior, then its ancestors, as a key typed into it. *)
let emit_key node event =
  let r = focus_node node in
  Renderer.dispatch_key r (Renderer.focus_path r) event

let emit_paste node event =
  let r = focus_node node in
  Renderer.dispatch_paste r (Renderer.focus_path r) event

(* UI-layer mouse events (Event.Mouse). Widget tests that work at the input
   layer instead — Matrix_input.Mouse_report — build their own; the two
   layers are different types, not duplicates. *)
let no_mod = Event.Mouse.no_modifier

let mouse_down ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod (Down { button = Left })

let mouse_up ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Up { button = Left; is_dragging = false })

let mouse_drag ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Drag { button = Left; is_dragging = true })

let mouse_move ~x ~y = Event.Mouse.make ~x ~y ~modifiers:no_mod Move
let mouse_out ~x ~y = Event.Mouse.make ~x ~y ~modifiers:no_mod Out

let mouse_scroll_down ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Scroll { direction = Scroll_down; delta = 1 })

let mouse_scroll_up ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Scroll { direction = Scroll_up; delta = 1 })

(* ── A widget's two doors ── *)

(* A widget takes its labels through its element and through its
   [Props.make], whose props its imperative [create] takes: both build its
   properties with its [Props.make]. [reaches]
   checks that one label, given through either door, reaches the live widget:
   the widget made by [create] with the label differs from the one made
   without it, and the element with the label, mounted or declared after the
   element without it (the update path, [apply_props]), is observed the same.
   Each door's widget sits alone in a renderer's root and is observed after
   [act], by default by the frame it draws. A label the widget stores and
   never draws or acts on is checked with [~shows:false]: both doors then only
   agree. *)

let doors_width = 30
let doors_height = 8

let frame renderer _node =
  Renderer.render_frame renderer ~width:doors_width ~height:doors_height ~dt:0.;
  Matrix_grid.to_ansi (Matrix_screen.next_grid (Renderer.screen renderer))

let no_act _renderer _node = ()

let only_child renderer =
  match Renderable.children (Renderer.root renderer) with
  | [ node ] -> node
  | _ -> Windtrap.fail "expected the widget alone in the root"

let through_element ?before ~act ~observe element =
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer) ~deliver:ignore
  in
  let declare view =
    Mosaic.Reconciler.render reconciler ~viewport_width:doors_width view;
    Renderer.render_frame renderer ~width:doors_width ~height:doors_height
      ~dt:0.
  in
  Option.iter declare before;
  declare element;
  let node = only_child renderer in
  act renderer node;
  observe renderer node

let through_create ~act ~observe create =
  let renderer = Renderer.create () in
  let node = create (Renderer.root renderer) in
  Renderer.render_frame renderer ~width:doors_width ~height:doors_height ~dt:0.;
  act renderer node;
  observe renderer node

let reaches ?(act = no_act) ?(observe = frame) ?(shows = true) label ~base
    ~element ~create_base ~create =
  let msg what = Printf.sprintf "%s: %s" label what in
  let without = through_create ~act ~observe create_base in
  let with_label = through_create ~act ~observe create in
  Windtrap.equal
    ~msg:(msg "both doors without the label")
    Windtrap.string without
    (through_element ~act ~observe base);
  if shows then
    Windtrap.not_equal
      ~msg:(msg "create shows the label")
      Windtrap.string without with_label;
  Windtrap.equal
    ~msg:(msg "the element mounted with the label")
    Windtrap.string with_label
    (through_element ~act ~observe element);
  Windtrap.equal
    ~msg:(msg "the element updated with the label")
    Windtrap.string with_label
    (through_element ~before:base ~act ~observe element)

(* [select_text] drags the pointer over the widget's first five cells, as a
   user selecting text does. *)
let select_text renderer node =
  ignore (Renderer.render ~full:true renderer : string);
  let x = Renderable.x node and y = Renderable.y node in
  ignore (dispatch_mouse renderer (mouse_report ~x ~y (Press Left)));
  ignore
    (dispatch_mouse renderer (mouse_report ~x:(x + 5) ~y (Motion (Some Left))))

let focus renderer node =
  Renderable.set_focusable node true;
  ignore (Renderer.focus renderer node : bool)

(* [press_key key] focuses the widget and presses [key]. *)
let press_key key renderer node =
  focus renderer node;
  ignore (dispatch_key renderer key : Event.key)

(* ── Paint by roles ── *)

(* The palettes of two terminals as they report them, the pair and entries
   0 to 15: xterm's default, black on white, and xterm with its XTerm-color
   resources, light gray on black. *)
let xterm_entries =
  [|
    (0x00, 0x00, 0x00);
    (0xcd, 0x00, 0x00);
    (0x00, 0xcd, 0x00);
    (0xcd, 0xcd, 0x00);
    (0x00, 0x00, 0xee);
    (0xcd, 0x00, 0xcd);
    (0x00, 0xcd, 0xcd);
    (0xe5, 0xe5, 0xe5);
    (0x7f, 0x7f, 0x7f);
    (0xff, 0x00, 0x00);
    (0x00, 0xff, 0x00);
    (0xff, 0xff, 0x00);
    (0x5c, 0x5c, 0xff);
    (0xff, 0x00, 0xff);
    (0x00, 0xff, 0xff);
    (0xff, 0xff, 0xff);
  |]

let xterm_light : Ansi.Palette.t =
  {
    foreground = (0, 0, 0);
    background = (255, 255, 255);
    cursor = None;
    entries = Array.map Option.some xterm_entries;
  }

let xterm_dark : Ansi.Palette.t =
  {
    foreground = (0xe5, 0xe5, 0xe5);
    background = (0, 0, 0);
    cursor = None;
    entries = Array.map Option.some xterm_entries;
  }

(* The RGB a cell's color shows on a terminal of palette [p]: the default
   color, and a transparent background, are the terminal's own, an entry
   the palette's, and one it does not know the color Ansi.Color assumes. *)
let shown (p : Ansi.Palette.t) ~default color =
  if Ansi.Color.alpha color = 0. then default
  else
    match Ansi.Color.intent color with
    | Default -> default
    | Indexed i -> (
        match Ansi.Palette.entry p i with
        | Some rgb -> rgb
        | None -> Ansi.Color.to_rgb color)
    | Rgb -> Ansi.Color.to_rgb color

let rgb (r, g, b) = Ansi.Color.of_rgb r g b

(* The cells of [grid] that hold text below the contrast of text, the lower
   of 4.5 and the pair's, on a terminal of palette [c], as "x,y". *)
let illegible (c : Ansi.Palette.t) grid =
  let pair = Ansi.Color.contrast (rgb c.foreground) (rgb c.background) in
  let target = Float.min 4.5 pair in
  let bad = ref [] in
  for y = Matrix_grid.height grid - 1 downto 0 do
    for x = Matrix_grid.width grid - 1 downto 0 do
      let i = Matrix_grid.idx grid ~x ~y in
      let text = Matrix_grid.get_text grid i in
      if text <> "" && String.trim text <> "" then begin
        let style = Matrix_grid.get_style grid i in
        let fg = shown c ~default:c.foreground (Matrix_grid.get_fg grid i) in
        let bg = shown c ~default:c.background (Matrix_grid.get_bg grid i) in
        let fg, bg =
          if Ansi.Attr.mem Inverse style.Ansi.Style.attrs then (bg, fg)
          else (fg, bg)
        in
        if Ansi.Color.contrast (rgb fg) (rgb bg) < target then
          bad := Printf.sprintf "%d,%d" x y :: !bad
      end
    done
  done;
  !bad

(* [paint ?theme ?focused view] mounts [view] alone in a renderer whose root
   theme is [theme] ({!Theme.plain} by default), focuses its node when
   [focused] is [true], and draws a frame. *)
let paint ?(theme = Theme.plain) ?(focused = false) view =
  let renderer = Renderer.create () in
  Renderer.set_theme renderer theme;
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer) ~deliver:ignore
  in
  Mosaic.Reconciler.render reconciler ~viewport_width:doors_width view;
  Renderer.render_frame renderer ~width:doors_width ~height:doors_height ~dt:0.;
  if focused then begin
    focus renderer (only_child renderer);
    Renderer.render_frame renderer ~width:doors_width ~height:doors_height
      ~dt:0.
  end;
  renderer

let painted renderer = Matrix_screen.next_grid (Renderer.screen renderer)

(* The frame at each of the four stylings an output may receive. *)
let stylings : (string * Ansi.Styling.t) list =
  [
    ("truecolor", `Truecolor);
    ("16 colors", `Ansi16);
    ("attributes only", `Mono);
    ("no styling", `Plain);
  ]

let at styling renderer = Matrix_grid.to_ansi ~styling (painted renderer)

(* Whether some cell of [a] and the same cell of [b] have other attributes,
   such as reverse or bold. *)
let attributes_differ a b =
  let differ = ref false in
  for i = 0 to (Matrix_grid.width a * Matrix_grid.height a) - 1 do
    if Matrix_grid.get_attrs a i <> Matrix_grid.get_attrs b i then
      differ := true
  done;
  !differ

(* The focus rule: at every styling the focused frame differs from the
   unfocused one, at no styling by a glyph, so in the text it shows, and
   also by an attribute that changes no color. *)
let focus_shows ?theme view =
  let unfocused = paint ?theme view in
  let focused = paint ?theme ~focused:true view in
  List.iter
    (fun (name, styling) ->
      Windtrap.not_equal ~msg:("focus shows, " ^ name) Windtrap.string
        (at styling unfocused) (at styling focused))
    stylings;
  Windtrap.equal ~msg:"focus shows by an attribute" Windtrap.bool true
    (attributes_differ (painted unfocused) (painted focused))

(* The style and the text of the cell at [x], [y] of the frame [renderer]
   drew last. *)
let cell_style renderer ~x ~y =
  let grid = painted renderer in
  Matrix_grid.get_style grid (Matrix_grid.idx grid ~x ~y)

let cell_text renderer ~x ~y =
  let grid = painted renderer in
  Matrix_grid.get_text grid (Matrix_grid.idx grid ~x ~y)

let has attr (s : Ansi.Style.t) = Ansi.Attr.mem attr s.attrs

let fg_is color (s : Ansi.Style.t) =
  Option.equal Ansi.Color.equal (Some color) s.fg

let bg_is color (s : Ansi.Style.t) =
  Option.equal Ansi.Color.equal (Some color) s.bg

(* Whether [text] includes [sub]. *)
let includes text sub =
  let n = String.length text and m = String.length sub in
  let rec at i = i + m <= n && (String.sub text i m = sub || at (i + 1)) in
  at 0
