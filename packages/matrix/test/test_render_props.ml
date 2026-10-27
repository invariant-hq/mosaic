(** The renderer, checked against a terminal.

    The runtime turns each frame, a grid of cells and a cursor, into the bytes a
    terminal receives. These properties draw random frame sequences, present
    them through {!Matrix.run} over a device whose output feeds a {!Vte} of the
    terminal's size, and check after every frame that the terminal shows what
    the frame asked for: each cell's text, width, colors, attributes and
    hyperlink, as far as they show (a space's foreground does not without an
    attribute), and the cursor's visibility and, when it is visible, its
    position and its shape (from the last DECSCUSR sent). The VTE uses each of
    the three width methods in turn, and so do the runtime's capabilities, or
    they start with another until the terminal reports its own (DECRPM 2027)
    before some frame: from then on every frame must show.

    Frames mix ASCII, wide CJK, combining marks, emoji sequences, characters of
    no width and characters that join their neighbors; every SGR attribute;
    default, 256-color, truecolor and translucent colors; hyperlinks; background
    fills, clears and empty cells; cursor moves, shapes and visibility;
    terminals with synchronized output or [explicit_cursor_positioning]; resizes
    between frames, growing and shrinking; and releases of the terminal, during
    which a program writes rows. Each case runs at one styling, the terminal's
    24-bit, 256 or 16 colors, [NO_COLOR] or [--color=never], and the terminal
    must show each cell as that styling writes it: its colors reduced to the
    depth, none under [NO_COLOR], and neither attributes nor a hyperlink under
    [--color=never]. While released, the screen must be as {!Matrix.release}
    leaves it, the cursor shown.

    {b Alternate screen.} The frame is the whole screen, and scroll hints, true
    or not, never change what it shows. The VTE's replies are ignored.

    {b Primary screen.} The terminal starts with shell output above its cursor,
    perhaps a prompt on the cursor's row. The runtime draws the live region
    below a blank row of its own, its origin, from where the cursor rests,
    without asking where that is; the VTE's replies are the runtime's input all
    the same. The contract, from [`Primary] and {!Matrix.static_write}:
    - the live region is the [snd (Matrix.size app)] rows below the origin, at
      least [min_tui_height] of them and at least the rows the frame requires
      ([primary_required_rows]), as far as the rows below the screen's first
      have them; the frame is drawn at its height, and it shows the frame;
    - the cursor rests on the origin when it is hidden, and on the frame's cell
      when it shows, so the origin is found from the cursor;
    - the transcript is everything above the origin, the scrollback and then the
      rows above it: the shell's output, then each static row, in order, each
      exactly once, as {!Matrix.static_replace} and {!Matrix.static_clear} leave
      it, and the rows of programs run while it is released. Nothing else is
      there but rows the terminal itself pushed into its scrollback as it
      shrank;
    - when the run ends, the live region is erased, the cursor at the start of
      the origin, or, kept ({!Matrix.keep_frame}), the rows of the last frame
      that show anything stay right below the transcript, the cursor at the
      start of the row after them.

    The VTE departs from xterm in places (vte.ml, [place] and [join]). Two
    concern printing, and neither applies here: it erases the cells a wide unit
    skips when it wraps, but a grid holds only units that fit their row; and it
    joins a unit of no width that follows a control function to the cell before
    the cursor, but a grid holds no unit of no width.

    The counts run in about a second. [MATRIX_FUZZ_SCALE=N] multiplies them. *)

module Grid = Matrix_grid
module Color = Ansi.Color
open Windtrap

(* The scrollback's lines, oldest first. *)
let scrollback_lines vte =
  let n = Vte.scrollback_size vte in
  List.init n (fun i -> Vte.text vte (i - n))

let scale =
  match Sys.getenv_opt "MATRIX_FUZZ_SCALE" with
  | Some s -> (
      match int_of_string_opt s with Some n when n > 0 -> n | _ -> 1)
  | None -> 1

let count n = n * scale

(* Cases ------------------------------------------------------------------ *)

type op =
  | Text of { x : int; y : int; text : string; style : Ansi.Style.t }
  | Fill of { x : int; y : int; w : int; h : int; color : Color.t }
  | Clear of { x : int; y : int; w : int; h : int }
  | Empty of { x : int; y : int; style : Ansi.Style.t }
      (** An empty cell, which shows as a space in its colors. *)

type cursor = {
  pos : (int * int) option;
  visible : bool;
  style : Ansi.cursor_style;
  blinking : bool;
}

(* What the application does to the primary transcript. *)
type static =
  | Write of { rows : int; preserve : bool }  (** [Matrix.static_write] *)
  | Replace of { rows : int; preserve : bool }  (** [Matrix.static_replace] *)
  | Clear_static  (** [Matrix.static_clear] *)

(* The application releases the terminal, and a program run meanwhile writes
   [child] lines before it takes the terminal back. *)
type suspend = { child : int }

type frame = {
  size : (int * int) option;  (** A terminal resize before the frame. *)
  suspend : suspend option;  (** Before the frame, after the resize. *)
  statics : static list;  (** Before the frame, after the resize. *)
  required : int option;  (** [primary_required_rows]. *)
  scroll : Matrix.Screen.scroll_hint option;
      (** A scroll hint, which need not match the frames: the screen must be
          right whatever the hint. *)
  ops : op list;
  cursor : cursor;
}

type case = {
  mode : Matrix.mode;
  width_method : Matrix.Text.width_method;
  cols : int;
  rows : int;
  shell : int;  (** Rows of shell output above the cursor, primary only. *)
  prompt : bool;  (** The cursor ends a prompt rather than starting a row. *)
  min_live : int;
      (** [min_tui_height], or [0] when the case states no height, where the
          live region takes the rows down to the last. *)
  sync : bool;  (** The terminal has synchronized output. *)
  report : int option;
      (** Before this frame the terminal reports its width method (DECRPM 2027),
          which the runtime's capabilities had wrong until then. *)
  reposition : bool;  (** The terminal has [explicit_cursor_positioning]. *)
  keep : bool;  (** The run keeps its last frame, primary only. *)
  styling : Ansi.Styling.t;
      (** What the run writes at: the terminal's colors, [NO_COLOR] ([`Mono]) or
          [--color=never] ([`Plain]). *)
  frames : frame list;
}

(* Generators ------------------------------------------------------------- *)

let map2 f a b = Gen.map (fun (a, b) -> f a b) (Gen.pair a b)
let map3 f a b c = Gen.map (fun (a, b, c) -> f a b c) (Gen.triple a b c)

let fragments =
  [
    "a";
    "ab";
    "xyz";
    "Hello";
    " ";
    "  ";
    (* wide *)
    "\u{4E2D}";
    "\u{6587}\u{5B57}";
    "\u{FF21}";
    (* combining marks *)
    "e\u{0301}";
    "a\u{0308}\u{0301}";
    (* emoji *)
    "\u{1F600}";
    "\u{1F468}\u{200D}\u{1F4BB}";
    "\u{2764}\u{FE0F}";
    "\u{2764}";
    "\u{1F44D}\u{1F3FB}";
    "1\u{FE0F}\u{20E3}";
    "\u{1F1EB}\u{1F1F7}";
    (* units that join their neighbors when written alone *)
    "\u{1F1EB}";
    "\u{1F3FB}";
    "\u{0903}";
    "\u{0D4E}";
    "\u{1100}";
    "\u{1161}";
    "x\u{200D}";
    (* zero width alone *)
    "\u{200B}";
    "\u{0301}";
    "\u{FE0F}";
    (* a conjunct, Hangul *)
    "\u{0915}\u{094D}\u{0937}";
    "\u{1100}\u{1161}\u{11A8}";
    (* controls, which no cell holds *)
    "\t";
    "\n";
  ]

let text_gen =
  let open Gen in
  map (String.concat "") (list ~size:(int_range 1 4) (of_list fragments))

let color_gen ~bg =
  let open Gen in
  frequency
    [
      (4, pure None);
      (1, pure (Some (if bg then Color.default_bg else Color.default_fg)));
      (3, map (fun i -> Some (Color.indexed i)) (int_range 0 255));
      ( 3,
        map3
          (fun r g b -> Some (Color.of_rgb r g b))
          (int_range 0 255) (int_range 0 255) (int_range 0 255) );
      ( 1,
        map2
          (fun c a ->
            let r, g, b = Color.to_rgb (Color.indexed c) in
            Some (Color.of_rgba r g b a))
          (int_range 0 255)
          (of_list [ 0; 1; 128; 254 ]) );
    ]

let attrs_gen =
  let open Gen in
  map
    (fun flags ->
      List.fold_left (fun a f -> Ansi.Attr.add f a) Ansi.Attr.empty flags)
    (list ~size:(int_range 0 3)
       (of_list
          Ansi.Attr.
            [
              Bold;
              Dim;
              Italic;
              Underline;
              Double_underline;
              Blink;
              Inverse;
              Hidden;
              Strikethrough;
              Overline;
              Framed;
              Encircled;
            ]))

let style_gen =
  let open Gen in
  let* fg = color_gen ~bg:false in
  let* bg = color_gen ~bg:true in
  let* attrs = frequency [ (2, pure Ansi.Attr.empty); (3, attrs_gen) ] in
  let+ link =
    frequency
      [ (5, pure None); (1, pure (Some "http://a")); (1, pure (Some "b:c")) ]
  in
  let style = Ansi.Style.make ?fg ?bg ?link () in
  Ansi.Style.with_attrs attrs style

let op_gen ~cols ~rows =
  let open Gen in
  let coord n = int_range (-1) n in
  frequency
    [
      ( 6,
        let* x = coord (cols - 1) in
        let* y = coord (rows - 1) in
        let* text = text_gen in
        let+ style = style_gen in
        Text { x; y; text; style } );
      ( 1,
        let* x = coord cols in
        let* y = coord rows in
        let* w = int_range 0 (cols + 1) in
        let* h = int_range 0 (rows + 1) in
        let+ color =
          frequency
            [
              (1, pure Color.default_bg);
              (2, map Color.indexed (int_range 0 255));
              ( 1,
                map3 Color.of_rgb (int_range 0 255) (int_range 0 255)
                  (int_range 0 255) );
            ]
        in
        Fill { x; y; w; h; color } );
      ( 1,
        let* x = coord cols in
        let* y = coord rows in
        let* w = int_range 0 (cols + 1) in
        let+ h = int_range 0 (rows + 1) in
        Clear { x; y; w; h } );
      ( 1,
        let* x = coord (cols - 1) in
        let* y = coord (rows - 1) in
        let+ style = style_gen in
        Empty { x; y; style } );
    ]

let cursor_gen ~cols ~rows =
  let open Gen in
  let* pos =
    frequency
      [
        (1, pure None);
        (4, map2 (fun x y -> Some (x, y)) (int_range 0 cols) (int_range 0 rows));
      ]
  in
  let* visible = frequency [ (1, pure false); (2, pure true) ] in
  let* style = of_list [ `Block; `Line; `Underline ] in
  let+ blinking = bool in
  { pos; visible; style; blinking }

let frame_gen ~mode ~max_cols ~max_rows ~min_cols ~min_rows (cols, rows) =
  let open Gen in
  let* size =
    frequency
      [
        (3, pure None);
        ( 1,
          map2
            (fun c r -> Some (c, r))
            (int_range min_cols max_cols)
            (int_range min_rows max_rows) );
      ]
  in
  let cols, rows = Option.value size ~default:(cols, rows) in
  let* suspend =
    frequency
      [
        (6, pure None);
        (1, map (fun child -> Some { child }) (int_range 0 (rows + 1)));
      ]
  in
  let* statics =
    match mode with
    | `Alt -> pure []
    | `Primary ->
        frequency
          [
            (3, pure []);
            ( 1,
              list ~size:(int_range 1 2)
                (frequency
                   [
                     ( 6,
                       map2
                         (fun rows preserve -> Write { rows; preserve })
                         (int_range 1 3)
                         (frequency [ (3, pure false); (1, pure true) ]) );
                     ( 1,
                       map2
                         (fun rows preserve -> Replace { rows; preserve })
                         (int_range 0 3) bool );
                     (1, pure Clear_static);
                   ]) );
          ]
  in
  let* scroll =
    match mode with
    | `Primary -> pure None
    | `Alt ->
        frequency
          [
            (3, pure None);
            ( 1,
              map3
                (fun top bottom delta ->
                  Some { Matrix.Screen.top; bottom; delta })
                (int_range (-1) rows) (int_range (-1) rows)
                (int_range (-rows - 1) (rows + 1)) );
          ]
  in
  let* required =
    match mode with
    | `Alt -> pure None
    | `Primary ->
        frequency
          [ (2, pure None); (1, map Option.some (int_range 1 (rows + 1))) ]
  in
  let* ops = list ~size:(int_range 0 8) (op_gen ~cols ~rows) in
  let+ cursor = cursor_gen ~cols ~rows in
  ({ size; suspend; statics; required; scroll; ops; cursor }, (cols, rows))

let rec frames_gen ~mode ~max_cols ~max_rows ~min_cols ~min_rows size n =
  let open Gen in
  if n = 0 then pure []
  else
    let* frame, size =
      frame_gen ~mode ~max_cols ~max_rows ~min_cols ~min_rows size
    in
    let+ rest =
      frames_gen ~mode ~max_cols ~max_rows ~min_cols ~min_rows size (n - 1)
    in
    frame :: rest

let case_gen =
  let open Gen in
  let* mode = of_list [ `Alt; `Primary ] in
  let* width_method = of_list [ `Unicode; `Unicode_wide; `Wcwidth ] in
  (* Primary transcripts carry a tag of up to four columns; keep them whole. *)
  let min_cols, max_cols, min_rows, max_rows =
    match mode with `Alt -> (1, 10, 1, 6) | `Primary -> (4, 10, 2, 8)
  in
  let* cols = int_range min_cols max_cols in
  let* rows = int_range min_rows max_rows in
  let* shell =
    match mode with `Alt -> pure 0 | `Primary -> int_range 0 (rows - 1)
  in
  let* prompt = bool in
  let* sync = bool in
  let* reposition = bool in
  let* min_live =
    match mode with `Alt -> pure 1 | `Primary -> int_range 0 rows
  in
  let* keep = bool in
  let* styling =
    frequency
      [
        (4, pure `Truecolor);
        (1, pure `Ansi256);
        (1, pure `Ansi16);
        (1, pure `Mono);
        (1, pure `Plain);
      ]
  in
  let* n = int_range 1 6 in
  (* A frame laid out by another method than the terminal's can scroll the
     screen, which in primary mode moves the transcript for good. DECRPM
     2027 tells Unicode from wcwidth only. *)
  let* report =
    match (mode, width_method) with
    | `Alt, (`Unicode | `Wcwidth) ->
        frequency [ (3, pure None); (1, map Option.some (int_range 0 (n - 1))) ]
    | _ -> pure None
  in
  let+ frames =
    frames_gen ~mode ~max_cols ~max_rows ~min_cols ~min_rows (cols, rows) n
  in
  {
    mode;
    width_method;
    cols;
    rows;
    shell;
    prompt;
    min_live;
    sync;
    report;
    reposition;
    keep;
    styling;
    frames;
  }

let pp_style fmt (s : Ansi.Style.t) =
  let color fmt = function
    | None -> Format.pp_print_string fmt "-"
    | Some c -> Color.pp fmt c
  in
  Format.fprintf fmt "{fg=%a bg=%a attrs=%d%s}" color s.fg color s.bg
    (Ansi.Attr.pack s.attrs)
    (match s.link with None -> "" | Some l -> " link=" ^ l)

let pp_op fmt = function
  | Text { x; y; text; style } ->
      Format.fprintf fmt "text (%d,%d) %S %a" x y text pp_style style
  | Fill { x; y; w; h; color } ->
      Format.fprintf fmt "fill (%d,%d) %dx%d %a" x y w h Color.pp color
  | Clear { x; y; w; h } -> Format.fprintf fmt "clear (%d,%d) %dx%d" x y w h
  | Empty { x; y; style } ->
      Format.fprintf fmt "empty (%d,%d) %a" x y pp_style style

let pp_cursor fmt c =
  Format.fprintf fmt "cursor %s %s %s%s"
    (match c.pos with
    | None -> "default"
    | Some (x, y) -> Printf.sprintf "(%d,%d)" x y)
    (if c.visible then "visible" else "hidden")
    (match c.style with
    | `Block -> "block"
    | `Line -> "line"
    | `Underline -> "underline")
    (if c.blinking then " blinking" else "")

let pp_frame fmt f =
  Format.fprintf fmt "@[<v 2>frame";
  Option.iter (fun (c, r) -> Format.fprintf fmt "@,resize %dx%d" c r) f.size;
  Option.iter
    (fun { child } -> Format.fprintf fmt "@,release, %d child rows" child)
    f.suspend;
  List.iter
    (function
      | Write { rows; preserve } ->
          Format.fprintf fmt "@,static_write %d rows%s" rows
            (if preserve then " preserving" else "")
      | Replace { rows; preserve } ->
          Format.fprintf fmt "@,static_replace %d rows%s" rows
            (if preserve then " preserving" else "")
      | Clear_static -> Format.fprintf fmt "@,static_clear")
    f.statics;
  Option.iter (fun r -> Format.fprintf fmt "@,required %d" r) f.required;
  Option.iter
    (fun (h : Matrix.Screen.scroll_hint) ->
      Format.fprintf fmt "@,scroll %d..%d by %d" h.top h.bottom h.delta)
    f.scroll;
  List.iter (fun op -> Format.fprintf fmt "@,%a" pp_op op) f.ops;
  Format.fprintf fmt "@,%a@]" pp_cursor f.cursor

let pp_case fmt c =
  Format.fprintf fmt "@[<v 2>%s %s %dx%d"
    (match c.mode with `Alt -> "alt" | `Primary -> "primary")
    (match c.width_method with
    | `Unicode -> "unicode"
    | `Unicode_wide -> "unicode_wide"
    | `Wcwidth -> "wcwidth")
    c.cols c.rows;
  if c.sync then Format.fprintf fmt " sync";
  Option.iter (Format.fprintf fmt " report before frame %d") c.report;
  if c.reposition then Format.fprintf fmt " reposition";
  if c.mode = `Primary then
    Format.fprintf fmt " shell %d%s min_live %d%s" c.shell
      (if c.prompt then " prompt" else "")
      c.min_live
      (if c.keep then " keep" else "");
  Format.fprintf fmt " %s"
    (match c.styling with
    | `Truecolor -> "24-bit"
    | `Ansi256 -> "256"
    | `Ansi16 -> "16"
    | `Ansi8 -> "8"
    | `Mono -> "mono"
    | `Plain -> "plain");
  List.iter (fun f -> Format.fprintf fmt "@,%a" pp_frame f) c.frames;
  Format.fprintf fmt "@]"

(* Drawing ---------------------------------------------------------------- *)

let draw grid = function
  | Text { x; y; text; style } -> Grid.draw_text ~style grid ~x ~y ~text
  | Fill { x; y; w; h; color } ->
      Grid.fill_rect grid ~x ~y ~width:w ~height:h ~color
  | Clear { x; y; w; h } -> Grid.clear_rect grid ~x ~y ~width:w ~height:h
  | Empty { x; y; style } ->
      let color c ~default = Option.value c ~default in
      Grid.set_uchar grid ~x ~y ~uchar:(Uchar.of_int 0x200B)
        ~fg:(color style.fg ~default:Color.default_fg)
        ~bg:(color style.bg ~default:Color.default_bg)
        ~attrs:style.attrs ?link:style.link ()

(* A static write's rows carry a tag, so the transcript can be followed. *)
let static_grid ~cols ~tag rows =
  let grid = Grid.create ~width:cols ~height:rows () in
  for y = 0 to rows - 1 do
    Grid.draw_text grid ~x:0 ~y ~text:(Printf.sprintf "S%d" (tag + y))
  done;
  grid

let shell_line i = Printf.sprintf "$%d" i

(* What a terminal shows ------------------------------------------------- *)

(* A color as a terminal stores it: SGR sends a translucent color as its
   opaque RGB, and a transparent one as the default. *)
let shown c =
  match Color.intent c with
  | Color.Default -> `Default
  | Color.Indexed i -> `Indexed i
  | Color.Rgb ->
      if Color.alpha c = 0. then `Default
      else
        let r, g, b = Color.to_rgb c in
        `Rgb (r, g, b)

let pp_shown fmt = function
  | `Default -> Format.pp_print_string fmt "default"
  | `Indexed i -> Format.fprintf fmt "%d" i
  | `Rgb (r, g, b) -> Format.fprintf fmt "#%02x%02x%02x" r g b

let cell_text g idx =
  if Grid.is_continuation g idx then "<cont>"
  else match Grid.get_text g idx with "" -> " " | s -> s

let cell_desc g idx =
  Format.asprintf "%S w%d fg=%a bg=%a attrs=%d link=%S" (cell_text g idx)
    (Grid.cell_width g idx) pp_shown
    (shown (Grid.get_fg g idx))
    pp_shown
    (shown (Grid.get_bg g idx))
    (Grid.get_attrs g idx) (Grid.get_link g idx)

(* A color as the terminal stores it once a run writes it at [styling]. *)
let shown_at styling c =
  match styling with
  | `Plain -> `Default
  | #Color.depth as d -> shown (Color.to_depth d c)

(* What a run at [styling] writes of a cell's attributes and link. *)
let attrs_at styling g idx =
  if styling = `Plain then 0 else Grid.get_attrs g idx

let link_at styling g idx = if styling = `Plain then "" else Grid.get_link g idx

(* A space shows its foreground only through an attribute. *)
let shown_fg ?(styling = `Truecolor) g idx =
  if cell_text g idx = " " && attrs_at styling g idx = 0 then `Default
  else shown_at styling (Grid.get_fg g idx)

(* An empty cell shows as a space, one cell wide. *)
let shown_width g idx = if Grid.is_empty g idx then 1 else Grid.cell_width g idx

(* Whether the terminal shows cell [vi] of [v] as a run at [styling] writes
   cell [ei] of [e]. *)
let same_cell ?(styling = `Truecolor) e ei v vi =
  String.equal (cell_text e ei) (cell_text v vi)
  && shown_width e ei = shown_width v vi
  && shown_fg ~styling e ei = shown_fg v vi
  && shown_at styling (Grid.get_bg e ei) = shown (Grid.get_bg v vi)
  && attrs_at styling e ei = Grid.get_attrs v vi
  && String.equal (link_at styling e ei) (Grid.get_link v vi)

let blank = Grid.create ~width:1 ~height:1 ()

let row_text g y =
  let w = Grid.width g in
  let b = Buffer.create w in
  for x = 0 to w - 1 do
    let idx = (y * w) + x in
    if not (Grid.is_continuation g idx) then
      Buffer.add_string b (cell_text g idx)
  done;
  Buffer.contents b

let rtrim s =
  let n = ref (String.length s) in
  while !n > 0 && s.[!n - 1] = ' ' do
    decr n
  done;
  String.sub s 0 !n

(* The runtime ------------------------------------------------------------ *)

let capabilities case : Matrix.Terminal.capabilities =
  {
    colors =
      (match case.styling with
      | `Ansi256 -> `Ansi256
      | `Ansi16 -> `Ansi16
      | _ -> `Truecolor);
    dumb = false;
    eager_wrap = false;
    lifts_resting_cursor = false;
    kitty_keyboard = None;
    kitty_graphics = false;
    bracketed_paste = false;
    focus_tracking = false;
    unicode_width =
      (match (case.report, case.width_method) with
      | None, m -> m
      | Some _, `Wcwidth -> `Unicode
      | Some _, (`Unicode | `Unicode_wide) -> `Wcwidth);
    grapheme_mode = None;
    sgr_pixels = false;
    color_scheme_updates = false;
    explicit_width = false;
    explicit_cursor_positioning = case.reposition;
    scaled_text = false;
    sixel = false;
    graphics = `Blocks;
    sync = case.sync;
    hyperlinks = true;
    notification = `Osc9;
    meta_sends_escape = None;
    multiplexer = None;
    identity = None;
  }

type run = {
  case : case;
  vte : Vte.t;
  mutable size : int * int;
  replies : Buffer.t;  (** Replies not yet read by the runtime. *)
  frame_bytes : Buffer.t;  (** Bytes written since the last check. *)
  decscusr : Ansi.Parser.t;
  mutable last_shape : int option;
  mutable winch : bool;
      (** The terminal resized since the runtime last looked. *)
  mutable pending : frame list;
  mutable current : frame option;
  mutable drawn : (Grid.t * cursor) option;
  mutable last : (Grid.t * int) option;
      (** The last frame checked, and the rows the live region showed. *)
  mutable index : int;
  mutable transcript : string list;  (** Expected, newest first. *)
  mutable pushed : int;  (** Rows the terminal pushed while resizing. *)
  mutable tag : int;
  mutable reported : bool;
      (** The runtime knows the terminal's width method: frames are checked. *)
}

let fail_frame r fmt =
  Format.kasprintf
    (fun msg ->
      failf "frame %d: %s@\nscreen:@\n%s@\nbytes: %S" r.index msg
        (Vte.to_string r.vte)
        (Buffer.contents r.frame_bytes))
    fmt

let shape_code shape =
  let code = ref None in
  List.iter
    (function
      | Ansi.Parser.Csi { final = 'q'; intermediates = " "; params; _ } ->
          code := Some (Ansi.Parser.Params.ps params 0 ~default:0)
      | _ -> ())
    (Ansi.Parser.parse (Ansi.to_string (Ansi.cursor_style ~shape)));
  !code

let scan_shapes r bytes off len =
  ignore
    (Ansi.Parser.feed r.decscusr bytes ~off ~len (function
       | Ansi.Parser.Csi { final = 'q'; intermediates = " "; params; _ } ->
           r.last_shape <- Some (Ansi.Parser.Params.ps params 0 ~default:0)
       | _ -> ())
      : int)

(* The terminal row of the live region's first row and the rows it shows.
   In [`Alt] mode it is the screen. In [`Primary] mode it is below the
   origin, which the cursor rests on when it is hidden, and which is as many
   rows above the cursor as the cell it shows on is below it otherwise. *)
let live_region r app (cursor : cursor) =
  let _, rows = r.size in
  let _, live = Matrix.size app in
  match r.case.mode with
  | `Alt -> (0, live)
  | `Primary ->
      let shown = min live (rows - 1) in
      let below =
        if not cursor.visible then 0
        else
          match cursor.pos with
          | Some (_, y) -> min (max 0 y) (shown - 1) + 1
          | None -> shown
      in
      (fst (Vte.cursor_pos r.vte) - below + 1, shown)

let check_live r app (expected, cursor) =
  let cols, rows = r.size in
  let offset, live = live_region r app cursor in
  let v = Vte.grid r.vte in
  let ew = Grid.width expected and eh = Grid.height expected in
  if ew <> cols then fail_frame r "frame is %d columns on %d" ew cols;
  if r.case.mode = `Primary then begin
    (* The frame is drawn at the live region's height, which has the rows
       the frame requires, as far as the terminal has them. *)
    if eh <> snd (Matrix.size app) then
      fail_frame r "frame of %d rows in a live region of %d" eh
        (snd (Matrix.size app));
    (match Option.bind r.current (fun f -> f.required) with
    | Some n when live < min n (rows - 1) ->
        fail_frame r "live region of %d rows, the frame requires %d" live n
    | _ -> ());
    if offset < 1 then fail_frame r "no origin above the live region";
    let v = Vte.grid r.vte in
    for x = 0 to cols - 1 do
      let vi = ((offset - 1) * cols) + x in
      if not (same_cell blank 0 v vi) then
        fail_frame r "origin col %d shows %s" x (cell_desc v vi)
    done
  end;
  for y = 0 to live - 1 do
    for x = 0 to cols - 1 do
      let vi = ((offset + y) * cols) + x in
      let e, ei = if y < eh then (expected, (y * ew) + x) else (blank, 0) in
      if not (same_cell ~styling:r.case.styling e ei v vi) then
        fail_frame r "live row %d col %d: expected %s, shows %s" y x
          (cell_desc e ei) (cell_desc v vi)
    done
  done;
  let modes = Vte.modes r.vte in
  if modes.cursor_visible <> cursor.visible then
    fail_frame r "cursor visible %b, expected %b" modes.cursor_visible
      cursor.visible;
  if cursor.visible then begin
    (match cursor.pos with
    | None -> ()
    | Some (x, y) ->
        let want = (offset + min y (live - 1), min x (cols - 1)) in
        if Vte.cursor_pos r.vte <> want then
          fail_frame r "cursor at %d,%d, expected %d,%d"
            (fst (Vte.cursor_pos r.vte))
            (snd (Vte.cursor_pos r.vte))
            (fst want) (snd want));
    let want =
      shape_code
        (Ansi.cursor_shape_of_style ~style:cursor.style
           ~blinking:cursor.blinking)
    in
    if r.last_shape <> want then fail_frame r "cursor shape not sent"
  end

(* The transcript: the scrollback, then the rows above the live region. *)
let is_tag s =
  String.length s > 1
  && (s.[0] = 'S' || s.[0] = '$' || s.[0] = 'C')
  && String.for_all
       (fun c -> c >= '0' && c <= '9')
       (String.sub s 1 (String.length s - 1))

let check_transcript r app cursor =
  let offset, live = live_region r app cursor in
  let _, rows = r.size in
  if live < min r.case.min_live (rows - 1) then
    fail_frame r "live region of %d rows, below the minimum %d" live
      r.case.min_live;
  let v = Vte.grid r.vte in
  let history =
    scrollback_lines r.vte
    @ List.init (offset - 1) (fun y -> rtrim (row_text v y))
  in
  let expected = List.rev r.transcript in
  let tags = List.filter is_tag history in
  let others = List.length history - List.length tags in
  if tags <> expected then
    fail_frame r "transcript [%s], expected [%s]"
      (String.concat "; " history)
      (String.concat "; " expected);
  if others > r.pushed then
    fail_frame r "%d other rows in the transcript [%s], the terminal pushed %d"
      others
      (String.concat "; " history)
      r.pushed

let check r app =
  match r.drawn with
  | None -> ()
  | Some drawn ->
      if r.reported then begin
        check_live r app drawn;
        if r.case.mode = `Primary then check_transcript r app (snd drawn)
      end;
      r.last <- Some (fst drawn, snd (live_region r app (snd drawn)));
      r.drawn <- None;
      Buffer.clear r.frame_bytes

(* What a program run while the terminal is released finds: in primary mode
   the transcript, and below it the live region erased, the cursor at the
   start of its origin, which the transcript ends above; in alt mode the
   primary screen. The cursor shows. *)
let check_released r =
  let cols, rows = r.size in
  let modes = Vte.modes r.vte in
  if not modes.cursor_visible then fail_frame r "released with a hidden cursor";
  match r.case.mode with
  | `Primary ->
      let top = fst (Vte.cursor_pos r.vte) in
      let v = Vte.grid r.vte in
      for y = top to rows - 1 do
        for x = 0 to cols - 1 do
          let vi = (y * cols) + x in
          if not (same_cell blank 0 v vi) then
            fail_frame r "released: row %d col %d shows %s" y x (cell_desc v vi)
        done
      done;
      if Vte.cursor_pos r.vte <> (top, 0) then
        fail_frame r "released: cursor at %d,%d, expected %d,0"
          (fst (Vte.cursor_pos r.vte))
          (snd (Vte.cursor_pos r.vte))
          top
  | `Alt ->
      if modes.alternate_screen then
        fail_frame r "released on the alternate screen"

(* After the run: what it leaves of the live region, below the transcript. *)
and check_end r =
  let cols, rows = r.size in
  let v = Vte.grid r.vte in
  let modes = Vte.modes r.vte in
  if not modes.cursor_visible then fail_frame r "ended with a hidden cursor";
  let crow, ccol = Vte.cursor_pos r.vte in
  if ccol <> 0 then fail_frame r "ended in column %d" ccol;
  let kept =
    match (r.case.keep, r.last) with
    | true, Some (frame, shown) when r.reported ->
        min shown (Grid.active_height frame)
    | _ -> 0
  in
  (match (r.case.keep, r.last) with
  | true, Some (frame, _) when r.reported ->
      let fw = Grid.width frame in
      for y = 0 to kept - 1 do
        for x = 0 to min fw cols - 1 do
          let vi = ((crow - kept + y) * cols) + x in
          if not (same_cell ~styling:r.case.styling frame ((y * fw) + x) v vi)
          then
            fail_frame r "kept row %d col %d: expected %s, shows %s" y x
              (cell_desc frame ((y * fw) + x))
              (cell_desc v vi)
        done
      done
  | _ -> ());
  for y = crow to rows - 1 do
    for x = 0 to cols - 1 do
      let vi = (y * cols) + x in
      if not (same_cell blank 0 v vi) then
        fail_frame r "ended: row %d col %d shows %s" y x (cell_desc v vi)
    done
  done;
  let history =
    scrollback_lines r.vte
    @ List.init (crow - kept) (fun y -> rtrim (row_text v y))
  in
  let tags = List.filter is_tag history in
  let others = List.length history - List.length tags in
  if tags <> List.rev r.transcript then
    fail_frame r "ended: transcript [%s], expected [%s]"
      (String.concat "; " history)
      (String.concat "; " (List.rev r.transcript));
  if others > r.pushed then
    fail_frame r "ended: %d other rows in the transcript [%s]" others
      (String.concat "; " history)

let start_frame r app (frame : frame) =
  (match frame.size with
  | Some (cols, rows) when (cols, rows) <> r.size ->
      let before = Vte.scrollback_size r.vte in
      ignore (Vte.resize r.vte ~rows ~cols : Matrix_input.Reply.t list);
      r.pushed <- r.pushed + max 0 (Vte.scrollback_size r.vte - before);
      r.size <- (cols, rows);
      r.winch <- true
  | _ -> ());
  Option.iter
    (fun { child } ->
      Matrix.release app (fun () ->
          check_released r;
          for _ = 1 to child do
            let line = Printf.sprintf "C%d" r.tag in
            r.tag <- r.tag + 1;
            ignore (Vte.feed_string r.vte (line ^ "\r\n") : _ list);
            if r.case.mode = `Primary then r.transcript <- line :: r.transcript
          done))
    frame.suspend;
  let tagged rows =
    (* The runtime applies a resize when it next draws: until then its
       width is the one static rows are written at. *)
    let cols, _ = Matrix.full_size app in
    let grid = static_grid ~cols ~tag:r.tag rows in
    let tags = List.init rows (fun y -> Printf.sprintf "S%d" (r.tag + y)) in
    r.tag <- r.tag + rows;
    (grid, List.rev tags)
  in
  List.iter
    (function
      | Write { rows; preserve } ->
          let grid, tags = tagged rows in
          r.transcript <- tags @ r.transcript;
          Matrix.static_write ~preserve_live_region:preserve app grid
      | Replace { rows; preserve } ->
          (* The scrollback goes, with whatever a resize pushed there. *)
          let grid, tags = tagged (max rows 1) in
          r.transcript <- (if rows = 0 then [] else tags);
          r.pushed <- 0;
          Matrix.static_replace ~preserve_live_region:preserve ~rows app grid
      | Clear_static ->
          (* The screen goes; the scrollback stays. *)
          Matrix.static_clear app;
          let history = scrollback_lines r.vte in
          r.transcript <- List.rev (List.filter is_tag history);
          r.pushed <- List.length history - List.length r.transcript)
    frame.statics;
  Option.iter (Matrix.set_scroll_hint app) frame.scroll;
  r.current <- Some frame;
  r.index <- r.index + 1;
  if r.case.report = Some r.index then begin
    Buffer.add_string r.replies
      (match r.case.width_method with
      | `Wcwidth -> "\027[?2027;4$y"
      | `Unicode | `Unicode_wide -> "\027[?2027;1$y");
    r.reported <- true
  end;
  Matrix.request_redraw app

let render r app grid =
  match r.current with
  | None -> ()
  | Some frame ->
      List.iter (draw grid) frame.ops;
      let c = frame.cursor in
      let screen = Matrix.screen app in
      Matrix.Screen.set_cursor screen
        {
          (Matrix.Screen.cursor screen) with
          position = c.pos;
          visible = c.visible;
          style = c.style;
          blinking = c.blinking;
        };
      r.drawn <- Some (Grid.copy grid, c)

(* A run on device [d], with [config], as [Matrix.run] gives one. *)
let run_on d ?stop ?(config = Matrix.config ()) f =
  Matrix.run ?stop ~config:(Matrix.on_device d config) f

let run_case case =
  let vte =
    Vte.create ~scrollback:4096 ~width_method:case.width_method ~rows:case.rows
      ~cols:case.cols ()
  in
  let r =
    {
      case;
      vte;
      size = (case.cols, case.rows);
      replies = Buffer.create 64;
      frame_bytes = Buffer.create 1024;
      decscusr = Ansi.Parser.create ();
      last_shape = None;
      winch = false;
      pending = case.frames;
      current = None;
      drawn = None;
      last = None;
      index = -1;
      transcript = [];
      pushed = 0;
      tag = 0;
      reported = case.report = None;
    }
  in
  let primary = case.mode = `Primary in
  if primary then begin
    for i = 0 to case.shell - 1 do
      ignore (Vte.feed_string vte (shell_line i ^ "\r\n") : _ list);
      r.transcript <- shell_line i :: r.transcript
    done;
    if case.prompt then begin
      ignore (Vte.feed_string vte (shell_line case.shell) : _ list);
      r.transcript <- shell_line case.shell :: r.transcript
    end
  end;
  let write_output b off len =
    Buffer.add_subbytes r.frame_bytes b off len;
    scan_shapes r b off len;
    let reply = Vte.feed vte b off len in
    if primary then
      Buffer.add_string r.replies (Matrix_input.Reply.encode reply)
  in
  let read_input b off len =
    let n = min len (Buffer.length r.replies) in
    Buffer.blit r.replies 0 b off n;
    let rest = Buffer.sub r.replies n (Buffer.length r.replies - n) in
    Buffer.clear r.replies;
    Buffer.add_string r.replies rest;
    if n = 0 then -1 else n
  in
  let now = ref 0 in
  (* The clock moves only to the deadline the loop waits for, a frame's. *)
  let wait ~until ~reading =
    if reading && Buffer.length r.replies > 0 then true
    else (
      (match until with
      | Some until -> now := max !now until
      | None -> failwith "the loop would wait forever");
      false)
  in
  let backend : Matrix.Device.t =
    {
      now = (fun () -> !now);
      wait;
      wake = ignore;
      take_signals =
        (fun () ->
          if r.winch then (
            r.winch <- false;
            [ Winch ])
          else []);
      write_output;
      read_input;
      input_is_tty = primary;
      output_is_tty = true;
      controlling = false;
      on_fatal = ignore;
      fds = (fun ~reading:_ -> []);
      terminal_size = (fun () -> Some r.size);
      window_pixels = (fun () -> None);
      set_raw_mode = ignore;
      flush_input = ignore;
      cleanup = ignore;
      env =
        Matrix.Terminal.Caps.env
          (function
            | "NO_COLOR" when case.styling = `Mono -> Some "1" | _ -> None)
          ~win32:false;
      in_process = None;
    }
  in
  let config =
    Matrix.config ~mode:case.mode ~mouse:`Off ~bracketed_paste:false
      ?color:(if case.styling = `Plain then Some `Never else None)
      ~kitty_keyboard:`Off
      ?min_tui_height:(if case.min_live = 0 then None else Some case.min_live)
      ~caps:(capabilities case) ()
  in
  run_on backend ~config (fun app ->
      (* Each frame of the case is set up, drawn and checked in turn. *)
      let next_frame () =
        match r.pending with
        | [] -> false
        | frame :: rest ->
            r.pending <- rest;
            start_frame r app frame;
            true
      in
      let rec loop () =
        match Matrix.next app with
        | Frame ->
            Matrix.draw app
              ?primary_required_rows:
                (Option.bind r.current (fun frame -> frame.required))
              (render r app);
            check r app;
            if next_frame () then loop ()
        | Stopped -> ()
        | Event _ | Timer _ | Posted _ -> loop ()
      in
      Matrix.keep_frame app r.case.keep;
      if next_frame () then loop ());
  if primary then check_end r

let () =
  run "matrix.render.props"
    [
      group "oracle"
        [
          prop ~count:(count 2000) "the terminal shows each frame"
            (Gen.with_pp pp_case case_gen)
            run_case;
        ];
    ]
