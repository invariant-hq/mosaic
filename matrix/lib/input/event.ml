(* Terminal input events: keys, modifiers, mouse, responses and high-level
   events. *)

module Modifier = struct
  type t = {
    ctrl : bool;
    alt : bool;
    shift : bool;
    super : bool;
    hyper : bool;
    meta : bool;
    caps_lock : bool;
    num_lock : bool;
  }

  let none =
    {
      ctrl = false;
      alt = false;
      shift = false;
      super = false;
      hyper = false;
      meta = false;
      caps_lock = false;
      num_lock = false;
    }

  let equal (a : t) (b : t) = a = b

  let pp fmt m =
    let mods = [] in
    let mods = if m.shift then "shift" :: mods else mods in
    let mods = if m.alt then "alt" :: mods else mods in
    let mods = if m.ctrl then "ctrl" :: mods else mods in
    let mods = if m.super then "super" :: mods else mods in
    let mods = if m.hyper then "hyper" :: mods else mods in
    let mods = if m.meta then "meta" :: mods else mods in
    let mods = if m.caps_lock then "caps_lock" :: mods else mods in
    let mods = if m.num_lock then "num_lock" :: mods else mods in
    match mods with
    | [] -> Format.fprintf fmt "none"
    | _ -> Format.fprintf fmt "{%s}" (String.concat "+" mods)

  let ctrl (m : t) = m.ctrl
  let alt (m : t) = m.alt
  let shift (m : t) = m.shift
end

module Key = struct
  type t =
    | Char of Uchar.t
    | Enter
    | Line_feed
    | Tab
    | Backspace
    | Delete
    | Escape
    | Up
    | Down
    | Left
    | Right
    | Home
    | End
    | Page_up
    | Page_down
    | Insert
    | F of int
    | Print_screen
    | Pause
    | Menu
    | Scroll_lock
    | Media_play
    | Media_pause
    | Media_play_pause
    | Media_stop
    | Media_reverse
    | Media_fast_forward
    | Media_rewind
    | Media_next
    | Media_prev
    | Media_record
    | Volume_up
    | Volume_down
    | Volume_mute
    | Shift_left
    | Shift_right
    | Ctrl_left
    | Ctrl_right
    | Alt_left
    | Alt_right
    | Super_left
    | Super_right
    | Hyper_left
    | Hyper_right
    | Meta_left
    | Meta_right
    | Iso_level3_shift
    | Iso_level5_shift
    | Caps_lock
    | Num_lock
    | KP_0
    | KP_1
    | KP_2
    | KP_3
    | KP_4
    | KP_5
    | KP_6
    | KP_7
    | KP_8
    | KP_9
    | KP_decimal
    | KP_divide
    | KP_multiply
    | KP_subtract
    | KP_add
    | KP_enter
    | KP_equal
    | KP_separator
    | KP_begin
    | KP_left
    | KP_right
    | KP_up
    | KP_down
    | KP_page_up
    | KP_page_down
    | KP_home
    | KP_end
    | KP_insert
    | KP_delete
    | Unknown of int

  (* Shared one-character strings for ASCII to avoid per-event allocation. *)
  let ascii_strings : string array =
    Array.init 128 (fun i -> String.make 1 (Char.chr i))

  let[@inline] one_char_string c =
    let code = Char.code c in
    if code < 128 then ascii_strings.(code) else String.make 1 c

  let equal (a : t) (b : t) = a = b

  let pp fmt = function
    | Char u ->
        let b = Buffer.create 4 in
        Buffer.add_utf_8_uchar b u;
        Format.fprintf fmt "Char(%s)" (Buffer.contents b)
    | F n -> Format.fprintf fmt "F%d" n
    | Unknown n -> Format.fprintf fmt "Unknown(%d)" n
    | other ->
        let name =
          match other with
          | Enter -> "Enter"
          | Line_feed -> "Line_feed"
          | Tab -> "Tab"
          | Backspace -> "Backspace"
          | Delete -> "Delete"
          | Escape -> "Escape"
          | Up -> "Up"
          | Down -> "Down"
          | Left -> "Left"
          | Right -> "Right"
          | Home -> "Home"
          | End -> "End"
          | Page_up -> "Page_up"
          | Page_down -> "Page_down"
          | Insert -> "Insert"
          | Print_screen -> "Print_screen"
          | Pause -> "Pause"
          | Menu -> "Menu"
          | Scroll_lock -> "Scroll_lock"
          | Media_play -> "Media_play"
          | Media_pause -> "Media_pause"
          | Media_play_pause -> "Media_play_pause"
          | Media_stop -> "Media_stop"
          | Media_reverse -> "Media_reverse"
          | Media_fast_forward -> "Media_fast_forward"
          | Media_rewind -> "Media_rewind"
          | Media_next -> "Media_next"
          | Media_prev -> "Media_prev"
          | Media_record -> "Media_record"
          | Volume_up -> "Volume_up"
          | Volume_down -> "Volume_down"
          | Volume_mute -> "Volume_mute"
          | Shift_left -> "Shift_left"
          | Shift_right -> "Shift_right"
          | Ctrl_left -> "Ctrl_left"
          | Ctrl_right -> "Ctrl_right"
          | Alt_left -> "Alt_left"
          | Alt_right -> "Alt_right"
          | Super_left -> "Super_left"
          | Super_right -> "Super_right"
          | Hyper_left -> "Hyper_left"
          | Hyper_right -> "Hyper_right"
          | Meta_left -> "Meta_left"
          | Meta_right -> "Meta_right"
          | Iso_level3_shift -> "Iso_level3_shift"
          | Iso_level5_shift -> "Iso_level5_shift"
          | Caps_lock -> "Caps_lock"
          | Num_lock -> "Num_lock"
          | KP_0 -> "KP_0"
          | KP_1 -> "KP_1"
          | KP_2 -> "KP_2"
          | KP_3 -> "KP_3"
          | KP_4 -> "KP_4"
          | KP_5 -> "KP_5"
          | KP_6 -> "KP_6"
          | KP_7 -> "KP_7"
          | KP_8 -> "KP_8"
          | KP_9 -> "KP_9"
          | KP_decimal -> "KP_decimal"
          | KP_divide -> "KP_divide"
          | KP_multiply -> "KP_multiply"
          | KP_subtract -> "KP_subtract"
          | KP_add -> "KP_add"
          | KP_enter -> "KP_enter"
          | KP_equal -> "KP_equal"
          | KP_separator -> "KP_separator"
          | KP_begin -> "KP_begin"
          | KP_left -> "KP_left"
          | KP_right -> "KP_right"
          | KP_up -> "KP_up"
          | KP_down -> "KP_down"
          | KP_page_up -> "KP_page_up"
          | KP_page_down -> "KP_page_down"
          | KP_home -> "KP_home"
          | KP_end -> "KP_end"
          | KP_insert -> "KP_insert"
          | KP_delete -> "KP_delete"
          | Char _ | F _ | Unknown _ -> "Key"
        in
        Format.pp_print_string fmt name

  type event_type = Press | Repeat | Release

  let pp_event_type fmt = function
    | Press -> Format.pp_print_string fmt "Press"
    | Repeat -> Format.pp_print_string fmt "Repeat"
    | Release -> Format.pp_print_string fmt "Release"

  type event = {
    key : t;
    modifier : Modifier.t;
    event_type : event_type;
    associated_text : string;
    shifted_key : Uchar.t option;
    base_key : Uchar.t option;
  }

  let make ?(modifier = Modifier.none) ?(event_type = Press)
      ?(associated_text = "") ?shifted_key ?base_key key =
    { key; modifier; event_type; associated_text; shifted_key; base_key }

  let of_char ?modifier ?event_type ?associated_text ?shifted_key ?base_key c =
    let associated_text =
      match associated_text with Some s -> s | None -> one_char_string c
    in
    make ?modifier ?event_type ~associated_text ?shifted_key ?base_key
      (Char (Uchar.of_char c))

  let equal_event (a : event) (b : event) = a = b

  let pp_uchar fmt u =
    let b = Buffer.create 4 in
    Buffer.add_utf_8_uchar b u;
    Format.fprintf fmt "%S" (Buffer.contents b)

  let pp_event fmt e =
    Format.fprintf fmt "{key=%a; modifier=%a; event_type=%a" pp e.key
      Modifier.pp e.modifier pp_event_type e.event_type;
    if e.associated_text <> "" then
      Format.fprintf fmt "; associated_text=%S" e.associated_text;
    (match e.shifted_key with
    | None -> ()
    | Some u -> Format.fprintf fmt "; shifted_key=%a" pp_uchar u);
    (match e.base_key with
    | None -> ()
    | Some u -> Format.fprintf fmt "; base_key=%a" pp_uchar u);
    Format.fprintf fmt "}"

  (* helpers *)

  let is_char = function Char _ -> true | _ -> false
  let is_enter = function Enter -> true | _ -> false
  let is_arrow = function Up | Down | Left | Right -> true | _ -> false
  let is_function = function F _ -> true | _ -> false

  let is_ctrl_char = function
    | Char u ->
        let c = Uchar.to_int u in
        c < 0x20 || c = 0x7F
    | _ -> false
end

module Mouse = struct
  type button = Left | Middle | Right | Button of int

  let equal_button (a : button) (b : button) = a = b

  let pp_button fmt = function
    | Left -> Format.pp_print_string fmt "Left"
    | Middle -> Format.pp_print_string fmt "Middle"
    | Right -> Format.pp_print_string fmt "Right"
    | Button n -> Format.fprintf fmt "Button(%d)" n

  type scroll_direction = Scroll_up | Scroll_down | Scroll_left | Scroll_right

  let equal_scroll_direction (a : scroll_direction) (b : scroll_direction) =
    a = b

  let pp_scroll_direction fmt = function
    | Scroll_up -> Format.pp_print_string fmt "Scroll_up"
    | Scroll_down -> Format.pp_print_string fmt "Scroll_down"
    | Scroll_left -> Format.pp_print_string fmt "Scroll_left"
    | Scroll_right -> Format.pp_print_string fmt "Scroll_right"

  type kind =
    | Down of { button : button }
    | Up of { button : button option }
    | Move
    | Drag of { button : button }
    | Scroll of { direction : scroll_direction; delta : int }

  let equal_kind (a : kind) (b : kind) = a = b

  let pp_kind fmt = function
    | Down { button } -> Format.fprintf fmt "Down(%a)" pp_button button
    | Up { button = Some button } ->
        Format.fprintf fmt "Up(%a)" pp_button button
    | Up { button = None } -> Format.pp_print_string fmt "Up(None)"
    | Move -> Format.pp_print_string fmt "Move"
    | Drag { button } -> Format.fprintf fmt "Drag(%a)" pp_button button
    | Scroll { direction; delta } ->
        Format.fprintf fmt "Scroll(%a,%d)" pp_scroll_direction direction delta

  type event = { x : int; y : int; modifiers : Modifier.t; kind : kind }

  let make ~x ~y ~modifiers kind = { x; y; modifiers; kind }
  let equal_event (a : event) (b : event) = a = b

  let pp_event fmt e =
    Format.fprintf fmt "{x=%d; y=%d; modifiers=%a; kind=%a}" e.x e.y Modifier.pp
      e.modifiers pp_kind e.kind
end

module Response = struct
  type mode_report = { is_private : bool; modes : (int * int) list }

  let equal_mode_report (a : mode_report) (b : mode_report) = a = b

  let pp_mode_report fmt r =
    let pairs =
      match r.modes with
      | [] -> ""
      | _ ->
          r.modes
          |> List.map (fun (m, v) -> Printf.sprintf "%d:%d" m v)
          |> String.concat ";"
    in
    Format.fprintf fmt "Mode_report(is_private=%b,[%s])" r.is_private pairs

  type capability =
    | Device_attributes of int list
    | Mode_report of mode_report
    | Pixel_resolution of int * int
    | Cursor_position of int * int
    | Xtversion of string
    | Kitty_graphics_reply of string
    | Kitty_keyboard of { level : int; flags : int option }
    | Color_scheme of [ `Dark | `Light | `Unknown of int ]

  let equal_capability (a : capability) (b : capability) = a = b

  let pp_capability fmt = function
    | Device_attributes attrs ->
        Format.fprintf fmt "Device_attributes([%s])"
          (String.concat ";" (List.map string_of_int attrs))
    | Mode_report r -> pp_mode_report fmt r
    | Pixel_resolution (w, h) ->
        Format.fprintf fmt "Pixel_resolution(%d,%d)" w h
    | Cursor_position (row, col) ->
        Format.fprintf fmt "Cursor_position(%d,%d)" row col
    | Xtversion s -> Format.fprintf fmt "Xtversion(%S)" s
    | Kitty_graphics_reply s -> Format.fprintf fmt "Kitty_graphics_reply(%S)" s
    | Kitty_keyboard { level; flags } -> (
        match flags with
        | None -> Format.fprintf fmt "Kitty_keyboard(level=%d)" level
        | Some f ->
            Format.fprintf fmt "Kitty_keyboard(level=%d,flags=%d)" level f)
    | Color_scheme scheme ->
        let s =
          match scheme with
          | `Dark -> "Dark"
          | `Light -> "Light"
          | `Unknown v -> Printf.sprintf "Unknown(%d)" v
        in
        Format.fprintf fmt "Color_scheme(%s)" s

  type t =
    | Capability of capability
    | Clipboard of string * string
    | Osc of int * string
    | Unknown of string

  let equal (a : t) (b : t) = a = b

  let pp fmt = function
    | Capability c -> Format.fprintf fmt "Capability(%a)" pp_capability c
    | Clipboard (sel, data) -> Format.fprintf fmt "Clipboard(%S,%S)" sel data
    | Osc (code, data) -> Format.fprintf fmt "Osc(%d,%S)" code data
    | Unknown s -> Format.fprintf fmt "Unknown(%S)" s
end

type t =
  | Key of Key.event
  | Mouse of Mouse.event
  | Resize of int * int
  | Focus
  | Blur
  | Paste of string

let equal (e1 : t) (e2 : t) =
  match (e1, e2) with
  | Key k1, Key k2 -> Key.equal_event k1 k2
  | Mouse m1, Mouse m2 -> Mouse.equal_event m1 m2
  | Resize (w1, h1), Resize (w2, h2) -> w1 = w2 && h1 = h2
  | Focus, Focus -> true
  | Blur, Blur -> true
  | Paste s1, Paste s2 -> s1 = s2
  | (Key _ | Mouse _ | Resize _ | Focus | Blur | Paste _), _ -> false

let pp fmt = function
  | Key k -> Format.fprintf fmt "Key(%a)" Key.pp_event k
  | Mouse m -> Format.fprintf fmt "Mouse(%a)" Mouse.pp_event m
  | Resize (w, h) -> Format.fprintf fmt "Resize(%d,%d)" w h
  | Focus -> Format.pp_print_string fmt "Focus"
  | Blur -> Format.pp_print_string fmt "Blur"
  | Paste s -> Format.fprintf fmt "Paste(%S)" s

(* Convenience constructors *)

let key ?modifier ?event_type ?associated_text ?shifted_key ?base_key k =
  Key (Key.make ?modifier ?event_type ?associated_text ?shifted_key ?base_key k)

let char ?modifier ?event_type ?associated_text ?shifted_key ?base_key c =
  Key
    (Key.of_char ?modifier ?event_type ?associated_text ?shifted_key ?base_key c)

let key_event ?modifier ?event_type ?associated_text ?shifted_key ?base_key k =
  Key.make ?modifier ?event_type ?associated_text ?shifted_key ?base_key k

let char_event ?modifier ?event_type ?associated_text ?shifted_key ?base_key c =
  Key.of_char ?modifier ?event_type ?associated_text ?shifted_key ?base_key c

let press ?modifier ?associated_text ?shifted_key ?base_key k =
  key_event ?modifier ~event_type:Key.Press ?associated_text ?shifted_key
    ?base_key k

let repeat ?modifier ?associated_text ?shifted_key ?base_key k =
  key_event ?modifier ~event_type:Key.Repeat ?associated_text ?shifted_key
    ?base_key k

let release ?modifier ?associated_text ?shifted_key ?base_key k =
  key_event ?modifier ~event_type:Key.Release ?associated_text ?shifted_key
    ?base_key k

let mouse_press ?(modifiers = Modifier.none) x y button =
  Mouse (Mouse.make ~x ~y ~modifiers (Mouse.Down { button }))

let mouse_release ?(modifiers = Modifier.none) x y button =
  Mouse (Mouse.make ~x ~y ~modifiers (Mouse.Up { button }))

let mouse_move ?(modifiers = Modifier.none) x y =
  Mouse (Mouse.make ~x ~y ~modifiers Mouse.Move)

let mouse_drag ?(modifiers = Modifier.none) x y button =
  Mouse (Mouse.make ~x ~y ~modifiers (Mouse.Drag { button }))

let mouse_scroll ?(modifiers = Modifier.none) ?(delta = 1) x y direction =
  Mouse (Mouse.make ~x ~y ~modifiers (Mouse.Scroll { direction; delta }))

(* Helpers that depend on [t] *)

let match_ctrl_char = function
  | Key { Key.key = Char u; modifier; _ } when modifier.ctrl ->
      let code = Uchar.to_int u in
      if code < 0x80 then Some (Char.chr code) else None
  | _ -> None

let is_scroll = function
  | Mouse { Mouse.kind = Scroll _; _ } -> true
  | _ -> false

let is_drag = function Mouse { Mouse.kind = Drag _; _ } -> true | _ -> false
