type 'a binding = {
  name : string;
  action : 'a;
  ctrl : bool;
  shift : bool;
  alt : bool;
  super : bool;
}

let binding ?(ctrl = false) ?(shift = false) ?(alt = false) ?(super = false)
    name action =
  { name; action; ctrl; shift; alt; super }

type key = {
  name : string;
  ctrl : bool;
  shift : bool;
  alt : bool;
  super : bool;
}

type 'a t = {
  actions : (string, 'a) Hashtbl.t;
  aliases : (string, string) Hashtbl.t;
}

let key_of_binding (b : _ binding) =
  {
    name = b.name;
    ctrl = b.ctrl;
    shift = b.shift;
    alt = b.alt;
    super = b.super;
  }

let key_string k =
  String.concat ":"
    [
      k.name;
      string_of_int (Bool.to_int k.ctrl);
      string_of_int (Bool.to_int k.shift);
      string_of_int (Bool.to_int k.alt);
      string_of_int (Bool.to_int k.super);
    ]

let with_name k name = { k with name }

let default_aliases =
  [
    ("enter", "return");
    ("esc", "escape");
    ("kp0", "0");
    ("kp1", "1");
    ("kp2", "2");
    ("kp3", "3");
    ("kp4", "4");
    ("kp5", "5");
    ("kp6", "6");
    ("kp7", "7");
    ("kp8", "8");
    ("kp9", "9");
    ("kpdecimal", ".");
    ("kpdivide", "/");
    ("kpmultiply", "*");
    ("kpminus", "-");
    ("kpplus", "+");
    ("kpenter", "enter");
    ("kpequal", "=");
    ("kpseparator", ",");
    ("kpleft", "left");
    ("kpright", "right");
    ("kpup", "up");
    ("kpdown", "down");
    ("kppageup", "pageup");
    ("kppagedown", "pagedown");
    ("kphome", "home");
    ("kpend", "end");
    ("kpinsert", "insert");
    ("kpdelete", "delete");
  ]

let merge_aliases custom =
  let aliases = Hashtbl.create 32 in
  List.iter
    (fun (name, alias) -> Hashtbl.replace aliases name alias)
    default_aliases;
  List.iter (fun (name, alias) -> Hashtbl.replace aliases name alias) custom;
  aliases

let add_binding actions aliases (b : _ binding) =
  let key = key_of_binding b in
  Hashtbl.replace actions (key_string key) b.action;
  match Hashtbl.find_opt aliases b.name with
  | None -> ()
  | Some name ->
      Hashtbl.replace actions (key_string (with_name key name)) b.action

let make ?(aliases = []) ~defaults ?(custom = []) () =
  let aliases = merge_aliases aliases in
  let actions = Hashtbl.create (List.length defaults + List.length custom) in
  List.iter (add_binding actions aliases) defaults;
  List.iter (add_binding actions aliases) custom;
  { actions; aliases }

let char_name u =
  let code = Uchar.to_int u in
  if code >= Char.code 'A' && code <= Char.code 'Z' then
    String.make 1 (Char.chr (code + 32))
  else
    let b = Buffer.create 4 in
    Buffer.add_utf_8_uchar b u;
    Buffer.contents b

let name_of_key key =
  let open Input.Key in
  match key with
  | Char u -> Some (char_name u)
  | Enter -> Some "return"
  | Line_feed -> Some "linefeed"
  | Tab -> Some "tab"
  | Backspace -> Some "backspace"
  | Delete -> Some "delete"
  | Escape -> Some "escape"
  | Up -> Some "up"
  | Down -> Some "down"
  | Left -> Some "left"
  | Right -> Some "right"
  | Home -> Some "home"
  | End -> Some "end"
  | Page_up -> Some "pageup"
  | Page_down -> Some "pagedown"
  | Insert -> Some "insert"
  | F n -> Some ("f" ^ string_of_int n)
  | KP_0 -> Some "kp0"
  | KP_1 -> Some "kp1"
  | KP_2 -> Some "kp2"
  | KP_3 -> Some "kp3"
  | KP_4 -> Some "kp4"
  | KP_5 -> Some "kp5"
  | KP_6 -> Some "kp6"
  | KP_7 -> Some "kp7"
  | KP_8 -> Some "kp8"
  | KP_9 -> Some "kp9"
  | KP_decimal -> Some "kpdecimal"
  | KP_divide -> Some "kpdivide"
  | KP_multiply -> Some "kpmultiply"
  | KP_subtract -> Some "kpminus"
  | KP_add -> Some "kpplus"
  | KP_enter -> Some "kpenter"
  | KP_equal -> Some "kpequal"
  | KP_separator -> Some "kpseparator"
  | KP_left -> Some "kpleft"
  | KP_right -> Some "kpright"
  | KP_up -> Some "kpup"
  | KP_down -> Some "kpdown"
  | KP_page_up -> Some "kppageup"
  | KP_page_down -> Some "kppagedown"
  | KP_home -> Some "kphome"
  | KP_end -> Some "kpend"
  | KP_insert -> Some "kpinsert"
  | KP_delete -> Some "kpdelete"
  | Print_screen | Pause | Menu | Scroll_lock | Media_play | Media_pause
  | Media_play_pause | Media_stop | Media_reverse | Media_fast_forward
  | Media_rewind | Media_next | Media_prev | Media_record | Volume_up
  | Volume_down | Volume_mute | Shift_left | Shift_right | Ctrl_left
  | Ctrl_right | Alt_left | Alt_right | Super_left | Super_right | Hyper_left
  | Hyper_right | Meta_left | Meta_right | Iso_level3_shift | Iso_level5_shift
  | Caps_lock | Num_lock | KP_begin | Unknown _ ->
      None

let base_name = function
  | None -> None
  | Some u ->
      let code = Uchar.to_int u in
      if code < 32 || code = 127 then None else Some (char_name u)

let lookup t ev name =
  let m = ev.Input.Key.modifier in
  let key =
    {
      name;
      ctrl = m.ctrl;
      shift = m.shift;
      alt = m.alt || m.meta;
      super = m.super;
    }
  in
  let rec find_alias seen name =
    if List.exists (String.equal name) seen then None
    else
      match Hashtbl.find_opt t.aliases name with
      | None -> None
      | Some alias -> (
          let key = key_string (with_name key alias) in
          match Hashtbl.find_opt t.actions key with
          | Some _ as action -> action
          | None -> find_alias (name :: seen) alias)
  in
  match Hashtbl.find_opt t.actions (key_string key) with
  | Some _ as action -> action
  | None -> find_alias [] name

let lookup_base_key t ev =
  match base_name ev.Input.Key.base_key with
  | None -> None
  | Some name -> lookup t ev name

let action t ev =
  match name_of_key ev.Input.Key.key with
  | None -> lookup_base_key t ev
  | Some name -> (
      match lookup t ev name with
      | Some _ as action -> action
      | None -> lookup_base_key t ev)
