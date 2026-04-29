type 'a binding = {
  key : Event.Key.t;
  ctrl : bool option;
  alt : bool option;
  shift : bool option;
  super : bool option;
  hyper : bool option;
  meta : bool option;
  data : 'a;
}

type 'a t = 'a binding list

let empty = []
let add_binding map binding = binding :: map

let add ?ctrl ?alt ?shift ?super ?hyper ?meta map key data =
  add_binding map { key; ctrl; alt; shift; super; hyper; meta; data }

let add_char ?(ctrl = false) ?(alt = false) ?(shift = false) ?(super = false)
    ?(hyper = false) ?(meta = false) map c data =
  add_binding map
    {
      key = Char (Uchar.of_char c);
      ctrl = Some ctrl;
      alt = Some alt;
      shift = Some shift;
      super = Some super;
      hyper = Some hyper;
      meta = Some meta;
      data;
    }

let matches_modifier (cond : _ binding) (actual : Event.Key.modifier) =
  let check_opt opt field =
    match opt with None -> true | Some v -> v = field
  in
  check_opt cond.ctrl actual.ctrl
  && check_opt cond.alt actual.alt
  && check_opt cond.shift actual.shift
  && check_opt cond.super actual.super
  && check_opt cond.hyper actual.hyper
  && check_opt cond.meta actual.meta

let default_event_type_filter = function
  | Event.Key.Press | Event.Key.Repeat -> true
  | Event.Key.Release -> false

let normalized_char_key u =
  let code = Uchar.to_int u in
  if code >= Char.code 'A' && code <= Char.code 'Z' then
    Event.Key.Char (Uchar.of_int (code + 32))
  else Char u

let matching_keys key base_key =
  match base_key with
  | Some u -> (
      let physical = normalized_char_key u in
      match Event.Key.equal key physical with
      | true -> [ key ]
      | false -> [ key; physical ])
  | None -> [ key ]

let find ?(event_type = default_event_type_filter) map = function
  | Event.Key { key; modifier; event_type = et; base_key; _ } ->
      if not (event_type et) then None
      else
        let keys = matching_keys key base_key in
        let rec loop = function
          | [] -> None
          | b :: rest ->
              if
                List.exists (fun key -> Event.Key.equal key b.key) keys
                && matches_modifier b modifier
              then Some b.data
              else loop rest
        in
        loop map
  | _ -> None
