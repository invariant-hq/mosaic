module Keystroke = Matrix_input.Keystroke

type t = { key : Keystroke.key; modifiers : int }

let modifiers (m : Keystroke.mods) =
  Bool.to_int m.ctrl
  lor (Bool.to_int m.alt lsl 1)
  lor (Bool.to_int m.shift lsl 2)
  lor (Bool.to_int m.super lsl 3)
  lor (Bool.to_int m.hyper lsl 4)
  lor (Bool.to_int m.meta lsl 5)

(* Shift with an ASCII letter types the capital, which is the key's value. *)
let key ?(ctrl = false) ?(alt = false) ?(shift = false) ?(super = false)
    ?(hyper = false) ?(meta = false) (key : Keystroke.key) =
  let key, shift =
    match key with
    | Char c when shift && Uchar.is_char c -> (
        match Uchar.to_char c with
        | 'a' .. 'z' as c ->
            (Keystroke.Char (Uchar.of_char (Char.uppercase_ascii c)), false)
        | 'A' .. 'Z' -> (key, false)
        | _ -> (key, shift))
    | key -> (key, shift)
  in
  {
    key;
    modifiers =
      modifiers { Keystroke.no_mods with ctrl; alt; shift; super; hyper; meta };
  }

let char ?ctrl ?alt ?shift ?super ?hyper ?meta c =
  key ?ctrl ?alt ?shift ?super ?hyper ?meta (Char (Uchar.of_char c))

let ctrl c = char ~ctrl:true c
let alt c = char ~alt:true c
let shift c = char ~shift:true c
let escape = key Escape
let enter = key Enter
let tab = key Tab
let backspace = key Backspace
let delete = key Delete
let f n = key (F n)
let up = key Up
let down = key Down
let left = key Left
let right = key Right
let space = char ' '
let equal a b = a.modifiers = b.modifiers && a.key = b.key
let is_upper c = c >= Char.code 'A' && c <= Char.code 'Z'
let is_lower c = c >= Char.code 'a' && c <= Char.code 'z'

(* Shift is bit 4 of [modifiers]; any other bit is a chord. *)
let is_chord modifier = modifier land lnot 4 <> 0

(* A letter typed with no chord modifier is looked up with Caps Lock undone
   when the keystroke says the lock is on, then, as a capital that matches
   nothing, as its lowercase letter: a terminal that does not report the
   lock sends Caps Lock with r as R. A chord's key is never changed by a
   lock, so a chord is looked up as it is. *)
let find_typed ~found find_char bindings modifier (mods : Keystroke.mods) c =
  if is_chord modifier || not (is_upper c || is_lower c) then
    find_char bindings modifier c
  else
    let c = if mods.caps_lock then c lxor 0x20 else c in
    let r = find_char bindings modifier c in
    if (not (found r)) && is_upper c then
      find_char bindings modifier (c lor 0x20)
    else r

(* [found] tells a result that found something from one that did not. *)
let resolve ~found ~find_char ~find_key bindings (k : Keystroke.t) =
  let modifier = modifiers k.mods in
  let direct =
    match k.key with
    | Char c ->
        find_typed ~found find_char bindings modifier k.mods (Uchar.to_int c)
    | key -> find_key bindings modifier key
  in
  match k.base with
  | Some base when not (found direct) ->
      find_char bindings modifier (Uchar.to_int base)
  | Some _ | None -> direct

let matches_char t modifier code =
  t.modifiers = modifier
  && match t.key with Char c -> Uchar.to_int c = code | _ -> false

let matches_key t modifier key = t.modifiers = modifier && t.key = key

let find_char t modifier code =
  if matches_char t modifier code then Some () else None

let find_key t modifier key =
  if matches_key t modifier key then Some () else None

let matches t event =
  Option.is_some
    (resolve ~found:Option.is_some ~find_char ~find_key t (Event.Key.data event))

(* The bindings from the last one that matches, [[]] when none does: a result
   with no allocation. *)
let rec find_last matches modifier key found = function
  | [] -> found
  | (shortcut, _) :: rest as bindings ->
      let found = if matches shortcut modifier key then bindings else found in
      find_last matches modifier key found rest

let find_list_char bindings modifier code =
  find_last matches_char modifier code [] bindings

let find_list_key bindings modifier key =
  find_last matches_key modifier key [] bindings

let found = function [] -> false | _ :: _ -> true

let find bindings event =
  match
    resolve ~found ~find_char:find_list_char ~find_key:find_list_key bindings
      (Event.Key.data event)
  with
  | (_, value) :: _ -> value
  | [] -> raise_notrace Not_found

let find_opt bindings event =
  match find bindings event with
  | value -> Some value
  | exception Not_found -> None
