type role =
  | Plain
  | Muted
  | Emphasis
  | Accent
  | Link
  | Ok
  | Warning
  | Error
  | Added
  | Removed
  | Border

let roles =
  [
    Plain;
    Muted;
    Emphasis;
    Accent;
    Link;
    Ok;
    Warning;
    Error;
    Added;
    Removed;
    Border;
  ]

let index = function
  | Plain -> 0
  | Muted -> 1
  | Emphasis -> 2
  | Accent -> 3
  | Link -> 4
  | Ok -> 5
  | Warning -> 6
  | Error -> 7
  | Added -> 8
  | Removed -> 9
  | Border -> 10

type colors = Color.reported = {
  foreground : int * int * int;
  background : int * int * int;
  palette : (int * int * int) option array;
}

(* A theme holds three styles per role, indexed by [index]: its text, a
   state glyph a word repeats, and a state glyph alone. The arrays are never
   mutated once built. *)
type t = { text : Style.t array; mark : Style.t array; alone : Style.t array }

(* The palette entry of a role's hue, whose bright variant is 8 entries
   above. *)
let slot = function
  | Error | Removed -> Some 1
  | Ok | Added -> Some 2
  | Warning -> Some 3
  | Link -> Some 4
  | Accent -> Some 6
  | Plain | Muted | Emphasis | Border -> None

let plain_text = function
  | Emphasis | Accent | Warning | Error -> Style.make ~bold:true ()
  | Link -> Style.make ~underline:true ()
  | Plain | Muted | Ok | Added | Removed | Border -> Style.default

let make f = Array.of_list (List.map f roles)
let rgb (r, g, b) = Color.of_rgb r g b

let plain =
  let text = make plain_text in
  let mark =
    make (fun role ->
        match slot role with
        | Some s -> Style.make ~fg:(Color.indexed s) ()
        | None -> plain_text role)
  in
  { text; mark; alone = text }

let default c =
  let pair = Color.contrast (rgb c.foreground) (rgb c.background) in
  let text =
    make (fun role ->
        let base = plain_text role in
        let entry =
          match role with
          | Link | Added | Removed -> Color.legible c (Option.get (slot role))
          | Muted -> (
              (* Entry 8 where it is legible and quieter than the pair. *)
              match (Color.legible c 8, Color.legible ~target:pair c 8) with
              | Some e, None -> Some e
              | _ -> None)
          | _ -> None
        in
        match entry with Some e -> Style.fg e base | None -> base)
  in
  let alone =
    make (fun role ->
        match Option.bind (slot role) (Color.legible ~target:3. c) with
        | Some e -> Style.make ~fg:e ()
        | None -> text.(index role))
  in
  let mark =
    make (fun role ->
        match slot role with
        | Some _ -> plain.mark.(index role)
        | None -> text.(index role))
  in
  { text; mark; alone }

let style t role = t.text.(index role)

let mark ?(alone = false) t role =
  (if alone then t.alone else t.mark).(index role)

let with_style role s t =
  let set a =
    let a = Array.copy a in
    a.(index role) <- s;
    a
  in
  { text = set t.text; mark = set t.mark; alone = set t.alone }

let equal a b =
  let same x y = Array.for_all2 Style.equal x y in
  same a.text b.text && same a.mark b.mark && same a.alone b.alone

let check c t =
  let pair = Color.contrast (rgb c.foreground) (rgb c.background) in
  List.filter_map
    (fun role ->
      let s = style t role in
      let target = Float.min (if role = Border then 3. else 4.5) pair in
      let or_default d = Option.value ~default:d in
      match
        Color.contrast_on c
          ~fg:(or_default Color.default_fg s.Style.fg)
          ~bg:(or_default Color.default_bg s.bg)
      with
      | Some k when k < target -> Some (role, k)
      | _ -> None)
    roles
