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

type colors = {
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

let plain =
  let text = make plain_text in
  let mark =
    make (fun role ->
        match slot role with
        | Some s -> Style.make ~fg:(Color.indexed s) ()
        | None -> plain_text role)
  in
  { text; mark; alone = text }

let rgb (r, g, b) = Color.of_rgb r g b
let contrast a b = Color.contrast (rgb a) (rgb b)

let entry c i =
  if i < Array.length c.palette && i < 16 then c.palette.(i) else None

(* The first of the entries [s] and [s + 8] that [c] reports and whose
   contrast against the background reaches [target]. *)
let reaching c ~target s =
  List.find_opt
    (fun i ->
      match entry c i with
      | Some e -> contrast e c.background >= target
      | None -> false)
    [ s; s + 8 ]

let default c =
  let pair = contrast c.foreground c.background in
  let target = Float.min 4.5 pair in
  let text =
    make (fun role ->
        let base = plain_text role in
        match role with
        | Link | Added | Removed -> (
            match reaching c ~target (Option.get (slot role)) with
            | Some i -> Style.fg (Color.indexed i) base
            | None -> base)
        | Muted -> (
            match entry c 8 with
            | Some e ->
                let k = contrast e c.background in
                if k >= target && k < pair then Style.fg (Color.indexed 8) base
                else base
            | None -> base)
        | _ -> base)
  in
  let alone =
    make (fun role ->
        match Option.bind (slot role) (reaching c ~target:3.) with
        | Some i -> Style.make ~fg:(Color.indexed i) ()
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

(* The RGB a color shows on a terminal of colors [c], [None] for an entry
   of 0 to 15 that [c] does not report. Entries 16 to 255 are the same on
   every terminal that has them, and an RGB color is itself. *)
let shown c ~default color =
  match color with
  | None -> Some default
  | Some color -> (
      match Color.intent color with
      | Default -> Some default
      | Indexed i when i < 16 -> entry c i
      | Indexed _ | Rgb -> Some (Color.to_rgb color))

let check c t =
  let pair = contrast c.foreground c.background in
  List.filter_map
    (fun role ->
      let s = style t role in
      let target = Float.min (if role = Border then 3. else 4.5) pair in
      match
        ( shown c ~default:c.foreground s.Style.fg,
          shown c ~default:c.background s.Style.bg )
      with
      | Some fg, Some bg ->
          let k = contrast fg bg in
          if k < target then Some (role, k) else None
      | _ -> None)
    roles
