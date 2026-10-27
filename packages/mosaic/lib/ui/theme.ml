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
  | Canvas
  | Panel
  | Field
  | Cursor
  | Selected
  | Highlight

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
    Canvas;
    Panel;
    Field;
    Cursor;
    Selected;
    Highlight;
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
  | Canvas -> 11
  | Panel -> 12
  | Field -> 13
  | Cursor -> 14
  | Selected -> 15
  | Highlight -> 16

(* A theme holds three styles per role, indexed by [index]: its text, a
   state glyph a word repeats, and a state glyph alone. The arrays are never
   mutated once built. *)
type t = {
  text : Ansi.Style.t array;
  mark : Ansi.Style.t array;
  alone : Ansi.Style.t array;
  glyphs : Glyphs.t;
  syntax : Syntax_style.t;
}

(* The palette entry of a role's hue, whose bright variant is 8 entries
   above. *)
let slot = function
  | Error | Removed -> Some 1
  | Ok | Added -> Some 2
  | Warning -> Some 3
  | Link -> Some 4
  | Accent -> Some 6
  | Plain | Muted | Emphasis | Border | Canvas | Panel | Field | Cursor
  | Selected | Highlight ->
      None

let plain_text = function
  | Emphasis | Accent | Warning | Error -> Ansi.Style.make ~bold:true ()
  | Link -> Ansi.Style.make ~underline:true ()
  | Cursor | Highlight -> Ansi.Style.make ~inverse:true ()
  | Selected -> Ansi.Style.make ~bold:true ~underline:true ()
  | Plain | Muted | Ok | Added | Removed | Border | Canvas | Panel | Field ->
      Ansi.Style.default

let make f = Array.of_list (List.map f roles)

let plain =
  let text = make plain_text in
  let mark =
    make (fun role ->
        match slot role with
        | Some s -> Ansi.Style.make ~fg:(Ansi.Color.indexed s) ()
        | None -> plain_text role)
  in
  {
    text;
    mark;
    alone = text;
    glyphs = Glyphs.unicode;
    syntax = Syntax_style.plain;
  }

let pair (p : Ansi.Palette.t) =
  let rgb (r, g, b) = Ansi.Color.of_rgb r g b in
  Ansi.Color.contrast (rgb p.foreground) (rgb p.background)

let default p =
  let text =
    make (fun role ->
        let base = plain_text role in
        let entry =
          match role with
          | Link | Added | Removed ->
              Ansi.Palette.legible p (Option.get (slot role))
          | Muted -> (
              (* Entry 8 where it is legible and quieter than the pair. *)
              match
                ( Ansi.Palette.legible p 8,
                  Ansi.Palette.legible ~target:(pair p) p 8 )
              with
              | Some e, None -> Some e
              | _ -> None)
          | _ -> None
        in
        match entry with Some e -> Ansi.Style.fg e base | None -> base)
  in
  let alone =
    make (fun role ->
        match Option.bind (slot role) (Ansi.Palette.legible ~target:3. p) with
        | Some e -> Ansi.Style.make ~fg:e ()
        | None -> text.(index role))
  in
  let mark =
    make (fun role ->
        match slot role with
        | Some _ -> plain.mark.(index role)
        | None -> text.(index role))
  in
  {
    text;
    mark;
    alone;
    glyphs = Glyphs.unicode;
    syntax = Syntax_style.of_palette p;
  }

let style t role = t.text.(index role)

let mark ?(alone = false) t role =
  (if alone then t.alone else t.mark).(index role)

let glyphs t = t.glyphs
let syntax t = t.syntax

let with_style role s t =
  let set a =
    let a = Array.copy a in
    a.(index role) <- s;
    a
  in
  { t with text = set t.text; mark = set t.mark; alone = set t.alone }

let with_glyphs glyphs t = { t with glyphs }
let with_syntax syntax t = { t with syntax }

let check p t =
  let pair = pair p in
  List.filter_map
    (fun role ->
      let s = style t role in
      let target = Float.min (if role = Border then 3. else 4.5) pair in
      let or_default d = Option.value ~default:d in
      match
        Ansi.Palette.contrast p
          ~fg:(or_default Ansi.Color.default_fg s.Ansi.Style.fg)
          ~bg:(or_default Ansi.Color.default_bg s.bg)
      with
      | Some k when k < target -> Some (role, k)
      | _ -> None)
    roles

let equal a b =
  let same x y = Array.for_all2 Ansi.Style.equal x y in
  same a.text b.text && same a.mark b.mark && same a.alone b.alone
  && Glyphs.equal a.glyphs b.glyphs
  && Syntax_style.equal a.syntax b.syntax
