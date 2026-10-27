module Scopes = Map.Make (String)

type t = { base : Ansi.Style.t; overlays : Ansi.Style.t Scopes.t }

let make ~base mappings =
  let overlays =
    List.fold_left
      (fun overlays (group, style) -> Scopes.add group style overlays)
      Scopes.empty mappings
  in
  { base; overlays }

let equal a b =
  Ansi.Style.equal a.base b.base
  && Scopes.equal Ansi.Style.equal a.overlays b.overlays

let rec resolve_overlay t group =
  match Scopes.find_opt group t.overlays with
  | Some overlay -> overlay
  | None -> (
      match String.rindex_opt group '.' with
      | Some i -> resolve_overlay t (String.sub group 0 i)
      | None -> Ansi.Style.default)

let resolve t group =
  Ansi.Style.merge ~base:t.base ~overlay:(resolve_overlay t group)

let default =
  let base = Ansi.Style.default in
  let property = Ansi.Style.make ~fg:(Ansi.Color.of_rgb 224 108 117) () in
  make ~base
    [
      ( "comment",
        Ansi.Style.make ~italic:true ~fg:(Ansi.Color.grayscale ~level:12) () );
      ( "keyword",
        Ansi.Style.make ~bold:true ~fg:(Ansi.Color.of_rgb 255 151 0) () );
      ("string", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 229 192 123) ());
      ("number", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 209 154 102) ());
      ("function", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 97 175 239) ());
      ("type", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 86 182 194) ());
      ("variable", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 224 108 117) ());
      ("operator", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 255 151 0) ());
      ("punctuation", Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ());
      ("constant", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 209 154 102) ());
      ("module", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 86 182 194) ());
      ("label", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 224 108 117) ());
      ("embedded", Ansi.Style.default);
      ("tag", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 224 108 117) ());
      ("attribute", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 229 192 123) ());
      ("property", property);
      ("variable.member", property);
      ("constructor", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 209 154 102) ());
    ]

(* The scopes [plain] and [of_colors] style, each with the palette entry of
   its hue and the style it keeps where no entry reaches its target: the
   attributes a scope keeps change no color. *)
let hued =
  [
    ("keyword", 5, Ansi.Style.make ~bold:true ());
    ("tag", 5, Ansi.Style.default);
    ("string", 2, Ansi.Style.default);
    ("attribute", 2, Ansi.Style.default);
    ("number", 3, Ansi.Style.default);
    ("constant", 3, Ansi.Style.default);
    ("constructor", 3, Ansi.Style.default);
    ("type", 3, Ansi.Style.default);
    ("module", 3, Ansi.Style.default);
    ("function", 4, Ansi.Style.default);
  ]

let comment = Ansi.Style.make ~italic:true ()

let plain =
  make ~base:Ansi.Style.default
    (("comment", comment)
    :: List.map (fun (scope, _, attributes) -> (scope, attributes)) hued)

let rgb (r, g, b) = Ansi.Color.of_rgb r g b

(* The entries of a scope's hue and of a comment are chosen as those of
   [Ansi.Theme.default]'s text are: a legible entry, and for a comment entry
   8 where it is legible and quieter than the pair, as [Muted] text. *)
let of_colors (c : Ansi.Color.reported) =
  let pair = Ansi.Color.contrast (rgb c.foreground) (rgb c.background) in
  let comment =
    match (Ansi.Color.legible c 8, Ansi.Color.legible ~target:pair c 8) with
    | Some e, None -> Ansi.Style.fg e comment
    | _ -> comment
  in
  make ~base:Ansi.Style.default
    (("comment", comment)
    :: List.map
         (fun (scope, s, attributes) ->
           match Ansi.Color.legible c s with
           | Some e -> (scope, Ansi.Style.make ~fg:e ())
           | None -> (scope, attributes))
         hued)

let base t = t.base
