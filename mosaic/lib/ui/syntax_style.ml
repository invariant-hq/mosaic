type t = { base : Ansi.Style.t; overlays : (string, Ansi.Style.t) Hashtbl.t }

let make ~base mappings =
  let overlays = Hashtbl.create (List.length mappings) in
  List.iter
    (fun (group, style) -> Hashtbl.replace overlays group style)
    mappings;
  { base; overlays }

let rec resolve_overlay t group =
  match Hashtbl.find_opt t.overlays group with
  | Some overlay -> overlay
  | None -> (
      match String.rindex_opt group '.' with
      | Some i -> resolve_overlay t (String.sub group 0 i)
      | None -> Ansi.Style.default)

let resolve t group =
  Ansi.Style.merge ~base:t.base ~overlay:(resolve_overlay t group)

let default =
  let base = Ansi.Style.default in
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
      ("tag", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 224 108 117) ());
      ("attribute", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 229 192 123) ());
    ]

let base t = t.base
