type surface = Canvas | Panel | Field | Cursor | Selected | Highlight

let surfaces = [ Canvas; Panel; Field; Cursor; Selected; Highlight ]

let index = function
  | Canvas -> 0
  | Panel -> 1
  | Field -> 2
  | Cursor -> 3
  | Selected -> 4
  | Highlight -> 5

(* The surfaces' styles are indexed by [index]; the array is never mutated
   once built. *)
type t = {
  text : Ansi.Theme.t;
  surfaces : Ansi.Style.t array;
  glyphs : Glyphs.t;
}

let plain_surface = function
  | Canvas | Panel | Field -> Ansi.Style.default
  | Cursor | Highlight -> Ansi.Style.make ~inverse:true ()
  | Selected -> Ansi.Style.make ~bold:true ~underline:true ()

let plain =
  {
    text = Ansi.Theme.plain;
    surfaces = Array.of_list (List.map plain_surface surfaces);
    glyphs = Glyphs.unicode;
  }

let default colors = { plain with text = Ansi.Theme.default colors }
let text t = t.text
let style t role = Ansi.Theme.style t.text role
let mark ?alone t role = Ansi.Theme.mark ?alone t.text role
let surface t s = t.surfaces.(index s)
let glyphs t = t.glyphs
let with_text text t = { t with text }
let with_style role s t = { t with text = Ansi.Theme.with_style role s t.text }

let with_surface s style t =
  let surfaces = Array.copy t.surfaces in
  surfaces.(index s) <- style;
  { t with surfaces }

let with_glyphs glyphs t = { t with glyphs }

let equal a b =
  Ansi.Theme.equal a.text b.text
  && Array.for_all2 Ansi.Style.equal a.surfaces b.surfaces
  && Glyphs.equal a.glyphs b.glyphs
