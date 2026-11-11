(* ───── Frame Sets ───── *)

type frame_set = Glyphs.spinner = { frames : string array; period : float }

let dots = Glyphs.unicode.spinner
let line = Glyphs.ascii.spinner

let dots2 =
  { frames = [| "⣾"; "⣽"; "⣻"; "⢿"; "⡿"; "⣟"; "⣯"; "⣷" |]; period = 0.08 }

let arc = { frames = [| "◜"; "◠"; "◝"; "◞"; "◡"; "◟" |]; period = 0.1 }

let bounce = { frames = [| "⠁"; "⠂"; "⠄"; "⠂" |]; period = 0.12 }
let circle = { frames = [| "◡"; "⊙"; "◠" |]; period = 0.12 }

let equal_frame_set a b =
  Float.equal a.period b.period
  &&
  let la = Array.length a.frames and lb = Array.length b.frames in
  la = lb
  &&
  let rec loop i =
    i >= la || (String.equal a.frames.(i) b.frames.(i) && loop (i + 1))
  in
  loop 0

(* ───── Props ───── *)

(* [Props.make] is the one place the spinner's labels get their defaults:
   [Mosaic.spinner] passes its labels on unset. *)
module Props = struct
  type t = { frame_set : frame_set option; color : Ansi.Color.t option }

  let make ?frame_set ?color () = { frame_set; color }
  let default = make ()

  let equal a b =
    Option.equal equal_frame_set a.frame_set b.frame_set
    && Option.equal Ansi.Color.equal a.color b.color
end

(* ───── Types ───── *)

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  mutable frame_index : int;
  mutable elapsed : float;
  mutable max_width : (Matrix.Text.width_method * int) option;
      (* The widest frame, by the screen's width method when it was measured. *)
}

let node t = t.node

(* Without a frame set of its own the spinner draws the theme's, read where
   it is drawn so that a new theme needs no other work. *)
let frame_set t =
  match t.props.frame_set with
  | Some fs -> fs
  | None -> (Theme.glyphs (Renderable.theme t.node)).spinner

let frames t = (frame_set t).frames
let period t = (frame_set t).period

(* ───── Display Width ───── *)

let compute_max_width ~width_method frames =
  Array.fold_left
    (fun acc frame ->
      Int.max acc (Matrix.Text.measure ~width_method ~tab_width:2 frame))
    0 frames

(* The theme's frames are glyphs drawn in one cell ({!Glyphs.t.spinner}),
   so a new theme changes no width. *)
let max_width t =
  match t.props.frame_set with
  | None -> 1
  | Some fs -> (
      let width_method = Renderable.width_method t.node in
      match t.max_width with
      | Some (cached, width) when cached = width_method -> width
      | Some _ | None ->
          let width = compute_max_width ~width_method fs.frames in
          t.max_width <- Some (width_method, width);
          width)

(* ───── Measure ───── *)

let measure t ~known_dimensions ~available_space:_ ~style:_ =
  Toffee.Geometry.Size.
    {
      width =
        (match known_dimensions.width with
        | Some w -> w
        | None -> Float.of_int (Int.max 1 (max_width t)));
      height = (match known_dimensions.height with Some h -> h | None -> 1.);
    }

(* ───── Rendering ───── *)

let render t _self grid ~dt:_ =
  let w = Renderable.width t.node in
  let h = Renderable.height t.node in
  if w > 0 && h > 0 then
    let frames = frames t in
    let n = Array.length frames in
    if n > 0 then
      let frame = frames.(t.frame_index mod n) in
      (* [color] first, then the [Plain] role. *)
      let style =
        let plain = Theme.style (Renderable.theme t.node) Plain in
        match t.props.color with
        | Some c -> Ansi.Style.fg c plain
        | None -> plain
      in
      Matrix_grid.draw_text ~style grid ~x:(Renderable.x t.node)
        ~y:(Renderable.y t.node) ~text:frame

(* ───── Animation ───── *)

(* The animation invariant belongs to the widget: the node is live exactly
   while it animates, so on_frame ticks without callers wiring liveness
   themselves. A single frame or non-positive period renders a static glyph
   and needs no per-frame work. The theme's frames always animate: the
   theme in scope can change without the spinner knowing. *)
let animates t =
  match t.props.frame_set with
  | None -> true
  | Some fs -> fs.period > 0. && Array.length fs.frames > 1

let update_live t = Renderable.set_live t.node (animates t)

let on_frame t _node ~dt =
  let period = period t in
  if period > 0. then (
    t.elapsed <- t.elapsed +. dt;
    if t.elapsed >= period then
      let n = Array.length (frames t) in
      if n > 0 then (
        let advance = int_of_float (t.elapsed /. period) in
        t.frame_index <- (t.frame_index + advance) mod n;
        t.elapsed <- Float.rem t.elapsed period;
        Renderable.request_render t.node))

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity props =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  (* It acts on a press, or paints no source text: a press on it neither
     starts nor clears a text selection. *)
  Renderable.set_selectable node (Some `Off);
  let t = { node; props; frame_index = 0; elapsed = 0.; max_width = None } in
  Renderable.set_render node (render t);
  Renderable.set_measure node (Some (measure t));
  Renderable.set_on_frame node (Some (on_frame t));
  update_live t;
  t

(* ───── Accessors ───── *)

(* The theme in scope may have changed to fewer frames since the last
   advance. *)
let frame_index t =
  let n = Array.length (frames t) in
  if n > 0 then t.frame_index mod n else 0

let elapsed t = t.elapsed

(* ───── Setters ───── *)

(* A new frame set starts its period afresh, at the frame of the same index
   wrapped to its count. *)
let restart t =
  let n = Array.length (frames t) in
  t.frame_index <- (if n > 0 then t.frame_index mod n else 0);
  t.elapsed <- 0.

let set_frame_set t fs =
  t.props <- { t.props with frame_set = fs };
  restart t;
  t.max_width <- None;
  Renderable.set_measure t.node (Some (measure t));
  update_live t;
  Renderable.request_render t.node

let set_color t c =
  if not (Option.equal Ansi.Color.equal t.props.color (Some c)) then (
    t.props <- { t.props with color = Some c };
    Renderable.request_render t.node)

(* ───── Apply Props ───── *)

let apply_props t (props : Props.t) =
  let reset_animation =
    not (Option.equal equal_frame_set t.props.frame_set props.frame_set)
  in
  t.props <- props;
  if reset_animation then restart t;
  t.max_width <- None;
  Renderable.set_measure t.node (Some (measure t));
  update_live t;
  Renderable.request_render t.node

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "Spinner(%s, frame=%d/%d)" (Renderable.id t.node)
    (frame_index t)
    (Array.length (frames t))
