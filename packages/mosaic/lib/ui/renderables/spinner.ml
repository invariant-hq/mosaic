(* ───── Frame Sets ───── *)

type frame_set = { frames : string array; period : float }

let dots =
  {
    frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |];
    period = 0.08;
  }

let line = { frames = [| "-"; "\\"; "|"; "/" |]; period = 0.13 }

let dots2 =
  { frames = [| "⣾"; "⣽"; "⣻"; "⢿"; "⡿"; "⣟"; "⣯"; "⣷" |]; period = 0.08 }

let arc = { frames = [| "◜"; "◠"; "◝"; "◞"; "◡"; "◟" |]; period = 0.1 }

let bounce = { frames = [| "⠁"; "⠂"; "⠄"; "⠂" |]; period = 0.12 }
let circle = { frames = [| "◡"; "⊙"; "◠" |]; period = 0.12 }
let default_frame_set = dots

(* ───── Props ───── *)

(* [Props.make] is the one place the spinner's labels get their defaults:
   [Mosaic.spinner] passes its labels on unset. *)
module Props = struct
  type t = { frame_set : frame_set; color : Ansi.Color.t option }

  let make ?(frame_set = default_frame_set) ?color () = { frame_set; color }
  let default = make ()

  let frames_equal a b =
    let la = Array.length a and lb = Array.length b in
    la = lb
    &&
    let rec loop i = i >= la || (String.equal a.(i) b.(i) && loop (i + 1)) in
    loop 0

  let equal a b =
    Float.equal a.frame_set.period b.frame_set.period
    && frames_equal a.frame_set.frames b.frame_set.frames
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

(* ───── Display Width ───── *)

let compute_max_width ~width_method frames =
  Array.fold_left
    (fun acc frame ->
      Int.max acc (Matrix.Text.measure ~width_method ~tab_width:2 frame))
    0 frames

let max_width t =
  let width_method = Renderable.width_method t.node in
  match t.max_width with
  | Some (cached, width) when cached = width_method -> width
  | Some _ | None ->
      let width = compute_max_width ~width_method t.props.frame_set.frames in
      t.max_width <- Some (width_method, width);
      width

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
    let n = Array.length t.props.frame_set.frames in
    if n > 0 then
      let frame = t.props.frame_set.frames.(t.frame_index) in
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
   while the frame set animates, so on_frame ticks without callers wiring
   liveness themselves. A single frame or non-positive period renders a
   static glyph and needs no per-frame work. *)
let animates t =
  let fs = t.props.frame_set in
  fs.period > 0. && Array.length fs.frames > 1

let update_live t = Renderable.set_live t.node (animates t)

let on_frame t _node ~dt =
  let period = t.props.frame_set.period in
  if period > 0. then (
    t.elapsed <- t.elapsed +. dt;
    if t.elapsed >= period then
      let n = Array.length t.props.frame_set.frames in
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
  let t = { node; props; frame_index = 0; elapsed = 0.; max_width = None } in
  Renderable.set_render node (render t);
  Renderable.set_measure node (Some (measure t));
  Renderable.set_on_frame node (Some (on_frame t));
  update_live t;
  t

(* ───── Accessors ───── *)

let frame_index t = t.frame_index
let elapsed t = t.elapsed

(* ───── Setters ───── *)

let set_frame_set t fs =
  t.props <- { t.props with frame_set = fs };
  let n = Array.length fs.frames in
  t.frame_index <- (if n > 0 then t.frame_index mod n else 0);
  t.elapsed <- 0.;
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
    (not (Float.equal t.props.frame_set.period props.frame_set.period))
    || not (Props.frames_equal t.props.frame_set.frames props.frame_set.frames)
  in
  t.props <- props;
  if reset_animation then (
    let n = Array.length props.frame_set.frames in
    t.frame_index <- (if n > 0 then t.frame_index mod n else 0);
    t.elapsed <- 0.);
  t.max_width <- None;
  Renderable.set_measure t.node (Some (measure t));
  update_live t;
  Renderable.request_render t.node

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "Spinner(%s, frame=%d/%d)" (Renderable.id t.node)
    t.frame_index
    (Array.length t.props.frame_set.frames)
