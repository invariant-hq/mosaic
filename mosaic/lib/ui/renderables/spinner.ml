(* ───── Frame Sets ───── *)

type frame_set = { frames : string array; interval : float }

let dots =
  {
    frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |];
    interval = 80.;
  }

let line = { frames = [| "-"; "\\"; "|"; "/" |]; interval = 130. }

let dots2 =
  { frames = [| "⣾"; "⣽"; "⣻"; "⢿"; "⡿"; "⣟"; "⣯"; "⣷" |]; interval = 80. }

let arc = { frames = [| "◜"; "◠"; "◝"; "◞"; "◡"; "◟" |]; interval = 100. }

let bounce = { frames = [| "⠁"; "⠂"; "⠄"; "⠂" |]; interval = 120. }
let circle = { frames = [| "◡"; "⊙"; "◠" |]; interval = 120. }
let default_frame_set = dots

(* ───── Props ───── *)

module Props = struct
  type t = { frame_set : frame_set; color : Ansi.Color.t }

  let make ?(frame_set = default_frame_set) ?(color = Ansi.Color.white) () =
    { frame_set; color }

  let default = make ()

  let frames_equal a b =
    let la = Array.length a and lb = Array.length b in
    la = lb
    &&
    let rec loop i = i >= la || (String.equal a.(i) b.(i) && loop (i + 1)) in
    loop 0

  let equal a b =
    Float.equal a.frame_set.interval b.frame_set.interval
    && frames_equal a.frame_set.frames b.frame_set.frames
    && Ansi.Color.equal a.color b.color
end

(* ───── Types ───── *)

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  mutable frame_index : int;
  mutable elapsed : float;
  mutable max_width : int;
}

let node t = t.node

(* ───── Display Width ───── *)

let compute_max_width frames =
  Array.fold_left
    (fun acc frame ->
      Int.max acc
        (Matrix.Text.measure ~width_method:`Unicode ~tab_width:2 frame))
    0 frames

(* ───── Measure ───── *)

let measure t ~known_dimensions ~available_space:_ ~style:_ =
  Toffee.Geometry.Size.
    {
      width =
        (match known_dimensions.width with
        | Some w -> w
        | None -> Float.of_int (Int.max 1 t.max_width));
      height = (match known_dimensions.height with Some h -> h | None -> 1.);
    }

(* ───── Rendering ───── *)

let render t _self grid ~delta:_ =
  let w = Renderable.width t.node in
  let h = Renderable.height t.node in
  if w > 0 && h > 0 then
    let n = Array.length t.props.frame_set.frames in
    if n > 0 then
      let frame = t.props.frame_set.frames.(t.frame_index) in
      let style = Ansi.Style.make ~fg:t.props.color () in
      Grid.draw_text ~style grid ~x:(Renderable.x t.node)
        ~y:(Renderable.y t.node) ~text:frame

(* ───── Animation ───── *)

let on_frame t _node ~delta =
  let interval = t.props.frame_set.interval in
  if interval > 0. then (
    let delta_ms = delta *. 1000. in
    t.elapsed <- t.elapsed +. delta_ms;
    if t.elapsed >= interval then
      let n = Array.length t.props.frame_set.frames in
      if n > 0 then (
        let advance = int_of_float (t.elapsed /. interval) in
        t.frame_index <- (t.frame_index + advance) mod n;
        t.elapsed <- Float.rem t.elapsed interval;
        Renderable.request_render t.node))

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity
    ?(frame_set = default_frame_set) ?(color = Ansi.Color.white) () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props = Props.make ~frame_set ~color () in
  let max_width = compute_max_width frame_set.frames in
  let t = { node; props; frame_index = 0; elapsed = 0.; max_width } in
  Renderable.set_render node (render t);
  Renderable.set_measure node (Some (measure t));
  Renderable.set_on_frame node (Some (on_frame t));
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
  t.max_width <- compute_max_width fs.frames;
  Renderable.set_measure t.node (Some (measure t));
  Renderable.request_render t.node

let set_color t c =
  if not (Ansi.Color.equal t.props.color c) then (
    t.props <- { t.props with color = c };
    Renderable.request_render t.node)

(* ───── Apply Props ───── *)

let apply_props t (props : Props.t) =
  let reset_animation =
    (not (Float.equal t.props.frame_set.interval props.frame_set.interval))
    || not (Props.frames_equal t.props.frame_set.frames props.frame_set.frames)
  in
  t.props <- props;
  if reset_animation then (
    let n = Array.length props.frame_set.frames in
    t.frame_index <- (if n > 0 then t.frame_index mod n else 0);
    t.elapsed <- 0.);
  t.max_width <- compute_max_width props.frame_set.frames;
  Renderable.set_measure t.node (Some (measure t));
  Renderable.request_render t.node

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "Spinner(%s, frame=%d/%d)" (Renderable.id t.node)
    t.frame_index
    (Array.length t.props.frame_set.frames)
