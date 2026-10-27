(* Props *)

type orientation = [ `Horizontal | `Vertical ]

(* [Props.make] is the one place the slider's labels get their defaults:
   [create] and [Mosaic.slider] pass their labels on unset. *)
module Props = struct
  type t = {
    orientation : orientation;
    value : float;
    min : float;
    max : float;
    viewport_size : float;
    track_color : Ansi.Color.t option;
    thumb_color : Ansi.Color.t option;
  }

  let make ?(orientation = `Horizontal) ?value ?(min = 0.) ?(max = 100.)
      ?viewport_size ?track_color ?thumb_color () =
    let viewport_size =
      match viewport_size with
      | Some v -> v
      | None -> Float.max 1. ((max -. min) *. 0.1)
    in
    let value = Option.value value ~default:min in
    { orientation; value; min; max; viewport_size; track_color; thumb_color }

  let default = make ()

  let equal a b =
    a.orientation = b.orientation
    && Float.equal a.value b.value
    && Float.equal a.min b.min && Float.equal a.max b.max
    && Float.equal a.viewport_size b.viewport_size
    && Option.equal Ansi.Color.equal a.track_color b.track_color
    && Option.equal Ansi.Color.equal a.thumb_color b.thumb_color
end

(* Types *)

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  mutable value : float;
  mutable on_change : (float -> unit) option;
  mutable dragging : bool;
  mutable drag_offset_virtual : int;
  mutable drag_pos : int;
      (* The pointer's position along the track at the last press or drag,
         whose value has been proposed. *)
}

let node t = t.node

(* Value management *)

let clamp_value ~min ~max v = Float.max min (Float.min max v)

let update_value t v =
  let clamped = clamp_value ~min:t.props.min ~max:t.props.max v in
  if not (Float.equal clamped t.value) then
    match t.on_change with Some f -> f clamped | None -> ()

(* Virtual coordinate system *)

(* Each terminal cell maps to 2 virtual units, giving us sub-cell positioning
   via Unicode half-block characters (▌▐ horizontal, ▀▄ vertical). *)

let track_dimension t =
  match t.props.orientation with
  | `Horizontal -> Renderable.width t.node
  | `Vertical -> Renderable.height t.node

let virtual_track_size t = track_dimension t * 2

(* Scrollbar-style proportional sizing: thumb = viewport / total_content.
   Guarantees at least 1 virtual unit so the thumb is always visible. *)
let virtual_thumb_size t =
  let vtrack = virtual_track_size t in
  let range = t.props.max -. t.props.min in
  if Float.equal range 0. then vtrack
  else
    let vp = Float.max 1. t.props.viewport_size in
    let content = range +. vp in
    if content <= vp then vtrack
    else
      let ratio = vp /. content in
      let calc = Float.to_int (Float.of_int vtrack *. ratio) in
      Int.max 1 (Int.min calc vtrack)

let virtual_thumb_start_for_value t value =
  let vtrack = virtual_track_size t in
  let range = t.props.max -. t.props.min in
  if Float.equal range 0. then 0
  else
    let ratio = (value -. t.props.min) /. range in
    let vthumb = virtual_thumb_size t in
    Float.to_int (Float.round (ratio *. Float.of_int (vtrack - vthumb)))

let virtual_thumb_start t = virtual_thumb_start_for_value t t.value

(* Thumb rectangle *)

type rect = { rx : int; ry : int; rw : int; rh : int }

(* Convert virtual thumb bounds back to real cell coordinates for hit-testing.
   The +1 in real_size rounds up so the rect covers all partially-occupied
   cells. *)
let thumb_rect t =
  let vthumb_size = virtual_thumb_size t in
  let vthumb_start = virtual_thumb_start t in
  let real_start = vthumb_start / 2 in
  let real_size = ((vthumb_start + vthumb_size + 1) / 2) - real_start in
  let x = Renderable.x t.node in
  let y = Renderable.y t.node in
  match t.props.orientation with
  | `Vertical ->
      {
        rx = x;
        ry = y + real_start;
        rw = Renderable.width t.node;
        rh = Int.max 1 real_size;
      }
  | `Horizontal ->
      {
        rx = x + real_start;
        ry = y;
        rw = Int.max 1 real_size;
        rh = Renderable.height t.node;
      }

(* Rendering *)

(* Pick the half-block cell for a single cell along the primary axis.
   [vthumb_start]/[vthumb_end] are in virtual (double-resolution) units. *)
let thumb_cell ~orientation ~vthumb_start ~vthumb_end real_pos =
  let vcell_start = real_pos * 2 in
  let vcell_end = vcell_start + 2 in
  let ts = Int.max vthumb_start vcell_start in
  let te = Int.min vthumb_end vcell_end in
  let coverage = te - ts in
  let code =
    if coverage >= 2 then 0x2588 (* █ *)
    else
      match orientation with
      | `Horizontal ->
          if ts = vcell_start then 0x258C (* ▌ *) else 0x2590 (* ▐ *)
      | `Vertical ->
          if coverage > 0 then
            if ts - vcell_start = 0 then 0x2580 (* ▀ *) else 0x2584 (* ▄ *)
          else 0x20 (* space — unreachable within thumb range *)
  in
  Uchar.of_int code

(* [track_line] draws a track of [width] by [height] cells as a line along
   [orientation] in [style]. *)
let track_line grid ~(style : Ansi.Style.t) ~orientation ~x ~y ~width ~height =
  let uchar =
    Uchar.of_int
      (match orientation with `Horizontal -> 0x2500 | `Vertical -> 0x2502)
  in
  let fg = Option.value style.fg ~default:Ansi.Color.default_fg in
  let bg = Option.value style.bg ~default:Ansi.Color.transparent in
  for row = y to y + height - 1 do
    for col = x to x + width - 1 do
      Matrix_grid.set_uchar ~blend:true grid ~x:col ~y:row ~uchar ~fg ~bg
        ~attrs:style.attrs ()
    done
  done

let render t _self grid ~dt:_ =
  let w = Renderable.width t.node in
  let h = Renderable.height t.node in
  if w <= 0 || h <= 0 then ()
  else
    let x = Renderable.x t.node in
    let y = Renderable.y t.node in
    let orientation = t.props.orientation in
    (* Labels first, then roles: without [track_color] the track is a line in
       the [Border] role, and without [thumb_color] the thumb's blocks are in
       the [Plain] role, so the slider shows whatever the styling. *)
    let theme = Renderable.theme t.node in
    let plain = Theme.style theme Plain in
    let thumb_color =
      match t.props.thumb_color with
      | Some c -> c
      | None -> Option.value plain.fg ~default:Ansi.Color.default_fg
    in
    let track_color =
      match t.props.track_color with
      | Some c ->
          Matrix_grid.fill_rect grid ~x ~y ~width:w ~height:h ~color:c;
          c
      | None ->
          track_line grid ~style:(Theme.style theme Border) ~orientation ~x ~y
            ~width:w ~height:h;
          Ansi.Color.transparent
    in
    let vthumb_start = virtual_thumb_start t in
    let vthumb_end = vthumb_start + virtual_thumb_size t in
    match orientation with
    | `Horizontal ->
        let real_start = Int.max 0 (vthumb_start / 2) in
        let real_end = Int.min (w - 1) (((vthumb_end + 1) / 2) - 1) in
        for real_x = real_start to real_end do
          let cell = thumb_cell ~orientation ~vthumb_start ~vthumb_end real_x in
          for row = 0 to h - 1 do
            Matrix_grid.set_uchar ~blend:true grid ~x:(x + real_x) ~y:(y + row)
              ~uchar:cell ~fg:thumb_color ~bg:track_color ~attrs:plain.attrs ()
          done
        done
    | `Vertical ->
        let real_start = Int.max 0 (vthumb_start / 2) in
        let real_end = Int.min (h - 1) (((vthumb_end + 1) / 2) - 1) in
        for real_y = real_start to real_end do
          let cell = thumb_cell ~orientation ~vthumb_start ~vthumb_end real_y in
          for col = 0 to w - 1 do
            Matrix_grid.set_uchar ~blend:true grid ~x:(x + col) ~y:(y + real_y)
              ~uchar:cell ~fg:thumb_color ~bg:track_color ~attrs:plain.attrs ()
          done
        done

(* Mouse handling *)

let mouse_pos_along_axis t ev =
  match t.props.orientation with
  | `Horizontal -> Event.Mouse.x ev
  | `Vertical -> Event.Mouse.y ev

let track_start t =
  match t.props.orientation with
  | `Horizontal -> Renderable.x t.node
  | `Vertical -> Renderable.y t.node

let calculate_drag_offset_virtual t ev =
  let origin = track_start t in
  let mouse_pos = mouse_pos_along_axis t ev - origin in
  let track_dim = track_dimension t in
  let clamped = Int.max 0 (Int.min track_dim mouse_pos) in
  let virtual_mouse = clamped * 2 in
  let vthumb_start = virtual_thumb_start t in
  let vthumb_size = virtual_thumb_size t in
  Int.max 0 (Int.min vthumb_size (virtual_mouse - vthumb_start))

(* Preserves the grab point within the thumb so dragging feels anchored to where
   the user initially clicked, rather than snapping to center. *)
let update_value_from_mouse_with_offset t ev offset_virtual =
  let origin = track_start t in
  let track_size = track_dimension t in
  let mouse_pos = mouse_pos_along_axis t ev in
  let relative = mouse_pos - origin in
  let clamped = Int.max 0 (Int.min track_size relative) in
  let virtual_mouse = clamped * 2 in
  let vthumb_size = virtual_thumb_size t in
  let vtrack = virtual_track_size t in
  let max_thumb_start = Int.max 0 (vtrack - vthumb_size) in
  let desired =
    Int.max 0 (Int.min max_thumb_start (virtual_mouse - offset_virtual))
  in
  let ratio =
    if max_thumb_start = 0 then 0.
    else Float.of_int desired /. Float.of_int max_thumb_start
  in
  let range = t.props.max -. t.props.min in
  update_value t (t.props.min +. (ratio *. range))

let point_in_rect ~px ~py r =
  px >= r.rx && px < r.rx + r.rw && py >= r.ry && py < r.ry + r.rh

let start_drag t ev =
  t.dragging <- true;
  t.drag_pos <- mouse_pos_along_axis t ev;
  t.drag_offset_virtual <- calculate_drag_offset_virtual t ev

let handle_mouse_down t ev =
  Event.Mouse.stop_propagation ev;
  Event.Mouse.prevent_default ev;
  let thumb = thumb_rect t in
  let mx = Event.Mouse.x ev in
  let my = Event.Mouse.y ev in
  (* Click on track: jump first, then start dragging. Click on thumb: start
     dragging directly. *)
  if point_in_rect ~px:mx ~py:my thumb then start_drag t ev
  else
    let size = track_dimension t in
    let relative =
      max 0 (min size (mouse_pos_along_axis t ev - track_start t))
    in
    let ratio =
      if size = 0 then 0. else float_of_int relative /. float_of_int size
    in
    let value = t.props.min +. (ratio *. (t.props.max -. t.props.min)) in
    t.dragging <- true;
    t.drag_pos <- mouse_pos_along_axis t ev;
    t.drag_offset_virtual <-
      max 0
        (min (virtual_thumb_size t)
           ((relative * 2) - virtual_thumb_start_for_value t value));
    update_value t value

let handle_mouse_drag t ev =
  if t.dragging then (
    Event.Mouse.stop_propagation ev;
    t.drag_pos <- mouse_pos_along_axis t ev;
    update_value_from_mouse_with_offset t ev t.drag_offset_virtual)

(* A release where the last press or drag left the pointer proposes nothing
   more: the press maps a track position to a value by its ratio, and the drag
   mapping would give a slightly different value for the same cell. *)
let handle_mouse_up t ev =
  if t.dragging && mouse_pos_along_axis t ev <> t.drag_pos then
    update_value_from_mouse_with_offset t ev t.drag_offset_virtual;
  t.dragging <- false

let setup_mouse t =
  Renderable.on_mouse t.node (fun ev ->
      match Event.Mouse.kind ev with
      | Down { button = Left } -> handle_mouse_down t ev
      | Drag { button = Left; _ } -> handle_mouse_drag t ev
      | Up { button = Left; _ } -> handle_mouse_up t ev
      | _ -> ())

(* Construction *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?orientation
    ?value ?min ?max ?viewport_size ?track_color ?thumb_color ?on_change () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ?orientation ?value ?min ?max ?viewport_size ?track_color
      ?thumb_color ()
  in
  let initial_value =
    clamp_value ~min:props.Props.min ~max:props.max props.value
  in
  let t =
    {
      node;
      props;
      value = initial_value;
      on_change;
      dragging = false;
      drag_offset_virtual = 0;
      drag_pos = 0;
    }
  in
  Renderable.set_render node (render t);
  setup_mouse t;
  t

(* Setters *)

let value t = t.value

let set_value t v =
  let value = clamp_value ~min:t.props.min ~max:t.props.max v in
  if not (Float.equal value t.value) then (
    t.value <- value;
    t.props <- { t.props with value };
    Renderable.request_render t.node)

let min t = t.props.min

let set_min t v =
  if not (Float.equal t.props.min v) then (
    t.props <- { t.props with min = v };
    if t.value < v then set_value t v;
    Renderable.request_render t.node)

let max t = t.props.max

let set_max t v =
  if not (Float.equal t.props.max v) then (
    t.props <- { t.props with max = v };
    if t.value > v then set_value t v;
    Renderable.request_render t.node)

let set_orientation t o =
  if t.props.orientation <> o then (
    t.props <- { t.props with orientation = o };
    Renderable.request_render t.node)

let set_viewport_size t v =
  let clamped = Float.max 0.01 (Float.min v (t.props.max -. t.props.min)) in
  if not (Float.equal t.props.viewport_size clamped) then (
    t.props <- { t.props with viewport_size = clamped };
    Renderable.request_render t.node)

let set_track_color t c =
  if not (Option.equal Ansi.Color.equal t.props.track_color (Some c)) then (
    t.props <- { t.props with track_color = Some c };
    Renderable.request_render t.node)

let set_thumb_color t c =
  if not (Option.equal Ansi.Color.equal t.props.thumb_color (Some c)) then (
    t.props <- { t.props with thumb_color = Some c };
    Renderable.request_render t.node)

let set_on_change t f = t.on_change <- f

(* Apply props *)

let apply_props t (props : Props.t) =
  t.props <- props;
  t.value <- clamp_value ~min:props.min ~max:props.max props.value;
  Renderable.request_render t.node

(* Pretty-printing *)

let pp ppf t =
  let orient =
    match t.props.orientation with `Horizontal -> "h" | `Vertical -> "v"
  in
  Format.fprintf ppf "Slider(%s, %s, %.1f/[%.1f..%.1f])" (Renderable.id t.node)
    orient t.value t.props.min t.props.max
