(* ───── Types ───── *)

type orientation = [ `Vertical | `Horizontal ]
type scroll_unit = [ `Absolute | `Viewport | `Content | `Step ]

(* ───── Arrow ───── *)

module Arrow = struct
  type direction = Up | Down | Left | Right

  let arrow_char = function
    | Up -> "\xe2\x96\xb2" (* ▲ *)
    | Down -> "\xe2\x96\xbc" (* ▼ *)
    | Left -> "\xe2\x97\x80" (* ◀ *)
    | Right -> "\xe2\x96\xb6" (* ▶ *)

  type t = {
    node : Renderable.t;
    direction : direction;
    fg : Ansi.Color.t;
    bg : Ansi.Color.t;
  }

  let node t = t.node

  let render t _self grid ~delta:_ =
    let x = Renderable.x t.node in
    let y = Renderable.y t.node in
    let text = arrow_char t.direction in
    let style = Ansi.Style.make ~fg:t.fg ~bg:t.bg () in
    Grid.draw_text grid ~x ~y ~text ~style

  let measure t ~known_dimensions:_ ~available_space:_ ~style:_ =
    let text = arrow_char t.direction in
    let width =
      float_of_int
        (max 1 (Glyph.String.measure ~width_method:`Unicode ~tab_width:2 text))
    in
    Toffee.Geometry.Size.make width 1.

  let create ~parent ~direction ?(fg = Ansi.Color.White)
      ?(bg = Ansi.Color.default) ~on_click () =
    let node = Renderable.create ~parent () in
    let t = { node; direction; fg; bg } in
    Renderable.set_render node (render t);
    Renderable.set_measure node (Some (measure t));
    let style =
      Renderable.style node
      |> Toffee.Style.set_align_self (Some Toffee.Style.Align_items.Center)
      |> Toffee.Style.set_size
           {
             width = Toffee.Style.Dimension.auto;
             height = Toffee.Style.Dimension.length 1.;
           }
    in
    Renderable.set_style node style;
    Renderable.on_mouse node (fun ev ->
        match Event.Mouse.kind ev with
        | Down { button = Left } ->
            on_click ();
            Event.Mouse.prevent_default ev;
            Event.Mouse.stop_propagation ev
        | _ -> ());
    t
end

(* ───── Props ───── *)

let default_track_color = Ansi.Color.of_rgb 37 37 39
let default_thumb_color = Ansi.Color.of_rgb 154 158 163

module Props = struct
  type t = {
    orientation : orientation;
    show_arrows : bool;
    track_color : Ansi.Color.t;
    thumb_color : Ansi.Color.t;
    arrow_fg : Ansi.Color.t;
    arrow_bg : Ansi.Color.t;
  }

  let make ?(orientation = `Vertical) ?(show_arrows = false)
      ?(track_color = default_track_color) ?(thumb_color = default_thumb_color)
      ?(arrow_fg = Ansi.Color.White) ?(arrow_bg = Ansi.Color.default) () =
    { orientation; show_arrows; track_color; thumb_color; arrow_fg; arrow_bg }

  let default = make ()

  let equal a b =
    a.orientation = b.orientation
    && a.show_arrows = b.show_arrows
    && Ansi.Color.equal a.track_color b.track_color
    && Ansi.Color.equal a.thumb_color b.thumb_color
    && Ansi.Color.equal a.arrow_fg b.arrow_fg
    && Ansi.Color.equal a.arrow_bg b.arrow_bg
end

(* ───── Scroll Bar ───── *)

type t = {
  node : Renderable.t;
  slider : Slider.t;
  start_arrow : Arrow.t;
  end_arrow : Arrow.t;
  mutable props : Props.t;
  mutable scroll_position : int;
  mutable scroll_size : int;
  mutable viewport_size : int;
  mutable scroll_step : int option;
  mutable manual_visibility : bool;
  mutable on_change : (int -> unit) option;
  mutable syncing_from_slider : bool;
  (* Arrow repeat state *)
  mutable repeat_active : bool;
  mutable repeat_is_end : bool;
  mutable repeat_delay : float;
  mutable repeat_elapsed : float;
}

let node t = t.node
let clamp v ~lo ~hi = max lo (min hi v)
let range t = max 0 (t.scroll_size - t.viewport_size)

(* ───── Slider Sync ───── *)

let sync_slider t =
  let scroll_range = range t in
  Slider.set_min t.slider 0.;
  Slider.set_max t.slider (float_of_int scroll_range);
  Slider.set_viewport_size t.slider (float_of_int (max 1 t.viewport_size));
  if not t.syncing_from_slider then (
    t.syncing_from_slider <- true;
    Slider.set_value t.slider
      (float_of_int (min t.scroll_position scroll_range));
    t.syncing_from_slider <- false)

(* ───── Visibility ───── *)

let recalc_visibility t =
  if not t.manual_visibility then Renderable.set_visible t.node (range t > 0)

(* ───── Position ───── *)

let set_scroll_position_internal t ?(emit = true) value =
  let clamped = clamp value ~lo:0 ~hi:(range t) in
  if clamped <> t.scroll_position then (
    t.scroll_position <- clamped;
    if not t.syncing_from_slider then (
      t.syncing_from_slider <- true;
      Slider.set_value t.slider (float_of_int clamped);
      t.syncing_from_slider <- false);
    (if emit then match t.on_change with Some f -> f clamped | None -> ());
    Renderable.request_render t.node)

let set_scroll_position t value = set_scroll_position_internal t value

let scroll_by t delta ~unit =
  let multiplier =
    match unit with
    | `Absolute -> 1.
    | `Viewport -> float_of_int (max 1 t.viewport_size)
    | `Content -> float_of_int (max 1 t.scroll_size)
    | `Step ->
        float_of_int
          (match t.scroll_step with Some s when s > 0 -> s | _ -> 1)
  in
  let target =
    int_of_float
      (Float.round (float_of_int t.scroll_position +. (delta *. multiplier)))
  in
  set_scroll_position t target

(* ───── Arrow Repeat ───── *)

let stop_repeat t =
  t.repeat_active <- false;
  Renderable.set_live t.node false

let start_repeat t ~is_end =
  t.repeat_active <- true;
  t.repeat_is_end <- is_end;
  t.repeat_delay <- 500.;
  t.repeat_elapsed <- 0.;
  Renderable.set_live t.node true

(* ───── Keyboard ───── *)

let handle_key t (event : Event.key) =
  let kev = Event.Key.data event in
  match kev.key with
  | Left when t.props.orientation = `Horizontal ->
      scroll_by t (-0.2) ~unit:`Viewport;
      true
  | Right when t.props.orientation = `Horizontal ->
      scroll_by t 0.2 ~unit:`Viewport;
      true
  | Up when t.props.orientation = `Vertical ->
      scroll_by t (-0.2) ~unit:`Viewport;
      true
  | Down when t.props.orientation = `Vertical ->
      scroll_by t 0.2 ~unit:`Viewport;
      true
  | Char c
    when t.props.orientation = `Horizontal && Uchar.equal c (Uchar.of_char 'h')
    ->
      scroll_by t (-0.2) ~unit:`Viewport;
      true
  | Char c
    when t.props.orientation = `Horizontal && Uchar.equal c (Uchar.of_char 'l')
    ->
      scroll_by t 0.2 ~unit:`Viewport;
      true
  | Char c
    when t.props.orientation = `Vertical && Uchar.equal c (Uchar.of_char 'k') ->
      scroll_by t (-0.2) ~unit:`Viewport;
      true
  | Char c
    when t.props.orientation = `Vertical && Uchar.equal c (Uchar.of_char 'j') ->
      scroll_by t 0.2 ~unit:`Viewport;
      true
  | Page_up ->
      scroll_by t (-0.5) ~unit:`Viewport;
      true
  | Page_down ->
      scroll_by t 0.5 ~unit:`Viewport;
      true
  | Home ->
      scroll_by t (-1.) ~unit:`Content;
      true
  | End ->
      scroll_by t 1. ~unit:`Content;
      true
  | _ -> false

(* ───── Construction ───── *)

let arrow_direction orientation ~is_end =
  match (orientation, is_end) with
  | `Vertical, false -> Arrow.Up
  | `Vertical, true -> Arrow.Down
  | `Horizontal, false -> Arrow.Left
  | `Horizontal, true -> Arrow.Right

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity
    ?(orientation = `Vertical) ?(show_arrows = false)
    ?(track_color = default_track_color) ?(thumb_color = default_thumb_color)
    ?(arrow_fg = Ansi.Color.White) ?(arrow_bg = Ansi.Color.default) ?on_change
    () =
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  (* Configure root layout as a flex container *)
  let root_style =
    let s = Renderable.style node in
    let s =
      match orientation with
      | `Vertical ->
          Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column s
      | `Horizontal ->
          Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Row s
    in
    let s =
      Toffee.Style.set_align_items (Some Toffee.Style.Align_items.Stretch) s
    in
    let s =
      Toffee.Style.set_align_self (Some Toffee.Style.Align_items.Stretch) s
    in
    let s = Toffee.Style.set_flex_shrink 0. s in
    let size = Toffee.Style.size s in
    let is_auto d =
      Toffee.Style.Dimension.equal d Toffee.Style.Dimension.auto
    in
    let size =
      match orientation with
      | `Vertical ->
          Toffee.Geometry.Size.
            {
              width =
                (if is_auto size.width then Toffee.Style.Dimension.length 1.
                 else size.width);
              height =
                (if is_auto size.height then Toffee.Style.Dimension.percent 1.0
                 else size.height);
            }
      | `Horizontal ->
          Toffee.Geometry.Size.
            {
              width =
                (if is_auto size.width then Toffee.Style.Dimension.percent 1.0
                 else size.width);
              height =
                (if is_auto size.height then Toffee.Style.Dimension.length 1.
                 else size.height);
            }
    in
    Toffee.Style.set_size size s
  in
  Renderable.set_style node root_style;
  Renderable.set_focusable node true;
  (* Placeholder: t needs to exist for closures. Use a ref to break the
     cycle. *)
  let t_ref = ref None in
  let get_t () = match !t_ref with Some t -> t | None -> assert false in
  (* Create start arrow *)
  let start_arrow =
    Arrow.create ~parent:node
      ~direction:(arrow_direction orientation ~is_end:false)
      ~fg:arrow_fg ~bg:arrow_bg
      ~on_click:(fun () ->
        let t = get_t () in
        scroll_by t (-0.5) ~unit:`Viewport;
        start_repeat t ~is_end:false)
      ()
  in
  Renderable.set_visible (Arrow.node start_arrow) show_arrows;
  (* Create slider *)
  let slider_orientation : Slider.orientation =
    match orientation with `Vertical -> `Vertical | `Horizontal -> `Horizontal
  in
  let slider =
    Slider.create ~parent:node ~orientation:slider_orientation ~min:0. ~max:0.
      ~value:0. ~viewport_size:1. ~track_color ~thumb_color ()
  in
  let slider_style =
    let s = Renderable.style (Slider.node slider) in
    let s = Toffee.Style.set_flex_grow 1. s in
    let s = Toffee.Style.set_flex_shrink 1. s in
    Toffee.Style.set_align_self (Some Toffee.Style.Align_items.Stretch) s
  in
  Renderable.set_style (Slider.node slider) slider_style;
  (* Create end arrow *)
  let end_arrow =
    Arrow.create ~parent:node
      ~direction:(arrow_direction orientation ~is_end:true)
      ~fg:arrow_fg ~bg:arrow_bg
      ~on_click:(fun () ->
        let t = get_t () in
        scroll_by t 0.5 ~unit:`Viewport;
        start_repeat t ~is_end:true)
      ()
  in
  Renderable.set_visible (Arrow.node end_arrow) show_arrows;
  let props =
    {
      Props.orientation;
      show_arrows;
      track_color;
      thumb_color;
      arrow_fg;
      arrow_bg;
    }
  in
  let t =
    {
      node;
      slider;
      start_arrow;
      end_arrow;
      props;
      scroll_position = 0;
      scroll_size = 0;
      viewport_size = 0;
      scroll_step = None;
      manual_visibility = false;
      on_change;
      syncing_from_slider = false;
      repeat_active = false;
      repeat_is_end = false;
      repeat_delay = 500.;
      repeat_elapsed = 0.;
    }
  in
  t_ref := Some t;
  (* Wire slider change → scroll position *)
  Slider.set_on_change slider
    (Some
       (fun v ->
         if not t.syncing_from_slider then (
           t.syncing_from_slider <- true;
           set_scroll_position t (int_of_float (Float.round v));
           t.syncing_from_slider <- false)));
  (* Wire keyboard handler *)
  Renderable.set_default_key_handler node
    (Some (fun ev -> if handle_key t ev then Event.Key.prevent_default ev));
  (* Arrow repeat via on_frame *)
  Renderable.set_on_frame node
    (Some
       (fun _ ~delta ->
         if t.repeat_active then
           let d = if Float.is_nan delta then 0. else max 0. delta in
           if t.repeat_delay > 0. then (
             let new_delay = t.repeat_delay -. d in
             t.repeat_delay <- new_delay;
             if new_delay <= 0. then
               let amt = if t.repeat_is_end then 0.5 else -0.5 in
               scroll_by t amt ~unit:`Viewport)
           else (
             t.repeat_elapsed <- t.repeat_elapsed +. d;
             if t.repeat_elapsed >= 200. then (
               t.repeat_elapsed <- 0.;
               let amt = if t.repeat_is_end then 0.2 else -0.2 in
               scroll_by t amt ~unit:`Viewport))));
  (* Stop repeat on mouse up anywhere on the bar *)
  Renderable.on_mouse node (fun ev ->
      match Event.Mouse.kind ev with
      | Up _ when t.repeat_active ->
          stop_repeat t;
          Event.Mouse.stop_propagation ev
      | _ -> ());
  sync_slider t;
  recalc_visibility t;
  t

(* ───── Accessors & Setters ───── *)

let scroll_position t = t.scroll_position
let scroll_size t = t.scroll_size
let viewport_size t = t.viewport_size
let set_on_change t cb = t.on_change <- cb
let set_scroll_step t step = t.scroll_step <- step

let set_scroll_size t size =
  let size = max 0 size in
  if size <> t.scroll_size then (
    t.scroll_size <- size;
    recalc_visibility t;
    sync_slider t;
    set_scroll_position_internal ~emit:false t t.scroll_position)

let set_viewport_size t size =
  let size = max 0 size in
  if size <> t.viewport_size then (
    t.viewport_size <- size;
    recalc_visibility t;
    sync_slider t;
    set_scroll_position_internal ~emit:false t t.scroll_position)

let set_show_arrows t v =
  if t.props.show_arrows <> v then (
    t.props <- { t.props with show_arrows = v };
    Renderable.set_visible (Arrow.node t.start_arrow) v;
    Renderable.set_visible (Arrow.node t.end_arrow) v;
    Renderable.request_render t.node)

let set_track_color t c =
  if not (Ansi.Color.equal t.props.track_color c) then (
    t.props <- { t.props with track_color = c };
    Slider.set_track_color t.slider c)

let set_thumb_color t c =
  if not (Ansi.Color.equal t.props.thumb_color c) then (
    t.props <- { t.props with thumb_color = c };
    Slider.set_thumb_color t.slider c)

let set_visible_override t vis =
  t.manual_visibility <- true;
  Renderable.set_visible t.node vis

let reset_visibility_control t =
  t.manual_visibility <- false;
  recalc_visibility t

(* ───── Apply Props ───── *)

let apply_props t (props : Props.t) =
  if not (Props.equal t.props props) then (
    set_show_arrows t props.show_arrows;
    set_track_color t props.track_color;
    set_thumb_color t props.thumb_color;
    t.props <-
      {
        t.props with
        show_arrows = props.show_arrows;
        track_color = props.track_color;
        thumb_color = props.thumb_color;
        arrow_fg = props.arrow_fg;
        arrow_bg = props.arrow_bg;
      };
    Renderable.request_render t.node)

(* ───── Pretty-printing ───── *)

let pp ppf t =
  let orient =
    match t.props.orientation with `Vertical -> "v" | `Horizontal -> "h"
  in
  Format.fprintf ppf "ScrollBar(%s, %s, pos=%d, size=%d, vp=%d)"
    (Renderable.id t.node) orient t.scroll_position t.scroll_size
    t.viewport_size
