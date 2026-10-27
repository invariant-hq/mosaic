(* ───── Types ───── *)

type orientation = [ `Vertical | `Horizontal ]
type scroll_unit = [ `Absolute | `Viewport | `Content | `Step ]

(* ───── Arrow ───── *)

module Arrow = struct
  type direction = Up | Down | Left | Right

  type t = {
    node : Renderable.t;
    mutable direction : direction;
    mutable fg : Ansi.Color.t option;
    mutable bg : Ansi.Color.t option;
  }

  let node t = t.node

  (* The arrow is the theme's glyph in the [Border] role, its colors given by
     label first. *)
  let arrow_char t =
    let glyphs = Theme.glyphs (Renderable.theme t.node) in
    match t.direction with
    | Up -> glyphs.arrow_up
    | Down -> glyphs.arrow_down
    | Left -> glyphs.arrow_left
    | Right -> glyphs.arrow_right

  let set_direction t d =
    if t.direction <> d then begin
      t.direction <- d;
      Renderable.request_render t.node
    end

  let set_colors t ~fg ~bg =
    let same a b = Option.equal Ansi.Color.equal a b in
    if not (same t.fg fg && same t.bg bg) then begin
      t.fg <- fg;
      t.bg <- bg;
      Renderable.request_render t.node
    end

  let render t _self grid ~dt:_ =
    let x = Renderable.x t.node in
    let y = Renderable.y t.node in
    let text = arrow_char t in
    let style = Theme.style (Renderable.theme t.node) Border in
    let style =
      Option.fold ~none:style ~some:(fun c -> Ansi.Style.fg c style) t.fg
    in
    let style =
      Option.fold ~none:style ~some:(fun c -> Ansi.Style.bg c style) t.bg
    in
    Matrix_grid.draw_text grid ~x ~y ~text ~style

  let measure t ~known_dimensions:_ ~available_space:_ ~style:_ =
    let text = arrow_char t in
    let width_method = Renderable.Private.width_method t.node in
    let width =
      float_of_int (max 1 (Matrix.Text.measure ~width_method ~tab_width:2 text))
    in
    Toffee.Geometry.Size.make width 1.

  let create ~parent ~direction ?fg ?bg ~on_click () =
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

type style = {
  show_arrows : bool;
  track_color : Ansi.Color.t option;
  thumb_color : Ansi.Color.t option;
  arrow_fg : Ansi.Color.t option;
  arrow_bg : Ansi.Color.t option;
}

let style ?(show_arrows = false) ?track_color ?thumb_color ?arrow_fg ?arrow_bg
    () =
  { show_arrows; track_color; thumb_color; arrow_fg; arrow_bg }

let style_equal a b =
  let same a b = Option.equal Ansi.Color.equal a b in
  a == b
  || a.show_arrows = b.show_arrows
     && same a.track_color b.track_color
     && same a.thumb_color b.thumb_color
     && same a.arrow_fg b.arrow_fg && same a.arrow_bg b.arrow_bg

(* [Props.make] is the one place the scroll bar's labels get their defaults:
   [create] and [Mosaic.scroll_bar] pass their labels on unset. *)
module Props = struct
  type t = {
    position : int;
    scroll_size : int;
    viewport_size : int;
    orientation : orientation;
    show_arrows : bool;
    track_color : Ansi.Color.t option;
    thumb_color : Ansi.Color.t option;
    arrow_fg : Ansi.Color.t option;
    arrow_bg : Ansi.Color.t option;
  }

  let make ?(position = 0) ?(scroll_size = 0) ?(viewport_size = 0)
      ?(orientation = `Vertical) ?(show_arrows = false) ?track_color
      ?thumb_color ?arrow_fg ?arrow_bg () =
    {
      position = max 0 position;
      scroll_size = max 0 scroll_size;
      viewport_size = max 0 viewport_size;
      orientation;
      show_arrows;
      track_color;
      thumb_color;
      arrow_fg;
      arrow_bg;
    }

  let default = make ()

  let equal a b =
    a.position = b.position
    && a.scroll_size = b.scroll_size
    && a.viewport_size = b.viewport_size
    && a.orientation = b.orientation
    && a.show_arrows = b.show_arrows
    && Option.equal Ansi.Color.equal a.track_color b.track_color
    && Option.equal Ansi.Color.equal a.thumb_color b.thumb_color
    && Option.equal Ansi.Color.equal a.arrow_fg b.arrow_fg
    && Option.equal Ansi.Color.equal a.arrow_bg b.arrow_bg
end

(* ───── Scroll Bar ───── *)

type t = {
  node : Renderable.t;
  slider : Slider.t;
  start_arrow : Arrow.t;
  end_arrow : Arrow.t;
  (* Dimensions the widget owns: the user left them [auto] at creation, so
     they default to the orientation's natural size and are swapped when the
     orientation changes. *)
  auto_width : bool;
  auto_height : bool;
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
  if clamped <> t.scroll_position then
    if emit then Option.iter (fun f -> f clamped) t.on_change
    else (
      t.scroll_position <- clamped;
      Slider.set_value t.slider (float_of_int clamped);
      Renderable.request_render t.node)

let set_scroll_position t value =
  set_scroll_position_internal t ~emit:false value

let scroll_target t delta ~unit =
  let multiplier =
    match unit with
    | `Absolute -> 1.
    | `Viewport -> float_of_int (max 1 t.viewport_size)
    | `Content -> float_of_int (max 1 t.scroll_size)
    | `Step ->
        float_of_int
          (match t.scroll_step with Some s when s > 0 -> s | _ -> 1)
  in
  int_of_float
    (Float.round (float_of_int t.scroll_position +. (delta *. multiplier)))

let scroll_by t delta ~unit =
  set_scroll_position_internal t (scroll_target t delta ~unit)

(* ───── Arrow Repeat ───── *)

let stop_repeat t =
  t.repeat_active <- false;
  Renderable.set_live t.node false

(* Seconds before the first repeat, then between repeats; frame deltas are
   seconds. *)
let repeat_first_delay = 0.5
let repeat_interval = 0.2

let start_repeat t ~is_end =
  t.repeat_active <- true;
  t.repeat_is_end <- is_end;
  t.repeat_delay <- repeat_first_delay;
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

(* Flex direction and natural size for [orientation]. Only the dimensions the
   widget owns ([auto_width]/[auto_height]) take the orientation's defaults;
   user-set dimensions are preserved. *)
let orientation_style style orientation ~auto_width ~auto_height =
  let style =
    match orientation with
    | `Vertical ->
        Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column style
    | `Horizontal ->
        Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Row style
  in
  let default_width, default_height =
    match orientation with
    | `Vertical ->
        (Toffee.Style.Dimension.length 1., Toffee.Style.Dimension.percent 1.0)
    | `Horizontal ->
        (Toffee.Style.Dimension.percent 1.0, Toffee.Style.Dimension.length 1.)
  in
  let size = Toffee.Style.size style in
  let size =
    Toffee.Geometry.Size.
      {
        width = (if auto_width then default_width else size.width);
        height = (if auto_height then default_height else size.height);
      }
  in
  Toffee.Style.set_size size style

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?position
    ?scroll_size ?viewport_size ?orientation ?show_arrows ?track_color
    ?thumb_color ?arrow_fg ?arrow_bg ?on_change () =
  let props =
    Props.make ?position ?scroll_size ?viewport_size ?orientation ?show_arrows
      ?track_color ?thumb_color ?arrow_fg ?arrow_bg ()
  in
  let ({
         position;
         scroll_size;
         viewport_size;
         orientation;
         show_arrows;
         track_color;
         thumb_color;
         arrow_fg;
         arrow_bg;
       }
        : Props.t) =
    props
  in
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  (* Configure root layout as a flex container *)
  let auto_width, auto_height =
    let size = Toffee.Style.size (Renderable.style node) in
    let is_auto d =
      Toffee.Style.Dimension.equal d Toffee.Style.Dimension.auto
    in
    (is_auto size.width, is_auto size.height)
  in
  let root_style =
    let s = Renderable.style node in
    let s =
      Toffee.Style.set_align_items (Some Toffee.Style.Align_items.Stretch) s
    in
    let s =
      Toffee.Style.set_align_self (Some Toffee.Style.Align_items.Stretch) s
    in
    let s = Toffee.Style.set_flex_shrink 0. s in
    orientation_style s orientation ~auto_width ~auto_height
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
      ?fg:arrow_fg ?bg:arrow_bg
      ~on_click:(fun () ->
        let t = get_t () in
        scroll_by t (-0.5) ~unit:`Viewport;
        start_repeat t ~is_end:false)
      ()
  in
  Renderable.set_visible (Arrow.node start_arrow) show_arrows;
  (* Create slider *)
  let slider =
    Slider.create ~parent:node ~orientation ~min:0. ~max:0. ~value:0.
      ~viewport_size:1. ?track_color ?thumb_color ()
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
      ?fg:arrow_fg ?bg:arrow_bg
      ~on_click:(fun () ->
        let t = get_t () in
        scroll_by t 0.5 ~unit:`Viewport;
        start_repeat t ~is_end:true)
      ()
  in
  Renderable.set_visible (Arrow.node end_arrow) show_arrows;
  let t =
    {
      node;
      slider;
      start_arrow;
      end_arrow;
      auto_width;
      auto_height;
      props;
      scroll_position = min position (max 0 (scroll_size - viewport_size));
      scroll_size;
      viewport_size;
      scroll_step = None;
      manual_visibility = false;
      on_change;
      syncing_from_slider = false;
      repeat_active = false;
      repeat_is_end = false;
      repeat_delay = repeat_first_delay;
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
           set_scroll_position_internal t (int_of_float (Float.round v));
           t.syncing_from_slider <- false)));
  (* Wire keyboard handler *)
  Renderable.set_key_behavior node
    (Some (fun ev -> if handle_key t ev then Event.Key.prevent_default ev));
  (* Arrow repeat via on_frame *)
  Renderable.set_on_frame node
    (Some
       (fun _ ~dt ->
         if t.repeat_active then
           let d = if Float.is_nan dt then 0. else max 0. dt in
           if t.repeat_delay > 0. then (
             let new_delay = t.repeat_delay -. d in
             t.repeat_delay <- new_delay;
             if new_delay <= 0. then
               let amt = if t.repeat_is_end then 0.5 else -0.5 in
               scroll_by t amt ~unit:`Viewport)
           else (
             t.repeat_elapsed <- t.repeat_elapsed +. d;
             if t.repeat_elapsed >= repeat_interval then (
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

(* The slider draws the track and the thumb, with the bar's colors, which
   may be unset: it takes them with the numbers it already has. *)
let sync_slider_colors t =
  Slider.apply_props t.slider
    (Slider.Props.make ~orientation:t.props.orientation
       ~min:(Slider.min t.slider) ~max:(Slider.max t.slider)
       ~value:(Slider.value t.slider)
       ~viewport_size:(float_of_int (max 1 t.viewport_size))
       ?track_color:t.props.track_color ?thumb_color:t.props.thumb_color ())

let set_slider_colors t ~track ~thumb =
  let same a b = Option.equal Ansi.Color.equal a b in
  if not (same t.props.track_color track && same t.props.thumb_color thumb) then begin
    t.props <- { t.props with track_color = track; thumb_color = thumb };
    sync_slider_colors t
  end

let set_track_color t c =
  set_slider_colors t ~track:(Some c) ~thumb:t.props.thumb_color

let set_thumb_color t c =
  set_slider_colors t ~track:t.props.track_color ~thumb:(Some c)

let set_orientation t orientation =
  if t.props.orientation <> orientation then begin
    t.props <- { t.props with orientation };
    Renderable.set_style t.node
      (orientation_style (Renderable.style t.node) orientation
         ~auto_width:t.auto_width ~auto_height:t.auto_height);
    Slider.set_orientation t.slider orientation;
    Arrow.set_direction t.start_arrow
      (arrow_direction orientation ~is_end:false);
    Arrow.set_direction t.end_arrow (arrow_direction orientation ~is_end:true);
    Renderable.request_render t.node
  end

let set_arrow_colors t ~fg ~bg =
  let same a b = Option.equal Ansi.Color.equal a b in
  if not (same t.props.arrow_fg fg && same t.props.arrow_bg bg) then begin
    t.props <- { t.props with arrow_fg = fg; arrow_bg = bg };
    Arrow.set_colors t.start_arrow ~fg ~bg;
    Arrow.set_colors t.end_arrow ~fg ~bg
  end

let apply_style t (style : style) =
  set_show_arrows t style.show_arrows;
  set_slider_colors t ~track:style.track_color ~thumb:style.thumb_color;
  set_arrow_colors t ~fg:style.arrow_fg ~bg:style.arrow_bg

let set_visible_override t vis =
  t.manual_visibility <- true;
  Renderable.set_visible t.node vis

let reset_visibility_control t =
  t.manual_visibility <- false;
  recalc_visibility t

(* ───── Apply Props ───── *)

let apply_props t (props : Props.t) =
  set_scroll_size t props.scroll_size;
  set_viewport_size t props.viewport_size;
  set_scroll_position t props.position;
  if not (Props.equal t.props props) then (
    set_orientation t props.orientation;
    set_show_arrows t props.show_arrows;
    set_slider_colors t ~track:props.track_color ~thumb:props.thumb_color;
    set_arrow_colors t ~fg:props.arrow_fg ~bg:props.arrow_bg;
    Renderable.request_render t.node);
  t.props <- props

(* ───── Pretty-printing ───── *)

let pp ppf t =
  let orient =
    match t.props.orientation with `Vertical -> "v" | `Horizontal -> "h"
  in
  Format.fprintf ppf "ScrollBar(%s, %s, pos=%d, size=%d, vp=%d)"
    (Renderable.id t.node) orient t.scroll_position t.scroll_size
    t.viewport_size
