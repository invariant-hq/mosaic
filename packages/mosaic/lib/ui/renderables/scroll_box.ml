(* ───── Scroll Acceleration ───── *)

module Scroll_accel = struct
  type strategy =
    | Linear
    | MacOS of { a : float; tau : float; max_multiplier : float }

  type t = {
    strategy : strategy;
    mutable last_now : float;
    mutable history : float list;
  }

  let min_interval = 0.006
  let streak_timeout = 0.150
  let ref_interval = 0.100
  let max_history = 3
  let linear () = { strategy = Linear; last_now = 0.; history = [] }

  let macos ?(a = 0.8) ?(tau = 3.0) ?(max_multiplier = 6.0) () =
    { strategy = MacOS { a; tau; max_multiplier }; last_now = 0.; history = [] }

  let tick t ~seconds:now =
    match t.strategy with
    | Linear -> 1.0
    | MacOS { a; tau; max_multiplier } ->
        let dt =
          if t.last_now = 0. then Float.infinity
          else Float.max 0. (now -. t.last_now)
        in
        if dt = Float.infinity || dt > streak_timeout then (
          t.last_now <- now;
          t.history <- [];
          1.0)
        else if dt < min_interval then 1.0
        else (
          t.last_now <- now;
          let history = dt :: t.history in
          let history =
            if List.length history > max_history then
              List.filteri (fun i _ -> i < max_history) history
            else history
          in
          t.history <- history;
          let avg_interval =
            match history with
            | [] -> dt
            | _ ->
                let sum = List.fold_left ( +. ) 0. history in
                sum /. float_of_int (List.length history)
          in
          let velocity = ref_interval /. avg_interval in
          let x = velocity /. tau in
          let mult = 1.0 +. (a *. (exp x -. 1.0)) in
          Float.min max_multiplier mult)

  let reset t =
    t.last_now <- 0.;
    t.history <- []
end

(* ───── Props ───── *)

(* [Props.make] is the one place the scroll box's labels get their defaults:
   [Mosaic.scroll_box] passes its labels on unset. *)
module Props = struct
  type t = {
    scroll_x : bool;
    scroll_y : bool;
    scroll : Scroll.t;
    background : Ansi.Color.t option;
    scroll_accel : Scroll_accel.t;
    show_scrollbars : bool;
    scrollbar_style : Scroll_bar.style option;
    vertical_bar_style : Scroll_bar.style option;
    horizontal_bar_style : Scroll_bar.style option;
  }

  (* One linear acceleration serves every box without its own: it keeps no
     history, so sharing it shares nothing, and two default props compare
     equal. *)
  let linear = Scroll_accel.linear ()

  let make ?(scroll_x = false) ?(scroll_y = true) ?(scroll = Scroll.at 0)
      ?background ?(scroll_accel = linear) ?(show_scrollbars = true)
      ?scrollbar_style ?vertical_bar_style ?horizontal_bar_style () =
    {
      scroll_x;
      scroll_y;
      scroll;
      background;
      scroll_accel;
      show_scrollbars;
      scrollbar_style;
      vertical_bar_style;
      horizontal_bar_style;
    }

  let default = make ()

  let equal a b =
    a.scroll_x = b.scroll_x && a.scroll_y = b.scroll_y
    && Scroll.equal a.scroll b.scroll
    && Option.equal Ansi.Color.equal a.background b.background
    && a.scroll_accel == b.scroll_accel
    && a.show_scrollbars = b.show_scrollbars
    && Option.equal Scroll_bar.style_equal a.scrollbar_style b.scrollbar_style
    && Option.equal Scroll_bar.style_equal a.vertical_bar_style
         b.vertical_bar_style
    && Option.equal Scroll_bar.style_equal a.horizontal_bar_style
         b.horizontal_bar_style
end

(* ───── Scroll Box ───── *)

type t = {
  node : Renderable.t;
  wrapper : Renderable.t;
  viewport : Renderable.t;
  content : Renderable.t;
  mutable props : Props.t;
  vertical_bar : Scroll_bar.t;
  horizontal_bar : Scroll_bar.t;
  mutable scroll_x : int;
  mutable scroll_y : int;
  mutable max_scroll_x : int;
  mutable max_scroll_y : int;
  mutable content_w : int;
  mutable content_h : int;
  mutable on_scroll : (Scroll.t -> unit) option;
  (* Seconds accumulated from frame deltas; feeds Scroll_accel.tick. *)
  mutable frame_clock : float;
  mutable wheel_acc_x : float;
  mutable wheel_acc_y : float;
  mutable auto_scroll_mouse_x : int;
  mutable auto_scroll_mouse_y : int;
  mutable cached_auto_scroll_speed : float;
  mutable auto_scroll_acc_x : float;
  mutable auto_scroll_acc_y : float;
  mutable auto_scrolling : bool;
}

let node t = t.node
let content t = t.content
let viewport t = t.viewport
let vertical_bar t = t.vertical_bar
let horizontal_bar t = t.horizontal_bar
let scroll_top t = t.scroll_y
let scroll_left t = t.scroll_x
let scroll_width t = t.content_w
let scroll_height t = t.content_h
let viewport_width t = Renderable.width t.viewport
let viewport_height t = Renderable.height t.viewport
let clamp v ~lo ~hi = max lo (min hi v)
let auto_scroll_threshold_vertical = 3
let auto_scroll_threshold_horizontal = 3

(* Selection auto-scroll speeds, in cells per second. *)
let auto_scroll_speed_slow = 6.
let auto_scroll_speed_medium = 36.
let auto_scroll_speed_fast = 72.

let set_child_offsets t =
  Renderable.set_translate t.content ~x:(-t.scroll_x) ~y:(-t.scroll_y)

(* ───── Scroll Position ───── *)

(* Report the movement so presentation can turn a lone vertical scroll into
   a DECSTBM hardware scroll instead of re-diffing the whole viewport. The
   region's rows are the viewport rows that shift; its x/width are the box's
   horizontal footprint (the renderer requires full-width ownership, since a
   hardware scroll moves entire terminal rows). *)
let report_scroll t ~dx ~dy =
  let vp = Renderable.bounds t.viewport in
  let box = Renderable.bounds t.node in
  if vp.height > 0 && box.width > 0 then
    Renderable.Private.report_scroll t.node
      ~region:
        {
          Matrix_grid.x = box.x;
          y = vp.y;
          width = box.width;
          height = vp.height;
        }
      ~dx ~dy

let scroll_to_internal t ?x ?y () =
  let nx =
    clamp (Option.value ~default:t.scroll_x x) ~lo:0 ~hi:t.max_scroll_x
  in
  let ny =
    clamp (Option.value ~default:t.scroll_y y) ~lo:0 ~hi:t.max_scroll_y
  in
  if nx <> t.scroll_x || ny <> t.scroll_y then
    let x = Option.map (fun _ -> nx) x in
    let y = Option.map (fun _ -> ny) y in
    Option.iter
      (fun f -> f (Scroll.with_position ?x ?y t.props.scroll))
      t.on_scroll

let resolve_scroll t =
  let x, y =
    Scroll.resolve t.props.scroll ~width:(viewport_width t)
      ~height:(viewport_height t)
      ~content_width:
        (if t.props.scroll_x then t.content_w else viewport_width t)
      ~content_height:
        (if t.props.scroll_y then t.content_h else viewport_height t)
  in
  let dx = x - t.scroll_x and dy = y - t.scroll_y in
  t.scroll_x <- x;
  t.scroll_y <- y;
  Scroll_bar.set_scroll_position t.horizontal_bar x;
  Scroll_bar.set_scroll_position t.vertical_bar y;
  set_child_offsets t;
  if dx <> 0 || dy <> 0 then (
    report_scroll t ~dx ~dy;
    Renderable.Private.request_selection_update t.node)

let set_scroll_top t v = scroll_to_internal t ~y:v ()
let set_scroll_left t v = scroll_to_internal t ~x:v ()
let scroll_to t ?x ?y () = scroll_to_internal t ?x ?y ()

let scroll_by t ?x ?y () =
  let nx = Option.map (fun dx -> t.scroll_x + dx) x in
  let ny = Option.map (fun dy -> t.scroll_y + dy) y in
  scroll_to_internal t ?x:nx ?y:ny ()

let scroll_by_unit t ?x ?y ~(unit : Scroll_bar.scroll_unit) () =
  let x =
    Option.map
      (fun delta -> Scroll_bar.scroll_target t.horizontal_bar delta ~unit)
      x
  in
  let y =
    Option.map
      (fun delta -> Scroll_bar.scroll_target t.vertical_bar delta ~unit)
      y
  in
  scroll_to_internal t ?x ?y ()

(* ───── Selection Auto-scroll ───── *)

let auto_scroll_direction_x t mouse_x =
  let relative_x = mouse_x - Renderable.x t.node in
  let dist_to_left = relative_x in
  let dist_to_right = Renderable.width t.node - relative_x in
  if dist_to_left <= auto_scroll_threshold_horizontal then
    if t.scroll_x > 0 then -1 else 0
  else if dist_to_right <= auto_scroll_threshold_horizontal then
    if t.scroll_x < t.max_scroll_x then 1 else 0
  else 0

let auto_scroll_direction_y t mouse_y =
  let relative_y = mouse_y - Renderable.y t.node in
  let dist_to_top = relative_y in
  let dist_to_bottom = Renderable.height t.node - relative_y in
  if dist_to_top <= auto_scroll_threshold_vertical then
    if t.scroll_y > 0 then -1 else 0
  else if dist_to_bottom <= auto_scroll_threshold_vertical then
    if t.scroll_y < t.max_scroll_y then 1 else 0
  else 0

let auto_scroll_speed t mouse_x mouse_y =
  let relative_x = mouse_x - Renderable.x t.node in
  let relative_y = mouse_y - Renderable.y t.node in
  let dist_to_left = relative_x in
  let dist_to_right = Renderable.width t.node - relative_x in
  let dist_to_top = relative_y in
  let dist_to_bottom = Renderable.height t.node - relative_y in
  let min_distance =
    min (min dist_to_left dist_to_right) (min dist_to_top dist_to_bottom)
  in
  if min_distance <= 1 then auto_scroll_speed_fast
  else if min_distance <= 2 then auto_scroll_speed_medium
  else auto_scroll_speed_slow

let stop_auto_scroll t =
  let was_auto_scrolling = t.auto_scrolling in
  t.auto_scrolling <- false;
  t.auto_scroll_acc_x <- 0.;
  t.auto_scroll_acc_y <- 0.;
  if was_auto_scrolling then Renderable.set_live t.node false

let start_auto_scroll t mouse_x mouse_y =
  stop_auto_scroll t;
  t.auto_scroll_mouse_x <- mouse_x;
  t.auto_scroll_mouse_y <- mouse_y;
  t.cached_auto_scroll_speed <- auto_scroll_speed t mouse_x mouse_y;
  t.auto_scrolling <- true;
  Renderable.set_live t.node true

let update_auto_scroll t mouse_x mouse_y =
  t.auto_scroll_mouse_x <- mouse_x;
  t.auto_scroll_mouse_y <- mouse_y;
  t.cached_auto_scroll_speed <- auto_scroll_speed t mouse_x mouse_y;
  let scroll_x = auto_scroll_direction_x t mouse_x in
  let scroll_y = auto_scroll_direction_y t mouse_y in
  if scroll_x = 0 && scroll_y = 0 then stop_auto_scroll t
  else if not t.auto_scrolling then start_auto_scroll t mouse_x mouse_y

let auto_scroll_step acc direction amount =
  let total = acc +. (float_of_int direction *. amount) in
  let cells = int_of_float total in
  (total -. float_of_int cells, cells)

let handle_auto_scroll t delta =
  if t.auto_scrolling then
    match Renderable.Private.get_selection t.node with
    | Some sel when Selection.is_dragging sel ->
        let scroll_x = auto_scroll_direction_x t t.auto_scroll_mouse_x in
        let scroll_y = auto_scroll_direction_y t t.auto_scroll_mouse_y in
        (* Speeds are cells per second. *)
        let scroll_amount = t.cached_auto_scroll_speed *. Float.max 0. delta in
        let acc_x, dx =
          auto_scroll_step t.auto_scroll_acc_x scroll_x scroll_amount
        in
        let acc_y, dy =
          auto_scroll_step t.auto_scroll_acc_y scroll_y scroll_amount
        in
        t.auto_scroll_acc_x <- acc_x;
        t.auto_scroll_acc_y <- acc_y;
        if dx <> 0 || dy <> 0 then
          scroll_to_internal t
            ?x:(if dx = 0 then None else Some (t.scroll_x + dx))
            ?y:(if dy = 0 then None else Some (t.scroll_y + dy))
            ();
        if scroll_x = 0 && scroll_y = 0 then stop_auto_scroll t
    | _ -> stop_auto_scroll t

(* ───── Metrics Recalculation ───── *)

let recalc_metrics t =
  t.content_w <- Renderable.width t.content;
  t.content_h <- Renderable.height t.content;
  let vw = Renderable.width t.viewport in
  let vh = Renderable.height t.viewport in
  t.max_scroll_x <- (if t.props.scroll_x then max 0 (t.content_w - vw) else 0);
  t.max_scroll_y <- (if t.props.scroll_y then max 0 (t.content_h - vh) else 0);
  Scroll_bar.set_scroll_size t.vertical_bar t.content_h;
  Scroll_bar.set_viewport_size t.vertical_bar vh;
  Scroll_bar.set_scroll_size t.horizontal_bar t.content_w;
  Scroll_bar.set_viewport_size t.horizontal_bar vw

(* Resolve accepted symbolic state before viewport culling. *)
let update_scroll_box t ~dt =
  let d = if Float.is_nan dt then 0. else Float.max 0. dt in
  t.frame_clock <- t.frame_clock +. d;
  if Renderable.width t.node > 0 && Renderable.height t.node > 0 then (
    recalc_metrics t;
    resolve_scroll t)

(* ───── Render ───── *)

let render_scroll_box t _self grid ~dt:_ =
  match t.props.background with
  | None -> ()
  | Some c ->
      let lx = Renderable.x t.node in
      let ly = Renderable.y t.node in
      let lw = Renderable.width t.node in
      let lh = Renderable.height t.node in
      if lw > 0 && lh > 0 then
        Matrix_grid.fill_rect grid ~x:lx ~y:ly ~width:lw ~height:lh ~color:c

(* ───── Mouse Wheel ───── *)

let remap_scroll_direction direction shift =
  if not shift then direction
  else
    match direction with
    | Event.Mouse.Scroll_up -> Event.Mouse.Scroll_left
    | Event.Mouse.Scroll_down -> Event.Mouse.Scroll_right
    | Event.Mouse.Scroll_left -> Event.Mouse.Scroll_down
    | Event.Mouse.Scroll_right -> Event.Mouse.Scroll_up

let on_mouse t (event : Event.mouse) =
  match Event.Mouse.kind event with
  | Scroll { direction; delta } ->
      let direction =
        remap_scroll_direction direction (Event.Mouse.modifiers event).shift
      in
      let dx, dy =
        match direction with
        | Event.Mouse.Scroll_up -> (0, -delta)
        | Event.Mouse.Scroll_down -> (0, delta)
        | Event.Mouse.Scroll_left -> (-delta, 0)
        | Event.Mouse.Scroll_right -> (delta, 0)
      in
      let dx = if t.props.scroll_x then dx else 0 in
      let dy = if t.props.scroll_y then dy else 0 in
      if dx <> 0 || dy <> 0 then begin
        let mult =
          Scroll_accel.tick t.props.scroll_accel ~seconds:t.frame_clock
        in
        let fx = float_of_int dx *. mult in
        let fy = float_of_int dy *. mult in
        t.wheel_acc_x <- t.wheel_acc_x +. fx;
        t.wheel_acc_y <- t.wheel_acc_y +. fy;
        let stepx = int_of_float t.wheel_acc_x in
        let stepy = int_of_float t.wheel_acc_y in
        if stepx <> 0 then t.wheel_acc_x <- t.wheel_acc_x -. float_of_int stepx;
        if stepy <> 0 then t.wheel_acc_y <- t.wheel_acc_y -. float_of_int stepy;
        if stepx <> 0 || stepy <> 0 then begin
          let x = clamp (t.scroll_x + stepx) ~lo:0 ~hi:t.max_scroll_x in
          let y = clamp (t.scroll_y + stepy) ~lo:0 ~hi:t.max_scroll_y in
          if x <> t.scroll_x || y <> t.scroll_y then (
            scroll_to_internal t
              ?x:(if stepx = 0 then None else Some x)
              ?y:(if stepy = 0 then None else Some y)
              ();
            Event.Mouse.stop_propagation event)
        end
      end
  | Drag { button = Left; is_dragging = true } ->
      update_auto_scroll t (Event.Mouse.x event) (Event.Mouse.y event)
  | Up { button = Left; _ } -> stop_auto_scroll t
  | _ -> ()

(* ───── Keyboard ───── *)

(* Only the bar of an axis the box scrolls takes keys: a key the box has no
   use for passes on. *)
let handle_key t (event : Event.key) =
  let consumed =
    t.props.scroll_y && Scroll_bar.handle_key t.vertical_bar event
  in
  let consumed =
    consumed
    || (t.props.scroll_x && Scroll_bar.handle_key t.horizontal_bar event)
  in
  if consumed then (
    Scroll_accel.reset t.props.scroll_accel;
    t.wheel_acc_x <- 0.;
    t.wheel_acc_y <- 0.;
    true)
  else false

(* ───── Setters ───── *)

let set_scroll t scroll =
  if not (Scroll.equal t.props.scroll scroll) then (
    t.props <- { t.props with scroll };
    resolve_scroll t;
    Renderable.request_render t.node)

let set_background t color =
  if not (Option.equal Ansi.Color.equal t.props.background color) then (
    t.props <- { t.props with background = color };
    Renderable.request_render t.node)

let set_on_scroll t cb = t.on_scroll <- cb
let set_scroll_accel t scroll_accel = t.props <- { t.props with scroll_accel }

(* ───── Apply Props ───── *)

let default_bar_style = Scroll_bar.style ()

let resolve_bar_style shared specific =
  match specific with
  | Some style -> style
  | None -> Option.value ~default:default_bar_style shared

(* Hidden bars use the manual-visibility override so overflow recalculation
   does not bring them back; showing again restores automatic visibility. *)
let set_show_scrollbars t v =
  if t.props.show_scrollbars <> v then begin
    t.props <- { t.props with show_scrollbars = v };
    if v then begin
      Scroll_bar.reset_visibility_control t.vertical_bar;
      Scroll_bar.reset_visibility_control t.horizontal_bar
    end
    else begin
      Scroll_bar.set_visible_override t.vertical_bar false;
      Scroll_bar.set_visible_override t.horizontal_bar false
    end
  end

(* The content is at least the viewport's size on both axes, and at most its
   size on an axis that does not scroll, so only a scrolling axis overflows. *)
let content_axes_style ~scroll_x ~scroll_y style =
  let pct100 = Toffee.Style.Dimension.percent 1.0 in
  let auto = Toffee.Style.Dimension.auto in
  Toffee.Style.set_max_size
    {
      width = (if scroll_x then auto else pct100);
      height = (if scroll_y then auto else pct100);
    }
    style

let apply_props t (props : Props.t) =
  let axes_changed =
    t.props.scroll_x <> props.scroll_x || t.props.scroll_y <> props.scroll_y
  in
  let scroll_changed =
    axes_changed || not (Scroll.equal t.props.scroll props.scroll)
  in
  if axes_changed then
    Renderable.set_style t.content
      (content_axes_style ~scroll_x:props.scroll_x ~scroll_y:props.scroll_y
         (Renderable.style t.content));
  set_background t props.background;
  set_show_scrollbars t props.show_scrollbars;
  t.props <- props;
  if scroll_changed then begin
    resolve_scroll t;
    Renderable.request_render t.node
  end;
  Scroll_bar.apply_style t.vertical_bar
    (resolve_bar_style props.scrollbar_style props.vertical_bar_style);
  Scroll_bar.apply_style t.horizontal_bar
    (resolve_bar_style props.scrollbar_style props.horizontal_bar_style)

(* ───── Root Style ───── *)

(* The root is a flex row of the wrapper and the vertical bar, stretched to
   the box's cross size. Its minimum size is the caller's where the caller
   gave one; an [auto] minimum would resolve to the content's size and stop
   the box from shrinking below it, so [auto] becomes zero on each axis
   independently. *)
let root_style style =
  let zero = Toffee.Style.Dimension.length 0. in
  let explicit_or_zero dim =
    if Toffee.Style.Dimension.is_auto dim then zero else dim
  in
  let min_size =
    Toffee.Geometry.Size.map explicit_or_zero (Toffee.Style.min_size style)
  in
  style
  |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Row
  |> Toffee.Style.set_align_items (Some Toffee.Style.Align_items.Stretch)
  |> Toffee.Style.set_min_size min_size

(* The wrapper's flex basis: the root's inner width, from which the wrapper
   shrinks by the vertical bar's width, as the bar does not shrink. A root of
   indefinite width, which a percentage basis cannot resolve against, sizes
   the wrapper by its content, as an [auto] basis does. With a definite width
   the wrapper's size does not depend on its content, and the basis spares
   measuring the content at its max-content width, a layout of the whole
   content at a width it is never laid out at. A root that wraps would put
   the bar on a line of its own under a full-width wrapper, so the wrapper of
   such a root keeps the [auto] basis. *)
let wrapper_basis root =
  if Toffee.Style.flex_wrap root = Toffee.Style.Flex_wrap.No_wrap then
    Toffee.Style.Dimension.percent 1.0
  else Toffee.Style.Dimension.auto

let set_style t style =
  let root = root_style style in
  Renderable.set_style t.node root;
  let wrapper = Renderable.style t.wrapper in
  let basis = wrapper_basis root in
  if not (Toffee.Style.Dimension.equal (Toffee.Style.flex_basis wrapper) basis)
  then
    Renderable.set_style t.wrapper (Toffee.Style.set_flex_basis basis wrapper)

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?on_scroll
    (props : Props.t) =
  let ({
         scroll_x;
         scroll_y;
         show_scrollbars;
         scrollbar_style;
         vertical_bar_style;
         horizontal_bar_style;
         _;
       }
        : Props.t) =
    props
  in
  let node =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let root = root_style (Renderable.style node) in
  Renderable.set_style node root;
  let zero = Toffee.Style.Dimension.length 0. in
  (* Wrapper: flex-column, flex-grow 1, flex-basis 100% *)
  let wrapper = Renderable.create ~parent:node () in
  let wrapper_style =
    Renderable.style wrapper
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
    |> Toffee.Style.set_flex_grow 1.
    |> Toffee.Style.set_flex_basis (wrapper_basis root)
    |> Toffee.Style.set_min_size (Toffee.Geometry.Size.square zero)
  in
  Renderable.set_style wrapper wrapper_style;
  (* Viewport: flex-column, flex-grow 1, overflow hidden *)
  let viewport_node = Renderable.create ~parent:wrapper () in
  let hidden =
    Toffee.Geometry.Point.
      { x = Toffee.Style.Overflow.Hidden; y = Toffee.Style.Overflow.Hidden }
  in
  let viewport_style =
    Renderable.style viewport_node
    |> Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column
    |> Toffee.Style.set_flex_grow 1.
    |> Toffee.Style.set_min_size (Toffee.Geometry.Size.square zero)
    |> Toffee.Style.set_overflow hidden
  in
  Renderable.set_style viewport_node viewport_style;
  (* Content: flex-column, flex-shrink 0, min 100% on both axes *)
  let content_node = Renderable.create ~parent:viewport_node () in
  let pct100 = Toffee.Style.Dimension.percent 1.0 in
  let content_style =
    let s = Renderable.style content_node in
    let s =
      Toffee.Style.set_flex_direction Toffee.Style.Flex_direction.Column s
    in
    let s =
      Toffee.Style.set_align_self (Some Toffee.Style.Align_items.Flex_start) s
    in
    let s = Toffee.Style.set_flex_shrink 0. s in
    let s = Toffee.Style.set_min_size { width = pct100; height = pct100 } s in
    content_axes_style ~scroll_x ~scroll_y s
  in
  Renderable.set_style content_node content_style;
  (* Viewport culling (opentui ScrollBox viewportCulling, default on): only
     the children whose subtrees reach the viewport are updated, rendered and
     hit-tested each frame. Every child is laid out: the content holds the
     children that cannot change one another's layout in chunks of at most
     256 in the layout tree, so a frame that changes a child lays out the
     members of its chunk again and answers the others from their layout
     caches. *)
  Renderable.set_viewport_cull content_node true;
  (* Horizontal scroll bar: inside wrapper, below viewport *)
  let horizontal_bar =
    let bar =
      Scroll_bar.create ~parent:wrapper
        (Scroll_bar.Props.make ~orientation:`Horizontal ())
    in
    Scroll_bar.apply_style bar
      (resolve_bar_style scrollbar_style horizontal_bar_style);
    bar
  in
  (* Vertical scroll bar: inside root, right of wrapper *)
  let vertical_bar =
    let bar =
      Scroll_bar.create ~parent:node
        (Scroll_bar.Props.make ~orientation:`Vertical ())
    in
    Scroll_bar.apply_style bar
      (resolve_bar_style scrollbar_style vertical_bar_style);
    bar
  in
  if not show_scrollbars then begin
    Scroll_bar.set_visible_override vertical_bar false;
    Scroll_bar.set_visible_override horizontal_bar false
  end;
  (* Route user children to content after internal structure is built. *)
  Renderable.set_child_target node (Some content_node);
  let t =
    {
      node;
      wrapper;
      viewport = viewport_node;
      content = content_node;
      props;
      vertical_bar;
      horizontal_bar;
      scroll_x = 0;
      scroll_y = 0;
      max_scroll_x = 0;
      max_scroll_y = 0;
      content_w = 0;
      content_h = 0;
      on_scroll;
      frame_clock = 0.;
      wheel_acc_x = 0.;
      wheel_acc_y = 0.;
      auto_scroll_mouse_x = 0;
      auto_scroll_mouse_y = 0;
      cached_auto_scroll_speed = auto_scroll_speed_slow;
      auto_scroll_acc_x = 0.;
      auto_scroll_acc_y = 0.;
      auto_scrolling = false;
    }
  in
  (* Wire scroll bar change → scroll position *)
  Scroll_bar.set_on_change t.vertical_bar
    (Some (fun pos -> scroll_to_internal t ~y:pos ()));
  Scroll_bar.set_on_change t.horizontal_bar
    (Some (fun pos -> scroll_to_internal t ~x:pos ()));
  (* Wire mouse handler *)
  Renderable.on_mouse node (on_mouse t);
  (* Wire keyboard handler *)
  Renderable.set_key_behavior node
    (Some (fun ev -> if handle_key t ev then Event.Key.prevent_default ev));
  Renderable.set_focusable node true;
  (* Wire update and render callbacks. The update hook lives on the content
     node so the offset settles right before its children are culled. *)
  Renderable.set_on_update content_node
    (Some (fun _ ~dt -> update_scroll_box t ~dt));
  Renderable.set_render node (render_scroll_box t);
  Renderable.set_on_frame node (Some (fun _ ~dt -> handle_auto_scroll t dt));
  (* Wire resize notifications *)
  Renderable.set_on_resize viewport_node
    (Some (fun _ -> Renderable.request_render node));
  Renderable.set_on_resize content_node
    (Some (fun _ -> Renderable.request_render node));
  set_child_offsets t;
  Renderable.request_render node;
  t

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "ScrollBox(%s, x=%d, y=%d, cw=%d, ch=%d, vw=%d, vh=%d)"
    (Renderable.id t.node) t.scroll_x t.scroll_y t.content_w t.content_h
    (viewport_width t) (viewport_height t)
