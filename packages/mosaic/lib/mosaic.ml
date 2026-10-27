(** TEA (The Elm Architecture) runtime for mosaic. *)

open Mosaic_ui

(* External re-exports *)

module Ansi = Matrix.Ansi
module Border = Matrix.Grid.Border
module Event = Mosaic_ui.Event
module Canvas = Mosaic_ui.Canvas
module Theme = Mosaic_ui.Theme
module Scroll = Mosaic_ui.Scroll
module Keyed = Mosaic_ui.Keyed
module Editor = Mosaic_ui.Editor

module Focus = struct
  let next ~equal order current =
    match order with
    | [] -> None
    | first :: _ -> (
        match current with
        | None -> Some first
        | Some current ->
            let rec scan = function
              | [] | [ _ ] -> Some first
              | item :: (following :: _ as rest) ->
                  if equal item current then Some following else scan rest
            in
            scan order)

  let prev ~equal order current =
    match order with
    | [] -> None
    | first :: rest -> (
        let rec last item = function
          | [] -> Some item
          | item :: rest -> last item rest
        in
        match current with
        | None -> last first rest
        | Some current when equal first current -> last first rest
        | Some current ->
            let rec scan previous = function
              | [] -> Some previous
              | item :: rest ->
                  if equal item current then Some previous else scan item rest
            in
            scan first rest)
end

module Shortcut = Mosaic_ui.Shortcut

(* Geometry type aliases *)

type 'a size = 'a Toffee.Geometry.Size.t = { width : 'a; height : 'a }

type 'a rect = 'a Toffee.Geometry.Rect.t = {
  left : 'a;
  right : 'a;
  top : 'a;
  bottom : 'a;
}

type 'a point = 'a Toffee.Geometry.Point.t = { x : 'a; y : 'a }
type 'a line = 'a Toffee.Geometry.Line.t = { start : 'a; end_ : 'a }
type dimension = Toffee.Style.Dimension.t
type length_percentage = Toffee.Style.Length_percentage.t
type length_percentage_auto = Toffee.Style.Length_percentage_auto.t
type span = Mosaic_ui.Text_buffer.span = { text : string; style : Ansi.Style.t }

(* Layout enum modules *)

module Display = Toffee.Style.Display
module Position = Toffee.Style.Position
module Box_sizing = Toffee.Style.Box_sizing
module Overflow = Toffee.Style.Overflow
module Text_align = Toffee.Style.Text_align
module Flex_direction = Toffee.Style.Flex_direction
module Flex_wrap = Toffee.Style.Flex_wrap
module Align = Toffee.Style.Align_items
module Justify = Toffee.Style.Align_content
module Grid_auto_flow = Toffee.Style.Grid_auto_flow

(* Grid module *)

module Grid = struct
  type template = Toffee.Style.Grid_template_component.t

  let fr = Toffee.Style.Grid_template_component.fr
  let length = Toffee.Style.Grid_template_component.length
  let percent = Toffee.Style.Grid_template_component.percent
  let auto = Toffee.Style.Grid_template_component.auto
  let min_content = Toffee.Style.Grid_template_component.min_content
  let max_content = Toffee.Style.Grid_template_component.max_content
  let fit_content = Toffee.Style.Grid_template_component.fit_content
  let minmax = Toffee.Style.Grid_template_component.minmax

  type placement = Toffee.Style.Grid_placement.t

  let line = Toffee.Style.Grid_placement.line
  let span = Toffee.Style.Grid_placement.span
  let auto_placement = Toffee.Style.Grid_placement.auto

  let line_range s e : placement Toffee.Geometry.Line.t =
    {
      start = Toffee.Style.Grid_placement.line s;
      end_ = Toffee.Style.Grid_placement.line e;
    }

  let span_range s n : placement Toffee.Geometry.Line.t =
    {
      start = Toffee.Style.Grid_placement.line s;
      end_ = Toffee.Style.Grid_placement.span n;
    }

  type track = Toffee.Style.Track_sizing_function.t

  let track_fr = Toffee.Style.Track_sizing_function.fr
  let track_length = Toffee.Style.Track_sizing_function.length
  let track_percent = Toffee.Style.Track_sizing_function.percent
  let track_auto = Toffee.Style.Track_sizing_function.auto
  let track_min_content = Toffee.Style.Track_sizing_function.min_content
  let track_max_content = Toffee.Style.Track_sizing_function.max_content

  type area = Toffee.Style.Grid_template_area.t
end

(* Narrowed widget modules *)

module Select = struct
  type action = Mosaic_ui.Select.action =
    | Move_up
    | Move_down
    | Move_up_fast
    | Move_down_fast
    | Activate

  type 'a item = { value : 'a; presentation : Mosaic_ui.Select.item }

  let item value ?description label =
    { value; presentation = { Mosaic_ui.Select.label; description } }
end

module Tab_select = struct
  type action = Mosaic_ui.Tab_select.action =
    | Move_left
    | Move_right
    | Activate

  type 'a item = { value : 'a; presentation : Mosaic_ui.Tab_select.item }

  let item value ?(description = "") label =
    { value; presentation = { Mosaic_ui.Tab_select.label; description } }
end

module Table = struct
  include Mosaic_ui.Table

  type 'a row = { value : 'a; cells : cell array }

  let row value cells = { value; cells }
end

module Tree = Mosaic_ui.Tree
module Spinner = Mosaic_ui.Spinner
module Slider = Mosaic_ui.Slider
module Scroll_box = Mosaic_ui.Scroll_box
module Scroll_bar = Mosaic_ui.Scroll_bar
module Text = Mosaic_ui.Text
module Text_surface = Mosaic_ui.Text_surface
module Line_number = Mosaic_ui.Line_number
module Code = Mosaic_ui.Code
module Diff = Mosaic_ui.Diff
module Markdown = Mosaic_ui.Markdown
module Syntax_highlight = Mosaic_ui.Syntax_highlight
module Syntax_style = Mosaic_ui.Syntax_style
module Reconciler = Reconciler

(* Semantic callbacks produce messages; raw event handlers may ignore events. *)
type 'msg t = 'msg Vnode.t

let map (f : 'a -> 'b) (view : 'a t) : 'b t = Vnode.map f view

type 'msg mailbox = { dispatch : 'msg -> unit; send : 'msg -> unit }

let map_mailbox f mailbox =
  {
    dispatch = (fun msg -> mailbox.dispatch (f msg));
    send = (fun msg -> mailbox.send (f msg));
  }

module Cmd = struct
  type 'msg t =
    | None
    | Batch of 'msg t list
    | Perform of (('msg -> unit) -> unit)
    | Exec of (unit -> 'msg)
    | Suspend
    | Quit
    | Set_title of string
    | Copy_to_clipboard of string
    | Copy_selection
    | Query_color_scheme
    | Query_palette
    | Bell
    | Notify of { title : string; body : string }
    | Clear_selection
    | Static_commit of 'msg Vnode.t
    | Static_clear
    | Abort_paste
    | Keep_frame of bool
    | Focus_next
    | Focus_prev

  let none = None
  let batch cmds = Batch cmds
  let perform f = Perform f
  let exec f = Exec f
  let suspend = Suspend
  let quit = Quit
  let set_title title = Set_title title
  let copy_to_clipboard text = Copy_to_clipboard text
  let copy_selection = Copy_selection
  let query_color_scheme = Query_color_scheme
  let query_palette = Query_palette
  let bell = Bell
  let notify ~title ~body = Notify { title; body }
  let clear_selection = Clear_selection
  let static_commit view = Static_commit view
  let static_clear = Static_clear
  let abort_paste = Abort_paste
  let keep_frame keep = Keep_frame keep
  let focus_next = Focus_next
  let focus_prev = Focus_prev

  let rec map (f : 'a -> 'b) (cmd : 'a t) : 'b t =
    match cmd with
    | None -> None
    | Batch cmds -> Batch (List.map (map f) cmds)
    | Perform g -> Perform (fun dispatch -> g (fun msg -> dispatch (f msg)))
    | Exec g -> Exec (fun () -> f (g ()))
    | Suspend -> Suspend
    | Quit -> Quit
    | Set_title title -> Set_title title
    | Copy_to_clipboard text -> Copy_to_clipboard text
    | Copy_selection -> Copy_selection
    | Query_color_scheme -> Query_color_scheme
    | Query_palette -> Query_palette
    | Bell -> Bell
    | Notify { title; body } -> Notify { title; body }
    | Clear_selection -> Clear_selection
    | Static_commit view -> Static_commit (Vnode.map f view)
    | Static_clear -> Static_clear
    | Abort_paste -> Abort_paste
    | Keep_frame keep -> Keep_frame keep
    | Focus_next -> Focus_next
    | Focus_prev -> Focus_prev
end

module Sub = struct
  type phase = Capture | Bubble

  module Stop = struct
    type t = {
      lock : Mutex.t;
      mutable requested : bool;
      mutable on_request : (unit -> unit) list;
    }

    let create () =
      { lock = Mutex.create (); requested = false; on_request = [] }

    let requested t = Mutex.protect t.lock (fun () -> t.requested)

    let on_request t f =
      let already =
        Mutex.protect t.lock (fun () ->
            if not t.requested then t.on_request <- f :: t.on_request;
            t.requested)
      in
      if already then f ()

    let request t =
      Mutex.protect t.lock (fun () ->
          let callbacks = if t.requested then [] else List.rev t.on_request in
          t.requested <- true;
          t.on_request <- [];
          callbacks)
      |> List.iter (fun f -> f ())
  end

  type 'msg t =
    | None
    | Batch of 'msg t list
    | Every of string * float * (int -> 'msg)
    | After of string * float * 'msg
    | On_tick of string * (dt:float -> 'msg)
    | On_key of phase * (Event.key -> 'msg option)
    | On_key_release of (Matrix.Input.Keystroke.release -> 'msg option)
    | On_mouse of phase * (Event.mouse -> 'msg option)
    | On_paste of phase * (Event.paste -> 'msg option)
    | On_paste_stalled of float * 'msg
    | On_resize of (width:int -> height:int -> 'msg)
    | On_focus of 'msg
    | On_blur of 'msg
    | On_color_scheme of ([ `Dark | `Light ] -> 'msg option)
    | Worker of string * (Stop.t -> 'msg mailbox -> unit)
    | On_focus_lost of 'msg
    | On_palette of (Ansi.Palette.t -> 'msg option)

  let none = None
  let batch subs = Batch subs

  let every ~id period f =
    if id = "" || (not (Float.is_finite period)) || period <= 0. then
      invalid_arg
        "Mosaic.Sub.every: nonempty id and positive finite period required";
    Every (id, period, f)

  let after ~id delay msg =
    if id = "" || (not (Float.is_finite delay)) || delay < 0. then
      invalid_arg
        "Mosaic.Sub.after: nonempty id and nonnegative finite delay required";
    After (id, delay, msg)

  let on_tick ~id f =
    if id = "" then invalid_arg "Mosaic.Sub.on_tick: empty id";
    On_tick (id, f)

  let on_key ?(phase = Bubble) f = On_key (phase, f)

  let on_keys ?(phase = Bubble) bindings =
    On_key
      ( phase,
        fun ev ->
          if Event.Key.default_prevented ev then None
          else
            let message = Shortcut.find_opt bindings ev in
            if Option.is_some message then Event.Key.prevent_default ev;
            message )

  let on_key_release f = On_key_release f
  let on_mouse ?(phase = Bubble) f = On_mouse (phase, f)
  let on_paste ?(phase = Bubble) f = On_paste (phase, f)

  let on_paste_stalled seconds msg =
    if (not (Float.is_finite seconds)) || seconds <= 0. then
      invalid_arg "Mosaic.Sub.on_paste_stalled: positive finite time required";
    On_paste_stalled (seconds, msg)

  let on_resize f = On_resize f
  let on_focus msg = On_focus msg
  let on_blur msg = On_blur msg
  let on_color_scheme f = On_color_scheme f
  let on_focus_lost msg = On_focus_lost msg
  let on_palette f = On_palette f

  let worker ~id f =
    if id = "" then invalid_arg "Mosaic.Sub.worker: empty id";
    Worker (id, f)

  let rec map (f : 'a -> 'b) (sub : 'a t) : 'b t =
    match sub with
    | None -> None
    | Batch subs -> Batch (List.map (map f) subs)
    | Every (id, period, g) -> Every (id, period, fun k -> f (g k))
    | After (id, delay, msg) -> After (id, delay, f msg)
    | On_tick (id, g) -> On_tick (id, fun ~dt -> f (g ~dt))
    | On_key (phase, g) -> On_key (phase, fun ev -> Option.map f (g ev))
    | On_key_release g -> On_key_release (fun ev -> Option.map f (g ev))
    | On_mouse (phase, g) -> On_mouse (phase, fun ev -> Option.map f (g ev))
    | On_paste (phase, g) -> On_paste (phase, fun ev -> Option.map f (g ev))
    | On_paste_stalled (seconds, msg) -> On_paste_stalled (seconds, f msg)
    | On_resize g -> On_resize (fun ~width ~height -> f (g ~width ~height))
    | On_focus msg -> On_focus (f msg)
    | On_blur msg -> On_blur (f msg)
    | On_color_scheme g ->
        On_color_scheme (fun scheme -> Option.map f (g scheme))
    | Worker (id, g) ->
        Worker (id, fun stop mailbox -> g stop (map_mailbox f mailbox))
    | On_focus_lost msg -> On_focus_lost (f msg)
    | On_palette g -> On_palette (fun p -> Option.map f (g p))
end

type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> 'msg t;
  subscriptions : 'model -> 'msg Sub.t;
}

(* Timer state, for [every] and [after]. A timer's deadlines are on the grid
   [origin + n * interval] for [n = 1, 2, ...], in the runtime's integer
   nanoseconds, computed from the origin each time: an exact grid, which a
   clock read and compared with it reaches exactly. [fired] is the last [n]
   it fired for, so its deadline is the grid point after it; an [after]
   timer has only the first. *)
type 'msg timer = {
  id : string;
  interval : int;
  origin : int;
  mutable fired : int;
  fire : 'msg fire;
}

and 'msg fire = Every of (int -> 'msg) | After of 'msg

let timer_deadline timer =
  match timer.fire with
  | Every _ -> timer.origin + ((timer.fired + 1) * timer.interval)
  | After _ ->
      if timer.fired = 0 then timer.origin + timer.interval else max_int

(* Whether [timer] continues [prev]: the same subscription, declared again. *)
let continues prev timer =
  String.equal prev.id timer.id
  && Int.equal prev.interval timer.interval
  &&
  match (prev.fire, timer.fire) with
  | Every _, Every _ | After _, After _ -> true
  | Every _, After _ | After _, Every _ -> false

let nanoseconds seconds = Float.to_int (Float.round (seconds *. 1e9))

(* What work done off the loop sends it: a message for [update], a widget's
   request for a frame, or the exception that work raised, which ends the
   run. *)
type 'msg post =
  | Message of Sub.Stop.t option * 'msg
  | Sent of Sub.Stop.t option * 'msg * latch
  | Redraw
  | Failed of exn * Printexc.raw_backtrace

(* Whether the loop has taken a message sent with [send], guarded by the
   runtime's [sync]. *)
and latch = { mutable received : bool }

type ('model, 'msg) runtime = {
  mutable model : 'model;
  (* Messages to apply on the loop's thread, newest first. Widgets deliver
     theirs here synchronously; work done on other threads posts its
     messages through the loop. *)
  pending_msgs : 'msg list Atomic.t;
  (* Messages posted from other threads and not yet taken by the loop. *)
  posted_msgs : int Atomic.t;
  mutable view_dirty : bool;
  mutable synchronizing : bool;
  mutable delivering : bool;
  (* A frame is being drawn, from the frame step on. *)
  mutable framing : bool;
  (* The frame being drawn is early ([Matrix.early]): it ticks nothing and
     runs no element's animation. *)
  mutable early : bool;
  mutable quit : bool;
  (* Whether the program leaves its last frame when it ends
     ([Cmd.keep_frame]), and, then, whether it has quit and draws that
     frame. *)
  mutable keep : bool;
  mutable closing : bool;
  app : ('model, 'msg) app;
  matrix_app : 'msg post Matrix.app;
  process_perform : (unit -> unit) -> unit;
  loop_thread : int;
  (* The workers running, by id, and those the subscriptions declare. *)
  mutable workers : (string * Sub.Stop.t) list;
  mutable worker_subs : (string * (Sub.Stop.t -> 'msg mailbox -> unit)) list;
  (* Senders waiting for the loop to take their message wait on [taken]
     until it has, or the run is [closed]. *)
  sync : Mutex.t;
  taken : Condition.t;
  mutable closed : bool;
  renderer : Renderer.t;
  reconciler : 'msg Reconciler.t;
  mutable key_subs : (Sub.phase * (Event.key -> 'msg option)) list;
  mutable key_release_subs :
    (Matrix.Input.Keystroke.release -> 'msg option) list;
  mutable mouse_subs : (Sub.phase * (Event.mouse -> 'msg option)) list;
  mutable paste_subs : (Sub.phase * (Event.paste -> 'msg option)) list;
  mutable resize_sub : (width:int -> height:int -> 'msg) list;
  mutable tick_sub : (string * (dt:float -> 'msg)) list;
  (* When each tick subscription last ticked, by id. *)
  ticked : (string, int) Hashtbl.t;
  mutable timers : 'msg timer list;
  mutable focus_sub : 'msg list;
  mutable blur_sub : 'msg list;
  mutable focus_lost_sub : 'msg list;
  (* The view last reconciled declared a focused element. *)
  mutable had_focus : bool;
  mutable color_scheme_sub : ([ `Dark | `Light ] -> 'msg option) list;
  mutable palette_sub : (Ansi.Palette.t -> 'msg option) list;
  (* The timer armed at the earliest deadline of the timers and the
     paste stalls, and that deadline. *)
  mutable wakeup : (Matrix.timer * int) option;
  (* The bracketed paste being received, assembled for the widgets. *)
  mutable paste : Buffer.t option;
  mutable paste_stalled_subs : (float * 'msg) list;
  (* When the paste being received last received data, and when its
     silence was last checked against [paste_stalled_subs]. *)
  mutable paste_heard : int;
  mutable paste_checked : int;
  (* The focus moves asked for and not yet made, oldest first. Each is made
     on the view of the model as it stands when its turn comes. *)
  focus_moves : [ `Next | `Prev ] Queue.t;
}

(* Push [msg] for the loop's thread to apply. *)
let push_msg runtime msg =
  let rec push () =
    let cur = Atomic.get runtime.pending_msgs in
    if not (Atomic.compare_and_set runtime.pending_msgs cur (msg :: cur)) then
      push ()
  in
  push ()

(* Static output renders offscreen with an auto-height root, mirroring the
   primary-screen path: one pass at the terminal height measures the content's
   natural height (layout is independent of the grid), and only when the
   content is taller does a second pass paint it at that exact height. Width
   comes from [effective_size] and the width method from the screen, the same
   sources the live view lays out against. *)
let render_static_view runtime (view : _ t) =
  let width, _ = Matrix.effective_size runtime.matrix_app in
  let width = max 1 width in
  let _, full_height = Matrix.full_size runtime.matrix_app in
  let style =
    Toffee.Style.default
    |> Toffee.Style.set_display Toffee.Style.Display.Block
    |> Toffee.Style.set_width
         (Toffee.Style.Dimension.length (Float.of_int width))
    |> Toffee.Style.set_height Toffee.Style.Dimension.auto
  in
  let renderer =
    Renderer.create ~style
      ~width_method:
        (Matrix.Screen.width_method (Matrix.screen runtime.matrix_app))
      ()
  in
  (* Pictures take their natural size from the terminal's cells. The rest of
     the screen's capabilities stay its defaults: it only lays the rows out. *)
  Matrix.Screen.apply_capabilities (Renderer.screen renderer)
    ~explicit_width:true ~explicit_cursor_positioning:false ~hyperlinks:true
    ~colors:`Truecolor ~styling:`Truecolor ~graphics:`Blocks
    ~cell_size:(Matrix.cell_size runtime.matrix_app);
  Renderer.set_theme renderer (Renderer.theme runtime.renderer);
  let reconciler =
    Reconciler.create ~container:(Renderer.root renderer) ~deliver:ignore
  in
  Reconciler.render reconciler ~viewport_width:width view;
  let measure_height = max 1 full_height in
  Renderer.render_frame renderer ~width ~height:measure_height ~dt:0.;
  let required = Renderable.height (Renderer.root renderer) in
  if required > measure_height then
    Renderer.render_frame renderer ~width ~height:required ~dt:0.;
  let grid = Matrix.Screen.next_grid (Renderer.screen renderer) in
  let used_rows = Matrix.Grid.active_height grid in
  (grid, used_rows)

(* Runs [work] off the loop with [executor]. Its exception is the run's: the
   loop raises it, with the backtrace of where [work] raised it. *)
let off_loop runtime executor work =
  executor (fun () ->
      try work ()
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Matrix.post runtime.matrix_app (Failed (exn, bt)))

let release_senders runtime latch =
  Mutex.protect runtime.sync (fun () ->
      Option.iter (fun latch -> latch.received <- true) latch;
      Condition.broadcast runtime.taken)

let stopped = function Some stop -> Sub.Stop.requested stop | None -> false

(* The way work done off the loop gives it messages, applied on the loop's
   thread in a later turn. A worker's, with its [stop], gives nothing once
   its stop is requested, and the loop drops what it gave before and takes
   after. *)
let mailbox ?stop runtime =
  let dispatch msg =
    if not (stopped stop) then (
      Atomic.incr runtime.posted_msgs;
      Matrix.post runtime.matrix_app (Message (stop, msg)))
  in
  let send msg =
    if Thread.id (Thread.self ()) = runtime.loop_thread then
      invalid_arg "Mosaic: send on the loop's thread";
    if not (stopped stop) then (
      let latch = { received = false } in
      Atomic.incr runtime.posted_msgs;
      Matrix.post runtime.matrix_app (Sent (stop, msg, latch));
      Mutex.protect runtime.sync (fun () ->
          while not (latch.received || runtime.closed) do
            Condition.wait runtime.taken runtime.sync
          done))
  in
  { dispatch; send }

(* An exception a worker raises after its stop is requested is dropped. *)
let start_worker runtime id f =
  let stop = Sub.Stop.create () in
  off_loop runtime runtime.process_perform (fun () ->
      try f stop (mailbox ~stop runtime)
      with exn when Sub.Stop.requested stop -> ignore exn);
  (id, stop)

(* The program ends. One that leaves its last frame in [`Primary] mode draws
   it first, from the model it ends with, which the frame before may not
   show; nothing is applied meanwhile. *)
let end_program runtime =
  if runtime.keep && Matrix.mode runtime.matrix_app = `Primary then (
    if not runtime.closing then (
      runtime.closing <- true;
      Matrix.request_redraw runtime.matrix_app))
  else runtime.quit <- true

let rec process_cmd runtime (cmd : _ Cmd.t) =
  match cmd with
  | Cmd.None -> ()
  | Cmd.Batch cmds -> List.iter (process_cmd runtime) cmds
  | Cmd.Perform f ->
      off_loop runtime runtime.process_perform (fun () ->
          f (mailbox runtime).dispatch)
  | Cmd.Exec f -> push_msg runtime (Matrix.release runtime.matrix_app f)
  | Cmd.Suspend -> Matrix.stop_job runtime.matrix_app
  | Cmd.Quit -> end_program runtime
  | Cmd.Set_title title ->
      let term = Matrix.terminal runtime.matrix_app in
      Matrix.Terminal.set_title term title
  | Cmd.Copy_to_clipboard text ->
      Matrix.Terminal.copy_to_clipboard
        (Matrix.terminal runtime.matrix_app)
        text
  | Cmd.Copy_selection -> (
      match Renderer.selection_text runtime.renderer with
      | Some text when text <> "" ->
          Matrix.Terminal.copy_to_clipboard
            (Matrix.terminal runtime.matrix_app)
            text
      | Some _ | None -> ())
  | Cmd.Query_color_scheme ->
      Matrix.Terminal.query_color_scheme (Matrix.terminal runtime.matrix_app)
  | Cmd.Query_palette -> Matrix.query_palette runtime.matrix_app
  | Cmd.Bell -> Matrix.Terminal.bell (Matrix.terminal runtime.matrix_app)
  | Cmd.Notify { title; body } ->
      Matrix.Terminal.notify (Matrix.terminal runtime.matrix_app) ~title ~body
  | Cmd.Clear_selection -> Renderer.clear_selection runtime.renderer
  | Cmd.Static_commit view ->
      let grid, rows = render_static_view runtime view in
      Matrix.static_write ~rows runtime.matrix_app grid
  | Cmd.Static_clear -> Matrix.static_clear runtime.matrix_app
  | Cmd.Abort_paste -> Matrix.abort_paste runtime.matrix_app
  | Cmd.Keep_frame keep ->
      runtime.keep <- keep;
      Matrix.keep_frame runtime.matrix_app keep
  | Cmd.Focus_next -> Queue.push `Next runtime.focus_moves
  | Cmd.Focus_prev -> Queue.push `Prev runtime.focus_moves

let rec collect_subs runtime (sub : _ Sub.t) =
  match sub with
  | Sub.None -> ()
  | Sub.Batch subs -> List.iter (collect_subs runtime) subs
  | Sub.Every (id, period, f) ->
      (* The schedule is assigned in [update_subscriptions], where the current
         time and the previous timer generation are both at hand. *)
      runtime.timers <-
        {
          id;
          interval = nanoseconds period;
          origin = 0;
          fired = 0;
          fire = Every f;
        }
        :: runtime.timers
  | Sub.After (id, delay, msg) ->
      runtime.timers <-
        {
          id;
          interval = nanoseconds delay;
          origin = 0;
          fired = 0;
          fire = After msg;
        }
        :: runtime.timers
  | Sub.On_tick (id, f) -> runtime.tick_sub <- (id, f) :: runtime.tick_sub
  | Sub.On_key (phase, f) -> runtime.key_subs <- (phase, f) :: runtime.key_subs
  | Sub.On_key_release f ->
      runtime.key_release_subs <- f :: runtime.key_release_subs
  | Sub.On_mouse (phase, f) ->
      runtime.mouse_subs <- (phase, f) :: runtime.mouse_subs
  | Sub.On_paste (phase, f) ->
      runtime.paste_subs <- (phase, f) :: runtime.paste_subs
  | Sub.On_paste_stalled (seconds, msg) ->
      runtime.paste_stalled_subs <- (seconds, msg) :: runtime.paste_stalled_subs
  | Sub.On_resize f -> runtime.resize_sub <- f :: runtime.resize_sub
  | Sub.On_focus msg -> runtime.focus_sub <- msg :: runtime.focus_sub
  | Sub.On_blur msg -> runtime.blur_sub <- msg :: runtime.blur_sub
  | Sub.On_color_scheme f ->
      runtime.color_scheme_sub <- f :: runtime.color_scheme_sub
  | Sub.Worker (id, f) -> runtime.worker_subs <- (id, f) :: runtime.worker_subs
  | Sub.On_focus_lost msg ->
      runtime.focus_lost_sub <- msg :: runtime.focus_lost_sub
  | Sub.On_palette f -> runtime.palette_sub <- f :: runtime.palette_sub

(* How long a paste may be silent before the program ends it, when the
   application declares no stall of its own. *)
let default_paste_stall = nanoseconds 5.

(* The instants at which a paste in progress has been silent as long as a
   stall subscription waits, that the loop has not yet checked, each with
   its message. Without a subscription, the program ends the paste ([None])
   after [default_paste_stall]. *)
let paste_stalls runtime =
  if Option.is_none runtime.paste then []
  else
    let pending wait msg =
      let due = runtime.paste_heard + wait in
      if due > runtime.paste_checked then Some (due, msg) else None
    in
    match runtime.paste_stalled_subs with
    | [] -> Option.to_list (pending default_paste_stall None)
    | subs ->
        List.filter_map
          (fun (seconds, msg) -> pending (nanoseconds seconds) (Some msg))
          subs

(* Arm one timer at the earliest timer or paste-stall deadline, or none
   when nothing is pending. Runs after every subscription recollection, after
   every fire and as a paste receives data: each can move the earliest
   deadline. *)
let arm_wakeup runtime =
  let deadline =
    List.fold_left
      (fun acc timer -> min acc (timer_deadline timer))
      max_int runtime.timers
  in
  let deadline =
    List.fold_left
      (fun acc (due, _) -> min acc due)
      deadline (paste_stalls runtime)
  in
  match runtime.wakeup with
  | Some (_, armed) when armed = deadline -> ()
  | wakeup ->
      Option.iter
        (fun (timer, _) -> Matrix.cancel runtime.matrix_app timer)
        wakeup;
      runtime.wakeup <-
        (if deadline = max_int then None
         else Some (Matrix.at runtime.matrix_app deadline, deadline))

(* Animation frames: a tick subscription or a live element asks for the next
   frame from each frame it draws. *)
let animating runtime =
  runtime.tick_sub <> []
  || Renderable.Private.live_count (Renderer.root runtime.renderer) > 0

(* The renderer's [animate] for the frame being drawn: constants, so that a
   frame allocates no option. *)
let animate runtime = if runtime.early then Some false else None

(* A worker runs from the update that declares its id to the one that no
   longer does. *)
let update_workers runtime =
  let declared = List.rev runtime.worker_subs in
  let gone, kept =
    List.partition
      (fun (id, _) -> not (List.mem_assoc id declared))
      runtime.workers
  in
  let started =
    List.filter_map
      (fun (id, f) ->
        if List.mem_assoc id kept then None
        else Some (start_worker runtime id f))
      declared
  in
  runtime.workers <- kept @ started;
  List.iter (fun (_, stop) -> Sub.Stop.request stop) gone

let update_subscriptions runtime =
  let prev_timers = runtime.timers in
  runtime.key_subs <- [];
  runtime.key_release_subs <- [];
  runtime.mouse_subs <- [];
  runtime.paste_subs <- [];
  runtime.paste_stalled_subs <- [];
  runtime.resize_sub <- [];
  runtime.tick_sub <- [];
  runtime.timers <- [];
  runtime.worker_subs <- [];
  runtime.focus_sub <- [];
  runtime.blur_sub <- [];
  runtime.focus_lost_sub <- [];
  runtime.color_scheme_sub <- [];
  runtime.palette_sub <- [];
  collect_subs runtime (runtime.app.subscriptions runtime.model);
  runtime.key_subs <- List.rev runtime.key_subs;
  runtime.key_release_subs <- List.rev runtime.key_release_subs;
  runtime.mouse_subs <- List.rev runtime.mouse_subs;
  runtime.paste_subs <- List.rev runtime.paste_subs;
  runtime.resize_sub <- List.rev runtime.resize_sub;
  runtime.tick_sub <- List.rev runtime.tick_sub;
  runtime.focus_sub <- List.rev runtime.focus_sub;
  runtime.blur_sub <- List.rev runtime.blur_sub;
  runtime.focus_lost_sub <- List.rev runtime.focus_lost_sub;
  runtime.color_scheme_sub <- List.rev runtime.color_scheme_sub;
  runtime.palette_sub <- List.rev runtime.palette_sub;
  let ids = Hashtbl.create 8 in
  let check_id id =
    if id = "" || Hashtbl.mem ids id then
      invalid_arg ("Mosaic.Sub: empty or duplicate id: " ^ id);
    Hashtbl.add ids id ()
  in
  List.iter (fun (id, _) -> check_id id) runtime.tick_sub;
  List.iter (fun (id, _) -> check_id id) runtime.worker_subs;
  Hashtbl.filter_map_inplace
    (fun id last ->
      if List.mem_assoc id runtime.tick_sub then Some last else None)
    runtime.ticked;
  let now = Matrix.now runtime.matrix_app in
  runtime.timers <-
    List.rev_map
      (fun timer ->
        check_id timer.id;
        (match timer.fire with
        | Every _ when timer.interval <= 0 ->
            invalid_arg "Mosaic.Sub.every: period shorter than a nanosecond"
        | Every _ | After _ -> ());
        match List.find_opt (fun prev -> continues prev timer) prev_timers with
        | Some prev -> { timer with origin = prev.origin; fired = prev.fired }
        | None -> { timer with origin = now })
      runtime.timers;
  update_workers runtime;
  if animating runtime then Matrix.request_redraw runtime.matrix_app;
  arm_wakeup runtime

(* Nothing after the update that quit is applied, not even a message the
   same event gives a later listener. *)
let dispatch runtime msg =
  if not (runtime.quit || runtime.closing) then (
    let model', cmd = runtime.app.update msg runtime.model in
    runtime.view_dirty <- runtime.view_dirty || not (model' == runtime.model);
    runtime.model <- model';
    process_cmd runtime cmd;
    update_subscriptions runtime)

(* Applies the pending messages. A frame is asked for when one changed the
   model or asked to move focus, unless a frame is being drawn, which will
   show it: a message that changes nothing draws nothing. *)
let process_pending_msgs runtime =
  let rec drain () =
    match Atomic.exchange runtime.pending_msgs [] with
    | [] -> ()
    | msgs ->
        List.iter (dispatch runtime) (List.rev msgs);
        drain ()
  in
  drain ();
  if
    (runtime.view_dirty || not (Queue.is_empty runtime.focus_moves))
    && not runtime.framing
  then Matrix.request_redraw runtime.matrix_app

(* The space the tree is laid out in, for input and for frames alike. In
   [`Primary] mode it is the most rows the live region can take, every row
   of the terminal but its origin's; the region is as tall as the frame's
   content. *)
let layout_size runtime =
  match Matrix.mode runtime.matrix_app with
  | `Alt -> Matrix.effective_size runtime.matrix_app
  | `Primary ->
      let width, height = Matrix.full_size runtime.matrix_app in
      (width, max 1 (height - 1))

(* Only the loop thread runs the application and reconciler. Widget callbacks
   complete their updates before default processing continues. During a view or
   geometry refresh, callbacks enqueue instead of re-entering reconciliation.
   A focus move waits for the view of the messages applied before it, so it
   counts from the focus that view declares and can reach an element the same
   update added, as Elm's [Browser.Dom.focus] waits for the next render.
   A view that loses the focus its predecessor declared delivers the
   [on_focus_lost] messages here, before the frame is laid out, so the view
   that answers them is the one the frame shows: an early frame, drawn in the
   turn that read a key, never presents the unfocused view in between. *)
let synchronize runtime =
  if not runtime.synchronizing then begin
    runtime.synchronizing <- true;
    Fun.protect
      ~finally:(fun () -> runtime.synchronizing <- false)
      (fun () ->
        let rec settle () =
          process_pending_msgs runtime;
          if runtime.view_dirty then begin
            runtime.view_dirty <- false;
            let width, height = layout_size runtime in
            let vnode = runtime.app.view runtime.model in
            Reconciler.render runtime.reconciler ~viewport_width:(max 1 width)
              vnode;
            Renderer.set_modal runtime.renderer
              (Reconciler.modal runtime.reconciler);
            (match Reconciler.focused runtime.reconciler with
            | Some node ->
                runtime.had_focus <- true;
                ignore (Renderer.focus runtime.renderer node : bool)
            | None ->
                if runtime.had_focus then begin
                  runtime.had_focus <- false;
                  List.iter (push_msg runtime) runtime.focus_lost_sub
                end;
                Renderer.blur runtime.renderer);
            Renderer.prepare_layout runtime.renderer ~width ~height;
            settle ()
          end
          else if not (Queue.is_empty runtime.focus_moves) then begin
            let direction = Queue.pop runtime.focus_moves in
            ignore (Renderer.request_focus runtime.renderer direction : bool);
            settle ()
          end
        in
        settle ())
  end

let deliver runtime msg =
  push_msg runtime msg;
  if not runtime.delivering then synchronize runtime

(* An event goes from the application's [Capture] listeners down its path
   and back up to the application's [Bubble] listeners, which see it unless
   an element stopped it. The application is one node: its listeners of a
   phase all see what reaches the phase, in declaration order. *)
let listen runtime ~stopped listeners dispatch event =
  let phase p =
    List.iter
      (fun (q, f) -> if q = p then Option.iter (deliver runtime) (f event))
      listeners
  in
  phase Sub.Capture;
  dispatch event;
  if not (stopped event) then phase Sub.Bubble

let handle_resize runtime ~width ~height =
  runtime.view_dirty <- true;
  List.iter (fun f -> push_msg runtime (f ~width ~height)) runtime.resize_sub;
  synchronize runtime

(* Each tick subscription ticks once per frame, with the time since its own
   previous tick: none on its first, whatever the frames before it. With no
   subscription it allocates nothing, so an early frame, which ticks nothing,
   and a grid frame cost the same. *)
let handle_tick runtime =
  match runtime.tick_sub with
  | [] -> ()
  | subs ->
      let now = Matrix.now runtime.matrix_app in
      List.iter
        (fun (id, f) ->
          let last =
            Option.value ~default:now (Hashtbl.find_opt runtime.ticked id)
          in
          Hashtbl.replace runtime.ticked id now;
          push_msg runtime (f ~dt:(Float.of_int (now - last) *. 1e-9)))
        subs

(* The timers whose deadline the turn reached fire once each, in the order of
   their deadlines: an [every] timer with the periods that ended since it
   last fired, which is more than one after a stall, an [after] timer, and a
   paste stall. *)
let handle_timers runtime =
  let now = Matrix.now runtime.matrix_app in
  let timers =
    List.filter_map
      (fun timer ->
        let deadline = timer_deadline timer in
        if deadline > now then None
        else
          Some
            ( deadline,
              fun () ->
                push_msg runtime
                  (match timer.fire with
                  | Every f ->
                      let ended = (now - timer.origin) / timer.interval in
                      let periods = ended - timer.fired in
                      timer.fired <- ended;
                      f periods
                  | After msg ->
                      timer.fired <- 1;
                      msg) ))
      runtime.timers
  in
  let stalls =
    List.filter_map
      (fun (due, msg) ->
        if due > now then None
        else
          Some
            ( due,
              fun () ->
                match msg with
                | Some msg -> push_msg runtime msg
                | None -> Matrix.abort_paste runtime.matrix_app ))
      (paste_stalls runtime)
  in
  runtime.paste_checked <- now;
  List.stable_sort (fun (a, _) (b, _) -> Int.compare a b) (timers @ stalls)
  |> List.iter (fun (_, fire) -> fire ())

(* The paste in progress received data: its silence starts again. *)
let paste_heard runtime =
  let now = Matrix.now runtime.matrix_app in
  runtime.paste_heard <- now;
  runtime.paste_checked <- now;
  arm_wakeup runtime

let dispatch_paste runtime text =
  let path = Renderer.focus_path runtime.renderer in
  listen runtime ~stopped:Event.Paste.propagation_stopped runtime.paste_subs
    (Renderer.dispatch_paste runtime.renderer path)
    (Event.Paste.of_text text)

(* The root's theme is the default of the terminal's palette, and the
   subscribers are given it. *)
let take_palette runtime p =
  Renderer.set_theme runtime.renderer (Theme.default p);
  List.iter (fun f -> Option.iter (deliver runtime) (f p)) runtime.palette_sub

(* Tab and Shift+Tab, with no other modifier, move focus; Escape alone
   closes the modal. *)
let key_default (key : Matrix.Input.Keystroke.t) =
  let mods = key.mods in
  let chord = mods.ctrl || mods.alt || mods.super || mods.hyper || mods.meta in
  match key.key with
  | Tab when not chord ->
      if mods.shift then Some (`Focus `Prev) else Some (`Focus `Next)
  | Escape when not (chord || mods.shift) -> Some `Close
  | _ -> None

(* A paste arrives in pieces and reaches widgets whole, once it completes; a
   cut paste is dropped. Text a terminal reports without a key, such as an
   input method's, is inserted as a paste is. *)
let handle_input runtime (input : Matrix.event) =
  synchronize runtime;
  (match input with
  | Focus_in -> List.iter (deliver runtime) runtime.focus_sub
  | Focus_out -> List.iter (deliver runtime) runtime.blur_sub
  | Color_scheme scheme ->
      (* The user may have changed the palette with the scheme. *)
      Matrix.query_palette runtime.matrix_app;
      List.iter
        (fun f -> Option.iter (deliver runtime) (f scheme))
        runtime.color_scheme_sub
  | Key key -> (
      let path = Renderer.focus_path runtime.renderer in
      let event = Event.Key.of_input key in
      listen runtime ~stopped:Event.Key.propagation_stopped runtime.key_subs
        (Renderer.dispatch_key runtime.renderer path)
        event;
      (* A key's default action comes after every listener, unless one
         prevented it. Ctrl+C quits: in raw mode it is a key, and its default
         is the program's. Tab and Shift+Tab move focus, and Escape is a
         request to close the modal, as in HTML's dialog. *)
      if not (Event.Key.default_prevented event) then
        if Matrix.Input.Keystroke.is_interrupt key then end_program runtime
        else
          match key_default key with
          | Some (`Focus direction) -> Queue.push direction runtime.focus_moves
          | Some `Close ->
              ignore (Reconciler.request_close runtime.reconciler : bool)
          | None -> ())
  | Key_release release ->
      List.iter
        (fun f -> Option.iter (deliver runtime) (f release))
        runtime.key_release_subs
  | Mouse mouse ->
      let path, event = Renderer.mouse_path runtime.renderer mouse in
      listen runtime ~stopped:Event.Mouse.propagation_stopped runtime.mouse_subs
        (Renderer.dispatch_mouse runtime.renderer path)
        event
  | Paste_start ->
      runtime.paste <- Some (Buffer.create 256);
      paste_heard runtime
  | Paste_data data ->
      Option.iter (fun b -> Buffer.add_string b data) runtime.paste;
      paste_heard runtime
  | Paste_end `Complete ->
      Option.iter
        (fun b -> dispatch_paste runtime (Buffer.contents b))
        runtime.paste;
      runtime.paste <- None;
      arm_wakeup runtime
  | Paste_end `Cut ->
      runtime.paste <- None;
      arm_wakeup runtime
  | Text text -> dispatch_paste runtime text
  | Resize (width, height) -> handle_resize runtime ~width ~height
  | Palette (Some p) -> take_palette runtime p
  | Palette None | Reply _ | Invalid _ -> ());
  synchronize runtime;
  Matrix.request_redraw runtime.matrix_app

let render runtime = synchronize runtime

(* A running program, its model hidden: the run that is Mosaic's, how the
   program acts on a step of it, whether it is over, and how messages reach
   it from elsewhere. *)
type 'msg running = {
  matrix_app : 'msg post Matrix.app;
  handle : 'msg post Matrix.step -> unit;
  over : unit -> bool;
  dispatch_msg : 'msg -> unit;
}

let program matrix_app ~process_perform app (model, init_cmd) f =
  let renderer_style =
    match Matrix.mode matrix_app with
    | `Primary ->
        Some
          (Toffee.Style.default
          |> Toffee.Style.set_width (Toffee.Style.Dimension.pct 100.)
          |> Toffee.Style.set_height Toffee.Style.Dimension.auto)
    | `Alt -> None
  in
  let rendering = Atomic.make false in
  (* A widget may ask for a frame from elsewhere than a step, as a
     highlighter's work does when it finishes, on another thread or on the
     loop's between two steps: that request goes through the loop, which
     takes it as a step of its own. *)
  let loop_thread = Thread.id (Thread.self ()) in
  let in_step = ref false in
  let redraw () = Matrix.request_redraw matrix_app in
  let renderer =
    Renderer.create ?style:renderer_style ~screen:(Matrix.screen matrix_app)
      ~request_render:(fun () ->
        if not (Atomic.get rendering) then
          if !in_step && Thread.id (Thread.self ()) = loop_thread then redraw ()
          else Matrix.post matrix_app Redraw)
      ~executor:process_perform ()
  in
  let container = Renderer.root renderer in
  (* The reconciler gives the loop the messages of the view's handlers. *)
  let deliver_to = ref ignore in
  let reconciler =
    Reconciler.create ~container ~deliver:(fun msg -> !deliver_to msg)
  in
  let runtime =
    {
      model;
      pending_msgs = Atomic.make [];
      posted_msgs = Atomic.make 0;
      view_dirty = true;
      synchronizing = false;
      delivering = false;
      framing = false;
      early = false;
      quit = false;
      keep = true;
      closing = false;
      app;
      matrix_app;
      process_perform;
      loop_thread;
      workers = [];
      worker_subs = [];
      sync = Mutex.create ();
      taken = Condition.create ();
      closed = false;
      renderer;
      reconciler;
      key_subs = [];
      mouse_subs = [];
      paste_subs = [];
      key_release_subs = [];
      paste = None;
      paste_stalled_subs = [];
      paste_heard = 0;
      paste_checked = 0;
      resize_sub = [];
      tick_sub = [];
      ticked = Hashtbl.create 4;
      timers = [];
      focus_sub = [];
      blur_sub = [];
      focus_lost_sub = [];
      had_focus = false;
      color_scheme_sub = [];
      palette_sub = [];
      wakeup = None;
      focus_moves = Queue.create ();
    }
  in
  deliver_to := deliver runtime;
  process_cmd runtime init_cmd;
  update_subscriptions runtime;

  (* A live element asks for frames while it is mounted. *)
  let root = Renderer.root renderer in
  Renderable.Private.set_on_live_count_change root
    (Some
       (fun _node -> if Renderable.Private.live_count root > 0 then redraw ()));

  (* Seconds since the previous frame on the grid, which [on_tick] and the
     widgets' frame callbacks receive. An early frame, drawn for input before
     the grid's next one ([Matrix.early]), runs neither, so animations keep
     the grid's rate while the user types. *)
  let last_frame = ref (Matrix.now matrix_app) in
  let frame_delta = ref 0. in
  (* In [`Primary] mode the live region takes the rows of the frame before it
     is drawn, so the frame is laid out first, against the terminal's height,
     the most rows the region can have, and drawn once the region has its
     rows. Messages the layout delivers wait for the next frame, as those of a
     paint do. *)
  let primary_required_rows () =
    render runtime;
    let width, height = layout_size runtime in
    runtime.delivering <- true;
    Fun.protect
      ~finally:(fun () -> runtime.delivering <- false)
      (fun () ->
        Renderer.layout_frame ?animate:(animate runtime) runtime.renderer ~width
          ~height ~dt:!frame_delta);
    max 1 (Renderable.height (Renderer.root runtime.renderer))
  in
  let finish_render () =
    (* Reconciliation invalidations are consumed by this frame. A worker or
       widget may still request a follow-up while it renders, and a message
       delivered during the frame waits for the next one. Clear the gate
       before checking so a concurrent notification cannot be lost. *)
    Atomic.set rendering false;
    if
      Renderer.needs_render runtime.renderer
      || Atomic.get runtime.pending_msgs <> []
      || animating runtime
    then redraw ()
  in
  (* The renderer builds directly into the runtime's screen, which
     Matrix.draw presents with a single diff. The frame's hardware scroll hint
     goes with it, so a scrolled viewport diffs only its revealed rows. *)
  let scroll_hint () =
    match Renderer.take_scroll_hint runtime.renderer with
    | Some hint -> Matrix.set_scroll_hint runtime.matrix_app hint
    | None -> ()
  in
  let render_frame () =
    match Matrix.mode runtime.matrix_app with
    | `Alt ->
        render runtime;
        Matrix.draw runtime.matrix_app (fun _grid ->
            let width, height = Matrix.size runtime.matrix_app in
            Renderer.render_frame ?animate:(animate runtime) runtime.renderer
              ~width ~height ~dt:!frame_delta;
            scroll_hint ())
    | `Primary ->
        let rows = primary_required_rows () in
        Matrix.draw runtime.matrix_app ~primary_required_rows:rows (fun _grid ->
            Renderer.paint_frame runtime.renderer
              ~height:(snd (Matrix.size runtime.matrix_app));
            scroll_hint ())
  in
  let frame () =
    let now = Matrix.now matrix_app in
    runtime.early <- Matrix.early matrix_app;
    let dt = Float.of_int (now - !last_frame) *. 1e-9 in
    if not runtime.early then last_frame := now;
    runtime.framing <- true;
    (* The frame renders what its ticks and the messages it applies change,
       so the requests to render they make are this frame's. *)
    Atomic.set rendering true;
    if not runtime.early then handle_tick runtime;
    synchronize runtime;
    frame_delta := if runtime.early then 0. else dt;
    runtime.delivering <- true;
    Fun.protect
      ~finally:(fun () ->
        runtime.delivering <- false;
        runtime.framing <- false;
        finish_render ())
      render_frame
  in
  let timer fired =
    match runtime.wakeup with
    | Some (armed, _) when armed = fired ->
        runtime.wakeup <- None;
        handle_timers runtime;
        synchronize runtime;
        (* A fired timer moved its deadline even when no message re-collected
           subscriptions, so the wakeup re-arms here. *)
        arm_wakeup runtime
    | _ -> ()
  in
  let handle : _ Matrix.step -> unit = function
    | Stopped -> runtime.quit <- true
    (* A terminal that gives no palette changes nothing to draw. *)
    | Event (Palette None) -> ()
    | Event input -> if not runtime.closing then handle_input runtime input
    | Timer fired -> if not runtime.closing then timer fired
    | Posted (Message (stop, msg)) ->
        (* Applied now, so the turn's budget counts its update. *)
        Atomic.decr runtime.posted_msgs;
        if not (stopped stop) then (
          push_msg runtime msg;
          process_pending_msgs runtime)
    | Posted Redraw -> redraw ()
    | Posted (Sent (stop, msg, latch)) ->
        Atomic.decr runtime.posted_msgs;
        Fun.protect
          ~finally:(fun () -> release_senders runtime (Some latch))
          (fun () ->
            if not (stopped stop) then (
              push_msg runtime msg;
              process_pending_msgs runtime))
    | Posted (Failed (exn, bt)) -> Printexc.raise_with_backtrace exn bt
    | Frame ->
        frame ();
        if runtime.closing then runtime.quit <- true
  in
  (* The run's end stops the workers and the senders' waits. *)
  let finish () =
    Mutex.protect runtime.sync (fun () -> runtime.closed <- true);
    release_senders runtime None;
    List.iter
      (fun (_, stop) -> try Sub.Stop.request stop with _ -> ())
      runtime.workers;
    Reconciler.unmount runtime.reconciler
  in
  Fun.protect ~finally:finish (fun () ->
      Matrix.query_palette matrix_app;
      let width, height = Matrix.size matrix_app in
      handle_resize runtime ~width ~height;
      f
        {
          matrix_app;
          handle =
            (fun step ->
              in_step := true;
              handle step;
              in_step := false);
          over = (fun () -> runtime.quit);
          dispatch_msg = (mailbox runtime).dispatch;
        })

(* The default executor. Work runs on systhreads of one helper domain, so
   the loop's domain runs the loop alone: a systhread on the loop's domain
   shares its lock, and work that computes would hold the loop up to a tick
   at each of its blocking calls. The domain starts with the first work and
   ends once it has none, which also stops the tick thread that systhreads
   start; work after that starts another domain, which joins the one before
   it as it ends, off the loop. *)
module Helper = struct
  type t = {
    lock : Mutex.t;
    changed : Condition.t;
    jobs : (unit -> unit) Queue.t;
    (* Jobs started and not finished. *)
    mutable running : int;
    (* Whether the domain serves: it takes jobs until it has none. *)
    mutable serving : bool;
    mutable domain : unit Domain.t option;
  }

  let create () =
    {
      lock = Mutex.create ();
      changed = Condition.create ();
      jobs = Queue.create ();
      running = 0;
      serving = false;
      domain = None;
    }

  let finished t =
    Mutex.protect t.lock (fun () ->
        t.running <- t.running - 1;
        Condition.signal t.changed)

  (* The helper domain's main thread, with [t.lock] held. *)
  let rec serve t =
    match Queue.take_opt t.jobs with
    | Some job ->
        t.running <- t.running + 1;
        let run () = Fun.protect job ~finally:(fun () -> finished t) in
        (match Thread.create run () with
        | (_ : Thread.t) -> ()
        | exception _ ->
            (* No thread can start: the job runs on this one, still off the
               loop. *)
            Mutex.unlock t.lock;
            run ();
            Mutex.lock t.lock);
        serve t
    | None when t.running > 0 ->
        Condition.wait t.changed t.lock;
        serve t
    | None -> t.serving <- false

  let submit t job =
    Mutex.protect t.lock (fun () ->
        Queue.push job t.jobs;
        if t.serving then Condition.signal t.changed
        else
          let previous = t.domain in
          t.serving <- true;
          t.domain <-
            Some
              (Domain.spawn (fun () ->
                   Mutex.protect t.lock (fun () -> serve t);
                   Option.iter Domain.join previous)))

  (* At the run's end: the domain that ended is joined; one still serving
     work that has not returned is left to it. *)
  let close t =
    let ended =
      Mutex.protect t.lock (fun () ->
          if t.serving then None
          else
            let domain = t.domain in
            t.domain <- None;
            domain)
    in
    Option.iter Domain.join ended
end

let start ?input ?output ?env ?stop ?config ?process_perform app f =
  let config =
    match config with Some config -> config | None -> Matrix.config ()
  in
  let init = app.init () in
  let within process_perform =
    Matrix.run ?input ?output ?env ?stop ~config (fun matrix_app ->
        program matrix_app ~process_perform app init f)
  in
  match process_perform with
  | Some process_perform -> within process_perform
  | None ->
      let helper = Helper.create () in
      Fun.protect
        ~finally:(fun () -> Helper.close helper)
        (fun () -> within (Helper.submit helper))

let step r ~now =
  if r.over () then `Stopped
  else
    match Matrix.step r.matrix_app ~now with
    | None -> `Idle
    | Some step ->
        r.handle step;
        if r.over () then `Stopped else `Stepped

let fds r = Matrix.fds r.matrix_app
let deadline r = Matrix.deadline r.matrix_app
let dispatch r msg = r.dispatch_msg msg

(* [step] waiting with [Matrix.next], which allocates nothing while the run
   is idle. *)
let run ?input ?output ?env ?stop ?config ?process_perform app =
  start ?input ?output ?env ?stop ?config ?process_perform app (fun r ->
      while not (r.over ()) do
        r.handle (Matrix.next r.matrix_app)
      done)

let empty = Vnode.empty
let fragment = Vnode.fragment
let embed = Vnode.embed
let viewport_switch = Vnode.viewport_switch
let memo = Vnode.memo
let keyed = Vnode.keyed

(* [override v set layout] is [layout] with [set v] applied, if [v] is given. *)
let override v set layout =
  match v with None -> layout | Some v -> set v layout

(* An element's layout is [layout], or the default, with the fields its
   layout labels give. Given no layout label, the element uses [layout], or
   [Toffee.Style.default], itself, so a layout built once costs nothing per
   element and the reconciler finds it unchanged by comparing it
   physically. A label applies its field's setter to [layout], which keeps
   the fields no label sets, such as the box generation mode. *)
let layout_style ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size ?aspect_ratio
    ?margin ?padding ?border_width ?gap ?align_items ?align_self ?align_content
    ?justify_items ?justify_self ?justify_content ?flex_direction ?flex_wrap
    ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column () =
  let no_label =
    match
      ( display,
        box_sizing,
        position,
        overflow,
        scrollbar_width,
        text_align,
        inset,
        size,
        min_size,
        max_size,
        aspect_ratio,
        margin,
        padding,
        border_width,
        gap,
        align_items,
        align_self,
        align_content,
        justify_items,
        justify_self,
        justify_content,
        flex_direction,
        flex_wrap,
        flex_grow,
        flex_shrink,
        flex_basis,
        grid_template_rows,
        grid_template_columns,
        grid_auto_rows,
        grid_auto_columns,
        grid_auto_flow,
        grid_template_areas,
        grid_template_column_names,
        grid_template_row_names,
        grid_row,
        grid_column )
    with
    | ( None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None ) ->
        true
    | _ -> false
  in
  match layout with
  | Some layout when no_label -> layout
  | None when no_label -> Toffee.Style.default
  | None ->
      Toffee.Style.make ?display ?box_sizing ?position ?overflow
        ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
        ?aspect_ratio ?margin ?padding ?border:border_width ?gap ?align_items
        ?align_self ?align_content ?justify_items ?justify_self ?justify_content
        ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
        ?grid_template_rows ?grid_template_columns ?grid_auto_rows
        ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
        ?grid_template_column_names ?grid_template_row_names ?grid_row
        ?grid_column ()
  | Some layout ->
      layout
      |> override display Toffee.Style.set_display
      |> override box_sizing Toffee.Style.set_box_sizing
      |> override position Toffee.Style.set_position
      |> override overflow Toffee.Style.set_overflow
      |> override scrollbar_width Toffee.Style.set_scrollbar_width
      |> override text_align Toffee.Style.set_text_align
      |> override inset Toffee.Style.set_inset
      |> override size Toffee.Style.set_size
      |> override min_size Toffee.Style.set_min_size
      |> override max_size Toffee.Style.set_max_size
      |> override aspect_ratio (fun v l ->
          Toffee.Style.set_aspect_ratio (Some v) l)
      |> override margin Toffee.Style.set_margin
      |> override padding Toffee.Style.set_padding
      |> override border_width Toffee.Style.set_border
      |> override gap Toffee.Style.set_gap
      |> override align_items (fun v l ->
          Toffee.Style.set_align_items (Some v) l)
      |> override align_self (fun v l -> Toffee.Style.set_align_self (Some v) l)
      |> override align_content (fun v l ->
          Toffee.Style.set_align_content (Some v) l)
      |> override justify_items (fun v l ->
          Toffee.Style.set_justify_items (Some v) l)
      |> override justify_self (fun v l ->
          Toffee.Style.set_justify_self (Some v) l)
      |> override justify_content (fun v l ->
          Toffee.Style.set_justify_content (Some v) l)
      |> override flex_direction Toffee.Style.set_flex_direction
      |> override flex_wrap Toffee.Style.set_flex_wrap
      |> override flex_grow Toffee.Style.set_flex_grow
      |> override flex_shrink Toffee.Style.set_flex_shrink
      |> override flex_basis Toffee.Style.set_flex_basis
      |> override grid_template_rows Toffee.Style.set_grid_template_rows
      |> override grid_template_columns Toffee.Style.set_grid_template_columns
      |> override grid_auto_rows Toffee.Style.set_grid_auto_rows
      |> override grid_auto_columns Toffee.Style.set_grid_auto_columns
      |> override grid_auto_flow Toffee.Style.set_grid_auto_flow
      |> override grid_template_areas Toffee.Style.set_grid_template_areas
      |> override grid_template_column_names
           Toffee.Style.set_grid_template_column_names
      |> override grid_template_row_names
           Toffee.Style.set_grid_template_row_names
      |> override grid_row Toffee.Style.set_grid_row
      |> override grid_column Toffee.Style.set_grid_column

let box ?key ?layout ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?inert ?on_mouse ?on_key
    ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?border
    ?border_style ?border_sides ?border_color ?focused_border_color ?background
    ?fill ?title ?title_alignment children =
  (* The box owns its border widths, so [border_width] goes to the box. *)
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.box
    (Mosaic_ui.Box.Props.make ?inert ?border ?border_style ?border_sides
       ?border_width ?border_color ?focused_border_color ?background ?fill
       ?title ?title_alignment ())
    children

(* In the top layer a modal is laid out against the root, which its size
   fills, and centres its child, as HTML centres a modal dialog in the
   viewport. In its parent's layout it is out of flow. *)
let modal_style =
  let full = Toffee.Style.Dimension.percent 1. in
  Toffee.Style.default
  |> Toffee.Style.set_position Toffee.Style.Position.Absolute
  |> Toffee.Style.set_size { width = full; height = full }
  |> Toffee.Style.set_align_items (Some Toffee.Style.Align_items.Center)
  |> Toffee.Style.set_justify_content (Some Toffee.Style.Align_content.Center)

let modal ?key ?on_close child =
  Vnode.element ?key ~style:modal_style ~kind:Kinds.modal
    ?on:(Option.map (fun message () -> Some message) on_close)
    Mosaic_ui.Box.Props.default [ child ]

let themed = Vnode.themed

(* The handlers of the widgets that report more than one event. *)
let choice on_change on_activate = function
  | Kinds.Change value -> Some (on_change value)
  | Activate value -> Option.map (fun f -> f value) on_activate

let edit on_change on_submit = function
  | Kinds.Edit editor -> Some (on_change editor)
  | Submit text -> Option.map (fun f -> f text) on_submit

let text ?key ?layout ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?style ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key
    ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?text_style ?wrap
    ?selectable ?selection_bg ?selection_fg ?tab_width ?truncate ?on_selection
    content =
  let layout_style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  let text_style =
    match text_style with Some _ -> text_style | None -> style
  in
  Vnode.element ?key ~style:layout_style ?visible ?z_index ?opacity ?focused
    ?buffered ?live ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request
    ?on_size ~kind:Kinds.text ?on:on_selection
    (Mosaic_ui.Text.Props.make ~content ?text_style ?wrap ?selectable
       ?selection_bg ?selection_fg ?tab_width ?truncate ())
    []

let rich_text ?key ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?style ?visible ?z_index ?opacity ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused
    ?text_style ?wrap ?selectable ?selection_bg ?selection_fg ?tab_width
    ?truncate ?on_selection content =
  let layout_style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  let text_style =
    match text_style with Some _ -> text_style | None -> style
  in
  Vnode.element ?key ~style:layout_style ?visible ?z_index ?opacity ?focused
    ?buffered ?live ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request
    ?on_size ~kind:Kinds.text ?on:on_selection
    (Mosaic_ui.Text.Props.make_rich ?text_style ?wrap ?selectable ?selection_bg
       ?selection_fg ?tab_width ?truncate content)
    []

let button ?key ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?buffered ?live
    ?on_focus_request ?on_size ?(focused = false) ?border ?border_style
    ?border_sides ?border_color ?focused_border_color ?background ?fill ?title
    ?title_alignment ?text_style ?(disabled = false) ~on_activate label =
  let text_style = Option.value text_style ~default:Ansi.Style.default in
  let text_style =
    if disabled then Ansi.Style.with_dim true text_style
    else if focused then Ansi.Style.with_inverse true text_style
    else text_style
  in
  let on_focus_request = if disabled then None else on_focus_request in
  let on_key event =
    if
      (not disabled)
      && (not (Event.Key.default_prevented event))
      && (not (Event.Key.data event).repeat)
      && (Shortcut.matches Shortcut.enter event
         || Shortcut.matches Shortcut.space event)
    then begin
      Event.Key.prevent_default event;
      Event.Key.stop_propagation event;
      Some on_activate
    end
    else None
  in
  let on_click event =
    if disabled || Event.Mouse.default_prevented event then None
    else begin
      Event.Mouse.prevent_default event;
      Some on_activate
    end
  in
  box ?key ?layout ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_focus_request ?on_size
    ~focused:(focused && not disabled) ?border ?border_style ?border_sides
    ?border_color ?focused_border_color ?background ?fill ?title
    ?title_alignment ~on_key ~on_click
    [ text ~text_style ~selectable:false label ]

let slider ?key ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?orientation
    ?min ?max ?viewport_size ?track_color ?thumb_color ~state ~on_change () =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.slider
    ~on:(fun value -> Some (on_change value))
    (Mosaic_ui.Slider.Props.make ?orientation ~value:state ?min ?max
       ?viewport_size ?track_color ?thumb_color ())
    []

let input ?key ?layout ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?placeholder ?text_color
    ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?selectable ?show_cursor ?key_bindings
    ~on_change ?on_submit ~state () =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.input ~on:(edit on_change on_submit)
    (Mosaic_ui.Text_input.Props.make ~state ?placeholder ?text_color
       ?background_color ?focused_text_color ?focused_background_color
       ?placeholder_color ?selection_color ?selection_fg ?cursor_style
       ?cursor_color ?cursor_blinking ?selectable ?show_cursor ?key_bindings ())
    []

(* The position of the first of [items] whose value is [selected], or [-1]. *)
let rec position ~equal ~value selected index = function
  | [] -> -1
  | item :: items ->
      if equal (value item) selected then index
      else position ~equal ~value selected (index + 1) items

let selection_index ~equal ~value selected items =
  match selected with
  | None -> -1
  | Some selected -> position ~equal ~value selected 0 items

(* A select, a tab select and a table take their options or rows from the
   application's list through these functions, which are the same on every
   declaration. A widget given the same list as before then neither converts
   nor compares it ([Mosaic_ui.Select.Props.with_options_of]). *)
let select_presentation item = item.Select.presentation
let tab_presentation item = item.Tab_select.presentation
let row_cells item = item.Table.cells

let select ?key ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?background
    ?text_color ?focused_background ?focused_text_color ?selected_background
    ?selected_text_color ?description_color ?selected_description_color
    ?show_description ?show_scroll_indicator ?wrap_selection ?key_bindings
    ?item_spacing ?fast_scroll_step ~on_change ?on_activate ~equal ~selected
    options =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  let selected_index =
    selection_index ~equal
      ~value:(fun item -> item.Select.value)
      selected options
  in
  let value index = (List.nth options index).Select.value in
  let on_change index = on_change (value index) in
  let on_activate = Option.map (fun f index -> f (value index)) on_activate in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.select
    ~on:(choice on_change on_activate)
    (Mosaic_ui.Select.Props.make ~selected_index ?background ?text_color
       ?focused_background ?focused_text_color ?selected_background
       ?selected_text_color ?description_color ?selected_description_color
       ?show_description ?show_scroll_indicator ?wrap_selection ?key_bindings
       ?item_spacing ?fast_scroll_step ()
    |> Mosaic_ui.Select.Props.with_options_of select_presentation options)
    []

let tab_select ?key ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?tab_width
    ?background ?text_color ?focused_background ?focused_text_color
    ?selected_background ?selected_text_color ?description_color
    ?selected_description_color ?show_underline ?show_description
    ?show_scroll_arrows ?wrap_selection ?key_bindings ~on_change ?on_activate
    ~equal ~selected options =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  let selected =
    selection_index ~equal
      ~value:(fun item -> item.Tab_select.value)
      selected options
  in
  let value index = (List.nth options index).Tab_select.value in
  let on_change index = on_change (value index) in
  let on_activate = Option.map (fun f index -> f (value index)) on_activate in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.tab_select
    ~on:(choice on_change on_activate)
    (Mosaic_ui.Tab_select.Props.make ~options:[] ~selected ?tab_width
       ?background ?text_color ?focused_background ?focused_text_color
       ?selected_background ?selected_text_color ?description_color
       ?selected_description_color ?show_underline ?show_description
       ?show_scroll_arrows ?wrap_selection ?key_bindings ()
    |> Mosaic_ui.Tab_select.Props.with_options_of tab_presentation options)
    []

let canvas ?key ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused draw =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.canvas draw []

let picture ?key ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused p =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.picture p []

let spinner ?key ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?frame_set
    ?color () =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered
    ?live:(match live with None -> Some true | live -> live)
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.spinner
    (Mosaic_ui.Spinner.Props.make ?frame_set ?color ())
    []

let progress_bar ?key ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?value ?min
    ?max ?orientation ?filled_color ?empty_color () =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.progress_bar
    (Mosaic_ui.Progress_bar.Props.make ?value ?min ?max ?orientation
       ?filled_color ?empty_color ())
    []

let scroll_bar ?key ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?orientation
    ?show_arrows ?track_color ?thumb_color ?arrow_fg ?arrow_bg ~on_change ~state
    ~scroll_size ~viewport_size () =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.scroll_bar
    ~on:(fun position -> Some (on_change position))
    (Mosaic_ui.Scroll_bar.Props.make ~position:state ~scroll_size ~viewport_size
       ?orientation ?show_arrows ?track_color ?thumb_color ?arrow_fg ?arrow_bg
       ())
    []

let scroll_box ?key ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?scroll_x
    ?scroll_y ?background ?scroll_accel ?show_scrollbars ?scrollbar_style
    ?vertical_bar_style ?horizontal_bar_style ~on_scroll ~scroll children =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.scroll_box
    ~on:(fun scroll -> Some (on_scroll scroll))
    (Mosaic_ui.Scroll_box.Props.make ~scroll ?scroll_x ?scroll_y ?background
       ?scroll_accel ?show_scrollbars ?scrollbar_style ?vertical_bar_style
       ?horizontal_bar_style ())
    children

let textarea ?key ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?annotations
    ?syntax ?ghost_text ?ghost_text_color ?placeholder ?wrap ?text_color
    ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?selectable ?show_cursor ?key_bindings
    ~on_change ?on_submit ~state () =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.textarea ~on:(edit on_change on_submit)
    (Mosaic_ui.Textarea.Props.make ~state ?annotations ?syntax ?ghost_text
       ?ghost_text_color ?placeholder ?wrap ?text_color ?background_color
       ?focused_text_color ?focused_background_color ?placeholder_color
       ?selection_color ?selection_fg ?cursor_style ?cursor_color
       ?cursor_blinking ?selectable ?show_cursor ?key_bindings ())
    []

let code ?key ?layout ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?syntax ?conceal ?text_style
    ?wrap ?tab_width ?truncate ?selectable ?selection_bg ?selection_fg
    ?on_selection content =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.code ?on:on_selection
    (Mosaic_ui.Code.Props.make ~content ?syntax ?conceal ?text_style ?wrap
       ?tab_width ?truncate ?selectable ?selection_bg ?selection_fg ())
    []

let line_number ?key ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?fg ?bg
    ?min_width ?padding_right ?show_line_numbers ?line_number_offset
    ?line_colors ?line_signs ?line_numbers ?hidden_line_numbers child =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.line_number
    (Mosaic_ui.Line_number.Props.make ?fg ?bg ?min_width ?padding_right
       ?show_line_numbers ?line_number_offset ?line_colors ?line_signs
       ?line_numbers ?hidden_line_numbers ())
    [ child ]

let diff ?key ?layout ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?view ?theme ?highlight
    ?line_highlights ?line_spans ?line_signs ?show_line_numbers ?wrap
    ?selectable ?text_style ?on_line_click patch =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.diff ?on:on_line_click
    (Mosaic_ui.Diff.Props.make ~patch ?layout:view ?theme ?highlight
       ?line_highlights ?line_spans ?line_signs ?show_line_numbers ?wrap
       ?selectable ?text_style ())
    []

let markdown ?key ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?md_style
    ?conceal ?streaming ?selectable ?selection_bg ?selection_fg ?render_node
    ?render_code ?highlighter ?syntax_style ?on_selection content =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.markdown ?on:on_selection
    (Mosaic_ui.Markdown.Props.make ~content ?style:md_style ?conceal ?streaming
       ?selectable ?selection_bg ?selection_fg ?render_node ?render_code
       ?highlighter ?syntax_style ())
    []

let table ?key ?layout ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?columns ?(rows = []) ?border
    ?border_style ?show_header ?show_column_separator ?show_row_separator
    ?cell_padding ?header_color ?header_background ?text_color ?background
    ?selected_text_color ?selected_background ?focused_selected_text_color
    ?focused_selected_background ?selection_visible ?show_scroll_indicator
    ?activate_on_click ?wheel_navigation ?row_styles ?wrap_selection
    ?key_bindings ?fast_scroll_step ~on_change ?on_activate ?on_hover ~equal
    ~selected () =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  let selected_row =
    selection_index ~equal ~value:(fun item -> item.Table.value) selected rows
  in
  let value index = (List.nth rows index).Table.value in
  let on_change index = on_change (value index) in
  let on_activate = Option.map (fun f index -> f (value index)) on_activate in
  let on_hover =
    Option.map (fun f index -> f (Option.map value index)) on_hover
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.table
    ~on:(function
      | Kinds.Choose choice' -> choice on_change on_activate choice'
      | Hover row -> Option.bind on_hover (fun f -> f row))
    (Mosaic_ui.Table.Props.make ?columns ~selected_row ?border ?border_style
       ?show_header ?show_column_separator ?show_row_separator ?cell_padding
       ?header_color ?header_background ?text_color ?background
       ?selected_text_color ?selected_background ?focused_selected_text_color
       ?focused_selected_background ?selection_visible ?show_scroll_indicator
       ?activate_on_click ?wheel_navigation ?row_styles ?wrap_selection
       ?key_bindings ?fast_scroll_step ()
    |> Mosaic_ui.Table.Props.with_rows_of row_cells rows)
    []

let tree ?key ?layout ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?items ?indent_size
    ?show_guides ?guide_style ?expand_icon ?collapse_icon ?leaf_icon ?background
    ?text_color ?selected_background ?selected_text_color
    ?focused_selected_background ?focused_selected_text_color ?guide_color
    ?icon_color ?wrap_selection ?key_bindings ?fast_scroll_step ~on_change
    ?on_activate ~state () =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size
    ~kind:Kinds.tree
    ~on:(function
      | Kinds.Tree_change state -> Some (on_change state)
      | Tree_activate item -> Option.map (fun f -> f item) on_activate)
    (Mosaic_ui.Tree.Props.make ~state ?items ?indent_size ?show_guides
       ?guide_style ?expand_icon ?collapse_icon ?leaf_icon ?background
       ?text_color ?selected_background ?selected_text_color
       ?focused_selected_background ?focused_selected_text_color ?guide_color
       ?icon_color ?wrap_selection ?key_bindings ?fast_scroll_step ())
    []

module Element = struct
  type ('props, 'event, 'w) kind = ('props, 'event, 'w) Kind.t

  let kind = Kind.make
  let name = Kind.name

  let rec props : type p e w m. (p, e, w) kind -> m t -> p option =
   fun kind view ->
    match view with
    | Vnode.Element (Vnode.E e) -> (
        match Type.Id.provably_equal kind.Kind.id e.kind.Kind.id with
        | Some Type.Equal -> Some e.props
        | None -> None)
    | Vnode.Map (_, view) -> props kind view
    | Vnode.Themed (_, view) -> props kind view
    | _ -> None
end

let element ?key ?layout ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused kind ?on
    props children =
  let style =
    layout_style ?layout ?display ?box_sizing ?position ?overflow
      ?scrollbar_width ?text_align ?inset ?size ?min_size ?max_size
      ?aspect_ratio ?margin ?padding ?border_width ?gap ?align_items ?align_self
      ?align_content ?justify_items ?justify_self ?justify_content
      ?flex_direction ?flex_wrap ?flex_grow ?flex_shrink ?flex_basis
      ?grid_template_rows ?grid_template_columns ?grid_auto_rows
      ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
      ?grid_template_column_names ?grid_template_row_names ?grid_row
      ?grid_column ()
  in
  Vnode.element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ~kind ?on
    props children

let px n = Toffee.Style.Dimension.length (Float.of_int n)
let pct n = Toffee.Style.Dimension.pct (Float.of_int n)
let auto = Toffee.Style.Dimension.auto
let size ~width ~height = Toffee.Geometry.Size.make (px width) (px height)

let gap n =
  let lp = Toffee.Style.Length_percentage.length (Float.of_int n) in
  Toffee.Geometry.Size.make lp lp

let padding n =
  let lp = Toffee.Style.Length_percentage.length (Float.of_int n) in
  Toffee.Geometry.Rect.all lp

let margin n =
  let lpa = Toffee.Style.Length_percentage_auto.length (Float.of_int n) in
  Toffee.Geometry.Rect.all lpa

let inset n =
  let lpa = Toffee.Style.Length_percentage_auto.length (Float.of_int n) in
  Toffee.Geometry.Rect.all lpa

let size_wh w h : dimension Toffee.Geometry.Size.t = { width = w; height = h }

let gap_xy x y : length_percentage Toffee.Geometry.Size.t =
  let x = Toffee.Style.Length_percentage.length (Float.of_int x) in
  let y = Toffee.Style.Length_percentage.length (Float.of_int y) in
  { width = x; height = y }

let padding_xy x y : length_percentage Toffee.Geometry.Rect.t =
  let x = Toffee.Style.Length_percentage.length (Float.of_int x) in
  let y = Toffee.Style.Length_percentage.length (Float.of_int y) in
  { left = x; right = x; top = y; bottom = y }

let padding_lrtb l r t b : length_percentage Toffee.Geometry.Rect.t =
  let f n = Toffee.Style.Length_percentage.length (Float.of_int n) in
  { left = f l; right = f r; top = f t; bottom = f b }

let margin_xy x y : length_percentage_auto Toffee.Geometry.Rect.t =
  let x = Toffee.Style.Length_percentage_auto.length (Float.of_int x) in
  let y = Toffee.Style.Length_percentage_auto.length (Float.of_int y) in
  { left = x; right = x; top = y; bottom = y }

let margin_lrtb l r t b : length_percentage_auto Toffee.Geometry.Rect.t =
  let f n = Toffee.Style.Length_percentage_auto.length (Float.of_int n) in
  { left = f l; right = f r; top = f t; bottom = f b }

let inset_lrtb l r t b : length_percentage_auto Toffee.Geometry.Rect.t =
  let f n = Toffee.Style.Length_percentage_auto.length (Float.of_int n) in
  { left = f l; right = f r; top = f t; bottom = f b }
