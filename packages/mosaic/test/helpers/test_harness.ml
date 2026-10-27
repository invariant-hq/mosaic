open Mosaic_ui

(* ── Ordering assertions ── *)

(* [satisfies] over [int] with the claim built from the bound, so a call
   site stays one line and the failure still shows both numbers. Thread
   [?pos] when the failing line matters more than the test's name. *)

let gt ?pos ?msg ~than n =
  Windtrap.satisfies ?pos ?msg
    ~claim:(Printf.sprintf "greater than %d" than)
    Windtrap.int
    (fun v -> v > than)
    n

let ge ?pos ?msg ~than n =
  Windtrap.satisfies ?pos ?msg
    ~claim:(Printf.sprintf "at least %d" than)
    Windtrap.int
    (fun v -> v >= than)
    n

let lt ?pos ?msg ~than n =
  Windtrap.satisfies ?pos ?msg
    ~claim:(Printf.sprintf "less than %d" than)
    Windtrap.int
    (fun v -> v < than)
    n

let le ?pos ?msg ~than n =
  Windtrap.satisfies ?pos ?msg
    ~claim:(Printf.sprintf "at most %d" than)
    Windtrap.int
    (fun v -> v <= than)
    n

(* ── Renderer Context ── *)

type ctx = {
  ctx : Renderable.Private.context;
  schedule_count : int ref;
  focus_log : string list ref;
  blur_log : string list ref;
  unregister_log : string list ref;
}

let next_num = ref 0

let make_ctx ?(width_method = `Unicode) () =
  let tree = Toffee.new_tree () in
  let schedule_count = ref 0 in
  let focus_log = ref [] in
  let blur_log = ref [] in
  let unregister_log = ref [] in
  let ctx : Renderable.Private.context =
    {
      tree;
      schedule = (fun () -> incr schedule_count);
      off_loop = (fun work -> ignore (Thread.create work () : Thread.t));
      focus =
        (fun n ->
          focus_log := Renderable.id n :: !focus_log;
          Renderable.Private.focus_direct n);
      blur =
        (fun n ->
          blur_log := Renderable.id n :: !blur_log;
          Renderable.Private.blur_direct n);
      get_selection = (fun () -> None);
      request_selection_update = (fun () -> ());
      register_lifecycle = (fun _ -> ());
      unregister_lifecycle = (fun _ -> ());
      alloc_num =
        (fun () ->
          let n = !next_num in
          incr next_num;
          n);
      register = (fun _ -> ());
      unregister =
        (fun n -> unregister_log := Renderable.id n :: !unregister_log);
      width_method = (fun () -> width_method);
      cell_size = (fun () -> None);
      report_scroll = (fun _ ~region:_ ~dx:_ ~dy:_ -> ());
      admit_size_report = (fun _ -> true);
      regroups = [];
      top_layer = None;
      theme = Theme.plain;
    }
  in
  { ctx; schedule_count; focus_log; blur_log; unregister_log }

(* ── Root and Layout ── *)

let make_root ?style t =
  let root = Renderable.Private.create_root t.ctx ?style () in
  Renderable.Private.set_is_root root true;
  root

let layout_node node ~x ~y ~width ~height =
  Renderable.Private.update_layout node ~x:(Float.of_int x) ~y:(Float.of_int y)
    ~width:(Float.of_int width) ~height:(Float.of_int height)

(* ── Box Helpers ── *)

let lp = Toffee.Style.Length_percentage.equal
let one = Toffee.Style.Length_percentage.length 1.
let zero_lp = Toffee.Style.Length_percentage.zero
let border_of node = Toffee.Style.border (Renderable.style node)

(* ── Grid ── *)

let make_grid ?(width = 80) ?(height = 24) ?(width_method = `Unicode) () =
  Matrix_grid.create ~width ~height ~width_method ()

(* ── Input events ── *)

(* Modifiers with the ones held named, for [make_key] and [char_key]. *)
let mods ?(ctrl = false) ?(alt = false) ?(shift = false) ?(super = false)
    ?(hyper = false) ?(meta = false) ?(caps_lock = false) ?(num_lock = false) ()
    : Matrix_input.Keystroke.mods =
  { shift; alt; ctrl; super; hyper; meta; caps_lock; num_lock }

let make_key ?ctrl ?alt ?shift ?super ?hyper ?meta ?caps_lock ?num_lock ?repeat
    ?text ?base ?keypad key =
  Matrix_input.Keystroke.make
    ~mods:(mods ?ctrl ?alt ?shift ?super ?hyper ?meta ?caps_lock ?num_lock ())
    ?repeat ?text ?base ?keypad key

let char_key ?ctrl ?alt ?shift ?super ?hyper ?meta ?repeat ?text ?base c =
  make_key ?ctrl ?alt ?shift ?super ?hyper ?meta ?repeat ?text ?base
    (Char (Uchar.of_char c))

(* Mouse reports as the terminal sends them, for tests that dispatch through
   the renderer. *)
let mouse_report ?(shift = false) ?(alt = false) ?(ctrl = false) ~x ~y kind :
    Matrix_input.Mouse_report.t =
  { kind; x; y; shift; alt; ctrl }

(* Dispatch through the renderer as a host does with no listeners of its
   own: the path is taken, then the event delivered along it. Each returns
   the event, whose flags tell what the tree did with it. *)
let dispatch_key renderer key =
  let event = Event.Key.of_input key in
  Renderer.dispatch_key renderer (Renderer.focus_path renderer) event;
  event

let dispatch_paste renderer text =
  let event = Event.Paste.of_text text in
  Renderer.dispatch_paste renderer (Renderer.focus_path renderer) event;
  event

let dispatch_mouse renderer report =
  let path, event = Renderer.mouse_path renderer report in
  Renderer.dispatch_mouse renderer path event;
  event

(* UI-layer mouse events (Event.Mouse). Widget tests that work at the input
   layer instead — Matrix_input.Mouse_report — build their own; the two
   layers are different types, not duplicates. *)
let no_mod = Event.Mouse.no_modifier

let mouse_down ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod (Down { button = Left })

let mouse_up ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Up { button = Left; is_dragging = false })

let mouse_drag ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Drag { button = Left; is_dragging = true })

let mouse_move ~x ~y = Event.Mouse.make ~x ~y ~modifiers:no_mod Move
let mouse_out ~x ~y = Event.Mouse.make ~x ~y ~modifiers:no_mod Out

let mouse_scroll_down ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Scroll { direction = Scroll_down; delta = 1 })

let mouse_scroll_up ~x ~y =
  Event.Mouse.make ~x ~y ~modifiers:no_mod
    (Scroll { direction = Scroll_up; delta = 1 })
