open Mosaic_ui

(* ── Renderer Context ── *)

type ctx = {
  ctx : Renderable.Private.context;
  schedule_count : int ref;
  focus_log : string list ref;
  blur_log : string list ref;
  unregister_log : string list ref;
}

let next_num = ref 0

let make_ctx () =
  let tree = Toffee.new_tree () in
  let schedule_count = ref 0 in
  let focus_log = ref [] in
  let blur_log = ref [] in
  let unregister_log = ref [] in
  let ctx : Renderable.Private.context =
    {
      tree;
      schedule = (fun () -> incr schedule_count);
      focus =
        (fun n ->
          focus_log := Renderable.id n :: !focus_log;
          Renderable.Private.focus_direct n);
      blur =
        (fun n ->
          blur_log := Renderable.id n :: !blur_log;
          Renderable.Private.blur_direct n);
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

let make_grid ?(width = 80) ?(height = 24) () =
  Grid.create ~width ~height ~glyph_pool:(Glyph.Pool.create ())
    ~width_method:`Unicode ~respect_alpha:false ()
