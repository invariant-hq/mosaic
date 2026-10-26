open Mosaic_ui

(* The reconciler runs once per redraw over the whole view, so its per-frame
   cost on a large keyed list is a contract: a chat/log transcript re-rendered
   per keystroke must not pay quadratic placement work. Both cases build the
   vnode list inside the measured region — a TEA view rebuilds its vnodes
   every frame, so that allocation is part of the reconcile cost. *)

let viewport_width = 80

(* A list of [rows] keyed rows. [offset] rotates it: offset 0 is the
   identity order, any other offset moves the head block to the tail,
   forcing keyed matching and child moves without creating or destroying
   fibers. *)
type keyed = { rows : int; keys : string array; labels : string array }

let keyed rows =
  {
    rows;
    keys = Array.init rows (fun i -> Printf.sprintf "k%d" i);
    labels =
      Array.init rows (fun i ->
          Thumper.black_box
            (Printf.sprintf "row %d: a representative transcript line" i));
  }

let view l ~offset =
  Vnode.box
    (List.init l.rows (fun i ->
         let j = (i + offset) mod l.rows in
         Vnode.text ~key:l.keys.(j) l.labels.(j)))

type state = {
  l : keyed;
  reconciler : Mosaic.Reconciler.t;
  mutable offset : int;
}

let make_state rows () =
  let l = keyed rows in
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer)
  in
  Mosaic.Reconciler.render reconciler ~viewport_width (view l ~offset:0);
  { l; reconciler; offset = 0 }

(* The 500 and 5,000 pairs show how the cost scales: linear, not
   quadratic. *)
let steady_keyed_list rows =
  Thumper.bench_with_setup ~setup:(make_state rows)
    (Printf.sprintf "steady/keyed-list-%d" rows) (fun state ->
      Mosaic.Reconciler.render state.reconciler ~viewport_width
        (view state.l ~offset:0))

let reorder_keyed_list rows =
  Thumper.bench_with_setup ~setup:(make_state rows)
    (Printf.sprintf "reorder/keyed-list-%d" rows) (fun state ->
      state.offset <- (state.offset + 1) mod rows;
      Mosaic.Reconciler.render state.reconciler ~viewport_width
        (view state.l ~offset:state.offset))

(* Unmounting tears down every node a view mounted, as a program does when it
   quits. A call mounts the 20,000 rows and unmounts them, since a sample
   cannot unmount twice what it mounted once; the teardown must stay linear
   in the rows. *)
let mount_and_unmount rows =
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let renderer = Renderer.create () in
      let reconciler =
        Mosaic.Reconciler.create ~container:(Renderer.root renderer)
      in
      (reconciler, view (keyed rows) ~offset:0))
    (Printf.sprintf "keyed-list-%d" rows)
    (fun (reconciler, view) ->
      Mosaic.Reconciler.render reconciler ~viewport_width view;
      Mosaic.Reconciler.unmount reconciler)

(* A list that empties drops every row by reconciliation, as a log or a
   search result cleared by the application does. A call mounts the 20,000
   rows and renders the list empty; dropping them must stay linear in the
   rows. *)
let mount_and_empty rows =
  Thumper.bench_with_setup
    ~setup:(fun () ->
      let renderer = Renderer.create () in
      let reconciler =
        Mosaic.Reconciler.create ~container:(Renderer.root renderer)
      in
      (reconciler, view (keyed rows) ~offset:0, Vnode.box []))
    (Printf.sprintf "keyed-list-%d" rows)
    (fun (reconciler, full, empty) ->
      Mosaic.Reconciler.render reconciler ~viewport_width full;
      Mosaic.Reconciler.render reconciler ~viewport_width empty)

(* THUMPER_MACHINE=exact selects the section CI checks: allocation alone,
   whose counts are the same on any x86_64 Linux under the locked compiler in
   release, while time compares only on one machine. *)
let exact = Sys.getenv_opt "THUMPER_MACHINE" = Some "exact"

let () =
  Thumper.run "reconciler"
    ~config:
      Thumper.Config.(
        default
        |> metrics
             (if exact then [ Thumper.Metric.alloc_words ]
              else
                Thumper.Metric.
                  [ wall_time; alloc_words; promoted_words; minor_collections ]))
    ~budgets:
      (Thumper.Budget.no_more_alloc_than 0.
      ::
      (if exact then []
       else
         [
           Thumper.Budget.no_slower_than 0.05;
           (* Promotions and minor collections depend on where the minor
              heap stands when a sample starts, so they are rarely exact:
              kept, with room for that noise. *)
           Thumper.Budget.relative ~metric:Thumper.Metric.promoted_words
             ~max_regression:0.5 ();
           Thumper.Budget.relative ~metric:Thumper.Metric.minor_collections
             ~max_regression:0.5 ();
         ]))
    [
      Thumper.group "reconcile"
        [
          steady_keyed_list 500;
          reorder_keyed_list 500;
          steady_keyed_list 5000;
          reorder_keyed_list 5000;
        ];
      Thumper.group "mount-and-unmount" [ mount_and_unmount 20_000 ];
      Thumper.group "mount-and-empty" [ mount_and_empty 20_000 ];
    ]
