(* Charts are rebuilt every frame: the program builds the figure from its
   data, lays it out at the region's size and draws it. These cases measure
   that frame, its layout alone, a hit under the pointer and a sparkline. *)

module G = Matrix_grid
module C = Matrix_charts

(* THUMPER_MACHINE=exact selects the section CI checks: allocation alone,
   whose counts are the same on any x86_64 Linux under the locked compiler in
   release, while time compares only on one machine. *)
let exact = Sys.getenv_opt "THUMPER_MACHINE" = Some "exact"

type point = { x : float; y : float }

let () = Random.init 7

let curve scale rate =
  Array.init 240 (fun i ->
      let x = float i in
      { x; y = (scale *. exp (-.x /. rate)) +. Random.float 0.1 })

let loss = curve 2. 80.
let val_loss = curve 2.1 90.

let cloud =
  Array.init 10_000 (fun _ -> { x = Random.float 100.; y = Random.float 50. })

let samples = List.init 30 (fun i -> 50. +. (40. *. sin (float i /. 3.)))
let x p = p.x
let y p = p.y

(* The training dashboard's loss chart: two curves, a reference line and
   gridlines. *)
let loss_chart () =
  C.(
    layer
      [
        line ~color:(const Ansi.Color.cyan) ~x ~y loss;
        line ~color:(const Ansi.Color.magenta) ~x ~y val_loss;
        hline 1.0;
        axis ~grid:`Ticks "x";
        axis ~grid:`Ticks "y";
      ])

let frame name ~width ~height fig =
  Thumper.bench_with_setup
    ~setup:(fun () -> G.create ~width ~height ())
    ~teardown:ignore name
    (fun g ->
      let l = C.layout ~width ~height (fig ()) in
      C.draw l g ~x:0 ~y:0)

let benchmarks =
  Thumper.
    [
      group "frame"
        [
          frame "frame/loss-78x20" ~width:78 ~height:20 loss_chart;
          frame "frame/scatter-10k-120x40" ~width:120 ~height:40 (fun () ->
              C.dot ~dots:`Braille ~x ~y cloud);
          frame "frame/spark-30x4" ~width:30 ~height:4 (fun () ->
              C.spark samples);
        ];
      group "pointer"
        [
          bench "pointer/layout-loss-78x20" (fun () ->
              ignore
                (Sys.opaque_identity
                   (C.layout ~width:78 ~height:20 (loss_chart ()))));
          bench "pointer/hit-loss-78x20" (fun () ->
              let l = C.layout ~width:78 ~height:20 (loss_chart ()) in
              ignore (Sys.opaque_identity (C.Layout.hit l (40, 10))));
        ];
    ]

let () =
  Thumper.run "charts"
    ~config:
      Thumper.Config.(
        default
        |> metrics
             (if exact then [ Thumper.Metric.alloc_words ]
              else Thumper.Metric.[ wall_time; alloc_words ]))
    ~budgets:
      (Thumper.Budget.no_more_alloc_than 0.
      :: (if exact then [] else [ Thumper.Budget.no_slower_than 0.05 ]))
    benchmarks
