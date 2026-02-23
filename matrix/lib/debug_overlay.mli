(** Debug overlay with runtime telemetry.

    Paints a semi-transparent box in a screen corner showing frame metrics from
    {!Screen.last_metrics}, rolling averages for frame time and interval, and
    per-frame GC statistics from {!Gc.quick_stat}.

    Wired into the Matrix runtime via {!Matrix.set_debug_overlay} but usable
    standalone in custom render pipelines.

    {b Note.} The default theme uses alpha=200 backgrounds. Set
    [respect_alpha=true] in {!Matrix.create} for correct transparency; otherwise
    the overlay renders opaque. *)

(** {1:types Types} *)

type corner = [ `Top_left | `Top_right | `Bottom_left | `Bottom_right ]
(** The type for overlay anchor corners. *)

(** {1:rendering Rendering} *)

val on_frame :
  ?corner:corner ->
  ?padding:int ->
  ?gap:int ->
  ?capacity:int ->
  unit ->
  Screen.t ->
  unit
(** [on_frame ()] is a callback that draws the overlay onto [screen]'s grid.
    Call once per frame after the UI has painted.

    The callback keeps internal state (rolling averages, GC deltas); reuse the
    same callback for the overlay's lifetime.
    - [corner] anchor position. Defaults to [`Bottom_right].
    - [padding] outer padding in cells.
    - [gap] gap between metric lines.
    - [capacity] maximum number of metric samples retained. *)
