(** Compact sparkline charts.

    Sparklines render recent data points in a fixed-width buffer using bar
    glyphs or Braille dots. The rendering adapts to the available grid
    dimensions. *)

type t
(** The type for sparklines. A sparkline holds a circular buffer of recent
    values with fixed capacity. Old values are automatically evicted when the
    buffer is full. *)

type kind = [ `Bars | `Braille ]
(** The type for sparkline rendering styles.
    - [`Bars]: vertical bars using block glyphs (▁▂▃▄▅▆▇█) with fractional
      heights.
    - [`Braille]: connected line segments using 2×4 Braille dot grids per cell.
*)

(** {1:create Constructors} *)

val create :
  ?style:Ansi.Style.t ->
  ?auto_max:bool ->
  ?max_value:float ->
  capacity:int ->
  unit ->
  t
(** [create ~capacity ()] is a sparkline buffer.
    - [style] is the rendering style. Defaults to {!Ansi.Style.default}.
    - [auto_max], when [true] (the default), updates [max_value] dynamically
      when larger values are pushed.
    - [max_value] is the initial maximum for scaling. Values exceeding it are
      clipped unless [auto_max] is [true]. Defaults to [1.0] if [None] or
      negative.
    - [capacity] is the buffer size. Clamped to [>= 1].

    Negative values are clamped to [0.0] on {!push}. *)

(** {1:mutating Mutating} *)

val clear : t -> unit
(** [clear t] empties the buffer. Capacity and configuration are unchanged. *)

val push : t -> float -> unit
(** [push t v] appends [v] to the buffer, evicting the oldest value if the
    buffer is full. Negative [v] is clamped to [0.0]. If [auto_max] is [true]
    and [v] exceeds [max_value], [max_value] is updated. *)

val push_all : t -> float list -> unit
(** [push_all t vs] is [List.iter (push t) vs]. *)

val set_max : t -> float -> unit
(** [set_max t m] sets the scaling maximum. If [m <= 0] it is set to [1.0]. *)

(** {1:rendering Rendering} *)

val draw :
  t ->
  kind:kind ->
  ?columns_only:bool ->
  ?x:int ->
  ?y:int ->
  Grid.t ->
  width:int ->
  height:int ->
  unit
(** [draw t ~kind grid ~width ~height] renders the sparkline to [grid]. Only the
    most recent [width] values are shown, right-aligned, scaled by [max_value].
    - [columns_only], when [true], skips the background fill. Defaults to
      [false].
    - [x] and [y] are grid offsets. Default [0].
    - [width] and [height] are clamped to [>= 1]. *)

val draw_values :
  ?style:Ansi.Style.t ->
  kind:kind ->
  ?x:int ->
  ?y:int ->
  float list ->
  Grid.t ->
  width:int ->
  height:int ->
  unit
(** [draw_values ~kind vs grid ~width ~height] renders [vs] directly without
    persistent state. Creates a temporary sparkline with capacity equal to
    [width], pushes all [vs], and draws.
    - [style] defaults to {!Ansi.Style.default}.
    - [x] and [y] are grid offsets. Default [0]. *)
