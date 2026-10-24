(** Axis ticks and their labels.

    The ticks of an axis are guide values inside its scale's domain, each with a
    label and, on a time scale, a context line, and possibly a note that every
    label is read against. {!choose} picks them for an axis of a given length
    with Talbot, Lin and Hanrahan's scored search, measuring the labels of each
    candidate with a function the caller gives. The rules are those
    {!Matrix_charts} states for axes (its section on guides). A value [v] lies
    at the normalized position [norm v] on the axis. *)

type family =
  | Linear  (** Decimal steps [1], [5], [2], [2.5] times a power of ten. *)
  | Log of float  (** Powers of the base, with their multiples. *)
  | Time of int
      (** Calendar boundaries of Unix seconds at a fixed offset from UTC, in
          seconds. *)

type tick = { value : float; label : string; context : string option }
(** The type for labelled ticks. [context] is a time tick's second line. *)

type t = { major : tick list; note : string option }
(** The type for the ticks of an axis, in increasing order of their positions.
*)

val label :
  family -> float array -> (string * string option) array * string option
(** [label f vs] labels the increasing values [vs] together, with their note. *)

val choose :
  family ->
  norm:(float -> float) ->
  labels:(float array -> (string * string option) array * string option) ->
  length:float ->
  spacing:float ->
  measure:(string -> float) ->
  float * float ->
  t
(** [choose f ~norm ~labels ~length ~spacing ~measure (a, b)] is the best ticks
    of the domain [(a, b)], [a <= b], for an axis of [length] cells, labelled by
    [labels]. [measure l] is the extent label [l] needs along the axis,
    clearance included. A domain whose ends normalize to the same position has
    the one tick [a]. *)

val of_values :
  norm:(float -> float) ->
  labels:(float array -> (string * string option) array * string option) ->
  float list ->
  float * float ->
  t
(** [of_values ~norm ~labels vs (a, b)] is the ticks at the values of [vs] in
    [(a, b)], each once. *)

val stride : length:float -> measure:(string -> float) -> string array -> int
(** [stride ~length ~measure names] is the least stride [k] of [1], [2], [5],
    [10], [20], … that labels at most a hundred of the [names], spread evenly
    over [length] cells, without overlap. *)

val nice : family -> float * float -> float * float
(** [nice f (a, b)] is [(a, b)] rounded outward as the nice rule of [f] rounds a
    fitted domain. A constant domain is returned as it is. *)
