(** Collapsible margin tracking for CSS Block layout.

    In CSS Block layout, adjacent vertical margins collapse according to
    specific rules. This module tracks sets of collapsible margins during layout
    computation, maintaining the largest positive and smallest (most negative)
    margins encountered. The final collapsed margin is the sum of these two
    values.

    {1 Margin Collapsing}

    CSS specifies that vertical margins between adjacent block-level boxes
    collapse. When multiple margins collapse together, the resulting margin is:
    - For positive margins: the maximum of all positive margins
    - For negative margins: the minimum (most negative) of all negative margins
    - Final collapsed value: positive + negative

    This implements the margin collapsing semantics defined in CSS 2.1 Section
    8.3.1.

    {1 Usage}

    Create a margin set from a single margin:
    {[
    let margin_set = Collapsible_margin_set.from_margin 16.0
    ]}

    Collapse additional margins into the set:
    {[
    let collapsed =
      margin_set
      |> Collapsible_margin_set.collapse_with_margin 24.0
      |> Collapsible_margin_set.collapse_with_margin (-8.0)
    ]}

    Resolve the final collapsed margin:
    {[
    let final_margin = Collapsible_margin_set.resolve collapsed
    (* = 24.0 + (-8.0) = 16.0 *)
    ]} *)

type t = { positive : float; negative : float }
(** The type representing a set of collapsible margins.

    Maintains the largest positive margin and the smallest (most negative)
    margin encountered during margin collapsing. *)

val zero : t
(** [zero] is a margin set with no collapsible margins.

    Both [positive] and [negative] are [0.0]. *)

val from_margin : float -> t
(** [from_margin margin] creates a margin set from a single margin value.

    If [margin >= 0.0], sets [positive = margin] and [negative = 0.0].
    Otherwise, sets [positive = 0.0] and [negative = margin]. *)

val collapse_with_margin : t -> float -> t
(** [collapse_with_margin t margin] collapses [margin] into margin set [t].

    Returns a new margin set where [positive] is [max t.positive margin] if
    [margin >= 0.0], or [negative] is [min t.negative margin] if [margin < 0.0].
    The other field remains unchanged. *)

val collapse_with_set : t -> t -> t
(** [collapse_with_set t other] collapses margin set [other] into [t].

    Returns a new margin set with [positive = max t.positive other.positive] and
    [negative = min t.negative other.negative]. *)

val resolve : t -> float
(** [resolve t] computes the final collapsed margin value.

    Returns [t.positive + t.negative]. This is the effective margin after all
    collapsing rules have been applied. *)

val to_string : t -> string
(** [to_string t] returns a debug string representation of the margin set. *)

val compare : t -> t -> int
(** [compare a b] provides total ordering for margin sets.

    Compares first by [positive], then by [negative] if [positive] values are
    equal. *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality of two margin sets.

    Returns [true] if both [positive] and [negative] fields are equal using
    [Float.equal]. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] is a pretty-printer for margin sets. *)
