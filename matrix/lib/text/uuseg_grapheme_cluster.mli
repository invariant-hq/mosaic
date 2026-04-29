[@@@ocamlformat "disable"]
(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Grapheme cluster segmenter.

    Vendored from uuseg v17.0.0 with the following additions:
    {ul
    {- {!reset} for zero-allocation segmenter reuse.}
    {- {!check_boundary} and {!check_boundary_with_width} for
       zero-allocation direct boundary checks.}} *)

(** {1:segmenter Segmenter} *)

type t
(** The type for grapheme cluster segmenters. *)

val create : unit -> t
(** [create ()] is a new grapheme cluster segmenter. *)

val copy : t -> t
(** [copy s] is a copy of [s]. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] are in the same state. *)

val reset : t -> unit
(** [reset s] resets [s] to its initial state, ready to segment a
    new string. *)

(** {1:boundary Boundary checks} *)

val check_boundary : t -> Uchar.t -> bool
(** [check_boundary s u] is [true] if there is a grapheme cluster
    boundary before [u], and updates [s]. The first character
    always returns [true] (rule GB1).

    This is a zero-allocation alternative to {!add}. *)

val check_boundary_with_width : t -> Uchar.t -> int
(** [check_boundary_with_width s u] is like {!check_boundary} but
    also extracts the display width from a single property-table
    lookup. The result is a packed integer: bit 2 = is_boundary,
    bits 0–1 = width encoding (0 = -1, 1 = 0, 2 = 1, 3 = 2). *)

(** {1:streaming Streaming} *)

val add : t -> [ `Await | `End | `Uchar of Uchar.t ] -> Uuseg_base.ret
(** [add s v] is the standard uuseg streaming API. See
    {!Uuseg_base.ret} for the return values. *)
