(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Segmenter commonalities.

    Types and helpers shared by segmenter modules. Vendored from
    uuseg v17.0.0. *)

(** {1:types Types} *)

type ret = [ `Await | `Boundary | `End | `Uchar of Uchar.t ]
(** The type for segmenter return values. *)

(** {1:fmt Formatting} *)

val pp_ret : Format.formatter -> [< ret ] -> unit
(** [pp_ret] formats a segmenter return value. *)

(** {1:errors Errors} *)

val err_exp_await : [< ret] -> 'a
(** [err_exp_await v] raises [Invalid_argument] indicating that
    [`Await] was expected but [v] was received. *)

val err_ended : [< ret] -> 'a
(** [err_ended v] raises [Invalid_argument] indicating that the
    segmenter has already ended. *)
