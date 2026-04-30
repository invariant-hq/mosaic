(** Validated unified diff patches.

    Patches are the pure data model used by {!Diff}. They contain one file's
    hunks in source order. File headers are intentionally not represented. *)

type tag =
  | Context
  | Added
  | Removed
      (** The role of a patch line. [Context] appears on both sides, [Added]
          only on the new side, and [Removed] only on the old side. *)

type line = { tag : tag; content : string }
(** The type for patch lines. [content] excludes the unified-diff prefix and
    trailing newline. *)

type hunk = {
  old_start : int;
  old_lines : int;
  new_start : int;
  new_lines : int;
  lines : line list;
}
(** The type for contiguous patch hunks. Empty old or new ranges use start line
    [0], matching standard unified diffs such as ["@@ -0,0 +1,3 @@"]. *)

type t
(** The type for validated patches. Invariant: hunk ranges and line tags agree,
    and hunks are ordered by old-file range. *)

val empty : t
(** [empty] is the empty patch. *)

val make : hunk list -> t
(** [make hunks] is a patch from [hunks]. Raises [Invalid_argument] if hunk
    starts, counts, or ordering are invalid. *)

val of_unified : string -> (t, string) result
(** [of_unified s] parses the first unified-diff file patch in [s]. File headers
    and prelude lines are tolerated. "\\ No newline at end of file" markers are
    skipped. *)

val of_strings : old:string -> new_:string -> ?context:int -> unit -> t
(** [of_strings ~old ~new_ ()] computes a line-level patch with Myers diff.
    [context] controls unchanged lines around changes and defaults to [3]. *)

val hunks : t -> hunk list
(** [hunks t] is [t]'s hunks, in source order. *)

val is_empty : t -> bool
(** [is_empty t] is [true] iff [t] has no hunks. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] contain the same hunks and lines. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] writes [t] in unified-diff hunk form, without file headers. *)
