(* Compute helpers - Helper functions used by layout algorithms *)

open Toffee_geometry
open Toffee_style

(* Compute how much width/height a child contributes to its parent's intrinsic
   content size. Respects overflow settings - visible overflow contributes to
   content size. *)
let compute_content_size_contribution ~(location : Pointf.t) ~(size : Sizef.t)
    ~(content_size : Sizef.t) ~overflow =
  let width =
    match overflow.Point.x with
    | Overflow.Visible -> Float.max size.width content_size.width
    | _ -> size.width
  in
  let height =
    match overflow.Point.y with
    | Overflow.Visible -> Float.max size.height content_size.height
    | _ -> size.height
  in
  if width > 0. && height > 0. then
    Sizef.{ width = location.x +. width; height = location.y +. height }
  else Sizef.zero

(* Implement fallback alignment.

   In addition to the spec at https://www.w3.org/TR/css-align-3/ this
   implementation follows the resolution of
   https://github.com/w3c/csswg-drafts/issues/10154 *)
let apply_alignment_fallback ~free_space ~num_items ~alignment_mode ~is_safe =
  (* Fallback occurs in two cases: *)

  (* 1. If there is only a single item being aligned and alignment is a
     distributed alignment keyword
     https://www.w3.org/TR/css-align-3/#distribution-values *)
  let alignment_mode, is_safe =
    if num_items <= 1 || free_space <= 0.0 then
      match alignment_mode with
      | Align_content.Stretch -> (Align_content.Flex_start, true)
      | Align_content.Space_between -> (Align_content.Flex_start, true)
      | Align_content.Space_around -> (Align_content.Center, true)
      | Align_content.Space_evenly -> (Align_content.Center, true)
      | _ -> (alignment_mode, is_safe)
    else (alignment_mode, is_safe)
  in

  (* 2. If free space is negative the "safe" alignment variants all fallback to
     Start alignment *)
  let alignment_mode =
    if free_space <= 0.0 && is_safe then Align_content.Start else alignment_mode
  in

  alignment_mode

(* Generic alignment function that is used: - For both align-content and
   justify-content alignment - For both the Flexbox and CSS Grid algorithms

   CSS Grid does not apply gaps as part of alignment, so the gap parameter
   should always be set to zero for CSS Grid. *)
let compute_alignment_offset ~free_space ~num_items ~gap ~alignment_mode
    ~layout_is_flex_reversed ~is_first =
  if is_first then
    match alignment_mode with
    | Align_content.Start -> 0.0
    | Align_content.Flex_start ->
        if layout_is_flex_reversed then free_space else 0.0
    | Align_content.End -> free_space
    | Align_content.Flex_end ->
        if layout_is_flex_reversed then 0.0 else free_space
    | Align_content.Center -> free_space /. 2.0
    | Align_content.Stretch -> 0.0
    | Align_content.Space_between -> 0.0
    | Align_content.Space_around ->
        if free_space >= 0.0 then free_space /. Float.of_int num_items /. 2.0
        else free_space /. 2.0
    | Align_content.Space_evenly ->
        if free_space >= 0.0 then free_space /. Float.of_int (num_items + 1)
        else free_space /. 2.0
  else
    let free_space = Float.max free_space 0.0 in
    gap
    +.
    match alignment_mode with
    | Align_content.Start -> 0.0
    | Align_content.Flex_start -> 0.0
    | Align_content.End -> 0.0
    | Align_content.Flex_end -> 0.0
    | Align_content.Center -> 0.0
    | Align_content.Stretch -> 0.0
    | Align_content.Space_between -> free_space /. Float.of_int (num_items - 1)
    | Align_content.Space_around -> free_space /. Float.of_int num_items
    | Align_content.Space_evenly -> free_space /. Float.of_int (num_items + 1)
