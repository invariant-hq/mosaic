(* Compute helpers - Helper functions used by layout algorithms *)

open Toffee_geometry
open Toffee_style
module Scrollable_overflow = Toffee_tree.Scrollable_overflow

(* Scrollable overflow (https://www.w3.org/TR/css-overflow-3/#scrollable),
   gathered in a container's padding-box frame *)

type overflow_acc = {
  mutable left : float;
  mutable right : float;
  mutable top : float;
  mutable bottom : float;
  origin_x : float;
  origin_y : float;
  mutable client_width : float;
  mutable client_height : float;
}

let overflow_acc ~origin_x ~origin_y ~client_width ~client_height =
  {
    left = 0.0;
    right = 0.0;
    top = 0.0;
    bottom = 0.0;
    origin_x;
    origin_y;
    client_width;
    client_height;
  }

type overflow_margins = Full_margins | Negative_margins | No_margins

(* The container's flags, packed in an int so that passing them allocates
   nothing *)
type overflow_flags = int

let scroll_container_bit = 1
let reversed_x_bit = 2
let reversed_y_bit = 4
let negative_margins_bit = 8
let no_margins_bit = 16

let overflow_flags ~is_scroll_container ~reversed_x ~reversed_y ~margins =
  (if is_scroll_container then scroll_container_bit else 0)
  lor (if reversed_x then reversed_x_bit else 0)
  lor (if reversed_y then reversed_y_bit else 0)
  lor
  match margins with
  | Full_margins -> 0
  | Negative_margins -> negative_margins_bit
  | No_margins -> no_margins_bit

let with_margins flags margins =
  flags
  land lnot (negative_margins_bit lor no_margins_bit)
  lor
  match margins with
  | Full_margins -> 0
  | Negative_margins -> negative_margins_bit
  | No_margins -> no_margins_bit

let union (acc : overflow_acc) ~left ~right ~top ~bottom =
  if left < acc.left then acc.left <- left;
  if right > acc.right then acc.right <- right;
  if top < acc.top then acc.top <- top;
  if bottom > acc.bottom then acc.bottom <- bottom
[@@inline]

let margin_of flags side =
  if flags land no_margins_bit <> 0 then 0.0
  else if flags land negative_margins_bit <> 0 then Float.min side 0.0
  else side
[@@inline]

let add_overflow_contribution (acc : overflow_acc) ~(flags : overflow_flags)
    ~(padding : Rectf.t) ~(location : Pointf.t) ~(size : Sizef.t)
    ~(overflow_area : Scrollable_overflow.t) ~(border : Rectf.t)
    ~(overflow : Overflow.t Point.t) ~(margin : Rectf.t) =
  let parent_is_scroll_container = flags land scroll_container_bit <> 0 in
  (* The child's area is relative to its inner border edge *)
  let is_scroll_container =
    Overflow.is_container overflow.x || Overflow.is_container overflow.y
  in
  let propagates_x = (not is_scroll_container) && overflow.x = Overflow.Visible
  and propagates_y =
    (not is_scroll_container) && overflow.y = Overflow.Visible
  in
  let end_width =
    if propagates_x then
      Float.max size.width
        (Scrollable_overflow.right overflow_area +. border.left)
    else size.width
  and end_height =
    if propagates_y then
      Float.max size.height
        (Scrollable_overflow.bottom overflow_area +. border.top)
    else size.height
  in
  (* An empty box contributes nothing, but to a scroll container, which
     scrolls to its position *)
  if (end_width > 0.0 && end_height > 0.0) || parent_is_scroll_container then
    let x = location.x -. acc.origin_x and y = location.y -. acc.origin_y in
    let left =
      x
      +.
      if propagates_x then
        Float.min 0.0 (Scrollable_overflow.left overflow_area +. border.left)
      else 0.0
    and top =
      y
      +.
      if propagates_y then
        Float.min 0.0 (Scrollable_overflow.top overflow_area +. border.top)
      else 0.0
    and right = x +. end_width
    and bottom = y +. end_height in
    if not parent_is_scroll_container then union acc ~left ~right ~top ~bottom
    else
      let reversed_x = flags land reversed_x_bit <> 0
      and reversed_y = flags land reversed_y_bit <> 0 in
      if
        (* A box wholly before the scroll origin is unreachable; an empty box
           at the origin is not before it *)
        (if reversed_x then
           left > acc.client_width || (left = acc.client_width && right > left)
         else right < 0.0 || (right = 0.0 && left < 0.0))
        ||
        if reversed_y then
          top > acc.client_height || (top = acc.client_height && bottom > top)
        else bottom < 0.0 || (bottom = 0.0 && top < 0.0)
      then ()
      else
        (* A scroll container's end padding extends past its children's
           margin boxes *)
        let left =
          if reversed_x then
            Float.min left
              (x -. Float.max 0.0 (padding.left +. margin_of flags margin.left))
          else left
        and right =
          if reversed_x then right
          else
            Float.max right
              (x +. size.width
              +. Float.max 0.0 (padding.right +. margin_of flags margin.right))
        and top =
          if reversed_y then
            Float.min top
              (y -. Float.max 0.0 (padding.top +. margin_of flags margin.top))
          else top
        and bottom =
          if reversed_y then bottom
          else
            Float.max bottom
              (y +. size.height
              +. Float.max 0.0 (padding.bottom +. margin_of flags margin.bottom)
              )
        in
        union acc ~left ~right ~top ~bottom

let finalize_overflow (acc : overflow_acc) ~(flags : overflow_flags) =
  let is_scroll_container = flags land scroll_container_bit <> 0
  and reversed_x = flags land reversed_x_bit <> 0
  and reversed_y = flags land reversed_y_bit <> 0 in
  let client_width = Float.max acc.client_width 0.0
  and client_height = Float.max acc.client_height 0.0 in
  if is_scroll_container then
    if
      Float.max acc.right client_width -. Float.min acc.left 0.0 <= 0.0
      || Float.max acc.bottom client_height -. Float.min acc.top 0.0 <= 0.0
    then
      (* A scroll container whose padding box and content cover no area has
         nothing to scroll *)
      Scrollable_overflow.make Rectf.zero ~reversed_x ~reversed_y
    else
      (* Only what scrolling can reach: past the origin's side *)
      Scrollable_overflow.make
        Rectf.
          {
            left = (if reversed_x then acc.left else 0.0);
            right =
              (if reversed_x then Float.min acc.right client_width
               else acc.right);
            top = (if reversed_y then acc.top else 0.0);
            bottom =
              (if reversed_y then Float.min acc.bottom client_height
               else acc.bottom);
          }
        ~reversed_x ~reversed_y
  else if
    (* Content within the padding box overflows nothing *)
    acc.left >= 0.0 && acc.top >= 0.0 && acc.right <= client_width
    && acc.bottom <= client_height
  then Scrollable_overflow.zero
  else
    Scrollable_overflow.make
      Rectf.
        {
          left = acc.left;
          right = acc.right;
          top = acc.top;
          bottom = acc.bottom;
        }
      ~reversed_x:false ~reversed_y:false

(* Implement fallback alignment.

   In addition to the spec at https://www.w3.org/TR/css-align-3/ this
   implementation follows the resolution of
   https://github.com/w3c/csswg-drafts/issues/10154 *)
let apply_alignment_fallback ~free_space ~num_items ~alignment_mode ~is_safe =
  (* Fallback occurs in two cases: *)

  (* 1. If there is only a single item being aligned or the items overflow,
     the distributed alignment keywords fall back to a positional one
     (https://www.w3.org/TR/css-align-3/#distribution-values): the space-*
     ones safely, so that step 2 turns them to Start when the items overflow,
     and stretch to plain flex-start. *)
  let alignment_mode, is_safe =
    if num_items <= 1 || free_space <= 0.0 then
      match alignment_mode with
      | Align_content.Stretch -> (Align_content.Flex_start, is_safe)
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
