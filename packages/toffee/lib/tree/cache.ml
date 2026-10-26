(* A port of Taffy's cache (src/tree/cache.rs): the final layout and up to
   [cache_size] measurements, each found by its full key and evicted by second
   chance (CLOCK). *)

module Geometry = Toffee_geometry

(* Taffy keeps 9, fewer than a flex column asks each of its children in one
   layout under a root that sizes it by its content: three questions (the
   max-content and min-content heights, then the size at a known height) at
   the max-content width, at the min-content width, and at each definite width
   the column is tried at, those once with the parent's width and once
   without. A scroll box's content in a column is tried at three definite
   widths, 24 questions. They come in the same order in every layout, and
   second chance over fewer entries than questions evicts each one before it
   is asked again, so no child hits. *)
let cache_size = 24

(* A key and, for a measurement, the size measured. Every field is a float, so
   an entry is a single flat block, which a store overwrites in place.

   [width_key] and [height_key] encode an axis's known dimension when there is
   one, and else its available space: a known dimension as itself, a definite
   available space as its negation, min-content as negative infinity and
   max-content as infinity (Taffy's [mixed_cache_key]). [parent_width] and
   [parent_height] are the parent size, infinity for none. [axis] is the
   requested axis: 1 horizontal, 2 vertical, 3 both. *)
type entry = {
  mutable width_key : float;
  mutable height_key : float;
  mutable parent_width : float;
  mutable parent_height : float;
  mutable axis : float;
  mutable width : float;
  mutable height : float;
}

(* The absent entry, told apart by physical equality and never written. *)
let no_entry =
  {
    width_key = nan;
    height_key = nan;
    parent_width = nan;
    parent_height = nan;
    axis = nan;
    width = nan;
    height = nan;
  }

(* [entries.(cache_size)] keys the final layout, [final_layout]; the others are
   the measurements. The array is made by the first store, as many nodes are
   never laid out. An entry counts only while its valid bit is set, so
   clearing a cache keeps its entries for the next stores to overwrite.
   [state] packs those bits and the rest of Taffy's cache into one int: which
   measurements were used since the eviction cursor last passed them (one bit
   each), which entries are valid (one bit each), the cursor (5 bits) and the
   cache's mode (3 bits, see [mode]). *)
type t = {
  mutable entries : entry array;
  mutable final_layout : Layout_output.t;
  mutable state : int;
}

let used_mask = (1 lsl cache_size) - 1
let valid_shift = cache_size
let valid_mask = ((1 lsl (cache_size + 1)) - 1) lsl valid_shift
let cursor_shift = valid_shift + cache_size + 1
let mode_shift = cursor_shift + 5
let recently_used t = t.state land used_mask
let valid t index = t.state land (1 lsl (valid_shift + index)) <> 0
let cursor t = (t.state lsr cursor_shift) land 0x1f
let cache_mode t = (t.state lsr mode_shift) land 0x7
let set_recently_used t used = t.state <- t.state land lnot used_mask lor used

let set_cursor t cursor =
  t.state <-
    t.state land lnot (0x1f lsl cursor_shift) lor (cursor lsl cursor_shift)

let set_cache_mode t mode =
  t.state <- t.state land lnot (0x7 lsl mode_shift) lor (mode lsl mode_shift)

let make () =
  { entries = [||]; final_layout = Layout_output.default; state = 0 }

type clear_state = Cleared | Already_empty

(* The inputs that affect a layout but are not in the key: its sizing mode and
   which of its vertical margins are collapsible. They change for a node only
   when its parent's algorithm or its own style does, so instead of keying each
   entry on them a cache records the one mode its entries were computed with
   (Taffy's [CacheMode]). *)
let mode inputs =
  let collapsible = Layout_input.vertical_margins_are_collapsible inputs in
  (match Layout_input.sizing_mode inputs with
    | Sizing_mode.Inherent_size -> 1
    | Content_size -> 0)
  lor (if collapsible.start then 2 else 0)
  lor if collapsible.end_ then 4 else 0

let axis_key = function
  | Requested_axis.Horizontal -> 1.
  | Vertical -> 2.
  | Both -> 3.
[@@inline]

let mixed_key known (available : Available_space.t) =
  match known with
  | Some value -> value
  | None -> (
      match available with
      | Definite value -> -.value
      | Min_content -> neg_infinity
      | Max_content -> infinity)
[@@inline]

let parent_key = function Some value -> value | None -> infinity [@@inline]

(* Keys compare by their bits, as in Taffy. *)
let same (a : float) (b : float) =
  Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b)
[@@inline]

let entry inputs ~width ~height =
  let known = Layout_input.known_dimensions inputs in
  let available = Layout_input.available_space inputs in
  let parent = Layout_input.parent_size inputs in
  {
    width_key = mixed_key known.width available.width;
    height_key = mixed_key known.height available.height;
    parent_width = parent_key parent.width;
    parent_height = parent_key parent.height;
    axis = axis_key (Layout_input.axis inputs);
    width;
    height;
  }

(* Makes [entries.(index)] a valid entry keyed on [inputs] and holding [width]
   and [height]: the entry there overwritten, or a new one in place of
   [no_entry]. *)
let set t index inputs ~width ~height =
  if Array.length t.entries = 0 then
    t.entries <- Array.make (cache_size + 1) no_entry;
  let e = t.entries.(index) in
  (if e == no_entry then t.entries.(index) <- entry inputs ~width ~height
   else
     let known = Layout_input.known_dimensions inputs in
     let available = Layout_input.available_space inputs in
     let parent = Layout_input.parent_size inputs in
     e.width_key <- mixed_key known.width available.width;
     e.height_key <- mixed_key known.height available.height;
     e.parent_width <- parent_key parent.width;
     e.parent_height <- parent_key parent.height;
     e.axis <- axis_key (Layout_input.axis inputs);
     e.width <- width;
     e.height <- height);
  t.state <- t.state lor (1 lsl (valid_shift + index))

let get t inputs =
  if cache_mode t <> mode inputs then None
  else
    let known = Layout_input.known_dimensions inputs in
    let available = Layout_input.available_space inputs in
    let parent = Layout_input.parent_size inputs in
    let width_key = mixed_key known.width available.width in
    let height_key = mixed_key known.height available.height in
    let parent_width = parent_key parent.width in
    let axis = axis_key (Layout_input.axis inputs) in
    match Layout_input.run_mode inputs with
    | Run_mode.Perform_layout ->
        if
          valid t cache_size
          &&
          let e = t.entries.(cache_size) in
          same e.width_key width_key
          && same e.height_key height_key
          && same e.parent_width parent_width
          && same e.parent_height (parent_key parent.height)
          && e.axis = axis
        then Some t.final_layout
        else None
    | Compute_size ->
        let found = ref (-1) and i = ref 0 in
        while !found < 0 && !i < cache_size do
          if
            valid t !i
            &&
            let e = t.entries.(!i) in
            same e.width_key width_key
            && same e.height_key height_key
            (* A measurement keys on the parent's width only, and one for a
               single axis may hold garbage in the other, so it answers only
               queries for that axis; one for both axes answers any. *)
            && same e.parent_width parent_width
            && (e.axis = 3. || e.axis = axis)
          then found := !i;
          incr i
        done;
        if !found < 0 then None
        else
          let e = t.entries.(!found) in
          set_recently_used t (recently_used t lor (1 lsl !found));
          Some
            (Layout_output.from_outer_size
               { Geometry.Sizef.width = e.width; height = e.height })
    | Perform_hidden_layout -> None

let clear t =
  if t.state land valid_mask = 0 then Already_empty
  else (
    t.final_layout <- Layout_output.default;
    t.state <- t.state land (0x7 lsl mode_shift);
    Cleared)

let carries_margin_collapse output =
  let zero (m : Collapsible_margin_set.t) =
    m.positive = 0. && m.negative = 0.
  in
  Layout_output.margins_can_collapse_through output
  || (not (zero (Layout_output.top_margin output)))
  || not (zero (Layout_output.bottom_margin output))

let store t inputs output =
  match Layout_input.run_mode inputs with
  | Run_mode.Perform_hidden_layout -> ()
  | (Perform_layout | Compute_size) as run_mode -> (
      let mode = mode inputs in
      if cache_mode t <> mode then (
        ignore (clear t);
        set_cache_mode t mode);
      match run_mode with
      | Perform_layout ->
          set t cache_size inputs ~width:0. ~height:0.;
          t.final_layout <- output
      | Compute_size ->
          (* A measurement keeps only the size and comes back through
             [Layout_output.from_outer_size], without margin-collapse
             metadata: a result that carries some is not cached. *)
          if not (carries_margin_collapse output) then (
            let size = Layout_output.size output in
            let known = Layout_input.known_dimensions inputs in
            let available = Layout_input.available_space inputs in
            let parent = Layout_input.parent_size inputs in
            let width_key = mixed_key known.width available.width in
            let height_key = mixed_key known.height available.height in
            let parent_width = parent_key parent.width in
            let parent_height = parent_key parent.height in
            let axis = axis_key (Layout_input.axis inputs) in
            let index = ref (-1) and i = ref 0 in
            while !index < 0 && !i < cache_size do
              if
                valid t !i
                &&
                let e = t.entries.(!i) in
                same e.width_key width_key
                && same e.height_key height_key
                && same e.parent_width parent_width
                && same e.parent_height parent_height
                && e.axis = axis
              then index := !i;
              incr i
            done;
            if !index < 0 then (
              (* Second chance: pass over the entries used since the cursor
                 last passed them, clearing their bit, and replace the first
                 that was not. *)
              let cursor = ref (cursor t)
              and used = ref (recently_used t) in
              while !used land (1 lsl !cursor) <> 0 do
                used := !used land lnot (1 lsl !cursor);
                cursor := (!cursor + 1) mod cache_size
              done;
              index := !cursor;
              set_recently_used t !used;
              set_cursor t ((!cursor + 1) mod cache_size));
            set t !index inputs ~width:size.width ~height:size.height;
            set_recently_used t (recently_used t lor (1 lsl !index)))
      | Perform_hidden_layout -> ())

let is_empty t = t.state land valid_mask = 0
