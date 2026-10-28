module Geometry = Toffee_geometry

type t = {
  size : Geometry.Sizef.t;
  scrollable_overflow : Scrollable_overflow.t;
  first_baselines : float option Geometry.point;
  top_margin : Collapsible_margin_set.t;
  bottom_margin : Collapsible_margin_set.t;
  margins_can_collapse_through : bool;
}

let make ~size ~scrollable_overflow ~first_baselines ~top_margin ~bottom_margin
    ~margins_can_collapse_through =
  {
    size;
    scrollable_overflow;
    first_baselines;
    top_margin;
    bottom_margin;
    margins_can_collapse_through;
  }

let size t = t.size
let scrollable_overflow t = t.scrollable_overflow
let first_baselines t = t.first_baselines
let top_margin t = t.top_margin
let bottom_margin t = t.bottom_margin
let margins_can_collapse_through t = t.margins_can_collapse_through

let hidden =
  {
    size = Geometry.Sizef.zero;
    scrollable_overflow = Scrollable_overflow.zero;
    first_baselines = Geometry.(Point.none);
    top_margin = Collapsible_margin_set.zero;
    bottom_margin = Collapsible_margin_set.zero;
    margins_can_collapse_through = false;
  }

let default =
  {
    size = Geometry.Sizef.zero;
    scrollable_overflow = Scrollable_overflow.zero;
    first_baselines = Geometry.(Point.none);
    top_margin = Collapsible_margin_set.zero;
    bottom_margin = Collapsible_margin_set.zero;
    margins_can_collapse_through = false;
  }

let from_outer_size size =
  {
    size;
    (* Match Taffy: when only the outer size is known, content size is zero. *)
    scrollable_overflow = Scrollable_overflow.zero;
    first_baselines = Geometry.(Point.none);
    top_margin = Collapsible_margin_set.zero;
    bottom_margin = Collapsible_margin_set.zero;
    margins_can_collapse_through = false;
  }

let from_sizes_and_baselines size scrollable_overflow first_baselines =
  {
    size;
    scrollable_overflow;
    first_baselines;
    top_margin = Collapsible_margin_set.zero;
    bottom_margin = Collapsible_margin_set.zero;
    margins_can_collapse_through = false;
  }

let to_string t =
  Printf.sprintf
    "LayoutOutput { size: %s; scrollable_overflow: %s; \
     margins_can_collapse_through: %b }"
    (Geometry.Sizef.to_string t.size)
    (Scrollable_overflow.to_string t.scrollable_overflow)
    t.margins_can_collapse_through

let compare a b =
  let cmp = Geometry.Sizef.compare a.size b.size in
  if cmp <> 0 then cmp
  else
    let cmp =
      Scrollable_overflow.compare a.scrollable_overflow b.scrollable_overflow
    in
    if cmp <> 0 then cmp
    else
      let cmp =
        Geometry.Point.compare
          (Option.compare Float.compare)
          a.first_baselines b.first_baselines
      in
      if cmp <> 0 then cmp
      else
        let cmp = Collapsible_margin_set.compare a.top_margin b.top_margin in
        if cmp <> 0 then cmp
        else
          let cmp =
            Collapsible_margin_set.compare a.bottom_margin b.bottom_margin
          in
          if cmp <> 0 then cmp
          else
            Bool.compare a.margins_can_collapse_through
              b.margins_can_collapse_through

let equal a b = compare a b = 0
let pp fmt t = Format.pp_print_string fmt (to_string t)
