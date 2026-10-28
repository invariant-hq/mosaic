module Geometry = Toffee_geometry

type t = {
  order : int;
  location : Geometry.Pointf.t;
  size : Geometry.Sizef.t;
  scrollable_overflow : Scrollable_overflow.t;
  scrollbar_size : Geometry.Sizef.t;
  border : Geometry.Rectf.t;
  padding : Geometry.Rectf.t;
  margin : Geometry.Rectf.t;
}

let make ~order ~location ~size ~scrollable_overflow ~scrollbar_size ~border
    ~padding ~margin =
  {
    order;
    location;
    size;
    scrollable_overflow;
    scrollbar_size;
    border;
    padding;
    margin;
  }

let order t = t.order
let location t = t.location
let size t = t.size
let scrollable_overflow t = t.scrollable_overflow
let scrollbar_size t = t.scrollbar_size
let border t = t.border
let padding t = t.padding
let margin t = t.margin

let default =
  {
    order = 0;
    location = Geometry.Pointf.zero;
    size = Geometry.Sizef.zero;
    scrollable_overflow = Scrollable_overflow.zero;
    scrollbar_size = Geometry.Sizef.zero;
    border = Geometry.Rectf.zero;
    padding = Geometry.Rectf.zero;
    margin = Geometry.Rectf.zero;
  }

let with_order order = { default with order }

let content_box_width layout =
  layout.size.width -. layout.padding.left -. layout.padding.right
  -. layout.border.left -. layout.border.right

let content_box_height layout =
  layout.size.height -. layout.padding.top -. layout.padding.bottom
  -. layout.border.top -. layout.border.bottom

let content_box_size layout =
  Geometry.Sizef.
    {
      width =
        layout.size.width -. layout.border.left -. layout.border.right
        -. layout.padding.left -. layout.padding.right;
      height =
        layout.size.height -. layout.border.top -. layout.border.bottom
        -. layout.padding.top -. layout.padding.bottom;
    }

let content_box_x layout =
  layout.location.x +. layout.margin.left +. layout.border.left
  +. layout.padding.left

let content_box_y layout =
  layout.location.y +. layout.margin.top +. layout.border.top
  +. layout.padding.top

let scroll_width layout =
  let overflow = layout.scrollable_overflow in
  if Scrollable_overflow.reversed_x overflow then
    -.Scrollable_overflow.left overflow
  else
    (* The padding box less the scrollbar, never below zero *)
    let client_width =
      Float.max 0.0
        (layout.size.width -. layout.border.left -. layout.border.right
        -. Float.min layout.scrollbar_size.width layout.size.width)
    in
    Float.max 0.0 (Scrollable_overflow.right overflow -. client_width)

let scroll_height layout =
  let overflow = layout.scrollable_overflow in
  if Scrollable_overflow.reversed_y overflow then
    -.Scrollable_overflow.top overflow
  else
    let client_height =
      Float.max 0.0
        (layout.size.height -. layout.border.top -. layout.border.bottom
        -. Float.min layout.scrollbar_size.height layout.size.height)
    in
    Float.max 0.0 (Scrollable_overflow.bottom overflow -. client_height)

let to_string t =
  Printf.sprintf
    "Layout { order: %d; location: %s; size: %s; scrollable_overflow: %s }"
    t.order
    (Geometry.Pointf.to_string t.location)
    (Geometry.Sizef.to_string t.size)
    (Scrollable_overflow.to_string t.scrollable_overflow)

let compare a b =
  let cmp = Int.compare a.order b.order in
  if cmp <> 0 then cmp
  else
    let cmp = Geometry.Pointf.compare a.location b.location in
    if cmp <> 0 then cmp
    else
      let cmp = Geometry.Sizef.compare a.size b.size in
      if cmp <> 0 then cmp
      else
        let cmp =
          Scrollable_overflow.compare a.scrollable_overflow
            b.scrollable_overflow
        in
        if cmp <> 0 then cmp
        else
          let cmp = Geometry.Sizef.compare a.scrollbar_size b.scrollbar_size in
          if cmp <> 0 then cmp
          else
            let cmp = Geometry.Rectf.compare a.border b.border in
            if cmp <> 0 then cmp
            else
              let cmp = Geometry.Rectf.compare a.padding b.padding in
              if cmp <> 0 then cmp else Geometry.Rectf.compare a.margin b.margin

let equal a b = compare a b = 0
let pp fmt t = Format.pp_print_string fmt (to_string t)
