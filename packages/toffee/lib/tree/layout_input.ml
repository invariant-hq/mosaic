module Geometry = Toffee_geometry

type t = {
  run_mode : Run_mode.t;
  sizing_mode : Sizing_mode.t;
  axis : Requested_axis.t;
  known_dimensions : float option Geometry.size;
  known_dimensions_are_definite : bool Geometry.size;
  parent_size : float option Geometry.size;
  available_space : Available_space.t Geometry.size;
  vertical_margins_are_collapsible : bool Geometry.line;
}

let definite : bool Geometry.size = { width = true; height = true }

let make ~run_mode ~sizing_mode ~axis ~known_dimensions ~parent_size
    ~available_space ~vertical_margins_are_collapsible =
  {
    run_mode;
    sizing_mode;
    axis;
    known_dimensions;
    known_dimensions_are_definite = definite;
    parent_size;
    available_space;
    vertical_margins_are_collapsible;
  }

let run_mode t = t.run_mode
let sizing_mode t = t.sizing_mode
let axis t = t.axis
let known_dimensions t = t.known_dimensions
let known_dimensions_are_definite t = t.known_dimensions_are_definite
let parent_size t = t.parent_size
let available_space t = t.available_space
let vertical_margins_are_collapsible t = t.vertical_margins_are_collapsible

let hidden =
  {
    run_mode = Run_mode.Perform_hidden_layout;
    sizing_mode = Sizing_mode.Inherent_size;
    axis = Requested_axis.Both;
    known_dimensions = Geometry.(Size.none);
    known_dimensions_are_definite = definite;
    parent_size = Geometry.(Size.none);
    available_space =
      {
        width = Available_space.Max_content;
        height = Available_space.Max_content;
      };
    vertical_margins_are_collapsible = { start = false; end_ = false };
  }

let to_string t =
  Printf.sprintf "LayoutInput { run_mode: %s; sizing_mode: %s; axis: %s; ... }"
    (Run_mode.to_string t.run_mode)
    (Sizing_mode.to_string t.sizing_mode)
    (Requested_axis.to_string t.axis)

let compare a b =
  let cmp = Run_mode.compare a.run_mode b.run_mode in
  if cmp <> 0 then cmp
  else
    let cmp = Sizing_mode.compare a.sizing_mode b.sizing_mode in
    if cmp <> 0 then cmp
    else
      let cmp = Requested_axis.compare a.axis b.axis in
      if cmp <> 0 then cmp
      else
        let cmp =
          Geometry.Size.compare
            (Option.compare Float.compare)
            a.known_dimensions b.known_dimensions
        in
        if cmp <> 0 then cmp
        else
          let cmp =
            Geometry.Size.compare Bool.compare a.known_dimensions_are_definite
              b.known_dimensions_are_definite
          in
          if cmp <> 0 then cmp
          else
            let cmp =
              Geometry.Size.compare
                (Option.compare Float.compare)
                a.parent_size b.parent_size
            in
            if cmp <> 0 then cmp
            else
              let cmp =
                Geometry.Size.compare Available_space.compare a.available_space
                  b.available_space
              in
              if cmp <> 0 then cmp
              else
                Geometry.Line.compare Bool.compare
                  a.vertical_margins_are_collapsible
                  b.vertical_margins_are_collapsible

let equal a b = compare a b = 0
let pp fmt t = Format.pp_print_string fmt (to_string t)
