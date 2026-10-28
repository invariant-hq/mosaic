type t = {
  min : Compact_length.t; (* Minimum track sizing function *)
  max : Compact_length.t; (* Maximum track sizing function *)
}

let make ~min ~max = { min; max }
let min t = t.min
let max t = t.max

(* Common constructors *)
let auto = { min = Compact_length.auto; max = Compact_length.auto }

let min_content =
  { min = Compact_length.min_content; max = Compact_length.min_content }

let max_content =
  { min = Compact_length.max_content; max = Compact_length.max_content }

let zero = { min = Compact_length.zero; max = Compact_length.zero }
let fr value = { min = Compact_length.auto; max = Compact_length.fr value }

let length value =
  { min = Compact_length.length value; max = Compact_length.length value }

let percent value =
  { min = Compact_length.percent value; max = Compact_length.percent value }

(* Create from a length_percentage *)
let from_length_percentage lp =
  (* length_percentage is already a Compact_length.t, so we can use it
     directly *)
  { min = lp; max = lp }

(* Create a fit-content track sizing function *)
let fit_content lp = { min = Compact_length.auto; max = lp }

(* Create a minmax function *)
let minmax ~min ~max = { min; max }

let equal a b =
  Compact_length.equal a.min b.min && Compact_length.equal a.max b.max

let compare a b =
  let c = Compact_length.compare a.min b.min in
  if c <> 0 then c else Compact_length.compare a.max b.max

let to_string t =
  if Compact_length.equal t.min t.max then Compact_length.to_string t.min
  else
    Printf.sprintf "minmax(%s, %s)"
      (Compact_length.to_string t.min)
      (Compact_length.to_string t.max)

let pp fmt t = Format.pp_print_string fmt (to_string t)

(* Check if at least one component is a fixed sizing function *)
let has_fixed_component t =
  Compact_length.is_length t.min
  || Compact_length.is_percent t.min
  || Compact_length.is_length t.max
  || Compact_length.is_percent t.max

(* Get the minimum sizing function *)
let min_sizing_function t = t.min

(* Get the maximum sizing function *)
let max_sizing_function t = t.max

(* Min track sizing function helpers *)
module Min = struct
  let is_intrinsic t = Compact_length.is_intrinsic t.min
  let is_min_or_max_content t = Compact_length.is_min_or_max_content t.min
  let is_fr t = Compact_length.is_fr t.min
  let is_auto t = Compact_length.is_auto t.min
  let is_min_content t = Compact_length.is_min_content t.min
  let is_max_content t = Compact_length.is_max_content t.min

  let definite_value t parent_size =
    match Compact_length.get_tag t.min with
    | tag when tag = Compact_length.length_tag ->
        Some (Compact_length.get_value t.min)
    | tag when tag = Compact_length.percent_tag ->
        Option.map
          (fun size -> Compact_length.get_value t.min *. size)
          parent_size
    | _ -> None

  let definite_value_with_calc t parent_size calc_resolver =
    match Compact_length.get_tag t.min with
    | tag when tag = Compact_length.length_tag ->
        Some (Compact_length.get_value t.min)
    | tag when tag = Compact_length.percent_tag ->
        Option.map
          (fun size -> Compact_length.get_value t.min *. size)
          parent_size
    | _ when Compact_length.is_calc t.min ->
        Option.map
          (fun size -> calc_resolver (Compact_length.get_calc_index t.min) size)
          parent_size
    | _ -> None

  let resolved_percentage_size t parent_size =
    Compact_length.resolved_percentage_size t.min parent_size

  let uses_percentage t = Compact_length.uses_percentage t.min

  let behaves_as_auto t parent_size =
    is_auto t || (Option.is_none parent_size && uses_percentage t)
end

(* Max track sizing function helpers *)
module Max = struct
  let is_intrinsic t = Compact_length.is_intrinsic t.max
  let is_max_content_alike t = Compact_length.is_max_content_alike t.max
  let is_fr t = Compact_length.is_fr t.max
  let is_auto t = Compact_length.is_auto t.max
  let is_min_content t = Compact_length.is_min_content t.max
  let is_max_content t = Compact_length.is_max_content t.max
  let is_fit_content t = Compact_length.is_fit_content t.max
  let is_max_or_fit_content t = Compact_length.is_max_or_fit_content t.max

  let fr_value t =
    if Compact_length.is_fr t.max then Compact_length.get_value t.max else 0.0

  let has_definite_value t parent_size =
    match Compact_length.get_tag t.max with
    | tag when tag = Compact_length.length_tag -> true
    | tag when tag = Compact_length.percent_tag -> Option.is_some parent_size
    | _ when Compact_length.is_calc t.max -> Option.is_some parent_size
    | _ -> false

  let definite_value t parent_size =
    match Compact_length.get_tag t.max with
    | tag when tag = Compact_length.length_tag ->
        Some (Compact_length.get_value t.max)
    | tag when tag = Compact_length.percent_tag ->
        Option.map
          (fun size -> Compact_length.get_value t.max *. size)
          parent_size
    | _ -> None

  let definite_value_with_calc t parent_size calc_resolver =
    match Compact_length.get_tag t.max with
    | tag when tag = Compact_length.length_tag ->
        Some (Compact_length.get_value t.max)
    | tag when tag = Compact_length.percent_tag ->
        Option.map
          (fun size -> Compact_length.get_value t.max *. size)
          parent_size
    | _ when Compact_length.is_calc t.max ->
        Option.map
          (fun size -> calc_resolver (Compact_length.get_calc_index t.max) size)
          parent_size
    | _ -> None

  let definite_limit t parent_size =
    match Compact_length.get_tag t.max with
    | tag when tag = Compact_length.fit_content_px_tag ->
        Some (Compact_length.get_value t.max)
    | tag when tag = Compact_length.fit_content_percent_tag ->
        Option.map
          (fun size -> Compact_length.get_value t.max *. size)
          parent_size
    | _ -> definite_value t parent_size

  let definite_limit_with_calc t parent_size calc_resolver =
    match Compact_length.get_tag t.max with
    | tag when tag = Compact_length.fit_content_px_tag ->
        Some (Compact_length.get_value t.max)
    | tag when tag = Compact_length.fit_content_percent_tag ->
        Option.map
          (fun size -> Compact_length.get_value t.max *. size)
          parent_size
    | _ -> definite_value_with_calc t parent_size calc_resolver

  let resolved_percentage_size t parent_size =
    Compact_length.resolved_percentage_size t.max parent_size

  let uses_percentage t = Compact_length.uses_percentage t.max
end
