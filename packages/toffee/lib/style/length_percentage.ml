type t = Compact_length.t

(* Constructors *)

let length value = Compact_length.length value
let percent value = Compact_length.percent value
let calc index = Compact_length.calc index
let px value = length value
let pct value = percent (value /. 100.0)

(* Constants *)

let zero = Compact_length.zero

(* Inspection *)

let is_length t = Compact_length.is_length t
let is_percent t = Compact_length.is_percent t
let is_calc t = Compact_length.is_calc t

(* Value extraction *)

let value t = Compact_length.value t

(* Conversion/Resolution *)

let resolve t context =
  if is_length t then value t
  else if is_percent t then
    (* Emulate f32 arithmetic to match Taffy's behavior *)
    let result = context *. value t in
    (* Round to f32 precision *)
    Int32.float_of_bits (Int32.bits_of_float result)
  else
    failwith
      "Length_percentage.resolve: calc requires resolve_with_calc; intrinsic \
       values are unsupported"

let resolve_with_calc t context calc_resolver =
  if is_length t then value t
  else if is_percent t then
    (* Emulate f32 arithmetic to match Taffy's behavior *)
    let result = context *. value t in
    (* Round to f32 precision *)
    Int32.float_of_bits (Int32.bits_of_float result)
  else if is_calc t then calc_resolver (Compact_length.get_calc_index t) context
  else
    failwith
      "Length_percentage.resolve_with_calc: unsupported tag (expected \
       length/percent/calc)"

(* Pretty printing *)

let to_string = Compact_length.to_string

(* Comparison *)

let equal = Compact_length.equal
let compare = Compact_length.compare
let pp = Compact_length.pp

(* Additional helpers *)

let uses_percentage t = Compact_length.uses_percentage t

let resolved_percentage_size t parent_size =
  Compact_length.resolved_percentage_size t parent_size

let resolved_percentage_size_with_calc t parent_size calc_resolver =
  Compact_length.resolved_percentage_size_with_calc t parent_size calc_resolver

(* Resolve functions from Taffy's resolve.rs *)

let maybe_resolve t context calc_resolver =
  if is_length t then Some (value t)
  else if is_percent t then
    match context with
    | None -> None
    | Some dim ->
        (* Emulate f32 arithmetic to match Taffy's behavior *)
        let result = dim *. value t in
        (* Round to f32 precision *)
        Some (Int32.float_of_bits (Int32.bits_of_float result))
  else if is_calc t then
    match context with
    | None -> None
    | Some dim -> Some (calc_resolver (Compact_length.get_calc_index t) dim)
  else
    failwith
      "Length_percentage.maybe_resolve: unsupported tag (expected \
       length/percent/calc)"

(* [maybe_resolve], with 0 for [None], and without its option: layout
   resolves every padding and border side of every node with it. *)
let resolve_or_zero t context calc_resolver =
  if is_length t then value t
  else if is_percent t then
    match context with
    | None -> 0.0
    | Some dim ->
        (* Emulate f32 arithmetic to match Taffy's behavior *)
        Int32.float_of_bits (Int32.bits_of_float (dim *. value t))
  else if is_calc t then
    match context with
    | None -> 0.0
    | Some dim -> calc_resolver (Compact_length.get_calc_index t) dim
  else
    failwith
      "Length_percentage.resolve_or_zero: unsupported tag (expected \
       length/percent/calc)"
