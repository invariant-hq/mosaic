type t = float

(* Tag constants - using the lower 8 bits for tagging *)

(* Non-calc tags must always set at least one of the lower 3 bits. Calc values
   rely on those bits being 0 for detection via [is_calc]. *)
let calc_tag = 0b0000_0000 (* calc uses tag 0 with lower 3 bits *)
let length_tag = 0b0000_0001
let percent_tag = 0b0000_0010
let auto_tag = 0b0000_0011
let fr_tag = 0b0000_0100
let min_content_tag = 0b0000_0111
let max_content_tag = 0b0000_1111
let fit_content_px_tag = 0b0001_0111
let fit_content_percent_tag = 0b0001_1111

(* Bit manipulation constants *)

let tag_mask = 0b1111_1111
let calc_tag_mask = 0b0000_0111 (* Only lower 3 bits for calc tag detection *)
let value_mask = lnot tag_mask

(* Helper functions for bit manipulation *)

let create_tagged_value value tag =
  if Sys.word_size = 64 then
    (* On 64-bit platforms, store f32 value in upper 32 bits, tag in lower 8
       bits This preserves full f32 precision, matching Taffy's approach *)
    let value_bits = Int32.bits_of_float value in
    let shifted_value = Int64.shift_left (Int64.of_int32 value_bits) 32 in
    let tagged_bits = Int64.logor shifted_value (Int64.of_int tag) in
    Int64.float_of_bits tagged_bits
  else
    (* On 32-bit platforms, use the old approach TODO: implement a better
       solution for 32-bit platforms *)
    let bits = Int64.bits_of_float value in
    let tagged_bits =
      Int64.logor
        (Int64.logand bits (Int64.of_int value_mask))
        (Int64.of_int tag)
    in
    Int64.float_of_bits tagged_bits

let get_tag t =
  let bits = Int64.bits_of_float t in
  Int64.to_int (Int64.logand bits (Int64.of_int tag_mask))

(* Inlined: a float returned from a call is boxed, and [is_zero], which
   layout calls for every side of every rect it resolves, only compares it. *)
let[@inline] get_value t =
  if Sys.word_size = 64 then
    let bits = Int64.bits_of_float t in
    (* Extract upper 32 bits and convert back to float *)
    let value_bits = Int64.shift_right_logical bits 32 in
    Int32.float_of_bits (Int64.to_int32 value_bits)
  else
    (* On 32-bit platforms, use the old approach *)
    let bits = Int64.bits_of_float t in
    let cleared_bits = Int64.logand bits (Int64.of_int value_mask) in
    Int64.float_of_bits cleared_bits

(* Special handling for calc - stores an integer index instead of a float
   value *)
let create_calc_value index =
  if Sys.word_size = 64 then
    (* Store the index in the upper 32 bits, with calc_tag in lower 3 bits *)
    let shifted_index = Int64.shift_left (Int64.of_int index) 32 in
    let tagged_bits = Int64.logor shifted_index (Int64.of_int calc_tag) in
    Int64.float_of_bits tagged_bits
  else
    (* Store the index in the upper bits, with calc_tag in lower 3 bits *)
    let shifted_index = Int64.shift_left (Int64.of_int index) 3 in
    let tagged_bits = Int64.logor shifted_index (Int64.of_int calc_tag) in
    Int64.float_of_bits tagged_bits

let get_calc_index t =
  let bits = Int64.bits_of_float t in
  if Sys.word_size = 64 then Int64.to_int (Int64.shift_right_logical bits 32)
  else Int64.to_int (Int64.shift_right_logical bits 3)

let is_calc t =
  let bits = Int64.bits_of_float t in
  let tag = Int64.to_int (Int64.logand bits (Int64.of_int calc_tag_mask)) in
  tag = calc_tag

(* Constructors *)

let length value = create_tagged_value value length_tag
let percent value = create_tagged_value value percent_tag
let auto = create_tagged_value 0.0 auto_tag
let fr value = create_tagged_value value fr_tag
let min_content = create_tagged_value 0.0 min_content_tag
let max_content = create_tagged_value 0.0 max_content_tag
let fit_content_px value = create_tagged_value value fit_content_px_tag

let fit_content_percent value =
  create_tagged_value value fit_content_percent_tag

let calc index = create_calc_value index

(* Constants *)

let zero = length 0.0

(* Inspection functions *)

let is_length t = get_tag t = length_tag
let is_percent t = get_tag t = percent_tag
let is_auto t = get_tag t = auto_tag
let is_fr t = get_tag t = fr_tag
let is_min_content t = get_tag t = min_content_tag
let is_max_content t = get_tag t = max_content_tag
let is_fit_content_px t = get_tag t = fit_content_px_tag
let is_fit_content_percent t = get_tag t = fit_content_percent_tag

let is_length_or_percentage t =
  let tag = get_tag t in
  tag = length_tag || tag = percent_tag

let is_intrinsic t =
  let tag = get_tag t in
  tag = min_content_tag || tag = max_content_tag || tag = fit_content_px_tag
  || tag = fit_content_percent_tag
  || tag = auto_tag

let is_fit_content t =
  let tag = get_tag t in
  tag = fit_content_px_tag || tag = fit_content_percent_tag

(* Additional helper functions *)

let is_zero t = get_tag t = length_tag && get_value t = 0.0

let is_max_or_fit_content t =
  let tag = get_tag t in
  tag = max_content_tag || tag = fit_content_px_tag
  || tag = fit_content_percent_tag

let is_max_content_alike t =
  let tag = get_tag t in
  tag = auto_tag || tag = max_content_tag || tag = fit_content_px_tag
  || tag = fit_content_percent_tag

let is_min_or_max_content t =
  let tag = get_tag t in
  tag = min_content_tag || tag = max_content_tag

let uses_percentage t =
  let tag = get_tag t in
  tag = percent_tag || tag = fit_content_percent_tag || is_calc t

(* Type for calc resolver function *)
type calc_resolver = int -> float -> float

let resolved_percentage_size t parent_size =
  match get_tag t with
  | tag when tag = percent_tag ->
      (* Emulate f32 arithmetic to match Taffy's behavior *)
      let result = get_value t *. parent_size in
      (* Round to f32 precision *)
      Some (Int32.float_of_bits (Int32.bits_of_float result))
  | _ -> None

let resolved_percentage_size_with_calc t parent_size calc_resolver =
  match get_tag t with
  | tag when tag = percent_tag ->
      (* Emulate f32 arithmetic to match Taffy's behavior *)
      let result = get_value t *. parent_size in
      (* Round to f32 precision *)
      Some (Int32.float_of_bits (Int32.bits_of_float result))
  | _ when is_calc t -> Some (calc_resolver (get_calc_index t) parent_size)
  | _ -> None

(* Value extraction *)

let value t =
  let tag = get_tag t in
  if
    tag = auto_tag || tag = min_content_tag || tag = max_content_tag
    || is_calc t
  then failwith "Cannot get value from auto/min-content/max-content/calc"
  else get_value t

(* Pretty printing for debugging *)

let to_string t =
  if is_calc t then Printf.sprintf "calc(#%d)" (get_calc_index t)
  else
    match get_tag t with
    | tag when tag = length_tag -> Printf.sprintf "%gpx" (get_value t)
    | tag when tag = percent_tag -> Printf.sprintf "%g%%" (get_value t *. 100.0)
    | tag when tag = auto_tag -> "auto"
    | tag when tag = fr_tag -> Printf.sprintf "%gfr" (get_value t)
    | tag when tag = min_content_tag -> "min-content"
    | tag when tag = max_content_tag -> "max-content"
    | tag when tag = fit_content_px_tag ->
        Printf.sprintf "fit-content(%gpx)" (get_value t)
    | tag when tag = fit_content_percent_tag ->
        Printf.sprintf "fit-content(%g%%)" (get_value t *. 100.0)
    | _ -> "unknown"

(* Comparison *)

let equal a b =
  if is_calc a && is_calc b then get_calc_index a = get_calc_index b
  else if is_calc a || is_calc b then false
  else
    let tag_a = get_tag a in
    let tag_b = get_tag b in
    if tag_a <> tag_b then false
    else if
      tag_a = auto_tag || tag_a = min_content_tag || tag_a = max_content_tag
    then true
    else Float.abs (get_value a -. get_value b) < Float.epsilon

let compare a b =
  if is_calc a && is_calc b then
    Int.compare (get_calc_index a) (get_calc_index b)
  else if is_calc a then -1
  else if is_calc b then 1
  else
    let tag_a = get_tag a in
    let tag_b = get_tag b in
    let tag_cmp = Int.compare tag_a tag_b in
    if tag_cmp <> 0 then tag_cmp
    else if
      tag_a = auto_tag || tag_a = min_content_tag || tag_a = max_content_tag
    then 0
    else Float.compare (get_value a) (get_value b)

let pp fmt t = Format.pp_print_string fmt (to_string t)
