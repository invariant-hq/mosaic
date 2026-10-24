(* Ticks: the search of Talbot, Lin and Hanrahan ("An Extension of Wilkinson's
   Algorithm for Positioning Tick Labels on Axes", InfoVis 2010) and the label
   grammar of raven's hugin kit (ticks.mli), copied for float domains measured
   in cells. *)

type family = Linear | Log of float | Time of int
type tick = { value : float; label : string; context : string option }
type t = { major : tick list; note : string option }

(* {1 Decimals} *)

let pow10 k = Float.of_string ("1e" ^ string_of_int k)

(* [decompose v] is the digits [d] (no trailing zeros) and the exponent [e] of
   the shortest decimal that reads back as [|v|], so [|v| = d.ddd × 10^e]. *)
let decompose_printed a =
  let rec go p =
    let s = Printf.sprintf "%.*e" p a in
    if p >= 16 || Float.equal (float_of_string s) a then s else go (p + 1)
  in
  let s = go 0 in
  let i = String.index s 'e' in
  let e = int_of_string (String.sub s (i + 1) (String.length s - i - 1)) in
  let digits = String.concat "" (String.split_on_char '.' (String.sub s 0 i)) in
  let n = ref (String.length digits) in
  while !n > 1 && digits.[!n - 1] = '0' do
    decr n
  done;
  (String.sub digits 0 !n, e)

let powers = Array.init 10 (fun k -> Float.of_string ("1e" ^ string_of_int k))

(* Most tick values are short decimals, whose digits an integer gives without
   searching the shortest float printing. *)
let rec short a k =
  if k >= Array.length powers then None
  else
    let r = Float.round (a *. powers.(k)) in
    if r < 1e15 && r /. powers.(k) = a then Some (Float.to_int r, k)
    else short a (k + 1)

let decompose v =
  let a = Float.abs v in
  match short a 0 with
  | Some (n, k) when n > 0 ->
      let s = string_of_int n in
      let len = ref (String.length s) in
      while !len > 1 && s.[!len - 1] = '0' do
        decr len
      done;
      (String.sub s 0 !len, String.length s - 1 - k)
  | _ -> decompose_printed a

(* The exponent of the last significant digit of [v], and of its first. *)
let last_digit v =
  let d, e = decompose v in
  e - (String.length d - 1)

let magnitude v = snd (decompose v)

let floor_div a b =
  let q = a / b in
  if a mod b <> 0 && a < 0 <> (b < 0) then q - 1 else q

let superscript n =
  let digit c =
    match c with
    | '-' -> "⁻"
    | '0' -> "⁰"
    | '1' -> "¹"
    | '2' -> "²"
    | '3' -> "³"
    | '4' -> "⁴"
    | '5' -> "⁵"
    | '6' -> "⁶"
    | '7' -> "⁷"
    | '8' -> "⁸"
    | _ -> "⁹"
  in
  String.concat ""
    (List.map digit (List.of_seq (String.to_seq (string_of_int n))))

(* [group s] puts a comma between each group of three digits of the integer
   part of the number [s]. *)
let group s =
  let sign, s =
    if String.length s > 0 && s.[0] = '-' then
      ("-", String.sub s 1 (String.length s - 1))
    else ("", s)
  in
  let int, frac =
    match String.index_opt s '.' with
    | None -> (s, "")
    | Some i -> (String.sub s 0 i, String.sub s i (String.length s - i))
  in
  let n = String.length int in
  let b = Buffer.create (n + (n / 3) + String.length frac + 1) in
  Buffer.add_string b sign;
  String.iteri
    (fun i c ->
      if i > 0 && (n - i) mod 3 = 0 then Buffer.add_char b ',';
      Buffer.add_char b c)
    int;
  Buffer.add_string b frac;
  Buffer.contents b

let fixed d v =
  let s = Printf.sprintf "%.*f" d v in
  if String.length s > 0 && s.[0] = '-' && float_of_string s = 0. then
    String.sub s 1 (String.length s - 1)
  else s

(* [exactly v] writes [v] with the fewest digits that write it exactly: plain
   and grouped from 10⁻⁴ to below 10⁶, in powers of ten otherwise. *)
let exactly v =
  let a = Float.abs v in
  if a = 0. then "0"
  else if a >= 1e-4 && a < 1e6 then group (fixed (max 0 (-last_digit a)) a)
  else
    let d, e = decompose a in
    let mantissa =
      if String.length d = 1 then d
      else String.sub d 0 1 ^ "." ^ String.sub d 1 (String.length d - 1)
    in
    if mantissa = "1" then "10" ^ superscript e
    else mantissa ^ "×10" ^ superscript e

(* {2 Quantities} *)

let quantities vs =
  let n = Array.length vs in
  let nonzero = List.filter (fun v -> v <> 0.) (Array.to_list vs) in
  let p_e vs =
    match List.filter (fun v -> v <> 0.) vs with
    | [] -> (0, 0)
    | nz ->
        List.fold_left
          (fun (p, e) v ->
            let d, m = decompose v in
            (min p (m - (String.length d - 1)), max e m))
          (max_int, min_int) nz
  in
  let p, e = p_e nonzero in
  let positive = Array.for_all (fun v -> v > 0.) vs
  and negative = Array.for_all (fun v -> v < 0.) vs in
  let offset =
    if n < 2 || (not (positive || negative)) || e - p + 1 <= 7 then None
    else
      let lo = Array.fold_left Float.min infinity vs
      and hi = Array.fold_left Float.max neg_infinity vs in
      if hi = lo then None
      else
        let q = magnitude (hi -. lo) + 1 in
        (* scaled by an exact power of ten, so that the offset is the float
           nearest its decimal *)
        let toward_zero x =
          let t = if x > 0. then Float.floor else Float.ceil in
          if q >= 0 then t (x /. pow10 q) *. pow10 q
          else t (x *. pow10 (-q)) /. pow10 (-q)
        in
        let o = toward_zero (if positive then lo else hi) in
        if o = 0. then None else Some o
  in
  let vs, p, e =
    match offset with
    | None -> (vs, p, e)
    | Some o ->
        let unit = pow10 p in
        let vs =
          Array.map (fun v -> Float.round ((v -. o) /. unit) *. unit) vs
        in
        let p', e = p_e (Array.to_list vs) in
        (vs, min p p', e)
  in
  let k = if e < -4 || e > 5 then 3 * floor_div e 3 else 0 in
  let d = max 0 (k - p) in
  let scaled v = if k = 0 then v else v /. pow10 k in
  let written = Array.map (fun v -> fixed d (scaled v)) vs in
  let grouped = Array.exists (fun v -> Float.abs (scaled v) >= 1e4) vs in
  let written = if grouped then Array.map group written else written in
  let factor = if k = 0 then None else Some ("×10" ^ superscript k) in
  let note =
    match (factor, offset) with
    | None, None -> None
    | Some f, None -> Some f
    | f, Some o ->
        let sign = if o < 0. then "-" else "+" in
        let f = match f with None -> "" | Some f -> f ^ " " in
        Some (f ^ sign ^ exactly o)
  in
  (Array.map (fun l -> (l, None)) written, note)

(* {2 Logarithms} *)

(* [power_of base v] is [Some (n, i)] iff [v] is [n × base^i] for an integer
   [n] in \[[1];[base - 1]\] ([n = 1] unless [base] is an integer). *)
let power_of base v =
  if not (v > 0.) then None
  else if base = 10. then
    let d, e = decompose v in
    if String.length d = 1 then Some (int_of_string d, e) else None
  else
    let i =
      Float.to_int (Float.floor ((Float.log v /. Float.log base) +. 1e-9))
    in
    let p = Float.pow base (Float.of_int i) in
    let n = Float.round (v /. p) in
    let integer = Float.is_integer base in
    if
      Float.abs ((n *. p) -. v) <= 1e-9 *. v
      && n >= 1.
      && (n = 1. || (integer && n < base))
    then Some (Float.to_int n, i)
    else None

let logarithms base vs =
  let powers = Array.map (power_of base) vs in
  if not (Array.for_all Option.is_some powers) then None
  else
    let label v (n, i) =
      if base = 10. then
        let small = Array.for_all (fun v -> v >= 1e-3 && v <= 1e4) vs in
        if small then
          let s = fixed (max 0 (-last_digit v)) v in
          if v >= 1e4 then group s else s
        else if n = 1 then "10" ^ superscript i
        else string_of_int n ^ "×10" ^ superscript i
      else
        let b =
          if base = Float.exp 1. then "e"
          else if Float.is_integer base then string_of_int (Float.to_int base)
          else Printf.sprintf "%g" base
        in
        let p = b ^ superscript i in
        if n = 1 then p else string_of_int n ^ "×" ^ p
    in
    Some
      (Array.mapi (fun k v -> (label v (Option.get powers.(k)), None)) vs, None)

(* {2 Time} *)

(* Days from 1970-01-01 to a civil date, and back (Howard Hinnant's
   chrono-compatible algorithms). *)
let days_from_civil y m d =
  let y = if m <= 2 then y - 1 else y in
  let era = floor_div y 400 in
  let yoe = y - (era * 400) in
  let mp = (m + 9) mod 12 in
  let doy = (((153 * mp) + 2) / 5) + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let civil_from_days z =
  let z = z + 719468 in
  let era = floor_div z 146097 in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let y = yoe + (era * 400) in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  ((if m <= 2 then y + 1 else y), m, d)

let months =
  [|
    "Jan";
    "Feb";
    "Mar";
    "Apr";
    "May";
    "Jun";
    "Jul";
    "Aug";
    "Sep";
    "Oct";
    "Nov";
    "Dec";
  |]

(* The local fields of an instant, its fraction of a second in microseconds. *)
type fields = {
  year : int;
  month : int;
  day : int;
  hour : int;
  minute : int;
  second : int;
  micro : int;
}

let fields offset t =
  let local = t +. Float.of_int offset in
  let micros = Float.round (local *. 1e6) in
  let secs = Float.floor (micros /. 1e6) in
  let micro = Float.to_int (micros -. (secs *. 1e6)) in
  let secs = Float.to_int secs in
  let days = floor_div secs 86400 in
  let sod = secs - (days * 86400) in
  let year, month, day = civil_from_days days in
  {
    year;
    month;
    day;
    hour = sod / 3600;
    minute = sod mod 3600 / 60;
    second = sod mod 60;
    micro;
  }

let times offset vs =
  let fs = Array.map (fields offset) vs in
  let all p = Array.for_all p fs in
  let midnight f = f.hour = 0 && f.minute = 0 && f.second = 0 && f.micro = 0 in
  let year f = Printf.sprintf "%d" f.year in
  let hm f = Printf.sprintf "%02d:%02d" f.hour f.minute in
  let pair =
    if all (fun f -> midnight f && f.month = 1 && f.day = 1) then fun f ->
      (year f, None)
    else if all (fun f -> midnight f && f.day = 1) then fun f ->
      (months.(f.month - 1), Some (year f))
    else if all midnight then fun f ->
      ( string_of_int f.day,
        Some (Printf.sprintf "%s %d" months.(f.month - 1) f.year) )
    else if all (fun f -> f.second = 0 && f.micro = 0) then fun f ->
      (hm f, Some (Printf.sprintf "%d %s" f.day months.(f.month - 1)))
    else if all (fun f -> f.micro = 0) then fun f ->
      (Printf.sprintf ":%02d" f.second, Some (hm f))
    else
      let digits =
        Array.fold_left
          (fun d f ->
            let rec need d m =
              if m mod 10 = 0 && d > 1 then need (d - 1) (m / 10) else d
            in
            max d (need 6 f.micro))
          1 fs
      in
      fun f ->
        let frac = Printf.sprintf "%06d" f.micro in
        ( "." ^ String.sub frac 0 digits,
          Some (Printf.sprintf "%s:%02d" (hm f) f.second) )
  in
  let labelled = Array.map pair fs in
  (* A context is shown on the first tick and where it changes. *)
  let prev = ref None in
  ( Array.map
      (fun (l, c) ->
        let shown = if c = !prev then None else c in
        prev := c;
        (l, shown))
      labelled,
    None )

let label family vs =
  if Array.length vs = 0 then ([||], None)
  else
    match family with
    | Linear -> quantities vs
    | Log base -> (
        match logarithms base vs with Some l -> l | None -> quantities vs)
    | Time offset -> times offset vs

(* {1 Steps} *)

(* A decimal step [m × 10^k]. Its multiples are computed from integers so that
   [3 × 0.1] is [0.3]. *)
let multiple m k i =
  let n = Float.of_int (i * m) in
  if k >= 0 then n *. pow10 k else n /. pow10 (-k)

let width (m, k) = multiple m k 1

(* The integers [i] whose multiples [i × step] lie in \[[a];[b]\], [None] if
   there are more than [limit]. *)
let indices ?(limit = 10_000) (m, k) a b =
  let w = width (m, k) in
  let lo = Float.ceil (a /. w) and hi = Float.floor (b /. w) in
  if not (Float.is_finite lo && Float.is_finite hi) then None
  else if hi -. lo +. 1. > Float.of_int limit then None
  else if hi < lo then Some (0, -1)
  else Some (Float.to_int lo, Float.to_int hi)

let multiples ?(skip = 1) ?(offset = 0) (m, k) a b =
  match indices (m, k) a b with
  | None -> [||]
  | Some (lo, hi) ->
      let out = ref [] in
      for i = hi downto lo do
        let r = ((i mod skip) + skip) mod skip in
        if r = offset then
          let v = multiple m k i in
          if a <= v && v <= b then out := v :: !out
      done;
      Array.of_list !out

(* The decimal step nearest by ratio to a tenth of \[[a];[b]\], in [1], [2],
   [5] and [10] times a power of ten (d3's tick increment). *)
let decimal_step a b =
  let t = (b -. a) /. 10. in
  let k = Float.to_int (Float.floor (Float.log10 t)) in
  let f = t /. pow10 k in
  let m =
    if f >= Float.sqrt 50. then 10
    else if f >= Float.sqrt 10. then 5
    else if f >= Float.sqrt 2. then 2
    else 1
  in
  if m = 10 then (1, k + 1) else (m, k)

(* {2 Calendar intervals} *)

type interval =
  | Seconds of float
  | Days of int
  | Weeks of int
  | Months of int
  | Years of int

let day = 86400.

let duration = function
  | Seconds s -> s
  | Days n -> Float.of_int n *. day
  | Weeks n -> Float.of_int (7 * n) *. day
  | Months n -> Float.of_int (30 * n) *. day
  | Years n -> Float.of_int (365 * n) *. day

let longest = function
  | Months n -> Float.of_int (31 * n) *. day
  | Years n -> Float.of_int (366 * n) *. day
  | i -> duration i

(* The intervals, finest first, each with its rank [i] among the [n] of its
   unit, as the simplicity of a candidate counts them. *)
let intervals =
  let ms = [ (1, 1); (2, 2); (5, 3) ] in
  let sub =
    List.concat_map
      (fun k ->
        List.map (fun (m, i) -> (Seconds (multiple m (k - 3) 1), i, 3)) ms)
      [ 0; 1; 2 ]
  in
  let four mk l = List.mapi (fun i n -> (mk n, i + 1, 4)) l in
  let years =
    List.concat_map
      (fun k ->
        List.map (fun (m, i) -> (Years (m * Float.to_int (pow10 k)), i, 3)) ms)
      [ 0; 1; 2; 3; 4 ]
  in
  Array.of_list
    (sub
    @ four (fun s -> Seconds s) [ 1.; 5.; 15.; 30. ]
    @ four (fun m -> Seconds (60. *. m)) [ 1.; 5.; 15.; 30. ]
    @ four (fun h -> Seconds (3600. *. h)) [ 1.; 3.; 6.; 12. ]
    @ [ (Days 1, 1, 2); (Days 2, 2, 2); (Weeks 1, 1, 2); (Weeks 2, 2, 2) ]
    @ [ (Months 1, 1, 3); (Months 3, 2, 3); (Months 6, 3, 3) ]
    @ years)

(* [boundaries offset iv a b] is the boundaries of [iv] in \[[a];[b]\], local
   time being [offset] seconds from UTC, [None] if more than [limit]. *)
let boundaries ?(limit = 2_000) offset iv a b =
  let off = Float.of_int offset in
  let out = ref [] and count = ref 0 in
  let push t =
    if a <= t && t <= b then (
      incr count;
      out := t :: !out)
  in
  let local_day t = Float.to_int (Float.floor ((t +. off) /. day)) in
  let at_day d = (Float.of_int d *. day) -. off in
  let too_many n = n > limit in
  (match iv with
  | Seconds s ->
      let lo = Float.ceil ((a +. off) /. s)
      and hi = Float.floor ((b +. off) /. s) in
      if not (too_many (Float.to_int (hi -. lo) + 1)) then
        let i = ref lo in
        while !i <= hi do
          push ((!i *. s) -. off);
          i := !i +. 1.
        done
  | Days n | Weeks n ->
      let step, base = match iv with Weeks _ -> (7 * n, 4) | _ -> (n, 0) in
      let d0 = local_day a and d1 = local_day b + 1 in
      if not (too_many ((d1 - d0) / step)) then
        let first = d0 - base in
        let first = (floor_div (first + step - 1) step * step) + base in
        let d = ref first in
        while !d <= d1 do
          push (at_day !d);
          d := !d + step
        done
  | Months n | Years n ->
      let step = match iv with Years _ -> 12 * n | _ -> n in
      let ya, ma, _ = civil_from_days (local_day a) in
      let yb, mb, _ = civil_from_days (local_day b) in
      let i0 = (ya * 12) + ma - 1 and i1 = (yb * 12) + mb - 1 in
      if not (too_many ((i1 - i0) / step)) then
        let i = ref (floor_div (i0 + step - 1) step * step) in
        while !i <= i1 do
          let y = floor_div !i 12 and m = !i - (floor_div !i 12 * 12) + 1 in
          push (at_day (days_from_civil y m 1));
          i := !i + step
        done);
  if too_many !count then None else Some (Array.of_list (List.rev !out))

(* The interval whose duration is nearest by ratio to [t], the coarser of two
   equally near. *)
let nearest_interval t =
  let best = ref 0 and dist = ref infinity in
  Array.iteri
    (fun i (iv, _, _) ->
      let r = Float.abs (Float.log (duration iv /. t)) in
      if r <= !dist then (
        best := i;
        dist := r))
    intervals;
  !best

(* {1 Nice domains} *)

let nice family (a, b) =
  if not (a < b) then (a, b)
  else
    match family with
    | Linear ->
        let rec loop a b prev n =
          if n = 0 then (a, b)
          else
            let ((m, k) as s) = decimal_step a b in
            if Some s = prev then (a, b)
            else
              let w = width (m, k) in
              let a' = multiple m k (Float.to_int (Float.floor (a /. w)))
              and b' = multiple m k (Float.to_int (Float.ceil (b /. w))) in
              if Float.is_finite a' && Float.is_finite b' then
                loop a' b' (Some s) (n - 1)
              else (a, b)
        in
        if Float.abs (a /. width (decimal_step a b)) > 1e15 then (a, b)
        else loop a b None 10
    | Log base ->
        if a <= 0. then (a, b)
        else
          let lb = Float.log base in
          let lo = Float.floor ((Float.log a /. lb) +. 1e-12)
          and hi = Float.ceil ((Float.log b /. lb) -. 1e-12) in
          let power i =
            if base = 10. then pow10 (Float.to_int i) else Float.pow base i
          in
          (power lo, power hi)
    | Time offset ->
        let rec loop a b prev n =
          if n = 0 then (a, b)
          else
            let i = nearest_interval ((b -. a) /. 10.) in
            if Some i = prev then (a, b)
            else
              let iv, _, _ = intervals.(i) in
              let d = longest iv in
              match
                ( boundaries offset iv (a -. d) a,
                  boundaries offset iv b (b +. d) )
              with
              | Some lo, Some hi when lo <> [||] && hi <> [||] ->
                  loop lo.(Array.length lo - 1) hi.(0) (Some i) (n - 1)
              | _ -> (a, b)
        in
        loop a b None 10

(* {1 The search} *)

type labels = float array -> (string * string option) array * string option

type best = {
  score : float;
  n : int;
  key : int list;
  values : float array;
  labelled : (string * string option) array;
  note : string option;
}

type search = {
  norm : float -> float;
  length : float;
  rho_t : float;
  labels : labels;
  extent : string -> float;
  mutable best : best option;
}

let tick_extent st (l, c) =
  match c with
  | None -> st.extent l
  | Some c -> Float.max (st.extent l) (st.extent c)

let beats st score n key =
  match st.best with
  | None -> true
  | Some b ->
      score > b.score
      || score = b.score
         && (n < b.n || (n = b.n && List.compare Int.compare key b.key < 0))

let can_beat st ub = match st.best with None -> true | Some b -> ub >= b.score

let density_bound st n =
  if n -. 1. > st.rho_t then 2. -. ((n -. 1.) /. st.rho_t) else 1.

(* The weights of simplicity, coverage and density, after Talbot et al. *)
let score s c d = (0.25 *. s) +. (0.2 *. c) +. (0.5 *. d)
let worth st s_max n = can_beat st (score s_max 1. (density_bound st n))

let skips st ~simplest ?(last = max_int) f =
  let rec loop j =
    if j <= last && worth st (simplest -. Float.of_int j) 0. then begin
      f j;
      loop (j + 1)
    end
  in
  loop 1

let overlaps st positions labels =
  let rec loop k =
    k + 1 < Array.length positions
    && ((positions.(k + 1) -. positions.(k)) *. st.length
        < (tick_extent st labels.(k) +. tick_extent st labels.(k + 1)) /. 2.
       || loop (k + 1))
  in
  loop 0

let sq x = x *. x

(* [vs] increasing and their positions, both in increasing order of
   position. *)
let arrange norm vs =
  let n = Array.length vs in
  let positions = Array.map norm vs in
  if n > 0 && positions.(0) > positions.(n - 1) then
    let rev a = Array.init n (fun i -> a.(n - 1 - i)) in
    (rev vs, rev positions)
  else (vs, positions)

let consider st ~s_term ~key vs =
  let n = Array.length vs in
  if n > 0 then begin
    let vs, positions = arrange st.norm vs in
    let p1 = positions.(0) and pn = positions.(n - 1) in
    let c = 1. -. (50. *. (sq (1. -. pn) +. sq p1)) in
    let rho = if n = 1 then 1. else Float.of_int (n - 1) /. (pn -. p1) in
    let d = 2. -. Float.max (rho /. st.rho_t) (st.rho_t /. rho) in
    let score = score s_term c d in
    if beats st score n key then
      let labelled, note = st.labels vs in
      if not (overlaps st positions labelled) then
        st.best <- Some { score; n; key; values = vs; labelled; note }
  end

let residue i j = ((i mod j) + j) mod j
let q_steps = [| (1, 0); (5, 0); (2, 0); (25, -1) |]
let q_rank i = Float.of_int i /. Float.of_int (Array.length q_steps - 1)

let skips_to_q j i =
  let rec mantissa n = if n mod 10 = 0 then mantissa (n / 10) else n in
  Array.exists (fun (m, _) -> m = mantissa (j * fst q_steps.(i))) q_steps

let decimal_family st ~v (a, b) =
  let mag = Float.max (Float.abs a) (Float.abs b) in
  let z_top = Float.to_int (Float.floor (Float.log10 mag)) + 2 in
  let step (m, dk) z = (m, z + dk) in
  let candidate j i z r =
    let vs = multiples ~skip:j ~offset:r (step q_steps.(i) z) a b in
    let s_term = 1. -. q_rank i -. Float.of_int j +. v vs in
    consider st ~s_term ~key:[ 0; j; i; -z; r ] vs
  in
  (* A lone tick: [j] beyond the multiples of the coarsest power of ten that
     has one in the domain. *)
  let rec seed z =
    if z > -400 then
      match multiples (step q_steps.(0) z) a b with
      | [||] -> seed (z - 1)
      | vs ->
          let w = width (step q_steps.(0) z) in
          let first = Float.to_int (Float.round (vs.(0) /. w)) in
          let j = Array.length vs in
          candidate j 0 z (residue first j)
  in
  let rec descend j i z =
    let s = step q_steps.(i) z in
    let delta = Float.of_int j *. width s in
    let n_lb = ((b -. a -. (2. *. delta)) /. delta) +. 1. in
    let s_max = 2. -. q_rank i -. Float.of_int j in
    if worth st s_max n_lb && (b -. a) /. delta < 1e4 && delta > 0. then begin
      for r = 0 to j - 1 do
        candidate j i z r
      done;
      descend j i (z - 1)
    end
  in
  seed z_top;
  skips st ~simplest:2. (fun j ->
      for i = 0 to Array.length q_steps - 1 do
        let s_max = 2. -. q_rank i -. Float.of_int j in
        if skips_to_q j i && worth st s_max 0. then descend j i z_top
      done)

let power base n i =
  if base = 10. then multiple n i 1
  else Float.of_int n *. Float.pow base (Float.of_int i)

let powers_family st ~v base (a, b) =
  let lb = Float.log base in
  let e_lo = Float.to_int (Float.floor (Float.log a /. lb)) - 1
  and e_hi = Float.to_int (Float.ceil (Float.log b /. lb)) + 1 in
  let collect ns keep =
    let out = ref [] in
    for i = e_hi downto e_lo do
      if keep i then
        List.iter
          (fun n ->
            let x = power base n i in
            if a <= x && x <= b then out := x :: !out)
          (List.rev ns)
    done;
    Array.of_list (List.sort_uniq Float.compare !out)
  in
  let inside =
    Float.floor (Float.log b /. lb) -. Float.ceil (Float.log a /. lb) -. 1.
  in
  let form i ns =
    let per = Float.of_int (List.length ns) in
    if worth st (2. -. (Float.of_int i /. 2.) -. 1.) (per *. (inside -. 1.))
    then
      let vs = collect ns (fun _ -> true) in
      consider st
        ~s_term:(1. -. (Float.of_int i /. 2.) -. 1. +. v vs)
        ~key:[ 1; 1; i; 0; 0 ] vs
  in
  if base = 10. then form 1 [ 1; 2; 5 ];
  if Float.is_integer base && base > 2. then
    form 2 (List.init (Float.to_int base - 1) (fun n -> n + 1));
  skips st ~simplest:2.
    ~last:(e_hi - e_lo + 1)
    (fun j ->
      if
        worth st (2. -. Float.of_int j) (Float.floor (inside /. Float.of_int j))
      then
        for r = 0 to j - 1 do
          let vs = collect [ 1 ] (fun i -> residue i j = r) in
          consider st
            ~s_term:(1. -. Float.of_int j +. v vs)
            ~key:[ 1; j; 0; 0; r ] vs
        done)

let calendar_family st offset (a, b) =
  let count = Array.length intervals in
  let length = b -. a in
  let pick iv j r =
    match boundaries offset iv a b with
    | None -> None
    | Some all ->
        Some
          (Array.of_list
             (List.filteri
                (fun k _ -> k >= r && (k - r) mod j = 0)
                (Array.to_list all)))
  in
  let candidate j idx r =
    let iv, i, n = intervals.(idx) in
    match pick iv j r with
    | None | Some [||] -> false
    | Some vs ->
        let s_term =
          1. -. (Float.of_int (i - 1) /. Float.of_int (n - 1)) -. Float.of_int j
        in
        consider st ~s_term ~key:[ 3; j; i; count - 1 - idx; r ] vs;
        true
  in
  let rec seed idx =
    if idx >= 0 then
      let iv, _, _ = intervals.(idx) in
      match boundaries offset iv a b with
      | Some [||] | None -> seed (idx - 1)
      | Some vs -> ignore (candidate (Array.length vs) idx 0)
  in
  seed (count - 1);
  skips st ~simplest:1. (fun j ->
      for rank = 1 to 4 do
        for idx = count - 1 downto 0 do
          let iv, i, n = intervals.(idx) in
          let s_max =
            1.
            -. (Float.of_int (i - 1) /. Float.of_int (n - 1))
            -. Float.of_int j
          in
          let span = Float.of_int j *. longest iv in
          let n_lb = ((length -. (2. *. span)) /. span) +. 1. in
          if i = rank && worth st s_max n_lb then
            let rec offsets r =
              if r < j && candidate j idx r then offsets (r + 1)
            in
            offsets 0
        done
      done)

(* About ten values of the scale's own family set the density target. *)
let reference family (a, b) =
  let decimal () = multiples (decimal_step a b) a b in
  match family with
  | Linear -> decimal ()
  | Log base ->
      let lb = Float.log base in
      let i0 = Float.to_int (Float.floor (Float.log a /. lb))
      and i1 = Float.to_int (Float.ceil (Float.log b /. lb)) in
      let vs =
        List.filter
          (fun v -> a <= v && v <= b)
          (List.init (i1 - i0 + 1) (fun k -> power base 1 (i0 + k)))
      in
      if 2 * List.length vs < 10 then decimal () else Array.of_list vs
  | Time offset ->
      let rec from i =
        if i < 0 then [||]
        else
          let iv, _, _ = intervals.(i) in
          match boundaries offset iv a b with
          | Some vs when vs <> [||] -> vs
          | _ -> from (i - 1)
      in
      from (nearest_interval ((b -. a) /. 10.))

let most_ticks = 100.

let cached measure =
  let table = Hashtbl.create 32 in
  fun l ->
    match Hashtbl.find_opt table l with
    | Some e -> e
    | None ->
        let e = Float.max 1. (measure l) in
        Hashtbl.add table l e;
        e

let ticks_of values labelled note =
  {
    major =
      Array.to_list
        (Array.mapi
           (fun k value ->
             let label, context = labelled.(k) in
             { value; label; context })
           values);
    note;
  }

let choose family ~norm ~labels ~length ~spacing ~measure (a, b) =
  let extent = cached measure in
  if not (Float.is_finite a && Float.is_finite b) then
    { major = []; note = None }
  else if (not (a < b)) || norm a = norm b then
    let labelled, note = labels [| a |] in
    ticks_of [| a |] labelled note
  else
    let reference = reference family (a, b) in
    let st0 = { norm; length; rho_t = 1.; labels; extent; best = None } in
    let mean =
      if reference = [||] then 1.
      else
        let labelled, _ = labels reference in
        Array.fold_left (fun m l -> m +. tick_extent st0 l) 0. labelled
        /. Float.of_int (Array.length labelled)
    in
    let m = Float.min (length /. (2. *. mean)) most_ticks in
    let rho_t =
      if spacing > 0. then Float.min (m -. 1.) (length /. spacing) else m -. 1.
    in
    let st = { st0 with rho_t = Float.max 1. rho_t } in
    let one = match family with Log _ -> 1. | _ -> 0. in
    let v vs = if Array.exists (Float.equal one) vs then 1. else 0. in
    (match family with
    | Linear -> decimal_family st ~v (a, b)
    | Log base ->
        powers_family st ~v base (a, b);
        decimal_family st ~v (a, b)
    | Time offset -> calendar_family st offset (a, b));
    match st.best with
    | None -> { major = []; note = None }
    | Some best -> ticks_of best.values best.labelled best.note

let of_values ~norm ~labels vs (a, b) =
  let lo = Float.min a b and hi = Float.max a b in
  let inside = List.filter (fun v -> lo <= v && v <= hi) vs in
  let vs = Array.of_list (List.sort_uniq Float.compare inside) in
  let vs, _ = arrange norm vs in
  let labelled, note = labels vs in
  ticks_of vs labelled note

let stride ~length ~measure names =
  let extent = cached measure in
  let n = Array.length names in
  let k_of r =
    let m = [| 1; 2; 5 |].(r mod 3) in
    m * Float.to_int (pow10 (r / 3))
  in
  let count k = (n + k - 1) / k in
  let overlap k =
    let c = count k in
    let rec loop i =
      i + 1 < c
      && (Float.of_int k /. Float.of_int n *. length
          < (extent names.(i * k) +. extent names.((i + 1) * k)) /. 2.
         || loop (i + 1))
    in
    loop 0
  in
  let rec least r =
    if Float.of_int (count (k_of r)) > most_ticks then least (r + 1) else r
  in
  let rec go r =
    let k = k_of r in
    if k < n && overlap k then go (r + 1) else k
  in
  if n = 0 then 1 else go (least 0)
