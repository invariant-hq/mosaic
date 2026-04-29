[@@@ocamlformat "disable"]
(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Vendored from uuseg v17.0.0 with the following modifications:
   - Added [reset] function for segmenter reuse (zero allocation)
   - Added [check_boundary] for zero-allocation direct boundary checks
   - Changed [break] and [update_left] to take pre-computed [is_extpic] flag
*)

(* These are the rules as found in [1], with property values aliases [2]
   substituted.

   GB1.               sot ÷ Any
   GB2.               Any ÷ eot
   GB3.                CR × LF
   GB4.        (CN|CR|LF) ÷
   GB5.                   ÷ (CN|CR|LF)
   GB6.                 L × (L|V|LV|LVT)
   GB7.            (LV|V) × (V|T)
   GB8.           (LVT|T) × T
   GB9.                   × (EX|ZWJ)
   GB9a.                  × SM
   GB9b.               PP ×
   GB9c. \p{InCB=Consonant} [\p{InCB=Extend}\p{InCB=Linker}]*
         \p{InCB=Linker} [\p{InCB=Extend}\p{InCB=Linker}]*
         ×
         \p{InCB=Consonant}
   GB11. \p{Extended_Pictographic} EX* ZWJ x \p{Extended_Pictographic}
   GB12.  sot (RI RI)* RI × RI
   GB13.   [^RI] (RI RI)* × RI
   GB999.             Any ÷ Any

   [1]: http://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries
   [2]: http://www.unicode.org/Public/7.0.0/ucd/PropertyValueAliases.txt
   [3]: http://www.unicode.org/Public/7.0.0/ucd/auxiliary/GraphemeBreakTest.html

   By the structure of the rules we see that grapheme clusters
   boundaries can *mostly* be determined by simply looking at the
   grapheme cluster break property value of the character on the left
   and on the right of a boundary. The exceptions are GB9c, GB10 and GB12-13
   which are handled specially by enriching the segmenter state in
   a horribly ad-hoc fashion. *)

type gcb =
  | CN | CR | EX | EB | EBG | EM | GAZ | L | LF | LV | LVT | PP | RI
  | SM | T | V | XX | ZWJ | Sot

type incb = Consonant | Extend | Linker | None'

(* WARNING. The indexes used here need to be synchronized with those
   in Unicode_data (generated from uucp).
*)

let byte_to_gcb =
  [| CN; CR; EX; EB; EBG; EM; GAZ; L; LF; LV; LVT; PP; RI;
     SM; T; V; XX; ZWJ; |]

let gcb u = byte_to_gcb.(Unicode.grapheme_cluster_break u)

let byte_to_incb = [| Consonant; Extend; Linker; None' |]
let incb u = byte_to_incb.(Unicode.indic_conjunct_break u)

type left_gb9c_state = (* Ad-hoc state for matching GB9c *)
| Reset | Has_consonant | Has_linker

type state =
| Fill  (* get next uchar to decide boundary. *)
| Flush (* an uchar is buffered, client needs to get it out with `Await. *)
| End   (* `End was added. *)

type t =
  { mutable state : state;                                 (* current state. *)
    mutable left_gb9c : left_gb9c_state;         (* state for matching gb9c. *)
    mutable left : gcb;            (* break property value left of boundary. *)
    mutable left_odd_ri : bool;             (* odd number of RI on the left. *)
    mutable left_emoji_seq : bool;                 (* emoji seq on the left. *)
    mutable buf : [ `Uchar of Uchar.t ] }                 (* bufferized add. *)

let nul_buf = `Uchar (Uchar.unsafe_of_int 0x0000)

let create () =
  { state = Fill;
    left_gb9c = Reset;
    left = Sot; left_odd_ri = false; left_emoji_seq = false;
    buf = nul_buf (* overwritten *) }

let copy s = { s with state = s.state; }
let equal = ( = )

let reset s =
  s.state <- Fill;
  s.left_gb9c <- Reset;
  s.left <- Sot;
  s.left_odd_ri <- false;
  s.left_emoji_seq <- false

let gb9c_match s right_incb = match s.left_gb9c, right_incb with
| Has_linker, Consonant -> true
| _, _ -> false

(* Core break check - takes pre-computed is_extpic for efficiency *)
let break s right right_incb ~is_extpic = match s.left, right with
| (* GB1 *)   Sot, _ -> true
  (* GB2 is handled by `End *)
| (* GB3 *)   CR, LF -> false
| (* GB4 *)   (CN|CR|LF), _ -> true
| (* GB5 *)   _, (CN|CR|LF) -> true
| (* GB6 *)   L, (L|V|LV|LVT) -> false
| (* GB7 *)   (LV|V), (V|T) -> false
| (* GB8 *)   (LVT|T), T -> false
| (* GB9+a *) _, (EX|ZWJ|SM) -> false
| (* GB9b *)  PP, _ -> false
| (* GB9c *)  _, _ when gb9c_match s right_incb -> false
| (* GB11 *)  ZWJ, _ when s.left_emoji_seq && is_extpic -> false
| (* GB12+13 *) RI, RI when s.left_odd_ri -> false
| (* GB999 *) _, _ -> true

(* Core state update - takes pre-computed is_extpic for efficiency *)
let update_left s right right_incb ~is_extpic =
  s.left <- right;
  begin match s.left with
  | EX | ZWJ ->
      s.left_odd_ri <- false
      (* keep s.left_emoji_seq as is *)
  | RI ->
      s.left_odd_ri <- not s.left_odd_ri;
      s.left_emoji_seq <- false;
  | _ when is_extpic ->
      s.left_odd_ri <- false;
      s.left_emoji_seq <- true;
  | _ ->
      s.left_odd_ri <- false;
      s.left_emoji_seq <- false
  end;
  s.left_gb9c <- begin match right_incb with
  | None' -> Reset
  | Consonant -> Has_consonant
  | Linker when s.left_gb9c = Has_consonant -> Has_linker
  | Extend | Linker -> s.left_gb9c
  end

(* Direct boundary check - zero allocation.
   Returns true if there is a boundary BEFORE this character.
   The first character always has a boundary before it (GB1).
   Uses combined property lookup for efficiency. *)
let[@inline] check_boundary s u =
  (* Single lookup: bits 0-4 = gcb, bits 5-6 = incb, bit 7 = extpic *)
  let props = Unicode.grapheme_props u in
  let right = byte_to_gcb.(props land 0x1F) in
  let right_incb = byte_to_incb.((props lsr 5) land 0x03) in
  let is_extpic = props land 0x80 <> 0 in
  let is_break = break s right right_incb ~is_extpic in
  update_left s right right_incb ~is_extpic;
  is_break

(* Combined boundary check + width extraction in one property lookup.
   Returns packed int: bit 2 = is_boundary, bits 0-1 = width_enc.
   Width encoding: 0 → -1, 1 → 0, 2 → 1, 3 → 2. *)
let[@inline] check_boundary_with_width s u =
  let packed = Unicode.packed_props u in
  let right = byte_to_gcb.(packed land 0x1F) in
  let right_incb = byte_to_incb.((packed lsr 5) land 0x03) in
  let is_extpic = packed land 0x80 <> 0 in
  let is_break = break s right right_incb ~is_extpic in
  update_left s right right_incb ~is_extpic;
  let width_enc = (packed lsr 8) land 0x03 in
  if is_break then width_enc lor 4 else width_enc

let add s = function
| `Uchar u as add ->
    begin match s.state with
    | Fill ->
        let right = gcb u in
        let right_incb = incb u in
        let is_extpic = Unicode.is_extended_pictographic u in
        let is_break = break s right right_incb ~is_extpic in
        update_left s right right_incb ~is_extpic;
        if not is_break then add else
        (s.state <- Flush; s.buf <- add; `Boundary)
    | Flush -> Uuseg_base.err_exp_await add
    | End -> Uuseg_base.err_ended add
    end
| `Await ->
    begin match s.state with
    | Flush -> s.state <- Fill; (s.buf :> Uuseg_base.ret)
    | End -> `End
    | Fill -> `Await
    end
| `End ->
    begin match s.state with
    | Fill -> s.state <- End; if s.left = Sot then `End else `Boundary
    | Flush -> Uuseg_base.err_exp_await `End
    | End -> Uuseg_base.err_ended `End
    end
