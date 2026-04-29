(* bench_glyph.ml *)

module G = Glyph

(* Helpers *)

let repeat chunk count =
  let buffer = Buffer.create (String.length chunk * count) in
  for _ = 1 to count do
    Buffer.add_string buffer chunk
  done;
  Buffer.contents buffer

let cycle_concat parts repeat_count =
  let len = Array.length parts in
  let total_len =
    Array.fold_left (fun acc s -> acc + String.length s) 0 parts
  in
  let avg_len = if len = 0 then 0 else (total_len + len - 1) / len in
  let buffer = Buffer.create (max total_len (avg_len * repeat_count)) in
  for i = 0 to repeat_count - 1 do
    Buffer.add_string buffer parts.(i mod len)
  done;
  Buffer.contents buffer

(* Synthetic but realistic workloads *)

(* ASCII-heavy line representative of CI logs / code / typical TUI text. *)
let ascii_line =
  repeat
    "The quick brown fox jumps over the lazy dog. Pack my box with five dozen \
     liquor jugs. 2024-01-02T12:34:56Z INFO matrix.glyph CI job #1234 \
     succeeded. "
    8

(* A small, reusable hot set of complex graphemes: - emoji ZWJ sequences - flags
   (regional indicators) - emoji with skin tone modifiers - rainbow flag (ZWJ +
   emoji) - Indic conjuncts - CJK - Latin + combining accent *)
let unicode_graphemes =
  [|
    "👩\u{200D}🚀";
    (* emoji ZWJ *)
    "🇫🇷";
    (* flag (RI pair) *)
    "👍🏽";
    (* emoji + skin tone *)
    "🏳️‍🌈";
    (* rainbow flag, ZWJ sequence *)
    "\u{0915}\u{094D}\u{0937}";
    (* Devanagari conjunct "क्ष" *)
    "漢";
    (* CJK *)
    "e\u{0301}";
    (* combining accent *)
    "🚴\u{200D}♀️";
    (* emoji ZWJ with gender selector *)
  |]

(* Emoji / complex text heavy line: status bars, dashboards, fancy prompts. *)
let complex_line = cycle_concat unicode_graphemes 256

(* Pool hot set: small number of frequently reused tokens (separators, keywords,
   icons, etc.). *)
let hotset_tokens =
  Array.concat
    [
      [| " "; ""; "a"; "b"; "c"; "."; ","; "the"; "matrix"; "glyph" |];
      unicode_graphemes;
    ]

(* Pool unique set: monotonically growing identifiers (e.g. log IDs). *)
let unique_tokens =
  let len = Array.length unicode_graphemes in
  Array.init 256 (fun idx ->
      Printf.sprintf "%s:%04x" unicode_graphemes.(idx mod len) idx)

(* Micro-bench wrappers *)

(* Reusable callback for iter_graphemes - avoids closure allocation per call. We
   use a mutable counter that gets reset before each benchmark iteration. *)
let segment_counter = ref 0
let segment_callback ~offset:_ ~len:_ = incr segment_counter

let segment_bench name text =
  Thumper.bench name (fun () ->
      segment_counter := 0;
      G.String.iter_graphemes segment_callback text;
      ignore (Sys.opaque_identity !segment_counter))

let width_bench name method_ text =
  Thumper.bench name (fun () ->
      let w = G.String.measure ~width_method:method_ ~tab_width:2 text in
      ignore (Sys.opaque_identity w))

(* For encode benchmarks, use setup to create pool and reusable callback
   state *)
let encode_bench name method_ text =
  let pool = G.Pool.create () in
  let cols = ref 0 in
  (* Reusable callback - captures pool and cols at definition time *)
  let callback g =
    if G.is_continuation g then ()
    else
      let w = G.cell_width g in
      cols := !cols + w;
      if not (G.is_inline g) then (
        G.Pool.incref pool g;
        G.Pool.decref pool g)
  in
  Thumper.bench name (fun () ->
      cols := 0;
      G.Pool.encode pool ~width_method:method_ ~tab_width:2 callback text;
      ignore (Sys.opaque_identity !cols))

(* Pool-level benchmarks *)

(* Scenario: small hotset of frequently reused graphemes (borders, separators,
   icons, keywords). Pool created once and cleared each iteration to avoid
   measuring pool allocation overhead. *)
let pool_intern_hotset =
  let pool = G.Pool.create () in
  let scratch = Array.make (Array.length hotset_tokens) G.empty in
  Thumper.bench "pool/intern_hotset" (fun () ->
      G.Pool.clear pool;
      for i = 0 to Array.length hotset_tokens - 1 do
        scratch.(i) <- G.Pool.intern pool hotset_tokens.(i);
        G.Pool.incref pool scratch.(i)
      done;
      for i = Array.length hotset_tokens - 1 downto 0 do
        G.Pool.decref pool scratch.(i)
      done)

(* Scenario: lots of one-off strings (e.g. unique IDs in logs). Pool created
   once and cleared each iteration. *)
let pool_intern_unique =
  let pool = G.Pool.create () in
  let scratch = Array.make 256 G.empty in
  Thumper.bench "pool/intern_unique_256" (fun () ->
      G.Pool.clear pool;
      for i = 0 to 255 do
        scratch.(i) <- G.Pool.intern pool unique_tokens.(i);
        G.Pool.incref pool scratch.(i)
      done;
      for i = 255 downto 0 do
        G.Pool.decref pool scratch.(i)
      done)

(* Scenario: repeatedly querying existing glyphs (snapshotting / rendering).
   Pool and ids created once at init; measurement only runs the length loop. *)
let pool_get_existing =
  let pool = G.Pool.create () in
  let ids =
    Array.init (Array.length hotset_tokens) (fun i ->
        let id = G.Pool.intern pool hotset_tokens.(i) in
        G.Pool.incref pool id;
        id)
  in
  Thumper.bench "pool/get_existing" (fun () ->
      let total = ref 0 in
      for i = 0 to Array.length ids - 1 do
        total := !total + G.Pool.length pool ids.(i)
      done;
      ignore (Sys.opaque_identity !total))

(* Benchmark tree *)

let benchmarks =
  Thumper.
    [
      (* Grapheme segmentation: used for cursor motion, selection, wrapping. *)
      group "segment"
        [
          segment_bench "segment/ascii_line" ascii_line;
          segment_bench "segment/complex_line" complex_line;
        ];
      (* Width calculation: layout for different width methods. *)
      group "width"
        [
          (* Pure ASCII fast path. *)
          width_bench "width/ascii/unicode" `Unicode ascii_line;
          (* Complex text with full Unicode width semantics (ZWJ, flags,
             Indic). *)
          width_bench "width/complex/unicode" `Unicode complex_line;
          (* Same text but with No_zwj semantics (split ZWJ sequences). *)
          width_bench "width/complex/no_zwj" `No_zwj complex_line;
          (* POSIX wcwidth-compatible path (per-codepoint). *)
          width_bench "width/complex/wcwidth" `Wcwidth complex_line;
        ];
      (* Encoding into glyphs as used by Grid.draw_text / terminal frontends. *)
      group "encode"
        [
          encode_bench "encode/ascii_line" `Unicode ascii_line;
          encode_bench "encode/complex_line" `Unicode complex_line;
        ];
      (* Pool behaviour under realistic access patterns. *)
      group "pool" [ pool_intern_hotset; pool_intern_unique; pool_get_existing ];
    ]

let () = Thumper.run "glyph" benchmarks
