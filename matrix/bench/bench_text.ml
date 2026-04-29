(* bench_text.ml *)

module T = Text

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

let ascii_line =
  repeat
    "The quick brown fox jumps over the lazy dog. Pack my box with five dozen \
     liquor jugs. 2024-01-02T12:34:56Z INFO matrix.text CI job #1234 \
     succeeded. "
    8

let unicode_graphemes =
  [|
    "👩\u{200D}🚀";
    "🇫🇷";
    "👍🏽";
    "🏳️‍🌈";
    "\u{0915}\u{094D}\u{0937}";
    "漢";
    "e\u{0301}";
    "🚴\u{200D}♀️";
  |]

let complex_line = cycle_concat unicode_graphemes 256
let mixed_line = repeat ("Hello 世界 👩\u{200D}🚀 cafe\u{0301} 🇫🇷 ") 128
let segment_counter = ref 0
let segment_callback ~offset:_ ~len:_ = incr segment_counter

let segment_bench name text =
  Thumper.bench name (fun () ->
      segment_counter := 0;
      T.iter_graphemes segment_callback text;
      ignore (Sys.opaque_identity !segment_counter))

let width_bench name method_ text =
  Thumper.bench name (fun () ->
      let w = T.measure ~width_method:method_ ~tab_width:2 text in
      ignore (Sys.opaque_identity w))

let position_bench name method_ text columns =
  Thumper.bench name (fun () ->
      let p = T.find_pos ~width_method:method_ ~tab_width:2 text ~columns in
      ignore (Sys.opaque_identity p))

let wrap_pos_bench name method_ text columns =
  Thumper.bench name (fun () ->
      let p =
        T.find_wrap_pos ~width_method:method_ ~tab_width:2 text
          ~max_columns:columns
      in
      ignore (Sys.opaque_identity p))

let width_at_bench name method_ text offset =
  Thumper.bench name (fun () ->
      let w = T.width_at ~width_method:method_ ~tab_width:2 text ~byte_offset:offset in
      ignore (Sys.opaque_identity w))

let prev_bench name method_ text offset =
  Thumper.bench name (fun () ->
      let g =
        T.prev_grapheme ~width_method:method_ ~tab_width:2 text
          ~byte_offset:offset
      in
      ignore (Sys.opaque_identity g))

let benchmarks =
  Thumper.
    [
      group "segment"
        [
          segment_bench "segment/ascii_line" ascii_line;
          segment_bench "segment/complex_line" complex_line;
        ];
      group "width"
        [
          width_bench "width/ascii/unicode" `Unicode ascii_line;
          width_bench "width/complex/unicode" `Unicode complex_line;
          width_bench "width/complex/no_zwj" `No_zwj complex_line;
          width_bench "width/complex/wcwidth" `Wcwidth complex_line;
        ];
      group "position"
        [
          position_bench "position/find_pos/ascii" `Unicode ascii_line 320;
          position_bench "position/find_pos/mixed" `Unicode mixed_line 320;
          position_bench "position/find_pos/wcwidth" `Wcwidth mixed_line 320;
          wrap_pos_bench "position/find_wrap_pos/ascii" `Unicode ascii_line 320;
          wrap_pos_bench "position/find_wrap_pos/mixed" `Unicode mixed_line 320;
          wrap_pos_bench "position/find_wrap_pos/wcwidth" `Wcwidth mixed_line 320;
          width_at_bench "position/width_at/ascii" `Unicode ascii_line 128;
          width_at_bench "position/width_at/mixed" `Unicode mixed_line 7;
          width_at_bench "position/width_at/wcwidth" `Wcwidth mixed_line 7;
          prev_bench "position/prev/ascii" `Unicode ascii_line
            (String.length ascii_line);
          prev_bench "position/prev/mixed" `Unicode mixed_line
            (String.length mixed_line);
          prev_bench "position/prev/wcwidth" `Wcwidth mixed_line
            (String.length mixed_line);
        ];
    ]

let () = Thumper.run "text" benchmarks
