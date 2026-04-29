include Escape
module Writer = Writer
module Color = Color
module Attr = Attr
module Style = Style
module Parser = Parser
module Sgr_state = Sgr_state

let render ?hyperlinks_enabled segments =
  Segment.render ?hyperlinks_enabled segments

let parse = Parser.parse

let strip str =
  match String.index_from_opt str 0 '\x1b' with
  | None -> str
  | Some _ ->
      let len = String.length str in
      let buf = Buffer.create (String.length str) in
      let[@inline] add_text start stop =
        if stop > start then Buffer.add_substring buf str start (stop - start)
      in
      let[@inline] is_csi_final_byte c =
        let code = Char.code c in
        code >= 0x40 && code <= 0x7e
      in
      let rec skip_csi i =
        if i >= len then len
        else
          let c = String.unsafe_get str i in
          if is_csi_final_byte c then i + 1 else skip_csi (i + 1)
      in
      let rec skip_string_control i =
        if i >= len then len
        else if
          String.unsafe_get str i = '\x1b'
          && i + 1 < len
          && String.unsafe_get str (i + 1) = '\\'
        then i + 2
        else skip_string_control (i + 1)
      in
      let rec skip_osc i =
        if i >= len then len
        else
          match String.unsafe_get str i with
          | '\x07' -> i + 1
          | '\x1b' when i + 1 < len && String.unsafe_get str (i + 1) = '\\' ->
              i + 2
          | _ -> skip_osc (i + 1)
      in
      let rec loop text_start i =
        if i >= len then add_text text_start len
        else if String.unsafe_get str i <> '\x1b' then loop text_start (i + 1)
        else begin
          add_text text_start i;
          let next = i + 1 in
          if next >= len then ()
          else
            let stop =
              match String.unsafe_get str next with
              | '[' -> skip_csi (i + 2)
              | ']' -> skip_osc (i + 2)
              | 'P' | 'X' | '^' | '_' -> skip_string_control (i + 2)
              | '%' | '(' | ')' | '*' | '+' ->
                  if i + 2 < len then i + 3 else len
              | _ -> min (i + 2) len
            in
            loop stop stop
        end
      in
      loop 0 0;
      Buffer.contents buf
