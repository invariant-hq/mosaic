type span = { text : string; style : Style.t }
type state = { style : Style.t; link_open : bool }

let initial_state = { style = Style.default; link_open = false }

let emit ?depth ?(state = initial_state) ?(hyperlinks_enabled = true)
    (w : Writer.t) (spans : span list) : state =
  List.fold_left
    (fun { style = prev_style; link_open } { text; style } ->
      let prev_link = Style.link prev_style in
      let next_link = Style.link style in
      (* Handle hyperlink transitions *)
      let link_open =
        if prev_link <> next_link then (
          (* Close existing hyperlink if open *)
          if link_open then Escape.hyperlink_end w;
          (* Open new hyperlink if present and enabled *)
          match next_link with
          | Some url when hyperlinks_enabled ->
              Escape.hyperlink_start ~url w;
              true
          | _ -> false)
        else link_open
      in
      (* Emit style transition and text *)
      Style.emit ?depth ~prev:prev_style style w;
      Escape.literal text w;
      { style; link_open })
    state spans

let render ?depth ?(state = initial_state) ?(hyperlinks_enabled = true)
    (spans : span list) : string =
  Escape.to_string (fun w ->
      let final_state = emit ?depth ~state ~hyperlinks_enabled w spans in
      if final_state.link_open then Escape.hyperlink_end w;
      Escape.reset w)
