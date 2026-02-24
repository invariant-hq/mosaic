(** Code editor demo with autocomplete, editable line numbers, and highlighted
    preview. *)

open Mosaic

type lang = OCaml | JSON
type completion = { prefix : string; items : string list; selected : int }

type model = {
  lang : lang;
  code : string;
  completion : completion option;
  status : string;
}

type msg =
  | Set_code of string
  | Trigger_completion
  | Next_completion
  | Prev_completion
  | Accept_completion
  | Dismiss_completion
  | Toggle_lang
  | Quit

let ocaml_sample =
  {|let greet name =
  Printf.printf "Hello, %s!\n" name

let rec sum = function
  | [] -> 0
  | x :: xs -> x + sum xs

let () =
  let values = [ 1; 2; 3; 4 ] in
  let total = sum values in
  greet "Mosaic";
  Printf.printf "total=%d\n" total|}

let json_sample =
  {|{
  "name": "mosaic",
  "version": "0.1.0",
  "features": ["editor", "line-numbers", "autocomplete"],
  "active": true,
  "retries": 3
}|}

let lang_label = function OCaml -> "OCaml" | JSON -> "JSON"
let sample_for_lang = function OCaml -> ocaml_sample | JSON -> json_sample

let keywords_for_lang = function
  | OCaml ->
      [
        "and";
        "as";
        "begin";
        "class";
        "done";
        "else";
        "end";
        "exception";
        "external";
        "false";
        "for";
        "fun";
        "function";
        "if";
        "in";
        "include";
        "let";
        "match";
        "module";
        "mutable";
        "of";
        "open";
        "rec";
        "sig";
        "struct";
        "then";
        "true";
        "try";
        "type";
        "val";
        "when";
        "with";
      ]
  | JSON ->
      [
        "false";
        "null";
        "true";
        "name";
        "version";
        "description";
        "features";
        "active";
        "retries";
        "dependencies";
      ]

let starts_with ~prefix s =
  let lp = String.length prefix and ls = String.length s in
  lp <= ls && String.sub s 0 lp = prefix

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let trailing_identifier s =
  let len = String.length s in
  if len = 0 then None
  else
    let last = len - 1 in
    if not (is_ident_char s.[last]) then None
    else
      let i = ref last in
      while !i >= 0 && is_ident_char s.[!i] do
        decr i
      done;
      let start = !i + 1 in
      Some (start, String.sub s start (len - start))

let replace_trailing_identifier s ~replacement =
  match trailing_identifier s with
  | Some (start, _) -> String.sub s 0 start ^ replacement
  | None -> s ^ replacement

let unique_sorted strings =
  let sorted = List.sort String.compare strings in
  let rec dedup acc = function
    | a :: (b :: _ as tl) when String.equal a b -> dedup acc tl
    | x :: tl -> dedup (x :: acc) tl
    | [] -> List.rev acc
  in
  dedup [] sorted

let collect_identifiers s =
  let tbl = Hashtbl.create 64 in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    if is_ident_start s.[!i] then begin
      let j = ref (!i + 1) in
      while !j < n && is_ident_char s.[!j] do
        incr j
      done;
      let token = String.sub s !i (!j - !i) in
      if String.length token >= 2 then Hashtbl.replace tbl token ();
      i := !j
    end
    else incr i
  done;
  Hashtbl.fold (fun key () acc -> key :: acc) tbl []

let completion_pool lang code =
  unique_sorted (keywords_for_lang lang @ collect_identifiers code)

let build_completion ?(force = false) lang code =
  let prefix =
    match trailing_identifier code with Some (_, p) -> p | None -> ""
  in
  if (not force) && String.length prefix = 0 then None
  else
    let items =
      completion_pool lang code
      |> List.filter (fun item ->
          (String.length prefix = 0 || starts_with ~prefix item)
          && not (String.equal item prefix))
    in
    match items with [] -> None | _ -> Some { prefix; items; selected = 0 }

let rec take n xs =
  if n <= 0 then []
  else match xs with [] -> [] | x :: tl -> x :: take (n - 1) tl

let line_count s =
  let n = ref 1 in
  String.iter (fun c -> if c = '\n' then incr n) s;
  !n

let selected_completion_item c =
  match c.items with
  | [] -> None
  | items ->
      let len = List.length items in
      Some (List.nth items (c.selected mod len))

let cycle_completion c delta =
  let len = List.length c.items in
  if len = 0 then c
  else
    let idx = (c.selected + delta + len) mod len in
    { c with selected = idx }

let completion_status = function
  | None -> "No suggestions"
  | Some c ->
      Printf.sprintf "Suggestions: %d (prefix: %s)" (List.length c.items)
        (if String.length c.prefix = 0 then "<any>" else c.prefix)

let editor_on_key model ev =
  let data = Event.Key.data ev in
  if data.event_type = Release then None
  else
    let m = data.modifier in
    match data.key with
    | Escape when Option.is_some model.completion ->
        Event.Key.prevent_default ev;
        Some Dismiss_completion
    | Up when Option.is_some model.completion ->
        Event.Key.prevent_default ev;
        Some Prev_completion
    | Down when Option.is_some model.completion ->
        Event.Key.prevent_default ev;
        Some Next_completion
    | Enter
      when Option.is_some model.completion && not (m.ctrl || m.alt || m.super)
      ->
        Event.Key.prevent_default ev;
        Some Accept_completion
    | Tab when Option.is_some model.completion ->
        Event.Key.prevent_default ev;
        Some (if m.shift then Prev_completion else Next_completion)
    | Tab ->
        Event.Key.prevent_default ev;
        Some Trigger_completion
    | Char c when m.ctrl && Uchar.to_int c = Char.code ' ' ->
        Event.Key.prevent_default ev;
        Some Trigger_completion
    | _ -> None

let init () =
  ( {
      lang = OCaml;
      code = ocaml_sample;
      completion = None;
      status = "Type code. Press Tab for completion.";
    },
    Cmd.none )

let update msg model =
  match msg with
  | Set_code code ->
      let completion =
        match model.completion with
        | Some _ -> build_completion ~force:true model.lang code
        | None -> build_completion model.lang code
      in
      ({ model with code; completion }, Cmd.none)
  | Trigger_completion ->
      let completion = build_completion ~force:true model.lang model.code in
      ( { model with completion; status = completion_status completion },
        Cmd.none )
  | Next_completion -> (
      match model.completion with
      | None -> (model, Cmd.none)
      | Some c ->
          ({ model with completion = Some (cycle_completion c 1) }, Cmd.none))
  | Prev_completion -> (
      match model.completion with
      | None -> (model, Cmd.none)
      | Some c ->
          ({ model with completion = Some (cycle_completion c (-1)) }, Cmd.none)
      )
  | Accept_completion -> (
      match model.completion with
      | None -> (model, Cmd.none)
      | Some c -> (
          match selected_completion_item c with
          | None -> ({ model with completion = None }, Cmd.none)
          | Some choice ->
              let code =
                if String.length c.prefix = 0 then model.code ^ choice
                else replace_trailing_identifier model.code ~replacement:choice
              in
              let completion = build_completion model.lang code in
              ( {
                  model with
                  code;
                  completion;
                  status = Printf.sprintf "Completed: %s" choice;
                },
                Cmd.none )))
  | Dismiss_completion ->
      ( { model with completion = None; status = "Completion dismissed." },
        Cmd.none )
  | Toggle_lang ->
      let lang = match model.lang with OCaml -> JSON | JSON -> OCaml in
      ( {
          lang;
          code = sample_for_lang lang;
          completion = None;
          status = Printf.sprintf "Language switched to %s." (lang_label lang);
        },
        Cmd.none )
  | Quit -> (model, Cmd.quit)

(* Palette *)
let header_bg = Ansi.Color.of_rgb 28 70 88
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:17) ()

let active_line_colors code =
  let line = max 0 (line_count code - 1) in
  [
    ( line,
      {
        Line_number.gutter = Ansi.Color.of_rgb 48 48 68;
        content = Some (Ansi.Color.of_rgb 32 32 48);
      } );
  ]

let completion_panel model =
  match model.completion with
  | None -> text ~style:hint "No active completion"
  | Some c ->
      box ~border:true ~border_color ~padding:(padding 1) ~flex_direction:Column
        ~gap:(gap 0)
        [
          text
            ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.cyan ())
            (Printf.sprintf "Completions (%d)" (List.length c.items));
          box ~flex_direction:Column ~gap:(gap 0)
            (take 8 c.items
            |> List.mapi (fun i item ->
                let selected = i = c.selected in
                let prefix = if selected then "> " else "  " in
                text
                  ~style:
                    (if selected then
                       Ansi.Style.make ~fg:Ansi.Color.black
                         ~bg:Ansi.Color.yellow ~bold:true ()
                     else Ansi.Style.make ~fg:Ansi.Color.white ())
                  (prefix ^ item)));
        ]

let highlight_for_lang lang code =
  let ranges =
    match lang with
    | OCaml -> Tree_sitter_ocaml.highlight_ocaml code
    | JSON -> Tree_sitter_json.highlight code
  in
  Syntax_theme.apply Syntax_theme.default ~content:code ranges

let view model =
  let highlights = highlight_for_lang model.lang model.code in
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~padding:(padding 1) ~background:header_bg
        [
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            ~size:{ width = pct 100; height = auto }
            [
              text
                ~style:(Ansi.Style.make ~bold:true ())
                (Printf.sprintf "▸ x-code-editor (%s)" (lang_label model.lang));
              text ~style:muted
                "editable line numbers + autocomplete + highlighting";
            ];
        ];
      box ~flex_grow:1. ~padding:(padding 1) ~flex_direction:Column ~gap:(gap 1)
        [
          text ~style:hint "Editor";
          box ~flex_grow:1. ~border:true ~border_color
            [
              line_number
                ~line_colors:(active_line_colors model.code)
                ~flex_grow:1.
                (textarea ~autofocus:true ~value:model.code ~highlights
                   ~wrap:`None ~cursor_style:`Line
                   ~size:{ width = pct 100; height = pct 100 }
                   ~on_key:(fun ev -> editor_on_key model ev)
                   ~on_input:(fun v -> Some (Set_code v))
                   ());
            ];
          completion_panel model;
        ];
      box ~padding:(padding 1) ~background:footer_bg
        [
          text ~style:hint
            "Tab trigger/cycle  •  Shift+Tab previous  •  Enter accept  •  Esc \
             dismiss popup";
          text ~style:hint "F2 toggle language  •  q or Esc quit";
          text ~style:muted model.status;
        ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Event.Key.data ev).key with
      | F 2 -> Some Toggle_lang
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
