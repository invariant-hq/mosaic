(** Code editor demo with inline completion, editable line numbers, and syntax
    highlighting while typing. *)

open Mosaic

type completion = {
  prefix : string;
  cursor_byte : int;
  items : string list;
  selected : int;
}

type model = {
  code : string;
  cursor : int;
  selection : (int * int) option;
  cursor_override : int option;
  popup_open : bool;
  completion : completion option;
  status : string;
}

type msg =
  | Set_code of string
  | Cursor_changed of int * (int * int) option
  | Trigger_completion
  | Next_completion
  | Prev_completion
  | Accept_completion
  | Dismiss_completion
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

let clamp lo hi x = if x < lo then lo else if x > hi then hi else x

let lowercase_codepoint i =
  if i >= Char.code 'A' && i <= Char.code 'Z' then i + 32 else i

let starts_with ~prefix s =
  let lp = String.length prefix and ls = String.length s in
  lp <= ls && String.sub s 0 lp = prefix

let is_ident_start = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

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

let ocaml_keywords =
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

let completion_pool code =
  unique_sorted (ocaml_keywords @ collect_identifiers code)

let grapheme_byte_offsets s =
  let n = Glyph.String.grapheme_count s in
  let offsets = Array.make (n + 1) (String.length s) in
  let i = ref 0 in
  Glyph.String.iter_graphemes
    (fun ~offset ~len:_ ->
      offsets.(!i) <- offset;
      incr i)
    s;
  offsets

let cursor_byte_of code cursor =
  let offsets = grapheme_byte_offsets code in
  let max_cursor = Array.length offsets - 1 in
  let cursor = clamp 0 max_cursor cursor in
  (cursor, offsets.(cursor))

let find_prefix_at_cursor code ~cursor ~selection =
  match selection with
  | Some _ -> None
  | None ->
      let cursor, cursor_byte = cursor_byte_of code cursor in
      let len = String.length code in
      let at_ident_end =
        cursor_byte = len || not (is_ident_char code.[cursor_byte])
      in
      if not at_ident_end then None
      else
        let i = ref (cursor_byte - 1) in
        while !i >= 0 && is_ident_char code.[!i] do
          decr i
        done;
        let start = !i + 1 in
        let prefix =
          if cursor_byte > start then String.sub code start (cursor_byte - start)
          else ""
        in
        Some (cursor, cursor_byte, prefix)

let selected_completion_item c =
  match c.items with
  | [] -> None
  | items ->
      let len = List.length items in
      Some (List.nth items (c.selected mod len))

let index_of item items =
  let rec loop i = function
    | [] -> None
    | x :: _ when String.equal x item -> Some i
    | _ :: tl -> loop (i + 1) tl
  in
  loop 0 items

let cycle_completion c delta =
  let len = List.length c.items in
  if len = 0 then c
  else { c with selected = (c.selected + delta + len) mod len }

let build_completion ?(force = false) code ~cursor ~selection =
  match find_prefix_at_cursor code ~cursor ~selection with
  | None -> None
  | Some (_, cursor_byte, prefix) -> (
      if (not force) && String.length prefix = 0 then None
      else
        let items =
          completion_pool code
          |> List.filter (fun item ->
              (String.length prefix = 0 || starts_with ~prefix item)
              && not (String.equal item prefix))
        in
        match items with
        | [] -> None
        | _ -> Some { prefix; cursor_byte; items; selected = 0 })

let preserve_selection prev next =
  match (prev, next) with
  | Some prev, Some next -> (
      match selected_completion_item prev with
      | Some item -> (
          match index_of item next.items with
          | Some idx -> Some { next with selected = idx }
          | None -> Some next)
      | None -> Some next)
  | _, x -> x

let recompute_completion model =
  let force = model.popup_open in
  let next =
    build_completion ~force model.code ~cursor:model.cursor
      ~selection:model.selection
  in
  { model with completion = preserve_selection model.completion next }

let completion_status completion =
  match completion with
  | None -> "No suggestions"
  | Some c ->
      Printf.sprintf "Suggestions: %d (prefix: %s)" (List.length c.items)
        (if String.length c.prefix = 0 then "<any>" else c.prefix)

let ghost_text model =
  match model.completion with
  | None -> None
  | Some c when String.length c.prefix = 0 -> None
  | Some c -> (
      match selected_completion_item c with
      | None -> None
      | Some item when starts_with ~prefix:c.prefix item ->
          let suffix =
            String.sub item (String.length c.prefix)
              (String.length item - String.length c.prefix)
          in
          if String.length suffix = 0 then None else Some suffix
      | Some _ -> None)

let insert_at_byte s ~byte text =
  let len = String.length s in
  let byte = clamp 0 len byte in
  String.sub s 0 byte ^ text ^ String.sub s byte (len - byte)

let apply_completion model c choice =
  let suffix =
    if starts_with ~prefix:c.prefix choice then
      String.sub choice (String.length c.prefix)
        (String.length choice - String.length c.prefix)
    else choice
  in
  if String.length suffix = 0 then
    { model with popup_open = false; status = "No completion change." }
    |> recompute_completion
  else
    let code = insert_at_byte model.code ~byte:c.cursor_byte suffix in
    let cursor = model.cursor + Glyph.String.grapheme_count suffix in
    {
      code;
      cursor;
      selection = None;
      cursor_override = Some cursor;
      popup_open = false;
      completion = None;
      status = Printf.sprintf "Completed: %s" choice;
    }
    |> recompute_completion

let rec take n xs =
  if n <= 0 then []
  else match xs with [] -> [] | x :: tl -> x :: take (n - 1) tl

let cursor_line code cursor =
  let _, cursor_byte = cursor_byte_of code cursor in
  let line = ref 0 in
  for i = 0 to cursor_byte - 1 do
    if code.[i] = '\n' then incr line
  done;
  !line

let editor_on_key model ev =
  let data = Event.Key.data ev in
  if data.event_type = Release then None
  else
    let m = data.modifier in
    match data.key with
    | Escape when model.popup_open ->
        Event.Key.prevent_default ev;
        Some Dismiss_completion
    | Enter when model.popup_open && not (m.ctrl || m.alt || m.super || m.shift)
      ->
        Event.Key.prevent_default ev;
        Some Accept_completion
    | Tab when model.popup_open && m.shift ->
        Event.Key.prevent_default ev;
        Some Prev_completion
    | Tab when model.popup_open ->
        Event.Key.prevent_default ev;
        Some Accept_completion
    | Tab when Option.is_none model.selection ->
        Event.Key.prevent_default ev;
        Some Trigger_completion
    | Char c
      when model.popup_open && m.ctrl
           && lowercase_codepoint (Uchar.to_int c) = Char.code 'n' ->
        Event.Key.prevent_default ev;
        Some Next_completion
    | Char c
      when model.popup_open && m.ctrl
           && lowercase_codepoint (Uchar.to_int c) = Char.code 'p' ->
        Event.Key.prevent_default ev;
        Some Prev_completion
    | Char c when m.ctrl && Uchar.to_int c = Char.code ' ' ->
        Event.Key.prevent_default ev;
        Some Trigger_completion
    | _ -> None

let init () =
  let code = ocaml_sample in
  let cursor = Glyph.String.grapheme_count code in
  ( {
      code;
      cursor;
      selection = None;
      cursor_override = None;
      popup_open = false;
      completion = None;
      status =
        "Type code. Tab accepts inline completion. Ctrl+Space opens list.";
    }
    |> recompute_completion,
    Cmd.none )

let update msg model =
  match msg with
  | Set_code code ->
      let model = { model with code; cursor_override = None } in
      let model =
        if Option.is_some model.selection then { model with popup_open = false }
        else model
      in
      (recompute_completion model, Cmd.none)
  | Cursor_changed (cursor, selection) ->
      let model =
        {
          model with
          cursor;
          selection;
          cursor_override = None;
          popup_open =
            (match selection with Some _ -> false | None -> model.popup_open);
        }
      in
      (recompute_completion model, Cmd.none)
  | Trigger_completion ->
      if Option.is_some model.selection then
        ( { model with status = "Dismiss selection before completion." },
          Cmd.none )
      else
        let model = recompute_completion { model with popup_open = true } in
        ({ model with status = completion_status model.completion }, Cmd.none)
  | Next_completion -> (
      match model.completion with
      | None -> (model, Cmd.none)
      | Some c ->
          ( {
              model with
              completion = Some (cycle_completion c 1);
              popup_open = true;
            },
            Cmd.none ))
  | Prev_completion -> (
      match model.completion with
      | None -> (model, Cmd.none)
      | Some c ->
          ( {
              model with
              completion = Some (cycle_completion c (-1));
              popup_open = true;
            },
            Cmd.none ))
  | Accept_completion -> (
      match model.completion with
      | None -> (model, Cmd.none)
      | Some c -> (
          match selected_completion_item c with
          | None ->
              ( { model with popup_open = false } |> recompute_completion,
                Cmd.none )
          | Some choice -> (apply_completion model c choice, Cmd.none)))
  | Dismiss_completion ->
      ( { model with popup_open = false; status = "Completion dismissed." }
        |> recompute_completion,
        Cmd.none )
  | Quit -> (model, Cmd.quit)

(* Palette *)
let header_bg = Ansi.Color.of_rgb 28 70 88
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:17) ()

let active_line_colors code cursor =
  let line = cursor_line code cursor in
  [
    ( line,
      {
        Line_number.gutter = Ansi.Color.of_rgb 48 48 68;
        content = Some (Ansi.Color.of_rgb 32 32 48);
      } );
  ]

let completion_panel model =
  if not model.popup_open then empty
  else
    match model.completion with
    | None ->
        box ~border:true ~border_color ~padding:(padding 1)
          [ text ~style:hint "No suggestions at cursor." ]
    | Some c ->
        box ~border:true ~border_color ~padding:(padding 1)
          ~flex_direction:Column ~gap:(gap 0)
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

let highlight_ocaml code =
  let ranges = Tree_sitter_ocaml.highlight_ocaml code in
  Syntax_theme.apply Syntax_theme.default ~content:code ranges

let view model =
  let spans = highlight_ocaml model.code in
  let ghost_text = ghost_text model in
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
                "▸ x-code-editor (OCaml)";
              text ~style:muted
                "inline completion + editable highlighting + line numbers";
            ];
        ];
      box ~flex_grow:1. ~padding:(padding 1) ~flex_direction:Column ~gap:(gap 1)
        [
          text ~style:hint "Editor";
          box ~flex_grow:1. ~border:true ~border_color
            [
              line_number
                ~line_colors:(active_line_colors model.code model.cursor)
                ~flex_grow:1.
                (textarea ~autofocus:true ~value:model.code
                   ?cursor:model.cursor_override ~spans ?ghost_text
                   ~ghost_text_color:(Ansi.Color.grayscale ~level:10)
                   ~wrap:`None ~cursor_style:`Line
                   ~size:{ width = pct 100; height = pct 100 }
                   ~on_key:(fun ev -> editor_on_key model ev)
                   ~on_input:(fun v -> Some (Set_code v))
                   ~on_cursor:(fun ~cursor ~selection ->
                     Some (Cursor_changed (cursor, selection)))
                   ());
            ];
          completion_panel model;
        ];
      box ~padding:(padding 1) ~background:footer_bg
        [
          text ~style:hint
            "Tab trigger/accept  •  Shift+Tab previous  •  Enter accept  •  \
             Esc dismiss";
          text ~style:hint "Ctrl+Space open list  •  Ctrl+N/P cycle";
          text ~style:hint "q or Esc quit";
          text ~style:muted model.status;
        ];
    ]

let subscriptions model =
  Sub.on_key (fun ev ->
      match (Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape when not model.popup_open -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
