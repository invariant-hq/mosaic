(** Agent-style TUI demo.

    This example focuses on interaction semantics:
    - committed transcript written as static content (primary screen),
    - dynamic pending output in the live UI,
    - explicit states (idle/responding/waiting confirmation),
    - resize-aware layout with constrained/expanded live area. *)

open Mosaic

type stream_state = Idle | Responding | Waiting_for_confirmation

type step =
  | Emit of string
  | Tool_start of string
  | Tool_finish of string
  | Need_confirmation of {
      prompt : string;
      on_yes : step list;
      on_no : step list;
    }

type waiting = {
  prompt : string;
  on_yes : step list;
  on_no : step list;
  tail : step list;
}

type model = {
  input : string;
  pending_text : string;
  pending_tool : string option;
  last_user : string option;
  queue : step list;
  waiting : waiting option;
  state : stream_state;
  committed_turns : int;
}

type msg =
  | Set_input of string
  | Submit of string
  | Tick
  | Confirm of bool
  | Quit

let initial_model =
  {
    input = "";
    pending_text = "";
    pending_tool = None;
    last_user = None;
    queue = [];
    waiting = None;
    state = Idle;
    committed_turns = 0;
  }

let trim = String.trim

let string_contains ~needle hay =
  if needle = "" then true
  else
    let hay = String.lowercase_ascii hay in
    let needle = String.lowercase_ascii needle in
    let n = String.length needle in
    let m = String.length hay in
    let rec loop i =
      if i + n > m then false
      else if String.sub hay i n = needle then true
      else loop (i + 1)
    in
    loop 0

let append_chunk text chunk = if text = "" then chunk else text ^ chunk
let append_line text line = if text = "" then line else text ^ "\n" ^ line

let has_tool_intent prompt =
  string_contains ~needle:"file" prompt
  || string_contains ~needle:"files" prompt
  || string_contains ~needle:"list" prompt
  || string_contains ~needle:"ls" prompt
  || string_contains ~needle:"read" prompt

let plan_for_prompt prompt =
  if has_tool_intent prompt then
    [
      Emit "I should inspect local workspace context. ";
      Need_confirmation
        {
          prompt = "Allow tool call: list_directory \".\" ?";
          on_yes =
            [
              Tool_start "list_directory \".\"";
              Emit "Running tool... ";
              Tool_finish "Tool result: found example files and source folders.";
              Emit "Using that context, I can summarize next steps. ";
            ];
          on_no =
            [ Emit "Skipping tool call; answering from prompt context only. " ];
        };
      Emit "Done.";
    ]
  else
    [
      Emit "Drafting response";
      Emit "... ";
      Emit "done. ";
      Emit "No external tool needed for this request.";
    ]

let turn_block ~role ~accent content =
  let body =
    let t = trim content in
    if t = "" then "_(empty)_" else t
  in
  text
    ~style:(Ansi.Style.make ~fg:accent ())
    ~wrap:`Word
    ~size:{ width = pct 100; height = auto }
    (role ^ "> " ^ body)

let assistant_text model =
  let t = trim model.pending_text in
  if t <> "" then model.pending_text
  else if model.state = Idle then "(no response)"
  else "Assistant output will stream here..."

let active_turn_view model =
  match model.last_user with
  | None -> empty
  | Some prompt ->
      box ~display:Display.Block ~flex_direction:Column ~gap:(gap 1)
        ~size:{ width = pct 100; height = auto }
        [
          turn_block ~role:"user" ~accent:Ansi.Color.cyan prompt;
          (match model.pending_tool with
          | None -> empty
          | Some label ->
              box ~flex_direction:Row ~gap:(gap 1)
                [ spinner ~color:Ansi.Color.cyan (); text ("Tool: " ^ label) ]);
          turn_block ~role:"assistant" ~accent:Ansi.Color.green
            (assistant_text model);
          (match model.waiting with
          | None -> empty
          | Some w ->
              box ~padding:(padding 1) ~border:true
                ~border_color:Ansi.Color.yellow
                [
                  text
                    ~style:(Ansi.Style.make ~fg:Ansi.Color.yellow ~bold:true ())
                    ("Action required: " ^ w.prompt);
                  text
                    ~style:(Ansi.Style.make ~dim:true ())
                    "Press y to approve, n to deny.";
                ]);
        ]

let init () =
  let banner =
    Cmd.static_commit
      (text "✻ x-agent\n  primary screen · seamless static commit\n")
  in
  (initial_model, banner)

let finalize_turn m =
  let commit_cmd =
    match m.last_user with
    | None -> Cmd.none
    | Some _ -> Cmd.static_commit (active_turn_view m)
  in
  ( {
      m with
      pending_text = "";
      pending_tool = None;
      last_user = None;
      queue = [];
      waiting = None;
      state = Idle;
      committed_turns = m.committed_turns + 1;
    },
    commit_cmd )

let update msg model =
  match msg with
  | Set_input value -> ({ model with input = value }, Cmd.none)
  | Submit raw_prompt ->
      let prompt = trim raw_prompt in
      if model.state <> Idle || prompt = "" then (model, Cmd.none)
      else
        let queue = plan_for_prompt prompt in
        ( {
            model with
            input = "";
            pending_text = "";
            pending_tool = None;
            last_user = Some prompt;
            queue;
            waiting = None;
            state = Responding;
          },
          Cmd.none )
  | Tick -> (
      match (model.state, model.waiting, model.queue) with
      | Responding, None, [] -> finalize_turn model
      | Responding, None, step :: tail -> (
          match step with
          | Emit chunk ->
              ( {
                  model with
                  pending_text = append_chunk model.pending_text chunk;
                  queue = tail;
                },
                Cmd.none )
          | Tool_start label ->
              ({ model with pending_tool = Some label; queue = tail }, Cmd.none)
          | Tool_finish result ->
              ( {
                  model with
                  pending_tool = None;
                  pending_text = append_line model.pending_text result;
                  queue = tail;
                },
                Cmd.none )
          | Need_confirmation { prompt; on_yes; on_no } ->
              ( {
                  model with
                  state = Waiting_for_confirmation;
                  waiting = Some { prompt; on_yes; on_no; tail };
                },
                Cmd.none ))
      | _ -> (model, Cmd.none))
  | Confirm allowed -> (
      match model.waiting with
      | None -> (model, Cmd.none)
      | Some w ->
          let next = (if allowed then w.on_yes else w.on_no) @ w.tail in
          ( {
              model with
              queue = next;
              waiting = None;
              state = Responding;
              pending_text =
                append_line model.pending_text
                  (if allowed then "User approved tool execution."
                   else "User denied tool execution.");
            },
            Cmd.none ))
  | Quit -> (model, Cmd.quit)

let view model =
  let subtle = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) () in
  let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) () in
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = auto }
    [
      active_turn_view model;
      box ~flex_direction:Column ~padding:(padding 1) ~gap:(gap 1)
        ~size:{ width = pct 100; height = auto }
        [
          text ~style:subtle
            (match model.state with
            | Idle -> "idle"
            | Responding -> "responding"
            | Waiting_for_confirmation -> "waiting_for_confirmation");
          box ~border:true ~title:"Prompt" ~padding:(padding 1)
            ~size:{ width = pct 100; height = auto }
            [
              box ~flex_direction:Row ~align_items:Center
                ~size:{ width = pct 100; height = auto }
                [
                  text ~style:(Ansi.Style.make ~bold:true ()) "❯ ";
                  input ~autofocus:true ~value:model.input
                    ~placeholder:
                      (match model.state with
                      | Idle -> "Ask anything"
                      | Responding -> "Assistant is responding..."
                      | Waiting_for_confirmation ->
                          "Waiting for confirmation (y/n)...")
                    ~size:{ width = pct 100; height = px 1 }
                    ~on_input:(fun v -> Some (Set_input v))
                    ~on_submit:(fun v -> Some (Submit v))
                    ();
                ];
            ];
          text ~style:hint
            (Printf.sprintf "Enter submit · y/n confirm · q quit · committed=%d"
               model.committed_turns);
        ];
    ]

let subscriptions model =
  let tick_sub =
    match model.state with
    | Responding -> Sub.every 0.08 (fun () -> Tick)
    | _ -> Sub.none
  in
  Sub.batch
    [
      tick_sub;
      Sub.on_key_all (fun ev ->
          let key = (Event.Key.data ev).key in
          match key with
          | Escape -> Some Quit
          | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
          | Char c
            when model.state = Waiting_for_confirmation
                 && Uchar.equal c (Uchar.of_char 'y') ->
              Some (Confirm true)
          | Char c
            when model.state = Waiting_for_confirmation
                 && Uchar.equal c (Uchar.of_char 'n') ->
              Some (Confirm false)
          | _ -> None);
    ]

let () =
  let matrix =
    Matrix.create ~mode:`Primary ~target_fps:(Some 30.) ~cursor_visible:false
      ~mouse_enabled:false ()
  in
  (match Matrix.mode matrix with
  | `Primary -> ()
  | `Alt -> failwith "x-agent must run in primary mode");
  run ~matrix { init; update; view; subscriptions }
