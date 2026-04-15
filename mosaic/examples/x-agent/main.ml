(** Agent-style TUI modelled after Claude Code's non-fullscreen layout.

    Stress-tests Mosaic's primary-screen rendering with markdown, code blocks,
    tool calls, and streaming content. *)

open Mosaic

(* ── Model ── *)

type tool_status = Running | Done [@@warning "-37"]

type tool_call = {
  name : string;
  args : string;
  output : string option;
  status : tool_status;
}

type stream_state = Idle | Streaming | Waiting_confirmation of string

type step =
  | Emit of string
  | Tool_start of { name : string; args : string }
  | Tool_finish of { output : string }
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
  tools : tool_call list;
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
    tools = [];
    last_user = None;
    queue = [];
    waiting = None;
    state = Idle;
    committed_turns = 0;
  }

(* ── Scenarios ── *)

let read_file_output =
  {|```ocaml
type t = {
  mutable render_offset : int;
  mutable tui_height : int;
  mutable static_queue : (string * int) list;
  mutable scroll_hint : Screen.scroll_hint option;
  mutable force_full_next_frame : bool;
}
```|}

let test_output =
  {|```
Testing matrix.runtime.

........

All tests passed in 18ms. 8 tests run.
```|}

let complex_plan =
  [
    Emit "I'll look at the project structure first.\n\n";
    Need_confirmation
      {
        prompt = "Read file: matrix/lib/matrix.ml";
        on_yes =
          [
            Tool_start { name = "Read"; args = "matrix/lib/matrix.ml" };
            Tool_finish { output = read_file_output };
            Emit
              "I can see the `app` type has been simplified. The key fields \
               are:\n\n\
               - `render_offset` \xe2\x80\x94 where the dynamic area starts\n\
               - `tui_height` \xe2\x80\x94 height of the dynamic region\n\
               - `scroll_hint` \xe2\x80\x94 DECSTBM optimisation for ScrollBox\n\
               - `force_full_next_frame` \xe2\x80\x94 forces full re-render\n\n\
               The static content handling now uses **erase-and-rewrite** \
               instead of DECSTBM scroll regions. Let me verify the tests \
               pass.\n\n";
            Need_confirmation
              {
                prompt = "Run: dune exec matrix/test/test_runtime.exe";
                on_yes =
                  [
                    Tool_start
                      {
                        name = "Bash";
                        args = "dune exec matrix/test/test_runtime.exe";
                      };
                    Tool_finish { output = test_output };
                    Emit
                      "All **8 tests pass**. The refactoring is working \
                       correctly.\n\n\
                       Here's a summary of what changed:\n\n\
                       | Component | Before | After |\n\
                       |-----------|--------|-------|\n\
                       | Static content | DECSTBM scroll regions | \
                       Erase-and-rewrite |\n\
                       | Scroll regions | Used in Primary mode | Removed \
                       from Primary |\n\
                       | Frame output | Multiple I/O calls | Single \
                       buffered write |\n\
                       | Alt mode | No cursor anchor | CSI H self-healing \
                       |\n\n\
                       The net change is **-12 lines** with cleaner \
                       separation of concerns.\n";
                  ];
                on_no = [ Emit "Skipping test run.\n" ];
              };
          ];
        on_no = [ Emit "OK, I'll work from the context I have.\n" ];
      };
    Emit "\nLet me know if you'd like me to make any further changes.\n";
  ]

let simple_plan =
  [
    Emit "Sure, I can help with that.\n\n";
    Emit
      "Here are a few approaches we could take:\n\n\
       1. **Direct implementation** \xe2\x80\x94 write the code inline, \
       simple and fast\n\
       2. **Module extraction** \xe2\x80\x94 separate the logic into its \
       own module with a clean `.mli`\n\
       3. **Test-driven** \xe2\x80\x94 write the tests first, then \
       implement to pass them\n\n\
       For a change this size, I'd recommend option 2. The module boundary \
       gives us:\n\n\
       - Type safety at the interface\n\
       - Easier testing in isolation\n\
       - Clear documentation via the `.mli`\n\n\
       ```ocaml\n\
       (* proposed signature *)\n\
       val create : ?mode:mode -> unit -> t\n\
       val submit : t -> unit\n\
       val set_scroll_hint : t -> Screen.scroll_hint -> unit\n\
       ```\n\n\
       Want me to proceed with this approach?\n";
  ]

let has_tool_intent prompt =
  let p = String.lowercase_ascii prompt in
  let has s =
    let n = String.length s in
    let m = String.length p in
    let rec loop i =
      if i + n > m then false
      else if String.sub p i n = s then true
      else loop (i + 1)
    in
    loop 0
  in
  has "file" || has "read" || has "matrix" || has "test" || has "code"

let plan_for_prompt prompt =
  if has_tool_intent prompt then complex_plan else simple_plan

(* ── Theme ── *)

let subtle = Ansi.Color.grayscale ~level:14
let dim_style = Ansi.Style.make ~fg:subtle ()
let user_msg_bg = Ansi.Color.of_rgb 55 55 55
let green = Ansi.Color.of_rgb 77 217 128
let yellow = Ansi.Color.yellow
let rule_color = Ansi.Color.grayscale ~level:8

(* ── Symbols ── *)

let s_prompt = "\xe2\x9d\xaf"       (* ❯ *)
let s_circle = "\xe2\x8f\xba"       (* ⏺ *)
let s_check = "\xe2\x9c\x93"        (* ✓ *)

(* ── Views ── *)

let user_message_view prompt =
  box ~flex_direction:Column
    ~margin:(margin_lrtb 0 0 1 0) ~padding:(padding_lrtb 0 1 0 0)
    ~background:user_msg_bg
    ~size:{ width = pct 100; height = auto }
    [
      box ~flex_direction:Row
        [ text ~style:(Ansi.Style.make ~fg:subtle ())
            (s_prompt ^ " ");
          text ~wrap:`Word prompt ];
    ]

let tool_call_view tc =
  let indicator, label_style =
    match tc.status with
    | Running ->
        ( spinner ~color:green
            ~size:{ width = px 2; height = px 1 } (),
          Ansi.Style.make ~bold:true () )
    | Done ->
        ( text ~style:(Ansi.Style.make ~fg:green ())
            (s_check ^ " "),
          Ansi.Style.make ~bold:true ~dim:true () )
  in
  let header =
    box ~flex_direction:Row ~gap:(gap 1)
      [ indicator;
        text ~style:label_style tc.name;
        text ~style:dim_style tc.args ]
  in
  let output_view =
    match tc.output with
    | None -> empty
    | Some out ->
        markdown ~size:{ width = pct 100; height = auto }
          ~margin:(margin_lrtb 2 0 0 0) out
  in
  box ~flex_direction:Column ~margin:(margin_lrtb 0 0 1 0)
    ~size:{ width = pct 100; height = auto }
    [ header; output_view ]

let assistant_text_view ?(streaming = false) content =
  if String.length (String.trim content) = 0 then empty
  else
    box ~flex_direction:Row ~margin:(margin_lrtb 0 0 1 0)
      ~size:{ width = pct 100; height = auto }
      [
        text ~style:(Ansi.Style.make ~fg:(Ansi.Color.white) ())
          (s_circle ^ " ");
        markdown ~streaming
          ~size:{ width = pct 100; height = auto } content;
      ]

let confirmation_view prompt =
  box ~flex_direction:Row ~gap:(gap 1) ~margin:(margin_lrtb 0 0 1 0)
    ~size:{ width = pct 100; height = auto }
    [
      text ~style:(Ansi.Style.make ~fg:yellow ~bold:true ()) prompt;
      text ~style:dim_style "(y/n)";
    ]

let active_turn_view model =
  match model.last_user with
  | None -> empty
  | Some prompt ->
      box ~flex_direction:Column
        ~size:{ width = pct 100; height = auto }
        (List.concat
           [
             [ user_message_view prompt ];
             List.map tool_call_view model.tools;
             [ assistant_text_view
                 ~streaming:(model.state = Streaming)
                 model.pending_text ];
             (match model.waiting with
             | None -> []
             | Some w -> [ confirmation_view w.prompt ]);
           ])

let input_view model =
  box ~border:true ~border_sides:[ `Top; `Bottom ]
    ~border_color:rule_color
    ~size:{ width = pct 100; height = auto }
    [
      box ~flex_direction:Row ~align_items:Center
        ~size:{ width = pct 100; height = px 1 }
        [
          text ~style:(Ansi.Style.make ~bold:true ~fg:green ())
            (s_prompt ^ " ");
          input ~autofocus:true ~value:model.input
            ~placeholder:
              (match model.state with
              | Idle -> ""
              | Streaming -> ""
              | Waiting_confirmation _ -> "")
            ~size:{ width = pct 100; height = px 1 }
            ~on_input:(fun v -> Some (Set_input v))
            ~on_submit:(fun v -> Some (Submit v))
            ();
        ];
    ]

let footer_view model =
  let status =
    match model.state with
    | Idle -> ""
    | Streaming -> "streaming"
    | Waiting_confirmation _ -> "awaiting confirmation"
  in
  let left =
    if status = "" then
      Printf.sprintf "  turns: %d" model.committed_turns
    else
      Printf.sprintf "  %s \xc2\xb7 turns: %d" status model.committed_turns
  in
  text ~style:dim_style left

(* ── Init / Update ── *)

let init () =
  let banner =
    Cmd.static_commit
      (box ~flex_direction:Column
         ~size:{ width = pct 100; height = auto }
         [
           text ~style:(Ansi.Style.make ~bold:true ())
             "x-agent";
           text ~style:dim_style
             "Primary screen \xc2\xb7 erase-and-rewrite static commits";
           text "";
         ])
  in
  (initial_model, banner)

let finalize_turn m =
  let commit_cmd =
    match m.last_user with
    | None -> Cmd.none
    | Some _ -> Cmd.static_commit (active_turn_view m)
  in
  ( { m with
      pending_text = "";
      tools = [];
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
  | Submit raw ->
      let prompt = String.trim raw in
      if model.state <> Idle || prompt = "" then (model, Cmd.none)
      else
        ( { model with
            input = "";
            pending_text = "";
            tools = [];
            last_user = Some prompt;
            queue = plan_for_prompt prompt;
            waiting = None;
            state = Streaming;
          },
          Cmd.none )
  | Tick -> (
      match (model.state, model.waiting, model.queue) with
      | Streaming, None, [] -> finalize_turn model
      | Streaming, None, step :: tail -> (
          match step with
          | Emit chunk ->
              ( { model with
                  pending_text = model.pending_text ^ chunk;
                  queue = tail;
                },
                Cmd.none )
          | Tool_start { name; args } ->
              ( { model with
                  tools =
                    model.tools
                    @ [ { name; args; output = None; status = Running } ];
                  queue = tail;
                },
                Cmd.none )
          | Tool_finish { output } ->
              let tools =
                List.map
                  (fun tc ->
                    if tc.status = Running then
                      { tc with status = Done; output = Some output }
                    else tc)
                  model.tools
              in
              ({ model with tools; queue = tail }, Cmd.none)
          | Need_confirmation { prompt; on_yes; on_no } ->
              ( { model with
                  state = Waiting_confirmation prompt;
                  waiting = Some { prompt; on_yes; on_no; tail };
                },
                Cmd.none ))
      | _ -> (model, Cmd.none))
  | Confirm allowed -> (
      match model.waiting with
      | None -> (model, Cmd.none)
      | Some w ->
          let next = (if allowed then w.on_yes else w.on_no) @ w.tail in
          ( { model with
              queue = next;
              waiting = None;
              state = Streaming;
              pending_text =
                model.pending_text
                ^ (if allowed then "" else "_Denied._\n\n");
            },
            Cmd.none ))
  | Quit -> (model, Cmd.quit)

(* ── View ── *)

let view model =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = auto }
    [
      active_turn_view model;
      input_view model;
      footer_view model;
    ]

(* ── Subscriptions ── *)

let subscriptions model =
  let tick =
    match model.state with
    | Streaming -> Sub.every 0.05 (fun () -> Tick)
    | _ -> Sub.none
  in
  Sub.batch
    [
      tick;
      Sub.on_key_all (fun ev ->
          match (Event.Key.data ev).key with
          | Escape -> Some Quit
          | Char c
            when Uchar.equal c (Uchar.of_char 'q') && model.state = Idle ->
              Some Quit
          | Char c
            when model.state <> Idle && Uchar.equal c (Uchar.of_char 'y') ->
              Some (Confirm true)
          | Char c
            when model.state <> Idle && Uchar.equal c (Uchar.of_char 'n') ->
              Some (Confirm false)
          | _ -> None);
    ]

(* ── Entry point ── *)

let () =
  let matrix =
    Matrix.create ~mode:`Primary ~target_fps:(Some 30.) ~cursor_visible:false
      ~mouse_enabled:false ()
  in
  (match Matrix.mode matrix with
  | `Primary -> ()
  | `Alt -> failwith "x-agent must run in primary mode");
  run ~matrix { init; update; view; subscriptions }
