open Windtrap
open Mosaic_ui
open Test_harness

let red = Ansi.Style.fg Ansi.Color.red Ansi.Style.default
let blue = Ansi.Style.fg Ansi.Color.blue Ansi.Style.default

let syntax_style color =
  Syntax_style.make ~base:Ansi.Style.default [ ("keyword", color) ]

let highlights =
  Syntax_highlight.of_triples ~content:"let x" [ (0, 3, "keyword") ]

let make_code ?content ?syntax ?text_style () =
  let t = make_ctx () in
  let root = make_root t in
  let code = Code.create ~parent:root ?content ?syntax ?text_style () in
  (t, code)

let render_code code =
  let grid = make_grid ~width:40 ~height:5 () in
  Renderable.Private.render_full (Code.node code) ~grid ~dt:0.

let styled_text code = Text_buffer.styled_text (Code.buffer code)
let span_text span = (span : Text_buffer.span).text
let span_style span = (span : Text_buffer.span).style

type async_job = {
  request : unit Code.Highlighter.request;
  notify : unit -> unit;
  mutable outcome : unit Code.Highlighter.result option;
  mutable cancelled : bool;
}

let async_highlighter () =
  let jobs = ref [] in
  let highlighter =
    Code.Highlighter.async (fun request ~notify ->
        let job = { request; notify; outcome = None; cancelled = false } in
        jobs := !jobs @ [ job ];
        Code.Highlighter.job
          ~poll:(fun () ->
            match job.outcome with
            | None -> None
            | Some outcome ->
                job.outcome <- None;
                Some outcome)
          ~cancel:(fun () -> job.cancelled <- true))
  in
  (jobs, highlighter)

let complete job outcome =
  job.outcome <- Some (Result.map (fun captures -> (captures, ())) outcome);
  job.notify ()

(* A tab width below 1 is a mistake, raised by the call that makes it. *)
let props_reject_tab_width_below_1 () =
  raises_match (Exn.invalid_arg ~substring:"tab width") (fun () ->
      ignore (Code.Props.make ~tab_width:0 () : Code.Props.t))

let plain_without_syntax () =
  let _ctx, code = make_code ~content:"let x = 1" () in
  equal ~msg:"plain text" string "let x = 1"
    (Text_buffer.plain_text (Code.buffer code));
  equal ~msg:"span count" int 1 (List.length (styled_text code))

let precomputed_highlights_apply_style () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  match styled_text code with
  | first :: _ ->
      equal ~msg:"highlighted text" string "let" (span_text first);
      is_true ~msg:"highlighted style" (Ansi.Style.equal red (span_style first))
  | [] -> fail "expected highlighted span"

let sync_highlighter_applies_style () =
  let highlighter =
    Code.Highlighter.sync (fun ~language ~content ->
        equal ~msg:"request content" string "let x" content;
        equal ~msg:"request language" string "ocaml" language;
        Syntax_highlight.of_triples ~content [ (0, 3, "keyword") ])
  in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_false ~msg:"not highlighting" (Code.is_highlighting code);
  match styled_text code with
  | first :: _ ->
      equal ~msg:"highlighted text" string "let" (span_text first);
      is_true ~msg:"highlighted style" (Ansi.Style.equal red (span_style first))
  | [] -> fail "expected highlighted span"

let async_highlighter_applies_completed_result () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_true ~msg:"highlighting" (Code.is_highlighting code);
  equal ~msg:"plain while pending" string "let x"
    (Text_buffer.plain_text (Code.buffer code));
  let job = List.hd !jobs in
  equal ~msg:"request content" string "let x" job.request.content;
  equal ~msg:"request language" string "ocaml" job.request.language;
  complete job (Ok highlights);
  render_code code;
  is_false ~msg:"done highlighting" (Code.is_highlighting code);
  match styled_text code with
  | first :: _ ->
      is_true ~msg:"highlighted style" (Ansi.Style.equal red (span_style first))
  | [] -> fail "expected highlighted span"

let style_of code byte = fst (Text_buffer.style_at (Code.buffer code) byte)
let is_red code byte = Ansi.Style.equal red (style_of code byte)

let keyword_at content start_byte end_byte =
  Syntax_highlight.of_triples ~content [ (start_byte, end_byte, "keyword") ]

let pending_append_keeps_highlighted_prefix () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  complete (List.hd !jobs) (Ok highlights);
  render_code code;
  Code.set_content code "let x = 1";
  is_true ~msg:"highlighting" (Code.is_highlighting code);
  equal ~msg:"current content shown" string "let x = 1"
    (Text_buffer.plain_text (Code.buffer code));
  is_true ~msg:"shared prefix keeps its styling" (is_red code 0);
  is_false ~msg:"appended tail is plain" (is_red code 8);
  let second = List.nth !jobs 1 in
  complete second (Ok (keyword_at second.request.content 6 7));
  render_code code;
  is_false ~msg:"done" (Code.is_highlighting code);
  is_false ~msg:"fresh result replaces carried styling" (is_red code 0);
  is_true ~msg:"fresh result applied" (is_red code 6)

let pending_edit_keeps_shared_prefix_and_suffix () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x in y" ~syntax () in
  complete (List.hd !jobs)
    (Ok
       (Syntax_highlight.of_triples ~content:"let x in y"
          [ (0, 3, "keyword"); (4, 5, "keyword"); (6, 8, "keyword") ]));
  render_code code;
  Code.set_content code "let yy in y";
  equal ~msg:"current content shown" string "let yy in y"
    (Text_buffer.plain_text (Code.buffer code));
  is_true ~msg:"prefix range kept" (is_red code 0);
  is_false ~msg:"range over changed bytes dropped" (is_red code 4);
  is_true ~msg:"suffix range shifted" (is_red code 7 && is_red code 8);
  is_false ~msg:"shift is exact" (is_red code 6)

let pending_edit_inside_a_range_keeps_its_other_bytes () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"(* hello *) x" ~syntax () in
  complete (List.hd !jobs) (Ok (keyword_at "(* hello *) x" 0 11));
  render_code code;
  Code.set_content code "(* helxlo *) x";
  is_true ~msg:"highlighting" (Code.is_highlighting code);
  is_true ~msg:"the range keeps its bytes before the edit"
    (List.for_all (is_red code) [ 0; 1; 2; 3; 4; 5 ]);
  is_false ~msg:"the inserted byte is plain" (is_red code 6);
  is_true ~msg:"the range keeps its bytes after the edit"
    (List.for_all (is_red code) [ 7; 8; 9; 10; 11 ]);
  is_false ~msg:"bytes outside the range stay plain" (is_red code 13);
  Code.set_content code "(* helo *) x";
  is_true ~msg:"a deletion keeps both sides"
    (List.for_all (is_red code) [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ])

let pending_edit_drops_a_touched_concealing_range () =
  let jobs, highlighter = async_highlighter () in
  let meta = { Syntax_highlight.default_meta with conceal = Some "*" } in
  let concealed content =
    Syntax_highlight.make ~content
      [
        Syntax_highlight.range ~meta ~start_byte:0 ~end_byte:4 ~scope:"keyword"
          ();
      ]
  in
  let syntax =
    Code.with_highlighter ~language:"md" ~style:(syntax_style red) highlighter
  in
  let _ctx, code = make_code ~content:"abcd e" ~syntax () in
  complete (List.hd !jobs) (Ok (concealed "abcd e"));
  render_code code;
  equal ~msg:"concealed" string "* e"
    (Text_buffer.plain_text (Code.buffer code));
  Code.set_content code "abXcd e";
  equal ~msg:"a touched concealing range shows its bytes" string "abXcd e"
    (Text_buffer.plain_text (Code.buffer code))

let stale_result_styles_shared_bytes () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  Code.set_content code "let x = 1";
  is_false ~msg:"nothing to carry yet" (is_red code 0);
  complete (List.hd !jobs) (Ok highlights);
  render_code code;
  is_true ~msg:"still highlighting the current content"
    (Code.is_highlighting code);
  equal ~msg:"current content shown" string "let x = 1"
    (Text_buffer.plain_text (Code.buffer code));
  is_true ~msg:"stale result styles the shared bytes" (is_red code 0);
  equal ~msg:"current content requested" string "let x = 1"
    (List.nth !jobs 1).request.content

let new_producer_starts_plain () =
  let jobs, highlighter = async_highlighter () in
  let next_jobs, next_highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  complete (List.hd !jobs) (Ok highlights);
  render_code code;
  Code.set_syntax code
    (Some
       (Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
          next_highlighter));
  is_true ~msg:"highlighting" (Code.is_highlighting code);
  is_false ~msg:"another producer's captures are not carried" (is_red code 0);
  equal int 1 (List.length !next_jobs)

let async_content_changes_coalesce () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  let first_job = List.hd !jobs in
  Code.set_content code "var x";
  for i = 1 to 100 do
    Code.set_content code (Printf.sprintf "var %d" i)
  done;
  Code.set_content code "var x";
  equal ~msg:"one running job during rapid updates" int 1 (List.length !jobs);
  is_false ~msg:"obsolete job remains observable until completion"
    first_job.cancelled;
  complete first_job (Ok highlights);
  render_code code;
  equal ~msg:"current plain text retained" string "var x"
    (Text_buffer.plain_text (Code.buffer code));
  let second_job = List.nth !jobs 1 in
  complete second_job
    (Ok
       (Syntax_highlight.of_triples ~content:second_job.request.content
          [ (0, 3, "keyword") ]));
  render_code code;
  match styled_text code with
  | first :: _ ->
      equal ~msg:"highlighted current content" string "var" (span_text first)
  | [] -> fail "expected highlighted span"

let async_configuration_changes_coalesce () =
  let jobs, highlighter = async_highlighter () in
  let next_jobs, next_highlighter = async_highlighter () in
  let syntax = Code.with_highlighter ~language:"ocaml" highlighter in
  let _ctx, code = make_code ~content:"old" ~syntax () in
  Code.set_content code "";
  is_false (Code.is_highlighting code);
  Code.set_syntax code
    (Some (Code.with_highlighter ~language:"json" next_highlighter));
  Code.set_content code "new";
  equal ~msg:"switching producer cannot overlap jobs" int 0
    (List.length !next_jobs);
  complete (List.hd !jobs) (Ok highlights);
  render_code code;
  let latest = List.hd !next_jobs in
  equal string "new" latest.request.content;
  equal string "json" latest.request.language;
  complete latest
    (Ok (Syntax_highlight.make ~content:latest.request.content []));
  render_code code;
  equal string "new" (Text_buffer.plain_text (Code.buffer code));
  is_false (Code.is_highlighting code)

let async_clear_discards_pending_request () =
  let jobs, highlighter = async_highlighter () in
  let syntax = Code.with_highlighter ~language:"ocaml" highlighter in
  let _ctx, code = make_code ~content:"old" ~syntax () in
  Code.set_content code "pending";
  Code.set_syntax code None;
  complete (List.hd !jobs) (Ok highlights);
  render_code code;
  equal ~msg:"removed syntax cannot restart pending work" int 1
    (List.length !jobs);
  equal string "pending" (Text_buffer.plain_text (Code.buffer code));
  is_false (Code.is_highlighting code)

let async_rows_stabilize_on_completion () =
  let jobs, highlighter = async_highlighter () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  let changes = ref 0 in
  Code.set_on_rows_change code (Some (fun () -> incr changes));
  is_true ~msg:"pending line info" (Code.is_highlighting code);
  complete (List.hd !jobs) (Ok highlights);
  equal ~msg:"callback waits for polling" int 0 !changes;
  render_code code;
  is_false ~msg:"stable line info" (Code.is_highlighting code);
  equal ~msg:"line info callback" int 1 !changes

let async_starter_exception_falls_back_to_plain_text () =
  let highlighter =
    Code.Highlighter.async (fun _request ~notify:_ ->
        raise (Failure "starter failed"))
  in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_false ~msg:"not highlighting" (Code.is_highlighting code);
  equal ~msg:"plain fallback" string "let x"
    (Text_buffer.plain_text (Code.buffer code))

let async_poll_exception_falls_back_to_plain_text () =
  let highlighter =
    Code.Highlighter.async (fun _request ~notify:_ ->
        Code.Highlighter.job
          ~poll:(fun () -> raise (Failure "poll failed"))
          ~cancel:(fun () -> ()))
  in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_false ~msg:"not highlighting" (Code.is_highlighting code);
  equal ~msg:"plain fallback" string "let x"
    (Text_buffer.plain_text (Code.buffer code))

let invalid_highlights_fall_back_to_plain_text () =
  let highlights =
    Syntax_highlight.of_triples ~content:"old" [ (0, 3, "keyword") ]
  in
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let" ~syntax () in
  equal ~msg:"plain text" string "let"
    (Text_buffer.plain_text (Code.buffer code));
  match styled_text code with
  | [ span ] ->
      is_true ~msg:"default style"
        (Ansi.Style.equal Ansi.Style.default (span_style span))
  | _ -> fail "expected one plain span"

let conceal_metadata_applies () =
  let meta = { Syntax_highlight.default_meta with conceal = Some "*" } in
  let highlights =
    Syntax_highlight.make ~content:"let x = 1"
      [
        Syntax_highlight.range ~meta ~start_byte:0 ~end_byte:3 ~scope:"keyword"
          ();
      ]
  in
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x = 1" ~syntax () in
  equal ~msg:"concealed text" string "* x = 1"
    (Text_buffer.plain_text (Code.buffer code))

let conceal_is_a_view_property () =
  let meta = { Syntax_highlight.default_meta with conceal = Some "*" } in
  let highlights =
    Syntax_highlight.make ~content:"let x = 1"
      [
        Syntax_highlight.range ~meta ~start_byte:0 ~end_byte:3 ~scope:"keyword"
          ();
      ]
  in
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x = 1" ~syntax () in
  Code.apply_props code
    (Code.Props.make ~content:"let x = 1" ~syntax ~conceal:false ());
  equal ~msg:"every byte shown" string "let x = 1"
    (Text_buffer.plain_text (Code.buffer code));
  is_true ~msg:"the capture still styles" (is_red code 0);
  Code.apply_props code (Code.Props.make ~content:"let x = 1" ~syntax ());
  equal ~msg:"concealed again" string "* x = 1"
    (Text_buffer.plain_text (Code.buffer code))

let set_content_reapplies_syntax () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let ctx, code = make_code ~content:"let x" ~syntax () in
  ignore ctx;
  Code.set_content code "var x";
  equal ~msg:"stale same-length captures leave current text plain" string
    "var x"
    (Text_buffer.plain_text (Code.buffer code));
  is_true
    (Ansi.Style.equal Ansi.Style.default
       (fst (Text_buffer.style_at (Code.buffer code) 0)))

let set_syntax_none_renders_plain () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  Code.set_syntax code None;
  match styled_text code with
  | [ span ] ->
      equal ~msg:"plain text" string "let x" (span_text span);
      is_true ~msg:"default style"
        (Ansi.Style.equal Ansi.Style.default (span_style span))
  | _ -> fail "expected one plain span"

let apply_props_updates_syntax_style () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  let syntax = Code.syntax ~style:(syntax_style blue) highlights in
  Code.apply_props code (Code.Props.make ~content:"let x" ~syntax ());
  match styled_text code with
  | first :: _ ->
      is_true ~msg:"updated style" (Ansi.Style.equal blue (span_style first))
  | [] -> fail "expected highlighted span"

let syntax_compares_styles_by_value () =
  let calls = ref 0 in
  let highlighter =
    Code.Highlighter.sync_incremental (fun { content; _ } ->
        incr calls;
        (Syntax_highlight.of_triples ~content [ (0, 3, "keyword") ], ()))
  in
  (* A view rebuilds its syntax settings, style included, on every render. *)
  let props style =
    Code.Props.make ~content:"let x"
      ~syntax:(Code.with_highlighter ~language:"ocaml" ~style highlighter)
      ()
  in
  is_true ~msg:"rebuilt equal styles are equal props"
    (Code.Props.equal (props (syntax_style red)) (props (syntax_style red)));
  is_false ~msg:"a changed style is a change"
    (Code.Props.equal (props (syntax_style red)) (props (syntax_style blue)));
  is_false ~msg:"a fresh highlighter is a new producer"
    (Code.Props.equal
       (Code.Props.make ~content:"let x"
          ~syntax:(Code.with_highlighter ~language:"ocaml" highlighter)
          ())
       (Code.Props.make ~content:"let x"
          ~syntax:
            (Code.with_highlighter ~language:"ocaml"
               (Code.Highlighter.sync_incremental (fun { content; _ } ->
                    (Syntax_highlight.make ~content [], ()))))
          ()));
  let _ctx, code = make_code () in
  Code.apply_props code (props (syntax_style red));
  Code.apply_props code (props (syntax_style blue));
  equal ~msg:"a style change restyles without highlighting again" int 1 !calls;
  match styled_text code with
  | first :: _ ->
      is_true ~msg:"restyled" (Ansi.Style.equal blue (span_style first))
  | [] -> fail "expected highlighted span"

let text_style_restamps_plain_content () =
  let _ctx, code = make_code ~content:"plain" () in
  Code.apply_props code (Code.Props.make ~content:"plain" ~text_style:red ());
  match styled_text code with
  | [ span ] ->
      is_true ~msg:"restamped style" (Ansi.Style.equal red (span_style span))
  | _ -> fail "expected one plain span"

let is_highlighting_false_for_sync_highlights () =
  let syntax = Code.syntax ~style:(syntax_style red) highlights in
  let _ctx, code = make_code ~content:"let x" ~syntax () in
  is_false ~msg:"not highlighting" (Code.is_highlighting code)

let await label ready =
  let deadline = Unix.gettimeofday () +. 5. in
  while not (ready ()) do
    if Unix.gettimeofday () >= deadline then fail ("timed out: " ^ label);
    Thread.delay 0.001
  done

let join_worker label worker =
  let joined = Atomic.make false in
  ignore
    (Thread.create
       (fun () ->
         Thread.join worker;
         Atomic.set joined true)
       ()
      : Thread.t);
  await label (fun () -> Atomic.get joined)

let gate () =
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  let released = ref false in
  let wait () =
    Mutex.protect mutex (fun () ->
        while not !released do
          Condition.wait condition mutex
        done)
  in
  let release () =
    Mutex.protect mutex (fun () ->
        released := true;
        Condition.broadcast condition)
  in
  (wait, release)

let threaded_requests_are_bounded_and_cancelled () =
  let wait, release = gate () in
  let calls = Atomic.make [] in
  let highlighter =
    Code.Highlighter.threaded (fun ~language ~content ->
        if language <> "ocaml" then failwith "wrong request language";
        Atomic.set calls (content :: Atomic.get calls);
        if content = "let old" then wait ();
        Syntax_highlight.of_triples ~content [ (0, 3, "keyword") ])
  in
  let renderer = Renderer.create () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let code =
    Code.create ~parent:(Renderer.root renderer) ~content:"let old" ~syntax ()
  in
  Fun.protect ~finally:release (fun () ->
      await "first request started" (fun () -> Atomic.get calls <> []);
      let obsolete = Weak.create 1 in
      let queue_obsolete () =
        let content = String.make 10000 'x' in
        Weak.set obsolete 0 (Some content);
        Code.set_content code content
      in
      queue_obsolete ();
      for i = 1 to 100 do
        Code.set_content code (Printf.sprintf "let edit%d" i)
      done;
      Code.set_content code "let final";
      let discarded =
        Code.create ~parent:(Renderer.root renderer) ~content:"let discarded"
          ~syntax ()
      in
      Renderable.destroy (Code.node discarded);
      let other =
        Code.create ~parent:(Renderer.root renderer) ~content:"let other"
          ~syntax ()
      in
      Thread.delay 0.01;
      equal ~msg:"one executing request, no worker per edit" (list string)
        [ "let old" ] (Atomic.get calls);
      Gc.full_major ();
      is_false ~msg:"cancelled queued source is released"
        (Weak.check obsolete 0);
      release ();
      await "live requests completed" (fun () ->
          render_code code;
          render_code other;
          not (Code.is_highlighting code || Code.is_highlighting other));
      equal ~msg:"only live FIFO requests execute" (list string)
        [ "let old"; "let other"; "let final" ]
        (List.rev (Atomic.get calls));
      equal ~msg:"stale result cannot replace current source" string "let final"
        (Text_buffer.plain_text (Code.buffer code));
      is_false ~msg:"destroyed widget cancelled"
        (Code.is_highlighting discarded))

let threaded_failure_and_idle_restart () =
  let worker = Atomic.make None in
  let highlighter =
    Code.Highlighter.threaded_incremental (fun { content; _ } ->
        Atomic.set worker (Some (Thread.self ()));
        if content = "bad" then failwith "highlight failed"
        else (Syntax_highlight.of_triples ~content [ (0, 3, "keyword") ], ()))
  in
  let renderer = Renderer.create () in
  let syntax =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style red)
      highlighter
  in
  let code =
    Code.create ~parent:(Renderer.root renderer) ~content:"bad" ~syntax ()
  in
  await "failure delivered" (fun () ->
      render_code code;
      not (Code.is_highlighting code));
  equal ~msg:"exception falls back to plain text" string "bad"
    (Text_buffer.plain_text (Code.buffer code));
  let first = Option.get (Atomic.get worker) in
  join_worker "idle worker exits" first;
  Code.set_content code "let x";
  await "restarted worker delivered" (fun () ->
      render_code code;
      not (Code.is_highlighting code));
  let second = Option.get (Atomic.get worker) in
  is_false ~msg:"idle worker is replaced on demand"
    (Thread.id first = Thread.id second);
  join_worker "restarted worker exits" second;
  match styled_text code with
  | first :: _ ->
      is_true ~msg:"worker survives an earlier failure"
        (Ansi.Style.equal red (span_style first))
  | [] -> fail "expected highlighted span"

let destroy_cancels_job_once () =
  let cancellations = ref 0 in
  let highlighter =
    Code.Highlighter.async (fun _ ~notify:_ ->
        Code.Highlighter.job
          ~poll:(fun () -> None)
          ~cancel:(fun () -> incr cancellations))
  in
  let syntax = Code.with_highlighter ~language:"ocaml" highlighter in
  let ctx, code = make_code ~content:"let x" ~syntax () in
  Code.set_content code "pending";
  Renderable.destroy (Code.node code);
  Renderable.destroy (Code.node code);
  equal ~msg:"cleanup is idempotent" int 1 !cancellations;
  is_false ~msg:"destroyed code has no pending job" (Code.is_highlighting code);
  equal ~msg:"node unregistered" (list string)
    [ Renderable.id (Code.node code) ]
    !(ctx.unregister_log)

let theme_changes_reuse_highlighter_captures () =
  let calls = ref 0 in
  let highlighter =
    Code.Highlighter.sync_incremental (fun { content; _ } ->
        incr calls;
        (Syntax_highlight.of_triples ~content [ (0, 3, "keyword") ], ()))
  in
  let syntax color =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style color)
      highlighter
  in
  let ctx, code = make_code ~content:"let x" ~syntax:(syntax red) () in
  ignore ctx;
  let source = Text_buffer.source (Code.buffer code) in
  let layout = Text_surface.layout (Code.surface code) in
  Code.apply_props code
    (Code.Props.make ~content:"let x" ~syntax:(syntax blue) ());
  equal ~msg:"theme does not invoke parser" int 1 !calls;
  is_true
    (Ansi.Style.equal blue (fst (Text_buffer.style_at (Code.buffer code) 0)));
  is_true ~msg:"theme keeps indexed source"
    (source == Text_buffer.source (Code.buffer code));
  is_true ~msg:"theme keeps layout"
    (layout == Text_surface.layout (Code.surface code));
  Code.set_content code "let y";
  equal ~msg:"source change invokes parser" int 2 !calls

let pending_result_uses_latest_theme () =
  let jobs, highlighter = async_highlighter () in
  let syntax color =
    Code.with_highlighter ~language:"ocaml" ~style:(syntax_style color)
      highlighter
  in
  let ctx, code = make_code ~content:"let x" ~syntax:(syntax red) () in
  ignore ctx;
  Code.set_syntax code (Some (syntax blue));
  equal ~msg:"theme creates no second job" int 1 (List.length !jobs);
  complete (List.hd !jobs) (Ok highlights);
  render_code code;
  equal ~msg:"completion creates no deferred second job" int 1
    (List.length !jobs);
  is_false (Code.is_highlighting code);
  is_true ~msg:"completion uses latest accepted theme"
    (Ansi.Style.equal blue (fst (Text_buffer.style_at (Code.buffer code) 0)))

let stale_async_capture_falls_back () =
  let jobs, highlighter = async_highlighter () in
  let ctx, code =
    make_code ~content:"let x"
      ~syntax:(Code.with_highlighter ~language:"ocaml" highlighter)
      ()
  in
  ignore ctx;
  complete (List.hd !jobs)
    (Ok (Syntax_highlight.of_triples ~content:"var x" [ (0, 3, "keyword") ]));
  render_code code;
  equal string "let x" (Text_buffer.plain_text (Code.buffer code));
  is_true
    (Ansi.Style.equal Ansi.Style.default
       (fst (Text_buffer.style_at (Code.buffer code) 0)))

let equivalent_capture_declaration_does_not_repaint () =
  let style = syntax_style red in
  let ctx, code =
    make_code ~content:"let x" ~syntax:(Code.syntax ~style highlights) ()
  in
  ignore ctx;
  let notifications = ref 0 in
  Code.set_on_rows_change code (Some (fun () -> incr notifications));
  let fresh =
    Syntax_highlight.of_triples
      ~content:(String.init 5 (String.get "let x"))
      [ (0, 3, "keyword") ]
  in
  Code.apply_props code
    (Code.Props.make ~content:"let x" ~syntax:(Code.syntax ~style fresh) ());
  equal ~msg:"equivalent capture declaration does not redo paint" int 0
    !notifications

(* ── Incremental highlighting ── *)

(* A toy incremental highlighter: its state is the words of the content, which
   a request with [previous] lexes again only around the edit. Its captures are
   always the whole highlight of its state. *)
module Words = struct
  type state = { id : int; words : (int * int) list }

  let is_letter c = c >= 'a' && c <= 'z'

  let lex content ~first ~last =
    let words = ref [] and i = ref first in
    while !i < last do
      if is_letter content.[!i] then begin
        let start = !i in
        while !i < last && is_letter content.[!i] do
          incr i
        done;
        words := (start, !i) :: !words
      end
      else incr i
    done;
    List.rev !words

  (* The words of [content] from the words of the previous content. *)
  let relex content (previous : state Code.Highlighter.previous) =
    let { Code.Highlighter.start; removed; inserted } = previous.edit in
    let shift = inserted - removed in
    let first = ref start and last = ref (start + inserted) in
    while !first > 0 && is_letter content.[!first - 1] do
      decr first
    done;
    while !last < String.length content && is_letter content.[!last] do
      incr last
    done;
    let before = List.filter (fun (_, e) -> e <= !first) previous.state.words in
    let after =
      List.filter_map
        (fun (s, e) ->
          if s + shift >= !last then Some (s + shift, e + shift) else None)
        previous.state.words
    in
    before @ lex content ~first:!first ~last:!last @ after

  let captures content words =
    Syntax_highlight.of_triples ~content
      (List.map
         (fun (s, e) ->
           ( s,
             e,
             if String.sub content s (e - s) = "let" then "keyword"
             else "variable" ))
         words)

  let fresh content =
    captures content (lex content ~first:0 ~last:(String.length content))
end

let words_style =
  Syntax_style.make ~base:Ansi.Style.default
    [ ("keyword", red); ("variable", blue) ]

let previous_carries_state_and_edit () =
  let seen = ref [] in
  let highlighter =
    Code.Highlighter.sync_incremental
      (fun ({ content; previous; _ } : int Code.Highlighter.request) ->
        seen := previous :: !seen;
        (Words.fresh content, String.length content))
  in
  let syntax = Code.with_highlighter ~language:"x" highlighter in
  let _ctx, code = make_code ~content:"let x = 1" ~syntax () in
  Code.set_content code "let xy = 1";
  (match !seen with
  | [ Some { content; state; edit = { start; removed; inserted } }; None ] ->
      equal ~msg:"previous content" string "let x = 1" content;
      equal ~msg:"state returned with it" int 9 state;
      equal ~msg:"edit" (triple int int int) (5, 0, 1) (start, removed, inserted)
  | _ -> fail "expected a first request without previous, then one with it");
  Code.set_syntax code (Some (Code.with_highlighter ~language:"y" highlighter));
  is_true ~msg:"a change of language starts afresh" (List.hd !seen = None)

(* Random scripts of edits, completions, failures and producer changes. The
   view must pass each request the content and state of its latest accepted
   result and the edit from it, show what a whole highlight of the content
   shows once a result arrives, and while one is pending keep the bytes outside
   the edit styled. *)
let incremental_requests_hold_on_random_scripts () =
  let random = Random.State.make [| 42 |] in
  let ids = ref 0 and content_of = Hashtbl.create 64 in
  let jobs = Queue.create () in
  (* A producer is a highlighter and a language; the view keeps the latest
     result of the current one. *)
  let highlighters = ref 0 and language = ref "a" in
  let producer () = (!highlighters, !language) in
  let latest = ref None in
  (* A starter's exception is a highlighting failure, so violations are
     collected and checked after each step. *)
  let violations = ref [] in
  let fail what = violations := what :: !violations in
  let check_request (request : Words.state Code.Highlighter.request) =
    let equal ~msg _ a b = if a <> b then fail msg in
    let is_true ~msg b = if not b then fail msg in
    match (request.previous, !latest) with
    | None, None -> ()
    | Some previous, Some id ->
        equal ~msg:"state of the latest result" int id previous.state.id;
        equal ~msg:"content of that result" string
          (Hashtbl.find content_of id)
          previous.content;
        let { Code.Highlighter.start; removed; inserted } = previous.edit in
        let old = previous.content and content = request.content in
        equal ~msg:"edit turns the previous content into the request's" string
          content
          (String.sub old 0 start
          ^ String.sub content start inserted
          ^ String.sub old (start + removed)
              (String.length old - start - removed));
        is_true ~msg:"the common prefix is longest"
          (removed = 0 || inserted = 0 || old.[start] <> content.[start]);
        is_true ~msg:"the common suffix is longest"
          (removed = 0 || inserted = 0
          || old.[start + removed - 1] <> content.[start + inserted - 1])
    | None, Some _ -> fail "a request lost the latest result"
    | Some _, None -> fail "a request carried a result of another producer"
  in
  let make_highlighter () =
    Code.Highlighter.async (fun request ~notify ->
        check_request request;
        let outcome = ref None in
        Queue.add (request, producer (), outcome, notify) jobs;
        Code.Highlighter.job
          ~poll:(fun () ->
            let result = !outcome in
            outcome := None;
            result)
          ~cancel:ignore)
  in
  let highlighter = ref (make_highlighter ()) in
  let syntax () =
    Code.with_highlighter ~language:!language ~style:words_style !highlighter
  in
  let reference_highlighter =
    Code.Highlighter.sync_incremental (fun { content; _ } ->
        (Words.fresh content, ()))
  in
  let initial = "let x be" in
  let _ctx, code = make_code ~content:initial ~syntax:(syntax ()) () in
  let _ctx, reference =
    make_code ~content:initial
      ~syntax:
        (Code.with_highlighter ~language:"a" ~style:words_style
           reference_highlighter)
      ()
  in
  let styles code =
    let buffer = Code.buffer code in
    List.init (Text_buffer.grapheme_count buffer) (fun g ->
        fst (Text_buffer.style_at buffer g))
  in
  let shown_ok = ref false in
  let content = ref initial in
  for step = 1 to 3_000 do
    let msg what = Printf.sprintf "step %d: %s" step what in
    if !violations <> [] then failf "step %d: %s" step (List.hd !violations);
    match Random.State.int random 10 with
    | 0 | 1 | 2 | 3 ->
        let n = String.length !content in
        let at = Random.State.int random (n + 1) in
        let removed = min (n - at) (Random.State.int random 3) in
        let insert =
          [| ""; "x"; "let"; " "; "y"; "le" |].(Random.State.int random 6)
        in
        let next =
          String.sub !content 0 at ^ insert
          ^ String.sub !content (at + removed) (n - at - removed)
        in
        if next <> "" && next <> !content then begin
          let settled = (not (Code.is_highlighting code)) && !shown_ok in
          let before = styles code and source = !content in
          content := next;
          Code.set_content code next;
          if settled then begin
            let { Code.Highlighter.start; removed; inserted } =
              (* The test's own derivation of the one edit. *)
              let p = ref 0
              and m = min (String.length source) (String.length next) in
              while !p < m && source.[!p] = next.[!p] do
                incr p
              done;
              let q = ref 0 in
              while
                !q < m - !p
                && source.[String.length source - 1 - !q]
                   = next.[String.length next - 1 - !q]
              do
                incr q
              done;
              {
                Code.Highlighter.start = !p;
                removed = String.length source - !p - !q;
                inserted = String.length next - !p - !q;
              }
            in
            let after = Array.of_list (styles code)
            and before = Array.of_list before in
            Array.iteri
              (fun i style ->
                let expected =
                  if i < start then before.(i)
                  else if i >= start + inserted then
                    before.(i - inserted + removed)
                  else Ansi.Style.default
                in
                if not (Ansi.Style.equal expected style) then
                  failf "step %d: pending byte %d of %S lost its style" step i
                    next)
              after
          end
        end
    | 4 | 5 | 6 | 7 -> (
        match Queue.take_opt jobs with
        | None -> ()
        | Some
            ( (request : Words.state Code.Highlighter.request),
              job_producer,
              outcome,
              notify ) ->
            let failed = Random.State.int random 5 = 0 in
            let current = job_producer = producer () in
            if failed then outcome := Some (Error (Failure "toy"))
            else begin
              let words =
                match request.previous with
                | None ->
                    Words.lex request.content ~first:0
                      ~last:(String.length request.content)
                | Some previous -> Words.relex request.content previous
              in
              incr ids;
              Hashtbl.replace content_of !ids request.content;
              outcome :=
                Some
                  (Ok
                     ( Words.captures request.content words,
                       { Words.id = !ids; words } ));
              if current then latest := Some !ids
            end;
            if current && String.equal request.content !content then
              shown_ok := not failed;
            notify ();
            render_code code;
            if not (Code.is_highlighting code) then begin
              Code.set_content reference !content;
              let shown = styles code in
              let expected =
                if !shown_ok then styles reference
                else List.map (fun _ -> Ansi.Style.default) shown
              in
              is_true
                ~msg:(msg "settled view equals a whole highlight")
                (List.equal Ansi.Style.equal expected shown)
            end)
    | 8 ->
        language := if !language = "a" then "b" else "a";
        latest := None;
        shown_ok := false;
        Code.set_syntax code (Some (syntax ()))
    | _ ->
        highlighter := make_highlighter ();
        incr highlighters;
        latest := None;
        shown_ok := false;
        Code.set_syntax code (Some (syntax ()))
  done;
  if !violations <> [] then failf "%s" (List.hd !violations)

(* ── Both doors ── *)

(* Each label, given to the element ({!Mosaic.code}) or to {!Code.create},
   reaches the live code ({!Test_harness.reaches}). *)

let doors_size = Mosaic.size ~width:8 ~height:3
let doors_style = Toffee.Style.make ~size:doors_size ()
let doors_bold = Ansi.Style.make ~bold:true ()
let let_x_syntax = Code.syntax ~style:(syntax_style red) highlights

let concealing =
  let meta = { Syntax_highlight.default_meta with conceal = Some "*" } in
  Code.syntax ~style:(syntax_style red)
    (Syntax_highlight.make ~content:"abcd e"
       [
         Syntax_highlight.range ~meta ~start_byte:0 ~end_byte:4 ~scope:"keyword"
           ();
       ])

let selected renderer _ =
  Option.value ~default:"(none)" (Renderer.selection_text renderer)

let doors_content () =
  Test_harness.reaches "content"
    ~base:(Mosaic.code ~size:doors_size "abc")
    ~element:(Mosaic.code ~size:doors_size "xyz")
    ~create_base:(fun parent ->
      Code.node (Code.create ~parent ~style:doors_style ~content:"abc" ()))
    ~create:(fun parent ->
      Code.node (Code.create ~parent ~style:doors_style ~content:"xyz" ()))

let doors_syntax () =
  Test_harness.reaches "syntax"
    ~base:(Mosaic.code ~size:doors_size "let x")
    ~element:(Mosaic.code ~size:doors_size ~syntax:let_x_syntax "let x")
    ~create_base:(fun parent ->
      Code.node (Code.create ~parent ~style:doors_style ~content:"let x" ()))
    ~create:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"let x"
           ~syntax:let_x_syntax ()))

let doors_conceal () =
  Test_harness.reaches "conceal"
    ~base:(Mosaic.code ~size:doors_size ~syntax:concealing "abcd e")
    ~element:
      (Mosaic.code ~size:doors_size ~syntax:concealing ~conceal:false "abcd e")
    ~create_base:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"abcd e"
           ~syntax:concealing ()))
    ~create:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"abcd e"
           ~syntax:concealing ~conceal:false ()))

let doors_text_style () =
  Test_harness.reaches "text_style"
    ~base:(Mosaic.code ~size:doors_size "hello world")
    ~element:(Mosaic.code ~size:doors_size ~text_style:doors_bold "hello world")
    ~create_base:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"hello world" ()))
    ~create:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"hello world"
           ~text_style:doors_bold ()))

let doors_wrap () =
  Test_harness.reaches "wrap"
    ~base:(Mosaic.code ~size:doors_size "hello world")
    ~element:(Mosaic.code ~size:doors_size ~wrap:`Word "hello world")
    ~create_base:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"hello world" ()))
    ~create:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"hello world"
           ~wrap:`Word ()))

let doors_tab_width () =
  Test_harness.reaches "tab_width"
    ~base:(Mosaic.code ~size:doors_size "a\tb")
    ~element:(Mosaic.code ~size:doors_size ~tab_width:2 "a\tb")
    ~create_base:(fun parent ->
      Code.node (Code.create ~parent ~style:doors_style ~content:"a\tb" ()))
    ~create:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"a\tb" ~tab_width:2 ()))

let doors_truncate () =
  Test_harness.reaches "truncate"
    ~base:(Mosaic.code ~size:doors_size "hello world")
    ~element:(Mosaic.code ~size:doors_size ~truncate:true "hello world")
    ~create_base:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"hello world" ()))
    ~create:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"hello world"
           ~truncate:true ()))

let doors_selectable () =
  Test_harness.reaches ~act:Test_harness.select_text ~observe:selected
    "selectable"
    ~base:(Mosaic.code ~size:doors_size "hello world")
    ~element:(Mosaic.code ~size:doors_size ~selectable:false "hello world")
    ~create_base:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"hello world" ()))
    ~create:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"hello world"
           ~selectable:false ()))

let doors_selection_bg () =
  Test_harness.reaches ~act:Test_harness.select_text "selection_bg"
    ~base:(Mosaic.code ~size:doors_size "hello world")
    ~element:
      (Mosaic.code ~size:doors_size ~selection_bg:Ansi.Color.red "hello world")
    ~create_base:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"hello world" ()))
    ~create:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"hello world"
           ~selection_bg:Ansi.Color.red ()))

let doors_selection_fg () =
  Test_harness.reaches ~act:Test_harness.select_text "selection_fg"
    ~base:(Mosaic.code ~size:doors_size "hello world")
    ~element:
      (Mosaic.code ~size:doors_size ~selection_fg:Ansi.Color.red "hello world")
    ~create_base:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"hello world" ()))
    ~create:(fun parent ->
      Code.node
        (Code.create ~parent ~style:doors_style ~content:"hello world"
           ~selection_fg:Ansi.Color.red ()))

let () =
  run "mosaic.code"
    [
      group "Syntax"
        [
          test "equivalent captures do not repaint"
            equivalent_capture_declaration_does_not_repaint;
          test "theme changes reuse captures"
            theme_changes_reuse_highlighter_captures;
          test "pending result uses latest theme"
            pending_result_uses_latest_theme;
          test "stale async capture falls back" stale_async_capture_falls_back;
          test "plain without syntax" plain_without_syntax;
          test "props reject a tab width below 1" props_reject_tab_width_below_1;
          test "precomputed highlights apply style"
            precomputed_highlights_apply_style;
          test "sync highlighter applies style" sync_highlighter_applies_style;
          test "async highlighter applies completed result"
            async_highlighter_applies_completed_result;
          test "pending append keeps highlighted prefix"
            pending_append_keeps_highlighted_prefix;
          test "pending edit keeps shared prefix and suffix"
            pending_edit_keeps_shared_prefix_and_suffix;
          test "pending edit inside a range keeps its other bytes"
            pending_edit_inside_a_range_keeps_its_other_bytes;
          test "pending edit drops a touched concealing range"
            pending_edit_drops_a_touched_concealing_range;
          test "stale result styles shared bytes"
            stale_result_styles_shared_bytes;
          test "new producer starts plain" new_producer_starts_plain;
          test "async content changes coalesce" async_content_changes_coalesce;
          test "async configuration changes coalesce"
            async_configuration_changes_coalesce;
          test "async clear discards pending request"
            async_clear_discards_pending_request;
          test "async line info stabilizes on completion"
            async_rows_stabilize_on_completion;
          test "async starter exception falls back to plain text"
            async_starter_exception_falls_back_to_plain_text;
          test "async poll exception falls back to plain text"
            async_poll_exception_falls_back_to_plain_text;
          test "invalid highlights fall back to plain text"
            invalid_highlights_fall_back_to_plain_text;
          test "conceal metadata applies" conceal_metadata_applies;
          test "conceal is a view property" conceal_is_a_view_property;
        ];
      group "Incremental"
        [
          test "previous carries the state and the edit"
            previous_carries_state_and_edit;
          test "requests hold on random scripts"
            incremental_requests_hold_on_random_scripts;
        ];
      group "Workers"
        [
          test "bounded queue cancels obsolete work"
            threaded_requests_are_bounded_and_cancelled;
          test "failure and idle restart" threaded_failure_and_idle_restart;
          test "destroy cancels once" destroy_cancels_job_once;
        ];
      group "Updates"
        [
          test "set_content reapplies syntax" set_content_reapplies_syntax;
          test "set_syntax None renders plain" set_syntax_none_renders_plain;
          test "apply_props updates syntax style"
            apply_props_updates_syntax_style;
          test "syntax compares styles by value" syntax_compares_styles_by_value;
          test "text_style restamps plain content"
            text_style_restamps_plain_content;
          test "is_highlighting false for sync highlights"
            is_highlighting_false_for_sync_highlights;
        ];
      group "Both doors"
        [
          test "content" doors_content;
          test "syntax" doors_syntax;
          test "conceal" doors_conceal;
          test "text_style" doors_text_style;
          test "wrap" doors_wrap;
          test "tab_width" doors_tab_width;
          test "truncate" doors_truncate;
          test "selectable" doors_selectable;
          test "selection_bg" doors_selection_bg;
          test "selection_fg" doors_selection_fg;
        ];
    ]
