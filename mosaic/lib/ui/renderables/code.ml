(* Highlighters *)

module Highlighter = struct
  type request = { content : string; language : string }
  type result = (Syntax_highlight.t, exn) Stdlib.result
  type job = { poll : unit -> result option; cancel : unit -> unit }

  type t =
    | Sync of (request -> Syntax_highlight.t)
    | Async of (request -> notify:(unit -> unit) -> job)

  let job ~poll ~cancel = { poll; cancel }
  let sync f = Sync f
  let async f = Async f

  let failed_job exn =
    let pending = ref (Some (Error exn)) in
    {
      poll =
        (fun () ->
          match !pending with
          | None -> None
          | Some outcome ->
              pending := None;
              Some outcome);
      cancel = (fun () -> pending := None);
    }

  let start t request ~notify =
    match t with
    | Async f -> ( try f request ~notify with exn -> failed_job exn)
    | Sync f ->
        let outcome = try Ok (f request) with exn -> Error exn in
        let pending = ref (Some outcome) in
        {
          poll =
            (fun () ->
              match !pending with
              | None -> None
              | Some outcome ->
                  pending := None;
                  Some outcome);
          cancel = (fun () -> pending := None);
        }
end

(* Syntax *)

type source = Ranges of Syntax_highlight.t | Highlighter of Highlighter.t

type syntax = {
  language : string option;
  style : Syntax_style.t;
  source : source;
  conceal : bool;
  draw_unstyled : bool;
  streaming : bool;
}

let syntax ?language ?(style = Syntax_style.default) ?(conceal = true)
    highlights =
  {
    language;
    style;
    source = Ranges highlights;
    conceal;
    draw_unstyled = true;
    streaming = false;
  }

let with_highlighter ~language ?(style = Syntax_style.default) ?(conceal = true)
    ?(draw_unstyled = true) ?(streaming = false) highlighter =
  {
    language = Some language;
    style;
    source = Highlighter highlighter;
    conceal;
    draw_unstyled;
    streaming;
  }

let source_equal a b =
  match (a, b) with
  | Ranges a, Ranges b -> a = b
  | Highlighter a, Highlighter b -> a == b
  | _ -> false

let syntax_equal a b =
  Option.equal String.equal a.language b.language
  && a.style == b.style
  && source_equal a.source b.source
  && Bool.equal a.conceal b.conceal
  && Bool.equal a.draw_unstyled b.draw_unstyled
  && Bool.equal a.streaming b.streaming

(* Props *)

module Props = struct
  type t = {
    content : string;
    syntax : syntax option;
    text_style : Ansi.Style.t;
    wrap : Text_surface.wrap;
    tab_width : int;
    truncate : bool;
    selectable : bool;
    selection_bg : Ansi.Color.t option;
    selection_fg : Ansi.Color.t option;
  }

  let make ?(content = "") ?syntax ?(text_style = Ansi.Style.default)
      ?(wrap = `None) ?(tab_width = 4) ?(truncate = false) ?(selectable = true)
      ?selection_bg ?selection_fg () =
    {
      content;
      syntax;
      text_style;
      wrap;
      tab_width;
      truncate;
      selectable;
      selection_bg;
      selection_fg;
    }

  let default = make ()

  let equal a b =
    String.equal a.content b.content
    && Option.equal syntax_equal a.syntax b.syntax
    && Ansi.Style.equal a.text_style b.text_style
    && a.wrap = b.wrap && a.tab_width = b.tab_width && a.truncate = b.truncate
    && a.selectable = b.selectable
    && Option.equal Ansi.Color.equal a.selection_bg b.selection_bg
    && Option.equal Ansi.Color.equal a.selection_fg b.selection_fg
end

(* Types *)

type t = {
  text : Text_renderable.t;
  mutable props : Props.t;
  mutable generation : int;
  mutable highlighting : bool;
  mutable job : running_job option;
  mutable had_initial_content : bool;
  mutable visible_final : bool;
  mutable on_line_info_change : (unit -> unit) option;
}

and running_job = {
  generation : int;
  content : string;
  syntax : syntax;
  job : Highlighter.job;
}

(* Accessors *)

let node t = Text_renderable.node t.text
let buffer t = Text_renderable.buffer t.text
let surface t = Text_renderable.surface t.text
let set_on_selection t h = Text_renderable.set_on_selection t.text h
let set_on_line_info_change t h = t.on_line_info_change <- h

let notify_line_info_change t =
  Option.iter (fun f -> f ()) t.on_line_info_change

let pending_work t =
  if t.highlighting then
    let label =
      match t.job with
      | Some { syntax = { language = Some language; _ }; _ } -> Some language
      | _ -> None
    in
    Some (Renderable.Pending.make ?label ~kind:"code.highlight" ())
  else None

(* Rendering *)

let request_render t = Renderable.request_render (Text_renderable.node t.text)

let cancel_job t =
  match t.job with
  | None -> ()
  | Some running ->
      (try running.job.cancel () with _ -> ());
      t.job <- None;
      t.highlighting <- false

let apply_plain ?(final = true) t content =
  Text_renderable.set_text t.text content;
  t.visible_final <- final

let apply_empty ?(final = true) t = apply_plain ~final t ""

let apply_highlights t content syntax highlights =
  try
    let spans =
      Syntax_highlight.to_spans ~conceal:syntax.conceal ~style:syntax.style
        ~content highlights
    in
    Text_renderable.set_styled_text t.text spans;
    t.visible_final <- true;
    true
  with _ ->
    apply_plain t content;
    false

let finish_job t (running : running_job) outcome =
  if running.generation = t.generation then begin
    t.job <- None;
    t.highlighting <- false;
    (match outcome with
    | Ok highlights ->
        ignore
          (apply_highlights t running.content running.syntax highlights : bool)
    | Error _ -> apply_plain t running.content);
    notify_line_info_change t;
    request_render t
  end

let poll_job t =
  match t.job with
  | None -> ()
  | Some running -> (
      match try running.job.poll () with exn -> Some (Error exn) with
      | None -> ()
      | Some outcome -> finish_job t running outcome)

let visible_before_highlight t content syntax =
  t.visible_final <- false;
  if syntax.streaming && t.had_initial_content then begin
    ()
  end
  else if syntax.draw_unstyled then begin
    Text_renderable.set_text t.text content
  end
  else begin
    Text_renderable.set_text t.text content;
    Text_renderable.set_render_enabled t.text false
  end

let start_highlighting t content syntax highlighter =
  cancel_job t;
  let language = Option.value syntax.language ~default:"" in
  visible_before_highlight t content syntax;
  let generation = t.generation in
  let request = { Highlighter.content; language } in
  let job =
    Highlighter.start highlighter request ~notify:(fun () -> request_render t)
  in
  t.job <- Some { generation; content; syntax; job };
  t.highlighting <- true;
  t.had_initial_content <- true;
  poll_job t

let render_syntax t content syntax =
  match syntax.source with
  | Ranges highlights ->
      cancel_job t;
      t.highlighting <- false;
      t.had_initial_content <- not (String.equal content "");
      ignore (apply_highlights t content syntax highlights : bool);
      notify_line_info_change t
  | Highlighter highlighter -> start_highlighting t content syntax highlighter

let render_content t (props : Props.t) =
  t.generation <- t.generation + 1;
  if String.equal props.content "" then begin
    cancel_job t;
    t.had_initial_content <- false;
    apply_empty t;
    notify_line_info_change t
  end
  else
    match props.syntax with
    | Some syntax -> render_syntax t props.content syntax
    | None ->
        cancel_job t;
        t.had_initial_content <- false;
        apply_plain t props.content;
        notify_line_info_change t

let render_before t _node _grid ~delta:_ = poll_job t

(* Construction *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?content ?syntax
    ?text_style ?wrap ?tab_width ?truncate ?selectable ?selection_bg
    ?selection_fg ?on_selection () =
  let props =
    Props.make ?content ?syntax ?text_style ?wrap ?tab_width ?truncate
      ?selectable ?selection_bg ?selection_fg ()
  in
  let text =
    Text_renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity
      ~text_style:props.text_style ~wrap:props.wrap ~tab_width:props.tab_width
      ~truncate:props.truncate ~selectable:props.selectable
      ?selection_bg:props.selection_bg ?selection_fg:props.selection_fg
      ?on_selection ()
  in
  let t =
    {
      text;
      props;
      generation = 0;
      highlighting = false;
      job = None;
      had_initial_content = false;
      visible_final = true;
      on_line_info_change = None;
    }
  in
  Renderable.set_render_before
    (Text_renderable.node text)
    (Some (render_before t));
  Renderable.set_pending_provider
    (Text_renderable.node text)
    (Some (fun () -> pending_work t));
  render_content t props;
  t

(* Content *)

let set_content t s =
  let props = { t.props with content = s } in
  render_content t props;
  t.props <- props

let set_syntax t syntax =
  let props = { t.props with syntax } in
  render_content t props;
  t.props <- props

(* Apply Props *)

let apply_props t (props : Props.t) =
  let style_changed =
    not (Ansi.Style.equal t.props.text_style props.text_style)
  in
  let metrics_changed = ref false in
  if style_changed then Text_renderable.set_text_style t.text props.text_style;
  if
    (not (String.equal t.props.content props.content))
    || (not (Option.equal syntax_equal t.props.syntax props.syntax))
    || style_changed
  then render_content t props;
  (* Wrap mode *)
  if t.props.wrap <> props.wrap then begin
    Text_renderable.set_wrap t.text props.wrap;
    metrics_changed := true
  end;
  (* Tab width *)
  if t.props.tab_width <> props.tab_width then begin
    Text_renderable.set_tab_width t.text props.tab_width;
    metrics_changed := true
  end;
  (* Truncate *)
  if t.props.truncate <> props.truncate then begin
    Text_renderable.set_truncate t.text props.truncate;
    metrics_changed := true
  end;
  (* Selection colors *)
  if not (Option.equal Ansi.Color.equal t.props.selection_bg props.selection_bg)
  then Text_renderable.set_selection_bg t.text props.selection_bg;
  if not (Option.equal Ansi.Color.equal t.props.selection_fg props.selection_fg)
  then Text_renderable.set_selection_fg t.text props.selection_fg;
  (* Selectable *)
  if t.props.selectable <> props.selectable then
    Text_renderable.set_selectable t.text props.selectable;
  t.props <- props;
  if !metrics_changed then notify_line_info_change t

(* Query *)

let is_highlighting t = t.highlighting
let line_info_stable t = (not t.highlighting) && t.visible_final
let line_count t = Text_renderable.line_count t.text
let display_line_count t = Text_renderable.display_line_count t.text
