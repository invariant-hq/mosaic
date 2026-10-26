module Source = Matrix.Text.Layout.Source

(* ───── Spans ───── *)

type span = { text : string; style : Ansi.Style.t }

module Annotations = struct
  type range = { start_byte : int; end_byte : int; style : Ansi.Style.t }
  type t = { content : string; runs : range array }

  let content t = t.content

  (* [add_run runs first last style] appends a run, extending the last one
     when it has the same style. *)
  let add_run runs first last style =
    if first < last then begin
      let n = Dynarray.length runs in
      if n > 0 && Ansi.Style.equal (Dynarray.get runs (n - 1)).style style then
        Dynarray.set runs (n - 1)
          { (Dynarray.get runs (n - 1)) with end_byte = last }
      else Dynarray.add_last runs { start_byte = first; end_byte = last; style }
    end

  let make ?(base_style = Ansi.Style.default) ~content ranges =
    let length = String.length content in
    let ordered = ref true and previous_end = ref 0 in
    List.iter
      (fun range ->
        if
          range.start_byte < 0
          || range.end_byte < range.start_byte
          || range.end_byte > length
        then
          invalid_arg "Text_buffer.Annotations.make: byte range out of bounds";
        if range.start_byte < range.end_byte then begin
          if range.start_byte < !previous_end then ordered := false;
          previous_end := range.end_byte
        end)
      ranges;
    let runs = Dynarray.create () in
    let emit = add_run runs in
    if !ordered then begin
      let current = ref 0 in
      let default_base = Ansi.Style.equal base_style Ansi.Style.default in
      List.iter
        (fun range ->
          if range.start_byte < range.end_byte then begin
            emit !current range.start_byte base_style;
            let style =
              if default_base then range.style
              else Ansi.Style.merge ~base:base_style ~overlay:range.style
            in
            emit range.start_byte range.end_byte style;
            current := range.end_byte
          end)
        ranges;
      emit !current length base_style
    end
    else begin
      let events = ref [] in
      List.iteri
        (fun id range ->
          if range.start_byte < range.end_byte then begin
            events := (range.start_byte, `Start (id, range.style)) :: !events;
            events := (range.end_byte, `Stop id) :: !events
          end)
        ranges;
      let events = List.sort (fun (a, _) (b, _) -> Int.compare a b) !events in
      let active = ref [] in
      let rec sweep pos events =
        let rec apply = function
          | (at, event) :: rest when at = pos ->
              (match event with
              | `Start entry ->
                  active :=
                    List.merge
                      (fun (a, _) (b, _) -> Int.compare a b)
                      [ entry ] !active
              | `Stop id ->
                  active := List.filter (fun (key, _) -> key <> id) !active);
              apply rest
          | rest -> rest
        in
        let rest = apply events in
        let last = match rest with (at, _) :: _ -> at | [] -> length in
        let style =
          List.fold_left
            (fun base (_, overlay) -> Ansi.Style.merge ~base ~overlay)
            base_style !active
        in
        emit pos last style;
        if last < length then sweep last rest
      in
      if length > 0 then sweep 0 events
    end;
    { content; runs = Dynarray.to_array runs }

  (* Both run arrays cover the content from its first byte to its last, so a
     walk over both meets every boundary of either. *)
  let merge ~base ~overlay =
    if
      base.content != overlay.content
      && not (String.equal base.content overlay.content)
    then invalid_arg "Text_buffer.Annotations.merge: sources differ";
    let runs = Dynarray.create () in
    let i = ref 0 and j = ref 0 and first = ref 0 in
    while !i < Array.length base.runs && !j < Array.length overlay.runs do
      let b = base.runs.(!i) and o = overlay.runs.(!j) in
      let last = min b.end_byte o.end_byte in
      add_run runs !first last (Ansi.Style.merge ~base:b.style ~overlay:o.style);
      first := last;
      if b.end_byte = last then incr i;
      if o.end_byte = last then incr j
    done;
    { content = base.content; runs = Dynarray.to_array runs }

  let to_spans t =
    let content = t.content in
    Array.to_list
      (Array.map
         (fun range ->
           {
             text =
               String.sub content range.start_byte
                 (range.end_byte - range.start_byte);
             style = range.style;
           })
         t.runs)
end

(* ───── Highlights ───── *)

module Highlight = struct
  type t = {
    start_offset : int;
    end_offset : int;
    style : Ansi.Style.t;
    priority : int;
    ref_id : int;
  }

  let make ~start_offset ~end_offset ~style ?(priority = 0) ~ref_id () =
    { start_offset; end_offset; style; priority; ref_id }

  let start_offset h = h.start_offset
  let end_offset h = h.end_offset
  let style h = h.style
  let priority h = h.priority
  let ref_id h = h.ref_id
end

(* ───── Buffer ───── *)

type content_mode = Forward | Reverse | Annotated of Annotations.t

type t = {
  mutable spans : span list;
  mutable content_mode : content_mode;
  mutable default_style : Ansi.Style.t;
  mutable highlights : Highlight.t list;
  mutable tab_width : int;
  mutable cached_plain_text : string option;
  mutable cached_max_line_width : (Matrix.Text.width_method * int) option;
  mutable source : Source.t option;
  mutable paint : (int * int * Ansi.Style.t) array option;
  mutable version : int;
}

let check_tab_width name w =
  if w < 1 then invalid_arg ("Text_buffer." ^ name ^ ": tab width below 1")

let create ?(default_style = Ansi.Style.default) ?(tab_width = 2) () =
  check_tab_width "create" tab_width;
  {
    spans = [];
    content_mode = Forward;
    default_style;
    highlights = [];
    tab_width;
    cached_plain_text = None;
    cached_max_line_width = None;
    source = None;
    paint = None;
    version = 0;
  }

(* ───── Invalidation ───── *)

let invalidate t =
  t.cached_plain_text <- None;
  t.cached_max_line_width <- None;
  t.source <- None;
  t.paint <- None;
  t.version <- t.version + 1

(* ───── Span Order ───── *)

let ensure_span_order t =
  match t.content_mode with
  | Reverse ->
      t.spans <- List.rev t.spans;
      t.content_mode <- Forward
  | Forward | Annotated _ -> ()

let ensure_spans_rev t =
  match t.content_mode with
  | Reverse -> ()
  | Forward ->
      t.spans <- List.rev t.spans;
      t.content_mode <- Reverse
  | Annotated annotations ->
      t.spans <- List.rev (Annotations.to_spans annotations);
      t.content_mode <- Reverse

(* ───── Content ───── *)

let set_text t s =
  let unchanged =
    match t.cached_plain_text with
    | Some text -> String.equal text s
    | None -> false
  in
  t.spans <- [ { text = s; style = t.default_style } ];
  t.content_mode <- Forward;
  if unchanged then t.paint <- None else invalidate t;
  t.cached_plain_text <- Some s

let styled_text t =
  match t.content_mode with
  | Annotated annotations -> Annotations.to_spans annotations
  | Forward | Reverse ->
      ensure_span_order t;
      t.spans

let set_styled_text t spans =
  let unchanged =
    match t.cached_plain_text with
    | None -> false
    | Some text ->
        let length = String.length text in
        let rec matches offset = function
          | [] -> offset = length
          | span :: rest ->
              let size = String.length span.text in
              if size > length - offset then false
              else
                let i = ref 0 in
                while
                  !i < size
                  && String.unsafe_get text (offset + !i)
                     = String.unsafe_get span.text !i
                do
                  incr i
                done;
                !i = size && matches (offset + size) rest
        in
        matches 0 spans
  in
  t.spans <- spans;
  t.content_mode <- Forward;
  if unchanged then t.paint <- None else invalidate t

let append t s =
  ensure_spans_rev t;
  t.spans <- { text = s; style = t.default_style } :: t.spans;
  invalidate t

let append_styled t new_spans =
  ensure_spans_rev t;
  t.spans <- List.rev_append new_spans t.spans;
  invalidate t

let clear t =
  t.spans <- [];
  t.content_mode <- Forward;
  t.highlights <- [];
  invalidate t

let plain_text t =
  match t.cached_plain_text with
  | Some s -> s
  | None ->
      ensure_span_order t;
      let s =
        match t.spans with
        | [] -> ""
        | [ s ] -> s.text
        | spans ->
            let buf = Buffer.create 256 in
            List.iter (fun s -> Buffer.add_string buf s.text) spans;
            Buffer.contents buf
      in
      t.cached_plain_text <- Some s;
      s

let source t =
  match t.source with
  | Some source -> source
  | None ->
      let source = Source.of_string (plain_text t) in
      t.source <- Some source;
      source

let set_source t source =
  match t.source with
  | Some current when current == source -> ()
  | _ ->
      if not (String.equal (plain_text t) (Source.text source)) then
        set_text t (Source.text source);
      t.source <- Some source;
      t.version <- t.version + 1

let set_annotations t annotations =
  let content = Annotations.content annotations in
  let current = plain_text t in
  if current != content && not (String.equal current content) then
    invalid_arg "Text_buffer.set_annotations: source does not match buffer";
  match t.content_mode with
  | Annotated current when current == annotations -> ()
  | Forward | Reverse | Annotated _ ->
      t.content_mode <- Annotated annotations;
      t.spans <- [];
      t.cached_plain_text <- Some content;
      t.paint <- None

let grapheme_count t = Source.length (source t)

(* ───── Default Style ───── *)

let default_style t = t.default_style
let set_default_style t s = t.default_style <- s

(* ───── Line Info ───── *)

let line_count t = Source.line_count (source t)

let line_width t ~width_method line =
  let src = source t in
  if line < 0 || line >= Source.line_count src then 0
  else
    Source.columns ~width_method ~tab_width:t.tab_width src
      ~first:(Source.line_start src line)
      ~last:(Source.line_end src line)

(* A text of one line measures it. A longer one asks for the layout without
   wrapping that its source keeps, which an edit carries, so an editor's
   widest line costs the lines the edit touched. *)
let max_line_width t ~width_method =
  match t.cached_max_line_width with
  | Some (cached, width) when cached = width_method -> width
  | Some _ | None ->
      let width =
        if line_count t = 1 then line_width t ~width_method 0
        else
          snd
            (Matrix.Text.Layout.measure (source t) ~width_method
               ~tab_width:t.tab_width ~width:0 ~wrap:`None)
      in
      t.cached_max_line_width <- Some (width_method, width);
      width

(* ───── Text In Range ───── *)

let text_in_range t ~start ~len =
  if len <= 0 then ""
  else Source.slice (source t) ~first:start ~last:(start + len)

(* ───── Highlights ───── *)

let add_highlight t h =
  t.paint <- None;
  t.highlights <- h :: t.highlights

let remove_highlights_by_ref t ref_id =
  t.paint <- None;
  t.highlights <-
    List.filter (fun h -> Highlight.ref_id h <> ref_id) t.highlights

let clear_highlights t =
  t.paint <- None;
  t.highlights <- []

let highlights_in_range t ~start ~len =
  let end_offset = start + len in
  t.highlights
  |> List.filter (fun h ->
      Highlight.start_offset h < end_offset && Highlight.end_offset h > start)
  |> List.sort (fun a b ->
      compare (Highlight.priority a) (Highlight.priority b))

(* ───── Tab Width ───── *)

let tab_width t = t.tab_width

let set_tab_width t w =
  check_tab_width "set_tab_width" w;
  if t.tab_width <> w then begin
    t.tab_width <- w;
    t.cached_max_line_width <- None;
    t.version <- t.version + 1
  end

(* ───── Versioning ───── *)

let version t = t.version

(* Sorting is O(E log E); partial-style resolution is O(E * A), where A is
   simultaneous overlays. This preparation never repeats on unchanged paint. *)
let paint_runs_with_overlays t =
  ensure_span_order t;
  let src = source t in
  let n = Source.length src in
  let events = ref [] and byte = ref 0 in
  (match t.content_mode with
  | Annotated annotations ->
      Array.iter
        (fun (range : Annotations.range) ->
          let g =
            Source.grapheme_of_byte src ~affinity:Downstream range.start_byte
          in
          if g < n then events := (g, `Base range.style) :: !events)
        annotations.runs
  | Forward | Reverse ->
      List.iter
        (fun (span : span) ->
          let g = Source.grapheme_of_byte src ~affinity:Downstream !byte in
          if g < n && String.length span.text > 0 then
            events := (g, `Base span.style) :: !events;
          byte := !byte + String.length span.text)
        t.spans);
  let highlights =
    List.stable_sort
      (fun a b -> compare (Highlight.priority a) (Highlight.priority b))
      t.highlights
  in
  List.iteri
    (fun id hl ->
      let first = max 0 hl.Highlight.start_offset in
      let last = min n hl.Highlight.end_offset in
      if first < last then begin
        events := (first, `Start (id, hl.Highlight.style)) :: !events;
        events := (last, `Stop id) :: !events
      end)
    highlights;
  let events =
    List.stable_sort (fun (a, _) (b, _) -> compare a b) (List.rev !events)
  in
  let active = ref [] and base = ref t.default_style in
  let runs = Dynarray.create () in
  let rec sweep pos events =
    let rec apply = function
      | (g, event) :: rest when g = pos ->
          (match event with
          | `Base style -> base := style
          | `Start entry ->
              active :=
                List.merge (fun (a, _) (b, _) -> compare a b) [ entry ] !active
          | `Stop id ->
              active := List.filter (fun (key, _) -> key <> id) !active);
          apply rest
      | rest -> rest
    in
    let rest = apply events in
    let last = match rest with (g, _) :: _ -> g | [] -> n in
    if last > pos then begin
      let style =
        List.fold_left
          (fun base (_, overlay) -> Ansi.Style.merge ~base ~overlay)
          !base !active
      in
      Dynarray.add_last runs (pos, last, style)
    end;
    if last < n then sweep last rest
  in
  if n > 0 then sweep 0 events;
  let runs = Dynarray.to_array runs in
  t.paint <- Some runs;
  runs

let paint_runs t =
  match t.paint with Some runs -> runs | None -> paint_runs_with_overlays t

(* Annotated paint without overlays is looked up where it is painted, so
   installing it costs nothing per run. A grapheme takes the style of the run
   holding its first byte: a boundary inside a grapheme resolves downstream. *)
let annotated_style_at t (annotations : Annotations.t) g =
  let runs = annotations.runs in
  if Array.length runs = 0 then (t.default_style, max_int)
  else begin
    let src = source t in
    let byte = Source.byte_offset src (max 0 (min g (Source.length src - 1))) in
    let lo = ref 0 and hi = ref (Array.length runs - 1) in
    while !lo < !hi do
      let mid = !lo + ((!hi - !lo + 1) / 2) in
      if runs.(mid).start_byte <= byte then lo := mid else hi := mid - 1
    done;
    let run = runs.(!lo) in
    (run.style, Source.grapheme_of_byte src ~affinity:Downstream run.end_byte)
  end

let style_at t g =
  match t.content_mode with
  | Annotated annotations when t.highlights = [] ->
      annotated_style_at t annotations g
  | Annotated _ | Forward | Reverse ->
      let runs = paint_runs t in
      let n = Array.length runs in
      if n = 0 then (t.default_style, max_int)
      else begin
        let lo = ref 0 and hi = ref (n - 1) in
        while !lo < !hi do
          let mid = !lo + ((!hi - !lo + 1) / 2) in
          let first, _, _ = runs.(mid) in
          if first <= g then lo := mid else hi := mid - 1
        done;
        let _, last, style = runs.(!lo) in
        (style, last)
      end
