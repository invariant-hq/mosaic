open Windtrap
open Mosaic_ui

let red = Ansi.Style.fg Ansi.Color.red Ansi.Style.default
let green = Ansi.Style.fg Ansi.Color.green Ansi.Style.default
let bold = Ansi.Style.with_bold true Ansi.Style.default

let style =
  Syntax_style.make ~base:Ansi.Style.default
    [ ("keyword", red); ("keyword.control", green); ("markup.raw.block", red) ]

let span_text (span : Text_buffer.span) = span.text
let span_style (span : Text_buffer.span) = span.style

let text_of_spans spans =
  let buf = Buffer.create 32 in
  List.iter (fun span -> Buffer.add_string buf (span_text span)) spans;
  Buffer.contents buf

let style_at spans index = span_style (List.nth spans index)

let of_triples_applies_style () =
  let spans =
    Syntax_highlight.of_triples ~content:"abcdef" [ (0, 3, "keyword") ]
    |> Syntax_highlight.to_spans ~style
  in
  equal ~msg:"span count" int 2 (List.length spans);
  equal ~msg:"highlighted text" string "abc" (span_text (List.nth spans 0));
  is_true ~msg:"highlighted style" (Ansi.Style.equal red (style_at spans 0));
  equal ~msg:"plain text" string "def" (span_text (List.nth spans 1));
  is_true ~msg:"plain style"
    (Ansi.Style.equal Ansi.Style.default (style_at spans 1))

let overlapping_same_scope_keeps_later_range_active () =
  let ranges =
    Syntax_highlight.of_triples ~content:"abcdef"
      [ (0, 4, "keyword"); (2, 6, "keyword") ]
  in
  let spans = Syntax_highlight.to_spans ~style ranges in
  equal ~msg:"text" string "abcdef" (text_of_spans spans);
  equal ~msg:"merged span count" int 1 (List.length spans);
  is_true ~msg:"style remains active through second range"
    (Ansi.Style.equal red (style_at spans 0))

let specificity_cascades_from_parent_to_child () =
  let style =
    Syntax_style.make ~base:Ansi.Style.default
      [ ("keyword", bold); ("keyword.control", green) ]
  in
  let spans =
    Syntax_highlight.of_triples ~content:"return"
      [ (0, 6, "keyword"); (0, 6, "keyword.control") ]
    |> Syntax_highlight.to_spans ~style
  in
  let expected = Ansi.Style.merge ~base:bold ~overlay:green in
  equal ~msg:"span count" int 1 (List.length spans);
  is_true ~msg:"merged style" (Ansi.Style.equal expected (style_at spans 0))

let conceal_replaces_range () =
  let meta = { Syntax_highlight.default_meta with conceal = Some "*" } in
  let ranges =
    [
      Syntax_highlight.range ~meta ~start_byte:0 ~end_byte:3 ~scope:"conceal" ();
    ]
  in
  let spans =
    Syntax_highlight.to_spans ~style
      (Syntax_highlight.make ~content:"abc def" ranges)
  in
  equal ~msg:"concealed text" string "* def" (text_of_spans spans)

let injection_container_suppresses_container_style () =
  let container_meta =
    { Syntax_highlight.default_meta with contains_injection = true }
  in
  let injected_meta =
    { Syntax_highlight.default_meta with is_injection = true }
  in
  let ranges =
    [
      Syntax_highlight.range ~meta:container_meta ~start_byte:0 ~end_byte:4
        ~scope:"markup.raw.block" ();
      Syntax_highlight.range ~meta:injected_meta ~start_byte:1 ~end_byte:3
        ~scope:"keyword.control" ();
    ]
  in
  let spans =
    Syntax_highlight.to_spans ~style
      (Syntax_highlight.make ~content:"xxxx" ranges)
  in
  equal ~msg:"text" string "xxxx" (text_of_spans spans);
  equal ~msg:"span count" int 3 (List.length spans);
  is_true ~msg:"container prefix is unstyled"
    (Ansi.Style.equal Ansi.Style.default (style_at spans 0));
  is_true ~msg:"injection is styled" (Ansi.Style.equal green (style_at spans 1));
  is_true ~msg:"container suffix is unstyled"
    (Ansi.Style.equal Ansi.Style.default (style_at spans 2))

let invalid_range_raises () =
  raises_match ~msg:"out of bounds"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () ->
      ignore
        (Syntax_highlight.of_triples ~content:"abc" [ (0, 10, "keyword") ]
        |> Syntax_highlight.to_spans ~style))

let vocabulary_fallback_and_extensions () =
  List.iter
    (fun scope ->
      let expected = Syntax_style.resolve Syntax_style.default scope in
      is_true
        ~msg:(scope ^ " dotted refinement inherits its category")
        (Ansi.Style.equal expected
           (Syntax_style.resolve Syntax_style.default (scope ^ ".example"))))
    [
      "comment";
      "string";
      "number";
      "constant";
      "keyword";
      "operator";
      "punctuation";
      "function";
      "type";
      "variable";
      "module";
      "attribute";
      "label";
      "embedded";
      "tag";
      "property";
      "constructor";
    ];
  List.iter
    (fun scope ->
      is_false
        ~msg:(scope ^ " is styled by the default theme")
        (Ansi.Style.equal Ansi.Style.default
           (Syntax_style.resolve_overlay Syntax_style.default scope)))
    [ "property"; "constructor" ];
  is_true ~msg:"variable.member is styled as a property"
    (Ansi.Style.equal
       (Syntax_style.resolve Syntax_style.default "property")
       (Syntax_style.resolve Syntax_style.default "variable.member"));
  let style = Syntax_style.make ~base:green [ ("application.custom", red) ] in
  is_true ~msg:"unknown scope keeps base style"
    (Ansi.Style.equal green (Syntax_style.resolve style "unknown.scope"));
  is_true ~msg:"applications may extend the vocabulary"
    (Ansi.Style.equal red
       (Syntax_style.resolve style "application.custom.child"))

let styles_compare_by_value () =
  let build () =
    Syntax_style.make ~base:green [ ("keyword", red); ("string", bold) ]
  in
  is_true ~msg:"separately built styles are equal"
    (Syntax_style.equal (build ()) (build ()));
  is_true ~msg:"mapping order is irrelevant"
    (Syntax_style.equal (build ())
       (Syntax_style.make ~base:green [ ("string", bold); ("keyword", red) ]));
  is_true ~msg:"the last mapping of a group wins"
    (Syntax_style.equal (build ())
       (Syntax_style.make ~base:green
          [ ("keyword", green); ("string", bold); ("keyword", red) ]));
  is_false ~msg:"overlays differ"
    (Syntax_style.equal (build ())
       (Syntax_style.make ~base:green [ ("keyword", bold); ("string", bold) ]));
  is_false ~msg:"groups differ"
    (Syntax_style.equal (build ())
       (Syntax_style.make ~base:green [ ("keyword", red) ]));
  is_false ~msg:"bases differ"
    (Syntax_style.equal (build ())
       (Syntax_style.make ~base:red [ ("keyword", red); ("string", bold) ]))

let annotations_are_source_bound_and_preserve_geometry () =
  let buffer = Text_buffer.create () in
  Text_buffer.set_text buffer "abcdef";
  let source = Text_buffer.source buffer in
  let version = Text_buffer.version buffer in
  let annotations =
    Text_buffer.Annotations.make ~content:"abcdef"
      [
        { start_byte = 0; end_byte = 4; style = red };
        { start_byte = 2; end_byte = 6; style = bold };
      ]
  in
  Text_buffer.set_annotations buffer annotations;
  is_true ~msg:"overlaps merge in declaration order"
    (Ansi.Style.equal
       (Ansi.Style.merge ~base:red ~overlay:bold)
       (fst (Text_buffer.style_at buffer 2)));
  is_true ~msg:"annotation install keeps index"
    (source == Text_buffer.source buffer);
  equal ~msg:"paint does not change geometry version" int version
    (Text_buffer.version buffer);
  let stale = Text_buffer.Annotations.make ~content:"ghijkl" [] in
  raises_match ~msg:"same-length stale source rejected"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () -> Text_buffer.set_annotations buffer stale);
  equal string "abcdef" (Text_buffer.plain_text buffer);
  is_true (Ansi.Style.equal red (fst (Text_buffer.style_at buffer 0)));
  let equal_content = String.init 6 (String.get "abcdef") in
  let equivalent = Text_buffer.Annotations.make ~content:equal_content [] in
  Text_buffer.set_annotations buffer equivalent;
  is_true ~msg:"equivalent source after remount is valid"
    (source == Text_buffer.source buffer);
  Text_buffer.set_annotations buffer annotations;
  Text_buffer.append buffer "!";
  equal string "abcdef!" (Text_buffer.plain_text buffer);
  is_true ~msg:"append retains installed styles"
    (Ansi.Style.equal red (fst (Text_buffer.style_at buffer 0)))

let annotation_boundaries_match_styled_graphemes () =
  let content = "e\204\129\240\159\153\130x\r\nz\255" in
  let length = String.length content in
  for first = 0 to length do
    for last = first to length do
      let annotated = Text_buffer.create ()
      and fragmented = Text_buffer.create () in
      Text_buffer.set_text annotated content;
      Text_buffer.set_annotations annotated
        (Text_buffer.Annotations.make ~content
           [ { start_byte = first; end_byte = last; style = red } ]);
      Text_buffer.set_styled_text fragmented
        [
          { text = String.sub content 0 first; style = Ansi.Style.default };
          { text = String.sub content first (last - first); style = red };
          {
            text = String.sub content last (length - last);
            style = Ansi.Style.default;
          };
        ];
      let count = Text_buffer.grapheme_count annotated in
      for grapheme = 0 to count - 1 do
        let style, next = Text_buffer.style_at annotated grapheme in
        is_true
          ~msg:
            "byte-interior and invalid UTF-8 boundaries preserve span semantics"
          (Ansi.Style.equal style
             (fst (Text_buffer.style_at fragmented grapheme)));
        is_true ~msg:"the next boundary lies ahead" (next > grapheme);
        for within = grapheme to min next count - 1 do
          is_true ~msg:"the style holds up to the next boundary"
            (Ansi.Style.equal style
               (fst (Text_buffer.style_at annotated within)))
        done
      done
    done
  done

let conceal_projection_is_reused_across_themes () =
  let meta = { Syntax_highlight.default_meta with conceal = Some "*" } in
  let captures =
    Syntax_highlight.make ~content:"abc def"
      [
        Syntax_highlight.range ~meta ~start_byte:0 ~end_byte:3 ~scope:"conceal"
          ();
      ]
  in
  let plain = Syntax_highlight.to_annotations ~style captures in
  equal ~msg:"editable annotations preserve hidden bytes" string "abc def"
    (Text_buffer.Annotations.content plain);
  let first = Syntax_highlight.to_annotations ~conceal:true ~style captures in
  let second =
    Syntax_highlight.to_annotations ~conceal:true
      ~style:(Syntax_style.make ~base:red [])
      captures
  in
  equal string "* def" (Text_buffer.Annotations.content first);
  is_true ~msg:"themes share projected bytes"
    (Text_buffer.Annotations.content first
    == Text_buffer.Annotations.content second);
  let buffer = Text_buffer.create () in
  Text_buffer.set_text buffer (Text_buffer.Annotations.content first);
  Text_buffer.set_annotations buffer first;
  let source = Text_buffer.source buffer in
  Text_buffer.set_annotations buffer second;
  is_true ~msg:"themes retain visible source index"
    (source == Text_buffer.source buffer)

let merge_puts_the_overlay_over_the_base () =
  let base =
    Text_buffer.Annotations.make ~content:"abcd"
      [
        {
          start_byte = 0;
          end_byte = 3;
          style = Ansi.Style.make ~bold:true ~fg:Ansi.Color.red ();
        };
      ]
  in
  let overlay =
    Text_buffer.Annotations.make ~content:"abcd"
      [
        {
          start_byte = 2;
          end_byte = 4;
          style = Ansi.Style.make ~fg:Ansi.Color.green ();
        };
      ]
  in
  let spans =
    Text_buffer.Annotations.to_spans
      (Text_buffer.Annotations.merge ~base ~overlay)
  in
  let at i =
    let rec find offset = function
      | (span : Text_buffer.span) :: rest ->
          if i < offset + String.length span.text then span.style
          else find (offset + String.length span.text) rest
      | [] -> fail "byte out of the spans"
    in
    find 0 spans
  in
  equal ~msg:"text" string "abcd" (text_of_spans spans);
  is_true ~msg:"the base where the overlay is default"
    (Ansi.Style.equal (Ansi.Style.make ~bold:true ~fg:Ansi.Color.red ()) (at 0));
  is_true ~msg:"the overlay wins its attributes, the base keeps the others"
    (Ansi.Style.equal
       (Ansi.Style.make ~bold:true ~fg:Ansi.Color.green ())
       (at 2));
  is_true ~msg:"the overlay alone"
    (Ansi.Style.equal (Ansi.Style.make ~fg:Ansi.Color.green ()) (at 3));
  raises_match ~msg:"other bytes"
    (function Invalid_argument _ -> true | _ -> false)
    (fun () ->
      Text_buffer.Annotations.merge ~base
        ~overlay:(Text_buffer.Annotations.make ~content:"abce" []))

let annotations_validate_bounds () =
  List.iter
    (fun (start_byte, end_byte) ->
      raises_match
        (function Invalid_argument _ -> true | _ -> false)
        (fun () ->
          ignore
            (Text_buffer.Annotations.make ~content:"abc"
               [ { start_byte; end_byte; style = red } ])))
    [ (-1, 1); (2, 1); (0, 4) ]

let shared_capture_batches_are_safe_across_domains () =
  let content = String.make 6000 'x' in
  let ranges = List.init 1000 (fun i -> (i * 6, (i * 6) + 3, "keyword")) in
  let captures =
    Array.init 8 (fun _ -> Syntax_highlight.of_triples ~content ranges)
  in
  let ready = Atomic.make false in
  let resolve theme =
    Array.map
      (fun captures ->
        let annotations =
          Syntax_highlight.to_annotations ~style:theme captures
        in
        let buffer = Text_buffer.create () in
        Text_buffer.set_text buffer content;
        Text_buffer.set_annotations buffer annotations;
        ( Text_buffer.Annotations.content annotations,
          fst (Text_buffer.style_at buffer 0) ))
      captures
  in
  let worker =
    Domain.spawn (fun () ->
        Atomic.set ready true;
        resolve
          (Syntax_style.make ~base:Ansi.Style.default [ ("keyword", green) ]))
  in
  while not (Atomic.get ready) do
    Domain.cpu_relax ()
  done;
  let local =
    match resolve style with
    | local -> local
    | exception exn ->
        ignore (Domain.join worker);
        raise exn
  in
  let remote = Domain.join worker in
  Array.iteri
    (fun i (text, paint) ->
      equal string content text;
      is_true (Ansi.Style.equal red paint);
      let other_text, other_paint = remote.(i) in
      is_true ~msg:"shared batch retains source" (text == other_text);
      is_true (Ansi.Style.equal green other_paint))
    local

let unordered_annotations_preserve_overlay_order () =
  let content = "abcdefg" in
  let base_style = Ansi.Style.make ~bold:true ~bg:Ansi.Color.blue () in
  let annotations =
    Text_buffer.Annotations.make ~content ~base_style
      [
        {
          start_byte = 2;
          end_byte = 6;
          style = Ansi.Style.make ~fg:Ansi.Color.red ~italic:true ();
        };
        {
          start_byte = 0;
          end_byte = 4;
          style = Ansi.Style.make ~fg:Ansi.Color.cyan ();
        };
        {
          start_byte = 3;
          end_byte = 3;
          style = Ansi.Style.make ~fg:Ansi.Color.magenta ();
        };
      ]
  in
  let buffer = Text_buffer.create () in
  Text_buffer.set_text buffer content;
  Text_buffer.set_annotations buffer annotations;
  let at grapheme = fst (Text_buffer.style_at buffer grapheme) in
  is_true ~msg:"later range overlays the non-default base"
    (Ansi.Style.equal
       (Ansi.Style.make ~bold:true ~bg:Ansi.Color.blue ~fg:Ansi.Color.cyan ())
       (at 0));
  let overlap =
    Ansi.Style.make ~bold:true ~italic:true ~bg:Ansi.Color.blue
      ~fg:Ansi.Color.cyan ()
  in
  is_true ~msg:"later color wins while earlier italic and base bold survive"
    (Ansi.Style.equal overlap (at 2));
  is_true ~msg:"empty range has no effect" (Ansi.Style.equal overlap (at 3));
  is_true ~msg:"earlier range remains after later range ends"
    (Ansi.Style.equal
       (Ansi.Style.make ~bold:true ~italic:true ~bg:Ansi.Color.blue
          ~fg:Ansi.Color.red ())
       (at 4));
  is_true ~msg:"uncovered suffix uses base style"
    (Ansi.Style.equal base_style (at 6))

(* The resolution that preceded the sorted sweep, kept as the reference: a
   boundary list sorted once, then per segment the open ranges sorted by
   specificity. *)
module Reference = struct
  module H = Syntax_highlight

  type boundary = Start of int * int | End of int * int

  let pos = function Start (pos, _) | End (pos, _) -> pos

  let specificity scope =
    let n = ref 1 in
    String.iter (fun c -> if c = '.' then incr n) scope;
    !n

  let replacement range =
    match (H.meta range).conceal with
    | Some text -> Some text
    | None ->
        let scope = H.scope range in
        if
          String.equal scope "conceal"
          || String.starts_with ~prefix:"conceal." scope
        then
          if String.equal scope "conceal.with.space" then Some " " else Some ""
        else None

  let spans ~conceal ~style captures =
    let content = H.content captures in
    let len = String.length content in
    let indexed =
      List.filter
        (fun (_, r) -> H.start_byte r <> H.end_byte r)
        (List.mapi (fun i r -> (i, r)) (H.ranges captures))
    in
    let by_index = Hashtbl.create 16 in
    List.iter (fun (i, r) -> Hashtbl.add by_index i r) indexed;
    let containers =
      List.filter_map
        (fun (_, r) ->
          if (H.meta r).contains_injection then
            Some (H.start_byte r, H.end_byte r)
          else None)
        indexed
    in
    let boundaries =
      List.fold_left
        (fun acc (i, r) ->
          End (H.end_byte r, i) :: Start (H.start_byte r, i) :: acc)
        [] indexed
      |> List.stable_sort (fun a b ->
          if pos a <> pos b then Int.compare (pos a) (pos b)
          else
            match (a, b) with
            | End _, Start _ -> -1
            | Start _, End _ -> 1
            | _ -> 0)
    in
    let active = ref [] and out = Buffer.create len and spans = ref [] in
    let current = ref 0 in
    let base = Syntax_style.base style in
    let emit first last scopes replacement =
      let text =
        match replacement with
        | Some text -> text
        | None -> String.sub content first (last - first)
      in
      Buffer.add_string out text;
      if text <> "" then
        let style =
          List.fold_left
            (fun base scope ->
              Ansi.Style.merge ~base
                ~overlay:(Syntax_style.resolve_overlay style scope))
            base scopes
        in
        spans := (text, style) :: !spans
    in
    let flush pos =
      if pos > !current then begin
        let ranges = List.map (fun i -> (i, Hashtbl.find by_index i)) !active in
        let replacement =
          if conceal then List.find_map (fun (_, r) -> replacement r) ranges
          else None
        in
        let scopes =
          match replacement with
          | Some _ -> []
          | None ->
              let inside =
                List.exists
                  (fun (s, e) -> !current >= s && !current < e)
                  containers
              in
              ranges
              |> List.filter (fun (_, r) ->
                  (not inside) || (H.meta r).is_injection
                  || not (String.equal (H.scope r) "markup.raw.block"))
              |> List.sort (fun (i, a) (j, b) ->
                  let c =
                    Int.compare
                      (specificity (H.scope a))
                      (specificity (H.scope b))
                  in
                  if c <> 0 then c else Int.compare i j)
              |> List.map (fun (_, r) -> H.scope r)
        in
        emit !current pos scopes replacement;
        current := pos
      end
    in
    let skip pos c =
      if pos < len && content.[pos] = c then current := pos + 1
    in
    List.iter
      (fun boundary ->
        flush (pos boundary);
        match boundary with
        | Start (_, i) -> active := i :: !active
        | End (p, i) ->
            active := List.filter (( <> ) i) !active;
            if conceal then begin
              let r = Hashtbl.find by_index i in
              let meta = H.meta r in
              if meta.conceal_lines then skip p '\n';
              match meta.conceal with
              | Some " " -> skip p ' '
              | Some "" when H.scope r = "conceal" && not meta.is_injection ->
                  skip p ' '
              | _ -> ()
            end)
      boundaries;
    if !current < len then emit !current len [] None;
    (Buffer.contents out, List.rev !spans)
end

(* Equal styles over equal bytes: span boundaries between equal styles are
   not part of the result. *)
let styled_bytes (text, spans) =
  ignore text;
  List.concat_map
    (fun (text, style) ->
      List.init (String.length text) (fun i -> (text.[i], style)))
    spans

let random_captures random =
  let pick a = a.(Random.State.int random (Array.length a)) in
  let content =
    String.concat ""
      (List.init (Random.State.int random 40) (fun _ ->
           pick [| "a"; "b"; " "; "\n"; "\195\169" |]))
  in
  let len = String.length content in
  let range () =
    let start_byte = Random.State.int random (len + 1) in
    let end_byte =
      start_byte + Random.State.int random (len - start_byte + 1)
    in
    let meta =
      {
        Syntax_highlight.is_injection = Random.State.int random 4 = 0;
        contains_injection = Random.State.int random 4 = 0;
        conceal =
          (if Random.State.int random 3 = 0 then
             Some (pick [| ""; " "; "*"; "<>" |])
           else None);
        conceal_lines = Random.State.int random 5 = 0;
      }
    in
    Syntax_highlight.range ~meta ~start_byte ~end_byte
      ~scope:
        (pick
           [|
             "keyword";
             "keyword.control";
             "string";
             "markup.raw.block";
             "conceal";
             "conceal.with.space";
             "conceal.other";
             "x.y.z";
           |])
      ()
  in
  let ranges = List.init (Random.State.int random 12) (fun _ -> range ()) in
  let ranges =
    if Random.State.bool random then
      List.stable_sort
        (fun a b ->
          Int.compare
            (Syntax_highlight.start_byte a)
            (Syntax_highlight.start_byte b))
        ranges
    else ranges
  in
  Syntax_highlight.make ~content ranges

let resolution_matches_reference () =
  let style =
    Syntax_style.make ~base:bold
      [
        ("keyword", red);
        ("keyword.control", green);
        ("string", Ansi.Style.make ~underline:true ());
        ("markup.raw.block", Ansi.Style.make ~bg:Ansi.Color.blue ());
        ("x", Ansi.Style.make ~italic:true ());
        ("x.y.z", Ansi.Style.make ~fg:Ansi.Color.cyan ());
      ]
  in
  let random = Random.State.make [| 8 |] in
  for case = 1 to 5_000 do
    let captures = random_captures random in
    List.iter
      (fun conceal ->
        let spans = Syntax_highlight.to_spans ~conceal ~style captures in
        let ((text, _) as expected) =
          Reference.spans ~conceal ~style captures
        in
        let actual =
          ( text_of_spans spans,
            List.map (fun s -> (span_text s, span_style s)) spans )
        in
        if
          not
            (String.equal text (fst actual)
            && List.equal
                 (fun (c, a) (d, b) -> Char.equal c d && Ansi.Style.equal a b)
                 (styled_bytes expected) (styled_bytes actual))
        then
          failf "case %d (conceal %b): %S over %S" case conceal (fst actual)
            (Syntax_highlight.content captures))
      [ false; true ]
  done

let () =
  exit
    (run "mosaic.syntax_highlight"
       [
         group "Annotations"
           [
             test "shared capture batches are safe across domains"
               shared_capture_batches_are_safe_across_domains;
             test "source-bound paint retains geometry"
               annotations_are_source_bound_and_preserve_geometry;
             test "byte boundaries match styled graphemes"
               annotation_boundaries_match_styled_graphemes;
             test "conceal projection is reused across themes"
               conceal_projection_is_reused_across_themes;
             test "bounds are validated" annotations_validate_bounds;
             test "merge puts the overlay over the base"
               merge_puts_the_overlay_over_the_base;
             test "unordered annotations preserve overlay order"
               unordered_annotations_preserve_overlay_order;
           ];
         group "Ranges"
           [
             test "scope vocabulary fallback and extensions"
               vocabulary_fallback_and_extensions;
             test "styles compare by value" styles_compare_by_value;
             test "of_triples applies style" of_triples_applies_style;
             test "overlapping same scope keeps later range active"
               overlapping_same_scope_keeps_later_range_active;
             test "specificity cascades from parent to child"
               specificity_cascades_from_parent_to_child;
             test "invalid range raises" invalid_range_raises;
             test "resolution matches the reference on random overlaps"
               resolution_matches_reference;
           ];
         group "Metadata"
           [
             test "conceal replaces range" conceal_replaces_range;
             test "injection container suppresses container style"
               injection_container_suppresses_container_style;
           ];
       ])
