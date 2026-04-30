type meta = {
  is_injection : bool;
  contains_injection : bool;
  conceal : string option;
  conceal_lines : bool;
}

let default_meta =
  {
    is_injection = false;
    contains_injection = false;
    conceal = None;
    conceal_lines = false;
  }

type range = { start_byte : int; end_byte : int; scope : string; meta : meta }
type t = range list

let range ?(meta = default_meta) ~start_byte ~end_byte ~scope () =
  if start_byte < 0 || end_byte < start_byte then
    invalid_arg
      (Printf.sprintf "Syntax_highlight.range: invalid range (%d, %d)"
         start_byte end_byte);
  { start_byte; end_byte; scope; meta }

let of_triples ranges =
  List.map
    (fun (start_byte, end_byte, scope) -> range ~start_byte ~end_byte ~scope ())
    ranges

let start_byte r = r.start_byte
let end_byte r = r.end_byte
let scope r = r.scope
let meta r = r.meta

let specificity scope =
  let n = ref 1 in
  String.iter (fun c -> if c = '.' then incr n) scope;
  !n

let should_suppress_in_injection range =
  (not range.meta.is_injection) && String.equal range.scope "markup.raw.block"

type indexed_range = { range : range; index : int }
type boundary = Start of int * int | End of int * int

let boundary_pos = function Start (pos, _) | End (pos, _) -> pos

let validate ~content ranges =
  let len = String.length content in
  List.iter
    (fun r ->
      if r.start_byte < 0 || r.end_byte > len || r.start_byte > r.end_byte then
        invalid_arg
          (Printf.sprintf
             "Syntax_highlight.to_spans: range (%d, %d) out of bounds for \
              content of length %d"
             r.start_byte r.end_byte len))
    ranges

let push_span result text style =
  if String.length text > 0 then
    result := { Text_buffer.text; style } :: !result

let coalesce_spans spans =
  let flush acc style texts =
    match texts with
    | [] -> acc
    | _ ->
        { Text_buffer.text = String.concat "" (List.rev texts); style } :: acc
  in
  let rec loop acc style texts = function
    | [] -> List.rev (flush acc style texts)
    | (span : Text_buffer.span) :: rest -> (
        match texts with
        | [] -> loop acc span.style [ span.text ] rest
        | _ when Ansi.Style.equal style span.style ->
            loop acc style (span.text :: texts) rest
        | _ ->
            let acc = flush acc style texts in
            loop acc span.style [ span.text ] rest)
  in
  match spans with
  | [] -> []
  | (span : Text_buffer.span) :: rest -> loop [] span.style [ span.text ] rest

let index_ranges ranges =
  ranges
  |> List.mapi (fun index range -> { range; index })
  |> List.filter (fun item -> item.range.start_byte <> item.range.end_byte)

let range_table ranges =
  let table = Hashtbl.create (List.length ranges) in
  List.iter (fun item -> Hashtbl.add table item.index item.range) ranges;
  table

let collect_injection_containers ranges =
  List.filter_map
    (fun item ->
      if item.range.meta.contains_injection then
        Some (item.range.start_byte, item.range.end_byte)
      else None)
    ranges

let active_ranges table active =
  List.map (fun id -> (id, Hashtbl.find table id)) active

let inside_injection_container containers pos =
  List.exists
    (fun (start_byte, end_byte) -> pos >= start_byte && pos < end_byte)
    containers

let conceal_replacement range =
  match range.meta.conceal with
  | Some text -> Some text
  | None ->
      if
        String.equal range.scope "conceal"
        || String.starts_with ~prefix:"conceal." range.scope
      then
        if String.equal range.scope "conceal.with.space" then Some " "
        else Some ""
      else None

let concealed_text ~conceal ranges =
  if conceal then
    List.find_map
      (fun (_, range) ->
        match conceal_replacement range with
        | Some replacement -> Some replacement
        | None -> None)
      ranges
  else None

let active_style ~style ~base_style ~injection_containers ~pos ranges =
  let inside_injection = inside_injection_container injection_containers pos in
  let ranges =
    ranges
    |> List.filter (fun (_, range) ->
        (not inside_injection) || not (should_suppress_in_injection range))
    |> List.sort (fun (index_a, range_a) (index_b, range_b) ->
        let c =
          Int.compare (specificity range_a.scope) (specificity range_b.scope)
        in
        if c <> 0 then c else Int.compare index_a index_b)
  in
  List.fold_left
    (fun acc (_, range) ->
      Ansi.Style.merge ~base:acc
        ~overlay:(Syntax_style.resolve_overlay style range.scope))
    base_style ranges

let emit_segment ~conceal ~style ~base_style ~content ~injection_containers
    result current pos ranges =
  if pos > current then
    match concealed_text ~conceal ranges with
    | Some replacement -> push_span result replacement base_style
    | None ->
        let text = String.sub content current (pos - current) in
        let style =
          active_style ~style ~base_style ~injection_containers ~pos:current
            ranges
        in
        push_span result text style

let sorted_boundaries ranges =
  let boundaries = ref [] in
  List.iter
    (fun item ->
      if item.range.start_byte <> item.range.end_byte then begin
        boundaries := Start (item.range.start_byte, item.index) :: !boundaries;
        boundaries := End (item.range.end_byte, item.index) :: !boundaries
      end)
    ranges;
  List.sort
    (fun a b ->
      let pos_a = boundary_pos a and pos_b = boundary_pos b in
      if pos_a <> pos_b then Int.compare pos_a pos_b
      else
        match (a, b) with End _, Start _ -> -1 | Start _, End _ -> 1 | _ -> 0)
    !boundaries

let to_spans ?(conceal = true) ~style ~content ranges =
  validate ~content ranges;
  let base_style = Syntax_style.base style in
  let len = String.length content in
  if len = 0 then []
  else if ranges = [] then
    [ { Text_buffer.text = content; style = base_style } ]
  else begin
    let indexed = index_ranges ranges in
    let by_index = range_table indexed in
    let injection_containers = collect_injection_containers indexed in
    let boundaries = sorted_boundaries indexed in
    let active = ref [] in
    let result = ref [] in
    let current = ref 0 in
    let flush_until pos =
      if pos > !current then begin
        let ranges = active_ranges by_index !active in
        emit_segment ~conceal ~style ~base_style ~content ~injection_containers
          result !current pos ranges;
        current := pos
      end
    in
    let skip_char_at pos c =
      if pos < len && Char.equal (String.unsafe_get content pos) c then
        current := pos + 1
    in
    List.iter
      (fun boundary ->
        let pos = boundary_pos boundary in
        flush_until pos;
        match boundary with
        | Start (_, id) -> active := id :: !active
        | End (_, id) ->
            active := List.filter (fun active_id -> active_id <> id) !active;
            if conceal then begin
              let range = Hashtbl.find by_index id in
              if range.meta.conceal_lines then skip_char_at pos '\n';
              match range.meta.conceal with
              | Some " " -> skip_char_at pos ' '
              | Some ""
                when String.equal range.scope "conceal"
                     && not range.meta.is_injection ->
                  skip_char_at pos ' '
              | _ -> ()
            end)
      boundaries;
    if !current < len then
      push_span result (String.sub content !current (len - !current)) base_style;
    coalesce_spans (List.rev !result)
  end
