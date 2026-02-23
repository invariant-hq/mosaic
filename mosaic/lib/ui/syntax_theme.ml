type t = { base : Ansi.Style.t; overlays : (string, Ansi.Style.t) Hashtbl.t }

let make ~base mappings =
  let overlays = Hashtbl.create (List.length mappings) in
  List.iter
    (fun (group, style) -> Hashtbl.replace overlays group style)
    mappings;
  { base; overlays }

let rec resolve_overlay t group =
  match Hashtbl.find_opt t.overlays group with
  | Some overlay -> overlay
  | None -> (
      match String.rindex_opt group '.' with
      | Some i -> resolve_overlay t (String.sub group 0 i)
      | None -> Ansi.Style.default)

let resolve t group =
  Ansi.Style.merge ~base:t.base ~overlay:(resolve_overlay t group)

let default =
  let base = Ansi.Style.default in
  make ~base
    [
      ( "comment",
        Ansi.Style.make ~italic:true ~fg:(Ansi.Color.grayscale ~level:12) () );
      ( "keyword",
        Ansi.Style.make ~bold:true ~fg:(Ansi.Color.of_rgb 255 151 0) () );
      ("string", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 229 192 123) ());
      ("number", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 209 154 102) ());
      ("function", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 97 175 239) ());
      ("type", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 86 182 194) ());
      ("variable", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 224 108 117) ());
      ("operator", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 255 151 0) ());
      ("punctuation", Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ());
      ("constant", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 209 154 102) ());
      ("tag", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 224 108 117) ());
      ("attribute", Ansi.Style.make ~fg:(Ansi.Color.of_rgb 229 192 123) ());
    ]

(* Count dots in a group name — more dots means more specific. *)
let specificity group =
  let n = ref 0 in
  String.iter (fun c -> if c = '.' then incr n) group;
  !n

(* A boundary event: a range starts or ends at a byte offset. *)
type boundary = Start of int * string | End of int * string

let apply t ~content ranges =
  let len = String.length content in
  List.iter
    (fun (s, e, _) ->
      if s < 0 || e > len || s > e then
        invalid_arg
          (Printf.sprintf
             "Syntax_theme.apply: range (%d, %d) out of bounds for content of \
              length %d"
             s e len))
    ranges;
  if ranges = [] then
    if len = 0 then [] else [ { Text_buffer.text = content; style = t.base } ]
  else
    (* Build sorted boundary list. Each range contributes a Start and End. *)
    let boundaries =
      let acc = ref [] in
      List.iter
        (fun (s, e, group) -> acc := Start (s, group) :: End (e, group) :: !acc)
        ranges;
      List.sort
        (fun a b ->
          let pos_of = function Start (p, _) -> p | End (p, _) -> p in
          let pa = pos_of a and pb = pos_of b in
          if pa <> pb then Int.compare pa pb
          else
            (* Ends before Starts at same position *)
            match (a, b) with
            | End _, Start _ -> -1
            | Start _, End _ -> 1
            | _ -> 0)
        !acc
    in
    (* Walk boundaries, tracking active groups. *)
    let active : (string * int) list ref = ref [] in
    (* (group, order) — order is insertion order for tie-breaking *)
    let order = ref 0 in
    let result = ref [] in
    let last_pos = ref 0 in
    let flush_segment pos =
      if pos > !last_pos then begin
        let text = String.sub content !last_pos (pos - !last_pos) in
        let style =
          match !active with
          | [] -> t.base
          | _ ->
              (* Cascade-merge all active groups: least-specific first, then by
                 insertion order. This matches CSS/TextMate cascade semantics
                 where child scopes inherit parent properties. *)
              let sorted =
                List.map (fun (g, ord) -> (g, specificity g, ord)) !active
                |> List.sort (fun (_, s1, o1) (_, s2, o2) ->
                    let c = Int.compare s1 s2 in
                    if c <> 0 then c else Int.compare o1 o2)
              in
              List.fold_left
                (fun acc (g, _, _) ->
                  Ansi.Style.merge ~base:acc ~overlay:(resolve_overlay t g))
                t.base sorted
        in
        result := { Text_buffer.text; style } :: !result
      end;
      last_pos := pos
    in
    List.iter
      (fun boundary ->
        match boundary with
        | Start (pos, group) ->
            flush_segment pos;
            let o = !order in
            incr order;
            active := (group, o) :: !active
        | End (pos, group) ->
            flush_segment pos;
            active :=
              List.filter (fun (g, _) -> not (String.equal g group)) !active)
      boundaries;
    (* Flush any trailing text after the last boundary. *)
    flush_segment len;
    List.rev !result
