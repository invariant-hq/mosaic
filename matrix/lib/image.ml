module Color = Ansi.Color
module Style = Ansi.Style

type style = Style.t
type h_align = [ `Left | `Center | `Right ]
type v_align = [ `Top | `Middle | `Bottom ]
type hit_id = int

type primitive =
  | P_fill of {
      x : int;
      y : int;
      width : int;
      height : int;
      color : Color.t;
      clip : Grid.region option;
    }
  | P_text of {
      x : int;
      y : int;
      lines : string array;
      style : Style.t;
      width_method : Glyph.width_method;
      clip : Grid.region option;
    }
  | P_box of {
      x : int;
      y : int;
      width : int;
      height : int;
      border : Grid.Border.t;
      border_sides : Grid.Border.side list;
      border_style : Style.t;
      fill : Color.t option;
      clip : Grid.region option;
    }
  | P_hit of {
      x : int;
      y : int;
      width : int;
      height : int;
      id : hit_id;
      clip : Grid.region option;
    }
  | P_custom of {
      x : int;
      y : int;
      draw : Grid.t -> Screen.Hit_grid.t option -> x:int -> y:int -> unit;
      clip : Grid.region option;
    }

type t = {
  width : int;
  height : int;
  ops : primitive array;
  clip : Grid.region option;
}

let clamp_nonneg n = if n <= 0 then 0 else n
let empty = { width = 0; height = 0; ops = [||]; clip = None }

let void width height =
  let width = clamp_nonneg width and height = clamp_nonneg height in
  if width = 0 || height = 0 then empty
  else { width; height; ops = [||]; clip = None }

let width { width; _ } = width
let height { height; _ } = height
let size t = (t.width, t.height)
let default_width_method = `Unicode

type region = Grid.region

let intersect_rect (a : region) (b : region) : region option =
  let open Grid in
  let x = max a.x b.x in
  let y = max a.y b.y in
  let x2 = min (a.x + a.width) (b.x + b.width) in
  let y2 = min (a.y + a.height) (b.y + b.height) in
  let width = x2 - x in
  let height = y2 - y in
  if width <= 0 || height <= 0 then None else Some { x; y; width; height }

let merge_clip (a : region option) (b : region option) =
  match (a, b) with
  | None, clip | clip, None -> clip
  | Some a, Some b -> intersect_rect a b

let shift_clip rect ~dx ~dy =
  let open Grid in
  { rect with x = rect.x + dx; y = rect.y + dy }

let shift_clip_opt clip ~dx ~dy =
  match clip with None -> None | Some rect -> Some (shift_clip rect ~dx ~dy)

let set_primitive_clip prim clip =
  match prim with
  | P_fill r -> P_fill { r with clip = merge_clip r.clip clip }
  | P_text r -> P_text { r with clip = merge_clip r.clip clip }
  | P_box r -> P_box { r with clip = merge_clip r.clip clip }
  | P_hit r -> P_hit { r with clip = merge_clip r.clip clip }
  | P_custom r -> P_custom { r with clip = merge_clip r.clip clip }

let shift_primitive prim ~dx ~dy =
  let clip = shift_clip_opt in
  match prim with
  | P_fill r ->
      P_fill { r with x = r.x + dx; y = r.y + dy; clip = clip r.clip ~dx ~dy }
  | P_text r ->
      P_text { r with x = r.x + dx; y = r.y + dy; clip = clip r.clip ~dx ~dy }
  | P_box r ->
      P_box { r with x = r.x + dx; y = r.y + dy; clip = clip r.clip ~dx ~dy }
  | P_hit r ->
      P_hit { r with x = r.x + dx; y = r.y + dy; clip = clip r.clip ~dx ~dy }
  | P_custom r ->
      P_custom { r with x = r.x + dx; y = r.y + dy; clip = clip r.clip ~dx ~dy }

let apply_image_clip t =
  match t.clip with
  | None -> t.ops
  | Some clip ->
      Array.map (fun prim -> set_primitive_clip prim (Some clip)) t.ops

let make_text ~style ~width_method lines =
  match lines with
  | [] -> empty
  | _ ->
      let widths =
        List.map
          (fun line -> Glyph.String.measure ~width_method ~tab_width:2 line)
          lines
      in
      let width = List.fold_left max 0 widths in
      let height = List.length lines in
      if width = 0 || height = 0 then empty
      else
        let lines = Array.of_list lines in
        {
          width;
          height;
          ops =
            [|
              P_text { x = 0; y = 0; lines; style; width_method; clip = None };
            |];
          clip = None;
        }

let text ?(style = Style.default) ?(width_method = default_width_method) s =
  if String.equal s "" then empty
  else
    let lines = String.split_on_char '\n' s in
    make_text ~style ~width_method lines

let string ?style ?width_method s = text ?style ?width_method s
let line ?style ?width_method s = text ?style ?width_method s

let fill ?(color = Color.black) ~width ~height () =
  let width = clamp_nonneg width and height = clamp_nonneg height in
  if width = 0 || height = 0 then empty
  else
    {
      width;
      height;
      ops = [| P_fill { x = 0; y = 0; width; height; color; clip = None } |];
      clip = None;
    }

let box ?(border = Grid.Border.single) ?(border_sides = Grid.Border.all)
    ?(border_style = Style.default) ?fill ~width ~height () =
  let width = clamp_nonneg width and height = clamp_nonneg height in
  if width = 0 || height = 0 then empty
  else
    {
      width;
      height;
      ops =
        [|
          P_box
            {
              x = 0;
              y = 0;
              width;
              height;
              border;
              border_sides;
              border_style;
              fill;
              clip = None;
            };
        |];
      clip = None;
    }

let uchar_to_string uchar =
  let code = Uchar.to_int uchar in
  let buf = Bytes.create 4 in
  let len =
    if code <= 0x7F then (
      Bytes.unsafe_set buf 0 (Char.unsafe_chr code);
      1)
    else if code <= 0x7FF then (
      Bytes.unsafe_set buf 0 (Char.unsafe_chr (0xC0 lor (code lsr 6)));
      Bytes.unsafe_set buf 1 (Char.unsafe_chr (0x80 lor (code land 0x3F)));
      2)
    else if code <= 0xFFFF then (
      Bytes.unsafe_set buf 0 (Char.unsafe_chr (0xE0 lor (code lsr 12)));
      Bytes.unsafe_set buf 1
        (Char.unsafe_chr (0x80 lor ((code lsr 6) land 0x3F)));
      Bytes.unsafe_set buf 2 (Char.unsafe_chr (0x80 lor (code land 0x3F)));
      3)
    else (
      Bytes.unsafe_set buf 0 (Char.unsafe_chr (0xF0 lor (code lsr 18)));
      Bytes.unsafe_set buf 1
        (Char.unsafe_chr (0x80 lor ((code lsr 12) land 0x3F)));
      Bytes.unsafe_set buf 2
        (Char.unsafe_chr (0x80 lor ((code lsr 6) land 0x3F)));
      Bytes.unsafe_set buf 3 (Char.unsafe_chr (0x80 lor (code land 0x3F)));
      4)
  in
  Bytes.sub_string buf 0 len

let repeat_uchar uchar count =
  let glyph = uchar_to_string uchar in
  let buf = Buffer.create (count * String.length glyph) in
  for _ = 1 to count do
    Buffer.add_string buf glyph
  done;
  Buffer.contents buf

let rule_h ?style ~width () =
  let style = Option.value style ~default:Style.default in
  let width = clamp_nonneg width in
  if width = 0 then empty
  else
    let ch = Uchar.of_int Grid.Border.single.horizontal in
    let line = repeat_uchar ch width in
    make_text ~style ~width_method:default_width_method [ line ]

let rule_v ?style ~height () =
  let style = Option.value style ~default:Style.default in
  let height = clamp_nonneg height in
  if height = 0 then empty
  else
    let ch = Uchar.of_int Grid.Border.single.vertical in
    let line = repeat_uchar ch 1 in
    let lines = List.init height (fun _ -> line) in
    make_text ~style ~width_method:default_width_method lines

let filter_non_empty images =
  List.filter (fun t -> t.width > 0 && t.height > 0) images

let dummy_primitive =
  P_fill
    { x = 0; y = 0; width = 0; height = 0; color = Color.black; clip = None }

let hcat images =
  let images = filter_non_empty images in
  match images with
  | [] -> empty
  | [ single ] -> single
  | _ ->
      let width = List.fold_left (fun acc t -> acc + t.width) 0 images in
      let height = List.fold_left (fun acc t -> max acc t.height) 0 images in
      let total_ops =
        List.fold_left (fun acc t -> acc + Array.length t.ops) 0 images
      in
      if width = 0 || height = 0 then empty
      else
        let ops = Array.make total_ops dummy_primitive in
        let idx = ref 0 in
        let offset_x = ref 0 in
        List.iter
          (fun t ->
            let ops_with_clip = apply_image_clip t in
            Array.iter
              (fun prim ->
                ops.(!idx) <- shift_primitive prim ~dx:!offset_x ~dy:0;
                incr idx)
              ops_with_clip;
            offset_x := !offset_x + t.width)
          images;
        { width; height; ops; clip = None }

let vcat images =
  let images = filter_non_empty images in
  match images with
  | [] -> empty
  | [ single ] -> single
  | _ ->
      let width = List.fold_left (fun acc t -> max acc t.width) 0 images in
      let height = List.fold_left (fun acc t -> acc + t.height) 0 images in
      let total_ops =
        List.fold_left (fun acc t -> acc + Array.length t.ops) 0 images
      in
      if width = 0 || height = 0 then empty
      else
        let ops = Array.make total_ops dummy_primitive in
        let idx = ref 0 in
        let offset_y = ref 0 in
        List.iter
          (fun t ->
            let ops_with_clip = apply_image_clip t in
            Array.iter
              (fun prim ->
                ops.(!idx) <- shift_primitive prim ~dx:0 ~dy:!offset_y;
                incr idx)
              ops_with_clip;
            offset_y := !offset_y + t.height)
          images;
        { width; height; ops; clip = None }

let overlay images =
  let images = filter_non_empty images in
  match images with
  | [] -> empty
  | [ single ] -> single
  | _ ->
      let width = List.fold_left (fun acc t -> max acc t.width) 0 images in
      let height = List.fold_left (fun acc t -> max acc t.height) 0 images in
      let total_ops =
        List.fold_left (fun acc t -> acc + Array.length t.ops) 0 images
      in
      if width = 0 || height = 0 then empty
      else
        let ops = Array.make total_ops dummy_primitive in
        let idx = ref 0 in
        List.iter
          (fun t ->
            let ops_with_clip = apply_image_clip t in
            Array.iter
              (fun prim ->
                ops.(!idx) <- prim;
                incr idx)
              ops_with_clip)
          images;
        { width; height; ops; clip = None }

let pad ?(left = 0) ?(right = 0) ?(top = 0) ?(bottom = 0) t =
  let left = clamp_nonneg left
  and right = clamp_nonneg right
  and top = clamp_nonneg top
  and bottom = clamp_nonneg bottom in
  if left = 0 && right = 0 && top = 0 && bottom = 0 then t
  else
    let width = t.width + left + right in
    let height = t.height + top + bottom in
    if width = 0 || height = 0 then empty
    else
      let ops =
        Array.map (fun prim -> shift_primitive prim ~dx:left ~dy:top) t.ops
      in
      let clip = shift_clip_opt t.clip ~dx:left ~dy:top in
      { width; height; ops; clip }

let hpad left right t = pad ~left ~right t
let vpad top bottom t = pad ~top ~bottom t

let normalize_crop value size =
  let v = clamp_nonneg value in
  if v > size then size else v

let apply_crop ?(left = 0) ?(right = 0) ?(top = 0) ?(bottom = 0) child =
  let child = if left < 0 then pad ~left:(-left) child else child in
  let child = if right < 0 then pad ~right:(-right) child else child in
  let child = if top < 0 then pad ~top:(-top) child else child in
  let child = if bottom < 0 then pad ~bottom:(-bottom) child else child in
  if left = 0 && right = 0 && top = 0 && bottom = 0 then child
  else
    let left = normalize_crop left child.width in
    let right = normalize_crop right (child.width - left) in
    let top = normalize_crop top child.height in
    let bottom = normalize_crop bottom (child.height - top) in
    let width = child.width - left - right in
    let height = child.height - top - bottom in
    if width <= 0 || height <= 0 then empty
    else
      let ops =
        Array.map
          (fun prim -> shift_primitive prim ~dx:(-left) ~dy:(-top))
          child.ops
      in
      let base_clip = shift_clip_opt child.clip ~dx:(-left) ~dy:(-top) in
      let clip =
        merge_clip base_clip (Some { Grid.x = 0; y = 0; width; height })
      in
      { width; height; ops; clip }

let hcrop left right child = apply_crop ~left ~right child
let vcrop top bottom child = apply_crop ~top ~bottom child

let crop ?l ?r ?t ?b child =
  apply_crop
    ~left:(Option.value l ~default:0)
    ~right:(Option.value r ~default:0)
    ~top:(Option.value t ~default:0)
    ~bottom:(Option.value b ~default:0)
    child

let hsnap ?(align = `Center) target image =
  if target <= 0 then empty
  else if target = image.width then image
  else if target < image.width then
    let excess = image.width - target in
    match align with
    | `Left -> hcrop 0 excess image
    | `Right -> hcrop excess 0 image
    | `Center ->
        let left = excess / 2 in
        let right = excess - left in
        hcrop left right image
  else
    let padding = target - image.width in
    match align with
    | `Left -> hpad 0 padding image
    | `Right -> hpad padding 0 image
    | `Center ->
        let left = padding / 2 in
        let right = padding - left in
        hpad left right image

let vsnap ?(align = `Middle) target image =
  if target <= 0 then empty
  else if target = image.height then image
  else if target < image.height then
    let excess = image.height - target in
    match align with
    | `Top -> vcrop 0 excess image
    | `Bottom -> vcrop excess 0 image
    | `Middle ->
        let top = excess / 2 in
        let bottom = excess - top in
        vcrop top bottom image
  else
    let padding = target - image.height in
    match align with
    | `Top -> vpad 0 padding image
    | `Bottom -> vpad padding 0 image
    | `Middle ->
        let top = padding / 2 in
        let bottom = padding - top in
        vpad top bottom image

let with_hit ~id image =
  if id <= 0 || image.width = 0 || image.height = 0 then image
  else
    let len = Array.length image.ops in
    let ops = Array.make (len + 1) dummy_primitive in
    Array.blit image.ops 0 ops 0 len;
    ops.(len) <-
      P_hit
        {
          x = 0;
          y = 0;
          width = image.width;
          height = image.height;
          id;
          clip = None;
        };
    { image with ops }

let with_hit_rect ~id ~x ~y ~width ~height image =
  let width = clamp_nonneg width and height = clamp_nonneg height in
  if id <= 0 || width = 0 || height = 0 then image
  else
    let len = Array.length image.ops in
    let ops = Array.make (len + 1) dummy_primitive in
    Array.blit image.ops 0 ops 0 len;
    ops.(len) <- P_hit { x; y; width; height; id; clip = None };
    { image with ops }

let custom ~width ~height draw =
  let width = clamp_nonneg width and height = clamp_nonneg height in
  if width = 0 || height = 0 then empty
  else
    {
      width;
      height;
      ops = [| P_custom { x = 0; y = 0; draw; clip = None } |];
      clip = None;
    }

let add_hit ?clip hits ~x ~y ~width ~height ~id =
  match hits with
  | None -> ()
  | Some hits -> (
      let rect = { Grid.x; y; width; height } in
      let clipped = merge_clip clip (Some rect) in
      match clipped with
      | None -> ()
      | Some rect ->
          Screen.Hit_grid.add hits ~x:rect.x ~y:rect.y ~width:rect.width
            ~height:rect.height ~id)

let render ?hits ?(x = 0) ?(y = 0) grid t =
  if t.width <= 0 || t.height <= 0 then ()
  else
    let base_clip = shift_clip_opt t.clip ~dx:x ~dy:y in
    let grid_height = Grid.height grid in
    Array.iter
      (function
        | P_fill r -> (
            let clip =
              merge_clip base_clip (shift_clip_opt r.clip ~dx:x ~dy:y)
            in
            let draw () =
              Grid.fill_rect grid ~x:(x + r.x) ~y:(y + r.y) ~width:r.width
                ~height:r.height ~color:r.color
            in
            match clip with
            | None -> draw ()
            | Some rect -> Grid.clip grid rect draw)
        | P_text r -> (
            let clip =
              merge_clip base_clip (shift_clip_opt r.clip ~dx:x ~dy:y)
            in
            let draw () =
              let len = Array.length r.lines in
              for i = 0 to len - 1 do
                let line_y = y + r.y + i in
                if line_y >= 0 && line_y < grid_height then
                  Grid.draw_text grid ~x:(x + r.x) ~y:line_y ~text:r.lines.(i)
                    ~style:r.style
              done
            in
            match clip with
            | None -> draw ()
            | Some rect -> Grid.clip grid rect draw)
        | P_box r -> (
            let clip =
              merge_clip base_clip (shift_clip_opt r.clip ~dx:x ~dy:y)
            in
            let draw () =
              if r.width > 0 && r.height > 0 then
                let style =
                  match r.fill with
                  | Some _ -> r.border_style
                  | None -> Ansi.Style.bg Color.black r.border_style
                in
                Grid.draw_box grid ~x:(x + r.x) ~y:(y + r.y) ~width:r.width
                  ~height:r.height ~border:r.border ~sides:r.border_sides ~style
                  ?fill:r.fill ()
            in
            match clip with
            | None -> draw ()
            | Some rect -> Grid.clip grid rect draw)
        | P_hit r ->
            let clip =
              merge_clip base_clip (shift_clip_opt r.clip ~dx:x ~dy:y)
            in
            add_hit ?clip hits ~x:(x + r.x) ~y:(y + r.y) ~width:r.width
              ~height:r.height ~id:r.id
        | P_custom r -> (
            let clip =
              merge_clip base_clip (shift_clip_opt r.clip ~dx:x ~dy:y)
            in
            let draw () = r.draw grid hits ~x:(x + r.x) ~y:(y + r.y) in
            match clip with
            | None -> draw ()
            | Some rect -> Grid.clip grid rect draw))
      t.ops

let draw t grid hits = render ~hits grid t
