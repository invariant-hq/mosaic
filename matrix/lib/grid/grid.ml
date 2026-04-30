(* grid.ml *)

module Cell = Packed_cell
module Color_packed = Ansi.Color.Packed

(* {1 Internal modules} *)

module Buf = struct
  type ('a, 'b) t = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

  let create kind len = Bigarray.Array1.create kind Bigarray.c_layout len
  let make_int16 len = create Bigarray.int16_unsigned len
  let make_int32 len = create Bigarray.int32 len
  let make_int len = create Bigarray.int len
  let[@inline] get arr i = Bigarray.Array1.unsafe_get arr i
  let[@inline] set arr i v = Bigarray.Array1.unsafe_set arr i v
  let fill = Bigarray.Array1.fill
  let blit = Bigarray.Array1.blit
  let dim = Bigarray.Array1.dim

  let[@inline] get_cell arr i =
    Packed_cell.unsafe_of_int (Bigarray.Array1.unsafe_get arr i)

  let[@inline] set_cell arr i (v : Packed_cell.t) =
    Bigarray.Array1.unsafe_set arr i (Packed_cell.to_int v)

  let fill_cell arr (v : Packed_cell.t) =
    Bigarray.Array1.fill arr (Packed_cell.to_int v)

  let[@inline] blit_range src ~src_pos dst ~dst_pos ~len =
    if src == dst && src_pos < dst_pos then
      for i = len - 1 downto 0 do
        set dst (dst_pos + i) (get src (src_pos + i))
      done
    else
      for i = 0 to len - 1 do
        set dst (dst_pos + i) (get src (src_pos + i))
      done

  let[@inline] fill_range arr ~pos ~len v =
    for i = pos to pos + len - 1 do
      set arr i v
    done

  let[@inline] blit_cell_range src ~src_pos dst ~dst_pos ~len =
    if src == dst && src_pos < dst_pos then
      for i = len - 1 downto 0 do
        set_cell dst (dst_pos + i) (get_cell src (src_pos + i))
      done
    else
      for i = 0 to len - 1 do
        set_cell dst (dst_pos + i) (get_cell src (src_pos + i))
      done

  let[@inline] fill_cell_range arr ~pos ~len v =
    let v = Packed_cell.to_int v in
    for i = pos to pos + len - 1 do
      set arr i v
    done
end

module Color_plane = struct
  let[@inline] perceptual_alpha a =
    if a >= 0.8 then
      let norm = (a -. 0.8) *. 5. in
      0.8 +. (Float.pow norm 0.2 *. 0.2)
    else Float.pow a 0.9

  let[@inline] mix src dst alpha = (src *. alpha) +. (dst *. (1. -. alpha))
end

module Rect = struct
  type t = { x : int; y : int; width : int; height : int }

  let intersection a b =
    let x = max a.x b.x in
    let y = max a.y b.y in
    let w = min (a.x + a.width) (b.x + b.width) - x in
    let h = min (a.y + a.height) (b.y + b.height) - y in
    if w > 0 && h > 0 then Some { x; y; width = w; height = h } else None

  let clip_to_bounds max_w max_h r =
    let x = max 0 r.x in
    let y = max 0 r.y in
    let w = min (r.x + r.width) max_w - x in
    let h = min (r.y + r.height) max_h - y in
    if w > 0 && h > 0 then Some { x; y; width = w; height = h } else None
end

module Scissor_stack = struct
  type t = Rect.t Dynarray.t

  let create () = Dynarray.create ()
  let current s = if Dynarray.is_empty s then None else Dynarray.find_last s

  let push s r =
    let r =
      match current s with
      | None -> r
      | Some c -> (
          match Rect.intersection c r with
          | Some i -> i
          | None -> Rect.{ x = 0; y = 0; width = 0; height = 0 })
    in
    Dynarray.add_last s r

  let pop s = ignore (Dynarray.pop_last_opt s)
  let clear s = Dynarray.clear s

  let[@inline] contains rect_opt ~x ~y =
    match rect_opt with
    | None -> true
    | Some Rect.{ x = rx; y = ry; width = rw; height = rh } ->
        x >= rx && y >= ry && x < rx + rw && y < ry + rh
end

module Links = struct
  type t = {
    mutable next_id : int32;
    forward : (string, int32) Hashtbl.t;
    reverse : (int32, string) Hashtbl.t;
  }

  let no_link = -1l

  let create () =
    { next_id = 1l; forward = Hashtbl.create 32; reverse = Hashtbl.create 32 }

  let clear t =
    t.next_id <- 1l;
    Hashtbl.clear t.forward;
    Hashtbl.clear t.reverse

  let copy_state ~src ~dst =
    clear dst;
    dst.next_id <- src.next_id;
    Hashtbl.iter (fun k v -> Hashtbl.add dst.forward k v) src.forward;
    Hashtbl.iter (fun k v -> Hashtbl.add dst.reverse k v) src.reverse

  let intern t = function
    | None -> no_link
    | Some url -> (
        match Hashtbl.find_opt t.forward url with
        | Some id -> id
        | None ->
            let id = t.next_id in
            t.next_id <- Int32.add id 1l;
            Hashtbl.add t.forward url id;
            Hashtbl.add t.reverse id url;
            id)

  let resolve t id = Hashtbl.find_opt t.reverse id

  let resolve_direct t id =
    Option.value (Hashtbl.find_opt t.reverse id) ~default:""
end

(* {1 Public types} *)

module Border = Border

type region = Rect.t = { x : int; y : int; width : int; height : int }

type t = {
  mutable width : int;
  mutable height : int;
  grapheme_store : Grapheme_store.t;
  mutable width_method : Text.width_method;
  mutable respect_alpha : bool;
  mutable chars : (int, Bigarray.int_elt) Buf.t;
  mutable fg_color : (int, Bigarray.int_elt) Buf.t;
  mutable bg_color : (int, Bigarray.int_elt) Buf.t;
  mutable attrs : (int, Bigarray.int16_unsigned_elt) Buf.t;
  mutable links : (int32, Bigarray.int32_elt) Buf.t;
  link_registry : Links.t;
  grapheme_tracker : Grapheme_tracker.t;
  scissor_stack : Scissor_stack.t;
  opacity_stack : float Dynarray.t;
  mutable opacity_product : float;
}

(* {1 Constants} *)

let space_cell = Packed_cell.space
let null_cell = Packed_cell.empty
let no_link = Links.no_link
let default_fg = Ansi.Color.of_rgb 255 255 255
let default_bg = Ansi.Color.default
let default_fg_packed = Color_packed.encode default_fg
let default_bg_packed = Color_packed.encode default_bg

(* {1 Helpers} *)

let[@inline] is_clipped t x y =
  not (Scissor_stack.contains (Scissor_stack.current t.scissor_stack) ~x ~y)

let[@inline] get_fg t idx = Color_packed.decode (Buf.get t.fg_color idx)
let[@inline] get_bg t idx = Color_packed.decode (Buf.get t.bg_color idx)

(** Clip a rectangle against both the scissor stack and grid bounds. *)
let clip_rect_to_grid t r =
  let open Rect in
  match Scissor_stack.current t.scissor_stack with
  | None -> clip_to_bounds t.width t.height r
  | Some s -> (
      match intersection r s with
      | None -> None
      | Some i -> clip_to_bounds t.width t.height i)

(** Reset a cell to a space, releasing its grapheme tracker entry. *)
let[@inline] clear_cell t idx =
  let code = Buf.get_cell t.chars idx in
  if code <> space_cell then (
    Grapheme_tracker.remove t.grapheme_tracker code;
    Buf.set_cell t.chars idx space_cell;
    Buf.set t.attrs idx 0;
    Buf.set t.links idx no_link;
    Buf.set t.fg_color idx default_fg_packed;
    Buf.set t.bg_color idx default_bg_packed)

let[@inline] reset_cell_no_tracker t idx =
  Buf.set_cell t.chars idx space_cell;
  Buf.set t.attrs idx 0;
  Buf.set t.links idx no_link;
  Buf.set t.fg_color idx default_fg_packed;
  Buf.set t.bg_color idx default_bg_packed

(** Clean up grapheme spans when overwriting a cell. Replaces neighboring
    continuation/start cells with spaces and releases store references. *)
let cleanup_grapheme_at t idx =
  let code = Buf.get_cell t.chars idx in
  if Packed_cell.is_inline code then begin
    let w = Packed_cell.cell_width code in
    if w > 1 then
      let limit = Buf.dim t.chars in
      for i = 1 to w - 1 do
        let ni = idx + i in
        if ni < limit then clear_cell t ni
      done
  end
  else
    let left = Packed_cell.left_extent code in
    let right = Packed_cell.right_extent code in
    if left > 0 || right > 0 then
      let start_idx = idx - left in
      let end_idx = idx + right in
      let limit = Buf.dim t.chars in
      if start_idx >= 0 && end_idx < limit then
        for i = start_idx to end_idx do
          if i <> idx then clear_cell t i
        done

let cleanup_overwritten_cell t idx =
  let old = Buf.get_cell t.chars idx in
  if old <> space_cell then begin
    if (not (Packed_cell.is_inline old)) || Packed_cell.cell_width old > 1 then
      cleanup_grapheme_at t idx;
    Grapheme_tracker.remove t.grapheme_tracker old
  end

(** Write a cell code into a cell, cleaning up the previous occupant and
    maintaining grapheme tracker state. This is the factored-out core of
    set_cell_internal's cell update logic. *)
let[@inline] write_cell t idx code =
  let old = Buf.get_cell t.chars idx in
  if old <> code then begin
    let old_inline = Packed_cell.is_inline old in
    let new_inline = Packed_cell.is_inline code in
    if (not old_inline) || Packed_cell.cell_width old > 1 then
      cleanup_grapheme_at t idx;
    (match (old_inline, new_inline) with
    | true, true -> ()
    | true, false -> Grapheme_tracker.add t.grapheme_tracker code
    | false, true -> Grapheme_tracker.remove t.grapheme_tracker old
    | false, false ->
        Grapheme_tracker.replace t.grapheme_tracker ~old_id:old ~new_id:code);
    Buf.set_cell t.chars idx code
  end

(** Create a cell copier for transferring complex cells between stores. Caches
    by stable store key to avoid re-interning identical graphemes. *)
let make_cell_copier ~src_store ~dst_store =
  let cache = Hashtbl.create 64 in
  fun code ->
    match Packed_cell.store_key code with
    | None -> code
    | Some key ->
        let start_cell =
          match Hashtbl.find_opt cache key with
          | Some g -> g
          | None ->
              let g = Packed_cell.copy ~src:src_store code ~dst:dst_store in
              Hashtbl.add cache key g;
              g
        in
        if Packed_cell.is_start code then start_cell
        else
          Packed_cell.make_continuation ~code:start_cell
            ~left:(Packed_cell.left_extent code)
            ~right:(Packed_cell.right_extent code)

(* {1 Creation & lifecycle} *)

let fill_defaults t =
  Grapheme_tracker.clear t.grapheme_tracker;
  Links.clear t.link_registry;
  Scissor_stack.clear t.scissor_stack;
  Buf.fill_cell t.chars space_cell;
  Buf.fill t.attrs 0;
  Buf.fill t.links no_link;
  Buf.fill t.fg_color default_fg_packed;
  Buf.fill t.bg_color default_bg_packed

let create_with_store ~width ~height ~grapheme_store ~width_method
    ~respect_alpha =
  if width <= 0 || height <= 0 then
    invalid_arg "Grid.create: width and height must be > 0";
  let size = width * height in
  let t =
    {
      width;
      height;
      grapheme_store;
      width_method;
      respect_alpha;
      chars = Buf.make_int size;
      fg_color = Buf.make_int size;
      bg_color = Buf.make_int size;
      attrs = Buf.make_int16 size;
      links = Buf.make_int32 size;
      link_registry = Links.create ();
      grapheme_tracker = Grapheme_tracker.create grapheme_store;
      scissor_stack = Scissor_stack.create ();
      opacity_stack = Dynarray.create ();
      opacity_product = 1.0;
    }
  in
  fill_defaults t;
  t

let create ~width ~height ?width_method ?(respect_alpha = false) () =
  create_with_store ~width ~height ~grapheme_store:(Grapheme_store.create ())
    ~width_method:(Option.value width_method ~default:`Unicode)
    ~respect_alpha

(* {1 Accessors} *)

let width t = t.width
let height t = t.height
let width_method t = t.width_method
let set_width_method t m = t.width_method <- m
let respect_alpha t = t.respect_alpha
let set_respect_alpha t b = t.respect_alpha <- b

let hyperlink_url t id =
  if id = no_link then None else Links.resolve t.link_registry id

let hyperlink_url_direct t id =
  if id = no_link then "" else Links.resolve_direct t.link_registry id

(* {1 Cell accessors} *)

let[@inline] idx t ~x ~y = (y * t.width) + x
let[@inline] get_cell t idx = Buf.get_cell t.chars idx
let[@inline] get_attrs t idx = Buf.get t.attrs idx
let[@inline] get_link t idx = Buf.get t.links idx

let get_style t idx =
  let attrs = Ansi.Attr.unpack (Buf.get t.attrs idx) in
  let link = hyperlink_url t (Buf.get t.links idx) in
  Ansi.Style.make ~fg:(get_fg t idx) ~bg:(get_bg t idx) ?link ()
  |> Ansi.Style.with_attrs attrs

let get_background t idx = get_bg t idx

let get_text t idx =
  let c = Buf.get_cell t.chars idx in
  if Packed_cell.is_continuation c then ""
  else Packed_cell.to_string t.grapheme_store c

let cell_text_length t idx =
  Packed_cell.length t.grapheme_store (Buf.get_cell t.chars idx)

let blit_cell_text t idx buf ~pos =
  Packed_cell.blit t.grapheme_store (Buf.get_cell t.chars idx) buf ~pos

let is_empty t idx = Buf.get_cell t.chars idx = Packed_cell.empty

let is_continuation t idx =
  Packed_cell.is_continuation (Buf.get_cell t.chars idx)

let is_inline t idx = Packed_cell.is_inline (Buf.get_cell t.chars idx)
let cell_width t idx = Packed_cell.cell_width (Buf.get_cell t.chars idx)

let[@inline] cells_equal t1 idx1 t2 idx2 =
  Buf.get_cell t1.chars idx1 = Buf.get_cell t2.chars idx2
  && Buf.get t1.attrs idx1 = Buf.get t2.attrs idx2
  && Buf.get t1.links idx1 = Buf.get t2.links idx2
  && Buf.get t1.fg_color idx1 = Buf.get t2.fg_color idx2
  && Buf.get t1.bg_color idx1 = Buf.get t2.bg_color idx2

let rebuild_grapheme_tracker t =
  Grapheme_tracker.clear t.grapheme_tracker;
  let len = t.width * t.height in
  for i = 0 to len - 1 do
    let code = Buf.get_cell t.chars i in
    if Packed_cell.is_complex code then
      Grapheme_tracker.add t.grapheme_tracker code
  done

let sanitize_spans t =
  for y = 0 to t.height - 1 do
    let row_start = y * t.width in
    let covered_until = ref (-1) in
    for x = 0 to t.width - 1 do
      let idx = row_start + x in
      let code = Buf.get_cell t.chars idx in
      if Packed_cell.is_continuation code then begin
        if x > !covered_until then reset_cell_no_tracker t idx
      end
      else
        let w = Packed_cell.cell_width code in
        if w > 1 then
          if x + w > t.width then reset_cell_no_tracker t idx
          else begin
            let valid = ref true in
            for dx = 1 to w - 1 do
              if
                not
                  (Packed_cell.is_continuation
                     (Buf.get_cell t.chars (idx + dx)))
              then valid := false
            done;
            if !valid then covered_until := max !covered_until (x + w - 1)
            else begin
              reset_cell_no_tracker t idx;
              for dx = 1 to w - 1 do
                reset_cell_no_tracker t (idx + dx)
              done
            end
          end
    done
  done

(* {1 Lifecycle: clear & resize} *)

let clear ?color t =
  Grapheme_tracker.clear t.grapheme_tracker;
  Links.clear t.link_registry;
  Buf.fill_cell t.chars space_cell;
  Buf.fill t.attrs 0;
  Buf.fill t.links no_link;
  Buf.fill t.fg_color default_fg_packed;
  let bg_color =
    Option.value color ~default:default_bg |> Color_packed.encode
  in
  Buf.fill t.bg_color bg_color

let resize t ~width ~height =
  if width <= 0 || height <= 0 then
    invalid_arg "Grid.resize: width and height must be > 0";
  if width = t.width && height = t.height then ()
  else
    let old_w = t.width and old_h = t.height in
    let old_chars = t.chars and old_attrs = t.attrs and old_links = t.links in
    let old_fg_color = t.fg_color and old_bg_color = t.bg_color in

    (* Allocate new storage *)
    let new_size = width * height in
    let new_chars = Buf.make_int new_size in
    let new_attrs = Buf.make_int16 new_size in
    let new_links = Buf.make_int32 new_size in
    let new_fg_color = Buf.make_int new_size in
    let new_bg_color = Buf.make_int new_size in

    Buf.fill_cell new_chars space_cell;
    Buf.fill new_attrs 0;
    Buf.fill new_links no_link;
    Buf.fill new_fg_color default_fg_packed;
    Buf.fill new_bg_color default_bg_packed;

    (* Copy surviving content *)
    let copy_w = min old_w width in
    let copy_h = min old_h height in
    for y = 0 to copy_h - 1 do
      let src_off = y * old_w and dst_off = y * width in
      Buf.blit_cell_range old_chars ~src_pos:src_off new_chars ~dst_pos:dst_off
        ~len:copy_w;
      Buf.blit_range old_attrs ~src_pos:src_off new_attrs ~dst_pos:dst_off
        ~len:copy_w;
      Buf.blit_range old_links ~src_pos:src_off new_links ~dst_pos:dst_off
        ~len:copy_w;
      Buf.blit_range old_fg_color ~src_pos:src_off new_fg_color ~dst_pos:dst_off
        ~len:copy_w;
      Buf.blit_range old_bg_color ~src_pos:src_off new_bg_color ~dst_pos:dst_off
        ~len:copy_w
    done;

    t.width <- width;
    t.height <- height;
    t.chars <- new_chars;
    t.attrs <- new_attrs;
    t.links <- new_links;
    t.fg_color <- new_fg_color;
    t.bg_color <- new_bg_color;

    sanitize_spans t;
    rebuild_grapheme_tracker t

let resize_clear ?color t ~width ~height =
  if width <= 0 || height <= 0 then
    invalid_arg "Grid.resize_clear: width and height must be > 0";
  if width <> t.width || height <> t.height then (
    resize t ~width ~height;
    clear ?color t)

(* {1 Clipping} *)

let push_clip t rect = Scissor_stack.push t.scissor_stack rect
let pop_clip t = Scissor_stack.pop t.scissor_stack
let clear_clip t = Scissor_stack.clear t.scissor_stack

let clip t rect f =
  push_clip t rect;
  Fun.protect ~finally:(fun () -> pop_clip t) f

(* {1 Opacity stack} *)

let push_opacity t opacity =
  let clamped = Float.max 0.0 (Float.min 1.0 opacity) in
  Dynarray.add_last t.opacity_stack clamped;
  t.opacity_product <- t.opacity_product *. clamped

let pop_opacity t =
  match Dynarray.pop_last_opt t.opacity_stack with
  | None -> ()
  | Some _ ->
      let depth = Dynarray.length t.opacity_stack in
      if depth = 0 then t.opacity_product <- 1.0
      else
        let p = ref 1.0 in
        for i = 0 to depth - 1 do
          p := !p *. Dynarray.get t.opacity_stack i
        done;
        t.opacity_product <- !p

let current_opacity t = t.opacity_product

(* {1 Core cell writing} *)

(** Zero-alloc core cell writer. Takes packed colors plus unpacked channels so
    hot callers can avoid tuple allocation. *)
let set_cell_internal t ~idx ~code ~fg_color ~bg_color ~fg_r ~fg_g ~fg_b ~fg_a
    ~bg_r ~bg_g ~bg_b ~bg_a ~attrs ~link_id ~blending =
  (* Apply opacity stack *)
  let fg_a, bg_a, blending, fg_color, bg_color =
    if t.opacity_product < 1.0 then
      let fg_a = fg_a *. t.opacity_product in
      let bg_a = bg_a *. t.opacity_product in
      ( fg_a,
        bg_a,
        true,
        Color_packed.of_rgba_f fg_r fg_g fg_b fg_a,
        Color_packed.of_rgba_f bg_r bg_g bg_b bg_a )
    else (fg_a, bg_a, blending, fg_color, bg_color)
  in
  if blending && (bg_a < 0.999 || fg_a < 0.999) then begin
    (* Blending path *)
    let dst_bg = Buf.get t.bg_color idx in
    let dr_bg = Color_packed.red_f dst_bg in
    let dg_bg = Color_packed.green_f dst_bg in
    let db_bg = Color_packed.blue_f dst_bg in
    let da_bg = Color_packed.alpha_f dst_bg in

    let overlay_is_space = code = space_cell || code = null_cell in
    let dest_code = Buf.get_cell t.chars idx in
    let dest_has_content = dest_code <> null_cell && dest_code <> space_cell in
    let preserve =
      overlay_is_space && dest_has_content
      && Packed_cell.cell_width dest_code = 1
    in

    if preserve then begin
      (* Preserve existing cell; tint foreground. The overlay link always wins
         since the overlay is conceptually in front. *)
      Buf.set t.links idx link_id;
      if bg_a >= 0.999 then
        let dst_fg = Buf.get t.fg_color idx in
        Buf.set t.fg_color idx
          (Color_packed.of_rgba_f bg_r bg_g bg_b (Color_packed.alpha_f dst_fg))
      else if bg_a > 0.001 then
        let dst_fg = Buf.get t.fg_color idx in
        let dr_fg = Color_packed.red_f dst_fg in
        let dg_fg = Color_packed.green_f dst_fg in
        let db_fg = Color_packed.blue_f dst_fg in
        let da_fg = Color_packed.alpha_f dst_fg in
        let pa = Color_plane.perceptual_alpha bg_a in
        let r = Color_plane.mix bg_r dr_fg pa in
        let g = Color_plane.mix bg_g dg_fg pa in
        let b = Color_plane.mix bg_b db_fg pa in
        Buf.set t.fg_color idx (Color_packed.of_rgba_f r g b da_fg)
    end
    else begin
      (* Normal blended overwrite *)
      write_cell t idx code;
      Buf.set t.attrs idx attrs;
      Buf.set t.links idx link_id;
      if fg_a < 0.999 then
        let pa = Color_plane.perceptual_alpha fg_a in
        let r = Color_plane.mix fg_r dr_bg pa in
        let g = Color_plane.mix fg_g dg_bg pa in
        let b = Color_plane.mix fg_b db_bg pa in
        Buf.set t.fg_color idx (Color_packed.of_rgba_f r g b da_bg)
      else Buf.set t.fg_color idx fg_color
    end;

    (* Always blend BG over dest BG *)
    if bg_a <= 0.001 then ()
    else if bg_a >= 0.999 then Buf.set t.bg_color idx bg_color
    else
      let pa = Color_plane.perceptual_alpha bg_a in
      let r = Color_plane.mix bg_r dr_bg pa in
      let g = Color_plane.mix bg_g dg_bg pa in
      let b = Color_plane.mix bg_b db_bg pa in
      Buf.set t.bg_color idx (Color_packed.of_rgba_f r g b bg_a)
  end
  else begin
    (* Fast path: opaque overwrite *)
    write_cell t idx code;
    Buf.set t.attrs idx attrs;
    Buf.set t.links idx link_id;
    Buf.set t.fg_color idx fg_color;
    Buf.set t.bg_color idx bg_color
  end

(* Cell writes *)

let set_cell t ~x ~y ~(cell : Cell.t) ~fg ~bg ~attrs ?link
    ?(blend = t.respect_alpha) () =
  if x >= 0 && y >= 0 && x < t.width && y < t.height && not (is_clipped t x y)
  then
    let idx = (y * t.width) + x in
    let fg_r, fg_g, fg_b, fg_a = Ansi.Color.to_rgba_f fg in
    let bg_r, bg_g, bg_b, bg_a = Ansi.Color.to_rgba_f bg in
    let link_id = Links.intern t.link_registry link in
    let attrs_packed = Ansi.Attr.pack attrs in
    set_cell_internal t ~idx ~code:cell ~fg_color:(Color_packed.encode fg)
      ~bg_color:(Color_packed.encode bg) ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r ~bg_g
      ~bg_b ~bg_a ~attrs:attrs_packed ~link_id ~blending:blend

(* {1 Bulk operations} *)

let fill_opaque_rect t ~x0 ~y0 ~width ~height ~bg_color =
  if x0 = 0 && y0 = 0 && width = t.width && height = t.height then begin
    Grapheme_tracker.clear t.grapheme_tracker;
    Links.clear t.link_registry;
    Buf.fill_cell t.chars space_cell;
    Buf.fill t.attrs 0;
    Buf.fill t.links no_link;
    Buf.fill t.fg_color default_fg_packed;
    Buf.fill t.bg_color bg_color
  end
  else
    let y1 = y0 + height - 1 in
    for row = y0 to y1 do
      let start_idx = (row * t.width) + x0 in
      let end_idx = start_idx + width - 1 in
      for i = start_idx to end_idx do
        cleanup_overwritten_cell t i
      done;
      Buf.fill_cell_range t.chars ~pos:start_idx ~len:width space_cell;
      Buf.fill_range t.attrs ~pos:start_idx ~len:width 0;
      Buf.fill_range t.links ~pos:start_idx ~len:width no_link;
      Buf.fill_range t.fg_color ~pos:start_idx ~len:width default_fg_packed;
      Buf.fill_range t.bg_color ~pos:start_idx ~len:width bg_color
    done

let clear_rect ?color t ~x ~y ~width ~height =
  match clip_rect_to_grid t Rect.{ x; y; width; height } with
  | None -> ()
  | Some r ->
      let bg_color =
        Option.value color ~default:default_bg |> Color_packed.encode
      in
      fill_opaque_rect t ~x0:r.x ~y0:r.y ~width:r.width ~height:r.height
        ~bg_color

let fill_rect t ~x ~y ~width ~height ~color =
  match clip_rect_to_grid t Rect.{ x; y; width; height } with
  | None -> ()
  | Some r ->
      let bg_r, bg_g, bg_b, bg_a = Ansi.Color.to_rgba_f color in
      let bg_color = Color_packed.encode color in
      let x0 = r.Rect.x and w = r.Rect.width in
      let y0 = r.Rect.y in
      let y1 = y0 + r.Rect.height - 1 in

      if bg_a <= 0.001 then ()
      else if bg_a < 0.999 then
        (* Semi-transparent: per-cell alpha blending *)
        let bg_a =
          if t.opacity_product < 1.0 then bg_a *. t.opacity_product else bg_a
        in
        let pa = Color_plane.perceptual_alpha bg_a in
        for row = y0 to y1 do
          let base = (row * t.width) + x0 in
          for dx = 0 to w - 1 do
            let idx = base + dx in
            let dst_bg = Buf.get t.bg_color idx in
            let dr_bg = Color_packed.red_f dst_bg in
            let dg_bg = Color_packed.green_f dst_bg in
            let db_bg = Color_packed.blue_f dst_bg in
            let overlay_is_space = true in
            let dest_code = Buf.get_cell t.chars idx in
            let dest_has_content =
              dest_code <> null_cell && dest_code <> space_cell
            in
            let preserve =
              overlay_is_space && dest_has_content
              && Packed_cell.cell_width dest_code = 1
            in
            if preserve then begin
              Buf.set t.links idx no_link;
              let dst_fg = Buf.get t.fg_color idx in
              let dr_fg = Color_packed.red_f dst_fg in
              let dg_fg = Color_packed.green_f dst_fg in
              let db_fg = Color_packed.blue_f dst_fg in
              let da_fg = Color_packed.alpha_f dst_fg in
              Buf.set t.fg_color idx
                (Color_packed.of_rgba_f
                   (Color_plane.mix bg_r dr_fg pa)
                   (Color_plane.mix bg_g dg_fg pa)
                   (Color_plane.mix bg_b db_fg pa)
                   da_fg)
            end
            else begin
              write_cell t idx space_cell;
              Buf.set t.attrs idx 0;
              Buf.set t.links idx no_link;
              Buf.set t.fg_color idx default_fg_packed
            end;
            Buf.set t.bg_color idx
              (Color_packed.of_rgba_f
                 (Color_plane.mix bg_r dr_bg pa)
                 (Color_plane.mix bg_g dg_bg pa)
                 (Color_plane.mix bg_b db_bg pa)
                 bg_a)
          done
        done
      else begin
        (* Opaque: fast bulk fill *)
        fill_opaque_rect t ~x0 ~y0 ~width:w ~height:r.height ~bg_color
      end

let blit ~src ~dst =
  if src == dst then ()
  else (
    resize dst ~width:src.width ~height:src.height;
    dst.width_method <- src.width_method;
    dst.respect_alpha <- src.respect_alpha;
    Buf.blit src.fg_color dst.fg_color;
    Buf.blit src.bg_color dst.bg_color;
    Buf.blit src.attrs dst.attrs;

    if src.grapheme_store == dst.grapheme_store then (
      Buf.blit src.chars dst.chars;
      Buf.blit src.links dst.links;
      Links.copy_state ~src:src.link_registry ~dst:dst.link_registry;
      Grapheme_tracker.clear dst.grapheme_tracker;
      let len = src.width * src.height in
      for i = 0 to len - 1 do
        let c = Buf.get_cell dst.chars i in
        if Packed_cell.is_complex c then
          Grapheme_tracker.add dst.grapheme_tracker c
      done)
    else
      let copy_cell =
        make_cell_copier ~src_store:src.grapheme_store
          ~dst_store:dst.grapheme_store
      in
      Grapheme_tracker.clear dst.grapheme_tracker;
      Links.clear dst.link_registry;
      let len = src.width * src.height in
      for i = 0 to len - 1 do
        let src_c = Buf.get_cell src.chars i in
        let dst_c =
          if Packed_cell.is_inline src_c then src_c else copy_cell src_c
        in
        Buf.set_cell dst.chars i dst_c;
        if Packed_cell.is_complex dst_c then
          Grapheme_tracker.add dst.grapheme_tracker dst_c;

        let src_l = Buf.get src.links i in
        let dst_l =
          match Links.resolve src.link_registry src_l with
          | None -> no_link
          | Some url -> Links.intern dst.link_registry (Some url)
        in
        Buf.set dst.links i dst_l
      done)

let copy t =
  let dst =
    create ~width:t.width ~height:t.height ~width_method:t.width_method
      ~respect_alpha:t.respect_alpha ()
  in
  blit ~src:t ~dst;
  dst

(** Update grapheme tracker for a bulk row copy within the same grid. *)
let bulk_update_graphemes t ~src_idx ~dst_idx ~len =
  if Grapheme_tracker.unique_count t.grapheme_tracker > 0 then begin
    for i = 0 to len - 1 do
      let code = Buf.get_cell t.chars (dst_idx + i) in
      if not (Packed_cell.is_inline code) then
        Grapheme_tracker.remove t.grapheme_tracker code
    done;
    for i = 0 to len - 1 do
      let code = Buf.get_cell t.chars (src_idx + i) in
      if not (Packed_cell.is_inline code) then
        Grapheme_tracker.add t.grapheme_tracker code
    done
  end

let blit_region ~src ~dst ~src_x ~src_y ~width ~height ~dst_x ~dst_y =
  let width = max 0 width and height = max 0 height in

  (* Clamp negative source/destination offsets *)
  let src_x, dst_x, width =
    if src_x < 0 then (0, dst_x - src_x, width + src_x)
    else (src_x, dst_x, width)
  in
  let src_y, dst_y, height =
    if src_y < 0 then (0, dst_y - src_y, height + src_y)
    else (src_y, dst_y, height)
  in
  let src_x, dst_x, width =
    if dst_x < 0 then (src_x - dst_x, 0, width + dst_x)
    else (src_x, dst_x, width)
  in
  let src_y, dst_y, height =
    if dst_y < 0 then (src_y - dst_y, 0, height + dst_y)
    else (src_y, dst_y, height)
  in
  let width = min width (min (src.width - src_x) (dst.width - dst_x)) in
  let height = min height (min (src.height - src_y) (dst.height - dst_y)) in

  if width <= 0 || height <= 0 then ()
  else
    let target = Rect.{ x = dst_x; y = dst_y; width; height } in
    let clipped =
      match Scissor_stack.current dst.scissor_stack with
      | None -> Some target
      | Some scissor -> Rect.intersection target scissor
    in
    match clipped with
    | None -> ()
    | Some clipped ->
        let dst_x = clipped.x and dst_y = clipped.y in
        let width = clipped.width and height = clipped.height in
        let src_x = src_x + (dst_x - target.x) in
        let src_y = src_y + (dst_y - target.y) in
        let same_grid = src == dst in

        (* Row iteration direction for correct overlap handling *)
        let y_start, y_limit, y_step =
          if same_grid && src_y < dst_y then (height - 1, -1, -1)
          else (0, height, 1)
        in

        (* Full-width self-blit (scrolling) uses fast bulk copy *)
        if same_grid && src_x = 0 && dst_x = 0 && width = src.width then begin
          let si = src_y * src.width in
          let di = dst_y * dst.width in
          let len = width * height in
          bulk_update_graphemes src ~src_idx:si ~dst_idx:di ~len;
          Buf.blit
            (Bigarray.Array1.sub src.chars si len)
            (Bigarray.Array1.sub dst.chars di len);
          Buf.blit
            (Bigarray.Array1.sub src.attrs si len)
            (Bigarray.Array1.sub dst.attrs di len);
          Buf.blit
            (Bigarray.Array1.sub src.links si len)
            (Bigarray.Array1.sub dst.links di len);
          Buf.blit
            (Bigarray.Array1.sub src.fg_color si len)
            (Bigarray.Array1.sub dst.fg_color di len);
          Buf.blit
            (Bigarray.Array1.sub src.bg_color si len)
            (Bigarray.Array1.sub dst.bg_color di len)
        end
        else begin
          (* General blit: cell-by-cell with cross-store support *)
          let copy_cell =
            if same_grid || src.grapheme_store == dst.grapheme_store then Fun.id
            else
              make_cell_copier ~src_store:src.grapheme_store
                ~dst_store:dst.grapheme_store
          in

          let x_start, x_limit, x_step =
            if same_grid && src_x < dst_x then (width - 1, -1, -1)
            else (0, width, 1)
          in

          let i = ref y_start in
          while !i <> y_limit do
            let sy = src_y + !i and dy = dst_y + !i in
            let k = ref x_start in
            while !k <> x_limit do
              let sx = src_x + !k and dx = dst_x + !k in
              let sidx = (sy * src.width) + sx in

              let code = Buf.get_cell src.chars sidx in
              let fg_color = Buf.get src.fg_color sidx in
              let bg_color = Buf.get src.bg_color sidx in
              let fg_r = Color_packed.red_f fg_color in
              let fg_g = Color_packed.green_f fg_color in
              let fg_b = Color_packed.blue_f fg_color in
              let fg_a = Color_packed.alpha_f fg_color in
              let bg_r = Color_packed.red_f bg_color in
              let bg_g = Color_packed.green_f bg_color in
              let bg_b = Color_packed.blue_f bg_color in
              let bg_a = Color_packed.alpha_f bg_color in
              let attrs = Buf.get src.attrs sidx in
              let src_link = Buf.get src.links sidx in

              let link_id =
                match Links.resolve src.link_registry src_link with
                | Some url -> Links.intern dst.link_registry (Some url)
                | None -> no_link
              in

              (* Detect orphan continuations whose start cell is outside the
                 copied region *)
              let is_orphan =
                Packed_cell.is_continuation code
                && sx - Packed_cell.left_extent code < src_x
              in
              let is_right_truncated_start =
                (not (Packed_cell.is_continuation code))
                &&
                let cw = Packed_cell.cell_width code in
                cw > 1 && (sx + cw > src_x + width || dx + cw > dst_x + width)
              in

              let mapped_code =
                if is_orphan || is_right_truncated_start then space_cell
                else if
                  src.grapheme_store == dst.grapheme_store
                  || Packed_cell.is_inline code
                then code
                else copy_cell code
              in

              let is_reset = mapped_code = space_cell && code <> space_cell in
              let attrs = if is_reset then 0 else attrs in
              let link_id = if is_reset then no_link else link_id in
              let fg_r, fg_g, fg_b, fg_a =
                if is_reset then (1., 1., 1., 1.) else (fg_r, fg_g, fg_b, fg_a)
              in
              let fg_color = if is_reset then default_fg_packed else fg_color in

              (* A source framebuffer opts into composition with
                 [respect_alpha]. Plain framebuffers copy stored alpha values;
                 alpha-respecting framebuffers composite and fully transparent
                 cells become holes. *)
              let compose = (not same_grid) && src.respect_alpha in
              if compose && fg_a <= 0.001 && bg_a <= 0.001 then ()
              else
                set_cell_internal dst
                  ~idx:((dy * dst.width) + dx)
                  ~code:mapped_code ~fg_color ~bg_color ~fg_r ~fg_g ~fg_b ~fg_a
                  ~bg_r ~bg_g ~bg_b ~bg_a ~attrs ~link_id ~blending:compose;

              k := !k + x_step
            done;
            i := !i + y_step
          done
        end

(* {1 Scrolling} *)

let scroll t ~top ~bottom n =
  if n = 0 || top >= bottom then ()
  else
    let region_h = bottom - top + 1 in
    let abs_n = abs n in
    if abs_n >= region_h then
      clear_rect t ~x:0 ~y:top ~width:t.width ~height:region_h
    else
      let copy_h = region_h - abs_n in
      if n > 0 then (
        (* Scroll up: shift content up, blank at bottom *)
        blit_region ~src:t ~dst:t ~src_x:0 ~src_y:(top + abs_n) ~width:t.width
          ~height:copy_h ~dst_x:0 ~dst_y:top;
        clear_rect t ~x:0 ~y:(bottom - abs_n + 1) ~width:t.width ~height:abs_n)
      else (
        (* Scroll down: shift content down, blank at top *)
        blit_region ~src:t ~dst:t ~src_x:0 ~src_y:top ~width:t.width
          ~height:copy_h ~dst_x:0 ~dst_y:(top + abs_n);
        clear_rect t ~x:0 ~y:top ~width:t.width ~height:abs_n)

(* {1 Text rendering} *)

let draw_text ?style ?(tab_width = 2) t ~x ~y ~text =
  if text = "" || y < 0 || y >= t.height then ()
  else
    let s = Option.value style ~default:Ansi.Style.default in
    let fg_r, fg_g, fg_b, fg_a =
      match s.fg with
      | Some c -> Ansi.Color.to_rgba_f c
      | None -> (1., 1., 1., 1.)
    in
    let fg_color =
      Option.value s.fg ~default:default_fg |> Color_packed.encode
    in
    let explicit_bg =
      match s.bg with
      | Some c -> Some (Color_packed.encode c, Ansi.Color.to_rgba_f c)
      | None -> None
    in
    let attrs = Ansi.Attr.pack s.attrs in
    let link_id = Links.intern t.link_registry s.link in
    let cur_x = ref x in
    let tabw = if tab_width <= 0 then 2 else tab_width in
    let text =
      if String.contains text '\t' then (
        let len = String.length text in
        let b = Buffer.create (len + (tabw * 4)) in
        for i = 0 to len - 1 do
          let c = String.unsafe_get text i in
          if Char.equal c '\t' then
            for _ = 1 to tabw do
              Buffer.add_char b ' '
            done
          else Buffer.add_char b c
        done;
        Buffer.contents b)
      else text
    in

    let scissor = Scissor_stack.current t.scissor_stack in
    let row_visible =
      match scissor with
      | None -> true
      | Some r -> y >= r.y && y < r.y + r.height
    in
    if not row_visible then ()
    else
      let cell_visible =
        match scissor with
        | None -> fun _ -> true
        | Some r ->
            let x_min = r.x and x_max = r.x + r.width in
            fun x -> x >= x_min && x < x_max
      in
      let writer code =
        if !cur_x < t.width then
          let w = Packed_cell.cell_width code in
          if w > 0 then begin
            let bounds_ok = !cur_x + w <= t.width && !cur_x >= 0 in
            let start_visible = cell_visible !cur_x in

            if bounds_ok && start_visible then begin
              let idx = (y * t.width) + !cur_x in
              let br, bg, bb, ba =
                match explicit_bg with
                | Some (_, (r, g, b, a)) -> (r, g, b, a)
                | None ->
                    let bg = Buf.get t.bg_color idx in
                    ( Color_packed.red_f bg,
                      Color_packed.green_f bg,
                      Color_packed.blue_f bg,
                      Color_packed.alpha_f bg )
              in
              let bg_color =
                match explicit_bg with
                | Some (color, _) -> color
                | None -> Buf.get t.bg_color idx
              in
              let blending = fg_a < 0.999 || ba < 0.999 || t.respect_alpha in
              set_cell_internal t ~idx ~code ~fg_color ~bg_color ~fg_r ~fg_g
                ~fg_b ~fg_a ~bg_r:br ~bg_g:bg ~bg_b:bb ~bg_a:ba ~attrs ~link_id
                ~blending;
              for i = 1 to w - 1 do
                let c_x = !cur_x + i in
                let c_idx = (y * t.width) + c_x in
                let br_c, bg_c, bb_c, ba_c =
                  match explicit_bg with
                  | Some (_, (r, g, b, a)) -> (r, g, b, a)
                  | None ->
                      let bg = Buf.get t.bg_color c_idx in
                      ( Color_packed.red_f bg,
                        Color_packed.green_f bg,
                        Color_packed.blue_f bg,
                        Color_packed.alpha_f bg )
                in
                let bg_color_c =
                  match explicit_bg with
                  | Some (color, _) -> color
                  | None -> Buf.get t.bg_color c_idx
                in
                let cont =
                  Packed_cell.make_continuation ~code ~left:i ~right:(w - 1 - i)
                in
                let blending_c =
                  fg_a < 0.999 || ba_c < 0.999 || t.respect_alpha
                in
                set_cell_internal t ~idx:c_idx ~code:cont ~fg_color
                  ~bg_color:bg_color_c ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r:br_c
                  ~bg_g:bg_c ~bg_b:bb_c ~bg_a:ba_c ~attrs ~link_id
                  ~blending:blending_c
              done
            end
            else if (not bounds_ok) && !cur_x >= 0 && !cur_x < t.width then
              (* Wide cell overflows: fill remainder with styled spaces *)
              for x_fill = !cur_x to t.width - 1 do
                if cell_visible x_fill then
                  let idx = (y * t.width) + x_fill in
                  let br, bg, bb, ba =
                    match explicit_bg with
                    | Some (_, (r, g, b, a)) -> (r, g, b, a)
                    | None ->
                        let bg = Buf.get t.bg_color idx in
                        ( Color_packed.red_f bg,
                          Color_packed.green_f bg,
                          Color_packed.blue_f bg,
                          Color_packed.alpha_f bg )
                  in
                  let bg_color =
                    match explicit_bg with
                    | Some (color, _) -> color
                    | None -> Buf.get t.bg_color idx
                  in
                  set_cell_internal t ~idx ~code:space_cell ~fg_color ~bg_color
                    ~fg_r ~fg_g ~fg_b ~fg_a ~bg_r:br ~bg_g:bg ~bg_b:bb ~bg_a:ba
                    ~attrs ~link_id ~blending:false
              done;

            cur_x := !cur_x + w
          end
      in

      let stop = ref false in
      try
        Text.iter_grapheme_info ~width_method:t.width_method ~tab_width:tabw
          (fun ~offset ~len ~width:w ->
            if !stop || w <= 0 then ()
            else
              let start_x = !cur_x in
              let end_x = start_x + w in
              if end_x <= 0 then cur_x := end_x
              else if start_x >= t.width then (
                stop := true;
                raise Exit)
              else
                let g =
                  Packed_cell.intern_sub t.grapheme_store
                    ~width_method:t.width_method ~tab_width:tabw text
                    ~pos:offset ~len ~width:w
                in
                if not (Packed_cell.is_continuation g) then writer g)
          text
      with Exit -> ()

(* {1 Box drawing} *)

let draw_box t ~x ~y ~width ~height ?(border = Border.single)
    ?(sides = Border.all) ?(style = Ansi.Style.default) ?fill ?title
    ?title_alignment ?title_style () =
  if width <= 0 || height <= 0 then ()
  else
    let target = Rect.{ x; y; width; height } in
    let scissor_ok =
      match Scissor_stack.current t.scissor_stack with
      | None -> true
      | Some scissor -> Option.is_some (Rect.intersection target scissor)
    in
    if not scissor_ok then ()
    else begin
      let border_chars = border in
      let should_fill = Option.is_some fill in
      let bg_color =
        match fill with
        | Some c -> c
        | None -> (
            match style.bg with Some c -> c | None -> Ansi.Color.default)
      in
      let bg_color_packed = Color_packed.encode bg_color in
      let open Border in
      let has side = List.mem side sides in
      let sx = max 0 x and sy = max 0 y in
      let ex = min (t.width - 1) (x + width - 1) in
      let ey = min (t.height - 1) (y + height - 1) in

      (* Fill interior *)
      if should_fill then begin
        let has_l = has `Left and has_r = has `Right in
        let has_t = has `Top and has_b = has `Bottom in
        let ix = if has_l then x + 1 else x in
        let iy = if has_t then y + 1 else y in
        let iw = width - (if has_l then 1 else 0) - if has_r then 1 else 0 in
        let ih = height - (if has_t then 1 else 0) - if has_b then 1 else 0 in
        if iw > 0 && ih > 0 then
          fill_rect t ~x:ix ~y:iy ~width:iw ~height:ih ~color:bg_color
      end;

      let b_fg_r, b_fg_g, b_fg_b, b_fg_a =
        match style.Ansi.Style.fg with
        | Some c -> Ansi.Color.to_rgba_f c
        | None -> (1., 1., 1., 1.)
      in
      let b_fg_color =
        Option.value style.Ansi.Style.fg ~default:default_fg
        |> Color_packed.encode
      in
      let b_bg_r, b_bg_g, b_bg_b, b_bg_a = Ansi.Color.to_rgba_f bg_color in
      let b_attrs = Ansi.Attr.pack style.attrs in

      let draw_b bx by uch =
        if
          bx >= 0 && by >= 0 && bx < t.width && by < t.height
          && not (is_clipped t bx by)
        then
          let cell = Packed_cell.of_uchar uch in
          set_cell_internal t
            ~idx:((by * t.width) + bx)
            ~code:cell ~fg_color:b_fg_color ~bg_color:bg_color_packed
            ~fg_r:b_fg_r ~fg_g:b_fg_g ~fg_b:b_fg_b ~fg_a:b_fg_a ~bg_r:b_bg_r
            ~bg_g:b_bg_g ~bg_b:b_bg_b ~bg_a:b_bg_a ~attrs:b_attrs
            ~link_id:no_link ~blending:true
      in

      (* Horizontal borders *)
      if has `Top || has `Bottom then begin
        let row y_pos top =
          if y_pos >= 0 && y_pos < t.height then
            for c = sx to ex do
              if c >= x && c < x + width then
                let ch =
                  if c = x then
                    if has `Left then
                      if top then border_chars.top_left
                      else border_chars.bottom_left
                    else border_chars.horizontal
                  else if c = x + width - 1 then
                    if has `Right then
                      if top then border_chars.top_right
                      else border_chars.bottom_right
                    else border_chars.horizontal
                  else border_chars.horizontal
                in
                draw_b c y_pos ch
            done
        in
        if has `Top then row y true;
        if has `Bottom then row (y + height - 1) false
      end;

      (* Vertical borders — extend to corners when only one horizontal side or
         neither is present *)
      let left_only = has `Left && (not (has `Top)) && not (has `Bottom) in
      let right_only = has `Right && (not (has `Top)) && not (has `Bottom) in
      let bottom_with_verts =
        has `Bottom && (not (has `Top)) && (has `Left || has `Right)
      in
      let top_with_verts =
        has `Top && (not (has `Bottom)) && (has `Left || has `Right)
      in
      let extend_top = left_only || right_only || bottom_with_verts in
      let extend_bottom = left_only || right_only || top_with_verts in
      let vy_start = max sy (if extend_top then y else y + 1) in
      let vy_end =
        min ey (if extend_bottom then y + height - 1 else y + height - 2)
      in

      if vy_start <= vy_end then begin
        if has `Left && x >= 0 && x < t.width then
          for r = vy_start to vy_end do
            draw_b x r border_chars.vertical
          done;
        if has `Right then
          let rx = x + width - 1 in
          if rx >= 0 && rx < t.width then
            for r = vy_start to vy_end do
              draw_b rx r border_chars.vertical
            done
      end;

      (* Title *)
      match title with
      | Some txt when has `Top && width >= 4 ->
          let w = Text.measure ~width_method:t.width_method ~tab_width:2 txt in
          if w <= width - 4 then
            let pad =
              match title_alignment with
              | Some `Right -> width - 2 - w
              | Some `Center -> (width - w) / 2
              | _ -> 2
            in
            let title_s =
              match title_style with
              | Some s -> Ansi.Style.bg bg_color s
              | None -> Ansi.Style.make ?fg:style.fg ~bg:bg_color ()
            in
            draw_text t ~x:(x + pad) ~y ~text:txt ~style:title_s
      | _ -> ()
    end

(* {1 Line drawing} *)

type line_symbols = {
  h : string;
  v : string;
  diag_up : string;
  diag_down : string;
}

let default_line_symbols = { h = "─"; v = "│"; diag_up = "╱"; diag_down = "╲" }

let ascii_line_symbols = { h = "-"; v = "|"; diag_up = "/"; diag_down = "\\" }

(* Precomputed braille lookup table: all 256 patterns as UTF-8 strings. *)
let braille_lut =
  Array.init 256 (fun bits ->
      let b = Buffer.create 4 in
      Buffer.add_utf_8_uchar b (Uchar.of_int (0x2800 + bits));
      Buffer.contents b)

let braille_base = 0x2800
let braille_max = 0x28FF

(** Decode an existing braille cell to its bit pattern, 0 for non-braille. *)
let decode_braille_bits t ~x ~y =
  if x < 0 || y < 0 || x >= t.width || y >= t.height then 0
  else
    let idx = (y * t.width) + x in
    let code = Buf.get_cell t.chars idx in
    let cp =
      if Packed_cell.is_inline code then Packed_cell.codepoint code
      else if Packed_cell.is_start code then
        let s = Packed_cell.to_string t.grapheme_store code in
        if String.length s = 3 then
          let b0 = Char.code (String.unsafe_get s 0) in
          let b1 = Char.code (String.unsafe_get s 1) in
          let b2 = Char.code (String.unsafe_get s 2) in
          if b0 land 0xF0 = 0xE0 && b1 land 0xC0 = 0x80 && b2 land 0xC0 = 0x80
          then
            ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F)
          else 0
        else 0
      else 0
    in
    if cp >= braille_base && cp <= braille_max then cp - braille_base else 0

let[@inline] braille_bit_pos bit_x bit_y =
  match (bit_x, bit_y) with
  | 0, 0 -> 0
  | 0, 1 -> 1
  | 0, 2 -> 2
  | 0, 3 -> 6
  | 1, 0 -> 3
  | 1, 1 -> 4
  | 1, 2 -> 5
  | 1, 3 -> 7
  | _ -> 0

let draw_line t ~x1 ~y1 ~x2 ~y2 ?(style = Ansi.Style.default)
    ?(symbols = default_line_symbols) ?(kind = `Line) () =
  let dx = abs (x2 - x1) and dy = abs (y2 - y1) in
  let sx = if x1 < x2 then 1 else -1 in
  let sy = if y1 < y2 then 1 else -1 in

  match kind with
  | `Line ->
      let diag_cell =
        if (x2 - x1) * (y2 - y1) > 0 then symbols.diag_down else symbols.diag_up
      in
      let x = ref x1 and y = ref y1 and err = ref (dx - dy) in
      while not (!x = x2 && !y = y2) do
        let e2 = 2 * !err in
        let move_x = e2 > -dy and move_y = e2 < dx in
        let cell =
          if move_x && move_y then diag_cell
          else if move_x then symbols.h
          else symbols.v
        in
        draw_text t ~x:!x ~y:!y ~text:cell ~style;
        if move_x then (
          x := !x + sx;
          err := !err - dy);
        if move_y then (
          y := !y + sy;
          err := !err + dx)
      done;
      let final_cell =
        if dx = 0 && dy = 0 then symbols.h
        else if dx = 0 then symbols.v
        else if dy = 0 then symbols.h
        else diag_cell
      in
      draw_text t ~x:!x ~y:!y ~text:final_cell ~style
  | `Braille ->
      let buffer = Hashtbl.create 32 in
      let set_dot px py =
        let cell_x = if px >= 0 then px / 2 else ((px + 1) / 2) - 1 in
        let cell_y = if py >= 0 then py / 4 else ((py + 1) / 4) - 1 in
        if cell_x >= 0 && cell_x < t.width && cell_y >= 0 && cell_y < t.height
        then
          let bit_x = ((px mod 2) + 2) mod 2 in
          let bit_y = ((py mod 4) + 4) mod 4 in
          let key = (cell_x, cell_y) in
          let current = Option.value (Hashtbl.find_opt buffer key) ~default:0 in
          Hashtbl.replace buffer key
            (current lor (1 lsl braille_bit_pos bit_x bit_y))
      in
      let x = ref x1 and y = ref y1 and err = ref (dx - dy) in
      while not (!x = x2 && !y = y2) do
        set_dot !x !y;
        let e2 = 2 * !err in
        if e2 > -dy then (
          x := !x + sx;
          err := !err - dy);
        if e2 < dx then (
          y := !y + sy;
          err := !err + dx)
      done;
      set_dot x2 y2;
      Hashtbl.iter
        (fun (cx, cy) bits ->
          let existing = decode_braille_bits t ~x:cx ~y:cy in
          draw_text t ~x:cx ~y:cy ~text:braille_lut.(existing lor bits) ~style)
        buffer

(* {1 Inspection & utilities} *)

let active_height t =
  let rec find_row y =
    if y < 0 then 0
    else
      let row_start = y * t.width in
      let rec has_content x =
        if x >= t.width then false
        else
          let code = Buf.get_cell t.chars (row_start + x) in
          if code <> space_cell && code <> null_cell then true
          else has_content (x + 1)
      in
      if has_content 0 then y + 1 else find_row (y - 1)
  in
  find_row (t.height - 1)

let diff_cells prev curr =
  let max_w = max prev.width curr.width in
  let max_h = max prev.height curr.height in
  let diffs = Dynarray.create () in
  for y = 0 to max_h - 1 do
    for x = 0 to max_w - 1 do
      let in_prev = x < prev.width && y < prev.height in
      let in_curr = x < curr.width && y < curr.height in
      match (in_prev, in_curr) with
      | false, false -> ()
      | true, false | false, true -> Dynarray.add_last diffs (x, y)
      | true, true ->
          let p_idx = (y * prev.width) + x in
          let c_idx = (y * curr.width) + x in
          if
            Buf.get_cell prev.chars p_idx <> Buf.get_cell curr.chars c_idx
            || Buf.get prev.attrs p_idx <> Buf.get curr.attrs c_idx
            || Buf.get prev.fg_color p_idx <> Buf.get curr.fg_color c_idx
            || Buf.get prev.bg_color p_idx <> Buf.get curr.bg_color c_idx
            || Buf.get prev.links p_idx <> Buf.get curr.links c_idx
          then Dynarray.add_last diffs (x, y)
    done
  done;
  Dynarray.to_array diffs

(* {1 Serialization} *)

let to_ansi ?(reset = true) t =
  let width = t.width and height = t.height in
  if width <= 0 || height <= 0 then ""
  else
    let store = t.grapheme_store in
    let scratch = ref (Bytes.create 256) in

    Ansi.to_string (fun w ->
        let style = Ansi.Sgr_state.create () in

        for y = 0 to height - 1 do
          if y > 0 then Ansi.emit (Ansi.char '\n') w;

          let x = ref 0 in
          while !x < width do
            let idx = (y * width) + !x in
            let code = get_cell t idx in
            let cw = Packed_cell.cell_width code in
            let step = if cw <= 0 then 1 else cw in

            Ansi.Sgr_state.update style w ~fg:(get_fg t idx) ~bg:(get_bg t idx)
              ~attrs:(get_attrs t idx)
              ~link:(hyperlink_url_direct t (get_link t idx));

            begin if
              cw <= 0
              || Packed_cell.is_continuation code
              || Packed_cell.is_empty code
            then Ansi.emit (Ansi.char ' ') w
            else
              let len = Packed_cell.length store code in
              if len <= 0 then Ansi.emit (Ansi.char ' ') w
              else begin
                if len > Bytes.length !scratch then
                  scratch := Bytes.create (max (Bytes.length !scratch * 2) len);
                let written = Packed_cell.blit store code !scratch ~pos:0 in
                Ansi.emit (Ansi.bytes !scratch ~off:0 ~len:written) w
              end
            end;

            x := !x + step
          done;

          Ansi.Sgr_state.close_link style w;
          Ansi.emit Ansi.reset w;
          Ansi.Sgr_state.reset style
        done;

        if reset then Ansi.emit Ansi.reset w)
