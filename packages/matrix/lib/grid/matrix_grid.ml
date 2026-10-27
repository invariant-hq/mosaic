(* grid.ml *)

module Text = Matrix_text
module Color_packed = Ansi.Color.Packed

(* {1 Internal modules} *)

(* The cell planes. Each accessor names its plane's element type, so it
   compiles to a load or a store; an accessor polymorphic in the element type
   calls into the runtime instead. *)
module Buf = struct
  type ints = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

  type shorts =
    (int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  let ints len : ints =
    Bigarray.Array1.create Bigarray.int Bigarray.c_layout len

  let shorts len : shorts =
    Bigarray.Array1.create Bigarray.int16_unsigned Bigarray.c_layout len

  let[@inline] get (a : ints) i = Bigarray.Array1.unsafe_get a i
  let[@inline] set (a : ints) i v = Bigarray.Array1.unsafe_set a i v
  let[@inline] get_short (a : shorts) i = Bigarray.Array1.unsafe_get a i
  let[@inline] set_short (a : shorts) i v = Bigarray.Array1.unsafe_set a i v
  let[@inline] get_cell a i = Packed_cell.unsafe_of_int (get a i)
  let[@inline] set_cell a i (v : Packed_cell.t) = set a i (Packed_cell.to_int v)
  let fill = Bigarray.Array1.fill
  let blit = Bigarray.Array1.blit
  let dim = Bigarray.Array1.dim
  let fill_cell a (v : Packed_cell.t) = fill a (Packed_cell.to_int v)

  (* Copies run backwards when the ranges overlap with the destination after
     the source. *)
  let[@inline] blit_range src ~src_pos dst ~dst_pos ~len =
    if src == dst && src_pos < dst_pos then
      for i = len - 1 downto 0 do
        set dst (dst_pos + i) (get src (src_pos + i))
      done
    else
      for i = 0 to len - 1 do
        set dst (dst_pos + i) (get src (src_pos + i))
      done

  let[@inline] blit_short_range src ~src_pos dst ~dst_pos ~len =
    if src == dst && src_pos < dst_pos then
      for i = len - 1 downto 0 do
        set_short dst (dst_pos + i) (get_short src (src_pos + i))
      done
    else
      for i = 0 to len - 1 do
        set_short dst (dst_pos + i) (get_short src (src_pos + i))
      done

  let[@inline] fill_range a ~pos ~len v =
    for i = pos to pos + len - 1 do
      set a i v
    done

  let[@inline] fill_short_range a ~pos ~len v =
    for i = pos to pos + len - 1 do
      set_short a i v
    done
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

(* {1 Public types} *)

module Border = Border
module Lines = Lines

type region = Rect.t = { x : int; y : int; width : int; height : int }

type t = {
  mutable width : int;
  mutable height : int;
  grapheme_store : Grapheme_store.t;
  mutable width_method : Text.width_method;
  mutable chars : Buf.ints;
  mutable fg_color : Buf.ints;
  mutable bg_color : Buf.ints;
  mutable attrs : Buf.shorts;
  mutable links : string array;
  grapheme_tracker : Grapheme_tracker.t;
  scissor_stack : Scissor_stack.t;
  opacity_stack : float Dynarray.t;
  mutable opacity_product : float;
}

(* {1 Constants} *)

let space_cell = Packed_cell.space
let null_cell = Packed_cell.empty
let no_link = ""
let default_fg = Ansi.Color.default_fg
let default_bg = Ansi.Color.transparent
let default_fg_packed = Color_packed.encode default_fg
let default_bg_packed = Color_packed.encode default_bg
let terminal_bg_packed = Color_packed.encode Ansi.Color.default_bg

(* {1 Helpers} *)

(* Most grids never contain links. Retain a plane only after the first URL,
   keeping unlinked clears and scrolls free of GC-scanned array writes. *)
let[@inline] get_link t idx =
  if Array.length t.links = 0 then no_link else Array.unsafe_get t.links idx

let ensure_links t =
  if Array.length t.links = 0 then
    t.links <- Array.make (t.width * t.height) no_link

let[@inline] set_link t idx link =
  if Array.length t.links <> 0 then Array.unsafe_set t.links idx link
  else if link <> no_link then (
    ensure_links t;
    Array.unsafe_set t.links idx link)

let clear_links t pos len =
  if Array.length t.links <> 0 then Array.fill t.links pos len no_link

let blit_links ~src ~dst src_pos dst_pos len =
  if Array.length src.links = 0 then clear_links dst dst_pos len
  else (
    ensure_links dst;
    Array.blit src.links src_pos dst.links dst_pos len)

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

(* Blanks. A blank is a space with the default foreground, no attributes and
   no link, on a background chosen by the operation that makes it. *)

(** Write a blank on [bg] into a cell, leaving the grapheme tracker alone. *)
let[@inline] write_blank t idx bg =
  Buf.set_cell t.chars idx space_cell;
  Buf.set_short t.attrs idx 0;
  set_link t idx no_link;
  Buf.set t.fg_color idx default_fg_packed;
  Buf.set t.bg_color idx bg

(** Turn a cell into a blank on [bg], releasing its grapheme tracker entry. *)
let[@inline] blank_cell t idx bg =
  let code = Buf.get_cell t.chars idx in
  if code <> space_cell then Grapheme_tracker.remove t.grapheme_tracker code;
  write_blank t idx bg

(* The distances from [idx] to the first and last cells of its grapheme. *)
let[@inline] extent_left code =
  if Packed_cell.is_inline code then 0 else Packed_cell.left_extent code

let[@inline] extent_right code =
  if Packed_cell.is_inline code then max 0 (Packed_cell.cell_width code - 1)
  else Packed_cell.right_extent code

(** Blank the other cells of the grapheme at [idx] on [bg], so that overwriting
    [idx] leaves no part of it. *)
let cleanup_grapheme_at t idx bg =
  let code = Buf.get_cell t.chars idx in
  let start_idx = idx - extent_left code
  and end_idx = idx + extent_right code in
  if start_idx >= 0 && end_idx < Buf.dim t.chars then
    for i = start_idx to end_idx do
      if i <> idx then blank_cell t i bg
    done

(** Blank every cell of the grapheme at [idx] on [bg]. *)
let blank_grapheme t idx bg =
  cleanup_grapheme_at t idx bg;
  blank_cell t idx bg

let cleanup_overwritten_cell t idx bg =
  let old = Buf.get_cell t.chars idx in
  if old <> space_cell then begin
    if (not (Packed_cell.is_inline old)) || Packed_cell.cell_width old > 1 then
      cleanup_grapheme_at t idx bg;
    Grapheme_tracker.remove t.grapheme_tracker old
  end

(** Write a cell code into a cell, blanking the rest of the grapheme it
    overwrites on [bg] and maintaining grapheme tracker state. *)
let[@inline] write_cell t idx code bg =
  let old = Buf.get_cell t.chars idx in
  if old <> code then begin
    (* [code] is counted before the old grapheme's cells are released: it may
       name the slot they hold, which their release would otherwise free. *)
    if not (Packed_cell.is_inline code) then
      Grapheme_tracker.add t.grapheme_tracker code;
    if (not (Packed_cell.is_inline old)) || Packed_cell.cell_width old > 1 then
      cleanup_grapheme_at t idx bg;
    Grapheme_tracker.remove t.grapheme_tracker old;
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
        start_cell

(* {1 Creation & lifecycle} *)

let fill_defaults t =
  Grapheme_tracker.clear t.grapheme_tracker;
  Scissor_stack.clear t.scissor_stack;
  Buf.fill_cell t.chars space_cell;
  Buf.fill t.attrs 0;
  clear_links t 0 (t.width * t.height);
  Buf.fill t.fg_color default_fg_packed;
  Buf.fill t.bg_color default_bg_packed

let create_with_store ~width ~height ~grapheme_store ~width_method =
  if width <= 0 || height <= 0 then
    invalid_arg "Matrix_grid.create: width and height must be > 0";
  if width > Packed_cell.max_width then
    invalid_arg "Matrix_grid.create: width exceeds the maximum";
  let size = width * height in
  let t =
    {
      width;
      height;
      grapheme_store;
      width_method;
      chars = Buf.ints size;
      fg_color = Buf.ints size;
      bg_color = Buf.ints size;
      attrs = Buf.shorts size;
      links = [||];
      grapheme_tracker = Grapheme_tracker.create grapheme_store;
      scissor_stack = Scissor_stack.create ();
      opacity_stack = Dynarray.create ();
      opacity_product = 1.0;
    }
  in
  fill_defaults t;
  t

let create ~width ~height ?width_method () =
  create_with_store ~width ~height ~grapheme_store:(Grapheme_store.create ())
    ~width_method:(Option.value width_method ~default:`Unicode)

let create_like g ~width ~height =
  create_with_store ~width ~height ~grapheme_store:g.grapheme_store
    ~width_method:g.width_method

(* {1 Accessors} *)

let max_width = Packed_cell.max_width
let width t = t.width
let height t = t.height
let width_method t = t.width_method
let set_width_method t m = t.width_method <- m

(* {1 Cell accessors} *)

let[@inline] idx t ~x ~y = (y * t.width) + x

let[@inline] get_codepoint t idx =
  let code = Buf.get_cell t.chars idx in
  if Packed_cell.is_inline code then Packed_cell.codepoint code else -1

(* The attrs plane holds Ansi.Attr's bits and, above them, DEC's
   selective-erase attribute, which no reader of attributes sees. The plane
   has 16 bits. *)
let protected_bit = 1 lsl Ansi.Attr.bits
let attrs_mask = protected_bit - 1
let () = assert (protected_bit <= 0x8000)
let[@inline] get_attrs t idx = Buf.get_short t.attrs idx land attrs_mask

let[@inline] is_protected t idx =
  Buf.get_short t.attrs idx land protected_bit <> 0

let get_style t idx =
  let attrs = Ansi.Attr.unpack (get_attrs t idx) in
  let url = get_link t idx in
  let link = if url = no_link then None else Some url in
  Ansi.Style.make ~fg:(get_fg t idx) ~bg:(get_bg t idx) ?link ()
  |> Ansi.Style.with_attrs attrs

(* A cell has text when it is neither empty nor a continuation. *)
let[@inline] has_text c =
  not (c = Packed_cell.empty || Packed_cell.is_continuation c)

let get_text t idx =
  let c = Buf.get_cell t.chars idx in
  if has_text c then Packed_cell.to_string t.grapheme_store c else ""

let cell_text_length t idx =
  let c = Buf.get_cell t.chars idx in
  if has_text c then Packed_cell.length t.grapheme_store c else 0

let blit_cell_text t idx buf ~pos =
  if pos < 0 || pos > Bytes.length buf then
    invalid_arg "Matrix_grid.blit_cell_text: position out of bounds";
  let c = Buf.get_cell t.chars idx in
  if has_text c then Packed_cell.blit t.grapheme_store c buf ~pos else 0

let is_empty t idx = Buf.get_cell t.chars idx = Packed_cell.empty

let is_continuation t idx =
  Packed_cell.is_continuation (Buf.get_cell t.chars idx)

let cell_width t idx = Packed_cell.cell_width (Buf.get_cell t.chars idx)

let extent t idx =
  if idx < 0 || idx >= t.width * t.height then
    invalid_arg "Matrix_grid.extent: index outside the grid";
  let code = Buf.get_cell t.chars idx in
  let left = extent_left code in
  (idx - left, left + extent_right code + 1)

let[@inline] cells_equal t1 idx1 t2 idx2 =
  let same_graphemes = t1.grapheme_store == t2.grapheme_store in
  let c1 = Buf.get_cell t1.chars idx1 and c2 = Buf.get_cell t2.chars idx2 in
  (if same_graphemes then c1 = c2
   else Packed_cell.equal t1.grapheme_store c1 t2.grapheme_store c2)
  && get_attrs t1 idx1 = get_attrs t2 idx2
  && String.equal (get_link t1 idx1) (get_link t2 idx2)
  && Buf.get t1.fg_color idx1 = Buf.get t2.fg_color idx2
  && Buf.get t1.bg_color idx1 = Buf.get t2.bg_color idx2

(* The cells' graphemes are counted afresh before the old counts release their
   references: the store frees a grapheme whose count reaches zero, and the
   cells still name it. The grid keeps its tracker, so a resize drops none for
   the GC to finalise. *)
let rebuild_grapheme_tracker t =
  let tracker = t.grapheme_tracker in
  let old = Grapheme_tracker.start_recount tracker in
  let len = t.width * t.height in
  for i = 0 to len - 1 do
    let code = Buf.get_cell t.chars i in
    if Packed_cell.is_complex code then Grapheme_tracker.add tracker code
  done;
  Grapheme_tracker.finish_recount tracker old

(* Blank, on the default background, the cells of extents the resize cut. The
   tracker is rebuilt from the cells afterwards. *)
let sanitize_extents t =
  for y = 0 to t.height - 1 do
    let row_start = y * t.width in
    let covered_until = ref (-1) in
    for x = 0 to t.width - 1 do
      let idx = row_start + x in
      let code = Buf.get_cell t.chars idx in
      if Packed_cell.is_continuation code then
        begin if x > !covered_until then write_blank t idx default_bg_packed
        end
      else
        let w = Packed_cell.cell_width code in
        if w > 1 then
          if x + w > t.width then write_blank t idx default_bg_packed
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
              write_blank t idx default_bg_packed;
              for dx = 1 to w - 1 do
                write_blank t (idx + dx) default_bg_packed
              done
            end
          end
    done
  done

(* {1 Lifecycle: clear & resize} *)

let clear ?color t =
  Grapheme_tracker.clear t.grapheme_tracker;
  Buf.fill_cell t.chars space_cell;
  Buf.fill t.attrs 0;
  clear_links t 0 (t.width * t.height);
  Buf.fill t.fg_color default_fg_packed;
  let bg_color =
    Option.value color ~default:default_bg |> Color_packed.encode
  in
  Buf.fill t.bg_color bg_color

let resize t ~width ~height =
  if width <= 0 || height <= 0 then
    invalid_arg "Matrix_grid.resize: width and height must be > 0";
  if width > Packed_cell.max_width then
    invalid_arg "Matrix_grid.resize: width exceeds the maximum";
  if width = t.width && height = t.height then ()
  else
    let old_w = t.width and old_h = t.height in
    let old_chars = t.chars and old_attrs = t.attrs and old_links = t.links in
    let old_fg_color = t.fg_color and old_bg_color = t.bg_color in

    (* Allocate new storage *)
    let new_size = width * height in
    let new_chars = Buf.ints new_size in
    let new_attrs = Buf.shorts new_size in
    let new_links =
      if Array.length old_links = 0 then [||] else Array.make new_size no_link
    in
    let new_fg_color = Buf.ints new_size in
    let new_bg_color = Buf.ints new_size in

    Buf.fill_cell new_chars space_cell;
    Buf.fill new_attrs 0;
    Buf.fill new_fg_color default_fg_packed;
    Buf.fill new_bg_color default_bg_packed;

    (* Copy surviving content *)
    let copy_w = min old_w width in
    let copy_h = min old_h height in
    for y = 0 to copy_h - 1 do
      let src_off = y * old_w and dst_off = y * width in
      Buf.blit_range old_chars ~src_pos:src_off new_chars ~dst_pos:dst_off
        ~len:copy_w;
      Buf.blit_short_range old_attrs ~src_pos:src_off new_attrs ~dst_pos:dst_off
        ~len:copy_w;
      if Array.length old_links <> 0 then
        Array.blit old_links src_off new_links dst_off copy_w;
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

    sanitize_extents t;
    rebuild_grapheme_tracker t

let resize_clear ?color t ~width ~height =
  if width <= 0 || height <= 0 then
    invalid_arg "Matrix_grid.resize_clear: width and height must be > 0";
  if width > Packed_cell.max_width then
    invalid_arg "Matrix_grid.resize_clear: width exceeds the maximum";
  if width <> t.width || height <> t.height then (
    resize t ~width ~height;
    clear ?color t)

(* {1 Clipping} *)

let push_clip t rect = Scissor_stack.push t.scissor_stack rect
let pop_clip t = Scissor_stack.pop t.scissor_stack
let clear_clip t = Scissor_stack.clear t.scissor_stack

let clip_region t =
  match Scissor_stack.current t.scissor_stack with
  | None -> { x = 0; y = 0; width = t.width; height = t.height }
  | Some r ->
      let x = max 0 r.x and y = max 0 r.y in
      {
        x;
        y;
        width = max 0 (min t.width (r.x + r.width) - x);
        height = max 0 (min t.height (r.y + r.height) - y);
      }

let intersects_clip t (r : region) =
  if r.width <= 0 || r.height <= 0 then false
  else
    let x0 = max 0 r.x in
    let y0 = max 0 r.y in
    let x1 = min t.width (r.x + r.width) in
    let y1 = min t.height (r.y + r.height) in
    if x0 >= x1 || y0 >= y1 then false
    else
      match Scissor_stack.current t.scissor_stack with
      | None -> true
      | Some clip ->
          x0 < clip.x + clip.width
          && x1 > clip.x
          && y0 < clip.y + clip.height
          && y1 > clip.y

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

let[@inline] apply_opacity t color =
  if t.opacity_product >= 1. then color
  else
    Color_packed.with_alpha color
      (int_of_float
         (Float.round
            (float_of_int (Color_packed.alpha color) *. t.opacity_product)))

(* Gives cell [idx] the rendition of cell [lead]. *)
let copy_rendition t ~lead idx =
  Buf.set_short t.attrs idx (Buf.get_short t.attrs lead);
  set_link t idx (get_link t lead);
  Buf.set t.fg_color idx (Buf.get t.fg_color lead);
  Buf.set t.bg_color idx (Buf.get t.bg_color lead)

(* How a write treats the cell under it. [Copy] stores the colors as they
   are. [Draw] blends them over the cell's and writes the code, a space
   included, as text drawing does. [Overlay] blends them too, but a space or
   an empty code lets the cell's text show through, tinted, as a translucent
   fill and the blanks of a drawn layer do. *)
type write = Copy | Draw | Overlay

(* A grapheme has one rendition, its start cell's: blending works on the start
   cell, and the other cells copy it. A wide character's start cell is
   written before its other cells. *)
let set_cell_internal t ~idx ~code ~fg_color ~bg_color ~attrs ~link ~write =
  let fg_alpha = Color_packed.alpha fg_color in
  let bg_alpha = Color_packed.alpha bg_color in
  let blending = write <> Copy in
  if blending && fg_alpha = 0 && bg_alpha = 0 then ()
  else if blending && (fg_alpha < 255 || bg_alpha < 255) then begin
    let dest_code = Buf.get_cell t.chars idx in
    let blank = write = Overlay && (code = space_cell || code = null_cell) in
    if Packed_cell.is_continuation code then begin
      (* The rest of a wide character whose start cell was just drawn. *)
      let lead = idx - Packed_cell.left_extent code in
      write_cell t idx code (Buf.get t.bg_color lead);
      copy_rendition t ~lead idx
    end
    else if blank && Packed_cell.is_continuation dest_code then
      (* A space tints the character it covers from its start cell. *)
      copy_rendition t ~lead:(idx - Packed_cell.left_extent dest_code) idx
    else begin
      let blended_bg =
        Color_packed.blend ~src:bg_color ~dst:(Buf.get t.bg_color idx)
      in
      if blank && dest_code <> null_cell && dest_code <> space_cell then begin
        Buf.set t.fg_color idx
          (Color_packed.blend ~src:bg_color ~dst:(Buf.get t.fg_color idx));
        set_link t idx link;
        Buf.set t.bg_color idx blended_bg;
        for i = idx + 1 to idx + extent_right dest_code do
          copy_rendition t ~lead:idx i
        done
      end
      else begin
        write_cell t idx code blended_bg;
        Buf.set_short t.attrs idx attrs;
        Buf.set t.fg_color idx
          (Color_packed.blend ~src:fg_color ~dst:blended_bg);
        set_link t idx link;
        Buf.set t.bg_color idx blended_bg
      end
    end
  end
  else begin
    write_cell t idx code bg_color;
    Buf.set_short t.attrs idx attrs;
    set_link t idx link;
    Buf.set t.fg_color idx fg_color;
    Buf.set t.bg_color idx bg_color
  end

(* Cell writes *)

let set_uchar t ~x ~y ~uchar ~fg ~bg ~attrs ?link ?(blend = false) () =
  if x >= 0 && y >= 0 && x < t.width && y < t.height && not (is_clipped t x y)
  then begin
    let code = Packed_cell.of_uchar uchar in
    let width = max 1 (Packed_cell.cell_width code) in
    let fits = x + width <= t.width in
    let fg_color = Color_packed.encode fg
    and bg_color = Color_packed.encode bg in
    let fg_color = if blend then apply_opacity t fg_color else fg_color in
    let bg_color = if blend then apply_opacity t bg_color else bg_color in
    let link = Option.value link ~default:no_link in
    let attrs = Ansi.Attr.pack attrs in
    for dx = 0 to min width (t.width - x) - 1 do
      let cell =
        if not fits then space_cell
        else if dx = 0 then code
        else Packed_cell.make_continuation ~left:dx ~right:(width - dx - 1)
      in
      set_cell_internal t
        ~idx:((y * t.width) + x + dx)
        ~code:cell ~fg_color ~bg_color ~attrs ~link
        ~write:(if blend then Draw else Copy)
    done
  end

(* C0, DEL, and C1 encoded in UTF-8 (0xC2 0x80 to 0xC2 0x9F), in [s] from
   [i] to [limit]. A function of its own, so no closure is allocated. *)
let rec has_control s i limit =
  i < limit
  &&
  let b = Char.code (String.unsafe_get s i) in
  b < 0x20 || b = 0x7F
  || b = 0xC2
     && i + 1 < limit
     && Char.code (String.unsafe_get s (i + 1)) < 0xA0
     && Char.code (String.unsafe_get s (i + 1)) >= 0x80
  || has_control s (i + 1) limit

let set_grapheme t ~x ~y ~width ~fg ~bg ~attrs ?(link = no_link)
    ?(protected = false) s ~pos ~len =
  if pos < 0 || len <= 0 || pos > String.length s - len then
    invalid_arg "Matrix_grid.set_grapheme: invalid substring";
  if width < 1 || x < 0 || x + width > t.width || y < 0 || y >= t.height then
    invalid_arg "Matrix_grid.set_grapheme: cells outside the grid";
  if has_control s pos (pos + len) then
    invalid_arg "Matrix_grid.set_grapheme: control character";
  let code = Packed_cell.intern_sub t.grapheme_store s ~pos ~len ~width in
  let fg_color = Color_packed.encode fg and bg_color = Color_packed.encode bg in
  let attrs =
    if protected then Ansi.Attr.pack attrs lor protected_bit
    else Ansi.Attr.pack attrs
  in
  let idx = (y * t.width) + x in
  set_cell_internal t ~idx ~code ~fg_color ~bg_color ~attrs ~link ~write:Copy;
  for i = 1 to width - 1 do
    let code = Packed_cell.make_continuation ~left:i ~right:(width - 1 - i) in
    set_cell_internal t ~idx:(idx + i) ~code ~fg_color ~bg_color ~attrs ~link
      ~write:Copy
  done

let erase_unprotected t (r : region) =
  match Rect.clip_to_bounds t.width t.height r with
  | None -> ()
  | Some r ->
      for y = r.y to r.y + r.height - 1 do
        let row = y * t.width in
        let first = row + r.x and last = row + r.x + r.width - 1 in
        let stop = last + extent_right (Buf.get_cell t.chars last) in
        let i = ref (first - extent_left (Buf.get_cell t.chars first)) in
        while !i <= stop do
          let w = max 1 (Packed_cell.cell_width (Buf.get_cell t.chars !i)) in
          if not (is_protected t !i) then
            for j = !i to !i + w - 1 do
              let code = Buf.get_cell t.chars j in
              if code <> space_cell then begin
                Grapheme_tracker.remove t.grapheme_tracker code;
                Buf.set_cell t.chars j space_cell
              end;
              set_link t j no_link
            done;
          i := !i + w
        done
      done

(* {1 Bulk operations} *)

let fill_opaque_rect t ~x0 ~y0 ~width ~height ~bg_color =
  if x0 = 0 && y0 = 0 && width = t.width && height = t.height then begin
    Grapheme_tracker.clear t.grapheme_tracker;
    Buf.fill_cell t.chars space_cell;
    Buf.fill t.attrs 0;
    clear_links t 0 (t.width * t.height);
    Buf.fill t.fg_color default_fg_packed;
    Buf.fill t.bg_color bg_color
  end
  else
    let y1 = y0 + height - 1 in
    for row = y0 to y1 do
      let start_idx = (row * t.width) + x0 in
      let end_idx = start_idx + width - 1 in
      for i = start_idx to end_idx do
        cleanup_overwritten_cell t i bg_color
      done;
      Buf.fill_range t.chars ~pos:start_idx ~len:width
        (Packed_cell.to_int space_cell);
      Buf.fill_short_range t.attrs ~pos:start_idx ~len:width 0;
      clear_links t start_idx width;
      Buf.fill_range t.fg_color ~pos:start_idx ~len:width default_fg_packed;
      Buf.fill_range t.bg_color ~pos:start_idx ~len:width bg_color
    done

(* Clipped as [clip_rect_to_grid] clips, in integers: a reader that clears
   one row at a time, such as Vte.blit_row, allocates nothing. *)
let clear_rect ?color t ~x ~y ~width ~height =
  let x0 = ref (max 0 x) and y0 = ref (max 0 y) in
  let x1 = ref (min t.width (x + width))
  and y1 = ref (min t.height (y + height)) in
  let scissors = Dynarray.length t.scissor_stack in
  if scissors > 0 then begin
    let s = Dynarray.get t.scissor_stack (scissors - 1) in
    x0 := max !x0 s.Rect.x;
    y0 := max !y0 s.y;
    x1 := min !x1 (s.x + s.width);
    y1 := min !y1 (s.y + s.height)
  end;
  if !x1 > !x0 && !y1 > !y0 then
    let bg_color =
      Option.value color ~default:default_bg |> Color_packed.encode
    in
    fill_opaque_rect t ~x0:!x0 ~y0:!y0 ~width:(!x1 - !x0) ~height:(!y1 - !y0)
      ~bg_color

let fill_rect t ~x ~y ~width ~height ~color =
  match clip_rect_to_grid t Rect.{ x; y; width; height } with
  | None -> ()
  | Some r ->
      let bg_color = apply_opacity t (Color_packed.encode color) in
      let alpha = Color_packed.alpha bg_color in
      if alpha = 0 then ()
      else if alpha = 255 then
        fill_opaque_rect t ~x0:r.x ~y0:r.y ~width:r.width ~height:r.height
          ~bg_color
      else
        for row = r.y to r.y + r.height - 1 do
          let base = (row * t.width) + r.x in
          for dx = 0 to r.width - 1 do
            set_cell_internal t ~idx:(base + dx) ~code:space_cell
              ~fg_color:default_fg_packed ~bg_color ~attrs:0 ~link:no_link
              ~write:Overlay
          done
        done

(* {1 Copying} *)

let blit ~src ~dst =
  if src == dst then ()
  else (
    resize dst ~width:src.width ~height:src.height;
    dst.width_method <- src.width_method;
    Buf.blit src.fg_color dst.fg_color;
    Buf.blit src.bg_color dst.bg_color;
    Buf.blit src.attrs dst.attrs;
    blit_links ~src ~dst 0 0 (src.width * src.height);

    if src.grapheme_store == dst.grapheme_store then (
      Buf.blit src.chars dst.chars;
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
      let len = src.width * src.height in
      for i = 0 to len - 1 do
        let src_c = Buf.get_cell src.chars i in
        let dst_c =
          if Packed_cell.is_inline src_c then src_c else copy_cell src_c
        in
        Buf.set_cell dst.chars i dst_c;
        if Packed_cell.is_complex dst_c then
          Grapheme_tracker.add dst.grapheme_tracker dst_c
      done)

let copy t =
  let dst =
    create ~width:t.width ~height:t.height ~width_method:t.width_method ()
  in
  blit ~src:t ~dst;
  dst

(** Update grapheme tracker for a bulk row copy within the same grid. *)
let bulk_update_graphemes t ~src_idx ~dst_idx ~len =
  if Grapheme_tracker.unique_count t.grapheme_tracker > 0 then begin
    (* Retain source payloads before releasing overwritten destination cells. *)
    for i = 0 to len - 1 do
      let code = Buf.get_cell t.chars (src_idx + i) in
      if not (Packed_cell.is_inline code) then
        Grapheme_tracker.add t.grapheme_tracker code
    done;
    for i = 0 to len - 1 do
      let code = Buf.get_cell t.chars (dst_idx + i) in
      if not (Packed_cell.is_inline code) then
        Grapheme_tracker.remove t.grapheme_tracker code
    done
  end

(* Move a rectangle within [t], both positions within bounds, without
   blending. A grapheme cut by the edge of the source or of the destination
   becomes blanks on [repair]. *)
let move_within t ~src_x ~src_y ~width ~height ~dst_x ~dst_y ~repair =
  if src_x = 0 && dst_x = 0 && width = t.width then begin
    (* Full rows cut no grapheme: one bulk copy. *)
    let si = src_y * t.width in
    let di = dst_y * t.width in
    let len = width * height in
    bulk_update_graphemes t ~src_idx:si ~dst_idx:di ~len;
    Buf.blit
      (Bigarray.Array1.sub t.chars si len)
      (Bigarray.Array1.sub t.chars di len);
    Buf.blit
      (Bigarray.Array1.sub t.attrs si len)
      (Bigarray.Array1.sub t.attrs di len);
    blit_links ~src:t ~dst:t si di len;
    Buf.blit
      (Bigarray.Array1.sub t.fg_color si len)
      (Bigarray.Array1.sub t.fg_color di len);
    Buf.blit
      (Bigarray.Array1.sub t.bg_color si len)
      (Bigarray.Array1.sub t.bg_color di len)
  end
  else begin
    (* Move storage before repairing edges: per-cell extent cleanup could erase
       source cells that an overlapping move has not read yet. *)
    let y_start, y_limit, y_step =
      if src_y < dst_y then (height - 1, -1, -1) else (0, height, 1)
    in
    let row = ref y_start in
    while !row <> y_limit do
      let si = ((src_y + !row) * t.width) + src_x in
      let di = ((dst_y + !row) * t.width) + dst_x in
      let last = di + width - 1 in
      let old_start = di - extent_left (Buf.get_cell t.chars di) in
      let old_end = last + extent_right (Buf.get_cell t.chars last) in
      bulk_update_graphemes t ~src_idx:si ~dst_idx:di ~len:width;
      Buf.blit_range t.chars ~src_pos:si t.chars ~dst_pos:di ~len:width;
      Buf.blit_short_range t.attrs ~src_pos:si t.attrs ~dst_pos:di ~len:width;
      blit_links ~src:t ~dst:t si di width;
      Buf.blit_range t.fg_color ~src_pos:si t.fg_color ~dst_pos:di ~len:width;
      Buf.blit_range t.bg_color ~src_pos:si t.bg_color ~dst_pos:di ~len:width;
      (* What the destination's edges cut of the graphemes it overwrote. *)
      for i = old_start to di - 1 do
        blank_cell t i repair
      done;
      for i = last + 1 to old_end do
        blank_cell t i repair
      done;
      (* What the source's edges cut of the graphemes it moved. *)
      let first = Buf.get_cell t.chars di in
      let final = Buf.get_cell t.chars last in
      if Packed_cell.is_continuation first then
        for i = di to min last (di + Packed_cell.right_extent first) do
          blank_cell t i repair
        done;
      if extent_right final > 0 then
        for i = max di (last - extent_left final) to last do
          blank_cell t i repair
        done;
      row := !row + y_step
    done
  end

let transfer_region ~blend ~src ~dst ~src_x ~src_y ~width ~height ~dst_x ~dst_y
    =
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

        if (not blend) && same_grid then
          move_within src ~src_x ~src_y ~width ~height ~dst_x ~dst_y
            ~repair:default_bg_packed
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
              let fg_color =
                if blend then apply_opacity dst fg_color else fg_color
              in
              let bg_color =
                if blend then apply_opacity dst bg_color else bg_color
              in
              if
                (not blend)
                || Color_packed.alpha fg_color <> 0
                || Color_packed.alpha bg_color <> 0
              then begin
                let attrs = Buf.get_short src.attrs sidx in
                let link = get_link src sidx in

                (* Detect orphan continuations whose start cell is outside the
                 copied region *)
                let is_orphan =
                  Packed_cell.is_continuation code
                  && (sx - Packed_cell.left_extent code < src_x
                     || sx + Packed_cell.right_extent code >= src_x + width)
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
                let link = if is_reset then no_link else link in
                let fg_color =
                  if is_reset then
                    if blend then apply_opacity dst default_fg_packed
                    else default_fg_packed
                  else fg_color
                in
                set_cell_internal dst
                  ~idx:((dy * dst.width) + dx)
                  ~code:mapped_code ~fg_color ~bg_color ~attrs ~link
                  ~write:(if blend then Overlay else Copy)
              end;

              k := !k + x_step
            done;
            i := !i + y_step
          done
        end

let blit_region = transfer_region ~blend:false

let draw_region ~src ~dst ~src_x ~src_y ~width ~height ~dst_x ~dst_y =
  if width > 0 && height > 0 && dst.opacity_product > 0. then begin
    let src = if src == dst then copy src else src in
    transfer_region ~blend:true ~src ~dst ~src_x ~src_y ~width ~height ~dst_x
      ~dst_y
  end

(* {1 Shifting} *)

(* Blank on [bg] every grapheme that crosses a vertical edge of [r]. *)
let blank_cut_edges t (r : region) bg =
  for y = r.y to r.y + r.height - 1 do
    let first = (y * t.width) + r.x in
    let last = first + r.width - 1 in
    if extent_left (Buf.get_cell t.chars first) > 0 then
      blank_grapheme t first bg;
    if extent_right (Buf.get_cell t.chars last) > 0 then
      blank_grapheme t last bg
  done

let shift t (r : region) ~dx ~dy ~color =
  match Rect.clip_to_bounds t.width t.height r with
  | None -> ()
  | Some _ when dx = 0 && dy = 0 -> ()
  | Some r ->
      let bg = Color_packed.encode color in
      let blank (b : region) =
        if b.width > 0 && b.height > 0 then
          fill_opaque_rect t ~x0:b.x ~y0:b.y ~width:b.width ~height:b.height
            ~bg_color:bg
      in
      blank_cut_edges t r bg;
      if abs dx >= r.width || abs dy >= r.height then blank r
      else begin
        let width = r.width - abs dx and height = r.height - abs dy in
        let src_x = if dx < 0 then r.x - dx else r.x in
        let src_y = if dy < 0 then r.y - dy else r.y in
        let dst_x = src_x + dx and dst_y = src_y + dy in
        move_within t ~src_x ~src_y ~width ~height ~dst_x ~dst_y ~repair:bg;
        (* What the move uncovered. *)
        let rows_x = r.x and rows_w = r.width in
        if dy > 0 then
          blank { x = rows_x; y = r.y; width = rows_w; height = dy }
        else if dy < 0 then
          blank
            {
              x = rows_x;
              y = r.y + r.height + dy;
              width = rows_w;
              height = -dy;
            };
        if dx > 0 then blank { x = r.x; y = dst_y; width = dx; height }
        else if dx < 0 then
          blank { x = r.x + r.width + dx; y = dst_y; width = -dx; height }
      end

(* {1 Text rendering} *)

let draw_text ?style ?(tab_width = 2) ?width_method t ~x ~y ~text =
  if tab_width < 1 then invalid_arg "Matrix_grid.draw_text: tab width below 1";
  let width_method = Option.value width_method ~default:t.width_method in
  if text = "" || y < 0 || y >= t.height then ()
  else
    let s = Option.value style ~default:Ansi.Style.default in
    let fg_color =
      Option.value s.fg ~default:default_fg
      |> Color_packed.encode |> apply_opacity t
    in
    let bg_color =
      Option.value s.bg ~default:default_bg
      |> Color_packed.encode |> apply_opacity t
    in
    if Color_packed.alpha fg_color = 0 && Color_packed.alpha bg_color = 0 then
      ()
    else
      let attrs = Ansi.Attr.pack s.attrs in
      let link = Option.value s.link ~default:no_link in
      let cur_x = ref x in
      let tabw = tab_width in
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
        (* Interning is deferred until the start cell is known to be written:
         clipped or overflowing graphemes must not allocate store slots, or
         each distinct one would leak a refcount-0 entry. *)
        let writer ~offset ~len ~w =
          let bounds_ok = !cur_x + w <= t.width && !cur_x >= 0 in
          let start_visible = cell_visible !cur_x in

          if bounds_ok && start_visible then begin
            let code =
              Packed_cell.intern_sub t.grapheme_store text ~pos:offset ~len
                ~width:w
            in
            let w = Packed_cell.cell_width code in
            begin
              let idx = (y * t.width) + !cur_x in
              set_cell_internal t ~idx ~code ~fg_color ~bg_color ~attrs ~link
                ~write:Draw;
              for i = 1 to w - 1 do
                let c_x = !cur_x + i in
                let c_idx = (y * t.width) + c_x in
                let cont =
                  Packed_cell.make_continuation ~left:i ~right:(w - 1 - i)
                in
                set_cell_internal t ~idx:c_idx ~code:cont ~fg_color ~bg_color
                  ~attrs ~link ~write:Draw
              done
            end;
            cur_x := !cur_x + w
          end
          else begin
            if (not bounds_ok) && !cur_x >= 0 && !cur_x < t.width then
              (* Wide cell overflows: fill remainder with styled spaces *)
              for x_fill = !cur_x to t.width - 1 do
                if cell_visible x_fill then
                  let idx = (y * t.width) + x_fill in
                  let bg_color =
                    match s.bg with
                    | None -> Buf.get t.bg_color idx
                    | Some _ -> bg_color
                  in
                  set_cell_internal t ~idx ~code:space_cell ~fg_color ~bg_color
                    ~attrs ~link ~write:Copy
              done;
            cur_x := !cur_x + w
          end
        in

        (* Text.iter_units skips units of no width, controls among
           them: no cell holds a control. *)
        try
          Text.iter_units ~width_method ~tab_width:tabw
            (fun ~offset ~len ~width:w ->
              let start_x = !cur_x in
              let end_x = start_x + w in
              if end_x <= 0 then cur_x := end_x
              else if start_x >= t.width then raise Exit
              else writer ~offset ~len ~w)
            text
        with Exit -> ()

let draw_spans ?(tab_width = 2) ?width_method ?width t ~x ~y spans =
  if tab_width < 1 then invalid_arg "Matrix_grid.draw_spans: tab width below 1";
  let width_method = Option.value width_method ~default:t.width_method in
  let room = Option.value width ~default:(t.width - x) in
  let spans =
    List.map
      (fun (s : Ansi.span) ->
        if String.contains s.text '\n' then
          { s with text = String.map (function '\n' -> ' ' | c -> c) s.text }
        else s)
      (Ansi.sanitize_spans spans)
  in
  let measure text = Text.measure ~width_method ~tab_width text in
  let draw x (s : Ansi.span) text =
    draw_text ~style:s.style ~tab_width ~width_method t ~x ~y ~text
  in
  let total =
    List.fold_left (fun a (s : Ansi.span) -> a + measure s.text) 0 spans
  in
  if total <= room then
    List.fold_left
      (fun x (s : Ansi.span) ->
        draw x s s.text;
        x + measure s.text)
      x spans
  else
    (* The line is cut after the graphemes that leave room for the ellipsis,
       which takes the style of the first grapheme it hides. *)
    let ellipsis = "\u{2026}" in
    let fit = room - measure ellipsis in
    let rec go used = function
      | [] -> x + used
      | (s : Ansi.span) :: rest ->
          let w = measure s.text in
          if used + w <= fit then (
            draw (x + used) s s.text;
            go (used + w) rest)
          else
            let start = used in
            let used = ref used and stop = ref 0 in
            (try
               Text.iter_units ~width_method ~tab_width
                 (fun ~offset ~len ~width:w ->
                   if !used + w > fit then raise Exit;
                   used := !used + w;
                   stop := offset + len)
                 s.text
             with Exit -> ());
            draw (x + start) s (String.sub s.text 0 !stop);
            draw (x + !used) s ellipsis;
            x + !used + measure ellipsis
    in
    if fit < 0 then x else go 0 spans

(* {1 Box drawing} *)

let draw_box t ~x ~y ~width ~height ?(border = Border.single)
    ?(sides = Border.all) ?(style = Ansi.Style.default) ?fill ?title
    ?title_alignment ?title_style () =
  (* A border cell holds one character in one cell: a wide one would cover
     its neighbor on the side. *)
  let wide u = Packed_cell.cell_width (Packed_cell.of_uchar u) > 1 in
  let b = border in
  if
    List.exists wide
      [
        b.top_left;
        b.top_right;
        b.bottom_left;
        b.bottom_right;
        b.horizontal;
        b.vertical;
        b.top_t;
        b.bottom_t;
        b.left_t;
        b.right_t;
        b.cross;
      ]
  then invalid_arg "Matrix_grid.draw_box: a border character wider than a cell";
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
            match style.bg with Some c -> c | None -> Ansi.Color.transparent)
      in
      let bg_color_packed = apply_opacity t (Color_packed.encode bg_color) in
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

      let b_fg_color =
        Option.value style.Ansi.Style.fg ~default:default_fg
        |> Color_packed.encode |> apply_opacity t
      in
      let b_attrs = Ansi.Attr.pack style.attrs in

      (* Pack the border characters once per box. *)
      let cell_top_left = Packed_cell.of_uchar border_chars.top_left in
      let cell_top_right = Packed_cell.of_uchar border_chars.top_right in
      let cell_bottom_left = Packed_cell.of_uchar border_chars.bottom_left in
      let cell_bottom_right = Packed_cell.of_uchar border_chars.bottom_right in
      let cell_horizontal = Packed_cell.of_uchar border_chars.horizontal in
      let cell_vertical = Packed_cell.of_uchar border_chars.vertical in

      let draw_b bx by cell =
        if
          bx >= 0 && by >= 0 && bx < t.width && by < t.height
          && not (is_clipped t bx by)
        then
          set_cell_internal t
            ~idx:((by * t.width) + bx)
            ~code:cell ~fg_color:b_fg_color ~bg_color:bg_color_packed
            ~attrs:b_attrs ~link:no_link ~write:Draw
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
                      if top then cell_top_left else cell_bottom_left
                    else cell_horizontal
                  else if c = x + width - 1 then
                    if has `Right then
                      if top then cell_top_right else cell_bottom_right
                    else cell_horizontal
                  else cell_horizontal
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
            draw_b x r cell_vertical
          done;
        if has `Right then
          let rx = x + width - 1 in
          if rx >= 0 && rx < t.width then
            for r = vy_start to vy_end do
              draw_b rx r cell_vertical
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

(* {1 Joined lines} *)

(* Draws scalar [u] at [(x, y)] as [draw_text] draws a text of it alone, for
   a scalar one cell wide that makes a unit of its own under every width
   method. A line set's glyphs are drawn every frame, and this writes them
   without allocating. *)
let draw_scalar t ~x ~y ~(style : Ansi.Style.t) u =
  if x >= 0 && y >= 0 && x < t.width && y < t.height && not (is_clipped t x y)
  then
    let color c default =
      Option.value c ~default |> Color_packed.encode |> apply_opacity t
    in
    set_cell_internal t
      ~idx:((y * t.width) + x)
      ~code:(Packed_cell.of_uchar u)
      ~fg_color:(color style.fg default_fg)
      ~bg_color:(color style.bg default_bg)
      ~attrs:(Ansi.Attr.pack style.attrs)
      ~link:(Option.value style.link ~default:no_link)
      ~write:Draw

let draw_glyph t ~x ~y ~style s =
  let len = String.length s in
  if len > 0 then
    let d = String.get_utf_8_uchar s 0 in
    let u = Uchar.utf_decode_uchar d in
    if
      Uchar.utf_decode_is_valid d
      && Uchar.utf_decode_length d = len
      && Text.uchar_width ~tab_width:1 u = 1
      && Text.grapheme_cluster_break u = `XX
    then draw_scalar t ~x ~y ~style u
    else draw_text ~style t ~x ~y ~text:s

let draw_lines ?(glyphs = Lines.Unicode) t lines =
  let lx = Lines.x lines and ly = Lines.y lines and lw = Lines.width lines in
  for y = max 0 ly to min (ly + Lines.height lines) t.height - 1 do
    for x = max 0 lx to min (lx + lw) t.width - 1 do
      let c = Lines.code lines (((y - ly) * lw) + x - lx) in
      if c <> 0 then
        let style = Lines.style lines c in
        match glyphs with
        | Lines.Unicode ->
            draw_scalar t ~x ~y ~style (Lines.scalar ~rounded:false c)
        | Rounded -> draw_scalar t ~x ~y ~style (Lines.scalar ~rounded:true c)
        | Set set -> draw_glyph t ~x ~y ~style (Lines.glyph set c)
    done
  done

(* {1 Line drawing} *)

type line_symbols = {
  h : string;
  v : string;
  diag_up : string;
  diag_down : string;
}

let default_line_symbols = { h = "─"; v = "│"; diag_up = "╱"; diag_down = "╲" }

let ascii_line_symbols = { h = "-"; v = "|"; diag_up = "/"; diag_down = "\\" }
let braille_lut = Grid_data.braille_lut
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

(* {1 Pictures} *)

type picture = Grapheme_store.picture

(* Each picture gets an id of its own, the identity the screens compare and
   keep pixels by. *)
let picture_ids = Atomic.make 1

let picture ~width ~height rgba =
  if width <= 0 || height <= 0 then
    invalid_arg "Matrix_grid.picture: width and height must be > 0";
  if String.length rgba <> 4 * width * height then
    invalid_arg "Matrix_grid.picture: rgba is not 4 * width * height bytes";
  {
    Grapheme_store.id = Atomic.fetch_and_add picture_ids 1;
    width;
    height;
    rgba;
  }

let picture_size (p : picture) = (p.width, p.height)

(* A tile is written as a copy is, on the cell's own background, with the
   default foreground, no attributes and no link. *)
let draw_picture t ~x ~y ~width ~height p =
  match clip_rect_to_grid t Rect.{ x; y; width; height } with
  | None -> ()
  | Some r ->
      for row = r.y to r.y + r.height - 1 do
        for col = r.x to r.x + r.width - 1 do
          let idx = (row * t.width) + col in
          let code =
            Packed_cell.intern_tile t.grapheme_store p ~cols:width ~rows:height
              ~col:(col - x) ~row:(row - y)
          in
          write_cell t idx code (Buf.get t.bg_color idx);
          Buf.set_short t.attrs idx 0;
          set_link t idx no_link;
          Buf.set t.fg_color idx default_fg_packed
        done
      done

(* {1 Inspection & utilities} *)

(* A cell a terminal erase reproduces: a space or nothing, with no attribute
   and no link, on the terminal's default background. *)
let space_code = Packed_cell.to_int space_cell
let null_code = Packed_cell.to_int null_cell

let[@inline] blank_cell t idx =
  let code = Buf.get t.chars idx in
  (code = space_code || code = null_code)
  && Buf.get_short t.attrs idx land attrs_mask = 0
  && (let bg = Buf.get t.bg_color idx in
      bg = default_bg_packed || bg = terminal_bg_packed
      || Color_packed.alpha bg = 0
      ||
      match Color_packed.intent bg with
      | Default -> true
      | Indexed _ | Rgb -> false)
  && (Array.length t.links = 0
     || String.length (Array.unsafe_get t.links idx) = 0)

let active_height t =
  let rec find_row y =
    if y < 0 then 0
    else
      let row_start = y * t.width in
      let x = ref 0 in
      while !x < t.width && blank_cell t (row_start + !x) do
        incr x
      done;
      if !x < t.width then y + 1 else find_row (y - 1)
  in
  find_row (t.height - 1)

let grapheme_stats t =
  ( Grapheme_store.live_count t.grapheme_store,
    Grapheme_store.slot_count t.grapheme_store )

(* {1 Serialization} *)

let to_text ?(trim = true) t =
  let width = t.width and height = t.height in
  if width <= 0 || height <= 0 then ""
  else begin
    let line = Buffer.create width in
    let buf = Buffer.create ((width + 1) * height) in
    for y = 0 to height - 1 do
      Buffer.clear line;
      for x = 0 to width - 1 do
        let idx = (y * width) + x in
        if not (is_continuation t idx) then begin
          let text = get_text t idx in
          if String.length text = 0 then Buffer.add_char line ' '
          else Buffer.add_string line text
        end
      done;
      let row = Buffer.contents line in
      let row =
        if trim then begin
          let last = ref (String.length row - 1) in
          while !last >= 0 && row.[!last] = ' ' do
            decr last
          done;
          if !last = String.length row - 1 then row
          else String.sub row 0 (!last + 1)
        end
        else row
      in
      Buffer.add_string buf row;
      if y < height - 1 then Buffer.add_char buf '\n'
    done;
    Buffer.contents buf
  end

(* A cell that shows nothing at the end of a row: empty or a space, on the
   default background, with no attribute and no link. *)
let shows_nothing t idx =
  let code = Buf.get_cell t.chars idx in
  (Packed_cell.is_empty code || code = space_cell)
  &&
  let bg = Buf.get t.bg_color idx in
  (bg = default_bg_packed || bg = terminal_bg_packed)
  && Buf.get_short t.attrs idx = 0
  && String.length (get_link t idx) = 0

let to_ansi ?(styling = `Truecolor) ?(trim = false) ?(complete_rows = false)
    ?rows t =
  let height = Option.value rows ~default:t.height in
  if height < 0 || height > t.height then
    invalid_arg "Matrix_grid.to_ansi: rows must be within grid height";
  let width = t.width in
  if width <= 0 || height <= 0 then ""
  else
    let store = t.grapheme_store in
    let scratch = ref (Bytes.create 256) in

    Ansi.to_string (fun w ->
        let style = Ansi.Sgr_state.create () in
        let styled =
          match styling with
          | `Plain -> false
          | #Ansi.Color.depth as depth ->
              Ansi.Sgr_state.set_color_depth style depth;
              true
        in

        for y = 0 to height - 1 do
          if y > 0 && not complete_rows then Ansi.emit (Ansi.char '\n') w;
          if complete_rows then Ansi.emit (Ansi.erase_line ~mode:`All) w;

          let last =
            let last = ref width in
            if trim then
              while !last > 0 && shows_nothing t ((y * width) + !last - 1) do
                decr last
              done;
            !last
          in
          let x = ref 0 in
          while !x < last do
            let idx = (y * width) + !x in
            let code = Buf.get_cell t.chars idx in
            let cw = Packed_cell.cell_width code in
            let step = if cw <= 0 then 1 else cw in

            if styled then
              Ansi.Sgr_state.update style w ~fg:(get_fg t idx)
                ~bg:(get_bg t idx) ~attrs:(get_attrs t idx)
                ~link:(get_link t idx);

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

          if styled then (
            Ansi.Sgr_state.close_link style w;
            Ansi.emit Ansi.reset w;
            Ansi.Sgr_state.reset style);
          if complete_rows then Ansi.emit (Ansi.literal "\r\n") w
        done)

(* {1 Checked accessors}

   The accessors a program calls check the index; the code above uses the
   unchecked ones, which these shadow from here on, and [Unsafe] hands them to
   matrix's own libraries. *)

module Unsafe = struct
  type tile = Grapheme_store.tile = {
    picture : picture;
    cols : int;
    rows : int;
    col : int;
    row : int;
  }

  let has_pictures t = Grapheme_store.live_tiles t.grapheme_store > 0

  let[@inline] is_picture t idx =
    let c = Buf.get_cell t.chars idx in
    Packed_cell.names_slot c
    && (Packed_cell.tile t.grapheme_store c).picture.id <> 0

  let tile t idx = Packed_cell.tile t.grapheme_store (Buf.get_cell t.chars idx)
  let picture_id (p : picture) = p.id
  let picture_rgba (p : picture) = p.rgba
  let get_codepoint = get_codepoint
  let get_attrs = get_attrs
  let get_link = get_link
  let get_fg = get_fg
  let get_bg = get_bg
  let cell_text_length = cell_text_length
  let blit_cell_text = blit_cell_text
  let cell_width = cell_width
  let is_empty = is_empty
  let cells_equal = cells_equal
end

let[@inline never] outside name =
  invalid_arg ("Matrix_grid." ^ name ^ ": index outside the grid")

let[@inline] check name t idx =
  if idx < 0 || idx >= t.width * t.height then outside name

let[@inline] get_codepoint t idx =
  check "get_codepoint" t idx;
  get_codepoint t idx

let[@inline] get_attrs t idx =
  check "get_attrs" t idx;
  get_attrs t idx

let[@inline] get_link t idx =
  check "get_link" t idx;
  get_link t idx

let[@inline] get_fg t idx =
  check "get_fg" t idx;
  get_fg t idx

let[@inline] get_bg t idx =
  check "get_bg" t idx;
  get_bg t idx

let[@inline] get_text t idx =
  check "get_text" t idx;
  get_text t idx

let[@inline] cell_text_length t idx =
  check "cell_text_length" t idx;
  cell_text_length t idx

let[@inline] blit_cell_text t idx buf ~pos =
  check "blit_cell_text" t idx;
  blit_cell_text t idx buf ~pos

let[@inline] get_style t idx =
  check "get_style" t idx;
  get_style t idx

let[@inline] is_empty t idx =
  check "is_empty" t idx;
  is_empty t idx

let[@inline] is_continuation t idx =
  check "is_continuation" t idx;
  is_continuation t idx

let[@inline] cell_width t idx =
  check "cell_width" t idx;
  cell_width t idx

let[@inline] cells_equal t1 idx1 t2 idx2 =
  check "cells_equal" t1 idx1;
  check "cells_equal" t2 idx2;
  cells_equal t1 idx1 t2 idx2

let[@inline] is_protected t idx =
  check "is_protected" t idx;
  is_protected t idx
