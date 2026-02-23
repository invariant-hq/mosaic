(** Declarative drawing API for terminal UIs.

    Image provides a compositional interface for building complex terminal
    layouts without manually managing grid coordinates or rendering order.
    Images are immutable rectangular descriptions of drawing operations that
    compile down to flat arrays of positioned primitives, enabling efficient
    batch rendering to {!Grid}.

    {1 Overview}

    An image is a width × height rectangle containing a sequence of drawing
    primitives (text, fills, boxes, hit regions, custom operations). Images are
    pure values: composition operations like {!hcat} and {!overlay} transform
    primitive arrays without side effects. Only {!render} performs actual
    drawing by walking the flat program and executing primitives into a {!Grid}.

    This design separates layout logic from rendering, making it simple to
    compose, transform, and cache UI elements. The library handles coordinate
    translation, clipping, and primitive ordering automatically.

    {1 Usage Basics}

    Build images from primitives:
    {[
      let greeting = Image.text "Hello, world!" in
      let box = Image.box ~width:20 ~height:3 () in
      let filled = Image.fill ~color:Ansi.Color.blue ~width:10 ~height:2 ()
    ]}

    Combine images with composition operators:
    {[
      let header = Image.hcat [
        Image.text "Status: ";
        Image.text ~style:(Ansi.Style.make ~fg:Ansi.Color.green ()) "OK"
      ] in
      let panel = Image.vcat [
        header;
        Image.rule_h ~width:20 ();
        Image.text "System ready"
      ]
    ]}

    Render to a grid for display:
    {[
      let grid = Grid.create ~width:80 ~height:24 () in
      Image.render grid panel ~x:0 ~y:0
    ]}

    {1 Key Concepts}

    {2 Immutability and Composition}

    Images are immutable values. Composition functions ({!hcat}, {!vcat},
    {!overlay}) produce new images without modifying their inputs. Each
    composition operation flattens nested primitives into a single array,
    adjusting coordinates and clipping regions as needed.

    Primitives are stored with absolute positions relative to the image's
    top-left corner (0, 0). Composition shifts primitives by accumulating
    offsets: {!hcat} accumulates horizontal displacement, {!vcat} accumulates
    vertical displacement, and {!overlay} stacks without offset.

    {2 Coordinate Systems}

    Images use standard grid coordinates:
    - Origin (0, 0) is the top-left corner
    - X increases rightward, Y increases downward
    - All coordinates are in terminal cells (not pixels or points)

    Composition operations maintain this invariant by translating child image
    coordinates into parent space. For example, {!hcat} shifts the second
    image's primitives right by the width of the first image.

    {2 Sizing and Alignment}

    Image dimensions are determined by their content:
    - Primitive constructors ({!text}, {!fill}, {!box}) calculate dimensions
      from parameters or text width
    - {!hcat} sums child widths and takes maximum height
    - {!vcat} sums child heights and takes maximum width
    - {!overlay} takes maximum width and height

    Empty images (width or height = 0) are filtered during composition and
    contribute nothing to layout or rendering.

    Alignment is handled via {!pad}, {!crop}, {!hsnap}, and {!vsnap}:
    - {!pad} adds transparent space around an image
    - {!crop} removes edges, potentially clipping primitives
    - {!hsnap} and {!vsnap} resize to exact dimensions, padding or cropping as
      needed with configurable alignment

    {2 Clipping Regions}

    Images maintain optional scissor rectangles that constrain rendering.
    Clipping is applied hierarchically:
    - {!crop} sets a clip rectangle on the resulting image
    - Composition merges parent and child clips via intersection
    - {!render} translates clips to {!Grid.push_clip} calls

    Primitives store their local clip (from {!crop} ancestors) separately from
    position. During rendering, the image's base clip is merged with each
    primitive's clip and shifted by the render offset [(x, y)].

    {2 Rendering Semantics}

    {!render} walks the primitive array in order, executing each operation:
    - Text primitives call {!Grid.draw_text} per line
    - Fill primitives call {!Grid.fill_rect}
    - Box primitives call {!Grid.draw_box}
    - Hit primitives register regions in the optional {!Screen.Hit_grid.t}
    - Custom primitives invoke user-provided drawing functions

    Primitives are drawn in array order (later primitives overdraw earlier
    ones). Use {!overlay} to control z-order explicitly: the last image in the
    list appears on top.

    Rendering respects grid clips set via {!Grid.push_clip}. Clips are applied
    by wrapping primitive execution in {!Grid.clip}, which ensures balanced
    push/pop pairs even when primitives are skipped.

    {2 Hit Regions}

    Hit regions enable mouse interaction by mapping screen coordinates to
    application-defined identifiers. Use {!with_hit} to associate an ID with an
    entire image, or {!with_hit_rect} for precise sub-rectangles.

    Hit primitives are rendered like any other, but write to the optional
    {!Screen.Hit_grid.t} instead of the display grid. This allows hit testing
    via {!Screen.Hit_grid.get} after rendering completes.

    Hit IDs must be strictly positive; zero and negative values are ignored.
    This convention reserves 0 for "no hit" in hit grid lookups.

    {1 Performance Considerations}

    - Image construction is O(total_primitives) across all children, as
      primitives are flattened into a single array
    - Primitive shifting and clipping updates are O(1) per primitive
    - Rendering is O(primitives) for iteration plus the cost of each drawing
      operation (see {!Grid} documentation for primitive costs)
    - Text width calculation is O(grapheme_count) per {!text} call, using the
      configured {!Glyph.width_method}

    Composition does not perform rendering; it only reorganizes primitive
    arrays. This makes it cheap to build complex layouts incrementally and cache
    intermediate results. Rendering cost is proportional to the number of
    primitives executed, not the depth of composition.

    {1 Invariants}

    - Image dimensions are non-negative; constructors clamp negative inputs to
      zero
    - Empty images (width = 0 or height = 0) contain no primitives
    - Primitive positions are relative to the image's top-left (0, 0)
    - Clip rectangles are expressed in image-local coordinates
    - Hit IDs are strictly positive or ignored *)

module Color = Ansi.Color
module Style = Ansi.Style

type style = Style.t
type h_align = [ `Left | `Center | `Right ]
type v_align = [ `Top | `Middle | `Bottom ]
type hit_id = int

type t
(** Immutable image description: width x height rectangle of drawing primitives.
*)

val empty : t
(** Empty image with zero width and height. *)

val void : int -> int -> t
(** Image with dimensions but no drawing commands. Negative dimensions clamp to
    zero. *)

val width : t -> int
val height : t -> int
val size : t -> int * int

(** {1 Primitive constructors} *)

val fill : ?color:Color.t -> width:int -> height:int -> unit -> t
(** Solid rectangle of [color]. *)

val text : ?style:Style.t -> ?width_method:Glyph.width_method -> string -> t
(** Multi-line text (splits on ['\n']). *)

val string : ?style:Style.t -> ?width_method:Glyph.width_method -> string -> t
(** Alias for {!text}. *)

val line : ?style:Style.t -> ?width_method:Glyph.width_method -> string -> t
(** Single-line alias for {!text}. *)

val box :
  ?border:Grid.Border.t ->
  ?border_sides:Grid.Border.side list ->
  ?border_style:Style.t ->
  ?fill:Color.t ->
  width:int ->
  height:int ->
  unit ->
  t
(** Box primitive built on {!Grid.draw_box}. *)

val rule_h : ?style:Style.t -> width:int -> unit -> t
(** Horizontal rule using single-line box characters. *)

val rule_v : ?style:Style.t -> height:int -> unit -> t
(** Vertical rule using single-line box characters. *)

(** {1 Composition} *)

val hcat : t list -> t
val vcat : t list -> t
val overlay : t list -> t
val pad : ?left:int -> ?right:int -> ?top:int -> ?bottom:int -> t -> t
val hpad : int -> int -> t -> t
val vpad : int -> int -> t -> t
val crop : ?l:int -> ?r:int -> ?t:int -> ?b:int -> t -> t
val hcrop : int -> int -> t -> t
val vcrop : int -> int -> t -> t
val hsnap : ?align:h_align -> int -> t -> t
val vsnap : ?align:v_align -> int -> t -> t

(** {1 Hit regions} *)

val with_hit : id:hit_id -> t -> t
(** Add a hit region covering the full image. Ignored when [id <= 0] or the
    image is empty. *)

val with_hit_rect :
  id:hit_id -> x:int -> y:int -> width:int -> height:int -> t -> t
(** Add a hit region for a sub-rectangle. Coordinates are relative to the image.
*)

(** {1 Rendering} *)

val render : ?hits:Screen.Hit_grid.t -> ?x:int -> ?y:int -> Grid.t -> t -> unit
(** Render at [(x, y)] into [grid], registering hits when provided. *)

val draw : t -> Grid.t -> Screen.Hit_grid.t -> unit
(** Convenience for [render ~hits ~x:0 ~y:0]. *)

(** {1 Low-level escape hatches} *)

val custom :
  width:int ->
  height:int ->
  (Grid.t -> Screen.Hit_grid.t option -> x:int -> y:int -> unit) ->
  t
(** Arbitrary drawing while still integrating with {!render}. *)
