(** Declarative image composition and rendering.

    An {e image} is an immutable rectangular description of drawing primitives
    (text, fills, boxes, hit regions). Images are pure values: composition
    functions like {!hcat} and {!overlay} rearrange flat primitive arrays
    without side effects. Only {!render} performs actual drawing by walking the
    array and executing each primitive into a {!Grid.t}.

    {1:quick_start Quick start}

    {[
      let header =
        Image.hcat
          [
            Image.text "Status: ";
            Image.text ~style:(Ansi.Style.make ~fg:Ansi.Color.green ()) "OK";
          ]
      in
      let panel =
        Image.vcat
          [ header; Image.rule_h ~width:20 (); Image.text "System ready" ]
      in
      let grid = Grid.create ~width:80 ~height:24 () in
      Image.render grid panel ~x:0 ~y:0
    ]}

    {1:coords Coordinate system}

    Images use standard grid coordinates: origin [(0, 0)] at the top-left, X
    increasing rightward, Y increasing downward. All values are in terminal
    cells.

    {1:sizing Sizing}

    Dimensions are determined by content:
    - {!hcat} sums widths, takes maximum height.
    - {!vcat} sums heights, takes maximum width.
    - {!overlay} takes maximums of both.

    Empty images (width or height [= 0]) are filtered during composition and
    contribute nothing to layout.

    {1:clipping Clipping}

    {!crop} sets a scissor rectangle on the resulting image. Composition merges
    parent and child clips via intersection. During {!render}, clips translate
    to {!Grid.push_clip} / {!Grid.pop_clip} pairs.

    {1:hits Hit regions}

    Hit regions map screen coordinates to application-defined identifiers for
    mouse interaction. Use {!with_hit} for a whole-image region or
    {!with_hit_rect} for a sub-rectangle. Hit primitives write to the optional
    {!Screen.Hit_grid.t} passed to {!render}.

    {b Note.} Hit IDs must be strictly positive; zero and negative values are
    ignored. *)

module Color = Ansi.Color
module Style = Ansi.Style

(** {1:types Types} *)

type style = Style.t
(** The type for styles. Alias for {!Ansi.Style.t}. *)

type h_align = [ `Left | `Center | `Right ]
(** The type for horizontal alignments. *)

type v_align = [ `Top | `Middle | `Bottom ]
(** The type for vertical alignments. *)

type hit_id = int
(** The type for hit-region identifiers. A {e strictly positive} integer. *)

type t
(** The type for images. An immutable width-by-height rectangle of drawing
    primitives. *)

(** {1:constructors Constructors} *)

val empty : t
(** [empty] is the empty image with zero dimensions. *)

val void : int -> int -> t
(** [void w h] is an image with dimensions [(w, h)] but no drawing primitives.
    Negative dimensions are clamped to [0]. *)

(** {1:accessors Accessors} *)

val width : t -> int
(** [width img] is [img]'s width in cells. *)

val height : t -> int
(** [height img] is [img]'s height in cells. *)

val size : t -> int * int
(** [size img] is [(width img, height img)]. *)

(** {1:primitives Primitives} *)

val fill : ?color:Color.t -> width:int -> height:int -> unit -> t
(** [fill ~width ~height ()] is a solid rectangle of [color]. [color] defaults
    to the terminal default. *)

val text : ?style:Style.t -> ?width_method:Text.width_method -> string -> t
(** [text s] is a multi-line text image. Lines are split on ['\n']. Width is
    computed from the widest line using [width_method] (defaults to the global
    setting). *)

val string : ?style:Style.t -> ?width_method:Text.width_method -> string -> t
(** [string] is {!text}. *)

val line : ?style:Style.t -> ?width_method:Text.width_method -> string -> t
(** [line] is {!text} for a single line. *)

val box :
  ?border:Grid.Border.t ->
  ?border_sides:Grid.Border.side list ->
  ?border_style:Style.t ->
  ?fill:Color.t ->
  width:int ->
  height:int ->
  unit ->
  t
(** [box ~width ~height ()] is a box primitive rendered via {!Grid.draw_box}. *)

val rule_h : ?style:Style.t -> width:int -> unit -> t
(** [rule_h ~width ()] is a horizontal rule of [width] cells. *)

val rule_v : ?style:Style.t -> height:int -> unit -> t
(** [rule_v ~height ()] is a vertical rule of [height] cells. *)

(** {1:composition Composition} *)

val hcat : t list -> t
(** [hcat imgs] concatenates [imgs] horizontally. Width is the sum of child
    widths, height the maximum. *)

val vcat : t list -> t
(** [vcat imgs] concatenates [imgs] vertically. Height is the sum of child
    heights, width the maximum. *)

val overlay : t list -> t
(** [overlay imgs] stacks [imgs] at the same origin. Later images overdraw
    earlier ones. Dimensions are the component-wise maximums. *)

val pad : ?left:int -> ?right:int -> ?top:int -> ?bottom:int -> t -> t
(** [pad img] adds transparent space around [img]. Each side defaults to [0]. *)

val hpad : int -> int -> t -> t
(** [hpad l r img] is [pad ~left:l ~right:r img]. *)

val vpad : int -> int -> t -> t
(** [vpad t b img] is [pad ~top:t ~bottom:b img]. *)

val crop : ?l:int -> ?r:int -> ?t:int -> ?b:int -> t -> t
(** [crop img] removes edges, potentially clipping primitives. Each side
    defaults to [0]. *)

val hcrop : int -> int -> t -> t
(** [hcrop l r img] is [crop ~l ~r img]. *)

val vcrop : int -> int -> t -> t
(** [vcrop t b img] is [crop ~t ~b img]. *)

val hsnap : ?align:h_align -> int -> t -> t
(** [hsnap w img] resizes [img] to exactly [w] columns, padding or cropping
    according to [align] (defaults to [`Left]). *)

val vsnap : ?align:v_align -> int -> t -> t
(** [vsnap h img] resizes [img] to exactly [h] rows, padding or cropping
    according to [align] (defaults to [`Top]). *)

(** {1:hit_regions Hit regions} *)

val with_hit : id:hit_id -> t -> t
(** [with_hit ~id img] registers a hit region covering all of [img]. Ignored
    when [id <= 0] or [img] is empty. *)

val with_hit_rect :
  id:hit_id -> x:int -> y:int -> width:int -> height:int -> t -> t
(** [with_hit_rect ~id ~x ~y ~width ~height img] registers a hit region for the
    sub-rectangle at [(x, y)] with dimensions [(width, height)]. Coordinates are
    relative to [img]. *)

(** {1:rendering Rendering} *)

val render : ?hits:Screen.Hit_grid.t -> ?x:int -> ?y:int -> Grid.t -> t -> unit
(** [render grid img] draws [img] at [(x, y)] into [grid]. When [hits] is
    provided, hit primitives are registered there. [x] and [y] default to [0].
*)

val draw : t -> Grid.t -> Screen.Hit_grid.t -> unit
(** [draw img grid hits] is [render ~hits ~x:0 ~y:0 grid img]. *)

(** {1:low Low-level} *)

val custom :
  width:int ->
  height:int ->
  (Grid.t -> Screen.Hit_grid.t option -> x:int -> y:int -> unit) ->
  t
(** [custom ~width ~height f] is an image that invokes [f] during {!render}. [f]
    receives the grid, the optional hit grid, and the render offset. *)
