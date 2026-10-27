(** Style types for CSS layout.

    This module provides CSS style properties for layout computation, including
    dimensions, positioning, flexbox, and grid layout. *)

type calc_resolver = int -> float -> float
(** Type for calc resolver function. *)

(** {1 Length types} *)

module Compact_length = Compact_length
module Length_percentage = Length_percentage
module Length_percentage_auto = Length_percentage_auto
module Dimension = Dimension

(** {1 Layout control types} *)

module Display = Display
module Position = Position
module Overflow = Overflow
module Box_sizing = Box_sizing
module Box_generation_mode = Box_generation_mode
module Text_align = Text_align

(** {1 Flexbox types} *)

module Flex_direction = Flex_direction
module Flex_wrap = Flex_wrap

(** {1 Alignment types} *)

module Align_items = Alignment.Align_items
module Justify_items = Alignment.Justify_items
module Align_self = Alignment.Align_self
module Justify_self = Alignment.Justify_self
module Align_content = Alignment.Align_content
module Justify_content = Alignment.Justify_content

(** {1 Grid types} *)

module Grid_auto_flow = Grid_auto_flow
module Grid_placement = Grid_placement
module Grid_repetition = Grid_repetition
module Grid_template_area = Grid_template_area
module Grid_template_component = Grid_template_component
module Repetition_count = Repetition_count
module Track_sizing_function = Track_sizing_function

(** Grid coordinate systems and track utilities with submodules. *)
module Grid : sig
  include module type of Grid
  module Auto_flow = Grid_auto_flow
  module Placement = Grid_placement
  module Track_sizing_function = Track_sizing_function
  module Repetition_count = Repetition_count
  module Repetition = Grid_repetition
  module Template_component = Grid_template_component
  module Template_area = Grid_template_area
end

(** {1 Type aliases} *)

type length_percentage = Length_percentage.t
(** Alias for {!Length_percentage.t}. *)

type length_percentage_auto = Length_percentage_auto.t
(** Alias for {!Length_percentage_auto.t}. *)

type dimension = Dimension.t
(** Alias for {!Dimension.t}. *)

type display = Display.t
(** Alias for {!Display.t}. *)

type position = Position.t
(** Alias for {!Position.t}. *)

type overflow = Overflow.t
(** Alias for {!Overflow.t}. *)

type box_sizing = Box_sizing.t
(** Alias for {!Box_sizing.t}. *)

type box_generation_mode = Box_generation_mode.t
(** Alias for {!Box_generation_mode.t}. *)

type text_align = Text_align.t
(** Alias for {!Text_align.t}. *)

type flex_direction = Flex_direction.t
(** Alias for {!Flex_direction.t}. *)

type flex_wrap = Flex_wrap.t
(** Alias for {!Flex_wrap.t}. *)

type align_items = Align_items.t
(** Alias for {!Align_items.t}. *)

type align_content = Align_content.t
(** Alias for {!Align_content.t}. *)

type justify_items = Justify_items.t
(** Alias for {!Justify_items.t}. *)

type align_self = Align_self.t
(** Alias for {!Align_self.t}. *)

type justify_self = Justify_self.t
(** Alias for {!Justify_self.t}. *)

type justify_content = Justify_content.t
(** Alias for {!Justify_content.t}. *)

type grid_auto_flow = Grid_auto_flow.t
(** Alias for {!Grid_auto_flow.t}. *)

type grid_placement = Grid_placement.t
(** Alias for {!Grid_placement.t}. *)

type track_sizing_function = Track_sizing_function.t
(** Alias for {!Track_sizing_function.t}. *)

type repetition_count = Repetition_count.t
(** Alias for {!Repetition_count.t}. *)

type grid_repetition = Grid_repetition.t
(** Alias for {!Grid_repetition.t}. *)

type grid_template_component = Grid_template_component.t
(** Alias for {!Grid_template_component.t}. *)

type grid_template_area = Grid_template_area.t
(** Alias for {!Grid_template_area.t}. *)

(** {1 Convenience constructors} *)

module Rect_dim : sig
  val all_px : float -> length_percentage Toffee_geometry.rect
  (** [all_px v] creates a rectangle with all edges set to [v] pixels, using
      {!Length_percentage.length}. *)

  val all_px_auto : float -> length_percentage_auto Toffee_geometry.rect
  (** [all_px_auto v] creates a rectangle with all edges set to [v] pixels, for
      use with auto-capable properties like margin and inset. *)

  val xy_px : x:float -> y:float -> length_percentage Toffee_geometry.rect
  (** [xy_px ~x ~y] creates a rectangle with horizontal edges set to [x] pixels
      and vertical edges set to [y] pixels. *)

  val xy_px_auto :
    x:float -> y:float -> length_percentage_auto Toffee_geometry.rect
  (** [xy_px_auto ~x ~y] is the auto-capable variant of {!xy_px}, suitable for
      margin and inset. *)

  val horizontal_px : float -> length_percentage Toffee_geometry.rect
  (** [horizontal_px v] creates a rectangle with left and right set to [v]
      pixels and top/bottom set to zero. *)

  val horizontal_px_auto : float -> length_percentage_auto Toffee_geometry.rect
  (** [horizontal_px_auto v] is the auto-capable variant of {!horizontal_px}. *)

  val vertical_px : float -> length_percentage Toffee_geometry.rect
  (** [vertical_px v] creates a rectangle with top and bottom set to [v] pixels
      and left/right set to zero. *)

  val vertical_px_auto : float -> length_percentage_auto Toffee_geometry.rect
  (** [vertical_px_auto v] is the auto-capable variant of {!vertical_px}. *)
end

module Size_dim : sig
  val px : w:float -> h:float -> dimension Toffee_geometry.size
  (** [px ~w ~h] constructs a size in pixels using {!Dimension.length}. *)

  val pct : w:float -> h:float -> dimension Toffee_geometry.size
  (** [pct ~w ~h] constructs a percentage size from 0–100 inputs using
      {!Dimension.pct}.

      For example, [pct ~w:50. ~h:100.] is equivalent to a size where width is
      [50%] and height is [100%]. *)
end

(** {1 Main Style type} *)

type t
(** The main style struct representing CSS style properties. *)

val default : t
(** Default style values. *)

(** {2 Constructor} *)

val make :
  ?display:display ->
  ?box_sizing:box_sizing ->
  ?position:position ->
  ?overflow:overflow Toffee_geometry.point ->
  ?scrollbar_width:float ->
  ?text_align:text_align ->
  ?inset:length_percentage_auto Toffee_geometry.rect ->
  ?size:dimension Toffee_geometry.size ->
  ?min_size:dimension Toffee_geometry.size ->
  ?max_size:dimension Toffee_geometry.size ->
  ?aspect_ratio:float ->
  ?margin:length_percentage_auto Toffee_geometry.rect ->
  ?padding:length_percentage Toffee_geometry.rect ->
  ?border:length_percentage Toffee_geometry.rect ->
  ?gap:length_percentage Toffee_geometry.size ->
  ?align_items:align_items ->
  ?align_self:align_self ->
  ?align_content:align_content ->
  ?justify_items:justify_items ->
  ?justify_self:justify_self ->
  ?justify_content:justify_content ->
  ?flex_direction:flex_direction ->
  ?flex_wrap:flex_wrap ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:grid_template_component list ->
  ?grid_template_columns:grid_template_component list ->
  ?grid_auto_rows:track_sizing_function list ->
  ?grid_auto_columns:track_sizing_function list ->
  ?grid_auto_flow:grid_auto_flow ->
  ?grid_template_areas:grid_template_area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:grid_placement Toffee_geometry.line ->
  ?grid_column:grid_placement Toffee_geometry.line ->
  unit ->
  t
(** [make ...] creates a style with optional parameters, defaulting to
    {!default} for unspecified fields. *)

(** {2 Accessors}

    Property accessors return the current value of style properties. See the
    corresponding module for property semantics. *)

val display : t -> display
(** [display t] returns the display mode. See {!Display}. *)

val item_is_table : t -> bool
(** [item_is_table t] returns [true] if the item is a table element. *)

val box_generation_mode : t -> box_generation_mode
(** [box_generation_mode t] returns the box generation mode. See
    {!Box_generation_mode}. *)

val set_box_generation_mode : box_generation_mode -> t -> t
(** [set_box_generation_mode mode t] returns a style with updated box generation
    mode. *)

val is_compressible_replaced : t -> bool
(** [is_compressible_replaced t] returns [true] if the item is a compressible
    replaced element. *)

val text_align : t -> text_align
(** [text_align t] returns the text alignment. See {!Text_align}. *)

val item_is_replaced : t -> bool
(** [item_is_replaced t] is [true] if the item is a replaced element, such as an
    image: its content is outside the layout, and its measure function gives its
    natural size. A block container does not stretch it to its width (CSS 2
    §10.3.4), and a grid's [normal] alignment, where neither the item nor the
    container sets one, sizes it as [Start] does (CSS Grid §6.2); a flex
    container stretches it as any item. Its measure function is given the size
    the layout fixes, or else its style's, in a measurement and in a final
    layout alike, so that it can set one dimension from the other through the
    element's ratio. Its natural ratio belongs in {!aspect_ratio} too, where the
    layouts read it. Default [false]. *)

val box_sizing : t -> box_sizing
(** [box_sizing t] returns the box sizing mode. See {!Box_sizing}. *)

val overflow : t -> overflow Toffee_geometry.point
(** [overflow t] returns the overflow behavior for both axes. See {!Overflow}.
*)

val scrollbar_width : t -> float
(** [scrollbar_width t] returns the scrollbar width in pixels. *)

val position : t -> position
(** [position t] returns the positioning mode. See {!Position}. *)

val inset : t -> length_percentage_auto Toffee_geometry.rect
(** [inset t] returns the inset values (top, right, bottom, left). *)

val size : t -> dimension Toffee_geometry.size
(** [size t] returns the explicit size (width, height). See {!Dimension}. *)

val min_size : t -> dimension Toffee_geometry.size
(** [min_size t] returns the minimum size constraints. *)

val max_size : t -> dimension Toffee_geometry.size
(** [max_size t] returns the maximum size constraints. *)

val aspect_ratio : t -> float option
(** [aspect_ratio t] returns the aspect ratio constraint, if any. *)

val margin : t -> length_percentage_auto Toffee_geometry.rect
(** [margin t] returns the margin values. See {!Length_percentage_auto}. *)

val padding : t -> length_percentage Toffee_geometry.rect
(** [padding t] returns the padding values. See {!Length_percentage}. *)

val border : t -> length_percentage Toffee_geometry.rect
(** [border t] returns the border widths. *)

val gap : t -> length_percentage Toffee_geometry.size
(** [gap t] returns the gap between items (row_gap, column_gap). *)

val align_items : t -> align_items option
(** [align_items t] returns the cross-axis alignment for children. See
    {!Align_items}. *)

val align_self : t -> align_self option
(** [align_self t] returns the self cross-axis alignment override. See
    {!Align_self}. *)

val align_content : t -> align_content option
(** [align_content t] returns the content alignment in the cross axis. See
    {!Align_content}. *)

val justify_items : t -> justify_items option
(** [justify_items t] returns the inline-axis alignment for children (Grid
    only). See {!Justify_items}. *)

val justify_self : t -> justify_self option
(** [justify_self t] returns the self inline-axis alignment override. See
    {!Justify_self}. *)

val justify_content : t -> justify_content option
(** [justify_content t] returns the content alignment in the main/inline axis.
    See {!Justify_content}. *)

val flex_direction : t -> flex_direction
(** [flex_direction t] returns the flex container direction. See
    {!Flex_direction}. *)

val flex_wrap : t -> flex_wrap
(** [flex_wrap t] returns the flex wrap behavior. See {!Flex_wrap}. *)

val flex_grow : t -> float
(** [flex_grow t] returns the flex grow factor. *)

val flex_shrink : t -> float
(** [flex_shrink t] returns the flex shrink factor. *)

val flex_basis : t -> dimension
(** [flex_basis t] returns the initial main size before growing/shrinking. *)

val grid_template_rows : t -> grid_template_component list
(** [grid_template_rows t] returns the explicit row track definitions. See
    {!Grid_template_component}. *)

val grid_template_columns : t -> grid_template_component list
(** [grid_template_columns t] returns the explicit column track definitions. *)

val grid_auto_rows : t -> track_sizing_function list
(** [grid_auto_rows t] returns the implicit row track sizing. See
    {!Track_sizing_function}. *)

val grid_auto_columns : t -> track_sizing_function list
(** [grid_auto_columns t] returns the implicit column track sizing. *)

val grid_auto_flow : t -> grid_auto_flow
(** [grid_auto_flow t] returns the auto-placement algorithm. See
    {!Grid_auto_flow}. *)

val grid_template_areas : t -> grid_template_area list
(** [grid_template_areas t] returns the named grid areas. See
    {!Grid_template_area}. *)

val grid_template_column_names : t -> string list list
(** [grid_template_column_names t] returns the named column lines. *)

val grid_template_row_names : t -> string list list
(** [grid_template_row_names t] returns the named row lines. *)

val grid_row : t -> grid_placement Toffee_geometry.line
(** [grid_row t] returns the row placement (start, end). See {!Grid_placement}.
*)

val grid_column : t -> grid_placement Toffee_geometry.line
(** [grid_column t] returns the column placement (start, end). *)

(** {2 Functional updates}

    Setters return a new style with the specified property updated. All other
    properties remain unchanged. *)

val set_display : display -> t -> t
(** [set_display v t] returns a style with updated display mode. *)

val set_position : position -> t -> t
(** [set_position v t] returns a style with updated positioning mode. *)

val set_box_sizing : box_sizing -> t -> t
(** [set_box_sizing v t] returns a style with updated box sizing mode. *)

val set_overflow : overflow Toffee_geometry.point -> t -> t
(** [set_overflow v t] returns a style with updated overflow behavior. *)

val set_scrollbar_width : float -> t -> t
(** [set_scrollbar_width v t] returns a style with updated scrollbar width. *)

val set_text_align : text_align -> t -> t
(** [set_text_align v t] returns a style with updated text alignment. *)

val set_inset : length_percentage_auto Toffee_geometry.rect -> t -> t
(** [set_inset v t] returns a style with updated inset values. *)

val set_size : dimension Toffee_geometry.size -> t -> t
(** [set_size v t] returns a style with updated explicit size. *)

val set_width : dimension -> t -> t
(** [set_width v t] returns a style with [size.width] updated to [v]. *)

val set_height : dimension -> t -> t
(** [set_height v t] returns a style with [size.height] updated to [v]. *)

val set_min_size : dimension Toffee_geometry.size -> t -> t
(** [set_min_size v t] returns a style with updated minimum size. *)

val set_min_width : dimension -> t -> t
(** [set_min_width v t] returns a style with [min_size.width] updated to [v]. *)

val set_min_height : dimension -> t -> t
(** [set_min_height v t] returns a style with [min_size.height] updated to [v].
*)

val set_max_size : dimension Toffee_geometry.size -> t -> t
(** [set_max_size v t] returns a style with updated maximum size. *)

val set_max_width : dimension -> t -> t
(** [set_max_width v t] returns a style with [max_size.width] updated to [v]. *)

val set_max_height : dimension -> t -> t
(** [set_max_height v t] returns a style with [max_size.height] updated to [v].
*)

val set_aspect_ratio : float option -> t -> t
(** [set_aspect_ratio v t] returns a style with updated aspect ratio. *)

val set_item_is_replaced : bool -> t -> t
(** [set_item_is_replaced v t] returns a style with {!item_is_replaced} set to
    [v]. *)

val set_margin : length_percentage_auto Toffee_geometry.rect -> t -> t
(** [set_margin v t] returns a style with updated margin values. *)

val set_margin_left : length_percentage_auto -> t -> t
(** [set_margin_left v t] returns a style with [margin.left] updated to [v]. *)

val set_margin_right : length_percentage_auto -> t -> t
(** [set_margin_right v t] returns a style with [margin.right] updated to [v].
*)

val set_margin_top : length_percentage_auto -> t -> t
(** [set_margin_top v t] returns a style with [margin.top] updated to [v]. *)

val set_margin_bottom : length_percentage_auto -> t -> t
(** [set_margin_bottom v t] returns a style with [margin.bottom] updated to [v].
*)

val set_margin_x : length_percentage_auto -> t -> t
(** [set_margin_x v t] returns a style with both [margin.left] and
    [margin.right] updated to [v]. *)

val set_margin_y : length_percentage_auto -> t -> t
(** [set_margin_y v t] returns a style with both [margin.top] and
    [margin.bottom] updated to [v]. *)

val set_padding : length_percentage Toffee_geometry.rect -> t -> t
(** [set_padding v t] returns a style with updated padding values. *)

val set_padding_left : length_percentage -> t -> t
(** [set_padding_left v t] returns a style with [padding.left] updated to [v].
*)

val set_padding_right : length_percentage -> t -> t
(** [set_padding_right v t] returns a style with [padding.right] updated to [v].
*)

val set_padding_top : length_percentage -> t -> t
(** [set_padding_top v t] returns a style with [padding.top] updated to [v]. *)

val set_padding_bottom : length_percentage -> t -> t
(** [set_padding_bottom v t] returns a style with [padding.bottom] updated to
    [v]. *)

val set_padding_x : length_percentage -> t -> t
(** [set_padding_x v t] returns a style with both [padding.left] and
    [padding.right] updated to [v]. *)

val set_padding_y : length_percentage -> t -> t
(** [set_padding_y v t] returns a style with both [padding.top] and
    [padding.bottom] updated to [v]. *)

val set_border : length_percentage Toffee_geometry.rect -> t -> t
(** [set_border v t] returns a style with updated border widths. *)

val set_gap : length_percentage Toffee_geometry.size -> t -> t
(** [set_gap v t] returns a style with updated gap between items. *)

val set_align_items : align_items option -> t -> t
(** [set_align_items v t] returns a style with updated child alignment. *)

val set_align_self : align_self option -> t -> t
(** [set_align_self v t] returns a style with updated self alignment. *)

val set_align_content : align_content option -> t -> t
(** [set_align_content v t] returns a style with updated content alignment. *)

val set_justify_items : justify_items option -> t -> t
(** [set_justify_items v t] returns a style with updated justify items. *)

val set_justify_self : justify_self option -> t -> t
(** [set_justify_self v t] returns a style with updated justify self. *)

val set_justify_content : justify_content option -> t -> t
(** [set_justify_content v t] returns a style with updated justify content. *)

val set_flex_direction : flex_direction -> t -> t
(** [set_flex_direction v t] returns a style with updated flex direction. *)

val set_flex_wrap : flex_wrap -> t -> t
(** [set_flex_wrap v t] returns a style with updated flex wrap. *)

val set_flex_grow : float -> t -> t
(** [set_flex_grow v t] returns a style with updated flex grow factor. *)

val set_flex_shrink : float -> t -> t
(** [set_flex_shrink v t] returns a style with updated flex shrink factor. *)

val set_flex_basis : dimension -> t -> t
(** [set_flex_basis v t] returns a style with updated flex basis. *)

val set_grid_template_rows : grid_template_component list -> t -> t
(** [set_grid_template_rows v t] returns a style with updated row templates. *)

val set_grid_template_columns : grid_template_component list -> t -> t
(** [set_grid_template_columns v t] returns a style with updated column
    templates. *)

val set_grid_auto_rows : track_sizing_function list -> t -> t
(** [set_grid_auto_rows v t] returns a style with updated auto row sizing. *)

val set_grid_auto_columns : track_sizing_function list -> t -> t
(** [set_grid_auto_columns v t] returns a style with updated auto column sizing.
*)

val set_grid_auto_flow : grid_auto_flow -> t -> t
(** [set_grid_auto_flow v t] returns a style with updated auto-flow algorithm.
*)

val set_grid_template_areas : grid_template_area list -> t -> t
(** [set_grid_template_areas v t] returns a style with updated named areas. *)

val set_grid_template_column_names : string list list -> t -> t
(** [set_grid_template_column_names v t] returns a style with updated column
    line names. *)

val set_grid_template_row_names : string list list -> t -> t
(** [set_grid_template_row_names v t] returns a style with updated row line
    names. *)

val set_grid_row : grid_placement Toffee_geometry.line -> t -> t
(** [set_grid_row v t] returns a style with updated row placement. *)

val set_grid_column : grid_placement Toffee_geometry.line -> t -> t
(** [set_grid_column v t] returns a style with updated column placement. *)

(** {2 Common style patterns} *)

val flex_row : t
(** Style with flex display and row direction. *)

val flex_column : t
(** Style with flex display and column direction. *)

val absolute : t
(** Style with absolute positioning. *)

val flex_1 : t
(** Style with flex_grow=1, flex_shrink=1, flex_basis=0. *)
