(** MLX-compatible wrappers for {!Mosaic}.

    This module re-exports the full {!Mosaic} API with child arguments changed
    from positional to optional, matching the MLX JSX convention where children
    are passed as [~children].

    All types, modules, helpers, and non-overridden elements are inherited
    unchanged from {!Mosaic}; see that module for their documentation. Only the
    elements listed below are overridden. *)

include module type of Mosaic

(** {1:overrides Overridden elements}

    Each element below replaces its {!Mosaic} counterpart solely to turn the
    positional [children] argument into [?children] with a trailing [unit]. All
    other arguments, defaults, and semantics are identical to {!Mosaic}; refer
    to the corresponding entry there for details.

    For text-based elements ({!val-text}, {!val-code}, {!val-markdown})
    [children] is [string list] whose items are concatenated. *)

val fragment : ?children:'msg t list -> unit -> 'msg t
(** Like {!Mosaic.fragment}. [children] defaults to [[]]. *)

val box :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?border:bool ->
  ?border_style:Border.t ->
  ?border_sides:Border.side list ->
  ?border_color:Ansi.Color.t ->
  ?focused_border_color:Ansi.Color.t ->
  ?background:Ansi.Color.t ->
  ?fill:bool ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  ?children:'msg t list ->
  unit ->
  'msg t
(** Like {!Mosaic.box}. [children] defaults to [[]]. *)

val text :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?style:Ansi.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?tab_width:int ->
  ?truncate:bool ->
  ?children:string list ->
  unit ->
  'msg t
(** Like {!Mosaic.text}. [children] is a [string list] whose items are
    concatenated. Defaults to [[]]. *)

val scroll_box :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?scroll_x:bool ->
  ?scroll_y:bool ->
  ?sticky_scroll:bool ->
  ?sticky_start:[ `Top | `Bottom | `Left | `Right ] ->
  ?background:Ansi.Color.t ->
  ?on_scroll:(x:int -> y:int -> 'msg option) ->
  ?children:'msg t list ->
  unit ->
  'msg t
(** Like {!Mosaic.scroll_box}. [children] defaults to [[]]. *)

val code :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?spans:span list ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?tab_width:int ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?on_selection:((int * int) option -> 'msg option) ->
  ?children:string list ->
  unit ->
  'msg t
(** Like {!Mosaic.code}. [children] is a [string list] whose items are
    concatenated. Defaults to [[]]. *)

val markdown :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?md_style:Markdown.style ->
  ?conceal:bool ->
  ?streaming:bool ->
  ?children:string list ->
  unit ->
  'msg t
(** Like {!Mosaic.markdown}. [children] is a [string list] whose items are
    concatenated. Defaults to [[]]. *)

val input :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?value:string ->
  ?cursor:int ->
  ?selection:(int * int) option ->
  ?placeholder:string ->
  ?max_length:int ->
  ?text_color:Ansi.Color.t ->
  ?background_color:Ansi.Color.t ->
  ?focused_text_color:Ansi.Color.t ->
  ?focused_background_color:Ansi.Color.t ->
  ?placeholder_color:Ansi.Color.t ->
  ?selection_color:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?cursor_style:[ `Block | `Line | `Underline ] ->
  ?cursor_color:Ansi.Color.t ->
  ?cursor_blinking:bool ->
  ?on_input:(string -> 'msg option) ->
  ?on_change:(string -> 'msg option) ->
  ?on_submit:(string -> 'msg option) ->
  ?on_cursor:(cursor:int -> selection:(int * int) option -> 'msg option) ->
  ?children:'msg t list ->
  unit ->
  'msg t
(** Like {!Mosaic.input}. [children] defaults to [[]]. *)

val textarea :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?value:string ->
  ?cursor:int ->
  ?selection:(int * int) option ->
  ?spans:Mosaic.span list ->
  ?ghost_text:string ->
  ?ghost_text_color:Ansi.Color.t ->
  ?placeholder:string ->
  ?wrap:Text_surface.wrap ->
  ?text_color:Ansi.Color.t ->
  ?background_color:Ansi.Color.t ->
  ?focused_text_color:Ansi.Color.t ->
  ?focused_background_color:Ansi.Color.t ->
  ?placeholder_color:Ansi.Color.t ->
  ?selection_color:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?cursor_style:[ `Block | `Line | `Underline ] ->
  ?cursor_color:Ansi.Color.t ->
  ?cursor_blinking:bool ->
  ?on_input:(string -> 'msg option) ->
  ?on_change:(string -> 'msg option) ->
  ?on_submit:(string -> 'msg option) ->
  ?on_cursor:(cursor:int -> selection:(int * int) option -> 'msg option) ->
  ?children:'msg t list ->
  unit ->
  'msg t
(** Like {!Mosaic.textarea}. [children] defaults to [[]]. *)

val line_number :
  ?key:string ->
  ?id:string ->
  ?display:Display.t ->
  ?box_sizing:Box_sizing.t ->
  ?position:Position.t ->
  ?overflow:Overflow.t point ->
  ?scrollbar_width:float ->
  ?text_align:Text_align.t ->
  ?inset:length_percentage_auto rect ->
  ?flex_direction:Flex_direction.t ->
  ?flex_wrap:Flex_wrap.t ->
  ?justify_content:Justify.t ->
  ?align_items:Align.t ->
  ?size:dimension size ->
  ?min_size:dimension size ->
  ?max_size:dimension size ->
  ?aspect_ratio:float ->
  ?gap:length_percentage size ->
  ?padding:length_percentage rect ->
  ?margin:length_percentage_auto rect ->
  ?border_width:length_percentage rect ->
  ?align_self:Align.t ->
  ?align_content:Justify.t ->
  ?justify_items:Align.t ->
  ?justify_self:Align.t ->
  ?flex_grow:float ->
  ?flex_shrink:float ->
  ?flex_basis:dimension ->
  ?grid_template_rows:Grid.template list ->
  ?grid_template_columns:Grid.template list ->
  ?grid_auto_rows:Grid.track list ->
  ?grid_auto_columns:Grid.track list ->
  ?grid_auto_flow:Grid_auto_flow.t ->
  ?grid_template_areas:Grid.area list ->
  ?grid_template_column_names:string list list ->
  ?grid_template_row_names:string list list ->
  ?grid_row:Grid.placement line ->
  ?grid_column:Grid.placement line ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?focusable:bool ->
  ?autofocus:bool ->
  ?buffered:bool ->
  ?live:bool ->
  ?ref:(Mosaic_ui.Renderable.t -> unit) ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?fg:Ansi.Color.t ->
  ?bg:Ansi.Color.t ->
  ?min_width:int ->
  ?padding_right:int ->
  ?show_line_numbers:bool ->
  ?line_number_offset:int ->
  ?line_colors:(int * Line_number.line_color) list ->
  ?line_signs:(int * Line_number.line_sign) list ->
  ?hidden_line_numbers:int list ->
  ?children:'msg t list ->
  unit ->
  'msg t
(** Like {!Mosaic.line_number}. [children] defaults to [[]]; pass the wrapped
    element (e.g. a {!val-code}) as a single-item list. *)
