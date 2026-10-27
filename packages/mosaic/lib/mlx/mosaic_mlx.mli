(** MLX-compatible wrappers for {!Mosaic}.

    This module re-exports the full {!Mosaic} API with child arguments changed
    from positional to optional, matching the MLX JSX convention where children
    are passed as [~children].

    All types, modules, helpers, and non-overridden elements are inherited
    unchanged from {!Mosaic}; see that module for their documentation. Only the
    elements listed below are overridden. *)

include module type of struct
  include Mosaic
end

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
  ?buffered:bool ->
  ?live:bool ->
  ?inert:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
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

val button :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?border:bool ->
  ?border_style:Border.t ->
  ?border_sides:Border.side list ->
  ?border_color:Ansi.Color.t ->
  ?focused_border_color:Ansi.Color.t ->
  ?background:Ansi.Color.t ->
  ?fill:bool ->
  ?title:string ->
  ?title_alignment:[ `Left | `Center | `Right ] ->
  ?text_style:Ansi.Style.t ->
  ?disabled:bool ->
  on_activate:'msg ->
  ?children:string list ->
  unit ->
  'msg t
(** Like {!Mosaic.button}. [children] defaults to [[]] and is concatenated to
    form the label. *)

val scroll_box :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?scroll_x:bool ->
  ?scroll_y:bool ->
  ?background:Ansi.Color.t ->
  ?show_scrollbars:bool ->
  ?scrollbar_style:Scroll_bar.style ->
  ?vertical_bar_style:Scroll_bar.style ->
  ?horizontal_bar_style:Scroll_bar.style ->
  on_scroll:(Scroll.t -> 'msg) ->
  scroll:Scroll.t ->
  ?children:'msg t list ->
  unit ->
  'msg t
(** Like {!Mosaic.scroll_box}. [children] defaults to [[]]. [scrollbar_style]
    sets shared bar appearance; [vertical_bar_style] and [horizontal_bar_style]
    replace it for the corresponding axis. *)

val text :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
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
(** Like {!Mosaic.text}. [children] defaults to [[]]. *)

val rich_text :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?tab_width:int ->
  ?truncate:bool ->
  ?children:Text.fragment list ->
  unit ->
  'msg t
(** [rich_text ~children:fragments ()] renders structured text. Children use
    {!Text.Fragment} constructors, retaining nested style inheritance. All
    labelled options have the same meaning as {!text}. *)

val code :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?syntax:Code.syntax ->
  ?conceal:bool ->
  ?text_style:Ansi.Style.t ->
  ?wrap:Text_surface.wrap ->
  ?tab_width:int ->
  ?truncate:bool ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?on_selection:((int * int) option -> 'msg option) ->
  ?children:string list ->
  unit ->
  'msg t
(** Like {!Mosaic.code}. [children] defaults to [[]]. *)

val markdown :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?md_style:Markdown.Style.t ->
  ?conceal:bool ->
  ?streaming:bool ->
  ?selectable:bool ->
  ?selection_bg:Ansi.Color.t ->
  ?selection_fg:Ansi.Color.t ->
  ?highlighter:Code.Highlighter.t ->
  ?syntax_style:Syntax_style.t ->
  ?on_selection:(string option -> 'msg option) ->
  ?children:string list ->
  unit ->
  'msg t
(** Like {!Mosaic.markdown}. [children] defaults to [[]]. *)

val input :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?placeholder:string ->
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
  ?selectable:bool ->
  ?show_cursor:bool ->
  ?key_bindings:Mosaic_ui.Text_input.key_binding list ->
  on_change:(Editor.t -> 'msg) ->
  ?on_submit:(string -> 'msg) ->
  state:Editor.t ->
  ?children:'msg t list ->
  unit ->
  'msg t
(** Like {!Mosaic.input}. [children] defaults to [[]]. Children are ignored;
    text is supplied by [state]. *)

val textarea :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
  ?annotations:Mosaic_ui.Text_buffer.Annotations.t ->
  ?syntax:Code.syntax ->
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
  ?selectable:bool ->
  ?show_cursor:bool ->
  ?key_bindings:Mosaic_ui.Textarea.key_binding list ->
  on_change:(Editor.t -> 'msg) ->
  ?on_submit:(string -> 'msg) ->
  state:Editor.t ->
  ?children:'msg t list ->
  unit ->
  'msg t
(** Like {!Mosaic.textarea}. [children] defaults to [[]]. Children are ignored;
    text is supplied by [state]. *)

val line_number :
  ?key:string ->
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
  ?buffered:bool ->
  ?live:bool ->
  ?on_mouse:(Event.mouse -> 'msg option) ->
  ?on_key:(Event.key -> 'msg option) ->
  ?on_paste:(Event.paste -> 'msg option) ->
  ?on_click:(Event.mouse -> 'msg option) ->
  ?on_focus_request:'msg ->
  ?on_size:(w:int -> h:int -> 'msg option) ->
  ?focused:bool ->
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
(** Like {!Mosaic.line_number}. [children] defaults to [[]]. Raises
    [Invalid_argument] unless [children] has exactly one element. *)
