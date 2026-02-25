include Mosaic

let fragment ?(children = []) () = Mosaic.fragment children

let box ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?border ?border_style ?border_sides
    ?border_color ?focused_border_color ?background ?fill ?title
    ?title_alignment ?(children = []) () =
  Mosaic.box ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?border ?border_style ?border_sides
    ?border_color ?focused_border_color ?background ?fill ?title
    ?title_alignment children

let scroll_box ?key ?id ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?scroll_x ?scroll_y
    ?sticky_scroll ?sticky_start ?background ?on_scroll ?(children = []) () =
  Mosaic.scroll_box ?key ?id ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?scroll_x ?scroll_y
    ?sticky_scroll ?sticky_start ?background ?on_scroll children

let text ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?style ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?text_style ?wrap ?selectable ?selection_bg
    ?selection_fg ?tab_width ?truncate ?(children = []) () =
  Mosaic.text ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?style ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?text_style ?wrap ?selectable ?selection_bg
    ?selection_fg ?tab_width ?truncate
    (String.concat "" children)

let code ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?spans ?text_style ?wrap ?tab_width
    ?selectable ?selection_bg ?selection_fg ?on_selection ?(children = []) () =
  Mosaic.code ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?spans ?text_style ?wrap ?tab_width
    ?selectable ?selection_bg ?selection_fg ?on_selection
    (String.concat "" children)

let markdown ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?md_style ?conceal ?streaming ?(children = [])
    () =
  Mosaic.markdown ?key ?id ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?md_style ?conceal
    ?streaming
    (String.concat "" children)

let input ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?value ?cursor ?selection ?placeholder
    ?max_length ?text_color ?background_color ?focused_text_color
    ?focused_background_color ?placeholder_color ?selection_color ?selection_fg
    ?cursor_style ?cursor_color ?cursor_blinking ?on_input ?on_change ?on_submit
    ?on_cursor ?children:_ () =
  Mosaic.input ?key ?id ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?value ?cursor ?selection
    ?placeholder ?max_length ?text_color ?background_color ?focused_text_color
    ?focused_background_color ?placeholder_color ?selection_color ?selection_fg
    ?cursor_style ?cursor_color ?cursor_blinking ?on_input ?on_change ?on_submit
    ?on_cursor ()

let textarea ?key ?id ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered ?live ?ref
    ?on_mouse ?on_key ?on_paste ?value ?cursor ?selection ?spans
    ?ghost_text ?ghost_text_color ?placeholder ?wrap ?text_color
    ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?on_input ?on_change ?on_submit ?on_cursor
    ?children:_ () =
  Mosaic.textarea ?key ?id ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?value ?cursor ?selection
    ?spans ?ghost_text ?ghost_text_color ?placeholder ?wrap ?text_color
    ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?on_input ?on_change ?on_submit ?on_cursor ()

let line_number ?key ?id ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?focusable ?autofocus
    ?buffered ?live ?ref ?on_mouse ?on_key ?on_paste ?fg ?bg ?min_width
    ?padding_right ?show_line_numbers ?line_number_offset ?line_colors
    ?line_signs ?hidden_line_numbers ?(children = []) () =
  match children with
  | [ child ] ->
      Mosaic.line_number ?key ?id ?display ?box_sizing ?position ?overflow
        ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
        ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio
        ?gap ?padding ?margin ?border_width ?align_self ?align_content
        ?justify_items ?justify_self ?flex_grow ?flex_shrink ?flex_basis
        ?grid_template_rows ?grid_template_columns ?grid_auto_rows
        ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
        ?grid_template_column_names ?grid_template_row_names ?grid_row
        ?grid_column ?visible ?z_index ?opacity ?focusable ?autofocus ?buffered
        ?live ?ref ?on_mouse ?on_key ?on_paste ?fg ?bg ?min_width ?padding_right
        ?show_line_numbers ?line_number_offset ?line_colors ?line_signs
        ?hidden_line_numbers child
  | _ -> Mosaic.empty
