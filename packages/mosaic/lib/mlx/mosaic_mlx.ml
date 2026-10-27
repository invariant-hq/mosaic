include Mosaic

let fragment ?(children = []) () = Mosaic.fragment children

let box ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?inert ?on_mouse ?on_key
    ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?border
    ?border_style ?border_sides ?border_color ?focused_border_color ?background
    ?fill ?title ?title_alignment ?(children = []) () =
  Mosaic.box ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?inert ?on_mouse ?on_key
    ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?border
    ?border_style ?border_sides ?border_color ?focused_border_color ?background
    ?fill ?title ?title_alignment children

let button ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_focus_request ?on_size
    ?focused ?border ?border_style ?border_sides ?border_color
    ?focused_border_color ?background ?fill ?title ?title_alignment ?text_style
    ?disabled ~on_activate ?(children = []) () =
  Mosaic.button ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_focus_request ?on_size
    ?focused ?border ?border_style ?border_sides ?border_color
    ?focused_border_color ?background ?fill ?title ?title_alignment ?text_style
    ?disabled ~on_activate
    (String.concat "" children)

let scroll_box ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?scroll_x ?scroll_y
    ?background ?show_scrollbars ?scrollbar_style ?vertical_bar_style
    ?horizontal_bar_style ~on_scroll ~scroll ?(children = []) () =
  Mosaic.scroll_box ?key ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
    ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?scroll_x
    ?scroll_y ?background ?show_scrollbars ?scrollbar_style ?vertical_bar_style
    ?horizontal_bar_style ~on_scroll ~scroll children

let text ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?style ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key
    ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?text_style ?wrap
    ?selectable ?selection_bg ?selection_fg ?tab_width ?truncate
    ?(children = []) () =
  Mosaic.text ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?style ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key
    ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?text_style ?wrap
    ?selectable ?selection_bg ?selection_fg ?tab_width ?truncate
    (String.concat "" children)

let rich_text ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?style ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key
    ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?text_style ?wrap
    ?selectable ?selection_bg ?selection_fg ?tab_width ?truncate
    ?(children = []) () =
  Mosaic.rich_text ?key ?display ?box_sizing ?position ?overflow
    ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
    ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio ?gap
    ?padding ?margin ?border_width ?align_self ?align_content ?justify_items
    ?justify_self ?flex_grow ?flex_shrink ?flex_basis ?grid_template_rows
    ?grid_template_columns ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow
    ?grid_template_areas ?grid_template_column_names ?grid_template_row_names
    ?grid_row ?grid_column ?style ?visible ?z_index ?opacity ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused
    ?text_style ?wrap ?selectable ?selection_bg ?selection_fg ?tab_width
    ?truncate children

let code ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?syntax ?conceal ?text_style
    ?wrap ?tab_width ?selectable ?selection_bg ?selection_fg ?on_selection
    ?(children = []) () =
  Mosaic.code ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?syntax ?conceal ?text_style
    ?wrap ?tab_width ?selectable ?selection_bg ?selection_fg ?on_selection
    (String.concat "" children)

let markdown ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?md_style ?conceal ?streaming
    ?selectable ?selection_bg ?selection_fg ?highlighter ?syntax_style
    ?on_selection ?(children = []) () =
  Mosaic.markdown ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?md_style ?conceal ?streaming
    ?selectable ?selection_bg ?selection_fg ?highlighter ?syntax_style
    ?on_selection
    (String.concat "" children)

let input ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?placeholder ?text_color
    ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?selectable ?show_cursor ?key_bindings
    ~on_change ?on_submit ~state ?(children = []) () =
  ignore children;
  Mosaic.input ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?placeholder ?text_color
    ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?selectable ?show_cursor ?key_bindings
    ~on_change ?on_submit ~state ()

let textarea ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?annotations ?syntax
    ?ghost_text ?ghost_text_color ?placeholder ?wrap ?text_color
    ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?selectable ?show_cursor ?key_bindings
    ~on_change ?on_submit ~state ?(children = []) () =
  ignore children;
  Mosaic.textarea ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?annotations ?syntax
    ?ghost_text ?ghost_text_color ?placeholder ?wrap ?text_color
    ?background_color ?focused_text_color ?focused_background_color
    ?placeholder_color ?selection_color ?selection_fg ?cursor_style
    ?cursor_color ?cursor_blinking ?selectable ?show_cursor ?key_bindings
    ~on_change ?on_submit ~state ()

let line_number ?key ?display ?box_sizing ?position ?overflow ?scrollbar_width
    ?text_align ?inset ?flex_direction ?flex_wrap ?justify_content ?align_items
    ?size ?min_size ?max_size ?aspect_ratio ?gap ?padding ?margin ?border_width
    ?align_self ?align_content ?justify_items ?justify_self ?flex_grow
    ?flex_shrink ?flex_basis ?grid_template_rows ?grid_template_columns
    ?grid_auto_rows ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
    ?grid_template_column_names ?grid_template_row_names ?grid_row ?grid_column
    ?visible ?z_index ?opacity ?buffered ?live ?on_mouse ?on_key ?on_paste
    ?on_click ?on_focus_request ?on_size ?focused ?fg ?bg ?min_width
    ?padding_right ?show_line_numbers ?line_number_offset ?line_colors
    ?line_signs ?hidden_line_numbers ?(children = []) () =
  match children with
  | [ child ] ->
      Mosaic.line_number ?key ?display ?box_sizing ?position ?overflow
        ?scrollbar_width ?text_align ?inset ?flex_direction ?flex_wrap
        ?justify_content ?align_items ?size ?min_size ?max_size ?aspect_ratio
        ?gap ?padding ?margin ?border_width ?align_self ?align_content
        ?justify_items ?justify_self ?flex_grow ?flex_shrink ?flex_basis
        ?grid_template_rows ?grid_template_columns ?grid_auto_rows
        ?grid_auto_columns ?grid_auto_flow ?grid_template_areas
        ?grid_template_column_names ?grid_template_row_names ?grid_row
        ?grid_column ?visible ?z_index ?opacity ?buffered ?live ?on_mouse
        ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ?focused ?fg ?bg
        ?min_width ?padding_right ?show_line_numbers ?line_number_offset
        ?line_colors ?line_signs ?hidden_line_numbers child
  | children ->
      invalid_arg
        (Printf.sprintf
           "Mosaic_mlx.line_number: expected exactly one child, got %d"
           (List.length children))
