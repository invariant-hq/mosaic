open Windtrap
open Mosaic_ui

(* ── Helpers ── *)

let red = Ansi.Color.red
let blue = Ansi.Color.blue

let make_line_number ?line_colors ?line_signs ?line_numbers ?hidden_line_numbers
    ?show_line_numbers () =
  let renderer = Renderer.create () in
  let ln =
    Line_number.create ~parent:(Renderer.root renderer) ?line_colors ?line_signs
      ?line_numbers ?hidden_line_numbers ?show_line_numbers ()
  in
  let (_ : Text.t) =
    Text.create ~parent:(Line_number.node ln) ~content:"alpha\nbeta\ngamma" ()
  in
  (renderer, ln)

let render renderer =
  Renderer.render_frame renderer ~width:20 ~height:3 ~dt:0.;
  ignore (Renderer.render ~full:true renderer : string);
  Matrix_screen.current_grid (Renderer.screen renderer)

(* ── Props ── *)

let props_equal_detects_line_color_diff () =
  let a =
    Line_number.Props.make
      ~line_colors:[ (0, { Line_number.gutter = red; content = None }) ]
      ()
  in
  let b = Line_number.Props.make () in
  is_false ~msg:"different" (Line_number.Props.equal a b);
  is_true ~msg:"equal" (Line_number.Props.equal a a)

let props_equal_detects_length_mismatch () =
  let a = Line_number.Props.make ~hidden_line_numbers:[ 1 ] () in
  let b = Line_number.Props.make ~hidden_line_numbers:[ 1; 2 ] () in
  is_false ~msg:"different lengths" (Line_number.Props.equal a b);
  is_false ~msg:"symmetric" (Line_number.Props.equal b a)

(* ── Rendering ── *)

let line_colors_render_in_gutter () =
  let renderer, _ln =
    make_line_number
      ~line_colors:[ (1, { Line_number.gutter = red; content = None }) ]
      ()
  in
  let grid = render renderer in
  let bg_of y = Matrix_grid.get_bg grid (Matrix_grid.idx grid ~x:0 ~y) in
  is_false ~msg:"row 0 uncolored" (Ansi.Color.equal red (bg_of 0));
  is_true ~msg:"row 1 colored" (Ansi.Color.equal red (bg_of 1))

let apply_props_refreshes_line_colors () =
  (* Row lookups are cached; a props update must invalidate the cache. *)
  let renderer, ln =
    make_line_number
      ~line_colors:[ (1, { Line_number.gutter = red; content = None }) ]
      ()
  in
  let (_ : Matrix_grid.t) = render renderer in
  Line_number.apply_props ln
    (Line_number.Props.make
       ~line_colors:[ (2, { Line_number.gutter = blue; content = None }) ]
       ());
  let grid = render renderer in
  let bg_of y = Matrix_grid.get_bg grid (Matrix_grid.idx grid ~x:0 ~y) in
  is_false ~msg:"old row no longer colored" (Ansi.Color.equal red (bg_of 1));
  is_true ~msg:"new row colored" (Ansi.Color.equal blue (bg_of 2))

let signs_reserve_measured_width () =
  (* Sign columns come from the precomputed maxima; a wide before sign must
     shift the line numbers right. *)
  let renderer, _ln =
    make_line_number
      ~line_signs:
        [
          ( 0,
            {
              Line_number.before = Some "+>";
              after = None;
              before_color = None;
              after_color = None;
            } );
        ]
      ()
  in
  let grid = render renderer in
  equal ~msg:"before sign rendered" string "+"
    (Matrix_grid.get_text grid (Matrix_grid.idx grid ~x:0 ~y:0));
  equal ~msg:"line number shifted past the sign column" string "1"
    (Matrix_grid.get_text grid (Matrix_grid.idx grid ~x:3 ~y:0))

let gutter_text grid ~y =
  String.concat ""
    (List.init 3 (fun x ->
         Matrix_grid.get_text grid (Matrix_grid.idx grid ~x ~y)))

let gutter_numbers_follow_scrolled_rows () =
  (* The gutter reads the content's rows from its scroll origin, so the first
     visible row of a scrolled editor is numbered by its own logical line. *)
  let renderer = Renderer.create () in
  let ln = Line_number.create ~parent:(Renderer.root renderer) () in
  let state =
    Editor.of_string "a\nb\nc\nd\ne"
    |> Editor.with_viewport ~x:0 ~y:2 ~follow:false
  in
  let (_ : Textarea.t) =
    Textarea.create ~parent:(Line_number.node ln)
      ~style:
        (Toffee.Style.make
           ~size:
             (Toffee.Geometry.Size.make Toffee.Style.Dimension.auto
                (Toffee.Style.Dimension.length 3.))
           ())
      ~state
      ~on_change:(fun _ -> ())
      ()
  in
  let grid = render renderer in
  equal ~msg:"first visible row" string " 3 " (gutter_text grid ~y:0);
  equal ~msg:"last visible row" string " 5 " (gutter_text grid ~y:2)

let gutter_numbers_only_first_rows () =
  (* A wrapped continuation shows no number; the next logical line keeps its
     own. *)
  let renderer = Renderer.create () in
  let ln = Line_number.create ~parent:(Renderer.root renderer) () in
  let (_ : Text.t) =
    Text.create ~parent:(Line_number.node ln) ~wrap:`Char
      ~content:(String.make 30 'x' ^ "\ny")
      ()
  in
  let grid = render renderer in
  equal ~msg:"wrapped line numbered once" string " 1 " (gutter_text grid ~y:0);
  equal ~msg:"continuation unnumbered" string "   " (gutter_text grid ~y:1);
  equal ~msg:"next line" string " 2 " (gutter_text grid ~y:2)

(* ── Runner ── *)

(* ── Both doors ── *)

(* Each label, given to the element ({!Mosaic.line_number}) or to {!Line_number.create},
   reaches the live gutter ({!Test_harness.reaches}). *)

let doors_size = Mosaic.size ~width:12 ~height:3
let doors_style = Toffee.Style.make ~size:doors_size ()
let doors_red = Ansi.Color.red

let doors_sign =
  {
    Line_number.before = Some "+";
    after = None;
    before_color = None;
    after_color = None;
  }

let doors_gutter gutter =
  ignore
    (Text.create ~parent:(Line_number.node gutter) ~content:"a\nb\nc" ()
      : Text.t);
  Line_number.node gutter

let doors_fg () =
  Test_harness.reaches "fg"
    ~base:(Mosaic.line_number ~size:doors_size (Mosaic.text "a\nb\nc"))
    ~element:
      (Mosaic.line_number ~size:doors_size ~fg:doors_red (Mosaic.text "a\nb\nc"))
    ~create_base:(fun parent ->
      doors_gutter (Line_number.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_gutter
        (Line_number.create ~parent ~style:doors_style ~fg:doors_red ()))

let doors_bg () =
  Test_harness.reaches "bg"
    ~base:(Mosaic.line_number ~size:doors_size (Mosaic.text "a\nb\nc"))
    ~element:
      (Mosaic.line_number ~size:doors_size ~bg:doors_red (Mosaic.text "a\nb\nc"))
    ~create_base:(fun parent ->
      doors_gutter (Line_number.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_gutter
        (Line_number.create ~parent ~style:doors_style ~bg:doors_red ()))

let doors_min_width () =
  Test_harness.reaches "min_width"
    ~base:(Mosaic.line_number ~size:doors_size (Mosaic.text "a\nb\nc"))
    ~element:
      (Mosaic.line_number ~size:doors_size ~min_width:5 (Mosaic.text "a\nb\nc"))
    ~create_base:(fun parent ->
      doors_gutter (Line_number.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_gutter
        (Line_number.create ~parent ~style:doors_style ~min_width:5 ()))

let doors_padding_right () =
  Test_harness.reaches "padding_right"
    ~base:(Mosaic.line_number ~size:doors_size (Mosaic.text "a\nb\nc"))
    ~element:
      (Mosaic.line_number ~size:doors_size ~padding_right:3
         (Mosaic.text "a\nb\nc"))
    ~create_base:(fun parent ->
      doors_gutter (Line_number.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_gutter
        (Line_number.create ~parent ~style:doors_style ~padding_right:3 ()))

let doors_show_line_numbers () =
  Test_harness.reaches "show_line_numbers"
    ~base:(Mosaic.line_number ~size:doors_size (Mosaic.text "a\nb\nc"))
    ~element:
      (Mosaic.line_number ~size:doors_size ~show_line_numbers:false
         (Mosaic.text "a\nb\nc"))
    ~create_base:(fun parent ->
      doors_gutter (Line_number.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_gutter
        (Line_number.create ~parent ~style:doors_style ~show_line_numbers:false
           ()))

let doors_line_number_offset () =
  Test_harness.reaches "line_number_offset"
    ~base:(Mosaic.line_number ~size:doors_size (Mosaic.text "a\nb\nc"))
    ~element:
      (Mosaic.line_number ~size:doors_size ~line_number_offset:10
         (Mosaic.text "a\nb\nc"))
    ~create_base:(fun parent ->
      doors_gutter (Line_number.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_gutter
        (Line_number.create ~parent ~style:doors_style ~line_number_offset:10 ()))

let doors_line_colors () =
  Test_harness.reaches "line_colors"
    ~base:(Mosaic.line_number ~size:doors_size (Mosaic.text "a\nb\nc"))
    ~element:
      (Mosaic.line_number ~size:doors_size
         ~line_colors:
           [ (0, { Line_number.gutter = doors_red; content = None }) ]
         (Mosaic.text "a\nb\nc"))
    ~create_base:(fun parent ->
      doors_gutter (Line_number.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_gutter
        (Line_number.create ~parent ~style:doors_style
           ~line_colors:
             [ (0, { Line_number.gutter = doors_red; content = None }) ]
           ()))

let doors_line_signs () =
  Test_harness.reaches "line_signs"
    ~base:(Mosaic.line_number ~size:doors_size (Mosaic.text "a\nb\nc"))
    ~element:
      (Mosaic.line_number ~size:doors_size
         ~line_signs:[ (0, doors_sign) ]
         (Mosaic.text "a\nb\nc"))
    ~create_base:(fun parent ->
      doors_gutter (Line_number.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_gutter
        (Line_number.create ~parent ~style:doors_style
           ~line_signs:[ (0, doors_sign) ]
           ()))

let doors_line_numbers () =
  Test_harness.reaches "line_numbers"
    ~base:(Mosaic.line_number ~size:doors_size (Mosaic.text "a\nb\nc"))
    ~element:
      (Mosaic.line_number ~size:doors_size
         ~line_numbers:[ (0, 42) ]
         (Mosaic.text "a\nb\nc"))
    ~create_base:(fun parent ->
      doors_gutter (Line_number.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_gutter
        (Line_number.create ~parent ~style:doors_style
           ~line_numbers:[ (0, 42) ]
           ()))

let doors_hidden_line_numbers () =
  Test_harness.reaches "hidden_line_numbers"
    ~base:(Mosaic.line_number ~size:doors_size (Mosaic.text "a\nb\nc"))
    ~element:
      (Mosaic.line_number ~size:doors_size ~hidden_line_numbers:[ 0 ]
         (Mosaic.text "a\nb\nc"))
    ~create_base:(fun parent ->
      doors_gutter (Line_number.create ~parent ~style:doors_style ()))
    ~create:(fun parent ->
      doors_gutter
        (Line_number.create ~parent ~style:doors_style
           ~hidden_line_numbers:[ 0 ] ()))

let () =
  run "mosaic.line_number"
    [
      group "Props"
        [
          test "detects line color difference"
            props_equal_detects_line_color_diff;
          test "detects length mismatch" props_equal_detects_length_mismatch;
        ];
      group "Rendering"
        [
          test "line colors render in gutter" line_colors_render_in_gutter;
          test "apply_props refreshes line colors"
            apply_props_refreshes_line_colors;
          test "signs reserve measured width" signs_reserve_measured_width;
          test "numbers follow scrolled rows"
            gutter_numbers_follow_scrolled_rows;
          test "numbers only first rows" gutter_numbers_only_first_rows;
        ];
      group "Both doors"
        [
          test "fg" doors_fg;
          test "bg" doors_bg;
          test "min_width" doors_min_width;
          test "padding_right" doors_padding_right;
          test "show_line_numbers" doors_show_line_numbers;
          test "line_number_offset" doors_line_number_offset;
          test "line_colors" doors_line_colors;
          test "line_signs" doors_line_signs;
          test "line_numbers" doors_line_numbers;
          test "hidden_line_numbers" doors_hidden_line_numbers;
        ];
    ]
