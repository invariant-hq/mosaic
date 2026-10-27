open Windtrap
open Ansi

(* --- Helpers --- *)

(* Test helper that wraps callback-based feed to return a list *)
let feed_to_list p bytes off len =
  let acc = ref [] in
  ignore (Parser.feed p bytes ~off ~len (fun tok -> acc := tok :: !acc) : int);
  List.rev !acc

let check_color = Testable.make ~pp:Color.pp ~equal:Color.equal

(* Strict checking for escape sequences *)
let check_seq msg expected actual = equal ~msg string expected actual

(* --- 1. Color Logic & Math --- *)

let color_parsing_hex () =
  let c = Color.of_hex_exn in
  (* Standard 6 digit *)
  equal ~msg:"hex 6 digit" check_color (Color.of_rgb 255 0 128) (c "#ff0080");
  (* Short 3 digit *)
  equal ~msg:"hex 3 digit" check_color (Color.of_rgb 255 0 255) (c "#f0f");
  (* Alpha 8 digit *)
  equal ~msg:"hex 8 digit" check_color
    (Color.of_rgba 0 255 0 128)
    (c "#00ff0080");
  (* Case insensitivity *)
  equal ~msg:"hex case" check_color (Color.of_rgb 170 187 204) (c "#AABBCC");
  (* Failures *)
  is_true ~msg:"invalid hex len" (Color.of_hex "12" = None);
  is_true ~msg:"invalid hex char" (Color.of_hex "GG0000" = None)

let color_hsl_roundtrip () =
  (* Pure Red (RGB): 0 deg, 100%, 50% *)
  (* Use explicit RGB rather than Color.red since ANSI red varies by terminal *)
  let pure_red = Color.of_rgb 255 0 0 in
  let h, s, l, _ = Color.to_hsl pure_red in
  equal ~msg:"red hue" (float 0.01) 0.0 h;
  equal ~msg:"red saturation" (float 0.01) 1.0 s;
  equal ~msg:"red lightness" (float 0.01) 0.5 l;

  (* Roundtrip reconstruction *)
  let reconstructed = Color.of_hsl ~h ~s ~l () in
  equal ~msg:"red roundtrip" check_color pure_red reconstructed

let color_blending () =
  let bg = Color.of_rgb 0 0 0 in
  (* Black *)
  let fg = Color.of_rgba 255 255 255 128 in
  (* 50% White *)

  (* Source-over: half white over black is opaque gray. *)
  let res = Color.blend ~src:fg ~dst:bg in
  let r, g, b, a = Color.to_rgba res in
  equal ~msg:"blend result opaque" int 255 a;
  is_true ~msg:"blend result roughly gray" (r > 120 && r < 135 && r = g && g = b);
  let red = Color.of_rgba 255 0 0 128 in
  equal ~msg:"transparent destination preserves source channels" check_color red
    (Color.blend ~src:red ~dst:Color.transparent);
  equal ~msg:"translucent layers accumulate coverage" check_color
    (Color.of_rgba 255 0 0 192)
    (Color.blend ~src:red ~dst:red);
  equal ~msg:"rounding alpha does not overflow RGB channels" check_color
    (Color.of_rgba 255 255 255 128)
    (Color.blend
       ~src:(Color.of_rgba 255 255 255 128)
       ~dst:(Color.of_rgba 255 255 255 1));
  List.iter
    (fun color ->
      equal ~msg:"transparent source preserves intent" check_color color
        (Color.blend ~src:Color.transparent ~dst:color);
      equal ~msg:"opaque source preserves intent" check_color color
        (Color.blend ~src:color ~dst:red))
    [ Color.default_fg; Color.default_bg; Color.indexed 1 ]

let color_intent_is_part_of_identity () =
  (* Exact equality must preserve terminal emission intent, while visual
     equality can still compare the RGBA snapshot. *)
  let indexed_red = Color.indexed 1 in
  let rgb_red = Color.of_rgb 128 0 0 in
  (match Color.intent indexed_red with
  | Color.Indexed 1 -> ()
  | _ -> fail "Expected indexed intent");
  (match Color.intent rgb_red with
  | Color.Rgb -> ()
  | _ -> fail "Expected RGB intent");
  is_false ~msg:"intent differs" (Color.equal indexed_red rgb_red);
  is_true ~msg:"rgba snapshot matches" (Color.equal_rgba indexed_red rgb_red);
  is_false ~msg:"default intent differs from transparent RGB"
    (Color.equal Color.default_bg (Color.of_rgba 0 0 0 0))

let color_ansi16_palette_matches_standard_fallback () =
  (* The fallback ANSI16 table uses 0x80/0xc0 base colors. *)
  let expected =
    [|
      (0x00, 0x00, 0x00);
      (0x80, 0x00, 0x00);
      (0x00, 0x80, 0x00);
      (0x80, 0x80, 0x00);
      (0x00, 0x00, 0x80);
      (0x80, 0x00, 0x80);
      (0x00, 0x80, 0x80);
      (0xc0, 0xc0, 0xc0);
      (0x80, 0x80, 0x80);
      (0xff, 0x00, 0x00);
      (0x00, 0xff, 0x00);
      (0xff, 0xff, 0x00);
      (0x00, 0x00, 0xff);
      (0xff, 0x00, 0xff);
      (0x00, 0xff, 0xff);
      (0xff, 0xff, 0xff);
    |]
  in
  Array.iteri
    (fun i expected_rgb ->
      let er, eg, eb = expected_rgb in
      let ar, ag, ab = Color.to_rgb (Color.indexed i) in
      equal ~msg:(Printf.sprintf "ansi16 slot %d r" i) int er ar;
      equal ~msg:(Printf.sprintf "ansi16 slot %d g" i) int eg ag;
      equal ~msg:(Printf.sprintf "ansi16 slot %d b" i) int eb ab)
    expected

let color_extended_out_of_range_is_safe () =
  (* Regression from the matrix.ansi review: out-of-range palette inputs must
     not reach unsafe palette indexing. *)
  is_true ~msg:"negative extended compares safely"
    (Color.equal (Color.indexed (-1)) Color.black);
  is_true ~msg:"large extended compares safely"
    (Color.equal (Color.indexed 999) (Color.indexed 255));
  ignore (Color.hash (Color.indexed 999) : int);
  ignore (Color.compare (Color.indexed (-1)) (Color.indexed 999) : int)

let low_indexed_colors_use_extended_palette_sgr () =
  equal ~msg:"foreground slot 1 keeps indexed SGR" (list int) [ 38; 5; 1 ]
    (Color.to_sgr_codes ~bg:false Color.red);
  equal ~msg:"background slot 4 keeps indexed SGR" (list int) [ 48; 5; 4 ]
    (Color.to_sgr_codes ~bg:true Color.blue)

(* WCAG 2 contrast ratios, against the measured terminal palettes: xterm's
   red (#cd0000), blue (#0000ee), yellow (#cdcd00) and bright black
   (#7f7f7f) on its white background, VTE's red (#c00000) on black, and
   Solarized light's red and foreground on its background. *)
let color_contrast () =
  let c = Color.of_hex_exn in
  let ratio = float 0.005 in
  let white = c "#ffffff" and black = c "#000000" in
  equal ~msg:"black on white" ratio 21.0 (Color.contrast black white);
  equal ~msg:"white on black" ratio 21.0 (Color.contrast white black);
  equal ~msg:"a color on itself" ratio 1.0 (Color.contrast white white);
  equal ~msg:"xterm red" ratio 5.84 (Color.contrast (c "#cd0000") white);
  equal ~msg:"xterm blue" ratio 9.40 (Color.contrast (c "#0000ee") white);
  equal ~msg:"xterm yellow" ratio 1.70 (Color.contrast (c "#cdcd00") white);
  equal ~msg:"xterm bright black" ratio 4.00
    (Color.contrast (c "#7f7f7f") white);
  equal ~msg:"VTE red on black" ratio 3.24 (Color.contrast (c "#c00000") black);
  equal ~msg:"Solarized light red" ratio 4.29
    (Color.contrast (c "#dc322f") (c "#fdf6e3"));
  equal ~msg:"Solarized light pair" ratio 4.13
    (Color.contrast (c "#657b83") (c "#fdf6e3"));
  equal ~msg:"alpha is ignored" ratio 21.0
    (Color.contrast (Color.of_rgba 0 0 0 0) white);
  equal ~msg:"a palette entry by its snapshot" ratio
    (Color.contrast (c "#ff0000") black)
    (Color.contrast Color.bright_red black)

(* xterm's palette as it reports it, entries 0 to 15, and its two defaults:
   black on white and, as XTerm-color, light gray on black. *)
let xterm_entries =
  Array.map
    (fun h -> Some (Color.to_rgb (Color.of_hex_exn h)))
    [|
      "000000";
      "cd0000";
      "00cd00";
      "cdcd00";
      "0000ee";
      "cd00cd";
      "00cdcd";
      "e5e5e5";
      "7f7f7f";
      "ff0000";
      "00ff00";
      "ffff00";
      "5c5cff";
      "ff00ff";
      "00ffff";
      "ffffff";
    |]

let xterm_light =
  {
    Palette.foreground = (0, 0, 0);
    background = (255, 255, 255);
    cursor = None;
    entries = xterm_entries;
  }

let xterm_dark =
  { xterm_light with foreground = (0xe5, 0xe5, 0xe5); background = (0, 0, 0) }

let palette_contrast () =
  let shown = option (float 0.005) in
  let p = { xterm_light with entries = [| None; Some (0xcd, 0, 0) |] } in
  let on_default fg = Palette.contrast p ~fg ~bg:Color.default_bg in
  equal ~msg:"the pair" shown (Some 21.0)
    (Palette.contrast p ~fg:Color.default_fg ~bg:Color.default_bg);
  equal ~msg:"a default background is the palette's" shown (Some 21.0)
    (on_default Color.default_bg);
  equal ~msg:"a transparent color is the default" shown (Some 21.0)
    (on_default Color.transparent);
  equal ~msg:"an entry as known" shown (Some 5.84)
    (on_default (Color.indexed 1));
  equal ~msg:"an entry unknown" shown None (on_default (Color.indexed 0));
  equal ~msg:"an entry past the array" shown None (on_default (Color.indexed 2));
  equal ~msg:"a background unknown" shown None
    (Palette.contrast p ~fg:Color.default_fg ~bg:(Color.indexed 0));
  equal ~msg:"entry 100 of 16 entries, unknown" shown None
    (Palette.contrast xterm_light ~fg:(Color.indexed 100) ~bg:Color.default_bg);
  equal ~msg:"entry 196 of the default palette" shown (Some 5.25)
    (Palette.contrast Palette.default ~fg:(Color.indexed 196)
       ~bg:Color.default_bg);
  equal ~msg:"an RGB color" shown (Some 4.48)
    (on_default (Color.of_rgb 0x77 0x77 0x77))

let palette_legible () =
  let entry = option check_color in
  let i = Color.indexed in
  equal ~msg:"blue on white" entry (Some (i 4)) (Palette.legible xterm_light 4);
  equal ~msg:"green on white, neither" entry None
    (Palette.legible xterm_light 2);
  equal ~msg:"red on black, the bright one" entry
    (Some (i 9))
    (Palette.legible xterm_dark 1);
  equal ~msg:"a target of 3" entry
    (Some (i 1))
    (Palette.legible ~target:3. xterm_light 1);
  equal ~msg:"yellow at 3, neither" entry None
    (Palette.legible ~target:3. xterm_light 3);
  equal ~msg:"entry 8 on white, below 4.5" entry None
    (Palette.legible xterm_light 8);
  equal ~msg:"entry 8 on black" entry
    (Some (i 8))
    (Palette.legible xterm_dark 8);
  equal ~msg:"an entry unknown" entry None
    (Palette.legible { xterm_light with entries = [||] } 4);
  equal ~msg:"entry 21, blue, on white" entry
    (Some (i 21))
    (Palette.legible { Palette.default with background = (255, 255, 255) } 21);
  equal ~msg:"entry 21 of 16 entries, unknown" entry None
    (Palette.legible xterm_light 21);
  (* A pair of 4.0:1, as xterm's bright black on white, lowers the target
     of text to its own. *)
  let low = { xterm_light with foreground = (0x7f, 0x7f, 0x7f) } in
  equal ~msg:"down to the pair" entry (Some (i 8)) (Palette.legible low 8);
  equal ~msg:"below the pair" entry None (Palette.legible low 3)

let palette_entry () =
  let rgb = option (triple int int int) in
  equal ~msg:"a known entry" rgb
    (Some (0xcd, 0, 0))
    (Palette.entry xterm_light 1);
  equal ~msg:"past the array" rgb None (Palette.entry xterm_light 16);
  equal ~msg:"below 0" rgb None (Palette.entry Palette.default (-1));
  equal ~msg:"above 255" rgb None (Palette.entry Palette.default 256);
  equal ~msg:"past 255 in a longer array" rgb None
    (Palette.entry
       { xterm_light with entries = Array.make 300 (Some (1, 2, 3)) }
       256);
  equal ~msg:"the default's entries are Color's" rgb
    (Some (Color.to_rgb (Color.indexed 100)))
    (Palette.entry Palette.default 100);
  equal ~msg:"the default's pair"
    (pair (triple int int int) (triple int int int))
    (Color.to_rgb (Color.indexed 7), Color.to_rgb (Color.indexed 0))
    (Palette.default.foreground, Palette.default.background);
  equal ~msg:"the default's cursor" rgb
    (Some (Color.to_rgb (Color.indexed 7)))
    Palette.default.cursor

let palette_equal () =
  let trailing =
    { xterm_light with entries = Array.append xterm_entries [| None; None |] }
  in
  is_true ~msg:"trailing unknowns" (Palette.equal xterm_light trailing);
  is_false ~msg:"an entry more"
    (Palette.equal xterm_light
       {
         xterm_light with
         entries = Array.append xterm_entries [| Some (0, 0, 0) |];
       });
  is_false ~msg:"an entry unknown"
    (Palette.equal xterm_light
       { xterm_light with entries = Array.sub xterm_entries 0 15 });
  is_false ~msg:"the cursor"
    (Palette.equal xterm_light { xterm_light with cursor = Some (0, 0, 0) });
  is_false ~msg:"the background" (Palette.equal xterm_light xterm_dark);
  is_true ~msg:"past 255"
    (Palette.equal Palette.default
       {
         Palette.default with
         entries = Array.append Palette.default.entries [| Some (1, 2, 3) |];
       })

let palette_pp () =
  let shown p = Format.asprintf "%a" Palette.pp p in
  equal ~msg:"up to the last entry known" string
    "{fg #000000; bg #ffffff; cursor -; entries [- #cd0000]}"
    (shown { xterm_light with entries = [| None; Some (0xcd, 0, 0); None |] });
  equal ~msg:"equal palettes print alike" string (shown xterm_light)
    (shown { xterm_light with entries = Array.append xterm_entries [| None |] })

let gen_rgb =
  Gen.triple (Gen.int_range 0 255) (Gen.int_range 0 255) (Gen.int_range 0 255)

let gen_palette =
  Gen.map
    (fun (foreground, background, entries) ->
      { Palette.foreground; background; cursor = None; entries })
    (Gen.triple gen_rgb gen_rgb
       (Gen.array ~size:(Gen.int_range 0 16) (Gen.option gen_rgb)))
  |> Gen.with_pp Palette.pp

(* What [legible] gives on any palette: entry [i] where it is known and
   reaches the target, else [i + 8] for [i] below 8 where it does, else
   nothing. *)
let palette_legible_laws (p, i) =
  let pair = Palette.contrast p ~fg:Color.default_fg ~bg:Color.default_bg in
  let target = Float.min 4.5 (Option.get pair) in
  let reaches i =
    match Palette.contrast p ~fg:(Color.indexed i) ~bg:Color.default_bg with
    | Some k -> k >= target
    | None -> false
  in
  let expected =
    if reaches i then Some (Color.indexed i)
    else if i < 8 && reaches (i + 8) then Some (Color.indexed (i + 8))
    else None
  in
  equal ~msg:"legible" (option check_color) expected (Palette.legible p i)

(* --- 2. Attributes & Style --- *)

let attr_bitmask_integrity () =
  (* Verify bitmasks don't overlap using a set operation *)
  let all_set =
    Attr.combine ~bold:true ~dim:true ~italic:true ~underline:true ~blink:true
      ~inverse:true ~hidden:true ~strikethrough:true ~double_underline:true
      ~overline:true ~framed:true ~encircled:true ()
  in
  let count = Attr.cardinal all_set in
  equal ~msg:"all flags distinct" int 12 count;

  (* Check that removing one specific flag works *)
  let minus_bold = Attr.remove Attr.Bold all_set in
  is_false ~msg:"removed bold" (Attr.mem Attr.Bold minus_bold);
  is_true ~msg:"kept italic" (Attr.mem Attr.Italic minus_bold)

let style_composition_and_hash () =
  let s1 = Style.make ~fg:Color.red () in
  let s2 = Style.make ~bg:Color.blue () in
  let s3 = Style.merge ~base:s1 ~overlay:s2 in

  equal ~msg:"composed fg" check_color Color.red (Option.get s3.fg);
  equal ~msg:"composed bg" check_color Color.blue (Option.get s3.bg);

  (* Hash consistency *)
  let s3_clone = Style.make ~fg:Color.red ~bg:Color.blue () in
  equal ~msg:"hash stable" int (Style.hash s3) (Style.hash s3_clone)

let attr_extended_ordering () =
  let style =
    Style.make ~double_underline:true ~overline:true ~framed:true
      ~encircled:true ()
  in
  (* Minimal SGR: no reset needed when going from default, just enable attrs *)
  check_seq "extended attr ordering" "\x1b[21;53;51;52m"
    (Style.sgr_sequence style);
  equal ~msg:"no-op when unchanged" string ""
    (Style.sgr_sequence ~prev:style style)

(* The transition ignores links, in to_sgr_codes and sgr_sequence as in
   emit, so a style with a link and default colors goes to CSI 0 m. *)
let style_transition_ignores_links () =
  let bold = Style.make ~bold:true () in
  let linked = Style.make ~link:"http://a" () in
  equal ~msg:"codes" (list int) [ 0 ] (Style.to_sgr_codes ~prev:bold linked);
  let emitted = Ansi.to_string (Style.emit ~prev:bold linked) in
  equal ~msg:"sgr_sequence is emit's" string emitted
    (Style.sgr_sequence ~prev:bold linked);
  equal ~msg:"links alone change nothing" (list int) []
    (Style.to_sgr_codes ~prev:linked (Style.make ~link:"http://b" ()))

(* A style is written at the terminal's color depth, as a tracker writes it:
   reduced by Color.to_depth, with the 16-color codes at 16 and 8 colors. *)
let style_writes_at_a_depth () =
  let check msg depth style expected =
    equal ~msg string expected (Style.sgr_sequence ~depth style);
    equal ~msg:(msg ^ ", emit") string expected
      (Ansi.to_string (Style.emit ~depth style))
  in
  check "palette red at 16 colors" `Ansi16
    (Style.make ~fg:(Color.indexed 1) ())
    "\027[31m";
  check "bright red background at 16 colors" `Ansi16
    (Style.make ~bg:(Color.indexed 9) ())
    "\027[101m";
  check "RGB red at 256 colors" `Ansi256
    (Style.make ~fg:(Color.of_rgb 255 0 0) ())
    "\027[38;5;196m";
  check "RGB red at true color" `Truecolor
    (Style.make ~fg:(Color.of_rgb 255 0 0) ())
    "\027[38;2;255;0;0m";
  equal ~msg:"Color.to_sgr_codes at 16 colors" (list int) [ 91 ]
    (Color.to_sgr_codes ~depth:`Ansi16 ~bg:false (Color.indexed 9))

let style_shared_disable_codes () =
  (* Bold and Dim share disable code 22. When transitioning from bold+dim to
     dim-only, we must re-enable dim after disabling bold. *)
  let bold_dim = Style.make ~bold:true ~dim:true () in
  let dim_only = Style.make ~dim:true () in
  let codes = Style.to_sgr_codes ~prev:bold_dim dim_only in
  (* Should contain 22 (disable bold/dim) and 2 (re-enable dim) *)
  is_true ~msg:"contains disable code 22" (List.mem 22 codes);
  is_true ~msg:"contains re-enable dim code 2" (List.mem 2 codes);

  (* Underline and Double_underline share disable code 24 *)
  let underline_double = Style.make ~underline:true ~double_underline:true () in
  let underline_only = Style.make ~underline:true () in
  let codes2 = Style.to_sgr_codes ~prev:underline_double underline_only in
  is_true ~msg:"contains disable code 24" (List.mem 24 codes2);
  is_true ~msg:"contains re-enable underline code 4" (List.mem 4 codes2)

let style_emit_function () =
  (* Test that emit produces the same output as sgr_sequence *)
  let style = Style.make ~fg:Color.red ~bold:true () in
  let w = Writer.create 64 in
  Style.emit style w;
  let emitted = Writer.contents w in
  let seq = Style.sgr_sequence style in
  check_seq "emit matches sgr_sequence" seq emitted;

  (* Test emit with prev style *)
  let w2 = Writer.create 64 in
  let style2 = Style.make ~fg:Color.red ~italic:true () in
  Style.emit ~prev:style style2 w2;
  let emitted2 = Writer.contents w2 in
  (* Should only disable bold (22) and enable italic (3) - not emit fg again *)
  let codes2 = Style.to_sgr_codes ~prev:style style2 in
  satisfies ~claim:"non-empty emit delta" string
    (fun s -> String.length s > 0)
    emitted2;
  mem ~msg:"delta contains disable bold (22)" int 22 codes2;
  mem ~msg:"delta contains enable italic (3)" int 3 codes2;
  is_false ~msg:"delta does not re-emit fg (31)" (List.mem 31 codes2)

let style_resolve_multiple () =
  let s1 = Style.make ~fg:Color.red () in
  let s2 = Style.make ~bg:Color.blue () in
  let s3 = Style.make ~bold:true () in
  let resolved = Style.resolve [ s1; s2; s3 ] in
  equal ~msg:"resolve fg" check_color Color.red (Option.get resolved.fg);
  equal ~msg:"resolve bg" check_color Color.blue (Option.get resolved.bg);
  is_true ~msg:"resolve has bold" (Attr.mem Attr.Bold resolved.attrs);

  (* Later styles override colors *)
  let s4 = Style.make ~fg:Color.green () in
  let resolved2 = Style.resolve [ s1; s4 ] in
  equal ~msg:"resolve override fg" check_color Color.green
    (Option.get resolved2.fg)

let style_compare_and_hash () =
  let s1 = Style.make ~fg:Color.red ~bold:true () in
  let s2 = Style.make ~fg:Color.red ~bold:true () in
  let s3 = Style.make ~fg:Color.blue ~bold:true () in

  (* Equal styles should compare equal *)
  equal ~msg:"equal styles compare 0" int 0 (Style.compare s1 s2);
  (* Equal styles should have same hash *)
  equal ~msg:"equal styles same hash" int (Style.hash s1) (Style.hash s2);

  (* Different styles should not compare equal *)
  is_true ~msg:"different styles compare non-zero" (Style.compare s1 s3 <> 0);

  (* Ordering should be consistent *)
  let cmp12 = Style.compare s1 s3 in
  let cmp21 = Style.compare s3 s1 in
  is_true ~msg:"compare antisymmetric"
    (cmp12 = -cmp21 || (cmp12 = 0 && cmp21 = 0))

(* --- 3. SGR State Machine (Diffing) --- *)

let sgr_state_transitions () =
  let w = Writer.create 128 in
  let state = Sgr_state.create () in

  (* 1. Initial: emits reset(0) + attributes *)
  Sgr_state.update state w ~fg:(Color.of_rgb 255 0 0) ~bg:Color.default_bg
    ~attrs:(Attr.pack Attr.bold) ~link:"";

  let out1 = Writer.contents w in
  (* Expect: Reset(0); FG(Red); Bold(1) *)
  (* Sequence: \x1b[0;38;2;255;0;0;1m *)
  check_seq "initial transition" "\x1b[0;38;2;255;0;0;1m" out1;

  (* 2. No-op: update with exact same values *)
  let w2 = Writer.create 128 in
  Sgr_state.update state w2 ~fg:(Color.of_rgb 255 0 0) ~bg:Color.default_bg
    ~attrs:(Attr.pack Attr.bold) ~link:"";
  equal ~msg:"noop update empty" int 0 (Writer.len w2);

  (* 3. Delta: Remove Bold, keep color *)
  let w3 = Writer.create 128 in
  Sgr_state.update state w3 ~fg:(Color.of_rgb 255 0 0) ~bg:Color.default_bg
    ~attrs:0 ~link:"";

  (* Only the attribute changes; the foreground is already active. *)
  let out3 = Writer.contents w3 in
  check_seq "delta transition" "\x1b[22m" out3

let sgr_foreground_only_change_does_not_reset () =
  let state = Sgr_state.create () in
  let w1 = Writer.create 128 in
  Sgr_state.update state w1 ~fg:(Color.of_rgb 255 0 0)
    ~bg:(Color.of_rgb 0 0 255) ~attrs:(Attr.pack Attr.bold) ~link:"";
  let w2 = Writer.create 128 in
  Sgr_state.update state w2 ~fg:(Color.of_rgb 0 255 0)
    ~bg:(Color.of_rgb 0 0 255) ~attrs:(Attr.pack Attr.bold) ~link:"";
  let out = Writer.contents w2 in
  check_seq "foreground-only delta" "\x1b[38;2;0;255;0m" out;
  is_false ~msg:"foreground-only delta does not reset"
    (String.starts_with ~prefix:"\x1b[0" out)

let sgr_background_only_change_does_not_reset () =
  let state = Sgr_state.create () in
  let w1 = Writer.create 128 in
  Sgr_state.update state w1 ~fg:(Color.of_rgb 255 0 0)
    ~bg:(Color.of_rgb 0 0 255) ~attrs:(Attr.pack Attr.bold) ~link:"";
  let w2 = Writer.create 128 in
  Sgr_state.update state w2 ~fg:(Color.of_rgb 255 0 0)
    ~bg:(Color.of_rgb 0 255 0) ~attrs:(Attr.pack Attr.bold) ~link:"";
  check_seq "background-only delta" "\x1b[48;2;0;255;0m" (Writer.contents w2)

let sgr_shared_attr_disable_reenables_survivor () =
  let state = Sgr_state.create () in
  let w1 = Writer.create 128 in
  let bold_dim = Attr.union Attr.bold Attr.dim in
  Sgr_state.update state w1 ~fg:Color.default_fg ~bg:Color.default_bg
    ~attrs:(Attr.pack bold_dim) ~link:"";
  let w2 = Writer.create 128 in
  Sgr_state.update state w2 ~fg:Color.default_fg ~bg:Color.default_bg
    ~attrs:(Attr.pack Attr.dim) ~link:"";
  check_seq "bold disabled and dim restored" "\x1b[22;2m" (Writer.contents w2)

let sgr_shared_attribute_transitions () =
  let paired =
    [|
      Attr.Bold;
      Attr.Dim;
      Attr.Underline;
      Attr.Double_underline;
      Attr.Framed;
      Attr.Encircled;
    |]
  in
  let attributes bits =
    let attrs = ref (Attr.union Attr.italic Attr.overline) in
    Array.iteri
      (fun i flag ->
        if bits land (1 lsl i) <> 0 then attrs := Attr.add flag !attrs)
      paired;
    !attrs
  in
  let apply attrs = function
    | 0 -> Attr.empty
    | 22 -> Attr.remove Attr.Bold (Attr.remove Attr.Dim attrs)
    | 24 -> Attr.remove Attr.Underline (Attr.remove Attr.Double_underline attrs)
    | 54 -> Attr.remove Attr.Framed (Attr.remove Attr.Encircled attrs)
    | code -> (
        match
          List.find_opt
            (fun flag -> Attr.flag_to_sgr_code flag = code)
            (Attr.to_list (Attr.unpack (-1)))
        with
        | Some flag -> Attr.add flag attrs
        | None -> failf "unexpected attribute SGR %d" code)
  in
  let codes output =
    if output = "" then []
    else
      String.sub output 2 (String.length output - 3)
      |> String.split_on_char ';' |> List.map int_of_string
  in
  let state = Sgr_state.create () in
  let writer = Writer.create 128 in
  for before = 0 to 63 do
    for after = 0 to 63 do
      let prev_attrs = attributes before and attrs = attributes after in
      let prev = Style.with_attrs prev_attrs Style.default in
      let next = Style.with_attrs attrs Style.default in
      let style_codes = Style.to_sgr_codes ~prev next in
      let msg = Printf.sprintf "paired attributes %02x -> %02x" before after in
      equal ~msg int (Attr.pack attrs)
        (Attr.pack (List.fold_left apply prev_attrs style_codes));
      equal ~msg string
        (Ansi.to_string (Ansi.sgr style_codes))
        (Ansi.to_string (Style.emit ~prev next));
      Sgr_state.reset state;
      Writer.reset_pos writer;
      Sgr_state.update state writer ~fg:Color.default_fg ~bg:Color.default_bg
        ~attrs:(Attr.pack prev_attrs) ~link:"";
      Writer.reset_pos writer;
      Sgr_state.update state writer ~fg:Color.default_fg ~bg:Color.default_bg
        ~attrs:(Attr.pack attrs) ~link:"";
      equal ~msg int (Attr.pack attrs)
        (Attr.pack
           (List.fold_left apply prev_attrs (codes (Writer.contents writer))))
    done
  done

let sgr_enclosure_disable_group () =
  (* SGR 54 clears both Framed and Encircled; the diff must treat the pair as
     one group when disabling and re-enabling survivors. *)
  (* Encircled -> none must emit the shared disable code, not an empty CSI m
     (which terminals treat as a full SGR reset). *)
  let state = Sgr_state.create () in
  let w1 = Writer.create 128 in
  Sgr_state.update state w1 ~fg:Color.default_fg ~bg:Color.default_bg
    ~attrs:(Attr.pack Attr.encircled) ~link:"";
  let w2 = Writer.create 128 in
  Sgr_state.update state w2 ~fg:Color.default_fg ~bg:Color.default_bg ~attrs:0
    ~link:"";
  check_seq "encircled disabled via 54" "\x1b[54m" (Writer.contents w2);
  (* Overline+Encircled -> Encircled: dropping overline must not emit 54. *)
  let state = Sgr_state.create () in
  let w1 = Writer.create 128 in
  Sgr_state.update state w1 ~fg:Color.default_fg ~bg:Color.default_bg
    ~attrs:(Attr.pack (Attr.union Attr.overline Attr.encircled))
    ~link:"";
  let w2 = Writer.create 128 in
  Sgr_state.update state w2 ~fg:Color.default_fg ~bg:Color.default_bg
    ~attrs:(Attr.pack Attr.encircled) ~link:"";
  check_seq "overline drop keeps encircled" "\x1b[55m" (Writer.contents w2);
  (* Framed+Encircled -> Framed: disable via 54, then re-enable framed. *)
  let state = Sgr_state.create () in
  let w1 = Writer.create 128 in
  Sgr_state.update state w1 ~fg:Color.default_fg ~bg:Color.default_bg
    ~attrs:(Attr.pack (Attr.union Attr.framed Attr.encircled))
    ~link:"";
  let w2 = Writer.create 128 in
  Sgr_state.update state w2 ~fg:Color.default_fg ~bg:Color.default_bg
    ~attrs:(Attr.pack Attr.framed) ~link:"";
  check_seq "encircled disabled and framed restored" "\x1b[54;51m"
    (Writer.contents w2)

let sgr_same_rgba_different_intent_emits_color_delta () =
  let state = Sgr_state.create () in
  let w1 = Writer.create 128 in
  Sgr_state.update state w1 ~fg:(Color.indexed 1) ~bg:Color.default_bg ~attrs:0
    ~link:"";
  let w2 = Writer.create 128 in
  Sgr_state.update state w2 ~fg:(Color.of_rgb 128 0 0) ~bg:Color.default_bg
    ~attrs:0 ~link:"";
  check_seq "intent-only color delta" "\x1b[38;2;128;0;0m" (Writer.contents w2)

let sgr_transparent_bg_resets () =
  let state = Sgr_state.create () in
  let w1 = Writer.create 128 in
  Sgr_state.update state w1 ~fg:(Color.of_rgb 255 0 0)
    ~bg:(Color.of_rgb 0 0 255) ~attrs:0 ~link:"";
  check_seq "bg applied" "\x1b[0;38;2;255;0;0;48;2;0;0;255m"
    (Writer.contents w1);
  let w2 = Writer.create 128 in
  Sgr_state.update state w2 ~fg:(Color.of_rgb 255 0 0) ~bg:Color.default_bg
    ~attrs:0 ~link:"";
  check_seq "bg cleared to default" "\x1b[49m" (Writer.contents w2)

let sgr_transparent_fg_resets_to_default () =
  let state = Sgr_state.create () in
  let w1 = Writer.create 128 in
  Sgr_state.update state w1 ~fg:(Color.of_rgb 255 0 0) ~bg:Color.default_bg
    ~attrs:0 ~link:"";
  check_seq "fg applied" "\x1b[0;38;2;255;0;0m" (Writer.contents w1);
  let w2 = Writer.create 128 in
  Sgr_state.update state w2 ~fg:Color.default_fg ~bg:Color.default_bg ~attrs:0
    ~link:"";
  let out = Writer.contents w2 in
  check_seq "fg cleared to default" "\x1b[39m" out;
  is_false ~msg:"default fg is not emitted as truecolor black"
    (String.contains out '3' && String.contains out '8')

let sgr_rgb_downgrades_to_ansi256 () =
  let state = Sgr_state.create () in
  Sgr_state.set_color_depth state `Ansi256;
  let w = Writer.create 128 in
  Sgr_state.update state w ~fg:(Color.of_rgb 255 0 0) ~bg:(Color.of_rgb 0 0 255)
    ~attrs:0 ~link:"";
  check_seq "rgb downgraded to ansi256" "\x1b[0;38;5;196;48;5;21m"
    (Writer.contents w)

let sgr_rgb_downgrades_to_ansi16 () =
  let state = Sgr_state.create () in
  Sgr_state.set_color_depth state `Ansi16;
  let w = Writer.create 128 in
  Sgr_state.update state w ~fg:(Color.of_rgb 255 0 0) ~bg:(Color.of_rgb 0 0 255)
    ~attrs:0 ~link:"";
  check_seq "rgb downgraded to ansi16" "\x1b[0;91;104m" (Writer.contents w)

let sgr_rgb_to_ansi8_and_mono () =
  let emit depth =
    let state = Sgr_state.create () in
    Sgr_state.set_color_depth state depth;
    let w = Writer.create 128 in
    Sgr_state.update state w ~fg:Color.bright_red ~bg:(Color.of_rgb 0 0 200)
      ~attrs:0 ~link:"";
    Writer.contents w
  in
  check_seq "ansi8" "\x1b[0;31;44m" (emit `Ansi8);
  check_seq "mono" "\x1b[0m" (emit `Mono)

(* xterm's palette from 256colres.pl, and xterm's search (xtermClosestColor):
   from the top entry down, the first of least squared distance. *)
let xterm_rgb i =
  if i < 232 then
    let level v = if v = 0 then 0 else (v * 40) + 55 in
    let n = i - 16 in
    (level (n / 36), level (n / 6 mod 6), level (n mod 6))
  else
    let v = ((i - 232) * 10) + 8 in
    (v, v, v)

let xterm_closest ~first r g b =
  let best = ref (-1) and best_d = ref max_int in
  for i = 255 downto first do
    let pr, pg, pb = xterm_rgb i in
    let d =
      ((r - pr) * (r - pr)) + ((g - pg) * (g - pg)) + ((b - pb) * (b - pb))
    in
    if d < !best_d then (
      best := i;
      best_d := d)
  done;
  !best

let color_to_depth_256_matches_xterm_search () =
  let check r g b =
    let expected = xterm_closest ~first:16 r g b in
    match Color.intent (Color.to_depth `Ansi256 (Color.of_rgb r g b)) with
    | Color.Indexed i when i = expected -> ()
    | Color.Indexed i -> failf "rgb(%d,%d,%d): %d, xterm %d" r g b i expected
    | _ -> failf "rgb(%d,%d,%d): not indexed" r g b
  in
  (* Each channel at the cube's midpoints and around them, and in steps. *)
  let values =
    List.sort_uniq Int.compare
      (List.concat_map
         (fun v -> [ v - 1; v; v + 1 ])
         [ 0; 47; 48; 114; 115; 154; 155; 194; 195; 234; 235; 255 ]
      @ List.init 16 (fun k -> 17 * k))
    |> List.filter (fun v -> v >= 0 && v <= 255)
  in
  List.iter
    (fun r ->
      List.iter (fun g -> List.iter (fun b -> check r g b) values) values)
    values;
  (* Near the gray axis, where the ramp and the cube's grays compete. *)
  for v = 0 to 255 do
    check v v v;
    if v < 255 then (
      check v v (v + 1);
      check v (v + 1) (v + 1))
  done;
  for i = 16 to 255 do
    let r, g, b = xterm_rgb i in
    equal
      ~msg:(Printf.sprintf "entry %d is xterm's" i)
      (triple int int int) (r, g, b)
      (Color.to_rgb (Color.indexed i))
  done

let color_to_depth () =
  let check msg depth c expected =
    equal ~msg check_color expected (Color.to_depth depth c)
  in
  let red = Color.of_rgb 255 0 0 in
  check "truecolor keeps rgb" `Truecolor red red;
  check "an exact cube entry" `Ansi256 red (Color.indexed 196);
  check "a gray goes to the ramp" `Ansi256 (Color.of_rgb 128 128 128)
    (Color.indexed 244);
  check "black stays in the cube" `Ansi256 (Color.of_rgb 0 0 0)
    (Color.indexed 16);
  check "never one of the 16 at ansi256" `Ansi256 (Color.of_rgb 128 0 0)
    (Color.indexed 88);
  check "ansi16" `Ansi16 (Color.of_rgb 128 0 0) Color.red;
  check "an indexed color above 15" `Ansi16 (Color.indexed 196) Color.bright_red;
  check "indexed within the depth" `Ansi16 Color.bright_blue Color.bright_blue;
  check "indexed within ansi256" `Ansi256 Color.bright_blue Color.bright_blue;
  check "ansi8 takes the nearest of 8" `Ansi8 Color.bright_black Color.white;
  check "ansi8 bright red" `Ansi8 Color.bright_red Color.red;
  check "default at every depth" `Ansi8 Color.default_fg Color.default_fg;
  check "transparent is unchanged" `Ansi16 Color.transparent Color.transparent;
  let translucent = Color.of_rgba 255 0 0 128 in
  equal ~msg:"alpha is kept" int 128
    (let _, _, _, a = Color.to_rgba (Color.to_depth `Ansi256 translucent) in
     a);
  let mono = Color.to_depth `Mono red in
  is_true ~msg:"mono is the default" (Color.intent mono = Color.Default);
  equal ~msg:"mono keeps the snapshot" (triple int int int) (255, 0, 0)
    (Color.to_rgb mono)

let sgr_indexed_stays_indexed_without_truecolor () =
  let state = Sgr_state.create () in
  Sgr_state.set_color_depth state `Ansi256;
  let w = Writer.create 128 in
  Sgr_state.update state w ~fg:(Color.indexed 42) ~bg:(Color.indexed 99)
    ~attrs:0 ~link:"";
  check_seq "indexed preserved" "\x1b[0;38;5;42;48;5;99m" (Writer.contents w)

(* --- 4. Parser --- *)

(* Tokens in ECMA-48's notation, adjacent Text and Data joined, so that the
   expectation does not depend on how a run was cut. *)
let join tokens =
  let rec go acc = function
    | Parser.Text a :: Parser.Text b :: rest ->
        go acc (Parser.Text (a ^ b) :: rest)
    | Parser.Data a :: Parser.Data b :: rest ->
        go acc (Parser.Data (a ^ b) :: rest)
    | token :: rest -> go (token :: acc) rest
    | [] -> List.rev acc
  in
  go [] tokens

let show tokens =
  String.concat ", "
    (List.map (Format.asprintf "%a" Parser.pp_token) (join tokens))

(* The tokens of [feeds], each read by one [feed] call. *)
let feed_all feeds =
  let p = Parser.create () in
  let acc = ref [] in
  List.iter
    (fun s ->
      ignore
        (Parser.feed p (Bytes.of_string s) ~off:0 ~len:(String.length s)
           (fun tok -> acc := tok :: !acc)
          : int))
    feeds;
  List.rev !acc

let check_tokens ?msg input expected =
  let msg = Option.value msg ~default:(Printf.sprintf "%S" input) in
  equal ~msg string expected (show (Parser.parse input))

(* [input], which ends in ground, gives [expected] however it is cut in
   two. *)
let check_cuts input expected =
  for cut = 0 to String.length input do
    let chunks =
      [
        String.sub input 0 cut; String.sub input cut (String.length input - cut);
      ]
    in
    equal
      ~msg:(Printf.sprintf "%S cut at %d" input cut)
      string expected
      (show (feed_all chunks))
  done

let text tokens =
  String.concat ""
    (List.filter_map (function Parser.Text s -> Some s | _ -> None) tokens)

let csi_params input =
  match Parser.parse input with
  | [ Parser.Csi { params; _ } ] -> params
  | _ -> failf "%S is not one control sequence" input

let read_sgr ?(from = Style.default) input =
  Style.apply_sgr (csi_params input) from

let check_style = Testable.make ~pp:Style.pp ~equal:Style.equal

let parser_chunked_utf8 () =
  let p = Parser.create () in
  (* Euro sign: \xE2\x82\xAC *)

  (* Feed byte 1 *)
  let t1 = feed_to_list p (Bytes.of_string "\xE2") 0 1 in
  equal ~msg:"chunk 1 buffered" int 0 (List.length t1);

  (* Feed byte 2 *)
  let t2 = feed_to_list p (Bytes.of_string "\x82") 0 1 in
  equal ~msg:"chunk 2 buffered" int 0 (List.length t2);

  (* Feed byte 3 + extra text *)
  let t3 = feed_to_list p (Bytes.of_string "\xACok") 0 3 in
  let combined =
    match t3 with
    | [ Parser.Text s ] -> s
    | [ Parser.Text utf8; Parser.Text plain ] -> utf8 ^ plain
    | _ -> ""
  in
  equal ~msg:"utf8 reconstructed" string "€ok" combined

let parser_chunked_utf8_same_buffer_slice () =
  (* Parser feeds must respect the logical off/len slice, not bytes that happen
     to follow in the same underlying buffer. *)
  let p = Parser.create () in
  let input = Bytes.of_string "\xE2\x82\xACok" in
  let t1 = feed_to_list p input 0 1 in
  equal ~msg:"same buffer byte 1 buffered" int 0 (List.length t1);
  let t2 = feed_to_list p input 1 1 in
  equal ~msg:"same buffer byte 2 buffered" int 0 (List.length t2);
  let t3 = feed_to_list p input 2 3 in
  equal ~msg:"same buffer utf8 reconstructed" string "€ok" (text t3)

let parser_chunked_escape () =
  let p = Parser.create () in
  (* Sequence: \x1b[31m (Red) *)

  (* Feed partial CSI *)
  let t1 = feed_to_list p (Bytes.of_string "\x1b[3") 0 3 in
  equal ~msg:"partial csi buffered" int 0 (List.length t1);

  (* Finish it *)
  let t2 = feed_to_list p (Bytes.of_string "1m") 0 2 in
  equal ~msg:"parsed split csi" string "CSI 31 m" (show t2);
  equal ~msg:"split csi is red" check_style
    (Style.make ~fg:Color.red ())
    (read_sgr "\x1b[31m")

let parser_controls_inside_csi () =
  (* A C0 control inside a CSI sequence is performed without ending it, DEL
     is dropped, ESC starts a new sequence, and CAN and SUB cancel it and are
     reported. *)
  check_tokens "\x1b[1\b\x7fC" "BS, CSI 1 C";
  check_tokens "\x1b[2\x1b[3C" "CSI 3 C";
  check_tokens "\x1b[2\x18b" "CAN, \"b\"";
  check_tokens "\x1b[2\x1ab" "SUB, \"b\""

let parser_malformed_input () =
  (* A lone ESC at the end of the input is an unfinished sequence: no
     effect. *)
  check_tokens "foo\x1b" "\"foo\""

(* Regression test: 0xFE and 0xFF are never valid UTF-8 lead bytes. Previously,
   the parser incorrectly treated any byte >= 0xC0 as a potential multi-byte
   sequence start and would buffer it, causing subsequent bytes to be lost.
   Valid UTF-8 lead bytes are only 0xC2-0xF4. *)
let parser_invalid_utf8_lead_bytes () =
  let p = Parser.create () in
  (* Input: 'A', 0xFF (invalid), 0xFE (invalid), 'B', 'C' *)
  let input = Bytes.create 5 in
  Bytes.set input 0 'A';
  Bytes.set input 1 '\xff';
  Bytes.set input 2 '\xfe';
  Bytes.set input 3 'B';
  Bytes.set input 4 'C';
  let tokens = feed_to_list p input 0 5 in
  (* U+FFFD in UTF-8 is \xEF\xBF\xBD *)
  equal ~msg:"invalid lead bytes produce replacements" string
    "A\xEF\xBF\xBD\xEF\xBF\xBDBC" (text tokens)

let parser_max_length_overflow () =
  (* No byte count bounds a sequence: a long number saturates. What is bounded
     is the fields kept, 256, and a sequence with more has no effect. *)
  check_tokens
    ("\x1b[" ^ String.make 300 '1' ^ "m")
    (Printf.sprintf "CSI %d m" max_int);
  let fields n = String.concat ";" (List.init n (fun _ -> "1")) in
  equal ~msg:"256 fields" int 256
    (Parser.Params.count (csi_params ("\x1b[" ^ fields 256 ^ "m")));
  check_tokens ~msg:"257 fields" ("\x1b[" ^ fields 257 ^ "mX") "\"X\"";
  check_tokens ~msg:"257 fields with sub-parameters"
    ("\x1b[" ^ fields 200 ^ ":"
    ^ String.concat ":" (List.init 57 string_of_int)
    ^ "mX")
    "\"X\""

let parser_osc8_st_terminated () =
  check_tokens "\x1b]8;;http://foo\x1b\\txt\x1b]8;;\x1b\\"
    "OSC, \"8;;http://foo\", ST, \"txt\", OSC, \"8;;\", ST"

let parser_osc8_bel_terminated () =
  check_tokens "\x1b]8;;http://foo\x07txt\x1b]8;;\x07"
    "OSC, \"8;;http://foo\", BEL, \"txt\", OSC, \"8;;\", BEL"

let parser_protocol_string_controls () =
  (* A DCS has a header like a control sequence: XTVERSION's reply is
     DCS > | then its text. *)
  check_tokens "\x1bP>|kitty(0.40.1)\x1b\\" "DCS > |, \"kitty(0.40.1)\", ST";
  check_tokens "\x1b_Gi=1;OK\x1b\\" "APC, \"Gi=1;OK\", ST";
  check_tokens "\x1b^pm-data\x1b\\" "PM, \"pm-data\", ST";
  check_tokens "\x1bXsos-data\x1b\\" "SOS, \"sos-data\", ST"

let parser_protocol_string_controls_chunked () =
  (* Strings are streamed: their content is reported as it arrives. *)
  let p = Parser.create () in
  let t1 = feed_to_list p (Bytes.of_string "\x1bP>|kitty") 0 9 in
  equal ~msg:"DCS opened and streamed" string "DCS > |, \"kitty\"" (show t1);
  let t2 = feed_to_list p (Bytes.of_string "(0.40.1)\x1b") 0 9 in
  equal ~msg:"DCS split ESC held" string "\"(0.40.1)\"" (show t2);
  let t3 = feed_to_list p (Bytes.of_string "\\") 0 1 in
  equal ~msg:"split DCS terminator" string "ST" (show t3);
  check_cuts "\x1bP>|kitty(0.40.1)\x1b\\" "DCS > |, \"kitty(0.40.1)\", ST"

let parser_esc_ends_string_controls () =
  (* ESC ends a control string, and starts a sequence unless it is ST. The
     string is aborted, not complete. *)
  check_tokens "\x1bP0;q\x1b[Cb" "DCS 0; q, Aborted, CSI C, \"b\"";
  List.iter
    (fun (intro, name) ->
      check_tokens
        ("\x1b" ^ intro ^ "0;q\x1b[Cb")
        (name ^ ", \"0;q\", Aborted, CSI C, \"b\""))
    [ ("X", "SOS"); ("^", "PM"); ("_", "APC"); ("]", "OSC") ]

let parser_non_ascii_ends_csi () =
  (* U+009B is not CSI in UTF-8: it is dropped, and U+FFFE is a character.
     A non-ASCII character inside a sequence ends it and is dropped, also
     when it is split across feeds. *)
  equal ~msg:"whole" string "x\xef\xbf\xbe</svg>"
    (text (Parser.parse "x\xc2\x9b\xef\xbf\xbe</svg>"));
  let p = Parser.create () in
  let feed s = feed_to_list p (Bytes.of_string s) 0 (String.length s) in
  let first = feed "x\x1b[1\xef\xbf" in
  equal ~msg:"split" string "x</svg>" (text (first @ feed "\xbe</svg>"))

let parser_csi_empty_parameter_defaults () =
  (* An absent parameter is not 0: the device gives it its default. *)
  let pn input i = Parser.Params.pn (csi_params input) i ~default:1 in
  equal ~msg:"CUP omitted row" (pair int int) (1, 5)
    (pn "\x1b[;5H" 0, pn "\x1b[;5H" 1);
  equal ~msg:"CUP omitted col" (pair int int) (5, 1)
    (pn "\x1b[5;H" 0, pn "\x1b[5;H" 1);
  equal ~msg:"CUP no parameters" (pair int int) (1, 1)
    (pn "\x1b[H" 0, pn "\x1b[H" 1);
  equal ~msg:"CUU omitted count" int 1 (pn "\x1b[A" 0);
  check_tokens "\x1b[;5H" "CSI ;5 H";
  equal ~msg:"SGR empty param is reset, then fg" check_style
    (Style.make ~fg:Color.red ())
    (read_sgr ~from:(Style.make ~bold:true ()) "\x1b[;31m")

let parser_utf8_split_invariance () =
  (* Feeding a byte stream whole or split at any boundary must decode to the
     same text (found by the matrix.vte split-invariance property fuzzing). *)
  let decode feeds = text (feed_all feeds) in
  (* Exact decodes for the three chunk-boundary defects. *)
  equal ~msg:"non-continuation after pending lead is not swallowed" string
    "\"\xEF\xBF\xBD\", LF"
    (show (feed_all [ "\xC3"; "\n" ]));
  equal ~msg:"overlong completes to one replacement per byte" string
    "\xEF\xBF\xBD\xEF\xBF\xBD\xEF\xBF\xBD"
    (decode [ "\xE0\x80"; "\x80" ]);
  equal ~msg:"invalid tail is not buffered over a following scalar" string
    "\xEF\xBF\xBD\xE2\x82\xAC"
    (decode [ "\xE0\xE2"; "\x82\xAC" ]);
  (* Split invariance at every cut point. *)
  let check_all_cuts name input =
    let whole = show (feed_all [ input ]) in
    for cut = 0 to String.length input do
      let split =
        feed_all
          [
            String.sub input 0 cut;
            String.sub input cut (String.length input - cut);
          ]
      in
      equal
        ~msg:(Printf.sprintf "%s cut at %d" name cut)
        string whole (show split)
    done
  in
  check_all_cuts "pending lead then newline" "\xC3\nX";
  check_all_cuts "overlong" "\xE0\x80\x80X";
  check_all_cuts "invalid tail then euro" "\xE0\xE2\x82\xACX";
  check_all_cuts "surrogate" "\xED\xA0\x80X";
  check_all_cuts "valid euro" "\xE2\x82\xACX";
  check_all_cuts "truncated at eof" "X\xF0\x9F\x9A";
  check_all_cuts "csi then pending lead" "\x1b[100;100A\xC3\nX";
  equal ~msg:"truncated at the end of the input" string "X\xEF\xBF\xBD"
    (text (Parser.parse "X\xF0\x9F\x9A"))

(* The end of the input finishes a character and a string, drops a sequence,
   and leaves the parser at the start of a stream. *)
let parser_finish () =
  let finished feeds =
    let p = Parser.create () in
    let acc = ref [] in
    let emit tok = acc := tok :: !acc in
    List.iter
      (fun s ->
        ignore
          (Parser.feed p (Bytes.of_string s) ~off:0 ~len:(String.length s) emit
            : int);
        Parser.finish p emit)
      feeds;
    show (List.rev !acc)
  in
  equal ~msg:"an unfinished character" string "\"X\xEF\xBF\xBD\""
    (finished [ "X\xF0\x9F\x9A" ]);
  equal ~msg:"an open string" string "OSC, \"0;t\", Aborted"
    (finished [ "\x1b]0;t" ]);
  equal ~msg:"an ESC that may begin ST" string "OSC, \"0;t\", Aborted"
    (finished [ "\x1b]0;t\x1b" ]);
  equal ~msg:"an unfinished sequence, then a new stream" string "\"m\""
    (finished [ "\x1b[3"; "m" ]);
  equal ~msg:"parse is feed then finish" string
    (show (Parser.parse "a\x1bPq#0\xC3"))
    (finished [ "a\x1bPq#0\xC3" ])

(* A boundary is where every byte fed belongs to a reported token. *)
let parser_at_boundary () =
  let after feeds =
    let p = Parser.create () in
    List.iter
      (fun s ->
        ignore
          (Parser.feed p (Bytes.of_string s) ~off:0 ~len:(String.length s)
             ignore
            : int))
      feeds;
    p
  in
  let check msg expected feeds =
    equal ~msg bool expected (Parser.at_boundary (after feeds))
  in
  check "a new parser" true [];
  check "after text" true [ "abc" ];
  check "after a control" true [ "a\n" ];
  check "after a complete sequence" true [ "\x1b[1;31m" ];
  check "after a complete string" true [ "\x1b]0;t\x07" ];
  check "after ST" true [ "\x1bPq\x1b\\" ];
  check "a cut character" false [ "X\xF0\x9F" ];
  check "a lone ESC" false [ "\x1b" ];
  check "a sequence without its final byte" false [ "\x1b[3" ];
  check "an escape sequence's intermediate" false [ "\x1b(" ];
  check "a DCS header" false [ "\x1bP1" ];
  check "an open string" false [ "\x1b]0;t" ];
  check "an ESC in a string" false [ "\x1b]0;t\x1b" ];
  check "a malformed sequence before its final byte" false [ "\x1b[1<" ];
  check "the character completed" true [ "X\xF0\x9F"; "\x9A\x80" ];
  check "the sequence completed" true [ "\x1b["; "3"; "m" ];
  (* Asking changes nothing: the sequence still completes. *)
  let p = after [ "\x1b[3" ] in
  ignore (Parser.at_boundary p);
  equal ~msg:"asking leaves the sequence in progress" string "CSI 3 m"
    (show (feed_to_list p (Bytes.of_string "m") 0 1));
  let p = after [ "\x1b]0;t" ] in
  Parser.finish p ignore;
  equal ~msg:"finish returns to a boundary" bool true (Parser.at_boundary p);
  let p = after [ "\x1b[3" ] in
  Parser.reset p;
  equal ~msg:"reset returns to a boundary" bool true (Parser.at_boundary p)

let parser_c1_controls () =
  (* In UTF-8, U+0080..U+009F are not C1 controls: they are dropped, whatever
     the state, and the text around them stays. *)
  check_cuts "a\xc2\x9b2Cb" "\"a2Cb\"";
  check_cuts "a\xc2\x9d0;t\xc3\xa9\xc2\x9cb" "\"a0;t\xc3\xa9b\"";
  check_cuts "a\xc2\x8db" "\"ab\"";
  check_cuts "\xc2\xa0\xc2\x9b1C" "\"\xc2\xa01C\"";
  (* Written ESC Fe, they are controls. *)
  check_cuts "a\x1b[2Cb" "\"a\", CSI 2 C, \"b\"";
  check_cuts "a\x1bMb" "\"a\", RI, \"b\""

let parser_charset_selection_split () =
  (* Designations are escape sequences: ESC, intermediates, a final byte. A
     lone designation byte arriving after a chunk that ended on the ESC used
     to rewind past the start of the current input and crash on Bytes.blit
     (found by the matrix.vte split-invariance property fuzzing). *)
  equal ~msg:"lone designation byte after an ESC-final chunk" string
    "\xF0\x9F\x98\x80"
    (text (feed_all [ "\xF0\x9F\x98\x80\x1b"; "%" ]));
  check_cuts "a\x1b(Bok" "\"a\", ESC ( B, \"ok\"";
  check_cuts "a\x1b%Gok" "\"a\", ESC % G, \"ok\"";
  check_cuts "\xF0\x9F\x98\x80\x1b%" "\"\xF0\x9F\x98\x80\"";
  (* Spaces are intermediates: four of them, and no final byte yet. *)
  check_cuts "\x1b(   " ""

let parser_unknown_escape_sequences () =
  (* An escape sequence runs from ESC through any intermediates (0x20-0x2F) to
     its final byte (0x30-0x7E). DECKPAM and DECKPNM, sent by smkx and rmkx,
     used to print "=" and ">". *)
  equal ~msg:"DECKPAM and DECKPNM" string "abc"
    (text (feed_all [ "a\x1b=b\x1b>c" ]));
  check_tokens "\x1b#3" "ESC # 3";
  check_tokens "\x1b%G\x1b%@" "ESC % G, ESC % @";
  check_cuts "a\x1b Fb\x1b#3c" "\"a\", ESC SP F, \"b\", ESC # 3, \"c\""

let parser_sgr_colon_subparameters () =
  (* Underline style sub-parameters (emitted by neovim/kitty for undercurl):
     curly degrades to plain underline, 4:0 disables, 4:2 is double. *)
  let underlined = Style.make ~underline:true ~double_underline:true () in
  equal ~msg:"4:3 (curly) degrades to underline" check_style
    (Style.make ~underline:true ())
    (read_sgr "\x1b[4:3m");
  equal ~msg:"4:0 disables underline" check_style Style.default
    (read_sgr ~from:underlined "\x1b[4:0m");
  equal ~msg:"4:2 is double underline" check_style
    (Style.make ~double_underline:true ())
    (read_sgr "\x1b[4:2m");
  (* Colon-form truecolor, with and without the ISO 8613-6 colorspace id. *)
  let check_rgb name input =
    equal ~msg:name check_style
      (Style.make ~fg:(Color.of_rgb 255 0 100) ())
      (read_sgr input)
  in
  check_rgb "colon rgb" "\x1b[38:2:255:0:100m";
  check_rgb "colon rgb with colorspace" "\x1b[38:2::255:0:100m";
  equal ~msg:"colon palette bg" check_style
    (Style.make ~bg:(Color.indexed 196) ())
    (read_sgr "\x1b[48:5:196m");
  (* Underline color (58) is not representable: its sub-parameters must be
     skipped, not misread as free-standing SGR parameters. *)
  equal ~msg:"58:2::2:2:2 skipped whole" check_style Style.default
    (read_sgr "\x1b[58:2::2:2:2m");
  equal ~msg:"58;2;2;2;2 skipped whole" check_style Style.default
    (read_sgr "\x1b[58;2;2;2;2m");
  (* Sub-parameterized params mixed with plain ones. *)
  equal ~msg:"1;4:3;31 parses each parameter" check_style
    (Style.make ~bold:true ~underline:true ~fg:Color.red ())
    (read_sgr "\x1b[1;4:3;31m");
  check_tokens "\x1b[1;4:3;31m" "CSI 1;4:3;31 m"

let style_apply_sgr () =
  (* SGR reads the attribute codes the writer writes, and a few more. *)
  let all =
    Style.make ~bold:true ~dim:true ~underline:true ~double_underline:true
      ~framed:true ~encircled:true ~link:"http://l" ()
  in
  let check msg ?(from = Style.default) input expected =
    equal ~msg check_style expected (read_sgr ~from input)
  in
  check "0 keeps the link" ~from:all "\x1b[0m" (Style.make ~link:"http://l" ());
  check "no parameter is 0" ~from:all "\x1b[m" (Style.make ~link:"http://l" ());
  check "22 clears bold and dim" ~from:all "\x1b[22m"
    (Style.make ~underline:true ~double_underline:true ~framed:true
       ~encircled:true ~link:"http://l" ());
  check "24 clears both underlines" ~from:all "\x1b[24;54m"
    (Style.make ~bold:true ~dim:true ~link:"http://l" ());
  check "21 is double underline" "\x1b[21m"
    (Style.make ~double_underline:true ());
  check "6 is blink" "\x1b[6m" (Style.make ~blink:true ());
  check "applied in order" "\x1b[7;0;1m" (Style.make ~bold:true ());
  check "palette" "\x1b[91;104m"
    (Style.make ~fg:Color.bright_red ~bg:Color.bright_blue ());
  check "defaults" "\x1b[31;39;41;49m"
    (Style.make ~fg:Color.default_fg ~bg:Color.default_bg ());
  check "38;5;n then more" "\x1b[38;5;196;1m"
    (Style.make ~fg:(Color.indexed 196) ~bold:true ());
  check "an incomplete color ends the list" "\x1b[1;38;5m"
    (Style.make ~bold:true ());
  check "an unknown color form is skipped alone" "\x1b[38;3;1m"
    (Style.make ~italic:true ~bold:true ());
  check "unknown codes have no effect" "\x1b[10;26;50;60;1000m" Style.default;
  (* Sub-parameters are defined for the colours and the underline style
     only: elsewhere the parameter has no effect, and the others act. *)
  check "1:2 is not bold" "\x1b[1:2m" Style.default;
  check "1:: is not bold" "\x1b[1::m" Style.default;
  check "a lone colon is not 0" ~from:all "\x1b[:m" all;
  check "the other parameters act" "\x1b[3;1:2;4:3m"
    (Style.make ~italic:true ~underline:true ());
  check "4:0 is 24"
    ~from:(Style.make ~underline:true ())
    "\x1b[4:0m" Style.default;
  check "38:5:n" "\x1b[38:5:9m" (Style.make ~fg:(Color.indexed 9) ())

let parser_mode_sequences () =
  (* Modes keep every parameter, so batched sequences like
     [CSI ? 1049 ; 2004 h] keep all modes. *)
  check_tokens "\x1b[?1049;2004h" "CSI ?1049;2004 h";
  check_tokens "\x1b[?25l" "CSI ?25 l";
  check_tokens "\x1b[4h" "CSI 4 h";
  check_tokens "\x1b[20l" "CSI 20 l";
  let modes = csi_params "\x1b[?1049;;2004h" in
  equal ~msg:"modes, the absent one skipped"
    (list (option int))
    [ Some 1049; None; Some 2004 ]
    (List.init (Parser.Params.count modes) (Parser.Params.value modes))

let parser_csi_private_and_intermediates () =
  (* The marker and intermediates name the function along with the final
     byte: XTMODKEYS (CSI > 4 ; 1 m) is not SGR, CSI = 5 u is not a cursor
     restore, and SL (CSI 1 SP @) is not ICH. A marker after a parameter is
     malformed. *)
  check_tokens "\x1b[>4;1m" "CSI >4;1 m";
  check_tokens "\x1b[=5u" "CSI =5 u";
  check_tokens "\x1b[>1u" "CSI >1 u";
  check_tokens "\x1b[?5r" "CSI ?5 r";
  check_tokens "\x1b[1?A" "";
  check_tokens "\x1b[1 @" "CSI 1 SP @";
  check_tokens "\x1b[1 A" "CSI 1 SP A";
  check_tokens "\x1b[?2J" "CSI ?2 J";
  check_tokens "\x1b[?K" "CSI ? K";
  check_tokens "\x1b[1\"q" "CSI 1 \" q"

(* The recovery rules, one test each. *)

let parser_esc_can_sub_end_everything () =
  (* ESC, CAN and SUB end whatever is in progress; CAN and SUB are reported,
     ESC starts a new sequence. *)
  List.iter
    (fun (prefix, opened) ->
      check_tokens (prefix ^ "\x18x") (opened ^ "CAN, \"x\"");
      check_tokens (prefix ^ "\x1ax") (opened ^ "SUB, \"x\"");
      check_tokens (prefix ^ "\x1b[Cx") (opened ^ "CSI C, \"x\""))
    [
      ("\x1b", "");
      ("\x1b(", "");
      ("\x1b[1;2", "");
      ("\x1bP1$", "");
      ("\x1b]0;t", "OSC, \"0;t\", Aborted, ");
      ("\x1bPq", "DCS q, Aborted, ");
      ("\x1b_G", "APC, \"G\", Aborted, ");
      ("\x1b^p", "PM, \"p\", Aborted, ");
      ("\x1bXs", "SOS, \"s\", Aborted, ");
    ];
  check_tokens "\x1b\x1b[Cb" "CSI C, \"b\""

let parser_c0_inside_sequences () =
  (* Performed inside ESC and CSI, never inside a DCS header or a string. *)
  check_tokens "\x1b#\n8" "LF, ESC # 8";
  check_tokens "\x1b(\rB" "CR, ESC ( B";
  check_tokens "\x1b[2\nC" "LF, CSI 2 C";
  check_tokens "\x1bP1\n$q\x1b\\" "DCS 1 $ q, ST";
  check_tokens "\x1bP1\n2q\x1b\\" "DCS 12 q, ST";
  (* In a string BS..CR are content, BEL ends an OSC only, and the other C0
     controls and DEL are dropped. *)
  check_tokens "\x1b]0;a\x01\x08\t\n\x0b\x0c\r\x7fb\x07"
    "OSC, \"0;a\\b\\t\\n\\x0B\\x0C\\rb\", BEL";
  check_tokens "\x1bPq\x07a\x1b\\" "DCS q, \"a\", ST";
  check_tokens "\x1b_\x07a\x1b\\" "APC, \"a\", ST"

let parser_del () =
  (* DEL is reported in ground and dropped inside sequences and strings. *)
  check_tokens "a\x7fb" "\"a\", DEL, \"b\"";
  check_tokens "\x1b\x7f7" "ESC 7";
  check_tokens "\x1b[1\x7f;2H" "CSI 1;2 H";
  check_tokens "\x1b]0;a\x7fb\x07" "OSC, \"0;ab\", BEL"

let parser_malformed_sequences () =
  (* Consumed to the final byte, with no effect. *)
  check_tokens "\x1b[1?2Ax" "\"x\"";
  check_tokens "\x1b[??2Ax" "\"x\"";
  check_tokens "\x1b[1 2Ax" "\"x\"";
  check_tokens "\x1b[ ?Ax" "\"x\"";
  (* Four intermediates are kept, a fifth makes the sequence malformed. *)
  check_tokens "\x1b$( !Ix" "ESC $ ( SP ! I, \"x\"";
  check_tokens "\x1b$( !!Ix" "\"x\"";
  check_tokens "\x1b[1 !\"#Ax" "CSI 1 SP ! \" # A, \"x\"";
  check_tokens "\x1b[1 !\"#$Ax" "\"x\"";
  (* A malformed DCS is consumed to its end without tokens. *)
  check_tokens "\x1bP1?2qdata\x1b\\x" "\"x\"";
  check_tokens "\x1bP1?2qdata\x18x" "CAN, \"x\""

let parser_non_ascii_in_sequences () =
  (* A non-ASCII character ends an escape or control sequence and is dropped;
     in a DCS header it makes the DCS malformed. *)
  check_tokens "\x1b\xc3\xa9b" "\"b\"";
  check_tokens "\x1b(\xc3\xa9b" "\"b\"";
  check_tokens "\x1b[1\xe4\xb8\xadCb" "\"Cb\"";
  check_tokens "\x1bP1\xc3\xa9q data\x1b\\b" "\"b\"";
  (* Inside a string it is content. *)
  check_tokens "\x1b]0;\xc3\xa9\x07" "OSC, \"0;\xc3\xa9\", BEL";
  (* ESC then a non-ASCII character in a string: the string is aborted. *)
  check_tokens "\x1b]0;t\x1b\xc3\xa9b" "OSC, \"0;t\", Aborted, \"b\""

let parser_utf8 () =
  (* Ill-formed subsequences become one U+FFFD each (maximal subparts). *)
  let fffd = "\xEF\xBF\xBD" in
  check_tokens "a\x80b" ("\"a" ^ fffd ^ "b\"");
  check_tokens "a\x9bb" ("\"a" ^ fffd ^ "b\"");
  check_tokens "a\xe2\x82b" ("\"a" ^ fffd ^ "b\"");
  check_tokens "a\xed\xa0\x80b" ("\"a" ^ fffd ^ fffd ^ fffd ^ "b\"");
  check_tokens "a\xf4\x90\x80\x80b" ("\"a" ^ fffd ^ fffd ^ fffd ^ fffd ^ "b\"");
  check_tokens "a\xe2\x82" ("\"a" ^ fffd ^ "\"");
  (* In strings too. *)
  check_tokens "\x1b]0;\xff\x07" ("OSC, \"0;" ^ fffd ^ "\", BEL");
  (* U+0080..U+009F are dropped in text and in strings. *)
  check_tokens "\xc2\x85na\xc2\x9fve" "\"nave\"";
  check_tokens "\x1b]0;a\xc2\x9cb\x07" "OSC, \"0;ab\", BEL"

let parser_string_terminators () =
  check_tokens "\x1b]2;t\x1b\\" "OSC, \"2;t\", ST";
  check_tokens "\x1b]2;t\x07" "OSC, \"2;t\", BEL";
  check_tokens "\x1b]2;t" "OSC, \"2;t\", Aborted";
  check_tokens "\x1b]2;t\x1b" "OSC, \"2;t\", Aborted";
  check_tokens "\x1b]2;t\x1b7" "OSC, \"2;t\", Aborted, ESC 7";
  check_tokens "\x1bX\x07\x1b\\" "SOS, ST";
  (* A lone ST and the C1 controls written ESC Fe. *)
  check_tokens "a\x1b\\b" "\"ab\"";
  check_tokens "\x1bD\x1bE\x1bH\x1bM\x1bN\x1bZ\x1b@"
    "IND, NEL, HTS, RI, SS2, SCI, ESC @";
  check_tokens "\x1b]\x1b\\" "OSC, ST"

let parser_params () =
  let p = csi_params "\x1b[0;;7:1::3;99999999999999999999m" in
  let module P = Parser.Params in
  equal ~msg:"count" int 4 (P.count p);
  equal ~msg:"pn 0 is the default" int 9 (P.pn p 0 ~default:9);
  equal ~msg:"ps 0 is 0" int 0 (P.ps p 0 ~default:9);
  equal ~msg:"pn absent" int 9 (P.pn p 1 ~default:9);
  equal ~msg:"ps absent" int 9 (P.ps p 1 ~default:9);
  equal ~msg:"value absent" (option int) None (P.value p 1);
  equal ~msg:"pn out of range" int 9 (P.pn p 4 ~default:9);
  equal ~msg:"fields" (list int) [ 1; 1; 4; 1; 0 ] (List.init 5 (P.fields p));
  equal ~msg:"sub-parameters"
    (list (option int))
    [ Some 7; Some 1; None; Some 3; None ]
    (List.init 5 (P.field p 2));
  equal ~msg:"saturated" int max_int (P.ps p 3 ~default:0);
  equal ~msg:"no parameters" int 0 (P.count (csi_params "\x1b[m"));
  equal ~msg:"two absent parameters" int 2 (P.count (csi_params "\x1b[;m"))

let parser_feed_contract () =
  let p = Parser.create () in
  raises (Invalid_argument "Ansi.Parser.feed") (fun () ->
      ignore (Parser.feed p (Bytes.create 4) ~off:2 ~len:3 ignore : int));
  raises (Invalid_argument "Ansi.Parser.feed") (fun () ->
      ignore (Parser.feed p (Bytes.create 4) ~off:(-1) ~len:1 ignore : int));
  equal ~msg:"len 0 does nothing" int 0
    (List.length (feed_to_list p (Bytes.of_string "\x1b]") 0 0));
  (* reset drops the string in progress without closing it. *)
  ignore (feed_to_list p (Bytes.of_string "\x1b]0;title") 0 9);
  Parser.reset p;
  equal ~msg:"after reset" string "\"x\""
    (show (feed_to_list p (Bytes.of_string "x\x1b\\") 0 3))

(* A stop takes effect after the token it is called on: [feed] reads no
   later byte and says how many it read, and the next feed goes on from
   there. *)
let parser_stop () =
  let p = Parser.create () in
  let stop_at keep b off len =
    let acc = ref [] in
    let n =
      Parser.feed p (Bytes.of_string b) ~off ~len (fun tok ->
          acc := tok :: !acc;
          if keep tok then Parser.stop p)
    in
    (n, show (List.rev !acc))
  in
  let csi = function Parser.Csi _ -> true | _ -> false in
  let text = function Parser.Text _ -> true | _ -> false in
  let s = "ab\x1b[1mcd" in
  equal ~msg:"after a sequence" (pair int string) (6, "\"ab\", CSI 1 m")
    (stop_at csi s 0 (String.length s));
  equal ~msg:"the rest" (pair int string) (2, "\"cd\"")
    (stop_at csi s 6 (String.length s - 6));
  equal ~msg:"after text that a control ends" (pair int string) (2, "\"ab\"")
    (stop_at text "ab\ncd" 0 5);
  equal ~msg:"the control, then text" (pair int string) (3, "LF, \"cd\"")
    (stop_at text "ab\ncd" 2 3);
  ignore (stop_at text "\xc3" 0 1 : int * string);
  equal ~msg:"after a character a feed completes" (pair int string)
    (1, "\"\xc3\xa9\"") (stop_at text "\xa9x" 0 2);
  equal ~msg:"CAN ends a string and is reported" (pair int string)
    (6, "OSC, \"0;t\", Aborted, CAN")
    (stop_at
       (function Parser.Close _ -> true | _ -> false)
       "\x1b]0;t\x18x" 0 7);
  Parser.stop p;
  equal ~msg:"a stop outside a feed does nothing" (pair int string) (2, "\"xy\"")
    (stop_at (fun _ -> false) "xy" 0 2)

let parser_split_invariance () =
  (* Every rule above, cut anywhere. *)
  check_cuts
    "a\xc3\xa9\x1b[1;2:3 \
     q\x1b(%6\x1bP1$q\x07\xe2\x82\xac\x1b\\\x1b]8;;u\x07\x1b#\n\
     8\x1b_x\x18\x7f\xc2\x9bz"
    "\"a\xc3\xa9\", CSI 1;2:3 SP q, ESC ( % 6, DCS 1 $ q, \"\xe2\x82\xac\", \
     ST, OSC, \"8;;u\", BEL, LF, ESC # 8, APC, \"x\", Aborted, CAN, DEL, \"z\""

let parser_known_defects () =
  (* The probes run against the parser this one replaces. *)
  check_tokens "\x1b]0;t\x18abc" "OSC, \"0;t\", Aborted, CAN, \"abc\"";
  check_tokens "\x1bPq\x1aabc" "DCS q, Aborted, SUB, \"abc\"";
  check_tokens "\x1b[1\xc2\x9b2Cb" "\"2Cb\"";
  check_tokens "\x1b]0;t\xc2\x9b2Cb" "OSC, \"0;t2Cb\", Aborted";
  check_tokens "\x1bPq\xc2\x9b2Cb" "DCS q, \"2Cb\", Aborted";
  check_tokens "\x1b%\x1b[Cb" "CSI C, \"b\"";
  check_tokens "\x1b%/G" "ESC % / G";
  check_tokens "\x1b(%6a" "ESC ( % 6, \"a\"";
  check_tokens "\x1b(\">q" "ESC ( \" >, \"q\"";
  check_tokens "\x1b(\x1b[Cb" "CSI C, \"b\"";
  check_tokens "\x1b\x1b[Cb" "CSI C, \"b\"";
  check_tokens "\x1b\xc3\xa9b" "\"b\"";
  check_tokens "\x1b#\n8" "LF, ESC # 8";
  check_tokens "\x1b_Gi=1\x1b[mx" "APC, \"Gi=1\", Aborted, CSI m, \"x\"";
  check_tokens "\x1b[0A\x1b[0I" "CSI 0 A, CSI 0 I";
  check_tokens "\x1b[4611686018427387904e" (Printf.sprintf "CSI %d e" max_int);
  check_tokens "\x1b[?6;1n\x1b[>00c" "CSI ?6;1 n, CSI >0 c";
  (* Long strings are streamed: nothing leaks into text. *)
  let sixel = "\x1bPq" ^ String.make 10_000 '#' ^ "\x1b\\" in
  equal ~msg:"long DCS" string "" (text (Parser.parse sixel));
  let osc = "\x1b]52;c;" ^ String.make 10_000 'A' ^ "\x07" in
  equal ~msg:"long OSC" int (10_000 + 5)
    (List.fold_left
       (fun n -> function Parser.Data s -> n + String.length s | _ -> n)
       0 (Parser.parse osc))

let parser_pp_token () =
  check_tokens "\x00\x1f\x7f" "NUL, IS1, DEL";
  check_tokens "\x1b[38:2::1:2:3m" "CSI 38:2::1:2:3 m";
  check_tokens "\x1b[!p" "CSI ! p";
  check_tokens "\x1b P" "ESC SP P"

(* --- 5. Writer & Low-Level Escape --- *)

let writer_utf8_encoding () =
  let w = Writer.create 16 in

  (* 1 byte: A *)
  utf8 (Char.code 'A') w;
  (* 3 byte: € (0x20AC) *)
  utf8 0x20AC w;
  (* 4 byte: 🚀 (Rocket 0x1F680) *)
  utf8 0x1F680 w;

  let out = Writer.contents w in
  equal ~msg:"utf8 encoding" string "A€🚀" out

let writer_hyperlink_params () =
  let w = Writer.create 64 in
  hyperlink_start ~params:"id=123" ~url:"http://foo" w;
  let out = Writer.contents w in
  (* OSC 8 ; params ; url ST (\x1b\\) *)
  check_seq "hyperlink with params" "\x1b]8;id=123;http://foo\x1b\\" out

let clipboard_base64_payload () =
  check_seq "OSC 52 with base64 payload" "\x1b]52;c;aGVsbG8=\x07"
    (Ansi.to_string (Ansi.set_clipboard ~text:"hello"));
  check_seq "empty clipboard payload" "\x1b]52;c;\x07"
    (Ansi.to_string (Ansi.set_clipboard ~text:""));
  (* Padding cases: one and two trailing bytes. *)
  check_seq "one-byte payload" "\x1b]52;c;YQ==\x07"
    (Ansi.to_string (Ansi.set_clipboard ~text:"a"));
  check_seq "two-byte payload" "\x1b]52;c;YWI=\x07"
    (Ansi.to_string (Ansi.set_clipboard ~text:"ab"))

(* An OSC's text is "composed of printable characters" (ctlseqs): a control
   or a byte that is not UTF-8 in a string would end the sequence early or
   start another, so the writers leave them out, and OSC 777 leaves out a ;
   from its title field. *)
let osc_text_is_printable () =
  let s f = Ansi.to_string f in
  equal ~msg:"title" string "\x1b]0;a]0;b\xc3\xa9c\x1b\\"
    (s (Ansi.set_title ~title:"a\x1b]0;\x07b\xc2\x9c\xffé\nc"));
  equal ~msg:"notify" string "\x1b]9;a;b: c;d\x07"
    (s (Ansi.notify `Osc9 ~title:"a;b\x07" ~body:"c;\x1bd"));
  equal ~msg:"notify, OSC 777" string "\x1b]777;notify;ab;c;d\x07"
    (s (Ansi.notify `Osc777 ~title:"a;b\x07" ~body:"c;\x1bd"));
  equal ~msg:"hyperlink" string "\x1b]8;id=1;http://a/\\b\x1b\\"
    (s (Ansi.hyperlink_start ~params:"id=1;" ~url:"http://a/\x1b\\b"));
  equal ~msg:"osc" string "\x1b]2;tt\x1b\\" (s (Ansi.osc ~payload:"2;t\x9ct"));
  equal ~msg:"explicit width" string "\x1b]66;w=2;ab\x1b\\"
    (s (Ansi.explicit_width ~width:2 ~text:"a\x07b"))

let notify_osc9_and_osc777 () =
  check_seq "OSC 9" "\x1b]9;Title: Body\x07"
    (Ansi.to_string (Ansi.notify `Osc9 ~title:"Title" ~body:"Body"));
  check_seq "OSC 777" "\x1b]777;notify;Title;Body\x07"
    (Ansi.to_string (Ansi.notify `Osc777 ~title:"Title" ~body:"Body"))

let tmux_passthrough_doubles_esc () =
  check_seq "DCS wrap with doubled ESC" "\x1bPtmux;\x1b\x1b]0;t\x1b\x1b\\\x1b\\"
    (Ansi.to_string (Ansi.tmux_passthrough (Ansi.set_title ~title:"t")))

let color_queries () =
  let q x = to_string (query x) in
  check_seq "foreground" "\027]10;?\027\\" (q Foreground_color);
  check_seq "background" "\027]11;?\027\\" (q Background_color);
  check_seq "cursor" "\027]12;?\027\\" (q Cursor_color);
  check_seq "palette entry" "\027]4;1;?\027\\" (q (Palette_entry 1));
  check_seq "bright entry" "\027]4;15;?\027\\" (q (Palette_entry 15));
  check_seq "clamped below" "\027]4;0;?\027\\" (q (Palette_entry (-3)));
  check_seq "clamped above" "\027]4;255;?\027\\" (q (Palette_entry 300))

let color_scheme_report_query () =
  check_seq "DSR 996" "\x1b[?996n"
    (Ansi.to_string (Ansi.query Ansi.Color_scheme_report))

let writer_invalid_scalar_replacement () =
  let w = Writer.create 8 in
  (* Surrogate code point should be clamped to U+FFFD *)
  utf8 0xD800 w;
  check_seq "invalid scalar -> replacement" "\xef\xbf\xbd" (Writer.contents w)

let writer_growth_preserves_output () =
  let w = Writer.create 0 in
  Writer.write_char w 'a';
  let tail = String.make 1024 'b' in
  Writer.write_string w tail;
  let snapshot = Writer.contents w in
  equal ~msg:"string growth preserves the prefix" string ("a" ^ tail) snapshot;
  Writer.reset_pos w;
  Writer.write_string w "reset";
  equal ~msg:"reset clears the length" string "reset" (Writer.contents w);
  equal ~msg:"contents does not alias retained storage" string ("a" ^ tail)
    snapshot;
  let w = Writer.create 4 in
  Writer.write_string w "abcd";
  Writer.write_subbytes w (Writer.buffer w) 1 3;
  equal ~msg:"growth accepts its own buffer as source" string "abcdbcd"
    (Writer.contents w);
  Writer.write_subbytes w (Writer.buffer w) 0 1;
  equal ~msg:"append accepts its own buffer without growth" string "abcdbcda"
    (Writer.contents w)

let writer_of_bytes () =
  let b = Bytes.make 6 '.' in
  let w = Writer.of_bytes b in
  equal ~msg:"starts empty" int 0 (Writer.len w);
  Writer.write_string w "abc";
  Writer.write_char w 'd';
  equal ~msg:"writes into the caller's bytes" string "abcd.."
    (Bytes.to_string b);
  is_true ~msg:"buffer is the caller's bytes" (Writer.buffer w == b);
  raises Writer.Buffer_full (fun () -> Writer.write_string w "xyz");
  equal ~msg:"a write that does not fit writes nothing" string "abcd.."
    (Bytes.to_string b);
  equal ~msg:"and leaves the length" int 4 (Writer.len w);
  raises Writer.Buffer_full (fun () ->
      Writer.write_subbytes w (Bytes.of_string "xyz") 0 3);
  Writer.write_string w "ef";
  equal ~msg:"a write that fits exactly" string "abcdef" (Writer.contents w);
  raises Writer.Buffer_full (fun () -> Writer.write_char w 'g');
  raises Writer.Buffer_full (fun () -> Ansi.cursor_up ~n:1 w);
  is_true ~msg:"never grows" (Writer.buffer w == b);
  Writer.reset_pos w;
  Ansi.cursor_up ~n:12 w;
  equal ~msg:"reusable after reset" string "\027[12A" (Writer.contents w);
  let w = Writer.of_bytes Bytes.empty in
  Writer.write_string w "";
  raises Writer.Buffer_full (fun () -> Writer.write_char w 'a')

let writer_checked_bounds () =
  let w = Writer.create 4 in
  Writer.write_string w "ok";
  let source = Bytes.of_string "abc" in
  List.iter
    (fun (off, len) ->
      (try
         Writer.write_subbytes w source off len;
         failf "expected invalid source slice off=%d len=%d" off len
       with Invalid_argument _ -> ());
      equal ~msg:"invalid slice preserves output" string "ok"
        (Writer.contents w))
    [ (-1, 1); (0, -1); (2, 2); (4, 0); (max_int, 1); (1, max_int) ];
  Writer.write_subbytes w source 3 0;
  equal ~msg:"empty slice at source end is valid" string "ok"
    (Writer.contents w)

let to_string_calls_emitter_once () =
  let calls = ref 0 in
  let payload = String.make 4096 'x' in
  let result =
    Ansi.to_string (fun writer ->
        incr calls;
        Writer.write_string writer (string_of_int !calls);
        Writer.write_string writer payload)
  in
  equal ~msg:"emitter executes once even when output grows" int 1 !calls;
  equal ~msg:"stateful emitter output is preserved" string ("1" ^ payload)
    result

(* --- 6. High Level API --- *)

let strip_edge_cases () =
  (* Standard *)
  equal ~msg:"strip standard" string "foo" (Ansi.strip "\x1b[31mfoo\x1b[0m");
  (* Incomplete at end *)
  equal ~msg:"strip incomplete" string "foo" (Ansi.strip "foo\x1b[");
  (* Lone ESC *)
  equal ~msg:"strip lone esc" string "foo" (Ansi.strip "foo\x1b");
  (* OSC title *)
  equal ~msg:"strip osc" string "foo" (Ansi.strip "\x1b]0;Title\007foo")

let strip_protocol_string_controls () =
  equal ~msg:"strip DCS" string "ok" (Ansi.strip "\x1bP>|kitty(0.40.1)\x1b\\ok");
  equal ~msg:"strip APC" string "ok" (Ansi.strip "\x1b_Gi=1;OK\x1b\\ok")

let strip_keeps_c0_and_follows_the_parser () =
  (* Line feeds and tabs survive, so a multiline paste keeps its lines. *)
  let plain = "line 1\n\tline 2\r\n" in
  is_true ~msg:"plain text is returned as is" (Ansi.strip plain == plain);
  equal ~msg:"C0 kept around sequences" string "a\nb\tc"
    (Ansi.strip "\x1b[1ma\n\x1b[0mb\x1b[2\tCc");
  (* Every recovery rule of the parser applies. *)
  equal ~msg:"CAN ends a string" string "\x18abc" (Ansi.strip "\x1b]0;t\x18abc");
  equal ~msg:"two-byte designator" string "a" (Ansi.strip "\x1b(%6a");
  equal ~msg:"ESC then non-ASCII" string "b" (Ansi.strip "\x1b\xc3\xa9b");
  equal ~msg:"unterminated string" string "a" (Ansi.strip "a\x1b]0;title");
  equal ~msg:"DEL and C1 dropped" string "ab" (Ansi.strip "a\x7f\xc2\x9bb");
  equal ~msg:"ill-formed UTF-8" string "a\xef\xbf\xbdb" (Ansi.strip "a\xffb");
  equal ~msg:"text kept whole" string "\xc3\xa9\xe4\xb8\xad"
    (Ansi.strip "\xc3\xa9\x1b[m\xe4\xb8\xad")

let contains s sub =
  let n = String.length sub in
  let rec at i =
    i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
  in
  at 0

(* Untrusted text keeps only what shows, in its order. *)
let sanitize_untrusted_text () =
  let plain = "line 1\n\tline 2" in
  is_true ~msg:"plain text is returned as is" (Ansi.sanitize plain == plain);
  List.iter
    (fun (msg, input, expected) ->
      equal ~msg string expected (Ansi.sanitize input))
    [
      ( "sequences and strings",
        "\x1b[2J\x1b]0;t\x07a\x1b]8;;http://x\x1b\\b\x1b[31mc",
        "abc" );
      ("C1 in both forms, DEL", "a\x1bDb\xc2\x9bc\x7fd", "abcd");
      ( "BS, BEL, SO and NUL go",
        "rm -rf /\b\b\b\b\b\b\bls\x07\x0e\x00",
        "rm -rf /ls" );
      ("a CR ends a line", "safe\revil", "safe\nevil");
      ("CR LF, CR CR LF, LF LF", "a\r\nb\r\r\nc\n\nd", "a\nb\nc\n\nd");
      ("VT, FF and NEL end lines", "a\x0bb\x0cc\x1bEd", "a\nb\nc\nd");
      ("tabs stay", "a\tb", "a\tb");
      ("ill-formed UTF-8", "a\xffb", "a\xef\xbf\xbdb");
      ( "bidi overrides and isolates",
        "if (\u{202E}nimda\u{202C}) \u{2066}x\u{2069}\u{200F}",
        "if (nimda) x" );
      ( "invisible characters",
        "pay\u{200B}pal\u{2060}\u{00AD}\u{FEFF}\u{3164}\u{E0041}",
        "paypal" );
      ("a selector after a base", "\u{2764}\u{FE0F}", "\u{2764}\u{FE0F}");
      ("a selector after no base", "\u{FE0F}a \u{FE0F}", "a ");
      ("a run of selectors", "a\u{FE0F}\u{FE0E}\u{E0100}", "a\u{FE0F}");
      ( "a ZWJ sequence",
        "\u{1F468}\u{200D}\u{1F4BB}\u{1F441}\u{FE0F}\u{200D}\u{1F5E8}\u{FE0F}",
        "\u{1F468}\u{200D}\u{1F4BB}\u{1F441}\u{FE0F}\u{200D}\u{1F5E8}\u{FE0F}"
      );
      ( "a ZWNJ in Persian",
        "\u{0645}\u{06CC}\u{200C}\u{062E}",
        "\u{0645}\u{06CC}\u{200C}\u{062E}" );
      ("a run of joiners", "a\u{200D}\u{200D}\u{200C}b", "a\u{200D}b");
      ("joiners after nothing", "\u{200D}a \u{200C}", "a ");
      ( "the flag of Scotland",
        "\u{1F3F4}\u{E0067}\u{E0062}\u{E0073}\u{E0063}\u{E0074}\u{E007F}",
        "\u{1F3F4}\u{E0067}\u{E0062}\u{E0073}\u{E0063}\u{E0074}\u{E007F}" );
      ( "tags on a flag that is not one",
        "\u{1F3F4}\u{E0068}\u{E0069}\u{E007F}",
        "\u{1F3F4}" );
      ("U+0085 is not NEL", "a\u{0085}b", "ab");
    ]

(* Whatever the input: valid UTF-8, no control but LF and HT, no bidi
   control, and sanitized already. *)
let pieces =
  [|
    "a";
    " ";
    "\n";
    "\r";
    "\t";
    "\x08";
    "\x07";
    "\x1b";
    "[";
    "]";
    "0";
    ";";
    "m";
    "\x07";
    "\x1b\\";
    "\x9b";
    "\xc2\x9b";
    "\x7f";
    "\xff";
    "\xe4\xb8";
    "\u{202E}";
    "\u{2067}";
    "\u{200E}";
    "\u{061C}";
    "\u{200D}";
    "\u{200C}";
    "\u{FE0F}";
    "\u{E0100}";
    "\u{180B}";
    "\u{E0067}";
    "\u{E0062}";
    "\u{E0073}";
    "\u{E0063}";
    "\u{E0074}";
    "\u{E007F}";
    "\u{1F3F4}";
    "\u{2764}";
    "\u{0301}";
    "\u{00AD}";
    "\u{FEFF}";
  |]

let sanitize_properties () =
  let rng = Random.State.make [| 42 |] in
  for _ = 1 to 20_000 do
    let s =
      String.concat ""
        (List.init (Random.State.int rng 12) (fun _ ->
             pieces.(Random.State.int rng (Array.length pieces))))
    in
    let t = Ansi.sanitize s in
    if not (String.is_valid_utf_8 t) then failf "%S: invalid UTF-8 %S" s t;
    String.iter
      (fun c ->
        if (c < ' ' && c <> '\n' && c <> '\t') || c = '\x7f' then
          failf "%S: control %C in %S" s c t)
      t;
    List.iter
      (fun bidi -> if contains t bidi then failf "%S: bidi control in %S" s t)
      [ "\u{202E}"; "\u{2067}"; "\u{200E}"; "\u{061C}" ];
    let again = Ansi.sanitize t in
    if again <> t then failf "%S: %S then %S" s t again
  done

(* A list of spans is one text: sanitized and cut into graphemes as a
   whole, each grapheme in the style of the span its first byte is in. *)
let spans_as_one_text () =
  let bold = Style.make ~bold:true () and red = Style.make ~fg:Color.red () in
  let same msg a b = check_seq msg (Ansi.render b) (Ansi.render a) in
  same "a ZWJ sequence split"
    [
      Ansi.span ~style:bold "\u{1F468}";
      Ansi.span ~style:red "\u{200D}\u{1F469}";
    ]
    [ Ansi.span ~style:bold "\u{1F468}\u{200D}\u{1F469}" ];
  same "a selector split"
    [ Ansi.span ~style:bold "\u{2764}"; Ansi.span ~style:red "\u{FE0F}" ]
    [ Ansi.span ~style:bold "\u{2764}\u{FE0F}" ];
  same "a CR, then its LF"
    [ Ansi.span "a\r"; Ansi.span "\nb" ]
    [ Ansi.span "a\nb" ];
  same "a combining mark split"
    [ Ansi.span ~style:bold "e"; Ansi.span ~style:red "\u{0301}x" ]
    [ Ansi.span ~style:bold "e\u{0301}"; Ansi.span ~style:red "x" ];
  same "control functions show as nothing"
    [
      Ansi.span ~style:red
        "a\x1b[2J\x1b]8;;http://evil\x1b\\\u{202E}b\x1b]8;;\x1b\\";
    ]
    [ Ansi.span ~style:red "ab" ];
  let rng = Random.State.make [| 7 |] in
  for _ = 1 to 20_000 do
    let spans =
      List.init (Random.State.int rng 5) (fun i ->
          Ansi.span
            ~style:(if i mod 2 = 0 then bold else red)
            (String.concat ""
               (List.init (Random.State.int rng 4) (fun _ ->
                    pieces.(Random.State.int rng (Array.length pieces))))))
    in
    let text spans =
      String.concat "" (List.map (fun (s : Ansi.span) -> s.text) spans)
    in
    let whole = Ansi.sanitize (text spans)
    and parts = Ansi.sanitize_spans spans in
    if text parts <> whole then
      failf "%S: %S as spans, %S whole" (text spans) (text parts) whole
  done

let parameter_clamping () =
  (* Cursor movement: negative -> 0 -> empty string *)
  check_seq "negative lines" "" Ansi.(to_string (cursor_up ~n:(-5)));
  check_seq "zero lines" "" Ansi.(to_string (cursor_up ~n:0));
  (* Absolute position: negative/zero -> 1 *)
  check_seq "clamp row/col" "\x1b[1;1H"
    Ansi.(to_string (cursor_position ~row:0 ~col:(-5)))

let explicit_error_handling () =
  try
    let w = Writer.create 16 in
    (set_scrolling_region ~top:10 ~bottom:5) w;
    fail "scrolling region validation failed"
  with Invalid_argument _ -> ()

let cursor_and_explicit_width_sequences () =
  check_seq "cursor color osc12" "\x1b]12;#010203\x07"
    Ansi.(to_string (cursor_color ~r:1 ~g:2 ~b:3));
  check_seq "explicit width osc66" "\x1b]66;w=5;hi\x1b\\"
    Ansi.(to_string (explicit_width ~width:5 ~text:"hi"))

let kitty_keyboard_sequences () =
  check_seq "kitty keyboard push" "\x1b[>5u"
    Ansi.(to_string (csi_u_push ~flags:5));
  check_seq "kitty keyboard pop" "\x1b[<u" Ansi.(to_string csi_u_pop)

let hyperlink_lifecycle () =
  let link_style = Style.hyperlink "http://foo" Style.default in
  let rendered = Ansi.render [ Ansi.span ~style:link_style "hi" ] in
  (* Minimal SGR: no SGR emitted when only link changes, just OSC 8 + text + end
     + reset *)
  check_seq "osc8 opened/closed with reset"
    "\x1b]8;;http://foo\x1b\\hi\x1b]8;;\x1b\\\x1b[0m" rendered;
  (* Test that hyperlinks are suppressed when disabled *)
  let rendered_no_links =
    Ansi.render ~hyperlinks_enabled:false [ Ansi.span ~style:link_style "hi" ]
  in
  (* No OSC8 sequences, just text + reset *)
  check_seq "osc8 suppressed when disabled" "hi\x1b[0m" rendered_no_links

(* --- 7. Self Round-Trip Tests --- *)

(* Verify parser recognizes sequences the library emits *)
let roundtrip_cursor_sequences () =
  let test_cursor name seq expected =
    check_tokens ~msg:name (to_string seq) expected
  in
  test_cursor "cursor up" (cursor_up ~n:5) "CSI 5 A";
  test_cursor "cursor down" (cursor_down ~n:3) "CSI 3 B";
  test_cursor "cursor forward" (cursor_forward ~n:10) "CSI 10 C";
  test_cursor "cursor back" (cursor_back ~n:2) "CSI 2 D";
  test_cursor "cursor position" (cursor_position ~row:10 ~col:20) "CSI 10;20 H";
  test_cursor "cursor next line" (cursor_next_line ~n:3) "CSI 3 E";
  test_cursor "cursor prev line" (cursor_previous_line ~n:2) "CSI 2 F";
  test_cursor "cursor horiz abs" (cursor_horizontal_absolute 15) "CSI 15 G";
  test_cursor "cursor vert abs" (cursor_vertical_absolute 8) "CSI 8 d";
  (* Test cursor save/restore (CSI s/u) *)
  test_cursor "cursor save" cursor_save "CSI s";
  test_cursor "cursor restore" cursor_restore "CSI u"

let roundtrip_screen_sequences () =
  let test_screen name seq expected =
    check_tokens ~msg:name (to_string seq) expected
  in
  test_screen "erase display 0" (erase_display ~mode:`Below) "CSI 0 J";
  test_screen "erase display 2" (erase_display ~mode:`All) "CSI 2 J";
  test_screen "erase line 0" (erase_line ~mode:`Right) "CSI 0 K";
  test_screen "erase line 2" (erase_line ~mode:`All) "CSI 2 K";
  test_screen "insert lines" (insert_lines ~n:5) "CSI 5 L";
  test_screen "delete lines" (delete_lines ~n:3) "CSI 3 M";
  test_screen "scroll up" (scroll_up ~n:4) "CSI 4 S";
  test_screen "scroll down" (scroll_down ~n:2) "CSI 2 T"

let roundtrip_sgr_sequences () =
  (* Test that SGR sequences round-trip through parser *)
  let test_sgr name seq expected =
    equal ~msg:name check_style expected
      (read_sgr ~from:(Style.make ~italic:true ()) (to_string seq))
  in
  test_sgr "sgr reset" reset Style.default;
  test_sgr "sgr bold" (sgr [ 1 ]) (Style.make ~italic:true ~bold:true ());
  test_sgr "sgr truecolor fg"
    (sgr [ 38; 2; 100; 150; 200 ])
    (Style.make ~italic:true ~fg:(Color.of_rgb 100 150 200) ());
  (* Every style the writer emits reads back as itself. *)
  List.iter
    (fun style ->
      equal
        ~msg:(Format.asprintf "%a" Style.pp style)
        check_style style
        (read_sgr (Style.sgr_sequence style)))
    [
      Style.make ~bold:true ~dim:true ~italic:true ~underline:true ();
      Style.make ~blink:true ~inverse:true ~hidden:true ~strikethrough:true ();
      Style.make ~overline:true ~double_underline:true ~framed:true ();
      Style.make ~encircled:true ~fg:(Color.indexed 200) ~bg:Color.bright_red ();
      Style.make ~fg:(Color.of_rgb 1 2 3) ~bg:(Color.of_rgb 4 5 6) ();
    ]

let roundtrip_hyperlink_sequences () =
  check_tokens ~msg:"hyperlink open"
    (to_string (hyperlink_start ~url:"http://test.com"))
    "OSC, \"8;;http://test.com\", ST";
  check_tokens ~msg:"hyperlink close" (to_string hyperlink_end)
    "OSC, \"8;;\", ST"

let roundtrip_strip_removes_all () =
  (* Verify strip removes all sequences we emit *)
  let test_strip name input expected =
    let stripped = Ansi.strip input in
    equal ~msg:name string expected stripped
  in
  let s = Ansi.to_string in
  (* Cursor sequences *)
  test_strip "strip cursor up" (s (cursor_up ~n:5) ^ "text") "text";
  test_strip "strip cursor position"
    (s (cursor_position ~row:10 ~col:20) ^ "text")
    "text";
  (* SGR sequences *)
  test_strip "strip sgr"
    (Style.styled (Style.make ~fg:Color.red ~bold:true ()) "text")
    "text";
  test_strip "strip reset" (s reset ^ "text") "text";
  (* Screen control *)
  test_strip "strip clear" (s clear ^ "text") "text";
  test_strip "strip erase display"
    (s (erase_display ~mode:`All) ^ "text")
    "text";
  (* Hyperlinks *)
  test_strip "strip hyperlink"
    (s (hyperlink ~url:"http://test" ~text:"link"))
    "link";
  (* Title *)
  test_strip "strip title" (s (set_title ~title:"test") ^ "text") "text";
  (* Alternate screen *)
  test_strip "strip alt screen"
    (s (enable Alternate_screen) ^ "text" ^ s (disable Alternate_screen))
    "text";
  (* Mouse modes *)
  test_strip "strip mouse on"
    (s (enable Mouse_button_tracking)
    ^ "text"
    ^ s (disable Mouse_button_tracking))
    "text"

(* --- 8. Styling --- *)

let styling_t =
  Testable.make
    ~equal:(fun a b -> a = b)
    ~pp:(fun ppf s ->
      Format.pp_print_string ppf
        (match (s : Styling.t) with
        | `Plain -> "plain"
        | `Mono -> "mono"
        | `Ansi8 -> "8"
        | `Ansi16 -> "16"
        | `Ansi256 -> "256"
        | `Truecolor -> "24-bit"))

(* The terminals of the rows: whether it is dumb, and the colors it shows. *)
let t256 = (false, `Ansi256)
let t16 = (false, `Ansi16)
let t24 = (false, `Truecolor)
let dumb = (true, `Ansi16)

(* Each row: the program's choice, a terminal or not, the terminal, the
   environment, and the styling. *)
let styling_rows =
  [
    ("a 256-color terminal", None, true, t256, [], `Ansi256);
    ("a 16-color terminal", None, true, t16, [], `Ansi16);
    ("24-bit", None, true, t24, [], `Truecolor);
    ("a pipe", None, false, t256, [], `Plain);
    ( "NO_COLOR keeps the attributes",
      None,
      true,
      t256,
      [ ("NO_COLOR", "1") ],
      `Mono );
    ("NO_COLOR on a pipe", None, false, t256, [ ("NO_COLOR", "1") ], `Plain);
    ( "an empty NO_COLOR is unset",
      None,
      true,
      t256,
      [ ("NO_COLOR", "") ],
      `Ansi256 );
    ( "FORCE_COLOR into a pipe",
      None,
      false,
      t256,
      [ ("FORCE_COLOR", "1") ],
      `Ansi256 );
    ( "FORCE_COLOR of any other value",
      None,
      false,
      t256,
      [ ("FORCE_COLOR", "3") ],
      `Ansi256 );
    ( "FORCE_COLOR=0 forces nothing",
      None,
      false,
      t256,
      [ ("FORCE_COLOR", "0") ],
      `Plain );
    ( "FORCE_COLOR=false forces nothing",
      None,
      false,
      t256,
      [ ("FORCE_COLOR", "false") ],
      `Plain );
    ( "CLICOLOR_FORCE into a pipe",
      None,
      false,
      t256,
      [ ("CLICOLOR_FORCE", "1") ],
      `Ansi256 );
    ( "NO_COLOR above a force",
      None,
      false,
      t256,
      [ ("NO_COLOR", "1"); ("FORCE_COLOR", "1") ],
      `Mono );
    ("CLICOLOR=0", None, true, t256, [ ("CLICOLOR", "0") ], `Mono);
    ("CLICOLOR=1 adds nothing", None, false, t256, [ ("CLICOLOR", "1") ], `Plain);
    ( "a force above CLICOLOR=0",
      None,
      true,
      t256,
      [ ("CLICOLOR", "0"); ("FORCE_COLOR", "1") ],
      `Ansi256 );
    ("a dumb terminal", None, true, dumb, [], `Plain);
    ( "forced on a dumb terminal",
      None,
      true,
      dumb,
      [ ("FORCE_COLOR", "1") ],
      `Ansi16 );
    ("CI keeps the colors", None, true, t256, [ ("CI", "true") ], `Ansi256);
    ("--color=never", Some `Never, true, t256, [], `Plain);
    ("--color=always into a pipe", Some `Always, false, t256, [], `Ansi256);
    ( "--color=always above NO_COLOR",
      Some `Always,
      true,
      t256,
      [ ("NO_COLOR", "1") ],
      `Ansi256 );
    ( "--color=never above a force",
      Some `Never,
      true,
      t256,
      [ ("FORCE_COLOR", "1") ],
      `Plain );
  ]

let styling_tests =
  List.map
    (fun (name, color, tty, (dumb, colors), vars, expected) ->
      test name (fun () ->
          let getenv v = List.assoc_opt v vars in
          equal ~msg:"styling" styling_t expected
            (Styling.decide ?color getenv ~tty ~dumb colors)))
    styling_rows
  @ [
      test "choices" (fun () ->
          equal ~msg:"names" (list string)
            [ "auto"; "always"; "never" ]
            (List.map fst Styling.choices));
      (* A terminal's capabilities give fewer depths than Color.depth has,
         and pass them as they are. *)
      test "a narrower type of depths" (fun () ->
          let colors : [ `Ansi16 | `Ansi256 | `Truecolor ] = `Ansi256 in
          equal ~msg:"styling" styling_t `Ansi256
            (Styling.decide (fun _ -> None) ~tty:true ~dumb:false colors));
    ]

let tests =
  [
    group "Palettes"
      [
        test "contrast as a palette shows colors" palette_contrast;
        test "legible entries" palette_legible;
        prop "legible on any palette"
          (Gen.pair gen_palette (Gen.int_range 0 15))
          palette_legible_laws;
        test "entries" palette_entry;
        test "equality" palette_equal;
        test "printing" palette_pp;
      ];
    group "Color Logic"
      [
        test "hex parsing" color_parsing_hex;
        test "hsl roundtrip" color_hsl_roundtrip;
        test "blending" color_blending;
        test "contrast" color_contrast;
        test "intent identity" color_intent_is_part_of_identity;
        test "ansi16 fallback palette"
          color_ansi16_palette_matches_standard_fallback;
        test "extended out of range safe" color_extended_out_of_range_is_safe;
        test "low indexed colors use extended palette SGR"
          low_indexed_colors_use_extended_palette_sgr;
      ];
    group "Attributes & Style"
      [
        test "bitmask integrity" attr_bitmask_integrity;
        test "style composition" style_composition_and_hash;
        test "extended attr ordering" attr_extended_ordering;
        test "shared disable codes" style_shared_disable_codes;
        test "emit function" style_emit_function;
        test "resolve multiple" style_resolve_multiple;
        test "compare and hash" style_compare_and_hash;
      ];
    group "SGR Engine"
      [
        test "the transition ignores links" style_transition_ignores_links;
        test "a style is written at a depth" style_writes_at_a_depth;
        test "state diffing" sgr_state_transitions;
        test "foreground-only change does not reset"
          sgr_foreground_only_change_does_not_reset;
        test "background-only change does not reset"
          sgr_background_only_change_does_not_reset;
        test "shared attr disable reenables survivor"
          sgr_shared_attr_disable_reenables_survivor;
        test "all shared attribute transitions" sgr_shared_attribute_transitions;
        test "enclosure shared disable group" sgr_enclosure_disable_group;
        test "same rgba different intent emits color delta"
          sgr_same_rgba_different_intent_emits_color_delta;
        test "transparent bg reset" sgr_transparent_bg_resets;
        test "transparent fg reset" sgr_transparent_fg_resets_to_default;
        test "rgb downgrade to ansi256" sgr_rgb_downgrades_to_ansi256;
        test "rgb downgrade to ansi16" sgr_rgb_downgrades_to_ansi16;
        test "indexed preserved without truecolor"
          sgr_indexed_stays_indexed_without_truecolor;
        test "rgb to ansi8 and mono" sgr_rgb_to_ansi8_and_mono;
        test "to_depth" color_to_depth;
        test "to_depth 256 matches xterm's search"
          color_to_depth_256_matches_xterm_search;
      ];
    group "Parser"
      [
        test "chunked utf8" parser_chunked_utf8;
        test "chunked utf8 same buffer slice"
          parser_chunked_utf8_same_buffer_slice;
        test "chunked escape" parser_chunked_escape;
        test "controls inside csi" parser_controls_inside_csi;
        test "malformed inputs" parser_malformed_input;
        test "invalid utf8 lead bytes" parser_invalid_utf8_lead_bytes;
        test "overflow protection" parser_max_length_overflow;
        test "osc8 st terminated" parser_osc8_st_terminated;
        test "osc8 bel terminated" parser_osc8_bel_terminated;
        test "protocol string controls" parser_protocol_string_controls;
        test "chunked protocol string controls"
          parser_protocol_string_controls_chunked;
        test "esc ends string controls" parser_esc_ends_string_controls;
        test "non-ascii ends csi" parser_non_ascii_ends_csi;
        test "csi empty parameter defaults" parser_csi_empty_parameter_defaults;
        test "utf8 split invariance" parser_utf8_split_invariance;
        test "c1 controls" parser_c1_controls;
        test "finish" parser_finish;
        test "at_boundary" parser_at_boundary;
        test "charset selection split" parser_charset_selection_split;
        test "unknown escape sequences" parser_unknown_escape_sequences;
        test "sgr colon subparameters" parser_sgr_colon_subparameters;
        test "mode sequences" parser_mode_sequences;
        test "csi private and intermediates"
          parser_csi_private_and_intermediates;
        test "esc, can and sub end everything" parser_esc_can_sub_end_everything;
        test "c0 inside sequences and strings" parser_c0_inside_sequences;
        test "del" parser_del;
        test "malformed sequences" parser_malformed_sequences;
        test "non-ascii in sequences" parser_non_ascii_in_sequences;
        test "utf8" parser_utf8;
        test "string terminators and c1" parser_string_terminators;
        test "params" parser_params;
        test "feed contract" parser_feed_contract;
        test "stop" parser_stop;
        test "split invariance" parser_split_invariance;
        test "known defects" parser_known_defects;
        test "pp_token" parser_pp_token;
        test "apply_sgr" style_apply_sgr;
      ];
    group "Writer & Seq"
      [
        test "utf8 encoding" writer_utf8_encoding;
        test "hyperlink params" writer_hyperlink_params;
        test "clipboard base64 payload" clipboard_base64_payload;
        test "notify in either form" notify_osc9_and_osc777;
        test "OSC text is printable" osc_text_is_printable;
        test "tmux passthrough doubles ESC" tmux_passthrough_doubles_esc;
        test "color scheme report query" color_scheme_report_query;
        test "color queries" color_queries;
        test "invalid scalar replacement" writer_invalid_scalar_replacement;
        test "growth preserves output" writer_growth_preserves_output;
        test "checked bounds" writer_checked_bounds;
        test "of_bytes" writer_of_bytes;
        test "to_string calls emitter once" to_string_calls_emitter_once;
      ];
    group "High Level API"
      [
        test "strip semantics" strip_edge_cases;
        test "strip protocol string controls" strip_protocol_string_controls;
        test "strip keeps C0 and follows the parser"
          strip_keeps_c0_and_follows_the_parser;
        test "sanitize untrusted text" sanitize_untrusted_text;
        test "sanitize properties" sanitize_properties;
        test "spans as one text" spans_as_one_text;
        test "param clamping" parameter_clamping;
        test "input validation" explicit_error_handling;
        test "cursor & explicit width" cursor_and_explicit_width_sequences;
        test "kitty keyboard sequences" kitty_keyboard_sequences;
        test "hyperlink lifecycle" hyperlink_lifecycle;
      ];
    group "Round-Trip"
      [
        test "cursor sequences" roundtrip_cursor_sequences;
        test "screen sequences" roundtrip_screen_sequences;
        test "sgr sequences" roundtrip_sgr_sequences;
        test "hyperlink sequences" roundtrip_hyperlink_sequences;
        test "strip removes all" roundtrip_strip_removes_all;
      ];
    group "Styling" styling_tests;
  ]

let () = run "matrix.ansi" tests
