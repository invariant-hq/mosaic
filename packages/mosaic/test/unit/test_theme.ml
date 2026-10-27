(* Themes and their scope: the theme in scope at a node is the renderer's
   with the function of each [themed] (or [Renderable.set_theme]) from the
   root down applied in turn, read when the node is drawn. *)

open Windtrap
open Mosaic_ui

let theme =
  Testable.make
    ~pp:(fun ppf _ -> Format.pp_print_string ppf "<theme>")
    ~equal:Theme.equal

let red = Ansi.Style.make ~fg:(Ansi.Color.indexed 1) ()
let blue = Ansi.Style.make ~fg:(Ansi.Color.indexed 4) ()
let ascii = Theme.with_glyphs Glyphs.ascii
let error_red = Theme.with_style Error red
let error_blue = Theme.with_style Error blue

(* ───── Values ───── *)

let plain_surfaces () =
  let surface s = Theme.style Theme.plain s in
  equal ~msg:"canvas" bool true
    (Ansi.Style.equal (surface Canvas) Ansi.Style.default);
  equal ~msg:"cursor" bool true
    (Ansi.Style.equal (surface Cursor) (Ansi.Style.make ~inverse:true ()));
  equal ~msg:"selected" bool true
    (Ansi.Style.equal (surface Selected)
       (Ansi.Style.make ~bold:true ~underline:true ()));
  equal ~msg:"glyphs" bool true
    (Glyphs.equal (Theme.glyphs Theme.plain) Glyphs.unicode)

let surfaces : Theme.role list =
  [ Canvas; Panel; Field; Cursor; Selected; Highlight ]

let default_surfaces () =
  let colors =
    {
      Ansi.Palette.foreground = (0, 0, 0);
      background = (255, 255, 255);
      cursor = None;
      entries = Array.init 16 (fun i -> Some (i * 16, 0, 0));
    }
  in
  let t = Theme.default colors in
  List.iter
    (fun s ->
      equal ~msg:"surfaces as plain's" bool true
        (Ansi.Style.equal (Theme.style t s) (Theme.style Theme.plain s)))
    surfaces

let modifiers () =
  let t = Theme.with_style Cursor red Theme.plain in
  equal ~msg:"the surface" bool true
    (Ansi.Style.equal (Theme.style t Cursor) red);
  equal ~msg:"the others kept" bool true
    (Ansi.Style.equal (Theme.style t Highlight)
       (Theme.style Theme.plain Highlight));
  equal ~msg:"plain unchanged" bool false (Theme.equal t Theme.plain);
  let t = error_red Theme.plain in
  equal ~msg:"the role's text" bool true
    (Ansi.Style.equal (Theme.style t Error) red);
  equal ~msg:"the role's glyphs" bool true
    (Ansi.Style.equal (Theme.mark ~alone:true t Error) red)

(* ───── Role tables ───── *)

let check_style = Testable.make ~pp:Ansi.Style.pp ~equal:Ansi.Style.equal

(* The colors of 27 configurations, each with what Theme.default draws.
   Eight were measured by querying the terminal (OSC 10, 11 and 4) with an
   empty configuration and with only its background set white; Solarized
   comes from its specification; the others from their sources (st
   config.def.h, foot foot.ini, Alacritty color.rs, Ghostty Config.zig,
   Windows Terminal defaults.json, Konsole Breeze.colorscheme, GNOME
   Terminal's gschema, and each theme's own). Each gives the default
   foreground and background, entries 0 to 15 ("" where not read), and the
   entry Theme.default gives Link, Added, Removed and Muted text and a state
   glyph alone of Error, Ok, Warning, Link and Accent ("-" for none). *)
let palettes =
  [
    ( "xterm, black on white (its default)",
      "000000",
      "ffffff",
      [
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
      ],
      "link 4 added - removed 1 muted -, alone 1 - - 4 -" );
    ( "xterm, XTerm-color",
      "e5e5e5",
      "000000",
      [
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
      ],
      "link - added 2 removed 9 muted 8, alone 1 2 3 12 6" );
    ( "VTE",
      "c0c0c0",
      "000000",
      [
        "000000";
        "c00000";
        "00c000";
        "c0c000";
        "0000c0";
        "c000c0";
        "00c0c0";
        "c0c0c0";
        "3f3f3f";
        "ff3f3f";
        "3fff3f";
        "ffff3f";
        "3f3fff";
        "ff3fff";
        "3fffff";
        "ffffff";
      ],
      "link - added 2 removed 9 muted -, alone 1 2 3 12 6" );
    ( "VTE, white background",
      "000000",
      "ffffff",
      [
        "000000";
        "c00000";
        "00c000";
        "c0c000";
        "0000c0";
        "c000c0";
        "00c0c0";
        "c0c0c0";
        "3f3f3f";
        "ff3f3f";
        "3fff3f";
        "ffff3f";
        "3f3fff";
        "ff3fff";
        "3fffff";
        "ffffff";
      ],
      "link 4 added - removed 1 muted 8, alone 1 - - 4 -" );
    ( "kitty",
      "dddddd",
      "000000",
      [
        "000000";
        "cc0403";
        "19cb00";
        "cecb00";
        "0d73cc";
        "cb1ed1";
        "0dcdcd";
        "dddddd";
        "767676";
        "f2201f";
        "23fd00";
        "fffd00";
        "1a8fff";
        "fd28ff";
        "14ffff";
        "ffffff";
      ],
      "link 12 added 2 removed 9 muted 8, alone 1 2 3 4 6" );
    ( "kitty, white background",
      "000000",
      "ffffff",
      [
        "000000";
        "cc0403";
        "19cb00";
        "cecb00";
        "0d73cc";
        "cb1ed1";
        "0dcdcd";
        "dddddd";
        "767676";
        "f2201f";
        "23fd00";
        "fffd00";
        "1a8fff";
        "fd28ff";
        "14ffff";
        "ffffff";
      ],
      "link 4 added - removed 1 muted 8, alone 1 - - 4 -" );
    ( "WezTerm",
      "b2b2b2",
      "000000",
      [
        "000000";
        "cc5555";
        "55cc55";
        "cdcd55";
        "5455cb";
        "cc55cc";
        "7acaca";
        "cccccc";
        "555555";
        "ff5555";
        "55ff55";
        "ffff55";
        "5555ff";
        "ff55ff";
        "55ffff";
        "ffffff";
      ],
      "link - added 2 removed 1 muted -, alone 1 2 3 4 6" );
    ( "WezTerm, white background",
      "000000",
      "ffffff",
      [
        "000000";
        "cc5555";
        "55cc55";
        "cdcd55";
        "5455cb";
        "cc55cc";
        "7acaca";
        "cccccc";
        "555555";
        "ff5555";
        "55ff55";
        "ffff55";
        "5555ff";
        "ff55ff";
        "55ffff";
        "ffffff";
      ],
      "link 4 added - removed - muted 8, alone 1 - - 4 -" );
    ( "Solarized dark",
      "839496",
      "002b36",
      [
        "073642";
        "dc322f";
        "859900";
        "b58900";
        "268bd2";
        "d33682";
        "2aa198";
        "eee8d5";
        "002b36";
        "cb4b16";
        "586e75";
        "657b83";
        "839496";
        "6c71c4";
        "93a1a1";
        "fdf6e3";
      ],
      "link 12 added 2 removed - muted -, alone 1 2 3 4 6" );
    ( "Solarized light",
      "657b83",
      "fdf6e3",
      [
        "073642";
        "dc322f";
        "859900";
        "b58900";
        "268bd2";
        "d33682";
        "2aa198";
        "eee8d5";
        "002b36";
        "cb4b16";
        "586e75";
        "657b83";
        "839496";
        "6c71c4";
        "93a1a1";
        "fdf6e3";
      ],
      "link - added 10 removed 1 muted -, alone 1 10 11 4 -" );
    ( "st 0.9 (config.def.h)",
      "e5e5e5",
      "000000",
      [
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
        "";
      ],
      "link - added 2 removed 9 muted 8, alone 1 2 3 12 6" );
    ( "foot master (foot.ini)",
      "ffffff",
      "242424",
      [
        "242424";
        "f62b5a";
        "47b413";
        "e3c401";
        "24acd4";
        "f2affd";
        "13c299";
        "e6e6e6";
        "616161";
        "ff4d51";
        "35d450";
        "e9e836";
        "5dc5f8";
        "feabf2";
        "24dfc4";
        "";
      ],
      "link 4 added 2 removed 9 muted -, alone 1 2 3 4 6" );
    ( "Alacritty (color.rs)",
      "d8d8d8",
      "181818",
      [
        "181818";
        "ac4242";
        "90a959";
        "f4bf75";
        "6a9fb5";
        "aa759f";
        "75b5aa";
        "d8d8d8";
        "6b6b6b";
        "c55555";
        "aac474";
        "feca88";
        "82b8c8";
        "c28cb8";
        "93d3c3";
        "";
      ],
      "link 4 added 2 removed - muted -, alone 1 2 3 4 6" );
    ( "Ghostty (Config.zig, color.zig)",
      "ffffff",
      "282c34",
      [
        "1d1f21";
        "cc6666";
        "b5bd68";
        "f0c674";
        "81a2be";
        "b294bb";
        "8abeb7";
        "c5c8c6";
        "666666";
        "d54e53";
        "b9ca4a";
        "e7c547";
        "7aa6da";
        "c397d8";
        "70c0b1";
        "";
      ],
      "link 4 added 2 removed - muted -, alone 1 2 3 4 6" );
    ( "Windows Terminal Campbell",
      "cccccc",
      "0c0c0c",
      [
        "0c0c0c";
        "c50f1f";
        "13a10e";
        "c19c00";
        "0037da";
        "881798";
        "3a96dd";
        "cccccc";
        "767676";
        "e74856";
        "16c60c";
        "f9f1a5";
        "3b78ff";
        "b4009e";
        "61d6d6";
        "";
      ],
      "link 12 added 2 removed 9 muted -, alone 1 2 3 12 6" );
    ( "Windows Terminal One Half Light",
      "383a42",
      "fafafa",
      [
        "383a42";
        "e45649";
        "50a14f";
        "c18301";
        "0184bc";
        "a626a4";
        "0997b3";
        "fafafa";
        "4f525d";
        "df6c75";
        "98c379";
        "e4c07a";
        "61afef";
        "c577dd";
        "56b5c1";
        "";
      ],
      "link - added - removed - muted 8, alone 1 2 3 4 6" );
    ( "Konsole Breeze",
      "fcfcfc",
      "232627",
      [
        "232627";
        "ed1515";
        "11d116";
        "f67400";
        "1d99f3";
        "9b59b6";
        "1abc9c";
        "fcfcfc";
        "7f8c8d";
        "c0392b";
        "1cdc9a";
        "fdbc4b";
        "3daee9";
        "8e44ad";
        "16a085";
        "";
      ],
      "link 4 added 2 removed - muted -, alone 1 2 3 4 6" );
    ( "GNOME Terminal schema (light)",
      "1e1e1e",
      "ffffff",
      [
        "1e1e1e";
        "c01c28";
        "26a269";
        "a2734c";
        "12488b";
        "a347ba";
        "2aa1b3";
        "cfcfcf";
        "5d5d5d";
        "f66151";
        "33d17a";
        "e9ad0c";
        "2a7bde";
        "c061cb";
        "33c7de";
        "";
      ],
      "link 4 added - removed 1 muted 8, alone 1 2 3 4 6" );
    ( "Dracula",
      "f8f8f2",
      "282a36",
      [
        "21222c";
        "ff5555";
        "50fa7b";
        "f1fa8c";
        "bd93f9";
        "ff79c6";
        "8be9fd";
        "f8f8f2";
        "6272a4";
        "ff6e6e";
        "69ff94";
        "ffffa5";
        "d6acff";
        "ff92df";
        "a4ffff";
        "";
      ],
      "link 4 added 2 removed 1 muted -, alone 1 2 3 4 6" );
    ( "Gruvbox dark",
      "ebdbb2",
      "282828",
      [
        "282828";
        "cc241d";
        "98971a";
        "d79921";
        "458588";
        "b16286";
        "689d6a";
        "a89984";
        "928374";
        "fb4934";
        "b8bb26";
        "fabd2f";
        "83a598";
        "d3869b";
        "8ec07c";
        "";
      ],
      "link 12 added 2 removed - muted -, alone 9 2 3 4 6" );
    ( "Gruvbox light",
      "3c3836",
      "fbf1c7",
      [
        "fbf1c7";
        "cc241d";
        "98971a";
        "d79921";
        "458588";
        "b16286";
        "689d6a";
        "7c6f64";
        "928374";
        "9d0006";
        "79740e";
        "b57614";
        "076678";
        "8f3f71";
        "427b58";
        "";
      ],
      "link 12 added - removed 1 muted -, alone 1 10 11 4 14" );
    ( "Nord",
      "d8dee9",
      "2e3440",
      [
        "3b4252";
        "bf616a";
        "a3be8c";
        "ebcb8b";
        "81a1c1";
        "b48ead";
        "88c0d0";
        "e5e9f0";
        "4c566a";
        "bf616a";
        "a3be8c";
        "ebcb8b";
        "81a1c1";
        "b48ead";
        "8fbcbb";
        "";
      ],
      "link 4 added 2 removed - muted -, alone 1 2 3 4 6" );
    ( "One Dark",
      "abb2bf",
      "282c34",
      [
        "1e2127";
        "e06c75";
        "98c379";
        "d19a66";
        "61afef";
        "c678dd";
        "56b6c2";
        "abb2bf";
        "5c6370";
        "e06c75";
        "98c379";
        "d19a66";
        "61afef";
        "c678dd";
        "56b6c2";
        "";
      ],
      "link 4 added 2 removed - muted -, alone 1 2 3 4 6" );
    ( "Tokyo Night",
      "c0caf5",
      "1a1b26",
      [
        "15161e";
        "f7768e";
        "9ece6a";
        "e0af68";
        "7aa2f7";
        "bb9af7";
        "7dcfff";
        "a9b1d6";
        "414868";
        "ff899d";
        "9fe044";
        "faba4a";
        "8db0ff";
        "c7a9ff";
        "a4daff";
        "";
      ],
      "link 4 added 2 removed 1 muted -, alone 1 2 3 4 6" );
    ( "Catppuccin Mocha",
      "cdd6f4",
      "1e1e2e",
      [
        "45475a";
        "f38ba8";
        "a6e3a1";
        "f9e2af";
        "89b4fa";
        "f5c2e7";
        "94e2d5";
        "bac2de";
        "585b70";
        "";
        "";
        "";
        "";
        "";
        "";
        "";
      ],
      "link 4 added 2 removed 1 muted -, alone 1 2 3 4 6" );
    ( "Catppuccin Latte",
      "4c4f69",
      "eff1f5",
      [
        "5c5f77";
        "d20f39";
        "40a02b";
        "df8e1d";
        "1e66f5";
        "ea76cb";
        "179299";
        "acb0be";
        "6c6f85";
        "";
        "";
        "";
        "";
        "";
        "";
        "";
      ],
      "link - added - removed 1 muted -, alone 1 - - 4 6" );
    ( "GitHub light",
      "24292f",
      "ffffff",
      [
        "24292e";
        "d73a49";
        "28a745";
        "dbab09";
        "0366d6";
        "5a32a3";
        "0598bc";
        "6a737d";
        "959da5";
        "cb2431";
        "22863a";
        "b08800";
        "005cc5";
        "5a32a3";
        "3192aa";
        "";
      ],
      "link 4 added 10 removed 1 muted -, alone 1 2 11 4 6" );
  ]

let hex h =
  let byte i = int_of_string ("0x" ^ String.sub h i 2) in
  (byte 0, byte 2, byte 4)

let colors_of fg bg entries =
  {
    Ansi.Palette.foreground = hex fg;
    background = hex bg;
    cursor = None;
    entries =
      Array.of_list
        (List.map (function "" -> None | h -> Some (hex h)) entries);
  }

let role_name : Theme.role -> string = function
  | Plain -> "Plain"
  | Muted -> "Muted"
  | Emphasis -> "Emphasis"
  | Accent -> "Accent"
  | Link -> "Link"
  | Ok -> "Ok"
  | Warning -> "Warning"
  | Error -> "Error"
  | Added -> "Added"
  | Removed -> "Removed"
  | Border -> "Border"
  | Canvas -> "Canvas"
  | Panel -> "Panel"
  | Field -> "Field"
  | Cursor -> "Cursor"
  | Selected -> "Selected"
  | Highlight -> "Highlight"

let check_result =
  list
    (pair
       (Testable.make
          ~pp:(fun ppf r -> Format.pp_print_string ppf (role_name r))
          ~equal:( = ))
       (float 0.005))

let entry (s : Ansi.Style.t) =
  match Option.map Ansi.Color.intent s.fg with
  | None -> "-"
  | Some (Indexed i) -> string_of_int i
  | Some _ -> "?"

let summary t =
  let alone r = entry (Theme.mark ~alone:true t r) in
  Printf.sprintf "link %s added %s removed %s muted %s, alone %s"
    (entry (Theme.style t Link))
    (entry (Theme.style t Added))
    (entry (Theme.style t Removed))
    (entry (Theme.style t Muted))
    (String.concat " " (List.map alone [ Error; Ok; Warning; Link; Accent ]))

let slot : Theme.role -> int option = function
  | Error | Removed -> Some 1
  | Ok | Added -> Some 2
  | Warning -> Some 3
  | Link -> Some 4
  | Accent -> Some 6
  | Plain | Muted | Emphasis | Border | Canvas | Panel | Field | Cursor
  | Selected | Highlight ->
      None

(* What every theme of [default] holds on a terminal of colors [c]: no
   text below its target, a state glyph a word repeats always in its
   role's entry, the other roles' text as [plain] draws it, and never bold
   with a palette entry or dim. *)
let theme_laws ~msg c =
  let t = Theme.default c in
  equal
    ~msg:(msg ^ ": no text below its target")
    check_result [] (Theme.check c t);
  List.iter
    (fun role ->
      let styles =
        [ Theme.style t role; Theme.mark t role; Theme.mark ~alone:true t role ]
      in
      List.iter
        (fun (s : Ansi.Style.t) ->
          is_false ~msg:(msg ^ ": no dim") (Ansi.Attr.mem Dim s.attrs);
          is_false
            ~msg:(msg ^ ": no bold with an entry")
            (Ansi.Attr.mem Bold s.attrs && s.fg <> None))
        styles;
      (match slot role with
      | Some s ->
          equal
            ~msg:(msg ^ ": a repeated glyph takes its entry")
            check_style
            (Ansi.Style.make ~fg:(Ansi.Color.indexed s) ())
            (Theme.mark t role)
      | None ->
          equal
            ~msg:(msg ^ ": a glyph without an entry is text")
            check_style (Theme.style t role) (Theme.mark t role));
      match role with
      | Link | Added | Removed | Muted -> ()
      | _ ->
          equal
            ~msg:(msg ^ ": text in the pair")
            check_style
            (Theme.style Theme.plain role)
            (Theme.style t role))
    Theme.roles

let theme_palettes () =
  List.iter
    (fun (name, fg, bg, entries, expected) ->
      let c = colors_of fg bg entries in
      theme_laws ~msg:name c;
      equal ~msg:name string expected (summary (Theme.default c)))
    palettes

(* Adjudicated classes: message text stays in the pair with its attribute;
   a glyph a word repeats takes its entry, reported or not; a glyph alone
   takes an entry that reaches 3:1, or the role's text; text whose hue is
   its content takes entry s, else s + 8, at the target. *)
let theme_classes () =
  let xterm_light =
    let _, fg, bg, entries, _ = List.hd palettes in
    colors_of fg bg entries
  in
  let vte =
    let _, fg, bg, entries, _ = List.nth palettes 2 in
    colors_of fg bg entries
  in
  let t = Theme.default xterm_light in
  let bold = Ansi.Style.make ~bold:true () in
  let fg i = Ansi.Style.make ~fg:(Ansi.Color.indexed i) () in
  equal ~msg:"error text" check_style bold (Theme.style t Error);
  equal ~msg:"warning text" check_style bold (Theme.style t Warning);
  equal ~msg:"ok text" check_style Ansi.Style.default (Theme.style t Ok);
  equal ~msg:"a repeated glyph, unreported" check_style (fg 1)
    (Theme.mark Theme.plain Error);
  equal ~msg:"a repeated glyph below 3:1" check_style (fg 3)
    (Theme.mark t Warning);
  equal ~msg:"a glyph alone, unreported" check_style bold
    (Theme.mark ~alone:true Theme.plain Error);
  equal ~msg:"a glyph alone at 5.84:1" check_style (fg 1)
    (Theme.mark ~alone:true t Error);
  equal ~msg:"a glyph alone below 3:1" check_style bold
    (Theme.mark ~alone:true t Warning);
  equal ~msg:"a link at 9.40:1" check_style
    (Ansi.Style.make ~fg:(Ansi.Color.indexed 4) ~underline:true ())
    (Theme.style t Link);
  equal ~msg:"added below 4.5:1 in both entries" check_style Ansi.Style.default
    (Theme.style t Added);
  equal ~msg:"removed in the bright entry" check_style (fg 9)
    (Theme.style (Theme.default vte) Removed);
  equal ~msg:"plain links are underlined" check_style
    (Ansi.Style.make ~underline:true ())
    (Theme.style Theme.plain Link)

let theme_with_style () =
  let red = Ansi.Style.make ~fg:(Ansi.Color.of_rgb 200 0 0) () in
  let t = Theme.with_style Error red Theme.plain in
  equal ~msg:"text" check_style red (Theme.style t Error);
  equal ~msg:"glyph" check_style red (Theme.mark t Error);
  equal ~msg:"glyph alone" check_style red (Theme.mark ~alone:true t Error);
  is_true ~msg:"equal" (Theme.equal Theme.plain Theme.plain);
  is_false ~msg:"changed" (Theme.equal Theme.plain t);
  let white =
    {
      Ansi.Palette.foreground = (0, 0, 0);
      background = (255, 255, 255);
      cursor = None;
      entries = Array.make 16 None;
    }
  in
  equal ~msg:"a gray below 4.5:1" check_result
    [ (Plain, 4.48) ]
    (Theme.check white
       (Theme.with_style Plain
          (Ansi.Style.make ~fg:(Ansi.Color.of_rgb 0x77 0x77 0x77) ())
          Theme.plain));
  equal ~msg:"a surface below 4.5:1" check_result
    [ (Field, 4.48) ]
    (Theme.check white
       (Theme.with_style Field
          (Ansi.Style.make
             ~bg:(Ansi.Color.of_rgb 0x77 0x77 0x77)
             ~fg:(Ansi.Color.of_rgb 255 255 255)
             ())
          Theme.plain));
  equal ~msg:"an unreported entry is left out" check_result []
    (Theme.check white
       (Theme.with_style Error
          (Ansi.Style.make ~fg:Ansi.Color.yellow ())
          Theme.plain))

let gen_rgb =
  Gen.triple (Gen.int_range 0 255) (Gen.int_range 0 255) (Gen.int_range 0 255)

let gen_palette =
  Gen.map
    (fun (foreground, background, entries) ->
      { Ansi.Palette.foreground; background; cursor = None; entries })
    (Gen.triple gen_rgb gen_rgb
       (Gen.array ~size:(Gen.int_range 0 16) (Gen.option gen_rgb)))
  |> Gen.with_pp Ansi.Palette.pp

(* ───── Scope ───── *)

let size w h =
  Toffee.Style.set_size (Mosaic.size ~width:w ~height:h) Toffee.Style.default

(* A canvas that records the theme it is drawn with, by name. *)
let seen = Hashtbl.create 8

let canvas name =
  Mosaic.canvas ~layout:(size 2 1) (fun c ~dt:_ ->
      Hashtbl.replace seen name (Canvas.theme c))

let mount () =
  Hashtbl.reset seen;
  let renderer = Renderer.create () in
  let reconciler =
    Mosaic.Reconciler.create ~container:(Renderer.root renderer)
  in
  let render view =
    Mosaic.Reconciler.render reconciler ~viewport_width:20 view;
    Renderer.render_frame renderer ~width:20 ~height:10 ~dt:0.
  in
  (renderer, render)

let saw name expected =
  match Hashtbl.find_opt seen name with
  | Some t -> equal ~msg:name theme expected t
  | None -> failf "%s was not drawn" name

let column children = Mosaic.box ~layout:(size 20 10) children

(* The nearest [themed] decides last; a sibling outside it keeps the outer
   scope, and an element outside every one the root's. *)
let nested () =
  let _, render = mount () in
  render
    (column
       [
         Mosaic.themed ascii
           (Mosaic.box ~layout:(size 20 5)
              [
                canvas "a";
                Mosaic.themed error_red (canvas "b");
                Mosaic.themed error_blue (Mosaic.themed error_red (canvas "c"));
              ]);
         canvas "d";
       ]);
  saw "a" (ascii Theme.plain);
  saw "b" (error_red (ascii Theme.plain));
  saw "c" (error_red (error_blue (ascii Theme.plain)));
  saw "d" Theme.plain

(* Themed fragments, and a themed child that switches with the viewport,
   give each of their nodes the theme. *)
let fragments () =
  let _, render = mount () in
  render
    (column
       [
         Mosaic.themed ascii
           (Mosaic.fragment
              [
                canvas "a";
                Mosaic.viewport_switch ~at_least_width:10 ~wide:(canvas "b")
                  ~narrow:Mosaic.empty;
              ]);
       ]);
  saw "a" (ascii Theme.plain);
  saw "b" (ascii Theme.plain)

(* The root's theme changes the scope of every node at the next frame,
   with no view reconciled again. *)
let root_theme () =
  let renderer, render = mount () in
  render (column [ Mosaic.themed ascii (canvas "a"); canvas "b" ]);
  let root = error_red Theme.plain in
  Renderer.set_theme renderer root;
  Hashtbl.reset seen;
  Renderer.render_frame renderer ~width:20 ~height:10 ~dt:0.;
  saw "a" (ascii root);
  saw "b" root

(* A view that drops a [themed] drops its theme, and one that adds one
   around a mounted element gives it. *)
let theme_follows_view () =
  let _, render = mount () in
  render (column [ Mosaic.themed ascii (canvas "a") ]);
  saw "a" (ascii Theme.plain);
  render (column [ canvas "a" ]);
  saw "a" Theme.plain;
  render (column [ Mosaic.themed error_red (canvas "a") ]);
  saw "a" (error_red Theme.plain)

(* An unchanged memo keeps its subtree, which still takes the theme its
   place declares now, and its view is not built again. *)
let memo_view_calls = ref 0

let memo_view () =
  incr memo_view_calls;
  Mosaic.themed error_red (canvas "m")

let memo () =
  memo_view_calls := 0;
  let _, render = mount () in
  render (column [ Mosaic.themed ascii (Mosaic.memo ~key:() memo_view) ]);
  saw "m" (error_red (ascii Theme.plain));
  render (column [ Mosaic.themed error_blue (Mosaic.memo ~key:() memo_view) ]);
  saw "m" (error_red (error_blue Theme.plain));
  render (column [ Mosaic.memo ~key:() memo_view ]);
  saw "m" (error_red Theme.plain);
  equal ~msg:"the view built once" int 1 !memo_view_calls

(* An embedded node has the theme of its place, and none outside every
   [themed]. *)
let embed () =
  let renderer, render = mount () in
  let node = Renderable.create ~parent:(Renderer.root renderer) () in
  render (column [ Mosaic.themed ascii (Mosaic.embed node) ]);
  equal ~msg:"themed" theme (ascii Theme.plain) (Renderable.theme node);
  render (column [ Mosaic.embed node ]);
  equal ~msg:"not themed" theme Theme.plain (Renderable.theme node)

(* An embedded node's own function applies after the scope of its place,
   which never replaces it. *)
let embed_own_theme () =
  let renderer, render = mount () in
  let node = Renderable.create ~parent:(Renderer.root renderer) () in
  Renderable.set_theme node (Some error_red);
  render
    (column
       [ Mosaic.themed (fun t -> ascii (error_blue t)) (Mosaic.embed node) ]);
  equal ~msg:"its own over its place's" theme
    (error_red (ascii (error_blue Theme.plain)))
    (Renderable.theme node);
  render (column [ Mosaic.embed node ]);
  equal ~msg:"its own alone" theme (error_red Theme.plain)
    (Renderable.theme node);
  Renderable.set_theme node None;
  render (column [ Mosaic.themed ascii (Mosaic.embed node) ]);
  equal ~msg:"its place's alone" theme (ascii Theme.plain)
    (Renderable.theme node)

(* A node's own function applies to its subtree, until removed. *)
let imperative () =
  let renderer = Renderer.create () in
  let parent = Renderable.create ~parent:(Renderer.root renderer) () in
  let child = Renderable.create ~parent () in
  Renderable.set_theme parent (Some ascii);
  equal ~msg:"the child" theme (ascii Theme.plain) (Renderable.theme child);
  Renderable.set_theme child (Some error_red);
  equal ~msg:"nearest last" theme
    (error_red (ascii Theme.plain))
    (Renderable.theme child);
  Renderable.set_theme parent None;
  equal ~msg:"removed" theme (error_red Theme.plain) (Renderable.theme child)

(* ───── Cost ───── *)

(* The words [f ()] allocates, after a first call that warms its caches,
   less what reading the counters allocates. *)
let words f =
  let count f =
    ignore (f ());
    let minor, promoted, major = Gc.counters () in
    ignore (f ());
    let minor', promoted', major' = Gc.counters () in
    Float.to_int
      (minor' -. minor +. (major' -. major) -. (promoted' -. promoted))
  in
  count f - count (fun () -> ())

(* Reading the scope allocates nothing, without a theme of a node's own
   and with one applied already to the theme around it. *)
let read_allocates_nothing () =
  let renderer = Renderer.create () in
  let rec chain parent n =
    if n = 0 then parent else chain (Renderable.create ~parent ()) (n - 1)
  in
  let deep = chain (Renderer.root renderer) 30 in
  equal ~msg:"no theme" int 0 (words (fun () -> Renderable.theme deep));
  let middle = chain (Renderer.root renderer) 10 in
  Renderable.set_theme middle (Some ascii);
  let below = chain middle 20 in
  equal ~msg:"a theme applied" int 0 (words (fun () -> Renderable.theme below))

(* ───── Default styles ───── *)

(* The default styles of syntax, Markdown and diffs come from the roles of
   the theme in scope: on a terminal of a known palette, under its
   Theme.default, no text they draw is below its contrast. The colors are
   xterm's two defaults, VTE's default and kitty on white. *)

let palette hex =
  List.map
    (fun h ->
      Some
        ( int_of_string ("0x" ^ String.sub h 0 2),
          int_of_string ("0x" ^ String.sub h 2 2),
          int_of_string ("0x" ^ String.sub h 4 2) ))
    hex
  |> Array.of_list

let vte : Ansi.Palette.t =
  {
    foreground = (0xc0, 0xc0, 0xc0);
    background = (0, 0, 0);
    cursor = None;
    entries =
      palette
        [
          "000000";
          "c00000";
          "00c000";
          "c0c000";
          "0000c0";
          "c000c0";
          "00c0c0";
          "c0c0c0";
          "3f3f3f";
          "ff3f3f";
          "3fff3f";
          "ffff3f";
          "3f3fff";
          "ff3fff";
          "3fffff";
          "ffffff";
        ];
  }

let kitty_white : Ansi.Palette.t =
  {
    foreground = (0, 0, 0);
    background = (255, 255, 255);
    cursor = None;
    entries =
      palette
        [
          "000000";
          "cc0403";
          "19cb00";
          "cecb00";
          "0d73cc";
          "cb1ed1";
          "0dcdcd";
          "dddddd";
          "767676";
          "f2201f";
          "23fd00";
          "fffd00";
          "1a8fff";
          "fd28ff";
          "14ffff";
          "ffffff";
        ];
  }

let terminals =
  [
    ("xterm light", Test_harness.xterm_light);
    ("xterm dark", Test_harness.xterm_dark);
    ("VTE", vte);
    ("kitty white", kitty_white);
  ]

let legible_everywhere view =
  List.iter
    (fun (name, colors) ->
      equal ~msg:name (list string) []
        (Test_harness.illegible colors
           (Test_harness.painted
              (Test_harness.paint ~theme:(Theme.default colors) view))))
    terminals

let source = "let x = \"s\" (* c *) 42 f M T"

let scopes =
  [
    (0, 3, "keyword");
    (8, 11, "string");
    (12, 19, "comment");
    (20, 22, "number");
    (23, 24, "function");
    (25, 26, "module");
    (27, 28, "type");
  ]

let syntax_default () =
  legible_everywhere
    (Mosaic.code ~layout:(size 40 2)
       ~syntax:
         (Code.syntax (Syntax_highlight.of_triples ~content:source scopes))
       source)

let markdown_default () =
  legible_everywhere
    (Mosaic.markdown ~layout:(size 30 8)
       "# Title\n\n**b** `code` [link](u) ~~s~~\n\n> quote\n\n- item\n\n---")

let diff_default () =
  legible_everywhere
    (Mosaic.diff ~layout:(size 30 6)
       (Diff.Patch.of_strings ~old:"a\nb\n" ~new_:"a\nc\n" ()))

(* A view given no style follows the theme in scope: the next frame after
   the root's theme changes draws with the new theme's styles, without a
   new declaration. On xterm's dark colors, muted text such as a list's
   marker takes entry 8, and a keyword entry 13. *)
let restyle view ~x ~y expected =
  let r = Test_harness.paint view in
  equal ~msg:"plain: the terminal's color" bool true
    (match (Test_harness.cell_style r ~x ~y).fg with
    | None -> true
    | Some c -> Ansi.Color.intent c = Default);
  Renderer.set_theme r (Theme.default Test_harness.xterm_dark);
  Renderer.render_frame r ~width:Test_harness.doors_width
    ~height:Test_harness.doors_height ~dt:0.;
  equal ~msg:"after the change" bool true
    (Test_harness.fg_is
       (Ansi.Color.indexed expected)
       (Test_harness.cell_style r ~x ~y))

let code_follows_theme () =
  restyle
    (Mosaic.code ~layout:(size 40 2)
       ~syntax:
         (Code.syntax (Syntax_highlight.of_triples ~content:source scopes))
       source)
    ~x:0 ~y:0 13

let markdown_follows_theme () =
  restyle (Mosaic.markdown ~layout:(size 30 4) "- item") ~x:0 ~y:0 8

let () =
  run "mosaic.theme"
    [
      group "Default styles"
        [
          test "syntax, legible" syntax_default;
          test "markdown, legible" markdown_default;
          test "diff, legible" diff_default;
          test "code follows the theme" code_follows_theme;
          test "markdown follows the theme" markdown_follows_theme;
        ];
      group "Values"
        [
          test "plain's surfaces and glyphs" plain_surfaces;
          test "default's surfaces" default_surfaces;
          test "modifiers" modifiers;
        ];
      group "Role tables"
        [
          test "27 palettes" theme_palettes;
          test "adjudicated classes" theme_classes;
          test "with_style, equal, check" theme_with_style;
          prop "any palette" gen_palette (fun c -> theme_laws ~msg:"random" c);
        ];
      group "Scope"
        [
          test "nested themed" nested;
          test "fragments and viewport switches" fragments;
          test "the root's theme" root_theme;
          test "the theme follows the view" theme_follows_view;
          test "memo" memo;
          test "embedded node" embed;
          test "embedded node with its own theme" embed_own_theme;
          test "a node's own theme" imperative;
        ];
      group "Cost" [ test "reading the scope" read_allocates_nothing ];
    ]
