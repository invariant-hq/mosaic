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
  let surface s = Theme.surface Theme.plain s in
  equal ~msg:"canvas" bool true
    (Ansi.Style.equal (surface Canvas) Ansi.Style.default);
  equal ~msg:"cursor" bool true
    (Ansi.Style.equal (surface Cursor) (Ansi.Style.make ~inverse:true ()));
  equal ~msg:"selected" bool true
    (Ansi.Style.equal (surface Selected)
       (Ansi.Style.make ~bold:true ~underline:true ()));
  equal ~msg:"glyphs" bool true
    (Glyphs.equal (Theme.glyphs Theme.plain) Glyphs.unicode)

let default_text () =
  let colors =
    {
      Ansi.Theme.foreground = (0, 0, 0);
      background = (255, 255, 255);
      palette = Array.init 16 (fun i -> Some (i * 16, 0, 0));
    }
  in
  let t = Theme.default colors in
  equal ~msg:"text" bool true
    (Ansi.Theme.equal (Theme.text t) (Ansi.Theme.default colors));
  List.iter
    (fun s ->
      equal ~msg:"surfaces as plain's" bool true
        (Ansi.Style.equal (Theme.surface t s) (Theme.surface Theme.plain s)))
    Theme.surfaces

let modifiers () =
  let t = Theme.with_surface Cursor red Theme.plain in
  equal ~msg:"the surface" bool true
    (Ansi.Style.equal (Theme.surface t Cursor) red);
  equal ~msg:"the others kept" bool true
    (Ansi.Style.equal
       (Theme.surface t Highlight)
       (Theme.surface Theme.plain Highlight));
  equal ~msg:"plain unchanged" bool false (Theme.equal t Theme.plain);
  let t = error_red Theme.plain in
  equal ~msg:"the role's text" bool true
    (Ansi.Style.equal (Theme.style t Error) red);
  equal ~msg:"the role's glyphs" bool true
    (Ansi.Style.equal (Theme.mark ~alone:true t Error) red)

(* ───── Scope ───── *)

let size w h =
  Toffee.Style.set_size (Vnode.size ~width:w ~height:h) Toffee.Style.default

(* A canvas that records the theme it is drawn with, by name. *)
let seen = Hashtbl.create 8

let canvas name =
  Vnode.canvas ~style:(size 2 1)
    ~on_draw:(fun c ~dt:_ -> Hashtbl.replace seen name (Canvas.theme c))
    ()

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

let column children = Vnode.box ~style:(size 20 10) children

(* The nearest [themed] decides last; a sibling outside it keeps the outer
   scope, and an element outside every one the root's. *)
let nested () =
  let _, render = mount () in
  render
    (column
       [
         Vnode.themed ascii
           (Vnode.box ~style:(size 20 5)
              [
                canvas "a";
                Vnode.themed error_red (canvas "b");
                Vnode.themed error_blue (Vnode.themed error_red (canvas "c"));
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
         Vnode.themed ascii
           (Vnode.fragment
              [
                canvas "a";
                Vnode.viewport_switch ~at_least_width:10 ~wide:(canvas "b")
                  ~narrow:Vnode.empty;
              ]);
       ]);
  saw "a" (ascii Theme.plain);
  saw "b" (ascii Theme.plain)

(* The root's theme changes the scope of every node at the next frame,
   with no view reconciled again. *)
let root_theme () =
  let renderer, render = mount () in
  render (column [ Vnode.themed ascii (canvas "a"); canvas "b" ]);
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
  render (column [ Vnode.themed ascii (canvas "a") ]);
  saw "a" (ascii Theme.plain);
  render (column [ canvas "a" ]);
  saw "a" Theme.plain;
  render (column [ Vnode.themed error_red (canvas "a") ]);
  saw "a" (error_red Theme.plain)

(* An unchanged memo keeps its subtree, which still takes the theme its
   place declares now, and its view is not built again. *)
let memo_view_calls = ref 0

let memo_view () =
  incr memo_view_calls;
  Vnode.themed error_red (canvas "m")

let memo () =
  memo_view_calls := 0;
  let _, render = mount () in
  render (column [ Vnode.themed ascii (Vnode.memo ~key:() memo_view) ]);
  saw "m" (error_red (ascii Theme.plain));
  render (column [ Vnode.themed error_blue (Vnode.memo ~key:() memo_view) ]);
  saw "m" (error_red (error_blue Theme.plain));
  render (column [ Vnode.memo ~key:() memo_view ]);
  saw "m" (error_red Theme.plain);
  equal ~msg:"the view built once" int 1 !memo_view_calls

(* An embedded node has the theme of its place, and none outside every
   [themed]. *)
let embed () =
  let renderer, render = mount () in
  let node = Renderable.create ~parent:(Renderer.root renderer) () in
  render (column [ Vnode.themed ascii (Vnode.embed node) ]);
  equal ~msg:"themed" theme (ascii Theme.plain) (Renderable.theme node);
  render (column [ Vnode.embed node ]);
  equal ~msg:"not themed" theme Theme.plain (Renderable.theme node)

(* An embedded node's own function applies after the scope of its place,
   which never replaces it. *)
let embed_own_theme () =
  let renderer, render = mount () in
  let node = Renderable.create ~parent:(Renderer.root renderer) () in
  Renderable.set_theme node (Some error_red);
  render
    (column [ Vnode.themed (fun t -> ascii (error_blue t)) (Vnode.embed node) ]);
  equal ~msg:"its own over its place's" theme
    (error_red (ascii (error_blue Theme.plain)))
    (Renderable.theme node);
  render (column [ Vnode.embed node ]);
  equal ~msg:"its own alone" theme (error_red Theme.plain)
    (Renderable.theme node);
  Renderable.set_theme node None;
  render (column [ Vnode.themed ascii (Vnode.embed node) ]);
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
   the theme in scope: on a terminal of reported colors, under its
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

let vte : Ansi.Theme.colors =
  {
    foreground = (0xc0, 0xc0, 0xc0);
    background = (0, 0, 0);
    palette =
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

let kitty_white : Ansi.Theme.colors =
  {
    foreground = (0, 0, 0);
    background = (255, 255, 255);
    palette =
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
    (Vnode.code ~style:(size 40 2)
       ~syntax:
         (Code.syntax (Syntax_highlight.of_triples ~content:source scopes))
       source)

let markdown_default () =
  legible_everywhere
    (Vnode.markdown ~style:(size 30 8)
       "# Title\n\n**b** `code` [link](u) ~~s~~\n\n> quote\n\n- item\n\n---")

let diff_default () =
  legible_everywhere
    (Vnode.diff ~style:(size 30 6)
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
    (Vnode.code ~style:(size 40 2)
       ~syntax:
         (Code.syntax (Syntax_highlight.of_triples ~content:source scopes))
       source)
    ~x:0 ~y:0 13

let markdown_follows_theme () =
  restyle (Vnode.markdown ~style:(size 30 4) "- item") ~x:0 ~y:0 8

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
          test "default's text" default_text;
          test "modifiers" modifiers;
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
