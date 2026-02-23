open Mosaic_ui
open Expect_harness

let%expect_test "renders through reconciler" =
  render ~width:40 ~height:8 (Vnode.markdown "# Hello\n\nWorld");
  [%expect {|Hello

World|}]

(* ── Headings ── *)

let%expect_test "h1 through h3" =
  render_markdown {|# Heading 1

## Heading 2

### Heading 3|};
  [%expect {|Heading 1

Heading 2

Heading 3|}]

let%expect_test "h1 through h6" =
  render_markdown {|# H1

## H2

### H3

#### H4

##### H5

###### H6|};
  [%expect {|H1

H2

H3

H4

H5

H6|}]

let%expect_test "headings with conceal=false show markers" =
  render_markdown ~conceal:false {|# Heading 1

## Heading 2|};
  [%expect {|# Heading 1

## Heading 2|}]

(* ── Paragraphs ── *)

let%expect_test "simple paragraph" =
  render_markdown "Hello, world!";
  [%expect {|Hello, world!|}]

let%expect_test "multiple paragraphs" =
  render_markdown {|First paragraph.

Second paragraph.

Third paragraph.|};
  [%expect {|First paragraph.

Second paragraph.

Third paragraph.|}]

(* ── Inline formatting ── *)

let%expect_test "bold text" =
  render_markdown "This has **bold** text in it.";
  [%expect {|This has bold text in it.|}]

let%expect_test "italic text" =
  render_markdown "This has *italic* text in it.";
  [%expect {|This has italic text in it.|}]

let%expect_test "inline code" =
  render_markdown "Use `console.log()` to debug.";
  [%expect {|Use console.log() to debug.|}]

let%expect_test "mixed inline formatting" =
  render_markdown "**Bold**, *italic*, and `code` together.";
  [%expect {|Bold, italic, and code together.|}]

let%expect_test "inline formatting with conceal=false" =
  render_markdown ~conceal:false "**Bold**, *italic*, and `code` together.";
  [%expect {|**Bold**, *italic*, and `code` together.|}]

(* ── Links ── *)

let%expect_test "links with conceal mode" =
  render_markdown
    "Check out [Mosaic](https://github.com/example/mosaic) for more.";
  [%expect {|Check out Mosaic (https://github.com/example/mosaic) for
more.|}]

let%expect_test "links with conceal=false" =
  render_markdown ~conceal:false
    "Check out [Mosaic](https://github.com/example/mosaic) for more.";
  [%expect {|Check out [Mosaic](https://github.com/example/mosaic) for
more.|}]

(* ── Lists ── *)

let%expect_test "unordered list" =
  render_markdown {|- Item one
- Item two
- Item three|};
  [%expect {|• Item one
• Item two
• Item three|}]

let%expect_test "ordered list" =
  render_markdown {|1. First item
2. Second item
3. Third item|};
  [%expect {|1. First item
2. Second item
3. Third item|}]

let%expect_test "nested list" =
  render_markdown {|- Parent
  - Child one
  - Child two
- Another parent|};
  [%expect {|• Parent
  • Child one
  • Child two
• Another parent|}]

let%expect_test "list with inline formatting" =
  render_markdown {|- **Bold** item
- *Italic* item
- `Code` item|};
  [%expect {|• Bold item
• Italic item
• Code item|}]

(* ── Task lists ── *)

let%expect_test "task list" =
  render_markdown
    {|- [ ] Unchecked task
- [x] Checked task
- [ ] Another unchecked|};
  [%expect {|• [ ] Unchecked task
• [x] Checked task
• [ ] Another unchecked|}]

(* ── Blockquotes ── *)

let%expect_test "simple blockquote" =
  render_markdown ~width:40 {|> This is a quote spanning multiple lines|};
  [%expect {|│ This is a quote spanning multiple
│ lines|}]

let%expect_test "blockquote with inline formatting" =
  render_markdown ~width:40 {|> This has **bold** and `code` inside|};
  [%expect {|│ This has bold and code inside|}]

let%expect_test "nested blockquote stacks vertically" =
  render_markdown ~width:60
    {|> This is a blockquote.
>
> > Nested blockquotes are supported.
> > They can contain **formatted text**.|};
  [%expect
    {|│ This is a blockquote.
│
│ │ Nested blockquotes are supported. They can contain
│ │ formatted text.|}]

(* ── Code blocks ── *)

let%expect_test "fenced code block" =
  render_markdown {|```
const x = 1;
console.log(x);
```|};
  [%expect {|│ const x = 1;
│ console.log(x);|}]

let%expect_test "code block with language tag" =
  render_markdown {|```javascript
const x = 1;
console.log(x);
```|};
  [%expect {|│ const x = 1;
│ console.log(x);|}]

let%expect_test "code block with custom render_code" =
  render_markdown
    ~render_code:(fun ~parent ~language:_ ~content ->
      let node = Renderable.create ~parent () in
      let _text = Text.create ~parent:node ~content:("CUSTOM: " ^ content) () in
      node)
    {|```js
hello
```|};
  [%expect {|CUSTOM: hello|}]

(* ── Thematic breaks ── *)

let%expect_test "horizontal rule" =
  render_markdown ~width:20 {|Before

---

After|};
  [%expect {|Before

───────────────────…

After|}]

(* ── Tables ── *)

let%expect_test "simple 2-column table" =
  render_markdown ~width:30 {|| A | B |
|---|---|
| 1 | 2 |
| 3 | 4 |};
  [%expect
    {|┌──────────────┬─────────────┐
│A             │B            │
│──────────────│─────────────│
│1             │2            │
└──────────────┴─────────────┘

| 3 | 4|}]

let%expect_test "table with 3 columns" =
  render_markdown ~width:40
    {|| Name | Age | City |
|---|---|---|
| Alice | 30 | NYC |
| Bob | 25 | LA |};
  [%expect
    {|┌─────────────┬───────────┬────────────┐
│Name         │Age        │City        │
│─────────────│───────────│────────────│
│Alice        │30         │NYC         │
└─────────────┴───────────┴────────────┘

| Bob | 25 | LA|}]

let%expect_test "table with column alignment" =
  render_markdown ~width:40
    {|| Left | Center | Right |
|:---|:---:|---:|
| L1 | C1 | R1 |
| L2 | C2 | R2 |};
  [%expect
    {|┌───────────┬─────────────┬────────────┐
│Left       │Center       │Right       │
│───────────│─────────────│────────────│
│L1         │C1           │R1          │
└───────────┴─────────────┴────────────┘

| L2 | C2 | R2|}]

let%expect_test "table with inline formatting in cells" =
  render_markdown ~width:40
    {|| Feature | Status |
|---|---|
| Tables | **Done** |
| Conceal | `Working` |};
  [%expect
    {|┌───────────────────┬──────────────────┐
│Feature            │Status            │
│───────────────────│──────────────────│
│Tables             │Done              │
└───────────────────┴──────────────────┘

| Conceal | Working|}]

let%expect_test "table with empty cells" =
  render_markdown ~width:30
    {|| A | B | C |
|---|---|---|
| 1 |   | 3 |
|   | 2 |   |};
  [%expect
    {|┌─────────┬────────┬─────────┐
│A        │B       │C        │
│─────────│────────│─────────│
│1        │        │3        │
│─────────│────────│─────────│
│         │2       │         │
└─────────┴────────┴─────────┘|}]

let%expect_test "table with only header no data rows" =
  render_markdown ~width:30 ~height:5 {|| Header1 | Header2 |
|---|---|};
  [%expect
    {|┌──────────────┬─────────────┐
│Header1       │Header2      │
└──────────────┴─────────────┘

|---|---|}]

let%expect_test "table shows all rows when streaming=false" =
  render_markdown ~width:30 ~streaming:false {|| A | B |
|---|---|
| 1 | 2 |};
  [%expect
    {|┌──────────────┬─────────────┐
│A             │B            │
│──────────────│─────────────│
└──────────────┴─────────────┘

| 1 | 2|}]

(* ── Conceal mode ── *)

let%expect_test "conceal=true hides markdown punctuation" =
  render_markdown
    {|# Heading

This has **bold** and *italic* and `code`.

[Link](https://example.com)|};
  [%expect
    {|Heading

This has bold and italic and code.

Link (https://example.com)|}]

let%expect_test "conceal=false shows all syntax" =
  render_markdown ~conceal:false
    {|# Heading

This has **bold** and *italic* and `code`.

[Link](https://example.com)|};
  [%expect
    {|# Heading

This has **bold** and *italic* and `code`.

[Link](https://example.com)|}]

let%expect_test "set_conceal toggles punctuation visibility" =
  let app = make_markdown_app "This has **bold** text." in
  markdown_frame app ~width:60 ~height:5;
  [%expect {|This has bold text.|}];
  Markdown.set_conceal app.md false;
  markdown_frame app ~width:60 ~height:5;
  [%expect {|This has **bold**ttext.|}]

(* ── Streaming ── *)

let%expect_test "streaming mode skips last table row" =
  render_markdown ~width:30 ~streaming:true
    {|| A | B |
|---|---|
| 1 | 2 |
| 3 | 4 |};
  [%expect
    {|┌──────────────┬─────────────┐
│A             │B            │
│──────────────│─────────────│
└──────────────┴─────────────┘

| 3 | 4|}]

let%expect_test "set_streaming toggles streaming mode" =
  let app = make_markdown_app ~streaming:false "# Hello" in
  markdown_frame app ~width:60 ~height:5;
  [%expect {|Hello|}];
  Markdown.set_streaming app.md true;
  markdown_frame app ~width:60 ~height:5;
  [%expect {|Hello|}]

let%expect_test "streaming mode with partial code block" =
  render_markdown ~streaming:true {|# Title

Some text.

```py|};
  [%expect {|Title

Some text.

│|}]

(* ── Content updates ── *)

let%expect_test "set_content replaces content" =
  let app = make_markdown_app "# First" in
  markdown_frame app ~width:60 ~height:5;
  [%expect {|First|}];
  Markdown.set_content app.md "# Second";
  markdown_frame app ~width:60 ~height:5;
  [%expect {|Second|}]

let%expect_test "set_content no-op for same content" =
  let app = make_markdown_app "# Hello" in
  markdown_frame app ~width:60 ~height:5;
  [%expect {|Hello|}];
  Markdown.set_content app.md "# Hello";
  markdown_frame app ~width:60 ~height:5;
  [%expect {|Hello|}]

let%expect_test "set_content appending" =
  let app = make_markdown_app "# Hello" in
  markdown_frame app ~width:60 ~height:5;
  [%expect {|Hello|}];
  Markdown.set_content app.md "# Hello\n\nWorld";
  markdown_frame app ~width:60 ~height:10;
  [%expect {|Hello

World|}]

(* ── Style changes ── *)

let%expect_test "set_style changes appearance" =
  let app = make_markdown_app "# Heading" in
  markdown_frame app ~width:60 ~height:5;
  [%expect {|Heading|}];
  let new_style = function
    | Markdown.Default -> Ansi.Style.default
    | _ -> Ansi.Style.default
  in
  Markdown.set_style app.md new_style;
  markdown_frame app ~width:60 ~height:5;
  [%expect {|Heading|}]

(* ── Custom renderers ── *)

let%expect_test "custom render_node overrides heading" =
  render_markdown
    ~render_node:(fun block ~parent ~is_last:_ ->
      match block with
      | Cmarkit.Block.Heading _ ->
          let node = Renderable.create ~parent () in
          let _text = Text.create ~parent:node ~content:"[CUSTOM] Heading" () in
          Some node
      | _ -> None)
    {|# Original Heading

Regular paragraph.|};
  [%expect {|[CUSTOM] Heading
Regular paragraph.|}]

let%expect_test "custom render_node returning None uses default" =
  render_markdown
    ~render_node:(fun _block ~parent:_ ~is_last:_ -> None)
    {|# Heading

Paragraph text.|};
  [%expect {|Heading

Paragraph text.|}]

(* ── Strikethrough ── *)

let%expect_test "strikethrough text" =
  render_markdown "This has ~~deleted~~ text.";
  [%expect {|This has deleted text.|}]

let%expect_test "strikethrough with conceal=false" =
  render_markdown ~conceal:false "This has ~~deleted~~ text.";
  [%expect {|This has ~~deleted~~ text.|}]

(* ── Edge cases ── *)

let%expect_test "empty content" =
  render_markdown "";
  [%expect {||}]

let%expect_test "incomplete code block (no closing fence)" =
  render_markdown
    {|Here is some code:

```javascript
const x = 1;
console.log(x);|};
  [%expect {|Here is some code:

│ const x = 1;
│ console.log(x);|}]

let%expect_test "incomplete bold markers" =
  render_markdown "This has **unclosed bold text";
  [%expect {|This has **unclosed bold text|}]

let%expect_test "trailing blank lines" =
  render_markdown "# Heading\n\nParagraph text.\n\n\n";
  [%expect {|Heading

Paragraph text.|}]

let%expect_test "blank lines between blocks add spacing" =
  render_markdown {|First

Second

Third|};
  [%expect {|First

Second

Third|}]

let%expect_test "complex markdown document" =
  render_markdown ~width:60
    {|# Project Title

Welcome to **Mosaic**, a terminal UI library.

## Features

- Automatic table alignment
- `inline code` support
- *Italic* and **bold** text

## Code Example

```ocaml
let md = Markdown.create ~parent ~content:"# Hello" ()
```

## Links

Visit [GitHub](https://github.com) for more.

---

*Press `?` for help*|};
  [%expect
    {|Project Title

Welcome to Mosaic, a terminal UI library.

Features

• Automatic table alignment
• inline code support
• Italic and bold text

Code Example

│ let md = Markdown.create ~parent ~content:"# Hello" ()

Links

Visit GitHub (https://github.com) for more.

───────────────────────────────────────|}]
