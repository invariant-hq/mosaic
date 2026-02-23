(** Terminal UI toolkit: widgets, rendering, and event dispatch.

    [Mosaic_ui] re-exports the modules that make up the Mosaic UI layer. It
    provides:

    - {b Core infrastructure.} {!Event} for input events, {!Selection} for text
      selections, {!Renderable} for the mutable node tree, {!Renderer} for the
      layout-render-diff pipeline, and {!Vnode} for declarative virtual-node
      descriptions.
    - {b Text primitives.} {!Text_buffer} for styled text storage,
      {!Text_surface} for viewport-aware text rendering with wrapping, and
      {!Edit_buffer} for grapheme-aware editing with undo.
    - {b Widget catalogue.} {!Box}, {!Text}, {!Slider}, {!Text_input},
      {!Canvas}, {!Select}, {!Tab_select}, {!Markdown}, {!Spinner},
      {!Progress_bar}, {!Textarea}, {!Scroll_bar}, {!Scroll_box}, {!Table},
      {!Tree}, {!Code}, and {!Line_number}.
    - {b Theming.} {!Syntax_theme} for tree-sitter/TextMate-style syntax
      colouring. *)

(** {1:events Events and input} *)

module Event = Event
(** Keyboard, paste, and mouse UI events with propagation control. *)

module Selection = Selection
(** Text selections with anchor and focus points. *)

(** {1:core Core infrastructure} *)

module Renderer = Renderer
(** Layout, drawing, hit testing, and event dispatch pipeline. *)

module Renderable = Renderable
(** Mutable UI tree nodes with layout integration. *)

(** {1:text Text primitives} *)

module Text_buffer = Text_buffer
(** Styled text storage with per-character styles and highlight overlays. *)

module Text_surface = Text_surface
(** Text rendering surface with wrapping, viewport, and selection. *)

module Edit_buffer = Edit_buffer
(** Grapheme-aware text editing buffer with cursor, selection, and undo. *)

(** {1:widgets Widget catalogue} *)

module Box = Box
(** Bordered container with background fill and optional title. *)

module Text = Text
(** Styled text display with wrapping, truncation, and selection. *)

module Slider = Slider
(** Interactive horizontal or vertical slider. *)

module Text_input = Text_input
(** Single-line text input with cursor, selection, and clipboard. *)

module Canvas = Canvas
(** Low-level drawing surface for custom cell-level rendering. *)

module Select = Select
(** Vertical list selector with optional descriptions. *)

module Tab_select = Tab_select
(** Horizontal tab navigation with optional underline and descriptions. *)

module Markdown = Markdown
(** CommonMark renderer with headings, code blocks, lists, and tables. *)

module Spinner = Spinner
(** Animated loading indicator with preset frame sets. *)

module Progress_bar = Progress_bar
(** Progress bar with sub-cell precision via Unicode half-blocks. *)

module Textarea = Textarea
(** Multi-line text editor with wrapping, scrolling, and selection. *)

module Scroll_bar = Scroll_bar
(** Scrollbar with proportional thumb sizing and arrow buttons. *)

module Scroll_box = Scroll_box
(** Scrollable container with optional scrollbars and sticky scroll. *)

module Table = Table
(** Data table with columns, rows, keyboard navigation, and selection. *)

module Tree = Tree
(** Hierarchical tree with expandable nodes, guide lines, and selection. *)

module Code = Code
(** Code display with syntax highlighting, wrapping, and selection. *)

module Line_number = Line_number
(** Line-number gutter with per-line colours, signs, and custom numbering. *)

(** {1:theming Theming} *)

module Syntax_theme = Syntax_theme
(** Syntax themes: maps capture-group names to terminal styles. *)

(** {1:vdom Declarative UI} *)

module Vnode = Vnode
(** Virtual node tree for declarative UI descriptions. *)
