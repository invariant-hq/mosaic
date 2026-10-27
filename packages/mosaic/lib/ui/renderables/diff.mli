(** Unified and split-view diff display.

    A composite renderable that displays a {!Patch.t} as a one- or two-column
    view of {!Code} content paired with {!Line_number} gutters. Scrolling is
    delegated to the parent; wrap a {!Diff} in a {!Scroll_box} to make it
    scrollable. *)

module Patch : sig
  type tag =
    | Context
    | Added
    | Removed
        (** The role of a patch line. [Context] appears on both sides, [Added]
            only on the new side, and [Removed] only on the old side. *)

  type line = { tag : tag; content : string }
  (** The type for patch lines. [content] excludes the unified-diff prefix and
      trailing newline. *)

  type hunk = {
    old_start : int;
    old_lines : int;
    new_start : int;
    new_lines : int;
    lines : line list;
  }
  (** The type for contiguous patch hunks. Empty old or new ranges use start
      line [0], matching standard unified diffs such as ["@@ -0,0 +1,3 @@"]. *)

  type t
  (** The type for validated patches. *)

  val make : hunk list -> t
  (** [make hunks] is a patch from [hunks]. Raises [Invalid_argument] if hunk
      starts, counts, or ordering are invalid. *)

  val of_unified : string -> (t, string) result
  (** [of_unified s] parses the first unified-diff file patch in [s]. File
      headers and prelude lines are tolerated. "\\ No newline at end of file"
      markers are skipped. *)

  val of_strings : old:string -> new_:string -> ?context:int -> unit -> t
  (** [of_strings ~old ~new_ ()] computes a line-level patch with Myers diff.
      [context] controls unchanged lines around changes and defaults to [3]. *)

  val hunks : t -> hunk list
  (** [hunks t] is [t]'s hunks, in source order. *)

  val is_empty : t -> bool
  (** [is_empty t] is [true] iff [t] has no hunks. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] writes [t] in unified-diff hunk form, without file headers. *)
end

type layout =
  | Unified
  | Split
      (** The diff layout. [Unified] renders one column; [Split] renders
          removals on the left and additions on the right. *)

type side = Old | New  (** The side of a source line in a diff. *)

type line_highlight = {
  side : side;
  first : int;
  last : int;
  color : Line_number.line_color;
}
(** The type for a source-line highlight.

    [first] and [last] are inclusive 1-based source line numbers. Highlights
    whose [first] is greater than [last] do not match any line. When highlights
    overlap, the first matching highlight in the list wins. In unified layout,
    context lines can match either {!Old} or {!New}. Highlight colours are
    alpha-blended over the normal diff line colour; opaque colours fully replace
    it. *)

type line_sign = {
  side : side;
  first : int;
  last : int;
  sign : Line_number.line_sign;
}
(** The type for a source-line gutter sign.

    [first] and [last] are inclusive 1-based source line numbers. Signs whose
    [first] is greater than [last] do not match any line. When signs overlap,
    the first matching sign in the list wins. In unified layout, context lines
    can match either {!Old} or {!New}. Custom signs are merged with built-in
    diff signs so that callers can use {!Line_number.line_sign.before} while the
    diff keeps its [+] and [-] signs in {!Line_number.line_sign.after}. *)

type line_span = {
  side : side;
  line : int;
  start_byte : int;
  end_byte : int;
  color : Ansi.Color.t;
}
(** The type for a source-line sub-range highlight.

    [line] is a 1-based source line number on [side]; [start_byte] and
    [end_byte] are byte offsets into that line's content, half-open on
    [\[start_byte, end_byte)]. Out-of-range bytes are clamped to the line and an
    empty range is ignored, as is a span whose [(side, line)] is absent from the
    patch. The span's [color] is drawn as a background over the covered cells,
    on whichever wrapped rows they fall, alpha-blended over the line's existing
    background; opaque colours replace it. Unlike {!line_highlight}, which
    colours a whole line, a span colours a byte range within one line. In
    unified layout, context lines can match either {!Old} or {!New}. *)

type source_line = { side : side; line : int }
(** The type for a 1-based source line on one side of a diff. *)

(** The kind of rendered diff line under a pointer hit. [Blank] is used for
    split-view alignment rows that do not correspond to source content. *)
type line_kind = Context | Added | Removed | Blank

(** The region of the diff row under a pointer hit. *)
type hit_region = Gutter | Sign | Content | Padding

type line_hit = {
  source : source_line option;
  kind : line_kind;
  region : hit_region;
  logical_row : int;
  visual_row : int;
}
(** A semantic hit-test result for a rendered diff line.

    [source] is [None] for split-view blank alignment rows. [logical_row] is the
    zero-based row in the diff content for the relevant layout side.
    [visual_row] is the zero-based display row under the pointer after wrapping.
    These row fields are display coordinates for the current render, not stable
    source positions or persistence keys. *)

type t
(** The type for diff display renderables. *)

val source_line_row : Patch.t -> layout:layout -> source_line -> int option
(** [source_line_row patch ~layout source] is the zero-based logical diff row of
    [source] in [patch], if [source] is present.

    In unified layout, context lines can be found through either side. In split
    layout, blank alignment rows are skipped. The result is independent of line
    wrapping and is an exact scroll coordinate only when wrapping is disabled.
*)

val hit_test : t -> x:int -> y:int -> line_hit option
(** [hit_test t ~x ~y] is the semantic diff hit at global screen coordinate
    [(x, y)], if the coordinate falls inside [t]. *)

type theme = {
  added_bg : Ansi.Color.t;
  removed_bg : Ansi.Color.t;
  context_bg : Ansi.Color.t option;
  added_content_bg : Ansi.Color.t option;
  removed_content_bg : Ansi.Color.t option;
  context_content_bg : Ansi.Color.t option;
  added_sign_color : Ansi.Color.t;
  removed_sign_color : Ansi.Color.t;
  added_line_number_bg : Ansi.Color.t option;
  removed_line_number_bg : Ansi.Color.t option;
  line_number_fg : Ansi.Color.t;
  line_number_bg : Ansi.Color.t option;
}
(** The type for diff colour themes. Changed-line content defaults to [added_bg]
    or [removed_bg]. Changed-line gutters default to transparent; use
    [added_line_number_bg] and [removed_line_number_bg] to colour them. *)

val default_theme : theme
(** [default_theme] is the built-in dark-terminal theme. *)

type syntax
(** The type for diff syntax settings.

    Diff syntax is highlighter-backed rather than precomputed because diff
    layouts may synthesize source buffers from patch rows. Split layout may also
    insert blank alignment rows into the rendered side content.

    Diff syntax settings compare by value, except for the {!Code.Highlighter.t}
    inside, which is a resource compared by identity. Create the highlighter
    once and reuse it across renders. *)

val syntax :
  language:string ->
  ?style:Syntax_style.t ->
  ?conceal:bool ->
  Code.Highlighter.t ->
  syntax
(** [syntax ~language highlighter] is a diff syntax configuration. Each side is
    a {!Code} view configured by {!Code.with_highlighter}, which states what a
    side shows while a result is pending, and [conceal] is the sides' [conceal]
    property (see {!Code.Props.make}). Defaults to [true]. Wrapped split views
    align their rows once both sides have their results, since highlighting may
    conceal text. *)

type highlight = { old : syntax; new_ : syntax }
(** Syntax configurations for old and new content.

    Unified layout uses [new_] because the rendered buffer combines context,
    additions, and removals from a single file language. Split layout uses [old]
    for the left side and [new_] for the right side. *)

module Props : sig
  type t

  val make :
    ?patch:Patch.t ->
    ?layout:layout ->
    ?theme:theme ->
    ?highlight:highlight ->
    ?line_highlights:line_highlight list ->
    ?line_spans:line_span list ->
    ?line_signs:line_sign list ->
    ?show_line_numbers:bool ->
    ?wrap:Text_surface.wrap ->
    ?selectable:bool ->
    ?text_style:Ansi.Style.t ->
    unit ->
    t
  (** [make ()] is a diff props value, with:
      - [patch] is the patch shown. Defaults to an empty patch.
      - [layout] is the view layout. Defaults to [Unified].
      - [theme] is the colour theme. Defaults to {!default_theme}.
      - [highlight] is optional highlighter-backed split-side syntax. Defaults
        to none.
      - [line_highlights] are source-line background highlights, using inclusive
        1-based old/new source line numbers. Earlier entries win when ranges
        overlap. Defaults to [[]].
      - [line_spans] are source-line sub-range highlights: a byte range within
        one 1-based old/new source line, drawn as a background over its cells.
        Defaults to [[]]. See {!line_span}.
      - [line_signs] are source-line gutter signs, using inclusive 1-based
        old/new source line numbers. Earlier entries win when ranges overlap.
        Defaults to [[]].
      - [show_line_numbers] controls whether gutters are shown. Defaults to
        [true].
      - [wrap] is the wrap mode for embedded {!Code} children. Defaults to
        [`None].
      - [selectable] controls text selection on embedded {!Code} children.
        Defaults to [true].
      - [text_style] is the base text style. Defaults to {!Ansi.Style.default}.

      {!val-create} and {!Vnode.diff} take these labels with these defaults. *)

  val default : t
  (** [default] is [make ()]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] describe the same display. *)
end

val create :
  parent:Renderable.t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?layout:layout ->
  ?theme:theme ->
  ?highlight:highlight ->
  ?line_highlights:line_highlight list ->
  ?line_spans:line_span list ->
  ?line_signs:line_sign list ->
  ?show_line_numbers:bool ->
  ?wrap:Text_surface.wrap ->
  ?selectable:bool ->
  ?text_style:Ansi.Style.t ->
  Patch.t ->
  t
(** [create ~parent patch] creates a diff renderable attached to [parent]. The
    labels after [opacity] are {!Props.make}'s and take its defaults. *)

val node : t -> Renderable.t
(** [node t] is [t]'s underlying renderable. *)

val patch : t -> Patch.t
(** [patch t] is the patch currently displayed by [t]. *)

val set_patch : t -> Patch.t -> unit
(** [set_patch t patch] replaces [t]'s patch and rebuilds the view. *)

val set_layout : t -> layout -> unit
(** [set_layout t layout] sets [t]'s layout and rebuilds the view. *)

val set_theme : t -> theme -> unit
(** [set_theme t theme] sets [t]'s theme and rebuilds the view. *)

val set_highlight : t -> highlight option -> unit
(** [set_highlight t highlight] sets split-view syntax highlighting and rebuilds
    the view. *)

val set_line_highlights : t -> line_highlight list -> unit
(** [set_line_highlights t highlights] sets source-line background highlights
    and rebuilds the view. *)

val set_line_spans : t -> line_span list -> unit
(** [set_line_spans t spans] sets source-line sub-range highlights and rebuilds
    the view. *)

val set_line_signs : t -> line_sign list -> unit
(** [set_line_signs t signs] sets source-line gutter signs and rebuilds the
    view. *)

val apply_props : t -> Props.t -> unit
(** [apply_props t props] applies [props] to [t]. *)

val set_on_line_click : t -> (line_hit -> unit) option -> unit
(** [set_on_line_click t f] sets the callback fired when the user clicks a
    rendered diff line. Drag selections do not trigger the callback. *)
