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
    context lines can match either {!Old} or {!New}. *)

type source_line = { side : side; line : int }
(** The type for a 1-based source line on one side of a diff. *)

val source_line_row : Patch.t -> layout:layout -> source_line -> int option
(** [source_line_row patch ~layout source] is the zero-based logical diff row of
    [source] in [patch], if [source] is present.

    In unified layout, context lines can be found through either side. In split
    layout, blank alignment rows are skipped. The result is independent of line
    wrapping and is an exact scroll coordinate only when wrapping is disabled.
*)

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
    insert blank alignment rows into the rendered side content. *)

val syntax :
  language:string ->
  ?style:Syntax_style.t ->
  ?conceal:bool ->
  ?draw_unstyled:bool ->
  ?streaming:bool ->
  Code.Highlighter.t ->
  syntax
(** [syntax ~language highlighter] is a diff syntax configuration.

    [draw_unstyled] controls whether plain source is visible before the first
    result is available. Wrapped concealed split views may suppress unstyled
    text while highlighting is pending so that alignment uses final line
    metrics. [streaming] keeps the previous rendered buffer visible while a
    fresh result is pending. *)

type highlight = { old : syntax; new_ : syntax }
(** Syntax configurations for old and new content.

    Unified layout uses [new_] because the rendered buffer combines context,
    additions, and removals from a single file language. Split layout uses [old]
    for the left side and [new_] for the right side. *)

type t
(** The type for diff display renderables. *)

module Props : sig
  type t

  val make :
    ?patch:Patch.t ->
    ?layout:layout ->
    ?theme:theme ->
    ?highlight:highlight ->
    ?line_highlights:line_highlight list ->
    ?show_line_numbers:bool ->
    ?wrap:Text_surface.wrap ->
    ?selectable:bool ->
    ?text_style:Ansi.Style.t ->
    unit ->
    t
  (** [make ()] is a diff props value. [patch] defaults to an empty patch,
      [layout] to [Unified], [theme] to {!default_theme}, [show_line_numbers] to
      [true], [line_highlights] to [[]], [wrap] to [`None], [selectable] to
      [true], and [text_style] to {!Ansi.Style.default}. *)

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
  ?show_line_numbers:bool ->
  ?wrap:Text_surface.wrap ->
  ?selectable:bool ->
  ?text_style:Ansi.Style.t ->
  Patch.t ->
  t
(** [create ~parent patch] creates a diff renderable attached to [parent]. *)

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

val apply_props : t -> Props.t -> unit
(** [apply_props t props] applies [props] to [t]. *)
