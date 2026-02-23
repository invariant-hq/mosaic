(** Styled text segment rendering.

    Renders [(Style.t, string)] segments to ANSI escape sequences with minimal
    style transitions and automatic hyperlink management. *)

(** {1:segments Segments} *)

type segment = Style.t * string
(** The type for styled text segments. *)

(** {1:state Render state} *)

type state = {
  style : Style.t;  (** Current terminal style. *)
  link_open : bool;  (** Whether a hyperlink is open. *)
}
(** The type for render state. Tracks the terminal's current escape state so
    that only necessary transitions are emitted.

    {b Warning.} The [state] passed to {!emit} must accurately reflect the
    terminal's state. If [link_open] is [true], an OSC 8 hyperlink must be open.
    Mismatches cause unbalanced sequences. *)

val initial_state : state
(** [initial_state] is {!Style.default} with no open link. *)

(** {1:rendering Rendering} *)

val emit :
  ?state:state -> ?hyperlinks_enabled:bool -> Writer.t -> segment list -> state
(** [emit ~state ~hyperlinks_enabled w segs] renders [segs] to [w] and returns
    the updated state. Style transitions emit only changed SGR components.
    Hyperlinks are managed automatically: changing to a different URL closes the
    previous link.

    [state] defaults to {!initial_state}. [hyperlinks_enabled] defaults to
    [true]; when [false], hyperlinks in styles are ignored. *)

val render : ?state:state -> ?hyperlinks_enabled:bool -> segment list -> string
(** [render ~state ~hyperlinks_enabled segs] renders [segs] to a string. Closes
    any active hyperlink and appends a reset sequence at the end.

    [state] defaults to {!initial_state}. [hyperlinks_enabled] defaults to
    [true]. *)
