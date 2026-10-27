(** Spans written with the fewest style and hyperlink changes, the
    implementation of {!Ansi.render}.

    A span is a text in a style. {!emit} writes spans one after the other, with
    the OSC 8 and the transition ({!Style.emit}) from the span before, and
    writes their texts as they are. *)

(** {1:spans Spans} *)

type span = { text : string; style : Style.t }
(** The type for spans. *)

(** {1:state Render state} *)

type state = {
  style : Style.t;  (** The style the terminal is in. *)
  link_open : bool;  (** [true] iff a hyperlink is open. *)
}
(** The type for the state of the terminal {!emit} writes to. The state given to
    {!emit} must be the terminal's, or the hyperlinks it writes are unbalanced.
*)

val initial_state : state
(** [initial_state] is {!Style.default} without an open hyperlink. *)

(** {1:rendering Rendering} *)

val emit :
  ?depth:Color.depth ->
  ?state:state ->
  ?hyperlinks_enabled:bool ->
  Writer.t ->
  span list ->
  state
(** [emit ~depth ~state ~hyperlinks_enabled w spans] writes [spans] to [w] from
    [state], with their transitions at [depth] ({!Style.emit}), and is the state
    after them. Where the link changes, it closes the open hyperlink and opens
    the new one, unless [hyperlinks_enabled] is [false]. [state] defaults to
    {!initial_state} and [hyperlinks_enabled] to [true]. *)

val render :
  ?depth:Color.depth ->
  ?state:state ->
  ?hyperlinks_enabled:bool ->
  span list ->
  string
(** [render ~depth ~state ~hyperlinks_enabled spans] is the bytes {!emit}
    writes, followed by the OSC 8 that closes an open hyperlink and [CSI 0 m].
*)
