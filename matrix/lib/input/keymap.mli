(** Key binding maps.

    A keymap binds key and modifier combinations to values. Later bindings take
    precedence over earlier ones: add general bindings first, then more specific
    ones to override.

    {1:modifier_semantics Modifier semantics}

    {!add} leaves unspecified modifiers as wildcards (match any state), enabling
    prefix-free patterns like Ctrl+C without constraining Shift or Alt.
    {!add_char} defaults all modifiers to [false] (exact match), so
    [add_char 'c'] only matches a plain [c] press with no modifiers active.

    {b Note.} Lock states ([caps_lock], [num_lock]) are not matchable by keymaps
    since they represent toggle states rather than pressed modifiers. *)

(** {1:keymaps Keymaps} *)

type 'a t
(** The type for immutable keymaps binding keys to values of type ['a]. *)

val empty : 'a t
(** [empty] is a keymap with no bindings. *)

(** {1:adding Adding bindings} *)

val add :
  ?ctrl:bool ->
  ?alt:bool ->
  ?shift:bool ->
  ?super:bool ->
  ?hyper:bool ->
  ?meta:bool ->
  'a t ->
  Event.Key.t ->
  'a ->
  'a t
(** [add map key data] is [map] with a binding from [key] to [data]. Unspecified
    modifier arguments are wildcards (match any state). Later bindings take
    precedence. *)

val add_char :
  ?ctrl:bool ->
  ?alt:bool ->
  ?shift:bool ->
  ?super:bool ->
  ?hyper:bool ->
  ?meta:bool ->
  'a t ->
  char ->
  'a ->
  'a t
(** [add_char map c data] is like {!add} for the character [c]. Unlike {!add},
    all modifier arguments default to [false] (exact match). *)

(** {1:finding Finding bindings} *)

val find :
  ?event_type:(Event.Key.event_type -> bool) -> 'a t -> Event.t -> 'a option
(** [find map event] is the most recently added binding matching a {!Event.Key}
    event with compatible modifiers, or [None] if [event] is not a key event or
    no binding matches.

    [event_type] filters which key event types are eligible. It defaults to
    accepting {!Event.Key.Press} and {!Event.Key.Repeat} but rejecting
    {!Event.Key.Release}, which prevents bindings from firing on key-up on
    terminals supporting the Kitty keyboard protocol. Pass
    [~event_type:(fun _ -> true)] to match all event types. *)
