(** Keyboard binding maps.

    [Keymap] maps normalized key events to semantic actions. Bindings are inert
    data, aliases are resolved at map construction, and lookup checks both the
    parsed key name and Kitty's base-layout key when present. *)

type 'a binding
(** The type for a key binding that resolves to an action of type ['a]. *)

val binding :
  ?ctrl:bool ->
  ?shift:bool ->
  ?alt:bool ->
  ?super:bool ->
  string ->
  'a ->
  'a binding
(** [binding name action] binds [name] with optional modifiers to [action].

    Matrix reports terminal Alt input as either [alt] or [meta], and lookup
    accepts both. *)

val binding_equal : ('a -> 'a -> bool) -> 'a binding -> 'a binding -> bool
(** [binding_equal equal_action a b] is [true] iff [a] and [b] bind the same key
    shape to equal actions. *)

type 'a t
(** The type for action lookup maps. *)

val make :
  ?aliases:(string * string) list ->
  defaults:'a binding list ->
  ?custom:'a binding list ->
  unit ->
  'a t
(** [make ~defaults ?custom ()] builds a keymap. Custom bindings override
    defaults with the same key shape. *)

val action : 'a t -> Input.Key.event -> 'a option
(** [action t ev] is the action bound to [ev], if any. *)
