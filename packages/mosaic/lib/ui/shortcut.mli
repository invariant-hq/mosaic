(** Keyboard shortcuts for subscriptions and event handlers.

    A shortcut matches a key press or repeat ({!Matrix_input.Keystroke.t}) by
    its key and its held modifiers. A key's value is the character typed, with
    Shift consumed: Shift+r is [Char 'R'], so {!char}[ ~shift:true 'r'] is the
    shortcut [Char 'R']. Caps Lock and Num Lock are ignored as modifiers, and a
    lock never changes a chord's key: Ctrl+s is [Char 's'] whatever Caps Lock's
    state.

    A letter typed with Caps Lock on arrives as the capital, as typed. An ASCII
    letter pressed with no Ctrl, Alt, Super, Hyper or Meta is therefore matched
    in two tries. The first is the letter as it would be typed without the lock:
    the other case when the keystroke says Caps Lock is on, the letter itself
    otherwise. The second, when the first is a capital that matches none of the
    shortcuts searched, is its lowercase letter, since a terminal that does not
    report the lock sends Caps Lock with r as R. The shortcuts searched are the
    one given to {!matches}, or the bindings of a list such as those of
    [Sub.on_keys] or of a widget's keymap. [char 'r'] thus matches r typed with
    Caps Lock on, and Shift+r unless a shortcut for [Char 'R'] is searched too,
    which then matches Shift+r.

    The keypad is ignored: keypad Enter matches {!enter}, and the keypad's 5
    without Num Lock is [Begin]. When a terminal reports the base-layout key,
    matching then tries that key with the same modifiers, so that Ctrl+c keeps
    working on a layout whose key in that place types another letter. Releases
    never match. *)

type t
(** The type for keyboard shortcuts. *)

val key :
  ?ctrl:bool ->
  ?alt:bool ->
  ?shift:bool ->
  ?super:bool ->
  ?hyper:bool ->
  ?meta:bool ->
  Matrix_input.Keystroke.key ->
  t
(** [key k] matches [k] with no modifiers. Optional modifier arguments default
    to [false]. With [shift], an ASCII letter is its capital and Shift is
    consumed, as in the keystrokes Matrix reports: [key ~shift:true (Char 'r')]
    is [key (Char 'R')]. [meta] is a Meta key distinct from Alt, which only the
    kitty keyboard protocol reports. *)

val char :
  ?ctrl:bool ->
  ?alt:bool ->
  ?shift:bool ->
  ?super:bool ->
  ?hyper:bool ->
  ?meta:bool ->
  char ->
  t
(** [char c] is [key (Char c)], with [c] as a Unicode code point. Optional
    modifier arguments default to [false]. *)

val ctrl : char -> t
(** [ctrl c] matches Ctrl+[c]. *)

val alt : char -> t
(** [alt c] matches Alt+[c]. *)

val shift : char -> t
(** [shift c] matches Shift+[c]: the capital, for a letter. *)

val escape : t
(** [escape] matches Escape. *)

val enter : t
(** [enter] matches Enter. *)

val tab : t
(** [tab] matches Tab. *)

val backspace : t
(** [backspace] matches Backspace. *)

val delete : t
(** [delete] matches Delete. *)

val f : int -> t
(** [f n] matches function key F[n]. *)

val up : t
(** [up] matches the Up arrow key. *)

val down : t
(** [down] matches the Down arrow key. *)

val left : t
(** [left] matches the Left arrow key. *)

val right : t
(** [right] matches the Right arrow key. *)

val space : t
(** [space] matches Space. *)

val equal : t -> t -> bool
(** [equal a b] is [true] iff [a] and [b] denote the same shortcut. *)

val matches : t -> Event.key -> bool
(** [matches shortcut ev] is [true] iff [ev] matches [shortcut]. *)

val find : (t * 'a) list -> Event.key -> 'a
(** [find bindings ev] is the value of the last binding of [bindings] whose
    shortcut {!matches} [ev]: of two bindings of one key, the later one wins, as
    the bindings a program gives a widget override the widget's own before them.
    It takes time linear in the number of bindings and allocates nothing.

    Raises [Not_found] if no binding matches [ev]. *)

val find_opt : (t * 'a) list -> Event.key -> 'a option
(** [find_opt bindings ev] is [find bindings ev], or [None] when no binding
    matches [ev]. *)
