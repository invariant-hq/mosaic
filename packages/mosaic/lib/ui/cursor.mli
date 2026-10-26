(** A collection's cursor: the item its keys act on, where it is scrolled, and
    where a range of items starts.

    A cursor value is the state of a collection of {!Items.t} that an
    application keeps in its model. A key gives an {!action}, and {!step}
    computes what the action does from the stored value: the next value, an item
    to activate, or a change to the items chosen. Nothing moves until the
    application stores the next value.

    The {e cursor item} is an item, compared with the collection's [equal], so
    reordering or filtering the items never moves the cursor to another item. A
    value also records its {e hint}, the position where its item was last seen.
    Finding the item costs one comparison while the items do not move, and one
    pass over them after they move. A cursor item absent from the items shows no
    cursor, and a key then acts as from no cursor item. An update that removes
    items keeps the user's place with {!nearest}.

    A value holds the collection's {!Scroll.t}, counted in items, so line [i] is
    the item at position [i]. A step that moves the cursor reveals the new
    cursor item, and {!with_scroll} scrolls without moving it.

    The {e anchor} is the item a range of items starts from, for a collection
    whose items can be chosen. The application keeps which items are chosen, and
    a step proposes a {!change} to them. The {e typed prefix} is what typeahead
    has matched so far.

    {!default_keys} binds the keys of a list box to actions, and
    {!wrapping_keys} binds them with moves that wrap. *)

(** {1:values Values} *)

type 'a t
(** The type for cursors over ['a] items. *)

val none : 'a t
(** [none] has no cursor item, no anchor, and is scrolled to the first item. *)

val at : 'a -> 'a t
(** [at x] is {!none} with [x] as its cursor item. It stays scrolled to the
    first item until a step moves it. *)

val item : 'a t -> 'a option
(** [item c] is [c]'s cursor item, if any. *)

val scroll : 'a t -> Scroll.t
(** [scroll c] is [c]'s scroll, in items. *)

val anchor : 'a t -> 'a option
(** [anchor c] is [c]'s anchor, if any. *)

val with_item : 'a option -> 'a t -> 'a t
(** [with_item x c] is [c] with [x] as its cursor item. *)

val with_scroll : Scroll.t -> 'a t -> 'a t
(** [with_scroll s c] is [c] scrolled to [s]. *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** [equal eq c c'] is [true] iff [c] and [c'] have equal cursor items and
    anchors under [eq], equal scrolls and the same typed prefix. Hints are not
    compared. *)

val index : equal:('a -> 'a -> bool) -> 'a Items.t -> 'a t -> int option
(** [index ~equal items c] is the position of [c]'s cursor item in [items], if
    it is there: the hint when the item there is equal, else the first equal
    item's. *)

(** {1:steps Steps} *)

(** The type for changes to the items an application chose. [Add] and [Remove]
    carry every item they concern, so applying one needs no knowledge of the
    items' order. *)
type 'a change =
  | Toggle of 'a  (** Choose the item if it is not chosen, else unchoose it. *)
  | Add of 'a list  (** Choose every item of the list. *)
  | Remove of 'a list  (** Unchoose every item of the list. *)

(** The type for what a key does to a collection. *)
type action =
  | Next  (** The next item; nothing at the last. *)
  | Prev  (** The previous item; nothing at the first. *)
  | Next_wrap  (** The next item; the first after the last. *)
  | Prev_wrap  (** The previous item; the last before the first. *)
  | First  (** The first item. *)
  | Last  (** The last item. *)
  | By of int  (** [n] items on, or back when [n] is negative, clamped. *)
  | Page_down  (** [By (page - 1)], at least one item. *)
  | Page_up  (** [By (1 - page)], at least one item. *)
  | Typeahead of Uchar.t  (** Type a character of an item's name. *)
  | Activate  (** Activate the cursor item. *)
  | Toggle  (** Toggle whether the cursor item is chosen. *)
  | Extend_next  (** Extend the range to the next item. *)
  | Extend_prev  (** Extend the range to the previous item. *)
  | Select_all  (** Choose every item. *)

(** The type for what an action does. *)
type 'a outcome =
  | Moved of 'a t  (** The next value. *)
  | Activated of 'a  (** The item to activate. *)
  | Changed of 'a change * 'a t
      (** A change to the chosen items, with the next value. *)
  | Nothing  (** The action changes nothing. *)

val step :
  equal:('a -> 'a -> bool) ->
  name:('a -> string) ->
  page:int ->
  'a Items.t ->
  'a t ->
  action ->
  'a outcome
(** [step ~equal ~name ~page items c a] is what [a] does from [c] over [items],
    where [page] is the number of items the collection shows. A move to a value
    equal to [c] gives [Nothing].

    A move gives the cursor item it lands on, reveals it and clears the anchor
    and the typed prefix. From no cursor item, [Next], [Next_wrap], [First],
    [By n] with [n > 0] and [Page_down] land on the first item, and [Prev],
    [Prev_wrap], [Last], [By n] with [n < 0] and [Page_up] on the last. [By 0]
    and every action over no items give [Nothing].

    [Typeahead u] adds [u] to the typed prefix and lands on the first item, from
    the cursor item on, whose [name] starts with the prefix, wrapping after the
    last item. While the typed prefix is empty the search starts after the
    cursor item, so typing one character again moves to the next item it starts.
    When no name starts with the extended prefix, the prefix restarts at [u],
    searched from after the cursor item. When none starts with [u] either, it
    gives [Nothing]. Names are compared byte by byte with ASCII letters folded
    to lower case. It costs the distance to the item it lands on, or a pass over
    the items when none matches.

    [Activate] gives [Activated] of the cursor item. [Toggle] gives
    [Changed (Toggle x, c')], where [x] is the cursor item and [c'] has it as
    anchor. [Extend_next] and [Extend_prev] move the cursor as [Next] and [Prev]
    do and give the change to the {e range}, the items from the anchor to the
    cursor item: [Add] of the item the range gains, or [Remove] of the one it
    loses. With no anchor present in [items], the anchor becomes the cursor item
    and the change adds it too; from no cursor item they add the item they land
    on, which becomes the anchor. [Select_all] gives [Add] of every item and
    costs their number. Over no cursor item, [Activate], [Toggle] give
    [Nothing], as do [Extend_next] at the last item and [Extend_prev] at the
    first. *)

val nearest :
  equal:('a -> 'a -> bool) -> before:'a Items.t -> 'a Items.t -> 'a t -> 'a t
(** [nearest ~equal ~before items c] is [c] on the cursor item that keeps the
    user's place when the items [before] become [items]: [c]'s own cursor item
    if [items] holds it, else the first item of [items] that followed it in
    [before], else the last that preceded it, else no cursor item. The cursor
    item is revealed.

    [items] must keep the items it shares with [before] in their order in
    [before], as removing and filtering do. It then costs a pass over each, and
    a pass over [before] for each item of [items] absent from [before].
    Otherwise the cursor lands on some item of [items] or on none. *)

(** {1:keys Keys} *)

val default_keys : (Shortcut.t * action) list
(** [default_keys] binds Down to [Next], Up to [Prev], Home to [First], End to
    [Last], PageDown to [Page_down], PageUp to [Page_up], Enter and Ctrl+J to
    [Activate], Space to [Toggle], Shift+Down to [Extend_next], Shift+Up to
    [Extend_prev] and Ctrl+A to [Select_all]. *)

val wrapping_keys : (Shortcut.t * action) list
(** [wrapping_keys] is {!default_keys} with Down bound to [Next_wrap] and Up to
    [Prev_wrap]. *)
