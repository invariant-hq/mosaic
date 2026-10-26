(** An editable choice: a text field and a list of items to choose from.

    A {e combobox} is a text field with a list of items below it, as APG's
    combobox with list autocomplete has. The user types in the field, the
    application derives the items from the text, and the user picks one of them
    or keeps the text. A value of type ['a t] holds the field's {!Editor.t}, the
    list's {!Cursor.t} and whether the list is {e expanded} (shown). It holds no
    items: the application derives them from [Editor.text (text c)], and a
    filter that removes the cursor item keeps the user's place with
    {!Cursor.nearest}. Nothing changes until the application stores a proposed
    value.

    Keys go to the field or to the list ({!default_keys}). Editing keys change
    the text, which {!edit} installs, and a change of the text expands the list.
    Down and Up expand the list, and move its cursor once it is expanded. Enter
    commits the cursor item: it replaces the text with the item's name and
    collapses the list. Escape collapses an expanded list, and clears the text
    of a collapsed one. Tab leaves the combobox with the list collapsed and the
    text as typed. *)

type 'a t
(** The type for comboboxes over ['a] items. *)

val make :
  ?text:Editor.t -> ?cursor:'a Cursor.t -> ?expanded:bool -> unit -> 'a t
(** [make ()] is the combobox of [text], [cursor] and [expanded], which default
    to {!Editor.empty}, {!Cursor.none} and [false]. *)

val text : 'a t -> Editor.t
(** [text c] is [c]'s field. *)

val cursor : 'a t -> 'a Cursor.t
(** [cursor c] is [c]'s list cursor. *)

val expanded : 'a t -> bool
(** [expanded c] is [true] iff [c]'s list is shown. *)

val with_cursor : 'a Cursor.t -> 'a t -> 'a t
(** [with_cursor k c] is [c] with the list cursor [k]. *)

val edit : Editor.t -> 'a t -> 'a t
(** [edit e c] is [c] with the field [e], the editor an editing key proposed.
    The list is expanded when the text of [e] differs from [c]'s. *)

(** {1:steps Steps} *)

(** The type for what a key does to a combobox. *)
type action =
  | Next  (** Expand the list, or move to the next item. *)
  | Prev  (** Expand the list, or move to the previous item. *)
  | Page_down  (** Move a page down in an expanded list. *)
  | Page_up  (** Move a page up in an expanded list. *)
  | Commit  (** Choose the cursor item of an expanded list. *)
  | Dismiss  (** Collapse the list, or clear the text of a collapsed one. *)
  | Leave  (** Collapse the list without choosing. *)

(** The type for what an action does. *)
type 'a outcome =
  | Changed of 'a t  (** The next value. *)
  | Committed of 'a * 'a t  (** The item chosen, with the next value. *)
  | Nothing  (** The action changes nothing. *)

val step :
  equal:('a -> 'a -> bool) ->
  name:('a -> string) ->
  page:int ->
  'a Items.t ->
  'a t ->
  action ->
  'a outcome
(** [step ~equal ~name ~page items c a] is what [a] does to [c], whose list
    shows [items], [page] of them at a time.

    On a collapsed list, [Next] and [Prev] expand it, and put its cursor on the
    first item or the last when [items] lacks the cursor item. On an expanded
    list they, [Page_down] and [Page_up] move the cursor as {!Cursor.step} does;
    on a collapsed one, [Page_down] and [Page_up] give [Nothing]. [Commit] on an
    expanded list with a cursor item [x] in [items] gives [Committed (x, c')],
    where [c'] holds the text [name x], as an undo step, with the caret at its
    end, and a collapsed list; otherwise it gives [Nothing]. [Dismiss] collapses
    an expanded list, clears the text of a collapsed one, as an undo step, and
    gives [Nothing] on an empty collapsed one. [Leave] collapses an expanded
    list and gives [Nothing] on a collapsed one. *)

(** {1:keys Keys} *)

val default_keys : (Shortcut.t * action) list
(** [default_keys] binds Down to [Next], Up to [Prev], PageDown to [Page_down],
    PageUp to [Page_up], Enter to [Commit], Escape to [Dismiss], and Tab and
    Shift+Tab to [Leave]. A combobox passes Tab on after [Leave], so focus
    moves. *)
