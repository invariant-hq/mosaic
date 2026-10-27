open Mosaic_ui

(* ───── Handlers ───── *)

type 'msg handlers = {
  on_mouse : (Event.mouse -> 'msg option) option;
  on_key : (Event.key -> 'msg option) option;
  on_paste : (Event.paste -> 'msg option) option;
  on_click : (Event.mouse -> 'msg option) option;
  on_focus_request : (unit -> 'msg) option;
  on_size : (w:int -> h:int -> 'msg option) option;
}

(* ───── Attributes ───── *)

type attrs = {
  style : Toffee.Style.t;
  visible : bool;
  z_index : int;
  opacity : float;
  focusable : bool;
  focused : bool;
  buffered : bool;
  live : bool;
}

(* ───── Virtual Nodes ───── *)

type 'msg t =
  | Element of 'msg element
  | Fragment of 'msg t list
  | Viewport_switch of 'msg viewport_switch
  | Memo of 'msg memo
  | Embed of Renderable.t
  | Themed of (Theme.t -> Theme.t) * 'msg t
  | Map : ('a -> 'msg) * 'a t -> 'msg t
  | Keyed : ('k, 'v) Keyed.t * ('k -> 'v -> 'msg t) -> 'msg t
  | Empty

and 'msg memo = { key : Obj.t; view : Obj.t; build : unit -> 'msg t }

and 'msg element =
  | E : {
      kind : ('props, 'event, 'w) Kind.t;
      props : 'props;
      on : 'event -> 'msg option;
      key : string option;
      attrs : attrs;
      handlers : 'msg handlers;
      children : 'msg t list;
    }
      -> 'msg element

and 'msg viewport_switch = {
  at_least_width : int;
  wide : 'msg t;
  narrow : 'msg t;
}

(* ───── Constructors ───── *)

let empty = Empty
let fragment children = Fragment children
let embed node = Embed node
let themed f child = Themed (f, child)

(* A memo key is compared by representation (see [Mosaic.memo]).
   Functions, objects and abstract blocks have no comparable representation
   and are rejected. A key is walked in full when its memo mounts
   ([check_memo_key]), and afterwards only where it is not, physically, the
   key it is compared with ([same_memo_key]): a block shared with the old
   key was checked with it. The rejection is the same whatever the rest of
   the key, so the first render that holds a function raises, and a key
   that changed little costs little. *)
let opaque tag =
  tag = Obj.closure_tag || tag = Obj.infix_tag || tag = Obj.object_tag
  || tag = Obj.abstract_tag || tag = Obj.cont_tag

let reject () =
  invalid_arg "Mosaic.memo: key contains a function or an abstract value"

let rec check_memo_key v =
  if Obj.is_block v then
    let tag = Obj.tag v in
    if opaque tag then reject ()
    else if tag < Obj.no_scan_tag then
      for i = 0 to Obj.size v - 1 do
        check_memo_key (Obj.field v i)
      done

let float_bits v i = Int64.bits_of_float (Obj.double_field v i)

(* Whether [v] has the representation of [old], which has been checked. It
   walks every block of [v] that is not physically the one at the same place
   in [old], whatever the result, and raises on an opaque one. *)
let rec same_memo_key old v =
  if old == v then true
  else if not (Obj.is_block v) then false
  else
    let tag = Obj.tag v and size = Obj.size v in
    if opaque tag then reject ()
    else if
      (not (Obj.is_block old)) || Obj.tag old <> tag || Obj.size old <> size
    then (
      check_memo_key v;
      false)
    else if tag = Obj.double_tag then
      Int64.equal
        (Int64.bits_of_float (Obj.obj old))
        (Int64.bits_of_float (Obj.obj v))
    else if tag = Obj.double_array_tag then
      let rec loop i =
        i = size
        || (Int64.equal (float_bits old i) (float_bits v i) && loop (i + 1))
      in
      loop 0
    else if tag = Obj.string_tag then String.equal (Obj.obj old) (Obj.obj v)
    else if tag < Obj.no_scan_tag then (
      let same = ref true in
      for i = 0 to size - 1 do
        if not (same_memo_key (Obj.field old i) (Obj.field v i)) then
          same := false
      done;
      !same)
    else compare old v = 0

let memo ~key view =
  let key = Obj.repr key in
  Memo { key; view = Obj.repr view; build = (fun () -> view (Obj.obj key)) }

let check_memo m = check_memo_key m.key

let memo_equal old m =
  let same_key = same_memo_key old.key m.key in
  old.view == m.view && same_key

let memo_view m = m.build ()

let viewport_switch ~at_least_width ~wide ~narrow =
  if at_least_width < 1 then
    invalid_arg "Mosaic.viewport_switch: at_least_width must be positive";
  Viewport_switch { at_least_width; wide; narrow }

(* ───── Elements ───── *)

(* The attributes of an element that declares none, for each default of
   [focusable], and the handlers of one that declares none. Every such
   element shares them, so the reconciler's physical comparison finds them
   unchanged. *)
let unfocusable_attrs =
  {
    style = Toffee.Style.default;
    visible = true;
    z_index = 0;
    opacity = 1.0;
    focusable = false;
    focused = false;
    buffered = false;
    live = false;
  }

let focusable_attrs = { unfocusable_attrs with focusable = true }

let no_handlers =
  {
    on_mouse = None;
    on_key = None;
    on_paste = None;
    on_click = None;
    on_focus_request = None;
    on_size = None;
  }

(* Every element's attributes and handlers. [focusable] is the kind's
   default, and an element that asks for focus is focusable whatever its
   kind. *)
let element ?key ~style ?visible ?z_index ?opacity ?focused ?buffered ?live
    ?on_mouse ?on_key ?on_paste ?on_click ?on_focus_request ?on_size ~kind
    ?(on = Kind.no_handler) props children =
  let attrs =
    match
      (visible, z_index, opacity, focused, buffered, live, on_focus_request)
    with
    | None, None, None, None, None, None, None
      when style == Toffee.Style.default ->
        if kind.Kind.focusable then focusable_attrs else unfocusable_attrs
    | _ ->
        {
          style;
          visible = Option.value visible ~default:true;
          z_index = Option.value z_index ~default:0;
          opacity = Option.value opacity ~default:1.0;
          focusable = kind.Kind.focusable || Option.is_some on_focus_request;
          focused = Option.value focused ~default:false;
          buffered = Option.value buffered ~default:false;
          live = Option.value live ~default:false;
        }
  in
  let handlers =
    match (on_mouse, on_key, on_paste, on_click, on_focus_request, on_size) with
    | None, None, None, None, None, None -> no_handlers
    | _ ->
        {
          on_mouse;
          on_key;
          on_paste;
          on_click;
          on_size;
          on_focus_request =
            Option.map (fun message () -> message) on_focus_request;
        }
  in
  Element (E { kind; props; on; key; attrs; handlers; children })

let map f vnode = Map (f, vnode)
let keyed m item = Keyed (m, item)
