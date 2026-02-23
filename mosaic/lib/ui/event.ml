(* {1 Dispatch control} *)

(* Shared propagation/default-prevention state used by Key, Paste, and Mouse. *)
module Dispatch_control = struct
  type t = {
    mutable propagation_stopped : bool;
    mutable default_prevented : bool;
  }

  let create () = { propagation_stopped = false; default_prevented = false }
  let stop_propagation t = t.propagation_stopped <- true
  let propagation_stopped t = t.propagation_stopped
  let prevent_default t = t.default_prevented <- true
  let default_prevented t = t.default_prevented
end

(* {1 Keyboard events} *)

module Key = struct
  type t = { data : Input.Key.event; ctl : Dispatch_control.t }

  let of_input data = { data; ctl = Dispatch_control.create () }
  let data t = t.data
  let stop_propagation t = Dispatch_control.stop_propagation t.ctl
  let propagation_stopped t = Dispatch_control.propagation_stopped t.ctl
  let prevent_default t = Dispatch_control.prevent_default t.ctl
  let default_prevented t = Dispatch_control.default_prevented t.ctl
  let equal a b = Input.Key.equal_event a.data b.data
  let pp ppf t = Input.Key.pp_event ppf t.data
end

(* {1 Paste events} *)

module Paste = struct
  type t = { text : string; ctl : Dispatch_control.t }

  let of_text text = { text; ctl = Dispatch_control.create () }
  let text t = t.text
  let stop_propagation t = Dispatch_control.stop_propagation t.ctl
  let propagation_stopped t = Dispatch_control.propagation_stopped t.ctl
  let prevent_default t = Dispatch_control.prevent_default t.ctl
  let default_prevented t = Dispatch_control.default_prevented t.ctl
  let equal a b = String.equal a.text b.text
  let pp ppf t = Format.fprintf ppf "Paste(%S)" t.text
end

(* {1 Mouse events} *)

module Mouse = struct
  type button = Left | Middle | Right | Button of int

  let equal_button a b =
    match (a, b) with
    | Left, Left | Middle, Middle | Right, Right -> true
    | Button a, Button b -> Int.equal a b
    | _ -> false

  let pp_button ppf = function
    | Left -> Format.pp_print_string ppf "Left"
    | Middle -> Format.pp_print_string ppf "Middle"
    | Right -> Format.pp_print_string ppf "Right"
    | Button n -> Format.fprintf ppf "Button(%d)" n

  type modifier = Input.Key.modifier = {
    ctrl : bool;
    alt : bool;
    shift : bool;
    super : bool;
    hyper : bool;
    meta : bool;
    caps_lock : bool;
    num_lock : bool;
  }

  let no_modifier = Input.Key.no_modifier
  let equal_modifier = Input.Key.equal_modifier
  let pp_modifier = Input.Key.pp_modifier

  type scroll_direction = Input.Mouse.scroll_direction =
    | Scroll_up
    | Scroll_down
    | Scroll_left
    | Scroll_right

  let equal_scroll_direction = Input.Mouse.equal_scroll_direction
  let pp_scroll_direction = Input.Mouse.pp_scroll_direction

  type kind =
    | Down of { button : button }
    | Up of { button : button; is_dragging : bool }
    | Move
    | Drag of { button : button; is_dragging : bool }
    | Drag_end of { button : button }
    | Drop of { button : button; source : int option }
    | Over of { source : int option }
    | Out
    | Scroll of { direction : scroll_direction; delta : int }

  let pp_button_drag ppf name button is_dragging =
    if is_dragging then
      Format.fprintf ppf "%s(%a, dragging)" name pp_button button
    else Format.fprintf ppf "%s(%a)" name pp_button button

  let pp_kind ppf = function
    | Down { button } -> Format.fprintf ppf "Down(%a)" pp_button button
    | Up { button; is_dragging } -> pp_button_drag ppf "Up" button is_dragging
    | Move -> Format.pp_print_string ppf "Move"
    | Drag { button; is_dragging } ->
        pp_button_drag ppf "Drag" button is_dragging
    | Drag_end { button } -> Format.fprintf ppf "Drag_end(%a)" pp_button button
    | Drop { button; source = Some s } ->
        Format.fprintf ppf "Drop(%a, source=%d)" pp_button button s
    | Drop { button; source = None } ->
        Format.fprintf ppf "Drop(%a)" pp_button button
    | Over { source = Some s } -> Format.fprintf ppf "Over(source=%d)" s
    | Over { source = None } -> Format.pp_print_string ppf "Over"
    | Out -> Format.pp_print_string ppf "Out"
    | Scroll { direction; delta } ->
        Format.fprintf ppf "Scroll(%a, %d)" Input.Mouse.pp_scroll_direction
          direction delta

  let equal_kind a b =
    match (a, b) with
    | Down a, Down b -> equal_button a.button b.button
    | Up a, Up b ->
        equal_button a.button b.button && Bool.equal a.is_dragging b.is_dragging
    | Move, Move -> true
    | Drag a, Drag b ->
        equal_button a.button b.button && Bool.equal a.is_dragging b.is_dragging
    | Drag_end a, Drag_end b -> equal_button a.button b.button
    | Drop a, Drop b ->
        equal_button a.button b.button
        && Option.equal Int.equal a.source b.source
    | Over a, Over b -> Option.equal Int.equal a.source b.source
    | Out, Out -> true
    | Scroll a, Scroll b ->
        equal_scroll_direction a.direction b.direction
        && Int.equal a.delta b.delta
    | _ -> false

  type t = {
    kind : kind;
    x : int;
    y : int;
    modifiers : modifier;
    target : int option;
    ctl : Dispatch_control.t;
  }

  let make ~x ~y ~modifiers ?target kind =
    { kind; x; y; modifiers; target; ctl = Dispatch_control.create () }

  let kind t = t.kind
  let x t = t.x
  let y t = t.y
  let modifiers t = t.modifiers
  let target t = t.target
  let stop_propagation t = Dispatch_control.stop_propagation t.ctl
  let propagation_stopped t = Dispatch_control.propagation_stopped t.ctl
  let prevent_default t = Dispatch_control.prevent_default t.ctl
  let default_prevented t = Dispatch_control.default_prevented t.ctl

  let equal a b =
    equal_kind a.kind b.kind && Int.equal a.x b.x && Int.equal a.y b.y
    && equal_modifier a.modifiers b.modifiers
    && Option.equal Int.equal a.target b.target

  let pp ppf t = Format.fprintf ppf "%a(%d, %d)" pp_kind t.kind t.x t.y
end

(* {1 Type aliases} *)

type key = Key.t
type paste = Paste.t
type mouse = Mouse.t
