(* ───── Item ───── *)

type item = { id : string; label : string; children : item list }

let item ~id ?(children = []) label = { id; label; children }

let rec item_equal a b =
  String.equal a.id b.id
  && String.equal a.label b.label
  && items_equal a.children b.children

and items_equal xs ys =
  xs == ys
  ||
  match (xs, ys) with
  | [], [] -> true
  | x :: xs, y :: ys -> item_equal x y && items_equal xs ys
  | _ -> false

module State = struct
  module Ids = Set.Make (String)

  (* [prefix] is the typed prefix, folded to lower case. Only [step] sets
     it; the widget's own keys leave it empty. *)
  type t = { selected : string option; expanded : Ids.t; prefix : string }

  let empty = { selected = None; expanded = Ids.empty; prefix = "" }

  let make ?selected ?(expanded = []) () =
    { selected; expanded = Ids.of_list expanded; prefix = "" }

  let selected t = t.selected
  let expanded t = Ids.elements t.expanded

  let select selected t =
    if Option.equal String.equal selected t.selected then t
    else { t with selected }

  let set_expanded id expanded t =
    let expanded =
      if expanded then Ids.add id t.expanded else Ids.remove id t.expanded
    in
    if expanded == t.expanded then t else { t with expanded }

  let is_expanded id t = Ids.mem id t.expanded

  let equal a b =
    a == b
    || Option.equal String.equal a.selected b.selected
       && (a.expanded == b.expanded || Ids.equal a.expanded b.expanded)
       && String.equal a.prefix b.prefix

  (* ───── Steps ───── *)

  type action =
    | Next
    | Prev
    | Next_wrap
    | Prev_wrap
    | First
    | Last
    | By of int
    | Expand
    | Collapse
    | Expand_siblings
    | Toggle
    | Activate
    | Typeahead of Uchar.t

  type outcome = Changed of t | Activated of item | Nothing

  (* A shown node: its item, and the position of its parent among the shown
     nodes, or -1 for a root. *)
  type shown = { node : item; parent : int }

  (* The nodes [t] shows, in order: every root, and the children of every
     shown node that is expanded. *)
  let shown items t =
    let acc = ref [] in
    let count = ref 0 in
    let rec walk parent items =
      List.iter
        (fun node ->
          let here = !count in
          acc := { node; parent } :: !acc;
          incr count;
          if node.children <> [] && Ids.mem node.id t.expanded then
            walk here node.children)
        items
    in
    walk (-1) items;
    Array.of_list (List.rev !acc)

  (* The nodes from a root down to the node [id], if the tree holds it. *)
  let rec path_to id = function
    | [] -> None
    | node :: rest -> (
        if String.equal node.id id then Some [ node ]
        else
          match path_to id node.children with
          | Some path -> Some (node :: path)
          | None -> path_to id rest)

  (* The node the keys act on: the selected node, or its nearest shown
     ancestor when it is inside a collapsed node. *)
  let acting items t =
    match t.selected with
    | None -> None
    | Some id -> (
        match path_to id items with
        | None -> None
        | Some path ->
            let rec first_shown = function
              | [] -> None
              | [ node ] -> Some node.id
              | node :: rest ->
                  if Ids.mem node.id t.expanded then first_shown rest
                  else Some node.id
            in
            first_shown path)

  let position shown id =
    let rec go i =
      if i = Array.length shown then -1
      else if String.equal shown.(i).node.id id then i
      else go (i + 1)
    in
    go 0

  let on t (node : item) = { t with selected = Some node.id; prefix = "" }

  let put shown t i =
    let next = on t shown.(i).node in
    if Option.equal String.equal next.selected t.selected && t.prefix = "" then
      Nothing
    else Changed next

  (* Whether [s] starts with [p], ASCII letters of [s] folded to lower case;
     [p] is folded already. *)
  let rec prefix_from p s lp i =
    i = lp
    || Char.equal
         (Char.lowercase_ascii (String.unsafe_get s i))
         (String.unsafe_get p i)
       && prefix_from p s lp (i + 1)

  let starts_with p s =
    let lp = String.length p in
    String.length s >= lp && prefix_from p s lp 0

  let rec match_from shown p n start k =
    if k = n then -1
    else
      let i = (start + k) mod n in
      if starts_with p shown.(i).node.label then i
      else match_from shown p n start (k + 1)

  let typeahead shown t ~current u =
    let n = Array.length shown in
    let typed =
      let b = Buffer.create 4 in
      Buffer.add_utf_8_uchar b u;
      String.lowercase_ascii (Buffer.contents b)
    in
    let prefix = t.prefix ^ typed in
    let after = if current < 0 then 0 else (current + 1) mod n in
    let start = if t.prefix = "" || current < 0 then after else current in
    let i, prefix =
      match match_from shown prefix n start 0 with
      | -1 when t.prefix <> "" -> (match_from shown typed n after 0, typed)
      | i -> (i, prefix)
    in
    if i < 0 then Nothing
    else
      let next = { (on t shown.(i).node) with prefix } in
      if
        Option.equal String.equal next.selected t.selected
        && String.equal prefix t.prefix
      then Nothing
      else Changed next

  let step items t action =
    let shown = shown items t in
    let n = Array.length shown in
    if n = 0 then Nothing
    else
      let current =
        match acting items t with None -> -1 | Some id -> position shown id
      in
      let by k =
        if k = 0 then Nothing
        else if current < 0 then put shown t (if k > 0 then 0 else n - 1)
        else put shown t (max 0 (min (n - 1) (current + k)))
      in
      let expandable i = shown.(i).node.children <> [] in
      let expanded i = expandable i && Ids.mem shown.(i).node.id t.expanded in
      let set_expanded i v =
        let node = shown.(i).node in
        let t = on t node in
        let expanded =
          if v then Ids.add node.id t.expanded
          else Ids.remove node.id t.expanded
        in
        Changed { t with expanded }
      in
      match action with
      | Next -> if current = n - 1 then Nothing else by 1
      | Prev -> if current = 0 then Nothing else by (-1)
      | Next_wrap ->
          put shown t (if current < 0 then 0 else (current + 1) mod n)
      | Prev_wrap -> put shown t (if current <= 0 then n - 1 else current - 1)
      | First -> put shown t 0
      | Last -> put shown t (n - 1)
      | By k -> by k
      | Typeahead u -> typeahead shown t ~current u
      | _ when current < 0 -> Nothing
      | Expand ->
          if expanded current then put shown t (current + 1)
          else if expandable current then set_expanded current true
          else Nothing
      | Collapse ->
          if expanded current then set_expanded current false
          else if shown.(current).parent >= 0 then
            put shown t shown.(current).parent
          else Nothing
      | Toggle ->
          if expandable current then
            set_expanded current (not (expanded current))
          else Nothing
      | Activate -> Activated shown.(current).node
      | Expand_siblings ->
          let parent = shown.(current).parent in
          let siblings =
            if parent < 0 then items else shown.(parent).node.children
          in
          let expanded =
            List.fold_left
              (fun set (node : item) ->
                if node.children = [] then set else Ids.add node.id set)
              t.expanded siblings
          in
          if Ids.equal expanded t.expanded then Nothing
          else Changed { (on t shown.(current).node) with expanded }
end

(* ───── Props ───── *)

type action =
  | Move_up
  | Move_down
  | Move_up_fast
  | Move_down_fast
  | Expand
  | Collapse
  | Toggle
  | Activate

type key_binding = Shortcut.t * action

let default_bindings =
  Shortcut.
    [
      (up, Move_up);
      (down, Move_down);
      (char 'k', Move_up);
      (char 'j', Move_down);
      (key ~shift:true Matrix_input.Keystroke.Up, Move_up_fast);
      (key ~shift:true Matrix_input.Keystroke.Down, Move_down_fast);
      (right, Expand);
      (left, Collapse);
      (space, Toggle);
      (enter, Activate);
      (ctrl 'j', Activate);
    ]

let default_keymap = Keymap.make ~defaults:default_bindings ()

let keymap = function
  | [] -> default_keymap
  | custom -> Keymap.make ~defaults:default_bindings ~custom ()

(* [Props.make] is the one place the tree's labels get their
   defaults: [create] and [Vnode.tree] pass their labels on unset. *)
module Props = struct
  type t = {
    key_bindings : key_binding list;
    items : item list;
    state : State.t;
    indent_size : int;
    show_guides : bool;
    guide_style : Matrix_grid.Border.t;
    expand_icon : string;
    collapse_icon : string;
    leaf_icon : string;
    background : Ansi.Color.t;
    text_color : Ansi.Color.t;
    selected_background : Ansi.Color.t;
    selected_text_color : Ansi.Color.t;
    focused_selected_background : Ansi.Color.t;
    focused_selected_text_color : Ansi.Color.t;
    guide_color : Ansi.Color.t;
    icon_color : Ansi.Color.t;
    wrap_selection : bool;
    fast_scroll_step : int;
  }

  let make ?(items = []) ?(state = State.empty) ?(indent_size = 2)
      ?(show_guides = false) ?(guide_style = Matrix_grid.Border.single)
      ?(expand_icon = "\xe2\x96\xb6") ?(collapse_icon = "\xe2\x96\xbc")
      ?(leaf_icon = " ") ?(background = Ansi.Color.of_rgba 0 0 0 0)
      ?(text_color = Ansi.Color.of_rgb 255 255 255)
      ?(selected_background = Ansi.Color.of_rgb 51 68 85)
      ?(selected_text_color = Ansi.Color.of_rgb 255 255 0)
      ?focused_selected_background ?focused_selected_text_color
      ?(guide_color = Ansi.Color.of_rgb 102 102 102)
      ?(icon_color = Ansi.Color.of_rgb 153 153 153) ?(wrap_selection = false)
      ?(key_bindings = []) ?(fast_scroll_step = 5) () =
    let focused_selected_background =
      match focused_selected_background with
      | Some c -> c
      | None -> selected_background
    in
    let focused_selected_text_color =
      match focused_selected_text_color with
      | Some c -> c
      | None -> selected_text_color
    in
    {
      key_bindings;
      items;
      state;
      indent_size = max 1 indent_size;
      show_guides;
      guide_style;
      expand_icon;
      collapse_icon;
      leaf_icon;
      background;
      text_color;
      selected_background;
      selected_text_color;
      focused_selected_background;
      focused_selected_text_color;
      guide_color;
      icon_color;
      wrap_selection;
      fast_scroll_step = max 1 fast_scroll_step;
    }

  let default = make ()

  let equal a b =
    List.equal (Keymap.binding_equal ( = )) a.key_bindings b.key_bindings
    && items_equal a.items b.items
    && State.equal a.state b.state
    && Int.equal a.indent_size b.indent_size
    && Bool.equal a.show_guides b.show_guides
    && a.guide_style = b.guide_style
    && String.equal a.expand_icon b.expand_icon
    && String.equal a.collapse_icon b.collapse_icon
    && String.equal a.leaf_icon b.leaf_icon
    && Ansi.Color.equal a.background b.background
    && Ansi.Color.equal a.text_color b.text_color
    && Ansi.Color.equal a.selected_background b.selected_background
    && Ansi.Color.equal a.selected_text_color b.selected_text_color
    && Ansi.Color.equal a.focused_selected_background
         b.focused_selected_background
    && Ansi.Color.equal a.focused_selected_text_color
         b.focused_selected_text_color
    && Ansi.Color.equal a.guide_color b.guide_color
    && Ansi.Color.equal a.icon_color b.icon_color
    && Bool.equal a.wrap_selection b.wrap_selection
    && Int.equal a.fast_scroll_step b.fast_scroll_step
end

(* ───── Visible Entry ───── *)

type visible_entry = {
  item : item;
  depth : int;
  expandable : bool;
  expanded : bool;
  is_last_child : bool;
  parent_guides : bool list;
}

(* ───── Types ───── *)

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  mutable keymap : action Keymap.t;
  mutable visible : visible_entry array;
  mutable selected_index : int;
  mutable on_change : (State.t -> unit) option;
  mutable on_activate : (item -> unit) option;
}

let node t = t.node

(* ───── Internal Helpers ───── *)

let request t = Renderable.request_render t.node
let visible_count t = Array.length t.visible

let clamp_index t idx =
  let len = visible_count t in
  if len = 0 then 0 else max 0 (min (len - 1) idx)

(* ───── Visible List Computation ───── *)

let update_selected_index t =
  t.selected_index <- -1;
  match State.selected t.props.state with
  | None -> ()
  | Some id ->
      let i = ref 0 in
      while !i < visible_count t && t.selected_index < 0 do
        if String.equal t.visible.(!i).item.id id then t.selected_index <- !i;
        incr i
      done

let recompute_visible t =
  let buf = ref [||] in
  let count = ref 0 in
  let add entry =
    if !count >= Array.length !buf then begin
      let old = !buf in
      let new_len = max 16 (Array.length old * 2) in
      let arr = Array.make new_len entry in
      Array.blit old 0 arr 0 (Array.length old);
      buf := arr
    end;
    !buf.(!count) <- entry;
    incr count
  in
  let rec walk items depth parent_guides =
    let n = List.length items in
    List.iteri
      (fun i it ->
        let expandable = it.children <> [] in
        let expanded = expandable && State.is_expanded it.id t.props.state in
        let is_last = i = n - 1 in
        add
          {
            item = it;
            depth;
            expandable;
            expanded;
            is_last_child = is_last;
            parent_guides;
          };
        if expanded then
          let guide_continues = not is_last in
          walk it.children (depth + 1) (guide_continues :: parent_guides))
      items
  in
  walk t.props.items 0 [];
  t.visible <- Array.sub !buf 0 !count;
  update_selected_index t

(* ───── Scrolling ───── *)

let max_visible t = max 1 (Renderable.height t.node)

(* The first visible row. It centers the selected row when possible, clamped so
   that it never goes negative or past the last page. It is a function of the
   accepted state, the items and the height, never stored, so a remounted tree
   shows the same rows and only an accepted selection scrolls. *)
let scroll_offset t =
  let max_visible = max_visible t in
  let max_off = max 0 (visible_count t - max_visible) in
  max 0 (min (t.selected_index - (max_visible / 2)) max_off)

let install_state t items state =
  let previous = t.props.state in
  let structure_changed =
    (not (items_equal t.props.items items))
    || not
         (previous.expanded == state.State.expanded
         || State.Ids.equal previous.expanded state.expanded)
  in
  if
    structure_changed
    || not (Option.equal String.equal previous.selected state.selected)
  then begin
    t.props <- { t.props with items; state };
    if structure_changed then recompute_visible t else update_selected_index t;
    request t
  end

let propose_selected_index t idx =
  let len = visible_count t in
  if len = 0 then ()
  else
    let idx = clamp_index t idx in
    if idx <> t.selected_index then
      Option.iter
        (fun f -> f (State.select (Some t.visible.(idx).item.id) t.props.state))
        t.on_change

(* ───── Public Accessors ───── *)

let items t = t.props.items
let selected_index t = t.selected_index

let selected_item t =
  let len = visible_count t in
  if len = 0 || t.selected_index < 0 then None
  else Some t.visible.(t.selected_index).item

let depth_of t i =
  if i < 0 || i >= visible_count t then 0 else t.visible.(i).depth

(* ───── Data ───── *)

let set_items t items = install_state t items t.props.state

(* ───── Selection ───── *)

let set_selected_index t idx =
  if visible_count t > 0 then
    let idx = clamp_index t idx in
    if idx <> t.selected_index then
      install_state t t.props.items
        (State.select (Some t.visible.(idx).item.id) t.props.state)

(* ───── Expansion ───── *)

let expansion_state t i expanded =
  let state = t.props.state in
  if i < 0 || i >= visible_count t then state
  else
    let entry = t.visible.(i) in
    if (not entry.expandable) || entry.expanded = expanded then state
    else
      let state = State.set_expanded entry.item.id expanded state in
      if (not expanded) && t.selected_index > i then
        let rec contains id item =
          String.equal item.id id || List.exists (contains id) item.children
        in
        match State.selected state with
        | Some id when contains id entry.item ->
            State.select (Some entry.item.id) state
        | _ -> state
      else state

let propose_expansion t i expanded =
  let state = expansion_state t i expanded in
  if state != t.props.state then Option.iter (fun f -> f state) t.on_change

let expand t i = install_state t t.props.items (expansion_state t i true)
let collapse t i = install_state t t.props.items (expansion_state t i false)

let toggle_expand t i =
  if i >= 0 && i < visible_count t then
    install_state t t.props.items
      (expansion_state t i (not t.visible.(i).expanded))

let expand_all t =
  let rec walk state items =
    List.fold_left
      (fun state item ->
        if item.children = [] then state
        else walk (State.set_expanded item.id true state) item.children)
      state items
  in
  install_state t t.props.items (walk t.props.state t.props.items)

let collapse_all t =
  install_state t t.props.items
    (State.make ?selected:(State.selected t.props.state) ())

let is_expanded t i =
  if i >= 0 && i < visible_count t then t.visible.(i).expanded else false

(* ───── Navigation ───── *)

let move_up ?(steps = 1) t =
  let len = visible_count t in
  if len = 0 then ()
  else
    let new_index = t.selected_index - steps in
    if new_index >= 0 then propose_selected_index t new_index
    else if t.props.wrap_selection then propose_selected_index t (len - 1)
    else propose_selected_index t 0

let move_down ?(steps = 1) t =
  let len = visible_count t in
  if len = 0 then ()
  else
    let new_index = t.selected_index + steps in
    if new_index < len then propose_selected_index t new_index
    else if t.props.wrap_selection then propose_selected_index t 0
    else propose_selected_index t (len - 1)

let find_parent_index t i =
  if i < 0 || i >= visible_count t then None
  else
    let entry = t.visible.(i) in
    if entry.depth = 0 then None
    else
      let target_depth = entry.depth - 1 in
      let rec scan j =
        if j < 0 then None
        else if t.visible.(j).depth = target_depth then Some j
        else scan (j - 1)
      in
      scan (i - 1)

(* ───── Display Setters ───── *)

let set_indent_size t n =
  let n = max 1 n in
  if t.props.indent_size <> n then (
    t.props <- { t.props with indent_size = n };
    request t)

let set_show_guides t v =
  if t.props.show_guides <> v then (
    t.props <- { t.props with show_guides = v };
    request t)

let set_guide_style t s =
  if t.props.guide_style <> s then (
    t.props <- { t.props with guide_style = s };
    request t)

let set_expand_icon t s =
  if not (String.equal t.props.expand_icon s) then (
    t.props <- { t.props with expand_icon = s };
    request t)

let set_collapse_icon t s =
  if not (String.equal t.props.collapse_icon s) then (
    t.props <- { t.props with collapse_icon = s };
    request t)

let set_leaf_icon t s =
  if not (String.equal t.props.leaf_icon s) then (
    t.props <- { t.props with leaf_icon = s };
    request t)

(* ───── Color Setters ───── *)

let set_background t c =
  if not (Ansi.Color.equal t.props.background c) then (
    t.props <- { t.props with background = c };
    request t)

let set_text_color t c =
  if not (Ansi.Color.equal t.props.text_color c) then (
    t.props <- { t.props with text_color = c };
    request t)

let set_selected_background t c =
  if not (Ansi.Color.equal t.props.selected_background c) then (
    t.props <- { t.props with selected_background = c };
    request t)

let set_selected_text_color t c =
  if not (Ansi.Color.equal t.props.selected_text_color c) then (
    t.props <- { t.props with selected_text_color = c };
    request t)

let set_focused_selected_background t c =
  if not (Ansi.Color.equal t.props.focused_selected_background c) then (
    t.props <- { t.props with focused_selected_background = c };
    request t)

let set_focused_selected_text_color t c =
  if not (Ansi.Color.equal t.props.focused_selected_text_color c) then (
    t.props <- { t.props with focused_selected_text_color = c };
    request t)

let set_guide_color t c =
  if not (Ansi.Color.equal t.props.guide_color c) then (
    t.props <- { t.props with guide_color = c };
    request t)

let set_icon_color t c =
  if not (Ansi.Color.equal t.props.icon_color c) then (
    t.props <- { t.props with icon_color = c };
    request t)

(* ───── Behavior Setters ───── *)

let set_wrap_selection t v =
  if t.props.wrap_selection <> v then (
    t.props <- { t.props with wrap_selection = v };
    request t)

let set_fast_scroll_step t n =
  let n = max 1 n in
  if t.props.fast_scroll_step <> n then (
    t.props <- { t.props with fast_scroll_step = n };
    request t)

(* ───── Callbacks ───── *)

let set_on_change t cb = t.on_change <- cb
let set_on_activate t cb = t.on_activate <- cb

(* ───── Rendering Helpers ───── *)

let draw_cell grid ~x ~y ~fg ~bg uch =
  Matrix_grid.set_uchar grid ~x ~y ~uchar:uch ~fg ~bg ~attrs:Ansi.Attr.empty ()

(* ───── Key Handling ───── *)

let handle_key t (event : Event.key) =
  match Keymap.action t.keymap (Event.Key.data event) with
  | None -> ()
  | Some action -> (
      Event.Key.prevent_default event;
      match action with
      | Move_up -> move_up t
      | Move_down -> move_down t
      | Move_up_fast -> move_up ~steps:t.props.fast_scroll_step t
      | Move_down_fast -> move_down ~steps:t.props.fast_scroll_step t
      | Expand ->
          if t.selected_index >= 0 && visible_count t > 0 then
            let entry = t.visible.(t.selected_index) in
            if entry.expandable && not entry.expanded then
              propose_expansion t t.selected_index true
            else if entry.expanded then
              let next = t.selected_index + 1 in
              if next < visible_count t then propose_selected_index t next
      | Collapse -> (
          if t.selected_index >= 0 && visible_count t > 0 then
            let entry = t.visible.(t.selected_index) in
            if entry.expandable && entry.expanded then
              propose_expansion t t.selected_index false
            else
              match find_parent_index t t.selected_index with
              | Some index -> propose_selected_index t index
              | None -> ())
      | Toggle ->
          if t.selected_index >= 0 && visible_count t > 0 then
            propose_expansion t t.selected_index
              (not t.visible.(t.selected_index).expanded)
      | Activate ->
          if t.selected_index >= 0 && visible_count t > 0 then
            Option.iter
              (fun f -> f t.visible.(t.selected_index).item)
              t.on_activate)

(* ───── Mouse Handling ───── *)

let handle_mouse t (event : Event.mouse) =
  let width = Renderable.width t.node in
  let height = Renderable.height t.node in
  let x = Event.Mouse.x event - Renderable.x t.node in
  let y = Event.Mouse.y event - Renderable.y t.node in
  match Event.Mouse.kind event with
  | Down { button = Left } ->
      if x >= 0 && x < width && y >= 0 && y < height then
        let index = scroll_offset t + y in
        if index >= 0 && index < visible_count t then (
          let entry = t.visible.(index) in
          let indent = entry.depth * t.props.indent_size in
          (* Icon occupies columns [indent, indent+2) *)
          if x >= indent && x < indent + 2 && entry.expandable then
            propose_expansion t index (not entry.expanded)
          else propose_selected_index t index;
          Event.Mouse.stop_propagation event)
  | Scroll { direction; delta } -> (
      match direction with
      | Event.Mouse.Scroll_up when delta > 0 ->
          move_up t;
          Event.Mouse.stop_propagation event
      | Event.Mouse.Scroll_down when delta > 0 ->
          move_down t;
          Event.Mouse.stop_propagation event
      | _ -> ())
  | _ -> ()

(* ───── Rendering ───── *)

let render t _self grid ~dt:_ =
  let width = Renderable.width t.node in
  let height = Renderable.height t.node in
  if width <= 0 || height <= 0 then ()
  else
    let focused = Renderable.focused t.node in

    (* Blanks on the background replace the last frame in the buffer, and a
       transparent background lets what is beneath show through. *)
    Matrix_grid.clear_rect ~color:t.props.background grid ~x:0 ~y:0 ~width
      ~height;

    let start_index = scroll_offset t in
    let end_index = min (visible_count t) (start_index + max_visible t) in

    let sel_bg =
      if focused then t.props.focused_selected_background
      else t.props.selected_background
    in
    let sel_fg =
      if focused then t.props.focused_selected_text_color
      else t.props.selected_text_color
    in

    for vi = start_index to end_index - 1 do
      let row = vi - start_index in
      if row < height then (
        let entry = t.visible.(vi) in
        let is_selected = vi = t.selected_index in

        (* Selection highlight *)
        if is_selected then
          Matrix_grid.fill_rect grid ~x:0 ~y:row ~width ~height:1 ~color:sel_bg;

        let indent = entry.depth * t.props.indent_size in

        (* Guide lines *)
        if t.props.show_guides && entry.depth > 0 then (
          let guide_fg = t.props.guide_color in
          let guide_bg = if is_selected then sel_bg else t.props.background in
          let border = t.props.guide_style in
          (* Draw ancestor continuation lines *)
          let guides = entry.parent_guides in
          let rec draw_guides gs d =
            match gs with
            | [] -> ()
            | continues :: rest ->
                let gx = (entry.depth - 1 - d) * t.props.indent_size in
                if continues && gx >= 0 && gx < width then
                  draw_cell grid ~x:gx ~y:row ~fg:guide_fg ~bg:guide_bg
                    border.vertical;
                draw_guides rest (d + 1)
          in
          draw_guides guides 0;
          (* Draw branch connector for this entry *)
          let branch_x = (entry.depth - 1) * t.props.indent_size in
          if branch_x >= 0 && branch_x < width then (
            let connector =
              if entry.is_last_child then border.bottom_left else border.left_t
            in
            draw_cell grid ~x:branch_x ~y:row ~fg:guide_fg ~bg:guide_bg
              connector;
            (* Horizontal segment *)
            let hx = branch_x + 1 in
            if hx < width && hx < indent then
              draw_cell grid ~x:hx ~y:row ~fg:guide_fg ~bg:guide_bg
                border.horizontal));

        (* Icon *)
        let icon_x = indent in
        (if icon_x < width then
           let icon =
             if entry.expandable then
               if entry.expanded then t.props.collapse_icon
               else t.props.expand_icon
             else t.props.leaf_icon
           in
           let icon_style =
             Ansi.Style.make
               ~fg:(if is_selected then sel_fg else t.props.icon_color)
               ()
           in
           Matrix_grid.draw_text ~style:icon_style grid ~x:icon_x ~y:row
             ~text:icon);

        (* Label *)
        let label_x = indent + 2 in
        if label_x < width then
          let label_fg = if is_selected then sel_fg else t.props.text_color in
          let label_style = Ansi.Style.make ~fg:label_fg () in
          Matrix_grid.draw_text ~style:label_style grid ~x:label_x ~y:row
            ~text:entry.item.label)
    done

(* ───── Resize ───── *)

let on_resize t _node = request t

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?items ?state
    ?indent_size ?show_guides ?guide_style ?expand_icon ?collapse_icon
    ?leaf_icon ?background ?text_color ?selected_background ?selected_text_color
    ?focused_selected_background ?focused_selected_text_color ?guide_color
    ?icon_color ?wrap_selection ?key_bindings ?fast_scroll_step () =
  let rnode =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ?items ?state ?indent_size ?show_guides ?guide_style ?expand_icon
      ?collapse_icon ?leaf_icon ?background ?text_color ?selected_background
      ?selected_text_color ?focused_selected_background
      ?focused_selected_text_color ?guide_color ?icon_color ?wrap_selection
      ?key_bindings ?fast_scroll_step ()
  in
  let t =
    {
      node = rnode;
      props;
      keymap = keymap props.key_bindings;
      visible = [||];
      selected_index = 0;
      on_change = None;
      on_activate = None;
    }
  in
  recompute_visible t;
  Renderable.set_render rnode (render t);
  Renderable.set_buffered rnode true;
  Renderable.set_focusable rnode true;
  Renderable.set_key_behavior rnode (Some (handle_key t));
  Renderable.on_mouse rnode (handle_mouse t);
  Renderable.set_on_resize rnode (Some (on_resize t));
  request t;
  t

(* ───── Layout ───── *)

let set_style t style = Renderable.set_style t.node style

(* ───── Apply Props ───── *)

let apply_props t (props : Props.t) =
  if
    not
      (List.equal
         (Keymap.binding_equal ( = ))
         t.props.key_bindings props.key_bindings)
  then begin
    t.keymap <- keymap props.key_bindings;
    t.props <- { t.props with key_bindings = props.key_bindings }
  end;
  install_state t props.items props.state;
  set_indent_size t props.indent_size;
  set_show_guides t props.show_guides;
  set_guide_style t props.guide_style;
  set_expand_icon t props.expand_icon;
  set_collapse_icon t props.collapse_icon;
  set_leaf_icon t props.leaf_icon;
  set_background t props.background;
  set_text_color t props.text_color;
  set_selected_background t props.selected_background;
  set_selected_text_color t props.selected_text_color;
  set_focused_selected_background t props.focused_selected_background;
  set_focused_selected_text_color t props.focused_selected_text_color;
  set_guide_color t props.guide_color;
  set_icon_color t props.icon_color;
  set_wrap_selection t props.wrap_selection;
  set_fast_scroll_step t props.fast_scroll_step

(* ───── Pretty-printing ───── *)

let pp ppf t =
  Format.fprintf ppf "Tree(%s, %d/%d" (Renderable.id t.node) t.selected_index
    (visible_count t);
  if t.props.show_guides then Format.pp_print_string ppf ", guides";
  if t.props.wrap_selection then Format.pp_print_string ppf ", wrap";
  Format.pp_print_char ppf ')'
