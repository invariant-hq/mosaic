(* ───── Item ───── *)

type item = { label : string; children : item list }

let item ?(children = []) label = { label; children }

let rec item_equal a b =
  String.equal a.label b.label && items_equal a.children b.children

and items_equal xs ys =
  match (xs, ys) with
  | [], [] -> true
  | x :: xs, y :: ys -> item_equal x y && items_equal xs ys
  | _ -> false

(* ───── Props ───── *)

module Props = struct
  type t = {
    items : item list;
    selected_index : int;
    expand_depth : int;
    indent_size : int;
    show_guides : bool;
    guide_style : Grid.Border.t;
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

  let make ?(items = []) ?(selected_index = 0) ?(expand_depth = 0)
      ?(indent_size = 2) ?(show_guides = false)
      ?(guide_style = Grid.Border.single) ?(expand_icon = "\xe2\x96\xb6")
      ?(collapse_icon = "\xe2\x96\xbc") ?(leaf_icon = " ")
      ?(background = Ansi.Color.of_rgba 0 0 0 0)
      ?(text_color = Ansi.Color.of_rgb 255 255 255)
      ?(selected_background = Ansi.Color.of_rgb 51 68 85)
      ?(selected_text_color = Ansi.Color.of_rgb 255 255 0)
      ?focused_selected_background ?focused_selected_text_color
      ?(guide_color = Ansi.Color.of_rgb 102 102 102)
      ?(icon_color = Ansi.Color.of_rgb 153 153 153) ?(wrap_selection = false)
      ?(fast_scroll_step = 5) () =
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
      items;
      selected_index = max 0 selected_index;
      expand_depth;
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
    items_equal a.items b.items
    && Int.equal a.selected_index b.selected_index
    && Int.equal a.expand_depth b.expand_depth
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
  path : int list;
}

(* ───── Expansion State ───── *)

module Path_key = struct
  type t = int list

  let equal = List.equal Int.equal
  let hash path = List.fold_left (fun acc i -> (acc * 31) + i + 1) 0 path
end

module Expansion = Hashtbl.Make (Path_key)

(* ───── Types ───── *)

type t = {
  node : Renderable.t;
  mutable props : Props.t;
  mutable tree_items : item list;
  mutable visible : visible_entry array;
  mutable selected_index : int;
  mutable scroll_offset : int;
  mutable max_visible : int;
  expansion : bool Expansion.t;
  mutable on_change : (int -> unit) option;
  mutable on_activate : (int -> unit) option;
  mutable on_expand : (int -> bool -> unit) option;
}

let node t = t.node

(* ───── Internal Helpers ───── *)

let request t = Renderable.request_render t.node
let visible_count t = Array.length t.visible

let clamp_index t idx =
  let len = visible_count t in
  if len = 0 then 0 else max 0 (min (len - 1) idx)

(* ───── Expansion Management ───── *)

let is_path_expanded t path = Expansion.find_opt t.expansion path = Some true

let set_path_expanded t path v =
  if v then Expansion.replace t.expansion path true
  else Expansion.remove t.expansion path

let init_expansion t ~expand_depth =
  Expansion.clear t.expansion;
  if expand_depth <> 0 then
    let rec walk items depth path_prefix =
      List.iteri
        (fun i it ->
          if it.children <> [] then
            let path = List.rev (i :: path_prefix) in
            if expand_depth < 0 || depth < expand_depth then (
              Expansion.replace t.expansion path true;
              walk it.children (depth + 1) (i :: path_prefix)))
        items
    in
    walk t.tree_items 0 []

(* ───── Visible List Computation ───── *)

let recompute_visible t =
  let entries = Buffer.create 64 in
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
  ignore entries;
  let rec walk items depth parent_guides path_prefix =
    let n = List.length items in
    List.iteri
      (fun i it ->
        let path = List.rev (i :: path_prefix) in
        let expandable = it.children <> [] in
        let expanded = expandable && is_path_expanded t path in
        let is_last = i = n - 1 in
        add
          {
            item = it;
            depth;
            expandable;
            expanded;
            is_last_child = is_last;
            parent_guides;
            path;
          };
        if expanded then
          let guide_continues = not is_last in
          walk it.children (depth + 1)
            (guide_continues :: parent_guides)
            (i :: path_prefix))
      items
  in
  walk t.tree_items 0 [] [];
  t.visible <- Array.sub !buf 0 !count

(* ───── Scrolling ───── *)

let recalc_max_visible t height = t.max_visible <- max 1 (max 0 height)

let update_scroll_offset t =
  let len = visible_count t in
  if len = 0 then t.scroll_offset <- 0
  else
    let half = max 0 (t.max_visible / 2) in
    let max_off = max 0 (len - t.max_visible) in
    let desired = t.selected_index - half in
    t.scroll_offset <- max 0 (min desired max_off)

let set_selected_index_internal t idx =
  let len = visible_count t in
  if len = 0 then ()
  else
    let idx = clamp_index t idx in
    if idx <> t.selected_index then (
      t.selected_index <- idx;
      update_scroll_offset t;
      (match t.on_change with None -> () | Some f -> f t.selected_index);
      request t)

(* ───── Public Accessors ───── *)

let items t = t.tree_items
let selected_index t = t.selected_index

let selected_item t =
  let len = visible_count t in
  if len = 0 then None else Some t.visible.(t.selected_index).item

let depth_of t i =
  if i < 0 || i >= visible_count t then 0 else t.visible.(i).depth

(* ───── Data ───── *)

let set_items t its =
  t.tree_items <- its;
  init_expansion t ~expand_depth:t.props.expand_depth;
  recompute_visible t;
  t.selected_index <- clamp_index t t.selected_index;
  update_scroll_offset t;
  request t

(* ───── Selection ───── *)

let set_selected_index t idx = set_selected_index_internal t idx

(* ───── Expansion ───── *)

let expand t i =
  if i >= 0 && i < visible_count t then
    let entry = t.visible.(i) in
    if entry.expandable && not entry.expanded then (
      set_path_expanded t entry.path true;
      recompute_visible t;
      update_scroll_offset t;
      (match t.on_expand with None -> () | Some f -> f i true);
      request t)

let collapse t i =
  if i >= 0 && i < visible_count t then
    let entry = t.visible.(i) in
    if entry.expandable && entry.expanded then (
      set_path_expanded t entry.path false;
      let old_count = visible_count t in
      recompute_visible t;
      let new_count = visible_count t in
      let removed = old_count - new_count in
      if t.selected_index > i && t.selected_index <= i + removed then
        t.selected_index <- i;
      t.selected_index <- clamp_index t t.selected_index;
      update_scroll_offset t;
      (match t.on_expand with None -> () | Some f -> f i false);
      request t)

let toggle_expand t i =
  if i >= 0 && i < visible_count t then
    let entry = t.visible.(i) in
    if entry.expandable then if entry.expanded then collapse t i else expand t i

let expand_all t =
  let rec walk items path_prefix =
    List.iteri
      (fun i it ->
        if it.children <> [] then (
          let path = List.rev (i :: path_prefix) in
          Expansion.replace t.expansion path true;
          walk it.children (i :: path_prefix)))
      items
  in
  walk t.tree_items [];
  recompute_visible t;
  update_scroll_offset t;
  request t

let collapse_all t =
  Expansion.clear t.expansion;
  recompute_visible t;
  t.selected_index <- clamp_index t t.selected_index;
  update_scroll_offset t;
  request t

let is_expanded t i =
  if i >= 0 && i < visible_count t then t.visible.(i).expanded else false

(* ───── Navigation ───── *)

let move_up ?(steps = 1) t =
  let len = visible_count t in
  if len = 0 then ()
  else
    let new_index = t.selected_index - steps in
    if new_index >= 0 then set_selected_index_internal t new_index
    else if t.props.wrap_selection then set_selected_index_internal t (len - 1)
    else set_selected_index_internal t 0

let move_down ?(steps = 1) t =
  let len = visible_count t in
  if len = 0 then ()
  else
    let new_index = t.selected_index + steps in
    if new_index < len then set_selected_index_internal t new_index
    else if t.props.wrap_selection then set_selected_index_internal t 0
    else set_selected_index_internal t (len - 1)

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
let set_on_expand t cb = t.on_expand <- cb

(* ───── Rendering Helpers ───── *)

let draw_cell grid ~x ~y ~fg ~bg uch =
  let cell = Grid.Cell.of_uchar uch in
  Grid.set_cell grid ~x ~y ~cell ~fg ~bg ~attrs:Ansi.Attr.empty ()

(* ───── Key Handling ───── *)

let handle_key t (event : Event.key) =
  let kev = Event.Key.data event in
  let shift = kev.modifier.shift in
  let consumed =
    match kev.key with
    | Up ->
        move_up ~steps:(if shift then t.props.fast_scroll_step else 1) t;
        true
    | Down ->
        move_down ~steps:(if shift then t.props.fast_scroll_step else 1) t;
        true
    | Char c when Uchar.equal c (Uchar.of_char 'k') ->
        move_up t;
        true
    | Char c when Uchar.equal c (Uchar.of_char 'j') ->
        move_down t;
        true
    | Right ->
        (if visible_count t > 0 then
           let entry = t.visible.(t.selected_index) in
           if entry.expandable && not entry.expanded then
             expand t t.selected_index
           else if entry.expanded then
             (* Move to first child *)
             let next = t.selected_index + 1 in
             if next < visible_count t then set_selected_index_internal t next);
        true
    | Left ->
        (if visible_count t > 0 then
           let entry = t.visible.(t.selected_index) in
           if entry.expandable && entry.expanded then
             collapse t t.selected_index
           else
             match find_parent_index t t.selected_index with
             | Some pi -> set_selected_index_internal t pi
             | None -> ());
        true
    | Char c when Uchar.equal c (Uchar.of_char ' ') ->
        if visible_count t > 0 then toggle_expand t t.selected_index;
        true
    | Enter | KP_enter ->
        (if visible_count t > 0 then
           match t.on_activate with None -> () | Some f -> f t.selected_index);
        true
    | _ -> false
  in
  if consumed then Event.Key.prevent_default event

(* ───── Mouse Handling ───── *)

let handle_mouse t (event : Event.mouse) =
  let width = Renderable.width t.node in
  let height = Renderable.height t.node in
  let x = Event.Mouse.x event in
  let y = Event.Mouse.y event in
  match Event.Mouse.kind event with
  | Down { button = Left } ->
      if x >= 0 && x < width && y >= 0 && y < height then
        let index = t.scroll_offset + y in
        if index >= 0 && index < visible_count t then (
          let entry = t.visible.(index) in
          let indent = entry.depth * t.props.indent_size in
          (* Icon occupies columns [indent, indent+2) *)
          if x >= indent && x < indent + 2 && entry.expandable then
            toggle_expand t index
          else set_selected_index_internal t index;
          Event.Mouse.stop_propagation event)
  | Scroll { direction; delta } -> (
      match direction with
      | Input.Mouse.Scroll_up when delta > 0 ->
          move_up t;
          Event.Mouse.stop_propagation event
      | Input.Mouse.Scroll_down when delta > 0 ->
          move_down t;
          Event.Mouse.stop_propagation event
      | _ -> ())
  | _ -> ()

(* ───── Rendering ───── *)

let render t _self grid ~delta:_ =
  let width = Renderable.width t.node in
  let height = Renderable.height t.node in
  if width <= 0 || height <= 0 then ()
  else
    let focused = Renderable.focused t.node in

    (* Recompute scroll metrics *)
    recalc_max_visible t height;
    update_scroll_offset t;

    (* Clear to opaque base so compositing fully replaces the screen grid, then
       apply the user's background on top. *)
    Grid.fill_rect grid ~x:0 ~y:0 ~width ~height
      ~color:(Ansi.Color.of_rgb 0 0 0);
    Grid.fill_rect grid ~x:0 ~y:0 ~width ~height ~color:t.props.background;

    let start_index = t.scroll_offset in
    let end_index = min (visible_count t) (start_index + t.max_visible) in

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
          Grid.fill_rect grid ~x:0 ~y:row ~width ~height:1 ~color:sel_bg;

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
           Grid.draw_text ~style:icon_style grid ~x:icon_x ~y:row ~text:icon);

        (* Label *)
        let label_x = indent + 2 in
        if label_x < width then
          let label_fg = if is_selected then sel_fg else t.props.text_color in
          let label_style = Ansi.Style.make ~fg:label_fg () in
          Grid.draw_text ~style:label_style grid ~x:label_x ~y:row
            ~text:entry.item.label)
    done

(* ───── Resize ───── *)

let on_resize t _node =
  let h = Renderable.height t.node in
  recalc_max_visible t h;
  update_scroll_offset t;
  request t

(* ───── Construction ───── *)

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ?items
    ?selected_index ?expand_depth ?indent_size ?show_guides ?guide_style
    ?expand_icon ?collapse_icon ?leaf_icon ?background ?text_color
    ?selected_background ?selected_text_color ?focused_selected_background
    ?focused_selected_text_color ?guide_color ?icon_color ?wrap_selection
    ?fast_scroll_step () =
  let rnode =
    Renderable.create ~parent ?index ?id ?style ?visible ?z_index ?opacity ()
  in
  let props =
    Props.make ?items ?selected_index ?expand_depth ?indent_size ?show_guides
      ?guide_style ?expand_icon ?collapse_icon ?leaf_icon ?background
      ?text_color ?selected_background ?selected_text_color
      ?focused_selected_background ?focused_selected_text_color ?guide_color
      ?icon_color ?wrap_selection ?fast_scroll_step ()
  in
  let t =
    {
      node = rnode;
      props;
      tree_items = props.items;
      visible = [||];
      selected_index = 0;
      scroll_offset = 0;
      max_visible = 1;
      expansion = Expansion.create 16;
      on_change = None;
      on_activate = None;
      on_expand = None;
    }
  in
  init_expansion t ~expand_depth:props.expand_depth;
  recompute_visible t;
  t.selected_index <- clamp_index t props.selected_index;
  recalc_max_visible t (Renderable.height rnode);
  update_scroll_offset t;
  Renderable.set_render rnode (render t);
  Renderable.set_buffered rnode true;
  Renderable.set_focusable rnode true;
  Renderable.on_key rnode (handle_key t);
  Renderable.on_mouse rnode (handle_mouse t);
  Renderable.set_on_resize rnode (Some (on_resize t));
  request t;
  t

(* ───── Layout ───── *)

let set_style t style = Renderable.set_style t.node style

(* ───── Apply Props ───── *)

let apply_props t (props : Props.t) =
  if not (items_equal t.tree_items props.items) then set_items t props.items;
  if props.selected_index <> t.selected_index then
    set_selected_index t props.selected_index;
  if props.expand_depth <> t.props.expand_depth then (
    t.props <- { t.props with expand_depth = props.expand_depth };
    init_expansion t ~expand_depth:props.expand_depth;
    recompute_visible t;
    t.selected_index <- clamp_index t t.selected_index;
    update_scroll_offset t;
    request t);
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
