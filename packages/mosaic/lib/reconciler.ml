open Mosaic_ui

(* ───── Common attributes ───── *)

let update_common (node : Renderable.t) ~(old_attrs : Vnode.attrs)
    ~(new_attrs : Vnode.attrs) : bool =
  let changed = ref false in
  if old_attrs.visible <> new_attrs.visible then (
    Renderable.set_visible node new_attrs.visible;
    changed := true);
  if old_attrs.z_index <> new_attrs.z_index then (
    Renderable.set_z_index node new_attrs.z_index;
    changed := true);
  if old_attrs.opacity <> new_attrs.opacity then (
    Renderable.set_opacity node new_attrs.opacity;
    changed := true);
  if
    old_attrs.focusable <> new_attrs.focusable
    || old_attrs.focused <> new_attrs.focused
  then (
    Renderable.set_focusable node (new_attrs.focusable || new_attrs.focused);
    changed := true);
  if old_attrs.buffered <> new_attrs.buffered then (
    Renderable.set_buffered node new_attrs.buffered;
    changed := true);
  if old_attrs.live <> new_attrs.live then (
    Renderable.set_live node new_attrs.live;
    changed := true);
  (* Style is handled in update_instance for widget-specific behavior.
     Box.set_style adds border insets and Scroll_box.set_style its root
     overrides; Renderable.set_style does not. *)
  !changed

(* ───── Messages and routes ───── *)

(* The reconciler holds the handlers of elements whose message types differ
   below each [Vnode.Map]. It erases them to [msg] where a view enters it
   ([render]) and where a map's subtree and function are stored. A handler's
   message goes up its fiber's route, through each map's current function,
   to [deliver]. [Vnode.map] states why the erasure is sound. *)
type msg = Obj.t

type route = Root of (msg -> unit) | Tagged of tagged

(* A map's place keeps its route; each render stores the map's function, so
   a kept subtree under it sends through the function the view gave last. *)
and tagged = { mutable tagger : msg -> msg; parent : route }

let rec send route m =
  match route with
  | Root deliver -> deliver m
  | Tagged t -> send t.parent (t.tagger m)

let send_option route = function Some m -> send route m | None -> ()

let focus_request route (handlers : msg Vnode.handlers) =
  match handlers.on_focus_request with
  | None -> None
  | Some g -> Some (fun () -> send route (g ()))

(* ───── Fiber Tracking ───── *)

(* The focus a subtree declares. A shown modal in it makes every element
   outside the modal inert, so its focus is the modal's alone. *)
type declared_focus =
  | Focus_none
  | Focus_one of Renderable.t
  | Focus_many
  | Modal of modal

and modal = {
  modal_node : Renderable.t;
  close : unit -> bool;
      (** Sends the modal's [on_close], and is [true] iff it has one. *)
  inside : declared_focus;  (** Never [Modal]. *)
}

(* A parent's children: elements, embedded nodes, and spans. A fiber is an
   element's widget of some kind, with the props, handler and attributes the
   element declared last; its constructor hides the kind's types, which only
   the kind's own operations need. *)
type child =
  | Fiber : ('props, 'event, 'w) fiber -> child
  | Embedded of Renderable.t
  | Span of span

and ('props, 'event, 'w) fiber = {
  kind : ('props, 'event, 'w) Kind.t;
  widget : 'w;
  mutable props : 'props;
  mutable on : 'event -> msg option;
      (** The handler the element declared last, which the widget's events read
          when they come. *)
  key : string option;
  mutable attrs : Vnode.attrs;
  mutable children : child list;
  route : route;  (** Fixed for its life: a fiber never changes lists. *)
  mutable handlers : msg Vnode.handlers;
      (** The handlers the element declared last, which the node's handlers read
          when an event comes. *)
  mutable declared_children : msg Vnode.t list;
  mutable viewport_width : int;
  mutable declared_focus : declared_focus;
  mutable pass : int;  (** The last matching pass that matched it. *)
}

(* A span owns the children of a memo's view, of a map's subtree or of one
   binding of a keyed map; an unchanged span keeps them without visiting
   them. A keyed map's span owns the spans of its bindings instead. *)
and span = {
  tagged : tagged option;  (** A map's span: its route cell. *)
  keyed : keyed option;  (** A keyed map's span: its bindings. *)
  span_route : route;  (** The route of its children. *)
  mutable memo : msg Vnode.memo option;  (** A memo's span: its memo. *)
  mutable span_theme : (Theme.t -> Theme.t) option;
      (** The theme its place declares, given to the nodes it places. *)
  mutable view : msg Vnode.t;
      (** A memo's view, built once, or a map's subtree. *)
  mutable span_viewport_width : int;
  mutable span_children : child list;
  mutable node_count : int;
      (** Renderable nodes in [span_children], spans flattened. *)
  mutable first_node : Renderable.t option;
  mutable span_focus : declared_focus;
  mutable reused : bool;
      (** The current render kept the span's children unchanged. *)
  mutable span_pass : int;  (** The last pass that matched it. *)
}

(* A keyed map's place: the map and item function it declared last, and one
   span per binding, in the map's order. [focused] holds the bindings that
   declare focus, so that the span's focus is found without visiting the
   others. [touched] holds the keys whose bindings its last reconciliation
   bound or reconciled again, newest first, unless it reconciled them all
   ([touched_all]): placing the span checks those alone. *)
and keyed = {
  mutable declared : (Obj.t, Obj.t) Keyed.t;
  mutable item : Obj.t -> Obj.t -> msg Vnode.t;
  mutable bindings : (Obj.t, span) Keyed.t;
  mutable focused : (Obj.t, span) Keyed.t;
  mutable touched : Obj.t list;
  mutable touched_all : bool;
}

let fiber_node f = f.kind.Kind.node f.widget

(* A spinner's node is live while its element is declared live and its
   frame set animates. The widget sets the node live from its frame set alone
   whenever its props are applied, so this runs after it. *)
let apply_live : type p e w. (p, e, w) fiber -> unit =
 fun f ->
  match Type.Id.provably_equal f.kind.id Kinds.spinner.id with
  | Some Type.Equal ->
      Renderable.set_live (fiber_node f)
        (f.attrs.live && Spinner.animates f.widget)
  | None -> Renderable.set_live (fiber_node f) f.attrs.live

(* The handler a kind's [handler] hook takes: none for an element that
   declares none, else one that sends through the handler [on] declared,
   which a widget keeps for an event that outlives a reconciliation. *)
let give_handler f =
  match f.kind.handler with
  | None -> ()
  | Some handler ->
      let on = f.on in
      handler f.widget
        (if on == Kind.no_handler then None
         else Some (fun event -> send_option f.route (on event)))

(* The widget, its node's handlers and its events read the fiber's current
   handlers, handler and route when an event comes, so each closure holds the
   fiber alone. *)
let create_fiber ~(parent : Renderable.t) ~route (elem : msg Vnode.element) :
    child =
  let (E e) = elem in
  let attrs = e.attrs in
  let widget = e.kind.create ~parent ~layout:attrs.style e.props in
  let f =
    {
      kind = e.kind;
      widget;
      props = e.props;
      on = e.on;
      key = e.key;
      attrs;
      children = [];
      route;
      handlers = e.handlers;
      declared_children = [];
      viewport_width = 0;
      declared_focus = Focus_none;
      pass = 0;
    }
  in
  let node = fiber_node f in
  if not attrs.visible then Renderable.set_visible node false;
  if attrs.z_index <> 0 then Renderable.set_z_index node attrs.z_index;
  if attrs.opacity <> 1.0 then Renderable.set_opacity node attrs.opacity;
  Renderable.set_focusable node (attrs.focusable || attrs.focused);
  if attrs.buffered then Renderable.set_buffered node true;
  if attrs.live || Kind.same e.kind Kinds.spinner then apply_live f;
  Renderable.set_on_focus_request node (focus_request route e.handlers);
  Renderable.on_mouse node (fun ev ->
      Option.iter (fun g -> send_option f.route (g ev)) f.handlers.on_mouse);
  Renderable.set_on_click node
    (Some
       (fun ev ->
         Option.iter (fun g -> send_option f.route (g ev)) f.handlers.on_click));
  Renderable.set_on_size node
    (Some
       (fun node ->
         Option.iter
           (fun g ->
             send_option f.route
               (g ~w:(Renderable.width node) ~h:(Renderable.height node)))
           f.handlers.on_size));
  Renderable.on_key node (fun ev ->
      match f.handlers.on_key with
      | Some h -> send_option f.route (h ev)
      | None -> ());
  Renderable.on_paste node (fun ev ->
      match f.handlers.on_paste with
      | Some h -> send_option f.route (h ev)
      | None -> ());
  Option.iter
    (fun events ->
      events widget (fun event -> send_option f.route (f.on event)))
    e.kind.events;
  give_handler f;
  Fiber f

(* A fiber keeps its records while the element's equal them, so the
   element's fresh records die young and an unchanged element writes
   nothing into the fiber. [elem] must be of the fiber's kind. *)
let update_fiber : type p e w. (p, e, w) fiber -> msg Vnode.element -> bool =
 fun f (E e) ->
  match Type.Id.provably_equal f.kind.id e.kind.id with
  | None -> invalid_arg "Reconciler: an element of another kind"
  | Some Type.Equal ->
      if f.handlers != e.handlers then begin
        Renderable.set_on_focus_request (fiber_node f)
          (focus_request f.route e.handlers);
        f.handlers <- e.handlers
      end;
      if f.on != e.on then begin
        f.on <- e.on;
        give_handler f
      end;
      let old_attrs = f.attrs and new_attrs = e.attrs in
      let attrs_changed =
        old_attrs != new_attrs
        &&
        let common = update_common (fiber_node f) ~old_attrs ~new_attrs in
        (* A layout shared by many elements is the same record on every
           render, and comparing it physically first costs nothing per
           field. *)
        let style =
          old_attrs.style != new_attrs.style
          && old_attrs.style <> new_attrs.style
        in
        if style then f.kind.layout f.widget new_attrs.style;
        common || style
      in
      let props_changed =
        f.props != e.props && not (f.kind.equal f.props e.props)
      in
      if attrs_changed then f.attrs <- new_attrs;
      if props_changed then begin
        f.kind.update f.widget e.props;
        f.props <- e.props
      end;
      if (attrs_changed || props_changed) && Kind.same f.kind Kinds.spinner then
        apply_live f;
      let changed = attrs_changed || props_changed in
      if changed then Renderable.request_render (fiber_node f);
      changed

(* A fiber's descendants have their nodes under its node, except embedded
   nodes, which the application owns and which outlive the fiber. Fibers are
   destroyed together: their nodes and the embedded nodes under them are
   detached in one pass, then each fiber's node is destroyed with the nodes
   under it. Detaching or destroying them one by one would detach each node
   from its parent alone, shifting the siblings after it, and the teardown
   would be quadratic in their number. *)
(* [fold_children f acc span] folds [f] over [span]'s children, or over its
   bindings' children for a keyed map's span, in order. *)
let fold_children f acc span =
  match span.keyed with
  | None -> List.fold_left f acc span.span_children
  | Some k ->
      Keyed.fold
        (fun _ binding acc -> List.fold_left f acc binding.span_children)
        k.bindings acc

let rec embeds_under acc = function
  | Fiber f -> List.fold_left embeds_under acc f.children
  | Embedded n -> n :: acc
  | Span span -> fold_children embeds_under acc span

let child_node = function
  | Fiber f -> fiber_node f
  | Embedded n -> n
  | Span _ -> invalid_arg "Reconciler: a span has no node"

(* Destroys [fibers], each a [Fiber], in order, and detaches [embeds] with
   them. *)
let destroy_fibers ?(embeds = []) (fibers : child list) =
  let embeds =
    List.fold_left
      (fun acc -> function
        | Fiber f -> List.fold_left embeds_under acc f.children
        | Embedded _ | Span _ -> acc)
      (List.rev embeds) fibers
  in
  Renderable.detach_all (List.rev_append embeds (List.map child_node fibers));
  List.iter
    (fun fiber -> Renderable.destroy_recursively (child_node fiber))
    fibers

(* The fibers among [children], spans flattened, and the embedded nodes, both
   in order. *)
let fibers_and_embeds children =
  let rec go (fibers, embeds) = function
    | Fiber _ as fiber -> (fiber :: fibers, embeds)
    | Embedded n -> (fibers, n :: embeds)
    | Span span -> fold_children go (fibers, embeds) span
  in
  let fibers, embeds = List.fold_left go ([], []) children in
  (List.rev fibers, List.rev embeds)

(* ───── Vnode Flattening ───── *)

(* [inner] applied to what [outer] gives. *)
let compose outer inner =
  match outer with None -> inner | Some outer -> fun t -> inner (outer t)

let same_theme a b =
  match (a, b) with
  | None, None -> true
  | Some f, Some g -> f == g
  | Some _, None | None, Some _ -> false

(* ───── Matching ───── *)

(* Each reconciliation of one list of children is a pass with a number of its
   own. The fibers and spans a pass matches carry its number, so the old
   children nothing matched are found by one walk of the old list, with no
   table and no array.

   Keyed elements match by key, unkeyed elements by position among the
   list's elements. This mirrors React's reconciliation heuristic: keys give
   stable identity across reorders, positional matching is the fallback.
   Memos match by position among the list's memos. A match is the cell of
   the old list that holds the old child, so a kept child is the old list's
   own value and the matcher allocates nothing for it. The key table is
   built the first time an element with a key asks for it. *)
let current_pass = ref 0

type matcher = {
  pass : int;
  old_children : child list;
  mutable next_element : child list;
      (** The old children from the next element position on. *)
  mutable next_span : child list;
      (** The old children from the next span position on: memos, maps and keyed
          maps share their positions. *)
  mutable keys : (string, child list) Hashtbl.t option;
}

let matcher old_children =
  incr current_pass;
  {
    pass = !current_pass;
    old_children;
    next_element = old_children;
    next_span = old_children;
    keys = None;
  }

(* The cell of the old fiber at the next element position, or [], moving
   the position past it. *)
let rec take_element m =
  match m.next_element with
  | [] -> []
  | Fiber _ :: rest as cell ->
      m.next_element <- rest;
      cell
  | (Embedded _ | Span _) :: rest ->
      m.next_element <- rest;
      take_element m

(* The cell of the old span at the next span position, or [], moving the
   position past it. The caller matches it only if it is of its kind. *)
let rec take_span m =
  match m.next_span with
  | [] -> []
  | Span _ :: rest as cell ->
      m.next_span <- rest;
      cell
  | (Embedded _ | Fiber _) :: rest ->
      m.next_span <- rest;
      take_span m

(* First occurrence wins on duplicate keys, so when a view accidentally
   emits duplicate-keyed siblings the first keeps its identity across
   renders and only the later duplicates remount. *)
let keys m =
  match m.keys with
  | Some keys -> keys
  | None ->
      let keys = Hashtbl.create 16 in
      let rec add cells =
        match cells with
        | [] -> ()
        | Fiber { key = Some k; _ } :: rest ->
            if not (Hashtbl.mem keys k) then Hashtbl.add keys k cells;
            add rest
        | _ :: rest -> add rest
      in
      add m.old_children;
      m.keys <- Some keys;
      keys

(* The cell of the old fiber [elem] matches, or []. The fiber takes the
   pass's number. *)
let find_match m (E e : msg Vnode.element) : child list =
  let positional = take_element m in
  let cell =
    match e.key with
    | None -> (
        match positional with
        | Fiber { key = None; _ } :: _ -> positional
        | _ -> [])
    | Some k -> (
        match Hashtbl.find_opt (keys m) k with Some cell -> cell | None -> [])
  in
  match cell with
  | Fiber f :: _ when f.pass <> m.pass && Kind.same f.kind e.kind ->
      f.pass <- m.pass;
      cell
  | _ -> []

(* ───── Building the new list ───── *)

(* The new children of a list, built as they are reconciled. While each
   child is the old list's child at the same index, physically, nothing is
   allocated, and a list whose children all stayed in place is the old list
   itself. *)
type builder = {
  old : child list;
  mutable rest : child list;  (** The old children after those kept. *)
  mutable kept : int;
  mutable acc : child list;
      (** Reversed, once a child is not the old one at its index; never empty
          then. *)
}

let builder old = { old; rest = old; kept = 0; acc = [] }

let rec rev_prefix list n acc =
  if n = 0 then acc
  else
    match list with
    | [] -> acc
    | child :: rest -> rev_prefix rest (n - 1) (child :: acc)

let emit b child =
  match b.acc with
  | _ :: _ -> b.acc <- child :: b.acc
  | [] -> (
      match b.rest with
      | c :: rest when c == child ->
          b.rest <- rest;
          b.kept <- b.kept + 1
      | _ -> b.acc <- child :: rev_prefix b.old b.kept [])

let finish b =
  match (b.acc, b.rest) with
  | _ :: _, _ -> List.rev b.acc
  | [], [] -> b.old
  | [], _ :: _ -> List.rev (rev_prefix b.old b.kept [])

(* ───── Commit Placement ───── *)

(* Placement of a parent's children happens after all of them are reconciled.
   Unmatched fibers were destroyed, which detaches them; an embedded node that
   left its list is detached unless it reappears elsewhere among the parent's
   children. Then the children are placed in order from index 0.

   We intentionally avoid scanning all concrete Renderable children under
   [parent], because some widgets (e.g. Markdown) manage internal children
   outside the reconciler. A first pass compares each child with the node at
   its index through the O(1) [child_at] probe, and a reused span whose first
   node is in place is skipped whole: its nodes were placed contiguously, and
   placement before it only shifts them. From the first child out of place,
   the children are placed in one pass ([Renderable.place_children]), so
   any order costs time linear in the number of children. Node sets are keyed
   by [Renderable.uid] -- hashing raw nodes would apply polymorphic
   hash and compare to cyclic records of closures. *)
let rec iter_embeds f = function
  | Fiber _ -> ()
  | Embedded n -> f n
  | Span span -> fold_children (fun () child -> iter_embeds f child) () span

let detach_stale_embeds ~stale (new_children : child list) =
  if stale <> [] then begin
    let kept = Hashtbl.create 8 in
    List.iter
      (iter_embeds (fun n -> Hashtbl.replace kept (Renderable.uid n) ()))
      new_children;
    Renderable.detach_all
      (List.filter (fun n -> not (Hashtbl.mem kept (Renderable.uid n))) stale)
  end

(* The index of the first of [children] that is not at its index among
   [parent]'s children, counting from [index], if any. *)
let rec first_misplaced parent index = function
  | [] -> None
  | Fiber f :: rest -> probe parent index (fiber_node f) rest
  | Embedded n :: rest -> probe parent index n rest
  | Span span :: rest -> (
      match misplaced_in_span parent index span with
      | None -> first_misplaced parent (index + span.node_count) rest
      | Some _ as misplaced -> misplaced)

and misplaced_in_span parent index span =
  let in_place =
    span.reused
    &&
    match (span.first_node, Renderable.child_at parent index) with
    | Some first, Some n -> n == first
    | None, _ -> true
    | Some _, None -> false
  in
  if in_place then None
  else
    match span.keyed with
    | None -> first_misplaced parent index span.span_children
    | Some k when k.touched_all -> misplaced_in_bindings parent index k
    | Some k -> misplaced_in_touched parent index span k

and misplaced_in_bindings parent index k =
  let index = ref index and found = ref None in
  try
    Keyed.iter
      (fun _ binding ->
        match misplaced_in_span parent !index binding with
        | None -> index := !index + binding.node_count
        | Some _ as misplaced ->
            found := misplaced;
            raise_notrace Exit)
      k.bindings;
    None
  with Exit -> !found

(* The bindings a reconciliation did not touch keep their nodes, and nodes
   only leave a parent or join it at its end before placement, so those
   bindings' nodes follow each other in order. When the first binding's node
   is at [index], the bindings before the first touched one are in place,
   and a touched binding in place is followed by bindings in place up to the
   next touched one: a new node in place has no old node after it, and an old
   one is followed by the old nodes that followed it. So the touched bindings
   are the ones to check, each at the count of nodes before it. *)
and misplaced_in_touched parent index span k =
  match Keyed.min_binding_opt k.bindings with
  | None -> None
  | Some (_, { first_node = None; _ }) -> misplaced_in_bindings parent index k
  | Some (_, { first_node = Some first; _ }) -> (
      match Renderable.child_at parent index with
      | Some n when n == first ->
          let rec check = function
            | [] -> None
            | key :: rest -> (
                match Keyed.find_opt key k.bindings with
                | None -> check rest
                | Some binding -> (
                    let at = index + nodes_before span k key in
                    match misplaced_in_span parent at binding with
                    | None -> check rest
                    | Some _ as misplaced -> misplaced))
          in
          check (List.rev k.touched)
      | Some _ | None -> Some index)

(* The nodes of the bindings before [key], counted from the nearer end. *)
and nodes_before span k key =
  let count _ binding nodes = nodes + binding.node_count in
  let i = Keyed.rank k.bindings key and n = Keyed.cardinal k.bindings in
  if i <= n - i then Keyed.fold_range count k.bindings 0 i 0
  else span.node_count - Keyed.fold_range count k.bindings i n 0

and probe parent index node rest =
  match Renderable.child_at parent index with
  | Some n when n == node -> first_misplaced parent (index + 1) rest
  | Some _ | None -> Some index

(* [iter_from from children f] applies [f] to the nodes of [children] from
   the [from]th on, in order. *)
let iter_from from children f =
  let position = ref 0 in
  let visit node =
    if !position >= from then f node;
    incr position
  in
  let rec go = function
    | [] -> ()
    | Fiber fiber :: rest ->
        visit (fiber_node fiber);
        go rest
    | Embedded n :: rest ->
        visit n;
        go rest
    | Span span :: rest ->
        (if !position + span.node_count <= from then
           position := !position + span.node_count
         else
           match span.keyed with
           | None -> go span.span_children
           | Some k ->
               Keyed.iter
                 (fun _ binding ->
                   if !position + binding.node_count <= from then
                     position := !position + binding.node_count
                   else go binding.span_children)
                 k.bindings);
        go rest
  in
  go children

let commit_placement parent_node ~stale new_children =
  detach_stale_embeds ~stale new_children;
  let parent = Renderable.child_target parent_node in
  match first_misplaced parent 0 new_children with
  | None -> ()
  | Some index ->
      Renderable.place_children parent index (iter_from index new_children)

(* ───── Reconciliation ───── *)

type 'msg t = {
  container : Renderable.t;
  root : route;
  mutable root_children : child list;
}

let create ~container ~(deliver : 'msg -> unit) =
  {
    container;
    root = Root (Obj.magic deliver : msg -> unit);
    root_children = [];
  }

let container t = t.container

(* Of two modals the later in tree order is the one, as the dialog opened
   last is on top. *)
let combine_focus left right =
  match (left, right) with
  | _, (Modal _ as modal) | (Modal _ as modal), _ -> modal
  | Focus_none, focus | focus, Focus_none -> focus
  | _ -> Focus_many

(* Only a box holds children, so only a box is inert. *)
let inert : type p e w. (p, e, w) Kind.t -> p -> bool =
 fun kind props ->
  match Type.Id.provably_equal kind.id Kinds.box.id with
  | Some Type.Equal -> Box.Props.inert props
  | None -> false

let inert_element (E e : msg Vnode.element) = inert e.kind e.props
let is_modal fiber = Kind.same fiber.kind Kinds.modal

let children_focus children =
  List.fold_left
    (fun focus -> function
      | Embedded _ -> focus
      | Fiber fiber -> combine_focus focus fiber.declared_focus
      | Span span -> combine_focus focus span.span_focus)
    Focus_none children

(* A modal's request to close it: its handler's message, if it gives one. *)
let close : type p e w. (p, e, w) fiber -> unit -> bool =
 fun fiber () ->
  match Type.Id.provably_equal fiber.kind.id Kinds.modal.id with
  | Some Type.Equal -> (
      match fiber.on () with
      | Some message ->
          send fiber.route message;
          true
      | None -> false)
  | None -> false

let update_declared_focus fiber =
  fiber.declared_focus <-
    (if (not fiber.attrs.visible) || inert fiber.kind fiber.props then
       Focus_none
     else
       let focus =
         combine_focus
           (if fiber.attrs.focused then Focus_one (fiber_node fiber)
            else Focus_none)
           (children_focus fiber.children)
       in
       match focus with
       | Modal _ -> focus
       | (Focus_none | Focus_one _ | Focus_many) as inside ->
           if is_modal fiber then
             Modal
               { modal_node = fiber_node fiber; close = close fiber; inside }
           else focus)

let rec node_count children =
  List.fold_left
    (fun count -> function
      | Fiber _ | Embedded _ -> count + 1 | Span span -> count + span.node_count)
    0 children

and first_node = function
  | [] -> None
  | Fiber f :: _ -> Some (fiber_node f)
  | Embedded n :: _ -> Some n
  | Span span :: rest -> (
      match span.first_node with
      | Some _ as node -> node
      | None -> first_node rest)

let element_attrs (E e : msg Vnode.element) = e.attrs
let element_children (E e : msg Vnode.element) = e.children

let rec reconcile_element ~(parent_node : Renderable.t) ~route m ~viewport_width
    (elem : msg Vnode.element) : child =
  let attrs = element_attrs elem and children = element_children elem in
  match find_match m elem with
  | (Fiber f as child) :: _ ->
      let focus_changed =
        f.attrs.focused <> attrs.focused
        || f.attrs.visible <> attrs.visible
        || inert f.kind f.props <> inert_element elem
      in
      let children_changed =
        f.declared_children != children || f.viewport_width <> viewport_width
      in
      ignore (update_fiber f elem : bool);
      if children_changed then begin
        reconcile_children f ~viewport_width children;
        f.declared_children <- children;
        f.viewport_width <- viewport_width
      end;
      if focus_changed || children_changed then update_declared_focus f;
      child
  | _ -> (
      match create_fiber ~parent:parent_node ~route elem with
      | Fiber f as child ->
          f.pass <- m.pass;
          reconcile_children f ~viewport_width children;
          f.declared_children <- children;
          f.viewport_width <- viewport_width;
          update_declared_focus f;
          child
      | Embedded _ | Span _ -> assert false)

and reconcile_children : type p e w.
    (p, e, w) fiber -> viewport_width:int -> msg Vnode.t list -> unit =
 fun f ~viewport_width children ->
  let stale = ref [] in
  let children =
    reconcile_list ~parent_node:(fiber_node f) ~route:f.route ~viewport_width
      ~stale ~theme:None f.children Vnode.Empty children
  in
  if children != f.children then f.children <- children;
  commit_placement (fiber_node f) ~stale:!stale children

(* Reconciles one list of children, a parent's or a span's, against
   [old_children]: [vnode], then [children], are walked through a matcher, and
   the new children given to a builder. Embedded nodes that leave the list
   are collected in [stale]. *)
and reconcile_list ~parent_node ~route ~viewport_width ~stale ~theme
    (old_children : child list) vnode children : child list =
  let m = matcher old_children in
  let b = builder old_children in
  walk ~parent_node ~route m b ~viewport_width ~stale ~theme vnode;
  walk_list ~parent_node ~route m b ~viewport_width ~stale ~theme children;
  (* A discarded span's embedded nodes may reappear elsewhere, so they are
     left to placement. *)
  let unused = ref [] and spans = ref [] in
  List.iter
    (function
      | Embedded n -> stale := n :: !stale
      | Fiber f as fiber -> if f.pass <> m.pass then unused := fiber :: !unused
      | Span span ->
          if span.span_pass <> m.pass then spans := Span span :: !spans)
    old_children;
  (match (!unused, !spans) with
  | [], [] -> ()
  | unused, spans ->
      let span_fibers, span_embeds = fibers_and_embeds (List.rev spans) in
      stale := List.rev_append span_embeds !stale;
      destroy_fibers (List.rev_append unused span_fibers));
  finish b

(* Fragments, themed vnodes and Empty nodes are structural: they group
   children but are no widgets. The walk goes through them to the concrete
   children (elements, embeds, and the spans of memos and maps) in order. A
   child inside [themed] wrappers takes their functions composed, outermost
   first, as its place's theme ([Renderable.set_place_theme]); a view
   without [themed] allocates nothing for them. *)
and walk ~parent_node ~route m b ~viewport_width ~stale ~theme
    (vnode : msg Vnode.t) =
  match vnode with
  | Vnode.Empty -> ()
  | Vnode.Element elem ->
      let child =
        reconcile_element ~parent_node ~route m ~viewport_width elem
      in
      (match child with
      | Fiber f -> Renderable.set_place_theme (fiber_node f) theme
      | Embedded _ | Span _ -> ());
      emit b child
  | Vnode.Embed n ->
      Renderable.set_place_theme n theme;
      let child =
        match b.rest with
        | (Embedded n' as child) :: _ when n' == n -> child
        | _ -> Embedded n
      in
      emit b child
  | Vnode.Memo memo ->
      let previous = take_span m in
      emit b
        (span_child m previous
           (reconcile_memo ~parent_node ~route ~viewport_width ~stale ~theme
              previous memo))
  | Vnode.Map (f, child) ->
      let previous = take_span m in
      emit b
        (span_child m previous
           (reconcile_map ~parent_node ~route ~viewport_width ~stale ~theme
              previous
              (Obj.magic f : msg -> msg)
              (Obj.magic child : msg Vnode.t)))
  | Vnode.Keyed (map, item) ->
      let previous = take_span m in
      emit b
        (span_child m previous
           (reconcile_keyed ~parent_node ~route ~viewport_width ~stale ~theme
              previous
              (Obj.magic map : (Obj.t, Obj.t) Keyed.t)
              (Obj.magic item : Obj.t -> Obj.t -> msg Vnode.t)))
  | Vnode.Themed (f, child) ->
      walk ~parent_node ~route m b ~viewport_width ~stale
        ~theme:(Some (compose theme f))
        child
  | Vnode.Fragment children ->
      walk_list ~parent_node ~route m b ~viewport_width ~stale ~theme children
  | Vnode.Viewport_switch { at_least_width; wide; narrow } ->
      walk ~parent_node ~route m b ~viewport_width ~stale ~theme
        (if viewport_width >= at_least_width then wide else narrow)

and walk_list ~parent_node ~route m b ~viewport_width ~stale ~theme children =
  match children with
  | [] -> ()
  | child :: rest ->
      walk ~parent_node ~route m b ~viewport_width ~stale ~theme child;
      walk_list ~parent_node ~route m b ~viewport_width ~stale ~theme rest

(* The child of [span]: the old list's value when [span] is the span of the
   [previous] cell, so a kept span allocates nothing. *)
and span_child m previous span =
  span.span_pass <- m.pass;
  match previous with
  | (Span old as child) :: _ when old == span -> child
  | _ -> Span span

and new_span ?keyed ~tagged ~memo ~route ~theme ~viewport_width view =
  {
    tagged;
    keyed;
    span_route = route;
    memo;
    span_theme = theme;
    view;
    span_viewport_width = viewport_width;
    span_children = [];
    node_count = 0;
    first_node = None;
    span_focus = Focus_none;
    reused = false;
    span_pass = 0;
  }

(* Like Elm's lazy nodes, an equal memo stops the diff: its span keeps its
   children untouched. Under another theme its view, kept, is reconciled
   again, so that its nodes take the theme. *)
and reconcile_memo ~parent_node ~route ~viewport_width ~stale ~theme previous
    memo =
  match previous with
  | Span ({ memo = Some old; _ } as span) :: _ ->
      let equal = Vnode.memo_equal old memo in
      (* An equal memo keeps the one mounted, whose key was checked. *)
      if not equal then span.memo <- Some memo;
      if
        equal
        && span.span_viewport_width = viewport_width
        && same_theme span.span_theme theme
      then span.reused <- true
      else begin
        if not equal then span.view <- Vnode.memo_view memo;
        span.span_viewport_width <- viewport_width;
        span.span_theme <- theme;
        update_span ~parent_node ~viewport_width ~stale span
      end;
      span
  | _ ->
      Vnode.check_memo memo;
      let span =
        new_span ~tagged:None ~memo:(Some memo) ~route ~theme ~viewport_width
          (Vnode.memo_view memo)
      in
      update_span ~parent_node ~viewport_width ~stale span;
      span

(* As Elm patches a tagger in place: the map's function is stored in its
   span's route cell, and its subtree is reconciled only when it is another
   value. *)
and reconcile_map ~parent_node ~route ~viewport_width ~stale ~theme previous f
    child =
  match previous with
  | Span ({ tagged = Some tagged; _ } as span) :: _ ->
      tagged.tagger <- f;
      if
        span.view == child
        && span.span_viewport_width = viewport_width
        && same_theme span.span_theme theme
      then span.reused <- true
      else begin
        span.view <- child;
        span.span_viewport_width <- viewport_width;
        span.span_theme <- theme;
        update_span ~parent_node ~viewport_width ~stale span
      end;
      span
  | _ ->
      let tagged = { tagger = f; parent = route } in
      let span =
        new_span ~tagged:(Some tagged) ~memo:None ~route:(Tagged tagged) ~theme
          ~viewport_width child
      in
      update_span ~parent_node ~viewport_width ~stale span;
      span

(* A keyed map's bindings are patched by the difference of the map declared
   last and this one: a removed key's span is destroyed, an added key's
   created, and a key whose value is another value is viewed again. When
   [item] is another function, the viewport width or the theme another, or
   the two maps have different orders, every binding is viewed or
   reconciled again instead, in place. Placement puts the bindings' nodes
   in the map's order. *)
and reconcile_keyed ~parent_node ~route ~viewport_width ~stale ~theme previous
    map item =
  match previous with
  | Span ({ keyed = Some k; _ } as span) :: _ ->
      let all =
        k.item != item
        || span.span_viewport_width <> viewport_width
        || (not (same_theme span.span_theme theme))
        || Keyed.order k.declared != Keyed.order map
      in
      k.touched <- [];
      k.touched_all <- all;
      let changed =
        if all then begin
          rebind_all ~parent_node ~route ~viewport_width ~stale ~theme span k
            map item;
          true
        end
        else
          Keyed.symmetric_diff k.declared map
            (fun key change _ ->
              (match change with
              | Keyed.Removed _ -> unbind ~stale span k key
              | Added value ->
                  k.touched <- key :: k.touched;
                  bind ~parent_node ~route ~viewport_width ~stale ~theme span k
                    key (item key value)
              | Changed (_, value) -> (
                  k.touched <- key :: k.touched;
                  match Keyed.find_opt key k.bindings with
                  | Some binding ->
                      rebind ~parent_node ~viewport_width ~stale ~theme span k
                        key binding
                        (Some (item key value))
                  | None -> ()));
              true)
            false
      in
      k.declared <- map;
      k.item <- item;
      span.span_viewport_width <- viewport_width;
      span.span_theme <- theme;
      span.reused <- not changed;
      if changed then keyed_summary span k;
      span
  | _ ->
      let k =
        {
          declared = map;
          item;
          bindings = Keyed.empty ~compare:(Keyed.order map);
          focused = Keyed.empty ~compare:(Keyed.order map);
          touched = [];
          touched_all = true;
        }
      in
      let span =
        new_span ~keyed:k ~tagged:None ~memo:None ~route ~theme ~viewport_width
          Vnode.Empty
      in
      Keyed.iter
        (fun key value ->
          bind ~parent_node ~route ~viewport_width ~stale ~theme span k key
            (item key value))
        map;
      keyed_summary span k;
      span

(* The span of a new binding, reconciled from [view]. *)
and bind ~parent_node ~route ~viewport_width ~stale ~theme span k key view =
  let binding =
    new_span ~tagged:None ~memo:None ~route ~theme ~viewport_width view
  in
  update_span ~parent_node ~viewport_width ~stale binding;
  k.bindings <- Keyed.add key binding k.bindings;
  span.node_count <- span.node_count + binding.node_count;
  note_focus k key binding

and unbind ~stale span k key =
  match Keyed.find_opt key k.bindings with
  | None -> ()
  | Some binding ->
      let fibers, embeds = fibers_and_embeds binding.span_children in
      stale := List.rev_append embeds !stale;
      destroy_fibers fibers;
      k.bindings <- Keyed.remove key k.bindings;
      k.focused <- Keyed.remove key k.focused;
      span.node_count <- span.node_count - binding.node_count

(* Reconciles a binding again, from [view] if it is given, else from the view
   it has. *)
and rebind ~parent_node ~viewport_width ~stale ~theme span k key binding view =
  let count = binding.node_count in
  Option.iter (fun view -> binding.view <- view) view;
  binding.span_viewport_width <- viewport_width;
  binding.span_theme <- theme;
  update_span ~parent_node ~viewport_width ~stale binding;
  span.node_count <- span.node_count - count + binding.node_count;
  note_focus k key binding

and rebind_all ~parent_node ~route ~viewport_width ~stale ~theme span k map item
    =
  let old = k.bindings in
  Keyed.iter
    (fun key _ ->
      if Option.is_none (Keyed.find_opt key map) then unbind ~stale span k key)
    old;
  let same_item = k.item == item in
  Keyed.iter
    (fun key value ->
      match Keyed.find_opt key k.bindings with
      | Some binding ->
          let unchanged =
            same_item
            &&
            match Keyed.find_opt key k.declared with
            | Some declared -> declared == value
            | None -> false
          in
          let view = if unchanged then None else Some (item key value) in
          rebind ~parent_node ~viewport_width ~stale ~theme span k key binding
            view
      | None ->
          bind ~parent_node ~route ~viewport_width ~stale ~theme span k key
            (item key value))
    map;
  if Keyed.order old != Keyed.order map then begin
    let bindings = Keyed.empty ~compare:(Keyed.order map) in
    k.bindings <-
      Keyed.fold
        (fun key binding acc -> Keyed.add key binding acc)
        k.bindings bindings;
    k.focused <-
      Keyed.fold
        (fun key binding acc -> Keyed.add key binding acc)
        k.focused
        (Keyed.empty ~compare:(Keyed.order map))
  end

and note_focus k key binding =
  match binding.span_focus with
  | Focus_none -> k.focused <- Keyed.remove key k.focused
  | Focus_one _ | Focus_many | Modal _ ->
      k.focused <- Keyed.add key binding k.focused

(* The span's focus and first node, from its bindings. *)
and keyed_summary span k =
  span.span_focus <-
    Keyed.fold
      (fun _ binding focus -> combine_focus focus binding.span_focus)
      k.focused Focus_none;
  span.first_node <- Keyed.find_map (fun b -> b.first_node) k.bindings

and update_span ~parent_node ~viewport_width ~stale span =
  let children =
    reconcile_list ~parent_node ~route:span.span_route ~viewport_width ~stale
      ~theme:span.span_theme span.span_children span.view []
  in
  span.span_children <- children;
  span.node_count <- node_count children;
  span.first_node <- first_node children;
  span.span_focus <- children_focus children;
  span.reused <- false

(* ───── Root Reconciliation ───── *)

let render (t : 'msg t) ~viewport_width (vnode : 'msg Vnode.t) : unit =
  if viewport_width < 1 then
    invalid_arg "Reconciler.render: viewport_width must be positive";
  let vnode : msg Vnode.t = Obj.magic vnode in
  let stale = ref [] in
  let children =
    reconcile_list ~parent_node:t.container ~route:t.root ~viewport_width ~stale
      ~theme:None t.root_children vnode []
  in
  t.root_children <- children;
  commit_placement t.container ~stale:!stale children;
  Renderable.request_render t.container

let unmount (t : _ t) : unit =
  let fibers, embeds = fibers_and_embeds t.root_children in
  destroy_fibers ~embeds fibers;
  t.root_children <- [];
  Renderable.request_render t.container

let focused t =
  let focus =
    match children_focus t.root_children with
    | Modal { inside; _ } -> inside
    | focus -> focus
  in
  match focus with
  | Focus_none | Modal _ -> None
  | Focus_one node -> Some node
  | Focus_many -> invalid_arg "Mosaic: more than one focused element"

let modal t =
  match children_focus t.root_children with
  | Modal { modal_node; _ } -> Some modal_node
  | Focus_none | Focus_one _ | Focus_many -> None

let request_close t =
  match children_focus t.root_children with
  | Modal { close; _ } -> close ()
  | Focus_none | Focus_one _ | Focus_many -> false
