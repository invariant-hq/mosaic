(** Mutable UI tree nodes with layout integration.

    A {!t} combines a layout node with rendering callbacks, event handlers,
    focus state, and lifecycle hooks. Nodes form a mutable tree: parent-child
    relationships drive both layout computation and rendering order.

    Widgets create nodes with {!create}, configure them with setters, and render
    content through callbacks. The renderer drives the pipeline through
    {!Private}. *)

(** {1:types Types} *)

type t
(** The type for mutable nodes in the UI tree. *)

type render = t -> Matrix_grid.t -> dt:float -> unit
(** The type for render callbacks. [render self grid ~dt] draws [self] into
    [grid]. [dt] is the elapsed time in seconds since the last frame. *)

type measure =
  known_dimensions:float option Toffee.Geometry.Size.t ->
  available_space:Toffee.Available_space.t Toffee.Geometry.Size.t ->
  style:Toffee.Style.t ->
  float Toffee.Geometry.Size.t
(** The type for intrinsic size computation callbacks. Called during layout when
    a node lacks explicit dimensions. *)

type cursor = {
  x : int;
  y : int;
  style : [ `Block | `Line | `Underline ];
  color : Ansi.Color.t;
  blinking : bool;
}
(** The type for hardware cursor descriptions. Coordinates are absolute terminal
    cell positions. *)

val equal_cursor : cursor -> cursor -> bool
(** [equal_cursor a b] is [true] iff all fields of [a] and [b] match. *)

val pp_cursor : Format.formatter -> cursor -> unit
(** [pp_cursor ppf c] formats [c] on [ppf] for debugging. *)

module Pending : sig
  type t = private { kind : string; label : string option }
  (** The type for pending render work reported by a node.

      [kind] identifies the producer class, for example ["code.highlight"].
      [label] optionally identifies the specific work item for diagnostics. *)

  val make : ?label:string -> kind:string -> unit -> t
  (** [make ~kind ()] is a pending render-work description. *)
end

(** {1:constructors Constructors} *)

val create :
  parent:t ->
  ?index:int ->
  ?id:string ->
  ?style:Toffee.Style.t ->
  ?visible:bool ->
  ?z_index:int ->
  ?opacity:float ->
  ?live:bool ->
  ?render:render ->
  unit ->
  t
(** [create ~parent ()] is a new node attached as a child of
    [child_target parent]. When [index] is omitted the node is appended last;
    otherwise it is inserted at the given position, clamped to
    \[[0];[child_count]\].

    Optional parameters:
    - [id]: unique identifier string. Defaults to ["node-N"].
    - [style]: flexbox style. Defaults to {!Toffee.Style.default}.
    - [visible]: initial visibility. Defaults to [true].
    - [z_index]: rendering order; higher values render on top. Defaults to [0].
    - [opacity]: opacity clamped to \[[0.0];[1.0]\]. Defaults to [1.0].
    - [live]: continuous rendering every frame. Defaults to [false].

    Raises [Invalid_argument] if the layout tree rejects the node. *)

val attach : parent:t -> ?index:int -> t -> unit
(** [attach ~parent t] attaches [t] as a child of [child_target parent],
    detaching [t] from any previous parent first. When [index] is omitted the
    node is appended last.

    Raises [Invalid_argument] if [parent] and [t] belong to different trees, or
    if either node is destroyed. *)

val detach : t -> unit
(** [detach t] removes [t] from its parent. The node remains valid and can be
    reattached with {!attach}. Blurs [t] if focused. No-op if already detached.
*)

val destroy : t -> unit
(** [destroy t] detaches [t], removes all children (clearing their parent
    pointers and lifecycle registrations), frees its layout node, and clears all
    handlers. The node becomes unusable. No-op if already destroyed.

    Children are removed but not themselves destroyed — they become orphaned
    nodes that can be reattached elsewhere. Use {!destroy_recursively} to
    destroy the entire subtree. *)

val destroy_recursively : t -> unit
(** [destroy_recursively t] destroys [t] and all descendants depth-first. See
    also {!destroy}. *)

val destroyed : t -> bool
(** [destroyed t] is [true] iff {!destroy} has been called on [t]. *)

(** {1:identity Identity} *)

val id : t -> string
(** [id t] is [t]'s string identifier. *)

val set_id : t -> string -> unit
(** [set_id t id] replaces [t]'s string identifier. Hosts that address nodes by
    id (e.g. focus targeting) observe the new value immediately. *)

val parent : t -> t option
(** [parent t] is [t]'s parent, or [None] if [t] is detached or a root. *)

val children : t -> t list
(** [children t] is [t]'s children in insertion order. The list is a snapshot;
    subsequent mutations are not reflected. *)

val child_target : t -> t
(** [child_target t] is the node that receives children on behalf of [t].
    Defaults to [t] itself. Composite widgets override this to redirect children
    to an internal container (e.g. a scroll box routes children to its content
    node).

    See also {!set_child_target}. *)

val set_child_target : t -> t option -> unit
(** [set_child_target t target] sets the node that receives children on behalf
    of [t]. [None] resets to [t] itself. *)

(** {1:layout Layout} *)

val set_style : t -> Toffee.Style.t -> unit
(** [set_style t style] updates [t]'s flexbox style, marks layout dirty, and
    schedules a re-render. When explicit dimensions are present and
    [flex_shrink] is still at its default [1.0], it is automatically set to
    [0.0] to match terminal layout conventions. *)

val style : t -> Toffee.Style.t
(** [style t] is [t]'s current flexbox style. *)

val set_measure : t -> measure option -> unit
(** [set_measure t fn] assigns a custom measure function for intrinsic sizing
    and marks layout dirty. [None] clears it. *)

val mark_dirty : t -> unit
(** [mark_dirty t] flags [t] for re-layout on the next frame. *)

val x : t -> int
(** [x t] is [t]'s rounded absolute left edge in terminal cells, plus any
    translation inherited from [t] or its ancestors. *)

val y : t -> int
(** [y t] is [t]'s rounded absolute top edge in terminal cells, plus any
    translation inherited from [t] or its ancestors. *)

val width : t -> int
(** [width t] is the distance between [t]'s rounded absolute horizontal edges in
    terminal cells. Adjacent Toffee edges therefore quantize to the same cell
    boundary, and a zero-width allocation remains zero. Translation does not
    affect the result. Returns [0] when [t] is hidden or layout has not yet been
    computed. *)

val height : t -> int
(** [height t] is the distance between [t]'s rounded absolute vertical edges in
    terminal cells. Adjacent Toffee edges therefore quantize to the same cell
    boundary, and a zero-height allocation remains zero. Translation does not
    affect the result. Returns [0] when [t] is hidden or layout has not yet been
    computed. *)

val bounds : t -> Matrix_grid.region
(** [bounds t] is [{ x = x t; y = y t; width = width t; height = height t }]. *)

val set_translate : t -> x:int -> y:int -> unit
(** [set_translate t ~x ~y] shifts [t]'s rendering position by [(x, y)] without
    affecting layout. Useful for scrolling. *)

val translate : t -> int * int
(** [translate t] is [t]'s current translation offset as [(x, y)]. *)

(** {1:rendering Rendering} *)

val set_render : t -> render -> unit
(** [set_render t fn] replaces [t]'s render callback and schedules a re-render.
*)

val request_render : t -> unit
(** [request_render t] schedules a re-render for [t]. *)

val off_loop : t -> (unit -> unit) -> unit
(** [off_loop t work] runs [work] off the rendering thread, on the executor of
    [t]'s renderer ({!Renderer.create}). [work] must not touch renderables, and
    gives its result back by publishing it and calling {!request_render}. *)

val set_render_before : t -> render option -> unit
(** [set_render_before t hook] assigns an optional pre-render hook. [None]
    clears it. *)

val set_render_after : t -> render option -> unit
(** [set_render_after t hook] assigns an optional post-render hook. [None]
    clears it. *)

val set_child_clip : t -> (t -> Matrix_grid.region option) option -> unit
(** [set_child_clip t fn] overrides the clipping rectangle applied to [t]'s
    children. When set, the renderer calls [fn t] to obtain a scissor rectangle
    before rendering children. Useful for scroll containers and bordered boxes.
    [None] clears the override. *)

(** {1:visual Visual properties} *)

val set_visible : t -> bool -> unit
(** [set_visible t v] shows or hides [t]. Blurs [t] if it is focused while being
    hidden. *)

val visible : t -> bool
(** [visible t] is [true] iff [t] is currently visible. *)

val set_z_index : t -> int -> unit
(** [set_z_index t z] sets [t]'s rendering order. Higher values render on top.
*)

val z_index : t -> int
(** [z_index t] is [t]'s z-index. *)

val set_opacity : t -> float -> unit
(** [set_opacity t v] sets [t]'s opacity, clamped to \[[0.0];[1.0]\]. Values
    below [1.0] cause the renderer to push an opacity context around [t] and its
    descendants. *)

val opacity : t -> float
(** [opacity t] is [t]'s opacity in \[[0.0];[1.0]\]. *)

val set_buffered : t -> bool -> unit
(** [set_buffered t v] enables or disables offscreen buffering. When [true], [t]
    renders into a private grid that is blitted to the parent grid. *)

val buffered : t -> bool
(** [buffered t] is [true] iff offscreen buffering is enabled. *)

val set_live : t -> bool -> unit
(** [set_live t v] enables or disables continuous rendering. When live, the
    renderer runs [t]'s render callback every frame rather than only on request.
*)

val live : t -> bool
(** [live t] is [true] iff continuous rendering is enabled. *)

(** {1:focus Focus} *)

val set_focusable : t -> bool -> unit
(** [set_focusable t v] marks [t] as focusable or not. *)

val focusable : t -> bool
(** [focusable t] is [true] iff [t] can receive focus. *)

val focused : t -> bool
(** [focused t] is [true] iff [t] currently holds focus. *)

val focus : t -> bool
(** [focus t] requests focus for [t]. Returns [false] if [t] is not focusable.
    Delegates to the renderer's focus controller. *)

val blur : t -> unit
(** [blur t] removes focus from [t]. No-op if [t] is not focused. *)

val set_cursor_provider : t -> (t -> cursor option) -> unit
(** [set_cursor_provider t f] registers a hardware cursor provider consulted
    when [t] is focused. [f] is called with [t] and returns the desired cursor
    state, or [None] to hide the cursor. *)

val clear_cursor_provider : t -> unit
(** [clear_cursor_provider t] removes [t]'s cursor provider. *)

val cursor : t -> cursor option
(** [cursor t] is the current hardware cursor state as reported by [t]'s cursor
    provider, or [None] if no provider is set. *)

val set_on_focus : t -> (t -> unit) option -> unit
(** [set_on_focus t callback] registers a focus lifecycle hook. The callback is
    called immediately after [t] becomes focused. [None] clears the hook. *)

val set_on_blur : t -> (t -> unit) option -> unit
(** [set_on_blur t callback] registers a blur lifecycle hook. The callback is
    called immediately after [t] loses focus. [None] clears the hook. *)

(** {1:events Events}

    An event reaches a node through the renderer, which delivers it along a path
    of nodes (see {!Renderer} for each kind of event and {!Event} for what
    stopping and preventing mean). At each node the node's handlers run, newest
    first, and all of them run: stopping propagation keeps the event from the
    other nodes, never from the rest of this one. A handler that destroys its
    node ends the node's run.

    A {e behavior} is what an event does to a widget when nothing prevents it:
    the character a key types into an editor, the selection a key moves, the
    text a paste inserts. A node has at most one key behavior and one paste
    behavior; the renderer runs it after the handlers, on the focused node,
    unless the event's default is prevented. A behavior that acts on an event
    prevents its default, and the renderer then stops its propagation: the event
    is consumed. The keys a widget's behavior acts on are part of the widget's
    interface, listed in its module. *)

val on_mouse : t -> (Event.mouse -> unit) -> unit
(** [on_mouse t handler] registers a mouse handler on [t]. *)

val on_key : t -> (Event.key -> unit) -> unit
(** [on_key t handler] registers a key handler on [t]. *)

val on_paste : t -> (Event.paste -> unit) -> unit
(** [on_paste t handler] registers a paste handler on [t]. *)

val set_key_behavior : t -> (Event.key -> unit) option -> unit
(** [set_key_behavior t behavior] sets [t]'s key behavior, replacing any
    previous one; [None] removes it. [behavior] calls
    {!Event.Key.prevent_default} when it acts on a key. *)

val set_paste_behavior : t -> (Event.paste -> unit) option -> unit
(** [set_paste_behavior t behavior] sets [t]'s paste behavior, replacing any
    previous one; [None] removes it. [behavior] calls
    {!Event.Paste.prevent_default} when it acts on a paste. *)

(** {1:selection Selection} *)

val set_selection :
  t ->
  should_start:(x:int -> y:int -> bool) ->
  on_change:(Selection.t option -> bool) ->
  clear:(unit -> unit) ->
  get_text:(unit -> string) ->
  unit
(** [set_selection t ~should_start ~on_change ~clear ~get_text] enables text
    selection on [t]. The renderer calls these callbacks during mouse
    interactions and clipboard operations. The contract is:
    - [should_start ~x ~y] is [true] iff a selection drag starting at [(x, y)]
      should be initiated.
    - [on_change sel] is called when the selection changes; returns [true] to
      accept the selection.
    - [clear ()] discards any active selection state.
    - [get_text ()] returns the currently selected text. *)

val unset_selection : t -> unit
(** [unset_selection t] disables text selection on [t]. *)

val selectable : t -> bool
(** [selectable t] is [true] iff [t] has selection callbacks registered. *)

(** {1:text_rows Text rows} *)

type text_rows = { layout : Matrix.Text.Layout.t; scroll_y : int }
(** The type for the rows a text node shows.
    - [layout] is the node's current geometry: one immutable value whose counts
      and row queries all describe the same source and width. Its rows are
      absolute visual rows, before scrolling.
    - [scroll_y] is the absolute row shown at the node's top edge.

    A row [r] of the node's own coordinates is the layout's row [scroll_y + r].
    Read only the rows you draw, with {!Matrix.Text.Layout.iter_rows}; a reader
    never needs every row unless it aligns the whole text. *)

val set_text_rows_provider : t -> (unit -> text_rows) option -> unit
(** [set_text_rows_provider t f] registers a function that supplies [t]'s rows.
    Composite widgets, such as line-number gutters, read a content child's rows
    through it without depending on the child's concrete type. [None] clears the
    provider. *)

val text_rows : t -> text_rows option
(** [text_rows t] is the current rows from [t]'s provider, or [None] if no
    provider is registered. *)

(** {1:pending_work Pending render work} *)

val set_pending_provider : t -> (unit -> Pending.t option) option -> unit
(** [set_pending_provider t f] registers a provider for render work that has not
    yet reached its final visible state. The renderer uses these providers to
    settle snapshots without knowing concrete widget types. [None] clears the
    provider. *)

(** {1:lifecycle Lifecycle} *)

val set_on_update : t -> (t -> dt:float -> unit) option -> unit
(** [set_on_update t callback] registers a per-frame state-update hook.
    [callback t ~dt] runs during frame traversal, after [t]'s layout is
    refreshed and before its children are traversed, culled, or rendered. Unlike
    {!set_on_frame} it is not gated on {!set_live}. Use it for state that must
    settle before descendants are processed — for example a scroll container
    maintaining its scroll offset — and keep drawing in the render callback.
    [None] clears the hook. *)

val set_on_frame : t -> (t -> dt:float -> unit) option -> unit
(** [set_on_frame t callback] registers a per-frame update hook.
    [callback t ~dt] is called every frame with [dt] as the elapsed seconds, but
    only while [t] is live: pair the hook with {!set_live} to start and stop the
    animation it drives. The hook runs only when [t] is part of the frame: a
    child culled by a scroll container's viewport (see
    {!Private.set_viewport_cull}) receives no ticks until it scrolls back into
    view. [None] clears the hook. *)

val set_on_resize : t -> (t -> unit) option -> unit
(** [set_on_resize t callback] registers a size-change hook. The callback is
    called when {!width} or {!height} changes after layout. [None] clears the
    hook. *)

(** {1:fmt Formatting and inspecting} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] formats [t] on [ppf] for debugging. *)

(** {1:private Renderer integration}

    Privileged operations used by the renderer during the render pipeline and
    event dispatch. Widget authors must not call these directly. *)

module Private : sig
  val set_focus_request : t -> (unit -> unit) option -> unit
  (** [set_focus_request t callback] sets the application-owned focus request.
      It is separate from the focus lifecycle hook. *)

  val request_focus : t -> bool
  (** [request_focus t] invokes its request callback, if alive, and reports
      whether one was invoked. The callback may reconcile or destroy [t]. *)

  (** {2:context Context} *)

  type context = {
    tree : unit Toffee.tree;
    schedule : unit -> unit;
        (** Requests a render. Must be safe to call from a worker thread. *)
    off_loop : (unit -> unit) -> unit;
        (** Runs work off the rendering thread ({!val-off_loop}). *)
    focus : t -> bool;
    blur : t -> unit;
    get_selection : unit -> Selection.t option;
    request_selection_update : unit -> unit;
    register_lifecycle : t -> unit;
    unregister_lifecycle : t -> unit;
    alloc_num : unit -> int;
    register : t -> unit;
    unregister : t -> unit;
    width_method : unit -> Matrix.Text.width_method;
        (** The width computation method of the screen the tree renders into,
            read without side effects. Widgets read it (see {!val-width_method})
            at every measurement so measurement matches how cells are laid down.
        *)
    cell_size : unit -> (int * int) option;
        (** The size in pixels of the terminal's cells, as the screen the tree
            renders into was given it, read without side effects. *)
    report_scroll : t -> region:Matrix_grid.region -> dx:int -> dy:int -> unit;
        (** Reports that a scroll container shifted its content by [(dx, dy)]
            cells. [region]'s rows are the viewport rows that visually shift;
            its [x]/[width] are the container's horizontal footprint. The
            renderer may turn a lone vertical shift into a hardware scroll hint
            for presentation (see {!Renderer.take_scroll_hint}). *)
    admit_size_report : t -> bool;
        (** Decides whether a node may deliver its changed size to its size
            observer now. A refused report stays pending and is offered again at
            the node's next traversal. *)
  }
  (** The type for renderer callbacks inherited by all nodes in a tree. *)

  (** {2:root Root construction} *)

  val create_root : context -> ?id:string -> ?style:Toffee.Style.t -> unit -> t
  (** [create_root ctx ()] is a new root node backed by [ctx]'s layout tree.

      Optional parameters:
      - [id]: identifier string. Defaults to ["node-N"].
      - [style]: flexbox style. Defaults to {!Toffee.Style.default}. *)

  val set_cleanup : t -> (unit -> unit) -> unit
  (** [set_cleanup t f] installs the widget's resource cleanup callback,
      replacing any previous callback. Destruction calls it exactly once, after
      detaching the node. [f] must not raise or mutate the destroyed node.
      Raises [Invalid_argument] if [t] is already destroyed. *)

  (** {2:identity Identity} *)

  val num : t -> int
  (** [num t] is [t]'s numeric identifier, unique within the renderer. *)

  val detach_all : t list -> unit
  (** [detach_all nodes] detaches each of [nodes] as {!detach} does, in time
      linear in the number of children their parents had. *)

  val child_at : t -> int -> t option
  (** [child_at t i] is [t]'s [i]th child, or [None] when [i] is out of bounds.
      O(1) and allocation-free, unlike {!children} which materializes a fresh
      list. *)

  val toffee_node : t -> Toffee.Node_id.t
  (** [toffee_node t] is [t]'s layout node identifier. *)

  val width_method : t -> Matrix.Text.width_method
  (** [width_method t] is the width computation method of the screen [t]'s tree
      renders into, now. The screen owns it and can change it while the tree
      lives, as when the terminal reports its method late, so a widget reads it
      at every measurement and paint and keys what it caches by it: wrap points,
      intrinsic widths, and cursor columns then agree with how the screen lays
      down cells. When it changes, the renderer marks every measured node dirty
      before the next layout. *)

  val cell_size : t -> (int * int) option
  (** [cell_size t] is the size in pixels of the cells of the terminal [t]'s
      tree renders into, [None] while it is not known. When it changes, the
      renderer marks every measured node dirty before the next layout. *)

  val set_is_root : t -> bool -> unit
  (** [set_is_root t v] marks or unmarks [t] as a root node. *)

  (** {2:layout_cache Layout cache} *)

  val layout_dirty : t -> bool
  (** [layout_dirty t] is [true] iff [t] needs re-layout. *)

  val clear_layout_dirty : t -> unit
  (** [clear_layout_dirty t] clears [t]'s layout dirty flag. *)

  val update_layout :
    t -> x:float -> y:float -> width:float -> height:float -> unit
  (** [update_layout t ~x ~y ~width ~height] caches layout results for [t],
      enabling O(1) queries via {!val-x}, {!val-y}, {!val-width}, and
      {!val-height}. *)

  val measure : t -> measure option
  (** [measure t] is [t]'s measure function, if any. *)

  (** {2:render_pipeline Render pipeline} *)

  val pre_render_update : ?frame:bool -> t -> dt:float -> unit
  (** [pre_render_update t ~dt] runs update and resize hooks. [frame] defaults
      to [true]; [false] skips animation hooks during layout settlement. *)

  val render : t -> Matrix_grid.t -> dt:float -> unit
  (** [render t grid ~dt] invokes [t]'s render callback. *)

  val render_before : t -> render option
  (** [render_before t] is [t]'s pre-render hook, if any. *)

  val render_after : t -> render option
  (** [render_after t] is [t]'s post-render hook, if any. *)

  val ensure_frame_buffer : t -> parent:Matrix_grid.t -> Matrix_grid.t option
  (** [ensure_frame_buffer t ~parent] is [Some buf] if [t] uses buffered
      rendering and has positive dimensions, [None] otherwise. *)

  val blit_frame_buffer : t -> dst:Matrix_grid.t -> unit
  (** [blit_frame_buffer t ~dst] copies [t]'s frame buffer into [dst]. *)

  val render_full : t -> grid:Matrix_grid.t -> dt:float -> unit
  (** [render_full t ~grid ~dt] runs the complete render sequence for [t]: frame
      buffer selection, pre-render hook, render callback, post-render hook, and
      frame buffer blit. *)

  val pending_work : t -> Pending.t option
  (** [pending_work t] is [Some work] when [t] reports pending render work. *)

  (** {2:children Children} *)

  val children_z : t -> t array
  (** [children_z t] is [t]'s children sorted by z-index. The result is cached.
  *)

  val iter_children_z : t -> (t -> unit) -> unit
  (** [iter_children_z t f] applies [f] to each child of [t] in z-index order.
  *)

  val children_in_viewport : parent:t -> viewport:Matrix_grid.region -> t list
  (** [children_in_viewport ~parent ~viewport] is the visible children of
      [parent] whose subtrees may paint inside [viewport], in z-index order.

      A subtree may paint inside its node's box, grown on both axes by its
      visible children's subtrees unless the node clips them. A child kept here
      may still paint nothing; a child left out cannot paint inside [viewport].
      A region with zero extent on an axis is kept on that axis when it lies on
      [viewport]'s closed interval. *)

  val iter_children_in_viewport :
    parent:t -> viewport:Matrix_grid.region -> (t -> unit) -> unit
  (** [iter_children_in_viewport ~parent ~viewport f] applies [f] to
      {!children_in_viewport}, in order. The cost is that of a binary search
      over [parent]'s children and of the children kept, unless the layout of
      [parent]'s subtree, its children or their translations changed since the
      last call; then it is linear in the children, and their cached layouts are
      refreshed from the layout tree. *)

  val set_viewport_cull : t -> bool -> unit
  (** [set_viewport_cull t v] enables or disables viewport culling of [t]'s
      children. When enabled, the renderer traverses only the children whose
      subtrees may paint inside the effective ancestor clip (via
      {!iter_children_in_viewport}); offscreen children are neither updated
      ([on_frame], resize hooks) nor rendered nor hit-testable until they scroll
      back into view. Scroll containers enable this on their content node. *)

  val viewport_cull : t -> bool
  (** [viewport_cull t] is [true] iff viewport culling is enabled on [t]. *)

  (** {2:focus Focus} *)

  val focus_direct : t -> bool
  (** [focus_direct t] focuses [t] without delegating to the renderer. Returns
      [false] if [t] is not focusable. *)

  val blur_direct : t -> unit
  (** [blur_direct t] blurs [t] without delegating to the renderer. *)

  (** {2:lifecycle Lifecycle} *)

  val live_count : t -> int
  (** [live_count t] is the total number of live-rendering nodes in [t]'s
      subtree, including [t] itself. *)

  val set_size_observer : t -> (t -> unit) option -> unit
  (** [set_size_observer t callback] observes size changes without replacing the
      widget's internal resize hook. During a traversal, [callback] runs when
      the node's size differs from the size it last reported and the context's
      [admit_size_report] admits the report. *)

  val set_on_live_count_change : t -> (t -> unit) option -> unit
  (** [set_on_live_count_change t cb] registers an observer called when
      [live_count t] changes. [None] clears it. *)

  val set_lifecycle_pass : t -> (t -> unit) option -> unit
  (** [set_lifecycle_pass t callback] registers a lifecycle pass hook for [t].
      [None] clears it. *)

  val run_lifecycle_pass : t -> unit
  (** [run_lifecycle_pass t] invokes [t]'s lifecycle callback, if any. *)

  (** {2:event_emission Event emission} *)

  val set_click_handler : t -> (Event.mouse -> unit) option -> unit
  (** [set_click_handler t handler] installs the callback for completed clicks.
      The renderer validates the press/release gesture before calling it. *)

  val emit_click : t -> Event.mouse -> unit
  (** [emit_click t event] calls [t]'s click handler without bubbling. *)

  val emit_mouse : t -> Event.mouse -> unit
  (** [emit_mouse t event] runs [t]'s mouse handlers on [event]. *)

  val emit_key : t -> Event.key -> unit
  (** [emit_key t event] runs [t]'s key handlers on [event]. *)

  val run_key_behavior : t -> Event.key -> unit
  (** [run_key_behavior t event] runs [t]'s key behavior on [event], if [t] has
      one. *)

  val emit_paste : t -> Event.paste -> unit
  (** [emit_paste t event] runs [t]'s paste handlers on [event]. *)

  val run_paste_behavior : t -> Event.paste -> unit
  (** [run_paste_behavior t event] runs [t]'s paste behavior on [event], if [t]
      has one. *)

  (** {2:selection Selection} *)

  val emit_selection_changed : t -> Selection.t option -> bool
  (** [emit_selection_changed t sel] notifies [t]'s selection [on_change]
      callback with [sel]. Returns [false] if no callback is registered. *)

  val clear_selection : t -> unit
  (** [clear_selection t] calls [t]'s selection [clear] callback, if any. *)

  val should_start_selection : t -> x:int -> y:int -> bool
  (** [should_start_selection t ~x ~y] is [true] iff [t]'s [should_start]
      callback returns [true] for [(x, y)]. Returns [false] if no callback is
      registered. *)

  val get_selected_text : t -> string
  (** [get_selected_text t] calls [t]'s [get_text] callback and returns the
      result, or [""] if no callback is registered. *)

  val get_selection : t -> Selection.t option
  (** [get_selection t] is the renderer's active text selection, if any. *)

  val request_selection_update : t -> unit
  (** [request_selection_update t] asks the renderer to re-evaluate the active
      selection against the current pointer position. *)

  val report_scroll : t -> region:Matrix_grid.region -> dx:int -> dy:int -> unit
  (** [report_scroll t ~region ~dx ~dy] forwards a scroll movement to the
      renderer's [report_scroll] context callback. See {!type-context}. *)

  (** {2:clipping Child clipping} *)

  val child_clip : t -> Matrix_grid.region option
  (** [child_clip t] is the clipping region for [t]'s children: the clip
      override's result when one is set, otherwise [t]'s bounds. The region may
      be empty; the renderer then skips [t]'s children entirely rather than
      painting them unclipped. *)
end
