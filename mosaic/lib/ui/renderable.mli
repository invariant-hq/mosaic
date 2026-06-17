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

type render = t -> Grid.t -> delta:float -> unit
(** The type for render callbacks. [render self grid ~delta] draws [self] into
    [grid]. [delta] is the elapsed time in milliseconds since the last frame. *)

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
(** [x t] is [t]'s absolute horizontal position in terminal cells. *)

val y : t -> int
(** [y t] is [t]'s absolute vertical position in terminal cells. *)

val width : t -> int
(** [width t] is [t]'s layout width in terminal cells. Returns [0] when [t] is
    hidden or layout has not yet been computed. *)

val height : t -> int
(** [height t] is [t]'s layout height in terminal cells. Returns [0] when [t] is
    hidden or layout has not yet been computed. *)

val bounds : t -> Grid.region
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

val set_render_before : t -> render option -> unit
(** [set_render_before t hook] assigns an optional pre-render hook. [None]
    clears it. *)

val set_render_after : t -> render option -> unit
(** [set_render_after t hook] assigns an optional post-render hook. [None]
    clears it. *)

val set_child_clip : t -> (t -> Grid.region option) option -> unit
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

(** {1:events Events} *)

val on_mouse : t -> (Event.mouse -> unit) -> unit
(** [on_mouse t handler] registers a mouse event handler on [t]. Handlers
    accumulate and run newest-first. Mouse events bubble to ancestors unless
    {!Event.Mouse.stop_propagation} is called. *)

val on_key : t -> (Event.key -> unit) -> unit
(** [on_key t handler] registers a keyboard handler on [t]. Handlers accumulate
    and run newest-first until one calls {!Event.Key.prevent_default}. *)

val on_paste : t -> (Event.paste -> unit) -> unit
(** [on_paste t handler] registers a paste handler on [t]. Handlers accumulate
    and run newest-first until one calls {!Event.Paste.prevent_default}. The
    default paste handler runs afterwards if the event was not prevented. *)

val set_default_key_handler : t -> (Event.key -> unit) option -> unit
(** [set_default_key_handler t handler] assigns a fallback key handler that runs
    after {!on_key} handlers. Only one fallback handler per node. [None] clears
    it. *)

val set_paste_handler : t -> (Event.paste -> unit) option -> unit
(** [set_paste_handler t handler] assigns the default paste handler on [t]. It
    runs after {!on_paste} handlers when the paste event was not prevented. Only
    one default handler per node. [None] clears it. *)

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

(** {1:line_info Line information} *)

type line_info = {
  line_count : int;
  display_line_count : int;
  line_sources : int array;
  line_wrap_indices : int array;
  scroll_y : int;
}
(** The type for display line metrics used by line-numbering widgets.
    - [line_count]: number of logical lines.
    - [display_line_count]: number of visual lines after wrapping.
    - [line_sources.(i)]: logical line index for display line [i].
    - [line_wrap_indices.(i)]: sub-line index within the logical line — [0] for
      the first display line, [1] for the first continuation, and so on.
    - [scroll_y]: vertical scroll offset in display lines. *)

val set_line_info_provider : t -> (unit -> line_info) option -> unit
(** [set_line_info_provider t f] registers a function that supplies line metrics
    for [t]. Used by composite widgets (e.g. line-number gutters) to read
    display line information from a content child without coupling to its
    concrete type. [None] clears the provider. *)

val line_info : t -> line_info option
(** [line_info t] is the current line metrics from [t]'s provider, or [None] if
    no provider is registered. *)

(** {1:pending_work Pending render work} *)

val set_pending_provider : t -> (unit -> Pending.t option) option -> unit
(** [set_pending_provider t f] registers a provider for render work that has not
    yet reached its final visible state. The renderer uses these providers to
    settle snapshots without knowing concrete widget types. [None] clears the
    provider. *)

(** {1:lifecycle Lifecycle} *)

val set_on_frame : t -> (t -> delta:float -> unit) option -> unit
(** [set_on_frame t callback] registers a per-frame update hook.
    [callback t ~delta] is called every frame with [delta] as the elapsed
    milliseconds. [None] clears the hook. *)

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
  (** {2:context Context} *)

  type context = {
    tree : unit Toffee.tree;
    schedule : unit -> unit;
    focus : t -> bool;
    blur : t -> unit;
    get_selection : unit -> Selection.t option;
    request_selection_update : unit -> unit;
    register_lifecycle : t -> unit;
    unregister_lifecycle : t -> unit;
    alloc_num : unit -> int;
    register : t -> unit;
    unregister : t -> unit;
  }
  (** The type for renderer callbacks inherited by all nodes in a tree. *)

  (** {2:root Root construction} *)

  val create_root : context -> ?id:string -> ?style:Toffee.Style.t -> unit -> t
  (** [create_root ctx ()] is a new root node backed by [ctx]'s layout tree.

      Optional parameters:
      - [id]: identifier string. Defaults to ["node-N"].
      - [style]: flexbox style. Defaults to {!Toffee.Style.default}. *)

  (** {2:identity Identity} *)

  val num : t -> int
  (** [num t] is [t]'s numeric identifier, unique within the renderer. *)

  val toffee_node : t -> Toffee.Node_id.t
  (** [toffee_node t] is [t]'s layout node identifier. *)

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

  val pre_render_update : t -> delta:float -> unit
  (** [pre_render_update t ~delta] runs [t]'s on-frame and resize hooks. *)

  val render : t -> Grid.t -> delta:float -> unit
  (** [render t grid ~delta] invokes [t]'s render callback. *)

  val render_before : t -> render option
  (** [render_before t] is [t]'s pre-render hook, if any. *)

  val render_after : t -> render option
  (** [render_after t] is [t]'s post-render hook, if any. *)

  val ensure_frame_buffer : t -> parent:Grid.t -> Grid.t option
  (** [ensure_frame_buffer t ~parent] is [Some buf] if [t] uses buffered
      rendering and has positive dimensions, [None] otherwise. *)

  val blit_frame_buffer : t -> dst:Grid.t -> unit
  (** [blit_frame_buffer t ~dst] copies [t]'s frame buffer into [dst]. *)

  val render_full : t -> grid:Grid.t -> delta:float -> unit
  (** [render_full t ~grid ~delta] runs the complete render sequence for [t]:
      frame buffer selection, pre-render hook, render callback, post-render
      hook, and frame buffer blit. *)

  val pending_work : t -> Pending.t option
  (** [pending_work t] is [Some work] when [t] reports pending render work. *)

  (** {2:children Children} *)

  val children_z : t -> t array
  (** [children_z t] is [t]'s children sorted by z-index. The result is cached.
  *)

  val iter_children_z : t -> (t -> unit) -> unit
  (** [iter_children_z t f] applies [f] to each child of [t] in z-index order.
  *)

  val children_in_viewport :
    parent:t -> viewport:Grid.region -> padding:int -> t list
  (** [children_in_viewport ~parent ~viewport ~padding] is the children of
      [parent] whose bounds intersect [viewport] expanded by [padding] cells,
      sorted by z-index. *)

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

  val set_on_live_count_change : t -> (t -> unit) option -> unit
  (** [set_on_live_count_change t cb] registers an observer called when
      [live_count t] changes. [None] clears it. *)

  val set_lifecycle_pass : t -> (t -> unit) option -> unit
  (** [set_lifecycle_pass t callback] registers a lifecycle pass hook for [t].
      [None] clears it. *)

  val run_lifecycle_pass : t -> unit
  (** [run_lifecycle_pass t] invokes [t]'s lifecycle callback, if any. *)

  (** {2:event_emission Event emission} *)

  val emit_mouse : t -> Event.mouse -> unit
  (** [emit_mouse t event] dispatches [event] to [t]'s mouse handlers, bubbling
      to ancestors unless propagation is stopped via
      {!Event.Mouse.stop_propagation}. *)

  val emit_key : t -> Event.key -> unit
  (** [emit_key t event] dispatches [event] to [t]'s key handlers. *)

  val emit_default_key : t -> Event.key -> unit
  (** [emit_default_key t event] dispatches [event] to [t]'s default key
      handler, if any. *)

  val emit_paste : t -> Event.paste -> unit
  (** [emit_paste t event] dispatches [event] to [t]'s paste handler, if any. *)

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

  (** {2:clipping Child clipping} *)

  val child_clip : t -> Grid.region option
  (** [child_clip t] is the clipping rectangle for [t]'s children as returned by
      [t]'s clip override, or [None] if no override is set. *)
end
