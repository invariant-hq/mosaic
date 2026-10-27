(** Renderer: layout, drawing, hit testing, and event dispatch.

    The renderer drives a three-pass pipeline over a {!Renderable.t} tree:
    - {e Lifecycle}: runs per-frame and resize hooks on registered nodes.
    - {e Layout and command generation}: computes layout via Toffee, walks the
      tree depth-first to extract absolute positions, and builds a flat render
      command list (opacity, scissors, draw calls).
    - {e Execution}: replays the command list to populate the grid and hit grid.

    After the pipeline, {!render} diffs the grid against the previous frame and
    returns minimal ANSI output.

    The renderer owns the root {!Renderable.t}, the {!Matrix_screen.t}, and all
    pipeline state. Widget code builds the tree under {!root}; the event loop
    calls {!render_frame} and {!render}, then dispatches input events. *)

(** {1:types Types} *)

type t
(** The type for renderer state. Owns the root renderable, layout tree, screen,
    and all render pipeline state. *)

(** {1:constructors Constructors} *)

val create :
  ?width_method:Matrix.Text.width_method ->
  ?screen:Matrix.Screen.t ->
  ?style:Toffee.Style.t ->
  ?request_render:(unit -> unit) ->
  ?executor:((unit -> unit) -> unit) ->
  unit ->
  t
(** [create ()] is a renderer with a root renderable and an empty screen. The
    optional parameters are:
    - [width_method]: the text width computation method. Defaults to [`Unicode].
    - [screen]: an existing screen to adopt instead of creating one — pass the
      host runtime's {!Matrix.screen} so {!render_frame} builds directly into it
      and the host presents frames with a single diff. Alpha blending is enabled
      on the adopted screen's grids. [width_method] is ignored when [screen] is
      given.
    - [request_render]: wakes the host event loop when a node requests a frame.
      Defaults to doing nothing. The callback must be thread-safe and must not
      raise or render synchronously; worker highlighters can call it.
    - [executor]: runs work off the rendering thread, such as a threaded
      highlighter's ({!Renderable.off_loop}), and must return without waiting
      for the work. Defaults to running each piece of work on a systhread of its
      own. A host runtime passes its own, so that the work runs where the host's
      other work does.
    - [style]: the root node's initial style. Defaults to
      {!Toffee.Style.default}. *)

(** {1:accessors Accessors} *)

val root : t -> Renderable.t
(** [root t] is the root renderable. Build the UI tree under this node. *)

val screen : t -> Matrix_screen.t
(** [screen t] is the underlying screen. *)

(** {1:pending_work Pending render work} *)

module Pending : sig
  type t
  (** The type for pending render work associated with the node reporting it. *)

  val node : t -> Renderable.t
  (** [node t] is the renderable reporting pending work. *)

  val work : t -> Renderable.Pending.t
  (** [work t] is the pending work description. *)
end

type settle_result = [ `Settled | `Pending of Pending.t list ]
(** The result of a bounded settlement render. *)

val pending_work : t -> Pending.t list
(** [pending_work t] is the pending render work currently reported by visible
    nodes in [t]'s tree. *)

val is_settled : t -> bool
(** [is_settled t] is [true] iff [pending_work t] is empty and no renderable has
    requested another render pass. *)

(** {1:rendering Rendering} *)

val prepare_layout : t -> width:int -> height:int -> unit
(** [prepare_layout t ~width ~height] refreshes layout and widget geometry for
    input dispatch without painting or running animation callbacks. Synchronous
    layout invalidations, including scrollbar visibility changes, settle before
    it returns. Raises [Invalid_argument] if layout callbacks do not converge.

    Size observers (see {!Renderable.set_on_size}) report during each call.
    Between two frames, once a call has delivered reports, later calls deliver
    only reports of nodes deeper than the shallowest node it reported; the
    others stay pending until the next {!layout_frame}, which delivers every
    pending report and requests the render that does so. Depth grows with each
    call that delivers, so repeated calls cannot report forever. This is the
    depth rule of CSS Resize Observer
    ({{:https://www.w3.org/TR/resize-observer/#gather-active-observations-h}
      §3.4.1}). *)

val render_frame :
  ?animate:bool -> t -> width:int -> height:int -> dt:float -> unit
(** [render_frame t ~width ~height ~dt] builds the next frame: it is
    {!layout_frame} followed by {!paint_frame} at the same [height], with the
    same [animate].

    Each call replaces the complete unpresented frame. Blank cells therefore
    belong to the new pass rather than compositing over glyphs from an earlier
    pass, including when {!render_frame_until_settled} renders multiple passes
    before presentation.

    [width] and [height] are the frame dimensions in terminal cells. [dt] is
    elapsed seconds since the last frame. *)

val layout_frame :
  ?animate:bool -> t -> width:int -> height:int -> dt:float -> unit
(** [layout_frame t ~width ~height ~dt] lays out the next frame against [width]
    by [height] cells, for {!paint_frame} to draw. In order, it:
    - Runs lifecycle passes ([on_frame] and resize hooks).
    - Runs frame callbacks (see {!add_frame_callback}).
    - Computes layout via Toffee — skipped when no layout-affecting mutation
      occurred since the last pass and the dimensions are unchanged.
    - Walks the tree: extracts layout and builds the render command list,
      omitting subtrees whose ancestor clip does not intersect the [width] by
      [height] frame. Every pending size report is delivered.
    - Settles layout changes made by update/resize hooks.

    Animation callbacks, the frame callbacks and the [on_frame] hooks of live
    nodes, run once per frame, here, if [animate] is [true], its default. A host
    passes [false] for an early frame ({!Matrix.early}), so that animations keep
    the frame grid's rate. Afterwards the root's {!Renderable.height} is the
    height of the frame's content, which a host can read before it sizes the
    grid the frame is drawn into: a primary-mode host lays the frame out against
    the terminal's height and draws it into a live region as tall as the
    content.

    Raises [Invalid_argument] if layout callbacks do not converge. *)

val paint_frame : t -> height:int -> unit
(** [paint_frame t ~height] draws the frame laid out by the last {!layout_frame}
    into the screen's next grid, as wide as the layout and [height] rows tall;
    rows of the layout below [height] are clipped. It executes the render
    commands, which draw to the grid and populate the hit grid; if drawing
    invalidates layout, it lays the frame out again at the same size, without
    animation callbacks, and rebuilds the unpresented frame. It then updates the
    cursor from the focused node and rechecks hover state against the frame's
    hit grid.

    Raises [Invalid_argument] if no frame is laid out, that is when no
    {!layout_frame} came since the last [paint_frame] or {!prepare_layout}, or
    if drawing repeatedly invalidates layout without converging. External
    asynchronous work remains pending for a later frame. *)

val render : ?full:bool -> t -> string
(** [render t] diffs the current frame against the previous one and returns the
    minimal ANSI output string. Call after {!render_frame}.

    When [full] is [true], all cells are emitted regardless of changes. [full]
    defaults to [false]. Consumes the pending scroll hint (see
    {!take_scroll_hint}), so a lone vertical scroll in a full-width scroll
    container presents as a DECSTBM hardware scroll plus a diff of the revealed
    rows.

    Raises [Invalid_argument] when the renderer adopted a host screen —
    presenting there would corrupt the host runtime's diff baseline, so
    presentation belongs to the host. *)

val take_scroll_hint : t -> Matrix.Screen.scroll_hint option
(** [take_scroll_hint t] is the hardware scroll hint accumulated since the last
    take, and resets it. [Some hint] is returned only when exactly one scroll
    container moved, purely vertically, over a stable full-grid-width row
    region, by less than the region height — the conditions under which a
    DECSTBM row shift beats a cell diff. {!render} consumes the hint
    automatically; a host runtime presenting an adopted screen calls this before
    submitting and forwards the hint (for example via {!Matrix.set_scroll_hint},
    which applies it in [`Alt] mode only). *)

val needs_render : t -> bool
(** [needs_render t] is [true] iff a renderable has requested a re-render or
    live nodes are active. *)

val live : t -> bool
(** [live t] is [true] iff a node of [t]'s tree is live
    ({!Renderable.set_live}), so that a program keeps drawing frames while it
    is. *)

val set_on_live_change : t -> (unit -> unit) option -> unit
(** [set_on_live_change t callback] calls [callback ()] whenever a node of [t]'s
    tree becomes live or stops being live, as one is mounted or unmounted, so
    that a program that draws only when asked reads {!live} and starts drawing
    again. [None] clears it. *)

val render_frame_until_settled :
  ?max_passes:int -> t -> width:int -> height:int -> dt:float -> settle_result
(** [render_frame_until_settled t ~width ~height ~dt] renders bounded frame
    passes until visible nodes report no pending render work and no renderable
    requests a follow-up pass.

    [max_passes] defaults to [4]. The function does not block for external
    asynchronous work; if work remains after the pass budget, it returns
    [`Pending pending]. [pending] may be empty when the remaining work is an
    undescribed render request rather than a pending-work provider. *)

(** {1:events Event dispatch}

    An event is delivered along a {e path}: its target node and the target's
    ancestors. The path is fixed when it is taken, before anything sees the
    event, as in the DOM (DOM Standard, §2.9 "Dispatching events"): a handler
    that moves focus, or a host listener run between taking the path and
    dispatching, changes where the next event goes, not where this one goes.
    Nodes destroyed since the path was taken are skipped.

    Delivery runs each node's handlers, target first, until propagation is
    stopped (see {!Event} for stopping and preventing). Keys and pastes also
    have a {e behavior} (see {!Renderable.set_key_behavior}): the effect the
    event has on the focused widget, such as the character a key types. It runs
    after the target's handlers, unless the default is prevented, on the node
    focused when it runs, which is the target unless focus moved: a listener
    that focuses an input on a printable key gets that character inserted into
    the input, as the input that follows a key press goes to the element focused
    then (UI Events, §3.7.5). A behavior that acts consumes the event: its
    propagation stops, so the target's ancestors and the host never see a key
    typed into an input.

    A host that has listeners of its own takes the path, runs the listeners that
    come first, dispatches, then runs the listeners that come last. *)

type path
(** The type for event paths. *)

val focus_path : t -> path
(** [focus_path t] is the path of keys and pastes: the focused node and its
    ancestors, or no node if nothing is focused. *)

val dispatch_key : t -> path -> Event.key -> unit
(** [dispatch_key t path event] delivers [event] to [path]'s target, runs the
    key behavior of the node focused then unless the default is prevented, and
    delivers [event] to the target's ancestors. *)

val dispatch_paste : t -> path -> Event.paste -> unit
(** [dispatch_paste t path event] is {!dispatch_key} for a paste, whose behavior
    inserts it. *)

val mouse_path : t -> Matrix_input.Mouse_report.t -> path * Event.mouse
(** [mouse_path t report] records the pointer and routes [report] to a target,
    returning the target's path and the event to dispatch along it.

    A primary press without Shift first invokes the focus request of the nearest
    focusable node under the pointer, if that node is unfocused and has one (see
    {!Renderable.set_on_focus_request}): a click never takes focus by itself;
    the focus owner grants it. Hit geometry is refreshed after the request, and
    the press proceeds only if the node is then focused and still contains the
    hit target. A rejected press is returned prevented, with no target, and the
    drag and release that follow it are rejected too. Without a request handler
    the press proceeds without focus moving; a program that owns focus
    imperatively gives the node a request that calls {!val-focus}.

    The target is the node under the pointer, from the latest layout. A wheel
    event over dead space goes to the focused node, whose ancestors reach the
    nearest scrollable one. A left drag captures the node it starts on: until
    the release, every event other than the wheel goes to it, and to no node
    once it is destroyed. The drag and release of a text selection go to the
    node the selection started on. Moving the pointer to another node sends
    [Out] to the node it leaves and [Over] to the node it enters. *)

val dispatch_mouse : t -> path -> Event.mouse -> unit
(** [dispatch_mouse t path event] delivers [event] to [path], then, unless the
    default is prevented, runs the mouse defaults:
    - a primary press without Ctrl on a selectable node starts a text selection
      (an editor's caret is one), if the node is focused or has nothing
      focusable above it; another primary press clears the selection;
    - a primary drag extends the selection;
    - a release that ends a capturing drag sends [Drop] to the node under the
      pointer;
    - a primary press and a release over the same node, without a drag between
      them, is a click: the node's click handler runs (see
      {!Renderable.set_on_click}), then those of its ancestors the release is
      also over.

    The release that ends a capturing drag first sends [Drag_end] to the
    captured node; the release of a selection ends it, prevented or not. *)

(** {1:focus Focus} *)

val focused : t -> Renderable.t option
(** [focused t] is the currently focused renderable, if any. Destroying,
    detaching or hiding it leaves nothing focused: nothing moves focus to
    another node. *)

val focus : t -> Renderable.t -> bool
(** [focus t node] focuses [node] and is [true] iff [node] is focusable and not
    inert. *)

val blur : t -> unit
(** [blur t] removes focus from the currently focused renderable. *)

val request_focus : t -> [ `Next | `Prev ] -> bool
(** [request_focus t direction] invokes the focus request of the stop after
    ([`Next]) or before ([`Prev]) the focused node, and is [true] iff it invoked
    one. A {e stop} is a node with a focus request
    ({!Renderable.set_on_focus_request}) that is shown, as its ancestors are,
    and not inert. Stops come in tree order, a node before its children, and the
    first stop follows the last. With nothing focused, the stop is the first
    one, or the last for [`Prev]. The focused node is never its own next stop.
    As for a press ({!mouse_path}), the request's owner grants focus or not. *)

val set_modal : t -> Renderable.t option -> unit
(** [set_modal t m] makes [m] the {e modal} node, or sets none. The modal node
    is in the {e top layer}, as HTML's modal dialog is: it stays where it is in
    the tree, for events and its children, and is laid out against the root, as
    the root of its own layout, painted after every other node and clipped by
    none of its ancestors, so the pointer finds it first. Every position read
    from it and its descendants, by hit-testing, a text selection, a mouse
    event's local cell or a scroll hint, is where it is painted
    ({!Renderable.Private.set_top_layer}). Every node but it and its descendants
    is inert ({!Renderable.set_inert}), so a focus move ({!request_focus}) stays
    in it. A focused node that becomes inert is blurred. Once [m] is no longer
    modal it is laid out and painted in its parent again. *)

val modal : t -> Renderable.t option
(** [modal t] is the modal node, if any. *)

val set_theme : t -> Theme.t -> unit
(** [set_theme t theme] makes [theme] the theme in scope at [t]'s root
    ({!Renderable.theme}), and asks for a frame, which draws every node with it.
    A new renderer's theme is {!Theme.plain}. *)

val theme : t -> Theme.t
(** [theme t] is the theme in scope at [t]'s root. *)

(** {1:selection Selection} *)

val selection : t -> Selection.t option
(** [selection t] is the active text selection, if any. *)

val selection_text : t -> string option
(** [selection_text t] is the text of the active selection, gathered in document
    order from the selectables under its container, or [None] when no selection
    is active or the selection is empty. *)

val clear_selection : t -> unit
(** [clear_selection t] clears the active text selection, notifying all
    selectable renderables under the selection container. *)

(** {1:drag Drag capture} *)

val captured : t -> Renderable.t option
(** [captured t] is the renderable currently capturing all mouse events during a
    drag gesture, if any. *)

(** {1:hover Hover} *)

val hover : t -> Renderable.t option
(** [hover t] is the renderable currently under the mouse pointer, if any. *)

(** {1:frame_callbacks Frame callbacks} *)

val add_frame_callback : t -> (dt:float -> unit) -> unit
(** [add_frame_callback t f] registers [f] to run at the start of each frame, as
    [f ~dt] with [dt] the frame's delta time in seconds. Callbacks run after
    lifecycle passes and before layout computation.

    See also {!remove_frame_callback} and {!clear_frame_callbacks}. *)

val remove_frame_callback : t -> (dt:float -> unit) -> unit
(** [remove_frame_callback t f] unregisters [f] using physical equality.

    See also {!add_frame_callback}. *)

val clear_frame_callbacks : t -> unit
(** [clear_frame_callbacks t] removes all registered frame callbacks.

    See also {!add_frame_callback}. *)
