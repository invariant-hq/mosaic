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

val mouse_path :
  t -> now:int -> Matrix_input.Mouse_report.t -> path * Event.mouse
(** [mouse_path t ~now report] records the pointer and routes [report] to a
    target, returning the target's path and the event to dispatch along it.
    [now] is the time, in nanoseconds, of the turn that reads [report], such as
    {!Matrix.now}: it times a press for its [clicks] ({!Event.Mouse.kind}), so
    their window is accurate to one turn.

    A press is {e held} until the release of its button; a release that does not
    say which button (X10, URXVT) is the held button's. While a press is held,
    another button's press or release leaves it alone, and a press of its button
    or a motion with no button means its release was lost: before the report is
    routed, the press ends as {!pointer_lost} ends it. A drag of the held button
    reported at the pressed cell before the pointer has left it, which some
    terminals send, is no drag: it is returned prevented, with no target. A
    release's [dragged] says whether the pointer left the pressed cell.

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
    nearest scrollable one. A left drag captures the node pressed: until the
    press ends, every event other than the wheel goes to it, and to no node once
    it is destroyed. Moving the pointer to another node sends [Out] to the node
    it leaves and [Over] to the node it enters. *)

val pointer_lost : t -> unit
(** [pointer_lost t] ends the held press, if any, as a lost release: its release
    will not come, as when the terminal's window loses focus. The gesture
    settles where the pointer was last reported: the text selection it drags
    ends there, the node it captured gets [Drag_end] and no [Drop], and nothing
    is clicked. A host calls it when the terminal reports a focus loss. *)

val dispatch_mouse : t -> path -> Event.mouse -> unit
(** [dispatch_mouse t path event] delivers [event] to [path], then, unless the
    default is prevented, runs the mouse defaults:
    - a primary press starts a selection ({!section-selection}) on a node that
      does not read [`Off] ({!Renderable.selectable}), or with Shift moves an
      end of the selection there is, clears it on an editor
      ({!Renderable.Private.set_owned_text}), which selects on its own, and
      leaves it as it is on a node that reads [`Off];
    - a primary drag moves the focus of the selection its press started;
    - a release that ends a capturing drag sends [Drop] to the node under the
      pointer;
    - a primary press and a release over the same node, without a drag between
      them, is a click: the node's click handler runs (see
      {!Renderable.set_on_click}), then those of its ancestors the release is
      also over.

    The release that ends a capturing drag first sends [Drag_end] to the
    captured node; the release of the press that started a selection settles it
    ({!take_selection_changes}), prevented or not. *)

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

(** {1:selection Selection}

    The renderer owns the view's selection. Each node {e reads} what the
    selection takes of it ({!Renderable.selectable}): nothing ([`Off]), its text
    ([`Text]), or every cell it paints, as a terminal selects ([`Cells]). Each
    of the selection's two ends, the anchor and the focus, is a position in the
    text of a node with text rows ({!Renderable.val-text_rows}), which
    scrolling, streaming and rewrapping leave in place, or a cell that shows no
    grapheme, such as a border or a blank past a line's end, of the node that
    paints it, which moves with that node, not with its children, and is held
    within its box.

    The selection lies in the {e scope} its press found: the nearest node at or
    above the pressed node that is the root of its layer, the modal node for the
    top layer, a scroll box ({!Renderable.Private.set_region}) that does not
    read [`Cells], or a node that reads [`Cells] whose parent does not. A press
    that finds no node has the modal node for its scope, or the root without
    one. A scope inside the selection's, where no node from its parent up to the
    selection's scope reads [`Cells], is taken whole or not at all: once the
    band covers each cell of its view, all of its text, rows out of view
    included, or, when it reads [`Cells], every cell its view shows; else
    nothing, and a pointer over it is as over a blank. Any other scroll box in
    the scope is a {e window} on the rows its view shows.

    A left press of one click on a cell that shows no grapheme, of a node that
    reads [`Cells] or of no node in a scope that does, anchors there. Any other
    press resolves to a text of the scope: the one under the pointer, else the
    first at or after the pressed cell in row-major order, else the last before
    it. Texts that read [`Off], editors, hidden nodes and the texts of a scope
    taken whole are passed by. The selection starts there with its anchor at the
    pressed cell, in the {e unit} the press's [clicks] select
    ({!Event.Mouse.kind}): for one click, the graphemes the cell shows, or the
    boundary nearest it; for two, the word around them; for three or more, their
    logical line, without its terminator. A word is a run of whitespace, a
    single boundary character, or a run of anything else; the boundary
    characters are Ghostty's: the apostrophe, the quotation mark, the grave
    accent, [│], [|], [:], [;], [,], [$], and the parentheses, square brackets,
    braces and angle brackets. A press that finds no text clears the selection.
    While the press is held, the pointer's cell, held in the scope's view, is
    the focus, with the unit around it in the text it resolves to as a press
    does, or alone over a scope taken whole. The scroll box the press found
    scrolls while the pointer holds near or past its edge, and the scope too
    when that box is a window in a scope that is a scroll box: up or down while
    the pointer, held in the scope's view, lies within the box's columns, and
    sideways while it lies within its rows, so that past a corner of the box it
    scrolls on neither axis. A single click the pointer does not leave selects
    nothing. When the press ends, a focus of one click on a cell that shows no
    text, or no grapheme of a text that reads [`Cells], stays that cell: of the
    nearest node at or above the cell's in the scope that reads [`Text] or
    [`Cells] and is no editor when that node reads [`Cells], or of the scope
    when there is no such node. Otherwise it becomes the far end of what the
    selection holds: the end of its last or first text, or the far corner of a
    scope taken whole that reads [`Cells] and shows no text. A selection that
    then holds nothing clears.

    A left press with Shift, on a node that does not read [`Off] and while a
    selection that is not behind the modal node exists, moves the end of the
    selection nearer the press, held in the scope's view, in row-major distance,
    the focus on a tie, and makes the other end the anchor. The press then drags
    the focus as above, in the selection's unit, or in that of its [clicks] if
    they are two or more. Without a selection, it is an ordinary press, as a
    press with Ctrl is.

    Between the ends lies the terminal's row-major {e band} in the scope's view:
    in each text of the scope, the graphemes from the first cell of the earlier
    end to the last cell of the later one, on the rows its windows show, and the
    units of the ends whole; a scope taken whole; and each cell that a node
    reading [`Cells] paints without text, as the frame on the screen shows it.
    An end that a window has scrolled out of view stands, for the band, at the
    edge of the window's view, sideways too for an end on a cell, and the text
    of that window between the end and the edge is selected too. Selected text
    paints with the [Highlight] role of its theme
    ({!Text_surface.section-paint}), and every cell of the band that a node
    reading [`Cells] paints, blanks included, is drawn again with the role of
    its node's theme, as a terminal shows its selection: each row to the view's
    edge but the last, which stops at the later end. A cell no node paints reads
    as the scope. Editors take no part in the selection: their cells are neither
    highlighted nor copied, and no end lies in them. Inert cells keep their
    style.

    The ends name text, so scrolling, appending, streaming and rewrapping leave
    them on the text they hold. An edit of an end's text
    ({!Renderable.Private.text_changed}) moves the end through the one span it
    changed: the clusters between the longest prefix and the longest suffix that
    the text before the edit and the text after it share. An end before the span
    or at its start stays, so text inserted at an end is left out of the
    selection at its end and taken in at its start; one at its old end or after
    it moves by the change in length; one strictly inside it moves to its start.
    A selection at rest that an edit leaves holding no text, as when its ends
    meet, clears. An end whose node is hidden, detached or destroyed, or below a
    node that is, clears the selection and ends the gesture that drags it, as
    does a change of the declaration ({!Renderable.set_selectable}) of the end's
    node or of one of its ancestors, which decides the end's scope and kind. *)

val selection_text : t -> string option
(** [selection_text t] is the text the view selection holds, as the user saw it
    laid out: the fragments of its texts, each split at the rows another
    fragment shares, in row-major order, without a line terminator at either
    end, joined by as many spaces as cells between them on a row, else by a line
    break and one more for each row between them on which the band highlights a
    cell, as a terminal copies a blank row, or, when it highlights none there,
    by a blank line where a row between them lies in no box that holds either
    below their common ancestor, else by a line break. Rows before the first
    fragment or after the last are not copied, and a band that holds no fragment
    holds nothing. Soft wraps add nothing, an ellipsis gives the text it hides,
    a scope taken whole gives its own copy as one block at its view's first
    cell, and a cell that a node reading [`Cells] paints without text gives its
    grapheme, blank cells at the end of a row left out. The spaces of a text
    stay.

    With no view selection, it is the selection of the focused editor
    ({!Renderable.Private.set_owned_text}), if any, and [None] otherwise. *)

val clear_selection : t -> unit
(** [clear_selection t] clears the view selection, and ends the gesture that
    drags it, if any: its later motion selects nothing, and its release settles
    nothing. *)

val selection_live : t -> bool
(** [selection_live t] is [true] iff the view selection holds text, so that
    {!selection_text} gives it, and is not {e inert}: a selection whose anchor
    is behind the modal node ({!set_modal}) stays as it is, and no key acts on
    it. A host acts on the keys of a live selection. *)

val extend_selection :
  t -> [ `Left | `Right | `Up | `Down | `Row_start | `Row_end ] -> unit
(** [extend_selection t direction] moves the focus of the view selection, when
    no gesture drags it and it is not inert, and keeps its anchor: by a grapheme
    for [`Left] and [`Right], to its column in the row above or below for [`Up]
    and [`Down], and to the start or the end of its row for [`Row_start] and
    [`Row_end], in the text it is in. Past the edge of that text it goes to the
    boundary, in another text, at the cell a press finds before or after it in
    row-major order, or at its column in the row above or below: in the window
    the focus is in first, then anywhere in the scope. A window, or a scope,
    whose view the focus leaves scrolls to show it, by its proposal. A focus in
    a scope taken whole moves out of it, from its view. A focus on a cell moves
    by a cell, a row or to the row's ends over the scope's view, from where it
    stands, at the edge of a window that has scrolled it out of view, and is the
    boundary of the text it lands on; on a cell that shows no text, or no
    grapheme of a text that reads [`Cells], it stays a cell where a release
    would, and goes to the next text otherwise. The selection then extends by
    graphemes ({!section-selection}), and clears if it holds nothing. *)

(** The type for changes of the view selection. *)
type selection_change =
  | Settled of string
      (** A pointer gesture that made or moved the view selection ended, and the
          selection holds this text. *)
  | Cleared  (** The view selection held text, and holds none. *)

val take_selection_changes : t -> selection_change list
(** [take_selection_changes t] is the changes of the view selection since the
    last take, oldest first, and forgets them. A gesture's selection is
    [Settled] when its press ends, at the release or a lost release
    ({!pointer_lost}), and the selection holds text; a double or triple click
    settles at its release. A selection is [Cleared] when it held text and holds
    none, whatever the cause: a press, {!clear_selection}, a gesture that ends
    with no text, {!extend_selection}, which settles nothing, an edit of its
    text, an end's node hidden or unmounted, or a declaration changed above an
    end ({!section-selection}). A gesture's motion queues nothing. *)

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
