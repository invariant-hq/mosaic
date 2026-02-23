(** Renderer: layout, drawing, hit testing, and event dispatch.

    The renderer drives a three-pass pipeline over a {!Renderable.t} tree:
    - {e Lifecycle}: runs per-frame and resize hooks on registered nodes.
    - {e Layout and command generation}: computes layout via Toffee, walks the
      tree depth-first to extract absolute positions, and builds a flat render
      command list (opacity, scissors, draw calls).
    - {e Execution}: replays the command list to populate the grid and hit grid.

    After the pipeline, {!render} diffs the grid against the previous frame and
    returns minimal ANSI output.

    The renderer owns the root {!Renderable.t}, the {!Screen.t}, and all
    pipeline state. Widget code builds the tree under {!root}; the event loop
    calls {!render_frame} and {!render}, then dispatches input events. *)

(** {1:types Types} *)

type t
(** The type for renderer state. Owns the root renderable, layout tree, screen,
    and all render pipeline state. *)

(** {1:constructors Constructors} *)

val create :
  ?glyph_pool:Glyph.Pool.t ->
  ?width_method:Glyph.width_method ->
  ?style:Toffee.Style.t ->
  unit ->
  t
(** [create ()] is a renderer with a root renderable and an empty screen. The
    optional parameters are:
    - [glyph_pool]: the shared glyph pool for text rendering. Defaults to a
      fresh {!Glyph.Pool.t}.
    - [width_method]: the glyph width computation method. Defaults to the
      {!Glyph.width_method} default.
    - [style]: the root node's initial style. Defaults to
      {!Toffee.Style.default}. *)

(** {1:accessors Accessors} *)

val root : t -> Renderable.t
(** [root t] is the root renderable. Build the UI tree under this node. *)

val screen : t -> Screen.t
(** [screen t] is the underlying screen. *)

val glyph_pool : t -> Glyph.Pool.t
(** [glyph_pool t] is the shared glyph pool for text rendering. *)

(** {1:rendering Rendering} *)

val render_frame : t -> width:int -> height:int -> delta:float -> unit
(** [render_frame t ~width ~height ~delta] builds the next frame.

    The pipeline runs in order:
    - Runs lifecycle passes ([on_frame] and resize hooks).
    - Runs frame callbacks (see {!add_frame_callback}).
    - Computes layout via Toffee.
    - Walks the tree: extracts layout and builds the render command list.
    - Executes render commands: draws to the grid and populates the hit grid.
    - Rechecks hover state against the updated hit grid.

    [width] and [height] are the frame dimensions in terminal cells. [delta] is
    elapsed milliseconds since the last frame. *)

val render : ?full:bool -> t -> string
(** [render t] diffs the current frame against the previous one and returns the
    minimal ANSI output string. Call after {!render_frame}.

    When [full] is [true], all cells are emitted regardless of changes. [full]
    defaults to [false]. *)

val needs_render : t -> bool
(** [needs_render t] is [true] iff a renderable has requested a re-render or
    live nodes are active. *)

(** {1:events Event dispatch} *)

val dispatch_key : t -> Input.Key.event -> Event.key
(** [dispatch_key t key] sends [key] to the focused renderable and returns the
    resulting event.

    If the focused node does not prevent default, the default key handler runs.
    The returned event carries the [default_prevented] flag set by the focused
    node's handler; callers can inspect it to determine whether the key was
    consumed. *)

val dispatch_mouse : t -> Input.Mouse.event -> unit
(** [dispatch_mouse t mouse] runs the full mouse dispatch pipeline:
    - Updates pointer state.
    - Hit-tests the mouse position.
    - Advances the selection state machine (start, update, or finish).
    - Tracks hover state and fires [Over]/[Out] events on target change.
    - Redirects events to the drag-captured node when active.
    - Dispatches with bubbling to the hit-tested node.
    - Auto-focuses on left click.
    - Clears stale selection if not prevented. *)

val dispatch_paste : t -> string -> unit
(** [dispatch_paste t text] sends [text] as a paste event to the focused
    renderable. *)

val dispatch_scroll :
  t ->
  x:int ->
  y:int ->
  direction:Event.Mouse.scroll_direction ->
  delta:int ->
  modifiers:Input.Key.modifier ->
  unit
(** [dispatch_scroll t ~x ~y ~direction ~delta ~modifiers] dispatches a scroll
    event at terminal cell position [(x, y)]. *)

(** {1:focus Focus} *)

val focused : t -> Renderable.t option
(** [focused t] is the currently focused renderable, if any. *)

val focus : t -> Renderable.t -> bool
(** [focus t node] focuses [node] and is [true] iff [node] is focusable. *)

val blur : t -> unit
(** [blur t] removes focus from the currently focused renderable. *)

(** {1:selection Selection} *)

val selection : t -> Selection.t option
(** [selection t] is the active text selection, if any. *)

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

val add_frame_callback : t -> (float -> unit) -> unit
(** [add_frame_callback t f] registers [f] to run at the start of each frame
    with delta time in milliseconds. Callbacks run after lifecycle passes and
    before layout computation.

    See also {!remove_frame_callback} and {!clear_frame_callbacks}. *)

val remove_frame_callback : t -> (float -> unit) -> unit
(** [remove_frame_callback t f] unregisters [f] using physical equality.

    See also {!add_frame_callback}. *)

val clear_frame_callbacks : t -> unit
(** [clear_frame_callbacks t] removes all registered frame callbacks.

    See also {!add_frame_callback}. *)

(** {1:post_process Post-processing} *)

val add_post_process : t -> (Grid.t -> delta:float -> unit) -> Screen.effect_id
(** [add_post_process t f] registers [f] as a persistent post-processing
    transform on the underlying screen. [f] receives the rendered {!Grid.t} and
    the frame delta in milliseconds; it runs after frame building and before
    diffing. Returns a {!Screen.effect_id} for later removal.

    See also {!remove_post_process} and {!clear_post_processes}. *)

val remove_post_process : t -> Screen.effect_id -> unit
(** [remove_post_process t id] unregisters the post-processor identified by
    [id].

    See also {!add_post_process}. *)

val clear_post_processes : t -> unit
(** [clear_post_processes t] removes all registered post-processing functions.

    See also {!add_post_process}. *)
