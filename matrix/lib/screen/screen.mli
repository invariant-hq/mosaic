(** Double-buffered terminal screen.

    [Screen] builds terminal frames by mutating a {e next} buffer, diffs it
    against the {e current} buffer to generate minimal ANSI escape sequences,
    then swaps the two. Post-processing transforms can run between building and
    diffing to apply animations or visual filters.

    {2:double_buf Double buffering}

    The screen maintains two grid buffers. During {!build} the caller populates
    the next buffer. During {!render} the next buffer is diffed against current
    to produce ANSI output, then the buffers swap and the (now-next) buffer is
    cleared. Content must be redrawn every frame.

    {2:hit_test Hit testing}

    A {!Hit_grid.t} is maintained alongside each visual grid, mapping screen
    coordinates to integer element IDs. Call {!Hit_grid.add} during frame
    building to register clickable regions. Query coordinates with {!query_hit}
    after rendering.

    {2:post_proc Post-processing}

    Post-processors are persistent functions registered via {!post_process}.
    They run after building and before diffing, receiving the grid and the delta
    time since the last frame. Processors execute in insertion order and persist
    across frames until removed.

    {2:invariants Invariants}
    - The {!Grid.t} passed to {!build} is the next buffer. After {!render} the
      buffers swap and the now-next buffer is cleared.
    - Visual and hit buffers swap on {!render}: regions registered via
      {!Hit_grid.add} become queryable only after the following render.
    - Post-processors always run, even when no cells changed. Their [~delta]
      argument is milliseconds since the last {!render} ([0.] for the first
      frame).
    - {!set_cursor} updates cursor intent only; rendering does not emit terminal
      mode changes. *)

module Hit_grid = Hit_grid

(** {1:types Types} *)

type t
(** The type for screens. Manages double-buffered grids, hit grids,
    post-processing pipeline, statistics, and frame rate control. *)

type stats = {
  frame_count : int;  (** Frames rendered since creation or {!reset}. *)
  total_cells : int;  (** Sum of cells diffed across frames. *)
  total_bytes : int;  (** Sum of ANSI bytes emitted. *)
}
(** The type for cumulative rendering statistics. *)

type frame_metrics = {
  frame_count : int;  (** One-based frame index. *)
  cells : int;  (** Cells diffed. *)
  bytes : int;  (** ANSI bytes written. *)
  frame_time_ms : float;  (** Diff/render duration. *)
  interval_ms : float;  (** Time since previous render. *)
  reset_ms : float;  (** Buffer swap/reset duration. *)
  cursor_visible : bool;  (** Cursor state at render time. *)
  timestamp_s : float;  (** {!Unix.gettimeofday} when rendering finished. *)
}
(** The type for per-frame metrics. All durations are in milliseconds unless
    noted otherwise. *)

(** {1:constructors Constructors} *)

val create :
  ?width_method:Text.width_method ->
  ?respect_alpha:bool ->
  ?cursor_visible:bool ->
  ?explicit_width:bool ->
  unit ->
  t
(** [create ~width_method ~respect_alpha ~cursor_visible ~explicit_width ()] is
    a screen with:
    - [width_method] is the character width computation method. Defaults to
      [`Unicode].
    - [respect_alpha] enables alpha blending when drawing cells. Defaults to
      [false].
    - [cursor_visible] is the initial cursor visibility. Defaults to [true].
    - [explicit_width] enables explicit-width OSC sequences for graphemes.
      Defaults to [false].

    Grids start at 1x1 and are resized automatically on the first {!build}. *)

(** {1:building Frame building} *)

val build :
  t -> width:int -> height:int -> (Grid.t -> Hit_grid.t -> unit) -> unit
(** [build t ~width ~height f] builds a frame.

    Resizes buffers to [width] x [height] when both are positive, clears the hit
    grid, then calls [f] with the next grid and hit grid for in-place mutation.

    When [width <= 0] or [height <= 0], [f] is not called; only the hit grid is
    cleared.

    {b Warning.} The {!Grid.t} and {!Hit_grid.t} passed to [f] must not be
    mutated after the next {!render} -- they become the diff baseline. *)

(** {1:effects Post-processing} *)

type effect_id = int
(** The type for post-processing effect identifiers. *)

val post_process : (Grid.t -> delta:float -> unit) -> t -> effect_id
(** [post_process f t] registers [f] as a persistent post-processing transform
    and is its {!effect_id}.

    [f] is called during each {!render} after frame building but before diffing.
    It receives the next grid and [~delta] (milliseconds since last render, [0.]
    on the first frame). Processors run in insertion order and persist across
    frames until removed.

    See also {!remove_post_process}. *)

val remove_post_process : effect_id -> t -> unit
(** [remove_post_process id t] unregisters the post-processor identified by
    [id]. *)

val clear_post_processes : t -> unit
(** [clear_post_processes t] removes all post-processing functions. *)

val add_hit_region :
  t -> x:int -> y:int -> width:int -> height:int -> id:int -> unit
(** [add_hit_region t ~x ~y ~width ~height ~id] registers a hit region on the
    next hit grid. Convenience wrapper around {!Hit_grid.add} for use outside
    the {!build} callback. Regions outside grid bounds are clipped. Negative
    dimensions are clamped to zero. *)

(** {1:rendering Rendering} *)

type scroll_hint = {
  top : int;  (** First row of the scroll region (0-indexed). *)
  bottom : int;  (** Last row of the scroll region (0-indexed, inclusive). *)
  delta : int;
      (** Rows to shift. Positive shifts content up (user scrolled down). *)
}
(** A scroll optimisation hint from a scrollable container. When provided, the
    renderer shifts the previous buffer and emits DECSTBM + hardware scroll
    sequences so only the newly-revealed edge rows need cell-level diffing.
    Without the hint, scrolling rewrites every row in the viewport. *)

type viewport = {
  y : int;  (** Zero-based terminal row offset of the rendered surface. *)
  height : int;  (** Number of rows in the rendered surface. *)
}
(** A terminal viewport rendered by the screen.

    [None] in {!render} and {!render_to_bytes} means render the full next grid
    at the screen's current {!row_offset}. [Some v] renders the first [v.height]
    grid rows at terminal row offset [v.y]. *)

val render :
  ?full:bool -> ?scroll_hint:scroll_hint -> ?viewport:viewport -> t -> string
(** [render ~full ~scroll_hint ~viewport t] is the ANSI output for the current
    frame.

    Applies post-processors, diffs next against current (or renders all cells
    when [full] is [true]), then swaps buffers. Hit regions registered during
    this frame become queryable via {!query_hit}.
    - [full] renders all cells regardless of changes. Defaults to [false].
    - [scroll_hint] when provided, emits DECSTBM hardware scroll before diffing.
      The hint is a rectangular diff optimization; the caller is responsible for
      using it only when temporary scroll-region changes are safe for the
      current terminal transaction.
    - [viewport] renders into an explicit terminal surface. Rows outside the
      viewport are not presented and do not become active hit regions.

    See also {!render_to_bytes}. *)

val render_to_bytes :
  ?full:bool ->
  ?scroll_hint:scroll_hint ->
  ?viewport:viewport ->
  t ->
  Bytes.t ->
  int
(** [render_to_bytes ~full ~scroll_hint ~viewport t buf] is like {!render} but
    writes into [buf] and is the number of bytes written. [buf] must be large
    enough for the output. *)

(** {1:screen_state Screen state} *)

val set_explicit_width : t -> bool -> unit
(** [set_explicit_width t b] enables or disables explicit-width OSC emission for
    graphemes. When enabled and the terminal supports it, OSC sequences
    specifying the exact width of multi-width characters are emitted to prevent
    terminal-side width mismatch. *)

type cursor_style = [ `Block | `Line | `Underline ]
(** The type for hardware cursor shapes. *)

type cursor = {
  position : (int * int) option;
      (** Zero-based [(x, y)] cell position, or [None] to leave the cursor at
          the renderer/runtime default position. *)
  style : cursor_style;  (** Cursor shape. *)
  blinking : bool;  (** [true] iff the cursor blinks. *)
  color : (int * int * int) option;
      (** [Some (r, g, b)] or [None] for the terminal default. *)
  visible : bool;  (** [true] iff the cursor is logically visible. *)
}
(** The type for cursor state. *)

val set_cursor : t -> cursor -> unit
(** [set_cursor t c] sets the desired cursor state. Colour components outside
    \[[0]; [255]\] are clamped. *)

val cursor : t -> cursor
(** [cursor t] is the current desired cursor state. *)

(** {1:capabilities Capabilities} *)

val apply_capabilities :
  t ->
  explicit_width:bool ->
  explicit_cursor_positioning:bool ->
  hyperlinks:bool ->
  color_depth:Ansi.Sgr_state.color_depth ->
  unit
(** [apply_capabilities t ~explicit_width ~explicit_cursor_positioning
     ~hyperlinks ~color_depth] applies terminal capability flags to [t].
    - [explicit_width]: whether the terminal supports explicit-width OSC
      sequences.
    - [explicit_cursor_positioning]: whether to reposition the cursor after wide
      graphemes as a fallback when [explicit_width] is [false]. Prevents column
      drift in terminals that miscalculate grapheme display widths.
    - [hyperlinks]: whether the terminal supports OSC 8 hyperlinks.
    - [color_depth]: terminal color emission depth. *)

val set_width_method : t -> Text.width_method -> unit
(** [set_width_method t m] sets the grapheme width computation method on both
    the current and next buffers. Use this after capability changes that affect
    width calculation to keep buffers consistent across swaps. *)

(** {1:layout Layout} *)

val resize : t -> width:int -> height:int -> unit
(** [resize t ~width ~height] resizes all internal buffers to [width] x
    [height]. If the dimensions are unchanged, this is a no-op so the presented
    diff baseline remains intact. Otherwise the presented diff baseline is
    cleared so the next render redraws the resized screen; hit grids are
    cleared.

    Raises [Invalid_argument] if [width <= 0] or [height <= 0].

    {b Note.} Normally unnecessary -- {!build} resizes automatically. *)

val reset : t -> unit
(** [reset t] clears the next buffer, empties hit grids, zeros statistics, and
    resets frame timing. The current buffer (diff baseline) is left intact so
    the next render can efficiently clear previously rendered content.
    Post-processors and configuration are preserved. *)

(** {1:statistics Statistics} *)

val stats : t -> stats
(** [stats t] is cumulative rendering statistics since creation or the last
    {!reset}. *)

val last_metrics : t -> frame_metrics
(** [last_metrics t] is the metrics for the most recent frame. *)

(** {1:direct Direct access}

    Direct access to internal buffers for advanced use cases. These functions
    bypass the builder API. *)

val next_grid : t -> Grid.t
(** [next_grid t] is [t]'s next buffer grid.

    {b Warning.} Do not mutate the returned grid after the next {!render} -- it
    becomes the diff baseline. *)

val next_hit_grid : t -> Hit_grid.t
(** [next_hit_grid t] is [t]'s next hit grid.

    {b Warning.} Do not mutate the returned hit grid after the next {!render} --
    it becomes the active hit grid for {!query_hit}. *)

val query_hit : t -> x:int -> y:int -> int
(** [query_hit t ~x ~y] is the element ID at [(x, y)] in the {e current} hit
    grid (i.e. regions from the most recent {!render}). Returns [0] if out of
    bounds or no region is registered. *)

val set_row_offset : t -> int -> unit
(** [set_row_offset t n] sets the vertical origin offset applied to all
    subsequent renders. Negative values are clamped to zero. Useful for inline
    primary-screen rendering. *)

val row_offset : t -> int
(** [row_offset t] is [t]'s current vertical origin offset. *)

val invalidate_presented : t -> unit
(** [invalidate_presented t] clears the current buffer so the diff renderer
    treats all cells as changed on the next render.

    Call this after physically erasing the terminal region to keep the
    renderer's baseline in sync with the actual terminal state.

    {b Note.} Does not emit any escape sequences. *)

val active_height : t -> int
(** [active_height t] is the number of rows containing non-blank content in the
    next buffer. Background-only cells (spaces) are ignored. Useful for sizing
    inline rendering regions. *)
