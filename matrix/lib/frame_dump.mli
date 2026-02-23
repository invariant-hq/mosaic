(** Frame dumping to disk.

    Serializes {!Screen.t} grids to ANSI files and optionally dumps the hit
    grid. Used by the Matrix runtime when periodic dumps are configured (see
    {!Matrix.configure_frame_dump}) but callable directly for custom tooling. *)

val on_frame :
  ?dir:string ->
  ?pattern:string ->
  ?hits:bool ->
  every:int ->
  unit ->
  Screen.t ->
  unit
(** [on_frame ~every ()] is a callback that dumps every [every]th frame. The
    callback keeps its own index counter and may be reused across frames. [hits]
    includes the hit grid when [true] (defaults to [false]).

    Raises [Invalid_argument] if [every <= 0]. *)

val snapshot : ?dir:string -> ?pattern:string -> ?hits:bool -> Screen.t -> unit
(** [snapshot screen] writes a single dump immediately. Shares the global frame
    index with {!on_frame} callbacks so filenames remain monotonic. *)
