(** Escape sequence builders.

    Low-level primitives for constructing ANSI escape sequences. Re-exported by
    {!Ansi}. *)

(** {1:writer Writer} *)

type writer = Writer.t

val make : bytes -> writer
val len : writer -> int
val reset_pos : writer -> unit
val slice : writer -> bytes

(** {1:combinators Combinators} *)

type t = writer -> unit

val empty : t
val literal : string -> t
val char : char -> t
val concat : t -> t -> t
val seq : t list -> t
val bytes : bytes -> off:int -> len:int -> t
val utf8 : int -> t
val emit : t -> writer -> unit
val to_string : t -> string
val to_buffer : t -> Buffer.t -> unit

(** {1:csi CSI and SGR} *)

val esc : string -> t
val csi : params:string -> command:char -> t
val sgr : int list -> t
val sgr_direct : ((int -> unit) -> unit) -> writer -> unit
val reset : t

(** {2:sgr_low Low-level SGR building} *)

val sgr_open : writer -> unit
val sgr_code : writer -> int -> unit
val sgr_sep : writer -> unit
val sgr_close : writer -> unit

(** {1:writing Low-level writing} *)

val write_char : writer -> char -> unit
val write_string : writer -> string -> unit
val write_subbytes : writer -> bytes -> int -> int -> unit

(** {1:cursor Cursor} *)

type cursor_shape =
  [ `Default
  | `Blinking_block
  | `Block
  | `Blinking_underline
  | `Underline
  | `Blinking_bar
  | `Bar ]

val cursor_up : n:int -> t
val cursor_down : n:int -> t
val cursor_forward : n:int -> t
val cursor_back : n:int -> t
val cursor_next_line : n:int -> t
val cursor_previous_line : n:int -> t
val cursor_horizontal_absolute : int -> t
val cursor_vertical_absolute : int -> t
val cursor_position : row:int -> col:int -> t
val cursor_save : t
val cursor_restore : t
val cursor_style : shape:cursor_shape -> t
val cursor_color : r:int -> g:int -> b:int -> t
val reset_cursor_color : t
val reset_cursor_color_fallback : t

(** {1:screen Screen} *)

type erase_display_mode = [ `Below | `Above | `All | `Scrollback ]
type erase_line_mode = [ `Right | `Left | `All ]

val clear : t
val home : t
val clear_and_home : t
val erase_display : mode:erase_display_mode -> t
val erase_below_cursor : t
val erase_line : mode:erase_line_mode -> t
val insert_lines : n:int -> t
val delete_lines : n:int -> t
val scroll_up : n:int -> t
val scroll_down : n:int -> t
val set_scrolling_region : top:int -> bottom:int -> t
val reset_scrolling_region : t

(** {1:colors Colors} *)

val set_foreground : r:int -> g:int -> b:int -> t
val set_background : r:int -> g:int -> b:int -> t
val reset_background : t
val reset_foreground : t

(** {1:terminal Terminal} *)

val set_title : title:string -> t
val explicit_width : width:int -> text:string -> t
val explicit_width_bytes : width:int -> bytes:bytes -> off:int -> len:int -> t

(** {1:osc OSC} *)

type terminator = [ `Bel | `St ]

val osc : ?terminator:terminator -> payload:string -> t

(** {2:hyperlinks Hyperlinks} *)

val hyperlink_start : ?params:string -> url:string -> t
val hyperlink_end : t
val hyperlink : ?params:string -> url:string -> text:string -> t
val hyperlink_open : writer -> string -> unit
val hyperlink_close : writer -> unit

(** {1:modes Modes} *)

type mode =
  | Cursor_visible
  | Mouse_tracking
  | Mouse_button_tracking
  | Mouse_motion
  | Mouse_sgr
  | Mouse_sgr_pixel
  | Mouse_x10
  | Urxvt_mouse
  | Alternate_screen
  | Focus_tracking
  | Bracketed_paste
  | Sync_output
  | Unicode
  | Color_scheme

val enable : mode -> t
val disable : mode -> t
val csi_u_on : t
val csi_u_off : t
val csi_u_push : flags:int -> t
val csi_u_pop : t
val modify_other_keys_on : t
val modify_other_keys_off : t

(** {1:queries Queries} *)

type query =
  | Cursor_position
  | Pixel_size
  | Device_attributes
  | Tertiary_attributes
  | Terminal_identity
  | Device_status
  | Csi_u_support
  | Kitty_graphics
  | Sixel_geometry
  | Explicit_width_support
  | Scaled_text_support
  | Color_scheme_query
  | Focus_mode
  | Sgr_pixels_mode
  | Bracketed_paste_mode
  | Sync_mode
  | Unicode_mode
  | Color_scheme_mode

val query : query -> t
