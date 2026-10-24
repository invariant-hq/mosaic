(* The device *)

let is_tty fd = try Unix.isatty fd with Unix.Unix_error _ -> false

external is_controlling : Unix.file_descr -> bool = "matrix_tty_is_controlling"
external window : Unix.file_descr -> (int * int) option = "matrix_tty_window"

external window_pixels : Unix.file_descr -> (int * int) option
  = "matrix_tty_window_pixels"

external now : unit -> int = "matrix_tty_now" [@@noalloc]
external readable : Unix.file_descr -> bool = "matrix_tty_readable" [@@noalloc]

(* Settings *)

external get_iexten : Unix.file_descr -> bool = "matrix_tty_get_iexten"
external set_iexten : Unix.file_descr -> bool -> unit = "matrix_tty_set_iexten"

type settings = { termios : Unix.terminal_io; iexten : bool }

(* A signal can interrupt these calls, [TCSADRAIN]'s wait for the output to
   be sent above all (macOS does), with EINTR; the call is made again. *)
let rec uninterrupted f x =
  try f x with Unix.Unix_error (Unix.EINTR, _, _) -> uninterrupted f x

let tcgetattr fd = uninterrupted Unix.tcgetattr fd
let tcsetattr fd mode t = uninterrupted (Unix.tcsetattr fd mode) t

let raw fd =
  let termios = tcgetattr fd in
  let iexten = get_iexten fd in
  tcsetattr fd Unix.TCSANOW
    {
      termios with
      c_echo = false;
      c_icanon = false;
      c_isig = false;
      c_vmin = 1;
      c_vtime = 0;
      c_ixon = false;
      c_icrnl = false;
    };
  (* The record write above leaves IEXTEN as it was. *)
  set_iexten fd false;
  { termios; iexten }

let restore fd s =
  tcsetattr fd Unix.TCSADRAIN s.termios;
  set_iexten fd s.iexten

let flush_input fd =
  try Unix.tcflush fd Unix.TCIFLUSH with Unix.Unix_error _ -> ()

(* The console's mode and output code page before [enable_vt], or [None]
   where it changed nothing. *)
type console = (int * int) option

external enable_vt : Unix.file_descr -> console = "matrix_tty_enable_vt"

external restore_console : Unix.file_descr -> console -> unit
  = "matrix_tty_restore_console"

(* Terminal descriptions *)

(* Where systems keep the database: ncurses' default, then the directories
   distributions compile in (Debian's /etc and /lib, Solaris', the BSDs' and
   Homebrew's local trees). *)
let system_databases =
  [
    "/etc/terminfo";
    "/lib/terminfo";
    "/usr/share/terminfo";
    "/usr/lib/terminfo";
    "/usr/share/lib/terminfo";
    "/usr/local/share/terminfo";
  ]

let decode_hex s =
  let digit c =
    match c with
    | '0' .. '9' -> Some (Char.code c - 48)
    | 'a' .. 'f' -> Some (Char.code c - 87)
    | 'A' .. 'F' -> Some (Char.code c - 55)
    | _ -> None
  in
  let n = String.length s in
  if n mod 2 <> 0 then None
  else
    let b = Bytes.create (n / 2) in
    let rec go i =
      if i >= n then Some (Bytes.to_string b)
      else
        match (digit s.[i], digit s.[i + 1]) with
        | Some h, Some l ->
            Bytes.set b (i / 2) (Char.chr ((h * 16) + l));
            go (i + 2)
        | _ -> None
    in
    go 0

(* Base64, in the alphabet tic -Q writes ([-_]) or the standard one
   ([+/]), padded or not. *)
let decode_b64 s =
  let value c =
    match c with
    | 'A' .. 'Z' -> Some (Char.code c - 65)
    | 'a' .. 'z' -> Some (Char.code c - 71)
    | '0' .. '9' -> Some (Char.code c + 4)
    | '+' | '-' -> Some 62
    | '/' | '_' -> Some 63
    | _ -> None
  in
  let s =
    match String.index_opt s '=' with Some i -> String.sub s 0 i | None -> s
  in
  let out = Buffer.create (String.length s * 3 / 4) in
  let rec go i bits acc =
    if i >= String.length s then Some (Buffer.contents out)
    else
      match value s.[i] with
      | None -> None
      | Some v ->
          let acc = (acc lsl 6) lor v and bits = bits + 6 in
          if bits >= 8 then (
            Buffer.add_char out (Char.chr ((acc lsr (bits - 8)) land 0xFF));
            go (i + 1) (bits - 8) (acc land ((1 lsl (bits - 8)) - 1)))
          else go (i + 1) bits acc
  in
  go 0 0 0

let terminfo name =
  if name = "" || name = "." || name = ".." || String.contains name '/' then
    Error `Not_found
  else
    (* A program that runs with another user's rights does not read the
       caller's descriptions, as ncurses does not. *)
    let trusted =
      Unix.getuid () = Unix.geteuid () && Unix.getgid () = Unix.getegid ()
    in
    let env var =
      match Sys.getenv_opt var with
      | Some v when trusted && v <> "" -> Some v
      | _ -> None
    in
    let invalid = ref None in
    let read data =
      match Matrix_terminfo.of_string data with
      | Ok t -> Some t
      | Error e ->
          if !invalid = None then invalid := Some e;
          None
    in
    let in_database dir =
      List.find_map
        (fun leaf ->
          let path = Filename.concat (Filename.concat dir leaf) name in
          match In_channel.with_open_bin path In_channel.input_all with
          | data -> read data
          | exception Sys_error _ -> None)
        [ String.make 1 name.[0]; Printf.sprintf "%02x" (Char.code name.[0]) ]
    in
    let encoded decode data =
      match decode data with
      | Some bytes -> (
          match read bytes with
          | Some t when List.mem name (Matrix_terminfo.names t) -> Some t
          | _ -> None)
      | None ->
          if !invalid = None then invalid := Some "TERMINFO is not encoded";
          None
    in
    let terminfo =
      match env "TERMINFO" with
      | Some v when String.starts_with ~prefix:"hex:" v ->
          [
            (fun () ->
              encoded decode_hex (String.sub v 4 (String.length v - 4)));
          ]
      | Some v when String.starts_with ~prefix:"b64:" v ->
          [
            (fun () ->
              encoded decode_b64 (String.sub v 4 (String.length v - 4)));
          ]
      | Some dir -> [ (fun () -> in_database dir) ]
      | None -> []
    in
    let home =
      match env "HOME" with
      | Some home -> [ Filename.concat home ".terminfo" ]
      | None -> []
    in
    let dirs =
      match env "TERMINFO_DIRS" with
      | Some v ->
          List.map
            (fun d -> if d = "" then "/usr/share/terminfo" else d)
            (String.split_on_char ':' v)
      | None -> []
    in
    let sources =
      terminfo
      @ List.map (fun dir () -> in_database dir) (home @ dirs @ system_databases)
    in
    match List.find_map (fun source -> source ()) sources with
    | Some t -> Ok t
    | None -> (
        match !invalid with
        | Some e -> Error (`Invalid e)
        | None -> Error `Not_found)

(* Signals *)

module Signal = struct
  let signals ~controlling =
    let ending =
      [ Sys.sigabrt; Sys.sigint; Sys.sigterm; Sys.sighup; Sys.sigquit ]
    in
    if controlling then Sys.sigwinch :: Sys.sigcont :: Sys.sigtstp :: ending
    else ending

  external take : int -> bool = "matrix_tty_take"
  external give_back : int -> unit = "matrix_tty_give_back"
  external ignored : int -> bool = "matrix_tty_ignored"
  external raise_default : int -> unit = "matrix_tty_raise"
  external stop_group : unit -> unit = "matrix_tty_stop_group"

  type holder = { id : int; wake_r : Unix.file_descr }

  external hold : bool -> Unix.file_descr option -> int * Unix.file_descr
    = "matrix_tty_hold"

  external unhold : int -> unit = "matrix_tty_unhold"
  external check : int -> unit = "matrix_tty_check"
  external wake : int -> unit = "matrix_tty_wake"
  external received : int -> int -> bool = "matrix_tty_received"
  external terminating : int -> bool = "matrix_tty_terminating"

  external keep_settings : int -> Unix.file_descr -> unit
    = "matrix_tty_keep_settings"

  external forget_settings : int -> unit = "matrix_tty_forget_settings"
  external set_reset : int -> string -> unit = "matrix_tty_set_reset"
  external restore_all : unit -> unit = "matrix_tty_restore_all"

  let hold ~controlling ~output =
    let id, wake_r = hold controlling output in
    { id; wake_r }

  let release h = unhold h.id

  let wake_fd h =
    check h.id;
    h.wake_r

  let wake h = wake h.id
  let received h s = received h.id s
  let terminating h = terminating h.id
  let keep_settings h ~input = keep_settings h.id input
  let forget_settings h = forget_settings h.id
  let set_reset h bytes = set_reset h.id bytes

  (* [Unix.write] goes on past its first [write(2)] and, interrupted there,
     raises without saying how much went: each call here is one
     [write(2)], and the write goes on from where it stopped. *)
  let rec write_rest h fd buf off len =
    if len > 0 then
      match Unix.single_write fd buf off len with
      | n -> write_rest h fd buf (off + n) (len - n)
      | exception Unix.Unix_error (Unix.EAGAIN, _, _) ->
          (try ignore (Unix.select [] [ fd ] [] (-1.))
           with Unix.Unix_error (Unix.EINTR, _, _) -> ());
          write_rest h fd buf off len
      | exception Unix.Unix_error (Unix.EINTR, _, _) ->
          if not (terminating h) then write_rest h fd buf off len

  let write h fd buf off len =
    check h.id;
    write_rest h fd buf off len
end
