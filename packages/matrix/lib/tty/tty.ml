(* The device *)

let is_tty fd = try Unix.isatty fd with Unix.Unix_error _ -> false

external is_controlling : Unix.file_descr -> bool = "matrix_tty_is_controlling"
external background : Unix.file_descr -> bool = "matrix_tty_background"
external window : Unix.file_descr -> (int * int) option = "matrix_tty_window"

external window_pixels : Unix.file_descr -> (int * int) option
  = "matrix_tty_window_pixels"

external now : unit -> int = "matrix_tty_now" [@@noalloc]
external readable : Unix.file_descr -> bool = "matrix_tty_readable" [@@noalloc]

(* Settings *)

external get_iexten : Unix.file_descr -> bool = "matrix_tty_get_iexten"
external set_iexten : Unix.file_descr -> bool -> unit = "matrix_tty_set_iexten"

(* A terminal's settings, or a Windows console's input mode. *)
type settings =
  | Termios of { termios : Unix.terminal_io; iexten : bool }
  | Console of int

(* A console's input mode, in the order the stubs number them. *)
type console_mode = Raw | Keys | No_echo

external console_set : Unix.file_descr -> console_mode -> int
  = "matrix_tty_console_set"

external console_restore : Unix.file_descr -> int -> unit
  = "matrix_tty_console_restore"

external console_flush : Unix.file_descr -> unit = "matrix_tty_console_flush"
external block_ttou : unit -> bool = "matrix_tty_block_ttou"
external unblock_ttou : bool -> unit = "matrix_tty_unblock_ttou"

(* A signal can interrupt these calls, [TCSADRAIN]'s wait for the output to
   be sent above all (macOS does), with EINTR; the call is made again. *)
let rec uninterrupted f x =
  try f x with Unix.Unix_error (Unix.EINTR, _, _) -> uninterrupted f x

let tcgetattr fd = uninterrupted Unix.tcgetattr fd
let tcsetattr fd mode t = uninterrupted (Unix.tcsetattr fd mode) t

let raw fd =
  if Sys.win32 then Console (console_set fd Raw)
  else
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
    Termios { termios; iexten }

let keys fd =
  if Sys.win32 then Console (console_set fd Keys)
  else
    let termios = tcgetattr fd in
    let iexten = get_iexten fd in
    tcsetattr fd Unix.TCSANOW
      {
        termios with
        c_echo = false;
        c_icanon = false;
        c_vmin = 1;
        c_vtime = 0;
        c_icrnl = false;
        c_inlcr = false;
        c_igncr = false;
      };
    Termios { termios; iexten }

let no_echo fd =
  if Sys.win32 then Console (console_set fd No_echo)
  else
    let termios = tcgetattr fd in
    let iexten = get_iexten fd in
    tcsetattr fd Unix.TCSAFLUSH
      {
        termios with
        c_echo = false;
        c_echoe = false;
        c_echok = false;
        c_echonl = false;
      };
    Termios { termios; iexten }

let restore fd = function
  | Console mode -> console_restore fd mode
  | Termios { termios; iexten } ->
      let blocked = block_ttou () in
      Fun.protect
        ~finally:(fun () -> unblock_ttou blocked)
        (fun () ->
          tcsetattr fd Unix.TCSADRAIN termios;
          set_iexten fd iexten)

let flush_input fd =
  if Sys.win32 then console_flush fd
  else try Unix.tcflush fd Unix.TCIFLUSH with Unix.Unix_error _ -> ()

(* The index of the record of what [enable_vt] changed, or [None] where it
   changed nothing. *)
type console = int option

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

  external take : int -> bool -> bool = "matrix_tty_take"
  external give_back : int -> unit = "matrix_tty_give_back"
  external ignored : int -> bool = "matrix_tty_ignored"
  external raise_default : int -> unit = "matrix_tty_raise"
  external stop_group : unit -> unit = "matrix_tty_stop_group"

  (* Whether OCaml's action for SIGINT is the default, which C cannot read on
     Windows. It is read by setting it, to ignore the signal meanwhile. *)
  let sigint_default () =
    let action = Sys.signal Sys.sigint Sys.Signal_ignore in
    Sys.set_signal Sys.sigint action;
    match action with Sys.Signal_default -> true | _ -> false

  let take s = take s (Sys.win32 && s = Sys.sigint && sigint_default ())

  (* A holder's wake pipe, which Windows has not: its wake is an event. *)
  type holder = { id : int; wake_r : Unix.file_descr option }

  external hold : bool -> Unix.file_descr option -> int * Unix.file_descr option
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

  external console_wait : int -> Unix.file_descr option -> int option -> bool
    = "matrix_tty_wait"

  external console_read : int -> Unix.file_descr -> bytes -> int -> int -> int
    = "matrix_tty_read"

  let hold ~controlling ~output =
    let id, wake_r = hold controlling output in
    { id; wake_r }

  let release h = unhold h.id

  let wake_fd h =
    check h.id;
    match h.wake_r with
    | Some fd -> fd
    | None ->
        invalid_arg
          "Tty.Signal.wake_fd: a Windows holder's wake is no descriptor"

  let wake h = wake h.id
  let received h s = received h.id s
  let terminating h = terminating h.id
  let keep_settings h ~input = keep_settings h.id input
  let forget_settings h = forget_settings h.id
  let set_reset h bytes = set_reset h.id bytes

  let drain fd =
    let buf = Bytes.create 64 in
    let rec go () =
      match Unix.read fd buf 0 64 with
      | 64 -> go ()
      | _ -> ()
      | exception Unix.Unix_error _ -> ()
    in
    go ()

  (* On POSIX, [select(2)] on the input and the wake pipe, then the pipe
     drained. A wait until a time already past looks with [readable]
     instead, which allocates nothing. *)
  let wait h ~input ~until =
    match h.wake_r with
    | None -> console_wait h.id input until
    | Some wake_r -> (
        check h.id;
        let now = match until with Some _ -> now () | None -> 0 in
        match until with
        | Some until when until <= now -> (
            if readable wake_r then drain wake_r;
            match input with Some fd -> readable fd | None -> false)
        | _ -> (
            let timeout =
              match until with
              | None -> -1.
              | Some until -> Float.of_int (until - now) *. 1e-9
            in
            let fds =
              match input with Some fd -> [ fd; wake_r ] | None -> [ wake_r ]
            in
            let ready, _, _ =
              try Unix.select fds [] [] timeout
              with Unix.Unix_error (Unix.EINTR, _, _) -> ([], [], [])
            in
            if List.mem wake_r ready then drain wake_r;
            match input with Some fd -> List.mem fd ready | None -> false))

  let rec read_fd fd buf off len =
    match Unix.read fd buf off len with
    | n -> n
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> read_fd fd buf off len
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> -1

  let read h fd buf off len =
    match h.wake_r with
    | Some _ ->
        check h.id;
        read_fd fd buf off len
    | None ->
        if off < 0 || len < 0 || off > Bytes.length buf - len then
          invalid_arg "Tty.Signal.read";
        console_read h.id fd buf off len

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
