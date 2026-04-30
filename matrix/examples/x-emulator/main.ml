open Tsdl
open Tsdl_ttf

(* Helper for handling TSDL errors *)
let or_else err_msg r =
  match r with
  | Ok v -> v
  | Error (`Msg e) ->
      Sdl.log "%s: %s" err_msg e;
      Sdl.quit ();
      exit 1

type state = {
  window : Sdl.window;
  renderer : Sdl.renderer;
  mutable font : Ttf.font;
  mutable font_size : int;
  mutable char_width : int;
  mutable char_height : int;
  font_path : string;
  pty : Pty.t;
  vte : Vte.t;
  mutable should_quit : bool;
  (* Selection state *)
  mutable selection_start : (int * int) option;
  mutable selection_end : (int * int) option;
  (* Force render on next frame (for selection changes, etc) *)
  mutable force_render : bool;
  (* Scrollback viewing: 0 = live terminal, >0 = lines scrolled back *)
  mutable display_offset : int;
  mutable last_rendered_offset : int;
      (* Track when we need to rebuild view grid *)
  view_grid : Grid.t; (* Cached grid for scrollback rendering *)
  (* Performance: Cache rendered glyphs to avoid re-rendering on every frame *)
  (* Key: (glyph, fg_color, bg_color, attrs) *)
  texture_cache :
    (string * Ansi.Color.t * Ansi.Color.t * Ansi.Attr.t, Sdl.texture) Hashtbl.t;
}
(** The state of our terminal emulator application. *)

(** Initializes all resources. *)
let init () =
  Sdl.init Sdl.Init.(video + events) |> or_else "SDL Init";
  Ttf.init () |> or_else "TTF Init";

  let font_size = 16 in
  let font_paths =
    [
      (* macOS system fonts *)
      "/System/Library/Fonts/Monaco.ttf";
      "/System/Library/Fonts/Menlo.ttc";
      "/System/Library/Fonts/Courier.ttc";
      "/System/Library/Fonts/Helvetica.ttc";
      "/Library/Fonts/Courier New.ttf";
      (* Common Linux paths *)
      "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf";
      "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf";
      "/usr/share/fonts/truetype/ubuntu/UbuntuMono-R.ttf";
      (* Windows paths *)
      "C:\\Windows\\Fonts\\consola.ttf";
      "C:\\Windows\\Fonts\\cour.ttf";
      (* Fallback to a basic font *)
      "font.ttf";
    ]
  in

  let rec try_fonts = function
    | [] ->
        Sdl.log "Failed to find any suitable font";
        Sdl.quit ();
        exit 1
    | path :: rest -> (
        match Ttf.open_font path font_size with
        | Ok f ->
            Sdl.log "Using font: %s" path;
            (f, path)
        | Error _ -> try_fonts rest)
  in

  let font, font_path = try_fonts font_paths in

  (* Terminal dimensions *)
  let term_cols = 80 in
  let term_rows = 24 in

  (* Get character dimensions *)
  let char_w, char_h =
    Ttf.size_text font "M" |> or_else "Failed to size text"
  in

  (* Create window sized to terminal dimensions *)
  let win_width = (term_cols * char_w) + 20 in
  (* +20 for margins *)
  let win_height = (term_rows * char_h) + 20 in

  let window, renderer =
    Sdl.create_window_and_renderer ~w:win_width ~h:win_height
      Sdl.Window.(shown + resizable)
    |> or_else "Failed to create window and renderer"
  in
  Sdl.set_window_title window "Matrix Terminal";

  (* Create VTE *)
  let vte = Vte.create ~rows:term_rows ~cols:term_cols () in

  (* Start PTY with shell *)
  let shell = try Sys.getenv "SHELL" with Not_found -> "/bin/sh" in
  let pty =
    Pty.spawn ~prog:shell ~args:[]
      ~winsize:
        { Pty.rows = term_rows; cols = term_cols; xpixel = 0; ypixel = 0 }
      ()
  in
  Pty.set_nonblock pty;

  (* Enable text input *)
  Sdl.start_text_input ();

  let view_grid = Grid.create ~width:term_cols ~height:term_rows () in

  {
    window;
    renderer;
    font;
    font_size;
    font_path;
    char_width = char_w;
    char_height = char_h;
    pty;
    vte;
    should_quit = false;
    selection_start = None;
    selection_end = None;
    force_render = true;
    display_offset = 0;
    (* Start at bottom (live terminal) *)
    last_rendered_offset = -1;
    (* Force initial render *)
    view_grid;
    texture_cache = Hashtbl.create 1024;
  }

(** Zoom in/out by changing font size *)
let zoom state delta =
  let new_size = max 8 (min 72 (state.font_size + delta)) in
  if new_size <> state.font_size then (
    (* Close old font *)
    Ttf.close_font state.font;

    (* Open font with new size *)
    match Ttf.open_font state.font_path new_size with
    | Error _ -> (
        (* If failed, reopen at old size *)
        match Ttf.open_font state.font_path state.font_size with
        | Ok f -> state.font <- f
        | Error _ -> ())
    | Ok new_font ->
        state.font <- new_font;
        state.font_size <- new_size;

        (* Get new character dimensions *)
        let char_w, char_h =
          match Ttf.size_text new_font "M" with
          | Ok dims -> dims
          | Error _ -> (state.char_width, state.char_height)
        in
        state.char_width <- char_w;
        state.char_height <- char_h;

        (* Calculate new terminal dimensions based on window size *)
        let w, h = Sdl.get_window_size state.window in
        let new_cols = max 1 ((w - 20) / char_w) in
        let new_rows = max 1 ((h - 20) / char_h) in

        (* Resize PTY and VTE *)
        Pty.resize state.pty ~cols:new_cols ~rows:new_rows;
        Vte.resize state.vte ~rows:new_rows ~cols:new_cols;
        Grid.resize state.view_grid ~width:new_cols ~height:new_rows;

        (* Clear texture cache (different font size) *)
        Hashtbl.iter (fun _ tex -> Sdl.destroy_texture tex) state.texture_cache;
        Hashtbl.clear state.texture_cache;

        (* Invalidate view grid cache (width changed) *)
        state.last_rendered_offset <- -1;
        state.force_render <- true;

        Sdl.log "Zoom: font size %d, grid %dx%d" new_size new_cols new_rows)

(** Cleans up all resources. *)
let cleanup state =
  Sdl.log "Cleaning up...";
  (try Pty.close state.pty with _ -> ());
  Hashtbl.iter (fun _ tex -> Sdl.destroy_texture tex) state.texture_cache;
  Ttf.close_font state.font;
  Sdl.destroy_renderer state.renderer;
  Sdl.destroy_window state.window;
  Ttf.quit ();
  Sdl.quit ()

(** Translates TSDL keyboard events into byte sequences for the PTY. *)
let handle_key_down state (event : Sdl.event) =
  let scancode = Sdl.Event.(get event keyboard_scancode) in
  let keymod = Sdl.get_mod_state () in
  let ctrl = keymod land Sdl.Kmod.ctrl <> 0 in
  let cmd = keymod land Sdl.Kmod.gui <> 0 in
  (* Cmd key on macOS *)
  let shift = keymod land Sdl.Kmod.shift <> 0 in
  (* let alt = (keymod land Sdl.Kmod.alt) <> 0 in *)

  (* Use a buffer to build the output sequence *)
  let buf = Buffer.create 8 in
  let app_cursor = Vte.cursor_key_mode state.vte in
  let bracketed_paste = Vte.bracketed_paste_mode state.vte in
  let cursor_seq normal application =
    if app_cursor then application else normal
  in

  (* Handle scrollback navigation with Shift+PageUp/Down/Home/End *)
  let scrollback_size = Vte.scrollback_size state.vte in
  let term_rows = Vte.rows state.vte in

  (match Sdl.Scancode.enum scancode with
  (* Zoom with Cmd+/Cmd- or Cmd+=/Cmd+0 *)
  | `Equals when cmd ->
      (* Cmd+= (same as Cmd+): zoom in *)
      zoom state 2
  | `Minus when cmd ->
      (* Cmd+-: zoom out *)
      zoom state (-2)
  | `K0 when cmd ->
      (* Cmd+0: reset to default size *)
      zoom state (16 - state.font_size)
  (* Scrollback navigation with Shift modifier *)
  | `Pageup when shift ->
      (* Shift+PageUp: scroll up one page *)
      state.display_offset <-
        min (state.display_offset + term_rows) scrollback_size;
      state.force_render <- true
  | `Pagedown when shift ->
      (* Shift+PageDown: scroll down one page *)
      state.display_offset <- max (state.display_offset - term_rows) 0;
      state.force_render <- true
  | `Home when shift ->
      (* Shift+Home: jump to top of scrollback *)
      state.display_offset <- scrollback_size;
      state.force_render <- true
  | `End when shift ->
      (* Shift+End: jump to live terminal (bottom) *)
      state.display_offset <- 0;
      state.force_render <- true
  (* Normal terminal input (no shift modifier) *)
  | `V when cmd || ctrl -> (
      (* Handle paste - Cmd+V on macOS, Ctrl+V elsewhere *)
      match Sdl.get_clipboard_text () with
      | Ok text ->
          if bracketed_paste then Buffer.add_string buf "\x1b[200~";
          Buffer.add_string buf text;
          if bracketed_paste then Buffer.add_string buf "\x1b[201~"
      | Error _ -> ())
  | `Backspace -> Buffer.add_char buf '\x7f' (* DEL *)
  | `Return -> Buffer.add_char buf '\r'
  | `Up -> Buffer.add_string buf (cursor_seq "\x1b[A" "\x1bOA")
  | `Down -> Buffer.add_string buf (cursor_seq "\x1b[B" "\x1bOB")
  | `Left -> Buffer.add_string buf (cursor_seq "\x1b[D" "\x1bOD")
  | `Right -> Buffer.add_string buf (cursor_seq "\x1b[C" "\x1bOC")
  | `Home -> Buffer.add_string buf (cursor_seq "\x1b[H" "\x1bOH")
  | `End -> Buffer.add_string buf (cursor_seq "\x1b[F" "\x1bOF")
  | `Pageup -> Buffer.add_string buf "\x1b[5~"
  | `Pagedown -> Buffer.add_string buf "\x1b[6~"
  | `Delete -> Buffer.add_string buf "\x1b[3~"
  | `Insert -> Buffer.add_string buf "\x1b[2~"
  | `Tab -> Buffer.add_char buf '\t'
  | `Escape -> Buffer.add_char buf '\x1b'
  (* Handle Ctrl+[A-Z] *)
  | c when ctrl && c >= `A && c <= `Z ->
      let code =
        match c with
        | `A -> 1
        | `B -> 2
        | `C -> 3
        | `D -> 4
        | `E -> 5
        | `F -> 6
        | `G -> 7
        | `H -> 8
        | `I -> 9
        | `J -> 10
        | `K -> 11
        | `L -> 12
        | `M -> 13
        | `N -> 14
        | `O -> 15
        | `P -> 16
        | `Q -> 17
        | `R -> 18
        | `S -> 19
        | `T -> 20
        | `U -> 21
        | `V -> 22
        | `W -> 23
        | `X -> 24
        | `Y -> 25
        | `Z -> 26
        | _ -> 0
      in
      if code > 0 then Buffer.add_char buf (Char.chr code)
  | _ -> () (* Other keys are handled by text_input event *));

  let str_to_write = Buffer.contents buf in
  if String.length str_to_write > 0 then (
    ignore
      (Pty.write_string state.pty str_to_write 0 (String.length str_to_write));
    (* Auto-scroll to live terminal on user input (Alacritty behavior) *)
    state.display_offset <- 0;
    state.force_render <- true)

(** Extract text content from a grid cell. *)
let get_cell_text grid x y =
  let idx = (y * Grid.width grid) + x in
  if Grid.is_continuation grid idx then ""
  else
    let s = Grid.get_text grid idx in
    if s = "" then " " else s

(** Handles all TSDL events. *)
let handle_event state event =
  (* Convert mouse coordinates to cell position *)
  let mouse_to_cell x y =
    let col = (x - 10) / state.char_width in
    let row = (y - 10) / state.char_height in
    if
      col >= 0
      && col < Vte.cols state.vte
      && row >= 0
      && row < Vte.rows state.vte
    then Some (row, col)
    else None
  in

  (* Get text from selection *)
  let get_selection_text () =
    match (state.selection_start, state.selection_end) with
    | Some (r1, c1), Some (r2, c2) ->
        let start_row = min r1 r2 in
        let end_row = max r1 r2 in
        let start_col = if r1 < r2 || (r1 = r2 && c1 <= c2) then c1 else c2 in
        let end_col = if r1 < r2 || (r1 = r2 && c1 <= c2) then c2 else c1 in

        let buffer = Buffer.create 256 in
        let grid =
          if state.display_offset = 0 then Vte.grid state.vte
          else (
            if state.last_rendered_offset <> state.display_offset then (
              Vte.render_with_scrollback state.vte ~offset:state.display_offset
                state.view_grid;
              state.last_rendered_offset <- state.display_offset);
            state.view_grid)
        in

        for row = start_row to end_row do
          let col_start = if row = start_row then start_col else 0 in
          let col_end =
            if row = end_row then end_col else Vte.cols state.vte - 1
          in
          for col = col_start to col_end do
            let text = get_cell_text grid col row in
            if text <> "" then Buffer.add_string buffer text
          done;
          if row < end_row then Buffer.add_char buffer '\n'
        done;
        Buffer.contents buffer
    | _ -> ""
  in

  let event_type = Sdl.Event.(get event typ) in
  match Sdl.Event.enum event_type with
  | `Quit -> state.should_quit <- true
  | `Key_down -> handle_key_down state event
  | `Text_input ->
      let text = Sdl.Event.(get event text_input_text) in
      ignore (Pty.write_string state.pty text 0 (String.length text))
  | `Mouse_button_down -> (
      let x = Sdl.Event.(get event mouse_button_x) in
      let y = Sdl.Event.(get event mouse_button_y) in
      match mouse_to_cell x y with
      | Some (row, col) ->
          state.selection_start <- Some (row, col);
          state.selection_end <- Some (row, col);
          state.force_render <- true
      | None -> ())
  | `Mouse_motion -> (
      let buttons = Sdl.Event.(get event mouse_motion_state) in
      if buttons <> 0l then
        let x = Sdl.Event.(get event mouse_motion_x) in
        let y = Sdl.Event.(get event mouse_motion_y) in
        match mouse_to_cell x y with
        | Some (row, col) ->
            state.selection_end <- Some (row, col);
            state.force_render <- true
        | None -> ())
  | `Mouse_button_up ->
      (* Copy selection to clipboard *)
      let text = get_selection_text () in
      if String.length text > 0 then ignore (Sdl.set_clipboard_text text)
  | `Mouse_wheel ->
      (* Scrollback navigation with mouse wheel *)
      let y = Sdl.Event.(get event mouse_wheel_y) in
      let scrollback_size = Vte.scrollback_size state.vte in
      if y > 0 then
        (* Scroll up (into history) - 1 line at a time *)
        state.display_offset <- min (state.display_offset + 1) scrollback_size
      else if y < 0 then
        (* Scroll down (toward live terminal) - 1 line at a time *)
        state.display_offset <- max (state.display_offset - 1) 0;
      state.force_render <- true
  | `Window_event -> (
      match Sdl.Event.(window_event_enum (get event window_event_id)) with
      | `Resized ->
          let w, h =
            Sdl.Event.(get event window_data1, get event window_data2)
          in
          let new_cols = max 1 ((Int32.to_int w - 20) / state.char_width) in
          let new_rows = max 1 ((Int32.to_int h - 20) / state.char_height) in
          (* Resize both PTY and VTE *)
          Pty.resize state.pty ~cols:new_cols ~rows:new_rows;
          Vte.resize state.vte ~rows:new_rows ~cols:new_cols;
          Grid.resize state.view_grid ~width:new_cols ~height:new_rows;
          state.last_rendered_offset <- -1;
          (* Invalidate cache *)
          state.force_render <- true
      | _ -> ())
  | _ -> ()

(** Renders the entire VTE grid to the screen. *)
let render state =
  let { renderer; vte; char_width; char_height; display_offset; view_grid; _ } =
    state
  in

  (* Choose which grid to render based on display_offset *)
  let grid =
    if display_offset = 0 then
      (* Fast path: render live terminal directly *)
      Vte.grid vte
    else (
      (* Scrollback view: use cached view grid, rebuild only if offset
         changed *)
      if state.last_rendered_offset <> display_offset then (
        Vte.render_with_scrollback vte ~offset:display_offset view_grid;
        state.last_rendered_offset <- display_offset);
      view_grid)
  in

  (* Window background – cells with transparent BG let this show through. *)
  Sdl.set_render_draw_color renderer 20 20 20 255 |> or_else "Set draw color";
  Sdl.render_clear renderer |> or_else "Render clear";

  let grid_width = Grid.width grid in
  let grid_height = Grid.height grid in

  (* Theme default foreground (for ANSI "default" color). *)
  let default_fg_r, default_fg_g, default_fg_b, default_fg_a =
    (230, 230, 230, 255)
  in

  (* Cache glyph textures by (text, fg color, attrs); bg is drawn separately. *)
  let get_or_create_texture (text : string) ~(fg_r : int) ~(fg_g : int)
      ~(fg_b : int) ~(fg_a : int) (attr : Ansi.Attr.t) =
    if text = "" then None
    else
      let fg_color_for_key = Ansi.Color.of_rgba fg_r fg_g fg_b fg_a in
      let cache_key =
        ( text,
          fg_color_for_key,
          Ansi.Color.default (* unused for rendering, just part of key *),
          attr )
      in
      match Hashtbl.find_opt state.texture_cache cache_key with
      | Some tex -> Some tex
      | None -> (
          let fg_sdl = Sdl.Color.create ~r:fg_r ~g:fg_g ~b:fg_b ~a:fg_a in
          match Ttf.render_utf8_blended state.font text fg_sdl with
          | Error _ -> None
          | Ok surface -> (
              match Sdl.create_texture_from_surface renderer surface with
              | Error _ ->
                  Sdl.free_surface surface;
                  None
              | Ok texture ->
                  Sdl.free_surface surface;
                  ignore
                    (Sdl.set_texture_blend_mode texture Sdl.Blend.mode_blend);
                  Hashtbl.add state.texture_cache cache_key texture;
                  Some texture))
  in

  (* Render grid cells *)
  for row = 0 to grid_height - 1 do
    for col = 0 to grid_width - 1 do
      let idx = (row * grid_width) + col in

      (* Grapheme text and width in cells *)
      let text = Grid.get_text grid idx in
      let raw_width = Grid.cell_width grid idx in
      let cell_width = if raw_width <= 0 then 1 else raw_width in

      let fg_r_raw, fg_g_raw, fg_b_raw, fg_a_raw =
        Grid.get_fg grid idx |> Ansi.Color.to_rgba
      in
      let bg_r_raw, bg_g_raw, bg_b_raw, bg_a_raw =
        Grid.get_bg grid idx |> Ansi.Color.to_rgba
      in

      (* Foreground: - (0,0,0,0) => default fg - otherwise literal RGBA *)
      let fg_r, fg_g, fg_b, fg_a =
        if fg_r_raw = 0 && fg_g_raw = 0 && fg_b_raw = 0 && fg_a_raw = 0 then
          (default_fg_r, default_fg_g, default_fg_b, default_fg_a)
        else (fg_r_raw, fg_g_raw, fg_b_raw, fg_a_raw)
      in

      (* Background: - bg_a = 0 => no background fill (transparent) - otherwise
         literal RGBA (including 0,0,0,255 for real black) *)
      let bg_r, bg_g, bg_b, bg_a =
        if bg_a_raw = 0 then (0, 0, 0, 0)
        else (bg_r_raw, bg_g_raw, bg_b_raw, bg_a_raw)
      in

      let attr = Grid.get_attrs grid idx |> Ansi.Attr.unpack in

      (* Check if this cell is within the current selection *)
      let is_selected =
        match (state.selection_start, state.selection_end) with
        | Some (r1, c1), Some (r2, c2) ->
            let start_row = min r1 r2 in
            let end_row = max r1 r2 in
            let start_col =
              if r1 < r2 || (r1 = r2 && c1 <= c2) then c1 else c2
            in
            let end_col = if r1 < r2 || (r1 = r2 && c1 <= c2) then c2 else c1 in
            if row >= start_row && row <= end_row then
              if row = start_row && row = end_row then
                col >= start_col && col <= end_col
              else if row = start_row then col >= start_col
              else if row = end_row then col <= end_col
              else true
            else false
        | _ -> false
      in

      (* Apply selection colors on top of resolved colors *)
      let fg_r, fg_g, fg_b, fg_a, bg_r, bg_g, bg_b, bg_a =
        if is_selected then (255, 255, 255, 255, 100, 100, 200, 255)
        else (fg_r, fg_g, fg_b, fg_a, bg_r, bg_g, bg_b, bg_a)
      in

      (* Draw background if non-transparent (alpha > 0). *)
      if bg_a > 0 then (
        Sdl.set_render_draw_color renderer bg_r bg_g bg_b bg_a
        |> or_else "Set bg color";
        let rect =
          Sdl.Rect.create
            ~x:(10 + (col * char_width))
            ~y:(10 + (row * char_height))
            ~w:(char_width * cell_width) ~h:char_height
        in
        Sdl.render_fill_rect renderer (Some rect) |> or_else "Fill background");

      (* Skip continuation cells and empty graphemes for glyph rendering. *)
      if (not (Grid.is_continuation grid idx)) && text <> "" then
        match get_or_create_texture text ~fg_r ~fg_g ~fg_b ~fg_a attr with
        | None -> ()
        | Some texture ->
            let dst =
              Sdl.Rect.create
                ~x:(10 + (col * char_width))
                ~y:(10 + (row * char_height))
                ~w:(char_width * cell_width) ~h:char_height
            in
            Sdl.render_copy renderer texture ~dst
            |> or_else "Failed to copy texture"
    done
  done;

  (* Render cursor (always at live position, even in scrollback view) *)
  if Vte.cursor_visible vte then (
    let r, c = Vte.cursor_pos vte in
    Sdl.set_render_draw_color renderer 255 255 0 255
    |> or_else "Set cursor color";
    let cursor_rect =
      Sdl.Rect.create
        ~x:(10 + (c * char_width))
        ~y:(10 + (r * char_height))
        ~w:char_width ~h:char_height
    in
    Sdl.render_fill_rect renderer (Some cursor_rect) |> or_else "Render cursor");

  Sdl.render_present renderer

(** The main loop of the application. *)
let main_loop state =
  let pty_fd = Pty.in_fd state.pty in
  let read_buffer = Bytes.create 4096 in

  let rec loop () =
    (* Read from PTY *)
    let readable, _, _ = Unix.select [ pty_fd ] [] [] 0.01 in
    if readable <> [] then (
      try
        let bytes_read =
          Pty.read state.pty read_buffer 0 (Bytes.length read_buffer)
        in
        if bytes_read > 0 then (
          Vte.feed state.vte read_buffer 0 bytes_read;
          (* Auto-scroll to live terminal on new output (Alacritty behavior) *)
          state.display_offset <- 0)
        else
          (* 0 bytes read means EOF, the child process has exited *)
          state.should_quit <- true
      with
      | Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
      | ex ->
          Sdl.log "Read error: %s" (Printexc.to_string ex);
          state.should_quit <- true);

    (* Only render if VTE state has changed or force_render is set *)
    if Vte.is_dirty state.vte || state.force_render then (
      render state;
      Vte.clear_dirty state.vte;
      state.force_render <- false);

    (* Handle events *)
    let event = Sdl.Event.create () in
    while Sdl.poll_event (Some event) && not state.should_quit do
      handle_event state event
    done;

    if not state.should_quit then (
      Sdl.delay 16l;
      loop ())
  in

  loop ()

(** Main entry point. *)
let run () =
  let state = init () in
  Fun.protect ~finally:(fun () -> cleanup state) (fun () -> main_loop state)

(* Start the emulator *)
let () = run ()
