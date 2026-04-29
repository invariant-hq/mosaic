type unicode_width = [ `Wcwidth | `Unicode ]

type t = {
  term : string;
  rgb : bool;
  kitty_keyboard : bool;
  kitty_graphics : bool;
  bracketed_paste : bool;
  focus_tracking : bool;
  unicode_width : unicode_width;
  sgr_pixels : bool;
  color_scheme_updates : bool;
  explicit_width : bool;
  explicit_cursor_positioning : bool;
  scaled_text : bool;
  sixel : bool;
  sync : bool;
  hyperlinks : bool;
}

type terminal_info = { name : string; version : string; from_xtversion : bool }

(* Non-allocating substring search *)
let contains_substring s sub =
  let len_s = String.length s and len_sub = String.length sub in
  if len_sub = 0 then true
  else if len_sub > len_s then false
  else
    (* Check if sub matches at position idx without allocation *)
    let matches_at idx =
      let rec check i =
        if i >= len_sub then true
        else if String.unsafe_get s (idx + i) <> String.unsafe_get sub i then
          false
        else check (i + 1)
      in
      check 0
    in
    let rec loop idx =
      if idx + len_sub > len_s then false
      else if matches_at idx then true
      else loop (idx + 1)
    in
    loop 0

let detect_rgb () =
  let env = Sys.getenv_opt "COLORTERM" |> Option.map String.lowercase_ascii in
  match env with
  | Some v when contains_substring v "truecolor" -> true
  | Some v when contains_substring v "24bit" -> true
  | _ -> false

let detect_kitty ~term =
  Option.is_some (Sys.getenv_opt "KITTY_WINDOW_ID")
  || contains_substring (String.lowercase_ascii term) "kitty"

(* Terminals known to support OSC 8 hyperlinks. *)
let is_hyperlink_term name =
  let lower = String.lowercase_ascii name in
  contains_substring lower "ghostty"
  || contains_substring lower "kitty"
  || contains_substring lower "wezterm"
  || contains_substring lower "alacritty"
  || contains_substring lower "iterm"

let make_initial_capabilities ~term =
  let rgb = detect_rgb () in
  let kitty = detect_kitty ~term in
  let hyperlinks = is_hyperlink_term term || rgb in
  {
    term;
    rgb;
    kitty_keyboard = kitty;
    kitty_graphics = false;
    bracketed_paste = false;
    focus_tracking = false;
    unicode_width = `Unicode;
    sgr_pixels = false;
    color_scheme_updates = false;
    explicit_width = false;
    explicit_cursor_positioning = false;
    scaled_text = false;
    sixel = false;
    sync = false;
    hyperlinks;
  }

let with_env name f caps =
  Option.fold ~none:caps ~some:(fun value -> f value caps) (Sys.getenv_opt name)

let with_env_bool name f caps =
  if Option.is_some (Sys.getenv_opt name) then f caps else caps

let colorterm_truecolor value =
  let v = String.lowercase_ascii value in
  String.equal v "truecolor" || String.equal v "24bit"

let apply_environment_overrides caps =
  (* On Windows, assume modern terminal capabilities (Windows Terminal, ConPTY,
     etc.) *)
  let caps =
    if Sys.win32 then
      {
        caps with
        rgb = true;
        unicode_width = `Unicode;
        bracketed_paste = true;
        (* ConPTY has limited capabilities compared to Unix terminals *)
        sixel = false;
        kitty_graphics = false;
        kitty_keyboard = false;
      }
    else caps
  in
  let caps =
    if Option.is_some (Sys.getenv_opt "TMUX") then
      { caps with unicode_width = `Wcwidth }
    else
      with_env "TERM"
        (fun term caps ->
          let lower = String.lowercase_ascii term in
          if
            String.starts_with ~prefix:"tmux" lower
            || String.starts_with ~prefix:"screen" lower
          then { caps with unicode_width = `Wcwidth }
          else caps)
        caps
  in
  let caps =
    with_env "TERM_PROGRAM"
      (fun prog caps ->
        let caps =
          if is_hyperlink_term prog then { caps with hyperlinks = true }
          else caps
        in
        match prog with
        | "vscode" ->
            {
              caps with
              kitty_keyboard = false;
              kitty_graphics = false;
              unicode_width = `Unicode;
            }
        | "Apple_Terminal" -> { caps with unicode_width = `Wcwidth }
        | _ -> caps)
      caps
  in
  let caps =
    with_env "COLORTERM"
      (fun value caps ->
        if colorterm_truecolor value then
          { caps with rgb = true; hyperlinks = true }
        else caps)
      caps
  in
  let caps =
    with_env_bool "TERMUX_VERSION"
      (fun caps -> { caps with unicode_width = `Wcwidth })
      caps
  in
  let caps =
    with_env_bool "VHS_RECORD"
      (fun caps ->
        {
          caps with
          unicode_width = `Wcwidth;
          kitty_keyboard = false;
          kitty_graphics = false;
        })
      caps
  in
  let caps =
    with_env_bool "MATRIX_FORCE_WCWIDTH"
      (fun caps -> { caps with unicode_width = `Wcwidth })
      caps
  in
  let caps =
    with_env_bool "MATRIX_FORCE_UNICODE"
      (fun caps -> { caps with unicode_width = `Unicode })
      caps
  in
  let caps =
    with_env_bool "KITTY_WINDOW_ID"
      (fun caps ->
        {
          caps with
          kitty_keyboard = true;
          kitty_graphics = true;
          unicode_width = `Unicode;
          rgb = true;
          sixel = true;
          hyperlinks = true;
        })
      caps
  in
  caps

let make_terminal_info term =
  { name = term; version = ""; from_xtversion = false }

let update_terminal_info_from_env info =
  if info.from_xtversion then info
  else
    match Sys.getenv_opt "TERM_PROGRAM" with
    | Some prog ->
        let version =
          Option.value ~default:"" (Sys.getenv_opt "TERM_PROGRAM_VERSION")
        in
        { name = prog; version; from_xtversion = false }
    | None -> info

let initial ?provided ~term () =
  match provided with
  | Some caps ->
      let info =
        make_terminal_info caps.term |> update_terminal_info_from_env
      in
      (caps, info)
  | None ->
      let caps =
        make_initial_capabilities ~term |> apply_environment_overrides
      in
      let info =
        make_terminal_info caps.term |> update_terminal_info_from_env
      in
      (caps, info)

let parse_xtversion_payload info payload =
  let payload = String.trim payload in
  if payload = "" then info
  else
    let len = String.length payload in
    match String.index_from_opt payload 0 '(' with
    | Some paren ->
        let close =
          Option.value ~default:len
            (String.index_from_opt payload (paren + 1) ')')
        in
        let name = String.sub payload 0 paren |> String.trim in
        let version =
          if close > paren + 1 then
            String.sub payload (paren + 1) (close - paren - 1) |> String.trim
          else ""
        in
        { name; version; from_xtversion = true }
    | None -> (
        match String.index_from_opt payload 0 ' ' with
        | Some space ->
            let name = String.sub payload 0 space |> String.trim in
            let rest_len = len - (space + 1) in
            let version =
              if rest_len > 0 then
                String.sub payload (space + 1) rest_len |> String.trim
              else ""
            in
            { name; version; from_xtversion = true }
        | None -> { name = payload; version = ""; from_xtversion = true })

(* Mark capabilities for a confirmed Kitty terminal (from XTVersion or env).
   This is only called when we're confident the terminal is actually Kitty. *)
let mark_kitty_terminal caps =
  {
    caps with
    kitty_keyboard = true;
    kitty_graphics = true;
    unicode_width = `Unicode;
    rgb = true;
    sixel = true;
    hyperlinks = true;
    bracketed_paste = true;
  }

let apply_mode_report caps report =
  (* DECRQM response values: 0 = not recognized (mode not supported) 1 = set
     (mode is enabled) 2 = reset (mode is disabled but supported) 3 =
     permanently set 4 = permanently reset

     For most modes, a response of 1 or 2 indicates the terminal supports the
     mode. For mode 2027 (Unicode width), the value indicates the current state:
     1 = Unicode mode active, 2 = wcwidth mode active. *)
  let is_supported value = value = 1 || value = 2 in
  let update caps (mode, value) =
    match mode with
    | 2027 ->
        (* Mode 2027 is special: value indicates current width mode state *)
        if value = 1 then { caps with unicode_width = `Unicode }
        else if value = 2 then { caps with unicode_width = `Wcwidth }
        else caps
    | _ -> (
        if
          (* Other modes: any recognized response means supported *)
          not (is_supported value)
        then caps
        else
          match mode with
          | 1004 -> { caps with focus_tracking = true }
          | 1016 -> { caps with sgr_pixels = true }
          | 2004 -> { caps with bracketed_paste = true }
          | 2026 -> { caps with sync = true }
          | 2031 -> { caps with color_scheme_updates = true }
          | _ -> caps)
  in
  if report.Input.Caps.is_private then List.fold_left update caps report.modes
  else caps

let apply_event_internal (caps, info) (event : Input.Caps.event) =
  match event with
  | Input.Caps.Device_attributes attrs ->
      let caps =
        if List.mem 4 attrs then { caps with sixel = true } else caps
      in
      (caps, info)
  | Input.Caps.Mode_report r ->
      let caps = apply_mode_report caps r in
      (caps, info)
  | Input.Caps.Pixel_resolution _ -> (caps, info)
  | Input.Caps.Cursor_position (row, col) ->
      let caps =
        let caps =
          if row = 1 && col >= 2 then { caps with explicit_width = true }
          else caps
        in
        if row = 1 && col >= 3 then { caps with scaled_text = true } else caps
      in
      (caps, info)
  | Input.Caps.Xtversion payload ->
      let info = parse_xtversion_payload info payload in
      let caps =
        (* XTVersion containing "kitty" confirms we're in actual Kitty terminal,
           so we can safely enable all Kitty capabilities *)
        if contains_substring (String.lowercase_ascii payload) "kitty" then
          mark_kitty_terminal caps
        else if is_hyperlink_term payload then { caps with hyperlinks = true }
        else caps
      in
      (caps, info)
  | Input.Caps.Kitty_graphics_reply payload ->
      (* Kitty graphics reply only confirms graphics support, nothing else *)
      let caps =
        if contains_substring payload "i=31337" then
          { caps with kitty_graphics = true }
        else caps
      in
      (caps, info)
  | Input.Caps.Kitty_keyboard { level; _ } ->
      (* Kitty keyboard reply only confirms keyboard protocol support. Other
         terminals (e.g., foot, WezTerm) may support the Kitty keyboard protocol
         without supporting Kitty graphics or other Kitty features. *)
      let caps =
        if level >= 0 then { caps with kitty_keyboard = true } else caps
      in
      (caps, info)
  | Input.Caps.Color_scheme _ ->
      (* Color scheme DSR response. The scheme value (Dark/Light) could be
         stored if needed, but for now we just acknowledge the capability. *)
      let caps = { caps with color_scheme_updates = true } in
      (caps, info)

(* Single-event version for callback-based parsing - no list allocation *)
let apply_event ?(apply_env_overrides = false) ~caps ~info event =
  let caps, info = apply_event_internal (caps, info) event in
  let caps =
    if apply_env_overrides then apply_environment_overrides caps else caps
  in
  (caps, info)

let apply_events ?(apply_env_overrides = false) ~caps ~info events =
  let caps, info = List.fold_left apply_event_internal (caps, info) events in
  let caps =
    if apply_env_overrides then apply_environment_overrides caps else caps
  in
  (caps, info)

let cursor_save = Ansi.(to_string cursor_save)
let cursor_restore = Ansi.(to_string cursor_restore)
let home = Ansi.(to_string home)
let explicit_width_query = Ansi.(to_string (query Explicit_width_support))
let scaled_text_query = Ansi.(to_string (query Scaled_text_support))
let xtversion_query = Ansi.(to_string (query Terminal_identity))
let csi_u_query = Ansi.(to_string (query Csi_u_support))
let decrqm_sgr_pixels = Ansi.(to_string (query Sgr_pixels_mode))
let decrqm_unicode = Ansi.(to_string (query Unicode_mode))
let decrqm_color_scheme = Ansi.(to_string (query Color_scheme_mode))
let decrqm_focus = Ansi.(to_string (query Focus_mode))
let decrqm_bracketed_paste = Ansi.(to_string (query Bracketed_paste_mode))
let decrqm_sync = Ansi.(to_string (query Sync_mode))
let cursor_position_request = Ansi.(to_string (query Cursor_position))
let kitty_graphics_query = Ansi.(to_string (query Kitty_graphics))
let primary_device_attrs = Ansi.(to_string (query Device_attributes))
let iterm2_proprietary_query = "\027]1337;ReportCellSize\007"

let is_tmux ~term =
  Option.is_some (Sys.getenv_opt "TMUX")
  ||
  let lower = String.lowercase_ascii term in
  String.starts_with ~prefix:"tmux" lower
  || String.starts_with ~prefix:"screen" lower

(* Wrap an escape sequence for tmux DCS passthrough. All ESC bytes (0x1b) in the
   payload are doubled so tmux forwards them to the outer terminal. *)
let wrap_for_tmux seq =
  let buf = Buffer.create (String.length seq + 16) in
  Buffer.add_string buf "\027Ptmux;";
  String.iter
    (fun c ->
      if c = '\027' then Buffer.add_string buf "\027\027"
      else Buffer.add_char buf c)
    seq;
  Buffer.add_string buf "\027\\";
  Buffer.contents buf

let build_probe_payload term =
  let tmux = is_tmux ~term in
  (* DECRQM queries probe whether the terminal supports specific private modes.
     Inside tmux, these must be DCS-wrapped to reach the outer terminal, since
     tmux intercepts DECRQM and may not know about newer modes (sync, unicode
     width, etc.). *)
  let decrqm_block =
    let raw =
      String.concat ""
        [
          decrqm_sgr_pixels;
          decrqm_unicode;
          decrqm_color_scheme;
          decrqm_focus;
          decrqm_bracketed_paste;
          decrqm_sync;
        ]
    in
    if tmux then wrap_for_tmux raw else raw
  in
  let graphics =
    if tmux then wrap_for_tmux kitty_graphics_query else kitty_graphics_query
  in
  let base_queries =
    [
      cursor_save;
      decrqm_block;
      primary_device_attrs;
      home;
      explicit_width_query;
      cursor_position_request;
      home;
      scaled_text_query;
      cursor_position_request;
      xtversion_query;
      csi_u_query;
      graphics;
    ]
  in
  let queries =
    let term_lower = String.lowercase_ascii term in
    if contains_substring term_lower "iterm" then
      base_queries @ [ iterm2_proprietary_query ]
    else base_queries
  in
  String.concat "" (queries @ [ cursor_restore ])

let probe ?(timeout = 0.2) ?(apply_env_overrides = false) ~on_event ~read_into
    ~wait_readable ~send ~parser ~caps ~info () =
  let payload = build_probe_payload caps.term in
  send payload;
  let buffer = Bytes.create 4096 in
  let deadline = Unix.gettimeofday () +. max 0. timeout in
  (* Grace period after DA: once we receive Device Attributes, the terminal has
     processed our entire query payload. Any remaining responses (CPR for
     explicit width/scaled text, XTVersion, etc.) should arrive shortly after.
     We use a short grace period instead of waiting the full timeout. *)
  let da_grace = 0.05 in
  let rec loop caps info got_explicit got_scaled got_da da_time =
    let now = Unix.gettimeofday () in
    let complete =
      (got_explicit || caps.explicit_width)
      && (got_scaled || caps.scaled_text)
      && got_da
    in
    let effective_deadline =
      match da_time with
      | Some t -> Float.min deadline (t +. da_grace)
      | None -> deadline
    in
    if now >= effective_deadline || complete then (caps, info)
    else
      let remaining = effective_deadline -. now in
      let select_timeout = Float.min remaining 0.05 in
      let ready = wait_readable ~timeout:select_timeout in
      if not ready then loop caps info got_explicit got_scaled got_da da_time
      else
        let read = read_into buffer 0 (Bytes.length buffer) in
        if read <= 0 then loop caps info got_explicit got_scaled got_da da_time
        else
          let caps_ref = ref caps in
          let info_ref = ref info in
          let found_da = ref false in
          Input.Parser.feed parser buffer 0 read ~now ~on_event
            ~on_caps:(fun c ->
              let caps', info' =
                apply_event ~apply_env_overrides ~caps:!caps_ref ~info:!info_ref
                  c
              in
              caps_ref := caps';
              info_ref := info';
              match c with
              | Input.Caps.Device_attributes _ -> found_da := true
              | _ -> ());
          let caps = !caps_ref in
          let info = !info_ref in
          let has_da = got_da || !found_da in
          let da_time = if has_da && not got_da then Some now else da_time in
          let got_explicit = got_explicit || caps.explicit_width in
          let got_scaled = got_scaled || caps.scaled_text in
          loop caps info got_explicit got_scaled has_da da_time
  in
  let caps, info =
    loop caps info caps.explicit_width caps.scaled_text false None
  in
  let caps_ref = ref caps in
  let info_ref = ref info in
  let now = Unix.gettimeofday () in
  Input.Parser.drain parser ~now ~on_event ~on_caps:(fun c ->
      let caps', info' =
        apply_event ~apply_env_overrides ~caps:!caps_ref ~info:!info_ref c
      in
      caps_ref := caps';
      info_ref := info');
  let caps =
    if apply_env_overrides then apply_environment_overrides !caps_ref
    else !caps_ref
  in
  (caps, !info_ref)
