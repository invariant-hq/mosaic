open Matrix
module Charts = Matrix_charts

(* Audio Waveform Synthesizer - Interactive terminal synthesizer with
   visualization *)

(* --- Audio Types --- *)

type waveform = Sine | Square | Sawtooth | Triangle

let waveform_name = function
  | Sine -> "Sine"
  | Square -> "Square"
  | Sawtooth -> "Saw"
  | Triangle -> "Triangle"

let all_waveforms = [| Sine; Square; Sawtooth; Triangle |]

(* --- Musical Note Conversion --- *)

let note_names =
  [| "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "A#"; "B" |]

let frequency_to_note freq =
  (* A4 = 440 Hz = MIDI note 69 *)
  let midi = 69.0 +. (12.0 *. Float.log2 (freq /. 440.0)) in
  let midi_rounded = int_of_float (Float.round midi) in
  let note_idx = midi_rounded mod 12 in
  let note_idx = if note_idx < 0 then note_idx + 12 else note_idx in
  let octave = (midi_rounded / 12) - 1 in
  Printf.sprintf "%s%d" note_names.(note_idx) octave

(* --- Harmonic Analysis --- *)

let harmonic_amplitudes waveform ~num_harmonics =
  (* Returns relative amplitudes of harmonics for each waveform type *)
  Array.init num_harmonics (fun i ->
      let n = i + 1 in
      match waveform with
      | Sine -> if n = 1 then 1.0 else 0.0
      | Square ->
          (* Only odd harmonics with 1/n amplitude *)
          if n mod 2 = 1 then 1.0 /. float_of_int n else 0.0
      | Sawtooth ->
          (* All harmonics with 1/n amplitude *)
          1.0 /. float_of_int n
      | Triangle ->
          (* Only odd harmonics with 1/n² amplitude, alternating sign *)
          if n mod 2 = 1 then 1.0 /. (float_of_int n *. float_of_int n) else 0.0)

(* --- WAV File Generation --- *)

let sample_rate = 44100
let bits_per_sample = 16

(* WAV files require little-endian format. OCaml's output_binary_int is
   big-endian, so we need custom little-endian writers. *)
let output_le16 oc n =
  output_byte oc (n land 0xFF);
  output_byte oc ((n lsr 8) land 0xFF)

let output_le32 oc n =
  output_byte oc (n land 0xFF);
  output_byte oc ((n lsr 8) land 0xFF);
  output_byte oc ((n lsr 16) land 0xFF);
  output_byte oc ((n lsr 24) land 0xFF)

let write_wav filename ~samples =
  let n = Array.length samples in
  let data_size = n * 2 in
  let oc = open_out_bin filename in
  (* RIFF header *)
  output_string oc "RIFF";
  output_le32 oc (36 + data_size);
  output_string oc "WAVE";
  (* fmt subchunk *)
  output_string oc "fmt ";
  output_le32 oc 16;
  output_le16 oc 1;
  (* audio format = PCM *)
  output_le16 oc 1;
  (* num channels = 1 *)
  output_le32 oc sample_rate;
  output_le32 oc (sample_rate * bits_per_sample / 8);
  (* byte rate *)
  output_le16 oc 2;
  (* block align *)
  output_le16 oc bits_per_sample;
  (* data subchunk *)
  output_string oc "data";
  output_le32 oc data_size;
  (* Write samples as 16-bit signed little-endian *)
  Array.iter
    (fun sample ->
      let clamped = max (-1.0) (min 1.0 sample) in
      let int_sample = int_of_float (clamped *. 32767.0) in
      let unsigned =
        if int_sample < 0 then int_sample + 65536 else int_sample
      in
      output_byte oc (unsigned land 0xFF);
      output_byte oc ((unsigned lsr 8) land 0xFF))
    samples;
  close_out oc

(* --- Waveform Generation --- *)

let pi = Float.pi

let generate_sample waveform ~phase =
  match waveform with
  | Sine -> sin (2.0 *. pi *. phase)
  | Square -> if phase < 0.5 then 1.0 else -1.0
  | Sawtooth -> (2.0 *. phase) -. 1.0
  | Triangle ->
      if phase < 0.5 then (4.0 *. phase) -. 1.0 else 3.0 -. (4.0 *. phase)

let generate_waveform waveform ~frequency ~duration ~volume =
  let n_samples = int_of_float (float_of_int sample_rate *. duration) in
  let samples = Array.make n_samples 0.0 in
  for i = 0 to n_samples - 1 do
    let t = float_of_int i /. float_of_int sample_rate in
    let phase = mod_float (t *. frequency) 1.0 in
    (* ADSR envelope: quick attack, sustain, quick release *)
    let attack = 0.01 and release = 0.05 in
    let total_time = duration in
    let envelope =
      if t < attack then t /. attack
      else if t > total_time -. release then (total_time -. t) /. release
      else 1.0
    in
    samples.(i) <- generate_sample waveform ~phase *. volume *. envelope
  done;
  samples

(* --- Audio Playback --- *)

let play_audio samples =
  let tmpfile = Filename.temp_file "synth" ".wav" in
  write_wav tmpfile ~samples;
  (* Platform detection: macOS uses afplay, Linux uses aplay or paplay *)
  let player =
    if Sys.command "which afplay > /dev/null 2>&1" = 0 then "afplay"
    else if Sys.command "which paplay > /dev/null 2>&1" = 0 then "paplay"
    else if Sys.command "which aplay > /dev/null 2>&1" = 0 then "aplay -q"
    else "echo 'No audio player found' #"
  in
  (* Run in subshell with all output suppressed to avoid terminal artifacts. The
     subshell also handles cleanup after playback completes. *)
  let cmd =
    Printf.sprintf "(%s %s >/dev/null 2>&1; rm -f %s) >/dev/null 2>&1 &" player
      (Filename.quote tmpfile) (Filename.quote tmpfile)
  in
  ignore (Sys.command cmd)

(* --- Application State --- *)

type state = {
  frequency : float;
  waveform_idx : int;
  volume : float;
  duration : float;
  samples : float array; (* Current waveform for visualization *)
}

let initial_state () =
  let waveform = Sine in
  let frequency = 440.0 in
  let duration = 0.5 in
  let volume = 0.7 in
  let samples = generate_waveform waveform ~frequency ~duration ~volume in
  { frequency; waveform_idx = 0; volume; duration; samples }

let current_waveform state = all_waveforms.(state.waveform_idx)

let regenerate_samples state =
  let waveform = current_waveform state in
  let samples =
    generate_waveform waveform ~frequency:state.frequency
      ~duration:state.duration ~volume:state.volume
  in
  { state with samples }

(* --- Drawing --- *)

let draw_header grid ~cols =
  let title = "Waveform Synthesizer" in
  let title_style = Ansi.Style.make ~fg:Ansi.Color.bright_cyan ~bold:true () in
  Grid.draw_text ~style:title_style grid
    ~x:((cols - String.length title) / 2)
    ~y:0 ~text:title

let draw_controls grid ~x ~y state =
  let label_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let value_style =
    Ansi.Style.make ~fg:Ansi.Color.bright_yellow ~bold:true ()
  in
  let key_style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
  (* Frequency with note name *)
  Grid.draw_text ~style:label_style grid ~x ~y ~text:"Frequency: ";
  let freq_text =
    Printf.sprintf "%.0f Hz (%s)" state.frequency
      (frequency_to_note state.frequency)
  in
  Grid.draw_text ~style:value_style grid ~x:(x + 11) ~y ~text:freq_text;
  Grid.draw_text ~style:key_style grid ~x:(x + 26) ~y ~text:"[A/Z]";
  (* Waveform *)
  Grid.draw_text ~style:label_style grid ~x ~y:(y + 1) ~text:"Waveform:  ";
  Grid.draw_text ~style:value_style grid ~x:(x + 11) ~y:(y + 1)
    ~text:(waveform_name (current_waveform state));
  Grid.draw_text ~style:key_style grid ~x:(x + 22) ~y:(y + 1) ~text:"[Tab]";
  (* Volume *)
  Grid.draw_text ~style:label_style grid ~x ~y:(y + 2) ~text:"Volume:    ";
  Grid.draw_text ~style:value_style grid ~x:(x + 11) ~y:(y + 2)
    ~text:(Printf.sprintf "%.0f%%" (state.volume *. 100.0));
  Grid.draw_text ~style:key_style grid ~x:(x + 22) ~y:(y + 2) ~text:"[S/X]";
  (* Duration *)
  Grid.draw_text ~style:label_style grid ~x ~y:(y + 3) ~text:"Duration:  ";
  Grid.draw_text ~style:value_style grid ~x:(x + 11) ~y:(y + 3)
    ~text:(Printf.sprintf "%.2fs" state.duration);
  Grid.draw_text ~style:key_style grid ~x:(x + 22) ~y:(y + 3) ~text:"[D/C]";
  (* Play instruction *)
  let play_style = Ansi.Style.make ~fg:Ansi.Color.bright_green ~bold:true () in
  Grid.draw_text ~style:play_style grid ~x ~y:(y + 5)
    ~text:"[Space] Play   [Q] Quit";
  (* ADSR envelope visualization - scales with duration *)
  let env_y = y + 7 in
  let env_width = 28 in
  let env_style = Ansi.Style.make ~fg:Ansi.Color.bright_green () in
  let dim_style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
  (* Attack = 0.01s, Release = 0.05s, Sustain = duration - 0.06s *)
  let attack_time = 0.01 in
  let release_time = 0.05 in
  let total_time = state.duration in
  let sustain_time = max 0.0 (total_time -. attack_time -. release_time) in
  (* Scale to env_width based on actual timings *)
  let attack_w =
    max 1 (int_of_float (attack_time /. total_time *. float_of_int env_width))
  in
  let release_w =
    max 1 (int_of_float (release_time /. total_time *. float_of_int env_width))
  in
  let sustain_w =
    max 0 (int_of_float (sustain_time /. total_time *. float_of_int env_width))
  in
  (* Top line (attack peak to sustain) *)
  for i = 0 to attack_w - 1 do
    Grid.draw_text ~style:env_style grid ~x:(x + i) ~y:env_y ~text:"╱"
  done;
  for i = 0 to sustain_w - 1 do
    Grid.draw_text ~style:env_style grid
      ~x:(x + attack_w + i)
      ~y:env_y ~text:"─"
  done;
  for i = 0 to release_w - 1 do
    Grid.draw_text ~style:env_style grid
      ~x:(x + attack_w + sustain_w + i)
      ~y:env_y ~text:"╲"
  done;
  (* Labels *)
  Grid.draw_text ~style:dim_style grid ~x ~y:(env_y + 1) ~text:"A";
  Grid.draw_text ~style:dim_style grid
    ~x:(x + attack_w + (sustain_w / 2))
    ~y:(env_y + 1) ~text:"S";
  Grid.draw_text ~style:dim_style grid
    ~x:(x + env_width - 1)
    ~y:(env_y + 1) ~text:"R";
  Grid.draw_text ~style:dim_style grid
    ~x:(x + (env_width / 2) - 4)
    ~y:(env_y + 2) ~text:"Envelope"

let draw_waveform_chart grid ~x ~y ~width ~height state =
  (* Subsample for display - show ~2 cycles *)
  let cycles = 2.0 in
  let samples_per_cycle =
    int_of_float (float_of_int sample_rate /. state.frequency)
  in
  let display_samples =
    int_of_float (float_of_int samples_per_cycle *. cycles)
  in
  let display_samples = min display_samples (Array.length state.samples) in
  let step = max 1 (display_samples / (width * 2)) in
  let data =
    Array.init (display_samples / step) (fun i ->
        let idx = i * step in
        let t = float_of_int idx /. float_of_int sample_rate *. 1000.0 in
        (t, state.samples.(idx)))
  in
  let chart =
    Charts.empty ()
    |> Charts.with_axes ~x:Charts.Axis.hidden ~y:Charts.Axis.hidden
    |> Charts.line ~resolution:`Braille2x4
         ~style:(Ansi.Style.make ~fg:Ansi.Color.bright_cyan ())
         ~x:fst ~y:snd data
  in
  ignore (Charts.draw chart grid ~x ~y ~width ~height)

let draw_sparkline grid ~x ~y ~width ~height state =
  (* Show amplitude envelope using sparkline bars *)
  let chunk_size = Array.length state.samples / width in
  let chunk_size = max 1 chunk_size in
  let amplitudes =
    Array.init
      (min width (Array.length state.samples / chunk_size))
      (fun i ->
        let start_idx = i * chunk_size in
        let end_idx =
          min (start_idx + chunk_size) (Array.length state.samples)
        in
        let max_amp = ref 0.0 in
        for j = start_idx to end_idx - 1 do
          max_amp := max !max_amp (Float.abs state.samples.(j))
        done;
        !max_amp)
  in
  let sp = Charts.Sparkline.create ~capacity:width () in
  Array.iter (Charts.Sparkline.push sp) amplitudes;
  Charts.Sparkline.draw sp ~kind:`Bars ~x ~y grid ~width ~height

let draw_spectrum grid ~x ~y ~width ~height state =
  (* Draw harmonic spectrum as vertical bars *)
  let num_harmonics = 8 in
  let amplitudes =
    harmonic_amplitudes (current_waveform state) ~num_harmonics
  in
  (* Find max for normalization *)
  let max_amp = Array.fold_left max 0.001 amplitudes in
  (* Bar layout *)
  let bar_width = max 1 ((width - num_harmonics - 1) / num_harmonics) in
  let gap = 1 in
  (* Block characters for fractional heights *)
  let blocks = [| " "; "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |] in
  let colors =
    [|
      Ansi.Color.bright_magenta;
      Ansi.Color.bright_cyan;
      Ansi.Color.bright_green;
      Ansi.Color.bright_yellow;
      Ansi.Color.bright_red;
      Ansi.Color.bright_blue;
      Ansi.Color.bright_magenta;
      Ansi.Color.bright_cyan;
    |]
  in
  for i = 0 to num_harmonics - 1 do
    let amp = amplitudes.(i) /. max_amp in
    let bar_x = x + (i * (bar_width + gap)) in
    let full_height = Float.round (amp *. float_of_int height *. 8.0) in
    let full_cells = int_of_float full_height / 8 in
    let frac = int_of_float full_height mod 8 in
    let style = Ansi.Style.make ~fg:colors.(i mod Array.length colors) () in
    (* Draw full cells from bottom up *)
    for row = 0 to full_cells - 1 do
      let cy = y + height - 1 - row in
      for col = 0 to bar_width - 1 do
        Grid.draw_text ~style grid ~x:(bar_x + col) ~y:cy ~text:"█"
      done
    done;
    (* Draw fractional top cell *)
    if frac > 0 && full_cells < height then
      let cy = y + height - 1 - full_cells in
      for col = 0 to bar_width - 1 do
        Grid.draw_text ~style grid ~x:(bar_x + col) ~y:cy ~text:blocks.(frac)
      done
  done;
  (* Draw harmonic labels *)
  let label_style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
  for i = 0 to min (num_harmonics - 1) ((width / (bar_width + gap)) - 1) do
    let bar_x = x + (i * (bar_width + gap)) in
    Grid.draw_text ~style:label_style grid ~x:bar_x ~y:(y + height)
      ~text:(Printf.sprintf "%d" (i + 1))
  done

let draw_box grid ~x ~y ~width ~height ~title =
  let border_style = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:10) () in
  let title_style =
    Ansi.Style.make ~fg:Ansi.Color.bright_yellow ~bold:true ()
  in
  Grid.draw_text ~style:border_style grid ~x ~y ~text:"┌";
  for i = 1 to width - 2 do
    Grid.draw_text ~style:border_style grid ~x:(x + i) ~y ~text:"─"
  done;
  Grid.draw_text ~style:border_style grid ~x:(x + width - 1) ~y ~text:"┐";
  if String.length title > 0 then (
    let title_x = x + 2 in
    Grid.draw_text ~style:border_style grid ~x:title_x ~y ~text:" ";
    Grid.draw_text ~style:title_style grid ~x:(title_x + 1) ~y ~text:title;
    Grid.draw_text ~style:border_style grid
      ~x:(title_x + 1 + String.length title)
      ~y ~text:" ");
  for row = 1 to height - 2 do
    Grid.draw_text ~style:border_style grid ~x ~y:(y + row) ~text:"│";
    Grid.draw_text ~style:border_style grid
      ~x:(x + width - 1)
      ~y:(y + row) ~text:"│"
  done;
  Grid.draw_text ~style:border_style grid ~x ~y:(y + height - 1) ~text:"└";
  for i = 1 to width - 2 do
    Grid.draw_text ~style:border_style grid ~x:(x + i)
      ~y:(y + height - 1)
      ~text:"─"
  done;
  Grid.draw_text ~style:border_style grid
    ~x:(x + width - 1)
    ~y:(y + height - 1)
    ~text:"┘"

let () =
  let app =
    Matrix.create ~target_fps:(Some 30.) ~mouse_enabled:false
      ~debug_overlay:false ()
  in
  let state = ref (initial_state ()) in
  Matrix.run app
    ~on_frame:(fun _ ~dt:_ -> ())
    ~on_input:(fun app event ->
      match event with
      | Input.Key { key = Input.Key.Escape; _ } -> Matrix.stop app
      | Input.Key { key = Input.Key.Char u; _ } -> (
          let c = Uchar.to_int u in
          match Char.chr c with
          | 'q' | 'Q' -> Matrix.stop app
          | ' ' ->
              let samples =
                generate_waveform (current_waveform !state)
                  ~frequency:!state.frequency ~duration:!state.duration
                  ~volume:!state.volume
              in
              play_audio samples
          | 'a' | 'A' ->
              let freq = min 2000.0 (!state.frequency *. 1.1) in
              state := regenerate_samples { !state with frequency = freq }
          | 'z' | 'Z' ->
              let freq = max 55.0 (!state.frequency /. 1.1) in
              state := regenerate_samples { !state with frequency = freq }
          | 's' | 'S' ->
              let vol = min 1.0 (!state.volume +. 0.1) in
              state := regenerate_samples { !state with volume = vol }
          | 'x' | 'X' ->
              let vol = max 0.1 (!state.volume -. 0.1) in
              state := regenerate_samples { !state with volume = vol }
          | 'd' | 'D' ->
              let dur = min 2.0 (!state.duration +. 0.1) in
              state := regenerate_samples { !state with duration = dur }
          | 'c' | 'C' ->
              let dur = max 0.1 (!state.duration -. 0.1) in
              state := regenerate_samples { !state with duration = dur }
          | _ -> ())
      | Input.Key { key = Input.Key.Tab; _ } ->
          let idx = (!state.waveform_idx + 1) mod Array.length all_waveforms in
          state := regenerate_samples { !state with waveform_idx = idx }
      | _ -> ())
    ~on_render:(fun app ->
      let grid = Matrix.grid app in
      let cols, rows = Matrix.size app in
      Grid.clear ~color:Ansi.Color.black grid;
      draw_header grid ~cols;
      (* Layout *)
      let control_width = 35 in
      let chart_x = control_width + 2 in
      let chart_width = cols - chart_x - 1 in
      let harmonics_height = 10 in
      let controls_height = rows - 2 - harmonics_height in
      let waveform_height = (rows - 4) * 2 / 3 in
      let sparkline_height = rows - 4 - waveform_height - 1 in
      (* Controls panel - top left *)
      draw_box grid ~x:0 ~y:1 ~width:control_width ~height:controls_height
        ~title:"Controls";
      draw_controls grid ~x:2 ~y:3 !state;
      (* Harmonic spectrum - bottom left *)
      let harmonics_y = 1 + controls_height in
      draw_box grid ~x:0 ~y:harmonics_y ~width:control_width
        ~height:harmonics_height ~title:"Harmonics";
      draw_spectrum grid ~x:2 ~y:(harmonics_y + 1) ~width:(control_width - 4)
        ~height:(harmonics_height - 3) !state;
      (* Waveform chart - top right *)
      draw_box grid ~x:chart_x ~y:1 ~width:chart_width
        ~height:(waveform_height + 2) ~title:"Waveform";
      draw_waveform_chart grid ~x:(chart_x + 1) ~y:2 ~width:(chart_width - 2)
        ~height:waveform_height !state;
      (* Amplitude envelope - bottom right *)
      let spark_y = waveform_height + 3 in
      draw_box grid ~x:chart_x ~y:spark_y ~width:chart_width
        ~height:(sparkline_height + 2) ~title:"Amplitude";
      draw_sparkline grid ~x:(chart_x + 1) ~y:(spark_y + 1)
        ~width:(chart_width - 2) ~height:sparkline_height !state)
