open Matrix

let () = Random.self_init ()
let rec ( -- ) a b = if a > b then [] else a :: (succ a -- b)

let utf8_of_code_point cp =
  let buf = Buffer.create 7 in
  Buffer.clear buf;
  Buffer.add_utf_8_uchar buf (Uchar.of_int cp);
  Buffer.contents buf

let nsym = 4096
let glitch = nsym / 20

let symbols =
  Array.concat
    [
      (* Skip dakuten/handakuten, which combine awkwardly in many fonts. *)
      Array.init 56 (fun x -> utf8_of_code_point (0xff66 + x));
      Array.init 10 (fun x -> utf8_of_code_point (0x30 + x));
    ]

let sym () = symbols.(Random.int (Array.length symbols))
let syms = Array.init nsym (fun _ -> sym ())

type column = Wait of int | Line of { i : int; sym : int; win : int; k : int }
type state = (int * int) * column list

let gen_wait h = Wait (Random.int (max 1 (h / 2)))

let gen_line h =
  let sym = Random.int (max 1 (nsym - h)) in
  let win = Random.int (max 1 (h + (h / 2))) + 1 in
  let k = Random.int 2 + 1 in
  Line { i = 0; sym; win; k }

let gen ((w, h) as dim) : state =
  let columns =
    1 -- w
    |> List.map (fun _ ->
        if Random.float 1. < 0.1 then gen_line h else gen_wait h)
  in
  (dim, columns)

let perturb_symbols () =
  let limit = if glitch > 0 then Random.int glitch else 0 in
  for _ = 0 to limit do
    syms.(Random.int nsym) <- sym ()
  done

let step (state : state) =
  let ((_, h) as dim), columns = state in
  let update = function
    | Wait 0 -> gen_line h
    | Wait n -> Wait (n - 1)
    | Line ({ i; win; k; _ } as line) ->
        if i - win + k >= h then gen_wait h else Line { line with i = i + k }
  in
  perturb_symbols ();
  (dim, List.map update columns)

let bg_color = Ansi.Color.of_rgb 0 0 0

let color i n =
  let chan x = int_of_float (x *. 255.) in
  let t = float i /. float n in
  let t1 = chan (Float.exp (-.t /. 0.02)) in
  let t2 = chan (Float.exp (-.t /. 0.45)) in
  Ansi.Color.of_rgb t1 t2 t1

module Style_key = struct
  type t = int * int

  let equal (a1, b1) (a2, b2) = a1 = a2 && b1 = b2
  let hash = Hashtbl.hash
end

module Style_cache = Hashtbl.Make (Style_key)

let style_cache : Ansi.Style.t Style_cache.t = Style_cache.create 256

let style_for idx win =
  let key = (idx, win) in
  match Style_cache.find_opt style_cache key with
  | Some style -> style
  | None ->
      let fg = color idx win in
      let style = Ansi.Style.make ~fg ~bg:bg_color () in
      Style_cache.replace style_cache key style;
      style

let draw_column grid ~height x = function
  | Wait _ -> ()
  | Line { i; sym; win; _ } ->
      let last = i - win in
      let off = max 0 (i - height + 1) in
      let top = max 0 (i - win) in
      let row = ref top in
      let idx = ref off in
      for w = win - off downto 0 do
        let ix = w + last in
        if 0 <= min ix w then (
          let sym_idx = sym + ix in
          (if sym_idx >= 0 && sym_idx < nsym then
             if !row >= 0 && !row < height then
               let style = style_for !idx win in
               let s = syms.(sym_idx) in
               Grid.draw_text ~style grid ~x ~y:!row ~text:s);
          incr idx;
          incr row)
      done

let draw_state grid (state : state) =
  let (w, h), columns = state in
  Grid.clear ~color:bg_color grid;
  let rec loop x cols =
    match cols with
    | [] -> ()
    | col :: rest ->
        if x < w then draw_column grid ~height:h x col;
        loop (x + 1) rest
  in
  loop 0 columns

let frame_interval = 0.075
let target_fps = 1. /. frame_interval

let () =
  let app =
    Matrix.create ~target_fps:(Some target_fps) ~mouse_enabled:false
      ~debug_overlay:true ()
  in
  let state = ref (gen (1, 1)) in
  Matrix.run app
    ~on_frame:(fun _ ~dt:_ -> state := step !state)
    ~on_input:(fun app event ->
      match event with
      | Input.Key { key = Input.Key.Escape; _ } -> Matrix.stop app
      | Input.Key { key = Input.Key.Char uchar; modifier; _ } ->
          let code = Uchar.to_int uchar in
          if
            code = 3
            || (modifier.ctrl && (code = Char.code 'c' || code = Char.code 'C'))
          then Matrix.stop app
          else if code = Char.code ' ' then
            let cols, rows = Matrix.size app in
            state := gen (cols, rows)
      | _ -> ())
    ~on_resize:(fun _ ~cols ~rows -> state := gen (cols, rows))
    ~on_render:(fun app ->
      let g = Matrix.grid app in
      draw_state g !state)
