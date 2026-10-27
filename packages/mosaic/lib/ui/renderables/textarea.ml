type t = Edit_surface.t

type action = Edit_surface.action =
  | Move_left
  | Move_right
  | Move_up
  | Move_down
  | Select_left
  | Select_right
  | Select_up
  | Select_down
  | Line_home
  | Line_end
  | Select_line_home
  | Select_line_end
  | Visual_line_home
  | Visual_line_end
  | Select_visual_line_home
  | Select_visual_line_end
  | Buffer_home
  | Buffer_end
  | Select_buffer_home
  | Select_buffer_end
  | Delete_line
  | Delete_to_line_end
  | Delete_to_line_start
  | Backspace
  | Delete
  | Newline
  | Undo
  | Redo
  | Word_forward
  | Word_backward
  | Select_word_forward
  | Select_word_backward
  | Delete_word_forward
  | Delete_word_backward
  | Select_all
  | Submit

type key_binding = Shortcut.t * action

module Props = Edit_surface.Props

let of_props ~parent ?index ?id ?style ?visible ?z_index ?opacity ~on_change
    ?on_submit (props : Props.t) =
  Edit_surface.create ~parent ?index ?id ?style ?visible ?z_index ?opacity
    ~on_change ?on_submit props

let create ~parent ?index ?id ?style ?visible ?z_index ?opacity ~on_change
    ?on_submit =
  Props.make_k (fun props ->
      of_props ~parent ?index ?id ?style ?visible ?z_index ?opacity ~on_change
        ?on_submit props)

let node = Edit_surface.node
let state = Edit_surface.state
let surface = Edit_surface.surface
let value = Edit_surface.value
let cursor = Edit_surface.cursor
let selection = Edit_surface.selection
let apply_props = Edit_surface.apply_props
let set_on_change = Edit_surface.set_on_change
let set_on_submit = Edit_surface.set_on_submit
let handle_paste = Edit_surface.handle_paste

let pp ppf t =
  Format.fprintf ppf "Textarea(%s" (Renderable.id (node t));
  let v = value t in
  if String.length v > 0 then begin
    let display =
      if String.length v > 20 then String.sub v 0 20 ^ "..." else v
    in
    Format.fprintf ppf ", %S" display
  end;
  Format.pp_print_char ppf ')'

module Private = struct
  let of_props = of_props
end
