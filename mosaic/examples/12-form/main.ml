(** Multi-component form with focus management. *)

open Mosaic

(* Field identifiers for focus management *)
type field = Name | Email | Role

let all_fields = [| Name; Email; Role |]

let field_id = function
  | Name -> "name-input"
  | Email -> "email-input"
  | Role -> "role-select"

type model = {
  name : string;
  email : string;
  role : int;
  submitted : bool;
  focus : int; (* index into all_fields *)
}

type msg =
  | Submit
  | Reset
  | Quit
  | Set_role of int
  | Set_name of string
  | Set_email of string
  | Focus_next
  | Focus_prev

let roles =
  [
    Select.{ label = "Developer"; description = Some "Write code" };
    { label = "Designer"; description = Some "Create designs" };
    { label = "Manager"; description = Some "Lead teams" };
    { label = "Analyst"; description = Some "Analyze data" };
  ]

let init () =
  ( { name = ""; email = ""; role = 0; submitted = false; focus = 0 },
    Cmd.focus (field_id Name) )

let num_fields = Array.length all_fields

let update msg model =
  match msg with
  | Submit -> ({ model with submitted = true }, Cmd.none)
  | Reset ->
      ( { name = ""; email = ""; role = 0; submitted = false; focus = 0 },
        Cmd.focus (field_id Name) )
  | Set_role role -> ({ model with role }, Cmd.none)
  | Set_name name -> ({ model with name; submitted = false }, Cmd.none)
  | Set_email email -> ({ model with email; submitted = false }, Cmd.none)
  | Quit -> (model, Cmd.quit)
  | Focus_next ->
      let next = (model.focus + 1) mod num_fields in
      ({ model with focus = next }, Cmd.focus (field_id all_fields.(next)))
  | Focus_prev ->
      let prev = (model.focus - 1 + num_fields) mod num_fields in
      ({ model with focus = prev }, Cmd.focus (field_id all_fields.(prev)))

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()
let accent = Ansi.Color.cyan

let view model =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Header *)
      box ~padding:(padding 1) ~background:header_bg
        [
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            ~size:{ width = pct 100; height = auto }
            [
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Form";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [
          box ~flex_direction:Column ~gap:(gap 2) ~border:true ~border_color
            ~padding:(padding 2)
            [
              text ~style:(Ansi.Style.make ~bold:true ()) "Registration";
              (* Name field *)
              box ~flex_direction:Row ~align_items:Center ~gap:(gap 1)
                [
                  box ~size:{ width = px 8; height = px 1 } [ text "Name:" ];
                  input ~id:(field_id Name) ~placeholder:"Enter your name..."
                    ~size:{ width = px 25; height = px 1 }
                    ~value:model.name
                    ~on_input:(fun v -> Some (Set_name v))
                    ();
                ];
              (* Email field *)
              box ~flex_direction:Row ~align_items:Center ~gap:(gap 1)
                [
                  box ~size:{ width = px 8; height = px 1 } [ text "Email:" ];
                  input ~id:(field_id Email) ~placeholder:"Enter your email..."
                    ~size:{ width = px 25; height = px 1 }
                    ~value:model.email
                    ~on_input:(fun v -> Some (Set_email v))
                    ();
                ];
              (* Role selector *)
              box ~flex_direction:Row ~align_items:Flex_start ~gap:(gap 1)
                [
                  box ~size:{ width = px 8; height = px 1 } [ text "Role:" ];
                  select ~id:(field_id Role) ~show_description:true
                    ~selected_background:accent
                    ~selected_text_color:Ansi.Color.black
                    ~selected_index:model.role
                    ~on_change:(fun idx -> Some (Set_role idx))
                    ~size:{ width = px 25; height = px 5 }
                    roles;
                ];
              (* Buttons *)
              box ~flex_direction:Row ~gap:(gap 2)
                [
                  box ~border:true ~border_color ~padding:(padding 1)
                    ~on_mouse:(fun ev ->
                      match Event.Mouse.kind ev with
                      | Down _ -> Some Submit
                      | _ -> None)
                    [ text "Submit" ];
                  box ~border:true ~border_color ~padding:(padding 1)
                    ~on_mouse:(fun ev ->
                      match Event.Mouse.kind ev with
                      | Down _ -> Some Reset
                      | _ -> None)
                    [ text "Reset" ];
                ];
              (* Submission feedback *)
              (if model.submitted then
                 box ~padding:(padding 1)
                   ~background:(Ansi.Color.grayscale ~level:3)
                   [
                     text
                       ~style:(Ansi.Style.make ~fg:Ansi.Color.green ())
                       (Printf.sprintf "Submitted: %s <%s> as %s" model.name
                          model.email (List.nth roles model.role).label);
                   ]
               else text ~style:hint "Fill in the form and click Submit");
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [
          text ~style:hint "Tab/Shift+Tab navigate  •  Enter submit  •  q quit";
        ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      let data = Event.Key.data ev in
      match data.key with
      | Tab -> if data.modifier.shift then Some Focus_prev else Some Focus_next
      | Enter -> Some Submit
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
