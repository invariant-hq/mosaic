(** Multi-component form with focus management. *)

open Mosaic

type role = Developer | Designer | Manager | Analyst
type field = Name | Email | Role | Submit_button | Reset_button

type model = {
  name : Editor.t;
  email : Editor.t;
  role : role option;
  submitted : bool;
  focus : field;
}

type msg =
  | Submit
  | Reset
  | Quit
  | Set_role of role
  | Focus of field
  | Set_name of Editor.t
  | Set_email of Editor.t

let role_label = function
  | Developer -> "Developer"
  | Designer -> "Designer"
  | Manager -> "Manager"
  | Analyst -> "Analyst"

let roles =
  [
    Select.item Developer ~description:"Write code" "Developer";
    Select.item Designer ~description:"Create designs" "Designer";
    Select.item Manager ~description:"Lead teams" "Manager";
    Select.item Analyst ~description:"Analyze data" "Analyst";
  ]

let init () =
  ( {
      name = Editor.empty;
      email = Editor.empty;
      role = Some Developer;
      submitted = false;
      focus = Name;
    },
    Cmd.none )

let update msg model =
  match msg with
  | Submit -> ({ model with submitted = true }, Cmd.none)
  | Reset ->
      ( {
          name = Editor.empty;
          email = Editor.empty;
          role = Some Developer;
          submitted = false;
          focus = Name;
        },
        Cmd.none )
  | Set_role role -> ({ model with role = Some role }, Cmd.none)
  | Focus focus -> ({ model with focus }, Cmd.none)
  | Set_name name ->
      let submitted =
        model.submitted
        && String.equal (Editor.text model.name) (Editor.text name)
      in
      ({ model with name; submitted }, Cmd.none)
  | Set_email email ->
      let submitted =
        model.submitted
        && String.equal (Editor.text model.email) (Editor.text email)
      in
      ({ model with email; submitted }, Cmd.none)
  | Quit -> (model, Cmd.quit)

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
                  input
                    ~size:{ width = px 25; height = px 1 }
                    ~focused:(model.focus = Name) ~on_focus_request:(Focus Name)
                    ~placeholder:"Enter your name..." ~state:model.name
                    ~on_change:(fun v -> Set_name v)
                    ~on_submit:(fun _ -> Submit)
                    ();
                ];
              (* Email field *)
              box ~flex_direction:Row ~align_items:Center ~gap:(gap 1)
                [
                  box ~size:{ width = px 8; height = px 1 } [ text "Email:" ];
                  input
                    ~size:{ width = px 25; height = px 1 }
                    ~focused:(model.focus = Email)
                    ~on_focus_request:(Focus Email)
                    ~placeholder:"Enter your email..." ~state:model.email
                    ~on_change:(fun v -> Set_email v)
                    ~on_submit:(fun _ -> Submit)
                    ();
                ];
              (* Role selector *)
              box ~flex_direction:Row ~align_items:Flex_start ~gap:(gap 1)
                [
                  box ~size:{ width = px 8; height = px 1 } [ text "Role:" ];
                  select
                    ~size:{ width = px 25; height = px 5 }
                    ~focused:(model.focus = Role) ~on_focus_request:(Focus Role)
                    ~show_description:true ~selected_background:accent
                    ~selected_text_color:Ansi.Color.black ~equal:( = )
                    ~selected:model.role
                    ~on_change:(fun role -> Set_role role)
                    roles;
                ];
              (* Buttons *)
              box ~flex_direction:Row ~gap:(gap 2)
                [
                  button ~border:true ~border_color ~padding:(padding 1)
                    ~focused:(model.focus = Submit_button)
                    ~on_focus_request:(Focus Submit_button)
                    ~focused_border_color:accent ~on_activate:Submit "Submit";
                  button ~border:true ~border_color ~padding:(padding 1)
                    ~focused:(model.focus = Reset_button)
                    ~on_focus_request:(Focus Reset_button)
                    ~focused_border_color:accent ~on_activate:Reset "Reset";
                ];
              (* Submission feedback *)
              (if model.submitted then
                 box ~padding:(padding 1)
                   ~background:(Ansi.Color.grayscale ~level:3)
                   [
                     text
                       ~style:(Ansi.Style.make ~fg:Ansi.Color.green ())
                       (Printf.sprintf "Submitted: %s <%s> as %s"
                          (Editor.text model.name) (Editor.text model.email)
                          (Option.fold ~none:"no role" ~some:role_label
                             model.role));
                   ]
               else text ~style:hint "Fill in the form and click Submit");
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [
          text ~style:hint
            "Tab/Shift+Tab navigate  •  Enter/Space activate buttons  •  \
             Ctrl+q quit";
        ];
    ]

(* Tab and Shift+Tab move focus through the fields in view order, each
   asking with its [on_focus_request]. Quitting works whatever is focused, so
   its keys are taken before the focused field sees them. *)
let subscriptions _model =
  Sub.on_keys ~phase:Capture
    [ (Shortcut.ctrl 'q', Quit); (Shortcut.escape, Quit) ]

let () = run { init; update; view; subscriptions }
