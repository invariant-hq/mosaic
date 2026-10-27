module Keystroke = Matrix_input.Keystroke

type 'a binding = Shortcut.t * 'a

let binding_equal equal_action (a, action_a) (b, action_b) =
  Shortcut.equal a b && equal_action action_a action_b

(* The bindings in order, [custom] after [defaults], so that [Shortcut.find]'s
   last match is the custom one, with each action held in the option [action]
   returns, so that finding one allocates nothing. *)
type 'a t = (Shortcut.t * 'a option) list

let make ~defaults ?(custom = []) () =
  List.map
    (fun (shortcut, action) -> (shortcut, Some action))
    (defaults @ custom)

let action t ev = try Shortcut.find t ev with Not_found -> None
