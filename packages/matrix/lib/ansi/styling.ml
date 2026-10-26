type t = [ `Plain | Color.depth ]
type choice = [ `Auto | `Always | `Never ]

let choices = [ ("auto", `Auto); ("always", `Always); ("never", `Never) ]

(* An empty variable is unset, as NO_COLOR, force-color.org and bixense's
   CLICOLOR all say. *)
let get getenv name =
  match getenv name with Some "" | None -> None | Some _ as v -> v

(* FORCE_COLOR and CLICOLOR_FORCE force colors unless they are 0 or false,
   the values Node's tools read as off: a force read wrongly writes escapes
   into a file, where a refusal read wrongly only prints plain text. *)
let forced getenv name =
  match get getenv name with
  | None | Some ("0" | "false") -> false
  | Some _ -> true

let decide ?(color = `Auto) getenv ~tty ~dumb (colors : Color.depth) : t =
  let colors = (colors :> t) in
  match color with
  | `Always -> colors
  | `Never -> `Plain
  | `Auto ->
      let force =
        forced getenv "FORCE_COLOR" || forced getenv "CLICOLOR_FORCE"
      in
      if not (force || (tty && not dumb)) then `Plain
      else if Option.is_some (get getenv "NO_COLOR") then `Mono
      else if force then colors
      else if get getenv "CLICOLOR" = Some "0" then `Mono
      else colors
