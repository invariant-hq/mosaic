(* Links the terminal core with no unix (see the dune file); each library is
   used, so that none is left out of the link. *)

let () =
  let grid = Matrix_grid.create ~width:8 ~height:1 () in
  let hits = Matrix_screen.Hit_grid.create ~width:8 ~height:1 in
  Matrix_image.draw ~hits (Matrix_image.text "pure") grid;
  let screen = Matrix_screen.create () in
  Matrix_screen.build screen ~width:8 ~height:1 (fun g _ ->
      Matrix_grid.blit ~src:grid ~dst:g);
  ignore (Matrix_screen.render screen : string);
  ignore
    (Matrix_screen.Region.draw
       (Matrix_screen.Region.create ())
       ~cols:8 ~rows:2 (Some grid)
      : string);
  ignore (Vte.feed_string (Vte.create ~rows:1 ~cols:8 ()) "pure" : _ list);
  ignore (Matrix_input.Decoder.create () : Matrix_input.Decoder.t);
  ignore (Matrix_text.grapheme_count "pure" : int);
  ignore
    (Matrix_charts.layout ~width:8 ~height:1 (Matrix_charts.spark [ 1. ])
      : Matrix_charts.Layout.t);
  ignore (Ansi.strip "pure" : string);
  let terminal =
    Matrix_terminal.make ~output:ignore ~tty:false
      (Matrix_terminal.Caps.derive
         (Matrix_terminal.Caps.evidence
            (Matrix_terminal.Caps.env (fun _ -> None) ~win32:false)))
  in
  Matrix_terminal.close terminal;
  ignore (Matrix_terminfo.expand "%p1%d" [ Int 1 ] : string)
