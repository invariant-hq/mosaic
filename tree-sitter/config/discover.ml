let write_sexp filename items =
  let oc = open_out filename in
  let write_item first item =
    if not first then output_char oc ' ';
    output_string oc item
  in
  output_char oc '(';
  (match items with
  | [] -> ()
  | hd :: tl ->
      write_item true hd;
      List.iter (write_item false) tl);
  output_string oc ")\n";
  close_out oc

let cflags =
  [
    "-Iinclude";
    "-Itree-sitter";
    "-Itree-sitter/unicode";
    "-Itree-sitter/portable";
  ]

let string_has ~needle s =
  let len_s = String.length s in
  let len_n = String.length needle in
  let rec loop i =
    if i + len_n > len_s then false
    else if String.sub s i len_n = needle then true
    else loop (i + 1)
  in
  loop 0

let detect_os () =
  let lowercase = String.lowercase_ascii in
  let fallback = lowercase Sys.os_type in
  match Sys.getenv_opt "OSTYPE" with
  | Some value when value <> "" -> lowercase value
  | _ ->
      let uname () =
        try
          let ic = Unix.open_process_in "uname -s" in
          let line =
            match input_line ic with
            | line -> line
            | exception End_of_file -> ""
          in
          ignore (Unix.close_process_in ic);
          line
        with _ -> ""
      in
      let sysname = uname () in
      if sysname = "" then fallback else lowercase sysname

let clibs =
  let sys = detect_os () in
  if string_has ~needle:"darwin" sys || string_has ~needle:"mac" sys then
    [ "-undefined"; "dynamic_lookup" ]
  else if string_has ~needle:"linux" sys || string_has ~needle:"gnu" sys then
    [ "-ldl"; "-lpthread" ]
  else if
    string_has ~needle:"win32" sys
    || string_has ~needle:"win64" sys
    || string_has ~needle:"cygwin" sys
    || string_has ~needle:"mingw" sys
    || string_has ~needle:"msvc" sys
  then []
  else []

let () =
  write_sexp "cflags.sexp" cflags;
  write_sexp "clibs.sexp" clibs
