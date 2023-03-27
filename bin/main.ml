let read_file fname =
  let ic = open_in fname in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  (* TODO handle other bom *)
  let bom = "\239\187\191" in
  if String.starts_with ~prefix:bom s then String.sub s 3 (String.length s - 3)
  else s

let () = print_endline "Hello, World!"

let () =
  let s =
    read_file
      (String.concat Filename.dir_sep [ "test"; "assets"; "maximal70.ged" ])
  in
  Format.printf "--- @.";
  let lexbuf = Sedlexing.Utf8.from_string s in
  let tokens = Line.Ast_1.parse [] lexbuf in
  List.iter Line.Ast_1.print_token tokens;
  Format.printf "--- @.";
  (*
  let tokens = Line.Ast_2.make tokens in
  List.iter Line.Ast_2.print_token tokens;
  *)
  ()
