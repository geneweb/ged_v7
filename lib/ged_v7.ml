open Parser
module Types = Types
module Version = Version

let read_file fname =
  let ic = open_in_bin fname in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  (* TODO handle other bom *)
  let bom = "\239\187\191" in
  if String.starts_with ~prefix:bom s then String.sub s 3 (String.length s - 3)
  else s

(* change fname for content? *)
let parse fname =
  let s = read_file fname in
  let lexbuf = Sedlexing.Utf8.from_string s in
  let tokens = Lexer.Basic.parse [] lexbuf |> Lexer.make in

  let provider =
    let tokens = ref tokens in
    let dummy_pos = Stdlib.Lexing.dummy_pos in
    fun () ->
      match !tokens with
      | [] -> (EOF, dummy_pos, dummy_pos)
      | tok :: l ->
          tokens := l;
          (* Token.print tok; *)
          (* TODO get start stop from lexer *)
          (tok, dummy_pos, dummy_pos)
  in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Parser.gedcom_tree
  in
  parser provider
