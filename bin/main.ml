open Menhir_parser

let read_file fname =
  let ic = open_in fname in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  (* TODO handle other bom *)
  let bom = "\239\187\191" in
  if String.starts_with ~prefix:bom s then String.sub s 3 (String.length s - 3)
  else s

let _files =
  [
    "abc.ged";
    "escapes.ged";
    "long-url.ged";
    "minimal70.ged";
    "remarriage2.ged";
    "spaces.ged";
    "extension-record.ged";
    "maximal70.ged";
    "remarriage1.ged";
    "same-sex-marriage.ged";
    "voidptr.ged";
  ]

let () =
  let s =
    read_file (String.concat Filename.dir_sep [ "test"; "assets"; "abc.ged" ])
  in
  Format.printf "--- Ast_1 @.";
  let lexbuf = Sedlexing.Utf8.from_string s in
  let tokens = Line.Ast_1.parse [] lexbuf in
  List.iter Line.Ast_1.print_token tokens;
  Format.printf "--- @.";
  Format.printf "@.";
  Format.printf "--- Ast_2 @.";
  let tokens = Line.Ast_2.make tokens in
  List.iter Token.print tokens;
  Format.printf "--- @.";

  let provider =
    let tokens = ref tokens in
    Format.printf "@.@.@. --- MENHIR --- @.@.@.";
    let dummy_pos = Stdlib.Lexing.dummy_pos in
    fun () ->
      match !tokens with
      | [] -> (EOF, dummy_pos, dummy_pos)
      | tok :: l ->
          tokens := l;
          Token.print tok;
          (* TODO start stop? *)
          (tok, dummy_pos, dummy_pos)
  in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Menhir_parser.gedcom
  in
  let _gedcom = parser provider in
  ()
