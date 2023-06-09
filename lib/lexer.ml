let banned =
  [%sedlex.regexp?
    ( 0x0 .. 0x8
    | 0x0B .. 0x0C
    | 0x0E .. 0x1F
    | 0x7F
    | 0x80 .. 0x9F
    | 0xD800 .. 0xDFFF
    | 0xFFFE .. 0xFFFF )]

let anychar = [%sedlex.regexp? Sub (any, banned)]
let eol = [%sedlex.regexp? 0x0D, Opt 0x0A | 0x0A]

(* non-EOL, as defined by gedcom *)
let non_eol = [%sedlex.regexp? 0x09 | 0x20 .. 0x10FFFF]
let digit = [%sedlex.regexp? '0' .. '9']
let integer = [%sedlex.regexp? Plus digit]
let nonzero = [%sedlex.regexp? Sub (digit, '0')]
let ucletter = [%sedlex.regexp? 'A' .. 'Z']
let level = [%sedlex.regexp? '0' | Plus nonzero]
let tag_char = [%sedlex.regexp? ucletter | digit | '_']
let std_tag = [%sedlex.regexp? ucletter, Star tag_char]
let ext_tag = [%sedlex.regexp? '_', Plus tag_char]
let tag = [%sedlex.regexp? std_tag | ext_tag]
let xref = [%sedlex.regexp? '@', Plus tag_char, '@']
let void_ptr = [%sedlex.regexp? "@VOID@"]
let pointer = [%sedlex.regexp? void_ptr | xref]

(* non-EOL, non-@ - as defined by gedcom *)
let non_at = [%sedlex.regexp? 0x09 | 0x20 .. 0x3F | 0x41 .. 0x10FFFF]
let line_str = [%sedlex.regexp? (non_at | "@@"), Star non_eol]
let line_val = [%sedlex.regexp? pointer | line_str]
let enum = [%sedlex.regexp? std_tag | integer | ext_tag]

exception Lexing_error of string
exception Gedcom_structure_error

(* TODO could do all steps in one pass (?)
   avoid Line.rev *)

(* Parses GEDCOM lines *)
module Basic = struct
  type token =
    | Tag_n of int
    | Tag of string
    | Xref of string
    | Value of string
    | Eof

  let pp_token fmt tok =
    match tok with
    | Tag_n n -> Format.fprintf fmt "Tag_n %d" n
    | Tag s -> Format.fprintf fmt "Tag %s" s
    | Xref s -> Format.fprintf fmt "Xref %s" s
    | Value s -> Format.fprintf fmt "Value %s" s
    | Eof -> Format.fprintf fmt "Eof"

  let print_token tok = Format.printf "%a@." pp_token tok

  let rule_value buf =
    let match_value buf =
      match%sedlex buf with
      | eol -> None
      | pointer ->
          let s = Sedlexing.Utf8.lexeme buf in
          Some (Xref s)
      | line_str ->
          let s = Sedlexing.Utf8.lexeme buf in
          (* TODO if starts_with "@@" remove first "@" *)
          Some (Value s)
      | _ -> raise (Lexing_error "Reached impossible place in rule_value")
    in
    (* consume ' ' before value *)
    match%sedlex buf with
    | eol -> None
    | ' ' -> match_value buf
    | _ -> raise (Lexing_error "Reached impossible place in rule_value")

  let rec rule_tag buf =
    match%sedlex buf with
    | ' ' -> rule_tag buf
    | tag ->
        let s = Sedlexing.Utf8.lexeme buf in
        Tag s
    | _ -> raise (Lexing_error "Reached impossible place in rule_tag")

  let rec rule_xref buf =
    match%sedlex buf with
    | ' ' -> rule_xref buf
    | xref ->
        let s = Sedlexing.Utf8.lexeme buf in
        Some (Xref s)
    | _ -> None

  let rec parse acc buf =
    match%sedlex buf with
    | ' ' -> parse acc buf
    | eol -> parse acc buf
    | level ->
        let n = Tag_n (int_of_string (Sedlexing.Utf8.lexeme buf)) in
        let xref = rule_xref buf in
        let tag = rule_tag buf in
        let value = rule_value buf in
        let acc =
          Option.to_list value @ (tag :: Option.to_list xref) @ (n :: acc)
        in
        parse acc buf
    | eof -> List.rev (Eof :: acc)
    | _ -> raise (Lexing_error "Reached impossible place in parse")
end

type token = End | Tag of string | Xref of string | Value of string | Eof

let pp_token fmt tok =
  match tok with
  | End -> Format.fprintf fmt "End"
  | Tag s -> Format.fprintf fmt "Tag %s" s
  | Xref s -> Format.fprintf fmt "Xref %s" s
  | Value s -> Format.fprintf fmt "Value %S" s
  | Eof -> Format.fprintf fmt "Eof"

let print_token tok = Format.printf "%a@." pp_token tok

(* convert tokens remove Tag_n and add End tokens to make parsing structure possible;
   changes Tag_n for END tokens to make structure defined by TAG ... END brackets *)
let from_basic tokens =
  let last_n = ref (-1) in
  List.fold_left
    (fun acc token ->
      match token with
      | Basic.Tag s -> Tag s :: acc
      | Xref s -> Xref s :: acc
      | Value s -> Value s :: acc
      | Eof -> Eof :: (* add End for TRLR *) End :: acc
      | Tag_n n ->
          let ends =
            if n > !last_n then []
            else List.init (!last_n - n + 1) (fun _i -> End)
          in
          last_n := n;
          ends @ acc)
    [] tokens
  |> List.rev

(* remove extension tags *)
(* TODO? add a EXT_TAG of string token and keep them *)
let trash_extensions tokens =
  (* if we see a extension tag, we increment this ref,
     for each End we decr it; this way (assuming correct gedcom structure)
     it will be back to 0 when we are out of the extension record we want to ignore *)
  List.fold_left
    (fun (ext_tag_level_count, acc) token ->
      if ext_tag_level_count < 0 then raise Gedcom_structure_error
      else if ext_tag_level_count > 0 then
        (* if we are in an extension tag we want to ignore this token *)
        let c =
          match token with
          | Value _ | Xref _ | Eof -> 0
          | End ->
              (* we go down one level for each End *)
              -1
          | Tag _ ->
              (* we go up one level for each Tag _ *)
              1
        in
        (ext_tag_level_count + c, acc)
      else
        (* if we are not in an extension tag we check if we are on a new tag, which may be an extension tag *)
        match token with
        | (Xref _ | Value _ | End | Eof) as token -> (0, token :: acc)
        | Tag s as token ->
            if String.starts_with ~prefix:"_" s then
              (* we switch to extension tag mode (ext_tag_level_count > 0)*)
              (* if last token was a xref we need to remove it from acc *)
              let acc = match acc with Xref _ :: l -> l | _l -> acc in
              (1, acc)
            else (0, token :: acc))
    (0, []) tokens
  |> snd |> List.rev

(* for each Value, concatenate subsequent "CONT" tag into it *)
let flatten_cont l =
  let rec get_conts acc l =
    match l with
    | Tag "CONT" :: Value cont :: End :: l -> get_conts (cont :: acc) l
    | Tag "CONT" :: End :: l -> get_conts ("" :: acc) l
    | _ -> (List.rev acc, l)
  in
  let rec flatten_cont acc l =
    match l with
    | [] -> acc
    | Value s :: l ->
        let conts, l = get_conts [] l in
        let acc = Value (String.concat "\n" (s :: conts)) :: acc in
        flatten_cont acc l
    | x :: l -> flatten_cont (x :: acc) l
  in
  flatten_cont [] l |> List.rev

(* TODO convert to Menhir_parser.token in Ast_1 *)
let to_menhir_tokens tokens =
  List.map
    (function
      | End -> Parser.END
      | Eof -> EOF
      | Tag s -> Token.tag_of_string s
      | Xref s ->
          let ptr = if s = "@VOID@" then None else Some s in
          XREF ptr
      | Value s -> VALUE s)
    tokens

let make (tokens : Basic.token list) =
  from_basic tokens |> trash_extensions |> flatten_cont |> to_menhir_tokens
