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

(* Parses GEDCOM lines *)
module Ast_1 = struct
  exception Lexing_error of string

  type token = Tag_n of int | Tag of string | Xref of string | Value of string

  let pp_token fmt tok =
    match tok with
    | Tag_n n -> Format.fprintf fmt "Tag_n %d" n
    | Tag s -> Format.fprintf fmt "Tag %s" s
    | Xref s -> Format.fprintf fmt "Xref %s" s
    | Value s -> Format.fprintf fmt "Value %s" s

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
    | eof -> List.rev acc
    | _ -> raise (Lexing_error "Reached impossible place in parse")
end

(* changes Tag_n for END tokens to make structure defined by TAG ... END brackets *)
(* remove extension tags *)
module Ast_2 = struct
  type token = End | Tag of string | Xref of string | Value of string

  exception Gedcom_structure_error

  let pp_token fmt tok =
    match tok with
    | End -> Format.fprintf fmt "End"
    | Tag s -> Format.fprintf fmt "Tag %s" s
    | Xref s -> Format.fprintf fmt "Xref %s" s
    | Value s -> Format.fprintf fmt "Value %s" s

  let print_token tok = Format.printf "%a@." pp_token tok

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
            | Value _ | Xref _ -> 0
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
          | (Xref _ | Value _ | End) as token -> (0, token :: acc)
          | Tag s as token ->
              if String.starts_with ~prefix:"_" s then
                (* we switch to extension tag mode (ext_tag_level_count > 0)*)
                (* if last token was a xref we need to remove it from acc *)
                let acc = match acc with Xref _ :: l -> l | _l -> acc in
                (1, acc)
              else (0, token :: acc))
      (0, []) tokens |> snd |> List.rev

  (* convert tokens
     remove Tag_n and add End tokens*)
  let convert tokens =
    let last_n = ref (-1) in
    List.fold_left
      (fun acc token ->
        match token with
        | Ast_1.Tag s -> Tag s :: acc
        | Xref s -> Xref s :: acc
        | Value s -> Value s :: acc
        | Tag_n n ->
            let ends =
              if n > !last_n then []
              else List.init (!last_n - n + 1) (fun _i -> End)
            in
            last_n := n;
            ends @ acc)
      [] tokens
    |> (* End for TRLR tag *) List.cons End |> List.rev

  let make (tokens : Ast_1.token list) = tokens |> convert |> trash_extensions
end
