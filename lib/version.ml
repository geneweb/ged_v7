(* GEDCOM Version Detection *)
(* Implementation of https://github.com/FamilySearch/GEDCOM/blob/main/version-detection/version-detection.md *)

type byte_order = Little_endian | Big_endian | Undefined

type t =
  | V_7_0
  | V_5_6
  | V_5_5_1
  | V_5_5
  | V_5_4
  | V_5_3
  | V_5_0
  | V_4
  | V_5_5_EL
  | V_5_5_5
  | Other
  | PAF

let to_string = function
  | V_7_0 -> "v7.0"
  | V_5_6 -> "5.6"
  | V_5_5_1 -> "5.5.1"
  | V_5_5 -> "5.5"
  | V_5_4 -> "5.4"
  | V_5_3 -> "5.3"
  | V_5_0 -> "5.0"
  | V_4 -> "4.0, 4+"
  | V_5_5_EL -> "5.5 EL"
  | V_5_5_5 -> "5.5.5"
  | Other -> "OTHER"
  | PAF -> "PAF"

let detect content =
  (* 0. Character Width and Byte Order Detection *)
  match try Some (String.sub content 0 2) with Invalid_argument _ -> None with
  | None -> None
  | Some initial_byte -> (
      let endianess =
        match initial_byte with
        | "\xFF\xFE" | "\x30\x00" -> Little_endian
        | "\xFE\xFF" | "\x00\x30" -> Big_endian
        | _ -> Undefined
      in

      (* 1. Try to read "1 GEDC" *)
      let gedc_tag =
        match endianess with
        | Little_endian -> "\x31\x00\x20\x00\x47\x00\x45\x00\x44\x00\x43\x00"
        | Big_endian -> "\x00\x31\x00\x20\x00\x47\x00\x45\x00\x44\x00\x43"
        | Undefined -> "\x31\x20\x47\x45\x44\x43"
      in
      let gedc_regex = Re.compile (Re.Perl.re gedc_tag) in
      match Re.exec_opt gedc_regex content with
      | None -> (
          (* 2. Try to read "1 SYST" *)
          let syst_tag = "\x31\x20\x53\x59\x53\x54" in
          let syst_regex = Re.compile (Re.Perl.re syst_tag) in
          match Re.exec_opt syst_regex content with
          | None -> None
          | Some _group -> Some PAF)
      | Some group -> (
          (* 3. Try to read "2 VERS" *)
          let gedc_stop = Re.Group.stop group 0 in
          let vers_tag =
            match endianess with
            | Little_endian ->
                "\x32\x00\x20\x00\x56\x00\x45\x00\x52\x00\x53\x00\x20\x00"
            | Big_endian ->
                "\x00\x32\x00\x20\x00\x56\x00\x45\x00\x52\x00\x53\x00\x20"
            | Undefined -> "\x32\x20\x56\x45\x52\x53\x20"
          in
          let vers_regex = Re.compile (Re.Perl.re vers_tag) in
          match Re.exec_opt ~pos:gedc_stop vers_regex content with
          | None -> None
          | Some group -> (
              let i = Re.Group.stop group 0 in
              (* 4. Read the next 5 * width bytes *)
              let width =
                match endianess with
                | Little_endian | Big_endian -> 2
                | Undefined -> 1
              in
              match
                try Some (String.sub content i (width * 5))
                with Invalid_argument _ -> None
              with
              | None -> None
              | Some sub ->
                  (* 5. Convert the bytes read to a 5-byte sequence by dropping all 00 bytes *)
                  let char_list =
                    String.fold_right
                      (fun c acc -> if c = '\x00' then acc else c :: acc)
                      sub []
                  in

                  (* 6. match version *)
                  let v =
                    match char_list with
                    | '\x34' :: _ -> V_4
                    | '\x35' :: '\x2E' :: '\x35' :: '\x2E' :: [ '\x31' ] ->
                        V_5_5_1
                    | '\x35' :: '\x2E' :: '\x25' :: '\x20' :: [ '\x45' ] ->
                        V_5_5_EL
                    | '\x35' :: '\x2E' :: '\x35' :: '\x2E' :: [ '\x35' ] ->
                        V_5_5_5
                    | '\x35' :: '\x2E' :: '\x36' :: _ -> V_5_6
                    | '\x35' :: '\x2E' :: '\x35' :: _ -> V_5_5
                    | '\x35' :: '\x2E' :: '\x34' :: _ -> V_5_4
                    | '\x35' :: '\x2E' :: '\x33' :: _ -> V_5_3
                    | '\x35' :: '\x2E' :: '\x30' :: _ -> V_5_0
                    | '\x37' :: '\x2E' :: '\x30' :: _ -> V_7_0
                    | _chars -> Other
                  in
                  Some v)))
