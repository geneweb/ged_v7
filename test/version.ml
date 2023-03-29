let read_file fname =
  (* TODO move this elsewhere *)
  let ic = open_in fname in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

(* TODO find files of each version *)
let invalid_file = "royal92.ged"

let files =
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

let check_version fname =
  match Version.detect (read_file (Filename.concat "assets" fname)) with
  | Error e -> Error (Format.sprintf "Version error in %s: %s" fname e)
  | Ok v -> Ok (Format.sprintf "VERSION: %s@." (Version.to_string v))

let () =
  assert (Result.is_error (check_version invalid_file));
  List.iter (fun fname -> assert (Result.is_ok (check_version fname))) files
