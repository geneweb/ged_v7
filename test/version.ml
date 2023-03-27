let check_version fname =
  match Ged_v7.Version.detect (Util.read_file fname) with
  | None -> Error (Format.sprintf "Version error in %s: Invalid file")
  | Some v -> Ok (Format.sprintf "VERSION: %s@." (Ged_v7.Version.to_string v))

let () =
  assert (Result.is_error (check_version Util.invalid_file));
  Array.iter
    (fun fname -> assert (Result.is_ok (check_version fname)))
    Util.files
