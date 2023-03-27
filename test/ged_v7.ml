let print fname tree =
  Format.printf "--- %s --- @.%a@." fname Ged_v7.Types.pp_gedcom_tree tree

let () = Array.iter (fun fname -> print fname (Ged_v7.parse fname)) Util.files
