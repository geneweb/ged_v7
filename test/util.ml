(* TODO find files of each version *)
let invalid_file = Filename.concat "assets" "royal92.ged"

let files =
  Array.map (Filename.concat "assets")
    [|
      "minimal70.ged";
      "maximal70.ged";
      "escapes.ged";
      "long-url.ged";
      "extension-record.ged";
      "same-sex-marriage.ged";
      "spaces.ged";
      "voidptr_reordered.ged";
      "remarriage1_reordered.ged";
      "remarriage2_reordered.ged";
      "voidptr.ged";
      "remarriage1.ged";
      "remarriage2.ged";
    |]

let read_file fname =
  let ic = open_in_bin fname in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s
