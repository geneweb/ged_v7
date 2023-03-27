module Types = Types
module Version = Version

val parse : string -> Types.gedcom_tree
(** [parse fname] parses file [fname]; assumes the file is a valid UTF-8 GEDCOM version 7.0 *)
