type t =
  | V_7_0
  | V_5_6
  | V_5_5_1
  | V_5_5
  | V_5_4
  | V_5_3
  | V_5_0
  | V_4
  (* unofficial format *)
  | V_5_5_EL
  | V_5_5_5
  | Other
  (* very old format *)
  | PAF

val to_string : t -> string
(** [to_string v] is the string representation of version [v] *)

val detect : string -> t option
(** [detect content] is
    [Some v] with [v] the gedcom version of [content]
    Or [None] if [content] version could not be infered *)
