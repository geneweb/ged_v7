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
  | PAF
  | Other

val to_string : t -> string
val detect : string -> (t, string) result
