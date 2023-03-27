type xref = string option [@@deriving show]

(* all possible substructure in gedcom *)
(* TODO should we not bother and make them all "... of sub"? *)
(* TODO Some tags such as DATE, have special value type *)
(* Tags do not always have the same value type everywhere...:
      DATE can be of value "DatePeriod" or "DateExact" or "DateValue" *)
(* TODO parse date values *)
type sub =
  | Version of string
  | Destination of string
  | Copyright of string
  | Language of string
  | Tag of string
  | Adr1 of string
  | Adr2 of string
  | Adr3 of string
  | City of string
  | State of string
  | Postal_code of string
  | Country of string
  | Phone of string
  | Email of string
  | Fax of string
  | Www of string
  | Mime of string
  | Page of string
  | Restriction of string
  | Phrase of string
  | Time of string
  | Responsible_agency of string
  | Cause of string
  | Uid of string
  | Temple of string
  | Quality_of_data of string
  | Date of v_subs
  | Submitter_xref of xref
  | Snote of xref
  | Schema of subs
  | Gedc of subs
  | Source_header of v_subs
  | Source_citation of xref_subs
  | Corporation of v_subs
  | Address of v_subs
  | Translation of v_subs
  | Data of v_subs
  | Note of v_subs
  | Husband_xref of xref_subs
  | Wife_xref of xref_subs
  | Children_xref of xref_subs
  | Husband of subs
  | Wife of subs
  | Age of v_subs
  | Annulement of v_opt_subs
  | Divorce of v_opt_subs
  | Divorce_filing of v_opt_subs
  | Engagement of v_opt_subs
  | Marriage_banns of v_opt_subs
  | Marriage_contract of v_opt_subs
  | Marriage_license of v_opt_subs
  | Marriage of v_opt_subs
  | Marriage_settlement of v_opt_subs
  | Event of v_subs
  | Place of v_subs
  | Sdate of v_subs
  | Association of xref_subs
  | Role of v_subs
  | Object of xref_subs
  | Reference of v_subs
  | Exid of v_subs
  | Non_event of v_subs
  | Lds_souse_sealing of subs
  | Status of v_subs
  | Title of v_subs
  | Change_date of subs
  | Creation_date of subs
  | Name of v_subs
  | Type of v_subs
  | Name_prefix of string
  | Given_name of string
  | Nickname of string
  | Surname_prefix of string
  | Surname of string
  | Name_suffix of string
  | Sex of string
  | Caste of v_subs
  | Description of v_subs
  | Education of v_subs
  | Identification_number of v_subs
  | Nationality of v_subs
  | Number_of_children of v_subs
  | Residence of v_subs
  | Number_of_marriages of v_subs
  | Occupation of v_subs
  | Property of v_subs
  | Religion of v_subs
  | Social_security_number of v_subs
  | Fact of v_subs
  | Adoption of v_opt_subs
  | Baptism of v_opt_subs
  | Bar_Mitzvah of v_opt_subs
  | Bas_Mitzvah of v_opt_subs
  | Birth of v_opt_subs
  | Blessing of v_opt_subs
  | Burial of v_opt_subs
  | Census of v_opt_subs
  | Christening of v_opt_subs
  | Christening_adult of v_opt_subs
  | Confirmation of v_opt_subs
  | Cremation of v_opt_subs
  | Death of v_opt_subs
  | Emigration of v_opt_subs
  | First_communion of v_opt_subs
  | Graduation of v_opt_subs
  | Immigration of v_opt_subs
  | Naturalization of v_opt_subs
  | Ordination of v_opt_subs
  | Probate of v_opt_subs
  | Retirement of v_opt_subs
  | Will of v_opt_subs
  | Family_child of xref_subs
  | Lds_event of subs
  | Pedigree of v_subs
  | Family_spouse of xref_subs
  | Alias of xref_subs
  | Ancestor_interest of xref_subs
  | Descendant_interest of xref_subs
  | File of v_subs
  | Medium of v_subs
  | Format of v_subs
  | Text of v_subs
  | Crop of subs
  | Top of string
  | Left of string
  | Height of string
  | Width of string
  | Source_repository_citation of xref_subs
  | Latitude of string
  | Longitude of string
  | Map of subs
  | Author of string
  | Abbreviation of string
  | Publication of string
  | Call_number of v_subs
[@@deriving show]

and subs = sub list [@@deriving show]
and v_subs = string * subs [@@deriving show]
and v_opt_subs = string option * subs [@@deriving show]
and xref_subs = xref * subs [@@deriving show]

type record =
  | Family of xref_subs
  | Individual of xref_subs
  | Multimedia of xref_subs
  | Repository of xref_subs
  | Shared_note of (xref * string * subs)
  | Source of xref_subs
  | Submitter of xref_subs
[@@deriving show]

type header = sub list [@@deriving show]
type gedcom_tree = { head : header; records : record list } [@@deriving show]
