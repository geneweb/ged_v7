(* TODO rename some type to *_structure *)
(* TODO remove useless line_line_opt type *)

type schema = string list

type address = {
  address : string;
  adr1 : string option;
  adr2 : string option;
  adr3 : string option;
  city : string option;
  state : string option;
  postal_code : string option;
  country : string option;
}

type corporation = {
  corporation : string;
  address : address option;
  phone : string list;
  email : string list;
  fax : string list;
  www : string list;
}

type date_exact = (* TODO *) string
type date_exact_time = { date : date_exact; time : string option }

type head_sour_data = {
  data : string;
  date_time : date_exact_time option;
  copyright : string option;
}

type translation = {
  translation : string;
  mime : string option;
  language : string option;
}

type date_time_phrase = {
  date : string;
  time : string option;
  phrase : string option;
}

type text = { text : string; mime : string option; language : string option }

type source_data = {
  date_time_phrase : date_time_phrase option;
  texts : text list;
}

type role_phrase = { role : string; phrase : string option }

type source_event = {
  event : string;
  phrase : string option;
  role : role_phrase option;
}

type xref = string option

type crop = {
  top : string option;
  left : string option;
  height : string option;
  width : string option;
}

type multimedia_link = {
  xref : xref;
  crop : crop option;
  title : string option;
}

type note = Note of note_record | Snote of xref

and note_record = {
  note : string;
  mime : string option;
  language : string option;
  translation : translation list;
  source_citation : source_citation list;
}

and source_citation = {
  xref : xref;
  page : string option;
  data : source_data option;
  event : source_event option;
  quality : string option;
  multimedia_links : multimedia_link list;
  notes : note list;
}

type source = {
  source : string;
  version : string option;
  name : string option;
  corporation : corporation option;
  data : head_sour_data option;
}

type header = {
  version : string;
  schema : schema option;
  source : source option;
  destination : string option;
  date_time : date_exact_time option;
  submitter : xref option;
  copyright : string option;
  language : string option;
  default_format : (* this is the PLAC.FORM*) string option;
  note : note option;
}

type place_translation = { translation : string; language : string }
type map = { latitude : string; longitude : string }
type exid = { exid : string; type' : string option }

type place = {
  place : string;
  form : string option;
  language : string option;
  translations : place_translation list;
  map : map option;
  exid : exid list;
  notes : note list;
}

type association = {
  xref : xref;
  phrase : string option;
  role_phrase : role_phrase;
  notes : note list;
  source_citations : source_citation list;
}

type event_detail = {
  date : date_time_phrase option;
  place : place option;
  address : address option;
  phones : string list;
  emails : string list;
  faxes : string list;
  wwws : string list;
  agency : string option;
  religion : string option;
  cause : string option;
  restriction : string option;
  sort_date : date_time_phrase option;
  associations : association list;
  notes : note list;
  source_citations : source_citation list;
  multimedia_links : multimedia_link list;
  uids : string list;
}

type age_phrase = { age : string; phrase : string option }

(* event_detail is supposed to be optioanl here, but conflict because it can be empty*)
type family_event_detail = {
  husb : age_phrase option;
  wife : age_phrase option;
  event_detail : event_detail;
}

(* family_event_detail is supposed to be optional, but it causes conflict because the record can be empty *)
type family_attribute =
  | Number_of_children of string * string option * family_event_detail
  | Residence of string * string option * family_event_detail
  | Fact of string * string * family_event_detail

type family_event_record = {
  value : string option;
  type' : string option;
  family_event_detail : family_event_detail;
}

type family_event =
  | Annulment of family_event_record
  | Census of family_event_record
  | Divorce of family_event_record
  | Divorce_filing of family_event_record
  | Engagement of family_event_record
  | Marriage_banns of family_event_record
  | Mariage_contract of family_event_record
  | Marriage_license of family_event_record
  | Marriage of family_event_record
  | Marriage_settlement of family_event_record
  | Familly_Event of family_event_record

type date_phrase = { date : string; phrase : string option }

type non_event = {
  value : string;
  date_phrase : date_phrase option;
  notes : note list;
  source_citations : source_citation list;
}

(*TODO refacto *)
type value_phrase = { value : string; phrase : string option }
type xref_phrase = { xref : xref; phrase : string option }
type ordinance_status = { status : string; date_time : date_exact_time }

type lds_ordinance_detail = {
  date_time_phrase : date_time_phrase option;
  temple : string option;
  place : place option;
  status : ordinance_status option;
  notes : note list;
  source_citations : source_citation list;
}

type identifier =
  | Reference of string * string option
  | Exid of string * string option
  | Uid of string

type change_date = { date_time : date_exact_time; notes : note list }

type family_record = {
  xref : xref;
  restriction : string option;
  attributes : family_attribute list;
  events : family_event list;
  non_events : non_event list;
  husband : xref_phrase option;
  wife : xref_phrase option;
  children : xref_phrase list;
  associations : association list;
  submitters : xref list;
  lds_spouse_sealing : lds_ordinance_detail list;
  identifiers : identifier list;
  notes : note list;
  source_citations : source_citation list;
  multimedia_links : multimedia_link list;
  change_date : change_date option;
  creation_date : date_exact_time option;
}

type type_phrase = { type' : string; phrase : string option }

type name_pieces = {
  name_prefixes : string list;
  given_names : string list;
  nicknames : string list;
  surname_prefixes : string list;
  surnames : string list;
  name_suffixes : string list;
}

type name_translation = {
  name : string;
  language : string;
  name_pieces : name_pieces;
}

type personal_name = {
  name : string;
  type_phrase : type_phrase option;
  name_pieces : name_pieces;
  translations : name_translation list;
  notes : note list;
  source_citations : source_citation list;
}

type individual_event_detail = {
  event_detail : event_detail;
  age_phrase : age_phrase option;
}

type individual_attribute_struct = {
  value : string;
  type' : string option;
  event : (*should be option *) individual_event_detail;
}

type individual_attribute =
  | Caste of individual_attribute_struct
  | Description of individual_attribute_struct
  | Education of individual_attribute_struct
  | Nationality of individual_attribute_struct
  | Number_of_children_indi of individual_attribute_struct
  | Number_of_marriage of individual_attribute_struct
  | Occupation of individual_attribute_struct
  | Property of individual_attribute_struct
  | Regigion of individual_attribute_struct
  | Residence_indi of individual_attribute_struct
  | Social_security_number of individual_attribute_struct
  | Title of individual_attribute_struct
  | Identification_number of individual_attribute_struct
  | Fact_indi of individual_attribute_struct

(* TODO refacto type like that *)
type adoption_phrase = { adoption : string; phrase : string option }
type family_child = { xref : xref; adoption_phrase : adoption_phrase option }

type individual_event_struct = {
  value : string option;
  type' : string option;
  event_detail : (* should be option *) individual_event_detail;
  family_child : family_child option;
}

type individual_event =
  | Baptism of individual_event_struct
  | Bar_mitzvah of individual_event_struct
  | Bas_mitzvah of individual_event_struct
  | Blessing of individual_event_struct
  | Burial of individual_event_struct
  | Census_indi of individual_event_struct
  | Christening_adult of individual_event_struct
  | Confirmation of individual_event_struct
  | Cremation of individual_event_struct
  | Death of individual_event_struct
  | Emigration of individual_event_struct
  | First_communion of individual_event_struct
  | Graduation of individual_event_struct
  | Immigration of individual_event_struct
  | Naturalization of individual_event_struct
  | Ordination of individual_event_struct
  | Probate of individual_event_struct
  | Retirement of individual_event_struct
  | Will of individual_event_struct
  | Birth of individual_event_struct
  | Christening of individual_event_struct
  | Event_indi of individual_event_struct
  | Adoption of individual_event_struct

type lds_individual_ordinance =
  | Lds_baptism of lds_ordinance_detail
  | Lds_confirmation of lds_ordinance_detail
  | Lds_endowment of lds_ordinance_detail
  | Lds_initiatory of lds_ordinance_detail
  | Lds_sealing of lds_ordinance_detail * xref

type individual_family_child = {
  xref : xref;
  pedigree : value_phrase option;
  status : value_phrase option;
  notes : note list;
}

type family_spouse = { xref : xref; notes : note list }

type individual_record = {
  xref : xref;
  restriction : string option;
  names : personal_name list;
  sex : string option;
  attributes : individual_attribute list;
  events : individual_event list;
  non_events : non_event list;
  lds_individual_ordinances : lds_individual_ordinance list;
  family_childs : individual_family_child list;
  family_spouses : family_spouse list;
  submitters : xref list;
  associations : association list;
  aliases : xref_phrase list;
  ancestor_interests : xref list;
  descendant_interests : xref list;
  identifiers : identifier list;
  notes : note list;
  source_citations : source_citation list;
  multimedia_links : multimedia_link list;
  change_date : change_date option;
  creation_date : date_exact_time option;
}

type translation_format = { translation : string; format : string }

type multimedia_file = {
  file : string;
  media_type : string;
  medium_phrase : value_phrase option;
  title : string option;
  translations : translation_format list;
}

type multimedia_record = {
  xref : xref;
  restriction : string option;
  files : multimedia_file list;
  identifiers : identifier list;
  notes : note list;
  source_citations : source_citation list;
  change_date : change_date option;
  creation_date : date_exact_time option;
}

type repository_record = {
  xref : xref;
  name : string;
  address : address option;
  phones : string list;
  emails : string list;
  faxes : string list;
  wwws : string list;
  notes : note list;
  identifiers : identifier list;
  change_date : change_date option;
  creation_date : date_exact_time option;
}

type shared_note_record = {
  xref : xref;
  note : string;
  mime : string option;
  language : string option;
  translations : translation list;
  source_citations : source_citation list;
  identifiers : identifier list;
  change_date : change_date option;
  creation_date : date_exact_time option;
}

type call_number = { call_number : string; medium_phrase : value_phrase option }

type source_repository_citation = {
  xref : xref;
  notes : note list;
  call_numbers : call_number list;
}

type source_record_data_event = {
  event : string;
  date_phrase : value_phrase option;
  place : place option;
}

type source_record_data = {
  events : source_record_data_event list;
  agency : string option;
  notes : note list;
}

type source_record = {
  xref : xref;
  data : source_record_data option;
  author : string option;
  title : string option;
  abbreviation : string option;
  publication : string option;
  text : text option;
  source_repository_citations : source_repository_citation list;
  identifiers : identifier list;
  notes : note list;
  multimedia_links : multimedia_link list;
  change_date : change_date option;
  creation_date : date_exact_time option;
}

type submitter_record = {
  xref : xref;
  name : string;
  address : address option;
  phones : string list;
  emails : string list;
  faxes : string list;
  wwws : string list;
  multimedia_links : multimedia_link list;
  languages : string list;
  identifiers : identifier list;
  notes : note list;
  change_date : change_date option;
  creation_date : date_exact_time option;
}

type record =
  | Fam of family_record
  | Individual of individual_record
  | Multimedia of multimedia_record
  | Repository of repository_record
  | Shared_note of shared_note_record
  | Source of source_record
  | Submitter of submitter_record

type gedcom = { head : header; records : record list }
