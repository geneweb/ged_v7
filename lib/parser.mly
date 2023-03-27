%token EOF END
%token HEAD GEDC VERS SCHMA TAG NAME SOUR CORP ADDR ADR1 ADR2 ADR3 CITY STAE POST CTRY PHON EMAIL FAX WWW DATA DATE
%token TIME COPR DEST LANG PLAC FORM SUBM NOTE SNOTE TRAN MIME EVEN PHRASE ROLE PAGE QUAY TEXT CROP TOP LEFT WIDTH HEIGHT OBJE TITL RESN FAM RELI LATI LONG CAUS ASSO EXID TYPE SDATE AGNC TRLR
%token AGE NCHI UID HUSB WIFE MAP RESI FACT DIVF ANUL MARC CENS ENGA MARL MARB MARR MARS DIV NO CHIL TEMP STAT SLGS REFN CHAN CREA
%token GIVN INDI SURN NICK SPFX NPFX NSFX SEX PROP EDUC DSCR SSN CAST IDNO NMR OCCU NATI BURI BLES BAPM RETI IMMI FCOM GRAD PROB CREM BASM BARM EMIG DEAT WILL NATU CHRA CONF ORDN FAMC BIRT CHR ADOP
%token INIL SLGC BAPL CONL ENDL PEDI FAMS ALIA DESI ANCI FILE MEDI REPO AUTH CALN ABBR PUBL
%token<string> VALUE
%token<string option> XREF

%{ open Types%}

%start <Types.gedcom_tree> gedcom_tree
%%

(* -- Helper functions -- *)

(* Note that production LineVal does not match the empty string. Because empty payloads and missing payloads are considered equivalent, both a structure with no payload and a structure with the empty string as its payload are encoded with no LineVal and no space after the Tag. *)
let value_opt ==
  | v = option(VALUE);
  {v}

let value ==
  | v = value_opt;
  {Option.value ~default:"" v}

let line(X) ==
  | X; v = value; END;
  { v }

let xref_line(X) ==
  | X; v = XREF; END;
  { v }

let line_line(X,Y) ==
  | X;
    a = value;
    b = line(Y);
    END;
  {a,b}

let line_line_opt(X,Y) ==
  | X;
    a = value;
    b = option(line(Y));
    END;
  {a,b}

let sub_aux ==
  | o = line(VERS); {Version o}
  | o = line(TAG); {Tag o}
  | o = line(PAGE); {Page o}
  | o = line(ADR1); {Adr1 o}
  | o = line(ADR2); {Adr2 o}
  | o = line(ADR3); {Adr3 o}
  | o = line(CITY); {City o}
  | o = line(STAE); {State o}
  | o = line(POST); {Postal_code o}
  | o = line(CTRY); {Country o}
  | o = line(DEST); {Destination o}
  | o = line(PHON); {Phone o}
  | o = line(EMAIL); {Email o}
  | o = line(FAX); {Fax o}
  | o = line(WWW); {Www o}
  | o = line(COPR); {Copyright o}
  | o = line(LANG); {Language o}
  | o = line(MIME); {Mime o}
  | o = line(RESN); {Restriction o}
  | o = line(PHRASE); {Phrase o}
  | o = line(TIME); {Time o}
  | o = line(AGNC); {Responsible_agency o}
  | o = line(CAUS); {Cause o}
  | o = line(UID); {Uid o}
  | o = line(TEMP); {Temple o}
  | o = line(QUAY); {Quality_of_data o}
  | o = line(NPFX); {Name_prefix o}
  | o = line(GIVN); {Given_name o}
  | o = line(NICK); {Nickname o}
  | o = line(SPFX); {Surname_prefix o}
  | o = line(SURN); {Surname o}
  | o = line(NSFX); {Name_suffix o}
  | o = line(SEX); {Sex o}
  | o = line(TOP); {Top o}
  | o = line(LEFT); {Top o}
  | o = line(HEIGHT); {Top o}
  | o = line(WIDTH); {Top o}
  | o = line(LATI); {Latitude o}
  | o = line(LONG); {Longitude o}
  | o = line(AUTH); {Author o}
  | o = line(ABBR); {Abbreviation o}
  | o = line(PUBL); {Publication o}
  | o = xref_line(SUBM); {Submitter_xref o}
  | o = xref_line(SNOTE); {Snote o}
  | o = sub(GEDC); {Gedc o}
  | o = sub(SCHMA); {Schema o}
  | o = sub(HUSB); {Husband o}
  | o = sub(WIFE); {Wife o}
  | o = sub(SLGS); {Lds_souse_sealing o}
  | o = sub(CHAN); {Change_date o}
  | o = sub(CREA); {Creation_date o}
  | o = sub(BAPL); {Lds_event o}
  | o = sub(CONL); {Lds_event o}
  | o = sub(ENDL); {Lds_event o}
  | o = sub(INIL); {Lds_event o}
  | o = sub(SLGC); {Lds_event o}
  | o = sub(CROP); {Crop o}
  | o = sub(MAP); {Map o}
  | o = v_sub(NAME); {Name o (* should be Personal_name if v_sub or Name if just a line *)}
  | o = v_sub(SOUR); {Source_header o}
  | o = v_sub(CORP); {Corporation o}
  | o = v_sub(ADDR); {Address o}
  | o = v_sub(DATA); {Data o}
  | o = v_sub(DATE); {Date o}
  | o = v_sub(FORM); {Format o (* can be line too *)}
  | o = v_sub(NOTE); {Note o}
  | o = v_sub(TRAN); {Translation o}
  | o = v_sub(TYPE); {Type o (* can be just line too *)}
  | o = v_sub(AGE); {Age o}
  | o = v_sub(EVEN); { Event o}
  | o = v_sub(PLAC); {(* TODO PLAC in HEAD: default format *) Place o}
  | o = v_sub(SDATE); {Sdate o}
  | o = v_sub(ROLE); {Role o}
  | o = v_sub(REFN); {Reference o}
  | o = v_sub(EXID); {Exid o}
  | o = v_sub(NO); {Non_event o}
  | o = v_sub(STAT); {Status o}
  | o = v_sub(TITL); (*it can be a line too*) {Title o}
  | o = v_sub(CAST); {Caste o}
  | o = v_sub(DSCR); {Description o}
  | o = v_sub(EDUC); {Education o}
  | o = v_sub(IDNO); {Identification_number o}
  | o = v_sub(NATI); {Nationality o}
  | o = v_sub(NCHI); {Number_of_children o}
  | o = v_sub(RESI); {Residence o}
  | o = v_sub(NMR);  {Number_of_marriages o}
  | o = v_sub(OCCU); {Occupation o}
  | o = v_sub(PROP); {Property o}
  | o = v_sub(RELI); {Religion o}
  | o = v_sub(SSN);  {Social_security_number o}
  | o = v_sub(FACT); {Fact o}
  | o = v_sub(PEDI); {Pedigree o}
  | o = v_sub(FILE); {File o}
  | o = v_sub(MEDI); {Medium o}
  | o = v_sub(TEXT); {Text o}
  | o = v_sub(CALN); {Call_number o}
  | o = xref_sub(FAMC); {Family_child o}
  | o = xref_sub(OBJE); {Object o}
  | o = xref_sub(FAMS); {Family_spouse o}
  | o = xref_sub(ALIA); {Alias o}
  | o = xref_sub(ANCI); {Ancestor_interest o}
  | o = xref_sub(DESI); {Descendant_interest o}
  | o = xref_sub(REPO); {Source_repository_citation o}
  | o = xref_sub(ASSO); {Association o}
  | o = xref_sub(SOUR); {Source_citation o}
  | o = xref_sub(HUSB); {Husband_xref o}
  | o = xref_sub(WIFE); {Wife_xref o}
  | o = xref_sub(CHIL); {Children_xref o}
  | o = v_opt_sub(ANUL); {Annulement o}
  | o = v_opt_sub(DIV); {Divorce o}
  | o = v_opt_sub(DIVF); {Divorce_filing o}
  | o = v_opt_sub(ENGA); {Engagement o}
  | o = v_opt_sub(MARB); {Marriage_banns o}
  | o = v_opt_sub(MARC); { (* v=oNTCxGDpIdo *) Marriage_contract o}
  | o = v_opt_sub(MARL); { Marriage_license o}
  | o = v_opt_sub(MARR); { Marriage o}
  | o = v_opt_sub(MARS); { Marriage_settlement o}
  | o = v_opt_sub(ADOP); {Adoption o}
  | o = v_opt_sub(BAPM); {Baptism o}
  | o = v_opt_sub(BARM); {Bar_Mitzvah o}
  | o = v_opt_sub(BASM); {Bas_Mitzvah o}
  | o = v_opt_sub(BIRT); {Birth o}
  | o = v_opt_sub(BLES); {Blessing o}
  | o = v_opt_sub(BURI); {Burial o}
  | o = v_opt_sub(CENS); {Census o}
  | o = v_opt_sub(CHR);  {Christening o}
  | o = v_opt_sub(CHRA); {Christening_adult o}
  | o = v_opt_sub(CONF); {Confirmation o}
  | o = v_opt_sub(CREM); {Cremation o}
  | o = v_opt_sub(DEAT); {Death o}
  | o = v_opt_sub(EMIG); {Emigration o}
  | o = v_opt_sub(FCOM); {First_communion o}
  | o = v_opt_sub(GRAD); {Graduation o}
  | o = v_opt_sub(IMMI); {Immigration o}
  | o = v_opt_sub(NATU); {Naturalization o}
  | o = v_opt_sub(ORDN); {Ordination o}
  | o = v_opt_sub(PROB); {Probate o}
  | o = v_opt_sub(RETI); {Retirement o}
  | o = v_opt_sub(WILL); {Will o}

let xref_sub(X) ==
  | X; xref = XREF; o = list(sub_aux); END; {xref,o}

let v_sub(X) ==
  | X; v = value; o = list(sub_aux); END; {v,o}

let v_opt_sub(X) ==
  | X; v = value_opt; o = list(sub_aux); END; {v,o}

let sub(X) ==
  | X; o = list(sub_aux); END; {o}

let record_aux(X) ==
  | xref = XREF; subs = sub(X); { xref,subs }

let record ==
  | o = record_aux(FAM); {Family o}
  | o = record_aux(INDI); {Individual o}
  | o = record_aux(OBJE); {Multimedia o}
  | o = record_aux(REPO); {Repository o}
  | xref = XREF; (v,subs) = v_sub(SNOTE) (* only this record has a value *); {Shared_note (xref,v,subs)}
  | o = record_aux(SOUR); {Source o}
  | o = record_aux(SUBM); {Submitter o}

let gedcom_tree :=
    | head = sub(HEAD); records = list(record); TRLR; END; EOF;
  {{head; records}}
