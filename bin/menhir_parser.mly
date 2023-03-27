%token EOF END
%token HEAD GEDC VERS SCHMA TAG NAME SOUR CORP ADDR ADR1 ADR2 ADR3 CITY STAE POST CTRY PHON EMAIL FAX WWW DATA DATE
%token TIME COPR DEST LANG PLAC FORM SUBM NOTE SNOTE TRAN MIME EVEN PHRASE ROLE PAGE QUAY TEXT CROP TOP LEFT WIDTH HEIGHT OBJE TITL RESN FAM RELI LATI LONG CAUS ASSO EXID TYPE SDATE AGNC TRLR
%token AGE NCHI UID HUSB WIFE MAP RESI FACT DIVF ANUL MARC CENS ENGA MARL MARB MARR MARS DIV NO CHIL TEMP STAT SLGS REFN CHAN CREA
%token GIVN INDI SURN NICK SPFX NPFX NSFX SEX PROP EDUC DSCR SSN CAST IDNO NMR OCCU NATI BURI BLES BAPM RETI IMMI FCOM GRAD PROB CREM BASM BARM EMIG DEAT WILL NATU CHRA CONF ORDN FAMC BIRT CHR ADOP
%token INIL SLGC BAPL CONL ENDL PEDI FAMS ALIA DESI ANCI FILE MEDI REPO AUTH CALN ABBR PUBL
%token<string> VALUE
%token<string option> XREF

%{ open Types %}

%start <Types.gedcom> gedcom
%%

let line(TAG_NAME) ==
  | TAG_NAME; v = VALUE; END;
  { v }

(* a line with value a XREF *)
let xref_line(TAG_NAME) ==
  | TAG_NAME; v = XREF; END;
  { v }

let schema ==
  | SCHMA; l = list(line(TAG)); END;
  {l}

let address ==
  | ADDR;
    address = VALUE;
    adr1 = option(line(ADR1));
    adr2 = option(line(ADR2));
    adr3 = option(line(ADR3));
    city = option(line(CITY));
    state = option(line(STAE));
    postal_code = option(line(POST));
    country = option(line(CTRY));
    END;
  {{ address; adr1; adr2; adr3; city; state;
    postal_code; country }}

let corporation ==
  | CORP;
    corporation = VALUE;
    address = option(address);
    phone = list(line(PHON));
    email = list(line(EMAIL));
    fax = list(line(FAX));
    www = list(line(WWW));
    END;
  {{corporation; address; phone; email; fax; www}}

let date_exact_time ==
  | DATE;
    date = VALUE;
    time = option(line(TIME));
    END;
    {{date;time}}

let date_phrase ==
  | DATE;
    date = VALUE;
    phrase = option(line(PHRASE));
    END;
    {{date;phrase}}

let data ==
  | DATA;
    data = VALUE;
    date_time = option(date_exact_time);
    copyright = option(line(COPR));
    END;
    {
      {data; date_time; copyright}}

let source ==
  | SOUR;
    source = VALUE;
    version = option(line(VERS));
    name = option(line(NAME));
    corporation = option(corporation);
    data = option(data);
    END;
  {{source; version ; name; corporation; data}}

let plac ==
  | PLAC;
    FORM;
    s = VALUE;
    END;
    END;
  {s}

let translation ==
  | TRAN;
    translation = VALUE;
    mime = option(line(MIME));
    language = option(line(LANG));
    END;
  {{translation;mime;language}}

let aux_date_time_phrase(X) ==
  | X;
    date = VALUE;
    time = option(line(TIME));
    phrase = option(line(PHRASE));
    END;
  {{date;time;phrase}}

let date_time_phrase ==
  | o = aux_date_time_phrase(DATE);
  {o}

let s_date_time_phrase ==
  | o = aux_date_time_phrase(SDATE);
  {o}

let text ==
  | TEXT;
    text = VALUE;
    mime = option(line(MIME));
    language = option(line(LANG));
    END;
  {{text;mime;language}}

let sour_data ==
  | DATA;
    date_time_phrase = option(date_time_phrase);
    texts = list(text);
    END;
  {{date_time_phrase; texts}}

let role ==
  | ROLE;
    role = VALUE;
    phrase = option(line(PHRASE));
    END;
  {{role;phrase}}

let source_event ==
  | EVEN;
    event = VALUE;
    phrase = option(line(PHRASE));
    role = option(role);
    END;
  {{event;phrase;role}}

let crop ==
  | CROP;
    top = option(line(TOP));
    left = option(line(LEFT));
    height = option(line(HEIGHT));
    width = option(line(WIDTH));
    END;
  {{top;left;height;width}}

let multimedia_link ==
  | OBJE;
    xref = XREF;
    crop = option(crop);
    title = option(line(TITL));
    END;
  {{xref;crop;title}}

let source_citation ==
  | SOUR;
    xref = XREF;
    page = option(line(PAGE));
    data = option(sour_data);
    event = option(source_event);
    quality = option(line(QUAY));
    multimedia_links = list(multimedia_link);
    notes = list(note);
    END;
  {{xref;page;data;event;quality; notes; multimedia_links}}

let note ==
  | NOTE;
    note = VALUE;
    mime = option(line(MIME));
    language = option(line(LANG));
    translation = list(translation);
    source_citation = list(source_citation);
    END;
  {Note {
    note;mime;language;translation;source_citation}}
  | SNOTE;
    xref = XREF;
    END;
  {
    Snote xref}

let head ==
| HEAD; GEDC; version = line(VERS); END;
    schema = option(schema);
    source = option(source);
    destination = option(line(DEST));
    date_time = option(date_exact_time);
    submitter = option(xref_line(SUBM));
    copyright = option(line(COPR));
    language = option(line(LANG));
    default_format = option(plac);
    note = option(note);
  END;
  {{version;schema;source; destination; date_time; copyright; language; submitter; default_format; note;}}

let place_translation ==
  | TRAN;
    translation = VALUE;
    language = line(LANG);
    END;
  {{translation; language}}

let map ==
  | MAP;
    latitude = line(LATI);
    longitude = line(LONG);
    END;
  {{latitude;longitude}}

let exid ==
  | EXID;
    exid = VALUE;
    t = option(line(TYPE));
    END;
  {{exid;type'=t}}

let place ==
  | PLAC;
    place = VALUE;
    form = option(line(FORM));
    language = option(line(LANG));
    translations = list(place_translation);
    map = option(map);
    exid = list(exid);
    notes = list(note);
    END;
  {{place;form;language;translations;map;exid;notes}}

let role_phrase ==
  | ROLE;
    role = VALUE;
    phrase = option(line(PHRASE));
    END;
  {{role; phrase}}

let association_structure ==
  | ASSO;
    xref = XREF;
    phrase = option(line(PHRASE));
    role_phrase = role_phrase;
    notes = list(note);
    source_citations = list(source_citation);
    END;
  {{xref;phrase;role_phrase;notes;source_citations}}

let event_detail ==
  | date = option(date_time_phrase);
    place = option(place);
    address = option(address);
    phones = list(line(PHON));
    emails = list(line(EMAIL));
    faxes = list(line(FAX));
    wwws = list(line(WWW));
    agency = option(line(AGNC));
    religion = option(line(RELI));
    cause = option(line(CAUS));
    restriction = option(line(RESN));
    sort_date = option(s_date_time_phrase);
    associations = list(association_structure);
    notes = list(note);
    source_citations = list(source_citation);
    multimedia_links = list(multimedia_link);
    uids = list(line(UID));
  {{date;place;address;phones;emails;faxes;wwws;agency;religion;cause;restriction;sort_date;associations;notes;source_citations;multimedia_links;uids}}

(* TODO make a function like line but with 2 param for rules like that? *)
let age_phrase ==
  | AGE;
    age = VALUE;
    phrase = option(line(PHRASE));
    END;
  {{age;phrase}}

let aux_event_detail(X) ==
  | X;
    x = age_phrase;
    END;
  {x}

let family_event_detail ==
  | husb = option(aux_event_detail(HUSB));
    wife = option(aux_event_detail(WIFE));
    event_detail = event_detail;
  {{husb; wife; event_detail}}

let family_attribute_structure ==
  | NCHI;
    nb_children = VALUE;
    t = option(line(TYPE));
    (* supposed to be optional, but conflict because the record can be empty ...*)
    family_event_detail = family_event_detail;
    END;
  {Number_of_children (nb_children, t, family_event_detail)}
  | RESI;
    nb_children = VALUE;
    t = option(line(TYPE));
    family_event_detail = family_event_detail;
    END;
  {Residence (nb_children, t, family_event_detail)}
  | FACT;
    nb_children = VALUE;
    t = line(TYPE);
    family_event_detail = family_event_detail;
    END;
  {Fact (nb_children, t, family_event_detail)}

let aux_family_event_structure(X) ==
  | X;
    value = option(VALUE);
    t = option(line(TYPE));
    family_event_detail = family_event_detail;
    END;
  {{value; type'=t; family_event_detail}}

let family_event_structure ==
  | o = aux_family_event_structure(ANUL);
  {Annulment o}
  | o = aux_family_event_structure(CENS);
  {Census o}
  | o = aux_family_event_structure(DIV);
  {Divorce o}
  | o = aux_family_event_structure(DIVF);
  {Divorce_filing o}
  | o = aux_family_event_structure(ENGA);
  {Engagement o}
  | o = aux_family_event_structure(MARB);
  {Marriage_banns o}
  | o = aux_family_event_structure(MARC);
  {Mariage_contract o}
  | o = aux_family_event_structure(MARL);
  {Marriage_license o}
  | o = aux_family_event_structure(MARR);
  {Marriage o}
  | o = aux_family_event_structure(MARS);
  {Marriage_settlement o}
  | (* for EVEN, it's VALUE and TYPE are not optional .. *)
    EVEN;
    value = VALUE;
    t = line(TYPE);
    family_event_detail = family_event_detail;
    END;
  {Familly_Event {value= Some value; type'= Some t; family_event_detail}}

let non_event_structure ==
  | NO; value = VALUE; date_phrase = option(date_phrase); notes = list(note); source_citations = list (source_citation); END;
  {{value; date_phrase; notes; source_citations}}

(* TODO refacto *)
let value_phrase(X) ==
  | X;
    value = VALUE;
    phrase = option(line(PHRASE));
    END;
  {{value ;phrase}}

let xref_phrase(X) ==
  | X; xref = XREF; phrase = option(line(PHRASE)); END;
  {{xref; phrase}}

let ordinance_status ==
  | STAT; status = VALUE; date_time = date_exact_time; END;
  {{status; date_time}}

let lds_ordinance_detail ==
  | date_time_phrase = option(date_time_phrase);
    temple = option(line(TEMP));
    place = option(place);
    status = option(ordinance_status);
    notes = list(note);
    source_citations = list(source_citation);
  {{date_time_phrase; temple; place; status; notes; source_citations}}

let lds_spouse_sealing ==
  | SLGS; o = lds_ordinance_detail; END;
  {o}

let identifier_structure ==
  | REFN; reference = VALUE; t = option(line(TYPE)); END;
  {Reference (reference, t) }
  | UID; uid = VALUE; END;
  {Uid uid}
  | EXID; exid = VALUE; t = option(line(TYPE)); END;
  {Exid (exid, t) }

let change_date ==
  | CHAN;
    date_time = date_exact_time;
    notes = list(note);
    END;
  {{date_time; notes}}

let creation_date ==
  | CREA;
    date_time = date_exact_time;
    END;
  {date_time}

let family_record ==
  | xref = XREF; FAM;
    restriction = option(line(RESN));
    attributes = list(family_attribute_structure);
    events = list(family_event_structure);
    non_events = list(non_event_structure);
    husband = option(xref_phrase(HUSB));
    wife = option(xref_phrase(WIFE));
    children = list(xref_phrase(CHIL));
    associations = list(association_structure);
    submitters = list(xref_line(SUBM));
    lds_spouse_sealing = list(lds_spouse_sealing);
    identifiers = list(identifier_structure);
    notes = list(note);
    source_citations = list(source_citation);
    multimedia_links = list(multimedia_link);
    change_date = option(change_date);
    creation_date = option(creation_date);
    END;
  {Fam {xref; restriction; attributes; events; non_events; husband; wife; children; associations; submitters; lds_spouse_sealing;
  identifiers; notes; source_citations; multimedia_links; change_date; creation_date}}

let type_phrase ==
  | TYPE; t = VALUE;
    phrase = option(line(PHRASE));
    END;
  {{type'=t;phrase}}

let personal_name_pieces ==
  | name_prefixes = list(line(NPFX));
    given_names = list(line(GIVN));
    nicknames = list(line(NICK));
    surname_prefixes = list(line(SPFX));
    surnames = list(line(SURN));
    name_suffixes = list(line(NSFX));
  {{name_prefixes;given_names;nicknames;surname_prefixes;surnames;name_suffixes}}

let personal_name_translation ==
  | TRAN;
    name = VALUE;
    language = line(LANG);
    name_pieces = personal_name_pieces;
    END;
  {{name; language; name_pieces}}

let personal_name_structure ==
  | NAME;
    name = VALUE;
    type_phrase = option(type_phrase);
    name_pieces = personal_name_pieces;
    translations = list(personal_name_translation);
    notes = list(note);
    source_citations = list(source_citation);
    END;
  {{name; type_phrase; name_pieces; translations; notes; source_citations}}

let individual_event_detail ==
  | event_detail = event_detail;
    age_phrase = option(age_phrase);
  {{event_detail;age_phrase}}

let aux_individual_attribute_structure(X) ==
  | X; value = VALUE;
    t = option(line(TYPE));
    event = individual_event_detail;
    END;
  {{value; type'=t;event}}

(* same but force to have type *)
let aux_type_individual_attribute_structure(X) ==
  | X; value = VALUE;
    t = line(TYPE);
    event = individual_event_detail;
    END;
  {{value; type'=Some t;event}}

let individual_attribute_structure ==
| o =  aux_individual_attribute_structure(CAST); {Caste o}
| o =  aux_individual_attribute_structure(DSCR); {Description o}
| o =  aux_individual_attribute_structure(EDUC); {Education o }
| o =  aux_individual_attribute_structure(NATI); {Nationality o}
| o =  aux_individual_attribute_structure(NCHI); {Number_of_children_indi o}
| o =  aux_individual_attribute_structure(NMR); {Number_of_marriage o}
| o =  aux_individual_attribute_structure(OCCU); {Occupation o}
| o =  aux_individual_attribute_structure(PROP); {Property o}
| o =  aux_individual_attribute_structure(RELI); {Regigion o}
| o =  aux_individual_attribute_structure(RESI); {Residence_indi o}
| o =  aux_individual_attribute_structure(SSN); {Social_security_number o}
| o =  aux_individual_attribute_structure(TITL); {Title o}
| o =  aux_type_individual_attribute_structure(IDNO); {Identification_number o}
| o =  aux_type_individual_attribute_structure(FACT); {Fact_indi o}


let aux_individual_event_structure(X) ==
  | X;
    value = option(VALUE);
    t = option(line(TYPE));
    event_detail = individual_event_detail;
    END;
  {{value;type'=t;event_detail; family_child = None}}

let aux_famc_individual_event_structure(X) ==
  | X;
    value = option(VALUE);
    t = option(line(TYPE));
    event_detail = individual_event_detail;
    xref_opt = option(xref_line(FAMC));
    END;
  {
  let family_child =
    match xref_opt with
    | None -> None
    | Some xref -> Some {xref; adoption_phrase=None}
  in
    {value;type'=t;event_detail;family_child}}

let adoption_phrase ==
  | ADOP;
    adoption = VALUE;
    phrase = option(line(PHRASE));
    END;
  {{adoption; phrase}}

let family_child ==
  | FAMC;
    xref = XREF;
    adoption_phrase = option(adoption_phrase);
    END;
  {{xref; adoption_phrase}}

let individual_event_structure ==
| o = aux_individual_event_structure(BAPM); {Baptism o}
| o = aux_individual_event_structure(BARM); {Bar_mitzvah o}
| o = aux_individual_event_structure(BASM); {Bas_mitzvah o}
| o = aux_individual_event_structure(BLES); {Blessing o}
| o = aux_individual_event_structure(BURI); {Burial o}
| o = aux_individual_event_structure(CENS); {Census_indi o}
| o = aux_individual_event_structure(CHRA); {Christening_adult o}
| o = aux_individual_event_structure(CONF); {Confirmation o}
| o = aux_individual_event_structure(CREM); {Cremation o}
| o = aux_individual_event_structure(DEAT); {Death o}
| o = aux_individual_event_structure(EMIG); {Emigration o}
| o = aux_individual_event_structure(FCOM); {First_communion o}
| o = aux_individual_event_structure(GRAD); {Graduation o}
| o = aux_individual_event_structure(IMMI); {Immigration o}
| o = aux_individual_event_structure(NATU); {Naturalization o}
| o = aux_individual_event_structure(ORDN); {Ordination o}
| o = aux_individual_event_structure(PROB); {Probate o}
| o = aux_individual_event_structure(RETI); {Retirement (* one day ... very far away *) o}
| o = aux_individual_event_structure(WILL); {Will o}
| (* like the others but type is not optional *)
    EVEN;
    value = option(VALUE);
    t = line(TYPE);
    event_detail = individual_event_detail;
    END;
  {Event_indi {value;type'=Some t;event_detail; family_child = None}}
| o = aux_famc_individual_event_structure(BIRT); {Birth o}
| o = aux_famc_individual_event_structure(CHR); {Christening o}
| ADOP;
  value = option(VALUE);
  t = option(line(TYPE));
  event_detail = individual_event_detail;
  family_child = option(family_child);
  END;
 {Adoption {value;type'=t;event_detail; family_child}}

let lds_individual_ordinance ==
  | BAPL; detail = lds_ordinance_detail; END;
  {Lds_baptism detail}
  | CONL; detail = lds_ordinance_detail; END;
  {Lds_confirmation detail}
  | ENDL; detail = lds_ordinance_detail; END;
  {Lds_endowment detail}
  | INIL; detail = lds_ordinance_detail; END;
  {Lds_initiatory detail}
  | SLGC; detail = lds_ordinance_detail; xref = xref_line(FAMC); END;
  {Lds_sealing (detail, xref)}

let individual_famc ==
  | FAMC;
    xref = XREF;
    pedigree = option(value_phrase(PEDI));
    status = option(value_phrase(STAT));
    notes = list(note);
    END;
  {{xref;pedigree;status;notes}}

let family_spouse ==
  | FAMS;
    xref = XREF;
    notes = list(note);
    END;
  {{xref;notes}}

(*
  +1 <<IDENTIFIER_STRUCTURE>>              {0:M}
  +1 <<NOTE_STRUCTURE>>                    {0:M}
  +1 <<SOURCE_CITATION>>                   {0:M}
  +1 <<MULTIMEDIA_LINK>>                   {0:M}
  +1 <<CHANGE_DATE>>                       {0:1}
  +1 <<CREATION_DATE>>                     {0:1}
*)

let individual_record ==
  | xref = XREF; INDI;
    restriction = option(line(RESN));
    names = list(personal_name_structure);
    sex = option(line(SEX));
    attributes = list(individual_attribute_structure);
    events = list(individual_event_structure);
    non_events = list(non_event_structure);
    lds_individual_ordinances = list(lds_individual_ordinance);
    family_childs = list(individual_famc);
    family_spouses = list(family_spouse);
    submitters = list(xref_line(SUBM));
    associations = list(association_structure);
    aliases = list(xref_phrase(ALIA));
    ancestor_interests = list(xref_line(ANCI));
    descendant_interests = list(xref_line(DESI));
    identifiers = list(identifier_structure);
    notes = list(note);
    source_citations = list(source_citation);
    multimedia_links = list(multimedia_link);
    change_date = option(change_date);
    creation_date = option(creation_date);
    END;
  {Individual {xref;restriction; names; sex; attributes; events; non_events; lds_individual_ordinances;family_childs;family_spouses; submitters; associations;aliases;ancestor_interests;descendant_interests;
  identifiers;notes;source_citations;multimedia_links;change_date;creation_date}}

let translation_format ==
    | TRAN; translation = VALUE; format = line(FORM); END;
  {{translation; format}}

let multimedia_file ==
  | FILE;
    file = VALUE;
    FORM;
    media_type = VALUE;
    medium_phrase = option(value_phrase(MEDI));
    END;
    title = option(line(TITL));
    translations = list(translation_format);
    END;
  {{file;media_type;medium_phrase;title;translations}}

let multimedia_record ==
  | xref = XREF; OBJE;
    restriction = option(line(RESN));
    files = nonempty_list(multimedia_file);
    identifiers = list(identifier_structure);
    notes = list(note);
    source_citations = list(source_citation);
    change_date = option(change_date);
    creation_date = option(creation_date);
    END;
  {Multimedia {xref;restriction;files;identifiers;notes;source_citations;change_date;creation_date}}

let repository_record ==
  | xref = XREF; REPO;
    name = line(NAME);
    address = option(address);
    phones = list(line(PHON));
    emails = list(line(EMAIL));
    faxes = list(line(FAX));
    wwws = list(line(WWW));
    notes = list(note);
    identifiers = list(identifier_structure);
    change_date = option(change_date);
    creation_date = option(creation_date);
    END;
  {Repository {xref;name;address;phones;emails;faxes;wwws;notes;identifiers;change_date;creation_date}}

let shared_note_record ==
  | xref = XREF; SNOTE;
    note = VALUE;
    mime = option(line(MIME));
    language = option(line(LANG));
    translations = list(translation);
    source_citations = list(source_citation);
    identifiers = list(identifier_structure);
    change_date = option(change_date);
    creation_date = option(creation_date);
    END;
  {Shared_note {xref;note;mime;language;translations;source_citations;identifiers;change_date;creation_date}}


let source_data_event ==
  | EVEN;
    event = VALUE;
    date_phrase = option(value_phrase(DATE));
    place = option(place);
    END;
  {{event;date_phrase;place}}


let source_data ==
  | DATA;
    events = list(source_data_event);
    agency = option(line(AGNC));
    notes = list(note);
    END;
  {{events;agency;notes}}

let call_number ==
  | CALN; call_number = VALUE;
    medium_phrase = option(value_phrase(MEDI));
    END;
  {{call_number;medium_phrase}}

let source_repository_citation ==
  | REPO; xref = XREF;
    notes = list(note);
    call_numbers = list(call_number);
    END;
  {{xref;notes;call_numbers}}

let source_record ==
  | xref = XREF; SOUR;
    data = option(source_data);
    author = option(line(AUTH));
    title = option(line(TITL));
    abbreviation = option(line(ABBR));
    publication = option(line(PUBL));
    text = option(text);
    source_repository_citations = list(source_repository_citation);
    identifiers = list(identifier_structure);
    notes = list(note);
    multimedia_links = list(multimedia_link);
    change_date = option(change_date);
    creation_date = option(creation_date);
    END;
  {Source {xref;data;author;title;abbreviation;publication;text;source_repository_citations;identifiers;notes;multimedia_links;change_date;creation_date}}

(*
n @XREF:SUBM@ SUBM                         {1:1}  g7:record-SUBM
  +1 NAME <Text>                           {1:1}  g7:NAME
  +1 <<ADDRESS_STRUCTURE>>                 {0:1}
  +1 PHON <Special>                        {0:M}  g7:PHON
  +1 EMAIL <Special>                       {0:M}  g7:EMAIL
  +1 FAX <Special>                         {0:M}  g7:FAX
  +1 WWW <Special>                         {0:M}  g7:WWW
  +1 <<MULTIMEDIA_LINK>>                   {0:M}
  +1 LANG <Language>                       {0:M}  g7:SUBM-LANG
  +1 <<IDENTIFIER_STRUCTURE>>              {0:M}
  +1 <<NOTE_STRUCTURE>>                    {0:M}
  +1 <<CHANGE_DATE>>                       {0:1}
  +1 <<CREATION_DATE>>                     {0:1}
*)

let submitter_record ==
  | xref = XREF; SUBM;
    name = line(NAME);
    address = option(address);
    phones = list(line(PHON));
    emails = list(line(EMAIL));
    faxes = list(line(FAX));
    wwws = list(line(WWW));
    multimedia_links = list(multimedia_link);
    languages = list(line(LANG));
    identifiers = list(identifier_structure);
    notes = list(note);
    change_date = option(change_date);
    creation_date = option(creation_date);
    END;
  {Submitter {xref;name;address;phones;emails;faxes;wwws;multimedia_links;languages;identifiers;notes;change_date;creation_date}}

let record ==
  | fam = family_record;
  {fam}
  | indi = individual_record;
  {indi}
  | multimedia = multimedia_record;
  {multimedia}
  | repository = repository_record;
  {repository}
  | shared_note = shared_note_record;
  {shared_note}
  | source = source_record;
  {source}
  | submitter = submitter_record;
  {submitter}

let gedcom :=
    | head = head ; records = list(record); TRLR; END; EOF;
  {{head; records}}
