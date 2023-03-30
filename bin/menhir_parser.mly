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

let exid ==
  | (exid,t) = line_line_opt(EXID,TYPE);
  {{exid;type'=t}}

let role_phrase ==
  | (role,phrase) = line_line_opt(ROLE,PHRASE);
  {{role; phrase}}

let age_phrase ==
  | (age,phrase) = line_line_opt(AGE,PHRASE);
  {{age; phrase}}

let value_phrase(X) ==
  | (value,phrase) = line_line_opt(X,PHRASE);
  {{value; phrase}}

let xref_phrase(X) ==
  | X; xref = XREF; phrase = option(line(PHRASE)); END;
  {{xref; phrase}}

let type_phrase ==
  | (t,phrase) = line_line_opt(TYPE,PHRASE);
  {{type'=t;phrase}}

let adoption_phrase ==
  | (adoption,phrase) = line_line_opt(ADOP,PHRASE);
  {{adoption; phrase}}

let date_exact_time ==
  | (date,time) = line_line_opt(DATE,TIME);
    {{date;time}}

let date_phrase ==
  | (date,phrase) = line_line_opt(DATE,PHRASE);
    {{date;phrase}}

let role ==
  | (role,phrase) = line_line_opt(ROLE,PHRASE);
  {{role;phrase}}

(* -- Miscs -- *)

let place_translation ==
  | (a,b) = line_line(TRAN,LANG);
  {{translation=a; language=b}}

let schema ==
  | SCHMA; l = list(line(TAG)); END;
  {l}

let corporation ==
  | CORP;
    corporation = value;
    address = option(address_structure);
    phone = list(line(PHON));
    email = list(line(EMAIL));
    fax = list(line(FAX));
    www = list(line(WWW));
    END;
  {{corporation; address; phone; email; fax; www}}

let data ==
  | DATA;
    data = value;
    date_time = option(date_exact_time);
    copyright = option(line(COPR));
    END;
  {{data; date_time; copyright}}

let source ==
  | SOUR;
    source = value;
    version = option(line(VERS));
    name = option(line(NAME));
    corporation = option(corporation);
    data = option(data);
    END;
  {{source; version ; name; corporation; data}}

let translation ==
  | TRAN;
    translation = value;
    mime = option(line(MIME));
    language = option(line(LANG));
    END;
  {{translation;mime;language}}

let aux_date_time_phrase(X) ==
  | X;
    date = value;
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
    text = value;
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

let source_event ==
  | EVEN;
    event = value;
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
    notes = list(note_structure);
    END;
  {{xref;page;data;event;quality; notes; multimedia_links}}

let plac ==
  | PLAC;
    s = line(FORM);
    END;
  {s}

let map ==
  | MAP;
    latitude = line(LATI);
    longitude = line(LONG);
    END;
  {{latitude;longitude}}

let event_detail ==
  | date = option(date_time_phrase);
    place = option(place_structure);
    address = option(address_structure);
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
    notes = list(note_structure);
    source_citations = list(source_citation);
    multimedia_links = list(multimedia_link);
    uids = list(line(UID));
  {{date;place;address;phones;emails;faxes;wwws;agency;religion;cause;restriction;sort_date;associations;notes;source_citations;multimedia_links;uids}}

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

let ordinance_status ==
  | STAT; status = value; date_time = date_exact_time; END;
  {{status; date_time}}

let lds_ordinance_detail ==
  | date_time_phrase = option(date_time_phrase);
    temple = option(line(TEMP));
    place = option(place_structure);
    status = option(ordinance_status);
    notes = list(note_structure);
    source_citations = list(source_citation);
  {{date_time_phrase; temple; place; status; notes; source_citations}}

let lds_spouse_sealing ==
  | SLGS; o = lds_ordinance_detail; END;
  {o}

let change_date ==
  | CHAN;
    date_time = date_exact_time;
    notes = list(note_structure);
    END;
  {{date_time; notes}}

let creation_date ==
  | CREA;
    date_time = date_exact_time;
    END;
  {date_time}

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
    name = value;
    language = line(LANG);
    name_pieces = personal_name_pieces;
    END;
  {{name; language; name_pieces}}

let individual_event_detail ==
  | event_detail = event_detail;
    age_phrase = option(age_phrase);
  {{event_detail;age_phrase}}

let family_child ==
  | FAMC;
    xref = XREF;
    adoption_phrase = option(adoption_phrase);
    END;
  {{xref; adoption_phrase}}

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
    notes = list(note_structure);
    END;
  {{xref;pedigree;status;notes}}

let family_spouse ==
  | FAMS;
    xref = XREF;
    notes = list(note_structure);
    END;
  {{xref;notes}}

let translation_format ==
    | TRAN; translation = value; format = line(FORM); END;
  {{translation; format}}

let multimedia_file ==
  | FILE;
    file = value;
    FORM;
    media_type = value;
    medium_phrase = option(value_phrase(MEDI));
    END;
    title = option(line(TITL));
    translations = list(translation_format);
    END;
  {{file;media_type;medium_phrase;title;translations}}

let source_data_event ==
  | EVEN;
    event = value;
    date_phrase = option(value_phrase(DATE));
    place = option(place_structure);
    END;
  {{event;date_phrase;place}}

let source_data ==
  | DATA;
    events = list(source_data_event);
    agency = option(line(AGNC));
    notes = list(note_structure);
    END;
  {{events;agency;notes}}

let call_number ==
  | CALN; call_number = value;
    medium_phrase = option(value_phrase(MEDI));
    END;
  {{call_number;medium_phrase}}

let source_repository_citation ==
  | REPO; xref = XREF;
    notes = list(note_structure);
    call_numbers = list(call_number);
    END;
  {{xref;notes;call_numbers}}

(* -- Substructures -- *)

let address_structure ==
  | ADDR;
    address = value;
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

let association_structure ==
  | ASSO;
    xref = XREF;
    phrase = option(line(PHRASE));
    role_phrase = role_phrase;
    notes = list(note_structure);
    source_citations = list(source_citation);
    END;
  {{xref;phrase;role_phrase;notes;source_citations}}

let family_attribute_structure ==
  | NCHI;
    nb_children = value;
    t = option(line(TYPE));
    (* supposed to be optional, but conflict because the record can be empty ...*)
    family_event_detail = family_event_detail;
    END;
  {Number_of_children (nb_children, t, family_event_detail)}
  | RESI;
    nb_children = value;
    t = option(line(TYPE));
    family_event_detail = family_event_detail;
    END;
  {Residence (nb_children, t, family_event_detail)}
  | FACT;
    nb_children = value;
    t = line(TYPE);
    family_event_detail = family_event_detail;
    END;
  {Fact (nb_children, t, family_event_detail)}

let aux_family_event_structure(X) ==
  | X;
    value = value_opt;
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
    v = value;
    t = line(TYPE);
    family_event_detail = family_event_detail;
    END;
  {Familly_Event {value = Some v; type'= Some t; family_event_detail}}

let identifier_structure ==
  | REFN; reference = value; t = option(line(TYPE)); END;
  {Reference (reference, t) }
  | UID; uid = value; END;
  {Uid uid}
  | EXID; exid = value; t = option(line(TYPE)); END;
  {Exid (exid, t) }

let aux_individual_attribute_structure(X) ==
  | X; value = value;
    t = option(line(TYPE));
    event = individual_event_detail;
    END;
  {{value; type'=t;event}}

(* same but force to have type *)
let aux_type_individual_attribute_structure(X) ==
  | X; value = value;
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
    value = value_opt;
    t = option(line(TYPE));
    event_detail = individual_event_detail;
    END;
  {{value;type'=t;event_detail; family_child = None}}

let aux_famc_individual_event_structure(X) ==
  | X;
    value = value_opt;
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
    v = value;
    t = line(TYPE);
    event_detail = individual_event_detail;
    END;
  {Event_indi {value = Some v;type'=Some t;event_detail; family_child = None}}
| o = aux_famc_individual_event_structure(BIRT); {Birth o}
| o = aux_famc_individual_event_structure(CHR); {Christening o}
| ADOP;
  value = value_opt;
  t = option(line(TYPE));
  event_detail = individual_event_detail;
  family_child = option(family_child);
  END;
 {Adoption {value;type'=t;event_detail; family_child}}

let non_event_structure ==
  | NO; value = value; date_phrase = option(date_phrase); notes = list(note_structure); source_citations = list (source_citation); END;
  {{value; date_phrase; notes; source_citations}}

let note_structure ==
  | NOTE;
    note = value;
    mime = option(line(MIME));
    language = option(line(LANG));
    translation = list(translation);
    source_citation = list(source_citation);
    END;
  {Note {note;mime;language;translation;source_citation}}
  | SNOTE; xref = XREF; END;
  {Snote xref}

let personal_name_structure ==
  | NAME;
    name = value;
    type_phrase = option(type_phrase);
    name_pieces = personal_name_pieces;
    translations = list(personal_name_translation);
    notes = list(note_structure);
    source_citations = list(source_citation);
    END;
  {{name; type_phrase; name_pieces; translations; notes; source_citations}}

let place_structure ==
  | PLAC;
    place = value;
    form = option(line(FORM));
    language = option(line(LANG));
    translations = list(place_translation);
    map = option(map);
    exid = list(exid);
    notes = list(note_structure);
    END;
  {{place;form;language;translations;map;exid;notes}}

(* -- Records -- *)

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
    notes = list(note_structure);
    source_citations = list(source_citation);
    multimedia_links = list(multimedia_link);
    change_date = option(change_date);
    creation_date = option(creation_date);
    END;
  {Fam {xref; restriction; attributes; events; non_events; husband; wife; children; associations; submitters; lds_spouse_sealing;
  identifiers; notes; source_citations; multimedia_links; change_date; creation_date}}

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
    notes = list(note_structure);
    source_citations = list(source_citation);
    multimedia_links = list(multimedia_link);
    change_date = option(change_date);
    creation_date = option(creation_date);
    END;
  {Individual {xref;restriction; names; sex; attributes; events; non_events; lds_individual_ordinances;family_childs;family_spouses; submitters; associations;aliases;ancestor_interests;descendant_interests;
  identifiers;notes;source_citations;multimedia_links;change_date;creation_date}}

let multimedia_record ==
  | xref = XREF; OBJE;
    restriction = option(line(RESN));
    files = nonempty_list(multimedia_file);
    identifiers = list(identifier_structure);
    notes = list(note_structure);
    source_citations = list(source_citation);
    change_date = option(change_date);
    creation_date = option(creation_date);
    END;
  {Multimedia {xref;restriction;files;identifiers;notes;source_citations;change_date;creation_date}}

let repository_record ==
  | xref = XREF; REPO;
    name = line(NAME);
    address = option(address_structure);
    phones = list(line(PHON));
    emails = list(line(EMAIL));
    faxes = list(line(FAX));
    wwws = list(line(WWW));
    notes = list(note_structure);
    identifiers = list(identifier_structure);
    change_date = option(change_date);
    creation_date = option(creation_date);
    END;
  {Repository {xref;name;address;phones;emails;faxes;wwws;notes;identifiers;change_date;creation_date}}

let shared_note_record ==
  | xref = XREF; SNOTE;
    note = value;
    mime = option(line(MIME));
    language = option(line(LANG));
    translations = list(translation);
    source_citations = list(source_citation);
    identifiers = list(identifier_structure);
    change_date = option(change_date);
    creation_date = option(creation_date);
    END;
  {Shared_note {xref;note;mime;language;translations;source_citations;identifiers;change_date;creation_date}}

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
    notes = list(note_structure);
    multimedia_links = list(multimedia_link);
    change_date = option(change_date);
    creation_date = option(creation_date);
    END;
  {Source {xref;data;author;title;abbreviation;publication;text;source_repository_citations;identifiers;notes;multimedia_links;change_date;creation_date}}

let submitter_record ==
  | xref = XREF; SUBM;
    name = line(NAME);
    address = option(address_structure);
    phones = list(line(PHON));
    emails = list(line(EMAIL));
    faxes = list(line(FAX));
    wwws = list(line(WWW));
    multimedia_links = list(multimedia_link);
    languages = list(line(LANG));
    identifiers = list(identifier_structure);
    notes = list(note_structure);
    change_date = option(change_date);
    creation_date = option(creation_date);
    END;
  {Submitter {xref;name;address;phones;emails;faxes;wwws;multimedia_links;languages;identifiers;notes;change_date;creation_date}}

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
    note = option(note_structure);
  END;
  {{version;schema;source; destination; date_time; copyright; language; submitter; default_format; note;}}

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
