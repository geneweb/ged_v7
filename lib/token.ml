open Parser

exception Unkwown_tag_name of string

(** [tag_of_string s] is the token corresponding to tag [s].
    raise Unkwown_tag_name if [s] is not a gedcom tag, or a extended tag *)
let tag_of_string s =
  match s with
  | "HEAD" -> HEAD
  | "GEDC" -> GEDC
  | "VERS" -> VERS
  | "SCHMA" -> SCHMA
  | "TAG" -> TAG
  | "NAME" -> NAME
  | "SOUR" -> SOUR
  | "CORP" -> CORP
  | "ADDR" -> ADDR
  | "ADR1" -> ADR1
  | "ADR2" -> ADR2
  | "ADR3" -> ADR3
  | "CITY" -> CITY
  | "STAE" -> STAE
  | "POST" -> POST
  | "CTRY" -> CTRY
  | "PHON" -> PHON
  | "EMAIL" -> EMAIL
  | "FAX" -> FAX
  | "WWW" -> WWW
  | "DATA" -> DATA
  | "DATE" -> DATE
  | "TIME" -> TIME
  | "COPR" -> COPR
  | "DEST" -> DEST
  | "LANG" -> LANG
  | "PLAC" -> PLAC
  | "FORM" -> FORM
  | "SUBM" -> SUBM
  | "NOTE" -> NOTE
  | "SNOTE" -> SNOTE
  | "TRAN" -> TRAN
  | "MIME" -> MIME
  | "TEXT" -> TEXT
  | "ROLE" -> ROLE
  | "QUAY" -> QUAY
  | "PHRASE" -> PHRASE
  | "PAGE" -> PAGE
  | "EVEN" -> EVEN
  | "WIDTH" -> WIDTH
  | "TOP" -> TOP
  | "TITL" -> TITL
  | "OBJE" -> OBJE
  | "LEFT" -> LEFT
  | "HEIGHT" -> HEIGHT
  | "CROP" -> CROP
  | "RESN" -> RESN
  | "FAM" -> FAM
  | "WIFE" -> WIFE
  | "UID" -> UID
  | "TYPE" -> TYPE
  | "SDATE" -> SDATE
  | "RESI" -> RESI
  | "RELI" -> RELI
  | "NCHI" -> NCHI
  | "MAP" -> MAP
  | "LONG" -> LONG
  | "LATI" -> LATI
  | "HUSB" -> HUSB
  | "FACT" -> FACT
  | "EXID" -> EXID
  | "CAUS" -> CAUS
  | "ASSO" -> ASSO
  | "AGNC" -> AGNC
  | "AGE" -> AGE
  | "MARS" -> MARS
  | "MARR" -> MARR
  | "MARL" -> MARL
  | "MARC" -> MARC
  | "MARB" -> MARB
  | "ENGA" -> ENGA
  | "DIVF" -> DIVF
  | "DIV" -> DIV
  | "CENS" -> CENS
  | "ANUL" -> ANUL
  | "NO" -> NO
  | "CHIL" -> CHIL
  | "TEMP" -> TEMP
  | "STAT" -> STAT
  | "SLGS" -> SLGS
  | "REFN" -> REFN
  | "CHAN" -> CHAN
  | "CREA" -> CREA
  | "SURN" -> SURN
  | "SPFX" -> SPFX
  | "NSFX" -> NSFX
  | "NPFX" -> NPFX
  | "NICK" -> NICK
  | "INDI" -> INDI
  | "GIVN" -> GIVN
  | "SEX" -> SEX
  | "SSN" -> SSN
  | "PROP" -> PROP
  | "OCCU" -> OCCU
  | "NMR" -> NMR
  | "NATI" -> NATI
  | "IDNO" -> IDNO
  | "EDUC" -> EDUC
  | "DSCR" -> DSCR
  | "CAST" -> CAST
  | "WILL" -> WILL
  | "RETI" -> RETI
  | "PROB" -> PROB
  | "ORDN" -> ORDN
  | "NATU" -> NATU
  | "IMMI" -> IMMI
  | "GRAD" -> GRAD
  | "FCOM" -> FCOM
  | "EMIG" -> EMIG
  | "DEAT" -> DEAT
  | "CREM" -> CREM
  | "CONF" -> CONF
  | "CHRA" -> CHRA
  | "BURI" -> BURI
  | "BLES" -> BLES
  | "BASM" -> BASM
  | "BARM" -> BARM
  | "BAPM" -> BAPM
  | "FAMC" -> FAMC
  | "CHR" -> CHR
  | "BIRT" -> BIRT
  | "ADOP" -> ADOP
  | "SLGC" -> SLGC
  | "INIL" -> INIL
  | "ENDL" -> ENDL
  | "CONL" -> CONL
  | "BAPL" -> BAPL
  | "PEDI" -> PEDI
  | "FAMS" -> FAMS
  | "ALIA" -> ALIA
  | "DESI" -> DESI
  | "ANCI" -> ANCI
  | "FILE" -> FILE
  | "MEDI" -> MEDI
  | "REPO" -> REPO
  | "PUBL" -> PUBL
  | "CALN" -> CALN
  | "AUTH" -> AUTH
  | "ABBR" -> ABBR
  | "TRLR" -> TRLR
  | s -> raise (Unkwown_tag_name s)

let to_string = function
  | VALUE s -> Format.sprintf "Value %s" s
  | XREF s_opt -> (
      match s_opt with None -> "@VOID@" | Some s -> Format.sprintf "XREF %s" s)
  | END -> "END"
  | HEAD -> "HEAD"
  | GEDC -> "GEDC"
  | VERS -> "VERS"
  | SCHMA -> "SCHMA"
  | TAG -> "TAG"
  | NAME -> "NAME"
  | SOUR -> "SOUR"
  | CORP -> "CORP"
  | ADDR -> "ADDR"
  | ADR1 -> "ADR1"
  | ADR2 -> "ADR2"
  | ADR3 -> "ADR3"
  | CITY -> "CITY"
  | STAE -> "STAE"
  | POST -> "POST"
  | CTRY -> "CTRY"
  | PHON -> "PHON"
  | EMAIL -> "EMAIL"
  | FAX -> "FAX"
  | WWW -> "WWW"
  | DATA -> "DATA"
  | DATE -> "DATE"
  | TIME -> "TIME"
  | COPR -> "COPR"
  | DEST -> "DEST"
  | LANG -> "LANG"
  | PLAC -> "PLAC"
  | FORM -> "FORM"
  | SUBM -> "SUBM"
  | NOTE -> "NOTE"
  | SNOTE -> "SNOTE"
  | TRAN -> "TRAN"
  | MIME -> "MIME"
  | TEXT -> "TEXT"
  | ROLE -> "ROLE"
  | QUAY -> "QUAY"
  | PHRASE -> "PHRASE"
  | PAGE -> "PAGE"
  | EVEN -> "EVEN"
  | WIDTH -> "WIDTH"
  | TOP -> "TOP"
  | TITL -> "TITL"
  | OBJE -> "OBJE"
  | LEFT -> "LEFT"
  | HEIGHT -> "HEIGHT"
  | CROP -> "CROP"
  | RESN -> "RESN"
  | FAM -> "FAM"
  | WIFE -> "WIFE"
  | UID -> "UID"
  | TYPE -> "TYPE"
  | SDATE -> "SDATE"
  | RESI -> "RESI"
  | RELI -> "RELI"
  | NCHI -> "NCHI"
  | MAP -> "MAP"
  | LONG -> "LONG"
  | LATI -> "LATI"
  | HUSB -> "HUSB"
  | FACT -> "FACT"
  | EXID -> "EXID"
  | CAUS -> "CAUS"
  | ASSO -> "ASSO"
  | AGNC -> "AGNC"
  | AGE -> "AGE"
  | MARS -> "MARS"
  | MARR -> "MARR"
  | MARL -> "MARL"
  | MARC -> "MARC"
  | MARB -> "MARB"
  | ENGA -> "ENGA"
  | DIVF -> "DIVF"
  | DIV -> "DIV"
  | CENS -> "CENS"
  | ANUL -> "ANUL"
  | NO -> "NO"
  | CHIL -> "CHIL"
  | TEMP -> "TEMP"
  | STAT -> "STAT"
  | SLGS -> "SLGS"
  | REFN -> "REFN"
  | CHAN -> "CHAN"
  | CREA -> "CREA"
  | SURN -> "SURN"
  | SPFX -> "SPFX"
  | NSFX -> "NSFX"
  | NPFX -> "NPFX"
  | NICK -> "NICK"
  | INDI -> "INDI"
  | GIVN -> "GIVN"
  | SEX -> "SEX"
  | SSN -> "SSN"
  | PROP -> "PROP"
  | OCCU -> "OCCU"
  | NMR -> "NMR"
  | NATI -> "NATI"
  | IDNO -> "IDNO"
  | EDUC -> "EDUC"
  | DSCR -> "DSCR"
  | CAST -> "CAST"
  | WILL -> "WILL"
  | RETI -> "RETI"
  | PROB -> "PROB"
  | ORDN -> "ORDN"
  | NATU -> "NATU"
  | IMMI -> "IMMI"
  | GRAD -> "GRAD"
  | FCOM -> "FCOM"
  | EMIG -> "EMIG"
  | DEAT -> "DEAT"
  | CREM -> "CREM"
  | CONF -> "CONF"
  | CHRA -> "CHRA"
  | BURI -> "BURI"
  | BLES -> "BLES"
  | BASM -> "BASM"
  | BARM -> "BARM"
  | BAPM -> "BAPM"
  | FAMC -> "FAMC"
  | CHR -> "CHR"
  | BIRT -> "BIRT"
  | ADOP -> "ADOP"
  | SLGC -> "SLGC"
  | INIL -> "INIL"
  | ENDL -> "ENDL"
  | CONL -> "CONL"
  | BAPL -> "BAPL"
  | PEDI -> "PEDI"
  | FAMS -> "FAMS"
  | ALIA -> "ALIA"
  | DESI -> "DESI"
  | ANCI -> "ANCI"
  | FILE -> "FILE"
  | MEDI -> "MEDI"
  | REPO -> "REPO"
  | PUBL -> "PUBL"
  | CALN -> "CALN"
  | AUTH -> "AUTH"
  | ABBR -> "ABBR"
  | TRLR -> "TRLR"
  | EOF -> "EOF"

type t = Parser.token

let print tok = Format.printf "%s@." (to_string tok)