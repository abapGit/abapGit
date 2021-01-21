INTERFACE zif_abapgit_lxe_texts
  PUBLIC .
  TYPES:
    ty_languages TYPE STANDARD TABLE OF langu WITH DEFAULT KEY,
    BEGIN OF ty_lxe_i18n,
      source_lang TYPE lxeisolang,
      target_lang TYPE lxeisolang,
      custmnr     TYPE lxecustmnr,
      objtype     TYPE trobjtype,
      objname     TYPE lxeobjname,
      text_pairs  TYPE lxe_tt_pcx_s1,
    END OF ty_lxe_i18n,
    ty_tlxe_i18n TYPE STANDARD TABLE OF ty_lxe_i18n WITH DEFAULT KEY.

ENDINTERFACE.
