INTERFACE zif_abapgit_lang_definitions
  PUBLIC .

  TYPES: BEGIN OF ty_i18n_tpool,
           language TYPE langu,
           textpool TYPE zif_abapgit_definitions=>ty_tpool_tt,
         END OF ty_i18n_tpool,
         tt_i18n_tpool TYPE STANDARD TABLE OF ty_i18n_tpool.

  TYPES: BEGIN OF ty_i18n_lines,
           language TYPE langu,
           lines    TYPE tlinetab,
         END OF ty_i18n_lines,
         tt_i18n_lines TYPE STANDARD TABLE OF ty_i18n_lines.

  TYPES: tt_langu TYPE STANDARD TABLE OF langu.

ENDINTERFACE.
