INTERFACE zif_abapgit_lang_definitions
  PUBLIC .

  TYPES: BEGIN OF ty_i18n_tpool,
           language TYPE langu,
           textpool TYPE zif_abapgit_definitions=>ty_tpool_tt,
         END OF ty_i18n_tpool,
         ty_i18n_tpools TYPE STANDARD TABLE OF ty_i18n_tpool.

  TYPES: BEGIN OF ty_i18n_line,
           language TYPE langu,
           lines    TYPE tlinetab,
         END OF ty_i18n_line,
         ty_i18n_lines TYPE STANDARD TABLE OF ty_i18n_line WITH KEY language.

  TYPES: ty_langus TYPE STANDARD TABLE OF langu.

ENDINTERFACE.
