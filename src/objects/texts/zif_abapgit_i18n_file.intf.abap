INTERFACE zif_abapgit_i18n_file PUBLIC.

  TYPES ty_table_of TYPE STANDARD TABLE OF REF TO zif_abapgit_i18n_file WITH DEFAULT KEY.

  METHODS render
    RETURNING
      VALUE(rv_data) TYPE xstring
    RAISING
      zcx_abapgit_exception.

  METHODS translate
    CHANGING
      cv_changed    TYPE abap_bool
      ct_text_pairs TYPE zif_abapgit_lxe_texts=>ty_text_pairs
    RAISING
      zcx_abapgit_exception.

  METHODS ext
    RETURNING
      VALUE(rv_ext) TYPE string.

  METHODS lang
    RETURNING
      VALUE(rv_lang) TYPE laiso.

  METHODS lang_suffix
    RETURNING
      VALUE(rv_lang_suffix) TYPE string.

ENDINTERFACE.
