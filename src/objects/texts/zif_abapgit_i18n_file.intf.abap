INTERFACE zif_abapgit_i18n_file
  PUBLIC .

  TYPES: ty_table_of TYPE STANDARD TABLE OF REF TO zif_abapgit_i18n_file WITH DEFAULT KEY.

  METHODS render
    RETURNING
      VALUE(rv_data) TYPE xstring
    RAISING
      zcx_abapgit_exception.

  METHODS ext
    RETURNING
      VALUE(rv_ext) TYPE string.
  METHODS lang
    RETURNING
      VALUE(rv_lang) TYPE string.

ENDINTERFACE.
