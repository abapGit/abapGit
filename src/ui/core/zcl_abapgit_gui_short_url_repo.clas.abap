CLASS zcl_abapgit_gui_short_url_repo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor,

      conv_long_to_short_url
        IMPORTING
          iv_url              TYPE string
        RETURNING
          VALUE(rv_short_url) TYPE string,

      conv_short_to_long_url
        IMPORTING
          iv_short_url  TYPE string
        RETURNING
          VALUE(rv_url) TYPE string.

  PRIVATE SECTION.
    DATA:
      mo_url_map TYPE REF TO zcl_abapgit_string_map.

ENDCLASS.



CLASS zcl_abapgit_gui_short_url_repo IMPLEMENTATION.

  METHOD constructor.

    CREATE OBJECT mo_url_map.

  ENDMETHOD.


  METHOD conv_long_to_short_url.

    " We need uppercase short urls because current edge control sends data case insensitive.
    " Chance of collisions is low.
    TRY.
        rv_short_url = to_upper( zcl_abapgit_hash=>sha1_string( iv_url ) ).

        mo_url_map->set(
            iv_key = rv_short_url
            iv_val = iv_url ).

      CATCH zcx_abapgit_exception.
        " Can only happen if string_map is read-only or hash calc doesn't work.
        " We're in trouble then anyway.
        ASSERT 1 = 2.
    ENDTRY.

  ENDMETHOD.


  METHOD conv_short_to_long_url.

    rv_url = mo_url_map->get( iv_short_url ).

  ENDMETHOD.

ENDCLASS.
