CLASS zcl_abapgit_gui_url_param_repo DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abapgit_ui_factory .

  PUBLIC SECTION.
    METHODS:
      constructor,

      encode
        IMPORTING
          iv_text          TYPE string
        RETURNING
          VALUE(rv_result) TYPE string,

      decode
        IMPORTING
          iv_text          TYPE string
        RETURNING
          VALUE(rv_result) TYPE string.

  PRIVATE SECTION.
    DATA:
      mo_url_map TYPE REF TO zcl_abapgit_string_map.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_URL_PARAM_REPO IMPLEMENTATION.


  METHOD constructor.

    CREATE OBJECT mo_url_map.

  ENDMETHOD.


  METHOD decode.

    rv_result = mo_url_map->get( iv_text ).

    IF rv_result IS INITIAL.
      rv_result = iv_text.
    ENDIF.

  ENDMETHOD.


  METHOD encode.

    " We need uppercase short strings because current edge control sends data case insensitive.
    " Chance of collisions is low.
    TRY.
        rv_result = to_upper( zcl_abapgit_hash=>sha1_string( iv_text ) ).

        mo_url_map->set(
            iv_key = rv_result
            iv_val = iv_text ).

      CATCH zcx_abapgit_exception.
        " Can only happen if string_map is read-only or hash calc doesn't work.
        " We're in trouble then anyway.
        ASSERT 1 = 2.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
