CLASS zcl_abapgit_git_utils DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_null TYPE c LENGTH 1 .

    CLASS-METHODS get_null
      RETURNING
        VALUE(rv_c) TYPE ty_null .
    CLASS-METHODS pkt_string
      IMPORTING
        !iv_string    TYPE string
      RETURNING
        VALUE(rv_pkt) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS length_utf8_hex
      IMPORTING
        !iv_data      TYPE xstring
      RETURNING
        VALUE(rv_len) TYPE i
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_git_utils IMPLEMENTATION.


  METHOD get_null.

* must be length 4, or it gives a syntax error on lower versions
    DATA: lv_x TYPE x LENGTH 4 VALUE '00000000'.
    FIELD-SYMBOLS <lv_y> TYPE c.

    ASSIGN lv_x TO <lv_y> CASTING.
    rv_c = <lv_y>.

  ENDMETHOD.


  METHOD length_utf8_hex.

    DATA: lv_xstring TYPE xstring,
          lv_char4   TYPE c LENGTH 4,
          lv_x       TYPE x LENGTH 2.

    IF xstrlen( iv_data ) < 4.
      zcx_abapgit_exception=>raise( 'error converting to hex, LENGTH_UTF8_HEX' ).
    ENDIF.

    lv_xstring = iv_data(4).

    lv_char4 = zcl_abapgit_convert=>xstring_to_string_utf8_raw(
      iv_data   = lv_xstring
      iv_length = 4 ).

    TRANSLATE lv_char4 TO UPPER CASE.
    lv_x = lv_char4.
    rv_len = lv_x.

  ENDMETHOD.


  METHOD pkt_string.

    " the length prefix counts the bytes sent, which are UTF-8, plus its own 4
    CONSTANTS lc_max_data TYPE i VALUE 65516.

    DATA: lv_x   TYPE x LENGTH 2,
          lv_len TYPE i.


    lv_len = xstrlen( zcl_abapgit_convert=>string_to_xstring_utf8( iv_string ) ).

    IF lv_len > lc_max_data.
      zcx_abapgit_exception=>raise( |PKT, line too long ({ lv_len } bytes)| ).
    ENDIF.

    lv_x = lv_len + 4.

    rv_pkt = lv_x && iv_string.

  ENDMETHOD.
ENDCLASS.
