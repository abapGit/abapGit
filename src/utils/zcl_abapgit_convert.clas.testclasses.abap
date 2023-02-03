*----------------------------------------------------------------------*
*       CLASS ltcl_convert DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_convert DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS convert_int FOR TESTING RAISING zcx_abapgit_exception.
    METHODS split_string FOR TESTING.
    METHODS convert_bitbyte FOR TESTING RAISING zcx_abapgit_exception.
    METHODS string_to_xstring_utf8 FOR TESTING RAISING zcx_abapgit_exception.
    METHODS string_to_xstring_utf8_bom FOR TESTING RAISING zcx_abapgit_exception.
    METHODS xstring_to_string_utf8 FOR TESTING RAISING zcx_abapgit_exception.
    METHODS xstring_to_string_not_utf8 FOR TESTING RAISING zcx_abapgit_exception.
    METHODS base64_to_xstring FOR TESTING.
    METHODS conversion_exit_isola_output FOR TESTING.
    METHODS string_to_tab FOR TESTING.
    METHODS string_to_xstring FOR TESTING RAISING zcx_abapgit_exception.
    METHODS xstring_to_bintab FOR TESTING.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_convert IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_convert IMPLEMENTATION.

  METHOD xstring_to_bintab.

    TYPES ty_hex TYPE x LENGTH 2.
    DATA lt_bintab TYPE STANDARD TABLE OF ty_hex WITH DEFAULT KEY.
    DATA lv_size TYPE i.

    zcl_abapgit_convert=>xstring_to_bintab(
      EXPORTING
        iv_xstr   = '112233'
      IMPORTING
        ev_size   = lv_size
        et_bintab = lt_bintab ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_size
      exp = 3 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_bintab )
      exp = 2 ).

  ENDMETHOD.

  METHOD string_to_xstring.

    DATA lv_xstr TYPE xstring.

    lv_xstr = zcl_abapgit_convert=>string_to_xstring( 'hello world' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_xstr
      exp = '68656C6C6F20776F726C64' ).

  ENDMETHOD.

  METHOD string_to_tab.

    TYPES ty_char TYPE c LENGTH 2.
    DATA lv_size TYPE i.
    DATA lt_tab TYPE STANDARD TABLE OF ty_char WITH DEFAULT KEY.

    zcl_abapgit_convert=>string_to_tab(
      EXPORTING
        iv_str  = 'hello world'
      IMPORTING
        ev_size = lv_size
        et_tab  = lt_tab ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_size
      exp = 11 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_tab )
      exp = 6 ).

  ENDMETHOD.

  METHOD conversion_exit_isola_output.

    DATA lv_laiso TYPE laiso.

    lv_laiso = zcl_abapgit_convert=>conversion_exit_isola_output( 'E' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_laiso
      exp = 'EN' ).

  ENDMETHOD.

  METHOD base64_to_xstring.

    DATA lv_result TYPE xstring.

    lv_result = zcl_abapgit_convert=>base64_to_xstring( 'YWJhcA==' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = '61626170' ).

  ENDMETHOD.

  METHOD string_to_xstring_utf8.

    DATA lv_result TYPE xstring.

    lv_result = zcl_abapgit_convert=>string_to_xstring_utf8( `` ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = `` ).

    lv_result = zcl_abapgit_convert=>string_to_xstring_utf8( 'abc' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = '616263' ).

  ENDMETHOD.

  METHOD string_to_xstring_utf8_bom.

    DATA lv_result TYPE xstring.

    lv_result = zcl_abapgit_convert=>string_to_xstring_utf8_bom( `` ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = `` ).

    lv_result = zcl_abapgit_convert=>string_to_xstring_utf8_bom( 'a' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'EFBBBF61' ).

    lv_result = zcl_abapgit_convert=>string_to_xstring_utf8_bom( 'abcd' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'EFBBBF61626364' ).

  ENDMETHOD.

  METHOD xstring_to_string_utf8.

    DATA lv_result TYPE string.

    lv_result = zcl_abapgit_convert=>xstring_to_string_utf8( `` ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = `` ).

    lv_result = zcl_abapgit_convert=>xstring_to_string_utf8( '616263' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'abc' ).

  ENDMETHOD.

  METHOD xstring_to_string_not_utf8.

    DATA lv_result TYPE string.

    " 0xF8-0xFF are not valid in UTF-8
    TRY.
        lv_result = zcl_abapgit_convert=>xstring_to_string_utf8( 'F8FF00' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD convert_bitbyte.

    DATA: lv_xstring  TYPE xstring,
          lv_byte     TYPE x,
          lv_input    TYPE i,
          lv_bitbyte  TYPE zif_abapgit_git_definitions=>ty_bitbyte,
          lv_byteint  TYPE i,
          lv_xbyteint TYPE xstring,
          lv_xresult  TYPE xstring,
          lv_result   TYPE i,
          lv_offset   TYPE i.

    DO 1000 TIMES.

      lv_result = 0.
      CLEAR: lv_byteint, lv_xbyteint, lv_xresult.

      lv_input  = sy-index * 64.
      lv_xstring = zcl_abapgit_convert=>int_to_xstring4( lv_input ).
      DO 4 TIMES.
        lv_offset = sy-index - 1.
        lv_byte = lv_xstring+lv_offset(1).
        lv_bitbyte = zcl_abapgit_convert=>x_to_bitbyte( lv_byte ).
        lv_byteint = zcl_abapgit_convert=>bitbyte_to_int( lv_bitbyte ).
        lv_xbyteint = lv_byteint.
        CONCATENATE lv_xresult lv_xbyteint INTO lv_xresult
          IN BYTE MODE.
      ENDDO.
      lv_result = zcl_abapgit_convert=>xstring_to_int( lv_xresult ).

      cl_abap_unit_assert=>assert_equals(
          exp = lv_input
          act = lv_result ).

    ENDDO.

  ENDMETHOD.

  METHOD convert_int.

    DATA: lv_xstring TYPE xstring,
          lv_input   TYPE i,
          lv_result  TYPE i.


    DO 1000 TIMES.
      lv_input = sy-index.
      lv_xstring = zcl_abapgit_convert=>int_to_xstring4( lv_input ).
      lv_result = zcl_abapgit_convert=>xstring_to_int( lv_xstring ).

      cl_abap_unit_assert=>assert_equals(
          exp = lv_input
          act = lv_result ).
    ENDDO.

  ENDMETHOD.

  METHOD split_string.

    DATA: lt_act TYPE string_table,
          lt_exp TYPE string_table.

    APPEND 'ABC' TO lt_exp.
    APPEND '123' TO lt_exp.

    " Case 1: String separated by CRLF
    lt_act = zcl_abapgit_convert=>split_string( 'ABC' && cl_abap_char_utilities=>cr_lf && '123' ).

    cl_abap_unit_assert=>assert_equals( exp = lt_exp
                                        act = lt_act
                                        msg = 'Error during string split: CRLF' ).

    CLEAR: lt_act.

    " Case 2: String separated by LF
    lt_act = zcl_abapgit_convert=>split_string( 'ABC' && cl_abap_char_utilities=>newline && '123' ).

    cl_abap_unit_assert=>assert_equals( exp = lt_exp
                                        act = lt_act
                                        msg = 'Error during string split: LF' ).

  ENDMETHOD.

ENDCLASS.
