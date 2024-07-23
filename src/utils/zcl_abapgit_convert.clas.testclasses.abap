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
    METHODS xstring_to_bintab_with_field FOR TESTING.

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

  METHOD xstring_to_bintab_with_field.

    DATA lt_bintab TYPE TABLE OF w3mime. " contains one field named 'LINE'
    DATA lv_size TYPE i.

    zcl_abapgit_convert=>xstring_to_bintab(
      EXPORTING
        iv_xstr   = '1122334455'
      IMPORTING
        ev_size   = lv_size
        et_bintab = lt_bintab ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_size
      exp = 5 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_bintab )
      exp = 1 ).

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

CLASS ltcl_bcp47_to_sap1 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS assert_bcp47_to_sap1
      IMPORTING
        im_from TYPE string
        im_to   TYPE sy-langu.

    METHODS assert_bcp47_to_sap1_fail
      IMPORTING
        im_from TYPE string.

    METHODS english FOR TESTING.
    METHODS english_us FOR TESTING.
    METHODS english_gb FOR TESTING.
    METHODS german FOR TESTING.
    METHODS german_germany FOR TESTING.
    METHODS german_swiss FOR TESTING.
    METHODS chinese FOR TESTING.
    METHODS chinese_singapore FOR TESTING.
    METHODS chinese_taiwan FOR TESTING.
    METHODS no_value FOR TESTING.
    METHODS unsupported FOR TESTING.
    METHODS unsupported_uppercase FOR TESTING.
    METHODS too_many_characters FOR TESTING.
    METHODS too_less_characters FOR TESTING.
    METHODS english_uppercase FOR TESTING.
    METHODS chinese_uppercase FOR TESTING.

ENDCLASS.

CLASS ltcl_bcp47_to_sap1 IMPLEMENTATION.

  METHOD assert_bcp47_to_sap1.
    DATA lv_result TYPE sy-langu.
    lv_result = zcl_abapgit_convert=>language_bcp47_to_sap1( im_from ).

    cl_abap_unit_assert=>assert_equals( exp = im_to
                                        act = lv_result ).
  ENDMETHOD.

  METHOD assert_bcp47_to_sap1_fail.
    DATA lv_result TYPE string.

    zcl_abapgit_convert=>language_bcp47_to_sap1(
      EXPORTING
        im_lang_bcp47 = im_from
      RECEIVING
        re_lang_sap1  = lv_result
      EXCEPTIONS
        no_assignment = 1
        OTHERS = 2 ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = sy-subrc ).
  ENDMETHOD.

  METHOD english.
    assert_bcp47_to_sap1( im_from = 'en'
                          im_to = 'E' ).
  ENDMETHOD.

  METHOD english_uppercase.
    assert_bcp47_to_sap1( im_from = 'EN'
                          im_to = 'E' ).
  ENDMETHOD.

  METHOD english_us.
    assert_bcp47_to_sap1( im_from = 'en-US'
                          im_to = 'E' ).
  ENDMETHOD.

  METHOD english_gb.
    DATA lv_to TYPE sy-langu.
    lv_to = zcl_abapgit_convert=>uccp( 'B46E' ).

    assert_bcp47_to_sap1( im_from = 'en-GB'
                          im_to = lv_to ).
  ENDMETHOD.

  METHOD german.
    assert_bcp47_to_sap1( im_from = 'de'
                          im_to = 'D' ).
  ENDMETHOD.

  METHOD german_germany.
    assert_bcp47_to_sap1( im_from = 'de-DE'
                          im_to = 'D' ).
  ENDMETHOD.

  METHOD german_swiss.
    DATA lv_to TYPE sy-langu.
    lv_to = zcl_abapgit_convert=>uccp( 'B3A7' ).

    assert_bcp47_to_sap1( im_from = 'de-CH'
                          im_to = lv_to ).
  ENDMETHOD.

  METHOD chinese.
    assert_bcp47_to_sap1( im_from = 'zh'
                          im_to = '1' ).
  ENDMETHOD.

  METHOD chinese_uppercase.
    assert_bcp47_to_sap1( im_from = 'ZH'
                          im_to = '1' ).
  ENDMETHOD.

  METHOD chinese_singapore.
    DATA lv_to TYPE sy-langu.
    lv_to = zcl_abapgit_convert=>uccp( 'B343' ).

    assert_bcp47_to_sap1( im_from = 'zh-SG'
                          im_to = lv_to ).
  ENDMETHOD.

  METHOD chinese_taiwan.
    assert_bcp47_to_sap1( im_from = 'zh-Hant'
                          im_to = 'M' ).
  ENDMETHOD.

  METHOD no_value.
    assert_bcp47_to_sap1_fail( '' ).
  ENDMETHOD.

  METHOD unsupported.
    assert_bcp47_to_sap1_fail( 'xx' ).
  ENDMETHOD.

  METHOD unsupported_uppercase.
    assert_bcp47_to_sap1_fail( 'XX' ).
  ENDMETHOD.

  METHOD too_many_characters.
    assert_bcp47_to_sap1_fail( 'eng' ).
  ENDMETHOD.

  METHOD too_less_characters.
    assert_bcp47_to_sap1_fail( 'e' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_sap1_to_bcp47 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS assert_sap1_to_bcp47
      IMPORTING
        im_from TYPE sy-langu
        im_to   TYPE string.

    METHODS assert_sap1_to_bcp47_fail
      IMPORTING
        im_from TYPE sy-langu.

    METHODS english FOR TESTING.
    METHODS english_gb FOR TESTING.
    METHODS chinese FOR TESTING.
    METHODS chinese_singapore FOR TESTING.
    METHODS chinese_taiwan FOR TESTING.
    METHODS unsupported FOR TESTING.
    METHODS no_value FOR TESTING.
    METHODS empty_value FOR TESTING.

ENDCLASS.

CLASS ltcl_sap1_to_bcp47 IMPLEMENTATION.

  METHOD assert_sap1_to_bcp47.
    DATA lv_result TYPE string.
    lv_result = zcl_abapgit_convert=>language_sap1_to_bcp47( im_from ).

    cl_abap_unit_assert=>assert_equals( exp = im_to
                                        act = lv_result ).
  ENDMETHOD.

  METHOD assert_sap1_to_bcp47_fail.
    DATA lv_result TYPE string.

    zcl_abapgit_convert=>language_sap1_to_bcp47(
      EXPORTING
        im_lang_sap1  = im_from
      RECEIVING
        re_lang_bcp47 = lv_result
      EXCEPTIONS
        no_assignment = 1
        OTHERS = 2 ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = sy-subrc ).
  ENDMETHOD.

  METHOD english.
    assert_sap1_to_bcp47( im_from = 'E'
                          im_to = 'en' ).
  ENDMETHOD.

  METHOD english_gb.
    DATA lv_from TYPE sy-langu.
    lv_from = zcl_abapgit_convert=>uccp( 'B46E' ).

    assert_sap1_to_bcp47( im_from = lv_from
                          im_to = 'en-GB' ).
  ENDMETHOD.

  METHOD chinese.
    assert_sap1_to_bcp47( im_from = '1'
                          im_to = 'zh' ).
  ENDMETHOD.

  METHOD chinese_singapore.
    DATA lv_from TYPE sy-langu.
    lv_from = zcl_abapgit_convert=>uccp( 'B343' ).

    assert_sap1_to_bcp47( im_from = lv_from
                          im_to = 'zh-SG' ).
  ENDMETHOD.

  METHOD chinese_taiwan.
    assert_sap1_to_bcp47( im_from = 'M'
                          im_to = 'zh-Hant' ).
  ENDMETHOD.

  METHOD unsupported.
    assert_sap1_to_bcp47_fail( 'x' ).
  ENDMETHOD.

  METHOD no_value.
    assert_sap1_to_bcp47_fail( ' ' ).
  ENDMETHOD.

  METHOD empty_value.
    assert_sap1_to_bcp47_fail( '' ).
  ENDMETHOD.

ENDCLASS.
