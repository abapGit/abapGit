CLASS ltcl_git_utils DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_git_utils.

    METHODS:
      setup,
      get_null FOR TESTING,
      pkt_string FOR TESTING RAISING zcx_abapgit_exception,
      pkt_string_utf8 FOR TESTING RAISING zcx_abapgit_exception,
      pkt_string_long FOR TESTING RAISING zcx_abapgit_exception,
      pkt_string_too_long FOR TESTING,
      length_utf8_hex FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_git_utils IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD get_null.

    CONSTANTS lc_null TYPE x LENGTH 2 VALUE '0000'.

    DATA lv_c TYPE c LENGTH 1.

    FIELD-SYMBOLS <lv_x> TYPE x.

    lv_c = mo_cut->get_null( ).

    ASSIGN lv_c TO <lv_x> CASTING.

    cl_abap_unit_assert=>assert_equals(
      act = <lv_x>
      exp = lc_null ).

  ENDMETHOD.

  METHOD pkt_string.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->pkt_string( 'test' )
      exp = '0008test' ).

  ENDMETHOD.

  METHOD pkt_string_utf8.

    " the length counts bytes as sent (UTF-8), not characters: "test-" plus a-umlaut is 7 bytes
    DATA lv_line TYPE string.

    lv_line = zcl_abapgit_convert=>xstring_to_string_utf8( '746573742DC3A4' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->pkt_string( lv_line )
      exp = |000B{ lv_line }| ).

  ENDMETHOD.

  METHOD pkt_string_long.

    " Git allows up to 65516 bytes of data in a line, not only 250
    DATA lv_line TYPE string.

    lv_line = repeat( val = 'a'
                      occ = 300 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->pkt_string( lv_line )
      exp = |0130{ lv_line }| ).

    lv_line = repeat( val = 'a'
                      occ = 65516 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->pkt_string( lv_line )
      exp = |FFF0{ lv_line }| ).

  ENDMETHOD.

  METHOD pkt_string_too_long.

    DATA lv_line TYPE string.

    lv_line = repeat( val = 'a'
                      occ = 65517 ).

    TRY.
        mo_cut->pkt_string( lv_line ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.

  METHOD length_utf8_hex.

    DATA lv_result TYPE i.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->length_utf8_hex( '30303030' )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->length_utf8_hex( '30303334' )
      exp = 52 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->length_utf8_hex( '303061354D617263' )
      exp = 165 ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->length_utf8_hex( '66666666' )
      exp = 65535 ).

    " too short
    TRY.
        lv_result = mo_cut->length_utf8_hex( '00' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
