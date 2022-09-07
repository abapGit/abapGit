CLASS ltcl_git_utils DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_git_utils.

    METHODS:
      setup,
      get_null FOR TESTING,
      pkt_string FOR TESTING RAISING zcx_abapgit_exception,
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
