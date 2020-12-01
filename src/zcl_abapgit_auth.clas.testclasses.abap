
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS default_is_allow FOR TESTING.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD default_is_allow.

    DATA lv_allowed TYPE abap_bool.

    lv_allowed = zcl_abapgit_auth=>is_allowed( 'DUMMY_UNIT_TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_allowed
      exp = abap_true ).

  ENDMETHOD.

ENDCLASS.
