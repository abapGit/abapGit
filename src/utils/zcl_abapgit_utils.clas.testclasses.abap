CLASS ltcl_utils_test DEFINITION FINAL
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS is_binary FOR TESTING.

ENDCLASS.

CLASS ltcl_utils_test IMPLEMENTATION.

  METHOD is_binary.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_utils=>is_binary( '616263' ) " abc
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_utils=>is_binary( '010203' )
      exp = abap_true ).

  ENDMETHOD.

ENDCLASS.
