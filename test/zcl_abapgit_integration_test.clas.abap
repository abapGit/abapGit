CLASS zcl_abapgit_integration_test DEFINITION PUBLIC FINAL CREATE PUBLIC
  FOR TESTING DURATION LONG RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS test FOR TESTING.

ENDCLASS.

CLASS zcl_abapgit_integration_test IMPLEMENTATION.

  METHOD test.

    DATA lv_bar TYPE i.

    lv_bar = 2.

    cl_abap_unit_assert=>assert_equals(
      act = lv_bar
      exp = 2 ).

  ENDMETHOD.
  
ENDCLASS.