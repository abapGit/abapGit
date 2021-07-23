CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS test01 FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test01.

    DATA lv_bar TYPE i.

    lv_bar = 2.

    cl_abap_unit_assert=>assert_equals(
      act = lv_bar
      exp = 2 ).

  ENDMETHOD.

ENDCLASS.