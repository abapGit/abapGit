CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS dir_encode FOR TESTING.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD dir_encode.

    DATA lv_encoded TYPE string.

    lv_encoded = zcl_abapgit_html_action_utils=>dir_encode( '/hello.,()[]' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_encoded
      exp = 'PATH=%2fhello.%2c()%5b%5d' ).

  ENDMETHOD.

ENDCLASS.
