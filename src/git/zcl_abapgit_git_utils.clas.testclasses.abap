
CLASS ltcl_length_utf8 DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_git_utils.

    METHODS:
      setup,
      length_utf8_hex FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.


CLASS ltcl_length_utf8 IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD length_utf8_hex.

    DATA lv_result TYPE i.

    lv_result = mo_cut->length_utf8_hex( '30303334' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 52 ).

  ENDMETHOD.

ENDCLASS.
