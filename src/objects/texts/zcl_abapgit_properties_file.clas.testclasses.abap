CLASS ltcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS assert_lang FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD assert_lang.

    DATA: lo_cut TYPE REF TO zcl_abapgit_properties_file.
    DATA: lv_act TYPE laiso.
    CREATE OBJECT lo_cut
      EXPORTING
        iv_lang = 'DE'.
    lv_act = lo_cut->zif_abapgit_i18n_file~lang( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'DE'
      act = lv_act ).
  ENDMETHOD.

ENDCLASS.
