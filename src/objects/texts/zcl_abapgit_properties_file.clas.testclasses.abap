CLASS lcl_test DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS assert_lang FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS lcl_test IMPLEMENTATION.

  METHOD assert_lang.

    DATA: lo_cut TYPE REF TO zif_abapgit_i18n_file.
    DATA: lv_act TYPE laiso.
    lo_cut = NEW zcl_abapgit_properties_file( iv_lang = 'DE' ).
    lv_act = lo_cut->lang( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 'DE'
      act = lv_act ).
  ENDMETHOD.

ENDCLASS.
