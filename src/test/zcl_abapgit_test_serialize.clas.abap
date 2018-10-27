CLASS zcl_abapgit_test_serialize DEFINITION
  PUBLIC
  CREATE PUBLIC
  FOR TESTING .

  PUBLIC SECTION.

    CLASS-METHODS check
      IMPORTING VALUE(is_item) TYPE zif_abapgit_definitions=>ty_item
      RAISING
                zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_test_serialize IMPLEMENTATION.


  METHOD check.

    DATA: lt_files TYPE zif_abapgit_definitions=>ty_files_tt.

    zcl_abapgit_objects=>serialize(
        EXPORTING iv_language = zif_abapgit_definitions=>c_english
        IMPORTING et_files    = lt_files
        CHANGING  cs_item     = is_item ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.
ENDCLASS.
