CLASS zcl_abapgit_test_serialize DEFINITION
  PUBLIC
  CREATE PUBLIC
  FOR TESTING .

  PUBLIC SECTION.

    CLASS-METHODS check
      IMPORTING
        !is_item TYPE zif_abapgit_definitions=>ty_item
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_TEST_SERIALIZE IMPLEMENTATION.


  METHOD check.

    DATA: lt_files TYPE zif_abapgit_definitions=>ty_files_tt.

    lt_files = zcl_abapgit_objects=>serialize(
      is_item     = is_item
      iv_language = zif_abapgit_definitions=>c_english ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.
ENDCLASS.
