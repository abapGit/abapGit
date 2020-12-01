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

    DATA: ls_files_item TYPE zcl_abapgit_objects=>ty_serialization.

    ls_files_item = zcl_abapgit_objects=>serialize( is_item     = is_item
                                                    iv_language = zif_abapgit_definitions=>c_english ).

    cl_abap_unit_assert=>assert_not_initial( ls_files_item-files ).
    cl_abap_unit_assert=>assert_equals( act = ls_files_item-item
                                        exp = is_item ).

  ENDMETHOD.
ENDCLASS.
