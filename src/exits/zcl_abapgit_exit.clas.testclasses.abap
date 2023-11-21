*"* use this source file for your ABAP unit test classes

CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_abapgit_exit DEFINITION LOCAL FRIENDS ltcl_test.

" The class name ltcl_test is hardcoded in zcl_abapgit_exit=>is_running_in_test_context

CLASS ltcl_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS:
      is_running_in_test_context FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD is_running_in_test_context.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_exit=>is_running_in_test_context( )
      exp = abap_true ).
  ENDMETHOD.
ENDCLASS.
