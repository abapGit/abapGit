*"* use this source file for your ABAP unit test classes
CLASS ltcl_user_master_record DEFINITION DEFERRED.
CLASS zcl_abapgit_user_master_record DEFINITION LOCAL FRIENDS ltcl_user_master_record.

CLASS ltcl_user_master_record DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS gc_wrong_user TYPE uname VALUE 'NONE' ##NO_TEXT.
    METHODS:
      test_invalid_user FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_user_master_record IMPLEMENTATION.

  METHOD test_invalid_user.
    DATA: lo_user_master_record TYPE REF TO zcl_abapgit_user_master_record.
*Given When invalid user is entered
    lo_user_master_record = zcl_abapgit_user_master_record=>get_instance( gc_wrong_user ).
*    Then the list should be empty
    cl_abap_unit_assert=>assert_true(
              act = xsdbool( lines( lo_user_master_record->gt_user ) = 0 )
              ).
  ENDMETHOD.



ENDCLASS.
