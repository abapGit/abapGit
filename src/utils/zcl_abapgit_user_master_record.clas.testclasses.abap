*"* use this source file for your ABAP unit test classes
CLASS ltcl_user_master_record DEFINITION DEFERRED.
CLASS zcl_abapgit_user_master_record DEFINITION LOCAL FRIENDS ltcl_user_master_record.

CLASS ltcl_user_master_record DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS gc_wrong_user TYPE uname VALUE 'NONE' ##NO_TEXT.
    METHODS:
      test_invalid_user FOR TESTING RAISING cx_static_check,
      test_valid_user FOR TESTING RAISING cx_static_check
      .
ENDCLASS.


CLASS ltcl_user_master_record IMPLEMENTATION.

  METHOD test_invalid_user.
    DATA:user_master_record TYPE REF TO zcl_abapgit_user_master_record.
*Given When invalid user is entered
    user_master_record = zcl_abapgit_user_master_record=>get_instance( gc_wrong_user ).
*    Then the list should be empty
    cl_abap_unit_assert=>assert_true( xsdbool( lines( user_master_record->gt_user ) = 0 ) ).


*    cl_abap_unit_assert=>fail( 'Implement your first test here' ).


  ENDMETHOD.

  METHOD test_valid_user.
    DATA:user_master_record TYPE REF TO zcl_abapgit_user_master_record.
*Given When invalid user is entered
    user_master_record = zcl_abapgit_user_master_record=>get_instance( 'DEVELOPER' ).
*    Then the list should be empty
    cl_abap_unit_assert=>assert_true( xsdbool( lines( user_master_record->gt_user ) = 1 ) ).


*   cl_abap_unit_assert=>fail( 'Implement your first test here' ).

  ENDMETHOD.

ENDCLASS.
