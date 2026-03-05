CLASS ltcl_user_record DEFINITION DEFERRED.

CLASS zcl_abapgit_user_record DEFINITION LOCAL FRIENDS ltcl_user_record.

CLASS ltcl_user_record DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS c_wrong_user TYPE sy-uname VALUE 'WRONG_USER'.
    METHODS:
      test_invalid_user FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_user_record IMPLEMENTATION.

  METHOD test_invalid_user.
    DATA li_user_record TYPE REF TO zif_abapgit_user_record.

    zcl_abapgit_user_record=>reset( ).
    li_user_record = zcl_abapgit_env_factory=>get_user_record( ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( zcl_abapgit_user_record=>gt_user )
      msg = |User { c_wrong_user } is missing in the list| ).
  ENDMETHOD.

ENDCLASS.
