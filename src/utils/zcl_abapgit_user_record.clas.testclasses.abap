CLASS ltcl_user_record DEFINITION DEFERRED.

CLASS zcl_abapgit_user_record DEFINITION LOCAL FRIENDS ltcl_user_record.

CLASS ltcl_user_record DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS gc_wrong_user TYPE sy-uname VALUE 'WRONG_USER'.
    METHODS:
      test_valid_user FOR TESTING RAISING cx_static_check,
      test_invalid_user FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_user_record IMPLEMENTATION.

  METHOD test_valid_user.
    DATA: lv_uname TYPE sy-uname.
    DATA: lo_user_record TYPE REF TO zcl_abapgit_user_record.

    " Find user id with an email address
    SELECT SINGLE u~bname INTO lv_uname FROM usr21 AS u
        INNER JOIN adrp AS p ON p~persnumber = u~persnumber
        INNER JOIN adr6 AS a ON a~persnumber = u~persnumber AND a~addrnumber = u~addrnumber
        WHERE p~date_from <= sy-datum AND p~date_to   >= sy-datum
          AND a~date_from <= sy-datum AND a~smtp_addr LIKE '%@%'.
    IF sy-subrc <> 0.
      RETURN. " skip test
    ENDIF.

    zcl_abapgit_user_record=>reset( ).
    lo_user_record = zcl_abapgit_user_record=>get_instance( lv_uname ).

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lines( zcl_abapgit_user_record=>gt_user )
      msg = |User { sy-uname } is missing in the list| ).
  ENDMETHOD.

  METHOD test_invalid_user.
    DATA: lo_user_record TYPE REF TO zcl_abapgit_user_record.

    zcl_abapgit_user_record=>reset( ).
    lo_user_record = zcl_abapgit_user_record=>get_instance( gc_wrong_user ).

    cl_abap_unit_assert=>assert_equals(
      exp = 0
      act = lines( zcl_abapgit_user_record=>gt_user )
      msg = |User { gc_wrong_user } is missing in the list| ).
  ENDMETHOD.

ENDCLASS.
