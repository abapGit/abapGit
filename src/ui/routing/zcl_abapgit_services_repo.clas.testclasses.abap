CLASS ltcl_objects_to_delete DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      confirmed_deletion FOR TESTING,
      declined_deletion FOR TESTING,
      table_with_data_confirmed FOR TESTING,
      table_with_data_declined FOR TESTING,
      add_not_deleted FOR TESTING.

    METHODS add_overwrite
      IMPORTING
        iv_obj_type TYPE tadir-object
        iv_obj_name TYPE tadir-obj_name
        iv_action   TYPE i
        iv_decision TYPE zif_abapgit_definitions=>ty_yes_no.

    METHODS add_tabl_with_data
      IMPORTING
        iv_obj_name TYPE tadir-obj_name
        iv_decision TYPE zif_abapgit_definitions=>ty_yes_no.

    METHODS assert_deleted
      IMPORTING
        iv_expected TYPE string.

    DATA ms_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.

ENDCLASS.

CLASS zcl_abapgit_services_repo DEFINITION LOCAL FRIENDS ltcl_objects_to_delete.

CLASS ltcl_objects_to_delete IMPLEMENTATION.

  METHOD add_overwrite.

    DATA ls_overwrite LIKE LINE OF ms_checks-overwrite.

    ls_overwrite-obj_type = iv_obj_type.
    ls_overwrite-obj_name = iv_obj_name.
    ls_overwrite-devclass = '$TEST'.
    ls_overwrite-action   = iv_action.
    ls_overwrite-decision = iv_decision.
    INSERT ls_overwrite INTO TABLE ms_checks-overwrite.

  ENDMETHOD.

  METHOD add_tabl_with_data.

    DATA ls_overwrite LIKE LINE OF ms_checks-delete_tabl_with_data.

    ls_overwrite-obj_type = 'TABL'.
    ls_overwrite-obj_name = iv_obj_name.
    ls_overwrite-devclass = '$TEST'.
    ls_overwrite-action   = zif_abapgit_objects=>c_deserialize_action-delete_tabl_with_data.
    ls_overwrite-decision = iv_decision.
    INSERT ls_overwrite INTO TABLE ms_checks-delete_tabl_with_data.

  ENDMETHOD.

  METHOD assert_deleted.

    " the deleted objects as "TYPE NAME" lines, separated by commas
    DATA lt_tadir TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA ls_tadir LIKE LINE OF lt_tadir.
    DATA lv_actual TYPE string.

    lt_tadir = zcl_abapgit_services_repo=>get_objects_to_delete( ms_checks ).

    LOOP AT lt_tadir INTO ls_tadir.
      IF lv_actual IS NOT INITIAL.
        lv_actual = lv_actual && `,`.
      ENDIF.
      lv_actual = lv_actual && |{ ls_tadir-object } { ls_tadir-obj_name }|.
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
      act = lv_actual
      exp = iv_expected ).

  ENDMETHOD.

  METHOD confirmed_deletion.

    add_overwrite( iv_obj_type = 'PROG'
                   iv_obj_name = 'ZTEST'
                   iv_action   = zif_abapgit_objects=>c_deserialize_action-delete
                   iv_decision = zif_abapgit_definitions=>c_yes ).

    assert_deleted( `PROG ZTEST` ).

  ENDMETHOD.

  METHOD declined_deletion.

    add_overwrite( iv_obj_type = 'PROG'
                   iv_obj_name = 'ZTEST'
                   iv_action   = zif_abapgit_objects=>c_deserialize_action-delete
                   iv_decision = zif_abapgit_definitions=>c_no ).

    assert_deleted( `` ).

  ENDMETHOD.

  METHOD table_with_data_confirmed.

    add_overwrite( iv_obj_type = 'TABL'
                   iv_obj_name = 'ZTEST'
                   iv_action   = zif_abapgit_objects=>c_deserialize_action-delete
                   iv_decision = zif_abapgit_definitions=>c_yes ).
    add_tabl_with_data( iv_obj_name = 'ZTEST'
                        iv_decision = zif_abapgit_definitions=>c_yes ).

    assert_deleted( `TABL ZTEST` ).

  ENDMETHOD.

  METHOD table_with_data_declined.

    " Yes to deleting the object, No to "Delete table that contains data"
    add_overwrite( iv_obj_type = 'TABL'
                   iv_obj_name = 'ZTEST'
                   iv_action   = zif_abapgit_objects=>c_deserialize_action-delete
                   iv_decision = zif_abapgit_definitions=>c_yes ).
    add_tabl_with_data( iv_obj_name = 'ZTEST'
                        iv_decision = zif_abapgit_definitions=>c_no ).
    add_overwrite( iv_obj_type = 'PROG'
                   iv_obj_name = 'ZTEST'
                   iv_action   = zif_abapgit_objects=>c_deserialize_action-delete
                   iv_decision = zif_abapgit_definitions=>c_yes ).

    assert_deleted( `PROG ZTEST` ).

  ENDMETHOD.

  METHOD add_not_deleted.

    add_overwrite( iv_obj_type = 'PROG'
                   iv_obj_name = 'ZTEST'
                   iv_action   = zif_abapgit_objects=>c_deserialize_action-add
                   iv_decision = zif_abapgit_definitions=>c_yes ).

    assert_deleted( `` ).

  ENDMETHOD.

ENDCLASS.
