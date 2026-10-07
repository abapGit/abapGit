CLASS ltcl_pull DEFINITION DEFERRED.
CLASS ltcl_objects_to_delete DEFINITION DEFERRED.
CLASS zcl_abapgit_repo_pull DEFINITION LOCAL FRIENDS ltcl_pull ltcl_objects_to_delete.

CLASS ltd_repo DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo.

    DATA ms_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.
    DATA mi_new_log TYPE REF TO zif_abapgit_log.
    DATA mv_deserialized TYPE abap_bool.
    DATA ms_deserialize_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.
    DATA mi_deserialize_log TYPE REF TO zif_abapgit_log.
    DATA mv_refreshed TYPE abap_bool.

ENDCLASS.

CLASS ltd_repo IMPLEMENTATION.

  METHOD zif_abapgit_repo~deserialize_checks.
    rs_checks = ms_checks.
  ENDMETHOD.

  METHOD zif_abapgit_repo~deserialize.
    mv_deserialized = abap_true.
    ms_deserialize_checks = is_checks.
    mi_deserialize_log = ii_log.
  ENDMETHOD.

  METHOD zif_abapgit_repo~create_new_log.
    CREATE OBJECT mi_new_log TYPE zcl_abapgit_log.
    ri_log = mi_new_log.
  ENDMETHOD.

  METHOD zif_abapgit_repo~refresh.
    mv_refreshed = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_key.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_name.
  ENDMETHOD.
  METHOD zif_abapgit_repo~is_offline.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_package.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_local_settings.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_tadir_objects.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_local_filtered.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_local.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_remote.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_dot_abapgit.
  ENDMETHOD.
  METHOD zif_abapgit_repo~set_dot_abapgit.
  ENDMETHOD.
  METHOD zif_abapgit_repo~find_remote_dot_abapgit.
  ENDMETHOD.
  METHOD zif_abapgit_repo~checksums.
  ENDMETHOD.
  METHOD zif_abapgit_repo~has_remote_source.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_log.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_dot_apack.
  ENDMETHOD.
  METHOD zif_abapgit_repo~delete_checks.
  ENDMETHOD.
  METHOD zif_abapgit_repo~set_files_remote.
  ENDMETHOD.
  METHOD zif_abapgit_repo~set_local_settings.
  ENDMETHOD.
  METHOD zif_abapgit_repo~switch_repo_type.
  ENDMETHOD.
  METHOD zif_abapgit_repo~refresh_local_object.
  ENDMETHOD.
  METHOD zif_abapgit_repo~refresh_local_objects.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_data_config.
  ENDMETHOD.
  METHOD zif_abapgit_repo~bind_listener.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_pull DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_repo TYPE REF TO ltd_repo.
    DATA mi_cut TYPE REF TO zif_abapgit_repo_pull.

    METHODS setup.
    METHODS checks_from_repo FOR TESTING RAISING zcx_abapgit_exception.
    METHODS pull_creates_log FOR TESTING RAISING zcx_abapgit_exception.
    METHODS pull_uses_given_log FOR TESTING RAISING zcx_abapgit_exception.
    METHODS pull_passes_decisions FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_pull IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_repo.
    CREATE OBJECT mi_cut TYPE zcl_abapgit_repo_pull
      EXPORTING
        ii_repo = mo_repo.
  ENDMETHOD.

  METHOD checks_from_repo.

    DATA ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.

    mo_repo->ms_checks-transport-required = abap_true.

    ls_checks = mi_cut->checks( ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_checks-transport-required
      exp = abap_true ).

  ENDMETHOD.

  METHOD pull_creates_log.

    DATA li_log TYPE REF TO zif_abapgit_log.
    DATA ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.

    li_log = mi_cut->pull( ls_checks ).

    cl_abap_unit_assert=>assert_bound( li_log ).
    cl_abap_unit_assert=>assert_true( boolc( li_log = mo_repo->mi_new_log ) ).
    cl_abap_unit_assert=>assert_true( boolc( li_log = mo_repo->mi_deserialize_log ) ).

  ENDMETHOD.

  METHOD pull_uses_given_log.

    DATA li_given TYPE REF TO zif_abapgit_log.
    DATA li_log TYPE REF TO zif_abapgit_log.
    DATA ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.

    CREATE OBJECT li_given TYPE zcl_abapgit_log.

    li_log = mi_cut->pull( is_checks = ls_checks
                           ii_log    = li_given ).

    cl_abap_unit_assert=>assert_true( boolc( li_log = li_given ) ).
    cl_abap_unit_assert=>assert_true( boolc( li_given = mo_repo->mi_deserialize_log ) ).
    cl_abap_unit_assert=>assert_not_bound( mo_repo->mi_new_log ).

  ENDMETHOD.

  METHOD pull_passes_decisions.

    " no confirmed deletion: nothing is deleted, the decisions go to deserialize
    DATA ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.
    DATA ls_overwrite LIKE LINE OF ls_checks-overwrite.

    ls_overwrite-obj_type = 'PROG'.
    ls_overwrite-obj_name = 'ZTEST'.
    ls_overwrite-action   = zif_abapgit_objects=>c_deserialize_action-delete.
    ls_overwrite-decision = zif_abapgit_definitions=>c_no.
    INSERT ls_overwrite INTO TABLE ls_checks-overwrite.
    ls_checks-transport-transport = 'A4HK900001'.

    mi_cut->pull( ls_checks ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->mv_deserialized
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->ms_deserialize_checks
      exp = ls_checks ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->mv_refreshed
      exp = abap_false ).

  ENDMETHOD.

ENDCLASS.

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

    lt_tadir = zcl_abapgit_repo_pull=>get_objects_to_delete( ms_checks ).

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
