CLASS ltd_pull DEFINITION DEFERRED.
CLASS ltcl_pull DEFINITION DEFERRED.
CLASS ltcl_objects_to_delete DEFINITION DEFERRED.
CLASS zcl_abapgit_repo_pull DEFINITION LOCAL FRIENDS ltd_pull ltcl_pull ltcl_objects_to_delete.

CLASS ltd_repo DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo.

    DATA ms_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.
    DATA mi_new_log TYPE REF TO zif_abapgit_log.
    DATA mv_deserialized TYPE abap_bool.
    DATA ms_deserialize_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.
    DATA mi_deserialize_log TYPE REF TO zif_abapgit_log.
    DATA mv_refreshed TYPE abap_bool.
    DATA mv_drop_log TYPE abap_bool.
    DATA mv_calls TYPE string.

ENDCLASS.

CLASS ltd_repo IMPLEMENTATION.

  METHOD zif_abapgit_repo~deserialize_checks.
    rs_checks = ms_checks.
  ENDMETHOD.

  METHOD zif_abapgit_repo~deserialize.
    mv_calls = mv_calls && `deserialize`.
    mv_deserialized = abap_true.
    ms_deserialize_checks = is_checks.
    mi_deserialize_log = ii_log.
  ENDMETHOD.

  METHOD zif_abapgit_repo~create_new_log.
    CREATE OBJECT mi_new_log TYPE zcl_abapgit_log.
    ri_log = mi_new_log.
  ENDMETHOD.

  METHOD zif_abapgit_repo~refresh.
    mv_calls = mv_calls && `refresh,`.
    mv_refreshed = abap_true.
    mv_drop_log = iv_drop_log.
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

CLASS ltd_pull DEFINITION FINAL INHERITING FROM zcl_abapgit_repo_pull FOR TESTING
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA mt_deleted TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA ms_delete_checks TYPE zif_abapgit_definitions=>ty_delete_checks.
    DATA mi_delete_log TYPE REF TO zif_abapgit_log.
    DATA mo_repo TYPE REF TO ltd_repo.

  PROTECTED SECTION.
    METHODS delete_tadir REDEFINITION.

ENDCLASS.

CLASS ltd_pull IMPLEMENTATION.

  METHOD delete_tadir.
    mo_repo->mv_calls = mo_repo->mv_calls && `delete,`.
    mt_deleted = it_tadir.
    ms_delete_checks = is_checks.
    mi_delete_log = ii_log.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_pull DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_repo TYPE REF TO ltd_repo.
    DATA mi_cut TYPE REF TO zif_abapgit_repo_pull.

    METHODS setup.
    METHODS teardown.
    METHODS factory_binds_repository FOR TESTING RAISING zcx_abapgit_exception.
    METHODS factory_uses_injected_pull FOR TESTING RAISING zcx_abapgit_exception.
    METHODS checks_from_repo FOR TESTING RAISING zcx_abapgit_exception.
    METHODS pull_creates_log FOR TESTING RAISING zcx_abapgit_exception.
    METHODS pull_uses_given_log FOR TESTING RAISING zcx_abapgit_exception.
    METHODS pull_passes_decisions FOR TESTING RAISING zcx_abapgit_exception.
    METHODS confirmed_deletion_sequence FOR TESTING RAISING zcx_abapgit_exception.

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

    DATA lv_same TYPE abap_bool.
    DATA li_log TYPE REF TO zif_abapgit_log.
    DATA ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.

    li_log = mi_cut->pull( ls_checks ).

    cl_abap_unit_assert=>assert_bound( li_log ).
    lv_same = boolc( li_log = mo_repo->mi_new_log ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_same
      exp = abap_true ).
    lv_same = boolc( li_log = mo_repo->mi_deserialize_log ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_same
      exp = abap_true ).

  ENDMETHOD.

  METHOD pull_uses_given_log.

    DATA lv_same TYPE abap_bool.
    DATA li_given TYPE REF TO zif_abapgit_log.
    DATA li_log TYPE REF TO zif_abapgit_log.
    DATA ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.

    CREATE OBJECT li_given TYPE zcl_abapgit_log.

    li_log = mi_cut->pull( is_checks = ls_checks
                           ii_log    = li_given ).

    lv_same = boolc( li_log = li_given ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_same
      exp = abap_true ).
    lv_same = boolc( li_given = mo_repo->mi_deserialize_log ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_same
      exp = abap_true ).
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

  METHOD confirmed_deletion_sequence.

    DATA lo_pull TYPE REF TO ltd_pull.
    DATA li_log TYPE REF TO zif_abapgit_log.
    DATA ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.
    DATA ls_overwrite LIKE LINE OF ls_checks-overwrite.
    DATA ls_expected TYPE zif_abapgit_definitions=>ty_tadir.
    DATA lt_expected TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lv_same TYPE abap_bool.

    CREATE OBJECT lo_pull EXPORTING ii_repo = mo_repo.
    lo_pull->mo_repo = mo_repo.
    ls_overwrite-obj_type = 'PROG'.
    ls_overwrite-obj_name = 'ZTEST'.
    ls_overwrite-devclass = '$TEST'.
    ls_overwrite-action = zif_abapgit_objects=>c_deserialize_action-delete.
    ls_overwrite-decision = zif_abapgit_definitions=>c_yes.
    INSERT ls_overwrite INTO TABLE ls_checks-overwrite.
    ls_checks-transport-required = abap_true.
    ls_checks-transport-transport = 'A4HK900001'.

    li_log = lo_pull->zif_abapgit_repo_pull~pull( ls_checks ).

    ls_expected-pgmid = 'R3TR'.
    ls_expected-object = 'PROG'.
    ls_expected-obj_name = 'ZTEST'.
    ls_expected-devclass = '$TEST'.
    INSERT ls_expected INTO TABLE lt_expected.
    cl_abap_unit_assert=>assert_equals(
      act = lo_pull->mt_deleted
      exp = lt_expected ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_pull->ms_delete_checks-transport
      exp = ls_checks-transport ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->mv_calls
      exp = `delete,refresh,deserialize` ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->mv_drop_log
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->ms_deserialize_checks
      exp = ls_checks ).
    lv_same = boolc( lo_pull->mi_delete_log = li_log ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_same
      exp = abap_true ).
    lv_same = boolc( mo_repo->mi_deserialize_log = li_log ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_same
      exp = abap_true ).

  ENDMETHOD.

  METHOD teardown.

    DATA li_no_pull TYPE REF TO zif_abapgit_repo_pull.

    zcl_abapgit_injector=>set_repo_pull( li_no_pull ).

  ENDMETHOD.

  METHOD factory_binds_repository.

    DATA lo_other TYPE REF TO ltd_repo.
    DATA li_first TYPE REF TO zif_abapgit_repo_pull.
    DATA li_second TYPE REF TO zif_abapgit_repo_pull.
    DATA ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.

    CREATE OBJECT lo_other.
    mo_repo->ms_checks-transport-transport = 'A4HK900001'.
    lo_other->ms_checks-transport-transport = 'A4HK900002'.
    li_first = zcl_abapgit_factory=>get_repo_pull( mo_repo ).
    li_second = zcl_abapgit_factory=>get_repo_pull( lo_other ).
    ls_checks = li_first->checks( ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_checks-transport-transport
      exp = 'A4HK900001' ).
    ls_checks = li_second->checks( ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_checks-transport-transport
      exp = 'A4HK900002' ).
    ls_checks = li_first->checks( ).
    li_first->pull( ls_checks ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->mv_deserialized
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_other->mv_deserialized
      exp = abap_false ).
    ls_checks = li_second->checks( ).
    li_second->pull( ls_checks ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_other->mv_deserialized
      exp = abap_true ).

  ENDMETHOD.

  METHOD factory_uses_injected_pull.

    DATA lo_other TYPE REF TO ltd_repo.
    DATA li_pull TYPE REF TO zif_abapgit_repo_pull.
    DATA ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.
    DATA lv_same TYPE abap_bool.

    CREATE OBJECT lo_other.
    zcl_abapgit_injector=>set_repo_pull( mi_cut ).
    li_pull = zcl_abapgit_factory=>get_repo_pull( lo_other ).
    lv_same = boolc( li_pull = mi_cut ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_same
      exp = abap_true ).
    li_pull->pull( ls_checks ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->mv_deserialized
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_other->mv_deserialized
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
      confirmed_delete_add FOR TESTING,
      declined_delete_add FOR TESTING,
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

  METHOD confirmed_delete_add.

    add_overwrite( iv_obj_type = 'PROG'
                   iv_obj_name = 'ZTEST'
                   iv_action   = zif_abapgit_objects=>c_deserialize_action-delete_add
                   iv_decision = zif_abapgit_definitions=>c_yes ).
    assert_deleted( `PROG ZTEST` ).

  ENDMETHOD.

  METHOD declined_delete_add.

    add_overwrite( iv_obj_type = 'PROG'
                   iv_obj_name = 'ZTEST'
                   iv_action   = zif_abapgit_objects=>c_deserialize_action-delete_add
                   iv_decision = zif_abapgit_definitions=>c_no ).
    assert_deleted( `` ).

  ENDMETHOD.

ENDCLASS.
