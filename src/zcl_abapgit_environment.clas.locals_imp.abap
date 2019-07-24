*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS ltcl_test_simple_positive DEFINITION DEFERRED.
CLASS ltcl_test_simple_negative DEFINITION DEFERRED.
CLASS zcl_abapgit_environment DEFINITION LOCAL FRIENDS ltcl_test_simple_positive ltcl_test_simple_negative.

CLASS lcl_abapgit_environment_logic DEFINITION.
  PUBLIC SECTION.
    INTERFACES
      zif_abapgit_environment.

    ALIASES:
    is_sap_cloud_platform FOR zif_abapgit_environment~is_sap_cloud_platform,
    is_merged FOR zif_abapgit_environment~is_merged,
    is_repo_object_changes_allowed FOR zif_abapgit_environment~is_repo_object_changes_allowed,
    compare_with_inactive FOR zif_abapgit_environment~compare_with_inactive,
    is_restart_required FOR zif_abapgit_environment~is_restart_required.

  PRIVATE SECTION.
    DATA mv_cloud TYPE abap_bool VALUE abap_undefined ##NO_TEXT.
    DATA mv_is_merged TYPE abap_bool VALUE abap_undefined ##NO_TEXT.
    DATA mv_client_modifiable TYPE abap_bool VALUE abap_undefined ##NO_TEXT.

ENDCLASS.

CLASS lcl_abapgit_environment_logic IMPLEMENTATION.

  METHOD zif_abapgit_environment~is_sap_cloud_platform.
    IF mv_cloud = abap_undefined.
      TRY.
          CALL METHOD ('CL_COS_UTILITIES')=>('IS_SAP_CLOUD_PLATFORM')
            RECEIVING
              rv_is_sap_cloud_platform = mv_cloud.
        CATCH cx_sy_dyn_call_illegal_method.
          mv_cloud = abap_false.
      ENDTRY.
    ENDIF.
    rv_result = mv_cloud.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_merged.
    DATA lo_marker TYPE REF TO data ##NEEDED.

    IF mv_is_merged = abap_undefined.
      TRY.
          CREATE DATA lo_marker TYPE REF TO ('LIF_ABAPMERGE_MARKER')  ##no_text.
          "No exception --> marker found
          mv_is_merged = abap_true.

        CATCH cx_sy_create_data_error.
          mv_is_merged = abap_false.
      ENDTRY.
    ENDIF.
    rv_result = mv_is_merged.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_repo_object_changes_allowed.
    DATA lv_ind TYPE t000-ccnocliind.

    IF mv_client_modifiable = abap_undefined.
      SELECT SINGLE ccnocliind FROM t000 INTO lv_ind
             WHERE mandt = sy-mandt.
      IF sy-subrc = 0
          AND ( lv_ind = ' ' OR lv_ind = '1' ). "check changes allowed
        mv_client_modifiable = abap_true.
      ELSE.
        mv_client_modifiable = abap_false.
      ENDIF.
    ENDIF.
    rv_result = mv_client_modifiable.
  ENDMETHOD.

  METHOD zif_abapgit_environment~compare_with_inactive.
    rv_result = is_sap_cloud_platform( ).
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_restart_required.
    rv_result = abap_false.
    TRY.
        CALL METHOD ('CL_APJ_SCP_TOOLS')=>('IS_RESTART_REQUIRED')
          RECEIVING
            restart_required = rv_result.
      CATCH cx_sy_dyn_call_illegal_method.
        rv_result = abap_false.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

CLASS ltd_abapgit_environment_stub DEFINITION FOR TESTING.

  PUBLIC SECTION.
    INTERFACES
      zif_abapgit_environment.

    ALIASES:
    is_sap_cloud_platform FOR zif_abapgit_environment~is_sap_cloud_platform,
    is_merged FOR zif_abapgit_environment~is_merged,
    is_repo_object_changes_allowed FOR zif_abapgit_environment~is_repo_object_changes_allowed,
    compare_with_inactive FOR zif_abapgit_environment~compare_with_inactive,
    is_restart_required FOR zif_abapgit_environment~is_restart_required.

    TYPES:
      BEGIN OF ty_return_value,
        return_value TYPE string,
        "exception    TYPE string, "currently there is no method which throws exceptions during processing
      END OF ty_return_value,

      BEGIN OF ty_return_value_config,
        is_sap_cloud_platform          TYPE ty_return_value,
        is_merged                      TYPE ty_return_value,
        is_repo_object_changes_allowed TYPE ty_return_value,
        compare_with_inactive          TYPE ty_return_value,
        is_restart_required            TYPE ty_return_value,
      END OF ty_return_value_config.

    METHODS:
      define_return_value_config IMPORTING is_config TYPE ty_return_value_config.

  PRIVATE SECTION.
    DATA:
    ms_config TYPE ty_return_value_config.

ENDCLASS.

CLASS ltd_abapgit_environment_stub IMPLEMENTATION.

  METHOD compare_with_inactive.
    rv_result = ms_config-compare_with_inactive-return_value.
  ENDMETHOD.

  METHOD is_merged.
    rv_result = ms_config-is_merged-return_value.
  ENDMETHOD.

  METHOD is_repo_object_changes_allowed.
    rv_result = ms_config-is_repo_object_changes_allowed-return_value.
  ENDMETHOD.

  METHOD is_restart_required.
    rv_result = ms_config-is_restart_required-return_value.
  ENDMETHOD.

  METHOD is_sap_cloud_platform.
    rv_result = ms_config-is_restart_required-return_value.
  ENDMETHOD.

  METHOD define_return_value_config.
    ms_config = is_config.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_test_simple_positive DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zif_abapgit_environment.

    METHODS:
      setup,
      compare_with_inactive FOR TESTING,
      is_merged FOR TESTING,
      is_repo_object_changes_allowed FOR TESTING,
      is_restart_required FOR TESTING,
      is_sap_cloud_platform FOR TESTING.

ENDCLASS.

CLASS ltcl_test_simple_negative DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zif_abapgit_environment.

    METHODS:
      setup,
      compare_with_inactive FOR TESTING,
      is_merged FOR TESTING,
      is_repo_object_changes_allowed FOR TESTING,
      is_restart_required FOR TESTING,
      is_sap_cloud_platform FOR TESTING.

ENDCLASS.

CLASS ltcl_test_environment_logic DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO lcl_abapgit_environment_logic.

    METHODS:
      setup,
      compare_with_inactive FOR TESTING,
      is_merged FOR TESTING,
      is_repo_object_changes_allowed FOR TESTING,
      is_restart_required FOR TESTING,
      is_sap_cloud_platform FOR TESTING.

ENDCLASS.


CLASS ltcl_test_simple_positive IMPLEMENTATION.

  METHOD setup.
    mo_cut = zcl_abapgit_environment=>get_instance( ).

    DATA mo_cut_downcast TYPE REF TO zcl_abapgit_environment.
    mo_cut_downcast ?= mo_cut.

    DATA ltd_Stub TYPE REF TO ltd_abapgit_environment_stub.
    CREATE OBJECT ltd_stub.

    DATA ms_config TYPE ltd_abapgit_environment_stub=>ty_return_value_config.

    "Define stub configuration
    ms_config-compare_with_inactive-return_value = abap_true.
    ms_config-is_merged-return_value = abap_true.
    ms_config-is_repo_object_changes_allowed-return_value = abap_true.
    ms_config-is_restart_required-return_value = abap_true.
    ms_config-is_sap_cloud_platform-return_value = abap_true.

    ltd_stub->define_return_value_config( ms_config ).

    mo_cut_downcast->inject( ltd_stub ).
  ENDMETHOD.

  METHOD compare_with_inactive.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->compare_with_inactive( )
                                        exp =  abap_true ).
  ENDMETHOD.

  METHOD is_merged.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_merged( )
                                        exp =  abap_true ).
  ENDMETHOD.

  METHOD is_repo_object_changes_allowed.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_repo_object_changes_allowed( )
                                        exp =  abap_true ).
  ENDMETHOD.

  METHOD is_restart_required.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_restart_required( )
                                        exp =  abap_true ).
  ENDMETHOD.

  METHOD is_sap_cloud_platform.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_sap_cloud_platform( )
                                        exp =  abap_true ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_test_simple_NEGATIVE IMPLEMENTATION.

  METHOD setup.
    mo_cut = zcl_abapgit_environment=>get_instance( ).

    DATA mo_cut_downcast TYPE REF TO zcl_abapgit_environment.
    mo_cut_downcast ?= mo_cut.

    DATA ltd_Stub TYPE REF TO ltd_abapgit_environment_stub.
    CREATE OBJECT ltd_stub.

    DATA ms_config TYPE ltd_abapgit_environment_stub=>ty_return_value_config.

    "Define stub configuration
    ms_config-compare_with_inactive-return_value = abap_false.
    ms_config-is_merged-return_value = abap_false.
    ms_config-is_repo_object_changes_allowed-return_value = abap_false.
    ms_config-is_restart_required-return_value = abap_false.
    ms_config-is_sap_cloud_platform-return_value = abap_false.

    ltd_stub->define_return_value_config( ms_config ).
    mo_cut_downcast->inject( ltd_stub ).
  ENDMETHOD.

  METHOD compare_with_inactive.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->compare_with_inactive( )
                                        exp =  abap_false ).
  ENDMETHOD.

  METHOD is_merged.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_merged( )
                                        exp =  abap_false ).
  ENDMETHOD.

  METHOD is_repo_object_changes_allowed.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_repo_object_changes_allowed( )
                                        exp =  abap_false ).
  ENDMETHOD.

  METHOD is_restart_required.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_restart_required( )
                                        exp =  abap_false ).
  ENDMETHOD.

  METHOD is_sap_cloud_platform.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_sap_cloud_platform( )
                                        exp =  abap_false ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_test_environment_logic IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD compare_with_inactive.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->compare_with_inactive( )
                                        exp = mo_cut->compare_with_inactive( ) ).
  ENDMETHOD.

  METHOD is_merged.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_merged( )
                                        exp = mo_cut->is_merged( ) ).
  ENDMETHOD.

  METHOD is_repo_object_changes_allowed.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_repo_object_changes_allowed( )
                                        exp = mo_cut->is_repo_object_changes_allowed( ) ).
  ENDMETHOD.

  METHOD is_restart_required.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_restart_required( )
                                        exp = mo_cut->is_restart_required( ) ).
  ENDMETHOD.

  METHOD is_sap_cloud_platform.
    cl_abap_unit_assert=>assert_equals( act = mo_cut->is_sap_cloud_platform( )
                                        exp = mo_cut->is_sap_cloud_platform( ) ).
  ENDMETHOD.
ENDCLASS.
