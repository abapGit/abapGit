CLASS ltd_environment DEFINITION FOR TESTING.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_environment.

    METHODS:
      set_is_merged
        IMPORTING
          iv_is_merged TYPE abap_bool.
  PRIVATE SECTION.
    DATA mv_is_merged TYPE abap_bool.

ENDCLASS.

CLASS ltd_environment IMPLEMENTATION.

  METHOD zif_abapgit_environment~check_parallel_processing.

  ENDMETHOD.

  METHOD zif_abapgit_environment~compare_with_inactive.

  ENDMETHOD.

  METHOD zif_abapgit_environment~get_available_user_sessions.

  ENDMETHOD.

  METHOD zif_abapgit_environment~get_basis_release.

  ENDMETHOD.

  METHOD zif_abapgit_environment~get_system_language_filter.

  ENDMETHOD.

  METHOD zif_abapgit_environment~init_parallel_processing.

  ENDMETHOD.

  METHOD zif_abapgit_environment~is_merged.
    rv_result = mv_is_merged.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_repo_object_changes_allowed.

  ENDMETHOD.

  METHOD zif_abapgit_environment~is_restart_required.

  ENDMETHOD.

  METHOD zif_abapgit_environment~is_sap_cloud_platform.

  ENDMETHOD.

  METHOD zif_abapgit_environment~is_sap_object_allowed.

  ENDMETHOD.

  METHOD zif_abapgit_environment~is_variant_maintenance.

  ENDMETHOD.

  METHOD set_is_merged.
    mv_is_merged = iv_is_merged.
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_feature DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    DATA mo_settings TYPE REF TO zcl_abapgit_settings.
    DATA mo_cut TYPE REF TO zcl_abapgit_feature.
    DATA mo_env TYPE REF TO ltd_environment.

    METHODS:
      setup,
      merged_feature_disabled FOR TESTING,
      dev_feature_disabled FOR TESTING,
      dev_feature_enabled FOR TESTING.

ENDCLASS.

CLASS ltcl_feature IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
    mo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).

    CREATE OBJECT mo_env.
    zcl_abapgit_injector=>set_environment( mo_env ).
  ENDMETHOD.

  METHOD merged_feature_disabled.

    mo_env->set_is_merged( abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( 'TEST' )
      exp = abap_false ).

  ENDMETHOD.

  METHOD dev_feature_disabled.

    mo_env->set_is_merged( abap_false ).

    " All off
    mo_settings->set_experimental_features( '' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( 'TEST' )
      exp = abap_false ).

  ENDMETHOD.

  METHOD dev_feature_enabled.

    mo_env->set_is_merged( abap_false ).

    " All on
    mo_settings->set_experimental_features( 'X' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( 'TEST' )
      exp = abap_true ).

    " Just one feature on
    mo_settings->set_experimental_features( 'TEST' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( 'TEST' )
      exp = abap_true ).

    " Several features on
    mo_settings->set_experimental_features( 'AFF,LXE' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( 'TEST' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( 'AFF' )
      exp = abap_true ).

    " Several features on with whitespace
    mo_settings->set_experimental_features( ' AFF , FLOW   ,LXE ' ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( 'TEST' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_enabled( 'FLOW' )
      exp = abap_true ).

  ENDMETHOD.

ENDCLASS.
