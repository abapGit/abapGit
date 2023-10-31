* Helper to toggle (cloud) enviroment
CLASS lcl_environment DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_environment.

    DATA mv_is_cloud TYPE abap_bool.

    METHODS set_cloud
      IMPORTING
        iv_is_cloud TYPE abap_bool.

ENDCLASS.

CLASS lcl_environment IMPLEMENTATION.

  METHOD set_cloud.
    mv_is_cloud = iv_is_cloud.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_sap_cloud_platform.
    rv_result = mv_is_cloud.
  ENDMETHOD.

  METHOD zif_abapgit_environment~compare_with_inactive.
  ENDMETHOD.
  METHOD zif_abapgit_environment~get_basis_release.
  ENDMETHOD.
  METHOD zif_abapgit_environment~get_system_language_filter.
  ENDMETHOD.
  METHOD zif_abapgit_environment~is_merged.
  ENDMETHOD.
  METHOD zif_abapgit_environment~is_repo_object_changes_allowed.
  ENDMETHOD.
  METHOD zif_abapgit_environment~is_restart_required.
  ENDMETHOD.
  METHOD zif_abapgit_environment~is_sap_object_allowed.
  ENDMETHOD.
  METHOD zif_abapgit_environment~is_variant_maintenance.
  ENDMETHOD.

ENDCLASS.

* Helper to toggle experimental features
CLASS lcl_persist_settings DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_persist_settings.

    DATA mo_settings TYPE REF TO zcl_abapgit_settings.

    METHODS constructor.

ENDCLASS.

CLASS lcl_persist_settings IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT mo_settings.
  ENDMETHOD.

  METHOD zif_abapgit_persist_settings~modify.
  ENDMETHOD.

  METHOD zif_abapgit_persist_settings~read.
    ro_settings = mo_settings.
  ENDMETHOD.

ENDCLASS.

* Test cases
CLASS ltcl_abap_language_version DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.
    " Cloud package hardcoded in cl_abap_language_version
    CONSTANTS c_cloud_package TYPE devclass VALUE 'TEST_LANGUAGE_VERSION_SCP'.

    DATA:
      mt_versions          TYPE string_table,
      mv_has_cloud_package TYPE abap_bool,
      mo_environment       TYPE REF TO lcl_environment,
      mi_persistency       TYPE REF TO zif_abapgit_persist_settings,
      mo_dot_abapgit       TYPE REF TO zcl_abapgit_dot_abapgit,
      mo_cut               TYPE REF TO zcl_abapgit_abap_language_vers.

    METHODS:
      setup,
      init
        IMPORTING
          iv_abap_language_version TYPE string,
      set_environment
        IMPORTING
          iv_is_cloud TYPE abap_bool,
      set_features
        IMPORTING
          iv_features TYPE string,
      repo_setting_test
        IMPORTING
          iv_version TYPE string
          iv_exp     TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version,
      object_type_test
        IMPORTING
          iv_version      TYPE string
          iv_standard     TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version
          iv_standard_src TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version
          iv_cloud        TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version
          iv_cloud_src    TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version,
      is_import_allowed_test
        IMPORTING
          iv_version  TYPE string
          iv_standard TYPE abap_bool
          iv_cloud    TYPE abap_bool
          iv_new      TYPE abap_bool.

    METHODS:
      repo_setting_feature_off FOR TESTING,
      repo_setting_feature_on FOR TESTING,
      object_type_feature_off FOR TESTING,
      object_type_feature_on FOR TESTING,
      is_import_allowed FOR TESTING.

ENDCLASS.

CLASS ltcl_abap_language_version IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_environment.
    zcl_abapgit_injector=>set_environment( mo_environment ).

    CREATE OBJECT mi_persistency TYPE lcl_persist_settings.
    zcl_abapgit_persist_injector=>set_settings( mi_persistency ).

    APPEND zif_abapgit_dot_abapgit=>c_abap_language_version-undefined TO mt_versions.
    APPEND zif_abapgit_dot_abapgit=>c_abap_language_version-standard TO mt_versions.
    APPEND zif_abapgit_dot_abapgit=>c_abap_language_version-key_user TO mt_versions.
    APPEND zif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development TO mt_versions.

    mv_has_cloud_package = zcl_abapgit_factory=>get_sap_package( c_cloud_package )->exists( ).
  ENDMETHOD.

  METHOD init.
    mo_dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( ).
    mo_dot_abapgit->set_abap_language_version( iv_abap_language_version ).

    CREATE OBJECT mo_cut
      EXPORTING
        io_dot_abapgit = mo_dot_abapgit.
  ENDMETHOD.

  METHOD set_environment.
    mo_environment->set_cloud( iv_is_cloud ).
  ENDMETHOD.

  METHOD set_features.
    mi_persistency->read( )->set_experimental_features( iv_features ).
  ENDMETHOD.

  METHOD repo_setting_test.

    init( iv_version ).

    " Assume on-prem (no cloud)
    set_environment( abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_repo_abap_language_version( )
      exp = iv_exp
      msg = |ABAP Language Version: { iv_version }, On-prem| ).

    " Assume cloud platform
    set_environment( abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_repo_abap_language_version( )
      exp = iv_exp
      msg = |ABAP Language Version: { iv_version }, Cloud| ).

  ENDMETHOD.

  METHOD repo_setting_feature_off.

    DATA lv_version TYPE string.

    " If experimental feature is off, repo setting is not taken into consideration
    set_features( '' ).

    LOOP AT mt_versions INTO lv_version.

      repo_setting_test(
        iv_version = lv_version
        iv_exp     = zcl_abapgit_abap_language_vers=>c_any_abap_language_version ).

    ENDLOOP.

  ENDMETHOD.

  METHOD repo_setting_feature_on.

    DATA lv_version TYPE string.

    " If experimental feature is on, repo setting is returned
    set_features( zcl_abapgit_abap_language_vers=>c_feature_flag ).

    LOOP AT mt_versions INTO lv_version.

      CASE lv_version.
        WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.

          repo_setting_test(
            iv_version = lv_version
            iv_exp     = zcl_abapgit_abap_language_vers=>c_any_abap_language_version ).

        WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-standard.

          repo_setting_test(
            iv_version = lv_version
            iv_exp     = zif_abapgit_aff_types_v1=>co_abap_language_version_src-standard ).

        WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-key_user.

          repo_setting_test(
            iv_version = lv_version
            iv_exp     = zif_abapgit_aff_types_v1=>co_abap_language_version_src-key_user ).

        WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development.

          repo_setting_test(
            iv_version = lv_version
            iv_exp     = zif_abapgit_aff_types_v1=>co_abap_language_version_src-cloud_development ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD object_type_test.

    init( iv_version ).

    " Assume on-prem (no cloud)
    set_environment( abap_false ).

    " source code
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_abap_language_vers_by_objt(
              iv_object_type = 'INTF'
              iv_package     = '$TMP' )
      exp = iv_standard_src
      msg = |ABAP Language Version: { iv_version }, On-prem| ).

    " non-source code
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_abap_language_vers_by_objt(
              iv_object_type = 'TABL'
              iv_package     = '$TMP' )
      exp = iv_standard
      msg = |ABAP Language Version: { iv_version }, On-prem| ).

    " Assume cloud platform
    IF mv_has_cloud_package = abap_false.
      RETURN.
    ENDIF.

    set_environment( abap_true ).

    " source code
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_abap_language_vers_by_objt(
              iv_object_type = 'INTF'
              iv_package     = c_cloud_package )
      exp = iv_cloud_src
      msg = |ABAP Language Version: { iv_version }, Cloud| ).

    " non-source code
    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->get_abap_language_vers_by_objt(
              iv_object_type = 'TABL'
              iv_package     = c_cloud_package )
      exp = iv_cloud
      msg = |ABAP Language Version: { iv_version }, Cloud| ).

  ENDMETHOD.

  METHOD object_type_feature_off.

    DATA lv_version TYPE string.

    " If experimental feature is off, repo setting is ignored
    set_features( '' ).

    LOOP AT mt_versions INTO lv_version.

      object_type_test(
        iv_version      = lv_version
        iv_standard     = zcl_abapgit_abap_language_vers=>c_any_abap_language_version
        iv_standard_src = zcl_abapgit_abap_language_vers=>c_any_abap_language_version
        iv_cloud        = zcl_abapgit_abap_language_vers=>c_any_abap_language_version
        iv_cloud_src    = zcl_abapgit_abap_language_vers=>c_any_abap_language_version ).

    ENDLOOP.

  ENDMETHOD.

  METHOD object_type_feature_on.

    DATA lv_version TYPE string.

    " If experimental feature is on, repo setting is ignored but package setting is returned
    set_features( zcl_abapgit_abap_language_vers=>c_feature_flag ).

    LOOP AT mt_versions INTO lv_version.

      CASE lv_version.
        WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.

          object_type_test(
            iv_version      = lv_version
            iv_standard     = zcl_abapgit_abap_language_vers=>c_any_abap_language_version
            iv_standard_src = zcl_abapgit_abap_language_vers=>c_any_abap_language_version
            iv_cloud        = zcl_abapgit_abap_language_vers=>c_any_abap_language_version
            iv_cloud_src    = zcl_abapgit_abap_language_vers=>c_any_abap_language_version ).

        WHEN OTHERS.

          object_type_test(
            iv_version      = lv_version
            iv_standard     = zif_abapgit_aff_types_v1=>co_abap_language_version-standard
            iv_standard_src = zif_abapgit_aff_types_v1=>co_abap_language_version_src-standard
            iv_cloud        = zif_abapgit_aff_types_v1=>co_abap_language_version-cloud_development
            iv_cloud_src    = zif_abapgit_aff_types_v1=>co_abap_language_version_src-cloud_development ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD is_import_allowed_test.

    init( iv_version ).

    " Assume on-prem (no cloud)
    set_environment( abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_import_allowed( '$TMP' ) " existing standard package
      exp = iv_standard ).

    IF mv_has_cloud_package = abap_true.
      cl_abap_unit_assert=>assert_equals(
        act = mo_cut->is_import_allowed( c_cloud_package ) " existing cloud package
        exp = iv_cloud ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_import_allowed( 'Z_FOO_BAR' ) " non-existing package
      exp = iv_new ).

    " Assume cloud platform
    set_environment( abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_import_allowed( '$TMP' ) " existing standard package
      exp = iv_standard ).

    IF mv_has_cloud_package = abap_true.
      cl_abap_unit_assert=>assert_equals(
        act = mo_cut->is_import_allowed( c_cloud_package ) " existing cloud package
        exp = iv_cloud ).
    ENDIF.

    cl_abap_unit_assert=>assert_equals(
      act = mo_cut->is_import_allowed( 'Z_FOO_BAR' ) " non-existing package
      exp = iv_new ).

  ENDMETHOD.


  METHOD is_import_allowed.

    DATA lv_version TYPE string.

    LOOP AT mt_versions INTO lv_version.

      CASE lv_version.
        WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-undefined.

          is_import_allowed_test(
            iv_version  = lv_version
            iv_standard = abap_true
            iv_cloud    = abap_true
            iv_new      = abap_true ).

        WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-standard.

          is_import_allowed_test(
            iv_version  = lv_version
            iv_standard = abap_true
            iv_cloud    = abap_false
            iv_new      = abap_true ).

        WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-key_user.

          is_import_allowed_test(
            iv_version  = lv_version
            iv_standard = abap_false
            iv_cloud    = abap_false
            iv_new      = abap_true ).

        WHEN zif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development.

          is_import_allowed_test(
            iv_version  = lv_version
            iv_standard = abap_false
            iv_cloud    = abap_true
            iv_new      = abap_true ).

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
