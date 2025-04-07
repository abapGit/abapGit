CLASS zcl_abapgit_web_environment DEFINITION PUBLIC.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_environment.
ENDCLASS.

CLASS zcl_abapgit_web_environment IMPLEMENTATION.

  METHOD zif_abapgit_environment~compare_with_inactive.
    ASSERT 1 = 'decoupled'.
  ENDMETHOD.

  METHOD zif_abapgit_environment~init_parallel_processing.
    ASSERT 1 = 'decoupled'.
  ENDMETHOD.

  METHOD zif_abapgit_environment~check_parallel_processing.
* the group doesnt exist
    rv_checked = abap_false.
  ENDMETHOD.

  METHOD zif_abapgit_environment~get_available_user_sessions.
    ASSERT 1 = 'decoupled'.
  ENDMETHOD.

  METHOD zif_abapgit_environment~get_basis_release.
    ASSERT 1 = 'decoupled'.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_merged.
    rv_result = abap_false.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_repo_object_changes_allowed.
    rv_result = abap_true.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_restart_required.
    ASSERT 1 = 'decoupled'.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_sap_cloud_platform.
    rv_result = abap_false.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_sap_object_allowed.
    rv_allowed = abap_false.
  ENDMETHOD.

  METHOD zif_abapgit_environment~get_system_language_filter.
    ASSERT 1 = 'decoupled'.
  ENDMETHOD.

  METHOD zif_abapgit_environment~is_variant_maintenance.
    ASSERT 1 = 'decoupled'.
  ENDMETHOD.

ENDCLASS.
