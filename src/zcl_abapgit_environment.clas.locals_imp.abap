*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS ltcl_abapgit_environment_stub DEFINITION FOR TESTING.

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

CLASS ltcl_abapgit_environment_stub IMPLEMENTATION.

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
