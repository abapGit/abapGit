INTERFACE zif_abapgit_environment
  PUBLIC.
  TYPES:
    BEGIN OF ty_release_sp,
      release TYPE c LENGTH 10,
      sp      TYPE c LENGTH 10,
    END OF ty_release_sp,
    ty_system_language_filter TYPE RANGE OF spras.

  METHODS is_sap_cloud_platform
    RETURNING
      VALUE(rv_result) TYPE abap_bool.
  METHODS is_merged
    RETURNING
      VALUE(rv_result) TYPE abap_bool.
  METHODS is_repo_object_changes_allowed
    RETURNING
      VALUE(rv_result) TYPE abap_bool.
  METHODS compare_with_inactive
    RETURNING
      VALUE(rv_result) TYPE abap_bool.
  METHODS is_restart_required
    RETURNING
      VALUE(rv_result) TYPE abap_bool.
  METHODS is_sap_object_allowed
    RETURNING
      VALUE(rv_allowed) TYPE abap_bool.
  METHODS get_basis_release
    RETURNING
      VALUE(rs_result) TYPE ty_release_sp.
  METHODS get_system_language_filter
    RETURNING
      VALUE(rt_system_language_filter) TYPE ty_system_language_filter.
  METHODS is_variant_maintenance
    RETURNING
      VALUE(rv_is_variant_maintenance) TYPE abap_bool.
ENDINTERFACE.
