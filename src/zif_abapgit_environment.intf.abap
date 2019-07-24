INTERFACE zif_abapgit_environment
    PUBLIC .
  METHODS is_sap_cloud_platform
    RETURNING
      VALUE(RV_result) TYPE abap_bool .
  METHODS is_merged
    RETURNING
      VALUE(RV_result) TYPE abap_bool .
  METHODS is_repo_object_changes_allowed
    RETURNING
      VALUE(RV_result) TYPE abap_bool .
  METHODS compare_with_inactive
    RETURNING
      VALUE(rv_result) TYPE boolean .
  METHODS is_restart_required
    RETURNING
      VALUE(RV_result) TYPE boolean .
ENDINTERFACE.
