INTERFACE zif_abapgit_repo_online
  PUBLIC .
  INTERFACES zif_abapgit_git_operations.
  INTERFACES zif_abapgit_repo.
  ALIASES:
    bind_listener FOR zif_abapgit_repo~bind_listener,
    deserialize_checks FOR zif_abapgit_repo~deserialize_checks,
    delete_checks FOR zif_abapgit_repo~delete_checks,
    get_key FOR zif_abapgit_repo~get_key,
    get_name FOR zif_abapgit_repo~get_name,
    get_files_local FOR zif_abapgit_repo~get_files_local,
    get_local_checksums_per_file FOR zif_abapgit_repo~get_local_checksums_per_file,
    get_files_remote FOR zif_abapgit_repo~get_files_remote,
    get_package FOR zif_abapgit_repo~get_package,
    get_dot_abapgit FOR zif_abapgit_repo~get_dot_abapgit,
    set_dot_abapgit FOR zif_abapgit_repo~set_dot_abapgit,
    get_dot_apack FOR zif_abapgit_repo~get_dot_apack,
    deserialize FOR zif_abapgit_repo~deserialize,
    refresh FOR zif_abapgit_repo~refresh,
    update_local_checksums FOR zif_abapgit_repo~update_local_checksums,
    rebuild_local_checksums FOR zif_abapgit_repo~rebuild_local_checksums,
    find_remote_dot_abapgit FOR zif_abapgit_repo~find_remote_dot_abapgit,
    find_remote_dot_apack FOR zif_abapgit_repo~find_remote_dot_apack,
    is_offline FOR zif_abapgit_repo~is_offline,
    set_files_remote FOR zif_abapgit_repo~set_files_remote,
    get_local_settings FOR zif_abapgit_repo~get_local_settings,
    set_local_settings FOR zif_abapgit_repo~set_local_settings,
    has_remote_source FOR zif_abapgit_repo~has_remote_source,
    status FOR zif_abapgit_repo~status,
    switch_repo_type FOR zif_abapgit_repo~switch_repo_type,
    create_new_log FOR zif_abapgit_repo~create_new_log,
    get_log FOR zif_abapgit_repo~get_log,
    reset_log FOR zif_abapgit_repo~reset_log,
    refresh_local_object FOR zif_abapgit_repo~refresh_local_object,
    refresh_local_objects FOR zif_abapgit_repo~refresh_local_objects,
    reset_status FOR zif_abapgit_repo~reset_status.
  METHODS get_url
    RETURNING
      VALUE(rv_url) TYPE zif_abapgit_persistence=>ty_repo-url .
  METHODS get_selected_branch
    RETURNING
      VALUE(rv_name) TYPE zif_abapgit_persistence=>ty_repo-branch_name .
  METHODS set_url
    IMPORTING
      iv_url TYPE zif_abapgit_persistence=>ty_repo-url
    RAISING
      zcx_abapgit_exception .
  METHODS select_branch
    IMPORTING
      iv_branch_name TYPE zif_abapgit_persistence=>ty_repo-branch_name
    RAISING
      zcx_abapgit_exception .
  METHODS get_selected_commit
    RETURNING
      VALUE(rv_selected_commit) TYPE zif_abapgit_persistence=>ty_repo-selected_commit
    RAISING
      zcx_abapgit_exception .
  METHODS get_current_remote
    RETURNING
      VALUE(rv_sha1) TYPE zif_abapgit_definitions=>ty_sha1
    RAISING
      zcx_abapgit_exception .
  METHODS select_commit
    IMPORTING
      iv_selected_commit TYPE zif_abapgit_persistence=>ty_repo-selected_commit
    RAISING
      zcx_abapgit_exception .
  METHODS get_objects
    RETURNING
      VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_objects_tt
    RAISING
      zcx_abapgit_exception .
  METHODS get_switched_origin
    RETURNING
      VALUE(rv_url) TYPE zif_abapgit_persistence=>ty_repo-switched_origin .
  METHODS switch_origin
    IMPORTING
      iv_url       TYPE zif_abapgit_persistence=>ty_repo-url
      iv_overwrite TYPE abap_bool DEFAULT abap_false
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
