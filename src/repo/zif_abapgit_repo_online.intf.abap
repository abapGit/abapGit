INTERFACE zif_abapgit_repo_online PUBLIC.

  INTERFACES zif_abapgit_repo.

  ALIASES ms_data
    FOR zif_abapgit_repo~ms_data .
  ALIASES deserialize
    FOR zif_abapgit_repo~deserialize .
  ALIASES deserialize_checks
    FOR zif_abapgit_repo~deserialize_checks .
  ALIASES get_dot_abapgit
    FOR zif_abapgit_repo~get_dot_abapgit .
  ALIASES get_tadir_objects
    FOR zif_abapgit_repo~get_tadir_objects .
  ALIASES get_files_local
    FOR zif_abapgit_repo~get_files_local .
  ALIASES get_files_local_filtered
    FOR zif_abapgit_repo~get_files_local_filtered .
  ALIASES get_files_remote
    FOR zif_abapgit_repo~get_files_remote .
  ALIASES get_key
    FOR zif_abapgit_repo~get_key .
  ALIASES get_local_settings
    FOR zif_abapgit_repo~get_local_settings .
  ALIASES get_name
    FOR zif_abapgit_repo~get_name .
  ALIASES get_package
    FOR zif_abapgit_repo~get_package .
  ALIASES is_offline
    FOR zif_abapgit_repo~is_offline .
  ALIASES refresh
    FOR zif_abapgit_repo~refresh .
  ALIASES set_dot_abapgit
    FOR zif_abapgit_repo~set_dot_abapgit .
  ALIASES find_remote_dot_abapgit
    FOR zif_abapgit_repo~find_remote_dot_abapgit .
  ALIASES has_remote_source
    FOR zif_abapgit_repo~has_remote_source .

  METHODS get_url
    RETURNING
      VALUE(rv_url) TYPE zif_abapgit_persistence=>ty_repo-url .
  METHODS get_selected_branch
    RETURNING
      VALUE(rv_name) TYPE zif_abapgit_persistence=>ty_repo-branch_name .
  METHODS set_url
    IMPORTING
      !iv_url TYPE zif_abapgit_persistence=>ty_repo-url
    RAISING
      zcx_abapgit_exception .
  METHODS select_branch
    IMPORTING
      !iv_branch_name TYPE zif_abapgit_persistence=>ty_repo-branch_name
    RAISING
      zcx_abapgit_exception .
  METHODS get_selected_commit
    RETURNING
      VALUE(rv_selected_commit) TYPE zif_abapgit_persistence=>ty_repo-selected_commit
    RAISING
      zcx_abapgit_exception .
  METHODS get_current_remote
    RETURNING
      VALUE(rv_sha1) TYPE zif_abapgit_git_definitions=>ty_sha1
    RAISING
      zcx_abapgit_exception .
  METHODS select_commit
    IMPORTING
      !iv_selected_commit TYPE zif_abapgit_persistence=>ty_repo-selected_commit
    RAISING
      zcx_abapgit_exception .
  METHODS switch_origin
    IMPORTING
      !iv_url       TYPE zif_abapgit_persistence=>ty_repo-url
      !iv_branch    TYPE zif_abapgit_persistence=>ty_repo-branch_name OPTIONAL
      !iv_overwrite TYPE abap_bool DEFAULT abap_false
    RAISING
      zcx_abapgit_exception .
  METHODS get_switched_origin
    RETURNING
      VALUE(rv_switched_origin) TYPE zif_abapgit_persistence=>ty_repo-switched_origin.
  METHODS push
    IMPORTING
      !is_comment TYPE zif_abapgit_git_definitions=>ty_comment
      !io_stage   TYPE REF TO zcl_abapgit_stage
    RAISING
      zcx_abapgit_exception .
  METHODS create_branch
    IMPORTING
      !iv_name TYPE string
      !iv_from TYPE zif_abapgit_git_definitions=>ty_sha1 OPTIONAL
    RAISING
      zcx_abapgit_exception .
  METHODS check_for_valid_branch
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
