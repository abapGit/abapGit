INTERFACE zif_abapgit_repo
  PUBLIC .
  METHODS bind_listener
    IMPORTING
      ii_listener TYPE REF TO zif_abapgit_repo_listener .
  METHODS deserialize_checks
    RETURNING
      VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_deserialize_checks
    RAISING
      zcx_abapgit_exception .
  METHODS delete_checks
    RETURNING
      VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_delete_checks
    RAISING
      zcx_abapgit_exception .
  METHODS get_key
    RETURNING
      VALUE(rv_key) TYPE zif_abapgit_persistence=>ty_value .
  METHODS get_name
    RETURNING
      VALUE(rv_name) TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS get_files_local
    IMPORTING
      ii_log          TYPE REF TO zif_abapgit_log OPTIONAL
      it_filter       TYPE zif_abapgit_definitions=>ty_tadir_tt OPTIONAL
    RETURNING
      VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_item_tt
    RAISING
      zcx_abapgit_exception .
  METHODS get_local_checksums_per_file
    RETURNING
      VALUE(rt_checksums) TYPE zif_abapgit_definitions=>ty_file_signatures_tt .
  METHODS get_files_remote
    RETURNING
      VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_tt
    RAISING
      zcx_abapgit_exception .
  METHODS get_package
    RETURNING
      VALUE(rv_package) TYPE zif_abapgit_persistence=>ty_repo-package .
  METHODS get_dot_abapgit
    RETURNING
      VALUE(ro_dot_abapgit) TYPE REF TO zcl_abapgit_dot_abapgit .
  METHODS set_dot_abapgit
    IMPORTING
      io_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit
    RAISING
      zcx_abapgit_exception .
  METHODS get_dot_apack
    RETURNING
      VALUE(ro_dot_apack) TYPE REF TO zcl_abapgit_apack_reader .
  METHODS deserialize
    IMPORTING
      is_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks
      ii_log    TYPE REF TO zif_abapgit_log
    RAISING
      zcx_abapgit_exception .
  METHODS refresh
    IMPORTING
      iv_drop_cache TYPE abap_bool DEFAULT abap_false
    RAISING
      zcx_abapgit_exception .
  METHODS update_local_checksums
    IMPORTING
      it_files TYPE zif_abapgit_definitions=>ty_file_signatures_tt
    RAISING
      zcx_abapgit_exception .
  METHODS rebuild_local_checksums
    RAISING
      zcx_abapgit_exception .
  METHODS find_remote_dot_abapgit
    RETURNING
      VALUE(ro_dot) TYPE REF TO zcl_abapgit_dot_abapgit
    RAISING
      zcx_abapgit_exception .
  METHODS find_remote_dot_apack
    RETURNING
      VALUE(ro_dot) TYPE REF TO zcl_abapgit_apack_reader
    RAISING
      zcx_abapgit_exception .
  METHODS is_offline
    RETURNING
      VALUE(rv_offline) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS set_files_remote
    IMPORTING
      it_files TYPE zif_abapgit_definitions=>ty_files_tt .
  METHODS get_local_settings
    RETURNING
      VALUE(rs_settings) TYPE zif_abapgit_persistence=>ty_repo-local_settings .
  METHODS set_local_settings
    IMPORTING
      is_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings
    RAISING
      zcx_abapgit_exception .
  METHODS has_remote_source
    RETURNING
      VALUE(rv_yes) TYPE abap_bool .
  METHODS status
    IMPORTING
      ii_log            TYPE REF TO zif_abapgit_log OPTIONAL
    RETURNING
      VALUE(rt_results) TYPE zif_abapgit_definitions=>ty_results_tt
    RAISING
      zcx_abapgit_exception .
  METHODS switch_repo_type
    IMPORTING
      iv_offline TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS create_new_log
    IMPORTING
      iv_title      TYPE string OPTIONAL
    RETURNING
      VALUE(ri_log) TYPE REF TO zif_abapgit_log .
  METHODS get_log
    RETURNING
      VALUE(ri_log) TYPE REF TO zif_abapgit_log .
  METHODS reset_log .
  METHODS refresh_local_object
    IMPORTING
      iv_obj_type TYPE tadir-object
      iv_obj_name TYPE tadir-obj_name
    RAISING
      zcx_abapgit_exception .
  METHODS refresh_local_objects
    RAISING
      zcx_abapgit_exception .
  METHODS reset_status .
ENDINTERFACE.
