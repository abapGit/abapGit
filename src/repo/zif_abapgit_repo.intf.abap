INTERFACE zif_abapgit_repo
  PUBLIC .

  DATA ms_data TYPE zif_abapgit_persistence=>ty_repo READ-ONLY.

  METHODS get_key
    RETURNING
      VALUE(rv_key) TYPE zif_abapgit_persistence=>ty_value .
  METHODS get_name
    RETURNING
      VALUE(rv_name) TYPE string.
  METHODS is_offline
    RETURNING
      VALUE(rv_offline) TYPE abap_bool .
  METHODS get_package
    RETURNING
      VALUE(rv_package) TYPE zif_abapgit_persistence=>ty_repo-package .
  METHODS get_local_settings
    RETURNING
      VALUE(rs_settings) TYPE zif_abapgit_persistence=>ty_repo-local_settings .

  METHODS get_tadir_objects
    RETURNING
      VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
    RAISING
      zcx_abapgit_exception .

  METHODS get_files_local_filtered
    IMPORTING
      !ii_obj_filter  TYPE REF TO zif_abapgit_object_filter
      !ii_log         TYPE REF TO zif_abapgit_log OPTIONAL
    RETURNING
      VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_item_tt
    RAISING
      zcx_abapgit_exception .

  METHODS get_files_local
    IMPORTING
      !ii_log         TYPE REF TO zif_abapgit_log OPTIONAL
    RETURNING
      VALUE(rt_files) TYPE zif_abapgit_definitions=>ty_files_item_tt
    RAISING
      zcx_abapgit_exception .

  METHODS get_files_remote
    IMPORTING
      !ii_obj_filter   TYPE REF TO zif_abapgit_object_filter OPTIONAL
      !iv_ignore_files TYPE abap_bool DEFAULT abap_false
        PREFERRED PARAMETER ii_obj_filter
    RETURNING
      VALUE(rt_files)  TYPE zif_abapgit_git_definitions=>ty_files_tt
    RAISING
      zcx_abapgit_exception .
  METHODS refresh
    IMPORTING
      !iv_drop_cache TYPE abap_bool DEFAULT abap_false
      !iv_drop_log   TYPE abap_bool DEFAULT abap_true
        PREFERRED PARAMETER iv_drop_cache
    RAISING
      zcx_abapgit_exception .
  METHODS get_dot_abapgit
    RETURNING
      VALUE(ro_dot_abapgit) TYPE REF TO zcl_abapgit_dot_abapgit .
  METHODS set_dot_abapgit
    IMPORTING
      !io_dot_abapgit TYPE REF TO zcl_abapgit_dot_abapgit
    RAISING
      zcx_abapgit_exception .
  METHODS find_remote_dot_abapgit
    RETURNING
      VALUE(ro_dot) TYPE REF TO zcl_abapgit_dot_abapgit
    RAISING
      zcx_abapgit_exception .
  METHODS deserialize
    IMPORTING
      !is_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks
      !ii_log    TYPE REF TO zif_abapgit_log
    RAISING
      zcx_abapgit_exception .
  METHODS deserialize_checks
    RETURNING
      VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_deserialize_checks
    RAISING
      zcx_abapgit_exception .

  METHODS checksums
    RETURNING
      VALUE(ri_checksums) TYPE REF TO zif_abapgit_repo_checksums
    RAISING
      zcx_abapgit_exception .

  METHODS has_remote_source
    RETURNING
      VALUE(rv_yes) TYPE abap_bool .
  METHODS get_log
    RETURNING
      VALUE(ri_log) TYPE REF TO zif_abapgit_log .
  METHODS create_new_log
    IMPORTING
      iv_title      TYPE string OPTIONAL
    RETURNING
      VALUE(ri_log) TYPE REF TO zif_abapgit_log .
  METHODS get_dot_apack
    RETURNING
      VALUE(ro_dot_apack) TYPE REF TO zcl_abapgit_apack_reader
    RAISING
      zcx_abapgit_exception.
  METHODS delete_checks
    RETURNING
      VALUE(rs_checks) TYPE zif_abapgit_definitions=>ty_delete_checks
    RAISING
      zcx_abapgit_exception .
  METHODS set_files_remote
    IMPORTING
      it_files TYPE zif_abapgit_git_definitions=>ty_files_tt .
  METHODS get_unsupported_objects_local
    RETURNING
      VALUE(rt_objects) TYPE zif_abapgit_definitions=>ty_items_tt
    RAISING
      zcx_abapgit_exception .
  METHODS set_local_settings
    IMPORTING
      is_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings
    RAISING
      zcx_abapgit_exception .
  METHODS switch_repo_type
    IMPORTING
      iv_offline TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS refresh_local_object
    IMPORTING
      iv_obj_type TYPE tadir-object
      iv_obj_name TYPE tadir-obj_name
    RAISING
      zcx_abapgit_exception .
  METHODS refresh_local_objects
    RAISING
      zcx_abapgit_exception .
  METHODS get_data_config
    RETURNING
      VALUE(ri_config) TYPE REF TO zif_abapgit_data_config
    RAISING
      zcx_abapgit_exception .
  METHODS bind_listener
    IMPORTING
      ii_listener TYPE REF TO zif_abapgit_repo_listener .
  METHODS remove_ignored_files
    CHANGING
      ct_files TYPE zif_abapgit_git_definitions=>ty_files_tt
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
