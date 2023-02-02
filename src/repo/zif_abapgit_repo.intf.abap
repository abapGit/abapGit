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

  METHODS get_files_local
    IMPORTING
      !ii_log         TYPE REF TO zif_abapgit_log OPTIONAL
      !ii_obj_filter  TYPE REF TO zif_abapgit_object_filter OPTIONAL
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

ENDINTERFACE.
