INTERFACE zif_abapgit_repo_srv
  PUBLIC.

  TYPES:
    ty_repo_list TYPE STANDARD TABLE OF REF TO zcl_abapgit_repo WITH DEFAULT KEY.

  METHODS delete
    IMPORTING
      !io_repo TYPE REF TO zcl_abapgit_repo
    RAISING
      zcx_abapgit_exception .
  METHODS get
    IMPORTING
      !iv_key        TYPE zif_abapgit_persistence=>ty_value
    RETURNING
      VALUE(ro_repo) TYPE REF TO zcl_abapgit_repo
    RAISING
      zcx_abapgit_exception .
  METHODS is_repo_installed
    IMPORTING
      !iv_url             TYPE string
      !iv_target_package  TYPE devclass OPTIONAL
    RETURNING
      VALUE(rv_installed) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS list
    RETURNING
      VALUE(rt_list) TYPE ty_repo_list
    RAISING
      zcx_abapgit_exception .
  METHODS new_offline
    IMPORTING
      !iv_url              TYPE string
      !iv_package          TYPE devclass
      !iv_folder_logic     TYPE string DEFAULT zif_abapgit_dot_abapgit=>c_folder_logic-full
      !iv_master_lang_only TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(ro_repo)       TYPE REF TO zcl_abapgit_repo_offline
    RAISING
      zcx_abapgit_exception .
  METHODS new_online
    IMPORTING
      !iv_url              TYPE string
      !iv_branch_name      TYPE string
      !iv_display_name     TYPE string OPTIONAL
      !iv_package          TYPE devclass
      !iv_folder_logic     TYPE string DEFAULT 'PREFIX'
      !iv_ign_subpkg       TYPE abap_bool DEFAULT abap_false
      !iv_master_lang_only TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(ro_repo)       TYPE REF TO zcl_abapgit_repo_online
    RAISING
      zcx_abapgit_exception .
  METHODS purge
    IMPORTING
      !io_repo   TYPE REF TO zcl_abapgit_repo
      !is_checks TYPE zif_abapgit_definitions=>ty_delete_checks
    RAISING
      zcx_abapgit_exception .
  METHODS validate_package
    IMPORTING
      !iv_package    TYPE devclass
      !iv_ign_subpkg TYPE abap_bool DEFAULT abap_false
      !iv_chk_exists TYPE abap_bool DEFAULT abap_true
    RAISING
      zcx_abapgit_exception .
  METHODS get_repo_from_package
    IMPORTING
      !iv_package    TYPE devclass
      !iv_ign_subpkg TYPE abap_bool DEFAULT abap_false
    EXPORTING
      VALUE(eo_repo) TYPE REF TO zcl_abapgit_repo
      !ev_reason     TYPE string
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
