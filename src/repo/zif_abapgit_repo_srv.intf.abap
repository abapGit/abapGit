INTERFACE zif_abapgit_repo_srv
  PUBLIC .


  TYPES:
    ty_repo_list TYPE STANDARD TABLE OF REF TO zif_abapgit_repo WITH DEFAULT KEY,

    BEGIN OF ty_label,
      label TYPE string,
    END OF ty_label,
    ty_labels TYPE STANDARD TABLE OF ty_label WITH NON-UNIQUE DEFAULT KEY
                   WITH NON-UNIQUE SORTED KEY key_label COMPONENTS label.

  METHODS init.
  METHODS delete
    IMPORTING
      !ii_repo TYPE REF TO zif_abapgit_repo
    RAISING
      zcx_abapgit_exception .
  METHODS get
    IMPORTING
      !iv_key        TYPE zif_abapgit_persistence=>ty_value
    RETURNING
      VALUE(ri_repo) TYPE REF TO zif_abapgit_repo
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
  METHODS list_favorites
    RETURNING
      VALUE(rt_list) TYPE ty_repo_list
    RAISING
      zcx_abapgit_exception .
  METHODS new_offline
    IMPORTING
      !iv_url            TYPE string
      !iv_package        TYPE devclass
      !iv_folder_logic   TYPE string DEFAULT zif_abapgit_dot_abapgit=>c_folder_logic-full
      !iv_labels         TYPE string OPTIONAL
      !iv_ign_subpkg     TYPE abap_bool DEFAULT abap_false
      !iv_main_lang_only TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(ri_repo)     TYPE REF TO zif_abapgit_repo
    RAISING
      zcx_abapgit_exception .
  METHODS new_online
    IMPORTING
      !iv_url            TYPE string
      !iv_branch_name    TYPE string OPTIONAL
      !iv_display_name   TYPE string OPTIONAL
      !iv_package        TYPE devclass
      !iv_folder_logic   TYPE string DEFAULT zif_abapgit_dot_abapgit=>c_folder_logic-prefix
      !iv_labels         TYPE string OPTIONAL
      !iv_ign_subpkg     TYPE abap_bool DEFAULT abap_false
      !iv_main_lang_only TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(ri_repo)     TYPE REF TO zif_abapgit_repo
    RAISING
      zcx_abapgit_exception .
  METHODS purge
    IMPORTING
      !ii_repo      TYPE REF TO zif_abapgit_repo
      !is_checks    TYPE zif_abapgit_definitions=>ty_delete_checks
    RETURNING
      VALUE(ri_log) TYPE REF TO zif_abapgit_log
    RAISING
      zcx_abapgit_exception .
  METHODS validate_package
    IMPORTING
      !iv_package    TYPE devclass
      !iv_ign_subpkg TYPE abap_bool DEFAULT abap_false
      !iv_chk_exists TYPE abap_bool DEFAULT abap_true
    RAISING
      zcx_abapgit_exception .
  METHODS validate_url
    IMPORTING
      !iv_url        TYPE string
      !iv_chk_exists TYPE abap_bool DEFAULT abap_true
    RAISING
      zcx_abapgit_exception .
  METHODS get_repo_from_package
    IMPORTING
      !iv_package    TYPE devclass
      !iv_ign_subpkg TYPE abap_bool DEFAULT abap_false
    EXPORTING
      VALUE(ei_repo) TYPE REF TO zif_abapgit_repo
      !ev_reason     TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS get_repo_from_url
    IMPORTING
      !iv_url    TYPE string
    EXPORTING
      !ei_repo   TYPE REF TO zif_abapgit_repo
      !ev_reason TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS get_label_list
    RETURNING
      VALUE(rt_labels) TYPE ty_labels
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
