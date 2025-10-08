INTERFACE zif_abapgit_persist_user
  PUBLIC .

  TYPES ty_favorites TYPE zif_abapgit_persistence=>ty_repo_keys .

  TYPES:
    BEGIN OF ty_s_user_settings,
      max_lines              TYPE i,
      adt_jump_enabled       TYPE abap_bool,
      show_default_repo      TYPE abap_bool,
      link_hints_enabled     TYPE abap_bool,
      link_hint_key          TYPE c LENGTH 1,
      parallel_proc_disabled TYPE abap_bool,
      icon_scaling           TYPE c LENGTH 1,
      ui_theme               TYPE string,
      hide_sapgui_hint       TYPE abap_bool,
      activate_wo_popup      TYPE abap_bool,
      label_colors           TYPE string,
      default_git_uname      TYPE string,
      default_git_email      TYPE string,
    END OF ty_s_user_settings .
  TYPES:
    BEGIN OF ty_list_settings,
      filter           TYPE string,
      only_favorites   TYPE abap_bool,
      show_details     TYPE abap_bool,
      order_by         TYPE string,
      order_descending TYPE abap_bool,
    END OF ty_list_settings.

  TYPES: BEGIN OF ty_flow_settings,
            only_my_transports  TYPE abap_bool,
            hide_full_matches   TYPE abap_bool,
            hide_matching_files TYPE abap_bool,
            hide_conflicts      TYPE abap_bool,
          END OF ty_flow_settings.

  METHODS get_changes_only
    RETURNING
      VALUE(rv_changes_only) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS get_default_git_user_email
    RETURNING
      VALUE(rv_email) TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS get_default_git_user_name
    RETURNING
      VALUE(rv_username) TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS get_diff_unified
    RETURNING
      VALUE(rv_diff_unified) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS get_favorites
    RETURNING
      VALUE(rt_favorites) TYPE ty_favorites
    RAISING
      zcx_abapgit_exception .
  METHODS get_hide_files
    RETURNING
      VALUE(rv_hide) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS get_show_folders
    RETURNING
      VALUE(rv_folders) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS get_repo_git_user_email
    IMPORTING
      !iv_url         TYPE zif_abapgit_persistence=>ty_repo-url
    RETURNING
      VALUE(rv_email) TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS get_repo_git_user_name
    IMPORTING
      !iv_url            TYPE zif_abapgit_persistence=>ty_repo-url
    RETURNING
      VALUE(rv_username) TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS get_repo_last_change_seen
    IMPORTING
      !iv_url           TYPE zif_abapgit_persistence=>ty_repo-url
    RETURNING
      VALUE(rv_version) TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS get_repo_login
    IMPORTING
      !iv_url         TYPE zif_abapgit_persistence=>ty_repo-url
    RETURNING
      VALUE(rv_login) TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS get_repo_show
    RETURNING
      VALUE(rv_key) TYPE zif_abapgit_persistence=>ty_repo-key
    RAISING
      zcx_abapgit_exception .
  METHODS is_favorite_repo
    IMPORTING
      !iv_repo_key  TYPE zif_abapgit_persistence=>ty_repo-key
    RETURNING
      VALUE(rv_yes) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS set_default_git_user_email
    IMPORTING
      !iv_email TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS set_default_git_user_name
    IMPORTING
      !iv_username TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS set_repo_git_user_email
    IMPORTING
      !iv_url   TYPE zif_abapgit_persistence=>ty_repo-url
      !iv_email TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS set_repo_git_user_name
    IMPORTING
      !iv_url      TYPE zif_abapgit_persistence=>ty_repo-url
      !iv_username TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS set_repo_last_change_seen
    IMPORTING
      !iv_url     TYPE zif_abapgit_persistence=>ty_repo-url
      !iv_version TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS set_repo_login
    IMPORTING
      !iv_url   TYPE zif_abapgit_persistence=>ty_repo-url
      !iv_login TYPE string
    RAISING
      zcx_abapgit_exception .
  METHODS set_repo_show
    IMPORTING
      !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
    RAISING
      zcx_abapgit_exception .
  METHODS toggle_changes_only
    RETURNING
      VALUE(rv_changes_only) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS get_order_by
    RETURNING
      VALUE(rv_order_by) TYPE string
    RAISING
      zcx_abapgit_exception.
  METHODS set_order_by
    IMPORTING
      iv_order_by        TYPE string
    RETURNING
      VALUE(rv_order_by) TYPE string
    RAISING
      zcx_abapgit_exception.
  METHODS get_order_descending
    RETURNING
      VALUE(rv_order_descending) TYPE abap_bool
    RAISING
      zcx_abapgit_exception.
  METHODS set_order_descending
    IMPORTING
      iv_order_descending        TYPE abap_bool
    RETURNING
      VALUE(rv_order_descending) TYPE abap_bool
    RAISING
      zcx_abapgit_exception.
  METHODS get_diff_first
    RETURNING
      VALUE(rv_diff_first) TYPE abap_bool
    RAISING
      zcx_abapgit_exception.
  METHODS set_diff_first
    IMPORTING
      iv_diff_first        TYPE abap_bool
    RETURNING
      VALUE(rv_diff_first) TYPE abap_bool
    RAISING
      zcx_abapgit_exception.
  METHODS toggle_diff_unified
    RETURNING
      VALUE(rv_diff_unified) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS toggle_favorite
    IMPORTING
      !iv_repo_key TYPE zif_abapgit_persistence=>ty_repo-key
    RAISING
      zcx_abapgit_exception .
  METHODS toggle_hide_files
    RETURNING
      VALUE(rv_hide) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
  METHODS toggle_show_folders
    RETURNING
      VALUE(rv_folders) TYPE abap_bool
    RAISING
      zcx_abapgit_exception.
  METHODS get_settings
    RETURNING
      VALUE(rs_user_settings) TYPE ty_s_user_settings
    RAISING
      zcx_abapgit_exception.
  METHODS set_settings
    IMPORTING
      is_user_settings TYPE ty_s_user_settings
    RAISING
      zcx_abapgit_exception.
  METHODS get_list_settings
    RETURNING
      VALUE(rs_list_settings) TYPE ty_list_settings
    RAISING
      zcx_abapgit_exception.
  METHODS set_list_settings
    IMPORTING
      is_list_settings TYPE ty_list_settings
    RAISING
      zcx_abapgit_exception.
  METHODS get_flow_settings
    RETURNING
      VALUE(rs_flow_settings) TYPE ty_flow_settings
    RAISING
      zcx_abapgit_exception.
  METHODS set_flow_settings
    IMPORTING
      is_flow_settings TYPE ty_flow_settings
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
