INTERFACE zif_abapgit_persist_user
  PUBLIC .

  TYPES tt_favorites TYPE zif_abapgit_persistence=>tt_repo_keys .

  METHODS get_show_order_by
    RETURNING
      VALUE(rv_show_order_by) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .
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
      VALUE(rt_favorites) TYPE tt_favorites
    RAISING
      zcx_abapgit_exception .
  METHODS get_hide_files
    RETURNING
      VALUE(rv_hide) TYPE abap_bool
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
  METHODS get_settings
    RETURNING
      VALUE(rs_user_settings) TYPE zif_abapgit_definitions=>ty_s_user_settings
    RAISING
      zcx_abapgit_exception.
  METHODS set_settings
    IMPORTING
      is_user_settings TYPE zif_abapgit_definitions=>ty_s_user_settings
    RAISING
      zcx_abapgit_exception.
  METHODS toggle_show_order_by
    RETURNING
      VALUE(rv_show_order_by) TYPE abap_bool
    RAISING
      zcx_abapgit_exception .


ENDINTERFACE.
