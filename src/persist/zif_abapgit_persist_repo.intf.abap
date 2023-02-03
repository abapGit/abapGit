INTERFACE zif_abapgit_persist_repo
  PUBLIC .


  METHODS add
    IMPORTING
      !iv_url         TYPE string
      !iv_branch_name TYPE string
      !iv_branch      TYPE zif_abapgit_git_definitions=>ty_sha1 OPTIONAL
      iv_display_name TYPE string OPTIONAL
      !iv_package     TYPE devclass
      !iv_offline     TYPE abap_bool DEFAULT abap_false
      !is_dot_abapgit TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit
    RETURNING
      VALUE(rv_key)   TYPE zif_abapgit_persistence=>ty_repo-key
    RAISING
      zcx_abapgit_exception .
  METHODS delete
    IMPORTING
      !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
    RAISING
      zcx_abapgit_exception .
  METHODS exists
    IMPORTING
      !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
    RETURNING
      VALUE(rv_yes) TYPE abap_bool.
  METHODS list
    RETURNING
      VALUE(rt_repos) TYPE zif_abapgit_persistence=>ty_repos
    RAISING
      zcx_abapgit_exception .
  METHODS list_by_keys
    IMPORTING
      it_keys         TYPE zif_abapgit_persistence=>ty_repo_keys
    RETURNING
      VALUE(rt_repos) TYPE zif_abapgit_persistence=>ty_repos
    RAISING
      zcx_abapgit_exception .
  METHODS lock
    IMPORTING
      !iv_mode TYPE enqmode
      !iv_key  TYPE zif_abapgit_persistence=>ty_repo-key
    RAISING
      zcx_abapgit_exception .
  METHODS read
    IMPORTING
      !iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
    RETURNING
      VALUE(rs_repo) TYPE zif_abapgit_persistence=>ty_repo
    RAISING
      zcx_abapgit_exception
      zcx_abapgit_not_found .
  METHODS update_metadata
    IMPORTING
      !iv_key         TYPE zif_abapgit_persistence=>ty_repo-key
      !is_meta        TYPE zif_abapgit_persistence=>ty_repo_xml
      !is_change_mask TYPE zif_abapgit_persistence=>ty_repo_meta_mask
    RAISING
      zcx_abapgit_exception .
ENDINTERFACE.
