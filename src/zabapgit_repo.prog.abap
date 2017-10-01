*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_REPO
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_repo DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_repo DEFINITION ABSTRACT FRIENDS lcl_repo_srv.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING is_data TYPE lcl_persistence_repo=>ty_repo,
      get_key
        RETURNING VALUE(rv_key) TYPE lcl_persistence_db=>ty_value,
      get_name
        RETURNING VALUE(rv_name) TYPE string
        RAISING   zcx_abapgit_exception,
      get_files_local
        IMPORTING io_log          TYPE REF TO lcl_log OPTIONAL
                  it_filter       TYPE scts_tadir OPTIONAL
        RETURNING VALUE(rt_files) TYPE lif_defs=>ty_files_item_tt
        RAISING   zcx_abapgit_exception,
      get_local_checksums
        RETURNING VALUE(rt_checksums) TYPE lcl_persistence_repo=>ty_local_checksum_tt,
      get_local_checksums_per_file
        RETURNING VALUE(rt_checksums) TYPE lif_defs=>ty_file_signatures_tt,
      get_files_remote
        RETURNING VALUE(rt_files) TYPE lif_defs=>ty_files_tt
        RAISING   zcx_abapgit_exception,
      get_package
        RETURNING VALUE(rv_package) TYPE lcl_persistence_repo=>ty_repo-package,
      get_master_language
        RETURNING VALUE(rv_language) TYPE spras,
      is_write_protected
        RETURNING VALUE(rv_yes) TYPE sap_bool,
      ignore_subpackages
        RETURNING VALUE(rv_yes) TYPE sap_bool,
      delete
        RAISING zcx_abapgit_exception,
      get_dot_abapgit
        RETURNING VALUE(ro_dot_abapgit) TYPE REF TO lcl_dot_abapgit,
      set_dot_abapgit
        IMPORTING io_dot_abapgit TYPE REF TO lcl_dot_abapgit
        RAISING   zcx_abapgit_exception,
      deserialize
        RAISING zcx_abapgit_exception,
      refresh
        IMPORTING iv_drop_cache TYPE abap_bool DEFAULT abap_false
        RAISING   zcx_abapgit_exception,
      refresh_local, " For testing purposes, maybe removed later
      update_local_checksums
        IMPORTING it_files TYPE lif_defs=>ty_file_signatures_tt
        RAISING   zcx_abapgit_exception,
      rebuild_local_checksums
        RAISING zcx_abapgit_exception,
      find_remote_dot_abapgit
        RETURNING VALUE(ro_dot) TYPE REF TO lcl_dot_abapgit
        RAISING   zcx_abapgit_exception,
      is_offline
        RETURNING VALUE(rv_offline) TYPE abap_bool
        RAISING   zcx_abapgit_exception.

  PROTECTED SECTION.
    DATA: mt_local              TYPE lif_defs=>ty_files_item_tt,
          mt_remote             TYPE lif_defs=>ty_files_tt,
          mv_do_local_refresh   TYPE abap_bool,
          mv_last_serialization TYPE timestamp,
          ms_data               TYPE lcl_persistence_repo=>ty_repo.

    METHODS:
      set
        IMPORTING iv_sha1        TYPE lif_defs=>ty_sha1 OPTIONAL
                  it_checksums   TYPE lcl_persistence_repo=>ty_local_checksum_tt OPTIONAL
                  iv_url         TYPE lcl_persistence_repo=>ty_repo-url OPTIONAL
                  iv_branch_name TYPE lcl_persistence_repo=>ty_repo-branch_name OPTIONAL
                  iv_head_branch TYPE lcl_persistence_repo=>ty_repo-head_branch OPTIONAL
                  iv_offline     TYPE lcl_persistence_repo=>ty_repo-offline OPTIONAL
                  is_dot_abapgit TYPE lcl_persistence_repo=>ty_repo-dot_abapgit OPTIONAL
        RAISING   zcx_abapgit_exception.

ENDCLASS.                    "lcl_repo DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_repo_online DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_repo_online DEFINITION INHERITING FROM lcl_repo FINAL.

  PUBLIC SECTION.
    METHODS:
      refresh REDEFINITION,
      constructor
        IMPORTING is_data TYPE lcl_persistence_repo=>ty_repo
        RAISING   zcx_abapgit_exception,
      get_url
        RETURNING VALUE(rv_url) TYPE lcl_persistence_repo=>ty_repo-url,
      get_branch_name
        RETURNING VALUE(rv_name) TYPE lcl_persistence_repo=>ty_repo-branch_name,
      get_head_branch_name
        RETURNING VALUE(rv_name) TYPE lcl_persistence_repo=>ty_repo-head_branch,
      get_branches
        RETURNING VALUE(ro_branches) TYPE REF TO lcl_git_branch_list
        RAISING   zcx_abapgit_exception,
      set_url
        IMPORTING iv_url TYPE lcl_persistence_repo=>ty_repo-url
        RAISING   zcx_abapgit_exception,
      set_branch_name
        IMPORTING iv_branch_name TYPE lcl_persistence_repo=>ty_repo-branch_name
        RAISING   zcx_abapgit_exception,
      set_new_remote
        IMPORTING iv_url         TYPE lcl_persistence_repo=>ty_repo-url
                  iv_branch_name TYPE lcl_persistence_repo=>ty_repo-branch_name
        RAISING   zcx_abapgit_exception,
      get_sha1_local
        RETURNING VALUE(rv_sha1) TYPE lcl_persistence_repo=>ty_repo-sha1,
      get_sha1_remote
        RETURNING VALUE(rv_sha1) TYPE lcl_persistence_repo=>ty_repo-sha1
        RAISING   zcx_abapgit_exception,
      get_files_remote REDEFINITION,
      get_objects
        RETURNING VALUE(rt_objects) TYPE lif_defs=>ty_objects_tt
        RAISING   zcx_abapgit_exception,
      deserialize REDEFINITION,
      status
        IMPORTING io_log            TYPE REF TO lcl_log OPTIONAL
        RETURNING VALUE(rt_results) TYPE lif_defs=>ty_results_tt
        RAISING   zcx_abapgit_exception,
      reset_status,
      rebuild_local_checksums REDEFINITION,
      push
        IMPORTING is_comment TYPE lif_defs=>ty_comment
                  io_stage   TYPE REF TO lcl_stage
        RAISING   zcx_abapgit_exception,
      get_unnecessary_local_objs
        RETURNING VALUE(rt_unnecessary_local_objects) TYPE LIF_DEFS=>TY_TADIR_TT
        RAISING   zcx_abapgit_exception,
      delete_unnecessary_local_objs
        RAISING   zcx_abapgit_exception lcx_cancel.

  PRIVATE SECTION.
    DATA:
      mt_objects     TYPE lif_defs=>ty_objects_tt,
      mv_branch      TYPE lif_defs=>ty_sha1,
      mv_initialized TYPE abap_bool,
      mo_branches    TYPE REF TO lcl_git_branch_list,
      mt_status      TYPE lif_defs=>ty_results_tt.

    METHODS:
      handle_stage_ignore
        IMPORTING io_stage TYPE REF TO lcl_stage
        RAISING   zcx_abapgit_exception,
      initialize
        RAISING zcx_abapgit_exception,
      actualize_head_branch
        RAISING zcx_abapgit_exception,
      delete_initial_online_repo
        IMPORTING iv_commit TYPE flag
        RAISING   zcx_abapgit_exception.

ENDCLASS.                    "lcl_repo_online DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_repo_offline DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_repo_offline DEFINITION INHERITING FROM lcl_repo FINAL.

  PUBLIC SECTION.
    METHODS:
      set_files_remote
        IMPORTING it_files TYPE lif_defs=>ty_files_tt
        RAISING   zcx_abapgit_exception.

ENDCLASS.                    "lcl_repo_offline DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_repo_srv DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_repo_srv DEFINITION FINAL CREATE PRIVATE FRIENDS lcl_app.

  PUBLIC SECTION.

    TYPES: ty_repo_tt TYPE STANDARD TABLE OF REF TO lcl_repo WITH DEFAULT KEY.

    METHODS list
      RETURNING VALUE(rt_list) TYPE ty_repo_tt
      RAISING   zcx_abapgit_exception.

    METHODS refresh
      RAISING zcx_abapgit_exception.

    METHODS new_online
      IMPORTING iv_url         TYPE string
                iv_branch_name TYPE string
                iv_package     TYPE devclass
      RETURNING VALUE(ro_repo) TYPE REF TO lcl_repo_online
      RAISING   zcx_abapgit_exception.

    METHODS new_offline
      IMPORTING iv_url         TYPE string
                iv_package     TYPE devclass
      RETURNING VALUE(ro_repo) TYPE REF TO lcl_repo_offline
      RAISING   zcx_abapgit_exception.

    METHODS delete
      IMPORTING io_repo TYPE REF TO lcl_repo
      RAISING   zcx_abapgit_exception.

    METHODS get
      IMPORTING iv_key         TYPE lcl_persistence_db=>ty_value
      RETURNING VALUE(ro_repo) TYPE REF TO lcl_repo
      RAISING   zcx_abapgit_exception.

    METHODS is_repo_installed
      IMPORTING iv_url              TYPE string
                iv_target_package   TYPE devclass OPTIONAL
      RETURNING VALUE(rv_installed) TYPE abap_bool
      RAISING   zcx_abapgit_exception.

    METHODS switch_repo_type
      IMPORTING iv_key     TYPE lcl_persistence_db=>ty_value
                iv_offline TYPE abap_bool
      RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.

    METHODS constructor.

    DATA: mv_init        TYPE abap_bool VALUE abap_false,
          mo_persistence TYPE REF TO lcl_persistence_repo,
          mt_list        TYPE ty_repo_tt.

    METHODS add
      IMPORTING io_repo TYPE REF TO lcl_repo
      RAISING   zcx_abapgit_exception.

    METHODS validate_package
      IMPORTING iv_package TYPE devclass
      RAISING   zcx_abapgit_exception.

ENDCLASS.                    "lcl_repo_srv DEFINITION
