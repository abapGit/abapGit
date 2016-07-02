*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_STAGE
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_stage DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_stage DEFINITION FINAL CREATE PRIVATE FRIENDS lcl_repo_srv.

  PUBLIC SECTION.
    TYPES: ty_method TYPE c LENGTH 1.

    CONSTANTS: BEGIN OF c_method,
                 add    TYPE ty_method VALUE 'A',
                 rm     TYPE ty_method VALUE 'R',
                 ignore TYPE ty_method VALUE 'I',
               END OF c_method.

    CONSTANTS: BEGIN OF c_wftype,
                 local  TYPE char1 VALUE 'L',
                 remote TYPE char1 VALUE 'R',
               END OF c_wftype.

    TYPES: BEGIN OF ty_stage,
             file   TYPE ty_file,
             method TYPE ty_method,
           END OF ty_stage.

    TYPES: ty_stage_tt TYPE SORTED TABLE OF ty_stage
      WITH UNIQUE KEY file-path file-filename.

    TYPES: BEGIN OF ty_work_file,
             type TYPE char1,
             file TYPE ty_file,
           END OF ty_work_file.

    DATA mv_repo_key  TYPE lcl_persistence_db=>ty_value READ-ONLY.
    DATA mv_local_cnt TYPE i READ-ONLY.
    DATA mt_workarea  TYPE STANDARD TABLE OF ty_work_file READ-ONLY.

    CLASS-METHODS method_description
      IMPORTING iv_method             TYPE ty_method
      RETURNING VALUE(rv_description) TYPE string
      RAISING   lcx_exception.

    METHODS constructor
      IMPORTING iv_repo_key TYPE lcl_persistence_db=>ty_value
      RAISING   lcx_exception.

    METHODS update_and_add_dot_abapgit
      IMPORTING iv_data TYPE ty_file-data
      RAISING   lcx_exception.

    METHODS:
      add
        IMPORTING iv_path     TYPE ty_file-path
                  iv_filename TYPE ty_file-filename
        RAISING   lcx_exception,
      reset
        IMPORTING iv_path     TYPE ty_file-path
                  iv_filename TYPE ty_file-filename
        RAISING   lcx_exception,
      rm
        IMPORTING iv_path     TYPE ty_file-path
                  iv_filename TYPE ty_file-filename
        RAISING   lcx_exception,
      ignore
        IMPORTING iv_path     TYPE ty_file-path
                  iv_filename TYPE ty_file-filename
        RAISING   lcx_exception,
      lookup
        IMPORTING iv_path          TYPE ty_file-path
                  iv_filename      TYPE ty_file-filename
        RETURNING VALUE(rv_method) TYPE ty_method,
      count
        RETURNING VALUE(rv_count) TYPE i,
      get_all
        RETURNING VALUE(rt_stage) TYPE ty_stage_tt.

  PRIVATE SECTION.
    DATA: mt_stage TYPE ty_stage_tt.

    METHODS append
      IMPORTING iv_path     TYPE ty_file-path
                iv_filename TYPE ty_file-filename
                iv_method   TYPE ty_method
      RAISING   lcx_exception.

    METHODS find_work_file
      IMPORTING iv_path        TYPE ty_file-path
                iv_filename    TYPE ty_file-filename
      RETURNING VALUE(rs_file) TYPE ty_file
      RAISING   lcx_exception.

ENDCLASS.   "lcl_stage DEFINITION