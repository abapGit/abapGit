REPORT zabapgit LINE-SIZE 100.

* See http://www.abapgit.org

CONSTANTS: gc_xml_version  TYPE string VALUE 'v1.0.0',      "#EC NOTEXT
           gc_abap_version TYPE string VALUE 'v1.12.6'.     "#EC NOTEXT

********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2014 abapGit Contributors
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
********************************************************************************

TYPE-POOLS seop.

TYPES: ty_type    TYPE c LENGTH 6,
       ty_bitbyte TYPE c LENGTH 8,
       ty_sha1    TYPE c LENGTH 40.

TYPES: BEGIN OF ty_file,
         path     TYPE string,
         filename TYPE string,
         data     TYPE xstring,
       END OF ty_file.
TYPES: ty_files_tt TYPE STANDARD TABLE OF ty_file WITH DEFAULT KEY.

TYPES: ty_string_tt TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
TYPES: tt_w3urls    TYPE STANDARD TABLE OF w3url  WITH DEFAULT KEY.

TYPES: BEGIN OF ty_comment,
         username TYPE string,
         email    TYPE string,
         comment  TYPE string,
       END OF ty_comment.

TYPES: BEGIN OF ty_item,
         obj_type TYPE tadir-object,
         obj_name TYPE tadir-obj_name,
       END OF ty_item.

TYPES: BEGIN OF ty_file_item,
         file TYPE ty_file,
         item TYPE ty_item,
       END OF ty_file_item.
TYPES: ty_files_item_tt TYPE STANDARD TABLE OF ty_file_item WITH DEFAULT KEY.

TYPES: BEGIN OF ty_metadata,
         class      TYPE string,
         version    TYPE string,
         late_deser TYPE string,
       END OF ty_metadata.

TYPES: BEGIN OF ty_web_asset,
         url     TYPE w3url,
         content TYPE string,
       END OF ty_web_asset.
TYPES  tt_web_assets TYPE STANDARD TABLE OF ty_web_asset WITH DEFAULT KEY.

TYPES: BEGIN OF ty_repo_file,
         path       TYPE string,
         filename   TYPE string,
         is_changed TYPE abap_bool,
       END OF ty_repo_file.
TYPES  tt_repo_files TYPE STANDARD TABLE OF ty_repo_file WITH DEFAULT KEY.

CONSTANTS: BEGIN OF gc_type,
             commit TYPE ty_type VALUE 'commit',            "#EC NOTEXT
             tree   TYPE ty_type VALUE 'tree',              "#EC NOTEXT
             ref_d  TYPE ty_type VALUE 'ref_d',             "#EC NOTEXT
             blob   TYPE ty_type VALUE 'blob',              "#EC NOTEXT
           END OF gc_type.

TYPES: ty_chmod TYPE c LENGTH 6.

TYPES: BEGIN OF ty_object,
         sha1 TYPE ty_sha1,
         type TYPE ty_type,
         data TYPE xstring,
       END OF ty_object.
TYPES: ty_objects_tt TYPE STANDARD TABLE OF ty_object WITH DEFAULT KEY.

TYPES: BEGIN OF ty_tadir,
         pgmid    TYPE tadir-pgmid,
         object   TYPE tadir-object,
         obj_name TYPE tadir-obj_name,
         devclass TYPE tadir-devclass,
         korrnum  TYPE tadir-korrnum,
         path     TYPE string,
       END OF ty_tadir.
TYPES: ty_tadir_tt TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY.

TYPES: BEGIN OF ty_result,
         obj_type TYPE tadir-object,
         obj_name TYPE tadir-obj_name,
         match    TYPE sap_bool,
         filename TYPE string,
         package  TYPE devclass,
         path     TYPE string,
       END OF ty_result.
TYPES: ty_results_tt TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.

CONSTANTS: BEGIN OF gc_chmod,
             file       TYPE ty_chmod VALUE '100644',
             executable TYPE ty_chmod VALUE '100755',
             dir        TYPE ty_chmod VALUE '40000 ',
           END OF gc_chmod.

CONSTANTS: BEGIN OF gc_event_state,
             not_handled         VALUE 0,
             re_render           VALUE 1,
             new_page            VALUE 2,
             go_back             VALUE 3,
             no_more_act         VALUE 4,
             new_page_w_bookmark VALUE 5,
             go_back_to_bookmark VALUE 6,
           END OF gc_event_state.

CONSTANTS: BEGIN OF gc_html_opt,
             emphas TYPE c VALUE 'E',
             cancel TYPE c VALUE 'C',
           END OF gc_html_opt.

CONSTANTS: BEGIN OF gc_action_type,
             sapevent TYPE c VALUE 'E',
             url      TYPE c VALUE 'U',
             onclick  TYPE c VALUE 'C',
           END OF gc_action_type.

CONSTANTS: gc_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>newline.

CONSTANTS: gc_english TYPE spras VALUE 'E'.

CONSTANTS: gc_abapgit_homepage TYPE string VALUE 'http://www.abapgit.org'.

CONSTANTS: gc_root_dir    TYPE string VALUE '/',
           gc_dot_abapgit TYPE string VALUE '.abapgit.xml'.

DEFINE _raise.
  RAISE EXCEPTION TYPE lcx_exception
    EXPORTING
      iv_text = &1.                                         "#EC NOTEXT
END-OF-DEFINITION.

******************

SELECTION-SCREEN BEGIN OF SCREEN 1001.
* dummy for triggering screen
SELECTION-SCREEN END OF SCREEN 1001.

******************
INCLUDE zabapgit_exceptions.
INCLUDE zabapgit_zlib.
INCLUDE zabapgit_util.
INCLUDE zabapgit_xml.

CLASS lcl_gui DEFINITION DEFERRED.
CLASS lcl_persistence_user DEFINITION DEFERRED.
CLASS lcl_repo_srv DEFINITION DEFERRED.
CLASS lcl_persistence_db DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*       CLASS lcl_app DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_app DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-METHODS run
      RAISING lcx_exception.

    CLASS-METHODS gui
      RETURNING VALUE(ro_gui) TYPE REF TO lcl_gui
      RAISING   lcx_exception.

    CLASS-METHODS user
      IMPORTING iv_user        TYPE xubname DEFAULT sy-uname
      RETURNING VALUE(ro_user) TYPE REF TO lcl_persistence_user
      RAISING   lcx_exception.

    CLASS-METHODS repo_srv
      RETURNING VALUE(ro_repo_srv) TYPE REF TO lcl_repo_srv.

    CLASS-METHODS db
      RETURNING VALUE(ro_db) TYPE REF TO lcl_persistence_db.

  PRIVATE SECTION.
    CLASS-DATA: go_gui          TYPE REF TO lcl_gui,
                go_current_user TYPE REF TO lcl_persistence_user,
                go_db           TYPE REF TO lcl_persistence_db,
                go_repo_srv     TYPE REF TO lcl_repo_srv.

ENDCLASS.   "lcl_app

INCLUDE zabapgit_persistence.
INCLUDE zabapgit_html.
INCLUDE zabapgit_dot_abapgit.
INCLUDE zabapgit_sap_package.

CLASS lcl_repo_online DEFINITION DEFERRED.

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

INCLUDE zabapgit_repo.
INCLUDE zabapgit_stage.
INCLUDE zabapgit_git.
INCLUDE zabapgit_objects.
INCLUDE zabapgit_tadir.
INCLUDE zabapgit_file_status.
INCLUDE zabapgit_object.
INCLUDE zabapgit_repo_impl.
INCLUDE zabapgit_background.
INCLUDE zabapgit_zip.
INCLUDE zabapgit_gui.
INCLUDE zabapgit_app_impl.
INCLUDE zabapgit_unit_test.
INCLUDE zabapgit_forms.

START-OF-SELECTION.
  PERFORM run.

* Hide Execute button from screen
AT SELECTION-SCREEN OUTPUT.
  PERFORM output.

* SAP back command re-direction
AT SELECTION-SCREEN ON EXIT-COMMAND.
  PERFORM exit.