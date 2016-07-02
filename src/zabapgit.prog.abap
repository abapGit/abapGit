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

CLASS lcl_stage IMPLEMENTATION.

  METHOD constructor.
    DATA: ls_files     TYPE lcl_stage_logic=>ty_stage_files,
          lo_repo      TYPE REF TO lcl_repo_online,
          ls_work_file LIKE LINE OF mt_workarea.

    FIELD-SYMBOLS: <ls_local>  LIKE LINE OF ls_files-local,
                   <ls_remote> LIKE LINE OF ls_files-remote.

    mv_repo_key = iv_repo_key.
    lo_repo    ?= lcl_app=>repo_srv( )->get( iv_repo_key ).
    ls_files    = lcl_stage_logic=>get( lo_repo ).

    " Unify structures
    LOOP AT ls_files-local ASSIGNING <ls_local>.
      ls_work_file-type     = c_wftype-local.
      ls_work_file-file     = <ls_local>-file.
      APPEND ls_work_file TO mt_workarea.
      mv_local_cnt = mv_local_cnt + 1.
    ENDLOOP.

    LOOP AT ls_files-remote ASSIGNING <ls_remote>.
      ls_work_file-type     = c_wftype-remote.
      ls_work_file-file     = <ls_remote>.
      APPEND ls_work_file TO mt_workarea.
    ENDLOOP.

  ENDMETHOD.        "constructor

  METHOD lookup.
    DATA ls_stage LIKE LINE OF mt_stage.

    READ TABLE mt_stage INTO ls_stage
      WITH KEY file-path     = iv_path
               file-filename = iv_filename.
    IF sy-subrc = 0.
      rv_method = ls_stage-method.
    ENDIF.

  ENDMETHOD.        "lookup

  METHOD get_all.
    rt_stage = mt_stage.
  ENDMETHOD.        "get_all

  METHOD append.

    DATA: ls_stage LIKE LINE OF mt_stage,
          ls_file  TYPE ty_file.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF mt_stage.

    ls_file = find_work_file( iv_path = iv_path iv_filename = iv_filename ).

    READ TABLE mt_stage WITH KEY
      file-path     = ls_file-path
      file-filename = ls_file-filename
      ASSIGNING <ls_stage>.
    IF sy-subrc = 0.
      <ls_stage>-file-data = ls_file-data.
      <ls_stage>-method    = iv_method.
    ELSE.
      ls_stage-file   = ls_file.
      ls_stage-method = iv_method.
      INSERT ls_stage INTO TABLE mt_stage.
    ENDIF.

  ENDMETHOD.        "append

  METHOD method_description.

    CASE iv_method.
      WHEN c_method-add.
        rv_description = 'add'.
      WHEN c_method-rm.
        rv_description = 'rm'.
      WHEN c_method-ignore.
        rv_description = 'ignore' ##NO_TEXT.
      WHEN OTHERS.
        _raise 'unknown staging method type'.
    ENDCASE.

  ENDMETHOD.        "method_description

  METHOD add.
    append( iv_path     = iv_path
            iv_filename = iv_filename
            iv_method   = c_method-add ).
  ENDMETHOD.        "add

  METHOD reset.
    DELETE mt_stage WHERE file-path     = iv_path
                    AND   file-filename = iv_filename.
    ASSERT sy-subrc = 0.
  ENDMETHOD.        "reset

  METHOD rm.
    append( iv_path     = iv_path
            iv_filename = iv_filename
            iv_method   = c_method-rm ).
  ENDMETHOD.        "rm

  METHOD ignore.
    append( iv_path     = iv_path
            iv_filename = iv_filename
            iv_method   = c_method-ignore ).
  ENDMETHOD.        "ignore

  METHOD count.
    rv_count = lines( mt_stage ).
  ENDMETHOD.        "count

  METHOD find_work_file.
    DATA ls_work_file LIKE LINE OF mt_workarea.

    READ TABLE mt_workarea INTO ls_work_file
      WITH KEY file-path     = iv_path
               file-filename = iv_filename.
    IF sy-subrc = 0.
      rs_file = ls_work_file-file.
    ELSE.
      _raise 'File not found in workarea'.
    ENDIF.

  ENDMETHOD.        "check_work_file_exists

  METHOD update_and_add_dot_abapgit.

    FIELD-SYMBOLS <ls_dot_abapgit> LIKE LINE OF mt_workarea.

    READ TABLE mt_workarea ASSIGNING <ls_dot_abapgit>
      WITH KEY file-path     = gc_root_dir
               file-filename = gc_dot_abapgit.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO mt_workarea ASSIGNING <ls_dot_abapgit>.
      <ls_dot_abapgit>-type          = c_wftype-local.
      <ls_dot_abapgit>-file-path     = gc_root_dir.
      <ls_dot_abapgit>-file-filename = gc_dot_abapgit.
    ENDIF.

    <ls_dot_abapgit>-file-data = iv_data.

    add( iv_path     = gc_root_dir
         iv_filename = gc_dot_abapgit ).

  ENDMETHOD.        "update_and_add_dot_abapgit

ENDCLASS.

INCLUDE zabapgit_git.

START-OF-SELECTION.
  PERFORM run.

*----------------------------------------------------------------------*
*       CLASS lcl_tadir DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_tadir DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_tadir,
             pgmid    TYPE tadir-pgmid,
             object   TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             devclass TYPE tadir-devclass,
             korrnum  TYPE tadir-korrnum,
             path     TYPE string,
           END OF ty_tadir.
    TYPES: ty_tadir_tt TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY.

    CLASS-METHODS:
      read
        IMPORTING iv_package      TYPE tadir-devclass
        RETURNING VALUE(rt_tadir) TYPE ty_tadir_tt
        RAISING   lcx_exception,
      read_single
        IMPORTING iv_pgmid        TYPE tadir-pgmid DEFAULT 'R3TR'
                  iv_object       TYPE tadir-object
                  iv_obj_name     TYPE tadir-obj_name
        RETURNING VALUE(rs_tadir) TYPE tadir.

  PRIVATE SECTION.
    CLASS-METHODS:
      check_exists
        IMPORTING it_tadir        TYPE ty_tadir_tt
        RETURNING VALUE(rt_tadir) TYPE ty_tadir_tt
        RAISING   lcx_exception,
      build
        IMPORTING iv_package      TYPE tadir-devclass
                  iv_parent       TYPE tadir-devclass
                  iv_path         TYPE string
        RETURNING VALUE(rt_tadir) TYPE ty_tadir_tt
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_tadir DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_activation DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_activation DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS add
      IMPORTING iv_type TYPE trobjtype
                iv_name TYPE clike
      RAISING   lcx_exception.

    CLASS-METHODS add_item
      IMPORTING is_item TYPE ty_item
      RAISING   lcx_exception.

    CLASS-METHODS activate
      RAISING lcx_exception.

    CLASS-METHODS clear.

  PRIVATE SECTION.
    CLASS-DATA: gt_ddic     TYPE TABLE OF dwinactiv,
                gt_programs TYPE TABLE OF dwinactiv.

ENDCLASS.                    "lcl_objects_activation DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_activation IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_activation IMPLEMENTATION.

  METHOD add_item.
    add( iv_type = is_item-obj_type
         iv_name = is_item-obj_name ).
  ENDMETHOD.                    "add_item

  METHOD clear.
    CLEAR: gt_ddic,
           gt_programs.
  ENDMETHOD.                    "clear

  METHOD activate.

* ddic
    IF NOT gt_ddic IS INITIAL.
      CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
        EXPORTING
          activate_ddic_objects  = abap_true
          with_popup             = abap_true
        TABLES
          objects                = gt_ddic
        EXCEPTIONS
          excecution_error       = 1
          cancelled              = 2
          insert_into_corr_error = 3
          OTHERS                 = 4.
      IF sy-subrc <> 0.
        _raise 'error from RS_WORKING_OBJECTS_ACTIVATE'.
      ENDIF.
    ENDIF.

* programs
    IF NOT gt_programs IS INITIAL.
      CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
        EXPORTING
          activate_ddic_objects  = abap_false
          with_popup             = abap_true
        TABLES
          objects                = gt_programs
        EXCEPTIONS
          excecution_error       = 1
          cancelled              = 2
          insert_into_corr_error = 3
          OTHERS                 = 4.
      IF sy-subrc <> 0.
        _raise 'error from RS_WORKING_OBJECTS_ACTIVATE'.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "activate

  METHOD add.

* function group SEWORKINGAREA
* function module RS_INSERT_INTO_WORKING_AREA
* class CL_WB_ACTIVATION_WORK_AREA

    DATA: lt_objects  TYPE dwinactiv_tab,
          lv_obj_name TYPE dwinactiv-obj_name.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.


    lv_obj_name = iv_name.

* todo, refactoring
    CASE iv_type.
      WHEN 'CLAS' OR 'WDYN'.
        CALL FUNCTION 'RS_INACTIVE_OBJECTS_IN_OBJECT'
          EXPORTING
            obj_name         = lv_obj_name
            object           = iv_type
          TABLES
            inactive_objects = lt_objects
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc <> 0.
          _raise 'Error from RS_INACTIVE_OBJECTS_IN_OBJECT'.
        ENDIF.

        APPEND LINES OF lt_objects TO gt_programs.
      WHEN 'DOMA' OR 'DTEL' OR 'TABL' OR 'INDX' OR 'TTYP'
        OR 'VIEW' OR 'SHLP' OR 'ENQU'
        OR 'SFSW' OR 'SFBF' OR 'SFBS'.
* todo also insert_into_working_area?
        APPEND INITIAL LINE TO gt_ddic ASSIGNING <ls_object>.
        <ls_object>-object   = iv_type.
        <ls_object>-obj_name = lv_obj_name.
      WHEN 'REPS' OR 'DYNP' OR 'CUAD' OR 'REPT' OR 'INTF'
          OR 'FUNC' OR 'ENHO' OR 'TYPE' OR 'XSLT'.
* these seem to go into the workarea automatically
        APPEND INITIAL LINE TO gt_programs ASSIGNING <ls_object>.
        <ls_object>-object   = iv_type.
        <ls_object>-obj_name = lv_obj_name.
      WHEN OTHERS.
        _raise 'activate, unknown type'.
    ENDCASE.

  ENDMETHOD.                    "activate

ENDCLASS.                    "lcl_objects_activation IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_files DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_files DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING is_item TYPE ty_item,
      add_string
        IMPORTING iv_extra  TYPE clike OPTIONAL
                  iv_ext    TYPE string
                  iv_string TYPE string
        RAISING   lcx_exception,
      read_string
        IMPORTING iv_extra         TYPE clike OPTIONAL
                  iv_ext           TYPE string
        RETURNING VALUE(rv_string) TYPE string
        RAISING   lcx_exception,
      add_xml
        IMPORTING iv_extra     TYPE clike OPTIONAL
                  io_xml       TYPE REF TO lcl_xml_output
                  iv_normalize TYPE sap_bool DEFAULT abap_true
                  is_metadata  TYPE ty_metadata OPTIONAL
        RAISING   lcx_exception,
* needed since type-check during dynamic call fails even if the object is compatible
      add_xml_from_plugin
        IMPORTING iv_extra     TYPE clike OPTIONAL
                  io_xml       TYPE REF TO object
                  iv_normalize TYPE sap_bool DEFAULT abap_true
        RAISING   lcx_exception ##called,
      read_xml
        IMPORTING iv_extra      TYPE clike OPTIONAL
        RETURNING VALUE(ro_xml) TYPE REF TO lcl_xml_input
        RAISING   lcx_exception,
      read_abap
        IMPORTING iv_extra       TYPE clike OPTIONAL
                  iv_error       TYPE sap_bool DEFAULT abap_true
        RETURNING VALUE(rt_abap) TYPE abaptxt255_tab
        RAISING   lcx_exception,
      add_abap
        IMPORTING iv_extra TYPE clike OPTIONAL
                  it_abap  TYPE STANDARD TABLE
        RAISING   lcx_exception,
      add
        IMPORTING is_file TYPE ty_file,
      get_files
        RETURNING VALUE(rt_files) TYPE ty_files_tt,
      set_files
        IMPORTING it_files TYPE ty_files_tt.

  PRIVATE SECTION.
    DATA: ms_item  TYPE ty_item,
          mt_files TYPE ty_files_tt.

    METHODS:
      filename
        IMPORTING iv_extra           TYPE clike OPTIONAL
                  iv_ext             TYPE string
        RETURNING VALUE(rv_filename) TYPE string.

ENDCLASS.                    "lcl_objects_files DEFINITION

*----------------------------------------------------------------------*
*       INTERFACE lif_object DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_object.

  METHODS:
    serialize
      IMPORTING io_xml TYPE REF TO lcl_xml_output
      RAISING   lcx_exception,
    deserialize
      IMPORTING iv_package TYPE devclass
                io_xml     TYPE REF TO lcl_xml_input
      RAISING   lcx_exception,
    delete
      RAISING lcx_exception,
    exists
      RETURNING VALUE(rv_bool) TYPE abap_bool
      RAISING   lcx_exception,
    jump
      RAISING lcx_exception,
    get_metadata
      RETURNING VALUE(rs_metadata) TYPE ty_metadata.

  DATA: mo_files TYPE REF TO lcl_objects_files.

ENDINTERFACE.                    "lif_object DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_files IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_files IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
  ENDMETHOD.                    "constructor

  METHOD add.
    APPEND is_file TO mt_files.
  ENDMETHOD.                    "add

  METHOD get_files.
    rt_files = mt_files.
  ENDMETHOD.                    "get_files

  METHOD set_files.
    mt_files = it_files.
  ENDMETHOD.                    "set_files

  METHOD read_string.

    DATA: lv_filename TYPE string.

    FIELD-SYMBOLS: <ls_html> LIKE LINE OF mt_files.


    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = iv_ext ).            "#EC NOTEXT

    READ TABLE mt_files ASSIGNING <ls_html> WITH KEY filename = lv_filename.
    IF sy-subrc <> 0.
      _raise 'html not found'.
    ENDIF.

    rv_string = lcl_convert=>xstring_to_string_utf8( <ls_html>-data ).

  ENDMETHOD.                    "read_string

  METHOD read_abap.

    DATA: lv_filename TYPE string,
          lv_abap     TYPE string.

    FIELD-SYMBOLS: <ls_abap> LIKE LINE OF mt_files.


    CLEAR rt_abap.

    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = 'abap' ).            "#EC NOTEXT

    READ TABLE mt_files ASSIGNING <ls_abap> WITH KEY filename = lv_filename.
    IF sy-subrc <> 0.
      IF iv_error = abap_true.
        _raise 'abap not found'.
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.
    lv_abap = lcl_convert=>xstring_to_string_utf8( <ls_abap>-data ).

    SPLIT lv_abap AT gc_newline INTO TABLE rt_abap.

  ENDMETHOD.                    "read_abap

  METHOD add_abap.

    DATA: lv_source TYPE string,
          ls_file   TYPE ty_file.


    CONCATENATE LINES OF it_abap INTO lv_source SEPARATED BY gc_newline.
    ls_file-path = '/'.
    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = 'abap' ).       "#EC NOTEXT
    ls_file-data = lcl_convert=>string_to_xstring_utf8( lv_source ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.                    "abap_to_file

  METHOD add_string.

    DATA: ls_file TYPE ty_file.


    ls_file-path = '/'.
    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = iv_ext ).       "#EC NOTEXT
    ls_file-data = lcl_convert=>string_to_xstring_utf8( iv_string ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.                    "add_string

  METHOD add_xml.

    DATA: lv_xml  TYPE string,
          ls_file TYPE ty_file.


    lv_xml = io_xml->render( iv_normalize = iv_normalize
                             is_metadata = is_metadata ).
    ls_file-path = '/'.

    ls_file-filename = filename( iv_extra = iv_extra
                                 iv_ext   = 'xml' ).        "#EC NOTEXT

    REPLACE FIRST OCCURRENCE
      OF '<?xml version="1.0" encoding="utf-16"?>'
      IN lv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.
    ls_file-data = lcl_convert=>string_to_xstring_utf8( lv_xml ).

    APPEND ls_file TO mt_files.

  ENDMETHOD.                    "do

  METHOD read_xml.

    DATA: lv_filename TYPE string,
          lv_xml      TYPE string.

    FIELD-SYMBOLS: <ls_xml> LIKE LINE OF mt_files.


    lv_filename = filename( iv_extra = iv_extra
                            iv_ext   = 'xml' ).             "#EC NOTEXT

    READ TABLE mt_files ASSIGNING <ls_xml> WITH KEY filename = lv_filename.
    IF sy-subrc <> 0.
      _raise 'xml not found'.
    ENDIF.

    lv_xml = lcl_convert=>xstring_to_string_utf8( <ls_xml>-data ).

    CREATE OBJECT ro_xml
      EXPORTING
        iv_xml = lv_xml.

  ENDMETHOD.                    "read_xml

  METHOD filename.

    DATA: lv_obj_name TYPE string.


    IF ms_item-obj_type = 'SICF'.
* multiple SICF nodes with same name cannot be added to repository
      lv_obj_name = ms_item-obj_name(15).
    ELSE.
      lv_obj_name = ms_item-obj_name.
    ENDIF.
* handle namespaces
    REPLACE ALL OCCURRENCES OF '/' IN lv_obj_name WITH '#'.

    IF iv_extra IS INITIAL.
      CONCATENATE lv_obj_name '.' ms_item-obj_type '.' iv_ext
        INTO rv_filename.                                   "#EC NOTEXT
    ELSE.
      CONCATENATE lv_obj_name '.' ms_item-obj_type '.' iv_extra '.' iv_ext
        INTO rv_filename.                                   "#EC NOTEXT
    ENDIF.
    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.                    "filename

  METHOD add_xml_from_plugin.
*    this method wraps add_xml as in the plugin. This is necessary as the wrapped
*    xml-object in the plugin can only be typed to object.
*    ABAP does not perform implicit type casts (also if compatible) in signatures,
*    therefore this method's signature is typed ref to object
    DATA lo_xml TYPE REF TO lcl_xml_output.

    lo_xml ?= io_xml.

    me->add_xml(
      iv_extra     = iv_extra
      io_xml       = lo_xml
      iv_normalize = iv_normalize ).

  ENDMETHOD.                    "add_xml_from_plugin

ENDCLASS.                    "lcl_objects_files IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_super DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_super DEFINITION ABSTRACT.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          is_item     TYPE ty_item
          iv_language TYPE spras.

  PROTECTED SECTION.

    DATA: ms_item     TYPE ty_item,
          mv_language TYPE spras.

    METHODS:
      get_metadata
        RETURNING VALUE(rs_metadata) TYPE ty_metadata,
      corr_insert
        IMPORTING iv_package TYPE devclass
        RAISING   lcx_exception,
      jump_se11
        IMPORTING iv_radio TYPE string
                  iv_field TYPE string
        RAISING   lcx_exception.

ENDCLASS.                    "lcl_objects_super DEFINITION

**********************************************************************
* Enable plugins

CLASS lcl_objects_bridge DEFINITION INHERITING FROM lcl_objects_super FINAL.

  PUBLIC SECTION.

    CLASS-METHODS class_constructor.

    METHODS constructor
      IMPORTING is_item TYPE ty_item
      RAISING   cx_sy_create_object_error.

    INTERFACES lif_object.
    ALIASES mo_files FOR lif_object~mo_files.

  PRIVATE SECTION.
    DATA: mo_plugin TYPE REF TO object.

    TYPES: BEGIN OF ty_s_objtype_map,
             obj_typ      TYPE trobjtype,
             plugin_class TYPE seoclsname,
           END OF ty_s_objtype_map,
           ty_t_objtype_map TYPE SORTED TABLE OF ty_s_objtype_map WITH UNIQUE KEY obj_typ.

    CLASS-DATA gt_objtype_map TYPE ty_t_objtype_map.

ENDCLASS.                    "lcl_objects_bridge DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_bridge IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_bridge IMPLEMENTATION.

  METHOD lif_object~get_metadata.

    CALL METHOD mo_plugin->('ZIF_ABAPGIT_PLUGIN~GET_METADATA')
      RECEIVING
        rs_metadata = rs_metadata.

  ENDMETHOD.                    "lif_object~get_metadata

  METHOD constructor.

    DATA ls_objtype_map LIKE LINE OF gt_objtype_map.

    super->constructor( is_item = is_item
                        iv_language = gc_english ).

*    determine the responsible plugin
    READ TABLE gt_objtype_map INTO ls_objtype_map
      WITH TABLE KEY obj_typ = is_item-obj_type.
    IF sy-subrc = 0.
      CREATE OBJECT mo_plugin TYPE (ls_objtype_map-plugin_class).

      CALL METHOD mo_plugin->('SET_ITEM')
        EXPORTING
          iv_obj_type = is_item-obj_type
          iv_obj_name = is_item-obj_name.
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_create_object_error
        EXPORTING
          classname = 'LCL_OBJECTS_BRIDGE'.
    ENDIF.
  ENDMETHOD.                    "constructor

  METHOD lif_object~serialize.

    CALL METHOD mo_plugin->('WRAP_SERIALIZE')
      EXPORTING
        io_xml = io_xml.

  ENDMETHOD.                    "lif_object~serialize

  METHOD lif_object~deserialize.

    DATA: lx_plugin        TYPE REF TO cx_static_check.

    TRY.
        CALL METHOD mo_plugin->('WRAP_DESERIALIZE')
          EXPORTING
            iv_package = iv_package
            io_xml     = io_xml.
      CATCH cx_static_check INTO lx_plugin.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            ix_previous = lx_plugin
            iv_text     = lx_plugin->get_text( ).
    ENDTRY.
  ENDMETHOD.                    "lif_object~deserialize

  METHOD lif_object~delete.
    DATA lx_plugin TYPE REF TO cx_static_check.

    TRY.
        CALL METHOD mo_plugin->('ZIF_ABAPGIT_PLUGIN~DELETE').
      CATCH cx_static_check INTO lx_plugin.
        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            ix_previous = lx_plugin
            iv_text     = lx_plugin->get_text( ).
    ENDTRY.

  ENDMETHOD.                    "lif_object~delete

  METHOD lif_object~exists.

    CALL METHOD mo_plugin->('ZIF_ABAPGIT_PLUGIN~EXISTS')
      RECEIVING
        rv_bool = rv_bool.

  ENDMETHOD.                    "lif_object~exists

  METHOD lif_object~jump.

    CALL METHOD mo_plugin->('ZIF_ABAPGIT_PLUGIN~JUMP').

  ENDMETHOD.                    "lif_object~jump

  METHOD class_constructor.

    DATA lt_plugin_class    TYPE STANDARD TABLE OF seoclsname WITH DEFAULT KEY.
    DATA lv_plugin_class    LIKE LINE OF lt_plugin_class.
    DATA lo_plugin          TYPE REF TO object.
    DATA lt_plugin_obj_type TYPE objtyptable.
    DATA ls_objtype_map     LIKE LINE OF gt_objtype_map.


    SELECT ext~clsname
      FROM vseoextend AS ext
      INTO TABLE lt_plugin_class
      WHERE ext~refclsname LIKE 'ZCL_ABAPGIT_OBJECT%'
      AND ext~version = '1'.                              "#EC CI_SUBRC

    CLEAR gt_objtype_map.
    LOOP AT lt_plugin_class INTO lv_plugin_class
        WHERE table_line <> 'ZCL_ABAPGIT_OBJECT_BY_SOBJ'.
* have the generic plugin only as fallback
      TRY.
          CREATE OBJECT lo_plugin TYPE (lv_plugin_class).
        CATCH cx_sy_create_object_error.
          CONTINUE. ">>>>>>>>>>>>>>
      ENDTRY.

      CALL METHOD lo_plugin->('GET_SUPPORTED_OBJ_TYPES')
        IMPORTING
          rt_obj_type = lt_plugin_obj_type.

      ls_objtype_map-plugin_class = lv_plugin_class.
      LOOP AT lt_plugin_obj_type INTO ls_objtype_map-obj_typ.
        INSERT ls_objtype_map INTO TABLE gt_objtype_map.
        IF sy-subrc <> 0.
* No exception in class-contructor possible.
* Anyway, a shortdump is more appropriate in this case
          ASSERT 'There must not be' =
            |multiple ABAPGit-Plugins for the same object type {
            ls_objtype_map-obj_typ }|.
        ENDIF.
      ENDLOOP.
    ENDLOOP. "at plugins

* and the same for the generic plugin if exists
* have the generic plugin only as fallback
    LOOP AT lt_plugin_class INTO lv_plugin_class
        WHERE table_line = 'ZCL_ABAPGIT_OBJECT_BY_SOBJ'.
      CREATE OBJECT lo_plugin TYPE (lv_plugin_class).

      CALL METHOD lo_plugin->('GET_SUPPORTED_OBJ_TYPES')
        RECEIVING
          rt_obj_type = lt_plugin_obj_type.

      ls_objtype_map-plugin_class = lv_plugin_class.
      LOOP AT lt_plugin_obj_type INTO ls_objtype_map-obj_typ.
        INSERT ls_objtype_map INTO TABLE gt_objtype_map. "knowingly ignore the subrc
      ENDLOOP.
    ENDLOOP. "at plugins

  ENDMETHOD.                    "class_constructor

ENDCLASS.                    "lcl_objects_bridge IMPLEMENTATION

**********************************************************************

CLASS lcl_objects_program DEFINITION INHERITING FROM lcl_objects_super.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_progdir,
             name    TYPE progdir-name,
             state   TYPE progdir-state,
             sqlx    TYPE progdir-sqlx,
             edtx    TYPE progdir-edtx,
             varcl   TYPE progdir-varcl,
             dbapl   TYPE progdir-dbapl,
             dbna    TYPE progdir-dbna,
             clas    TYPE progdir-clas,
             type    TYPE progdir-type,
             occurs  TYPE progdir-occurs,
             subc    TYPE progdir-subc,
             appl    TYPE progdir-appl,
             secu    TYPE progdir-secu,
             cnam    TYPE progdir-cnam,
             cdat    TYPE progdir-cdat,
             unam    TYPE progdir-unam,
             udat    TYPE progdir-udat,
             vern    TYPE progdir-vern,
             levl    TYPE progdir-levl,
             rstat   TYPE progdir-rstat,
             rmand   TYPE progdir-rmand,
             rload   TYPE progdir-rload,
             fixpt   TYPE progdir-fixpt,
             sset    TYPE progdir-sset,
             sdate   TYPE progdir-sdate,
             stime   TYPE progdir-stime,
             idate   TYPE progdir-idate,
             itime   TYPE progdir-itime,
             ldbname TYPE progdir-ldbname,
             uccheck TYPE progdir-uccheck,
           END OF ty_progdir.

    METHODS serialize_program
      IMPORTING io_xml     TYPE REF TO lcl_xml_output OPTIONAL
                is_item    TYPE ty_item
                io_files   TYPE REF TO lcl_objects_files
                iv_program TYPE programm OPTIONAL
                iv_extra   TYPE clike OPTIONAL
      RAISING   lcx_exception.

    METHODS read_progdir
      IMPORTING iv_program        TYPE programm
      RETURNING VALUE(rs_progdir) TYPE ty_progdir.

    METHODS deserialize_program
      IMPORTING is_progdir TYPE ty_progdir
                it_source  TYPE abaptxt255_tab
                it_tpool   TYPE textpool_table
                iv_package TYPE devclass
      RAISING   lcx_exception.

  PROTECTED SECTION.
    TYPES: ty_spaces_tt TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_tpool.
        INCLUDE TYPE textpool.
    TYPES:   split TYPE c LENGTH 8.
    TYPES: END OF ty_tpool.

    TYPES: ty_tpool_tt TYPE STANDARD TABLE OF ty_tpool WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_dynpro,
             header     TYPE rpy_dyhead,
             containers TYPE dycatt_tab,
             fields     TYPE dyfatc_tab,
             flow_logic TYPE swydyflow,
             spaces     TYPE ty_spaces_tt,
           END OF ty_dynpro.

    TYPES: ty_dynpro_tt TYPE STANDARD TABLE OF ty_dynpro WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_cua,
             adm TYPE rsmpe_adm,
             sta TYPE STANDARD TABLE OF rsmpe_stat WITH DEFAULT KEY,
             fun TYPE STANDARD TABLE OF rsmpe_funt WITH DEFAULT KEY,
             men TYPE STANDARD TABLE OF rsmpe_men WITH DEFAULT KEY,
             mtx TYPE STANDARD TABLE OF rsmpe_mnlt WITH DEFAULT KEY,
             act TYPE STANDARD TABLE OF rsmpe_act WITH DEFAULT KEY,
             but TYPE STANDARD TABLE OF rsmpe_but WITH DEFAULT KEY,
             pfk TYPE STANDARD TABLE OF rsmpe_pfk WITH DEFAULT KEY,
             set TYPE STANDARD TABLE OF rsmpe_staf WITH DEFAULT KEY,
             doc TYPE STANDARD TABLE OF rsmpe_atrt WITH DEFAULT KEY,
             tit TYPE STANDARD TABLE OF rsmpe_titt WITH DEFAULT KEY,
             biv TYPE STANDARD TABLE OF rsmpe_buts WITH DEFAULT KEY,
           END OF ty_cua.

    METHODS serialize_dynpros
      IMPORTING iv_program_name  TYPE programm
      RETURNING VALUE(rt_dynpro) TYPE ty_dynpro_tt
      RAISING   lcx_exception.

    METHODS serialize_cua
      IMPORTING iv_program_name TYPE programm
      RETURNING VALUE(rs_cua)   TYPE ty_cua
      RAISING   lcx_exception.

    METHODS deserialize_dynpros
      IMPORTING it_dynpros TYPE ty_dynpro_tt
      RAISING   lcx_exception.

    METHODS deserialize_cua
      IMPORTING is_cua TYPE ty_cua
      RAISING   lcx_exception.

    CLASS-METHODS:
      add_tpool
        IMPORTING it_tpool        TYPE textpool_table
        RETURNING VALUE(rt_tpool) TYPE ty_tpool_tt,
      read_tpool
        IMPORTING it_tpool        TYPE ty_tpool_tt
        RETURNING VALUE(rt_tpool) TYPE ty_tpool_tt.

  PRIVATE SECTION.
    METHODS:
      condense_flow
        EXPORTING et_spaces TYPE ty_spaces_tt
        CHANGING  ct_flow   TYPE swydyflow,
      uncondense_flow
        IMPORTING it_flow        TYPE swydyflow
                  it_spaces      TYPE ty_spaces_tt
        RETURNING VALUE(rt_flow) TYPE swydyflow.


ENDCLASS.                    "lcl_objects_program DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_program IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_program IMPLEMENTATION.

  METHOD condense_flow.

    DATA: lv_spaces LIKE LINE OF et_spaces.

    FIELD-SYMBOLS: <ls_flow> LIKE LINE OF ct_flow.


    CLEAR et_spaces.

    LOOP AT ct_flow ASSIGNING <ls_flow>.
      lv_spaces = 0.

      WHILE NOT <ls_flow>-line IS INITIAL AND <ls_flow>-line(1) = space.
        lv_spaces = lv_spaces + 1.
        <ls_flow>-line = <ls_flow>-line+1.
      ENDWHILE.

      APPEND lv_spaces TO et_spaces.
    ENDLOOP.

  ENDMETHOD.

  METHOD uncondense_flow.

    DATA: lv_spaces LIKE LINE OF it_spaces.

    FIELD-SYMBOLS: <ls_flow>   LIKE LINE OF it_flow,
                   <ls_output> LIKE LINE OF rt_flow.


    LOOP AT it_flow ASSIGNING <ls_flow>.
      APPEND INITIAL LINE TO rt_flow ASSIGNING <ls_output>.
      <ls_output>-line = <ls_flow>-line.

      READ TABLE it_spaces INDEX sy-tabix INTO lv_spaces.
      IF sy-subrc = 0.
        SHIFT <ls_output>-line RIGHT BY lv_spaces PLACES IN CHARACTER MODE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD serialize_program.

    DATA: ls_progdir      TYPE ty_progdir,
          lv_program_name TYPE programm,
          lt_dynpros      TYPE ty_dynpro_tt,
          ls_cua          TYPE ty_cua,
          lt_source       TYPE TABLE OF abaptxt255,
          lt_tpool        TYPE textpool_table,
          ls_tpool        LIKE LINE OF lt_tpool,
          lo_xml          TYPE REF TO lcl_xml_output.

    IF iv_program IS INITIAL.
      lv_program_name = is_item-obj_name.
    ELSE.
      lv_program_name = iv_program.
    ENDIF.

    CALL FUNCTION 'RPY_PROGRAM_READ'
      EXPORTING
        program_name     = lv_program_name
        with_lowercase   = abap_true
      TABLES
        source_extended  = lt_source
        textelements     = lt_tpool
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4.
    IF sy-subrc = 2.
      RETURN.
    ELSEIF sy-subrc <> 0.
      _raise 'Error reading program'.
    ENDIF.

    ls_progdir = read_progdir( lv_program_name ).

    IF io_xml IS BOUND.
      lo_xml = io_xml.
    ELSE.
      CREATE OBJECT lo_xml.
    ENDIF.

    lo_xml->add( iv_name = 'PROGDIR'
                 ig_data = ls_progdir ).
    IF ls_progdir-subc = '1'.
      lt_dynpros = serialize_dynpros( lv_program_name ).
      lo_xml->add( iv_name = 'DYNPROS'
                   ig_data = lt_dynpros ).

      ls_cua = serialize_cua( lv_program_name ).
      lo_xml->add( iv_name = 'CUA'
                   ig_data = ls_cua ).
    ENDIF.

    IF lines( lt_tpool ) = 1.
      READ TABLE lt_tpool INDEX 1 INTO ls_tpool.
      ASSERT sy-subrc = 0.
      IF ls_tpool-id = 'R' AND ls_tpool-key = '' AND ls_tpool-length = 0.
        DELETE lt_tpool INDEX 1.
      ENDIF.
    ENDIF.

    lo_xml->add( iv_name = 'TPOOL'
                 ig_data = add_tpool( lt_tpool ) ).

    IF NOT io_xml IS BOUND.
      io_files->add_xml( iv_extra = iv_extra
                         io_xml   = lo_xml ).
    ENDIF.

    io_files->add_abap( iv_extra = iv_extra
                        it_abap  = lt_source ).

  ENDMETHOD.                    "serialize_program

  METHOD deserialize_program.

    DATA: lv_exists      TYPE sap_bool,
          lv_progname    TYPE reposrc-progname,
          ls_tpool       LIKE LINE OF it_tpool,
          lv_title       TYPE rglif-title,
          ls_progdir_new TYPE progdir.


    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = is_progdir-name
        object_class        = 'ABAP'
        devclass            = iv_package
        master_language     = mv_language
        mode                = 'INSERT'
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc = 1.
      _raise 'Cancelled'.
    ELSEIF sy-subrc <> 0.
      _raise 'error from RS_CORR_INSERT'.
    ENDIF.

    READ TABLE it_tpool INTO ls_tpool WITH KEY id = 'R'.  "#EC CI_SUBRC
    lv_title = ls_tpool-entry.

    SELECT SINGLE progname FROM reposrc INTO lv_progname
      WHERE progname = is_progdir-name
      AND r3state = 'A'.
    IF sy-subrc = 0.
      lv_exists = abap_true.
    ELSE.
      lv_exists = abap_false.
    ENDIF.

    IF lv_exists = abap_true.
      CALL FUNCTION 'RPY_PROGRAM_UPDATE'
        EXPORTING
          program_name     = is_progdir-name
          title_string     = lv_title
          save_inactive    = 'I'
        TABLES
          source_extended  = it_source
        EXCEPTIONS
          cancelled        = 1
          permission_error = 2
          not_found        = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        IF sy-msgid = 'EU' AND sy-msgno = '510'.
          _raise 'User is currently editing program'.
        ELSE.
          _raise 'PROG, error updating'.
        ENDIF.
      ENDIF.
    ELSE.
* function module RPY_PROGRAM_INSERT cannot handle function group includes

      INSERT REPORT is_progdir-name
        FROM it_source
        STATE 'I'
        PROGRAM TYPE is_progdir-subc.
      IF sy-subrc <> 0.
        _raise 'error from INSERT REPORT'.
      ENDIF.

      IF NOT it_tpool[] IS INITIAL.
        INSERT TEXTPOOL is_progdir-name
          FROM it_tpool
          LANGUAGE mv_language
          STATE 'I'.
        IF sy-subrc <> 0.
          _raise 'error from INSERT TEXTPOOL'.
        ENDIF.
      ENDIF.

    ENDIF.

    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = is_progdir-name
        i_state    = 'I'
      IMPORTING
        e_progdir  = ls_progdir_new
      EXCEPTIONS
        not_exists = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      _raise 'not found in PROGDIR'.
    ENDIF.

* todo, package?

    ls_progdir_new-ldbname = is_progdir-ldbname.
    ls_progdir_new-dbna    = is_progdir-dbna.
    ls_progdir_new-dbapl   = is_progdir-dbapl.
    ls_progdir_new-rload   = is_progdir-rload.
    ls_progdir_new-fixpt   = is_progdir-fixpt.
    ls_progdir_new-varcl   = is_progdir-varcl.
    ls_progdir_new-appl    = is_progdir-appl.

    CALL FUNCTION 'UPDATE_PROGDIR'
      EXPORTING
        i_progdir    = ls_progdir_new
        i_progname   = ls_progdir_new-name
        i_state      = ls_progdir_new-state
      EXCEPTIONS
        not_executed = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      _raise 'PROG, error inserting'.
    ENDIF.

    lcl_objects_activation=>add( iv_type = 'REPS'
                                 iv_name = is_progdir-name ).

  ENDMETHOD.                    "deserialize_program

  METHOD read_progdir.

    DATA: ls_sapdir TYPE progdir.


    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = iv_program
        i_state    = 'A'
      IMPORTING
        e_progdir  = ls_sapdir.
    MOVE-CORRESPONDING ls_sapdir TO rs_progdir.

    CLEAR: rs_progdir-edtx,
           rs_progdir-cnam,
           rs_progdir-cdat,
           rs_progdir-unam,
           rs_progdir-udat,
           rs_progdir-vern,
           rs_progdir-rmand,
           rs_progdir-sdate,
           rs_progdir-stime,
           rs_progdir-idate,
           rs_progdir-itime.

  ENDMETHOD.                    "read_progdir

  METHOD serialize_cua.

    CALL FUNCTION 'RS_CUA_INTERNAL_FETCH'
      EXPORTING
        program         = iv_program_name
        language        = mv_language
        state           = 'A'
      IMPORTING
        adm             = rs_cua-adm
      TABLES
        sta             = rs_cua-sta
        fun             = rs_cua-fun
        men             = rs_cua-men
        mtx             = rs_cua-mtx
        act             = rs_cua-act
        but             = rs_cua-but
        pfk             = rs_cua-pfk
        set             = rs_cua-set
        doc             = rs_cua-doc
        tit             = rs_cua-tit
        biv             = rs_cua-biv
      EXCEPTIONS
        not_found       = 1
        unknown_version = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      _raise 'error from RS_CUA_INTERNAL_FETCH'.
    ENDIF.

  ENDMETHOD.                    "serialize_cua

  METHOD serialize_dynpros.

    DATA: ls_header               TYPE rpy_dyhead,
          lt_containers           TYPE dycatt_tab,
          lt_fields_to_containers TYPE dyfatc_tab,
          lt_flow_logic           TYPE swydyflow,
          lt_d020s                TYPE TABLE OF d020s.

    FIELD-SYMBOLS: <ls_d020s>       LIKE LINE OF lt_d020s,
                   <lv_outputstyle> TYPE scrpostyle,
                   <ls_field>       LIKE LINE OF lt_fields_to_containers,
                   <ls_dynpro>      LIKE LINE OF rt_dynpro.


    CALL FUNCTION 'RS_SCREEN_LIST'
      EXPORTING
        dynnr     = ''
        progname  = iv_program_name
      TABLES
        dynpros   = lt_d020s
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 2.
      _raise 'error from screen_list'.
    ENDIF.

* loop dynpros and skip generated selection screens
    LOOP AT lt_d020s ASSIGNING <ls_d020s> WHERE type <> 'S'.

      CALL FUNCTION 'RPY_DYNPRO_READ'
        EXPORTING
          progname             = iv_program_name
          dynnr                = <ls_d020s>-dnum
        IMPORTING
          header               = ls_header
        TABLES
          containers           = lt_containers
          fields_to_containers = lt_fields_to_containers
          flow_logic           = lt_flow_logic
        EXCEPTIONS
          cancelled            = 1
          not_found            = 2
          permission_error     = 3
          OTHERS               = 4.
      IF sy-subrc <> 0.
        _raise 'Error while reading dynpro'.
      ENDIF.

      LOOP AT lt_fields_to_containers ASSIGNING <ls_field>.
* output style is a NUMC field, the XML conversion will fail if it contains invalid value
* field does not exist in all versions
        ASSIGN COMPONENT 'OUTPUTSTYLE' OF STRUCTURE <ls_field> TO <lv_outputstyle>.
        IF sy-subrc = 0 AND <lv_outputstyle> = '  '.
          CLEAR <lv_outputstyle>.
        ENDIF.
      ENDLOOP.

      APPEND INITIAL LINE TO rt_dynpro ASSIGNING <ls_dynpro>.
      <ls_dynpro>-header     = ls_header.
      <ls_dynpro>-containers = lt_containers.
      <ls_dynpro>-fields     = lt_fields_to_containers.

      condense_flow( IMPORTING et_spaces = <ls_dynpro>-spaces
                     CHANGING ct_flow = lt_flow_logic ).
      <ls_dynpro>-flow_logic = lt_flow_logic.

    ENDLOOP.

  ENDMETHOD.                    "serialize_dynpros


  METHOD deserialize_dynpros.

    DATA: lv_name   TYPE dwinactiv-obj_name,
          ls_dynpro LIKE LINE OF it_dynpros.


* ls_dynpro is changed by the function module, a field-symbol will cause
* the program to dump since it_dynpros cannot be changed
    LOOP AT it_dynpros INTO ls_dynpro.

      ls_dynpro-flow_logic = uncondense_flow(
        it_flow = ls_dynpro-flow_logic
        it_spaces = ls_dynpro-spaces ).

      CALL FUNCTION 'RPY_DYNPRO_INSERT'
        EXPORTING
          header                 = ls_dynpro-header
          suppress_exist_checks  = abap_true
        TABLES
          containers             = ls_dynpro-containers
          fields_to_containers   = ls_dynpro-fields
          flow_logic             = ls_dynpro-flow_logic
        EXCEPTIONS
          cancelled              = 1
          already_exists         = 2
          program_not_exists     = 3
          not_executed           = 4
          missing_required_field = 5
          illegal_field_value    = 6
          field_not_allowed      = 7
          not_generated          = 8
          illegal_field_position = 9
          OTHERS                 = 10.
      IF sy-subrc <> 2 AND sy-subrc <> 0.
        _raise 'error from RPY_DYNPRO_INSERT'.
      ENDIF.
* todo, RPY_DYNPRO_UPDATE?

      CONCATENATE ls_dynpro-header-program ls_dynpro-header-screen
        INTO lv_name RESPECTING BLANKS.
      ASSERT NOT lv_name IS INITIAL.

      lcl_objects_activation=>add( iv_type = 'DYNP'
                                   iv_name = lv_name ).

    ENDLOOP.

  ENDMETHOD.                    "deserialize_dynpros

  METHOD add_tpool.

    FIELD-SYMBOLS: <ls_tpool_in>  LIKE LINE OF it_tpool,
                   <ls_tpool_out> LIKE LINE OF rt_tpool.


    LOOP AT it_tpool ASSIGNING <ls_tpool_in>.
      APPEND INITIAL LINE TO rt_tpool ASSIGNING <ls_tpool_out>.
      MOVE-CORRESPONDING <ls_tpool_in> TO <ls_tpool_out>.
      IF <ls_tpool_out>-id = 'S'.
        <ls_tpool_out>-split = <ls_tpool_out>-entry.
        <ls_tpool_out>-entry = <ls_tpool_out>-entry+8.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "add_tpool

  METHOD read_tpool.

    FIELD-SYMBOLS: <ls_tpool_in>  LIKE LINE OF it_tpool,
                   <ls_tpool_out> LIKE LINE OF rt_tpool.


    LOOP AT it_tpool ASSIGNING <ls_tpool_in>.
      APPEND INITIAL LINE TO rt_tpool ASSIGNING <ls_tpool_out>.
      MOVE-CORRESPONDING <ls_tpool_in> TO <ls_tpool_out>.
      IF <ls_tpool_out>-id = 'S'.
        CONCATENATE <ls_tpool_in>-split <ls_tpool_in>-entry
          INTO <ls_tpool_out>-entry
          RESPECTING BLANKS.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "read_tpool

  METHOD deserialize_cua.

    DATA: ls_tr_key TYPE trkey.


    IF is_cua-adm IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE devclass INTO ls_tr_key-devclass
      FROM tadir
      WHERE pgmid = 'R3TR'
      AND object = ms_item-obj_type
      AND obj_name = ms_item-obj_name.                  "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'not found in tadir'.
    ENDIF.

    ls_tr_key-obj_type = ms_item-obj_type.
    ls_tr_key-obj_name = ms_item-obj_name.
    ls_tr_key-sub_type = 'CUAD'.
    ls_tr_key-sub_name = ms_item-obj_name.

    sy-tcode = 'SE41' ##write_ok. " evil hack, workaround to handle fixes in note 2159455
    CALL FUNCTION 'RS_CUA_INTERNAL_WRITE'
      EXPORTING
        program   = ms_item-obj_name
        language  = mv_language
        tr_key    = ls_tr_key
        adm       = is_cua-adm
        state     = 'I'
      TABLES
        sta       = is_cua-sta
        fun       = is_cua-fun
        men       = is_cua-men
        mtx       = is_cua-mtx
        act       = is_cua-act
        but       = is_cua-but
        pfk       = is_cua-pfk
        set       = is_cua-set
        doc       = is_cua-doc
        tit       = is_cua-tit
        biv       = is_cua-biv
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      _raise 'error from RS_CUA_INTERNAL_WRITE'.
    ENDIF.

    lcl_objects_activation=>add( iv_type = 'CUAD'
                                 iv_name = ms_item-obj_name ).

  ENDMETHOD.                    "deserialize_cua

ENDCLASS.                    "lcl_objects_program IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_super IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_super IMPLEMENTATION.

  METHOD constructor.
    ms_item = is_item.
    ASSERT NOT ms_item IS INITIAL.
    mv_language = iv_language.
    ASSERT NOT mv_language IS INITIAL.
  ENDMETHOD.                    "constructor

  METHOD jump_se11.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.


    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPLSD_ENTRY'.
    <ls_bdcdata>-dynpro   = '1000'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=WB_DISPLAY'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = iv_radio.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = iv_field.
    <ls_bdcdata>-fval = ms_item-obj_name.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                 = 'SE11'
        mode_val              = 'E'
      TABLES
        using_tab             = lt_bdcdata
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4
        ##fm_subrc_ok.                                                   "#EC CI_SUBRC

  ENDMETHOD.                                                "jump_se11

  METHOD get_metadata.
    rs_metadata-class =
      cl_abap_classdescr=>describe_by_object_ref( me )->get_relative_name( ).
    rs_metadata-version = 'v1.0.0' ##no_text.
  ENDMETHOD.                    "get_metadata

  METHOD corr_insert.

    DATA: ls_object TYPE ddenqs.


    ls_object-objtype = ms_item-obj_type.
    ls_object-objname = ms_item-obj_name.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = ls_object
        object_class        = 'DICT'
        devclass            = iv_package
        master_language     = mv_language
        mode                = 'INSERT'
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc = 1.
      _raise 'Cancelled'.
    ELSEIF sy-subrc <> 0.
      _raise 'error from RS_CORR_INSERT'.
    ENDIF.

  ENDMETHOD.                    "corr_insert

ENDCLASS.                    "lcl_objects_super IMPLEMENTATION

INCLUDE zabapgit_object.

*----------------------------------------------------------------------*
*       CLASS lcl_object DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: ty_types_tt TYPE STANDARD TABLE OF tadir-object WITH DEFAULT KEY.

    CLASS-METHODS serialize
      IMPORTING is_item         TYPE ty_item
                iv_language     TYPE spras
      RETURNING VALUE(rt_files) TYPE ty_files_tt
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING io_repo TYPE REF TO lcl_repo
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING it_tadir TYPE lcl_tadir=>ty_tadir_tt
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE ty_item
      RAISING   lcx_exception.

    CLASS-METHODS is_supported
      IMPORTING is_item        TYPE ty_item
      RETURNING VALUE(rv_bool) TYPE abap_bool.

    CLASS-METHODS exists
      IMPORTING is_item        TYPE ty_item
      RETURNING VALUE(rv_bool) TYPE abap_bool.

    CLASS-METHODS supported_list
      RETURNING VALUE(rt_types) TYPE ty_types_tt.

  PRIVATE SECTION.
    CLASS-METHODS create_object
      IMPORTING is_item       TYPE ty_item
                iv_language   TYPE spras
                is_metadata   TYPE ty_metadata OPTIONAL
      RETURNING VALUE(ri_obj) TYPE REF TO lif_object
      RAISING   lcx_exception.

    CLASS-METHODS
      prioritize_deser
        IMPORTING it_results        TYPE ty_results_tt
        RETURNING VALUE(rt_results) TYPE ty_results_tt.

    CLASS-METHODS
      path_to_package
        IMPORTING iv_top            TYPE devclass
                  iv_start          TYPE string
                  iv_path           TYPE string
        RETURNING VALUE(rv_package) TYPE devclass
        RAISING   lcx_exception.

    CLASS-METHODS class_name
      IMPORTING is_item              TYPE ty_item
      RETURNING VALUE(rv_class_name) TYPE string.

    CLASS-METHODS resolve_ddic
      CHANGING ct_tadir TYPE lcl_tadir=>ty_tadir_tt
      RAISING  lcx_exception.

    CLASS-METHODS warning_overwrite
      IMPORTING io_repo    TYPE REF TO lcl_repo
      CHANGING  ct_results TYPE ty_results_tt
      RAISING   lcx_exception.

    CLASS-METHODS warning_package
      IMPORTING is_item          TYPE ty_item
                iv_package       TYPE devclass
      RETURNING VALUE(rv_cancel) TYPE abap_bool
      RAISING   lcx_exception.

    CLASS-METHODS update_package_tree
      IMPORTING iv_package TYPE devclass.

    CLASS-METHODS delete_obj
      IMPORTING is_item TYPE ty_item
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object DEFINITION



*----------------------------------------------------------------------*
*       CLASS lcl_tadir IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_tadir IMPLEMENTATION.

  METHOD read_single.

    DATA: lv_obj_name TYPE tadir-obj_name.


    IF iv_object = 'SICF'.
      CONCATENATE iv_obj_name '%' INTO lv_obj_name.
    ELSE.
      lv_obj_name = iv_obj_name.
    ENDIF.

    SELECT SINGLE * FROM tadir INTO rs_tadir
      WHERE pgmid = iv_pgmid
      AND object = iv_object
      AND obj_name LIKE lv_obj_name.      "#EC CI_SUBRC "#EC CI_GENBUFF

  ENDMETHOD.                    "read_single

  METHOD check_exists.

    DATA: lv_exists TYPE abap_bool,
          ls_item   TYPE ty_item.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


* rows from database table TADIR are not removed for
* transportable objects until the transport is released
    LOOP AT it_tadir ASSIGNING <ls_tadir>.
      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.

      lv_exists = lcl_objects=>exists( ls_item ).
      IF lv_exists = abap_true.
        APPEND <ls_tadir> TO rt_tadir.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "check_exists

  METHOD read.

* start recursion
    rt_tadir = build( iv_package = iv_package
                      iv_parent  = ''
                      iv_path    = '' ).

    rt_tadir = check_exists( rt_tadir ).

  ENDMETHOD.                    "read

  METHOD build.

    DATA: lv_index    TYPE i,
          lt_tadir    TYPE ty_tadir_tt,
          lt_tdevc    TYPE STANDARD TABLE OF tdevc,
          lv_len      TYPE i,
          lv_message  TYPE string,
          lv_path     TYPE string,
          lv_category TYPE seoclassdf-category.

    FIELD-SYMBOLS: <ls_tdevc> LIKE LINE OF lt_tdevc,
                   <ls_tadir> LIKE LINE OF rt_tadir.


    SELECT * FROM tadir
      INTO CORRESPONDING FIELDS OF TABLE rt_tadir
      WHERE devclass = iv_package
      AND object <> 'DEVC'
      AND object <> 'SOTR'
      AND object <> 'SFB1'
      AND object <> 'SFB2'
      AND delflag = abap_false
      ORDER BY PRIMARY KEY.               "#EC CI_GENBUFF "#EC CI_SUBRC

    LOOP AT rt_tadir ASSIGNING <ls_tadir>.
      lv_index = sy-tabix.

      <ls_tadir>-path = iv_path.

      CASE <ls_tadir>-object.
        WHEN 'SICF'.
          <ls_tadir>-obj_name = <ls_tadir>-obj_name(15).
        WHEN 'INTF'.
          SELECT SINGLE category FROM seoclassdf INTO lv_category
            WHERE clsname = <ls_tadir>-obj_name
            AND ( version = '1'
            OR version = '0' ) ##warn_ok.               "#EC CI_GENBUFF
          IF sy-subrc = 0 AND lv_category = seoc_category_webdynpro_class.
            DELETE rt_tadir INDEX lv_index.
          ENDIF.
      ENDCASE.
    ENDLOOP.

* look for subpackages
    SELECT * FROM tdevc INTO TABLE lt_tdevc
      WHERE parentcl = iv_package
      ORDER BY PRIMARY KEY.               "#EC CI_SUBRC "#EC CI_GENBUFF
    LOOP AT lt_tdevc ASSIGNING <ls_tdevc>.
      lv_len = strlen( iv_package ).
      IF <ls_tdevc>-devclass(lv_len) <> iv_package.
* if abapGit project is installed in package ZZZ, all subpackages should be named
* ZZZ_something. This will define the folder name in the zip file to be "something",
* similarily with online projects
        lv_message = 'Unexpected package naming(' &&
          <ls_tdevc>-devclass && ')' ##no_text.
        MESSAGE lv_message TYPE 'I'.
        CONTINUE.
      ENDIF.

      lv_path = <ls_tdevc>-devclass+lv_len.
      IF lv_path(1) = '_'.
        lv_path = lv_path+1.
      ENDIF.
      TRANSLATE lv_path TO LOWER CASE.
      CONCATENATE iv_path lv_path '/' INTO lv_path.

      lt_tadir = build( iv_package = <ls_tdevc>-devclass
                        iv_parent  = iv_package
                        iv_path    = lv_path ).
      APPEND LINES OF lt_tadir TO rt_tadir.
    ENDLOOP.

  ENDMETHOD.                    "build

ENDCLASS.                    "lcl_tadir IMPLEMENTATION


INCLUDE zabapgit_file_status.


*----------------------------------------------------------------------*
*       CLASS lcl_object IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects IMPLEMENTATION.

  METHOD warning_overwrite.

    DATA: lv_index    TYPE i,
          lv_answer   TYPE c,
          lv_question TYPE string,
          lt_before   TYPE lcl_persistence_repo=>ty_local_checksum_tt,
          lt_current  TYPE lcl_persistence_repo=>ty_local_checksum_tt.

    FIELD-SYMBOLS: <ls_before>  LIKE LINE OF lt_before,
                   <ls_current> LIKE LINE OF lt_current,
                   <ls_result>  LIKE LINE OF ct_results.


    lt_before = io_repo->get_local_checksums( ).
    lt_current = io_repo->build_local_checksums( ).

    LOOP AT ct_results ASSIGNING <ls_result>.
      lv_index = sy-tabix.

      READ TABLE lt_before ASSIGNING <ls_before>
        WITH KEY item-obj_type = <ls_result>-obj_type
        item-obj_name = <ls_result>-obj_name.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_current ASSIGNING <ls_current>
        WITH KEY item-obj_type = <ls_result>-obj_type
        item-obj_name = <ls_result>-obj_name.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF <ls_before>-sha1 <> <ls_current>-sha1.
        lv_question = |It looks like object { <ls_result>-obj_type
          } { <ls_result>-obj_name
          } has been modified locally, overwrite object?|.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Warning'
            text_question         = lv_question
            display_cancel_button = abap_false
          IMPORTING
            answer                = lv_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2 ##NO_TEXT.
        IF sy-subrc <> 0.
          _raise 'error from POPUP_TO_CONFIRM'.
        ENDIF.

        IF lv_answer = '2'.
          DELETE ct_results INDEX lv_index.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD warning_package.

    DATA: lv_question TYPE c LENGTH 200,
          lv_answer   TYPE c,
          ls_tadir    TYPE tadir.


    ls_tadir = lcl_tadir=>read_single( iv_object   = is_item-obj_type
                                       iv_obj_name = is_item-obj_name ).
    IF NOT ls_tadir IS INITIAL AND ls_tadir-devclass <> iv_package.
      CONCATENATE 'Overwrite object' is_item-obj_type is_item-obj_name
        'from package' ls_tadir-devclass
        INTO lv_question SEPARATED BY space.                "#EC NOTEXT

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Warning'
          text_question         = lv_question
          text_button_1         = 'Ok'
          icon_button_1         = 'ICON_DELETE'
          text_button_2         = 'Cancel'
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '2'
          display_cancel_button = abap_false
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.                        "#EC NOTEXT
      IF sy-subrc <> 0.
        _raise 'error from POPUP_TO_CONFIRM'.
      ENDIF.

      IF lv_answer = '2'.
        rv_cancel = abap_true.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "check_warning

  METHOD update_package_tree.

    DATA: lt_packages TYPE lcl_sap_package=>ty_devclass_tt,
          lv_package  LIKE LINE OF lt_packages,
          lv_tree     TYPE dirtree-tname.


    lt_packages = lcl_sap_package=>list_subpackages( iv_package ).
    APPEND iv_package TO lt_packages.

    LOOP AT lt_packages INTO lv_package.
* update package tree for SE80
      lv_tree = 'EU_' && lv_package.
      CALL FUNCTION 'WB_TREE_ACTUALIZE'
        EXPORTING
          tree_name              = lv_tree
          without_crossreference = abap_true
          with_tcode_index       = abap_true.
    ENDLOOP.

  ENDMETHOD.                    "update_package_tree

  METHOD create_object.

    TYPES: BEGIN OF ty_obj_serializer_map,
             item     LIKE is_item,
             metadata LIKE is_metadata,
           END OF ty_obj_serializer_map.

    STATICS st_obj_serializer_map
      TYPE SORTED TABLE OF ty_obj_serializer_map WITH UNIQUE KEY item.

    DATA: lv_message            TYPE string,
          lv_class_name         TYPE string,
          ls_obj_serializer_map LIKE LINE OF st_obj_serializer_map.


    READ TABLE st_obj_serializer_map
      INTO ls_obj_serializer_map WITH KEY item = is_item.
    IF sy-subrc = 0.
      lv_class_name = ls_obj_serializer_map-metadata-class.
    ELSEIF is_metadata IS NOT INITIAL.
*        Metadata is provided only on serialization
*        Once this has been triggered, the same serializer shall be used
*        for subsequent processes.
*        Thus, buffer the metadata afterwards
      ls_obj_serializer_map-item      = is_item.
      ls_obj_serializer_map-metadata  = is_metadata.
      INSERT ls_obj_serializer_map INTO TABLE st_obj_serializer_map.

      lv_class_name = is_metadata-class.
    ELSE.
      lv_class_name = class_name( is_item ).
    ENDIF.

    TRY.
        CREATE OBJECT ri_obj TYPE (lv_class_name)
          EXPORTING
            is_item = is_item
            iv_language = iv_language.
      CATCH cx_sy_create_object_error.
        TRY.
* 2nd step, try looking for plugins
            CREATE OBJECT ri_obj TYPE lcl_objects_bridge
              EXPORTING
                is_item = is_item.
          CATCH cx_sy_create_object_error.
            CONCATENATE 'Object type' is_item-obj_type 'not supported, serialize'
              INTO lv_message
              SEPARATED BY space.                           "#EC NOTEXT
            _raise lv_message.
        ENDTRY.
    ENDTRY.

  ENDMETHOD.                    "create_object

  METHOD is_supported.

    TRY.
        create_object( is_item = is_item
                       iv_language = gc_english ).
        rv_bool = abap_true.
      CATCH lcx_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.                    "is_supported

  METHOD supported_list.

    DATA: lv_type  LIKE LINE OF rt_types,
          lt_snode TYPE TABLE OF snode.

    FIELD-SYMBOLS: <ls_snode> LIKE LINE OF lt_snode.


    CALL FUNCTION 'WB_TREE_ACTUALIZE'
      EXPORTING
        tree_name              = 'PG_ZABAPGIT'
        without_crossreference = abap_true
        with_tcode_index       = abap_true
      TABLES
        p_tree                 = lt_snode.

    DELETE lt_snode WHERE type <> 'OPL'
      OR name NP 'LCL_OBJECT_++++'.

    LOOP AT lt_snode ASSIGNING <ls_snode>.
      lv_type = <ls_snode>-name+11.
      APPEND lv_type TO rt_types.
    ENDLOOP.

  ENDMETHOD.                    "supported_list

  METHOD exists.

    DATA: li_obj TYPE REF TO lif_object.


    TRY.
        li_obj = create_object( is_item = is_item
                                iv_language = gc_english ).
        rv_bool = li_obj->exists( ).
      CATCH lcx_exception.
* ignore all errors and assume the object exists
        rv_bool = abap_true.
    ENDTRY.

  ENDMETHOD.                    "exists

  METHOD path_to_package.

    DATA: lv_length TYPE i,
          lv_path   TYPE string.


    lv_length = strlen( iv_start ) - 1.
    lv_path = iv_path+lv_length.

    CONCATENATE iv_top lv_path INTO rv_package.

    TRANSLATE rv_package USING '/_'.

    lv_length = strlen( rv_package ) - 1.

    rv_package = rv_package(lv_length).

    TRANSLATE rv_package TO UPPER CASE.

    IF lcl_sap_package=>exists( rv_package ) = abap_false.
      lcl_sap_package=>create_child( iv_parent = iv_top
                                     iv_child = rv_package ).
    ENDIF.

  ENDMETHOD.

  METHOD class_name.

    CONCATENATE 'LCL_OBJECT_' is_item-obj_type INTO rv_class_name. "#EC NOTEXT

  ENDMETHOD.                    "class_name

  METHOD jump.

    DATA: li_obj TYPE REF TO lif_object.


    li_obj = create_object( is_item = is_item
                            iv_language = gc_english ).
    li_obj->jump( ).

  ENDMETHOD.                    "jump

  METHOD delete.

    DATA: ls_item  TYPE ty_item,
          lt_tadir LIKE it_tadir.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


* misuse field KORRNUM to fix deletion sequence

    lt_tadir[] = it_tadir[].

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      CASE <ls_tadir>-object.
        WHEN 'IATU'.
          <ls_tadir>-korrnum = '5500'.
        WHEN 'IARP'.
          <ls_tadir>-korrnum = '5510'.
        WHEN 'IASP'.
          <ls_tadir>-korrnum = '5520'.
        WHEN 'SUSC'.
          <ls_tadir>-korrnum = '5000'.
        WHEN 'TTYP' OR 'TABL' OR 'VIEW'.
          <ls_tadir>-korrnum = '7000'.
        WHEN 'DTEL'.
          <ls_tadir>-korrnum = '8000'.
        WHEN 'DOMA'.
          <ls_tadir>-korrnum = '9000'.
        WHEN 'PROG'.
* delete includes after main programs
          SELECT COUNT(*) FROM reposrc
            WHERE progname = <ls_tadir>-obj_name
            AND r3state = 'A'
            AND subc = 'I'.
          IF sy-subrc = 0.
            <ls_tadir>-korrnum = '2000'.
          ELSE.
            <ls_tadir>-korrnum = '1000'.
          ENDIF.
        WHEN OTHERS.
          <ls_tadir>-korrnum = '1000'.
      ENDCASE.
    ENDLOOP.

    resolve_ddic( CHANGING ct_tadir = lt_tadir ).

    SORT lt_tadir BY korrnum ASCENDING.

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      lcl_progress=>show( iv_key     = 'Delete'
                          iv_current = sy-tabix
                          iv_total   = lines( lt_tadir )
                          iv_text    = <ls_tadir>-obj_name ) ##NO_TEXT.

      CLEAR ls_item.
      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      delete_obj( ls_item ).
    ENDLOOP.

  ENDMETHOD.                    "delete

  METHOD resolve_ddic.
* this will make sure the deletion sequence of structures/tables work
* in case they have dependencies with .INCLUDE

    TYPES: BEGIN OF ty_edge,
             from TYPE ty_item,
             to   TYPE ty_item,
           END OF ty_edge.

    DATA: lt_nodes        TYPE TABLE OF ty_item,
          lt_edges        TYPE TABLE OF ty_edge,
          lt_findstrings  TYPE TABLE OF rsfind,
          lv_plus         TYPE i VALUE 1,
          lv_find_obj_cls TYPE euobj-id,
          lv_index        TYPE i,
          lv_before       TYPE i,
          lt_founds       TYPE TABLE OF rsfindlst,
          lt_scope        TYPE STANDARD TABLE OF seu_obj.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF ct_tadir,
                   <ls_edge>  LIKE LINE OF lt_edges,
                   <ls_found> LIKE LINE OF lt_founds,
                   <ls_node>  LIKE LINE OF lt_nodes.


* build nodes
    LOOP AT ct_tadir ASSIGNING <ls_tadir>
        WHERE object = 'TABL'
        OR object = 'TTYP'.
      APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
      <ls_node>-obj_name = <ls_tadir>-obj_name.
      <ls_node>-obj_type = <ls_tadir>-object.
    ENDLOOP.

    APPEND 'TABL' TO lt_scope.
    APPEND 'STRU' TO lt_scope.
    APPEND 'TTYP' TO lt_scope.

* build edges
    LOOP AT lt_nodes ASSIGNING <ls_node>.

      CLEAR lt_findstrings.
      APPEND <ls_node>-obj_name TO lt_findstrings.
      lv_find_obj_cls = <ls_node>-obj_type.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls           = lv_find_obj_cls
        TABLES
          i_findstrings            = lt_findstrings
          o_founds                 = lt_founds
          i_scope_object_cls       = lt_scope
        EXCEPTIONS
          not_executed             = 1
          not_found                = 2
          illegal_object           = 3
          no_cross_for_this_object = 4
          batch                    = 5
          batchjob_error           = 6
          wrong_type               = 7
          object_not_exist         = 8
          OTHERS                   = 9.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      LOOP AT lt_founds ASSIGNING <ls_found>.
        APPEND INITIAL LINE TO lt_edges ASSIGNING <ls_edge>.
        <ls_edge>-from = <ls_node>.

        <ls_edge>-to-obj_name   = <ls_found>-object.
        CASE <ls_found>-object_cls.
          WHEN 'DS'
              OR 'DT'.
            <ls_edge>-to-obj_type = 'TABL'.
          WHEN 'DA'.
            <ls_edge>-to-obj_type = 'TTYP'.
          WHEN OTHERS.
            _raise 'resolve_ddic, unknown object_cls'.
        ENDCASE.
      ENDLOOP.

    ENDLOOP.

    DO.
      lv_before = lines( lt_nodes ).
      LOOP AT lt_nodes ASSIGNING <ls_node>.
        lv_index = sy-tabix.
        READ TABLE lt_edges WITH KEY
          from-obj_name = <ls_node>-obj_name
          from-obj_type = <ls_node>-obj_type
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          LOOP AT ct_tadir ASSIGNING <ls_tadir>
              WHERE obj_name = <ls_node>-obj_name
              AND object = <ls_node>-obj_type.
            <ls_tadir>-korrnum = <ls_tadir>-korrnum + lv_plus.
            CONDENSE <ls_tadir>-korrnum.
          ENDLOOP.
          DELETE lt_edges
            WHERE to-obj_name = <ls_node>-obj_name
            AND to-obj_type = <ls_node>-obj_type.
          DELETE lt_nodes INDEX lv_index.
          EXIT. " make sure the sequence is fixed
        ENDIF.
      ENDLOOP.
      IF lv_before = lines( lt_nodes ).
        EXIT.
      ENDIF.
      lv_plus = lv_plus + 1.
    ENDDO.

  ENDMETHOD.                    "resolve_ddic

  METHOD delete_obj.

    DATA: li_obj TYPE REF TO lif_object.


    li_obj = create_object( is_item = is_item
                            iv_language = gc_english ).
    li_obj->delete( ).

  ENDMETHOD.                    "delete

  METHOD serialize.

    DATA: lt_files TYPE ty_files_tt,
          li_obj   TYPE REF TO lif_object,
          lo_xml   TYPE REF TO lcl_xml_output,
          lo_files TYPE REF TO lcl_objects_files.


    CREATE OBJECT lo_files
      EXPORTING
        is_item = is_item.

    li_obj = create_object( is_item = is_item
                            iv_language = iv_language ).
    li_obj->mo_files = lo_files.
    CREATE OBJECT lo_xml.
    li_obj->serialize( lo_xml ).
    lo_files->add_xml( io_xml      = lo_xml
                       is_metadata = li_obj->get_metadata( ) ).

    rt_files = lo_files->get_files( ).

* check for duplicates
    lt_files[] = rt_files[].
    SORT lt_files BY path ASCENDING filename ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_files COMPARING path filename.
    IF lines( lt_files ) <> lines( rt_files ).
      _raise 'Duplicates'.
    ENDIF.

  ENDMETHOD.                    "serialize

  METHOD prioritize_deser.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF it_results.

* PROG before internet services, as the services might use the screens
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'PROG'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

* ISAP has to be handled before ISRP
    LOOP AT it_results ASSIGNING <ls_result> WHERE obj_type = 'IASP'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

    LOOP AT it_results ASSIGNING <ls_result>
        WHERE obj_type <> 'IASP' AND obj_type <> 'PROG'.
      APPEND <ls_result> TO rt_results.
    ENDLOOP.

  ENDMETHOD.                    "prioritize_deser

  METHOD deserialize.

    TYPES: BEGIN OF ty_late,
             obj     TYPE REF TO lif_object,
             xml     TYPE REF TO lcl_xml_input,
             package TYPE devclass,
           END OF ty_late.

    DATA: ls_item    TYPE ty_item,
          lv_cancel  TYPE abap_bool,
          li_obj     TYPE REF TO lif_object,
          lt_remote  TYPE ty_files_tt,
          lv_package TYPE devclass,
          lo_files   TYPE REF TO lcl_objects_files,
          lo_xml     TYPE REF TO lcl_xml_input,
          lt_results TYPE ty_results_tt,
          lt_late    TYPE TABLE OF ty_late.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results,
                   <ls_late>   LIKE LINE OF lt_late.


    lcl_objects_activation=>clear( ).

    lt_remote = io_repo->get_files_remote( ).

    lt_results = lcl_file_status=>status( io_repo ).
    DELETE lt_results WHERE match = abap_true.
    SORT lt_results BY obj_type ASCENDING obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_results COMPARING obj_type obj_name.

    lt_results = prioritize_deser( lt_results ).

    warning_overwrite( EXPORTING io_repo = io_repo
                       CHANGING ct_results = lt_results ).

    LOOP AT lt_results ASSIGNING <ls_result>.
      lcl_progress=>show( iv_key     = 'Deserialize'
                          iv_current = sy-tabix
                          iv_total   = lines( lt_results )
                          iv_text    = <ls_result>-obj_name ) ##NO_TEXT.

      CLEAR ls_item.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.
* handle namespaces
      REPLACE ALL OCCURRENCES OF '#' IN ls_item-obj_name WITH '/'.

      lv_package = path_to_package(
        iv_top   = io_repo->get_package( )
        iv_start = io_repo->get_dot_abapgit( )->get_starting_folder( )
        iv_path  = <ls_result>-path ).

      lv_cancel = warning_package( is_item    = ls_item
                                   iv_package = lv_package ).
      IF lv_cancel = abap_true.
        _raise 'cancelled'.
      ENDIF.

      CREATE OBJECT lo_files
        EXPORTING
          is_item = ls_item.
      lo_files->set_files( lt_remote ).

* Analyze XML in order to instantiate the proper serializer
      lo_xml = lo_files->read_xml( ).

      li_obj = create_object( is_item     = ls_item
                              iv_language = io_repo->get_master_language( )
                              is_metadata = lo_xml->get_metadata( ) ).

      li_obj->mo_files = lo_files.

      IF li_obj->get_metadata( )-late_deser = abap_true.
        APPEND INITIAL LINE TO lt_late ASSIGNING <ls_late>.
        <ls_late>-obj = li_obj.
        <ls_late>-xml = lo_xml.
        <ls_late>-package = lv_package.
        CONTINUE.
      ENDIF.

      li_obj->deserialize( iv_package = lv_package
                           io_xml     = lo_xml ).

    ENDLOOP.

    lcl_objects_activation=>activate( ).

    LOOP AT lt_late ASSIGNING <ls_late>.
      <ls_late>-obj->deserialize( iv_package = <ls_late>-package
                                  io_xml     = <ls_late>-xml ).
    ENDLOOP.

    update_package_tree( io_repo->get_package( ) ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object IMPLEMENTATION

*----------------------------------------------------------------------*
*       INTERFACE lif_gui_page DEFINITION
*----------------------------------------------------------------------*
INTERFACE lif_gui_page.

  METHODS on_event
    IMPORTING iv_action       TYPE clike
              iv_frame        TYPE clike
              iv_getdata      TYPE clike
              it_postdata     TYPE cnht_post_data_tab
              it_query_table  TYPE cnht_query_table
    RETURNING VALUE(rv_state) TYPE i
    RAISING   lcx_exception.

  METHODS render
    RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
    RAISING   lcx_exception.

  METHODS get_assets
    RETURNING VALUE(rt_assets) TYPE tt_web_assets.

ENDINTERFACE.

*----------------------------------------------------------------------*
*       CLASS lcl_gui_router DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_gui_router DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS on_event
      IMPORTING iv_action   TYPE clike
                iv_getdata  TYPE clike OPTIONAL
                it_postdata TYPE cnht_post_data_tab OPTIONAL
      EXPORTING ei_page     TYPE REF TO lif_gui_page
                ev_state    TYPE i
      RAISING   lcx_exception.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_popup,
             url         TYPE string,
             package     TYPE devclass,
             branch_name TYPE string,
             cancel      TYPE abap_bool,
           END OF ty_popup.

    METHODS get_page_by_name
      IMPORTING iv_name        TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS repo_popup
      IMPORTING iv_url          TYPE string
                iv_package      TYPE devclass OPTIONAL
                iv_branch       TYPE string DEFAULT 'refs/heads/master'
      RETURNING VALUE(rs_popup) TYPE ty_popup
      RAISING   lcx_exception.

    METHODS get_page_diff
      IMPORTING iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS get_page_branch_overview
      IMPORTING iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS get_page_stage
      IMPORTING iv_key         TYPE lcl_persistence_repo=>ty_repo-key
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS get_page_commit
      IMPORTING iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS get_page_db_by_name
      IMPORTING iv_name        TYPE clike
                iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS abapgit_installation
      RAISING lcx_exception.

    METHODS repo_clone
      IMPORTING iv_url TYPE string
      RAISING   lcx_exception.

    METHODS repo_purge
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS repo_remove
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS repo_new_offline
      RAISING lcx_exception.

    METHODS repo_package_zip
      RAISING lcx_exception.

    METHODS repo_pull
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS switch_branch
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS reset
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS create_branch_popup
      EXPORTING ev_name   TYPE string
                ev_cancel TYPE abap_bool
      RAISING   lcx_exception.

    METHODS create_branch
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS db_delete
      IMPORTING iv_getdata TYPE clike
      RAISING   lcx_exception.

    METHODS db_save
      IMPORTING it_postdata TYPE cnht_post_data_tab
      RAISING   lcx_exception.

    METHODS commit_push
      IMPORTING it_postdata TYPE cnht_post_data_tab
      RAISING   lcx_exception.

    METHODS stage_handle_action
      IMPORTING iv_getdata TYPE clike
                iv_action  TYPE clike
      RAISING   lcx_exception.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_gui DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_gui DEFINITION FINAL CREATE PRIVATE FRIENDS lcl_app.

  PUBLIC SECTION.

    METHODS go_home
      RAISING lcx_exception.

    METHODS back
      IMPORTING iv_to_bookmark TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_exit) TYPE xfeld
      RAISING   lcx_exception.

    METHODS on_event FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING action frame getdata postdata query_table.  "#EC NEEDED

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_page_stack,
             page     TYPE REF TO lif_gui_page,
             bookmark TYPE abap_bool,
           END OF ty_page_stack.

    DATA: mi_cur_page    TYPE REF TO lif_gui_page,
          mt_stack       TYPE TABLE OF ty_page_stack,
          mt_assets      TYPE tt_w3urls,
          mo_router      TYPE REF TO lcl_gui_router,
          mo_html_viewer TYPE REF TO cl_gui_html_viewer.

    METHODS constructor
      RAISING lcx_exception.

    METHODS startup
      RAISING lcx_exception.

    METHODS cache_image
      IMPORTING iv_url    TYPE w3url
                iv_base64 TYPE string.

    METHODS cache_html
      IMPORTING iv_html       TYPE string
      RETURNING VALUE(rv_url) TYPE w3url.

    METHODS render
      RAISING lcx_exception.

    METHODS call_page
      IMPORTING ii_page          TYPE REF TO lif_gui_page
                iv_with_bookmark TYPE abap_bool DEFAULT abap_false
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_gui DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_repo_offline IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_repo_offline IMPLEMENTATION.

  METHOD set_files_remote.

    mt_remote = it_files.

    find_dot_abapgit( ).

  ENDMETHOD.

ENDCLASS.                    "lcl_repo_offline IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_repo_online IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_repo_online IMPLEMENTATION.

  METHOD constructor.

    super->constructor( is_data ).

    mv_initialized = abap_false.

  ENDMETHOD.                    "constructor

  METHOD initialize.
    IF mv_initialized = abap_false.
      refresh( ).
    ENDIF.
  ENDMETHOD.

  METHOD status.

    initialize( ).

    rt_results = lcl_file_status=>status( io_repo = me
                                          io_log  = io_log ).

  ENDMETHOD.                    "status

  METHOD deserialize.

    initialize( ).

    super->deserialize( ).

    set( iv_sha1 = mv_branch ).

  ENDMETHOD.                    "deserialize

  METHOD refresh.

    super->refresh( ).

    lcl_progress=>show( iv_key     = 'Fetch'
                        iv_current = 1
                        iv_total   = 1
                        iv_text    = 'Remote files' ) ##NO_TEXT.

    lcl_git_porcelain=>pull( EXPORTING io_repo    = me
                             IMPORTING et_files   = mt_remote
                                       et_objects = mt_objects
                                       ev_branch  = mv_branch ).

    find_dot_abapgit( ).

    mv_initialized = abap_true.

  ENDMETHOD.                    "refresh

  METHOD get_sha1_remote.
    initialize( ).

    rv_sha1 = mv_branch.
  ENDMETHOD.                    "get_sha1_remote

  METHOD get_files_remote.
    initialize( ).

    rt_files = mt_remote.
  ENDMETHOD.                    "get_files

  METHOD get_objects.
    initialize( ).

    rt_objects = mt_objects.
  ENDMETHOD.                    "get_objects

  METHOD get_url.
    rv_url = ms_data-url.
  ENDMETHOD.                    "get_url

  METHOD get_branch_name.
    rv_name = ms_data-branch_name.
  ENDMETHOD.                    "get_branch_name

  METHOD set_url.

    mv_initialized = abap_false.
    set( iv_url = iv_url ).

  ENDMETHOD.

  METHOD set_branch_name.

    mv_initialized = abap_false.
    set( iv_branch_name = iv_branch_name ).

  ENDMETHOD.

  METHOD get_sha1_local.
    rv_sha1 = ms_data-sha1.
  ENDMETHOD.                    "get_sha1_local

  METHOD push.

    DATA: lv_branch TYPE ty_sha1.

    ASSERT get_key( ) = io_stage->mv_repo_key.

    handle_stage_ignore( io_stage ).

    lv_branch = lcl_git_porcelain=>push( is_comment = is_comment
                                         io_repo    = me
                                         io_stage   = io_stage ).

    set( iv_sha1 = lv_branch ).

    refresh( ).

    set( it_checksums = build_local_checksums( ) ).

  ENDMETHOD.                    "push

  METHOD handle_stage_ignore.

    DATA: lt_stage TYPE lcl_stage=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage.

    ASSERT get_key( ) = io_stage->mv_repo_key.

    lt_stage = io_stage->get_all( ).
    LOOP AT lt_stage ASSIGNING <ls_stage> WHERE method = lcl_stage=>c_method-ignore.

      mo_dot_abapgit->add_ignore(
        iv_path     = <ls_stage>-file-path
        iv_filename = <ls_stage>-file-filename ).

      " remove it from the staging object, as the action is handled here
      io_stage->reset( iv_path     = <ls_stage>-file-path
                       iv_filename = <ls_stage>-file-filename ).

      io_stage->update_and_add_dot_abapgit( mo_dot_abapgit->serialize( ) ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.                    "lcl_repo_online IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_repo IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_repo IMPLEMENTATION.

  METHOD constructor.

    ASSERT NOT is_data-key IS INITIAL.

    ms_data = is_data.

  ENDMETHOD.                    "constructor

  METHOD find_dot_abapgit.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF mt_remote.


    READ TABLE mt_remote ASSIGNING <ls_remote>
      WITH KEY path = gc_root_dir
      filename = gc_dot_abapgit.
    IF sy-subrc = 0.
      mo_dot_abapgit = lcl_dot_abapgit=>deserialize( <ls_remote>-data ).
    ENDIF.

  ENDMETHOD.

  METHOD get_files_remote.
    rt_files = mt_remote.
  ENDMETHOD.

  METHOD set.

    DATA: lo_persistence TYPE REF TO lcl_persistence_repo.


    ASSERT iv_sha1 IS SUPPLIED
      OR it_checksums IS SUPPLIED
      OR iv_url IS SUPPLIED
      OR iv_branch_name IS SUPPLIED.

    CREATE OBJECT lo_persistence.

    IF iv_sha1 IS SUPPLIED.
      lo_persistence->update_sha1(
        iv_key         = ms_data-key
        iv_branch_sha1 = iv_sha1 ).
      ms_data-sha1 = iv_sha1.
    ENDIF.

    IF it_checksums IS SUPPLIED.
      lo_persistence->update_local_checksums(
        iv_key       = ms_data-key
        it_checksums = it_checksums ).
      ms_data-local_checksums = it_checksums.
    ENDIF.

    IF iv_url IS SUPPLIED.
      lo_persistence->update_url(
        iv_key = ms_data-key
        iv_url = iv_url ).
      ms_data-url = iv_url.
    ENDIF.

    IF iv_branch_name IS SUPPLIED.
      lo_persistence->update_branch_name(
        iv_key         = ms_data-key
        iv_branch_name = iv_branch_name ).
      ms_data-branch_name = iv_branch_name.
    ENDIF.

  ENDMETHOD.                    "set_sha1

  METHOD build_local_checksums.

    DATA: lv_xstring TYPE xstring,
          lt_local   TYPE ty_files_item_tt.

    FIELD-SYMBOLS: <ls_item>     LIKE LINE OF lt_local,
                   <ls_checksum> LIKE LINE OF rt_checksums,
                   <ls_local>    LIKE LINE OF lt_local.


    lt_local = get_files_local( ).

    LOOP AT lt_local ASSIGNING <ls_item> WHERE NOT item IS INITIAL.

      CLEAR lv_xstring.

      LOOP AT lt_local ASSIGNING <ls_local> WHERE item = <ls_item>-item.
        CONCATENATE lv_xstring <ls_local>-file-data INTO lv_xstring IN BYTE MODE.
      ENDLOOP.

      APPEND INITIAL LINE TO rt_checksums ASSIGNING <ls_checksum>.
      <ls_checksum>-item = <ls_item>-item.
      ASSERT NOT lv_xstring IS INITIAL.
      <ls_checksum>-sha1 = lcl_hash=>sha1_raw( lv_xstring ).

      DELETE lt_local WHERE item = <ls_item>-item.

    ENDLOOP.

  ENDMETHOD.

  METHOD deserialize.

    IF mo_dot_abapgit->get_master_language( ) <> sy-langu.
      _raise 'Current login language does not match master language'.
    ENDIF.

    lcl_objects=>deserialize( me ).

    CLEAR mt_local.

    set( it_checksums = build_local_checksums( ) ).

  ENDMETHOD.

  METHOD get_local_checksums.
    rt_checksums = ms_data-local_checksums.
  ENDMETHOD.

  METHOD get_files_local.

    DATA: lt_tadir TYPE lcl_tadir=>ty_tadir_tt,
          ls_item  TYPE ty_item,
          lt_files TYPE ty_files_tt.

    FIELD-SYMBOLS: <ls_file>   LIKE LINE OF lt_files,
                   <ls_return> LIKE LINE OF rt_files,
                   <ls_tadir>  LIKE LINE OF lt_tadir.


    IF lines( mt_local ) > 0.
      rt_files = mt_local.
      RETURN.
    ENDIF.

    IF mo_dot_abapgit IS INITIAL.
      mo_dot_abapgit = lcl_dot_abapgit=>build_default( ms_data-master_language ).
    ENDIF.
    APPEND INITIAL LINE TO rt_files ASSIGNING <ls_return>.
    <ls_return>-file-path     = gc_root_dir.
    <ls_return>-file-filename = gc_dot_abapgit.
    <ls_return>-file-data     = mo_dot_abapgit->serialize( ).

    lt_tadir = lcl_tadir=>read( get_package( ) ).
    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      lcl_progress=>show( iv_key     = 'Serialize'
                          iv_current = sy-tabix
                          iv_total   = lines( lt_tadir )
                          iv_text    = <ls_tadir>-obj_name ) ##NO_TEXT.

      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.

      IF lcl_objects=>is_supported( ls_item ) = abap_false.
        IF NOT io_log IS INITIAL.
          io_log->add( iv_msgv1 = 'Object type ignored, not supported:'
                       iv_msgv2 = ls_item-obj_type
                       iv_msgv3 = '-'
                       iv_msgv4 = ls_item-obj_name ) ##no_text.
        ENDIF.
        CONTINUE.
      ENDIF.

      lt_files = lcl_objects=>serialize( is_item = ls_item
                                         iv_language = get_master_language( ) ).
      LOOP AT lt_files ASSIGNING <ls_file>.
        <ls_file>-path = mo_dot_abapgit->get_starting_folder( ) && <ls_tadir>-path.

        APPEND INITIAL LINE TO rt_files ASSIGNING <ls_return>.
        <ls_return>-file = <ls_file>.
        <ls_return>-item = ls_item.
      ENDLOOP.
    ENDLOOP.

    mt_local = rt_files.

  ENDMETHOD.

  METHOD get_dot_abapgit.
    ro_dot_abapgit = mo_dot_abapgit.
  ENDMETHOD.

  METHOD delete.

    DATA: lo_persistence TYPE REF TO lcl_persistence_repo.


    CREATE OBJECT lo_persistence.

    lo_persistence->delete( ms_data-key ).

  ENDMETHOD.                    "delete

  METHOD is_offline.
    rv_offline = ms_data-offline.
  ENDMETHOD.

  METHOD refresh.
    CLEAR mt_local.
  ENDMETHOD.                    "refresh

  METHOD get_package.
    rv_package = ms_data-package.
  ENDMETHOD.                    "get_package

  METHOD get_master_language.
    rv_language = ms_data-master_language.
  ENDMETHOD.

  METHOD get_key.
    rv_key = ms_data-key.
  ENDMETHOD.                    "get_key

  METHOD get_name.

    IF ms_data-offline = abap_true.
      rv_name = ms_data-url.
    ELSE.
      rv_name = lcl_url=>name( ms_data-url ).
    ENDIF.

  ENDMETHOD.                    "get_name

ENDCLASS.                    "lcl_repo IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_repo_srv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_repo_srv IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT mo_persistence.
  ENDMETHOD.                    "class_constructor

  METHOD list.

    IF mv_init = abap_false.
      refresh( ).
    ENDIF.

    rt_list = mt_list.

  ENDMETHOD.                    "list

  METHOD get.

    FIELD-SYMBOLS: <lo_list> LIKE LINE OF mt_list.


    IF mv_init = abap_false.
      refresh( ).
    ENDIF.

    LOOP AT mt_list ASSIGNING <lo_list>.
      IF <lo_list>->get_key( ) = iv_key.
        ro_repo = <lo_list>.
        RETURN.
      ENDIF.
    ENDLOOP.

    ASSERT 1 = 0.

  ENDMETHOD.                    "get

  METHOD refresh.

    DATA: lt_list    TYPE lcl_persistence_repo=>tt_repo,
          lo_online  TYPE REF TO lcl_repo_online,
          lo_offline TYPE REF TO lcl_repo_offline.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    CLEAR mt_list.

    lt_list = mo_persistence->list( ).
    LOOP AT lt_list ASSIGNING <ls_list>.
      IF <ls_list>-offline = abap_false.
        CREATE OBJECT lo_online
          EXPORTING
            is_data = <ls_list>.
        APPEND lo_online TO mt_list.
      ELSE.
        CREATE OBJECT lo_offline
          EXPORTING
            is_data = <ls_list>.
        APPEND lo_offline TO mt_list.
      ENDIF.
    ENDLOOP.

    mv_init = abap_true.

  ENDMETHOD.                    "refresh

  METHOD new_online.

    DATA: ls_repo TYPE lcl_persistence_repo=>ty_repo,
          lv_key  TYPE lcl_persistence_repo=>ty_repo-key.


    validate_package( iv_package ).

    lv_key = mo_persistence->add(
      iv_url         = iv_url
      iv_branch_name = iv_branch_name
      iv_package     = iv_package ).

    TRY.
        ls_repo = mo_persistence->read( lv_key ).
      CATCH lcx_not_found.
        _raise 'new_online not found'.
    ENDTRY.

    CREATE OBJECT ro_repo
      EXPORTING
        is_data = ls_repo.

    add( ro_repo ).

  ENDMETHOD.                    "new_online

  METHOD new_offline.

    DATA: ls_repo TYPE lcl_persistence_repo=>ty_repo,
          lv_key  TYPE lcl_persistence_repo=>ty_repo-key.


    validate_package( iv_package ).

    lv_key = mo_persistence->add(
      iv_url         = iv_url
      iv_branch_name = ''
      iv_package     = iv_package
      iv_offline     = abap_true ).

    TRY.
        ls_repo = mo_persistence->read( lv_key ).
      CATCH lcx_not_found.
        _raise 'new_offline not found'.
    ENDTRY.

    CREATE OBJECT ro_repo
      EXPORTING
        is_data = ls_repo.

    add( ro_repo ).

  ENDMETHOD.                    "new_offline

  METHOD add.

    DATA: lo_repo LIKE LINE OF mt_list.


    LOOP AT mt_list INTO lo_repo.
      IF lo_repo->get_key( ) = io_repo->get_key( ).
        IF lo_repo = io_repo.
          RETURN.
        ENDIF.
        _raise 'identical keys'.
      ENDIF.
    ENDLOOP.

    APPEND io_repo TO mt_list.

  ENDMETHOD.                    "add

  METHOD validate_package.

    DATA: lv_devclass TYPE tdevc-devclass,
          lt_repos    TYPE lcl_persistence_repo=>tt_repo.


    IF iv_package IS INITIAL.
      _raise 'add, package empty'.
    ENDIF.

    IF iv_package = '$TMP'.
      _raise 'not possible to use $TMP, create new (local) package'.
    ENDIF.

    SELECT SINGLE devclass FROM tdevc INTO lv_devclass
      WHERE devclass = iv_package
      AND as4user <> 'SAP'.                             "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'package not found or not allowed'.
    ENDIF.

    " make sure its not already in use for a different repository
    lt_repos = mo_persistence->list( ).
    READ TABLE lt_repos WITH KEY package = iv_package TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      _raise 'Package already in use'.
    ENDIF.

  ENDMETHOD.                    "validate_package

  METHOD delete.

    io_repo->delete( ).

    DELETE TABLE mt_list FROM io_repo.
    ASSERT sy-subrc = 0.

  ENDMETHOD.                    "delete

  METHOD is_repo_installed.

    DATA: lt_repo        TYPE lcl_repo_srv=>ty_repo_tt,
          lo_repo        TYPE REF TO lcl_repo,
          lv_url         TYPE string,
          lv_package     TYPE devclass,
          lo_repo_online TYPE REF TO lcl_repo_online,
          lv_err         TYPE string.

    lt_repo = list( ).

    LOOP AT lt_repo INTO lo_repo.
      CHECK lo_repo->is_offline( ) = abap_false.
      lo_repo_online ?= lo_repo.

      lv_url     = lo_repo_online->get_url( ).
      lv_package = lo_repo_online->get_package( ).
      CHECK to_upper( lv_url ) = to_upper( iv_url ).

      " Validate bindings
      "TODO refactor: move this message out of this method
      IF iv_target_package IS NOT INITIAL AND iv_target_package <> lv_package.
        lv_err = |Installation to package { lv_package } detected. |
              && |Cancelling installation|.
        _raise lv_err.
      ENDIF.

      rv_installed = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD. "is_repo_installed

  METHOD get_stage.

    DATA ls_stage LIKE LINE OF mt_stages.

    IF iv_new = abap_true.

      free_stage( iv_repo_key ). " Kill existing stage if any
      CREATE OBJECT ls_stage-stage EXPORTING iv_repo_key = iv_repo_key.
      ls_stage-repo_key = iv_repo_key.
      APPEND ls_stage TO mt_stages.

    ELSE.

      READ TABLE mt_stages INTO ls_stage WITH KEY repo_key = iv_repo_key.
      IF sy-subrc <> 0.
        _raise 'Existing stage not found'.
      ENDIF.

    ENDIF.

    ro_stage = ls_stage-stage.

  ENDMETHOD. "get_stage

  METHOD free_stage.

    DELETE mt_stages WHERE repo_key = iv_repo_key. " Kill existing stage if any

  ENDMETHOD. "free_stage

ENDCLASS.                    "lcl_repo_srv IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_zip DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zip DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS import
      IMPORTING iv_key TYPE lcl_persistence_db=>ty_value
      RAISING   lcx_exception.

    CLASS-METHODS export
      IMPORTING io_repo TYPE REF TO lcl_repo
                iv_zip  TYPE abap_bool DEFAULT abap_true
      RAISING   lcx_exception.

  PRIVATE SECTION.

    CLASS-METHODS file_upload
      RETURNING VALUE(rv_xstr) TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS unzip_file
      IMPORTING iv_xstr         TYPE xstring
      RETURNING VALUE(rt_files) TYPE ty_files_tt
      RAISING   lcx_exception.

    CLASS-METHODS filename
      IMPORTING iv_str             TYPE string
      RETURNING VALUE(rv_filename) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS file_download
      IMPORTING iv_package TYPE devclass
                iv_xstr    TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS files_commit
      IMPORTING it_files TYPE ty_files_item_tt
      RAISING   lcx_exception.

    CLASS-METHODS encode_files
      IMPORTING it_files       TYPE ty_files_item_tt
      RETURNING VALUE(rv_xstr) TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS get_message
      RETURNING VALUE(rv_message) TYPE string
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_zip DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_zip IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zip IMPLEMENTATION.

  METHOD get_message.

    DATA: lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'ABAPTXT255'.
    <ls_field>-fieldname = 'LINE'.
    <ls_field>-fieldtext = 'Commit message'.                "#EC NOTEXT
    <ls_field>-field_obl = abap_true.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Enter commit message'            "#EC NOTEXT
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      _raise 'Error from POPUP_GET_VALUES'.
    ENDIF.
    IF lv_returncode = 'A'.
      _raise 'cancelled'.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rv_message = <ls_field>-value.

  ENDMETHOD.                    "get_message

  METHOD file_download.

    DATA: lt_rawdata  TYPE solix_tab,
          lv_action   TYPE i,
          lv_filename TYPE string,
          lv_default  TYPE string,
          lv_path     TYPE string,
          lv_fullpath TYPE string.


    CONCATENATE iv_package '_' sy-datlo '_' sy-timlo INTO lv_default.

    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING
        window_title         = 'Export ZIP'
        default_extension    = 'zip'
        default_file_name    = lv_default
      CHANGING
        filename             = lv_filename
        path                 = lv_path
        fullpath             = lv_fullpath
        user_action          = lv_action
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).                         "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'error from file_save_dialog'.
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      _raise 'cancelled'.
    ENDIF.

    lt_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstr ).

    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize              = xstrlen( iv_xstr )
        filename                  = lv_fullpath
        filetype                  = 'BIN'
      CHANGING
        data_tab                  = lt_rawdata
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24 ).
    IF sy-subrc <> 0.
      _raise 'error from gui_download'.
    ENDIF.

  ENDMETHOD.                    "file_download

  METHOD encode_files.

    DATA: lo_zip      TYPE REF TO cl_abap_zip,
          lv_filename TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.


    CREATE OBJECT lo_zip.

    LOOP AT it_files ASSIGNING <ls_file>.
      CONCATENATE <ls_file>-file-path+1 <ls_file>-file-filename INTO lv_filename.
      lo_zip->add( name    = lv_filename
                   content = <ls_file>-file-data ).
    ENDLOOP.

    rv_xstr = lo_zip->save( ).

  ENDMETHOD.                    "encode_files

  METHOD filename.

    DATA: lv_path TYPE string.                              "#EC NEEDED


    IF iv_str CA '/'.
      FIND REGEX '(.*/)(.*)' IN iv_str
        SUBMATCHES lv_path rv_filename.
      IF sy-subrc <> 0.
        _raise 'Malformed path'.
      ENDIF.
    ELSE.
      rv_filename = iv_str.
    ENDIF.
    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.                    "filename

  METHOD file_upload.

    DATA: lt_data       TYPE TABLE OF x255,
          lt_file_table TYPE filetable,
          ls_file_table LIKE LINE OF lt_file_table,
          lv_action     TYPE i,
          lv_string     TYPE string,
          lv_rc         TYPE i,
          lv_length     TYPE i.


    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = 'Import ZIP'
        default_extension       = 'ZIP'
      CHANGING
        file_table              = lt_file_table
        rc                      = lv_rc
        user_action             = lv_action
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5 ).                      "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'error from file_open_dialog'.
    ENDIF.
    IF lv_action = cl_gui_frontend_services=>action_cancel.
      _raise 'cancelled'.
    ENDIF.

    READ TABLE lt_file_table INDEX 1 INTO ls_file_table.
    ASSERT sy-subrc = 0.
    lv_string = ls_file_table-filename.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename                = lv_string
        filetype                = 'BIN'
      IMPORTING
        filelength              = lv_length
      CHANGING
        data_tab                = lt_data
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19 ).
    IF sy-subrc <> 0.
      _raise 'error from gui_upload'.
    ENDIF.

    CONCATENATE LINES OF lt_data INTO rv_xstr IN BYTE MODE.
    rv_xstr = rv_xstr(lv_length).

  ENDMETHOD.                    "file_upload

  METHOD unzip_file.

    DATA: lo_zip    TYPE REF TO cl_abap_zip,
          lv_xstr   TYPE xstring,
          lt_splice TYPE cl_abap_zip=>t_splice_entries.

    FIELD-SYMBOLS: <ls_splice> LIKE LINE OF lt_splice,
                   <ls_file>   LIKE LINE OF rt_files.


    CREATE OBJECT lo_zip.
    lo_zip->load( EXPORTING
                    zip             = iv_xstr
                  EXCEPTIONS
                    zip_parse_error = 1
                    OTHERS          = 2 ).
    IF sy-subrc <> 0.
      _raise 'error from zip'.
    ENDIF.

    lt_splice = cl_abap_zip=>splice( iv_xstr ).

    LOOP AT lt_splice ASSIGNING <ls_splice>.
      lo_zip->get(
        EXPORTING
          name                    = <ls_splice>-name
        IMPORTING
          content                 = lv_xstr
        EXCEPTIONS
          zip_index_error         = 1
          zip_decompression_error = 2
          OTHERS                  = 3 ).
      IF sy-subrc <> 0.
        _raise 'error from zip get'.
      ENDIF.

      APPEND INITIAL LINE TO rt_files ASSIGNING <ls_file>.
      <ls_file>-path     = '/'.
      <ls_file>-filename = filename( <ls_splice>-name ).
      <ls_file>-data     = lv_xstr.

    ENDLOOP.

  ENDMETHOD.                    "decode_files

  METHOD export.

    DATA: lo_log TYPE REF TO lcl_log,
          lt_zip TYPE ty_files_item_tt.


    CREATE OBJECT lo_log.

    lt_zip = io_repo->get_files_local( lo_log ).

    IF lo_log->count( ) > 0.
      lo_log->show( ).
    ENDIF.

    IF iv_zip = abap_true.
      file_download( iv_package = io_repo->get_package( )
                     iv_xstr = encode_files( lt_zip ) ).
    ELSE.
      files_commit( lt_zip ).
    ENDIF.

  ENDMETHOD.                    "export_key

  METHOD import.

    DATA: lo_repo TYPE REF TO lcl_repo_offline.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).
    lo_repo->set_files_remote( unzip_file( file_upload( ) ) ).
    lo_repo->deserialize( ).

  ENDMETHOD.                    "import

  METHOD files_commit.

    DATA: lv_folder   TYPE string,
          lv_filename TYPE string,
          lv_par      TYPE string,
          lv_message  TYPE string,
          lt_rawdata  TYPE solix_tab.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.


    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = 'Select folder'
      CHANGING
        selected_folder      = lv_folder
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4 ).                         "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'error from directory_browser'.
    ENDIF.

    IF lv_folder IS INITIAL.
      RETURN.
    ENDIF.

    lv_message = get_message( ).

    LOOP AT it_files ASSIGNING <ls_file>.
      lt_rawdata = cl_bcs_convert=>xstring_to_solix( <ls_file>-file-data ).

      CONCATENATE lv_folder <ls_file>-file-path <ls_file>-file-filename INTO lv_filename.

      cl_gui_frontend_services=>gui_download(
        EXPORTING
          bin_filesize            = xstrlen( <ls_file>-file-data )
          filename                = lv_filename
          filetype                = 'BIN'
        CHANGING
          data_tab                = lt_rawdata
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          not_supported_by_gui    = 22
          error_no_gui            = 23
          OTHERS                  = 24 ).
      IF sy-subrc <> 0.
        _raise 'error from gui_download'.
      ENDIF.

    ENDLOOP.

* assumption: git command is in PATH
    cl_gui_frontend_services=>execute(
      EXPORTING
        application            = 'git'
        default_directory      = lv_folder
        synchronous            = 'X'
        parameter              = 'add *'
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10 ).                      "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'error from execute'.
    ENDIF.

* make sure to set git user.email and user.name manually
    lv_par = 'commit -m "' && lv_message && '"'.            "#EC NOTEXT
    cl_gui_frontend_services=>execute(
      EXPORTING
        application            = 'git'
        default_directory      = lv_folder
        synchronous            = 'X'
        parameter              = lv_par
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10 ).
    IF sy-subrc <> 0.
      _raise 'error from execute'.
    ENDIF.

  ENDMETHOD.                    "files_commit

ENDCLASS.                    "lcl_zip IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui IMPLEMENTATION.

  METHOD constructor.

    startup( ).

  ENDMETHOD.            "constructor

  METHOD on_event.

    DATA: lx_exception TYPE REF TO lcx_exception,
          li_page      TYPE REF TO lif_gui_page,
          lv_state     TYPE i.

    TRY.
        IF mi_cur_page IS BOUND.
          lv_state = mi_cur_page->on_event(
            iv_action      = action
            iv_frame       = frame
            iv_getdata     = getdata
            it_postdata    = postdata
            it_query_table = query_table ).
        ENDIF.

        IF lv_state IS INITIAL.
          mo_router->on_event(
            EXPORTING
              iv_action   = action
              iv_getdata  = getdata
              it_postdata = postdata
            IMPORTING
              ei_page     = li_page
              ev_state    = lv_state ).
        ENDIF.

        CASE lv_state.
          WHEN gc_event_state-re_render.
            render( ).
          WHEN gc_event_state-new_page.
            call_page( li_page ).
          WHEN gc_event_state-new_page_w_bookmark.
            call_page( ii_page = li_page iv_with_bookmark = abap_true ).
          WHEN gc_event_state-go_back.
            back( ).
          WHEN gc_event_state-go_back_to_bookmark.
            back( iv_to_bookmark = abap_true ).
          WHEN gc_event_state-no_more_act.
            " Do nothing, handling completed
          WHEN OTHERS.
            _raise 'Unknown action'.
        ENDCASE.

      CATCH lcx_exception INTO lx_exception.
        ROLLBACK WORK.
        MESSAGE lx_exception->mv_text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "on_event

  METHOD back.

    DATA: lv_index TYPE i,
          ls_stack LIKE LINE OF mt_stack.

    lv_index = lines( mt_stack ).

    IF lv_index = 0.
      rv_exit = abap_true.
      RETURN.
    ENDIF.

    DO lv_index TIMES.
      READ TABLE mt_stack INDEX lv_index INTO ls_stack.
      ASSERT sy-subrc = 0.

      DELETE mt_stack INDEX lv_index.
      ASSERT sy-subrc = 0.

      lv_index = lv_index - 1.

      IF iv_to_bookmark = abap_false OR ls_stack-bookmark = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    mi_cur_page = ls_stack-page. " last page always stays
    render( ).

  ENDMETHOD.                "back

  METHOD call_page.

    DATA: lt_assets TYPE tt_web_assets,
          ls_stack  TYPE ty_page_stack.
    FIELD-SYMBOLS <ls_asset> LIKE LINE OF lt_assets.

    IF NOT mi_cur_page IS INITIAL.
      ls_stack-page     = mi_cur_page.
      ls_stack-bookmark = iv_with_bookmark.
      APPEND ls_stack TO mt_stack.
    ENDIF.

    lt_assets = ii_page->get_assets( ).
    IF lines( lt_assets ) > 0.
      LOOP AT lt_assets ASSIGNING <ls_asset>.
        READ TABLE mt_assets TRANSPORTING NO FIELDS WITH KEY table_line = <ls_asset>-url.
        CHECK sy-subrc IS NOT INITIAL.
        APPEND <ls_asset>-url TO mt_assets.
        cache_image( iv_url = <ls_asset>-url iv_base64 = <ls_asset>-content ).
      ENDLOOP.
    ENDIF.

    mi_cur_page = ii_page.
    render( ).

  ENDMETHOD.                "call_page

  METHOD go_home.

    on_event( action = 'main' ).

  ENDMETHOD.                "go_home

  METHOD startup.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events.

    CREATE OBJECT mo_router.
    CREATE OBJECT mo_html_viewer
      EXPORTING
        query_table_disabled = abap_true
        parent               = cl_gui_container=>screen0.

    CLEAR ls_event.
    ls_event-eventid = mo_html_viewer->m_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.
    mo_html_viewer->set_registered_events( lt_events ).

    SET HANDLER me->on_event FOR mo_html_viewer.

  ENDMETHOD.                    "startup

  METHOD render.

    DATA lv_url TYPE w3url.

    lv_url = cache_html( mi_cur_page->render( )->mv_html ).

    mo_html_viewer->show_url( lv_url ).

  ENDMETHOD.                    "render

  METHOD cache_html.

    DATA: lt_data TYPE TABLE OF text200.

    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = iv_html
      TABLES
        ftext_tab = lt_data.

    mo_html_viewer->load_data(
      IMPORTING
        assigned_url = rv_url
      CHANGING
        data_table   = lt_data ).

  ENDMETHOD.                    "cache_html

  METHOD cache_image.

    DATA lv_xtmp  TYPE xstring.
    DATA lv_size  TYPE int4.
    DATA lt_xdata TYPE TABLE OF w3_mime. " RAW255

    CALL FUNCTION 'SSFC_BASE64_DECODE'
      EXPORTING
        b64data = iv_base64
      IMPORTING
        bindata = lv_xtmp
      EXCEPTIONS
        OTHERS  = 1.

    ASSERT sy-subrc = 0. " Image data error

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xtmp
      IMPORTING
        output_length = lv_size
      TABLES
        binary_tab    = lt_xdata.

    mo_html_viewer->load_data(
      EXPORTING  type         = 'image'
                 subtype      = 'png'
                 size         = lv_size
                 url          = iv_url
      CHANGING   data_table   = lt_xdata
      EXCEPTIONS OTHERS       = 1 ).

    ASSERT sy-subrc = 0. " Image data error

  ENDMETHOD.                  "cache_image

ENDCLASS.                     "lcl_gui IMPLEMENTATION

CLASS lcl_gui_page_super DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_gui_page ABSTRACT METHODS render.

  PROTECTED SECTION.
    METHODS header
      IMPORTING io_include_style TYPE REF TO lcl_html_helper OPTIONAL
      RETURNING VALUE(ro_html)   TYPE REF TO lcl_html_helper.

    METHODS footer
      IMPORTING io_include_script TYPE REF TO lcl_html_helper OPTIONAL
      RETURNING VALUE(ro_html)    TYPE REF TO lcl_html_helper.

    METHODS title
      IMPORTING iv_title       TYPE string
                io_menu        TYPE REF TO lcl_html_toolbar OPTIONAL
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS redirect
      IMPORTING iv_url         TYPE string
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

  PRIVATE SECTION.
    METHODS styles RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_super IMPLEMENTATION.

  METHOD header.

    CREATE OBJECT ro_html.

    ro_html->add( '<!DOCTYPE html>' ).                      "#EC NOTEXT
    ro_html->add( '<html>' ).                               "#EC NOTEXT
    ro_html->add( '<head>' ).                               "#EC NOTEXT
    ro_html->add( '<title>abapGit</title>' ).               "#EC NOTEXT
    ro_html->add( styles( ) ).

    IF io_include_style IS BOUND.
      ro_html->add( '<style type="text/css">' ).            "#EC NOTEXT
      ro_html->add( io_include_style ).
      ro_html->add( '</style>' ).                           "#EC NOTEXT
    ENDIF.

    ro_html->add( '<meta http-equiv="content-type" content="text/html; charset=utf-8">' ). "#EC NOTEXT
    ro_html->add( '</head>' ).                              "#EC NOTEXT
    ro_html->add( '<body>' ).                               "#EC NOTEXT

  ENDMETHOD.                    "render html header

  METHOD title.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="header">' ).                    "#EC NOTEXT
    ro_html->add( '<table width="100%"><tr>' ).             "#EC NOTEXT

    ro_html->add( '<td class="logo">' ).                    "#EC NOTEXT
    ro_html->add( '<a href="sapevent:abapgithome">' ).      "#EC NOTEXT
    ro_html->add( '<img src="img/logo">' ).                 "#EC NOTEXT
    ro_html->add( '</a>' ).                                 "#EC NOTEXT
    ro_html->add( '</td>' ).                                "#EC NOTEXT

    ro_html->add( '<td class="headpad"><span class="page_title">' ). "#EC NOTEXT
    ro_html->add( |&#x25BA; { iv_title }| ).                "#EC NOTEXT
    ro_html->add( '</span></td>' ).                         "#EC NOTEXT

    IF io_menu IS BOUND.
      ro_html->add( '<td class="headpad right">' ).         "#EC NOTEXT
      ro_html->add( io_menu->render( ) ).
      ro_html->add( '</td>' ).                              "#EC NOTEXT
    ENDIF.

    ro_html->add( '</tr></table>' ).                        "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.                    "render page title

  METHOD footer.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="footer">' ).                    "#EC NOTEXT
    ro_html->add( '<img src="img/logo" >' ).                "#EC NOTEXT
    ro_html->add( |<span class="version">{ gc_abap_version }</span>| ). "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT
    ro_html->add( '</body>' ).                              "#EC NOTEXT

    IF io_include_script IS BOUND.
      ro_html->add( '<script type="text/javascript">' ).
      ro_html->add( io_include_script ).
      ro_html->add( '</script>' ).
    ENDIF.

    ro_html->add( '</html>').                               "#EC NOTEXT

  ENDMETHOD.                    "render html footer & logo

  METHOD redirect.

    CREATE OBJECT ro_html.

    ro_html->add( '<!DOCTYPE html>' ).                      "#EC NOTEXT
    ro_html->add( '<html><head>' ).                         "#EC NOTEXT
    ro_html->add( |<meta http-equiv="refresh" content="0; url={ iv_url }">| ). "#EC NOTEXT
    ro_html->add( '</head></html>').                        "#EC NOTEXT

  ENDMETHOD.

  METHOD styles.

    DEFINE _add.
      ro_html->add( &1 ) ##NO_TEXT.
    END-OF-DEFINITION.

    CREATE OBJECT ro_html.

    _add '<style type="text/css">'.

    " Global styles
    _add '/* GLOBALS */'.
    _add 'body {'.
    _add '  font-family: Arial,Helvetica,sans-serif;'.
    _add '  font-size:    12pt;'.
    _add '  background: #E8E8E8;'.
    _add '}'.
    _add 'a, a:visited {'.
    _add '  color:            #4078c0;'.
    _add '  text-decoration:  none;'.
    _add '}'.
    _add 'a:hover, a:active {'.
    _add '  cursor: pointer;'.
    _add '  text-decoration: underline;'.
    _add '}'.
    _add 'img               { border: 0px; vertical-align: middle; }'.
    _add 'table             { border-collapse: collapse; }'.
    _add 'pre               { display: inline; }'.

    _add 'form input, textarea {'.
    _add '  border: 1px solid #DDD;'.
    _add '  padding: 3px 6px;'.
    _add '}'.
    _add 'form input:focus, textarea:focus {'.
    _add '  border: 1px solid #8cadd9;'.
    _add '}'.

    " Modifiers
    _add '/* MODIFIERS */'.
    _add '.grey             { color: lightgrey  !important; }'.
    _add '.emphasis         { font-weight: bold !important; }'.
    _add '.attention        { color: red        !important; }'.
    _add '.right            { text-align:right; }'.
    _add '.paddings         { padding: 0.5em 0.5em 0.5em 0.5em; }'.

    " Structure div styles: header, footer, toc
    _add '/* STRUCTURE DIVS, HEADER & FOOTER */'.
    _add 'td.headpad { padding-top: 11px; }'.
    _add 'td.logo    { width: 164px; }'.
    _add 'div#header {'.
    _add '  padding:          0.5em 0.5em 0.5em 0.5em;'.
    _add '  border-bottom:    3px double lightgrey;'.
    _add '}'.
    _add 'div#toc {'.
    _add '  padding:          0.5em 1em 0.5em 1em;'.
    _add '  background-color: #f2f2f2;'.
    _add '}'.
    _add 'div#footer {'.
    _add '  padding:          0.5em 1em 0.5em 1em;'.
    _add '  border-top:       3px double lightgrey;'.
    _add '  text-align:       center;'.
    _add '}'.
    _add 'div.dummydiv {'.
    _add '  background-color: #f2f2f2;'.
    _add '  padding:          0.5em 1em 0.5em 1em;'.
    _add '  text-align:       center;'.
    _add '}'.
    _add 'span.version {'.
    _add '  display: block;'.
    _add '  color: grey;'.
    _add '  margin-top: 0.3em;'.
    _add '}'.
    _add 'span.page_title {'.
    _add '  font-weight: normal;'.
    _add '  font-size: 18pt;'.
    _add '  color: #bbb;'.
    _add '  padding-left: 0.4em;'.
    _add '}'.

    " Menu styles
    _add '/* MENU */'.
    _add 'div.menu           { display: inline; }'.
    _add 'div.menu .menu_end { border-right: 0px !important; }'.
    _add 'div.menu a {'.
    _add '  padding-left: 0.5em;'.
    _add '  padding-right: 0.5em;'.
    _add '  border-right: 1px solid lightgrey;'.
    _add '  font-size: 12pt;'.
    _add '}'.
    _add 'div.menu_vertical   { display: inline; }'.
    _add 'div.menu_vertical a {'.
    _add '  display: block; '.
    _add '  font-size: 12pt;'.
    _add '}'.

    " Drop down styles
    _add '/*DROP DOWN*/'.
    _add '.dropdown {'.
    _add '    position: relative;'.
    _add '    display: inline;'.
    _add '}'.
    _add '.dropdown_content {'.
    _add '    display: none;'.
    _add '    z-index: 1;'.
    _add '    position: absolute;'.
    _add '    right: 0;'.
    _add '    top: 1.1em; /*IE7 woraround*/'.
    _add '    background-color: #f9f9f9;'.
    _add '    white-space: nowrap;'.
*    _add '    min-width: 10em;'.
    _add '    border-bottom: 1px solid lightgrey;'.
    _add '}'.
    _add '.dropdown_content a {'.
    _add '    padding: 0.2em;'.
    _add '    text-decoration: none;'.
    _add '    display: block;'.
    _add '}'.
    _add '.dropdown_content a:hover { background-color: #f1f1f1 }'.
    _add '.dropdown:hover .dropdown_content { display: block; }'.
    _add '.dropdown:hover .dropbtn  { color: #79a0d2; }'.

    " Other and outdated (?) styles
    _add '/* MISC AND REFACTOR */'.
    _add 'a.grey:link {color: grey; font-size: smaller;}'.
    _add 'a.grey:visited {color: grey; font-size: smaller;}'.
    _add 'a.plain:link {color: black; text-decoration: none;}'.
    _add 'a.plain:visited {color: black; text-decoration: none;}'.
    _add 'a.bkg:link {color: #E8E8E8;}'.
    _add 'a.bkg:visited {color: #E8E8E8;}'.
    _add 'h1 {display: inline;}'.
    _add 'h2 {display: inline;}'.
    _add 'h3 {'.
    _add '  display: inline;'.
    _add '  color: grey;'.
    _add '  font-weight:normal;'.
    _add '  font-size: smaller;'.
    _add '}'.

    _add '.hidden-submit {'.
    _add '  border: 0 none;'.
    _add '  height: 0;'.
    _add '  width: 0;'.
    _add '  padding: 0;'.
    _add '  margin: 0;'.
    _add '  overflow: hidden;'.
    _add '}'.

    _add '</style>'.

  ENDMETHOD.                    "common styles

  METHOD lif_gui_page~get_assets. " Common images here

    DATA ls_image TYPE ty_web_asset.

    ls_image-url     = 'img/logo'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAAKMAAAAoCAYAAACSG0qbAAAABHNCSVQICAgIfAhkiAAA'
      && 'AAlwSFlzAAAEJQAABCUBprHeCQAAABl0RVh0U29mdHdhcmUAd3d3Lmlua3NjYXBlLm9y'
      && 'Z5vuPBoAAA8VSURBVHic7Zx7cJzVeYef31nJAtvYko1JjM3FYHlXimwZkLWyLEMcwIGQ'
      && 'cEkDJWmTltLStGkoDCkzwBAuCemUlksDNCkhJTTTljJpZhIuBQxxAWPvyuYiW7UkG8Il'
      && 'UByIsS1sLEu75+0fu5JXu9/etAJz0TOzM/rOec85765+37m+3yczY8w0NU3qrwv9npfa'
      && 'Hfx02pPPd469sgk+7misYnyjpWXy5IOG7kd8ZjjNjEtr13TdOm7eTfCxwo2lUJAQASRu'
      && '2dnRfMn4uDbBx42yxZhPiMNMCHKCsVK2GGuqqqoQUwrZTAhygrFQshjfaGmZ/M7yxQtm'
      && 'xGL9/qDqzwLxQvYTgpygXEoS4/DQ7LE1O05atLBu1YZdE4KcYLwpupoOmCO+5Z2dXPfE'
      && 'xk07Tm2ZroGhBwX1wAygKqiOiVX2Rw9Jam/gyH0wuGGzvTEudRYSY4HFyogghxN2n7Sw'
      && 'IendvcCioLoOtCCXNeqohOf0oDwPq9f3Wt/77dOHlWhYzUj/BRybTnrGEnZO5wv2m0rq'
      && 'DezJoOiqeZbzegzpk6TVPPWJTT39y5svMogF1ZcesjlQgkwYp4F+EJQXwv4E+MiLUZJa'
      && 'F7AIcRq4hWZ2mMRhQD/oZcErXv7FScaja3rt/wpU9E/sFyLACQq57wB/XIl/gWIstn2T'
      && 'xpHVre7ZW71p8sFDeQscSEHKu3pTBadNH2Lq61VT57iwNazLgaNSqYaUaWXLDZCJIbBo'
      && 'g3tK2A2xHns0oMrm3CRrqdTPnAVMiUIEmLlz2XGLMxNmH7YrifFcoUIHalHj8f8p6UfA'
      && 'O+932weStno1zghps6Q7GBFiUYRxopkeaZ2vIwLyfxtQ4vV8lbWHNScacf+T/vwqn90o'
      && 'MZYhRADJ+bv725vmj6Q8tHWffPKUD6IgO/tsfawneRHYd97Pdg8kSyJaZiGtBY4pYPYO'
      && 'kH84C0Cyv8tKSiK7OZ99EpYAJ2V8AhkRY5lCHGaxhaq+BLCzY/EXd5y0aOG0td1vf1AF'
      && 'CWCw7/1u80DQEtahQvcB03MyjQfM7Hwnmxfv9dPivX5SssqOwuzPSqk71mN3ymw5ZtdK'
      && 'dmVIdly8xx7JZ29yy0qptwrGLMRRCA6T1w93nLTo5Lq13Zv625tOMRd6DLF4v0lWmQO8'
      && 'qPko45y7TWaHZyUnwa6M99mN2fYbuu1V4K5oxF1B4Z4UgFifrQHWFLNbvkh1QheV5DNN'
      && 'TZMqFWIGs5zX48M95PTqGa3TZ4erzbvj8/WUErf0L2++uNyGJLn2Js1oDeuYlkbNbmlR'
      && 'deXup2hq0qS2es2VlHMDFaOlRdXL5uuwlnodG23QTEljCkbJV3d7WHOK+dXWqHqZnZeb'
      && 'Y1fGe3OFOArRU5GTGbSHNWdwUL8Epo1qIQ9V/bXu3HES4jCznNfjb7e1zZ8Ri/UD1MLz'
      && 'u05s/huMx4IKGNy4+8Tj/2Pqk8++Vaji86TQqxEuNNM5rWGtSCaokSDkgd0QjbidoPvN'
      && '+5s7t9jz5TgdbdBMvLsG2cop6FgLUdUaZk804jYKuyrWa6vzlT2+XrOqQnxd6KwQOj5R'
      && 'hULpL9Yaxkcj7g3QT6zK397ZbdtGtbtAZ+B0U3adkt0c67E7OyI6fFDuSpktC6HGpJjU'
      && 'GmZ3NOI2mdnVnX32eHZZ7903hGXfBG8mp3J7sd/B0DPCTgUmBf9O7lmMybk56or3Jn8f'
      && 'oLVB7Q5dZ9Iy4OBsw2jYbUUk96fwQrzHf955iBZzsDA+aL9k1owZ20fNzaY/tfFXwK48'
      && 'ldQkSZ5YqJXmZk15JaJfmOmfgdOAmgCzWrCvyum5aIO+Uor3AIbOx7QV2TeBMPu3vKYA'
      && 'Sw091hbWt4PKRhu0oDqkmND1wAnk3vkOmAN2lRLa2hrWMVm5Tek2R3286YzWiK4eQltk'
      && '9g1gMfsFMhVYKunR1obQddk+SXZqwLe8acMGe7fYb9HZk7wm3utrBmpsqiXsyClHMHK6'
      && '0hLWoRjHBfmLbP9K3bPYjFPIFWLaQeZnlZ8H4JyFflrMwcK4wG63v3/ycZnXOzqalxE0'
      && 'mU7x9rvvVv93oVZqBtzNGGeU7Jbp9pZGzS7ReiVQVyDfmXRda4PaA9p5mBLmWGmmSron'
      && 'M0FytUGGgjPTAi8UIeVk9u1og5YOJ0QbNBOjIac+Y22JPgLQ1WV7Ol+w36xebYnhtGpj'
      && 'FjBYTj3l4KY9/dx6My4d74pN/Ki/Y9HpSG5HR/Nyh/1DHtO9OM6dvWFDwbtWslOykt6U'
      && 's5VWZbOFnQtsyMqvc56Ty3T7NeBhLGAfDZDpe5nX6V5uXpbZ43K2NGQ2V9glwLas/I62'
      && 'hfrE8EWsJ3mFsGYs+OQqze+A1cBLgbmma4f/9AmOJGBe5vKVLYN1W6wnOWSHmdkVhexM'
      && 'PG6yC0x2AbmjoQ3njdh4uwrSw1Htmq5bd3Y0I3FLpQ5n0GTSQ7s6Fva70RPYTPbi+Pz0'
      && 'J7ryboRC+m5PnRfsJjVEAfp5bLNflTb52dKIBj36RWY5ZyX2WCLukvbX67ZYHFLHZtGw'
      && '+1fD/jDL8qQljWpav9m6Uw3wKYzXgUNJTxsk+0Fssw0L6x+j4dCx6eF/BEtwDBkbx7Fe'
      && '29gWCa0yrC2rvXXO26WZfrWG3V2kji8zWbm0QUev67GX5ZgZ8A0H121hXIIZNrxou9oW'
      && '6m4b4m/z2aTP+fsAohF3PaNHROvssZ8ElRs5DnyPBAkovxDFF4oJESDeY9tJD4Ur5umg'
      && 'PSFm1Uy23Zk2SaM7e43p5Y4uxUMzu2f4H56+tuZmff2gfTqHrGEy5DkW6Abo7LH7gfsB'
      && '2uo1LQGzBmoYFSwg57vNcjqqo4F1JXh2S7Zfx83TZZNqdD6MXkQkU369jONgcmfxe83M'
      && 'B7XQEdEhg1B0HzDk2ZHpy3vBqLPpMQhyi/f2AIA3WyPZG6KkeVpKiE925awEi7H6JRsA'
      && 'cqJDfIi9oayfW8ZB5dY/TFeX7YlGQg+RmgJkcnSQfWyr9QP92enmGcgeNCvx67mXbGdb'
      && 'xD1hjI5AklJ+ydgTUGz6iiZNXd09+gYGGIRlQgXn6wDesZYSRFsJOYES5QjSw7fqnu7q'
      && 'Bqh7uqu7f3nzdw3uKFJszEIcpqVRs12SRuAYiTrJ1YXMzSGgS6iQnHmWyQWe70pySz/F'
      && 'MZagMWnMlaiTuTqTTih7s7IIHm1T1ncVI37l3BAAA4McAYF7iAvG17uxExi1U6Igd9XN'
      && 'Dj+UmZA8qPrf3MDQbeSPIN8Ldub0JzeWLcT2I3Swn8JFhr4VQnMze5uKnv0ugOHfUXa3'
      && 'ZhySedkR0eGDuMtbw/rTZCI1pA9PF0yWf4e3MnJ7YKXm0pOr6H03QRIIZeYnUj1njhid'
      && '8aaRscKX/VGWSRLsCjnK2rcdC3njGUsQ5PSdv92yqJaMk5WBoRMpJsSnNgZufBdCkmsN'
      && '60FgRbllK8PNzOlttT/qpz2sOUnpeWGHvq9ewcyc28/7XQCru213NOL+l6wgZ0kXAjnD'
      && 'cazP7gXuTdu41rCyxbgr3mt/P16+F6LgUVXtmq5bC237yNsNu5YtPBZgx4kLFznZ1XlM'
      && 'BzB/1liECBAN801yhfiq0HflbKXz1ojZ4qCylSBsbm6q/93wX0n0Q1Ir6UzWYXaZyZaF'
      && 'qqxeZn813n4ZlhPWJWXMo00P5OTDF5c0qmm8fRlPip6bFhHk6Ti3ddfy5i3OXBemJQE2'
      && 'A5g/c/qaTasC8krC0KdzE+3qWG/y6thmW7Vui/UkQ7w51vqDaGnRZFInPdlshNQ2C8oJ'
      && 'h0oqaefF++zmzh5bu7bbXrBxjp88bp5qgZzNdyfWD/9t+B+TO4GW8/p+R0SHcGBxLWEF'
      && 'jiQlHeIXEaRIPZAVRMVCTDcQCUh8LfOyaqjgCcr+YpY7NRFa2VY/egsqtNtdw8ie5gjJ'
      && 'oUTqicjofOYA2f/YgcR03s5MMBF4wlIa7rMr5mnUyru6xl0LZAeFvDG3l83DF5199muk'
      && 'oJO1FUMoviSi8Nh9Kg+Ru7qvUvCqPO+cMZsxbPsM4HXW9KcrEyKApTa7s9BVSyLaF3Ik'
      && 'SbLSQros18RyInkkV2u5q+6zLaS+aCT0oJl/QVI78IWcsvDos1vtLYCE551QKNuCKW63'
      && '+157g36cMOYI9yWhC3K+j4KDEHKxC9+t0altDaFHwL/kvVZIBJw761/uM5/MTJlU7S/Z'
      && 'N6hTBNlhZA0OPReNuGdM6nL4jR4G5ZnRusAtKmVHwg1Slcxe11nODZJKh1fJ6kwM3dQa'
      && 'VgOw3omjkGuL9/o/L/vFTzs7mi8pQZBpIT4f9PxE2bRFQncY9pdjKDoExDH7ebzPbgFo'
      && 'bQjdng48KBfvzZau77ORN61FI66PsW2N7ARiZnZTZ589BtAWCV1v5J1zF+JNVdui2CbL'
      && 'OcJsq1ejD2lVgCDL4e14r58J0N6k+cmEu0HYIssdrbxgnaGeeG9yJEg32hC6GbOix81y'
      && 'trTsWLtiixpgQNLZ4yVEgCT++xSP0H7C0N1ZadVAh6SR3kRm2WfJO0H/XqTuQcn+IlOI'
      && 'AFjRVaZhus3g2az0WuA0wcIi5QP3DDNIIPtakBABYltts7AO4OEi9eTFYGCksSRzwM4L'
      && 'ECKAM1gG9tVR5UP+RkqZN5s7a0yBnwUEOSDp7GlPPp83BH0srO+1PmQrDIIen9wOdnln'
      && 'n31G5n9ZtDLL6ck2x3uTf6DUee8rASX6vNnyWI/dmZ0R77O7LNXLBkWy9CE7Pd6XvNih'
      && 'QkEQeZHZl9PBFtsDstebtyWFwv0B4r32UrzXn+6xDtBdwIslNL0N+JnMvravxiraFO/s'
      && 'tm0y+xzQlcfkddCNCe/vGfP7GQH6lzdfbHAjqSCBHZK+PN5CzESSlixgnhMLzXAeXp+3'
      && 'hWfuM0sWL10abQv1CdtHixzvmtiYPhcvSFOTJk1NEPEQkWdPUry4oc96y2o3YJiWs5Wx'
      && 'zbYq83THHHu9Y1N2kG45tDRqdsgzxxuznKPOGbsTsN2M7d6zfXhePJ5Ici1h6mUcAcw0'
      && '8Zo5fp35NoqKxAjwTrRhZmLSpPY9ySmPzV27dm+lTn9cKSTGA+XT+03Jq+l8HBLv2Q7c'
      && 'X9K+ygQTFGDcHhaaoGJyouDNV7JH+eGj4mF6gspoC+tzJt1ObsT4MDsF2zxs886+Ml5v'
      && '/PogUvEwPUGFiE+SX4gAtQa1gkhV7onQR4oJMR5oxC6stDeghd7Dh6E+CPw/HL4vVO2f'
      && 'cpUAAAAASUVORK5CYII='.
    APPEND ls_image TO rt_assets.

  ENDMETHOD.                    "lif_gui_page~get_assets

  METHOD lif_gui_page~on_event.
    rv_state = gc_event_state-not_handled.
  ENDMETHOD.                    "lif_gui_page~on_event

ENDCLASS.

CLASS lcl_gui_page_branch_overview DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_repo TYPE REF TO lcl_repo_online,
      lif_gui_page~render REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo TYPE REF TO lcl_repo_online.

ENDCLASS.                       "lcl_gui_page_explore DEFINITION

CLASS lcl_gui_page_branch_overview IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_repo = io_repo.
  ENDMETHOD.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( ) ).
    ro_html->add( title( 'BRANCH_OVERVIEW' ) ).

    ro_html->add( 'hello world' ).
    ro_html->add( '<svg width="100" height="100">' ).
    ro_html->add( '<circle cx="50" cy="50" r="40" stroke="green" stroke-width="4" fill="yellow" />' ).
    ro_html->add( '</svg>' ).
    ro_html->add( '<canvas id="myCanvas" width="200" height="100" style="border:1px solid #000000;">' ).
    ro_html->add( '</canvas>' ).
    ro_html->add( '<script>' ).
    ro_html->add( 'var c = document.getElementById("myCanvas");' ).
    ro_html->add( 'var ctx = c.getContext("2d");' ).
    ro_html->add( 'ctx.moveTo(0,0);' ).
    ro_html->add( 'ctx.lineTo(200,100);' ).
    ro_html->add( 'ctx.stroke();' ).
    ro_html->add( '</script>' ).

    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui_page_explore DEFINITION FINAL INHERITING FROM lcl_gui_page_super.
  PUBLIC SECTION.
    METHODS lif_gui_page~render REDEFINITION.

ENDCLASS.                       "lcl_gui_page_explore DEFINITION

CLASS lcl_gui_page_explore IMPLEMENTATION.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.
    ro_html->add( redirect( 'http://larshp.github.io/abapGit/explore.html' ) ).

  ENDMETHOD.

ENDCLASS.                       "lcl_gui_page_explore IMPLEMENTATION


CLASS lcl_gui_page_main DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~render     REDEFINITION.
    METHODS lif_gui_page~get_assets REDEFINITION.

    CLASS-METHODS render_repo_top
      IMPORTING io_repo        TYPE REF TO lcl_repo
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_repo_item,
             obj_type TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             is_first TYPE abap_bool,
             files    TYPE tt_repo_files,
           END OF ty_repo_item.
    TYPES   tt_repo_items TYPE STANDARD TABLE OF ty_repo_item WITH DEFAULT KEY.

    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS render_error
      IMPORTING ix_error       TYPE REF TO lcx_exception
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS render_toc
      IMPORTING it_list        TYPE lcl_repo_srv=>ty_repo_tt
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS build_main_menu
      RETURNING VALUE(ro_menu) TYPE REF TO lcl_html_toolbar.

    METHODS render_repo_menu
      IMPORTING io_repo        TYPE REF TO lcl_repo
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS render_repo
      IMPORTING io_repo        TYPE REF TO lcl_repo
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS extract_repo_content
      IMPORTING io_repo       TYPE REF TO lcl_repo
      EXPORTING et_repo_items TYPE tt_repo_items
                eo_log        TYPE REF TO lcl_log
      RAISING   lcx_exception.

    METHODS render_repo_item
      IMPORTING io_repo        TYPE REF TO lcl_repo
                is_item        TYPE ty_repo_item
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS render_obj_jump_link
      IMPORTING iv_obj_type    TYPE tadir-object
                iv_obj_name    TYPE tadir-obj_name
      RETURNING VALUE(rv_html) TYPE string.

    METHODS render_explore
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS needs_installation
      RETURNING VALUE(rv_not_completely_installed) TYPE abap_bool.

ENDCLASS.

CLASS lcl_gui_page_diff DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_local  TYPE ty_file
        is_remote TYPE ty_file.

    METHODS lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.
    DATA: mv_filename TYPE string,
          mo_diff     TYPE REF TO lcl_diff.

    METHODS styles       RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_head  RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_diff  RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_lines RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_diff IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    mv_filename = is_local-filename.

    CREATE OBJECT mo_diff
      EXPORTING
        iv_local  = is_local-data
        iv_remote = is_remote-data.

  ENDMETHOD.

  METHOD styles.
    DATA lo_html TYPE REF TO lcl_html_helper.
    CREATE OBJECT lo_html.

    lo_html->add( '/* DIFF */' ).                           "#EC NOTEXT
    lo_html->add( 'div.diff {' ).                           "#EC NOTEXT
    lo_html->add( '  background-color: #f2f2f2;' ).         "#EC NOTEXT
    lo_html->add( '  padding: 0.7em    ' ).                 "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'div.diff_head {' ).                      "#EC NOTEXT
    lo_html->add( '  border-bottom: 1px solid #DDD;' ).     "#EC NOTEXT
    lo_html->add( '  padding-bottom: 0.7em;' ).             "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'span.diff_name {' ).                     "#EC NOTEXT
    lo_html->add( '  padding-left: 0.5em;' ).               "#EC NOTEXT
    lo_html->add( '  color: grey;' ).                       "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'span.diff_name strong {' ).              "#EC NOTEXT
    lo_html->add( '  color: #333;' ).                       "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'span.diff_banner {' ).                   "#EC NOTEXT
    lo_html->add( '  border-style: solid;' ).               "#EC NOTEXT
    lo_html->add( '  border-width: 1px;' ).                 "#EC NOTEXT
    lo_html->add( '  border-radius: 3px;' ).                "#EC NOTEXT
    lo_html->add( '  padding-left: 0.3em;' ).               "#EC NOTEXT
    lo_html->add( '  padding-right: 0.3em;' ).              "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( '.diff_ins {' ).                          "#EC NOTEXT
    lo_html->add( '  border-color: #38e038;' ).             "#EC NOTEXT
    lo_html->add( '  background-color: #91ee91 !important;' ). "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( '.diff_del {' ).                          "#EC NOTEXT
    lo_html->add( '  border-color: #ff8093;' ).             "#EC NOTEXT
    lo_html->add( '  background-color: #ffb3be !important;' ). "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( '.diff_upd {' ).                          "#EC NOTEXT
    lo_html->add( '  border-color: #dada00;' ).             "#EC NOTEXT
    lo_html->add( '  background-color: #ffffb3 !important;' ). "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'div.diff_content {' ).                   "#EC NOTEXT
    lo_html->add( '  background: #fff;' ).                  "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT

    " Table part
    lo_html->add( '/* DIFF TABLE */' ).                     "#EC NOTEXT
    lo_html->add( 'table.diff_tab {' ).                     "#EC NOTEXT
    lo_html->add( '  font-family: Consolas, Courier, monospace;' ). "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab th {' ).                  "#EC NOTEXT
    lo_html->add( '  color: grey;' ).                       "#EC NOTEXT
    lo_html->add( '  text-align: left;' ).                  "#EC NOTEXT
    lo_html->add( '  font-weight: normal;' ).               "#EC NOTEXT
    lo_html->add( '  padding: 0.5em;' ).                    "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab td {' ).                  "#EC NOTEXT
    lo_html->add( '  color: #444;' ).                       "#EC NOTEXT
    lo_html->add( '  padding-left: 0.5em;' ).               "#EC NOTEXT
    lo_html->add( '  padding-right: 0.5em;' ).              "#EC NOTEXT
    lo_html->add( '  font-size: 12pt;' ).                   "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab td.num, th.num {' ).      "#EC NOTEXT
    lo_html->add( '  text-align: right;' ).                 "#EC NOTEXT
    lo_html->add( '  color: #ccc;' ).                       "#EC NOTEXT
    lo_html->add( '  border-left: 1px solid #eee;' ).       "#EC NOTEXT
    lo_html->add( '  border-right: 1px solid #eee;' ).      "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab td.cmd, th.cmd {' ).      "#EC NOTEXT
    lo_html->add( '  text-align: center !important;' ).     "#EC NOTEXT
    lo_html->add( '  white-space: nowrap;' ).               "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab tr.diff_nav_line {').     "#EC NOTEXT
    lo_html->add( '  background-color: #edf2f9;').          "#EC NOTEXT
    lo_html->add( '}').                                     "#EC NOTEXT
    lo_html->add( 'table.diff_tab tr.diff_nav_line td {').  "#EC NOTEXT
    lo_html->add( '  color: #ccc;').                        "#EC NOTEXT
    lo_html->add( '}').                                     "#EC NOTEXT
    lo_html->add( 'table.diff_tab code {' ).                "#EC NOTEXT
    lo_html->add( '  font-family: inherit;' ).              "#EC NOTEXT
    lo_html->add( '  white-space: pre;' ).                  "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT

    ro_html = lo_html.
  ENDMETHOD.

  METHOD render_head.
    DATA: lo_html  TYPE REF TO lcl_html_helper,
          ls_stats TYPE lcl_diff=>ty_count.

    CREATE OBJECT lo_html.

    ls_stats = mo_diff->stats( ).

    lo_html->add( '<div class="diff_head">' ).              "#EC NOTEXT
    lo_html->add( |<span class="diff_banner diff_ins">+ { ls_stats-insert }</span>| ).
    lo_html->add( |<span class="diff_banner diff_del">- { ls_stats-delete }</span>| ).
    lo_html->add( |<span class="diff_banner diff_upd">~ { ls_stats-update }</span>| ).
    lo_html->add( '<span class="diff_name">' ).             "#EC NOTEXT
    lo_html->add( |{ mv_filename }| ).
    lo_html->add( '</span>' ).                              "#EC NOTEXT
    lo_html->add( '</div>' ).                               "#EC NOTEXT

    ro_html = lo_html.
  ENDMETHOD.

  METHOD render_diff.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="diff">' ).                   "#EC NOTEXT
    ro_html->add( render_head( ) ).

    " Content
    ro_html->add( '<div class="diff_content">' ).           "#EC NOTEXT
    ro_html->add( '<table width="100%" class="diff_tab">' ). "#EC NOTEXT
    ro_html->add(   '<tr>' ).                               "#EC NOTEXT
    ro_html->add(   '<th class="num"></th>' ).              "#EC NOTEXT
    ro_html->add(   '<th>@LOCAL</th>' ).                    "#EC NOTEXT
    ro_html->add(   '<th class="num"></th>' ).              "#EC NOTEXT
    ro_html->add(   '<th>@REMOTE</th>' ).                   "#EC NOTEXT
    ro_html->add(   '</tr>' ).                              "#EC NOTEXT
    ro_html->add( render_lines( ) ).
    ro_html->add( '</table>' ).                             "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT

    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.

  METHOD render_lines.

    DATA: lt_diffs       TYPE lcl_diff=>ty_diffs_tt,
          lv_local       TYPE string,
          lv_remote      TYPE string,
          lv_attr_local  TYPE string,
          lv_attr_remote TYPE string,
          lv_beacon      TYPE string,
          lv_insert_nav  TYPE abap_bool.

    FIELD-SYMBOLS <ls_diff>  LIKE LINE OF lt_diffs.


    CREATE OBJECT ro_html.
    lt_diffs = mo_diff->get( ).

    LOOP AT lt_diffs ASSIGNING <ls_diff>.
      IF <ls_diff>-short = abap_false.
        lv_insert_nav = abap_true.
        CONTINUE.
      ENDIF.

      IF lv_insert_nav = abap_true. " Insert separator line with navigation
        IF <ls_diff>-beacon > 0.
          READ TABLE mo_diff->mt_beacons INTO lv_beacon INDEX <ls_diff>-beacon.
        ELSE.
          lv_beacon = '---'.
        ENDIF.

        ro_html->add( '<tr class="diff_nav_line">').
        ro_html->add( '<td class="num"></td>' ).
        ro_html->add( |<td colspan="3">@@ { <ls_diff>-local_line } @@ { lv_beacon }</td>| ).
        ro_html->add( '</tr>' ).
        lv_insert_nav = abap_false.
      ENDIF.

      lv_local  = escape( val = <ls_diff>-local  format = cl_abap_format=>e_html_attr ).
      lv_remote = escape( val = <ls_diff>-remote format = cl_abap_format=>e_html_attr ).

      CLEAR: lv_attr_local, lv_attr_remote. " Class for changed lines
      CASE <ls_diff>-result.
        WHEN lcl_diff=>c_diff-insert.
          lv_attr_local  = ' class="diff_ins"'.             "#EC NOTEXT
        WHEN lcl_diff=>c_diff-delete.
          lv_attr_remote = ' class="diff_del"'.             "#EC NOTEXT
        WHEN lcl_diff=>c_diff-update.
          lv_attr_local  = ' class="diff_upd"'.             "#EC NOTEXT
          lv_attr_remote = ' class="diff_upd"'.             "#EC NOTEXT
      ENDCASE.

      ro_html->add( '<tr>' ).                               "#EC NOTEXT
      ro_html->add( |<td class="num">{ <ls_diff>-local_line }</td>| ). "#EC NOTEXT
      ro_html->add( |<td{ lv_attr_local }><code>{ lv_local }</code></td>| ). "#EC NOTEXT
      ro_html->add( |<td class="num">{ <ls_diff>-remote_line }</td>| ). "#EC NOTEXT
      ro_html->add( |<td{ lv_attr_remote }><code>{ lv_remote }</code></td>| ). "#EC NOTEXT
      ro_html->add( '</tr>' ).                              "#EC NOTEXT

    ENDLOOP.

  ENDMETHOD.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'DIFF' ) ).
    ro_html->add( render_diff( ) ).
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_background DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: run
      RAISING lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS: push
      IMPORTING io_repo TYPE REF TO lcl_repo_online
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_gui_page_background_run DEFINITION FINAL
    INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      lif_gui_page~on_event REDEFINITION,
      lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.
    DATA: mt_text TYPE TABLE OF string.

    METHODS: run.

ENDCLASS.

CLASS lcl_gui_page_background_run IMPLEMENTATION.

  METHOD lif_gui_page~on_event.
    RETURN.
  ENDMETHOD.

  METHOD run.

    DATA: lx_error TYPE REF TO lcx_exception,
          lv_text  TYPE string,
          lv_line  TYPE i VALUE 1.


    TRY.
        lcl_background=>run( ).

        DO.
          READ LINE lv_line LINE VALUE INTO lv_text.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          APPEND lv_text TO mt_text.
          lv_line = lv_line + 1.
        ENDDO.
      CATCH lcx_exception INTO lx_error.
        APPEND lx_error->mv_text TO mt_text.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lv_text LIKE LINE OF mt_text.


    run( ).

    CREATE OBJECT ro_html.

    ro_html->add( header( ) ).
    ro_html->add( title( 'BACKGROUND_RUN' ) ).
    ro_html->add( '<div id="toc">' ).
    LOOP AT mt_text INTO lv_text.
      ro_html->add( '<pre>' && lv_text && '</pre><br>' ).
    ENDLOOP.
    ro_html->add( '</div>' ).
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui_page_background DEFINITION FINAL
    INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      lif_gui_page~on_event REDEFINITION,
      lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.

    METHODS:
      parse_fields
        IMPORTING iv_getdata       TYPE clike
        RETURNING VALUE(rs_fields) TYPE lcl_persistence_background=>ty_background,
      render_data
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      save
        IMPORTING iv_getdata TYPE clike
        RAISING   lcx_exception.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_app IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.

  METHOD run.

    IF sy-batch = abap_true.
      lcl_background=>run( ).
    ELSE.
      gui( )->go_home( ).
      CALL SELECTION-SCREEN 1001. " trigger screen
    ENDIF.

  ENDMETHOD.      "run

  METHOD gui.

    IF go_gui IS NOT BOUND.
      CREATE OBJECT go_gui.
    ENDIF.
    ro_gui = go_gui.

  ENDMETHOD.      "gui

  METHOD user.

    IF iv_user = sy-uname ##USER_OK.
      IF go_current_user IS NOT BOUND.
        CREATE OBJECT go_current_user.
      ENDIF.
      ro_user = go_current_user.
    ELSE.
      CREATE OBJECT ro_user
        EXPORTING
          iv_user = iv_user.
    ENDIF.

  ENDMETHOD.      "user

  METHOD repo_srv.

    IF go_repo_srv IS NOT BOUND.
      CREATE OBJECT go_repo_srv.
    ENDIF.
    ro_repo_srv = go_repo_srv.

  ENDMETHOD.      "repo_srv

  METHOD db.

    IF go_db IS NOT BOUND.
      CREATE OBJECT go_db.
    ENDIF.
    ro_db = go_db.

  ENDMETHOD.      "repo_srv

ENDCLASS.   "lcl_app


CLASS lcl_gui_page_background IMPLEMENTATION.

  METHOD parse_fields.

    DEFINE _field.
      READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = &1.
      IF sy-subrc = 0.
        rs_fields-&2 = <ls_field>-value.
      ENDIF.
    END-OF-DEFINITION.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    lv_string = iv_getdata.     " type conversion
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    _field 'key' key.
    _field 'method' method.
    _field 'username' username.
    _field 'password' password.

    ASSERT NOT rs_fields IS INITIAL.

  ENDMETHOD.

  METHOD lif_gui_page~on_event.

    CASE iv_action.
      WHEN 'save'.
        save( iv_getdata ).
        rv_state = gc_event_state-re_render.
    ENDCASE.

  ENDMETHOD.

  METHOD save.

    DATA: ls_fields      TYPE lcl_persistence_background=>ty_background,
          lo_persistence TYPE REF TO lcl_persistence_background.


    ls_fields = parse_fields( iv_getdata ).

    CREATE OBJECT lo_persistence.

    IF ls_fields-method = lcl_persistence_background=>c_method-nothing.
      lo_persistence->delete( ls_fields-key ).
    ELSE.
      lo_persistence->modify( ls_fields ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.

  METHOD render_data.

    DATA: lo_repo    TYPE REF TO lcl_repo,
          lo_online  TYPE REF TO lcl_repo_online,
          lo_per     TYPE REF TO lcl_persistence_background,
          lt_per     TYPE lcl_persistence_background=>tt_background,
          ls_per     LIKE LINE OF lt_per,
          lv_nothing TYPE string,
          lv_push    TYPE string,
          lv_pull    TYPE string,
          lt_list    TYPE lcl_repo_srv=>ty_repo_tt.


    CREATE OBJECT ro_html.

    ro_html->add( '<div id="toc">' ).
    ro_html->add( 'Listing online repositories' ) ##NO_TEXT.
    ro_html->add( '<br><br>' ).

    CREATE OBJECT lo_per.
    lt_per = lo_per->list( ).
    lt_list = lcl_app=>repo_srv( )->list( ).

    LOOP AT lt_list INTO lo_repo.
      IF lo_repo->is_offline( ) = abap_false.
        lo_online ?= lo_repo.

        READ TABLE lt_per INTO ls_per WITH KEY key = lo_online->get_key( ).
        IF sy-subrc <> 0.
          CLEAR ls_per.
        ENDIF.

        CLEAR lv_push.
        CLEAR lv_pull.
        CLEAR lv_nothing.
        CASE ls_per-method.
          WHEN lcl_persistence_background=>c_method-push.
            lv_push = ' checked'.
          WHEN lcl_persistence_background=>c_method-pull.
            lv_pull = ' checked'.
          WHEN OTHERS.
            lv_nothing = ' checked'.
        ENDCASE.

        ro_html->add( '<h1>' && lo_online->get_name( ) && '</h1>' ).
        ro_html->add( '<form method="get" action="sapevent:save">' ).
        ro_html->add( '<input type="hidden" name="key" value="' &&
          lo_repo->get_key( ) && '">' ).
        ro_html->add( '<input type="radio" name="method" value="nothing"' &&
          lv_nothing && '>Do nothing<br>' )  ##NO_TEXT.
        ro_html->add( '<input type="radio" name="method" value="push"' &&
          lv_push && '>Automatic push<br>' )  ##NO_TEXT.
        ro_html->add( '<input type="radio" name="method" value="pull"' &&
          lv_pull && '>Automatic pull<br>' )  ##NO_TEXT.
        ro_html->add( '<br>' ).
        ro_html->add( 'Authentication, optional<br>' )  ##NO_TEXT.
        ro_html->add( '(password will be saved in clear text)<br>' )  ##NO_TEXT.
        ro_html->add( '<table>' ).
        ro_html->add( '<tr>' ).
        ro_html->add( '<td>Username:</td>' ).
        ro_html->add( '<td><input type="text" name="username" value="' &&
          ls_per-username && '"></td>' ).
        ro_html->add( '</tr>' ).
        ro_html->add( '<tr>' ).
        ro_html->add( '<td>Password:</td>' ).
        ro_html->add( '<td><input type="text" name="password" value="' &&
          ls_per-password && '"></td>' ).
        ro_html->add( '</tr>' ).
        ro_html->add( '<tr><td colspan="2" align="right">' ).
        ro_html->add( '<input type="submit" value="Save">' ).
        ro_html->add( '</td></tr>' ).
        ro_html->add( '</table>' ).
        ro_html->add( '</form>' ).
        ro_html->add( '<br>' ).
      ENDIF.
    ENDLOOP.

    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA lo_toolbar TYPE REF TO lcl_html_toolbar.

    CREATE OBJECT lo_toolbar.
    CREATE OBJECT ro_html.

    lo_toolbar->add( iv_txt = 'Run background logic'
                     iv_act = 'background_run' ).

    ro_html->add( header( ) ).
    ro_html->add( title( iv_title = 'BACKGROUND' io_menu = lo_toolbar ) ).
    ro_html->add( render_data( ) ).
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui_page_commit DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_repo_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo  TYPE REF TO lcl_repo_online,
          mo_stage TYPE REF TO lcl_stage.

    METHODS render_menu
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS render_stage
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS render_form
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS scripts
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_commit IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    mo_repo ?= lcl_app=>repo_srv( )->get( iv_repo_key ).
    mo_stage = lcl_app=>repo_srv( )->get_stage( iv_repo_key ).
  ENDMETHOD.

  METHOD render_stage.

    DATA: lt_stage TYPE lcl_stage=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage.

    CREATE OBJECT ro_html.

    lt_stage = mo_stage->get_all( ).

    ro_html->add( '<table class="stage_tab">' ).
    ro_html->add( '<tr class="title firstrow">').
    ro_html->add( '<td colspan="2">Staged files</td>').
    ro_html->add( '</tr>' ).

    LOOP AT lt_stage ASSIGNING <ls_stage>.
      ro_html->add( '<tr>' ).
      ro_html->add( '<td class="method">' ).
      ro_html->add( lcl_stage=>method_description( <ls_stage>-method ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '<td>' ).
      ro_html->add( <ls_stage>-file-path && <ls_stage>-file-filename ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.

    ro_html->add( '</table>' ).

  ENDMETHOD.    "render_stage

  METHOD render_form.
    DATA: lo_user  TYPE REF TO lcl_persistence_user,
          lv_user  TYPE string,
          lv_key   TYPE string,
          lv_email TYPE string.

* see https://git-scm.com/book/ch5-2.html
* commit messages should be max 50 characters
* body should wrap at 72 characters

    lo_user  = lcl_app=>user( ).
    lv_user  = lo_user->get_username( ).
    lv_email = lo_user->get_email( ).
    lv_key   = mo_repo->get_key( ).

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="form_div">' ).
    ro_html->add( '<form id="commit_form" method="post" action="sapevent:commit_post">' ).
    ro_html->add( |<input name="key" type="hidden" value="{ lv_key }">| ).
    ro_html->add( '<table>' ).

    ro_html->add( '<tr>' ).
    ro_html->add( '<td class="field_name">username</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( |<input name="username" type="text" size="50" value="{ lv_user }">| ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).

    ro_html->add( '<tr>' ).
    ro_html->add( '<td class="field_name">email</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( |<input name="email" type="text" size="50" value="{ lv_email }">| ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).

    ro_html->add( '<tr>' ).
    ro_html->add( '<td class="field_name">comment</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( '<input name="comment" type="text"' &&
                  ' id="commit_msg" maxlength="50" size="50">' ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).

    ro_html->add( '<tr>' ).
    ro_html->add( '<td class="field_name">body</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( '<textarea name="body" rows="10" cols="50"></textarea>' ).

    ro_html->add( '<input type="submit" class="hidden-submit">' ). "Hmmm ... reconsider

    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).

    ro_html->add( '</table>' ).
    ro_html->add( '</form>' ).

    ro_html->add( '</div>' ).

  ENDMETHOD.    "render_form

  METHOD render_menu.

    DATA lo_toolbar TYPE REF TO lcl_html_toolbar.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    lo_toolbar->add( iv_act = 'submitCommit();'
                     iv_txt = 'Commit'
                     iv_typ = gc_action_type-onclick
                     iv_opt = gc_html_opt-emphas ) ##NO_TEXT.

    lo_toolbar->add( iv_act = 'commit_cancel'
                     iv_txt = 'Cancel'
                     iv_opt = gc_html_opt-cancel ) ##NO_TEXT.

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.      "render_menu

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'COMMIT' ) ).

    ro_html->add( '<div class="repo">' ).
    ro_html->add( lcl_gui_page_main=>render_repo_top( mo_repo ) ).
    ro_html->add( render_menu( ) ).
    ro_html->add( render_form( ) ).
    ro_html->add( render_stage( ) ).
    ro_html->add( '</div>' ).

    ro_html->add( footer( io_include_script = scripts( ) ) ).

  ENDMETHOD.  "lif_gui_page~render

  METHOD styles.
    CREATE OBJECT ro_html.

    ro_html->add('/* REPOSITORY */').
    ro_html->add('div.repo {').
    ro_html->add('  margin-top:       3px;').
    ro_html->add('  background-color: #f2f2f2;').
    ro_html->add('  padding: 0.5em 1em 0.5em 1em;').
    ro_html->add('}').
    ro_html->add('.repo_name span {').
    ro_html->add('  color: #333;').
    ro_html->add('  font-weight: bold;').
    ro_html->add('  font-size: 14pt;').
    ro_html->add('}').
    ro_html->add('.repo_name img {').
    ro_html->add('  vertical-align: baseline;').
    ro_html->add('  margin: 0 5px 0 5px;').
    ro_html->add('}').
    ro_html->add('.repo_attr {').
    ro_html->add('  color: grey;').
    ro_html->add('  font-size: 12pt;').
    ro_html->add('}').
    ro_html->add('.repo_attr span {').
    ro_html->add('  margin-left: 0.2em;').
    ro_html->add('  margin-right: 0.5em;').
    ro_html->add('}').
    ro_html->add('.repo_attr input {').
    ro_html->add('  color: grey;').     " Input wants it personaly
    ro_html->add('  font-size: 12pt;'). " Input wants it personaly
    ro_html->add('  margin-left: 0.5em;').
    ro_html->add('  margin-right: 0.5em;').
    ro_html->add('  background-color: transparent;').
    ro_html->add('  border-style: none;').
    ro_html->add('  text-overflow: ellipsis;').
    ro_html->add('}').

    ro_html->add('/* STAGE */').
    ro_html->add('.stage_tab {').
    ro_html->add('  border: 1px solid #DDD;').
    ro_html->add('  background: #fff;').
    ro_html->add('  margin-top: 0.2em;').
    ro_html->add('}').
    ro_html->add('.stage_tab td {').
    ro_html->add('  border-top: 1px solid #eee;').
    ro_html->add('  color: #333;').
    ro_html->add('  vertical-align: middle;').
    ro_html->add('  padding: 2px 0.5em;').
    ro_html->add('}').
    ro_html->add('.stage_tab td.method {').
    ro_html->add('  color: #ccc;').
    ro_html->add('}').
    ro_html->add('.stage_tab tr.firstrow td { border-top: 0px; } ' ).
    ro_html->add('.stage_tab tr.title td {').
    ro_html->add('  color: #BBB;').
    ro_html->add('  font-size: 10pt;').
    ro_html->add('  background-color: #edf2f9;').
    ro_html->add('  padding: 4px 0.5em;').
    ro_html->add('  text-align: center;').
    ro_html->add('}').

    ro_html->add('/* COMMIT */').
    ro_html->add('div.form_div {').
    ro_html->add('  margin: 0.5em 0em;').
    ro_html->add('  background-color: #F8F8F8;').
    ro_html->add('  padding: 1em 1em;').
    ro_html->add('}').
    ro_html->add('div.form_div td.field_name {').
    ro_html->add('  color: #BBB;').
    ro_html->add('  padding-right: 1em;').
    ro_html->add('}').

  ENDMETHOD.    "styles

  METHOD scripts.

    CREATE OBJECT ro_html.

    ro_html->add( 'function setInitialFocus() {' ).
    ro_html->add( '  document.getElementById("commit_msg").focus();' ).
    ro_html->add( '}' ).
    ro_html->add( 'function submitCommit() {' ).
    ro_html->add( '  document.getElementById("commit_form").submit();' ).
    ro_html->add( '}' ).
    ro_html->add( 'setInitialFocus();' ).

  ENDMETHOD.    "scripts

ENDCLASS.       "lcl_gui_page_commit

CLASS lcl_gui_page_stage DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_repo TYPE REF TO lcl_repo_online
      RAISING   lcx_exception.

    METHODS lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo  TYPE REF TO lcl_repo_online,
          mo_stage TYPE REF TO lcl_stage.

    METHODS render_lines
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS render_menu
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_stage IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).
    mo_repo   = io_repo.
    mo_stage  = lcl_app=>repo_srv( )->get_stage( iv_repo_key = mo_repo->get_key( )
                                                 iv_new      = abap_true ).
  ENDMETHOD.

  METHOD render_lines.

    DATA: lv_method  TYPE lcl_stage=>ty_method,
          lv_param   TYPE string,
          lv_status  TYPE string,
          lo_toolbar TYPE REF TO lcl_html_toolbar.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF mo_stage->mt_workarea.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    LOOP AT mo_stage->mt_workarea ASSIGNING <ls_file>.

      AT NEW type. " Local/remote header line
        IF sy-tabix = 1.
          ro_html->add('<tr class="separator firstrow">').
        ELSE.
          ro_html->add('<tr class="separator">').
        ENDIF.
        IF <ls_file>-type = lcl_stage=>c_wftype-local.
          ro_html->add( '<td></td><td colspan="2">LOCAL</td>' ) ##NO_TEXT.
        ELSE. "c_remote
          ro_html->add( '<td></td><td colspan="2">REMOTE</td>' ) ##NO_TEXT.
        ENDIF.
        ro_html->add('</tr>').
      ENDAT.

      lv_method = mo_stage->lookup( iv_path     = <ls_file>-file-path
                                    iv_filename = <ls_file>-file-filename ).
      lv_param  = lcl_html_action_utils=>file_encode( iv_key  = mo_repo->get_key( )
                                                      ig_file = <ls_file>-file ).

      lo_toolbar->reset( ). " Build line actions
      IF <ls_file>-type = lcl_stage=>c_wftype-local.
        IF lv_method IS NOT INITIAL.
          lo_toolbar->add( iv_txt = 'reset' iv_act = 'stage_reset?' && lv_param ).
        ELSE.
          lo_toolbar->add( iv_txt = 'add'   iv_act = 'stage_add?' && lv_param ).
        ENDIF.
      ELSE. "c_remote
        IF lv_method IS NOT INITIAL.
          lo_toolbar->add( iv_txt = 'reset'  iv_act = 'stage_reset?' && lv_param ).
        ELSE.
          lo_toolbar->add( iv_txt = 'ignore' iv_act = 'stage_ignore?' && lv_param ).
          lo_toolbar->add( iv_txt = 'remove' iv_act = 'stage_rm?' && lv_param ).
        ENDIF.
      ENDIF.

      IF lv_method IS INITIAL.
        lv_status = '<span class="grey">?</span>'.
      ELSE.
        lv_status = lv_method.
      ENDIF.

      ro_html->add( '<tr>' ).
      ro_html->add( |<td class="status">{ lv_status }</td>| ).
      ro_html->add( |<td>{ <ls_file>-file-path && <ls_file>-file-filename }</td>| ).
      ro_html->add( '<td>' ).
      ro_html->add( lo_toolbar->render( iv_no_separator = abap_true ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).

    ENDLOOP.

  ENDMETHOD.      "render_lines

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'STAGE' ) ).

    ro_html->add( '<div class="repo">' ).
    ro_html->add( lcl_gui_page_main=>render_repo_top( mo_repo ) ).
    ro_html->add( render_menu( ) ).

    ro_html->add( '<table class="stage_tab">' ).
    ro_html->add( render_lines( ) ).
    ro_html->add( '</table>' ).

    ro_html->add( '</div>' ).
    ro_html->add( footer( ) ).

  ENDMETHOD.      "lif_gui_page~render

  METHOD render_menu.

    DATA: lo_toolbar TYPE REF TO lcl_html_toolbar,
          lv_action  TYPE string.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    lv_action = lcl_html_action_utils=>repo_key_encode( mo_repo->get_key( ) ).

    IF mo_stage->count( ) > 0.
      lo_toolbar->add( iv_act = |stage_commit?{ lv_action }|
                       iv_txt = 'Commit'
                       iv_opt = gc_html_opt-emphas ).
    ELSEIF mo_stage->mv_local_cnt > 0.
      lo_toolbar->add( iv_act = |stage_all?{ lv_action }|
                       iv_txt = 'Add all and commit').
    ENDIF.

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.      "render_menu

  METHOD styles.
    CREATE OBJECT ro_html.

    ro_html->add('/* REPOSITORY */').
    ro_html->add('div.repo {').
    ro_html->add('  margin-top:       3px;').
    ro_html->add('  background-color: #f2f2f2;').
    ro_html->add('  padding: 0.5em 1em 0.5em 1em;').
    ro_html->add('}').
    ro_html->add('.repo_name span {').
    ro_html->add('  color: #333;').
    ro_html->add('  font-weight: bold;').
    ro_html->add('  font-size: 14pt;').
    ro_html->add('}').
    ro_html->add('.repo_name img {').
    ro_html->add('  vertical-align: baseline;').
    ro_html->add('  margin: 0 5px 0 5px;').
    ro_html->add('}').
    ro_html->add('.repo_attr {').
    ro_html->add('  color: grey;').
    ro_html->add('  font-size: 12pt;').
    ro_html->add('}').
    ro_html->add('.repo_attr span {').
    ro_html->add('  margin-left: 0.2em;').
    ro_html->add('  margin-right: 0.5em;').
    ro_html->add('}').
    ro_html->add('.repo_attr input {').
    ro_html->add('  color: grey;').     " Input wants it personaly
    ro_html->add('  font-size: 12pt;'). " Input wants it personaly
    ro_html->add('  margin-left: 0.5em;').
    ro_html->add('  margin-right: 0.5em;').
    ro_html->add('  background-color: transparent;').
    ro_html->add('  border-style: none;').
    ro_html->add('  text-overflow: ellipsis;').
    ro_html->add('}').

    ro_html->add('/* STAGE */').
    ro_html->add('.stage_tab {').
    ro_html->add('  border: 1px solid #DDD;').
    ro_html->add('  background: #fff;').
    ro_html->add('  margin-top: 0.2em;').
    ro_html->add('}').
    ro_html->add('.stage_tab td {').
    ro_html->add('  border-top: 1px solid #eee;').
    ro_html->add('  color: #333;').
    ro_html->add('  vertical-align: middle;').
    ro_html->add('  padding: 2px 0.5em;').
    ro_html->add('}').
    ro_html->add('.stage_tab td.status {').
    ro_html->add('  width: 2em;').
    ro_html->add('  text-align: center;').
    ro_html->add('}').
    ro_html->add('.stage_tab tr.separator td {').
    ro_html->add('  color: #BBB;').
    ro_html->add('  font-size: 10pt;').
    ro_html->add('  background-color: #edf2f9;').
    ro_html->add('  padding: 4px 0.5em;').
    ro_html->add('}').
    ro_html->add('.stage_tab tr.firstrow td { border-top: 0px; } ' ).

  ENDMETHOD.    "styles

ENDCLASS.

CLASS lcl_gui_page_db DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~render REDEFINITION.

  PRIVATE SECTION.
    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_main IMPLEMENTATION.

  METHOD render_obj_jump_link.

    DATA: lv_encode TYPE string,
          lo_html   TYPE REF TO lcl_html_helper.

    lv_encode = lcl_html_action_utils=>jump_encode( iv_obj_type = iv_obj_type
                                                    iv_obj_name = iv_obj_name ).

    CREATE OBJECT lo_html.
    lo_html->add_anchor( iv_txt = |{ iv_obj_name }| iv_act = |jump?{ lv_encode }| ).
    rv_html = lo_html->mv_html.

  ENDMETHOD.

  METHOD build_main_menu.

    DATA lo_betasub TYPE REF TO lcl_html_toolbar.


    CREATE OBJECT ro_menu.
    CREATE OBJECT lo_betasub.

    lo_betasub->add( iv_txt = 'Database util'    iv_act = 'db' ).
    lo_betasub->add( iv_txt = 'Package to zip'   iv_act = 'packagezip' ).
    lo_betasub->add( iv_txt = 'Background mode'  iv_act = 'background' ).

    ro_menu->add( iv_txt = 'Refresh all'      iv_act = 'refresh' ).
    ro_menu->add( iv_txt = 'Clone'            iv_act = 'install' ).
    ro_menu->add( iv_txt = 'Explore'          iv_act = 'explore' ).
    ro_menu->add( iv_txt = 'New offline repo' iv_act = 'newoffline' ).
    IF needs_installation( ) = abap_true.
      ro_menu->add( iv_txt = 'Get abapGit'    iv_act = 'abapgit_installation' ).
    ENDIF.
    ro_menu->add( iv_txt = '&#x03b2;'         io_sub = lo_betasub ).

  ENDMETHOD.                    "build main_menu

  METHOD styles.
    CREATE OBJECT ro_html.

    ro_html->add('/* REPOSITORY */').
    ro_html->add('div.repo {').
    ro_html->add('  margin-top:       3px;').
    ro_html->add('  background-color: #f2f2f2;').
    ro_html->add('  padding: 0.5em 1em 0.5em 1em;').
    ro_html->add('}').
    ro_html->add('.repo_name span {').
    ro_html->add('  font-weight: bold;').
    ro_html->add('  color: #333;').
    ro_html->add('  font-size: 14pt;').
    ro_html->add('}').
    ro_html->add('.repo_name img {').
    ro_html->add('  vertical-align: baseline;').
    ro_html->add('  margin: 0 5px 0 5px;').
    ro_html->add('}').
    ro_html->add('.repo_attr {').
    ro_html->add('  color: grey;').
    ro_html->add('  font-size: 12pt;').
    ro_html->add('}').
    ro_html->add('.repo_attr span {').
    ro_html->add('  margin-left: 0.2em;').
    ro_html->add('  margin-right: 0.5em;').
    ro_html->add('}').
    ro_html->add('.repo_attr input {').
    ro_html->add('  color: grey;').     " Input wants it personaly
    ro_html->add('  font-size: 12pt;'). " Input wants it personaly
    ro_html->add('  margin-left: 0.5em;').
    ro_html->add('  margin-right: 0.5em;').
    ro_html->add('  background-color: transparent;').
    ro_html->add('  border-style: none;').
    ro_html->add('  text-overflow: ellipsis;').
    ro_html->add('}').

    ro_html->add('/* REPOSITORY TABLE*/').
    ro_html->add('.repo_tab {').
    ro_html->add('  border: 1px solid #DDD;').
    ro_html->add('  border-radius: 3px;').
    ro_html->add('  background: #fff;').
    ro_html->add('  margin-top: 0.5em;').
    ro_html->add('}').
    ro_html->add('.repo_tab td {').
    ro_html->add('  border-top: 1px solid #eee;').
    ro_html->add('  vertical-align: middle;').
    ro_html->add('  color: #333;').
    ro_html->add('  padding-top: 2px;').
    ro_html->add('  padding-bottom: 2px;').
    ro_html->add('}').
    ro_html->add('.repo_tab td.icon {').
    ro_html->add('  width: 32px;').
    ro_html->add('  text-align: center;').
    ro_html->add('}').
    ro_html->add('.repo_tab td.type {').
    ro_html->add('  width: 3em;').
    ro_html->add('}').
    ro_html->add('.repo_tab td.object {').
    ro_html->add('  padding-left: 0.5em;').
    ro_html->add('}').
    ro_html->add('.repo_tab td.files {').
    ro_html->add('  padding-left: 0.5em;').
    ro_html->add('}').
    ro_html->add('.repo_tab td.cmd {').
    ro_html->add('  text-align: right;').
    ro_html->add('  padding-left: 0.5em;').
    ro_html->add('  padding-right: 1em;').
    ro_html->add('}').
    ro_html->add('.repo_tab tr.unsupported { color: lightgrey; }').
    ro_html->add('.repo_tab tr.firstrow td { border-top: 0px; } ' ).
    ro_html->add('.repo_tab td.files span  { display: block; }').
    ro_html->add('.repo_tab td.cmd span    { display: block; }').
    ro_html->add('.repo_tab td.cmd a       { display: block; }').

  ENDMETHOD.

  METHOD render_repo_menu.

    DATA: lo_toolbar     TYPE REF TO lcl_html_toolbar,
          lv_key         TYPE lcl_persistence_db=>ty_value,
          lo_sub         TYPE REF TO lcl_html_toolbar,
          lo_repo_online TYPE REF TO lcl_repo_online.


    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    lv_key = io_repo->get_key( ).

    IF lcl_app=>user( )->is_hidden( lv_key ) = abap_true.
      lo_toolbar->add( iv_txt = 'Show'
                       iv_act = |unhide?{ lv_key }| ).
    ELSE.
      IF io_repo->is_offline( ) = abap_true.
        lo_toolbar->add( iv_txt = 'Import ZIP'
                         iv_act = |zipimport?{ lv_key }|
                         iv_opt = gc_html_opt-emphas ).
        lo_toolbar->add( iv_txt = 'Export ZIP'
                         iv_act = |zipexport?{ lv_key }|
                         iv_opt = gc_html_opt-emphas ).
        lo_toolbar->add( iv_txt = 'Export&amp;Commit'
                         iv_act = |files_commit?{ lv_key }|
                         iv_opt = gc_html_opt-emphas ).
      ELSE.
        lo_repo_online ?= io_repo.
        TRY.
            IF lo_repo_online->get_sha1_remote( ) <> lo_repo_online->get_sha1_local( ).
              lo_toolbar->add( iv_txt = 'Pull'
                               iv_act = |pull?{ lv_key }|
                               iv_opt = gc_html_opt-emphas ).
            ELSEIF lcl_stage_logic=>count( lo_repo_online ) > 0.
              lo_toolbar->add( iv_txt = 'Stage'
                               iv_act = |stage?{ lv_key }|
                               iv_opt = gc_html_opt-emphas ).
            ENDIF.
          CATCH lcx_exception ##NO_HANDLER.
* authorization error or repository does not exist
* ignore error
        ENDTRY.
      ENDIF.

      CREATE OBJECT lo_sub.
      lo_sub->add( iv_txt = 'Remove'
                   iv_act = |remove?{ lv_key }| ).
      lo_sub->add( iv_txt = 'Uninstall'
                   iv_act = |uninstall?{ lv_key }| ).
      lo_sub->add( iv_txt = 'Switch branch'
                   iv_act = |switch_branch?{ lv_key }| ).
      lo_sub->add( iv_txt = 'Reset'
                   iv_act = |reset?{ lv_key }| ).
      lo_sub->add( iv_txt = 'Create branch'
                   iv_act = |create_branch?{ lv_key }| ).
      lo_sub->add( iv_txt = 'Branch overview'
                   iv_act = |branch_overview?{ lv_key }| ).
      lo_toolbar->add( iv_txt = 'Advanced'
                       io_sub = lo_sub ).

      lo_toolbar->add( iv_txt = 'Refresh'
                       iv_act = |refresh?{ lv_key }| ).
      lo_toolbar->add( iv_txt = 'Hide'
                       iv_act = |hide?{ lv_key }| ).
    ENDIF.

    ro_html->add( '<div class="paddings right">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD render_repo_top.
    DATA  lo_repo_online  TYPE REF TO lcl_repo_online.
    DATA  lv_icon         TYPE string.

    CREATE OBJECT ro_html.

    IF io_repo->is_offline( ) = abap_true.
      lv_icon = 'img/repo_offline'.
    ELSE.
      lv_icon = 'img/repo_online'.
    ENDIF.

    ro_html->add( |<a id="repo{ io_repo->get_key( ) }"></a>| ).
    ro_html->add( '<table width="100%"><tr>' ).

    ro_html->add( '<td class="repo_name">' ).
    ro_html->add( |<img src="{ lv_icon }">| ).
    ro_html->add( |<span>{ io_repo->get_name( ) }</span>| ).
    ro_html->add( '</td>' ).

    ro_html->add( '<td class="repo_attr right">' ).
    ro_html->add( '<img src="img/pkg">' ).
    ro_html->add( |<span>{ io_repo->get_package( ) }</span>| ).

    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.
      ro_html->add( '<img src="img/branch">' ).
      ro_html->add( |<span>{ lo_repo_online->get_branch_name( ) }</span>| ).
      ro_html->add( '<img src="img/link">' ).
      ro_html->add( |<input type="text" value="{ lo_repo_online->get_url( ) }" readonly>| ).
    ENDIF.

    ro_html->add( '</td>' ).
    ro_html->add( '</tr></table>' ).

  ENDMETHOD.

  METHOD render_repo.
    DATA: lt_repo_items TYPE tt_repo_items,
          lx_error      TYPE REF TO lcx_exception,
          lo_log        TYPE REF TO lcl_log.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF lt_repo_items.


    CREATE OBJECT ro_html.

    ro_html->add( |<div class="repo" id="repo{ io_repo->get_key( ) }">| ).
    ro_html->add( render_repo_top( io_repo ) ).

    ro_html->add( render_repo_menu( io_repo ) ).

    IF lcl_app=>user( )->is_hidden( io_repo->get_key( ) ) = abap_false.
      TRY.
          extract_repo_content( EXPORTING io_repo       = io_repo
                                IMPORTING et_repo_items = lt_repo_items
                                          eo_log        = lo_log ).

          ro_html->add( '<table width="100%" class="repo_tab">' ).

          IF lines( lt_repo_items ) = 0.
            ro_html->add( '<tr class="unsupported firstrow"><td class="paddings">'
                         && '<center>Empty package</center>'
                         && '</td></tr>' ) ##NO_TEXT.
          ELSE.
            LOOP AT lt_repo_items ASSIGNING <ls_item>.
              ro_html->add( render_repo_item( io_repo = io_repo is_item = <ls_item> ) ).
            ENDLOOP.
          ENDIF.

          ro_html->add( '</table>' ).

          IF io_repo->is_offline( ) = abap_false.
            ro_html->add( '<div class="log">' ). "TODO ??????
            ro_html->add( lo_log->to_html( ) ).
            ro_html->add( '</div>' ).
          ENDIF.
        CATCH lcx_exception INTO lx_error.
          ro_html->add( render_error( lx_error ) ).
      ENDTRY.
    ENDIF. " Hidden

    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD extract_repo_content.

    DATA: lo_repo_online TYPE REF TO lcl_repo_online,
          lt_tadir       TYPE lcl_tadir=>ty_tadir_tt,
          ls_repo_item   TYPE ty_repo_item,
          ls_file        TYPE ty_repo_file,
          lt_results     TYPE ty_results_tt.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results,
                   <ls_tadir>  LIKE LINE OF lt_tadir.


    CLEAR et_repo_items.

    IF io_repo->is_offline( ) = abap_true.
      lt_tadir = lcl_tadir=>read( io_repo->get_package( ) ).
      LOOP AT lt_tadir ASSIGNING <ls_tadir>.
        CLEAR ls_repo_item.
        IF sy-tabix = 1.
          ls_repo_item-is_first = abap_true.
        ENDIF.
        ls_repo_item-obj_type = <ls_tadir>-object.
        ls_repo_item-obj_name = <ls_tadir>-obj_name.
        APPEND ls_repo_item TO et_repo_items.
      ENDLOOP.

    ELSE.
      CREATE OBJECT eo_log.
      lo_repo_online ?= io_repo.
      lt_results      = lo_repo_online->status( eo_log ).
      LOOP AT lt_results ASSIGNING <ls_result>.
        AT NEW obj_name. "obj_type + obj_name
          CLEAR ls_repo_item.
          IF sy-tabix = 1.
            ls_repo_item-is_first = abap_true.
          ENDIF.
          ls_repo_item-obj_type = <ls_result>-obj_type.
          ls_repo_item-obj_name = <ls_result>-obj_name.
        ENDAT.

        IF <ls_result>-filename IS NOT INITIAL.
          ls_file-path       = <ls_result>-path.
          ls_file-filename   = <ls_result>-filename.
          ls_file-is_changed = boolc( NOT <ls_result>-match = abap_true ).
          APPEND ls_file TO ls_repo_item-files.
        ENDIF.

        AT END OF obj_name. "obj_type + obj_name
          APPEND ls_repo_item TO et_repo_items.
        ENDAT.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD render_repo_item.
    DATA:
      lv_link     TYPE string,
      lv_icon     TYPE string,
      lv_difflink TYPE string,
      ls_file     LIKE LINE OF is_item-files,
      lv_trclass  TYPE string.

    CREATE OBJECT ro_html.

    IF is_item-is_first = abap_true. " TR class
      lv_trclass = 'firstrow'.
    ENDIF.
    IF is_item-obj_name IS INITIAL.
      lv_trclass = lv_trclass && ' unsupported'.
    ENDIF.
    IF lv_trclass IS NOT INITIAL.
      SHIFT lv_trclass LEFT DELETING LEADING space.
      lv_trclass = | class="{ lv_trclass }"|.
    ENDIF.

    ro_html->add( |<tr{ lv_trclass }>| ).

    IF is_item-obj_name IS INITIAL.
      ro_html->add( '<td colspan="3"></td>' ).
    ELSE.
      CASE is_item-obj_type. "TODO ??
        WHEN 'PROG' OR 'CLAS' OR 'FUGR'.
          lv_icon = |<img src="img/code">|.
        WHEN 'W3MI' OR 'W3HT'.
          lv_icon = |<img src="img/bin">|.
        WHEN ''.
          " no icon
        WHEN OTHERS.
          lv_icon = |<img src="img/obj">|.
      ENDCASE.

      lv_link = render_obj_jump_link( iv_obj_name = is_item-obj_name
                                      iv_obj_type = is_item-obj_type ).
      ro_html->add( |<td class="icon">{ lv_icon }</td>| ).
      ro_html->add( |<td class="type">{ is_item-obj_type }</td>| ).
      ro_html->add( |<td class="object">{ lv_link }</td>| ).
    ENDIF.

    IF io_repo->is_offline( ) = abap_false. " Files for online repos only

      ro_html->add( '<td class="files">' ).
      LOOP AT is_item-files INTO ls_file.
        ro_html->add( |<span>{ ls_file-path && ls_file-filename }</span>| ).
      ENDLOOP.
      ro_html->add( '</td>' ).

      ro_html->add( '<td class="cmd">' ).
      IF lines( is_item-files ) = 0.
        ro_html->add( '<span class="grey">new</span>' ).
      ELSE.
        LOOP AT is_item-files INTO ls_file.
          IF ls_file-is_changed = abap_true.
            lv_difflink = lcl_html_action_utils=>file_encode(
              iv_key  = io_repo->get_key( )
              ig_file = ls_file ).
            ro_html->add_anchor(
              iv_txt = 'diff'
              iv_act = |diff?{ lv_difflink }| ).
          ELSE.
            ro_html->add( |<span>&nbsp;</span>| ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      ro_html->add( '</td>' ).

    ENDIF.

    ro_html->add( '</tr>' ).

  ENDMETHOD.

  METHOD needs_installation.

    CONSTANTS:
      lc_abapgit TYPE string VALUE 'https://github.com/larshp/abapGit.git',
      lc_plugins TYPE string VALUE 'https://github.com/larshp/abapGit-plugins.git' ##NO_TEXT.

    TRY.
        IF lcl_app=>repo_srv( )->is_repo_installed( lc_abapgit ) = abap_false
            OR lcl_app=>repo_srv( )->is_repo_installed( lc_plugins ) = abap_false.
          rv_not_completely_installed = abap_true.
        ENDIF.
      CATCH lcx_exception.
        " cannot be installed anyway in this case, e.g. no connection
        rv_not_completely_installed = abap_false.
    ENDTRY.
  ENDMETHOD.                    "needs_installation

  METHOD render_toc.

    DATA: lo_repo    LIKE LINE OF it_list,
          lo_toolbar TYPE REF TO lcl_html_toolbar.


    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    IF lines( it_list ) = 0.
      RETURN.
    ENDIF.

    LOOP AT it_list INTO lo_repo.
      lo_toolbar->add( iv_txt = lo_repo->get_name( )
                       iv_typ = gc_action_type-url
                       iv_act = |#repo{ lo_repo->get_key( ) }| ).
    ENDLOOP.

    ro_html->add( '<div id="toc">' ) ##NO_TEXT.
    ro_html->add( '<img src="img/toc">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD render_error.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="dummydiv attention">' ).
    ro_html->add( |Error: { ix_error->mv_text }| ).
    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD render_explore.

    DATA lo_toolbar TYPE REF TO lcl_html_toolbar.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    lo_toolbar->add( iv_txt = 'Explore new projects'
                     iv_act = 'explore' ).

    ro_html->add( '<div class="dummydiv">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lt_repos TYPE lcl_repo_srv=>ty_repo_tt,
          lx_error TYPE REF TO lcx_exception,
          lo_repo  LIKE LINE OF lt_repos.


    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( iv_title = 'HOME' io_menu = build_main_menu( ) ) ).

    TRY.
        lt_repos = lcl_app=>repo_srv( )->list( ).
      CATCH lcx_exception INTO lx_error.
        ro_html->add( render_error( lx_error ) ).
    ENDTRY.

    ro_html->add( render_toc( lt_repos ) ).

    IF lines( lt_repos ) = 0 AND lx_error IS INITIAL.
      ro_html->add( render_explore( ) ).
    ELSE.
      LOOP AT lt_repos INTO lo_repo.
        lcl_progress=>show( iv_key     = 'Render'
                            iv_current = sy-tabix
                            iv_total   = lines( lt_repos )
                            iv_text    = lo_repo->get_name( ) ) ##NO_TEXT.
        ro_html->add( render_repo( lo_repo ) ).
      ENDLOOP.
    ENDIF.

    ro_html->add( footer( ) ).

  ENDMETHOD.

  METHOD lif_gui_page~get_assets.

    DATA ls_image TYPE ty_web_asset.

    rt_assets = super->lif_gui_page~get_assets( ).

    ls_image-url     = 'img/toc'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAAFVBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgIAO39T0AAAABnRSTlMABBCRlMXJzV0oAAAAN0lEQVQIW2NgwABuaWlB'
      && 'YWlpDgwJDAxiAgxACshgYwAz0tLY2NISSBWBMYAmg4ADyBZhARCJAQBBchGypGCbQgAA'
      && 'AABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/repo_online'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAApVBMVEUAAABQbJxQbJxQ'
      && 'bJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQ'
      && 'bJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQ'
      && 'bJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQ'
      && 'bJz+TJ01AAAANnRSTlMAAQIDBAcJCgwSFBocHygqMTM1NkRHSU1QUWFiZGlweHuDiImL'
      && 'lZiio6a5vsfT3uTo6e3x9fsxY2JuAAAAgUlEQVQYGXXB6RaBUBSA0e+IEuIiMs9zhlDn'
      && '/R/NZWmt/LA3f1RcoaB50SydCbn20wjedkPu3sKSpMGH21PhLdZ0BATZ+cCXtxtDHGLV'
      && 'pgFW9QqJj2U0wvJvMF+5jiNGI3HK9dMQSouH6sRoFGoWd8l1dEDRWlWPQsFS98KPvvDH'
      && 'C3HLClrWc70ZAAAAAElFTkSuQmCC'.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/repo_offline'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBB'
      && 'ZG9iZSBJbWFnZVJlYWR5ccllPAAAAWNJREFUeNrEkr1KxFAQhe9P/iS6goLWiiB2PoCN'
      && 'lYW9ChbbiFhYRAQ7MaS2SOdT2PkSvoGPINiF1YTNz/WceC+sohDYwoFvZ/Zm78mcmZXG'
      && 'GDFPKDFn/L+AdEWWZUIptRmG4bLWeglHNXjHjGoppUa9CiaoX3ieJEl/z3MCXdfdIKXT'
      && '6bRFju2xYeASJ618338Dl6gf8zw3FOktpGk6QrrFmyPP82J0IgmCHxq1F0URBdbxuzuw'
      && '9nMGR2CRltCBbJpG1HUtmNGZcN/tynfAgbPgBMbWxp/DcmIIDaFdWOjtK7S/hbxnDQu0'
      && 'LGBFBEHQg7YNbAnCZ5xJWZbnRVFsuw7GM4P8hhXkPLgh0batqKqKFmM8O3FbeAanIOAM'
      && 'cJFQWNoBLpAv/e6D4PKEK3UCh+DiN9/sgG8lbhSWCNyDJ2U3MDSOwQa7cfc828rKQIF9'
      && '+x9QsxauwAMYDRA4s/kVXLP4FGAAajajeu7yxJkAAAAASUVORK5CYII='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/pkg'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA30lEQVQoU43OIUuDcRSF'
      && '8fvqhuB0mFwaKLbVBVdkX0GTFss+wYL2H4rJIIgyQQSzZcUPoGHZ9CKCmAwTMS8Y/ga3'
      && 'BWVjT7hwOQ+HEzEbMhU7jrTd69q2KhtFRU2nrvS927dm3pyqPXcuNRVD7sxiRIQlDSc+'
      && 'PGjZUFDWkYekLfdoV2XYua4rSZ61pZBkEUq2XPty41XuXJIiZGNhPDVZiFCYIMSor+Db'
      && '7RQhYnQnCsNvNmGgPFFYMQh1PU9aqrLxyGUNx/p66r9mUc2hFx3JhU9vDtQU4y9KGjaV'
      && '/gXT+AGZVIinhU2EAwAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/branch'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAqFBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA'
      && 'gICAgID/OyosAAAAN3RSTlMAAQIDBAYICQ8TFRweJScoKSo3Oj1FRk1dYWJjZmhzdIaJ'
      && 'j5GVm6CwsrS5vsHDyszV19ne7/X583teZAAAAIFJREFUGFdVytkagVAYheFvFzJlnqc0'
      && 'EEoR+u//zhxI7dbZ9z4LMJ1op9DmjpntdXiBigHbLiAYqukBVr63+YGRSazgCY/iEooP'
      && 'xKZxr0EnSbo14B1Rg4msKzj150fJrQpERPLBv7mIfNxlq+zRbZsu0JYpGlcdwjY9Twfr'
      && 'nAbNsr6IKQxJI/U5CgAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/link'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAXVBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICVwFMKAAAAHnRSTlMAAwQFBgcK'
      && 'FR4gIiMmP0JHSm+RmKDByM/R09rg+/0jN/q+AAAAX0lEQVQYV43Nxw6AIBAE0FGw916Z'
      && '//9MRQ0S4sG5bPZlCxqSCyBGXgFUJKUA4A8PUOKONzuQOxOZIjcLkrMvxGQg3skSCFYL'
      && 'Kl1Ds5LWz+33yyf4rQOSf6CjnV6rHeAA87gJtKzI8ocAAAAASUVORK5CYII='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/code'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOBAMAAADtZjDiAAAAElBMVEUAAACAgICAgICA'
      && 'gICAgICAgIC07w1vAAAABXRSTlMABECUxcOwZQcAAAA1SURBVAhbY2AODQ0NEWBgYGVg'
      && 'YGByhNAMKgIMrKyhAQxMDhA+QwCCZgVqIIUP1Q+yJzTUAAAfUAq+Os55uAAAAABJRU5E'
      && 'rkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/bin'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOBAMAAADtZjDiAAAAElBMVEUAAACAgICAgICA'
      && 'gICAgICAgIC07w1vAAAABXRSTlMABECUxcOwZQcAAABBSURBVAhbXcqxDYAwAMRAK8h9'
      && 'hmAARoANvuD3X4UCiojqZMlsbe8JAuN6ZZ9ozThRCVmsJe9H0HwdXf19W9v2eAA6Fws2'
      && 'RotPsQAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/obj'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOBAMAAADtZjDiAAAAIVBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgIDcWqnoAAAACnRSTlMABD1AZI+RlcPFIaFe1gAA'
      && 'AEVJREFUCFtjYF+1atVKAQYGLgYGBuaJEJrBUgBCM0+A0AwLgLQIgyOIZmwCSgNptgAG'
      && '1gQQfzKDhgCSPFw9Kg2yZ9WqAgBWJBENLk6V3AAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_background IMPLEMENTATION.

  METHOD push.

    DATA: ls_comment TYPE ty_comment,
          ls_files   TYPE lcl_stage_logic=>ty_stage_files,
          lo_stage   TYPE REF TO lcl_stage.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ls_files-local.


    ls_files = lcl_stage_logic=>get( io_repo ).
    IF lines( ls_files-local ) = 0.
      WRITE: / 'nothing to stage'.
      RETURN.
    ENDIF.

    ls_comment-username = 'foobar'.
    ls_comment-email    = 'foo@bar.com'.
    ls_comment-comment  = 'background mode'.

    lo_stage = lcl_app=>repo_srv( )->get_stage( io_repo->get_key( ) ).

    LOOP AT ls_files-local ASSIGNING <ls_file>.
      WRITE: / 'stage', <ls_file>-file-path, <ls_file>-file-filename.
      lo_stage->add( iv_path     = <ls_file>-file-path
                     iv_filename = <ls_file>-file-filename ).
    ENDLOOP.

    io_repo->push( is_comment = ls_comment
                   io_stage   = lo_stage ).

  ENDMETHOD.

  METHOD run.

    DATA: lo_per       TYPE REF TO lcl_persistence_background,
          lo_repo      TYPE REF TO lcl_repo_online,
          lt_list      TYPE lcl_persistence_background=>tt_background,
          lv_repo_name TYPE string.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    CREATE OBJECT lo_per.
    lt_list = lo_per->list( ).

    WRITE: / 'Background mode'.

    LOOP AT lt_list ASSIGNING <ls_list>.
      lo_repo ?= lcl_app=>repo_srv( )->get( <ls_list>-key ).
      lv_repo_name = lo_repo->get_name( ).
      WRITE: / <ls_list>-method, lv_repo_name.

      lcl_login_manager=>set(
        iv_uri      = lo_repo->get_url( )
        iv_username = <ls_list>-username
        iv_password = <ls_list>-password ).

      CASE <ls_list>-method.
        WHEN lcl_persistence_background=>c_method-pull.
          lo_repo->deserialize( ).
        WHEN lcl_persistence_background=>c_method-push.
          push( lo_repo ).
        WHEN OTHERS.
          _raise 'background, unknown mode'.
      ENDCASE.
    ENDLOOP.

    IF lines( lt_list ) = 0.
      WRITE: / 'Nothing configured'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
FORM run.

  DATA: lx_exception TYPE REF TO lcx_exception,
        lv_ind       TYPE t000-ccnocliind.


  SELECT SINGLE ccnocliind FROM t000 INTO lv_ind
    WHERE mandt = sy-mandt.
  IF sy-subrc = 0
      AND lv_ind <> ' '
      AND lv_ind <> '1'. " check changes allowed
    WRITE: / 'Wrong client, changes to repository objects not allowed'. "#EC NOTEXT
    RETURN.
  ENDIF.

  TRY.
      lcl_persistence_migrate=>run( ).
      lcl_app=>run( ).
    CATCH lcx_exception INTO lx_exception.
      MESSAGE lx_exception->mv_text TYPE 'E'.
  ENDTRY.

ENDFORM.                    "run

*&---------------------------------------------------------------------*
*&      Form  branch_popup
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TT_FIELDS      text
*      -->PV_CODE        text
*      -->CS_ERROR       text
*      -->CV_SHOW_POPUP  text
*      -->RAISING        text
*      -->LCX_EXCEPTION  text
*      -->##CALLED       text
*      -->##NEEDED       text
*----------------------------------------------------------------------*
FORM branch_popup TABLES   tt_fields STRUCTURE sval
                  USING    pv_code TYPE clike
                  CHANGING cs_error TYPE svale
                           cv_show_popup TYPE c
                  RAISING lcx_exception ##called ##needed.
* called dynamically from function module POPUP_GET_VALUES_USER_BUTTONS

  DATA: lv_url          TYPE string,
        lv_answer       TYPE c,
        lx_error        TYPE REF TO lcx_exception,
        lt_selection    TYPE TABLE OF spopli,
        ls_package_data TYPE scompkdtln,
        lt_branches     TYPE lcl_git_transport=>ty_branch_list_tt.

  FIELD-SYMBOLS: <ls_fbranch> LIKE LINE OF tt_fields,
                 <ls_branch>  LIKE LINE OF lt_branches,
                 <ls_sel>     LIKE LINE OF lt_selection,
                 <ls_furl>    LIKE LINE OF tt_fields.


  CLEAR cs_error.

  IF pv_code = 'COD1'.
    cv_show_popup = abap_true.

    READ TABLE tt_fields ASSIGNING <ls_furl> WITH KEY tabname = 'ABAPTXT255'.
    IF sy-subrc <> 0 OR <ls_furl>-value IS INITIAL.
      MESSAGE 'Fill URL' TYPE 'S' DISPLAY LIKE 'E'.         "#EC NOTEXT
      RETURN.
    ENDIF.
    lv_url = <ls_furl>-value.

    TRY.
        lt_branches = lcl_git_transport=>branches( lv_url ).
      CATCH lcx_exception INTO lx_error.
        MESSAGE lx_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    LOOP AT lt_branches ASSIGNING <ls_branch>.
      APPEND INITIAL LINE TO lt_selection ASSIGNING <ls_sel>.
      <ls_sel>-varoption = <ls_branch>-name.
    ENDLOOP.

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        textline1          = 'Select branch'
        titel              = 'Select branch'
      IMPORTING
        answer             = lv_answer
      TABLES
        t_spopli           = lt_selection
      EXCEPTIONS
        not_enough_answers = 1
        too_much_answers   = 2
        too_much_marks     = 3
        OTHERS             = 4.                             "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'Error from POPUP_TO_DECIDE_LIST'.
    ENDIF.

    IF lv_answer = 'A'. " cancel
      RETURN.
    ENDIF.

    READ TABLE lt_selection ASSIGNING <ls_sel> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.

    READ TABLE tt_fields ASSIGNING <ls_fbranch> WITH KEY tabname = 'TEXTL'.
    ASSERT sy-subrc = 0.
    <ls_fbranch>-value = <ls_sel>-varoption.

  ELSEIF pv_code = 'COD2'.
    cv_show_popup = abap_true.

    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = 'PB_POPUP_PACKAGE_CREATE'
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.
    IF sy-subrc = 1.
* looks like the function module used does not exist on all
* versions since 702, so show an error
      _raise 'Function module PB_POPUP_PACKAGE_CREATE does not exist'.
    ENDIF.

    CALL FUNCTION 'PB_POPUP_PACKAGE_CREATE'
      CHANGING
        p_object_data    = ls_package_data
      EXCEPTIONS
        action_cancelled = 1.
    IF sy-subrc = 1.
      RETURN.
    ENDIF.

    lcl_sap_package=>create( ls_package_data ).
    COMMIT WORK.

    READ TABLE tt_fields ASSIGNING <ls_fbranch> WITH KEY tabname = 'TDEVC'.
    ASSERT sy-subrc = 0.
    <ls_fbranch>-value = ls_package_data-devclass.
  ENDIF.

ENDFORM.                    "branch_popup

CLASS lcl_gui_page_db_display DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~render   REDEFINITION.

    METHODS: constructor
      IMPORTING is_key TYPE lcl_persistence_db=>ty_content.

  PRIVATE SECTION.
    DATA: ms_key TYPE lcl_persistence_db=>ty_content.

    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_db_display IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_key = is_key.
  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lv_data TYPE lcl_persistence_db=>ty_content-data_str.

    TRY.
        lv_data = lcl_app=>db( )->read(
          iv_type = ms_key-type
          iv_value = ms_key-value ).
      CATCH lcx_not_found ##NO_HANDLER.
    ENDTRY.

    lv_data = lcl_xml_pretty=>print( lv_data ).

    lv_data = escape( val    = lv_data
                      format = cl_abap_format=>e_html_attr ).

    CREATE OBJECT ro_html.
    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'CONFIG DISPLAY' ) ).

    ro_html->add( '<div class="db_entry">' ).
    ro_html->add( |<table class="tag"><tr><td class="label">Type:</td>| &&
                  |  <td>{ ms_key-type }</td></tr></table>| ).
    ro_html->add( |<table class="tag"><tr><td class="label">Value:</td>| &&
                  |  <td>{ ms_key-value }</td></tr></table>| ).
    ro_html->add( |<pre>{ lv_data }</pre>| ).
    ro_html->add( '</div>' ).

    ro_html->add( footer( ) ).

  ENDMETHOD.

  METHOD styles.
    CREATE OBJECT ro_html.

    ro_html->add('/* DB ENTRY DISPLAY */').
    ro_html->add('div.db_entry {').
    ro_html->add('  background-color: #f2f2f2;').
    ro_html->add('  padding: 0.5em;').
    ro_html->add('}').

    ro_html->add('div.db_entry pre { ').
    ro_html->add('  display: block; ').
    ro_html->add('  overflow: hidden; ').
    ro_html->add('  word-wrap:break-word; ').
    ro_html->add('  white-space: pre-wrap; ').
    ro_html->add('  background-color: #eaeaea;').
    ro_html->add('  padding: 0.5em;').
    ro_html->add('  width: 50em; ').
    ro_html->add('}').

    ro_html->add('table.tag {').
    ro_html->add('  display: inline-block;').
    ro_html->add('  border: 1px #b3c1cc solid;').
    ro_html->add('  background-color: #eee;').
    ro_html->add('  margin-right: 0.5em; ').
    ro_html->add('}').
    ro_html->add('table.tag td { padding: 0.2em 0.5em; }').
    ro_html->add('table.tag td.label { background-color: #b3c1cc; }').

  ENDMETHOD.            "styles

ENDCLASS.

CLASS lcl_gui_page_db_edit DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~render   REDEFINITION.

    METHODS: constructor
      IMPORTING is_key TYPE lcl_persistence_db=>ty_content.

  PRIVATE SECTION.
    DATA: ms_key TYPE lcl_persistence_db=>ty_content.

    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS scripts
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_db_edit IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_key = is_key.
  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lv_data    TYPE lcl_persistence_db=>ty_content-data_str,
          lo_toolbar TYPE REF TO lcl_html_toolbar.

    TRY.
        lv_data = lcl_app=>db( )->read(
          iv_type  = ms_key-type
          iv_value = ms_key-value ).
      CATCH lcx_not_found ##NO_HANDLER.
    ENDTRY.

    lcl_app=>db( )->lock(
      iv_type  = ms_key-type
      iv_value = ms_key-value ).

    lv_data = lcl_xml_pretty=>print( lv_data ).

    lv_data = escape( val    = lv_data
                      format = cl_abap_format=>e_html_attr ).

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'CONFIG EDIT' ) ).

    ro_html->add( '<div class="db_entry">' ).

    " Banners
    ro_html->add( |<table class="tag"><tr><td class="label">Type:</td>| &&
                  |  <td>{ ms_key-type }</td></tr></table>| ).
    ro_html->add( |<table class="tag"><tr><td class="label">Value:</td>| &&
                  |  <td>{ ms_key-value }</td></tr></table>| ).

    " Form
    ro_html->add( '<form id="db_form" method="post" action="sapevent:db_save">' ).
    ro_html->add( |<input type="hidden" name="type" value="{ ms_key-type }">| ).
    ro_html->add( |<input type="hidden" name="value" value="{ ms_key-value }">| ).
    ro_html->add( |<textarea rows="20" cols="100" name="xmldata">{ lv_data
                     }</textarea>| ).
    ro_html->add( '</form>' ).

    " Menu
    lo_toolbar->add( iv_act = 'submitDBForm();'
                     iv_txt = 'Save'
                     iv_typ = gc_action_type-onclick
                     iv_opt = gc_html_opt-emphas ).

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

    ro_html->add( '</div>' ). "db_entry

    ro_html->add( footer( io_include_script = scripts( ) ) ).

  ENDMETHOD.

  METHOD styles.
    CREATE OBJECT ro_html.

    ro_html->add('/* DB ENTRY DISPLAY */').
    ro_html->add('div.db_entry {').
    ro_html->add('  background-color: #f2f2f2;').
    ro_html->add('  padding: 0.5em;').
    ro_html->add('}').
    ro_html->add('div.db_entry textarea { margin: 0.5em 0em; }').
    ro_html->add('table.tag {').
    ro_html->add('  display: inline-block;').
    ro_html->add('  border: 1px #b3c1cc solid;').
    ro_html->add('  background-color: #eee;').
    ro_html->add('  margin-right: 0.5em; ').
    ro_html->add('}').
    ro_html->add('table.tag td { padding: 0.2em 0.5em; }').
    ro_html->add('table.tag td.label { background-color: #b3c1cc; }').

  ENDMETHOD.            "styles

  METHOD scripts.

    CREATE OBJECT ro_html.

    ro_html->add( 'function submitDBForm() {' ).
    ro_html->add( '  document.getElementById("db_form").submit();' ).
    ro_html->add( '}' ).

  ENDMETHOD.    "scripts

ENDCLASS.

CLASS lcl_gui_page_db IMPLEMENTATION.

  METHOD lif_gui_page~render.

    DATA: lt_data    TYPE lcl_persistence_db=>tt_content,
          lv_escaped TYPE string,
          lv_action  TYPE string,
          lv_trclass TYPE string,
          lo_toolbar TYPE REF TO lcl_html_toolbar.

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF lt_data.


    lt_data = lcl_app=>db( )->list( ).

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'DATABASE PERSISTENCY' ) ).

    ro_html->add( '<div class="db_list">' ).
    ro_html->add( '<table width="100%" class="db_tab">' ).

    " Header
    ro_html->add( '<tr>' ).
    ro_html->add( '<th>Type</th>' ).
    ro_html->add( '<th>Value</th>' ).
    ro_html->add( '<th>Data</th>' ).
    ro_html->add( '<th></th>' ).
    ro_html->add( '</tr>' ).

    " Lines
    LOOP AT lt_data ASSIGNING <ls_data>.
      CLEAR lv_trclass.
      IF sy-tabix = 1.
        lv_trclass = ' class="firstrow"'.
      ENDIF.

      IF strlen( <ls_data>-data_str ) >= 250.
        lv_escaped = escape( val    = <ls_data>-data_str(250)
                             format = cl_abap_format=>e_html_attr ).
      ELSE.
        lv_escaped = escape( val    = <ls_data>-data_str
                             format = cl_abap_format=>e_html_attr ).
      ENDIF.

      lv_action = lcl_html_action_utils=>dbkey_encode( <ls_data> ).

      CREATE OBJECT lo_toolbar.
      lo_toolbar->add( iv_txt = 'Display' iv_act = |db_display?{ lv_action }| ).
      lo_toolbar->add( iv_txt = 'Edit'    iv_act = |db_edit?{ lv_action }| ).
      lo_toolbar->add( iv_txt = 'Delete'  iv_act = |db_delete?{ lv_action }| ).

      ro_html->add( |<tr{ lv_trclass }>| ).
      ro_html->add( |<td>{ <ls_data>-type }</td>| ).
      ro_html->add( |<td>{ <ls_data>-value }</td>| ).
      ro_html->add( |<td><pre>{ lv_escaped }</pre></td>| ).
      ro_html->add( '<td>' ).
      ro_html->add( lo_toolbar->render( iv_vertical = abap_true ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.

    ro_html->add( '</table>' ).
    ro_html->add( '</div>' ).

    ro_html->add( footer( ) ).

  ENDMETHOD.            "lif_gui_page~render

  METHOD styles.
    CREATE OBJECT ro_html.

    ro_html->add('/* DB ENTRIES */').
    ro_html->add('div.db_list {').
    ro_html->add('  background-color: #f2f2f2;').
    ro_html->add('  padding: 0.5em;').
    ro_html->add('}').
    ro_html->add('table.db_tab pre { ').
    ro_html->add('  display: block; ').
    ro_html->add('  overflow: hidden; ').
    ro_html->add('  word-wrap:break-word; ').
    ro_html->add('  white-space: pre-wrap; ').
    ro_html->add('  background-color: #eaeaea;').
    ro_html->add('  padding: 3px;').
    ro_html->add('  width: 50em; ').
    ro_html->add('}').
    ro_html->add('table.db_tab tr.firstrow td { padding-top: 0.5em; } ').
    ro_html->add('table.db_tab th {').
    ro_html->add('  text-align: left;').
    ro_html->add('  color: #888;').
    ro_html->add('  padding: 0.2em;').
    ro_html->add('  border-bottom: 1px #ddd solid;').
    ro_html->add('}').
    ro_html->add('table.db_tab td {').
    ro_html->add('  color: #333;').
    ro_html->add('  padding: 0.2em;').
    ro_html->add('  vertical-align: top;').
    ro_html->add('}').

  ENDMETHOD.            "styles

ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS lcl_gui_router IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_gui_router IMPLEMENTATION.

  DEFINE _add_dialog_fld.
    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname    = &1.                             "#EC NOTEXT
    <ls_field>-fieldname  = &2.                             "#EC NOTEXT
    <ls_field>-fieldtext  = &3.                             "#EC NOTEXT
    <ls_field>-value      = &4.                             "#EC NOTEXT
    <ls_field>-field_attr = &5.                             "#EC NOTEXT
  END-OF-DEFINITION.

  METHOD on_event.
    DATA: lv_url  TYPE string,
          lv_key  TYPE lcl_persistence_repo=>ty_repo-key,
          ls_item TYPE ty_item.

    CASE iv_action.
        " General routing
      WHEN 'main'
          OR 'explore'
          OR 'db'
          OR 'background'
          OR 'background_run'.
        ei_page  = get_page_by_name( iv_action ).
        ev_state = gc_event_state-new_page.
      WHEN 'abapgithome'.
        cl_gui_frontend_services=>execute( EXPORTING document = gc_abapgit_homepage
                                           EXCEPTIONS OTHERS = 1 ).
        IF sy-subrc <> 0.
          _raise 'Opening page in external browser failed.'.
        ENDIF.
        ev_state = gc_event_state-no_more_act.
      WHEN 'abapgit_installation'.
        abapgit_installation( ).
        ev_state = gc_event_state-re_render.
      WHEN 'jump'.
        lcl_html_action_utils=>jump_decode( EXPORTING iv_string   = iv_getdata
                                            IMPORTING ev_obj_type = ls_item-obj_type
                                                      ev_obj_name = ls_item-obj_name ).
        lcl_objects=>jump( ls_item ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'diff'.
        ei_page  = get_page_diff( iv_getdata ).
        ev_state = gc_event_state-new_page.

        " DB actions
      WHEN 'db_display' OR 'db_edit'.
        ei_page  = get_page_db_by_name( iv_name = iv_action  iv_getdata = iv_getdata ).
        ev_state = gc_event_state-new_page.
      WHEN 'db_delete'.
        db_delete( iv_getdata = iv_getdata ).
        ev_state = gc_event_state-re_render.
      WHEN 'db_save'.
        db_save( it_postdata ).
        ev_state = gc_event_state-go_back.

        " Repository state actions
      WHEN 'install'.
        lv_url   = iv_getdata.
        repo_clone( lv_url ).
        ev_state = gc_event_state-re_render.
      WHEN 'uninstall'.
        lv_key   = iv_getdata.
        repo_purge( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'remove'.
        lv_key   = iv_getdata.
        repo_remove( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'zipimport'.
        lv_key   = iv_getdata.
        lcl_zip=>import( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'zipexport'.
        lv_key   = iv_getdata.
        lcl_zip=>export( lcl_app=>repo_srv( )->get( lv_key ) ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'newoffline'.
        repo_new_offline( ).
        ev_state = gc_event_state-re_render.
      WHEN 'files_commit'. "TODO refactor name ?
        lv_key   = iv_getdata.
        lcl_zip=>export( io_repo = lcl_app=>repo_srv( )->get( lv_key )
                         iv_zip  = abap_false ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'packagezip'. "TODO refactor name ?
        repo_package_zip( ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'hide'.
        lv_key   = iv_getdata.
        lcl_app=>user( )->hide( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'unhide'.
        lv_key   = iv_getdata.
        lcl_app=>user( )->unhide( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'refresh'.
        lv_key = iv_getdata.
        IF lv_key IS INITIAL. " Refresh all or single
          lcl_app=>repo_srv( )->refresh( ).
        ELSE.
          lcl_app=>repo_srv( )->get( lv_key )->refresh( ).
        ENDIF.
        ev_state = gc_event_state-re_render.

        " Repository online actions
      WHEN 'pull'.
        lv_key   = iv_getdata.
        repo_pull( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'stage'.
        lv_key   = iv_getdata.
        ei_page  = get_page_stage( lv_key ).
        ev_state = gc_event_state-new_page_w_bookmark.
      WHEN 'switch_branch'.
        lv_key   = iv_getdata.
        switch_branch( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'reset'.
        lv_key   = iv_getdata.
        reset( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'create_branch'.
        lv_key   = iv_getdata.
        create_branch( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'branch_overview'.
        ei_page  = get_page_branch_overview( iv_getdata ).
        ev_state = gc_event_state-new_page.

        " Stage
      WHEN 'stage_commit'.
        ei_page  = get_page_commit( iv_getdata ).
        ev_state = gc_event_state-new_page.
      WHEN 'stage_all'.
        stage_handle_action( iv_getdata = iv_getdata iv_action = iv_action ).
        ei_page  = get_page_commit( iv_getdata ).
        ev_state = gc_event_state-new_page.
      WHEN 'stage_add' OR 'stage_reset' OR 'stage_ignore' OR 'stage_rm'.
        stage_handle_action( iv_getdata = iv_getdata iv_action = iv_action ).
        ev_state = gc_event_state-re_render.

        " Commit
      WHEN 'commit_post'.
        commit_push( it_postdata ).
        ev_state = gc_event_state-go_back_to_bookmark.
      WHEN 'commit_cancel'.
        ev_state = gc_event_state-go_back.

      WHEN OTHERS.
        ev_state = gc_event_state-not_handled.
    ENDCASE.
  ENDMETHOD.        " on_event

  METHOD get_page_by_name.

    DATA: lv_page_class TYPE string,
          lv_message    TYPE string.

    lv_page_class = |LCL_GUI_PAGE_{ to_upper( iv_name ) }|.

    TRY.
        CREATE OBJECT ri_page TYPE (lv_page_class).
      CATCH cx_sy_create_object_error.
        lv_message = |Cannot create page class { lv_page_class }|.
        _raise lv_message.
    ENDTRY.

  ENDMETHOD.        " get_page_by_name

  METHOD get_page_db_by_name.

    DATA: lv_page_class TYPE string,
          lv_message    TYPE string,
          ls_key        TYPE lcl_persistence_db=>ty_content.

    lv_page_class = |LCL_GUI_PAGE_{ to_upper( iv_name ) }|.
    ls_key        = lcl_html_action_utils=>dbkey_decode( iv_getdata ).

    TRY.
        CREATE OBJECT ri_page TYPE (lv_page_class)
          EXPORTING
            is_key = ls_key.

      CATCH cx_sy_create_object_error.
        lv_message = |Cannot create page class { lv_page_class }|.
        _raise lv_message.
    ENDTRY.

  ENDMETHOD.        " get_page_db_by_name

  METHOD get_page_branch_overview.

    DATA: lo_repo TYPE REF TO lcl_repo_online,
          lo_page TYPE REF TO lcl_gui_page_branch_overview,
          lv_key  TYPE lcl_persistence_repo=>ty_repo-key.


    lv_key = iv_getdata.

    lo_repo ?= lcl_app=>repo_srv( )->get( lv_key ).

    CREATE OBJECT lo_page
      EXPORTING
        io_repo = lo_repo.

    ri_page = lo_page.

  ENDMETHOD.

  METHOD get_page_diff.

    DATA: lt_remote TYPE ty_files_tt,
          lt_local  TYPE ty_files_item_tt,
          lo_page   TYPE REF TO lcl_gui_page_diff,
          lo_repo   TYPE REF TO lcl_repo_online,
          ls_file   TYPE ty_repo_file,
          lv_key    TYPE lcl_persistence_repo=>ty_repo-key.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF lt_remote,
                   <ls_local>  LIKE LINE OF lt_local.

    lcl_html_action_utils=>file_decode( EXPORTING iv_string = iv_getdata
                                        IMPORTING ev_key    = lv_key
                                                  eg_file   = ls_file ).

    lo_repo  ?= lcl_app=>repo_srv( )->get( lv_key ).
    lt_remote = lo_repo->get_files_remote( ).
    lt_local  = lo_repo->get_files_local( ).

    READ TABLE lt_remote ASSIGNING <ls_remote>
      WITH KEY filename = ls_file-filename
               path     = ls_file-path.
    IF sy-subrc <> 0.
      _raise 'file not found remotely'.
    ENDIF.

    READ TABLE lt_local ASSIGNING <ls_local>
      WITH KEY file-filename = ls_file-filename
               file-path     = ls_file-path.
    IF sy-subrc <> 0.
      _raise 'file not found locally'.
    ENDIF.

    CREATE OBJECT lo_page
      EXPORTING
        is_local  = <ls_local>-file
        is_remote = <ls_remote>.

    ri_page = lo_page.

  ENDMETHOD.

  METHOD abapgit_installation.

    CONSTANTS lc_package_abapgit TYPE devclass VALUE '$ABAPGIT'.
    CONSTANTS lc_package_plugins TYPE devclass VALUE '$ABAPGIT_PLUGINS'.

    DATA lv_text            TYPE c LENGTH 100.
    DATA lv_answer          TYPE c LENGTH 1.
    DATA lo_repo            TYPE REF TO lcl_repo_online.
    DATA lv_url             TYPE string.
    DATA lv_target_package  TYPE devclass.

    lv_text = |Installing current version ABAPGit to package { lc_package_abapgit } |
           && |and plugins to { lc_package_plugins }|.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Install abapGit'
        text_question         = lv_text
        text_button_1         = 'Continue'
        text_button_2         = 'Cancel'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer ##no_text.
    IF lv_answer <> '1'.
      RETURN. ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ENDIF.

    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          lv_url            = 'https://github.com/larshp/abapGit.git'.
          lv_target_package = lc_package_abapgit.
        WHEN 2.
          lv_url            = 'https://github.com/larshp/abapGit-plugins.git' ##no_text.
          lv_target_package = lc_package_plugins.
      ENDCASE.

      IF abap_false = lcl_app=>repo_srv( )->is_repo_installed(
          iv_url              = lv_url
          iv_target_package   = lv_target_package ).

        lcl_sap_package=>create_local( lv_target_package ).

        lo_repo = lcl_app=>repo_srv( )->new_online(
          iv_url         = lv_url
          iv_branch_name = 'refs/heads/master'
          iv_package     = lv_target_package ) ##NO_TEXT.

        lo_repo->status( ). " check for errors
        lo_repo->deserialize( ).
      ENDIF.
    ENDDO.

    COMMIT WORK.

  ENDMETHOD. "abapgit_installation

  METHOD repo_popup.

    DATA: lv_returncode TYPE c,
          lv_icon_ok    TYPE icon-name,
          lv_icon_br    TYPE icon-name,
          lt_fields     TYPE TABLE OF sval,
          lv_pattr      TYPE spo_fattr,
          lv_button2    TYPE svalbutton-buttontext,
          lv_icon2      TYPE icon-name.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    IF NOT iv_package IS INITIAL.
      lv_pattr = '05'.
    ELSE.
      lv_button2 = 'Create package'.
      lv_icon2   = icon_msg.
    ENDIF.

*                   TAB           FLD       LABEL            DEF        ATTR
    _add_dialog_fld 'ABAPTXT255' 'LINE'     'Git Clone Url'  iv_url     ''.
    _add_dialog_fld 'TDEVC'      'DEVCLASS' 'Target Package' iv_package lv_pattr.
    _add_dialog_fld 'TEXTL'      'LINE'     'Branch'         iv_branch  '05'.

    lv_icon_ok  = icon_okay.
    lv_icon_br  = icon_workflow_fork.

    CALL FUNCTION 'POPUP_GET_VALUES_USER_BUTTONS'
      EXPORTING
        popup_title       = 'Repository'
        programname       = sy-repid
        formname          = 'BRANCH_POPUP'
        ok_pushbuttontext = 'OK'
        icon_ok_push      = lv_icon_ok
        first_pushbutton  = 'Select branch'
        icon_button_1     = lv_icon_br
        second_pushbutton = lv_button2
        icon_button_2     = lv_icon2
      IMPORTING
        returncode        = lv_returncode
      TABLES
        fields            = lt_fields
      EXCEPTIONS
        error_in_fields   = 1
        OTHERS            = 2.                              "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'Error from POPUP_GET_VALUES'.
    ENDIF.
    IF lv_returncode = 'A'.
      rs_popup-cancel = abap_true.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_popup-url = <ls_field>-value.
    lcl_url=>name( rs_popup-url ).         " validate

    READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_popup-package = <ls_field>-value.
    TRANSLATE rs_popup-package TO UPPER CASE.

    READ TABLE lt_fields INDEX 3 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_popup-branch_name = <ls_field>-value.

  ENDMETHOD.

  METHOD repo_clone.

    DATA: ls_popup TYPE ty_popup,
          lo_repo  TYPE REF TO lcl_repo_online.


    ls_popup = repo_popup( iv_url ).
    IF ls_popup-cancel = abap_true.
      RETURN.
    ENDIF.

    lo_repo = lcl_app=>repo_srv( )->new_online(
      iv_url         = ls_popup-url
      iv_branch_name = ls_popup-branch_name
      iv_package     = ls_popup-package ).
    lo_repo->status( ). " check for errors
    lo_repo->deserialize( ).

    COMMIT WORK.

  ENDMETHOD.                    "repo_clone

  METHOD repo_purge.

    DATA: lt_tadir    TYPE lcl_tadir=>ty_tadir_tt,
          lv_count    TYPE c LENGTH 3,
          lv_answer   TYPE c LENGTH 1,
          lo_repo     TYPE REF TO lcl_repo,
          lv_package  TYPE devclass,
          lv_question TYPE c LENGTH 100.


    lo_repo = lcl_app=>repo_srv( )->get( iv_key ).
    lv_package = lo_repo->get_package( ).

    lt_tadir = lcl_tadir=>read( lv_package ).

    IF lines( lt_tadir ) > 0.
      lv_count = lines( lt_tadir ).

      CONCATENATE 'This will delete all objects in package' lv_package
        INTO lv_question
        SEPARATED BY space.                                 "#EC NOTEXT

      CONCATENATE lv_question '(' lv_count 'objects)'
        INTO lv_question
        SEPARATED BY space.                                 "#EC NOTEXT

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Uninstall'
          text_question         = lv_question
          text_button_1         = 'Delete'
          icon_button_1         = 'ICON_DELETE'
          text_button_2         = 'Cancel'
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '2'
          display_cancel_button = abap_false
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.                        "#EC NOTEXT
      IF sy-subrc <> 0.
        _raise 'error from POPUP_TO_CONFIRM'.
      ENDIF.

      IF lv_answer = '2'.
        RETURN.
      ENDIF.

      lcl_objects=>delete( lt_tadir ).

    ENDIF.

    lcl_app=>repo_srv( )->delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.                    "repo_purge

  METHOD repo_remove.

    DATA: lv_answer   TYPE c LENGTH 1,
          lo_repo     TYPE REF TO lcl_repo,
          lv_package  TYPE devclass,
          lv_question TYPE c LENGTH 100.


    lo_repo = lcl_app=>repo_srv( )->get( iv_key ).
    lv_package = lo_repo->get_package( ).

    CONCATENATE 'This will remove the repository reference to the package'
      lv_package
      INTO lv_question
      SEPARATED BY space.                                   "#EC NOTEXT

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Remove'
        text_question         = lv_question
        text_button_1         = 'Remove'
        icon_button_1         = 'ICON_WF_UNLINK'
        text_button_2         = 'Cancel'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.                          "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'error from POPUP_TO_CONFIRM'.
    ENDIF.

    IF lv_answer = '2'.
      RETURN.
    ENDIF.

    lcl_app=>repo_srv( )->delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.                    "repo_remove

  METHOD repo_new_offline.

    DATA: lv_returncode TYPE c,
          lv_url        TYPE string,
          lv_package    TYPE devclass,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.

    "               TAB           FLD       LABEL     DEF                 ATTR
    _add_dialog_fld 'ABAPTXT255' 'LINE'     'Name'    ''                  ''.
    _add_dialog_fld 'TDEVC'      'DEVCLASS' 'Package' ''                  ''.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'New Offline Project'             "#EC NOTEXT
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      _raise 'Error from POPUP_GET_VALUES'.
    ENDIF.
    IF lv_returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    lv_url = <ls_field>-value.

    READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    lv_package = <ls_field>-value.
    TRANSLATE lv_package TO UPPER CASE.

    lcl_app=>repo_srv( )->new_offline(
      iv_url     = lv_url
      iv_package = lv_package ).

    COMMIT WORK.

  ENDMETHOD.                    "repo_new_offline

  METHOD repo_package_zip.

    DATA: lo_repo       TYPE REF TO lcl_repo_offline,
          ls_data       TYPE lcl_persistence_repo=>ty_repo,
          lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.

    "               TAB           FLD       LABEL     DEF                 ATTR
    _add_dialog_fld 'TDEVC'      'DEVCLASS' 'Package' ''                  ''.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Export'             "#EC NOTEXT
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      _raise 'Error from POPUP_GET_VALUES'.
    ENDIF.
    IF lv_returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    TRANSLATE <ls_field>-value TO UPPER CASE.

    ls_data-key             = 'DUMMY'.
    ls_data-package         = <ls_field>-value.
    ls_data-master_language = sy-langu.

    CREATE OBJECT lo_repo
      EXPORTING
        is_data = ls_data.

    lcl_zip=>export( lo_repo ).

  ENDMETHOD.                    "repo_package_zip

  METHOD switch_branch.

    DATA: lo_repo  TYPE REF TO lcl_repo_online,
          ls_popup TYPE ty_popup.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    ls_popup = repo_popup(
      iv_url     = lo_repo->get_url( )
      iv_package = lo_repo->get_package( )
      iv_branch  = lo_repo->get_branch_name( ) ).
    IF ls_popup-cancel = abap_true.
      RETURN.
    ENDIF.

    lo_repo->set_url( ls_popup-url ).
    lo_repo->set_branch_name( ls_popup-branch_name ).

    COMMIT WORK.

    lo_repo->deserialize( ).

  ENDMETHOD.

  METHOD reset.

    DATA: lo_repo   TYPE REF TO lcl_repo_online,
          lv_answer TYPE c LENGTH 1.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Warning'
        text_question         = 'Reset local objects?'
        text_button_1         = 'Ok'
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'Cancel'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.                        "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'error from POPUP_TO_CONFIRM'.
    ENDIF.

    IF lv_answer = '2'.
      RETURN.
    ENDIF.

    lo_repo->deserialize( ).

  ENDMETHOD.

  METHOD create_branch_popup.

    DATA: lv_answer TYPE c LENGTH 1,
          lt_fields TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    CLEAR ev_name.
    CLEAR ev_cancel.

*                   TAB     FLD   LABEL   DEF                       ATTR
    _add_dialog_fld 'TEXTL' 'LINE' 'Name' 'refs/heads/branch_name'  ''.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Create branch'
      IMPORTING
        returncode      = lv_answer
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2 ##NO_TEXT.
    IF sy-subrc <> 0.
      _raise 'error from POPUP_GET_VALUES'.
    ENDIF.

    IF lv_answer = 'A'.
      ev_cancel = abap_true.
    ELSE.
      READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      ev_name = <ls_field>-value.
    ENDIF.

  ENDMETHOD.

  METHOD create_branch.

    DATA: lv_name   TYPE string,
          lv_cancel TYPE abap_bool,
          lo_repo   TYPE REF TO lcl_repo_online.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    create_branch_popup(
      IMPORTING
        ev_name   = lv_name
        ev_cancel = lv_cancel ).
    IF lv_cancel = abap_true.
      RETURN.
    ENDIF.

    ASSERT lv_name CP 'refs/heads/+*'.

    lcl_git_porcelain=>create_branch(
      io_repo = lo_repo
      iv_name = lv_name
      iv_from = lo_repo->get_sha1_local( ) ).

* automatically switch to new branch
    lo_repo->set_branch_name( lv_name ).

    MESSAGE 'Switched to new branch' TYPE 'S'.

  ENDMETHOD.

  METHOD repo_pull.

    DATA: lo_repo TYPE REF TO lcl_repo_online.

    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).
    lo_repo->refresh( ).
    lo_repo->deserialize( ).

    COMMIT WORK.

  ENDMETHOD.                    "pull

  METHOD get_page_stage.

    DATA: lo_repo       TYPE REF TO lcl_repo_online,
          lo_stage_page TYPE REF TO lcl_gui_page_stage.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    " force refresh on stage, to make sure the latest local and remote files are used
    lo_repo->refresh( ).

    CREATE OBJECT lo_stage_page
      EXPORTING
        io_repo = lo_repo.

    ri_page = lo_stage_page.

  ENDMETHOD.

  METHOD db_delete.

    DATA: lv_answer TYPE c LENGTH 1,
          ls_key    TYPE lcl_persistence_db=>ty_content.


    ls_key = lcl_html_action_utils=>dbkey_decode( iv_getdata ).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Warning'
        text_question         = 'Delete?'
        text_button_1         = 'Ok'
        icon_button_1         = 'ICON_DELETE'
        text_button_2         = 'Cancel'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.                        "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'error from POPUP_TO_CONFIRM'.
    ENDIF.

    IF lv_answer = '2'.
      RETURN.
    ENDIF.

    lcl_app=>db( )->delete(
      iv_type  = ls_key-type
      iv_value = ls_key-value ).

    COMMIT WORK.

  ENDMETHOD.

  METHOD db_save.

    DATA: lv_string  TYPE string,
          ls_content TYPE lcl_persistence_db=>ty_content,
          lt_fields  TYPE tihttpnvp.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    CONCATENATE LINES OF it_postdata INTO lv_string.

    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'type' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    ls_content-type = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'value' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    ls_content-value = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'xmldata' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    IF <ls_field>-value(1) <> '<'.
      ls_content-data_str = <ls_field>-value+1. " hmm
    ENDIF.

    lcl_app=>db( )->update(
      iv_type  = ls_content-type
      iv_value = ls_content-value
      iv_data  = ls_content-data_str ).

    COMMIT WORK.

  ENDMETHOD.

  METHOD commit_push.

    DATA: ls_fields  TYPE lcl_html_action_utils=>ty_commit_fields,
          ls_comment TYPE ty_comment,
          lo_stage   TYPE REF TO lcl_stage,
          lo_repo    TYPE REF TO lcl_repo_online,
          lo_user    TYPE REF TO lcl_persistence_user.

    ls_fields = lcl_html_action_utils=>parse_commit_request( it_postdata ).

    lo_user = lcl_app=>user( ).   " TODO refactor - password manager
    lo_user->set_username( ls_fields-username ).
    lo_user->set_email( ls_fields-email ).

    IF ls_fields-username IS INITIAL.
      _raise 'empty username'.
    ENDIF.
    IF ls_fields-email IS INITIAL.
      _raise 'empty email'.
    ENDIF.
    IF ls_fields-comment IS INITIAL.
      _raise 'empty comment'.
    ENDIF.

    lo_repo            ?= lcl_app=>repo_srv( )->get( ls_fields-repo_key ).
    lo_stage            = lcl_app=>repo_srv( )->get_stage( ls_fields-repo_key ).
    ls_comment-username = ls_fields-username.
    ls_comment-email    = ls_fields-email.
    ls_comment-comment  = ls_fields-comment.

    IF NOT ls_fields-body IS INITIAL.
      CONCATENATE ls_comment-comment gc_newline ls_fields-body
        INTO ls_comment-comment.
    ENDIF.

    lo_repo->push( is_comment = ls_comment
                   io_stage   = lo_stage ).

    COMMIT WORK.

  ENDMETHOD.      "commit_push

  METHOD get_page_commit.

    DATA: lo_commit_page TYPE REF TO lcl_gui_page_commit,
          lv_key         TYPE lcl_persistence_repo=>ty_repo-key.

    lv_key = lcl_html_action_utils=>repo_key_decode( iv_getdata ).

    CREATE OBJECT lo_commit_page
      EXPORTING
        iv_repo_key = lv_key.

    ri_page ?= lo_commit_page.

  ENDMETHOD.

  METHOD stage_handle_action.

    DATA: ls_file  TYPE ty_file,
          lo_stage TYPE REF TO lcl_stage,
          lv_key   TYPE lcl_persistence_repo=>ty_repo-key.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lo_stage->mt_workarea.

    IF iv_action = 'stage_all'.
      lv_key = lcl_html_action_utils=>repo_key_decode( iv_getdata ).
    ELSE.
      lcl_html_action_utils=>file_decode( EXPORTING iv_string = iv_getdata
                                          IMPORTING ev_key    = lv_key
                                                    eg_file   = ls_file ).
    ENDIF.

    lo_stage = lcl_app=>repo_srv( )->get_stage( lv_key ).

    CASE iv_action.
      WHEN 'stage_add'.
        lo_stage->add( iv_path = ls_file-path iv_filename = ls_file-filename ).
      WHEN 'stage_all'.
        LOOP AT lo_stage->mt_workarea ASSIGNING <ls_file>
            WHERE type = lcl_stage=>c_wftype-local.
          lo_stage->add( iv_path     = <ls_file>-file-path
                         iv_filename = <ls_file>-file-filename ).
        ENDLOOP.
      WHEN 'stage_reset'.
        lo_stage->reset( iv_path = ls_file-path iv_filename = ls_file-filename ).
      WHEN 'stage_ignore'.
        lo_stage->ignore( iv_path = ls_file-path iv_filename = ls_file-filename ).
      WHEN 'stage_rm'.
        lo_stage->rm( iv_path = ls_file-path iv_filename = ls_file-filename ).
    ENDCASE.

  ENDMETHOD.        "stage_handle_action

ENDCLASS.           " lcl_gui_router

INCLUDE zabapgit_unit_test.

* Hide Execute button from screen
AT SELECTION-SCREEN OUTPUT.
  PERFORM output.

* SAP back command re-direction
AT SELECTION-SCREEN ON EXIT-COMMAND.
  PERFORM exit.

FORM output.
  DATA: lt_ucomm TYPE TABLE OF sy-ucomm.
  PERFORM set_pf_status IN PROGRAM rsdbrunt IF FOUND.

  APPEND: 'CRET' TO lt_ucomm.  "Button Execute

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = lt_ucomm.
ENDFORM.

FORM exit RAISING lcx_exception.
  CASE sy-ucomm.
    WHEN 'CBAC'.  "Back
      IF lcl_app=>gui( )->back( ) IS INITIAL.
        LEAVE TO SCREEN 1001.
      ENDIF.
  ENDCASE.
ENDFORM.