REPORT zabapgit.

* See https://github.com/larshp/abapGit/

CONSTANTS: gc_xml_version  TYPE string VALUE 'v0.2-alpha',  "#EC NOTEXT
           gc_abap_version TYPE string VALUE 'v0.64'.       "#EC NOTEXT

********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2014 Lars Hvam Petersen
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

TYPES: t_type     TYPE c LENGTH 6,
       t_bitbyte  TYPE c LENGTH 8,
       t_adler32  TYPE x LENGTH 4,
       t_sha1     TYPE c LENGTH 40,
       t_unixtime TYPE c LENGTH 16.

TYPES: BEGIN OF st_node,
         chmod TYPE string,
         name  TYPE string,
         sha1  TYPE t_sha1,
       END OF st_node.
TYPES: tt_nodes TYPE STANDARD TABLE OF st_node WITH DEFAULT KEY.

TYPES: BEGIN OF st_object,
         sha1 TYPE t_sha1,
         type TYPE t_type,
         data TYPE xstring,
       END OF st_object.
TYPES: tt_objects TYPE STANDARD TABLE OF st_object WITH DEFAULT KEY.

TYPES: BEGIN OF st_file,
         path     TYPE string,
         filename TYPE string,
         data     TYPE xstring,
       END OF st_file.
TYPES: tt_files TYPE STANDARD TABLE OF st_file WITH DEFAULT KEY.

TYPES: BEGIN OF st_commit,
         tree      TYPE t_sha1,
         parent    TYPE t_sha1,
         author    TYPE string,
         committer TYPE string,
         body      TYPE string,
       END OF st_commit.

TYPES: BEGIN OF st_repo,
         url         TYPE string,
         branch_name TYPE string,
       END OF st_repo.

TYPES: BEGIN OF st_branch_list,
         sha1 TYPE t_sha1,
         name TYPE string,
       END OF st_branch_list.
TYPES: tt_branch_list TYPE STANDARD TABLE OF st_branch_list WITH DEFAULT KEY.

TYPES: BEGIN OF st_repo_persi,
         url         TYPE string,
         branch_name TYPE string,
         sha1        TYPE t_sha1,
         package     TYPE devclass,
         offline     TYPE sap_bool,
       END OF st_repo_persi.
TYPES: tt_repos_persi TYPE STANDARD TABLE OF st_repo_persi WITH DEFAULT KEY.

TYPES: BEGIN OF st_result,
         obj_type TYPE tadir-object,
         obj_name TYPE tadir-obj_name,
         match    TYPE sap_bool,
         filename TYPE string,
       END OF st_result.
TYPES: tt_results TYPE STANDARD TABLE OF st_result WITH DEFAULT KEY.

TYPES: BEGIN OF st_diff,
         local  TYPE string,
         result TYPE c LENGTH 1,
         remote TYPE string,
       END OF st_diff.
TYPES: tt_diffs TYPE STANDARD TABLE OF st_diff WITH DEFAULT KEY.

TYPES: BEGIN OF st_tadir,
         pgmid    TYPE tadir-pgmid,
         object   TYPE tadir-object,
         obj_name TYPE tadir-obj_name,
         devclass TYPE tadir-devclass,
         korrnum  TYPE tadir-korrnum,
         path     TYPE string,
       END OF st_tadir.
TYPES: tt_tadir TYPE STANDARD TABLE OF st_tadir WITH DEFAULT KEY.

TYPES: tt_icfhandler TYPE STANDARD TABLE OF icfhandler WITH DEFAULT KEY.

TYPES: tt_rs38l_incl TYPE STANDARD TABLE OF rs38l_incl WITH DEFAULT KEY.

TYPES: tt_string TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

TYPES: BEGIN OF st_comment,
         username TYPE string,
         email    TYPE string,
         comment  TYPE string,
       END OF st_comment.

TYPES: BEGIN OF st_item,
         obj_type TYPE tadir-object,
         obj_name TYPE tadir-obj_name,
       END OF st_item.

CONSTANTS: gc_commit TYPE t_type VALUE 'commit',            "#EC NOTEXT
           gc_tree   TYPE t_type VALUE 'tree',              "#EC NOTEXT
           gc_ref_d  TYPE t_type VALUE 'ref_d',             "#EC NOTEXT
           gc_blob   TYPE t_type VALUE 'blob'.              "#EC NOTEXT

CONSTANTS: BEGIN OF gc_chmod,
             file TYPE c LENGTH 6 VALUE '100644',
             dir  TYPE c LENGTH 5 VALUE '40000',
           END OF gc_chmod.

CONSTANTS: gc_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>newline.

CONSTANTS: gc_english TYPE spras VALUE 'E'.

CONSTANTS: BEGIN OF gc_diff,
             insert TYPE c LENGTH 1 VALUE 'I',
             delete TYPE c LENGTH 1 VALUE 'D',
             update TYPE c LENGTH 1 VALUE 'U',
           END OF gc_diff.

DATA: gv_agent TYPE string.

DEFINE _raise.
  raise exception type lcx_exception
    exporting
      iv_text = &1.                                         "#EC NOTEXT
END-OF-DEFINITION.

******************

SELECTION-SCREEN BEGIN OF SCREEN 1001.
* dummy for triggering screen
SELECTION-SCREEN END OF SCREEN 1001.

******************

START-OF-SELECTION.
  PERFORM run.

*----------------------------------------------------------------------*
*       CLASS LCX_EXCEPTION DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.
    DATA mv_text TYPE string.

    METHODS constructor
      IMPORTING iv_text TYPE string.

ENDCLASS.                    "CX_LOCAL_EXCEPTION DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCX_EXCEPTION IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_exception IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mv_text = iv_text.
  ENDMETHOD.                    "CONSTRUCTOR

ENDCLASS.                    "lcx_exception IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCX_NOT_FOUND DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_not_found DEFINITION INHERITING FROM cx_static_check FINAL.

ENDCLASS.                    "CX_LOCAL_EXCEPTION DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCX_NOT_FOUND IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_not_found IMPLEMENTATION.

ENDCLASS.

CLASS lcl_tadir DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      read
        IMPORTING iv_package      TYPE tadir-devclass
        RETURNING VALUE(rt_tadir) TYPE tt_tadir
        RAISING   lcx_exception,
      read_single
        IMPORTING iv_pgmid        TYPE tadir-pgmid DEFAULT 'R3TR'
                  iv_object       TYPE tadir-object
                  iv_obj_name     TYPE tadir-obj_name
        RETURNING VALUE(rs_tadir) TYPE tadir.

  PRIVATE SECTION.
    CLASS-METHODS:
      build
        IMPORTING iv_package      TYPE tadir-devclass
                  iv_parent       TYPE tadir-devclass
                  iv_path         TYPE string
        RETURNING VALUE(rt_tadir) TYPE tt_tadir
        RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_tadir IMPLEMENTATION.

  METHOD read_single.

    SELECT SINGLE * FROM tadir INTO rs_tadir
      WHERE pgmid = iv_pgmid
      AND object = iv_object
      AND obj_name = iv_obj_name.                         "#EC CI_SUBRC

  ENDMETHOD.

  METHOD read.

* start recursion
    rt_tadir = build( iv_package = iv_package
                      iv_parent  = ''
                      iv_path    = '' ).

  ENDMETHOD.

  METHOD build.

    DATA: lv_index    TYPE i,
          lt_tadir    TYPE tt_tadir,
          lt_tdevc    TYPE STANDARD TABLE OF tdevc,
          lv_len      TYPE i,
          lv_path     TYPE string,
          lv_category TYPE seoclassdf-category.

    FIELD-SYMBOLS: <ls_tdevc> LIKE LINE OF lt_tdevc,
                   <ls_tadir> LIKE LINE OF rt_tadir.


    SELECT * FROM tadir
      INTO CORRESPONDING FIELDS OF TABLE rt_tadir
      WHERE devclass = iv_package
      AND object <> 'DEVC'
      AND object <> 'SOTR'
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
            AND ( version = '1' OR version = '0' ).
          IF sy-subrc = 0 AND lv_category = seoc_category_webdynpro_class.
            DELETE rt_tadir INDEX lv_index.
          ENDIF.
      ENDCASE.
    ENDLOOP.

* look for subpackages
    SELECT * FROM tdevc INTO TABLE lt_tdevc
      WHERE parentcl = iv_package.                        "#EC CI_SUBRC
    LOOP AT lt_tdevc ASSIGNING <ls_tdevc>.
      lv_len = strlen( iv_package ).
      IF <ls_tdevc>-devclass(lv_len) <> iv_package.
        _raise 'Unexpected package naming'.
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

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_user DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_user DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS set_username
      IMPORTING iv_username TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS get_username
      RETURNING VALUE(rv_username) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS set_email
      IMPORTING iv_email TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS get_email
      RETURNING VALUE(rv_email) TYPE string
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS read
      IMPORTING iv_name         TYPE tdobname
      RETURNING VALUE(rv_value) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS save
      IMPORTING iv_name  TYPE tdobname
                iv_value TYPE string
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_user DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_user IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_user IMPLEMENTATION.

  METHOD read.

    DATA: lt_lines TYPE TABLE OF tline,
          ls_line  LIKE LINE OF lt_lines.


    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ST'
        language                = gc_english
        name                    = iv_name
        object                  = 'TEXT'
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc <> 4 AND sy-subrc <> 0.
      _raise 'error from READ_TEXT'.
    ENDIF.

    READ TABLE lt_lines INTO ls_line INDEX 1.
    IF sy-subrc = 0.
      rv_value = ls_line-tdline.
    ENDIF.

  ENDMETHOD.                    "get_details

  METHOD save.

    DATA: ls_header TYPE thead,
          lt_lines  TYPE TABLE OF tline,
          ls_line   LIKE LINE OF lt_lines.


    ls_line-tdformat = '*'.
    ls_line-tdline = iv_value.
    APPEND ls_line TO lt_lines.

    ls_header-tdid       = 'ST'.
    ls_header-tdspras    = gc_english.
    ls_header-tdname     = iv_name.
    ls_header-tdobject   = 'TEXT'.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header   = ls_header
      TABLES
        lines    = lt_lines
      EXCEPTIONS
        id       = 1
        language = 2
        name     = 3
        object   = 4
        OTHERS   = 5.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      _raise 'error from SAVE_TEXT'.
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.                    "change

  METHOD set_username.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_USERNAME_' sy-uname INTO lv_name.

    save( iv_name  = lv_name
          iv_value = iv_username ).

  ENDMETHOD.                    "set_username

  METHOD get_username.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_USERNAME_' sy-uname INTO lv_name.

    rv_username = read( lv_name ).

  ENDMETHOD.                    "get_username

  METHOD set_email.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_EMAIL_' sy-uname INTO lv_name.

    save( iv_name  = lv_name
          iv_value = iv_email ).

  ENDMETHOD.                    "set_email

  METHOD get_email.

    DATA: lv_name TYPE tdobname.


    CONCATENATE 'ZABAPGIT_EMAIL_' sy-uname INTO lv_name.

    rv_email = read( lv_name ).

  ENDMETHOD.                    "get_email

ENDCLASS.                    "lcl_user IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_xml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml DEFINITION FINAL.

  PUBLIC SECTION.
    DATA: mi_xml_doc TYPE REF TO if_ixml_document.

    METHODS constructor
      IMPORTING iv_xml   TYPE string OPTIONAL
                iv_empty TYPE sap_bool DEFAULT abap_false
      RAISING   lcx_exception.

    METHODS element_add
      IMPORTING ig_element TYPE data
                iv_name    TYPE string OPTIONAL
                ii_root    TYPE REF TO if_ixml_element OPTIONAL
      RAISING   lcx_exception.

    METHODS element_read
      IMPORTING ii_root    TYPE REF TO if_ixml_element OPTIONAL
                iv_name    TYPE string OPTIONAL
      EXPORTING ev_success TYPE abap_bool
      CHANGING  cg_element TYPE data
      RAISING   lcx_exception.

    METHODS structure_add
      IMPORTING ig_structure TYPE data
                iv_name      TYPE string OPTIONAL
                ii_root      TYPE REF TO if_ixml_element OPTIONAL
      RAISING   lcx_exception.

    METHODS structure_read
      IMPORTING ii_root      TYPE REF TO if_ixml_element OPTIONAL
                iv_name      TYPE string OPTIONAL
      EXPORTING ev_success   TYPE abap_bool
      CHANGING  cg_structure TYPE data
      RAISING   lcx_exception.

    METHODS table_add
      IMPORTING it_table TYPE STANDARD TABLE
                iv_name  TYPE string OPTIONAL
                ii_root  TYPE REF TO if_ixml_element OPTIONAL
      RAISING   lcx_exception.

    METHODS table_read
      IMPORTING ii_root  TYPE REF TO if_ixml_element OPTIONAL
                iv_name  TYPE string OPTIONAL
      CHANGING  ct_table TYPE STANDARD TABLE
      RAISING   lcx_exception.

    METHODS xml_render
      IMPORTING iv_normalize     TYPE sap_bool DEFAULT abap_true
      RETURNING VALUE(rv_string) TYPE string.

    METHODS xml_element
      IMPORTING iv_name           TYPE string
      RETURNING VALUE(ri_element) TYPE REF TO if_ixml_element.

    METHODS xml_add
      IMPORTING ii_root    TYPE REF TO if_ixml_element OPTIONAL
                ii_element TYPE REF TO if_ixml_element.

    METHODS xml_find
      IMPORTING ii_root           TYPE REF TO if_ixml_element OPTIONAL
                iv_name           TYPE string
      RETURNING VALUE(ri_element) TYPE REF TO if_ixml_element.

  PRIVATE SECTION.

    DATA: mi_ixml TYPE REF TO if_ixml,
          mi_root TYPE REF TO if_ixml_element.

    METHODS special_names
      CHANGING cv_name TYPE string.

    METHODS error
      IMPORTING ii_parser TYPE REF TO if_ixml_parser
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_xml DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_xml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml IMPLEMENTATION.

  METHOD xml_find.

    DATA: li_root LIKE ii_root.


    IF ii_root IS BOUND.
      li_root = ii_root.
    ELSE.
      li_root = mi_root.
    ENDIF.

    ri_element = li_root->find_from_name( depth = 0 name = iv_name ).
    IF NOT ri_element IS BOUND.
      RETURN.
    ENDIF.
    li_root->remove_child( ri_element ).

  ENDMETHOD.                    "xml_find

  METHOD xml_element.

    ri_element = mi_xml_doc->create_element( iv_name ).

  ENDMETHOD.                    "xml_element

  METHOD special_names.

    IF cv_name(1) = '*'.
      CONCATENATE 'STAR' cv_name+1 INTO cv_name.
    ELSEIF cv_name(1) = '2'.
      CONCATENATE 'TWO' cv_name+1 INTO cv_name.
    ENDIF.

  ENDMETHOD.                    "special_names

  METHOD structure_read.

    DATA: lv_name      TYPE string,
          li_struct    TYPE REF TO if_ixml_element,
          lo_typedescr TYPE REF TO cl_abap_typedescr,
          lo_descr_ref TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <lg_any>  TYPE any,
                   <ls_comp> LIKE LINE OF lo_descr_ref->components.


    CLEAR cg_structure.
    ev_success = abap_true.

    lo_descr_ref ?= cl_abap_typedescr=>describe_by_data( cg_structure ).
    IF iv_name IS INITIAL.
      lv_name = lo_descr_ref->get_relative_name( ).
      IF lv_name IS INITIAL.
        _raise 'no name, structure read'.
      ENDIF.
    ELSE.
      lv_name = iv_name.
    ENDIF.

    li_struct = xml_find( ii_root = ii_root
                          iv_name = lv_name ).
    IF NOT li_struct IS BOUND.
      ev_success = abap_false.
      RETURN.
    ENDIF.

    LOOP AT lo_descr_ref->components ASSIGNING <ls_comp>.
      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE cg_structure TO <lg_any>.
      ASSERT sy-subrc = 0.

      lv_name = <ls_comp>-name.
      special_names( CHANGING cv_name = lv_name ).

      lo_typedescr = cl_abap_typedescr=>describe_by_data( <lg_any> ).
      CASE lo_typedescr->kind.
        WHEN cl_abap_typedescr=>kind_table.
          table_read( EXPORTING ii_root  = li_struct
                                iv_name  = lv_name
                      CHANGING  ct_table = <lg_any> ).
        WHEN cl_abap_typedescr=>kind_struct.
          structure_read( EXPORTING ii_root      = li_struct
                                    iv_name      = lv_name
                          CHANGING  cg_structure = <lg_any> ).
        WHEN cl_abap_typedescr=>kind_elem.
          element_read( EXPORTING ii_root    = li_struct
                                  iv_name    = lv_name
                        CHANGING  cg_element = <lg_any> ).
        WHEN cl_abap_typedescr=>kind_ref.
          CONTINUE.
        WHEN OTHERS.
          _raise 'unknown kind, structure read'.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.                    "structure_read

  METHOD table_read.

    DATA: lv_name        TYPE string,
          li_root        TYPE REF TO if_ixml_element,
          lv_kind        TYPE abap_typecategory,
          lv_index       TYPE i,
          lv_success     TYPE abap_bool,
          lo_data_descr  TYPE REF TO cl_abap_datadescr,
          lo_table_descr TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <lg_line> TYPE any.


    CLEAR ct_table[].

    lo_table_descr ?= cl_abap_typedescr=>describe_by_data( ct_table ).
    lv_name = lo_table_descr->get_relative_name( ).

    IF lv_name IS INITIAL.
      lv_name = iv_name.
    ENDIF.

    IF lv_name IS INITIAL.
      _raise 'no name, table read'.
    ENDIF.

    li_root = xml_find( ii_root   = ii_root
                        iv_name   = lv_name ).
    IF NOT li_root IS BOUND.
      RETURN.
    ENDIF.

    lo_data_descr = lo_table_descr->get_table_line_type( ).
    lv_kind = lo_data_descr->kind.

    DO.
      APPEND INITIAL LINE TO ct_table ASSIGNING <lg_line>.
      CASE lv_kind.
        WHEN cl_abap_typedescr=>kind_struct.
          structure_read( EXPORTING ii_root    = li_root
                          IMPORTING ev_success = lv_success
                          CHANGING cg_structure = <lg_line> ).
        WHEN cl_abap_typedescr=>kind_elem.
          element_read( EXPORTING ii_root    = li_root
                        IMPORTING ev_success = lv_success
                        CHANGING  cg_element = <lg_line> ).
        WHEN OTHERS.
          _raise 'unknown kind'.
      ENDCASE.

      IF lv_success = abap_false.
        lv_index = lines( ct_table ).
        DELETE ct_table INDEX lv_index.
        ASSERT sy-subrc = 0.
        EXIT. " current loop
      ENDIF.
    ENDDO.

  ENDMETHOD.                    "table_read

  METHOD error.

    DATA: lv_error TYPE i,
          lv_txt1  TYPE string,
          lv_txt2  TYPE string,
          lv_txt3  TYPE string,
          lv_times TYPE i,
          li_error TYPE REF TO if_ixml_parse_error.


    IF ii_parser->num_errors( ) <> 0.
      lv_times = ii_parser->num_errors( ).
      DO lv_times TIMES.
        lv_error = sy-index - 1.
        li_error = ii_parser->get_error( lv_error ).

        lv_txt1 = li_error->get_column( ).
        CONCATENATE 'Column:' lv_txt1 INTO lv_txt1.         "#EC NOTEXT
        lv_txt2 = li_error->get_line( ).
        CONCATENATE 'Line:' lv_txt2 INTO lv_txt2.           "#EC NOTEXT
        lv_txt3 = li_error->get_reason( ).

        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = 'Error from XML parser'                 "#EC NOTEXT
            txt1  = lv_txt1
            txt2  = lv_txt2
            txt3  = lv_txt3.
      ENDDO.
    ENDIF.

    _raise 'Error while parsing XML'.
  ENDMETHOD.                    "error

  METHOD constructor.

    DATA: li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_parser         TYPE REF TO if_ixml_parser.


    mi_ixml = cl_ixml=>create( ).
    mi_xml_doc = mi_ixml->create_document( ).

    IF iv_xml IS SUPPLIED.
      li_stream_factory = mi_ixml->create_stream_factory( ).
      li_istream = li_stream_factory->create_istream_string( iv_xml ).
      li_parser = mi_ixml->create_parser( stream_factory = li_stream_factory
                                          istream        = li_istream
                                          document       = mi_xml_doc ).
      li_parser->set_normalizing( is_normalizing = abap_false ).
      IF li_parser->parse( ) <> 0.
        error( li_parser ).
      ENDIF.

      li_istream->close( ).

      mi_root = mi_xml_doc->find_from_name( depth = 0 name = 'abapGit' ).
    ELSEIF iv_empty = abap_false.
      mi_root = mi_xml_doc->create_element( 'abapGit' ).
      mi_root->set_attribute( name = 'version' value = gc_xml_version ). "#EC NOTEXT
      mi_xml_doc->append_child( mi_root ).
    ENDIF.

  ENDMETHOD.                    "xml_root

  METHOD table_add.

    DATA: lv_name        TYPE string,
          li_table       TYPE REF TO if_ixml_element,
          lv_kind        TYPE abap_typecategory,
          lo_data_descr  TYPE REF TO cl_abap_datadescr,
          lo_table_descr TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <lg_line> TYPE any.


    lo_table_descr ?= cl_abap_typedescr=>describe_by_data( it_table ).
    lv_name = lo_table_descr->get_relative_name( ).

    IF lv_name IS INITIAL.
      lv_name = iv_name.
    ENDIF.

    IF lv_name IS INITIAL.
      _raise 'no name, table add'.
    ENDIF.

    li_table = mi_xml_doc->create_element( lv_name ).
    lo_data_descr = lo_table_descr->get_table_line_type( ).
    lv_kind = lo_data_descr->kind.

    LOOP AT it_table ASSIGNING <lg_line>.
      CASE lv_kind.
        WHEN cl_abap_typedescr=>kind_struct.
          structure_add( ig_structure = <lg_line>
                         ii_root      = li_table ).
        WHEN cl_abap_typedescr=>kind_elem.
          element_add( ig_element = <lg_line>
                       ii_root    = li_table ).
        WHEN OTHERS.
          _raise 'unknown kind'.
      ENDCASE.
    ENDLOOP.

    xml_add( ii_root    = ii_root
             ii_element = li_table ).

  ENDMETHOD.                    "table_add

  METHOD xml_add.

    IF ii_root IS BOUND.
      ii_root->append_child( ii_element ).
    ELSE.
      mi_root->append_child( ii_element ).
    ENDIF.

  ENDMETHOD.                    "xml_add

  METHOD element_add.

    DATA: lo_descr   TYPE REF TO cl_abap_elemdescr,
          lv_string  TYPE string,
          li_element TYPE REF TO if_ixml_element,
          li_text    TYPE REF TO if_ixml_text,
          lv_name    TYPE string.


    lo_descr ?= cl_abap_typedescr=>describe_by_data( ig_element ).

    IF iv_name IS INITIAL.
      lv_name = lo_descr->get_relative_name( ).
      IF lv_name IS INITIAL.
        _raise 'no name, element add'.
      ENDIF.
    ELSE.
      lv_name = iv_name.
    ENDIF.

    li_element = mi_xml_doc->create_element( lv_name ).

    lv_string  = ig_element.
    li_text    = mi_xml_doc->create_text( lv_string ).

    li_element->append_child( li_text ).

    xml_add( ii_root    = ii_root
             ii_element = li_element ).

  ENDMETHOD.                    "element_add

  METHOD element_read.

    DATA: lo_descr   TYPE REF TO cl_abap_elemdescr,
          li_element TYPE REF TO if_ixml_element,
          lv_name    TYPE string.


    ev_success = abap_true.

    lo_descr ?= cl_abap_typedescr=>describe_by_data( cg_element ).

    IF iv_name IS INITIAL.
      lv_name = lo_descr->get_relative_name( ).
      IF lv_name IS INITIAL.
        _raise 'no name, element read'.
      ENDIF.
    ELSE.
      lv_name = iv_name.
    ENDIF.

    li_element = xml_find( ii_root = ii_root
                           iv_name = lv_name ).
    IF NOT li_element IS BOUND.
      ev_success = abap_false.
      RETURN.
    ENDIF.

    cg_element = li_element->get_value( ).

  ENDMETHOD.                    "element_read

  METHOD structure_add.

    DATA: li_structure TYPE REF TO if_ixml_element,
          lv_name      TYPE string,
          lo_typedescr TYPE REF TO cl_abap_typedescr,
          lo_descr     TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <ls_comp> LIKE LINE OF lo_descr->components,
                   <lg_any>  TYPE any.


    lo_descr ?= cl_abap_typedescr=>describe_by_data( ig_structure ).

    IF iv_name IS INITIAL.
      lv_name = lo_descr->get_relative_name( ).
      IF lv_name IS INITIAL.
        _raise 'no name, structure add'.
      ENDIF.
    ELSE.
      lv_name = iv_name.
    ENDIF.
    li_structure = mi_xml_doc->create_element( lv_name ).

    LOOP AT lo_descr->components ASSIGNING <ls_comp>.
      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE ig_structure TO <lg_any>.
      ASSERT sy-subrc = 0.

      lv_name  = <ls_comp>-name.
      special_names( CHANGING cv_name = lv_name ).

      lo_typedescr = cl_abap_typedescr=>describe_by_data( <lg_any> ).
      CASE lo_typedescr->kind.
        WHEN cl_abap_typedescr=>kind_table.
          table_add( it_table = <lg_any>
                     iv_name  = lv_name
                     ii_root  = li_structure ).
        WHEN cl_abap_typedescr=>kind_struct.
          structure_add( ig_structure = <lg_any>
                         iv_name      = lv_name
                         ii_root      = li_structure ).
        WHEN cl_abap_typedescr=>kind_elem.
          element_add( ig_element = <lg_any>
                       iv_name    = lv_name
                       ii_root    = li_structure ).
        WHEN cl_abap_typedescr=>kind_ref.
          CONTINUE.
        WHEN OTHERS.
          _raise 'unknown kind, structure add'.
      ENDCASE.

    ENDLOOP.

    xml_add( ii_root    = ii_root
             ii_element = li_structure ).

  ENDMETHOD.                    "structure_to_xml

  METHOD xml_render.
* will render to codepage UTF-16

    DATA: li_ostream       TYPE REF TO if_ixml_ostream,
          li_renderer      TYPE REF TO if_ixml_renderer,
          li_streamfactory TYPE REF TO if_ixml_stream_factory.


    li_streamfactory = mi_ixml->create_stream_factory( ).

    li_ostream = li_streamfactory->create_ostream_cstring( rv_string ).

    li_renderer = mi_ixml->create_renderer( ostream  = li_ostream
                                            document = mi_xml_doc ).
    IF iv_normalize = abap_true.
      li_renderer->set_normalizing( ).
    ENDIF.
    li_renderer->render( ).

  ENDMETHOD.                    "xml_render

ENDCLASS.                    "lcl_xml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_debug DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_debug DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS debug_toggle
      RAISING lcx_exception.

    CLASS-METHODS render_objects
      IMPORTING iv_message TYPE string
                it_objects TYPE tt_objects.

    CLASS-METHODS message
      IMPORTING iv_message TYPE string.

    CLASS-METHODS get_html
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS get_debug
      RETURNING VALUE(rv_debug) TYPE sap_bool.

    CLASS-METHODS clear.

  PRIVATE SECTION.
    CLASS-DATA: gv_debug TYPE sap_bool VALUE abap_false,
                gv_html  TYPE string.

ENDCLASS.                    "lcl_debug DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_debug IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_debug IMPLEMENTATION.

  METHOD get_debug.
    rv_debug = gv_debug.
  ENDMETHOD.                    "get_debug

  METHOD get_html.
    rv_html = gv_html.
  ENDMETHOD.                    "get_html

  METHOD message.

    IF gv_debug = abap_false.
      RETURN.
    ENDIF.

    CONCATENATE gv_html '<br>' iv_message '<br>' INTO gv_html.

  ENDMETHOD.                    "message

  METHOD render_objects.

    DATA: lv_len    TYPE i,
          lv_text50 TYPE c LENGTH 50.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects.


    IF gv_debug = abap_false.
      RETURN.
    ENDIF.

    message( iv_message ).

    CONCATENATE gv_html '<table border="1">' gc_newline INTO gv_html.
    LOOP AT it_objects ASSIGNING <ls_object>.

      lv_len = xstrlen( <ls_object>-data ).
      IF lv_len > 50.
        lv_len = 50.
      ENDIF.

      lv_text50 = <ls_object>-data(lv_len).
      CONCATENATE gv_html
        '<tr>' gc_newline
        '<td>' <ls_object>-sha1 '</td>' gc_newline
        '<td>' <ls_object>-type '</td>' gc_newline
        '<td>' lv_text50 '</td>' gc_newline
        '</tr>' gc_newline INTO gv_html.
    ENDLOOP.
    CONCATENATE gv_html '</table>' gc_newline INTO gv_html.

  ENDMETHOD.                    "render_objects

  METHOD clear.

    gv_html = '<h2>Debug Information</h2><br>'.

  ENDMETHOD.                    "clear

  METHOD debug_toggle.

    CASE gv_debug.
      WHEN abap_true.
        gv_debug = abap_false.
        MESSAGE 'Debug mode disabled' TYPE 'S'.             "#EC NOTEXT
      WHEN abap_false.
        gv_debug = abap_true.
        MESSAGE 'Debug mode enabled' TYPE 'S'.              "#EC NOTEXT
        clear( ).
      WHEN OTHERS.
        _raise 'Unknown debug toggle'.
    ENDCASE.

  ENDMETHOD.                    "debug_toggle

ENDCLASS.                    "lcl_debug IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_time DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_time DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS get
      RETURNING VALUE(rv_time) TYPE t_unixtime
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CONSTANTS: c_epoch TYPE datum VALUE '19700101'.

ENDCLASS.                    "lcl_time DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_time IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_time IMPLEMENTATION.

  METHOD get.

    DATA: lv_i       TYPE i,
          lv_tz      TYPE tznzone,
          lv_utcdiff TYPE tznutcdiff,
          lv_utcsign TYPE tznutcsign.


    lv_i = sy-datum - c_epoch.
    lv_i = lv_i * 86400.
    lv_i = lv_i + sy-uzeit.

    CALL FUNCTION 'TZON_GET_OS_TIMEZONE'
      IMPORTING
        ef_timezone = lv_tz.

    CALL FUNCTION 'TZON_GET_OFFSET'
      EXPORTING
        if_timezone      = lv_tz
        if_local_date    = sy-datum
        if_local_time    = sy-uzeit
      IMPORTING
        ef_utcdiff       = lv_utcdiff
        ef_utcsign       = lv_utcsign
      EXCEPTIONS
        conversion_error = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      _raise 'Timezone error'.
    ENDIF.

    CASE lv_utcsign.
      WHEN '+'.
        lv_i = lv_i - lv_utcdiff.
      WHEN '-'.
        lv_i = lv_i + lv_utcdiff.
    ENDCASE.

    rv_time = lv_i.
    CONDENSE rv_time.
    rv_time+11 = lv_utcsign.
    rv_time+12 = lv_utcdiff.

  ENDMETHOD.                    "get

ENDCLASS.                    "lcl_time IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_repo DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_url DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS host
      IMPORTING iv_repo        TYPE string
      RETURNING VALUE(rv_host) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS name
      IMPORTING iv_repo        TYPE string
      RETURNING VALUE(rv_name) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS path_name
      IMPORTING iv_repo             TYPE string
      RETURNING VALUE(rv_path_name) TYPE string
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS regex
      IMPORTING iv_repo TYPE string
      EXPORTING ev_host TYPE string
                ev_path TYPE string
                ev_name TYPE string
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_repo DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_repo IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_url IMPLEMENTATION.

  METHOD host.
    regex( EXPORTING iv_repo = iv_repo
           IMPORTING ev_host = rv_host ).
  ENDMETHOD.                    "host

  METHOD name.
    regex( EXPORTING iv_repo = iv_repo
           IMPORTING ev_name = rv_name ).
  ENDMETHOD.                    "short_name

  METHOD path_name.

    DATA: lv_path TYPE string,
          lv_name TYPE string.

    regex( EXPORTING iv_repo = iv_repo
           IMPORTING ev_path = lv_path
                     ev_name = lv_name ).

    CONCATENATE lv_path lv_name INTO rv_path_name.

  ENDMETHOD.                    "path_name

  METHOD regex.

    FIND REGEX '(.*://[^/]*)(.*/)(.*).git' IN iv_repo
                     SUBMATCHES ev_host ev_path ev_name.
    IF sy-subrc <> 0.
      _raise 'Malformed URL'.
    ENDIF.

  ENDMETHOD.                    "url

ENDCLASS.                    "lcl_repo IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_convert DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_convert DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS bitbyte_to_int
      IMPORTING iv_bits       TYPE clike
      RETURNING VALUE(rv_int) TYPE i.

    CLASS-METHODS x_to_bitbyte
      IMPORTING iv_x              TYPE x
      RETURNING VALUE(rv_bitbyte) TYPE t_bitbyte.

    CLASS-METHODS string_to_xstring_utf8
      IMPORTING iv_string         TYPE string
      RETURNING VALUE(rv_xstring) TYPE xstring.

    CLASS-METHODS xstring_to_string_utf8
      IMPORTING iv_data          TYPE xstring
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS xstring_to_int
      IMPORTING iv_xstring  TYPE xstring
      RETURNING VALUE(rv_i) TYPE i
      RAISING   lcx_exception.

    CLASS-METHODS int_to_xstring
      IMPORTING iv_i              TYPE i
                iv_length         TYPE i
      RETURNING VALUE(rv_xstring) TYPE xstring.

ENDCLASS.                    "lcl_convert DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_convert IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_convert IMPLEMENTATION.

  METHOD int_to_xstring.

    DATA: lv_x TYPE x LENGTH 4.


    ASSERT iv_length = 4. " other cases not implemented

    lv_x = iv_i.
    rv_xstring = lv_x.

  ENDMETHOD.                    "int_to_xstring

  METHOD xstring_to_int.

    DATA: lv_xstring TYPE xstring,
          lv_x       TYPE x.


    lv_xstring = iv_xstring.
    WHILE xstrlen( lv_xstring ) > 0.
      lv_x = lv_xstring(1).
      rv_i = rv_i * 256 + lv_x.
      lv_xstring = lv_xstring+1.
    ENDWHILE.

  ENDMETHOD.                    "xstring_to_int

  METHOD xstring_to_string_utf8.

    DATA: lv_len TYPE i,
          lo_obj TYPE REF TO cl_abap_conv_in_ce.


    TRY.
        lo_obj = cl_abap_conv_in_ce=>create(
            input    = iv_data
            encoding = 'UTF-8' ).
        lv_len = xstrlen( iv_data ).

        lo_obj->read( EXPORTING n    = lv_len
                      IMPORTING data = rv_string ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.                  "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "xstring_to_string_utf8

  METHOD string_to_xstring_utf8.

    DATA: lo_obj TYPE REF TO cl_abap_conv_out_ce.


    TRY.
        lo_obj = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

        lo_obj->convert( EXPORTING data = iv_string
                         IMPORTING buffer = rv_xstring ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.                  "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "string_to_xstring_utf8

  METHOD bitbyte_to_int.

    DATA: lv_bits TYPE string.


    lv_bits = iv_bits.

    rv_int = 0.
    WHILE strlen( lv_bits ) > 0.
      rv_int = rv_int * 2.
      IF lv_bits(1) = '1'.
        rv_int = rv_int + 1.
      ENDIF.
      lv_bits = lv_bits+1.
    ENDWHILE.

  ENDMETHOD.                    "bitbyte_to_int

  METHOD x_to_bitbyte.

    DATA: lv_b TYPE n.

    CLEAR rv_bitbyte.

    DO 8 TIMES.
      GET BIT sy-index OF iv_x INTO lv_b.
      CONCATENATE rv_bitbyte lv_b INTO rv_bitbyte.
    ENDDO.

  ENDMETHOD.                    "x_to_bitbyte

ENDCLASS.                    "lcl_convert IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_diff DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_diff DEFINITION FINAL.

  PUBLIC SECTION.
* assumes data is UTF8 based with newlines
* only works with lines up to 255 characters
    CLASS-METHODS diff
      IMPORTING iv_local       TYPE xstring
                iv_remote      TYPE xstring
      RETURNING VALUE(rt_diff) TYPE tt_diffs.

  PRIVATE SECTION.
    CLASS-METHODS: unpack
      IMPORTING iv_local  TYPE xstring
                iv_remote TYPE xstring
      EXPORTING et_local  TYPE abaptxt255_tab
                et_remote TYPE abaptxt255_tab.

    CLASS-METHODS: render
      IMPORTING it_local       TYPE abaptxt255_tab
                it_remote      TYPE abaptxt255_tab
                it_delta       TYPE vxabapt255_tab
      RETURNING VALUE(rt_diff) TYPE tt_diffs.

    CLASS-METHODS: compute
      IMPORTING it_local        TYPE abaptxt255_tab
                it_remote       TYPE abaptxt255_tab
      RETURNING VALUE(rt_delta) TYPE vxabapt255_tab.

ENDCLASS.                    "lcl_diff DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_diff IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_diff IMPLEMENTATION.

  METHOD unpack.

    DATA: lv_local  TYPE string,
          lv_remote TYPE string.


    lv_local  = lcl_convert=>xstring_to_string_utf8( iv_local ).
    lv_remote = lcl_convert=>xstring_to_string_utf8( iv_remote ).

    SPLIT lv_local  AT gc_newline INTO TABLE et_local.
    SPLIT lv_remote AT gc_newline INTO TABLE et_remote.

  ENDMETHOD.

  METHOD compute.

    DATA: lt_trdirtab_old TYPE TABLE OF trdir,
          lt_trdirtab_new TYPE TABLE OF trdir,
          lt_trdir_delta  TYPE TABLE OF xtrdir.


    CALL FUNCTION 'SVRS_COMPUTE_DELTA_REPS'
      TABLES
        texttab_old  = it_remote
        texttab_new  = it_local
        trdirtab_old = lt_trdirtab_old
        trdirtab_new = lt_trdirtab_new
        trdir_delta  = lt_trdir_delta
        text_delta   = rt_delta.

  ENDMETHOD.

  METHOD diff.

    DATA: lt_delta  TYPE vxabapt255_tab,
          lt_local  TYPE abaptxt255_tab,
          lt_remote TYPE abaptxt255_tab.


    unpack( EXPORTING iv_local  = iv_local
                      iv_remote = iv_remote
            IMPORTING et_local  = lt_local
                      et_remote = lt_remote ).

    lt_delta = compute( it_local  = lt_local
                        it_remote = lt_remote ).

    rt_diff = render( it_local  = lt_local
                      it_remote = lt_remote
                      it_delta  = lt_delta ).

  ENDMETHOD.                    "diff

  METHOD render.

    DEFINE _append.
      clear ls_diff.
      ls_diff-local = &1.
      ls_diff-result = &2.
      ls_diff-remote = &3.
      append ls_diff to rt_diff.
    END-OF-DEFINITION.

    DATA: lv_rindex TYPE i VALUE 1,
          lv_lindex TYPE i VALUE 1,
          ls_local  LIKE LINE OF it_local,
          ls_remote LIKE LINE OF it_remote,
          ls_diff   LIKE LINE OF rt_diff,
          lt_delta  LIKE it_delta,
          ls_delta  LIKE LINE OF it_delta.


    lt_delta = it_delta.

    DO.
      READ TABLE lt_delta INTO ls_delta WITH KEY number = lv_rindex.
      IF sy-subrc = 0.
        DELETE lt_delta INDEX sy-tabix.

        CASE ls_delta-vrsflag.
          WHEN gc_diff-delete.
            _append '' gc_diff-delete ls_delta-line.
            lv_rindex = lv_rindex + 1.
          WHEN gc_diff-insert.
            _append ls_delta-line gc_diff-insert ''.
            lv_lindex = lv_lindex + 1.
          WHEN gc_diff-update.
            CLEAR ls_local.
            READ TABLE it_local INTO ls_local INDEX lv_lindex.
            ASSERT sy-subrc = 0.
            _append ls_local gc_diff-update ls_delta-line.
            lv_lindex = lv_lindex + 1.
            lv_rindex = lv_rindex + 1.
          WHEN OTHERS.
            ASSERT 1 = 1 + 1.
        ENDCASE.
      ELSE.
        CLEAR ls_local.
        READ TABLE it_local INTO ls_local INDEX lv_lindex. "#EC CI_SUBRC
        lv_lindex = lv_lindex + 1.
        CLEAR ls_remote.
        READ TABLE it_remote INTO ls_remote INDEX lv_rindex. "#EC CI_SUBRC
        lv_rindex = lv_rindex + 1.
        _append ls_local '' ls_remote.
      ENDIF.

      IF lv_lindex > lines( it_local ) AND lv_rindex > lines( it_remote ).
        EXIT. " current loop
      ENDIF.
    ENDDO.

  ENDMETHOD.

ENDCLASS.                    "lcl_diff IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_common DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_common DEFINITION ABSTRACT.

  PUBLIC SECTION.
    CLASS-DATA: gt_ddic     TYPE TABLE OF dwinactiv,
                gt_programs TYPE TABLE OF dwinactiv.

  PROTECTED SECTION.
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

    CLASS-METHODS xml_to_file
      IMPORTING is_item        TYPE st_item
                iv_extra       TYPE clike OPTIONAL
                io_xml         TYPE REF TO lcl_xml
                iv_normalize   TYPE sap_bool DEFAULT abap_true
      RETURNING VALUE(rs_file) TYPE st_file
      RAISING   lcx_exception.

    CLASS-METHODS read_xml
      IMPORTING is_item       TYPE st_item
                iv_extra      TYPE clike OPTIONAL
                it_files      TYPE tt_files
      RETURNING VALUE(ro_xml) TYPE REF TO lcl_xml
      RAISING   lcx_exception.

    CLASS-METHODS read_abap
      IMPORTING is_item  TYPE st_item
                iv_extra TYPE clike OPTIONAL
                it_files TYPE tt_files
                iv_error TYPE sap_bool DEFAULT abap_true
      CHANGING  ct_abap  TYPE STANDARD TABLE
      RAISING   lcx_exception.

    CLASS-METHODS abap_to_file
      IMPORTING is_item        TYPE st_item
                iv_extra       TYPE clike OPTIONAL
                it_abap        TYPE STANDARD TABLE
      RETURNING VALUE(rs_file) TYPE st_file
      RAISING   lcx_exception.

    CLASS-METHODS activation_add
      IMPORTING iv_type TYPE trobjtype
                iv_name TYPE clike
      RAISING   lcx_exception.

    CLASS-METHODS corr_insert
      IMPORTING is_item    TYPE st_item
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS jump_se11
      IMPORTING is_item  TYPE st_item
                iv_radio TYPE string
                iv_field TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS read_progdir
      IMPORTING iv_program        TYPE programm
      RETURNING VALUE(rs_progdir) TYPE ty_progdir.

    CLASS-METHODS serialize_program
      IMPORTING is_item         TYPE st_item
                iv_program      TYPE programm OPTIONAL
                iv_extra        TYPE clike OPTIONAL
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize_program
      IMPORTING is_progdir TYPE ty_progdir
                it_source  TYPE abaptxt255_tab
                it_tpool   TYPE textpool_table
                iv_package TYPE devclass
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS filename
      IMPORTING is_item            TYPE st_item
                iv_extra           TYPE clike OPTIONAL
                iv_ext             TYPE string
      RETURNING VALUE(rv_filename) TYPE string.

    CLASS-METHODS serialize_dynpros
      IMPORTING iv_program_name TYPE programm
                io_xml          TYPE REF TO lcl_xml
      RAISING   lcx_exception.

    CLASS-METHODS serialize_cua
      IMPORTING iv_program_name TYPE programm
                io_xml          TYPE REF TO lcl_xml
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_objects_common DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_objects_common IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects_common IMPLEMENTATION.

  METHOD deserialize_program.

    DATA: lv_exists      TYPE sap_bool,
          lv_progname    TYPE reposrc-progname,
          ls_tpool       LIKE LINE OF it_tpool,
          lv_title       TYPE rglif-title,
          ls_progdir_new TYPE progdir.


    READ TABLE it_tpool INTO ls_tpool WITH KEY id = 'R'.
    ASSERT sy-subrc = 0.
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
          LANGUAGE gc_english
          STATE 'I'.
        IF sy-subrc <> 0.
          _raise 'error from INSERT TEXTPOOL'.
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

    ENDIF.

    activation_add( iv_type = 'REPS'
                    iv_name = is_progdir-name ).

  ENDMETHOD.                    "deserialize_program

  METHOD serialize_cua.

    DATA: ls_adm TYPE rsmpe_adm,
          lt_sta TYPE TABLE OF rsmpe_stat,
          lt_fun TYPE TABLE OF rsmpe_funt,
          lt_men TYPE TABLE OF rsmpe_men,
          lt_mtx TYPE TABLE OF rsmpe_mnlt,
          lt_act TYPE TABLE OF rsmpe_act,
          lt_but TYPE TABLE OF rsmpe_but,
          lt_pfk TYPE TABLE OF rsmpe_pfk,
          lt_set TYPE TABLE OF rsmpe_staf,
          lt_doc TYPE TABLE OF rsmpe_atrt,
          lt_tit TYPE TABLE OF rsmpe_titt,
          lt_biv TYPE TABLE OF rsmpe_buts.


    CALL FUNCTION 'RS_CUA_INTERNAL_FETCH'
      EXPORTING
        program         = iv_program_name
        language        = gc_english
        state           = 'A'
      IMPORTING
        adm             = ls_adm
      TABLES
        sta             = lt_sta
        fun             = lt_fun
        men             = lt_men
        mtx             = lt_mtx
        act             = lt_act
        but             = lt_but
        pfk             = lt_pfk
        set             = lt_set
        doc             = lt_doc
        tit             = lt_tit
        biv             = lt_biv
      EXCEPTIONS
        not_found       = 1
        unknown_version = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      _raise 'error from RS_CUA_INTERNAL_FETCH'.
    ENDIF.

    io_xml->structure_add( ls_adm ).
    io_xml->table_add( it_table = lt_sta
                       iv_name = 'RSMPE_STAT_TABLE' ).
    io_xml->table_add( it_table = lt_fun
                       iv_name = 'RSMPE_FUNT_TABLE' ).
    io_xml->table_add( it_table = lt_men
                       iv_name = 'RSMPE_MEN_TABLE' ).
    io_xml->table_add( it_table = lt_mtx
                       iv_name = 'RSMPE_MNLT_TABLE' ).
    io_xml->table_add( it_table = lt_act
                       iv_name = 'RSMPE_ACT_TABLE' ).
    io_xml->table_add( it_table = lt_but
                       iv_name = 'RSMPE_BUT_TABLE' ).
    io_xml->table_add( it_table = lt_pfk
                       iv_name = 'RSMPE_PFK_TABLE' ).
    io_xml->table_add( it_table = lt_set
                       iv_name = 'RSMPE_STAF_TABLE' ).
    io_xml->table_add( it_table = lt_doc
                       iv_name = 'RSMPE_ATRT_TABLE' ).
    io_xml->table_add( it_table = lt_tit
                       iv_name = 'RSMPE_TITT_TABLE' ).
    io_xml->table_add( it_table = lt_biv
                       iv_name = 'RSMPE_BUTS_TABLE' ).

  ENDMETHOD.                    "serialize_cua

  METHOD serialize_dynpros.

    DATA: ls_header               TYPE rpy_dyhead,
          lt_containers           TYPE dycatt_tab,
          lt_fields_to_containers TYPE dyfatc_tab,
          lt_flow_logic           TYPE swydyflow,
          li_element              TYPE REF TO if_ixml_element,
          lt_dynpros              TYPE TABLE OF d020s.

    FIELD-SYMBOLS: <ls_dynpro> LIKE LINE OF lt_dynpros.


    CALL FUNCTION 'RS_SCREEN_LIST'
      EXPORTING
        dynnr     = ''
        progname  = iv_program_name
      TABLES
        dynpros   = lt_dynpros
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 2.
      _raise 'error from screen_list'.
    ENDIF.

* loop dynpros and skip generated selection screens
    LOOP AT lt_dynpros ASSIGNING <ls_dynpro> WHERE type <> 'S'.

      li_element = io_xml->xml_element( 'SCREEN' ).

      CALL FUNCTION 'RPY_DYNPRO_READ'
        EXPORTING
          progname             = iv_program_name
          dynnr                = <ls_dynpro>-dnum
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

      io_xml->structure_add( ig_structure = ls_header
                             ii_root      = li_element ).

      io_xml->table_add( it_table = lt_containers
                         ii_root  = li_element ).
      io_xml->table_add( it_table = lt_fields_to_containers
                         ii_root  = li_element ).
      io_xml->table_add( it_table = lt_flow_logic
                         ii_root  = li_element ).

      io_xml->xml_add( li_element ).

    ENDLOOP.

  ENDMETHOD.                    "serialize_dynpros

  METHOD serialize_program.

    DATA: ls_progdir      TYPE ty_progdir,
          lv_program_name TYPE programm,
          lt_source       TYPE TABLE OF abaptxt255,
          ls_file         LIKE LINE OF rt_files,
          lt_tpool        TYPE textpool_table,
          ls_tpool        LIKE LINE OF lt_tpool,
          lo_xml          TYPE REF TO lcl_xml.

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

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ig_structure = ls_progdir
                           iv_name      = 'PROGDIR' ).
    IF ls_progdir-subc = '1'.
      serialize_dynpros( iv_program_name = lv_program_name
                         io_xml          = lo_xml ).
      serialize_cua( iv_program_name = lv_program_name
                     io_xml          = lo_xml ).
    ENDIF.

    IF lines( lt_tpool ) = 1.
      READ TABLE lt_tpool INDEX 1 INTO ls_tpool.
      ASSERT sy-subrc = 0.
      IF ls_tpool-id = 'R' AND ls_tpool-key = '' AND ls_tpool-length = 0.
        DELETE lt_tpool INDEX 1.
      ENDIF.
    ENDIF.

    lo_xml->table_add( lt_tpool ).

    ls_file = xml_to_file( is_item  = is_item
                           iv_extra = iv_extra
                           io_xml   = lo_xml ).
    APPEND ls_file TO rt_files.

    ls_file = abap_to_file( is_item  = is_item
                            iv_extra = iv_extra
                            it_abap  = lt_source ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.                    "serialize_program

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
    <ls_bdcdata>-fval = is_item-obj_name.

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
        OTHERS                = 4 ##FM_SUBRC_OK. "#EC CI_SUBRC

  ENDMETHOD.                                                "jump_se11

  METHOD corr_insert.

    DATA: ls_object TYPE ddenqs.


    ls_object-objtype = is_item-obj_type.
    ls_object-objname = is_item-obj_name.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = ls_object
        object_class        = 'DICT'
        devclass            = iv_package
        master_language     = gc_english
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

  METHOD filename.

    DATA: lv_obj_name TYPE string.


    lv_obj_name = is_item-obj_name.
* handle namespaces
    REPLACE ALL OCCURRENCES OF '/' IN lv_obj_name WITH '#'.

    IF iv_extra IS INITIAL.
      CONCATENATE lv_obj_name '.' is_item-obj_type '.' iv_ext
        INTO rv_filename.                                   "#EC NOTEXT
    ELSE.
      CONCATENATE lv_obj_name '.' is_item-obj_type '.' iv_extra '.' iv_ext
        INTO rv_filename.                                   "#EC NOTEXT
    ENDIF.
    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.                    "filename

  METHOD activation_add.

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

        APPEND LINES OF lt_objects TO lcl_objects_common=>gt_programs.
      WHEN 'DOMA' OR 'DTEL' OR 'TABL' OR 'INDX' OR 'TTYP' OR 'VIEW' OR 'SHLP' OR 'ENQU'.
* todo also insert_into_working_area?
        APPEND INITIAL LINE TO lcl_objects_common=>gt_ddic ASSIGNING <ls_object>.
        <ls_object>-object   = iv_type.
        <ls_object>-obj_name = lv_obj_name.
      WHEN 'REPS' OR 'DYNP' OR 'CUAD' OR 'REPT' OR 'INTF' OR 'FUNC'.
* these seem to go into the workarea automatically
        APPEND INITIAL LINE TO lcl_objects_common=>gt_programs ASSIGNING <ls_object>.
        <ls_object>-object   = iv_type.
        <ls_object>-obj_name = lv_obj_name.
      WHEN OTHERS.
        _raise 'activate, unknown type'.
    ENDCASE.

  ENDMETHOD.                    "activate

  METHOD read_abap.

    DATA: lv_filename TYPE string,
          lv_abap     TYPE string.

    FIELD-SYMBOLS: <ls_abap> LIKE LINE OF it_files.


    CLEAR ct_abap[].

    lv_filename = filename( is_item  = is_item
                            iv_extra = iv_extra
                            iv_ext   = 'abap' ).            "#EC NOTEXT

    READ TABLE it_files ASSIGNING <ls_abap> WITH KEY filename = lv_filename.
    IF sy-subrc <> 0.
      IF iv_error = abap_true.
        _raise 'abap not found'.
      ELSE.
        RETURN.
      ENDIF.
    ENDIF.
    lv_abap = lcl_convert=>xstring_to_string_utf8( <ls_abap>-data ).

    SPLIT lv_abap AT gc_newline INTO TABLE ct_abap.

  ENDMETHOD.                    "read_abap

  METHOD abap_to_file.

    DATA: lv_source TYPE string.


    CONCATENATE LINES OF it_abap INTO lv_source SEPARATED BY gc_newline.
    CLEAR rs_file.
    rs_file-path = '/'.
    rs_file-filename = filename( is_item  = is_item
                                 iv_extra = iv_extra
                                 iv_ext   = 'abap' ).       "#EC NOTEXT
    rs_file-data = lcl_convert=>string_to_xstring_utf8( lv_source ).

  ENDMETHOD.                    "abap_to_file

  METHOD read_xml.

    DATA: lv_filename TYPE string,
          lv_xml      TYPE string.

    FIELD-SYMBOLS: <ls_xml> LIKE LINE OF it_files.


    lv_filename = filename( is_item  = is_item
                            iv_extra = iv_extra
                            iv_ext   = 'xml' ).             "#EC NOTEXT

    READ TABLE it_files ASSIGNING <ls_xml> WITH KEY filename = lv_filename.
    IF sy-subrc <> 0.
      _raise 'xml not found'.
    ENDIF.

    lv_xml = lcl_convert=>xstring_to_string_utf8( <ls_xml>-data ).

    CREATE OBJECT ro_xml
      EXPORTING
        iv_xml = lv_xml.

  ENDMETHOD.                    "read_xml

  METHOD xml_to_file.

    DATA: lv_xml TYPE string.


    lv_xml = io_xml->xml_render( iv_normalize = iv_normalize ).
    rs_file-path = '/'.

    rs_file-filename = filename( is_item  = is_item
                                 iv_extra = iv_extra
                                 iv_ext   = 'xml' ).        "#EC NOTEXT

    REPLACE FIRST OCCURRENCE
      OF '<?xml version="1.0" encoding="utf-16"?>'
      IN lv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.
    rs_file-data = lcl_convert=>string_to_xstring_utf8( lv_xml ).

  ENDMETHOD.                    "do

ENDCLASS.                    "lcl_objects_common IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_doma DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_doma DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_doma DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_doma IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_doma IMPLEMENTATION.

  METHOD jump.

    jump_se11( is_item  = is_item
               iv_radio = 'RSRD1-DOMA'
               iv_field = 'RSRD1-DOMA_VAL' ).

  ENDMETHOD.                    "jump

  METHOD delete.
* see class CL_WB_DDIC

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = is_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'D'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_DD_DELETE_OBJ, DOMA'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD serialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd01v TYPE dd01v,
          lt_dd07v TYPE TABLE OF dd07v,
          ls_file  TYPE st_file,
          lo_xml   TYPE REF TO lcl_xml.


    lv_name = is_item-obj_name.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = lv_name
        langu         = gc_english
      IMPORTING
        dd01v_wa      = ls_dd01v
      TABLES
        dd07v_tab     = lt_dd07v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_DOMA_GET'.
    ENDIF.
    IF ls_dd01v IS INITIAL.
      RETURN. " does not exist
    ENDIF.

    CLEAR: ls_dd01v-as4user,
           ls_dd01v-as4date,
           ls_dd01v-as4time.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_dd01v ).
    lo_xml->table_add( iv_name = 'DD07V_TAB'
                       it_table = lt_dd07v ).

    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.                    "serialize

  METHOD deserialize.

* package SEDD
* package SDIC

* fm TR_TADIR_INTERFACE
* fm RS_CORR_INSERT ?

    DATA: lo_xml   TYPE REF TO lcl_xml,
          ls_dd01v TYPE dd01v,
          lv_name  TYPE ddobjname,
          lt_dd07v TYPE TABLE OF dd07v.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->structure_read( CHANGING cg_structure = ls_dd01v ).
    lo_xml->table_read( EXPORTING iv_name = 'DD07V_TAB'
                        CHANGING ct_table = lt_dd07v ).

    corr_insert( is_item    = is_item
                 iv_package = iv_package ).

    lv_name = is_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_DOMA_PUT'
      EXPORTING
        name              = lv_name
        dd01v_wa          = ls_dd01v
      TABLES
        dd07v_tab         = lt_dd07v
      EXCEPTIONS
        doma_not_found    = 1
        name_inconsistent = 2
        doma_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_DOMA_PUT'.
    ENDIF.

    activation_add( iv_type = is_item-obj_type
                    iv_name = is_item-obj_name ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_doma IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_dtel DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_dtel IMPLEMENTATION.

  METHOD jump.

    jump_se11( is_item  = is_item
               iv_radio = 'RSRD1-DDTYPE'
               iv_field = 'RSRD1-DDTYPE_VAL' ).

  ENDMETHOD.                    "jump

  METHOD delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = is_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'E'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_DD_DELETE_OBJ, DTEL'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD serialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd04v TYPE dd04v,
          ls_tpara TYPE tpara,
          ls_file  TYPE st_file,
          lo_xml   TYPE REF TO lcl_xml.


    lv_name = is_item-obj_name.

    CALL FUNCTION 'DDIF_DTEL_GET'
      EXPORTING
        name          = lv_name
        langu         = gc_english
      IMPORTING
        dd04v_wa      = ls_dd04v
        tpara_wa      = ls_tpara
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      _raise 'Error from DDIF_DTEL_GET'.
    ENDIF.
    IF ls_dd04v IS INITIAL.
      RETURN. " does not exist
    ENDIF.

    CLEAR: ls_dd04v-as4user,
           ls_dd04v-as4date,
           ls_dd04v-as4time.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_dd04v ).
    lo_xml->structure_add( ls_tpara ).

    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.                    "serialize

  METHOD deserialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          ls_dd04v TYPE dd04v,
          lv_name  TYPE ddobjname,
          ls_tpara TYPE tpara.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->structure_read( CHANGING cg_structure = ls_dd04v ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tpara ).

    corr_insert( is_item    = is_item
                 iv_package = iv_package ).

    lv_name = is_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_DTEL_PUT'
      EXPORTING
        name              = lv_name
        dd04v_wa          = ls_dd04v
      EXCEPTIONS
        dtel_not_found    = 1
        name_inconsistent = 2
        dtel_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_DTEL_PUT'.
    ENDIF.

    activation_add( iv_type = is_item-obj_type
                    iv_name = is_item-obj_name ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_dtel IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_clas DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS deserialize_abap
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                io_xml     TYPE REF TO lcl_xml
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS deserialize_textpool
      IMPORTING is_item TYPE st_item
                io_xml  TYPE REF TO lcl_xml
      RAISING   lcx_exception.

    CLASS-METHODS deserialize_docu
      IMPORTING is_item TYPE st_item
                io_xml  TYPE REF TO lcl_xml
      RAISING   lcx_exception.

    CLASS-METHODS exists
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rv_exists) TYPE sap_bool.

    CLASS-METHODS serialize_abap
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE tt_string
      RAISING   lcx_exception.

    CLASS-METHODS serialize_locals_imp
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE tt_string
      RAISING   lcx_exception.

    CLASS-METHODS serialize_locals_def
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE tt_string
      RAISING   lcx_exception.

    CLASS-METHODS serialize_testclasses
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE tt_string
      RAISING   lcx_exception.

    CLASS-METHODS serialize_macros
      IMPORTING is_clskey        TYPE seoclskey
      RETURNING VALUE(rt_source) TYPE tt_string
      RAISING   lcx_exception.

    CLASS-METHODS serialize_xml
      IMPORTING is_item       TYPE st_item
      RETURNING VALUE(ro_xml) TYPE REF TO lcl_xml
      RAISING   lcx_exception.

    CLASS-METHODS remove_signatures
      CHANGING ct_source TYPE tt_string.

    CLASS-METHODS reduce
      CHANGING ct_source TYPE tt_string.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_clas IMPLEMENTATION.

  METHOD jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = is_item-obj_name
        object_type   = 'CLAS'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD delete.

    DATA: ls_clskey TYPE seoclskey.


    ls_clskey-clsname = is_item-obj_name.

    CASE is_item-obj_type.
      WHEN 'CLAS'.
        CALL FUNCTION 'SEO_CLASS_DELETE_COMPLETE'
          EXPORTING
            clskey       = ls_clskey
          EXCEPTIONS
            not_existing = 1
            is_interface = 2
            db_error     = 3
            no_access    = 4
            other        = 5
            OTHERS       = 6.
        IF sy-subrc <> 0.
          _raise 'Error from SEO_CLASS_DELETE_COMPLETE'.
        ENDIF.
      WHEN 'INTF'.
        CALL FUNCTION 'SEO_INTERFACE_DELETE_COMPLETE'
          EXPORTING
            intkey       = ls_clskey
          EXCEPTIONS
            not_existing = 1
            is_class     = 2
            db_error     = 3
            no_access    = 4
            other        = 5
            OTHERS       = 6.
        IF sy-subrc <> 0.
          _raise 'Error from SEO_INTERFACE_DELETE_COMPLETE'.
        ENDIF.
      WHEN OTHERS.
        _raise 'class delete, unknown type'.
    ENDCASE.

  ENDMETHOD.                    "delete

  METHOD reduce.

    DATA: lv_source LIKE LINE OF ct_source,
          lv_found  TYPE sap_bool.


* skip files that only contain the standard comments
    lv_found = abap_false.
    LOOP AT ct_source INTO lv_source.
      IF strlen( lv_source ) >= 3 AND lv_source(3) <> '*"*'.
        lv_found = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_found = abap_false.
      CLEAR ct_source[].
    ENDIF.

  ENDMETHOD.                    "reduce

  METHOD serialize_locals_imp.

    CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_SOURCE'
      EXPORTING
        clskey                       = is_clskey
        inctype                      = seop_ext_class_locals_imp
      IMPORTING
        source_expanded              = rt_source
      EXCEPTIONS
        _internal_class_not_existing = 1
        not_existing                 = 2
        OTHERS                       = 3.
    IF sy-subrc <> 0 AND sy-subrc <> 2.
      _raise 'Error from get_include_source, imp'.
    ENDIF.

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_local

  METHOD serialize_locals_def.

    CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_SOURCE'
      EXPORTING
        clskey                       = is_clskey
        inctype                      = seop_ext_class_locals_def
      IMPORTING
        source_expanded              = rt_source
      EXCEPTIONS
        _internal_class_not_existing = 1
        not_existing                 = 2
        OTHERS                       = 3.
    IF sy-subrc <> 0 AND sy-subrc <> 2.
      _raise 'Error from get_include_source, def'.
    ENDIF.

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_locals_def

  METHOD serialize_testclasses.

    CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_SOURCE'
      EXPORTING
        clskey                       = is_clskey
        inctype                      = seop_ext_class_testclasses
      IMPORTING
        source_expanded              = rt_source
      EXCEPTIONS
        _internal_class_not_existing = 1
        not_existing                 = 2
        OTHERS                       = 3.
    IF sy-subrc <> 0 AND sy-subrc <> 2.
      _raise 'Error from get_include_source, test'.
    ENDIF.

  ENDMETHOD.                    "serialize_test

  METHOD serialize_macros.

    CALL FUNCTION 'SEO_CLASS_GET_INCLUDE_SOURCE'
      EXPORTING
        clskey                       = is_clskey
        inctype                      = seop_ext_class_macros
      IMPORTING
        source_expanded              = rt_source
      EXCEPTIONS
        _internal_class_not_existing = 1
        not_existing                 = 2
        OTHERS                       = 3.
    IF sy-subrc <> 0 AND sy-subrc <> 2.
      _raise 'Error from get_include_source, macros'.
    ENDIF.

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_macro

  METHOD serialize_abap.

    DATA: lo_source TYPE REF TO cl_oo_source.


    CREATE OBJECT lo_source
      EXPORTING
        clskey             = is_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      _raise 'error from CL_OO_SOURCE'.
    ENDIF.

    lo_source->read( 'A' ).
    rt_source = lo_source->get_old_source( ).
    remove_signatures( CHANGING ct_source = rt_source ).

  ENDMETHOD.                    "serialize_abap

  METHOD remove_signatures.

* signatures messes up in CL_OO_SOURCE when deserializing and serializing
* within same session

    DATA: lv_begin  TYPE string,
          lv_end    TYPE string,
          lv_remove TYPE sap_bool,
          lv_source LIKE LINE OF ct_source.


    CONCATENATE '* <SIGNATURE>------------------------------------'
      '---------------------------------------------------+'
      INTO lv_begin.

    CONCATENATE '* +------------------------------------------------'
      '--------------------------------------</SIGNATURE>'
      INTO lv_end.

    lv_remove = abap_false.
    LOOP AT ct_source INTO lv_source.
      IF lv_source = lv_begin.
        lv_remove = abap_true.
      ENDIF.
      IF lv_remove = abap_true.
        DELETE ct_source INDEX sy-tabix.
      ENDIF.
      IF lv_source = lv_end.
        lv_remove = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "remove_signatures

  METHOD exists.

    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = is_clskey
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.
    IF sy-subrc = 2.
      rv_exists = abap_false.
    ELSE.
      rv_exists = abap_true.
    ENDIF.

  ENDMETHOD.                    "exists

  METHOD serialize.

    DATA: lt_source TYPE seop_source_string,
          ls_file   TYPE st_file,
          lo_xml    TYPE REF TO lcl_xml,
          ls_clskey TYPE seoclskey.


    ls_clskey-clsname = is_item-obj_name.

    IF exists( ls_clskey ) = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_active
        force   = seox_true.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        version = seoc_version_inactive
        force   = seox_true.

    lt_source = serialize_abap( ls_clskey ).
    ls_file = abap_to_file( is_item = is_item
                            it_abap = lt_source ).
    APPEND ls_file TO rt_files.

    IF is_item-obj_type = 'CLAS'.
      lt_source = serialize_locals_def( ls_clskey ).
      IF NOT lt_source[] IS INITIAL.
        ls_file = abap_to_file( is_item  = is_item
                                iv_extra = 'locals_def'
                                it_abap  = lt_source ).     "#EC NOTEXT
        APPEND ls_file TO rt_files.
      ENDIF.

      lt_source = serialize_locals_imp( ls_clskey ).
      IF NOT lt_source[] IS INITIAL.
        ls_file = abap_to_file( is_item  = is_item
                                iv_extra = 'locals_imp'
                                it_abap  = lt_source ).     "#EC NOTEXT
        APPEND ls_file TO rt_files.
      ENDIF.

      lt_source = serialize_testclasses( ls_clskey ).
      IF NOT lt_source[] IS INITIAL.
        ls_file = abap_to_file( is_item  = is_item
                                iv_extra = 'testclasses'
                                it_abap  = lt_source ).     "#EC NOTEXT
        APPEND ls_file TO rt_files.
      ENDIF.

      lt_source = serialize_macros( ls_clskey ).
      IF NOT lt_source[] IS INITIAL.
        ls_file = abap_to_file( is_item  = is_item
                                iv_extra = 'macros'
                                it_abap  = lt_source ).     "#EC NOTEXT
        APPEND ls_file TO rt_files.
      ENDIF.
    ENDIF.

    lo_xml = serialize_xml( is_item ).
    IF lo_xml IS BOUND.
      ls_file = xml_to_file( is_item = is_item
                             io_xml  = lo_xml ).
      APPEND ls_file TO rt_files.
    ENDIF.

  ENDMETHOD.                    "serialize

  METHOD serialize_xml.

    DATA: ls_vseoclass  TYPE vseoclass,
          lv_cp         TYPE program,
          lt_tpool      TYPE textpool_table,
          lv_object     TYPE dokhl-object,
          lv_state      TYPE dokhl-dokstate,
          ls_vseointerf TYPE vseointerf,
          ls_clskey     TYPE seoclskey,
          lt_lines      TYPE tlinetab.


    ls_clskey-clsname = is_item-obj_name.

    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = ls_clskey
        version      = seoc_version_active
      IMPORTING
        class        = ls_vseoclass
        interface    = ls_vseointerf
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc = 1.
      RETURN. " in case only inactive version exists
    ELSEIF sy-subrc <> 0.
      _raise 'error from seo_clif_get'.
    ENDIF.

    CLEAR: ls_vseoclass-uuid,
           ls_vseoclass-author,
           ls_vseoclass-createdon,
           ls_vseoclass-changedby,
           ls_vseoclass-changedon,
           ls_vseoclass-r3release.

    CLEAR: ls_vseointerf-uuid,
           ls_vseointerf-author,
           ls_vseointerf-createdon,
           ls_vseointerf-changedby,
           ls_vseointerf-changedon,
           ls_vseointerf-r3release.

    CREATE OBJECT ro_xml.

    CASE is_item-obj_type.
      WHEN 'CLAS'.
        ro_xml->structure_add( ls_vseoclass ).

        lv_cp = cl_oo_classname_service=>get_classpool_name( ls_clskey-clsname ).
        READ TEXTPOOL lv_cp INTO lt_tpool LANGUAGE gc_english. "#EC CI_READ_REP
        ro_xml->table_add( lt_tpool ).
      WHEN 'INTF'.
        ro_xml->structure_add( ls_vseointerf ).
      WHEN OTHERS.
        ASSERT 1 = 1 + 1.
    ENDCASE.

    lv_object = ls_clskey-clsname.
    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id                = 'CL'
        langu             = gc_english
        object            = lv_object
      IMPORTING
        dokstate          = lv_state
      TABLES
        line              = lt_lines
      EXCEPTIONS
        no_docu_on_screen = 1
        no_docu_self_def  = 2
        no_docu_temp      = 3
        ret_code          = 4
        OTHERS            = 5.
    IF sy-subrc = 0 AND lv_state = 'R'.
      ro_xml->table_add( lt_lines ).
    ENDIF.

  ENDMETHOD.                    "serialize_xml

  METHOD deserialize.

* function group SEOK
* function group SEOQ
* function group SEOP
* class CL_OO_CLASSNAME_SERVICE
* class CL_OO_SOURCE

    DATA: lo_xml TYPE REF TO lcl_xml.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    deserialize_abap( is_item  = is_item
                      it_files = it_files
                      io_xml   = lo_xml
                      iv_package = iv_package ).

    IF is_item-obj_type = 'CLAS'.
      deserialize_textpool( is_item = is_item
                            io_xml  = lo_xml ).
    ENDIF.

    deserialize_docu( is_item = is_item
                      io_xml  = lo_xml ).

  ENDMETHOD.                    "deserialize

  METHOD deserialize_docu.

    DATA: lt_lines  TYPE tlinetab,
          lv_object TYPE dokhl-object.


    io_xml->table_read( CHANGING ct_table = lt_lines ).

    IF lt_lines[] IS INITIAL.
      RETURN.
    ENDIF.

    lv_object = is_item-obj_name.
    CALL FUNCTION 'DOCU_UPD'
      EXPORTING
        id       = 'CL'
        langu    = gc_english
        object   = lv_object
      TABLES
        line     = lt_lines
      EXCEPTIONS
        ret_code = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      _raise 'error from DOCU_UPD'.
    ENDIF.

  ENDMETHOD.                    "deserialize_doku

  METHOD deserialize_textpool.

    DATA: lv_cp      TYPE program,
          lv_clsname TYPE seoclsname,
          lt_tpool   TYPE textpool_table.


    io_xml->table_read( CHANGING ct_table = lt_tpool ).

    IF lt_tpool[] IS INITIAL.
      RETURN.
    ENDIF.

    lv_clsname = is_item-obj_name.
    lv_cp = cl_oo_classname_service=>get_classpool_name( lv_clsname ).

    INSERT TEXTPOOL lv_cp
      FROM lt_tpool
      LANGUAGE gc_english
      STATE 'I'.
    IF sy-subrc <> 0.
      _raise 'error from INSERT TEXTPOOL'.
    ENDIF.

    activation_add( iv_type = 'REPT'
                    iv_name = lv_cp ).

  ENDMETHOD.                    "deserialize_textpool

  METHOD deserialize_abap.

    DATA: ls_vseoclass   TYPE vseoclass,
          ls_vseointerf  TYPE vseointerf,
          lt_source      TYPE seop_source_string,
          lo_source      TYPE REF TO cl_oo_source,
          lt_locals_def  TYPE seop_source_string,
          lt_locals_imp  TYPE seop_source_string,
          lt_locals_mac  TYPE seop_source_string,
          lt_testclasses TYPE seop_source_string,
          ls_clskey      TYPE seoclskey.


    read_abap( EXPORTING is_item  = is_item
                         it_files = it_files
               CHANGING  ct_abap  = lt_source ).

    read_abap( EXPORTING is_item  = is_item
                         iv_extra = 'locals_def'
                         it_files = it_files
                         iv_error = abap_false
               CHANGING  ct_abap  = lt_locals_def ).        "#EC NOTEXT

    read_abap( EXPORTING is_item  = is_item
                         iv_extra = 'locals_imp'
                         it_files = it_files
                         iv_error = abap_false
               CHANGING  ct_abap  = lt_locals_imp ).        "#EC NOTEXT

    read_abap( EXPORTING is_item  = is_item
                         iv_extra = 'macros'
                         it_files = it_files
                         iv_error = abap_false
               CHANGING  ct_abap  = lt_locals_mac ).        "#EC NOTEXT

    read_abap( EXPORTING is_item  = is_item
                         iv_extra = 'testclasses'
                         it_files = it_files
                         iv_error = abap_false
               CHANGING  ct_abap  = lt_testclasses ).       "#EC NOTEXT

    ls_clskey-clsname = is_item-obj_name.


    CASE is_item-obj_type.
      WHEN 'CLAS'.
        io_xml->structure_read( CHANGING cg_structure = ls_vseoclass ).

        CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = seox_true
          CHANGING
            class           = ls_vseoclass
          EXCEPTIONS
            existing        = 1
            is_interface    = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
        IF sy-subrc <> 0.
          _raise 'error from SEO_CLASS_CREATE_COMPLETE'.
        ENDIF.

      WHEN 'INTF'.
        io_xml->structure_read( CHANGING cg_structure = ls_vseointerf ).

        CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = seox_true
          CHANGING
            interface       = ls_vseointerf
          EXCEPTIONS
            existing        = 1
            is_class        = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
        IF sy-subrc <> 0.
          _raise 'Error from SEO_INTERFACE_CREATE_COMPLETE'.
        ENDIF.

      WHEN OTHERS.
        ASSERT 1 = 1 + 1.
    ENDCASE.

    IF is_item-obj_type = 'CLAS'.
      CALL FUNCTION 'SEO_CLASS_GENERATE_LOCALS'
        EXPORTING
          clskey                 = ls_clskey
          force                  = seox_true
          locals_def             = lt_locals_def
          locals_imp             = lt_locals_imp
          locals_mac             = lt_locals_mac
          locals_testclasses     = lt_testclasses
        EXCEPTIONS
          not_existing           = 1
          model_only             = 2
          locals_not_generated   = 3
          locals_not_initialised = 4
          OTHERS                 = 5.
      IF sy-subrc <> 0.
        _raise 'error from generate_locals'.
      ENDIF.
    ENDIF.

    CREATE OBJECT lo_source
      EXPORTING
        clskey             = ls_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      _raise 'error from CL_OO_SOURCE'.
    ENDIF.

    TRY.
        lo_source->access_permission( seok_access_modify ).
        lo_source->set_source( lt_source ).
        lo_source->save( ).
        lo_source->access_permission( seok_access_free ).
      CATCH cx_oo_access_permission.
        _raise 'permission error'.
      CATCH cx_oo_source_save_failure.
        _raise 'save failure'.
    ENDTRY.

    activation_add( iv_type = is_item-obj_type
                    iv_name = is_item-obj_name ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_CLAS IMPLEMENTATION

CLASS lcl_object_smim DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception ##needed.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS get_filename
      IMPORTING iv_url             TYPE string
      RETURNING VALUE(rv_filename) TYPE string.

    CLASS-METHODS find_content
      IMPORTING is_item           TYPE st_item
                it_files          TYPE tt_files
                iv_url            TYPE string
      RETURNING VALUE(rv_content) TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS build_filename
      IMPORTING is_item            TYPE st_item
                iv_filename        TYPE string
      RETURNING VALUE(rv_filename) TYPE string.

    CLASS-METHODS get_url_for_io
      IMPORTING iv_loio      TYPE sdok_docid
      EXPORTING ev_url       TYPE string
                ev_is_folder TYPE boole_d
      RAISING   lcx_not_found
                lcx_exception.

ENDCLASS.

CLASS lcl_object_smim IMPLEMENTATION.

  METHOD get_url_for_io.

    DATA: ls_io       TYPE skwf_io,
          lv_url      TYPE skwf_url,
          ls_smimloio TYPE smimloio.


    SELECT SINGLE * FROM smimloio INTO ls_smimloio
      WHERE loio_id = iv_loio.                          "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_not_found.
    ENDIF.

    IF ls_smimloio-lo_class = wbmr_c_skwf_folder_class.
      ev_is_folder = abap_true.
      ls_io-objtype = skwfc_obtype_folder.
    ELSE.
      ls_io-objtype = skwfc_obtype_loio.
    ENDIF.
    ls_io-class = ls_smimloio-lo_class.
    ls_io-objid = ls_smimloio-loio_id.

    CALL FUNCTION 'SKWF_NMSPC_IO_ADDRESS_GET'
      EXPORTING
        io  = ls_io
      IMPORTING
        url = lv_url.

    ev_url = lv_url.

  ENDMETHOD.

  METHOD build_filename.

    CONCATENATE is_item-obj_name is_item-obj_type iv_filename
      INTO rv_filename SEPARATED BY '.'.
    TRANSLATE rv_filename TO LOWER CASE.

  ENDMETHOD.

  METHOD find_content.

    DATA: lv_filename TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.


    lv_filename = get_filename( iv_url ).

    lv_filename = build_filename( is_item     = is_item
                                  iv_filename = lv_filename ).

    READ TABLE it_files ASSIGNING <ls_file> WITH KEY filename = lv_filename.
    IF sy-subrc <> 0.
      _raise 'SMIM, file not found'.
    ENDIF.

    rv_content = <ls_file>-data.

  ENDMETHOD.

  METHOD get_filename.

    DATA: lv_lines   TYPE i,
          lt_strings TYPE TABLE OF string.


    SPLIT iv_url AT '/' INTO TABLE lt_strings.
    lv_lines = lines( lt_strings ).
    ASSERT lv_lines > 0.
    READ TABLE lt_strings INDEX lv_lines INTO rv_filename.
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD serialize.

    DATA: lv_io       TYPE sdok_docid,
          lv_url      TYPE string,
          lv_folder   TYPE abap_bool,
          ls_file     LIKE LINE OF rt_files,
          lv_filename TYPE string,
          lv_content  TYPE xstring,
          lo_xml      TYPE REF TO lcl_xml,
          li_api      TYPE REF TO if_mr_api.


    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
    lv_io = is_item-obj_name.

    TRY.
        get_url_for_io(
          EXPORTING
            iv_loio      = lv_io
          IMPORTING
            ev_url       = lv_url
            ev_is_folder = lv_folder ).
      CATCH lcx_not_found.
        RETURN.
    ENDTRY.

    IF lv_folder = abap_false.
      li_api->get(
        EXPORTING
          i_url              = lv_url
        IMPORTING
          e_content          = lv_content
        EXCEPTIONS
          parameter_missing  = 1
          error_occured      = 2
          not_found          = 3
          permission_failure = 4
          OTHERS             = 5 ).
      IF sy-subrc <> 0.
        _raise 'error from mime api->get'.
      ENDIF.

      lv_filename = get_filename( lv_url ).
      CLEAR ls_file.
      ls_file-filename = build_filename( is_item     = is_item
                                         iv_filename = lv_filename ).
      ls_file-path = '/'.
      ls_file-data = lv_content.
      APPEND ls_file TO rt_files.
    ENDIF.

    CREATE OBJECT lo_xml.
    lo_xml->element_add( iv_name = 'URL' ig_element = lv_url ).
    lo_xml->element_add( iv_name = 'FOLDER' ig_element = lv_folder ).
    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.

  METHOD deserialize.

    DATA: lv_url      TYPE string,
          lv_folder   TYPE abap_bool,
          lo_xml      TYPE REF TO lcl_xml,
          lv_content  TYPE xstring,
          lv_filename TYPE skwf_filnm,
          lv_io       TYPE sdok_docid,
          ls_skwf_io  TYPE skwf_io,
          li_api      TYPE REF TO if_mr_api.


    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
    lv_io = is_item-obj_name.

    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->element_read( EXPORTING iv_name = 'URL'
                          CHANGING cg_element = lv_url ).
    lo_xml->element_read( EXPORTING iv_name = 'FOLDER'
                          CHANGING cg_element = lv_folder ).

    ls_skwf_io-objid = lv_io.

    IF lv_folder = abap_true.
      li_api->create_folder(
        EXPORTING
          i_url              = lv_url
          i_language         = sy-langu
          i_dev_package      = iv_package
          i_folder_loio      = ls_skwf_io
        EXCEPTIONS
          parameter_missing  = 1
          error_occured      = 2
          cancelled          = 3
          permission_failure = 4
          folder_exists      = 5
          OTHERS             = 6 ).
      IF sy-subrc <> 5 AND sy-subrc <> 0.
        _raise 'error frrom SMIM create_folder'.
      ENDIF.
    ELSE.
      lv_filename = get_filename( lv_url ).
      cl_wb_mime_repository=>determine_io_class(
        EXPORTING
          filename = lv_filename
        IMPORTING
          io_class = ls_skwf_io-class ).
      CONCATENATE ls_skwf_io-class '_L' INTO ls_skwf_io-class.

      lv_content = find_content( is_item  = is_item
                                 it_files = it_files
                                 iv_url   = lv_url ).

      li_api->put(
        EXPORTING
          i_url                   = lv_url
          i_content               = lv_content
          i_dev_package           = iv_package
          i_new_loio              = ls_skwf_io
        EXCEPTIONS
          parameter_missing       = 1
          error_occured           = 2
          cancelled               = 3
          permission_failure      = 4
          data_inconsistency      = 5
          new_loio_already_exists = 6
          is_folder               = 7
          OTHERS                  = 8 ).
      IF sy-subrc <> 0.
        _raise 'error from SMIM put'.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD delete.

    DATA: li_api TYPE REF TO if_mr_api,
          lv_url TYPE string,
          lv_io  TYPE sdok_docid.


    li_api = cl_mime_repository_api=>if_mr_api~get_api( ).
    lv_io = is_item-obj_name.


    TRY.
        get_url_for_io(
          EXPORTING
            iv_loio = lv_io
          IMPORTING
            ev_url  = lv_url ).
      CATCH lcx_not_found.
        RETURN.
    ENDTRY.

    li_api->delete(
      EXPORTING
        i_url              = lv_url
        i_delete_children  = abap_true
      EXCEPTIONS
        parameter_missing  = 1
        error_occured      = 2
        cancelled          = 3
        permission_failure = 4
        not_found          = 5
        OTHERS             = 6 ).
    IF sy-subrc <> 0.
      _raise 'error from delete'.
    ENDIF.

  ENDMETHOD.

  METHOD jump.
    _raise 'todo, SMIM'.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_object_sicf DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception ##needed.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_sicf_key,
             icf_name   TYPE icfservice-icf_name,
             icfparguid TYPE icfservice-icfparguid,
           END OF ty_sicf_key.

    CLASS-METHODS read
      IMPORTING is_item       TYPE st_item
      EXPORTING es_icfservice TYPE icfservice
                es_icfdocu    TYPE icfdocu
                et_icfhandler TYPE tt_icfhandler
                ev_url        TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS insert_sicf
      IMPORTING is_icfservice TYPE icfservice
                is_icfdocu    TYPE icfdocu
                it_icfhandler TYPE tt_icfhandler
                iv_package    TYPE devclass
                iv_url        TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS change_sicf
      IMPORTING is_icfservice TYPE icfservice
                is_icfdocu    TYPE icfdocu
                it_icfhandler TYPE tt_icfhandler
                iv_package    TYPE devclass
                iv_parent     TYPE icfparguid
      RAISING   lcx_exception.

    CLASS-METHODS to_icfhndlist
      IMPORTING it_list        TYPE tt_icfhandler
      RETURNING VALUE(rt_list) TYPE icfhndlist.

    CLASS-METHODS find_parent
      IMPORTING iv_url           TYPE string
      RETURNING VALUE(rv_parent) TYPE icfparguid
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_object_sicf IMPLEMENTATION.

  METHOD serialize.

    DATA: ls_icfservice TYPE icfservice,
          ls_icfdocu    TYPE icfdocu,
          lv_url        TYPE string,
          ls_item       LIKE is_item,
          lo_xml        TYPE REF TO lcl_xml,
          ls_file       LIKE LINE OF rt_files,
          lt_icfhandler TYPE TABLE OF icfhandler.


    read( EXPORTING is_item = is_item
          IMPORTING es_icfservice = ls_icfservice
                    es_icfdocu    = ls_icfdocu
                    et_icfhandler = lt_icfhandler
                    ev_url        = lv_url ).

    IF ls_icfservice IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR ls_icfservice-icfnodguid.
    CLEAR ls_icfservice-icfparguid.

    CREATE OBJECT lo_xml.
    lo_xml->element_add( lv_url ).
    lo_xml->structure_add( ls_icfservice ).
    lo_xml->structure_add( ls_icfdocu ).
    lo_xml->table_add( iv_name = 'ICFHANDLER_TABLE' it_table = lt_icfhandler ).

* multiple SICF nodes with same name cannot be added to repository
    MOVE-CORRESPONDING is_item TO ls_item.
    ls_item-obj_name = ls_icfservice-icf_name.
    ls_file = xml_to_file( is_item = ls_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.

  METHOD read.

    DATA: lt_serv_info TYPE icfservtbl,
          ls_serv_info LIKE LINE OF lt_serv_info,
          ls_key       TYPE ty_sicf_key.

    FIELD-SYMBOLS: <ls_icfhandler> LIKE LINE OF et_icfhandler.


    ls_key = is_item-obj_name.
    IF ls_key-icfparguid IS INITIAL.
* limitation: name must be unique
      SELECT SINGLE icfparguid FROM icfservice
        INTO ls_key-icfparguid
        WHERE icf_name = ls_key-icf_name
        AND icf_cuser <> 'SAP' ##WARN_OK.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDIF.

    cl_icf_tree=>if_icf_tree~get_info_from_serv(
      EXPORTING
        icf_name          = ls_key-icf_name
        icfparguid        = ls_key-icfparguid
        icf_langu         = gc_english
      IMPORTING
        serv_info         = lt_serv_info
        icfdocu           = es_icfdocu
        url               = ev_url
      EXCEPTIONS
        wrong_name        = 1
        wrong_parguid     = 2
        incorrect_service = 3
        no_authority      = 4
        OTHERS            = 5 ).
    IF sy-subrc <> 0.
      _raise 'error from get_info_from_serv'.
    ENDIF.

    ASSERT lines( lt_serv_info ) = 1.
    READ TABLE lt_serv_info INDEX 1 INTO ls_serv_info.
    ASSERT sy-subrc = 0.

    MOVE-CORRESPONDING ls_serv_info-service TO es_icfservice.
    CLEAR es_icfservice-icf_cuser.
    CLEAR es_icfservice-icf_cdate.
    CLEAR es_icfservice-icf_muser.
    CLEAR es_icfservice-icf_mdate.

    CLEAR es_icfdocu-icfparguid.

    APPEND LINES OF ls_serv_info-handlertbl TO et_icfhandler.
    LOOP AT et_icfhandler ASSIGNING <ls_icfhandler>.
      CLEAR <ls_icfhandler>-icfparguid.
    ENDLOOP.

  ENDMETHOD.

  METHOD deserialize.

    DATA: lo_xml        TYPE REF TO lcl_xml,
          ls_icfservice TYPE icfservice,
          ls_read       TYPE icfservice,
          ls_icfdocu    TYPE icfdocu,
          lv_url        TYPE string,
          lt_icfhandler TYPE TABLE OF icfhandler.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->element_read( CHANGING cg_element = lv_url ).
    lo_xml->structure_read( CHANGING cg_structure = ls_icfservice ).
    lo_xml->structure_read( CHANGING cg_structure = ls_icfdocu ).
    lo_xml->table_read( EXPORTING iv_name = 'ICFHANDLER_TABLE'
                        CHANGING ct_table = lt_icfhandler ).

    read( EXPORTING is_item       = is_item
          IMPORTING es_icfservice = ls_read ).
    IF ls_read IS INITIAL.
      insert_sicf( is_icfservice = ls_icfservice
                   is_icfdocu    = ls_icfdocu
                   it_icfhandler = lt_icfhandler
                   iv_package    = iv_package
                   iv_url        = lv_url ).
    ELSE.
      change_sicf( is_icfservice = ls_icfservice
                   is_icfdocu    = ls_icfdocu
                   it_icfhandler = lt_icfhandler
                   iv_package    = iv_package
                   iv_parent     = ls_read-icfparguid ).
    ENDIF.

  ENDMETHOD.

  METHOD to_icfhndlist.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF it_list.


* convert to sorted table
    LOOP AT it_list ASSIGNING <ls_list>.
      INSERT <ls_list>-icfhandler INTO TABLE rt_list.
    ENDLOOP.

  ENDMETHOD.

  METHOD find_parent.

    cl_icf_tree=>if_icf_tree~service_from_url(
      EXPORTING
        url                   = iv_url
        hostnumber            = 0
      IMPORTING
        icfnodguid            = rv_parent
      EXCEPTIONS
        wrong_application     = 1
        no_application        = 2
        not_allow_application = 3
        wrong_url             = 4
        no_authority          = 5
        OTHERS                = 6 ).
    IF sy-subrc <> 0.
      _raise 'error from service_from_url'.
    ENDIF.

  ENDMETHOD.

  METHOD insert_sicf.

    DATA: lt_icfhndlist TYPE icfhndlist,
          ls_icfserdesc TYPE icfserdesc,
          ls_icfdocu    TYPE icfdocu,
          lv_parent     TYPE icfparguid.


    lt_icfhndlist = to_icfhndlist( it_icfhandler ).
    lv_parent = find_parent( iv_url ).

* nice, it seems that the structure should be mistreated
    ls_icfdocu = is_icfdocu-icf_docu.

    MOVE-CORRESPONDING is_icfservice TO ls_icfserdesc.

    cl_icf_tree=>if_icf_tree~insert_node(
      EXPORTING
        icf_name                  = is_icfservice-orig_name
        icfparguid                = lv_parent
        icfdocu                   = ls_icfdocu
        doculang                  = gc_english
        icfhandlst                = lt_icfhndlist
        package                   = iv_package
        application               = space
        icfserdesc                = ls_icfserdesc
        icfactive                 = abap_true
      EXCEPTIONS
        empty_icf_name            = 1
        no_new_virtual_host       = 2
        special_service_error     = 3
        parent_not_existing       = 4
        enqueue_error             = 5
        node_already_existing     = 6
        empty_docu                = 7
        doculang_not_installed    = 8
        security_info_error       = 9
        user_password_error       = 10
        password_encryption_error = 11
        invalid_url               = 12
        invalid_otr_concept       = 13
        formflg401_error          = 14
        handler_error             = 15
        transport_error           = 16
        tadir_error               = 17
        package_not_found         = 18
        wrong_application         = 19
        not_allow_application     = 20
        no_application            = 21
        invalid_icfparguid        = 22
        alt_name_invalid          = 23
        alternate_name_exist      = 24
        wrong_icf_name            = 25
        no_authority              = 26
        OTHERS                    = 27 ).
    IF sy-subrc <> 0.
      _raise 'error from insert_node'.
    ENDIF.

  ENDMETHOD.

  METHOD change_sicf.

    DATA: lt_icfhndlist TYPE icfhndlist,
          ls_icfserdesc TYPE icfserdesc.


    lt_icfhndlist = to_icfhndlist( it_icfhandler ).

    MOVE-CORRESPONDING is_icfservice TO ls_icfserdesc.

    cl_icf_tree=>if_icf_tree~change_node(
      EXPORTING
        icf_name                  = is_icfservice-orig_name
        icfparguid                = iv_parent
        icfdocu                   = is_icfdocu
        doculang                  = gc_english
        icfhandlst                = lt_icfhndlist
        package                   = iv_package
        application               = space
        icfserdesc                = ls_icfserdesc
        icfactive                 = abap_true
      EXCEPTIONS
        empty_icf_name            = 1
        no_new_virtual_host       = 2
        special_service_error     = 3
        parent_not_existing       = 4
        enqueue_error             = 5
        node_already_existing     = 6
        empty_docu                = 7
        doculang_not_installed    = 8
        security_info_error       = 9
        user_password_error       = 10
        password_encryption_error = 11
        invalid_url               = 12
        invalid_otr_concept       = 13
        formflg401_error          = 14
        handler_error             = 15
        transport_error           = 16
        tadir_error               = 17
        package_not_found         = 18
        wrong_application         = 19
        not_allow_application     = 20
        no_application            = 21
        invalid_icfparguid        = 22
        alt_name_invalid          = 23
        alternate_name_exist      = 24
        wrong_icf_name            = 25
        no_authority              = 26
        OTHERS                    = 27 ).
    IF sy-subrc <> 0.
      _raise 'error from change_node'.
    ENDIF.

  ENDMETHOD.

  METHOD delete.

    DATA: ls_icfservice TYPE icfservice.


    read( EXPORTING is_item       = is_item
          IMPORTING es_icfservice = ls_icfservice ).

    cl_icf_tree=>if_icf_tree~delete_node(
      EXPORTING
        icfparguid                  = ls_icfservice-icfparguid
      CHANGING
        icf_name                    = ls_icfservice-icf_name
      EXCEPTIONS
        no_virtual_host_delete      = 1
        special_service_error       = 2
        enqueue_error               = 3
        node_not_existing           = 4
        node_has_childs             = 5
        node_is_aliased             = 6
        node_not_in_original_system = 7
        transport_error             = 8
        tadir_error                 = 9
        db_error                    = 10
        no_authority                = 11
        OTHERS                      = 12 ).
    IF sy-subrc <> 0.
      _raise 'error from delete_node'.
    ENDIF.

  ENDMETHOD.

  METHOD jump.
    _raise 'todo, SICF'.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_object_ssst DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception ##needed.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS validate_font
      IMPORTING iv_tdfamily TYPE tdfamily
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_object_ssst IMPLEMENTATION.

  METHOD validate_font.

    DATA: lv_tdfamily TYPE tfo01-tdfamily.


    SELECT SINGLE tdfamily FROM tfo01 INTO lv_tdfamily
      WHERE tdfamily = iv_tdfamily.
    IF sy-subrc <> 0.
      _raise 'Font family not found'.
    ENDIF.

  ENDMETHOD.

  METHOD serialize.
* see fm SSF_DOWNLOAD_STYLE

    DATA: lo_xml        TYPE REF TO lcl_xml,
          lv_style_name TYPE tdssname,
          ls_header     TYPE ssfcats,
          ls_file       LIKE LINE OF rt_files,
          lt_paragraphs TYPE TABLE OF ssfparas,
          lt_strings    TYPE TABLE OF ssfstrings,
          lt_tabstops   TYPE TABLE OF stxstab.


    lv_style_name = is_item-obj_name.

    CALL FUNCTION 'SSF_READ_STYLE'
      EXPORTING
        i_style_name             = lv_style_name
        i_style_active_flag      = 'A'
        i_style_variant          = '%MAIN'
        i_style_language         = gc_english
      IMPORTING
        e_header                 = ls_header
      TABLES
        e_paragraphs             = lt_paragraphs
        e_strings                = lt_strings
        e_tabstops               = lt_tabstops
      EXCEPTIONS
        no_name                  = 1
        no_style                 = 2
        active_style_not_found   = 3
        inactive_style_not_found = 4
        no_variant               = 5
        no_main_variant          = 6
        cancelled                = 7
        no_access_permission     = 8
        OTHERS                   = 9.
    IF sy-subrc = 2.
      RETURN.
    ELSEIF sy-subrc <> 0.
      _raise 'error from SSF_READ_STYLE'.
    ENDIF.

    CLEAR ls_header-version.
    CLEAR ls_header-firstuser.
    CLEAR ls_header-firstdate.
    CLEAR ls_header-firsttime.
    CLEAR ls_header-lastuser.
    CLEAR ls_header-lastdate.
    CLEAR ls_header-lasttime.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_header ).
    lo_xml->table_add( it_table = lt_paragraphs
                       iv_name  = 'SSFPARAS' ).
    lo_xml->table_add( it_table = lt_strings
                       iv_name  = 'SSFSTRINGS' ).
    lo_xml->table_add( it_table = lt_tabstops
                       iv_name  = 'STXSTAB' ).

    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.

  METHOD deserialize.
* see fm SSF_UPLOAD_STYLE

    DATA: lo_xml        TYPE REF TO lcl_xml,
          ls_header     TYPE ssfcats,
          lt_paragraphs TYPE TABLE OF ssfparas,
          lt_strings    TYPE TABLE OF ssfstrings,
          lt_tabstops   TYPE TABLE OF stxstab.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->structure_read( CHANGING cg_structure = ls_header ).
    lo_xml->table_read( EXPORTING iv_name = 'SSFPARAS'
                        CHANGING ct_table = lt_paragraphs ).
    lo_xml->table_read( EXPORTING iv_name = 'SSFSTRINGS'
                        CHANGING ct_table = lt_strings ).
    lo_xml->table_read( EXPORTING iv_name = 'STXSTAB'
                        CHANGING ct_table = lt_tabstops ).

    validate_font( ls_header-tdfamily ).

    CALL FUNCTION 'SSF_SAVE_STYLE'
      EXPORTING
        i_header     = ls_header
      TABLES
        i_paragraphs = lt_paragraphs
        i_strings    = lt_strings
        i_tabstops   = lt_tabstops.

    CALL FUNCTION 'SSF_ACTIVATE_STYLE'
      EXPORTING
        i_stylename          = ls_header-stylename
      EXCEPTIONS
        no_name              = 1
        no_style             = 2
        cancelled            = 3
        no_access_permission = 4
        illegal_language     = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      _raise 'error from SSF_ACTIVATE_STYLE'.
    ENDIF.

  ENDMETHOD.

  METHOD delete.

    DATA: lv_stylename TYPE tdssname.


    lv_stylename = is_item-obj_name.

    CALL FUNCTION 'SSF_DELETE_STYLE'
      EXPORTING
        i_stylename           = lv_stylename
        i_with_dialog         = abap_false
        i_with_confirm_dialog = abap_false
      EXCEPTIONS
        no_name               = 1
        no_style              = 2
        style_locked          = 3
        cancelled             = 4
        no_access_permission  = 5
        illegal_language      = 6
        OTHERS                = 7.
    IF sy-subrc <> 0 AND sy-subrc <> 2.
      _raise 'error from SSF_DELETE_STYLE'.
    ENDIF.

  ENDMETHOD.

  METHOD jump.
    _raise 'todo'.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_object_suso DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception ##needed.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_object_wdyn DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception ##needed.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

  PRIVATE SECTION.

    CLASS-DATA:
      gt_components TYPE TABLE OF wdy_ctlr_compo_vrs,
      gt_sources    TYPE TABLE OF wdy_ctlr_compo_source_vrs.

    CLASS-METHODS:
      get_limu_objects
        IMPORTING is_item           TYPE st_item
        RETURNING VALUE(rt_objects) TYPE wdy_md_transport_keys,
      read
        IMPORTING is_item             TYPE st_item
        RETURNING VALUE(rs_component) TYPE wdy_component_metadata
        RAISING   lcx_exception,
      read_controller
        IMPORTING is_key               TYPE wdy_md_controller_key
        RETURNING VALUE(rs_controller) TYPE wdy_md_controller_meta_data
        RAISING   lcx_exception,
      read_definition
        IMPORTING is_key               TYPE wdy_md_component_key
        RETURNING VALUE(rs_definition) TYPE wdy_md_component_meta_data
        RAISING   lcx_exception,
      read_view
        IMPORTING is_key         TYPE wdy_md_view_key
        RETURNING VALUE(rs_view) TYPE wdy_md_view_meta_data
        RAISING   lcx_exception,
      recover_controller
        IMPORTING is_controller TYPE wdy_md_controller_meta_data
        RAISING   lcx_exception,
      recover_definition
        IMPORTING is_definition TYPE wdy_md_component_meta_data
        RAISING   lcx_exception,
      recover_view
        IMPORTING is_view TYPE wdy_md_view_meta_data
        RAISING   lcx_exception,
      delta_controller
        IMPORTING is_controller   TYPE wdy_md_controller_meta_data
        RETURNING VALUE(rs_delta) TYPE svrs2_xversionable_object
        RAISING   lcx_exception,
      delta_definition
        IMPORTING is_definition   TYPE wdy_md_component_meta_data
        RETURNING VALUE(rs_delta) TYPE svrs2_xversionable_object
        RAISING   lcx_exception,
      delta_view
        IMPORTING is_view         TYPE wdy_md_view_meta_data
        RETURNING VALUE(rs_delta) TYPE svrs2_xversionable_object
        RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_object_wdyn IMPLEMENTATION.

  METHOD delta_definition.

    DATA: ls_key       TYPE wdy_md_component_key,
          lv_found     TYPE abap_bool,
          ls_obj_new   TYPE svrs2_versionable_object,
          li_component TYPE REF TO if_wdy_md_component,
          ls_obj_old   TYPE svrs2_versionable_object.


    ls_key-component_name = is_definition-definition-component_name.

    lv_found = cl_wdy_md_component=>check_existency( ls_key-component_name ).
    IF lv_found = abap_false.
      TRY.
          cl_wdy_md_component=>create_complete(
            EXPORTING
              name      = ls_key-component_name
            IMPORTING
              component = li_component ).
          li_component->save_to_database( ).
          li_component->unlock( ).
        CATCH cx_wdy_md_exception.
          _raise 'error creating dummy component'.
      ENDTRY.
    ENDIF.

    ls_obj_new-objtype = wdyn_limu_component_definition.
    ls_obj_new-objname = ls_key-component_name.

    ls_obj_old-objtype = wdyn_limu_component_definition.
    ls_obj_old-objname = ls_key-component_name.

    APPEND is_definition-definition TO ls_obj_old-wdyd-defin.
    ls_obj_old-wdyd-descr = is_definition-descriptions.
    ls_obj_old-wdyd-cusag = is_definition-component_usages.
    ls_obj_old-wdyd-intrf = is_definition-interface_implementings.
    ls_obj_old-wdyd-libra = is_definition-library_usages.
    ls_obj_old-wdyd-ctuse = is_definition-ext_ctlr_usages.
    ls_obj_old-wdyd-ctmap = is_definition-ext_ctx_mappings.

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      CHANGING
        delta                = rs_delta
      EXCEPTIONS
        inconsistent_objects = 1.
    IF sy-subrc <> 0.
      _raise 'error from SVRS_MAKE_OBJECT_DELTA'.
    ENDIF.

  ENDMETHOD.

  METHOD delta_controller.

    DATA: li_controller TYPE REF TO if_wdy_md_controller,
          lv_found      TYPE abap_bool,
          ls_key        TYPE wdy_md_controller_key,
          ls_obj_new    TYPE svrs2_versionable_object,
          ls_obj_old    TYPE svrs2_versionable_object.

    FIELD-SYMBOLS: <ls_component> LIKE LINE OF gt_components,
                   <ls_source>    LIKE LINE OF gt_sources.


    ls_key-component_name = is_controller-definition-component_name.
    ls_key-controller_name = is_controller-definition-controller_name.

    lv_found = cl_wdy_md_controller=>check_existency(
          component_name  = ls_key-component_name
          controller_name = ls_key-controller_name ).
    IF lv_found = abap_false.
      TRY.
          li_controller ?= cl_wdy_md_controller=>create_complete(
                component_name  = ls_key-component_name
                controller_name = ls_key-controller_name
                controller_type = is_controller-definition-controller_type ).
          li_controller->save_to_database( ).
          li_controller->unlock( ).
        CATCH cx_wdy_md_exception.
          _raise 'error creating dummy controller'.
      ENDTRY.
    ENDIF.

    ls_obj_new-objtype = wdyn_limu_component_controller.
    ls_obj_new-objname = ls_key.

    ls_obj_old-objtype = wdyn_limu_component_controller.
    ls_obj_old-objname = ls_key.

    APPEND is_controller-definition TO ls_obj_old-wdyc-defin.

    LOOP AT gt_components ASSIGNING <ls_component>
        WHERE component_name = ls_key-component_name
        AND controller_name = ls_key-controller_name.
      APPEND <ls_component> TO ls_obj_old-wdyc-ccomp.
    ENDLOOP.
    LOOP AT gt_sources ASSIGNING <ls_source>
        WHERE component_name = ls_key-component_name
        AND controller_name = ls_key-controller_name.
      APPEND <ls_source> TO ls_obj_old-wdyc-ccoms.
    ENDLOOP.

    ls_obj_old-wdyc-descr = is_controller-descriptions.
    ls_obj_old-wdyc-cusag = is_controller-controller_usages.
    ls_obj_old-wdyc-ccomt = is_controller-controller_component_texts.
    ls_obj_old-wdyc-cpara = is_controller-controller_parameters.
    ls_obj_old-wdyc-cpart = is_controller-controller_parameter_texts.
    ls_obj_old-wdyc-cnode = is_controller-context_nodes.
    ls_obj_old-wdyc-cattr = is_controller-context_attributes.
    ls_obj_old-wdyc-cmapp = is_controller-context_mappings.
    ls_obj_old-wdyc-excp  = is_controller-controller_exceptions.
    ls_obj_old-wdyc-excpt = is_controller-controller_exception_texts.
    ls_obj_old-wdyc-fgrps = is_controller-fieldgroups.

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      CHANGING
        delta                = rs_delta
      EXCEPTIONS
        inconsistent_objects = 1.
    IF sy-subrc <> 0.
      _raise 'error from SVRS_MAKE_OBJECT_DELTA'.
    ENDIF.

  ENDMETHOD.

  METHOD delta_view.

    DATA: ls_key       TYPE wdy_md_view_key,
          ls_obj_new   TYPE svrs2_versionable_object,
          ls_obj_old   TYPE svrs2_versionable_object,
          lv_found     TYPE abap_bool,
          li_view      TYPE REF TO if_wdy_md_abstract_view,
          lt_psmodilog TYPE TABLE OF smodilog,
          lt_psmodisrc TYPE TABLE OF smodisrc.

    FIELD-SYMBOLS: <ls_def> LIKE LINE OF ls_obj_old-wdyv-defin.


    ls_key-component_name = is_view-definition-component_name.
    ls_key-view_name      = is_view-definition-view_name.

    lv_found = cl_wdy_md_abstract_view=>check_existency(
                 component_name = ls_key-component_name
                 name           = ls_key-view_name ).
    IF lv_found = abap_false.
      TRY.
          li_view = cl_wdy_md_abstract_view=>create(
                      component_name = is_view-definition-component_name
                      view_name      = is_view-definition-view_name
                      type           = is_view-definition-type ).
          li_view->save_to_database( ).
          li_view->unlock( ).
        CATCH cx_wdy_md_exception.
          _raise 'error creating dummy view'.
      ENDTRY.
    ENDIF.

    ls_obj_new-objtype = wdyn_limu_component_view.
    ls_obj_new-objname = ls_key.

    ls_obj_old-objtype = wdyn_limu_component_view.
    ls_obj_old-objname = ls_key.

    APPEND INITIAL LINE TO ls_obj_old-wdyv-defin ASSIGNING <ls_def>.
    MOVE-CORRESPONDING is_view-definition TO <ls_def>.

    ls_obj_old-wdyv-descr = is_view-descriptions.
    ls_obj_old-wdyv-vcont = is_view-view_containers.
    ls_obj_old-wdyv-vcntt = is_view-view_container_texts.
    ls_obj_old-wdyv-ibplg = is_view-iobound_plugs.
    ls_obj_old-wdyv-ibplt = is_view-iobound_plug_texts.
    ls_obj_old-wdyv-plpar = is_view-plug_parameters.
    ls_obj_old-wdyv-plprt = is_view-plug_parameter_texts.
    ls_obj_old-wdyv-uiele = is_view-ui_elements.
    ls_obj_old-wdyv-uicon = is_view-ui_context_bindings.
    ls_obj_old-wdyv-uievt = is_view-ui_event_bindings.
    ls_obj_old-wdyv-uiddc = is_view-ui_ddic_bindings.
    ls_obj_old-wdyv-uiprp = is_view-ui_properties.
    ls_obj_old-wdyv-navil = is_view-navigation_links.
    ls_obj_old-wdyv-navit = is_view-navigation_target_refs.
    ls_obj_old-wdyv-vshno = is_view-vsh_nodes.
    ls_obj_old-wdyv-vshpl = is_view-vsh_placeholders.
    ls_obj_old-wdyv-views = is_view-viewset_properties.

    CALL FUNCTION 'SVRS_MAKE_OBJECT_DELTA'
      EXPORTING
        obj_old              = ls_obj_new
        obj_new              = ls_obj_old
      CHANGING
        delta                = rs_delta
      EXCEPTIONS
        inconsistent_objects = 1.
    IF sy-subrc <> 0.
      _raise 'error from SVRS_MAKE_OBJECT_DELTA'.
    ENDIF.

  ENDMETHOD.

  METHOD recover_definition.

    DATA: ls_key    TYPE wdy_md_component_key,
          lv_corrnr TYPE trkorr,
          ls_delta  TYPE svrs2_xversionable_object.


    ls_delta = delta_definition( is_definition ).
    ls_key-component_name = is_definition-definition-component_name.

    cl_wdy_md_component=>recover_version(
      EXPORTING
        component_key = ls_key
        delta         = ls_delta-wdyd
      CHANGING
        corrnr        = lv_corrnr ).

  ENDMETHOD.

  METHOD recover_controller.

    DATA: ls_key    TYPE wdy_controller_key,
          lv_corrnr TYPE trkorr,
          ls_delta  TYPE svrs2_xversionable_object.


    ls_delta = delta_controller( is_controller ).
    ls_key-component_name  = is_controller-definition-component_name.
    ls_key-controller_name = is_controller-definition-controller_name.

    cl_wdy_md_controller=>recover_version(
      EXPORTING
        controller_key = ls_key
        delta          = ls_delta-wdyc
      CHANGING
        corrnr         = lv_corrnr ).

  ENDMETHOD.

  METHOD recover_view.

    DATA: ls_key    TYPE wdy_md_view_key,
          lv_corrnr TYPE trkorr,
          ls_delta  TYPE svrs2_xversionable_object.


    ls_delta = delta_view( is_view ).
    ls_key-component_name = is_view-definition-component_name.
    ls_key-view_name      = is_view-definition-view_name.

    cl_wdy_md_abstract_view=>recover_version(
      EXPORTING
        view_key = ls_key
        delta    = ls_delta-wdyv
      CHANGING
        corrnr   = lv_corrnr ).

  ENDMETHOD.

  METHOD read_controller.

    DATA: lt_components TYPE TABLE OF wdy_ctlr_compo_vrs,
          lt_sources    TYPE TABLE OF wdy_ctlr_compo_source_vrs,
          lt_definition TYPE TABLE OF wdy_controller.


    CALL FUNCTION 'WDYC_GET_OBJECT'
      EXPORTING
        controller_key               = is_key
        get_all_translations         = abap_false
      TABLES
        definition                   = lt_definition
        descriptions                 = rs_controller-descriptions
        controller_usages            = rs_controller-controller_usages
        controller_components        = lt_components
        controller_component_sources = lt_sources
        controller_component_texts   = rs_controller-controller_component_texts
        controller_parameters        = rs_controller-controller_parameters
        controller_parameter_texts   = rs_controller-controller_parameter_texts
        context_nodes                = rs_controller-context_nodes
        context_attributes           = rs_controller-context_attributes
        context_mappings             = rs_controller-context_mappings
        fieldgroups                  = rs_controller-fieldgroups
        controller_exceptions        = rs_controller-controller_exceptions
        controller_exception_texts   = rs_controller-controller_exception_texts
      EXCEPTIONS
        not_existing                 = 1
        OTHERS                       = 2.
    IF sy-subrc <> 0.
      _raise 'error from WDYC_GET_OBJECT'.
    ENDIF.

    APPEND LINES OF lt_components TO gt_components.
    APPEND LINES OF lt_sources TO gt_sources.

    READ TABLE lt_definition INDEX 1 INTO rs_controller-definition.
    IF sy-subrc <> 0.
      _raise 'WDYC, definition not found'.
    ENDIF.

    CLEAR: rs_controller-definition-author,
           rs_controller-definition-createdon,
           rs_controller-definition-changedby,
           rs_controller-definition-changedon.

  ENDMETHOD.

  METHOD read_definition.

    DATA: lt_definition TYPE TABLE OF wdy_component.


    CALL FUNCTION 'WDYD_GET_OBJECT'
      EXPORTING
        component_key           = is_key
        get_all_translations    = abap_false
      TABLES
        definition              = lt_definition
        descriptions            = rs_definition-descriptions
        component_usages        = rs_definition-component_usages
        interface_implementings = rs_definition-interface_implementings
        library_usages          = rs_definition-library_usages
        ext_ctlr_usages         = rs_definition-ext_ctlr_usages
        ext_ctx_mappings        = rs_definition-ext_ctx_mappings
      EXCEPTIONS
        not_existing            = 1
        OTHERS                  = 2.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      _raise 'error from WDYD_GET_OBJECT'.
    ENDIF.

    READ TABLE lt_definition INDEX 1 INTO rs_definition-definition.
    IF sy-subrc <> 0.
      _raise 'WDYD, definition not found'.
    ENDIF.

    CLEAR: rs_definition-definition-author,
           rs_definition-definition-createdon,
           rs_definition-definition-changedby,
           rs_definition-definition-changedon,
           rs_definition-definition-gendate,
           rs_definition-definition-gentime.

  ENDMETHOD.

  METHOD read_view.

    DATA: lt_definition TYPE TABLE OF wdy_view_vrs,
          lt_psmodilog  TYPE TABLE OF smodilog,
          lt_psmodisrc  TYPE TABLE OF smodisrc.

    FIELD-SYMBOLS: <ls_definition> LIKE LINE OF lt_definition.


    CALL FUNCTION 'WDYV_GET_OBJECT'
      EXPORTING
        view_key               = is_key
        get_all_translations   = abap_false
      TABLES
        definition             = lt_definition
        descriptions           = rs_view-descriptions
        view_containers        = rs_view-view_containers
        view_container_texts   = rs_view-view_container_texts
        iobound_plugs          = rs_view-iobound_plugs
        iobound_plug_texts     = rs_view-iobound_plug_texts
        plug_parameters        = rs_view-plug_parameters
        plug_parameter_texts   = rs_view-plug_parameter_texts
        ui_elements            = rs_view-ui_elements
        ui_context_bindings    = rs_view-ui_context_bindings
        ui_event_bindings      = rs_view-ui_event_bindings
        ui_ddic_bindings       = rs_view-ui_ddic_bindings
        ui_properties          = rs_view-ui_properties
        navigation_links       = rs_view-navigation_links
        navigation_target_refs = rs_view-navigation_target_refs
        vsh_nodes              = rs_view-vsh_nodes
        vsh_placeholders       = rs_view-vsh_placeholders
        viewset_properties     = rs_view-viewset_properties
        psmodilog              = lt_psmodilog
        psmodisrc              = lt_psmodisrc
      EXCEPTIONS
        not_existing           = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      _raise 'error from WDYV_GET_OBJECT'.
    ENDIF.

    READ TABLE lt_definition INDEX 1 ASSIGNING <ls_definition>.
    ASSERT sy-subrc = 0.
    MOVE-CORRESPONDING <ls_definition> TO rs_view-definition.

    CLEAR: rs_view-definition-author,
           rs_view-definition-createdon,
           rs_view-definition-changedby,
           rs_view-definition-changedon.

  ENDMETHOD.

  METHOD get_limu_objects.

    DATA: lv_name TYPE wdy_component_name.


    lv_name = is_item-obj_name.
    CALL FUNCTION 'WDYN_GET_LIMU_OBJECTS'
      EXPORTING
        component_name = lv_name
      IMPORTING
        limu_objects   = rt_objects.

  ENDMETHOD.

  METHOD read.

    DATA: lt_objects        TYPE wdy_md_transport_keys,
          ls_controller_key TYPE wdy_md_controller_key,
          ls_component_key  TYPE wdy_md_component_key,
          ls_view_key       TYPE wdy_md_view_key.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.


    CLEAR gt_components.
    CLEAR gt_sources.

    lt_objects = get_limu_objects( is_item ).

    LOOP AT lt_objects ASSIGNING <ls_object>.
      CASE <ls_object>-sub_type.
        WHEN wdyn_limu_component_controller.
          ls_controller_key = <ls_object>-sub_name.
          APPEND read_controller( ls_controller_key ) TO rs_component-ctlr_metadata.
        WHEN wdyn_limu_component_definition.
          ls_component_key = <ls_object>-sub_name.
          rs_component-comp_metadata = read_definition( ls_component_key ).
        WHEN wdyn_limu_component_view.
          ls_view_key = <ls_object>-sub_name.
          APPEND read_view( ls_view_key ) TO rs_component-view_metadata.
        WHEN OTHERS.
          ASSERT 1 = 1 + 1.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD serialize.

    DATA: ls_file      TYPE st_file,
          lo_xml       TYPE REF TO lcl_xml,
          ls_component TYPE wdy_component_metadata.


    ls_component = read( is_item ).

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_component ).
    lo_xml->table_add( it_table = gt_components
                       iv_name  = 'COMPONENTS' ).
    lo_xml->table_add( it_table = gt_sources
                       iv_name  = 'SOURCES' ).
    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.

  METHOD deserialize.

    DATA: lo_xml       TYPE REF TO lcl_xml,
          ls_component TYPE wdy_component_metadata.

    FIELD-SYMBOLS: <ls_view>       LIKE LINE OF ls_component-view_metadata,
                   <ls_controller> LIKE LINE OF ls_component-ctlr_metadata.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->structure_read( CHANGING cg_structure = ls_component ).
    lo_xml->table_read(
      EXPORTING
        iv_name  = 'COMPONENTS'
      CHANGING
        ct_table = gt_components ).
    lo_xml->table_read(
      EXPORTING
        iv_name  = 'SOURCES'
      CHANGING
        ct_table = gt_sources ).

    ls_component-comp_metadata-definition-author = sy-uname.
    ls_component-comp_metadata-definition-createdon = sy-datum.
    recover_definition( ls_component-comp_metadata ).

    LOOP AT ls_component-ctlr_metadata ASSIGNING <ls_controller>.
      <ls_controller>-definition-author = sy-uname.
      <ls_controller>-definition-createdon = sy-datum.
      recover_controller( <ls_controller> ).
    ENDLOOP.
    LOOP AT ls_component-view_metadata ASSIGNING <ls_view>.
      <ls_view>-definition-author = sy-uname.
      <ls_view>-definition-createdon = sy-datum.
      recover_view( <ls_view> ).
    ENDLOOP.

    activation_add( iv_type = is_item-obj_type
                    iv_name = is_item-obj_name ).

  ENDMETHOD.

  METHOD delete.

    DATA: lo_component   TYPE REF TO cl_wdy_wb_component,
          lo_request     TYPE REF TO cl_wb_request,
          li_state       TYPE REF TO if_wb_program_state,
          lv_object_name TYPE seu_objkey.


    CREATE OBJECT lo_component.

    lv_object_name = is_item-obj_name.
    CREATE OBJECT lo_request
      EXPORTING
        p_object_type = 'YC'
        p_object_name = lv_object_name
        p_operation   = swbm_c_op_delete_no_dialog.

    lo_component->if_wb_program~process_wb_request(
      p_wb_request       = lo_request
      p_wb_program_state = li_state ).

  ENDMETHOD.

  METHOD jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = is_item-obj_name
        object_type   = is_item-obj_type
        in_new_window = abap_true.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_object_wdca DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS read
      IMPORTING is_item    TYPE st_item
      EXPORTING es_outline TYPE wdy_cfg_outline_data
                et_data    TYPE wdy_cfg_persist_data_appl_tab
      RAISING   lcx_exception.

    CLASS-METHODS save
      IMPORTING is_outline TYPE wdy_cfg_outline_data
                it_data    TYPE wdy_cfg_persist_data_appl_tab
                iv_package TYPE devclass
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_object_wdca IMPLEMENTATION.

  METHOD save.

    _raise 'WDCA, save, todo'.

*    DATA: lo_cfg       TYPE REF TO cl_wdr_cfg_persistence_appl,
*          ls_key       TYPE wdy_config_key,
*          ls_data      LIKE LINE OF it_data,
*          lv_operation TYPE i,
*          lv_name      TYPE wdy_md_object_name.
*
*
*    MOVE-CORRESPONDING is_outline TO ls_key.
*
*    TRY.
*        CREATE OBJECT lo_cfg
*          EXPORTING
*            config_key  = ls_key
*            object_name = lv_name.
*
*        READ TABLE it_data INDEX 1 INTO ls_data.
*        ASSERT sy-subrc = 0.
*
*        lv_operation = if_wdr_cfg_constants=>c_cts_operation-e_save.
*        lo_cfg->do_next_step( CHANGING c_operation = lv_operation ).
*        lo_cfg->do_next_step( CHANGING c_operation = lv_operation ).
*
*        lo_cfg->set_save_data( ls_data ).
*
*      CATCH cx_wd_configuration.
*        _raise 'WDCA, save error'.
*    ENDTRY.

  ENDMETHOD.

  METHOD read.

    DATA: lo_cfg      TYPE REF TO cl_wdr_cfg_persistence_appl,
          ls_key      TYPE wdy_config_key,
          lt_messages TYPE cts_messages,
          lv_exists   TYPE abap_bool,
          lx_err      TYPE REF TO cx_wd_configuration,
          lv_name     TYPE wdy_md_object_name.


    ls_key = is_item-obj_name.

    TRY.
        CREATE OBJECT lo_cfg
          EXPORTING
            config_key  = ls_key
            object_name = lv_name.

        MOVE-CORRESPONDING ls_key TO es_outline.

        lo_cfg->check_config_existent(
          EXPORTING
            i_outline_data       = es_outline
            i_only_current_layer = abap_false
            i_is_original        = abap_true
          IMPORTING
            e_is_existent        = lv_exists ).
        IF lv_exists = abap_false.
          RETURN.
        ENDIF.

        es_outline = lo_cfg->read_outline_data( ).
      CATCH cx_wd_configuration INTO lx_err.
        IF lx_err->textid = cx_wd_configuration=>conf_config_not_exist.
          RETURN.
        ELSE.
          _raise 'WDCA, read error'.
        ENDIF.
    ENDTRY.

    CLEAR: es_outline-devclass,
           es_outline-author,
           es_outline-createdon,
           es_outline-changedby,
           es_outline-changedon.
    et_data = lo_cfg->read_data( ).

  ENDMETHOD.

  METHOD serialize.

    DATA: ls_file    TYPE st_file,
          lo_xml     TYPE REF TO lcl_xml,
          ls_outline TYPE wdy_cfg_outline_data,
          lt_data    TYPE wdy_cfg_persist_data_appl_tab.


    read( EXPORTING is_item = is_item
          IMPORTING es_outline = ls_outline
                    et_data = lt_data ).
    IF ls_outline IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_outline ).
    lo_xml->table_add( lt_data ).
    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.

  METHOD deserialize.

    DATA: lo_xml     TYPE REF TO lcl_xml,
          ls_outline TYPE wdy_cfg_outline_data,
          lt_data    TYPE wdy_cfg_persist_data_appl_tab.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->structure_read( CHANGING cg_structure = ls_outline ).
    lo_xml->table_read( CHANGING ct_table = lt_data ).

    save( is_outline = ls_outline
          it_data    = lt_data
          iv_package = iv_package ).

  ENDMETHOD.

  METHOD delete.

    DATA: ls_key TYPE wdy_config_key.


    ls_key = is_item-obj_name.

    cl_wdr_configuration_utils=>delete_config_4_appl( ls_key ).

  ENDMETHOD.

  METHOD jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = is_item-obj_name
        object_type   = is_item-obj_type
        in_new_window = abap_true.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_object_wdya DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS read
      IMPORTING is_item       TYPE st_item
      EXPORTING es_app        TYPE wdy_application
                et_properties TYPE wdy_app_property_table
      RAISING   lcx_exception.

    CLASS-METHODS save
      IMPORTING is_app        TYPE wdy_application
                it_properties TYPE wdy_app_property_table
                iv_package    TYPE devclass
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_object_wdya IMPLEMENTATION.

  METHOD read.

    DATA: li_app  TYPE REF TO if_wdy_md_application,
          li_map  TYPE REF TO if_object_map,
          lo_prop TYPE REF TO cl_wdy_md_application_property,
          ls_prop LIKE LINE OF et_properties,
          lv_name TYPE wdy_application_name.


    lv_name = is_item-obj_name.
    TRY.
        li_app = cl_wdy_md_application=>get_object_by_key(
                   name    = lv_name
                   version = 'A' ).
      CATCH cx_wdy_md_not_existing.
        RETURN.
      CATCH cx_wdy_md_permission_failure.
        _raise 'WDYA, permission failure'.
    ENDTRY.

    li_app->if_wdy_md_object~get_definition( IMPORTING definition = es_app ).
    CLEAR: es_app-author,
           es_app-createdon,
           es_app-changedby,
           es_app-changedon.

    li_map = li_app->get_properties( ).
    DO li_map->size( ) TIMES.
      lo_prop ?= li_map->get_by_position( sy-index ).
      lo_prop->get_definition( IMPORTING definition = ls_prop ).
      APPEND ls_prop TO et_properties.
    ENDDO.

  ENDMETHOD.

  METHOD serialize.

    DATA: ls_file       TYPE st_file,
          lo_xml        TYPE REF TO lcl_xml,
          ls_app        TYPE wdy_application,
          lt_properties TYPE wdy_app_property_table.


    read( EXPORTING is_item = is_item
          IMPORTING es_app = ls_app
                    et_properties = lt_properties ).

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_app ).
    lo_xml->table_add( lt_properties ).
    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.

  METHOD save.

    DATA: li_prop TYPE REF TO if_wdy_md_application_property,
          lo_app  TYPE REF TO cl_wdy_md_application.

    FIELD-SYMBOLS: <ls_property> LIKE LINE OF it_properties.


    TRY.
        CREATE OBJECT lo_app
          EXPORTING
            name       = is_app-application_name
            definition = is_app
            devclass   = iv_package.

        LOOP AT it_properties ASSIGNING <ls_property>.
          li_prop = lo_app->if_wdy_md_application~create_property( <ls_property>-name ).
          li_prop->set_value( <ls_property>-value ).
        ENDLOOP.

        lo_app->if_wdy_md_lockable_object~save_to_database( ).
      CATCH cx_wdy_md_exception.
        _raise 'error saving WDYA'.
    ENDTRY.

  ENDMETHOD.

  METHOD deserialize.

    DATA: lo_xml        TYPE REF TO lcl_xml,
          ls_app        TYPE wdy_application,
          lt_properties TYPE wdy_app_property_table.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->structure_read( CHANGING cg_structure = ls_app ).
    lo_xml->table_read( CHANGING ct_table = lt_properties ).

    save( is_app        = ls_app
          it_properties = lt_properties
          iv_package    = iv_package ).

  ENDMETHOD.

  METHOD delete.

    DATA: li_app  TYPE REF TO if_wdy_md_application,
          lv_name TYPE wdy_application_name.


    lv_name = is_item-obj_name.
    TRY.
        li_app = cl_wdy_md_application=>get_object_by_key(
                   name    = lv_name
                   version = 'A' ).
        li_app->if_wdy_md_object~delete( ).
        li_app->if_wdy_md_lockable_object~save_to_database( ).
      CATCH cx_wdy_md_not_existing.
        RETURN.
      CATCH cx_wdy_md_exception.
        _raise 'WDYA, error deleting'.
    ENDTRY.

  ENDMETHOD.

  METHOD jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = is_item-obj_name
        object_type   = is_item-obj_type
        in_new_window = abap_true.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_object_susc DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception ##needed.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_object_suso IMPLEMENTATION.

  METHOD serialize.

    DATA: lo_xml        TYPE REF TO lcl_xml,
          ls_file       LIKE LINE OF rt_files,
          ls_tobj       TYPE tobj,
          ls_tobjt      TYPE tobjt,
          ls_tobjvorflg TYPE tobjvorflg,
          lt_tactz      TYPE TABLE OF tactz,
          lt_tobjvordat TYPE TABLE OF tobjvordat,
          lt_tobjvor    TYPE TABLE OF tobjvor.


    SELECT SINGLE * FROM tobj INTO ls_tobj
      WHERE objct = is_item-obj_name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    CLEAR ls_tobj-bname.

    SELECT SINGLE * FROM tobjt INTO ls_tobjt
      WHERE object = is_item-obj_name
      AND langu = gc_english.                           "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'TOBJT no english description'.
    ENDIF.

    SELECT SINGLE * FROM tobjvorflg INTO ls_tobjvorflg
      WHERE objct = is_item-obj_name.                     "#EC CI_SUBRC

    SELECT * FROM tactz INTO TABLE lt_tactz
      WHERE brobj = is_item-obj_name.     "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT * FROM tobjvordat INTO TABLE lt_tobjvordat
      WHERE objct = is_item-obj_name.     "#EC CI_SUBRC "#EC CI_GENBUFF

    SELECT * FROM tobjvor INTO TABLE lt_tobjvor
      WHERE objct = is_item-obj_name.                     "#EC CI_SUBRC

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_tobj ).
    lo_xml->structure_add( ls_tobjt ).
    lo_xml->structure_add( ls_tobjvorflg ).
    lo_xml->table_add( it_table = lt_tactz
                       iv_name = 'TACTZ' ).
    lo_xml->table_add( it_table = lt_tobjvordat
                       iv_name = 'TOBJVORDAT' ).
    lo_xml->table_add( it_table = lt_tobjvor
                       iv_name = 'TOBJVOR' ).
    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.

  METHOD deserialize.
* see function group SUSA

    DATA: lo_xml        TYPE REF TO lcl_xml,
          lv_objectname TYPE e071-obj_name,
          ls_tobj       TYPE tobj,
          ls_tobjt      TYPE tobjt,
          ls_tobjvorflg TYPE tobjvorflg,
          lt_tactz      TYPE TABLE OF tactz,
          lt_tobjvordat TYPE TABLE OF tobjvordat,
          lt_tobjvor    TYPE TABLE OF tobjvor.


    ASSERT NOT is_item-obj_name IS INITIAL.

    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tobj ).
    ls_tobj-bname = sy-uname.
    lo_xml->structure_read( CHANGING cg_structure = ls_tobjt ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tobjvorflg ).
    lo_xml->table_read( EXPORTING iv_name  = 'TACTZ'
                        CHANGING  ct_table = lt_tactz ).
    lo_xml->table_read( EXPORTING iv_name  = 'TOBJVORDAT'
                        CHANGING  ct_table = lt_tobjvordat ).
    lo_xml->table_read( EXPORTING iv_name  = 'TOBJVOR'
                        CHANGING  ct_table = lt_tobjvor ).

    lv_objectname = is_item-obj_name.
    CALL FUNCTION 'SUSR_COMMEDITCHECK'
      EXPORTING
        objectname      = lv_objectname
        transobjecttype = 'O'.

    MODIFY tobj FROM ls_tobj.                             "#EC CI_SUBRC
    MODIFY tobjt FROM ls_tobjt.                           "#EC CI_SUBRC
    MODIFY tobjvorflg FROM ls_tobjvorflg.                 "#EC CI_SUBRC
    DELETE FROM tactz WHERE brobj = is_item-obj_name.     "#EC CI_SUBRC
    INSERT tactz FROM TABLE lt_tactz.                     "#EC CI_SUBRC
    DELETE FROM tobjvordat WHERE objct = is_item-obj_name. "#EC CI_SUBRC
    INSERT tobjvordat FROM TABLE lt_tobjvordat.           "#EC CI_SUBRC
    DELETE FROM tobjvor WHERE objct = is_item-obj_name.   "#EC CI_SUBRC
    INSERT tobjvor FROM TABLE lt_tobjvor.                 "#EC CI_SUBRC

  ENDMETHOD.

  METHOD delete.

    DATA: lv_object TYPE tobj-objct.


    lv_object = is_item-obj_name.
    CALL FUNCTION 'SUSR_DELETE_OBJECT'
      EXPORTING
        object = lv_object.

  ENDMETHOD.

  METHOD jump.

    DATA: lv_object TYPE tobj-objct.


    lv_object = is_item-obj_name.
    CALL FUNCTION 'SUSR_SHOW_OBJECT'
      EXPORTING
        object = lv_object.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_object_susc IMPLEMENTATION.

  METHOD serialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          ls_file  LIKE LINE OF rt_files,
          ls_tobc  TYPE tobc,
          ls_tobct TYPE tobct.


    SELECT SINGLE * FROM tobc INTO ls_tobc
      WHERE oclss = is_item-obj_name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tobct INTO ls_tobct
      WHERE oclss = is_item-obj_name
      AND langu = gc_english.
    IF sy-subrc <> 0.
      _raise 'TOBCT no english description'.
    ENDIF.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_tobc ).
    lo_xml->structure_add( ls_tobct ).
    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.

  METHOD deserialize.
* see function group SUSA

    DATA: lo_xml        TYPE REF TO lcl_xml,
          ls_tobc       TYPE tobc,
          lv_objectname TYPE e071-obj_name,
          ls_tobct      TYPE tobct.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tobc ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tobct ).

    lv_objectname = is_item-obj_name.
    CALL FUNCTION 'SUSR_COMMEDITCHECK'
      EXPORTING
        objectname      = lv_objectname
        transobjecttype = 'C'.

    INSERT tobc FROM ls_tobc.                             "#EC CI_SUBRC
* ignore sy-subrc as all fields are key fields

    MODIFY tobct FROM ls_tobct.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD delete.

    DATA: lv_objclass TYPE tobc-oclss.


    lv_objclass = is_item-obj_name.
    CALL FUNCTION 'SUSR_DELETE_OBJECT_CLASS'
      EXPORTING
        objclass = lv_objclass.

  ENDMETHOD.

  METHOD jump.

    DATA: lv_objclass TYPE tobc-oclss.


    lv_objclass = is_item-obj_name.
    CALL FUNCTION 'SUSR_SHOW_OBJECT_CLASS'
      EXPORTING
        objclass = lv_objclass.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_object_para DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception ##needed.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_object_para IMPLEMENTATION.

  METHOD serialize.

    DATA: lo_xml    TYPE REF TO lcl_xml,
          ls_file   LIKE LINE OF rt_files,
          ls_tpara  TYPE tpara,
          ls_tparat TYPE tparat.


    SELECT SINGLE * FROM tpara INTO ls_tpara
      WHERE paramid = is_item-obj_name.                 "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tparat INTO ls_tparat
      WHERE paramid = is_item-obj_name
      AND sprache = gc_english.                         "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'PARA no english description'.
    ENDIF.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_tpara ).
    lo_xml->structure_add( ls_tparat ).
    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.

  METHOD deserialize.
* see fm RS_PARAMETER_ADD and RS_PARAMETER_EDIT

    DATA: lo_xml    TYPE REF TO lcl_xml,
          lv_mode   TYPE c LENGTH 1,
          ls_tpara  TYPE tpara,
          ls_tparat TYPE tparat.


    SELECT SINGLE * FROM tpara INTO ls_tpara
      WHERE paramid = is_item-obj_name.                 "#EC CI_GENBUFF
    IF sy-subrc = 0.
      lv_mode = 'M'.
    ELSE.
      lv_mode = 'I'.
    ENDIF.

    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tpara ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tparat ).

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = is_item-obj_name
        object_class        = 'PARA'
        mode                = lv_mode
        global_lock         = abap_true
        devclass            = iv_package
        master_language     = gc_english
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_CORR_INSERT, PARA'.
    ENDIF.

    MODIFY tpara FROM ls_tpara.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    MODIFY tparat FROM ls_tparat.                         "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.

  METHOD delete.

    DATA: lv_paramid TYPE tpara-paramid.


    lv_paramid = is_item-obj_name.
    CALL FUNCTION 'RS_PARAMETER_DELETE'
      EXPORTING
        objectname = lv_paramid
      EXCEPTIONS
        cancelled  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      _raise 'error from RS_PRAMETER_DELETE'.
    ENDIF.

  ENDMETHOD.

  METHOD jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = is_item-obj_name
        object_type   = 'PARA'
        in_new_window = abap_true.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_object_ssfo DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ssfo DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception ##needed.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ssfo IMPLEMENTATION.

  METHOD jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.


    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMSSFO'.
    <ls_bdcdata>-dynpro   = '0100'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=DISPLAY'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RB_SF'.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'SSFSCREEN-FNAME'.
    <ls_bdcdata>-fval = is_item-obj_name.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                 = 'SMARTFORMS'
        mode_val              = 'E'
      TABLES
        using_tab             = lt_bdcdata
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4 ##FM_SUBRC_OK. "#EC CI_SUBRC

  ENDMETHOD.                    "jump

  METHOD delete.

    DATA: lv_formname TYPE tdsfname.


    lv_formname = is_item-obj_name.

    CALL FUNCTION 'FB_DELETE_FORM'
      EXPORTING
        i_formname            = lv_formname
        i_with_dialog         = abap_false
        i_with_confirm_dialog = abap_false
      EXCEPTIONS
        no_name               = 1
        no_form               = 2
        form_locked           = 3
        no_access_permission  = 4
        illegal_language      = 5
        illegal_formtype      = 6
        OTHERS                = 7.
    IF sy-subrc <> 0 AND sy-subrc <> 2.
      _raise 'Error from FB_DELETE_FORM'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD serialize.
* see function module FB_DOWNLOAD_FORM

    DATA: lo_sf       TYPE REF TO cl_ssf_fb_smart_form,
          lo_xml      TYPE REF TO lcl_xml,
          ls_file     TYPE st_file,
          lv_name     TYPE string,
          li_node     TYPE REF TO if_ixml_node,
          li_element  TYPE REF TO if_ixml_element,
          li_iterator TYPE REF TO if_ixml_node_iterator,
          li_attr     TYPE REF TO if_ixml_named_node_map,
          lv_formname TYPE tdsfname.


    CREATE OBJECT lo_xml
      EXPORTING
        iv_empty = abap_true.

    CREATE OBJECT lo_sf.
    lv_formname = is_item-obj_name. " convert type
    TRY.
        lo_sf->load( im_formname = lv_formname
                     im_language = '' ).
      CATCH cx_ssf_fb.
* the smartform is not present in system, or other error occured
        RETURN.
    ENDTRY.

    lo_sf->xml_download( EXPORTING parent   = lo_xml->mi_xml_doc
                         CHANGING  document = lo_xml->mi_xml_doc ).

    li_iterator = lo_xml->mi_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.

      lv_name = li_node->get_name( ).
      IF lv_name = 'DEVCLASS'
          OR lv_name = 'LASTDATE'
          OR lv_name = 'LASTTIME'.
        li_node->set_value( '' ).
      ENDIF.
      IF lv_name = 'FIRSTUSER'
          OR lv_name = 'LASTUSER'.
        li_node->set_value( 'DUMMY' ).
      ENDIF.

* remove IDs it seems that they are not used for anything
* the IDs are "random" so it caused diff files
      IF lv_name = 'NODE' OR lv_name = 'WINDOW'.
        li_attr = li_node->get_attributes( ).
        li_attr->remove_named_item( 'ID' ).
      ENDIF.

      li_node = li_iterator->get_next( ).
    ENDWHILE.

    li_element = lo_xml->mi_xml_doc->get_root_element( ).
    li_element->set_attribute(
      name      = 'sf'
      namespace = 'xmlns'
      value     = 'urn:sap-com:SmartForms:2000:internal-structure' ). "#EC NOTEXT
    li_element->set_attribute(
      name  = 'xmlns'
      value = 'urn:sap-com:sdixml-ifr:2000' ).              "#EC NOTEXT

* the upload fails when the smartform is normalized
    ls_file = xml_to_file( is_item      = is_item
                           io_xml       = lo_xml
                           iv_normalize = abap_false ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.                    "serialize

  METHOD deserialize.
* see function module FB_UPLOAD_FORM

    DATA: lo_xml      TYPE REF TO lcl_xml,
          li_node     TYPE REF TO if_ixml_node,
          lv_formname TYPE tdsfname,
          lv_name     TYPE string,
          li_iterator TYPE REF TO if_ixml_node_iterator,
          lo_sf       TYPE REF TO cl_ssf_fb_smart_form,
          lo_res      TYPE REF TO cl_ssf_fb_smart_form.


    CREATE OBJECT lo_sf.

    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

* set "created by" and "changed by" to current user
    li_iterator = lo_xml->mi_xml_doc->create_iterator( ).
    li_node = li_iterator->get_next( ).
    WHILE NOT li_node IS INITIAL.
      lv_name = li_node->get_name( ).
      CASE lv_name.
        WHEN 'LASTDATE'.
          li_node->set_value(
            sy-datum(4) && '-' && sy-datum+4(2) && '-' && sy-datum+6(2) ).
        WHEN 'LASTTIME'.
          li_node->set_value(
            sy-uzeit(2) && ':' && sy-uzeit+2(2) && ':' && sy-uzeit+4(2) ).
        WHEN 'FIRSTUSER' OR 'LASTUSER'.
          li_node->set_value( sy-uname && '' ).
      ENDCASE.

      li_node = li_iterator->get_next( ).
    ENDWHILE.

    li_node = lo_xml->mi_xml_doc->get_root_element( ).
    lv_formname = is_item-obj_name.

* todo, iv_package?
    lo_sf->enqueue( suppress_corr_check = space
                    master_language     = gc_english
                    mode                = 'INSERT'
                    formname            = lv_formname ).

    lo_sf->xml_upload( EXPORTING dom      = li_node
                                 formname = lv_formname
                                 language = gc_english
                       CHANGING  sform    = lo_res ).

    lo_res->store( im_formname = lo_res->header-formname
                   im_language = gc_english
                   im_active   = abap_true ).

    lo_sf->dequeue( lv_formname ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_ssfo IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_tabl DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_tabl IMPLEMENTATION.

  METHOD jump.

    jump_se11( is_item  = is_item
               iv_radio = 'RSRD1-DDTYPE'
               iv_field = 'RSRD1-DDTYPE_VAL' ).

  ENDMETHOD.                    "jump

  METHOD delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = is_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_false
        objname              = lv_objname
        objtype              = 'T'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_DD_DELETE_OBJ, TABL'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD serialize.

    DATA: lv_name  TYPE ddobjname,
          lo_xml   TYPE REF TO lcl_xml,
          ls_file  TYPE st_file,
          ls_dd02v TYPE dd02v,
          ls_dd09l TYPE dd09l,
          lt_dd03p TYPE TABLE OF dd03p,
          lt_dd05m TYPE TABLE OF dd05m,
          lt_dd08v TYPE TABLE OF dd08v,
          lt_dd12v TYPE dd12vtab,
          lt_dd17v TYPE dd17vtab,
          lt_dd35v TYPE TABLE OF dd35v,
          lt_dd36m TYPE dd36mttyp.

    FIELD-SYMBOLS: <ls_dd12v> LIKE LINE OF lt_dd12v.


    lv_name = is_item-obj_name.

    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = lv_name
        langu         = gc_english
      IMPORTING
        dd02v_wa      = ls_dd02v
        dd09l_wa      = ls_dd09l
      TABLES
        dd03p_tab     = lt_dd03p
        dd05m_tab     = lt_dd05m
        dd08v_tab     = lt_dd08v
        dd12v_tab     = lt_dd12v
        dd17v_tab     = lt_dd17v
        dd35v_tab     = lt_dd35v
        dd36m_tab     = lt_dd36m
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_TABL_GET'.
    ENDIF.
    IF ls_dd02v IS INITIAL.
      RETURN. " object does not exits
    ENDIF.

    CLEAR: ls_dd02v-as4user,
           ls_dd02v-as4date,
           ls_dd02v-as4time.

    CLEAR: ls_dd09l-as4user,
           ls_dd09l-as4date,
           ls_dd09l-as4time.

    LOOP AT lt_dd12v ASSIGNING <ls_dd12v>.
      CLEAR: <ls_dd12v>-as4user,
             <ls_dd12v>-as4date,
             <ls_dd12v>-as4time.
    ENDLOOP.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_dd02v ).
    lo_xml->structure_add( ls_dd09l ).
    lo_xml->table_add( it_table = lt_dd03p
                       iv_name  = 'DD03P_TABLE' ).
    lo_xml->table_add( it_table = lt_dd05m
                       iv_name  = 'DD05M_TABLE' ).
    lo_xml->table_add( it_table = lt_dd08v
                       iv_name  = 'DD08V_TABLE' ).
    lo_xml->table_add( lt_dd12v ).
    lo_xml->table_add( lt_dd17v ).
    lo_xml->table_add( it_table = lt_dd35v
                       iv_name  = 'DD35V_TALE' ).
    lo_xml->table_add( lt_dd36m ).

    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.                    "serialize

  METHOD deserialize.

    DATA: lv_name      TYPE ddobjname,
          lv_tname     TYPE trobj_name,
          lo_xml       TYPE REF TO lcl_xml,
          ls_dd02v     TYPE dd02v,
          ls_dd09l     TYPE dd09l,
          lt_dd03p     TYPE TABLE OF dd03p,
          lt_dd05m     TYPE TABLE OF dd05m,
          lt_dd08v     TYPE TABLE OF dd08v,
          lt_dd12v     TYPE dd12vtab,
          lt_dd17v     TYPE dd17vtab,
          ls_dd17v     LIKE LINE OF lt_dd17v,
          lt_secondary LIKE lt_dd17v,
          lt_dd35v     TYPE TABLE OF dd35v,
          lt_dd36m     TYPE dd36mttyp,
          ls_dd12v     LIKE LINE OF lt_dd12v.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->structure_read( CHANGING cg_structure = ls_dd02v ).
    lo_xml->structure_read( CHANGING cg_structure = ls_dd09l ).

    lo_xml->table_read( EXPORTING iv_name  = 'DD03P_TABLE'
                        CHANGING ct_table = lt_dd03p ).
    lo_xml->table_read( EXPORTING iv_name  = 'DD05M_TABLE'
                        CHANGING ct_table = lt_dd05m ).
    lo_xml->table_read( EXPORTING iv_name  = 'DD08V_TABLE'
                        CHANGING ct_table = lt_dd08v ).
    lo_xml->table_read( CHANGING ct_table = lt_dd12v ).
    lo_xml->table_read( CHANGING ct_table = lt_dd17v ).
    lo_xml->table_read( EXPORTING iv_name  = 'DD35V_TALE'
                        CHANGING ct_table = lt_dd35v ).
    lo_xml->table_read( CHANGING ct_table = lt_dd36m ).

    corr_insert( is_item    = is_item
                 iv_package = iv_package ).

    lv_name = is_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_TABL_PUT'
      EXPORTING
        name              = lv_name
        dd02v_wa          = ls_dd02v
        dd09l_wa          = ls_dd09l
      TABLES
        dd03p_tab         = lt_dd03p
        dd05m_tab         = lt_dd05m
        dd08v_tab         = lt_dd08v
        dd35v_tab         = lt_dd35v
        dd36m_tab         = lt_dd36m
      EXCEPTIONS
        tabl_not_found    = 1
        name_inconsistent = 2
        tabl_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_TABL_PUT'.
    ENDIF.

    activation_add( iv_type = is_item-obj_type
                    iv_name = is_item-obj_name ).

* handle indexes
    LOOP AT lt_dd12v INTO ls_dd12v.

* todo, call corr_insert?

      CLEAR lt_secondary.
      LOOP AT lt_dd17v INTO ls_dd17v
          WHERE sqltab = ls_dd12v-sqltab AND indexname = ls_dd12v-indexname.
        APPEND ls_dd17v TO lt_secondary.
      ENDLOOP.

      CALL FUNCTION 'DDIF_INDX_PUT'
        EXPORTING
          name              = ls_dd12v-sqltab
          id                = ls_dd12v-indexname
          dd12v_wa          = ls_dd12v
        TABLES
          dd17v_tab         = lt_secondary
        EXCEPTIONS
          indx_not_found    = 1
          name_inconsistent = 2
          indx_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.
      IF sy-subrc <> 0.
        _raise 'error from DDIF_INDX_PUT'.
      ENDIF.

      CALL FUNCTION 'DD_DD_TO_E071'
        EXPORTING
          type     = 'INDX'
          name     = ls_dd12v-sqltab
          id       = ls_dd12v-indexname
        IMPORTING
          obj_name = lv_tname.

      activation_add( iv_type = 'INDX'
                      iv_name = lv_tname ).

    ENDLOOP.

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_TABL IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_enqu DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enqu DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_enqu IMPLEMENTATION.

  METHOD jump.

    jump_se11( is_item  = is_item
               iv_radio = 'RSRD1-ENQU'
               iv_field = 'RSRD1-ENQU_VAL' ).

  ENDMETHOD.                    "jump

  METHOD delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = is_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'L'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_DD_DELETE_OBJ, ENQU'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD serialize.

    DATA: lv_name  TYPE ddobjname,
          ls_file  TYPE st_file,
          lo_xml   TYPE REF TO lcl_xml,
          ls_dd25v TYPE dd25v,
          lt_dd26e TYPE TABLE OF dd26e,
          lt_dd27p TYPE TABLE OF dd27p.


    lv_name = is_item-obj_name.

    CALL FUNCTION 'DDIF_ENQU_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = gc_english
      IMPORTING
        dd25v_wa      = ls_dd25v
      TABLES
        dd26e_tab     = lt_dd26e
        dd27p_tab     = lt_dd27p
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_ENQU_GET'.
    ENDIF.
    IF ls_dd25v IS INITIAL.
      RETURN. " does not exist in system
    ENDIF.

    CLEAR: ls_dd25v-as4user,
           ls_dd25v-as4date,
           ls_dd25v-as4time.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_dd25v ).
    lo_xml->table_add( it_table = lt_dd26e
                       iv_name = 'DD26E_TABLE').
    lo_xml->table_add( it_table = lt_dd27p
                       iv_name = 'DD27P_TABLE' ).

    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.                    "serialize

  METHOD deserialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          lt_dd26e TYPE TABLE OF dd26e,
          lt_dd27p TYPE TABLE OF dd27p.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->structure_read( CHANGING cg_structure = ls_dd25v ).
    lo_xml->table_read( EXPORTING iv_name = 'DD26E_TABLE' CHANGING ct_table = lt_dd26e ).
    lo_xml->table_read( EXPORTING iv_name = 'DD27P_TABLE' CHANGING ct_table = lt_dd27p ).

    corr_insert( is_item    = is_item
                 iv_package = iv_package ).

    lv_name = is_item-obj_name.

    CALL FUNCTION 'DDIF_ENQU_PUT'
      EXPORTING
        name              = lv_name
        dd25v_wa          = ls_dd25v
      TABLES
        dd26e_tab         = lt_dd26e
        dd27p_tab         = lt_dd27p
      EXCEPTIONS
        enqu_not_found    = 1
        name_inconsistent = 2
        enqu_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_ENQU_PUT'.
    ENDIF.

    activation_add( iv_type = is_item-obj_type
                    iv_name = is_item-obj_name ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_enqu IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_shlp DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_shlp DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_shlp IMPLEMENTATION.

  METHOD jump.

    jump_se11( is_item  = is_item
               iv_radio = 'RSRD1-SHMA'
               iv_field = 'RSRD1-SHMA_VAL' ).

  ENDMETHOD.                    "jump

  METHOD delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = is_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'H'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_DD_DELETE_OBJ, SHLP'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD serialize.

    DATA: lv_name  TYPE ddobjname,
          ls_file  TYPE st_file,
          lo_xml   TYPE REF TO lcl_xml,
          ls_dd30v TYPE dd30v,
          lt_dd31v TYPE TABLE OF dd31v,
          lt_dd32p TYPE TABLE OF dd32p,
          lt_dd33v TYPE TABLE OF dd33v.


    lv_name = is_item-obj_name.

    CALL FUNCTION 'DDIF_SHLP_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = gc_english
      IMPORTING
        dd30v_wa      = ls_dd30v
      TABLES
        dd31v_tab     = lt_dd31v
        dd32p_tab     = lt_dd32p
        dd33v_tab     = lt_dd33v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_SHLP_GET'.
    ENDIF.
    IF ls_dd30v IS INITIAL.
      RETURN. " does not exist in system
    ENDIF.

    CLEAR: ls_dd30v-as4user,
           ls_dd30v-as4date,
           ls_dd30v-as4time.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_dd30v ).
    lo_xml->table_add( it_table = lt_dd31v
                       iv_name  = 'DD31V_TABLE' ).
    lo_xml->table_add( it_table = lt_dd32p
                       iv_name  = 'DD32P_TABLE' ).
    lo_xml->table_add( it_table = lt_dd33v
                       iv_name  = 'DD33V_TABLE' ).

    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.                    "serialize

  METHOD deserialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          lv_name  TYPE ddobjname,
          ls_dd30v TYPE dd30v,
          lt_dd31v TYPE TABLE OF dd31v,
          lt_dd32p TYPE TABLE OF dd32p,
          lt_dd33v TYPE TABLE OF dd33v.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->structure_read( CHANGING cg_structure = ls_dd30v ).
    lo_xml->table_read( EXPORTING iv_name = 'DD31V_TABLE'
                        CHANGING ct_table = lt_dd31v ).
    lo_xml->table_read( EXPORTING iv_name = 'DD32P_TABLE'
                        CHANGING ct_table = lt_dd32p ).
    lo_xml->table_read( EXPORTING iv_name = 'DD33V_TABLE'
                        CHANGING ct_table = lt_dd33v ).

    corr_insert( is_item    = is_item
                 iv_package = iv_package ).

    lv_name = is_item-obj_name.

    CALL FUNCTION 'DDIF_SHLP_PUT'
      EXPORTING
        name              = lv_name
        dd30v_wa          = ls_dd30v
      TABLES
        dd31v_tab         = lt_dd31v
        dd32p_tab         = lt_dd32p
        dd33v_tab         = lt_dd33v
      EXCEPTIONS
        shlp_not_found    = 1
        name_inconsistent = 2
        shlp_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_SHLP_PUT'.
    ENDIF.

    activation_add( iv_type = is_item-obj_type
                    iv_name = is_item-obj_name ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_shlp IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_TRAN DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_tran DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_TRAN DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_msag IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_tran IMPLEMENTATION.

  METHOD jump.

    DATA: lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.


    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPLSEUK'.
    <ls_bdcdata>-dynpro   = '0390'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'TSTC-TCODE'.
    <ls_bdcdata>-fval = is_item-obj_name.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      STARTING NEW TASK 'GIT'
      EXPORTING
        tcode                 = 'SE93'
        mode_val              = 'E'
      TABLES
        using_tab             = lt_bdcdata
      EXCEPTIONS
        system_failure        = 1
        communication_failure = 2
        resource_failure      = 3
        OTHERS                = 4 ##FM_SUBRC_OK. "#EC CI_SUBRC

  ENDMETHOD.                    "jump

  METHOD delete.

    DATA: lv_transaction TYPE tstc-tcode.


    lv_transaction = is_item-obj_name.

    CALL FUNCTION 'RPY_TRANSACTION_DELETE'
      EXPORTING
        transaction      = lv_transaction
      EXCEPTIONS
        not_excecuted    = 1
        object_not_found = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      _raise 'Error from RPY_TRANSACTION_DELETE'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD deserialize.

    CONSTANTS: lc_hex_tra TYPE x VALUE '00',
*               c_hex_men TYPE x VALUE '01',
               lc_hex_par TYPE x VALUE '02',
               lc_hex_rep TYPE x VALUE '80'.
*               c_hex_rpv TYPE x VALUE '10',
*               c_hex_obj TYPE x VALUE '08',
*               c_hex_chk TYPE x VALUE '04',
*               c_hex_enq TYPE x VALUE '20'.

    DATA: lv_dynpro TYPE d020s-dnum,
          lo_xml    TYPE REF TO lcl_xml,
          ls_tstc   TYPE tstc,
          lv_type   TYPE rglif-docutype,
          ls_tstct  TYPE tstct,
          ls_tstcc  TYPE tstcc.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->structure_read( CHANGING cg_structure = ls_tstc ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tstcc ).
    lo_xml->structure_read( CHANGING cg_structure = ls_tstct ).

    lv_dynpro = ls_tstc-dypno.

    CASE ls_tstc-cinfo.
      WHEN lc_hex_tra.
        lv_type = ststc_c_type_dialog.
      WHEN lc_hex_rep.
        lv_type = ststc_c_type_report.
      WHEN lc_hex_par.
        lv_type = ststc_c_type_parameters.
* todo, or ststc_c_type_variant?
      WHEN OTHERS.
        _raise 'Transaction, unknown CINFO'.
    ENDCASE.

    CALL FUNCTION 'RPY_TRANSACTION_INSERT'
      EXPORTING
        transaction         = ls_tstc-tcode
        program             = ls_tstc-pgmna
        dynpro              = lv_dynpro
        language            = gc_english
        development_class   = iv_package
        transaction_type    = lv_type
        shorttext           = ls_tstct-ttext
        html_enabled        = ls_tstcc-s_webgui
        java_enabled        = ls_tstcc-s_platin
        wingui_enabled      = ls_tstcc-s_win32
      EXCEPTIONS
        cancelled           = 1
        already_exist       = 2
        permission_error    = 3
        name_not_allowed    = 4
        name_conflict       = 5
        illegal_type        = 6
        object_inconsistent = 7
        db_access_error     = 8
        OTHERS              = 9.
    IF sy-subrc <> 0.
      _raise 'Error from RPY_TRANSACTION_INSERT'.
    ENDIF.

  ENDMETHOD.                    "deserialize

  METHOD serialize.

    DATA: lv_transaction TYPE tstc-tcode,
          lt_tcodes      TYPE TABLE OF tstc,
          ls_tcode       LIKE LINE OF lt_tcodes,
          ls_tstct       TYPE tstct,
          lt_gui_attr    TYPE TABLE OF tstcc,
          lo_xml         TYPE REF TO lcl_xml,
          ls_file        TYPE st_file,
          ls_gui_attr    LIKE LINE OF lt_gui_attr.


    lv_transaction = is_item-obj_name.

    CALL FUNCTION 'RPY_TRANSACTION_READ'
      EXPORTING
        transaction      = lv_transaction
      TABLES
        tcodes           = lt_tcodes
        gui_attributes   = lt_gui_attr
      EXCEPTIONS
        permission_error = 1
        cancelled        = 2
        not_found        = 3
        object_not_found = 4
        OTHERS           = 5.
    IF sy-subrc = 4 OR sy-subrc = 3.
      RETURN.
    ENDIF.
    IF sy-subrc <> 0.
      _raise 'Error from RPY_TRANSACTION_READ'.
    ENDIF.

    SELECT SINGLE * FROM tstct INTO ls_tstct
      WHERE sprsl = gc_english
      AND tcode = lv_transaction.                       "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'Transaction description not found'.
    ENDIF.

    READ TABLE lt_tcodes INDEX 1 INTO ls_tcode.
    ASSERT sy-subrc = 0.
    READ TABLE lt_gui_attr INDEX 1 INTO ls_gui_attr.
    ASSERT sy-subrc = 0.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_tcode ).
    lo_xml->structure_add( ls_gui_attr ).
    lo_xml->structure_add( ls_tstct ).

    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.                    "serialize

ENDCLASS.                    "lcl_object_msag IMPLEMENTATION

CLASS lcl_object_tobj DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_object_tobj IMPLEMENTATION.

  METHOD serialize.

    DATA: ls_objh  TYPE objh,
          ls_objt  TYPE objt,
          lt_objs  TYPE tt_objs,
          lt_objsl TYPE tt_objsl,
          lt_objm  TYPE tt_objm,
          lo_xml   TYPE REF TO lcl_xml,
          ls_file  TYPE st_file.


    ls_objh-objectname = is_item-obj_name(10).
    ls_objh-objecttype = is_item-obj_name+10.

    CALL FUNCTION 'CTO_OBJECT_GET'
      EXPORTING
        iv_objectname      = ls_objh-objectname
        iv_objecttype      = ls_objh-objecttype
        iv_language        = gc_english
        iv_sel_objt        = abap_true
        iv_sel_objs        = abap_true
        iv_sel_objsl       = abap_true
        iv_sel_objm        = abap_true
      IMPORTING
        es_objh            = ls_objh
        es_objt            = ls_objt
*       EV_OBJT_DOESNT_EXIST       =
      TABLES
        tt_objs            = lt_objs
        tt_objsl           = lt_objsl
        tt_objm            = lt_objm
      EXCEPTIONS
        object_not_defined = 1
        OTHERS             = 2.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      _raise 'error from CTO_OBJECT_GET'.
    ENDIF.

    CLEAR: ls_objh-luser,
           ls_objh-ldate.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_objh ).
    lo_xml->structure_add( ls_objt ).
    lo_xml->table_add( lt_objs ).
    lo_xml->table_add( lt_objsl ).
    lo_xml->table_add( lt_objm ).

    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.

  METHOD deserialize.

    DATA: ls_objh  TYPE objh,
          ls_objt  TYPE objt,
          lt_objs  TYPE tt_objs,
          lt_objsl TYPE tt_objsl,
          lt_objm  TYPE tt_objm,
          lo_xml   TYPE REF TO lcl_xml.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).
    lo_xml->structure_read( CHANGING cg_structure = ls_objh ).
    lo_xml->structure_read( CHANGING cg_structure = ls_objt ).
    lo_xml->table_read( CHANGING ct_table = lt_objs ).
    lo_xml->table_read( CHANGING ct_table = lt_objsl ).
    lo_xml->table_read( CHANGING ct_table = lt_objm ).

    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_objectname         = ls_objh-objectname
        iv_objecttype         = ls_objh-objecttype
        iv_maint_mode         = 'I'
        iv_objecttext         = ls_objt-ddtext
        iv_objcateg           = ls_objh-objcateg
        iv_objtransp          = ls_objh-objtransp
        iv_devclass           = iv_package
      TABLES
        tt_v_obj_s            = lt_objs
        tt_objm               = lt_objm
      EXCEPTIONS
        illegal_call          = 1
        object_not_found      = 2
        generate_error        = 3
        transport_error       = 4
        object_enqueue_failed = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
* todo, TOBJ has to be saved/generated after the DDIC tables have been activated
      _raise 'error from OBJ_GENERATE'.
    ENDIF.

  ENDMETHOD.

  METHOD delete.

    DATA: ls_objh TYPE objh.


    ls_objh-objectname = is_item-obj_name(10).
    ls_objh-objecttype = is_item-obj_name+10.

    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_objectname         = ls_objh-objectname
        iv_objecttype         = ls_objh-objecttype
        iv_maint_mode         = 'D'
      EXCEPTIONS
        illegal_call          = 1
        object_not_found      = 2
        generate_error        = 3
        transport_error       = 4
        object_enqueue_failed = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      _raise 'error from OBJ_GENERATE'.
    ENDIF.

  ENDMETHOD.

  METHOD jump.
    _raise 'todo, TOBJ jump'.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_object_msag DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_msag DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_msag DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_msag IMPLEMENTATION.

  METHOD jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = is_item-obj_name
        object_type   = 'MSAG'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD delete.

* parameter SUPPRESS_DIALOG doesnt exist in all versions
    CALL FUNCTION 'RS_DELETE_MESSAGE_ID'
      EXPORTING
        nachrichtenklasse = is_item-obj_name
      EXCEPTIONS
        not_executed      = 1
        not_found         = 2
        no_permission     = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      _raise 'Error from RS_DELETE_MESSAGE_ID'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD deserialize.
* fm RPY_MESSAGE_ID_INSERT almost works, but not in older versions

    DATA: lo_xml   TYPE REF TO lcl_xml,
          ls_t100a TYPE t100a,
          ls_t100t TYPE t100t,
          ls_t100u TYPE t100u,
          lt_t100  TYPE TABLE OF t100.

    FIELD-SYMBOLS: <ls_t100> LIKE LINE OF lt_t100.

    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->structure_read( CHANGING cg_structure = ls_t100a ).
    lo_xml->table_read( EXPORTING iv_name = 'T100'
                        CHANGING ct_table = lt_t100 ).

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        global_lock         = abap_true
        devclass            = iv_package
        object              = ls_t100a-arbgb
        object_class        = 'T100'
      EXCEPTIONS
        cancelled           = 01
        permission_failure  = 02
        unknown_objectclass = 03.
    IF sy-subrc <> 0.
      _raise 'Error from RS_CORR_INSERT'.
    ENDIF.

    LOOP AT lt_t100 ASSIGNING <ls_t100>.
      MODIFY t100 FROM <ls_t100>.                         "#EC CI_SUBRC
      ASSERT sy-subrc = 0.

      CLEAR ls_t100u.
      MOVE-CORRESPONDING <ls_t100> TO ls_t100u ##ENH_OK.
      ls_t100u-name    = sy-uname.
      ls_t100u-datum   = sy-datum.
      ls_t100u-selfdef = '3'.
      MODIFY t100u FROM ls_t100u.                         "#EC CI_SUBRC
      ASSERT sy-subrc = 0.
    ENDLOOP.

    ls_t100a-masterlang = gc_english.
    ls_t100a-lastuser = sy-uname.
    ls_t100a-respuser = sy-uname.
    ls_t100a-ldate = sy-datum.
    ls_t100a-ltime = sy-uzeit.
    MODIFY t100a FROM ls_t100a.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    ls_t100t-sprsl = gc_english.
    ls_t100t-arbgb = ls_t100a-arbgb.
    ls_t100t-stext = ls_t100a-stext.
    MODIFY t100t FROM ls_t100t.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.                    "deserialize

  METHOD serialize.

    DATA: lv_msg_id TYPE rglif-message_id,
          ls_file   TYPE st_file,
          ls_inf    TYPE t100a,
          lt_source TYPE TABLE OF t100,
          lo_xml    TYPE REF TO lcl_xml.


    lv_msg_id = is_item-obj_name.

    CALL FUNCTION 'RPY_MESSAGE_ID_READ'
      EXPORTING
        language         = gc_english
        message_id       = lv_msg_id
      IMPORTING
        message_id_inf   = ls_inf
      TABLES
        source           = lt_source
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4.
    IF sy-subrc = 2.
      RETURN.
    ENDIF.
    IF sy-subrc <> 0.
      _raise 'Error from RPY_MESSAGE_ID_READ'.
    ENDIF.

    CLEAR: ls_inf-lastuser,
           ls_inf-ldate,
           ls_inf-ltime.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_inf ).
    lo_xml->table_add( it_table = lt_source
                       iv_name  = 'T100' ).

    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.                    "serialize

ENDCLASS.                    "lcl_object_view IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_fugr DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_fugr DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS main_name
      IMPORTING is_item           TYPE st_item
      RETURNING VALUE(rv_program) TYPE program
      RAISING   lcx_exception.

    CLASS-METHODS functions
      IMPORTING is_item           TYPE st_item
      RETURNING VALUE(rt_functab) TYPE tt_rs38l_incl
      RAISING   lcx_exception.

    CLASS-METHODS includes
      IMPORTING is_item            TYPE st_item
      RETURNING VALUE(rt_includes) TYPE rso_t_objnm
      RAISING   lcx_exception.

    CLASS-METHODS serialize_functions
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize_functions
      IMPORTING is_item  TYPE st_item
                it_files TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS serialize_xml
      IMPORTING is_item        TYPE st_item
      RETURNING VALUE(rs_file) TYPE st_file
      RAISING   lcx_exception.

    CLASS-METHODS deserialize_xml
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS serialize_includes
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize_includes
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_fugr IMPLEMENTATION.

* function group SEUF
* function group SIFP
* function group SUNI

  METHOD deserialize_functions.

    DATA: lv_include       TYPE rs38l-include,
          lv_area          TYPE rs38l-area,
          lo_xml           TYPE REF TO lcl_xml,
          lt_functab       TYPE tt_rs38l_incl,
          lt_import        TYPE TABLE OF rsimp,
          lt_changing      TYPE TABLE OF rscha,
          lt_export        TYPE TABLE OF rsexp,
          lt_tables        TYPE TABLE OF rstbl,
          lt_exception     TYPE TABLE OF rsexc,
          lt_documentation TYPE TABLE OF rsfdo,
          lt_source        TYPE TABLE OF rssource,
          lv_global_flag   TYPE rs38l-global,
          lv_remote_call   TYPE rs38l-remote,
          lv_update_task   TYPE rs38l-utask,
          lv_short_text    TYPE tftit-stext,
          lv_remote_basxml TYPE rs38l-basxml_enabled.

    FIELD-SYMBOLS: <ls_functab> LIKE LINE OF lt_functab.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->table_read( CHANGING  ct_table = lt_functab ).

    LOOP AT lt_functab ASSIGNING <ls_functab>.

      read_abap( EXPORTING is_item  = is_item
                           iv_extra = <ls_functab>-funcname
                           it_files = it_files
                 CHANGING ct_abap = lt_source ).

      lo_xml = read_xml( is_item  = is_item
                         iv_extra = <ls_functab>-funcname
                         it_files = it_files ).

      lo_xml->element_read( CHANGING cg_element = lv_global_flag ).
      lo_xml->element_read( CHANGING cg_element = lv_remote_call ).
      lo_xml->element_read( CHANGING cg_element = lv_update_task ).
      lo_xml->element_read( CHANGING cg_element = lv_short_text ).
      lo_xml->element_read( CHANGING cg_element = lv_remote_basxml ).

      lo_xml->table_read( EXPORTING iv_name = 'IMPORT'
                          CHANGING ct_table = lt_import ).
      lo_xml->table_read( EXPORTING iv_name = 'CHANGING'
                          CHANGING ct_table = lt_changing ).
      lo_xml->table_read( EXPORTING iv_name = 'EXPORT'
                          CHANGING ct_table = lt_export ).
      lo_xml->table_read( EXPORTING iv_name = 'TABLES'
                          CHANGING ct_table = lt_tables ).
      lo_xml->table_read( EXPORTING iv_name = 'EXCEPTION'
                          CHANGING ct_table = lt_exception ).
      lo_xml->table_read( EXPORTING iv_name = 'DOCUMENTATION'
                          CHANGING ct_table = lt_documentation ).

      lv_area = is_item-obj_name.

      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname           = <ls_functab>-funcname
        IMPORTING
          include            = lv_include
        EXCEPTIONS
          function_not_exist = 1.
      IF sy-subrc = 0.
* delete the function module to make sure the parameters are updated
* havent found a nice way to update the paramters
        CALL FUNCTION 'FUNCTION_DELETE'
          EXPORTING
            funcname                 = <ls_functab>-funcname
*           SUPPRESS_DELETE_LONGTEXT = ' '
            suppress_success_message = abap_true
*           SUPPRESS_DELE_ENHA       = ' '
*           SUPPRESS_LOCK            = ' '
          EXCEPTIONS
            error_message            = 1
            OTHERS                   = 2.
        IF sy-subrc <> 0.
          _raise 'error from FUNCTION_DELETE'.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'RS_FUNCTIONMODULE_INSERT'
        EXPORTING
          funcname                = <ls_functab>-funcname
          function_pool           = lv_area
          interface_global        = lv_global_flag
          remote_call             = lv_remote_call
          short_text              = lv_short_text
*         NAMESPACE               = ' ' todo
          remote_basxml_supported = lv_remote_basxml
        IMPORTING
          function_include        = lv_include
        TABLES
          import_parameter        = lt_import
          export_parameter        = lt_export
          tables_parameter        = lt_tables
          changing_parameter      = lt_changing
          exception_list          = lt_exception
          parameter_docu          = lt_documentation
        EXCEPTIONS
          double_task             = 1
          error_message           = 2
          function_already_exists = 3
          invalid_function_pool   = 4
          invalid_name            = 5
          too_many_functions      = 6
          no_modify_permission    = 7
          no_show_permission      = 8
          enqueue_system_failure  = 9
          canceled_in_corr        = 10
          OTHERS                  = 11.
      IF sy-subrc <> 0.
        _raise 'error from RS_FUNCTIONMODULE_INSERT'.
      ENDIF.

      INSERT REPORT lv_include FROM lt_source.

      activation_add( iv_type = 'FUNC'
                      iv_name = <ls_functab>-funcname ).

    ENDLOOP.

  ENDMETHOD.                    "deserialize_functions

  METHOD deserialize_includes.

    DATA: lo_xml      TYPE REF TO lcl_xml,
          ls_progdir  TYPE ty_progdir,
          lt_includes TYPE rso_t_objnm,
          lt_tpool    TYPE textpool_table,
          lt_source   TYPE TABLE OF abaptxt255.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).
    lo_xml->table_read( CHANGING ct_table = lt_includes ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

      read_abap( EXPORTING is_item  = is_item
                           iv_extra = <lv_include>
                           it_files = it_files
                 CHANGING ct_abap = lt_source ).

      lo_xml = read_xml( is_item  = is_item
                         iv_extra = <lv_include>
                         it_files = it_files ).

      lo_xml->structure_read( EXPORTING iv_name     = 'PROGDIR'
                              CHANGING cg_structure = ls_progdir ).

      lo_xml->table_read( CHANGING ct_table = lt_tpool ).

      deserialize_program( is_progdir = ls_progdir
                           it_source  = lt_source
                           it_tpool   = lt_tpool
                           iv_package = iv_package ).

    ENDLOOP.

  ENDMETHOD.                    "deserialize_includes

  METHOD deserialize_xml.

    DATA: lv_complete  TYPE rs38l-area,
          lo_xml       TYPE REF TO lcl_xml,
          lv_namespace TYPE rs38l-namespace,
          lv_areat     TYPE tlibt-areat,
          lv_stext     TYPE tftit-stext,
          lv_group     TYPE rs38l-area.


    lv_complete = is_item-obj_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area                = lv_complete
      IMPORTING
        namespace                    = lv_namespace
        group                        = lv_group
      EXCEPTIONS
        include_not_exists           = 1
        group_not_exists             = 2
        no_selections                = 3
        no_function_include          = 4
        no_function_pool             = 5
        delimiter_wrong_position     = 6
        no_customer_function_group   = 7
        no_customer_function_include = 8
        reserved_name_customer       = 9
        namespace_too_long           = 10
        area_length_error            = 11
        OTHERS                       = 12.
    IF sy-subrc <> 0.
      _raise 'error from FUNCTION_INCLUDE_SPLIT'.
    ENDIF.

    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).
    lo_xml->element_read( CHANGING cg_element = lv_areat ).
    lv_stext = lv_areat.

    CALL FUNCTION 'RS_FUNCTION_POOL_INSERT'
      EXPORTING
        function_pool           = lv_group
        short_text              = lv_stext
        namespace               = lv_namespace
        devclass                = iv_package
      EXCEPTIONS
        name_already_exists     = 1
        name_not_correct        = 2
        function_already_exists = 3
        invalid_function_pool   = 4
        invalid_name            = 5
        too_many_functions      = 6
        no_modify_permission    = 7
        no_show_permission      = 8
        enqueue_system_failure  = 9
        canceled_in_corr        = 10
        undefined_error         = 11
        OTHERS                  = 12.
    IF sy-subrc <> 0 AND sy-subrc <> 1 AND sy-subrc <> 3.
* todo, change description
      _raise 'error from RS_FUNCTION_POOL_INSERT'.
    ENDIF.

  ENDMETHOD.                    "deserialize_xml

  METHOD serialize_xml.

    DATA: lo_xml      TYPE REF TO lcl_xml,
          lt_functab  TYPE tt_rs38l_incl,
          lt_includes TYPE rso_t_objnm,
          lv_areat    TYPE tlibt-areat.


    SELECT SINGLE areat INTO lv_areat
      FROM tlibt
      WHERE spras = gc_english
      AND area = is_item-obj_name.                      "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'not found in TLIBT'.
    ENDIF.

    lt_functab = functions( is_item ).
    lt_includes = includes( is_item ).

* todo, dynpros

    CREATE OBJECT lo_xml.
    lo_xml->element_add( lv_areat ).
    lo_xml->table_add( it_table = lt_functab ).
    lo_xml->table_add( it_table = lt_includes ).

    rs_file = xml_to_file( is_item  = is_item
                           io_xml   = lo_xml ).

  ENDMETHOD.                    "serialize_xml

  METHOD includes.

    DATA: lv_program TYPE program,
          lv_cnam    TYPE reposrc-cnam,
          lt_functab TYPE tt_rs38l_incl.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF rt_includes,
                   <ls_func>    LIKE LINE OF lt_functab.


    lv_program = main_name( is_item ).
    lt_functab = functions( is_item ).

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = lv_program
*       WITH_RESERVED_INCLUDES =
*       WITH_CLASS_INCLUDES    = ' ' hmm, todo
      TABLES
        includetab   = rt_includes
      EXCEPTIONS
        not_existent = 1
        no_program   = 2
        OTHERS       = 3.
    IF sy-subrc <> 0.
      _raise 'Error from RS_GET_ALL_INCLUDES'.
    ENDIF.

    LOOP AT lt_functab ASSIGNING <ls_func>.
      DELETE TABLE rt_includes FROM <ls_func>-include.
    ENDLOOP.

* skip SAP standard includes
    LOOP AT rt_includes ASSIGNING <lv_include>.
      SELECT SINGLE cnam FROM reposrc INTO lv_cnam
        WHERE progname = <lv_include>
        AND r3state = 'A'
        AND cnam = 'SAP'.
      IF sy-subrc = 0.
        DELETE rt_includes INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    APPEND lv_program TO rt_includes.

  ENDMETHOD.                    "includes

  METHOD functions.

    DATA: lv_area TYPE rs38l-area.


    lv_area = is_item-obj_name.

    CALL FUNCTION 'RS_FUNCTION_POOL_CONTENTS'
      EXPORTING
        function_pool           = lv_area
      TABLES
        functab                 = rt_functab
      EXCEPTIONS
        function_pool_not_found = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      _raise 'Error from RS_FUNCTION_POOL_CONTENTS'.
    ENDIF.

  ENDMETHOD.                    "functions

  METHOD main_name.

    DATA: lv_area      TYPE rs38l-area,
          lv_namespace TYPE rs38l-namespace,
          lv_group     TYPE rs38l-area.


    lv_area = is_item-obj_name.

    CALL FUNCTION 'FUNCTION_INCLUDE_SPLIT'
      EXPORTING
        complete_area                = lv_area
      IMPORTING
        namespace                    = lv_namespace
        group                        = lv_group
      EXCEPTIONS
        include_not_exists           = 1
        group_not_exists             = 2
        no_selections                = 3
        no_function_include          = 4
        no_function_pool             = 5
        delimiter_wrong_position     = 6
        no_customer_function_group   = 7
        no_customer_function_include = 8
        reserved_name_customer       = 9
        namespace_too_long           = 10
        area_length_error            = 11
        OTHERS                       = 12.
    IF sy-subrc <> 0.
      _raise 'Error from FUNCTION_INCLUDE_SPLIT'.
    ENDIF.

    CONCATENATE lv_namespace 'SAPL' lv_group INTO rv_program.

  ENDMETHOD.                    "main_name

  METHOD serialize_functions.

    DATA: lt_import        TYPE TABLE OF rsimp,
          lo_xml           TYPE REF TO lcl_xml,
          ls_file          TYPE st_file,
          lt_changing      TYPE TABLE OF rscha,
          lt_export        TYPE TABLE OF rsexp,
          lt_tables        TYPE TABLE OF rstbl,
          lt_exception     TYPE TABLE OF rsexc,
          lt_documentation TYPE TABLE OF rsfdo,
          lt_source        TYPE TABLE OF rssource,
          lv_global_flag   TYPE rs38l-global,
          lv_remote_call   TYPE rs38l-remote,
          lv_update_task   TYPE rs38l-utask,
          lv_short_text    TYPE tftit-stext,
          lt_functab       TYPE tt_rs38l_incl,
          lt_new_source    TYPE rsfb_source,
          lv_remote_basxml TYPE rs38l-basxml_enabled.

    FIELD-SYMBOLS: <ls_func> LIKE LINE OF lt_functab.


    lt_functab = functions( is_item ).

    LOOP AT lt_functab ASSIGNING <ls_func>.
* fm RPY_FUNCTIONMODULE_READ does not support source code
* lines longer than 72 characters
      CALL FUNCTION 'RPY_FUNCTIONMODULE_READ_NEW'
        EXPORTING
          functionname            = <ls_func>-funcname
        IMPORTING
          global_flag             = lv_global_flag
          remote_call             = lv_remote_call
          update_task             = lv_update_task
          short_text              = lv_short_text
          remote_basxml_supported = lv_remote_basxml
        TABLES
          import_parameter        = lt_import
          changing_parameter      = lt_changing
          export_parameter        = lt_export
          tables_parameter        = lt_tables
          exception_list          = lt_exception
          documentation           = lt_documentation
          source                  = lt_source
        CHANGING
          new_source              = lt_new_source
        EXCEPTIONS
          error_message           = 1
          function_not_found      = 2
          invalid_name            = 3
          OTHERS                  = 4.
      IF sy-subrc <> 0.
        _raise 'Error from RPY_FUNCTIONMODULE_READ_NEW'.
      ENDIF.

      CREATE OBJECT lo_xml.
      lo_xml->element_add( lv_global_flag ).
      lo_xml->element_add( lv_remote_call ).
      lo_xml->element_add( lv_update_task ).
      lo_xml->element_add( lv_short_text ).
      lo_xml->element_add( lv_remote_basxml ).

      lo_xml->table_add( it_table = lt_import
                         iv_name  = 'IMPORT' ).
      lo_xml->table_add( it_table = lt_changing
                         iv_name  = 'CHANGING' ).
      lo_xml->table_add( it_table = lt_export
                         iv_name  = 'EXPORT' ).
      lo_xml->table_add( it_table = lt_tables
                         iv_name  = 'TABLES' ).
      lo_xml->table_add( it_table = lt_exception
                         iv_name  = 'EXCEPTION' ).
      lo_xml->table_add( it_table = lt_documentation
                         iv_name  = 'DOCUMENTATION' ).

      ls_file = xml_to_file( is_item  = is_item
                             iv_extra = <ls_func>-funcname
                             io_xml   = lo_xml ).
      APPEND ls_file TO rt_files.

      IF NOT lt_new_source IS INITIAL.
        ls_file = abap_to_file( is_item  = is_item
                                iv_extra = <ls_func>-funcname
                                it_abap  = lt_new_source ).
      ELSE.
        ls_file = abap_to_file( is_item  = is_item
                                iv_extra = <ls_func>-funcname
                                it_abap  = lt_source ).
      ENDIF.
      APPEND ls_file TO rt_files.

    ENDLOOP.

  ENDMETHOD.                    "serialize_functions

  METHOD serialize_includes.

    DATA: lt_files    LIKE rt_files,
          lt_includes TYPE rso_t_objnm.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.


    lt_includes = includes( is_item ).

    LOOP AT lt_includes ASSIGNING <lv_include>.

* todo, filename is not correct, a include can be used in several programs
      lt_files = serialize_program( is_item    = is_item
                                    iv_program = <lv_include>
                                    iv_extra   = <lv_include> ).

      APPEND LINES OF lt_files TO rt_files.

    ENDLOOP.

  ENDMETHOD.                    "serialize_includes

  METHOD serialize.

    DATA: lv_pool  TYPE tlibg-area,
          lt_files LIKE rt_files,
          ls_file  LIKE LINE OF rt_files.


    lv_pool = is_item-obj_name.
    CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
      EXPORTING
        function_pool   = lv_pool
      EXCEPTIONS
        pool_not_exists = 1.
    IF sy-subrc = 1.
      RETURN.
    ENDIF.

    ls_file = serialize_xml( is_item ).
    APPEND ls_file TO rt_files.

    lt_files = serialize_functions( is_item ).
    APPEND LINES OF lt_files TO rt_files.

    lt_files = serialize_includes( is_item ).
    APPEND LINES OF lt_files TO rt_files.

  ENDMETHOD.                    "serialize

  METHOD deserialize.

    deserialize_xml( is_item    = is_item
                     it_files   = it_files
                     iv_package = iv_package ).

    deserialize_functions( is_item  = is_item
                           it_files = it_files ).

    deserialize_includes( is_item    = is_item
                          it_files   = it_files
                          iv_package = iv_package ).

  ENDMETHOD.                    "deserialize

  METHOD delete.

    DATA: lv_area TYPE rs38l-area.


    lv_area = is_item-obj_name.

    CALL FUNCTION 'RS_FUNCTION_POOL_DELETE'
      EXPORTING
        area                   = lv_area
        suppress_popups        = abap_true
        skip_progress_ind      = abap_true
      EXCEPTIONS
        canceled_in_corr       = 1
        enqueue_system_failure = 2
        function_exist         = 3
        not_executed           = 4
        no_modify_permission   = 5
        no_show_permission     = 6
        permission_failure     = 7
        pool_not_exist         = 8
        cancelled              = 9
        OTHERS                 = 10.
    IF sy-subrc <> 0.
      _raise 'error from RS_FUNCTION_POOL_DELETE'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = is_item-obj_name
        object_type   = 'FUGR'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_fugr IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_view DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_view IMPLEMENTATION.

  METHOD jump.

    jump_se11( is_item  = is_item
               iv_radio = 'RSRD1-VIMA'
               iv_field = 'RSRD1-VIMA_VAL' ).

  ENDMETHOD.                    "jump

  METHOD delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = is_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'V'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_DD_DELETE_OBJ, VIEW'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD serialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          ls_file  TYPE st_file,
          lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE TABLE OF dd26v,
          lt_dd27p TYPE TABLE OF dd27p,
          lt_dd28j TYPE TABLE OF dd28j,
          lt_dd28v TYPE TABLE OF dd28v.


    lv_name = is_item-obj_name.

    CALL FUNCTION 'DDIF_VIEW_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = gc_english
      IMPORTING
        dd25v_wa      = ls_dd25v
        dd09l_wa      = ls_dd09l
      TABLES
        dd26v_tab     = lt_dd26v
        dd27p_tab     = lt_dd27p
        dd28j_tab     = lt_dd28j
        dd28v_tab     = lt_dd28v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_VIEW_GET'.
    ENDIF.
    IF ls_dd25v IS INITIAL.
      RETURN. " does not exist in system
    ENDIF.

    CLEAR: ls_dd25v-as4user,
           ls_dd25v-as4date,
           ls_dd25v-as4time.

    CLEAR: ls_dd09l-as4user,
           ls_dd09l-as4date,
           ls_dd09l-as4time.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_dd25v ).
    lo_xml->structure_add( ls_dd09l ).

    lo_xml->table_add( it_table = lt_dd26v
                       iv_name = 'DD26V_TABLE' ).
    lo_xml->table_add( it_table = lt_dd27p
                       iv_name = 'DD27P_TABLE' ).
    lo_xml->table_add( it_table = lt_dd28j
                       iv_name = 'DD28J_TABLE' ).
    lo_xml->table_add( it_table = lt_dd28v
                       iv_name = 'DD28V_TABLE' ).

    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.                    "serialize

  METHOD deserialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          lv_name  TYPE ddobjname,
          ls_dd25v TYPE dd25v,
          ls_dd09l TYPE dd09l,
          lt_dd26v TYPE TABLE OF dd26v,
          lt_dd27p TYPE TABLE OF dd27p,
          lt_dd28j TYPE TABLE OF dd28j,
          lt_dd28v TYPE TABLE OF dd28v.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->structure_read( CHANGING cg_structure = ls_dd25v ).
    lo_xml->structure_read( CHANGING cg_structure = ls_dd09l ).

    lo_xml->table_read( EXPORTING iv_name = 'DD26V_TABLE'
                        CHANGING ct_table = lt_dd26v ).
    lo_xml->table_read( EXPORTING iv_name = 'DD27P_TABLE'
                        CHANGING ct_table = lt_dd27p ).
    lo_xml->table_read( EXPORTING iv_name = 'DD28J_TABLE'
                        CHANGING ct_table = lt_dd28j ).
    lo_xml->table_read( EXPORTING iv_name = 'DD28V_TABLE'
                        CHANGING ct_table = lt_dd28v ).

    corr_insert( is_item    = is_item
                 iv_package = iv_package ).

    lv_name = is_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_VIEW_PUT'
      EXPORTING
        name              = lv_name
        dd25v_wa          = ls_dd25v
        dd09l_wa          = ls_dd09l
      TABLES
        dd26v_tab         = lt_dd26v
        dd27p_tab         = lt_dd27p
        dd28j_tab         = lt_dd28j
        dd28v_tab         = lt_dd28v
      EXCEPTIONS
        view_not_found    = 1
        name_inconsistent = 2
        view_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_VIEW_PUT'.
    ENDIF.

    activation_add( iv_type = is_item-obj_type
                    iv_name = is_item-obj_name ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_view IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_nrob DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_nrob DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_nrob DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_nrob IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_nrob IMPLEMENTATION.

  METHOD serialize.

    DATA: lo_xml        TYPE REF TO lcl_xml,
          lv_object     TYPE tnro-object,
          ls_file       LIKE LINE OF rt_files,
          ls_attributes TYPE tnro,
          ls_text       TYPE tnrot.


    lv_object = is_item-obj_name.

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_READ'
      EXPORTING
        language          = gc_english
        object            = lv_object
      IMPORTING
        object_attributes = ls_attributes
        object_text       = ls_text
      EXCEPTIONS
        object_not_found  = 1
        OTHERS            = 2.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      _raise 'error from NUMBER_RANGE_OBJECT_READ'.
    ENDIF.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_attributes ).
    lo_xml->structure_add( ls_text ).
    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.                    "serialize

  METHOD deserialize.

    DATA: lo_xml        TYPE REF TO lcl_xml,
          lt_errors     TYPE TABLE OF inoer,
          ls_attributes TYPE tnro,
          ls_text       TYPE tnrot.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).
    lo_xml->structure_read( CHANGING cg_structure = ls_attributes ).
    lo_xml->structure_read( CHANGING cg_structure = ls_text ).

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_UPDATE'
      EXPORTING
        indicator                 = 'I'
        object_attributes         = ls_attributes
        object_text               = ls_text
      TABLES
        errors                    = lt_errors
      EXCEPTIONS
        object_already_exists     = 1
        object_attributes_missing = 2
        object_not_found          = 3
        object_text_missing       = 4
        wrong_indicator           = 5
        OTHERS                    = 6.
    IF sy-subrc <> 0.
      _raise 'error from NUMBER_RANGE_OBJECT_UPDATE'.
    ENDIF.

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_CLOSE'
      EXPORTING
        object                 = ls_attributes-object
      EXCEPTIONS
        object_not_initialized = 1.
    IF sy-subrc <> 0.
      _raise 'error from NUMBER_RANGE_OBJECT_CLOSE'.
    ENDIF.

    CALL FUNCTION 'TR_TADIR_INTERFACE'
      EXPORTING
        wi_test_modus       = abap_false
        wi_tadir_pgmid      = 'R3TR'
        wi_tadir_object     = 'NROB'
        wi_tadir_obj_name   = is_item-obj_name
        wi_tadir_author     = sy-uname
        wi_tadir_devclass   = iv_package
        wi_tadir_masterlang = gc_english
        wi_set_genflag      = abap_true
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      _raise 'error from TR_TADIR_INTERFACE'.
    ENDIF.

  ENDMETHOD.                    "deserialize

  METHOD delete.

    DATA: lv_object TYPE tnro-object.


    lv_object = is_item-obj_name.

    CALL FUNCTION 'NUMBER_RANGE_OBJECT_DELETE'
      EXPORTING
        language           = gc_english
        object             = lv_object
      EXCEPTIONS
        delete_not_allowed = 1
        object_not_found   = 2
        wrong_indicator    = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      _raise 'error from NUMBER_RANGE_OBJECT_DELETE'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD jump.

    _raise 'todo'.

  ENDMETHOD.                    "jump

ENDCLASS.                    "lcl_object_nrob IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_ttyp DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ttyp DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_ttyp IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_ttyp IMPLEMENTATION.

  METHOD jump.

    jump_se11( is_item  = is_item
               iv_radio = 'RSRD1-DDTYPE'
               iv_field = 'RSRD1-DDTYPE_VAL' ).

  ENDMETHOD.                    "jump

  METHOD delete.

    DATA: lv_objname TYPE rsedd0-ddobjname.


    lv_objname = is_item-obj_name.

    CALL FUNCTION 'RS_DD_DELETE_OBJ'
      EXPORTING
        no_ask               = abap_true
        objname              = lv_objname
        objtype              = 'A'
      EXCEPTIONS
        not_executed         = 1
        object_not_found     = 2
        object_not_specified = 3
        permission_failure   = 4.
    IF sy-subrc <> 0.
      _raise 'error from RS_DD_DELETE_OBJ, TTYP'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD serialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          ls_file  TYPE st_file,
          lv_name  TYPE ddobjname,
          lt_dd42v TYPE dd42v_tab,
          lt_dd43v TYPE dd43v_tab,
          ls_dd40v TYPE dd40v.


    lv_name = is_item-obj_name.

    CALL FUNCTION 'DDIF_TTYP_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = gc_english
      IMPORTING
        dd40v_wa      = ls_dd40v
      TABLES
        dd42v_tab     = lt_dd42v
        dd43v_tab     = lt_dd43v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_TTYP_GET'.
    ENDIF.
    IF ls_dd40v IS INITIAL.
      RETURN. " does not exist in system
    ENDIF.

    CLEAR: ls_dd40v-as4user,
           ls_dd40v-as4date,
           ls_dd40v-as4time.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_dd40v ).
    lo_xml->table_add( lt_dd42v ).
    lo_xml->table_add( lt_dd43v ).

    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.                    "serialize

  METHOD deserialize.

    DATA: lo_xml   TYPE REF TO lcl_xml,
          lv_name  TYPE ddobjname,
          lt_dd42v TYPE dd42v_tab,
          lt_dd43v TYPE dd43v_tab,
          ls_dd40v TYPE dd40v.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->structure_read( CHANGING cg_structure = ls_dd40v ).
    lo_xml->table_read( CHANGING ct_table = lt_dd42v ).
    lo_xml->table_read( CHANGING ct_table = lt_dd43v ).

    corr_insert( is_item    = is_item
                 iv_package = iv_package ).

    lv_name = is_item-obj_name. " type conversion

    CALL FUNCTION 'DDIF_TTYP_PUT'
      EXPORTING
        name              = lv_name
        dd40v_wa          = ls_dd40v
      TABLES
        dd42v_tab         = lt_dd42v
        dd43v_tab         = lt_dd43v
      EXCEPTIONS
        ttyp_not_found    = 1
        name_inconsistent = 2
        ttyp_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      _raise 'error from DDIF_TTYP_PUT'.
    ENDIF.

    activation_add( iv_type = is_item-obj_type
                    iv_name = is_item-obj_name ).

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_object_ttyp IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object_prog DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_prog DEFINITION INHERITING FROM lcl_objects_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING is_item    TYPE st_item
                it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS deserialize_dynpros
      IMPORTING io_xml TYPE REF TO lcl_xml
      RAISING   lcx_exception.

    CLASS-METHODS deserialize_cua
      IMPORTING io_xml  TYPE REF TO lcl_xml
                is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS deserialize_textpool
      IMPORTING it_tpool TYPE textpool_table
                is_item  TYPE st_item
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_object_prog DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object_prog IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_object_prog IMPLEMENTATION.

  METHOD jump.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = is_item-obj_name
        object_type   = 'PROG'
        in_new_window = abap_true.

  ENDMETHOD.                    "jump

  METHOD delete.

    DATA: lv_program LIKE sy-repid.


    lv_program = is_item-obj_name.

    CALL FUNCTION 'RS_DELETE_PROGRAM'
      EXPORTING
        program            = lv_program
        suppress_popup     = abap_true
      EXCEPTIONS
        enqueue_lock       = 1
        object_not_found   = 2
        permission_failure = 3
        reject_deletion    = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      _raise 'error from RS_DELETE_PROGRAM'.
    ENDIF.

  ENDMETHOD.                    "delete

  METHOD deserialize_textpool.

    READ TABLE it_tpool WITH KEY id = 'R' TRANSPORTING NO FIELDS.
    IF ( sy-subrc = 0 AND lines( it_tpool ) = 1 ) OR lines( it_tpool ) = 0.
      RETURN. " no action for includes
    ENDIF.

    INSERT TEXTPOOL is_item-obj_name
      FROM it_tpool
      LANGUAGE gc_english
      STATE 'I'.
    IF sy-subrc <> 0.
      _raise 'error from INSERT TEXTPOOL'.
    ENDIF.

    activation_add( iv_type = 'REPT'
                    iv_name = is_item-obj_name ).

  ENDMETHOD.                    "deserialize_textpool

  METHOD deserialize_cua.

    DATA: ls_tr_key TYPE trkey,
          ls_adm    TYPE rsmpe_adm,
          lt_sta    TYPE TABLE OF rsmpe_stat,
          lt_fun    TYPE TABLE OF rsmpe_funt,
          lt_men    TYPE TABLE OF rsmpe_men,
          lt_mtx    TYPE TABLE OF rsmpe_mnlt,
          lt_act    TYPE TABLE OF rsmpe_act,
          lt_but    TYPE TABLE OF rsmpe_but,
          lt_pfk    TYPE TABLE OF rsmpe_pfk,
          lt_set    TYPE TABLE OF rsmpe_staf,
          lt_doc    TYPE TABLE OF rsmpe_atrt,
          lt_tit    TYPE TABLE OF rsmpe_titt,
          lt_biv    TYPE TABLE OF rsmpe_buts.


    io_xml->structure_read( CHANGING cg_structure = ls_adm ).
    IF ls_adm IS INITIAL.
      RETURN.
    ENDIF.

    io_xml->table_read( EXPORTING iv_name = 'RSMPE_STAT_TABLE'
                        CHANGING ct_table = lt_sta ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_FUNT_TABLE'
                        CHANGING ct_table = lt_fun ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_MEN_TABLE'
                        CHANGING ct_table = lt_men ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_MNLT_TABLE'
                        CHANGING ct_table = lt_mtx ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_ACT_TABLE'
                        CHANGING ct_table = lt_act ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_BUT_TABLE'
                        CHANGING ct_table = lt_but ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_PFK_TABLE'
                        CHANGING ct_table = lt_pfk ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_STAF_TABLE'
                        CHANGING ct_table = lt_set ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_ATRT_TABLE'
                        CHANGING ct_table = lt_doc ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_TITT_TABLE'
                        CHANGING ct_table = lt_tit ).
    io_xml->table_read( EXPORTING iv_name = 'RSMPE_BUTS_TABLE'
                        CHANGING ct_table = lt_biv ).

    SELECT SINGLE devclass INTO ls_tr_key-devclass
      FROM tadir
      WHERE pgmid = 'R3TR'
      AND object = is_item-obj_type
      AND obj_name = is_item-obj_name.                  "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'not found in tadir'.
    ENDIF.

    ls_tr_key-obj_type = is_item-obj_type.
    ls_tr_key-obj_name = is_item-obj_name.
    ls_tr_key-sub_type = 'CUAD'.
    ls_tr_key-sub_name = is_item-obj_name.

    CALL FUNCTION 'RS_CUA_INTERNAL_WRITE'
      EXPORTING
        program   = is_item-obj_name
        language  = gc_english
        tr_key    = ls_tr_key
        adm       = ls_adm
        state     = 'I'
      TABLES
        sta       = lt_sta
        fun       = lt_fun
        men       = lt_men
        mtx       = lt_mtx
        act       = lt_act
        but       = lt_but
        pfk       = lt_pfk
        set       = lt_set
        doc       = lt_doc
        tit       = lt_tit
        biv       = lt_biv
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      _raise 'error from RS_CUA_INTERNAL_WRITE'.
    ENDIF.

    activation_add( iv_type = 'CUAD'
                    iv_name = is_item-obj_name ).

  ENDMETHOD.                    "deserialize_cua

  METHOD serialize.

    rt_files = serialize_program( is_item ).

  ENDMETHOD.                    "lif_serialize~serialize

  METHOD deserialize.

    DATA: lo_xml     TYPE REF TO lcl_xml,
          ls_progdir TYPE ty_progdir,
          lt_tpool   TYPE textpool_table,
          lt_source  TYPE abaptxt255_tab.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    read_abap( EXPORTING is_item  = is_item
                         it_files = it_files
               CHANGING  ct_abap  = lt_source ).

    lo_xml->table_read( CHANGING ct_table = lt_tpool ).

    lo_xml->structure_read( EXPORTING iv_name = 'PROGDIR'
                            CHANGING cg_structure = ls_progdir ).
    deserialize_program( is_progdir = ls_progdir
                         it_source  = lt_source
                         it_tpool   = lt_tpool
                         iv_package = iv_package ).

    deserialize_dynpros( lo_xml ).

    deserialize_cua( is_item = is_item
                     io_xml  = lo_xml ).

    deserialize_textpool( is_item = is_item
                          it_tpool = lt_tpool ).

  ENDMETHOD.                    "lif_serialize~deserialize

  METHOD deserialize_dynpros.

    DATA: li_element              TYPE REF TO if_ixml_element,
          ls_header               TYPE rpy_dyhead,
          lt_containers           TYPE dycatt_tab,
          lv_name                 TYPE dwinactiv-obj_name,
          lt_fields_to_containers TYPE dyfatc_tab,
          lt_flow_logic           TYPE swydyflow.


    DO.
      li_element = io_xml->xml_find( 'SCREEN' ).
      IF NOT li_element IS BOUND.
        EXIT. " current loop
      ENDIF.

      io_xml->structure_read( EXPORTING ii_root     = li_element
                              CHANGING cg_structure = ls_header ).

      io_xml->table_read( EXPORTING ii_root  = li_element
                          CHANGING  ct_table = lt_containers ).
      io_xml->table_read( EXPORTING ii_root  = li_element
                          CHANGING  ct_table = lt_fields_to_containers ).
      io_xml->table_read( EXPORTING ii_root  = li_element
                          CHANGING  ct_table = lt_flow_logic ).

      CALL FUNCTION 'RPY_DYNPRO_INSERT'
        EXPORTING
          header                 = ls_header
          suppress_exist_checks  = abap_true
        TABLES
          containers             = lt_containers
          fields_to_containers   = lt_fields_to_containers
          flow_logic             = lt_flow_logic
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

      CONCATENATE ls_header-program ls_header-screen INTO lv_name RESPECTING BLANKS.

      activation_add( iv_type = 'DYNP'
                      iv_name = lv_name ).

    ENDDO.

  ENDMETHOD.                    "deserialize_dynpros

ENDCLASS.                    "lcl_object_prog IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_object DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize
      IMPORTING is_item         TYPE st_item
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS status
      IMPORTING it_files          TYPE tt_files
                iv_package        TYPE devclass OPTIONAL
      RETURNING VALUE(rt_results) TYPE tt_results
      RAISING   lcx_exception.

    CLASS-METHODS deserialize
      IMPORTING it_files   TYPE tt_files
                iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING it_tadir TYPE tt_tadir
      RAISING   lcx_exception.

    CLASS-METHODS jump
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS class_name
      IMPORTING is_item              TYPE st_item
      RETURNING VALUE(rv_class_name) TYPE string.

  PRIVATE SECTION.
    CLASS-METHODS resolve_tabl
      CHANGING ct_tadir TYPE tt_tadir
      RAISING  lcx_exception.

    CLASS-METHODS check_warning
      IMPORTING is_item          TYPE st_item
                iv_package       TYPE devclass
      RETURNING VALUE(rv_cancel) TYPE abap_bool
      RAISING   lcx_exception.

    CLASS-METHODS update_package_tree
      IMPORTING iv_package TYPE devclass.

    CLASS-METHODS delete_obj
      IMPORTING is_item TYPE st_item
      RAISING   lcx_exception.

    CLASS-METHODS compare_files
      IMPORTING it_repo         TYPE tt_files
                is_gen          TYPE st_file
      RETURNING VALUE(rv_match) TYPE sap_bool
      RAISING   lcx_exception.

    CLASS-METHODS show_progress
      IMPORTING iv_current  TYPE i
                iv_total    TYPE i
                iv_obj_name TYPE tadir-obj_name.

    CLASS-METHODS activate
      RAISING lcx_exception.

ENDCLASS.                    "lcl_object DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_object IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_objects IMPLEMENTATION.

  METHOD show_progress.

    DATA: lv_pct TYPE i,
          lv_f   TYPE f.


    lv_f = ( iv_current / iv_total ) * 100.
    lv_pct = lv_f.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_pct
        text       = iv_obj_name.

  ENDMETHOD.

  METHOD check_warning.

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

  ENDMETHOD.

  METHOD update_package_tree.

    DATA: lv_tree TYPE dirtree-tname.


* update package tree for SE80
    lv_tree = 'EU_' && iv_package.
    CALL FUNCTION 'WB_TREE_ACTUALIZE'
      EXPORTING
        tree_name              = lv_tree
        without_crossreference = abap_true
        with_tcode_index       = abap_true.

  ENDMETHOD.

  METHOD class_name.

    CONCATENATE 'LCL_OBJECT_' is_item-obj_type INTO rv_class_name. "#EC NOTEXT
    IF rv_class_name = 'LCL_OBJECT_INTF'.
      rv_class_name = 'LCL_OBJECT_CLAS'.
    ENDIF.

  ENDMETHOD.                    "class_name

  METHOD jump.

    DATA: lv_class_name TYPE string,
          lv_message    TYPE string.


    lv_class_name = class_name( is_item ).

    TRY.
        CALL METHOD (lv_class_name)=>jump
          EXPORTING
            is_item = is_item.
      CATCH cx_sy_dyn_call_illegal_class cx_sy_dyn_call_illegal_method.
        CONCATENATE 'Object type' is_item-obj_type 'not supported, jump'
          INTO lv_message
          SEPARATED BY space.                               "#EC NOTEXT
        _raise lv_message.
    ENDTRY.

  ENDMETHOD.                    "jump

  METHOD delete.

    DATA: ls_item  TYPE st_item,
          lt_tadir LIKE it_tadir.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


* misuse field KORRNUM to fix deletion sequence

    lt_tadir[] = it_tadir[].

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      CASE <ls_tadir>-object.
        WHEN 'SUSC'.
          <ls_tadir>-korrnum = '5000'.
        WHEN 'TTYP'.
          <ls_tadir>-korrnum = '6000'.
        WHEN 'TABL' OR 'VIEW'.
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

    resolve_tabl( CHANGING ct_tadir = lt_tadir ).

    SORT lt_tadir BY korrnum ASCENDING.

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      CLEAR ls_item.
      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      lcl_objects=>delete_obj( ls_item ).
    ENDLOOP.

  ENDMETHOD.                    "delete

  METHOD resolve_tabl.
* this will make sure the deletion sequence of structures/tables work
* in case they have dependencies with .INCLUDE

    TYPES: BEGIN OF ty_edge,
             from TYPE sobj_name,
             to   TYPE sobj_name,
           END OF ty_edge.

    DATA: lt_nodes       TYPE TABLE OF sobj_name,
          lt_edges       TYPE TABLE OF ty_edge,
          lt_findstrings TYPE TABLE OF rsfind,
          lv_plus        TYPE i VALUE 0,
          lv_index       TYPE i,
          lv_before      TYPE i,
          lt_founds      TYPE TABLE OF rsfindlst,
          lt_scope       TYPE STANDARD TABLE OF seu_obj.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF ct_tadir,
                   <ls_edge>  LIKE LINE OF lt_edges,
                   <ls_found> LIKE LINE OF lt_founds,
                   <lv_node>  LIKE LINE OF lt_nodes.


* build nodes
    LOOP AT ct_tadir ASSIGNING <ls_tadir> WHERE object = 'TABL'.
      APPEND <ls_tadir>-obj_name TO lt_nodes.
    ENDLOOP.

    APPEND 'TABL' TO lt_scope.
    APPEND 'STRU' TO lt_scope.

* build edges
    LOOP AT lt_nodes ASSIGNING <lv_node>.

      CLEAR lt_findstrings.
      APPEND <lv_node> TO lt_findstrings.

      CALL FUNCTION 'RS_EU_CROSSREF'
        EXPORTING
          i_find_obj_cls           = 'TABL'
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
        <ls_edge>-from = <lv_node>.
        <ls_edge>-to   = <ls_found>-object.
      ENDLOOP.

    ENDLOOP.

    DO.
      lv_before = lines( lt_nodes ).
      LOOP AT lt_nodes ASSIGNING <lv_node>.
        lv_index = sy-tabix.
        READ TABLE lt_edges WITH KEY from = <lv_node> TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          LOOP AT ct_tadir ASSIGNING <ls_tadir>
              WHERE obj_name = <lv_node> AND object = 'TABL'.
            <ls_tadir>-korrnum = <ls_tadir>-korrnum + lv_plus.
            CONDENSE <ls_tadir>-korrnum.
          ENDLOOP.
          DELETE lt_edges WHERE to = <lv_node>.
          DELETE lt_nodes INDEX lv_index.
          EXIT. " make sure the sequence is fixed
        ENDIF.
      ENDLOOP.
      IF lv_before = lines( lt_nodes ).
        EXIT.
      ENDIF.
      lv_plus = lv_plus + 1.
    ENDDO.

  ENDMETHOD.

  METHOD delete_obj.

    DATA: lv_class_name TYPE string,
          lv_message    TYPE string.


    lv_class_name = class_name( is_item ).

    TRY.
        CALL METHOD (lv_class_name)=>delete
          EXPORTING
            is_item = is_item.
      CATCH cx_sy_dyn_call_illegal_class cx_sy_dyn_call_illegal_method.
        CONCATENATE 'Object type' is_item-obj_type 'not supported, delete'
          INTO lv_message
          SEPARATED BY space.                               "#EC NOTEXT
        _raise lv_message.
    ENDTRY.

  ENDMETHOD.                    "delete

  METHOD serialize.

    DATA: lt_files      TYPE tt_files,
          lv_class_name TYPE string,
          lv_message    TYPE string.


    lv_class_name = class_name( is_item ).

    TRY.
        CALL METHOD (lv_class_name)=>serialize
          EXPORTING
            is_item  = is_item
          RECEIVING
            rt_files = rt_files.
      CATCH cx_sy_dyn_call_illegal_class cx_sy_dyn_call_illegal_method.
        CONCATENATE 'Object type' is_item-obj_type 'not supported, serialize'
          INTO lv_message
          SEPARATED BY space.                               "#EC NOTEXT
        _raise lv_message.
    ENDTRY.

* check for duplicates
    lt_files[] = rt_files[].
    SORT lt_files BY path ASCENDING filename ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_files COMPARING path filename.
    IF lines( lt_files ) <> lines( rt_files ).
      _raise 'Duplicates'.
    ENDIF.

  ENDMETHOD.                    "serialize

  METHOD status.

    DATA: lv_pre    TYPE tadir-obj_name,
          lt_files  TYPE tt_files,
          ls_result LIKE LINE OF rt_results,
          lv_type   TYPE string,
          ls_item   TYPE st_item,
          lt_tadir  TYPE tt_tadir,
          lv_ext    TYPE string.

    FIELD-SYMBOLS: <ls_file>  LIKE LINE OF it_files,
                   <ls_tadir> LIKE LINE OF lt_tadir,
                   <ls_gen>   LIKE LINE OF lt_files.


    LOOP AT it_files ASSIGNING <ls_file>.
      SPLIT <ls_file>-filename AT '.' INTO lv_pre lv_type lv_ext.
      TRANSLATE lv_pre TO UPPER CASE.
      TRANSLATE lv_type TO UPPER CASE.

      IF lv_ext <> 'xml' OR strlen( lv_type ) <> 4.
        CONTINUE. " current loop
      ENDIF.

* handle namespaces
      REPLACE ALL OCCURRENCES OF '#' IN lv_pre WITH '/'.

      CLEAR ls_result.
      ls_result-obj_type = lv_type.
      ls_result-obj_name = lv_pre.

      CLEAR ls_item.
      ls_item-obj_type = lv_type.
      ls_item-obj_name = lv_pre.

      lt_files = serialize( ls_item ).

      IF lt_files[] IS INITIAL.
* item does not exist locally
        ls_result-filename = <ls_file>-filename.
        APPEND ls_result TO rt_results.
        CONTINUE. " current loop
      ENDIF.

      LOOP AT lt_files ASSIGNING <ls_gen>.
        ls_result-filename = <ls_gen>-filename.
        ls_result-match = compare_files( it_repo = it_files
                                         is_gen  = <ls_gen> ).
        APPEND ls_result TO rt_results.
      ENDLOOP.
    ENDLOOP.

* find files only existing remotely, including non abapGit related
    LOOP AT it_files ASSIGNING <ls_file>.
      READ TABLE rt_results WITH KEY filename = <ls_file>-filename
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CLEAR ls_result.
        ls_result-match    = abap_true.
        ls_result-filename = <ls_file>-filename.
        APPEND ls_result TO rt_results.
      ENDIF.
    ENDLOOP.

* find objects only existing locally
    IF NOT iv_package IS INITIAL.
      lt_tadir = lcl_tadir=>read( iv_package ).
      LOOP AT lt_tadir ASSIGNING <ls_tadir>.
        READ TABLE rt_results
          WITH KEY obj_type = <ls_tadir>-object obj_name = <ls_tadir>-obj_name
          TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CLEAR ls_result.
          ls_result-match    = abap_true.
          ls_result-obj_type = <ls_tadir>-object.
          ls_result-obj_name = <ls_tadir>-obj_name.
          APPEND ls_result TO rt_results.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SORT rt_results BY obj_type ASCENDING obj_name ASCENDING filename ASCENDING.
    DELETE ADJACENT DUPLICATES FROM rt_results
      COMPARING obj_type obj_name filename.

  ENDMETHOD.                    "status

  METHOD deserialize.

    DATA: ls_item       TYPE st_item,
          lv_class_name TYPE string,
          lv_message    TYPE string,
          lv_cancel     TYPE abap_bool,
          lt_results    TYPE tt_results.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results.


    CLEAR lcl_objects_common=>gt_ddic[].
    CLEAR lcl_objects_common=>gt_programs[].

    lt_results = status( it_files   = it_files
                         iv_package = iv_package ).
    DELETE lt_results WHERE match = abap_true.
    SORT lt_results BY obj_type ASCENDING obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_results COMPARING obj_type obj_name.

    LOOP AT lt_results ASSIGNING <ls_result>.
      show_progress( iv_current  = sy-tabix
                     iv_total    = lines( lt_results )
                     iv_obj_name = <ls_result>-obj_name ).

      CLEAR ls_item.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.
* handle namespaces
      REPLACE ALL OCCURRENCES OF '#' IN ls_item-obj_name WITH '/'.

      lv_cancel = check_warning( is_item    = ls_item
                                 iv_package = iv_package ).
      IF lv_cancel = abap_true.
        RETURN.
      ENDIF.

      lv_class_name = class_name( ls_item ).

      TRY.
          CALL METHOD (lv_class_name)=>deserialize
            EXPORTING
              is_item    = ls_item
              it_files   = it_files
              iv_package = iv_package.
        CATCH cx_sy_dyn_call_illegal_class cx_sy_dyn_call_illegal_method.
          CONCATENATE 'Object type' ls_item-obj_type 'not supported, deserialize'
            INTO lv_message
            SEPARATED BY space.                             "#EC NOTEXT
          _raise lv_message.
      ENDTRY.

    ENDLOOP.

    activate( ).

    update_package_tree( iv_package ).

  ENDMETHOD.                    "deserialize

  METHOD activate.

* ddic
    IF NOT lcl_objects_common=>gt_ddic[] IS INITIAL.
      CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
        EXPORTING
          activate_ddic_objects  = abap_true
          with_popup             = abap_true
        TABLES
          objects                = lcl_objects_common=>gt_ddic
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
    IF NOT lcl_objects_common=>gt_programs[] IS INITIAL.
      CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
        EXPORTING
          activate_ddic_objects  = abap_false
          with_popup             = abap_true
        TABLES
          objects                = lcl_objects_common=>gt_programs
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

  METHOD compare_files.

    READ TABLE it_repo WITH KEY path = is_gen-path
                                filename = is_gen-filename
                                data = is_gen-data
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      rv_match = abap_false.
    ELSE.
      rv_match = abap_true.
    ENDIF.

  ENDMETHOD.                    "compare_files

ENDCLASS.                    "lcl_object IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_hash DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_hash DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS adler32
      IMPORTING iv_xstring         TYPE xstring
      RETURNING VALUE(rv_checksum) TYPE t_adler32.

    CLASS-METHODS sha1
      IMPORTING iv_type        TYPE t_type
                iv_data        TYPE xstring
      RETURNING VALUE(rv_sha1) TYPE t_sha1
      RAISING   lcx_exception.

    CLASS-METHODS sha1_raw
      IMPORTING iv_data        TYPE xstring
      RETURNING VALUE(rv_sha1) TYPE t_sha1
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_hash DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_hash IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_hash IMPLEMENTATION.

  METHOD adler32.

    CONSTANTS: lc_adler TYPE i VALUE 65521.

    DATA: lv_index TYPE i,
          lv_a     TYPE i VALUE 1,
          lv_b     TYPE i VALUE 0,
          lv_x     TYPE x LENGTH 2,
          lv_ca    TYPE c LENGTH 4,
          lv_cb    TYPE c LENGTH 4,
          lv_char8 TYPE c LENGTH 8.


    DO xstrlen( iv_xstring ) TIMES.
      lv_index = sy-index - 1.

      lv_a = ( lv_a + iv_xstring+lv_index(1) ) MOD lc_adler.
      lv_b = ( lv_b + lv_a ) MOD lc_adler.
    ENDDO.

    lv_x = lv_a.
    lv_ca = lv_x.

    lv_x = lv_b.
    lv_cb = lv_x.

    CONCATENATE lv_cb lv_ca INTO lv_char8.

    rv_checksum = lv_char8.

  ENDMETHOD.                                                "adler32

  METHOD sha1_raw.

    DATA: lv_hash TYPE hash160.


    CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
      EXPORTING
        data           = iv_data
      IMPORTING
        hash           = lv_hash
      EXCEPTIONS
        unknown_alg    = 1
        param_error    = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      _raise 'Error while calculating SHA1'.
    ENDIF.

    rv_sha1 = lv_hash.

    TRANSLATE rv_sha1 TO LOWER CASE.

  ENDMETHOD.                                                "sha1_raw

  METHOD sha1.

    DATA: lv_len     TYPE i,
          lv_char10  TYPE c LENGTH 10,
          lv_string  TYPE string,
          lv_xstring TYPE xstring.


    lv_len = xstrlen( iv_data ).
    lv_char10 = lv_len.
    CONDENSE lv_char10.
    CONCATENATE iv_type lv_char10 INTO lv_string SEPARATED BY space.
    lv_xstring = lcl_convert=>string_to_xstring_utf8( lv_string ).

    lv_string = lv_xstring.
    CONCATENATE lv_string '00' INTO lv_string.
    lv_xstring = lv_string.

    CONCATENATE lv_xstring iv_data INTO lv_xstring IN BYTE MODE.

    rv_sha1 = sha1_raw( lv_xstring ).

  ENDMETHOD.                                                "sha1

ENDCLASS.                    "lcl_hash IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_pack DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_pack DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS decode
      IMPORTING iv_data           TYPE xstring
      RETURNING VALUE(rt_objects) TYPE tt_objects
      RAISING   lcx_exception.

    CLASS-METHODS decode_tree
      IMPORTING iv_data         TYPE xstring
      RETURNING VALUE(rt_nodes) TYPE tt_nodes
      RAISING   lcx_exception.

    CLASS-METHODS decode_deltas
      CHANGING ct_objects TYPE tt_objects
      RAISING  lcx_exception.

    CLASS-METHODS decode_commit
      IMPORTING iv_data          TYPE xstring
      RETURNING VALUE(rs_commit) TYPE st_commit
      RAISING   lcx_exception.

    CLASS-METHODS encode
      IMPORTING it_objects     TYPE tt_objects
      RETURNING VALUE(rv_data) TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS encode_tree
      IMPORTING it_nodes       TYPE tt_nodes
      RETURNING VALUE(rv_data) TYPE xstring.

    CLASS-METHODS encode_commit
      IMPORTING is_commit      TYPE st_commit
      RETURNING VALUE(rv_data) TYPE xstring.

  PRIVATE SECTION.
    CONSTANTS: c_pack_start TYPE x LENGTH 4 VALUE '5041434B', " PACK
               c_debug_pack TYPE sap_bool VALUE abap_false,
               c_zlib       TYPE x LENGTH 2 VALUE '789C',
               c_zlib_hmm   TYPE x LENGTH 2 VALUE '7801',
               c_version    TYPE x LENGTH 4 VALUE '00000002'.

    CLASS-METHODS type_and_length
      IMPORTING is_object         TYPE st_object
      RETURNING VALUE(rv_xstring) TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS delta
      IMPORTING is_object  TYPE st_object
      CHANGING  ct_objects TYPE tt_objects
      RAISING   lcx_exception.

    CLASS-METHODS delta_header
      CHANGING cv_delta TYPE xstring.

    CLASS-METHODS get_type
      IMPORTING iv_x           TYPE x
      RETURNING VALUE(rv_type) TYPE t_type
      RAISING   lcx_exception.

    CLASS-METHODS get_length
      EXPORTING ev_length TYPE i
      CHANGING  cv_data   TYPE xstring.

ENDCLASS.                    "lcl_pack DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_pack IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_pack IMPLEMENTATION.

  METHOD type_and_length.

    DATA: lv_bits   TYPE string,
          lv_type   TYPE string,
          lv_result TYPE string,
          lv_c      TYPE c,
          lv_offset TYPE i,
          lv_x4     TYPE x LENGTH 4,
          lv_x      TYPE x LENGTH 1.


    CASE is_object-type.
      WHEN gc_commit.
        lv_type = '001'.
      WHEN gc_tree.
        lv_type = '010'.
      WHEN gc_blob.
        lv_type = '011'.
      WHEN gc_ref_d.
        lv_type = '111'.
      WHEN OTHERS.
        _raise 'Unexpected object type while encoding pack'.
    ENDCASE.

    lv_x4 = xstrlen( is_object-data ).
    DO 32 TIMES.
      GET BIT sy-index OF lv_x4 INTO lv_c.
      CONCATENATE lv_bits lv_c INTO lv_bits.
    ENDDO.

    IF lv_bits(28) = '0000000000000000000000000000'.
      CONCATENATE '0' lv_type lv_bits+28(4) INTO lv_result.
    ELSEIF lv_bits(21) = '000000000000000000000'.
      CONCATENATE '1' lv_type lv_bits+28(4) INTO lv_result.
      CONCATENATE lv_result '0' lv_bits+21(7) INTO lv_result.
    ELSEIF lv_bits(14) = '00000000000000'.
      CONCATENATE '1' lv_type lv_bits+28(4) INTO lv_result.
      CONCATENATE lv_result '1' lv_bits+21(7) INTO lv_result.
      CONCATENATE lv_result '0' lv_bits+14(7) INTO lv_result.
    ELSE.
* use shifting?
      _raise 'Todo, encoding length'.
    ENDIF.

* convert bit string to xstring
    CLEAR lv_x.
    DO strlen( lv_result ) TIMES.
      lv_offset = sy-index - 1.
      IF lv_result+lv_offset(1) = '1'.
        SET BIT ( lv_offset MOD 8 ) + 1 OF lv_x.
      ENDIF.
      IF ( lv_offset + 1 ) MOD 8 = 0.
        CONCATENATE rv_xstring lv_x INTO rv_xstring IN BYTE MODE.
        CLEAR lv_x.
      ENDIF.
    ENDDO.

  ENDMETHOD.                    "type_and_length

  METHOD get_length.

    DATA: lv_x           TYPE x,
          lv_length_bits TYPE string,
          lv_bitbyte     TYPE t_bitbyte.


    lv_x = cv_data(1).
    IF c_debug_pack = abap_true.
      WRITE: / 'A:', lv_x, '(hex)'.                         "#EC NOTEXT
    ENDIF.
    lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).
    IF c_debug_pack = abap_true.
      WRITE: lv_bitbyte.
    ENDIF.

    cv_data = cv_data+1.
    lv_length_bits = lv_bitbyte+4.

    WHILE lv_bitbyte(1) <> '0'.
      lv_x = cv_data(1).
      IF c_debug_pack = abap_true.
        WRITE: / 'x:', lv_x, '(hex)'.                       "#EC NOTEXT
      ENDIF.
      lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).
      IF c_debug_pack = abap_true.
        WRITE: lv_bitbyte.
      ENDIF.
      cv_data = cv_data+1.
      CONCATENATE lv_bitbyte+1 lv_length_bits INTO lv_length_bits.
    ENDWHILE.

    ev_length = lcl_convert=>bitbyte_to_int( lv_length_bits ).

  ENDMETHOD.                    "get_length

  METHOD encode_tree.

    CONSTANTS: lc_null TYPE x VALUE '00'.

    DATA: lv_string  TYPE string,
          lt_nodes   LIKE it_nodes,
          lv_hex20   TYPE x LENGTH 20,
          lv_xstring TYPE xstring.

    FIELD-SYMBOLS: <ls_node> LIKE LINE OF it_nodes.


    lt_nodes[] = it_nodes[].
* following has to be done, or unpack will fail on server side
    SORT lt_nodes BY name ASCENDING.

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      CONCATENATE <ls_node>-chmod <ls_node>-name INTO lv_string SEPARATED BY space.
      lv_xstring = lcl_convert=>string_to_xstring_utf8( lv_string ).

      lv_hex20 = to_upper( <ls_node>-sha1 ).
      CONCATENATE rv_data lv_xstring lc_null lv_hex20 INTO rv_data IN BYTE MODE.
    ENDLOOP.

  ENDMETHOD.                    "encode_tree

  METHOD encode_commit.

    DATA: lv_string       TYPE string,
          lv_tmp          TYPE string,
          lv_tree_lower   TYPE string,
          lv_parent_lower TYPE string.


    lv_tree_lower = is_commit-tree.
    TRANSLATE lv_tree_lower TO LOWER CASE.

    lv_parent_lower = is_commit-parent.
    TRANSLATE lv_parent_lower TO LOWER CASE.

    lv_string = ''.

    CONCATENATE 'tree' lv_tree_lower INTO lv_tmp SEPARATED BY space. "#EC NOTEXT
    CONCATENATE lv_string lv_tmp gc_newline INTO lv_string.

    IF NOT is_commit-parent IS INITIAL.
      CONCATENATE 'parent' lv_parent_lower
        INTO lv_tmp SEPARATED BY space.                     "#EC NOTEXT
      CONCATENATE lv_string lv_tmp gc_newline INTO lv_string.
    ENDIF.

    CONCATENATE 'author' is_commit-author
      INTO lv_tmp SEPARATED BY space.                       "#EC NOTEXT
    CONCATENATE lv_string lv_tmp gc_newline INTO lv_string.

    CONCATENATE 'committer' is_commit-committer
      INTO lv_tmp SEPARATED BY space.                       "#EC NOTEXT
    CONCATENATE lv_string lv_tmp gc_newline INTO lv_string.

    CONCATENATE lv_string gc_newline is_commit-body INTO lv_string.

    rv_data = lcl_convert=>string_to_xstring_utf8( lv_string ).

  ENDMETHOD.                    "encode_commit

  METHOD get_type.

    DATA: lv_char3   TYPE c LENGTH 3,
          lv_bitbyte TYPE t_bitbyte.


    lv_bitbyte = lcl_convert=>x_to_bitbyte( iv_x ).
    lv_char3 = lv_bitbyte+1.

    CASE lv_char3.
      WHEN '001'.
        rv_type = gc_commit.
      WHEN '010'.
        rv_type = gc_tree.
      WHEN '011'.
        rv_type = gc_blob.
      WHEN '111'.
        rv_type = gc_ref_d.
      WHEN OTHERS.
        _raise 'Todo, unknown type'.
    ENDCASE.

  ENDMETHOD.                    "get_type

  METHOD decode_commit.

    DATA: lv_string TYPE string,
          lv_mode   TYPE string,
          lv_len    TYPE i,
          lt_string TYPE TABLE OF string.

    FIELD-SYMBOLS: <lv_string> LIKE LINE OF lt_string.


    lv_string = lcl_convert=>xstring_to_string_utf8( iv_data ).

    SPLIT lv_string AT gc_newline INTO TABLE lt_string.

    lv_mode = 'tree'.                                       "#EC NOTEXT
    LOOP AT lt_string ASSIGNING <lv_string>.
      lv_len = strlen( lv_mode ).

      IF NOT lv_mode IS INITIAL AND <lv_string>(lv_len) = lv_mode.
        CASE lv_mode.
          WHEN 'tree'.
            rs_commit-tree = <lv_string>+5.
            lv_mode = 'parent'.                             "#EC NOTEXT
          WHEN 'parent'.
            rs_commit-parent = <lv_string>+7.
            lv_mode = 'author'.                             "#EC NOTEXT
          WHEN 'author'.
            rs_commit-author = <lv_string>+7.
            lv_mode = 'committer'.                          "#EC NOTEXT
          WHEN 'committer'.
            rs_commit-committer = <lv_string>+10.
            CLEAR lv_mode.
        ENDCASE.
      ELSEIF lv_mode = 'parent' AND <lv_string>(6) = 'author'. "#EC NOTEXT
* first commit doesnt have parent
        rs_commit-author = <lv_string>+7.
        lv_mode = 'committer'.                              "#EC NOTEXT
      ELSE.
* body
        CONCATENATE rs_commit-body <lv_string> INTO rs_commit-body
          SEPARATED BY gc_newline.
      ENDIF.
    ENDLOOP.

* strip first newline
    IF strlen( rs_commit-body ) >= 2.
      rs_commit-body = rs_commit-body+2.
    ENDIF.

    IF rs_commit-author IS INITIAL
        OR rs_commit-committer IS INITIAL
        OR rs_commit-tree IS INITIAL.
      _raise 'multiple parents? not supported'.
    ENDIF.

  ENDMETHOD.                    "decode_commit

  METHOD delta_header.

    DATA: lv_bitbyte TYPE t_bitbyte,
          lv_header1 TYPE i,                                "#EC NEEDED
          lv_header2 TYPE i,                                "#EC NEEDED
          lv_bits    TYPE string,
          lv_x       TYPE x.

* todo, use headers for verification

* Header 1
    lv_bits = ''.
    DO.
      lv_x = cv_delta(1).
      cv_delta = cv_delta+1.
      lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).
      CONCATENATE lv_bitbyte+1 lv_bits INTO lv_bits.
      IF lv_bitbyte(1) = '0'.
        EXIT. " current loop
      ENDIF.
    ENDDO.
    lv_header1 = lcl_convert=>bitbyte_to_int( lv_bits ).

* Header 2
    lv_bits = ''.
    DO.
      lv_x = cv_delta(1).
      cv_delta = cv_delta+1.
      lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).
      CONCATENATE lv_bitbyte+1 lv_bits INTO lv_bits.
      IF lv_bitbyte(1) = '0'.
        EXIT. " current loop
      ENDIF.
    ENDDO.
    lv_header2 = lcl_convert=>bitbyte_to_int( lv_bits ).

  ENDMETHOD.                    "delta_header

  METHOD delta.

    DATA: lv_delta   TYPE xstring,
          lv_base    TYPE xstring,
          lv_result  TYPE xstring,
          lv_bitbyte TYPE t_bitbyte,
          lv_offset  TYPE i,
          lv_message TYPE string,
          lv_sha1    TYPE t_sha1,
          ls_object  LIKE LINE OF ct_objects,
          lv_len     TYPE i,
          lv_x       TYPE x.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF ct_objects.


    lv_delta = is_object-data.

* find base
    READ TABLE ct_objects ASSIGNING <ls_object> WITH KEY sha1 = is_object-sha1.
    IF sy-subrc <> 0.
      CONCATENATE 'Base not found,' is_object-sha1 INTO lv_message
        SEPARATED BY space.                                 "#EC NOTEXT
      _raise lv_message.
    ELSE.
      lv_base = <ls_object>-data.
    ENDIF.

* sanity check
    IF <ls_object>-type = gc_ref_d.
      _raise 'Delta, base eq delta'.
    ENDIF.

    delta_header( CHANGING cv_delta = lv_delta ).


    WHILE xstrlen( lv_delta ) > 0.

      lv_x = lv_delta(1).
      lv_delta = lv_delta+1.
      lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).

      IF lv_bitbyte(1) = '1'. " MSB

        lv_offset = 0.
        IF lv_bitbyte+7(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_offset = lv_x.
        ENDIF.
        IF lv_bitbyte+6(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_offset = lv_offset + lv_x * 256.
        ENDIF.
        IF lv_bitbyte+5(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_offset = lv_offset + lv_x * 65536.
        ENDIF.
        IF lv_bitbyte+4(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_offset = lv_offset + lv_x * 16777216. " hmm, overflow?
        ENDIF.

        lv_len = 0.
        IF lv_bitbyte+3(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_len = lv_x.
        ENDIF.
        IF lv_bitbyte+2(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_len = lv_len + lv_x * 256.
        ENDIF.
        IF lv_bitbyte+1(1) = '1'.
          lv_x = lv_delta(1).
          lv_delta = lv_delta+1.
          lv_len = lv_len + lv_x * 65536.
        ENDIF.

        IF lv_len = 0.
          lv_len = 65536.
        ENDIF.

        CONCATENATE lv_result lv_base+lv_offset(lv_len) INTO lv_result IN BYTE MODE.
      ELSE. " lv_bitbyte(1) = '0'
* insert from delta
        lv_len = lv_x.
        CONCATENATE lv_result lv_delta(lv_len) INTO lv_result IN BYTE MODE.
        lv_delta = lv_delta+lv_len.
      ENDIF.

    ENDWHILE.

    lv_sha1 = lcl_hash=>sha1( iv_type = <ls_object>-type iv_data = lv_result ).

    lcl_debug=>message( 'Delta,' &&
      <ls_object>-type &&
      ',new sha1, ' &&
      lv_sha1 &&
      ',old: ' &&
      <ls_object>-sha1 ).                                   "#EC NOTEXT

    CLEAR ls_object.
    ls_object-sha1 = lv_sha1.
    ls_object-type = <ls_object>-type.
    ls_object-data = lv_result.
    APPEND ls_object TO ct_objects.

  ENDMETHOD.                    "delta

  METHOD decode_deltas.

    DATA: ls_object LIKE LINE OF ct_objects,
          lt_deltas LIKE ct_objects.


    LOOP AT ct_objects INTO ls_object WHERE type = gc_ref_d.
      DELETE ct_objects INDEX sy-tabix.
      APPEND ls_object TO lt_deltas.
    ENDLOOP.

    LOOP AT lt_deltas INTO ls_object.
      delta( EXPORTING is_object = ls_object
             CHANGING ct_objects = ct_objects ).
    ENDLOOP.

  ENDMETHOD.                    "decode_deltas

  METHOD decode_tree.

    CONSTANTS: lc_sha_length TYPE i VALUE 20,
               lc_null       TYPE x VALUE '00'.

    DATA: lv_xstring TYPE xstring,
          lv_chmod   TYPE string,
          lv_name    TYPE string,
          lv_string  TYPE string,
          lv_len     TYPE i,
          lv_offset  TYPE i,
          lv_cursor  TYPE i,
          ls_node    TYPE st_node,
          lv_start   TYPE i.


    DO.
      IF lv_cursor >= xstrlen( iv_data ).
        EXIT. " current loop
      ENDIF.

      IF iv_data+lv_cursor(1) = lc_null.
        lv_len = lv_cursor - lv_start.
        lv_xstring = iv_data+lv_start(lv_len).

        lv_string = lcl_convert=>xstring_to_string_utf8( lv_xstring ).
        SPLIT lv_string AT space INTO lv_chmod lv_name.

        lv_offset = lv_cursor + 1.

        CLEAR ls_node.
        ls_node-chmod = lv_chmod.
        IF ls_node-chmod <> gc_chmod-dir AND ls_node-chmod <> gc_chmod-file.
          _raise 'Unknown chmod'.
        ENDIF.

        ls_node-name = lv_name.
        ls_node-sha1 = iv_data+lv_offset(lc_sha_length).
        TRANSLATE ls_node-sha1 TO LOWER CASE.
        APPEND ls_node TO rt_nodes.

        lv_start = lv_cursor + 1 + lc_sha_length.
        lv_cursor = lv_start.
      ELSE.
        lv_cursor = lv_cursor + 1.
      ENDIF.
    ENDDO.

  ENDMETHOD.                    "decode_tree

  METHOD decode.

    DATA: lv_x              TYPE x,
          lv_data           TYPE xstring,
          lv_type           TYPE c LENGTH 6,
          lv_zlib           TYPE x LENGTH 2,
          lv_objects        TYPE i,
          lv_len            TYPE i,
          lv_sha1           TYPE t_sha1,
          lv_ref_delta      TYPE t_sha1,
          lv_adler32        TYPE t_adler32,
          lv_compressed     TYPE xstring,
          lv_compressed_len TYPE i,
          lv_decompress_len TYPE i,
          lv_decompressed   TYPE xstring,
          lv_xstring        TYPE xstring,
          lv_expected       TYPE i,
          ls_object         LIKE LINE OF rt_objects.


    lv_data = iv_data.

* header
    IF NOT xstrlen( lv_data ) > 4 OR lv_data(4) <> c_pack_start.
      _raise 'Unexpected pack header'.
    ENDIF.
    lv_data = lv_data+4.

* version
    IF lv_data(4) <> c_version.
      _raise 'Version not supported'.
    ENDIF.
    lv_data = lv_data+4.

* number of objects
    lv_xstring = lv_data(4).
    lv_objects = lcl_convert=>xstring_to_int( lv_xstring ).
    lv_data = lv_data+4.


    DO lv_objects TIMES.

      lv_x = lv_data(1).
      lv_type = get_type( lv_x ).

      get_length( IMPORTING ev_length = lv_expected
                  CHANGING cv_data = lv_data ).

      IF lv_type = gc_ref_d.
        lv_ref_delta = lv_data(20).
        lv_data = lv_data+20.
      ENDIF.

* strip header, '789C', CMF + FLG
      lv_zlib = lv_data(2).
      IF lv_zlib <> c_zlib AND lv_zlib <> c_zlib_hmm.
        _raise 'Unexpected zlib header'.
      ENDIF.
      lv_data = lv_data+2.

*******************************

      IF lv_zlib = c_zlib.
        cl_abap_gzip=>decompress_binary(
          EXPORTING
            gzip_in     = lv_data
          IMPORTING
            raw_out     = lv_decompressed
            raw_out_len = lv_decompress_len ).

        IF lv_expected <> lv_decompress_len.
          _raise 'Decompression falied'.
        ENDIF.

        cl_abap_gzip=>compress_binary(
          EXPORTING
            raw_in         = lv_decompressed
          IMPORTING
            gzip_out       = lv_compressed
            gzip_out_len   = lv_compressed_len ).

        IF lv_compressed(lv_compressed_len) <> lv_data(lv_compressed_len).
          _raise 'Compressed data doesnt match'.
        ENDIF.

        lv_data = lv_data+lv_compressed_len.
        lv_data = lv_data+4. " skip adler checksum

      ELSEIF lv_zlib = c_zlib_hmm.
* this takes some processing, when time permits, implement DEFLATE algorithm
* cl_abap_gzip copmression works for '789C', but does not produce the same
* result when '7801'
* compressed data might be larger than origial so add 10, adding 10 is safe
* as package always ends with sha1 checksum
        DO lv_expected + 10 TIMES.
          lv_compressed_len = sy-index.

          cl_abap_gzip=>decompress_binary(
            EXPORTING
              gzip_in     = lv_data
              gzip_in_len = lv_compressed_len
            IMPORTING
              raw_out     = lv_decompressed
              raw_out_len = lv_decompress_len ).

          IF lv_decompress_len = lv_expected.
            EXIT.
          ELSE.
            CLEAR lv_compressed_len.
          ENDIF.
        ENDDO.

        IF lv_compressed_len IS INITIAL.
          _raise 'Decompression falied :o/'.
        ENDIF.

        lv_data = lv_data+lv_compressed_len.

        lv_adler32 = lcl_hash=>adler32( lv_decompressed ).
        IF lv_data(4) <> lv_adler32.
          lv_data = lv_data+1.
        ENDIF.
        IF lv_data(4) <> lv_adler32.
          lv_data = lv_data+1.
        ENDIF.
        IF lv_data(4) <> lv_adler32.
          _raise 'Wrong Adler checksum'.
        ENDIF.

        lv_data = lv_data+4. " skip adler checksum

      ENDIF.

*************************

      CLEAR ls_object.
      IF lv_type = gc_ref_d.
        ls_object-sha1 = lv_ref_delta.
        TRANSLATE ls_object-sha1 TO LOWER CASE.
      ELSE.
        ls_object-sha1 = lcl_hash=>sha1( iv_type = lv_type iv_data = lv_decompressed ).
      ENDIF.
      ls_object-type = lv_type.
      ls_object-data = lv_decompressed.
      APPEND ls_object TO rt_objects.

      IF c_debug_pack = abap_true.
        WRITE: /.
      ENDIF.
    ENDDO.

* check SHA1 at end of pack
    lv_len = xstrlen( iv_data ) - 20.
    lv_xstring = iv_data(lv_len).
    lv_sha1 = lcl_hash=>sha1_raw( lv_xstring ).
    IF to_upper( lv_sha1 ) <> lv_data.
      _raise 'SHA1 at end of pack doesnt match'.
    ENDIF.

  ENDMETHOD.                    "decode

  METHOD encode.

    DATA: lv_sha1       TYPE x LENGTH 20,
          lv_adler32    TYPE t_adler32,
          lv_len        TYPE i,
          lv_compressed TYPE xstring,
          lv_xstring    TYPE xstring.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects.


    rv_data = c_pack_start.

    CONCATENATE rv_data c_version INTO rv_data IN BYTE MODE.

    lv_len = lines( it_objects ).
    lv_xstring = lcl_convert=>int_to_xstring( iv_i      = lv_len
                                              iv_length = 4 ).
    CONCATENATE rv_data lv_xstring INTO rv_data IN BYTE MODE.

    LOOP AT it_objects ASSIGNING <ls_object>.
      lv_xstring = type_and_length( <ls_object> ).
      CONCATENATE rv_data lv_xstring INTO rv_data IN BYTE MODE.

      cl_abap_gzip=>compress_binary(
        EXPORTING
          raw_in   = <ls_object>-data
        IMPORTING
          gzip_out = lv_compressed ).

      CONCATENATE rv_data c_zlib lv_compressed INTO rv_data IN BYTE MODE.

      lv_adler32 = lcl_hash=>adler32( <ls_object>-data ).
      CONCATENATE rv_data lv_adler32 INTO rv_data IN BYTE MODE.

    ENDLOOP.

    lv_sha1 = to_upper( lcl_hash=>sha1_raw( rv_data ) ).
    CONCATENATE rv_data lv_sha1 INTO rv_data IN BYTE MODE.

  ENDMETHOD.                    "encode

ENDCLASS.                    "lcl_pack IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_persistence DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_persistence DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS list
      RETURNING VALUE(rt_repos) TYPE tt_repos_persi
      RAISING   lcx_exception.

    CLASS-METHODS update
      IMPORTING is_repo   TYPE st_repo
                iv_branch TYPE t_sha1
      RAISING   lcx_exception.

    CLASS-METHODS add
      IMPORTING is_repo    TYPE st_repo
                iv_branch  TYPE t_sha1 OPTIONAL
                iv_package TYPE devclass
                iv_offline TYPE sap_bool DEFAULT abap_false
      RAISING   lcx_exception.

    CLASS-METHODS validate_package
      IMPORTING iv_package TYPE devclass
      RAISING   lcx_exception.

    CLASS-METHODS delete
      IMPORTING is_repo TYPE st_repo_persi
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS read_text_online
      RETURNING VALUE(rt_repos) TYPE tt_repos_persi
      RAISING   lcx_exception.

    CLASS-METHODS save_text_online
      IMPORTING it_repos TYPE tt_repos_persi
      RAISING   lcx_exception.

    CLASS-METHODS header_online
      RETURNING VALUE(rs_header) TYPE thead.

    CLASS-METHODS read_text_offline
      RETURNING VALUE(rt_repos) TYPE tt_repos_persi
      RAISING   lcx_exception.

    CLASS-METHODS save_text_offline
      IMPORTING it_repos TYPE tt_repos_persi
      RAISING   lcx_exception.

    CLASS-METHODS header_offline
      RETURNING VALUE(rs_header) TYPE thead.

    CLASS-METHODS read_text
      IMPORTING is_header       TYPE thead
      RETURNING VALUE(rt_lines) TYPE tlinetab
      RAISING   lcx_exception.

    CLASS-METHODS save_text
      IMPORTING is_header TYPE thead
                it_lines  TYPE tlinetab
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_persistence DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_persistence IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_persistence IMPLEMENTATION.

  METHOD save_text.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header   = is_header
      TABLES
        lines    = it_lines
      EXCEPTIONS
        id       = 1
        language = 2
        name     = 3
        object   = 4
        OTHERS   = 5.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      _raise 'error from SAVE_TEXT'.
    ENDIF.

  ENDMETHOD.                    "save_text

  METHOD header_online.
    rs_header-tdid     = 'ST'.
    rs_header-tdspras  = gc_english.
    rs_header-tdname   = 'ZABAPGIT'.
    rs_header-tdobject = 'TEXT'.
  ENDMETHOD.                    "header

  METHOD header_offline.
    rs_header-tdid     = 'ST'.
    rs_header-tdspras  = gc_english.
    rs_header-tdname   = 'ZABAPGIT_OFFLINE'.
    rs_header-tdobject = 'TEXT'.
  ENDMETHOD.                    "header_offline

  METHOD delete.

    DATA: lt_repos TYPE tt_repos_persi.


    lt_repos = list( ).

    DELETE lt_repos WHERE url = is_repo-url AND branch_name = is_repo-branch_name.
    IF sy-subrc <> 0.
      _raise 'repo not found, delete'.
    ENDIF.

    save_text_online( lt_repos ).
    save_text_offline( lt_repos ).

  ENDMETHOD.                    "delete

  METHOD save_text_online.

    DATA: lt_lines  TYPE TABLE OF tline.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF it_repos,
                   <ls_line> LIKE LINE OF lt_lines.


    LOOP AT it_repos ASSIGNING <ls_repo> WHERE offline = abap_false.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-url.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-branch_name.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-sha1.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-package.
    ENDLOOP.

    save_text( is_header = header_online( )
               it_lines  = lt_lines ).

    COMMIT WORK.

  ENDMETHOD.                    "save_text

  METHOD save_text_offline.

    DATA: lt_lines  TYPE TABLE OF tline.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF it_repos,
                   <ls_line> LIKE LINE OF lt_lines.


    LOOP AT it_repos ASSIGNING <ls_repo> WHERE offline = abap_true.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-url.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-package.
    ENDLOOP.

    save_text( is_header = header_offline( )
               it_lines  = lt_lines ).

    COMMIT WORK.

  ENDMETHOD.                    "save_text_offline

  METHOD validate_package.

    DATA: lv_devclass TYPE tdevc-devclass,
          lt_repos    TYPE tt_repos_persi.


    IF iv_package IS INITIAL.
      _raise 'add, package empty'.
    ENDIF.

    IF iv_package = '$TMP'.
      _raise 'not possible to use $TMP, create $FOO package'.
    ENDIF.

    SELECT SINGLE devclass FROM tdevc INTO lv_devclass
      WHERE devclass = iv_package
      AND as4user <> 'SAP'.                             "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'package not found or not allowed'.
    ENDIF.

* make sure its not already in use for a different repository
    lt_repos = list( ).
    READ TABLE lt_repos WITH KEY package = iv_package TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      _raise 'Package already in use'.
    ENDIF.

  ENDMETHOD.                    "validate_package

  METHOD add.

    DATA: lt_repos TYPE tt_repos_persi.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF lt_repos.


    IF iv_branch IS INITIAL AND iv_offline = abap_false.
      _raise 'add, sha empty'.
    ENDIF.

    validate_package( iv_package ).

    lt_repos = list( ).

    READ TABLE lt_repos WITH KEY url = is_repo-url branch_name = is_repo-branch_name
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      _raise 'already inserted'.
    ENDIF.

    APPEND INITIAL LINE TO lt_repos ASSIGNING <ls_repo>.
    <ls_repo>-url         = is_repo-url.
    <ls_repo>-branch_name = is_repo-branch_name.
    <ls_repo>-sha1        = iv_branch.
    <ls_repo>-package     = iv_package.
    <ls_repo>-offline     = iv_offline.

    save_text_online( lt_repos ).
    save_text_offline( lt_repos ).

  ENDMETHOD.                    "insert

  METHOD update.

    DATA: lt_repos TYPE tt_repos_persi.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF lt_repos.


    IF iv_branch IS INITIAL.
      _raise 'update, sha empty'.
    ENDIF.

    lt_repos = list( ).

    READ TABLE lt_repos ASSIGNING <ls_repo>
      WITH KEY url = is_repo-url branch_name = is_repo-branch_name.
    IF sy-subrc <> 0.
      _raise 'persist update, repo not found'.
    ENDIF.

    <ls_repo>-sha1 = iv_branch.

    save_text_online( lt_repos ).

  ENDMETHOD.                    "update

  METHOD list.
    CLEAR rt_repos.
    APPEND LINES OF read_text_online( ) TO rt_repos.
    APPEND LINES OF read_text_offline( ) TO rt_repos.
  ENDMETHOD.                    "list

  METHOD read_text.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = is_header-tdid
        language                = is_header-tdspras
        name                    = is_header-tdname
        object                  = is_header-tdobject
      TABLES
        lines                   = rt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.
    IF sy-subrc = 4.
      RETURN.
    ELSEIF sy-subrc <> 0.
      _raise 'Error from READ_TEXT'.
    ENDIF.

  ENDMETHOD.                    "read_text

  METHOD read_text_online.

    DATA: lt_lines TYPE TABLE OF tline,
          ls_repo  TYPE st_repo_persi.

    FIELD-SYMBOLS: <ls_line> LIKE LINE OF lt_lines.


    lt_lines = read_text( header_online( ) ).
    IF lines( lt_lines ) = 0.
      RETURN.
    ENDIF.

    IF lines( lt_lines ) MOD 4 <> 0.
* if this happens, delete text ZABAPGIT in SO10 or edit the text
* manually, so it contains the right information
      _raise 'Persistence, text broken'.
    ENDIF.

    CLEAR ls_repo.
    LOOP AT lt_lines ASSIGNING <ls_line>.
      IF <ls_line>-tdline IS INITIAL.
        _raise 'Persistence, text broken'.
      ENDIF.
      IF ls_repo-url IS INITIAL.
        ls_repo-url = <ls_line>-tdline.
        CONTINUE. " current loop
      ENDIF.
      IF ls_repo-branch_name IS INITIAL.
        ls_repo-branch_name = <ls_line>-tdline.
        CONTINUE. " current loop
      ENDIF.
      IF ls_repo-sha1 IS INITIAL.
        ls_repo-sha1 = <ls_line>-tdline.
        CONTINUE. " current loop
      ENDIF.

      ls_repo-package = <ls_line>-tdline.
      APPEND ls_repo TO rt_repos.
      CLEAR ls_repo.
    ENDLOOP.

  ENDMETHOD.                    "list

  METHOD read_text_offline.

    DATA: lt_lines TYPE TABLE OF tline,
          ls_repo  TYPE st_repo_persi.

    FIELD-SYMBOLS: <ls_line> LIKE LINE OF lt_lines.


    lt_lines = read_text( header_offline( ) ).
    IF lines( lt_lines ) = 0.
      RETURN.
    ENDIF.

    IF lines( lt_lines ) MOD 2 <> 0.
* if this happens, delete text ZABAPGIT in SO10 or edit the text
* manually, so it contains the right information
      _raise 'Persistence, text broken'.
    ENDIF.

    CLEAR ls_repo.
    LOOP AT lt_lines ASSIGNING <ls_line>.
      IF <ls_line>-tdline IS INITIAL.
        _raise 'Persistence, text broken'.
      ENDIF.
      IF ls_repo-url IS INITIAL.
        ls_repo-url = <ls_line>-tdline.
        CONTINUE. " current loop
      ENDIF.

      ls_repo-package = <ls_line>-tdline.
      ls_repo-offline = abap_true.
      APPEND ls_repo TO rt_repos.
      CLEAR ls_repo.
    ENDLOOP.

  ENDMETHOD.                    "list

ENDCLASS.                    "lcl_persistence IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_transport DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_transport DEFINITION FINAL.

  PUBLIC SECTION.
* remote to local
    CLASS-METHODS upload_pack
      IMPORTING is_repo   TYPE st_repo
      EXPORTING ev_pack   TYPE xstring
                ev_branch TYPE t_sha1
      RAISING   lcx_exception.

* local to remote
    CLASS-METHODS receive_pack
      IMPORTING is_repo   TYPE st_repo
                iv_commit TYPE t_sha1
                iv_pack   TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS branch_list
      IMPORTING iv_url         TYPE string
                iv_service     TYPE string DEFAULT 'upload'
      EXPORTING ei_client      TYPE REF TO if_http_client
                et_branch_list TYPE tt_branch_list
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS pkt_string
      IMPORTING iv_string     TYPE string
      RETURNING VALUE(rv_pkt) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS find_branch
      IMPORTING is_repo    TYPE st_repo
                iv_service TYPE string
      EXPORTING ei_client  TYPE REF TO if_http_client
                ev_branch  TYPE t_sha1
      RAISING   lcx_exception.

    CLASS-METHODS parse
      EXPORTING ev_pack TYPE xstring
      CHANGING  cv_data TYPE xstring.

    CLASS-METHODS length_utf8_hex
      IMPORTING iv_data       TYPE xstring
      RETURNING VALUE(rv_len) TYPE i.

    CLASS-METHODS parse_branch_list
      IMPORTING iv_data        TYPE string
      RETURNING VALUE(rt_list) TYPE tt_branch_list
      RAISING   lcx_exception.

    CLASS-METHODS set_headers
      IMPORTING is_repo    TYPE st_repo
                iv_service TYPE string
                ii_client  TYPE REF TO if_http_client
      RAISING   lcx_exception.

    CLASS-METHODS check_http_200
      IMPORTING ii_client TYPE REF TO if_http_client
      RAISING   lcx_exception.

    CLASS-METHODS get_null
      RETURNING VALUE(rv_c) TYPE char1.

ENDCLASS.                    "lcl_transport DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_transport IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_transport IMPLEMENTATION.

  METHOD set_headers.

    DATA: lv_value TYPE string.


    ii_client->request->set_header_field(
        name  = '~request_method'
        value = 'POST' ).

    lv_value = lcl_url=>path_name( is_repo-url ) && '.git/git-' && iv_service && '-pack'.
    ii_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_value ).

    lv_value = 'application/x-git-'
                  && iv_service && '-pack-request'.         "#EC NOTEXT
    ii_client->request->set_header_field(
        name  = 'Content-Type'
        value = lv_value ).                                 "#EC NOTEXT

    lv_value = 'application/x-git-'
                  && iv_service && '-pack-result'.          "#EC NOTEXT
    ii_client->request->set_header_field(
        name  = 'Accept'
        value = lv_value ).                                 "#EC NOTEXT

  ENDMETHOD.                    "set_headers

  METHOD get_null.

    DATA: lv_x(4) TYPE x VALUE '00000000',
          lv_z(2) TYPE c.

    FIELD-SYMBOLS <lv_y> TYPE c.


    ASSIGN lv_x TO <lv_y> CASTING.
    lv_z = <lv_y>.
    rv_c = lv_z(1).

  ENDMETHOD.                    "get_null

  METHOD check_http_200.

    DATA: lv_code TYPE i.


    ii_client->response->get_status(
      IMPORTING
        code   = lv_code ).
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 302.
        _raise 'HTTP redirect, check URL'.
      WHEN 401.
        _raise 'HTTP 401, unauthorized'.
      WHEN 403.
        _raise 'HTTP 403, forbidden'.
      WHEN 404.
        _raise 'HTTP 404, not found'.
      WHEN 415.
        _raise 'HTTP 415, unsupported media type'.
      WHEN OTHERS.
        _raise 'HTTP error code'.
    ENDCASE.

  ENDMETHOD.                                                "http_200

  METHOD parse_branch_list.

    DATA: lt_result TYPE TABLE OF string,
          lv_hash   TYPE t_sha1,
          lv_name   TYPE string,
          lv_foo    TYPE string ##NEEDED,
          lv_char   TYPE c,
          lv_data   LIKE LINE OF lt_result.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF rt_list.


    SPLIT iv_data AT gc_newline INTO TABLE lt_result.
    LOOP AT lt_result INTO lv_data.
      IF sy-tabix = 1.
        CONTINUE. " current loop
      ELSEIF sy-tabix = 2 AND strlen( lv_data ) > 49.
        lv_hash = lv_data+8.
        lv_name = lv_data+49.
        lv_char = get_null( ).
        SPLIT lv_name AT lv_char INTO lv_name lv_foo.
      ELSEIF sy-tabix > 2 AND strlen( lv_data ) > 45.
        lv_hash = lv_data+4.
        lv_name = lv_data+45.
      ELSEIF sy-tabix = 2 AND strlen( lv_data ) = 8 AND lv_data(8) = '00000000'.
        _raise 'No branches, create branch manually by adding file'.
      ELSE.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO rt_list ASSIGNING <ls_branch>.
      <ls_branch>-sha1 = lv_hash.
      <ls_branch>-name = lv_name.
    ENDLOOP.

  ENDMETHOD.

  METHOD find_branch.

    DATA: lt_branch_list TYPE tt_branch_list,
          ls_branch_list LIKE LINE OF lt_branch_list.


    branch_list(
      EXPORTING
        iv_url          = is_repo-url
        iv_service      = iv_service
      IMPORTING
        ei_client       = ei_client
        et_branch_list  = lt_branch_list ).

    IF is_repo-branch_name IS INITIAL.
      _raise 'branch empty'.
    ENDIF.

    READ TABLE lt_branch_list INTO ls_branch_list WITH KEY name = is_repo-branch_name.
    IF sy-subrc <> 0.
      _raise 'Branch not found'.
    ENDIF.

    ev_branch = ls_branch_list-sha1.

  ENDMETHOD.

  METHOD branch_list.

    DATA: lv_data TYPE string,
          lv_uri  TYPE string,
          lv_text TYPE string.

    STATICS: sv_authorization TYPE string.


    cl_http_client=>create_by_url(
      EXPORTING
        url    = lcl_url=>host( iv_url )
        ssl_id = 'ANONYM'
      IMPORTING
        client = ei_client ).

    ei_client->request->set_cdata( '' ).
    ei_client->request->set_header_field(
        name  = '~request_method'
        value = 'GET' ).
    ei_client->request->set_header_field(
        name  = 'user-agent'
        value = gv_agent ).                                 "#EC NOTEXT
    lv_uri = lcl_url=>path_name( iv_url ) &&
             '.git/info/refs?service=git-' &&
             iv_service &&
             '-pack'.
    ei_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_uri ).
    IF NOT sv_authorization IS INITIAL.
* note this will only work if all repositories uses the same login
      ei_client->request->set_header_field(
          name  = 'authorization'
          value = sv_authorization ).                       "#EC NOTEXT
      ei_client->propertytype_logon_popup = ei_client->co_disabled.
    ENDIF.
    ei_client->send( ).
    ei_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
* make sure:
* a) SSL is setup properly in STRUST
* b) no firewalls
* check trace file in transaction SMICM
          lv_text = 'HTTP Communication Failure'.           "#EC NOTEXT
        WHEN 2.
          lv_text = 'HTTP Invalid State'.                   "#EC NOTEXT
        WHEN 3.
          lv_text = 'HTTP Processing failed'.               "#EC NOTEXT
        WHEN OTHERS.
          lv_text = 'Another error occured'.                "#EC NOTEXT
      ENDCASE.
      RAISE EXCEPTION TYPE lcx_exception
        EXPORTING
          iv_text = lv_text.
    ENDIF.

    check_http_200( ei_client ).
    sv_authorization = ei_client->request->get_header_field(
                                                  'authorization' ). "#EC NOTEXT

    lv_data = ei_client->response->get_cdata( ).
    et_branch_list = parse_branch_list( lv_data ).

  ENDMETHOD.                    "ref_discovery

  METHOD receive_pack.

    CONSTANTS: lc_service TYPE string VALUE 'receive'.      "#EC NOTEXT

    DATA: li_client  TYPE REF TO if_http_client,
          lv_cmd_pkt TYPE string,
          lv_line    TYPE string,
          lv_tmp     TYPE xstring,
          lv_xstring TYPE xstring,
          lv_string  TYPE string,
          lv_buffer  TYPE string,
          lv_branch  TYPE t_sha1.


    find_branch(
      EXPORTING
        is_repo    = is_repo
        iv_service = lc_service
      IMPORTING
        ei_client  = li_client
        ev_branch  = lv_branch ).

    set_headers(
        is_repo    = is_repo
        iv_service = lc_service
        ii_client  = li_client ).

    lv_line = lv_branch &&
              ` ` &&
              iv_commit &&
              ` ` &&
              is_repo-branch_name &&
              get_null( ) &&
              ` ` &&
              'report-status agent=' && gv_agent &&
              gc_newline.                                   "#EC NOTEXT
    lv_cmd_pkt = pkt_string( lv_line ).

    lv_buffer = lv_cmd_pkt && '0000'.
    lv_tmp = lcl_convert=>string_to_xstring_utf8( lv_buffer ).

    CONCATENATE lv_tmp iv_pack INTO lv_xstring IN BYTE MODE.

    li_client->request->set_data( lv_xstring ).

    li_client->send( ).
    li_client->receive( ).
    check_http_200( li_client ).

    lv_xstring = li_client->response->get_data( ).
    li_client->close( ).

    lv_string = lcl_convert=>xstring_to_string_utf8( lv_xstring ).
    IF NOT lv_string CP '*unpack ok*'.
      _raise 'unpack not ok'.
    ELSEIF lv_string CP '*pre-receive hook declined*'.
      _raise 'pre-receive hook declined'.
    ENDIF.

  ENDMETHOD.                    "receive_pack

  METHOD length_utf8_hex.

    DATA: lv_xstring TYPE xstring,
          lv_string  TYPE string,
          lv_char4   TYPE c LENGTH 4,
          lv_x       TYPE x LENGTH 2,
          lo_obj     TYPE REF TO cl_abap_conv_in_ce,
          lv_len     TYPE int4.

* hmm, can this be done easier?

    lv_xstring = iv_data(4).

    lo_obj = cl_abap_conv_in_ce=>create(
        input    = lv_xstring
        encoding = 'UTF-8' ).
    lv_len = xstrlen( lv_xstring ).

    lo_obj->read( EXPORTING n    = lv_len
                  IMPORTING data = lv_string ).

    lv_char4 = lv_string.
    TRANSLATE lv_char4 TO UPPER CASE.
    lv_x = lv_char4.
    rv_len = lv_x.

  ENDMETHOD.                    "length_utf8_hex

  METHOD parse.

    CONSTANTS: lc_band1 TYPE x VALUE '01'.

    DATA: lv_len      TYPE i,
          lv_contents TYPE xstring,
          lv_pack     TYPE xstring.


    WHILE xstrlen( cv_data ) >= 4.
      lv_len = length_utf8_hex( cv_data ).

      lv_contents = cv_data(lv_len).
      IF lv_len = 0.
        cv_data = cv_data+4.
        CONTINUE.
      ELSE.
        cv_data = cv_data+lv_len.
      ENDIF.

      lv_contents = lv_contents+4.

      IF xstrlen( lv_contents ) > 1 AND lv_contents(1) = lc_band1.
        CONCATENATE lv_pack lv_contents+1 INTO lv_pack IN BYTE MODE.
      ENDIF.

    ENDWHILE.

    ev_pack = lv_pack.

  ENDMETHOD.                    "parse

  METHOD upload_pack.

    CONSTANTS: lc_service TYPE string VALUE 'upload'.       "#EC NOTEXT

    DATA: li_client  TYPE REF TO if_http_client,
          lv_buffer  TYPE string,
          lv_xstring TYPE xstring,
          lv_line    TYPE string,
          lv_pkt1    TYPE string,
          lv_pkt2    TYPE string.


    find_branch(
      EXPORTING
        is_repo    = is_repo
        iv_service = lc_service
      IMPORTING
        ei_client  = li_client
        ev_branch  = ev_branch ).

    set_headers(
        is_repo    = is_repo
        iv_service = lc_service
        ii_client  = li_client ).

    lv_line = 'want' &&
              ` ` &&
              ev_branch &&
              ` ` &&
              'side-band-64k no-progress agent=' && gv_agent
              && gc_newline.                                "#EC NOTEXT
    lv_pkt1 = pkt_string( lv_line ).

    lv_pkt2 = pkt_string( 'deepen 1' && gc_newline ).       "#EC NOTEXT

    lv_buffer = lv_pkt1
             && lv_pkt2
             && '0000'
             && '0009done' && gc_newline.

* do not use set_cdata as it modifies the Content-Type header field
    li_client->request->set_data( lcl_convert=>string_to_xstring_utf8( lv_buffer ) ).
    li_client->send( ).
    li_client->receive( ).
    check_http_200( li_client ).
    lv_xstring = li_client->response->get_data( ).
    li_client->close( ).

    parse( IMPORTING ev_pack = ev_pack
           CHANGING cv_data = lv_xstring ).

  ENDMETHOD.                    "upload_pack

  METHOD pkt_string.

    DATA: lv_x   TYPE x,
          lv_len TYPE i.


    lv_len = strlen( iv_string ).

    IF lv_len >= 255.
      _raise 'PKT, todo'.
    ENDIF.

    lv_x = lv_len + 4.

    rv_pkt = rv_pkt && '00' && lv_x && iv_string.

  ENDMETHOD.                    "pkt

ENDCLASS.                    "lcl_transport IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_zip DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_zip DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS import
      IMPORTING is_repo TYPE st_repo_persi
      RAISING   lcx_exception.

    CLASS-METHODS export
      IMPORTING is_repo TYPE st_repo_persi
                iv_zip  TYPE abap_bool DEFAULT abap_true
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS show_progress
      IMPORTING iv_current  TYPE i
                iv_total    TYPE i
                iv_obj_name TYPE tadir-obj_name.

    CLASS-METHODS file_upload
      RETURNING VALUE(rv_xstr) TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS decode_files
      IMPORTING iv_xstr         TYPE xstring
      RETURNING VALUE(rt_files) TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS filename
      IMPORTING iv_str             TYPE string
      RETURNING VALUE(rv_filename) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS file_download
      IMPORTING is_repo TYPE st_repo_persi
                iv_xstr TYPE xstring
      RAISING   lcx_exception.

    CLASS-METHODS files_commit
      IMPORTING it_files TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS encode_files
      IMPORTING it_files       TYPE tt_files
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

  METHOD show_progress.

    DATA: lv_pct TYPE i,
          lv_f   TYPE f.


    lv_f = ( iv_current / iv_total ) * 100.
    lv_pct = lv_f.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_pct
        text       = iv_obj_name.

  ENDMETHOD.

  METHOD get_message.

    DATA: lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname = 'ABAPTXT255'.
    <ls_field>-fieldname = 'LINE'.
    <ls_field>-fieldtext = 'Commit message'.                "#EC NOTEXT
    <ls_field>-field_obl = abap_true.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Enter commit message'    "#EC NOTEXT
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

  ENDMETHOD.

  METHOD file_download.

    DATA: lt_rawdata  TYPE solix_tab,
          lv_action   TYPE i,
          lv_filename TYPE string,
          lv_default  TYPE string,
          lv_path     TYPE string,
          lv_fullpath TYPE string.


    CONCATENATE is_repo-url '_' sy-datlo '_' sy-timlo INTO lv_default.

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
      CONCATENATE <ls_file>-path <ls_file>-filename INTO lv_filename.
      lo_zip->add( name    = lv_filename
                   content = <ls_file>-data ).
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

  METHOD decode_files.

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

  METHOD import.

    DATA: lt_files TYPE tt_files.


    lt_files = decode_files( file_upload( ) ).

    lcl_objects=>deserialize( it_files   = lt_files
                              iv_package = is_repo-package ).

  ENDMETHOD.                    "import

  METHOD export.

    DATA: lt_tadir TYPE tt_tadir,
          ls_item  TYPE st_item,
          lt_files TYPE tt_files,
          lt_zip   TYPE tt_files.

    FIELD-SYMBOLS: <ls_file>  LIKE LINE OF lt_files,
                   <ls_tadir> LIKE LINE OF lt_tadir.


    lt_tadir = lcl_tadir=>read( is_repo-package ).

    IF lt_tadir IS INITIAL.
      _raise 'Package is empty'.
    ENDIF.

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      show_progress( iv_current  = sy-tabix
                     iv_total    = lines( lt_tadir )
                     iv_obj_name = <ls_tadir>-obj_name ).

      CLEAR ls_item.
      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      lt_files = lcl_objects=>serialize( ls_item ).

      LOOP AT lt_files ASSIGNING <ls_file>.
        <ls_file>-path = <ls_tadir>-path.
      ENDLOOP.

      APPEND LINES OF lt_files TO lt_zip.
    ENDLOOP.

    IF iv_zip = abap_true.
      file_download( is_repo = is_repo
                     iv_xstr = encode_files( lt_zip ) ).
    ELSE.
      files_commit( lt_zip ).
    ENDIF.

  ENDMETHOD.                    "export

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
      lt_rawdata = cl_bcs_convert=>xstring_to_solix( <ls_file>-data ).

      CONCATENATE lv_folder <ls_file>-path <ls_file>-filename INTO lv_filename.

      cl_gui_frontend_services=>gui_download(
        EXPORTING
          bin_filesize            = xstrlen( <ls_file>-data )
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

  ENDMETHOD.

ENDCLASS.                    "lcl_zip IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_porcelain DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_porcelain DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS pull
      IMPORTING is_repo    TYPE st_repo
      EXPORTING et_files   TYPE tt_files
                et_objects TYPE tt_objects
                ev_branch  TYPE t_sha1
      RAISING   lcx_exception.

    CLASS-METHODS push
      IMPORTING is_repo          TYPE st_repo
                is_comment       TYPE st_comment
                it_files         TYPE tt_files
      RETURNING VALUE(rv_branch) TYPE t_sha1
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS walk
      IMPORTING it_objects TYPE tt_objects
                iv_sha1    TYPE t_sha1
                iv_path    TYPE string
      CHANGING  ct_files   TYPE tt_files
      RAISING   lcx_exception.

    CLASS-METHODS root_tree
      IMPORTING it_objects      TYPE tt_objects
                iv_branch       TYPE t_sha1
      RETURNING VALUE(rt_nodes) TYPE tt_nodes
      RAISING   lcx_exception.

    CLASS-METHODS receive_pack
      IMPORTING is_comment       TYPE st_comment
                is_repo          TYPE st_repo
                it_nodes         TYPE tt_nodes
                it_files         TYPE tt_files
                iv_branch        TYPE t_sha1
      RETURNING VALUE(rv_branch) TYPE t_sha1
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_porcelain DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_porcelain IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_porcelain IMPLEMENTATION.

  METHOD receive_pack.

    DATA: lv_tree    TYPE xstring,
          lv_time    TYPE t_unixtime,
          lv_commit  TYPE xstring,
          lt_objects TYPE tt_objects,
          lv_pack    TYPE xstring,
          ls_object  LIKE LINE OF lt_objects,
          ls_commit  TYPE st_commit.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files.


    lv_tree = lcl_pack=>encode_tree( it_nodes ).

* new commit
    lv_time = lcl_time=>get( ).
    ls_commit-tree      = lcl_hash=>sha1( iv_type = gc_tree iv_data = lv_tree ).
    ls_commit-parent    = iv_branch.
    CONCATENATE is_comment-username space '<' is_comment-email '>' space lv_time
      INTO ls_commit-author RESPECTING BLANKS.
    ls_commit-committer = ls_commit-author.
    ls_commit-body      = is_comment-comment.
    lv_commit = lcl_pack=>encode_commit( ls_commit ).


    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_commit iv_data = lv_commit ).
    ls_object-type = gc_commit.
    ls_object-data = lv_commit.
    APPEND ls_object TO lt_objects.
    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_tree iv_data = lv_tree ).
    ls_object-type = gc_tree.
    ls_object-data = lv_tree.
    APPEND ls_object TO lt_objects.
    LOOP AT it_files ASSIGNING <ls_file>.
      CLEAR ls_object.
      ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_blob iv_data = <ls_file>-data ).
      ls_object-type = gc_blob.
      ls_object-data = <ls_file>-data.
      APPEND ls_object TO lt_objects.
    ENDLOOP.

    lv_pack = lcl_pack=>encode( lt_objects ).

    rv_branch = lcl_hash=>sha1( iv_type = gc_commit iv_data = lv_commit ).
    lcl_transport=>receive_pack( is_repo   = is_repo
                                 iv_commit = rv_branch
                                 iv_pack   = lv_pack ).

  ENDMETHOD.                    "receive_pack

  METHOD push.

* todo, only works with root files

    DATA: lt_objects TYPE tt_objects,
          lt_nodes   TYPE tt_nodes,
          lt_files   LIKE it_files,
          lv_sha1    TYPE t_sha1,
          lv_index   TYPE i,
          lv_branch  TYPE t_sha1.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files,
                   <ls_node> LIKE LINE OF lt_nodes.


    lcl_porcelain=>pull( EXPORTING is_repo    = is_repo
                         IMPORTING et_objects = lt_objects
                                   ev_branch  = lv_branch ).

    lt_nodes = root_tree( it_objects = lt_objects
                          iv_branch  = lv_branch ).

    lt_files[] = it_files[].

    LOOP AT lt_files ASSIGNING <ls_file>.
      lv_index = sy-tabix.
      READ TABLE lt_nodes ASSIGNING <ls_node> WITH KEY name = <ls_file>-filename.
      IF sy-subrc <> 0.
* new files
        APPEND INITIAL LINE TO lt_nodes ASSIGNING <ls_node>.
        <ls_node>-chmod = gc_chmod-file.
        <ls_node>-name = <ls_file>-filename.
      ENDIF.

      lv_sha1 = lcl_hash=>sha1( iv_type = gc_blob iv_data = <ls_file>-data ).
      IF <ls_node>-sha1 <> lv_sha1.
        <ls_node>-sha1 = lv_sha1.
      ELSE.
        DELETE lt_files INDEX lv_index.
      ENDIF.
    ENDLOOP.

    IF lt_files[] IS INITIAL.
      _raise 'no files'.
    ENDIF.

    rv_branch = receive_pack( is_comment = is_comment
                              is_repo    = is_repo
                              it_nodes   = lt_nodes
                              it_files   = lt_files
                              iv_branch  = lv_branch ).

  ENDMETHOD.                    "push

  METHOD root_tree.

    DATA: ls_object LIKE LINE OF it_objects,
          ls_commit TYPE st_commit.


    READ TABLE it_objects INTO ls_object WITH KEY sha1 = iv_branch type = gc_commit.
    IF sy-subrc <> 0.
      _raise 'commit not found'.
    ENDIF.
    ls_commit = lcl_pack=>decode_commit( ls_object-data ).

    READ TABLE it_objects INTO ls_object WITH KEY sha1 = ls_commit-tree type = gc_tree.
    IF sy-subrc <> 0.
      _raise 'tree not found'.
    ENDIF.
    rt_nodes = lcl_pack=>decode_tree( ls_object-data ).

  ENDMETHOD.                    "root_tree

  METHOD pull.

    DATA: ls_object LIKE LINE OF et_objects,
          ls_commit TYPE st_commit,
          lv_pack   TYPE xstring.


    lcl_transport=>upload_pack( EXPORTING is_repo = is_repo
                                IMPORTING ev_pack = lv_pack
                                          ev_branch = ev_branch ).

    IF lv_pack IS INITIAL.
      _raise 'empty pack'.
    ENDIF.

    et_objects = lcl_pack=>decode( lv_pack ).
    lcl_debug=>render_objects( iv_message = 'Before deltas'
                               it_objects = et_objects ).   "#EC NOTEXT

    lcl_pack=>decode_deltas( CHANGING ct_objects = et_objects ).
    lcl_debug=>render_objects( iv_message = 'After deltas'
                               it_objects = et_objects ).   "#EC NOTEXT

    READ TABLE et_objects INTO ls_object WITH KEY sha1 = ev_branch type = gc_commit.
    IF sy-subrc <> 0.
      _raise 'Commit/branch not found'.
    ENDIF.
    ls_commit = lcl_pack=>decode_commit( ls_object-data ).

    walk( EXPORTING it_objects = et_objects
                    iv_sha1 = ls_commit-tree
                    iv_path = '/'
          CHANGING ct_files = et_files ).

  ENDMETHOD.                    "pull

  METHOD walk.

    DATA: lv_path  TYPE string,
          ls_file  LIKE LINE OF ct_files,
          lt_nodes TYPE tt_nodes.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF it_objects,
                   <ls_blob> LIKE LINE OF it_objects,
                   <ls_node> LIKE LINE OF lt_nodes.


    READ TABLE it_objects ASSIGNING <ls_tree> WITH KEY sha1 = iv_sha1 type = gc_tree.
    IF sy-subrc <> 0.
      _raise 'Walk, tree not found'.
    ENDIF.

    lt_nodes = lcl_pack=>decode_tree( <ls_tree>-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      IF <ls_node>-chmod = gc_chmod-file.
        READ TABLE it_objects ASSIGNING <ls_blob>
          WITH KEY sha1 = <ls_node>-sha1 type = gc_blob.
        IF sy-subrc <> 0.
          _raise 'Walk, blob not found'.
        ENDIF.

        CLEAR ls_file.
        ls_file-path     = iv_path.
        ls_file-filename = <ls_node>-name.
        ls_file-data     = <ls_blob>-data.
        APPEND ls_file TO ct_files.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_nodes ASSIGNING <ls_node> WHERE chmod = gc_chmod-dir.
      CONCATENATE iv_path <ls_node>-name '/' INTO lv_path.
      walk( EXPORTING it_objects = it_objects
                      iv_sha1 = <ls_node>-sha1
                      iv_path = lv_path
            CHANGING ct_files = ct_files ).
    ENDLOOP.

  ENDMETHOD.                    "walk

ENDCLASS.                    "lcl_porcelain IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS run
      RAISING lcx_exception.

    CLASS-METHODS on_event
                  FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING action frame getdata postdata query_table.  "#EC NEEDED

  PRIVATE SECTION.
    CLASS-DATA go_html_viewer TYPE REF TO cl_gui_html_viewer.

    CLASS-METHODS view
      IMPORTING iv_html TYPE string.

    CLASS-METHODS render
      RETURNING VALUE(rv_html) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS render_css
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS render_repo_online
      IMPORTING is_repo_persi  TYPE st_repo_persi
      RETURNING VALUE(rv_html) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS render_repo_offline
      IMPORTING is_repo_persi  TYPE st_repo_persi
      RETURNING VALUE(rv_html) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS repo_name
      IMPORTING is_repo        TYPE st_repo_persi
      RETURNING VALUE(rv_name) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS render_header
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS render_menu
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS render_footer
      RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS install
      IMPORTING iv_url TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS newoffline
      RAISING lcx_exception.

    CLASS-METHODS add
      IMPORTING is_item       TYPE st_item
                is_repo_persi TYPE st_repo_persi
      RAISING   lcx_exception.

    CLASS-METHODS uninstall
      IMPORTING is_repo TYPE st_repo_persi
      RAISING   lcx_exception.

    CLASS-METHODS remove
      IMPORTING is_repo TYPE st_repo_persi
      RAISING   lcx_exception.

    CLASS-METHODS pull
      IMPORTING is_repo_persi TYPE st_repo_persi
      RAISING   lcx_exception.

    CLASS-METHODS commit
      IMPORTING is_repo TYPE st_repo
      RAISING   lcx_exception.

    CLASS-METHODS diff
      IMPORTING is_result TYPE st_result
                is_repo   TYPE st_repo
      RAISING   lcx_exception.

    CLASS-METHODS render_diff
      IMPORTING is_result TYPE st_result
                it_diffs  TYPE tt_diffs.

    CLASS-METHODS struct_encode
      IMPORTING ig_structure1    TYPE any
                ig_structure2    TYPE any OPTIONAL
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS struct_decode
      IMPORTING iv_string    TYPE clike
      CHANGING  cg_structure TYPE any
      RAISING   lcx_exception.

    CLASS-METHODS popup_comment
      RETURNING VALUE(rs_comment) TYPE st_comment
      RAISING   lcx_exception.

    CLASS-METHODS get_logo_src
      RETURNING VALUE(rv_src) TYPE string.

    CLASS-METHODS zipexport
      RAISING lcx_exception.

ENDCLASS.                    "lcl_gui DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui IMPLEMENTATION.

  METHOD zipexport.

    DATA: lv_returncode TYPE c,
          ls_repo_persi TYPE st_repo_persi,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'TDEVC'.
    <ls_field>-fieldname = 'DEVCLASS'.
    <ls_field>-fieldtext = 'Package'.                       "#EC NOTEXT

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Export package to ZIP'                           "#EC NOTEXT
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
    ls_repo_persi-package = <ls_field>-value.
    TRANSLATE ls_repo_persi-package TO UPPER CASE.
    ls_repo_persi-url = ls_repo_persi-package.

    lcl_zip=>export( ls_repo_persi ).

  ENDMETHOD.

  METHOD render_header.

    rv_html = '<html>'
          && gc_newline &&
          '<head>'
          && gc_newline &&
          '<title>abapGit</title>'
          && gc_newline &&
          render_css( )
          && gc_newline &&
          '<meta http-equiv="content-type" content="text/html; charset=utf-8">'
          && gc_newline &&
          '<script>'
          && gc_newline &&
          'function goBack() {'
          && gc_newline &&
          '  window.history.back();'
          && gc_newline &&
          '}'
          && gc_newline &&
          '</script>'
          && gc_newline &&
          '</head>'
          && gc_newline &&
          '<body style="background: rgba(222, 241, 242, 1);">'
          && gc_newline.                                    "#EC NOTEXT

  ENDMETHOD.                    "render_head

  METHOD diff.

    DATA: lt_remote TYPE tt_files,
          lt_local  TYPE tt_files,
          ls_item   TYPE st_item,
          lt_diffs  TYPE tt_diffs.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF lt_remote,
                   <ls_local>  LIKE LINE OF lt_local.


    lcl_porcelain=>pull( EXPORTING is_repo  = is_repo
                         IMPORTING et_files = lt_remote ).

    CLEAR ls_item.
    ls_item-obj_type = is_result-obj_type.
    ls_item-obj_name = is_result-obj_name.

    lt_local = lcl_objects=>serialize( ls_item ).

    READ TABLE lt_remote ASSIGNING <ls_remote>
      WITH KEY filename = is_result-filename.
    IF sy-subrc <> 0.
      _raise 'not found remotely'.
    ENDIF.
    READ TABLE lt_local ASSIGNING <ls_local>
      WITH KEY filename = is_result-filename.
    IF sy-subrc <> 0.
      _raise 'not found locally'.
    ENDIF.

    lt_diffs = lcl_diff=>diff( iv_local  = <ls_local>-data
                               iv_remote = <ls_remote>-data ).

    render_diff( is_result = is_result
                 it_diffs  = lt_diffs ).

  ENDMETHOD.                    "diff

  METHOD render_diff.

    DATA: lv_html    TYPE string,
          lv_local   TYPE string,
          lv_remote  TYPE string,
          lv_clocal  TYPE string,
          lv_cremote TYPE string.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF it_diffs.


    lv_html = render_header( ) &&
              '<h1>diff</h1>&nbsp;<a href="javascript:goBack()">Back</a>' &&
              '<hr><h3>' &&
              is_result-obj_type && '&nbsp;' &&
              is_result-obj_name && '&nbsp;' &&
              is_result-filename && '</h3><br><br>'.

    lv_html = lv_html &&
              '<table border="0">'       && gc_newline &&
              '<tr>'                     && gc_newline &&
              '<td><h2>Local</h2></td>'  && gc_newline &&
              '<td></td>'                && gc_newline &&
              '<td><h2>Remote</h2></td>' && gc_newline &&
              '</tr>'.

    LOOP AT it_diffs ASSIGNING <ls_diff>.
      lv_local = escape( val = <ls_diff>-local format = cl_abap_format=>e_html_attr ).
      lv_remote = escape( val = <ls_diff>-remote format = cl_abap_format=>e_html_attr ).

      CASE <ls_diff>-result.
        WHEN gc_diff-insert.
          lv_clocal = ' style="background:lightgreen;"'.    "#EC NOTEXT
          lv_cremote = ''.
        WHEN gc_diff-delete.
          lv_clocal = ''.
          lv_cremote = ' style="background:lightpink;"'.    "#EC NOTEXT
        WHEN gc_diff-update.
          lv_clocal = ' style="background:lightgreen;"'.    "#EC NOTEXT
          lv_cremote = ' style="background:lightpink;"'.    "#EC NOTEXT
        WHEN OTHERS.
          lv_clocal = ''.
          lv_cremote = ''.
      ENDCASE.

      lv_html = lv_html &&
        '<tr>' && gc_newline &&
        '<td' && lv_clocal && '><pre>' && lv_local && '</pre></td>' &&
        gc_newline &&
        '<td>&nbsp;' && <ls_diff>-result && '&nbsp;</td>' &&
        gc_newline &&
        '<td' && lv_cremote && '><pre>' && lv_remote && '</pre></td>' &&
        gc_newline &&
        '</tr>' && gc_newline.
    ENDLOOP.
    lv_html = lv_html && '</table>' && gc_newline.

    lv_html = lv_html && render_footer( ).
    view( lv_html ).

  ENDMETHOD.                    "render_diff

  METHOD popup_comment.

    DATA: lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname = 'BAPIRTEXT'.
    <ls_field>-fieldname = 'TEXT'.
    <ls_field>-fieldtext = 'Username'.                      "#EC NOTEXT
    <ls_field>-field_obl = abap_true.
    <ls_field>-value = lcl_user=>get_username( ).

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname = 'BAPIRTEXT1'.
    <ls_field>-fieldname = 'TEXT'.
    <ls_field>-fieldtext = 'E-Mail'.                        "#EC NOTEXT
    <ls_field>-field_obl = abap_true.
    <ls_field>-value = lcl_user=>get_email( ).

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname = 'ABAPTXT255'.
    <ls_field>-fieldname = 'LINE'.
    <ls_field>-fieldtext = 'Comment'.                       "#EC NOTEXT
    <ls_field>-field_obl = abap_true.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Enter Git username and email'    "#EC NOTEXT
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
      CLEAR rs_comment.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_comment-username = <ls_field>-value.
    lcl_user=>set_username( rs_comment-username ).

    READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_comment-email = <ls_field>-value.
    lcl_user=>set_email( rs_comment-email ).

    READ TABLE lt_fields INDEX 3 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_comment-comment = <ls_field>-value.

  ENDMETHOD.                    "popup_commit

  METHOD pull.

    DATA: lt_files  TYPE tt_files,
          ls_repo   TYPE st_repo,
          lv_branch TYPE t_sha1.


    MOVE-CORRESPONDING is_repo_persi TO ls_repo.

    lcl_porcelain=>pull( EXPORTING is_repo   = ls_repo
                         IMPORTING et_files  = lt_files
                                   ev_branch = lv_branch ).

    lcl_objects=>deserialize( it_files   = lt_files
                              iv_package = is_repo_persi-package ).

    lcl_persistence=>update( is_repo   = ls_repo
                             iv_branch = lv_branch ).

    view( render( ) ).

  ENDMETHOD.                    "pull

  METHOD commit.

    DATA: lv_branch  TYPE t_sha1,
          lt_results TYPE tt_results,
          lt_push    TYPE tt_files,
          ls_item    TYPE st_item,
          ls_comment TYPE st_comment,
          lt_files   TYPE tt_files.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results.


    lcl_porcelain=>pull( EXPORTING is_repo   = is_repo
                         IMPORTING et_files  = lt_files ).

    lt_results = lcl_objects=>status( lt_files ).

    CLEAR lt_files[].
    LOOP AT lt_results ASSIGNING <ls_result> WHERE match = abap_false.
      CLEAR ls_item.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.

      lt_files = lcl_objects=>serialize( ls_item ).
      APPEND LINES OF lt_files TO lt_push.
    ENDLOOP.

    IF lt_push[] IS INITIAL.
      _raise 'no changes'.
    ENDIF.

    ls_comment = popup_comment( ).
    IF ls_comment IS INITIAL.
      RETURN.
    ENDIF.

    lv_branch = lcl_porcelain=>push(
      is_comment     = ls_comment
      is_repo        = is_repo
      it_files       = lt_push ).

    lcl_persistence=>update( is_repo   = is_repo
                             iv_branch = lv_branch ).

    view( render( ) ).

  ENDMETHOD.                    "commit

  METHOD struct_decode.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields,
                   <lg_any>   TYPE any.


    lv_string = iv_string.     " type conversion
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    LOOP AT lt_fields ASSIGNING <ls_field>.
      ASSIGN COMPONENT <ls_field>-name OF STRUCTURE cg_structure TO <lg_any>.
      IF sy-subrc <> 0.
        CONTINUE. " more structures might be encoded in same string
      ENDIF.

      <lg_any> = <ls_field>-value.
    ENDLOOP.

  ENDMETHOD.                    "struct_decode

  METHOD struct_encode.

    DATA: lt_fields    TYPE tihttpnvp,
          lo_descr_ref TYPE REF TO cl_abap_structdescr,
          ls_field     LIKE LINE OF lt_fields.

    FIELD-SYMBOLS: <ls_comp> LIKE LINE OF lo_descr_ref->components,
                   <lg_any>  TYPE any.


    lo_descr_ref ?= cl_abap_typedescr=>describe_by_data( ig_structure1 ).

    LOOP AT lo_descr_ref->components ASSIGNING <ls_comp>.

      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE ig_structure1 TO <lg_any>.
      ASSERT sy-subrc = 0.

      ls_field-name = <ls_comp>-name.
      ls_field-value = <lg_any>.
      APPEND ls_field TO lt_fields.
    ENDLOOP.

    IF ig_structure2 IS SUPPLIED.
      lo_descr_ref ?= cl_abap_typedescr=>describe_by_data( ig_structure2 ).

      LOOP AT lo_descr_ref->components ASSIGNING <ls_comp>.

        ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE ig_structure2 TO <lg_any>.
        ASSERT sy-subrc = 0.

        ls_field-name = <ls_comp>-name.
        ls_field-value = <lg_any>.
        APPEND ls_field TO lt_fields.
      ENDLOOP.
    ENDIF.

    rv_string = cl_http_utility=>if_http_utility~fields_to_string( lt_fields ).

  ENDMETHOD.                    "encode_struct

  METHOD on_event.

    DATA: lx_exception  TYPE REF TO lcx_exception,
          ls_result     TYPE st_result,
          lv_url        TYPE string,
          ls_repo       TYPE st_repo,
          ls_item       TYPE st_item,
          ls_repo_persi TYPE st_repo_persi.


    TRY.
        CASE action.
          WHEN 'install'.
            lv_url = getdata.
            install( lv_url ).
          WHEN 'explore'.
            go_html_viewer->show_url( 'http://larshp.github.io/abapGit/explore.html' ).
          WHEN 'abapgithome'.
            cl_gui_frontend_services=>execute(
                 document = 'https://github.com/larshp/abapGit' ).
          WHEN 'add'.
            struct_decode( EXPORTING iv_string = getdata
                           CHANGING cg_structure = ls_result ).
            struct_decode( EXPORTING iv_string = getdata
                           CHANGING cg_structure = ls_repo_persi ).
            CLEAR ls_item.
            MOVE-CORRESPONDING ls_result TO ls_item.
            add( is_item       = ls_item
                 is_repo_persi = ls_repo_persi ).
          WHEN 'uninstall'.
            struct_decode( EXPORTING iv_string = getdata
                           CHANGING cg_structure = ls_repo_persi ).
            uninstall( ls_repo_persi ).
          WHEN 'remove'.
            struct_decode( EXPORTING iv_string = getdata
                           CHANGING cg_structure = ls_repo_persi ).
            remove( ls_repo_persi ).
          WHEN 'refresh'.
            view( render( ) ).
          WHEN 'commit'.
            struct_decode( EXPORTING iv_string = getdata
                           CHANGING cg_structure = ls_repo ).
            commit( ls_repo ).
          WHEN 'diff'.
            struct_decode( EXPORTING iv_string = getdata
                           CHANGING cg_structure = ls_result ).
            struct_decode( EXPORTING iv_string = getdata
                           CHANGING cg_structure = ls_repo ).
            diff( is_result = ls_result
                  is_repo   = ls_repo ).
          WHEN 'jump'.
            struct_decode( EXPORTING iv_string = getdata
                           CHANGING cg_structure = ls_result ).
            CLEAR ls_item.
            MOVE-CORRESPONDING ls_result TO ls_item.
            lcl_objects=>jump( ls_item ).
          WHEN 'pull'.
            struct_decode( EXPORTING iv_string = getdata
                           CHANGING cg_structure = ls_repo_persi ).
            pull( ls_repo_persi ).
          WHEN 'debug'.
            lcl_debug=>debug_toggle( ).
          WHEN 'newoffline'.
            newoffline( ).
          WHEN 'zipimport'.
            struct_decode( EXPORTING iv_string = getdata
                           CHANGING cg_structure = ls_repo_persi ).
            lcl_zip=>import( ls_repo_persi ).
            view( render( ) ).
          WHEN 'zipexport'.
            struct_decode( EXPORTING iv_string = getdata
                           CHANGING cg_structure = ls_repo_persi ).
            lcl_zip=>export( ls_repo_persi ).
            view( render( ) ).
          WHEN 'files_commit'.
            struct_decode( EXPORTING iv_string = getdata
                           CHANGING cg_structure = ls_repo_persi ).
            lcl_zip=>export( is_repo = ls_repo_persi
                             iv_zip  = abap_false ).
            view( render( ) ).
          WHEN 'zipexport_gui'.
            zipexport( ).
          WHEN OTHERS.
            _raise 'Unknown action'.
        ENDCASE.
      CATCH lcx_exception INTO lx_exception.
        IF lcl_debug=>get_debug( ) = abap_true.
          view( render_header( ) &&
            lcl_debug=>get_html( ) &&
            render_footer( ) ).
        ENDIF.
        MESSAGE lx_exception->mv_text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "on_event

  METHOD uninstall.

    DATA: lt_tadir    TYPE tt_tadir,
          lv_count    TYPE c LENGTH 3,
          lv_answer   TYPE c LENGTH 1,
          lv_question TYPE c LENGTH 100.


    lt_tadir = lcl_tadir=>read( is_repo-package ).

    IF lines( lt_tadir ) > 0.
      lv_count = lines( lt_tadir ).

      CONCATENATE 'This will delete all objects in package' is_repo-package
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

    lcl_persistence=>delete( is_repo ).

    view( render( ) ).

  ENDMETHOD.                    "uninstall

  METHOD remove.

    DATA: lv_answer   TYPE c LENGTH 1,
          lv_question TYPE c LENGTH 100.


    CONCATENATE 'This will remove the repository reference to the package'
      is_repo-package
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

    lcl_persistence=>delete( is_repo ).

    view( render( ) ).

  ENDMETHOD.                    "remove

  METHOD add.

    DATA: lt_files    TYPE tt_files,
          ls_comment  TYPE st_comment,
          ls_repo     TYPE st_repo,
          lv_branch   TYPE t_sha1,
          lv_obj_name TYPE tadir-obj_name.


* fix for SICF services
    CONCATENATE is_item-obj_name '%' INTO lv_obj_name.

    SELECT SINGLE obj_name FROM tadir
      INTO lv_obj_name
      WHERE pgmid = 'R3TR'
      AND object = is_item-obj_type
      AND obj_name LIKE lv_obj_name
      AND devclass = is_repo_persi-package.             "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      _raise 'Object not found or in wrong package'.
    ENDIF.

    lt_files = lcl_objects=>serialize( is_item ).

    ls_comment = popup_comment( ).
    IF ls_comment IS INITIAL.
      RETURN.
    ENDIF.

    MOVE-CORRESPONDING is_repo_persi TO ls_repo.
    lv_branch = lcl_porcelain=>push( is_comment = ls_comment
                                     is_repo    = ls_repo
                                     it_files   = lt_files ).

    lcl_persistence=>update( is_repo   = ls_repo
                             iv_branch = lv_branch ).

    view( render( ) ).

  ENDMETHOD.                    "add

  METHOD newoffline.

    DATA: lv_returncode TYPE c,
          ls_repo       TYPE st_repo,
          lv_package    TYPE devclass,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'ABAPTXT255'.
    <ls_field>-fieldname = 'LINE'.
    <ls_field>-fieldtext = 'Name'.                          "#EC NOTEXT

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'TDEVC'.
    <ls_field>-fieldname = 'DEVCLASS'.
    <ls_field>-fieldtext = 'Package'.                       "#EC NOTEXT

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
    ls_repo-url = <ls_field>-value.

    READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    lv_package = <ls_field>-value.
    TRANSLATE lv_package TO UPPER CASE.

    lcl_persistence=>validate_package( lv_package ).

    lcl_persistence=>add( is_repo    = ls_repo
                          iv_package = lv_package
                          iv_offline = abap_true ).

    view( render( ) ).

  ENDMETHOD.                    "newoffline

  METHOD install.

    DATA: lv_returncode TYPE c,
          lt_files      TYPE tt_files,
          ls_repo       TYPE st_repo,
          lv_branch     TYPE t_sha1,
          lv_icon_ok    TYPE icon-name,
          lv_icon_br    TYPE icon-name,
          lv_package    TYPE devclass,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'ABAPTXT255'.
    <ls_field>-fieldname = 'LINE'.
    <ls_field>-fieldtext = 'Git Clone Url'.                 "#EC NOTEXT
    <ls_field>-value     = iv_url.

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'TDEVC'.
    <ls_field>-fieldname = 'DEVCLASS'.
    <ls_field>-fieldtext = 'Target Package'.                "#EC NOTEXT

    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'TEXTL'.
    <ls_field>-fieldname = 'LINE'.
    <ls_field>-fieldtext = 'Branch'.                        "#EC NOTEXT
    <ls_field>-value     = 'refs/heads/master'.             "#EC NOTEXT
    <ls_field>-field_attr = '05'.

    lv_icon_ok = icon_okay.
    lv_icon_br = icon_workflow_fork.

    CALL FUNCTION 'POPUP_GET_VALUES_USER_BUTTONS'
      EXPORTING
        popup_title       = 'Clone'
        programname       = sy-repid
        formname          = 'BRANCH_POPUP'
        ok_pushbuttontext = 'OK'
        icon_ok_push      = lv_icon_ok
        first_pushbutton  = 'Select branch'
        icon_button_1     = lv_icon_br
      IMPORTING
        returncode        = lv_returncode
      TABLES
        fields            = lt_fields
      EXCEPTIONS
        error_in_fields   = 1
        OTHERS            = 2. "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'Error from POPUP_GET_VALUES'.
    ENDIF.
    IF lv_returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    ls_repo-url = <ls_field>-value.
    lcl_url=>name( ls_repo-url ).         " validate

    READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    lv_package = <ls_field>-value.
    TRANSLATE lv_package TO UPPER CASE.
    lcl_persistence=>validate_package( lv_package ).

    READ TABLE lt_fields INDEX 3 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    ls_repo-branch_name = <ls_field>-value.

    lcl_porcelain=>pull( EXPORTING is_repo   = ls_repo
                         IMPORTING et_files  = lt_files
                                   ev_branch = lv_branch ).

* call status to check for errors
    lcl_objects=>status( it_files   = lt_files
                         iv_package = lv_package ).

    lcl_objects=>deserialize( it_files   = lt_files
                              iv_package = lv_package ).

    lcl_persistence=>add( is_repo    = ls_repo
                          iv_branch  = lv_branch
                          iv_package = lv_package ).

    view( render( ) ).

  ENDMETHOD.                    "install

  METHOD render_css.

    rv_html = '<style type="text/css">' && gc_newline &&
          'body {'                      && gc_newline &&    "#EC NOTEXT
          '  font-family: Arial,Helvetica,sans-serif;' && gc_newline && "#EC NOTEXT
          '  background: #DEF1F2;'      && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a:link {'                    && gc_newline &&    "#EC NOTEXT
          '  color: blue;'              && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a:visited {'                 && gc_newline &&    "#EC NOTEXT
          '  color: blue;'              && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.grey:link {'               && gc_newline &&    "#EC NOTEXT
          '  color: grey;'              && gc_newline &&    "#EC NOTEXT
          '  font-size: smaller;'       && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.grey:visited {'            && gc_newline &&    "#EC NOTEXT
          '  color: grey;'              && gc_newline &&    "#EC NOTEXT
          '  font-size: smaller;'       && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.plain:link {'              && gc_newline &&    "#EC NOTEXT
          '  color: black;'             && gc_newline &&    "#EC NOTEXT
          '  text-decoration: none;'    && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.plain:visited {'           && gc_newline &&    "#EC NOTEXT
          '  color: black;'             && gc_newline &&    "#EC NOTEXT
          '  text-decoration: none;'    && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.white:link {'              && gc_newline &&    "#EC NOTEXT
          '  color: white;'             && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'a.white:visited {'           && gc_newline &&    "#EC NOTEXT
          '  color: white;'             && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'h1 {'                        && gc_newline &&    "#EC NOTEXT
          '  display: inline;'          && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'h2 {'                        && gc_newline &&    "#EC NOTEXT
          '  display: inline;'          && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'h3 {'                        && gc_newline &&    "#EC NOTEXT
          '  display: inline;'          && gc_newline &&    "#EC NOTEXT
          '  color: grey;'              && gc_newline &&    "#EC NOTEXT
          '  font-weight:normal;'       && gc_newline &&    "#EC NOTEXT
          '  font-size: smaller;'       && gc_newline &&    "#EC NOTEXT
          '}'                           && gc_newline &&
          'pre {'                       && gc_newline &&
          '  display: inline;'          && gc_newline &&
          '}'                           && gc_newline &&
          'table, th, td {'             && gc_newline &&
          '  border: 1px solid black;'  && gc_newline &&
          '  border-collapse: collapse;' && gc_newline &&
          '}'                           && gc_newline &&
          'th, td {'                    && gc_newline &&
          '  padding: 5px;'             && gc_newline &&
          '}'                           && gc_newline &&
          'th {'                        && gc_newline &&
          '  background: #e5e5e5;'      && gc_newline &&
          '}'                           && gc_newline &&
          'td {'                        && gc_newline &&
           ' background: #F8FCFC;'      && gc_newline &&
          '}'                           && gc_newline &&
          '</style>'                    && gc_newline.

  ENDMETHOD.                    "render_css

  METHOD render_menu.
    rv_html =
      |<img src="{ get_logo_src( ) }" height="50px">|
      && gc_newline &&
      '<h1>abapGit</h1>&nbsp;'                                  && gc_newline &&
      '<a href="sapevent:refresh">Refresh</a>&nbsp;'            && gc_newline &&
      '<a href="sapevent:install">Clone</a>&nbsp;'              && gc_newline &&
      '<a href="sapevent:explore">Explore</a>&nbsp;'            && gc_newline &&
      '<a href="sapevent:abapgithome">abapGit@GitHub</a>&nbsp;' && gc_newline &&
      '<a href="sapevent:newoffline">New offline project</a>&nbsp;' && gc_newline &&
      '<hr>'                                                    && gc_newline.

  ENDMETHOD.                    "render_menu

  METHOD repo_name.

    IF is_repo-offline = abap_true.
      rv_name = is_repo-url.
    ELSE.
      rv_name = lcl_url=>name( is_repo-url ).
    ENDIF.

  ENDMETHOD.                    "repo_name

  METHOD render.

    DATA: lt_repos TYPE tt_repos_persi,
          lv_text  TYPE c LENGTH 100,
          lv_pct   TYPE i,
          lv_f     TYPE f,
          ls_repo  LIKE LINE OF lt_repos.


    lt_repos = lcl_persistence=>list( ).

    rv_html = render_header( ) && render_menu( ).

    LOOP AT lt_repos INTO ls_repo.

      rv_html = rv_html &&
        '<a href="#' && repo_name( ls_repo ) &&'" class="grey">' &&
        repo_name( ls_repo ) &&
        '</a>&nbsp;'.
    ENDLOOP.

    IF lt_repos[] IS INITIAL.
      rv_html = rv_html && '<br><a href="sapevent:explore">Explore</a> new projects'.
    ELSE.
      rv_html = rv_html && '<br><br><br>'.

      LOOP AT lt_repos INTO ls_repo.
        lv_f = ( sy-tabix / lines( lt_repos ) ) * 100.
        lv_pct = lv_f.
        IF lv_pct = 100.
          lv_pct = 99.
        ENDIF.
        lv_text = repo_name( ls_repo ).
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = lv_pct
            text       = lv_text.

        IF ls_repo-offline = abap_true.
          rv_html = rv_html && render_repo_offline( ls_repo ).
        ELSE.
          rv_html = rv_html && render_repo_online( ls_repo ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    rv_html = rv_html &&
              render_footer( ).

  ENDMETHOD.                    "render

  METHOD render_footer.

    rv_html = rv_html &&
      '<br><br><hr><center><h3>abapGit Version:&nbsp;' &&
      gc_abap_version &&
      '&nbsp;<a href="sapevent:debug" class="white">d</a>' &&
      '&nbsp;<a href="sapevent:zipexport_gui" class="white">e</a>' &&
      '</h3></center>'.                                     "#EC NOTEXT

    rv_html = rv_html &&
      '<center>' &&
      |<img src="{ get_logo_src( ) }" >| &&
      '</center>'.                                          "#EC NOTEXT

    rv_html = rv_html && '</body></html>'.

  ENDMETHOD.                    "render_footer

  METHOD render_repo_offline.

    DATA: lt_tadir TYPE tt_tadir.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.


    rv_html = rv_html &&
      '<a id="' && repo_name( is_repo_persi ) && '"></a>' &&
      '<h2>' && repo_name( is_repo_persi ) && '</h2>&nbsp;' &&
      '<h3>' && is_repo_persi-package && '</h3>&nbsp;&nbsp;' &&
      '<br>' &&
      '<a href="sapevent:remove?' &&
      struct_encode( is_repo_persi ) &&
      '" class="grey">' &&
      'remove' &&
      '</a>&nbsp;' &&
      '<a href="sapevent:uninstall?' &&
      struct_encode( is_repo_persi ) &&
      '" class="grey">' &&
      'uninstall' &&
      '</a><br><br>'.                                       "#EC NOTEXT

    rv_html = rv_html && '<table border="1">' && gc_newline &&
      '<tr>'                                  && gc_newline &&
      '<th><u>Local object</u></th>'          && gc_newline &&
      '</tr>'                                 && gc_newline.

    lt_tadir = lcl_tadir=>read( is_repo_persi-package ).

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
* todo, add jump link like in online rendering
      rv_html = rv_html && '<tr>' &&
        '<td>' && <ls_tadir>-object &&
        '&nbsp;' && <ls_tadir>-obj_name &&
        '</td>' && gc_newline &&
        '</tr>' && gc_newline.
    ENDLOOP.

    rv_html = rv_html && '</table>' && gc_newline.

    rv_html = rv_html && '<a href="sapevent:zipimport?' &&
      struct_encode( is_repo_persi ) &&
      '">' && 'Import ZIP' &&
      '</a>&nbsp;' &&
      '<a href="sapevent:zipexport?' &&
      struct_encode( is_repo_persi ) &&
      '">' && 'Export ZIP' &&
      '</a>&nbsp;' &&
      '<a href="sapevent:files_commit?' &&
      struct_encode( is_repo_persi ) &&
      '">' && 'Export files and commit' &&
      '</a>&nbsp;' &&
      '<br><br><br>'.                                       "#EC NOTEXT

  ENDMETHOD.                    "render_repo_offline

  METHOD render_repo_online.

    DATA: lt_files      TYPE tt_files,
          ls_repo       TYPE st_repo,
          lv_branch     TYPE t_sha1,
          lv_link       TYPE string,
          lv_status     TYPE string,
          lv_object     TYPE string,
          lv_index      LIKE sy-tabix,
          lv_span       TYPE i,
          lt_results    TYPE tt_results,
          ls_next       LIKE LINE OF lt_results,
          ls_item       TYPE st_item,
          lv_class_name TYPE string,
          lo_object     TYPE REF TO object.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results.


    MOVE-CORRESPONDING is_repo_persi TO ls_repo.

    rv_html = rv_html &&
      '<a id="' && repo_name( is_repo_persi ) && '"></a>' &&
      '<h2>' && repo_name( is_repo_persi ) && '</h2>&nbsp;' &&
      '<h3>' && is_repo_persi-url && '</h3>&nbsp;&nbsp;' &&
      '<h3>' && is_repo_persi-branch_name && '</h3>&nbsp;&nbsp;' &&
      '<h3>' && is_repo_persi-package && '</h3>&nbsp;&nbsp;' &&
      '<br>' &&
      '<a href="sapevent:remove?' &&
      struct_encode( is_repo_persi ) &&
      '" class="grey">' &&
      'remove' &&
      '</a>&nbsp;' &&
      '<a href="sapevent:uninstall?' &&
      struct_encode( is_repo_persi ) &&
      '" class="grey">' &&
      'uninstall' &&
      '</a><br>'.                                           "#EC NOTEXT

    lcl_porcelain=>pull( EXPORTING is_repo   = ls_repo
                         IMPORTING et_files  = lt_files
                                   ev_branch = lv_branch ).

    rv_html = rv_html && '<br>'.

    lt_results = lcl_objects=>status( it_files   = lt_files
                                      iv_package = is_repo_persi-package ).
    IF lv_branch <> is_repo_persi-sha1.
      lv_status = 'pull'.                                   "#EC NOTEXT
    ELSE.
      READ TABLE lt_results WITH KEY match = abap_false TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lv_status = 'commit'.                               "#EC NOTEXT
      ELSE.
        lv_status = 'match'.                                "#EC NOTEXT
      ENDIF.
    ENDIF.

    rv_html = rv_html && '<table border="1">' && gc_newline &&
      '<tr>'                                  && gc_newline &&
      '<th><u>Local object</u></th>'          && gc_newline &&
      '<th></td>'                             && gc_newline &&
      '<th><u>Remote file</u></th>'           && gc_newline &&
      '<th></th>'                             && gc_newline &&
      '</tr>'                                 && gc_newline.

    LOOP AT lt_results ASSIGNING <ls_result>.
      lv_index = sy-tabix.

      CLEAR lv_link.
      IF lv_status = 'match' AND <ls_result>-filename IS INITIAL.
        MOVE-CORRESPONDING <ls_result> TO ls_item.
        lv_class_name = lcl_objects=>class_name( ls_item ).
        TRY.
            CREATE OBJECT lo_object TYPE (lv_class_name).
            lv_link = '<a href="sapevent:add?' &&
              struct_encode( ig_structure1 = is_repo_persi ig_structure2 = <ls_result> )
              && '">add</a>'.
          CATCH cx_sy_create_object_error.
            lv_link = |Object type <b>{ ls_item-obj_type }</b> not supported|.
        ENDTRY.
      ELSEIF <ls_result>-match = abap_false.
        lv_link = '<a href="sapevent:diff?' &&
          struct_encode( ig_structure1 = <ls_result> ig_structure2 = ls_repo ) &&
          '">diff</a>'.
      ENDIF.

      IF lv_span = 0.
        READ TABLE lt_results INTO ls_next INDEX lv_index.
        ASSERT sy-subrc = 0.
        WHILE ls_next-obj_type = <ls_result>-obj_type
            AND ls_next-obj_name = <ls_result>-obj_name.
          lv_span  = lv_span + 1.
          lv_index = lv_index + 1.
          READ TABLE lt_results INTO ls_next INDEX lv_index.
          IF sy-subrc <> 0.
            EXIT. " current loop.
          ENDIF.
        ENDWHILE.

        IF <ls_result>-obj_type IS INITIAL.
          lv_object = '<td rowspan="' &&
            lv_span &&
            '" valign="top">&nbsp;</td>' &&
            gc_newline.
        ELSE.
          lv_object = '<td rowspan="' &&
            lv_span &&
            '" valign="top"><a href="sapevent:jump?' &&
            struct_encode( <ls_result> ) &&
            '" class="plain">' &&
            <ls_result>-obj_type &&
            '&nbsp;' &&
            <ls_result>-obj_name  &&
            '</a></td>' && gc_newline.
        ENDIF.
      ELSE.
        CLEAR lv_object.
      ENDIF.

      rv_html = rv_html &&
        '<tr>'                                    && gc_newline &&
        lv_object                                 &&
        '<td>' && <ls_result>-match && '</td>'    && gc_newline &&
        '<td>' && <ls_result>-filename && '</td>' && gc_newline &&
        '<td>' && lv_link && '</td>'              && gc_newline &&
        '</tr>'                                   && gc_newline.

      lv_span = lv_span - 1.
    ENDLOOP.
    rv_html = rv_html && '</table>' && gc_newline.

    CASE lv_status.
      WHEN 'commit'.
        rv_html = rv_html && '<a href="sapevent:commit?'
                  && struct_encode( ls_repo ) && '">commit</a>'.
      WHEN 'pull'.
        rv_html = rv_html && '<a href="sapevent:pull?'
                  && struct_encode( is_repo_persi ) && '">pull</a>'.
    ENDCASE.

    rv_html = rv_html && '<br><br><br>'.

  ENDMETHOD.                    "render_repo

  METHOD run.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events.


    CREATE OBJECT go_html_viewer
      EXPORTING
        parent = cl_gui_container=>screen0.

    CLEAR ls_event.
    ls_event-eventid = go_html_viewer->m_id_sapevent.
    ls_event-appl_event = 'x'.
    APPEND ls_event TO lt_events.
    go_html_viewer->set_registered_events( lt_events ).

    SET HANDLER lcl_gui=>on_event FOR go_html_viewer.

    view( render( ) ).

  ENDMETHOD.                    "init

  METHOD view.

    DATA: lt_data TYPE TABLE OF text200,
          lv_html TYPE string,
          lv_url  TYPE text200.


    lv_html = iv_html.

    WHILE strlen( lv_html ) > 0.
      IF strlen( lv_html ) < 200.
        APPEND lv_html TO lt_data.
        CLEAR lv_html.
      ELSE.
        APPEND lv_html(200) TO lt_data.
        lv_html = lv_html+200.
      ENDIF.
    ENDWHILE.

    go_html_viewer->load_data(
      IMPORTING
        assigned_url = lv_url
      CHANGING
        data_table   = lt_data ).

    go_html_viewer->show_url( lv_url ).

  ENDMETHOD.                    "view

  METHOD get_logo_src.
    rv_src =
      |data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAX8AAAF/CAMAAACWmjlVAAAA| &&
      |M1BMVEX////wUDPwUDPwUDPwUDPwUDPwUDPwUDPwUDPwUDPwUDPwUDPwUDPwUDPwUDPwUD| &&
      |PwUDP3eUwZAAAAEHRSTlMA8DAQ0KDAQGCA4CCQUHCw+BUOAQAACQ5JREFUeF7s1VFKw0AA| &&
      |RVHTpmkbGzr7X63gn6D1K7ko521g4J5h5u2vbrsv6/jctDy285FH2/Uyja97bgCO2m0d32| &&
      |y9AThip2X8sPkKYPfdx4s9AOy783O83LzrRyz/PH7Z/A4gzD/GdAIQ5A8A5A8A5M8A5E8B| &&
      |5E8B5E8B5E8B5E8B5E8B5E8B5E8B5E8B5E8B5G8B5O8B5O8B5O8B5O8B5O8B5O8B5O8B5O| &&
      |8B5O8B5O8B5O8B5O8B5O8B5O8B5O8B5O8B5O8B5O8B5O8B5O8B9AcAAIABAGAAugEAYAAA| &&
      |GAAABgCAAQDw//sCaK/3BUD7ugBI8wNI8wNI8wNI8wNI8wNI8wNI8wNI8wNI8wNI8wNI8w| &&
      |NI8wNI8wNI8wNI8wNI8wMI83+wcy85EsIwAEQDJAH6A3X/086yZ20hlWTZN6CewPIGKAA3| &&
      |fwH4+QvAz18Afv4C8PMXgJ+/APz8cYA5P+d5jtkLIJY/DrCM43+b/boL4Kn8MCLURy+AeP| &&
      |44wG/2WQCB/M8BtL0XQCD/cwDtLIBffgNgfRXAA/lh1M+7zfxxgDZ8AD8/BaDmVwG6DODn| &&
      |dwE2dwn7+W2AFRHAz+8DXCaAn98H6CKAn98H2BEB/Pw+wBQB/Pw+wI4AkDE/fL0XwAfw8y| &&
      |9bqP+BB+Dn9xfAhgCQMD/fFptbAEiYn827wXwAP39vwVkRANLl59OigwCQLj9nuH8XANLl| &&
      |Zw/3n3gAfn6//wcBIFt+4k96IgBky0/z+vsAfn42r78P4OdnF/vHAfz8fv8bASBbfo5w/4| &&
      |kH4Of3768FASBbfu5o/jcIANnys0T7f/EA/Pz+ATYQAPLl5wr2fyEA5MtPD69fD8DP73+A| &&
      |VgEgY35GKwAxP7wLwMzPaAUg5oe1AMz8zFYAYn64CsDMD2sBmPlZtgIQ80MvADM/BbD1P/| &&
      |buNudxFQjCqAk2Nvgj7H+19/4raTTzjoKGFFQ3K0DnkRNsJ2oGuwcAP3kVe1/CfH4/hoKf| &&
      |v9JpMQD4+euKBgOAn7/CFocPoMiPtVu5AsA/hf9LLAD45/BPh1QA8M/iX5UCgH8ef6EA4J| &&
      |/IXycA+GfylwkA/qn8VQKAfy5/kQDgn8xfIwD4Z/OXCAD+6fwVAoB/Pn+BAOCf0F8gAPgn| &&
      |9BcIAP4J/QUCgH9Cf4EA4J/QXyAA+Cf0FwgA/gn9BQKAX8B/ogDgF/CfLwD4BfznCwB+Af| &&
      |+mAOuWj/3/decrEQKAf2b/xgDp+iXpfhVCAPBr+CNAw3ptXwsAfj3/9gDLeX0nAPgl/dsD| &&
      |LGf6QgDwq/q3B1iO0DsA+IX92wO81r4BwC/t3x4gpp4BwK/uX48Bx9mCX8N/ugDg1/dHAP| &&
      |5I87PHoCC+f68Aoed/mXM14o8A/GGGCSesquPfLcDVb6xNEvLvFiCWXnM9jirk3y/A0esT| &&
      |qCj5dwxQ+sxV3Ks1/3qMdAFs9vwD/wLAN0Cw57/Rj0DYyF3t+b/o4zxxIWZ7/gGk/A+gZM| &&
      |//PcxjuPosxZ7/A1H2CWhbqj1/1OM/BLLoH8eZ5xws+i/j+FeD/mVhztNz/2TeH8v9A5HR| &&
      |/ddPkvrn//rvz58XkdHPPxf1+Zuf/48lEhn9/vdclrc5/9zq//QYbHnTFP3552Py/XsYZp| &&
      |x5tPT7E6x7kPdf2yenWn//u3WaKrpb8w9xiN/gXnixb8u/PiO8/SoR99XG/MsIb993c/+/| &&
      |wMr8w//TMJZAxj9E9qf/BnwEkPdvvwd79+JHAH3/9t/g3t34EcCUf3gxBieBnxuA71/XSJ| &&
      |uCsDVEVvNHAD4/AhjyRwA+PwIY8kcAPj8CWPKvYWd89RICsP3bb4Rz/Q4/Apjyr+uPl8C+| &&
      |fpMfAfT9sdIfC5zv+mV+BND3x/r92M47VQI/Auj7Y4XtOBesJd5bqAR+BLDij1VSzs++Pz| &&
      |mnAjQCPwII+PMX+AkB3B/8hADuD35CAPcHPyGA+4OfEMD9wU8I4P7gJwRwf/ATArg/+AkB| &&
      |3B/8hADuD35CAPcHPyGA+4OfEMD9wU8I4P7gJwRwf/ATArg/+AkB3B/8hADuD35CAPcHPy| &&
      |GA+4OfEMD9wU8I4P7glw5QchzSH/zKAUL+YVzsewh+5QDv+OMG98LnFw4Q7r9tMF5sfuEA| &&
      |69kwMZnALxogxVF3CH7hANuwOwS/cIA07A7BLxxgjbRJFXx+foDw2dCoh8GvHOAmjEpg8/| &&
      |MDtM+MOgOBXzZAOD/eYCbwywbIvGl1fH5+gBAb/A8Cv2iAq2l/gcCvGeBs8s8EfskAa9vu| &&
      |TgK/ZICHOC6Hz88PcDb6XwR+wQCFOK+Fz88P8G7eG4FfMEBu9i8Efr0Ad7N/IvDrBdib/S| &&
      |9xfgQY0z8T+PUCLCP6/8feHaQ2DEMBEK0tWZGiOu79T9tFF39RqCHwGYomJ4jfBInwkQXw| &&
      |AwFwf56fD7AD/gA/EABY/wF+KgDv30B+PgD//6uC/HwA/nmz+NcKUN/+Okn8qwUo4H3JPD| &&
      |8foL/pf3H8fAD+oQvIzwfgF6AO8vMB+AWocvx8AH4C+QD5+QD8DtwS+FcN8Czgz5/n5wMM| &&
      |cvbL8/MBdnD0wvPzAWoBVx+enw9wEKdfgt8Ag72v1wAd5jfAP+B3D5hp/AZo5RbgTNx6Db| &&
      |Cdt6//kT91HtDmHwD9KX/6POCYN/ryJ88DWi+/AOYIffnTZ8LXeATAPD/rcsNG/nzA1n4+| &&
      |QSY/cUBD/vsA8hsA4DeA/BFAfgMA/AaQPwLIbwCA3wDyRwD5DQDwGwDiN4D8EUB+AwD8Bp| &&
      |A/AshvAIDfAPJHAPkNAPAbAOA3AMBvAIDfABy/AXh+A/D8BpDf65INIP9K1yUbgOc3AM9v| &&
      |AJ7fADy/AXh+A/D8BuD5DcDzG+D4bu9eUhiGYTAIJ7ai9BFM73/aQpeFamcPheG/wTdeGl| &&
      |QBGADmNwDMbwCY3wAwvwFgfgPA/AaQ3yM+BgD5DQDzG0B+dM+QH10PmN8AML8BYH4DwPwG| &&
      |gPkNAPMbAOY3AMxvAJjfADC/AWB+A8D8BoD5DQDzGwDmNwDMbwCY3wAwvwFgfgPA/AaQH1| &&
      |3Pmn9INHn3Qv9x6jN9LX/xX12dFTuzOMvsFqwdX98TY/j2166NIz8RIq/b3z79N8fTabrp| &&
      |KtbiAAAAAElFTkSuQmCC|.

  ENDMETHOD.                    "base64_logo
ENDCLASS.                    "lcl_gui IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM run.

  DATA: lx_exception TYPE REF TO lcx_exception,
        lv_ind       TYPE t000-ccnocliind.


  IF sy-langu <> gc_english.
    WRITE: / 'Use English as logon language'.               "#EC NOTEXT
    RETURN.
  ENDIF.

  SELECT SINGLE ccnocliind FROM t000 INTO lv_ind
    WHERE mandt = sy-mandt.
  IF sy-subrc = 0
      AND lv_ind <> ' '
      AND lv_ind <> '1'. " check changes allowed
    WRITE: / 'Wrong client, changes to repository objects not allowed'. "#EC NOTEXT
    RETURN.
  ENDIF.

* bitbucket require agent prefix = "git/"
  gv_agent = 'git/abapGit ' && gc_abap_version.

  TRY.
      lcl_gui=>run( ).
    CATCH lcx_exception INTO lx_exception.
      MESSAGE lx_exception->mv_text TYPE 'E'.
  ENDTRY.

  CALL SELECTION-SCREEN 1001. " trigger screen

ENDFORM.                    "run

FORM branch_popup TABLES   tt_fields STRUCTURE sval
                  USING    pv_code
                  CHANGING cs_error TYPE svale
                           cv_show_popup TYPE c
                  RAISING lcx_exception ##CALLED ##NEEDED.
* called dynamically from function module POPUP_GET_VALUES_USER_BUTTONS

  DATA: li_client    TYPE REF TO if_http_client,
        lv_url       TYPE string,
        lv_answer    TYPE c,
        lx_error     TYPE REF TO lcx_exception,
        lt_selection TYPE TABLE OF spopli,
        lt_branches  TYPE tt_branch_list.

  FIELD-SYMBOLS: <ls_fbranch> LIKE LINE OF tt_fields,
                 <ls_branch>  LIKE LINE OF lt_branches,
                 <ls_sel>     LIKE LINE OF lt_selection,
                 <ls_furl>    LIKE LINE OF tt_fields.


  IF pv_code = 'COD1'.
    cv_show_popup = abap_true.

    READ TABLE tt_fields ASSIGNING <ls_furl> WITH KEY tabname = 'ABAPTXT255'.
    IF sy-subrc <> 0 OR <ls_furl>-value IS INITIAL.
      MESSAGE 'Fill URL' TYPE 'S' DISPLAY LIKE 'E'.         "#EC NOTEXT
      RETURN.
    ENDIF.
    lv_url = <ls_furl>-value.

    TRY.
        lcl_transport=>branch_list(
          EXPORTING
            iv_url         = lv_url
          IMPORTING
            ei_client      = li_client
            et_branch_list = lt_branches ).
        li_client->close( ).
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
        OTHERS             = 4. "#EC NOTEXT
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

  ENDIF.

ENDFORM.

CLASS ltcl_diff DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    DATA: mt_local    TYPE TABLE OF string,
          mt_remote   TYPE TABLE OF string,
          mt_expected TYPE tt_diffs,
          ms_expected LIKE LINE OF mt_expected.

    METHODS: setup.
    METHODS: test.

    METHODS:
      diff01 FOR TESTING,
      diff02 FOR TESTING,
      diff03 FOR TESTING,
      diff04 FOR TESTING,
      diff05 FOR TESTING,
      diff06 FOR TESTING.

ENDCLASS.

CLASS ltcl_diff IMPLEMENTATION.

  DEFINE _local.
    append &1 to mt_local.
  END-OF-DEFINITION.

  DEFINE _remote.
    append &1 to mt_remote.
  END-OF-DEFINITION.

  DEFINE _expected.
    clear ms_expected.
    ms_expected-local = &1.
    ms_expected-result = &2.
    ms_expected-remote = &3.
    append ms_expected to mt_expected.
  END-OF-DEFINITION.

  METHOD setup.
    CLEAR mt_local.
    CLEAR mt_remote.
    CLEAR mt_expected.
  ENDMETHOD.

  METHOD test.

    DATA: lv_local   TYPE string,
          lv_xlocal  TYPE xstring,
          lv_remote  TYPE string,
          lv_xremote TYPE xstring,
          lt_diff    TYPE tt_diffs.


    CONCATENATE LINES OF mt_local  INTO lv_local SEPARATED BY gc_newline.
    CONCATENATE LINES OF mt_remote INTO lv_remote SEPARATED BY gc_newline.

    lv_xlocal  = lcl_convert=>string_to_xstring_utf8( lv_local ).
    lv_xremote = lcl_convert=>string_to_xstring_utf8( lv_remote ).

    lt_diff = lcl_diff=>diff( iv_local  = lv_xlocal
                              iv_remote = lv_xremote ).

    cl_abap_unit_assert=>assert_equals( act = lt_diff
                                        exp = mt_expected ).


  ENDMETHOD.

  METHOD diff01.

* insert
    _local '1'.
    _expected '1' gc_diff-insert ''.
    test( ).

  ENDMETHOD.

  METHOD diff02.

* identical
    _local '1'.
    _remote '1'.
    _expected '1' '' '1'.
    test( ).

  ENDMETHOD.

  METHOD diff03.

* delete
    _remote '1'.
    _expected '' gc_diff-delete '1'.
    test( ).

  ENDMETHOD.

  METHOD diff04.

* update
    _local '1+'.
    _remote '1'.
    _expected '1+' gc_diff-update '1'.
    test( ).

  ENDMETHOD.

  METHOD diff05.

* identical
    _local '1'.
    _local '2'.
    _remote '1'.
    _remote '2'.
    _expected '1' '' '1'.
    _expected '2' '' '2'.
    test( ).

  ENDMETHOD.

  METHOD diff06.

    _local '1'.
    _local '2'.
    _local 'inserted'.
    _local '3'.
    _local '4 update'.

    _remote '1'.
    _remote '2'.
    _remote '3'.
    _remote '4'.

    _expected '1' '' '1'.
    _expected '2' '' '2'.
    _expected 'inserted' gc_diff-insert ''.
    _expected '3' '' '3'.
    _expected '4 update' gc_diff-update '4'.

    test( ).

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_abap_unit DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS encode_decode_tree FOR TESTING RAISING lcx_exception.
    METHODS encode_decode_commit FOR TESTING RAISING lcx_exception.
    METHODS encode_decode_pack_short FOR TESTING RAISING lcx_exception.
    METHODS encode_decode_pack_long FOR TESTING RAISING lcx_exception.
    METHODS encode_decode_pack_multiple FOR TESTING RAISING lcx_exception.

    METHODS convert_int FOR TESTING RAISING lcx_exception.

ENDCLASS.                    "test DEFINITION

CLASS ltcl_url DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS repo_url FOR TESTING RAISING lcx_exception.
    METHODS repo_error FOR TESTING.

ENDCLASS.

CLASS ltcl_serialize DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS serialize_tabl FOR TESTING RAISING lcx_exception.
    METHODS serialize_enqu FOR TESTING RAISING lcx_exception.
    METHODS serialize_shlp FOR TESTING RAISING lcx_exception.
    METHODS serialize_view FOR TESTING RAISING lcx_exception.

ENDCLASS.

CLASS ltcl_xml DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS xml FOR TESTING RAISING lcx_exception.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_xml IMPLEMENTATION.

  METHOD xml.

    DATA: lo_xml           TYPE REF TO lcl_xml,
          lv_xml           TYPE string,
          ls_component_in  TYPE wdy_component_metadata,
          ls_component_out TYPE wdy_component_metadata.


    ls_component_in-comp_metadata-definition-component_name = 'FOOBAR'.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_component_in ).
    lv_xml = lo_xml->xml_render( ).

    CREATE OBJECT lo_xml
      EXPORTING
        iv_xml = lv_xml.
    lo_xml->structure_read( CHANGING cg_structure = ls_component_out ).

    cl_abap_unit_assert=>assert_equals( act = ls_component_out
                                        exp = ls_component_in ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_serialize IMPLEMENTATION.

  METHOD serialize_enqu.

    DATA: ls_item  TYPE st_item,
          lt_files TYPE tt_files.


    ls_item-obj_type = 'ENQU'.
    ls_item-obj_name = 'E_USR04'.

    lt_files = lcl_objects=>serialize( ls_item ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_shlp.

    DATA: ls_item  TYPE st_item,
          lt_files TYPE tt_files.


    ls_item-obj_type = 'SHLP'.
    ls_item-obj_name = 'USER_LOGON'.

    lt_files = lcl_objects=>serialize( ls_item ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_view.

    DATA: ls_item  TYPE st_item,
          lt_files TYPE tt_files.


    ls_item-obj_type = 'VIEW'.
    ls_item-obj_name = 'VUSR02_HEADER'.

    lt_files = lcl_objects=>serialize( ls_item ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_tabl.

    DATA: ls_item  TYPE st_item,
          lt_files TYPE tt_files.


    ls_item-obj_type = 'TABL'.
    ls_item-obj_name = 'USR02'.

    lt_files = lcl_objects=>serialize( ls_item ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "serialize_table

ENDCLASS.

CLASS ltcl_url IMPLEMENTATION.

  METHOD repo_error.

    TRY.
        lcl_url=>host( 'not a real url' ).                  "#EC NOTEXT
        cl_abap_unit_assert=>fail( ).
      CATCH lcx_exception.                              "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "repo_error

  METHOD repo_url.

    DATA: lv_host TYPE string.

    lv_host = lcl_url=>host( 'https://github.com/larshp/Foobar.git' ).

    cl_abap_unit_assert=>assert_equals(
        exp = 'https://github.com'
        act = lv_host ).

  ENDMETHOD.                    "repo_url

ENDCLASS.

CLASS ltcl_abap_unit IMPLEMENTATION.

  METHOD convert_int.

    DATA: lv_xstring TYPE xstring,
          lv_input   TYPE i,
          lv_result  TYPE i.


    DO 1000 TIMES.
      lv_input = sy-index.
      lv_xstring = lcl_convert=>int_to_xstring( iv_i      = lv_input
                                                iv_length = 4 ).
      lv_result = lcl_convert=>xstring_to_int( lv_xstring ).

      cl_abap_unit_assert=>assert_equals(
          exp = lv_input
          act = lv_result ).
    ENDDO.

  ENDMETHOD.                    "convert_int

  METHOD encode_decode_pack_multiple.

    CONSTANTS: lc_data TYPE x LENGTH 15 VALUE '123456789ABCDEF545794254754554',
               lc_sha  TYPE t_sha1 VALUE '5f46cb3c4b7f0b3600b64f744cde614a283a88dc'.

    DATA: lt_objects TYPE tt_objects,
          ls_object  LIKE LINE OF lt_objects,
          lt_nodes   TYPE tt_nodes,
          ls_node    LIKE LINE OF lt_nodes,
          ls_commit  TYPE st_commit,
          lt_result  TYPE tt_objects,
          lv_data    TYPE xstring.


* blob
    lv_data = lc_data.
    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_blob iv_data = lv_data ).
    ls_object-type = gc_blob.
    ls_object-data = lv_data.
    APPEND ls_object TO lt_objects.

* commit
    CLEAR ls_commit.
    ls_commit-tree      = lc_sha.
    ls_commit-parent    = lc_sha.
    ls_commit-author    = 'John Foobar'.
    ls_commit-committer = 'John Foobar'.
    ls_commit-body      = 'body'.
    lv_data = lcl_pack=>encode_commit( ls_commit ).
    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_commit iv_data = lv_data ).
    ls_object-type = gc_commit.
    ls_object-data = lv_data.
    APPEND ls_object TO lt_objects.

* tree
    CLEAR ls_node.
    ls_node-chmod     = '12456'.
    ls_node-name      = 'foobar.abap'.
    ls_node-sha1      = lc_sha.
    APPEND ls_node TO lt_nodes.
    lv_data = lcl_pack=>encode_tree( lt_nodes ).
    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_tree iv_data = lv_data ).
    ls_object-type = gc_tree.
    ls_object-data = lv_data.
    APPEND ls_object TO lt_objects.


    CLEAR lv_data.
    lv_data = lcl_pack=>encode( lt_objects ).
    lt_result = lcl_pack=>decode( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_objects
        act = lt_result ).

  ENDMETHOD.                    "encode_decode_pack_multiple

  METHOD encode_decode_pack_short.

    CONSTANTS: lc_data TYPE x LENGTH 8 VALUE '0123456789ABCDEF'.

    DATA: lt_objects TYPE tt_objects,
          ls_object  LIKE LINE OF lt_objects,
          lt_result  TYPE tt_objects,
          lv_data    TYPE xstring.


    lv_data = lc_data.

    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_blob
                                     iv_data = lv_data ).
    ls_object-type = gc_blob.
    ls_object-data = lv_data.
    APPEND ls_object TO lt_objects.

    CLEAR lv_data.
    lv_data = lcl_pack=>encode( lt_objects ).
    lt_result = lcl_pack=>decode( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_objects
        act = lt_result ).

  ENDMETHOD.                    "encode_decode_pack

  METHOD encode_decode_pack_long.

    CONSTANTS: lc_data TYPE x LENGTH 8 VALUE '0123456789ABCDEF'.

    DATA: lt_objects TYPE tt_objects,
          ls_object  LIKE LINE OF lt_objects,
          lv_xstring TYPE xstring,
          lt_result  TYPE tt_objects,
          lv_data    TYPE xstring.


    lv_xstring = lc_data.
    DO 20 TIMES.
      CONCATENATE lv_xstring lv_data INTO lv_data IN BYTE MODE.
    ENDDO.

    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_blob
                                     iv_data = lv_data ).
    ls_object-type = gc_blob.
    ls_object-data = lv_data.
    APPEND ls_object TO lt_objects.

    CLEAR lv_data.
    lv_data = lcl_pack=>encode( lt_objects ).
    lt_result = lcl_pack=>decode( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_objects
        act = lt_result ).

  ENDMETHOD.                    "encode_decode_pack_long

  METHOD encode_decode_tree.

    CONSTANTS: lc_sha TYPE t_sha1 VALUE '5f46cb3c4b7f0b3600b64f744cde614a283a88dc'.

    DATA: lt_nodes  TYPE tt_nodes,
          ls_node   LIKE LINE OF lt_nodes,
          lv_data   TYPE xstring,
          lt_result TYPE tt_nodes.

    CLEAR ls_node.
    ls_node-chmod = gc_chmod-file.
    ls_node-name = 'foobar.txt'.
    ls_node-sha1 = lc_sha.
    APPEND ls_node TO lt_nodes.

    lv_data = lcl_pack=>encode_tree( lt_nodes ).
    lt_result = lcl_pack=>decode_tree( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_nodes
        act = lt_result ).

  ENDMETHOD.                    "tree_encode_decode

  METHOD encode_decode_commit.

    CONSTANTS: lc_tree   TYPE t_sha1 VALUE '5f46cb3c4b7f0b3600b64f744cde614a283a88dc',
               lc_parent TYPE t_sha1 VALUE '1236cb3c4b7f0b3600b64f744cde614a283a88dc'.

    DATA: ls_commit TYPE st_commit,
          ls_result TYPE st_commit,
          lv_data   TYPE xstring.


    ls_commit-tree      = lc_tree.
    ls_commit-parent    = lc_parent.
    ls_commit-author    = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-committer = 'larshp <larshp@hotmail.com> 1387823471 +0100'.
    ls_commit-body      = 'very informative'.

    lv_data = lcl_pack=>encode_commit( ls_commit ).
    ls_result = lcl_pack=>decode_commit( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = ls_commit
        act = ls_result ).

  ENDMETHOD.                    "commit_encode_decode

ENDCLASS.                    "lcl_abap_unit IMPLEMENTATION
