REPORT zabapgit.

* See https://github.com/larshp/abapGit/

CONSTANTS: gc_version TYPE string VALUE 'v0.2-alpha'.       "#EC NOTEXT

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

TYPES: t_type     TYPE c LENGTH 6,
       t_bitbyte  TYPE c LENGTH 8,
       t_adler32  TYPE x LENGTH 4,
       t_sha1     TYPE x LENGTH 20,
       t_unixtime TYPE c LENGTH 16.

TYPES: BEGIN OF st_node,
         chmod     TYPE string,
         name      TYPE string,
         sha1      TYPE t_sha1,
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
         url TYPE string,
         branch_name TYPE string,
       END OF st_repo.

TYPES: BEGIN OF st_repo_sha1,
         url         TYPE string,
         branch_name TYPE string,
         sha1        TYPE string,
       END OF st_repo_sha1.
TYPES: tt_repos_sha1 TYPE STANDARD TABLE OF st_repo_sha1 WITH DEFAULT KEY.

TYPES: BEGIN OF st_result,
         obj_type TYPE tadir-object,
         obj_name TYPE tadir-obj_name,
         match    TYPE abap_bool,
         filename TYPE string,
       END OF st_result.
TYPES: tt_results TYPE STANDARD TABLE OF st_result WITH DEFAULT KEY.

TYPES: BEGIN OF st_diff,
         local TYPE string,
         result TYPE c LENGTH 1,
         remote TYPE string,
       END OF st_diff.
TYPES: tt_diffs TYPE STANDARD TABLE OF st_diff WITH DEFAULT KEY.

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

CONSTANTS: gc_chmod_file TYPE c LENGTH 6 VALUE '100644',
           gc_chmod_dir  TYPE c LENGTH 5 VALUE '40000'.

CONSTANTS: gc_newline TYPE abap_char1 VALUE cl_abap_char_utilities=>newline.

DEFINE _raise.
  raise exception type lcx_exception
    exporting
      iv_text = &1.                                         "#EC NOTEXT
END-OF-DEFINITION.

******************

START-OF-SELECTION.
  PERFORM run.

*----------------------------------------------------------------------*
*       CLASS CX_LOCAL_EXCEPTION DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcx_exception DEFINITION INHERITING FROM cx_static_check FINAL.

  PUBLIC SECTION.
    DATA mv_text TYPE string.
    METHODS constructor IMPORTING iv_text TYPE string.

ENDCLASS.                    "CX_LOCAL_EXCEPTION DEFINITION

*----------------------------------------------------------------------*
*       CLASS CX_LOCAL_EXCEPTION IMPLEMENTATION
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
*       CLASS lcl_user DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_user DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: set_username IMPORTING iv_username TYPE string
                                RAISING lcx_exception.

    CLASS-METHODS: get_username RETURNING value(rv_username) TYPE string
                                RAISING lcx_exception.

    CLASS-METHODS: set_email IMPORTING iv_email TYPE string
                             RAISING lcx_exception.

    CLASS-METHODS: get_email RETURNING value(rv_email) TYPE string
                             RAISING lcx_exception.

  PRIVATE SECTION.

    CLASS-METHODS: read IMPORTING iv_name TYPE tdobname
                        RETURNING value(rv_value) TYPE string
                               RAISING lcx_exception.

    CLASS-METHODS: save IMPORTING iv_name TYPE tdobname
                                  iv_value TYPE string
                          RAISING lcx_exception.

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
        language                = 'E'
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
    ls_header-tdspras    = 'E'.
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
    METHODS constructor    IMPORTING iv_xml TYPE string OPTIONAL
                           RAISING lcx_exception.

    METHODS element_add    IMPORTING ig_element TYPE data
                                     ii_root TYPE REF TO if_ixml_element OPTIONAL
                           RAISING lcx_exception.

* METHODS element_read

    METHODS structure_add  IMPORTING ig_structure TYPE data
                                     ii_root TYPE REF TO if_ixml_element OPTIONAL
                           RAISING lcx_exception.

    METHODS structure_read IMPORTING ii_root TYPE REF TO if_ixml_element OPTIONAL
                           CHANGING cg_structure TYPE data
                           RAISING lcx_exception.

    METHODS table_add      IMPORTING it_table TYPE STANDARD TABLE
                                     iv_name TYPE string OPTIONAL
                                     ii_root TYPE REF TO if_ixml_element OPTIONAL
                           RAISING lcx_exception.

    METHODS table_read     IMPORTING ii_root TYPE REF TO if_ixml_element OPTIONAL
                                     iv_name TYPE string OPTIONAL
                           CHANGING ct_table TYPE STANDARD TABLE
                           RAISING lcx_exception.


    METHODS xml_render     RETURNING value(rv_string) TYPE string.

    METHODS xml_element    IMPORTING iv_name TYPE string
                           RETURNING value(ri_element) TYPE REF TO if_ixml_element.

    METHODS xml_add        IMPORTING ii_root TYPE REF TO if_ixml_element OPTIONAL
                                     ii_element TYPE REF TO if_ixml_element.

    METHODS xml_find       IMPORTING ii_root TYPE REF TO if_ixml_element OPTIONAL
                                     iv_name TYPE string
                           RETURNING value(ri_element) TYPE REF TO if_ixml_element.

  PRIVATE SECTION.

    DATA: mi_ixml    TYPE REF TO if_ixml,
          mi_xml_doc TYPE REF TO if_ixml_document,
          mi_root    TYPE REF TO if_ixml_element.

    METHODS special_names CHANGING cv_name TYPE string.

    METHODS error IMPORTING ii_parser TYPE REF TO if_ixml_parser
                  RAISING lcx_exception.

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
          lv_value     TYPE string,
          li_struct    TYPE REF TO if_ixml_element,
          lo_descr_ref TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <lg_any>  TYPE any,
                   <ls_comp> TYPE abap_compdescr.


    CLEAR cg_structure.

    lo_descr_ref ?= cl_abap_typedescr=>describe_by_data( cg_structure ).
    lv_name = lo_descr_ref->get_relative_name( ).
    IF lv_name IS INITIAL.
      _raise 'no name'.
    ENDIF.

    li_struct = xml_find( ii_root = ii_root
                          iv_name = lv_name ).
    IF NOT li_struct IS BOUND.
      RETURN.
    ENDIF.

    LOOP AT lo_descr_ref->components ASSIGNING <ls_comp>.

      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE cg_structure TO <lg_any>.

      lv_name = <ls_comp>-name.
      special_names( CHANGING cv_name = lv_name ).
      lv_value = li_struct->find_from_name( depth = 0 name = lv_name )->get_value( ).

      <lg_any> = lv_value.
    ENDLOOP.

  ENDMETHOD.                    "structure_read

  METHOD table_read.

    DATA: lv_name        TYPE string,
          li_root        TYPE REF TO if_ixml_element,
          lv_kind        TYPE abap_typecategory,
          lo_table_descr TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <lg_line> TYPE any.


    CLEAR ct_table[].

    lo_table_descr ?= cl_abap_typedescr=>describe_by_data( ct_table ).
    lv_name = lo_table_descr->get_relative_name( ).

    IF lv_name IS INITIAL.
      lv_name = iv_name.
    ENDIF.

    IF lv_name IS INITIAL.
      _raise 'no name'.
    ENDIF.

    li_root = xml_find( ii_root   = ii_root
                        iv_name   = lv_name ).
    IF NOT li_root IS BOUND.
      RETURN.
    ENDIF.

    lv_kind = lo_table_descr->get_table_line_type( )->kind.

    DO.
      APPEND INITIAL LINE TO ct_table ASSIGNING <lg_line>.
      CASE lv_kind.
        WHEN cl_abap_typedescr=>kind_struct.
          structure_read( EXPORTING ii_root     = li_root
                          CHANGING cg_structure = <lg_line> ).
        WHEN OTHERS.
          _raise 'unknown kind'.
      ENDCASE.

      IF <lg_line> IS INITIAL.
        DELETE ct_table INDEX lines( ct_table ).
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
          li_error TYPE REF TO if_ixml_parse_error.


    IF ii_parser->num_errors( ) <> 0.
      DO ii_parser->num_errors( ) TIMES.
        lv_error = sy-index - 1.
        li_error = ii_parser->get_error( lv_error ).

        lv_txt1 = 'Column:' && li_error->get_column( ).     "#EC NOTEXT
        lv_txt2 = 'Line:' && li_error->get_line( ).         "#EC NOTEXT
        lv_txt3 = li_error->get_reason( ).

        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = 'Error from XLM parser'                 "#EC NOTEXT
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
      IF li_parser->parse( ) <> 0.
        error( li_parser ).
      ENDIF.

      li_istream->close( ).

      mi_root = mi_xml_doc->find_from_name( depth = 0 name = 'abapGit' ).
    ELSE.
      mi_root = mi_xml_doc->create_element( 'abapGit' ).
      mi_root->set_attribute( name = 'version' value = gc_version ). "#EC NOTEXT
      mi_xml_doc->append_child( mi_root ).
    ENDIF.

  ENDMETHOD.                    "xml_root

  METHOD table_add.

    DATA: lv_name         TYPE string,
          li_table        TYPE REF TO if_ixml_element,
          lv_kind         TYPE abap_typecategory,
          lo_table_descr  TYPE REF TO cl_abap_tabledescr.

    FIELD-SYMBOLS: <lg_line> TYPE any.


    lo_table_descr ?= cl_abap_typedescr=>describe_by_data( it_table ).
    lv_name = lo_table_descr->get_relative_name( ).

    IF lv_name IS INITIAL.
      lv_name = iv_name.
    ENDIF.

    IF lv_name IS INITIAL.
      _raise 'no name'.
    ENDIF.

    li_table = mi_xml_doc->create_element( lv_name ).
    lv_kind = lo_table_descr->get_table_line_type( )->kind.

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

    lv_name = lo_descr->get_relative_name( ).
    IF lv_name IS INITIAL.
      _raise 'no name'.
    ENDIF.

    li_element = mi_xml_doc->create_element( lv_name ).

    lv_string  = ig_element.
    li_text    = mi_xml_doc->create_text( lv_string ).

    li_element->append_child( li_text ).

    xml_add( ii_root    = ii_root
             ii_element = li_element ).

  ENDMETHOD.                    "element_add

  METHOD structure_add.

    DATA: li_element   TYPE REF TO if_ixml_element,
          li_structure TYPE REF TO if_ixml_element,
          li_text      TYPE REF TO if_ixml_text,
          lv_string    TYPE string,
          lv_name      TYPE string,
          lo_descr     TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <ls_comp> TYPE abap_compdescr,
                   <lg_any>  TYPE any.


    lo_descr ?= cl_abap_typedescr=>describe_by_data( ig_structure ).

    lv_name = lo_descr->get_relative_name( ).
    IF lv_name IS INITIAL.
      _raise 'no name'.
    ENDIF.
    li_structure = mi_xml_doc->create_element( lv_name ).

    LOOP AT lo_descr->components ASSIGNING <ls_comp>.

      ASSIGN COMPONENT <ls_comp>-name OF STRUCTURE ig_structure TO <lg_any>.

      lv_name  = <ls_comp>-name.
      special_names( CHANGING cv_name = lv_name ).
      li_element = mi_xml_doc->create_element( lv_name ).

      lv_string  = <lg_any>.
      li_text    = mi_xml_doc->create_text( lv_string ).

      li_element->append_child( li_text ).

      li_structure->append_child( li_element ).
    ENDLOOP.

    xml_add( ii_root    = ii_root
             ii_element = li_structure ).

  ENDMETHOD.                    "structure_to_xml

  METHOD xml_render.

    DATA: li_ostream       TYPE REF TO if_ixml_ostream,
          li_renderer      TYPE REF TO if_ixml_renderer,
          li_streamfactory TYPE REF TO if_ixml_stream_factory.

* todo, the xml file says "encoding=utf-16" but its wrong

    li_streamfactory = mi_ixml->create_stream_factory( ).
    li_ostream = li_streamfactory->create_ostream_cstring( rv_string ).
    li_renderer = mi_ixml->create_renderer( ostream = li_ostream document = mi_xml_doc ).
    li_renderer->set_normalizing( ).
    li_renderer->render( ).

  ENDMETHOD.                    "xml_render

ENDCLASS.                    "lcl_xml IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS lcl_time DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_time DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS get RETURNING value(rv_time) TYPE t_unixtime
                      RAISING lcx_exception.

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
    CLASS-METHODS: host
                      IMPORTING iv_repo TYPE string
                      RETURNING value(rv_host) TYPE string
                      RAISING lcx_exception.

    CLASS-METHODS: name
                      IMPORTING iv_repo TYPE string
                      RETURNING value(rv_name) TYPE string
                      RAISING lcx_exception.

    CLASS-METHODS: path_name IMPORTING iv_repo TYPE string
                      RETURNING value(rv_path_name) TYPE string
                      RAISING lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS: regex IMPORTING iv_repo TYPE string
                         EXPORTING ev_host TYPE string
                                   ev_path TYPE string
                                   ev_name TYPE string
                         RAISING lcx_exception.

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
    CLASS-METHODS bitbyte_to_int IMPORTING iv_bits TYPE clike
                                 RETURNING value(rv_int) TYPE i.

    CLASS-METHODS x_to_bitbyte IMPORTING iv_x TYPE x
                               RETURNING value(rv_bitbyte) TYPE t_bitbyte.

    CLASS-METHODS string_to_xstring_utf8 IMPORTING iv_string TYPE string
                                RETURNING value(rv_xstring) TYPE xstring.

    CLASS-METHODS xstring_to_string_utf8 IMPORTING iv_data TYPE xstring
                                         RETURNING value(rv_string) TYPE string.

    CLASS-METHODS xstring_to_int IMPORTING iv_xstring TYPE xstring
                                 RETURNING value(rv_i) TYPE i
                                 RAISING lcx_exception.

    CLASS-METHODS int_to_xstring IMPORTING iv_i TYPE i
                                           iv_length TYPE i
                                 RETURNING value(rv_xstring) TYPE xstring.

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
    CLASS-METHODS diff IMPORTING iv_local TYPE xstring
                                 iv_remote TYPE xstring
                       RETURNING value(rt_diffs) TYPE tt_diffs.

ENDCLASS.                    "lcl_diff DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_diff IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_diff IMPLEMENTATION.

  METHOD diff.

* todo, this is way too simple, but will do for now

    DATA: lv_local  TYPE string,
          lv_remote TYPE string,
          lt_local  TYPE TABLE OF string,
          lt_remote TYPE TABLE OF string.

    FIELD-SYMBOLS: <lv_string> TYPE string,
                   <ls_diff>   LIKE LINE OF rt_diffs.


    lv_local = lcl_convert=>xstring_to_string_utf8( iv_local ).
    lv_remote = lcl_convert=>xstring_to_string_utf8( iv_remote ).

    SPLIT lv_local AT gc_newline INTO TABLE lt_local.
    SPLIT lv_remote AT gc_newline INTO TABLE lt_remote.


    LOOP AT lt_local ASSIGNING <lv_string>.
      APPEND INITIAL LINE TO rt_diffs ASSIGNING <ls_diff>.
      <ls_diff>-local = <lv_string>.
    ENDLOOP.

    LOOP AT lt_remote ASSIGNING <lv_string>.
      READ TABLE rt_diffs INDEX sy-tabix ASSIGNING <ls_diff>.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO rt_diffs ASSIGNING <ls_diff>.
      ENDIF.
      <ls_diff>-remote = <lv_string>.
    ENDLOOP.

    LOOP AT rt_diffs ASSIGNING <ls_diff>.
      IF <ls_diff>-local = <ls_diff>-remote.
        <ls_diff>-result = '='.
      ELSE.
        <ls_diff>-result = '!'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "diff

ENDCLASS.                    "lcl_diff IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_common DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_common DEFINITION ABSTRACT.

  PUBLIC SECTION.
    CLASS-DATA: gt_ddic     TYPE TABLE OF dwinactiv,
                gt_programs TYPE TABLE OF dwinactiv.

  PROTECTED SECTION.
    CLASS-METHODS: xml_to_file IMPORTING is_item TYPE st_item
                                         io_xml  TYPE REF TO lcl_xml
                               RETURNING value(rs_file) TYPE st_file
                               RAISING lcx_exception.

    CLASS-METHODS: read_xml IMPORTING is_item  TYPE st_item
                                      it_files TYPE tt_files
                            RETURNING value(ro_xml) TYPE REF TO lcl_xml
                            RAISING lcx_exception.

    CLASS-METHODS: read_abap IMPORTING is_item  TYPE st_item
                                       iv_extra TYPE string OPTIONAL
                                       it_files TYPE tt_files
                                       iv_error TYPE abap_bool DEFAULT abap_true
                            CHANGING ct_abap TYPE STANDARD TABLE
                            RAISING lcx_exception.

    CLASS-METHODS: abap_to_file IMPORTING is_item TYPE st_item
                                          iv_extra TYPE string OPTIONAL
                                          it_abap TYPE STANDARD TABLE
                                RETURNING value(rs_file) TYPE st_file
                                RAISING lcx_exception.

    CLASS-METHODS: activation_add IMPORTING iv_type TYPE trobjtype
                                            iv_name TYPE clike
                            RAISING lcx_exception.

    CLASS-METHODS: corr_insert IMPORTING is_item TYPE st_item
                               RAISING lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS: filename IMPORTING is_item  TYPE st_item
                                      iv_extra TYPE string OPTIONAL
                                      iv_ext   TYPE string
                            RETURNING value(rv_filename) TYPE string.

ENDCLASS.                    "lcl_serialize_common DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_common IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_common IMPLEMENTATION.

  METHOD corr_insert.

    DATA: ls_object TYPE ddenqs.


    ls_object-objtype = is_item-obj_type.
    ls_object-objname = is_item-obj_name.

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = ls_object
        object_class        = 'DICT'
        master_language     = sy-langu
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
      WHEN 'CLAS'.
        CALL FUNCTION 'RS_INACTIVE_OBJECTS_IN_OBJECT'
          EXPORTING
            obj_name         = lv_obj_name
            object           = iv_type
          TABLES
            inactive_objects = lt_objects.
        APPEND LINES OF lt_objects TO lcl_serialize_common=>gt_programs.
      WHEN 'DOMA' OR 'DTEL' OR 'TABL' OR 'INDX' OR 'TTYP' OR 'VIEW' OR 'SHLP' OR 'ENQU'.
* todo also insert_into_working_area?
        APPEND INITIAL LINE TO lcl_serialize_common=>gt_ddic ASSIGNING <ls_object>.
        <ls_object>-object   = iv_type.
        <ls_object>-obj_name = lv_obj_name.
      WHEN 'REPS' OR 'DYNP' OR 'CUAD' OR 'REPT'.
* these seem to go into the workarea automatically
        APPEND INITIAL LINE TO lcl_serialize_common=>gt_programs ASSIGNING <ls_object>.
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


    lv_xml = io_xml->xml_render( ).
    rs_file-path = '/'.

    rs_file-filename = filename( is_item  = is_item
                                 iv_ext   = 'xml' ).        "#EC NOTEXT

    rs_file-data = lcl_convert=>string_to_xstring_utf8( lv_xml ).

  ENDMETHOD.                    "do

ENDCLASS.                    "lcl_serialize_common IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_doma DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_doma DEFINITION INHERITING FROM lcl_serialize_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: serialize IMPORTING is_item TYPE st_item
                             RETURNING value(rt_files) TYPE tt_files
                             RAISING lcx_exception.

    CLASS-METHODS: deserialize IMPORTING is_item TYPE st_item
                                         it_files TYPE tt_files
                               RAISING lcx_exception.

ENDCLASS.                    "lcl_serialize_doma DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_doma IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_doma IMPLEMENTATION.

  METHOD serialize.

    DATA: lv_name  TYPE ddobjname,
          ls_dd01v TYPE dd01v,
          lt_dd07v TYPE dd07v_tab,
          ls_file  TYPE st_file,
          lo_xml   TYPE REF TO lcl_xml.


    lv_name = is_item-obj_name.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = lv_name
        langu         = sy-langu
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

* todo, translated texts?

    CLEAR: ls_dd01v-as4user,
           ls_dd01v-as4date,
           ls_dd01v-as4time.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_dd01v ).
    lo_xml->table_add( lt_dd07v ).

    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.                    "serialize

  METHOD deserialize.

* package SEDD
* package SDIC

* fm TR_TADIR_INTERFACE
* fm RS_CORR_INSERT ?

    DATA: lo_xml    TYPE REF TO lcl_xml,
          ls_dd01v  TYPE dd01v,
          lv_name   TYPE ddobjname,
          lt_dd07v  TYPE dd07v_tab.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    lo_xml->structure_read( CHANGING cg_structure = ls_dd01v ).
    lo_xml->table_read( CHANGING ct_table = lt_dd07v ).

    corr_insert( is_item ).

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

ENDCLASS.                    "lcl_serialize_doma IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_dtel DEFINITION INHERITING FROM lcl_serialize_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: serialize IMPORTING is_item TYPE st_item
                             RETURNING value(rt_files) TYPE tt_files
                             RAISING lcx_exception.

    CLASS-METHODS: deserialize IMPORTING is_item TYPE st_item
                                         it_files TYPE tt_files
                               RAISING lcx_exception.

ENDCLASS.                    "lcl_serialize_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_dtel IMPLEMENTATION.

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
        langu         = sy-langu
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

* todo, translated texts?

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

    corr_insert( is_item ).

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

ENDCLASS.                    "lcl_serialize_dtel IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_clas DEFINITION INHERITING FROM lcl_serialize_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: serialize IMPORTING is_item TYPE st_item
                             RETURNING value(rt_files) TYPE tt_files
                             RAISING lcx_exception.

    CLASS-METHODS: deserialize IMPORTING is_item TYPE st_item
                                         it_files TYPE tt_files
                               RAISING lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS: deserialize_abap IMPORTING is_item  TYPE st_item
                                              it_files TYPE tt_files
                                              io_xml   TYPE REF TO lcl_xml
                                    RAISING lcx_exception.

    CLASS-METHODS: deserialize_textpool IMPORTING is_item  TYPE st_item
                                                  io_xml   TYPE REF TO lcl_xml
                                        RAISING lcx_exception.

    CLASS-METHODS: deserialize_docu IMPORTING is_item  TYPE st_item
                                              io_xml   TYPE REF TO lcl_xml
                                    RAISING lcx_exception.

    CLASS-METHODS exists IMPORTING is_clskey TYPE seoclskey
                         RETURNING value(rv_exists) TYPE abap_bool.

    CLASS-METHODS serialize_abap IMPORTING is_clskey TYPE seoclskey
                                 RETURNING value(rt_source) TYPE seop_source_string
                                 RAISING lcx_exception.

    CLASS-METHODS serialize_locals_imp IMPORTING is_clskey TYPE seoclskey
                                 RETURNING value(rt_source) TYPE seop_source_string
                                 RAISING lcx_exception.

    CLASS-METHODS serialize_locals_def IMPORTING is_clskey TYPE seoclskey
                                 RETURNING value(rt_source) TYPE seop_source_string
                                 RAISING lcx_exception.

    CLASS-METHODS serialize_testclasses IMPORTING is_clskey TYPE seoclskey
                                 RETURNING value(rt_source) TYPE seop_source_string
                                 RAISING lcx_exception.

    CLASS-METHODS serialize_macros IMPORTING is_clskey TYPE seoclskey
                                 RETURNING value(rt_source) TYPE seop_source_string
                                 RAISING lcx_exception.

    CLASS-METHODS serialize_xml IMPORTING is_clskey TYPE seoclskey
                                RETURNING value(ro_xml) TYPE REF TO lcl_xml
                                RAISING lcx_exception.

    CLASS-METHODS remove_signatures CHANGING ct_source TYPE seop_source_string.

    CLASS-METHODS reduce CHANGING ct_source TYPE seop_source_string.

ENDCLASS.                    "lcl_serialize_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_clas IMPLEMENTATION.

  METHOD reduce.

    DATA: lv_source LIKE LINE OF ct_source,
          lv_found  TYPE abap_bool.


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
    IF sy-subrc <> 0.
      _raise 'Error from get_include_source'.
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
    IF sy-subrc <> 0.
      _raise 'Error from get_include_source'.
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
      _raise 'Error from get_include_source'.
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
    IF sy-subrc <> 0.
      _raise 'Error from get_include_source'.
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

    CONSTANTS:
      lc_begin TYPE string VALUE '* <SIGNATURE>---------------------------------------------------------------------------------------+',
      lc_end   TYPE string VALUE '* +--------------------------------------------------------------------------------------</SIGNATURE>'.

    DATA: lv_remove TYPE abap_bool,
          lv_source LIKE LINE OF ct_source.


    lv_remove = abap_false.
    LOOP AT ct_source INTO lv_source.
      IF lv_source = lc_begin.
        lv_remove = abap_true.
      ENDIF.
      IF lv_remove = abap_true.
        DELETE ct_source INDEX sy-tabix.
      ENDIF.
      IF lv_source = lc_end.
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

    DATA: lt_source     TYPE seop_source_string,
          ls_file       TYPE st_file,
          lo_xml        TYPE REF TO lcl_xml,
          ls_clskey     TYPE seoclskey.


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

    lt_source = serialize_locals_def( ls_clskey ).
    IF NOT lt_source[] IS INITIAL.
      ls_file = abap_to_file( is_item  = is_item
                              iv_extra = 'locals_def'
                              it_abap  = lt_source ).       "#EC NOTEXT
      APPEND ls_file TO rt_files.
    ENDIF.

    lt_source = serialize_locals_imp( ls_clskey ).
    IF NOT lt_source[] IS INITIAL.
      ls_file = abap_to_file( is_item  = is_item
                              iv_extra = 'locals_imp'
                              it_abap  = lt_source ).       "#EC NOTEXT
      APPEND ls_file TO rt_files.
    ENDIF.

    lt_source = serialize_testclasses( ls_clskey ).
    IF NOT lt_source[] IS INITIAL.
      ls_file = abap_to_file( is_item  = is_item
                              iv_extra = 'testclasses'
                              it_abap  = lt_source ).       "#EC NOTEXT
      APPEND ls_file TO rt_files.
    ENDIF.

    lt_source = serialize_macros( ls_clskey ).
    IF NOT lt_source[] IS INITIAL.
      ls_file = abap_to_file( is_item  = is_item
                              iv_extra = 'macros'
                              it_abap  = lt_source ).       "#EC NOTEXT
      APPEND ls_file TO rt_files.
    ENDIF.

    lo_xml = serialize_xml( ls_clskey ).
    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.                    "serialize

  METHOD serialize_xml.

    DATA: ls_vseoclass TYPE vseoclass,
          lv_cp        TYPE program,
          lt_tpool     TYPE textpool_table,
          lv_object    TYPE dokhl-object,
          lv_state     TYPE dokhl-dokstate,
          lt_lines     TYPE tlinetab.


    CALL FUNCTION 'SEO_CLASS_GET'
      EXPORTING
        clskey       = is_clskey
        version      = seoc_version_active
      IMPORTING
        class        = ls_vseoclass
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        is_interface = 3
        model_only   = 4
        OTHERS       = 5.
    IF sy-subrc <> 0.
      _raise 'error from seo_class_get'.
    ENDIF.

    CLEAR: ls_vseoclass-uuid,
           ls_vseoclass-author,
           ls_vseoclass-createdon,
           ls_vseoclass-changedby,
           ls_vseoclass-changedon.

    CREATE OBJECT ro_xml.
    ro_xml->structure_add( ls_vseoclass ).


    lv_cp = cl_oo_classname_service=>get_classpool_name( is_clskey-clsname ).
    READ TEXTPOOL lv_cp INTO lt_tpool LANGUAGE sy-langu. "#EC CI_READ_REP
    ro_xml->table_add( lt_tpool ).


    lv_object = is_clskey-clsname.
    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id                = 'CL'
        langu             = 'E'
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
                      io_xml   = lo_xml ).

    deserialize_textpool( is_item = is_item
                          io_xml  = lo_xml ).

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
        langu    = 'E'
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
      LANGUAGE sy-langu
      STATE 'I'.                                     "#EC CI_INSERT_REP
    IF sy-subrc <> 0.
      _raise 'error from INSERT TEXTPOOL'.
    ENDIF.

    activation_add( iv_type = 'REPT'
                    iv_name = lv_cp ).

  ENDMETHOD.                    "deserialize_textpool

  METHOD deserialize_abap.

    DATA: ls_vseoclass   TYPE vseoclass,
          lt_source      TYPE seop_source_string,
          lo_source      TYPE REF TO cl_oo_source,
          lt_locals_def  TYPE seop_source_string,
          lt_locals_imp  TYPE seop_source_string,
          lt_locals_mac  TYPE seop_source_string,
          lt_testclasses TYPE seop_source_string,
          ls_clskey      TYPE seoclskey.


    io_xml->structure_read( CHANGING cg_structure = ls_vseoclass ).

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



    CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
      EXPORTING
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

ENDCLASS.                    "lcl_serialize_CLAS IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_fugr DEFINITION INHERITING FROM lcl_serialize_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: serialize IMPORTING is_item TYPE st_item
                             RETURNING value(rt_files) TYPE tt_files
                             RAISING lcx_exception.

    CLASS-METHODS: deserialize IMPORTING is_item TYPE st_item
                                         it_files TYPE tt_files
                               RAISING lcx_exception.

ENDCLASS.                    "lcl_serialize_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_fugr IMPLEMENTATION.

  METHOD serialize.
    _raise 'todo'.
  ENDMETHOD.                    "serialize

  METHOD deserialize.
    _raise 'todo'.
  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_serialize_FUGR IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_tabl DEFINITION INHERITING FROM lcl_serialize_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: serialize IMPORTING is_item TYPE st_item
                             RETURNING value(rt_files) TYPE tt_files
                             RAISING lcx_exception.

    CLASS-METHODS: deserialize IMPORTING is_item TYPE st_item
                                         it_files TYPE tt_files
                               RAISING lcx_exception.

ENDCLASS.                    "lcl_serialize_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_tabl IMPLEMENTATION.

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
        langu         = sy-langu
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

    DATA: lv_name  TYPE ddobjname,
          lv_tname TYPE trobj_name,
          lo_xml   TYPE REF TO lcl_xml,
          ls_dd02v TYPE dd02v,
          ls_dd09l TYPE dd09l,
          lt_dd03p TYPE TABLE OF dd03p,
          lt_dd05m TYPE TABLE OF dd05m,
          lt_dd08v TYPE TABLE OF dd08v,
          lt_dd12v TYPE dd12vtab,
          lt_dd17v TYPE dd17vtab,
          ls_dd17v LIKE LINE OF lt_dd17v,
          lt_secondary LIKE lt_dd17v,
          lt_dd35v TYPE TABLE OF dd35v,
          lt_dd36m TYPE dd36mttyp,
          ls_dd12v LIKE LINE OF lt_dd12v.


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

    corr_insert( is_item ).

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

      REFRESH lt_secondary.
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

      lv_tname = ls_dd12v-sqltab.
      lv_tname+10 = ls_dd12v-indexname.
      activation_add( iv_type = 'INDX'
                      iv_name = lv_tname ).

    ENDLOOP.

  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_serialize_TABL IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_msag DEFINITION INHERITING FROM lcl_serialize_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: serialize IMPORTING is_item TYPE st_item
                             RETURNING value(rt_files) TYPE tt_files
                             RAISING lcx_exception.

    CLASS-METHODS: deserialize IMPORTING is_item TYPE st_item
                                         it_files TYPE tt_files
                               RAISING lcx_exception.

ENDCLASS.                    "lcl_serialize_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_msag IMPLEMENTATION.

  METHOD serialize.
    _raise 'todo'.
  ENDMETHOD.                    "serialize

  METHOD deserialize.
    _raise 'todo'.
  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_serialize_MSAG IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_tran DEFINITION INHERITING FROM lcl_serialize_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: serialize IMPORTING is_item TYPE st_item
                             RETURNING value(rt_files) TYPE tt_files
                             RAISING lcx_exception.

    CLASS-METHODS: deserialize IMPORTING is_item TYPE st_item
                                         it_files TYPE tt_files
                               RAISING lcx_exception.

ENDCLASS.                    "lcl_serialize_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_tran IMPLEMENTATION.

  METHOD serialize.
    _raise 'todo'.
  ENDMETHOD.                    "serialize

  METHOD deserialize.
    _raise 'todo'.
  ENDMETHOD.                    "deserialize

ENDCLASS.                    "lcl_serialize_TRAN IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_enqu DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_enqu DEFINITION INHERITING FROM lcl_serialize_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: serialize IMPORTING is_item TYPE st_item
                             RETURNING value(rt_files) TYPE tt_files
                             RAISING lcx_exception.

    CLASS-METHODS: deserialize IMPORTING is_item TYPE st_item
                                         it_files TYPE tt_files
                               RAISING lcx_exception.

ENDCLASS.                    "lcl_serialize_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_enqu IMPLEMENTATION.

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
        langu         = sy-langu
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

    corr_insert( is_item ).

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

ENDCLASS.                    "lcl_serialize_enqu IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_shlp DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_shlp DEFINITION INHERITING FROM lcl_serialize_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: serialize IMPORTING is_item TYPE st_item
                             RETURNING value(rt_files) TYPE tt_files
                             RAISING lcx_exception.

    CLASS-METHODS: deserialize IMPORTING is_item TYPE st_item
                                         it_files TYPE tt_files
                               RAISING lcx_exception.

ENDCLASS.                    "lcl_serialize_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_shlp IMPLEMENTATION.

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
        langu         = sy-langu
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

    corr_insert( is_item ).

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

ENDCLASS.                    "lcl_serialize_shlp IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_view DEFINITION INHERITING FROM lcl_serialize_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: serialize IMPORTING is_item TYPE st_item
                             RETURNING value(rt_files) TYPE tt_files
                             RAISING lcx_exception.

    CLASS-METHODS: deserialize IMPORTING is_item TYPE st_item
                                         it_files TYPE tt_files
                               RAISING lcx_exception.

ENDCLASS.                    "lcl_serialize_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_view IMPLEMENTATION.

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
        langu         = sy-langu
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

    corr_insert( is_item ).

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

ENDCLASS.                    "lcl_serialize_view IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_ttyp DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_ttyp DEFINITION INHERITING FROM lcl_serialize_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: serialize IMPORTING is_item TYPE st_item
                             RETURNING value(rt_files) TYPE tt_files
                             RAISING lcx_exception.

    CLASS-METHODS: deserialize IMPORTING is_item TYPE st_item
                                         it_files TYPE tt_files
                               RAISING lcx_exception.

ENDCLASS.                    "lcl_serialize_dtel DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_dtel IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_ttyp IMPLEMENTATION.

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
        langu         = sy-langu
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

    corr_insert( is_item ).

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

ENDCLASS.                    "lcl_serialize_ttyp IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_prog DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_prog DEFINITION INHERITING FROM lcl_serialize_common FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: serialize  IMPORTING is_item TYPE st_item
                              RETURNING value(rt_files) TYPE tt_files
                              RAISING lcx_exception.

    CLASS-METHODS: deserialize
                              IMPORTING is_item TYPE st_item
                                       it_files TYPE tt_files
                              RAISING lcx_exception.


  PRIVATE SECTION.
    CLASS-METHODS: serialize_dynpros
                              IMPORTING iv_program_name TYPE programm
                                        io_xml TYPE REF TO lcl_xml
                              RAISING lcx_exception.

    CLASS-METHODS: serialize_cua
                              IMPORTING iv_program_name TYPE programm
                                        io_xml TYPE REF TO lcl_xml
                              RAISING lcx_exception.

    CLASS-METHODS: serialize_textpool
                              IMPORTING iv_program_name TYPE programm
                                        io_xml TYPE REF TO lcl_xml
                              RAISING lcx_exception.

    CLASS-METHODS: deserialize_dynpros
                              IMPORTING io_xml   TYPE REF TO lcl_xml
                              RAISING lcx_exception.

    CLASS-METHODS: deserialize_cua
                              IMPORTING io_xml  TYPE REF TO lcl_xml
                                        is_item TYPE st_item
                              RAISING lcx_exception.

    CLASS-METHODS: deserialize_textpool
                              IMPORTING it_tpool TYPE textpool_table
                                        is_item  TYPE st_item
                              RAISING lcx_exception.

    CLASS-METHODS: deserialize_abap
                              IMPORTING is_item     TYPE st_item
                                        io_xml      TYPE REF TO lcl_xml
                                        it_source   TYPE abaptxt255_tab
                                        it_tpool    TYPE textpool_table
                              RAISING lcx_exception.

    CLASS-METHODS exists IMPORTING iv_obj_name TYPE tadir-obj_name
                         RETURNING value(rv_exists) TYPE abap_bool.

ENDCLASS.                    "lcl_serialize_prog DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize_prog IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize_prog IMPLEMENTATION.

  METHOD deserialize_textpool.

    READ TABLE it_tpool WITH KEY id = 'R' TRANSPORTING NO FIELDS.
    IF ( sy-subrc = 0 AND lines( it_tpool ) = 1 ) OR lines( it_tpool ) = 0.
      RETURN. " no action for includes
    ENDIF.

    INSERT TEXTPOOL is_item-obj_name
      FROM it_tpool
      LANGUAGE sy-langu
      STATE 'I'.                                     "#EC CI_INSERT_REP
    IF sy-subrc <> 0.
      _raise 'error from INSERT TEXTPOOL'.
    ENDIF.

    activation_add( iv_type = 'REPT'
                    iv_name = is_item-obj_name ).

  ENDMETHOD.                    "deserialize_textpool

  METHOD serialize_textpool.

* function modules RS_TEXTPOOL_* cannot be used as these dont work for includes

    DATA: lt_tpool TYPE textpool_table.


    READ TEXTPOOL iv_program_name INTO lt_tpool LANGUAGE sy-langu. "#EC CI_READ_REP
    io_xml->table_add( lt_tpool ).

  ENDMETHOD.                    "serialize_textpool

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
      AND obj_name = is_item-obj_name.
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
        language  = sy-langu
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
        language        = sy-langu
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

  METHOD serialize.

    DATA: ls_progdir      TYPE progdir,
          lv_program_name TYPE programm,
          lt_source       TYPE TABLE OF abaptxt255,
          ls_file         LIKE LINE OF rt_files,
          lo_xml          TYPE REF TO lcl_xml.


    lv_program_name = is_item-obj_name.

    IF exists( lv_program_name ) = abap_false.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RPY_PROGRAM_READ'
      EXPORTING
        program_name     = lv_program_name
        with_lowercase   = abap_true
      TABLES
        source_extended  = lt_source
      EXCEPTIONS
        cancelled        = 1
        not_found        = 2
        permission_error = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      _raise 'Error reading program'.
    ENDIF.

    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = lv_program_name
        i_state    = 'A'
      IMPORTING
        e_progdir  = ls_progdir.

    CLEAR: ls_progdir-edtx,
           ls_progdir-cnam,
           ls_progdir-cdat,
           ls_progdir-unam,
           ls_progdir-udat,
           ls_progdir-vern,
           ls_progdir-rmand,
           ls_progdir-sdate,
           ls_progdir-stime,
           ls_progdir-idate,
           ls_progdir-itime.

    CREATE OBJECT lo_xml.
    lo_xml->structure_add( ls_progdir ).
    IF ls_progdir-subc = '1'.
      serialize_dynpros( EXPORTING iv_program_name = lv_program_name
                                   io_xml          = lo_xml ).
      serialize_cua( EXPORTING iv_program_name = lv_program_name
                               io_xml          = lo_xml ).
    ENDIF.

    serialize_textpool( EXPORTING iv_program_name = lv_program_name
                                  io_xml          = lo_xml ).

    ls_file = xml_to_file( is_item = is_item
                           io_xml  = lo_xml ).
    APPEND ls_file TO rt_files.

    ls_file = abap_to_file( is_item = is_item
                            it_abap = lt_source ).
    APPEND ls_file TO rt_files.

  ENDMETHOD.                    "lif_serialize~serialize

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

  METHOD deserialize.

    DATA: lo_xml      TYPE REF TO lcl_xml,
          lt_tpool    TYPE textpool_table,
          lt_source   TYPE abaptxt255_tab.


    lo_xml = read_xml( is_item  = is_item
                       it_files = it_files ).

    read_abap( EXPORTING is_item  = is_item
                         it_files = it_files
               CHANGING  ct_abap  = lt_source ).

    lo_xml->table_read( CHANGING  ct_table  = lt_tpool ).

    deserialize_abap( is_item     = is_item
                      io_xml      = lo_xml
                      it_source   = lt_source
                      it_tpool    = lt_tpool ).

    deserialize_dynpros( lo_xml ).

    deserialize_cua( is_item = is_item
                     io_xml  = lo_xml ).

    deserialize_textpool( is_item = is_item
                          it_tpool = lt_tpool
                          ).

  ENDMETHOD.                    "lif_serialize~deserialize

  METHOD deserialize_dynpros.

    DATA: li_element              TYPE REF TO if_ixml_element,
          ls_header               TYPE rpy_dyhead,
          lt_containers           TYPE dycatt_tab,
          ls_item                 TYPE st_item,
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

  METHOD deserialize_abap.

    DATA: ls_tpool       LIKE LINE OF it_tpool,
          ls_progdir     TYPE progdir,
          lv_title       TYPE rglif-title,
          ls_item        LIKE is_item,
          ls_progdir_new TYPE progdir.


    io_xml->structure_read( CHANGING cg_structure = ls_progdir ).


    READ TABLE it_tpool INTO ls_tpool WITH KEY id = 'R'.
    lv_title = ls_tpool-entry.

    IF exists( is_item-obj_name ) = abap_true.
      CALL FUNCTION 'RPY_PROGRAM_UPDATE'
        EXPORTING
          program_name     = ls_progdir-name
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
      CALL FUNCTION 'RPY_PROGRAM_INSERT'
        EXPORTING
          application         = ls_progdir-appl
          authorization_group = ls_progdir-secu
          program_name        = ls_progdir-name
          program_type        = ls_progdir-subc
          title_string        = lv_title
          save_inactive       = 'I'
        TABLES
          source_extended     = it_source
        EXCEPTIONS
          already_exists      = 1
          cancelled           = 2
          name_not_allowed    = 3
          permission_error    = 4
          OTHERS              = 5.
      IF sy-subrc <> 0.
        _raise 'PROG, error inserting'.
      ENDIF.

      CALL FUNCTION 'READ_PROGDIR'
        EXPORTING
          i_progname = ls_progdir-name
          i_state    = 'I'
        IMPORTING
          e_progdir  = ls_progdir_new
        EXCEPTIONS
          not_exists = 1
          OTHERS     = 2.
      IF sy-subrc <> 0.
        _raise 'not found in PROGDIR'.
      ENDIF.

      ls_progdir_new-ldbname = ls_progdir-ldbname.
      ls_progdir_new-dbapl = ls_progdir-dbapl.
      ls_progdir_new-rload = ls_progdir-rload.
      ls_progdir_new-fixpt = ls_progdir-fixpt.

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
                    iv_name = is_item-obj_name ).

  ENDMETHOD.                    "lif_serialize~deserialize

  METHOD exists.

    DATA: lv_progname TYPE reposrc-progname.

* function group SEUEXIST
    SELECT SINGLE progname FROM reposrc INTO lv_progname
      WHERE progname = iv_obj_name.                         "#EC WARNOK
    IF sy-subrc = 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.

  ENDMETHOD.                    "exists

ENDCLASS.                    "lcl_serialize_prog IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS serialize IMPORTING is_item TYPE st_item
                            RETURNING value(rt_files) TYPE tt_files
                            RAISING lcx_exception.

    CLASS-METHODS status    IMPORTING it_files TYPE tt_files
                            RETURNING value(rt_results) TYPE tt_results
                            RAISING lcx_exception.

    CLASS-METHODS deserialize IMPORTING it_files TYPE tt_files
                            RAISING lcx_exception.

  PRIVATE SECTION.

    CLASS-METHODS compare_files
                            IMPORTING it_repo TYPE tt_files
                                      is_gen TYPE st_file
                            RETURNING value(rv_match) TYPE abap_bool
                            RAISING lcx_exception.

    CLASS-METHODS activate RAISING lcx_exception.

ENDCLASS.                    "lcl_serialize DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_serialize IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_serialize IMPLEMENTATION.

  METHOD serialize.

    DATA: lt_files TYPE tt_files.

* todo, refactoring
    CASE is_item-obj_type.
      WHEN 'PROG'.
        rt_files = lcl_serialize_prog=>serialize( is_item ).
      WHEN 'DOMA'.
        rt_files = lcl_serialize_doma=>serialize( is_item ).
      WHEN 'DTEL'.
        rt_files = lcl_serialize_dtel=>serialize( is_item ).
      WHEN 'CLAS'.
        rt_files = lcl_serialize_clas=>serialize( is_item ).
      WHEN 'FUGR'.
        rt_files = lcl_serialize_fugr=>serialize( is_item ).
      WHEN 'TABL'.
        rt_files = lcl_serialize_tabl=>serialize( is_item ).
      WHEN 'TRAN'.
        rt_files = lcl_serialize_tran=>serialize( is_item ).
      WHEN 'MSAG'.
        rt_files = lcl_serialize_msag=>serialize( is_item ).
      WHEN 'TTYP'.
        rt_files = lcl_serialize_ttyp=>serialize( is_item ).
      WHEN 'VIEW'.
        rt_files = lcl_serialize_view=>serialize( is_item ).
      WHEN 'SHLP'.
        rt_files = lcl_serialize_shlp=>serialize( is_item ).
      WHEN 'ENQU'.
        rt_files = lcl_serialize_enqu=>serialize( is_item ).
      WHEN OTHERS.
        _raise 'Serialize, unknown type'.
    ENDCASE.

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
          lv_ext    TYPE string.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF it_files,
                   <ls_gen>  LIKE LINE OF lt_files.


    LOOP AT it_files ASSIGNING <ls_file>.
      SPLIT <ls_file>-filename AT '.' INTO lv_pre lv_type lv_ext.
      TRANSLATE lv_pre TO UPPER CASE.
      TRANSLATE lv_type TO UPPER CASE.

      IF lv_ext <> 'xml'.
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

    SORT rt_results BY obj_type obj_name filename.
    DELETE ADJACENT DUPLICATES FROM rt_results
      COMPARING obj_type obj_name filename.

* todo, how to handle deleted in repo?

  ENDMETHOD.                    "status

  METHOD deserialize.

    DATA: ls_item    TYPE st_item,
          lt_results TYPE tt_results.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results.


    CLEAR lcl_serialize_common=>gt_ddic[].
    CLEAR lcl_serialize_common=>gt_programs[].


    lt_results = status( it_files ).
    DELETE lt_results WHERE match = abap_true.
    SORT lt_results BY obj_type ASCENDING obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_results COMPARING obj_type obj_name.

    LOOP AT lt_results ASSIGNING <ls_result>.

      CLEAR ls_item.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.
* handle namespaces
      REPLACE ALL OCCURRENCES OF '#' IN ls_item-obj_name WITH '/'.

* todo, refactoring
      CASE ls_item-obj_type.
        WHEN 'PROG'.
          lcl_serialize_prog=>deserialize( is_item  = ls_item
                                           it_files = it_files ).
        WHEN 'DOMA'.
          lcl_serialize_doma=>deserialize( is_item  = ls_item
                                           it_files = it_files ).
        WHEN 'DTEL'.
          lcl_serialize_dtel=>deserialize( is_item  = ls_item
                                           it_files = it_files ).
        WHEN 'CLAS'.
          lcl_serialize_clas=>deserialize( is_item  = ls_item
                                           it_files = it_files ).
        WHEN 'FUGR'.
          lcl_serialize_fugr=>deserialize( is_item  = ls_item
                                           it_files = it_files ).
        WHEN 'TABL'.
          lcl_serialize_tabl=>deserialize( is_item  = ls_item
                                           it_files = it_files ).
        WHEN 'TRAN'.
          lcl_serialize_tran=>deserialize( is_item  = ls_item
                                           it_files = it_files ).
        WHEN 'MSAG'.
          lcl_serialize_msag=>deserialize( is_item  = ls_item
                                           it_files = it_files ).
        WHEN 'TTYP'.
          lcl_serialize_ttyp=>deserialize( is_item  = ls_item
                                           it_files = it_files ).
        WHEN 'VIEW'.
          lcl_serialize_view=>deserialize( is_item  = ls_item
                                           it_files = it_files ).
        WHEN 'SHLP'.
          lcl_serialize_shlp=>deserialize( is_item  = ls_item
                                           it_files = it_files ).
        WHEN 'ENQU'.
          lcl_serialize_enqu=>deserialize( is_item  = ls_item
                                           it_files = it_files ).
        WHEN OTHERS.
          _raise 'deserialize, unknown type'.
      ENDCASE.
    ENDLOOP.

    activate( ).

  ENDMETHOD.                    "deserialize

  METHOD activate.

* ddic
    IF NOT lcl_serialize_common=>gt_ddic[] IS INITIAL.
      CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
        EXPORTING
          activate_ddic_objects  = abap_true
          with_popup             = abap_true
        TABLES
          objects                = lcl_serialize_common=>gt_ddic
        EXCEPTIONS
          excecution_error       = 1
          cancelled              = 2
          insert_into_corr_error = 3
          execution_error        = 4
          OTHERS                 = 5.
      IF sy-subrc <> 0.
        _raise 'error from RS_WORKING_OBJECTS_ACTIVATE'.
      ENDIF.
    ENDIF.

* programs
    IF NOT lcl_serialize_common=>gt_programs[] IS INITIAL.
      CALL FUNCTION 'RS_WORKING_OBJECTS_ACTIVATE'
        EXPORTING
          activate_ddic_objects  = abap_false
          with_popup             = abap_true
        TABLES
          objects                = lcl_serialize_common=>gt_programs
        EXCEPTIONS
          excecution_error       = 1
          cancelled              = 2
          insert_into_corr_error = 3
          execution_error        = 4
          OTHERS                 = 5.
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

ENDCLASS.                    "lcl_serialize IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_hash DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_hash DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS adler32 IMPORTING iv_xstring TYPE xstring
                          RETURNING value(rv_checksum) TYPE t_adler32.

    CLASS-METHODS sha1 IMPORTING iv_type TYPE t_type
                                 iv_data TYPE xstring
                       RETURNING value(rv_sha1) TYPE t_sha1
                       RAISING lcx_exception.

    CLASS-METHODS sha1_raw IMPORTING iv_data TYPE xstring
                       RETURNING value(rv_sha1) TYPE t_sha1
                       RAISING lcx_exception.

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

  ENDMETHOD.                    "adler32

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

  ENDMETHOD.                    "sha1_raw

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

  ENDMETHOD.                    "sha1

ENDCLASS.                    "lcl_hash IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_pack DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_pack DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS decode IMPORTING iv_data TYPE xstring
                         RETURNING value(rt_objects) TYPE tt_objects
                         RAISING lcx_exception.

    CLASS-METHODS decode_tree IMPORTING iv_data TYPE xstring
                         RETURNING value(rt_nodes) TYPE tt_nodes
                         RAISING lcx_exception.

    CLASS-METHODS decode_deltas CHANGING ct_objects TYPE tt_objects
                         RAISING lcx_exception.

    CLASS-METHODS decode_commit IMPORTING iv_data TYPE xstring
                         RETURNING value(rs_commit) TYPE st_commit
                         RAISING lcx_exception.

    CLASS-METHODS encode IMPORTING it_objects TYPE tt_objects
                         RETURNING value(rv_data) TYPE xstring
                         RAISING lcx_exception.

    CLASS-METHODS: encode_tree IMPORTING it_nodes TYPE tt_nodes
                         RETURNING value(rv_data) TYPE xstring.

    CLASS-METHODS: encode_commit IMPORTING is_commit TYPE st_commit
                         RETURNING value(rv_data) TYPE xstring.


  PRIVATE SECTION.

    CONSTANTS: c_pack_start TYPE x LENGTH 4 VALUE '5041434B', " PACK
               c_debug_pack TYPE abap_bool VALUE abap_false,
               c_zlib       TYPE x LENGTH 2 VALUE '789C',
               c_zlib_hmm   TYPE x LENGTH 2 VALUE '7801',
               c_version    TYPE x LENGTH 4 VALUE '00000002'.

    CLASS-METHODS type_and_length IMPORTING is_object TYPE st_object
                                  RETURNING value(rv_xstring) TYPE xstring
                                  RAISING lcx_exception.

    CLASS-METHODS delta IMPORTING is_object TYPE st_object
                        CHANGING ct_objects TYPE tt_objects
                        RAISING lcx_exception.

    CLASS-METHODS delta_header CHANGING cv_delta TYPE xstring.

    CLASS-METHODS get_type IMPORTING iv_x TYPE x
                           RETURNING value(rv_type) TYPE t_type
                           RAISING lcx_exception.

    CLASS-METHODS get_length EXPORTING ev_length TYPE i
                             CHANGING cv_data TYPE xstring.

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

    DATA: lv_string  TYPE string,
          lv_null    TYPE x,
          lt_nodes   LIKE it_nodes,
          lv_xstring TYPE xstring.

    FIELD-SYMBOLS: <ls_node> LIKE LINE OF it_nodes.


    lv_null = '00'.

    lt_nodes[] = it_nodes[].
* following has to be done, or unpack will fail on server side
    SORT lt_nodes BY name ASCENDING.

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      CONCATENATE <ls_node>-chmod <ls_node>-name INTO lv_string SEPARATED BY space.
      lv_xstring = lcl_convert=>string_to_xstring_utf8( lv_string ).

      CONCATENATE rv_data lv_xstring lv_null <ls_node>-sha1 INTO rv_data IN BYTE MODE.
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
          lv_char40 TYPE c LENGTH 40,
          lv_mode   TYPE string,
          lv_len    TYPE i,
          lt_string TYPE TABLE OF string.

    FIELD-SYMBOLS: <lv_string> TYPE string.


    lv_string = lcl_convert=>xstring_to_string_utf8( iv_data ).

    SPLIT lv_string AT gc_newline INTO TABLE lt_string.

    lv_mode = 'tree'.                                       "#EC NOTEXT
    LOOP AT lt_string ASSIGNING <lv_string>.
      lv_len = strlen( lv_mode ).

      IF NOT lv_mode IS INITIAL AND <lv_string>(lv_len) = lv_mode.
        CASE lv_mode.
          WHEN 'tree'.
            lv_char40 = <lv_string>+5.
            TRANSLATE lv_char40 TO UPPER CASE.
            rs_commit-tree = lv_char40.
            lv_mode = 'parent'.                             "#EC NOTEXT
          WHEN 'parent'.
            lv_char40 = <lv_string>+7.
            TRANSLATE lv_char40 TO UPPER CASE.
            rs_commit-parent = lv_char40.
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
          ls_object  LIKE LINE OF ct_objects,
          lv_len     TYPE i,
          lv_x       TYPE x.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF ct_objects.


    lv_delta = is_object-data.

* find base
    READ TABLE ct_objects ASSIGNING <ls_object> WITH KEY sha1 = is_object-sha1.
    IF sy-subrc <> 0.
      _raise 'Base not found'.
    ELSE.
      lv_base = <ls_object>-data.
    ENDIF.


    delta_header( CHANGING cv_delta = lv_delta ).


    WHILE xstrlen( lv_delta ) > 0.

      lv_x = lv_delta(1).
      lv_delta = lv_delta+1.
      lv_bitbyte = lcl_convert=>x_to_bitbyte( lv_x ).
*    WRITE: / 'Opcode', lv_x, lv_bitbyte.

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

        CONCATENATE lv_result lv_base+lv_offset(lv_len) INTO lv_result IN BYTE MODE.
      ELSE. " lv_bitbyte(1) = '0'
* insert from delta
        lv_len = lv_x.
        CONCATENATE lv_result lv_delta(lv_len) INTO lv_result IN BYTE MODE.
        lv_delta = lv_delta+lv_len.
      ENDIF.

    ENDWHILE.

    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = <ls_object>-type iv_data = lv_result ).
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

    CONSTANTS: lc_sha_length TYPE i VALUE 20.

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

      IF iv_data+lv_cursor(1) = '00'.
        lv_len = lv_cursor - lv_start.
        lv_xstring = iv_data+lv_start(lv_len).

        lv_string = lcl_convert=>xstring_to_string_utf8( lv_xstring ).
        SPLIT lv_string AT space INTO lv_chmod lv_name.

        lv_offset = lv_cursor + 1.

        CLEAR ls_node.
        ls_node-chmod = lv_chmod.
        IF ls_node-chmod <> gc_chmod_dir AND ls_node-chmod <> gc_chmod_file.
          _raise 'Unknown chmod'.
        ENDIF.

        ls_node-name = lv_name.
        ls_node-sha1 = iv_data+lv_offset(lc_sha_length).
        APPEND ls_node TO rt_nodes.

        lv_start = lv_cursor + 1 + lc_sha_length.
        lv_cursor = lv_start.
      ELSE.
        lv_cursor = lv_cursor + 1.
      ENDIF.
    ENDDO.

  ENDMETHOD.                    "decode_tree

  METHOD decode.

    DATA: lv_x           TYPE x,
          lv_data        TYPE xstring,
          lv_type        TYPE c LENGTH 6,
          lv_zlib        TYPE x LENGTH 2,
          lv_objects     TYPE i,
          lv_len         TYPE i,
          lv_sha1        TYPE t_sha1,
          lv_ref_delta   TYPE t_sha1,
          lv_adler32     TYPE t_adler32,
          lv_compressed     TYPE xstring,
          lv_compressed_len TYPE i,
          lv_decompress_len TYPE i,
          lv_decompressed   TYPE xstring,
          lv_xstring     TYPE xstring,
          lv_expected    TYPE i,
          ls_object      LIKE LINE OF rt_objects.


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
    IF lv_sha1 <> lv_data.
      _raise 'SHA1 at end of pack doesnt match'.
    ENDIF.

  ENDMETHOD.                    "decode

  METHOD encode.

    DATA: lv_sha1       TYPE t_sha1,
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
      CONCATENATE rv_data lv_adler32  INTO rv_data IN BYTE MODE.

    ENDLOOP.

    lv_sha1 = lcl_hash=>sha1_raw( rv_data ).
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
* class-methods delete

    CLASS-METHODS list RETURNING value(rt_repos) TYPE tt_repos_sha1
                       RAISING lcx_exception.

    CLASS-METHODS update IMPORTING is_repo TYPE st_repo
                                   iv_branch TYPE t_sha1
                         RAISING lcx_exception.

    CLASS-METHODS add IMPORTING is_repo TYPE st_repo
                                   iv_branch TYPE t_sha1
                         RAISING lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS read_text RETURNING value(rt_repos) TYPE tt_repos_sha1
                            RAISING lcx_exception.

    CLASS-METHODS save_text IMPORTING it_repos TYPE tt_repos_sha1
                            RAISING lcx_exception.

    CLASS-METHODS header RETURNING value(rs_header) TYPE thead.

ENDCLASS.                    "lcl_persistence DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_persistence IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_persistence IMPLEMENTATION.

  METHOD header.
    rs_header-tdid     = 'ST'.
    rs_header-tdspras  = 'E'.
    rs_header-tdname   = 'ZABAPGIT'.
    rs_header-tdobject = 'TEXT'.
  ENDMETHOD.                    "header

  METHOD save_text.

    DATA: lt_lines  TYPE TABLE OF tline,
          ls_header TYPE thead.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF it_repos,
                   <ls_line> LIKE LINE OF lt_lines.


    LOOP AT it_repos ASSIGNING <ls_repo>.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-url.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-branch_name.
      APPEND INITIAL LINE TO lt_lines ASSIGNING <ls_line>.
      <ls_line>-tdformat = '*'.
      <ls_line>-tdline = <ls_repo>-sha1.
    ENDLOOP.

    ls_header = header( ).

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

  ENDMETHOD.                    "save_text

  METHOD add.

    DATA: lt_repos TYPE tt_repos_sha1.

    FIELD-SYMBOLS: <ls_repo> LIKE LINE OF lt_repos.


    IF iv_branch IS INITIAL.
      _raise 'update, sha empty'.
    ENDIF.

    lt_repos = list( ).

    READ TABLE lt_repos WITH KEY url = is_repo-url branch_name = is_repo-branch_name
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      _raise 'already inserted'.
    ENDIF.

    APPEND INITIAL LINE TO lt_repos ASSIGNING <ls_repo>.
    <ls_repo>-url = is_repo-url.
    <ls_repo>-branch_name = is_repo-branch_name.
    <ls_repo>-sha1 = iv_branch.

    save_text( lt_repos ).

  ENDMETHOD.                    "insert

  METHOD update.

    DATA: lt_repos TYPE tt_repos_sha1.

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

    save_text( lt_repos ).

  ENDMETHOD.                    "update

  METHOD list.
    rt_repos = read_text( ).
  ENDMETHOD.                    "list

  METHOD read_text.

    DATA: lt_lines  TYPE TABLE OF tline,
          ls_header TYPE thead,
          ls_repo   TYPE st_repo_sha1.

    FIELD-SYMBOLS: <ls_line> LIKE LINE OF lt_lines.


    ls_header = header( ).

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = ls_header-tdid
        language                = ls_header-tdspras
        name                    = ls_header-tdname
        object                  = ls_header-tdobject
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
    IF sy-subrc = 4.
      RETURN.
    ELSEIF sy-subrc <> 0.
      _raise 'Error from READ_TEXT'.
    ENDIF.

    IF lines( lt_lines ) MOD 3 <> 0.
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

      ls_repo-sha1 = <ls_line>-tdline.
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
    CLASS-METHODS upload_pack IMPORTING is_repo TYPE st_repo
                              EXPORTING ev_pack TYPE xstring
                                        ev_branch TYPE t_sha1
                              RAISING lcx_exception.

* local to remote
    CLASS-METHODS receive_pack IMPORTING is_repo TYPE st_repo
                                         iv_commit TYPE t_sha1
                                         iv_pack TYPE xstring
                               RAISING lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS pkt_string
                      IMPORTING iv_string TYPE string
                      RETURNING value(rv_pkt) TYPE string
                      RAISING lcx_exception.

    CLASS-METHODS parse
                      EXPORTING ev_pack TYPE xstring
                      CHANGING cv_data TYPE xstring.

    CLASS-METHODS length_utf8_hex
                      IMPORTING iv_data TYPE xstring
                      RETURNING value(rv_len) TYPE i.

    CLASS-METHODS ref_discovery
                      IMPORTING is_repo TYPE st_repo
                                iv_service TYPE string
                      EXPORTING ei_client TYPE REF TO if_http_client
                                ev_branch TYPE t_sha1
                      RAISING lcx_exception.

    CLASS-METHODS set_headers
                      IMPORTING is_repo TYPE st_repo
                                iv_service TYPE string
                                ii_client TYPE REF TO if_http_client
                      RAISING lcx_exception.

    CLASS-METHODS check_http_200
                      IMPORTING ii_client TYPE REF TO if_http_client
                      RAISING lcx_exception.

    CLASS-METHODS get_null RETURNING value(rv_c) TYPE char1.

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

    lv_value = 'Content-Type: application/x-git-' && iv_service && '-pack-request'.
    ii_client->request->set_header_field(
        name  = 'Content-Type'
        value = lv_value ).

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
    IF lv_code <> 200.
      _raise 'HTTP error code'.
    ENDIF.

  ENDMETHOD.                    "http_200

  METHOD ref_discovery.

    DATA: lv_hash   TYPE c LENGTH 40,
          lv_len    TYPE i,
          lt_result TYPE TABLE OF string,
          lv_data   TYPE string,
          lv_text   TYPE string.

    STATICS: sv_authorization TYPE string.


    cl_http_client=>create_by_url(
      EXPORTING
        url    = lcl_url=>host( is_repo-url )
      IMPORTING
        client = ei_client ).

    ei_client->request->set_cdata( '' ).
    ei_client->request->set_header_field(
        name  = '~request_method'
        value = 'GET' ).
    ei_client->request->set_header_field(
        name  = '~request_uri'
        value = lcl_url=>path_name( is_repo-url ) && '.git/info/refs?service=git-' && iv_service && '-pack' ).
    IF NOT sv_authorization IS INITIAL.
* note this will only work if all repositories uses the same login
      ei_client->request->set_header_field(
          name  = 'authorization'
          value = sv_authorization ).
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
    sv_authorization = ei_client->request->get_header_field( 'authorization' ).

    lv_data = ei_client->response->get_cdata( ).

    IF is_repo-branch_name IS INITIAL.
      _raise 'branch empty'.
    ENDIF.

    lv_len = strlen( is_repo-branch_name ).
    SPLIT lv_data AT gc_newline INTO TABLE lt_result.
    LOOP AT lt_result INTO lv_data.
      IF sy-tabix = 1.
        CONTINUE. " current loop
      ELSEIF sy-tabix = 2 AND strlen( lv_data ) > 49
          AND lv_data+49(lv_len) = is_repo-branch_name.
        lv_hash = lv_data+8.
        EXIT. " current loop
      ELSEIF sy-tabix > 2 AND strlen( lv_data ) > 45
          AND lv_data+45 = is_repo-branch_name.
        lv_hash = lv_data+4.
        EXIT. " current loop
      ELSEIF sy-tabix = 2 AND strlen( lv_data ) = 8 AND lv_data(8) = '00000000'.
        _raise 'No branches, create branch manually by adding file'.
      ENDIF.
    ENDLOOP.

    TRANSLATE lv_hash TO UPPER CASE.
    IF strlen( lv_hash ) <> 40.
      _raise 'Branch not found'.
    ENDIF.

    ev_branch = lv_hash.

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


    ref_discovery(
      EXPORTING
        is_repo    = is_repo
        iv_service = lc_service
      IMPORTING
        ei_client  = li_client
        ev_branch  = lv_branch ).

    set_headers(
      EXPORTING
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
              'report-status agent=abapGit/' && gc_version &&
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

    DATA: lv_len      TYPE i,
          lv_contents TYPE xstring,
          lv_pack     TYPE xstring.


    WHILE xstrlen( cv_data ) >= 4.
      lv_len = length_utf8_hex( cv_data ).
      IF lv_len = 0.
        EXIT. " current loop
      ENDIF.

      lv_contents = cv_data(lv_len).
      lv_contents = lv_contents+4.
      IF xstrlen( lv_contents ) > 1 AND lv_contents(1) = '01'. " band 1
        CONCATENATE lv_pack lv_contents+1 INTO lv_pack IN BYTE MODE.
      ENDIF.

      cv_data = cv_data+lv_len.
    ENDWHILE.

    ev_pack = lv_pack.

  ENDMETHOD.                    "parse

  METHOD upload_pack.

    CONSTANTS: lc_service TYPE string VALUE 'upload'.       "#EC NOTEXT

    DATA: li_client      TYPE REF TO if_http_client,
          lv_buffer      TYPE string,
          lv_xstring     TYPE xstring,
          lv_line        TYPE string,
          lv_pkt         TYPE string.


    ref_discovery(
      EXPORTING
        is_repo    = is_repo
        iv_service = lc_service
      IMPORTING
        ei_client  = li_client
        ev_branch  = ev_branch ).

    set_headers(
      EXPORTING
        is_repo    = is_repo
        iv_service = lc_service
        ii_client  = li_client ).

    lv_line = 'want' &&
              ` ` &&
              ev_branch &&
              ` ` &&
              'side-band-64k no-progress agent=abapGit/' && gc_version
              && gc_newline.                                "#EC NOTEXT
    lv_pkt = pkt_string( lv_line ).

    lv_buffer = lv_pkt
             && '0000'
             && '0009done' && gc_newline.

    li_client->request->set_cdata( lv_buffer ).
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
*       CLASS lcl_porcelain DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_porcelain DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS pull IMPORTING is_repo TYPE st_repo
                       EXPORTING et_files TYPE tt_files
                                 et_objects TYPE tt_objects
                                 ev_branch TYPE t_sha1
                       RAISING lcx_exception.

    CLASS-METHODS push IMPORTING is_repo TYPE st_repo
                                 is_comment TYPE st_comment
                                 it_files TYPE tt_files
                       RETURNING value(rv_branch) TYPE t_sha1
                       RAISING lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS walk IMPORTING it_objects TYPE tt_objects
                                 iv_sha1 TYPE t_sha1
                                 iv_path TYPE string
                       CHANGING ct_files TYPE tt_files
                       RAISING lcx_exception.

    CLASS-METHODS root_tree IMPORTING it_objects TYPE tt_objects
                                      iv_branch TYPE t_sha1
                            RETURNING value(rt_nodes) TYPE tt_nodes
                            RAISING lcx_exception.

    CLASS-METHODS receive_pack IMPORTING is_comment TYPE st_comment
                                         is_repo TYPE st_repo
                                         it_nodes TYPE tt_nodes
                                         it_files TYPE tt_files
                                         iv_branch TYPE t_sha1
                               RETURNING value(rv_branch) TYPE t_sha1
                               RAISING lcx_exception.

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
        <ls_node>-chmod = gc_chmod_file.
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

    DATA: ls_object  LIKE LINE OF et_objects,
          ls_commit  TYPE st_commit,
          lv_pack    TYPE xstring.


    lcl_transport=>upload_pack( EXPORTING is_repo = is_repo
                                IMPORTING ev_pack = lv_pack
                                          ev_branch = ev_branch ).

    IF lv_pack IS INITIAL.
      _raise 'empty pack'.
    ENDIF.

    et_objects = lcl_pack=>decode( lv_pack ).
    lcl_pack=>decode_deltas( CHANGING ct_objects = et_objects ).

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

    DATA: lv_path   TYPE string,
          ls_file   LIKE LINE OF ct_files,
          lt_nodes  TYPE tt_nodes.

    FIELD-SYMBOLS: <ls_tree> LIKE LINE OF it_objects,
                   <ls_blob> LIKE LINE OF it_objects,
                   <ls_node> LIKE LINE OF lt_nodes.


    READ TABLE it_objects ASSIGNING <ls_tree> WITH KEY sha1 = iv_sha1 type = gc_tree.
    IF sy-subrc <> 0.
      _raise 'Walk, tree not found'.
    ENDIF.

    lt_nodes = lcl_pack=>decode_tree( <ls_tree>-data ).

    LOOP AT lt_nodes ASSIGNING <ls_node>.
      IF <ls_node>-chmod = gc_chmod_file.
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

    LOOP AT lt_nodes ASSIGNING <ls_node> WHERE chmod = gc_chmod_dir.
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
    CLASS-METHODS: run RAISING lcx_exception.

    CLASS-METHODS: on_event
                      FOR EVENT sapevent OF cl_gui_html_viewer
                      IMPORTING action frame getdata postdata query_table. "#EC NEEDED

  PRIVATE SECTION.
    CLASS-DATA go_html_viewer TYPE REF TO cl_gui_html_viewer.

    CLASS-METHODS: view
                      IMPORTING iv_html TYPE string.

    CLASS-METHODS: render
                      RETURNING value(rv_html) TYPE string
                      RAISING lcx_exception.

    CLASS-METHODS: render_css
                      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS: render_repo
                      IMPORTING is_repo TYPE st_repo_sha1
                      RETURNING value(rv_html) TYPE string
                      RAISING lcx_exception.

    CLASS-METHODS: render_header
                      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS: render_footer
                      RETURNING value(rv_html) TYPE string.

    CLASS-METHODS: install
                      IMPORTING iv_url TYPE string
                      RAISING lcx_exception.

    CLASS-METHODS: add
                      IMPORTING is_repo TYPE st_repo
                      RAISING lcx_exception.

    CLASS-METHODS: pull
                      IMPORTING is_repo TYPE st_repo
                      RAISING lcx_exception.

    CLASS-METHODS: commit
                      IMPORTING is_repo TYPE st_repo
                      RAISING lcx_exception.

    CLASS-METHODS: diff
                      IMPORTING is_result TYPE st_result
                                is_repo TYPE st_repo
                      RAISING lcx_exception.

    CLASS-METHODS render_diff
                      IMPORTING is_result TYPE st_result
                                it_diffs TYPE tt_diffs.

    CLASS-METHODS: struct_encode
                      IMPORTING ig_structure1 TYPE any
                                ig_structure2 TYPE any OPTIONAL
                      RETURNING value(rv_string) TYPE string.

    CLASS-METHODS: struct_decode
                      IMPORTING iv_string TYPE clike
                      CHANGING cg_structure TYPE any
                      RAISING lcx_exception.

    CLASS-METHODS: popup_comment
                      RETURNING value(rs_comment) TYPE st_comment
                      RAISING lcx_exception.

ENDCLASS.                    "lcl_gui DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui IMPLEMENTATION.

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
          '<body>'.

  ENDMETHOD.                    "render_head

  METHOD diff.

    DATA: lt_remote  TYPE tt_files,
          lt_local   TYPE tt_files,
          ls_item    TYPE st_item,
          lt_diffs   TYPE tt_diffs.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF lt_remote,
                   <ls_local> LIKE LINE OF lt_local.


    lcl_porcelain=>pull( EXPORTING is_repo  = is_repo
                         IMPORTING et_files = lt_remote ).

    CLEAR ls_item.
    ls_item-obj_type = is_result-obj_type.
    ls_item-obj_name = is_result-obj_name.

    lt_local = lcl_serialize=>serialize( ls_item ).

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

    lt_diffs = lcl_diff=>diff( iv_local = <ls_local>-data
                               iv_remote = <ls_remote>-data ).

    render_diff( is_result = is_result
                 it_diffs  = lt_diffs ).

  ENDMETHOD.                    "diff

  METHOD render_diff.

    DATA: lv_html   TYPE string,
          lv_local  TYPE string,
          lv_remote TYPE string.


    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF it_diffs.


    lv_html = render_header( ) &&
              '<h1>diff</h1>&nbsp;<a href="javascript:goBack()">Back</a>' &&
              '<hr><h3>' &&
              is_result-obj_type && '&nbsp;' &&
              is_result-obj_name && '&nbsp;' &&
              is_result-filename && '</h3><br><br>'.

    lv_html = lv_html && '<table border="0">' && gc_newline &&
              '<tr><td><h2>Local</h2></td><td></td><td><h2>Remote</h2></td></tr>'.

    LOOP AT it_diffs ASSIGNING <ls_diff>.
      lv_local = escape( val = <ls_diff>-local format = cl_abap_format=>e_html_attr ).
      lv_remote = escape( val = <ls_diff>-remote format = cl_abap_format=>e_html_attr ).

      lv_html = lv_html &&
        '<tr>' && gc_newline &&
        '<td><pre>' && lv_local && '</pre></td>' && gc_newline &&
        '<td>&nbsp;' && <ls_diff>-result && '&nbsp;</td>' && gc_newline &&
        '<td><pre>' && lv_remote && '</pre></td>' && gc_newline &&
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

    DATA: lt_files   TYPE tt_files,
          lv_branch  TYPE t_sha1.


    lcl_porcelain=>pull( EXPORTING is_repo   = is_repo
                         IMPORTING et_files  = lt_files
                                   ev_branch = lv_branch ).

    lcl_serialize=>deserialize( lt_files ).

    lcl_persistence=>update( is_repo   = is_repo
                             iv_branch = lv_branch ).

    view( render( ) ).

  ENDMETHOD.                    "pull

  METHOD commit.

    DATA: lv_branch      TYPE t_sha1,
          lt_results     TYPE tt_results,
          lt_push        TYPE tt_files,
          ls_item        TYPE st_item,
          ls_comment     TYPE st_comment,
          lt_files       TYPE tt_files.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results.


    lcl_porcelain=>pull( EXPORTING is_repo   = is_repo
                         IMPORTING et_files  = lt_files ).

    lt_results = lcl_serialize=>status( lt_files ).

    CLEAR lt_files[].
    LOOP AT lt_results ASSIGNING <ls_result> WHERE match = abap_false.
      CLEAR ls_item.
      ls_item-obj_type = <ls_result>-obj_type.
      ls_item-obj_name = <ls_result>-obj_name.

      lt_files = lcl_serialize=>serialize( ls_item ).
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

    FIELD-SYMBOLS: <ls_comp> TYPE abap_compdescr,
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

    DATA: lx_exception TYPE REF TO lcx_exception,
          ls_result    TYPE st_result,
          lv_url       TYPE string,
          ls_repo      TYPE st_repo.


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
                           CHANGING cg_structure = ls_repo ).
            add( ls_repo ).
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
          WHEN 'pull'.
            struct_decode( EXPORTING iv_string = getdata
                           CHANGING cg_structure = ls_repo ).
            pull( ls_repo ).
          WHEN OTHERS.
            _raise 'Unknown action'.
        ENDCASE.
      CATCH lcx_exception INTO lx_exception.
        MESSAGE lx_exception->mv_text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "on_event

  METHOD add.

    DATA: lt_files    TYPE tt_files,
          ls_item     TYPE st_item,
          ls_comment  TYPE st_comment,
          lv_euobj_id TYPE euobj-id,
          lv_branch   TYPE t_sha1,
          lt_spopli   TYPE TABLE OF spopli,
          lv_answer   TYPE c.

    FIELD-SYMBOLS: <ls_spopli> LIKE LINE OF lt_spopli.

    DEFINE _add.
      append initial line to lt_spopli assigning <ls_spopli>.
      <ls_spopli>-varoption = &1.                           "#EC NOTEXT
    END-OF-DEFINITION.


* todo, by package, fm TADIR_GET
* todo, by transport

    _add 'PROG Program'.
    _add 'DTEL Data Element'.
    _add 'DOMA Domain'.
    _add 'CLAS Class'.
    _add 'TABL Table/Structure'.
    _add 'TTYP Table Type'.
    _add 'VIEW View'.
    _add 'SHLP Search Help'.
    _add 'ENQU Lock Object'.
    _add 'FUGR Function Group (todo)'.
    _add 'MSAG Message Class (todo)'.
    _add 'TRAN Transaction (todo)'.
    _add 'SSFO Smart Form (todo)'.
    _add 'FORM SAP Script (todo)'.
*table contents
*lock object
*web dynpro

    CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
      EXPORTING
        start_col          = 10
        start_row          = 5
        textline1          = 'Choose object type'           "#EC NOTEXT
        titel              = 'Choose object type'           "#EC NOTEXT
      IMPORTING
        answer             = lv_answer
      TABLES
        t_spopli           = lt_spopli
      EXCEPTIONS
        not_enough_answers = 1
        too_much_answers   = 2
        too_much_marks     = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      _raise 'error from decide_list'.
    ENDIF.
    IF lv_answer = 'A'. " Cancelled
      RETURN.
    ENDIF.

    READ TABLE lt_spopli ASSIGNING <ls_spopli> WITH KEY selflag = abap_true.
    ASSERT sy-subrc = 0.
    ls_item-obj_type = <ls_spopli>-varoption.

    lv_euobj_id = ls_item-obj_type.
    CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
      EXPORTING
        object_type          = lv_euobj_id
        suppress_selection   = abap_true
      IMPORTING
        object_name_selected = ls_item-obj_name
      EXCEPTIONS
        cancel               = 01.
    IF sy-subrc = 1.
      RETURN.
    ENDIF.

    lt_files = lcl_serialize=>serialize( ls_item ).

    ls_comment = popup_comment( ).
    IF ls_comment IS INITIAL.
      RETURN.
    ENDIF.

    lv_branch = lcl_porcelain=>push( is_comment = ls_comment
                                     is_repo    = is_repo
                                     it_files   = lt_files ).

    lcl_persistence=>update( is_repo   = is_repo
                             iv_branch = lv_branch ).

    view( render( ) ).

  ENDMETHOD.                    "add

  METHOD install.

    DATA: lv_returncode TYPE c,
          lt_files      TYPE tt_files,
          ls_repo       TYPE st_repo,
          lv_branch     TYPE t_sha1,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'ABAPTXT255'.
    <ls_field>-fieldname = 'LINE'.
    <ls_field>-fieldtext = 'Url'.                           "#EC NOTEXT
    <ls_field>-value     = iv_url.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Clone'                           "#EC NOTEXT
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
* todo, select branch or tag
    ls_repo-branch_name = 'refs/heads/master'.              "#EC NOTEXT
    lcl_url=>name( ls_repo-url ).         " validate


    lcl_porcelain=>pull( EXPORTING is_repo   = ls_repo
                         IMPORTING et_files  = lt_files
                                   ev_branch = lv_branch ).

    lcl_serialize=>deserialize( lt_files ).

    lcl_persistence=>add( is_repo   = ls_repo
                          iv_branch = lv_branch ).

    view( render( ) ).

  ENDMETHOD.                    "install

  METHOD render_css.

    rv_html = '<style type="text/css">' && gc_newline &&
          'body {'                      && gc_newline &&    "#EC NOTEXT
          '  font-family: verdana;'     && gc_newline &&    "#EC NOTEXT
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
          '</style>'                    && gc_newline.

  ENDMETHOD.                    "render_css

  METHOD render.

    DATA: lt_repos TYPE tt_repos_sha1,
          lv_text  TYPE c LENGTH 100,
          lv_pct   TYPE i,
          lv_f     TYPE f,
          ls_repo  LIKE LINE OF lt_repos.


    lt_repos = lcl_persistence=>list( ).

    rv_html = render_header( ) &&
      '<h1>abapGit</h1>&nbsp;'                                  && gc_newline &&
      '<a href="sapevent:refresh">Refresh</a>&nbsp;'            && gc_newline &&
      '<a href="sapevent:install">Clone</a>&nbsp;'              && gc_newline &&
      '<a href="sapevent:explore">Explore</a>&nbsp;'            && gc_newline &&
      '<a href="sapevent:abapgithome">abapGit@GitHub</a>&nbsp;' && gc_newline &&
      '<hr>'                                                    && gc_newline.

    LOOP AT lt_repos INTO ls_repo.
      rv_html = rv_html &&
        '<a href="#' && lcl_url=>name( ls_repo-url ) &&'" class="grey">' &&
        lcl_url=>name( ls_repo-url ) &&
        '</a>&nbsp;'.
    ENDLOOP.

    IF lt_repos[] IS INITIAL.
      rv_html = rv_html && '<br><a href="sapevent:explore">Explore</a> new projects'.
    ELSE.
      rv_html = rv_html && '<br><br><br>'.

      LOOP AT lt_repos INTO ls_repo.
        lv_f = ( sy-tabix / lines( lt_repos ) ) * 100.
        lv_pct = lv_f.
        lv_text = lcl_url=>name( ls_repo-url ).
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = lv_pct
            text       = lv_text.

        rv_html = rv_html && render_repo( ls_repo ).
      ENDLOOP.
    ENDIF.

    rv_html = rv_html && render_footer( ).

  ENDMETHOD.                    "render

  METHOD render_footer.

    rv_html = rv_html &&
              '<br><br><hr><center><h3>abapGit Version:&nbsp;' &&
              gc_version &&
              '</h3></center>'.

    rv_html = rv_html && '</body></html>'.

  ENDMETHOD.                    "render_footer

  METHOD render_repo.

    DATA: lt_files   TYPE tt_files,
          ls_repo    TYPE st_repo,
          lv_branch  TYPE t_sha1,
          lv_link    TYPE string,
          lv_status  TYPE string,
          lt_results TYPE tt_results.

    FIELD-SYMBOLS: <ls_file>   LIKE LINE OF lt_files,
                   <ls_result> LIKE LINE OF lt_results.


    rv_html = rv_html &&
      '<a id="' && lcl_url=>name( is_repo-url ) && '"></a>' &&
      '<h2>' && lcl_url=>name( is_repo-url ) && '</h2>&nbsp;' &&
      '<h3>' && is_repo-url && '</h3>&nbsp;' &&
      '<h3>' && is_repo-branch_name && '</h3>&nbsp;' &&
      '<br>'.

    MOVE-CORRESPONDING is_repo TO ls_repo.
    lcl_porcelain=>pull( EXPORTING is_repo   = ls_repo
                         IMPORTING et_files  = lt_files
                                   ev_branch = lv_branch ).

    rv_html = rv_html && '<br><u>Remote files</u><table border="1">' && gc_newline.
    LOOP AT lt_files ASSIGNING <ls_file>.
      rv_html = rv_html &&
        '<tr>' && gc_newline &&
        '<td>' && <ls_file>-path && '</td>' && gc_newline &&
        '<td>' && <ls_file>-filename && '</td>' && gc_newline &&
        '</tr>' && gc_newline.
    ENDLOOP.
    rv_html = rv_html && '</table>' && gc_newline.

    rv_html = rv_html && '<br>'.

    lt_results = lcl_serialize=>status( lt_files ).
    IF lv_branch <> is_repo-sha1.
      lv_status = 'pull'.                                   "#EC NOTEXT
    ELSE.
      READ TABLE lt_results WITH KEY match = abap_false TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lv_status = 'commit'.                               "#EC NOTEXT
      ELSE.
        lv_status = 'match'.                                "#EC NOTEXT
      ENDIF.
    ENDIF.

    rv_html = rv_html && '<u>Objects</u><table border="1">' && gc_newline.
    LOOP AT lt_results ASSIGNING <ls_result>.
      IF <ls_result>-match = abap_false.
        lv_link = '<a href="sapevent:diff?' &&
          struct_encode( ig_structure1 = <ls_result> ig_structure2 = is_repo ) &&
          '">diff</a>'.
      ELSE.
        CLEAR lv_link.
      ENDIF.

      rv_html = rv_html &&
        '<tr>'                                    && gc_newline &&
        '<td>' && <ls_result>-obj_type && '</td>' && gc_newline &&
        '<td>' && <ls_result>-obj_name && '</td>' && gc_newline &&
        '<td>' && <ls_result>-match && '</td>'    && gc_newline &&
        '<td>' && <ls_result>-filename && '</td>' && gc_newline &&
        '<td>' && lv_link && '</td>'              && gc_newline &&
        '</tr>'                                   && gc_newline.
    ENDLOOP.
    rv_html = rv_html && '</table>' && gc_newline.

    CASE lv_status.
      WHEN 'match'.
        rv_html = rv_html && '<a href="sapevent:add?'
                  && struct_encode( ls_repo ) && '">add</a>'.
      WHEN 'commit'.
        rv_html = rv_html && '<a href="sapevent:commit?'
                  && struct_encode( ls_repo ) && '">commit</a>'.
      WHEN 'pull'.
        rv_html = rv_html && '<a href="sapevent:pull?'
                  && struct_encode( ls_repo ) && '">pull</a>'.
      WHEN OTHERS.
        _raise 'status unknown'.
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

ENDCLASS.                    "lcl_gui IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  run
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM run.

  DATA: lx_exception TYPE REF TO lcx_exception.


  TRY.
      lcl_gui=>run( ).
    CATCH lcx_exception INTO lx_exception.
      MESSAGE lx_exception->mv_text TYPE 'E'.
  ENDTRY.

  WRITE: / '.'.     " required

ENDFORM.                    "run

*----------------------------------------------------------------------*
*       CLASS test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abap_unit DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.

    METHODS encode_decode_tree FOR TESTING RAISING lcx_exception.
    METHODS encode_decode_commit FOR TESTING RAISING lcx_exception.
    METHODS encode_decode_pack_short FOR TESTING RAISING lcx_exception.
    METHODS encode_decode_pack_long FOR TESTING RAISING lcx_exception.
    METHODS encode_decode_pack_multiple FOR TESTING RAISING lcx_exception.

    METHODS convert_int FOR TESTING RAISING lcx_exception.

    METHODS repo_url FOR TESTING RAISING lcx_exception.
    METHODS repo_error FOR TESTING.

    METHODS serialize_tabl FOR TESTING RAISING lcx_exception.
    METHODS serialize_enqu FOR TESTING RAISING lcx_exception.
    METHODS serialize_shlp FOR TESTING RAISING lcx_exception.
    METHODS serialize_view FOR TESTING RAISING lcx_exception.

ENDCLASS.                    "test DEFINITION
*----------------------------------------------------------------------*
*       CLASS test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abap_unit IMPLEMENTATION.

  METHOD serialize_enqu.

    DATA: ls_item  TYPE st_item,
          lt_files TYPE tt_files.


    ls_item-obj_type = 'ENQU'.
    ls_item-obj_name = 'E_USR04'.

    lt_files = lcl_serialize=>serialize( ls_item ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_shlp.

    DATA: ls_item  TYPE st_item,
          lt_files TYPE tt_files.


    ls_item-obj_type = 'SHLP'.
    ls_item-obj_name = 'USER_LOGON'.

    lt_files = lcl_serialize=>serialize( ls_item ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_view.

    DATA: ls_item  TYPE st_item,
          lt_files TYPE tt_files.


    ls_item-obj_type = 'VIEW'.
    ls_item-obj_name = 'VUSR02_HEADER'.

    lt_files = lcl_serialize=>serialize( ls_item ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "lcl_abap_unit

  METHOD serialize_tabl.

    DATA: ls_item  TYPE st_item,
          lt_files TYPE tt_files.


    ls_item-obj_type = 'TABL'.
    ls_item-obj_name = 'USR02'.

    lt_files = lcl_serialize=>serialize( ls_item ).

    cl_abap_unit_assert=>assert_not_initial( lt_files ).

  ENDMETHOD.                    "serialize_table

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

    DATA: lt_objects TYPE tt_objects,
          ls_object  LIKE LINE OF lt_objects,
          lt_nodes   TYPE tt_nodes,
          ls_node    LIKE LINE OF lt_nodes,
          ls_commit  TYPE st_commit,
          lt_result  TYPE tt_objects,
          lv_data    TYPE xstring.


* blob
    lv_data = '123456789ABCDEF545794254754554'.
    CLEAR ls_object.
    ls_object-sha1 = lcl_hash=>sha1( iv_type = gc_blob iv_data = lv_data ).
    ls_object-type = gc_blob.
    ls_object-data = lv_data.
    APPEND ls_object TO lt_objects.

* commit
    CLEAR ls_commit.
    ls_commit-tree      = '5F46CB3C4B7F0B3600B64F744CDE614A283A88DC'.
    ls_commit-parent    = '5F46CB3C4B7F0B3600B64F744CDE614A283A88DC'.
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
    ls_node-sha1      = '5F46CB3C4B7F0B3600B64F744CDE614A283A88DC'.
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

    DATA: lt_objects TYPE tt_objects,
          ls_object  LIKE LINE OF lt_objects,
          lt_result  TYPE tt_objects,
          lv_data    TYPE xstring.


    lv_data = '0123456789ABCDEF'.

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

    DATA: lt_objects TYPE tt_objects,
          ls_object  LIKE LINE OF lt_objects,
          lv_xstring TYPE xstring,
          lt_result  TYPE tt_objects,
          lv_data    TYPE xstring.


    lv_xstring = '0123456789ABCDEF'.
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

    DATA: lt_nodes  TYPE tt_nodes,
          ls_node   LIKE LINE OF lt_nodes,
          lv_data   TYPE xstring,
          lt_result TYPE tt_nodes.

    CLEAR ls_node.
    ls_node-chmod = gc_chmod_file.
    ls_node-name = 'foobar.txt'.
    ls_node-sha1 = '5F46CB3C4B7F0B3600B64F744CDE614A283A88DC'.
    APPEND ls_node TO lt_nodes.

    lv_data = lcl_pack=>encode_tree( lt_nodes ).
    lt_result = lcl_pack=>decode_tree( lv_data ).

    cl_abap_unit_assert=>assert_equals(
        exp = lt_nodes
        act = lt_result ).

  ENDMETHOD.                    "tree_encode_decode

  METHOD encode_decode_commit.

    DATA: ls_commit TYPE st_commit,
          ls_result TYPE st_commit,
          lv_data   TYPE xstring.


    ls_commit-tree      = '44CDE614A283A88DC5F46CB3C4B7F0B3600B64F7'.
    ls_commit-parent    = '83A88DC5F46CB3C4B7F0B3600B64F744CDE614A2'.
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
