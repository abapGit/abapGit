*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_XML
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_xml DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml DEFINITION ABSTRACT.

  PUBLIC SECTION.
    METHODS:
      constructor.

  PROTECTED SECTION.
    DATA: mi_ixml     TYPE REF TO if_ixml,
          mi_xml_doc  TYPE REF TO if_ixml_document,
          ms_metadata TYPE ty_metadata.

    CONSTANTS: c_abapgit_tag             TYPE string VALUE 'abapGit' ##NO_TEXT,
               c_attr_version            TYPE string VALUE 'version' ##NO_TEXT,
               c_attr_serializer         TYPE string VALUE 'serializer' ##NO_TEXT,
               c_attr_serializer_version TYPE string VALUE 'serializer_version' ##NO_TEXT.

    METHODS to_xml
      IMPORTING iv_normalize  TYPE sap_bool DEFAULT abap_true
      RETURNING VALUE(rv_xml) TYPE string.

    METHODS parse
      IMPORTING iv_normalize TYPE abap_bool DEFAULT abap_true
                iv_xml       TYPE string
      RAISING   lcx_exception.

  PRIVATE SECTION.
    METHODS error
      IMPORTING ii_parser TYPE REF TO if_ixml_parser
      RAISING   lcx_exception.

    METHODS display_xml_error
      RAISING lcx_exception.

ENDCLASS.                    "lcl_xml DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_xml IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml IMPLEMENTATION.

  METHOD constructor.
    mi_ixml = cl_ixml=>create( ).
    mi_xml_doc = mi_ixml->create_document( ).
  ENDMETHOD.                    "constructor

  METHOD parse.

    DATA: li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_element        TYPE REF TO if_ixml_element,
          li_version        TYPE REF TO if_ixml_node,
          li_parser         TYPE REF TO if_ixml_parser.


    ASSERT NOT iv_xml IS INITIAL.

    li_stream_factory = mi_ixml->create_stream_factory( ).
    li_istream = li_stream_factory->create_istream_string( iv_xml ).
    li_parser = mi_ixml->create_parser( stream_factory = li_stream_factory
                                        istream        = li_istream
                                        document       = mi_xml_doc ).
    li_parser->set_normalizing( iv_normalize ).
    IF li_parser->parse( ) <> 0.
      error( li_parser ).
    ENDIF.

    li_istream->close( ).


    li_element = mi_xml_doc->find_from_name_ns( depth = 0 name = c_abapgit_tag ).
    li_version = li_element->if_ixml_node~get_attributes(
      )->get_named_item_ns( c_attr_version ) ##no_text.
    IF li_version->get_value( ) <> gc_xml_version.
      display_xml_error( ).
    ENDIF.

* buffer serializer metadata. Git node will be removed lateron
    ms_metadata-class   = li_element->get_attribute_ns( c_attr_serializer ).
    ms_metadata-version = li_element->get_attribute_ns( c_attr_serializer_version ).

  ENDMETHOD.                    "parse

  METHOD display_xml_error.

    DATA: lv_version TYPE string.


    lv_version = |abapGit version: { gc_abap_version }|.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'abapGit XML version mismatch'
        txt1  = 'abapGit XML version mismatch'
        txt2  = 'See https://github.com/larshp/abapGit/wiki/XML-Mismatch'
        txt3  = lv_version.                                 "#EC NOTEXT

    lcx_exception=>raise( 'XML error' ).

  ENDMETHOD.                    "display_xml_error

  METHOD to_xml.
* will render to codepage UTF-16

    DATA: li_ostream       TYPE REF TO if_ixml_ostream,
          li_renderer      TYPE REF TO if_ixml_renderer,
          li_streamfactory TYPE REF TO if_ixml_stream_factory.


    li_streamfactory = mi_ixml->create_stream_factory( ).

    li_ostream = li_streamfactory->create_ostream_cstring( rv_xml ).

    li_renderer = mi_ixml->create_renderer( ostream  = li_ostream
                                            document = mi_xml_doc ).
    li_renderer->set_normalizing( iv_normalize ).

    li_renderer->render( ).

  ENDMETHOD.                    "to_xml

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

    lcx_exception=>raise( 'Error while parsing XML' ).
  ENDMETHOD.                    "error

ENDCLASS.                    "lcl_xml IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_xml_output DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml_output DEFINITION FINAL INHERITING FROM lcl_xml CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      add
        IMPORTING iv_name TYPE clike
                  ig_data TYPE any
        RAISING   lcx_exception,
      set_raw
        IMPORTING ii_raw TYPE REF TO if_ixml_element,
      add_xml
        IMPORTING iv_name TYPE clike
                  ii_xml  TYPE REF TO if_ixml_element,
      render
        IMPORTING iv_normalize  TYPE sap_bool DEFAULT abap_true
                  is_metadata   TYPE ty_metadata OPTIONAL
        RETURNING VALUE(rv_xml) TYPE string.

  PRIVATE SECTION.
    DATA: mi_raw  TYPE REF TO if_ixml_element.

ENDCLASS.                    "lcl_xml_output DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_xml_output IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml_output IMPLEMENTATION.

  METHOD set_raw.
    mi_raw = ii_raw.
  ENDMETHOD.                    "set_raw

  METHOD add.

    DATA: li_node TYPE REF TO if_ixml_node,
          li_doc  TYPE REF TO if_ixml_document,
          lt_stab TYPE abap_trans_srcbind_tab.

    FIELD-SYMBOLS: <ls_stab> LIKE LINE OF lt_stab.


    ASSERT NOT iv_name IS INITIAL.

    APPEND INITIAL LINE TO lt_stab ASSIGNING <ls_stab>.
    <ls_stab>-name = iv_name.
    GET REFERENCE OF ig_data INTO <ls_stab>-value.

    li_doc = cl_ixml=>create( )->create_document( ).

    CALL TRANSFORMATION id
      OPTIONS initial_components = 'suppress'
      SOURCE (lt_stab)
      RESULT XML li_doc.

    li_node = mi_xml_doc->get_root( )->get_first_child( ).
    IF li_node IS BOUND.
      mi_xml_doc->get_root( )->get_first_child( )->get_first_child( )->append_child(
        li_doc->get_root( )->get_first_child( )->get_first_child( )->get_first_child( ) ).
    ELSE.
      mi_xml_doc->get_root( )->append_child( li_doc->get_root( )->get_first_child( ) ).
    ENDIF.

  ENDMETHOD.

  METHOD add_xml.

    DATA: li_element TYPE REF TO if_ixml_element.

    li_element = mi_xml_doc->create_element( iv_name ).
    li_element->append_child( ii_xml ).

    mi_xml_doc->get_root( )->get_first_child( )->get_first_child( )->append_child( li_element ).

  ENDMETHOD.

  METHOD render.

    DATA: li_git  TYPE REF TO if_ixml_element,
          li_abap TYPE REF TO if_ixml_element.


    IF mi_raw IS INITIAL.
      li_abap ?= mi_xml_doc->get_root( )->get_first_child( ).
      mi_xml_doc->get_root( )->remove_child( li_abap ).
    ELSE.
      li_abap = mi_raw.
    ENDIF.

    li_git = mi_xml_doc->create_element( c_abapgit_tag ).
    li_git->set_attribute( name = c_attr_version value = gc_xml_version ). "#EC NOTEXT
    IF NOT is_metadata IS INITIAL.
      li_git->set_attribute( name  = c_attr_serializer
                             value = is_metadata-class ).   "#EC NOTEXT
      li_git->set_attribute( name  = c_attr_serializer_version
                             value = is_metadata-version ). "#EC NOTEXT
    ENDIF.
    li_git->append_child( li_abap ).
    mi_xml_doc->get_root( )->append_child( li_git ).

    rv_xml = to_xml( iv_normalize ).

  ENDMETHOD.                    "render

ENDCLASS.                    "lcl_xml_output IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_xml_input DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml_input DEFINITION FINAL INHERITING FROM lcl_xml CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_xml TYPE clike
        RAISING   lcx_exception,
      read
        IMPORTING iv_name TYPE clike
        CHANGING  cg_data TYPE any
        RAISING   lcx_exception,
      get_raw
        RETURNING VALUE(ri_raw) TYPE REF TO if_ixml_document,
* todo, add read_xml to match add_xml in lcl_xml_output
      get_metadata
        RETURNING VALUE(rs_metadata) TYPE ty_metadata.

  PRIVATE SECTION.
    METHODS: fix_xml.

ENDCLASS.                    "lcl_xml_input DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_xml_input IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_xml_input IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).
    parse( iv_xml ).
    fix_xml( ).

  ENDMETHOD.                    "constructor

  METHOD get_raw.
    ri_raw = mi_xml_doc.
  ENDMETHOD.                    "get_raw

  METHOD fix_xml.

    DATA: li_git  TYPE REF TO if_ixml_element,
          li_abap TYPE REF TO if_ixml_node.


    li_git ?= mi_xml_doc->find_from_name_ns( depth = 0 name = c_abapgit_tag ).
    li_abap = li_git->get_first_child( ).

    mi_xml_doc->get_root( )->remove_child( li_git ).
    mi_xml_doc->get_root( )->append_child( li_abap ).

  ENDMETHOD.                    "fix_xml

  METHOD read.

    DATA: lx_error TYPE REF TO cx_transformation_error,
          lt_rtab  TYPE abap_trans_resbind_tab.

    FIELD-SYMBOLS: <ls_rtab> LIKE LINE OF lt_rtab.


    ASSERT NOT iv_name IS INITIAL.

    APPEND INITIAL LINE TO lt_rtab ASSIGNING <ls_rtab>.
    <ls_rtab>-name = iv_name.
    GET REFERENCE OF cg_data INTO <ls_rtab>-value.

    TRY.
        CALL TRANSFORMATION id
          OPTIONS value_handling = 'accept_data_loss'
          SOURCE XML mi_xml_doc
          RESULT (lt_rtab) ##no_text.
      CATCH cx_transformation_error INTO lx_error.
        lcx_exception=>raise( lx_error->if_message~get_text( ) ).
    ENDTRY.

  ENDMETHOD.                    "read

  METHOD get_metadata.
    rs_metadata = ms_metadata.
  ENDMETHOD.                    "get_metadata

ENDCLASS.                    "lcl_xml_input IMPLEMENTATION

CLASS lcl_xml_pretty DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: print
      IMPORTING iv_xml           TYPE string
                iv_ignore_errors TYPE abap_bool DEFAULT abap_true
                iv_unpretty      TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_xml)    TYPE string
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_xml_pretty IMPLEMENTATION.

  METHOD print.

    DATA: li_ixml           TYPE REF TO if_ixml,
          li_xml_doc        TYPE REF TO if_ixml_document,
          li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_parser         TYPE REF TO if_ixml_parser,
          li_ostream        TYPE REF TO if_ixml_ostream,
          li_renderer       TYPE REF TO if_ixml_renderer.


    ASSERT NOT iv_xml IS INITIAL.

    li_ixml    = cl_ixml=>create( ).
    li_xml_doc = li_ixml->create_document( ).

    li_stream_factory = li_ixml->create_stream_factory( ).
    li_istream        = li_stream_factory->create_istream_string( iv_xml ).
    li_parser         = li_ixml->create_parser( stream_factory = li_stream_factory
                                                istream        = li_istream
                                                document       = li_xml_doc ).
    li_parser->set_normalizing( abap_true ).
    IF li_parser->parse( ) <> 0.
      IF iv_ignore_errors = abap_true.
        rv_xml = iv_xml.
        RETURN.
      ELSE.
        lcx_exception=>raise( 'error parsing xml' ).
      ENDIF.
    ENDIF.
    li_istream->close( ).


    li_ostream  = li_stream_factory->create_ostream_cstring( rv_xml ).

    li_renderer = li_ixml->create_renderer( ostream  = li_ostream
                                            document = li_xml_doc ).

    li_renderer->set_normalizing( boolc( iv_unpretty = abap_false ) ).

    li_renderer->render( ).

  ENDMETHOD.

ENDCLASS.
