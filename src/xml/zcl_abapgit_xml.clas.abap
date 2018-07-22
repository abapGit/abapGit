CLASS zcl_abapgit_xml DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor.

  PROTECTED SECTION.
    DATA: mi_ixml     TYPE REF TO if_ixml,
          mi_xml_doc  TYPE REF TO if_ixml_document,
          ms_metadata TYPE zif_abapgit_definitions=>ty_metadata.

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
      RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    METHODS error
      IMPORTING ii_parser TYPE REF TO if_ixml_parser
      RAISING   zcx_abapgit_exception.

    METHODS display_xml_error
      RAISING zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_XML IMPLEMENTATION.


  METHOD constructor.
    mi_ixml = cl_ixml=>create( ).
    mi_xml_doc = mi_ixml->create_document( ).
  ENDMETHOD.                    "constructor


  METHOD display_xml_error.

    DATA: lv_version TYPE string.


    lv_version = |abapGit version: { zif_abapgit_version=>gc_abap_version }|.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'abapGit XML version mismatch'
        txt1  = 'abapGit XML version mismatch'
        txt2  = 'See http://larshp.github.io/abapGit/other-xml-mismatch.html'
        txt3  = lv_version.                                 "#EC NOTEXT

    zcx_abapgit_exception=>raise( 'XML error' ).

  ENDMETHOD.                    "display_xml_error


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

    zcx_abapgit_exception=>raise( 'Error while parsing XML' ).
  ENDMETHOD.                    "error


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
    IF li_version->get_value( ) <> zif_abapgit_version=>gc_xml_version.
      display_xml_error( ).
    ENDIF.

* buffer serializer metadata. Git node will be removed lateron
    ms_metadata-class   = li_element->get_attribute_ns( c_attr_serializer ).
    ms_metadata-version = li_element->get_attribute_ns( c_attr_serializer_version ).

  ENDMETHOD.                    "parse


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
ENDCLASS.
