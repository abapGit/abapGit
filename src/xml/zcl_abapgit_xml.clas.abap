CLASS zcl_abapgit_xml DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING iv_filename TYPE string OPTIONAL.
  PROTECTED SECTION.
    DATA: mi_ixml     TYPE REF TO if_ixml,
          mi_xml_doc  TYPE REF TO if_ixml_document,
          ms_metadata TYPE zif_abapgit_definitions=>ty_metadata,
          mv_filename TYPE string.

    CONSTANTS: c_abapgit_tag             TYPE string VALUE 'abapGit' ##NO_TEXT,
               c_attr_version            TYPE string VALUE 'version' ##NO_TEXT,
               c_attr_serializer         TYPE string VALUE 'serializer' ##NO_TEXT,
               c_attr_serializer_version TYPE string VALUE 'serializer_version' ##NO_TEXT.

    METHODS to_xml
      IMPORTING iv_normalize  TYPE abap_bool DEFAULT abap_true
      RETURNING VALUE(rv_xml) TYPE string.

    METHODS parse
      IMPORTING iv_xml TYPE string
      RAISING   zcx_abapgit_exception.
  PRIVATE SECTION.

    METHODS error
      IMPORTING ii_parser TYPE REF TO if_ixml_parser
      RAISING   zcx_abapgit_exception.
    METHODS display_version_mismatch
      RAISING zcx_abapgit_exception.
    METHODS show_parser_errors
      IMPORTING ii_parser TYPE REF TO if_ixml_parser.
    METHODS raise_exception_for
      IMPORTING
        ii_error TYPE REF TO if_ixml_parse_error
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_xml IMPLEMENTATION.


  METHOD constructor.
    mi_ixml     = cl_ixml=>create( ).
    mi_xml_doc  = mi_ixml->create_document( ).
    mv_filename = iv_filename.
  ENDMETHOD.


  METHOD display_version_mismatch.

    DATA: lv_version TYPE string.
    DATA: lv_file    TYPE string.

    lv_version = |abapGit version: { zif_abapgit_version=>gc_abap_version }|.
    IF mv_filename IS NOT INITIAL.
      lv_file = |File: { mv_filename }|.
    ENDIF.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'abapGit XML version mismatch'
        txt1  = 'abapGit XML version mismatch'
        txt2  = 'See https://docs.abapgit.org/other-xml-mismatch.html'
        txt3  = lv_version
        txt4  = lv_file.

    IF mv_filename IS INITIAL.
      zcx_abapgit_exception=>raise( 'abapGit XML version mismatch' ).
    ELSE.
      zcx_abapgit_exception=>raise( |abapGit XML version mismatch in file { mv_filename }| ).
    ENDIF.

  ENDMETHOD.


  METHOD error.

    IF ii_parser->num_errors( ) <> 0.

      IF zcl_abapgit_ui_factory=>get_gui_functions( )->gui_is_available( ) = abap_true.
        show_parser_errors( ii_parser ).
      ELSE.
        raise_exception_for( ii_parser->get_error( 0 ) ).
      ENDIF.

    ENDIF.

    IF mv_filename IS INITIAL.
      zcx_abapgit_exception=>raise( |Error while parsing XML| ).
    ELSE.
      zcx_abapgit_exception=>raise( |Error while parsing XML file { mv_filename }| ).
    ENDIF.

  ENDMETHOD.


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
    li_parser->add_strip_space_element( ).
    IF li_parser->parse( ) <> 0.
      error( li_parser ).
    ENDIF.

    li_istream->close( ).


    li_element = mi_xml_doc->find_from_name_ns( depth = 0
                                                name = c_abapgit_tag ).
    li_version = li_element->if_ixml_node~get_attributes(
      )->get_named_item_ns( c_attr_version ).
    IF li_version->get_value( ) <> zif_abapgit_version=>gc_xml_version.
      display_version_mismatch( ).
    ENDIF.

* buffer serializer metadata. Git node will be removed lateron
    ms_metadata-class   = li_element->get_attribute_ns( c_attr_serializer ).
    ms_metadata-version = li_element->get_attribute_ns( c_attr_serializer_version ).

  ENDMETHOD.


  METHOD to_xml.
* will render to codepage UTF-16

    DATA: li_ostream       TYPE REF TO if_ixml_ostream,
          li_renderer      TYPE REF TO if_ixml_renderer,
          lv_mark          TYPE string,
          li_streamfactory TYPE REF TO if_ixml_stream_factory.


    li_streamfactory = mi_ixml->create_stream_factory( ).

    li_ostream = li_streamfactory->create_ostream_cstring( rv_xml ).

    li_renderer = mi_ixml->create_renderer( ostream  = li_ostream
                                            document = mi_xml_doc ).
    li_renderer->set_normalizing( iv_normalize ).

    li_renderer->render( ).

    "unicode systems always add the byte order mark to the xml, while non-unicode does not
    "this code will always add the byte order mark if it is not in the xml
    lv_mark = zcl_abapgit_convert=>xstring_to_string_utf8( cl_abap_char_utilities=>byte_order_mark_utf8 ).
    IF rv_xml(1) <> lv_mark.
      CONCATENATE lv_mark rv_xml INTO rv_xml.
    ENDIF.

  ENDMETHOD.

  METHOD show_parser_errors.

    DATA lv_error TYPE i.
    DATA lv_column TYPE string.
    DATA lv_line TYPE string.
    DATA lv_reason TYPE string.
    DATA lv_txt1 TYPE string.
    DATA lv_txt2 TYPE string.
    DATA lv_txt3 TYPE string.
    DATA lv_txt4 TYPE string.
    DATA lv_times TYPE i.
    DATA li_error TYPE REF TO if_ixml_parse_error.

    lv_times = ii_parser->num_errors( ).

    DO lv_times TIMES.
      lv_error = sy-index - 1.
      li_error = ii_parser->get_error( lv_error ).

      lv_column = li_error->get_column( ).
      lv_line   = li_error->get_line( ).
      lv_reason = li_error->get_reason( ).

      IF mv_filename IS NOT INITIAL.
        lv_txt1 = |File: { mv_filename }|.
        lv_txt2 = |Column: { lv_column }|.
        lv_txt3 = |Line: { lv_line }|.
        lv_txt4 = lv_reason.
      ELSE.
        lv_txt1 = |Column: { lv_column }|.
        lv_txt2 = |Line: { lv_line }|.
        lv_txt3 = lv_reason.
        CLEAR lv_txt4.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Error from XML parser'
          txt1  = lv_txt1
          txt2  = lv_txt2
          txt3  = lv_txt3
          txt4  = lv_txt4.
    ENDDO.

  ENDMETHOD.


  METHOD raise_exception_for.
    DATA lv_message TYPE string.

    lv_message = |XML parser error: { ii_error->get_reason( ) }, | &&
                 |Line { ii_error->get_line( ) } | &&
                 |Col. { ii_error->get_column( ) }|.

    IF mv_filename IS NOT INITIAL.
      lv_message = lv_message && | File { mv_filename }|.
    ENDIF.

    zcx_abapgit_exception=>raise( lv_message ).

  ENDMETHOD.

ENDCLASS.
