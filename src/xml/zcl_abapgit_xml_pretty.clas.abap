CLASS zcl_abapgit_xml_pretty DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS print
      IMPORTING
        !iv_xml           TYPE string
        !iv_ignore_errors TYPE abap_bool DEFAULT abap_true
        !iv_unpretty      TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_xml)     TYPE string
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS raise_error
      IMPORTING
        ii_parser TYPE REF TO if_ixml_parser
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_xml_pretty IMPLEMENTATION.


  METHOD print.

    DATA: li_ixml           TYPE REF TO if_ixml,
          li_xml_doc        TYPE REF TO if_ixml_document,
          li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_parser         TYPE REF TO if_ixml_parser,
          lv_xstring        TYPE xstring,
          li_encoding       TYPE REF TO if_ixml_encoding,
          li_ostream        TYPE REF TO if_ixml_ostream,
          li_renderer       TYPE REF TO if_ixml_renderer.


    ASSERT NOT iv_xml IS INITIAL.

    li_ixml    = cl_ixml=>create( ).
    li_xml_doc = li_ixml->create_document( ).

    li_stream_factory = li_ixml->create_stream_factory( ).
    li_istream        = li_stream_factory->create_istream_xstring(
      zcl_abapgit_convert=>string_to_xstring_utf8( iv_xml ) ).
    li_parser         = li_ixml->create_parser( stream_factory = li_stream_factory
                                                istream        = li_istream
                                                document       = li_xml_doc ).
    li_parser->set_normalizing( abap_true ).
    IF li_parser->parse( ) <> 0.
      IF iv_ignore_errors = abap_true.
        rv_xml = iv_xml.
        RETURN.
      ELSE.
        raise_error( li_parser ).
      ENDIF.
    ENDIF.


    li_ostream  = li_stream_factory->create_ostream_xstring( lv_xstring ).

    li_encoding = li_ixml->create_encoding(
      character_set = 'utf-8'
      byte_order    = if_ixml_encoding=>co_big_endian ).

    li_xml_doc->set_encoding( li_encoding ).
    li_renderer = li_ixml->create_renderer( ostream  = li_ostream
                                            document = li_xml_doc ).

    li_renderer->set_normalizing( boolc( iv_unpretty = abap_false ) ).

    li_renderer->render( ).

    rv_xml = zcl_abapgit_convert=>xstring_to_string_utf8_bom( lv_xstring ).
    REPLACE FIRST OCCURRENCE OF 'utf-8' IN rv_xml WITH 'utf-16'.

  ENDMETHOD.


  METHOD raise_error.

    DATA:
      lv_num_errors TYPE i,
      li_error      TYPE REF TO if_ixml_parse_error,
      lv_reason     TYPE string.

    lv_num_errors = ii_parser->num_errors( ).

    DO lv_num_errors TIMES.

      li_error = ii_parser->get_error(
                     index        = sy-index
                     min_severity = if_ixml_parse_error=>co_info ).

      IF li_error IS BOUND.
        lv_reason = li_error->get_reason( ).
      ENDIF.

      IF lv_reason IS NOT INITIAL.
        EXIT.
      ENDIF.

    ENDDO.

    IF lv_reason IS NOT INITIAL.
      zcx_abapgit_exception=>raise( lv_reason ).
    ELSE.
      zcx_abapgit_exception=>raise( 'error parsing xml' ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
