CLASS ltcl_test_sort_texts DEFINITION DEFERRED.
CLASS zcl_abapgit_object_ssfo DEFINITION LOCAL FRIENDS ltcl_test_sort_texts.


CLASS ltcl_test_sort_texts DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.

    METHODS empty_list FOR TESTING RAISING cx_static_check.
    METHODS basic FOR TESTING RAISING cx_static_check.
    METHODS field_omitted FOR TESTING RAISING cx_static_check.

    METHODS parse
      IMPORTING
        iv_xml        TYPE string
      RETURNING
        VALUE(ri_doc) TYPE REF TO if_ixml_document.

    METHODS render
      IMPORTING
        ii_doc        TYPE REF TO if_ixml_document
      RETURNING
        VALUE(rv_xml) TYPE string.
ENDCLASS.


CLASS ltcl_test_sort_texts IMPLEMENTATION.

  METHOD parse.

    DATA li_factory TYPE REF TO if_ixml_stream_factory.
    DATA li_istream TYPE REF TO if_ixml_istream.
    DATA li_parser  TYPE REF TO if_ixml_parser.
    DATA lv_subrc   TYPE i.
    DATA li_ixml    TYPE REF TO if_ixml.

    li_ixml = cl_ixml=>create( ).
    ri_doc = li_ixml->create_document( ).

    li_factory = li_ixml->create_stream_factory( ).
    li_istream = li_factory->create_istream_string( iv_xml ).
    li_parser = li_ixml->create_parser( stream_factory = li_factory
                                        istream        = li_istream
                                        document       = ri_doc ).
    li_parser->add_strip_space_element( ).
    lv_subrc = li_parser->parse( ).
    li_istream->close( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_subrc
      exp = 0 ).

  ENDMETHOD.

  METHOD render.

    DATA li_ostream  TYPE REF TO if_ixml_ostream.
    DATA li_renderer TYPE REF TO if_ixml_renderer.
    DATA li_factory  TYPE REF TO if_ixml_stream_factory.
    DATA li_ixml     TYPE REF TO if_ixml.

    li_ixml = cl_ixml=>create( ).

    li_factory = li_ixml->create_stream_factory( ).
    li_ostream = li_factory->create_ostream_cstring( rv_xml ).
    li_renderer = li_ixml->create_renderer(
      ostream  = li_ostream
      document = ii_doc ).
    li_renderer->render( ).

  ENDMETHOD.

  METHOD empty_list.

    DATA lv_xml     TYPE string.
    DATA lv_result  TYPE string.
    DATA li_xml_doc TYPE REF TO if_ixml_document.

    lv_xml = '<T_CAPTION></T_CAPTION>'.
    li_xml_doc = parse( lv_xml ).
    zcl_abapgit_object_ssfo=>sort_texts( li_xml_doc ).
    lv_result = render( li_xml_doc ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*<T_CAPTION/>*' ).

  ENDMETHOD.

  METHOD basic.

    DATA lv_xml     TYPE string.
    DATA lv_result  TYPE string.
    DATA li_xml_doc TYPE REF TO if_ixml_document.

    lv_xml =
      |<T_CAPTION>| &&
      |  <item>| &&
      |   <LANGU>B</LANGU>| &&
      |   <FORMNAME>ZFOOBAR</FORMNAME>| &&
      |   <OBJTYPE>CD</OBJTYPE>| &&
      |   <INAME>%CONDITION125</INAME>| &&
      |   <CAPTION>New Alternative 114</CAPTION>| &&
      |  </item>| &&
      |  <item>| &&
      |   <LANGU>A</LANGU>| &&
      |   <FORMNAME>ZFOOBAR</FORMNAME>| &&
      |   <OBJTYPE>CD</OBJTYPE>| &&
      |   <INAME>%CONDITION125</INAME>| &&
      |   <CAPTION>New Alternative 114</CAPTION>| &&
      |  </item>| &&
      |</T_CAPTION>|.
    li_xml_doc = parse( lv_xml ).
    zcl_abapgit_object_ssfo=>sort_texts( li_xml_doc ).
    lv_result = render( li_xml_doc ).

* check LANGU = A is sorted first
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*<T_CAPTION><item><LANGU>A</LANGU>*' ).

  ENDMETHOD.

  METHOD field_omitted.

    DATA lv_xml     TYPE string.
    DATA lv_result  TYPE string.
    DATA li_xml_doc TYPE REF TO if_ixml_document.

* CAPTION is omitted in the first item,
    lv_xml =
      |<T_CAPTION>| &&
      |  <item>| &&
      |   <LANGU>B</LANGU>| &&
      |   <FORMNAME>ZFOOBAR</FORMNAME>| &&
      |   <OBJTYPE>CD</OBJTYPE>| &&
      |   <INAME>%CONDITION125</INAME>| &&
      |  </item>| &&
      |  <item>| &&
      |   <LANGU>A</LANGU>| &&
      |   <FORMNAME>ZFOOBAR</FORMNAME>| &&
      |   <OBJTYPE>CD</OBJTYPE>| &&
      |   <INAME>%CONDITION125</INAME>| &&
      |   <CAPTION>New Alternative 114</CAPTION>| &&
      |  </item>| &&
      |</T_CAPTION>|.
    li_xml_doc = parse( lv_xml ).
    zcl_abapgit_object_ssfo=>sort_texts( li_xml_doc ).
    lv_result = render( li_xml_doc ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*New Alternative 114*' ).

  ENDMETHOD.

ENDCLASS.
