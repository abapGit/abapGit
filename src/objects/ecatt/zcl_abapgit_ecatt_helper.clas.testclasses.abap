CLASS ltcl_strip_deep_data DEFINITION DEFERRED.
CLASS zcl_abapgit_ecatt_helper DEFINITION LOCAL FRIENDS ltcl_strip_deep_data.


CLASS ltcl_strip_deep_data DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.

    CONSTANTS c_rfc TYPE string VALUE 'xmlns:rfc="http://xml.sap.com/2003/05/eCATT/rfc"'.

    METHODS mixed_content_trimmed FOR TESTING RAISING cx_static_check.
    METHODS whitespace_nodes_removed FOR TESTING RAISING cx_static_check.
    METHODS leaf_value_kept FOR TESTING RAISING cx_static_check.
    METHODS whitespace_leaf_kept FOR TESTING RAISING cx_static_check.
    METHODS inner_blanks_kept FOR TESTING RAISING cx_static_check.
    METHODS entity_in_mixed_text FOR TESTING RAISING cx_static_check.
    METHODS cdata_and_comment_kept FOR TESTING RAISING cx_static_check.
    METHODS outside_deep_data_kept FOR TESTING RAISING cx_static_check.
    METHODS several_deep_data FOR TESTING RAISING cx_static_check.
    METHODS clean_xml_unchanged FOR TESTING RAISING cx_static_check.
    METHODS idempotent FOR TESTING RAISING cx_static_check.

    METHODS strip
      IMPORTING
        iv_xml        TYPE string
      RETURNING
        VALUE(rv_xml) TYPE string.

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


CLASS ltcl_strip_deep_data IMPLEMENTATION.

  METHOD parse.

    " keep the whitespace as the eCATT parser does: no strip space element, no normalizing
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
    li_parser->set_normalizing( abap_false ).
    lv_subrc = li_parser->parse( ).
    li_istream->close( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_subrc
      exp = 0 ).

  ENDMETHOD.

  METHOD render.

    " returns the document from its root element on, without BOM and XML declaration
    DATA li_ostream  TYPE REF TO if_ixml_ostream.
    DATA li_renderer TYPE REF TO if_ixml_renderer.
    DATA li_factory  TYPE REF TO if_ixml_stream_factory.
    DATA li_ixml     TYPE REF TO if_ixml.
    DATA lv_offset   TYPE i.

    li_ixml = cl_ixml=>create( ).

    li_factory = li_ixml->create_stream_factory( ).
    li_ostream = li_factory->create_ostream_cstring( rv_xml ).
    li_renderer = li_ixml->create_renderer(
      ostream  = li_ostream
      document = ii_doc ).
    li_renderer->render( ).

    FIND FIRST OCCURRENCE OF REGEX '<[^?]' IN rv_xml MATCH OFFSET lv_offset.
    IF sy-subrc = 0.
      rv_xml = rv_xml+lv_offset.
    ENDIF.

  ENDMETHOD.

  METHOD strip.

    DATA li_doc TYPE REF TO if_ixml_document.

    li_doc = parse( iv_xml ).
    zcl_abapgit_ecatt_helper=>strip_deep_data_whitespace( li_doc->get_root_element( ) ).
    rv_xml = render( li_doc ).

  ENDMETHOD.

  METHOD mixed_content_trimmed.

    cl_abap_unit_assert=>assert_equals(
      act = strip( |<DEEP_DATA><rfc:FM { c_rfc }><INPUT>\n <valueRefName>I_INPUT</valueRefName>\n X\n</INPUT>|
                && |</rfc:FM></DEEP_DATA>| )
      exp = |<DEEP_DATA><rfc:FM { c_rfc }><INPUT><valueRefName>I_INPUT</valueRefName>X</INPUT>|
         && |</rfc:FM></DEEP_DATA>| ).

  ENDMETHOD.

  METHOD whitespace_nodes_removed.

    cl_abap_unit_assert=>assert_equals(
      act = strip( |<DEEP_DATA>\n <rfc:FM { c_rfc }>\n  <IMPORTING>\n   <INPUT/>\n  </IMPORTING>\n\n\n|
                && |  <EXPORTING>\n   <OUTPUT/>\n  </EXPORTING>\n\n </rfc:FM>\n</DEEP_DATA>| )
      exp = |<DEEP_DATA><rfc:FM { c_rfc }><IMPORTING><INPUT/></IMPORTING>|
         && |<EXPORTING><OUTPUT/></EXPORTING></rfc:FM></DEEP_DATA>| ).

  ENDMETHOD.

  METHOD leaf_value_kept.

    cl_abap_unit_assert=>assert_equals(
      act = strip( |<DEEP_DATA>\n <FM>\n  <INPUT>  value with blanks  </INPUT>\n </FM>\n</DEEP_DATA>| )
      exp = |<DEEP_DATA><FM><INPUT>  value with blanks  </INPUT></FM></DEEP_DATA>| ).

  ENDMETHOD.

  METHOD whitespace_leaf_kept.

    cl_abap_unit_assert=>assert_equals(
      act = strip( |<DEEP_DATA>\n <FM>\n  <INPUT> </INPUT>\n </FM>\n</DEEP_DATA>| )
      exp = |<DEEP_DATA><FM><INPUT> </INPUT></FM></DEEP_DATA>| ).

  ENDMETHOD.

  METHOD inner_blanks_kept.

    cl_abap_unit_assert=>assert_equals(
      act = strip( |<DEEP_DATA><FM><A/>\n  x  y\n</FM></DEEP_DATA>| )
      exp = |<DEEP_DATA><FM><A/>x  y</FM></DEEP_DATA>| ).

  ENDMETHOD.

  METHOD entity_in_mixed_text.

    " the parser may split this text into several nodes
    cl_abap_unit_assert=>assert_equals(
      act = strip( |<DEEP_DATA><FM><A/>\n  a &amp; b &lt; c\n</FM></DEEP_DATA>| )
      exp = |<DEEP_DATA><FM><A/>a &amp; b &lt; c</FM></DEEP_DATA>| ).

  ENDMETHOD.

  METHOD cdata_and_comment_kept.

    cl_abap_unit_assert=>assert_equals(
      act = strip( |<DEEP_DATA><FM><A/><![CDATA[ c ]]><!-- k --></FM></DEEP_DATA>| )
      exp = |<DEEP_DATA><FM><A/><![CDATA[ c ]]><!-- k --></FM></DEEP_DATA>| ).

  ENDMETHOD.

  METHOD outside_deep_data_kept.

    cl_abap_unit_assert=>assert_equals(
      act = strip( |<ROOT><OTHER><A/> X </OTHER><DEEP_DATA><A/> X </DEEP_DATA></ROOT>| )
      exp = |<ROOT><OTHER><A/> X </OTHER><DEEP_DATA><A/>X</DEEP_DATA></ROOT>| ).

  ENDMETHOD.

  METHOD several_deep_data.

    cl_abap_unit_assert=>assert_equals(
      act = strip( |<ROOT><item><DEEP_DATA>\n <A/>\n</DEEP_DATA></item>|
                && |<item><DEEP_DATA>\n <B/>\n</DEEP_DATA></item></ROOT>| )
      exp = |<ROOT><item><DEEP_DATA><A/></DEEP_DATA></item><item><DEEP_DATA><B/></DEEP_DATA></item></ROOT>| ).

  ENDMETHOD.

  METHOD clean_xml_unchanged.

    DATA lv_xml TYPE string.

    lv_xml = |<DEEP_DATA><rfc:FM { c_rfc }><INPUT><valueRefName>I_INPUT</valueRefName>X</INPUT>|
          && |<OUTPUT/></rfc:FM></DEEP_DATA>|.

    cl_abap_unit_assert=>assert_equals(
      act = strip( lv_xml )
      exp = lv_xml ).

  ENDMETHOD.

  METHOD idempotent.

    DATA lv_once TYPE string.

    lv_once = strip( |<DEEP_DATA>\n <FM>\n  <INPUT>\n   <valueRefName>I</valueRefName>\n   X\n  </INPUT>\n</FM>|
                  && |\n</DEEP_DATA>| ).

    cl_abap_unit_assert=>assert_equals(
      act = strip( lv_once )
      exp = lv_once ).

  ENDMETHOD.

ENDCLASS.
