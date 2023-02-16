CLASS ltcl_xml DEFINITION DEFERRED.

CLASS ltcl_xml_concrete DEFINITION FOR TESTING
    FINAL
    INHERITING FROM zcl_abapgit_xml
    FRIENDS ltcl_xml.
ENDCLASS.

CLASS ltcl_xml_concrete IMPLEMENTATION.
ENDCLASS.

CLASS ltcl_xml DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS setup.

    METHODS:
      space_leading_trailing FOR TESTING RAISING zcx_abapgit_exception,
      bad_version_raises_exc FOR TESTING RAISING cx_static_check,
      bad_xml_raises_exc FOR TESTING RAISING cx_static_check.

    METHODS:
      parse_xml
        IMPORTING
          iv_xml TYPE csequence
        RAISING
          zcx_abapgit_exception,
      render_xml
        IMPORTING
          iv_name       TYPE string
        RETURNING
          VALUE(rv_xml) TYPE string.

    DATA mo_xml TYPE REF TO ltcl_xml_concrete.

ENDCLASS.


CLASS ltcl_xml IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_xml.
  ENDMETHOD.

  METHOD parse_xml.

    DATA lv_xml TYPE string.

    lv_xml = |<?xml version="1.0"?>|
          && |<{ mo_xml->c_abapgit_tag } { mo_xml->c_attr_version }="{ zif_abapgit_version=>c_xml_version }">|
          && iv_xml
          && |</{ mo_xml->c_abapgit_tag }>|.

    mo_xml->parse( iv_xml = lv_xml ).

  ENDMETHOD.

  METHOD space_leading_trailing.

    DATA: lv_from_xml TYPE string,
          lv_to_xml   TYPE string.

    lv_from_xml = `<FOO> A </FOO>`.

    parse_xml( lv_from_xml ).

    lv_to_xml = render_xml( 'FOO' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_to_xml
      exp = |*{ lv_from_xml }*| ).

  ENDMETHOD.

  METHOD render_xml.

* this code replicates the functionality in ZCL_ABAPGIT_XML_OUTPUT,

    DATA: li_ostream       TYPE REF TO if_ixml_ostream,
          li_renderer      TYPE REF TO if_ixml_renderer,
          li_streamfactory TYPE REF TO if_ixml_stream_factory.

    li_streamfactory = mo_xml->mi_ixml->create_stream_factory( ).

    li_ostream = li_streamfactory->create_ostream_cstring( rv_xml ).

    li_renderer = mo_xml->mi_ixml->create_renderer(
      ostream  = li_ostream
      document = mo_xml->mi_xml_doc ).

    li_renderer->render( ).

  ENDMETHOD.

  METHOD bad_version_raises_exc.

    DATA: lv_xml   TYPE string,
          lo_error TYPE REF TO zcx_abapgit_exception,
          lv_text  TYPE string.

    lv_xml = |<?xml version="1.0"?>|
          && |<{ mo_xml->c_abapgit_tag } { mo_xml->c_attr_version }="v9.8.7">|
          && |<TEST>data</TEST>|
          && |</{ mo_xml->c_abapgit_tag }>|.

    TRY.
        mo_xml->parse( iv_xml = lv_xml ).
        cl_abap_unit_assert=>fail( msg = 'Exception not raised' ).

      CATCH zcx_abapgit_exception INTO lo_error.
        lv_text = lo_error->get_text( ).
        cl_abap_unit_assert=>assert_char_cp(
            act              = lv_text
            exp              = '*XML version*' ).
    ENDTRY.

  ENDMETHOD.

  METHOD bad_xml_raises_exc.

    DATA: lv_xml   TYPE string,
          lo_error TYPE REF TO zcx_abapgit_exception,
          lv_text  TYPE string.

    lv_xml = |<?xml version="1.0"?>|
          && |<{ mo_xml->c_abapgit_tag } { mo_xml->c_attr_version }="{ zif_abapgit_version=>c_xml_version }">|
          && |<open_tag>|
          && |</{ mo_xml->c_abapgit_tag }>|.

    TRY.
        mo_xml->parse( iv_xml = lv_xml ).
        cl_abap_unit_assert=>fail( msg = 'Exception not raised' ).

      CATCH zcx_abapgit_exception INTO lo_error.
        lv_text = lo_error->get_text( ).
        cl_abap_unit_assert=>assert_char_cp(
            act              = lv_text
            exp              = '*open_tag*' ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
