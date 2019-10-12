CLASS ltcl_xml_output DEFINITION DEFERRED.
CLASS zcl_abapgit_xml_output DEFINITION LOCAL FRIENDS ltcl_xml_output.

CLASS ltcl_xml_output DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS:
      render_xml_string FOR TESTING
        RAISING zcx_abapgit_exception,
      add_simple_object FOR TESTING
        RAISING zcx_abapgit_exception.

    TYPES: BEGIN OF st_old,
             foo TYPE i,
             bar TYPE c LENGTH 1,
           END OF st_old.

ENDCLASS.
CLASS ltcl_xml_output IMPLEMENTATION.

  METHOD add_simple_object.

    DATA: ls_input       TYPE st_old,
          ls_result      TYPE st_old,
          lv_value       TYPE string,
          li_xml_element TYPE REF TO if_ixml_element,
          lo_output      TYPE REF TO zcl_abapgit_xml_output.

    ls_input-foo = '2'.
    ls_input-bar = 'A'.

    CREATE OBJECT lo_output.
    lo_output->add( iv_name = 'DATA'
                    ig_data = ls_input ).

    li_xml_element = lo_output->mi_xml_doc->find_from_name( 'FOO' ).
    lv_value = li_xml_element->get_value( ).
    ls_result-foo = lv_value.
    li_xml_element = lo_output->mi_xml_doc->find_from_name( 'BAR' ).
    lv_value = li_xml_element->get_value( ).
    ls_result-bar = lv_value.

    cl_abap_unit_assert=>assert_equals(
      act = ls_input
      exp = ls_result ).

  ENDMETHOD.
  METHOD render_xml_string.

    DATA: ls_input          TYPE st_old,
          lv_value          TYPE string,
          lv_xml            TYPE string,
          lo_output         TYPE REF TO zcl_abapgit_xml_output,
          lo_conv_in_string TYPE REF TO cl_abap_conv_in_ce,
          lv_encoding       TYPE abap_encoding,
          lv_xstring        TYPE xstring.

    ls_input-foo = '2'.
    ls_input-bar = 'A'.

    lv_value =
      '<?xml version="1.0" encoding="utf-16"?>#<abapGit version="v1.0.0">#' &
      ' <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">#' &
      '  <asx:values>#   <DATA>#    <FOO>2</FOO>#    <BAR>A' &
      '</BAR>#   </DATA>#  </asx:values># </asx:abap>#</abapGit>#'.

    REPLACE ALL OCCURRENCES OF '#' IN lv_value WITH cl_abap_char_utilities=>newline.

    CREATE OBJECT lo_output.
    lo_output->add( iv_name = 'DATA'
                    ig_data = ls_input ).

    lv_xml = lo_output->render( ).

    lv_encoding = cl_abap_codepage=>sap_codepage( `UTF-16LE` ).

    TRY.
        lv_xstring = cl_bcs_convert=>string_to_xstring(
                       iv_string     = lv_value
                       iv_codepage   = lv_encoding
                       iv_add_bom    = 'X' ).
      CATCH cx_bcs.
    ENDTRY.

    lo_conv_in_string = cl_abap_conv_in_ce=>create(
                          encoding = lv_encoding
                          input = lv_xstring ).

    lo_conv_in_string->read( IMPORTING data = lv_value ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_xml
      exp = lv_value ).

  ENDMETHOD.

ENDCLASS.
