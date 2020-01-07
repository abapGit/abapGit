CLASS ltcl_syntax_cases DEFINITION DEFERRED.
CLASS zcl_abapgit_syntax_highlighter DEFINITION LOCAL FRIENDS ltcl_syntax_cases.

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_cases definition
*----------------------------------------------------------------------*
CLASS ltcl_syntax_cases DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.

    DATA:
      mt_after_parse  TYPE zcl_abapgit_syntax_highlighter=>ty_match_tt,
      mt_after_order  TYPE zcl_abapgit_syntax_highlighter=>ty_match_tt,
      mt_after_extend TYPE zcl_abapgit_syntax_highlighter=>ty_match_tt,
      ms_match        TYPE zcl_abapgit_syntax_highlighter=>ty_match.

    METHODS:
      do_test IMPORTING iv_line     TYPE string
                        iv_filename TYPE string,
      test_abap_01 FOR TESTING,
      test_abap_02 FOR TESTING,
      test_abap_03 FOR TESTING,
      test_abap_04 FOR TESTING,
      test_abap_05 FOR TESTING,
      test_abap_06 FOR TESTING,
      test_abap_07 FOR TESTING,
      test_abap_08 FOR TESTING,
      test_xml_01  FOR TESTING,
      test_xml_02  FOR TESTING,
      test_xml_03  FOR TESTING,
      test_xml_04  FOR TESTING,
      test_xml_05  FOR TESTING,
      test_xml_06  FOR TESTING,
      test_xml_07  FOR TESTING,
      test_xml_08  FOR TESTING.

ENDCLASS.
*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_cases IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS ltcl_syntax_cases IMPLEMENTATION.

  DEFINE _generate_parse.
    ms_match-token    = &1.
    ms_match-offset   = &2.
    ms_match-length   = &3.
    APPEND ms_match TO mt_after_parse.
  END-OF-DEFINITION.

  DEFINE _generate_order.
    ms_match-token    = &1.
    ms_match-offset   = &2.
    ms_match-length   = &3.
    ms_match-text_tag = &4.
    APPEND ms_match TO mt_after_order.
  END-OF-DEFINITION.

  DEFINE _generate_extend.
    ms_match-token    = &1.
    ms_match-offset   = &2.
    ms_match-length   = &3.
    ms_match-text_tag = &4.
    APPEND ms_match TO mt_after_extend.
  END-OF-DEFINITION.

  METHOD do_test.

    DATA: lt_matches_act TYPE zcl_abapgit_syntax_highlighter=>ty_match_tt,
          lo_syntax      TYPE REF TO zcl_abapgit_syntax_highlighter.


    lo_syntax = zcl_abapgit_syntax_highlighter=>create( iv_filename ).
    lt_matches_act = lo_syntax->parse_line( iv_line ).

    SORT lt_matches_act BY offset.

    cl_abap_unit_assert=>assert_equals( exp = mt_after_parse
                                        act = lt_matches_act
                                        msg = | Error during parsing: { iv_line }| ).

    lo_syntax->order_matches( EXPORTING iv_line    = iv_line
                       CHANGING  ct_matches = lt_matches_act ).

    cl_abap_unit_assert=>assert_equals( exp = mt_after_order
                                        act = lt_matches_act
                                        msg = | Error during ordering: { iv_line }| ).

    lo_syntax->extend_matches(
      EXPORTING
        iv_line    = iv_line
      CHANGING
        ct_matches = lt_matches_act ).

    cl_abap_unit_assert=>assert_equals( exp = mt_after_extend
                                        act = lt_matches_act
                                        msg = | Error during extending: { iv_line }| ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering of comments              *
******************************************************
  METHOD test_abap_01.

    DATA lv_line TYPE string.

    lv_line = '* commented out line with key word data'.    "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'C' 0  1.
    _generate_parse 'K' 12 3.
    _generate_parse 'K' 16 4.
    _generate_parse 'K' 21 4.
    _generate_parse 'K' 26 3.
    _generate_parse 'K' 30 4.
    _generate_parse 'K' 35 4.

    " Generate table with expected values after ordering
    _generate_order 'C' 0  39 ''.

    " Generate table with expected values after ordering
    _generate_extend 'C' 0  39 ''.

    do_test( iv_line = lv_line iv_filename = '*.abap' ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering of remainder of string   *
******************************************************
  METHOD test_abap_02.

    DATA lv_line TYPE string.

    lv_line = 'data: lv_var_name type string.'.             "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'K' 0  4.
    _generate_parse 'K' 18 4.

    " Generate table with expected values after ordering
    _generate_order 'K' 0  4  ''.
    _generate_order 'K' 18 4  ''.

    " Generate table with expected values after extending
    _generate_extend 'K' 0  4  ''.
    _generate_extend '.' 4  14 ''.
    _generate_extend 'K' 18 4  ''.
    _generate_extend '.' 22 8  ''.

    do_test( iv_line = lv_line iv_filename = '*.abap' ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering of key words & texts     *
******************************************************
  METHOD test_abap_03.

    DATA lv_line TYPE string.


    lv_line = 'call function ''FM_NAME''. " Commented'.     "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'K' 0  4.
    _generate_parse 'K' 5  8.
    _generate_parse 'T' 14 1.
    _generate_parse 'T' 22 1.
    _generate_parse 'C' 25 1.

    " Generate table with expected values after ordering
    _generate_order 'K' 0  4  ''.
    _generate_order 'K' 5  8  ''.
    _generate_order 'T' 14 9  ''''.
    _generate_order 'C' 25 11 ''.

    " Generate table with expected values after extending
    _generate_extend 'K' 0  4  ''.
    _generate_extend '.' 4  1  ''.
    _generate_extend 'K' 5  8  ''.
    _generate_extend '.' 13 1  ''.
    _generate_extend 'T' 14 9  ''''.
    _generate_extend '.' 23 2  ''.
    _generate_extend 'C' 25 11 ''.

    do_test( iv_line = lv_line iv_filename = '*.abap' ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering of key words in texts    *
******************************************************
  METHOD test_abap_04.

    DATA lv_line TYPE string.

    lv_line = 'constants: lc_var type string value ''simpletext data simpletext''.'. "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'K' 0  9.
    _generate_parse 'K' 18 4.
    _generate_parse 'K' 30 5.
    _generate_parse 'T' 36 1.
    _generate_parse 'K' 48 4.
    _generate_parse 'T' 63 1.

    " Generate table with expected values after ordering
    _generate_order 'K' 0  9  ''.
    _generate_order 'K' 18 4  ''.
    _generate_order 'K' 30 5  ''.
    _generate_order 'T' 36 28 ''''.

    " Generate table with expected values after ordering
    _generate_extend 'K' 0  9  ''.
    _generate_extend '.' 9  9  ''.
    _generate_extend 'K' 18 4  ''.
    _generate_extend '.' 22 8  ''.
    _generate_extend 'K' 30 5  ''.
    _generate_extend '.' 35 1  ''.
    _generate_extend 'T' 36 28 ''''.
    _generate_extend '.' 64 1  ''.

    do_test( iv_line = lv_line iv_filename = '*.abap' ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering texts in curly brackets  *
******************************************************
  METHOD test_abap_05.

    DATA lv_line TYPE string.

    lv_line = 'a = |{ b }={ c }|.'.                         "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'T' 4  1.
    _generate_parse 'T' 5  1.
    _generate_parse 'T' 9  1.
    _generate_parse 'T' 11 1.
    _generate_parse 'K' 13 1.
    _generate_parse 'T' 15 1.
    _generate_parse 'T' 16 1.

    " Generate table with expected values after ordering
    _generate_order 'T' 4  1  '|'.
    _generate_order 'T' 10 1  '}'.
    _generate_order 'K' 13 1  ''.
    _generate_order 'T' 16 1  '}'.

    " Generate table with expected values after extending
    _generate_extend '.' 0  4  ''.
    _generate_extend 'T' 4  1  '|'.
    _generate_extend '.' 5  5  ''.
    _generate_extend 'T' 10 1  '}'.
    _generate_extend '.' 11 2  ''.
    _generate_extend 'K' 13 1  ''.
    _generate_extend '.' 14 2  ''.
    _generate_extend 'T' 16 1  '}'.
    _generate_extend '.' 17 1  ''.

    do_test( iv_line = lv_line iv_filename = '*.abap' ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering of texts                 *
******************************************************
  METHOD test_abap_06.

    DATA lv_line TYPE string.

    lv_line = 'lv_line = lc_constant && |XYZ { ''ab'' && |ac{ ''UU'' }| }|'. "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'K' 22 2.
    _generate_parse 'T' 25 1.
    _generate_parse 'T' 30 1.
    _generate_parse 'T' 32 1.
    _generate_parse 'T' 35 1.
    _generate_parse 'K' 37 2.
    _generate_parse 'T' 40 1.
    _generate_parse 'T' 43 1.
    _generate_parse 'T' 45 1.
    _generate_parse 'T' 48 1.
    _generate_parse 'T' 50 1.
    _generate_parse 'T' 51 1.
    _generate_parse 'T' 53 1.
    _generate_parse 'T' 54 1.

    " Generate table with expected values after ordering
    _generate_order 'K' 22 2  ''.
    _generate_order 'T' 25 5  '|'.
    _generate_order 'T' 32 4  ''''.
    _generate_order 'K' 37 2  ''.
    _generate_order 'T' 40 3  '|'.
    _generate_order 'T' 45 4  ''''.
    _generate_order 'T' 51 1  '}'.
    _generate_order 'T' 54 1  '}'.

    " Generate table with expected values after extending
    _generate_extend '.' 00 22 ''.
    _generate_extend 'K' 22 2  ''.
    _generate_extend '.' 24 1  ''.
    _generate_extend 'T' 25 5  '|'.
    _generate_extend '.' 30 2  ''.
    _generate_extend 'T' 32 4  ''''.
    _generate_extend '.' 36 1  ''.
    _generate_extend 'K' 37 2  ''.
    _generate_extend '.' 39 1  ''.
    _generate_extend 'T' 40 3  '|'.
    _generate_extend '.' 43 2  ''.
    _generate_extend 'T' 45 4  ''''.
    _generate_extend '.' 49 2  ''.
    _generate_extend 'T' 51 1  '}'.
    _generate_extend '.' 52 2  ''.
    _generate_extend 'T' 54 1  '}'.

    do_test( iv_line = lv_line iv_filename = '*.abap' ).

  ENDMETHOD.

********************************************************
* Check that '*' in select statement is not a match    *
********************************************************
  METHOD test_abap_07.

    DATA lv_line TYPE string.

    lv_line = 'SELECT * FROM foo'.                          "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'K' 0  6.
    _generate_parse 'K' 9  4.

    " Generate table with expected values after ordering
    _generate_order 'K' 0  6 ''.
    _generate_order 'K' 9  4 ''.

    " Generate table with expected values after extending
    _generate_extend 'K' 0  6 ''.
    _generate_extend '.' 6  3 ''.
    _generate_extend 'K' 9  4 ''.
    _generate_extend '.' 13 4 ''.

    do_test( iv_line = lv_line iv_filename = '*.abap' ).

  ENDMETHOD.

********************************************************
* Test parsing and ordering of key words in structures *
********************************************************
  METHOD test_abap_08.

    DATA lv_line TYPE string.

    lv_line = 'lv_length = <match>-length.'.                "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'K' 13 5.
    _generate_parse 'K' 20 6.

    " Generate table with expected values after extending
    _generate_extend '.' 0  27 ''.

    do_test( iv_line = lv_line iv_filename = '*.abap' ).

  ENDMETHOD.

********************************************************
* Test parsing and ordering of tags in xml             *
********************************************************
  METHOD test_xml_01.

    DATA lv_line TYPE string.

    lv_line = '<tag>Text</tag>'.                            "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'X' 0  1.
    _generate_parse 'X' 4  1.
    _generate_parse 'X' 9  1.
    _generate_parse 'X' 14 1.

    " Generate table with expected values after ordering
    _generate_order 'X' 0  5 '<'.
    _generate_order 'X' 9  6 '<'.

    " Generate table with expected values after extending
    _generate_extend 'X' 0  5 '<'.
    _generate_extend '.' 5  4 ''.
    _generate_extend 'X' 9  6 '<'.

    do_test( iv_line = lv_line iv_filename = '*.xml' ).

  ENDMETHOD.

  METHOD test_xml_02.

    DATA lv_line TYPE string.

    lv_line = '<tag/>'.                                     "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'X' 0  1.
    _generate_parse 'X' 5  1.

    " Generate table with expected values after ordering
    _generate_order 'X' 0  6 '<'.

    " Generate table with expected values after extending
    _generate_extend 'X' 0  6 '<'.

    do_test( iv_line = lv_line iv_filename = '*.xml' ).

  ENDMETHOD.

  METHOD test_xml_03.

    DATA lv_line TYPE string.

    lv_line = '<tag attribute="value"/>'.                   "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'X' 0  1.
    _generate_parse 'A' 4  10.
    _generate_parse 'V' 15 7.
    _generate_parse 'X' 23 1.

    " Generate table with expected values after ordering
    _generate_order 'X' 0  4 '<'.
    _generate_order 'A' 4  10 ''.
    _generate_order 'V' 15 7 ''.
    _generate_order 'X' 22 2 '>'.

    " Generate table with expected values after extending
    _generate_extend 'X' 0  4 '<'.
    _generate_extend 'A' 4  10 ''.
    _generate_extend '.' 14 1 ''.
    _generate_extend 'V' 15 7 ''.
    _generate_extend 'X' 22 2 '>'.

    do_test( iv_line = lv_line iv_filename = '*.xml' ).

  ENDMETHOD.

  METHOD test_xml_04.

    DATA lv_line TYPE string.

    lv_line = '<?xml version="1.0"?>'.                      "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'X' 0  1.
    _generate_parse 'A' 5  8.
    _generate_parse 'V' 14 5.
    _generate_parse 'X' 20 1.

    " Generate table with expected values after ordering
    _generate_order 'X' 0  5 '<'.
    _generate_order 'A' 5  8 ''.
    _generate_order 'V' 14 5 ''.
    _generate_order 'X' 19 2 '>'.

    " Generate table with expected values after extending
    _generate_extend 'X' 0  5 '<'.
    _generate_extend 'A' 5  8 ''.
    _generate_extend '.' 13 1 ''.
    _generate_extend 'V' 14 5 ''.
    _generate_extend 'X' 19 2 '>'.

    do_test( iv_line = lv_line iv_filename = '*.xml' ).

  ENDMETHOD.

  METHOD test_xml_05.

    DATA lv_line TYPE string.

    lv_line = '<ns:tag ns:a1="v1" ns:a2=''v2''>"text"</ns:tag>'. "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'X' 0  1.
    _generate_parse 'A' 7  6.
    _generate_parse 'V' 14 4.
    _generate_parse 'A' 18 6.
    _generate_parse 'V' 25 4.
    _generate_parse 'X' 29 1.
    _generate_parse 'V' 30 6.
    _generate_parse 'X' 36 1.
    _generate_parse 'X' 44 1.

    " Generate table with expected values after ordering
    _generate_order 'X' 0  7 '<'.
    _generate_order 'A' 7  6 ''.
    _generate_order 'V' 14 4 ''.
    _generate_order 'A' 18 6 ''.
    _generate_order 'V' 25 4 ''.
    _generate_order 'X' 29 1 '>'.
    _generate_order 'X' 36 9 '<'.

    " Generate table with expected values after extending
    _generate_extend 'X' 0  7 '<'.
    _generate_extend 'A' 7  6 ''.
    _generate_extend '.' 13 1 ''.
    _generate_extend 'V' 14 4 ''.
    _generate_extend 'A' 18 6 ''.
    _generate_extend '.' 24 1 ''.
    _generate_extend 'V' 25 4 ''.
    _generate_extend 'X' 29 1 '>'.
    _generate_extend '.' 30 6 ''.
    _generate_extend 'X' 36 9 '<'.

    do_test( iv_line = lv_line iv_filename = '*.xml' ).

  ENDMETHOD.

  METHOD test_xml_06.
    DATA lv_line TYPE string.

    "unclosed tag
    lv_line = '<ns:tag ns:a1="v1"'.                                     "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'X' 0  1.
    _generate_parse 'A' 7  6.
    _generate_parse 'V' 14 4.

    " Generate table with expected values after ordering
    _generate_order 'X' 0  7 '<'.
    _generate_order 'A' 7  6 ''.
    _generate_order 'V' 14 4 ''.

    " Generate table with expected values after extending
    _generate_extend 'X' 0  7 '<'.
    _generate_extend 'A' 7  6 ''.
    _generate_extend '.' 13 1 ''.
    _generate_extend 'V' 14 4 ''.

    do_test( iv_line = lv_line iv_filename = '*.xml' ).
  ENDMETHOD.

  METHOD test_xml_07.
    "invalid XML characters in a string
    DATA lv_line TYPE string.

    "xml special characters in attribute
    lv_line = '<tag attribute=" '' > "/>'.                   "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'X' 0  1.
    _generate_parse 'A' 4  10.
    _generate_parse 'V' 15 7.
    _generate_parse 'X' 23 1.

    " Generate table with expected values after ordering
    _generate_order 'X' 0  4 '<'.
    _generate_order 'A' 4  10 ''.
    _generate_order 'V' 15 7 ''.
    _generate_order 'X' 22 2 '>'.

    " Generate table with expected values after extending
    _generate_extend 'X' 0  4 '<'.
    _generate_extend 'A' 4  10 ''.
    _generate_extend '.' 14 1 ''.
    _generate_extend 'V' 15 7 ''.
    _generate_extend 'X' 22 2 '>'.

    do_test( iv_line = lv_line iv_filename = '*.xml' ).


  ENDMETHOD.


  METHOD test_xml_08.
    "invalid XML characters in a string
    DATA lv_line TYPE string.

    "attribute at beginning of line
    lv_line = 'attribute=''>" '''.                   "#EC NOTEXT

    " Generate table with expected values after parsing
    _generate_parse 'A' 0  9.
    _generate_parse 'V' 10 5.

    " Generate table with expected values after ordering
    _generate_order 'A' 0  9 ''.
    _generate_order 'V' 10 5 ''.

    " Generate table with expected values after extending
    _generate_extend 'A' 0  9 ''.
    _generate_extend '.' 9 1 ''.
    _generate_extend 'V' 10 5 ''.

    do_test( iv_line = lv_line iv_filename = '*.xml' ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_syntax_basic_logic DEFINITION DEFERRED.
CLASS zcl_abapgit_syntax_highlighter DEFINITION LOCAL FRIENDS ltcl_syntax_basic_logic.

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_basic_logic DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_syntax_basic_logic DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.

    DATA mo_syntax_highlighter TYPE REF TO zcl_abapgit_syntax_highlighter.

    METHODS:
      setup,
      process_line  FOR TESTING,
      format_line   FOR TESTING,
      apply_style   FOR TESTING.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_highlighter IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS ltcl_syntax_basic_logic IMPLEMENTATION.

  METHOD setup.
    mo_syntax_highlighter = zcl_abapgit_syntax_highlighter=>create( '*.abap' ).
  ENDMETHOD.

  METHOD format_line.

    DATA:
      lv_line     TYPE string,
      lv_line_act TYPE string,
      lv_line_exp TYPE string.

    lv_line = 'call function ''FM_NAME''. " Commented'.     "#EC NOTEXT

    lv_line_exp =
      '<span class="keyword">call</span>' &&                "#EC NOTEXT
      ' <span class="keyword">function</span>' &&           "#EC NOTEXT
      ' <span class="text">&#39;FM_NAME&#39;</span>.' &&    "#EC NOTEXT
      ' <span class="comment">&quot; Commented</span>'.     "#EC NOTEXT

    lv_line_act = mo_syntax_highlighter->process_line( lv_line ).

    cl_abap_unit_assert=>assert_equals( exp = lv_line_exp
                                        act = lv_line_act
                                        msg = | Error during formating: { lv_line }| ).

  ENDMETHOD.

  METHOD apply_style.

    DATA lv_line_act TYPE string.

    " Call the method and compare results
    lv_line_act = mo_syntax_highlighter->apply_style( iv_line  = 'CALL FUNCTION' "#EC NOTEXT
                                   iv_class = zcl_abapgit_syntax_abap=>c_css-keyword ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_line_act
      exp = '<span class="keyword">CALL FUNCTION</span>'    "#EC NOTEXT
      msg = 'Failure during applying of style.' ).          "#EC NOTEXT

  ENDMETHOD.

  METHOD process_line.

    DATA lv_line_act TYPE string.

    " Call the method with empty parameter and compare results
    lv_line_act = mo_syntax_highlighter->process_line( '' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_line_act
      exp = ''
      msg = 'Failure in method process_line.' ).            "#EC NOTEXT

    " Call the method with non-empty line and compare results
    lv_line_act = mo_syntax_highlighter->process_line( '* CALL FUNCTION' ). "#EC NOTEXT

    cl_abap_unit_assert=>assert_equals(
      act = lv_line_act
      exp = '<span class="comment">* CALL FUNCTION</span>'  "#EC NOTEXT
      msg = 'Failure in method process_line.' ).            "#EC NOTEXT

  ENDMETHOD.

ENDCLASS.
