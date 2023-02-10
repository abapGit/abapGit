CLASS ltcl_abapgit_syntax_xml DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_syntax_xml.

    METHODS:
      setup,
      sole_closing_xml_tag FOR TESTING RAISING cx_static_check,
      complete_xml_tag FOR TESTING RAISING cx_static_check,
      complete_xml_tag_with_closing FOR TESTING RAISING cx_static_check,
      empty_attributes FOR TESTING RAISING cx_static_check,
      open_tags FOR TESTING RAISING cx_static_check,
      attributes_only FOR TESTING RAISING cx_static_check,
      attribute_value_equal_signs FOR TESTING RAISING cx_static_check,
      multi_line_comments FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_abapgit_syntax_xml IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_cut.

  ENDMETHOD.

  METHOD sole_closing_xml_tag.

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="xml_tag">&gt;</span>|
      act = mo_cut->process_line( |>| ) ).

  ENDMETHOD.

  METHOD complete_xml_tag.

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="xml_tag">&lt;tag&gt;</span>|
      act = mo_cut->process_line( |<tag>| ) ).

  ENDMETHOD.

  METHOD complete_xml_tag_with_closing.

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="xml_tag">&lt;tag/&gt;</span>|
      act = mo_cut->process_line( |<tag/>| ) ).

  ENDMETHOD.

  METHOD empty_attributes.

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="xml_tag">&lt;ECTD</span>|
         && |<span class="attr"> SAPRL</span>=|
         && |<span class="attr_val">"751"</span>|
         && |<span class="attr"> VERSION</span>=|
         && |<span class="attr_val">"1.5"</span>|
         && |<span class="attr"> DOWNLOADDATE</span>=<span class="attr_val">""</span>|
         && |<span class="attr"> DOWNLOADTIME</span>=<span class="attr_val">""</span>|
         && |<span class="xml_tag">&gt;</span>|
      act = mo_cut->process_line( |<ECTD SAPRL="751" VERSION="1.5" DOWNLOADDATE="" DOWNLOADTIME="">| ) ).

  ENDMETHOD.

  METHOD attributes_only.

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="attr"> SAPRL</span>=|
         && |<span class="attr_val">"751"</span>|
         && |<span class="attr"> VERSION</span>=|
         && |<span class="attr_val">"&gt;1.5"</span>|
      act = mo_cut->process_line( | SAPRL="751" VERSION=">1.5"| ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="attr">SAPRL</span>=|
         && |<span class="attr_val">"751"</span>|
         && |<span class="attr"> VERSION</span>=|
         && |<span class="attr_val">'&gt;1.5'</span>|
      act = mo_cut->process_line( |SAPRL="751" VERSION='>1.5'| ) ).

  ENDMETHOD.

  METHOD open_tags.

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="xml_tag">&lt;ECTD</span>|
      act = mo_cut->process_line( |<ECTD| ) ).

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="xml_tag">&lt;ECTD</span>|
         && |<span class="attr"> SAPRL</span>=|
         && |<span class="attr_val">"751"</span>|
         && |<span class="attr"> VERSION</span>=|
         && |<span class="attr_val">"1.5"</span>|
      act = mo_cut->process_line( |<ECTD SAPRL="751" VERSION="1.5"| ) ).

  ENDMETHOD.

  METHOD attribute_value_equal_signs.

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="xml_tag">&lt;meta</span>|
         && |<span class="attr"> name</span>=|
         && |<span class="attr_val">"viewport"</span>|
         && |<span class="attr"> content</span>=|
         && |<span class="attr_val">"width=device, initial=1.0, maximum=1.0"</span>|
         && |<span class="xml_tag">&gt;</span>|
      act = mo_cut->process_line( |<meta name="viewport" content="width=device, initial=1.0, maximum=1.0">| ) ).

  ENDMETHOD.

  METHOD multi_line_comments.

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="comment">&lt;!-- comment</span>|
      act = mo_cut->process_line( |<!-- comment| ) ).

    " New instance (i.e. different file)
    CREATE OBJECT mo_cut.

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="xml_tag">&lt;tag&gt;</span>|
      act = mo_cut->process_line( |<tag>| ) ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_syntax_cases DEFINITION DEFERRED.
CLASS zcl_abapgit_syntax_xml DEFINITION LOCAL FRIENDS ltcl_syntax_cases.

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_cases definition
*----------------------------------------------------------------------*
CLASS ltcl_syntax_cases DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.

    DATA:
      mt_after_parse  TYPE zcl_abapgit_syntax_xml=>ty_match_tt,
      mt_after_order  TYPE zcl_abapgit_syntax_xml=>ty_match_tt,
      mt_after_extend TYPE zcl_abapgit_syntax_xml=>ty_match_tt.

    METHODS:
      do_test IMPORTING iv_line TYPE string,
      generate_parse IMPORTING iv_token  TYPE c
                               iv_offset TYPE i
                               iv_length TYPE i,
      generate_order IMPORTING iv_token    TYPE c
                               iv_offset   TYPE i
                               iv_length   TYPE i
                               iv_text_tag TYPE string,
      generate_extend IMPORTING iv_token    TYPE c
                                iv_offset   TYPE i
                                iv_length   TYPE i
                                iv_text_tag TYPE string,
      test_xml_01 FOR TESTING,
      test_xml_02 FOR TESTING,
      test_xml_03 FOR TESTING,
      test_xml_04 FOR TESTING,
      test_xml_05 FOR TESTING,
      test_xml_06 FOR TESTING,
      test_xml_07 FOR TESTING,
      test_xml_08 FOR TESTING,
      test_xml_09 FOR TESTING.

ENDCLASS.
*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_cases IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS ltcl_syntax_cases IMPLEMENTATION.

  METHOD do_test.

    DATA: lt_matches_act TYPE zcl_abapgit_syntax_xml=>ty_match_tt,
          ls_match       LIKE LINE OF lt_matches_act,
          lv_offs        TYPE i,
          lo_syntax      TYPE REF TO zcl_abapgit_syntax_xml.


    CREATE OBJECT lo_syntax.
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

    " Check consistency
    lv_offs = 0.
    LOOP AT lt_matches_act INTO ls_match.
      IF ls_match-offset <> lv_offs.
        cl_abap_unit_assert=>assert_equals( exp = lv_offs
                                            act = ls_match-offset
                                            msg = | Error during consistency check: { sy-tabix }| ).
      ENDIF.
      lv_offs = lv_offs + ls_match-length.
    ENDLOOP.

  ENDMETHOD.

  METHOD generate_parse.
    DATA ls_match TYPE zcl_abapgit_syntax_xml=>ty_match.

    ls_match-token    = iv_token.
    ls_match-offset   = iv_offset.
    ls_match-length   = iv_length.
    APPEND ls_match TO mt_after_parse.
  ENDMETHOD.

  METHOD generate_order.
    DATA ls_match TYPE zcl_abapgit_syntax_xml=>ty_match.

    ls_match-token    = iv_token.
    ls_match-offset   = iv_offset.
    ls_match-length   = iv_length.
    ls_match-text_tag = iv_text_tag.
    APPEND ls_match TO mt_after_order.
  ENDMETHOD.

  METHOD generate_extend.
    DATA ls_match TYPE zcl_abapgit_syntax_xml=>ty_match.

    ls_match-token    = iv_token.
    ls_match-offset   = iv_offset.
    ls_match-length   = iv_length.
    ls_match-text_tag = iv_text_tag.
    APPEND ls_match TO mt_after_extend.
  ENDMETHOD.

********************************************************
* Test parsing and ordering of tags in xml             *
********************************************************
  METHOD test_xml_01.

    DATA lv_line TYPE string.

    lv_line = '<tag>Text</tag>'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'X'
                    iv_offset = 0
                    iv_length = 1 ).
    generate_parse( iv_token  = 'X'
                    iv_offset = 4
                    iv_length = 1 ).
    generate_parse( iv_token  = 'X'
                    iv_offset = 9
                    iv_length = 1 ).
    generate_parse( iv_token  = 'X'
                    iv_offset = 14
                    iv_length = 1 ).

    " Generate table with expected values after ordering

    generate_order( iv_token    = 'X'
                    iv_offset   = 0
                    iv_length   = 5
                    iv_text_tag = '<' ).
    generate_order( iv_token    = 'X'
                    iv_offset   = 9
                    iv_length   = 6
                    iv_text_tag = '<' ).

    " Generate table with expected values after extending
    generate_extend( iv_token    = 'X'
                     iv_offset   = 0
                     iv_length   = 5
                     iv_text_tag = '<' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 5
                     iv_length   = 4
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'X'
                     iv_offset   = 9
                     iv_length   = 6
                     iv_text_tag = '<' ).

    do_test( lv_line ).

  ENDMETHOD.

  METHOD test_xml_02.

    DATA lv_line TYPE string.

    lv_line = '<tag/>'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'X'
                    iv_offset = 0
                    iv_length = 1 ).
    generate_parse( iv_token  = 'X'
                    iv_offset = 5
                    iv_length = 1 ).

    " Generate table with expected values after ordering
    generate_order( iv_token    = 'X'
                    iv_offset   = 0
                    iv_length   = 6
                    iv_text_tag = '<' ).
    " Generate table with expected values after extending
    generate_extend( iv_token    = 'X'
                     iv_offset   = 0
                     iv_length   = 6
                     iv_text_tag = '<' ).

    do_test( lv_line ).

  ENDMETHOD.

  METHOD test_xml_03.

    DATA lv_line TYPE string.

    lv_line = '<tag attribute="value"/>'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'X'
                    iv_offset = 0
                    iv_length = 1 ).
    generate_parse( iv_token  = 'A'
                    iv_offset = 4
                    iv_length = 10 ).
    generate_parse( iv_token  = 'V'
                    iv_offset = 15
                    iv_length = 7 ).
    generate_parse( iv_token  = 'X'
                    iv_offset = 23
                    iv_length = 1 ).

    " Generate table with expected values after ordering
    generate_order( iv_token    = 'X'
                    iv_offset   = 0
                    iv_length   = 4
                    iv_text_tag = '<' ).
    generate_order( iv_token    = 'A'
                    iv_offset   = 4
                    iv_length   = 10
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'V'
                    iv_offset   = 15
                    iv_length   = 7
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'X'
                    iv_offset   = 22
                    iv_length   = 2
                    iv_text_tag = '>' ).

    " Generate table with expected values after extending
    generate_extend( iv_token    = 'X'
                     iv_offset   = 0
                     iv_length   = 4
                     iv_text_tag = '<' ).
    generate_extend( iv_token    = 'A'
                     iv_offset   = 4
                     iv_length   = 10
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 14
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'V'
                     iv_offset   = 15
                     iv_length   = 7
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'X'
                     iv_offset   = 22
                     iv_length   = 2
                     iv_text_tag = '>' ).

    do_test( lv_line ).

  ENDMETHOD.

  METHOD test_xml_04.

    DATA lv_line TYPE string.

    lv_line = '<?xml version="1.0"?>'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'X'
                    iv_offset = 0
                    iv_length = 1 ).
    generate_parse( iv_token  = 'A'
                    iv_offset = 5
                    iv_length = 8 ).
    generate_parse( iv_token  = 'V'
                    iv_offset = 14
                    iv_length = 5 ).
    generate_parse( iv_token  = 'X'
                    iv_offset = 20
                    iv_length = 1 ).

    " Generate table with expected values after ordering
    generate_order( iv_token    = 'X'
                    iv_offset   = 0
                    iv_length   = 5
                    iv_text_tag = '<' ).
    generate_order( iv_token    = 'A'
                    iv_offset   = 5
                    iv_length   = 8
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'V'
                    iv_offset   = 14
                    iv_length   = 5
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'X'
                    iv_offset   = 19
                    iv_length   = 2
                    iv_text_tag = '>' ).

    " Generate table with expected values after extending
    generate_extend( iv_token    = 'X'
                     iv_offset   = 0
                     iv_length   = 5
                     iv_text_tag = '<' ).
    generate_extend( iv_token    = 'A'
                     iv_offset   = 5
                     iv_length   = 8
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 13
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'V'
                     iv_offset   = 14
                     iv_length   = 5
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'X'
                     iv_offset   = 19
                     iv_length   = 2
                     iv_text_tag = '>' ).

    do_test( lv_line ).

  ENDMETHOD.

  METHOD test_xml_05.

    DATA lv_line TYPE string.

    lv_line = '<ns:tag ns:a1="v1" ns:a2=''v2''>"text"</ns:tag>'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'X'
                    iv_offset = 0
                    iv_length = 1 ).
    generate_parse( iv_token  = 'A'
                    iv_offset = 7
                    iv_length = 6 ).
    generate_parse( iv_token  = 'V'
                    iv_offset = 14
                    iv_length = 4 ).
    generate_parse( iv_token  = 'A'
                    iv_offset = 18
                    iv_length = 6 ).
    generate_parse( iv_token  = 'V'
                    iv_offset = 25
                    iv_length = 4 ).
    generate_parse( iv_token  = 'X'
                    iv_offset = 29
                    iv_length = 1 ).
    generate_parse( iv_token  = 'V'
                    iv_offset = 30
                    iv_length = 6 ).
    generate_parse( iv_token  = 'X'
                    iv_offset = 36
                    iv_length = 1 ).
    generate_parse( iv_token  = 'X'
                    iv_offset = 44
                    iv_length = 1 ).

    " Generate table with expected values after ordering
    generate_order( iv_token    = 'X'
                    iv_offset   = 0
                    iv_length   = 7
                    iv_text_tag = '<' ).
    generate_order( iv_token    = 'A'
                    iv_offset   = 7
                    iv_length   = 6
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'V'
                    iv_offset   = 14
                    iv_length   = 4
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'A'
                    iv_offset   = 18
                    iv_length   = 6
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'V'
                    iv_offset   = 25
                    iv_length   = 4
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'X'
                    iv_offset   = 29
                    iv_length   = 1
                    iv_text_tag = '>' ).
    generate_order( iv_token    = 'X'
                    iv_offset   = 36
                    iv_length   = 9
                    iv_text_tag = '<' ).

    " Generate table with expected values after extending
    generate_extend( iv_token    = 'X'
                     iv_offset   = 0
                     iv_length   = 7
                     iv_text_tag = '<' ).
    generate_extend( iv_token    = 'A'
                     iv_offset   = 7
                     iv_length   = 6
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 13
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'V'
                     iv_offset   = 14
                     iv_length   = 4
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'A'
                     iv_offset   = 18
                     iv_length   = 6
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 24
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'V'
                     iv_offset   = 25
                     iv_length   = 4
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'X'
                     iv_offset   = 29
                     iv_length   = 1
                     iv_text_tag = '>' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 30
                     iv_length   = 6
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'X'
                     iv_offset   = 36
                     iv_length   = 9
                     iv_text_tag = '<' ).

    do_test( lv_line ).

  ENDMETHOD.

  METHOD test_xml_06.
    DATA lv_line TYPE string.

    "unclosed tag
    lv_line = '<ns:tag ns:a1="v1"'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'X'
                    iv_offset = 0
                    iv_length = 1 ).
    generate_parse( iv_token  = 'A'
                    iv_offset = 7
                    iv_length = 6 ).
    generate_parse( iv_token  = 'V'
                    iv_offset = 14
                    iv_length = 4 ).

    " Generate table with expected values after ordering
    generate_order( iv_token    = 'X'
                    iv_offset   = 0
                    iv_length   = 7
                    iv_text_tag = '<' ).
    generate_order( iv_token    = 'A'
                    iv_offset   = 7
                    iv_length   = 6
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'V'
                    iv_offset   = 14
                    iv_length   = 4
                    iv_text_tag = '' ).

    " Generate table with expected values after extending
    generate_extend( iv_token    = 'X'
                     iv_offset   = 0
                     iv_length   = 7
                     iv_text_tag = '<' ).
    generate_extend( iv_token    = 'A'
                     iv_offset   = 7
                     iv_length   = 6
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 13
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'V'
                     iv_offset   = 14
                     iv_length   = 4
                     iv_text_tag = '' ).

    do_test( lv_line ).

  ENDMETHOD.

  METHOD test_xml_07.
    "invalid XML characters in a string
    DATA lv_line TYPE string.

    "xml special characters in attribute
    lv_line = '<tag attribute=" '' > "/>'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'X'
                    iv_offset = 0
                    iv_length = 1 ).
    generate_parse( iv_token  = 'A'
                    iv_offset = 4
                    iv_length = 10 ).
    generate_parse( iv_token  = 'V'
                    iv_offset = 15
                    iv_length = 7 ).
    generate_parse( iv_token  = 'X'
                    iv_offset = 23
                    iv_length = 1 ).

    " Generate table with expected values after ordering
    generate_order( iv_token    = 'X'
                    iv_offset   = 0
                    iv_length   = 4
                    iv_text_tag = '<' ).
    generate_order( iv_token    = 'A'
                    iv_offset   = 4
                    iv_length   = 10
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'V'
                    iv_offset   = 15
                    iv_length   = 7
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'X'
                    iv_offset   = 22
                    iv_length   = 2
                    iv_text_tag = '>' ).

    " Generate table with expected values after extending
    generate_extend( iv_token    = 'X'
                     iv_offset   = 0
                     iv_length   = 4
                     iv_text_tag = '<' ).
    generate_extend( iv_token    = 'A'
                     iv_offset   = 4
                     iv_length   = 10
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 14
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'V'
                     iv_offset   = 15
                     iv_length   = 7
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'X'
                     iv_offset   = 22
                     iv_length   = 2
                     iv_text_tag = '>' ).

    do_test( lv_line ).

  ENDMETHOD.

  METHOD test_xml_08.
    "invalid XML characters in a string
    DATA lv_line TYPE string.

    "attribute at beginning of line
    lv_line = 'attribute=''>" '''.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'A'
                    iv_offset = 0
                    iv_length = 9 ).
    generate_parse( iv_token  = 'V'
                    iv_offset = 10
                    iv_length = 5 ).

    " Generate table with expected values after ordering
    generate_order( iv_token    = 'A'
                    iv_offset   = 0
                    iv_length   = 9
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'V'
                    iv_offset   = 10
                    iv_length   = 5
                    iv_text_tag = '' ).

    " Generate table with expected values after extending
    generate_extend( iv_token    = 'A'
                     iv_offset   = 0
                     iv_length   = 9
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 9
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'V'
                     iv_offset   = 10
                     iv_length   = 5
                     iv_text_tag = '' ).

    do_test( lv_line ).

  ENDMETHOD.

  METHOD test_xml_09.
    "back quotes used for attribute values (HTML)
    DATA lv_line TYPE string.

    lv_line = '<tag attribute=`value`/>'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'X'
                    iv_offset = 0
                    iv_length = 1 ).
    generate_parse( iv_token  = 'A'
                    iv_offset = 4
                    iv_length = 10 ).
    generate_parse( iv_token  = 'V'
                    iv_offset = 15
                    iv_length = 7 ).
    generate_parse( iv_token  = 'X'
                    iv_offset = 23
                    iv_length = 1 ).

    " Generate table with expected values after ordering
    generate_order( iv_token    = 'X'
                    iv_offset   = 0
                    iv_length   = 4
                    iv_text_tag = '<' ).
    generate_order( iv_token    = 'A'
                    iv_offset   = 4
                    iv_length   = 10
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'V'
                    iv_offset   = 15
                    iv_length   = 7
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'X'
                    iv_offset   = 22
                    iv_length   = 2
                    iv_text_tag = '>' ).

    " Generate table with expected values after extending
    generate_extend( iv_token    = 'X'
                     iv_offset   = 0
                     iv_length   = 4
                     iv_text_tag = '<' ).
    generate_extend( iv_token    = 'A'
                     iv_offset   = 4
                     iv_length   = 10
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 14
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'V'
                     iv_offset   = 15
                     iv_length   = 7
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'X'
                     iv_offset   = 22
                     iv_length   = 2
                     iv_text_tag = '>' ).

    do_test( lv_line ).

  ENDMETHOD.

ENDCLASS.
