CLASS ltcl_abapgit_syntax_abap DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
     mo_cut TYPE REF TO zcl_abapgit_syntax_abap.

    METHODS:
      setup,
      report_header FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_abapgit_syntax_abap IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_cut.

  ENDMETHOD.

  METHOD report_header.

    DATA lv_act TYPE string.

    lv_act = mo_cut->process_line( |REPORT zfoo.| ).

    cl_abap_unit_assert=>assert_equals(
      exp = |<span class="keyword">REPORT</span> zfoo.|
      act = lv_act ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_syntax_basic_logic DEFINITION DEFERRED.
CLASS zcl_abapgit_syntax_abap DEFINITION LOCAL FRIENDS ltcl_syntax_basic_logic.

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_basic_logic DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_syntax_basic_logic DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.

    DATA mo_syntax_highlighter TYPE REF TO zcl_abapgit_syntax_abap.

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
    CREATE OBJECT mo_syntax_highlighter.
  ENDMETHOD.

  METHOD format_line.

    DATA:
      lv_line     TYPE string,
      lv_line_act TYPE string,
      lv_line_exp TYPE string.

    lv_line = 'call function ''FM_NAME''. " Commented'.

    lv_line_exp =
      '<span class="keyword">call</span>' &&
      ' <span class="keyword">function</span>' &&
      | <span class="text">'FM_NAME'</span>.| &&
      ' <span class="comment">" Commented</span>'.

    lv_line_act = mo_syntax_highlighter->process_line( lv_line ).

    cl_abap_unit_assert=>assert_equals( exp = lv_line_exp
                                        act = lv_line_act
                                        msg = |Error during formating: { lv_line }| ).

  ENDMETHOD.

  METHOD apply_style.

    DATA lv_line_act TYPE string.

    " Call the method and compare results
    lv_line_act = mo_syntax_highlighter->apply_style(
      iv_line  = 'CALL FUNCTION'
      iv_class = zcl_abapgit_syntax_abap=>c_css-keyword ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_line_act
      exp = '<span class="keyword">CALL FUNCTION</span>'
      msg = 'Failure during applying of style.' ).

  ENDMETHOD.

  METHOD process_line.

    DATA lv_line_act TYPE string.

    " Call the method with empty parameter and compare results
    lv_line_act = mo_syntax_highlighter->process_line( '' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_line_act
      exp = ''
      msg = 'Failure in method process_line.' ).

    " Call the method with non-empty line and compare results
    lv_line_act = mo_syntax_highlighter->process_line( '* CALL FUNCTION' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_line_act
      exp = '<span class="comment">* CALL FUNCTION</span>'
      msg = 'Failure in method process_line.' ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_syntax_cases DEFINITION DEFERRED.
CLASS zcl_abapgit_syntax_abap DEFINITION LOCAL FRIENDS ltcl_syntax_cases.

*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_cases definition
*----------------------------------------------------------------------*
CLASS ltcl_syntax_cases DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS
    DURATION SHORT.

  PRIVATE SECTION.

    DATA:
      mt_after_parse  TYPE zcl_abapgit_syntax_abap=>ty_match_tt,
      mt_after_order  TYPE zcl_abapgit_syntax_abap=>ty_match_tt,
      mt_after_extend TYPE zcl_abapgit_syntax_abap=>ty_match_tt.

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
      test_abap_01 FOR TESTING,
      test_abap_02 FOR TESTING,
      test_abap_03 FOR TESTING,
      test_abap_04 FOR TESTING,
      test_abap_05 FOR TESTING,
      test_abap_06 FOR TESTING,
      test_abap_07 FOR TESTING,
      test_abap_08 FOR TESTING.

ENDCLASS.
*----------------------------------------------------------------------*
*       CLASS ltcl_syntax_cases IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS ltcl_syntax_cases IMPLEMENTATION.

  METHOD do_test.

    DATA: lt_matches_act TYPE zcl_abapgit_syntax_abap=>ty_match_tt,
          ls_match       LIKE LINE OF lt_matches_act,
          lv_offs        TYPE i,
          lo_syntax      TYPE REF TO zcl_abapgit_syntax_abap.


    CREATE OBJECT lo_syntax.
    lt_matches_act = lo_syntax->parse_line( iv_line ).

    SORT lt_matches_act BY offset.

    cl_abap_unit_assert=>assert_equals( exp = mt_after_parse
                                        act = lt_matches_act
                                        msg = |Error during parsing: { iv_line }| ).

    lo_syntax->order_matches( EXPORTING iv_line    = iv_line
                       CHANGING  ct_matches = lt_matches_act ).

    cl_abap_unit_assert=>assert_equals( exp = mt_after_order
                                        act = lt_matches_act
                                        msg = |Error during ordering: { iv_line }| ).

    lo_syntax->extend_matches(
      EXPORTING
        iv_line    = iv_line
      CHANGING
        ct_matches = lt_matches_act ).

    cl_abap_unit_assert=>assert_equals( exp = mt_after_extend
                                        act = lt_matches_act
                                        msg = |Error during extending: { iv_line }| ).

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
    DATA ls_match TYPE zcl_abapgit_syntax_abap=>ty_match.

    ls_match-token    = iv_token.
    ls_match-offset   = iv_offset.
    ls_match-length   = iv_length.
    APPEND ls_match TO mt_after_parse.
  ENDMETHOD.

  METHOD generate_order.
    DATA ls_match TYPE zcl_abapgit_syntax_abap=>ty_match.

    ls_match-token    = iv_token.
    ls_match-offset   = iv_offset.
    ls_match-length   = iv_length.
    ls_match-text_tag = iv_text_tag.
    APPEND ls_match TO mt_after_order.
  ENDMETHOD.

  METHOD generate_extend.
    DATA ls_match TYPE zcl_abapgit_syntax_abap=>ty_match.

    ls_match-token    = iv_token.
    ls_match-offset   = iv_offset.
    ls_match-length   = iv_length.
    ls_match-text_tag = iv_text_tag.
    APPEND ls_match TO mt_after_extend.
  ENDMETHOD.

******************************************************
* Test parsing and ordering of comments              *
******************************************************
  METHOD test_abap_01.

    DATA lv_line TYPE string.

    lv_line = '* commented out line with key word data'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'C'
                    iv_offset = 0
                    iv_length = 1 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 12
                    iv_length = 3 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 16
                    iv_length = 4 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 21
                    iv_length = 4 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 26
                    iv_length = 3 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 30
                    iv_length = 4 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 35
                    iv_length = 4 ).


    " Generate table with expected values after ordering
    generate_order( iv_token    = 'C'
                    iv_offset   = 0
                    iv_length   = 39
                    iv_text_tag = '' ).

    " Generate table with expected values after ordering
    generate_extend( iv_token    = 'C'
                     iv_offset   = 0
                     iv_length   = 39
                     iv_text_tag = '' ).

    do_test( lv_line ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering of remainder of string   *
******************************************************
  METHOD test_abap_02.

    DATA lv_line TYPE string.

    lv_line = 'data: lv_var_name type string.'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'K'
                    iv_offset = 0
                    iv_length = 4 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 18
                    iv_length = 4 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 23
                    iv_length = 6 ).

    " Generate table with expected values after ordering
    generate_order( iv_token    = 'K'
                    iv_offset   = 0
                    iv_length   = 4
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'K'
                    iv_offset   = 18
                    iv_length   = 4
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'K'
                    iv_offset   = 23
                    iv_length   = 6
                    iv_text_tag = '' ).

    " Generate table with expected values after extending
    generate_extend( iv_token    = 'K'
                     iv_offset   = 0
                     iv_length   = 4
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 4
                     iv_length   = 14
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'K'
                     iv_offset   = 18
                     iv_length   = 4
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 22
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'K'
                     iv_offset   = 23
                     iv_length   = 6
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 29
                     iv_length   = 1
                     iv_text_tag = '' ).

    do_test( lv_line ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering of key words & texts     *
******************************************************
  METHOD test_abap_03.

    DATA lv_line TYPE string.


    lv_line = 'call function ''FM_NAME''. " Commented'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'K'
                    iv_offset = 0
                    iv_length = 4 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 5
                    iv_length = 8 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 14
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 22
                    iv_length = 1 ).
    generate_parse( iv_token  = 'C'
                    iv_offset = 25
                    iv_length = 1 ).

    " Generate table with expected values after ordering
    generate_order( iv_token    = 'K'
                    iv_offset   = 0
                    iv_length   = 4
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'K'
                    iv_offset   = 5
                    iv_length   = 8
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'T'
                    iv_offset   = 14
                    iv_length   = 9
                    iv_text_tag = '''' ).
    generate_order( iv_token    = 'C'
                    iv_offset   = 25
                    iv_length   = 11
                    iv_text_tag = '' ).

    " Generate table with expected values after extending
    generate_extend( iv_token    = 'K'
                     iv_offset   = 0
                     iv_length   = 4
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 4
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'K'
                     iv_offset   = 5
                     iv_length   = 8
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 13
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'T'
                     iv_offset   = 14
                     iv_length   = 9
                     iv_text_tag = '''' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 23
                     iv_length   = 2
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'C'
                     iv_offset   = 25
                     iv_length   = 11
                     iv_text_tag = '' ).

    do_test( lv_line ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering of key words in texts    *
******************************************************
  METHOD test_abap_04.

    DATA lv_line TYPE string.

    lv_line = 'constants: lc_var type string value ''simpletext data simpletext''.'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'K'
                    iv_offset = 0
                    iv_length = 9 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 18
                    iv_length = 4 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 23
                    iv_length = 6 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 30
                    iv_length = 5 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 36
                    iv_length = 1 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 48
                    iv_length = 4 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 63
                    iv_length = 1 ).

    " Generate table with expected values after ordering
    generate_order( iv_token    = 'K'
                    iv_offset   = 0
                    iv_length   = 9
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'K'
                    iv_offset   = 18
                    iv_length   = 4
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'K'
                    iv_offset   = 23
                    iv_length   = 6
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'K'
                    iv_offset   = 30
                    iv_length   = 5
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'T'
                    iv_offset   = 36
                    iv_length   = 28
                    iv_text_tag = '''' ).

    " Generate table with expected values after ordering
    generate_extend( iv_token    = 'K'
                     iv_offset   = 0
                     iv_length   = 9
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 9
                     iv_length   = 9
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'K'
                     iv_offset   = 18
                     iv_length   = 4
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 22
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'K'
                     iv_offset   = 23
                     iv_length   = 6
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 29
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'K'
                     iv_offset   = 30
                     iv_length   = 5
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 35
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'T'
                     iv_offset   = 36
                     iv_length   = 28
                     iv_text_tag = '''' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 64
                     iv_length   = 1
                     iv_text_tag = '' ).

    do_test( lv_line ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering texts in curly brackets  *
******************************************************
  METHOD test_abap_05.

    DATA lv_line TYPE string.

    lv_line = 'a = |{ b }={ c }|.'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'T'
                    iv_offset = 4
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 5
                    iv_length = 1 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 7
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 9
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 11
                    iv_length = 1 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 13
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 15
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 16
                    iv_length = 1 ).

    " Generate table with expected values after ordering
    generate_order( iv_token    = 'T'
                    iv_offset   = 4
                    iv_length   = 1
                    iv_text_tag = '|' ).
    generate_order( iv_token    = 'K'
                    iv_offset   = 7
                    iv_length   = 1
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'T'
                    iv_offset   = 10
                    iv_length   = 1
                    iv_text_tag = '}' ).
    generate_order( iv_token    = 'K'
                    iv_offset   = 13
                    iv_length   = 1
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'T'
                    iv_offset   = 16
                    iv_length   = 1
                    iv_text_tag = '}' ).

    " Generate table with expected values after extending
    generate_extend( iv_token    = '.'
                     iv_offset   = 0
                     iv_length   = 4
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'T'
                     iv_offset   = 4
                     iv_length   = 1
                     iv_text_tag = '|' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 5
                     iv_length   = 2
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'K'
                     iv_offset   = 7
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 8
                     iv_length   = 2
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'T'
                     iv_offset   = 10
                     iv_length   = 1
                     iv_text_tag = '}' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 11
                     iv_length   = 2
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'K'
                     iv_offset   = 13
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 14
                     iv_length   = 2
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'T'
                     iv_offset   = 16
                     iv_length   = 1
                     iv_text_tag = '}' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 17
                     iv_length   = 1
                     iv_text_tag = '' ).

    do_test( lv_line ).

  ENDMETHOD.

******************************************************
* Test parsing and ordering of texts                 *
******************************************************
  METHOD test_abap_06.

    DATA lv_line TYPE string.

    lv_line = 'lv_line = lc_constant && |XYZ { ''ab'' && |ac{ ''UU'' }| }|'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'K'
                    iv_offset = 22
                    iv_length = 2 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 25
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 30
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 32
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 35
                    iv_length = 1 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 37
                    iv_length = 2 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 40
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 43
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 45
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 48
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 50
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 51
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 53
                    iv_length = 1 ).
    generate_parse( iv_token  = 'T'
                    iv_offset = 54
                    iv_length = 1 ).

    " Generate table with expected values after ordering
    generate_order( iv_token    = 'K'
                    iv_offset   = 22
                    iv_length   = 2
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'T'
                    iv_offset   = 25
                    iv_length   = 5
                    iv_text_tag = '|' ).
    generate_order( iv_token    = 'T'
                    iv_offset   = 32
                    iv_length   = 4
                    iv_text_tag = '''' ).
    generate_order( iv_token    = 'K'
                    iv_offset   = 37
                    iv_length   = 2
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'T'
                    iv_offset   = 40
                    iv_length   = 3
                    iv_text_tag = '|' ).
    generate_order( iv_token    = 'T'
                    iv_offset   = 45
                    iv_length   = 4
                    iv_text_tag = '''' ).
    generate_order( iv_token    = 'T'
                    iv_offset   = 51
                    iv_length   = 1
                    iv_text_tag = '}' ).
    generate_order( iv_token    = 'T'
                    iv_offset   = 54
                    iv_length   = 1
                    iv_text_tag = '}' ).

    " Generate table with expected values after extending
    generate_extend( iv_token    = '.'
                     iv_offset   = 0
                     iv_length   = 22
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'K'
                     iv_offset   = 22
                     iv_length   = 2
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 24
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'T'
                     iv_offset   = 25
                     iv_length   = 5
                     iv_text_tag = '|' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 30
                     iv_length   = 2
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'T'
                     iv_offset   = 32
                     iv_length   = 4
                     iv_text_tag = '''' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 36
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'K'
                     iv_offset   = 37
                     iv_length   = 2
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 39
                     iv_length   = 1
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'T'
                     iv_offset   = 40
                     iv_length   = 3
                     iv_text_tag = '|' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 43
                     iv_length   = 2
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'T'
                     iv_offset   = 45
                     iv_length   = 4
                     iv_text_tag = '''' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 49
                     iv_length   = 2
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'T'
                     iv_offset   = 51
                     iv_length   = 1
                     iv_text_tag = '}' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 52
                     iv_length   = 2
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'T'
                     iv_offset   = 54
                     iv_length   = 1
                     iv_text_tag = '}' ).

    do_test( lv_line ).

  ENDMETHOD.

********************************************************
* Check that '*' in select statement is not a match    *
********************************************************
  METHOD test_abap_07.

    DATA lv_line TYPE string.

    lv_line = 'SELECT * FROM foo'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'K'
                    iv_offset = 0
                    iv_length = 6 ).

    generate_parse( iv_token  = 'K'
                    iv_offset = 9
                    iv_length = 4 ).

    " Generate table with expected values after ordering
    generate_order( iv_token    = 'K'
                    iv_offset   = 0
                    iv_length   = 6
                    iv_text_tag = '' ).
    generate_order( iv_token    = 'K'
                    iv_offset   = 9
                    iv_length   = 4
                    iv_text_tag = '' ).

    " Generate table with expected values after extending
    generate_extend( iv_token    = 'K'
                     iv_offset   = 0
                     iv_length   = 6
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 6
                     iv_length   = 3
                     iv_text_tag = '' ).
    generate_extend( iv_token    = 'K'
                     iv_offset   = 9
                     iv_length   = 4
                     iv_text_tag = '' ).
    generate_extend( iv_token    = '.'
                     iv_offset   = 13
                     iv_length   = 4
                     iv_text_tag = '' ).

    do_test( lv_line ).

  ENDMETHOD.

********************************************************
* Test parsing and ordering of key words in structures *
********************************************************
  METHOD test_abap_08.

    DATA lv_line TYPE string.

    lv_line = 'lv_length = <match>-length.'.

    " Generate table with expected values after parsing
    generate_parse( iv_token  = 'K'
                    iv_offset = 13
                    iv_length = 5 ).
    generate_parse( iv_token  = 'K'
                    iv_offset = 20
                    iv_length = 6 ).

    " Generate table with expected values after extending
    generate_extend( iv_token    = '.'
                     iv_offset   = 0
                     iv_length   = 27
                     iv_text_tag = '' ).

    do_test( lv_line ).

  ENDMETHOD.

ENDCLASS.
