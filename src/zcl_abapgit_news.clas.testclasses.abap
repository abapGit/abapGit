CLASS ltcl_relevant DEFINITION DEFERRED.
CLASS zcl_abapgit_news DEFINITION LOCAL FRIENDS ltcl_relevant.

CLASS ltcl_relevant DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS:
      test01 FOR TESTING,
      test02 FOR TESTING,
      test03 FOR TESTING.

ENDCLASS.

CLASS ltcl_relevant IMPLEMENTATION.

  METHOD test01.

    DATA: lv_relevant TYPE abap_bool.

    lv_relevant = zcl_abapgit_news=>is_relevant( 'https://github.com/larshp/abapGit.git' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_relevant
      exp = abap_true ).

  ENDMETHOD.

  METHOD test02.

    DATA: lv_relevant TYPE abap_bool.

    lv_relevant = zcl_abapgit_news=>is_relevant( 'https://github.com/larshp/abapGit' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_relevant
      exp = abap_true ).

  ENDMETHOD.

  METHOD test03.

    DATA: lv_relevant TYPE abap_bool.

    lv_relevant = zcl_abapgit_news=>is_relevant( 'https://github.com/larshp/something' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_relevant
      exp = abap_false ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_news DEFINITION DEFERRED.
CLASS zcl_abapgit_news DEFINITION LOCAL FRIENDS ltcl_news.

*----------------------------------------------------------------------*
*       CLASS ltcl_news DEFINITION
*----------------------------------------------------------------------*
* Definition of test class for news announcement
*----------------------------------------------------------------------*
CLASS ltcl_news DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS:
      version_to_numeric FOR TESTING,
      compare_versions   FOR TESTING,
      normalize_version  FOR TESTING,
      parse_line         FOR TESTING,
      parse              FOR TESTING.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_news IMPLEMENTATION
*----------------------------------------------------------------------*
* Implementation of test class for news announcement
*----------------------------------------------------------------------*
CLASS ltcl_news IMPLEMENTATION.

  METHOD version_to_numeric.

    DATA: lv_version_exp TYPE i VALUE 1023010,
          lv_version_act TYPE i.

    lv_version_act = zcl_abapgit_news=>version_to_numeric( '1.23.10' ).

    cl_abap_unit_assert=>assert_equals( exp = lv_version_exp
                                        act = lv_version_act
                                        msg = ' Error during conversion of version to numeric value' ).

  ENDMETHOD.

  METHOD compare_versions.

    DATA lv_result TYPE i.

    " Case 1: version A > version B
    lv_result = zcl_abapgit_news=>compare_versions( iv_a = '1.28.10' iv_b = '1.23.10' ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lv_result
                                        msg = ' Error during comparison of versions. Case: A > B' ).

    CLEAR: lv_result.

    " Case 2: version A < version B
    lv_result = zcl_abapgit_news=>compare_versions( iv_a = '1.28.10' iv_b = '2.23.10' ).

    cl_abap_unit_assert=>assert_equals( exp = -1
                                        act = lv_result
                                        msg = ' Error during comparison of versions. Case: A < B' ).

    CLEAR: lv_result.

    " Case 3: version A = version B
    lv_result = zcl_abapgit_news=>compare_versions( iv_a = '1.28.10' iv_b = '1.28.10' ).

    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = lv_result
                                        msg = ' Error during comparison of versions. Case: A = B' ).

  ENDMETHOD.

  METHOD normalize_version.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_news=>normalize_version( '1.28.10' )
      exp = '1.28.10' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_news=>normalize_version( 'v1.28.10' )
      exp = '1.28.10' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_news=>normalize_version( 'b1.28.10' )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_news=>normalize_version( 'x.y.z' )
      exp = '' ).

  ENDMETHOD.

  METHOD parse_line.

    DATA: ls_log TYPE zcl_abapgit_news=>ty_log.

    ls_log = zcl_abapgit_news=>parse_line(
      iv_line            = '======'
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_initial( ls_log ).

    ls_log = zcl_abapgit_news=>parse_line(
      iv_line            = ''
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_initial( ls_log ).

    ls_log = zcl_abapgit_news=>parse_line(
      iv_line            = '------'
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_initial( ls_log ).

    CLEAR ls_log.
    ls_log = zcl_abapgit_news=>parse_line(
      iv_line            = '2017-02-13 v1.28.0'
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-version    exp = '1.28.0' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_header  exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-pos_to_cur exp = 1 ).

    CLEAR ls_log.
    ls_log = zcl_abapgit_news=>parse_line(
      iv_line            = '2017-02-13 v1.26.0'
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-version    exp = '1.26.0' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_header  exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-pos_to_cur exp = -1 ).

    CLEAR ls_log.
    ls_log = zcl_abapgit_news=>parse_line(
      iv_line            = 'news'
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-version      exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_header    exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-pos_to_cur   exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_important exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-text         exp = 'news' ).

    CLEAR ls_log.
    ls_log = zcl_abapgit_news=>parse_line(
      iv_line            = ' ! important news'
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-version      exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_header    exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-pos_to_cur   exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_important exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-text         exp = ' ! important news' ).

  ENDMETHOD.

  METHOD parse.

    DEFINE _add_news_log_entry.
      CLEAR: ls_log.
      ls_log-version      = &1.
      ls_log-is_header    = &2.
      ls_log-is_important = &3.
      ls_log-pos_to_cur   = &4.
      ls_log-text         = &5.
      APPEND ls_log TO lt_log_exp.
    END-OF-DEFINITION.

    DEFINE _add_news_txt_entry.
      APPEND &1 TO lt_lines.
    END-OF-DEFINITION.

    DATA: lt_log_exp TYPE zcl_abapgit_news=>tt_log,
          lt_log_act TYPE zcl_abapgit_news=>tt_log,
          ls_log     LIKE LINE OF lt_log_exp,
          lt_lines   TYPE string_table.

    " Generate test data
    _add_news_txt_entry '======'.
    _add_news_txt_entry '------'.
    _add_news_txt_entry `      `.
    _add_news_txt_entry 'abapGit changelog'.
    _add_news_txt_entry '2017-02-13 v1.28.0'.
    _add_news_txt_entry '------------------'.
    _add_news_txt_entry '+ Staging page redesigned'.
    _add_news_txt_entry '! Support for core data services'.
    _add_news_txt_entry `      `.
    _add_news_txt_entry '2017-01-25 v1.27.0'.
    _add_news_txt_entry '------------------'.
    _add_news_txt_entry '+ Two factor authentication with github.com'.
    _add_news_txt_entry '2017-01-25 v1.26.0'.

    " Case 1
    " Generate expected results
    "                   VERSION  HEAD IMP POS TEXT
    _add_news_log_entry '1.28.0' 'X'  ''  1   '2017-02-13 v1.28.0'.
    _add_news_log_entry '1.28.0' ''   ''  0   '+ Staging page redesigned'.
    _add_news_log_entry '1.28.0' ''   'X' 0   '! Support for core data services'.
    _add_news_log_entry '1.27.0' 'X'  ''  1   '2017-01-25 v1.27.0'.
    _add_news_log_entry '1.27.0' ''   ''  0   '+ Two factor authentication with github.com'.

    lt_log_act = zcl_abapgit_news=>parse( it_lines = lt_lines iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_equals( exp = lt_log_exp
                                        act = lt_log_act
                                        msg = ' Error during parsing: Case 1.' ).


    " Case 2 (exect version match)
    CLEAR lt_log_exp.
    "                   VERSION  HEAD IMP UPD TEXT
    _add_news_log_entry '1.28.0' 'X'  ''  1   '2017-02-13 v1.28.0'.
    _add_news_log_entry '1.28.0' ''   ''  0   '+ Staging page redesigned'.
    _add_news_log_entry '1.28.0' ''   'X' 0   '! Support for core data services'.

    " Case 1: Test parsing of data
    lt_log_act = zcl_abapgit_news=>parse( it_lines = lt_lines iv_current_version = '1.27.00' ).
    cl_abap_unit_assert=>assert_equals( exp = lt_log_exp
                                        act = lt_log_act
                                        msg = ' Error during parsing: Case 2.' ).

    " Case 3 (display tail)
    CLEAR lt_log_exp.
    "                   VERSION  HEAD IMP UPD TEXT
    _add_news_log_entry '1.28.0' 'X'  ''  0   '2017-02-13 v1.28.0'.
    _add_news_log_entry '1.28.0' ''   ''  0   '+ Staging page redesigned'.
    _add_news_log_entry '1.28.0' ''   'X' 0   '! Support for core data services'.
    _add_news_log_entry '1.27.0' 'X'  ''  -1  '2017-01-25 v1.27.0'.
    _add_news_log_entry '1.27.0' ''   ''  0   '+ Two factor authentication with github.com'.
    _add_news_log_entry '1.26.0' 'X'  ''  -1  '2017-01-25 v1.26.0'.

    " Case 1: Test parsing of data
    lt_log_act = zcl_abapgit_news=>parse( it_lines = lt_lines iv_current_version = '1.28.00' ).
    cl_abap_unit_assert=>assert_equals( exp = lt_log_exp
                                        act = lt_log_act
                                        msg = ' Error during parsing: Case 3.' ).

  ENDMETHOD.

ENDCLASS.
