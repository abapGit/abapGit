**********************************************************************
* Helper classed

CLASS lcl_string_buffer DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_buffer TYPE string_table.
    METHODS add
      IMPORTING
        iv_str TYPE string.
ENDCLASS.

CLASS lcl_string_buffer IMPLEMENTATION.
  METHOD add.
    APPEND iv_str TO mt_buffer.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_log_entries DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_log_entries TYPE zcl_abapgit_news=>ty_logs.
    METHODS add
      IMPORTING
        iv_str TYPE string.
ENDCLASS.

CLASS lcl_log_entries IMPLEMENTATION.
  METHOD add.
    DATA ls_log LIKE LINE OF mt_log_entries.
    DATA lv_pos_to_cur_str TYPE string.

    SPLIT iv_str AT '/' INTO
      ls_log-version
      ls_log-is_header
      ls_log-is_important
      lv_pos_to_cur_str
      ls_log-text.

    CONDENSE ls_log-version.
    CONDENSE ls_log-is_header.
    CONDENSE ls_log-is_important.
    CONDENSE ls_log-text.
    ls_log-pos_to_cur = lv_pos_to_cur_str.

    APPEND ls_log TO mt_log_entries.
  ENDMETHOD.
ENDCLASS.

**********************************************************************

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
      parse_line         FOR TESTING,
      parse              FOR TESTING.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS ltcl_news IMPLEMENTATION
*----------------------------------------------------------------------*
* Implementation of test class for news announcement
*----------------------------------------------------------------------*
CLASS ltcl_news IMPLEMENTATION.

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
    cl_abap_unit_assert=>assert_equals( act = ls_log-version
                                        exp = '1.28.0' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_header
                                        exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-pos_to_cur
                                        exp = 1 ).

    CLEAR ls_log.
    ls_log = zcl_abapgit_news=>parse_line(
      iv_line            = '2017-02-13 v1.26.0'
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-version
                                        exp = '1.26.0' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_header
                                        exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-pos_to_cur
                                        exp = -1 ).

    CLEAR ls_log.
    ls_log = zcl_abapgit_news=>parse_line(
      iv_line            = 'news'
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-version
                                        exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_header
                                        exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-pos_to_cur
                                        exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_important
                                        exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-text
                                        exp = 'news' ).

    CLEAR ls_log.
    ls_log = zcl_abapgit_news=>parse_line(
      iv_line            = ' ! important news'
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-version
                                        exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_header
                                        exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-pos_to_cur
                                        exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_important
                                        exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-text
                                        exp = ' ! important news' ).

  ENDMETHOD.

  METHOD parse.

    DATA lt_log_act TYPE zcl_abapgit_news=>ty_logs.
    DATA lo_src_text_buf TYPE REF TO lcl_string_buffer.
    DATA lo_log_entries TYPE REF TO lcl_log_entries.

    " Generate test data
    CREATE OBJECT lo_src_text_buf.
    lo_src_text_buf->add( '======' ).
    lo_src_text_buf->add( '------' ).
    lo_src_text_buf->add( `      ` ).
    lo_src_text_buf->add( 'abapGit changelog' ).
    lo_src_text_buf->add( '2017-02-13 v1.28.0' ).
    lo_src_text_buf->add( '------------------' ).
    lo_src_text_buf->add( '+ Staging page redesigned' ).
    lo_src_text_buf->add( '! Support for core data services' ).
    lo_src_text_buf->add( `      ` ).
    lo_src_text_buf->add( '2017-01-25 v1.27.0' ).
    lo_src_text_buf->add( '------------------' ).
    lo_src_text_buf->add( '+ Two factor authentication with github.com' ).
    lo_src_text_buf->add( '2017-01-25 v1.26.0' ).

    " Case 1
    " Generate expected results
    CREATE OBJECT lo_log_entries.
    "                   VERSION  HEAD IMP POS  TEXT
    lo_log_entries->add( '1.28.0 /X   /   /1   /2017-02-13 v1.28.0' ).
    lo_log_entries->add( '1.28.0 /    /   /0   /+ Staging page redesigned' ).
    lo_log_entries->add( '1.28.0 /    /X  /0   /! Support for core data services' ).
    lo_log_entries->add( '1.27.0 /X   /   /1   /2017-01-25 v1.27.0' ).
    lo_log_entries->add( '1.27.0 /    /   /0   /+ Two factor authentication with github.com' ).

    lt_log_act = zcl_abapgit_news=>parse(
      it_lines = lo_src_text_buf->mt_buffer
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_equals(
      exp = lo_log_entries->mt_log_entries
      act = lt_log_act
      msg = ' Error during parsing: Case 1.' ).


    " Case 2 (exect version match)
    CREATE OBJECT lo_log_entries.
    "                   VERSION  HEAD IMP UPD TEXT
    lo_log_entries->add( '1.28.0 /X  /   /1   /2017-02-13 v1.28.0' ).
    lo_log_entries->add( '1.28.0 /   /   /0   /+ Staging page redesigned' ).
    lo_log_entries->add( '1.28.0 /   /X  /0   /! Support for core data services' ).

    lt_log_act = zcl_abapgit_news=>parse(
      it_lines = lo_src_text_buf->mt_buffer
      iv_current_version = '1.27.00' ).
    cl_abap_unit_assert=>assert_equals(
      exp = lo_log_entries->mt_log_entries
      act = lt_log_act
      msg = ' Error during parsing: Case 2.' ).

    " Case 3 (display tail)
    CREATE OBJECT lo_log_entries.
    "                   VERSION  HEAD IMP UPD TEXT
    lo_log_entries->add( '1.28.0 /X  /   /0   /2017-02-13 v1.28.0' ).
    lo_log_entries->add( '1.28.0 /   /   /0   /+ Staging page redesigned' ).
    lo_log_entries->add( '1.28.0 /   /X  /0   /! Support for core data services' ).
    lo_log_entries->add( '1.27.0 /X  /   /-1  /2017-01-25 v1.27.0' ).
    lo_log_entries->add( '1.27.0 /   /   /0   /+ Two factor authentication with github.com' ).
    lo_log_entries->add( '1.26.0 /X  /   /-1  /2017-01-25 v1.26.0' ).

    lt_log_act = zcl_abapgit_news=>parse(
      it_lines = lo_src_text_buf->mt_buffer
      iv_current_version = '1.28.00' ).
    cl_abap_unit_assert=>assert_equals(
      exp = lo_log_entries->mt_log_entries
      act = lt_log_act
      msg = ' Error during parsing: Case 3.' ).

  ENDMETHOD.

ENDCLASS.
