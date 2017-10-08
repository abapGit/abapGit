*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_NEWS
*&---------------------------------------------------------------------*
CLASS ltcl_news DEFINITION DEFERRED.

*&---------------------------------------------------------------------*
*&       Class lcl_news
*&---------------------------------------------------------------------*
* Class responsible for preparation of data for news announcements
*----------------------------------------------------------------------*
CLASS lcl_news DEFINITION CREATE PRIVATE FRIENDS ltcl_news.

  PUBLIC SECTION.

    CONSTANTS: c_tail_length TYPE i VALUE 5. " Number of versions to display if no updates

    TYPES:
      BEGIN OF ty_log,
        version      TYPE string,
        pos_to_cur   TYPE i,
        is_header    TYPE abap_bool,
        is_important TYPE abap_bool,
        text         TYPE string,
      END OF ty_log,
      tt_log TYPE STANDARD TABLE OF ty_log WITH DEFAULT KEY.

    CLASS-METHODS:
      create " TODO REFACTOR
        IMPORTING io_repo            TYPE REF TO lcl_repo
        RETURNING VALUE(ro_instance) TYPE REF TO lcl_news
        RAISING   zcx_abapgit_exception.

    METHODS:
      get_log
        RETURNING VALUE(rt_log) TYPE tt_log,
      latest_version
        RETURNING VALUE(rv_version) TYPE string,
      has_news
        RETURNING VALUE(rv_boolean) TYPE abap_bool,
      has_important
        RETURNING VALUE(rv_boolean) TYPE abap_bool,
      has_updates
        RETURNING VALUE(rv_boolean) TYPE abap_bool,
      has_unseen
        RETURNING VALUE(rv_boolean) TYPE abap_bool.

  PRIVATE SECTION.
    DATA: mt_log              TYPE tt_log,
          mv_current_version  TYPE string,
          mv_lastseen_version TYPE string,
          mv_latest_version   TYPE string.

    METHODS:
      constructor
        IMPORTING iv_rawdata          TYPE xstring
                  iv_lastseen_version TYPE string
                  iv_current_version  TYPE string.

    CLASS-METHODS:
      version_to_numeric
        IMPORTING iv_version        TYPE string
        RETURNING VALUE(rv_version) TYPE i,
      normalize_version
        IMPORTING iv_version        TYPE string
        RETURNING VALUE(rv_version) TYPE string,
      compare_versions
        IMPORTING iv_a             TYPE string
                  iv_b             TYPE string
        RETURNING VALUE(rv_result) TYPE i,
      parse_line
        IMPORTING iv_line            TYPE string
                  iv_current_version TYPE string
        RETURNING VALUE(rs_log)      TYPE ty_log,
      parse
        IMPORTING it_lines           TYPE string_table
                  iv_current_version TYPE string
        RETURNING VALUE(rt_log)      TYPE tt_log.

ENDCLASS.               "lcl_news

*----------------------------------------------------------------------*
*       CLASS lcl_news IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_news IMPLEMENTATION.

  METHOD create. " TODO REFACTOR !

    CONSTANTS: " TODO refactor
      lc_log_path     TYPE string VALUE '/',
      lc_log_filename TYPE string VALUE 'changelog.txt'.

    DATA: lt_remote      TYPE zif_abapgit_definitions=>ty_files_tt,
          lv_last_seen   TYPE string,
          lv_url         TYPE string,
          lo_repo_online TYPE REF TO lcl_repo_online.

    FIELD-SYMBOLS <file> LIKE LINE OF lt_remote.

    IF io_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    lo_repo_online ?= io_repo.
    lv_url          = lo_repo_online->get_url( ).

    " News announcement temporary restricted to abapGit only
    IF lv_url NS '/abapGit.git'. " TODO refactor
      RETURN.
    ENDIF.

    lv_last_seen = lcl_app=>user( )->get_repo_last_change_seen( lv_url ).

    TRY.
        " Find changelog
        lt_remote = io_repo->get_files_remote( ).
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    READ TABLE lt_remote ASSIGNING <file>
      WITH KEY path = lc_log_path filename = lc_log_filename.

    IF sy-subrc = 0.
      CREATE OBJECT ro_instance
        EXPORTING
          iv_rawdata          = <file>-data
          iv_current_version  = zif_abapgit_definitions=>gc_abap_version " TODO refactor
          iv_lastseen_version = normalize_version( lv_last_seen ).
    ENDIF.

    IF ro_instance IS BOUND.
      lcl_app=>user( )->set_repo_last_change_seen(
        iv_url     = lv_url
        iv_version = ro_instance->latest_version( ) ).
    ENDIF.

  ENDMETHOD.                    "create

  METHOD constructor.

    DATA: lt_lines    TYPE string_table,
          lv_string   TYPE string,
          ls_log_line LIKE LINE OF mt_log.

    " Validate params
    mv_current_version  = normalize_version( iv_current_version ).
    mv_lastseen_version = normalize_version( iv_lastseen_version ).
    IF mv_current_version IS INITIAL.
      RETURN. " Internal format of program version is not correct -> abort parsing
    ENDIF.

    lv_string = lcl_convert=>xstring_to_string_utf8( iv_rawdata ).
    lt_lines  = lcl_convert=>split_string( lv_string ).
    mt_log    = parse( it_lines = lt_lines iv_current_version = mv_current_version ).

    READ TABLE mt_log INTO ls_log_line INDEX 1.
    mv_latest_version = ls_log_line-version. " Empty if not found

  ENDMETHOD.                    "constructor

  METHOD parse.

    DATA: lv_tail                TYPE i,
          lv_first_version_found TYPE abap_bool,
          lv_version             TYPE string,
          ls_log                 LIKE LINE OF rt_log.

    FIELD-SYMBOLS: <line> LIKE LINE OF it_lines.

    LOOP AT it_lines ASSIGNING <line>.
      ls_log = parse_line( iv_line = <line> iv_current_version = iv_current_version ).

      " Skip until first version head and Skip empty lines
      CHECK ls_log IS NOT INITIAL AND
            ( lv_first_version_found = abap_true OR ls_log-version IS NOT INITIAL ).

      IF lv_first_version_found = abap_false.
        lv_first_version_found = abap_true.
        IF compare_versions( iv_a = ls_log-version iv_b = iv_current_version ) <= 0.
          lv_tail = c_tail_length. " Display some last versions if no updates
        ENDIF.
      ENDIF.

      IF ls_log-is_header = abap_true.
        "Skip everything below current version or show tail news
        IF compare_versions( iv_a = ls_log-version iv_b = iv_current_version ) <= 0.
          IF lv_tail > 0.
            lv_tail = lv_tail - 1.
          ELSE.
            EXIT.
          ENDIF.
        ENDIF.
        lv_version = ls_log-version. " Save to fill news lines
      ELSE.
        ls_log-version = lv_version.
      ENDIF.

      APPEND ls_log TO rt_log.
    ENDLOOP.

  ENDMETHOD.                    "parse

  METHOD parse_line.

    CONSTANTS: lc_header_pattern TYPE string
        VALUE '^\d{4}-\d{2}-\d{2}\s+v(\d{1,3}\.\d{1,3}\.\d{1,3})\s*$'.

    DATA: lv_version TYPE string.

    IF iv_line IS INITIAL OR iv_line CO ' -='.
      RETURN. " Skip empty and markup lines
    ENDIF.

    " Check if line is a header line
    FIND FIRST OCCURRENCE OF REGEX lc_header_pattern IN iv_line SUBMATCHES lv_version.
    IF sy-subrc IS INITIAL.
      lv_version        = normalize_version( lv_version ).
      rs_log-version    = lv_version.
      rs_log-is_header  = abap_true.
      rs_log-pos_to_cur = compare_versions( iv_a = lv_version iv_b = iv_current_version ).
    ELSE.
      FIND FIRST OCCURRENCE OF REGEX '^\s*!' IN iv_line.
      rs_log-is_important = boolc( sy-subrc IS INITIAL ). " Change is important
    ENDIF.

    rs_log-text = iv_line.

  ENDMETHOD.                    "parse_line

  METHOD get_log.
    rt_log = me->mt_log.
  ENDMETHOD.                    "get_log

  METHOD latest_version.
    rv_version = me->mv_latest_version.
  ENDMETHOD.                    "latest_version

  METHOD has_news.
    rv_boolean = boolc( lines( mt_log ) > 0 ).
  ENDMETHOD.                    "has_news

  METHOD has_important.
    READ TABLE mt_log WITH KEY is_important = abap_true TRANSPORTING NO FIELDS.
    rv_boolean = boolc( sy-subrc IS INITIAL ).
  ENDMETHOD.                    "has_important_news

  METHOD has_updates.
    rv_boolean = boolc( compare_versions(
      iv_a = mv_latest_version
      iv_b = mv_current_version ) > 0 ).
  ENDMETHOD.                    "has_updates

  METHOD has_unseen.
    rv_boolean = boolc( compare_versions(
      iv_a = mv_latest_version
      iv_b = mv_lastseen_version ) > 0 ).
  ENDMETHOD.                    "has_unseen

  METHOD compare_versions.

    DATA: lv_version_a TYPE i,
          lv_version_b TYPE i.

    " Convert versions to numeric
    lv_version_a = version_to_numeric( iv_a ).
    lv_version_b = version_to_numeric( iv_b ).

    " Compare versions
    IF lv_version_a > lv_version_b.
      rv_result = 1.
    ELSEIF lv_version_a < lv_version_b.
      rv_result = -1.
    ELSE.
      rv_result = 0.
    ENDIF.

  ENDMETHOD.                    "compare_versions

  METHOD normalize_version.

    " Internal program version should be in format "XXX.XXX.XXX" or "vXXX.XXX.XXX"
    CONSTANTS: lc_version_pattern TYPE string VALUE '^v?(\d{1,3}\.\d{1,3}\.\d{1,3})\s*$'.

    FIND FIRST OCCURRENCE OF REGEX lc_version_pattern
      IN iv_version SUBMATCHES rv_version.

  ENDMETHOD.                    "normalize_version

  METHOD version_to_numeric.

    DATA: lv_major   TYPE numc4,
          lv_minor   TYPE numc4,
          lv_release TYPE numc4.

    SPLIT iv_version AT '.' INTO lv_major lv_minor lv_release.

    " Calculated value of version number, empty version will become 0 which is OK
    rv_version = lv_major * 1000000 + lv_minor * 1000 + lv_release.

  ENDMETHOD.                    "convert_version_to_numeric

ENDCLASS.               "lcl_news

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

ENDCLASS.                    "ltcl_news DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_news IMPLEMENTATION
*----------------------------------------------------------------------*
* Implementation of test class for news announcement
*----------------------------------------------------------------------*
CLASS ltcl_news IMPLEMENTATION.

  METHOD version_to_numeric.

    DATA: lv_version_exp TYPE i VALUE 1023010,
          lv_version_act TYPE i.

    lv_version_act = lcl_news=>version_to_numeric( '1.23.10' ).

    cl_abap_unit_assert=>assert_equals( exp = lv_version_exp
                                        act = lv_version_act
                                        msg = ' Error during conversion of version to numeric value' ).

  ENDMETHOD.                    "convert_version_to_numeric

  METHOD compare_versions.

    DATA lv_result TYPE i.

    " Case 1: version A > version B
    lv_result = lcl_news=>compare_versions( iv_a = '1.28.10' iv_b = '1.23.10' ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lv_result
                                        msg = ' Error during comparison of versions. Case: A > B' ).

    CLEAR: lv_result.

    " Case 2: version A < version B
    lv_result = lcl_news=>compare_versions( iv_a = '1.28.10' iv_b = '2.23.10' ).

    cl_abap_unit_assert=>assert_equals( exp = -1
                                        act = lv_result
                                        msg = ' Error during comparison of versions. Case: A < B' ).

    CLEAR: lv_result.

    " Case 3: version A = version B
    lv_result = lcl_news=>compare_versions( iv_a = '1.28.10' iv_b = '1.28.10' ).

    cl_abap_unit_assert=>assert_equals( exp = 0
                                        act = lv_result
                                        msg = ' Error during comparison of versions. Case: A = B' ).

  ENDMETHOD.                    "compare_versions

  METHOD normalize_version.

    cl_abap_unit_assert=>assert_equals(
      act = lcl_news=>normalize_version( '1.28.10' )
      exp = '1.28.10' ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_news=>normalize_version( 'v1.28.10' )
      exp = '1.28.10' ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_news=>normalize_version( 'b1.28.10' )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_news=>normalize_version( 'x.y.z' )
      exp = '' ).

  ENDMETHOD.                    "normalize_version

  METHOD parse_line.

    DATA: ls_log TYPE lcl_news=>ty_log.

    ls_log = lcl_news=>parse_line(
      iv_line            = '======'
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_initial( ls_log ).

    ls_log = lcl_news=>parse_line(
      iv_line            = ''
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_initial( ls_log ).

    ls_log = lcl_news=>parse_line(
      iv_line            = '------'
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_initial( ls_log ).

    CLEAR ls_log.
    ls_log = lcl_news=>parse_line(
      iv_line            = '2017-02-13 v1.28.0'
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-version    exp = '1.28.0' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_header  exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-pos_to_cur exp = 1 ).

    CLEAR ls_log.
    ls_log = lcl_news=>parse_line(
      iv_line            = '2017-02-13 v1.26.0'
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-version    exp = '1.26.0' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_header  exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-pos_to_cur exp = -1 ).

    CLEAR ls_log.
    ls_log = lcl_news=>parse_line(
      iv_line            = 'news'
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-version      exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_header    exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-pos_to_cur   exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_important exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-text         exp = 'news' ).

    CLEAR ls_log.
    ls_log = lcl_news=>parse_line(
      iv_line            = ' ! important news'
      iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-version      exp = '' ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_header    exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-pos_to_cur   exp = 0 ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-is_important exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = ls_log-text         exp = ' ! important news' ).

  ENDMETHOD.                    "parse_line

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

    DATA: lt_log_exp TYPE lcl_news=>tt_log,
          lt_log_act TYPE lcl_news=>tt_log,
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

    lt_log_act = lcl_news=>parse( it_lines = lt_lines iv_current_version = '1.26.01' ).
    cl_abap_unit_assert=>assert_equals( exp = lt_log_exp
                                        act = lt_log_act
                                        msg = ' Error during parsing: Case 1.' ).


    " Case 2 (exect version match)
    CLEAR lt_log_exp.
    "                   VERSION  HEAD IMP UPD TEXT
    _add_news_log_entry '1.28.0' 'X'  ''  1   '2017-02-13 v1.28.0'.
    _add_news_log_entry '1.28.0' ''   ''  0   '+ Staging page redesigned'.
    _add_news_log_entry '1.28.0' ''   'X' 0   '! Support for core data services'.

    " Case 1. Test parsing of data
    lt_log_act = lcl_news=>parse( it_lines = lt_lines iv_current_version = '1.27.00' ).
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

    " Case 1. Test parsing of data
    lt_log_act = lcl_news=>parse( it_lines = lt_lines iv_current_version = '1.28.00' ).
    cl_abap_unit_assert=>assert_equals( exp = lt_log_exp
                                        act = lt_log_act
                                        msg = ' Error during parsing: Case 3.' ).

  ENDMETHOD.                    "parse

ENDCLASS.                    "ltcl_news IMPLEMENTATION
