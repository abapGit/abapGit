*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_NEWS
*&---------------------------------------------------------------------*
CLASS ltcl_news DEFINITION DEFERRED.

*&---------------------------------------------------------------------*
*&       Class lcl_news
*&---------------------------------------------------------------------*
* Class responsible for preparation of data for news announcements
*----------------------------------------------------------------------*
CLASS lcl_news DEFINITION FRIENDS ltcl_news.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_log,
        version     TYPE string,
        header      TYPE abap_bool,
        important   TYPE abap_bool,
        text        TYPE string,
      END OF ty_log,
      tt_log TYPE STANDARD TABLE OF ty_log WITH DEFAULT KEY.

    CONSTANTS:
      c_log_filename TYPE string VALUE '/',
      c_log_path     TYPE string VALUE 'changelog.txt'.

    CLASS-METHODS create
      IMPORTING io_repo             TYPE REF TO lcl_repo
      RETURNING VALUE(ro_instance)  TYPE REF TO lcl_news.

    METHODS:
      constructor
        IMPORTING iv_rawdata        TYPE xstring
                  iv_version        TYPE string,
      has_news
        RETURNING value(rv_boolean) TYPE abap_bool,
      get_log
        RETURNING value(rt_log)     TYPE tt_log,
      has_important_news
        RETURNING value(rv_boolean) TYPE abap_bool.

  PRIVATE SECTION.
    DATA mt_log TYPE tt_log.

    CLASS-METHODS:
      split_string
        IMPORTING iv_string         TYPE string
        RETURNING value(rt_lines)   TYPE string_table,

      convert_version_to_numeric
        IMPORTING iv_version        TYPE string
        RETURNING value(rv_version) TYPE i,

      parse_data
        IMPORTING it_lines          TYPE string_table
                  iv_version        TYPE string
        RETURNING value(rt_log)     TYPE tt_log,

      compare_versions
        IMPORTING iv_a              TYPE string
                  iv_b              TYPE string
        RETURNING value(rv_result)  TYPE i.

ENDCLASS.               "lcl_news

*----------------------------------------------------------------------*
*       CLASS lcl_news IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_news IMPLEMENTATION.

  METHOD constructor.
    DATA:
          lt_lines  TYPE string_table,
          lv_string TYPE string.

    lv_string = lcl_convert=>xstring_to_string_utf8( iv_rawdata ).
    lt_lines  = lcl_news=>split_string( lv_string ).
    mt_log    = lcl_news=>parse_data( it_lines = lt_lines iv_version = iv_version ).

  ENDMETHOD.                    "constructor

  METHOD create.

    DATA:
          lt_remote      TYPE ty_files_tt,
          lo_repo_online TYPE REF TO lcl_repo_online.

    FIELD-SYMBOLS <file> LIKE LINE OF lt_remote.

    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.

      " News announcement temporary restricted to abapGit only
      IF lo_repo_online->get_url( ) CS '/abapGit.git'.
        lt_remote = io_repo->get_files_remote( ).

        READ TABLE lt_remote ASSIGNING <file> WITH KEY path = c_log_filename
                                                   filename = c_log_path.
        IF sy-subrc = 0.
        CREATE OBJECT ro_instance EXPORTING
          iv_rawdata = <file>-data
          iv_version = gc_abap_version.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "create

  METHOD split_string.

    DATA ls_line LIKE LINE OF rt_lines.

    FIND FIRST OCCURRENCE OF cl_abap_char_utilities=>cr_lf IN iv_string.

    " Convert string into table depending on separator type CR_LF vs. LF
    IF sy-subrc = 0.
      SPLIT iv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE rt_lines.
    ELSE.
      SPLIT iv_string AT cl_abap_char_utilities=>newline INTO TABLE rt_lines.
    ENDIF.

  ENDMETHOD.                    "split_string

  METHOD parse_data.

    CONSTANTS:
      lc_changelog_version TYPE string
        VALUE '^\d{4}-\d{2}-\d{2}\s+v(\d{1,3}\.\d{1,3}\.\d{1,3})\s*$',
      lc_internal_version  TYPE string
        VALUE '^v?(\d{1,3}\.\d{1,3}\.\d{1,3})\s*$'.

    DATA:
          lv_first_version_found  TYPE abap_bool,
          lv_version              TYPE string,
          lv_current_version      TYPE string,
          ls_log                  LIKE LINE OF rt_log.

    FIELD-SYMBOLS: <line> LIKE LINE OF it_lines.

    " Internal program version should be in format "vXXX.XXX.XXX"
    FIND FIRST OCCURRENCE OF REGEX lc_internal_version IN iv_version SUBMATCHES lv_current_version.

    IF sy-subrc IS NOT INITIAL.
      RETURN. " Internal format of program version is not correct. TODO implement error message
    ENDIF.

    LOOP AT it_lines ASSIGNING <line>.
      CLEAR: lv_version, ls_log-text, ls_log-important, ls_log-header.

      IF <line> IS INITIAL OR <line> CO ' -='. " Skip empty and technical lines
        CONTINUE.
      ENDIF.

      " Check if line is a header line
      FIND FIRST OCCURRENCE OF REGEX lc_changelog_version IN <line> SUBMATCHES lv_version.

      " Skip entries before first version found
      IF lv_first_version_found = abap_false.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ELSE.
          lv_first_version_found = abap_true.
        ENDIF.
      ENDIF.

      "Skip everything below current version
      IF lv_version IS NOT INITIAL
         AND lcl_news=>compare_versions( iv_a = lv_version iv_b = lv_current_version ) < 1.
        EXIT.
      ENDIF.

      " Populate log
      IF lv_version IS NOT INITIAL. " Version header
        ls_log-version = lv_version. " ... stays for all subsequent non-version lines
        ls_log-header  = abap_true.
      ELSE.                         " Version line item
        FIND FIRST OCCURRENCE OF REGEX '^\s*!' IN <line>.
        IF sy-subrc IS INITIAL.
          ls_log-important = abap_true. " Change is important
        ENDIF.
      ENDIF.

      ls_log-text = <line>.

      APPEND ls_log TO rt_log.
    ENDLOOP.

  ENDMETHOD.                    "parse_data

  METHOD has_news.

    rv_boolean = boolc( lines( mt_log ) > 0 ).

  ENDMETHOD.                    "has_news

  METHOD get_log.

    rt_log = me->mt_log.

  ENDMETHOD.                    "get_log

  METHOD has_important_news.

    READ TABLE mt_log WITH KEY important = abap_true TRANSPORTING NO FIELDS.

    rv_boolean = boolc( sy-subrc IS INITIAL ).

  ENDMETHOD.                    "has_important_news

  METHOD compare_versions.

    DATA:
          lv_version_a  TYPE i,
          lv_version_b  TYPE i.

    " Convert versions to numeric
    lv_version_a = lcl_news=>convert_version_to_numeric( iv_a ).
    lv_version_b = lcl_news=>convert_version_to_numeric( iv_b ).

    " Compare versions
    IF lv_version_a > lv_version_b.
      rv_result = 1.
    ELSEIF lv_version_a < lv_version_b.
      rv_result = -1.
    ELSE.
      rv_result = 0.
    ENDIF.

  ENDMETHOD.                    "compare_versions

  METHOD convert_version_to_numeric.

    DATA: lv_major   TYPE numc4,
          lv_minor   TYPE numc4,
          lv_release TYPE numc4.

    SPLIT iv_version AT '.' INTO lv_major lv_minor lv_release.

    " Calculated value of version number
    rv_version = lv_major * 1000000 + lv_minor * 1000 + lv_release.

  ENDMETHOD.                    "convert_version_to_numeric

ENDCLASS.               "lcl_news

*----------------------------------------------------------------------*
*       CLASS ltcl_news DEFINITION
*----------------------------------------------------------------------*
* Definition of test class for news announcement
*----------------------------------------------------------------------*
CLASS ltcl_news DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS:
      split_string                FOR TESTING,
      convert_version_to_numeric  FOR TESTING,
      compare_versions            FOR TESTING,
      parse_data                  FOR TESTING.

ENDCLASS.                    "ltcl_news DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_news IMPLEMENTATION
*----------------------------------------------------------------------*
* Implementation of test class for news announcement
*----------------------------------------------------------------------*
CLASS ltcl_news IMPLEMENTATION.

  METHOD split_string.

    DATA: lt_act TYPE string_table,
          lt_exp TYPE string_table.

    APPEND 'ABC' TO lt_exp.
    APPEND '123' TO lt_exp.

    " Case 1. String separated by CRLF
    lt_act = lcl_news=>split_string( 'ABC' && cl_abap_char_utilities=>cr_lf && '123' ).

    cl_abap_unit_assert=>assert_equals( exp = lt_exp
                                        act = lt_act
                                        msg = ' Error during string split: CRLF' ).

    CLEAR: lt_act.

    " Case 2. String separated by LF
    lt_act = lcl_news=>split_string( 'ABC' && cl_abap_char_utilities=>newline && '123' ).

    cl_abap_unit_assert=>assert_equals( exp = lt_exp
                                        act = lt_act
                                        msg = ' Error during string split: LF' ).

  ENDMETHOD.                    "split_string.

  METHOD convert_version_to_numeric.

    DATA: lv_version_exp  TYPE i VALUE 1023010,
          lv_version_act  TYPE i.

    lv_version_act = lcl_news=>convert_version_to_numeric( '1.23.10' ).

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

  DEFINE _add_news_log_entry.
    CLEAR: ls_log.
    ls_log-version   = &1.
    ls_log-header    = &2.
    ls_log-important = &3.
    ls_log-text      = &4.
    APPEND ls_log TO lt_log_exp.
  END-OF-DEFINITION.

  METHOD parse_data.

    DATA:
          lt_log_exp  TYPE lcl_news=>tt_log,
          lt_log_act  TYPE lcl_news=>tt_log,
          ls_log      LIKE LINE OF lt_log_exp,
          lt_lines    TYPE string_table.

    " Generate test data
    APPEND '======' TO lt_lines.
    APPEND '------' TO lt_lines.
    APPEND '      ' TO lt_lines.
    APPEND 'abapGit changelog' TO lt_lines.
    APPEND '2017-02-13 v1.28.0' TO lt_lines.
    APPEND '------------------' TO lt_lines.
    APPEND '+ Staging page redesigned' TO lt_lines.
    APPEND '! Support for core data services' TO lt_lines.
    APPEND '      ' TO lt_lines.
    APPEND '2017-01-25 v1.27.0' TO lt_lines.
    APPEND '------------------' TO lt_lines.
    APPEND '+ Two factor authentication with github.com' TO lt_lines.
    APPEND '2017-01-25 v1.26.0' TO lt_lines.

    " Generate expected results
    _add_news_log_entry '1.28.0' 'X'  ''  '2017-02-13 v1.28.0'.
    _add_news_log_entry '1.28.0' ''   ''  '+ Staging page redesigned'.
    _add_news_log_entry '1.28.0' ''   'X' '! Support for core data services'.
    _add_news_log_entry '1.27.0' 'X'  ''  '2017-01-25 v1.27.0'.
    _add_news_log_entry '1.27.0' ''   ''  '+ Two factor authentication with github.com'.

    " Case 1. Test parsing of data
    lt_log_act = lcl_news=>parse_data( it_lines = lt_lines iv_version = '1.26.01' ).

    cl_abap_unit_assert=>assert_equals( exp = lt_log_exp
                                        act = lt_log_act
                                        msg = ' Error during parsing: Case 1.' ).

    CLEAR: lt_log_act, lt_log_exp.

    " Case 2. Negative test - format of version is not correct
    lt_log_act = lcl_news=>parse_data( it_lines = lt_lines iv_version = 'version.1.27.00' ).

    cl_abap_unit_assert=>assert_equals( exp = lt_log_exp
                                        act = lt_log_act
                                        msg = ' Error during parsing: Case 2.' ).

  ENDMETHOD.                    "parse_data

ENDCLASS.                    "ltcl_news IMPLEMENTATION
