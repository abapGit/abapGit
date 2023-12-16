CLASS zcl_abapgit_news DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_log,
        version      TYPE string,
        pos_to_cur   TYPE i,
        is_header    TYPE abap_bool,
        is_important TYPE abap_bool,
        text         TYPE string,
      END OF ty_log .
    TYPES:
      ty_logs TYPE STANDARD TABLE OF ty_log WITH DEFAULT KEY .

    CONSTANTS c_tail_length TYPE i VALUE 5 ##NO_TEXT.     " Number of versions to display if no updates

    CLASS-METHODS create     " TODO REFACTOR
      IMPORTING
        !io_repo           TYPE REF TO zcl_abapgit_repo
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_news
      RAISING
        zcx_abapgit_exception .
    METHODS get_log
      RETURNING
        VALUE(rt_log) TYPE ty_logs .
    METHODS has_news
      RETURNING
        VALUE(rv_boolean) TYPE abap_bool .
    METHODS has_important
      RETURNING
        VALUE(rv_boolean) TYPE abap_bool .
    METHODS has_updates
      RETURNING
        VALUE(rv_boolean) TYPE abap_bool .
    METHODS has_unseen
      RETURNING
        VALUE(rv_boolean) TYPE abap_bool .
    METHODS constructor
      IMPORTING
        !iv_rawdata          TYPE xstring
        !iv_lastseen_version TYPE string
        !iv_current_version  TYPE string
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_log TYPE ty_logs .
    DATA mv_current_version TYPE string .
    DATA mv_lastseen_version TYPE string .
    DATA mv_latest_version TYPE string .

    METHODS latest_version
      RETURNING
        VALUE(rv_version) TYPE string .
    CLASS-METHODS parse_line
      IMPORTING
        !iv_line            TYPE string
        !iv_current_version TYPE string
      RETURNING
        VALUE(rs_log)       TYPE ty_log .
    CLASS-METHODS parse
      IMPORTING
        !it_lines           TYPE string_table
        !iv_current_version TYPE string
      RETURNING
        VALUE(rt_log)       TYPE ty_logs .
ENDCLASS.



CLASS zcl_abapgit_news IMPLEMENTATION.


  METHOD constructor.

    DATA: lt_lines    TYPE string_table,
          lv_string   TYPE string,
          ls_log_line LIKE LINE OF mt_log.

    " Validate params
    mv_current_version  = zcl_abapgit_version=>normalize( iv_current_version ).
    mv_lastseen_version = zcl_abapgit_version=>normalize( iv_lastseen_version ).
    IF mv_current_version IS INITIAL.
      RETURN. " Internal format of program version is not correct -> abort parsing
    ENDIF.

    lv_string = zcl_abapgit_convert=>xstring_to_string_utf8( iv_rawdata ).
    lt_lines  = zcl_abapgit_convert=>split_string( lv_string ).
    mt_log    = parse( it_lines = lt_lines
                       iv_current_version = mv_current_version ).

    READ TABLE mt_log INTO ls_log_line INDEX 1.
    mv_latest_version = ls_log_line-version. " Empty if not found

  ENDMETHOD.


  METHOD create.

    CONSTANTS: " TODO refactor
      lc_log_path        TYPE string VALUE '/',
      lc_log_filename    TYPE string VALUE 'changelog*',
      lc_log_filename_up TYPE string VALUE 'CHANGELOG*'.

    DATA: lo_apack            TYPE REF TO zcl_abapgit_apack_reader,
          lt_remote           TYPE zif_abapgit_git_definitions=>ty_files_tt,
          lv_version          TYPE string,
          lv_last_seen        TYPE string,
          lv_url              TYPE string,
          lo_repo_online      TYPE REF TO zcl_abapgit_repo_online,
          lv_version_constant TYPE zif_abapgit_dot_abapgit=>ty_dot_abapgit-version_constant.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF lt_remote.


    IF io_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    lo_repo_online ?= io_repo.
    lv_url          = lo_repo_online->get_url( ).

    lo_apack = io_repo->get_dot_apack( ).
    IF lo_apack IS BOUND.
      lv_version = lo_apack->get_manifest_descriptor( )-version.
    ENDIF.

    IF lv_version IS INITIAL.
      TRY.
          lv_version_constant = io_repo->get_dot_abapgit( )->get_version_constant( ).
          IF lv_version_constant IS NOT INITIAL.
            lv_version = zcl_abapgit_version=>get_version_constant_value( lv_version_constant ).
          ENDIF.
        CATCH zcx_abapgit_exception.
          CLEAR lv_version.
      ENDTRY.
    ENDIF.

    IF lv_version IS INITIAL.
      RETURN.
    ENDIF.

    lv_last_seen = zcl_abapgit_persistence_user=>get_instance( )->get_repo_last_change_seen( lv_url ).

    TRY. " Find changelog
        lt_remote = io_repo->get_files_remote( ).
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    LOOP AT lt_remote ASSIGNING <ls_file> WHERE path = lc_log_path
                                            AND ( filename CP lc_log_filename OR filename CP lc_log_filename_up ).

      CREATE OBJECT ro_instance
        EXPORTING
          iv_rawdata          = <ls_file>-data
          iv_current_version  = lv_version
          iv_lastseen_version = zcl_abapgit_version=>normalize( lv_last_seen ).

      EXIT.

    ENDLOOP.

    IF ro_instance IS BOUND AND lv_last_seen <> ro_instance->latest_version( ).
      zcl_abapgit_persistence_user=>get_instance( )->set_repo_last_change_seen(
        iv_url     = lv_url
        iv_version = ro_instance->latest_version( ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_log.
    rt_log = mt_log.
  ENDMETHOD.


  METHOD has_important.
    READ TABLE mt_log WITH KEY is_important = abap_true TRANSPORTING NO FIELDS.
    rv_boolean = boolc( sy-subrc IS INITIAL ).
  ENDMETHOD.


  METHOD has_news.
    rv_boolean = boolc( lines( mt_log ) > 0 ).
  ENDMETHOD.


  METHOD has_unseen.
    rv_boolean = boolc( zcl_abapgit_version=>compare(
      iv_a = mv_latest_version
      iv_b = mv_lastseen_version ) > 0 ).
  ENDMETHOD.


  METHOD has_updates.
    rv_boolean = boolc( zcl_abapgit_version=>compare(
      iv_a = mv_latest_version
      iv_b = mv_current_version ) > 0 ).
  ENDMETHOD.


  METHOD latest_version.
    rv_version = mv_latest_version.
  ENDMETHOD.


  METHOD parse.

    DATA: lv_tail                TYPE i,
          lv_first_version_found TYPE abap_bool,
          lv_version             TYPE string,
          ls_log                 LIKE LINE OF rt_log.

    FIELD-SYMBOLS: <lv_line> LIKE LINE OF it_lines.


    LOOP AT it_lines ASSIGNING <lv_line>.
      ls_log = parse_line( iv_line = <lv_line>
                           iv_current_version = iv_current_version ).

      " Skip until first version head and Skip empty lines
      CHECK ls_log IS NOT INITIAL AND
            ( lv_first_version_found = abap_true OR ls_log-version IS NOT INITIAL ).

      IF lv_first_version_found = abap_false.
        lv_first_version_found = abap_true.
        IF zcl_abapgit_version=>compare( iv_a = ls_log-version
                                         iv_b = iv_current_version ) <= 0.
          lv_tail = c_tail_length. " Display some last versions if no updates
        ENDIF.
      ENDIF.

      IF ls_log-is_header = abap_true.
        "Skip everything below current version or show tail news
        IF zcl_abapgit_version=>compare( iv_a = ls_log-version
                                         iv_b = iv_current_version ) <= 0.
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

  ENDMETHOD.


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
      lv_version        = zcl_abapgit_version=>normalize( lv_version ).
      rs_log-version    = lv_version.
      rs_log-is_header  = abap_true.
      rs_log-pos_to_cur = zcl_abapgit_version=>compare( iv_a = lv_version
                                                        iv_b = iv_current_version ).
    ELSE.
      FIND FIRST OCCURRENCE OF REGEX '^\s*!' IN iv_line.
      rs_log-is_important = boolc( sy-subrc IS INITIAL ). " Change is important
    ENDIF.

    rs_log-text = iv_line.

  ENDMETHOD.
ENDCLASS.
