*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_UTIL
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_time DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_time DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: ty_unixtime TYPE c LENGTH 16.

    CLASS-METHODS get
      RETURNING VALUE(rv_time) TYPE ty_unixtime
      RAISING   lcx_exception.

  PRIVATE SECTION.
    CONSTANTS: c_epoch TYPE datum VALUE '19700101'.

ENDCLASS.                    "lcl_time DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_time IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_time IMPLEMENTATION.

  METHOD get.

    DATA: lv_i       TYPE i,
          lv_tz      TYPE tznzone,
          lv_utcdiff TYPE tznutcdiff,
          lv_utcsign TYPE tznutcsign.


    lv_i = sy-datum - c_epoch.
    lv_i = lv_i * 86400.
    lv_i = lv_i + sy-uzeit.

    CALL FUNCTION 'TZON_GET_OS_TIMEZONE'
      IMPORTING
        ef_timezone = lv_tz.

    CALL FUNCTION 'TZON_GET_OFFSET'
      EXPORTING
        if_timezone      = lv_tz
        if_local_date    = sy-datum
        if_local_time    = sy-uzeit
      IMPORTING
        ef_utcdiff       = lv_utcdiff
        ef_utcsign       = lv_utcsign
      EXCEPTIONS
        conversion_error = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'Timezone error' ).
    ENDIF.

    CASE lv_utcsign.
      WHEN '+'.
        lv_i = lv_i - lv_utcdiff.
      WHEN '-'.
        lv_i = lv_i + lv_utcdiff.
    ENDCASE.

    rv_time = lv_i.
    CONDENSE rv_time.
    rv_time+11 = lv_utcsign.
    rv_time+12 = lv_utcdiff.

  ENDMETHOD.                    "get

ENDCLASS.                    "lcl_time IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_convert DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_convert DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS bitbyte_to_int
      IMPORTING iv_bits       TYPE clike
      RETURNING VALUE(rv_int) TYPE i.

    CLASS-METHODS x_to_bitbyte
      IMPORTING iv_x              TYPE x
      RETURNING VALUE(rv_bitbyte) TYPE ty_bitbyte.

    CLASS-METHODS string_to_xstring_utf8
      IMPORTING iv_string         TYPE string
      RETURNING VALUE(rv_xstring) TYPE xstring.

    CLASS-METHODS xstring_to_string_utf8
      IMPORTING iv_data          TYPE xstring
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS xstring_to_int
      IMPORTING iv_xstring  TYPE xstring
      RETURNING VALUE(rv_i) TYPE i
      RAISING   lcx_exception.

    CLASS-METHODS int_to_xstring
      IMPORTING iv_i              TYPE i
                iv_length         TYPE i
      RETURNING VALUE(rv_xstring) TYPE xstring.

ENDCLASS.                    "lcl_convert DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_convert IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_convert IMPLEMENTATION.

  METHOD int_to_xstring.

    DATA: lv_x TYPE x LENGTH 4.


    ASSERT iv_length = 4. " other cases not implemented

    lv_x = iv_i.
    rv_xstring = lv_x.

  ENDMETHOD.                    "int_to_xstring

  METHOD xstring_to_int.

    DATA: lv_xstring TYPE xstring,
          lv_x       TYPE x.


    lv_xstring = iv_xstring.
    WHILE xstrlen( lv_xstring ) > 0.
      lv_x = lv_xstring(1).
      rv_i = rv_i * 256 + lv_x.
      lv_xstring = lv_xstring+1.
    ENDWHILE.

  ENDMETHOD.                    "xstring_to_int

  METHOD xstring_to_string_utf8.

    DATA: lv_len TYPE i,
          lo_obj TYPE REF TO cl_abap_conv_in_ce.


    TRY.
        lo_obj = cl_abap_conv_in_ce=>create(
            input    = iv_data
            encoding = 'UTF-8' ).
        lv_len = xstrlen( iv_data ).

        lo_obj->read( EXPORTING n    = lv_len
                      IMPORTING data = rv_string ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.                  "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "xstring_to_string_utf8

  METHOD string_to_xstring_utf8.

    DATA: lo_obj TYPE REF TO cl_abap_conv_out_ce.


    TRY.
        lo_obj = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

        lo_obj->convert( EXPORTING data = iv_string
                         IMPORTING buffer = rv_xstring ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.                  "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.                    "string_to_xstring_utf8

  METHOD bitbyte_to_int.

    DATA: lv_bits TYPE string.


    lv_bits = iv_bits.

    rv_int = 0.
    WHILE strlen( lv_bits ) > 0.
      rv_int = rv_int * 2.
      IF lv_bits(1) = '1'.
        rv_int = rv_int + 1.
      ENDIF.
      lv_bits = lv_bits+1.
    ENDWHILE.

  ENDMETHOD.                    "bitbyte_to_int

  METHOD x_to_bitbyte.

    DATA: lv_b TYPE n.

    CLEAR rv_bitbyte.

    DO 8 TIMES.
      GET BIT sy-index OF iv_x INTO lv_b.
      CONCATENATE rv_bitbyte lv_b INTO rv_bitbyte.
    ENDDO.

  ENDMETHOD.                    "x_to_bitbyte

ENDCLASS.                    "lcl_convert IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_hash DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_hash DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: ty_adler32 TYPE x LENGTH 4.

    CLASS-METHODS adler32
      IMPORTING iv_xstring         TYPE xstring
      RETURNING VALUE(rv_checksum) TYPE ty_adler32.

    CLASS-METHODS sha1
      IMPORTING iv_type        TYPE ty_type
                iv_data        TYPE xstring
      RETURNING VALUE(rv_sha1) TYPE ty_sha1
      RAISING   lcx_exception.

    CLASS-METHODS sha1_raw
      IMPORTING iv_data        TYPE xstring
      RETURNING VALUE(rv_sha1) TYPE ty_sha1
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_hash DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_hash IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_hash IMPLEMENTATION.

  METHOD adler32.

    CONSTANTS: lc_adler TYPE i VALUE 65521.

    DATA: lv_index TYPE i,
          lv_a     TYPE i VALUE 1,
          lv_b     TYPE i VALUE 0,
          lv_x     TYPE x LENGTH 2,
          lv_ca    TYPE c LENGTH 4,
          lv_cb    TYPE c LENGTH 4,
          lv_char8 TYPE c LENGTH 8.


    DO xstrlen( iv_xstring ) TIMES.
      lv_index = sy-index - 1.

      lv_a = ( lv_a + iv_xstring+lv_index(1) ) MOD lc_adler.
      lv_b = ( lv_b + lv_a ) MOD lc_adler.
    ENDDO.

    lv_x = lv_a.
    lv_ca = lv_x.

    lv_x = lv_b.
    lv_cb = lv_x.

    CONCATENATE lv_cb lv_ca INTO lv_char8.

    rv_checksum = lv_char8.

  ENDMETHOD.                                                "adler32

  METHOD sha1_raw.

    DATA: lv_hash TYPE hash160.


    CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
      EXPORTING
        data           = iv_data
      IMPORTING
        hash           = lv_hash
      EXCEPTIONS
        unknown_alg    = 1
        param_error    = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'Error while calculating SHA1' ).
    ENDIF.

    rv_sha1 = lv_hash.

    TRANSLATE rv_sha1 TO LOWER CASE.

  ENDMETHOD.                                                "sha1_raw

  METHOD sha1.

    DATA: lv_len     TYPE i,
          lv_char10  TYPE c LENGTH 10,
          lv_string  TYPE string,
          lv_xstring TYPE xstring.


    lv_len = xstrlen( iv_data ).
    lv_char10 = lv_len.
    CONDENSE lv_char10.
    CONCATENATE iv_type lv_char10 INTO lv_string SEPARATED BY space.
    lv_xstring = lcl_convert=>string_to_xstring_utf8( lv_string ).

    lv_string = lv_xstring.
    CONCATENATE lv_string '00' INTO lv_string.
    lv_xstring = lv_string.

    CONCATENATE lv_xstring iv_data INTO lv_xstring IN BYTE MODE.

    rv_sha1 = sha1_raw( lv_xstring ).

  ENDMETHOD.                                                "sha1

ENDCLASS.                    "lcl_hash IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_url DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_url DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS host
      IMPORTING iv_repo        TYPE string
      RETURNING VALUE(rv_host) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS name
      IMPORTING iv_repo        TYPE string
      RETURNING VALUE(rv_name) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS path_name
      IMPORTING iv_repo             TYPE string
      RETURNING VALUE(rv_path_name) TYPE string
      RAISING   lcx_exception.

    CLASS-METHODS split_file_location
      IMPORTING iv_fullpath TYPE string
      EXPORTING ev_path     TYPE string
                ev_filename TYPE string.

  PRIVATE SECTION.
    CLASS-METHODS regex
      IMPORTING iv_repo TYPE string
      EXPORTING ev_host TYPE string
                ev_path TYPE string
                ev_name TYPE string
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_repo DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_url IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_url IMPLEMENTATION.

  METHOD split_file_location.

    DATA: lv_cnt TYPE i,
          lv_off TYPE i,
          lv_len TYPE i.

    FIND FIRST OCCURRENCE OF REGEX '^/(.*/)?' IN iv_fullpath
      MATCH COUNT lv_cnt
      MATCH OFFSET lv_off
      MATCH LENGTH lv_len.

    IF lv_cnt > 0.
      ev_path     = iv_fullpath+0(lv_len).
      ev_filename = iv_fullpath+lv_len.
    ELSE.
      CLEAR ev_path.
      ev_filename = iv_fullpath.
    ENDIF.

  ENDMETHOD.  "split_file_location

  METHOD host.
    regex( EXPORTING iv_repo = iv_repo
           IMPORTING ev_host = rv_host ).
  ENDMETHOD.                    "host

  METHOD name.
    regex( EXPORTING iv_repo = iv_repo
           IMPORTING ev_name = rv_name ).
  ENDMETHOD.                    "short_name

  METHOD path_name.

    DATA: lv_host TYPE string.

    FIND REGEX '(.*://[^/]*)(.*)' IN iv_repo
      SUBMATCHES lv_host rv_path_name.

  ENDMETHOD.                    "path_name

  METHOD regex.

    FIND REGEX '(.*://[^/]*)(.*/)([^\.]*)[\.git]?' IN iv_repo
      SUBMATCHES ev_host ev_path ev_name.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'Malformed URL' ).
    ENDIF.

  ENDMETHOD.                    "url

ENDCLASS.                    "lcl_repo IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_diff DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_diff DEFINITION FINAL.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF c_diff,
                 insert TYPE c LENGTH 1 VALUE 'I',
                 delete TYPE c LENGTH 1 VALUE 'D',
                 update TYPE c LENGTH 1 VALUE 'U',
               END OF c_diff.

    TYPES: BEGIN OF ty_diff,
             local_line  TYPE c LENGTH 6,
             local       TYPE string,
             result      TYPE c LENGTH 1,
             remote_line TYPE c LENGTH 6,
             remote      TYPE string,
             short       TYPE abap_bool,
             beacon      TYPE i,
           END OF ty_diff.
    TYPES:  ty_diffs_tt TYPE STANDARD TABLE OF ty_diff WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_count,
             insert TYPE i,
             delete TYPE i,
             update TYPE i,
           END OF ty_count.

    DATA mt_beacons TYPE ty_string_tt READ-ONLY.

* assumes data is UTF8 based with newlines
* only works with lines up to 255 characters
    METHODS constructor
      IMPORTING iv_local  TYPE xstring
                iv_remote TYPE xstring.

    METHODS get
      RETURNING VALUE(rt_diff) TYPE ty_diffs_tt.

    METHODS stats
      RETURNING VALUE(rs_count) TYPE ty_count.

  PRIVATE SECTION.
    DATA mt_diff     TYPE ty_diffs_tt.
    DATA ms_stats    TYPE ty_count.

    CLASS-METHODS:
      unpack
        IMPORTING iv_local  TYPE xstring
                  iv_remote TYPE xstring
        EXPORTING et_local  TYPE abaptxt255_tab
                  et_remote TYPE abaptxt255_tab,
      render
        IMPORTING it_local       TYPE abaptxt255_tab
                  it_remote      TYPE abaptxt255_tab
                  it_delta       TYPE vxabapt255_tab
        RETURNING VALUE(rt_diff) TYPE ty_diffs_tt,
      compute
        IMPORTING it_local        TYPE abaptxt255_tab
                  it_remote       TYPE abaptxt255_tab
        RETURNING VALUE(rt_delta) TYPE vxabapt255_tab.

    METHODS:
      calculate_line_num_and_stats,
      map_beacons,
      shortlist.

ENDCLASS.                    "lcl_diff DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_diff IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_diff IMPLEMENTATION.

  METHOD get.
    rt_diff = mt_diff.
  ENDMETHOD.                    "get

  METHOD stats.
    rs_count = ms_stats.
  ENDMETHOD.                    "count

  METHOD unpack.

    DATA: lv_local  TYPE string,
          lv_remote TYPE string.


    lv_local  = lcl_convert=>xstring_to_string_utf8( iv_local ).
    lv_remote = lcl_convert=>xstring_to_string_utf8( iv_remote ).

    SPLIT lv_local  AT gc_newline INTO TABLE et_local.
    SPLIT lv_remote AT gc_newline INTO TABLE et_remote.

  ENDMETHOD.                    "unpack

  METHOD compute.

    DATA: lt_trdirtab_old TYPE TABLE OF trdir,
          lt_trdirtab_new TYPE TABLE OF trdir,
          lt_trdir_delta  TYPE TABLE OF xtrdir.


    CALL FUNCTION 'SVRS_COMPUTE_DELTA_REPS'
      TABLES
        texttab_old  = it_remote
        texttab_new  = it_local
        trdirtab_old = lt_trdirtab_old
        trdirtab_new = lt_trdirtab_new
        trdir_delta  = lt_trdir_delta
        text_delta   = rt_delta.

  ENDMETHOD.                    "compute

  METHOD shortlist.

    DATA: lv_index TYPE i.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.

    IF lines( mt_diff ) < 100.
      LOOP AT mt_diff ASSIGNING <ls_diff>.
        <ls_diff>-short = abap_true.
      ENDLOOP.
    ELSE.
      LOOP AT mt_diff TRANSPORTING NO FIELDS
          WHERE NOT result IS INITIAL AND short = abap_false.
        lv_index = sy-tabix.

        DO 10 TIMES. " Backward
          READ TABLE mt_diff INDEX ( lv_index - sy-index ) ASSIGNING <ls_diff>.
          IF sy-subrc <> 0 OR <ls_diff>-short = abap_true. " tab bound or prev marker
            EXIT.
          ENDIF.
          <ls_diff>-short = abap_true.
        ENDDO.

        DO 10 TIMES. " Forward
          READ TABLE mt_diff INDEX ( lv_index + sy-index - 1 ) ASSIGNING <ls_diff>.
          IF sy-subrc <> 0. " tab bound reached
            EXIT.
          ENDIF.
          CHECK <ls_diff>-short = abap_false. " skip marked
          <ls_diff>-short = abap_true.
        ENDDO.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.                " shortlist

  METHOD calculate_line_num_and_stats.

    DATA: lv_local  TYPE i VALUE 1,
          lv_remote TYPE i VALUE 1.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.


    LOOP AT mt_diff ASSIGNING <ls_diff>.
      <ls_diff>-local_line = lv_local.
      <ls_diff>-remote_line = lv_remote.

      CASE <ls_diff>-result. " Line nums
        WHEN c_diff-delete.
          lv_remote = lv_remote + 1.
          CLEAR <ls_diff>-local_line.
        WHEN c_diff-insert.
          lv_local = lv_local + 1.
          CLEAR <ls_diff>-remote_line.
        WHEN OTHERS.
          lv_local = lv_local + 1.
          lv_remote = lv_remote + 1.
      ENDCASE.

      CASE <ls_diff>-result. " Stats
        WHEN c_diff-insert.
          ms_stats-insert = ms_stats-insert + 1.
        WHEN c_diff-delete.
          ms_stats-delete = ms_stats-delete + 1.
        WHEN c_diff-update.
          ms_stats-update = ms_stats-update + 1.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.                " calculate_line_num_and_stats

  METHOD map_beacons.

    DEFINE _add_regex.
      CREATE OBJECT lo_regex
        EXPORTING pattern     = &1
                  ignore_case = abap_true ##NO_TEXT.
      APPEND lo_regex TO lt_regex_set.
    END-OF-DEFINITION.

    DATA: lv_beacon    TYPE i,
          lv_offs      TYPE i,
          lv_code_line TYPE string,
          lo_regex     TYPE REF TO cl_abap_regex,
          lt_regex_set TYPE TABLE OF REF TO cl_abap_regex.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.


    _add_regex '^\s*(CLASS|FORM|MODULE|REPORT)\s'.
    _add_regex '^\s*START-OF-'.
    _add_regex '^\s*INITIALIZATION(\s|\.)'.

    LOOP AT mt_diff ASSIGNING <ls_diff>.
      <ls_diff>-beacon = lv_beacon.
      LOOP AT lt_regex_set INTO lo_regex.
        FIND FIRST OCCURRENCE OF REGEX lo_regex IN <ls_diff>-local.
        IF sy-subrc = 0. " Match
          lv_code_line = <ls_diff>-local.

          " Get rid of comments
          FIND FIRST OCCURRENCE OF '.' IN lv_code_line MATCH OFFSET lv_offs.
          IF sy-subrc = 0.
            lv_code_line = lv_code_line(lv_offs).
          ENDIF.

          APPEND lv_code_line TO mt_beacons.
          lv_beacon        = sy-tabix.
          <ls_diff>-beacon = lv_beacon.
          EXIT. "Loop
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.                " map_beacons

  METHOD constructor.

    DATA: lt_delta  TYPE vxabapt255_tab,
          lt_local  TYPE abaptxt255_tab,
          lt_remote TYPE abaptxt255_tab.


    unpack( EXPORTING iv_local  = iv_local
                      iv_remote = iv_remote
            IMPORTING et_local  = lt_local
                      et_remote = lt_remote ).

    lt_delta = compute( it_local  = lt_local
                        it_remote = lt_remote ).

    mt_diff = render( it_local  = lt_local
                      it_remote = lt_remote
                      it_delta  = lt_delta ).

    calculate_line_num_and_stats( ).
    map_beacons( ).
    shortlist( ).

  ENDMETHOD.                    "diff

  METHOD render.

    DEFINE _append.
      CLEAR ls_diff.
      ls_diff-local = &1.
      ls_diff-result = &2.
      ls_diff-remote = &3.
      APPEND ls_diff TO rt_diff.
    END-OF-DEFINITION.

    DATA: lv_rindex TYPE i VALUE 1,
          lv_lindex TYPE i VALUE 1,
          ls_local  LIKE LINE OF it_local,
          ls_remote LIKE LINE OF it_remote,
          ls_diff   LIKE LINE OF rt_diff,
          lt_delta  LIKE it_delta,
          ls_delta  LIKE LINE OF it_delta.


    lt_delta = it_delta.

    DO.
      READ TABLE lt_delta INTO ls_delta WITH KEY number = lv_rindex.
      IF sy-subrc = 0.
        DELETE lt_delta INDEX sy-tabix.

        CASE ls_delta-vrsflag.
          WHEN c_diff-delete.
            _append '' c_diff-delete ls_delta-line.
            lv_rindex = lv_rindex + 1.
          WHEN c_diff-insert.
            _append ls_delta-line c_diff-insert ''.
            lv_lindex = lv_lindex + 1.
          WHEN c_diff-update.
            CLEAR ls_local.
            READ TABLE it_local INTO ls_local INDEX lv_lindex.
            ASSERT sy-subrc = 0.
            _append ls_local c_diff-update ls_delta-line.
            lv_lindex = lv_lindex + 1.
            lv_rindex = lv_rindex + 1.
          WHEN OTHERS.
            ASSERT 0 = 1.
        ENDCASE.
      ELSE.
        CLEAR ls_local.
        READ TABLE it_local INTO ls_local INDEX lv_lindex. "#EC CI_SUBRC
        lv_lindex = lv_lindex + 1.
        CLEAR ls_remote.
        READ TABLE it_remote INTO ls_remote INDEX lv_rindex. "#EC CI_SUBRC
        lv_rindex = lv_rindex + 1.
        _append ls_local '' ls_remote.
      ENDIF.

      IF lv_lindex > lines( it_local ) AND lv_rindex > lines( it_remote ).
        EXIT. " current loop
      ENDIF.
    ENDDO.

  ENDMETHOD.                " render

ENDCLASS.                    "lcl_diff IMPLEMENTATION

CLASS lcl_login_manager DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      load
        IMPORTING iv_uri                  TYPE string
                  ii_client               TYPE REF TO if_http_client OPTIONAL
        RETURNING VALUE(rv_authorization) TYPE string
        RAISING   lcx_exception,
      save
        IMPORTING iv_uri    TYPE string
                  ii_client TYPE REF TO if_http_client
        RAISING   lcx_exception,
      clear,
      set
        IMPORTING iv_uri         TYPE string
                  iv_username    TYPE string
                  iv_password    TYPE string
        RETURNING VALUE(rv_auth) TYPE string
        RAISING   lcx_exception.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_auth,
             uri           TYPE string,
             authorization TYPE string,
           END OF ty_auth.

    CLASS-DATA: gt_auth TYPE TABLE OF ty_auth WITH DEFAULT KEY.

    CLASS-METHODS:
      append
        IMPORTING iv_uri  TYPE string
                  iv_auth TYPE string
        RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_login_manager IMPLEMENTATION.

  METHOD clear.
    CLEAR gt_auth.
  ENDMETHOD.

  METHOD set.

    DATA: lv_concat TYPE string.


    ASSERT NOT iv_uri IS INITIAL.

    IF iv_username IS INITIAL OR iv_password IS INITIAL.
      RETURN.
    ENDIF.

    CONCATENATE iv_username ':' iv_password INTO lv_concat.

    rv_auth = cl_http_utility=>if_http_utility~encode_base64( lv_concat ).

    CONCATENATE 'Basic' rv_auth INTO rv_auth
      SEPARATED BY space ##NO_TEXT.

    append( iv_uri  = iv_uri
            iv_auth = rv_auth ).

  ENDMETHOD.

  METHOD load.

    DATA: ls_auth LIKE LINE OF gt_auth.


    READ TABLE gt_auth INTO ls_auth WITH KEY uri = lcl_url=>host( iv_uri ).
    IF sy-subrc = 0.
      rv_authorization = ls_auth-authorization.

      IF NOT ii_client IS INITIAL.
        ii_client->request->set_header_field(
          name  = 'authorization'
          value = ls_auth-authorization ).                  "#EC NOTEXT
        ii_client->propertytype_logon_popup = ii_client->co_disabled.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD save.

    DATA: lv_auth TYPE string.


    lv_auth = ii_client->request->get_header_field( 'authorization' ). "#EC NOTEXT

    IF NOT lv_auth IS INITIAL.
      append( iv_uri  = iv_uri
              iv_auth = lv_auth ).
    ENDIF.

  ENDMETHOD.

  METHOD append.

    FIELD-SYMBOLS: <ls_auth> LIKE LINE OF gt_auth.


    READ TABLE gt_auth WITH KEY uri = lcl_url=>host( iv_uri )
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO gt_auth ASSIGNING <ls_auth>.
      <ls_auth>-uri           = lcl_url=>host( iv_uri ).
      <ls_auth>-authorization = iv_auth.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_progress DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      show
        IMPORTING
          iv_key            TYPE string
          VALUE(iv_current) TYPE i
          iv_total          TYPE i
          iv_text           TYPE csequence.

  PRIVATE SECTION.
    CLASS-METHODS:
      calc_pct
        IMPORTING iv_current    TYPE i
                  iv_total      TYPE i
        RETURNING VALUE(rv_pct) TYPE i.

ENDCLASS.

CLASS lcl_progress IMPLEMENTATION.

  METHOD show.

    DATA: lv_pct  TYPE i,
          lv_text TYPE string.


    lv_pct = calc_pct( iv_current = iv_current
                       iv_total   = iv_total ).
    CONCATENATE iv_key '-' iv_text INTO lv_text SEPARATED BY space.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_pct
        text       = lv_text.

  ENDMETHOD.

  METHOD calc_pct.

    DATA: lv_f TYPE f.


    lv_f = ( iv_current / iv_total ) * 100.
    rv_pct = lv_f.

    IF rv_pct = 100.
      rv_pct = 99.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_log DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      add
        IMPORTING
          iv_msgv1 TYPE csequence
          iv_msgv2 TYPE csequence OPTIONAL
          iv_msgv3 TYPE csequence OPTIONAL
          iv_msgv4 TYPE csequence OPTIONAL,
      count
        RETURNING VALUE(rv_count) TYPE i,
      to_html
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper,
      show.

  PRIVATE SECTION.
    DATA: mt_log TYPE rs_t_msg.

ENDCLASS.

CLASS lcl_log IMPLEMENTATION.

  METHOD to_html.

    DATA: lv_string TYPE string.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF mt_log.

    CREATE OBJECT ro_html.

    IF count( ) = 0.
      RETURN.
    ENDIF.

    ro_html->add( '<br>' ).
    LOOP AT mt_log ASSIGNING <ls_log>.
      CONCATENATE <ls_log>-msgv1
        <ls_log>-msgv2
        <ls_log>-msgv3
        <ls_log>-msgv4 INTO lv_string SEPARATED BY space.
      ro_html->add( lv_string ).
      ro_html->add( '<br>' ).
    ENDLOOP.
    ro_html->add( '<br>' ).

  ENDMETHOD.

  METHOD add.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF mt_log.

    APPEND INITIAL LINE TO mt_log ASSIGNING <ls_log>.
    <ls_log>-msgty = 'W'.
    <ls_log>-msgid = '00'.
    <ls_log>-msgno = '001'.
    <ls_log>-msgv1 = iv_msgv1.
    <ls_log>-msgv2 = iv_msgv2.
    <ls_log>-msgv3 = iv_msgv3.
    <ls_log>-msgv4 = iv_msgv4.

  ENDMETHOD.

  METHOD show.
    CALL FUNCTION 'RSDC_SHOW_MESSAGES_POPUP'
      EXPORTING
        i_t_msg           = mt_log
        i_txt             = 'Warning'
        i_with_s_on_empty = abap_false
        i_one_msg_direct  = abap_false
        i_one_msg_type_s  = abap_false
        ##no_text.
  ENDMETHOD.

  METHOD count.
    rv_count = lines( mt_log ).
  ENDMETHOD.

ENDCLASS.