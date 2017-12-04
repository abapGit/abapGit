*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_UTIL
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_language DEFINITION
*----------------------------------------------------------------------*
* This helper class is used to set and restore the current language.
* As some of the SAP functions used rely on SY-LANGU containing the
* master language, this class is used to temporarily change and then
* restore the value of SY-LANGU.
*----------------------------------------------------------------------*
CLASS lcl_language DEFINITION.

  PUBLIC SECTION.

    CLASS-DATA:
      current_language TYPE langu READ-ONLY,
      login_language   TYPE langu READ-ONLY.

    CLASS-METHODS:
      class_constructor,
      restore_login_language,
      set_current_language
        IMPORTING
          iv_language TYPE langu.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_language IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_language IMPLEMENTATION.

  METHOD class_constructor.
    DATA lv_dummy TYPE string.

    GET LOCALE LANGUAGE login_language COUNTRY lv_dummy MODIFIER lv_dummy.
  ENDMETHOD.

  METHOD restore_login_language.
    SET LOCALE LANGUAGE login_language.
  ENDMETHOD.

  METHOD set_current_language.
    SET LOCALE LANGUAGE iv_language.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_state DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      reduce
        IMPORTING
          iv_cur  TYPE char1
        CHANGING
          cv_prev TYPE char1.

ENDCLASS.

CLASS lcl_state IMPLEMENTATION.

  METHOD reduce.

    IF cv_prev = iv_cur OR iv_cur IS INITIAL.
      RETURN. " No change
    ELSEIF cv_prev IS INITIAL.
      cv_prev = iv_cur.
    ELSE.
      cv_prev = zif_abapgit_definitions=>gc_state-mixed.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

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
      RETURNING VALUE(rv_bitbyte) TYPE zif_abapgit_definitions=>ty_bitbyte.

    CLASS-METHODS string_to_xstring_utf8
      IMPORTING iv_string         TYPE string
      RETURNING VALUE(rv_xstring) TYPE xstring.

    CLASS-METHODS xstring_to_string_utf8
      IMPORTING iv_data          TYPE xstring
      RETURNING VALUE(rv_string) TYPE string.

    CLASS-METHODS xstring_to_int
      IMPORTING iv_xstring  TYPE xstring
      RETURNING VALUE(rv_i) TYPE i
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS int_to_xstring4
      IMPORTING iv_i              TYPE i
      RETURNING VALUE(rv_xstring) TYPE xstring.

    CLASS-METHODS split_string
      IMPORTING iv_string       TYPE string
      RETURNING VALUE(rt_lines) TYPE string_table.

ENDCLASS.                    "lcl_convert DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_convert IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_convert IMPLEMENTATION.

  METHOD int_to_xstring4.
* returns xstring of length 4 containing the integer value iv_i

    DATA: lv_x TYPE x LENGTH 4.


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

  METHOD split_string.

    FIND FIRST OCCURRENCE OF cl_abap_char_utilities=>cr_lf IN iv_string.

    " Convert string into table depending on separator type CR_LF vs. LF
    IF sy-subrc = 0.
      SPLIT iv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE rt_lines.
    ELSE.
      SPLIT iv_string AT cl_abap_char_utilities=>newline INTO TABLE rt_lines.
    ENDIF.

  ENDMETHOD.                    "split_string

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
      IMPORTING iv_type        TYPE zif_abapgit_definitions=>ty_type
                iv_data        TYPE xstring
      RETURNING VALUE(rv_sha1) TYPE zif_abapgit_definitions=>ty_sha1
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS sha1_raw
      IMPORTING iv_data        TYPE xstring
      RETURNING VALUE(rv_sha1) TYPE zif_abapgit_definitions=>ty_sha1
      RAISING   zcx_abapgit_exception.

ENDCLASS.                    "lcl_hash DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_hash IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_hash IMPLEMENTATION.

  METHOD adler32.

    CONSTANTS: lc_adler TYPE i VALUE 65521,
               lc_max_b TYPE i VALUE 1800000000.

    DATA: lv_index TYPE i,
          lv_a     TYPE i VALUE 1,
          lv_b     TYPE i VALUE 0,
          lv_x     TYPE x LENGTH 2,
          lv_ca    TYPE c LENGTH 4,
          lv_cb    TYPE c LENGTH 4,
          lv_char8 TYPE c LENGTH 8.


    DO xstrlen( iv_xstring ) TIMES.
      lv_index = sy-index - 1.

      lv_a = lv_a + iv_xstring+lv_index(1).
      lv_b = lv_b + lv_a.

* delay the MOD operation until the integer might overflow
* articles describe 5552 additions are allowed, but this assumes unsigned integers
* instead of allowing a fixed number of additions before running MOD, then
* just compare value of lv_b, this is 1 operation less than comparing and adding
      IF lv_b > lc_max_b.
        lv_a = lv_a MOD lc_adler.
        lv_b = lv_b MOD lc_adler.
      ENDIF.
    ENDDO.

    lv_a = lv_a MOD lc_adler.
    lv_b = lv_b MOD lc_adler.

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
      zcx_abapgit_exception=>raise( 'Error while calculating SHA1' ).
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

CLASS lcl_path DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS split_file_location
      IMPORTING iv_fullpath TYPE string
      EXPORTING ev_path     TYPE string
                ev_filename TYPE string.

    CLASS-METHODS is_root
      IMPORTING iv_path       TYPE string
      RETURNING VALUE(rv_yes) TYPE abap_bool.

    CLASS-METHODS is_subdir
      IMPORTING iv_path       TYPE string
                iv_parent     TYPE string
      RETURNING VALUE(rv_yes) TYPE abap_bool.

    CLASS-METHODS change_dir
      IMPORTING iv_cur_dir     TYPE string
                iv_cd          TYPE string
      RETURNING VALUE(rv_path) TYPE string.

    CLASS-METHODS get_filename_from_syspath
      IMPORTING iv_path            TYPE string
      RETURNING VALUE(rv_filename) TYPE string.

ENDCLASS. "lcl_path

CLASS lcl_path IMPLEMENTATION.

  METHOD split_file_location.

    DATA: lv_cnt TYPE i,
          lv_len TYPE i.

    FIND FIRST OCCURRENCE OF REGEX '^/(.*/)?' IN iv_fullpath
      MATCH COUNT lv_cnt
      MATCH LENGTH lv_len.

    IF lv_cnt > 0.
      ev_path     = iv_fullpath+0(lv_len).
      ev_filename = iv_fullpath+lv_len.
    ELSE.
      CLEAR ev_path.
      ev_filename = iv_fullpath.
    ENDIF.

  ENDMETHOD.  "split_file_location

  METHOD is_root.
    rv_yes = boolc( iv_path = '/' ).
  ENDMETHOD. "is_root

  METHOD is_subdir.

    DATA lv_len  TYPE i.
    DATA lv_last TYPE i.

    lv_len  = strlen( iv_parent ).
    lv_last = lv_len - 1.
    rv_yes  = boolc( strlen( iv_path ) > lv_len
                 AND iv_path+0(lv_len) = iv_parent
                 AND ( iv_parent+lv_last(1) = '/' OR iv_path+lv_len(1) = '/' ) ).

  ENDMETHOD. "is_subdir

  METHOD change_dir.

    DATA: lv_last TYPE i,
          lv_temp TYPE string.

    lv_last = strlen( iv_cur_dir ) - 1.

    IF iv_cd = '' OR iv_cd = '.'. " No change
      rv_path = iv_cur_dir.
    ELSEIF iv_cd+0(1) = '/'.      " Absolute path
      rv_path = iv_cd.
    ELSEIF iv_cd = '..'.          " CD back
      IF iv_cur_dir = '/' OR iv_cur_dir = ''. " Back from root = root
        rv_path = iv_cur_dir.
      ELSE.
        lv_temp = reverse( iv_cur_dir ).
        IF lv_temp+0(1) = '/'.
          SHIFT lv_temp BY 1 PLACES LEFT.
        ENDIF.
        SHIFT lv_temp UP TO '/' LEFT.
        rv_path = reverse( lv_temp ).
      ENDIF.
    ELSEIF iv_cur_dir+lv_last(1) = '/'.  " Append cd to cur_dir separated by /
      rv_path = iv_cur_dir && iv_cd.
    ELSE.
      rv_path = iv_cur_dir && '/' && iv_cd.
    ENDIF.

    " TODO: improve logic and cases

  ENDMETHOD. "change_dir

  METHOD get_filename_from_syspath.

    DATA: lv_split TYPE c LENGTH 1,
          lv_index TYPE i,
          lt_split TYPE TABLE OF string.

    " filename | c:\filename | /dir/filename | \\server\filename
    IF iv_path CA '/'.
      lv_split = '/'.
    ELSE.
      lv_split = '\'.
    ENDIF.

    SPLIT iv_path AT lv_split INTO TABLE lt_split.

    lv_index = lines( lt_split ).

    READ TABLE lt_split INDEX lv_index INTO rv_filename.

  ENDMETHOD.  " get_filename_from_syspath.

ENDCLASS. "lcl_path

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
             new_num TYPE c LENGTH 6,
             new     TYPE string,
             result  TYPE c LENGTH 1,
             old_num TYPE c LENGTH 6,
             old     TYPE string,
             short   TYPE abap_bool,
             beacon  TYPE i,
           END OF ty_diff.
    TYPES:  ty_diffs_tt TYPE STANDARD TABLE OF ty_diff WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_count,
             insert TYPE i,
             delete TYPE i,
             update TYPE i,
           END OF ty_count.

    DATA mt_beacons TYPE zif_abapgit_definitions=>ty_string_tt READ-ONLY.

* assumes data is UTF8 based with newlines
* only works with lines up to 255 characters
    METHODS constructor
      IMPORTING iv_new TYPE xstring
                iv_old TYPE xstring.

    METHODS get
      RETURNING VALUE(rt_diff) TYPE ty_diffs_tt.

    METHODS stats
      RETURNING VALUE(rs_count) TYPE ty_count.

  PRIVATE SECTION.
    DATA mt_diff     TYPE ty_diffs_tt.
    DATA ms_stats    TYPE ty_count.

    CLASS-METHODS:
      unpack
        IMPORTING iv_new TYPE xstring
                  iv_old TYPE xstring
        EXPORTING et_new TYPE abaptxt255_tab
                  et_old TYPE abaptxt255_tab,
      render
        IMPORTING it_new         TYPE abaptxt255_tab
                  it_old         TYPE abaptxt255_tab
                  it_delta       TYPE vxabapt255_tab
        RETURNING VALUE(rt_diff) TYPE ty_diffs_tt,
      compute
        IMPORTING it_new          TYPE abaptxt255_tab
                  it_old          TYPE abaptxt255_tab
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

    DATA: lv_new TYPE string,
          lv_old TYPE string.


    lv_new = lcl_convert=>xstring_to_string_utf8( iv_new ).
    lv_old = lcl_convert=>xstring_to_string_utf8( iv_old ).

    SPLIT lv_new AT zif_abapgit_definitions=>gc_newline INTO TABLE et_new.
    SPLIT lv_old AT zif_abapgit_definitions=>gc_newline INTO TABLE et_old.

  ENDMETHOD.                    "unpack

  METHOD compute.

    DATA: lt_trdirtab_old TYPE TABLE OF trdir,
          lt_trdirtab_new TYPE TABLE OF trdir,
          lt_trdir_delta  TYPE TABLE OF xtrdir.


    CALL FUNCTION 'SVRS_COMPUTE_DELTA_REPS'
      TABLES
        texttab_old  = it_old
        texttab_new  = it_new
        trdirtab_old = lt_trdirtab_old
        trdirtab_new = lt_trdirtab_new
        trdir_delta  = lt_trdir_delta
        text_delta   = rt_delta.

  ENDMETHOD.                    "compute

  METHOD shortlist.

    DATA: lv_index TYPE i.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.

    IF lines( mt_diff ) < 20.
      LOOP AT mt_diff ASSIGNING <ls_diff>.
        <ls_diff>-short = abap_true.
      ENDLOOP.
    ELSE.
      LOOP AT mt_diff TRANSPORTING NO FIELDS
          WHERE NOT result IS INITIAL AND short = abap_false.
        lv_index = sy-tabix.

        DO 8 TIMES. " Backward
          READ TABLE mt_diff INDEX ( lv_index - sy-index ) ASSIGNING <ls_diff>.
          IF sy-subrc <> 0 OR <ls_diff>-short = abap_true. " tab bound or prev marker
            EXIT.
          ENDIF.
          <ls_diff>-short = abap_true.
        ENDDO.

        DO 8 TIMES. " Forward
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

    DATA: lv_new TYPE i VALUE 1,
          lv_old TYPE i VALUE 1.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.


    LOOP AT mt_diff ASSIGNING <ls_diff>.
      <ls_diff>-new_num = lv_new.
      <ls_diff>-old_num = lv_old.

      CASE <ls_diff>-result. " Line nums
        WHEN c_diff-delete.
          lv_old = lv_old + 1.
          CLEAR <ls_diff>-new_num.
        WHEN c_diff-insert.
          lv_new = lv_new + 1.
          CLEAR <ls_diff>-old_num.
        WHEN OTHERS.
          lv_new = lv_new + 1.
          lv_old = lv_old + 1.
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

    DATA: lv_beacon_idx  TYPE i,
          lv_offs        TYPE i,
          lv_beacon_str  TYPE string,
          lv_beacon_2lev TYPE string,
          lv_submatch    TYPE string,
          lo_regex       TYPE REF TO cl_abap_regex,
          lt_regex_set   TYPE TABLE OF REF TO cl_abap_regex.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.


    _add_regex '^\s*(CLASS|FORM|MODULE|REPORT|METHOD)\s'.
    _add_regex '^\s*START-OF-'.
    _add_regex '^\s*INITIALIZATION(\s|\.)'.

    LOOP AT mt_diff ASSIGNING <ls_diff>.

      CLEAR lv_offs.
      <ls_diff>-beacon = lv_beacon_idx.

      LOOP AT lt_regex_set INTO lo_regex. "
        FIND FIRST OCCURRENCE OF REGEX lo_regex IN <ls_diff>-new SUBMATCHES lv_submatch.
        IF sy-subrc = 0. " Match
          lv_beacon_str = <ls_diff>-new.
          lv_submatch = to_upper( lv_submatch ).

          " Get rid of comments and end of line
          FIND FIRST OCCURRENCE OF '.' IN lv_beacon_str MATCH OFFSET lv_offs.
          IF sy-subrc <> 0.
            FIND FIRST OCCURRENCE OF '"' IN lv_beacon_str MATCH OFFSET lv_offs.
          ENDIF.

          IF lv_offs > 0.
            lv_beacon_str = lv_beacon_str(lv_offs).
          ENDIF.

          IF lv_submatch = 'CLASS'.
            lv_beacon_2lev = lv_beacon_str.
          ELSEIF lv_submatch = 'METHOD'.
            lv_beacon_str = lv_beacon_2lev && ` => ` && lv_beacon_str.
          ENDIF.

          APPEND lv_beacon_str TO mt_beacons.
          lv_beacon_idx    = sy-tabix.
          <ls_diff>-beacon = lv_beacon_idx.
          EXIT. "Loop
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.                " map_beacons

  METHOD constructor.

    DATA: lt_delta TYPE vxabapt255_tab,
          lt_new   TYPE abaptxt255_tab,
          lt_old   TYPE abaptxt255_tab.


    unpack( EXPORTING iv_new = iv_new
                      iv_old = iv_old
            IMPORTING et_new = lt_new
                      et_old = lt_old ).

    lt_delta = compute( it_new = lt_new
                        it_old = lt_old ).

    mt_diff = render( it_new   = lt_new
                      it_old   = lt_old
                      it_delta = lt_delta ).

    calculate_line_num_and_stats( ).
    map_beacons( ).
    shortlist( ).

  ENDMETHOD.                    "diff

  METHOD render.

    DEFINE _append.
      CLEAR ls_diff.
      ls_diff-new    = &1.
      ls_diff-result = &2.
      ls_diff-old    = &3.
      APPEND ls_diff TO rt_diff.
    END-OF-DEFINITION.

    DATA: lv_oindex TYPE i VALUE 1,
          lv_nindex TYPE i VALUE 1,
          ls_new    LIKE LINE OF it_new,
          ls_old    LIKE LINE OF it_old,
          ls_diff   LIKE LINE OF rt_diff,
          lt_delta  LIKE it_delta,
          ls_delta  LIKE LINE OF it_delta.


    lt_delta = it_delta.

    DO.
      READ TABLE lt_delta INTO ls_delta WITH KEY number = lv_oindex.
      IF sy-subrc = 0.
        DELETE lt_delta INDEX sy-tabix.

        CASE ls_delta-vrsflag.
          WHEN c_diff-delete.
            _append '' c_diff-delete ls_delta-line.
            lv_oindex = lv_oindex + 1.
          WHEN c_diff-insert.
            _append ls_delta-line c_diff-insert ''.
            lv_nindex = lv_nindex + 1.
          WHEN c_diff-update.
            CLEAR ls_new.
            READ TABLE it_new INTO ls_new INDEX lv_nindex.
            ASSERT sy-subrc = 0.
            _append ls_new c_diff-update ls_delta-line.
            lv_nindex = lv_nindex + 1.
            lv_oindex = lv_oindex + 1.
          WHEN OTHERS.
            ASSERT 0 = 1.
        ENDCASE.
      ELSE.
        CLEAR ls_new.
        READ TABLE it_new INTO ls_new INDEX lv_nindex.    "#EC CI_SUBRC
        lv_nindex = lv_nindex + 1.
        CLEAR ls_old.
        READ TABLE it_old INTO ls_old INDEX lv_oindex.    "#EC CI_SUBRC
        lv_oindex = lv_oindex + 1.
        _append ls_new '' ls_old.
      ENDIF.

      IF lv_nindex > lines( it_new ) AND lv_oindex > lines( it_old ).
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
        RAISING   zcx_abapgit_exception,
      save
        IMPORTING iv_uri    TYPE string
                  ii_client TYPE REF TO if_http_client
        RAISING   zcx_abapgit_exception,
      clear,
      set
        IMPORTING iv_uri         TYPE string
                  iv_username    TYPE string
                  iv_password    TYPE string
        RETURNING VALUE(rv_auth) TYPE string
        RAISING   zcx_abapgit_exception.

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
        RAISING   zcx_abapgit_exception.

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


    READ TABLE gt_auth INTO ls_auth WITH KEY uri = zcl_abapgit_url=>host( iv_uri ).
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


    READ TABLE gt_auth WITH KEY uri = zcl_abapgit_url=>host( iv_uri )
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND INITIAL LINE TO gt_auth ASSIGNING <ls_auth>.
      <ls_auth>-uri           = zcl_abapgit_url=>host( iv_uri ).
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
          iv_msg  TYPE csequence
          iv_type TYPE symsgty   DEFAULT 'E'
          iv_rc   TYPE balsort   OPTIONAL,
      count
        RETURNING VALUE(rv_count) TYPE i,
      to_html
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html,
      clear,
      has_rc "For unit tests mainly
        IMPORTING iv_rc         TYPE balsort
        RETURNING VALUE(rv_yes) TYPE abap_bool,
      show.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_log,
             msg  TYPE string,
             type TYPE symsgty,
             rc   TYPE balsort,
           END OF ty_log.

    DATA: mt_log TYPE STANDARD TABLE OF ty_log WITH DEFAULT KEY.

ENDCLASS.

CLASS lcl_log IMPLEMENTATION.

  METHOD to_html.

    DATA: lv_class TYPE string,
          lv_icon  TYPE string.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF mt_log.

    CREATE OBJECT ro_html.

    IF count( ) = 0.
      RETURN.
    ENDIF.

    LOOP AT mt_log ASSIGNING <ls_log>.
      CASE <ls_log>-type.
        WHEN 'W'.
          lv_icon  = 'alert'.
          lv_class = 'warning'.
        WHEN 'E'.
          lv_icon  = 'flame'.
          lv_class = 'error'.
        WHEN OTHERS. " ??? unexpected
          lv_icon  = 'flame'.
          lv_class = 'error'.
      ENDCASE.

      ro_html->add( |<span class="{ lv_class }">| ).
      ro_html->add_icon( iv_name = lv_icon ).
      ro_html->add( <ls_log>-msg ).
      ro_html->add( '</span>' ).
    ENDLOOP.

  ENDMETHOD.

  METHOD add.

    FIELD-SYMBOLS: <ls_log> LIKE LINE OF mt_log.

    APPEND INITIAL LINE TO mt_log ASSIGNING <ls_log>.
    <ls_log>-msg  = iv_msg.
    <ls_log>-type = iv_type.
    <ls_log>-rc   = iv_rc.

  ENDMETHOD.

  METHOD show.
* only supports showing 4 errors, but I guess this is okay
* alternatively refactor to use method TO_HTML instead

    DATA: ls_log1 LIKE LINE OF mt_log,
          ls_log2 LIKE LINE OF mt_log,
          ls_log3 LIKE LINE OF mt_log,
          ls_log4 LIKE LINE OF mt_log.


    READ TABLE mt_log INDEX 1 INTO ls_log1.
    READ TABLE mt_log INDEX 2 INTO ls_log2.
    READ TABLE mt_log INDEX 3 INTO ls_log3.
    READ TABLE mt_log INDEX 4 INTO ls_log4.

    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Log'
        txt1  = ls_log1-msg
        txt2  = ls_log2-msg
        txt3  = ls_log3-msg
        txt4  = ls_log4-msg.

  ENDMETHOD.

  METHOD count.
    rv_count = lines( mt_log ).
  ENDMETHOD.

  METHOD clear.
    CLEAR mt_log.
  ENDMETHOD.  " clear.

  METHOD has_rc.
    READ TABLE mt_log WITH KEY rc = iv_rc TRANSPORTING NO FIELDS.
    rv_yes = boolc( sy-subrc = 0 ).
  ENDMETHOD. "has_rc

ENDCLASS.
