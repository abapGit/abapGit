CLASS zcl_abapgit_diff DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA mt_beacons TYPE zif_abapgit_definitions=>ty_string_tt READ-ONLY.

* assumes data is UTF8 based with newlines
* only works with lines up to 255 characters
    METHODS constructor
      IMPORTING iv_new TYPE xstring
                iv_old TYPE xstring.

    METHODS get
      RETURNING VALUE(rt_diff) TYPE zif_abapgit_definitions=>ty_diffs_tt.

    METHODS stats
      RETURNING VALUE(rs_count) TYPE zif_abapgit_definitions=>ty_count.
  PRIVATE SECTION.
    DATA mt_diff     TYPE zif_abapgit_definitions=>ty_diffs_tt.
    DATA ms_stats    TYPE zif_abapgit_definitions=>ty_count.

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
        RETURNING VALUE(rt_diff) TYPE zif_abapgit_definitions=>ty_diffs_tt,
      compute
        IMPORTING it_new          TYPE abaptxt255_tab
                  it_old          TYPE abaptxt255_tab
        RETURNING VALUE(rt_delta) TYPE vxabapt255_tab.

    METHODS:
      calculate_line_num_and_stats,
      map_beacons,
      shortlist.
ENDCLASS.



CLASS ZCL_ABAPGIT_DIFF IMPLEMENTATION.


  METHOD calculate_line_num_and_stats.

    DATA: lv_new TYPE i VALUE 1,
          lv_old TYPE i VALUE 1.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.


    LOOP AT mt_diff ASSIGNING <ls_diff>.
      <ls_diff>-new_num = lv_new.
      <ls_diff>-old_num = lv_old.

      CASE <ls_diff>-result. " Line nums
        WHEN zif_abapgit_definitions=>c_diff-delete.
          lv_old = lv_old + 1.
          CLEAR <ls_diff>-new_num.
        WHEN zif_abapgit_definitions=>c_diff-insert.
          lv_new = lv_new + 1.
          CLEAR <ls_diff>-old_num.
        WHEN OTHERS.
          lv_new = lv_new + 1.
          lv_old = lv_old + 1.
      ENDCASE.

      CASE <ls_diff>-result. " Stats
        WHEN zif_abapgit_definitions=>c_diff-insert.
          ms_stats-insert = ms_stats-insert + 1.
        WHEN zif_abapgit_definitions=>c_diff-delete.
          ms_stats-delete = ms_stats-delete + 1.
        WHEN zif_abapgit_definitions=>c_diff-update.
          ms_stats-update = ms_stats-update + 1.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.                " calculate_line_num_and_stats


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


  METHOD get.
    rt_diff = mt_diff.
  ENDMETHOD.                    "get


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
          WHEN zif_abapgit_definitions=>c_diff-delete.
            _append '' zif_abapgit_definitions=>c_diff-delete ls_delta-line.
            lv_oindex = lv_oindex + 1.
          WHEN zif_abapgit_definitions=>c_diff-insert.
            _append ls_delta-line zif_abapgit_definitions=>c_diff-insert ''.
            lv_nindex = lv_nindex + 1.
          WHEN zif_abapgit_definitions=>c_diff-update.
            CLEAR ls_new.
            READ TABLE it_new INTO ls_new INDEX lv_nindex.
            ASSERT sy-subrc = 0.
            _append ls_new zif_abapgit_definitions=>c_diff-update ls_delta-line.
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


  METHOD stats.
    rs_count = ms_stats.
  ENDMETHOD.                    "count


  METHOD unpack.

    DATA: lv_new TYPE string,
          lv_old TYPE string.


    lv_new = zcl_abapgit_convert=>xstring_to_string_utf8( iv_new ).
    lv_old = zcl_abapgit_convert=>xstring_to_string_utf8( iv_old ).

    SPLIT lv_new AT zif_abapgit_definitions=>gc_newline INTO TABLE et_new.
    SPLIT lv_old AT zif_abapgit_definitions=>gc_newline INTO TABLE et_old.

  ENDMETHOD.                    "unpack
ENDCLASS.
