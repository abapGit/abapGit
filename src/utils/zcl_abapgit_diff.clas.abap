CLASS zcl_abapgit_diff DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS co_starting_beacon TYPE i VALUE 1.

* assumes data is UTF8 based with newlines
    METHODS constructor
      IMPORTING
        !iv_new                TYPE xstring
        !iv_old                TYPE xstring
        !iv_ignore_indentation TYPE abap_bool DEFAULT abap_false
        !iv_ignore_comments    TYPE abap_bool DEFAULT abap_false
        !iv_ignore_case        TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception.
    METHODS get
      RETURNING
        VALUE(rt_diff) TYPE zif_abapgit_definitions=>ty_diffs_tt.
    METHODS stats
      RETURNING
        VALUE(rs_count) TYPE zif_abapgit_definitions=>ty_count.
    METHODS set_patch_new
      IMPORTING
        !iv_line_new   TYPE i
        !iv_patch_flag TYPE abap_bool
      RAISING
        zcx_abapgit_exception.
    METHODS set_patch_old
      IMPORTING
        !iv_line_old   TYPE i
        !iv_patch_flag TYPE abap_bool
      RAISING
        zcx_abapgit_exception.
    METHODS get_beacons
      RETURNING
        VALUE(rt_beacons) TYPE zif_abapgit_definitions=>ty_string_tt.
    METHODS is_line_patched
      IMPORTING
        iv_index          TYPE i
      RETURNING
        VALUE(rv_patched) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.
    METHODS set_patch_by_old_diff
      IMPORTING
        is_diff_old   TYPE zif_abapgit_definitions=>ty_diff
        iv_patch_flag TYPE abap_bool.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      ty_regexset_tt TYPE STANDARD TABLE OF REF TO cl_abap_regex WITH KEY table_line.

    DATA mt_beacons TYPE zif_abapgit_definitions=>ty_string_tt.
    DATA mt_diff TYPE zif_abapgit_definitions=>ty_diffs_tt.
    DATA ms_stats TYPE zif_abapgit_definitions=>ty_count.
    DATA mv_compare_mode TYPE c LENGTH 1.
    DATA mv_ignore_case TYPE abap_bool.

    METHODS unpack
      IMPORTING
        !iv_new TYPE xstring
        !iv_old TYPE xstring
      EXPORTING
        !et_new TYPE rswsourcet
        !et_old TYPE rswsourcet
      RAISING
        zcx_abapgit_exception.
    METHODS map_beacons.
    METHODS shortlist.
    METHODS create_regex_set
      RETURNING
        VALUE(rt_regex_set) TYPE ty_regexset_tt.
    METHODS compute_and_render
      IMPORTING
        !it_new        TYPE rswsourcet
        !it_old        TYPE rswsourcet
      RETURNING
        VALUE(rt_diff) TYPE zif_abapgit_definitions=>ty_diffs_tt.
    METHODS calculate_stats.
    METHODS adjust_diff.
ENDCLASS.



CLASS zcl_abapgit_diff IMPLEMENTATION.


  METHOD adjust_diff.

    " ABAP kernel diff traverses files from bottom up which leads to odd display of diffs
    " SAP won't adjust this kernel service so we will do it here
    " https://github.com/abapGit/abapGit/issues/4395

    TYPES:
      BEGIN OF ty_diff_block,
        start TYPE i,
        len   TYPE i,
      END OF ty_diff_block.

    DATA:
      lv_block_begin TYPE i,
      lv_block_end   TYPE i,
      ls_diff_block  TYPE ty_diff_block,
      lt_diff_block  TYPE STANDARD TABLE OF ty_diff_block WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_diff>       LIKE LINE OF mt_diff,
      <ls_diff_begin> LIKE LINE OF mt_diff,
      <ls_diff_end>   LIKE LINE OF mt_diff.

    " Determine start and length of diff blocks
    LOOP AT mt_diff ASSIGNING <ls_diff>.
      IF <ls_diff>-result = zif_abapgit_definitions=>c_diff-insert OR
         <ls_diff>-result = zif_abapgit_definitions=>c_diff-delete.
        IF ls_diff_block IS INITIAL.
          ls_diff_block-start = sy-tabix.
        ENDIF.
        ls_diff_block-len = ls_diff_block-len + 1.
      ELSEIF ls_diff_block-start IS NOT INITIAL.
        APPEND ls_diff_block TO lt_diff_block.
        CLEAR ls_diff_block.
      ENDIF.
    ENDLOOP.

    " For each diff block, check if beginning is same as end of block
    " If yes, move diff block down
    LOOP AT lt_diff_block INTO ls_diff_block.
      DO ls_diff_block-len TIMES.
        lv_block_begin = ls_diff_block-start + sy-index - 1.
        READ TABLE mt_diff ASSIGNING <ls_diff_begin> INDEX lv_block_begin.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        lv_block_end = ls_diff_block-start + ls_diff_block-len + sy-index - 1.
        READ TABLE mt_diff ASSIGNING <ls_diff_end> INDEX lv_block_end.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        CASE <ls_diff_begin>-result.
          WHEN zif_abapgit_definitions=>c_diff-insert.
            IF <ls_diff_begin>-new = <ls_diff_end>-new.
              <ls_diff_begin>-old_num = <ls_diff_end>-old_num.
              <ls_diff_begin>-old     = <ls_diff_end>-old.
              <ls_diff_end>-result    = <ls_diff_begin>-result.
              CLEAR: <ls_diff_begin>-result, <ls_diff_end>-old_num, <ls_diff_end>-old.
            ELSE.
              EXIT.
            ENDIF.
          WHEN zif_abapgit_definitions=>c_diff-delete.
            IF <ls_diff_begin>-old = <ls_diff_end>-old.
              <ls_diff_begin>-new_num = <ls_diff_end>-new_num.
              <ls_diff_begin>-new     = <ls_diff_end>-new.
              <ls_diff_end>-result    = <ls_diff_begin>-result.
              CLEAR: <ls_diff_begin>-result, <ls_diff_end>-new_num, <ls_diff_end>-new.
            ELSE.
              EXIT.
            ENDIF.
          WHEN OTHERS.
            EXIT.
        ENDCASE.
      ENDDO.
    ENDLOOP.

  ENDMETHOD.


  METHOD calculate_stats.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.

    LOOP AT mt_diff ASSIGNING <ls_diff>.
      CASE <ls_diff>-result.
        WHEN zif_abapgit_definitions=>c_diff-insert.
          ms_stats-insert = ms_stats-insert + 1.
        WHEN zif_abapgit_definitions=>c_diff-delete.
          ms_stats-delete = ms_stats-delete + 1.
        WHEN zif_abapgit_definitions=>c_diff-update.
          ms_stats-update = ms_stats-update + 1.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD compute_and_render.

    DATA:
      lv_i     TYPE i,
      ls_diff  LIKE LINE OF rt_diff,
      lt_delta TYPE STANDARD TABLE OF rsedcresul WITH DEFAULT KEY.

    FIELD-SYMBOLS:
      <ls_old>   LIKE LINE OF it_old,
      <ls_new>   LIKE LINE OF it_new,
      <ls_delta> LIKE LINE OF lt_delta.

    " Note: Ignore case is for keywords, variables, types etc, but not for literals
    CALL FUNCTION 'RS_CMP_COMPUTE_DELTA'
      EXPORTING
        compare_mode            = mv_compare_mode
        ignore_case_differences = mv_ignore_case
      TABLES
        text_tab1               = it_new
        text_tab2               = it_old
        text_tab_res            = lt_delta
      EXCEPTIONS
        parameter_invalid       = 1
        difference_not_found    = 2
        OTHERS                  = 3.

    IF sy-subrc = 0.
      " Process delta
      LOOP AT lt_delta ASSIGNING <ls_delta>.
        CLEAR ls_diff.
        IF <ls_delta>-line1 > 0.
          lv_i = <ls_delta>-line1.
          ls_diff-old_num = lv_i.
          ls_diff-old     = <ls_delta>-text1.
        ENDIF.
        IF <ls_delta>-line2 > 0.
          lv_i = <ls_delta>-line2.
          ls_diff-new_num = lv_i.
          ls_diff-new     = <ls_delta>-text2.
        ENDIF.
        IF <ls_delta>-flag1 = 'D'.
          ls_diff-result = zif_abapgit_definitions=>c_diff-delete.
        ELSEIF <ls_delta>-flag2 = 'I'.
          ls_diff-result = zif_abapgit_definitions=>c_diff-insert.
        ELSEIF <ls_delta>-flag1 = 'M' AND <ls_delta>-flag2 = 'M'.
          ls_diff-result = zif_abapgit_definitions=>c_diff-update.
        ELSEIF <ls_delta>-flag1 = '' AND <ls_delta>-flag2 = ''.
          ls_diff-result = zif_abapgit_definitions=>c_diff-unchanged.
        ELSEIF <ls_delta>-flag1 = '' AND <ls_delta>-flag2 = 'E'. " ignore comment
          ls_diff-result = zif_abapgit_definitions=>c_diff-unchanged.
        ELSEIF <ls_delta>-flag1 = 'E' AND <ls_delta>-flag2 = ''. " ignore comment
          ls_diff-result = zif_abapgit_definitions=>c_diff-unchanged.
        ELSE.
          ASSERT 0 = 1. " unknown comparison result
        ENDIF.
        APPEND ls_diff TO rt_diff.
      ENDLOOP.
    ELSEIF sy-subrc = 2.
      " Copy input... but it might not be identical
      LOOP AT it_old ASSIGNING <ls_old>.
        CLEAR ls_diff.
        ls_diff-old_num = sy-tabix.
        ls_diff-old     = <ls_old>.
        READ TABLE it_new ASSIGNING <ls_new> INDEX sy-tabix.
        ASSERT sy-subrc = 0.
        ls_diff-new_num = sy-tabix.
        ls_diff-new     = <ls_new>.
        " SAP function ignores lines that contain only whitespace so we compare directly
        IF ( mv_compare_mode = 1 OR mv_compare_mode = 3 ) AND <ls_old> <> <ls_new> AND
           ( strlen( condense( <ls_old> ) ) = 0 OR strlen( condense( <ls_new> ) ) = 0 ).
          ls_diff-result = zif_abapgit_definitions=>c_diff-update.
        ENDIF.
        APPEND ls_diff TO rt_diff.
      ENDLOOP.
    ELSE.
      ASSERT 0 = 1. " incorrect function call
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    DATA: lt_new TYPE rswsourcet,
          lt_old TYPE rswsourcet.

    mv_compare_mode = 1.
    IF iv_ignore_indentation = abap_true.
      mv_compare_mode = mv_compare_mode + 1.
    ENDIF.
    IF iv_ignore_comments = abap_true.
      mv_compare_mode = mv_compare_mode + 2.
    ENDIF.
    mv_ignore_case = iv_ignore_case.

    unpack( EXPORTING iv_new = iv_new
                      iv_old = iv_old
            IMPORTING et_new = lt_new
                      et_old = lt_old ).

    mt_diff = compute_and_render( it_new = lt_new
                                  it_old = lt_old ).

    adjust_diff( ).

    calculate_stats( ).
    map_beacons( ).
    shortlist( ).

  ENDMETHOD.


  METHOD create_regex_set.

    DATA: lo_regex TYPE REF TO cl_abap_regex,
          lt_regex TYPE zif_abapgit_definitions=>ty_string_tt,
          lv_regex LIKE LINE OF lt_regex.

    APPEND '^\s*(CLASS|FORM|MODULE|REPORT|METHOD|INTERFACE|FUNCTION)\s[^=]' TO lt_regex.
    APPEND '^\s*(PUBLIC|PROTECTED|PRIVATE)\sSECTION(\s|\.)' TO lt_regex.
    APPEND '^\s*(CLASS|INTERFACE|FUNCTION|TYPE)-POOL\s' TO lt_regex.
    APPEND '^\s*(START|END)-OF-SELECTION(\s|\.)' TO lt_regex.
    APPEND '^\s*INITIALIZATION(\s|\.)' TO lt_regex.
    APPEND '^\s*(TOP-OF-PAGE|END-OF-PAGE)(\s|\.)' TO lt_regex.
    APPEND '^\s*AT\s*(SELECTION-SCREEN|LINE-SELECTION|USER-COMMAND|PF\d+)(\s|\.)' TO lt_regex.
    APPEND '^\s*(DEFINE|ENHANCEMENT)\s' TO lt_regex.

    LOOP AT lt_regex INTO lv_regex.
      CREATE OBJECT lo_regex
        EXPORTING
          pattern     = lv_regex
          ignore_case = abap_true.
      APPEND lo_regex TO rt_regex_set.
    ENDLOOP.

  ENDMETHOD.


  METHOD get.
    rt_diff = mt_diff.
  ENDMETHOD.


  METHOD get_beacons.
    rt_beacons = mt_beacons.
  ENDMETHOD.


  METHOD is_line_patched.

    FIELD-SYMBOLS: <ls_diff> TYPE zif_abapgit_definitions=>ty_diff.

    READ TABLE mt_diff INDEX iv_index
                       ASSIGNING <ls_diff>.
    IF sy-subrc = 0.
      rv_patched = <ls_diff>-patch_flag.
    ELSE.
      zcx_abapgit_exception=>raise( |Diff line not found { iv_index }| ).
    ENDIF.

  ENDMETHOD.


  METHOD map_beacons.

    DATA: lv_beacon_idx  TYPE i VALUE co_starting_beacon,
          lv_offs        TYPE i,
          lv_beacon_str  TYPE string,
          lv_beacon_2lev TYPE string,
          lv_submatch    TYPE string,
          lo_regex       TYPE REF TO cl_abap_regex,
          lt_regex       TYPE ty_regexset_tt.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff.

    lt_regex = create_regex_set( ).
    LOOP AT mt_diff ASSIGNING <ls_diff>.

      CLEAR lv_offs.
      <ls_diff>-beacon = lv_beacon_idx.

      LOOP AT lt_regex INTO lo_regex.
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
          lv_beacon_str = condense( val = lv_beacon_str
                                    del = ` ` ).

          IF lv_submatch = 'CLASS'.
            lv_beacon_2lev = replace( val   = lv_beacon_str
                                      regex = '\s+(DEFINITION|IMPLEMENTATION)'
                                      with  = ''
                                      occ   = 0 ).
          ELSEIF lv_submatch = 'METHOD'.
            lv_beacon_str = lv_beacon_2lev && ` => ` && lv_beacon_str.
          ELSEIF lv_submatch = 'PUBLIC' OR lv_submatch = 'PROTECTED' OR lv_submatch = 'PRIVATE'.
            lv_beacon_str = lv_beacon_2lev && ` ` && lv_beacon_str.
          ENDIF.

          APPEND lv_beacon_str TO mt_beacons.
          lv_beacon_idx    = sy-tabix.
          <ls_diff>-beacon = lv_beacon_idx.
          EXIT. "Loop
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_patch_by_old_diff.

    FIELD-SYMBOLS: <ls_diff> TYPE zif_abapgit_definitions=>ty_diff.

    LOOP AT mt_diff ASSIGNING <ls_diff>
                    USING KEY new_num
                    WHERE old     = is_diff_old-old
                    AND   new     = is_diff_old-new
                    AND   new_num = is_diff_old-new_num
                    AND   old_num = is_diff_old-old_num.

      <ls_diff>-patch_flag = iv_patch_flag.
      EXIT.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_patch_new.

    FIELD-SYMBOLS: <ls_diff> TYPE zif_abapgit_definitions=>ty_diff.

    READ TABLE mt_diff WITH TABLE KEY new_num
                       COMPONENTS new_num = iv_line_new
                       ASSIGNING <ls_diff>.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Invalid new line number { iv_line_new }| ).
    ENDIF.

    <ls_diff>-patch_flag = iv_patch_flag.

  ENDMETHOD.


  METHOD set_patch_old.

    FIELD-SYMBOLS: <ls_diff> TYPE zif_abapgit_definitions=>ty_diff.

    READ TABLE mt_diff WITH TABLE KEY old_num
                       COMPONENTS old_num = iv_line_old
                       ASSIGNING <ls_diff>.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( |Invalid old line number { iv_line_old }| ).
    ENDIF.

    <ls_diff>-patch_flag = iv_patch_flag.

  ENDMETHOD.


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

  ENDMETHOD.


  METHOD stats.
    rs_count = ms_stats.
  ENDMETHOD.


  METHOD unpack.

    DATA: lv_new      TYPE string,
          lv_old      TYPE string,
          lv_new_last TYPE c LENGTH 1,
          lv_old_last TYPE c LENGTH 1.

    lv_new = zcl_abapgit_convert=>xstring_to_string_utf8( iv_new ).
    lv_old = zcl_abapgit_convert=>xstring_to_string_utf8( iv_old ).

    " Check if one value contains a final newline but the other not
    " If yes, add a special characters that's visible in diff render
    IF lv_new IS NOT INITIAL.
      lv_new_last = substring(
        val = lv_new
        off = strlen( lv_new ) - 1 ).
    ENDIF.
    IF lv_old IS NOT INITIAL.
      lv_old_last = substring(
        val = lv_old
        off = strlen( lv_old ) - 1 ).
    ENDIF.

    IF lv_new_last = cl_abap_char_utilities=>newline AND lv_old_last <> cl_abap_char_utilities=>newline
      AND lv_old IS NOT INITIAL.
      lv_old = lv_old && cl_abap_char_utilities=>form_feed.
    ELSEIF lv_new_last <> cl_abap_char_utilities=>newline AND lv_old_last = cl_abap_char_utilities=>newline
      AND lv_new IS NOT INITIAL.
      lv_new = lv_new && cl_abap_char_utilities=>form_feed.
    ENDIF.

    SPLIT lv_new AT cl_abap_char_utilities=>newline INTO TABLE et_new.
    SPLIT lv_old AT cl_abap_char_utilities=>newline INTO TABLE et_old.

  ENDMETHOD.
ENDCLASS.
