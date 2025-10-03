CLASS zcl_abapgit_diff_std DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS compute
      IMPORTING
        iv_new                TYPE xstring
        iv_old                TYPE xstring
        iv_ignore_indentation TYPE abap_bool
        iv_ignore_comments    TYPE abap_bool
        iv_ignore_case        TYPE abap_bool
      RETURNING
        VALUE(rt_diff)        TYPE zif_abapgit_definitions=>ty_diffs_tt
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    CLASS-DATA gt_diff TYPE zif_abapgit_definitions=>ty_diffs_tt.
    CLASS-DATA gv_compare_mode TYPE c LENGTH 1.
    CLASS-DATA gv_ignore_case TYPE abap_bool.

    CLASS-METHODS unpack
      IMPORTING
        !iv_new TYPE xstring
        !iv_old TYPE xstring
      EXPORTING
        !et_new TYPE rswsourcet
        !et_old TYPE rswsourcet
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS compute_diff
      IMPORTING
        !it_new        TYPE rswsourcet
        !it_old        TYPE rswsourcet
      RETURNING
        VALUE(rt_diff) TYPE zif_abapgit_definitions=>ty_diffs_tt.

    CLASS-METHODS compute_diff_extra
      IMPORTING
        !it_new        TYPE rswsourcet
        !it_old        TYPE rswsourcet
      RETURNING
        VALUE(rt_diff) TYPE zif_abapgit_definitions=>ty_diffs_tt.

    CLASS-METHODS adjust_diff.

    CLASS-METHODS has_line_diff
      IMPORTING
        iv_old             TYPE string
        iv_new             TYPE string
      RETURNING
        VALUE(rv_has_diff) TYPE abap_bool.

ENDCLASS.

CLASS zcl_abapgit_diff_std IMPLEMENTATION.
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
      <ls_diff>       LIKE LINE OF gt_diff,
      <ls_diff_begin> LIKE LINE OF gt_diff,
      <ls_diff_end>   LIKE LINE OF gt_diff.

    " Determine start and length of diff blocks
    LOOP AT gt_diff ASSIGNING <ls_diff>.
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
        READ TABLE gt_diff ASSIGNING <ls_diff_begin> INDEX lv_block_begin.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        lv_block_end = ls_diff_block-start + ls_diff_block-len + sy-index - 1.
        READ TABLE gt_diff ASSIGNING <ls_diff_end> INDEX lv_block_end.
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

  METHOD unpack.

    DATA: lv_new      TYPE string,
          lv_old      TYPE string,
          lv_new_last TYPE c LENGTH 1,
          lv_old_last TYPE c LENGTH 1.

    lv_new = zcl_abapgit_convert=>xstring_to_string_utf8( iv_new ).
    lv_old = zcl_abapgit_convert=>xstring_to_string_utf8( iv_old ).

    " Check if one value contains a final newline but the other not
    " If yes, add a special character that's visible in diff render
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

  METHOD compute.
    DATA: lt_new TYPE rswsourcet,
          lt_old TYPE rswsourcet.

    gv_compare_mode = 1.
    IF iv_ignore_indentation = abap_true.
      gv_compare_mode = gv_compare_mode + 1.
    ENDIF.
    IF iv_ignore_comments = abap_true.
      gv_compare_mode = gv_compare_mode + 2.
    ENDIF.
    gv_ignore_case = iv_ignore_case.

    unpack( EXPORTING iv_new = iv_new
                      iv_old = iv_old
            IMPORTING et_new = lt_new
                      et_old = lt_old ).

    gt_diff = compute_diff( it_new = lt_new
                            it_old = lt_old ).

    adjust_diff( ).

    rt_diff = gt_diff.

  ENDMETHOD.

  METHOD compute_diff.

    DATA:
      lv_i     TYPE i,
      ls_diff  LIKE LINE OF rt_diff,
      lt_delta TYPE STANDARD TABLE OF rsedcresul WITH DEFAULT KEY.

    FIELD-SYMBOLS <ls_delta> LIKE LINE OF lt_delta.

    " Note: Ignore case is for keywords, variables, types etc, but not for literals
    CALL FUNCTION 'RS_CMP_COMPUTE_DELTA'
      EXPORTING
        compare_mode            = gv_compare_mode
        ignore_case_differences = gv_ignore_case
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
      " The function doesn't find all diffs...
      rt_diff = compute_diff_extra( it_new = it_new
                                    it_old = it_old ).
    ELSE.
      ASSERT 0 = 1. " incorrect function call
    ENDIF.

  ENDMETHOD.


  METHOD compute_diff_extra.

    DATA:
      ls_diff LIKE LINE OF rt_diff.

    FIELD-SYMBOLS:
      <lv_old> LIKE LINE OF it_old,
      <lv_new> LIKE LINE OF it_new.

    LOOP AT it_old ASSIGNING <lv_old>.
      CLEAR ls_diff.
      ls_diff-old_num = sy-tabix.
      ls_diff-old     = <lv_old>.
      READ TABLE it_new ASSIGNING <lv_new> INDEX sy-tabix.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      ls_diff-new_num = sy-tabix.
      ls_diff-new     = <lv_new>.

      IF ( gv_compare_mode = 1 OR gv_compare_mode = 3 )
      AND has_line_diff( iv_old = <lv_old>
                         iv_new = <lv_new> ) = abap_true.
        ls_diff-result = zif_abapgit_definitions=>c_diff-update.
      ENDIF.
      APPEND ls_diff TO rt_diff.
    ENDLOOP.

  ENDMETHOD.

  METHOD has_line_diff.

    " SAP function ignores lines that contain only whitespace so we compare directly
    " Also check if length differs and implicitly if one line has trailing space(s)
    rv_has_diff = boolc( iv_old <> iv_new
                   AND ( strlen( condense( iv_old ) ) = 0
                      OR strlen( condense( iv_new ) ) = 0
                      OR strlen( iv_old ) <> strlen( iv_new ) ) ).

  ENDMETHOD.

ENDCLASS.
