CLASS zcl_abapgit_diff_diff3 DEFINITION PUBLIC.
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
    CLASS-METHODS normalize_line
      IMPORTING
        iv_line               TYPE string
        iv_ignore_indentation TYPE abap_bool
        iv_ignore_comments    TYPE abap_bool
        iv_ignore_case        TYPE abap_bool
      RETURNING
        VALUE(rv_line)        TYPE string.
    CLASS-METHODS prepare_lines
      IMPORTING
        it_lines              TYPE string_table
        iv_ignore_indentation TYPE abap_bool
        iv_ignore_comments    TYPE abap_bool
        iv_ignore_case        TYPE abap_bool
      RETURNING
        VALUE(rt_lines)       TYPE string_table.
ENDCLASS.

CLASS zcl_abapgit_diff_diff3 IMPLEMENTATION.
  METHOD compute.
* Beware: this is AI generated code -Hvam 2025-09-29
    DATA: lv_new      TYPE string,
          lv_old      TYPE string,
          lv_new_last TYPE c LENGTH 1,
          lv_old_last TYPE c LENGTH 1,
          lt_new      TYPE string_table,
          lt_old      TYPE string_table,
          lt_new_cmp  TYPE string_table,
          lt_old_cmp  TYPE string_table,
          lt_comm     TYPE zif_abapgit_diff3=>ty_comm_result_t,
          lo_diff3    TYPE REF TO zif_abapgit_diff3,
          ls_comm     LIKE LINE OF lt_comm,
          ls_diff     TYPE zif_abapgit_definitions=>ty_diff,
          lt_ins      TYPE string_table,
          lt_del      TYPE string_table,
          lv_line     TYPE string,
          lv_max      TYPE i,
          lv_new_idx  TYPE i VALUE 0,
          lv_old_idx  TYPE i VALUE 0,
          lv_i        TYPE i.

    CLEAR rt_diff.

    lv_new = zcl_abapgit_convert=>xstring_to_string_utf8( iv_new ).
    lv_old = zcl_abapgit_convert=>xstring_to_string_utf8( iv_old ).

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
    IF lv_new_last = cl_abap_char_utilities=>newline
      AND lv_old_last <> cl_abap_char_utilities=>newline
      AND lv_old IS NOT INITIAL.
      lv_old = lv_old && cl_abap_char_utilities=>form_feed.
    ELSEIF lv_new_last <> cl_abap_char_utilities=>newline
      AND lv_old_last = cl_abap_char_utilities=>newline
      AND lv_new IS NOT INITIAL.
      lv_new = lv_new && cl_abap_char_utilities=>form_feed.
    ENDIF.

    SPLIT lv_new AT cl_abap_char_utilities=>newline INTO TABLE lt_new.
    SPLIT lv_old AT cl_abap_char_utilities=>newline INTO TABLE lt_old.

    lt_new_cmp = prepare_lines(
      it_lines              = lt_new
      iv_ignore_indentation = iv_ignore_indentation
      iv_ignore_comments    = iv_ignore_comments
      iv_ignore_case        = iv_ignore_case ).
    lt_old_cmp = prepare_lines(
      it_lines              = lt_old
      iv_ignore_indentation = iv_ignore_indentation
      iv_ignore_comments    = iv_ignore_comments
      iv_ignore_case        = iv_ignore_case ).

    lo_diff3 = zcl_abapgit_diff3=>create( ).
    lt_comm = lo_diff3->diff_comm(
      it_buffer1 = lt_new_cmp
      it_buffer2 = lt_old_cmp ).

    LOOP AT lt_comm INTO ls_comm.
      IF ls_comm-common IS NOT INITIAL.
        LOOP AT ls_comm-common INTO lv_line.
          lv_new_idx = lv_new_idx + 1.
          lv_old_idx = lv_old_idx + 1.
          CLEAR ls_diff.
          lv_i = lv_new_idx.
          ls_diff-new_num = lv_i.
          lv_i = lv_old_idx.
          ls_diff-old_num = lv_i.
          READ TABLE lt_new INDEX lv_new_idx INTO lv_line.
          ls_diff-new = lv_line.
          READ TABLE lt_old INDEX lv_old_idx INTO lv_line.
          ls_diff-old = lv_line.

          IF iv_ignore_comments = abap_true
            AND normalize_line(
              iv_line               = ls_diff-new
              iv_ignore_indentation = iv_ignore_indentation
              iv_ignore_comments    = iv_ignore_comments
              iv_ignore_case        = iv_ignore_case ) IS INITIAL
            AND normalize_line(
              iv_line               = ls_diff-old
              iv_ignore_indentation = iv_ignore_indentation
              iv_ignore_comments    = iv_ignore_comments
              iv_ignore_case        = iv_ignore_case ) IS INITIAL.
            IF ls_diff-new IS NOT INITIAL AND ls_diff-old IS INITIAL.
              CLEAR: ls_diff-old, ls_diff-old_num.
            ELSEIF ls_diff-old IS NOT INITIAL AND ls_diff-new IS INITIAL.
              CLEAR: ls_diff-new, ls_diff-new_num.
            ENDIF.
          ENDIF.

          ls_diff-result = zif_abapgit_definitions=>c_diff-unchanged.
          APPEND ls_diff TO rt_diff.
        ENDLOOP.
      ELSE.
        lt_ins = ls_comm-diff-buffer1.
        lt_del = ls_comm-diff-buffer2.
        lv_max = lines( lt_ins ).
        IF lines( lt_del ) > lv_max.
          lv_max = lines( lt_del ).
        ENDIF.
        DO lv_max TIMES.
          CLEAR ls_diff.
          IF sy-index <= lines( lt_del ) AND sy-index <= lines( lt_ins ).
            lv_old_idx = lv_old_idx + 1.
            lv_new_idx = lv_new_idx + 1.
            lv_i = lv_new_idx.
            ls_diff-new_num = lv_i.
            lv_i = lv_old_idx.
            ls_diff-old_num = lv_i.
            READ TABLE lt_new INDEX lv_new_idx INTO lv_line.
            ls_diff-new = lv_line.
            READ TABLE lt_old INDEX lv_old_idx INTO lv_line.
            ls_diff-old = lv_line.
            ls_diff-result = zif_abapgit_definitions=>c_diff-update.
          ELSEIF sy-index <= lines( lt_del ).
            lv_old_idx = lv_old_idx + 1.
            READ TABLE lt_old INDEX lv_old_idx INTO lv_line.
            ls_diff-old = lv_line.
            ls_diff-old_num = lv_old_idx.
            ls_diff-result = zif_abapgit_definitions=>c_diff-delete.
            IF iv_ignore_comments = abap_true AND normalize_line(
              iv_line               = ls_diff-old
              iv_ignore_indentation = iv_ignore_indentation
              iv_ignore_comments    = iv_ignore_comments
              iv_ignore_case        = iv_ignore_case ) IS INITIAL.
              ls_diff-result = zif_abapgit_definitions=>c_diff-unchanged.
            ENDIF.
          ELSEIF sy-index <= lines( lt_ins ).
            lv_new_idx = lv_new_idx + 1.
            READ TABLE lt_new INDEX lv_new_idx INTO lv_line.
            ls_diff-new = lv_line.
            ls_diff-new_num = lv_new_idx.
            ls_diff-result = zif_abapgit_definitions=>c_diff-insert.
            IF iv_ignore_comments = abap_true AND normalize_line(
              iv_line               = ls_diff-new
              iv_ignore_indentation = iv_ignore_indentation
              iv_ignore_comments    = iv_ignore_comments
              iv_ignore_case        = iv_ignore_case ) IS INITIAL.
              ls_diff-result = zif_abapgit_definitions=>c_diff-unchanged.
            ENDIF.
          ENDIF.
          APPEND ls_diff TO rt_diff.
        ENDDO.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD normalize_line.
    DATA: lv_offset  TYPE i,
          lv_length  TYPE i,
          lv_literal TYPE c LENGTH 1,
          lv_char    TYPE string,
          lv_source  TYPE string.

    rv_line = iv_line.

    IF iv_ignore_indentation = abap_true.
      SHIFT rv_line LEFT DELETING LEADING space.
      WHILE rv_line IS NOT INITIAL
        AND rv_line(1) = cl_abap_char_utilities=>horizontal_tab.
        rv_line = substring(
          val = rv_line
          off = 1 ).
        SHIFT rv_line LEFT DELETING LEADING space.
      ENDWHILE.
    ENDIF.

    IF iv_ignore_comments = abap_true
      AND rv_line IS NOT INITIAL
      AND rv_line(1) = '*'.
      CLEAR rv_line.
      RETURN.
    ENDIF.

    IF iv_ignore_comments = abap_false AND iv_ignore_case = abap_false.
      RETURN.
    ENDIF.

    lv_source = rv_line.
    lv_length = strlen( lv_source ).
    CLEAR rv_line.
    WHILE lv_offset < lv_length.
      lv_char = substring(
        val = lv_source
        off = lv_offset
        len = 1 ).

      IF lv_literal IS INITIAL AND ( lv_char = '''' OR lv_char = '`' OR lv_char = '|' ).
        lv_literal = lv_char.
      ELSEIF lv_literal = lv_char.
        CLEAR lv_literal.
      ELSEIF lv_literal IS INITIAL AND lv_char = '"' AND iv_ignore_comments = abap_true.
        EXIT.
      ELSEIF lv_literal IS INITIAL AND iv_ignore_case = abap_true.
        TRANSLATE lv_char TO UPPER CASE.
      ENDIF.

      rv_line = rv_line && lv_char.
      lv_offset = lv_offset + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD prepare_lines.
    DATA: lv_line TYPE string,
          lv_cmp  TYPE string,
          lv_index TYPE i.

    LOOP AT it_lines INTO lv_line.
      IF iv_ignore_indentation = abap_false
        AND iv_ignore_comments = abap_false
        AND iv_ignore_case = abap_false.
        lv_cmp = lv_line.
      ELSE.
        lv_cmp = normalize_line(
          iv_line               = lv_line
          iv_ignore_indentation = iv_ignore_indentation
          iv_ignore_comments    = iv_ignore_comments
          iv_ignore_case        = iv_ignore_case ).
      ENDIF.
      IF iv_ignore_indentation = abap_false AND condense( lv_cmp ) = ''.
        lv_index = sy-tabix.
        lv_cmp = lv_cmp && cl_abap_char_utilities=>form_feed && lv_index.
      ENDIF.
      APPEND lv_cmp TO rt_lines.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
