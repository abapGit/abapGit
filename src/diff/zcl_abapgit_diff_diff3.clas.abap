CLASS zcl_abapgit_diff_diff3 DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS compute
      IMPORTING
        iv_new         TYPE xstring
        iv_old         TYPE xstring
        " todo
        " iv_ignore_indentation TYPE abap_bool
        " iv_ignore_comments    TYPE abap_bool
        " iv_ignore_case        TYPE abap_bool
      RETURNING
        VALUE(rt_diff) TYPE zif_abapgit_definitions=>ty_diffs_tt
      RAISING
        zcx_abapgit_exception.
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

    lo_diff3 = zcl_abapgit_diff3=>create( ).
    lt_comm = lo_diff3->diff_comm(
      it_buffer1 = lt_new
      it_buffer2 = lt_old ).

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
          ls_diff-new = lv_line.
          ls_diff-old = lv_line.
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
            READ TABLE lt_ins INDEX sy-index INTO lv_line.
            IF sy-subrc = 0.
              ls_diff-new = lv_line.
            ENDIF.
            READ TABLE lt_del INDEX sy-index INTO lv_line.
            IF sy-subrc = 0.
              ls_diff-old = lv_line.
            ENDIF.
            ls_diff-result = zif_abapgit_definitions=>c_diff-update.
          ELSEIF sy-index <= lines( lt_del ).
            lv_old_idx = lv_old_idx + 1.
            READ TABLE lt_del INDEX sy-index INTO lv_line.
            IF sy-subrc = 0.
              ls_diff-old = lv_line.
            ENDIF.
            ls_diff-old_num = lv_old_idx.
            ls_diff-result = zif_abapgit_definitions=>c_diff-delete.
          ELSEIF sy-index <= lines( lt_ins ).
            lv_new_idx = lv_new_idx + 1.
            READ TABLE lt_ins INDEX sy-index INTO lv_line.
            IF sy-subrc = 0.
              ls_diff-new = lv_line.
            ENDIF.
            ls_diff-new_num = lv_new_idx.
            ls_diff-result = zif_abapgit_definitions=>c_diff-insert.
          ENDIF.
          APPEND ls_diff TO rt_diff.
        ENDDO.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
