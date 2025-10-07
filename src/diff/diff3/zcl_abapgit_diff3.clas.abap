CLASS zcl_abapgit_diff3 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

************************************************************************
* ABAP Diff3
*
* https://github.com/Marc-Bernard-Tools/ABAP-Diff3
*
* This is a port of JavaScript (https://github.com/bhousel/node-diff3, MIT license)
* https://github.com/bhousel/node-diff3/blob/main/index.mjs as of 2021-09-24
*
* Copyright 2022 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************

  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '1.1.0' ##NEEDED.

    INTERFACES zif_abapgit_diff3.

    CLASS-METHODS create
      RETURNING
        VALUE(ri_result) TYPE REF TO zif_abapgit_diff3.

    CLASS-METHODS convert_to_abap_indices
      CHANGING
        !ct_diff_indices TYPE zif_abapgit_diff3=>ty_diff_indices_result_t OPTIONAL.

  PROTECTED SECTION.

    TYPES:
      ty_ab TYPE c LENGTH 1.

    TYPES:
      BEGIN OF ty_hunk,
        ab        TYPE ty_ab,
        o_start   TYPE zif_abapgit_diff3=>ty_number,
        o_length  TYPE zif_abapgit_diff3=>ty_number,
        ab_start  TYPE zif_abapgit_diff3=>ty_number,
        ab_length TYPE zif_abapgit_diff3=>ty_number,
      END OF ty_hunk.
    TYPES:
      ty_hunks TYPE STANDARD TABLE OF ty_hunk WITH DEFAULT KEY.

    METHODS process_common
      CHANGING
        !ct_common TYPE string_table
        !ct_result TYPE zif_abapgit_diff3=>ty_comm_result_t.

    METHODS chunk_description
      IMPORTING
        !it_buffer       TYPE string_table
        !iv_offset       TYPE zif_abapgit_diff3=>ty_number
        !iv_length       TYPE zif_abapgit_diff3=>ty_number
      RETURNING
        VALUE(rs_result) TYPE zif_abapgit_diff3=>ty_chunk.

    METHODS add_hunk
      IMPORTING
        !it_buffer TYPE zif_abapgit_diff3=>ty_diff_indices_result_t
        !iv_ab     TYPE ty_ab
      CHANGING
        !ct_hunks  TYPE ty_hunks.

    METHODS advance_to
      IMPORTING
        !iv_end_offset  TYPE zif_abapgit_diff3=>ty_number
        !it_o           TYPE string_table
      CHANGING
        !cv_curr_offset TYPE zif_abapgit_diff3=>ty_number
        !ct_results     TYPE zif_abapgit_diff3=>ty_region_t.

    METHODS flush_ok
      CHANGING
        !ct_buffer TYPE string_table
        !ct_result TYPE zif_abapgit_diff3=>ty_merge_region_t.

    METHODS get_labels
      IMPORTING
        !is_labels       TYPE zif_abapgit_diff3=>ty_labels
      RETURNING
        VALUE(rs_labels) TYPE zif_abapgit_diff3=>ty_labels.

  PRIVATE SECTION.

    METHODS _reverse
      IMPORTING
        !it_data         TYPE string_table
      RETURNING
        VALUE(rt_result) TYPE string_table.

    METHODS _slice
      IMPORTING
        !it_data         TYPE string_table
        !iv_start        TYPE i
        !iv_end          TYPE i
      RETURNING
        VALUE(rt_result) TYPE string_table.

ENDCLASS.



CLASS zcl_abapgit_diff3 IMPLEMENTATION.


  METHOD add_hunk.
    " hunks are array subsets where `a` or `b` are different from `o`
    " https://www.gnu.org/software/diffutils/manual/html_node/diff3-Hunks.html

    DATA ls_hunk LIKE LINE OF ct_hunks.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF it_buffer.
    LOOP AT it_buffer ASSIGNING <ls_item>.
      ls_hunk-ab        = iv_ab.
      ls_hunk-o_start   = <ls_item>-buffer1-key.
      ls_hunk-o_length  = <ls_item>-buffer1-len.
      ls_hunk-ab_start  = <ls_item>-buffer2-key.
      ls_hunk-ab_length = <ls_item>-buffer2-len.
      INSERT ls_hunk INTO TABLE ct_hunks.
    ENDLOOP.

  ENDMETHOD.


  METHOD advance_to.

    DATA ls_result LIKE LINE OF ct_results.

    IF iv_end_offset > cv_curr_offset.
      ls_result-stable = abap_true.
      CLEAR ls_result-stable_region.
      ls_result-stable_region-buffer = 'o'.
      ls_result-stable_region-buffer_start = cv_curr_offset.
      ls_result-stable_region-buffer_length = iv_end_offset - cv_curr_offset.
      ls_result-stable_region-buffer_content = _slice(
it_data = it_o
iv_start = cv_curr_offset
iv_end = iv_end_offset ).
      INSERT ls_result INTO TABLE ct_results.
      cv_curr_offset = iv_end_offset.
    ENDIF.

  ENDMETHOD.


  METHOD chunk_description.
    DATA temp1 LIKE LINE OF it_buffer.
    DATA temp2 LIKE sy-tabix.

    rs_result-offset = iv_offset.
    rs_result-length = iv_length.

    DO iv_length TIMES.


      temp2 = sy-tabix.
      READ TABLE it_buffer INDEX iv_offset + sy-index INTO temp1.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_error.
      ENDIF.
      APPEND temp1 TO rs_result-chunk.
    ENDDO.

  ENDMETHOD.


  METHOD convert_to_abap_indices.

    FIELD-SYMBOLS <ls_diff_indices> LIKE LINE OF ct_diff_indices.
    LOOP AT ct_diff_indices ASSIGNING <ls_diff_indices>.
      <ls_diff_indices>-buffer1-key = <ls_diff_indices>-buffer1-key + 1.
      <ls_diff_indices>-buffer2-key = <ls_diff_indices>-buffer2-key + 1.
    ENDLOOP.

  ENDMETHOD.


  METHOD create.

    CREATE OBJECT ri_result TYPE zcl_abapgit_diff3.

  ENDMETHOD.


  METHOD flush_ok.

    DATA ls_result LIKE LINE OF ct_result.

    IF ct_buffer IS NOT INITIAL.
      INSERT LINES OF ct_buffer INTO TABLE ls_result-ok.
      INSERT ls_result INTO TABLE ct_result.
      CLEAR ct_buffer.
    ENDIF.

  ENDMETHOD.


  METHOD get_labels.

    CLEAR rs_labels.
    rs_labels-a = `<<<<<<<`.
    rs_labels-o = `|||||||`.
    rs_labels-x = `=======`.
    rs_labels-b = `>>>>>>>`.

    IF is_labels-a IS NOT INITIAL.
      rs_labels-a = rs_labels-a && | { is_labels-a }|.
    ENDIF.
    IF is_labels-o IS NOT INITIAL.
      rs_labels-o = rs_labels-o && | { is_labels-o }|.
    ENDIF.
    IF is_labels-b IS NOT INITIAL.
      rs_labels-b = rs_labels-b && | { is_labels-b }|.
    ENDIF.

  ENDMETHOD.


  METHOD process_common.
    DATA temp3 TYPE zif_abapgit_diff3=>ty_comm_result.
    DATA ls_res LIKE temp3.

    IF ct_common IS NOT INITIAL.

      CLEAR temp3.
      temp3-common = _reverse( ct_common ).

      ls_res = temp3.
      INSERT ls_res INTO ct_result INDEX 1.
      CLEAR ct_common.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_diff3~diff3_merge.
    " Applies the output of diff3MergeRegions to actually
    " construct the merged buffer; the returned result alternates
    " between 'ok' and 'conflict' blocks.
    " A "false conflict" is where `a` and `b` both change the same from `o`

    DATA ls_result LIKE LINE OF rt_result.
    DATA lt_ok_buffer TYPE string_table.

    DATA lt_regions TYPE zif_abapgit_diff3=>ty_region_t.
    FIELD-SYMBOLS <ls_region> LIKE LINE OF lt_regions.
    lt_regions = zif_abapgit_diff3~diff3_merge_regions(
      it_a = it_a
      it_o = it_o
      it_b = it_b ).


    LOOP AT lt_regions ASSIGNING <ls_region>.
      IF <ls_region>-stable = abap_true.
        INSERT LINES OF <ls_region>-stable_region-buffer_content INTO TABLE lt_ok_buffer.
      ELSE.
        IF iv_exclude_false_conflicts = abap_true AND
          <ls_region>-unstable_region-a_length  = <ls_region>-unstable_region-b_length AND
          <ls_region>-unstable_region-a_content = <ls_region>-unstable_region-b_content.
          INSERT LINES OF <ls_region>-unstable_region-a_content INTO TABLE lt_ok_buffer.
        ELSE.
          flush_ok(
            CHANGING
              ct_buffer = lt_ok_buffer
              ct_result = rt_result ).

          CLEAR ls_result.
          CLEAR ls_result-conflict.
          ls_result-conflict-a = <ls_region>-unstable_region-a_content.
          ls_result-conflict-a_index = <ls_region>-unstable_region-a_start.
          ls_result-conflict-o = <ls_region>-unstable_region-o_content.
          ls_result-conflict-o_index = <ls_region>-unstable_region-o_start.
          ls_result-conflict-b = <ls_region>-unstable_region-b_content.
          ls_result-conflict-b_index = <ls_region>-unstable_region-b_start.
          INSERT ls_result INTO TABLE rt_result.
        ENDIF.
      ENDIF.
    ENDLOOP.

    flush_ok(
      CHANGING
        ct_buffer = lt_ok_buffer
        ct_result = rt_result ).

  ENDMETHOD.


  METHOD zif_abapgit_diff3~diff3_merge_regions.
    " Given three buffers, A, O, and B, where both A and B are
    " independently derived from O, returns a fairly complicated
    " internal representation of merge decisions it's taken. The
    " interested reader may wish to consult
    "
    " Sanjeev Khanna, Keshav Kunal, and Benjamin C. Pierce.
    " 'A Formal Investigation of ' In Arvind and Prasad,
    " editors, Foundations of Software Technology and Theoretical
    " Computer Science (FSTTCS), December 2007.
    "
    " (http://www.cis.upenn.edu/~bcpierce/papers/diff3-short.pdf)

    TYPES:
      BEGIN OF ty_bound,
        n0 TYPE zif_abapgit_diff3=>ty_number,
        n1 TYPE zif_abapgit_diff3=>ty_number,
        n2 TYPE zif_abapgit_diff3=>ty_number,
        n3 TYPE zif_abapgit_diff3=>ty_number,
      END OF ty_bound.

    DATA:
      ls_result       LIKE LINE OF rt_result,
      lt_hunks        TYPE STANDARD TABLE OF ty_hunk WITH DEFAULT KEY,
      lt_region_hunks TYPE STANDARD TABLE OF ty_hunk WITH DEFAULT KEY.

    DATA:
      BEGIN OF ls_bounds,
        a TYPE ty_bound,
        b TYPE ty_bound,
      END OF ls_bounds.
    DATA lv_curr_offset TYPE i.
    DATA lv_hunk TYPE i.
    DATA ls_hunk LIKE LINE OF lt_hunks.
    DATA temp1 LIKE LINE OF lt_hunks.
    DATA temp2 LIKE sy-tabix.
    DATA lv_region_start LIKE ls_hunk-o_start.
    DATA lv_region_end TYPE zif_abapgit_diff3=>ty_number.
    DATA ls_next_hunk LIKE LINE OF lt_hunks.
    DATA temp3 LIKE LINE OF lt_hunks.
    DATA temp7 LIKE sy-tabix.
    DATA lv_next_hunk_start LIKE ls_next_hunk-o_start.
    FIELD-SYMBOLS <lt_buffer> TYPE string_table.
    DATA lv_region_hunk TYPE i.
    DATA temp4 LIKE LINE OF lt_region_hunks.
    DATA temp5 LIKE sy-tabix.
    DATA lv_o_start LIKE ls_hunk-o_start.
    DATA lv_o_end TYPE zif_abapgit_diff3=>ty_number.
    DATA lv_ab_start LIKE ls_hunk-ab_start.
    DATA lv_ab_end TYPE zif_abapgit_diff3=>ty_number.
    FIELD-SYMBOLS <ls_b> TYPE ty_bound.
    DATA temp6 TYPE ty_bound.
    DATA ls_b LIKE temp6.
    DATA lv_a_start TYPE zif_abapgit_diff3=>ty_number.
    DATA lv_a_end TYPE zif_abapgit_diff3=>ty_number.
    DATA lv_b_start TYPE zif_abapgit_diff3=>ty_number.
    DATA lv_b_end TYPE zif_abapgit_diff3=>ty_number.

    add_hunk(
      EXPORTING
        it_buffer = zif_abapgit_diff3~diff_indices(
                      it_buffer1 = it_o
                      it_buffer2 = it_a )
        iv_ab     = 'a'
      CHANGING
        ct_hunks  = lt_hunks ).

    add_hunk(
      EXPORTING
        it_buffer = zif_abapgit_diff3~diff_indices(
                      it_buffer1 = it_o
                      it_buffer2 = it_b )
        iv_ab     = 'b'
      CHANGING
        ct_hunks  = lt_hunks ).

    SORT lt_hunks BY o_start ab.


    lv_curr_offset = 0.


    lv_hunk = 0.
    WHILE lv_hunk < lines( lt_hunks ).



      temp2 = sy-tabix.
      READ TABLE lt_hunks INDEX lv_hunk + 1 INTO temp1.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_error.
      ENDIF.
      ls_hunk = temp1.
      lv_hunk = lv_hunk + 1.


      lv_region_start = ls_hunk-o_start.

      lv_region_end   = ls_hunk-o_start + ls_hunk-o_length.

      CLEAR lt_region_hunks.
      INSERT ls_hunk INTO TABLE lt_region_hunks.

      advance_to(
        EXPORTING
          iv_end_offset  = lv_region_start
          it_o           = it_o
        CHANGING
          cv_curr_offset = lv_curr_offset
          ct_results     = rt_result ).

      " Try to pull next overlapping hunk into this region
      WHILE lv_hunk < lines( lt_hunks ).



        temp7 = sy-tabix.
        READ TABLE lt_hunks INDEX lv_hunk + 1 INTO temp3.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_error.
        ENDIF.
        ls_next_hunk = temp3.

        lv_next_hunk_start = ls_next_hunk-o_start.

        IF lv_next_hunk_start > lv_region_end.
          EXIT.          " no overlap
        ENDIF.

        lv_region_end = nmax(
          val1 = lv_region_end
          val2 = lv_next_hunk_start + ls_next_hunk-o_length ).

        INSERT ls_next_hunk INTO TABLE lt_region_hunks.
        lv_hunk = lv_hunk + 1.
      ENDWHILE.

      IF lines( lt_region_hunks ) = 1.
        " Only one hunk touches this region, meaning that there is no conflict here.
        " either `a` or `b` is inserting into a region of `o` unchanged by the other.
        IF ls_hunk-ab_length > 0.
          IF ls_hunk-ab = 'a'.

            ASSIGN it_a TO <lt_buffer>.
            ASSERT sy-subrc = 0.
          ELSE.
            ASSIGN it_b TO <lt_buffer>.
            ASSERT sy-subrc = 0.
          ENDIF.
          CLEAR ls_result.
          ls_result-stable = abap_true.
          CLEAR ls_result-stable_region.
          ls_result-stable_region-buffer = ls_hunk-ab.
          ls_result-stable_region-buffer_start = ls_hunk-ab_start.
          ls_result-stable_region-buffer_length = ls_hunk-ab_length.
          ls_result-stable_region-buffer_content = _slice(
it_data = <lt_buffer>
iv_start = ls_hunk-ab_start
iv_end = ls_hunk-ab_start + ls_hunk-ab_length ).
          INSERT ls_result INTO TABLE rt_result.
        ENDIF.
      ELSE.
        " a true a/b conflict. determine the bounds involved from `a`, `o`, and `b`.
        " effectively merge all the `a` hunks into one giant hunk, then do the
        " same for the `b` hunks. then, correct for skew in the regions of `o`
        " that each side changed, and report appropriate spans for the three sides.
        CLEAR ls_bounds.
        CLEAR ls_bounds-a.
        ls_bounds-a-n0 = lines( it_a ).
        ls_bounds-a-n1 = -1.
        ls_bounds-a-n2 = lines( it_o ).
        ls_bounds-a-n3 = -1.
        CLEAR ls_bounds-b.
        ls_bounds-b-n0 = lines( it_b ).
        ls_bounds-b-n1 = -1.
        ls_bounds-b-n2 = lines( it_o ).
        ls_bounds-b-n3 = -1.


        lv_region_hunk = 0.
        WHILE lv_region_hunk < lines( lt_region_hunks ).


          temp5 = sy-tabix.
          READ TABLE lt_region_hunks INDEX lv_region_hunk + 1 INTO temp4.

          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_sy_itab_error.
          ENDIF.
          ls_hunk = temp4.
          lv_region_hunk = lv_region_hunk + 1.


          lv_o_start = ls_hunk-o_start.

          lv_o_end    = lv_o_start + ls_hunk-o_length.

          lv_ab_start = ls_hunk-ab_start.

          lv_ab_end   = lv_ab_start + ls_hunk-ab_length.

          IF ls_hunk-ab = 'a'.

            ASSIGN ls_bounds-a TO <ls_b>.
            ASSERT sy-subrc = 0.
          ELSE.
            ASSIGN ls_bounds-b TO <ls_b>.
            ASSERT sy-subrc = 0.
          ENDIF.


          CLEAR temp6.
          temp6-n0 = nmin( val1 = lv_ab_start
                           val2 = <ls_b>-n0 ).
          temp6-n1 = nmax( val1 = lv_ab_end
                           val2 = <ls_b>-n1 ).
          temp6-n2 = nmin( val1 = lv_o_start
                           val2 = <ls_b>-n2 ).
          temp6-n3 = nmax( val1 = lv_o_end
                           val2 = <ls_b>-n3 ).

          ls_b = temp6.
          <ls_b> = ls_b.
        ENDWHILE.


        lv_a_start = ls_bounds-a-n0 + lv_region_start - ls_bounds-a-n2.

        lv_a_end   = ls_bounds-a-n1 + lv_region_end - ls_bounds-a-n3.

        lv_b_start = ls_bounds-b-n0 + lv_region_start - ls_bounds-b-n2.

        lv_b_end   = ls_bounds-b-n1 + lv_region_end - ls_bounds-b-n3.

        CLEAR ls_result.
        ls_result-stable = abap_false.
        CLEAR ls_result-unstable_region.
        ls_result-unstable_region-a_start = lv_a_start.
        ls_result-unstable_region-a_length = lv_a_end - lv_a_start.
        ls_result-unstable_region-a_content = _slice(
it_data = it_a
iv_start = lv_a_start
iv_end = lv_a_end ).
        ls_result-unstable_region-o_start = lv_region_start.
        ls_result-unstable_region-o_length = lv_region_end - lv_region_start.
        ls_result-unstable_region-o_content = _slice(
it_data = it_o
iv_start = lv_region_start
iv_end = lv_region_end ).
        ls_result-unstable_region-b_start = lv_b_start.
        ls_result-unstable_region-b_length = lv_b_end - lv_b_start.
        ls_result-unstable_region-b_content = _slice(
it_data = it_b
iv_start = lv_b_start
iv_end = lv_b_end ).
        INSERT ls_result INTO TABLE rt_result.
      ENDIF.

      lv_curr_offset = lv_region_end.
    ENDWHILE.

    advance_to(
      EXPORTING
        iv_end_offset  = lines( it_o )
        it_o           = it_o
      CHANGING
        cv_curr_offset = lv_curr_offset
        ct_results     = rt_result ).

  ENDMETHOD.


  METHOD zif_abapgit_diff3~diff_comm.
    " We apply the LCS to build a 'comm'-style picture of the
    " differences between buffer1 and buffer2.

    DATA:
      ls_res       LIKE LINE OF rt_result,
      ls_different TYPE zif_abapgit_diff3=>ty_comm_result-diff,
      lt_common    TYPE zif_abapgit_diff3=>ty_comm_result-common.

    DATA lt_lcs TYPE zif_abapgit_diff3=>ty_lcs_result_t.
    DATA lv_tail1 TYPE i.
    DATA lv_tail2 TYPE i.
    DATA ls_candidate LIKE LINE OF lt_lcs.
    DATA temp15 LIKE LINE OF lt_lcs.
    DATA temp16 LIKE sy-tabix.
    DATA temp7 LIKE LINE OF it_buffer1.
    DATA temp8 LIKE sy-tabix.
    DATA temp9 LIKE LINE OF it_buffer2.
    DATA temp10 LIKE sy-tabix.
    DATA temp11 LIKE LINE OF it_buffer1.
    DATA temp12 LIKE sy-tabix.
    DATA temp13 LIKE LINE OF lt_lcs.
    DATA temp14 LIKE sy-tabix.
    lt_lcs = zif_abapgit_diff3~lcs( it_buffer1 = it_buffer1
                                         it_buffer2 = it_buffer2 ).


    lv_tail1 = lines( it_buffer1 ).

    lv_tail2 = lines( it_buffer2 ).



    temp16 = sy-tabix.
    READ TABLE lt_lcs WITH KEY key = lines( lt_lcs ) - 1 INTO temp15.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_error.
    ENDIF.
    ls_candidate = temp15.
    DO.
      CLEAR ls_different.

      DO.
        lv_tail1 = lv_tail1 - 1.
        IF lv_tail1 <= ls_candidate-buffer1index.
          EXIT.
        ENDIF.


        temp8 = sy-tabix.
        READ TABLE it_buffer1 INDEX lv_tail1 + 1 INTO temp7.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_error.
        ENDIF.
        INSERT temp7 INTO TABLE ls_different-buffer1.
      ENDDO.

      DO.
        lv_tail2 = lv_tail2 - 1.
        IF lv_tail2 <= ls_candidate-buffer2index.
          EXIT.
        ENDIF.


        temp10 = sy-tabix.
        READ TABLE it_buffer2 INDEX lv_tail2 + 1 INTO temp9.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_error.
        ENDIF.
        INSERT temp9 INTO TABLE ls_different-buffer2.
      ENDDO.

      IF lines( ls_different-buffer1 ) > 0 OR lines( ls_different-buffer2 ) > 0.
        process_common(
          CHANGING
            ct_common = lt_common
            ct_result = rt_result ).

        CLEAR ls_res.
        ls_res-diff-buffer1 = _reverse( ls_different-buffer1 ).
        ls_res-diff-buffer2 = _reverse( ls_different-buffer2 ).
        INSERT ls_res INTO rt_result INDEX 1.
      ENDIF.

      IF lv_tail1 >= 0.


        temp12 = sy-tabix.
        READ TABLE it_buffer1 INDEX lv_tail1 + 1 INTO temp11.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_error.
        ENDIF.
        INSERT temp11 INTO TABLE lt_common.
      ENDIF.

      IF ls_candidate-chain = -1.
        EXIT.
      ENDIF.



      temp14 = sy-tabix.
      READ TABLE lt_lcs WITH KEY key = ls_candidate-chain INTO temp13.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_error.
      ENDIF.
      ls_candidate = temp13.
    ENDDO.

    process_common(
      CHANGING
        ct_common = lt_common
        ct_result = rt_result ).

  ENDMETHOD.


  METHOD zif_abapgit_diff3~diff_indices.
    " We apply the LCS to give a simple representation of the
    " offsets and lengths of mismatched chunks in the input
    " buffers. This is used by diff3MergeRegions.

    DATA lt_lcs TYPE zif_abapgit_diff3=>ty_lcs_result_t.
    DATA lv_tail1 TYPE i.
    DATA lv_tail2 TYPE i.
    DATA ls_candidate LIKE LINE OF lt_lcs.
    DATA temp18 LIKE LINE OF lt_lcs.
    DATA temp19 LIKE sy-tabix.
    DATA lv_mismatch_length1 TYPE i.
    DATA lv_mismatch_length2 TYPE i.
    DATA temp15 TYPE zif_abapgit_diff3=>ty_diff_indices_result.
    DATA ls_result LIKE temp15.
    DATA temp16 LIKE LINE OF lt_lcs.
    DATA temp17 LIKE sy-tabix.
    lt_lcs = zif_abapgit_diff3~lcs(
      it_buffer1 = it_buffer1
      it_buffer2 = it_buffer2 ).


    lv_tail1 = lines( it_buffer1 ).

    lv_tail2 = lines( it_buffer2 ).



    temp19 = sy-tabix.
    READ TABLE lt_lcs WITH KEY key = lines( lt_lcs ) - 1 INTO temp18.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_error.
    ENDIF.
    ls_candidate = temp18.
    DO.
      IF ls_candidate-chain = -1.
        EXIT.
      ENDIF.


      lv_mismatch_length1 = lv_tail1 - ls_candidate-buffer1index - 1.

      lv_mismatch_length2 = lv_tail2 - ls_candidate-buffer2index - 1.
      lv_tail1 = ls_candidate-buffer1index.
      lv_tail2 = ls_candidate-buffer2index.

      IF lv_mismatch_length1 > 0 OR lv_mismatch_length2 > 0.

        CLEAR temp15.
        temp15-buffer1-key = lv_tail1 + 1.
        temp15-buffer1-len = lv_mismatch_length1.
        temp15-buffer1content = _slice( it_data = it_buffer1
                                        iv_start = lv_tail1 + 1
                                        iv_end = lv_tail1 + 1 + lv_mismatch_length1 ).
        temp15-buffer2-key = lv_tail2 + 1.
        temp15-buffer2-len = lv_mismatch_length2.
        temp15-buffer2content = _slice( it_data = it_buffer2
                                        iv_start = lv_tail2 + 1
                                        iv_end = lv_tail2 + 1 + lv_mismatch_length2 ).

        ls_result = temp15.
        INSERT ls_result INTO rt_result INDEX 1.
      ENDIF.



      temp17 = sy-tabix.
      READ TABLE lt_lcs WITH KEY key = ls_candidate-chain INTO temp16.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_error.
      ENDIF.
      ls_candidate = temp16.
    ENDDO.

  ENDMETHOD.


  METHOD zif_abapgit_diff3~diff_patch.
    " We apply the LCS to build a JSON representation of a
    " diff(1)-style patch.

    DATA ls_result LIKE LINE OF rt_result.

    DATA lt_lcs TYPE zif_abapgit_diff3=>ty_lcs_result_t.
    DATA lv_tail1 TYPE i.
    DATA lv_tail2 TYPE i.
    DATA ls_candidate LIKE LINE OF lt_lcs.
    DATA temp20 LIKE LINE OF lt_lcs.
    DATA temp21 LIKE sy-tabix.
    DATA lv_mismatchlength1 TYPE i.
    DATA lv_mismatchlength2 TYPE i.
    DATA temp18 LIKE LINE OF lt_lcs.
    DATA temp19 LIKE sy-tabix.
    lt_lcs = zif_abapgit_diff3~lcs( it_buffer1 = it_buffer1
                                         it_buffer2 = it_buffer2 ).


    lv_tail1 = lines( it_buffer1 ).

    lv_tail2 = lines( it_buffer2 ).



    temp21 = sy-tabix.
    READ TABLE lt_lcs WITH KEY key = lines( lt_lcs ) - 1 INTO temp20.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_error.
    ENDIF.
    ls_candidate = temp20.
    DO.
      IF ls_candidate-chain = -1.
        EXIT.
      ENDIF.


      lv_mismatchlength1 = lv_tail1 - ls_candidate-buffer1index - 1.

      lv_mismatchlength2 = lv_tail2 - ls_candidate-buffer2index - 1.
      lv_tail1 = ls_candidate-buffer1index.
      lv_tail2 = ls_candidate-buffer2index.

      IF lv_mismatchlength1 > 0 OR lv_mismatchlength2 > 0.
        CLEAR ls_result.
        ls_result-buffer1 = chunk_description( it_buffer = it_buffer1
                                               iv_offset = lv_tail1 + 1
                                               iv_length = lv_mismatchlength1 ).
        ls_result-buffer2 = chunk_description( it_buffer = it_buffer2
                                               iv_offset = lv_tail2 + 1
                                               iv_length = lv_mismatchlength2 ).
        INSERT ls_result INTO rt_result INDEX 1.
      ENDIF.



      temp19 = sy-tabix.
      READ TABLE lt_lcs WITH KEY key = ls_candidate-chain INTO temp18.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_error.
      ENDIF.
      ls_candidate = temp18.
    ENDDO.

  ENDMETHOD.


  METHOD zif_abapgit_diff3~invert_patch.
    " Takes the output of diffPatch(), and inverts the sense of it, so that it
    " can be applied to buffer2 to give buffer1 rather than the other way around.

    DATA ls_result LIKE LINE OF rt_result.

    FIELD-SYMBOLS <ls_patch> LIKE LINE OF it_patchres.
    LOOP AT it_patchres ASSIGNING <ls_patch>.
      CLEAR ls_result.
      ls_result-buffer1 = <ls_patch>-buffer2.
      ls_result-buffer2 = <ls_patch>-buffer1.
      INSERT ls_result INTO TABLE rt_result.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_diff3~lcs.
    " Text diff algorithm following Hunt and McIlroy 1976.
    " J. W. Hunt and M. D. McIlroy, An algorithm for differential buffer
    " comparison, Bell Telephone Laboratories CSTR #41 (1976)
    " http:"www.cs.dartmouth.edu/~doug/
    " https:"en.wikipedia.org/wiki/Longest_common_subsequence_problem
    "
    " Expects two arrays, finds longest common sequence

    TYPES:
      BEGIN OF ty_equivalenceclass,
        key    TYPE string,
        values TYPE zif_abapgit_diff3=>ty_numbers,
      END OF ty_equivalenceclass.

    DATA:
      lt_equivalenceclasses TYPE HASHED TABLE OF ty_equivalenceclass WITH UNIQUE KEY key,
      lt_candidates         TYPE zif_abapgit_diff3=>ty_lcs_result_t,
      ls_newcandidate       TYPE zif_abapgit_diff3=>ty_lcs_result.

    DATA lv_j TYPE i.
    FIELD-SYMBOLS <lv_buffer2> LIKE LINE OF it_buffer2.
    FIELD-SYMBOLS <ls_equivalentclass> TYPE ty_equivalenceclass.
    DATA temp20 TYPE ty_equivalenceclass.
    DATA ls_equivalenceclass LIKE temp20.
    DATA temp21 TYPE zif_abapgit_diff3=>ty_lcs_result.
    DATA ls_nullresult LIKE temp21.
    DATA lv_i TYPE i.
    FIELD-SYMBOLS <lv_buffer1> LIKE LINE OF it_buffer1.
    DATA temp22 LIKE sy-subrc.
    DATA lt_buffer2indices TYPE zif_abapgit_diff3=>ty_numbers.
    DATA temp27 LIKE LINE OF lt_equivalenceclasses.
    DATA temp29 LIKE sy-tabix.
    DATA lv_r TYPE i.
    DATA ls_c LIKE LINE OF lt_candidates.
    DATA temp31 LIKE LINE OF lt_candidates.
    DATA temp34 LIKE sy-tabix.
    DATA lv_s LIKE lv_r.
    DATA temp23 LIKE LINE OF lt_candidates.
    DATA temp24 LIKE sy-tabix.
    DATA temp25 LIKE LINE OF lt_candidates.
    DATA temp26 LIKE sy-tabix.
    FIELD-SYMBOLS <temp27> LIKE LINE OF lt_candidates.
    DATA temp28 LIKE sy-tabix.
    FIELD-SYMBOLS <temp29> LIKE LINE OF lt_candidates.
    DATA temp30 LIKE sy-tabix.
    FIELD-SYMBOLS <temp31> LIKE LINE OF lt_candidates.
    DATA temp32 LIKE sy-tabix.
    DATA temp33 LIKE sy-subrc.
    FIELD-SYMBOLS <temp34> LIKE LINE OF lt_candidates.
    DATA temp35 LIKE sy-tabix.
    FIELD-SYMBOLS <temp36> LIKE LINE OF lt_candidates.
    DATA temp37 LIKE sy-tabix.
    FIELD-SYMBOLS <temp38> LIKE LINE OF lt_candidates.
    DATA temp39 LIKE sy-tabix.
    lv_j = 0.

    LOOP AT it_buffer2 ASSIGNING <lv_buffer2>.

      READ TABLE lt_equivalenceclasses ASSIGNING <ls_equivalentclass>
        WITH TABLE KEY key = <lv_buffer2>.
      IF sy-subrc <> 0.

        CLEAR temp20.
        temp20-key = <lv_buffer2>.

        ls_equivalenceclass = temp20.
        INSERT ls_equivalenceclass INTO TABLE lt_equivalenceclasses ASSIGNING <ls_equivalentclass>.
      ENDIF.
      INSERT lv_j INTO TABLE <ls_equivalentclass>-values.
      lv_j = lv_j + 1.
    ENDLOOP.


    CLEAR temp21.
    temp21-key = 0.
    temp21-buffer1index = -1.
    temp21-buffer2index = -1.
    temp21-chain = -1.

    ls_nullresult = temp21.
    INSERT ls_nullresult INTO TABLE lt_candidates.


    lv_i = 0.

    LOOP AT it_buffer1 ASSIGNING <lv_buffer1>.

      READ TABLE lt_equivalenceclasses WITH KEY key = <lv_buffer1> TRANSPORTING NO FIELDS.
      temp22 = sy-subrc.
      IF temp22 = 0.



        temp29 = sy-tabix.
        READ TABLE lt_equivalenceclasses WITH KEY key = <lv_buffer1> INTO temp27.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_error.
        ENDIF.
        lt_buffer2indices = temp27-values.
      ELSE.
        CLEAR lt_buffer2indices.
      ENDIF.


      lv_r = 0.



      temp34 = sy-tabix.
      READ TABLE lt_candidates WITH KEY key = 0 INTO temp31.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_error.
      ENDIF.
      ls_c = temp31.
      LOOP AT lt_buffer2indices INTO lv_j.


        lv_s = lv_r.
        DO.
          IF lv_s < lines( lt_candidates ).


            temp24 = sy-tabix.
            READ TABLE lt_candidates WITH KEY key = lv_s INTO temp23.

            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE cx_sy_itab_error.
            ENDIF.
            IF temp23-buffer2index < lv_j.
              IF lv_s = lines( lt_candidates ) - 1.
                EXIT.
              ENDIF.


              temp26 = sy-tabix.
              READ TABLE lt_candidates WITH KEY key = lv_s + 1 INTO temp25.

              IF sy-subrc <> 0.
                RAISE EXCEPTION TYPE cx_sy_itab_error.
              ENDIF.
              IF temp25-buffer2index > lv_j.
                EXIT.
              ENDIF.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.
          lv_s = lv_s + 1.
        ENDDO.

        IF lv_s < lines( lt_candidates ).
          ls_newcandidate-buffer1index = lv_i.
          ls_newcandidate-buffer2index = lv_j.
          ls_newcandidate-chain        = lv_s.

          IF lv_r = lines( lt_candidates ).
            ls_c-key = lines( lt_candidates ) + 1.
            INSERT ls_c INTO TABLE lt_candidates.
          ELSE.


            temp28 = sy-tabix.
            READ TABLE lt_candidates WITH KEY key = lv_r ASSIGNING <temp27>.

            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE cx_sy_itab_error.
            ENDIF.
            <temp27>-buffer1index = ls_c-buffer1index.


            temp30 = sy-tabix.
            READ TABLE lt_candidates WITH KEY key = lv_r ASSIGNING <temp29>.

            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE cx_sy_itab_error.
            ENDIF.
            <temp29>-buffer2index = ls_c-buffer2index.


            temp32 = sy-tabix.
            READ TABLE lt_candidates WITH KEY key = lv_r ASSIGNING <temp31>.

            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE cx_sy_itab_error.
            ENDIF.
            <temp31>-chain        = ls_c-chain.
          ENDIF.

          lv_r = lv_s + 1.
          ls_c = ls_newcandidate.

          IF lv_r = lines( lt_candidates ).
            EXIT. " no point in examining further (j)s
          ENDIF.
        ENDIF.
      ENDLOOP.


      READ TABLE lt_candidates WITH KEY key = lv_r TRANSPORTING NO FIELDS.
      temp33 = sy-subrc.
      IF temp33 = 0.


        temp35 = sy-tabix.
        READ TABLE lt_candidates WITH KEY key = lv_r ASSIGNING <temp34>.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_error.
        ENDIF.
        <temp34>-buffer1index = ls_c-buffer1index.


        temp37 = sy-tabix.
        READ TABLE lt_candidates WITH KEY key = lv_r ASSIGNING <temp36>.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_error.
        ENDIF.
        <temp36>-buffer2index = ls_c-buffer2index.


        temp39 = sy-tabix.
        READ TABLE lt_candidates WITH KEY key = lv_r ASSIGNING <temp38>.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_error.
        ENDIF.
        <temp38>-chain        = ls_c-chain.
      ELSE.
        ls_c-key = lv_r.
        INSERT ls_c INTO TABLE lt_candidates.
      ENDIF.

      lv_i = lv_i + 1.
    ENDLOOP.

    " At this point, we know the LCS: it's in the reverse of the
    " linked-list through chain of candidates[ lines( candidates ) - 1 ].

    rt_result = lt_candidates.

  ENDMETHOD.


  METHOD zif_abapgit_diff3~merge.

    DATA ls_labels TYPE zif_abapgit_diff3=>ty_labels.
    DATA lt_regions TYPE zif_abapgit_diff3=>ty_merge_region_t.
    FIELD-SYMBOLS <ls_region> LIKE LINE OF lt_regions.
    ls_labels = get_labels( is_labels ).


    lt_regions = zif_abapgit_diff3~diff3_merge(
      it_a                       = it_a
      it_o                       = it_o
      it_b                       = it_b
      iv_exclude_false_conflicts = iv_exclude_false_conflicts ).


    LOOP AT lt_regions ASSIGNING <ls_region>.
      IF <ls_region>-ok IS NOT INITIAL.
        INSERT LINES OF <ls_region>-ok INTO TABLE rs_result-result.
      ELSEIF <ls_region>-conflict IS NOT INITIAL.
        rs_result-conflict = abap_true.
        INSERT ls_labels-a INTO TABLE rs_result-result.
        INSERT LINES OF <ls_region>-conflict-a INTO TABLE rs_result-result.
        INSERT ls_labels-x INTO TABLE rs_result-result.
        INSERT LINES OF <ls_region>-conflict-b INTO TABLE rs_result-result.
        INSERT ls_labels-b INTO TABLE rs_result-result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_diff3~merge_diff3.

    DATA ls_labels TYPE zif_abapgit_diff3=>ty_labels.
    DATA lt_regions TYPE zif_abapgit_diff3=>ty_merge_region_t.
    FIELD-SYMBOLS <ls_region> LIKE LINE OF lt_regions.
    ls_labels = get_labels( is_labels ).


    lt_regions = zif_abapgit_diff3~diff3_merge(
      it_a                       = it_a
      it_o                       = it_o
      it_b                       = it_b
      iv_exclude_false_conflicts = iv_exclude_false_conflicts ).


    LOOP AT lt_regions ASSIGNING <ls_region>.
      IF <ls_region>-ok IS NOT INITIAL.
        INSERT LINES OF <ls_region>-ok INTO TABLE rs_result-result.
      ELSEIF <ls_region>-conflict IS NOT INITIAL.
        rs_result-conflict = abap_true.
        INSERT ls_labels-a INTO TABLE rs_result-result.
        INSERT LINES OF <ls_region>-conflict-a INTO TABLE rs_result-result.
        INSERT ls_labels-o INTO TABLE rs_result-result.
        INSERT LINES OF <ls_region>-conflict-o INTO TABLE rs_result-result.
        INSERT ls_labels-x INTO TABLE rs_result-result.
        INSERT LINES OF <ls_region>-conflict-b INTO TABLE rs_result-result.
        INSERT ls_labels-b INTO TABLE rs_result-result.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_diff3~merge_dig_in.

    DATA ls_labels TYPE zif_abapgit_diff3=>ty_labels.
    DATA lt_regions TYPE zif_abapgit_diff3=>ty_merge_region_t.
    FIELD-SYMBOLS <ls_region> LIKE LINE OF lt_regions.
    DATA lt_c TYPE zif_abapgit_diff3=>ty_comm_result_t.
    FIELD-SYMBOLS <ls_c> LIKE LINE OF lt_c.
    ls_labels = get_labels( is_labels ).


    lt_regions = zif_abapgit_diff3~diff3_merge(
      it_a                       = it_a
      it_o                       = it_o
      it_b                       = it_b
      iv_exclude_false_conflicts = iv_exclude_false_conflicts ).


    LOOP AT lt_regions ASSIGNING <ls_region>.
      IF <ls_region>-ok IS NOT INITIAL.
        INSERT LINES OF <ls_region>-ok INTO TABLE rs_result-result.
      ELSE.

        lt_c = zif_abapgit_diff3~diff_comm(
          it_buffer1 = <ls_region>-conflict-a
          it_buffer2 = <ls_region>-conflict-b ).


        LOOP AT lt_c ASSIGNING <ls_c>.
          IF <ls_c>-common IS NOT INITIAL.
            INSERT LINES OF <ls_c>-common INTO TABLE rs_result-result.
          ELSE.
            rs_result-conflict = abap_true.
            INSERT ls_labels-a INTO TABLE rs_result-result.
            INSERT LINES OF <ls_c>-diff-buffer1 INTO TABLE rs_result-result.
            INSERT ls_labels-x INTO TABLE rs_result-result.
            INSERT LINES OF <ls_c>-diff-buffer2 INTO TABLE rs_result-result.
            INSERT ls_labels-b INTO TABLE rs_result-result.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_abapgit_diff3~patch.
    " Applies a patch to a buffer.
    " Given buffer1 and buffer2, `patch(buffer1, diffPatch(buffer1, buffer2))` should give buffer2.

    DATA lv_curroffset TYPE i.
    FIELD-SYMBOLS <ls_patch> LIKE LINE OF it_patchres.
    DATA temp40 LIKE LINE OF it_buffer.
    DATA temp41 LIKE sy-tabix.
    DATA temp42 LIKE LINE OF <ls_patch>-buffer2-chunk.
    DATA temp43 LIKE sy-tabix.
    DATA temp44 LIKE LINE OF it_buffer.
    DATA temp45 LIKE sy-tabix.
    lv_curroffset = 0.


    LOOP AT it_patchres ASSIGNING <ls_patch>.
      WHILE lv_curroffset < <ls_patch>-buffer1-offset.


        temp41 = sy-tabix.
        READ TABLE it_buffer INDEX lv_curroffset + 1 INTO temp40.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_error.
        ENDIF.
        APPEND temp40 TO rt_result.
        lv_curroffset = lv_curroffset + 1.
      ENDWHILE.

      DO lines( <ls_patch>-buffer2-chunk ) TIMES.


        temp43 = sy-tabix.
        READ TABLE <ls_patch>-buffer2-chunk INDEX sy-index INTO temp42.

        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_sy_itab_error.
        ENDIF.
        APPEND temp42 TO rt_result.
      ENDDO.

      lv_curroffset = lv_curroffset + <ls_patch>-buffer1-length.
    ENDLOOP.

    WHILE lv_curroffset < lines( it_buffer ).


      temp45 = sy-tabix.
      READ TABLE it_buffer INDEX lv_curroffset + 1 INTO temp44.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_error.
      ENDIF.
      APPEND temp44 TO rt_result.
      lv_curroffset = lv_curroffset + 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD zif_abapgit_diff3~strip_patch.
    " Takes the output of diffPatch(), and removes extra information from it.
    " It can still be used by patch(), below, but can no longer be inverted.

    DATA ls_result LIKE LINE OF rt_result.

    FIELD-SYMBOLS <ls_patch> LIKE LINE OF it_patchres.
    LOOP AT it_patchres ASSIGNING <ls_patch>.
      CLEAR ls_result.
      ls_result-buffer1-offset = <ls_patch>-buffer1-offset.
      ls_result-buffer1-length = <ls_patch>-buffer1-length.
      ls_result-buffer2-chunk  = <ls_patch>-buffer2-chunk.
      INSERT ls_result INTO TABLE rt_result.
    ENDLOOP.

  ENDMETHOD.


  METHOD _reverse.

    DATA lv_line TYPE i.
    DATA temp46 LIKE LINE OF it_data.
    DATA temp47 LIKE sy-tabix.
    lv_line = lines( it_data ).

    DO lines( it_data ) TIMES.


      temp47 = sy-tabix.
      READ TABLE it_data INDEX lv_line INTO temp46.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_sy_itab_error.
      ENDIF.
      INSERT temp46 INTO TABLE rt_result.
      lv_line = lv_line - 1.
    ENDDO.

  ENDMETHOD.


  METHOD _slice.

    " select from start to end (end not included!)
    FIELD-SYMBOLS <ls_data> LIKE LINE OF it_data.
    LOOP AT it_data ASSIGNING <ls_data> FROM iv_start + 1 TO iv_end.
      APPEND <ls_data> TO rt_result.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
