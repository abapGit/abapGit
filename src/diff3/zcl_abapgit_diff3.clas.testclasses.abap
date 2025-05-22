************************************************************************
* Helper Class
************************************************************************
CLASS lcl_helper DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS
      split
        IMPORTING
          iv_string        TYPE string
        RETURNING
          VALUE(rt_result) TYPE string_table.

    CLASS-METHODS
      concat
        IMPORTING
          it_strings       TYPE string_table
        RETURNING
          VALUE(rv_result) TYPE string.

ENDCLASS.

CLASS lcl_helper IMPLEMENTATION.

  METHOD split.
    IF iv_string CS '\n'.
      SPLIT iv_string AT '\n' INTO TABLE rt_result.
    ELSE.
      SPLIT iv_string AT space INTO TABLE rt_result.
    ENDIF.
  ENDMETHOD.

  METHOD concat.
    CONCATENATE LINES OF it_strings INTO rv_result SEPARATED BY space.
  ENDMETHOD.

ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/LCS.test.js
**********************************************************************
CLASS ltcl_lcs DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      test FOR TESTING.

ENDCLASS.

CLASS ltcl_lcs IMPLEMENTATION.

  METHOD test.

    " returns the LCS of two arrays
    DATA lt_lcs TYPE zif_abapgit_diff3=>ty_lcs_result_t.
    DATA ls_result LIKE LINE OF lt_lcs.
    DATA temp35 LIKE LINE OF lt_lcs.
    DATA temp36 LIKE sy-tabix.
    DATA temp48 LIKE LINE OF lt_lcs.
    DATA temp49 LIKE sy-tabix.
    DATA temp50 LIKE LINE OF lt_lcs.
    DATA temp51 LIKE sy-tabix.
    DATA temp52 LIKE LINE OF lt_lcs.
    DATA temp53 LIKE sy-tabix.
    DATA temp54 LIKE LINE OF lt_lcs.
    DATA temp55 LIKE sy-tabix.
    DATA temp56 LIKE LINE OF lt_lcs.
    DATA temp57 LIKE sy-tabix.
    DATA temp58 LIKE LINE OF lt_lcs.
    DATA temp59 LIKE sy-tabix.
    lt_lcs = zcl_abapgit_diff3=>create( )->lcs(
      it_buffer1 = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' )
      it_buffer2 = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ) ).



    temp36 = sy-tabix.
    READ TABLE lt_lcs WITH KEY key = lines( lt_lcs ) - 1 INTO temp35.
    sy-tabix = temp36.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    ls_result = temp35.

    " '99'
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer1index
      exp = 10 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer2index
      exp = 9 ).

    " 'M'


    temp49 = sy-tabix.
    READ TABLE lt_lcs WITH KEY key = ls_result-chain INTO temp48.
    sy-tabix = temp49.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    ls_result = temp48.

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer1index
      exp = 9 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer2index
      exp = 6 ).

    " 'ZZ'


    temp51 = sy-tabix.
    READ TABLE lt_lcs WITH KEY key = ls_result-chain INTO temp50.
    sy-tabix = temp51.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    ls_result = temp50.

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer1index
      exp = 4 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer2index
      exp = 4 ).

    " 'c'


    temp53 = sy-tabix.
    READ TABLE lt_lcs WITH KEY key = ls_result-chain INTO temp52.
    sy-tabix = temp53.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    ls_result = temp52.

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer1index
      exp = 3 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer2index
      exp = 3 ).

    " 'a'


    temp55 = sy-tabix.
    READ TABLE lt_lcs WITH KEY key = ls_result-chain INTO temp54.
    sy-tabix = temp55.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    ls_result = temp54.

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer1index
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer2index
      exp = 1 ).

    " 'AA'


    temp57 = sy-tabix.
    READ TABLE lt_lcs WITH KEY key = ls_result-chain INTO temp56.
    sy-tabix = temp57.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    ls_result = temp56.

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer1index
      exp = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer2index
      exp = 0 ).

    " end


    temp59 = sy-tabix.
    READ TABLE lt_lcs WITH KEY key = ls_result-chain INTO temp58.
    sy-tabix = temp59.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    ls_result = temp58.

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer1index
      exp = -1 ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-buffer2index
      exp = -1 ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/diffComm.test.js
**********************************************************************
CLASS ltcl_diff_comm DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      test FOR TESTING.

ENDCLASS.

CLASS ltcl_diff_comm IMPLEMENTATION.

  METHOD test.

    " returns a comm-style diff of two arrays
    DATA lt_result TYPE zif_abapgit_diff3=>ty_comm_result_t.
    DATA temp60 LIKE LINE OF lt_result.
    DATA temp61 LIKE sy-tabix.
    DATA temp62 LIKE LINE OF lt_result.
    DATA temp63 LIKE sy-tabix.
    DATA temp64 LIKE LINE OF lt_result.
    DATA temp65 LIKE sy-tabix.
    DATA temp66 LIKE LINE OF lt_result.
    DATA temp67 LIKE sy-tabix.
    DATA temp68 LIKE LINE OF lt_result.
    DATA temp69 LIKE sy-tabix.
    DATA temp70 LIKE LINE OF lt_result.
    DATA temp71 LIKE sy-tabix.
    DATA temp72 LIKE LINE OF lt_result.
    DATA temp73 LIKE sy-tabix.
    DATA temp74 LIKE LINE OF lt_result.
    DATA temp75 LIKE sy-tabix.
    DATA temp76 LIKE LINE OF lt_result.
    DATA temp77 LIKE sy-tabix.
    DATA temp78 LIKE LINE OF lt_result.
    DATA temp79 LIKE sy-tabix.
    DATA temp80 LIKE LINE OF lt_result.
    DATA temp81 LIKE sy-tabix.
    DATA temp82 LIKE LINE OF lt_result.
    DATA temp83 LIKE sy-tabix.
    DATA temp84 LIKE LINE OF lt_result.
    DATA temp85 LIKE sy-tabix.
    DATA temp86 LIKE LINE OF lt_result.
    DATA temp87 LIKE sy-tabix.
    DATA temp88 LIKE LINE OF lt_result.
    DATA temp89 LIKE sy-tabix.
    DATA temp90 LIKE LINE OF lt_result.
    DATA temp91 LIKE sy-tabix.
    DATA temp92 LIKE LINE OF lt_result.
    DATA temp93 LIKE sy-tabix.
    lt_result = zcl_abapgit_diff3=>create( )->diff_comm(
      it_buffer1 = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' )
      it_buffer2 = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ) ).



    temp61 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp60.
    sy-tabix = temp61.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp60-common )
      exp = 'AA a' ).


    temp63 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp62.
    sy-tabix = temp63.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp62-diff ).



    temp65 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp64.
    sy-tabix = temp65.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp64-common ).


    temp67 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp66.
    sy-tabix = temp67.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp66-diff-buffer1 )
      exp = 'b' ).


    temp69 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp68.
    sy-tabix = temp69.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp68-diff-buffer2 )
      exp = 'd' ).



    temp71 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp70.
    sy-tabix = temp71.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp70-common )
      exp = 'c ZZ' ).


    temp73 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp72.
    sy-tabix = temp73.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp72-diff ).



    temp75 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp74.
    sy-tabix = temp75.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp74-common ).


    temp77 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp76.
    sy-tabix = temp77.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp76-diff-buffer1 )
      exp = 'new 00 a a' ).


    temp79 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp78.
    sy-tabix = temp79.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp78-diff-buffer2 )
      exp = '11' ).



    temp81 = sy-tabix.
    READ TABLE lt_result INDEX 5 INTO temp80.
    sy-tabix = temp81.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp80-common )
      exp = 'M' ).


    temp83 = sy-tabix.
    READ TABLE lt_result INDEX 5 INTO temp82.
    sy-tabix = temp83.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp82-diff ).



    temp85 = sy-tabix.
    READ TABLE lt_result INDEX 6 INTO temp84.
    sy-tabix = temp85.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp84-common ).


    temp87 = sy-tabix.
    READ TABLE lt_result INDEX 6 INTO temp86.
    sy-tabix = temp87.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp86-diff-buffer1 )
      exp = '' ).


    temp89 = sy-tabix.
    READ TABLE lt_result INDEX 6 INTO temp88.
    sy-tabix = temp89.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp88-diff-buffer2 )
      exp = 'z z' ).



    temp91 = sy-tabix.
    READ TABLE lt_result INDEX 7 INTO temp90.
    sy-tabix = temp91.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp90-common )
      exp = '99' ).


    temp93 = sy-tabix.
    READ TABLE lt_result INDEX 7 INTO temp92.
    sy-tabix = temp93.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp92-diff ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/diffIndices.test.js
**********************************************************************
CLASS ltcl_diff_indices DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS:
      test FOR TESTING.

ENDCLASS.

CLASS ltcl_diff_indices IMPLEMENTATION.

  METHOD test.

    " returns array indices for differing regions of two arrays
    DATA lt_result TYPE zif_abapgit_diff3=>ty_diff_indices_result_t.
    DATA temp94 LIKE LINE OF lt_result.
    DATA temp95 LIKE sy-tabix.
    DATA temp96 LIKE LINE OF lt_result.
    DATA temp97 LIKE sy-tabix.
    DATA temp98 LIKE LINE OF lt_result.
    DATA temp99 LIKE sy-tabix.
    DATA temp100 LIKE LINE OF lt_result.
    DATA temp101 LIKE sy-tabix.
    DATA temp102 LIKE LINE OF lt_result.
    DATA temp103 LIKE sy-tabix.
    DATA temp104 LIKE LINE OF lt_result.
    DATA temp105 LIKE sy-tabix.
    DATA temp106 LIKE LINE OF lt_result.
    DATA temp107 LIKE sy-tabix.
    DATA temp108 LIKE LINE OF lt_result.
    DATA temp109 LIKE sy-tabix.
    DATA temp110 LIKE LINE OF lt_result.
    DATA temp111 LIKE sy-tabix.
    DATA temp112 LIKE LINE OF lt_result.
    DATA temp113 LIKE sy-tabix.
    DATA temp114 LIKE LINE OF lt_result.
    DATA temp115 LIKE sy-tabix.
    DATA temp116 LIKE LINE OF lt_result.
    DATA temp117 LIKE sy-tabix.
    DATA temp118 LIKE LINE OF lt_result.
    DATA temp119 LIKE sy-tabix.
    DATA temp120 LIKE LINE OF lt_result.
    DATA temp121 LIKE sy-tabix.
    DATA temp122 LIKE LINE OF lt_result.
    DATA temp123 LIKE sy-tabix.
    DATA temp124 LIKE LINE OF lt_result.
    DATA temp125 LIKE sy-tabix.
    DATA temp126 LIKE LINE OF lt_result.
    DATA temp127 LIKE sy-tabix.
    DATA temp128 LIKE LINE OF lt_result.
    DATA temp129 LIKE sy-tabix.
    lt_result = zcl_abapgit_diff3=>create( )->diff_indices(
      it_buffer1 = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' )
      it_buffer2 = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ) ).



    temp95 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp94.
    sy-tabix = temp95.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp94-buffer1-key
      exp = 2 ).


    temp97 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp96.
    sy-tabix = temp97.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp96-buffer1-len
      exp = 1 ).


    temp99 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp98.
    sy-tabix = temp99.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp98-buffer1content )
      exp = 'b' ).


    temp101 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp100.
    sy-tabix = temp101.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp100-buffer2-key
      exp = 2 ).


    temp103 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp102.
    sy-tabix = temp103.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp102-buffer2-len
      exp = 1 ).


    temp105 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp104.
    sy-tabix = temp105.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp104-buffer2content )
      exp = 'd' ).



    temp107 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp106.
    sy-tabix = temp107.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp106-buffer1-key
      exp = 5 ).


    temp109 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp108.
    sy-tabix = temp109.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp108-buffer1-len
      exp = 4 ).


    temp111 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp110.
    sy-tabix = temp111.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp110-buffer1content )
      exp = 'new 00 a a' ).


    temp113 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp112.
    sy-tabix = temp113.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp112-buffer2-key
      exp = 5 ).


    temp115 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp114.
    sy-tabix = temp115.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp114-buffer2-len
      exp = 1 ).


    temp117 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp116.
    sy-tabix = temp117.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp116-buffer2content )
      exp = '11' ).



    temp119 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp118.
    sy-tabix = temp119.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp118-buffer1-key
      exp = 10 ).


    temp121 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp120.
    sy-tabix = temp121.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp120-buffer1-len
      exp = 0 ).


    temp123 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp122.
    sy-tabix = temp123.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp122-buffer1content )
      exp = '' ).


    temp125 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp124.
    sy-tabix = temp125.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp124-buffer2-key
      exp = 7 ).


    temp127 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp126.
    sy-tabix = temp127.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp126-buffer2-len
      exp = 2 ).


    temp129 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp128.
    sy-tabix = temp129.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp128-buffer2content )
      exp = 'z z' ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/diffPatch.test.js
**********************************************************************
CLASS ltcl_diff_patch DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mi_diff3 TYPE REF TO zif_abapgit_diff3,
      mt_a     TYPE string_table,
      mt_b     TYPE string_table.

    METHODS:
      setup,
      test_diff_patch FOR TESTING,
      test_patch FOR TESTING,
      test_strip_patch FOR TESTING,
      test_invert_patch FOR TESTING.

ENDCLASS.

CLASS ltcl_diff_patch IMPLEMENTATION.

  METHOD setup.
    mi_diff3 = zcl_abapgit_diff3=>create( ).

    mt_a = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' ).
    mt_b = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ).
  ENDMETHOD.

  METHOD test_diff_patch.

    " returns a patch-style diff of two arrays
    DATA lt_result TYPE zif_abapgit_diff3=>ty_patch_result_t.
    DATA temp130 LIKE LINE OF lt_result.
    DATA temp131 LIKE sy-tabix.
    DATA temp132 LIKE LINE OF lt_result.
    DATA temp133 LIKE sy-tabix.
    DATA temp134 LIKE LINE OF lt_result.
    DATA temp135 LIKE sy-tabix.
    DATA temp136 LIKE LINE OF lt_result.
    DATA temp137 LIKE sy-tabix.
    DATA temp138 LIKE LINE OF lt_result.
    DATA temp139 LIKE sy-tabix.
    DATA temp140 LIKE LINE OF lt_result.
    DATA temp141 LIKE sy-tabix.
    DATA temp142 LIKE LINE OF lt_result.
    DATA temp143 LIKE sy-tabix.
    DATA temp144 LIKE LINE OF lt_result.
    DATA temp145 LIKE sy-tabix.
    DATA temp146 LIKE LINE OF lt_result.
    DATA temp147 LIKE sy-tabix.
    DATA temp148 LIKE LINE OF lt_result.
    DATA temp149 LIKE sy-tabix.
    DATA temp150 LIKE LINE OF lt_result.
    DATA temp151 LIKE sy-tabix.
    DATA temp152 LIKE LINE OF lt_result.
    DATA temp153 LIKE sy-tabix.
    DATA temp154 LIKE LINE OF lt_result.
    DATA temp155 LIKE sy-tabix.
    DATA temp156 LIKE LINE OF lt_result.
    DATA temp157 LIKE sy-tabix.
    DATA temp158 LIKE LINE OF lt_result.
    DATA temp159 LIKE sy-tabix.
    DATA temp160 LIKE LINE OF lt_result.
    DATA temp161 LIKE sy-tabix.
    DATA temp162 LIKE LINE OF lt_result.
    DATA temp163 LIKE sy-tabix.
    DATA temp164 LIKE LINE OF lt_result.
    DATA temp165 LIKE sy-tabix.
    lt_result = mi_diff3->diff_patch(
      it_buffer1 = mt_a
      it_buffer2 = mt_b ).



    temp131 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp130.
    sy-tabix = temp131.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp130-buffer1-offset
      exp = 2 ).


    temp133 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp132.
    sy-tabix = temp133.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp132-buffer1-length
      exp = 1 ).


    temp135 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp134.
    sy-tabix = temp135.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp134-buffer1-chunk )
      exp = 'b' ).


    temp137 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp136.
    sy-tabix = temp137.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp136-buffer2-offset
      exp = 2 ).


    temp139 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp138.
    sy-tabix = temp139.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp138-buffer2-length
      exp = 1 ).


    temp141 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp140.
    sy-tabix = temp141.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp140-buffer2-chunk )
      exp = 'd' ).



    temp143 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp142.
    sy-tabix = temp143.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp142-buffer1-offset
      exp = 5 ).


    temp145 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp144.
    sy-tabix = temp145.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp144-buffer1-length
      exp = 4 ).


    temp147 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp146.
    sy-tabix = temp147.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp146-buffer1-chunk )
      exp = 'new 00 a a' ).


    temp149 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp148.
    sy-tabix = temp149.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp148-buffer2-offset
      exp = 5 ).


    temp151 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp150.
    sy-tabix = temp151.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp150-buffer2-length
      exp = 1 ).


    temp153 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp152.
    sy-tabix = temp153.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp152-buffer2-chunk )
      exp = '11' ).



    temp155 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp154.
    sy-tabix = temp155.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp154-buffer1-offset
      exp = 10 ).


    temp157 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp156.
    sy-tabix = temp157.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp156-buffer1-length
      exp = 0 ).


    temp159 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp158.
    sy-tabix = temp159.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp158-buffer1-chunk )
      exp = '' ).


    temp161 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp160.
    sy-tabix = temp161.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp160-buffer2-offset
      exp = 7 ).


    temp163 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp162.
    sy-tabix = temp163.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp162-buffer2-length
      exp = 2 ).


    temp165 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp164.
    sy-tabix = temp165.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp164-buffer2-chunk )
      exp = 'z z' ).

  ENDMETHOD.

  METHOD test_patch.

    " applies a patch against buffer1 to get buffer2
    DATA lt_patch TYPE zif_abapgit_diff3=>ty_patch_result_t.
    DATA lt_result TYPE string_table.
    lt_patch = mi_diff3->diff_patch(
      it_buffer1 = mt_a
      it_buffer2 = mt_b ).


    lt_result = mi_diff3->patch(
      it_buffer   = mt_a
      it_patchres = lt_patch ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result
      exp = mt_b ).

  ENDMETHOD.

  METHOD test_strip_patch.

    " removes extra information from the diffPatch result
    DATA lt_patch TYPE zif_abapgit_diff3=>ty_patch_result_t.
    DATA lt_strip TYPE zif_abapgit_diff3=>ty_patch_result_t.
    DATA temp166 LIKE LINE OF lt_strip.
    DATA temp167 LIKE sy-tabix.
    DATA temp168 LIKE LINE OF lt_strip.
    DATA temp169 LIKE sy-tabix.
    DATA temp170 LIKE LINE OF lt_strip.
    DATA temp171 LIKE sy-tabix.
    DATA temp172 LIKE LINE OF lt_strip.
    DATA temp173 LIKE sy-tabix.
    DATA temp174 LIKE LINE OF lt_strip.
    DATA temp175 LIKE sy-tabix.
    DATA temp176 LIKE LINE OF lt_strip.
    DATA temp177 LIKE sy-tabix.
    DATA temp178 LIKE LINE OF lt_strip.
    DATA temp179 LIKE sy-tabix.
    DATA temp180 LIKE LINE OF lt_strip.
    DATA temp181 LIKE sy-tabix.
    DATA temp182 LIKE LINE OF lt_strip.
    DATA temp183 LIKE sy-tabix.
    DATA temp184 LIKE LINE OF lt_strip.
    DATA temp185 LIKE sy-tabix.
    DATA temp186 LIKE LINE OF lt_strip.
    DATA temp187 LIKE sy-tabix.
    DATA temp188 LIKE LINE OF lt_strip.
    DATA temp189 LIKE sy-tabix.
    DATA temp190 LIKE LINE OF lt_strip.
    DATA temp191 LIKE sy-tabix.
    DATA temp192 LIKE LINE OF lt_strip.
    DATA temp193 LIKE sy-tabix.
    DATA temp194 LIKE LINE OF lt_strip.
    DATA temp195 LIKE sy-tabix.
    DATA temp196 LIKE LINE OF lt_strip.
    DATA temp197 LIKE sy-tabix.
    DATA temp198 LIKE LINE OF lt_strip.
    DATA temp199 LIKE sy-tabix.
    DATA temp200 LIKE LINE OF lt_strip.
    DATA temp201 LIKE sy-tabix.
    DATA lt_result TYPE string_table.
    lt_patch = mi_diff3->diff_patch(
      it_buffer1 = mt_a
      it_buffer2 = mt_b ).


    lt_strip = mi_diff3->strip_patch( lt_patch ).



    temp167 = sy-tabix.
    READ TABLE lt_strip INDEX 1 INTO temp166.
    sy-tabix = temp167.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp166-buffer1-offset
      exp = 2 ).


    temp169 = sy-tabix.
    READ TABLE lt_strip INDEX 1 INTO temp168.
    sy-tabix = temp169.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp168-buffer1-length
      exp = 1 ).


    temp171 = sy-tabix.
    READ TABLE lt_strip INDEX 1 INTO temp170.
    sy-tabix = temp171.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp170-buffer1-chunk ).


    temp173 = sy-tabix.
    READ TABLE lt_strip INDEX 1 INTO temp172.
    sy-tabix = temp173.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp172-buffer2-offset ).


    temp175 = sy-tabix.
    READ TABLE lt_strip INDEX 1 INTO temp174.
    sy-tabix = temp175.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp174-buffer2-length ).


    temp177 = sy-tabix.
    READ TABLE lt_strip INDEX 1 INTO temp176.
    sy-tabix = temp177.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp176-buffer2-chunk )
      exp = 'd' ).



    temp179 = sy-tabix.
    READ TABLE lt_strip INDEX 2 INTO temp178.
    sy-tabix = temp179.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp178-buffer1-offset
      exp = 5 ).


    temp181 = sy-tabix.
    READ TABLE lt_strip INDEX 2 INTO temp180.
    sy-tabix = temp181.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp180-buffer1-length
      exp = 4 ).


    temp183 = sy-tabix.
    READ TABLE lt_strip INDEX 2 INTO temp182.
    sy-tabix = temp183.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp182-buffer1-chunk ).


    temp185 = sy-tabix.
    READ TABLE lt_strip INDEX 2 INTO temp184.
    sy-tabix = temp185.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp184-buffer2-offset ).


    temp187 = sy-tabix.
    READ TABLE lt_strip INDEX 2 INTO temp186.
    sy-tabix = temp187.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp186-buffer2-length ).


    temp189 = sy-tabix.
    READ TABLE lt_strip INDEX 2 INTO temp188.
    sy-tabix = temp189.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp188-buffer2-chunk )
      exp = '11' ).



    temp191 = sy-tabix.
    READ TABLE lt_strip INDEX 3 INTO temp190.
    sy-tabix = temp191.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp190-buffer1-offset
      exp = 10 ).


    temp193 = sy-tabix.
    READ TABLE lt_strip INDEX 3 INTO temp192.
    sy-tabix = temp193.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp192-buffer1-length
      exp = 0 ).


    temp195 = sy-tabix.
    READ TABLE lt_strip INDEX 3 INTO temp194.
    sy-tabix = temp195.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp194-buffer1-chunk ).


    temp197 = sy-tabix.
    READ TABLE lt_strip INDEX 3 INTO temp196.
    sy-tabix = temp197.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp196-buffer2-offset ).


    temp199 = sy-tabix.
    READ TABLE lt_strip INDEX 3 INTO temp198.
    sy-tabix = temp199.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp198-buffer2-length ).


    temp201 = sy-tabix.
    READ TABLE lt_strip INDEX 3 INTO temp200.
    sy-tabix = temp201.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp200-buffer2-chunk )
      exp = 'z z' ).

    " applies a stripped patch against buffer1 to get buffer2

    lt_result = mi_diff3->patch(
      it_buffer   = mt_a
      it_patchres = lt_strip ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result
      exp = mt_b ).

  ENDMETHOD.

  METHOD test_invert_patch.

    " inverts the diffPatch result
    DATA lt_patch TYPE zif_abapgit_diff3=>ty_patch_result_t.
    DATA lt_invert TYPE zif_abapgit_diff3=>ty_patch_result_t.
    DATA temp202 LIKE LINE OF lt_invert.
    DATA temp203 LIKE sy-tabix.
    DATA temp204 LIKE LINE OF lt_invert.
    DATA temp205 LIKE sy-tabix.
    DATA temp206 LIKE LINE OF lt_invert.
    DATA temp207 LIKE sy-tabix.
    DATA temp208 LIKE LINE OF lt_invert.
    DATA temp209 LIKE sy-tabix.
    DATA temp210 LIKE LINE OF lt_invert.
    DATA temp211 LIKE sy-tabix.
    DATA temp212 LIKE LINE OF lt_invert.
    DATA temp213 LIKE sy-tabix.
    DATA temp214 LIKE LINE OF lt_invert.
    DATA temp215 LIKE sy-tabix.
    DATA temp216 LIKE LINE OF lt_invert.
    DATA temp217 LIKE sy-tabix.
    DATA temp218 LIKE LINE OF lt_invert.
    DATA temp219 LIKE sy-tabix.
    DATA temp220 LIKE LINE OF lt_invert.
    DATA temp221 LIKE sy-tabix.
    DATA temp222 LIKE LINE OF lt_invert.
    DATA temp223 LIKE sy-tabix.
    DATA temp224 LIKE LINE OF lt_invert.
    DATA temp225 LIKE sy-tabix.
    DATA temp226 LIKE LINE OF lt_invert.
    DATA temp227 LIKE sy-tabix.
    DATA temp228 LIKE LINE OF lt_invert.
    DATA temp229 LIKE sy-tabix.
    DATA temp230 LIKE LINE OF lt_invert.
    DATA temp231 LIKE sy-tabix.
    DATA temp232 LIKE LINE OF lt_invert.
    DATA temp233 LIKE sy-tabix.
    DATA temp234 LIKE LINE OF lt_invert.
    DATA temp235 LIKE sy-tabix.
    DATA temp236 LIKE LINE OF lt_invert.
    DATA temp237 LIKE sy-tabix.
    DATA lt_result TYPE string_table.
    lt_patch = mi_diff3->diff_patch(
      it_buffer1 = mt_a
      it_buffer2 = mt_b ).


    lt_invert = mi_diff3->invert_patch( lt_patch ).



    temp203 = sy-tabix.
    READ TABLE lt_invert INDEX 1 INTO temp202.
    sy-tabix = temp203.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp202-buffer2-offset
      exp = 2 ).


    temp205 = sy-tabix.
    READ TABLE lt_invert INDEX 1 INTO temp204.
    sy-tabix = temp205.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp204-buffer2-length
      exp = 1 ).


    temp207 = sy-tabix.
    READ TABLE lt_invert INDEX 1 INTO temp206.
    sy-tabix = temp207.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp206-buffer2-chunk )
      exp = 'b' ).


    temp209 = sy-tabix.
    READ TABLE lt_invert INDEX 1 INTO temp208.
    sy-tabix = temp209.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp208-buffer1-offset
      exp = 2 ).


    temp211 = sy-tabix.
    READ TABLE lt_invert INDEX 1 INTO temp210.
    sy-tabix = temp211.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp210-buffer1-length
      exp = 1 ).


    temp213 = sy-tabix.
    READ TABLE lt_invert INDEX 1 INTO temp212.
    sy-tabix = temp213.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp212-buffer1-chunk )
      exp = 'd' ).



    temp215 = sy-tabix.
    READ TABLE lt_invert INDEX 2 INTO temp214.
    sy-tabix = temp215.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp214-buffer2-offset
      exp = 5 ).


    temp217 = sy-tabix.
    READ TABLE lt_invert INDEX 2 INTO temp216.
    sy-tabix = temp217.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp216-buffer2-length
      exp = 4 ).


    temp219 = sy-tabix.
    READ TABLE lt_invert INDEX 2 INTO temp218.
    sy-tabix = temp219.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp218-buffer2-chunk )
      exp = 'new 00 a a' ).


    temp221 = sy-tabix.
    READ TABLE lt_invert INDEX 2 INTO temp220.
    sy-tabix = temp221.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp220-buffer1-offset
      exp = 5 ).


    temp223 = sy-tabix.
    READ TABLE lt_invert INDEX 2 INTO temp222.
    sy-tabix = temp223.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp222-buffer1-length
      exp = 1 ).


    temp225 = sy-tabix.
    READ TABLE lt_invert INDEX 2 INTO temp224.
    sy-tabix = temp225.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp224-buffer1-chunk )
      exp = '11' ).



    temp227 = sy-tabix.
    READ TABLE lt_invert INDEX 3 INTO temp226.
    sy-tabix = temp227.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp226-buffer2-offset
      exp = 10 ).


    temp229 = sy-tabix.
    READ TABLE lt_invert INDEX 3 INTO temp228.
    sy-tabix = temp229.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp228-buffer2-length
      exp = 0 ).


    temp231 = sy-tabix.
    READ TABLE lt_invert INDEX 3 INTO temp230.
    sy-tabix = temp231.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp230-buffer2-chunk )
      exp = '' ).


    temp233 = sy-tabix.
    READ TABLE lt_invert INDEX 3 INTO temp232.
    sy-tabix = temp233.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp232-buffer1-offset
      exp = 7 ).


    temp235 = sy-tabix.
    READ TABLE lt_invert INDEX 3 INTO temp234.
    sy-tabix = temp235.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp234-buffer1-length
      exp = 2 ).


    temp237 = sy-tabix.
    READ TABLE lt_invert INDEX 3 INTO temp236.
    sy-tabix = temp237.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp236-buffer1-chunk )
      exp = 'z z' ).

    " applies a stripped patch against buffer1 to get buffer2

    lt_result = mi_diff3->patch(
      it_buffer   = mt_b
      it_patchres = lt_invert ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_result
      exp = mt_a ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/diff3MergeRegions.test.js
**********************************************************************
CLASS ltcl_diff3_merge_regions DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mi_diff3 TYPE REF TO zif_abapgit_diff3,
      mt_o     TYPE string_table,
      mt_a     TYPE string_table,
      mt_b     TYPE string_table.

    METHODS:
      setup,
      test FOR TESTING.

ENDCLASS.

CLASS ltcl_diff3_merge_regions IMPLEMENTATION.

  METHOD setup.
    mi_diff3 = zcl_abapgit_diff3=>create( ).
  ENDMETHOD.

  METHOD test.
    DATA lt_result TYPE zif_abapgit_diff3=>ty_region_t.
    DATA temp238 LIKE LINE OF lt_result.
    DATA temp239 LIKE sy-tabix.
    DATA temp240 LIKE LINE OF lt_result.
    DATA temp241 LIKE sy-tabix.
    DATA temp242 LIKE LINE OF lt_result.
    DATA temp243 LIKE sy-tabix.
    DATA temp244 LIKE LINE OF lt_result.
    DATA temp245 LIKE sy-tabix.
    DATA temp246 LIKE LINE OF lt_result.
    DATA temp247 LIKE sy-tabix.
    DATA temp248 LIKE LINE OF lt_result.
    DATA temp249 LIKE sy-tabix.
    DATA temp250 LIKE LINE OF lt_result.
    DATA temp251 LIKE sy-tabix.
    DATA temp252 LIKE LINE OF lt_result.
    DATA temp253 LIKE sy-tabix.
    DATA temp254 LIKE LINE OF lt_result.
    DATA temp255 LIKE sy-tabix.
    DATA temp256 LIKE LINE OF lt_result.
    DATA temp257 LIKE sy-tabix.
    DATA temp258 LIKE LINE OF lt_result.
    DATA temp259 LIKE sy-tabix.
    DATA temp260 LIKE LINE OF lt_result.
    DATA temp261 LIKE sy-tabix.
    DATA temp262 LIKE LINE OF lt_result.
    DATA temp263 LIKE sy-tabix.
    DATA temp264 LIKE LINE OF lt_result.
    DATA temp265 LIKE sy-tabix.
    DATA temp266 LIKE LINE OF lt_result.
    DATA temp267 LIKE sy-tabix.
    DATA temp268 LIKE LINE OF lt_result.
    DATA temp269 LIKE sy-tabix.
    DATA temp270 LIKE LINE OF lt_result.
    DATA temp271 LIKE sy-tabix.
    DATA temp272 LIKE LINE OF lt_result.
    DATA temp273 LIKE sy-tabix.
    DATA temp274 LIKE LINE OF lt_result.
    DATA temp275 LIKE sy-tabix.
    DATA temp276 LIKE LINE OF lt_result.
    DATA temp277 LIKE sy-tabix.
    DATA temp278 LIKE LINE OF lt_result.
    DATA temp279 LIKE sy-tabix.
    DATA temp280 LIKE LINE OF lt_result.
    DATA temp281 LIKE sy-tabix.
    DATA temp282 LIKE LINE OF lt_result.
    DATA temp283 LIKE sy-tabix.
    DATA temp284 LIKE LINE OF lt_result.
    DATA temp285 LIKE sy-tabix.
    DATA temp286 LIKE LINE OF lt_result.
    DATA temp287 LIKE sy-tabix.
    DATA temp288 LIKE LINE OF lt_result.
    DATA temp289 LIKE sy-tabix.
    DATA temp290 LIKE LINE OF lt_result.
    DATA temp291 LIKE sy-tabix.
    DATA temp292 LIKE LINE OF lt_result.
    DATA temp293 LIKE sy-tabix.
    DATA temp294 LIKE LINE OF lt_result.
    DATA temp295 LIKE sy-tabix.
    DATA temp296 LIKE LINE OF lt_result.
    DATA temp297 LIKE sy-tabix.
    DATA temp298 LIKE LINE OF lt_result.
    DATA temp299 LIKE sy-tabix.
    DATA temp300 LIKE LINE OF lt_result.
    DATA temp301 LIKE sy-tabix.
    DATA temp302 LIKE LINE OF lt_result.
    DATA temp303 LIKE sy-tabix.
    DATA temp304 LIKE LINE OF lt_result.
    DATA temp305 LIKE sy-tabix.
    DATA temp306 LIKE LINE OF lt_result.
    DATA temp307 LIKE sy-tabix.
    DATA temp308 LIKE LINE OF lt_result.
    DATA temp309 LIKE sy-tabix.
    DATA temp310 LIKE LINE OF lt_result.
    DATA temp311 LIKE sy-tabix.
    DATA temp312 LIKE LINE OF lt_result.
    DATA temp313 LIKE sy-tabix.
    DATA temp314 LIKE LINE OF lt_result.
    DATA temp315 LIKE sy-tabix.
    DATA temp316 LIKE LINE OF lt_result.
    DATA temp317 LIKE sy-tabix.
    DATA temp318 LIKE LINE OF lt_result.
    DATA temp319 LIKE sy-tabix.
    DATA temp320 LIKE LINE OF lt_result.
    DATA temp321 LIKE sy-tabix.
    DATA temp322 LIKE LINE OF lt_result.
    DATA temp323 LIKE sy-tabix.
    DATA temp324 LIKE LINE OF lt_result.
    DATA temp325 LIKE sy-tabix.
    DATA temp326 LIKE LINE OF lt_result.
    DATA temp327 LIKE sy-tabix.

    " returns results of 3-way diff from o,a,b arrays
    mt_o = lcl_helper=>split( 'AA ZZ 00 M 99' ).
    mt_a = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' ).
    mt_b = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ).


    lt_result = mi_diff3->diff3_merge_regions(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).



    temp239 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp238.
    sy-tabix = temp239.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp238-stable
      exp = abap_true ).


    temp241 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp240.
    sy-tabix = temp241.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp240-stable_region-buffer
      exp = 'o' ).


    temp243 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp242.
    sy-tabix = temp243.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp242-stable_region-buffer_start
      exp = 0 ).


    temp245 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp244.
    sy-tabix = temp245.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp244-stable_region-buffer_length
      exp = 1 ).


    temp247 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp246.
    sy-tabix = temp247.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp246-stable_region-buffer_content )
      exp = 'AA' ).



    temp249 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp248.
    sy-tabix = temp249.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp248-stable
      exp = abap_false ).


    temp251 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp250.
    sy-tabix = temp251.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp250-unstable_region-a_start
      exp = 1 ).


    temp253 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp252.
    sy-tabix = temp253.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp252-unstable_region-a_length
      exp = 3 ).


    temp255 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp254.
    sy-tabix = temp255.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp254-unstable_region-a_content )
      exp = 'a b c' ).


    temp257 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp256.
    sy-tabix = temp257.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp256-unstable_region-o_start
      exp = 1 ).


    temp259 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp258.
    sy-tabix = temp259.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp258-unstable_region-o_length
      exp = 0 ).


    temp261 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp260.
    sy-tabix = temp261.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp260-unstable_region-o_content )
      exp = '' ).


    temp263 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp262.
    sy-tabix = temp263.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp262-unstable_region-b_start
      exp = 1 ).


    temp265 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp264.
    sy-tabix = temp265.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp264-unstable_region-b_length
      exp = 3 ).


    temp267 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp266.
    sy-tabix = temp267.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp266-unstable_region-b_content )
      exp = 'a d c' ).



    temp269 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp268.
    sy-tabix = temp269.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp268-stable
      exp = abap_true ).


    temp271 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp270.
    sy-tabix = temp271.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp270-stable_region-buffer
      exp = 'o' ).


    temp273 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp272.
    sy-tabix = temp273.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp272-stable_region-buffer_start
      exp = 1 ).


    temp275 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp274.
    sy-tabix = temp275.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp274-stable_region-buffer_length
      exp = 1 ).


    temp277 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp276.
    sy-tabix = temp277.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp276-stable_region-buffer_content )
      exp = 'ZZ' ).



    temp279 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp278.
    sy-tabix = temp279.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp278-stable
      exp = abap_false ).


    temp281 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp280.
    sy-tabix = temp281.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp280-unstable_region-a_start
      exp = 5 ).


    temp283 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp282.
    sy-tabix = temp283.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp282-unstable_region-a_length
      exp = 4 ).


    temp285 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp284.
    sy-tabix = temp285.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp284-unstable_region-a_content )
      exp = 'new 00 a a' ).


    temp287 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp286.
    sy-tabix = temp287.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp286-unstable_region-o_start
      exp = 2 ).


    temp289 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp288.
    sy-tabix = temp289.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp288-unstable_region-o_length
      exp = 1 ).


    temp291 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp290.
    sy-tabix = temp291.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp290-unstable_region-o_content )
      exp = '00' ).


    temp293 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp292.
    sy-tabix = temp293.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp292-unstable_region-b_start
      exp = 5 ).


    temp295 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp294.
    sy-tabix = temp295.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp294-unstable_region-b_length
      exp = 1 ).


    temp297 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp296.
    sy-tabix = temp297.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp296-unstable_region-b_content )
      exp = '11' ).



    temp299 = sy-tabix.
    READ TABLE lt_result INDEX 5 INTO temp298.
    sy-tabix = temp299.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp298-stable
      exp = abap_true ).


    temp301 = sy-tabix.
    READ TABLE lt_result INDEX 5 INTO temp300.
    sy-tabix = temp301.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp300-stable_region-buffer
      exp = 'o' ).


    temp303 = sy-tabix.
    READ TABLE lt_result INDEX 5 INTO temp302.
    sy-tabix = temp303.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp302-stable_region-buffer_start
      exp = 3 ).


    temp305 = sy-tabix.
    READ TABLE lt_result INDEX 5 INTO temp304.
    sy-tabix = temp305.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp304-stable_region-buffer_length
      exp = 1 ).


    temp307 = sy-tabix.
    READ TABLE lt_result INDEX 5 INTO temp306.
    sy-tabix = temp307.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp306-stable_region-buffer_content )
      exp = 'M' ).



    temp309 = sy-tabix.
    READ TABLE lt_result INDEX 6 INTO temp308.
    sy-tabix = temp309.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp308-stable
      exp = abap_true ).


    temp311 = sy-tabix.
    READ TABLE lt_result INDEX 6 INTO temp310.
    sy-tabix = temp311.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp310-stable_region-buffer
      exp = 'b' ).


    temp313 = sy-tabix.
    READ TABLE lt_result INDEX 6 INTO temp312.
    sy-tabix = temp313.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp312-stable_region-buffer_start
      exp = 7 ).


    temp315 = sy-tabix.
    READ TABLE lt_result INDEX 6 INTO temp314.
    sy-tabix = temp315.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp314-stable_region-buffer_length
      exp = 2 ).


    temp317 = sy-tabix.
    READ TABLE lt_result INDEX 6 INTO temp316.
    sy-tabix = temp317.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp316-stable_region-buffer_content )
      exp = 'z z' ).



    temp319 = sy-tabix.
    READ TABLE lt_result INDEX 7 INTO temp318.
    sy-tabix = temp319.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp318-stable
      exp = abap_true ).


    temp321 = sy-tabix.
    READ TABLE lt_result INDEX 7 INTO temp320.
    sy-tabix = temp321.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp320-stable_region-buffer
      exp = 'o' ).


    temp323 = sy-tabix.
    READ TABLE lt_result INDEX 7 INTO temp322.
    sy-tabix = temp323.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp322-stable_region-buffer_start
      exp = 4 ).


    temp325 = sy-tabix.
    READ TABLE lt_result INDEX 7 INTO temp324.
    sy-tabix = temp325.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp324-stable_region-buffer_length
      exp = 1 ).


    temp327 = sy-tabix.
    READ TABLE lt_result INDEX 7 INTO temp326.
    sy-tabix = temp327.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp326-stable_region-buffer_content )
      exp = '99' ).

  ENDMETHOD.
ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/diff3Merge.test.js
**********************************************************************
CLASS ltcl_diff3_merge DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mi_diff3 TYPE REF TO zif_abapgit_diff3,
      mt_o     TYPE string_table,
      mt_a     TYPE string_table,
      mt_b     TYPE string_table.

    METHODS:
      setup,
      test FOR TESTING,
      exclude_false_conflicts FOR TESTING,
      include_false_conflicts FOR TESTING.

ENDCLASS.

CLASS ltcl_diff3_merge IMPLEMENTATION.

  METHOD setup.
    mi_diff3 = zcl_abapgit_diff3=>create( ).
  ENDMETHOD.

  METHOD test.
    DATA lt_result TYPE zif_abapgit_diff3=>ty_merge_region_t.
    DATA temp328 LIKE LINE OF lt_result.
    DATA temp329 LIKE sy-tabix.
    DATA temp330 LIKE LINE OF lt_result.
    DATA temp331 LIKE sy-tabix.
    DATA temp332 LIKE LINE OF lt_result.
    DATA temp333 LIKE sy-tabix.
    DATA temp334 LIKE LINE OF lt_result.
    DATA temp335 LIKE sy-tabix.
    DATA temp336 LIKE LINE OF lt_result.
    DATA temp337 LIKE sy-tabix.
    DATA temp338 LIKE LINE OF lt_result.
    DATA temp339 LIKE sy-tabix.
    DATA temp340 LIKE LINE OF lt_result.
    DATA temp341 LIKE sy-tabix.
    DATA temp342 LIKE LINE OF lt_result.
    DATA temp343 LIKE sy-tabix.
    DATA temp344 LIKE LINE OF lt_result.
    DATA temp345 LIKE sy-tabix.
    DATA temp346 LIKE LINE OF lt_result.
    DATA temp347 LIKE sy-tabix.
    DATA temp348 LIKE LINE OF lt_result.
    DATA temp349 LIKE sy-tabix.
    DATA temp350 LIKE LINE OF lt_result.
    DATA temp351 LIKE sy-tabix.
    DATA temp352 LIKE LINE OF lt_result.
    DATA temp353 LIKE sy-tabix.
    DATA temp354 LIKE LINE OF lt_result.
    DATA temp355 LIKE sy-tabix.

    " performs diff3 merge on arrays
    mt_o = lcl_helper=>split( 'AA ZZ 00 M 99' ).
    mt_a = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' ).
    mt_b = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ).


    lt_result = mi_diff3->diff3_merge(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).

* AA
* <<<<<<< a
* a
* b
* c
* ||||||| o
* =======
* a
* d
* c
* >>>>>>> b
* ZZ
* <<<<<<< a
* new
* 00
* a
* a
* ||||||| o
* 00
* =======
* 11
* >>>>>>> b
* M
* z
* z
* 99



    temp329 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp328.
    sy-tabix = temp329.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp328-ok )
      exp = 'AA' ).


    temp331 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp330.
    sy-tabix = temp331.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp330-conflict ).



    temp333 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp332.
    sy-tabix = temp333.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp332-ok ).


    temp335 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp334.
    sy-tabix = temp335.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp334-conflict-o ).


    temp337 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp336.
    sy-tabix = temp337.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp336-conflict-a )
      exp = 'a b c' ).


    temp339 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp338.
    sy-tabix = temp339.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp338-conflict-b )
      exp = 'a d c' ).



    temp341 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp340.
    sy-tabix = temp341.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp340-ok )
      exp = 'ZZ' ).


    temp343 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp342.
    sy-tabix = temp343.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp342-conflict ).



    temp345 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp344.
    sy-tabix = temp345.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp344-ok ).


    temp347 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp346.
    sy-tabix = temp347.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp346-conflict-o )
      exp = '00' ).


    temp349 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp348.
    sy-tabix = temp349.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp348-conflict-a )
      exp = 'new 00 a a' ).


    temp351 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp350.
    sy-tabix = temp351.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp350-conflict-b )
      exp = '11' ).



    temp353 = sy-tabix.
    READ TABLE lt_result INDEX 5 INTO temp352.
    sy-tabix = temp353.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp352-ok )
      exp = 'M z z 99' ).


    temp355 = sy-tabix.
    READ TABLE lt_result INDEX 5 INTO temp354.
    sy-tabix = temp355.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp354-conflict ).

  ENDMETHOD.

  METHOD exclude_false_conflicts.
    DATA lt_result TYPE zif_abapgit_diff3=>ty_merge_region_t.
    DATA temp356 LIKE LINE OF lt_result.
    DATA temp357 LIKE sy-tabix.
    DATA temp358 LIKE LINE OF lt_result.
    DATA temp359 LIKE sy-tabix.

    " excludes false conflicts by default
    mt_o = lcl_helper=>split( 'AA ZZ' ).
    mt_a = lcl_helper=>split( 'AA a b c ZZ' ).
    mt_b = lcl_helper=>split( 'AA a b c ZZ' ).


    lt_result = mi_diff3->diff3_merge(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).



    temp357 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp356.
    sy-tabix = temp357.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp356-ok )
      exp = 'AA a b c ZZ' ).


    temp359 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp358.
    sy-tabix = temp359.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp358-conflict ).

  ENDMETHOD.

  METHOD include_false_conflicts.
    DATA lt_result TYPE zif_abapgit_diff3=>ty_merge_region_t.
    DATA temp360 LIKE LINE OF lt_result.
    DATA temp361 LIKE sy-tabix.
    DATA temp362 LIKE LINE OF lt_result.
    DATA temp363 LIKE sy-tabix.
    DATA temp364 LIKE LINE OF lt_result.
    DATA temp365 LIKE sy-tabix.
    DATA temp366 LIKE LINE OF lt_result.
    DATA temp367 LIKE sy-tabix.
    DATA temp368 LIKE LINE OF lt_result.
    DATA temp369 LIKE sy-tabix.
    DATA temp370 LIKE LINE OF lt_result.
    DATA temp371 LIKE sy-tabix.
    DATA temp372 LIKE LINE OF lt_result.
    DATA temp373 LIKE sy-tabix.
    DATA temp374 LIKE LINE OF lt_result.
    DATA temp375 LIKE sy-tabix.

    " can include false conflicts with option
    mt_o = lcl_helper=>split( 'AA ZZ' ).
    mt_a = lcl_helper=>split( 'AA a b c ZZ' ).
    mt_b = lcl_helper=>split( 'AA a b c ZZ' ).


    lt_result = mi_diff3->diff3_merge(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b
      iv_exclude_false_conflicts = abap_false ).



    temp361 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp360.
    sy-tabix = temp361.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp360-ok )
      exp = 'AA' ).


    temp363 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp362.
    sy-tabix = temp363.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp362-conflict ).



    temp365 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp364.
    sy-tabix = temp365.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp364-ok ).


    temp367 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp366.
    sy-tabix = temp367.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp366-conflict-o ).


    temp369 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp368.
    sy-tabix = temp369.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp368-conflict-a )
      exp = 'a b c' ).


    temp371 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp370.
    sy-tabix = temp371.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp370-conflict-b )
      exp = 'a b c' ).



    temp373 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp372.
    sy-tabix = temp373.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( temp372-ok )
      exp = 'ZZ' ).


    temp375 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp374.
    sy-tabix = temp375.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp374-conflict ).

  ENDMETHOD.
ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/merge.test.js
**********************************************************************
CLASS ltcl_merge DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mi_diff3 TYPE REF TO zif_abapgit_diff3,
      mt_o     TYPE string_table,
      mt_a     TYPE string_table,
      mt_b     TYPE string_table.

    METHODS:
      setup,
      test_conflict FOR TESTING,
      test_result FOR TESTING.

ENDCLASS.

CLASS ltcl_merge IMPLEMENTATION.

  METHOD setup.
    mi_diff3 = zcl_abapgit_diff3=>create( ).
  ENDMETHOD.

  METHOD test_conflict.
    DATA ls_result TYPE zif_abapgit_diff3=>ty_merge_result.

    " performs diff3 merge on arrays
    mt_o = lcl_helper=>split( 'AA' ).
    mt_a = lcl_helper=>split( 'AA' ).
    mt_b = lcl_helper=>split( 'AA' ).


    ls_result = mi_diff3->merge(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-conflict
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( ls_result-result )
      exp = 'AA' ).

  ENDMETHOD.

  METHOD test_result.
    DATA ls_result TYPE zif_abapgit_diff3=>ty_merge_result.
    DATA lt_exp TYPE string_table.

    " returns a diff3-style merge result
    mt_o = lcl_helper=>split( 'AA ZZ 00 M 99' ).
    mt_a = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' ).
    mt_b = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ).


    ls_result = mi_diff3->merge(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).


    lt_exp = lcl_helper=>split(
      'AA\n<<<<<<<\na\nb\nc\n=======\na\nd\nc\n' &&
      '>>>>>>>\nZZ\n<<<<<<<\nnew\n00\na\na\n' &&
      '=======\n11\n>>>>>>>\nM\nz\nz\n99' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-conflict
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-result
      exp = lt_exp ).

  ENDMETHOD.
ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/mergeDiff3.test.js
**********************************************************************
CLASS ltcl_merge_diff3 DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mi_diff3 TYPE REF TO zif_abapgit_diff3,
      mt_o     TYPE string_table,
      mt_a     TYPE string_table,
      mt_b     TYPE string_table.

    METHODS:
      setup,
      test_conflict FOR TESTING,
      test_result FOR TESTING.

ENDCLASS.

CLASS ltcl_merge_diff3 IMPLEMENTATION.

  METHOD setup.
    mi_diff3 = zcl_abapgit_diff3=>create( ).
  ENDMETHOD.

  METHOD test_conflict.
    DATA ls_result TYPE zif_abapgit_diff3=>ty_merge_result.

    " performs diff3 merge on arrays
    mt_o = lcl_helper=>split( 'AA' ).
    mt_a = lcl_helper=>split( 'AA' ).
    mt_b = lcl_helper=>split( 'AA' ).


    ls_result = mi_diff3->merge_diff3(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-conflict
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( ls_result-result )
      exp = 'AA' ).

  ENDMETHOD.

  METHOD test_result.
    DATA temp376 TYPE zif_abapgit_diff3=>ty_labels.
    DATA ls_labels LIKE temp376.
    DATA ls_result TYPE zif_abapgit_diff3=>ty_merge_result.
    DATA lt_exp TYPE string_table.

    " returns a diff3-style merge result
    mt_o = lcl_helper=>split( 'AA ZZ 00 M 99' ).
    mt_a = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' ).
    mt_b = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ).


    CLEAR temp376.
    temp376-a = 'a'.
    temp376-b = 'b'.
    temp376-o = 'o'.

    ls_labels = temp376.


    ls_result = mi_diff3->merge_diff3(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b
      is_labels = ls_labels ).


    lt_exp = lcl_helper=>split(
      'AA\n<<<<<<< a\na\nb\nc\n||||||| o\n=======\na\nd\nc\n' &&
      '>>>>>>> b\nZZ\n<<<<<<< a\nnew\n00\na\na\n' &&
      '||||||| o\n00\n=======\n11\n>>>>>>> b\nM\nz\nz\n99' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-conflict
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-result
      exp = lt_exp ).

  ENDMETHOD.
ENDCLASS.

**********************************************************************
* https://github.com/bhousel/node-diff3/blob/main/test/mergeDigIn.test.js
**********************************************************************
CLASS ltcl_merge_dig_in DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mi_diff3 TYPE REF TO zif_abapgit_diff3,
      mt_o     TYPE string_table,
      mt_a     TYPE string_table,
      mt_b     TYPE string_table.

    METHODS:
      setup,
      test_conflict FOR TESTING,
      test_result FOR TESTING.

ENDCLASS.

CLASS ltcl_merge_dig_in IMPLEMENTATION.

  METHOD setup.
    mi_diff3 = zcl_abapgit_diff3=>create( ).
  ENDMETHOD.

  METHOD test_conflict.
    DATA ls_result TYPE zif_abapgit_diff3=>ty_merge_result.

    " performs diff3 merge on arrays
    mt_o = lcl_helper=>split( 'AA' ).
    mt_a = lcl_helper=>split( 'AA' ).
    mt_b = lcl_helper=>split( 'AA' ).


    ls_result = mi_diff3->merge_dig_in(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-conflict
      exp = abap_false ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_helper=>concat( ls_result-result )
      exp = 'AA' ).

  ENDMETHOD.

  METHOD test_result.
    DATA ls_result TYPE zif_abapgit_diff3=>ty_merge_result.
    DATA lt_exp TYPE string_table.

    " returns a diff3-style merge result
    mt_o = lcl_helper=>split( 'AA ZZ 00 M 99' ).
    mt_a = lcl_helper=>split( 'AA a b c ZZ new 00 a a M 99' ).
    mt_b = lcl_helper=>split( 'AA a d c ZZ 11 M z z 99' ).


    ls_result = mi_diff3->merge_dig_in(
      it_a = mt_a
      it_o = mt_o
      it_b = mt_b ).


    lt_exp = lcl_helper=>split(
      'AA\na\n<<<<<<<\nb\n=======\nd\n' &&
      '>>>>>>>\nc\nZZ\n<<<<<<<\nnew\n00\na\na\n' &&
      '=======\n11\n>>>>>>>\nM\nz\nz\n99' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-conflict
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-result
      exp = lt_exp ).

  ENDMETHOD.
ENDCLASS.

**********************************************************************
* Diff ABAP Code
**********************************************************************
CLASS ltcl_abap_code DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    DATA:
      mt_old   TYPE string_table,
      mt_new   TYPE string_table,
      mi_diff3 TYPE REF TO zif_abapgit_diff3.

    METHODS:
      setup,
      diff_comm FOR TESTING,
      diff_indices FOR TESTING.

ENDCLASS.

CLASS ltcl_abap_code IMPLEMENTATION.

  METHOD setup.
    DATA lv_old TYPE string.
    DATA lv_new TYPE string.

    mi_diff3 = zcl_abapgit_diff3=>create( ).


    lv_old = `REPORT z_differ_test_prog.\n`
      && `\n`
      && `* next line was added\n`
      && `\n`
      && `* next line was changed\n`
      && `MESSAGE 'changed line' TYPE 'I'.\n`
      && `\n`
      && `* next line was removed\n`
      && `MESSAGE 'removed line' TYPE 'I'.\n`
      && `\n`
      && `* Some comment\n`
      && `" Another comment\n`
      && `DATA variable TYPE string.\n`
      && `\n`
      && `variable = 'some text'. " in-line comment\n`
      && `variable = |some text|.\n`
      && `variable = |some { variable } text|.\n`
      && `\n`
      && `* eof *\n`.


    lv_new = `REPORT z_differ_test_prog.\n`
      && `\n`
      && `* next line was added\n`
      && `MESSAGE 'added line' TYPE 'I'.\n`
      && `\n`
      && `* next line was changed\n`
      && `MESSAGE 'changed line' TYPE 'W'.\n`
      && `\n`
      && `* next line was removed\n`
      && `\n`
      && `* Some comment\n`
      && `" Another comment\n`
      && `DATA variable TYPE string.\n`
      && `\n`
      && `variable = 'some text'. " in-line comment\n`
      && `variable = |some text|.\n`
      && `variable = |some { variable } text|.\n`
      && `\n`
      && `* eof **\n`.

    mt_old = lcl_helper=>split( lv_old ).
    mt_new = lcl_helper=>split( lv_new ).

  ENDMETHOD.

  METHOD diff_comm.

    DATA lt_result TYPE zif_abapgit_diff3=>ty_comm_result_t.
    DATA temp377 LIKE LINE OF lt_result.
    DATA temp378 LIKE sy-tabix.
    DATA temp379 LIKE LINE OF lt_result.
    DATA temp380 LIKE sy-tabix.
    DATA temp381 LIKE LINE OF lt_result.
    DATA temp382 LIKE sy-tabix.
    DATA temp383 LIKE LINE OF lt_result.
    DATA temp384 LIKE sy-tabix.
    DATA temp385 LIKE LINE OF lt_result.
    DATA temp386 LIKE sy-tabix.
    DATA temp387 LIKE LINE OF lt_result.
    DATA temp388 LIKE sy-tabix.
    DATA temp389 LIKE LINE OF lt_result.
    DATA temp390 LIKE sy-tabix.
    DATA temp391 LIKE LINE OF lt_result.
    DATA temp392 LIKE sy-tabix.
    DATA temp393 LIKE LINE OF lt_result.
    DATA temp394 LIKE sy-tabix.
    DATA temp395 LIKE LINE OF lt_result.
    DATA temp396 LIKE sy-tabix.
    DATA temp397 LIKE LINE OF lt_result.
    DATA temp398 LIKE sy-tabix.
    DATA temp399 LIKE LINE OF lt_result.
    DATA temp400 LIKE sy-tabix.
    DATA temp401 LIKE LINE OF lt_result.
    DATA temp402 LIKE sy-tabix.
    DATA temp403 LIKE LINE OF lt_result.
    DATA temp404 LIKE sy-tabix.
    DATA temp405 LIKE LINE OF lt_result.
    DATA temp406 LIKE sy-tabix.
    DATA temp407 LIKE LINE OF lt_result.
    DATA temp408 LIKE sy-tabix.
    DATA temp409 LIKE LINE OF lt_result.
    DATA temp410 LIKE sy-tabix.
    DATA temp411 LIKE LINE OF lt_result.
    DATA temp412 LIKE sy-tabix.
    DATA temp413 LIKE LINE OF lt_result.
    DATA temp414 LIKE sy-tabix.
    DATA temp415 LIKE LINE OF lt_result.
    DATA temp416 LIKE sy-tabix.
    lt_result = mi_diff3->diff_comm(
      it_buffer1 = mt_old
      it_buffer2 = mt_new ).



    temp378 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp377.
    sy-tabix = temp378.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lines( temp377-common )
      exp = 3 ).


    temp380 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp379.
    sy-tabix = temp380.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp379-diff ).



    temp382 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp381.
    sy-tabix = temp382.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp381-common ).


    temp384 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp383.
    sy-tabix = temp384.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lines( temp383-diff-buffer1 )
      exp = 0 ).


    temp386 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp385.
    sy-tabix = temp386.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lines( temp385-diff-buffer2 )
      exp = 1 ).



    temp388 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp387.
    sy-tabix = temp388.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lines( temp387-common )
      exp = 2 ).


    temp390 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp389.
    sy-tabix = temp390.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp389-diff ).



    temp392 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp391.
    sy-tabix = temp392.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp391-common ).


    temp394 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp393.
    sy-tabix = temp394.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lines( temp393-diff-buffer1 )
      exp = 1 ).


    temp396 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp395.
    sy-tabix = temp396.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lines( temp395-diff-buffer2 )
      exp = 1 ).



    temp398 = sy-tabix.
    READ TABLE lt_result INDEX 5 INTO temp397.
    sy-tabix = temp398.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lines( temp397-common )
      exp = 2 ).


    temp400 = sy-tabix.
    READ TABLE lt_result INDEX 5 INTO temp399.
    sy-tabix = temp400.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp399-diff ).



    temp402 = sy-tabix.
    READ TABLE lt_result INDEX 6 INTO temp401.
    sy-tabix = temp402.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp401-common ).


    temp404 = sy-tabix.
    READ TABLE lt_result INDEX 6 INTO temp403.
    sy-tabix = temp404.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lines( temp403-diff-buffer1 )
      exp = 1 ).


    temp406 = sy-tabix.
    READ TABLE lt_result INDEX 6 INTO temp405.
    sy-tabix = temp406.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lines( temp405-diff-buffer2 )
      exp = 0 ).



    temp408 = sy-tabix.
    READ TABLE lt_result INDEX 7 INTO temp407.
    sy-tabix = temp408.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lines( temp407-common )
      exp = 9 ).


    temp410 = sy-tabix.
    READ TABLE lt_result INDEX 7 INTO temp409.
    sy-tabix = temp410.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp409-diff ).



    temp412 = sy-tabix.
    READ TABLE lt_result INDEX 8 INTO temp411.
    sy-tabix = temp412.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_initial( temp411-common ).


    temp414 = sy-tabix.
    READ TABLE lt_result INDEX 8 INTO temp413.
    sy-tabix = temp414.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lines( temp413-diff-buffer1 )
      exp = 1 ).


    temp416 = sy-tabix.
    READ TABLE lt_result INDEX 8 INTO temp415.
    sy-tabix = temp416.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = lines( temp415-diff-buffer2 )
      exp = 1 ).

  ENDMETHOD.

  METHOD diff_indices.

    DATA lt_result TYPE zif_abapgit_diff3=>ty_diff_indices_result_t.
    DATA temp417 LIKE LINE OF lt_result.
    DATA temp418 LIKE sy-tabix.
    DATA temp419 LIKE LINE OF lt_result.
    DATA temp420 LIKE sy-tabix.
    DATA temp421 LIKE LINE OF lt_result.
    DATA temp422 LIKE sy-tabix.
    DATA temp423 LIKE LINE OF lt_result.
    DATA temp424 LIKE sy-tabix.
    DATA temp425 LIKE LINE OF lt_result.
    DATA temp426 LIKE sy-tabix.
    DATA temp427 LIKE LINE OF lt_result.
    DATA temp428 LIKE sy-tabix.
    DATA temp429 LIKE LINE OF lt_result.
    DATA temp430 LIKE sy-tabix.
    DATA temp431 LIKE LINE OF lt_result.
    DATA temp432 LIKE sy-tabix.
    DATA temp433 LIKE LINE OF lt_result.
    DATA temp434 LIKE sy-tabix.
    DATA temp435 LIKE LINE OF lt_result.
    DATA temp436 LIKE sy-tabix.
    DATA temp437 LIKE LINE OF lt_result.
    DATA temp438 LIKE sy-tabix.
    DATA temp439 LIKE LINE OF lt_result.
    DATA temp440 LIKE sy-tabix.
    DATA temp441 LIKE LINE OF lt_result.
    DATA temp442 LIKE sy-tabix.
    DATA temp443 LIKE LINE OF lt_result.
    DATA temp444 LIKE sy-tabix.
    DATA temp445 LIKE LINE OF lt_result.
    DATA temp446 LIKE sy-tabix.
    DATA temp447 LIKE LINE OF lt_result.
    DATA temp448 LIKE sy-tabix.
    lt_result = mi_diff3->diff_indices(
      it_buffer1 = mt_old
      it_buffer2 = mt_new ).



    temp418 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp417.
    sy-tabix = temp418.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp417-buffer1-key
      exp = 3 ).


    temp420 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp419.
    sy-tabix = temp420.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp419-buffer1-len
      exp = 0 ).


    temp422 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp421.
    sy-tabix = temp422.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp421-buffer2-key
      exp = 3 ).


    temp424 = sy-tabix.
    READ TABLE lt_result INDEX 1 INTO temp423.
    sy-tabix = temp424.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp423-buffer2-len
      exp = 1 ).



    temp426 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp425.
    sy-tabix = temp426.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp425-buffer1-key
      exp = 5 ).


    temp428 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp427.
    sy-tabix = temp428.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp427-buffer1-len
      exp = 1 ).


    temp430 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp429.
    sy-tabix = temp430.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp429-buffer2-key
      exp = 6 ).


    temp432 = sy-tabix.
    READ TABLE lt_result INDEX 2 INTO temp431.
    sy-tabix = temp432.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp431-buffer2-len
      exp = 1 ).



    temp434 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp433.
    sy-tabix = temp434.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp433-buffer1-key
      exp = 8 ).


    temp436 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp435.
    sy-tabix = temp436.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp435-buffer1-len
      exp = 1 ).


    temp438 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp437.
    sy-tabix = temp438.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp437-buffer2-key
      exp = 9 ).


    temp440 = sy-tabix.
    READ TABLE lt_result INDEX 3 INTO temp439.
    sy-tabix = temp440.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp439-buffer2-len
      exp = 0 ).



    temp442 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp441.
    sy-tabix = temp442.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp441-buffer1-key
      exp = 18 ).


    temp444 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp443.
    sy-tabix = temp444.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp443-buffer1-len
      exp = 1 ).


    temp446 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp445.
    sy-tabix = temp446.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp445-buffer2-key
      exp = 18 ).


    temp448 = sy-tabix.
    READ TABLE lt_result INDEX 4 INTO temp447.
    sy-tabix = temp448.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
    ENDIF.
    cl_abap_unit_assert=>assert_equals(
      act = temp447-buffer2-len
      exp = 1 ).

  ENDMETHOD.
ENDCLASS.
