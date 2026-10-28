CLASS ltcl_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    METHODS strip_generation_comments_1 FOR TESTING.
    METHODS strip_generation_comments_2 FOR TESTING.
    METHODS fix_vislength_right_gap_1 FOR TESTING.
    METHODS fix_vislength_right_gap_5 FOR TESTING.
    METHODS fix_vislength_right_radio FOR TESTING.
    METHODS fix_vislength_left FOR TESTING.
    METHODS fix_vislength_no_button FOR TESTING.
    METHODS fix_vislength_no_text FOR TESTING.
    METHODS fix_vislength
      IMPORTING
        iv_type             TYPE string
        iv_butt_right       TYPE abap_bool
        iv_vislength        TYPE i
        iv_text_coln        TYPE i
        iv_box_coln         TYPE i
      RETURNING
        VALUE(rv_vislength) TYPE i.
ENDCLASS.

CLASS zcl_abapgit_objects_program DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test IMPLEMENTATION.
  METHOD strip_generation_comments_1.

    DATA lo_cut TYPE REF TO zcl_abapgit_objects_program.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lt_src_orig TYPE abaptxt255_tab.
    DATA lt_src_act TYPE abaptxt255_tab.
    DATA lt_src_exp TYPE abaptxt255_tab.


    APPEND '*---------------------------------------------------------------------*' TO lt_src_orig.
    APPEND '*    view related data declarations' TO lt_src_orig.
    APPEND '*   generation date: 03.02.2022 at 13:19:02' TO lt_src_orig.
    APPEND '*   view maintenance generator version: #001407#' TO lt_src_orig.
    APPEND '*---------------------------------------------------------------------*' TO lt_src_orig.
    APPEND 'some code starts here' TO lt_src_orig.

    APPEND '*---------------------------------------------------------------------*' TO lt_src_exp.
    APPEND '*    view related data declarations' TO lt_src_exp.
    APPEND '*---------------------------------------------------------------------*' TO lt_src_exp.
    APPEND 'some code starts here' TO lt_src_exp.

    " case 1, not FUGR, should skip
    ls_item-obj_type = 'PROG'.
    CREATE OBJECT lo_cut
      EXPORTING
        iv_language = 'E'
        is_item     = ls_item.

    lt_src_act = lt_src_orig.
    lo_cut->strip_generation_comments( CHANGING ct_source = lt_src_act ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_src_act
      exp = lt_src_orig ).

    " case 2, FUGR
    ls_item-obj_type = 'FUGR'.
    CREATE OBJECT lo_cut
      EXPORTING
        iv_language = 'E'
        is_item     = ls_item.

    lt_src_act = lt_src_orig.
    lo_cut->strip_generation_comments( CHANGING ct_source = lt_src_act ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_src_act
      exp = lt_src_exp ).

    " case 3, wrong pattern

    CLEAR lt_src_orig.
    APPEND '*---------------------------------------------------------------------*' TO lt_src_orig.
    APPEND '* extra line' TO lt_src_orig.
    APPEND '*    view related data declarations' TO lt_src_orig.
    APPEND '*   generation date: 03.02.2022 at 13:19:02' TO lt_src_orig.
    APPEND '*   view maintenance generator version: #001407#' TO lt_src_orig.
    APPEND '*---------------------------------------------------------------------*' TO lt_src_orig.
    APPEND 'some code starts here' TO lt_src_orig.

    ls_item-obj_type = 'FUGR'.
    CREATE OBJECT lo_cut
      EXPORTING
        iv_language = 'E'
        is_item     = ls_item.

    lt_src_act = lt_src_orig.
    lo_cut->strip_generation_comments( CHANGING ct_source = lt_src_act ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_src_act
      exp = lt_src_orig ).

  ENDMETHOD.

  METHOD strip_generation_comments_2.

    DATA lo_cut TYPE REF TO zcl_abapgit_objects_program.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lt_src_orig TYPE abaptxt255_tab.
    DATA lt_src_act TYPE abaptxt255_tab.
    DATA lt_src_exp TYPE abaptxt255_tab.


    APPEND '* regenerated at 06.06.2022 10:47:40' TO lt_src_orig.
    APPEND 'some code 1' TO lt_src_orig.
    APPEND 'some code 2' TO lt_src_orig.
    APPEND 'some code 3' TO lt_src_orig.
    APPEND 'some code 4' TO lt_src_orig.
    APPEND 'some code 5' TO lt_src_orig.

    APPEND 'some code 1' TO lt_src_exp.
    APPEND 'some code 2' TO lt_src_exp.
    APPEND 'some code 3' TO lt_src_exp.
    APPEND 'some code 4' TO lt_src_exp.
    APPEND 'some code 5' TO lt_src_exp.

    " case 1, not FUGR, should skip
    ls_item-obj_type = 'FUGR'.
    CREATE OBJECT lo_cut
      EXPORTING
        iv_language = 'E'
        is_item     = ls_item.

    lt_src_act = lt_src_orig.
    lo_cut->strip_generation_comments( CHANGING ct_source = lt_src_act ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_src_act
      exp = lt_src_exp ).

  ENDMETHOD.

  METHOD fix_vislength.

    DATA lt_fieldlist_int TYPE zcl_abapgit_objects_program=>ty_d021s_tt.
    DATA lt_fields TYPE dyfatc_tab.
    DATA ls_field_int LIKE LINE OF lt_fieldlist_int.
    DATA ls_field LIKE LINE OF lt_fields.

    ls_field_int-fnam = 'P_FIELD'.
    IF iv_type = 'RADIO'.
      ls_field_int-fill = 'A'.
    ELSE.
      ls_field_int-fill = 'C'.
    ENDIF.

    IF iv_text_coln > 0.
      ls_field_int-coln = iv_text_coln.
      APPEND ls_field_int TO lt_fieldlist_int.
    ENDIF.

    ls_field_int-coln = iv_box_coln.
    ls_field_int-flg1 = '80'.
    APPEND ls_field_int TO lt_fieldlist_int.

    ls_field-name       = 'P_FIELD'.
    ls_field-type       = iv_type.
    ls_field-butt_right = iv_butt_right.
    ls_field-vislength  = iv_vislength.
    APPEND ls_field TO lt_fields.

    zcl_abapgit_objects_program=>fix_button_right_vislength(
      EXPORTING
        it_fieldlist_int = lt_fieldlist_int
      CHANGING
        ct_fields        = lt_fields ).

    READ TABLE lt_fields INTO ls_field INDEX 1.
    rv_vislength = ls_field-vislength.

  ENDMETHOD.

  METHOD fix_vislength_right_gap_1.

    " text with length 20 in column 2, box in column 23
    cl_abap_unit_assert=>assert_equals(
      act = fix_vislength( iv_type       = 'CHECK'
                           iv_butt_right = abap_true
                           iv_vislength  = 41
                           iv_text_coln  = 3
                           iv_box_coln   = 24 )
      exp = 22 ).

    " already corrected value stays
    cl_abap_unit_assert=>assert_equals(
      act = fix_vislength( iv_type       = 'CHECK'
                           iv_butt_right = abap_true
                           iv_vislength  = 22
                           iv_text_coln  = 3
                           iv_box_coln   = 24 )
      exp = 22 ).

  ENDMETHOD.

  METHOD fix_vislength_right_gap_5.

    " text with length 16 in column 2, box in column 23
    cl_abap_unit_assert=>assert_equals(
      act = fix_vislength( iv_type       = 'CHECK'
                           iv_butt_right = abap_true
                           iv_vislength  = 37
                           iv_text_coln  = 3
                           iv_box_coln   = 24 )
      exp = 22 ).

  ENDMETHOD.

  METHOD fix_vislength_right_radio.

    " text with length 10 in column 2, box in column 13
    cl_abap_unit_assert=>assert_equals(
      act = fix_vislength( iv_type       = 'RADIO'
                           iv_butt_right = abap_true
                           iv_vislength  = 21
                           iv_text_coln  = 3
                           iv_box_coln   = 14 )
      exp = 12 ).

  ENDMETHOD.

  METHOD fix_vislength_left.

    cl_abap_unit_assert=>assert_equals(
      act = fix_vislength( iv_type       = 'CHECK'
                           iv_butt_right = abap_false
                           iv_vislength  = 22
                           iv_text_coln  = 5
                           iv_box_coln   = 3 )
      exp = 22 ).

  ENDMETHOD.

  METHOD fix_vislength_no_button.

    cl_abap_unit_assert=>assert_equals(
      act = fix_vislength( iv_type       = 'TEMPLATE'
                           iv_butt_right = abap_true
                           iv_vislength  = 41
                           iv_text_coln  = 3
                           iv_box_coln   = 24 )
      exp = 41 ).

  ENDMETHOD.

  METHOD fix_vislength_no_text.

    " table control column, box without text
    cl_abap_unit_assert=>assert_equals(
      act = fix_vislength( iv_type       = 'CHECK'
                           iv_butt_right = abap_true
                           iv_vislength  = 1
                           iv_text_coln  = 0
                           iv_box_coln   = 3 )
      exp = 1 ).

  ENDMETHOD.
ENDCLASS.
