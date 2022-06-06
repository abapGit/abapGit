CLASS ltcl_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    METHODS strip_generation_comments_1 for testing.
    METHODS strip_generation_comments_2 for testing.
ENDCLASS.

CLASS zcl_abapgit_objects_program DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test IMPLEMENTATION.
  METHOD strip_generation_comments_1.

    DATA lo_cut TYPE REF TO zcl_abapgit_objects_program.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lt_src_orig TYPE abaptxt255_tab.
    DATA lt_src_act TYPE abaptxt255_tab.
    DATA lt_src_exp TYPE abaptxt255_tab.


    append '*---------------------------------------------------------------------*' to lt_src_orig.
    append '*    view related data declarations' to lt_src_orig.
    append '*   generation date: 03.02.2022 at 13:19:02' to lt_src_orig.
    append '*   view maintenance generator version: #001407#' to lt_src_orig.
    append '*---------------------------------------------------------------------*' to lt_src_orig.
    append 'some code starts here' to lt_src_orig.

    append '*---------------------------------------------------------------------*' to lt_src_exp.
    append '*    view related data declarations' to lt_src_exp.
    append '*---------------------------------------------------------------------*' to lt_src_exp.
    append 'some code starts here' to lt_src_exp.

    " case 1, not FUGR, should skip
    ls_item-obj_type = 'PROG'.
    CREATE OBJECT lo_cut
      EXPORTING
        iv_language = 'E'
        is_item = ls_item.

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
        is_item = ls_item.

    lt_src_act = lt_src_orig.
    lo_cut->strip_generation_comments( CHANGING ct_source = lt_src_act ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_src_act
      exp = lt_src_exp ).

    " case 3, wrong pattern

    clear lt_src_orig.
    append '*---------------------------------------------------------------------*' to lt_src_orig.
    append '* extra line' to lt_src_orig.
    append '*    view related data declarations' to lt_src_orig.
    append '*   generation date: 03.02.2022 at 13:19:02' to lt_src_orig.
    append '*   view maintenance generator version: #001407#' to lt_src_orig.
    append '*---------------------------------------------------------------------*' to lt_src_orig.
    append 'some code starts here' to lt_src_orig.

    ls_item-obj_type = 'FUGR'.
    CREATE OBJECT lo_cut
      EXPORTING
        iv_language = 'E'
        is_item = ls_item.

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


    append '* regenerated at 06.06.2022 10:47:40' to lt_src_orig.
    append 'some code 1' to lt_src_orig.
    append 'some code 2' to lt_src_orig.
    append 'some code 3' to lt_src_orig.
    append 'some code 4' to lt_src_orig.
    append 'some code 5' to lt_src_orig.

    append 'some code 1' to lt_src_exp.
    append 'some code 2' to lt_src_exp.
    append 'some code 3' to lt_src_exp.
    append 'some code 4' to lt_src_exp.
    append 'some code 5' to lt_src_exp.

    " case 1, not FUGR, should skip
    ls_item-obj_type = 'FUGR'.
    CREATE OBJECT lo_cut
      EXPORTING
        iv_language = 'E'
        is_item = ls_item.

    lt_src_act = lt_src_orig.
    lo_cut->strip_generation_comments( CHANGING ct_source = lt_src_act ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_src_act
      exp = lt_src_exp ).

  ENDMETHOD.
ENDCLASS.
