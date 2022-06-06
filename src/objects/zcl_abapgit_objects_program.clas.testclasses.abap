CLASS ltcl_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    METHODS strip_generation_comments_1 FOR TESTING.
    METHODS strip_generation_comments_2 FOR TESTING.
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
ENDCLASS.
