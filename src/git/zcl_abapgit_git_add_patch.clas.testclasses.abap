*"* use this source file for your ABAP unit test classes

CLASS ltcl_calculate_patch DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      single_insert FOR TESTING RAISING cx_static_check,
      multiple_adjacent_insert FOR TESTING RAISING cx_static_check,
      multiple_non_adjacent_insert FOR TESTING RAISING cx_static_check,
      multiple_partial_insert FOR TESTING RAISING cx_static_check,

      single_delete FOR TESTING RAISING cx_static_check,
      multiple_adjacend_delete FOR TESTING RAISING cx_static_check,
      multiple_non_adjacent_delete FOR TESTING RAISING cx_static_check,
      multiple_partial_delete FOR TESTING RAISING cx_static_check,

      single_update FOR TESTING RAISING cx_static_check,
      multiple_adjacend_update FOR TESTING RAISING cx_static_check,
      multiple_non_adjacent_update FOR TESTING RAISING cx_static_check,
      multiple_partial_update FOR TESTING RAISING cx_static_check,

      mixed FOR TESTING RAISING cx_static_check,

      no_diff FOR TESTING RAISING cx_static_check,

      unknown_result_type FOR TESTING RAISING cx_static_check.

    METHODS:
      given_diff
        IMPORTING
          iv_patch_flag TYPE zif_abapgit_definitions=>ty_diff-patch_flag
          iv_new_num    TYPE zif_abapgit_definitions=>ty_diff-new_num
          iv_new        TYPE zif_abapgit_definitions=>ty_diff-new
          iv_result     TYPE zif_abapgit_definitions=>ty_diff-result
          iv_old_num    TYPE zif_abapgit_definitions=>ty_diff-old_num
          iv_old        TYPE zif_abapgit_definitions=>ty_diff-old
          iv_short      TYPE zif_abapgit_definitions=>ty_diff-short DEFAULT 'X'
          iv_beacon     TYPE zif_abapgit_definitions=>ty_diff-beacon DEFAULT 1,

      when_patch_is_calculated,

      then_patch_should_be
        IMPORTING
          iv_exp_patch TYPE string,
      then_exception_is_raised.

    DATA:
      mt_diff  TYPE zif_abapgit_definitions=>ty_diffs_tt,
      mt_patch TYPE string_table,
      mx_error TYPE REF TO zcx_abapgit_exception.

ENDCLASS.


CLASS ltcl_calculate_patch IMPLEMENTATION.

  METHOD single_insert.

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    1'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    1'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '    2'
                iv_new        = 'write: `Test`.'
                iv_result     = 'I'
                iv_old_num    = '     '
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    3'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    2'
                iv_old        = ' ' ).

    when_patch_is_calculated( ).

    then_patch_should_be(
      |\n| &&
      |write: `Test`.\n| &&
      |\n| ).

  ENDMETHOD.

  METHOD multiple_adjacent_insert.

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    1'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    1'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '    2'
                iv_new        = 'write: `Test`.'
                iv_result     = 'I'
                iv_old_num    = '     '
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '    3'
                iv_new        = 'write: `Hello world`.'
                iv_result     = 'I'
                iv_old_num    = '     '
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    4'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    2'
                iv_old        = ' ' ).

    when_patch_is_calculated( ).

    then_patch_should_be(
      |\n| &&
      |write: `Test`.\n| &&
      |write: `Hello world`.\n| &&
      |\n| ).

  ENDMETHOD.

  METHOD multiple_non_adjacent_insert.

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    1'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    1'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '    2'
                iv_new        = 'write: `Test`.'
                iv_result     = 'I'
                iv_old_num    = '     '
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    3'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    2'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '    4'
                iv_new        = 'write: `Hello world`.'
                iv_result     = 'I'
                iv_old_num    = '     '
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    5'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    3'
                iv_old        = ' ' ).

    when_patch_is_calculated( ).

    then_patch_should_be(
      |\n| &&
      |write: `Test`.\n| &&
      |\n| &&
      |write: `Hello world`.\n| &&
      |\n| ).

  ENDMETHOD.

  METHOD multiple_partial_insert.

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    1'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    1'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '    2'
                iv_new        = 'write: `Test`.'
                iv_result     = 'I'
                iv_old_num    = '     '
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    3'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    2'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    4'
                iv_new        = 'write: `Hello world`.'
                iv_result     = 'I'
                iv_old_num    = '     '
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    5'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    3'
                iv_old        = ' ' ).

    when_patch_is_calculated( ).

    then_patch_should_be(
      |\n| &&
      |write: `Test`.\n| &&
      |\n| &&
      |\n| ).

  ENDMETHOD.

  METHOD single_delete.

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    1'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    1'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '     '
                iv_new        = ' '
                iv_result     = 'D'
                iv_old_num    = '    2'
                iv_old        = 'write: `Test`.' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    2'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    3'
                iv_old        = ' ' ).

    when_patch_is_calculated( ).

    then_patch_should_be( |\n\n| ).

  ENDMETHOD.

  METHOD multiple_adjacend_delete.

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    1'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    1'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '     '
                iv_new        = ' '
                iv_result     = 'D'
                iv_old_num    = '    2'
                iv_old        = 'write: `Test`.' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '     '
                iv_new        = ' '
                iv_result     = 'D'
                iv_old_num    = '    3'
                iv_old        = 'write: `Hello world`.' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    2'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    4'
                iv_old        = ' ' ).

    when_patch_is_calculated( ).

    then_patch_should_be( |\n\n| ).

  ENDMETHOD.

  METHOD multiple_non_adjacent_delete.

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    1'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    1'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '     '
                iv_new        = ' '
                iv_result     = 'D'
                iv_old_num    = '    2'
                iv_old        = 'write: `Test`.' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '     '
                iv_new        = ' '
                iv_result     = 'D'
                iv_old_num    = '    3'
                iv_old        = 'write: `Hello world`.' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '     '
                iv_new        = ' '
                iv_result     = 'D'
                iv_old_num    = '    4'
                iv_old        = 'write: `Hello 123`.' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    2'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    5'
                iv_old        = ' ' ).

    when_patch_is_calculated( ).

    then_patch_should_be(
      |\n| &&
      |write: `Hello world`.| &&
      |\n| ).

  ENDMETHOD.

  METHOD multiple_partial_delete.

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    1'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    1'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '     '
                iv_new        = ' '
                iv_result     = 'D'
                iv_old_num    = '    2'
                iv_old        = 'write: `Test`.' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '     '
                iv_new        = ' '
                iv_result     = 'D'
                iv_old_num    = '    3'
                iv_old        = 'write: `Hello world`.' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '     '
                iv_new        = ' '
                iv_result     = 'D'
                iv_old_num    = '    4'
                iv_old        = 'write: `Hello 123`.' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '     '
                iv_new        = ' '
                iv_result     = 'D'
                iv_old_num    = '    5'
                iv_old        = 'write: `Hello test`.' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    2'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    6'
                iv_old        = ' ' ).

    when_patch_is_calculated( ).

    then_patch_should_be(
      |\n| &&
      |write: `Hello world`.\n| &&
      |write: `Hello 123`.\n| &&
      |\n| ).

  ENDMETHOD.

  METHOD single_update.

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    1'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    1'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '    2'
                iv_new        = 'write: `Hello world`.'
                iv_result     = 'U'
                iv_old_num    = '    2'
                iv_old        = 'write: `Test`.' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    3'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    3'
                iv_old        = ' ' ).


    when_patch_is_calculated( ).

    then_patch_should_be(
      |\n| &&
      |write: `Hello world`.\n| &&
      |\n| ).

  ENDMETHOD.

  METHOD multiple_adjacend_update.

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    1'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    1'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '    2'
                iv_new        = 'write: `Hello world`.'
                iv_result     = 'U'
                iv_old_num    = '    2'
                iv_old        = 'write: `Test`.' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '    3'
                iv_new        = 'write: `Test`.'
                iv_result     = 'U'
                iv_old_num    = '    3'
                iv_old        = 'write: `Hello world`.' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    4'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    4'
                iv_old        = ' ' ).

    when_patch_is_calculated( ).

    then_patch_should_be(
      |\n| &&
      |write: `Hello world`.\n| &&
      |write: `Test`.\n| &&
      |\n| ).

  ENDMETHOD.

  METHOD multiple_non_adjacent_update.

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    1'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    1'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '    2'
                iv_new        = 'write: `Hello world`.'
                iv_result     = 'U'
                iv_old_num    = '    2'
                iv_old        = 'write: `Test`.' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    3'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    3'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '    4'
                iv_new        = 'write: `Test`.'
                iv_result     = 'U'
                iv_old_num    = '    4'
                iv_old        = 'write: `Hello world`.' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    5'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    5'
                iv_old        = ' ' ).

    when_patch_is_calculated( ).

    then_patch_should_be(
      |\n| &&
      |write: `Hello world`.\n| &&
      |\n| &&
      |write: `Test`.\n| &&
      |\n| ).

  ENDMETHOD.

  METHOD multiple_partial_update.


    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    1'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    1'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '    2'
                iv_new        = 'write: `Hello world`.'
                iv_result     = 'U'
                iv_old_num    = '    2'
                iv_old        = 'write: `Test`.' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    3'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    3'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    4'
                iv_new        = 'write: `Test`.'
                iv_result     = 'U'
                iv_old_num    = '    4'
                iv_old        = 'write: `Hello world`.' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    5'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    5'
                iv_old        = ' ' ).

    when_patch_is_calculated( ).

    then_patch_should_be(
      |\n| &&
      |write: `Hello world`.\n| &&
      |\n| &&
      |write: `Hello world`.\n| &&
      |\n| ).

  ENDMETHOD.

  METHOD mixed.

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    1'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    1'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '    2'
                iv_new        = 'write: `Hello world`.'
                iv_result     = 'U'
                iv_old_num    = '    2'
                iv_old        = 'write: `Test`.' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    3'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    3'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    4'
                iv_new        = 'write: `Test`.'
                iv_result     = 'U'
                iv_old_num    = '    4'
                iv_old        = 'write: `Hello world`.' ).

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    5'
                iv_new        = ' '
                iv_result     = ' '
                iv_old_num    = '    5'
                iv_old        = ' ' ).

    given_diff( iv_patch_flag = 'X'
                iv_new_num    = '    6'
                iv_new        = 'write: `newline`.'
                iv_result     = 'I'
                iv_old_num    = '     '
                iv_old        = ' ' ).

    when_patch_is_calculated( ).

    then_patch_should_be(
      |\n| &&
      |write: `Hello world`.\n| &&
      |\n| &&
      |write: `Hello world`.\n| &&
      |\n| &&
      |write: `newline`.\n| ).

  ENDMETHOD.


  METHOD no_diff.

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    1'
                iv_new        = '" new'
                iv_result     = ' '
                iv_old_num    = '    1'
                iv_old        = '" old' ).

    when_patch_is_calculated( ).

    then_patch_should_be( |" old| ).

  ENDMETHOD.


  METHOD unknown_result_type.

    given_diff( iv_patch_flag = ' '
                iv_new_num    = '    1'
                iv_new        = ' '
                iv_result     = 'X'
                iv_old_num    = '    1'
                iv_old        = ' ' ).

    when_patch_is_calculated( ).

    then_exception_is_raised( ).

  ENDMETHOD.

  METHOD given_diff.

    DATA: ls_diff LIKE LINE OF mt_diff.

    ls_diff-patch_flag = iv_patch_flag.
    ls_diff-new_num    = iv_new_num.
    ls_diff-new        = iv_new.
    ls_diff-result     = iv_result.
    ls_diff-old_num    = iv_old_num.
    ls_diff-old        = iv_old.
    ls_diff-short      = iv_short.
    ls_diff-beacon     = iv_beacon.
    INSERT ls_diff INTO TABLE mt_diff.

  ENDMETHOD.


  METHOD when_patch_is_calculated.

    DATA: lo_git_add_patch TYPE REF TO zcl_abapgit_git_add_patch.

    CREATE OBJECT lo_git_add_patch
      EXPORTING
        it_diff = mt_diff.

    TRY.
        mt_patch = lo_git_add_patch->get_patch( ).
      CATCH zcx_abapgit_exception INTO mx_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD then_patch_should_be.

    DATA: lt_patch TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          lv_patch LIKE LINE OF lt_patch.

    FIELD-SYMBOLS: <lv_patch> LIKE LINE OF mt_patch.

    SPLIT iv_exp_patch AT |\n| INTO TABLE lt_patch IN CHARACTER MODE.

    LOOP AT lt_patch INTO lv_patch.
      READ TABLE mt_patch INDEX sy-tabix ASSIGNING <lv_patch>.
      cl_abap_unit_assert=>assert_subrc( ).

      cl_abap_unit_assert=>assert_equals(
        exp = lv_patch
        act = <lv_patch> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD then_exception_is_raised.

    cl_abap_unit_assert=>assert_equals(
      exp = |Unknown result|
      act = mx_error->get_text( ) ).

  ENDMETHOD.

ENDCLASS.
