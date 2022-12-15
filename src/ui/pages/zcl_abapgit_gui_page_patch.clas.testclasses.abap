*"* use this source file for your ABAP unit test classes

CLASS ltcl_get_patch_data DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      get_patch_data_add FOR TESTING RAISING cx_static_check,
      get_patch_data_remove FOR TESTING RAISING cx_static_check,
      invalid_patch_missing_file FOR TESTING RAISING cx_static_check,
      invalid_patch_missing_index FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_is_patch_line_possible DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mv_is_patch_line_possible TYPE abap_bool,
      ms_diff_line              TYPE zif_abapgit_definitions=>ty_diff.

    METHODS:
      initial_diff_line FOR TESTING RAISING cx_static_check,
      for_update_patch_shd_be_possbl FOR TESTING RAISING cx_static_check,
      for_insert_patch_shd_be_possbl FOR TESTING RAISING cx_static_check,
      for_delete_patch_shd_be_possbl FOR TESTING RAISING cx_static_check,

      given_diff_line
        IMPORTING
          is_diff_line TYPE zif_abapgit_definitions=>ty_diff OPTIONAL,

      when_is_patch_line_possible,

      then_patch_shd_be_possible,
      then_patch_shd_not_be_possible.

ENDCLASS.

CLASS zcl_abapgit_gui_page_patch DEFINITION LOCAL FRIENDS ltcl_is_patch_line_possible.

CLASS ltcl_get_patch_data IMPLEMENTATION.

  METHOD get_patch_data_add.

    DATA: lv_file_name  TYPE string,
          lv_line_index TYPE string.

    zcl_abapgit_gui_page_patch=>get_patch_data(
      EXPORTING
        iv_patch      = |patch_line_zcl_test_git_add_p.clas.abap_0_19|
      IMPORTING
        ev_filename   = lv_file_name
        ev_line_index = lv_line_index ).

    cl_abap_unit_assert=>assert_equals(
      exp = |zcl_test_git_add_p.clas.abap|
      act = lv_file_name ).

    cl_abap_unit_assert=>assert_equals(
      exp = |19|
      act = lv_line_index ).

  ENDMETHOD.

  METHOD get_patch_data_remove.

    DATA: lv_file_name  TYPE string,
          lv_line_index TYPE string.

    zcl_abapgit_gui_page_patch=>get_patch_data(
      EXPORTING
        iv_patch      = |patch_line_ztest_patch.prog.abap_0_39|
      IMPORTING
        ev_filename   = lv_file_name
        ev_line_index = lv_line_index ).

    cl_abap_unit_assert=>assert_equals(
      exp = |ztest_patch.prog.abap|
      act = lv_file_name ).

    cl_abap_unit_assert=>assert_equals(
      exp = |39|
      act = lv_line_index ).

  ENDMETHOD.


  METHOD invalid_patch_missing_file.

    DATA: lv_file_name  TYPE string,
          lv_line_index TYPE string,
          lx_error      TYPE REF TO zcx_abapgit_exception.

    TRY.
        zcl_abapgit_gui_page_patch=>get_patch_data(
          EXPORTING
            iv_patch      = |patch_39|
          IMPORTING
            ev_filename   = lv_file_name
            ev_line_index = lv_line_index ).

        cl_abap_unit_assert=>fail( ).

      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          exp = |Invalid patch|
          act = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

  METHOD invalid_patch_missing_index.

    DATA: lv_file_name  TYPE string,
          lv_line_index TYPE string,
          lx_error      TYPE REF TO zcx_abapgit_exception.

    TRY.
        zcl_abapgit_gui_page_patch=>get_patch_data(
          EXPORTING
            iv_patch      = |patch_ztest_patch.prog.abap|
          IMPORTING
            ev_filename   = lv_file_name
            ev_line_index = lv_line_index ).

        cl_abap_unit_assert=>fail( ).

      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          exp = |Invalid patch|
          act = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.



CLASS ltcl_is_patch_line_possible IMPLEMENTATION.

  METHOD initial_diff_line.

    given_diff_line( ).
    when_is_patch_line_possible( ).
    then_patch_shd_not_be_possible( ).

  ENDMETHOD.


  METHOD for_update_patch_shd_be_possbl.

    DATA: ls_diff_line TYPE zif_abapgit_definitions=>ty_diff.

    ls_diff_line-result = zif_abapgit_definitions=>c_diff-update.

    given_diff_line( ls_diff_line ).
    when_is_patch_line_possible( ).
    then_patch_shd_be_possible( ).

  ENDMETHOD.


  METHOD for_insert_patch_shd_be_possbl.

    DATA: ls_diff_line TYPE zif_abapgit_definitions=>ty_diff.

    ls_diff_line-result = zif_abapgit_definitions=>c_diff-insert.

    given_diff_line( ls_diff_line ).
    when_is_patch_line_possible( ).
    then_patch_shd_be_possible( ).

  ENDMETHOD.


  METHOD for_delete_patch_shd_be_possbl.

    DATA: ls_diff_line TYPE zif_abapgit_definitions=>ty_diff.

    ls_diff_line-result = zif_abapgit_definitions=>c_diff-delete.

    given_diff_line( ls_diff_line ).
    when_is_patch_line_possible( ).
    then_patch_shd_be_possible( ).

  ENDMETHOD.


  METHOD when_is_patch_line_possible.

    mv_is_patch_line_possible = zcl_abapgit_gui_page_patch=>is_patch_line_possible( ms_diff_line ).

  ENDMETHOD.


  METHOD then_patch_shd_be_possible.

    cl_abap_unit_assert=>assert_not_initial(
        act = mv_is_patch_line_possible
        msg = |Patch should be possible| ).

  ENDMETHOD.


  METHOD then_patch_shd_not_be_possible.

    cl_abap_unit_assert=>assert_initial(
        act = mv_is_patch_line_possible
        msg = |Patch should not be possible| ).

  ENDMETHOD.


  METHOD given_diff_line.

    ms_diff_line = is_diff_line.

  ENDMETHOD.

ENDCLASS.
