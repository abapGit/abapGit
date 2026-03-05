*"* use this source file for your ABAP unit test classes
CLASS lcl_mock_event DEFINITION.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_event.
    METHODS constructor.
    METHODS set_file
      IMPORTING
        iv_key   TYPE csequence
        iv_value TYPE csequence
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA mo_files TYPE REF TO zcl_abapgit_string_map.

ENDCLASS.


CLASS ltcl_stage DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_mock_event TYPE REF TO lcl_mock_event,
      mt_status     TYPE zif_abapgit_definitions=>ty_results_ts_path,
      mt_local      TYPE zif_abapgit_definitions=>ty_files_item_tt,
      mo_stage      TYPE REF TO zcl_abapgit_stage,
      mx_error      TYPE REF TO zcx_abapgit_exception.

    METHODS: setup RAISING cx_static_check,
      emtpy_list FOR TESTING RAISING cx_static_check,
      unknown_file FOR TESTING RAISING cx_static_check,
      add FOR TESTING RAISING cx_static_check,
      uppercase_path FOR TESTING RAISING cx_static_check,
      uppercase_status_path FOR TESTING RAISING cx_static_check,
      mixed_case FOR TESTING RAISING cx_static_check,
      unknown_method FOR TESTING RAISING cx_static_check,
      status_missing FOR TESTING RAISING cx_static_check,
      remove FOR TESTING RAISING cx_static_check,
      check_selected_rm FOR TESTING RAISING cx_static_check,
      skip FOR TESTING RAISING cx_static_check,
      when_stage_selected
        RAISING
          zcx_abapgit_exception,
      given_file
        IMPORTING
          iv_key   TYPE csequence
          iv_value TYPE csequence
        RAISING
          zcx_abapgit_exception,
      given_status
        IMPORTING
          iv_path     TYPE csequence
          iv_filename TYPE csequence,
      then_file_is_ignored
        IMPORTING
          iv_path     TYPE csequence
          iv_filename TYPE csequence,
      then_file_is_added
        IMPORTING
          iv_path     TYPE csequence
          iv_filename TYPE csequence,
      then_error_contains
        IMPORTING
          iv_exp_error TYPE csequence,
      given_local
        IMPORTING
          iv_path     TYPE csequence
          iv_filename TYPE csequence,
      then_file_is_removed
        IMPORTING
          iv_path     TYPE csequence
          iv_filename TYPE csequence,
      then_there_is_nothing_to_stage,
      assert_table_contains
        IMPORTING
          is_stage TYPE zif_abapgit_definitions=>ty_stage
          it_stage TYPE zif_abapgit_definitions=>ty_stage_tt.

ENDCLASS.

CLASS lcl_mock_event IMPLEMENTATION.

  METHOD constructor.

    CREATE OBJECT mo_files.

  ENDMETHOD.

  METHOD zif_abapgit_gui_event~form_data.

    ro_string_map = mo_files.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event~query.

  ENDMETHOD.


  METHOD set_file.

    mo_files->set(
        iv_key = iv_key
        iv_val = iv_value ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_stage IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_mock_event TYPE lcl_mock_event.

  ENDMETHOD.


  METHOD emtpy_list.

    when_stage_selected( ).
    then_error_contains( 'empty list' ).

  ENDMETHOD.


  METHOD unknown_file.

    given_file(
        iv_key   = '/src/zddls.ddls.baseinfo'
        iv_value = zif_abapgit_definitions=>c_method-add ).

    given_status(
        iv_path     = '/src/'
        iv_filename = 'zddls.ddls.baseinfo' ).

    when_stage_selected( ).

    then_error_contains( 'unknown file' ).

  ENDMETHOD.


  METHOD add.

    given_file(
        iv_key = '/src/zddls.ddls.baseinfo'
        iv_value = zif_abapgit_definitions=>c_method-add ).

    given_status(
        iv_path = '/src/'
        iv_filename = 'zddls.ddls.baseinfo' ).

    given_local(
        iv_path = '/src/'
        iv_filename = 'zddls.ddls.baseinfo' ).

    when_stage_selected( ).

    then_file_is_added(
        iv_path     = '/src/'
        iv_filename = 'zddls.ddls.baseinfo' ).

  ENDMETHOD.


  METHOD uppercase_path.

    given_file(
        iv_key   = '/SRC/ZDDLS.DDLS.BASEINFO'
        iv_value = zif_abapgit_definitions=>c_method-ignore ).

    given_status(
        iv_path     = '/src/'
        iv_filename = 'zddls.ddls.baseinfo' ).

    when_stage_selected( ).

    then_file_is_ignored(
        iv_path     = '/src/'
        iv_filename = 'zddls.ddls.baseinfo' ).

  ENDMETHOD.


  METHOD uppercase_status_path.

    given_file(
        iv_key   = '/CODE/ZFALV.FUGR.SCREEN_0200.ABAP'
        iv_value = zif_abapgit_definitions=>c_method-ignore ).

    given_status(
        iv_path     = '/CODE/'
        iv_filename = 'zfalv.fugr.screen_0200.abap' ).

    given_local(
        iv_path     = '/CODE/'
        iv_filename = 'zfalv.fugr.screen_0200.abap' ).

    when_stage_selected( ).

    then_file_is_ignored(
        iv_path     = '/CODE/'
        iv_filename = 'zfalv.fugr.screen_0200.abap' ).

  ENDMETHOD.


  METHOD mixed_case.

    given_file(
        iv_key   = '/SRC/PACKAGE.DEVC.XML'
        iv_value = zif_abapgit_definitions=>c_method-add ).

    given_status(
        iv_path     = '/SrC/'
        iv_filename = 'package.devc.xml' ).

    given_local(
        iv_path     = '/SrC/'
        iv_filename = 'package.devc.xml' ).

    when_stage_selected( ).

    then_file_is_added(
        iv_path     = '/SrC/'
        iv_filename = 'package.devc.xml' ).

  ENDMETHOD.


  METHOD unknown_method.

    given_file(
        iv_key   = '/SRC/ZDDLS.DDLS.BASEINFO'
        iv_value = 'X' ).

    given_status(
        iv_path     = '/src/'
        iv_filename = 'zddls.ddls.baseinfo' ).

    when_stage_selected( ).

    then_error_contains( 'unknown method' ).

  ENDMETHOD.


  METHOD status_missing.

    given_file(
        iv_key   = '/SRC/ZDDLS.DDLS.BASEINFO'
        iv_value = zif_abapgit_definitions=>c_method-add ).

    when_stage_selected( ).

    then_error_contains( 'Unable to stage' ).
  ENDMETHOD.


  METHOD remove.

    given_file(
        iv_key   = '/src/zddls.ddls.baseinfo'
        iv_value = zif_abapgit_definitions=>c_method-rm ).

    given_status(
        iv_path     = '/src/'
        iv_filename = 'zddls.ddls.baseinfo' ).

    given_local(
        iv_path     = '/src/'
        iv_filename = 'zddls.ddls.baseinfo' ).

    when_stage_selected( ).

    then_file_is_removed(
        iv_path     = '/src/'
        iv_filename = 'zddls.ddls.baseinfo' ).

  ENDMETHOD.


  METHOD check_selected_rm.

    given_file(
        iv_key   = '/src/p1/zddls.ddls.baseinfo'
        iv_value = zif_abapgit_definitions=>c_method-add ).

    given_file(
        iv_key   = '/src/p2/zddls.ddls.baseinfo'
        iv_value = zif_abapgit_definitions=>c_method-add ).

    given_status(
        iv_path     = '/src/'
        iv_filename = 'zddls.ddls.baseinfo' ).

    given_local(
        iv_path     = '/src/'
        iv_filename = 'zddls.ddls.baseinfo' ).

    when_stage_selected( ).

    then_error_contains( |you have to remove| ).

  ENDMETHOD.


  METHOD skip.

    given_file(
        iv_key   = '/src/zddls.ddls.baseinfo'
        iv_value = zif_abapgit_definitions=>c_method-skip ).

    when_stage_selected( ).

    then_there_is_nothing_to_stage( ).

  ENDMETHOD.


  METHOD when_stage_selected.

    TRY.
        mo_stage = lcl_selected=>get_instance( )->stage_selected(
                       ii_event  = mo_mock_event
                       it_status = mt_status
                       it_local  = mt_local ).

      CATCH zcx_abapgit_exception INTO mx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD given_file.

    mo_mock_event->set_file(
        iv_key   = iv_key
        iv_value = iv_value ).

  ENDMETHOD.


  METHOD given_status.

    DATA ls_status LIKE LINE OF mt_status.

    ls_status-path = iv_path.
    ls_status-filename = iv_filename.
    INSERT ls_status INTO TABLE mt_status.

  ENDMETHOD.


  METHOD then_file_is_ignored.

    DATA: lt_stage TYPE zif_abapgit_definitions=>ty_stage_tt.
    DATA: ls_exp LIKE LINE OF lt_stage.

    lt_stage = mo_stage->get_all( ).

    ls_exp-file-path     = iv_path.
    ls_exp-file-filename = iv_filename.
    ls_exp-method        = zif_abapgit_definitions=>c_method-ignore.

    assert_table_contains(
        is_stage = ls_exp
        it_stage = lt_stage ).

  ENDMETHOD.


  METHOD then_file_is_added.

    DATA: lt_stage TYPE zif_abapgit_definitions=>ty_stage_tt.
    DATA: ls_exp LIKE LINE OF lt_stage.

    lt_stage = mo_stage->get_all( ).

    ls_exp-file-path       = iv_path.
    ls_exp-file-filename   = iv_filename.
    ls_exp-method          = zif_abapgit_definitions=>c_method-add.
    ls_exp-status-filename = iv_filename.
    ls_exp-status-path     = iv_path.

    assert_table_contains(
        is_stage = ls_exp
        it_stage = lt_stage ).

  ENDMETHOD.


  METHOD then_error_contains.
    cl_abap_unit_assert=>assert_text_matches(
        pattern = iv_exp_error
        text    = mx_error->get_text( ) ).
  ENDMETHOD.


  METHOD given_local.

    DATA ls_local LIKE LINE OF mt_local.

    ls_local-file-path = iv_path.
    ls_local-file-filename = iv_filename.
    INSERT ls_local INTO TABLE mt_local.

  ENDMETHOD.


  METHOD then_file_is_removed.

    DATA: lt_stage TYPE zif_abapgit_definitions=>ty_stage_tt.
    DATA: ls_exp LIKE LINE OF lt_stage.

    lt_stage = mo_stage->get_all( ).

    ls_exp-file-path       = iv_path.
    ls_exp-file-filename   = iv_filename.
    ls_exp-method          = zif_abapgit_definitions=>c_method-rm.
    ls_exp-status-filename = iv_filename.
    ls_exp-status-path     = iv_path.

    assert_table_contains(
        is_stage = ls_exp
        it_stage = lt_stage ).

  ENDMETHOD.


  METHOD then_there_is_nothing_to_stage.

    cl_abap_unit_assert=>assert_initial( mo_stage->get_all( ) ).

  ENDMETHOD.


  METHOD assert_table_contains.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF it_stage.

    READ TABLE it_stage WITH TABLE KEY file-path = is_stage-file-path
                                       file-filename = is_stage-file-filename
                        ASSIGNING <ls_stage>.
    cl_abap_unit_assert=>assert_subrc( 0 ).

    cl_abap_unit_assert=>assert_equals(
        act = <ls_stage>
        exp = is_stage ).

  ENDMETHOD.

ENDCLASS.
