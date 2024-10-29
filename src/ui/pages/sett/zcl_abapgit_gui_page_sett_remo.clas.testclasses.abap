CLASS ltd_git_transport DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_git_transport.

ENDCLASS.


CLASS ltd_branch_list DEFINITION FINAL FOR TESTING INHERITING FROM zcl_abapgit_git_branch_list.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          iv_data TYPE string
        RAISING
          zcx_abapgit_exception,

      find_by_name REDEFINITION.

ENDCLASS.


CLASS ltcl_validate_form DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS:
      c_git_repo_url TYPE string VALUE 'https://repo.com/demo.git'.

    DATA:
      mo_cut                    TYPE REF TO zcl_abapgit_gui_page_sett_remo,

      mo_repo                   TYPE REF TO zcl_abapgit_repo_online,
      mo_given_form_data        TYPE REF TO zcl_abapgit_string_map,
      mo_act_validation_log     TYPE REF TO zcl_abapgit_string_map,

      mo_git_transport_mock     TYPE REF TO zif_abapgit_git_transport,
      mo_frontend_services_mock TYPE REF TO zif_abapgit_frontend_services.

    METHODS:
      setup RAISING cx_static_check,
      teardown RAISING cx_static_check,

      switch_to_offline_no_error FOR TESTING RAISING cx_static_check,
      invalid_url FOR TESTING RAISING cx_static_check,
      invalid_url2 FOR TESTING RAISING cx_static_check,
      valid_url_no_head_type FOR TESTING RAISING cx_static_check,

      invalid_branch FOR TESTING RAISING cx_static_check,
      valid_branch FOR TESTING RAISING cx_static_check,

      invalid_tag FOR TESTING RAISING cx_static_check,
      valid_tag FOR TESTING RAISING cx_static_check,

      invalid_pull_request FOR TESTING RAISING cx_static_check,
      valid_pull_request FOR TESTING RAISING cx_static_check,

      invalid_commit FOR TESTING RAISING cx_static_check,
      valid_commit FOR TESTING RAISING cx_static_check,

      when_validate_form
        RAISING
          zcx_abapgit_exception,

      then_no_error_shd_occur,
      then_error_shd_occur
        IMPORTING
          iv_exp_error_key TYPE string.

ENDCLASS.

CLASS zcl_abapgit_gui_page_sett_remo DEFINITION LOCAL FRIENDS ltcl_validate_form.

CLASS ltd_git_transport IMPLEMENTATION.

  METHOD zif_abapgit_git_transport~branches.

    CONSTANTS: lc_dummy_data TYPE string VALUE '0000'.

    CREATE OBJECT ro_branch_list TYPE ltd_branch_list
      EXPORTING
        iv_data = lc_dummy_data.

  ENDMETHOD.

ENDCLASS.


CLASS ltd_branch_list IMPLEMENTATION.

  METHOD constructor.

    super->constructor( iv_data ).

  ENDMETHOD.


  METHOD find_by_name.

    IF iv_branch_name CS 'feature'
    OR iv_branch_name CS 'inv_tag'
    OR iv_branch_name CS 'inv_pr'
    OR iv_branch_name IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_exception.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_validate_form IMPLEMENTATION.

  METHOD setup.

    DATA: ls_data TYPE zif_abapgit_persistence=>ty_repo.

    CREATE OBJECT mo_git_transport_mock TYPE ltd_git_transport.
    zcl_abapgit_git_injector=>set_git_transport( mo_git_transport_mock ).

    " Disable GUI
    mo_frontend_services_mock = zcl_abapgit_ui_factory=>get_frontend_services( abap_true ).

    ls_data-key = 1.
    ls_data-branch_name = 'main'.

    CREATE OBJECT mo_repo EXPORTING is_data = ls_data.

    CREATE OBJECT mo_given_form_data.
    mo_given_form_data->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-branch
        iv_val = 'main' ).

    CREATE OBJECT mo_cut EXPORTING io_repo = mo_repo.

  ENDMETHOD.


  METHOD teardown.

    CLEAR:
      mo_git_transport_mock,
      mo_frontend_services_mock.
    zcl_abapgit_git_injector=>set_git_transport( mo_git_transport_mock ).
    zcl_abapgit_ui_injector=>set_frontend_services( mo_frontend_services_mock ).

  ENDMETHOD.


  METHOD switch_to_offline_no_error.

    mo_given_form_data->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-offline
        iv_val = abap_true ).
    when_validate_form( ).
    then_no_error_shd_occur( ).

  ENDMETHOD.


  METHOD invalid_url.

    mo_given_form_data->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-offline
        iv_val = abap_false
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-url
        iv_val = 'http://test' ).

    when_validate_form( ).
    then_error_shd_occur( zcl_abapgit_gui_page_sett_remo=>c_id-url ).

  ENDMETHOD.


  METHOD invalid_url2.

    mo_given_form_data->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-offline
        iv_val = abap_false
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-url
        iv_val = 'test' ).

    when_validate_form( ).
    then_error_shd_occur( zcl_abapgit_gui_page_sett_remo=>c_id-url ).

  ENDMETHOD.


  METHOD valid_url_no_head_type.

    mo_given_form_data->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-offline
        iv_val = abap_false
     )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-url
        iv_val = c_git_repo_url ).

    when_validate_form( ).
    then_error_shd_occur( zcl_abapgit_gui_page_sett_remo=>c_id-head_type ).

  ENDMETHOD.


  METHOD invalid_branch.

    mo_given_form_data->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-offline
        iv_val = abap_false
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-url
        iv_val = c_git_repo_url
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-head_type
        iv_val = zcl_abapgit_gui_page_sett_remo=>c_head_types-branch
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-branch
        iv_val = 'feature' ).

    when_validate_form( ).
    then_error_shd_occur( zcl_abapgit_gui_page_sett_remo=>c_id-branch ).

  ENDMETHOD.


  METHOD valid_branch.

    mo_given_form_data->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-offline
        iv_val = abap_false
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-url
        iv_val = c_git_repo_url
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-head_type
        iv_val = zcl_abapgit_gui_page_sett_remo=>c_head_types-branch
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-branch
        iv_val = 'main' ).

    when_validate_form( ).
    then_no_error_shd_occur( ).

  ENDMETHOD.


  METHOD invalid_tag.

    mo_given_form_data->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-offline
        iv_val = abap_false
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-url
        iv_val = c_git_repo_url
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-head_type
        iv_val = zcl_abapgit_gui_page_sett_remo=>c_head_types-tag
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-tag
        iv_val = 'inv_tag' ).

    when_validate_form( ).
    then_error_shd_occur( zcl_abapgit_gui_page_sett_remo=>c_id-tag ).

  ENDMETHOD.


  METHOD valid_tag.

    mo_given_form_data->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-offline
        iv_val = abap_false
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-url
        iv_val = c_git_repo_url
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-head_type
        iv_val = zcl_abapgit_gui_page_sett_remo=>c_head_types-tag
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-tag
        iv_val = 'v1.1.3' ).

    when_validate_form( ).
    then_no_error_shd_occur( ).

  ENDMETHOD.


  METHOD invalid_pull_request.

    mo_given_form_data->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-offline
        iv_val = abap_false
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-url
        iv_val = c_git_repo_url
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-head_type
        iv_val = zcl_abapgit_gui_page_sett_remo=>c_head_types-pull_request
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-pull_request
        iv_val = 'x@inv_pr' ).

    when_validate_form( ).
    then_error_shd_occur( zcl_abapgit_gui_page_sett_remo=>c_id-pull_request ).

  ENDMETHOD.


  METHOD valid_pull_request.

    mo_given_form_data->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-offline
        iv_val = abap_false
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-url
        iv_val = c_git_repo_url
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-head_type
        iv_val = zcl_abapgit_gui_page_sett_remo=>c_head_types-pull_request
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-pull_request
        iv_val = 'x@pr' ).

    when_validate_form( ).
    then_no_error_shd_occur( ).

  ENDMETHOD.


  METHOD invalid_commit.

    mo_given_form_data->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-offline
        iv_val = abap_false
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-url
        iv_val = c_git_repo_url
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-head_type
        iv_val = zcl_abapgit_gui_page_sett_remo=>c_head_types-commit
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-commit
        iv_val = 'ABCD1234' ).

    when_validate_form( ).
    then_error_shd_occur( zcl_abapgit_gui_page_sett_remo=>c_id-commit ).

  ENDMETHOD.


  METHOD valid_commit.

    mo_given_form_data->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-offline
        iv_val = abap_false
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-url
        iv_val = c_git_repo_url
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-head_type
        iv_val = zcl_abapgit_gui_page_sett_remo=>c_head_types-commit
      )->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-commit
        iv_val = '1d4abf5342a939202ae24ab4a5ad78da3cad24fb' ).

    when_validate_form( ).
    then_no_error_shd_occur( ).

  ENDMETHOD.


  METHOD when_validate_form.

    mo_act_validation_log = mo_cut->validate_form(
      io_form_data        = mo_given_form_data
      iv_connection_check = abap_false ).

  ENDMETHOD.


  METHOD then_no_error_shd_occur.

    cl_abap_unit_assert=>assert_equals(
        exp = abap_true
        act = mo_act_validation_log->is_empty( ) ).

  ENDMETHOD.


  METHOD then_error_shd_occur.

    cl_abap_unit_assert=>assert_not_initial( mo_act_validation_log->get( iv_exp_error_key ) ).

  ENDMETHOD.

ENDCLASS.
