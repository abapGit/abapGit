CLASS ltd_git_transport DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_git_transport.

ENDCLASS.


CLASS ltd_branch_list DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_git_branch_list.
ENDCLASS.


CLASS ltd_repo_online DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_repo_online.

ENDCLASS.


CLASS ltcl_validate_form DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS:
      c_git_repo_url TYPE string VALUE 'https://repo.com/demo.git'.

    DATA:
      mo_cut                    TYPE REF TO zcl_abapgit_gui_page_sett_remo,

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

    CREATE OBJECT ri_branch_list TYPE ltd_branch_list.

  ENDMETHOD.

ENDCLASS.


CLASS ltd_branch_list IMPLEMENTATION.

  METHOD zif_abapgit_git_branch_list~find_by_name.

    IF iv_branch_name CS 'feature'
    OR iv_branch_name CS 'inv_tag'
    OR iv_branch_name CS 'inv_pr'
    OR iv_branch_name IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_exception.
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_git_branch_list~get_head_symref.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_git_branch_list~get_all.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_git_branch_list~get_branches_only.
    RETURN.
  ENDMETHOD.
  METHOD zif_abapgit_git_branch_list~get_tags_only.
    RETURN.
  ENDMETHOD.

ENDCLASS.


CLASS ltd_repo_online IMPLEMENTATION.

  METHOD zif_abapgit_repo~bind_listener.
  ENDMETHOD.

  METHOD zif_abapgit_repo~checksums.
  ENDMETHOD.

  METHOD zif_abapgit_repo_online~check_for_valid_branch.
  ENDMETHOD.

  METHOD zif_abapgit_repo_online~create_branch.
  ENDMETHOD.

  METHOD zif_abapgit_repo~create_new_log.
  ENDMETHOD.

  METHOD zif_abapgit_repo~delete_checks.
  ENDMETHOD.

  METHOD zif_abapgit_repo~deserialize.
  ENDMETHOD.

  METHOD zif_abapgit_repo~deserialize_checks.
  ENDMETHOD.

  METHOD zif_abapgit_repo~find_remote_dot_abapgit.
  ENDMETHOD.

  METHOD zif_abapgit_repo_online~get_current_remote.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_data_config.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_dot_abapgit.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_dot_apack.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_files_local.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_files_local_filtered.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_files_remote.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_key.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_local_settings.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_log.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_name.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_package.
  ENDMETHOD.

  METHOD zif_abapgit_repo_online~get_selected_branch.
  ENDMETHOD.

  METHOD zif_abapgit_repo_online~get_selected_commit.
  ENDMETHOD.

  METHOD zif_abapgit_repo_online~get_switched_origin.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_tadir_objects.
  ENDMETHOD.

  METHOD zif_abapgit_repo~get_unsupported_objects_local.
  ENDMETHOD.

  METHOD zif_abapgit_repo_online~get_url.
  ENDMETHOD.

  METHOD zif_abapgit_repo~has_remote_source.
  ENDMETHOD.

  METHOD zif_abapgit_repo~is_offline.
  ENDMETHOD.

  METHOD zif_abapgit_repo_online~push.
  ENDMETHOD.

  METHOD zif_abapgit_repo~refresh.
  ENDMETHOD.

  METHOD zif_abapgit_repo~refresh_local_object.
  ENDMETHOD.

  METHOD zif_abapgit_repo~refresh_local_objects.
  ENDMETHOD.

  METHOD zif_abapgit_repo~remove_ignored_files.
  ENDMETHOD.

  METHOD zif_abapgit_repo_online~select_branch.
  ENDMETHOD.

  METHOD zif_abapgit_repo_online~select_commit.
  ENDMETHOD.

  METHOD zif_abapgit_repo~set_dot_abapgit.
  ENDMETHOD.

  METHOD zif_abapgit_repo~set_files_remote.
  ENDMETHOD.

  METHOD zif_abapgit_repo~set_local_settings.
  ENDMETHOD.

  METHOD zif_abapgit_repo_online~set_url.
  ENDMETHOD.

  METHOD zif_abapgit_repo_online~switch_origin.
  ENDMETHOD.

  METHOD zif_abapgit_repo~switch_repo_type.
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_validate_form IMPLEMENTATION.

  METHOD setup.

    DATA: ls_data TYPE zif_abapgit_persistence=>ty_repo.
    DATA: li_repo_online TYPE REF TO zif_abapgit_repo_online.

    CREATE OBJECT mo_git_transport_mock TYPE ltd_git_transport.
    zcl_abapgit_git_injector=>set_git_transport( mo_git_transport_mock ).

    " Disable GUI
    mo_frontend_services_mock = zcl_abapgit_ui_factory=>get_frontend_services( abap_true ).

    ls_data-key = 1.
    ls_data-branch_name = 'main'.

    CREATE OBJECT li_repo_online TYPE ltd_repo_online.

    CREATE OBJECT mo_given_form_data.
    mo_given_form_data->set(
        iv_key = zcl_abapgit_gui_page_sett_remo=>c_id-branch
        iv_val = 'main' ).

    CREATE OBJECT mo_cut EXPORTING ii_repo = li_repo_online.

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
