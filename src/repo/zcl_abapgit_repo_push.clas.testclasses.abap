CLASS ltd_calls DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    " the calls of the doubles in order, separated by commas
    CLASS-DATA gv_calls TYPE string.
    CLASS-METHODS add
      IMPORTING
        iv_call TYPE string.

ENDCLASS.

CLASS ltd_calls IMPLEMENTATION.

  METHOD add.
    IF gv_calls IS NOT INITIAL.
      gv_calls = gv_calls && `,`.
    ENDIF.
    gv_calls = gv_calls && iv_call.
  ENDMETHOD.

ENDCLASS.

CLASS ltd_exit DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_exit.

    DATA mv_fail_before TYPE abap_bool.
    DATA mv_fail_after TYPE abap_bool.
    DATA ms_comment TYPE zif_abapgit_git_definitions=>ty_comment.

ENDCLASS.

CLASS ltd_exit IMPLEMENTATION.

  METHOD zif_abapgit_exit~validate_before_push.
    ltd_calls=>add( `before` ).
    ms_comment = is_comment.
    IF mv_fail_before = abap_true.
      zcx_abapgit_exception=>raise( 'rejected by exit' ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_exit~validate_after_push.
    ltd_calls=>add( `after` ).
    IF mv_fail_after = abap_true.
      zcx_abapgit_exception=>raise( 'rejected after push' ).
    ENDIF.
  ENDMETHOD.

  METHOD zif_abapgit_exit~adjust_commit_message.
  ENDMETHOD.
  METHOD zif_abapgit_exit~adjust_display_commit_url.
  ENDMETHOD.
  METHOD zif_abapgit_exit~adjust_display_filename.
  ENDMETHOD.
  METHOD zif_abapgit_exit~allow_sap_objects.
  ENDMETHOD.
  METHOD zif_abapgit_exit~change_committer_info.
  ENDMETHOD.
  METHOD zif_abapgit_exit~change_password_popup_username.
  ENDMETHOD.
  METHOD zif_abapgit_exit~change_local_host.
  ENDMETHOD.
  METHOD zif_abapgit_exit~change_max_parallel_processes.
  ENDMETHOD.
  METHOD zif_abapgit_exit~change_proxy_authentication.
  ENDMETHOD.
  METHOD zif_abapgit_exit~change_proxy_port.
  ENDMETHOD.
  METHOD zif_abapgit_exit~change_proxy_url.
  ENDMETHOD.
  METHOD zif_abapgit_exit~change_rfc_server_group.
  ENDMETHOD.
  METHOD zif_abapgit_exit~change_supported_data_objects.
  ENDMETHOD.
  METHOD zif_abapgit_exit~change_supported_object_types.
  ENDMETHOD.
  METHOD zif_abapgit_exit~change_tadir.
  ENDMETHOD.
  METHOD zif_abapgit_exit~create_http_client.
  ENDMETHOD.
  METHOD zif_abapgit_exit~custom_serialize_abap_clif.
  ENDMETHOD.
  METHOD zif_abapgit_exit~deserialize_postprocess.
  ENDMETHOD.
  METHOD zif_abapgit_exit~determine_transport_request.
  ENDMETHOD.
  METHOD zif_abapgit_exit~enable_adjust_commit_message.
  ENDMETHOD.
  METHOD zif_abapgit_exit~enhance_any_toolbar.
  ENDMETHOD.
  METHOD zif_abapgit_exit~enhance_repo_toolbar.
  ENDMETHOD.
  METHOD zif_abapgit_exit~get_ci_tests.
  ENDMETHOD.
  METHOD zif_abapgit_exit~get_ssl_id.
  ENDMETHOD.
  METHOD zif_abapgit_exit~http_client.
  ENDMETHOD.
  METHOD zif_abapgit_exit~on_event.
  ENDMETHOD.
  METHOD zif_abapgit_exit~pre_calculate_repo_status.
  ENDMETHOD.
  METHOD zif_abapgit_exit~serialize_postprocess.
  ENDMETHOD.
  METHOD zif_abapgit_exit~wall_message_list.
  ENDMETHOD.
  METHOD zif_abapgit_exit~wall_message_repo.
  ENDMETHOD.

ENDCLASS.

CLASS ltd_repo_online DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo.
    INTERFACES zif_abapgit_repo_online.

    DATA ms_comment TYPE zif_abapgit_git_definitions=>ty_comment.
    DATA mo_stage TYPE REF TO zcl_abapgit_stage.
    DATA mv_fail_push TYPE abap_bool.

ENDCLASS.

CLASS ltd_repo_online IMPLEMENTATION.

  METHOD zif_abapgit_repo_online~push.
    ltd_calls=>add( `push` ).
    IF mv_fail_push = abap_true.
      zcx_abapgit_exception=>raise( 'push failed' ).
    ENDIF.
    ms_comment = is_comment.
    mo_stage = io_stage.
  ENDMETHOD.

  METHOD zif_abapgit_repo_online~get_url.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~get_selected_branch.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~set_url.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~select_branch.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~get_selected_commit.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~get_current_remote.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~select_commit.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~switch_origin.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~get_switched_origin.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~create_branch.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~check_for_valid_branch.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~get_remote_settings.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_key.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_name.
  ENDMETHOD.
  METHOD zif_abapgit_repo~is_offline.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_package.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_local_settings.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_tadir_objects.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_local_filtered.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_local.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_remote.
  ENDMETHOD.
  METHOD zif_abapgit_repo~refresh.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_dot_abapgit.
  ENDMETHOD.
  METHOD zif_abapgit_repo~set_dot_abapgit.
  ENDMETHOD.
  METHOD zif_abapgit_repo~find_remote_dot_abapgit.
  ENDMETHOD.
  METHOD zif_abapgit_repo~deserialize.
  ENDMETHOD.
  METHOD zif_abapgit_repo~deserialize_checks.
  ENDMETHOD.
  METHOD zif_abapgit_repo~checksums.
  ENDMETHOD.
  METHOD zif_abapgit_repo~has_remote_source.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_log.
  ENDMETHOD.
  METHOD zif_abapgit_repo~create_new_log.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_dot_apack.
  ENDMETHOD.
  METHOD zif_abapgit_repo~delete_checks.
  ENDMETHOD.
  METHOD zif_abapgit_repo~set_files_remote.
  ENDMETHOD.
  METHOD zif_abapgit_repo~set_local_settings.
  ENDMETHOD.
  METHOD zif_abapgit_repo~switch_repo_type.
  ENDMETHOD.
  METHOD zif_abapgit_repo~refresh_local_object.
  ENDMETHOD.
  METHOD zif_abapgit_repo~refresh_local_objects.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_data_config.
  ENDMETHOD.
  METHOD zif_abapgit_repo~bind_listener.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_push DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_exit TYPE REF TO ltd_exit.
    DATA mo_repo TYPE REF TO ltd_repo_online.
    DATA mi_cut TYPE REF TO zif_abapgit_repo_push.
    DATA mo_stage TYPE REF TO zcl_abapgit_stage.
    DATA ms_comment TYPE zif_abapgit_git_definitions=>ty_comment.

    METHODS setup.
    METHODS teardown.
    METHODS exits_around_push FOR TESTING RAISING zcx_abapgit_exception.
    METHODS passes_comment_and_stage FOR TESTING RAISING zcx_abapgit_exception.
    METHODS exit_before_stops_push FOR TESTING.
    METHODS push_failure_skips_after FOR TESTING.
    METHODS exit_after_failure_propagates FOR TESTING.
    METHODS factory_binds_repository FOR TESTING RAISING zcx_abapgit_exception.
    METHODS factory_uses_injected_push FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS zcl_abapgit_repo_push DEFINITION LOCAL FRIENDS ltcl_push.

CLASS ltcl_push IMPLEMENTATION.

  METHOD setup.

    CLEAR ltd_calls=>gv_calls.

    CREATE OBJECT mo_exit.
    zcl_abapgit_injector=>set_exit( mo_exit ).

    CREATE OBJECT mo_repo.
    CREATE OBJECT mi_cut TYPE zcl_abapgit_repo_push
      EXPORTING
        ii_repo_online = mo_repo.

    CREATE OBJECT mo_stage.
    ms_comment-committer-name  = 'Tester'.
    ms_comment-committer-email = 'tester@localhost'.
    ms_comment-comment         = 'Test commit'.

  ENDMETHOD.

  METHOD teardown.

    DATA li_no_exit TYPE REF TO zif_abapgit_exit.
    DATA li_no_push TYPE REF TO zif_abapgit_repo_push.

    zcl_abapgit_injector=>set_exit( li_no_exit ).
    zcl_abapgit_injector=>set_repo_push( li_no_push ).

  ENDMETHOD.

  METHOD exits_around_push.

    mi_cut->push( is_comment = ms_comment
                  io_stage   = mo_stage ).

    cl_abap_unit_assert=>assert_equals(
      act = ltd_calls=>gv_calls
      exp = `before,push,after` ).

  ENDMETHOD.

  METHOD passes_comment_and_stage.

    DATA lv_same TYPE abap_bool.

    mi_cut->push( is_comment = ms_comment
                  io_stage   = mo_stage ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_exit->ms_comment
      exp = ms_comment ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->ms_comment
      exp = ms_comment ).
    lv_same = boolc( mo_repo->mo_stage = mo_stage ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_same
      exp = abap_true ).

  ENDMETHOD.

  METHOD exit_before_stops_push.

    mo_exit->mv_fail_before = abap_true.

    TRY.
        mi_cut->push( is_comment = ms_comment
                      io_stage   = mo_stage ).
        cl_abap_unit_assert=>fail( 'exception expected' ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

    cl_abap_unit_assert=>assert_equals(
      act = ltd_calls=>gv_calls
      exp = `before` ).

  ENDMETHOD.

  METHOD push_failure_skips_after.

    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    mo_repo->mv_fail_push = abap_true.
    TRY.
        mi_cut->push( is_comment = ms_comment
                      io_stage   = mo_stage ).
        cl_abap_unit_assert=>fail( 'exception expected' ).
      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          act = lx_error->get_text( )
          exp = 'push failed' ).
    ENDTRY.
    cl_abap_unit_assert=>assert_equals(
      act = ltd_calls=>gv_calls
      exp = `before,push` ).

  ENDMETHOD.

  METHOD exit_after_failure_propagates.

    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    mo_exit->mv_fail_after = abap_true.
    TRY.
        mi_cut->push( is_comment = ms_comment
                      io_stage   = mo_stage ).
        cl_abap_unit_assert=>fail( 'exception expected' ).
      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          act = lx_error->get_text( )
          exp = 'rejected after push' ).
    ENDTRY.
    cl_abap_unit_assert=>assert_equals(
      act = ltd_calls=>gv_calls
      exp = `before,push,after` ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->ms_comment
      exp = ms_comment ).

  ENDMETHOD.

  METHOD factory_binds_repository.

    DATA lo_other TYPE REF TO ltd_repo_online.
    DATA li_first TYPE REF TO zif_abapgit_repo_push.
    DATA li_second TYPE REF TO zif_abapgit_repo_push.

    CREATE OBJECT lo_other.
    li_first = zcl_abapgit_factory=>get_repo_push( mo_repo ).
    li_second = zcl_abapgit_factory=>get_repo_push( lo_other ).
    li_first->push( is_comment = ms_comment
                    io_stage   = mo_stage ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->ms_comment
      exp = ms_comment ).
    cl_abap_unit_assert=>assert_initial( lo_other->ms_comment ).
    ms_comment-comment = 'Second repository'.
    li_second->push( is_comment = ms_comment
                     io_stage   = mo_stage ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_other->ms_comment
      exp = ms_comment ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->ms_comment-comment
      exp = 'Test commit' ).

  ENDMETHOD.

  METHOD factory_uses_injected_push.

    DATA lo_other TYPE REF TO ltd_repo_online.
    DATA li_push TYPE REF TO zif_abapgit_repo_push.
    DATA lv_same TYPE abap_bool.

    CREATE OBJECT lo_other.
    zcl_abapgit_injector=>set_repo_push( mi_cut ).
    li_push = zcl_abapgit_factory=>get_repo_push( lo_other ).
    lv_same = boolc( li_push = mi_cut ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_same
      exp = abap_true ).
    li_push->push( is_comment = ms_comment
                   io_stage   = mo_stage ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->ms_comment
      exp = ms_comment ).
    cl_abap_unit_assert=>assert_initial( lo_other->ms_comment ).

  ENDMETHOD.

ENDCLASS.
