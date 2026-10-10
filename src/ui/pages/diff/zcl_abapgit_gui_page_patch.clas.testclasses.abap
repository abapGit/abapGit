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


CLASS lcl_continue_repo DEFINITION FINAL INHERITING FROM zcl_abapgit_repo.
  PUBLIC SECTION.
    DATA mv_pushes TYPE i.
    INTERFACES zif_abapgit_repo_online.
    METHODS zif_abapgit_repo~get_files_local REDEFINITION.
    METHODS zif_abapgit_repo~get_files_remote REDEFINITION.
    METHODS zif_abapgit_repo~find_remote_dot_abapgit REDEFINITION.
ENDCLASS.

CLASS lcl_continue_user DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_persist_user.
ENDCLASS.

CLASS lcl_continue_repo_srv DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_repo_srv.
    DATA mi_repo TYPE REF TO zif_abapgit_repo.
ENDCLASS.

CLASS lcl_continue_patch DEFINITION FINAL INHERITING FROM zcl_abapgit_gui_page_patch.
  PUBLIC SECTION.
    DATA mv_refreshes TYPE i.
    DATA mv_fail_refresh TYPE abap_bool.
    METHODS get_diffs
      RETURNING
        VALUE(rt_diffs) TYPE zif_abapgit_gui_diff=>ty_file_diffs.
  PROTECTED SECTION.
    METHODS get_files_and_status REDEFINITION.
    METHODS refresh_full REDEFINITION.
ENDCLASS.

CLASS ltcl_continue_patching DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mi_previous_srv TYPE REF TO zif_abapgit_repo_srv.
    DATA mi_previous_user TYPE REF TO zif_abapgit_persist_user.
    DATA mo_repo TYPE REF TO lcl_continue_repo.
    DATA mo_patch TYPE REF TO zcl_abapgit_gui_page_patch.
    DATA mo_fixture TYPE REF TO lcl_continue_patch.
    METHODS setup RAISING cx_static_check.
    METHODS teardown.
    METHODS successive_patches FOR TESTING RAISING cx_static_check.
    METHODS invalid_commit_keeps_patch FOR TESTING RAISING cx_static_check.
    METHODS reject_continue_without_patch FOR TESTING RAISING cx_static_check.
    METHODS commit_with_remaining_changes FOR TESTING RAISING cx_static_check.
    METHODS commit_without_remaining FOR TESTING RAISING cx_static_check.
    METHODS commit_refresh_failure FOR TESTING RAISING cx_static_check.
    METHODS commit_patch
      RETURNING VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING cx_static_check.
ENDCLASS.


CLASS lcl_continue_repo_srv IMPLEMENTATION.
  METHOD zif_abapgit_repo_srv~get.
    ri_repo = mi_repo.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~init.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~delete.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~is_repo_installed.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~list.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~list_favorites.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~new_offline.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~new_online.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~purge.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~validate_package.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~validate_url.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~get_repo_from_package.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~get_repo_from_url.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~get_label_list.
  ENDMETHOD.
  METHOD zif_abapgit_repo_srv~reload.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_continue_repo IMPLEMENTATION.
  METHOD zif_abapgit_repo_online~get_url.
    rv_url = ms_data-url.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~get_selected_branch.
    rv_name = ms_data-branch_name.
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
  METHOD zif_abapgit_repo_online~push.
    mv_pushes = mv_pushes + 1.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~create_branch.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~check_for_valid_branch.
  ENDMETHOD.
  METHOD zif_abapgit_repo_online~get_remote_settings.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_local.
    DATA ls_file LIKE LINE OF rt_files.
    ls_file-file-path = '/'.
    ls_file-file-filename = 'test.abap'.
    ls_file-file-data = zcl_abapgit_convert=>string_to_xstring_utf8(
      |unchanged\nfirst new\nsecond new\n| ).
    ls_file-file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-file-data ).
    APPEND ls_file TO rt_files.
  ENDMETHOD.
  METHOD zif_abapgit_repo~get_files_remote.
    DATA ls_file LIKE LINE OF rt_files.
    ls_file-path = '/'.
    ls_file-filename = 'test.abap'.
    ls_file-data = zcl_abapgit_convert=>string_to_xstring_utf8(
      |unchanged\nfirst old\nsecond old\n| ).
    ls_file-sha1 = zcl_abapgit_hash=>sha1_blob( ls_file-data ).
    INSERT ls_file INTO TABLE rt_files.
  ENDMETHOD.
  METHOD zif_abapgit_repo~find_remote_dot_abapgit.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_continue_user IMPLEMENTATION.
  METHOD zif_abapgit_persist_user~get_changes_only.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_default_git_user_email.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_default_git_user_name.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_diff_unified.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_favorites.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_hide_files.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_show_folders.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_repo_git_user_email.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_repo_git_user_name.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_repo_last_change_seen.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_repo_login.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_repo_show.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~is_favorite_repo.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~set_default_git_user_email.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~set_default_git_user_name.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~set_repo_git_user_email.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~set_repo_git_user_name.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~set_repo_last_change_seen.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~set_repo_login.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~set_repo_show.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~toggle_changes_only.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_order_by.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~set_order_by.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_order_descending.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~set_order_descending.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_diff_first.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~set_diff_first.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~toggle_diff_unified.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~toggle_favorite.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~toggle_hide_files.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~toggle_show_folders.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_settings.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~set_settings.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_list_settings.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~set_list_settings.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~get_flow_settings.
  ENDMETHOD.
  METHOD zif_abapgit_persist_user~set_flow_settings.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_continue_patch IMPLEMENTATION.
  METHOD refresh_full.
    " Simulate a successful push changing the remote, without network access
    mv_refreshes = mv_refreshes + 1.
    IF mv_fail_refresh = abap_true.
      zcx_abapgit_exception=>raise( 'Test refresh failure' ).
    ENDIF.
  ENDMETHOD.

  METHOD get_diffs.
    rt_diffs = mt_diff_files.
  ENDMETHOD.

  METHOD get_files_and_status.
    DATA ls_local LIKE LINE OF et_local.
    DATA ls_remote LIKE LINE OF et_remote.
    DATA ls_status LIKE LINE OF et_status.
    DATA lv_remote TYPE string.
    DATA lv_local TYPE string.

    CLEAR: et_local, et_remote, et_status.
    lv_local = |unchanged\nfirst new\nsecond new\n|.
    CASE mv_refreshes.
      WHEN 0.
        lv_remote = |unchanged\nfirst old\nsecond old\n|.
      WHEN 1.
        lv_remote = |unchanged\nfirst new\nsecond old\n|.
      WHEN OTHERS.
        lv_remote = lv_local.
    ENDCASE.
    ls_local-file-path = '/'.
    ls_local-file-filename = 'test.abap'.
    ls_local-file-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_local ).
    APPEND ls_local TO et_local.
    ls_remote-path = '/'.
    ls_remote-filename = 'test.abap'.
    ls_remote-data = zcl_abapgit_convert=>string_to_xstring_utf8( lv_remote ).
    INSERT ls_remote INTO TABLE et_remote.
    ls_status-path = '/'.
    ls_status-filename = 'test.abap'.
    IF lv_local = lv_remote.
      ls_status-match = abap_true.
    ELSE.
      ls_status-lstate = zif_abapgit_definitions=>c_state-modified.
    ENDIF.
    APPEND ls_status TO et_status.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_continue_patching IMPLEMENTATION.
  METHOD setup.
    DATA lo_srv TYPE REF TO lcl_continue_repo_srv.
    DATA ls_repo TYPE zif_abapgit_persistence=>ty_repo.
    DATA ls_file TYPE zif_abapgit_git_definitions=>ty_file.
    DATA lo_user TYPE REF TO lcl_continue_user.

    mi_previous_srv = zcl_abapgit_repo_srv=>get_instance( ).
    mi_previous_user = zcl_abapgit_persist_factory=>get_user( ).
    CREATE OBJECT lo_user.
    zcl_abapgit_persist_injector=>set_current_user( lo_user ).
    CREATE OBJECT lo_srv.
    ls_repo-key = '1'.
    ls_repo-url = 'https://github.com/abapGit/abapGit.git'.
    ls_repo-branch_name = 'refs/heads/main'.
    ls_repo-package = '$TMP'.
    ls_repo-dot_abapgit = zcl_abapgit_dot_abapgit=>build_default( )->get_data( ).
    ls_repo-dot_abapgit-folder_logic = zif_abapgit_dot_abapgit=>c_folder_logic-full.
    CREATE OBJECT lo_srv->mi_repo TYPE lcl_continue_repo
      EXPORTING
        is_data = ls_repo.
    mo_repo ?= lo_srv->mi_repo.
    zcl_abapgit_repo_srv=>inject_instance( lo_srv ).
    ls_file-path = '/'.
    ls_file-filename = 'test.abap'.
    CREATE OBJECT mo_fixture
      EXPORTING
        iv_key = ls_repo-key
        is_file = ls_file.
    mo_patch = mo_fixture.
  ENDMETHOD.

  METHOD teardown.
    zcl_abapgit_repo_srv=>inject_instance( mi_previous_srv ).
    zcl_abapgit_persist_injector=>set_current_user( mi_previous_user ).
  ENDMETHOD.

  METHOD commit_patch.
    DATA lo_page TYPE REF TO zcl_abapgit_gui_page_hoc.
    DATA lo_commit TYPE REF TO zcl_abapgit_gui_page_commit.
    DATA lo_stage TYPE REF TO zcl_abapgit_stage.
    DATA lt_postdata TYPE zif_abapgit_html_viewer=>ty_post_data.

    CREATE OBJECT lo_stage.
    lo_page ?= zcl_abapgit_gui_page_commit=>create(
      ii_repo_online = mo_repo
      io_stage = lo_stage
      io_patch = mo_patch ).
    lo_commit ?= lo_page->get_child( ).
    APPEND 'comment=Test&committer_name=Committer&committer_email=test@example.org' TO lt_postdata.
    rs_handled = lo_commit->zif_abapgit_gui_event_handler~on_event(
      zcl_abapgit_gui_event=>new( iv_action = 'commit_patch'
        it_postdata = lt_postdata ) ).
  ENDMETHOD.

  METHOD commit_with_remaining_changes.
    DATA ls_handled TYPE zif_abapgit_gui_event_handler=>ty_handling_result.
    ls_handled = commit_patch( ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_handled-state
      exp = zcl_abapgit_gui=>c_event_state-go_back ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->mv_pushes
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_fixture->mv_refreshes
      exp = 1 ).
  ENDMETHOD.

  METHOD commit_without_remaining.
    DATA ls_handled TYPE zif_abapgit_gui_event_handler=>ty_handling_result.
    mo_fixture->mv_refreshes = 1.
    ls_handled = commit_patch( ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_handled-state
      exp = zcl_abapgit_gui=>c_event_state-go_back_to_bookmark ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->mv_pushes
      exp = 1 ).
    cl_abap_unit_assert=>assert_initial( mo_fixture->get_diffs( ) ).
  ENDMETHOD.

  METHOD commit_refresh_failure.
    DATA ls_handled TYPE zif_abapgit_gui_event_handler=>ty_handling_result.
    DATA lv_message TYPE string.
    mo_fixture->mv_fail_refresh = abap_true.
    ls_handled = commit_patch( ).
    CONCATENATE sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_message.
    cl_abap_unit_assert=>assert_equals(
      act = boolc( lv_message CS 'Commit was successful, but continuing patching failed' )
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_handled-state
      exp = zcl_abapgit_gui=>c_event_state-go_back_to_bookmark ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_repo->mv_pushes
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_fixture->mv_refreshes
      exp = 1 ).
  ENDMETHOD.

  METHOD successive_patches.
    DATA lt_files TYPE zif_abapgit_gui_diff=>ty_file_diffs.
    DATA ls_file LIKE LINE OF lt_files.
    DATA lt_diff TYPE zif_abapgit_definitions=>ty_diffs_tt.
    DATA ls_line LIKE LINE OF lt_diff.
    DATA ls_commit TYPE zif_abapgit_services_git=>ty_commit_fields.
    DATA lv_updates TYPE i.

    lt_files = mo_fixture->get_diffs( ).
    READ TABLE lt_files INTO ls_file INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).
    ls_file-o_diff->set_patch_new(
      iv_line_new = 2
      iv_patch_flag = abap_true ).
    ls_commit-committer_name = 'Committer'.
    ls_commit-committer_email = 'committer@example.org'.
    ls_commit-author_name = 'Author'.
    ls_commit-author_email = 'author@example.org'.
    ls_commit-comment = 'First patch'.
    ls_commit-body = 'First description'.

    cl_abap_unit_assert=>assert_equals(
      act = mo_patch->continue_patching( ls_commit )
      exp = abap_true ).
    lt_files = mo_fixture->get_diffs( ).
    READ TABLE lt_files INTO ls_file INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).
    lt_diff = ls_file-o_diff->get( ).
    LOOP AT lt_diff INTO ls_line.
      cl_abap_unit_assert=>assert_initial( ls_line-patch_flag ).
      IF ls_line-result = zif_abapgit_definitions=>c_diff-update.
        lv_updates = lv_updates + 1.
        cl_abap_unit_assert=>assert_equals(
          act = ls_line-old
          exp = 'second old' ).
        cl_abap_unit_assert=>assert_equals(
          act = ls_line-new
          exp = 'second new' ).
      ENDIF.
    ENDLOOP.
    cl_abap_unit_assert=>assert_equals(
      act = lv_updates
      exp = 1 ).

    " Completing the single-file scope must return to the repository
    cl_abap_unit_assert=>assert_equals(
      act = mo_patch->continue_patching( ls_commit )
      exp = abap_false ).
    lt_files = mo_fixture->get_diffs( ).
    cl_abap_unit_assert=>assert_initial( lt_files ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_fixture->mv_refreshes
      exp = 2 ).
  ENDMETHOD.

  METHOD invalid_commit_keeps_patch.
    DATA lo_commit TYPE REF TO zcl_abapgit_gui_page_commit.
    DATA lo_page TYPE REF TO zcl_abapgit_gui_page_hoc.
    DATA li_repo TYPE REF TO zif_abapgit_repo_online.
    DATA lo_stage TYPE REF TO zcl_abapgit_stage.
    DATA ls_handled TYPE zif_abapgit_gui_event_handler=>ty_handling_result.

    li_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( '1' ).
    CREATE OBJECT lo_stage.
    lo_page ?= zcl_abapgit_gui_page_commit=>create(
        ii_repo_online = li_repo
        io_stage       = lo_stage
        io_patch       = mo_patch
        iv_sci_result  = zif_abapgit_definitions=>c_sci_result-no_run ).
    lo_commit ?= lo_page->get_child( ).
    " Missing required fields: no push or patch reset may happen
    ls_handled = lo_commit->zif_abapgit_gui_event_handler~on_event(
      zcl_abapgit_gui_event=>new( iv_action = 'commit_patch' ) ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_handled-state
      exp = zcl_abapgit_gui=>c_event_state-re_render ).
    cl_abap_unit_assert=>assert_initial( mo_fixture->mv_refreshes ).
  ENDMETHOD.

  METHOD reject_continue_without_patch.
    DATA lo_commit TYPE REF TO zcl_abapgit_gui_page_commit.
    DATA lo_page TYPE REF TO zcl_abapgit_gui_page_hoc.
    DATA li_repo TYPE REF TO zif_abapgit_repo_online.
    DATA lo_stage TYPE REF TO zcl_abapgit_stage.
    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    li_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( '1' ).
    CREATE OBJECT lo_stage.
    lo_page ?= zcl_abapgit_gui_page_commit=>create(
        ii_repo_online = li_repo
        io_stage       = lo_stage
        iv_sci_result  = zif_abapgit_definitions=>c_sci_result-no_run ).
    lo_commit ?= lo_page->get_child( ).
    TRY.
        lo_commit->zif_abapgit_gui_event_handler~on_event(
          zcl_abapgit_gui_event=>new( iv_action = 'commit_patch' ) ).
        cl_abap_unit_assert=>fail( 'A normal commit must not accept the patch continuation action' ).
      CATCH zcx_abapgit_exception INTO lx_error.
        cl_abap_unit_assert=>assert_equals(
          act = lx_error->get_text( )
          exp = 'Continue patching is only available when committing a patch' ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
