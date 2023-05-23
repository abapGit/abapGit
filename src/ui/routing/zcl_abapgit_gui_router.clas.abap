CLASS zcl_abapgit_gui_router DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS general_page_routing
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception .
    METHODS abapgit_services_actions
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception .
    METHODS db_actions
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS file_download
      IMPORTING
        !iv_package TYPE devclass
        !iv_xstr    TYPE xstring
      RAISING
        zcx_abapgit_exception .
    METHODS git_services
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception .
    METHODS sap_gui_actions
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception .
    METHODS other_utilities
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception .
    METHODS zip_services
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception .
    METHODS repository_services
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception .
    METHODS get_page_diff
      IMPORTING
        !ii_event      TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS get_page_stage
      IMPORTING
        !ii_event      TYPE REF TO zif_abapgit_gui_event
        ii_obj_filter  TYPE REF TO zif_abapgit_object_filter OPTIONAL
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS jump_object
      IMPORTING
        !iv_obj_type TYPE string
        !iv_obj_name TYPE string
        !iv_filename TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS jump_display_transport
      IMPORTING
        !iv_transport TYPE trkorr
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS jump_display_user
      IMPORTING
        !iv_username TYPE syuname
      RAISING
        zcx_abapgit_exception .
    METHODS call_browser
      IMPORTING
        !iv_url TYPE csequence
      RAISING
        zcx_abapgit_exception .
    METHODS get_state_settings
      IMPORTING
        !ii_event       TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rv_state) TYPE i .
    METHODS get_state_diff
      IMPORTING
        !ii_event       TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rv_state) TYPE i .
    METHODS main_page
      RETURNING VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING   zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_gui_router IMPLEMENTATION.


  METHOD abapgit_services_actions.

    IF ii_event->mv_action = zif_abapgit_definitions=>c_action-abapgit_home.
      rs_handled-page  = zcl_abapgit_gui_page_repo_over=>create( ).
      rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
    ENDIF.

  ENDMETHOD.


  METHOD call_browser.

    zcl_abapgit_ui_factory=>get_frontend_services( )->execute( iv_document = |{ iv_url }| ).

  ENDMETHOD.


  METHOD db_actions.

    DATA ls_db_key TYPE zif_abapgit_persistence=>ty_content.
    DATA lo_query TYPE REF TO zcl_abapgit_string_map.

    lo_query = ii_event->query( ).
    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-db_edit.
        lo_query->to_abap( CHANGING cs_container = ls_db_key ).
        rs_handled-page  = zcl_abapgit_gui_page_db_entry=>create(
          is_key       = ls_db_key
          iv_edit_mode = abap_true ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-db_display.
        lo_query->to_abap( CHANGING cs_container = ls_db_key ).
        rs_handled-page  = zcl_abapgit_gui_page_db_entry=>create( ls_db_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
    ENDCASE.

  ENDMETHOD.


  METHOD file_download.

    DATA:
      lv_path    TYPE string,
      lv_default TYPE string,
      li_fe_serv TYPE REF TO zif_abapgit_frontend_services,
      lv_package TYPE devclass.

    lv_package = iv_package.
    TRANSLATE lv_package USING '/#'.
    CONCATENATE lv_package '_' sy-datlo '_' sy-timlo INTO lv_default.

    li_fe_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).

    lv_path = li_fe_serv->show_file_save_dialog(
      iv_title            = 'Export ZIP'
      iv_extension        = 'zip'
      iv_default_filename = lv_default ).

    li_fe_serv->file_download(
      iv_path = lv_path
      iv_xstr = iv_xstr ).

  ENDMETHOD.


  METHOD general_page_routing.

    DATA: lv_key              TYPE zif_abapgit_persistence=>ty_repo-key,
          lv_last_repo_key    TYPE zif_abapgit_persistence=>ty_repo-key,
          lo_obj_filter_trans TYPE REF TO zcl_abapgit_object_filter_tran,
          lo_repo             TYPE REF TO zcl_abapgit_repo,
          lt_r_trkorr         TYPE zif_abapgit_definitions=>ty_trrngtrkor_tt.

    lv_key = ii_event->query( )->get( 'KEY' ).

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-go_home.
        lv_last_repo_key = zcl_abapgit_persistence_user=>get_instance( )->get_repo_show( ).

        IF lv_last_repo_key IS NOT INITIAL.
          rs_handled-page  = zcl_abapgit_gui_page_repo_view=>create( lv_last_repo_key ).
          rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
        ELSE.
          rs_handled-page = main_page( ).
          rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
        ENDIF.
      WHEN zif_abapgit_definitions=>c_action-go_db.                          " Go DB util page
        rs_handled-page  = zcl_abapgit_gui_page_db=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-go_debuginfo.                   " Go debug info
        rs_handled-page  = zcl_abapgit_gui_page_debuginfo=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-go_settings.                    " Go global settings
        rs_handled-page  = zcl_abapgit_gui_page_sett_glob=>create( ).
        rs_handled-state = get_state_settings( ii_event ).
      WHEN zif_abapgit_definitions=>c_action-go_settings_personal.           " Go personal settings
        rs_handled-page  = zcl_abapgit_gui_page_sett_pers=>create( ).
        rs_handled-state = get_state_settings( ii_event ).
      WHEN zif_abapgit_definitions=>c_action-go_background_run.              " Go background run page
        rs_handled-page  = zcl_abapgit_gui_page_run_bckg=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-go_repo_diff                         " Go Diff page
        OR zif_abapgit_definitions=>c_action-go_file_diff.
        rs_handled-page  = get_page_diff( ii_event ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page_w_bookmark.
      WHEN zif_abapgit_definitions=>c_action-go_stage.                        " Go Staging page
        rs_handled-page  = get_page_stage( ii_event ).
        rs_handled-state = get_state_diff( ii_event ).
      WHEN zif_abapgit_definitions=>c_action-go_stage_transport.              " Go Staging page by Transport

        lt_r_trkorr = zcl_abapgit_ui_factory=>get_popups( )->popup_select_wb_tc_tr_and_tsk( ).

        lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).

        CREATE OBJECT lo_obj_filter_trans.
        lo_obj_filter_trans->set_filter_values( iv_package  = lo_repo->get_package( )
                                                it_r_trkorr = lt_r_trkorr ).

        rs_handled-page = get_page_stage( ii_event      = ii_event
                                          ii_obj_filter = lo_obj_filter_trans ).
        rs_handled-state = get_state_diff( ii_event ).
      WHEN zif_abapgit_definitions=>c_action-go_tutorial.                     " Go to tutorial
        rs_handled-page  = zcl_abapgit_gui_page_tutorial=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-documentation.                   " abapGit docs
        zcl_abapgit_services_abapgit=>open_abapgit_wikipage( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-go_explore.                      " dotabap
        zcl_abapgit_services_abapgit=>open_dotabap_homepage( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-changelog.                       " abapGit full changelog
        zcl_abapgit_services_abapgit=>open_abapgit_changelog( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-homepage.                        " abapGit homepage
        zcl_abapgit_services_abapgit=>open_abapgit_homepage( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-sponsor.                         " abapGit sponsor us
        zcl_abapgit_services_abapgit=>open_abapgit_homepage( 'sponsor.html' ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-show_hotkeys.                    " show hotkeys
        zcl_abapgit_ui_factory=>get_gui_services(
                             )->get_hotkeys_ctl(
                             )->set_visible( abap_true ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD get_page_diff.

    DATA: ls_file   TYPE zif_abapgit_git_definitions=>ty_file,
          ls_object TYPE zif_abapgit_definitions=>ty_item,
          lo_page   TYPE REF TO zcl_abapgit_gui_page_diff,
          lv_key    TYPE zif_abapgit_persistence=>ty_repo-key.

    lv_key             = ii_event->query( )->get( 'KEY' ).
    ls_file-path       = ii_event->query( )->get( 'PATH' ).
    ls_file-filename   = ii_event->query( )->get( 'FILENAME' ). " unescape ?
    ls_object-obj_type = ii_event->query( )->get( 'OBJ_TYPE' ).
    ls_object-obj_name = ii_event->query( )->get( 'OBJ_NAME' ). " unescape ?

    CREATE OBJECT lo_page
      EXPORTING
        iv_key    = lv_key
        is_file   = ls_file
        is_object = ls_object.

    ri_page = lo_page.

  ENDMETHOD.


  METHOD get_page_stage.

    DATA: lo_repo                     TYPE REF TO zcl_abapgit_repo_online,
          lv_key                      TYPE zif_abapgit_persistence=>ty_repo-key,
          lv_seed                     TYPE string,
          lo_stage_page               TYPE REF TO zcl_abapgit_gui_page_stage,
          lo_code_inspector_page      TYPE REF TO zcl_abapgit_gui_page_code_insp,
          lv_answer                   TYPE c LENGTH 1,
          lv_question_text            TYPE string,
          lv_question_title           TYPE string,
          lv_show_create_branch_popup TYPE c LENGTH 1,
          lx_error                    TYPE REF TO cx_sy_move_cast_error.

    lv_key   = ii_event->query( )->get( 'KEY' ).
    lv_seed  = ii_event->query( )->get( 'SEED' ).
    TRY.
        lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
      CATCH cx_sy_move_cast_error INTO lx_error.
        zcx_abapgit_exception=>raise( `Staging is only possible for online repositories.` ).
    ENDTRY.

    IF lo_repo->get_local_settings( )-code_inspector_check_variant IS NOT INITIAL.

      CREATE OBJECT lo_code_inspector_page
        EXPORTING
          io_repo = lo_repo.

      IF lo_code_inspector_page->is_nothing_to_display( ) = abap_true.
        " force refresh on stage, to make sure the latest local and remote files are used
        lo_repo->refresh( ).
        CREATE OBJECT lo_stage_page
          EXPORTING
            io_repo       = lo_repo
            iv_seed       = lv_seed
            iv_sci_result = zif_abapgit_definitions=>c_sci_result-passed
            ii_obj_filter = ii_obj_filter.

        ri_page = lo_stage_page.
      ELSE.
        ri_page = lo_code_inspector_page.
      ENDIF.

    ELSEIF lo_repo->get_selected_branch( ) CP zif_abapgit_definitions=>c_git_branch-tags.
      lv_show_create_branch_popup = abap_true.
      lv_question_title = 'Staging on a tag'.
      lv_question_text = 'You are currently working on a tag.'.
      lv_question_text = |{ lv_question_text } You must be on a branch to stage.|.
      lv_question_text = |{ lv_question_text } Create new branch?|.
    ELSEIF lo_repo->get_selected_commit( ) IS NOT INITIAL.
      lv_show_create_branch_popup = abap_true.
      lv_question_title = 'Staging on a checked out commit'.
      lv_question_text = 'You are currently checked out in a commit.'.
      lv_question_text = |{ lv_question_text } You must be on a branch to stage.|.
      lv_question_text = |{ lv_question_text } Create new branch?|.
    ELSE.

      " force refresh on stage, to make sure the latest local and remote files are used
      lo_repo->refresh( ).

      CREATE OBJECT lo_stage_page
        EXPORTING
          io_repo       = lo_repo
          iv_seed       = lv_seed
          ii_obj_filter = ii_obj_filter.

      ri_page = lo_stage_page.

    ENDIF.

    IF lv_show_create_branch_popup = abap_true.

      lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
        iv_titlebar              = lv_question_title
        iv_text_question         = lv_question_text
        iv_text_button_1         = 'New branch' "Ideally the button name would be Create branch, but it did not fit
        iv_icon_button_1         = 'ICON_OKAY'
        iv_text_button_2         = 'Cancel'
        iv_icon_button_2         = 'ICON_CANCEL'
        iv_default_button        = '2'
        iv_display_cancel_button = abap_false ).
      IF lv_answer = '1'.
        TRY.
            zcl_abapgit_services_git=>create_branch( iv_key = lo_repo->get_key( ) ).
          CATCH zcx_abapgit_cancel.
            "Continue processing so we can return to the correct page
        ENDTRY.
      ENDIF.

      ri_page = zcl_abapgit_gui_page_repo_view=>create( lo_repo->get_key( ) ).

    ENDIF.

  ENDMETHOD.


  METHOD get_state_diff.

    " Bookmark current page before jumping to diff page
    IF ii_event->mv_current_page_name CP 'ZCL_ABAPGIT_GUI_PAGE_DIFF'.
      rv_state = zcl_abapgit_gui=>c_event_state-new_page.
    ELSE.
      rv_state = zcl_abapgit_gui=>c_event_state-new_page_w_bookmark.
    ENDIF.

  ENDMETHOD.


  METHOD get_state_settings.

    " Bookmark current page before jumping to any settings page
    IF ii_event->mv_current_page_name CP 'ZCL_ABAPGIT_GUI_PAGE_SETT_*'.
      rv_state = zcl_abapgit_gui=>c_event_state-new_page_replacing.
    ELSE.
      rv_state = zcl_abapgit_gui=>c_event_state-new_page_w_bookmark.
    ENDIF.

  ENDMETHOD.


  METHOD git_services.

    DATA lv_key TYPE zif_abapgit_persistence=>ty_repo-key.
    DATA li_repo TYPE REF TO zif_abapgit_repo.

    lv_key = ii_event->query( )->get( 'KEY' ).

    IF lv_key IS NOT INITIAL.
      li_repo = zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
    ENDIF.

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-git_pull.                      " GIT Pull
        zcl_abapgit_services_git=>pull( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-git_branch_create.             " GIT Create new branch
        zcl_abapgit_services_git=>create_branch( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-git_branch_delete.             " GIT Delete remote branch
        zcl_abapgit_services_git=>delete_branch( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-git_branch_switch.             " GIT Switch branch
        zcl_abapgit_services_git=>switch_branch( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-git_branch_merge.              " GIT Merge branch
        rs_handled-page  = zcl_abapgit_gui_page_merge_sel=>create( li_repo ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-git_tag_create.                " GIT Tag create
        rs_handled-page  = zcl_abapgit_gui_page_tags=>create( li_repo ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-git_tag_delete.                " GIT Tag delete
        zcl_abapgit_services_git=>delete_tag( lv_key ).
        zcl_abapgit_services_repo=>refresh( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-git_tag_switch.                " GIT Switch Tag
        zcl_abapgit_services_git=>switch_tag( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD jump_display_transport.

    DATA:
      lv_adt_link         TYPE string,
      lv_adt_jump_enabled TYPE abap_bool.

    lv_adt_jump_enabled = zcl_abapgit_persist_factory=>get_settings( )->read( )->get_adt_jump_enabled( ).
    IF lv_adt_jump_enabled = abap_true.
      TRY.
          lv_adt_link = zcl_abapgit_adt_link=>link_transport( iv_transport ).
          zcl_abapgit_ui_factory=>get_frontend_services( )->execute( iv_document = lv_adt_link ).
        CATCH zcx_abapgit_exception.
          " Fallback if ADT link execution failed or was cancelled
          CALL FUNCTION 'TR_DISPLAY_REQUEST'
            EXPORTING
              i_trkorr = iv_transport.
      ENDTRY.
    ELSE.
      CALL FUNCTION 'TR_DISPLAY_REQUEST'
        EXPORTING
          i_trkorr = iv_transport.
    ENDIF.

  ENDMETHOD.


  METHOD jump_display_user.

    " todo, user display in ADT

    CALL FUNCTION 'BAPI_USER_DISPLAY'
      EXPORTING
        username = iv_username.

  ENDMETHOD.


  METHOD jump_object.

    DATA:
      ls_item        TYPE zif_abapgit_definitions=>ty_item,
      lv_extra       TYPE string,
      lx_error       TYPE REF TO zcx_abapgit_exception,
      li_html_viewer TYPE REF TO zif_abapgit_html_viewer.

    ls_item-obj_type = cl_http_utility=>unescape_url( |{ iv_obj_type }| ).
    ls_item-obj_name = cl_http_utility=>unescape_url( |{ iv_obj_name }| ).

    TRY.
        li_html_viewer = zcl_abapgit_ui_factory=>get_html_viewer( ).

        " Hide HTML Viewer in dummy screen0 for direct CALL SCREEN to work
        li_html_viewer->set_visiblity( abap_false ).

        IF ls_item-obj_type = zif_abapgit_data_config=>c_data_type-tabu.
          zcl_abapgit_data_utils=>jump( ls_item ).
        ELSE.
          zcl_abapgit_objects=>jump(
            is_item     = ls_item
            iv_filename = iv_filename ).
        ENDIF.

        li_html_viewer->set_visiblity( abap_true ).
      CATCH zcx_abapgit_exception INTO lx_error.
        li_html_viewer->set_visiblity( abap_true ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

  ENDMETHOD.


  METHOD main_page.

    DATA lt_repo_all_list TYPE zif_abapgit_repo_srv=>ty_repo_list.

    " if there are no favorites, check if there are any repositories at all
    " if not, go to tutorial where the user can create the first repository
    lt_repo_all_list = zcl_abapgit_repo_srv=>get_instance( )->list( ).
    IF lt_repo_all_list IS NOT INITIAL.
      ri_page = zcl_abapgit_gui_page_repo_over=>create( ).
    ELSE.
      ri_page = zcl_abapgit_gui_page_tutorial=>create( ).
    ENDIF.

  ENDMETHOD.


  METHOD other_utilities.
    TYPES ty_char600 TYPE c LENGTH 600.
    DATA lv_clip_content TYPE string.
    DATA lt_clipboard TYPE STANDARD TABLE OF ty_char600.

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-ie_devtools.
        zcl_abapgit_services_basis=>open_ie_devtools( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-clipboard.
        lv_clip_content = ii_event->query( )->get( 'CLIPBOARD' ).
        APPEND lv_clip_content TO lt_clipboard.
        zcl_abapgit_ui_factory=>get_frontend_services( )->clipboard_export( lt_clipboard ).
        MESSAGE 'Successfully exported URL to Clipboard.' TYPE 'S'.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-yank_to_clipboard.
        lv_clip_content = ii_event->form_data( )->get( 'CLIPBOARD' ).
        APPEND lv_clip_content TO lt_clipboard.
        zcl_abapgit_ui_factory=>get_frontend_services( )->clipboard_export( lt_clipboard ).
        MESSAGE 'Successfully exported to Clipboard.' TYPE 'S'.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
    ENDCASE.

  ENDMETHOD.


  METHOD repository_services.

    DATA:
      lv_key  TYPE zif_abapgit_persistence=>ty_repo-key,
      lo_repo TYPE REF TO zcl_abapgit_repo,
      li_log  TYPE REF TO zif_abapgit_log.

    lv_key = ii_event->query( )->get( 'KEY' ).
    IF lv_key IS NOT INITIAL.
      lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
    ENDIF.

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-repo_newoffline.                 " New offline repo
        rs_handled-page  = zcl_abapgit_gui_page_addofflin=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-repo_add_all_obj_to_trans_req.   " Add objects to transport
        zcl_abapgit_transport=>add_all_objects_to_trans_req( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_refresh.                    " Repo refresh
        zcl_abapgit_services_repo=>refresh( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_syntax_check.
        CREATE OBJECT rs_handled-page TYPE zcl_abapgit_gui_page_syntax " Syntax check
          EXPORTING
            io_repo = lo_repo.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-repo_code_inspector.             " Code inspector
        CREATE OBJECT rs_handled-page TYPE zcl_abapgit_gui_page_code_insp
          EXPORTING
            io_repo = lo_repo.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-repo_purge.                      " Repo purge all objects (uninstall)
        zcl_abapgit_services_repo=>purge( lv_key ).
        rs_handled-page  = zcl_abapgit_gui_page_repo_over=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page_replacing.
      WHEN zif_abapgit_definitions=>c_action-repo_remove.                     " Repo remove
        zcl_abapgit_services_repo=>remove( lv_key ).
        rs_handled-page  = zcl_abapgit_gui_page_repo_over=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page_replacing.
      WHEN zif_abapgit_definitions=>c_action-repo_activate_objects.           " Repo activate objects
        zcl_abapgit_services_repo=>activate_objects( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_newonline.                  " New offline repo
        rs_handled-page  = zcl_abapgit_gui_page_addonline=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-repo_refresh_checksums.          " Rebuild local checksums
        zcl_abapgit_services_repo=>refresh_local_checksums( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_toggle_fav.                 " Toggle repo as favorite
        zcl_abapgit_services_repo=>toggle_favorite( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_transport_to_branch.        " Transport to branch
        zcl_abapgit_services_repo=>transport_to_branch( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-repo_settings.                   " Repo settings
        rs_handled-page  = zcl_abapgit_gui_page_sett_repo=>create( lo_repo ).
        rs_handled-state = get_state_settings( ii_event ).
      WHEN zif_abapgit_definitions=>c_action-repo_local_settings.             " Local repo settings
        rs_handled-page  = zcl_abapgit_gui_page_sett_locl=>create( lo_repo ).
        rs_handled-state = get_state_settings( ii_event ).
      WHEN zif_abapgit_definitions=>c_action-repo_remote_settings.            " Remote repo settings
        rs_handled-page  = zcl_abapgit_gui_page_sett_remo=>create( lo_repo ).
        rs_handled-state = get_state_settings( ii_event ).
      WHEN zif_abapgit_definitions=>c_action-repo_background.                 " Repo background mode
        rs_handled-page  = zcl_abapgit_gui_page_sett_bckg=>create( lo_repo ).
        rs_handled-state = get_state_settings( ii_event ).
      WHEN zif_abapgit_definitions=>c_action-repo_infos.                      " Repo infos
        rs_handled-page  = zcl_abapgit_gui_page_sett_info=>create( lo_repo ).
        rs_handled-state = get_state_settings( ii_event ).
      WHEN zif_abapgit_definitions=>c_action-repo_log.                        " Repo log
        li_log = lo_repo->get_log( ).
        zcl_abapgit_log_viewer=>show_log( li_log ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
    ENDCASE.

  ENDMETHOD.


  METHOD sap_gui_actions.

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-jump.                          " Open object editor
        jump_object(
          iv_obj_type = ii_event->query( )->get( 'TYPE' )
          iv_obj_name = ii_event->query( )->get( 'NAME' )
          iv_filename = ii_event->query( )->get( 'FILE' ) ).

        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN zif_abapgit_definitions=>c_action-jump_transport.
        jump_display_transport( |{ ii_event->query( )->get( 'TRANSPORT' ) }| ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN zif_abapgit_definitions=>c_action-jump_user.
        jump_display_user( |{ ii_event->query( )->get( 'USER' ) }| ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN zif_abapgit_definitions=>c_action-url.
        call_browser( ii_event->query( )->get( 'URL' ) ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    rs_handled = zcl_abapgit_exit=>get_instance( )->on_event( ii_event ).

    IF rs_handled-state IS INITIAL.
      rs_handled = general_page_routing( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = repository_services( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = git_services( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = zip_services( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = db_actions( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = abapgit_services_actions( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = sap_gui_actions( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = other_utilities( ii_event ).
    ENDIF.

    IF rs_handled-state IS INITIAL.
      rs_handled-state = zcl_abapgit_gui=>c_event_state-not_handled.
    ENDIF.

  ENDMETHOD.


  METHOD zip_services.

    DATA: lv_key              TYPE zif_abapgit_persistence=>ty_repo-key,
          lo_repo             TYPE REF TO zcl_abapgit_repo,
          lv_path             TYPE string,
          lv_dest             TYPE rfcdest,
          lv_msg              TYPE c LENGTH 200,
          lv_xstr             TYPE xstring,
          lv_package          TYPE zif_abapgit_persistence=>ty_repo-package,
          lv_folder_logic     TYPE string,
          lv_main_lang_only   TYPE zif_abapgit_persistence=>ty_local_settings-main_language_only,
          lo_obj_filter_trans TYPE REF TO zcl_abapgit_object_filter_tran,
          lt_r_trkorr         TYPE zif_abapgit_definitions=>ty_trrngtrkor_tt.

    CONSTANTS:
      BEGIN OF lc_page,
        main_view TYPE string VALUE 'ZCL_ABAPGIT_GUI_PAGE_MAIN',
        repo_view TYPE string VALUE 'ZCL_ABAPGIT_GUI_PAGE_REPO_VIEW',
      END OF lc_page.

    lv_key = ii_event->query( )->get( 'KEY' ).

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-zip_import                       " Import repo from ZIP
        OR zif_abapgit_definitions=>c_action-rfc_compare.                     " Compare repo via RFC

        lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).

        IF ii_event->mv_action = zif_abapgit_definitions=>c_action-zip_import.
          lv_path = zcl_abapgit_ui_factory=>get_frontend_services( )->show_file_open_dialog(
            iv_title            = 'Import ZIP'
            iv_extension        = 'zip'
            iv_default_filename = '*.zip' ).
          lv_xstr = zcl_abapgit_ui_factory=>get_frontend_services( )->file_upload( lv_path ).
        ELSE.
          lv_dest = zcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'RFCDES-RFCDEST' ).

          IF lv_dest IS INITIAL.
            rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
            RETURN.
          ENDIF.

          lv_package            = lo_repo->get_package( ).
          lv_folder_logic       = lo_repo->get_dot_abapgit( )->get_folder_logic( ).
          lv_main_lang_only     = lo_repo->get_local_settings( )-main_language_only.

          CALL FUNCTION 'Z_ABAPGIT_SERIALIZE_PACKAGE'
            DESTINATION lv_dest
            EXPORTING
              iv_package            = lv_package
              iv_folder_logic       = lv_folder_logic
              iv_main_lang_only     = lv_main_lang_only
            IMPORTING
              ev_xstring            = lv_xstr
            EXCEPTIONS
              system_failure        = 1 MESSAGE lv_msg
              communication_failure = 2 MESSAGE lv_msg
              OTHERS                = 3.
          IF sy-subrc <> 0.
            zcx_abapgit_exception=>raise( |RFC import error: { lv_msg }| ).
          ENDIF.
        ENDIF.

        lo_repo->set_files_remote( zcl_abapgit_zip=>load( lv_xstr ) ).
        zcl_abapgit_services_repo=>refresh( lv_key ).

        CASE ii_event->mv_current_page_name.
          WHEN lc_page-repo_view.
            rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
          WHEN lc_page-main_view.
            rs_handled-page  = zcl_abapgit_gui_page_repo_view=>create( lo_repo->get_key( ) ).
            rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
          WHEN OTHERS.
            rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ENDCASE.
      WHEN zif_abapgit_definitions=>c_action-zip_export.                      " Export repo as ZIP
        lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
        lv_xstr = zcl_abapgit_zip=>encode_files( lo_repo->get_files_local( ) ).
        file_download( iv_package = lo_repo->get_package( )
                       iv_xstr    = lv_xstr ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-zip_export_transport.                      " Export repo as ZIP

        lt_r_trkorr = zcl_abapgit_ui_factory=>get_popups( )->popup_select_wb_tc_tr_and_tsk( ).
        lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
        lo_repo->refresh( ).
        CREATE OBJECT lo_obj_filter_trans.
        lo_obj_filter_trans->set_filter_values( iv_package  = lo_repo->get_package( )
                                                it_r_trkorr = lt_r_trkorr ).

        lv_xstr = zcl_abapgit_zip=>encode_files( lo_repo->get_files_local( ii_obj_filter = lo_obj_filter_trans ) ).
        lo_repo->refresh( ).
        file_download( iv_package = lo_repo->get_package( )
                       iv_xstr    = lv_xstr ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-zip_package.                     " Export package as ZIP
        rs_handled-page  = zcl_abapgit_gui_page_ex_pckage=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN zif_abapgit_definitions=>c_action-zip_transport.                   " Export transports as ZIP
        zcl_abapgit_transport_mass=>run( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-zip_object.                      " Export object as ZIP
        rs_handled-page  = zcl_abapgit_gui_page_ex_object=>create( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
