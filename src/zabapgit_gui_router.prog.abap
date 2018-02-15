*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_GUI_ROUTER
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_gui_router DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_gui_router DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS on_event
      IMPORTING iv_action    TYPE clike
                iv_prev_page TYPE clike
                iv_getdata   TYPE clike OPTIONAL
                it_postdata  TYPE cnht_post_data_tab OPTIONAL
      EXPORTING ei_page      TYPE REF TO zif_abapgit_gui_page
                ev_state     TYPE i
      RAISING   zcx_abapgit_exception zcx_abapgit_cancel.

  PRIVATE SECTION.

    METHODS get_page_by_name
      IMPORTING iv_name        TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO zif_abapgit_gui_page
      RAISING   zcx_abapgit_exception.

    METHODS get_page_diff
      IMPORTING iv_getdata     TYPE clike
                iv_prev_page   TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO zif_abapgit_gui_page
      RAISING   zcx_abapgit_exception.

    METHODS get_page_branch_overview
      IMPORTING iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO zif_abapgit_gui_page
      RAISING   zcx_abapgit_exception.

    METHODS get_page_stage
      IMPORTING iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO zif_abapgit_gui_page
      RAISING   zcx_abapgit_exception.

    METHODS get_page_db_by_name
      IMPORTING iv_name        TYPE clike
                iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO zif_abapgit_gui_page
      RAISING   zcx_abapgit_exception.

    METHODS get_page_background
      IMPORTING iv_key         TYPE zcl_abapgit_persistence_repo=>ty_repo-key
      RETURNING VALUE(ri_page) TYPE REF TO zif_abapgit_gui_page
      RAISING   zcx_abapgit_exception.

    METHODS get_page_playground
      RETURNING VALUE(ri_page) TYPE REF TO zif_abapgit_gui_page
      RAISING   zcx_abapgit_exception zcx_abapgit_cancel.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_gui_router IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_gui_router IMPLEMENTATION.

  METHOD on_event.

    DATA: lv_url  TYPE string,
          lv_key  TYPE zcl_abapgit_persistence_repo=>ty_repo-key,
          ls_db   TYPE zif_abapgit_persistence=>ty_content,
          ls_item TYPE zif_abapgit_definitions=>ty_item.

    lv_key = iv_getdata. " TODO refactor
    lv_url = iv_getdata. " TODO refactor

    CASE iv_action.
        " General PAGE routing
      WHEN zif_abapgit_definitions=>gc_action-go_main                          " Go Main page
          OR zif_abapgit_definitions=>gc_action-go_explore                     " Go Explore page
          OR zif_abapgit_definitions=>gc_action-go_db                          " Go DB util page
          OR zif_abapgit_definitions=>gc_action-go_debuginfo                   " Go debug info page
          OR zif_abapgit_definitions=>gc_action-go_settings.                   " Go settings page
        ei_page  = get_page_by_name( iv_action ).
        ev_state = zif_abapgit_definitions=>gc_event_state-new_page.
      WHEN zif_abapgit_definitions=>gc_action-go_background_run.              " Go background run page
        CREATE OBJECT ei_page TYPE lcl_gui_page_bkg_run.
        ev_state = zif_abapgit_definitions=>gc_event_state-new_page.
      WHEN zif_abapgit_definitions=>gc_action-go_background.                   " Go Background page
        ei_page  = get_page_background( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-new_page.
      WHEN zif_abapgit_definitions=>gc_action-go_diff.                         " Go Diff page
        ei_page  = get_page_diff(
          iv_getdata   = iv_getdata
          iv_prev_page = iv_prev_page ).
        ev_state = zif_abapgit_definitions=>gc_event_state-new_page_w_bookmark.
      WHEN zif_abapgit_definitions=>gc_action-go_stage.                        " Go Staging page
        ei_page  = get_page_stage( iv_getdata ).
        IF iv_prev_page = 'PAGE_DIFF'.
          ev_state = zif_abapgit_definitions=>gc_event_state-new_page.
        ELSE.
          ev_state = zif_abapgit_definitions=>gc_event_state-new_page_w_bookmark.
        ENDIF.
      WHEN zif_abapgit_definitions=>gc_action-go_branch_overview.              " Go repo branch overview
        ei_page  = get_page_branch_overview( iv_getdata ).
        ev_state = zif_abapgit_definitions=>gc_event_state-new_page.
      WHEN zif_abapgit_definitions=>gc_action-go_playground.                   " Create playground page
        ei_page  = get_page_playground( ).
        ev_state = zif_abapgit_definitions=>gc_event_state-new_page.
      WHEN zif_abapgit_definitions=>gc_action-go_tutorial.                     " Go to tutorial
        zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( '' ).        " Clear show_id
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.          " Assume we are on main page

        " SAP GUI actions
      WHEN zif_abapgit_definitions=>gc_action-jump.                          " Open object editor
        zcl_abapgit_html_action_utils=>jump_decode(
          EXPORTING iv_string   = iv_getdata
          IMPORTING ev_obj_type = ls_item-obj_type
                    ev_obj_name = ls_item-obj_name ).
        lcl_objects=>jump( ls_item ).
        ev_state = zif_abapgit_definitions=>gc_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>gc_action-jump_pkg.                      " Open SE80
        lcl_services_repo=>open_se80( |{ iv_getdata }| ).
        ev_state = zif_abapgit_definitions=>gc_event_state-no_more_act.

        " DB actions
      WHEN zif_abapgit_definitions=>gc_action-db_edit.
        ei_page = get_page_db_by_name( iv_name    = iv_action
                                       iv_getdata = iv_getdata ).
        ev_state = zif_abapgit_definitions=>gc_event_state-new_page.
        IF iv_prev_page = 'PAGE_DB_DIS'.
          ev_state = zif_abapgit_definitions=>gc_event_state-new_page_replacing.
        ENDIF.
      WHEN zif_abapgit_definitions=>gc_action-db_display.
        ei_page = get_page_db_by_name( iv_name    = 'DB_DIS'
                                       iv_getdata = iv_getdata ).
        ev_state = zif_abapgit_definitions=>gc_event_state-new_page.
      WHEN zif_abapgit_definitions=>gc_action-db_delete.                       " DB Delete
        ls_db = zcl_abapgit_html_action_utils=>dbkey_decode( iv_getdata ).
        lcl_services_db=>delete( ls_db ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-db_update.                       " DB Update
        ls_db = zcl_abapgit_html_action_utils=>dbcontent_decode( it_postdata ).
        lcl_services_db=>update( ls_db ).
        ev_state = zif_abapgit_definitions=>gc_event_state-go_back.

        " ABAPGIT services actions
      WHEN zif_abapgit_definitions=>gc_action-abapgit_home.                    " Go abapGit homepage
        lcl_services_abapgit=>open_abapgit_homepage( ).
        ev_state = zif_abapgit_definitions=>gc_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>gc_action-abapgit_wiki.                    " Go abapGit wikipage
        lcl_services_abapgit=>open_abapgit_wikipage( ).
        ev_state = zif_abapgit_definitions=>gc_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>gc_action-abapgit_install.                 " Install abapGit
        lcl_services_abapgit=>install_abapgit( ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-abapgit_install_pi.              " Install abapGit plugins
        lcl_services_abapgit=>install_abapgit_pi( ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.

        " REPOSITORY services actions
      WHEN zif_abapgit_definitions=>gc_action-repo_newoffline.                 " New offline repo
        lcl_services_repo=>new_offline( ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-repo_refresh.                    " Repo refresh
        lcl_services_repo=>refresh( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-repo_syntax_check.
        CREATE OBJECT ei_page TYPE lcl_gui_page_syntax
          EXPORTING
            io_repo = lcl_repo_srv=>get_instance( )->get( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-new_page.
      WHEN zif_abapgit_definitions=>gc_action-repo_purge.                      " Repo remove & purge all objects
        lcl_services_repo=>purge( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-repo_remove.                     " Repo remove
        lcl_services_repo=>remove( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-repo_clone OR 'install'.    " Repo clone, 'install' is for explore page
        lcl_services_repo=>clone( lv_url ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-repo_refresh_checksums.          " Rebuil local checksums
        lcl_services_repo=>refresh_local_checksums( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-repo_toggle_fav.                 " Toggle repo as favorite
        lcl_services_repo=>toggle_favorite( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-repo_transport_to_branch.
        lcl_services_repo=>transport_to_branch( iv_repository_key = lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-repo_settings.
        CREATE OBJECT ei_page TYPE lcl_gui_page_repo_sett
          EXPORTING
            io_repo = lcl_repo_srv=>get_instance( )->get( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-new_page.

        " ZIP services actions
      WHEN zif_abapgit_definitions=>gc_action-zip_import.                      " Import repo from ZIP
        lcl_zip=>import( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-zip_export.                      " Export repo as ZIP
        lcl_zip=>export( lcl_repo_srv=>get_instance( )->get( lv_key ) ).
        ev_state = zif_abapgit_definitions=>gc_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>gc_action-zip_package.                     " Export package as ZIP
        lcl_zip=>export_package( ).
        ev_state = zif_abapgit_definitions=>gc_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>gc_action-zip_transport.                   " Export transport as ZIP
        lcl_transport=>zip( ).
        ev_state = zif_abapgit_definitions=>gc_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>gc_action-zip_object.                      " Export object as ZIP
        lcl_zip=>export_object( ).
        ev_state = zif_abapgit_definitions=>gc_event_state-no_more_act.

        " Remote ORIGIN manipulations
      WHEN zif_abapgit_definitions=>gc_action-repo_remote_attach.            " Remote attach
        lcl_services_repo=>remote_attach( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-repo_remote_detach.            " Remote detach
        lcl_services_repo=>remote_detach( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-repo_remote_change.            " Remote change
        lcl_services_repo=>remote_change( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.

        " GIT actions
      WHEN zif_abapgit_definitions=>gc_action-git_pull.                      " GIT Pull
        lcl_services_git=>pull( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-git_reset.                     " GIT Reset
        lcl_services_git=>reset( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-git_branch_create.             " GIT Create new branch
        lcl_services_git=>create_branch( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-git_branch_delete.             " GIT Delete remote branch
        lcl_services_git=>delete_branch( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-git_branch_switch.             " GIT Switch branch
        lcl_services_git=>switch_branch( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-go_tag_overview.               " GIT Tag overview
        lcl_services_git=>tag_overview( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-git_tag_create.                " GIT Tag create
        lcl_services_git=>create_tag( lv_key ).
        lcl_services_repo=>refresh( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-git_tag_delete.                " GIT Tag create
        lcl_services_git=>delete_tag( lv_key ).
        lcl_services_repo=>refresh( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
      WHEN zif_abapgit_definitions=>gc_action-git_tag_switch.                " GIT Switch Tag
        lcl_services_git=>switch_tag( lv_key ).
        ev_state = zif_abapgit_definitions=>gc_event_state-re_render.

        "Others
      WHEN OTHERS.
        ev_state = zif_abapgit_definitions=>gc_event_state-not_handled.
    ENDCASE.

  ENDMETHOD.        " on_event

  METHOD get_page_by_name.

    DATA: lv_page_class TYPE string,
          lv_page_name  TYPE string.

    lv_page_name  = iv_name.
    SHIFT lv_page_name LEFT DELETING LEADING 'go_'.
    lv_page_class = |LCL_GUI_PAGE_{ to_upper( lv_page_name ) }|.

    TRY.
        CREATE OBJECT ri_page TYPE (lv_page_class).
      CATCH cx_sy_create_object_error.
        zcx_abapgit_exception=>raise( |Cannot create page class { lv_page_class }| ).
    ENDTRY.

  ENDMETHOD.        " get_page_by_name

  METHOD get_page_db_by_name.

    DATA: lv_page_class TYPE string,
          lv_message    TYPE string,
          ls_key        TYPE zif_abapgit_persistence=>ty_content.

    lv_page_class = |LCL_GUI_PAGE_{ to_upper( iv_name ) }|.
    ls_key        = zcl_abapgit_html_action_utils=>dbkey_decode( iv_getdata ).

    TRY.
        CREATE OBJECT ri_page TYPE (lv_page_class)
          EXPORTING
            is_key = ls_key.

      CATCH cx_sy_create_object_error.
        lv_message = |Cannot create page class { lv_page_class }|.
        zcx_abapgit_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.        " get_page_db_by_name

  METHOD get_page_branch_overview.

    DATA: lo_repo TYPE REF TO lcl_repo_online,
          lo_page TYPE REF TO lcl_gui_page_boverview,
          lv_key  TYPE zcl_abapgit_persistence_repo=>ty_repo-key.


    lv_key = iv_getdata.

    lo_repo ?= lcl_repo_srv=>get_instance( )->get( lv_key ).

    CREATE OBJECT lo_page
      EXPORTING
        io_repo = lo_repo.

    ri_page = lo_page.

  ENDMETHOD.  "get_page_branch_overview

  METHOD get_page_diff.

    DATA: ls_file   TYPE zif_abapgit_definitions=>ty_file,
          ls_object TYPE zif_abapgit_definitions=>ty_item,
          lo_page   TYPE REF TO lcl_gui_page_diff,
          lv_key    TYPE zcl_abapgit_persistence_repo=>ty_repo-key.


    zcl_abapgit_html_action_utils=>file_obj_decode(
      EXPORTING
        iv_string = iv_getdata
      IMPORTING
        ev_key    = lv_key
        eg_file   = ls_file
        eg_object = ls_object ).

    CREATE OBJECT lo_page
      EXPORTING
        iv_key           = lv_key
        is_file          = ls_file
        is_object        = ls_object
        iv_supress_stage = boolc( iv_prev_page = 'PAGE_STAGE' ).

    ri_page = lo_page.

  ENDMETHOD.  "get_page_diff

  METHOD get_page_stage.

    DATA: lo_repo       TYPE REF TO lcl_repo_online,
          lv_key        TYPE zcl_abapgit_persistence_repo=>ty_repo-key,
          lv_seed       TYPE string,
          lo_stage_page TYPE REF TO lcl_gui_page_stage.

    FIND FIRST OCCURRENCE OF '=' IN iv_getdata.
    IF sy-subrc <> 0. " Not found ? -> just repo key in params
      lv_key = iv_getdata.
    ELSE.
      zcl_abapgit_html_action_utils=>stage_decode(
        EXPORTING iv_getdata = iv_getdata
        IMPORTING ev_key     = lv_key
                  ev_seed    = lv_seed ).
    ENDIF.

    lo_repo ?= lcl_repo_srv=>get_instance( )->get( lv_key ).

    " force refresh on stage, to make sure the latest local and remote files are used
    lo_repo->refresh( ).

    CREATE OBJECT lo_stage_page
      EXPORTING
        io_repo = lo_repo
        iv_seed = lv_seed.

    ri_page = lo_stage_page.

  ENDMETHOD.  "get_page_stage

  METHOD get_page_background.

    CREATE OBJECT ri_page TYPE lcl_gui_page_bkg
      EXPORTING
        iv_key = iv_key.

  ENDMETHOD.  "get_page_background

  METHOD get_page_playground.
    DATA: lv_class_name TYPE string,
          lv_cancel     TYPE abap_bool.

    lcl_popups=>run_page_class_popup( IMPORTING ev_name   = lv_class_name
                                                ev_cancel = lv_cancel ).
    IF lv_cancel = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    TRY.
        CREATE OBJECT ri_page TYPE (lv_class_name).
      CATCH cx_sy_create_object_error.
        zcx_abapgit_exception=>raise( |Cannot create page class { lv_class_name }| ).
    ENDTRY.

  ENDMETHOD.  "get_page_playground

ENDCLASS.           " lcl_gui_router
