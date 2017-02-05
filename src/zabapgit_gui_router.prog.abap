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
      EXPORTING ei_page      TYPE REF TO lif_gui_page
                ev_state     TYPE i
      RAISING   lcx_exception lcx_cancel.

  PRIVATE SECTION.

    METHODS get_page_by_name
      IMPORTING iv_name        TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS get_page_diff
      IMPORTING iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS get_page_branch_overview
      IMPORTING iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS get_page_stage
      IMPORTING iv_key          TYPE lcl_persistence_repo=>ty_repo-key
      RETURNING VALUE(ri_page)  TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS get_page_db_by_name
      IMPORTING iv_name        TYPE clike
                iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS get_page_background
      IMPORTING iv_key         TYPE lcl_persistence_repo=>ty_repo-key
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS get_page_playground
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception lcx_cancel.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_gui_router IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_gui_router IMPLEMENTATION.

  METHOD on_event.

    DATA: lv_url     TYPE string,
          lv_key     TYPE lcl_persistence_repo=>ty_repo-key,
          ls_db      TYPE lcl_persistence_db=>ty_content,
          ls_item    TYPE ty_item.

    lv_key = iv_getdata. " TODO refactor
    lv_url = iv_getdata. " TODO refactor

    CASE iv_action.
        " General routing
      WHEN gc_action-go_main                          " Go Main page
          OR gc_action-go_explore                     " Go Explore page
          OR gc_action-go_db                          " Go DB util page
          OR gc_action-go_background_run              " Go background run page
          OR gc_action-go_debuginfo                   " Go debug info page
          OR gc_action-go_settings.                   " Go settings page
        ei_page  = get_page_by_name( iv_action ).
        ev_state = gc_event_state-new_page.
      WHEN gc_action-go_background.                   " Go Background page
        ei_page  = get_page_background( lv_key ).
        ev_state = gc_event_state-new_page.
      WHEN gc_action-go_diff.                         " Go Diff page
        ei_page  = get_page_diff( iv_getdata ).
        ev_state = gc_event_state-new_page.
      WHEN gc_action-go_stage.                        " Go Staging page
        ei_page  = get_page_stage( lv_key ).
        ev_state = gc_event_state-new_page_w_bookmark.
      WHEN gc_action-go_branch_overview.              " Go repo branch overview
        ei_page  = get_page_branch_overview( iv_getdata ).
        ev_state = gc_event_state-new_page.
      WHEN gc_action-go_playground.                   " Create playground page
        ei_page  = get_page_playground( ).
        ev_state = gc_event_state-new_page.
      WHEN gc_action-go_tutorial.                     " Go to tutorial
        lcl_app=>user( )->set_repo_show( '' ).        " Clear show_id
        ev_state = gc_event_state-re_render.          " Assume we are on main page

        " SAP GUI actions
      WHEN gc_action-jump.                          " Open object editor
        lcl_html_action_utils=>jump_decode( EXPORTING iv_string   = iv_getdata
                                            IMPORTING ev_obj_type = ls_item-obj_type
                                                      ev_obj_name = ls_item-obj_name ).
        lcl_objects=>jump( ls_item ).
        ev_state = gc_event_state-no_more_act.

      WHEN gc_action-jump_pkg.                      " Open SE80
        lcl_services_repo=>open_se80( |{ iv_getdata }| ).
        ev_state = gc_event_state-no_more_act.

        " DB actions
      WHEN gc_action-db_display OR gc_action-db_edit. " DB Display/Edit
        ei_page  = get_page_db_by_name( iv_name = iv_action  iv_getdata = iv_getdata ).
        ev_state = gc_event_state-new_page.
        IF iv_prev_page = 'PAGE_DB_DISPLAY'.
          ev_state = gc_event_state-new_page_replacing.
        ENDIF.
      WHEN gc_action-db_delete.                       " DB Delete
        ls_db = lcl_html_action_utils=>dbkey_decode( iv_getdata ).
        lcl_services_db=>delete( ls_db ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-db_update.                       " DB Update
        ls_db = lcl_html_action_utils=>dbcontent_decode( it_postdata ).
        lcl_services_db=>update( ls_db ).
        ev_state = gc_event_state-go_back.

        " Abapgit services actions
      WHEN gc_action-abapgit_home.                    " Go abapGit homepage
        lcl_services_abapgit=>open_abapgit_homepage( ).
        ev_state = gc_event_state-no_more_act.
      WHEN gc_action-abapgit_wiki.                    " Go abapGit wikipage
        lcl_services_abapgit=>open_abapgit_wikipage( ).
        ev_state = gc_event_state-no_more_act.
      WHEN gc_action-abapgit_install.                 " Install abapGit
        lcl_services_abapgit=>install_abapgit( ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-abapgit_install_pi.              " Install abapGit plugins
        lcl_services_abapgit=>install_abapgit_pi( ).
        ev_state = gc_event_state-re_render.

        " Repository services actions
      WHEN gc_action-repo_newoffline.                 " New offline repo
        lcl_services_repo=>new_offline( ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-repo_refresh.                    " Repo refresh
        lcl_services_repo=>refresh( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-repo_purge.                      " Repo remove & purge all objects
        lcl_services_repo=>purge( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-repo_remove.                     " Repo remove
        lcl_services_repo=>remove( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-repo_clone OR 'install'.    " Repo clone, 'install' is for explore page
        lcl_services_repo=>clone( lv_url ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-repo_refresh_checksums.          " Rebuil local checksums
        lcl_services_repo=>refresh_local_checksums( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-repo_toggle_fav.                 " Toggle repo as favorite
        lcl_services_repo=>toggle_favorite( lv_key ).
        ev_state = gc_event_state-re_render.

        " ZIP services actions
      WHEN gc_action-zip_import.                      " Import repo from ZIP
        lcl_zip=>import( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-zip_export.                      " Export repo as ZIP
        lcl_zip=>export( lcl_app=>repo_srv( )->get( lv_key ) ).
        ev_state = gc_event_state-no_more_act.
      WHEN gc_action-zip_package.                     " Export package as ZIP
        lcl_zip=>export_package( ).
        ev_state = gc_event_state-no_more_act.
      WHEN gc_action-zip_transport.                   " Export transport as ZIP
        lcl_transport=>zip( ).
        ev_state = gc_event_state-no_more_act.
      WHEN gc_action-zip_object.                      " Export object as ZIP
        lcl_zip=>export_object( ).
        ev_state = gc_event_state-no_more_act.

        " Remote origin manipulations
      WHEN gc_action-repo_remote_attach.            " Remote attach
        lcl_services_repo=>remote_attach( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-repo_remote_detach.            " Remote detach
        lcl_services_repo=>remote_detach( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-repo_remote_change.            " Remote change
        lcl_services_repo=>remote_change( lv_key ).
        ev_state = gc_event_state-re_render.

        " Git actions
      WHEN gc_action-git_pull.                      " GIT Pull
        lcl_services_git=>pull( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-git_reset.                     " GIT Reset
        lcl_services_git=>reset( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-git_branch_create.             " GIT Create new branch
        lcl_services_git=>create_branch( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-git_branch_delete.             " Delete remote branch
        lcl_services_git=>delete_branch( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-git_branch_switch.             " Switch branch
        lcl_services_git=>switch_branch( lv_key ).
        ev_state = gc_event_state-re_render.

        "Others
      WHEN OTHERS.
        ev_state = gc_event_state-not_handled.
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
        lcx_exception=>raise( |Cannot create page class { lv_page_class }| ).
    ENDTRY.

  ENDMETHOD.        " get_page_by_name

  METHOD get_page_db_by_name.

    DATA: lv_page_class TYPE string,
          lv_message    TYPE string,
          ls_key        TYPE lcl_persistence_db=>ty_content.

    lv_page_class = |LCL_GUI_PAGE_{ to_upper( iv_name ) }|.
    ls_key        = lcl_html_action_utils=>dbkey_decode( iv_getdata ).

    TRY.
        CREATE OBJECT ri_page TYPE (lv_page_class)
          EXPORTING
            is_key = ls_key.

      CATCH cx_sy_create_object_error.
        lv_message = |Cannot create page class { lv_page_class }|.
        lcx_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.        " get_page_db_by_name

  METHOD get_page_branch_overview.

    DATA: lo_repo TYPE REF TO lcl_repo_online,
          lo_page TYPE REF TO lcl_gui_page_branch_overview,
          lv_key  TYPE lcl_persistence_repo=>ty_repo-key.


    lv_key = iv_getdata.

    lo_repo ?= lcl_app=>repo_srv( )->get( lv_key ).

    CREATE OBJECT lo_page
      EXPORTING
        io_repo = lo_repo.

    ri_page = lo_page.

  ENDMETHOD.  "get_page_branch_overview

  METHOD get_page_diff.

    DATA: ls_file   TYPE ty_file,
          ls_object TYPE ty_item,
          lo_page   TYPE REF TO lcl_gui_page_diff,
          lv_key    TYPE lcl_persistence_repo=>ty_repo-key.


    lcl_html_action_utils=>file_obj_decode( EXPORTING iv_string = iv_getdata
                                            IMPORTING ev_key    = lv_key
                                                      eg_file   = ls_file
                                                      eg_object = ls_object ).

    CREATE OBJECT lo_page
      EXPORTING
        iv_key    = lv_key
        is_file   = ls_file
        is_object = ls_object.

    ri_page = lo_page.

  ENDMETHOD.  "get_page_diff

  METHOD get_page_stage.

    DATA: lo_repo       TYPE REF TO lcl_repo_online,
          lo_stage_page TYPE REF TO lcl_gui_page_stage.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    " force refresh on stage, to make sure the latest local and remote files are used
    lo_repo->refresh( ).

    CREATE OBJECT lo_stage_page
      EXPORTING
        io_repo         = lo_repo.

    ri_page = lo_stage_page.

  ENDMETHOD.  "get_page_stage

  METHOD get_page_background.

    CREATE OBJECT ri_page TYPE lcl_gui_page_background
      EXPORTING
        iv_key = iv_key.

  ENDMETHOD.  "get_page_background

  METHOD get_page_playground.
    DATA: lv_class_name TYPE string,
          lv_cancel     TYPE abap_bool.

    lcl_popups=>run_page_class_popup( IMPORTING ev_name   = lv_class_name
                                                ev_cancel = lv_cancel ).
    IF lv_cancel = abap_true.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    TRY.
        CREATE OBJECT ri_page TYPE (lv_class_name).
      CATCH cx_sy_create_object_error.
        lcx_exception=>raise( |Cannot create page class { lv_class_name }| ).
    ENDTRY.

  ENDMETHOD.  "get_page_playground

ENDCLASS.           " lcl_gui_router