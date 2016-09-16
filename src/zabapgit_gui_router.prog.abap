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
      IMPORTING iv_key         TYPE lcl_persistence_repo=>ty_repo-key
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
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

    METHODS repo_detach
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS repo_attach
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

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
      WHEN 'main'
          OR 'explore'
          OR 'db'
          OR 'background_run'.
        ei_page  = get_page_by_name( iv_action ).
        ev_state = gc_event_state-new_page.
      WHEN 'background'.
        ei_page  = get_page_background( lv_key ).
        ev_state = gc_event_state-new_page.
      WHEN 'jump'.
        lcl_html_action_utils=>jump_decode( EXPORTING iv_string   = iv_getdata
                                            IMPORTING ev_obj_type = ls_item-obj_type
                                                      ev_obj_name = ls_item-obj_name ).
        lcl_objects=>jump( ls_item ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'diff'.
        ei_page  = get_page_diff( iv_getdata ).
        ev_state = gc_event_state-new_page.
      WHEN 'stage'.
        lv_key   = iv_getdata.
        ei_page  = get_page_stage( lv_key ).
        ev_state = gc_event_state-new_page_w_bookmark.
      WHEN 'branch_overview'.
        ei_page  = get_page_branch_overview( iv_getdata ).
        ev_state = gc_event_state-new_page.

        " DB actions
      WHEN gc_action-db_display OR gc_action-db_edit.
        ei_page  = get_page_db_by_name( iv_name = iv_action  iv_getdata = iv_getdata ).
        ev_state = gc_event_state-new_page.
        IF iv_prev_page = 'PAGE_DB_DISPLAY'.
          ev_state = gc_event_state-new_page_replacing.
        ENDIF.
      WHEN gc_action-db_delete.
        ls_db = lcl_html_action_utils=>dbkey_decode( iv_getdata ).
        lcl_services_db=>delete( ls_db ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-db_update.
        ls_db = lcl_html_action_utils=>dbcontent_decode( it_postdata ).
        lcl_services_db=>update( ls_db ).
        ev_state = gc_event_state-go_back.

        " Abapgit services actions
      WHEN gc_action-abapgit_home.
        lcl_services_abapgit=>open_abapgit_homepage( ).
        ev_state = gc_event_state-no_more_act.
      WHEN gc_action-abapgit_install.
        lcl_services_abapgit=>install_abapgit( ).
        ev_state = gc_event_state-re_render.

        " Repository services actions
      WHEN gc_action-repo_refresh.  " Repo refresh
        lcl_services_repo=>refresh( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-repo_purge.    " Repo remove & purge all objects
        lcl_services_repo=>purge( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-repo_remove.   " Repo remove
        lcl_services_repo=>remove( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-repo_clone OR 'install'.    " Repo clone, 'install' is for explore page
        lcl_services_repo=>clone( lv_url ).
        ev_state = gc_event_state-re_render.

        " ZIP services actions
      WHEN gc_action-zip_import.      " Import repo from ZIP
        lcl_zip=>import( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-zip_export.      " Export repo as ZIP
        lcl_zip=>export( lcl_app=>repo_srv( )->get( lv_key ) ).
        ev_state = gc_event_state-no_more_act.
      WHEN gc_action-zip_package.     " Export package as ZIP
        lcl_zip=>export_package( ).
        ev_state = gc_event_state-no_more_act.
      WHEN gc_action-zip_transport.   " Export transport as ZIP
        lcl_transport=>zip( ).
        ev_state = gc_event_state-no_more_act.

        " Remote origin manipulations
      WHEN 'remote_attach'.
        lv_key   = iv_getdata.
        repo_attach( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'remote_detach'.
        lv_key   = iv_getdata.
        repo_detach( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'remote_change'.
        "TODO
        ev_state = gc_event_state-re_render.

        " Git actions
      WHEN gc_action-git_pull.
        lcl_services_git=>pull( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-git_reset.
        lcl_services_git=>reset( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN gc_action-git_branch_create.
        lcl_services_git=>create_branch( lv_key ).
        ev_state = gc_event_state-re_render.

        "Others
      WHEN OTHERS.
        ev_state = gc_event_state-not_handled.
    ENDCASE.

  ENDMETHOD.        " on_event

  METHOD get_page_by_name.

    DATA: lv_page_class TYPE string,
          lv_message    TYPE string.

    lv_page_class = |LCL_GUI_PAGE_{ to_upper( iv_name ) }|.

    TRY.
        CREATE OBJECT ri_page TYPE (lv_page_class).
      CATCH cx_sy_create_object_error.
        lv_message = |Cannot create page class { lv_page_class }|.
        lcx_exception=>raise( lv_message ).
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

    DATA: lt_remote TYPE ty_files_tt,
          lt_local  TYPE ty_files_item_tt,
          lo_page   TYPE REF TO lcl_gui_page_diff,
          lo_repo   TYPE REF TO lcl_repo_online,
          ls_file   TYPE ty_repo_file,
          lv_key    TYPE lcl_persistence_repo=>ty_repo-key.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF lt_remote,
                   <ls_local>  LIKE LINE OF lt_local.

    lcl_html_action_utils=>file_decode( EXPORTING iv_string = iv_getdata
                                        IMPORTING ev_key    = lv_key
                                                  eg_file   = ls_file ).

    lo_repo  ?= lcl_app=>repo_srv( )->get( lv_key ).
    lt_remote = lo_repo->get_files_remote( ).
    lt_local  = lo_repo->get_files_local( ).

    READ TABLE lt_remote ASSIGNING <ls_remote>
      WITH KEY filename = ls_file-filename
               path     = ls_file-path.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'file not found remotely' ).
    ENDIF.

    READ TABLE lt_local ASSIGNING <ls_local>
      WITH KEY file-filename = ls_file-filename
               file-path     = ls_file-path.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'file not found locally' ).
    ENDIF.

    CREATE OBJECT lo_page
      EXPORTING
        is_local  = <ls_local>-file
        is_remote = <ls_remote>.

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
        io_repo = lo_repo.

    ri_page = lo_stage_page.

  ENDMETHOD.  "get_page_stage

  METHOD get_page_background.

    CREATE OBJECT ri_page TYPE lcl_gui_page_background
      EXPORTING
        iv_key = iv_key.

  ENDMETHOD.  "get_page_background

  METHOD repo_detach.
    DATA: lv_answer TYPE c LENGTH 1,
          lo_repo   TYPE REF TO lcl_repo_online.

    lv_answer = lcl_popups=>popup_to_confirm(
      titlebar              = 'Make repository OFF-line'
      text_question         = 'This will detach the repo from remote and make it OFF-line'
      text_button_1         = 'Make OFF-line'
      icon_button_1         = 'ICON_WF_UNLINK'
      text_button_2         = 'Cancel'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false
    ).  "#EC NOTEXT

    IF lv_answer = '2'.
      RETURN.
    ENDIF.

*    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).
*    lo_repo->switch_type( iv_offline = abap_true ).
*
*    COMMIT WORK.
  ENDMETHOD.  "repo_detach


  METHOD repo_attach.
    DATA: ls_popup  TYPE lcl_popups=>ty_popup,
          lo_repo   TYPE REF TO lcl_repo_online.

    ls_popup = lcl_popups=>repo_popup( '' ).
    IF ls_popup-cancel = abap_true.
      RETURN.
    ENDIF.

*    "!!!! move convertion of repo type to srv
*    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).
*    lo_repo->switch_type( iv_offline = abap_false ).
*    lo_repo->set_url( ls_popup-url ).
*    lo_repo->set_branch_name( ls_popup-branch_name ).

*    COMMIT WORK.

  ENDMETHOD.  "repo_attach

ENDCLASS.           " lcl_gui_router