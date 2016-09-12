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
      RAISING   lcx_exception.

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

    METHODS abapgit_installation
      RAISING lcx_exception.

    METHODS repo_purge
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS repo_remove
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS repo_pull
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS repo_detach
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS repo_attach
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS reset
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS create_branch
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS db_delete
      IMPORTING iv_getdata TYPE clike
      RAISING   lcx_exception.

    METHODS db_save
      IMPORTING it_postdata TYPE cnht_post_data_tab
      RAISING   lcx_exception.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_gui_router IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_gui_router IMPLEMENTATION.

  METHOD on_event.

    DATA: lv_url  TYPE string,
          lv_key  TYPE lcl_persistence_repo=>ty_repo-key,
          ls_item TYPE ty_item.


    CASE iv_action.
        " General routing
      WHEN 'main'
          OR 'explore'
          OR 'db'
          OR 'background_run'.
        ei_page  = get_page_by_name( iv_action ).
        ev_state = gc_event_state-new_page.
      WHEN 'background'.
        lv_key = iv_getdata.
        CREATE OBJECT ei_page TYPE lcl_gui_page_background
          EXPORTING
            iv_key = lv_key.
        ev_state = gc_event_state-new_page.
      WHEN 'abapgithome'.
        cl_gui_frontend_services=>execute( EXPORTING document = gc_abapgit_homepage
                                           EXCEPTIONS OTHERS = 1 ).
        IF sy-subrc <> 0.
          lcx_exception=>raise( 'Opening page in external browser failed.' ).
        ENDIF.
        ev_state = gc_event_state-no_more_act.
      WHEN 'abapgit_installation'.
        abapgit_installation( ).
        ev_state = gc_event_state-re_render.
      WHEN 'jump'.
        lcl_html_action_utils=>jump_decode( EXPORTING iv_string   = iv_getdata
                                            IMPORTING ev_obj_type = ls_item-obj_type
                                                      ev_obj_name = ls_item-obj_name ).
        lcl_objects=>jump( ls_item ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'diff'.
        ei_page  = get_page_diff( iv_getdata ).
        ev_state = gc_event_state-new_page.

        " DB actions
      WHEN 'db_display' OR 'db_edit'.
        ei_page  = get_page_db_by_name( iv_name = iv_action  iv_getdata = iv_getdata ).
        IF iv_prev_page = 'PAGE_DB_DISPLAY'.
          ev_state = gc_event_state-new_page_replacing.
        ELSE.
          ev_state = gc_event_state-new_page.
        ENDIF.
      WHEN 'db_delete'.
        db_delete( iv_getdata = iv_getdata ).
        ev_state = gc_event_state-re_render.
      WHEN 'db_save'.
        db_save( it_postdata ).
        ev_state = gc_event_state-go_back.

        " Repository state actions
      WHEN 'uninstall'.
        lv_key   = iv_getdata.
        repo_purge( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'remove'.
        lv_key   = iv_getdata.
        repo_remove( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'zipimport'.
        lv_key   = iv_getdata.
        lcl_zip=>import( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'zipexport'.
        lv_key   = iv_getdata.
        lcl_zip=>export( lcl_app=>repo_srv( )->get( lv_key ) ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'files_commit'. "TODO refactor name ?
        lv_key   = iv_getdata.
        lcl_zip=>export( io_repo = lcl_app=>repo_srv( )->get( lv_key )
                         iv_zip  = abap_false ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'packagezip'.
        lcl_popups=>repo_package_zip( ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'transportzip'.
        lcl_transport=>zip( ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'refresh'.
        lv_key = iv_getdata.
        lcl_app=>repo_srv( )->get( lv_key )->refresh( ).
        ev_state = gc_event_state-re_render.

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

        " explore page
      WHEN 'install'.
        lv_url = iv_getdata.
        lcl_popups=>repo_clone( lv_url ).
        ev_state = gc_event_state-re_render.

        " Repository online actions
      WHEN 'pull'.
        lv_key   = iv_getdata.
        repo_pull( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'stage'.
        lv_key   = iv_getdata.
        ei_page  = get_page_stage( lv_key ).
        ev_state = gc_event_state-new_page_w_bookmark.
      WHEN 'reset'.
        lv_key   = iv_getdata.
        reset( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'create_branch'.
        lv_key   = iv_getdata.
        create_branch( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'branch_overview'.
        ei_page  = get_page_branch_overview( iv_getdata ).
        ev_state = gc_event_state-new_page.
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

  METHOD abapgit_installation.

    CONSTANTS lc_package_abapgit TYPE devclass VALUE '$ABAPGIT'.
    CONSTANTS lc_package_plugins TYPE devclass VALUE '$ABAPGIT_PLUGINS'.

    DATA lv_text            TYPE c LENGTH 100.
    DATA lv_answer          TYPE c LENGTH 1.
    DATA lo_repo            TYPE REF TO lcl_repo_online.
    DATA lv_url             TYPE string.
    DATA lv_target_package  TYPE devclass.

    lv_text = |Installing current version ABAPGit to package { lc_package_abapgit } |
           && |and plugins to { lc_package_plugins }|.

    lv_answer = lcl_popups=>popup_to_confirm(
      titlebar              = 'Install abapGit'
      text_question         = lv_text
      text_button_1         = 'Continue'
      text_button_2         = 'Cancel'
      default_button        = '2'
      display_cancel_button = abap_false
    ).  "#EC NOTEXT

    IF lv_answer <> '1'.
      RETURN. ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ENDIF.

    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          lv_url            = 'https://github.com/larshp/abapGit.git'.
          lv_target_package = lc_package_abapgit.
        WHEN 2.
          lv_url            = 'https://github.com/larshp/abapGit-plugins.git' ##no_text.
          lv_target_package = lc_package_plugins.
      ENDCASE.

      IF abap_false = lcl_app=>repo_srv( )->is_repo_installed(
          iv_url              = lv_url
          iv_target_package   = lv_target_package ).

        lcl_sap_package=>create_local( lv_target_package ).

        lo_repo = lcl_app=>repo_srv( )->new_online(
          iv_url         = lv_url
          iv_branch_name = 'refs/heads/master' "TODO replace with HEAD ?
          iv_package     = lv_target_package ) ##NO_TEXT.

        lo_repo->status( ). " check for errors
        lo_repo->deserialize( ).
      ENDIF.
    ENDDO.

    COMMIT WORK.

  ENDMETHOD. "abapgit_installation

  METHOD repo_purge.

    DATA: lt_tadir    TYPE ty_tadir_tt,
          lv_count    TYPE c LENGTH 3,
          lv_answer   TYPE c LENGTH 1,
          lo_repo     TYPE REF TO lcl_repo,
          lv_package  TYPE devclass,
          lv_question TYPE c LENGTH 100.


    lo_repo = lcl_app=>repo_srv( )->get( iv_key ).

    IF lo_repo->is_write_protected( ) = abap_true.
      lcx_exception=>raise( 'Cannot purge. Local code is write-protected by repo config' ).
    ENDIF.

    lv_package = lo_repo->get_package( ).
    lt_tadir   = lcl_tadir=>read( lv_package ).

    IF lines( lt_tadir ) > 0.
      lv_count = lines( lt_tadir ).
      SHIFT lv_count LEFT DELETING LEADING space.

      lv_question = |This will DELETE all objects in package { lv_package }|
                 && | ({ lv_count } objects) from the system.|. "#EC NOTEXT

      lv_answer = lcl_popups=>popup_to_confirm(
        titlebar              = 'Uninstall'
        text_question         = lv_question
        text_button_1         = 'Delete'
        icon_button_1         = 'ICON_DELETE'
        text_button_2         = 'Cancel'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false
      ).  "#EC NOTEXT

      IF lv_answer = '2'.
        RETURN.
      ENDIF.

      lcl_objects=>delete( lt_tadir ).

    ENDIF.

    lcl_app=>repo_srv( )->delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.                    "repo_purge

  METHOD repo_remove.

    DATA: lv_answer   TYPE c LENGTH 1,
          lo_repo     TYPE REF TO lcl_repo,
          lv_package  TYPE devclass,
          lv_question TYPE c LENGTH 200.


    lo_repo = lcl_app=>repo_srv( )->get( iv_key ).
    lv_package = lo_repo->get_package( ).

    CONCATENATE 'This will remove the repository reference to the package'
      lv_package
      '. All objects will safely remain in the system.'
      INTO lv_question
      SEPARATED BY space.                                   "#EC NOTEXT

    lv_answer = lcl_popups=>popup_to_confirm(
      titlebar              = 'Remove'
      text_question         = lv_question
      text_button_1         = 'Remove'
      icon_button_1         = 'ICON_WF_UNLINK'
      text_button_2         = 'Cancel'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false
    ).  "#EC NOTEXT

    IF lv_answer = '2'.
      RETURN.
    ENDIF.

    lcl_app=>repo_srv( )->delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.                    "repo_remove

  METHOD reset.

    DATA: lo_repo   TYPE REF TO lcl_repo_online,
          lv_answer TYPE c LENGTH 1.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    IF lo_repo->is_write_protected( ) = abap_true.
      lcx_exception=>raise( 'Cannot reset. Local code is write-protected by repo config' ).
    ENDIF.

    lv_answer = lcl_popups=>popup_to_confirm(
      titlebar              = 'Warning'
      text_question         = 'Reset local objects?'
      text_button_1         = 'Ok'
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = 'Cancel'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false
    ).  "#EC NOTEXT

    IF lv_answer = '2'.
      RETURN.
    ENDIF.

    lo_repo->deserialize( ).

  ENDMETHOD.  "reset

  METHOD create_branch.

    DATA: lv_name   TYPE string,
          lv_cancel TYPE abap_bool,
          lo_repo   TYPE REF TO lcl_repo_online.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    lcl_popups=>create_branch_popup(
      IMPORTING
        ev_name   = lv_name
        ev_cancel = lv_cancel ).
    IF lv_cancel = abap_true.
      RETURN.
    ENDIF.

    ASSERT lv_name CP 'refs/heads/+*'.

    lcl_git_porcelain=>create_branch(
      io_repo = lo_repo
      iv_name = lv_name
      iv_from = lo_repo->get_sha1_local( ) ).

* automatically switch to new branch
    lo_repo->set_branch_name( lv_name ).

    MESSAGE 'Switched to new branch' TYPE 'S' ##NO_TEXT.

  ENDMETHOD.  "create_branch

  METHOD repo_pull.

    DATA: lo_repo TYPE REF TO lcl_repo_online.

    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    IF lo_repo->is_write_protected( ) = abap_true.
      lcx_exception=>raise( 'Cannot pull. Local code is write-protected by repo config' ).
    ENDIF.

    lo_repo->refresh( ).
    lo_repo->deserialize( ).

    COMMIT WORK.

  ENDMETHOD.  "repo_pull

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

  METHOD db_delete.

    DATA: lv_answer TYPE c LENGTH 1,
          ls_key    TYPE lcl_persistence_db=>ty_content.


    ls_key = lcl_html_action_utils=>dbkey_decode( iv_getdata ).

    lv_answer = lcl_popups=>popup_to_confirm(
      titlebar              = 'Warning'
      text_question         = 'Delete?'
      text_button_1         = 'Ok'
      icon_button_1         = 'ICON_DELETE'
      text_button_2         = 'Cancel'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false
    ).  "#EC NOTEXT

    IF lv_answer = '2'.
      RETURN.
    ENDIF.

    lcl_app=>db( )->delete(
      iv_type  = ls_key-type
      iv_value = ls_key-value ).

    COMMIT WORK.

  ENDMETHOD.  "db_delete

  METHOD db_save.

    DATA: lv_string  TYPE string,
          ls_content TYPE lcl_persistence_db=>ty_content,
          lt_fields  TYPE tihttpnvp.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    CONCATENATE LINES OF it_postdata INTO lv_string.

    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'type' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    ls_content-type = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'value' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    ls_content-value = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'xmldata' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    IF <ls_field>-value(1) <> '<'.
      ls_content-data_str = <ls_field>-value+1. " hmm
    ENDIF.

    lcl_app=>db( )->update(
      iv_type  = ls_content-type
      iv_value = ls_content-value
      iv_data  = ls_content-data_str ).

    COMMIT WORK.

  ENDMETHOD.  "db_save

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