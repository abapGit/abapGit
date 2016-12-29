*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_SERVICES_REPO
*&---------------------------------------------------------------------*

CLASS lcl_services_repo DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS clone
      IMPORTING iv_url TYPE string
      RAISING   lcx_exception lcx_cancel.

    CLASS-METHODS refresh
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    CLASS-METHODS remove
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception lcx_cancel.

    CLASS-METHODS purge
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception lcx_cancel.

    CLASS-METHODS new_offline
      RAISING lcx_exception lcx_cancel.

    CLASS-METHODS remote_attach
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception lcx_cancel.

    CLASS-METHODS remote_detach
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception lcx_cancel.

    CLASS-METHODS remote_change
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception lcx_cancel.

    CLASS-METHODS refresh_local_checksums
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception lcx_cancel.

    CLASS-METHODS toggle_favorite
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    CLASS-METHODS open_se80
      IMPORTING iv_package TYPE devclass
      RAISING   lcx_exception.

ENDCLASS. "lcl_services_repo

CLASS lcl_services_repo IMPLEMENTATION.

  METHOD clone.

    DATA: lo_repo  TYPE REF TO lcl_repo_online,
          ls_popup TYPE lcl_popups=>ty_popup.


    ls_popup = lcl_popups=>repo_popup( iv_url ).
    IF ls_popup-cancel = abap_true.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    lo_repo = lcl_app=>repo_srv( )->new_online(
      iv_url         = ls_popup-url
      iv_branch_name = ls_popup-branch_name
      iv_package     = ls_popup-package ).

    toggle_favorite( lo_repo->get_key( ) ).

    lo_repo->status( ). " check for errors
    lo_repo->deserialize( ).

    lcl_app=>user( )->set_repo_show( lo_repo->get_key( ) ). " Set default repo for user

    COMMIT WORK.

  ENDMETHOD.  "clone

  METHOD refresh.

    lcl_app=>repo_srv( )->get( iv_key )->refresh( ).

  ENDMETHOD.  "refresh

  METHOD remove.

    DATA: lv_answer   TYPE c LENGTH 1,
          lo_repo     TYPE REF TO lcl_repo,
          lv_package  TYPE devclass,
          lv_question TYPE c LENGTH 200.


    lo_repo     = lcl_app=>repo_srv( )->get( iv_key ).
    lv_package  = lo_repo->get_package( ).
    lv_question = |This will remove the repository reference to the package { lv_package }|
               && '. All objects will safely remain in the system.'.

    lv_answer = lcl_popups=>popup_to_confirm(
      titlebar              = 'Remove'
      text_question         = lv_question
      text_button_1         = 'Remove'
      icon_button_1         = 'ICON_WF_UNLINK'
      text_button_2         = 'Cancel'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false ).                 "#EC NOTEXT

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    lcl_app=>repo_srv( )->delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.  "remove

  METHOD purge.

    DATA: lt_tadir    TYPE ty_tadir_tt,
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

      lv_question = |This will DELETE all objects in package { lv_package }|
                 && | ({ lines( lt_tadir ) } objects) from the system|. "#EC NOTEXT

      lv_answer = lcl_popups=>popup_to_confirm(
        titlebar              = 'Uninstall'
        text_question         = lv_question
        text_button_1         = 'Delete'
        icon_button_1         = 'ICON_DELETE'
        text_button_2         = 'Cancel'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false ).               "#EC NOTEXT

      IF lv_answer = '2'.
        RAISE EXCEPTION TYPE lcx_cancel.
      ENDIF.

      lcl_objects=>delete( lt_tadir ).

    ENDIF.

    lcl_app=>repo_srv( )->delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.  "purge

  METHOD new_offline.

    DATA: lo_repo  TYPE REF TO lcl_repo,
          ls_popup TYPE lcl_popups=>ty_popup.

    ls_popup  = lcl_popups=>repo_new_offline( ).
    IF ls_popup-cancel = abap_true.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    lo_repo = lcl_app=>repo_srv( )->new_offline(
      iv_url     = ls_popup-url
      iv_package = ls_popup-package ).

    lcl_app=>user( )->set_repo_show( lo_repo->get_key( ) ). " Set default repo for user
    toggle_favorite( lo_repo->get_key( ) ).

    COMMIT WORK.

  ENDMETHOD.  "new_offline

  METHOD remote_detach.

    DATA: lv_answer TYPE c LENGTH 1.

    lv_answer = lcl_popups=>popup_to_confirm(
      titlebar              = 'Make repository OFF-line'
      text_question         = 'This will detach the repo from remote and make it OFF-line'
      text_button_1         = 'Make OFF-line'
      icon_button_1         = 'ICON_WF_UNLINK'
      text_button_2         = 'Cancel'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false ).                 "#EC NOTEXT

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    lcl_app=>repo_srv( )->switch_repo_type( iv_key = iv_key  iv_offline = abap_true ).

    COMMIT WORK.

  ENDMETHOD.  "remote_detach


  METHOD remote_attach.

    DATA: ls_popup TYPE lcl_popups=>ty_popup,
          lo_repo  TYPE REF TO lcl_repo_online.

    ls_popup = lcl_popups=>repo_popup(
      iv_title          = 'Attach repo to remote ...'
      iv_url            = ''
      iv_package        = lcl_app=>repo_srv( )->get( iv_key )->get_package( )
      iv_freeze_package = abap_true ).
    IF ls_popup-cancel = abap_true.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    lcl_app=>repo_srv( )->switch_repo_type( iv_key = iv_key  iv_offline = abap_false ).

    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).
    lo_repo->set_url( ls_popup-url ).
    lo_repo->set_branch_name( ls_popup-branch_name ).

    COMMIT WORK.

  ENDMETHOD.  "remote_attach

  METHOD remote_change.

    DATA: ls_popup TYPE lcl_popups=>ty_popup,
          lo_repo  TYPE REF TO lcl_repo_online.

    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    ls_popup = lcl_popups=>repo_popup(
      iv_title          = 'Change repo remote ...'
      iv_url            = lo_repo->get_url( )
      iv_package        = lo_repo->get_package( )
      iv_freeze_package = abap_true ).
    IF ls_popup-cancel = abap_true.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).
    lo_repo->set_new_remote( iv_url         = ls_popup-url
                             iv_branch_name = ls_popup-branch_name ).

    COMMIT WORK.

  ENDMETHOD.  "remote_change

  METHOD refresh_local_checksums.

    DATA: lv_answer   TYPE c,
          lv_question TYPE string,
          lo_repo     TYPE REF TO lcl_repo.


    lo_repo = lcl_app=>repo_srv( )->get( iv_key ).

    lv_question =  'This will rebuild and overwrite local repo checksums.'.

    IF lo_repo->is_offline( ) = abap_false.
      lv_question = lv_question
                && ' The logic: if local and remote file differs then:'
                && ' if remote branch is ahead then assume changes are remote,'
                && ' else (branches are equal) assume changes are local.'
                && ' This will lead to incorrect state for files changed on both sides.'
                && ' Please make sure you don''t have ones like that.'.
    ENDIF.

    lv_answer = lcl_popups=>popup_to_confirm(
      titlebar              = 'Warning'
      text_question         = lv_question
      text_button_1         = 'OK'
      icon_button_1         = 'ICON_DELETE'
      text_button_2         = 'Cancel'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false ).                 "#EC NOTEXT

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    lo_repo->rebuild_local_checksums( ).

  ENDMETHOD.  "refresh_local_checksums

  METHOD toggle_favorite.

    lcl_app=>user( )->toggle_favorite( iv_key ).

  ENDMETHOD.  " toggle_favorite.

  METHOD open_se80.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation       = 'SHOW'
        in_new_window   = 'X'
        object_name     = iv_package
        object_type     = 'DEVC'
        with_objectlist = 'X'.

  ENDMETHOD.  " open_se80.

ENDCLASS. "lcl_services_repo