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

ENDCLASS. "lcl_services_repo

CLASS lcl_services_repo IMPLEMENTATION.

  METHOD clone.

    DATA: lo_repo TYPE REF TO lcl_repo_online,
          ls_popup TYPE lcl_popups=>ty_popup.


    ls_popup = lcl_popups=>repo_popup( iv_url ).
    IF ls_popup-cancel = abap_true.
      RAISE EXCEPTION TYPE lcx_cancel.
    ENDIF.

    lo_repo = lcl_app=>repo_srv( )->new_online(
      iv_url         = ls_popup-url
      iv_branch_name = ls_popup-branch_name
      iv_package     = ls_popup-package ).
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
      display_cancel_button = abap_false
    ).  "#EC NOTEXT

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
        display_cancel_button = abap_false
      ).  "#EC NOTEXT

      IF lv_answer = '2'.
        RAISE EXCEPTION TYPE lcx_cancel.
      ENDIF.

      lcl_objects=>delete( lt_tadir ).

    ENDIF.

    lcl_app=>repo_srv( )->delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.  "purge

ENDCLASS. "lcl_services_repo