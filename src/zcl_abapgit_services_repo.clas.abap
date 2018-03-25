CLASS zcl_abapgit_services_repo DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS clone
      IMPORTING iv_url TYPE string
      RAISING   zcx_abapgit_exception zcx_abapgit_cancel.

    CLASS-METHODS refresh
      IMPORTING iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS remove
      IMPORTING iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING   zcx_abapgit_exception zcx_abapgit_cancel.

    CLASS-METHODS purge
      IMPORTING iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING   zcx_abapgit_exception zcx_abapgit_cancel.

    CLASS-METHODS new_offline
      RAISING zcx_abapgit_exception zcx_abapgit_cancel.

    CLASS-METHODS remote_attach
      IMPORTING iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING   zcx_abapgit_exception zcx_abapgit_cancel.

    CLASS-METHODS remote_detach
      IMPORTING iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING   zcx_abapgit_exception zcx_abapgit_cancel.

    CLASS-METHODS remote_change
      IMPORTING iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING   zcx_abapgit_exception zcx_abapgit_cancel.

    CLASS-METHODS refresh_local_checksums
      IMPORTING iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING   zcx_abapgit_exception zcx_abapgit_cancel.

    CLASS-METHODS toggle_favorite
      IMPORTING iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS open_se80
      IMPORTING iv_package TYPE devclass
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS transport_to_branch
      IMPORTING iv_repository_key TYPE zif_abapgit_persistence=>ty_value
      RAISING   zcx_abapgit_exception zcx_abapgit_cancel.
ENDCLASS.



CLASS ZCL_ABAPGIT_SERVICES_REPO IMPLEMENTATION.


  METHOD clone.

    DATA: lo_repo  TYPE REF TO zcl_abapgit_repo_online,
          ls_popup TYPE zcl_abapgit_popups=>ty_popup.


    ls_popup = zcl_abapgit_popups=>repo_popup( iv_url ).
    IF ls_popup-cancel = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    lo_repo = zcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url         = ls_popup-url
      iv_branch_name = ls_popup-branch_name
      iv_package     = ls_popup-package ).

    toggle_favorite( lo_repo->get_key( ) ).

    lo_repo->initialize( ).
    lo_repo->find_remote_dot_abapgit( ).
    lo_repo->status( ). " check for errors
    lo_repo->deserialize( ).

    zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lo_repo->get_key( ) ). " Set default repo for user

    COMMIT WORK.

  ENDMETHOD.  "clone


  METHOD new_offline.

    DATA: lo_repo  TYPE REF TO zcl_abapgit_repo,
          ls_popup TYPE zcl_abapgit_popups=>ty_popup.

    ls_popup  = zcl_abapgit_popups=>repo_new_offline( ).
    IF ls_popup-cancel = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    lo_repo = zcl_abapgit_repo_srv=>get_instance( )->new_offline(
      iv_url     = ls_popup-url
      iv_package = ls_popup-package ).

    zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lo_repo->get_key( ) ). " Set default repo for user
    toggle_favorite( lo_repo->get_key( ) ).

    COMMIT WORK.

  ENDMETHOD.  "new_offline


  METHOD open_se80.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation       = 'SHOW'
        in_new_window   = abap_true
        object_name     = iv_package
        object_type     = 'DEVC'
        with_objectlist = abap_true.

  ENDMETHOD.  " open_se80.


  METHOD purge.

    DATA: lt_tadir    TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lv_answer   TYPE c LENGTH 1,
          lo_repo     TYPE REF TO zcl_abapgit_repo,
          lv_package  TYPE devclass,
          lv_question TYPE c LENGTH 100.


    lo_repo = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    IF lo_repo->get_local_settings( )-write_protected = abap_true.
      zcx_abapgit_exception=>raise( 'Cannot purge. Local code is write-protected by repo config' ).
    ENDIF.

    lv_package = lo_repo->get_package( ).
    lt_tadir   = zcl_abapgit_tadir=>read( lv_package ).

    IF lines( lt_tadir ) > 0.

      lv_question = |This will DELETE all objects in package { lv_package }|
                 && | ({ lines( lt_tadir ) } objects) from the system|. "#EC NOTEXT

      lv_answer = zcl_abapgit_popups=>popup_to_confirm(
        titlebar              = 'Uninstall'
        text_question         = lv_question
        text_button_1         = 'Delete'
        icon_button_1         = 'ICON_DELETE'
        text_button_2         = 'Cancel'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false ).               "#EC NOTEXT

      IF lv_answer = '2'.
        RAISE EXCEPTION TYPE zcx_abapgit_cancel.
      ENDIF.

      zcl_abapgit_objects=>delete( lt_tadir ).

    ENDIF.

    zcl_abapgit_repo_srv=>get_instance( )->delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.  "purge


  METHOD refresh.

    zcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->refresh( ).

  ENDMETHOD.  "refresh


  METHOD refresh_local_checksums.

    DATA: lv_answer   TYPE c,
          lv_question TYPE string,
          lo_repo     TYPE REF TO zcl_abapgit_repo.


    lo_repo = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    lv_question =  'This will rebuild and overwrite local repo checksums.'.

    IF lo_repo->is_offline( ) = abap_false.
      lv_question = lv_question
                && ' The logic: if local and remote file differs then:'
                && ' if remote branch is ahead then assume changes are remote,'
                && ' else (branches are equal) assume changes are local.'
                && ' This will lead to incorrect state for files changed on both sides.'
                && ' Please make sure you don''t have ones like that.'.
    ENDIF.

    lv_answer = zcl_abapgit_popups=>popup_to_confirm(
      titlebar              = 'Warning'
      text_question         = lv_question
      text_button_1         = 'OK'
      icon_button_1         = 'ICON_DELETE'
      text_button_2         = 'Cancel'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false ).                 "#EC NOTEXT

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    lo_repo->rebuild_local_checksums( ).

  ENDMETHOD.  "refresh_local_checksums


  METHOD remote_attach.

    DATA: ls_popup TYPE zcl_abapgit_popups=>ty_popup,
          lo_repo  TYPE REF TO zcl_abapgit_repo_online.

    ls_popup = zcl_abapgit_popups=>repo_popup(
      iv_title          = 'Attach repo to remote ...'
      iv_url            = ''
      iv_package        = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->get_package( )
      iv_freeze_package = abap_true ).
    IF ls_popup-cancel = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    zcl_abapgit_repo_srv=>get_instance( )->switch_repo_type( iv_key = iv_key  iv_offline = abap_false ).

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lo_repo->set_url( ls_popup-url ).
    lo_repo->set_branch_name( ls_popup-branch_name ).

    COMMIT WORK.

  ENDMETHOD.  "remote_attach


  METHOD remote_change.

    DATA: ls_popup TYPE zcl_abapgit_popups=>ty_popup,
          lo_repo  TYPE REF TO zcl_abapgit_repo_online.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    ls_popup = zcl_abapgit_popups=>repo_popup(
      iv_title          = 'Change repo remote ...'
      iv_url            = lo_repo->get_url( )
      iv_package        = lo_repo->get_package( )
      iv_freeze_package = abap_true ).
    IF ls_popup-cancel = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lo_repo->set_new_remote( iv_url         = ls_popup-url
                             iv_branch_name = ls_popup-branch_name ).

    COMMIT WORK.

  ENDMETHOD.  "remote_change


  METHOD remote_detach.

    DATA: lv_answer TYPE c LENGTH 1.

    lv_answer = zcl_abapgit_popups=>popup_to_confirm(
      titlebar              = 'Make repository OFF-line'
      text_question         = 'This will detach the repo from remote and make it OFF-line'
      text_button_1         = 'Make OFF-line'
      icon_button_1         = 'ICON_WF_UNLINK'
      text_button_2         = 'Cancel'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false ).                 "#EC NOTEXT

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    zcl_abapgit_repo_srv=>get_instance( )->switch_repo_type( iv_key = iv_key  iv_offline = abap_true ).

    COMMIT WORK.

  ENDMETHOD.  "remote_detach


  METHOD remove.

    DATA: lv_answer   TYPE c LENGTH 1,
          lo_repo     TYPE REF TO zcl_abapgit_repo,
          lv_package  TYPE devclass,
          lv_question TYPE c LENGTH 200.


    lo_repo     = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lv_package  = lo_repo->get_package( ).
    lv_question = |This will remove the repository reference to the package { lv_package }|
               && '. All objects will safely remain in the system.'.

    lv_answer = zcl_abapgit_popups=>popup_to_confirm(
      titlebar              = 'Remove'
      text_question         = lv_question
      text_button_1         = 'Remove'
      icon_button_1         = 'ICON_WF_UNLINK'
      text_button_2         = 'Cancel'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = abap_false ).                 "#EC NOTEXT

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    zcl_abapgit_repo_srv=>get_instance( )->delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.  "remove


  METHOD toggle_favorite.

    zcl_abapgit_persistence_user=>get_instance( )->toggle_favorite( iv_key ).

  ENDMETHOD.  " toggle_favorite.


  METHOD transport_to_branch.
    DATA:
      lo_repository          TYPE REF TO zcl_abapgit_repo_online,
      lo_transport_to_branch TYPE REF TO zcl_abapgit_transport_2_branch,
      lt_transport_headers   TYPE trwbo_request_headers,
      lt_transport_objects   TYPE scts_tadir,
      ls_transport_to_branch TYPE zif_abapgit_definitions=>ty_transport_to_branch.

    lo_repository ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_repository_key ).

    lt_transport_headers = zcl_abapgit_popups=>popup_to_select_transports( ).
    lt_transport_objects = zcl_abapgit_transport=>to_tadir( lt_transport_headers ).
    IF lt_transport_objects IS INITIAL.
      zcx_abapgit_exception=>raise( 'Canceled or List of objects is empty ' ).
    ENDIF.

    ls_transport_to_branch = zcl_abapgit_popups=>popup_to_create_transp_branch(
      lt_transport_headers ).

    CREATE OBJECT lo_transport_to_branch.
    lo_transport_to_branch->create(
      io_repository          = lo_repository
      is_transport_to_branch = ls_transport_to_branch
      it_transport_objects   = lt_transport_objects ).
  ENDMETHOD.
ENDCLASS.
