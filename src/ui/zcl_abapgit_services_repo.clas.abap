CLASS zcl_abapgit_services_repo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS new_online
      IMPORTING
        !iv_url        TYPE string
      RETURNING
        VALUE(ro_repo) TYPE REF TO zcl_abapgit_repo_online
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS refresh
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS remove
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS purge
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS new_offline
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS remote_attach
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS remote_detach
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS remote_change
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS refresh_local_checksums
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS toggle_favorite
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS open_se80
      IMPORTING
        !iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS transport_to_branch
      IMPORTING
        !iv_repository_key TYPE zif_abapgit_persistence=>ty_value
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS gui_deserialize
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    CLASS-METHODS popup_overwrite
      CHANGING
        !ct_overwrite TYPE zif_abapgit_definitions=>ty_overwrite_tt
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
    CLASS-METHODS popup_package_overwrite
      CHANGING
        !ct_overwrite TYPE zif_abapgit_definitions=>ty_overwrite_tt
      RAISING
        zcx_abapgit_exception
        zcx_abapgit_cancel .
ENDCLASS.



CLASS ZCL_ABAPGIT_SERVICES_REPO IMPLEMENTATION.


  METHOD gui_deserialize.

    DATA: ls_checks       TYPE zif_abapgit_definitions=>ty_deserialize_checks,
          lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt.


* find troublesome objects
    ls_checks = io_repo->deserialize_checks( ).

* and let the user decide what to do
    TRY.
        popup_overwrite( CHANGING ct_overwrite = ls_checks-overwrite ).
        popup_package_overwrite( CHANGING ct_overwrite = ls_checks-warning_package ).

        IF ls_checks-requirements-met = 'N'.
          lt_requirements = io_repo->get_dot_abapgit( )->get_data( )-requirements.
          zcl_abapgit_requirement_helper=>requirements_popup( lt_requirements ).
          ls_checks-requirements-decision = 'Y'.
        ENDIF.

        IF ls_checks-transport-required = abap_true.
          ls_checks-transport-transport = zcl_abapgit_popups=>popup_transport_request( ).
        ENDIF.

      CATCH zcx_abapgit_cancel.
        RETURN.
    ENDTRY.

* and pass decisions to deserialize
    io_repo->deserialize( ls_checks ).

  ENDMETHOD.


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


  METHOD new_online.

    DATA: ls_popup TYPE zcl_abapgit_popups=>ty_popup.


    ls_popup = zcl_abapgit_popups=>repo_popup( iv_url ).
    IF ls_popup-cancel = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    ro_repo = zcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url         = ls_popup-url
      iv_branch_name = ls_popup-branch_name
      iv_package     = ls_popup-package ).

    toggle_favorite( ro_repo->get_key( ) ).

* Set default repo for user
    zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( ro_repo->get_key( ) ).

    COMMIT WORK.

  ENDMETHOD.


  METHOD open_se80.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation       = 'SHOW'
        in_new_window   = abap_true
        object_name     = iv_package
        object_type     = 'DEVC'
        with_objectlist = abap_true.

  ENDMETHOD.  " open_se80.


  METHOD popup_overwrite.

    DATA: lt_columns  TYPE stringtab,
          lt_selected LIKE ct_overwrite,
          lv_column   LIKE LINE OF lt_columns.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF ct_overwrite.


    IF lines( ct_overwrite ) = 0.
      RETURN.
    ENDIF.

    lv_column = 'OBJ_TYPE'.
    INSERT lv_column INTO TABLE lt_columns.
    lv_column = 'OBJ_NAME'.
    INSERT lv_column INTO TABLE lt_columns.

    zcl_abapgit_popups=>popup_to_select_from_list(
      EXPORTING
        it_list               = ct_overwrite
        i_header_text         = |The following Objects have been modified locally.|
                            && | Select the Objects which should be overwritten.|
        i_select_column_text  = 'Overwrite?'
        it_columns_to_display = lt_columns
      IMPORTING
        et_list               = lt_selected ).

    LOOP AT ct_overwrite ASSIGNING <ls_overwrite>.
      READ TABLE lt_selected WITH KEY
        obj_type = <ls_overwrite>-obj_type
        obj_name = <ls_overwrite>-obj_name
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        <ls_overwrite>-decision = 'Y'.
      ELSE.
        <ls_overwrite>-decision = 'N'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD popup_package_overwrite.

    DATA: lv_question TYPE c LENGTH 200,
          lv_answer   TYPE c.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF ct_overwrite.


    IF lines( ct_overwrite ) = 0.
      RETURN.
    ENDIF.

    LOOP AT ct_overwrite ASSIGNING <ls_overwrite>.
      CONCATENATE 'Overwrite object' <ls_overwrite>-obj_type <ls_overwrite>-obj_name
        'from package' <ls_overwrite>-devclass
        INTO lv_question SEPARATED BY space.                "#EC NOTEXT

      lv_answer = zcl_abapgit_popups=>popup_to_confirm(
        titlebar              = 'Warning'
        text_question         = lv_question
        text_button_1         = 'Ok'
        icon_button_1         = 'ICON_DELETE'
        text_button_2         = 'Cancel'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false ).               "#EC NOTEXT

      IF lv_answer = '2'.
        RAISE EXCEPTION TYPE zcx_abapgit_cancel.
      ENDIF.

* todo, let the user decide yes/no/cancel
      <ls_overwrite>-decision = 'Y'.

    ENDLOOP.

  ENDMETHOD.


  METHOD purge.

    DATA: lt_tadir    TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lv_answer   TYPE c LENGTH 1,
          lo_repo     TYPE REF TO zcl_abapgit_repo,
          lv_package  TYPE devclass,
          lv_question TYPE c LENGTH 100.


    lo_repo = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    lv_package = lo_repo->get_package( ).
    lt_tadir   = zcl_abapgit_tadir=>read( lv_package ).

    IF lines( lt_tadir ) > 0.

      lv_question = |This will DELETE all objects in package { lv_package
        } ({ lines( lt_tadir ) } objects) from the system|. "#EC NOTEXT

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

    ENDIF.

    zcl_abapgit_repo_srv=>get_instance( )->purge( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.


  METHOD refresh.

    zcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->refresh( ).

  ENDMETHOD.  "refresh


  METHOD refresh_local_checksums.

    DATA: lv_answer   TYPE c,
          lv_question TYPE string,
          lo_repo     TYPE REF TO zcl_abapgit_repo.


    IF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>gc_authorization-update_local_checksum ) = abap_false.
      zcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

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

    zcl_abapgit_repo_srv=>get_instance( )->switch_repo_type(
      iv_key = iv_key
      iv_offline = abap_false ).

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

  ENDMETHOD.


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

  ENDMETHOD.


  METHOD remove.

    DATA: lv_answer   TYPE c LENGTH 1,
          lo_repo     TYPE REF TO zcl_abapgit_repo,
          lv_package  TYPE devclass,
          lv_question TYPE c LENGTH 200.


    lo_repo     = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lv_package  = lo_repo->get_package( ).
    lv_question = |This will remove the repository reference to the package { lv_package
      }. All objects will safely remain in the system.|.

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

  ENDMETHOD.


  METHOD toggle_favorite.

    zcl_abapgit_persistence_user=>get_instance( )->toggle_favorite( iv_key ).

  ENDMETHOD.


  METHOD transport_to_branch.

    DATA:
      lo_repository          TYPE REF TO zcl_abapgit_repo_online,
      lo_transport_to_branch TYPE REF TO zcl_abapgit_transport_2_branch,
      lt_transport_headers   TYPE trwbo_request_headers,
      lt_transport_objects   TYPE scts_tadir,
      ls_transport_to_branch TYPE zif_abapgit_definitions=>ty_transport_to_branch.


    IF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>gc_authorization-transport_to_branch ) = abap_false.
      zcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

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
