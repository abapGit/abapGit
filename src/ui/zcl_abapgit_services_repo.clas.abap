CLASS zcl_abapgit_services_repo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS new_online
      IMPORTING
        !is_repo_params TYPE zif_abapgit_services_repo=>ty_repo_params
      RETURNING
        VALUE(ro_repo) TYPE REF TO zcl_abapgit_repo_online
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS refresh
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS remove
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS purge
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS new_offline
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS remote_attach
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS remote_detach
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS remote_change
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS refresh_local_checksums
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS toggle_favorite
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS transport_to_branch
      IMPORTING
        !iv_repository_key TYPE zif_abapgit_persistence=>ty_value
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS gui_deserialize
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS popup_overwrite
      CHANGING
        !ct_overwrite TYPE zif_abapgit_definitions=>ty_overwrite_tt
      RAISING
        zcx_abapgit_exception.
    CLASS-METHODS popup_package_overwrite
      CHANGING
        !ct_overwrite TYPE zif_abapgit_definitions=>ty_overwrite_tt
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS zcl_abapgit_services_repo IMPLEMENTATION.


  METHOD gui_deserialize.

    DATA: ls_checks       TYPE zif_abapgit_definitions=>ty_deserialize_checks,
          lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
          lt_dependencies TYPE zif_abapgit_apack_definitions=>tt_dependencies.


* find troublesome objects
    ls_checks = io_repo->deserialize_checks( ).

* and let the user decide what to do
    TRY.
        popup_overwrite( CHANGING ct_overwrite = ls_checks-overwrite ).
        popup_package_overwrite( CHANGING ct_overwrite = ls_checks-warning_package ).

        IF ls_checks-requirements-met = zif_abapgit_definitions=>gc_no.
          lt_requirements = io_repo->get_dot_abapgit( )->get_data( )-requirements.
          zcl_abapgit_requirement_helper=>requirements_popup( lt_requirements ).
          ls_checks-requirements-decision = zif_abapgit_definitions=>gc_yes.
        ENDIF.

        IF ls_checks-dependencies-met = zif_abapgit_definitions=>gc_no.
          lt_dependencies = io_repo->get_dot_apack( )->get_manifest_descriptor( )-dependencies.
          zcl_abapgit_apack_helper=>dependencies_popup( lt_dependencies ).
        ENDIF.

        IF ls_checks-transport-required = abap_true.
          ls_checks-transport-transport = zcl_abapgit_ui_factory=>get_popups( )->popup_transport_request(
            is_transport_type = ls_checks-transport-type ).
        ENDIF.

      CATCH zcx_abapgit_cancel.
        RETURN.
    ENDTRY.

* and pass decisions to deserialize
    io_repo->deserialize( is_checks = ls_checks
                          ii_log    = io_repo->create_new_log( 'Pull Log' ) ).

  ENDMETHOD.


  METHOD new_offline.

    DATA: ls_popup        TYPE zif_abapgit_popups=>ty_popup,
          lo_repo         TYPE REF TO zcl_abapgit_repo,
          lo_repo_offline TYPE REF TO zcl_abapgit_repo_offline,
          li_repo_srv     TYPE REF TO zif_abapgit_repo_srv,
          lv_reason       TYPE string.

    ls_popup  = zcl_abapgit_ui_factory=>get_popups( )->repo_new_offline( ).
    IF ls_popup-cancel = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    " make sure package is not already in use for a different repository
    " 702: chaining calls with exp&imp parameters causes syntax error
    li_repo_srv = zcl_abapgit_repo_srv=>get_instance( ).
    li_repo_srv->get_repo_from_package(
      EXPORTING
        iv_package = ls_popup-package
      IMPORTING
        eo_repo    = lo_repo
        ev_reason  = lv_reason ).

    IF lo_repo IS BOUND.
      MESSAGE lv_reason TYPE 'S'.
    ELSE.
      " create new repo and add to favorites
      lo_repo_offline = zcl_abapgit_repo_srv=>get_instance( )->new_offline(
        iv_url          = ls_popup-url
        iv_package      = ls_popup-package
        iv_folder_logic = ls_popup-folder_logic
        iv_master_lang_only = ls_popup-master_lang_only ).

      lo_repo_offline->rebuild_local_checksums( ).

      lo_repo ?= lo_repo_offline.

      toggle_favorite( lo_repo->get_key( ) ).
    ENDIF.

    " Set default repo for user
    zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lo_repo->get_key( ) ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD new_online.

    DATA:
          lo_repo     TYPE REF TO zcl_abapgit_repo,
          li_repo_srv TYPE REF TO zif_abapgit_repo_srv,
          lv_reason   TYPE string.

    " make sure package is not already in use for a different repository
    " 702: chaining calls with exp&imp parameters causes syntax error
    li_repo_srv = zcl_abapgit_repo_srv=>get_instance( ).
    li_repo_srv->get_repo_from_package(
      EXPORTING
        iv_package = is_repo_params-package
      IMPORTING
        eo_repo    = lo_repo
        ev_reason  = lv_reason ).

    IF lo_repo IS BOUND.
      zcx_abapgit_exception=>raise( lv_reason ).
    ENDIF.

    ro_repo = zcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url              = is_repo_params-url
      iv_branch_name      = is_repo_params-branch_name
      iv_package          = is_repo_params-package
      iv_display_name     = is_repo_params-display_name
      iv_folder_logic     = is_repo_params-folder_logic
      iv_ign_subpkg       = is_repo_params-ignore_subpackages
      iv_master_lang_only = is_repo_params-master_lang_only ).

    toggle_favorite( ro_repo->get_key( ) ).

    " Set default repo for user
    zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( ro_repo->get_key( ) ).

    COMMIT WORK.

  ENDMETHOD.


  METHOD popup_overwrite.

    DATA: lt_columns  TYPE zif_abapgit_definitions=>ty_alv_column_tt,
          lt_selected LIKE ct_overwrite,
          li_popups   TYPE REF TO zif_abapgit_popups.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF ct_overwrite,
                   <ls_column>    TYPE zif_abapgit_definitions=>ty_alv_column.


    IF lines( ct_overwrite ) = 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_TYPE'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_NAME'.

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).
    li_popups->popup_to_select_from_list(
      EXPORTING
        it_list               = ct_overwrite
        iv_header_text        = |The following objects have been modified locally.|
                             && | Select the objects which should be overwritten.|
        iv_select_column_text = 'Overwrite?'
        it_columns_to_display = lt_columns
      IMPORTING
        et_list               = lt_selected ).

    LOOP AT ct_overwrite ASSIGNING <ls_overwrite>.
      READ TABLE lt_selected WITH TABLE KEY object_type_and_name
                             COMPONENTS obj_type = <ls_overwrite>-obj_type
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

    DATA: lt_columns  TYPE zif_abapgit_definitions=>ty_alv_column_tt,
          lt_selected LIKE ct_overwrite,
          li_popups   TYPE REF TO zif_abapgit_popups.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF ct_overwrite,
                   <ls_column>    TYPE zif_abapgit_definitions=>ty_alv_column.

    IF lines( ct_overwrite ) = 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_TYPE'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_NAME'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'DEVCLASS'.

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).
    li_popups->popup_to_select_from_list(
      EXPORTING
        it_list               = ct_overwrite
        iv_header_text        = |The following objects have been created in other packages.|
                             && | Select the objects which should be overwritten.|
        iv_select_column_text = |Overwrite?|
        it_columns_to_display = lt_columns
      IMPORTING
        et_list               = lt_selected ).

    LOOP AT ct_overwrite ASSIGNING <ls_overwrite>.

      READ TABLE lt_selected WITH TABLE KEY object_type_and_name
                             COMPONENTS obj_type = <ls_overwrite>-obj_type
                                        obj_name = <ls_overwrite>-obj_name
                             TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        <ls_overwrite>-decision = 'Y'.
      ELSE.
        <ls_overwrite>-decision = 'N'.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD purge.

    DATA: lt_tadir    TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lv_answer   TYPE c LENGTH 1,
          lo_repo     TYPE REF TO zcl_abapgit_repo,
          lv_package  TYPE devclass,
          lv_question TYPE c LENGTH 100,
          ls_checks   TYPE zif_abapgit_definitions=>ty_delete_checks.


    lo_repo = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    lv_package = lo_repo->get_package( ).
    lt_tadir   = zcl_abapgit_factory=>get_tadir( )->read( lv_package ).

    IF lines( lt_tadir ) > 0.

      lv_question = |This will DELETE all objects in package { lv_package
        } ({ lines( lt_tadir ) } objects) from the system|. "#EC NOTEXT

      lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
        iv_titlebar              = 'Uninstall'
        iv_text_question         = lv_question
        iv_text_button_1         = 'Delete'
        iv_icon_button_1         = 'ICON_DELETE'
        iv_text_button_2         = 'Cancel'
        iv_icon_button_2         = 'ICON_CANCEL'
        iv_default_button        = '2'
        iv_display_cancel_button = abap_false ).            "#EC NOTEXT

      IF lv_answer = '2'.
        RAISE EXCEPTION TYPE zcx_abapgit_cancel.
      ENDIF.

    ENDIF.

    ls_checks = lo_repo->delete_checks( ).
    IF ls_checks-transport-required = abap_true.
      ls_checks-transport-transport = zcl_abapgit_ui_factory=>get_popups(
                                        )->popup_transport_request( ls_checks-transport-type ).
    ENDIF.

    zcl_abapgit_repo_srv=>get_instance( )->purge( io_repo   = lo_repo
                                                  is_checks = ls_checks ).

    COMMIT WORK.

  ENDMETHOD.


  METHOD refresh.

    zcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->refresh( ).

  ENDMETHOD.


  METHOD refresh_local_checksums.

    DATA: lv_answer   TYPE c,
          lv_question TYPE string,
          lo_repo     TYPE REF TO zcl_abapgit_repo.


    IF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>gc_authorization-update_local_checksum ) = abap_false.
      zcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    lo_repo = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    lv_question = 'This will rebuild and overwrite local repo checksums.'.

    IF lo_repo->is_offline( ) = abap_false.
      lv_question = lv_question
                && ' The logic: if local and remote file differs then:'
                && ' if remote branch is ahead then assume changes are remote,'
                && ' else (branches are equal) assume changes are local.'
                && ' This will lead to incorrect state for files changed on both sides.'
                && ' Please make sure you don''t have ones like that.'.
    ENDIF.

    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Warning'
      iv_text_question         = lv_question
      iv_text_button_1         = 'OK'
      iv_icon_button_1         = 'ICON_DELETE'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).              "#EC NOTEXT

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    lo_repo->rebuild_local_checksums( ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD remote_attach.

    DATA: ls_popup TYPE zif_abapgit_popups=>ty_popup,
          ls_loc   TYPE zif_abapgit_persistence=>ty_repo-local_settings,
          lo_repo  TYPE REF TO zcl_abapgit_repo_online.

    ls_loc = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->get_local_settings( ).

    ls_popup = zcl_abapgit_ui_factory=>get_popups( )->repo_popup(
      iv_title          = 'Attach repo to remote ...'
      iv_url            = ''
      iv_display_name   = ls_loc-display_name
      iv_package        = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->get_package( )
      iv_freeze_package = abap_true ).
    IF ls_popup-cancel = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    zcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->switch_repo_type( iv_offline = abap_false ).
    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lo_repo->set_url( ls_popup-url ).
    lo_repo->set_branch_name( ls_popup-branch_name ).

    ls_loc = lo_repo->get_local_settings( ). " Just in case ... if switch affects LS state
    ls_loc-display_name = ls_popup-display_name.
    lo_repo->set_local_settings( ls_loc ).

    COMMIT WORK.

  ENDMETHOD.


  METHOD remote_change.

    DATA: ls_popup TYPE zif_abapgit_popups=>ty_popup,
          ls_loc   TYPE zif_abapgit_persistence=>ty_repo-local_settings,
          lo_repo  TYPE REF TO zcl_abapgit_repo_online.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    ls_loc = lo_repo->get_local_settings( ).

    ls_popup = zcl_abapgit_ui_factory=>get_popups( )->repo_popup(
      iv_title          = 'Change repo remote ...'
      iv_url            = lo_repo->get_url( )
      iv_package        = lo_repo->get_package( )
      iv_display_name   = ls_loc-display_name
      iv_freeze_package = abap_true ).
    IF ls_popup-cancel = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lo_repo->set_url( ls_popup-url ).
    lo_repo->set_branch_name( ls_popup-branch_name ).

    ls_loc-display_name = ls_popup-display_name.
    lo_repo->set_local_settings( ls_loc ).

    COMMIT WORK.

  ENDMETHOD.


  METHOD remote_detach.

    DATA: lv_answer TYPE c LENGTH 1.

    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Make repository OFF-line'
      iv_text_question         = 'This will detach the repo from remote and make it OFF-line'
      iv_text_button_1         = 'Make OFF-line'
      iv_icon_button_1         = 'ICON_WF_UNLINK'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).              "#EC NOTEXT

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    zcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->switch_repo_type( iv_offline = abap_true ).

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

    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Remove'
      iv_text_question         = lv_question
      iv_text_button_1         = 'Remove'
      iv_icon_button_1         = 'ICON_WF_UNLINK'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).              "#EC NOTEXT

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
      lt_transport_objects   TYPE zif_abapgit_definitions=>ty_tadir_tt,
      ls_transport_to_branch TYPE zif_abapgit_definitions=>ty_transport_to_branch.


    IF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>gc_authorization-transport_to_branch ) = abap_false.
      zcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    lo_repository ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_repository_key ).

    lt_transport_headers = zcl_abapgit_ui_factory=>get_popups( )->popup_to_select_transports( ).
    lt_transport_objects = zcl_abapgit_transport=>to_tadir( lt_transport_headers ).
    IF lt_transport_objects IS INITIAL.
      zcx_abapgit_exception=>raise( 'Canceled or List of objects is empty ' ).
    ENDIF.

    ls_transport_to_branch = zcl_abapgit_ui_factory=>get_popups( )->popup_to_create_transp_branch(
      lt_transport_headers ).

    CREATE OBJECT lo_transport_to_branch.
    lo_transport_to_branch->create(
      io_repository          = lo_repository
      is_transport_to_branch = ls_transport_to_branch
      it_transport_objects   = lt_transport_objects ).

  ENDMETHOD.
ENDCLASS.
