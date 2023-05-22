CLASS zcl_abapgit_services_repo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS new_online
      IMPORTING
        !is_repo_params TYPE zif_abapgit_services_repo=>ty_repo_params
      RETURNING
        VALUE(ro_repo)  TYPE REF TO zcl_abapgit_repo_online
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS refresh
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS remove
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS purge
      IMPORTING
        !iv_key       TYPE zif_abapgit_persistence=>ty_repo-key
      RETURNING
        VALUE(ri_log) TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS new_offline
      IMPORTING
        !is_repo_params TYPE zif_abapgit_services_repo=>ty_repo_params
      RETURNING
        VALUE(ro_repo)  TYPE REF TO zcl_abapgit_repo_offline
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS refresh_local_checksums
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS toggle_favorite
      IMPORTING
        !iv_key TYPE zif_abapgit_persistence=>ty_repo-key
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS transport_to_branch
      IMPORTING
        !iv_repository_key TYPE zif_abapgit_persistence=>ty_value
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS gui_deserialize
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS activate_objects
      IMPORTING
        !iv_key       TYPE zif_abapgit_persistence=>ty_repo-key
      RETURNING
        VALUE(ri_log) TYPE REF TO zif_abapgit_log
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS delete_unnecessary_objects
      IMPORTING
        !io_repo   TYPE REF TO zcl_abapgit_repo
        !ii_log    TYPE REF TO zif_abapgit_log
        !is_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS popup_decisions
      IMPORTING
        !io_repo   TYPE REF TO zcl_abapgit_repo
      CHANGING
        !cs_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks
      RAISING
        zcx_abapgit_cancel
        zcx_abapgit_exception .
    CLASS-METHODS popup_overwrite
      CHANGING
        !ct_overwrite TYPE zif_abapgit_definitions=>ty_overwrite_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS popup_package_overwrite
      CHANGING
        !ct_overwrite TYPE zif_abapgit_definitions=>ty_overwrite_tt
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS check_package
      IMPORTING
        !is_repo_params TYPE zif_abapgit_services_repo=>ty_repo_params
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_services_repo IMPLEMENTATION.


  METHOD activate_objects.

    DATA:
      lo_repo       TYPE REF TO zcl_abapgit_repo,
      lo_browser    TYPE REF TO zcl_abapgit_repo_content_list,
      lt_repo_items TYPE zif_abapgit_definitions=>ty_repo_item_tt,
      lv_count      TYPE i,
      lv_message    TYPE string.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF lt_repo_items.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    CREATE OBJECT lo_browser
      EXPORTING
        io_repo = lo_repo.

    lt_repo_items = lo_browser->list( '/' ).

    ri_log = lo_repo->create_new_log( 'Activation Log' ).

    " Add all inactive objects to activation queue
    zcl_abapgit_objects_activation=>clear( ).

    LOOP AT lt_repo_items ASSIGNING <ls_item> WHERE inactive = abap_true.
      zcl_abapgit_objects_activation=>add(
        iv_type = <ls_item>-obj_type
        iv_name = <ls_item>-obj_name ).
      lv_count = lv_count + 1.
    ENDLOOP.

    IF lv_count = 0.
      MESSAGE 'No inactive objects found' TYPE 'S'.
      RETURN.
    ENDIF.

    " Activate DDIC + non-DDIC
    zcl_abapgit_objects_activation=>activate(
      iv_ddic = abap_true
      ii_log  = ri_log ).

    zcl_abapgit_objects_activation=>activate(
      iv_ddic = abap_false
      ii_log  = ri_log ).

    IF ri_log->get_status( ) <> zif_abapgit_log=>c_status-error.
      lv_message = |Successfully activated { lv_count } objects|.
      MESSAGE lv_message TYPE 'S'.
    ENDIF.

    lo_repo->refresh( iv_drop_log = abap_false ).

  ENDMETHOD.


  METHOD check_package.

    DATA:
      li_repo     TYPE REF TO zif_abapgit_repo,
      li_repo_srv TYPE REF TO zif_abapgit_repo_srv,
      lv_reason   TYPE string.

    " make sure package is not already in use for a different repository
    " 702: chaining calls with exp&imp parameters causes syntax error
    li_repo_srv = zcl_abapgit_repo_srv=>get_instance( ).
    li_repo_srv->get_repo_from_package(
      EXPORTING
        iv_package    = is_repo_params-package
        iv_ign_subpkg = is_repo_params-ignore_subpackages
      IMPORTING
        ei_repo    = li_repo
        ev_reason  = lv_reason ).

    IF li_repo IS BOUND.
      zcx_abapgit_exception=>raise( lv_reason ).
    ENDIF.

  ENDMETHOD.


  METHOD delete_unnecessary_objects.

    DATA:
      ls_checks TYPE zif_abapgit_definitions=>ty_delete_checks,
      ls_tadir  TYPE zif_abapgit_definitions=>ty_tadir,
      lt_tadir  TYPE zif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS <ls_overwrite> LIKE LINE OF is_checks-overwrite.

    " get confirmed deletions
    LOOP AT is_checks-overwrite ASSIGNING <ls_overwrite>
      WHERE ( action = zif_abapgit_objects=>c_deserialize_action-delete
      OR action = zif_abapgit_objects=>c_deserialize_action-delete_add )
      AND decision = zif_abapgit_definitions=>c_yes.

      ls_tadir-pgmid    = 'R3TR'.
      ls_tadir-object   = <ls_overwrite>-obj_type.
      ls_tadir-obj_name = <ls_overwrite>-obj_name.
      ls_tadir-devclass = <ls_overwrite>-devclass.
      INSERT ls_tadir INTO TABLE lt_tadir.

    ENDLOOP.

    " todo, check if object type supports deletion of parts to avoid deleting complete object

    " delete objects
    IF lines( lt_tadir ) > 0.
      ls_checks-transport = is_checks-transport.

      zcl_abapgit_objects=>delete( it_tadir  = lt_tadir
                                   is_checks = ls_checks
                                   ii_log    = ii_log ).

      io_repo->refresh( iv_drop_log = abap_false ).
    ENDIF.

  ENDMETHOD.


  METHOD gui_deserialize.

    DATA:
      lv_msg    TYPE string,
      ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks,
      li_log    TYPE REF TO zif_abapgit_log.

    " find troublesome objects
    ls_checks = io_repo->deserialize_checks( ).

    IF ls_checks-overwrite IS INITIAL.
      zcx_abapgit_exception=>raise(
        'There is nothing to pull. The local state completely matches the remote repository.' ).
    ENDIF.

    " let the user decide what to do
    TRY.
        popup_decisions(
          EXPORTING
            io_repo   = io_repo
          CHANGING
            cs_checks = ls_checks ).

      CATCH zcx_abapgit_cancel.
        RETURN.
    ENDTRY.

    li_log = io_repo->create_new_log( 'Pull Log' ).

    " pass decisions to delete
    delete_unnecessary_objects(
      io_repo   = io_repo
      is_checks = ls_checks
      ii_log    = li_log ).

    " pass decisions to deserialize
    io_repo->deserialize(
      is_checks = ls_checks
      ii_log    = li_log ).

    IF li_log->get_status( ) = zif_abapgit_log=>c_status-ok.
      lv_msg = |Repository { io_repo->get_name( ) } successfully pulled for package { io_repo->get_package( ) }|.
      MESSAGE lv_msg TYPE 'S'.
    ENDIF.

  ENDMETHOD.


  METHOD new_offline.

    check_package( is_repo_params ).

    " create new repo and add to favorites
    ro_repo ?= zcl_abapgit_repo_srv=>get_instance( )->new_offline(
      iv_url            = is_repo_params-url
      iv_package        = is_repo_params-package
      iv_folder_logic   = is_repo_params-folder_logic
      iv_labels         = is_repo_params-labels
      iv_main_lang_only = is_repo_params-main_lang_only ).

    " Make sure there're no leftovers from previous repos
    ro_repo->zif_abapgit_repo~checksums( )->rebuild( ).
    ro_repo->reset_status( ). " TODO refactor later

    toggle_favorite( ro_repo->get_key( ) ).

    " Set default repo for user
    zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( ro_repo->get_key( ) ).

    COMMIT WORK AND WAIT.


  ENDMETHOD.


  METHOD new_online.

    check_package( is_repo_params ).

    ro_repo ?= zcl_abapgit_repo_srv=>get_instance( )->new_online(
      iv_url            = is_repo_params-url
      iv_branch_name    = is_repo_params-branch_name
      iv_package        = is_repo_params-package
      iv_display_name   = is_repo_params-display_name
      iv_folder_logic   = is_repo_params-folder_logic
      iv_labels         = is_repo_params-labels
      iv_ign_subpkg     = is_repo_params-ignore_subpackages
      iv_main_lang_only = is_repo_params-main_lang_only ).

    " Make sure there're no leftovers from previous repos
    ro_repo->zif_abapgit_repo~checksums( )->rebuild( ).
    ro_repo->reset_status( ). " TODO refactor later

    toggle_favorite( ro_repo->get_key( ) ).

    " Set default repo for user
    zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( ro_repo->get_key( ) ).

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD popup_decisions.

    DATA:
      lt_decision     TYPE zif_abapgit_definitions=>ty_overwrite_tt,
      lt_requirements TYPE zif_abapgit_dot_abapgit=>ty_requirement_tt,
      lt_dependencies TYPE zif_abapgit_apack_definitions=>ty_dependencies.

    FIELD-SYMBOLS:
      <ls_overwrite> LIKE LINE OF cs_checks-overwrite,
      <ls_decision>  LIKE LINE OF lt_decision.

    lt_decision = cs_checks-overwrite.

    " If there's a new namespace, it has to be pulled before all other objects
    READ TABLE lt_decision ASSIGNING <ls_decision> WITH KEY obj_type = 'NSPC'.
    IF sy-subrc = 0 AND <ls_decision>-action = zif_abapgit_objects=>c_deserialize_action-add.
      <ls_decision>-decision = zif_abapgit_definitions=>c_yes.
    ELSE.
      " Set all new objects to YES
      LOOP AT lt_decision ASSIGNING <ls_decision> WHERE action = zif_abapgit_objects=>c_deserialize_action-add.
        <ls_decision>-decision = zif_abapgit_definitions=>c_yes.
      ENDLOOP.
    ENDIF.

    " Ask user what to do
    popup_overwrite( CHANGING ct_overwrite = lt_decision ).
    popup_package_overwrite( CHANGING ct_overwrite = cs_checks-warning_package ).

    IF cs_checks-requirements-met = zif_abapgit_definitions=>c_no.
      lt_requirements = io_repo->get_dot_abapgit( )->get_data( )-requirements.
      zcl_abapgit_requirement_helper=>requirements_popup( lt_requirements ).
      cs_checks-requirements-decision = zif_abapgit_definitions=>c_yes.
    ENDIF.

    IF cs_checks-dependencies-met = zif_abapgit_definitions=>c_no.
      lt_dependencies = io_repo->get_dot_apack( )->get_manifest_descriptor( )-dependencies.
      zcl_abapgit_apack_helper=>dependencies_popup( lt_dependencies ).
    ENDIF.

    IF  cs_checks-transport-required = abap_true
    AND cs_checks-transport-transport IS INITIAL.
      cs_checks-transport-transport = zcl_abapgit_ui_factory=>get_popups( )->popup_transport_request(
        is_transport_type = cs_checks-transport-type ).
    ENDIF.

    " Update decisions
    LOOP AT cs_checks-overwrite ASSIGNING <ls_overwrite>.
      READ TABLE lt_decision ASSIGNING <ls_decision> WITH KEY object_type_and_name COMPONENTS
        obj_type = <ls_overwrite>-obj_type
        obj_name = <ls_overwrite>-obj_name.
      ASSERT sy-subrc = 0.
      <ls_overwrite>-decision = <ls_decision>-decision.
    ENDLOOP.

  ENDMETHOD.


  METHOD popup_overwrite.

    DATA: lt_columns  TYPE zif_abapgit_popups=>ty_alv_column_tt,
          lt_selected LIKE ct_overwrite,
          li_popups   TYPE REF TO zif_abapgit_popups.
    DATA lt_preselected_rows TYPE zif_abapgit_popups=>ty_rows.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF ct_overwrite,
                   <ls_column>    TYPE zif_abapgit_popups=>ty_alv_column.


    IF lines( ct_overwrite ) = 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_TYPE'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_NAME'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'DEVCLASS'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'STATE'.
    <ls_column>-text = 'State'.
    <ls_column>-length = 3.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'ICON'.
    <ls_column>-text = 'Action'.
    <ls_column>-show_icon = abap_true.
    <ls_column>-length = 5.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'TEXT'.
    <ls_column>-text = 'Description'.

    LOOP AT ct_overwrite ASSIGNING <ls_overwrite> WHERE decision = zif_abapgit_definitions=>c_yes.
      INSERT sy-tabix INTO TABLE lt_preselected_rows.
    ENDLOOP.

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).
    li_popups->popup_to_select_from_list(
      EXPORTING
        it_list               = ct_overwrite
        iv_header_text        = |The following objects are different between local and remote repository.|
                             && | Select the objects which should be brought in line with the remote version.|
        iv_select_column_text = 'Change?'
        it_columns_to_display = lt_columns
        it_preselected_rows   = lt_preselected_rows
      IMPORTING
        et_list               = lt_selected ).

    LOOP AT ct_overwrite ASSIGNING <ls_overwrite>.
      READ TABLE lt_selected WITH TABLE KEY object_type_and_name
                             COMPONENTS obj_type = <ls_overwrite>-obj_type
                                        obj_name = <ls_overwrite>-obj_name
                             TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        <ls_overwrite>-decision = zif_abapgit_definitions=>c_yes.
      ELSE.
        <ls_overwrite>-decision = zif_abapgit_definitions=>c_no.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD popup_package_overwrite.

    DATA: lt_columns  TYPE zif_abapgit_popups=>ty_alv_column_tt,
          lt_selected LIKE ct_overwrite,
          li_popups   TYPE REF TO zif_abapgit_popups.

    FIELD-SYMBOLS: <ls_overwrite> LIKE LINE OF ct_overwrite,
                   <ls_column>    TYPE zif_abapgit_popups=>ty_alv_column.

    IF lines( ct_overwrite ) = 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_TYPE'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'OBJ_NAME'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'DEVCLASS'.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'STATE'.
    <ls_column>-text = 'State'.
    <ls_column>-length = 3.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'ICON'.
    <ls_column>-text = 'Action'.
    <ls_column>-show_icon = abap_true.
    <ls_column>-length = 5.
    APPEND INITIAL LINE TO lt_columns ASSIGNING <ls_column>.
    <ls_column>-name = 'TEXT'.
    <ls_column>-text = 'Description'.

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
        <ls_overwrite>-decision = zif_abapgit_definitions=>c_yes.
      ELSE.
        <ls_overwrite>-decision = zif_abapgit_definitions=>c_no.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD purge.

    DATA: lt_tadir     TYPE zif_abapgit_definitions=>ty_tadir_tt,
          lv_answer    TYPE c LENGTH 1,
          lo_repo      TYPE REF TO zcl_abapgit_repo,
          lv_package   TYPE devclass,
          lv_question  TYPE c LENGTH 100,
          ls_checks    TYPE zif_abapgit_definitions=>ty_delete_checks,
          lv_repo_name TYPE string,
          lv_message   TYPE string.


    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lv_repo_name = lo_repo->get_name( ).

    lv_package = lo_repo->get_package( ).
    lt_tadir   = zcl_abapgit_factory=>get_tadir( )->read( lv_package ).

    IF lines( lt_tadir ) > 0.

      lv_question = |This will DELETE all objects in package { lv_package
        } including subpackages ({ lines( lt_tadir ) } objects) from the system|.

      lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
        iv_titlebar              = 'Uninstall'
        iv_text_question         = lv_question
        iv_text_button_1         = 'Delete'
        iv_icon_button_1         = 'ICON_DELETE'
        iv_text_button_2         = 'Cancel'
        iv_icon_button_2         = 'ICON_CANCEL'
        iv_default_button        = '2'
        iv_display_cancel_button = abap_false ).

      IF lv_answer = '2'.
        RAISE EXCEPTION TYPE zcx_abapgit_cancel.
      ENDIF.

    ENDIF.

    ls_checks = lo_repo->delete_checks( ).
    IF ls_checks-transport-required = abap_true.
      ls_checks-transport-transport = zcl_abapgit_ui_factory=>get_popups(
                                        )->popup_transport_request( ls_checks-transport-type ).
    ENDIF.

    ri_log = zcl_abapgit_repo_srv=>get_instance( )->purge(
      ii_repo   = lo_repo
      is_checks = ls_checks ).

    COMMIT WORK.

    IF ri_log IS BOUND AND ri_log->get_status( ) = zif_abapgit_log=>c_status-error.
      zcl_abapgit_log_viewer=>show_log( ri_log ).
      RETURN.
    ENDIF.

    lv_message = |Repository { lv_repo_name } successfully uninstalled from Package { lv_package }. |.
    MESSAGE lv_message TYPE 'S'.

  ENDMETHOD.


  METHOD refresh.

    zcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->refresh( ).

  ENDMETHOD.


  METHOD refresh_local_checksums.

    DATA: lv_answer   TYPE c,
          lv_question TYPE string,
          lo_repo     TYPE REF TO zcl_abapgit_repo.


    IF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>c_authorization-update_local_checksum ) = abap_false.
      zcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

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
      iv_display_cancel_button = abap_false ).

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    lo_repo->zif_abapgit_repo~checksums( )->rebuild( ).
    lo_repo->reset_status( ). " TODO refactor later

    COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD remove.

    DATA: lv_answer    TYPE c LENGTH 1,
          li_repo      TYPE REF TO zif_abapgit_repo,
          lv_package   TYPE devclass,
          lv_question  TYPE c LENGTH 200,
          lv_repo_name TYPE string,
          lv_message   TYPE string.


    li_repo      = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lv_repo_name = li_repo->get_name( ).
    lv_package   = li_repo->get_package( ).
    lv_question  = |This will remove the repository reference to the package { lv_package
      }. All objects will safely remain in the system.|.

    lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
      iv_titlebar              = 'Remove'
      iv_text_question         = lv_question
      iv_text_button_1         = 'Remove'
      iv_icon_button_1         = 'ICON_WF_UNLINK'
      iv_text_button_2         = 'Cancel'
      iv_icon_button_2         = 'ICON_CANCEL'
      iv_default_button        = '2'
      iv_display_cancel_button = abap_false ).

    IF lv_answer = '2'.
      RAISE EXCEPTION TYPE zcx_abapgit_cancel.
    ENDIF.

    zcl_abapgit_repo_srv=>get_instance( )->delete( li_repo ).

    COMMIT WORK.

    lv_message = |Reference to repository { lv_repo_name } successfully removed from Package { lv_package }. |.
    MESSAGE lv_message TYPE 'S'.

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


    IF zcl_abapgit_auth=>is_allowed( zif_abapgit_auth=>c_authorization-transport_to_branch ) = abap_false.
      zcx_abapgit_exception=>raise( 'Not authorized' ).
    ENDIF.

    lo_repository ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_repository_key ).

    lt_transport_headers = zcl_abapgit_ui_factory=>get_popups( )->popup_to_select_transports( ).
    " Also include deleted objects that are included in transport
    lt_transport_objects = zcl_abapgit_transport=>to_tadir(
      it_transport_headers = lt_transport_headers
      iv_deleted_objects   = abap_true ).
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
