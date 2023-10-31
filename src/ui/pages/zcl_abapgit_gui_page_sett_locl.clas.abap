CLASS zcl_abapgit_gui_page_sett_locl DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .

    CLASS-METHODS create
      IMPORTING
        !io_repo       TYPE REF TO zcl_abapgit_repo
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      IMPORTING
        !io_repo TYPE REF TO zcl_abapgit_repo
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_popup_picklist TYPE REF TO zcl_abapgit_gui_picklist.

    CONSTANTS:
      BEGIN OF c_id,
        local                        TYPE string VALUE 'local',
        display_name                 TYPE string VALUE 'display_name',
        transport_request            TYPE string VALUE 'transport_request',
        customizing_request          TYPE string VALUE 'customizing_request',
        labels                       TYPE string VALUE 'labels',
        ignore_subpackages           TYPE string VALUE 'ignore_subpackages',
        write_protected              TYPE string VALUE 'write_protected',
        only_local_objects           TYPE string VALUE 'only_local_objects',
        main_language_only           TYPE string VALUE 'main_language_only',
        checks                       TYPE string VALUE 'checks',
        code_inspector_check_variant TYPE string VALUE 'code_inspector_check_variant',
        block_commit                 TYPE string VALUE 'block_commit',
        flow                         TYPE string VALUE 'flow',
      END OF c_id .
    CONSTANTS:
      BEGIN OF c_event,
        save                       TYPE string VALUE 'save',
        choose_transport_request   TYPE string VALUE 'choose_transport_request',
        choose_customizing_request TYPE string VALUE 'choose_customizing_request',
        choose_labels              TYPE string VALUE 'choose-labels',
        choose_check_variant       TYPE string VALUE 'choose_check_variant',
      END OF c_event .
    DATA mo_form TYPE REF TO zcl_abapgit_html_form .
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map .
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map .
    DATA mo_repo TYPE REF TO zcl_abapgit_repo .
    DATA ms_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings .

    METHODS validate_form
      IMPORTING
        !io_form_data            TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception .
    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form
      RAISING
        zcx_abapgit_exception .
    METHODS read_settings
      RETURNING
        VALUE(ro_form_data) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception .
    METHODS save_settings
      RAISING
        zcx_abapgit_exception .
    METHODS choose_labels
      RAISING
        zcx_abapgit_exception .
    METHODS choose_check_variant
      IMPORTING
        iv_is_return TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
    METHODS choose_transport_request
      RAISING
        zcx_abapgit_exception .
    METHODS choose_customizing_request
      RAISING
        zcx_abapgit_exception .
    METHODS is_customizing_included
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    METHODS handle_picklist_state
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_gui_page_sett_locl IMPLEMENTATION.


  METHOD choose_check_variant.

    DATA ls_variant         TYPE zif_abapgit_code_inspector=>ty_variant.
    DATA lv_popup_cancelled TYPE abap_bool.

    IF iv_is_return = abap_false.

      mo_popup_picklist = zcl_abapgit_popup_code_insp=>create(
        )->create_picklist(
        )->set_id( c_event-choose_check_variant
        )->set_in_page( abap_false ).

    ELSE.

      lv_popup_cancelled = mo_popup_picklist->was_cancelled( ).
      IF lv_popup_cancelled = abap_false.
        mo_popup_picklist->get_result_item( CHANGING cs_selected = ls_variant ).
        IF ls_variant IS NOT INITIAL.
          mo_form_data->set(
            iv_key = c_id-code_inspector_check_variant
            iv_val = ls_variant-name ).
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD choose_customizing_request.

    DATA:
      ls_transport_type      TYPE zif_abapgit_definitions=>ty_transport_type,
      lv_customizing_request TYPE trkorr.

    ls_transport_type-request = zif_abapgit_cts_api=>c_transport_type-cust_request.
    ls_transport_type-task    = zif_abapgit_cts_api=>c_transport_type-cust_task.

    lv_customizing_request = zcl_abapgit_ui_factory=>get_popups( )->popup_transport_request( ls_transport_type ).

    IF lv_customizing_request IS NOT INITIAL.
      mo_form_data->set(
        iv_key = c_id-customizing_request
        iv_val = lv_customizing_request ).
    ENDIF.

  ENDMETHOD.


  METHOD choose_labels.

    DATA:
      lv_old_labels TYPE string,
      lv_new_labels TYPE string.

    lv_old_labels = mo_form_data->get( c_id-labels ).

    lv_new_labels = zcl_abapgit_ui_factory=>get_popups( )->popup_to_select_labels( lv_old_labels ).

    mo_form_data->set(
      iv_key = c_id-labels
      iv_val = lv_new_labels ).

  ENDMETHOD.


  METHOD choose_transport_request.

    DATA: lv_transport_request TYPE trkorr.

    lv_transport_request = zcl_abapgit_ui_factory=>get_popups( )->popup_transport_request( ).

    IF lv_transport_request IS NOT INITIAL.
      mo_form_data->set(
        iv_key = c_id-transport_request
        iv_val = lv_transport_request ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_repo = io_repo.
    mo_form = get_form_schema( ).
    mo_form_data = read_settings( ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_sett_locl.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Local Settings & Checks'
      io_page_menu       = zcl_abapgit_gui_menus=>repo_settings(
                             iv_key = io_repo->get_key( )
                             iv_act = zif_abapgit_definitions=>c_action-repo_local_settings )
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD get_form_schema.

    DATA: li_package TYPE REF TO zif_abapgit_sap_package.

    li_package = zcl_abapgit_factory=>get_sap_package( mo_repo->get_package( ) ).

    ro_form = zcl_abapgit_html_form=>create(
      iv_form_id   = 'repo-local-settings-form'
      iv_help_page = 'https://docs.abapgit.org/settings-local.html' ).

    ro_form->start_group(
      iv_name        = c_id-local
      iv_label       = 'Local Settings'
      iv_hint        = 'Settings valid for this system only'
    )->text(
      iv_name        = c_id-display_name
      iv_label       = 'Display Name'
      iv_hint        = 'Name to show instead of original repo name (optional)' ).

    IF li_package->are_changes_recorded_in_tr_req( ) = abap_true.
      ro_form->text(
        iv_name        = c_id-transport_request
        iv_side_action = c_event-choose_transport_request
        iv_label       = |Transport Request|
        iv_hint        = 'Transport request; All changes are recorded therein and no transport popup appears|' ).
    ENDIF.

    IF is_customizing_included( ) = abap_true.
      ro_form->text(
        iv_name        = c_id-customizing_request
        iv_side_action = c_event-choose_customizing_request
        iv_label       = |Customizing Request|
        iv_hint        = 'Customizing request; All changes are recorded therein and no customizing popup appears|' ).
    ENDIF.

    ro_form->text(
      iv_name        = c_id-labels
      iv_side_action = c_event-choose_labels
      iv_label       = |Labels (comma-separated, allowed chars: "{ zcl_abapgit_repo_labels=>c_allowed_chars }")|
      iv_hint        = 'Comma-separated labels for grouping and repo organization (optional)'
    )->checkbox(
      iv_name        = c_id-write_protected
      iv_label       = 'Write Protected'
      iv_hint        = 'Lock repository against changes from remote (pull)'
    )->checkbox(
      iv_name        = c_id-ignore_subpackages
      iv_label       = 'Ignore Subpackages'
      iv_hint        = 'Syncronize root package only'
    )->checkbox(
      iv_name        = c_id-only_local_objects
      iv_label       = 'Only Local Objects'
      iv_hint        = 'Ignore objects imported from other systems; serialize only objects created in this system'
    )->checkbox(
      iv_name        = c_id-main_language_only
      iv_label       = 'Only Serialize Main Language'
      iv_hint        = 'Ignore translations; serialize only main language of repository' ).

    IF zcl_abapgit_feature=>is_enabled( 'FLOW' ) = abap_true.
      ro_form->checkbox(
        iv_name  = c_id-flow
        iv_label = 'Enable Flow Page' ).
    ENDIF.

    ro_form->start_group(
      iv_name        = c_id-checks
      iv_label       = 'Local Checks'
      iv_hint        = 'Code Inspector check performed to run from menu and before commit'
    )->text(
      iv_name        = c_id-code_inspector_check_variant
      iv_side_action = c_event-choose_check_variant
      iv_label       = 'Code Inspector Check Variant'
      iv_hint        = 'Global check variant for Code Inspector or ABAP Test Cockpit'
    )->checkbox(
      iv_name        = c_id-block_commit
      iv_label       = 'Block Commit If Code Inspection Has Errors'
      iv_hint        = 'Prevent staging if errors of priority 1 or 2 were found during check'
    )->command(
      iv_label       = 'Save Settings'
      iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-save
    )->command(
      iv_label       = 'Back'
      iv_action      = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD handle_picklist_state.

    IF mo_popup_picklist IS BOUND AND
      ( mo_popup_picklist->is_fulfilled( ) = abap_true OR mo_popup_picklist->is_in_page( ) = abap_false ).
      " Picklist is either fullfilled OR
      " it was on its own page and user went back from it via F3/ESC and the picklist had no "graceful back" handler
      CASE mo_popup_picklist->id( ).
        WHEN c_event-choose_check_variant.
          choose_check_variant( abap_true ).
        WHEN OTHERS.
          zcx_abapgit_exception=>raise( |Unexpected picklist id { mo_popup_picklist->id( ) }| ).
      ENDCASE.

      CLEAR mo_popup_picklist.
    ENDIF.

  ENDMETHOD.


  METHOD is_customizing_included.

    DATA lt_files TYPE zif_abapgit_definitions=>ty_files_item_tt.

    lt_files = mo_repo->get_files_local( ).

    READ TABLE lt_files TRANSPORTING NO FIELDS
      WITH KEY item-obj_type = zif_abapgit_data_config=>c_data_type-tabu. "todo
    IF sy-subrc = 0.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD read_settings.

    DATA: li_package TYPE REF TO zif_abapgit_sap_package.

    li_package = zcl_abapgit_factory=>get_sap_package( mo_repo->get_package( ) ).

    " Get settings from DB
    ms_settings = mo_repo->get_local_settings( ).
    CREATE OBJECT ro_form_data.

    " Local Settings
    ro_form_data->set(
      iv_key = c_id-display_name
      iv_val = ms_settings-display_name ).

    IF li_package->are_changes_recorded_in_tr_req( ) = abap_true.
      ro_form_data->set(
        iv_key = c_id-transport_request
        iv_val = ms_settings-transport_request ).
    ENDIF.

    IF is_customizing_included( ) = abap_true.
      ro_form_data->set(
        iv_key = c_id-customizing_request
        iv_val = ms_settings-customizing_request ).
    ENDIF.

    ro_form_data->set(
      iv_key = c_id-labels
      iv_val = ms_settings-labels ).
    ro_form_data->set(
      iv_key = c_id-ignore_subpackages
      iv_val = boolc( ms_settings-ignore_subpackages = abap_true ) ) ##TYPE.
    ro_form_data->set(
      iv_key = c_id-main_language_only
      iv_val = boolc( ms_settings-main_language_only = abap_true ) ) ##TYPE.
    ro_form_data->set(
      iv_key = c_id-flow
      iv_val = boolc( ms_settings-flow = abap_true ) ) ##TYPE.
    ro_form_data->set(
      iv_key = c_id-write_protected
      iv_val = boolc( ms_settings-write_protected = abap_true ) ) ##TYPE.
    ro_form_data->set(
      iv_key = c_id-only_local_objects
      iv_val = boolc( ms_settings-only_local_objects = abap_true ) ) ##TYPE.
    ro_form_data->set(
      iv_key = c_id-code_inspector_check_variant
      iv_val = |{ ms_settings-code_inspector_check_variant }| ).
    ro_form_data->set(
      iv_key = c_id-block_commit
      iv_val = boolc( ms_settings-block_commit = abap_true ) ) ##TYPE.

  ENDMETHOD.


  METHOD save_settings.

    ms_settings-display_name                 = mo_form_data->get( c_id-display_name ).
    ms_settings-transport_request            = mo_form_data->get( c_id-transport_request ).
    ms_settings-customizing_request          = mo_form_data->get( c_id-customizing_request ).
    ms_settings-labels                       = zcl_abapgit_repo_labels=>normalize( mo_form_data->get( c_id-labels ) ).
    ms_settings-ignore_subpackages           = mo_form_data->get( c_id-ignore_subpackages ).
    ms_settings-main_language_only           = mo_form_data->get( c_id-main_language_only ).
    ms_settings-flow                         = mo_form_data->get( c_id-flow ).
    ms_settings-write_protected              = mo_form_data->get( c_id-write_protected ).
    ms_settings-only_local_objects           = mo_form_data->get( c_id-only_local_objects ).
    ms_settings-code_inspector_check_variant = mo_form_data->get( c_id-code_inspector_check_variant ).
    ms_settings-block_commit                 = mo_form_data->get( c_id-block_commit ).

    mo_repo->set_local_settings( ms_settings ).

    COMMIT WORK AND WAIT.

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

    mo_form_data = read_settings( ).

  ENDMETHOD.


  METHOD validate_form.

    DATA:
      lx_error               TYPE REF TO zcx_abapgit_exception,
      lv_transport_request   TYPE trkorr,
      lv_customizing_request TYPE trkorr,
      lv_check_variant       TYPE sci_chkv.

    ro_validation_log = zcl_abapgit_html_form_utils=>create( mo_form )->validate( io_form_data ).

    lv_transport_request = io_form_data->get( c_id-transport_request ).
    IF lv_transport_request IS NOT INITIAL.
      TRY.
          zcl_abapgit_factory=>get_cts_api( )->validate_transport_request( lv_transport_request ).
        CATCH zcx_abapgit_exception INTO lx_error.
          ro_validation_log->set(
            iv_key = c_id-transport_request
            iv_val = lx_error->get_text( ) ).
      ENDTRY.
    ENDIF.

    lv_customizing_request = io_form_data->get( c_id-customizing_request ).
    IF lv_customizing_request IS NOT INITIAL.
      TRY.
          zcl_abapgit_factory=>get_cts_api( )->validate_transport_request( lv_customizing_request ).
        CATCH zcx_abapgit_exception INTO lx_error.
          ro_validation_log->set(
            iv_key = c_id-customizing_request
            iv_val = lx_error->get_text( ) ).
      ENDTRY.
    ENDIF.

    lv_check_variant = to_upper( io_form_data->get( c_id-code_inspector_check_variant ) ).
    IF lv_check_variant IS NOT INITIAL.
      TRY.
          zcl_abapgit_factory=>get_code_inspector( mo_repo->get_package( )
            )->validate_check_variant( lv_check_variant ).
        CATCH zcx_abapgit_exception INTO lx_error.
          ro_validation_log->set(
            iv_key = c_id-code_inspector_check_variant
            iv_val = lx_error->get_text( ) ).
      ENDTRY.
    ENDIF.

    IF io_form_data->get( c_id-block_commit ) = abap_true AND lv_check_variant IS INITIAL.
      ro_validation_log->set(
        iv_key = c_id-block_commit
        iv_val = |If block commit is active, a check variant has to be maintained| ).
    ENDIF.

    TRY.
        zcl_abapgit_repo_labels=>validate( io_form_data->get( c_id-labels ) ).
      CATCH zcx_abapgit_exception INTO lx_error.
        ro_validation_log->set(
          iv_key = c_id-labels
          iv_val = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    mo_form_data->merge( zcl_abapgit_html_form_utils=>create( mo_form )->normalize( ii_event->form_data( ) ) ).

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-go_back.
        rs_handled-state = zcl_abapgit_html_form_utils=>create( mo_form )->exit(
          io_form_data    = mo_form_data
          io_compare_with = read_settings( ) ).

      WHEN c_event-choose_transport_request.

        choose_transport_request( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_event-choose_customizing_request.

        choose_customizing_request( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_event-choose_labels.

        choose_labels( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_event-choose_check_variant.

        choose_check_variant( ).

      WHEN c_event-save.
        " Validate form entries before saving
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          save_settings( ).
        ENDIF.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

    IF mo_popup_picklist IS BOUND. " Uniform popup state handling
      " This should happen only for a new popup because
      " on the first re-render main component event handling is blocked
      " and not called again until the popup distruction
      IF mo_popup_picklist->is_in_page( ) = abap_true.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      ELSE.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
        rs_handled-page  = zcl_abapgit_gui_page_hoc=>create(
          ii_child_component = mo_popup_picklist
          iv_show_as_modal   = abap_true ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    handle_picklist_state( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).

    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
      io_repo               = mo_repo
      iv_show_commit        = abap_false
      iv_interactive_branch = abap_true ) ).

    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).

    ri_html->add( `</div>` ).

    IF mo_popup_picklist IS NOT BOUND OR mo_popup_picklist->is_in_page( ) = abap_false.
      register_handlers( ).
    ELSEIF mo_popup_picklist->is_in_page( ) = abap_true.
      " Block usual page events if the popup is an in-page popup
      ri_html->add( zcl_abapgit_gui_in_page_modal=>create( mo_popup_picklist ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
