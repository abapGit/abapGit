CLASS zcl_abapgit_gui_page_sett_pers DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.

    CLASS-METHODS create
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.
    METHODS constructor
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_id,
        startup                TYPE string VALUE 'startup',
        show_default_repo      TYPE string VALUE 'show_default_repo',
        ui                     TYPE string VALUE 'ui',
        ui_theme               TYPE string VALUE 'ui_theme',
        icon_scaling           TYPE string VALUE 'icon_scaling',
        max_lines              TYPE string VALUE 'max_lines',
        interaction            TYPE string VALUE 'interaction',
        adt_jump_enabled       TYPE string VALUE 'adt_jump_enabled',
        link_hints_enabled     TYPE string VALUE 'link_hints_enabled',
        link_hint_key          TYPE string VALUE 'link_hint_key',
        hotkeys                TYPE string VALUE 'hotkeys',
        resources              TYPE string VALUE 'resources',
        parallel_proc_disabled TYPE string VALUE 'parallel_proc_disabled',
        hide_sapgui_hint       TYPE string VALUE 'hide_sapgui_hint',
        activate_wo_popup      TYPE string VALUE 'activate_wo_popup',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_event,
        go_back TYPE string VALUE 'go-back',
        save    TYPE string VALUE 'save',
      END OF c_event.

    DATA mo_settings TYPE REF TO zcl_abapgit_settings.
    DATA ms_settings TYPE zif_abapgit_definitions=>ty_s_user_settings.
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form TYPE REF TO zcl_abapgit_html_form.

    METHODS validate_form
      IMPORTING
        !io_form_data            TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.
    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form.
    METHODS read_settings
      RAISING
        zcx_abapgit_exception.
    METHODS save_settings
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_SETT_PERS IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_sett_pers.

    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Personal Settings'
      io_page_menu       = zcl_abapgit_gui_chunk_lib=>settings_toolbar(
                             zif_abapgit_definitions=>c_action-go_settings_personal )
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD get_form_schema.

    ro_form = zcl_abapgit_html_form=>create(
      iv_form_id   = 'personal-setting-form'
      iv_help_page = 'https://docs.abapgit.org/guide-settings-personal.html' ).

    ro_form->start_group(
      iv_name          = c_id-startup
      iv_label         = 'Startup'
    )->checkbox(
      iv_name          = c_id-show_default_repo
      iv_label         = 'Show Last Opened Repository'
      iv_hint          = 'Recommended to check, if you are using ADT'
    )->start_group(
      iv_name          = c_id-ui
      iv_label         = 'User Interface'
    )->radio(
      iv_name          = c_id-ui_theme
      iv_default_value = zcl_abapgit_settings=>c_ui_theme-default
      iv_label         = 'Theme'
    )->option(
      iv_label         = 'Default'
      iv_value         = zcl_abapgit_settings=>c_ui_theme-default
    )->option(
      iv_label         = 'Dark'
      iv_value         = zcl_abapgit_settings=>c_ui_theme-dark
    )->option(
      iv_label         = 'Belize'
      iv_value         = zcl_abapgit_settings=>c_ui_theme-belize
    )->option(
      iv_label         = 'Synced with SAP GUI'
      iv_value         = zcl_abapgit_settings=>c_ui_theme-synced_with_gui
    )->radio(
      iv_name          = c_id-icon_scaling
      iv_default_value = ''
      iv_label         = 'Icon Scaling'
    )->option(
      iv_label         = 'Automatic'
      iv_value         = ''
    )->option(
      iv_label         = 'Small'
      iv_value         = zcl_abapgit_settings=>c_icon_scaling-small
    )->option(
      iv_label         = 'Large'
      iv_value         = zcl_abapgit_settings=>c_icon_scaling-large
    )->number(
      iv_name          = c_id-max_lines
      iv_label         = 'List Size'
      iv_hint          = 'Maximum number of objects listed (0 = All)'
      iv_min           = 0
      iv_max           = 10000
    )->start_group(
      iv_name          = c_id-interaction
      iv_label         = 'Interaction'
    )->checkbox(
      iv_name          = c_id-activate_wo_popup
      iv_label         = 'Activate Objects Without Popup'
      iv_hint          = 'Activates objects automatically without showing popup'
    )->checkbox(
      iv_name          = c_id-adt_jump_enabled
      iv_label         = 'Enable Jump to ABAP Development Tools (If Available)'
      iv_hint          = 'Recommended to check, if you are using ADT'
    )->checkbox(
      iv_name          = c_id-link_hints_enabled
      iv_label         = 'Enable Vimium-like Link Hints'
      iv_hint          = 'When you hit the key, abapGit will identify clickable things and put a label beside it'
    )->text(
      iv_name          = c_id-link_hint_key
      iv_label         = 'Key to Activate Link Hints'
      iv_min           = 0
      iv_max           = 1
    )->start_group(
      iv_name          = c_id-resources
      iv_label         = 'System Resources'
    )->checkbox(
      iv_name          = c_id-parallel_proc_disabled
      iv_label         = 'Disable Parallel Processing'
      iv_hint          = 'If disabled, abapGit will use only a single thread to serialize objects'
    )->command(
      iv_label         = 'Save Settings'
      iv_is_main       = abap_true
      iv_action        = c_event-save
    )->command(
      iv_label         = 'Back'
      iv_action        = c_event-go_back ).

    " Not available via this form:
    " - User-specific hotkey settings have been discontinued
    " - hide_sapgui_hint is set via ZCL_ABAPGIT_SERVICES_ABAPGIT-CHECK_SAPGUI

  ENDMETHOD.


  METHOD read_settings.

    " Get settings from DB
    mo_settings = zcl_abapgit_persist_settings=>get_instance( )->read( ).
    ms_settings = mo_settings->get_user_settings( ).

    " Startup
    mo_form_data->set(
      iv_key = c_id-show_default_repo
      iv_val = |{ ms_settings-show_default_repo }| ).

    " UI
    mo_form_data->set(
      iv_key = c_id-ui_theme
      iv_val = ms_settings-ui_theme ).
    mo_form_data->set(
      iv_key = c_id-icon_scaling
      iv_val = |{ ms_settings-icon_scaling }| ).
    mo_form_data->set(
      iv_key = c_id-max_lines
      iv_val = |{ ms_settings-max_lines }| ).

    " Interaction
    mo_form_data->set(
      iv_key = c_id-activate_wo_popup
      iv_val = |{ ms_settings-activate_wo_popup }| ).
    mo_form_data->set(
      iv_key = c_id-adt_jump_enabled
      iv_val = |{ ms_settings-adt_jump_enabled }| ).
    mo_form_data->set(
      iv_key = c_id-link_hints_enabled
      iv_val = |{ ms_settings-link_hints_enabled }| ).
    mo_form_data->set(
      iv_key = c_id-link_hint_key
      iv_val = |{ ms_settings-link_hint_key }| ).

    " Resources
    mo_form_data->set(
      iv_key = c_id-parallel_proc_disabled
      iv_val = |{ ms_settings-parallel_proc_disabled }| ).

  ENDMETHOD.


  METHOD save_settings.

    DATA lo_persistence TYPE REF TO zcl_abapgit_persist_settings.

    " Startup
    ms_settings-show_default_repo = mo_form_data->get( c_id-show_default_repo ).

    " UI
    ms_settings-ui_theme = mo_form_data->get( c_id-ui_theme ).
    ms_settings-icon_scaling = mo_form_data->get( c_id-icon_scaling ).
    ms_settings-max_lines = mo_form_data->get( c_id-max_lines ).

    " Interaction
    ms_settings-activate_wo_popup = mo_form_data->get( c_id-activate_wo_popup ).
    ms_settings-adt_jump_enabled = mo_form_data->get( c_id-adt_jump_enabled ).
    ms_settings-link_hints_enabled = mo_form_data->get( c_id-link_hints_enabled ).
    ms_settings-link_hint_key = mo_form_data->get( c_id-link_hint_key ).

    " Resources
    ms_settings-parallel_proc_disabled = mo_form_data->get( c_id-parallel_proc_disabled ).

    " Store in DB
    mo_settings->set_user_settings( ms_settings ).

    lo_persistence = zcl_abapgit_persist_settings=>get_instance( ).
    lo_persistence->modify( mo_settings ).

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

  ENDMETHOD.


  METHOD validate_form.

    ro_validation_log = mo_form->validate_required_fields( io_form_data ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    mo_form_data = mo_form->normalize_form_data( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-go_back.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_event-save.
        " Validate all form entries
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          save_settings( ).

          rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render. " Display errors
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    gui_services( )->register_event_handler( me ).

    read_settings( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( mo_form->render(
      iv_form_class     = 'dialog w600px m-em5-sides margin-v1'
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).

  ENDMETHOD.
ENDCLASS.
