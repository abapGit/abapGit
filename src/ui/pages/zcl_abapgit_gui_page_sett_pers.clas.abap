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
        label_colors           TYPE string VALUE 'label_colors',
      END OF c_id.
    CONSTANTS:
      BEGIN OF c_event,
        save TYPE string VALUE 'save',
      END OF c_event.

    DATA mo_form TYPE REF TO zcl_abapgit_html_form.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_util TYPE REF TO zcl_abapgit_html_form_utils.
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.

    DATA mo_settings TYPE REF TO zcl_abapgit_settings.
    DATA ms_settings TYPE zif_abapgit_definitions=>ty_s_user_settings.

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
    METHODS render_repo_labels_help_hint
      RETURNING
        VALUE(rv_html) TYPE string.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_SETT_PERS IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).

    read_settings( ).

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
      iv_label         = 'Icon Scaling (HDPI)'
      iv_hint          = 'Adjust size of icons for High DPI displays'
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
    )->textarea(
      iv_name          = c_id-label_colors
      iv_rows          = 3
      iv_label         = `Repo label colors ` && render_repo_labels_help_hint( )
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
      iv_cmd_type      = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action        = c_event-save
    )->command(
      iv_label         = 'Back'
      iv_action        = zif_abapgit_definitions=>c_action-go_back ).

    " Not available via this form:
    " - User-specific hotkey settings have been discontinued
    " - hide_sapgui_hint is set via ZCL_ABAPGIT_SERVICES_ABAPGIT-CHECK_SAPGUI

  ENDMETHOD.


  METHOD read_settings.

    " Get settings from DB
    mo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
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
    mo_form_data->set(
      iv_key = c_id-label_colors
      iv_val = ms_settings-label_colors ).

    " Interaction
    mo_form_data->set(
      iv_key = c_id-activate_wo_popup
      iv_val = boolc( ms_settings-activate_wo_popup = abap_true ) ) ##TYPE.
    mo_form_data->set(
      iv_key = c_id-adt_jump_enabled
      iv_val = boolc( ms_settings-adt_jump_enabled = abap_true ) ) ##TYPE.
    mo_form_data->set(
      iv_key = c_id-link_hints_enabled
      iv_val = boolc( ms_settings-link_hints_enabled = abap_true ) ) ##TYPE.
    mo_form_data->set(
      iv_key = c_id-link_hint_key
      iv_val = |{ ms_settings-link_hint_key }| ).

    " Resources
    mo_form_data->set(
      iv_key = c_id-parallel_proc_disabled
      iv_val = boolc( ms_settings-parallel_proc_disabled = abap_true ) ) ##TYPE.

    " Set for is_dirty check
    mo_form_util->set_data( mo_form_data ).

  ENDMETHOD.


  METHOD render_repo_labels_help_hint.

    DATA lt_fragments TYPE string_table.
    DATA lt_labels TYPE string_table.
    DATA lv_l TYPE string.
    DATA lo_colors TYPE REF TO zcl_abapgit_string_map.

    APPEND `<p style="margin-bottom: 0.3em">` TO lt_fragments.
    APPEND `Comma-separated list of <code>label:color</code> pairs.` TO lt_fragments.
    APPEND ` <code>color</code> part can be either a css style (see below) or <code>#fg/bg</code> pair,`
      TO lt_fragments.
    APPEND ` where <code>fg</code> and <code>bg</code> are RGB color codes (3 or 6 long).` TO lt_fragments.
    APPEND ` You can also specify just <code>fg</code> or <code>bg</code>` TO lt_fragments.
    APPEND ` (defaults will be used for missing parts).` TO lt_fragments.
    APPEND ` E.g. <code>utils:brown, work:#ff0000/880000, client X:#ddd, client Y:#/333</code>` TO lt_fragments.
    APPEND `<br>Available CSS styles:` TO lt_fragments.
    APPEND `</p>` TO lt_fragments.

    APPEND `white` TO lt_labels.
    APPEND `white-b` TO lt_labels.
    APPEND `white-r` TO lt_labels.
    APPEND `grey` TO lt_labels.
    APPEND `dark-w` TO lt_labels.
    APPEND `dark-y` TO lt_labels.
    APPEND `dark-r` TO lt_labels.
    APPEND `dark-b` TO lt_labels.
    APPEND `lightblue` TO lt_labels.
    APPEND `darkblue` TO lt_labels.
    APPEND `lightgreen` TO lt_labels.
    APPEND `darkgreen` TO lt_labels.
    APPEND `lightred` TO lt_labels.
    APPEND `darkred` TO lt_labels.
    APPEND `yellow` TO lt_labels.
    APPEND `darkyellow` TO lt_labels.
    APPEND `orrange` TO lt_labels.
    APPEND `brown` TO lt_labels.
    APPEND `pink` TO lt_labels.
    APPEND `teal` TO lt_labels.
    APPEND `darkviolet` TO lt_labels.

    lo_colors = zcl_abapgit_string_map=>create( ).
    LOOP AT lt_labels INTO lv_l.
      TRY.
          lo_colors->set(
            iv_key = lv_l
            iv_val = lv_l ).
        CATCH zcx_abapgit_exception.
      ENDTRY.
    ENDLOOP.

    APPEND zcl_abapgit_gui_chunk_lib=>render_label_list(
      it_labels       = lt_labels
      io_label_colors = lo_colors ) TO lt_fragments.

    APPEND
      `<p style="margin-top: 0.3em">see also <code>rl-*</code> styles in common.css (styles, forgotten here)</p>`
      TO lt_fragments.

    rv_html = zcl_abapgit_gui_chunk_lib=>render_help_hint( concat_lines_of( table = lt_fragments ) ).

  ENDMETHOD.


  METHOD save_settings.

    DATA li_persistence TYPE REF TO zif_abapgit_persist_settings.

    " Startup
    ms_settings-show_default_repo = mo_form_data->get( c_id-show_default_repo ).

    " UI
    ms_settings-ui_theme = mo_form_data->get( c_id-ui_theme ).
    ms_settings-icon_scaling = mo_form_data->get( c_id-icon_scaling ).
    ms_settings-max_lines = mo_form_data->get( c_id-max_lines ).
    ms_settings-label_colors = zcl_abapgit_repo_labels=>normalize_colors( mo_form_data->get(  c_id-label_colors ) ).

    " Interaction
    ms_settings-activate_wo_popup = mo_form_data->get( c_id-activate_wo_popup ).
    ms_settings-adt_jump_enabled = mo_form_data->get( c_id-adt_jump_enabled ).
    ms_settings-link_hints_enabled = mo_form_data->get( c_id-link_hints_enabled ).
    ms_settings-link_hint_key = mo_form_data->get( c_id-link_hint_key ).

    " Resources
    ms_settings-parallel_proc_disabled = mo_form_data->get( c_id-parallel_proc_disabled ).

    " Store in DB
    mo_settings->set_user_settings( ms_settings ).

    li_persistence = zcl_abapgit_persist_factory=>get_settings( ).
    li_persistence->modify( mo_settings ).

    COMMIT WORK AND WAIT.

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

    read_settings( ).

  ENDMETHOD.


  METHOD validate_form.

    DATA lx_error TYPE REF TO zcx_abapgit_exception.

    ro_validation_log = mo_form_util->validate( io_form_data ).

    TRY.
        zcl_abapgit_repo_labels=>validate_colors( io_form_data->get( c_id-label_colors ) ).
      CATCH zcx_abapgit_exception INTO lx_error.
        ro_validation_log->set(
          iv_key = c_id-label_colors
          iv_val = lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-go_back.
        rs_handled-state = mo_form_util->exit( mo_form_data ).

      WHEN c_event-save.
        " Validate form entries before saving
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          save_settings( ).
        ENDIF.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    IF mo_form_util->is_empty( mo_form_data ) = abap_true.
      read_settings( ).
    ENDIF.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( '<div class="form-container">' ).
    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).
    ri_html->add( '</div>' ).
  ENDMETHOD.
ENDCLASS.
