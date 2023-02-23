CLASS zcl_abapgit_gui_page_sett_bckg DEFINITION
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

    CONSTANTS:
      BEGIN OF c_id,
        mode_selection TYPE string VALUE 'mode_selection',
        method         TYPE string VALUE 'method',
        authentication TYPE string VALUE 'authentication',
        username       TYPE string VALUE 'username',
        password       TYPE string VALUE 'password',
        mode_settings  TYPE string VALUE 'mode_settings',
        settings       TYPE string VALUE 'settings',
      END OF c_id .
    CONSTANTS:
      BEGIN OF c_event,
        run_now TYPE string VALUE 'run_now',
        save    TYPE string VALUE 'save',
      END OF c_event .
    DATA mo_form TYPE REF TO zcl_abapgit_html_form .
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map .
    DATA mo_form_util TYPE REF TO zcl_abapgit_html_form_utils .
    DATA mo_repo TYPE REF TO zcl_abapgit_repo .
    DATA mv_settings_count TYPE i .

    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form
      RAISING
        zcx_abapgit_exception .
    METHODS read_settings
      RAISING
        zcx_abapgit_exception .
    METHODS read_persist
      RETURNING
        VALUE(rs_persist) TYPE zcl_abapgit_persist_background=>ty_background
      RAISING
        zcx_abapgit_exception .
    METHODS save_settings
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_SETT_BCKG IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_form_data.
    mo_repo = io_repo.
    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).

    read_settings( ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_sett_bckg.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo = io_repo.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Background Mode'
      io_page_menu       = zcl_abapgit_gui_chunk_lib=>settings_repo_toolbar(
                             iv_key = io_repo->get_key( )
                             iv_act = zif_abapgit_definitions=>c_action-repo_background )
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD get_form_schema.

    DATA:
      lt_methods TYPE zcl_abapgit_background=>ty_methods,
      ls_method  LIKE LINE OF lt_methods,
      lv_hint    TYPE string.

    lt_methods = zcl_abapgit_background=>list_methods( ).

    ro_form = zcl_abapgit_html_form=>create(
                iv_form_id   = 'repo-background-form'
                iv_help_page = 'https://docs.abapgit.org/settings-background-mode.html' ).

    ro_form->start_group(
      iv_name          = c_id-mode_selection
      iv_label         = 'Mode'
    )->radio(
      iv_name          = c_id-method
      iv_default_value = ''
      iv_label         = 'Selection'
      iv_hint          = 'Define the action that will be executed in background mode'
    )->option(
      iv_label         = 'Do Nothing'
      iv_value         = '' ).

    LOOP AT lt_methods INTO ls_method.
      ro_form->option(
        iv_label       = ls_method-description
        iv_value       = ls_method-class ).
    ENDLOOP.

    ro_form->table(
      iv_name        = c_id-settings
      iv_hint        = 'Settings required for selected background action'
      iv_label       = 'Additional Settings'
    )->column(
      iv_label       = 'Key'
      iv_width       = '50%'
      iv_readonly    = abap_true
    )->column(
      iv_label       = 'Value'
      iv_width       = '50%' ).

    lv_hint = 'Password will be saved in clear text!'.

    ro_form->start_group(
      iv_name        = c_id-authentication
      iv_label       = 'HTTP Authentication (Optional)'
      iv_hint        = lv_hint
    )->text(
      iv_name        = c_id-username
      iv_label       = 'Username'
      iv_hint        = lv_hint
    )->text(
      iv_name        = c_id-password
      iv_label       = 'Password'
      iv_hint        = lv_hint
      iv_placeholder = lv_hint ).

    ro_form->command(
      iv_label       = 'Save Settings'
      iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-save
    )->command(
      iv_label       = 'Run Background Logic'
      iv_action      = zif_abapgit_definitions=>c_action-go_background_run
    )->command(
      iv_label       = 'Back'
      iv_action      = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD read_persist.

    DATA lo_per TYPE REF TO zcl_abapgit_persist_background.

    CREATE OBJECT lo_per.

    TRY.
        rs_persist = lo_per->get_by_key( mo_repo->get_key( ) ).
      CATCH zcx_abapgit_not_found.
        CLEAR rs_persist.
    ENDTRY.

  ENDMETHOD.


  METHOD read_settings.

    DATA:
      ls_per      TYPE zcl_abapgit_persist_background=>ty_background,
      lv_row      TYPE i,
      lv_val      TYPE string,
      lt_settings LIKE ls_per-settings,
      ls_settings LIKE LINE OF ls_per-settings.

    ls_per = read_persist( ).

    " Mode Selection
    mo_form_data->set(
      iv_key = c_id-method
      iv_val = ls_per-method ).

    " Mode Settings
    IF ls_per-method IS NOT INITIAL.

      lt_settings = ls_per-settings.

      " skip invalid values, from old background logic
      IF ls_per-method <> 'push' AND ls_per-method <> 'pull' AND ls_per-method <> 'nothing'.
        CALL METHOD (ls_per-method)=>zif_abapgit_background~get_settings
          CHANGING
            ct_settings = lt_settings.
      ENDIF.

      LOOP AT lt_settings INTO ls_settings.
        lv_row = lv_row + 1.
        DO 3 TIMES.
          CASE sy-index.
            WHEN 1.
              lv_val = ls_settings-key.
            WHEN 2.
              lv_val = ls_settings-value.
          ENDCASE.
          mo_form_data->set(
            iv_key = |{ c_id-settings }-{ lv_row }-{ sy-index }|
            iv_val = lv_val ).
        ENDDO.
      ENDLOOP.

    ENDIF.

    mv_settings_count = lv_row.

    mo_form_data->set(
      iv_key = |{ c_id-settings }-{ zif_abapgit_html_form=>c_rows }|
      iv_val = |{ mv_settings_count }| ).

    " Authentication
    mo_form_data->set(
      iv_key = c_id-username
      iv_val = ls_per-username ).
    mo_form_data->set(
      iv_key = c_id-password
      iv_val = ls_per-password ).

    " Set for is_dirty check
    mo_form_util->set_data( mo_form_data ).

  ENDMETHOD.


  METHOD save_settings.

    DATA:
      lo_persistence TYPE REF TO zcl_abapgit_persist_background,
      ls_per         TYPE zcl_abapgit_persist_background=>ty_background,
      lt_settings    LIKE ls_per-settings.

    FIELD-SYMBOLS:
      <ls_settings> LIKE LINE OF ls_per-settings.

    ls_per-key = mo_repo->get_key( ).

    " Mode Selection
    ls_per-method = mo_form_data->get( c_id-method ).

    " Mode Settings
    IF ls_per-method IS NOT INITIAL.

      lt_settings = ls_per-settings.

      " skip invalid values, from old background logic
      IF ls_per-method <> 'push' AND ls_per-method <> 'pull' AND ls_per-method <> 'nothing'.
        CALL METHOD (ls_per-method)=>zif_abapgit_background~get_settings
          CHANGING
            ct_settings = lt_settings.
      ENDIF.

      LOOP AT lt_settings ASSIGNING <ls_settings>.
        <ls_settings>-value = mo_form_data->get( |{ c_id-settings }-{ sy-tabix }-2| ).
      ENDLOOP.

      ls_per-settings = lt_settings.

    ENDIF.

    " Authentication
    ls_per-username = mo_form_data->get( c_id-username ).
    ls_per-password = mo_form_data->get( c_id-password ).

    CREATE OBJECT lo_persistence.

    IF ls_per-method IS INITIAL.
      lo_persistence->delete( ls_per-key ).
    ELSE.
      lo_persistence->modify( ls_per ).
    ENDIF.

    COMMIT WORK AND WAIT.

    MESSAGE 'Settings succesfully saved' TYPE 'S'.

    read_settings( ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-go_back.
        rs_handled-state = mo_form_util->exit( mo_form_data ).

      WHEN c_event-save.
        save_settings( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    read_settings( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).

    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
                    io_repo               = mo_repo
                    iv_show_commit        = abap_false
                    iv_interactive_branch = abap_true ) ).

    ri_html->add( mo_form->render(
      iv_form_class = 'w800px'
      io_values     = mo_form_data ) ).

    ri_html->add( `</div>` ).

  ENDMETHOD.
ENDCLASS.
