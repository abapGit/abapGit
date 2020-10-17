CLASS zcl_abapgit_gui_page_theme DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_abapgit_gui_page
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_action,
        change_theme TYPE string VALUE 'change_theme',
      END OF c_action.
    METHODS:
      constructor RAISING zcx_abapgit_exception,
      zif_abapgit_gui_event_handler~on_event REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION.
  PRIVATE SECTION.
    METHODS:
      build_menu RETURNING VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar,
      render_text IMPORTING ii_html TYPE REF TO zif_abapgit_html,
      render_themed_table IMPORTING ii_html TYPE REF TO zif_abapgit_html,
      render_menu_test IMPORTING ii_html TYPE REF TO zif_abapgit_html,
      render_input IMPORTING ii_html TYPE REF TO zif_abapgit_html,
      render_form IMPORTING ii_html TYPE REF TO zif_abapgit_html,
      render_icons IMPORTING ii_html TYPE REF TO zif_abapgit_html,
      render_icon IMPORTING ii_html TYPE REF TO zif_abapgit_html
                            iv_name TYPE string.
    DATA:
      mo_form_values TYPE REF TO zcl_abapgit_string_map.
ENDCLASS.



CLASS zcl_abapgit_gui_page_theme IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'Theme Test'.
    ms_control-page_menu = build_menu( ).
  ENDMETHOD.

  METHOD render_content.
    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    render_text( ri_html ).
    ri_html->add( '<hr>' ).

    ri_html->add( '<h1>Color Palette</h1>' ).

    ri_html->add( '<hr>' ).

    render_themed_table( ri_html ).
    ri_html->add( '<hr>' ).

    render_icons( ri_html ).
    ri_html->add( '<hr>' ).

    render_menu_test( ri_html ).
    ri_html->add( '<hr>' ).

    render_input( ri_html ).
    ri_html->add( '<hr>' ).

    render_form( ri_html ).
  ENDMETHOD.

  METHOD build_menu.
    DATA: li_gui_functions        TYPE REF TO zif_abapgit_gui_functions,
          lv_supports_ie_devtools TYPE abap_bool,
          lo_theme_sub_menu       TYPE REF TO zcl_abapgit_html_toolbar.

    li_gui_functions = zcl_abapgit_ui_factory=>get_gui_functions( ).
    lv_supports_ie_devtools = li_gui_functions->is_sapgui_for_windows( ).

    CREATE OBJECT ro_menu.

    IF lv_supports_ie_devtools = abap_true.
      ro_menu->add(
        iv_txt = 'Open IE DevTools'
        iv_act = zif_abapgit_definitions=>c_action-ie_devtools ).
    ENDIF.

    CREATE OBJECT lo_theme_sub_menu.

    lo_theme_sub_menu->add(
      iv_txt = 'Default'
      iv_act = |{ c_action-change_theme }?{ zcl_abapgit_settings=>c_ui_theme-default }|
    )->add(
      iv_txt = 'Belize'
      iv_act = |{ c_action-change_theme }?{ zcl_abapgit_settings=>c_ui_theme-belize }|
    )->add(
      iv_txt = 'Dark'
      iv_act = |{ c_action-change_theme }?{ zcl_abapgit_settings=>c_ui_theme-dark }|
    )->add(
      iv_txt = 'Quartz'
      iv_act = |{ c_action-change_theme }?{ zcl_abapgit_settings=>c_ui_theme-quartz }|
    )->add(
      iv_txt = 'Quartz Dark'
      iv_act = |{ c_action-change_theme }?{ zcl_abapgit_settings=>c_ui_theme-quartz_dark }|
    )->add(
      iv_txt = 'Synced with SAP GUI'
      iv_act = |{ c_action-change_theme }?{ zcl_abapgit_settings=>c_ui_theme-synced_with_gui }| ).

    ro_menu->add(
      iv_txt = 'Theme'
      io_sub = lo_theme_sub_menu ).
  ENDMETHOD.

  METHOD render_text.
    ii_html->add( '<h1>H1 Header</h1>' ).
    ii_html->add( '<h2>H2 Header</h2>' ).
    ii_html->add( '<h3>H3 Header</h3>' ).
    ii_html->add( '<h4>H4 Header</h4>' ).
    ii_html->add( '<p>' ).
    DO 3 TIMES.
      ii_html->add(
        |Lorem <a href="#">ipsum</a> dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labo| &&
        |re et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliqu| &&
        |id ex ea commodi consequat. Quis aute iure <em>reprehenderit</em> in voluptate velit esse cillum dolore eu | &&
        |fugiat nulla pariatur. Excepteur sint obcaecat cupiditat non proident, sunt in culpa qui officia deserunt m| &&
        |ollit <b>anim</b> id estlaborum. | ).
    ENDDO.
    ii_html->add( '</p>' ).
  ENDMETHOD.

  METHOD render_themed_table.
    ii_html->add( '<h1>Themed Table</h1>' ).
    ii_html->add( '<table class="themed-table">' ).
    ii_html->add( '<tr><th>Column A</th><th>Column B</th></tr>' ).
    ii_html->add( '<tr><td>Value A1</th><td>Value B1</th></tr>' ).
    ii_html->add( '<tr><td>Value A2</th><td>Value B2</th></tr>' ).
    ii_html->add( '<tr><td>Value A3</th><td>Value B3</th></tr>' ).
    ii_html->add( '<tr><td>Value A4</th><td>Value B4</th></tr>' ).
    ii_html->add( '</table>' ).
  ENDMETHOD.

  METHOD render_menu_test.
    DATA: lo_menu     TYPE REF TO zcl_abapgit_html_toolbar,
          lo_sub_menu TYPE REF TO zcl_abapgit_html_toolbar.

    CREATE OBJECT lo_menu.

    CREATE OBJECT lo_sub_menu.

    lo_sub_menu->add(
      iv_txt = 'Sub Item 1'
      iv_act = 'DUMMY' ).
    lo_sub_menu->add(
      iv_txt = 'Sub Item 2'
      iv_act = 'DUMMY' ).
    lo_sub_menu->add(
      iv_txt = 'Sub Item 3'
      iv_act = 'DUMMY' ).
    lo_sub_menu->add(
      iv_txt = 'Sub Item 4'
      iv_act = 'DUMMY' ).

    lo_menu->add(
      iv_txt = 'Submenu'
      io_sub = lo_sub_menu ).

    ii_html->add( '<h1>Menus</h1>' ).
    ii_html->add( lo_menu->render( ) ).
  ENDMETHOD.

  METHOD render_input.
    ii_html->add( '<h1>Input</h1>' ).
    ii_html->add( '<p>' ).
    ii_html->add( '<label for="inputText">Text</label><input type="text" id="inputText" value="Value" /><br>' ).
    ii_html->add( '<p/>' ).
    ii_html->add( '<p>' ).
    ii_html->add( '<label><input type="checkbox">Checkbox</label><br>' ).
    ii_html->add( '</p>' ).
    ii_html->add( '<p>' ).
    ii_html->add( '<label for="select">Value List</label>' ).
    ii_html->add( '<select id="select" size="3">' ).
    ii_html->add( '<option value="value1">Value 1</option>' ).
    ii_html->add( '<option value="value2">Value 2</option>' ).
    ii_html->add( '<option value="value3">Value 3</option>' ).
    ii_html->add( '</select><br>' ).
    ii_html->add( '</p>' ).
    ii_html->add( '<p>' ).
    ii_html->add( '<label for="select2">Dropdown</label>' ).
    ii_html->add( '<select id="select2">' ).
    ii_html->add( '<option value="value1">Value 1</option>' ).
    ii_html->add( '<option value="value2">Value 2</option>' ).
    ii_html->add( '<option value="value3">Value 3</option>' ).
    ii_html->add( '</select><br>' ).
    ii_html->add( '</p>' ).
    ii_html->add( '<p>' ).
    ii_html->add( '<input type="submit" value="Button">' ).
    ii_html->add( '</p>' ).
  ENDMETHOD.

  METHOD render_form.
    DATA: lo_form TYPE REF TO zcl_abapgit_html_form.

    ii_html->add( '<h1>Forms</h1>' ).

    IF mo_form_values IS NOT BOUND.
      CREATE OBJECT mo_form_values.
    ENDIF.

    lo_form = zcl_abapgit_html_form=>create( 'dummy-form' ).
    lo_form->text(
      iv_name          = 'text'
      iv_label         = 'Simple text'
    )->text(
      iv_name          = 'text2'
      iv_label         = 'Obligatory text with hint and placeholder'
      iv_hint          = 'Explaining hint for obligatory text'
      iv_placeholder   = 'Input value here'
      iv_required      = abap_true
    )->text(
      iv_name          = 'text3'
      iv_label         = 'Text with side action'
      iv_side_action   = 'dummy'
    )->radio(
      iv_name          = 'radio'
      iv_label         = 'Radio buttons'
      iv_default_value = 'optiona'
    )->option(
      iv_label         = 'Option A'
      iv_value         = 'optiona'
    )->option(
      iv_label         = 'Option B'
      iv_value         = 'optionb'
    )->checkbox(
      iv_name          = 'checkbox'
      iv_label         = 'Checkbox'
    )->command(
      iv_label         = 'Submit'
      iv_action        = 'dummy'
      iv_is_main       = abap_true
    )->command(
      iv_label         = 'Cancel'
      iv_action        = 'dummy' ).

    ii_html->add( lo_form->render(
      iv_form_class = 'dialog w600px'
      io_values     = mo_form_values ) ).
  ENDMETHOD.

  METHOD render_icons.
    ii_html->add( '<h1>Logos and Icons</h1>' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'git-alt' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'abapgit' ).
    ii_html->add( '<br>' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'arrow-circle-up' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'bars' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'bolt' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'box' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'briefcase' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'check' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'chevron-down' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'chevron-left' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'chevron-right' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'chevron-up' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'cloud-upload-alt' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'code-branch' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'exclamation-circle' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'exclamation-triangle' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'file-alt' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'file-code' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'file-image' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'file' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'fire-alt' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'folder' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'lock' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'plug' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'question-circle-solid' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'sliders-h' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'snowflake' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'star' ).
    render_icon(
      ii_html = ii_html
      iv_name = 'tools-solid' ).
  ENDMETHOD.

  METHOD render_icon.
    ii_html->add_icon(
      iv_name  = iv_name
      iv_hint  = iv_name
      iv_class = 'large' ).
  ENDMETHOD.

  METHOD zif_abapgit_gui_event_handler~on_event.
    DATA: lo_settings_persistence TYPE REF TO zcl_abapgit_persist_settings,
          lo_settings             TYPE REF TO zcl_abapgit_settings.

    CASE ii_event->mv_action.
      WHEN c_action-change_theme.
        lo_settings_persistence = zcl_abapgit_persist_settings=>get_instance( ).
        lo_settings = lo_settings_persistence->read( ).
        lo_settings->set_ui_theme( ii_event->mv_getdata ).
        lo_settings_persistence->modify( lo_settings ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
