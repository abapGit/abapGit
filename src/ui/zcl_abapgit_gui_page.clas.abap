CLASS zcl_abapgit_gui_page DEFINITION PUBLIC ABSTRACT
  INHERITING FROM zcl_abapgit_gui_component
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_gui_renderable,
      zif_abapgit_gui_event_handler,
      zif_abapgit_gui_error_handler.

    METHODS:
      constructor RAISING zcx_abapgit_exception.

  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_control,
        page_title TYPE string,
        page_menu  TYPE REF TO zcl_abapgit_html_toolbar,
      END OF  ty_control .

    DATA ms_control TYPE ty_control .

    METHODS render_content
          ABSTRACT
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    DATA mo_settings TYPE REF TO zcl_abapgit_settings .
    DATA mx_error TYPE REF TO zcx_abapgit_exception .
    DATA mo_exception_viewer TYPE REF TO zcl_abapgit_exception_viewer .

    METHODS render_deferred_parts
      IMPORTING
        !ii_html          TYPE REF TO zif_abapgit_html
        !iv_part_category TYPE string
      RAISING
        zcx_abapgit_exception .
    METHODS html_head
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS title
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS footer
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_link_hints
      IMPORTING
        !ii_html TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_command_palettes
      IMPORTING
        !ii_html TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_hotkey_overview
      RETURNING
        VALUE(ro_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_error_message_box
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS scripts
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).
    mo_settings = zcl_abapgit_persist_settings=>get_instance( )->read( ).

  ENDMETHOD.


  METHOD footer.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div id="footer">' ).
    ri_html->add( '<table class="w100"><tr>' ).

    ri_html->add( '<td class="w40"></td>' ).  " spacer

    ri_html->add( '<td class="center">' ).
    ri_html->add( '<div class="logo">' ).
    ri_html->add( ri_html->icon( 'git-alt' ) ).
    ri_html->add( ri_html->icon( 'abapgit' ) ).
    ri_html->add( '</div>' ).
    ri_html->add( |<div class="version">{ zif_abapgit_version=>gc_abap_version }</div>| ).
    ri_html->add( '</td>' ).

    ri_html->add( '<td id="debug-output" class="w40"></td>' ).

    ri_html->add( '</tr></table>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD html_head.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<head>' ).

    ri_html->add( '<meta http-equiv="content-type" content="text/html; charset=utf-8">' ).
    ri_html->add( '<meta http-equiv="X-UA-Compatible" content="IE=11,10,9,8" />' ).

    ri_html->add( '<title>abapGit</title>' ).
    ri_html->add( '<link rel="stylesheet" type="text/css" href="css/common.css">' ).
    ri_html->add( '<link rel="stylesheet" type="text/css" href="css/ag-icons.css">' ).

    " Themes
    ri_html->add( '<link rel="stylesheet" type="text/css" href="css/theme-default.css">' ). " Theme basis
    CASE mo_settings->get_ui_theme( ).
      WHEN zcl_abapgit_settings=>c_ui_theme-dark.
        ri_html->add( '<link rel="stylesheet" type="text/css" href="css/theme-dark.css">' ).
      WHEN zcl_abapgit_settings=>c_ui_theme-belize.
        ri_html->add( '<link rel="stylesheet" type="text/css" href="css/theme-belize-blue.css">' ).
    ENDCASE.

    ri_html->add( '<script type="text/javascript" src="js/common.js"></script>' ).

    CASE mo_settings->get_icon_scaling( ). " Enforce icon scaling
      WHEN mo_settings->c_icon_scaling-large.
        ri_html->add( '<style>.icon { font-size: 200% }</style>' ).
      WHEN mo_settings->c_icon_scaling-small.
        ri_html->add( '<style>.icon.large { font-size: inherit }</style>' ).
    ENDCASE.

    ri_html->add( '</head>' ).

  ENDMETHOD.


  METHOD render_command_palettes.

    ii_html->add( 'var gCommandPalette = new CommandPalette(enumerateToolbarActions, {' ).
    ii_html->add( '  toggleKey: "F1",' ).
    ii_html->add( '  hotkeyDescription: "Command ..."' ).
    ii_html->add( '});' ).

  ENDMETHOD.


  METHOD render_deferred_parts.

    DATA lt_parts TYPE zif_abapgit_html=>ty_table_of.
    DATA li_part LIKE LINE OF lt_parts.

    lt_parts = gui_services( )->get_html_parts( )->get_parts( iv_part_category ).
    LOOP AT lt_parts INTO li_part.
      ii_html->add( li_part ).
    ENDLOOP.

  ENDMETHOD.


  METHOD render_error_message_box.

    " You should remember that the we have to instantiate ro_html even
    " it's overwritten further down. Because ADD checks whether it's
    " bound.
    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    " You should remember that we render the message panel only
    " if we have an error.
    IF mx_error IS NOT BOUND.
      RETURN.
    ENDIF.

    ri_html = zcl_abapgit_gui_chunk_lib=>render_error_message_box( mx_error ).

    " You should remember that the exception viewer dispatches the events of
    " error message panel
    CREATE OBJECT mo_exception_viewer
      EXPORTING
        ix_error = mx_error.

    " You should remember that we render the message panel just once
    " for each exception/error text.
    CLEAR:
      mx_error.

  ENDMETHOD.


  METHOD render_hotkey_overview.

    DATA lo_hotkeys_component TYPE REF TO zif_abapgit_gui_renderable.

    lo_hotkeys_component ?= gui_services( )->get_hotkeys_ctl( ). " Mmmm ...
    ro_html = lo_hotkeys_component->render( ).

  ENDMETHOD.


  METHOD render_link_hints.

    DATA: lv_link_hint_key TYPE c LENGTH 1.

    lv_link_hint_key = mo_settings->get_link_hint_key( ).

    IF mo_settings->get_link_hints_enabled( ) = abap_true AND lv_link_hint_key IS NOT INITIAL.

      ii_html->add( |activateLinkHints("{ lv_link_hint_key }");| ).
      ii_html->add( |setInitialFocusWithQuerySelector('a span', true);| ).
      ii_html->add( |enableArrowListNavigation();| ).

    ENDIF.

  ENDMETHOD.


  METHOD scripts.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    render_deferred_parts(
      ii_html          = ri_html
      iv_part_category = c_html_parts-scripts ).

    render_link_hints( ri_html ).
    render_command_palettes( ri_html ).

  ENDMETHOD.


  METHOD title.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div id="header">' ).

    ri_html->add( '<div class="logo">' ).
    ri_html->add( ri_html->icon( 'git-alt' ) ).
    ri_html->add( ri_html->icon( 'abapgit' ) ).
    ri_html->add( '</div>' ).

    ri_html->add( |<div class="page-title"><span class="spacer">&#x25BA;</span>{ ms_control-page_title }</div>| ).

    IF ms_control-page_menu IS BOUND.
      ri_html->add( '<div class="float-right">' ).
      ri_html->add( ms_control-page_menu->render( iv_right = abap_true ) ).
      ri_html->add( '</div>' ).
    ENDIF.

    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_error_handler~handle_error.

    mx_error = ix_error.
    rv_handled = abap_true.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-goto_source.

        IF mo_exception_viewer IS BOUND.
          mo_exception_viewer->goto_source( ).
        ENDIF.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN zif_abapgit_definitions=>c_action-show_callstack.

        IF mo_exception_viewer IS BOUND.
          mo_exception_viewer->show_callstack( ).
        ENDIF.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN zif_abapgit_definitions=>c_action-goto_message.

        IF mo_exception_viewer IS BOUND.
          mo_exception_viewer->goto_message( ).
        ENDIF.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA: li_script TYPE REF TO zif_abapgit_html.

    gui_services( )->register_event_handler( me ).

    " Real page
    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<!DOCTYPE html>' ).
    ri_html->add( '<html>' ).
    ri_html->add( html_head( ) ).
    ri_html->add( '<body>' ).
    ri_html->add( title( ) ).

    ri_html->add( render_content( ) ). " TODO -> render child

    ri_html->add( render_hotkey_overview( ) ).
    ri_html->add( render_error_message_box( ) ).

    render_deferred_parts(
      ii_html          = ri_html
      iv_part_category = c_html_parts-hidden_forms ).

    ri_html->add( footer( ) ).
    ri_html->add( '</body>' ).

    li_script = scripts( ).

    IF li_script IS BOUND AND li_script->is_empty( ) = abap_false.
      ri_html->add( '<script type="text/javascript">' ).
      ri_html->add( li_script ).
      ri_html->add( 'confirmInitialized();' ).
      ri_html->add( '</script>' ).
    ENDIF.

    ri_html->add( '</html>' ).

  ENDMETHOD.
ENDCLASS.
