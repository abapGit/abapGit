CLASS zcl_abapgit_gui_page DEFINITION PUBLIC ABSTRACT CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_gui_renderable,
      zif_abapgit_gui_event_handler.

    CONSTANTS:
      BEGIN OF c_global_page_action,
        showhotkeys TYPE string VALUE `showHotkeys` ##NO_TEXT,
      END OF c_global_page_action.

    CLASS-METHODS:
      get_hotkey_actions
        RETURNING
          VALUE(rt_hotkey_actions) TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_action.

    METHODS:
      constructor.

  PROTECTED SECTION.

    TYPES: BEGIN OF ty_control,
             redirect_url TYPE string,
             page_title   TYPE string,
             page_menu    TYPE REF TO zcl_abapgit_html_toolbar,
           END OF  ty_control.

    DATA: ms_control TYPE ty_control.

    METHODS render_content ABSTRACT
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING   zcx_abapgit_exception.

    METHODS scripts
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA: mo_settings         TYPE REF TO zcl_abapgit_settings.
    METHODS html_head
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.

    METHODS title
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.

    METHODS footer
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.

    METHODS redirect
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.

    METHODS link_hints
      IMPORTING
        io_html TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS add_hotkeys
      IMPORTING
        io_html TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS render_hotkey_overview
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception.

    METHODS call_browser
      IMPORTING
        iv_url TYPE csequence
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE IMPLEMENTATION.


  METHOD add_hotkeys.

    DATA: lv_json    TYPE string,
          lt_hotkeys TYPE zif_abapgit_definitions=>tty_hotkey.

    FIELD-SYMBOLS: <ls_hotkey> TYPE zif_abapgit_definitions=>ty_hotkey.

    lt_hotkeys = zcl_abapgit_hotkeys=>get_relevant_hotkeys_for_page( me ).

    lv_json = `{`.

    LOOP AT lt_hotkeys ASSIGNING <ls_hotkey>.

      IF sy-tabix > 1.
        lv_json = lv_json && |,|.
      ENDIF.

      lv_json = lv_json && |  "{ <ls_hotkey>-sequence }" : "{ <ls_hotkey>-action }" |.

    ENDLOOP.

    lv_json = lv_json && `}`.

    io_html->add( |setKeyBindings({ lv_json });| ).

  ENDMETHOD.


  METHOD call_browser.

    cl_gui_frontend_services=>execute(
      EXPORTING
        document               = |{ iv_url }|
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10 ).

    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    mo_settings = zcl_abapgit_persist_settings=>get_instance( )->read( ).

  ENDMETHOD.


  METHOD footer.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="footer">' ).                    "#EC NOTEXT

    ro_html->add( '<img src="img/logo" alt="logo">' ).      "#EC NOTEXT
    ro_html->add( '<table class="w100"><tr>' ).             "#EC NOTEXT

    ro_html->add( '<td class="w40"></td>' ).                "#EC NOTEXT
    ro_html->add( |<td><span class="version">{ zif_abapgit_version=>gc_abap_version }</span></td>| ). "#EC NOTEXT
    ro_html->add( '<td id="debug-output" class="w40"></td>' ). "#EC NOTEXT

    ro_html->add( '</tr></table>' ).                        "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.


  METHOD get_hotkey_actions.

    " these are the global shortcuts active on all pages

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-name           = |Show hotkeys help|.
    ls_hotkey_action-action         = c_global_page_action-showhotkeys.
    ls_hotkey_action-default_hotkey = |?|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD html_head.

    CREATE OBJECT ro_html.

    ro_html->add( '<head>' ).                               "#EC NOTEXT

    ro_html->add( '<meta http-equiv="content-type" content="text/html; charset=utf-8">' ). "#EC NOTEXT
    ro_html->add( '<meta http-equiv="X-UA-Compatible" content="IE=11,10,9,8" />' ). "#EC NOTEXT

    ro_html->add( '<title>abapGit</title>' ).               "#EC NOTEXT
    ro_html->add( '<link rel="stylesheet" type="text/css" href="css/common.css">' ).
    ro_html->add( '<link rel="stylesheet" type="text/css" href="css/ag-icons.css">' ).
    ro_html->add( '<script type="text/javascript" src="js/common.js"></script>' ). "#EC NOTEXT

    CASE mo_settings->get_icon_scaling( ). " Enforce icon scaling
      WHEN mo_settings->c_icon_scaling-large.
        ro_html->add( '<style>.icon { font-size: 200% }</style>' ).
      WHEN mo_settings->c_icon_scaling-small.
        ro_html->add( '<style>.icon.large { font-size: inherit }</style>' ).
    ENDCASE.

    ro_html->add( '</head>' ).                              "#EC NOTEXT

  ENDMETHOD.


  METHOD link_hints.

    DATA: lv_link_hint_key    TYPE char01,
          lv_background_color TYPE string.

    lv_link_hint_key = mo_settings->get_link_hint_key( ).
    lv_background_color = mo_settings->get_link_hint_background_color( ).

    IF mo_settings->get_link_hints_enabled( ) = abap_true
    AND lv_link_hint_key IS NOT INITIAL.

      io_html->add( |setLinkHints("{ lv_link_hint_key }","{ lv_background_color }");| ).
      io_html->add( |setInitialFocusWithQuerySelector('a span', true);| ).
      io_html->add( |enableArrowListNavigation();| ).

    ENDIF.

  ENDMETHOD.


  METHOD redirect.

    CREATE OBJECT ro_html.

    ro_html->add( '<!DOCTYPE html>' ).                      "#EC NOTEXT
    ro_html->add( '<html>' ).                               "#EC NOTEXT
    ro_html->add( '<head>' ).                               "#EC NOTEXT
    ro_html->add( |<meta http-equiv="refresh" content="0; url={
                  ms_control-redirect_url }">| ).           "#EC NOTEXT
    ro_html->add( '</head>' ).                              "#EC NOTEXT
    ro_html->add( '</html>' ).                              "#EC NOTEXT

  ENDMETHOD.


  METHOD render_hotkey_overview.

    ro_html = zcl_abapgit_gui_chunk_lib=>render_hotkey_overview( me ).

  ENDMETHOD.


  METHOD scripts.

    CREATE OBJECT ro_html.

    link_hints( ro_html ).
    add_hotkeys( ro_html ).

  ENDMETHOD.


  METHOD title.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="header">' ).                    "#EC NOTEXT
    ro_html->add( '<table class="w100"><tr>' ).             "#EC NOTEXT

    ro_html->add( |<td class="logo">{
                  zcl_abapgit_html=>a( iv_txt = '<img src="img/logo" alt="logo">'
                                       iv_id  = 'abapGitLogo'
                                       iv_act = zif_abapgit_definitions=>c_action-abapgit_home )
                  }</td>| ).                                "#EC NOTEXT

    ro_html->add( |<td><span class="page_title"> &#x25BA; {
                  ms_control-page_title
                  }</span></td>| ).                         "#EC NOTEXT

    IF ms_control-page_menu IS BOUND.
      ro_html->add( '<td class="right">' ).                 "#EC NOTEXT
      ro_html->add( ms_control-page_menu->render( iv_right = abap_true ) ).
      ro_html->add( '</td>' ).                              "#EC NOTEXT
    ENDIF.

    ro_html->add( '</tr></table>' ).                        "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE iv_action.
      WHEN zif_abapgit_definitions=>c_action-url.

        call_browser( iv_getdata ).
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN OTHERS.

        ev_state = zcl_abapgit_gui=>c_event_state-not_handled.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA lo_script TYPE REF TO zcl_abapgit_html.

    " Redirect
    IF ms_control-redirect_url IS NOT INITIAL.
      ro_html = redirect( ).
      RETURN.
    ENDIF.

    " Real page
    CREATE OBJECT ro_html TYPE zcl_abapgit_html.

    ro_html->add( '<!DOCTYPE html>' ).                      "#EC NOTEXT
    ro_html->add( '<html>' ).                               "#EC NOTEXT
    ro_html->add( html_head( ) ).
    ro_html->add( '<body>' ).                               "#EC NOTEXT
    ro_html->add( title( ) ).
    ro_html->add( render_hotkey_overview( ) ).
    ro_html->add( render_content( ) ).
    ro_html->add( footer( ) ).
    ro_html->add( '</body>' ).                              "#EC NOTEXT

    lo_script = scripts( ).

    IF lo_script IS BOUND AND lo_script->is_empty( ) = abap_false.
      ro_html->add( '<script type="text/javascript">' ).
      ro_html->add( lo_script ).
      ro_html->add( 'confirmInitialized();' ).
      ro_html->add( '</script>' ).
    ENDIF.

    ro_html->add( '</html>' ).                              "#EC NOTEXT

  ENDMETHOD.
ENDCLASS.
