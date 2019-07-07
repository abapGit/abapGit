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
      get_global_hotkeys
        RETURNING
          VALUE(rt_hotkey) TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_with_name.

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
    DATA: mo_settings         TYPE REF TO zcl_abapgit_settings,
          mt_hotkeys          TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_with_name,
          mx_error            TYPE REF TO zcx_abapgit_exception,
          mo_exception_viewer TYPE REF TO zcl_abapgit_exception_viewer.
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

    METHODS insert_hotkeys_to_page
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

    METHODS define_hotkeys
      RETURNING
        VALUE(rt_hotkeys) TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_with_name
      RAISING
        zcx_abapgit_exception.
    METHODS get_default_hotkeys
      RETURNING
        VALUE(rt_default_hotkeys) TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_with_name.
    METHODS render_message_box
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_gui_page IMPLEMENTATION.


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


  METHOD define_hotkeys.

    DATA: lo_settings             TYPE REF TO zcl_abapgit_settings,
          lt_user_defined_hotkeys TYPE zif_abapgit_definitions=>tty_hotkey.

    FIELD-SYMBOLS: <ls_hotkey>              TYPE zif_abapgit_gui_page_hotkey=>ty_hotkey_with_name,
                   <ls_user_defined_hotkey> LIKE LINE OF lt_user_defined_hotkeys.

    rt_hotkeys = get_default_hotkeys( ).

    " Override default hotkeys with user defined
    lo_settings = zcl_abapgit_persist_settings=>get_instance( )->read( ).
    lt_user_defined_hotkeys = lo_settings->get_hotkeys( ).

    LOOP AT rt_hotkeys ASSIGNING <ls_hotkey>.

      READ TABLE lt_user_defined_hotkeys ASSIGNING <ls_user_defined_hotkey>
                                         WITH TABLE KEY action
                                         COMPONENTS action = <ls_hotkey>-action.
      IF sy-subrc = 0.
        <ls_hotkey>-hotkey = <ls_user_defined_hotkey>-hotkey.
      ELSEIF lines( lt_user_defined_hotkeys ) > 0.
        " User removed the hotkey
        DELETE TABLE rt_hotkeys FROM <ls_hotkey>.
      ENDIF.

    ENDLOOP.

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


  METHOD get_default_hotkeys.

    DATA: lt_page_hotkeys LIKE mt_hotkeys.

    rt_default_hotkeys = get_global_hotkeys( ).

    TRY.
        CALL METHOD me->('ZIF_ABAPGIT_GUI_PAGE_HOTKEY~GET_HOTKEY_ACTIONS')
          RECEIVING
            rt_hotkey_actions = lt_page_hotkeys.

        INSERT LINES OF lt_page_hotkeys INTO TABLE rt_default_hotkeys.

      CATCH cx_root.
        " Current page doesn't implement hotkey interface, do nothing
    ENDTRY.

  ENDMETHOD.


  METHOD get_global_hotkeys.

    " these are the global shortcuts active on all pages

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey.

    ls_hotkey_action-name           = |Show hotkeys help|.
    ls_hotkey_action-action         = c_global_page_action-showhotkeys.
    ls_hotkey_action-hotkey = |?|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey.

  ENDMETHOD.


  METHOD html_head.

    CREATE OBJECT ro_html.

    ro_html->add( '<head>' ).                               "#EC NOTEXT

    ro_html->add( '<meta http-equiv="content-type" content="text/html; charset=utf-8">' ). "#EC NOTEXT
    ro_html->add( '<meta http-equiv="X-UA-Compatible" content="IE=11,10,9,8" />' ). "#EC NOTEXT

    ro_html->add( '<title>abapGit</title>' ).               "#EC NOTEXT
    ro_html->add( '<link rel="stylesheet" type="text/css" href="css/common.css">' ).
    ro_html->add( '<link rel="stylesheet" type="text/css" href="css/ag-icons.css">' ).

    " Themes
    ro_html->add( '<link rel="stylesheet" type="text/css" href="css/theme-default.css">' ). " Theme basis
    CASE mo_settings->get_ui_theme( ).
      WHEN zcl_abapgit_settings=>c_ui_theme-dark.
        "TODO
      WHEN zcl_abapgit_settings=>c_ui_theme-belize.
        ro_html->add( '<link rel="stylesheet" type="text/css" href="css/theme-belize-blue.css">' ).
    ENDCASE.

    ro_html->add( '<script type="text/javascript" src="js/common.js"></script>' ). "#EC NOTEXT

    CASE mo_settings->get_icon_scaling( ). " Enforce icon scaling
      WHEN mo_settings->c_icon_scaling-large.
        ro_html->add( '<style>.icon { font-size: 200% }</style>' ).
      WHEN mo_settings->c_icon_scaling-small.
        ro_html->add( '<style>.icon.large { font-size: inherit }</style>' ).
    ENDCASE.

    ro_html->add( '</head>' ).                              "#EC NOTEXT

  ENDMETHOD.


  METHOD insert_hotkeys_to_page.

    DATA: lv_json TYPE string.

    FIELD-SYMBOLS: <ls_hotkey> LIKE LINE OF mt_hotkeys.

    lv_json = `{`.

    LOOP AT mt_hotkeys ASSIGNING <ls_hotkey>.

      IF sy-tabix > 1.
        lv_json = lv_json && |,|.
      ENDIF.

      lv_json = lv_json && |  "{ <ls_hotkey>-hotkey }" : "{ <ls_hotkey>-action }" |.

    ENDLOOP.

    lv_json = lv_json && `}`.

    io_html->add( |setKeyBindings({ lv_json });| ).

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
    insert_hotkeys_to_page( ro_html ).
    ro_html->add( |errorMessagePanelRegisterClick();| ).

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

      WHEN  zif_abapgit_definitions=>c_action-goto_source.

        IF mo_exception_viewer IS BOUND.
          mo_exception_viewer->goto_source( ).
        ENDIF.
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN  zif_abapgit_definitions=>c_action-callstack.

        IF mo_exception_viewer IS BOUND.
          mo_exception_viewer->callstack( ).
        ENDIF.
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN zif_abapgit_definitions=>c_action-goto_message.

        IF mo_exception_viewer IS BOUND.
          mo_exception_viewer->goto_message( ).
        ENDIF.
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN OTHERS.

        ev_state = zcl_abapgit_gui=>c_event_state-not_handled.

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA: lo_script TYPE REF TO zcl_abapgit_html.

    " Redirect
    IF ms_control-redirect_url IS NOT INITIAL.
      ro_html = redirect( ).
      RETURN.
    ENDIF.

    mt_hotkeys = define_hotkeys( ).

    " Real page
    CREATE OBJECT ro_html TYPE zcl_abapgit_html.

    ro_html->add( '<!DOCTYPE html>' ).                      "#EC NOTEXT
    ro_html->add( '<html>' ).                               "#EC NOTEXT
    ro_html->add( html_head( ) ).
    ro_html->add( '<body>' ).                               "#EC NOTEXT
    ro_html->add( title( ) ).
    ro_html->add( render_hotkey_overview( ) ).
    ro_html->add( render_content( ) ).
    ro_html->add( render_message_box( ) ).
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


  METHOD zif_abapgit_gui_renderable~show_error.

    mx_error = ix_error.

  ENDMETHOD.


  METHOD render_message_box.

    DATA:
      lv_error_text TYPE string,
      lv_longtext   TYPE string.

    CREATE OBJECT ro_html.

    " You should remember that we render the message panel only
    " if we have an error text.

    IF mx_error IS BOUND.

      lv_error_text = mx_error->get_text( ).
      lv_longtext = mx_error->get_longtext( abap_true ).

      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
              IN lv_longtext
              WITH '<br>'.

      REPLACE FIRST OCCURRENCE OF REGEX `(.{5})`
              IN lv_longtext
              WITH `<a id="a_goto_message" href="sapevent:goto_message">$1</a>`.

      REPLACE FIRST OCCURRENCE OF REGEX |(<br>{ zcx_abapgit_exception=>gc_section_text-cause }<br>)|
              IN lv_longtext
              WITH |<h3>$1</h3>|.

      REPLACE FIRST OCCURRENCE OF REGEX |(<br>{ zcx_abapgit_exception=>gc_section_text-system_response }<br>)|
              IN lv_longtext
              WITH |<h3>$1</h3>|.

      REPLACE FIRST OCCURRENCE OF REGEX |(<br>{ zcx_abapgit_exception=>gc_section_text-what_to_do }<br>)|
              IN lv_longtext
              WITH |<h3>$1</h3>|.

      REPLACE FIRST OCCURRENCE OF REGEX |(<br>{ zcx_abapgit_exception=>gc_section_text-sys_admin }<br>)|
              IN lv_longtext
              WITH |<h3>$1</h3>|.

      ro_html->add( |<div id="message" class="message-panel-fixed">|
                 && |  <div class="message-panel-border">|
                 && |    <div class="message-panel-outer">|
                 && |      <div id="message-header" class="message-panel-inner message-header">{ lv_error_text }|
                 && |        <div class="float-right">|
                 && |          <a class="close-btn" href="#" onclick="toggleDisplay('message')">|
                 && |            &#x274c;|
                 && |            <span class="tooltiptext hidden"></span>|
                 && |          </a>|
                 && |        </div>|
                 && |      </div>|
                 && |      <div id="message-detail" class="message-panel-inner" style="display:none" >|
                 && |        { lv_longtext  }|
                 && |        <br>|
                 && |        <br><a id="a_goto_source" href="sapevent:goto_source">Goto source</a>|
                 && |        <br><a id="a_callstack" href="sapevent:callstack">Callstack</a>|
                 && |      </div>|
                 && |    </div>|
                 && |  </div>|
                 && |</div>| ).

      CREATE OBJECT mo_exception_viewer
        EXPORTING
          ix_error = mx_error.

      " You should remember that we render the message panel just once
      " for each exception/error text.
      CLEAR:
        mx_error.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
