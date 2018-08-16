CLASS zcl_abapgit_gui_page DEFINITION PUBLIC ABSTRACT CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_gui_page.

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

    METHODS hotkeys
      IMPORTING
        io_html TYPE REF TO zcl_abapgit_html.

    METHODS get_relevant_hotkeys
      RETURNING
        VALUE(rt_hotkeys) TYPE zif_abapgit_definitions=>tty_hotkey.

ENDCLASS.



CLASS zcl_abapgit_gui_page IMPLEMENTATION.


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

  ENDMETHOD. "footer


  METHOD html_head.

    CREATE OBJECT ro_html.

    ro_html->add( '<head>' ).                               "#EC NOTEXT

    ro_html->add( '<meta http-equiv="content-type" content="text/html; charset=utf-8">' ). "#EC NOTEXT
    ro_html->add( '<meta http-equiv="X-UA-Compatible" content="IE=11,10,9,8" />' ). "#EC NOTEXT

    ro_html->add( '<title>abapGit</title>' ).               "#EC NOTEXT
    ro_html->add( '<link rel="stylesheet" type="text/css" href="css/common.css">' ).
    ro_html->add( '<script type="text/javascript" src="js/common.js"></script>' ). "#EC NOTEXT

    ro_html->add( zcl_abapgit_gui_asset_manager=>get_webfont_link( ) ). " Web fonts

    ro_html->add( '</head>' ).                              "#EC NOTEXT

  ENDMETHOD.                    "html_head


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


  METHOD scripts.

    CREATE OBJECT ro_html.

    link_hints( ro_html ).
    hotkeys( ro_html ).

  ENDMETHOD. "scripts


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

  ENDMETHOD.                    "render page title


  METHOD zif_abapgit_gui_page~on_event.
    ev_state = zif_abapgit_definitions=>c_event_state-not_handled.
  ENDMETHOD. "lif_gui_page~on_event


  METHOD zif_abapgit_gui_page~render.

    DATA lo_script TYPE REF TO zcl_abapgit_html.

    " Redirect
    IF ms_control-redirect_url IS NOT INITIAL.
      ro_html = redirect( ).
      RETURN.
    ENDIF.

    " Real page
    CREATE OBJECT ro_html.

    ro_html->add( '<!DOCTYPE html>' ).                      "#EC NOTEXT
    ro_html->add( '<html>' ).                               "#EC NOTEXT
    ro_html->add( html_head( ) ).
    ro_html->add( '<body>' ).                               "#EC NOTEXT
    ro_html->add( title( ) ).
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

  ENDMETHOD.  " lif_gui_page~render.

  METHOD link_hints.

    DATA: lo_settings         TYPE REF TO zcl_abapgit_settings,
          lv_link_hint_key    TYPE char01,
          lv_background_color TYPE string.

    lo_settings = zcl_abapgit_persist_settings=>get_instance( )->read( ).

    lv_link_hint_key = lo_settings->get_link_hint_key( ).
    lv_background_color = lo_settings->get_link_hint_background_color( ).

    IF lo_settings->get_link_hints_enabled( ) = abap_true
    AND lv_link_hint_key IS NOT INITIAL.

      io_html->add( |setLinkHints("{ lv_link_hint_key }","{ lv_background_color }");| ).
      io_html->add( |setInitialFocusWithQuerySelector('a span', true);| ).
      io_html->add( |enableArrowListNavigation();| ).

    ENDIF.

  ENDMETHOD.


  METHOD hotkeys.

    DATA: lv_json    TYPE string,
          lt_hotkeys TYPE zif_abapgit_definitions=>tty_hotkey.

    FIELD-SYMBOLS: <ls_hotkey> TYPE zif_abapgit_definitions=>ty_hotkey.

    lt_hotkeys = get_relevant_hotkeys( ).

    IF lines( lt_hotkeys ) = 0.
      RETURN.
    ENDIF.

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


  METHOD get_relevant_hotkeys.

    DATA: lo_settings                    TYPE REF TO zcl_abapgit_settings,
          lv_class_name                  TYPE abap_abstypename,
          lt_hotkey_actions_of_curr_page TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_action,
          lv_save_tabix                  TYPE syst-tabix,
          ls_hotkey                      LIKE LINE OF rt_hotkeys.

    FIELD-SYMBOLS: <ls_hotkey>              TYPE zif_abapgit_definitions=>ty_hotkey,
                   <ls_hotkey_of_curr_page> TYPE zif_abapgit_gui_page_hotkey=>ty_hotkey_action.

    lo_settings = zcl_abapgit_persist_settings=>get_instance( )->read( ).

    rt_hotkeys = lo_settings->get_hotkeys( ).

    lv_class_name = cl_abap_classdescr=>get_class_name( me ).

    TRY.
        CALL METHOD (lv_class_name)=>zif_abapgit_gui_page_hotkey~get_hotkey_actions
          RECEIVING
            rt_hotkey_actions = lt_hotkey_actions_of_curr_page.

      CATCH cx_root.
        RETURN.
    ENDTRY.

    IF lines( rt_hotkeys ) = 0.
      " when no use defined hotkeys exist, we use the default
      LOOP AT lt_hotkey_actions_of_curr_page ASSIGNING <ls_hotkey_of_curr_page>.

        ls_hotkey-action   = <ls_hotkey_of_curr_page>-action.
        ls_hotkey-sequence = <ls_hotkey_of_curr_page>-default_hotkey.
        INSERT ls_hotkey INTO TABLE rt_hotkeys.

      ENDLOOP.

    ELSE.

      LOOP AT rt_hotkeys ASSIGNING <ls_hotkey>.

        lv_save_tabix = sy-tabix.

        READ TABLE lt_hotkey_actions_of_curr_page TRANSPORTING NO FIELDS
                                                  WITH KEY action = <ls_hotkey>-action.
        IF sy-subrc <> 0.
          " We only offer hotkeys which are supported by the current page
          DELETE rt_hotkeys INDEX lv_save_tabix.
        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
