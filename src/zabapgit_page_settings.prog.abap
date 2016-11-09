*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_SETTINGS
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_settings DEFINITION FINAL INHERITING FROM lcl_gui_page_super.
  PUBLIC SECTION.
    METHODS lif_gui_page~render REDEFINITION.
    METHODS lif_gui_page~on_event REDEFINITION.
    CONSTANTS:
      BEGIN OF c_action,
        save_settings TYPE string VALUE 'save_settings',
      END OF c_action.
  PRIVATE SECTION.
    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_proxy
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
ENDCLASS.

CLASS lcl_gui_page_settings IMPLEMENTATION.

  METHOD lif_gui_page~render.
    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'Settings' ) ).
    ro_html->add( render_proxy( ) ).
  ENDMETHOD.

  METHOD styles.
    CREATE OBJECT ro_html.

    _add '/* settings STYLES */'.
    _add 'div.settings_container {'.
    _add '  padding: 0.5em;'.
    _add '  font-size: 10pt;'.
    _add '  color: #444;'.
    _add '  background-color: #f2f2f2;'.
    _add '}'.
  ENDMETHOD.

  METHOD render_proxy.
    CREATE OBJECT ro_html.
    ro_html->add( '<div class="settings_container">' ).
    ro_html->add( `<form id="settings_form" method="post" action="sapevent:` && c_action-save_settings && `">` ).
    ro_html->add( |<h2>Proxy</h2>| ).
    ro_html->add( |<label for="proxy_url">Proxy URL</label>| ).
    ro_html->add( |<br>| ).
    ro_html->add( |<input name="proxy_url" type="text" size="50" value="">| ).
    ro_html->add( |<br>| ).
    ro_html->add( |<label for="proxy_port">Proxy Port</label>| ).
    ro_html->add( |<br>| ).
    ro_html->add( |<input name="proxy_port" type="text" size="5" value="">| ).
    ro_html->add( |<br>| ).
    ro_html->add( |<br>| ).

    ro_html->add( '<input type="submit" value="Save" class="submit">' ).
    ro_html->add( '</form>' ).

    ro_html->add( '</div>' ).
  ENDMETHOD.

  METHOD lif_gui_page~on_event.
    CASE iv_action.
      WHEN c_action-save_settings.
        DATA: lv_serialized_post_data TYPE string,
              lt_post_fields TYPE tihttpnvp.

        CONCATENATE LINES OF it_postdata INTO lv_serialized_post_data.
        lt_post_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_serialized_post_data ).

        ev_state = gc_event_state-go_back.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.