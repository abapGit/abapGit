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
    METHODS render_form_begin
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_form_end
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS build_settings
      IMPORTING
        it_post_fields TYPE tihttpnvp.
    METHODS validate_settings.
    METHODS parse_post
      IMPORTING
        it_postdata           TYPE cnht_post_data_tab
      RETURNING
        VALUE(rt_post_fields) TYPE tihttpnvp.
    METHODS persist_settings
      RAISING
        lcx_exception.
    METHODS read_settings.
    DATA:
      mo_settings TYPE REF TO lcl_settings,
      mv_error    TYPE abap_bool.
ENDCLASS.

CLASS lcl_gui_page_settings IMPLEMENTATION.

  METHOD lif_gui_page~render.
    CREATE OBJECT ro_html.

    read_settings( ).

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'Settings' ) ).

    ro_html->add( render_form_begin( ) ).
    ro_html->add( render_proxy( ) ).
    ro_html->add( render_form_end( ) ).
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
    ro_html->add( |<h2>Proxy</h2>| ).
    ro_html->add( |<label for="proxy_url">Proxy URL</label>| ).
    ro_html->add( |<br>| ).
    ro_html->add( `<input name="proxy_url" type="text" size="50" value="` && mo_settings->get_proxy_url( ) && `">` ).
    ro_html->add( |<br>| ).
    ro_html->add( |<label for="proxy_port">Proxy Port</label>| ).
    ro_html->add( |<br>| ).
    ro_html->add( `<input name="proxy_port" type="text" size="5" value="` && mo_settings->get_proxy_port( ) && `">` ).
    ro_html->add( |<br>| ).
    ro_html->add( |<br>| ).
  ENDMETHOD.

  METHOD lif_gui_page~on_event.
    DATA:
      lt_post_fields TYPE tihttpnvp.
    CASE iv_action.
      WHEN c_action-save_settings.
        lt_post_fields = parse_post( it_postdata ).

        build_settings( lt_post_fields ).

        validate_settings( ).

        IF mv_error = abap_true.
          MESSAGE 'Error when saving settings. Open an issue at https://github.com/larshp/abapGit' TYPE 'E'.
        ELSE.
          persist_settings( ).
        ENDIF.

        ev_state = gc_event_state-go_back.
    ENDCASE.
  ENDMETHOD.


  METHOD build_settings.
    DATA ls_post_field TYPE ihttpnvp.

    CREATE OBJECT mo_settings.
    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'proxy_url'.
    IF sy-subrc <> 0.
      mv_error = abap_true.
    ENDIF.
    mo_settings->set_proxy_url( ls_post_field-value ).

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'proxy_port'.
    IF sy-subrc <> 0.
      mv_error = abap_true.
    ENDIF.
    mo_settings->set_proxy_port( ls_post_field-value ).
  ENDMETHOD.


  METHOD validate_settings.
    IF ( mo_settings->get_proxy_url( ) IS NOT INITIAL AND  mo_settings->get_proxy_port( ) IS INITIAL ) OR
                 ( mo_settings->get_proxy_url( ) IS INITIAL AND  mo_settings->get_proxy_port( ) IS NOT INITIAL ).
      MESSAGE 'If specifying proxy, specify both URL and port' TYPE 'W'.
    ENDIF.
  ENDMETHOD.


  METHOD parse_post.
    DATA lv_serialized_post_data TYPE string.

    CONCATENATE LINES OF it_postdata INTO lv_serialized_post_data.
    rt_post_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_serialized_post_data ).
  ENDMETHOD.


  METHOD persist_settings.

    DATA lo_settings_persistence TYPE REF TO lcl_persistence_settings.

    lo_settings_persistence = lcl_app=>settings( ).
    lo_settings_persistence->modify( mo_settings ).
    MESSAGE 'Settings succesfully saved' TYPE 'S'.

  ENDMETHOD.

  METHOD render_form_begin.
    CREATE OBJECT ro_html.
    ro_html->add( '<div class="settings_container">' ).
    ro_html->add( `<form id="settings_form" method="post" action="sapevent:` && c_action-save_settings && `">` ).
  ENDMETHOD.

  METHOD render_form_end.
    CREATE OBJECT ro_html.
    ro_html->add( '<input type="submit" value="Save" class="submit">' ).
    ro_html->add( '</form>' ).
    ro_html->add( '</div>' ).
  ENDMETHOD.


  METHOD read_settings.

    DATA lo_settings_persistence TYPE REF TO lcl_persistence_settings.
    lo_settings_persistence = lcl_app=>settings( ).
    mo_settings = lo_settings_persistence->read( ).

  ENDMETHOD.

ENDCLASS.