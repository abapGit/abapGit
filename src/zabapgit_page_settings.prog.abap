*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_SETTINGS
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_settings DEFINITION FINAL INHERITING FROM lcl_gui_page.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_action,
        save_settings TYPE string VALUE 'save_settings',
      END OF c_action.

    METHODS constructor.
    METHODS lif_gui_page~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.

    DATA:
      mo_settings TYPE REF TO lcl_settings,
      mv_error    TYPE abap_bool.

    METHODS render_proxy
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html.
    METHODS render_development_internals
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html.
    METHODS render_form_begin
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html.
    METHODS render_form_end
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html.
    METHODS render_max_lines
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html.
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

ENDCLASS.

CLASS lcl_gui_page_settings IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'SETTINGS'.
  ENDMETHOD.  " constructor.

  METHOD render_content.

    CREATE OBJECT ro_html.

    read_settings( ).

    ro_html->add( render_form_begin( ) ).
    ro_html->add( render_proxy( ) ).
    ro_html->add( |<hr>| ).
    ro_html->add( render_development_internals( ) ).
    ro_html->add( |<hr>| ).
    ro_html->add( render_max_lines( ) ).
    ro_html->add( render_form_end( ) ).

  ENDMETHOD.  "render_content

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

    DATA: ls_post_field           TYPE ihttpnvp,
          lv_max_lines_as_integer TYPE i.

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

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'critical_tests'.
    IF sy-subrc = 0.
      mo_settings->set_run_critical_tests( abap_true ).
    ELSE.
      mo_settings->set_run_critical_tests( abap_false ).
    ENDIF.

    READ TABLE it_post_fields INTO ls_post_field WITH KEY name = 'max_lines'.
    IF sy-subrc = 0.
      lv_max_lines_as_integer = ls_post_field-value.
      mo_settings->set_max_lines( lv_max_lines_as_integer ).
    ELSE.
      mo_settings->set_max_lines( 0 ).
    ENDIF.

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

  METHOD render_development_internals.

    DATA lv_checked TYPE string.

    IF mo_settings->get_run_critical_tests( ) = abap_true.
      lv_checked = 'checked'.
    ENDIF.

    CREATE OBJECT ro_html.
    ro_html->add( |<h2>abapGit Development Internals settings</h2>| ).
    ro_html->add( `<input type="checkbox" name="critical_tests" value="X" `
                   && lv_checked && ` > Enable critical unit tests (see LTCL_DANGEROUS)` ).
    ro_html->add( |<br>| ).
    ro_html->add( |<br>| ).

  ENDMETHOD.

  METHOD render_max_lines.
    CREATE OBJECT ro_html.

    ro_html->add( |<h2>List size</h2>| ).
    ro_html->add( |<label for="max_lines">Max. # of objects listed (0 = all)</label>| ).
    ro_html->add( |<br>| ).
    ro_html->add( `<input name="max_lines" type="text" size="5" value="` && mo_settings->get_max_lines( ) && `">` ).
    ro_html->add( |<br>| ).
    ro_html->add( |<br>| ).
  ENDMETHOD.

ENDCLASS.
