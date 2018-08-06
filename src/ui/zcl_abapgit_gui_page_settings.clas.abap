CLASS zcl_abapgit_gui_page_settings DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_action,
        save_settings TYPE string VALUE 'save_settings',
      END OF c_action.

    METHODS constructor.
    METHODS zif_abapgit_gui_page~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.

    DATA:
      mo_settings TYPE REF TO zcl_abapgit_settings,
      mv_error    TYPE abap_bool.

    METHODS render_proxy
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_development_internals
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_form_begin
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_form_end
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_max_lines
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_adt_jump_enabled
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_commit_msg
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
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
        zcx_abapgit_exception.
    METHODS read_settings.
    METHODS render_section_begin
      IMPORTING
                iv_header      TYPE csequence
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_section_end
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_start_up
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_link_hints
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception.


ENDCLASS.



CLASS zcl_abapgit_gui_page_settings IMPLEMENTATION.


  METHOD build_settings.

    DATA: lv_i_param_value         TYPE i.
    FIELD-SYMBOLS: <ls_post_field> TYPE ihttpnvp.


    CREATE OBJECT mo_settings.
    READ TABLE it_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'proxy_url'.
    IF sy-subrc <> 0.
      mv_error = abap_true.
    ENDIF.
    mo_settings->set_proxy_url( <ls_post_field>-value ).

    READ TABLE it_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'proxy_port'.
    IF sy-subrc <> 0.
      mv_error = abap_true.
    ENDIF.
    mo_settings->set_proxy_port( <ls_post_field>-value ).

    READ TABLE it_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'proxy_auth'.
    IF sy-subrc = 0.
      mo_settings->set_proxy_authentication( abap_true ).
    ELSE.
      mo_settings->set_proxy_authentication( abap_false ).
    ENDIF.

    READ TABLE it_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'critical_tests'.
    IF sy-subrc = 0.
      mo_settings->set_run_critical_tests( abap_true ).
    ELSE.
      mo_settings->set_run_critical_tests( abap_false ).
    ENDIF.

    READ TABLE it_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'experimental_features'.
    IF sy-subrc = 0.
      mo_settings->set_experimental_features( abap_true ).
    ELSE.
      mo_settings->set_experimental_features( abap_false ).
    ENDIF.

    READ TABLE it_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'show_default_repo'.
    IF sy-subrc = 0.
      mo_settings->set_show_default_repo( abap_true ).
    ELSE.
      mo_settings->set_show_default_repo( abap_false ).
    ENDIF.

    READ TABLE it_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'max_lines'.
    IF sy-subrc = 0.
      lv_i_param_value = <ls_post_field>-value.
      mo_settings->set_max_lines( lv_i_param_value ).
    ELSE.
      mo_settings->set_max_lines( 0 ).
    ENDIF.

    READ TABLE it_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'adt_jump_enabled'.
    IF sy-subrc = 0.
      mo_settings->set_adt_jump_enanbled( abap_true ).
    ELSE.
      mo_settings->set_adt_jump_enanbled( abap_false ).
    ENDIF.

    READ TABLE it_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'link_hints_enabled'.
    IF sy-subrc = 0.
      mo_settings->set_link_hints_enabled( abap_true ).
    ELSE.
      mo_settings->set_link_hints_enabled( abap_false ).
    ENDIF.

    READ TABLE it_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'link_hint_key'.
    IF sy-subrc = 0.
      mo_settings->set_link_hint_key( |{ <ls_post_field>-value }| ).
    ENDIF.

    READ TABLE it_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'link_hint_background_color'.
    IF sy-subrc = 0.
      mo_settings->set_link_hint_background_color( |{ <ls_post_field>-value }| ).
    ENDIF.

    READ TABLE it_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'comment_length'.
    IF sy-subrc = 0.
      lv_i_param_value = <ls_post_field>-value.
      IF lv_i_param_value < zcl_abapgit_settings=>c_commitmsg_comment_length_dft.
        lv_i_param_value = zcl_abapgit_settings=>c_commitmsg_comment_length_dft.
      ENDIF.
      mo_settings->set_commitmsg_comment_length( lv_i_param_value ).
    ELSE.
      mo_settings->set_commitmsg_comment_length( zcl_abapgit_settings=>c_commitmsg_comment_length_dft ).
    ENDIF.

    READ TABLE it_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'body_size'.
    IF sy-subrc = 0.
      lv_i_param_value = <ls_post_field>-value.
      IF lv_i_param_value < zcl_abapgit_settings=>c_commitmsg_body_size_dft.
        lv_i_param_value = zcl_abapgit_settings=>c_commitmsg_body_size_dft.
      ENDIF.
      mo_settings->set_commitmsg_body_size( lv_i_param_value ).
    ELSE.
      mo_settings->set_commitmsg_body_size( zcl_abapgit_settings=>c_commitmsg_body_size_dft ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'SETTINGS'.
  ENDMETHOD.  " constructor.


  METHOD parse_post.

    DATA lv_serialized_post_data TYPE string.

    CONCATENATE LINES OF it_postdata INTO lv_serialized_post_data.
    rt_post_fields = zcl_abapgit_html_action_utils=>parse_fields( lv_serialized_post_data ).

  ENDMETHOD.


  METHOD persist_settings.

    DATA lo_settings_persistence TYPE REF TO zcl_abapgit_persist_settings.

    lo_settings_persistence = zcl_abapgit_persist_settings=>get_instance( ).
    lo_settings_persistence->modify( mo_settings ).
    MESSAGE 'Settings succesfully saved' TYPE 'S'.

  ENDMETHOD.


  METHOD read_settings.

    DATA lo_settings_persistence TYPE REF TO zcl_abapgit_persist_settings.

    lo_settings_persistence = zcl_abapgit_persist_settings=>get_instance( ).
    mo_settings = lo_settings_persistence->read( ).

  ENDMETHOD.


  METHOD render_adt_jump_enabled.

    DATA lv_checked TYPE string.

    IF mo_settings->get_adt_jump_enabled( ) = abap_true.
      lv_checked = 'checked'.
    ENDIF.

    CREATE OBJECT ro_html.
    ro_html->add( |<h2>ABAP Development Tools (ADT)</h2>| ).
    ro_html->add( `<input type="checkbox" name="adt_jump_enabled" value="X" `
                   && lv_checked && ` > Enable jump to ADT first` ).
    ro_html->add( |<br>| ).
    ro_html->add( |<br>| ).
  ENDMETHOD.


  METHOD render_commit_msg.
    CREATE OBJECT ro_html.

    ro_html->add( |<h2>Commit Message</h2>| ).
    ro_html->add( |<label for="comment_length">Max. length of comment (recommendation 50)</label>| ).
    ro_html->add( |<br>| ).
    ro_html->add( |<input name="comment_length" type="number" step="10" size="3" maxlength="3" min="50"| &&
                  | value="{ mo_settings->get_commitmsg_comment_length( ) }">| ).
    ro_html->add( |<br>| ).
    ro_html->add( |<label for="body_size">Max. line size of body (recommendation 72)</label>| ).
    ro_html->add( |<br>| ).
    ro_html->add( |<input name="body_size" type="number" size="3" maxlength="3" min="50"| &&
                  | value="{ mo_settings->get_commitmsg_body_size( ) }">| ).
    ro_html->add( |<br>| ).
    ro_html->add( |<br>| ).
  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ro_html.

    read_settings( ).

    ro_html->add( render_form_begin( ) ).
    ro_html->add( render_section_begin( |Global settings| ) ).
    ro_html->add( render_proxy( ) ).
    ro_html->add( |<hr>| ).
    ro_html->add( render_commit_msg( ) ).
    ro_html->add( |<hr>| ).
    ro_html->add( render_development_internals( ) ).
    ro_html->add( render_section_end( ) ).
    ro_html->add( render_section_begin( |User specific settings| ) ).
    ro_html->add( render_start_up( ) ).
    ro_html->add( render_max_lines( ) ).
    ro_html->add( |<hr>| ).
    ro_html->add( render_adt_jump_enabled( ) ).
    ro_html->add( |<hr>| ).
    ro_html->add( render_link_hints( ) ).
    ro_html->add( render_section_end( ) ).
    ro_html->add( render_form_end( ) ).

  ENDMETHOD.  "render_content


  METHOD render_development_internals.

    DATA: lv_critical_tests TYPE string,
          lv_experimental   TYPE string.

    IF mo_settings->get_run_critical_tests( ) = abap_true.
      lv_critical_tests = 'checked'.
    ENDIF.

    IF mo_settings->get_experimental_features( ) = abap_true.
      lv_experimental = 'checked'.
    ENDIF.

    CREATE OBJECT ro_html.
    ro_html->add( |<h2>abapGit Development Internals settings</h2>| ).
    ro_html->add( `<input type="checkbox" name="critical_tests" `
                   && lv_critical_tests && ` > Enable critical unit tests (see LTCL_DANGEROUS)` ).
    ro_html->add( |<br>| ).
    ro_html->add( `<input type="checkbox" name="experimental_features" `
                   && lv_experimental && ` > Enable experimental features` ).
    ro_html->add( |<br>| ).
    ro_html->add( |<br>| ).

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


  METHOD render_max_lines.
    CREATE OBJECT ro_html.

    ro_html->add( |<h2>List size</h2>| ).
    ro_html->add( |<label for="max_lines">Max. # of objects listed (0 = all)</label>| ).
    ro_html->add( |<br>| ).
    ro_html->add( `<input name="max_lines" type="text" size="5" value="` && mo_settings->get_max_lines( ) && `">` ).
    ro_html->add( |<br>| ).
    ro_html->add( |<br>| ).
  ENDMETHOD.


  METHOD render_proxy.

    CREATE OBJECT ro_html.

    ro_html->add( |<h2>Proxy</h2>| ).
    ro_html->add( |<label for="proxy_url">Proxy URL</label>| ).
    ro_html->add( |<br>| ).
    ro_html->add( `<input name="proxy_url" type="text" size="50" value="` &&
      mo_settings->get_proxy_url( ) && `">` ).
    ro_html->add( |<br>| ).
    ro_html->add( |<label for="proxy_port">Proxy Port</label>| ).
    ro_html->add( |<br>| ).
    ro_html->add( `<input name="proxy_port" type="text" size="5" value="` &&
      mo_settings->get_proxy_port( ) && `">` ).
    ro_html->add( |<br>| ).
    ro_html->add( |<label for="proxy_auth">Proxy Authentication</label>| ).
    IF mo_settings->get_proxy_authentication( ) = abap_true.
      ro_html->add( `<input name="proxy_auth" type="checkbox" checked>` ).
    ELSE.
      ro_html->add( `<input name="proxy_auth" type="checkbox">` ).
    ENDIF.
    ro_html->add( |<br>| ).

    ro_html->add( |<br>| ).

  ENDMETHOD.


  METHOD validate_settings.

    IF ( mo_settings->get_proxy_url( ) IS NOT INITIAL AND  mo_settings->get_proxy_port( ) IS INITIAL ) OR
                 ( mo_settings->get_proxy_url( ) IS INITIAL AND  mo_settings->get_proxy_port( ) IS NOT INITIAL ).
      MESSAGE 'If specifying proxy, specify both URL and port' TYPE 'W'.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page~on_event.
* todo, check input values eg INT

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

        ev_state = zif_abapgit_definitions=>gc_event_state-go_back.
    ENDCASE.

  ENDMETHOD.

  METHOD render_section_begin.

    CREATE OBJECT ro_html.

    ro_html->add( |<h1>{ iv_header }</h1>| ).
    ro_html->add( |<div class="settings_section">| ).

  ENDMETHOD.


  METHOD render_section_end.

    CREATE OBJECT ro_html.

    ro_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_start_up.

    DATA lv_checked TYPE string.

    IF mo_settings->get_show_default_repo( ) = abap_true.
      lv_checked = 'checked'.
    ENDIF.

    CREATE OBJECT ro_html.
    ro_html->add( |<h2>Start up</h2>| ).
    ro_html->add( `<input type="checkbox" name="show_default_repo" value="X" `
                   && lv_checked && ` > Show last repo` ).
    ro_html->add( |<br>| ).
    ro_html->add( |<br>| ).
  ENDMETHOD.


  METHOD render_link_hints.

    DATA: lv_checked               TYPE string,
          lv_link_hint_key         TYPE char01,
          lv_link_background_color TYPE string.

    IF mo_settings->get_link_hints_enabled( ) = abap_true.
      lv_checked = 'checked'.
    ENDIF.

    lv_link_hint_key = mo_settings->get_link_hint_key( ).
    lv_link_background_color = mo_settings->get_link_hint_background_color( ).

    CREATE OBJECT ro_html.
    ro_html->add( |<h2>Vimium like link hints</h2>| ).
    ro_html->add( `<input type="checkbox" name="link_hints_enabled" value="X" `
                   && lv_checked && ` > Enable Vimium like link hints` ).
    ro_html->add( |<br>| ).
    ro_html->add( |<br>| ).
    ro_html->add( |<input type="text" name="link_hint_key" size="1" maxlength="1" value="{ lv_link_hint_key }" |
               && |> Single key to activate links| ).
    ro_html->add( |<br>| ).
    ro_html->add( |<br>| ).
    ro_html->add( |<input type="text" name="link_hint_background_color" size="20" maxlength="20"|
               && | value="{ lv_link_background_color }"|
               && |> Background Color (HTML colors e.g. lightgreen or #42f47a)| ).

    ro_html->add( |<br>| ).
    ro_html->add( |<br>| ).

  ENDMETHOD.

ENDCLASS.
