CLASS zcl_abapgit_gui_page_settings DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_gui_page_hotkey.

    CONSTANTS:
      BEGIN OF c_action,
        save_settings TYPE string VALUE 'save_settings',
      END OF c_action.

    METHODS constructor.
    METHODS zif_abapgit_gui_event_handler~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.

    DATA mo_settings TYPE REF TO zcl_abapgit_settings .
    DATA mv_error TYPE abap_bool .
    DATA mt_post_fields TYPE tihttpnvp .

    METHODS post_commit_msg .
    METHODS post_development_internals .
    METHODS post_hotkeys .
    METHODS render_proxy
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_development_internals
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_form_begin
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_form_end
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_max_lines
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_icon_scaling
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_ui_theme
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_adt_jump_enabled
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_commit_msg
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS post_proxy .
    METHODS post
      IMPORTING
        !it_post_fields TYPE tihttpnvp .
    METHODS validate_settings .
    METHODS parse_post
      IMPORTING
        !it_postdata          TYPE cnht_post_data_tab
      RETURNING
        VALUE(rt_post_fields) TYPE tihttpnvp .
    METHODS persist_settings
      RAISING
        zcx_abapgit_exception .
    METHODS read_settings .
    METHODS render_section_begin
      IMPORTING
        !iv_header     TYPE csequence
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_section_end
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_start_up
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_link_hints
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_hotkeys
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS get_possible_hotkey_actions
      RETURNING
        VALUE(rt_hotkey_actions) TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_with_name
      RAISING
        zcx_abapgit_exception .
    METHODS get_default_hotkeys
      RETURNING
        VALUE(rt_default_hotkeys) TYPE zif_abapgit_definitions=>tty_hotkey
      RAISING
        zcx_abapgit_exception .
    METHODS is_post_field_checked
      IMPORTING
        iv_name          TYPE string
      RETURNING
        VALUE(rv_return) TYPE abap_bool .

    METHODS render_parallel_proc
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_SETTINGS IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'SETTINGS'.
  ENDMETHOD.


  METHOD get_default_hotkeys.

    DATA: lt_actions TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_with_name,
          ls_hotkey  LIKE LINE OF rt_default_hotkeys.

    FIELD-SYMBOLS: <ls_action> LIKE LINE OF lt_actions.

    lt_actions = zcl_abapgit_hotkeys=>get_all_default_hotkeys( ).

    LOOP AT lt_actions ASSIGNING <ls_action>.
      ls_hotkey-action   = <ls_action>-action.
      ls_hotkey-hotkey = <ls_action>-hotkey.
      INSERT ls_hotkey INTO TABLE rt_default_hotkeys.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_possible_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    rt_hotkey_actions = zcl_abapgit_hotkeys=>get_all_default_hotkeys( ).

    " insert empty row at the beginning, so that we can unset a hotkey
    INSERT ls_hotkey_action INTO rt_hotkey_actions INDEX 1.

  ENDMETHOD.


  METHOD is_post_field_checked.
    FIELD-SYMBOLS: <ls_post_field> TYPE ihttpnvp.
    READ TABLE mt_post_fields ASSIGNING <ls_post_field> WITH KEY name = iv_name.
    IF sy-subrc = 0
        AND ( <ls_post_field>-value = abap_true "HTML value when using standard netweaver GUI
        OR <ls_post_field>-value = 'on' ).     "HTML value when using Netweaver Java GUI
      rv_return = abap_true.
    ENDIF.
  ENDMETHOD.


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


  METHOD post.

    DATA lv_i_param_value TYPE i.
    DATA lv_c_param_value TYPE c.

    FIELD-SYMBOLS: <ls_post_field> TYPE ihttpnvp.


    CREATE OBJECT mo_settings.
    mt_post_fields = it_post_fields.


    post_proxy( ).
    post_commit_msg( ).
    post_development_internals( ).

* todo, refactor to private POST_* methods
    IF is_post_field_checked( 'show_default_repo' ) = abap_true.
      mo_settings->set_show_default_repo( abap_true ).
    ELSE.
      mo_settings->set_show_default_repo( abap_false ).
    ENDIF.

    READ TABLE mt_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'max_lines'.
    IF sy-subrc = 0.
      lv_i_param_value = <ls_post_field>-value.
      mo_settings->set_max_lines( lv_i_param_value ).
    ELSE.
      mo_settings->set_max_lines( 0 ).
    ENDIF.

    IF is_post_field_checked( 'adt_jump_enabled' ) = abap_true.
      mo_settings->set_adt_jump_enanbled( abap_true ).
    ELSE.
      mo_settings->set_adt_jump_enanbled( abap_false ).
    ENDIF.

    IF is_post_field_checked( 'link_hints_enabled' ) = abap_true.
      mo_settings->set_link_hints_enabled( abap_true ).
    ELSE.
      mo_settings->set_link_hints_enabled( abap_false ).
    ENDIF.

    READ TABLE mt_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'link_hint_key'.
    IF sy-subrc = 0.
      mo_settings->set_link_hint_key( |{ <ls_post_field>-value }| ).
    ENDIF.

    IF is_post_field_checked( 'parallel_proc_disabled' ) = abap_true.
      mo_settings->set_parallel_proc_disabled( abap_true ).
    ELSE.
      mo_settings->set_parallel_proc_disabled( abap_false ).
    ENDIF.

    READ TABLE mt_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'icon_scaling'.
    IF sy-subrc = 0.
      lv_c_param_value = <ls_post_field>-value.
      mo_settings->set_icon_scaling( lv_c_param_value ).
    ELSE.
      mo_settings->set_icon_scaling( '' ).
    ENDIF.

    READ TABLE mt_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'ui_theme'.
    IF sy-subrc = 0.
      mo_settings->set_ui_theme( <ls_post_field>-value ).
    ELSE.
      mo_settings->set_ui_theme( zcl_abapgit_settings=>c_ui_theme-default ).
    ENDIF.

    post_hotkeys( ).

  ENDMETHOD.


  METHOD post_commit_msg.

    DATA: lv_i_param_value TYPE i.

    FIELD-SYMBOLS: <ls_post_field> TYPE ihttpnvp.


    READ TABLE mt_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'comment_length'.
    IF sy-subrc = 0.
      lv_i_param_value = <ls_post_field>-value.
      IF lv_i_param_value < zcl_abapgit_settings=>c_commitmsg_comment_length_dft.
        lv_i_param_value = zcl_abapgit_settings=>c_commitmsg_comment_length_dft.
      ENDIF.
      mo_settings->set_commitmsg_comment_length( lv_i_param_value ).
    ELSE.
      mo_settings->set_commitmsg_comment_length( zcl_abapgit_settings=>c_commitmsg_comment_length_dft ).
    ENDIF.

    READ TABLE mt_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'body_size'.
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


  METHOD post_development_internals.

    IF is_post_field_checked( 'critical_tests' ) = abap_true.
      mo_settings->set_run_critical_tests( abap_true ).
    ELSE.
      mo_settings->set_run_critical_tests( abap_false ).
    ENDIF.

    IF is_post_field_checked( 'experimental_features' ) = abap_true.
      mo_settings->set_experimental_features( abap_true ).
    ELSE.
      mo_settings->set_experimental_features( abap_false ).
    ENDIF.

  ENDMETHOD.


  METHOD post_hotkeys.

    DATA: lv_column       TYPE string,
          lt_key_bindings TYPE zif_abapgit_definitions=>tty_hotkey.

    FIELD-SYMBOLS: <ls_post_field>  TYPE ihttpnvp,
                   <ls_key_binding> TYPE zif_abapgit_definitions=>ty_hotkey.


    LOOP AT mt_post_fields ASSIGNING <ls_post_field> WHERE name CP 'key*'.

      FIND FIRST OCCURRENCE OF REGEX `key_(.*)_`
           IN <ls_post_field>-name
           SUBMATCHES lv_column.

      CASE lv_column.
        WHEN 'sequence'.
          INSERT INITIAL LINE INTO TABLE lt_key_bindings ASSIGNING <ls_key_binding>.
          <ls_key_binding>-hotkey = <ls_post_field>-value.
        WHEN 'action'.
          <ls_key_binding>-action = <ls_post_field>-value.
      ENDCASE.
    ENDLOOP.

    DELETE lt_key_bindings WHERE hotkey IS INITIAL OR action IS INITIAL.

    mo_settings->set_hotkeys( lt_key_bindings ).

  ENDMETHOD.


  METHOD post_proxy.

    FIELD-SYMBOLS: <ls_post_field> TYPE ihttpnvp.


    READ TABLE mt_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'proxy_url'.
    IF sy-subrc <> 0.
      mv_error = abap_true.
    ENDIF.
    mo_settings->set_proxy_url( <ls_post_field>-value ).

    READ TABLE mt_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'proxy_port'.
    IF sy-subrc <> 0.
      mv_error = abap_true.
    ENDIF.
    mo_settings->set_proxy_port( <ls_post_field>-value ).

    IF is_post_field_checked( 'proxy_auth' ) = abap_true.
      mo_settings->set_proxy_authentication( abap_true ).
    ELSE.
      mo_settings->set_proxy_authentication( abap_false ).
    ENDIF.

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
    ro_html->add( render_icon_scaling( ) ).
    ro_html->add( render_ui_theme( ) ).
    ro_html->add( |<hr>| ).
    ro_html->add( render_adt_jump_enabled( ) ).
    ro_html->add( |<hr>| ).
    ro_html->add( render_parallel_proc( ) ).
    ro_html->add( |<hr>| ).
    ro_html->add( render_link_hints( ) ).
    ro_html->add( |<hr>| ).
    ro_html->add( render_hotkeys( ) ).
    ro_html->add( render_section_end( ) ).
    ro_html->add( render_form_end( ) ).

  ENDMETHOD.


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
    ro_html->add( '<input type="submit" value="Save" class="floating-button blue-set emphasis">' ).
    ro_html->add( '</form>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_hotkeys.

    DATA: lv_index    TYPE i,
          lt_hotkeys  TYPE zif_abapgit_definitions=>tty_hotkey,
          lv_selected TYPE string,
          lt_actions  TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_with_name.

    FIELD-SYMBOLS: <ls_key_binding> LIKE LINE OF lt_hotkeys,
                   <ls_action>      LIKE LINE OF lt_actions.

    lt_hotkeys = mo_settings->get_hotkeys( ).

    IF lines( lt_hotkeys ) = 0.
      lt_hotkeys = get_default_hotkeys( ).
    ENDIF.

    DO 3 TIMES.
      APPEND INITIAL LINE TO lt_hotkeys.
    ENDDO.

    CREATE OBJECT ro_html.
    ro_html->add( |<h2>Hotkeys</h2>| ).
    ro_html->add( |(Only available with installed abapGit repo)| ).
    ro_html->add( |<br/>| ).
    ro_html->add( |<br/>| ).

    ro_html->add( '<table class="repo_tab" id="key_bindings" style="max-width: 300px;">' ).
    ro_html->add( '<tr><th>key</th><th>action</th></tr>' ).

    lt_actions = get_possible_hotkey_actions( ).

    LOOP AT lt_hotkeys ASSIGNING <ls_key_binding>.

      lv_index = sy-tabix.

      ro_html->add( '<tr>' ).
      ro_html->add( |<td><input name="key_sequence_{ lv_index }" maxlength=1 type="text" | &&
                    |value="{ <ls_key_binding>-hotkey }"></td>| ).

      ro_html->add( |<td><select name="key_action_{ lv_index }">| ).

      LOOP AT lt_actions ASSIGNING <ls_action>.

        IF <ls_key_binding>-action = <ls_action>-action.
          lv_selected = 'selected'.
        ELSE.
          CLEAR: lv_selected.
        ENDIF.

        ro_html->add( |<option value="{ <ls_action>-action }" |
                   && |{ lv_selected }>|
                   && |{ <ls_action>-name }</option>| ).

      ENDLOOP.

      ro_html->add( '</select></td>' ).
      ro_html->add( '</tr>' ).

    ENDLOOP.


    ro_html->add( '</select></td>' ).
    ro_html->add( '</tr>' ).

    ro_html->add( '</table>' ).

    ro_html->add( |<br>| ).
    ro_html->add( |<br>| ).

  ENDMETHOD.


  METHOD render_icon_scaling.

    DATA:
      BEGIN OF ls_sel,
        auto  TYPE string,
        large TYPE string,
        small TYPE string,
      END OF ls_sel.

    CASE mo_settings->get_icon_scaling( ).
      WHEN zcl_abapgit_settings=>c_icon_scaling-large.
        ls_sel-large = ' selected'.
      WHEN zcl_abapgit_settings=>c_icon_scaling-small.
        ls_sel-small = ' selected'.
      WHEN OTHERS.
        ls_sel-auto = ' selected'.
    ENDCASE.

    CREATE OBJECT ro_html.

    ro_html->add( |<h2>UI Icon scaling</h2>| ).
    ro_html->add( |<label for="icon_scaling">High DPI icon scaling</label>| ).
    ro_html->add( |<br>| ).
    ro_html->add( |<select name="icon_scaling" size="3">| ).
    ro_html->add( |<option value=""{ ls_sel-auto }>Auto</option>| ).
    ro_html->add( |<option value="{ zcl_abapgit_settings=>c_icon_scaling-large }"{ ls_sel-large }>Large</option>| ).
    ro_html->add( |<option value="{ zcl_abapgit_settings=>c_icon_scaling-small }"{ ls_sel-small }>Small</option>| ).
    ro_html->add( |</select>| ).

    ro_html->add( |<br>| ).
    ro_html->add( |<br>| ).

  ENDMETHOD.


  METHOD render_link_hints.

    DATA: lv_checked               TYPE string,
          lv_link_hint_key         TYPE char01.

    IF mo_settings->get_link_hints_enabled( ) = abap_true.
      lv_checked = 'checked'.
    ENDIF.

    lv_link_hint_key = mo_settings->get_link_hint_key( ).

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


  METHOD render_parallel_proc.

    DATA lv_checked TYPE string.

    IF mo_settings->get_parallel_proc_disabled( ) = abap_true.
      lv_checked = 'checked'.
    ENDIF.

    CREATE OBJECT ro_html.
    ro_html->add( |<h2>Parallel processing</h2>| ).
    ro_html->add( `<input type="checkbox" name="parallel_proc_disabled" value="X" `
                   && lv_checked && ` > Disable parallel processing` ).
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


  METHOD render_ui_theme.

    " TODO: unify with render_icon_scaling, make list component

    DATA:
      BEGIN OF ls_sel,
        default TYPE string,
        dark TYPE string,
        belize TYPE string,
      END OF ls_sel.

    CASE mo_settings->get_ui_theme( ).
      WHEN zcl_abapgit_settings=>c_ui_theme-default.
        ls_sel-default = ' selected'.
      WHEN zcl_abapgit_settings=>c_ui_theme-dark.
        ls_sel-dark = ' selected'.
      WHEN zcl_abapgit_settings=>c_ui_theme-belize.
        ls_sel-belize = ' selected'.
    ENDCASE.

    CREATE OBJECT ro_html.

    ro_html->add( |<h2>UI Theme</h2>| ).
    ro_html->add( |<label for="ui_theme">UI Theme</label>| ).
    ro_html->add( |<br>| ).
    ro_html->add( |<select name="ui_theme" size="3">| ).
    ro_html->add( |<option value="{ zcl_abapgit_settings=>c_ui_theme-default }"{
      ls_sel-default }>{ zcl_abapgit_settings=>c_ui_theme-default }</option>| ).
    ro_html->add( |<option value="{ zcl_abapgit_settings=>c_ui_theme-dark }"{
      ls_sel-dark }>{ zcl_abapgit_settings=>c_ui_theme-dark }</option>| ).
    ro_html->add( |<option value="{ zcl_abapgit_settings=>c_ui_theme-belize }"{
      ls_sel-belize }>{ zcl_abapgit_settings=>c_ui_theme-belize }</option>| ).
    ro_html->add( |</select>| ).

    ro_html->add( |<br>| ).
    ro_html->add( |<br>| ).

  ENDMETHOD.


  METHOD validate_settings.

    IF ( mo_settings->get_proxy_url( ) IS NOT INITIAL AND mo_settings->get_proxy_port( ) IS INITIAL ) OR
                 ( mo_settings->get_proxy_url( ) IS INITIAL AND mo_settings->get_proxy_port( ) IS NOT INITIAL ).
      MESSAGE 'If specifying proxy, specify both URL and port' TYPE 'W'.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.
* todo, check input values eg INT

    DATA:
      lt_post_fields TYPE tihttpnvp.

    CASE iv_action.
      WHEN c_action-save_settings.
        lt_post_fields = parse_post( it_postdata ).

        post( lt_post_fields ).
        validate_settings( ).

        IF mv_error = abap_true.
          MESSAGE 'Error when saving settings. Open an issue at https://github.com/larshp/abapGit' TYPE 'E'.
        ELSE.
          persist_settings( ).
        ENDIF.

        ev_state = zcl_abapgit_gui=>c_event_state-go_back.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_hotkey~get_hotkey_actions.
    RETURN.
  ENDMETHOD.
ENDCLASS.
