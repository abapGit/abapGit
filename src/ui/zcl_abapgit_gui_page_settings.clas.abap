CLASS zcl_abapgit_gui_page_settings DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        save_settings       TYPE string VALUE 'save_settings',
        change_proxy_bypass TYPE string VALUE 'change_proxy_bypass',
      END OF c_action.

    METHODS constructor
      RAISING zcx_abapgit_exception.
    METHODS zif_abapgit_gui_event_handler~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.

    DATA mo_settings TYPE REF TO zcl_abapgit_settings .
    DATA mv_error TYPE abap_bool .
    DATA mt_post_fields TYPE tihttpnvp .
    DATA mt_proxy_bypass TYPE zif_abapgit_definitions=>ty_range_proxy_bypass_url.
    DATA mt_default_hotkeys TYPE zif_abapgit_gui_hotkeys=>ty_hotkeys_with_descr.

    METHODS post_commit_msg .
    METHODS post_development_internals .
    METHODS post_hotkeys .
    METHODS render_proxy
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_development_internals
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_form_begin
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_form_end
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_max_lines
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_icon_scaling
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_ui_theme
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_adt_jump_enabled
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_commit_msg
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS post_proxy .
    METHODS post
      IMPORTING
        !it_post_fields TYPE tihttpnvp .
    METHODS validate_settings .
    METHODS persist_settings
      RAISING
        zcx_abapgit_exception .
    METHODS read_settings .
    METHODS render_section_begin
      IMPORTING
        !iv_header     TYPE csequence
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_section_end
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_start_up
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_link_hints
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_hotkeys
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS is_post_field_checked
      IMPORTING
        iv_name          TYPE string
      RETURNING
        VALUE(rv_return) TYPE abap_bool .

    METHODS render_parallel_proc
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_SETTINGS IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'Settings'.
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

    READ TABLE mt_post_fields ASSIGNING <ls_post_field> WITH KEY name = 'comment_default'.
    IF sy-subrc = 0.
      mo_settings->set_commitmsg_comment_default( <ls_post_field>-value ).
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

    mo_settings->set_run_critical_tests( is_post_field_checked( 'critical_tests' ) ).

    mo_settings->set_experimental_features( is_post_field_checked( 'experimental_features' ) ).

    mo_settings->set_activate_wo_popup( is_post_field_checked( 'activate_wo_popup' ) ).

  ENDMETHOD.


  METHOD post_hotkeys.

    DATA:
      lt_key_bindings TYPE zif_abapgit_definitions=>ty_hotkey_tt,
      ls_key_binding  LIKE LINE OF lt_key_bindings.

    FIELD-SYMBOLS:
      <ls_default_hotkey> LIKE LINE OF mt_default_hotkeys,
      <ls_post_field>     TYPE ihttpnvp.

    LOOP AT mt_post_fields ASSIGNING <ls_post_field> WHERE name CP 'hk~*'.

      FIND FIRST OCCURRENCE OF REGEX `hk~(.+)~(.+)`
        IN <ls_post_field>-name
        SUBMATCHES ls_key_binding-ui_component ls_key_binding-action.
      CHECK sy-subrc = 0.

      READ TABLE mt_default_hotkeys
        ASSIGNING <ls_default_hotkey>
        WITH TABLE KEY action
        COMPONENTS
          ui_component = ls_key_binding-ui_component
          action       = ls_key_binding-action.
      IF sy-subrc = 0 AND <ls_post_field>-value IS NOT INITIAL AND <ls_post_field>-value <> <ls_default_hotkey>-hotkey.
        ls_key_binding-hotkey = <ls_post_field>-value.
        APPEND ls_key_binding TO lt_key_bindings.
      ENDIF.

    ENDLOOP.

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

    mo_settings->set_proxy_bypass( mt_proxy_bypass ).

  ENDMETHOD.


  METHOD read_settings.

    DATA lo_settings_persistence TYPE REF TO zcl_abapgit_persist_settings.

    lo_settings_persistence = zcl_abapgit_persist_settings=>get_instance( ).
    mo_settings = lo_settings_persistence->read( ).

    mt_proxy_bypass = mo_settings->get_proxy_bypass( ).

  ENDMETHOD.


  METHOD render_adt_jump_enabled.

    DATA lv_checked TYPE string.

    IF mo_settings->get_adt_jump_enabled( ) = abap_true.
      lv_checked = 'checked'.
    ENDIF.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( |<h2>ABAP Development Tools (ADT)</h2>| ).
    ri_html->add( `<input type="checkbox" name="adt_jump_enabled" value="X" `
                   && lv_checked && ` > Enable Jump to ADT First` ).
    ri_html->add( |<br>| ).
    ri_html->add( |<br>| ).
  ENDMETHOD.


  METHOD render_commit_msg.
    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( |<h2>Commit Message</h2>| ).
    ri_html->add( |<label for="comment_length" title="(Recommendation 50)">Max. Length of Comment</label>| ).
    ri_html->add( |<br>| ).
    ri_html->add( |<input name="comment_length" type="number" step="10" size="3" maxlength="3" min="50"| &&
                  | value="{ mo_settings->get_commitmsg_comment_length( ) }">| ).
    ri_html->add( |<br>| ).
    ri_html->add( |<label for="comment_default"| &&
                  |title="(Possible Variables: $OBJECT, $FILE)">Default for Comment</label>| ).
    ri_html->add( |<br>| ).
    ri_html->add( |<input name="comment_default" type="text" size="80" maxlength="255"| &&
                  | value="{ mo_settings->get_commitmsg_comment_default( ) }">| ).
    ri_html->add( |<br>| ).
    ri_html->add( |<label for="body_size" title="(Recommendation 72)">Max. Line Size of Body</label>| ).
    ri_html->add( |<br>| ).
    ri_html->add( |<input name="body_size" type="number" size="3" maxlength="3" min="50"| &&
                  | value="{ mo_settings->get_commitmsg_body_size( ) }">| ).
    ri_html->add( |<br>| ).
    ri_html->add( |<br>| ).
  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    read_settings( ).

    ri_html->add( render_form_begin( ) ).
    ri_html->add( render_section_begin( |Global Settings| ) ).
    ri_html->add( render_proxy( ) ).
    ri_html->add( |<hr>| ).
    ri_html->add( render_commit_msg( ) ).
    ri_html->add( |<hr>| ).
    ri_html->add( render_development_internals( ) ).
    ri_html->add( render_section_end( ) ).
    ri_html->add( render_section_begin( |User Specific Settings| ) ).
    ri_html->add( render_start_up( ) ).
    ri_html->add( render_max_lines( ) ).
    ri_html->add( render_icon_scaling( ) ).
    ri_html->add( render_ui_theme( ) ).
    ri_html->add( |<hr>| ).
    ri_html->add( render_adt_jump_enabled( ) ).
    ri_html->add( |<hr>| ).
    ri_html->add( render_parallel_proc( ) ).
    ri_html->add( |<hr>| ).
    ri_html->add( render_link_hints( ) ).
    ri_html->add( |<hr>| ).
    ri_html->add( render_hotkeys( ) ).
    ri_html->add( render_section_end( ) ).
    ri_html->add( render_form_end( ) ).

  ENDMETHOD.


  METHOD render_development_internals.

    DATA: lv_critical_tests TYPE string,
          lv_experimental   TYPE string,
          lv_act_wo_popup   TYPE string.

    IF mo_settings->get_run_critical_tests( ) = abap_true.
      lv_critical_tests = 'checked'.
    ENDIF.

    IF mo_settings->get_experimental_features( ) = abap_true.
      lv_experimental = 'checked'.
    ENDIF.

    IF mo_settings->get_activate_wo_popup( ) = abap_true.
      lv_act_wo_popup = 'checked'.
    ENDIF.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( |<h2>abapGit Development Internals</h2>| ).
    ri_html->add( `<input type="checkbox" name="critical_tests" `
                   && lv_critical_tests && ` > Enable Critical Unit Tests (See LTCL_DANGEROUS)` ).
    ri_html->add( |<br>| ).
    ri_html->add( `<input type="checkbox" name="experimental_features" `
                   && lv_experimental && ` > Enable Experimental Features` ).
    ri_html->add( |<br>| ).
    ri_html->add( `<input type="checkbox" name="activate_wo_popup" `
                   && lv_act_wo_popup && ` > Activate Objects Without Popup` ).
    ri_html->add( |<br>| ).
    ri_html->add( |<br>| ).

  ENDMETHOD.


  METHOD render_form_begin.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( '<div class="settings_container">' ).
    ri_html->add( `<form id="settings_form" method="post" action="sapevent:` && c_action-save_settings && `">` ).

  ENDMETHOD.


  METHOD render_form_end.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( '<input type="submit" value="Save" class="floating-button blue-set emphasis">' ).
    ri_html->add( '</form>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_hotkeys.

    DATA lv_hk_id TYPE string.
    DATA lt_hotkeys LIKE mt_default_hotkeys.
    FIELD-SYMBOLS <ls_key> LIKE LINE OF mt_default_hotkeys.

    mt_default_hotkeys = zcl_abapgit_hotkeys=>get_all_default_hotkeys( ). " Cache for save processing
    lt_hotkeys = mt_default_hotkeys.
    zcl_abapgit_hotkeys=>merge_hotkeys_with_settings( CHANGING ct_hotkey_actions = lt_hotkeys ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( |<h2>Hotkeys</h2>| ).

    ri_html->add( '<table class="settings_tab">' ).
    ri_html->add( '<thead><tr><th>Component</th><th>Action</th><th>Key</th></tr></thead>' ).

    LOOP AT lt_hotkeys ASSIGNING <ls_key>.

      ri_html->add( '<tr>' ).
      ri_html->add( |<td>{ <ls_key>-ui_component }</td>| ).
      ri_html->add( |<td>{ <ls_key>-description }</td>| ).
      lv_hk_id = |hk~{ <ls_key>-ui_component }~{ <ls_key>-action }|.
      ri_html->add( |<td><input name="{ lv_hk_id }" maxlength=1 type="text" value="{ <ls_key>-hotkey }"></td>| ).
      ri_html->add( '</tr>' ).

    ENDLOOP.

    ri_html->add( '</table>' ).

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

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( |<h2>UI Icon Scaling</h2>| ).
    ri_html->add( |<label for="icon_scaling">High DPI Icon Scaling</label>| ).
    ri_html->add( |<br>| ).
    ri_html->add( |<select name="icon_scaling" size="3">| ).
    ri_html->add( |<option value=""{ ls_sel-auto }>Auto</option>| ).
    ri_html->add( |<option value="{ zcl_abapgit_settings=>c_icon_scaling-large }"{ ls_sel-large }>Large</option>| ).
    ri_html->add( |<option value="{ zcl_abapgit_settings=>c_icon_scaling-small }"{ ls_sel-small }>Small</option>| ).
    ri_html->add( |</select>| ).

    ri_html->add( |<br>| ).
    ri_html->add( |<br>| ).

  ENDMETHOD.


  METHOD render_link_hints.

    DATA: lv_checked       TYPE string,
          lv_link_hint_key TYPE c LENGTH 1.

    IF mo_settings->get_link_hints_enabled( ) = abap_true.
      lv_checked = 'checked'.
    ENDIF.

    lv_link_hint_key = mo_settings->get_link_hint_key( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( |<h2>Vimium-like Link Hints</h2>| ).
    ri_html->add( `<input type="checkbox" name="link_hints_enabled" value="X" `
                   && lv_checked && ` > Enable Vimium-like Link Hints` ).
    ri_html->add( |<br>| ).
    ri_html->add( |<br>| ).
    ri_html->add( |<input type="text" name="link_hint_key" size="1" maxlength="1" value="{ lv_link_hint_key }" |
               && |> Key to Activate Links| ).

    ri_html->add( |<br>| ).
    ri_html->add( |<br>| ).

  ENDMETHOD.


  METHOD render_max_lines.
    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( |<h2>List size</h2>| ).
    ri_html->add( |<label for="max_lines">Max. # of Objects Listed (0 = All)</label>| ).
    ri_html->add( |<br>| ).
    ri_html->add( `<input name="max_lines" type="text" size="5" value="` && mo_settings->get_max_lines( ) && `">` ).
    ri_html->add( |<br>| ).
    ri_html->add( |<br>| ).
  ENDMETHOD.


  METHOD render_parallel_proc.

    DATA lv_checked TYPE string.

    IF mo_settings->get_parallel_proc_disabled( ) = abap_true.
      lv_checked = 'checked'.
    ENDIF.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( |<h2>Parallel Processing</h2>| ).
    ri_html->add( `<input type="checkbox" name="parallel_proc_disabled" value="X" `
                   && lv_checked && ` > Disable Parallel Processing` ).
    ri_html->add( |<br>| ).
    ri_html->add( |<br>| ).
  ENDMETHOD.


  METHOD render_proxy.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( |<h2>Proxy</h2>| ).
    ri_html->add( |<label for="proxy_url">Proxy URL</label>| ).
    ri_html->add( |<br>| ).
    ri_html->add( `<input name="proxy_url" type="text" size="50" value="` &&
      mo_settings->get_proxy_url( ) && `">` ).
    ri_html->add( |<br>| ).
    ri_html->add( |<label for="proxy_port">Proxy Port</label>| ).
    ri_html->add( |<br>| ).
    ri_html->add( `<input name="proxy_port" type="text" size="5" value="` &&
      mo_settings->get_proxy_port( ) && `">` ).
    ri_html->add( |<br>| ).
    ri_html->add( |<label for="proxy_auth">Proxy Authentication</label>| ).
    IF mo_settings->get_proxy_authentication( ) = abap_true.
      ri_html->add( `<input name="proxy_auth" type="checkbox" checked>` ).
    ELSE.
      ri_html->add( `<input name="proxy_auth" type="checkbox">` ).
    ENDIF.
    ri_html->add( |<br>| ).
    ri_html->add( |<br>| ).
    ri_html->add( |<label for="proxy_bypass">Bypass Proxy Settings for These Hosts & Domains</label>| ).
    ri_html->add( |<br>| ).
    ri_html->add( |<button type="button" name="proxy_bypass" class="grey-set"|
                & |onclick="location.href='sapevent:{ c_action-change_proxy_bypass }';">Maintain</button>| ).
    ri_html->add( |<br>| ).

    ri_html->add( |<br>| ).

  ENDMETHOD.


  METHOD render_section_begin.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( |<h1>{ iv_header }</h1>| ).
    ri_html->add( |<div class="settings_section">| ).

  ENDMETHOD.


  METHOD render_section_end.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_start_up.

    DATA lv_checked TYPE string.

    IF mo_settings->get_show_default_repo( ) = abap_true.
      lv_checked = 'checked'.
    ENDIF.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( |<h2>Startup</h2>| ).
    ri_html->add( `<input type="checkbox" name="show_default_repo" value="X" `
                   && lv_checked && ` > Show Last Opened Repository` ).
    ri_html->add( |<br>| ).
    ri_html->add( |<br>| ).
  ENDMETHOD.


  METHOD render_ui_theme.

    " TODO: unify with render_icon_scaling, make list component

    DATA:
      BEGIN OF ls_sel,
        default         TYPE string,
        dark            TYPE string,
        belize          TYPE string,
        synced_with_gui TYPE string,
      END OF ls_sel.

    CASE mo_settings->get_ui_theme( abap_false ).
      WHEN zcl_abapgit_settings=>c_ui_theme-default.
        ls_sel-default = ' selected'.
      WHEN zcl_abapgit_settings=>c_ui_theme-dark.
        ls_sel-dark = ' selected'.
      WHEN zcl_abapgit_settings=>c_ui_theme-belize.
        ls_sel-belize = ' selected'.
      WHEN zcl_abapgit_settings=>c_ui_theme-synced_with_gui.
        ls_sel-synced_with_gui = ' selected'.
    ENDCASE.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( |<h2>UI Theme</h2>| ).
    ri_html->add( |<label for="ui_theme">UI Theme</label>| ).
    ri_html->add( |<br>| ).
    ri_html->add( |<select name="ui_theme" size="4">| ).
    ri_html->add( |<option value="{ zcl_abapgit_settings=>c_ui_theme-default }"{
      ls_sel-default }>{ zcl_abapgit_settings=>c_ui_theme-default }</option>| ).
    ri_html->add( |<option value="{ zcl_abapgit_settings=>c_ui_theme-dark }"{
      ls_sel-dark }>{ zcl_abapgit_settings=>c_ui_theme-dark }</option>| ).
    ri_html->add( |<option value="{ zcl_abapgit_settings=>c_ui_theme-belize }"{
      ls_sel-belize }>{ zcl_abapgit_settings=>c_ui_theme-belize }</option>| ).
    ri_html->add( |<option value="{ zcl_abapgit_settings=>c_ui_theme-synced_with_gui }"{
      ls_sel-synced_with_gui }>Synced with SAP GUI</option>| ).
    ri_html->add( |</select>| ).

    ri_html->add( |<br>| ).
    ri_html->add( |<br>| ).

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

    CASE ii_event->mv_action.
      WHEN c_action-save_settings.
        lt_post_fields = zcl_abapgit_html_action_utils=>parse_post_form_data( ii_event->mt_postdata ).

        post( lt_post_fields ).
        validate_settings( ).

        IF mv_error = abap_true.
          MESSAGE 'Error when saving settings. Open an issue at https://github.com/abapGit/abapGit' TYPE 'E'.
        ELSE.
          persist_settings( ).
        ENDIF.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
      WHEN c_action-change_proxy_bypass.
        mt_proxy_bypass = zcl_abapgit_ui_factory=>get_popups( )->popup_proxy_bypass( mt_proxy_bypass ).

        rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
