CLASS ltcl_webgui_actions DEFINITION DEFERRED.
CLASS zcl_abapgit_html_form DEFINITION LOCAL FRIENDS ltcl_webgui_actions.

CLASS ltcl_webgui_actions DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mo_form TYPE REF TO zcl_abapgit_html_form.
    DATA mi_html TYPE REF TO zif_abapgit_html.

    METHODS setup.
    METHODS command_marker FOR TESTING.
    METHODS picker_marker FOR TESTING.

ENDCLASS.

CLASS ltcl_webgui_actions IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_form.
    mo_form->mv_webgui = abap_true.
    mo_form->mv_form_id = 'test_form'.
    mi_html = zcl_abapgit_html=>create( ).
  ENDMETHOD.

  METHOD command_marker.
    DATA ls_command TYPE zif_abapgit_html_form=>ty_command.
    DATA lv_html TYPE string.

    ls_command-label = 'Save'.
    ls_command-action = 'save?key=1&name=example'.
    ls_command-cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main.
    mo_form->render_command(
      ii_html = mi_html
      is_cmd  = ls_command ).
    lv_html = mi_html->render( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_html
      exp = '*data-sapevent="save?key=1&amp;name=example"*' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_html
      exp = |*submitSapeventForm(\{ \}, this.getAttribute('data-sapevent'), 'post', *| ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_html
      exp = '*class="dialog-commands main">Save</a>*' ).
  ENDMETHOD.

  METHOD picker_marker.
    DATA ls_field TYPE zif_abapgit_html_form=>ty_field.
    DATA ls_attr TYPE zcl_abapgit_html_form=>ty_attr.
    DATA lv_html TYPE string.

    ls_field-name = 'package'.
    ls_field-label = 'Package'.
    ls_field-side_action = 'choose_package'.
    mo_form->render_field_text(
      ii_html  = mi_html
      is_field = ls_field
      is_attr  = ls_attr ).
    lv_html = mi_html->render( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_html
      exp = '*type="button"*data-sapevent="choose_package"*' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_html
      exp = |*submitSapeventForm(\{ \}, this.getAttribute('data-sapevent'), 'post', *| ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_form_hotkeys DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS primary_action_alias FOR TESTING RAISING zcx_abapgit_exception.
    METHODS secondary_action_no_alias FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_form_hotkeys IMPLEMENTATION.
  METHOD primary_action_alias.
    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
    DATA lt_hotkeys TYPE zif_abapgit_gui_hotkeys=>ty_hotkeys_with_descr.
    DATA ls_hotkey LIKE LINE OF lt_hotkeys.

    lo_form = zcl_abapgit_html_form=>create( ).
    lo_form->command(
      iv_label = 'Save'
      iv_action = 'save'
      iv_cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main ).
    lt_hotkeys = lo_form->zif_abapgit_gui_hotkeys~get_hotkey_actions( ).

    READ TABLE lt_hotkeys WITH KEY hotkey = 'Enter' INTO ls_hotkey.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_hotkey-action
      exp = 'save' ).
    READ TABLE lt_hotkeys WITH KEY hotkey = '^Enter' INTO ls_hotkey.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_hotkey-action
      exp = 'save' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_hotkey-description
      exp = 'Save' ).
  ENDMETHOD.

  METHOD secondary_action_no_alias.
    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
    DATA lt_hotkeys TYPE zif_abapgit_gui_hotkeys=>ty_hotkeys_with_descr.

    lo_form = zcl_abapgit_html_form=>create( ).
    lo_form->command(
      iv_label = 'Choose'
      iv_action = 'choose' ).
    lt_hotkeys = lo_form->zif_abapgit_gui_hotkeys~get_hotkey_actions( ).
    cl_abap_unit_assert=>assert_initial( lt_hotkeys ).
  ENDMETHOD.
ENDCLASS.
