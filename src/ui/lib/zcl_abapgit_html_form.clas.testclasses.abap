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
