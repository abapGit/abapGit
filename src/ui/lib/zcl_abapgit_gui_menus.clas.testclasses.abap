CLASS ltcl_settings_back DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS settings_form FOR TESTING.
    METHODS repository_form FOR TESTING.
    METHODS statistics_back FOR TESTING.
    METHODS no_back FOR TESTING.
ENDCLASS.

CLASS lcl_toolbar_exit DEFINITION INHERITING FROM zcl_abapgit_exit FINAL.
  PUBLIC SECTION.
    DATA mv_call_count TYPE i.
    DATA mv_repo_key TYPE zif_abapgit_persistence=>ty_repo-key.
    DATA mv_action TYPE string.
    METHODS zif_abapgit_exit~enhance_repo_toolbar REDEFINITION.
ENDCLASS.

CLASS lcl_toolbar_exit IMPLEMENTATION.
  METHOD zif_abapgit_exit~enhance_repo_toolbar.
    mv_call_count = mv_call_count + 1.
    mv_repo_key = iv_key.
    mv_action = iv_act.
    io_menu->add(
      iv_txt = 'Custom Settings'
      iv_act = |custom_settings?key={ iv_key }| ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_repo_settings_exit DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA mo_exit TYPE REF TO lcl_toolbar_exit.
    DATA mi_previous_exit TYPE REF TO zif_abapgit_exit.
    METHODS setup.
    METHODS teardown.
    METHODS settings_page FOR TESTING.
    METHODS repo_dropdown FOR TESTING.
    METHODS overview_dropdown FOR TESTING.
ENDCLASS.

CLASS ltcl_repo_settings_exit IMPLEMENTATION.
  METHOD setup.
    mi_previous_exit = zcl_abapgit_exit=>get_instance( ).
    CREATE OBJECT mo_exit.
    zcl_abapgit_injector=>set_exit( mo_exit ).
  ENDMETHOD.

  METHOD teardown.
    zcl_abapgit_injector=>set_exit( mi_previous_exit ).
  ENDMETHOD.

  METHOD settings_page.
    DATA lv_html TYPE string.
    lv_html = zcl_abapgit_gui_menus=>repo_settings(
      iv_key = '123'
      iv_act = zif_abapgit_definitions=>c_action-repo_settings )->render( )->render( ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_exit->mv_call_count
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_exit->mv_repo_key
      exp = '123' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_exit->mv_action
      exp = zif_abapgit_definitions=>c_action-repo_settings ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_html
      exp = '*custom_settings?key=123*' ).
  ENDMETHOD.

  METHOD repo_dropdown.
    DATA lv_html TYPE string.
    lv_html = zcl_abapgit_gui_menus=>repo_settings( '123' )->render( )->render( ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_exit->mv_call_count
      exp = 0 ).
    cl_abap_unit_assert=>assert_char_np(
      act = lv_html
      exp = '*custom_settings*' ).
  ENDMETHOD.

  METHOD overview_dropdown.
    DATA lv_html TYPE string.
    lv_html = zcl_abapgit_gui_menus=>repo_settings(
      iv_key   = '#'
      iv_class = 'action_link' )->render( )->render( ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_exit->mv_call_count
      exp = 0 ).
    cl_abap_unit_assert=>assert_char_np(
      act = lv_html
      exp = '*custom_settings*' ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_settings_back IMPLEMENTATION.
  METHOD settings_form.
    DATA lv_html TYPE string.
    lv_html = zcl_abapgit_gui_menus=>settings(
      iv_with_back = abap_true
      iv_form_id   = 'global-setting-form' )->render( )->render( ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_html
      exp = `*onclick="submitSapeventForm({ }, 'go_back', 'post', document.getElementById('global-setting-form'))"*` ).
    cl_abap_unit_assert=>assert_char_np(
      act = lv_html
      exp = '*sapevent:go_back*' ).
  ENDMETHOD.

  METHOD repository_form.
    DATA lv_html TYPE string.
    lv_html = zcl_abapgit_gui_menus=>repo_settings(
      iv_key       = '1'
      iv_with_back = abap_true
      iv_form_id   = 'repo-settings-form' )->render( )->render( ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_html
      exp = `*onclick="submitSapeventForm({ }, 'go_back', 'post', document.getElementById('repo-settings-form'))"*` ).
    cl_abap_unit_assert=>assert_char_np(
      act = lv_html
      exp = '*sapevent:go_back*' ).
  ENDMETHOD.

  METHOD statistics_back.
    DATA lv_html TYPE string.
    lv_html = zcl_abapgit_gui_menus=>repo_settings(
      iv_key       = '1'
      iv_with_back = abap_true )->render( )->render( ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_html
      exp = '*href="sapevent:go_back"*' ).
    cl_abap_unit_assert=>assert_char_np(
      act = lv_html
      exp = '*submitSapeventForm*' ).
  ENDMETHOD.

  METHOD no_back.
    DATA lv_html TYPE string.
    lv_html = zcl_abapgit_gui_menus=>settings( )->render( )->render( ).
    cl_abap_unit_assert=>assert_char_np(
      act = lv_html
      exp = '*go_back*' ).
    lv_html = zcl_abapgit_gui_menus=>repo_settings( '1' )->render( )->render( ).
    cl_abap_unit_assert=>assert_char_np(
      act = lv_html
      exp = '*go_back*' ).
  ENDMETHOD.
ENDCLASS.
