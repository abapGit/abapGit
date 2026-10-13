CLASS ltcl_settings_back DEFINITION FINAL FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS settings_form FOR TESTING.
    METHODS repository_form FOR TESTING.
    METHODS statistics_back FOR TESTING.
    METHODS no_back FOR TESTING.
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
