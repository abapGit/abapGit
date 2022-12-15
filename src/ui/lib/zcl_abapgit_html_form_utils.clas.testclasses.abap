CLASS ltcl_popups_mock DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    TYPES:
      ty_user_decision TYPE string.

    CONSTANTS:
      BEGIN OF c_user_decision,
        cancel  TYPE ty_user_decision VALUE 'cancel',
        confirm TYPE ty_user_decision VALUE 'confirm',
      END OF c_user_decision.

    INTERFACES:
      zif_abapgit_popups.

    METHODS:
      was_confirm_popup_shown
        RETURNING
          VALUE(rv_popup_shown) TYPE abap_bool,

      set_user_decision
        IMPORTING
          iv_user_decision TYPE ty_user_decision.

  PRIVATE SECTION.
    DATA:
      BEGIN OF ms_called,
        popup_to_confirm TYPE abap_bool,
      END OF ms_called,
      mv_user_decision TYPE ty_user_decision.

ENDCLASS.

CLASS ltcl_popups_mock IMPLEMENTATION.

  METHOD zif_abapgit_popups~branch_list_popup.
  ENDMETHOD.

  METHOD zif_abapgit_popups~choose_pr_popup.
  ENDMETHOD.

  METHOD zif_abapgit_popups~commit_list_popup.
  ENDMETHOD.

  METHOD zif_abapgit_popups~create_branch_popup.
  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_folder_logic.
  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_search_help.
  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_confirm.

    ms_called-popup_to_confirm = abap_true.

    CASE mv_user_decision.
      WHEN c_user_decision-cancel.
        rv_answer = 'A'.
      WHEN c_user_decision-confirm.
        rv_answer = '1'.
      WHEN OTHERS.
        cl_abap_unit_assert=>fail( ).
    ENDCASE.

  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_create_package.
  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_create_transp_branch.
  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_select_from_list.
  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_select_transports.
  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_transport_request.
  ENDMETHOD.

  METHOD was_confirm_popup_shown.
    rv_popup_shown = ms_called-popup_to_confirm.
  ENDMETHOD.

  METHOD set_user_decision.
    mv_user_decision = iv_user_decision.
  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_select_tr_requests.
  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_select_wb_tc_tr_and_tsk.
  ENDMETHOD.

  METHOD zif_abapgit_popups~tag_list_popup.
  ENDMETHOD.

  METHOD zif_abapgit_popups~popup_to_select_labels.
  ENDMETHOD.

  METHOD zif_abapgit_popups~choose_code_insp_check_variant.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_test_form DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.
    DATA:
      mo_popups_mock TYPE REF TO ltcl_popups_mock.

    METHODS setup.
    METHODS validate1 FOR TESTING RAISING zcx_abapgit_exception.
    METHODS validate2 FOR TESTING RAISING zcx_abapgit_exception.
    METHODS validate3 FOR TESTING RAISING zcx_abapgit_exception.
    METHODS normalize FOR TESTING RAISING zcx_abapgit_exception.
    METHODS is_empty FOR TESTING RAISING zcx_abapgit_exception.
    METHODS exit_clean FOR TESTING RAISING zcx_abapgit_exception.
    METHODS exit_dirty_confirm FOR TESTING RAISING zcx_abapgit_exception.
    METHODS exit_dirty_cancel FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_test_form IMPLEMENTATION.

  METHOD setup.

    CREATE OBJECT mo_popups_mock TYPE ltcl_popups_mock.
    zcl_abapgit_ui_injector=>set_popups( mo_popups_mock ).

  ENDMETHOD.

  METHOD validate1.

    DATA lo_cut TYPE REF TO zcl_abapgit_html_form_utils.
    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
    DATA lo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA lo_log TYPE REF TO zcl_abapgit_string_map.

    lo_form      = zcl_abapgit_html_form=>create( ).
    lo_form_data = zcl_abapgit_string_map=>create( ).

    lo_form->text(
      iv_name     = 'field1'
      iv_required = abap_true
      iv_label    = 'Field name 1'
    )->text(
      iv_name  = 'field2'
      iv_label = 'Field name 2' ).

    lo_cut = zcl_abapgit_html_form_utils=>create( lo_form ).

    lo_log = lo_cut->validate( lo_form_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lo_log->get( 'field1' )
      exp = '*cannot be empty' ).

    lo_form_data->set(
      iv_key = 'field1'
      iv_val = '' ).

    lo_log = lo_cut->validate( lo_form_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lo_log->get( 'field1' )
      exp = '*cannot be empty' ).

    lo_form_data->set(
      iv_key = 'field1'
      iv_val = 'xyz' ).

    lo_log = lo_cut->validate( lo_form_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 0 ).

  ENDMETHOD.

  METHOD validate2.

    DATA lo_cut TYPE REF TO zcl_abapgit_html_form_utils.
    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
    DATA lo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA lo_log TYPE REF TO zcl_abapgit_string_map.

    " New form
    lo_form = zcl_abapgit_html_form=>create( ).
    lo_form_data = zcl_abapgit_string_map=>create( ).

    lo_form->text(
      iv_name  = 'field3'
      iv_min   = 3
      iv_max   = 10
      iv_label = 'Field name 3' ).

    lo_cut = zcl_abapgit_html_form_utils=>create( lo_form ).

    lo_form_data->set(
      iv_key = 'field3'
      iv_val = 'xy' ).

    lo_log = lo_cut->validate( lo_form_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lo_log->get( 'field3' )
      exp = '*must not be shorter*' ).

    lo_form_data->set(
      iv_key = 'field3'
      iv_val = '01234567890123' ).

    lo_log = lo_cut->validate( lo_form_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lo_log->get( 'field3' )
      exp = '*must not be longer*' ).

    lo_form_data->set(
      iv_key = 'field3'
      iv_val = 'xyz!' ).

    lo_log = lo_cut->validate( lo_form_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 0 ).

  ENDMETHOD.

  METHOD validate3.

    DATA lo_cut TYPE REF TO zcl_abapgit_html_form_utils.
    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
    DATA lo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA lo_log TYPE REF TO zcl_abapgit_string_map.

    " New form
    lo_form = zcl_abapgit_html_form=>create( ).
    lo_form_data = zcl_abapgit_string_map=>create( ).

    lo_form->number(
      iv_name  = 'field4'
      iv_min   = 100
      iv_max   = 200
      iv_label = 'Field name 4' ).

    lo_cut = zcl_abapgit_html_form_utils=>create( lo_form ).

    lo_form_data->set(
      iv_key = 'field4'
      iv_val = '123-456' ).

    lo_log = lo_cut->validate( lo_form_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lo_log->get( 'field4' )
      exp = '*is not numeric*' ).

    lo_form_data->set(
      iv_key = 'field4'
      iv_val = '50' ).

    lo_log = lo_cut->validate( lo_form_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lo_log->get( 'field4' )
      exp = '*must not be lower*' ).

    lo_form_data->set(
      iv_key = 'field4'
      iv_val = '250' ).

    lo_log = lo_cut->validate( lo_form_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lo_log->get( 'field4' )
      exp = '*must not be higher*' ).

    lo_form_data->set(
      iv_key = 'field4'
      iv_val = '150' ).

    lo_log = lo_cut->validate( lo_form_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 0 ).

  ENDMETHOD.

  METHOD normalize.

    DATA lo_cut TYPE REF TO zcl_abapgit_html_form_utils.
    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
    DATA lo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA lo_normalized_act TYPE REF TO zcl_abapgit_string_map.
    DATA lo_normalized_exp TYPE REF TO zcl_abapgit_string_map.

    lo_form           = zcl_abapgit_html_form=>create( ).
    lo_form_data      = zcl_abapgit_string_map=>create( iv_case_insensitive = abap_true ).
    lo_normalized_exp = zcl_abapgit_string_map=>create( ).

    lo_form->text(
      iv_name  = 'field1'
      iv_label = 'Field name 1'
    )->text(
      iv_name       = 'field2'
      iv_upper_case = abap_true
      iv_label      = 'Field name 2'
    )->text(
      iv_name  = 'field3'
      iv_label = 'Field name 3'
    )->checkbox(
      iv_name  = 'chk1'
      iv_label = 'Checkbox1'
    )->checkbox(
      iv_name  = 'chk2'
      iv_label = 'Checkbox2'
    )->number(
      iv_name  = 'num1'
      iv_label = 'Number 1'
    )->table(
      iv_name  = 'tab1'
      iv_label = 'Table 1'
    )->column( 'Column 1'
    )->column( 'Column 2'
    )->number(
      iv_name  = |tab1-{ zif_abapgit_html_form=>c_rows }|
      iv_label = 'Number of Rows' ). " simulate hidden form field

    lo_cut = zcl_abapgit_html_form_utils=>create( lo_form ).

    lo_form_data->set(
      iv_key = 'field1'
      iv_val = 'val1' ).
    lo_form_data->set(
      iv_key = 'field2'
      iv_val = 'val2' ).
    " Intentionally field3 is not specificed
    lo_form_data->set(
      iv_key = 'chk1'
      iv_val = '' ).
    lo_form_data->set(
      iv_key = 'chk2'
      iv_val = 'on' ).
    lo_form_data->set(
      iv_key = 'chk3'
      iv_val = 'on' ). " Extra field - filtered by normalizing

    lo_form_data->set(
      iv_key = 'num1'
      iv_val = '   1234' ).

    " Table with 2 rows, 2 columns
    lo_form_data->set(
      iv_key = |tab1-{ zif_abapgit_html_form=>c_rows }|
      iv_val = '2' ).
    lo_form_data->set(
      iv_key = |tab1-1-1|
      iv_val = 'abc' ).
    lo_form_data->set(
      iv_key = |tab1-1-2|
      iv_val = '123' ).
    lo_form_data->set(
      iv_key = |tab1-2-1|
      iv_val = '' ).
    lo_form_data->set(
      iv_key = |tab1-2-2|
      iv_val = '0' ).

    lo_normalized_exp->set(
      iv_key = 'field1'
      iv_val = 'val1' ).
    lo_normalized_exp->set(
      iv_key = 'field2'
      iv_val = 'VAL2' ).
    lo_normalized_exp->set(
      iv_key = 'field3'
      iv_val = '' ). " But it is present in normalized
    lo_normalized_exp->set(
      iv_key = 'chk1'
      iv_val = ` ` ). " hmmm
    lo_normalized_exp->set(
      iv_key = 'chk2'
      iv_val = 'X' ).
    lo_normalized_exp->set(
      iv_key = 'chk2'
      iv_val = 'X' ).

    lo_normalized_exp->set(
      iv_key = 'num1'
      iv_val = '1234' ).

    lo_normalized_exp->set(
      iv_key = |tab1-{ zif_abapgit_html_form=>c_rows }|
      iv_val = '2' ).
    lo_normalized_exp->set(
      iv_key = |tab1-1-1|
      iv_val = 'abc' ).
    lo_normalized_exp->set(
      iv_key = |tab1-1-2|
      iv_val = '123' ).
    lo_normalized_exp->set(
      iv_key = |tab1-2-1|
      iv_val = '' ).
    lo_normalized_exp->set(
      iv_key = |tab1-2-2|
      iv_val = '0' ).

    lo_normalized_act = lo_cut->normalize( lo_form_data ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_normalized_act->mt_entries
      exp = lo_normalized_exp->mt_entries ).

  ENDMETHOD.

  METHOD is_empty.

    DATA lo_cut TYPE REF TO zcl_abapgit_html_form_utils.
    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
    DATA lo_form_data TYPE REF TO zcl_abapgit_string_map.

    lo_form      = zcl_abapgit_html_form=>create( ).
    lo_form_data = zcl_abapgit_string_map=>create( ).

    lo_form->text(
      iv_name  = 'field1'
      iv_label = 'Field name 1'
    )->checkbox(
      iv_name  = 'chk1'
      iv_label = 'Checkbox1'
    )->number(
      iv_name  = 'num1'
      iv_label = 'Number 1'
    )->table(
      iv_name  = 'tab1'
      iv_label = 'Table 1'
    )->column( 'Column 1'
    )->column( 'Column 2'
    )->number(
      iv_name  = |tab1-{ zif_abapgit_html_form=>c_rows }|
      iv_label = 'Number of Rows' ). " simulate hidden form field

    lo_cut = zcl_abapgit_html_form_utils=>create( lo_form ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_empty( lo_form_data )
      exp = abap_true ).

    lo_form_data->set(
      iv_key = 'field1'
      iv_val = 'val1' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_empty( lo_form_data )
      exp = abap_false ).

    lo_form_data = zcl_abapgit_string_map=>create( ).
    lo_form_data->set(
      iv_key = 'chk1'
      iv_val = 'X' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_empty( lo_form_data )
      exp = abap_false ).

    lo_form_data = zcl_abapgit_string_map=>create( ).
    lo_form_data->set(
      iv_key = 'num1'
      iv_val = '123' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_empty( lo_form_data )
      exp = abap_false ).

    lo_form_data = zcl_abapgit_string_map=>create( ).
    lo_form_data->set(
      iv_key = 'num1'
      iv_val = '0' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_empty( lo_form_data )
      exp = abap_true ).

    lo_form_data = zcl_abapgit_string_map=>create( ).
    lo_form_data->set(
      iv_key = |tab1-{ zif_abapgit_html_form=>c_rows }|
      iv_val = '2' ).
    lo_form_data->set(
      iv_key = |tab1-1-1|
      iv_val = '' ).
    lo_form_data->set(
      iv_key = |tab1-1-2|
      iv_val = '' ).
    lo_form_data->set(
      iv_key = |tab1-2-1|
      iv_val = '' ).
    lo_form_data->set(
      iv_key = |tab1-2-2|
      iv_val = 'Hello' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->is_empty( lo_form_data )
      exp = abap_false ).

  ENDMETHOD.

  METHOD exit_clean.

    DATA lo_cut TYPE REF TO zcl_abapgit_html_form_utils.
    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
    DATA lo_form_orig TYPE REF TO zcl_abapgit_string_map.
    DATA lo_form_data TYPE REF TO zcl_abapgit_string_map.

    lo_form      = zcl_abapgit_html_form=>create( ).
    lo_form_orig = zcl_abapgit_string_map=>create( ).
    lo_form_data = zcl_abapgit_string_map=>create( ).

    lo_form->text(
      iv_name  = 'field'
      iv_label = 'Field name' ).

    lo_cut = zcl_abapgit_html_form_utils=>create( lo_form ).

    " Clean form (no changes)
    lo_form_orig->set(
      iv_key = 'field'
      iv_val = 'val' ).

    lo_cut->set_data( lo_form_orig ).

    lo_form_data->set(
      iv_key = 'field'
      iv_val = 'val' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->exit( lo_form_data )
      exp = zcl_abapgit_gui=>c_event_state-go_back_to_bookmark ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_popups_mock->was_confirm_popup_shown( )
      exp = abap_false ).

  ENDMETHOD.

  METHOD exit_dirty_confirm.

    DATA lo_cut TYPE REF TO zcl_abapgit_html_form_utils.
    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
    DATA lo_form_orig TYPE REF TO zcl_abapgit_string_map.
    DATA lo_form_data TYPE REF TO zcl_abapgit_string_map.

    lo_form      = zcl_abapgit_html_form=>create( ).
    lo_form_orig = zcl_abapgit_string_map=>create( ).
    lo_form_data = zcl_abapgit_string_map=>create( ).

    lo_form->text(
      iv_name  = 'field'
      iv_label = 'Field name' ).

    lo_cut = zcl_abapgit_html_form_utils=>create( lo_form ).

    " Dirty form (changed field value)
    lo_form_orig = zcl_abapgit_string_map=>create( ).

    lo_cut->set_data( lo_form_orig ).

    lo_form_data->set(
      iv_key = 'field'
      iv_val = 'val' ).

    mo_popups_mock->set_user_decision( ltcl_popups_mock=>c_user_decision-confirm ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->exit( lo_form_data )
      exp = zcl_abapgit_gui=>c_event_state-go_back_to_bookmark ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_popups_mock->was_confirm_popup_shown( )
      exp = abap_true ).

  ENDMETHOD.

  METHOD exit_dirty_cancel.

    DATA lo_cut TYPE REF TO zcl_abapgit_html_form_utils.
    DATA lo_form TYPE REF TO zcl_abapgit_html_form.
    DATA lo_form_orig TYPE REF TO zcl_abapgit_string_map.
    DATA lo_form_data TYPE REF TO zcl_abapgit_string_map.

    lo_form      = zcl_abapgit_html_form=>create( ).
    lo_form_orig = zcl_abapgit_string_map=>create( ).
    lo_form_data = zcl_abapgit_string_map=>create( ).

    lo_form->text(
      iv_name  = 'field'
      iv_label = 'Field name' ).

    lo_cut = zcl_abapgit_html_form_utils=>create( lo_form ).

    " Dirty form (changed field value)
    lo_form_orig = zcl_abapgit_string_map=>create( ).

    lo_cut->set_data( lo_form_orig ).

    lo_form_data->set(
      iv_key = 'field'
      iv_val = 'val' ).

    mo_popups_mock->set_user_decision( ltcl_popups_mock=>c_user_decision-cancel ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->exit( lo_form_data )
      exp = zcl_abapgit_gui=>c_event_state-no_more_act ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_popups_mock->was_confirm_popup_shown( )
      exp = abap_true ).

  ENDMETHOD.
ENDCLASS.
