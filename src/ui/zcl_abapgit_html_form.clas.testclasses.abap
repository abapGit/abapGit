CLASS ltcl_test_form DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    METHODS validate FOR TESTING RAISING zcx_abapgit_exception.
    METHODS normalize FOR TESTING RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_test_form IMPLEMENTATION.

  METHOD validate.

    DATA lo_cut TYPE REF TO zcl_abapgit_html_form.
    DATA lo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA lo_log TYPE REF TO zcl_abapgit_string_map.

    lo_cut       = zcl_abapgit_html_form=>create( ).
    lo_form_data = zcl_abapgit_string_map=>create( ).

    lo_cut->text(
      iv_name        = 'field1'
      iv_required    = abap_true
      iv_label       = 'Field name 1'
    )->text(
      iv_name        = 'field2'
      iv_label       = 'Field name 2' ).

    lo_log = lo_cut->validate_required_fields( lo_form_data ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lo_log->get( 'field1' )
      exp = '*cannot be empty' ).

    lo_form_data->set(
      iv_key = 'field1'
      iv_val = '' ).
    lo_log = lo_cut->validate_required_fields( lo_form_data ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lo_log->get( 'field1' )
      exp = '*cannot be empty' ).

    lo_form_data->set(
      iv_key = 'field1'
      iv_val = 'xyz' ).
    lo_log = lo_cut->validate_required_fields( lo_form_data ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 0 ).

  ENDMETHOD.

  METHOD normalize.

    DATA lo_cut TYPE REF TO zcl_abapgit_html_form.
    DATA lo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA lo_normalized_act TYPE REF TO zcl_abapgit_string_map.
    DATA lo_normalized_exp TYPE REF TO zcl_abapgit_string_map.

    lo_cut            = zcl_abapgit_html_form=>create( ).
    lo_form_data      = zcl_abapgit_string_map=>create( iv_case_insensitive = abap_true ).
    lo_normalized_exp = zcl_abapgit_string_map=>create( ).

    lo_cut->text(
      iv_name        = 'field1'
      iv_label       = 'Field name 1' ).
    lo_cut->text(
      iv_name        = 'field2'
      iv_upper_case  = abap_true
      iv_label       = 'Field name 2' ).
    lo_cut->text(
      iv_name        = 'field3'
      iv_label       = 'Field name 3' ).
    lo_cut->checkbox(
      iv_name        = 'chk1'
      iv_label       = 'Checkbox1' ).
    lo_cut->checkbox(
      iv_name        = 'chk2'
      iv_label       = 'Checkbox2' ).

    lo_form_data->set(
      iv_key = 'field1'
      iv_val = 'val1' ).
    lo_form_data->set(
      iv_key = 'field2'
      iv_val = 'val2' ).
    " Intentinally field3 is not specificed
    lo_form_data->set(
      iv_key = 'chk1'
      iv_val = '' ).
    lo_form_data->set(
      iv_key = 'chk2'
      iv_val = 'on' ).
    lo_form_data->set(
      iv_key = 'chk3'
      iv_val = 'on' ). " Extra field - filtered by normalizing

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

    lo_normalized_act = lo_cut->normalize_form_data( lo_form_data ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_normalized_act->mt_entries
      exp = lo_normalized_exp->mt_entries ).

  ENDMETHOD.

ENDCLASS.
