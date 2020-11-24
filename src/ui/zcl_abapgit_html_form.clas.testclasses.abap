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

    " New form
    lo_cut = zcl_abapgit_html_form=>create( ).

    lo_cut->text(
      iv_name        = 'field3'
      iv_min         = 3
      iv_max         = 10
      iv_label       = 'Field name 3' ).

    lo_form_data->set(
      iv_key = 'field3'
      iv_val = 'xy' ).
    lo_log = lo_cut->validate_required_fields( lo_form_data ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lo_log->get( 'field3' )
      exp = '*must not be shorter*' ).

    lo_form_data->set(
      iv_key = 'field3'
      iv_val = '01234567890123' ).
    lo_log = lo_cut->validate_required_fields( lo_form_data ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lo_log->get( 'field3' )
      exp = '*must not be longer*' ).

    lo_form_data->set(
      iv_key = 'field3'
      iv_val = 'xyz!' ).
    lo_log = lo_cut->validate_required_fields( lo_form_data ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 0 ).

    " New form
    lo_cut = zcl_abapgit_html_form=>create( ).

    lo_cut->number(
      iv_name        = 'field4'
      iv_min         = 100
      iv_max         = 200
      iv_label       = 'Field name 4' ).

    lo_form_data->set(
      iv_key = 'field4'
      iv_val = '123-456' ).
    lo_log = lo_cut->validate_required_fields( lo_form_data ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lo_log->get( 'field4' )
      exp = '*is not numeric*' ).

    lo_form_data->set(
      iv_key = 'field4'
      iv_val = '50' ).
    lo_log = lo_cut->validate_required_fields( lo_form_data ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lo_log->get( 'field4' )
      exp = '*must not be lower*' ).

    lo_form_data->set(
      iv_key = 'field4'
      iv_val = '250' ).
    lo_log = lo_cut->validate_required_fields( lo_form_data ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_log->size( )
      exp = 1 ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lo_log->get( 'field4' )
      exp = '*must not be higher*' ).

    lo_form_data->set(
      iv_key = 'field4'
      iv_val = '150' ).
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
    lo_cut->number(
      iv_name        = 'num1'
      iv_label       = 'Number 1' ).
    lo_cut->table(
      iv_name        = 'tab1'
      iv_label       = 'Table 1' ).
    lo_cut->column( iv_label = 'Column 1' ).
    lo_cut->column( iv_label = 'Column 2' ).
    lo_cut->number(
      iv_name        = |tab1-{ zcl_abapgit_html_form=>c_rows }|
      iv_label       = 'Number of Rows' ). " simulate hidden form field

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
      iv_key = |tab1-{ zcl_abapgit_html_form=>c_rows }|
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
      iv_key = |tab1-{ zcl_abapgit_html_form=>c_rows }|
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

    lo_normalized_act = lo_cut->normalize_form_data( lo_form_data ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_normalized_act->mt_entries
      exp = lo_normalized_exp->mt_entries ).

  ENDMETHOD.

ENDCLASS.
