CLASS ltcl_test_mappers DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      from_json_to_json FOR TESTING RAISING zcx_abapgit_ajson_error,
      to_abap FOR TESTING RAISING zcx_abapgit_ajson_error,
      to_json FOR TESTING RAISING zcx_abapgit_ajson_error,
      to_json_nested_struc FOR TESTING RAISING zcx_abapgit_ajson_error,
      to_json_nested_table FOR TESTING RAISING zcx_abapgit_ajson_error,
      to_json_first_lower FOR TESTING RAISING zcx_abapgit_ajson_error.

    METHODS:
      to_snake FOR TESTING RAISING zcx_abapgit_ajson_error,
      to_camel FOR TESTING RAISING zcx_abapgit_ajson_error,
      to_camel_1st_upper FOR TESTING RAISING zcx_abapgit_ajson_error,
      rename_by_attr FOR TESTING RAISING zcx_abapgit_ajson_error,
      rename_by_path FOR TESTING RAISING zcx_abapgit_ajson_error,
      rename_by_pattern FOR TESTING RAISING zcx_abapgit_ajson_error,
      compound_mapper FOR TESTING RAISING zcx_abapgit_ajson_error,
      test_to_upper FOR TESTING RAISING zcx_abapgit_ajson_error,
      test_to_lower FOR TESTING RAISING zcx_abapgit_ajson_error.

ENDCLASS.


CLASS ltcl_test_mappers IMPLEMENTATION.


  METHOD from_json_to_json.

    DATA:
      lo_ajson TYPE REF TO zcl_abapgit_ajson.

    lo_ajson =
        zcl_abapgit_ajson=>parse(
            iv_json           = `{"fieldData":"field_value"}`
            ii_custom_mapping = zcl_abapgit_ajson_mapping=>create_camel_case( iv_first_json_upper = abap_false ) ).

    lo_ajson->set_string( iv_path = `/fieldData`
                          iv_val = 'E' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"fieldData":"E"}' ).

  ENDMETHOD.


  METHOD to_abap.

    DATA:
      lo_ajson   TYPE REF TO zcl_abapgit_ajson,
      li_mapping TYPE REF TO zif_abapgit_ajson_mapping.
    DATA:
      BEGIN OF ls_result,
        field_data TYPE string,
      END OF ls_result.

    li_mapping = zcl_abapgit_ajson_mapping=>create_camel_case( ).

    lo_ajson = zcl_abapgit_ajson=>parse( iv_json = '{"FieldData":"field_value"}'
                                         ii_custom_mapping = li_mapping ).

    lo_ajson->to_abap( IMPORTING ev_container = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-field_data
      exp = 'field_value' ).

  ENDMETHOD.


  METHOD to_json.

    DATA:
      lo_ajson   TYPE REF TO zcl_abapgit_ajson,
      li_mapping TYPE REF TO zif_abapgit_ajson_mapping.
    DATA:
      BEGIN OF ls_result,
        field_data TYPE string,
      END OF ls_result.

    li_mapping = zcl_abapgit_ajson_mapping=>create_camel_case( iv_first_json_upper = abap_false ).

    ls_result-field_data = 'field_value'.

    lo_ajson = zcl_abapgit_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/'
                   iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"fieldData":"field_value"}' ).

  ENDMETHOD.


  METHOD to_json_nested_struc.

    DATA:
      lo_ajson   TYPE REF TO zcl_abapgit_ajson,
      li_mapping TYPE REF TO zif_abapgit_ajson_mapping.
    DATA:
      BEGIN OF ls_result,
        field_data TYPE string,
        BEGIN OF struc_data,
          field_more TYPE string,
        END OF struc_data,
      END OF ls_result.

    li_mapping = zcl_abapgit_ajson_mapping=>create_camel_case( iv_first_json_upper = abap_false ).

    ls_result-field_data = 'field_value'.
    ls_result-struc_data-field_more = 'field_more'.

    lo_ajson = zcl_abapgit_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/'
                   iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"fieldData":"field_value","strucData":{"fieldMore":"field_more"}}' ).

  ENDMETHOD.


  METHOD to_json_nested_table.

    DATA:
      lo_ajson   TYPE REF TO zcl_abapgit_ajson,
      li_mapping TYPE REF TO zif_abapgit_ajson_mapping.
    DATA:
      lv_value TYPE string,
      BEGIN OF ls_result,
        field_data TYPE string,
        BEGIN OF struc_data,
          field_more TYPE string_table,
        END OF struc_data,
      END OF ls_result.

    li_mapping = zcl_abapgit_ajson_mapping=>create_camel_case( iv_first_json_upper = abap_false ).

    ls_result-field_data = 'field_value'.
    lv_value = 'field_more'.
    INSERT lv_value INTO TABLE ls_result-struc_data-field_more.

    lo_ajson = zcl_abapgit_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/'
                   iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"fieldData":"field_value","strucData":{"fieldMore":["field_more"]}}' ).

  ENDMETHOD.


  METHOD to_json_first_lower.

    DATA:
      lo_ajson   TYPE REF TO zcl_abapgit_ajson,
      li_mapping TYPE REF TO zif_abapgit_ajson_mapping.
    DATA:
      BEGIN OF ls_result,
        field_data TYPE string,
      END OF ls_result.

    li_mapping = zcl_abapgit_ajson_mapping=>create_camel_case( ).

    ls_result-field_data = 'field_value'.

    lo_ajson = zcl_abapgit_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/'
                   iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"FieldData":"field_value"}' ).

  ENDMETHOD.


  METHOD test_to_upper.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_ajson=>create_from(
        ii_source_json = zcl_abapgit_ajson=>parse( '{"a":1,"b":{"c":2}}' )
        ii_mapper      = zcl_abapgit_ajson_mapping=>create_upper_case( ) )->stringify( )
      exp = '{"A":1,"B":{"C":2}}' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_ajson=>parse( '{"a":1,"b":{"c":2}}'
        )->map( zcl_abapgit_ajson_mapping=>create_upper_case( )
        )->stringify( )
      exp = '{"A":1,"B":{"C":2}}' ).

  ENDMETHOD.

  METHOD test_to_lower.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_ajson=>create_from(
        ii_source_json = zcl_abapgit_ajson=>parse( '{"A":1,"B":{"C":2}}' )
        ii_mapper      = zcl_abapgit_ajson_mapping=>create_lower_case( ) )->stringify( )
      exp = '{"a":1,"b":{"c":2}}' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_ajson=>parse( '{"A":1,"B":{"C":2}}'
        )->map( zcl_abapgit_ajson_mapping=>create_lower_case( )
        )->stringify( )
      exp = '{"a":1,"b":{"c":2}}' ).

  ENDMETHOD.

  METHOD rename_by_attr.

    DATA lt_map TYPE zif_abapgit_ajson_mapping=>tty_rename_map.
    FIELD-SYMBOLS <i> LIKE LINE OF lt_map.

    APPEND INITIAL LINE TO lt_map ASSIGNING <i>.
    <i>-from = 'a'.
    <i>-to   = 'x'.
    APPEND INITIAL LINE TO lt_map ASSIGNING <i>.
    <i>-from = 'c'.
    <i>-to   = 'y'.
    APPEND INITIAL LINE TO lt_map ASSIGNING <i>.
    <i>-from = 'd'.
    <i>-to   = 'z'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_ajson=>create_from(
        ii_source_json = zcl_abapgit_ajson=>parse( '{"a":1,"b":{"c":2},"d":{"e":3}}' )
        ii_mapper      = zcl_abapgit_ajson_mapping=>create_rename( lt_map
        ) )->stringify( )
      exp = '{"b":{"y":2},"x":1,"z":{"e":3}}' ).

  ENDMETHOD.

  METHOD rename_by_path.

    DATA lt_map TYPE zif_abapgit_ajson_mapping=>tty_rename_map.
    FIELD-SYMBOLS <i> LIKE LINE OF lt_map.

    APPEND INITIAL LINE TO lt_map ASSIGNING <i>.
    <i>-from = '/b/a'.
    <i>-to   = 'x'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_ajson=>create_from(
        ii_source_json = zcl_abapgit_ajson=>parse( '{"a":1,"b":{"a":2},"c":{"a":3}}' )
        ii_mapper      = zcl_abapgit_ajson_mapping=>create_rename(
          it_rename_map = lt_map
          iv_rename_by  = zcl_abapgit_ajson_mapping=>rename_by-full_path
        ) )->stringify( )
      exp = '{"a":1,"b":{"x":2},"c":{"a":3}}' ).

  ENDMETHOD.

  METHOD rename_by_pattern.

    DATA lt_map TYPE zif_abapgit_ajson_mapping=>tty_rename_map.
    FIELD-SYMBOLS <i> LIKE LINE OF lt_map.

    APPEND INITIAL LINE TO lt_map ASSIGNING <i>.
    <i>-from = '/*/this*'.
    <i>-to   = 'x'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_ajson=>create_from(
        ii_source_json = zcl_abapgit_ajson=>parse( '{"andthisnot":1,"b":{"thisone":2},"c":{"a":3}}' )
        ii_mapper      = zcl_abapgit_ajson_mapping=>create_rename(
          it_rename_map = lt_map
          iv_rename_by  = zcl_abapgit_ajson_mapping=>rename_by-pattern
        ) )->stringify( )
      exp = '{"andthisnot":1,"b":{"x":2},"c":{"a":3}}' ).

  ENDMETHOD.

  METHOD compound_mapper.

    DATA lt_map TYPE zif_abapgit_ajson_mapping=>tty_rename_map.
    FIELD-SYMBOLS <i> LIKE LINE OF lt_map.

    APPEND INITIAL LINE TO lt_map ASSIGNING <i>.
    <i>-from = '/b/a'.
    <i>-to   = 'x'.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_ajson=>create_from(
        ii_source_json = zcl_abapgit_ajson=>parse( '{"a":1,"b":{"a":2},"c":{"a":3}}' )
        ii_mapper      = zcl_abapgit_ajson_mapping=>create_compound_mapper(
          ii_mapper1 = zcl_abapgit_ajson_mapping=>create_rename(
            it_rename_map = lt_map
            iv_rename_by  = zcl_abapgit_ajson_mapping=>rename_by-full_path )
          ii_mapper2 = zcl_abapgit_ajson_mapping=>create_upper_case( ) )
        )->stringify( )
      exp = '{"A":1,"B":{"X":2},"C":{"A":3}}' ).

  ENDMETHOD.

  METHOD to_snake.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_ajson=>create_from(
        ii_source_json = zcl_abapgit_ajson=>parse( '{"aB":1,"BbC":2,"cD":{"xY":3},"ZZ":4}' )
        ii_mapper      = zcl_abapgit_ajson_mapping=>create_to_snake_case( )
        )->stringify( )
      exp = '{"a_b":1,"bb_c":2,"c_d":{"x_y":3},"zz":4}' ).

  ENDMETHOD.

  METHOD to_camel.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_ajson=>create_from(
        ii_source_json = zcl_abapgit_ajson=>parse( '{"a_b":1,"bb_c":2,"c_d":{"x_y":3},"zz":4}' )
        ii_mapper      = zcl_abapgit_ajson_mapping=>create_to_camel_case( )
        )->stringify( )
      exp = '{"aB":1,"bbC":2,"cD":{"xY":3},"zz":4}' ).

    " Forced underscore
    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_ajson=>create_from(
        ii_source_json = zcl_abapgit_ajson=>parse( '{"a__b":1}' )
        ii_mapper      = zcl_abapgit_ajson_mapping=>create_to_camel_case( )
        )->stringify( )
      exp = '{"a_b":1}' ).

  ENDMETHOD.

  METHOD to_camel_1st_upper.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_ajson=>create_from(
        ii_source_json = zcl_abapgit_ajson=>parse( '{"aj_bc":1,"bb_c":2,"c_d":{"xq_yq":3},"zz":4}' )
        ii_mapper      = zcl_abapgit_ajson_mapping=>create_to_camel_case( iv_first_json_upper = abap_true )
        )->stringify( )
      exp = '{"AjBc":1,"BbC":2,"CD":{"XqYq":3},"Zz":4}' ).

  ENDMETHOD.

ENDCLASS.



CLASS ltcl_fields DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      to_json_without_path FOR TESTING RAISING zcx_abapgit_ajson_error,
      to_json_with_path FOR TESTING RAISING zcx_abapgit_ajson_error,
      to_abap FOR TESTING RAISING zcx_abapgit_ajson_error,
      to_json IMPORTING iv_path TYPE string RETURNING VALUE(rv_result) TYPE string RAISING zcx_abapgit_ajson_error.


ENDCLASS.


CLASS ltcl_fields IMPLEMENTATION.


  METHOD to_abap.

    DATA:
      lo_ajson          TYPE REF TO zcl_abapgit_ajson,
      li_mapping        TYPE REF TO zif_abapgit_ajson_mapping,
      lt_mapping_fields TYPE zif_abapgit_ajson_mapping=>ty_mapping_fields,
      ls_mapping_field  LIKE LINE OF lt_mapping_fields.
    DATA:
      BEGIN OF ls_result,
        abap_field TYPE string,
        field      TYPE string,
      END OF ls_result.

    CLEAR ls_mapping_field.
    ls_mapping_field-abap  = 'ABAP_FIELD'.
    ls_mapping_field-json = 'json.field'.
    INSERT ls_mapping_field INTO TABLE lt_mapping_fields.

    li_mapping = zcl_abapgit_ajson_mapping=>create_field_mapping( lt_mapping_fields ).

    lo_ajson =
        zcl_abapgit_ajson=>parse( iv_json = '{"field":"value","json.field":"field_value"}'
                                  ii_custom_mapping = li_mapping ).

    lo_ajson->to_abap( IMPORTING ev_container = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-abap_field
      exp = 'field_value' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_result-field
      exp = 'value' ).

  ENDMETHOD.


  METHOD to_json_without_path.

    cl_abap_unit_assert=>assert_equals(
      act = to_json( `/` )
      exp = '{"field":"value","json.field":"field_value"}' ).

  ENDMETHOD.


  METHOD to_json_with_path.

    cl_abap_unit_assert=>assert_equals(
      act = to_json( '/samplePath' )
      exp = '{"samplePath":{"field":"value","json.field":"field_value"}}' ).

  ENDMETHOD.


  METHOD to_json.

    DATA:
      lo_ajson          TYPE REF TO zcl_abapgit_ajson,
      li_mapping        TYPE REF TO zif_abapgit_ajson_mapping,
      lt_mapping_fields TYPE zif_abapgit_ajson_mapping=>ty_mapping_fields,
      ls_mapping_field  LIKE LINE OF lt_mapping_fields.
    DATA:
      BEGIN OF ls_result,
        abap_field TYPE string,
        field      TYPE string,
      END OF ls_result.

    CLEAR ls_mapping_field.
    ls_mapping_field-abap  = 'ABAP_FIELD'.
    ls_mapping_field-json = 'json.field'.
    INSERT ls_mapping_field INTO TABLE lt_mapping_fields.

    li_mapping = zcl_abapgit_ajson_mapping=>create_field_mapping( lt_mapping_fields ).

    ls_result-abap_field = 'field_value'.
    ls_result-field      = 'value'.

    lo_ajson = zcl_abapgit_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = iv_path
                   iv_val = ls_result ).

    rv_result = lo_ajson->stringify( ).

  ENDMETHOD.


ENDCLASS.



CLASS ltcl_to_lower DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      to_json FOR TESTING RAISING zcx_abapgit_ajson_error.
ENDCLASS.


CLASS ltcl_to_lower IMPLEMENTATION.


  METHOD to_json.

    DATA:
      lo_ajson   TYPE REF TO zcl_abapgit_ajson,
      li_mapping TYPE REF TO zif_abapgit_ajson_mapping.
    DATA:
      BEGIN OF ls_result,
        field_data TYPE string,
      END OF ls_result.

    li_mapping = zcl_abapgit_ajson_mapping=>create_lower_case( ).

    ls_result-field_data = 'field_value'.

    lo_ajson = zcl_abapgit_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/'
                   iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"field_data":"field_value"}' ).

  ENDMETHOD.


ENDCLASS.



CLASS ltcl_to_upper DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      to_json FOR TESTING RAISING zcx_abapgit_ajson_error.
ENDCLASS.


CLASS ltcl_to_upper IMPLEMENTATION.


  METHOD to_json.

    DATA:
      lo_ajson   TYPE REF TO zcl_abapgit_ajson,
      li_mapping TYPE REF TO zif_abapgit_ajson_mapping.
    DATA:
      BEGIN OF ls_result,
        field_data TYPE string,
      END OF ls_result.

    li_mapping = zcl_abapgit_ajson_mapping=>create_upper_case( ).

    ls_result-field_data = 'field_value'.

    lo_ajson = zcl_abapgit_ajson=>create_empty( ii_custom_mapping = li_mapping ).

    lo_ajson->set( iv_path = '/'
                   iv_val = ls_result ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_ajson->stringify( )
      exp = '{"FIELD_DATA":"field_value"}' ).

  ENDMETHOD.


ENDCLASS.
