

**********************************************************************
* UTIL
**********************************************************************
CLASS lcl_nodes_helper DEFINITION FINAL.
  PUBLIC SECTION.

    DATA mt_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt.
    METHODS add
      IMPORTING
        iv_str TYPE string.
    METHODS sorted
      RETURNING
        VALUE(rt_nodes) TYPE zif_abapgit_ajson=>ty_nodes_ts.

ENDCLASS.

CLASS lcl_nodes_helper IMPLEMENTATION.
  METHOD add.

    FIELD-SYMBOLS <n> LIKE LINE OF mt_nodes.
    DATA lv_children TYPE string.
    DATA lv_index TYPE string.
    DATA lv_order TYPE string.

    APPEND INITIAL LINE TO mt_nodes ASSIGNING <n>.

    SPLIT iv_str AT '|' INTO
      <n>-path
      <n>-name
      <n>-type
      <n>-value
      lv_index
      lv_children
      lv_order.
    CONDENSE <n>-path.
    CONDENSE <n>-name.
    CONDENSE <n>-type.
    CONDENSE <n>-value.
    <n>-index = lv_index.
    <n>-children = lv_children.
    <n>-order = lv_order.

  ENDMETHOD.

  METHOD sorted.
    rt_nodes = mt_nodes.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
* PARSER
**********************************************************************

CLASS ltcl_parser_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.

    CLASS-METHODS sample_json
      IMPORTING
        iv_separator TYPE string OPTIONAL
      RETURNING
        VALUE(rv_json) TYPE string.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_json_parser.
    DATA mo_nodes TYPE REF TO lcl_nodes_helper.

    METHODS setup.
    METHODS parse FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS parse_string FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS parse_number FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS parse_float FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS parse_boolean FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS parse_false FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS parse_null FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS parse_date FOR TESTING RAISING zcx_abapgit_ajson_error.

ENDCLASS.

CLASS ltcl_parser_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
    CREATE OBJECT mo_nodes.
  ENDMETHOD.

  METHOD parse_string.
    mo_nodes->add( '                 |         |object |                        |  |1' ).
    mo_nodes->add( '/                |string   |str    |abc                     |  |0' ).

    DATA lt_act TYPE zif_abapgit_ajson=>ty_nodes_tt.
    lt_act = mo_cut->parse( '{"string": "abc"}' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).
  ENDMETHOD.

  METHOD parse_number.
    mo_nodes->add( '                 |         |object |                        |  |1' ).
    mo_nodes->add( '/                |number   |num    |123                     |  |0' ).

    DATA lt_act TYPE zif_abapgit_ajson=>ty_nodes_tt.
    lt_act = mo_cut->parse( '{"number": 123}' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).
  ENDMETHOD.

  METHOD parse_float.
    mo_nodes->add( '                 |         |object |                        |  |1' ).
    mo_nodes->add( '/                |float    |num    |123.45                  |  |0' ).

    DATA lt_act TYPE zif_abapgit_ajson=>ty_nodes_tt.
    CREATE OBJECT mo_cut.
    lt_act = mo_cut->parse( '{"float": 123.45}' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).
  ENDMETHOD.

  METHOD parse_boolean.
    mo_nodes->add( '                 |         |object |                        |  |1' ).
    mo_nodes->add( '/                |boolean  |bool   |true                    |  |0' ).

    DATA lt_act TYPE zif_abapgit_ajson=>ty_nodes_tt.
    lt_act = mo_cut->parse( '{"boolean": true}' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).
  ENDMETHOD.

  METHOD parse_false.
    mo_nodes->add( '                 |         |object |                        |  |1' ).
    mo_nodes->add( '/                |false    |bool   |false                   |  |0' ).

    DATA lt_act TYPE zif_abapgit_ajson=>ty_nodes_tt.
    lt_act = mo_cut->parse( '{"false": false}' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).
  ENDMETHOD.

  METHOD parse_null.
    mo_nodes->add( '                 |         |object |                        |  |1' ).
    mo_nodes->add( '/                |null     |null   |                        |  |0' ).

    DATA lt_act TYPE zif_abapgit_ajson=>ty_nodes_tt.
    lt_act = mo_cut->parse( '{"null": null}' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).
  ENDMETHOD.

  METHOD parse_date.
    mo_nodes->add( '                 |         |object |                        |  |1' ).
    mo_nodes->add( '/                |date     |str    |2020-03-15              |  |0' ).

    DATA lt_act TYPE zif_abapgit_ajson=>ty_nodes_tt.
    lt_act = mo_cut->parse( '{"date": "2020-03-15"}' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = mo_nodes->mt_nodes ).
  ENDMETHOD.

  METHOD sample_json.

    rv_json =
      '{\n' &&
      '  "string": "abc",\n' &&
      '  "number": 123,\n' &&
      '  "float": 123.45,\n' &&
      '  "boolean": true,\n' &&
      '  "false": false,\n' &&
      '  "null": null,\n' &&
      '  "date": "2020-03-15",\n' &&
      '  "issues": [\n' &&
      '    {\n' &&
      '      "message": "Indentation problem ...",\n' &&
      '      "key": "indentation",\n' &&
      '      "start": {\n' &&
      '        "row": 4,\n' &&
      '        "col": 3\n' &&
      '      },\n' &&
      '      "end": {\n' &&
      '        "row": 4,\n' &&
      '        "col": 26\n' &&
      '      },\n' &&
      '      "filename": "./zxxx.prog.abap"\n' &&
      '    },\n' &&
      '    {\n' &&
      '      "message": "Remove space before XXX",\n' &&
      '      "key": "space_before_dot",\n' &&
      '      "start": {\n' &&
      '        "row": 3,\n' &&
      '        "col": 21\n' &&
      '      },\n' &&
      '      "end": {\n' &&
      '        "row": 3,\n' &&
      '        "col": 22\n' &&
      '      },\n' &&
      '      "filename": "./zxxx.prog.abap"\n' &&
      '    }\n' &&
      '  ]\n' &&
      '}'.

    REPLACE ALL OCCURRENCES OF '\n' IN rv_json WITH iv_separator.

  ENDMETHOD.

  METHOD parse.

    DATA lo_cut TYPE REF TO lcl_json_parser.
    DATA lt_act TYPE zif_abapgit_ajson=>ty_nodes_tt.
    DATA lo_nodes TYPE REF TO lcl_nodes_helper.

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '                 |         |object |                        |  |8' ).
    lo_nodes->add( '/                |string   |str    |abc                     |  |0' ).
    lo_nodes->add( '/                |number   |num    |123                     |  |0' ).
    lo_nodes->add( '/                |float    |num    |123.45                  |  |0' ).
    lo_nodes->add( '/                |boolean  |bool   |true                    |  |0' ).
    lo_nodes->add( '/                |false    |bool   |false                   |  |0' ).
    lo_nodes->add( '/                |null     |null   |                        |  |0' ).
    lo_nodes->add( '/                |date     |str    |2020-03-15              |  |0' ).
    lo_nodes->add( '/                |issues   |array  |                        |  |2' ).
    lo_nodes->add( '/issues/         |1        |object |                        |1 |5' ).
    lo_nodes->add( '/issues/1/       |message  |str    |Indentation problem ... |  |0' ).
    lo_nodes->add( '/issues/1/       |key      |str    |indentation             |  |0' ).
    lo_nodes->add( '/issues/1/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/issues/1/start/ |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/issues/1/start/ |col      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/1/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/issues/1/end/   |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/issues/1/end/   |col      |num    |26                      |  |0' ).
    lo_nodes->add( '/issues/1/       |filename |str    |./zxxx.prog.abap        |  |0' ).
    lo_nodes->add( '/issues/         |2        |object |                        |2 |5' ).
    lo_nodes->add( '/issues/2/       |message  |str    |Remove space before XXX |  |0' ).
    lo_nodes->add( '/issues/2/       |key      |str    |space_before_dot        |  |0' ).
    lo_nodes->add( '/issues/2/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/issues/2/start/ |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/2/start/ |col      |num    |21                      |  |0' ).
    lo_nodes->add( '/issues/2/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/issues/2/end/   |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/2/end/   |col      |num    |22                      |  |0' ).
    lo_nodes->add( '/issues/2/       |filename |str    |./zxxx.prog.abap        |  |0' ).

    CREATE OBJECT lo_cut.
    lt_act = lo_cut->parse( sample_json( ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lo_nodes->mt_nodes ).

    lt_act = lo_cut->parse( sample_json( |{ cl_abap_char_utilities=>newline }| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lo_nodes->mt_nodes ).

    lt_act = lo_cut->parse( sample_json( |{ cl_abap_char_utilities=>cr_lf }| ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lo_nodes->mt_nodes ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* SERIALIZER
**********************************************************************

CLASS ltcl_serializer_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PUBLIC SECTION.

    CLASS-METHODS sample_json
      RETURNING
        VALUE(rv_json) TYPE string.
    CLASS-METHODS sample_nodes
      RETURNING
        VALUE(rt_nodes) TYPE zif_abapgit_ajson=>ty_nodes_ts.

  PRIVATE SECTION.

    METHODS stringify_condensed FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS stringify_indented FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS array_index FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS item_order FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS simple_indented FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS empty_set FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS escape FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS empty FOR TESTING RAISING zcx_abapgit_ajson_error.

ENDCLASS.

CLASS ltcl_serializer_test IMPLEMENTATION.

  METHOD sample_json.

    rv_json =
      '{\n' &&
      '  "boolean": true,\n' &&
      '  "date": "2020-03-15",\n' &&
      '  "false": false,\n' &&
      '  "float": 123.45,\n' &&
      '  "issues": [\n' &&
      '    {\n' &&
      '      "end": {\n' &&
      '        "col": 26,\n' &&
      '        "row": 4\n' &&
      '      },\n' &&
      '      "filename": "./zxxx.prog.abap",\n' &&
      '      "key": "indentation",\n' &&
      '      "message": "Indentation problem ...",\n' &&
      '      "start": {\n' &&
      '        "col": 3,\n' &&
      '        "row": 4\n' &&
      '      }\n' &&
      '    },\n' &&
      '    {\n' &&
      '      "end": {\n' &&
      '        "col": 22,\n' &&
      '        "row": 3\n' &&
      '      },\n' &&
      '      "filename": "./zxxx.prog.abap",\n' &&
      '      "key": "space_before_dot",\n' &&
      '      "message": "Remove space before XXX",\n' &&
      '      "start": {\n' &&
      '        "col": 21,\n' &&
      '        "row": 3\n' &&
      '      }\n' &&
      '    }\n' &&
      '  ],\n' &&
      '  "null": null,\n' &&
      '  "number": 123,\n' &&
      '  "string": "abc"\n' &&
      '}'.

    rv_json = replace(
      val = rv_json
      sub = '\n'
      with = cl_abap_char_utilities=>newline
      occ = 0 ).

  ENDMETHOD.

  METHOD sample_nodes.

    DATA lo_nodes TYPE REF TO lcl_nodes_helper.

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '                 |         |object |                        |  |8' ).
    lo_nodes->add( '/                |string   |str    |abc                     |  |0' ).
    lo_nodes->add( '/                |number   |num    |123                     |  |0' ).
    lo_nodes->add( '/                |float    |num    |123.45                  |  |0' ).
    lo_nodes->add( '/                |boolean  |bool   |true                    |  |0' ).
    lo_nodes->add( '/                |false    |bool   |false                   |  |0' ).
    lo_nodes->add( '/                |null     |null   |                        |  |0' ).
    lo_nodes->add( '/                |date     |str    |2020-03-15              |  |0' ).
    lo_nodes->add( '/                |issues   |array  |                        |  |2' ).
    lo_nodes->add( '/issues/         |1        |object |                        |1 |5' ).
    lo_nodes->add( '/issues/1/       |message  |str    |Indentation problem ... |  |0' ).
    lo_nodes->add( '/issues/1/       |key      |str    |indentation             |  |0' ).
    lo_nodes->add( '/issues/1/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/issues/1/start/ |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/issues/1/start/ |col      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/1/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/issues/1/end/   |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/issues/1/end/   |col      |num    |26                      |  |0' ).
    lo_nodes->add( '/issues/1/       |filename |str    |./zxxx.prog.abap        |  |0' ).
    lo_nodes->add( '/issues/         |2        |object |                        |2 |5' ).
    lo_nodes->add( '/issues/2/       |message  |str    |Remove space before XXX |  |0' ).
    lo_nodes->add( '/issues/2/       |key      |str    |space_before_dot        |  |0' ).
    lo_nodes->add( '/issues/2/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/issues/2/start/ |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/2/start/ |col      |num    |21                      |  |0' ).
    lo_nodes->add( '/issues/2/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/issues/2/end/   |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/2/end/   |col      |num    |22                      |  |0' ).
    lo_nodes->add( '/issues/2/       |filename |str    |./zxxx.prog.abap        |  |0' ).

    rt_nodes = lo_nodes->sorted( ).

  ENDMETHOD.

  METHOD stringify_condensed.

    DATA lv_act TYPE string.
    DATA lv_exp TYPE string.

    lv_act = lcl_json_serializer=>stringify( sample_nodes( ) ).
    lv_exp = sample_json( ).

    lv_exp = replace(
      val = lv_exp
      sub = cl_abap_char_utilities=>newline
      with = ''
      occ = 0 ).
    CONDENSE lv_exp.
    lv_exp = replace(
      val = lv_exp
      sub = `: `
      with = ':'
      occ = 0 ).
    lv_exp = replace(
      val = lv_exp
      sub = `{ `
      with = '{'
      occ = 0 ).
    lv_exp = replace(
      val = lv_exp
      sub = `[ `
      with = '['
      occ = 0 ).
    lv_exp = replace(
      val = lv_exp
      sub = ` }`
      with = '}'
      occ = 0 ).
    lv_exp = replace(
      val = lv_exp
      sub = ` ]`
      with = ']'
      occ = 0 ).
    lv_exp = replace(
      val = lv_exp
      sub = `, `
      with = ','
      occ = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD stringify_indented.

    DATA lv_act TYPE string.
    DATA lv_exp TYPE string.

    lv_act = lcl_json_serializer=>stringify(
      it_json_tree = sample_nodes( )
      iv_indent    = 2 ).
    lv_exp = sample_json( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD array_index.

    DATA lv_act TYPE string.
    DATA lv_exp TYPE string.
    DATA lo_nodes TYPE REF TO lcl_nodes_helper.

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '                |    |array  |                        |  |3' ).
    lo_nodes->add( '/               |1   |str    |abc                     |2 |0' ).
    lo_nodes->add( '/               |2   |num    |123                     |1 |0' ).
    lo_nodes->add( '/               |3   |num    |123.45                  |3 |0' ).

    lv_act = lcl_json_serializer=>stringify( lo_nodes->sorted( ) ).
    lv_exp = '[123,"abc",123.45]'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD item_order.

    DATA lv_act TYPE string.
    DATA lv_exp TYPE string.
    DATA lo_nodes TYPE REF TO lcl_nodes_helper.

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '                |       |object |                   |  |3 |0' ).
    lo_nodes->add( '/               |beta   |str    |b                  |  |0 |3' ).
    lo_nodes->add( '/               |zulu   |str    |z                  |  |0 |1' ).
    lo_nodes->add( '/               |alpha  |str    |a                  |  |0 |2' ).

    lv_act = lcl_json_serializer=>stringify( lo_nodes->sorted( ) ).
    lv_exp = '{"alpha":"a","beta":"b","zulu":"z"}'. " NAME order ! (it is also a UT)

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

    lv_act = lcl_json_serializer=>stringify(
      it_json_tree = lo_nodes->sorted( )
      iv_keep_item_order = abap_true ).
    lv_exp = '{"zulu":"z","alpha":"a","beta":"b"}'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD simple_indented.

    DATA lv_act TYPE string.
    DATA lv_exp TYPE string.
    DATA lo_nodes TYPE REF TO lcl_nodes_helper.

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '                |    |array  |                        |  |3' ).
    lo_nodes->add( '/               |1   |object |                        |2 |2' ).
    lo_nodes->add( '/1/             |a   |num    |1                       |  |0' ).
    lo_nodes->add( '/1/             |b   |num    |2                       |  |0' ).
    lo_nodes->add( '/               |2   |num    |123                     |1 |0' ).
    lo_nodes->add( '/               |3   |num    |123.45                  |3 |0' ).

    lv_act = lcl_json_serializer=>stringify(
      it_json_tree = lo_nodes->sorted( )
      iv_indent    = 2 ).
    lv_exp = '[\n' &&
    '  123,\n' &&
    '  {\n' &&
    '    "a": 1,\n' &&
    '    "b": 2\n' &&
    '  },\n' &&
    '  123.45\n' &&
    ']'.
    lv_exp = replace(
      val = lv_exp
      sub = '\n'
      with = cl_abap_char_utilities=>newline
      occ = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD empty_set.

    DATA lv_act TYPE string.
    DATA lv_exp TYPE string.
    DATA lo_nodes TYPE REF TO lcl_nodes_helper.

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '                |    |array  |                        |  |0' ).

    lv_act = lcl_json_serializer=>stringify(
      it_json_tree = lo_nodes->sorted( )
      iv_indent    = 0 ).
    lv_exp = '[]'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

    lv_act = lcl_json_serializer=>stringify(
      it_json_tree = lo_nodes->sorted( )
      iv_indent    = 2 ).
    lv_exp = '[]'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD escape.

    DATA lv_act TYPE string.
    DATA lv_exp TYPE string.
    DATA lv_val TYPE string.
    DATA lo_nodes TYPE REF TO lcl_nodes_helper.

    CREATE OBJECT lo_nodes.
    lv_val = 'a' && '"' && '\' && cl_abap_char_utilities=>horizontal_tab && cl_abap_char_utilities=>cr_lf.
    lo_nodes->add( | \| \|str \|{ lv_val }\| \|0| ).

    lv_act = lcl_json_serializer=>stringify( lo_nodes->sorted( ) ).
    lv_exp = '"a\"\\\t\r\n"'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

  METHOD empty.

    DATA lv_act TYPE string.
    DATA lv_exp TYPE string.
    DATA lo_nodes TYPE REF TO lcl_nodes_helper.

    CREATE OBJECT lo_nodes.

    lv_act = lcl_json_serializer=>stringify( lo_nodes->sorted( ) ).
    lv_exp = ''.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* UTILS
**********************************************************************

CLASS ltcl_utils_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.

    METHODS normalize_path FOR TESTING.
    METHODS split_path FOR TESTING.
    METHODS validate_array_index FOR TESTING RAISING zcx_abapgit_ajson_error.

ENDCLASS.

CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS ltcl_utils_test.

CLASS ltcl_utils_test IMPLEMENTATION.

  METHOD validate_array_index.

    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>validate_array_index( iv_path = 'x' iv_index = '123' )
      exp = 123 ).

    TRY.
        lcl_utils=>validate_array_index( iv_path = 'x'
                                         iv_index = 'a' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error.
    ENDTRY.

    TRY.
        lcl_utils=>validate_array_index( iv_path = 'x'
                                         iv_index = '0' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error.
    ENDTRY.

  ENDMETHOD.

  METHOD normalize_path.

    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>normalize_path( '' )
      exp = '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>normalize_path( '/' )
      exp = '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>normalize_path( 'abc' )
      exp = '/abc/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>normalize_path( '/abc' )
      exp = '/abc/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>normalize_path( 'abc/' )
      exp = '/abc/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>normalize_path( '/abc/' )
      exp = '/abc/' ).

  ENDMETHOD.

  METHOD split_path.

    DATA ls_exp TYPE zif_abapgit_ajson=>ty_path_name.
    DATA lv_path TYPE string.

    lv_path     = ''. " alias to root
    ls_exp-path = ''.
    ls_exp-name = ''.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = '/'.
    ls_exp-path = ''.
    ls_exp-name = ''.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = '/abc/'.
    ls_exp-path = '/'.
    ls_exp-name = 'abc'.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = 'abc'.
    ls_exp-path = '/'.
    ls_exp-name = 'abc'.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = '/abc'.
    ls_exp-path = '/'.
    ls_exp-name = 'abc'.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = 'abc/'.
    ls_exp-path = '/'.
    ls_exp-name = 'abc'.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = '/abc/xyz'.
    ls_exp-path = '/abc/'.
    ls_exp-name = 'xyz'.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

    lv_path     = '/abc/xyz/'.
    ls_exp-path = '/abc/'.
    ls_exp-name = 'xyz'.
    cl_abap_unit_assert=>assert_equals(
      act = lcl_utils=>split_path( lv_path )
      exp = ls_exp ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* READER
**********************************************************************

CLASS ltcl_reader_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.

    METHODS get_value FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS get_node_type FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS exists FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS value_integer FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS value_number FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS value_boolean FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS value_string FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS members FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS slice FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS array_to_string_table FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS get_date FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS get_timestamp FOR TESTING RAISING zcx_abapgit_ajson_error.

ENDCLASS.

CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS ltcl_reader_test.

CLASS ltcl_reader_test IMPLEMENTATION.

  METHOD slice.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA lo_nodes TYPE REF TO lcl_nodes_helper.

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '          |         |array  |                        |  |2' ).
    lo_nodes->add( '/         |1        |object |                        |1 |5' ).
    lo_nodes->add( '/1/       |message  |str    |Indentation problem ... |  |0' ).
    lo_nodes->add( '/1/       |key      |str    |indentation             |  |0' ).
    lo_nodes->add( '/1/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/1/start/ |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/1/start/ |col      |num    |3                       |  |0' ).
    lo_nodes->add( '/1/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/1/end/   |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/1/end/   |col      |num    |26                      |  |0' ).
    lo_nodes->add( '/1/       |filename |str    |./zxxx.prog.abap        |  |0' ).
    lo_nodes->add( '/         |2        |object |                        |2 |5' ).
    lo_nodes->add( '/2/       |message  |str    |Remove space before XXX |  |0' ).
    lo_nodes->add( '/2/       |key      |str    |space_before_dot        |  |0' ).
    lo_nodes->add( '/2/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/2/start/ |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/2/start/ |col      |num    |21                      |  |0' ).
    lo_nodes->add( '/2/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/2/end/   |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/2/end/   |col      |num    |22                      |  |0' ).
    lo_nodes->add( '/2/       |filename |str    |./zxxx.prog.abap        |  |0' ).


    lo_cut = zcl_abapgit_ajson=>parse( ltcl_parser_test=>sample_json( ) ).
    lo_cut ?= lo_cut->zif_abapgit_ajson_reader~slice( '/issues' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

    " **********************************************************************

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '                 |         |object |                        |  |8' ).
    lo_nodes->add( '/                |string   |str    |abc                     |  |0' ).
    lo_nodes->add( '/                |number   |num    |123                     |  |0' ).
    lo_nodes->add( '/                |float    |num    |123.45                  |  |0' ).
    lo_nodes->add( '/                |boolean  |bool   |true                    |  |0' ).
    lo_nodes->add( '/                |false    |bool   |false                   |  |0' ).
    lo_nodes->add( '/                |null     |null   |                        |  |0' ).
    lo_nodes->add( '/                |date     |str    |2020-03-15              |  |0' ).
    lo_nodes->add( '/                |issues   |array  |                        |  |2' ).
    lo_nodes->add( '/issues/         |1        |object |                        |1 |5' ).
    lo_nodes->add( '/issues/1/       |message  |str    |Indentation problem ... |  |0' ).
    lo_nodes->add( '/issues/1/       |key      |str    |indentation             |  |0' ).
    lo_nodes->add( '/issues/1/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/issues/1/start/ |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/issues/1/start/ |col      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/1/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/issues/1/end/   |row      |num    |4                       |  |0' ).
    lo_nodes->add( '/issues/1/end/   |col      |num    |26                      |  |0' ).
    lo_nodes->add( '/issues/1/       |filename |str    |./zxxx.prog.abap        |  |0' ).
    lo_nodes->add( '/issues/         |2        |object |                        |2 |5' ).
    lo_nodes->add( '/issues/2/       |message  |str    |Remove space before XXX |  |0' ).
    lo_nodes->add( '/issues/2/       |key      |str    |space_before_dot        |  |0' ).
    lo_nodes->add( '/issues/2/       |start    |object |                        |  |2' ).
    lo_nodes->add( '/issues/2/start/ |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/2/start/ |col      |num    |21                      |  |0' ).
    lo_nodes->add( '/issues/2/       |end      |object |                        |  |2' ).
    lo_nodes->add( '/issues/2/end/   |row      |num    |3                       |  |0' ).
    lo_nodes->add( '/issues/2/end/   |col      |num    |22                      |  |0' ).
    lo_nodes->add( '/issues/2/       |filename |str    |./zxxx.prog.abap        |  |0' ).

    lo_cut = zcl_abapgit_ajson=>parse( ltcl_parser_test=>sample_json( ) ).
    lo_cut ?= lo_cut->zif_abapgit_ajson_reader~slice( '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

    " **********************************************************************

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '  |         |object |                        | |2' ).
    lo_nodes->add( '/ |row      |num    |3                       | |0' ).
    lo_nodes->add( '/ |col      |num    |21                      | |0' ).

    lo_cut = zcl_abapgit_ajson=>parse( ltcl_parser_test=>sample_json( ) ).
    lo_cut ?= lo_cut->zif_abapgit_ajson_reader~slice( '/issues/2/start/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

  ENDMETHOD.

  METHOD get_value.

    DATA lo_cut TYPE REF TO zif_abapgit_ajson_reader.
    lo_cut ?= zcl_abapgit_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( '/string' )
      exp = 'abc' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( '/string/' )
      exp = 'abc' ). " Hmmm ?

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( '/boolean' )
      exp = 'true' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get( '/issues/2/start/row' )
      exp = '3' ).

  ENDMETHOD.

  METHOD get_node_type.

    DATA li_cut TYPE REF TO zif_abapgit_ajson_reader.
    li_cut = zcl_abapgit_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/' )
      exp = 'object' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/string' )
      exp = 'str' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/number' )
      exp = 'num' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/float' )
      exp = 'num' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/boolean' )
      exp = 'bool' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/false' )
      exp = 'bool' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/null' )
      exp = 'null' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/date' )
      exp = 'str' ).
    cl_abap_unit_assert=>assert_equals(
      act = li_cut->get_node_type( '/issues' )
      exp = 'array' ).

  ENDMETHOD.

  METHOD get_date.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA lo_nodes TYPE REF TO lcl_nodes_helper.
    DATA lv_exp TYPE d.

    CREATE OBJECT lo_cut.
    lv_exp = '20200728'.

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '  |         |object |                        | |1' ).
    lo_nodes->add( '/ |date1    |str    |2020-07-28              | |0' ).
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->zif_abapgit_ajson_reader~get_date( '/date1' )
      exp = lv_exp ).

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '  |         |object |                        | |1' ).
    lo_nodes->add( '/ |date1    |str    |2020-07-28T01:00:00Z    | |0' ).
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->zif_abapgit_ajson_reader~get_date( '/date1' )
      exp = lv_exp ).

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '  |         |object |                        | |1' ).
    lo_nodes->add( '/ |date1    |str    |20200728                | |0' ).
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->zif_abapgit_ajson_reader~get_date( '/date1' )
      exp = '' ).

  ENDMETHOD.

  METHOD get_timestamp.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA lo_nodes TYPE REF TO lcl_nodes_helper.
    DATA lv_exp TYPE timestamp VALUE `20200728000000`.

    CREATE OBJECT lo_cut.

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '  |         |object |                        | |1' ).
    lo_nodes->add( '/ |timestamp|str    |2020-07-28T00:00:00Z    | |0' ).
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->zif_abapgit_ajson_reader~get_timestamp( '/timestamp' )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD exists.

    DATA lo_cut TYPE REF TO zif_abapgit_ajson_reader.
    lo_cut ?= zcl_abapgit_ajson=>parse( ltcl_parser_test=>sample_json( ) ).


    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->exists( '/string' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->exists( '/string/' )
      exp = abap_true ). " mmmm ?

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->exists( '/xxx' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->exists( '/issues/2/start/row' )
      exp = abap_true ).

  ENDMETHOD.

  METHOD value_integer.

    DATA lo_cut TYPE REF TO zif_abapgit_ajson_reader.
    lo_cut ?= zcl_abapgit_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_integer( '/string' )
      exp = 0 ). " Hmmmm ????

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_integer( '/number' )
      exp = 123 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_integer( '/float' )
      exp = 123 ).

  ENDMETHOD.

  METHOD value_number.

    DATA lo_cut TYPE REF TO zif_abapgit_ajson_reader.
    lo_cut ?= zcl_abapgit_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_number( '/string' )
      exp = 0 ). " Hmmmm ????

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_number( '/number' )
      exp = +'123.0' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_number( '/float' )
      exp = +'123.45' ).

  ENDMETHOD.

  METHOD value_boolean.

    DATA lo_cut TYPE REF TO zif_abapgit_ajson_reader.
    lo_cut ?= zcl_abapgit_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_boolean( '/string' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_boolean( '/number' )
      exp = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_boolean( '/xxx' )
      exp = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_boolean( '/boolean' )
      exp = abap_true ).

  ENDMETHOD.

  METHOD value_string.

    DATA lo_cut TYPE REF TO zif_abapgit_ajson_reader.
    lo_cut ?= zcl_abapgit_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_string( '/string' )
      exp = 'abc' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_string( '/number' )
      exp = '123' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_string( '/xxx' )
      exp = '' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->get_string( '/boolean' )
      exp = 'true' ).

  ENDMETHOD.

  METHOD members.

    DATA lt_exp TYPE string_table.
    DATA lo_cut TYPE REF TO zif_abapgit_ajson_reader.
    lo_cut ?= zcl_abapgit_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    CLEAR lt_exp.
    APPEND '1' TO lt_exp.
    APPEND '2' TO lt_exp.
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->members( '/issues' )
      exp = lt_exp ).

    CLEAR lt_exp.
    APPEND 'col' TO lt_exp.
    APPEND 'row' TO lt_exp.
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->members( '/issues/1/start/' )
      exp = lt_exp ).

  ENDMETHOD.

  METHOD array_to_string_table.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA lo_nodes TYPE REF TO lcl_nodes_helper.
    DATA lt_act TYPE string_table.
    DATA lt_exp TYPE string_table.

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '  |         |array  |                        | |6' ).
    lo_nodes->add( '/ |1        |num    |123                     |1|0' ).
    lo_nodes->add( '/ |2        |num    |234                     |2|0' ).
    lo_nodes->add( '/ |3        |str    |abc                     |3|0' ).
    lo_nodes->add( '/ |4        |bool   |true                    |4|0' ).
    lo_nodes->add( '/ |5        |bool   |false                   |5|0' ).
    lo_nodes->add( '/ |6        |null   |null                    |6|0' ).

    APPEND '123' TO lt_exp.
    APPEND '234' TO lt_exp.
    APPEND 'abc' TO lt_exp.
    APPEND 'X' TO lt_exp.
    APPEND '' TO lt_exp.
    APPEND '' TO lt_exp.

    CREATE OBJECT lo_cut.
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    lt_act = lo_cut->zif_abapgit_ajson_reader~array_to_string_table( '/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

    " negative
    DATA lx TYPE REF TO zcx_abapgit_ajson_error.

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '  |         |object |                        | |1' ).
    lo_nodes->add( '/ |a        |str    |abc                     | |0' ).
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    TRY.
        lo_cut->zif_abapgit_ajson_reader~array_to_string_table( '/x' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Path not found: /x' ).
    ENDTRY.

    TRY.
        lo_cut->zif_abapgit_ajson_reader~array_to_string_table( '/' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Array expected at: /' ).
    ENDTRY.

    TRY.
        lo_cut->zif_abapgit_ajson_reader~array_to_string_table( '/a' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Array expected at: /a' ).
    ENDTRY.

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '  |         |array  |                        | |1' ).
    lo_nodes->add( '/ |1        |object |                        |1|0' ).
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    TRY.
        lo_cut->zif_abapgit_ajson_reader~array_to_string_table( '/' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Cannot convert [object] to string at [/1]' ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.


**********************************************************************
* JSON TO ABAP
**********************************************************************

CLASS ltcl_json_to_abap DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_struc,
        a TYPE string,
        b TYPE i,
      END OF ty_struc,
      tty_struc TYPE STANDARD TABLE OF ty_struc WITH DEFAULT KEY,
      BEGIN OF ty_complex,
        str   TYPE string,
        int   TYPE i,
        float TYPE f,
        bool  TYPE abap_bool,
        obj   TYPE ty_struc,
        tab   TYPE tty_struc,
        oref  TYPE REF TO object,
        date1 TYPE d,
        date2 TYPE d,
        timestamp1 TYPE timestamp,
        timestamp2 TYPE timestamp,
        timestamp3 TYPE timestamp,
      END OF ty_complex.

    METHODS find_loc FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS find_loc_negative FOR TESTING.
    METHODS find_loc_append FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS to_abap FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS to_abap_negative FOR TESTING.

    METHODS prepare_cut
      EXPORTING
        eo_cut TYPE REF TO lcl_json_to_abap
        e_elem TYPE ty_struc
        e_mock TYPE ty_complex.

ENDCLASS.

CLASS ltcl_json_to_abap IMPLEMENTATION.

  METHOD prepare_cut.

    e_mock-str = 'Hello'.
    e_mock-int = 10.
    e_mock-obj-a = 'World'.
    e_elem-a = 'One'.
    e_elem-b = 1.
    APPEND e_elem TO e_mock-tab.
    e_elem-a = 'two'.
    e_elem-b = 2.
    APPEND e_elem TO e_mock-tab.

    lcl_json_to_abap=>bind(
      CHANGING
        c_obj = e_mock
        co_instance = eo_cut ).

  ENDMETHOD.

  METHOD find_loc.

    DATA last_elem TYPE ty_struc.
    DATA ls_mock TYPE ty_complex.
    DATA lo_cut TYPE REF TO lcl_json_to_abap.

    prepare_cut(
      IMPORTING
        eo_cut = lo_cut
        e_mock = ls_mock
        e_elem = last_elem ).

    DATA lr_ref TYPE REF TO data.
    FIELD-SYMBOLS <val> TYPE any.

    lr_ref = lo_cut->find_loc( 'str' ). " Relative also works but from root
    ASSIGN lr_ref->* TO <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'Hello' ).

    lr_ref = lo_cut->find_loc( '/str' ).
    ASSIGN lr_ref->* TO <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'Hello' ).

    lr_ref = lo_cut->find_loc( '/int' ).
    ASSIGN lr_ref->* TO <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 10 ).

    lr_ref = lo_cut->find_loc( '/obj/a' ).
    ASSIGN lr_ref->* TO <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'World' ).

    lr_ref = lo_cut->find_loc( iv_path = '/obj'
                               iv_name = 'a' ).
    ASSIGN lr_ref->* TO <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'World' ).

    lr_ref = lo_cut->find_loc( '/obj' ).
    ASSIGN lr_ref->* TO <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = ls_mock-obj ).

    lr_ref = lo_cut->find_loc( '/' ).
    ASSIGN lr_ref->* TO <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = ls_mock ).

    lr_ref = lo_cut->find_loc( '/tab/2' ).
    ASSIGN lr_ref->* TO <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = last_elem ).

    lr_ref = lo_cut->find_loc( '/tab/1/a' ).
    ASSIGN lr_ref->* TO <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'One' ).

  ENDMETHOD.

  METHOD find_loc_append.

    DATA last_elem TYPE ty_struc.
    DATA ls_mock TYPE ty_complex.
    DATA lo_cut TYPE REF TO lcl_json_to_abap.
    DATA lx TYPE REF TO zcx_abapgit_ajson_error.

    prepare_cut(
      IMPORTING
        eo_cut = lo_cut
        e_mock = ls_mock
        e_elem = last_elem ).

    DATA lr_ref TYPE REF TO data.
    FIELD-SYMBOLS <val> TYPE any.

    lr_ref = lo_cut->find_loc( '/tab/1/a' ).
    ASSIGN lr_ref->* TO <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = 'One' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_mock-tab )
      exp = 2 ).

    TRY.
        lo_cut->find_loc( '/tab/3/a' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Index not found in table' ).
    ENDTRY.

    lr_ref = lo_cut->find_loc( iv_path = '/tab/3/a'
                               iv_append_tables = abap_true ).
    ASSIGN lr_ref->* TO <val>.
    cl_abap_unit_assert=>assert_equals(
      act = <val>
      exp = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_mock-tab )
      exp = 3 ).

    TRY.
        lo_cut->find_loc( '/tab/5/a' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Index not found in table' ).
    ENDTRY.

  ENDMETHOD.

  METHOD find_loc_negative.

    DATA lo_cut TYPE REF TO lcl_json_to_abap.
    DATA lx TYPE REF TO zcx_abapgit_ajson_error.
    DATA ls_mock TYPE ty_complex.

    prepare_cut(
      IMPORTING
        e_mock = ls_mock " Must be here to keep reference alive
        eo_cut = lo_cut ).

    TRY.
        lo_cut->find_loc( '/xyz' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Path not found' ).
    ENDTRY.

    TRY.
        lo_cut->find_loc( '/oref/xyz' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Cannot assign to ref' ).
    ENDTRY.

    TRY.
        lo_cut->find_loc( '/tab/xyz' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Need index to access tables' ).
    ENDTRY.

    TRY.
        lo_cut->find_loc( '/tab/5' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Index not found in table' ).
    ENDTRY.

  ENDMETHOD.

  METHOD to_abap.

    DATA lo_cut TYPE REF TO lcl_json_to_abap.
    DATA ls_mock TYPE ty_complex.
    DATA lv_exp_date TYPE d VALUE '20200728'.
    DATA lv_exp_timestamp TYPE timestamp VALUE '20200728000000'.
    lcl_json_to_abap=>bind(
      CHANGING
        c_obj = ls_mock
        co_instance = lo_cut ).

    DATA lo_nodes TYPE REF TO lcl_nodes_helper.
    CREATE OBJECT lo_nodes.
    lo_nodes->add( '/      |           |object |                          | ' ).
    lo_nodes->add( '/      |str        |str    |hello                     | ' ).
    lo_nodes->add( '/      |int        |num    |5                         | ' ).
    lo_nodes->add( '/      |float      |num    |5.5                       | ' ).
    lo_nodes->add( '/      |bool       |bool   |true                      | ' ).
    lo_nodes->add( '/      |obj        |object |                          | ' ).
    lo_nodes->add( '/obj   |a          |str    |world                     | ' ).
    lo_nodes->add( '/      |tab        |array  |                          | ' ).
    lo_nodes->add( '/tab   |1          |object |                          |1' ).
    lo_nodes->add( '/tab/1 |a          |str    | One                      | ' ).
    lo_nodes->add( '/tab   |2          |object |                          |2' ).
    lo_nodes->add( '/tab/2 |a          |str    | Two                      | ' ).
    lo_nodes->add( '/      |date1      |str    |2020-07-28                | ' ).
    lo_nodes->add( '/      |date2      |str    |2020-07-28T00:00:00Z      | ' ).
    lo_nodes->add( '/      |timestamp1 |str    |2020-07-28T00:00:00       | ' ).
    lo_nodes->add( '/      |timestamp2 |str    |2020-07-28T00:00:00Z      | ' ).
    lo_nodes->add( '/      |timestamp3 |str    |2020-07-28T01:00:00+01:00 | ' ).

    lo_cut->to_abap( lo_nodes->sorted( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-str
      exp = 'hello' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-int
      exp = 5 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-float
      exp = '5.5' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-bool
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-obj-a
      exp = 'world' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-date1
      exp = lv_exp_date ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-date2
      exp = lv_exp_date ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-timestamp1
      exp = lv_exp_timestamp ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-timestamp2
      exp = lv_exp_timestamp ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mock-timestamp3
      exp = lv_exp_timestamp ).

    DATA ls_elem LIKE LINE OF ls_mock-tab.
    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_mock-tab )
      exp = 2 ).

    READ TABLE ls_mock-tab INTO ls_elem INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_elem-a
      exp = 'One' ).
    READ TABLE ls_mock-tab INTO ls_elem INDEX 2.
    cl_abap_unit_assert=>assert_equals(
      act = ls_elem-a
      exp = 'Two' ).

  ENDMETHOD.

  METHOD to_abap_negative.

    DATA lo_cut TYPE REF TO lcl_json_to_abap.
    DATA lx TYPE REF TO zcx_abapgit_ajson_error.
    DATA ls_mock TYPE ty_complex.
    lcl_json_to_abap=>bind(
      CHANGING
        c_obj = ls_mock
        co_instance = lo_cut ).

    DATA lo_nodes TYPE REF TO lcl_nodes_helper.

    TRY.
        CREATE OBJECT lo_nodes.
        lo_nodes->add( '/    |      |object | ' ).
        lo_nodes->add( '/    |str   |object | ' ).

        lo_cut->to_abap( lo_nodes->sorted( ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Expected structure' ).
    ENDTRY.

    TRY.
        CREATE OBJECT lo_nodes.
        lo_nodes->add( '/    |      |object | ' ).
        lo_nodes->add( '/    |str   |array  | ' ).

        lo_cut->to_abap( lo_nodes->sorted( ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Expected table' ).
    ENDTRY.

    TRY.
        CREATE OBJECT lo_nodes.
        lo_nodes->add( '/    |      |object |      ' ).
        lo_nodes->add( '/    |int   |str    |hello ' ).

        lo_cut->to_abap( lo_nodes->sorted( ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Source is not a number' ).
    ENDTRY.

    TRY.
        CREATE OBJECT lo_nodes.
        lo_nodes->add( '/    |      |object |        ' ).
        lo_nodes->add( '/    |date1 |str    |baddate ' ).

        lo_cut->to_abap( lo_nodes->sorted( ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Unexpected date format' ).
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* WRITER
**********************************************************************

CLASS ltcl_writer_test DEFINITION FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.

    METHODS set_ajson FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS set_value FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS ignore_empty FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS set_obj FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS set_tab FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS prove_path_exists FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS delete_subtree FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS delete FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS arrays FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS arrays_negative FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS root_assignment FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS set_bool FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS set_str FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS set_int FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS set_date FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS read_only FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS set_array_obj FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS set_with_type FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS set_with_type_slice
      IMPORTING
        io_json_in TYPE REF TO zcl_abapgit_ajson
        io_json_out TYPE REF TO zif_abapgit_ajson_writer
        iv_path TYPE string
      RAISING
        zcx_abapgit_ajson_error.

ENDCLASS.

CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS ltcl_writer_test.

CLASS ltcl_writer_test IMPLEMENTATION.

  METHOD prove_path_exists.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.

    lo_cut = zcl_abapgit_ajson=>create_empty( ).

    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |a     |object |     ||1' ).
    lo_nodes_exp->add( '/a/     |b     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/   |c     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/c/ |d     |object |     ||0' ).

    lo_cut->prove_path_exists( '/a/b/c/d/' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '         |      |object |     ||1' ).
    lo_nodes_exp->add( '/        |a     |object |     ||1' ).
    lo_nodes_exp->add( '/a/      |b     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/    |c     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/c/  |d     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/c/d |e     |object |     ||0' ).
    lo_cut->prove_path_exists( '/a/b/c/d/e/' ).

  ENDMETHOD.

  METHOD delete_subtree.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.

    lo_cut = zcl_abapgit_ajson=>create_empty( ).

    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |a     |object |     ||1' ).
    lo_nodes_exp->add( '/a/     |b     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/   |c     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/c/ |d     |object |     ||0' ).

    lo_cut->mt_json_tree = lo_nodes_exp->mt_nodes.

    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |a     |object |     ||0' ).

    lo_cut->delete_subtree(
      iv_path = '/a/'
      iv_name = 'b' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  ENDMETHOD.

  METHOD delete.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.

    lo_cut = zcl_abapgit_ajson=>create_empty( ).

    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |a     |object |     ||1' ).
    lo_nodes_exp->add( '/a/     |b     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/   |c     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/c/ |d     |object |     ||0' ).

    lo_cut->mt_json_tree = lo_nodes_exp->mt_nodes.

    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |a     |object |     ||0' ).

    lo_cut->zif_abapgit_ajson_writer~delete( iv_path = '/a/b' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |a     |object |     ||1' ).
    lo_nodes_exp->add( '/a/     |b     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/   |c     |object |     ||1' ).
    lo_nodes_exp->add( '/a/b/c/ |d     |object |     ||0' ).

    lo_cut->mt_json_tree = lo_nodes_exp->mt_nodes.

    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |a     |object |     ||0' ).

    lo_cut->zif_abapgit_ajson_writer~delete( iv_path = '/a/b/' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  ENDMETHOD.

  METHOD set_ajson.

    DATA lo_nodes TYPE REF TO lcl_nodes_helper.
    DATA lo_src TYPE REF TO zcl_abapgit_ajson.
    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.

    lo_src = zcl_abapgit_ajson=>create_empty( ).
    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.

    " Prepare source
    CREATE OBJECT lo_nodes.
    lo_nodes->add( '        |      |object |     ||1' ).
    lo_nodes->add( '/       |x     |object |     ||2' ).
    lo_nodes->add( '/x/     |b     |str    |abc  ||0' ).
    lo_nodes->add( '/x/     |c     |num    |10   ||0' ).
    lo_src->mt_json_tree = lo_nodes->mt_nodes.

    " Test 1 - assign root
    li_writer->set(
      iv_path = ''
      iv_val  = lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

    li_writer->set(
      iv_path = '/'
      iv_val  = lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

    " Test 2 - assign deep
    CREATE OBJECT lo_nodes.
    lo_nodes->add( '        |      |object |     ||1' ).
    lo_nodes->add( '/       |a     |object |     ||1' ).
    lo_nodes->add( '/a/     |b     |object |     ||1' ).
    lo_nodes->add( '/a/b/     |c     |object |     ||1' ).
    lo_nodes->add( '/a/b/c/   |x     |object |     ||2' ).
    lo_nodes->add( '/a/b/c/x/ |b     |str    |abc  ||0' ).
    lo_nodes->add( '/a/b/c/x/ |c     |num    |10   ||0' ).

    li_writer->clear( ).
    li_writer->set(
      iv_path = '/a/b/c'
      iv_val  = lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

    " Test 3 - assign rewrite
    CREATE OBJECT lo_nodes.
    lo_nodes->add( '        |      |object |     ||1' ).
    lo_nodes->add( '/       |a     |object |     ||1' ).
    lo_nodes->add( '/a/       |b     |object |     ||1' ).
    lo_nodes->add( '/a/b/     |x     |object |     ||2' ).
    lo_nodes->add( '/a/b/x/   |b     |str    |abc  ||0' ).
    lo_nodes->add( '/a/b/x/   |c     |num    |10   ||0' ).

    li_writer->set(
      iv_path = '/a/b'
      iv_val  = lo_src ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

  ENDMETHOD.

  METHOD set_value.

    DATA lo_nodes TYPE REF TO lcl_nodes_helper.
    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.

    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.

    " Prepare source
    CREATE OBJECT lo_nodes.
    lo_nodes->add( '        |      |object |     ||1' ).
    lo_nodes->add( '/       |x     |object |     ||2' ).
    lo_nodes->add( '/x/     |b     |str    |abc  ||0' ).
    lo_nodes->add( '/x/     |c     |num    |10   ||0' ).

    li_writer->set(
      iv_path = '/x/b'
      iv_val  = 'abc' ).
    li_writer->set(
      iv_path = '/x/c'
      iv_val  = 10 ).
    li_writer->set( " ignore empty
      iv_path = '/x/d'
      iv_val  = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

  ENDMETHOD.

  METHOD ignore_empty.

    DATA lo_nodes TYPE REF TO lcl_nodes_helper.
    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.

    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '        |      |object |     ||1' ).
    lo_nodes->add( '/       |a     |num    |1    ||0' ).

    li_writer->set(
      iv_path = '/a'
      iv_val  = 1 ).
    li_writer->set( " ignore empty
      iv_path = '/b'
      iv_val  = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '        |      |object |     ||2' ).
    lo_nodes->add( '/       |a     |num    |1    ||0' ).
    lo_nodes->add( '/       |b     |num    |0    ||0' ).

    li_writer->set(
      iv_ignore_empty = abap_false
      iv_path = '/b'
      iv_val  = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

  ENDMETHOD.

  METHOD set_obj.

    DATA lo_nodes TYPE REF TO lcl_nodes_helper.
    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.

    DATA:
      BEGIN OF ls_struc,
        b TYPE string VALUE 'abc',
        c TYPE i VALUE 10,
      END OF ls_struc.

    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.

    " Prepare source
    CREATE OBJECT lo_nodes.
    lo_nodes->add( '        |      |object |     ||1' ).
    lo_nodes->add( '/       |x     |object |     ||2' ).
    lo_nodes->add( '/x/     |b     |str    |abc  ||0' ).
    lo_nodes->add( '/x/     |c     |num    |10   ||0' ).

    li_writer->set(
      iv_path = '/x'
      iv_val  = ls_struc ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

  ENDMETHOD.

  METHOD set_tab.

    DATA lo_nodes TYPE REF TO lcl_nodes_helper.
    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.
    DATA lt_tab TYPE string_table.

    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.

    APPEND 'hello' TO lt_tab.
    APPEND 'world' TO lt_tab.

    " Prepare source
    CREATE OBJECT lo_nodes.
    lo_nodes->add( '        |      |object |     | |1' ).
    lo_nodes->add( '/       |x     |array  |     | |2' ).
    lo_nodes->add( '/x/     |1     |str    |hello|1|0' ).
    lo_nodes->add( '/x/     |2     |str    |world|2|0' ).

    li_writer->set(
      iv_path = '/x'
      iv_val  = lt_tab ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes->sorted( ) ).

  ENDMETHOD.

  METHOD arrays.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.

    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.

    " touch
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     | |1' ).
    lo_nodes_exp->add( '/       |a     |array  |     | |0' ).

    li_writer->touch_array( iv_path = '/a' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " add string
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     | |1' ).
    lo_nodes_exp->add( '/       |a     |array  |     | |1' ).
    lo_nodes_exp->add( '/a/     |1     |str    |hello|1|0' ).

    li_writer->push(
      iv_path = '/a'
      iv_val  = 'hello' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " add obj
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     | |1' ).
    lo_nodes_exp->add( '/       |a     |array  |     | |2' ).
    lo_nodes_exp->add( '/a/     |1     |str    |hello|1|0' ).
    lo_nodes_exp->add( '/a/     |2     |object |     |2|1' ).
    lo_nodes_exp->add( '/a/2/   |x     |str    |world| |0' ).

    DATA:
      BEGIN OF ls_dummy,
        x TYPE string VALUE 'world',
      END OF ls_dummy.

    li_writer->push(
      iv_path = '/a'
      iv_val  = ls_dummy ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " re-touch
    li_writer->touch_array( iv_path = '/a' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " re-touch with clear
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     | |1' ).
    lo_nodes_exp->add( '/       |a     |array  |     | |0' ).

    li_writer->touch_array(
      iv_path = '/a'
      iv_clear = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " free-add array item (index must be updated)
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     | |1' ).
    lo_nodes_exp->add( '/       |a     |array  |     | |2' ).
    lo_nodes_exp->add( '/a/     |1     |object |     |1|1' ).
    lo_nodes_exp->add( '/a/1/   |x     |num    |123  | |0' ).
    lo_nodes_exp->add( '/a/     |2     |num    |234  |2|0' ).

    li_writer->set(
      iv_path = '/a/1/x'
      iv_val  = 123 ).
    li_writer->set(
      iv_path = '/a/2'
      iv_val  = 234 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  ENDMETHOD.

  METHOD arrays_negative.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.

    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.

    li_writer->touch_array( iv_path = '/a' ).
    li_writer->push(
      iv_path = '/a'
      iv_val = 123 ).

    " touch another node
    DATA lx TYPE REF TO zcx_abapgit_ajson_error.
    TRY.
        li_writer->touch_array( iv_path = '/a/1' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Path [/a/1] already used and is not array' ).
    ENDTRY.

    " push to not array
    TRY.
        li_writer->push(
        iv_path = '/a/1'
        iv_val  = 123 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Path [/a/1] is not array' ).
    ENDTRY.

    " push to not array
    TRY.
        li_writer->push(
        iv_path = '/x'
        iv_val  = 123 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Path [/x] does not exist' ).
    ENDTRY.

    " set array item with non-numeric key
    TRY.
        li_writer->set(
        iv_path = '/a/abc/x'
        iv_val  = 123 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Cannot add non-numeric key [abc] to array [/a/]' ).
    ENDTRY.

    TRY.
        li_writer->set(
        iv_path = '/a/abc'
        iv_val  = 123 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Cannot add non-numeric key [abc] to array [/a/]' ).
    ENDTRY.

    " set array item with zero key
    TRY.
        li_writer->set(
        iv_path = '/a/0'
        iv_val  = 123 ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lx.
        cl_abap_unit_assert=>assert_equals(
        act = lx->message
        exp = 'Cannot add zero key to array [/a/]' ).
    ENDTRY.

  ENDMETHOD.


  METHOD root_assignment.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.
    DATA:
      BEGIN OF ls_dummy,
        x TYPE string VALUE 'hello',
      END OF ls_dummy.

    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.

    " object
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |x     |str    |hello||0' ).

    li_writer->set(
      iv_path = '/'
      iv_val  = ls_dummy ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " object empty path
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |     ||1' ).
    lo_nodes_exp->add( '/       |x     |str    |hello||0' ).

    li_writer->clear( ).
    li_writer->set(
      iv_path = ''
      iv_val  = ls_dummy ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " array
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |array  |     | |1' ).
    lo_nodes_exp->add( '/       |1     |str    |hello|1|0' ).

    li_writer->clear( ).
    li_writer->touch_array( iv_path = '' ).
    li_writer->push(
      iv_path = ''
      iv_val  = 'hello' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " value
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |str    |hello||0' ).

    li_writer->clear( ).
    li_writer->set(
      iv_path = ''
      iv_val  = 'hello' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  ENDMETHOD.

  METHOD set_bool.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.
    DATA lt_tab TYPE string_table.

    " abap_bool
    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |      ||2' ).
    lo_nodes_exp->add( '/       |a     |bool   |true  ||0' ).
    lo_nodes_exp->add( '/       |b     |bool   |false ||0' ).

    li_writer->set_boolean(
      iv_path = '/a'
      iv_val  = abap_true ).
    li_writer->set_boolean(
      iv_path = '/b'
      iv_val  = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " int
    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |      ||2' ).
    lo_nodes_exp->add( '/       |a     |bool   |true  ||0' ).
    lo_nodes_exp->add( '/       |b     |bool   |false ||0' ).

    li_writer->set_boolean(
      iv_path = '/a'
      iv_val  = 1 ).
    li_writer->set_boolean(
      iv_path = '/b'
      iv_val  = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

    " tab
    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |      ||2' ).
    lo_nodes_exp->add( '/       |a     |bool   |true  ||0' ).
    lo_nodes_exp->add( '/       |b     |bool   |false ||0' ).

    APPEND 'hello' TO lt_tab.
    li_writer->set_boolean(
      iv_path = '/a'
      iv_val  = lt_tab ).
    CLEAR lt_tab.
    li_writer->set_boolean(
      iv_path = '/b'
      iv_val  = lt_tab ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  ENDMETHOD.

  METHOD set_str.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.
    DATA lv_date TYPE d.

    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |         ||3' ).
    lo_nodes_exp->add( '/       |a     |str    |123      ||0' ).
    lo_nodes_exp->add( '/       |b     |str    |X        ||0' ).
    lo_nodes_exp->add( '/       |c     |str    |20200705 ||0' ).

    li_writer->set_string(
      iv_path = '/a'
      iv_val  = '123' ).
    li_writer->set_string(
      iv_path = '/b'
      iv_val  = abap_true ).
    lv_date = '20200705'.
    li_writer->set_string(
      iv_path = '/c'
      iv_val  = lv_date ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  ENDMETHOD.

  METHOD set_int.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.

    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |         ||1' ).
    lo_nodes_exp->add( '/       |a     |num    |123      ||0' ).

    li_writer->set_integer(
      iv_path = '/a'
      iv_val  = 123 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  ENDMETHOD.

  METHOD set_date.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.
    DATA lv_date TYPE d.

    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |object |           ||1' ).
    lo_nodes_exp->add( '/       |a     |str    |2020-07-05 ||0' ).

    lv_date = '20200705'.
    li_writer->set_date(
      iv_path = '/a'
      iv_val  = lv_date ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  ENDMETHOD.

  METHOD read_only.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.

    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.

    " Prepare source
    li_writer->set(
      iv_path = '/a'
      iv_val  = 'abc' ).
    li_writer->touch_array( iv_path = '/b' ).
    li_writer->push(
      iv_path = '/b'
      iv_val  = 'abc' ).

    lo_cut->freeze( ).

    TRY.
        li_writer->set(
        iv_path = '/c'
        iv_val  = 'abc' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error.
    ENDTRY.

    TRY.
        li_writer->touch_array( iv_path = '/d' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error.
    ENDTRY.

    TRY.
        li_writer->push(
        iv_path = '/b'
        iv_val  = 'xyz' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error.
    ENDTRY.

    TRY.
        li_writer->delete( iv_path = '/a' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error.
    ENDTRY.

    TRY.
        li_writer->clear( ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error.
    ENDTRY.

  ENDMETHOD.

  METHOD set_array_obj.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.

    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '                 |         |object |                        |  |1' ).
    lo_nodes_exp->add( '/                |issues   |array  |                        |  |2' ).
    lo_nodes_exp->add( '/issues/         |1        |object |                        |1 |1' ).
    lo_nodes_exp->add( '/issues/         |2        |object |                        |2 |1' ).
    lo_nodes_exp->add( '/issues/1/       |end      |object |                        |  |2' ).
    lo_nodes_exp->add( '/issues/1/end/   |col      |num    |26                      |  |0' ).
    lo_nodes_exp->add( '/issues/1/end/   |row      |num    |4                       |  |0' ).
    lo_nodes_exp->add( '/issues/2/       |end      |object |                        |  |2' ).
    lo_nodes_exp->add( '/issues/2/end/   |col      |num    |22                      |  |0' ).
    lo_nodes_exp->add( '/issues/2/end/   |row      |num    |3                       |  |0' ).

    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.

    li_writer->touch_array( iv_path = '/issues' ).
    li_writer->set(
      iv_path = '/issues/1/end/col'
      iv_val  = 26 ).
    li_writer->set(
      iv_path = '/issues/1/end/row'
      iv_val  = 4 ).
    li_writer->set(
      iv_path = '/issues/2/end/col'
      iv_val  = 22 ).
    li_writer->set(
      iv_path = '/issues/2/end/row'
      iv_val  = 3 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_nodes_exp->sorted( ) ).

  ENDMETHOD.

  METHOD set_with_type.

    DATA lo_sample TYPE REF TO zcl_abapgit_ajson.
    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.

    lo_sample = zcl_abapgit_ajson=>parse( ltcl_parser_test=>sample_json( ) ).

    lo_cut = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.

    set_with_type_slice( io_json_in  = lo_sample
                         io_json_out = li_writer
                         iv_path     = '/' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->mt_json_tree
      exp = lo_sample->mt_json_tree ).

  ENDMETHOD.

  METHOD set_with_type_slice.

    DATA lv_path TYPE string.

    FIELD-SYMBOLS <node> TYPE zif_abapgit_ajson=>ty_node.

    LOOP AT io_json_in->mt_json_tree ASSIGNING <node> WHERE path = iv_path.
      lv_path = <node>-path && <node>-name && '/'.
      CASE <node>-type.
        WHEN 'array'.
          io_json_out->touch_array( lv_path ).
          set_with_type_slice( io_json_in  = io_json_in
                               io_json_out = io_json_out
                               iv_path     = lv_path ).
        WHEN 'object'.
          set_with_type_slice( io_json_in  = io_json_in
                               io_json_out = io_json_out
                               iv_path     = lv_path ).
        WHEN OTHERS.
          io_json_out->set(
            iv_path      = lv_path
            iv_val       = <node>-value
            iv_node_type = <node>-type ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.


**********************************************************************
* INTEGRATED
**********************************************************************
CLASS ltcl_integrated DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_loc,
        row TYPE i,
        col TYPE i,
      END OF ty_loc,
      BEGIN OF ty_issue,
        message TYPE string,
        key TYPE string,
        filename TYPE string,
        start TYPE ty_loc,
        end TYPE ty_loc,
      END OF ty_issue,
      tt_issues TYPE STANDARD TABLE OF ty_issue WITH DEFAULT KEY,
      BEGIN OF ty_target,
        string TYPE string,
        number TYPE i,
        float TYPE f,
        boolean TYPE abap_bool,
        false TYPE abap_bool,
        null TYPE string,
        date TYPE string, " ??? TODO
        issues TYPE tt_issues,
      END OF ty_target.

    METHODS reader FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS array_index FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS array_simple FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS stringify FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS item_order_integrated FOR TESTING RAISING zcx_abapgit_ajson_error.

ENDCLASS.

CLASS ltcl_integrated IMPLEMENTATION.

  METHOD array_simple.

    DATA lt_act TYPE string_table.
    DATA lt_exp TYPE string_table.
    DATA lv_exp TYPE string.

    DATA lv_src TYPE string.
    lv_src = '['.
    DO 10 TIMES.
      IF sy-index <> 1.
        lv_src = lv_src && `, `.
      ENDIF.
      lv_src = lv_src && |"{ sy-index }"|.
      lv_exp = |{ sy-index }|.
      APPEND lv_exp TO lt_exp.
    ENDDO.
    lv_src = lv_src && ']'.

    DATA li_reader TYPE REF TO zif_abapgit_ajson_reader.
    li_reader = zcl_abapgit_ajson=>parse( lv_src ).
    li_reader->to_abap( IMPORTING ev_container = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  ENDMETHOD.

  METHOD array_index.

    DATA lt_act TYPE TABLE OF ty_loc.
    DATA lt_exp TYPE TABLE OF ty_loc.
    DATA ls_exp TYPE ty_loc.

    DATA lv_src TYPE string.
    lv_src = '['.
    DO 10 TIMES.
      IF sy-index <> 1.
        lv_src = lv_src && `, `.
      ENDIF.
      lv_src = lv_src && |\{ "row": { sy-index } \}|.
      ls_exp-row = sy-index.
      APPEND ls_exp TO lt_exp.
    ENDDO.
    lv_src = lv_src && ']'.

    DATA li_reader TYPE REF TO zif_abapgit_ajson_reader.
    li_reader = zcl_abapgit_ajson=>parse( lv_src ).
    li_reader->to_abap( IMPORTING ev_container = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  ENDMETHOD.

  METHOD reader.

    DATA lv_source TYPE string.
    DATA li_reader TYPE REF TO zif_abapgit_ajson_reader.

    lv_source = ltcl_parser_test=>sample_json( ).
    li_reader = zcl_abapgit_ajson=>parse( lv_source ).

    cl_abap_unit_assert=>assert_equals(
      act = li_reader->get( '/string' )
      exp = 'abc' ).

    DATA ls_act TYPE ty_target.
    DATA ls_exp TYPE ty_target.
    FIELD-SYMBOLS <i> LIKE LINE OF ls_exp-issues.

    ls_exp-string = 'abc'.
    ls_exp-number = 123.
    ls_exp-float = '123.45'.
    ls_exp-boolean = abap_true.
    ls_exp-false = abap_false.
    ls_exp-date = '2020-03-15'.

    APPEND INITIAL LINE TO ls_exp-issues ASSIGNING <i>.
    <i>-message  = 'Indentation problem ...'.
    <i>-key      = 'indentation'.
    <i>-filename = './zxxx.prog.abap'.
    <i>-start-row = 4.
    <i>-start-col = 3.
    <i>-end-row   = 4.
    <i>-end-col   = 26.

    APPEND INITIAL LINE TO ls_exp-issues ASSIGNING <i>.
    <i>-message  = 'Remove space before XXX'.
    <i>-key      = 'space_before_dot'.
    <i>-filename = './zxxx.prog.abap'.
    <i>-start-row = 3.
    <i>-start-col = 21.
    <i>-end-row   = 3.
    <i>-end-col   = 22.

    li_reader->to_abap( IMPORTING ev_container = ls_act ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_act
      exp = ls_exp ).

  ENDMETHOD.

  METHOD stringify.

    DATA lo_cut TYPE REF TO zcl_abapgit_ajson.
    DATA li_writer TYPE REF TO zif_abapgit_ajson_writer.
    DATA lv_exp TYPE string.
    DATA: BEGIN OF ls_dummy, x TYPE i, END OF ls_dummy.

    ls_dummy-x = 1.
    lo_cut    = zcl_abapgit_ajson=>create_empty( ).
    li_writer = lo_cut.

    li_writer->set(
      iv_path = '/a'
      iv_val  = 1 ).
    li_writer->set(
      iv_path = '/b'
      iv_val  = 'B' ).
    li_writer->set(
      iv_path = '/c'
      iv_val  = abap_true ).
    li_writer->set_null( iv_path = '/d' ).

    " simple test
    lv_exp = '{"a":1,"b":"B","c":true,"d":null}'.
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->stringify( )
      exp = lv_exp ).

    li_writer->touch_array( iv_path = '/e' ).
    li_writer->touch_array( iv_path = '/f' ).
    li_writer->push(
      iv_path = '/f'
      iv_val  = 5 ).
    li_writer->push(
      iv_path = '/f'
      iv_val  = ls_dummy ).
    li_writer->set(
      iv_path = '/g'
      iv_val  = ls_dummy ).

    " complex test
    lv_exp = '{"a":1,"b":"B","c":true,"d":null,"e":[],"f":[5,{"x":1}],"g":{"x":1}}'.
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->stringify( )
      exp = lv_exp ).

    " complex test indented
    lv_exp =
      '{\n' &&
      '  "a": 1,\n' &&
      '  "b": "B",\n' &&
      '  "c": true,\n' &&
      '  "d": null,\n' &&
      '  "e": [],\n' &&
      '  "f": [\n' &&
      '    5,\n' &&
      '    {\n' &&
      '      "x": 1\n' &&
      '    }\n' &&
      '  ],\n' &&
      '  "g": {\n' &&
      '    "x": 1\n' &&
      '  }\n' &&
      '}'.
    lv_exp = replace(
      val = lv_exp
      sub = '\n'
      with = cl_abap_char_utilities=>newline
      occ = 0 ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_cut->stringify( iv_indent = 2 )
      exp = lv_exp ).

  ENDMETHOD.

  METHOD item_order_integrated.

    DATA:
      BEGIN OF ls_dummy,
        zulu TYPE string,
        alpha TYPE string,
        beta TYPE string,
      END OF ls_dummy.

    DATA lv_act TYPE string.
    DATA lv_exp TYPE string.
    DATA li_cut TYPE REF TO zif_abapgit_ajson.

    ls_dummy-alpha = 'a'.
    ls_dummy-beta  = 'b'.
    ls_dummy-zulu  = 'z'.

    " NAME order
    li_cut = zcl_abapgit_ajson=>create_empty( ).
    li_cut->set(
      iv_path = '/'
      iv_val  = ls_dummy ).

    lv_act = li_cut->stringify( ).
    lv_exp = '{"alpha":"a","beta":"b","zulu":"z"}'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

    " STRUC order (keep)
    li_cut = zcl_abapgit_ajson=>create_empty( ).
    li_cut->keep_item_order( ).
    li_cut->set(
      iv_path = '/'
      iv_val  = ls_dummy ).

    lv_act = li_cut->stringify( ).
    lv_exp = '{"zulu":"z","alpha":"a","beta":"b"}'.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_exp ).

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* ABAP TO JSON
**********************************************************************
CLASS ltcl_abap_to_json DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_struc,
        a TYPE string,
        b TYPE i,
        c TYPE abap_bool,
        d TYPE xfeld,
      END OF ty_struc,
      tt_struc TYPE STANDARD TABLE OF ty_struc WITH DEFAULT KEY,
      BEGIN OF ty_struc_complex.
        INCLUDE TYPE ty_struc.
    TYPES:
        el TYPE string,
        struc TYPE ty_struc,
        tab TYPE tt_struc,
        stab TYPE string_table,
      END OF ty_struc_complex.

    METHODS set_ajson FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS set_value FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS set_null FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS set_obj FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS set_array FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS set_complex_obj FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS prefix FOR TESTING RAISING zcx_abapgit_ajson_error.

ENDCLASS.

CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS ltcl_abap_to_json.

CLASS ltcl_abap_to_json IMPLEMENTATION.

  METHOD set_ajson.

    DATA lo_nodes TYPE REF TO lcl_nodes_helper.
    DATA lo_src TYPE REF TO zcl_abapgit_ajson.
    lo_src = zcl_abapgit_ajson=>create_empty( ).

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '        |      |object |     ||1' ).
    lo_nodes->add( '/       |a     |object |     ||1' ).
    lo_nodes->add( '/a/     |b     |object |     ||1' ).
    lo_nodes->add( '/a/b/   |c     |object |     ||0' ).
    lo_src->mt_json_tree = lo_nodes->mt_nodes.

    DATA lt_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt.
    lt_nodes = lcl_abap_to_json=>convert( iv_data = lo_src ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes->mt_nodes ).

  ENDMETHOD.

  METHOD set_value.

    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.
    DATA lt_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt.

    " number
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |num |1     ||' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

    " string
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |str |abc     ||' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = 'abc' ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

    " true
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |bool |true     ||' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = abap_true ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

    " false
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |bool |false    ||' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = abap_false ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

    " xfeld
    DATA lv_xfeld TYPE xfeld.
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '        |      |bool |true     ||' ).

    lv_xfeld = 'X'.
    lt_nodes = lcl_abap_to_json=>convert( iv_data = lv_xfeld ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

  ENDMETHOD.

  METHOD set_null.

    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.
    DATA lt_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt.
    DATA lv_null_ref TYPE REF TO data.

    " null
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '       |      |null |null ||' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = lv_null_ref ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

  ENDMETHOD.

  METHOD prefix.

    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.
    DATA lt_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt.
    DATA ls_prefix TYPE zif_abapgit_ajson=>ty_path_name.

    ls_prefix-path = '/a/'.
    ls_prefix-name = 'b'.
    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '/a/       |b     |num |1     ||' ).

    lt_nodes = lcl_abap_to_json=>convert(
      iv_data   = 1
      is_prefix = ls_prefix ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

  ENDMETHOD.

  METHOD set_obj.

    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.
    DATA ls_struc TYPE ty_struc.
    DATA lt_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt.

    ls_struc-a = 'abc'.
    ls_struc-b = 10.
    ls_struc-c = abap_true.
    ls_struc-d = 'X'.

    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '       |      |object |     ||4' ).
    lo_nodes_exp->add( '/      |a     |str    |abc  ||0' ).
    lo_nodes_exp->add( '/      |b     |num    |10   ||0' ).
    lo_nodes_exp->add( '/      |c     |bool   |true ||0' ).
    lo_nodes_exp->add( '/      |d     |bool   |true ||0' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = ls_struc ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

  ENDMETHOD.

  METHOD set_complex_obj.

    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.
    DATA ls_struc TYPE ty_struc_complex.
    DATA lt_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt.
    FIELD-SYMBOLS <i> LIKE LINE OF ls_struc-tab.

    ls_struc-a = 'abc'.
    ls_struc-b = 10.
    ls_struc-c = abap_true.
    ls_struc-d = 'X'.
    ls_struc-el = 'elem'.

    ls_struc-struc-a = 'deep'.
    ls_struc-struc-b = 123.

    APPEND 'hello' TO ls_struc-stab.
    APPEND 'world' TO ls_struc-stab.

    APPEND INITIAL LINE TO ls_struc-tab ASSIGNING <i>.
    <i>-a = 'abc'.
    APPEND INITIAL LINE TO ls_struc-tab ASSIGNING <i>.
    <i>-a = 'bcd'.

    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '       |      |object |     ||8' ).
    lo_nodes_exp->add( '/      |a     |str    |abc  ||0' ).
    lo_nodes_exp->add( '/      |b     |num    |10   ||0' ).
    lo_nodes_exp->add( '/      |c     |bool   |true ||0' ).
    lo_nodes_exp->add( '/      |d     |bool   |true ||0' ).
    lo_nodes_exp->add( '/      |el    |str    |elem ||0' ).
    lo_nodes_exp->add( '/      |struc |object |     ||4' ).
    lo_nodes_exp->add( '/struc/|a     |str    |deep ||0' ).
    lo_nodes_exp->add( '/struc/|b     |num    |123  ||0' ).
    lo_nodes_exp->add( '/struc/|c     |bool   |false||0' ).
    lo_nodes_exp->add( '/struc/|d     |bool   |false||0' ).

    lo_nodes_exp->add( '/      |tab   |array  |     | |2' ).
    lo_nodes_exp->add( '/tab/  |1     |object |     |1|4' ).
    lo_nodes_exp->add( '/tab/1/|a     |str    |abc  | |0' ).
    lo_nodes_exp->add( '/tab/1/|b     |num    |0    | |0' ).
    lo_nodes_exp->add( '/tab/1/|c     |bool   |false| |0' ).
    lo_nodes_exp->add( '/tab/1/|d     |bool   |false| |0' ).
    lo_nodes_exp->add( '/tab/  |2     |object |     |2|4' ).
    lo_nodes_exp->add( '/tab/2/|a     |str    |bcd  | |0' ).
    lo_nodes_exp->add( '/tab/2/|b     |num    |0    | |0' ).
    lo_nodes_exp->add( '/tab/2/|c     |bool   |false| |0' ).
    lo_nodes_exp->add( '/tab/2/|d     |bool   |false| |0' ).

    lo_nodes_exp->add( '/      |stab  |array  |     | |2' ).
    lo_nodes_exp->add( '/stab/ |1     |str    |hello|1|0' ).
    lo_nodes_exp->add( '/stab/ |2     |str    |world|2|0' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = ls_struc ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

  ENDMETHOD.

  METHOD set_array.

    DATA lo_nodes_exp TYPE REF TO lcl_nodes_helper.
    DATA lt_nodes TYPE zif_abapgit_ajson=>ty_nodes_tt.

    DATA lt_tab TYPE TABLE OF ty_struc.
    FIELD-SYMBOLS <s> LIKE LINE OF lt_tab.

    APPEND INITIAL LINE TO lt_tab ASSIGNING <s>.
    <s>-a = 'abc'.
    <s>-b = 10.
    APPEND INITIAL LINE TO lt_tab ASSIGNING <s>.
    <s>-a = 'bcd'.
    <s>-b = 20.

    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '       |      |array  |     | |2' ).
    lo_nodes_exp->add( '/      |1     |object |     |1|4' ).
    lo_nodes_exp->add( '/1/    |a     |str    |abc  | |0' ).
    lo_nodes_exp->add( '/1/    |b     |num    |10   | |0' ).
    lo_nodes_exp->add( '/1/    |c     |bool   |false| |0' ).
    lo_nodes_exp->add( '/1/    |d     |bool   |false| |0' ).
    lo_nodes_exp->add( '/      |2     |object |     |2|4' ).
    lo_nodes_exp->add( '/2/    |a     |str    |bcd  | |0' ).
    lo_nodes_exp->add( '/2/    |b     |num    |20   | |0' ).
    lo_nodes_exp->add( '/2/    |c     |bool   |false| |0' ).
    lo_nodes_exp->add( '/2/    |d     |bool   |false| |0' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = lt_tab ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

    DATA lt_strtab TYPE string_table.
    APPEND 'abc' TO lt_strtab.
    APPEND 'bcd' TO lt_strtab.

    CREATE OBJECT lo_nodes_exp.
    lo_nodes_exp->add( '       |      |array  |     | |2' ).
    lo_nodes_exp->add( '/      |1     |str    |abc  |1|0' ).
    lo_nodes_exp->add( '/      |2     |str    |bcd  |2|0' ).

    lt_nodes = lcl_abap_to_json=>convert( iv_data = lt_strtab ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes
      exp = lo_nodes_exp->mt_nodes ).

  ENDMETHOD.

ENDCLASS.
