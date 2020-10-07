**********************************************************************
* UTIL
**********************************************************************
CLASS lcl_nodes_helper DEFINITION FINAL.
  PUBLIC SECTION.

    DATA mt_nodes TYPE zcl_abapgit_ajson=>ty_nodes_tt.
    METHODS add
      IMPORTING
        iv_str TYPE string.
    METHODS sorted
      RETURNING
        VALUE(rt_nodes) TYPE zcl_abapgit_ajson=>ty_nodes_ts.

ENDCLASS.

CLASS lcl_nodes_helper IMPLEMENTATION.
  METHOD add.

    FIELD-SYMBOLS <ls_n> LIKE LINE OF mt_nodes.
    DATA lv_children TYPE string.
    DATA lv_index TYPE string.

    APPEND INITIAL LINE TO mt_nodes ASSIGNING <ls_n>.

    SPLIT iv_str AT '|' INTO
      <ls_n>-path
      <ls_n>-name
      <ls_n>-type
      <ls_n>-value
      lv_index
      lv_children.
    CONDENSE <ls_n>-path.
    CONDENSE <ls_n>-name.
    CONDENSE <ls_n>-type.
    CONDENSE <ls_n>-value.
    <ls_n>-index = lv_index.
    <ls_n>-children = lv_children.

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
      RETURNING
        VALUE(rv_json) TYPE string.

  PRIVATE SECTION.

    METHODS parse FOR TESTING RAISING zcx_abapgit_ajson_error.

ENDCLASS.

CLASS ltcl_parser_test IMPLEMENTATION.

  METHOD sample_json.

    rv_json =
      '{' &&
      '  "string": "abc",' &&
      '  "number": 123,' &&
      '  "float": 123.45,' &&
      '  "boolean": true,' &&
      '  "false": false,' &&
      '  "null": null,' &&
      '  "date": "2020-03-15",' &&
      '  "issues": [' &&
      '    {' &&
      '      "message": "Indentation problem ...",' &&
      '      "key": "indentation",' &&
      '      "start": {' &&
      '        "row": 4,' &&
      '        "col": 3' &&
      '      },' &&
      '      "end": {' &&
      '        "row": 4,' &&
      '        "col": 26' &&
      '      },' &&
      '      "filename": "./zxxx.prog.abap"' &&
      '    },' &&
      '    {' &&
      '      "message": "Remove space before XXX",' &&
      '      "key": "space_before_dot",' &&
      '      "start": {' &&
      '        "row": 3,' &&
      '        "col": 21' &&
      '      },' &&
      '      "end": {' &&
      '        "row": 3,' &&
      '        "col": 22' &&
      '      },' &&
      '      "filename": "./zxxx.prog.abap"' &&
      '    }' &&
      '  ]' &&
      '}'.

  ENDMETHOD.

  METHOD parse.

    DATA lo_cut TYPE REF TO lcl_json_parser.
    DATA lt_act TYPE zcl_abapgit_ajson=>ty_nodes_tt.
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

ENDCLASS.

CLASS zcl_abapgit_ajson DEFINITION LOCAL FRIENDS ltcl_utils_test.

CLASS ltcl_utils_test IMPLEMENTATION.

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

    DATA ls_exp TYPE zcl_abapgit_ajson=>ty_path_name.
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
    METHODS exists FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS value_integer FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS value_number FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS value_boolean FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS value_string FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS members FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS slice FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS array_to_string_table FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS get_date FOR TESTING RAISING zcx_abapgit_ajson_error.

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
    DATA lo_err TYPE REF TO zcx_abapgit_ajson_error.

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

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '  |         |object |                        | |1' ).
    lo_nodes->add( '/ |a        |str    |abc                     | |0' ).
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    TRY.
        lo_cut->zif_abapgit_ajson_reader~array_to_string_table( '/x' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_err.
        cl_abap_unit_assert=>assert_equals(
          act = lo_err->message
          exp = 'Path not found: /x' ).
    ENDTRY.

    TRY.
        lo_cut->zif_abapgit_ajson_reader~array_to_string_table( '/' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_err.
        cl_abap_unit_assert=>assert_equals(
          act = lo_err->message
          exp = 'Array expected at: /' ).
    ENDTRY.

    TRY.
        lo_cut->zif_abapgit_ajson_reader~array_to_string_table( '/a' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_err.
        cl_abap_unit_assert=>assert_equals(
          act = lo_err->message
          exp = 'Array expected at: /a' ).
    ENDTRY.

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '  |         |array  |                        | |1' ).
    lo_nodes->add( '/ |1        |object |                        |1|0' ).
    lo_cut->mt_json_tree = lo_nodes->mt_nodes.

    TRY.
        lo_cut->zif_abapgit_ajson_reader~array_to_string_table( '/' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_err.
        cl_abap_unit_assert=>assert_equals(
          act = lo_err->message
          exp = 'Cannot convert [object] to string at [/1]' ).
    ENDTRY.

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
      ty_struc_tt TYPE STANDARD TABLE OF ty_struc WITH DEFAULT KEY,
      BEGIN OF ty_complex,
        str   TYPE string,
        int   TYPE i,
        float TYPE f,
        bool  TYPE abap_bool,
        obj   TYPE ty_struc,
        tab   TYPE ty_struc_tt,
        oref  TYPE REF TO object,
        date1 TYPE d,
        date2 TYPE d,
      END OF ty_complex.

    METHODS find_loc FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS find_loc_negative FOR TESTING.
    METHODS find_loc_append FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS to_abap FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS to_abap_negative FOR TESTING.

    METHODS prepare_cut
      EXPORTING
        eo_cut TYPE REF TO lcl_json_to_abap
        es_elem TYPE ty_struc
        es_mock TYPE ty_complex.

ENDCLASS.

CLASS ltcl_json_to_abap IMPLEMENTATION.

  METHOD prepare_cut.

    es_mock-str = 'Hello'.
    es_mock-int = 10.
    es_mock-obj-a = 'World'.
    es_elem-a = 'One'.
    es_elem-b = 1.
    APPEND es_elem TO es_mock-tab.
    es_elem-a = 'two'.
    es_elem-b = 2.
    APPEND es_elem TO es_mock-tab.

    lcl_json_to_abap=>bind(
      CHANGING
        cv_obj = es_mock
        co_instance = eo_cut ).

  ENDMETHOD.

  METHOD find_loc.

    DATA ls_last_elem TYPE ty_struc.
    DATA ls_mock TYPE ty_complex.
    DATA lo_cut TYPE REF TO lcl_json_to_abap.
    DATA lv_ref TYPE REF TO data.
    FIELD-SYMBOLS <lv_val> TYPE any.

    prepare_cut(
      IMPORTING
        eo_cut = lo_cut
        es_mock = ls_mock
        es_elem = ls_last_elem ).


    lv_ref = lo_cut->find_loc( 'str' ). " Relative also works but from root
    ASSIGN lv_ref->* TO <lv_val>.
    cl_abap_unit_assert=>assert_equals(
      act = <lv_val>
      exp = 'Hello' ).

    lv_ref = lo_cut->find_loc( '/str' ).
    ASSIGN lv_ref->* TO <lv_val>.
    cl_abap_unit_assert=>assert_equals(
      act = <lv_val>
      exp = 'Hello' ).

    lv_ref = lo_cut->find_loc( '/int' ).
    ASSIGN lv_ref->* TO <lv_val>.
    cl_abap_unit_assert=>assert_equals(
      act = <lv_val>
      exp = 10 ).

    lv_ref = lo_cut->find_loc( '/obj/a' ).
    ASSIGN lv_ref->* TO <lv_val>.
    cl_abap_unit_assert=>assert_equals(
      act = <lv_val>
      exp = 'World' ).

    lv_ref = lo_cut->find_loc(
      iv_path = '/obj'
      iv_name = 'a' ).
    ASSIGN lv_ref->* TO <lv_val>.
    cl_abap_unit_assert=>assert_equals(
      act = <lv_val>
      exp = 'World' ).

    lv_ref = lo_cut->find_loc( '/obj' ).
    ASSIGN lv_ref->* TO <lv_val>.
    cl_abap_unit_assert=>assert_equals(
      act = <lv_val>
      exp = ls_mock-obj ).

    lv_ref = lo_cut->find_loc( '/' ).
    ASSIGN lv_ref->* TO <lv_val>.
    cl_abap_unit_assert=>assert_equals(
      act = <lv_val>
      exp = ls_mock ).

    lv_ref = lo_cut->find_loc( '/tab/2' ).
    ASSIGN lv_ref->* TO <lv_val>.
    cl_abap_unit_assert=>assert_equals(
      act = <lv_val>
      exp = ls_last_elem ).

    lv_ref = lo_cut->find_loc( '/tab/1/a' ).
    ASSIGN lv_ref->* TO <lv_val>.
    cl_abap_unit_assert=>assert_equals(
      act = <lv_val>
      exp = 'One' ).

  ENDMETHOD.

  METHOD find_loc_append.

    DATA ls_last_elem TYPE ty_struc.
    DATA ls_mock TYPE ty_complex.
    DATA lo_cut TYPE REF TO lcl_json_to_abap.
    DATA lo_err TYPE REF TO zcx_abapgit_ajson_error.
    DATA lv_ref TYPE REF TO data.
    FIELD-SYMBOLS <lv_val> TYPE any.

    prepare_cut(
      IMPORTING
        eo_cut = lo_cut
        es_mock = ls_mock
        es_elem = ls_last_elem ).


    lv_ref = lo_cut->find_loc( '/tab/1/a' ).
    ASSIGN lv_ref->* TO <lv_val>.
    cl_abap_unit_assert=>assert_equals(
      act = <lv_val>
      exp = 'One' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_mock-tab )
      exp = 2 ).

    TRY.
        lo_cut->find_loc( '/tab/3/a' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_err.
        cl_abap_unit_assert=>assert_equals(
          act = lo_err->message
          exp = 'Index not found in table' ).
    ENDTRY.

    lv_ref = lo_cut->find_loc(
      iv_path = '/tab/3/a'
      iv_append_tables = abap_true ).
    ASSIGN lv_ref->* TO <lv_val>.
    cl_abap_unit_assert=>assert_equals(
      act = <lv_val>
      exp = '' ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_mock-tab )
      exp = 3 ).

    TRY.
        lo_cut->find_loc( '/tab/5/a' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_err.
        cl_abap_unit_assert=>assert_equals(
          act = lo_err->message
          exp = 'Index not found in table' ).
    ENDTRY.

  ENDMETHOD.

  METHOD find_loc_negative.

    DATA lo_cut TYPE REF TO lcl_json_to_abap.
    DATA lo_err TYPE REF TO zcx_abapgit_ajson_error.
    DATA ls_mock TYPE ty_complex.

    prepare_cut(
      IMPORTING
        es_mock = ls_mock " Must be here to keep reference alive
        eo_cut = lo_cut ).

    TRY.
        lo_cut->find_loc( '/xyz' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_err.
        cl_abap_unit_assert=>assert_equals(
          act = lo_err->message
          exp = 'Path not found' ).
    ENDTRY.

    TRY.
        lo_cut->find_loc( '/oref/xyz' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_err.
        cl_abap_unit_assert=>assert_equals(
          act = lo_err->message
          exp = 'Cannot assign to ref' ).
    ENDTRY.

    TRY.
        lo_cut->find_loc( '/tab/xyz' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_err.
        cl_abap_unit_assert=>assert_equals(
          act = lo_err->message
          exp = 'Need index to access tables' ).
    ENDTRY.

    TRY.
        lo_cut->find_loc( '/tab/5' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_err.
        cl_abap_unit_assert=>assert_equals(
          act = lo_err->message
          exp = 'Index not found in table' ).
    ENDTRY.

  ENDMETHOD.

  METHOD to_abap.

    DATA lo_cut TYPE REF TO lcl_json_to_abap.
    DATA ls_mock TYPE ty_complex.
    DATA lv_exp_date TYPE d VALUE '20200728'.
    DATA ls_elem LIKE LINE OF ls_mock-tab.
    DATA lo_nodes TYPE REF TO lcl_nodes_helper.
    lcl_json_to_abap=>bind(
      CHANGING
        cv_obj = ls_mock
        co_instance = lo_cut ).

    CREATE OBJECT lo_nodes.
    lo_nodes->add( '/      |      |object |       | ' ).
    lo_nodes->add( '/      |str   |str    |hello  | ' ).
    lo_nodes->add( '/      |int   |num    |5      | ' ).
    lo_nodes->add( '/      |float |num    |5.5    | ' ).
    lo_nodes->add( '/      |bool  |bool   |true   | ' ).
    lo_nodes->add( '/      |obj   |object |       | ' ).
    lo_nodes->add( '/obj   |a     |str    |world  | ' ).
    lo_nodes->add( '/      |tab   |array  |       | ' ).
    lo_nodes->add( '/tab   |1     |object |       |1' ).
    lo_nodes->add( '/tab/1 |a     |str    | One   | ' ).
    lo_nodes->add( '/tab   |2     |object |       |2' ).
    lo_nodes->add( '/tab/2 |a     |str    | Two   | ' ).
    lo_nodes->add( '/      |date1 |str    |2020-07-28 | ' ).
    lo_nodes->add( '/      |date2 |str    |2020-07-28T00:00:00Z | ' ).

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
    DATA lo_err TYPE REF TO zcx_abapgit_ajson_error.
    DATA ls_mock TYPE ty_complex.
    DATA lo_nodes TYPE REF TO lcl_nodes_helper.

    lcl_json_to_abap=>bind(
      CHANGING
        cv_obj = ls_mock
        co_instance = lo_cut ).

    TRY.
        CREATE OBJECT lo_nodes.
        lo_nodes->add( '/    |      |object | ' ).
        lo_nodes->add( '/    |str   |object | ' ).

        lo_cut->to_abap( lo_nodes->sorted( ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_err.
        cl_abap_unit_assert=>assert_equals(
          act = lo_err->message
          exp = 'Expected structure' ).
    ENDTRY.

    TRY.
        CREATE OBJECT lo_nodes.
        lo_nodes->add( '/    |      |object | ' ).
        lo_nodes->add( '/    |str   |array  | ' ).

        lo_cut->to_abap( lo_nodes->sorted( ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_err.
        cl_abap_unit_assert=>assert_equals(
          act = lo_err->message
          exp = 'Expected table' ).
    ENDTRY.

    TRY.
        CREATE OBJECT lo_nodes.
        lo_nodes->add( '/    |      |object |      ' ).
        lo_nodes->add( '/    |int   |str    |hello ' ).

        lo_cut->to_abap( lo_nodes->sorted( ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_err.
        cl_abap_unit_assert=>assert_equals(
          act = lo_err->message
          exp = 'Source is not a number' ).
    ENDTRY.

    TRY.
        CREATE OBJECT lo_nodes.
        lo_nodes->add( '/    |      |object |        ' ).
        lo_nodes->add( '/    |date1 |str    |baddate ' ).

        lo_cut->to_abap( lo_nodes->sorted( ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_ajson_error INTO lo_err.
        cl_abap_unit_assert=>assert_equals(
          act = lo_err->message
          exp = 'Unexpected date format' ).
    ENDTRY.

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
        message  TYPE string,
        key      TYPE string,
        filename TYPE string,
        start    TYPE ty_loc,
        end      TYPE ty_loc,
      END OF ty_issue,
      ty_issues TYPE STANDARD TABLE OF ty_issue WITH DEFAULT KEY,
      BEGIN OF ty_target,
        string  TYPE string,
        number  TYPE i,
        float   TYPE f,
        boolean TYPE abap_bool,
        false   TYPE abap_bool,
        null    TYPE string,
        date    TYPE string, " ??? TODO
        issues  TYPE ty_issues,
      END OF ty_target.

    METHODS reader FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS array_index FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS array_simple FOR TESTING RAISING zcx_abapgit_ajson_error.

ENDCLASS.

CLASS ltcl_integrated IMPLEMENTATION.

  METHOD array_simple.

    DATA lt_act TYPE string_table.
    DATA lt_exp TYPE string_table.
    DATA lv_exp TYPE string.
    DATA li_reader TYPE REF TO zif_abapgit_ajson_reader.
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
    DATA li_reader TYPE REF TO zif_abapgit_ajson_reader.
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

    li_reader = zcl_abapgit_ajson=>parse( lv_src ).
    li_reader->to_abap( IMPORTING ev_container = lt_act ).

    cl_abap_unit_assert=>assert_equals(
      act = lt_act
      exp = lt_exp ).

  ENDMETHOD.

  METHOD reader.

    DATA lv_source TYPE string.
    DATA li_reader TYPE REF TO zif_abapgit_ajson_reader.
    DATA ls_act TYPE ty_target.
    DATA ls_exp TYPE ty_target.
    FIELD-SYMBOLS <ls_i> LIKE LINE OF ls_exp-issues.

    lv_source = ltcl_parser_test=>sample_json( ).
    li_reader = zcl_abapgit_ajson=>parse( lv_source ).

    cl_abap_unit_assert=>assert_equals(
      act = li_reader->get( '/string' )
      exp = 'abc' ).


    ls_exp-string = 'abc'.
    ls_exp-number = 123.
    ls_exp-float = '123.45'.
    ls_exp-boolean = abap_true.
    ls_exp-false = abap_false.
    ls_exp-date = '2020-03-15'.

    APPEND INITIAL LINE TO ls_exp-issues ASSIGNING <ls_i>.
    <ls_i>-message  = 'Indentation problem ...'.
    <ls_i>-key      = 'indentation'.
    <ls_i>-filename = './zxxx.prog.abap'.
    <ls_i>-start-row = 4.
    <ls_i>-start-col = 3.
    <ls_i>-end-row   = 4.
    <ls_i>-end-col   = 26.

    APPEND INITIAL LINE TO ls_exp-issues ASSIGNING <ls_i>.
    <ls_i>-message  = 'Remove space before XXX'.
    <ls_i>-key      = 'space_before_dot'.
    <ls_i>-filename = './zxxx.prog.abap'.
    <ls_i>-start-row = 3.
    <ls_i>-start-col = 21.
    <ls_i>-end-row   = 3.
    <ls_i>-end-col   = 22.

    li_reader->to_abap( IMPORTING ev_container = ls_act ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_act
      exp = ls_exp ).

  ENDMETHOD.

ENDCLASS.
