**********************************************************************
* UTIL
**********************************************************************

CLASS lcl_nodes_helper DEFINITION FINAL.
  PUBLIC SECTION.

    DATA mt_nodes TYPE zif_abapgit_ajson_types=>ty_nodes_tt READ-ONLY.

    METHODS add
      IMPORTING
        iv_str TYPE string.
    METHODS sorted
      RETURNING
        VALUE(rt_nodes) TYPE zif_abapgit_ajson_types=>ty_nodes_ts.

ENDCLASS.

CLASS lcl_nodes_helper IMPLEMENTATION.
  METHOD add.

    FIELD-SYMBOLS <n> LIKE LINE OF mt_nodes.
    DATA lv_children TYPE string.
    DATA lv_index TYPE string.

    APPEND INITIAL LINE TO mt_nodes ASSIGNING <n>.

    SPLIT iv_str AT '|' INTO
      <n>-path
      <n>-name
      <n>-type
      <n>-value
      lv_index
      lv_children.
    CONDENSE <n>-path.
    CONDENSE <n>-name.
    CONDENSE <n>-type.
    CONDENSE <n>-value.
    <n>-index = lv_index.
    <n>-children = lv_children.

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
        iv_separator   TYPE string OPTIONAL
      RETURNING
        VALUE(rv_json) TYPE string.

ENDCLASS.

CLASS ltcl_parser_test IMPLEMENTATION.

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

ENDCLASS.

**********************************************************************
* JSON UTILITIES
**********************************************************************

CLASS ltcl_json_utils DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.

    METHODS json_diff FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS json_diff_types FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS json_diff_arrays FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS json_merge FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS json_sort FOR TESTING RAISING zcx_abapgit_ajson_error.
    METHODS is_equal FOR TESTING RAISING zcx_abapgit_ajson_error.

ENDCLASS.

CLASS ltcl_json_utils IMPLEMENTATION.

  METHOD json_diff.

    DATA:
      lv_json       TYPE string,
      lo_util       TYPE REF TO zcl_abapgit_ajson_utilities,
      lo_insert     TYPE REF TO zif_abapgit_ajson,
      lo_delete     TYPE REF TO zif_abapgit_ajson,
      lo_change     TYPE REF TO zif_abapgit_ajson,
      lo_insert_exp TYPE REF TO lcl_nodes_helper,
      lo_delete_exp TYPE REF TO lcl_nodes_helper,
      lo_change_exp TYPE REF TO lcl_nodes_helper.

    lv_json =
      '{\n' &&
      '  "string": "abc",\n' && " no changes
      '  "number": 789,\n' &&   " changed value
      '  "float": 123.45,\n' &&
      '  "boolean": "true",\n' && " changed type
      '  "true": true,\n' &&    " insert
*      '  "false": false,\n' &&    " delete
      '  "null": null,\n' &&
      '  "date": "2020-03-15",\n' &&
      '  "issues": [\n' &&
      '    {\n' &&
      '      "message": "Indentation problem ...",\n' &&
      '      "key": "indentation",\n' &&
      '      "start": {\n' &&
      '        "row": 5,\n' &&  " array change
      '        "col": 3\n' &&
      '      },\n' &&
      '      "end": {\n' &&
      '        "new": 1,\n' &&  " array insert
*      '        "row": 4,\n' && " array delete
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

    REPLACE ALL OCCURRENCES OF '\n' IN lv_json WITH cl_abap_char_utilities=>newline.

    CREATE OBJECT lo_insert_exp.
    lo_insert_exp->add( '                |        |object |        |0|3' ).
    lo_insert_exp->add( '/               |boolean |str    |true    |0|0' ). " changed type (insert new)
    lo_insert_exp->add( '/               |issues  |array  |        |0|1' ).
    lo_insert_exp->add( '/               |true    |bool   |true    |0|0' ). " insert
    lo_insert_exp->add( '/issues/        |1       |object |        |1|1' ).
    lo_insert_exp->add( '/issues/1/      |end     |object |        |0|1' ).
    lo_insert_exp->add( '/issues/1/end/  |new     |num    |1       |0|0' ). " array insert

    CREATE OBJECT lo_delete_exp.
    lo_delete_exp->add( '                |        |object |        |0|3' ).
    lo_delete_exp->add( '/               |boolean |bool   |true    |0|0' ). " changed type (delete old)
    lo_delete_exp->add( '/               |false   |bool   |false   |0|0' ). " delete
    lo_delete_exp->add( '/               |issues  |array  |        |0|1' ).
    lo_delete_exp->add( '/issues/        |1       |object |        |1|1' ).
    lo_delete_exp->add( '/issues/1/      |end     |object |        |0|1' ).
    lo_delete_exp->add( '/issues/1/end/  |row     |num    |4       |0|0' ). " array delete

    CREATE OBJECT lo_change_exp.
    lo_change_exp->add( '                |        |object |        |0|2' ).
    lo_change_exp->add( '/               |issues  |array  |        |0|1' ).
    lo_change_exp->add( '/               |number  |num    |789     |0|0' ). " changed value
    lo_change_exp->add( '/issues/        |1       |object |        |1|1' ).
    lo_change_exp->add( '/issues/1/      |start   |object |        |0|1' ).
    lo_change_exp->add( '/issues/1/start/|row     |num    |5       |0|0' ). " array change

    CREATE OBJECT lo_util.

    lo_util->diff(
      EXPORTING
        iv_json_a = ltcl_parser_test=>sample_json( )
        iv_json_b = lv_json
      IMPORTING
        eo_insert = lo_insert
        eo_delete = lo_delete
        eo_change = lo_change ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_insert->mt_json_tree
      exp = lo_insert_exp->mt_nodes ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_delete->mt_json_tree
      exp = lo_delete_exp->mt_nodes ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_change->mt_json_tree
      exp = lo_change_exp->mt_nodes ).

  ENDMETHOD.

  METHOD json_diff_types.

    DATA:
      lv_json_a     TYPE string,
      lv_json_b     TYPE string,
      lo_util       TYPE REF TO zcl_abapgit_ajson_utilities,
      lo_insert     TYPE REF TO zif_abapgit_ajson,
      lo_delete     TYPE REF TO zif_abapgit_ajson,
      lo_change     TYPE REF TO zif_abapgit_ajson,
      lo_insert_exp TYPE REF TO lcl_nodes_helper,
      lo_delete_exp TYPE REF TO lcl_nodes_helper.

    " Change single value to array
    lv_json_a =
      '{\n' &&
      '  "string": "abc",\n' &&
      '  "number": 123\n' &&
      '}'.

    lv_json_b =
      '{\n' &&
      '  "string": [\n' &&
      '    "a",\n' &&
      '    "b",\n' &&
      '    "c"\n' &&
      '  ],\n' &&
      '  "number": 123\n' &&
      '}'.

    REPLACE ALL OCCURRENCES OF '\n' IN lv_json_a WITH cl_abap_char_utilities=>newline.
    REPLACE ALL OCCURRENCES OF '\n' IN lv_json_b WITH cl_abap_char_utilities=>newline.

    CREATE OBJECT lo_insert_exp.
    lo_insert_exp->add( '                |        |object |        |0|1' ).
    lo_insert_exp->add( '/               |string  |array  |        |0|3' ).
    lo_insert_exp->add( '/string/        |1       |str    |a       |1|0' ).
    lo_insert_exp->add( '/string/        |2       |str    |b       |2|0' ).
    lo_insert_exp->add( '/string/        |3       |str    |c       |3|0' ).

    CREATE OBJECT lo_delete_exp.
    lo_delete_exp->add( '                |        |object |        |0|1' ).
    lo_delete_exp->add( '/               |string  |str    |abc     |0|0' ).

    CREATE OBJECT lo_util.

    lo_util->diff(
      EXPORTING
        iv_json_a = lv_json_a
        iv_json_b = lv_json_b
      IMPORTING
        eo_insert = lo_insert
        eo_delete = lo_delete
        eo_change = lo_change ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_insert->mt_json_tree
      exp = lo_insert_exp->mt_nodes ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_delete->mt_json_tree
      exp = lo_delete_exp->mt_nodes ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_change->mt_json_tree )
      exp = 0 ).

    " Change array to single value
    lo_util->diff(
      EXPORTING
        iv_json_a = lv_json_b
        iv_json_b = lv_json_a
      IMPORTING
        eo_insert = lo_insert
        eo_delete = lo_delete
        eo_change = lo_change ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_insert->mt_json_tree
      exp = lo_delete_exp->mt_nodes ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_delete->mt_json_tree
      exp = lo_insert_exp->mt_nodes ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_change->mt_json_tree )
      exp = 0 ).

  ENDMETHOD.

  METHOD json_diff_arrays.

    DATA:
      lv_json_a     TYPE string,
      lv_json_b     TYPE string,
      lo_util       TYPE REF TO zcl_abapgit_ajson_utilities,
      lo_insert     TYPE REF TO zif_abapgit_ajson,
      lo_delete     TYPE REF TO zif_abapgit_ajson,
      lo_change     TYPE REF TO zif_abapgit_ajson,
      lo_insert_exp TYPE REF TO lcl_nodes_helper.

    " Add empty array
    lv_json_a =
      '{\n' &&
      '  "number": 123\n' &&
      '}'.

    lv_json_b =
      '{\n' &&
      '  "names": [],\n' &&
      '  "number": 123\n' &&
      '}'.

    REPLACE ALL OCCURRENCES OF '\n' IN lv_json_a WITH cl_abap_char_utilities=>newline.
    REPLACE ALL OCCURRENCES OF '\n' IN lv_json_b WITH cl_abap_char_utilities=>newline.

    CREATE OBJECT lo_util.

    " Empty arrays are ignored by default
    lo_util->diff(
      EXPORTING
        iv_json_a = lv_json_a
        iv_json_b = lv_json_b
      IMPORTING
        eo_insert = lo_insert
        eo_delete = lo_delete
        eo_change = lo_change ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_insert->mt_json_tree )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_delete->mt_json_tree )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_change->mt_json_tree )
      exp = 0 ).

    " Keep empty arrays
    lo_util->diff(
      EXPORTING
        iv_json_a = lv_json_a
        iv_json_b = lv_json_b
        iv_keep_empty_arrays = abap_true
      IMPORTING
        eo_insert = lo_insert
        eo_delete = lo_delete
        eo_change = lo_change ).

    CREATE OBJECT lo_insert_exp.
    lo_insert_exp->add( '                |        |object |        |0|1' ).
    lo_insert_exp->add( '/               |names   |array  |        |0|0' ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_insert->mt_json_tree
      exp = lo_insert_exp->mt_nodes ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_delete->mt_json_tree )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_change->mt_json_tree )
      exp = 0 ).

  ENDMETHOD.

  METHOD json_merge.

    DATA:
      lv_json_a    TYPE string,
      lv_json_b    TYPE string,
      lo_util      TYPE REF TO zcl_abapgit_ajson_utilities,
      lo_merge     TYPE REF TO zif_abapgit_ajson,
      lo_merge_exp TYPE REF TO lcl_nodes_helper.

    " Merge new value of b into a
    lv_json_a =
      '{\n' &&
      '  "string": [\n' &&
      '    "a",\n' &&
      '    "c"\n' &&
      '  ],\n' &&
      '  "number": 123\n' &&
      '}'.

    lv_json_b =
      '{\n' &&
      '  "string": [\n' &&
      '    "a",\n' &&
      '    "b"\n' && " new array value
      '  ],\n' &&
      '  "number": 456,\n' && " existing values are not overwritten
      '  "float": 123.45\n' &&
      '}'.

    REPLACE ALL OCCURRENCES OF '\n' IN lv_json_a WITH cl_abap_char_utilities=>newline.
    REPLACE ALL OCCURRENCES OF '\n' IN lv_json_b WITH cl_abap_char_utilities=>newline.

    CREATE OBJECT lo_merge_exp.
    lo_merge_exp->add( '                |        |object |        |0|3' ).
    lo_merge_exp->add( '/               |float   |num    |123.45  |0|0' ).
    lo_merge_exp->add( '/               |number  |num    |123     |0|0' ).
    lo_merge_exp->add( '/               |string  |array  |        |0|3' ).
    lo_merge_exp->add( '/string/        |1       |str    |a       |1|0' ).
    lo_merge_exp->add( '/string/        |2       |str    |c       |2|0' ).
    lo_merge_exp->add( '/string/        |3       |str    |b       |3|0' ).

    CREATE OBJECT lo_util.

    lo_merge = lo_util->merge(
      iv_json_a = lv_json_a
      iv_json_b = lv_json_b ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_merge->mt_json_tree
      exp = lo_merge_exp->mt_nodes ).

  ENDMETHOD.

  METHOD json_sort.

    DATA:
      lv_json       TYPE string,
      lo_util       TYPE REF TO zcl_abapgit_ajson_utilities,
      lv_sorted     TYPE string,
      lv_sorted_exp TYPE string.

    lv_json =
      '{\n' &&
      '  "string": "abc",\n' &&
      '  "number": 789,\n' &&
      '  "float": 123.45,\n' &&
      '  "boolean": "true",\n' &&
      '  "true": true,\n' &&
      '  "false": false,\n' &&
      '  "null": null,\n' &&
      '  "date": "2020-03-15"\n' &&
      '}'.

    REPLACE ALL OCCURRENCES OF '\n' IN lv_json WITH cl_abap_char_utilities=>newline.

    lv_sorted_exp =
      '{\n' &&
      '  "boolean": "true",\n' &&
      '  "date": "2020-03-15",\n' &&
      '  "false": false,\n' &&
      '  "float": 123.45,\n' &&
      '  "null": null,\n' &&
      '  "number": 789,\n' &&
      '  "string": "abc",\n' &&
      '  "true": true\n' &&
      '}'.

    REPLACE ALL OCCURRENCES OF '\n' IN lv_sorted_exp WITH cl_abap_char_utilities=>newline.

    CREATE OBJECT lo_util.

    lv_sorted = lo_util->sort( iv_json = lv_json ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_sorted
      exp = lv_sorted_exp ).

  ENDMETHOD.

  METHOD is_equal.

    cl_abap_unit_assert=>assert_true(
      zcl_abapgit_ajson_utilities=>new( )->is_equal(
        ii_json_a = zcl_abapgit_ajson=>parse( '{"a":1,"b":2}' )
        ii_json_b = zcl_abapgit_ajson=>parse( '{"a":1,"b":2}' ) ) ).

    cl_abap_unit_assert=>assert_true(
      zcl_abapgit_ajson_utilities=>new( )->is_equal(
        iv_json_a = '{"a":1,"b":2}'
        iv_json_b = '{"a":1,"b":2}' ) ).

    cl_abap_unit_assert=>assert_false(
      zcl_abapgit_ajson_utilities=>new( )->is_equal(
        iv_json_a = '{"a":1,"b":2}'
        iv_json_b = '{"a":1,"b":3}' ) ).

    cl_abap_unit_assert=>assert_false(
      zcl_abapgit_ajson_utilities=>new( )->is_equal(
        iv_json_a = '{"a":1,"b":2}'
        iv_json_b = '{"a":1,"b":2,"c":3}' ) ).

    cl_abap_unit_assert=>assert_false(
      zcl_abapgit_ajson_utilities=>new( )->is_equal(
        iv_json_a = '{"a":1,"b":2,"c":3}'
        iv_json_b = '{"a":1,"b":2}' ) ).

  ENDMETHOD.

ENDCLASS.
