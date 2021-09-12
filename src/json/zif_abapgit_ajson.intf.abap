INTERFACE zif_abapgit_ajson
  PUBLIC.

  CONSTANTS version TYPE string VALUE 'v1.1.0'. "#EC NOTEXT
  CONSTANTS origin TYPE string VALUE 'https://github.com/sbcgua/ajson'. "#EC NOTEXT
  CONSTANTS license TYPE string VALUE 'MIT'. "#EC NOTEXT

  CONSTANTS:
    BEGIN OF node_type,
      boolean TYPE string VALUE 'bool',
      string  TYPE string VALUE 'str',
      number  TYPE string VALUE 'num',
      null    TYPE string VALUE 'null',
      array   TYPE string VALUE 'array',
      object  TYPE string VALUE 'object',
    END OF node_type.

  TYPES:
    BEGIN OF ty_node,
      path TYPE string,
      name TYPE string,
      type TYPE string,
      value TYPE string,
      index TYPE i,
      order TYPE i,
      children TYPE i,
    END OF ty_node .
  TYPES:
    ty_nodes_tt TYPE STANDARD TABLE OF ty_node WITH KEY path name .
  TYPES:
    ty_nodes_ts TYPE SORTED TABLE OF ty_node
      WITH UNIQUE KEY path name
      WITH NON-UNIQUE SORTED KEY array_index COMPONENTS path index
      WITH NON-UNIQUE SORTED KEY item_order COMPONENTS path order .
  TYPES:
    BEGIN OF ty_path_name,
      path TYPE string,
      name TYPE string,
    END OF ty_path_name.

  " DATA

  DATA mt_json_tree TYPE ty_nodes_ts READ-ONLY.

  " METHODS

  METHODS freeze.
  METHODS keep_item_order
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson.

  " METHODS ex.reader

  METHODS exists
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(rv_exists) TYPE abap_bool.

  METHODS members
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(rt_members) TYPE string_table.

  METHODS get
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(rv_value) TYPE string.

  METHODS get_node_type
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(rv_node_type) TYPE string.

  METHODS get_boolean
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(rv_value) TYPE abap_bool.

  METHODS get_integer
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(rv_value) TYPE i.

  METHODS get_number
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(rv_value) TYPE f.

  METHODS get_date
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(rv_value) TYPE d.

  METHODS get_timestamp
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(rv_value) TYPE timestamp.

  METHODS get_string
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(rv_value) TYPE string.

  METHODS slice
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson.

  METHODS to_abap
    EXPORTING
      ev_container TYPE any
    RAISING
      zcx_abapgit_ajson_error.

  METHODS array_to_string_table
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(rt_string_table) TYPE string_table
    RAISING
      zcx_abapgit_ajson_error.

  " METHODS ex.writer

  METHODS clear
    RAISING
      zcx_abapgit_ajson_error.

  METHODS set
    IMPORTING
      iv_path TYPE string
      iv_val TYPE any
      iv_ignore_empty TYPE abap_bool DEFAULT abap_true
      iv_node_type TYPE string OPTIONAL
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson
    RAISING
      zcx_abapgit_ajson_error.

  METHODS set_boolean
    IMPORTING
      iv_path TYPE string
      iv_val TYPE any
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson
    RAISING
      zcx_abapgit_ajson_error.

  METHODS set_string
    IMPORTING
      iv_path TYPE string
      iv_val TYPE clike
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson
    RAISING
      zcx_abapgit_ajson_error.

  METHODS set_integer
    IMPORTING
      iv_path TYPE string
      iv_val TYPE i
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson
    RAISING
      zcx_abapgit_ajson_error.

  METHODS set_date
    IMPORTING
      iv_path TYPE string
      iv_val TYPE d
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson
    RAISING
      zcx_abapgit_ajson_error.

  METHODS set_timestamp
    IMPORTING
      iv_path TYPE string
      iv_val TYPE timestamp
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson
    RAISING
      zcx_abapgit_ajson_error.

  METHODS set_null
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson
    RAISING
      zcx_abapgit_ajson_error.

  METHODS delete
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson
    RAISING
      zcx_abapgit_ajson_error.

  METHODS touch_array
    IMPORTING
      iv_path TYPE string
      iv_clear TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson
    RAISING
      zcx_abapgit_ajson_error.

  METHODS push
    IMPORTING
      iv_path TYPE string
      iv_val TYPE any
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson
    RAISING
      zcx_abapgit_ajson_error.

  METHODS stringify
    IMPORTING
      iv_indent TYPE i DEFAULT 0
    RETURNING
      VALUE(rv_json) TYPE string
    RAISING
      zcx_abapgit_ajson_error.

ENDINTERFACE.
