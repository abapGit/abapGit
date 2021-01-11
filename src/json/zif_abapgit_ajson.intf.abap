INTERFACE zif_abapgit_ajson
  PUBLIC .

  CONSTANTS version TYPE string VALUE 'v1.0.3'.
  CONSTANTS origin TYPE string VALUE 'https://github.com/sbcgua/ajson'.

  INTERFACES zif_abapgit_ajson_reader.
  INTERFACES zif_abapgit_ajson_writer.

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
  METHODS keep_item_order.

  " METHODS (merged from reader/writer), maybe will completely move to this IF in future !

  ALIASES:
    exists FOR zif_abapgit_ajson_reader~exists,
    members FOR zif_abapgit_ajson_reader~members,
    get FOR zif_abapgit_ajson_reader~get,
    get_boolean FOR zif_abapgit_ajson_reader~get_boolean,
    get_integer FOR zif_abapgit_ajson_reader~get_integer,
    get_number FOR zif_abapgit_ajson_reader~get_number,
    get_date FOR zif_abapgit_ajson_reader~get_date,
    get_string FOR zif_abapgit_ajson_reader~get_string,
    slice FOR zif_abapgit_ajson_reader~slice,
    to_abap FOR zif_abapgit_ajson_reader~to_abap,
    array_to_string_table FOR zif_abapgit_ajson_reader~array_to_string_table.

  ALIASES:
    clear FOR zif_abapgit_ajson_writer~clear,
    set FOR zif_abapgit_ajson_writer~set,
    set_boolean FOR zif_abapgit_ajson_writer~set_boolean,
    set_string FOR zif_abapgit_ajson_writer~set_string,
    set_integer FOR zif_abapgit_ajson_writer~set_integer,
    set_date FOR zif_abapgit_ajson_writer~set_date,
    set_null FOR zif_abapgit_ajson_writer~set_null,
    delete FOR zif_abapgit_ajson_writer~delete,
    touch_array FOR zif_abapgit_ajson_writer~touch_array,
    push FOR zif_abapgit_ajson_writer~push,
    stringify FOR zif_abapgit_ajson_writer~stringify.

ENDINTERFACE.
