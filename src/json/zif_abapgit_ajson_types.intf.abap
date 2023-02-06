INTERFACE zif_abapgit_ajson_types
  PUBLIC.

  TYPES:
    ty_node_type TYPE string.

  CONSTANTS:
    BEGIN OF node_type,
      boolean TYPE ty_node_type VALUE 'bool',
      string  TYPE ty_node_type VALUE 'str',
      number  TYPE ty_node_type VALUE 'num',
      null    TYPE ty_node_type VALUE 'null',
      array   TYPE ty_node_type VALUE 'array',
      object  TYPE ty_node_type VALUE 'object',
    END OF node_type.

  TYPES:
    BEGIN OF ty_node,
      path TYPE string,
      name TYPE string,
      type TYPE ty_node_type,
      value TYPE string,
      index TYPE i,
      order TYPE i,
      children TYPE i,
    END OF ty_node.
  TYPES:
    ty_nodes_tt TYPE STANDARD TABLE OF ty_node WITH KEY path name.
  TYPES:
    ty_nodes_ts TYPE SORTED TABLE OF ty_node
      WITH UNIQUE KEY path name
      WITH NON-UNIQUE SORTED KEY array_index COMPONENTS path index
      WITH NON-UNIQUE SORTED KEY item_order COMPONENTS path order.

  TYPES:
    BEGIN OF ty_path_name,
      path TYPE string,
      name TYPE string,
    END OF ty_path_name.

ENDINTERFACE.
