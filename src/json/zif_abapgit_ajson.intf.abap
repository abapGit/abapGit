INTERFACE zif_abapgit_ajson
  PUBLIC.

  CONSTANTS version TYPE string VALUE 'v1.1.8'. "#EC NOTEXT
  CONSTANTS origin TYPE string VALUE 'https://github.com/sbcgua/ajson'. "#EC NOTEXT
  CONSTANTS license TYPE string VALUE 'MIT'. "#EC NOTEXT

  TYPES:
    BEGIN OF ty_opts,
      read_only TYPE abap_bool,
      keep_item_order TYPE abap_bool,
      format_datetime TYPE abap_bool,
      to_abap_corresponding_only TYPE abap_bool,
    END OF ty_opts.

  " DATA

  DATA mt_json_tree TYPE zif_abapgit_ajson_types=>ty_nodes_ts READ-ONLY.

  " CLONING

  METHODS clone
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson
    RAISING
      zcx_abapgit_ajson_error.
  METHODS filter
    IMPORTING
      ii_filter TYPE REF TO zif_abapgit_ajson_filter
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson
    RAISING
      zcx_abapgit_ajson_error.
  METHODS map
    IMPORTING
      ii_mapper TYPE REF TO zif_abapgit_ajson_mapping
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson
    RAISING
      zcx_abapgit_ajson_error.

  " METHODS

  METHODS freeze.
  METHODS keep_item_order
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson.
  METHODS format_datetime
    IMPORTING
      iv_use_iso TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson.
  METHODS to_abap_corresponding_only
    IMPORTING
      iv_enable TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson.
  METHODS opts
    RETURNING
      VALUE(rs_opts) TYPE ty_opts.

  " METHODS ex.reader

  METHODS is_empty
    RETURNING
      VALUE(rv_yes) TYPE abap_bool.

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
      VALUE(rv_node_type) TYPE zif_abapgit_ajson_types=>ty_node_type.

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
    IMPORTING
      iv_corresponding TYPE abap_bool DEFAULT abap_false
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
      iv_node_type TYPE zif_abapgit_ajson_types=>ty_node_type OPTIONAL
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson
    RAISING
      zcx_abapgit_ajson_error.

  METHODS setx
    IMPORTING
      iv_param TYPE string
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
