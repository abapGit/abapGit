INTERFACE zif_abapgit_ajson_reader
  PUBLIC .

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
  METHODS get_string
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(rv_value) TYPE string.
  METHODS slice
    IMPORTING
      iv_path TYPE string
    RETURNING
      VALUE(ri_json) TYPE REF TO zif_abapgit_ajson_reader.
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

ENDINTERFACE.
