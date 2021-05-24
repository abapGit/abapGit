INTERFACE zif_abapgit_ajson_writer
  PUBLIC .

  METHODS clear
    RAISING
      zcx_abapgit_ajson_error.

  METHODS set
    IMPORTING
      iv_path TYPE string
      iv_val TYPE any
      iv_ignore_empty TYPE abap_bool DEFAULT abap_true
      iv_node_type TYPE string OPTIONAL
    RAISING
      zcx_abapgit_ajson_error.

  METHODS set_boolean
    IMPORTING
      iv_path TYPE string
      iv_val TYPE any
    RAISING
      zcx_abapgit_ajson_error.

  METHODS set_string
    IMPORTING
      iv_path TYPE string
      iv_val TYPE clike
    RAISING
      zcx_abapgit_ajson_error.

  METHODS set_integer
    IMPORTING
      iv_path TYPE string
      iv_val TYPE i
    RAISING
      zcx_abapgit_ajson_error.

  METHODS set_date
    IMPORTING
      iv_path TYPE string
      iv_val TYPE d
    RAISING
      zcx_abapgit_ajson_error.

  METHODS set_timestamp
    IMPORTING
      iv_path TYPE string
      iv_val TYPE timestamp
    RAISING
      zcx_abapgit_ajson_error.

  METHODS set_null
    IMPORTING
      iv_path TYPE string
    RAISING
      zcx_abapgit_ajson_error.

  METHODS delete
    IMPORTING
      iv_path TYPE string
    RAISING
      zcx_abapgit_ajson_error.

  METHODS touch_array
    IMPORTING
      iv_path TYPE string
      iv_clear TYPE abap_bool DEFAULT abap_false
    RAISING
      zcx_abapgit_ajson_error.

  METHODS push
    IMPORTING
      iv_path TYPE string
      iv_val TYPE any
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
