INTERFACE zif_abapgit_log
  PUBLIC .

  CONSTANTS:
    BEGIN OF c_status,
      ok      TYPE sy-msgty VALUE 'S',
      error   TYPE sy-msgty VALUE 'E',
      warning TYPE sy-msgty VALUE 'W',
    END OF c_status.

  CONSTANTS:
    BEGIN OF c_log_level,
      empty   TYPE i VALUE 0,
      info    TYPE i VALUE 1,
      warning TYPE i VALUE 2,
      error   TYPE i VALUE 3,
    END OF c_log_level.

  TYPES:
    BEGIN OF ty_log_out,
      type      TYPE sy-msgty,
      text      TYPE string,
      obj_type  TYPE tadir-object,
      obj_name  TYPE tadir-obj_name,
      exception TYPE REF TO cx_root,
    END OF ty_log_out .
  TYPES:
    ty_log_outs TYPE STANDARD TABLE OF ty_log_out
                WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    BEGIN OF ty_msg,
      text TYPE string,
      type TYPE sy-msgty,
      level TYPE i,
    END OF ty_msg .
  TYPES:
    ty_msgs TYPE STANDARD TABLE OF ty_msg
                          WITH NON-UNIQUE DEFAULT KEY .
  TYPES:
    BEGIN OF ty_item_status_out,
      item     TYPE zif_abapgit_definitions=>ty_item,
      status   TYPE sy-msgty,
      messages TYPE ty_msgs,
    END OF ty_item_status_out .
  TYPES:
    ty_item_status_outs TYPE SORTED TABLE OF ty_item_status_out
                        WITH UNIQUE KEY item-obj_type item-obj_name .

  METHODS add
    IMPORTING
      !iv_msg  TYPE csequence
      !iv_type TYPE sy-msgty DEFAULT 'E'
      !is_item TYPE zif_abapgit_definitions=>ty_item OPTIONAL
      !ix_exc  TYPE REF TO cx_root OPTIONAL .
  METHODS add_error
    IMPORTING
      !iv_msg  TYPE csequence
      !is_item TYPE zif_abapgit_definitions=>ty_item OPTIONAL .
  METHODS add_info
    IMPORTING
      !iv_msg  TYPE csequence
      !is_item TYPE zif_abapgit_definitions=>ty_item OPTIONAL .
  METHODS add_warning
    IMPORTING
      !iv_msg  TYPE csequence
      !is_item TYPE zif_abapgit_definitions=>ty_item OPTIONAL .
  METHODS add_success
    IMPORTING
      !iv_msg  TYPE csequence
      !is_item TYPE zif_abapgit_definitions=>ty_item OPTIONAL .
  METHODS add_exception
    IMPORTING
      !ix_exc  TYPE REF TO cx_root
      !is_item TYPE zif_abapgit_definitions=>ty_item OPTIONAL .
  METHODS clear .
  METHODS count
    RETURNING
      VALUE(rv_count) TYPE i .
  METHODS get_messages
    RETURNING
      VALUE(rt_msg) TYPE ty_log_outs .
  METHODS get_item_status
    RETURNING
      VALUE(rt_item_status) TYPE ty_item_status_outs .
  METHODS get_status
    RETURNING
      VALUE(rv_status) TYPE sy-msgty .
  METHODS get_log_level
    RETURNING
      VALUE(rv_level) TYPE i .
  METHODS get_title
    RETURNING
      VALUE(rv_title) TYPE string .
  METHODS set_title
    IMPORTING
      !iv_title TYPE csequence
    RETURNING
      VALUE(ri_log) TYPE REF TO zif_abapgit_log.
  METHODS merge_with
    IMPORTING
      ii_log TYPE REF TO zif_abapgit_log
      iv_min_level TYPE i DEFAULT 0
    RETURNING
      VALUE(ri_log) TYPE REF TO zif_abapgit_log.
  METHODS clone
    RETURNING
      VALUE(ri_log) TYPE REF TO zif_abapgit_log.

ENDINTERFACE.
