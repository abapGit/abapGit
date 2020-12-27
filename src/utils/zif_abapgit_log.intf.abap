INTERFACE zif_abapgit_log
  PUBLIC .


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
      !iv_rc   TYPE sy-subrc OPTIONAL
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
  METHODS has_rc
    IMPORTING
      !iv_rc        TYPE sy-subrc
    RETURNING
      VALUE(rv_yes) TYPE abap_bool .
  METHODS get_messages
    RETURNING
      VALUE(rt_msg) TYPE ty_log_outs .
  METHODS get_item_status
    RETURNING
      VALUE(rt_item_status) TYPE ty_item_status_outs .
  METHODS get_status
    RETURNING
      VALUE(rv_status) TYPE sy-msgty .
  METHODS get_title
    RETURNING
      VALUE(rv_title) TYPE string .
  METHODS set_title
    IMPORTING
      !iv_title TYPE csequence .
ENDINTERFACE.
