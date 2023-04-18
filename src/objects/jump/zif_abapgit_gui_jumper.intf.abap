INTERFACE zif_abapgit_gui_jumper
  PUBLIC.

  TYPES:
    ty_bdcdata_tt TYPE STANDARD TABLE OF bdcdata WITH DEFAULT KEY.

  METHODS jump
    IMPORTING
      !is_item         TYPE zif_abapgit_definitions=>ty_item
      !is_sub_item     TYPE zif_abapgit_definitions=>ty_item OPTIONAL
      !iv_line_number  TYPE i OPTIONAL
      !iv_new_window   TYPE abap_bool DEFAULT abap_true
    RETURNING
      VALUE(rv_exit)   TYPE abap_bool
    RAISING
      zcx_abapgit_exception.

  METHODS jump_adt
    IMPORTING
      !is_item         TYPE zif_abapgit_definitions=>ty_item
      !iv_sub_obj_name TYPE zif_abapgit_definitions=>ty_item-obj_name
      !iv_line_number  TYPE i
    RETURNING
      VALUE(rv_exit)   TYPE abap_bool
    RAISING
      zcx_abapgit_exception.

  METHODS jump_batch_input
    IMPORTING
      !iv_tcode      TYPE sy-tcode
      !it_bdcdata    TYPE ty_bdcdata_tt
      !iv_new_window TYPE abap_bool DEFAULT abap_true
    RAISING
      zcx_abapgit_exception.

  METHODS jump_abapgit
    IMPORTING
      !iv_language TYPE spras
      !iv_key      TYPE zif_abapgit_persistence=>ty_value
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
