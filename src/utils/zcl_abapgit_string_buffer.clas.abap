CLASS zcl_abapgit_string_buffer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS new
      RETURNING
        VALUE(ro_me) TYPE REF TO zcl_abapgit_string_buffer.
    METHODS add
      IMPORTING
        !iv_str      TYPE string
      RETURNING
        VALUE(ro_me) TYPE REF TO zcl_abapgit_string_buffer.
    METHODS join_and_flush
      RETURNING
        VALUE(rv_str) TYPE string.
    METHODS join_w_newline_and_flush
      RETURNING
        VALUE(rv_str) TYPE string.
    METHODS join_w_space_and_flush
      RETURNING
        VALUE(rv_str) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_buffer TYPE string_table.
ENDCLASS.



CLASS zcl_abapgit_string_buffer IMPLEMENTATION.


  METHOD add.
    APPEND iv_str TO mt_buffer.
    ro_me = me.
  ENDMETHOD.


  METHOD join_and_flush.
    rv_str = concat_lines_of( table = mt_buffer ).
    CLEAR mt_buffer.
  ENDMETHOD.


  METHOD join_w_newline_and_flush.
    rv_str = concat_lines_of(
      table = mt_buffer
      sep   = cl_abap_char_utilities=>newline ).
    CLEAR mt_buffer.
  ENDMETHOD.


  METHOD join_w_space_and_flush.
    rv_str = concat_lines_of(
      table = mt_buffer
      sep   = ` ` ).
    CLEAR mt_buffer.
  ENDMETHOD.


  METHOD new.
    CREATE OBJECT ro_me.
  ENDMETHOD.
ENDCLASS.
