CLASS zcl_abapgit_string_buffer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS add
      IMPORTING
        iv_str TYPE string.
    METHODS join_and_flush
      RETURNING
        VALUE(rv_str) TYPE string.
    METHODS join_w_newline_and_flush
      RETURNING
        VALUE(rv_str) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_buffer TYPE string_table.
ENDCLASS.



CLASS ZCL_ABAPGIT_STRING_BUFFER IMPLEMENTATION.


  METHOD add.
    APPEND iv_str TO mt_buffer.
  ENDMETHOD.


  METHOD join_and_flush.
    rv_str = concat_lines_of( table = mt_buffer ).
    CLEAR mt_buffer.
  ENDMETHOD.


  METHOD join_w_newline_and_flush.
    rv_str = concat_lines_of(
      table = mt_buffer
      sep = cl_abap_char_utilities=>newline ).
    CLEAR mt_buffer.
  ENDMETHOD.
ENDCLASS.
