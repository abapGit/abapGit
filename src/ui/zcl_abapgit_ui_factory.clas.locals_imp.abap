CLASS lcl_string_buffer DEFINITION FINAL.
  PUBLIC SECTION.
    DATA mt_buffer TYPE string_table READ-ONLY.
    METHODS add
      IMPORTING
        iv_str TYPE string.
    METHODS join_and_flush
      RETURNING
        VALUE(rv_str) TYPE string.
    METHODS join_w_newline_and_flush
      RETURNING
        VALUE(rv_str) TYPE string.
ENDCLASS.

CLASS lcl_string_buffer IMPLEMENTATION.
  METHOD add.
    APPEND iv_str TO mt_buffer.
  ENDMETHOD.
  METHOD join_and_flush.
    rv_str = concat_lines_of( table = mt_buffer ).
    CLEAR mt_buffer.
  ENDMETHOD.
  METHOD join_w_newline_and_flush.
    rv_str = concat_lines_of( table = mt_buffer sep = cl_abap_char_utilities=>newline ).
    CLEAR mt_buffer.
  ENDMETHOD.
ENDCLASS.
