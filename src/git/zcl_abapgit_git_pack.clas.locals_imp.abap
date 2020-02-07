*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_stream IMPLEMENTATION.

  METHOD constructor.
    mv_data = iv_data.
  ENDMETHOD.

  METHOD eat_byte.
    rv_x = mv_data(1).
    mv_data = mv_data+1.
  ENDMETHOD.

  METHOD get.
    rv_data = mv_data.
  ENDMETHOD.

  METHOD eat_bytes.
    rv_x = mv_data(iv_length).
    mv_data = mv_data+iv_length.
  ENDMETHOD.

ENDCLASS.
