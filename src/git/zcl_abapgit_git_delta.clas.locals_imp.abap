CLASS lcl_stream IMPLEMENTATION.

  METHOD constructor.
    mv_data = iv_data.
    mv_position = 0.
  ENDMETHOD.

  METHOD eat_byte.
    rv_x = mv_data+mv_position(1).
    mv_position = mv_position + 1.
  ENDMETHOD.

  METHOD eat_bytes.
    rv_x = mv_data+mv_position(iv_length).
    mv_position = mv_position + iv_length.
  ENDMETHOD.

  METHOD has_data.
    rv_data = boolc( mv_position < xstrlen( mv_data ) ).
  ENDMETHOD.

ENDCLASS.
