CLASS lcl_stream IMPLEMENTATION.

  METHOD constructor.
    mv_data = iv_data.
    mv_position = 0.
    mv_length = xstrlen( mv_data ).
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
    rv_data = boolc( mv_position < mv_length ).
  ENDMETHOD.

  METHOD eat_offset_and_length.
* note: the logic in this method is unrolled/duplicated for performance reasons
    CONSTANTS: lc_1  TYPE x VALUE '01',
               lc_2  TYPE x VALUE '02',
               lc_4  TYPE x VALUE '04',
               lc_8  TYPE x VALUE '08',
               lc_16 TYPE x VALUE '10',
               lc_32 TYPE x VALUE '20',
               lc_64 TYPE x VALUE '40'.

    IF iv_instruction O lc_1.
      ev_offset   = mv_data+mv_position(1).
      mv_position = mv_position + 1.
    ENDIF.
    IF iv_instruction O lc_2.
      ev_offset   = ev_offset + mv_data+mv_position(1) * 256.
      mv_position = mv_position + 1.
    ENDIF.
    IF iv_instruction O lc_4.
      ev_offset   = ev_offset + mv_data+mv_position(1) * 65536.
      mv_position = mv_position + 1.
    ENDIF.
    IF iv_instruction O lc_8.
      ev_offset   = ev_offset + mv_data+mv_position(1) * 16777216.
      mv_position = mv_position + 1.
    ENDIF.

    IF iv_instruction O lc_16.
      ev_length   = mv_data+mv_position(1).
      mv_position = mv_position + 1.
    ENDIF.
    IF iv_instruction O lc_32.
      ev_length   = ev_length + mv_data+mv_position(1) * 256.
      mv_position = mv_position + 1.
    ENDIF.
    IF iv_instruction O lc_64.
      ev_length   = ev_length + mv_data+mv_position(1) * 65536.
      mv_position = mv_position + 1.
    ENDIF.

    IF ev_length = 0.
      ev_length = 65536.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
