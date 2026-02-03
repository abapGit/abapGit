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

    DATA lv_x TYPE x.

    ev_offset = 0.
    IF iv_instruction BIT-AND lc_1 = lc_1.
      lv_x = mv_data+mv_position(1).
      mv_position = mv_position + 1.
      ev_offset = lv_x.
    ENDIF.
    IF iv_instruction BIT-AND lc_2 = lc_2.
      lv_x = mv_data+mv_position(1).
      mv_position = mv_position + 1.
      ev_offset = ev_offset + lv_x * 256.
    ENDIF.
    IF iv_instruction BIT-AND lc_4 = lc_4.
      lv_x = mv_data+mv_position(1).
      mv_position = mv_position + 1.
      ev_offset = ev_offset + lv_x * 65536.
    ENDIF.
    IF iv_instruction BIT-AND lc_8 = lc_8.
      lv_x = mv_data+mv_position(1).
      mv_position = mv_position + 1.
      ev_offset = ev_offset + lv_x * 16777216.
    ENDIF.

    ev_length = 0.
    IF iv_instruction BIT-AND lc_16 = lc_16.
      lv_x = mv_data+mv_position(1).
      mv_position = mv_position + 1.
      ev_length = lv_x.
    ENDIF.
    IF iv_instruction BIT-AND lc_32 = lc_32.
      lv_x = mv_data+mv_position(1).
      mv_position = mv_position + 1.
      ev_length = ev_length + lv_x * 256.
    ENDIF.
    IF iv_instruction BIT-AND lc_64 = lc_64.
      lv_x = mv_data+mv_position(1).
      mv_position = mv_position + 1.
      ev_length = ev_length + lv_x * 65536.
    ENDIF.

    IF ev_length = 0.
      ev_length = 65536.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
