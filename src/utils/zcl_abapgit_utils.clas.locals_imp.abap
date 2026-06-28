CLASS lcl_utf8_utils DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS strip_incomplete_utf8_tail
      IMPORTING
        iv_bytes        TYPE xstring
      RETURNING
        VALUE(rv_bytes) TYPE xstring.

ENDCLASS.

CLASS lcl_utf8_utils IMPLEMENTATION.

  METHOD strip_incomplete_utf8_tail.

    CONSTANTS:
      lc_mask_80 TYPE x LENGTH 1 VALUE '80',
      lc_mask_c0 TYPE x LENGTH 1 VALUE 'C0',
      lc_mask_e0 TYPE x LENGTH 1 VALUE 'E0',
      lc_mask_f0 TYPE x LENGTH 1 VALUE 'F0',
      lc_mask_f8 TYPE x LENGTH 1 VALUE 'F8',
      lc_val_00  TYPE x LENGTH 1 VALUE '00',
      lc_val_c0  TYPE x LENGTH 1 VALUE 'C0',
      lc_val_e0  TYPE x LENGTH 1 VALUE 'E0',
      lc_val_f0  TYPE x LENGTH 1 VALUE 'F0'.

    DATA lv_len      TYPE i.
    DATA lv_i        TYPE i.
    DATA lv_b        TYPE x LENGTH 1.
    DATA lv_and      TYPE x LENGTH 1.
    DATA lv_expected TYPE i.
    DATA lv_actual   TYPE i.
    DATA lv_cut      TYPE i.

    rv_bytes = iv_bytes.

    lv_len = xstrlen( rv_bytes ).
    IF lv_len = 0.
      RETURN.
    ENDIF.

    lv_i = lv_len - 1.

    WHILE lv_i >= 0.
      lv_b = rv_bytes+lv_i(1).
      lv_and = lv_b BIT-AND lc_mask_c0.
      IF lv_and <> lc_mask_80.
        EXIT.
      ENDIF.
      lv_i = lv_i - 1.
    ENDWHILE.

    IF lv_i < 0.
      CLEAR rv_bytes.
      RETURN.
    ENDIF.

    lv_b = rv_bytes+lv_i(1).

    lv_and = lv_b BIT-AND lc_mask_80.
    IF lv_and = lc_val_00.
      lv_expected = 1.
    ELSE.
      lv_and = lv_b BIT-AND lc_mask_e0.
      IF lv_and = lc_val_c0.
        lv_expected = 2.
      ELSE.
        lv_and = lv_b BIT-AND lc_mask_f0.
        IF lv_and = lc_val_e0.
          lv_expected = 3.
        ELSE.
          lv_and = lv_b BIT-AND lc_mask_f8.
          IF lv_and = lc_val_f0.
            lv_expected = 4.
          ELSE.
            lv_cut = lv_i.
            IF lv_cut > 0.
              rv_bytes = rv_bytes(lv_cut).
            ELSE.
              CLEAR rv_bytes.
            ENDIF.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    lv_actual = lv_len - lv_i.

    IF lv_actual < lv_expected.
      lv_cut = lv_i.
      IF lv_cut > 0.
        rv_bytes = rv_bytes(lv_cut).
      ELSE.
        CLEAR rv_bytes.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
