CLASS zcl_abapgit_zlib_huffman DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_lengths TYPE STANDARD TABLE OF i WITH DEFAULT KEY .

    " If this ever changes, adjust the bit instance data as well
    CONSTANTS c_maxbits TYPE i VALUE 15 ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !it_lengths TYPE ty_lengths .
    METHODS get_count
      IMPORTING
        !iv_index       TYPE i
      RETURNING
        VALUE(rv_value) TYPE i .
    METHODS get_symbol
      IMPORTING
        !iv_index       TYPE i
      RETURNING
        VALUE(rv_value) TYPE i .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: mt_count  TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
          mt_symbol TYPE STANDARD TABLE OF i WITH DEFAULT KEY.

    DATA:
      mv_bit01 TYPE i,
      mv_bit02 TYPE i,
      mv_bit03 TYPE i,
      mv_bit04 TYPE i,
      mv_bit05 TYPE i,
      mv_bit06 TYPE i,
      mv_bit07 TYPE i,
      mv_bit08 TYPE i,
      mv_bit09 TYPE i,
      mv_bit10 TYPE i,
      mv_bit11 TYPE i,
      mv_bit12 TYPE i,
      mv_bit13 TYPE i,
      mv_bit14 TYPE i,
      mv_bit15 TYPE i.

ENDCLASS.



CLASS ZCL_ABAPGIT_ZLIB_HUFFMAN IMPLEMENTATION.


  METHOD constructor.

    DATA: lv_index  TYPE i,
          lt_offset TYPE TABLE OF i,
          lv_length LIKE LINE OF it_lengths,
          lv_prev   TYPE i,
          lv_count  LIKE LINE OF mt_count.

    FIELD-SYMBOLS: <lv_offset> LIKE LINE OF lt_offset,
                   <lv_symbol> LIKE LINE OF mt_symbol,
                   <lv_i>      LIKE LINE OF it_lengths.


    DO c_maxbits TIMES.
      APPEND 0 TO mt_count.
    ENDDO.
    LOOP AT it_lengths INTO lv_index.
      IF lv_index = 0.
        CONTINUE.
      ENDIF.
      READ TABLE mt_count INDEX lv_index ASSIGNING <lv_i>.
      ASSERT sy-subrc = 0.
      <lv_i> = <lv_i> + 1.
    ENDLOOP.

************

    APPEND 0 TO lt_offset.
    DO c_maxbits - 1 TIMES.
      READ TABLE mt_count INDEX sy-index INTO lv_count.
      ASSERT sy-subrc = 0.
      lv_prev = lv_prev + lv_count.
      APPEND lv_prev TO lt_offset.
    ENDDO.

    DO lines( it_lengths ) TIMES.
      APPEND 0 TO mt_symbol.
    ENDDO.
    DO lines( it_lengths ) TIMES.
      lv_index = sy-index.
      READ TABLE it_lengths INDEX lv_index INTO lv_length.
      ASSERT sy-subrc = 0.
      IF lv_length = 0.
        CONTINUE.
      ENDIF.
      READ TABLE lt_offset INDEX lv_length ASSIGNING <lv_offset>.
      ASSERT sy-subrc = 0.
      READ TABLE mt_symbol INDEX <lv_offset> + 1 ASSIGNING <lv_symbol>.
      ASSERT sy-subrc = 0.
      <lv_symbol> = lv_index - 1.
      <lv_offset> = <lv_offset> + 1.
    ENDDO.

    READ TABLE mt_count INDEX 1 INTO mv_bit01.            "#EC CI_SUBRC
    READ TABLE mt_count INDEX 2 INTO mv_bit02.            "#EC CI_SUBRC
    READ TABLE mt_count INDEX 3 INTO mv_bit03.            "#EC CI_SUBRC
    READ TABLE mt_count INDEX 4 INTO mv_bit04.            "#EC CI_SUBRC
    READ TABLE mt_count INDEX 5 INTO mv_bit05.            "#EC CI_SUBRC
    READ TABLE mt_count INDEX 6 INTO mv_bit06.            "#EC CI_SUBRC
    READ TABLE mt_count INDEX 7 INTO mv_bit07.            "#EC CI_SUBRC
    READ TABLE mt_count INDEX 8 INTO mv_bit08.            "#EC CI_SUBRC
    READ TABLE mt_count INDEX 9 INTO mv_bit09.            "#EC CI_SUBRC
    READ TABLE mt_count INDEX 10 INTO mv_bit10.           "#EC CI_SUBRC
    READ TABLE mt_count INDEX 11 INTO mv_bit11.           "#EC CI_SUBRC
    READ TABLE mt_count INDEX 12 INTO mv_bit12.           "#EC CI_SUBRC
    READ TABLE mt_count INDEX 13 INTO mv_bit13.           "#EC CI_SUBRC
    READ TABLE mt_count INDEX 14 INTO mv_bit14.           "#EC CI_SUBRC
    READ TABLE mt_count INDEX 15 INTO mv_bit15.           "#EC CI_SUBRC

  ENDMETHOD.


  METHOD get_count.
    " This method is on hot path so we use instance variables to avoid reading mt_count
    CASE iv_index.
      WHEN 1.
        rv_value = mv_bit01.
      WHEN 2.
        rv_value = mv_bit02.
      WHEN 3.
        rv_value = mv_bit03.
      WHEN 4.
        rv_value = mv_bit04.
      WHEN 5.
        rv_value = mv_bit05.
      WHEN 6.
        rv_value = mv_bit06.
      WHEN 7.
        rv_value = mv_bit07.
      WHEN 8.
        rv_value = mv_bit08.
      WHEN 9.
        rv_value = mv_bit09.
      WHEN 10.
        rv_value = mv_bit10.
      WHEN 11.
        rv_value = mv_bit11.
      WHEN 12.
        rv_value = mv_bit12.
      WHEN 13.
        rv_value = mv_bit13.
      WHEN 14.
        rv_value = mv_bit14.
      WHEN 15.
        rv_value = mv_bit15.
      WHEN OTHERS.
        rv_value = 0.
    ENDCASE.
  ENDMETHOD.


  METHOD get_symbol.
    READ TABLE mt_symbol INDEX iv_index INTO rv_value.    "#EC CI_SUBRC
  ENDMETHOD.
ENDCLASS.
