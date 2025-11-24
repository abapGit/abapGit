CLASS zcl_abapgit_zlib_stream DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_data TYPE xstring .
    METHODS take_bits
      IMPORTING
        !iv_length     TYPE i
      RETURNING
        VALUE(rv_bits) TYPE string .
    METHODS take_int
      IMPORTING
        !iv_length    TYPE i
      RETURNING
        VALUE(rv_int) TYPE i .
    METHODS remaining
      RETURNING
        VALUE(rv_length) TYPE i .
    "! Take bytes, there's an implicit realignment to start at the beginning of a byte
    "! i.e. if next bit of current byte is not the first bit, then this byte is skipped
    "! and the bytes are taken from the next one.
    "! @parameter iv_length | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rv_bytes | <p class="shorttext synchronized" lang="en"></p>
    METHODS take_bytes
      IMPORTING
        !iv_length      TYPE i
      RETURNING
        VALUE(rv_bytes) TYPE xstring .
    METHODS clear_bits .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_bits TYPE string .
    DATA mv_compressed TYPE xstring .
    DATA mv_offset TYPE i.
ENDCLASS.



CLASS zcl_abapgit_zlib_stream IMPLEMENTATION.


  METHOD clear_bits.
    CLEAR mv_bits.
  ENDMETHOD.


  METHOD constructor.

    mv_compressed = iv_data.
    mv_offset     = 0.

  ENDMETHOD.


  METHOD remaining.

    rv_length = xstrlen( mv_compressed ) + 1 - mv_offset.

  ENDMETHOD.


  METHOD take_bits.

    DATA:
      lv_left  TYPE i,
      lv_index TYPE i,
      lv_c     TYPE c,
      lv_x     TYPE x LENGTH 1.

    WHILE strlen( rv_bits ) < iv_length.
      IF mv_bits IS INITIAL.
        lv_x = mv_compressed+mv_offset(1).

        " inlining hex_to_bits for better performance
        DO 8 TIMES.
          GET BIT sy-index OF lv_x INTO lv_c.
          CONCATENATE mv_bits lv_c INTO mv_bits.
        ENDDO.

        mv_offset = mv_offset + 1.
      ENDIF.
      lv_left = iv_length - strlen( rv_bits ).
      IF lv_left >= strlen( mv_bits ).
        CONCATENATE mv_bits rv_bits INTO rv_bits.
        CLEAR mv_bits.
      ELSE.
        lv_index = strlen( mv_bits ) - lv_left.
        CONCATENATE mv_bits+lv_index(lv_left) rv_bits INTO rv_bits.
        mv_bits = mv_bits(lv_index).
      ENDIF.

    ENDWHILE.

  ENDMETHOD.


  METHOD take_bytes.

    rv_bytes = mv_compressed+mv_offset(iv_length).
    mv_offset = mv_offset + iv_length.

  ENDMETHOD.


  METHOD take_int.

    DATA:
      lv_bits   TYPE string,
      lv_i      TYPE i,
      lv_offset TYPE i.

    lv_bits = take_bits( iv_length ).

    " inlining bits_to_int for better performance
    DO strlen( lv_bits ) TIMES.
      lv_i = lv_bits+lv_offset(1).
      rv_int = rv_int * 2 + lv_i.
      lv_offset = lv_offset + 1.
    ENDDO.

  ENDMETHOD.
ENDCLASS.
