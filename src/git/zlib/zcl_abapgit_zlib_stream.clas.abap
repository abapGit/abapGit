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
ENDCLASS.



CLASS ZCL_ABAPGIT_ZLIB_STREAM IMPLEMENTATION.


  METHOD clear_bits.
    CLEAR mv_bits.
  ENDMETHOD.


  METHOD constructor.

    mv_compressed = iv_data.

  ENDMETHOD.


  METHOD remaining.

    rv_length = xstrlen( mv_compressed ) + 1.

  ENDMETHOD.


  METHOD take_bits.

    DATA: lv_left  TYPE i,
          lv_index TYPE i,
          lv_x     TYPE x LENGTH 1.


    WHILE strlen( rv_bits ) < iv_length.
      IF mv_bits IS INITIAL.
        lv_x = mv_compressed(1).
        mv_bits = zcl_abapgit_zlib_convert=>hex_to_bits( lv_x ).
        mv_compressed = mv_compressed+1.
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

    rv_bytes = mv_compressed(iv_length).
    mv_compressed = mv_compressed+iv_length.

  ENDMETHOD.


  METHOD take_int.

    rv_int = zcl_abapgit_zlib_convert=>bits_to_int( take_bits( iv_length ) ).

  ENDMETHOD.
ENDCLASS.
