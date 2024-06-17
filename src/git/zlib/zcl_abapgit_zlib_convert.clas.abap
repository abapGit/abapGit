CLASS zcl_abapgit_zlib_convert DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-METHODS hex_to_bits
      IMPORTING
        !iv_hex        TYPE xsequence
      RETURNING
        VALUE(rv_bits) TYPE string.

    CLASS-METHODS bits_to_int
      IMPORTING
        !iv_bits      TYPE clike
      RETURNING
        VALUE(rv_int) TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_ZLIB_CONVERT IMPLEMENTATION.


  METHOD bits_to_int.

    DATA lv_i      TYPE i.
    DATA lv_offset TYPE i.

    DO strlen( iv_bits ) TIMES.
      lv_i = iv_bits+lv_offset(1).
      rv_int = rv_int * 2 + lv_i.
      lv_offset = lv_offset + 1.
    ENDDO.

  ENDMETHOD.


  METHOD hex_to_bits.

    DATA: lv_x   TYPE x LENGTH 1,
          lv_c   TYPE c LENGTH 1,
          lv_bit TYPE i,
          lv_hex TYPE xstring.


    lv_hex = iv_hex.
    WHILE NOT lv_hex IS INITIAL.
      lv_x = lv_hex.
      DO 8 TIMES.
        lv_bit = sy-index.
        GET BIT lv_bit OF lv_x INTO lv_c.
        CONCATENATE rv_bits lv_c INTO rv_bits.
      ENDDO.
      lv_hex = lv_hex+1.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.
