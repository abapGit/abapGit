CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_abapgit_zlib_stream DEFINITION LOCAL FRIENDS ltcl_test.

CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
    METHODS test_byte_00 FOR TESTING RAISING cx_static_check.
    METHODS test_byte_ff FOR TESTING RAISING cx_static_check.
    METHODS test_empty FOR TESTING RAISING cx_static_check.
    METHODS test_take_bit_vs_take_bits FOR TESTING RAISING cx_static_check.
    METHODS test_take_bit_cross_byte FOR TESTING RAISING cx_static_check.
    METHODS test_take_bit_last_clears FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD test.

    DATA: lo_stream    TYPE REF TO zcl_abapgit_zlib_stream,
          lv_remaining TYPE i,
          lv_int       TYPE i,
          lv_bits      TYPE string,
          lv_bytes     TYPE xstring.


    CREATE OBJECT lo_stream
      EXPORTING
        iv_data = '112233445566'.

    lv_bits = lo_stream->take_bits( 8 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_bits
      exp = '00010001' ).

    lv_remaining = lo_stream->remaining( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_remaining
      exp = 6 ).

    lv_int = lo_stream->take_int( 8 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_int
      exp = 34 ).

    lv_bytes = lo_stream->take_bytes( 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_bytes
      exp = '3344' ).

  ENDMETHOD.

  METHOD test_empty.

    DATA lo_stream TYPE REF TO zcl_abapgit_zlib_stream.
    DATA lv_xstr TYPE xstring.

    CREATE OBJECT lo_stream
      EXPORTING
        iv_data = lv_xstr.

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->remaining( )
      exp = 1 ).

  ENDMETHOD.

  METHOD test_byte_00.

    DATA lo_stream TYPE REF TO zcl_abapgit_zlib_stream.

    CREATE OBJECT lo_stream
      EXPORTING
        iv_data = '00'.

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->take_bits( 8 )
      exp = '00000000' ).

    CREATE OBJECT lo_stream
      EXPORTING
        iv_data = '00'.

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->take_int( 8 )
      exp = 0 ).

    CREATE OBJECT lo_stream
      EXPORTING
        iv_data = '00'.

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->take_bytes( 1 )
      exp = '00' ).

  ENDMETHOD.

  METHOD test_byte_ff.

    DATA lo_stream TYPE REF TO zcl_abapgit_zlib_stream.

    CREATE OBJECT lo_stream
      EXPORTING
        iv_data = 'FF'.

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->take_bits( 8 )
      exp = '11111111' ).

    CREATE OBJECT lo_stream
      EXPORTING
        iv_data = 'FF'.

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->take_int( 8 )
      exp = 255 ).

    CREATE OBJECT lo_stream
      EXPORTING
        iv_data = 'FF'.

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->take_bytes( 1 )
      exp = 'FF' ).

  ENDMETHOD.

  METHOD test_take_bit_vs_take_bits.

    DATA lo_stream1 TYPE REF TO zcl_abapgit_zlib_stream.
    DATA lo_stream2 TYPE REF TO zcl_abapgit_zlib_stream.
    DATA lv_bit     TYPE i.
    DATA lv_bits    TYPE string.

    " A5 = 10100101, mix of 0s and 1s across a full byte
    CREATE OBJECT lo_stream1 EXPORTING iv_data = 'A5'.
    CREATE OBJECT lo_stream2 EXPORTING iv_data = 'A5'.

    DO 8 TIMES.
      lv_bit  = lo_stream1->take_bit( ).
      lv_bits = lo_stream2->take_bits( 1 ).

      cl_abap_unit_assert=>assert_equals(
        act = lv_bit
        exp = lv_bits
        msg = |Mismatch at bit { sy-index }| ).

      cl_abap_unit_assert=>assert_equals(
        act = lo_stream1->remaining( )
        exp = lo_stream2->remaining( ) ).

      cl_abap_unit_assert=>assert_equals(
        act = lo_stream1->mv_bits
        exp = lo_stream2->mv_bits ).
    ENDDO.

  ENDMETHOD.

  METHOD test_take_bit_cross_byte.

* Regression test: take_bit must work across byte boundaries.
* After consuming all 8 bits of a byte, take_bit uses mv_bits(0)
* instead of CLEAR mv_bits. In native ABAP, mv_bits(0) may not
* produce a truly initial string, preventing the next byte from
* being loaded on the subsequent call.

    DATA lo_stream1 TYPE REF TO zcl_abapgit_zlib_stream.
    DATA lo_stream2 TYPE REF TO zcl_abapgit_zlib_stream.
    DATA lv_bit     TYPE i.
    DATA lv_bits    TYPE string.

    " Two bytes: A5 3C = 10100101 00111100
    " Reading 16 bits one at a time must cross the byte boundary
    CREATE OBJECT lo_stream1 EXPORTING iv_data = 'A53C'.
    CREATE OBJECT lo_stream2 EXPORTING iv_data = 'A53C'.

    DO 16 TIMES.
      cl_abap_unit_assert=>assert_equals(
        act = lo_stream1->remaining( )
        exp = lo_stream2->remaining( ) ).

      cl_abap_unit_assert=>assert_equals(
        act = lo_stream1->mv_bits
        exp = lo_stream2->mv_bits ).

      lv_bit  = lo_stream1->take_bit( ).
      lv_bits = lo_stream2->take_bits( 1 ).

      cl_abap_unit_assert=>assert_equals(
        act = lv_bit
        exp = lv_bits
        msg = |Mismatch at bit { sy-index }| ).

      cl_abap_unit_assert=>assert_equals(
        act = lo_stream1->remaining( )
        exp = lo_stream2->remaining( ) ).

      cl_abap_unit_assert=>assert_equals(
        act = lo_stream1->mv_bits
        exp = lo_stream2->mv_bits ).
    ENDDO.

  ENDMETHOD.

  METHOD test_take_bit_last_clears.

* Regression: take_bit must leave mv_bits initial after consuming
* the last bit of a byte. Without an explicit CLEAR, the expression
* mv_bits = mv_bits(0) can produce a non-initial value in native
* ABAP, preventing the next byte from being loaded.

    DATA lo_stream TYPE REF TO zcl_abapgit_zlib_stream.

    CREATE OBJECT lo_stream EXPORTING iv_data = 'A53C'.

    " Consume all 8 bits of the first byte
    DO 8 TIMES.
      lo_stream->take_bit( ).
    ENDDO.

    " After 8 bits, mv_bits must be initial so the next call loads byte 2
    cl_abap_unit_assert=>assert_initial(
      act = lo_stream->mv_bits
      msg = |mv_bits must be initial after consuming all 8 bits| ).

    " Verify the 9th bit comes from byte 2 (0x3C = 00111100)
    " Bits are read right-to-left, so first bit of 0x3C is 0
    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->take_bit( )
      exp = 0
      msg = |9th bit must come from second byte| ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_stream->mv_bits
      exp = '0011110'
      msg = |mv_bits must contain remaining 7 bits of second byte| ).

  ENDMETHOD.

ENDCLASS.
