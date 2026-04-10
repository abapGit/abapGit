
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
    METHODS test_byte_00 FOR TESTING RAISING cx_static_check.
    METHODS test_byte_ff FOR TESTING RAISING cx_static_check.
    METHODS test_empty FOR TESTING RAISING cx_static_check.

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

    CREATE OBJECT lo_stream
      EXPORTING
        iv_data = ''.

* hmm, yea, this is seems to be the behavior
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

ENDCLASS.
