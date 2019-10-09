
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    DATA:
      mo_cut TYPE REF TO zcl_abapgit_zlib_convert.

    METHODS:
      setup,
      bits_to_int FOR TESTING,
      hex_to_bits FOR TESTING,
      int_to_hex FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_cut.
  ENDMETHOD.

  METHOD bits_to_int.

    DATA: lv_result TYPE i.

    lv_result = mo_cut->bits_to_int( '111' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 7 ).

  ENDMETHOD.

  METHOD hex_to_bits.

    DATA: lv_bits TYPE string.

    lv_bits = mo_cut->hex_to_bits( '0101' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_bits
      exp = '0000000100000001' ).

  ENDMETHOD.

  METHOD int_to_hex.

    DATA: lv_hex TYPE xstring.

    lv_hex = mo_cut->int_to_hex( 64 ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_hex
      exp = '40' ).

  ENDMETHOD.

ENDCLASS.
