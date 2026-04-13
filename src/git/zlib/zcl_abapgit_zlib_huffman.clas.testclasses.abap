CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS create1 FOR TESTING RAISING cx_static_check.
    METHODS create2 FOR TESTING RAISING cx_static_check.
    METHODS create3 FOR TESTING RAISING cx_static_check.
    METHODS create4 FOR TESTING RAISING cx_static_check.
    METHODS create_with_zeros FOR TESTING RAISING cx_static_check.
    METHODS get_count_boundaries FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD create1.

    DATA lo_huffman TYPE REF TO zcl_abapgit_zlib_huffman.
    DATA lt_lengths TYPE zcl_abapgit_zlib_huffman=>ty_lengths.


    DO 10 TIMES.
      APPEND 8 TO lt_lengths.
    ENDDO.

    CREATE OBJECT lo_huffman
      EXPORTING
        it_lengths = lt_lengths.

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_count( 1 )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_count( 8 )
      exp = 10 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_symbol( 1 )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_symbol( 10 )
      exp = 9 ).

  ENDMETHOD.

  METHOD create2.

    DATA lo_huffman TYPE REF TO zcl_abapgit_zlib_huffman.
    DATA lt_lengths TYPE zcl_abapgit_zlib_huffman=>ty_lengths.


    DO 144 TIMES.
      APPEND 8 TO lt_lengths.
    ENDDO.
    DO 112 TIMES.
      APPEND 9 TO lt_lengths.
    ENDDO.
    DO 24 TIMES.
      APPEND 7 TO lt_lengths.
    ENDDO.
    DO 8 TIMES.
      APPEND 8 TO lt_lengths.
    ENDDO.

    CREATE OBJECT lo_huffman
      EXPORTING
        it_lengths = lt_lengths.

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_count( 8 )
      exp = 152 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_count( 15 )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_symbol( 1 )
      exp = 256 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_symbol( 288 )
      exp = 255 ).

  ENDMETHOD.

  METHOD create3.
    " Minimal tree: two symbols both at length 1
    DATA lo_huffman TYPE REF TO zcl_abapgit_zlib_huffman.
    DATA lt_lengths TYPE zcl_abapgit_zlib_huffman=>ty_lengths.

    APPEND 1 TO lt_lengths.
    APPEND 1 TO lt_lengths.

    CREATE OBJECT lo_huffman
      EXPORTING
        it_lengths = lt_lengths.

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_count( 1 )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_count( 2 )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_symbol( 1 )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_symbol( 2 )
      exp = 1 ).

  ENDMETHOD.

  METHOD create4.
    " Mixed lengths [1, 2, 3, 3]: one symbol at each of bits 1 and 2, two at bit 3
    DATA lo_huffman TYPE REF TO zcl_abapgit_zlib_huffman.
    DATA lt_lengths TYPE zcl_abapgit_zlib_huffman=>ty_lengths.

    APPEND 1 TO lt_lengths.
    APPEND 2 TO lt_lengths.
    APPEND 3 TO lt_lengths.
    APPEND 3 TO lt_lengths.

    CREATE OBJECT lo_huffman
      EXPORTING
        it_lengths = lt_lengths.

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_count( 1 )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_count( 2 )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_count( 3 )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_count( 4 )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_symbol( 1 )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_symbol( 2 )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_symbol( 3 )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_symbol( 4 )
      exp = 3 ).

  ENDMETHOD.

  METHOD create_with_zeros.
    " Symbols with length 0 are excluded; only symbols 1 and 3 are coded
    DATA lo_huffman TYPE REF TO zcl_abapgit_zlib_huffman.
    DATA lt_lengths TYPE zcl_abapgit_zlib_huffman=>ty_lengths.

    APPEND 0 TO lt_lengths.
    APPEND 1 TO lt_lengths.
    APPEND 0 TO lt_lengths.
    APPEND 1 TO lt_lengths.

    CREATE OBJECT lo_huffman
      EXPORTING
        it_lengths = lt_lengths.

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_count( 1 )
      exp = 2 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_symbol( 1 )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_symbol( 2 )
      exp = 3 ).

  ENDMETHOD.

  METHOD get_count_boundaries.
    " Indices outside 1..15 should return 0 via the OTHERS branch
    DATA lo_huffman TYPE REF TO zcl_abapgit_zlib_huffman.
    DATA lt_lengths TYPE zcl_abapgit_zlib_huffman=>ty_lengths.

    APPEND 8 TO lt_lengths.

    CREATE OBJECT lo_huffman
      EXPORTING
        it_lengths = lt_lengths.

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_count( 0 )
      exp = 0 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_huffman->get_count( 16 )
      exp = 0 ).

  ENDMETHOD.

ENDCLASS.
