CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS create1 FOR TESTING RAISING cx_static_check.
    METHODS create2 FOR TESTING RAISING cx_static_check.
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


  ENDMETHOD.

ENDCLASS.
