
CLASS ltcl_zlib DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      decompress FOR TESTING RAISING cx_dynamic_check.

ENDCLASS.


CLASS ltcl_zlib IMPLEMENTATION.

  METHOD decompress.

    DATA: ls_data TYPE zcl_abapgit_zlib=>ty_decompress.

    CONSTANTS:
      lc_raw        TYPE xstring VALUE '48656C6C6F20576F726C64210D0A',
      lc_compressed TYPE xstring VALUE 'F348CDC9C95708CF2FCA4951E4E5020024E90455'.


    ls_data = zcl_abapgit_zlib=>decompress( lc_compressed ).

    cl_abap_unit_assert=>assert_not_initial( ls_data-raw ).
    cl_abap_unit_assert=>assert_equals( act = ls_data-raw
                                        exp = lc_raw ).

  ENDMETHOD.

ENDCLASS.
