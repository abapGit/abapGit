*----------------------------------------------------------------------*
*       CLASS ltcl_convert DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_convert DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.

  PRIVATE SECTION.
    METHODS convert_int FOR TESTING RAISING zcx_abapgit_exception.
    METHODS split_string FOR TESTING.

ENDCLASS.                    "ltcl_convert DEFINITION

*----------------------------------------------------------------------*
*       CLASS ltcl_convert IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_convert IMPLEMENTATION.

  METHOD convert_int.

    DATA: lv_xstring TYPE xstring,
          lv_input   TYPE i,
          lv_result  TYPE i.


    DO 1000 TIMES.
      lv_input = sy-index.
      lv_xstring = zcl_abapgit_convert=>int_to_xstring4( lv_input ).
      lv_result = zcl_abapgit_convert=>xstring_to_int( lv_xstring ).

      cl_abap_unit_assert=>assert_equals(
          exp = lv_input
          act = lv_result ).
    ENDDO.

  ENDMETHOD.                    "convert_int

  METHOD split_string.

    DATA: lt_act TYPE string_table,
          lt_exp TYPE string_table.

    APPEND 'ABC' TO lt_exp.
    APPEND '123' TO lt_exp.

    " Case 1. String separated by CRLF
    lt_act = zcl_abapgit_convert=>split_string( 'ABC' && cl_abap_char_utilities=>cr_lf && '123' ).

    cl_abap_unit_assert=>assert_equals( exp = lt_exp
                                        act = lt_act
                                        msg = ' Error during string split: CRLF' ).

    CLEAR: lt_act.

    " Case 2. String separated by LF
    lt_act = zcl_abapgit_convert=>split_string( 'ABC' && cl_abap_char_utilities=>newline && '123' ).

    cl_abap_unit_assert=>assert_equals( exp = lt_exp
                                        act = lt_act
                                        msg = ' Error during string split: LF' ).

  ENDMETHOD.                    "split_string.

ENDCLASS.                    "ltcl_convert IMPLEMENTATION
