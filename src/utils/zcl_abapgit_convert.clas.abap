CLASS zcl_abapgit_convert DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS bitbyte_to_int
      IMPORTING
        !iv_bits      TYPE clike
      RETURNING
        VALUE(rv_int) TYPE i .
    CLASS-METHODS x_to_bitbyte
      IMPORTING
        !iv_x             TYPE x
      RETURNING
        VALUE(rv_bitbyte) TYPE zif_abapgit_git_definitions=>ty_bitbyte .
    CLASS-METHODS string_to_xstring_utf8
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS xstring_to_string_utf8
      IMPORTING
        !iv_data         TYPE xsequence
        !iv_length       TYPE i OPTIONAL
      RETURNING
        VALUE(rv_string) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS string_to_xstring_utf8_bom
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS xstring_to_int
      IMPORTING
        !iv_xstring TYPE xstring
      RETURNING
        VALUE(rv_i) TYPE i
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS int_to_xstring4
      IMPORTING
        !iv_i             TYPE i
      RETURNING
        VALUE(rv_xstring) TYPE xstring .
    CLASS-METHODS split_string
      IMPORTING
        !iv_string      TYPE string
      RETURNING
        VALUE(rt_lines) TYPE string_table .
    CLASS-METHODS conversion_exit_isola_output
      IMPORTING
        !iv_spras       TYPE spras
      RETURNING
        VALUE(rv_spras) TYPE laiso .
    CLASS-METHODS string_to_xstring
      IMPORTING
        !iv_str        TYPE string
      RETURNING
        VALUE(rv_xstr) TYPE xstring
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS string_to_tab
      IMPORTING
        !iv_str  TYPE string
      EXPORTING
        !ev_size TYPE i
        !et_tab  TYPE STANDARD TABLE .
    CLASS-METHODS base64_to_xstring
      IMPORTING
        !iv_base64     TYPE string
      RETURNING
        VALUE(rv_xstr) TYPE xstring .
    CLASS-METHODS xstring_to_bintab
      IMPORTING
        !iv_xstr   TYPE xstring
      EXPORTING
        !ev_size   TYPE i
        !et_bintab TYPE STANDARD TABLE .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA go_convert_out TYPE REF TO cl_abap_conv_out_ce .
    CLASS-DATA go_convert_in TYPE REF TO cl_abap_conv_in_ce .

    CLASS-METHODS xstring_remove_bom
      IMPORTING
        iv_xstr        TYPE xsequence
      RETURNING
        VALUE(rv_xstr) TYPE xstring.
ENDCLASS.



CLASS zcl_abapgit_convert IMPLEMENTATION.


  METHOD base64_to_xstring.

    rv_xstr = cl_http_utility=>decode_x_base64( iv_base64 ).

  ENDMETHOD.


  METHOD bitbyte_to_int.

    DATA: lv_bitbyte TYPE string,
          lv_len     TYPE i,
          lv_offset  TYPE i.

    lv_bitbyte = iv_bits.
    SHIFT lv_bitbyte LEFT DELETING LEADING '0 '.
    lv_len     = strlen( lv_bitbyte ).
    lv_offset  = lv_len - 1.

    rv_int = 0.
    DO lv_len TIMES.

      IF sy-index = 1.
        "Intialize
        IF lv_bitbyte+lv_offset(1) = '1'.
          rv_int = 1.
        ENDIF.
      ELSEIF lv_bitbyte+lv_offset(1) = '1'.
        rv_int = rv_int + ( 2 ** ( sy-index - 1 ) ).
      ENDIF.

      lv_offset = lv_offset - 1. "Move Cursor

    ENDDO.

  ENDMETHOD.


  METHOD conversion_exit_isola_output.

    cl_gdt_conversion=>language_code_outbound(
      EXPORTING
        im_value = iv_spras
      IMPORTING
        ex_value = rv_spras ).

    TRANSLATE rv_spras TO UPPER CASE.

  ENDMETHOD.


  METHOD int_to_xstring4.
* returns xstring of length 4 containing the integer value iv_i

    DATA lv_x TYPE x LENGTH 4.

    lv_x = iv_i.
    rv_xstring = lv_x.

  ENDMETHOD.


  METHOD split_string.

    FIND FIRST OCCURRENCE OF cl_abap_char_utilities=>cr_lf IN iv_string.

    " Convert string into table depending on separator type CR_LF vs. LF
    IF sy-subrc = 0.
      SPLIT iv_string AT cl_abap_char_utilities=>cr_lf INTO TABLE rt_lines.
    ELSE.
      SPLIT iv_string AT cl_abap_char_utilities=>newline INTO TABLE rt_lines.
    ENDIF.

  ENDMETHOD.


  METHOD string_to_tab.

    DATA lv_length TYPE i.
    DATA lv_iterations TYPE i.
    DATA lv_offset TYPE i.

    FIELD-SYMBOLS <lg_line> TYPE any.


    CLEAR et_tab.
    ev_size = strlen( iv_str ).

    APPEND INITIAL LINE TO et_tab ASSIGNING <lg_line>.
    <lg_line> = iv_str.
    DESCRIBE FIELD <lg_line> LENGTH lv_length IN CHARACTER MODE.
    lv_iterations = ev_size DIV lv_length.

    DO lv_iterations TIMES.
      lv_offset = sy-index * lv_length.
      APPEND INITIAL LINE TO et_tab ASSIGNING <lg_line>.
      <lg_line> = iv_str+lv_offset.
    ENDDO.

  ENDMETHOD.


  METHOD string_to_xstring.

    rv_xstr = string_to_xstring_utf8( iv_str ).

  ENDMETHOD.


  METHOD string_to_xstring_utf8.

    DATA lx_error TYPE REF TO cx_root.

    TRY.
        IF go_convert_out IS INITIAL.
          go_convert_out = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
        ENDIF.

        go_convert_out->convert(
          EXPORTING
            data   = iv_string
          IMPORTING
            buffer = rv_xstring ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD string_to_xstring_utf8_bom.

    IF iv_string IS INITIAL.
      RETURN.
    ENDIF.

    rv_xstring = string_to_xstring_utf8( iv_string ).

    " Add UTF-8 BOM
    IF xstrlen( rv_xstring ) < 3 OR rv_xstring(3) <> cl_abap_char_utilities=>byte_order_mark_utf8.
      rv_xstring = cl_abap_char_utilities=>byte_order_mark_utf8 && rv_xstring.
    ENDIF.

  ENDMETHOD.


  METHOD xstring_remove_bom.

    rv_xstr = iv_xstr.

    " cl_abap_conv_in_ce does not handle BOM in non-Unicode systems, so we remove it
    IF cl_abap_char_utilities=>charsize = 1 AND xstrlen( rv_xstr ) > 3
        AND rv_xstr(3) = cl_abap_char_utilities=>byte_order_mark_utf8.

      rv_xstr = rv_xstr+3.

    ENDIF.

  ENDMETHOD.


  METHOD xstring_to_bintab.

    DATA lv_length TYPE i.
    DATA lv_iterations TYPE i.
    DATA lv_offset TYPE i.

    FIELD-SYMBOLS <lg_line> TYPE any.


    CLEAR et_bintab.
    ev_size = xstrlen( iv_xstr ).

    APPEND INITIAL LINE TO et_bintab ASSIGNING <lg_line>.
    <lg_line> = iv_xstr.
    DESCRIBE FIELD <lg_line> LENGTH lv_length IN BYTE MODE.
    lv_iterations = ev_size DIV lv_length.

    DO lv_iterations TIMES.
      lv_offset = sy-index * lv_length.
      APPEND INITIAL LINE TO et_bintab ASSIGNING <lg_line>.
      <lg_line> = iv_xstr+lv_offset.
    ENDDO.

  ENDMETHOD.


  METHOD xstring_to_int.

* use the built-in type conversion
    rv_i = iv_xstring.

  ENDMETHOD.


  METHOD xstring_to_string_utf8.

    DATA lx_error  TYPE REF TO cx_root.
    DATA lv_data TYPE xstring.
    DATA lv_length TYPE i.

    " Remove BOM for non-Unicode systems
    lv_data = xstring_remove_bom( iv_data ).

    lv_length = iv_length.
    IF lv_length <= 0.
      lv_length = xstrlen( lv_data ).
    ENDIF.

    TRY.
        IF go_convert_in IS INITIAL.
          go_convert_in = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
        ENDIF.

        go_convert_in->convert(
          EXPORTING
            input = lv_data
            n     = lv_length
          IMPORTING
            data  = rv_string ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.


  METHOD x_to_bitbyte.

    CLEAR rv_bitbyte.

    GET BIT 1 OF iv_x INTO rv_bitbyte+0(1).
    GET BIT 2 OF iv_x INTO rv_bitbyte+1(1).
    GET BIT 3 OF iv_x INTO rv_bitbyte+2(1).
    GET BIT 4 OF iv_x INTO rv_bitbyte+3(1).
    GET BIT 5 OF iv_x INTO rv_bitbyte+4(1).
    GET BIT 6 OF iv_x INTO rv_bitbyte+5(1).
    GET BIT 7 OF iv_x INTO rv_bitbyte+6(1).
    GET BIT 8 OF iv_x INTO rv_bitbyte+7(1).

  ENDMETHOD.
ENDCLASS.
