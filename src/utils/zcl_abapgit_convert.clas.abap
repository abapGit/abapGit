class ZCL_ABAPGIT_CONVERT definition
  public
  create public .

public section.

  class-methods BITBYTE_TO_INT
    importing
      !IV_BITS type CLIKE
    returning
      value(RV_INT) type I .
  class-methods X_TO_BITBYTE
    importing
      !IV_X type X
    returning
      value(RV_BITBYTE) type ZIF_ABAPGIT_DEFINITIONS=>TY_BITBYTE .
  class-methods STRING_TO_XSTRING_UTF8
    importing
      !IV_STRING type STRING
    returning
      value(RV_XSTRING) type XSTRING .
  class-methods XSTRING_TO_STRING_UTF8
    importing
      !IV_DATA type XSEQUENCE
    returning
      value(RV_STRING) type STRING .
  class-methods STRING_TO_XSTRING_UTF8_BOM
    importing
      !IV_STRING type STRING
    returning
      value(RV_XSTRING) type XSTRING .
  class-methods XSTRING_TO_INT
    importing
      !IV_XSTRING type XSTRING
    returning
      value(RV_I) type I
    raising
      ZCX_ABAPGIT_EXCEPTION .
  class-methods INT_TO_XSTRING4
    importing
      !IV_I type I
    returning
      value(RV_XSTRING) type XSTRING .
  class-methods SPLIT_STRING
    importing
      !IV_STRING type STRING
    returning
      value(RT_LINES) type STRING_TABLE .
  class-methods CONVERSION_EXIT_ISOLA_OUTPUT
    importing
      !IV_SPRAS type SPRAS
    returning
      value(RV_SPRAS) type LAISO .
  class-methods ALPHA_OUTPUT
    importing
      !IV_VAL type CLIKE
    returning
      value(RV_STR) type STRING .
  class-methods STRING_TO_XSTRING
    importing
      !IV_STR type STRING
    returning
      value(RV_XSTR) type XSTRING .
  class-methods STRING_TO_TAB
    importing
      !IV_STR type STRING
    exporting
      value(ET_TAB) type STANDARD TABLE .
  class-methods BASE64_TO_XSTRING
    importing
      !IV_BASE64 type STRING
    returning
      value(RV_XSTR) type XSTRING .
  class-methods BINTAB_TO_XSTRING
    importing
      !IT_BINTAB type LVC_T_MIME
      !IV_SIZE type I
    returning
      value(RV_XSTR) type XSTRING .
  class-methods XSTRING_TO_BINTAB
    importing
      !IV_XSTR type XSTRING
    exporting
      !EV_SIZE type I
      !ET_BINTAB type LVC_T_MIME .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_CONVERT IMPLEMENTATION.


  METHOD alpha_output.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = iv_val
      IMPORTING
        output = rv_str.

    CONDENSE rv_str.

  ENDMETHOD.


  METHOD base64_to_xstring.

    CALL FUNCTION 'SSFC_BASE64_DECODE'
      EXPORTING
        b64data = iv_base64
      IMPORTING
        bindata = rv_xstr
      EXCEPTIONS
        OTHERS  = 1.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD bintab_to_xstring.

    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = iv_size
      IMPORTING
        buffer       = rv_xstr
      TABLES
        binary_tab   = it_bintab
      EXCEPTIONS
        failed       = 1 ##FM_SUBRC_OK.
    ASSERT sy-subrc = 0.

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

    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
      EXPORTING
        input  = iv_spras
      IMPORTING
        output = rv_spras.

  ENDMETHOD.


  METHOD int_to_xstring4.
* returns xstring of length 4 containing the integer value iv_i

    DATA: lv_x TYPE x LENGTH 4.


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

    CLEAR et_tab[].
    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = iv_str
*     IMPORTING
*         LENGTH    = LENGTH
      TABLES
        ftext_tab = et_tab.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD string_to_xstring.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = iv_str
      IMPORTING
        buffer = rv_xstr
      EXCEPTIONS
        OTHERS = 1.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD string_to_xstring_utf8.

    DATA: lo_obj TYPE REF TO cl_abap_conv_out_ce.


    TRY.
        lo_obj = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).

        lo_obj->convert( EXPORTING data = iv_string
                         IMPORTING buffer = rv_xstring ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.                  "#EC NO_HANDLER
    ENDTRY.

  ENDMETHOD.


  METHOD string_to_xstring_utf8_bom.

    DATA: lv_hex     TYPE x LENGTH 1 VALUE '23',
          lv_hex_bom TYPE x LENGTH 3 VALUE 'EFBBBF'.

    rv_xstring = string_to_xstring_utf8( iv_string ).

    "unicode systems always add the byte order mark to the xml, while non-unicode does not
    "in class ZCL_ABAPGIT_XML~TO_XML byte order mark was added to XML as #
    "In non-unicode systems zcl_abapgit_convert=>xstring_to_string_utf8( cl_abap_char_utilities=>byte_order_mark_utf8 )
    "has result # as HEX 23 and not HEX EFBBBF.
    "So we have to remove 23 first and add EFBBBF after to serialized string
    IF rv_xstring(3) <> cl_abap_char_utilities=>byte_order_mark_utf8
    AND rv_xstring(1) = lv_hex.
      REPLACE FIRST OCCURRENCE
        OF lv_hex IN rv_xstring WITH lv_hex_bom IN BYTE MODE.
      ASSERT sy-subrc = 0.
    ENDIF.

  ENDMETHOD.


  METHOD xstring_to_bintab.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = iv_xstr
      IMPORTING
        output_length = ev_size
      TABLES
        binary_tab    = et_bintab.

  ENDMETHOD.


  METHOD xstring_to_int.

    DATA: lv_xstring TYPE xstring,
          lv_x       TYPE x.


    lv_xstring = iv_xstring.
    WHILE xstrlen( lv_xstring ) > 0.
      lv_x = lv_xstring(1).
      rv_i = rv_i * 256 + lv_x.
      lv_xstring = lv_xstring+1.
    ENDWHILE.

  ENDMETHOD.


  METHOD xstring_to_string_utf8.

    DATA: lv_len TYPE i,
          lo_obj TYPE REF TO cl_abap_conv_in_ce.


    TRY.
        lo_obj = cl_abap_conv_in_ce=>create(
            input    = iv_data
            encoding = 'UTF-8' ).
        lv_len = xstrlen( iv_data ).

        lo_obj->read( EXPORTING n    = lv_len
                      IMPORTING data = rv_string ).

      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type.                  "#EC NO_HANDLER
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
