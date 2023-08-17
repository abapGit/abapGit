CLASS lcl_in DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS convert
      IMPORTING
        !iv_data         TYPE xsequence
        !iv_length       TYPE i OPTIONAL
      RETURNING
        VALUE(rv_string) TYPE string
      RAISING
        zcx_abapgit_exception.
  PRIVATE SECTION.
    CLASS-DATA go_conv_new TYPE REF TO object.
    CLASS-DATA go_conv_old TYPE REF TO object.
ENDCLASS.

CLASS lcl_in IMPLEMENTATION.
  METHOD convert.

    DATA lv_class TYPE string.
    DATA lx_error TYPE REF TO cx_root.

    IF go_conv_new IS INITIAL AND go_conv_old IS INITIAL.
      TRY.
          CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_in
            RECEIVING
              instance = go_conv_new.
        CATCH cx_sy_dyn_call_illegal_class.
          lv_class = 'CL_ABAP_CONV_IN_CE'.
          CALL METHOD (lv_class)=>create
            EXPORTING
              encoding = 'UTF-8'
            RECEIVING
              conv     = go_conv_old.
      ENDTRY.
    ENDIF.

    TRY.
        IF go_conv_new IS NOT INITIAL.
          CALL METHOD go_conv_new->('IF_ABAP_CONV_IN~CONVERT')
            EXPORTING
              source = iv_data
            RECEIVING
              result = rv_string.
        ELSE.
          CALL METHOD go_conv_old->('CONVERT')
            EXPORTING
              input = iv_data
              n     = iv_length
            IMPORTING
              data  = rv_string.
        ENDIF.
      CATCH cx_parameter_invalid_range
        cx_sy_codepage_converter_init
        cx_sy_conversion_codepage
        cx_parameter_invalid_type INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_out DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS convert
      IMPORTING
        !iv_string        TYPE string
      RETURNING
        VALUE(rv_xstring) TYPE xstring
      RAISING
        zcx_abapgit_exception.
  PRIVATE SECTION.
    CLASS-DATA go_conv_new TYPE REF TO object.
    CLASS-DATA go_conv_old TYPE REF TO object.
ENDCLASS.

CLASS lcl_out IMPLEMENTATION.
  METHOD convert.
    DATA lx_error TYPE REF TO cx_root.
    DATA lv_class TYPE string.

    IF go_conv_new IS INITIAL AND go_conv_old IS INITIAL.
      TRY.
          CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_out
            RECEIVING
              instance = go_conv_new.
        CATCH cx_sy_dyn_call_illegal_class.
          lv_class = 'CL_ABAP_CONV_OUT_CE'.
          CALL METHOD (lv_class)=>create
            EXPORTING
              encoding = 'UTF-8'
            RECEIVING
              conv     = go_conv_old.
      ENDTRY.
    ENDIF.

    TRY.
        IF go_conv_new IS NOT INITIAL.
          CALL METHOD go_conv_new->('IF_ABAP_CONV_OUT~CONVERT')
            EXPORTING
              source = iv_string
            RECEIVING
              result = rv_xstring.
        ELSE.
          CALL METHOD go_conv_old->('CONVERT')
            EXPORTING
              data   = iv_string
            IMPORTING
              buffer = rv_xstring.
        ENDIF.
      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
