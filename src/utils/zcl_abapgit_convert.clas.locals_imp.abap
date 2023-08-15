CLASS lcl_in DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS convert
      IMPORTING
        !iv_data         TYPE xsequence
        !iv_length       TYPE i OPTIONAL
      RETURNING
        VALUE(rv_string) TYPE string
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_in IMPLEMENTATION.
  METHOD convert.
    DATA lo_conv  TYPE REF TO object.
    DATA lv_class TYPE string.
    DATA lx_error TYPE REF TO cx_root.

    TRY.
        TRY.
            CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_in
              RECEIVING
                instance = lo_conv.

            CALL METHOD lo_conv->('IF_ABAP_CONV_IN~CONVERT')
              EXPORTING
                source = iv_data
              RECEIVING
                result = rv_string.
            rv_string = rv_string(iv_length).
          CATCH cx_sy_dyn_call_illegal_class.
            lv_class = 'CL_ABAP_CONV_IN_CE'.
            CALL METHOD (lv_class)=>create
              EXPORTING
                encoding = 'UTF-8'
              RECEIVING
                conv     = lo_conv.

            CALL METHOD lo_conv->('CONVERT')
              EXPORTING
                input = iv_data
                n     = iv_length
              IMPORTING
                data  = rv_string.
        ENDTRY.
      CATCH cx_parameter_invalid_range
            cx_sy_codepage_converter_init
            cx_sy_conversion_codepage
            cx_parameter_invalid_type INTO lx_error.
        zcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
