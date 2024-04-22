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

CLASS lcl_bcp47_language_table DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_language_mapping,
             sap1_code  TYPE sy-langu,
             bcp47_code TYPE string,
           END OF ty_language_mapping,
           ty_language_mappings TYPE STANDARD TABLE OF ty_language_mapping WITH DEFAULT KEY.
    CLASS-DATA gv_language_mappings TYPE ty_language_mappings.
    CLASS-METHODS:
      sap1_to_bcp47
        IMPORTING
          im_sap1         TYPE sy-langu
        RETURNING
          VALUE(re_bcp47) TYPE string
        EXCEPTIONS
          no_assignment,
      bcp47_to_sap1
        IMPORTING
          im_bcp47       TYPE string
        RETURNING
          VALUE(re_sap1) TYPE sy-langu
        EXCEPTIONS
          no_assignment.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS fill_language_mappings.

ENDCLASS.

CLASS lcl_bcp47_language_table IMPLEMENTATION.

  METHOD sap1_to_bcp47.
    DATA lv_language_mapping TYPE ty_language_mapping.

    IF gv_language_mappings IS INITIAL OR lines( gv_language_mappings ) = 0.
      fill_language_mappings( ).
    ENDIF.

    LOOP AT gv_language_mappings INTO lv_language_mapping WHERE sap1_code = im_sap1.
      IF re_bcp47 IS INITIAL OR strlen( re_bcp47 ) > strlen( lv_language_mapping-bcp47_code ).
        re_bcp47 = lv_language_mapping-bcp47_code.
      ENDIF.
    ENDLOOP.

    IF re_bcp47 IS INITIAL.
      RAISE no_assignment.
    ENDIF.
  ENDMETHOD.

  METHOD bcp47_to_sap1.
    DATA lv_language_mapping TYPE ty_language_mapping.

    IF gv_language_mappings IS INITIAL OR lines( gv_language_mappings ) = 0.
      fill_language_mappings( ).
    ENDIF.

    LOOP AT gv_language_mappings INTO lv_language_mapping.
      IF to_lower( lv_language_mapping-bcp47_code ) = to_lower( im_bcp47 ).
        IF re_sap1 IS INITIAL.
          re_sap1 = lv_language_mapping-sap1_code.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF re_sap1 IS INITIAL.
      RAISE no_assignment.
    ENDIF.
  ENDMETHOD.

  METHOD fill_language_mappings.
    DATA lv_line TYPE ty_language_mapping.

    lv_line-bcp47_code = 'af'.
    lv_line-sap1_code = 'a'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'sq'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'BF51' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'eu'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'BA15' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'bs'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B8F3' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'bg'.
    lv_line-sap1_code = 'W'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'ca'.
    lv_line-sap1_code = 'c'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'zh'.
    lv_line-sap1_code = '1'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'zh-Hans'.
    lv_line-sap1_code = '1'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'zh-SG'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B343' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'zh-Hant'.
    lv_line-sap1_code = 'M'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'hr'.
    lv_line-sap1_code = '6'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'cs'.
    lv_line-sap1_code = 'C'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'da'.
    lv_line-sap1_code = 'K'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'nl'.
    lv_line-sap1_code = 'N'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'nl-NL'.
    lv_line-sap1_code = 'N'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'nl-BE'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B284' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en'.
    lv_line-sap1_code = 'E'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-US'.
    lv_line-sap1_code = 'E'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-GB'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B46E' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-AU'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B285' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-BZ'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B2E5' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-CA'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B345' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-HK'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B405' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-IN'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B465' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-ID'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B4C5' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-IE'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B525' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-JM'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B585' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-MY'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B225' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-NZ'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B28E' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-PH'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B2EE' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-SG'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B34E' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-ZA'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B3AE' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-TT'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B40E' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'en-ZW'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B4CE' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'et'.
    lv_line-sap1_code = '9'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fi'.
    lv_line-sap1_code = 'U'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fr'.
    lv_line-sap1_code = 'F'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fr-FR'.
    lv_line-sap1_code = 'F'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fr-BE'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B286' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fr-CM'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B2E6' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fr-CA'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B346' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fr-CG'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B3A6' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fr-CI'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B406' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fr-HT'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B466' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fr-LU'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B4C6' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fr-ML'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B526' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fr-MC'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B586' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fr-MA'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B288' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fr-RE'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B2E8' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fr-SN'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B348' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'fr-CH'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B3A8' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'gd'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'BAC4' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'gl'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'BACC' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'de'.
    lv_line-sap1_code = 'D'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'de-DE'.
    lv_line-sap1_code = 'D'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'de-AT'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B287' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'de-LI'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B2E7' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'de-LU'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B347' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'de-CH'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B3A7' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'el'.
    lv_line-sap1_code = 'G'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'he'.
    lv_line-sap1_code = 'B'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'hu'.
    lv_line-sap1_code = 'H'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'is'.
    lv_line-sap1_code = 'b'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'id'.
    lv_line-sap1_code = 'i'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'ga'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'BAC1' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'it'.
    lv_line-sap1_code = 'I'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'it-IT'.
    lv_line-sap1_code = 'I'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'it-CH'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B289' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'ja'.
    lv_line-sap1_code = 'J'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'ko'.
    lv_line-sap1_code = '3'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'ko-KR'.
    lv_line-sap1_code = '3'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'lv'.
    lv_line-sap1_code = 'Y'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'lt'.
    lv_line-sap1_code = 'X'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'ms'.
    lv_line-sap1_code = '7'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'ms-MY'.
    lv_line-sap1_code = '7'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'ms-BN'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B28D' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'no'.
    lv_line-sap1_code = 'O'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'pl'.
    lv_line-sap1_code = 'L'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'pt'.
    lv_line-sap1_code = 'P'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'pt-BR'.
    lv_line-sap1_code = 'P'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'rm'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'BEED' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'ro'.
    lv_line-sap1_code = '4'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'ro-RO'.
    lv_line-sap1_code = '4'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'ru-RU'.
    lv_line-sap1_code = 'R'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'sr'.
    lv_line-sap1_code = '0'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'sr-Cyrl'.
    lv_line-sap1_code = '0'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'sr-Latn'.
    lv_line-sap1_code = 'd'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'sk'.
    lv_line-sap1_code = 'Q'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'sl'.
    lv_line-sap1_code = '5'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'wen'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'BF42' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'dsb'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B9B3' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'hsb'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'BB33' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es'.
    lv_line-sap1_code = 'S'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-ES'.
    lv_line-sap1_code = 'S'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-AR'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B293' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-BO'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B2F3' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-CL'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B353' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-CO'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B233' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-CR'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B3B3' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-DO'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B413' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-EC'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B473' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-SV'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B4D3' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-GT'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B533' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-HN'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B593' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-MX'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B298' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-NI'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B2F8' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-PA'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B358' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-PY'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B3B8' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-PE'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B418' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-PR'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B478' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-UY'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B4D8' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'es-VE'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'B538' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'sw'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'BF57' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'sv'.
    lv_line-sap1_code = 'V'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'tl'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'BFAC' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'th'.
    lv_line-sap1_code = '2'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'tr'.
    lv_line-sap1_code = 'T'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'uk'.
    lv_line-sap1_code = '8'.
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'vi'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'C069' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

    lv_line-bcp47_code = 'wa'.
    lv_line-sap1_code = cl_abap_conv_in_ce=>uccp( 'C0C1' ).
    APPEND lv_line TO gv_language_mappings.
    CLEAR lv_line.

  ENDMETHOD.

ENDCLASS.
