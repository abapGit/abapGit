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
             sap2_code  TYPE laiso,
             bcp47_code TYPE string,
           END OF ty_language_mapping,
           ty_language_mappings TYPE STANDARD TABLE OF ty_language_mapping WITH DEFAULT KEY.
    CLASS-DATA gt_language_mappings TYPE ty_language_mappings.
    CLASS-METHODS:
      sap1_to_sap2
        IMPORTING
          im_sap1        TYPE sy-langu
        RETURNING
          VALUE(re_sap2) TYPE laiso
        RAISING
          zcx_abapgit_exception,
      sap2_to_sap1
        IMPORTING
          im_sap2        TYPE laiso
        RETURNING
          VALUE(re_sap1) TYPE sy-langu
        RAISING
          zcx_abapgit_exception,
      sap1_to_bcp47
        IMPORTING
          im_sap1         TYPE sy-langu
        RETURNING
          VALUE(re_bcp47) TYPE string
        RAISING
          zcx_abapgit_exception,
      bcp47_to_sap1
        IMPORTING
          im_bcp47       TYPE string
        RETURNING
          VALUE(re_sap1) TYPE sy-langu
        RAISING
          zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS fill_language_mappings.
    CLASS-METHODS fill_language_mappings_1.
    CLASS-METHODS fill_language_mappings_2.
    CLASS-METHODS
      fill_language_mapping
        IMPORTING
          im_sap1  TYPE string
          im_sap2  TYPE string
          im_bcp47 TYPE string.

ENDCLASS.

CLASS lcl_bcp47_language_table IMPLEMENTATION.

  METHOD sap1_to_bcp47.
    DATA lv_language_mapping TYPE ty_language_mapping.

    IF gt_language_mappings IS INITIAL OR lines( gt_language_mappings ) = 0.
      fill_language_mappings( ).
    ENDIF.

    LOOP AT gt_language_mappings INTO lv_language_mapping WHERE sap1_code = im_sap1.
      IF re_bcp47 IS INITIAL OR strlen( re_bcp47 ) > strlen( lv_language_mapping-bcp47_code ).
        re_bcp47 = lv_language_mapping-bcp47_code.
      ENDIF.
    ENDLOOP.

    IF re_bcp47 IS INITIAL.
      zcx_abapgit_exception=>raise( |Could not map SAP1 language code { im_sap1 } to BCP47 language code.| ).
    ENDIF.
  ENDMETHOD.

  METHOD bcp47_to_sap1.
    DATA lv_language_mapping TYPE ty_language_mapping.

    IF gt_language_mappings IS INITIAL OR lines( gt_language_mappings ) = 0.
      fill_language_mappings( ).
    ENDIF.

    LOOP AT gt_language_mappings INTO lv_language_mapping.
      IF to_lower( lv_language_mapping-bcp47_code ) = to_lower( im_bcp47 ) AND re_sap1 IS INITIAL.
        re_sap1 = lv_language_mapping-sap1_code.
      ENDIF.
    ENDLOOP.

    IF re_sap1 IS INITIAL.
      zcx_abapgit_exception=>raise( |Could not map BCP47 language code { im_bcp47 } to SAP1 language code.| ).
    ENDIF.
  ENDMETHOD.


  METHOD sap1_to_sap2.
    DATA lv_language_mapping TYPE ty_language_mapping.

    IF gt_language_mappings IS INITIAL.
      fill_language_mappings( ).
    ENDIF.

    READ TABLE gt_language_mappings WITH KEY sap1_code = im_sap1 INTO lv_language_mapping.
    re_sap2 = lv_language_mapping-sap2_code.

    IF re_sap2 IS INITIAL.
      zcx_abapgit_exception=>raise( |Could not map SAP1 language code { im_sap1 } to SAP2 language code.| ).
    ENDIF.
  ENDMETHOD.

  METHOD sap2_to_sap1.
    DATA lv_language_mapping TYPE ty_language_mapping.

    IF gt_language_mappings IS INITIAL.
      fill_language_mappings( ).
    ENDIF.

    READ TABLE gt_language_mappings WITH KEY sap2_code = im_sap2 INTO lv_language_mapping.
    re_sap1 = lv_language_mapping-sap1_code.

    IF re_sap1 IS INITIAL.
      zcx_abapgit_exception=>raise( |Could not map SAP2 language code { im_sap2 } to SAP1 language code.| ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_language_mapping.
    DATA lv_line TYPE ty_language_mapping.
    DATA lv_sap1 TYPE sy-langu.

    IF strlen( im_sap1 ) = 4.
      TRY.
          lv_sap1 = zcl_abapgit_convert=>uccp( im_sap1 ).
        CATCH cx_root.
          " Language is not supported in this system -> ignore it
          " Should someone try to use the language in a repo, it will result in an error (see above)
          RETURN.
      ENDTRY.
    ELSEIF strlen( im_sap1 ) = 1.
      lv_sap1 = im_sap1.
    ENDIF.

    lv_line-bcp47_code = im_bcp47.
    lv_line-sap2_code = im_sap2.
    lv_line-sap1_code = lv_sap1.

    APPEND lv_line TO gt_language_mappings.
    CLEAR lv_line.
  ENDMETHOD.

  METHOD fill_language_mappings.
    " table is filled with two separate methods to comply
    " with https://rules.abaplint.org/method_length/
    fill_language_mappings_1( ).
    fill_language_mappings_2( ).
  ENDMETHOD.

  METHOD fill_language_mappings_1.
    fill_language_mapping( im_sap1  = 'a'
                           im_sap2  = 'AF'
                           im_bcp47 = 'af' ).

    fill_language_mapping( im_sap1  = 'BF51'
                           im_sap2  = 'SQ'
                           im_bcp47 = 'sq' ).

    fill_language_mapping( im_sap1  = 'BA15'
                           im_sap2  = 'EU'
                           im_bcp47 = 'eu' ).

    fill_language_mapping( im_sap1  = 'B8F3'
                           im_sap2  = 'BS'
                           im_bcp47 = 'bs' ).

    fill_language_mapping( im_sap1  = 'W'
                           im_sap2  = 'BG'
                           im_bcp47 = 'bg' ).

    fill_language_mapping( im_sap1  = 'c'
                           im_sap2  = 'CA'
                           im_bcp47 = 'ca' ).

    fill_language_mapping( im_sap1  = '1'
                           im_sap2  = 'ZH'
                           im_bcp47 = 'zh' ).

    fill_language_mapping( im_sap1  = '1'
                           im_sap2  = 'ZH'
                           im_bcp47 = 'zh-Hans' ).

    fill_language_mapping( im_sap1  = 'B343'
                           im_sap2  = '3C'
                           im_bcp47 = 'zh-SG' ).

    fill_language_mapping( im_sap1  = 'M'
                           im_sap2  = 'ZF'
                           im_bcp47 = 'zh-Hant' ).

    fill_language_mapping( im_sap1  = '6'
                           im_sap2  = 'HR'
                           im_bcp47 = 'hr' ).

    fill_language_mapping( im_sap1  = 'C'
                           im_sap2  = 'CS'
                           im_bcp47 = 'cs' ).

    fill_language_mapping( im_sap1  = 'K'
                           im_sap2  = 'DA'
                           im_bcp47 = 'da' ).

    fill_language_mapping( im_sap1  = 'N'
                           im_sap2  = 'NL'
                           im_bcp47 = 'nl' ).

    fill_language_mapping( im_sap1  = 'N'
                           im_sap2  = 'NL'
                           im_bcp47 = 'nl-NL' ).

    fill_language_mapping( im_sap1  = 'B284'
                           im_sap2  = '1D'
                           im_bcp47 = 'nl-BE' ).

    fill_language_mapping( im_sap1  = 'E'
                           im_sap2  = 'EN'
                           im_bcp47 = 'en' ).

    fill_language_mapping( im_sap1  = 'E'
                           im_sap2  = 'EN'
                           im_bcp47 = 'en-US' ).

    fill_language_mapping( im_sap1  = 'B46E'
                           im_sap2  = '6N'
                           im_bcp47 = 'en-GB' ).

    fill_language_mapping( im_sap1  = 'B285'
                           im_sap2  = '1E'
                           im_bcp47 = 'en-AU' ).

    fill_language_mapping( im_sap1  = 'B2E5'
                           im_sap2  = '2E'
                           im_bcp47 = 'en-BZ' ).

    fill_language_mapping( im_sap1  = 'B345'
                           im_sap2  = '3E'
                           im_bcp47 = 'en-CA' ).

    fill_language_mapping( im_sap1  = 'B405'
                           im_sap2  = '5E'
                           im_bcp47 = 'en-HK' ).

    fill_language_mapping( im_sap1  = 'B465'
                           im_sap2  = '6E'
                           im_bcp47 = 'en-IN' ).

    fill_language_mapping( im_sap1  = 'B4C5'
                           im_sap2  = '7E'
                           im_bcp47 = 'en-ID' ).

    fill_language_mapping( im_sap1  = 'B525'
                           im_sap2  = '8E'
                           im_bcp47 = 'en-IE' ).

    fill_language_mapping( im_sap1  = 'B585'
                           im_sap2  = '9E'
                           im_bcp47 = 'en-JM' ).

    fill_language_mapping( im_sap1  = 'B225'
                           im_sap2  = '0E'
                           im_bcp47 = 'en-MY' ).

    fill_language_mapping( im_sap1  = 'B28E'
                           im_sap2  = '1N'
                           im_bcp47 = 'en-NZ' ).

    fill_language_mapping( im_sap1  = 'B2EE'
                           im_sap2  = '2N'
                           im_bcp47 = 'en-PH' ).

    fill_language_mapping( im_sap1  = 'B34E'
                           im_sap2  = '3N'
                           im_bcp47 = 'en-SG' ).

    fill_language_mapping( im_sap1  = 'B3AE'
                           im_sap2  = '4N'
                           im_bcp47 = 'en-ZA' ).

    fill_language_mapping( im_sap1  = 'B40E'
                           im_sap2  = '5N'
                           im_bcp47 = 'en-TT' ).

    fill_language_mapping( im_sap1  = 'B4CE'
                           im_sap2  = '7N'
                           im_bcp47 = 'en-ZW' ).

    fill_language_mapping( im_sap1  = '9'
                           im_sap2  = 'ET'
                           im_bcp47 = 'et' ).

    fill_language_mapping( im_sap1  = 'U'
                           im_sap2  = 'FI'
                           im_bcp47 = 'fi' ).

    fill_language_mapping( im_sap1  = 'F'
                           im_sap2  = 'FR'
                           im_bcp47 = 'fr' ).

    fill_language_mapping( im_sap1  = 'F'
                           im_sap2  = 'FR'
                           im_bcp47 = 'fr-FR' ).

    fill_language_mapping( im_sap1  = 'B286'
                           im_sap2  = '1F'
                           im_bcp47 = 'fr-BE' ).

    fill_language_mapping( im_sap1  = 'B2E6'
                           im_sap2  = '2F'
                           im_bcp47 = 'fr-CM' ).
  ENDMETHOD.

  METHOD fill_language_mappings_2.
    fill_language_mapping( im_sap1  = 'B346'
                           im_sap2  = '3F'
                           im_bcp47 = 'fr-CA' ).

    fill_language_mapping( im_sap1  = 'B3A6'
                           im_sap2  = '4F'
                           im_bcp47 = 'fr-CG' ).

    fill_language_mapping( im_sap1  = 'B406'
                           im_sap2  = '5F'
                           im_bcp47 = 'fr-CI' ).

    fill_language_mapping( im_sap1  = 'B466'
                           im_sap2  = '6F'
                           im_bcp47 = 'fr-HT' ).

    fill_language_mapping( im_sap1  = 'B4C6'
                           im_sap2  = '7F'
                           im_bcp47 = 'fr-LU' ).

    fill_language_mapping( im_sap1  = 'B526'
                           im_sap2  = '8F'
                           im_bcp47 = 'fr-ML' ).

    fill_language_mapping( im_sap1  = 'B586'
                           im_sap2  = '9F'
                           im_bcp47 = 'fr-MC' ).

    fill_language_mapping( im_sap1  = 'B288'
                           im_sap2  = '1H'
                           im_bcp47 = 'fr-MA' ).

    fill_language_mapping( im_sap1  = 'B2E8'
                           im_sap2  = '2H'
                           im_bcp47 = 'fr-RE' ).

    fill_language_mapping( im_sap1  = 'B348'
                           im_sap2  = '3H'
                           im_bcp47 = 'fr-SN' ).

    fill_language_mapping( im_sap1  = 'B3A8'
                           im_sap2  = '4H'
                           im_bcp47 = 'fr-CH' ).

    fill_language_mapping( im_sap1  = 'BAC4'
                           im_sap2  = 'GD'
                           im_bcp47 = 'gd' ).

    fill_language_mapping( im_sap1  = 'BACC'
                           im_sap2  = 'GL'
                           im_bcp47 = 'gl' ).

    fill_language_mapping( im_sap1  = 'D'
                           im_sap2  = 'DE'
                           im_bcp47 = 'de' ).

    fill_language_mapping( im_sap1  = 'D'
                           im_sap2  = 'DE'
                           im_bcp47 = 'de-DE' ).

    fill_language_mapping( im_sap1  = 'B287'
                           im_sap2  = '1G'
                           im_bcp47 = 'de-AT' ).

    fill_language_mapping( im_sap1  = 'B2E7'
                           im_sap2  = '2G'
                           im_bcp47 = 'de-LI' ).

    fill_language_mapping( im_sap1  = 'B347'
                           im_sap2  = '3G'
                           im_bcp47 = 'de-LU' ).

    fill_language_mapping( im_sap1  = 'B3A7'
                           im_sap2  = '4G'
                           im_bcp47 = 'de-CH' ).

    fill_language_mapping( im_sap1  = 'G'
                           im_sap2  = 'EL'
                           im_bcp47 = 'el' ).

    fill_language_mapping( im_sap1  = 'B'
                           im_sap2  = 'HE'
                           im_bcp47 = 'he' ).

    fill_language_mapping( im_sap1  = 'H'
                           im_sap2  = 'HU'
                           im_bcp47 = 'hu' ).

    fill_language_mapping( im_sap1  = 'b'
                           im_sap2  = 'IS'
                           im_bcp47 = 'is' ).

    fill_language_mapping( im_sap1  = 'i'
                           im_sap2  = 'ID'
                           im_bcp47 = 'id' ).

    fill_language_mapping( im_sap1  = 'BAC1'
                           im_sap2  = 'GA'
                           im_bcp47 = 'ga' ).

    fill_language_mapping( im_sap1  = 'I'
                           im_sap2  = 'IT'
                           im_bcp47 = 'it' ).

    fill_language_mapping( im_sap1  = 'I'
                           im_sap2  = 'IT'
                           im_bcp47 = 'it-IT' ).

    fill_language_mapping( im_sap1  = 'B289'
                           im_sap2  = '1I'
                           im_bcp47 = 'it-CH' ).

    fill_language_mapping( im_sap1  = 'J'
                           im_sap2  = 'JA'
                           im_bcp47 = 'ja' ).

    fill_language_mapping( im_sap1  = '3'
                           im_sap2  = 'KO'
                           im_bcp47 = 'ko' ).

    fill_language_mapping( im_sap1  = '3'
                           im_sap2  = 'KO'
                           im_bcp47 = 'ko-KR' ).

    fill_language_mapping( im_sap1  = 'Y'
                           im_sap2  = 'LV'
                           im_bcp47 = 'lv' ).

    fill_language_mapping( im_sap1  = 'X'
                           im_sap2  = 'LT'
                           im_bcp47 = 'lt' ).

    fill_language_mapping( im_sap1  = '7'
                           im_sap2  = 'MS'
                           im_bcp47 = 'ms' ).

    fill_language_mapping( im_sap1  = '7'
                           im_sap2  = 'MS'
                           im_bcp47 = 'ms-MY' ).

    fill_language_mapping( im_sap1  = 'B28D'
                           im_sap2  = '1M'
                           im_bcp47 = 'ms-BN' ).

    fill_language_mapping( im_sap1  = 'O'
                           im_sap2  = 'NO'
                           im_bcp47 = 'no' ).

    fill_language_mapping( im_sap1  = 'L'
                           im_sap2  = 'PL'
                           im_bcp47 = 'pl' ).

    fill_language_mapping( im_sap1  = 'P'
                           im_sap2  = 'PT'
                           im_bcp47 = 'pt' ).

    fill_language_mapping( im_sap1  = 'P'
                           im_sap2  = 'PT'
                           im_bcp47 = 'pt-BR' ).

    fill_language_mapping( im_sap1  = 'BEED'
                           im_sap2  = 'RM'
                           im_bcp47 = 'rm' ).

    fill_language_mapping( im_sap1  = '4'
                           im_sap2  = 'RO'
                           im_bcp47 = 'ro' ).

    fill_language_mapping( im_sap1  = '4'
                           im_sap2  = 'RO'
                           im_bcp47 = 'ro-RO' ).

    fill_language_mapping( im_sap1  = 'R'
                           im_sap2  = 'RU'
                           im_bcp47 = 'ru-RU' ).

    fill_language_mapping( im_sap1  = '0'
                           im_sap2  = 'SR'
                           im_bcp47 = 'sr' ).

    fill_language_mapping( im_sap1  = '0'
                           im_sap2  = 'SR'
                           im_bcp47 = 'sr-Cyrl' ).

    fill_language_mapping( im_sap1  = 'd'
                           im_sap2  = 'SH'
                           im_bcp47 = 'sr-Latn' ).

    fill_language_mapping( im_sap1  = 'Q'
                           im_sap2  = 'SK'
                           im_bcp47 = 'sk' ).

    fill_language_mapping( im_sap1  = '5'
                           im_sap2  = 'SL'
                           im_bcp47 = 'sl' ).

    fill_language_mapping( im_sap1  = 'BF42'
                           im_sap2  = 'SB'
                           im_bcp47 = 'wen' ).

    fill_language_mapping( im_sap1  = 'B9B3'
                           im_sap2  = 'DS'
                           im_bcp47 = 'dsb' ).

    fill_language_mapping( im_sap1  = 'BB33'
                           im_sap2  = 'HS'
                           im_bcp47 = 'hsb' ).

    fill_language_mapping( im_sap1  = 'S'
                           im_sap2  = 'ES'
                           im_bcp47 = 'es' ).

    fill_language_mapping( im_sap1  = 'S'
                           im_sap2  = 'ES'
                           im_bcp47 = 'es-ES' ).

    fill_language_mapping( im_sap1  = 'B293'
                           im_sap2  = '1S'
                           im_bcp47 = 'es-AR' ).

    fill_language_mapping( im_sap1  = 'B2F3'
                           im_sap2  = '2S'
                           im_bcp47 = 'es-BO' ).

    fill_language_mapping( im_sap1  = 'B353'
                           im_sap2  = '3S'
                           im_bcp47 = 'es-CL' ).

    fill_language_mapping( im_sap1  = 'B233'
                           im_sap2  = '0S'
                           im_bcp47 = 'es-CO' ).

    fill_language_mapping( im_sap1  = 'B3B3'
                           im_sap2  = '4S'
                           im_bcp47 = 'es-CR' ).

    fill_language_mapping( im_sap1  = 'B413'
                           im_sap2  = '5S'
                           im_bcp47 = 'es-DO' ).

    fill_language_mapping( im_sap1  = 'B473'
                           im_sap2  = '6S'
                           im_bcp47 = 'es-EC' ).

    fill_language_mapping( im_sap1  = 'B4D3'
                           im_sap2  = '7S'
                           im_bcp47 = 'es-SV' ).

    fill_language_mapping( im_sap1  = 'B533'
                           im_sap2  = '8S'
                           im_bcp47 = 'es-GT' ).

    fill_language_mapping( im_sap1  = 'B593'
                           im_sap2  = '9S'
                           im_bcp47 = 'es-HN' ).

    fill_language_mapping( im_sap1  = 'B298'
                           im_sap2  = '1X'
                           im_bcp47 = 'es-MX' ).

    fill_language_mapping( im_sap1  = 'B2F8'
                           im_sap2  = '2X'
                           im_bcp47 = 'es-NI' ).

    fill_language_mapping( im_sap1  = 'B358'
                           im_sap2  = '3X'
                           im_bcp47 = 'es-PA' ).

    fill_language_mapping( im_sap1  = 'B3B8'
                           im_sap2  = '4X'
                           im_bcp47 = 'es-PY' ).

    fill_language_mapping( im_sap1  = 'B418'
                           im_sap2  = '5X'
                           im_bcp47 = 'es-PE' ).

    fill_language_mapping( im_sap1  = 'B478'
                           im_sap2  = '6X'
                           im_bcp47 = 'es-PR' ).

    fill_language_mapping( im_sap1  = 'B4D8'
                           im_sap2  = '7X'
                           im_bcp47 = 'es-UY' ).

    fill_language_mapping( im_sap1  = 'B538'
                           im_sap2  = '8X'
                           im_bcp47 = 'es-VE' ).

    fill_language_mapping( im_sap1  = 'BF57'
                           im_sap2  = 'SW'
                           im_bcp47 = 'sw' ).

    fill_language_mapping( im_sap1  = 'V'
                           im_sap2  = 'SV'
                           im_bcp47 = 'sv' ).

    fill_language_mapping( im_sap1  = 'BFAC'
                           im_sap2  = 'TL'
                           im_bcp47 = 'tl' ).

    fill_language_mapping( im_sap1  = '2'
                           im_sap2  = 'TH'
                           im_bcp47 = 'th' ).

    fill_language_mapping( im_sap1  = 'T'
                           im_sap2  = 'TR'
                           im_bcp47 = 'tr' ).

    fill_language_mapping( im_sap1  = '8'
                           im_sap2  = 'UK'
                           im_bcp47 = 'uk' ).

    fill_language_mapping( im_sap1  = 'C069'
                           im_sap2  = 'VI'
                           im_bcp47 = 'vi' ).

    fill_language_mapping( im_sap1  = 'C0C1'
                           im_sap2  = 'WA'
                           im_bcp47 = 'wa' ).

    fill_language_mapping( im_sap1  = 'A'
                           im_sap2  = 'AR'
                           im_bcp47 = 'ar-SA' ).
  ENDMETHOD.


ENDCLASS.
