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
             text       TYPE string,
           END OF ty_language_mapping,
           ty_language_mappings TYPE STANDARD TABLE OF ty_language_mapping WITH DEFAULT KEY.
    CLASS-DATA gt_language_mappings TYPE ty_language_mappings.
    CLASS-METHODS:
      sap1_to_text
        IMPORTING
          im_sap1        TYPE sy-langu
        RETURNING
          VALUE(re_text) TYPE string,
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
    CLASS-METHODS
      fill_language_mapping
        IMPORTING
          im_sap1  TYPE string
          im_sap2  TYPE string
          im_bcp47 TYPE string
          iv_text  TYPE string OPTIONAL.

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

  METHOD sap1_to_text.
    DATA lv_language_mapping TYPE ty_language_mapping.

    IF gt_language_mappings IS INITIAL.
      fill_language_mappings( ).
    ENDIF.

    READ TABLE gt_language_mappings WITH KEY sap1_code = im_sap1 INTO lv_language_mapping.
    IF sy-subrc = 0.
      re_text = lv_language_mapping-text.
    ELSE.
      re_text = 'Unknonw language'.
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
    lv_line-text      = iv_text.

    APPEND lv_line TO gt_language_mappings.
    CLEAR lv_line.
  ENDMETHOD.

  METHOD fill_language_mappings.
    fill_language_mapping( im_sap1  = 'a'
                           im_sap2  = 'AF'
                           im_bcp47 = 'af'
                           iv_text  = 'Afrikaans' ).

    fill_language_mapping( im_sap1  = 'BF51'
                           im_sap2  = 'SQ'
                           im_bcp47 = 'sq'
                           iv_text  = 'Albanian' ).

    fill_language_mapping( im_sap1  = 'A'
                           im_sap2  = 'AR'
                           im_bcp47 = 'ar-SA'
                           iv_text  = 'Arabic' ).

    fill_language_mapping( im_sap1  = 'BA15'
                           im_sap2  = 'EU'
                           im_bcp47 = 'eu'
                           iv_text  = 'Basque' ).

    fill_language_mapping( im_sap1  = 'B8F3'
                           im_sap2  = 'BS'
                           im_bcp47 = 'bs'
                           iv_text  = 'Bosnian' ).

    fill_language_mapping( im_sap1  = 'W'
                           im_sap2  = 'BG'
                           im_bcp47 = 'bg'
                           iv_text  = 'Bulgarian' ).

    fill_language_mapping( im_sap1  = 'c'
                           im_sap2  = 'CA'
                           im_bcp47 = 'ca'
                           iv_text  = 'Catalan' ).

    fill_language_mapping( im_sap1  = '1'
                           im_sap2  = 'ZH'
                           im_bcp47 = 'zh'
                           iv_text  = 'Chinese' ).

    fill_language_mapping( im_sap1  = '1'
                           im_sap2  = 'ZH'
                           im_bcp47 = 'zh-Hans'
                           iv_text  = 'Chinese (Simplified)' ).

    fill_language_mapping( im_sap1  = 'B343'
                           im_sap2  = '3C'
                           im_bcp47 = 'zh-SG'
                           iv_text  = 'Chinese (Singapore)' ).

    fill_language_mapping( im_sap1  = 'M'
                           im_sap2  = 'ZF'
                           im_bcp47 = 'zh-Hant'
                           iv_text  = 'Chinese (Traditional)' ).

    fill_language_mapping( im_sap1  = '6'
                           im_sap2  = 'HR'
                           im_bcp47 = 'hr'
                           iv_text  = 'Croatian' ).

    fill_language_mapping( im_sap1  = 'C'
                           im_sap2  = 'CS'
                           im_bcp47 = 'cs'
                           iv_text  = 'Czech' ).

    fill_language_mapping( im_sap1  = 'K'
                           im_sap2  = 'DA'
                           im_bcp47 = 'da'
                           iv_text  = 'Danish' ).

    fill_language_mapping( im_sap1  = 'N'
                           im_sap2  = 'NL'
                           im_bcp47 = 'nl'
                           iv_text  = 'Dutch' ).

    fill_language_mapping( im_sap1  = 'N'
                           im_sap2  = 'NL'
                           im_bcp47 = 'nl-NL'
                           iv_text  = 'Dutch' ).

    fill_language_mapping( im_sap1  = 'B284'
                           im_sap2  = '1D'
                           im_bcp47 = 'nl-BE'
                           iv_text  = 'Dutch (Belgium)' ).

    fill_language_mapping( im_sap1  = 'E'
                           im_sap2  = 'EN'
                           im_bcp47 = 'en'
                           iv_text  = 'English' ).

    fill_language_mapping( im_sap1  = 'E'
                           im_sap2  = 'EN'
                           im_bcp47 = 'en-US'
                           iv_text  = 'English (American)' ).

    fill_language_mapping( im_sap1  = 'B46E'
                           im_sap2  = '6N'
                           im_bcp47 = 'en-GB'
                           iv_text  = 'English (British)' ).

    fill_language_mapping( im_sap1  = 'B285'
                           im_sap2  = '1E'
                           im_bcp47 = 'en-AU'
                           iv_text  = 'English (Australia)' ).

    fill_language_mapping( im_sap1  = 'B2E5'
                           im_sap2  = '2E'
                           im_bcp47 = 'en-BZ'
                           iv_text  = 'English (Belize)' ).

    fill_language_mapping( im_sap1  = 'B345'
                           im_sap2  = '3E'
                           im_bcp47 = 'en-CA'
                           iv_text  = 'English (Canada)' ).

    fill_language_mapping( im_sap1  = 'B405'
                           im_sap2  = '5E'
                           im_bcp47 = 'en-HK'
                           iv_text  = 'English (Hong Kong SAR China)' ).

    fill_language_mapping( im_sap1  = 'B465'
                           im_sap2  = '6E'
                           im_bcp47 = 'en-IN'
                           iv_text  = 'English (India)' ).

    fill_language_mapping( im_sap1  = 'B4C5'
                           im_sap2  = '7E'
                           im_bcp47 = 'en-ID'
                           iv_text  = 'English' ).

    fill_language_mapping( im_sap1  = 'B525'
                           im_sap2  = '8E'
                           im_bcp47 = 'en-IE'
                           iv_text  = 'English (Ireland)' ).

    fill_language_mapping( im_sap1  = 'B585'
                           im_sap2  = '9E'
                           im_bcp47 = 'en-JM'
                           iv_text  = 'English (Jamaica)' ).

    fill_language_mapping( im_sap1  = 'B225'
                           im_sap2  = '0E'
                           im_bcp47 = 'en-MY'
                           iv_text  = 'English (Malaysia)' ).

    fill_language_mapping( im_sap1  = 'B28E'
                           im_sap2  = '1N'
                           im_bcp47 = 'en-NZ'
                           iv_text  = 'English (New Zealand)' ).

    fill_language_mapping( im_sap1  = 'B2EE'
                           im_sap2  = '2N'
                           im_bcp47 = 'en-PH'
                           iv_text  = 'English (Philippines)' ).

    fill_language_mapping( im_sap1  = 'B34E'
                           im_sap2  = '3N'
                           im_bcp47 = 'en-SG'
                           iv_text  = 'English (Singapore)' ).

    fill_language_mapping( im_sap1  = 'B3AE'
                           im_sap2  = '4N'
                           im_bcp47 = 'en-ZA'
                           iv_text  = 'English (South Africa)' ).

    fill_language_mapping( im_sap1  = 'B40E'
                           im_sap2  = '5N'
                           im_bcp47 = 'en-TT'
                           iv_text  = 'English (Trinidad & Tobago)' ).

    fill_language_mapping( im_sap1  = 'B4CE'
                           im_sap2  = '7N'
                           im_bcp47 = 'en-ZW'
                           iv_text  = 'English (Zimbabwe)' ).

    fill_language_mapping( im_sap1  = '9'
                           im_sap2  = 'ET'
                           im_bcp47 = 'et'
                           iv_text  = 'Estonian' ).

    fill_language_mapping( im_sap1  = 'U'
                           im_sap2  = 'FI'
                           im_bcp47 = 'fi'
                           iv_text  = 'Finnish' ).

    fill_language_mapping( im_sap1  = 'F'
                           im_sap2  = 'FR'
                           im_bcp47 = 'fr'
                           iv_text  = 'French' ).

    fill_language_mapping( im_sap1  = 'F'
                           im_sap2  = 'FR'
                           im_bcp47 = 'fr-FR'
                           iv_text  = 'French' ).

    fill_language_mapping( im_sap1  = 'B286'
                           im_sap2  = '1F'
                           im_bcp47 = 'fr-BE'
                           iv_text  = 'French (Belgium)' ).

    fill_language_mapping( im_sap1  = 'B2E6'
                           im_sap2  = '2F'
                           im_bcp47 = 'fr-CM'
                           iv_text  = 'French (Cameroon)' ).

    fill_language_mapping( im_sap1  = 'B346'
                           im_sap2  = '3F'
                           im_bcp47 = 'fr-CA'
                           iv_text  = 'French (Canada)' ).

    fill_language_mapping( im_sap1  = 'B3A6'
                           im_sap2  = '4F'
                           im_bcp47 = 'fr-CG'
                           iv_text  = 'French (Congo-Brazzaville)' ).

    fill_language_mapping( im_sap1  = 'B406'
                           im_sap2  = '5F'
                           im_bcp47 = 'fr-CI'
                           iv_text  = 'French (Cote d''Ivoire)' ).

    fill_language_mapping( im_sap1  = 'B466'
                           im_sap2  = '6F'
                           im_bcp47 = 'fr-HT'
                           iv_text  = 'French (Haiti)' ).

    fill_language_mapping( im_sap1  = 'B4C6'
                           im_sap2  = '7F'
                           im_bcp47 = 'fr-LU'
                           iv_text  = 'French (Luxembourg)' ).

    fill_language_mapping( im_sap1  = 'B526'
                           im_sap2  = '8F'
                           im_bcp47 = 'fr-ML'
                           iv_text  = 'French (Mali)' ).

    fill_language_mapping( im_sap1  = 'B586'
                           im_sap2  = '9F'
                           im_bcp47 = 'fr-MC'
                           iv_text  = 'French (Monaco)' ).

    fill_language_mapping( im_sap1  = 'B288'
                           im_sap2  = '1H'
                           im_bcp47 = 'fr-MA'
                           iv_text  = 'French (Morocco)' ).

    fill_language_mapping( im_sap1  = 'B2E8'
                           im_sap2  = '2H'
                           im_bcp47 = 'fr-RE'
                           iv_text  = 'French (Reunion)' ).

    fill_language_mapping( im_sap1  = 'B348'
                           im_sap2  = '3H'
                           im_bcp47 = 'fr-SN'
                           iv_text  = 'French (Senegal)' ).

    fill_language_mapping( im_sap1  = 'B3A8'
                           im_sap2  = '4H'
                           im_bcp47 = 'fr-CH'
                           iv_text  = 'French (Switzerland)' ).

    fill_language_mapping( im_sap1  = 'BAC4'
                           im_sap2  = 'GD'
                           im_bcp47 = 'gd'
                           iv_text  = 'Scottish Gaelic' ).

    fill_language_mapping( im_sap1  = 'BACC'
                           im_sap2  = 'GL'
                           im_bcp47 = 'gl'
                           iv_text  = 'Galician' ).

    fill_language_mapping( im_sap1  = 'D'
                           im_sap2  = 'DE'
                           im_bcp47 = 'de'
                           iv_text  = 'German' ).

    fill_language_mapping( im_sap1  = 'D'
                           im_sap2  = 'DE'
                           im_bcp47 = 'de-DE'
                           iv_text  = 'German' ).

    fill_language_mapping( im_sap1  = 'B287'
                           im_sap2  = '1G'
                           im_bcp47 = 'de-AT'
                           iv_text  = 'German (Austria)' ).

    fill_language_mapping( im_sap1  = 'B2E7'
                           im_sap2  = '2G'
                           im_bcp47 = 'de-LI'
                           iv_text  = 'German (Liechtenstein)' ).

    fill_language_mapping( im_sap1  = 'B347'
                           im_sap2  = '3G'
                           im_bcp47 = 'de-LU'
                           iv_text  = 'German (Luxembourg)' ).

    fill_language_mapping( im_sap1  = 'B3A7'
                           im_sap2  = '4G'
                           im_bcp47 = 'de-CH'
                           iv_text  = 'German (Switzerland)' ).

    fill_language_mapping( im_sap1  = 'G'
                           im_sap2  = 'EL'
                           im_bcp47 = 'el'
                           iv_text  = 'Greek' ).

    fill_language_mapping( im_sap1  = 'B'
                           im_sap2  = 'HE'
                           im_bcp47 = 'he'
                           iv_text  = 'Hebrew' ).

    fill_language_mapping( im_sap1  = 'BB29'
                           im_sap2  = 'HI'
                           im_bcp47 = 'hi'
                           iv_text  = 'Hindi' ).

    fill_language_mapping( im_sap1  = 'H'
                           im_sap2  = 'HU'
                           im_bcp47 = 'hu'
                           iv_text  = 'Hungarian' ).

    fill_language_mapping( im_sap1  = 'b'
                           im_sap2  = 'IS'
                           im_bcp47 = 'is'
                           iv_text  = 'Icelandic' ).

    fill_language_mapping( im_sap1  = 'i'
                           im_sap2  = 'ID'
                           im_bcp47 = 'id'
                           iv_text  = 'Indonesian' ).

    fill_language_mapping( im_sap1  = 'BAC1'
                           im_sap2  = 'GA'
                           im_bcp47 = 'ga'
                           iv_text  = 'Irish' ).

    fill_language_mapping( im_sap1  = 'I'
                           im_sap2  = 'IT'
                           im_bcp47 = 'it'
                           iv_text  = 'Italian' ).

    fill_language_mapping( im_sap1  = 'I'
                           im_sap2  = 'IT'
                           im_bcp47 = 'it-IT'
                           iv_text  = 'Italian' ).

    fill_language_mapping( im_sap1  = 'B289'
                           im_sap2  = '1I'
                           im_bcp47 = 'it-CH'
                           iv_text  = 'Italian (Swiss)' ).

    fill_language_mapping( im_sap1  = 'J'
                           im_sap2  = 'JA'
                           im_bcp47 = 'ja'
                           iv_text  = 'Japanese' ).

    fill_language_mapping( im_sap1  = '3'
                           im_sap2  = 'KO'
                           im_bcp47 = 'ko'
                           iv_text  = 'Korean' ).

    fill_language_mapping( im_sap1  = '3'
                           im_sap2  = 'KO'
                           im_bcp47 = 'ko-KR'
                           iv_text  = 'Korean' ).

    fill_language_mapping( im_sap1  = 'Y'
                           im_sap2  = 'LV'
                           im_bcp47 = 'lv'
                           iv_text  = 'Latvian' ).

    fill_language_mapping( im_sap1  = 'X'
                           im_sap2  = 'LT'
                           im_bcp47 = 'lt'
                           iv_text  = 'Lithuanian' ).

    fill_language_mapping( im_sap1  = '7'
                           im_sap2  = 'MS'
                           im_bcp47 = 'ms'
                           iv_text  = 'Malay' ).

    fill_language_mapping( im_sap1  = '7'
                           im_sap2  = 'MS'
                           im_bcp47 = 'ms-MY'
                           iv_text  = 'Malay' ).

    fill_language_mapping( im_sap1  = 'B28D'
                           im_sap2  = '1M'
                           im_bcp47 = 'ms-BN'
                           iv_text  = 'Malay (Brunei)' ).

    fill_language_mapping( im_sap1  = 'O'
                           im_sap2  = 'NO'
                           im_bcp47 = 'no'
                           iv_text  = 'Norwegian' ).

    fill_language_mapping( im_sap1  = 'L'
                           im_sap2  = 'PL'
                           im_bcp47 = 'pl'
                           iv_text  = 'Polish' ).

    fill_language_mapping( im_sap1  = 'P'
                           im_sap2  = 'PT'
                           im_bcp47 = 'pt'
                           iv_text  = 'Portuguese' ).

    fill_language_mapping( im_sap1  = 'P'
                           im_sap2  = 'PT'
                           im_bcp47 = 'pt-BR'
                           iv_text  = 'Portuguese' ).

    fill_language_mapping( im_sap1  = 'BEED'
                           im_sap2  = 'RM'
                           im_bcp47 = 'rm'
                           iv_text  = 'Romansh' ).

    fill_language_mapping( im_sap1  = '4'
                           im_sap2  = 'RO'
                           im_bcp47 = 'ro'
                           iv_text  = 'Romanian' ).

    fill_language_mapping( im_sap1  = '4'
                           im_sap2  = 'RO'
                           im_bcp47 = 'ro-RO'
                           iv_text  = 'Romanian' ).

    fill_language_mapping( im_sap1  = 'R'
                           im_sap2  = 'RU'
                           im_bcp47 = 'ru-RU'
                           iv_text  = 'Russian' ).

    fill_language_mapping( im_sap1  = '0'
                           im_sap2  = 'SR'
                           im_bcp47 = 'sr'
                           iv_text  = 'Serbian' ).

    fill_language_mapping( im_sap1  = '0'
                           im_sap2  = 'SR'
                           im_bcp47 = 'sr-Cyrl'
                           iv_text  = 'Serbian' ).

    fill_language_mapping( im_sap1  = 'd'
                           im_sap2  = 'SH'
                           im_bcp47 = 'sr-Latn'
                           iv_text  = 'Serbian (Latin)' ).

    fill_language_mapping( im_sap1  = 'Q'
                           im_sap2  = 'SK'
                           im_bcp47 = 'sk'
                           iv_text  = 'Slovak' ).

    fill_language_mapping( im_sap1  = '5'
                           im_sap2  = 'SL'
                           im_bcp47 = 'sl'
                           iv_text  = 'Slovenian' ).

    fill_language_mapping( im_sap1  = 'BF42'
                           im_sap2  = 'SB'
                           im_bcp47 = 'wen'
                           iv_text  = '' ).

    fill_language_mapping( im_sap1  = 'B9B3'
                           im_sap2  = 'DS'
                           im_bcp47 = 'dsb'
                           iv_text  = '' ).

    fill_language_mapping( im_sap1  = 'BB33'
                           im_sap2  = 'HS'
                           im_bcp47 = 'hsb'
                           iv_text  = '' ).

    fill_language_mapping( im_sap1  = 'S'
                           im_sap2  = 'ES'
                           im_bcp47 = 'es'
                           iv_text  = 'Spanish' ).

    fill_language_mapping( im_sap1  = 'S'
                           im_sap2  = 'ES'
                           im_bcp47 = 'es-ES'
                           iv_text  = 'Spanish' ).

    fill_language_mapping( im_sap1  = 'B293'
                           im_sap2  = '1S'
                           im_bcp47 = 'es-AR'
                           iv_text  = 'Spanish (Argentina)' ).

    fill_language_mapping( im_sap1  = 'B2F3'
                           im_sap2  = '2S'
                           im_bcp47 = 'es-BO'
                           iv_text  = 'Spanish (Bolivia)' ).

    fill_language_mapping( im_sap1  = 'B353'
                           im_sap2  = '3S'
                           im_bcp47 = 'es-CL'
                           iv_text  = 'Spanish (Chile)' ).

    fill_language_mapping( im_sap1  = 'B233'
                           im_sap2  = '0S'
                           im_bcp47 = 'es-CO'
                           iv_text  = 'Spanish (Colombia)' ).

    fill_language_mapping( im_sap1  = 'B3B3'
                           im_sap2  = '4S'
                           im_bcp47 = 'es-CR'
                           iv_text  = 'Spanish (Costa Rica)' ).

    fill_language_mapping( im_sap1  = 'B413'
                           im_sap2  = '5S'
                           im_bcp47 = 'es-DO'
                           iv_text  = 'Spanish (Dominican Republic)' ).

    fill_language_mapping( im_sap1  = 'B473'
                           im_sap2  = '6S'
                           im_bcp47 = 'es-EC'
                           iv_text  = 'Spanish (Ecuador)' ).

    fill_language_mapping( im_sap1  = 'B4D3'
                           im_sap2  = '7S'
                           im_bcp47 = 'es-SV'
                           iv_text  = 'Spanish (El Salvador)' ).

    fill_language_mapping( im_sap1  = 'B533'
                           im_sap2  = '8S'
                           im_bcp47 = 'es-GT'
                           iv_text  = 'Spanish (Guatemala)' ).

    fill_language_mapping( im_sap1  = 'B593'
                           im_sap2  = '9S'
                           im_bcp47 = 'es-HN'
                           iv_text  = 'Spanish (Honduras)' ).

    fill_language_mapping( im_sap1  = 'B298'
                           im_sap2  = '1X'
                           im_bcp47 = 'es-MX'
                           iv_text  = 'Spanish (Mexico)' ).

    fill_language_mapping( im_sap1  = 'B2F8'
                           im_sap2  = '2X'
                           im_bcp47 = 'es-NI'
                           iv_text  = 'Spanish (Nicaragua)' ).

    fill_language_mapping( im_sap1  = 'B358'
                           im_sap2  = '3X'
                           im_bcp47 = 'es-PA'
                           iv_text  = 'Spanish (Panama)' ).

    fill_language_mapping( im_sap1  = 'B3B8'
                           im_sap2  = '4X'
                           im_bcp47 = 'es-PY'
                           iv_text  = 'Spanish (Paraguay)' ).

    fill_language_mapping( im_sap1  = 'B418'
                           im_sap2  = '5X'
                           im_bcp47 = 'es-PE'
                           iv_text  = 'Spanish (Peru)' ).

    fill_language_mapping( im_sap1  = 'B478'
                           im_sap2  = '6X'
                           im_bcp47 = 'es-PR'
                           iv_text  = 'Spanish (Puerto Rico)' ).

    fill_language_mapping( im_sap1  = 'B4D8'
                           im_sap2  = '7X'
                           im_bcp47 = 'es-UY'
                           iv_text  = 'Spanish (Uruguay)' ).

    fill_language_mapping( im_sap1  = 'B538'
                           im_sap2  = '8X'
                           im_bcp47 = 'es-VE'
                           iv_text  = 'Spanish (Venezuela)' ).

    fill_language_mapping( im_sap1  = 'BF57'
                           im_sap2  = 'SW'
                           im_bcp47 = 'sw'
                           iv_text  = 'Swahili' ).

    fill_language_mapping( im_sap1  = 'V'
                           im_sap2  = 'SV'
                           im_bcp47 = 'sv'
                           iv_text  = 'Swedish' ).

    fill_language_mapping( im_sap1  = 'BFAC'
                           im_sap2  = 'TL'
                           im_bcp47 = 'tl'
                           iv_text  = 'Tagalog' ).

    fill_language_mapping( im_sap1  = '2'
                           im_sap2  = 'TH'
                           im_bcp47 = 'th'
                           iv_text  = 'Thai' ).

    fill_language_mapping( im_sap1  = 'T'
                           im_sap2  = 'TR'
                           im_bcp47 = 'tr'
                           iv_text  = 'Turkish' ).

    fill_language_mapping( im_sap1  = '8'
                           im_sap2  = 'UK'
                           im_bcp47 = 'uk'
                           iv_text  = 'Ukrainian' ).

    fill_language_mapping( im_sap1  = 'C069'
                           im_sap2  = 'VI'
                           im_bcp47 = 'vi'
                           iv_text  = 'Vietnamese' ).

    fill_language_mapping( im_sap1  = 'C0C1'
                           im_sap2  = 'WA'
                           im_bcp47 = 'wa'
                           iv_text  = 'Wa' ).

    fill_language_mapping( im_sap1  = 'Z'
                           im_sap2  = 'Z1'
                           im_bcp47 = 'z1'
                           iv_text  = 'Customer reserve' ).
  ENDMETHOD.


ENDCLASS.
