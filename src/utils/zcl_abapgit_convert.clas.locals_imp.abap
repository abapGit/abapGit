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
           tt_language_mappings TYPE STANDARD TABLE OF ty_language_mapping WITH DEFAULT KEY.
    CLASS-DATA language_mappings TYPE tt_language_mappings.
    CLASS-METHODS:
      sap1_to_bcp47
        IMPORTING
          sap1         TYPE sy-langu
        RETURNING
          VALUE(bcp47) TYPE string
        EXCEPTIONS
          no_assignment,
      bcp47_to_sap1
        IMPORTING
          bcp47       TYPE string
        RETURNING
          VALUE(sap1) TYPE sy-langu
        EXCEPTIONS
          no_assignment.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS fill_language_mappings.

ENDCLASS.

CLASS lcl_bcp47_language_table IMPLEMENTATION.

  METHOD sap1_to_bcp47.
    DATA language_mapping TYPE ty_language_mapping.

    IF language_mappings IS INITIAL OR lines( language_mappings ) = 0.
      fill_language_mappings(  ).
    ENDIF.

    LOOP AT language_mappings INTO language_mapping WHERE sap1_code = sap1.
      IF bcp47 IS INITIAL OR strlen( bcp47 ) > strlen( language_mapping-bcp47_code ).
        bcp47 = language_mapping-bcp47_code.
      ENDIF.
    ENDLOOP.

    IF bcp47 IS INITIAL.
      RAISE no_assignment.
    ENDIF.
  ENDMETHOD.

  METHOD bcp47_to_sap1.
    IF language_mappings IS INITIAL OR lines( language_mappings ) = 0.
      fill_language_mappings(  ).
    ENDIF.

    sap1 = language_mappings[ bcp47_code = bcp47 ]-sap1_code.

    IF sap1 IS INITIAL.
      RAISE no_assignment.
    ENDIF.
  ENDMETHOD.

  METHOD fill_language_mappings.
    language_mappings = VALUE #(
      ( bcp47_code = 'af' sap1_code = 'a' )
      ( bcp47_code = 'sq' sap1_code = '뽑' )
      ( bcp47_code = 'eu' sap1_code = '먕' )
      ( bcp47_code = 'bs' sap1_code = '룳' )
      ( bcp47_code = 'bg' sap1_code = 'W' )
      ( bcp47_code = 'ca' sap1_code = 'c' )
      ( bcp47_code = 'zh' sap1_code = '1' )
      ( bcp47_code = 'zh-Hans' sap1_code = '1' )
      ( bcp47_code = 'zh-SG' sap1_code = '덃' )
      ( bcp47_code = 'zh-Hant' sap1_code = 'M' )
      ( bcp47_code = 'hr' sap1_code = '6' )
      ( bcp47_code = 'cs' sap1_code = 'C' )
      ( bcp47_code = 'da' sap1_code = 'K' )
      ( bcp47_code = 'nl' sap1_code = 'N' )
      ( bcp47_code = 'nl-NL' sap1_code = 'N' )
      ( bcp47_code = 'nl-BE' sap1_code = '늄' )
      ( bcp47_code = 'en' sap1_code = 'E' )
      ( bcp47_code = 'en-US' sap1_code = 'E' )
      ( bcp47_code = 'en-GB' sap1_code = '둮' )
      ( bcp47_code = 'en-AU' sap1_code = '늅' )
      ( bcp47_code = 'en-BZ' sap1_code = '닥' )
      ( bcp47_code = 'en-CA' sap1_code = '덅' )
      ( bcp47_code = 'en-HK' sap1_code = '됅' )
      ( bcp47_code = 'en-IN' sap1_code = '둥' )
      ( bcp47_code = 'en-ID' sap1_code = '듅' )
      ( bcp47_code = 'en-IE' sap1_code = '딥' )
      ( bcp47_code = 'en-JM' sap1_code = '떅' )
      ( bcp47_code = 'en-MY' sap1_code = '눥' )
      ( bcp47_code = 'en-NZ' sap1_code = '늎' )
      ( bcp47_code = 'en-PH' sap1_code = '닮' )
      ( bcp47_code = 'en-SG' sap1_code = '덎' )
      ( bcp47_code = 'en-ZA' sap1_code = '뎮' )
      ( bcp47_code = 'en-TT' sap1_code = '됎' )
      ( bcp47_code = 'en-ZW' sap1_code = '듎' )
      ( bcp47_code = 'et' sap1_code = '9' )
      ( bcp47_code = 'fi' sap1_code = 'U' )
      ( bcp47_code = 'fr' sap1_code = 'F' )
      ( bcp47_code = 'fr-FR' sap1_code = 'F' )
      ( bcp47_code = 'fr-BE' sap1_code = '늆' )
      ( bcp47_code = 'fr-CM' sap1_code = '닦' )
      ( bcp47_code = 'fr-CA' sap1_code = '덆' )
      ( bcp47_code = 'fr-CG' sap1_code = '뎦' )
      ( bcp47_code = 'fr-CI' sap1_code = '됆' )
      ( bcp47_code = 'fr-HT' sap1_code = '둦' )
      ( bcp47_code = 'fr-LU' sap1_code = '듆' )
      ( bcp47_code = 'fr-ML' sap1_code = '딦' )
      ( bcp47_code = 'fr-MC' sap1_code = '떆' )
      ( bcp47_code = 'fr-MA' sap1_code = '늈' )
      ( bcp47_code = 'fr-RE' sap1_code = '단' )
      ( bcp47_code = 'fr-SN' sap1_code = '덈' )
      ( bcp47_code = 'fr-CH' sap1_code = '뎨' )
      ( bcp47_code = 'gd' sap1_code = '뫄' )
      ( bcp47_code = 'gl' sap1_code = '뫌' )
      ( bcp47_code = 'de' sap1_code = 'D' )
      ( bcp47_code = 'de-DE' sap1_code = 'D' )
      ( bcp47_code = 'de-AT' sap1_code = '늇' )
      ( bcp47_code = 'de-LI' sap1_code = '닧' )
      ( bcp47_code = 'de-LU' sap1_code = '덇' )
      ( bcp47_code = 'de-CH' sap1_code = '뎧' )
      ( bcp47_code = 'el' sap1_code = 'G' )
      ( bcp47_code = 'he' sap1_code = 'B' )
      ( bcp47_code = 'hu' sap1_code = 'H' )
      ( bcp47_code = 'is' sap1_code = 'b' )
      ( bcp47_code = 'id' sap1_code = 'i' )
      ( bcp47_code = 'ga' sap1_code = '뫁' )
      ( bcp47_code = 'it' sap1_code = 'I' )
      ( bcp47_code = 'it-IT' sap1_code = 'I' )
      ( bcp47_code = 'it-CH' sap1_code = '늉' )
      ( bcp47_code = 'ja' sap1_code = 'J' )
      ( bcp47_code = 'ko' sap1_code = '3' )
      ( bcp47_code = 'ko-KR' sap1_code = '3' )
      ( bcp47_code = 'lv' sap1_code = 'Y' )
      ( bcp47_code = 'lt' sap1_code = 'X' )
      ( bcp47_code = 'ms' sap1_code = '7' )
      ( bcp47_code = 'ms-MY' sap1_code = '7' )
      ( bcp47_code = 'ms-BN' sap1_code = '늍' )
      ( bcp47_code = 'no' sap1_code = 'O' )
      ( bcp47_code = 'pl' sap1_code = 'L' )
      ( bcp47_code = 'pt' sap1_code = 'P' )
      ( bcp47_code = 'pt-BR' sap1_code = 'P' )
      ( bcp47_code = 'rm' sap1_code = '뻭' )
      ( bcp47_code = 'ro' sap1_code = '4' )
      ( bcp47_code = 'ro-RO' sap1_code = '4' )
      ( bcp47_code = 'ru-RU' sap1_code = 'R' )
      ( bcp47_code = 'sr' sap1_code = '0' )
      ( bcp47_code = 'sr-Cyrl' sap1_code = '0' )
      ( bcp47_code = 'sr-Latn' sap1_code = 'd' )
      ( bcp47_code = 'sk' sap1_code = 'Q' )
      ( bcp47_code = 'sl' sap1_code = '5' )
      ( bcp47_code = 'wen' sap1_code = '뽂' )
      ( bcp47_code = 'dsb' sap1_code = '릳' )
      ( bcp47_code = 'hsb' sap1_code = '묳' )
      ( bcp47_code = 'es' sap1_code = 'S' )
      ( bcp47_code = 'es-ES' sap1_code = 'S' )
      ( bcp47_code = 'es-AR' sap1_code = '늓' )
      ( bcp47_code = 'es-BO' sap1_code = '닳' )
      ( bcp47_code = 'es-CL' sap1_code = '덓' )
      ( bcp47_code = 'es-CO' sap1_code = '눳' )
      ( bcp47_code = 'es-CR' sap1_code = '뎳' )
      ( bcp47_code = 'es-DO' sap1_code = '됓' )
      ( bcp47_code = 'es-EC' sap1_code = '둳' )
      ( bcp47_code = 'es-SV' sap1_code = '듓' )
      ( bcp47_code = 'es-GT' sap1_code = '딳' )
      ( bcp47_code = 'es-HN' sap1_code = '떓' )
      ( bcp47_code = 'es-MX' sap1_code = '늘' )
      ( bcp47_code = 'es-NI' sap1_code = '닸' )
      ( bcp47_code = 'es-PA' sap1_code = '던' )
      ( bcp47_code = 'es-PY' sap1_code = '뎸' )
      ( bcp47_code = 'es-PE' sap1_code = '되' )
      ( bcp47_code = 'es-PR' sap1_code = '둸' )
      ( bcp47_code = 'es-UY' sap1_code = '듘' )
      ( bcp47_code = 'es-VE' sap1_code = '딸' )
      ( bcp47_code = 'sw' sap1_code = '뽗' )
      ( bcp47_code = 'sv' sap1_code = 'V' )
      ( bcp47_code = 'tl' sap1_code = '뾬' )
      ( bcp47_code = 'th' sap1_code = '2' )
      ( bcp47_code = 'tr' sap1_code = 'T' )
      ( bcp47_code = 'uk' sap1_code = '8' )
      ( bcp47_code = 'vi' sap1_code = '쁩' )
      ( bcp47_code = 'wa' sap1_code = '상' )
     ).
  ENDMETHOD.

ENDCLASS.
