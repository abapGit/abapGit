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
    DATA language_mapping TYPE ty_language_mapping.

    IF language_mappings IS INITIAL OR lines( language_mappings ) = 0.
      fill_language_mappings( ).
    ENDIF.

    LOOP AT language_mappings INTO language_mapping WHERE bcp47_code = bcp47.
      IF sap1 IS INITIAL.
        sap1 = language_mapping-sap1_code.
      ENDIF.
    ENDLOOP.

    IF sap1 IS INITIAL.
      RAISE no_assignment.
    ENDIF.
  ENDMETHOD.

  METHOD fill_language_mappings.
    DATA line TYPE ty_language_mapping.

    line-bcp47_code = 'af'.
    line-sap1_code = 'a'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'sq'.
    line-sap1_code = '뽑'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'eu'.
    line-sap1_code = '먕'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'bs'.
    line-sap1_code = '룳'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'bg'.
    line-sap1_code = 'W'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'ca'.
    line-sap1_code = 'c'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'zh'.
    line-sap1_code = '1'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'zh-Hans'.
    line-sap1_code = '1'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'zh-SG'.
    line-sap1_code = '덃'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'zh-Hant'.
    line-sap1_code = 'M'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'hr'.
    line-sap1_code = '6'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'cs'.
    line-sap1_code = 'C'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'da'.
    line-sap1_code = 'K'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'nl'.
    line-sap1_code = 'N'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'nl-NL'.
    line-sap1_code = 'N'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'nl-BE'.
    line-sap1_code = '늄'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en'.
    line-sap1_code = 'E'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-US'.
    line-sap1_code = 'E'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-GB'.
    line-sap1_code = '둮'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-AU'.
    line-sap1_code = '늅'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-BZ'.
    line-sap1_code = '닥'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-CA'.
    line-sap1_code = '덅'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-HK'.
    line-sap1_code = '됅'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-IN'.
    line-sap1_code = '둥'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-ID'.
    line-sap1_code = '듅'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-IE'.
    line-sap1_code = '딥'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-JM'.
    line-sap1_code = '떅'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-MY'.
    line-sap1_code = '눥'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-NZ'.
    line-sap1_code = '늎'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-PH'.
    line-sap1_code = '닮'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-SG'.
    line-sap1_code = '덎'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-ZA'.
    line-sap1_code = '뎮'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-TT'.
    line-sap1_code = '됎'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'en-ZW'.
    line-sap1_code = '듎'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'et'.
    line-sap1_code = '9'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fi'.
    line-sap1_code = 'U'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fr'.
    line-sap1_code = 'F'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fr-FR'.
    line-sap1_code = 'F'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fr-BE'.
    line-sap1_code = '늆'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fr-CM'.
    line-sap1_code = '닦'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fr-CA'.
    line-sap1_code = '덆'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fr-CG'.
    line-sap1_code = '뎦'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fr-CI'.
    line-sap1_code = '됆'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fr-HT'.
    line-sap1_code = '둦'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fr-LU'.
    line-sap1_code = '듆'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fr-ML'.
    line-sap1_code = '딦'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fr-MC'.
    line-sap1_code = '떆'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fr-MA'.
    line-sap1_code = '늈'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fr-RE'.
    line-sap1_code = '단'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fr-SN'.
    line-sap1_code = '덈'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'fr-CH'.
    line-sap1_code = '뎨'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'gd'.
    line-sap1_code = '뫄'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'gl'.
    line-sap1_code = '뫌'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'de'.
    line-sap1_code = 'D'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'de-DE'.
    line-sap1_code = 'D'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'de-AT'.
    line-sap1_code = '늇'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'de-LI'.
    line-sap1_code = '닧'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'de-LU'.
    line-sap1_code = '덇'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'de-CH'.
    line-sap1_code = '뎧'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'el'.
    line-sap1_code = 'G'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'he'.
    line-sap1_code = 'B'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'hu'.
    line-sap1_code = 'H'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'is'.
    line-sap1_code = 'b'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'id'.
    line-sap1_code = 'i'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'ga'.
    line-sap1_code = '뫁'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'it'.
    line-sap1_code = 'I'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'it-IT'.
    line-sap1_code = 'I'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'it-CH'.
    line-sap1_code = '늉'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'ja'.
    line-sap1_code = 'J'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'ko'.
    line-sap1_code = '3'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'ko-KR'.
    line-sap1_code = '3'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'lv'.
    line-sap1_code = 'Y'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'lt'.
    line-sap1_code = 'X'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'ms'.
    line-sap1_code = '7'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'ms-MY'.
    line-sap1_code = '7'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'ms-BN'.
    line-sap1_code = '늍'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'no'.
    line-sap1_code = 'O'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'pl'.
    line-sap1_code = 'L'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'pt'.
    line-sap1_code = 'P'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'pt-BR'.
    line-sap1_code = 'P'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'rm'.
    line-sap1_code = '뻭'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'ro'.
    line-sap1_code = '4'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'ro-RO'.
    line-sap1_code = '4'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'ru-RU'.
    line-sap1_code = 'R'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'sr'.
    line-sap1_code = '0'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'sr-Cyrl'.
    line-sap1_code = '0'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'sr-Latn'.
    line-sap1_code = 'd'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'sk'.
    line-sap1_code = 'Q'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'sl'.
    line-sap1_code = '5'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'wen'.
    line-sap1_code = '뽂'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'dsb'.
    line-sap1_code = '릳'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'hsb'.
    line-sap1_code = '묳'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es'.
    line-sap1_code = 'S'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-ES'.
    line-sap1_code = 'S'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-AR'.
    line-sap1_code = '늓'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-BO'.
    line-sap1_code = '닳'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-CL'.
    line-sap1_code = '덓'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-CO'.
    line-sap1_code = '눳'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-CR'.
    line-sap1_code = '뎳'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-DO'.
    line-sap1_code = '됓'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-EC'.
    line-sap1_code = '둳'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-SV'.
    line-sap1_code = '듓'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-GT'.
    line-sap1_code = '딳'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-HN'.
    line-sap1_code = '떓'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-MX'.
    line-sap1_code = '늘'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-NI'.
    line-sap1_code = '닸'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-PA'.
    line-sap1_code = '던'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-PY'.
    line-sap1_code = '뎸'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-PE'.
    line-sap1_code = '되'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-PR'.
    line-sap1_code = '둸'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-UY'.
    line-sap1_code = '듘'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'es-VE'.
    line-sap1_code = '딸'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'sw'.
    line-sap1_code = '뽗'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'sv'.
    line-sap1_code = 'V'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'tl'.
    line-sap1_code = '뾬'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'th'.
    line-sap1_code = '2'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'tr'.
    line-sap1_code = 'T'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'uk'.
    line-sap1_code = '8'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'vi'.
    line-sap1_code = '쁩'.
    APPEND line TO language_mappings.
    CLEAR line.

    line-bcp47_code = 'wa'.
    line-sap1_code = '상'.
    APPEND line TO language_mappings.
    CLEAR line.

  ENDMETHOD.

ENDCLASS.
