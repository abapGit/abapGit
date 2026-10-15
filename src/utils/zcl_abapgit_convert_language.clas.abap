CLASS zcl_abapgit_convert_language DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      ty_char02 TYPE c LENGTH 2.

    CLASS-METHODS conversion_exit_isola_output
      IMPORTING
        !iv_spras       TYPE spras
      RETURNING
        VALUE(rv_spras) TYPE laiso.

    CLASS-METHODS sap1_to_sap2
      IMPORTING
        !im_lang_sap1       TYPE sy-langu
      RETURNING
        VALUE(re_lang_sap2) TYPE string
      EXCEPTIONS
        no_assignment.
    CLASS-METHODS sap1_to_text
      IMPORTING
        !im_lang_sap1  TYPE sy-langu
      RETURNING
        VALUE(re_text) TYPE string.

    CLASS-METHODS sap2_to_sap1
      IMPORTING
        !im_lang_sap2       TYPE laiso
      RETURNING
        VALUE(re_lang_sap1) TYPE sy-langu
      EXCEPTIONS
        no_assignment.
    CLASS-METHODS sap1_to_bcp47
      IMPORTING
        !im_lang_sap1        TYPE sy-langu
      RETURNING
        VALUE(re_lang_bcp47) TYPE string
      EXCEPTIONS
        no_assignment.
    CLASS-METHODS bcp47_to_sap1
      IMPORTING
        !im_lang_bcp47      TYPE string
      RETURNING
        VALUE(re_lang_sap1) TYPE sy-langu
      EXCEPTIONS
        no_assignment.
    CLASS-METHODS uccp
      IMPORTING
        !iv_uccp       TYPE string
      RETURNING
        VALUE(rv_char) TYPE ty_char02
      EXCEPTIONS
        no_assignment.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_convert_language IMPLEMENTATION.


  METHOD bcp47_to_sap1.
    DATA lv_converter_instance TYPE REF TO object.
    DATA lv_converter_class_name TYPE string VALUE `CL_AFF_LANGUAGE_CONVERTER`.
    DATA lv_regex TYPE REF TO cl_abap_regex.
    DATA lv_abap_matcher TYPE REF TO cl_abap_matcher.

    DATA lv_sap2_lang_code TYPE laiso.

    TRY.
        CALL METHOD (lv_converter_class_name)=>create_instance
          RECEIVING
            result = lv_converter_instance.

        TRY.
            CALL METHOD lv_converter_instance->(`IF_AFF_LANGUAGE_CONVERTER~BCP47_TO_SAP1`)
              EXPORTING
                language = im_lang_bcp47
              RECEIVING
                result   = re_lang_sap1.

          CATCH cx_static_check.
            RAISE no_assignment.
        ENDTRY.

      CATCH cx_sy_dyn_call_error.
        TRY.
            re_lang_sap1 = lcl_bcp47_language_table=>bcp47_to_sap1( im_lang_bcp47 ).
          CATCH zcx_abapgit_exception.

            CREATE OBJECT lv_regex EXPORTING pattern = `[A-Z0-9]{2}` ##REGEX_POSIX.
            lv_abap_matcher = lv_regex->create_matcher( text = im_lang_bcp47 ).

            IF abap_true = lv_abap_matcher->match( ).
              "Fallback try to convert from SAP language
              lv_sap2_lang_code = im_lang_bcp47.

              sap2_to_sap1(
                EXPORTING
                  im_lang_sap2  = lv_sap2_lang_code
                RECEIVING
                  re_lang_sap1  = re_lang_sap1
                EXCEPTIONS
                  no_assignment = 1
                  OTHERS        = 2 ).
              IF sy-subrc <> 0.
                RAISE no_assignment.
              ENDIF.

            ELSE.
              RAISE no_assignment.
            ENDIF.
        ENDTRY.
    ENDTRY.
  ENDMETHOD.


  METHOD conversion_exit_isola_output.

    sap1_to_sap2(
      EXPORTING
        im_lang_sap1  = iv_spras
      RECEIVING
        re_lang_sap2  = rv_spras
      EXCEPTIONS
        no_assignment = 1
        OTHERS        = 2 ).                              "#EC CI_SUBRC

    TRANSLATE rv_spras TO UPPER CASE.

  ENDMETHOD.


  METHOD sap1_to_bcp47.
    DATA lv_converter_instance TYPE REF TO object.
    DATA lv_converter_class_name TYPE string VALUE `CL_AFF_LANGUAGE_CONVERTER`.

    TRY.
        CALL METHOD (lv_converter_class_name)=>create_instance
          RECEIVING
            result = lv_converter_instance.

        TRY.
            CALL METHOD lv_converter_instance->(`IF_AFF_LANGUAGE_CONVERTER~SAP1_TO_BCP47`)
              EXPORTING
                language = im_lang_sap1
              RECEIVING
                result   = re_lang_bcp47.
          CATCH cx_static_check.
            RAISE no_assignment.
        ENDTRY.
      CATCH cx_sy_dyn_call_error.
        TRY.
            re_lang_bcp47 = lcl_bcp47_language_table=>sap1_to_bcp47( im_lang_sap1 ).
          CATCH zcx_abapgit_exception.
            RAISE no_assignment.
        ENDTRY.
    ENDTRY.
  ENDMETHOD.


  METHOD sap1_to_sap2.

    TRY.
        re_lang_sap2 = lcl_bcp47_language_table=>sap1_to_sap2( im_lang_sap1 ).
      CATCH zcx_abapgit_exception.
        RAISE no_assignment.
    ENDTRY.

  ENDMETHOD.


  METHOD sap1_to_text.
    re_text = lcl_bcp47_language_table=>sap1_to_text( im_lang_sap1 ).
  ENDMETHOD.


  METHOD sap2_to_sap1.

    TRY.
        re_lang_sap1 = lcl_bcp47_language_table=>sap2_to_sap1( im_lang_sap2 ).
      CATCH zcx_abapgit_exception.
        RAISE no_assignment.
    ENDTRY.

  ENDMETHOD.


  METHOD uccp.

    DATA lv_class    TYPE string.
    DATA lv_xstr     TYPE xstring.
    DATA lo_instance TYPE REF TO object.

    lv_class = 'CL_ABAP_CONV_IN_CE'.

    TRY.
        CALL METHOD (lv_class)=>uccp
          EXPORTING
            uccp = iv_uccp
          RECEIVING
            char = rv_char.
      CATCH cx_sy_dyn_call_illegal_class.
        lv_xstr = iv_uccp.

        CALL METHOD ('CL_ABAP_CONV_CODEPAGE')=>create_in
          EXPORTING
            codepage = 'UTF-16'
          RECEIVING
            instance = lo_instance.

* convert endianness
        CONCATENATE lv_xstr+1(1) lv_xstr(1) INTO lv_xstr IN BYTE MODE.

        CALL METHOD lo_instance->('IF_ABAP_CONV_IN~CONVERT')
          EXPORTING
            source = lv_xstr
          RECEIVING
            result = rv_char.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
