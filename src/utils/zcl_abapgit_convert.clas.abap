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
    CLASS-METHODS xstring_to_string_utf8_bom
      IMPORTING
        !iv_xstring      TYPE xstring
      RETURNING
        VALUE(rv_string) TYPE string
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

    CLASS-METHODS language_sap1_to_sap2
      IMPORTING
        im_lang_sap1        TYPE sy-langu
      RETURNING
        VALUE(re_lang_sap2) TYPE string
      EXCEPTIONS
        no_assignment.

    CLASS-METHODS language_sap2_to_sap1
      IMPORTING
        im_lang_sap2        TYPE laiso
      RETURNING
        VALUE(re_lang_sap1) TYPE sy-langu
      EXCEPTIONS
        no_assignment.

    CLASS-METHODS language_sap1_to_bcp47
      IMPORTING
        im_lang_sap1         TYPE sy-langu
      RETURNING
        VALUE(re_lang_bcp47) TYPE string
      EXCEPTIONS
        no_assignment.

    CLASS-METHODS language_bcp47_to_sap1
      IMPORTING
        im_lang_bcp47       TYPE string
      RETURNING
        VALUE(re_lang_sap1) TYPE sy-langu
      EXCEPTIONS
        no_assignment.

    TYPES ty_char02 TYPE c LENGTH 2.
    CLASS-METHODS uccp
      IMPORTING
        iv_uccp        TYPE string
      RETURNING
        VALUE(rv_char) TYPE ty_char02
      EXCEPTIONS
        no_assignment.

  PROTECTED SECTION.
  PRIVATE SECTION.
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
        "Initialize
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

    language_sap1_to_sap2(
      EXPORTING
        im_lang_sap1  = iv_spras
      RECEIVING
        re_lang_sap2  = rv_spras
      EXCEPTIONS
        no_assignment = 1
        OTHERS        = 2 ).                              "#EC CI_SUBRC

    TRANSLATE rv_spras TO UPPER CASE.

  ENDMETHOD.


  METHOD int_to_xstring4.
* returns xstring of length 4 containing the integer value iv_i

    DATA lv_x TYPE x LENGTH 4.

    lv_x = iv_i.
    rv_xstring = lv_x.

  ENDMETHOD.

  METHOD language_sap1_to_bcp47.
    DATA lv_converter_instance TYPE REF TO object.
    DATA lv_converter_class_name TYPE string VALUE `CL_AFF_LANGUAGE_CONVERTER`.

    TRY.
        CALL METHOD (lv_converter_class_name)=>create_instance
          RECEIVING
            result = lv_converter_instance.

        TRY.
            CALL METHOD lv_converter_instance->(`IF_AFF_LANGUAGE_CONVERTER~SAP1_TO_BCP47`)
              EXPORTING
                language      = im_lang_sap1
              RECEIVING
                result        = re_lang_bcp47.
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

  METHOD language_bcp47_to_sap1.
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

            CREATE OBJECT lv_regex EXPORTING pattern = `[A-Z0-9]{2}`.
            lv_abap_matcher = lv_regex->create_matcher( text = im_lang_bcp47 ).

            IF abap_true = lv_abap_matcher->match( ).
              "Fallback try to convert from SAP language
              lv_sap2_lang_code = im_lang_bcp47.

              language_sap2_to_sap1(
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

  METHOD language_sap1_to_sap2.

    DATA lv_class TYPE string.

    TRY.
        SELECT SINGLE languageisocode FROM ('I_LANGUAGE')
          INTO re_lang_sap2
          WHERE language = im_lang_sap1.
        IF sy-subrc <> 0.
          RAISE no_assignment.
        ENDIF.
      CATCH cx_sy_dynamic_osql_error.
        lv_class = 'CL_I18N_LANGUAGES'.
        CALL METHOD (lv_class)=>sap1_to_sap2
          EXPORTING
            im_lang_sap1  = im_lang_sap1
          RECEIVING
            re_lang_sap2  = re_lang_sap2
          EXCEPTIONS
            no_assignment = 1
            OTHERS        = 2.
        IF sy-subrc = 1.
          RAISE no_assignment.
        ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD language_sap2_to_sap1.

    DATA lv_class TYPE string.

    TRY.
        SELECT SINGLE language FROM ('I_LANGUAGE')
          INTO re_lang_sap1
          WHERE languageisocode = im_lang_sap2.
        IF sy-subrc <> 0.
          RAISE no_assignment.
        ENDIF.
      CATCH cx_sy_dynamic_osql_error.
        lv_class = 'CL_I18N_LANGUAGES'.
        CALL METHOD (lv_class)=>sap2_to_sap1
          EXPORTING
            im_lang_sap2  = im_lang_sap2
          RECEIVING
            re_lang_sap1  = re_lang_sap1
          EXCEPTIONS
            no_assignment = 1
            OTHERS        = 2.
        IF sy-subrc = 1.
          RAISE no_assignment.
        ENDIF.
    ENDTRY.
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
    lv_length = cl_abap_typedescr=>describe_by_data( <lg_line> )->length / cl_abap_char_utilities=>charsize.
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

    rv_xstring = lcl_out=>convert( iv_string ).

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
    DATA lv_struct TYPE abap_bool.

    FIELD-SYMBOLS <lg_line> TYPE any.


    CLEAR et_bintab.
    ev_size = xstrlen( iv_xstr ).

    APPEND INITIAL LINE TO et_bintab ASSIGNING <lg_line>.
    lv_struct = boolc(
      cl_abap_typedescr=>describe_by_data( <lg_line> )->type_kind = cl_abap_typedescr=>typekind_struct1 ).
    IF lv_struct = abap_true.
      ASSIGN COMPONENT 1 OF STRUCTURE <lg_line> TO <lg_line>.
    ENDIF.
    <lg_line> = iv_xstr.

    lv_length = cl_abap_typedescr=>describe_by_data( <lg_line> )->length.
    lv_iterations = ev_size DIV lv_length.

    DO lv_iterations TIMES.
      lv_offset = sy-index * lv_length.
      APPEND INITIAL LINE TO et_bintab ASSIGNING <lg_line>.
      IF lv_struct = abap_true.
        ASSIGN COMPONENT 1 OF STRUCTURE <lg_line> TO <lg_line>.
      ENDIF.
      <lg_line> = iv_xstr+lv_offset.
    ENDDO.

  ENDMETHOD.


  METHOD xstring_to_int.

* use the built-in type conversion
    rv_i = iv_xstring.

  ENDMETHOD.


  METHOD xstring_to_string_utf8.

    DATA lv_data   TYPE xstring.
    DATA lv_length TYPE i.

    " Remove BOM for non-Unicode systems
    lv_data = xstring_remove_bom( iv_data ).

    lv_length = iv_length.
    IF lv_length <= 0.
      lv_length = xstrlen( lv_data ).
    ENDIF.

    rv_string = lcl_in=>convert(
      iv_data   = lv_data
      iv_length = lv_length ).

  ENDMETHOD.


  METHOD xstring_to_string_utf8_bom.

    DATA lv_xstring TYPE xstring.

    IF iv_xstring IS INITIAL.
      RETURN.
    ENDIF.

    lv_xstring = iv_xstring.
    " Add UTF-8 BOM
    IF xstrlen( lv_xstring ) < 3 OR lv_xstring(3) <> cl_abap_char_utilities=>byte_order_mark_utf8.
      lv_xstring = cl_abap_char_utilities=>byte_order_mark_utf8 && lv_xstring.
    ENDIF.

    rv_string = xstring_to_string_utf8( lv_xstring ).

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
