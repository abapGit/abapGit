CLASS ltcl_bcp47_to_sap1 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS assert_bcp47_to_sap1
      IMPORTING
        im_from TYPE string
        im_to   TYPE sy-langu.

    METHODS assert_bcp47_to_sap1_fail
      IMPORTING
        im_from TYPE string.

    METHODS english FOR TESTING.
    METHODS english_us FOR TESTING.
    METHODS english_gb FOR TESTING.
    METHODS german FOR TESTING.
    METHODS german_germany FOR TESTING.
    METHODS german_swiss FOR TESTING.
    METHODS chinese FOR TESTING.
    METHODS chinese_singapore FOR TESTING.
    METHODS chinese_taiwan FOR TESTING.
    METHODS no_value FOR TESTING.
    METHODS unsupported FOR TESTING.
    METHODS unsupported_uppercase FOR TESTING.
    METHODS too_many_characters FOR TESTING.
    METHODS too_less_characters FOR TESTING.
    METHODS english_uppercase FOR TESTING.
    METHODS chinese_uppercase FOR TESTING.

ENDCLASS.

CLASS ltcl_bcp47_to_sap1 IMPLEMENTATION.

  METHOD assert_bcp47_to_sap1.
    DATA lv_result TYPE sy-langu.
    lv_result = zcl_abapgit_convert_language=>bcp47_to_sap1( im_from ).

    cl_abap_unit_assert=>assert_equals(
      exp = im_to
      act = lv_result
      msg = |Converting "{ im_from }" should result in "{ im_to }"| ).
  ENDMETHOD.

  METHOD assert_bcp47_to_sap1_fail.
    DATA lv_result TYPE string.
    DATA lv_act TYPE sy-subrc.

    zcl_abapgit_convert_language=>bcp47_to_sap1(
      EXPORTING
        im_lang_bcp47 = im_from
      RECEIVING
        re_lang_sap1  = lv_result
      EXCEPTIONS
        no_assignment = 1
        OTHERS        = 2 ).

    " Assert itself might change sy-subrc (it does in 702!)
    lv_act = sy-subrc.

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lv_act
      msg = |Converting "{ im_from }" should fail| ).
  ENDMETHOD.

  METHOD english.
    assert_bcp47_to_sap1( im_from = 'en'
                          im_to   = 'E' ).
  ENDMETHOD.

  METHOD english_uppercase.
    assert_bcp47_to_sap1( im_from = 'EN'
                          im_to   = 'E' ).
  ENDMETHOD.

  METHOD english_us.
    assert_bcp47_to_sap1( im_from = 'en-US'
                          im_to   = 'E' ).
  ENDMETHOD.

  METHOD english_gb.
    DATA lv_to TYPE sy-langu.
    lv_to = zcl_abapgit_convert_language=>uccp( 'B46E' ).

    assert_bcp47_to_sap1( im_from = 'en-GB'
                          im_to   = lv_to ).
  ENDMETHOD.

  METHOD german.
    assert_bcp47_to_sap1( im_from = 'de'
                          im_to   = 'D' ).
  ENDMETHOD.

  METHOD german_germany.
    assert_bcp47_to_sap1( im_from = 'de-DE'
                          im_to   = 'D' ).
  ENDMETHOD.

  METHOD german_swiss.
    DATA lv_to TYPE sy-langu.
    lv_to = zcl_abapgit_convert_language=>uccp( 'B3A7' ).

    assert_bcp47_to_sap1( im_from = 'de-CH'
                          im_to   = lv_to ).
  ENDMETHOD.

  METHOD chinese.
    assert_bcp47_to_sap1( im_from = 'zh'
                          im_to   = '1' ).
  ENDMETHOD.

  METHOD chinese_uppercase.
    assert_bcp47_to_sap1( im_from = 'ZH'
                          im_to   = '1' ).
  ENDMETHOD.

  METHOD chinese_singapore.
    DATA lv_to TYPE sy-langu.
    lv_to = zcl_abapgit_convert_language=>uccp( 'B343' ).

    assert_bcp47_to_sap1( im_from = 'zh-SG'
                          im_to   = lv_to ).
  ENDMETHOD.

  METHOD chinese_taiwan.
    assert_bcp47_to_sap1( im_from = 'zh-Hant'
                          im_to   = 'M' ).
  ENDMETHOD.

  METHOD no_value.
    assert_bcp47_to_sap1_fail( '' ).
  ENDMETHOD.

  METHOD unsupported.
    assert_bcp47_to_sap1_fail( 'xx' ).
  ENDMETHOD.

  METHOD unsupported_uppercase.
    assert_bcp47_to_sap1_fail( 'XX' ).
  ENDMETHOD.

  METHOD too_many_characters.
    assert_bcp47_to_sap1_fail( 'eng' ).
  ENDMETHOD.

  METHOD too_less_characters.
    assert_bcp47_to_sap1_fail( 'e' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_sap1_to_bcp47 DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS assert_sap1_to_bcp47
      IMPORTING
        im_from TYPE sy-langu
        im_to   TYPE string.

    METHODS assert_sap1_to_bcp47_fail
      IMPORTING
        im_from TYPE sy-langu.

    METHODS english FOR TESTING.
    METHODS english_gb FOR TESTING.
    METHODS chinese FOR TESTING.
    METHODS chinese_singapore FOR TESTING.
    METHODS chinese_taiwan FOR TESTING.
    METHODS unsupported FOR TESTING.
    METHODS no_value FOR TESTING.
    METHODS empty_value FOR TESTING.

ENDCLASS.

CLASS ltcl_sap1_to_bcp47 IMPLEMENTATION.

  METHOD assert_sap1_to_bcp47.
    DATA lv_result TYPE string.

    lv_result = zcl_abapgit_convert_language=>sap1_to_bcp47( im_from ).

    cl_abap_unit_assert=>assert_equals(
      exp = im_to
      act = lv_result
      msg = |Converting "{ im_from }" should result in "{ im_to }"| ).
  ENDMETHOD.

  METHOD assert_sap1_to_bcp47_fail.
    DATA lv_result TYPE string.
    DATA lv_act TYPE sy-subrc.

    zcl_abapgit_convert_language=>sap1_to_bcp47(
      EXPORTING
        im_lang_sap1  = im_from
      RECEIVING
        re_lang_bcp47 = lv_result
      EXCEPTIONS
        no_assignment = 1
        OTHERS        = 2 ).

    " Assert itself might change sy-subrc (it does in 702!)
    lv_act = sy-subrc.

    cl_abap_unit_assert=>assert_equals(
      exp = 1
      act = lv_act
      msg = |Converting "{ im_from }" should fail| ).
  ENDMETHOD.

  METHOD english.
    assert_sap1_to_bcp47( im_from = 'E'
                          im_to   = 'en' ).
  ENDMETHOD.

  METHOD english_gb.
    DATA lv_from TYPE sy-langu.
    lv_from = zcl_abapgit_convert_language=>uccp( 'B46E' ).

    assert_sap1_to_bcp47( im_from = lv_from
                          im_to   = 'en-GB' ).
  ENDMETHOD.

  METHOD chinese.
    assert_sap1_to_bcp47( im_from = '1'
                          im_to   = 'zh' ).
  ENDMETHOD.

  METHOD chinese_singapore.
    DATA lv_from TYPE sy-langu.
    lv_from = zcl_abapgit_convert_language=>uccp( 'B343' ).

    assert_sap1_to_bcp47( im_from = lv_from
                          im_to   = 'zh-SG' ).
  ENDMETHOD.

  METHOD chinese_taiwan.
    assert_sap1_to_bcp47( im_from = 'M'
                          im_to   = 'zh-Hant' ).
  ENDMETHOD.

  METHOD unsupported.
    assert_sap1_to_bcp47_fail( 'x' ).
  ENDMETHOD.

  METHOD no_value.
    assert_sap1_to_bcp47_fail( ' ' ).
  ENDMETHOD.

  METHOD empty_value.
    assert_sap1_to_bcp47_fail( '' ).
  ENDMETHOD.

ENDCLASS.
