CLASS ltcl_remove_abap_lang_version DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PRIVATE SECTION.

    METHODS assert_abap_lang_vers_removed
      IMPORTING
        iv_json     TYPE string
        iv_json_exp TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS at_end_of_header FOR TESTING RAISING zcx_abapgit_exception.
    METHODS no_abap_lang_vers_in_header FOR TESTING RAISING cx_static_check.
    METHODS at_begin_of_header FOR TESTING RAISING cx_static_check.
    METHODS only_abap_lang_vers_in_header FOR TESTING RAISING cx_static_check.
    METHODS other_abap_lang_vers_fld FOR TESTING RAISING cx_static_check.
    METHODS only_other_abap_lang_vers_fld FOR TESTING RAISING cx_static_check.
    METHODS keep_item_order FOR TESTING RAISING cx_static_check.


ENDCLASS.

CLASS zcl_abapgit_object_common_aff DEFINITION LOCAL FRIENDS ltcl_remove_abap_lang_version.
CLASS ltcl_remove_abap_lang_version IMPLEMENTATION.

  METHOD assert_abap_lang_vers_removed.
    DATA lv_json_act TYPE string.
    DATA lv_json_x TYPE xstring.
    DATA lv_json_act_x TYPE xstring.

    lv_json_x     = zcl_abapgit_convert=>string_to_xstring_utf8( iv_json ).

    lv_json_act_x = zcl_abapgit_object_common_aff=>remove_abap_language_version( lv_json_x ).

    lv_json_act = zcl_abapgit_convert=>xstring_to_string_utf8( lv_json_act_x ).
    cl_abap_unit_assert=>assert_equals( act = lv_json_act
                                        exp = iv_json_exp ).
  ENDMETHOD.

  METHOD at_end_of_header.
    assert_abap_lang_vers_removed(
      iv_json     = |\{\n| &
                    |  "formatVersion": "1",\n| &
                    |  "header": \{\n| &
                    |    "description": "Category",\n| &
                    |    "originalLanguage": "en",\n| &
                    |    "abapLanguageVersion": "cloudDevelopment"\n| &
                    |  \}\n| &
                    |\}|
      iv_json_exp = |\{\n| &
                    |  "formatVersion": "1",\n| &
                    |  "header": \{\n| &
                    |    "description": "Category",\n| &
                    |    "originalLanguage": "en"\n| &
                    |  \}\n| &
                    |\}| ).
  ENDMETHOD.

  METHOD no_abap_lang_vers_in_header.
    assert_abap_lang_vers_removed(
      iv_json     = |\{\n| &
                    |  "formatVersion": "1",\n| &
                    |  "header": \{\n| &
                    |    "description": "Category",\n| &
                    |    "originalLanguage": "en"\n| &
                    |  \}\n| &
                    |\}|
      iv_json_exp = |\{\n| &
                    |  "formatVersion": "1",\n| &
                    |  "header": \{\n| &
                    |    "description": "Category",\n| &
                    |    "originalLanguage": "en"\n| &
                    |  \}\n| &
                    |\}| ).
  ENDMETHOD.

  METHOD at_begin_of_header.
    assert_abap_lang_vers_removed(
      iv_json     = |\{\n| &
                    |  "formatVersion": "1",\n| &
                    |  "header": \{\n| &
                    |    "abapLanguageVersion": "cloudDevelopment",\n| &
                    |    "description": "Category",\n| &
                    |    "originalLanguage": "en"\n| &
                    |  \}\n| &
                    |\}|
      iv_json_exp = |\{\n| &
                    |  "formatVersion": "1",\n| &
                    |  "header": \{\n| &
                    |    "description": "Category",\n| &
                    |    "originalLanguage": "en"\n| &
                    |  \}\n| &
                    |\}| ).
  ENDMETHOD.

  METHOD only_abap_lang_vers_in_header.
    assert_abap_lang_vers_removed(
      iv_json     = |\{\n| &
                    |  "formatVersion": "1",\n| &
                    |  "header": \{\n| &
                    |    "abapLanguageVersion": "cloudDevelopment"\n| &
                    |  \}\n| &
                    |\}|
      iv_json_exp = |\{\n| &
                    |  "formatVersion": "1",\n| &
                    |  "header": \{\}\n| &
                    |\}| ).
  ENDMETHOD.

  METHOD other_abap_lang_vers_fld.
    assert_abap_lang_vers_removed(
      iv_json     = |\{\n| &
                    |  "formatVersion": "1",\n| &
                    |  "header": \{\n| &
                    |    "description": "Category",\n| &
                    |    "originalLanguage": "en",\n| &
                    |    "abapLanguageVersion": "cloudDevelopment"\n| &
                    |  \},\n| &
                    |  "abapLanguageVersion": "someOtherConent"\n| &
                    |\}|
      iv_json_exp = |\{\n| &
                    |  "formatVersion": "1",\n| &
                    |  "header": \{\n| &
                    |    "description": "Category",\n| &
                    |    "originalLanguage": "en"\n| &
                    |  \},\n| &
                    |  "abapLanguageVersion": "someOtherConent"\n| &
                    |\}| ).
  ENDMETHOD.

  METHOD only_other_abap_lang_vers_fld.
    assert_abap_lang_vers_removed(
      iv_json     = |\{\n| &
                    |  "formatVersion": "1",\n| &
                    |  "header": \{\n| &
                    |    "description": "Category",\n| &
                    |    "originalLanguage": "en"\n| &
                    |  \},\n| &
                    |  "abapLanguageVersion": "someOtherConent"\n| &
                    |\}|
      iv_json_exp = |\{\n| &
                    |  "formatVersion": "1",\n| &
                    |  "header": \{\n| &
                    |    "description": "Category",\n| &
                    |    "originalLanguage": "en"\n| &
                    |  \},\n| &
                    |  "abapLanguageVersion": "someOtherConent"\n| &
                    |\}| ).
  ENDMETHOD.


  METHOD keep_item_order.
    assert_abap_lang_vers_removed(
      iv_json     = |\{\n| &
                    |  "c": "firstField",\n| &
                    |  "b": \{\n| &
                    |    "z": "firstFieldInObject",\n| &
                    |    "y": "secondFieldInObject"\n| &
                    |  \},\n| &
                    |  "a": "lastField"\n| &
                    |\}|
      iv_json_exp = |\{\n| &
                    |  "c": "firstField",\n| &
                    |  "b": \{\n| &
                    |    "z": "firstFieldInObject",\n| &
                    |    "y": "secondFieldInObject"\n| &
                    |  \},\n| &
                    |  "a": "lastField"\n| &
                    |\}| ).
  ENDMETHOD.

ENDCLASS.