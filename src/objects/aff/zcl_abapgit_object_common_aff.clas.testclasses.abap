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
                    |  "abapLanguageVersion": "someOtherContent"\n| &
                    |\}|
      iv_json_exp = |\{\n| &
                    |  "formatVersion": "1",\n| &
                    |  "header": \{\n| &
                    |    "description": "Category",\n| &
                    |    "originalLanguage": "en"\n| &
                    |  \},\n| &
                    |  "abapLanguageVersion": "someOtherContent"\n| &
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
                    |  "abapLanguageVersion": "someOtherContent"\n| &
                    |\}|
      iv_json_exp = |\{\n| &
                    |  "formatVersion": "1",\n| &
                    |  "header": \{\n| &
                    |    "description": "Category",\n| &
                    |    "originalLanguage": "en"\n| &
                    |  \},\n| &
                    |  "abapLanguageVersion": "someOtherContent"\n| &
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

CLASS ltcl_aff_settings_deserialize DEFINITION DEFERRED.

"! Non-abstract stub of class under test
CLASS ltcl_abapgit_object_common_aff DEFINITION INHERITING FROM zcl_abapgit_object_common_aff
                                     FINAL FOR TESTING FRIENDS ltcl_aff_settings_deserialize.
  PUBLIC SECTION.
    METHODS: zif_abapgit_object~changed_by REDEFINITION.
ENDCLASS.

CLASS ltcl_abapgit_object_common_aff IMPLEMENTATION.
  METHOD zif_abapgit_object~changed_by.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_aff_settings_deserialize DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      assert_setting_creation_for
        IMPORTING
          iv_abap_lv_repo_setting TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version
          iv_exp_setting_abap_lv  TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version
        RAISING
          cx_static_check,
      abap_language_version_standard FOR TESTING RAISING cx_static_check,
      abap_language_version_cloud    FOR TESTING RAISING cx_static_check,
      any_abap_language_version FOR TESTING RAISING cx_static_check,
      no_abap_language_version FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_aff_settings_deserialize IMPLEMENTATION.

  METHOD assert_setting_creation_for.

    DATA:
      lo_cut                  TYPE REF TO ltcl_abapgit_object_common_aff,
      ls_item                 TYPE zif_abapgit_definitions=>ty_item,
      lo_settings_deserialize TYPE REF TO object,
      lv_act_setting_abap_lv  TYPE zif_abapgit_aff_types_v1=>ty_abap_language_version,
      lv_act_setting_version  TYPE r3state,
      lv_act_setting_language TYPE spras,
      lv_act_setting_user     TYPE uname.

    ls_item-obj_type = 'CHKO'.
    ls_item-obj_name = 'Z_DUMMY'.
    ls_item-abap_language_version = iv_abap_lv_repo_setting.

    IF zcl_abapgit_objects=>is_supported( ls_item ) = abap_false.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_cut
      EXPORTING
        is_item     = ls_item
        iv_language = 'E'.

    TRY.
        lo_settings_deserialize = lo_cut->create_aff_setting_deserialize( ).

        CALL METHOD lo_settings_deserialize->('IF_AFF_SETTINGS_DESERIALIZE~GET_ABAP_LANGUAGE_VERSION')
          RECEIVING
            result = lv_act_setting_abap_lv.
        cl_abap_unit_assert=>assert_equals( msg = 'Unexpected ABAP language version in settings'
                                            act = lv_act_setting_abap_lv
                                            exp = iv_exp_setting_abap_lv ).

        CALL METHOD lo_settings_deserialize->('IF_AFF_SETTINGS_DESERIALIZE~GET_VERSION')
          RECEIVING
            result = lv_act_setting_version.
        cl_abap_unit_assert=>assert_equals( msg = 'Unexpected Version in settings'
                                            act = lv_act_setting_version
                                            exp = 'A' ).

        CALL METHOD lo_settings_deserialize->('IF_AFF_SETTINGS_DESERIALIZE~GET_LANGUAGE')
          RECEIVING
            result = lv_act_setting_language.
        cl_abap_unit_assert=>assert_equals( msg = 'Unexpected Language in settings'
                                            act = lv_act_setting_language
                                            exp = 'E' ).

        CALL METHOD lo_settings_deserialize->('IF_AFF_SETTINGS_DESERIALIZE~GET_USER')
          RECEIVING
            result = lv_act_setting_user.
        cl_abap_unit_assert=>assert_equals( msg = 'Unexpected User in settings'
                                            act = lv_act_setting_user
                                            exp = sy-uname ).

      CATCH cx_root.
        " System doesn't support AFF with ABAP language version
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD abap_language_version_standard.
    assert_setting_creation_for(
        iv_abap_lv_repo_setting = zif_abapgit_aff_types_v1=>co_abap_language_version-standard
        iv_exp_setting_abap_lv  = zif_abapgit_aff_types_v1=>co_abap_language_version-standard ).
  ENDMETHOD.

  METHOD abap_language_version_cloud.
    assert_setting_creation_for(
        iv_abap_lv_repo_setting = zif_abapgit_aff_types_v1=>co_abap_language_version-cloud_development
        iv_exp_setting_abap_lv  = zif_abapgit_aff_types_v1=>co_abap_language_version-cloud_development ).
  ENDMETHOD.

  METHOD any_abap_language_version.
    assert_setting_creation_for(
        iv_abap_lv_repo_setting = zcl_abapgit_abap_language_vers=>c_any_abap_language_version
        iv_exp_setting_abap_lv  = '-' ). "AFF: Not specified
  ENDMETHOD.

  METHOD no_abap_language_version.
    assert_setting_creation_for(
        iv_abap_lv_repo_setting = zcl_abapgit_abap_language_vers=>c_no_abap_language_version
        iv_exp_setting_abap_lv  = '-' ). "AFF: Not specified
  ENDMETHOD.

ENDCLASS.
