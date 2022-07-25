*"* use this source file for your ABAP unit test classes
CLASS ltcl_serialize DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      serialize_key_user FOR TESTING RAISING cx_static_check,
      serialize_default FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_serialize IMPLEMENTATION.

  METHOD serialize_default.
    DATA:
      lv_expected        TYPE string,
      lv_actual          TYPE string,
      lv_serialized_data TYPE xstring,
      lv_is_equal        TYPE abap_bool,
      ls_intf            TYPE zcl_abapgit_object_intf=>ty_intf.

    ls_intf-vseointerf-unicode = zif_abapgit_aff_types_v1=>co_abap_language_version_src-standard.
    ls_intf-vseointerf-descript = `abc`.
    ls_intf-vseointerf-langu = `E`.
    ls_intf-vseointerf-category = zif_abapgit_aff_intf_v1=>co_category-general.
    ls_intf-vseointerf-clsproxy = abap_false.

    lv_serialized_data = lcl_aff_serialize_metadata=>serialize( ls_intf ).

    lv_expected =
      `{` && cl_abap_char_utilities=>newline &&
      `  "formatVersion": "1",` && cl_abap_char_utilities=>newline &&
      `  "header": {` && cl_abap_char_utilities=>newline &&
      `    "description": "abc",` && cl_abap_char_utilities=>newline &&
      `    "originalLanguage": "en"` && cl_abap_char_utilities=>newline &&
      `  }` && cl_abap_char_utilities=>newline &&
      `}` && cl_abap_char_utilities=>newline.

    lv_actual = cl_abap_codepage=>convert_from( lv_serialized_data ).

    lv_is_equal = zcl_abapgit_ajson_utilities=>new( )->is_equal(
      iv_json_a = lv_actual
      iv_json_b = lv_expected ).
    cl_abap_unit_assert=>assert_true( lv_is_equal ).
  ENDMETHOD.


  METHOD serialize_key_user.
    DATA:
      lv_expected        TYPE string,
      lv_actual          TYPE string,
      lv_serialized_data TYPE xstring,
      lv_is_equal        TYPE abap_bool,
      ls_intf            TYPE zcl_abapgit_object_intf=>ty_intf.

    " code does select DB for category and clsproxy
    ls_intf-vseointerf-unicode = zif_abapgit_aff_types_v1=>co_abap_language_version_src-key_user.
    ls_intf-vseointerf-descript = `abc`.
    ls_intf-vseointerf-langu = `F`.
    ls_intf-vseointerf-category = zif_abapgit_aff_intf_v1=>co_category-db_procedure_proxy.
    ls_intf-vseointerf-clsproxy = abap_true.

    lv_serialized_data = lcl_aff_serialize_metadata=>serialize( ls_intf ).

    lv_expected =
      `{` && cl_abap_char_utilities=>newline &&
      `  "formatVersion": "1",` && cl_abap_char_utilities=>newline &&
      `  "header": {` && cl_abap_char_utilities=>newline &&
      `    "description": "abc",` && cl_abap_char_utilities=>newline &&
      `    "originalLanguage": "fr",` && cl_abap_char_utilities=>newline &&
      `    "abapLanguageVersion": "keyUser"` && cl_abap_char_utilities=>newline &&
      `  },` && cl_abap_char_utilities=>newline &&
      `  "category": "dbProcedureProxy",` && cl_abap_char_utilities=>newline &&
      `  "proxy": true` && cl_abap_char_utilities=>newline &&
      `}` && cl_abap_char_utilities=>newline.

    lv_actual = cl_abap_codepage=>convert_from( lv_serialized_data ).

    lv_is_equal = zcl_abapgit_ajson_utilities=>new( )->is_equal(
      iv_json_a = lv_actual
      iv_json_b = lv_expected ).
    cl_abap_unit_assert=>assert_true( lv_is_equal ).

  ENDMETHOD.

ENDCLASS.
