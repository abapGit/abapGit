CLASS ltcl_aff_metadata_handler DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.
    METHODS predefined_roundtrip FOR TESTING RAISING cx_static_check.
    METHODS special_type_roundtrip FOR TESTING RAISING cx_static_check.
    METHODS domain_mapping FOR TESTING RAISING cx_static_check.
    METHODS reference_mappings FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_aff_metadata_handler IMPLEMENTATION.

  METHOD predefined_roundtrip.
    DATA lv_json TYPE string.
    DATA lv_json_actual TYPE string.
    DATA lv_json_roundtrip TYPE xstring.
    DATA ls_dd04v TYPE dd04v.
    DATA lv_abap_language_version TYPE uccheck.
    DATA lv_is_equal TYPE abap_bool.

    lv_json = `{` &&
      `"formatVersion":"1",` &&
      `"header":{"description":"Character value","originalLanguage":"en"},` &&
      `"dataTypeInformation":{"category":"predefinedType",` &&
      `"predefinedType":{"dataType":"CHAR","length":10}}` &&
      `}`.

    lcl_aff_metadata_handler=>deserialize(
      EXPORTING
        iv_json                  = zcl_abapgit_convert=>string_to_xstring_utf8( lv_json )
        iv_object_name           = 'z_test_dtel'
      IMPORTING
        es_dd04v                 = ls_dd04v
        ev_abap_language_version = lv_abap_language_version ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-rollname
      exp = 'Z_TEST_DTEL' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-ddtext
      exp = 'Character value' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-ddlanguage
      exp = 'E' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-refkind
      exp = 'T' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-datatype
      exp = 'CHAR' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-leng
      exp = 10 ).

    lv_json_roundtrip = lcl_aff_metadata_handler=>serialize(
      is_dd04v                 = ls_dd04v
      iv_abap_language_version = lv_abap_language_version ).
    lv_json_actual = zcl_abapgit_convert=>xstring_to_string_utf8( lv_json_roundtrip ).
    lv_is_equal = zcl_abapgit_ajson_utilities=>new( )->is_equal(
      iv_json_a = lv_json
      iv_json_b = lv_json_actual ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_is_equal
      exp = abap_true
      msg = lv_json_actual ).
  ENDMETHOD.


  METHOD special_type_roundtrip.
    DATA lv_json TYPE string.
    DATA lv_json_roundtrip TYPE xstring.
    DATA ls_dd04v TYPE dd04v.
    DATA lv_abap_language_version TYPE uccheck.
    DATA lv_is_equal TYPE abap_bool.

    lv_json = `{` &&
      `"formatVersion":"1",` &&
      `"header":{"description":"Decimal value","originalLanguage":"en"},` &&
      `"dataTypeInformation":{"category":"predefinedType",` &&
      `"predefinedType":{"dataType":"DF34_RAW","length":34,"decimals":16}}` &&
      `}`.

    lcl_aff_metadata_handler=>deserialize(
      EXPORTING
        iv_json                  = zcl_abapgit_convert=>string_to_xstring_utf8( lv_json )
        iv_object_name           = 'z_test_dtel'
      IMPORTING
        es_dd04v                 = ls_dd04v
        ev_abap_language_version = lv_abap_language_version ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-datatype
      exp = 'D34R' ).

    lv_json_roundtrip = lcl_aff_metadata_handler=>serialize(
      is_dd04v                 = ls_dd04v
      iv_abap_language_version = lv_abap_language_version ).
    lv_is_equal = zcl_abapgit_ajson_utilities=>new( )->is_equal(
      iv_json_a = lv_json
      iv_json_b = zcl_abapgit_convert=>xstring_to_string_utf8( lv_json_roundtrip ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_is_equal
      exp = abap_true ).
  ENDMETHOD.


  METHOD domain_mapping.
    DATA lv_json TYPE string.
    DATA ls_dd04v TYPE dd04v.
    DATA lv_abap_language_version TYPE uccheck.

    lv_json = `{` &&
      `"formatVersion":"1",` &&
      `"header":{"description":"Domain value","originalLanguage":"en"},` &&
      `"dataTypeInformation":{"category":"domain","typeName":"z_test_domain"},` &&
      `"fieldLabels":{"short":"Short","shortLength":5,` &&
      `"medium":"Medium","mediumLength":10,"long":"Long","longLength":20,` &&
      `"heading":"Heading","headingLength":12},` &&
      `"additionalProperties":{"searchHelp":{"name":"z_test_help","parameter":"VALUE"},` &&
      `"bidirectionalOptions":{"basicDirection":"rightToLeft","noFiltering":true},` &&
      `"parameterId":"ZID","defaultComponentName":"VALUE",` &&
      `"changeDocumentRelevant":true,"noInputHistory":true}` &&
      `}`.

    lcl_aff_metadata_handler=>deserialize(
      EXPORTING
        iv_json                  = zcl_abapgit_convert=>string_to_xstring_utf8( lv_json )
        iv_object_name           = 'z_test_dtel'
      IMPORTING
        es_dd04v                 = ls_dd04v
        ev_abap_language_version = lv_abap_language_version ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-refkind
      exp = 'D' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-domname
      exp = 'Z_TEST_DOMAIN' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-shlpname
      exp = 'Z_TEST_HELP' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-shlpfield
      exp = 'VALUE' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-ltrflddis
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-bidictrlc
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-memoryid
      exp = 'ZID' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-deffdname
      exp = 'VALUE' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-logflag
      exp = abap_true ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v-nohistory
      exp = abap_true ).
  ENDMETHOD.


  METHOD reference_mappings.
    TYPES:
      BEGIN OF ty_test_case,
        category    TYPE string,
        type_name   TYPE string,
        exp_reftype TYPE dd04v-reftype,
      END OF ty_test_case.
    DATA lt_test_cases TYPE STANDARD TABLE OF ty_test_case.
    DATA ls_test_case TYPE ty_test_case.
    DATA lv_json TYPE string.
    DATA ls_dd04v TYPE dd04v.
    DATA lv_abap_language_version TYPE uccheck.

    ls_test_case-category = 'referenceToPredefinedType'.
    ls_test_case-type_name = 'ANY'.
    ls_test_case-exp_reftype = 'A'.
    APPEND ls_test_case TO lt_test_cases.
    ls_test_case-category = 'referenceDictionaryType'.
    ls_test_case-type_name = 'Z_OTHER_DTEL'.
    ls_test_case-exp_reftype = 'B'.
    APPEND ls_test_case TO lt_test_cases.
    ls_test_case-category = 'referenceClasIntType'.
    ls_test_case-type_name = 'ZCL_TEST'.
    ls_test_case-exp_reftype = 'C'.
    APPEND ls_test_case TO lt_test_cases.
    ls_test_case-category = 'referenceClasIntType'.
    ls_test_case-type_name = 'ZIF_TEST'.
    ls_test_case-exp_reftype = 'I'.
    APPEND ls_test_case TO lt_test_cases.

    LOOP AT lt_test_cases INTO ls_test_case.
      lv_json = `{` &&
        `"formatVersion":"1",` &&
        `"header":{"description":"Reference","originalLanguage":"en"},` &&
        `"dataTypeInformation":{"category":"` && ls_test_case-category &&
        `","typeName":"` && ls_test_case-type_name && `"}` &&
        `}`.

      lcl_aff_metadata_handler=>deserialize(
        EXPORTING
          iv_json                  = zcl_abapgit_convert=>string_to_xstring_utf8( lv_json )
          iv_object_name           = 'z_test_dtel'
        IMPORTING
          es_dd04v                 = ls_dd04v
          ev_abap_language_version = lv_abap_language_version ).

      cl_abap_unit_assert=>assert_equals(
        act = ls_dd04v-refkind
        exp = 'R' ).
      cl_abap_unit_assert=>assert_equals(
        act = ls_dd04v-datatype
        exp = 'REF' ).
      cl_abap_unit_assert=>assert_equals(
        act = ls_dd04v-reftype
        exp = ls_test_case-exp_reftype
        msg = ls_test_case-category ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
