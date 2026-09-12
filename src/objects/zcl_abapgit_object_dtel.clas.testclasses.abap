CLASS ltcl_delete_longtexts DEFINITION DEFERRED.
CLASS zcl_abapgit_object_dtel DEFINITION LOCAL FRIENDS ltcl_delete_longtexts.

CLASS lcl_longtexts_double DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_abapgit_longtexts.
    DATA mt_deleted_ids TYPE STANDARD TABLE OF dokil-id WITH DEFAULT KEY.
ENDCLASS.

CLASS lcl_longtexts_double IMPLEMENTATION.
  METHOD zif_abapgit_longtexts~changed_by.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_longtexts~serialize.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_longtexts~serialize_aff.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_longtexts~deserialize.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_longtexts~deserialize_aff.
    RETURN.
  ENDMETHOD.

  METHOD zif_abapgit_longtexts~delete.
    APPEND iv_longtext_id TO mt_deleted_ids.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_delete_longtexts DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT FINAL.
  PRIVATE SECTION.
    DATA mo_longtexts TYPE REF TO lcl_longtexts_double.

    METHODS setup.
    METHODS teardown.
    METHODS delete_both_documentation FOR TESTING RAISING zcx_abapgit_exception.
ENDCLASS.

CLASS ltcl_delete_longtexts IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT mo_longtexts.
    zcl_abapgit_injector=>set_longtexts( mo_longtexts ).
  ENDMETHOD.

  METHOD teardown.
    DATA li_longtexts TYPE REF TO zif_abapgit_longtexts.

    zcl_abapgit_injector=>set_longtexts( li_longtexts ).
  ENDMETHOD.

  METHOD delete_both_documentation.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lo_dtel TYPE REF TO zcl_abapgit_object_dtel.
    DATA lv_longtext_id TYPE dokil-id.

    ls_item-obj_type = 'DTEL'.
    ls_item-obj_name = 'Z_TEST_DTEL'.
    CREATE OBJECT lo_dtel
      EXPORTING
        is_item     = ls_item
        iv_language = sy-langu.

    lo_dtel->delete_documentation( ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( mo_longtexts->mt_deleted_ids )
      exp = 2 ).
    READ TABLE mo_longtexts->mt_deleted_ids INDEX 1 INTO lv_longtext_id.
    cl_abap_unit_assert=>assert_equals(
      act = lv_longtext_id
      exp = 'DE' ).
    READ TABLE mo_longtexts->mt_deleted_ids INDEX 2 INTO lv_longtext_id.
    cl_abap_unit_assert=>assert_equals(
      act = lv_longtext_id
      exp = 'DZ' ).
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_aff_metadata_handler DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT
  FINAL.

  PRIVATE SECTION.
    METHODS predefined_roundtrip FOR TESTING RAISING cx_static_check.
    METHODS special_type_roundtrip FOR TESTING RAISING cx_static_check.
    METHODS zero_length_roundtrip FOR TESTING RAISING cx_static_check.
    METHODS domain_mapping FOR TESTING RAISING cx_static_check.
    METHODS domain_omits_predefined_type FOR TESTING RAISING cx_static_check.
    METHODS standard_abap_language_vers FOR TESTING RAISING cx_static_check.
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


  METHOD zero_length_roundtrip.
    DATA lv_json TYPE string.
    DATA lv_json_roundtrip TYPE xstring.
    DATA ls_dd04v TYPE dd04v.
    DATA lv_abap_language_version TYPE uccheck.
    DATA lv_is_equal TYPE abap_bool.

    " "length" is mandatory for predefined types, so it must survive serialization
    " even though it is zero
    lv_json = `{` &&
      `"formatVersion":"1",` &&
      `"header":{"description":"String value","originalLanguage":"en"},` &&
      `"dataTypeInformation":{"category":"predefinedType",` &&
      `"predefinedType":{"dataType":"STRING","length":0}}` &&
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
      exp = 'STRG' ).

    lv_json_roundtrip = lcl_aff_metadata_handler=>serialize(
      is_dd04v                 = ls_dd04v
      iv_abap_language_version = lv_abap_language_version ).
    lv_is_equal = zcl_abapgit_ajson_utilities=>new( )->is_equal(
      iv_json_a = lv_json
      iv_json_b = zcl_abapgit_convert=>xstring_to_string_utf8( lv_json_roundtrip ) ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_is_equal
      exp = abap_true
      msg = zcl_abapgit_convert=>xstring_to_string_utf8( lv_json_roundtrip ) ).
  ENDMETHOD.


  METHOD domain_omits_predefined_type.
    DATA lv_json TYPE string.
    DATA lv_json_actual TYPE string.
    DATA ls_dd04v TYPE dd04v.
    DATA lv_abap_language_version TYPE uccheck.

    " The AFF schema requires "dataType" and "length" inside "predefinedType",
    " so the node must not be written for the other categories
    lv_json = `{` &&
      `"formatVersion":"1",` &&
      `"header":{"description":"Domain value","originalLanguage":"en"},` &&
      `"dataTypeInformation":{"category":"domain","typeName":"Z_TEST_DOMAIN"}` &&
      `}`.

    lcl_aff_metadata_handler=>deserialize(
      EXPORTING
        iv_json                  = zcl_abapgit_convert=>string_to_xstring_utf8( lv_json )
        iv_object_name           = 'z_test_dtel'
      IMPORTING
        es_dd04v                 = ls_dd04v
        ev_abap_language_version = lv_abap_language_version ).

    lv_json_actual = zcl_abapgit_convert=>xstring_to_string_utf8(
      lcl_aff_metadata_handler=>serialize(
        is_dd04v                 = ls_dd04v
        iv_abap_language_version = lv_abap_language_version ) ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_ajson=>parse( lv_json_actual )->exists( '/dataTypeInformation/predefinedType' )
      exp = abap_false
      msg = lv_json_actual ).
  ENDMETHOD.


  METHOD standard_abap_language_vers.
    DATA lv_json TYPE string.
    DATA ls_dd04v TYPE dd04v.
    DATA lv_abap_language_version TYPE uccheck.

    " DD04L expects the DDIC representation of "standard", which is initial
    lv_json = `{` &&
      `"formatVersion":"1",` &&
      `"header":{"description":"Character value","originalLanguage":"en",` &&
      `"abapLanguageVersion":"standard"},` &&
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

    cl_abap_unit_assert=>assert_initial( lv_abap_language_version ).
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
        object_type TYPE tadir-object,
        exp_reftype TYPE dd04v-reftype,
      END OF ty_test_case.
    DATA lt_test_cases TYPE STANDARD TABLE OF ty_test_case.
    DATA ls_test_case TYPE ty_test_case.
    DATA lt_files TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA ls_file TYPE zif_abapgit_git_definitions=>ty_file.
    DATA ls_item TYPE zif_abapgit_definitions=>ty_item.
    DATA lv_json TYPE string.
    DATA lv_json_actual TYPE string.
    DATA lv_json_roundtrip TYPE xstring.
    DATA ls_dd04v TYPE dd04v.
    DATA lv_abap_language_version TYPE uccheck.
    DATA lv_is_equal TYPE abap_bool.

    ls_test_case-category = 'referenceToPredefinedType'.
    ls_test_case-type_name = 'ANY'.
    ls_test_case-exp_reftype = 'A'.
    APPEND ls_test_case TO lt_test_cases.
    ls_test_case-type_name = 'DATA'.
    ls_test_case-exp_reftype = 'D'.
    APPEND ls_test_case TO lt_test_cases.
    ls_test_case-type_name = 'OBJECT'.
    ls_test_case-exp_reftype = 'O'.
    APPEND ls_test_case TO lt_test_cases.

    ls_test_case-category = 'referenceDictionaryType'.
    ls_test_case-type_name = 'CHAR'.
    ls_test_case-exp_reftype = 'B'.
    APPEND ls_test_case TO lt_test_cases.
    ls_test_case-type_name = 'Z_OTHER_DTEL'.
    ls_test_case-object_type = 'DTEL'.
    ls_test_case-exp_reftype = 'E'.
    APPEND ls_test_case TO lt_test_cases.
    ls_test_case-type_name = 'Z_TABLE_TYPE'.
    ls_test_case-object_type = 'TTYP'.
    ls_test_case-exp_reftype = 'L'.
    APPEND ls_test_case TO lt_test_cases.
    ls_test_case-type_name = 'Z_STRUCTURE'.
    ls_test_case-object_type = 'TABL'.
    ls_test_case-exp_reftype = 'S'.
    APPEND ls_test_case TO lt_test_cases.

    ls_test_case-category = 'referenceClasIntType'.
    ls_test_case-type_name = 'Z_SERVICE'.
    ls_test_case-object_type = 'INTF'.
    ls_test_case-exp_reftype = 'I'.
    APPEND ls_test_case TO lt_test_cases.
    ls_test_case-type_name = 'ZIF_TEST'.
    ls_test_case-object_type = 'CLAS'.
    ls_test_case-exp_reftype = 'C'.
    APPEND ls_test_case TO lt_test_cases.

    LOOP AT lt_test_cases INTO ls_test_case.
      CLEAR lt_files.
      IF ls_test_case-object_type IS NOT INITIAL.
        CLEAR ls_item.
        ls_item-obj_type = ls_test_case-object_type.
        ls_item-obj_name = ls_test_case-type_name.
        CLEAR ls_file.
        ls_file-filename = zcl_abapgit_filename_logic=>object_to_file(
          is_item = ls_item
          iv_ext  = 'xml' ).
        APPEND ls_file TO lt_files.
      ENDIF.

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
          it_files                 = lt_files
        IMPORTING
          es_dd04v                 = ls_dd04v
          ev_abap_language_version = lv_abap_language_version ).

      cl_abap_unit_assert=>assert_equals(
        act = ls_dd04v-refkind
        exp = 'R' ).
      IF ls_test_case-exp_reftype = 'B'.
        cl_abap_unit_assert=>assert_equals(
          act = ls_dd04v-datatype
          exp = ls_test_case-type_name ).
        cl_abap_unit_assert=>assert_initial( ls_dd04v-domname ).
      ELSE.
        cl_abap_unit_assert=>assert_equals(
          act = ls_dd04v-datatype
          exp = 'REF' ).
      ENDIF.
      cl_abap_unit_assert=>assert_equals(
        act = ls_dd04v-reftype
        exp = ls_test_case-exp_reftype
        msg = ls_test_case-category ).

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
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
