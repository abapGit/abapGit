CLASS ltcl_aff_type_mapping DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA mi_cut TYPE REF TO zif_abapgit_aff_type_mapping.

    METHODS:
      setup,
      assert_to_aff
        IMPORTING
          iv_ddic   TYPE dd01v-datatype
          iv_length TYPE dd01v-leng DEFAULT 0
          iv_aff    TYPE zif_abapgit_aff_ddic_types_v1=>ty_data_type,
      assert_to_abapgit
        IMPORTING
          iv_aff  TYPE zif_abapgit_aff_ddic_types_v1=>ty_data_type
          iv_ddic TYPE dd01v-datatype,
      data_types_to_aff FOR TESTING,
      data_types_to_abapgit FOR TESTING,
      values_and_intervals FOR TESTING.
ENDCLASS.


CLASS ltcl_aff_metadata DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      assert_json_equals
        IMPORTING
          iv_actual   TYPE string
          iv_expected TYPE string
        RAISING
          zcx_abapgit_ajson_error,
      assert_data_type_json
        IMPORTING
          iv_ddic     TYPE dd01v-datatype
          iv_length   TYPE dd01v-leng DEFAULT 0
          iv_expected TYPE string
        RAISING
          zcx_abapgit_exception
          zcx_abapgit_ajson_error,
      data_type_enums FOR TESTING RAISING cx_static_check,
      serialize_minimal FOR TESTING RAISING cx_static_check,
      serialize_non_defaults FOR TESTING RAISING cx_static_check,
      deserialize_minimal FOR TESTING RAISING cx_static_check,
      deserialize_non_defaults FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_aff_type_mapping IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mi_cut TYPE lcl_aff_type_mapping.
  ENDMETHOD.

  METHOD assert_to_aff.
    DATA lo_source TYPE REF TO lcl_doma_data.
    DATA ls_aff TYPE zif_abapgit_aff_doma_v1=>ty_main.

    CREATE OBJECT lo_source.
    lo_source->ms_dd01v-datatype = iv_ddic.
    lo_source->ms_dd01v-leng = iv_length.

    mi_cut->to_aff(
      EXPORTING
        iv_data = lo_source
      IMPORTING
        es_data = ls_aff ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_aff-format-data_type
      exp = iv_aff
      msg = iv_ddic ).
  ENDMETHOD.

  METHOD assert_to_abapgit.
    DATA lo_actual TYPE REF TO lcl_doma_data.
    DATA ls_aff TYPE zif_abapgit_aff_doma_v1=>ty_main.

    ls_aff-format-data_type = iv_aff.

    mi_cut->to_abapgit(
      EXPORTING
        iv_data        = ls_aff
        iv_object_name = 'ZDOMAIN'
      IMPORTING
        es_data        = lo_actual ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_actual->ms_dd01v-datatype
      exp = iv_ddic
      msg = iv_aff ).
  ENDMETHOD.

  METHOD data_types_to_aff.
    DATA ls_data_types LIKE zif_abapgit_aff_ddic_types_v1=>co_data_type.

    FIELD-SYMBOLS <lv_data_type> TYPE any.

    ls_data_types = zif_abapgit_aff_ddic_types_v1=>co_data_type.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE ls_data_types TO <lv_data_type>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      assert_to_aff( iv_ddic = <lv_data_type>
                     iv_aff  = <lv_data_type> ).
    ENDDO.

    assert_to_aff( iv_ddic = 'DF16'
                   iv_aff  = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat16 ).
    assert_to_aff( iv_ddic = 'DF34'
                   iv_aff  = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat34 ).
    assert_to_aff( iv_ddic  = 'DECF'
                   iv_length = 16
                   iv_aff   = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat16 ).
    assert_to_aff( iv_ddic  = 'DECF'
                   iv_length = 34
                   iv_aff   = zif_abapgit_aff_ddic_types_v1=>co_data_type-decfloat34 ).
    assert_to_aff( iv_ddic = 'GEOM'
                   iv_aff  = zif_abapgit_aff_ddic_types_v1=>co_data_type-geom_ewkb ).
    assert_to_aff( iv_ddic = 'RAWS'
                   iv_aff  = zif_abapgit_aff_ddic_types_v1=>co_data_type-rawstring ).
    assert_to_aff( iv_ddic = 'ZZZZ'
                   iv_aff  = zif_abapgit_aff_ddic_types_v1=>co_data_type-char ).
  ENDMETHOD.

  METHOD data_types_to_abapgit.
    DATA ls_data_types LIKE zif_abapgit_aff_ddic_types_v1=>co_data_type.

    FIELD-SYMBOLS <lv_data_type> TYPE any.

    ls_data_types = zif_abapgit_aff_ddic_types_v1=>co_data_type.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE ls_data_types TO <lv_data_type>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      assert_to_abapgit( iv_aff  = <lv_data_type>
                         iv_ddic = <lv_data_type> ).
    ENDDO.

    assert_to_abapgit( iv_aff  = 'ZZZZ'
                       iv_ddic = 'CHAR' ).
  ENDMETHOD.

  METHOD values_and_intervals.
    DATA lo_source TYPE REF TO lcl_doma_data.
    DATA lo_actual TYPE REF TO lcl_doma_data.
    DATA ls_aff TYPE zif_abapgit_aff_doma_v1=>ty_main.
    DATA ls_dd07v TYPE dd07v.

    CREATE OBJECT lo_source.
    ls_dd07v-domvalue_l = 'A'.
    ls_dd07v-domvalue_h = 'A'.
    ls_dd07v-ddtext = 'Active'.
    APPEND ls_dd07v TO lo_source->ms_dd07v.

    CLEAR ls_dd07v.
    ls_dd07v-domvalue_l = '10'.
    ls_dd07v-domvalue_h = '20'.
    ls_dd07v-ddtext = 'Interval'.
    APPEND ls_dd07v TO lo_source->ms_dd07v.

    mi_cut->to_aff(
      EXPORTING
        iv_data = lo_source
      IMPORTING
        es_data = ls_aff ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_aff-fixed_values )
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( ls_aff-fixed_value_intervals )
      exp = 1 ).

    mi_cut->to_abapgit(
      EXPORTING
        iv_data        = ls_aff
        iv_object_name = 'zdomain'
      IMPORTING
        es_data        = lo_actual ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lo_actual->ms_dd07v )
      exp = 2 ).
    READ TABLE lo_actual->ms_dd07v INTO ls_dd07v INDEX 1.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd07v-domname
      exp = 'ZDOMAIN' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd07v-valpos
      exp = 1 ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd07v-domvalue_l
      exp = 'A' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd07v-domvalue_h
      exp = 'A' ).
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_aff_metadata IMPLEMENTATION.

  METHOD assert_json_equals.
    DATA lv_is_equal TYPE abap_bool.

    lv_is_equal = zcl_abapgit_ajson_utilities=>new( )->is_equal(
      iv_json_a = iv_actual
      iv_json_b = iv_expected ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_is_equal
      exp = abap_true
      msg = iv_actual ).
  ENDMETHOD.

  METHOD assert_data_type_json.
    DATA ls_dd01v TYPE dd01v.
    DATA ls_actual_dd01v TYPE dd01v.
    DATA lt_dd07v TYPE dd07v_tab.
    DATA lt_actual_dd07v TYPE dd07v_tab.
    DATA lv_serialized TYPE xstring.
    DATA lv_json TYPE string.
    DATA li_json TYPE REF TO zif_abapgit_ajson.

    ls_dd01v-datatype = iv_ddic.
    ls_dd01v-leng = iv_length.
    ls_dd01v-ddlanguage = 'E'.

    lv_serialized = lcl_aff_metadata_handler=>serialize(
      is_dd01v = ls_dd01v
      it_dd07v = lt_dd07v ).
    lv_json = zcl_abapgit_convert=>xstring_to_string_utf8( lv_serialized ).
    li_json = zcl_abapgit_ajson=>parse( lv_json ).

    cl_abap_unit_assert=>assert_equals(
      act = li_json->get( '/format/dataType' )
      exp = iv_expected
      msg = iv_ddic ).

    lcl_aff_metadata_handler=>deserialize(
      EXPORTING
        iv_json        = lv_serialized
        iv_object_name = 'ZDOMAIN'
      IMPORTING
        es_dd01v       = ls_actual_dd01v
        et_dd07v       = lt_actual_dd07v ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_actual_dd01v-datatype
      exp = iv_ddic
      msg = iv_expected ).
  ENDMETHOD.

  METHOD data_type_enums.
    DATA lt_enum_pairs TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_enum_pair TYPE string.
    DATA lv_ddic TYPE dd01v-datatype.
    DATA lv_json TYPE string.

    SPLIT `D16D=DF16_DEC D16R=DF16_RAW D16S=DF16_SCL D16N=DECFLOAT16 ` &&
      `D34D=DF34_DEC D34R=DF34_RAW D34S=DF34_SCL D34N=DECFLOAT34 ` &&
      `GGM1=GEOM_EWKB RSTR=RAWSTRING SSTR=SSTRING STRG=STRING UTCL=UTCLONG`
      AT space INTO TABLE lt_enum_pairs.
    LOOP AT lt_enum_pairs INTO lv_enum_pair.
      SPLIT lv_enum_pair AT '=' INTO lv_ddic lv_json.
      assert_data_type_json( iv_ddic    = lv_ddic
                             iv_expected = lv_json ).
    ENDLOOP.
  ENDMETHOD.

  METHOD serialize_minimal.
    DATA ls_dd01v TYPE dd01v.
    DATA lt_dd07v TYPE dd07v_tab.
    DATA lv_actual TYPE string.
    DATA lv_expected TYPE string.

    ls_dd01v-ddtext = 'Test domain'.
    ls_dd01v-ddlanguage = 'E'.
    ls_dd01v-datatype = 'CHAR'.
    ls_dd01v-leng = 3.

    lv_actual = zcl_abapgit_convert=>xstring_to_string_utf8(
      lcl_aff_metadata_handler=>serialize(
        is_dd01v = ls_dd01v
        it_dd07v = lt_dd07v ) ).

    lv_expected =
      `{` &&
      `"formatVersion":"1",` &&
      `"header":{"description":"Test domain","originalLanguage":"en"},` &&
      `"format":{"dataType":"CHAR","length":3},` &&
      `"outputCharacteristics":{"length":0}` &&
      `}`.

    assert_json_equals(
      iv_actual   = lv_actual
      iv_expected = lv_expected ).
  ENDMETHOD.

  METHOD serialize_non_defaults.
    DATA ls_dd01v TYPE dd01v.
    DATA ls_dd07v TYPE dd07v.
    DATA lt_dd07v TYPE dd07v_tab.
    DATA lv_actual TYPE string.
    DATA lv_expected TYPE string.

    ls_dd01v-ddtext = 'Test domain'.
    ls_dd01v-ddlanguage = 'E'.
    ls_dd01v-datatype = 'DEC'.
    ls_dd01v-leng = 5.
    ls_dd01v-decimals = 2.
    ls_dd01v-outputlen = 7.
    ls_dd01v-convexit = 'ALPHA'.
    ls_dd01v-lowercase = abap_true.
    ls_dd01v-signflag = abap_true.
    ls_dd01v-entitytab = 'ZVALUE_TABLE'.

    ls_dd07v-domvalue_l = 'A'.
    ls_dd07v-domvalue_h = 'A'.
    ls_dd07v-ddtext = 'Active'.
    APPEND ls_dd07v TO lt_dd07v.

    CLEAR ls_dd07v.
    ls_dd07v-domvalue_l = '10'.
    ls_dd07v-domvalue_h = '20'.
    ls_dd07v-ddtext = 'Interval'.
    APPEND ls_dd07v TO lt_dd07v.

    lv_actual = zcl_abapgit_convert=>xstring_to_string_utf8(
      lcl_aff_metadata_handler=>serialize(
        is_dd01v = ls_dd01v
        it_dd07v = lt_dd07v ) ).

    lv_expected =
      `{` &&
      `"formatVersion":"1",` &&
      `"header":{"description":"Test domain","originalLanguage":"en"},` &&
      `"format":{"dataType":"DEC","length":5,"decimals":2},` &&
      `"outputCharacteristics":{` &&
      `"length":7,"conversionRoutine":"ALPHA","caseSensitive":true,"negativeValues":true},` &&
      `"fixedValues":[{"fixedValue":"A","description":"Active"}],` &&
      `"fixedValueIntervals":[{"lowLimit":"10","highLimit":"20","description":"Interval"}],` &&
      `"valueTable":{"name":"ZVALUE_TABLE"}` &&
      `}`.

    assert_json_equals(
      iv_actual   = lv_actual
      iv_expected = lv_expected ).
  ENDMETHOD.

  METHOD deserialize_minimal.
    DATA lv_json TYPE string.
    DATA ls_actual_dd01v TYPE dd01v.
    DATA ls_expected_dd01v TYPE dd01v.
    DATA lt_actual_dd07v TYPE dd07v_tab.

    lv_json =
      `{` &&
      `"formatVersion":"1",` &&
      `"header":{"description":"Test domain","originalLanguage":"fr"},` &&
      `"format":{"dataType":"CHAR","length":3}` &&
      `}`.

    lcl_aff_metadata_handler=>deserialize(
      EXPORTING
        iv_json        = zcl_abapgit_convert=>string_to_xstring_utf8( lv_json )
        iv_object_name = 'ztest_doma'
      IMPORTING
        es_dd01v       = ls_actual_dd01v
        et_dd07v       = lt_actual_dd07v ).

    ls_expected_dd01v-domname = 'ZTEST_DOMA'.
    ls_expected_dd01v-ddtext = 'Test domain'.
    ls_expected_dd01v-ddlanguage = 'F'.
    ls_expected_dd01v-datatype = 'CHAR'.
    ls_expected_dd01v-leng = 3.

    cl_abap_unit_assert=>assert_equals(
      act = ls_actual_dd01v
      exp = ls_expected_dd01v ).
    cl_abap_unit_assert=>assert_initial( act = lt_actual_dd07v ).
  ENDMETHOD.

  METHOD deserialize_non_defaults.
    DATA lv_json TYPE string.
    DATA ls_actual_dd01v TYPE dd01v.
    DATA ls_expected_dd01v TYPE dd01v.
    DATA ls_expected_dd07v TYPE dd07v.
    DATA lt_actual_dd07v TYPE dd07v_tab.
    DATA lt_expected_dd07v TYPE dd07v_tab.

    lv_json =
      `{` &&
      `"formatVersion":"1",` &&
      `"header":{"description":"Test domain","originalLanguage":"en"},` &&
      `"format":{"dataType":"DEC","length":5,"decimals":2},` &&
      `"outputCharacteristics":{` &&
      `"length":7,"conversionRoutine":"ALPHA","caseSensitive":true,"negativeValues":true},` &&
      `"fixedValues":[{"fixedValue":"A","description":"Active"}],` &&
      `"fixedValueIntervals":[{"lowLimit":"10","highLimit":"20","description":"Interval"}],` &&
      `"valueTable":{"name":"ZVALUE_TABLE"}` &&
      `}`.

    lcl_aff_metadata_handler=>deserialize(
      EXPORTING
        iv_json        = zcl_abapgit_convert=>string_to_xstring_utf8( lv_json )
        iv_object_name = 'ztest_doma'
      IMPORTING
        es_dd01v       = ls_actual_dd01v
        et_dd07v       = lt_actual_dd07v ).

    ls_expected_dd01v-domname = 'ZTEST_DOMA'.
    ls_expected_dd01v-ddtext = 'Test domain'.
    ls_expected_dd01v-ddlanguage = 'E'.
    ls_expected_dd01v-datatype = 'DEC'.
    ls_expected_dd01v-leng = 5.
    ls_expected_dd01v-decimals = 2.
    ls_expected_dd01v-outputlen = 7.
    ls_expected_dd01v-convexit = 'ALPHA'.
    ls_expected_dd01v-lowercase = abap_true.
    ls_expected_dd01v-signflag = abap_true.
    ls_expected_dd01v-entitytab = 'ZVALUE_TABLE'.

    ls_expected_dd07v-domname = 'ZTEST_DOMA'.
    ls_expected_dd07v-valpos = 1.
    ls_expected_dd07v-ddlanguage = 'E'.
    ls_expected_dd07v-domvalue_l = 'A'.
    ls_expected_dd07v-domvalue_h = 'A'.
    ls_expected_dd07v-ddtext = 'Active'.
    APPEND ls_expected_dd07v TO lt_expected_dd07v.

    CLEAR ls_expected_dd07v.
    ls_expected_dd07v-domname = 'ZTEST_DOMA'.
    ls_expected_dd07v-valpos = 2.
    ls_expected_dd07v-ddlanguage = 'E'.
    ls_expected_dd07v-domvalue_l = '10'.
    ls_expected_dd07v-domvalue_h = '20'.
    ls_expected_dd07v-ddtext = 'Interval'.
    APPEND ls_expected_dd07v TO lt_expected_dd07v.

    cl_abap_unit_assert=>assert_equals(
      act = ls_actual_dd01v
      exp = ls_expected_dd01v ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_actual_dd07v
      exp = lt_expected_dd07v ).
  ENDMETHOD.

ENDCLASS.
