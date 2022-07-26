CLASS ltcl_unit_test DEFINITION DEFERRED.
CLASS zcl_abapgit_object_intf DEFINITION LOCAL FRIENDS ltcl_unit_test.

CLASS lth_oo_object_fnc DEFINITION FINAL FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_oo_object_fnc.

    DATA:
      mv_create_package     TYPE devclass,
      ms_create_vseointerf  TYPE vseointerf,
      ms_deserialize_key    TYPE seoclskey,
      mt_deserialize_source TYPE zif_abapgit_definitions=>ty_string_tt,
      ms_descriptions_key   TYPE seoclskey,
      mt_descriptions       TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt,
      ms_activation_item    TYPE zif_abapgit_definitions=>ty_item,
      mt_docu_lines         TYPE tlinetab,
      mv_docu_id            TYPE dokhl-id,
      mv_docu_object_name   TYPE dokhl-object,
      mv_docu_langu         TYPE spras.
ENDCLASS.

CLASS lth_oo_object_fnc IMPLEMENTATION.

  METHOD zif_abapgit_oo_object_fnc~add_to_activation_list.
    ms_activation_item = is_item.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~create.
    mv_create_package = iv_package.
    ms_create_vseointerf = cg_properties.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~create_documentation.
    mt_docu_lines = it_lines.
    mv_docu_id = iv_id.
    mv_docu_object_name = iv_object_name.
    mv_docu_langu =   iv_language.

  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~create_sotr.
  ENDMETHOD.
  METHOD zif_abapgit_oo_object_fnc~delete.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~delete_documentation.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~deserialize_source.
    ms_deserialize_key = is_key.
    mt_deserialize_source = it_source.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~exists.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~generate_locals.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~get_class_properties.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~get_includes.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~get_interface_properties.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~get_skip_test_classes.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~insert_text_pool.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~read_attributes.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~read_descriptions.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~read_documentation.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~read_sotr.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~read_superclass.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~read_text_pool.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~serialize_abap.
  ENDMETHOD.

  METHOD zif_abapgit_oo_object_fnc~update_descriptions.
    ms_descriptions_key = is_key.
    mt_descriptions = it_descriptions.
  ENDMETHOD.

ENDCLASS.


CLASS ltcl_unit_test DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA:
      mo_cut        TYPE REF TO zif_abapgit_object,
      mo_log        TYPE REF TO zcl_abapgit_log,
      mo_object_fnc TYPE REF TO lth_oo_object_fnc,
      ms_item       TYPE zif_abapgit_definitions=>ty_item.

    METHODS:
      setup,

      get_xml
        RETURNING VALUE(rv_xml) TYPE string,
      get_source
        RETURNING VALUE(rt_source) TYPE rswsourcet,

      deserializes FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_unit_test IMPLEMENTATION.

  METHOD setup.
    DATA lo_cut TYPE REF TO zcl_abapgit_object_intf.

    ms_item-obj_name = 'ZIF_ABAPGIT_TEST_INTF'.
    ms_item-obj_type = 'INTF'.

    CREATE OBJECT lo_cut
      EXPORTING
        is_item     = ms_item
        iv_language = 'E'.

    CREATE OBJECT lo_cut->zif_abapgit_object~mo_files
      EXPORTING
        is_item = ms_item.

    CREATE OBJECT mo_log.

    CREATE OBJECT mo_object_fnc.
    lo_cut->mi_object_oriented_object_fct  = mo_object_fnc.

    mo_cut = lo_cut.

  ENDMETHOD.

  METHOD deserializes.
    DATA lo_xmlin TYPE REF TO zcl_abapgit_xml_input.
    DATA ls_expected_vseointerf TYPE vseointerf.
    DATA ls_expected_clskey TYPE seoclskey.
    DATA ls_expected_description TYPE seocompotx.
    DATA lt_expected_descriptions TYPE zif_abapgit_oo_object_fnc=>ty_seocompotx_tt.
    DATA ls_expected_docu_line TYPE tline.
    DATA lt_expected_docu_lines TYPE tlinetab.

    CREATE OBJECT lo_xmlin TYPE zcl_abapgit_xml_input
      EXPORTING
        iv_xml = get_xml( ).

    mo_cut->mo_files->add_abap( get_source( ) ).

    mo_cut->deserialize(
      iv_package   = 'MY_PACKAGE'
      io_xml       = lo_xmlin
      iv_step      = zif_abapgit_object=>gc_step_id-abap
      ii_log       = mo_log
      iv_transport = 'XXX12345678' ).


    cl_abap_unit_assert=>assert_equals( exp = 'MY_PACKAGE'
                                        act = mo_object_fnc->mv_create_package ).

    ls_expected_vseointerf-clsname = 'ZIF_ABAPGIT_TEST_INTF'.
    ls_expected_vseointerf-langu = 'E'.
    ls_expected_vseointerf-descript = 'Test interface for abap git'.
    ls_expected_vseointerf-exposure = '2'.
    ls_expected_vseointerf-state = '1'.
    ls_expected_vseointerf-unicode = 'X'.
    cl_abap_unit_assert=>assert_equals( exp = ls_expected_vseointerf
                                        act = mo_object_fnc->ms_create_vseointerf ).

    ls_expected_clskey-clsname = 'ZIF_ABAPGIT_TEST_INTF'.
    cl_abap_unit_assert=>assert_equals( exp = ls_expected_clskey
                                        act = mo_object_fnc->ms_deserialize_key ).
    cl_abap_unit_assert=>assert_equals( exp = ls_expected_clskey
                                        act = mo_object_fnc->ms_deserialize_key ).

    cl_abap_unit_assert=>assert_equals( exp = ls_expected_clskey
                                        act = mo_object_fnc->ms_descriptions_key ).
    ls_expected_description-clsname = 'ZIF_ABAPGIT_TEST_INTF'.
    ls_expected_description-cmpname = 'ONE_METHOD'.
    ls_expected_description-langu = 'E'.
    ls_expected_description-descript = 'Method One, one, one, one, ...'.
    APPEND ls_expected_description TO lt_expected_descriptions.
    cl_abap_unit_assert=>assert_equals( exp = lt_expected_descriptions
                                        act = mo_object_fnc->mt_descriptions ).
    cl_abap_unit_assert=>assert_equals( exp = ms_item
                                        act = mo_object_fnc->ms_activation_item ).

    ls_expected_docu_line-tdformat = 'U1'.
    ls_expected_docu_line-tdline = '&MEANING&'.
    APPEND ls_expected_docu_line TO lt_expected_docu_lines.
    ls_expected_docu_line-tdformat = 'AS'.
    ls_expected_docu_line-tdline = 'Interface for abap git unit tests.'.
    APPEND ls_expected_docu_line TO lt_expected_docu_lines.
    cl_abap_unit_assert=>assert_equals( exp = lt_expected_docu_lines
                                        act = mo_object_fnc->mt_docu_lines ).
    cl_abap_unit_assert=>assert_equals( exp = 'IF'
                                        act = mo_object_fnc->mv_docu_id ).
    cl_abap_unit_assert=>assert_equals( exp = 'E'
                                        act = mo_object_fnc->mv_docu_langu ).
    cl_abap_unit_assert=>assert_equals( exp = 'ZIF_ABAPGIT_TEST_INTF'
                                        act = mo_object_fnc->mv_docu_object_name ).
  ENDMETHOD.

  METHOD get_xml.
    rv_xml =
    '<?xml version="1.0" encoding="UTF-8"?><abapGit version="v1.0.0">' &&
    '  <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">' &&
    '    <asx:values>' &&
    '      <VSEOINTERF>' &&
    '        <CLSNAME>ZIF_ABAPGIT_TEST_INTF</CLSNAME>' &&
    '        <LANGU>E</LANGU>' &&
    '        <DESCRIPT>Test interface for abap git</DESCRIPT>' &&
    '        <EXPOSURE>2</EXPOSURE>' &&
    '        <STATE>1</STATE>' &&
    '        <UNICODE>X</UNICODE>' &&
    '      </VSEOINTERF>' &&
    '      <DESCRIPTIONS>' &&
    '        <SEOCOMPOTX>' &&
    '          <CLSNAME>ZIF_ABAPGIT_TEST_INTF</CLSNAME>' &&
    '          <CMPNAME>ONE_METHOD</CMPNAME>' &&
    '          <LANGU>E</LANGU>' &&
    '          <DESCRIPT>Method One, one, one, one, ...</DESCRIPT>' &&
    '        </SEOCOMPOTX>' &&
    '      </DESCRIPTIONS>' &&
    '      <LINES>' &&
    '        <TLINE>' &&
    '          <TDFORMAT>U1</TDFORMAT>' &&
    '          <TDLINE>&amp;MEANING&amp;</TDLINE>' &&
    '        </TLINE>' &&
    '        <TLINE>' &&
    '          <TDFORMAT>AS</TDFORMAT>' &&
    '          <TDLINE>Interface for abap git unit tests.</TDLINE>' &&
    '        </TLINE>' &&
    '      </LINES>' &&
    '    </asx:values>' &&
    '  </asx:abap>' &&
    '</abapGit>'.
  ENDMETHOD.

  METHOD get_source.
    APPEND 'interface zif_abapgit_test_intf' TO rt_source.
    APPEND '  public.' TO rt_source.
    APPEND '  methods:' TO rt_source.
    APPEND '    one_method' TO rt_source.
    APPEND '      returning value(rv_value) type string.' TO rt_source.
    APPEND '' TO rt_source.
    APPEND 'endinterface.' TO rt_source.
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_serialize DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS:
      serialize_non_default FOR TESTING RAISING cx_static_check,
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


  METHOD serialize_non_default.
    DATA:
      lv_expected        TYPE string,
      lv_actual          TYPE string,
      lv_serialized_data TYPE xstring,
      lv_is_equal        TYPE abap_bool,
      ls_intf            TYPE zcl_abapgit_object_intf=>ty_intf.

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
