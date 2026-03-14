CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PRIVATE SECTION.
    METHODS setup RAISING cx_static_check.
    METHODS teardown RAISING cx_static_check.
    METHODS deserialize_serialize_xml FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD setup.
    zcl_abapgit_inject_setup=>setup( ).
  ENDMETHOD.

  METHOD teardown.
    zcl_abapgit_inject_setup=>teardown( ).
  ENDMETHOD.

  METHOD deserialize_serialize_xml.

    DATA ls_item        TYPE zif_abapgit_definitions=>ty_item.
    DATA lo_doma        TYPE REF TO zif_abapgit_object.
    DATA li_xml_in      TYPE REF TO zif_abapgit_xml_input.
    DATA li_xml_out     TYPE REF TO zif_abapgit_xml_output.
    DATA lv_xml         TYPE string.
    DATA lv_act         TYPE string.
    DATA lo_log         TYPE REF TO zif_abapgit_log.
    DATA lo_i18n_params TYPE REF TO zcl_abapgit_i18n_params.
    DATA li_xml_check   TYPE REF TO zif_abapgit_xml_input.
    DATA ls_dd01v_act   TYPE dd01v.

    ls_item-obj_type = 'DOMA'.
    ls_item-obj_name = 'ZABAPGIT_TEST_DOMA'.

    lv_xml = |<?xml version="1.0" encoding="utf-8"?>\n| &&
             |<abapGit version="v1.0.0" serializer="LCL_OBJECT_DOMA" serializer_version="v1.0.0">\n| &&
             | <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">\n| &&
             |  <asx:values>\n| &&
             |   <DD01V>\n| &&
             |    <DOMNAME>ZABAPGIT_TEST_DOMA</DOMNAME>\n| &&
             |    <DDLANGUAGE>E</DDLANGUAGE>\n| &&
             |    <DATATYPE>CHAR</DATATYPE>\n| &&
             |    <LENG>000001</LENG>\n| &&
             |    <OUTPUTLEN>000001</OUTPUTLEN>\n| &&
             |    <DDTEXT>Testing</DDTEXT>\n| &&
             |   </DD01V>\n| &&
             |  </asx:values>\n| &&
             | </asx:abap>\n| &&
             |</abapGit>|.

    CREATE OBJECT li_xml_in TYPE zcl_abapgit_xml_input EXPORTING iv_xml = lv_xml.
    CREATE OBJECT lo_log TYPE zcl_abapgit_log.

    lo_i18n_params = zcl_abapgit_i18n_params=>new(
      iv_main_language      = sy-langu
      iv_main_language_only = abap_true ).

    CREATE OBJECT lo_doma TYPE zcl_abapgit_object_doma
      EXPORTING
        iv_language    = sy-langu
        is_item        = ls_item
        io_i18n_params = lo_i18n_params.

    lo_doma->deserialize(
      iv_package   = '$TMP'
      iv_step      = zif_abapgit_object=>gc_step_id-ddic
      ii_log       = lo_log
      iv_transport = ''
      io_xml       = li_xml_in ).

******************

    CREATE OBJECT li_xml_out TYPE zcl_abapgit_xml_output.
    lo_doma->serialize( li_xml_out ).

    lv_act = li_xml_out->render( is_metadata = lo_doma->get_metadata( ) ).

    CREATE OBJECT li_xml_check TYPE zcl_abapgit_xml_input EXPORTING iv_xml = lv_act.

    li_xml_check->read(
      EXPORTING iv_name = 'DD01V'
      CHANGING cg_data = ls_dd01v_act ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_dd01v_act-domname
      exp = 'ZABAPGIT_TEST_DOMA' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd01v_act-ddlanguage
      exp = 'E' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd01v_act-datatype
      exp = 'CHAR' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd01v_act-leng
      exp = '000001' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd01v_act-outputlen
      exp = '000001' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd01v_act-ddtext
      exp = 'Testing' ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_test_aff DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PRIVATE SECTION.
    METHODS setup RAISING cx_static_check.
    METHODS teardown RAISING cx_static_check.
    METHODS deserialize_xml_serialize_aff FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_test_aff IMPLEMENTATION.
  METHOD setup.
    DATA lo_settings TYPE REF TO zcl_abapgit_settings.

    zcl_abapgit_inject_setup=>setup( ).

    lo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
    lo_settings->set_experimental_features( zcl_abapgit_aff_registry=>c_aff_feature ).
  ENDMETHOD.

  METHOD teardown.
    DATA lo_settings TYPE REF TO zcl_abapgit_settings.

    lo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
    lo_settings->set_experimental_features( '' ).

    zcl_abapgit_inject_setup=>teardown( ).
  ENDMETHOD.

  METHOD deserialize_xml_serialize_aff.

    DATA ls_item        TYPE zif_abapgit_definitions=>ty_item.
    DATA lo_doma        TYPE REF TO zif_abapgit_object.
    DATA li_xml_in      TYPE REF TO zif_abapgit_xml_input.
    DATA li_xml_out     TYPE REF TO zif_abapgit_xml_output.
    DATA lv_xml         TYPE string.
    DATA lo_log         TYPE REF TO zif_abapgit_log.
    DATA lo_i18n_params TYPE REF TO zcl_abapgit_i18n_params.
    DATA lo_files       TYPE REF TO zcl_abapgit_objects_files.
    DATA lt_files       TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA lv_json        TYPE string.
    DATA lv_exp         TYPE string.
    DATA lv_is_equal    TYPE abap_bool.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF lt_files.

    ls_item-obj_type = 'DOMA'.
    ls_item-obj_name = 'ZABAPGIT_TEST_DOMA'.

    lv_xml = |<?xml version="1.0" encoding="utf-8"?>\n| &&
             |<abapGit version="v1.0.0" serializer="LCL_OBJECT_DOMA" serializer_version="v1.0.0">\n| &&
             | <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">\n| &&
             |  <asx:values>\n| &&
             |   <DD01V>\n| &&
             |    <DOMNAME>ZABAPGIT_TEST_DOMA</DOMNAME>\n| &&
             |    <DDLANGUAGE>E</DDLANGUAGE>\n| &&
             |    <DATATYPE>CHAR</DATATYPE>\n| &&
             |    <LENG>000001</LENG>\n| &&
             |    <OUTPUTLEN>000001</OUTPUTLEN>\n| &&
             |    <DDTEXT>Testing</DDTEXT>\n| &&
             |   </DD01V>\n| &&
             |  </asx:values>\n| &&
             | </asx:abap>\n| &&
             |</abapGit>|.

    CREATE OBJECT li_xml_in TYPE zcl_abapgit_xml_input EXPORTING iv_xml = lv_xml.
    CREATE OBJECT lo_log TYPE zcl_abapgit_log.

    lo_i18n_params = zcl_abapgit_i18n_params=>new(
      iv_main_language      = sy-langu
      iv_main_language_only = abap_true ).

    lo_files = zcl_abapgit_objects_files=>new( ls_item ).

    CREATE OBJECT lo_doma TYPE zcl_abapgit_object_doma
      EXPORTING
        iv_language    = sy-langu
        is_item        = ls_item
        io_files       = lo_files
        io_i18n_params = lo_i18n_params.

    lo_doma->deserialize(
      iv_package   = '$TMP'
      iv_step      = zif_abapgit_object=>gc_step_id-ddic
      ii_log       = lo_log
      iv_transport = ''
      io_xml       = li_xml_in ).

******************

    CREATE OBJECT li_xml_out TYPE zcl_abapgit_xml_output.
    lo_doma->serialize( li_xml_out ).

    lt_files = lo_files->get_files( ).

    LOOP AT lt_files ASSIGNING <ls_file> WHERE filename CP '*.json'.
      lv_json = zcl_abapgit_convert=>xstring_to_string_utf8( <ls_file>-data ).
      EXIT.
    ENDLOOP.

    lv_exp = `{` && cl_abap_char_utilities=>newline &&
             `  "formatVersion": "1",` && cl_abap_char_utilities=>newline &&
             `  "header": {` && cl_abap_char_utilities=>newline &&
             `    "description": "Testing",` && cl_abap_char_utilities=>newline &&
             `    "originalLanguage": "en"` && cl_abap_char_utilities=>newline &&
             `  },` && cl_abap_char_utilities=>newline &&
             `  "format": {` && cl_abap_char_utilities=>newline &&
             `    "dataType": "CHAR",` && cl_abap_char_utilities=>newline &&
             `    "length": 1,` && cl_abap_char_utilities=>newline &&
             `    "decimals": 0` && cl_abap_char_utilities=>newline &&
             `  },` && cl_abap_char_utilities=>newline &&
             `  "outputCharacteristics": {` && cl_abap_char_utilities=>newline &&
             `    "style": "00",` && cl_abap_char_utilities=>newline &&
             `    "length": 1` && cl_abap_char_utilities=>newline &&
             `  }` && cl_abap_char_utilities=>newline &&
             `}` && cl_abap_char_utilities=>newline.

    lv_is_equal = zcl_abapgit_ajson_utilities=>new( )->is_equal(
      iv_json_a = lv_json
      iv_json_b = lv_exp ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_is_equal
      exp = abap_true ).

  ENDMETHOD.

ENDCLASS.
