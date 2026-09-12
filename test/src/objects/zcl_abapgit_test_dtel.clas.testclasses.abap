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
    DATA lo_dtel        TYPE REF TO zif_abapgit_object.
    DATA li_xml_in      TYPE REF TO zif_abapgit_xml_input.
    DATA li_xml_out     TYPE REF TO zif_abapgit_xml_output.
    DATA lv_xml         TYPE string.
    DATA lv_act         TYPE string.
    DATA lo_log         TYPE REF TO zif_abapgit_log.
    DATA lo_i18n_params TYPE REF TO zcl_abapgit_i18n_params.
    DATA li_xml_check   TYPE REF TO zif_abapgit_xml_input.
    DATA ls_dd04v_act   TYPE dd04v.

    ls_item-obj_type = 'DTEL'.
    ls_item-obj_name = 'ZABAPGIT_TEST_DTEL'.

    lv_xml = |<?xml version="1.0" encoding="utf-8"?>\n| &&
             |<abapGit version="v1.0.0" serializer="LCL_OBJECT_DTEL" serializer_version="v1.0.0">\n| &&
             | <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">\n| &&
             |  <asx:values>\n| &&
             |   <DD04V>\n| &&
             |    <ROLLNAME>ZABAPGIT_TEST_DTEL</ROLLNAME>\n| &&
             |    <DDLANGUAGE>E</DDLANGUAGE>\n| &&
             |    <REFKIND>T</REFKIND>\n| &&
             |    <DATATYPE>CHAR</DATATYPE>\n| &&
             |    <LENG>000010</LENG>\n| &&
             |    <OUTPUTLEN>000010</OUTPUTLEN>\n| &&
             |    <DDTEXT>Testing</DDTEXT>\n| &&
             |    <SCRTEXT_S>Short</SCRTEXT_S>\n| &&
             |    <SCRLEN1>05</SCRLEN1>\n| &&
             |    <SCRTEXT_M>Medium</SCRTEXT_M>\n| &&
             |    <SCRLEN2>10</SCRLEN2>\n| &&
             |    <SCRTEXT_L>Long</SCRTEXT_L>\n| &&
             |    <SCRLEN3>20</SCRLEN3>\n| &&
             |    <REPTEXT>Heading</REPTEXT>\n| &&
             |    <HEADLEN>12</HEADLEN>\n| &&
             |   </DD04V>\n| &&
             |  </asx:values>\n| &&
             | </asx:abap>\n| &&
             |</abapGit>|.

    CREATE OBJECT li_xml_in TYPE zcl_abapgit_xml_input EXPORTING iv_xml = lv_xml.
    CREATE OBJECT lo_log TYPE zcl_abapgit_log.

    lo_i18n_params = zcl_abapgit_i18n_params=>new(
      iv_main_language      = sy-langu
      iv_main_language_only = abap_true ).

    CREATE OBJECT lo_dtel TYPE zcl_abapgit_object_dtel
      EXPORTING
        iv_language    = sy-langu
        is_item        = ls_item
        io_i18n_params = lo_i18n_params.

    lo_dtel->deserialize(
      iv_package   = '$TMP'
      iv_step      = zif_abapgit_object=>gc_step_id-ddic
      ii_log       = lo_log
      iv_transport = ''
      io_xml       = li_xml_in ).

******************

    CREATE OBJECT li_xml_out TYPE zcl_abapgit_xml_output.
    lo_dtel->serialize( li_xml_out ).

    lv_act = li_xml_out->render( is_metadata = lo_dtel->get_metadata( ) ).

    CREATE OBJECT li_xml_check TYPE zcl_abapgit_xml_input EXPORTING iv_xml = lv_act.

    li_xml_check->read(
      EXPORTING iv_name = 'DD04V'
      CHANGING cg_data = ls_dd04v_act ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v_act-rollname
      exp = 'ZABAPGIT_TEST_DTEL'
      msg = lv_act ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v_act-ddlanguage
      exp = 'E' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v_act-refkind
      exp = 'T' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v_act-datatype
      exp = 'CHAR' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v_act-leng
      exp = '000010' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v_act-ddtext
      exp = 'Testing' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v_act-scrtext_s
      exp = 'Short' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v_act-scrlen1
      exp = '05' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v_act-reptext
      exp = 'Heading' ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_dd04v_act-headlen
      exp = '12' ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_test_aff DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PRIVATE SECTION.
    METHODS setup RAISING cx_static_check.
    METHODS teardown RAISING cx_static_check.

    METHODS deserialize_xml_serialize_aff FOR TESTING RAISING cx_static_check.
    METHODS domain_serialize_aff FOR TESTING RAISING cx_static_check.
    METHODS deserialize_aff_serialize_aff FOR TESTING RAISING cx_static_check.
    METHODS serialize_inactive_empty_aff FOR TESTING RAISING cx_static_check.

    METHODS deserialize_xml
      IMPORTING
        is_item        TYPE zif_abapgit_definitions=>ty_item
        iv_xml         TYPE string
        io_files       TYPE REF TO zcl_abapgit_objects_files
      RETURNING
        VALUE(ri_dtel) TYPE REF TO zif_abapgit_object
      RAISING
        zcx_abapgit_exception.

    METHODS serialize_json
      IMPORTING
        ii_dtel        TYPE REF TO zif_abapgit_object
        io_files       TYPE REF TO zcl_abapgit_objects_files
      RETURNING
        VALUE(rv_json) TYPE string
      RAISING
        zcx_abapgit_exception.

ENDCLASS.

CLASS ltcl_test_aff IMPLEMENTATION.
  METHOD setup.
    DATA lo_settings TYPE REF TO zcl_abapgit_settings.
    DATA li_registry TYPE REF TO zif_abapgit_aff_registry.

    zcl_abapgit_inject_setup=>setup( ).

    lo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
    lo_settings->set_experimental_features( zcl_abapgit_aff_registry=>c_aff_feature ).

    " make sure to reset the aff registry in case other tests have set it
    zcl_abapgit_aff_injector=>set_registry( li_registry ).
  ENDMETHOD.

  METHOD teardown.
    DATA lo_settings TYPE REF TO zcl_abapgit_settings.
    DATA li_registry TYPE REF TO zif_abapgit_aff_registry.

    lo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).
    lo_settings->set_experimental_features( '' ).

    zcl_abapgit_aff_injector=>set_registry( li_registry ).

    zcl_abapgit_inject_setup=>teardown( ).
  ENDMETHOD.

  METHOD deserialize_xml.

    DATA li_xml_in      TYPE REF TO zif_abapgit_xml_input.
    DATA lo_log         TYPE REF TO zif_abapgit_log.
    DATA lo_i18n_params TYPE REF TO zcl_abapgit_i18n_params.

    CREATE OBJECT li_xml_in TYPE zcl_abapgit_xml_input EXPORTING iv_xml = iv_xml.
    CREATE OBJECT lo_log TYPE zcl_abapgit_log.

    lo_i18n_params = zcl_abapgit_i18n_params=>new(
      iv_main_language      = sy-langu
      iv_main_language_only = abap_true ).

    CREATE OBJECT ri_dtel TYPE zcl_abapgit_object_dtel
      EXPORTING
        iv_language    = sy-langu
        is_item        = is_item
        io_files       = io_files
        io_i18n_params = lo_i18n_params.

    ri_dtel->deserialize(
      iv_package   = '$TMP'
      iv_step      = zif_abapgit_object=>gc_step_id-ddic
      ii_log       = lo_log
      iv_transport = ''
      io_xml       = li_xml_in ).

  ENDMETHOD.

  METHOD serialize_json.

    DATA li_xml_out TYPE REF TO zif_abapgit_xml_output.
    DATA lt_files   TYPE zif_abapgit_git_definitions=>ty_files_tt.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF lt_files.

    CREATE OBJECT li_xml_out TYPE zcl_abapgit_xml_output.
    ii_dtel->serialize( li_xml_out ).

    lt_files = io_files->get_files( ).

    LOOP AT lt_files ASSIGNING <ls_file> WHERE filename CP '*.dtel.json'.
      rv_json = zcl_abapgit_convert=>xstring_to_string_utf8( <ls_file>-data ).
      EXIT.
    ENDLOOP.

    cl_abap_unit_assert=>assert_not_initial(
      act = rv_json
      msg = 'no JSON file serialized' ).

  ENDMETHOD.

  METHOD deserialize_xml_serialize_aff.

    DATA ls_item  TYPE zif_abapgit_definitions=>ty_item.
    DATA lo_dtel  TYPE REF TO zif_abapgit_object.
    DATA lo_files TYPE REF TO zcl_abapgit_objects_files.
    DATA lv_xml   TYPE string.
    DATA lv_json  TYPE string.
    DATA lv_exp   TYPE string.

    ls_item-obj_type = 'DTEL'.
    ls_item-obj_name = 'ZABAPGIT_TEST_DTEL'.

    lv_xml = |<?xml version="1.0" encoding="utf-8"?>\n| &&
             |<abapGit version="v1.0.0" serializer="LCL_OBJECT_DTEL" serializer_version="v1.0.0">\n| &&
             | <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">\n| &&
             |  <asx:values>\n| &&
             |   <DD04V>\n| &&
             |    <ROLLNAME>ZABAPGIT_TEST_DTEL</ROLLNAME>\n| &&
             |    <DDLANGUAGE>E</DDLANGUAGE>\n| &&
             |    <REFKIND>T</REFKIND>\n| &&
             |    <DATATYPE>CHAR</DATATYPE>\n| &&
             |    <LENG>000010</LENG>\n| &&
             |    <OUTPUTLEN>000010</OUTPUTLEN>\n| &&
             |    <DDTEXT>Testing</DDTEXT>\n| &&
             |    <SCRTEXT_S>Short</SCRTEXT_S>\n| &&
             |    <SCRLEN1>05</SCRLEN1>\n| &&
             |    <SCRTEXT_M>Medium</SCRTEXT_M>\n| &&
             |    <SCRLEN2>10</SCRLEN2>\n| &&
             |    <SCRTEXT_L>Long</SCRTEXT_L>\n| &&
             |    <SCRLEN3>20</SCRLEN3>\n| &&
             |    <REPTEXT>Heading</REPTEXT>\n| &&
             |    <HEADLEN>12</HEADLEN>\n| &&
             |   </DD04V>\n| &&
             |  </asx:values>\n| &&
             | </asx:abap>\n| &&
             |</abapGit>|.

    lo_files = zcl_abapgit_objects_files=>new( ls_item ).

    lo_dtel = deserialize_xml(
      is_item  = ls_item
      iv_xml   = lv_xml
      io_files = lo_files ).

    lv_json = serialize_json(
      ii_dtel  = lo_dtel
      io_files = lo_files ).

    lv_exp = `{` &&
      `"formatVersion":"1",` &&
      `"header":{"description":"Testing","originalLanguage":"en"},` &&
      `"dataTypeInformation":{"category":"predefinedType",` &&
      `"predefinedType":{"dataType":"CHAR","length":10}},` &&
      `"fieldLabels":{"short":"Short","shortLength":5,` &&
      `"medium":"Medium","mediumLength":10,"long":"Long","longLength":20,` &&
      `"heading":"Heading","headingLength":12}` &&
      `}`.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_ajson_utilities=>new( )->is_equal( iv_json_a = lv_json
                                                           iv_json_b = lv_exp )
      exp = abap_true
      msg = lv_json ).

  ENDMETHOD.

  METHOD domain_serialize_aff.

    DATA ls_item  TYPE zif_abapgit_definitions=>ty_item.
    DATA lo_dtel  TYPE REF TO zif_abapgit_object.
    DATA lo_files TYPE REF TO zcl_abapgit_objects_files.
    DATA lv_xml   TYPE string.
    DATA lv_json  TYPE string.
    DATA lv_exp   TYPE string.

    " "predefinedType" requires "dataType" and "length", so the node must not
    " be written for a domain based data element

    ls_item-obj_type = 'DTEL'.
    ls_item-obj_name = 'ZABAPGIT_TEST_DTEL_DOMA'.

    lv_xml = |<?xml version="1.0" encoding="utf-8"?>\n| &&
             |<abapGit version="v1.0.0" serializer="LCL_OBJECT_DTEL" serializer_version="v1.0.0">\n| &&
             | <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">\n| &&
             |  <asx:values>\n| &&
             |   <DD04V>\n| &&
             |    <ROLLNAME>ZABAPGIT_TEST_DTEL_DOMA</ROLLNAME>\n| &&
             |    <DDLANGUAGE>E</DDLANGUAGE>\n| &&
             |    <REFKIND>D</REFKIND>\n| &&
             |    <DOMNAME>ZABAPGIT_TEST_DOMA</DOMNAME>\n| &&
             |    <DDTEXT>Testing</DDTEXT>\n| &&
             |   </DD04V>\n| &&
             |  </asx:values>\n| &&
             | </asx:abap>\n| &&
             |</abapGit>|.

    lo_files = zcl_abapgit_objects_files=>new( ls_item ).

    lo_dtel = deserialize_xml(
      is_item  = ls_item
      iv_xml   = lv_xml
      io_files = lo_files ).

    lv_json = serialize_json(
      ii_dtel  = lo_dtel
      io_files = lo_files ).

    lv_exp = `{` &&
      `"formatVersion":"1",` &&
      `"header":{"description":"Testing","originalLanguage":"en"},` &&
      `"dataTypeInformation":{"category":"domain","typeName":"ZABAPGIT_TEST_DOMA"}` &&
      `}`.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_abapgit_ajson_utilities=>new( )->is_equal( iv_json_a = lv_json
                                                           iv_json_b = lv_exp )
      exp = abap_true
      msg = lv_json ).

  ENDMETHOD.

  METHOD serialize_inactive_empty_aff.

    DATA ls_item        TYPE zif_abapgit_definitions=>ty_item.
    DATA lo_dtel        TYPE REF TO zif_abapgit_object.
    DATA lo_files       TYPE REF TO zcl_abapgit_objects_files.
    DATA li_xml_out     TYPE REF TO zif_abapgit_xml_output.
    DATA lt_files       TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA ls_file        TYPE zif_abapgit_git_definitions=>ty_file.
    DATA lo_i18n_params TYPE REF TO zcl_abapgit_i18n_params.

    ls_item-obj_type = 'DTEL'.
    ls_item-obj_name = 'ZABAPGIT_TEST_DTEL_INACTIVE'.

    lo_files = zcl_abapgit_objects_files=>new( ls_item ).
    lo_i18n_params = zcl_abapgit_i18n_params=>new(
      iv_main_language      = sy-langu
      iv_main_language_only = abap_true ).

    CREATE OBJECT lo_dtel TYPE zcl_abapgit_object_dtel
      EXPORTING
        iv_language    = sy-langu
        is_item        = ls_item
        io_files       = lo_files
        io_i18n_params = lo_i18n_params.

    CREATE OBJECT li_xml_out TYPE zcl_abapgit_xml_output.
    lo_dtel->serialize( li_xml_out ).

    lt_files = lo_files->get_files( ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_files )
      exp = 1 ).

    READ TABLE lt_files INTO ls_file INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_file-filename
      exp = 'zabapgit_test_dtel_inactive.dtel.json' ).
    cl_abap_unit_assert=>assert_initial( ls_file-data ).

  ENDMETHOD.

  METHOD deserialize_aff_serialize_aff.

    DATA ls_item        TYPE zif_abapgit_definitions=>ty_item.
    DATA lo_dtel        TYPE REF TO zif_abapgit_object.
    DATA li_xml_in      TYPE REF TO zif_abapgit_xml_input.
    DATA li_xml_out     TYPE REF TO zif_abapgit_xml_output.
    DATA lo_log         TYPE REF TO zif_abapgit_log.
    DATA lo_i18n_params TYPE REF TO zcl_abapgit_i18n_params.
    DATA lo_files       TYPE REF TO zcl_abapgit_objects_files.
    DATA lt_files       TYPE zif_abapgit_git_definitions=>ty_files_tt.
    DATA ls_file        TYPE zif_abapgit_git_definitions=>ty_file.
    DATA lv_json        TYPE string.
    DATA lv_act         TYPE string.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF lt_files.

    ls_item-obj_type = 'DTEL'.
    ls_item-obj_name = 'ZABAPGIT_TEST_DTEL_AFF'.

    lv_json = `{` && cl_abap_char_utilities=>newline &&
      `  "formatVersion": "1",` && cl_abap_char_utilities=>newline &&
      `  "header": {` && cl_abap_char_utilities=>newline &&
      `    "description": "Testing",` && cl_abap_char_utilities=>newline &&
      `    "originalLanguage": "en"` && cl_abap_char_utilities=>newline &&
      `  },` && cl_abap_char_utilities=>newline &&
      `  "dataTypeInformation": {` && cl_abap_char_utilities=>newline &&
      `    "category": "predefinedType",` && cl_abap_char_utilities=>newline &&
      `    "predefinedType": {` && cl_abap_char_utilities=>newline &&
      `      "dataType": "NUMC",` && cl_abap_char_utilities=>newline &&
      `      "length": 4` && cl_abap_char_utilities=>newline &&
      `    }` && cl_abap_char_utilities=>newline &&
      `  }` && cl_abap_char_utilities=>newline &&
      `}` && cl_abap_char_utilities=>newline.

    ls_file-path     = '/'.
    ls_file-filename = 'zabapgit_test_dtel_aff.dtel.json'.
    ls_file-data     = zcl_abapgit_convert=>string_to_xstring_utf8( lv_json ).
    APPEND ls_file TO lt_files.

    lo_files = zcl_abapgit_objects_files=>new( ls_item ).
    lo_files->set_files( lt_files ).

    CREATE OBJECT lo_log TYPE zcl_abapgit_log.

    lo_i18n_params = zcl_abapgit_i18n_params=>new(
      iv_main_language      = sy-langu
      iv_main_language_only = abap_true ).

    CREATE OBJECT lo_dtel TYPE zcl_abapgit_object_dtel
      EXPORTING
        iv_language    = sy-langu
        is_item        = ls_item
        io_files       = lo_files
        io_i18n_params = lo_i18n_params.

    " li_xml_in stays unbound: the framework supplies no XML when the
    " repository holds JSON metadata for the object
    lo_dtel->deserialize(
      iv_package   = '$TMP'
      iv_step      = zif_abapgit_object=>gc_step_id-ddic
      ii_log       = lo_log
      iv_transport = ''
      io_xml       = li_xml_in ).

******************

    CREATE OBJECT li_xml_out TYPE zcl_abapgit_xml_output.
    lo_dtel->serialize( li_xml_out ).

    CLEAR lt_files.
    lt_files = lo_files->get_files( ).

    " the serialized file is appended after the one read above, so take the last
    LOOP AT lt_files ASSIGNING <ls_file> WHERE filename = 'zabapgit_test_dtel_aff.dtel.json'.
      lv_act = zcl_abapgit_convert=>xstring_to_string_utf8( <ls_file>-data ).
    ENDLOOP.

    cl_abap_unit_assert=>assert_equals(
      act = lv_act
      exp = lv_json ).

  ENDMETHOD.

ENDCLASS.
