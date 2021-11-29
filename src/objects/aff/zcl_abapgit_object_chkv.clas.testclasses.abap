*CLASS ltcl_chkv DEFINITION FINAL FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    CLASS-DATA:
*      environment       TYPE REF TO if_osql_test_environment,
*      chkv_example      TYPE chkv_content,
*      chkv_example_json TYPE chkv_content,
*      chkv_object       TYPE trkey.
*    CLASS-METHODS:
*      class_setup,
*      class_teardown.
*
*    DATA:
*      cut         TYPE REF TO zif_abapgit_object.
*
*    METHODS: setup,
*      serialize FOR TESTING RAISING cx_static_check,
*      delete FOR TESTING RAISING cx_static_check,
*      deserialize FOR TESTING RAISING cx_static_check,
*      deserialize_additional_props FOR TESTING RAISING cx_static_check,
*      chkv_exists FOR TESTING RAISING cx_static_check,
*      serialize_json_parameters FOR TESTING RAISING cx_static_check,
*      insert_chkv_in_db IMPORTING name    TYPE trkey-obj_name
*                                  as_json TYPE abap_bool DEFAULT abap_false,
*      get_chkv_as_json RETURNING VALUE(result) TYPE rswsourcet,
*      get_chkv_additional_props RETURNING VALUE(result) TYPE rswsourcet,
*      assert_chkv_exists_on_db IMPORTING chkv TYPE trkey
*                                         user TYPE sy-uname OPTIONAL.
*
*ENDCLASS.
*
*
*CLASS ltcl_chkv IMPLEMENTATION.
*
*  METHOD class_setup.
*
*    SELECT SINGLE * FROM chkv_content INTO @chkv_example WHERE name = 'AFF_EXAMPLE_CHKV' AND version = 'A'.
*    SELECT SINGLE * FROM chkv_content INTO @chkv_example_json WHERE name = 'AFF_EXAMPLE_CHKV_JSON' AND version = 'A'.
*
*    environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #(
*      ( 'TADIR' )
*      ( 'CHKV_HEADER' )
*      ( 'CHKV_HEADERT' )
*      ( 'CHKV_CONTENT' )
*     ) ).
*    chkv_object-devclass = '$TMP'.
*    chkv_object-obj_type = 'CHKV'.
*    chkv_object-obj_name = 'CHKV_TEST'.
*  ENDMETHOD.
*
*  METHOD class_teardown.
*    IF environment IS BOUND.
*      environment->destroy( ).
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD setup.
*    DATA(item) = VALUE zif_abapgit_definitions=>ty_item(
*      obj_name = 'CHKV_TEST'
*      obj_type = 'CHKV'
*      devclass = '$TMP' ).
*
*    cut = NEW zcl_abapgit_object_chkv( iv_language = sy-langu
*                                           is_item = item ).
*    cut->mo_files = NEW zcl_abapgit_objects_files( is_item = item ).
*    environment->clear_doubles( ).
*  ENDMETHOD.
*
*  METHOD serialize.
*    insert_chkv_in_db( chkv_object-obj_name ).
*
*    cut->serialize(
*     io_xml = NEW zcl_abapgit_xml_output( ) ).
*
*    DATA(act_files) = cut->mo_files->get_files( ).
*
*    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( act_files ) ).
*    DATA(json) = cl_abap_codepage=>convert_from( act_files[ 1 ]-data ).
*    cl_abap_unit_assert=>assert_not_initial(
*      EXPORTING
*        act              = json  ).
*  ENDMETHOD.
*
*  METHOD serialize_json_parameters.
*    insert_chkv_in_db( chkv_object-obj_name ).
*    cut->serialize(
*     io_xml = NEW zcl_abapgit_xml_output( ) ).
*
*    DATA(act_files) = cut->mo_files->get_files( ).
*
*
*    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( act_files ) ).
*    DATA(json) = cl_abap_codepage=>convert_from( act_files[ 1 ]-data ).
*    cl_abap_unit_assert=>assert_not_initial(
*      EXPORTING
*        act              = json  ).
*
*  ENDMETHOD.
*
*  METHOD delete.
*    insert_chkv_in_db( chkv_object-obj_name ).
*
*    cut->delete( iv_package = '$TMP' ).
*
*    cl_abap_unit_assert=>assert_false( cut->exists( ) ).
*  ENDMETHOD.
*
*  METHOD deserialize.
*    DATA(json_table) = get_chkv_as_json( ).
*    CONCATENATE LINES OF json_table INTO DATA(json).
*    DATA(json_as_xstring) = zcl_abapgit_convert=>string_to_xstring_utf8( json ).
*
*    cut->mo_files->add_raw( iv_ext = 'json' iv_data = json_as_xstring ).
*
*    cut->deserialize(
*      iv_package = '$TMP'
*      io_xml     = VALUE #( )
*      iv_step    = zif_abapgit_object=>gc_step_id-abap
*      ii_log     = NEW zcl_abapgit_log( ) ).
*
*
*    assert_chkv_exists_on_db( chkv = chkv_object user = sy-uname ).
*  ENDMETHOD.
*
*  METHOD deserialize_additional_props.
*    DATA(json_table) = get_chkv_additional_props( ).
*    CONCATENATE LINES OF json_table INTO DATA(json).
*    DATA(json_as_xstring) = zcl_abapgit_convert=>string_to_xstring_utf8( json ).
*
*    cut->mo_files->add_raw( iv_ext = 'json' iv_data = json_as_xstring ).
*
*    cut->deserialize(
*      iv_package = '$TMP'
*      io_xml     = VALUE #( )
*      iv_step    = zif_abapgit_object=>gc_step_id-abap
*      ii_log     = NEW zcl_abapgit_log( ) ).
*
*
*    assert_chkv_exists_on_db( chkv = chkv_object user = sy-uname ).
*  ENDMETHOD.
*
*  METHOD chkv_exists.
*    cl_abap_unit_assert=>assert_false( cut->exists( ) ).
*
*    insert_chkv_in_db( chkv_object-obj_name ).
*
*    cl_abap_unit_assert=>assert_true( cut->exists( ) ).
*  ENDMETHOD.
*
*  METHOD insert_chkv_in_db.
*
*    DATA chkv_header TYPE TABLE OF chkv_header.
*    chkv_header = VALUE #(
*      ( name = name version = 'A' abap_language_version = if_abap_language_version=>gc_version-sap_cloud_platform )
*    ).
*    environment->insert_test_data( chkv_header ).
*
*    DATA chkv_headert TYPE TABLE OF chkv_headert.
*    chkv_headert = VALUE #(
*      ( name = name version = 'A' spras = 'E' description = 'Example CHKV for ABAP file formats' )
*    ).
*    environment->insert_test_data( chkv_headert ).
*
*    DATA chkv_content TYPE TABLE OF chkv_content.
*
*    IF as_json = abap_true.
*      chkv_content = VALUE #( ( name = name version = 'A' content  = chkv_example_json-content ) ).
*    ELSE.
*      chkv_content = VALUE #( ( name = name version = 'A' content  = chkv_example-content ) ).
*    ENDIF.
*    environment->insert_test_data( chkv_content ).
*
*  ENDMETHOD.
*
*  METHOD get_chkv_as_json.
*
*    result = VALUE #(
*( `{` )
*( |  "formatVersion": "{ cl_chkv_aff_objecthandler=>c_format_version }",| )
*( `  "header": { ` )
*( `    "description": "Example CHKV for ABAP file formats",` )
*( `    "originalLanguage": "EN",` )
*( `    "abapLanguageVersion": "cloudDevelopment"` )
*( `  },`)
*( `  "selectedChecks": [` )
*( `    {`)
*( `      "checkName": "CI_SEARCH_ABAP_PATTERN",` )
*( `      "parameters": [` )
*( `        {` )
*( `          "name": "StatementPattern",` )
*( `          "valueList": [` )
*( `            "CALL FUNCTION 'DB_EXISTS_INDEX' *",` )
*( `            "CALL FUNCTION 'DD_INDEX_NAME' *"` )
*( `          ]` )
*( `        },` )
*( `        {` )
*( `          "name": "FindingSeverity",` )
*( `          "value": "E"` )
*( `        }` )
*( `      ]` )
*( `    },` )
*( `    {` )
*( `      "checkName": "SLIN_VERS",` )
*( `      "parameters": [` )
*( `        {` )
*( `          "name": "ABAPLanguageVersion",` )
*( `          "value": "1"` )
*( `        }` )
*( `      ]` )
*( `    },` )
*( `    {` )
*( `      "checkName": "SYCM_S4H_SEARCH_DB_OPS",` )
*( `      "parameters": [` )
*( `        {` )
*( `          "name": "ReleaseID",` )
*( `          "value": "S4HANA_READINESS_1809"` )
*( `        },` )
*( `        {` )
*( `          "name": "SAPNotes",` )
*( `          "valueRangeList": [` )
*( `            {` )
*( `              "sign": "exclude",` )
*( `              "option": "equals",` )
*( `              "low": "0002470721",` )
*( `              "high": "0000000001"` )
*( `            }` )
*( `          ]` )
*( `        },` )
*( `        {` )
*( `          "name": "SimplificationItemCategories",` )
*( `          "valueRangeList": [` )
*( `            {` )
*( `              "sign": "exclude",` )
*( `              "option": "equals",` )
*( `              "low": "P",` )
*( `              "high": ""` )
*( `            }` )
*( `          ]` )
*( `        }` )
*( `      ]` )
*( `    }` )
*( `  ]` )
*( `}` )
*    ) .
*  ENDMETHOD.
*
*  METHOD get_chkv_additional_props.
*
*    result = VALUE #(
*( `{` )
*( |  "formatVersion": "{ cl_chkv_aff_objecthandler=>c_format_version }",| )
*( `  "header": { ` )
*( `    "description": "Example CHKV for ABAP file formats",` )
*( `    "originalLanguage": "EN",` )
*( `    "abapLanguageVersion": "cloudDevelopment",` )
*( `    "additionalProps": true` )
*( `  },`)
*( `  "selectedChecks": [` )
*( `    {`)
*( `      "checkName": "CI_SEARCH_ABAP_PATTERN",` )
*( `      "parameters": [` )
*( `        {` )
*( `          "name": "StatementPattern",` )
*( `          "valueList": [` )
*( `            "CALL FUNCTION 'DB_EXISTS_INDEX' *",` )
*( `            "CALL FUNCTION 'DD_INDEX_NAME' *"` )
*( `          ]` )
*( `        },` )
*( `        {` )
*( `          "name": "FindingSeverity",` )
*( `          "value": "E"` )
*( `        }` )
*( `      ]` )
*( `    },` )
*( `    {` )
*( `      "checkName": "SLIN_VERS",` )
*( `      "parameters": [` )
*( `        {` )
*( `          "name": "ABAPLanguageVersion",` )
*( `          "value": "1"` )
*( `        }` )
*( `      ]` )
*( `    },` )
*( `    {` )
*( `      "checkName": "SYCM_S4H_SEARCH_DB_OPS",` )
*( `      "parameters": [` )
*( `        {` )
*( `          "name": "ReleaseID",` )
*( `          "value": "S4HANA_READINESS_1809"` )
*( `        },` )
*( `        {` )
*( `          "name": "SAPNotes",` )
*( `          "valueRangeList": [` )
*( `            {` )
*( `              "sign": "exclude",` )
*( `              "option": "equals",` )
*( `              "low": "0002470721",` )
*( `              "high": "0000000001",` )
*( `              "additionalProps": true` )
*( `            }` )
*( `          ],` )
*( `          "additionalProps": true` )
*( `        },` )
*( `        {` )
*( `          "name": "SimplificationItemCategories",` )
*( `          "valueRangeList": [` )
*( `            {` )
*( `              "sign": "exclude",` )
*( `              "option": "equals",` )
*( `              "low": "P",` )
*( `              "high": "",` )
*( `              "additionalProps": true` )
*( `            }` )
*( `          ],` )
*( `          "additionalProps": true` )
*( `        }` )
*( `      ],` )
*( `      "additionalProps": true` )
*( `    }` )
*( `  ],` )
*( `  "additionalProps": true` )
*( `}` )
*    ) .
*  ENDMETHOD.
*
*  METHOD assert_chkv_exists_on_db.
*
*    SELECT SINGLE FROM chkv_header FIELDS * WHERE name = @chkv-obj_name AND version = 'A' INTO @DATA(chkv_header).
*    cl_abap_unit_assert=>assert_equals( exp = user act = chkv_header-changed_by ).
*    cl_abap_unit_assert=>assert_equals( exp = user act = chkv_header-created_by ).
*    cl_abap_unit_assert=>assert_equals( exp = '5' act = chkv_header-abap_language_version ).
*
*    SELECT SINGLE FROM chkv_headert FIELDS * WHERE name = @chkv-obj_name AND version = 'A' AND spras = 'E' INTO @DATA(chkv_headert).
*    cl_abap_unit_assert=>assert_equals( exp = 'Example CHKV for ABAP file formats' act = chkv_headert-description ).
*
*  ENDMETHOD.
*
*ENDCLASS.
