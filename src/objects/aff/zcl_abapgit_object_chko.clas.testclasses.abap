*CLASS ltcl_chko DEFINITION FINAL FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    CLASS-DATA:
*      environment TYPE REF TO if_osql_test_environment.
*    CLASS-METHODS:
*      class_setup,
*      class_teardown.
*
*    DATA:
*      cut TYPE REF TO zif_abapgit_object.
*    METHODS:
*      setup,
*      insert_chko_in_db IMPORTING name TYPE wb_object_name,
*      get_chko_as_json RETURNING VALUE(result) TYPE rswsourcet,
*      get_chko_as_json_no_params RETURNING VALUE(result) TYPE rswsourcet,
*      serialize FOR TESTING RAISING cx_static_check,
*      deserialize_chko_w_params FOR TESTING RAISING cx_static_check,
*      deserialize_chko_wo_params FOR TESTING RAISING cx_static_check,
*      delete FOR TESTING RAISING cx_static_check.
*ENDCLASS.
*
*
*CLASS ltcl_chko IMPLEMENTATION.
*
*  METHOD class_setup.
*    environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #(
*      ( 'TADIR' )
*      ( 'CHKO_HEADER' )
*      ( 'CHKO_HEADERT' )
*      ( 'CHKO_CONTENT' )
*      ( 'CHKO_PARAMETER' )
*      ( 'CHKO_PARAMETERST' )
*     ) ).
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
*      obj_name = 'CHKO_TEST'
*      obj_type = 'CHKO'
*      devclass = '$TMP' ).
*    cut = NEW zcl_abapgit_object_chko( iv_language = sy-langu
*                                          is_item = item ).
*    cut->mo_files = NEW zcl_abapgit_objects_files( is_item = item ).
*    environment->clear_doubles( ).
*  ENDMETHOD.
*
*  METHOD serialize.
*    insert_chko_in_db( 'CHKO_TEST' ).
*    cut->serialize(
*      io_xml = NEW zcl_abapgit_xml_output( ) ).
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
*  METHOD deserialize_chko_wo_params.
*    DATA(json_table) = get_chko_as_json_no_params( ).
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
*    SELECT FROM chko_header FIELDS * INTO TABLE @DATA(header).
*    cl_abap_unit_assert=>assert_subrc( act = sy-subrc ).
*
*    cl_abap_unit_assert=>assert_true( cut->exists( ) ).
*
*  ENDMETHOD.
*
*  METHOD deserialize_chko_w_params.
*    DATA(json_table) = get_chko_as_json( ).
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
*    SELECT FROM chko_header FIELDS * INTO TABLE @DATA(header).
*    cl_abap_unit_assert=>assert_subrc( act = sy-subrc ).
*
*    cl_abap_unit_assert=>assert_true( cut->exists( ) ).
*  ENDMETHOD.
*
*  METHOD delete.
*    insert_chko_in_db( 'CHKO_TEST' ).
*
*    cut->delete( iv_package = '$TMP' ).
*
*    cl_abap_unit_assert=>assert_false( cut->exists( ) ).
*  ENDMETHOD.
*
*  METHOD insert_chko_in_db.
*    DATA chko_header TYPE TABLE OF chko_header.
*    chko_header = VALUE #(
*      ( name = name version = 'A' abap_language_version = if_abap_language_version=>gc_version-sap_cloud_platform )
*      ).
*    environment->insert_test_data( chko_header ).
*
*    DATA chko_headert TYPE TABLE OF chko_headert.
*    chko_headert = VALUE #(
*      ( name = name version = 'A' spras = sy-langu description = 'Test description' )
*    ).
*    environment->insert_test_data( chko_headert ).
*
*    DATA chko_content TYPE TABLE OF chko_content.
*    chko_content = VALUE #(
*      ( name = name version = 'A' category = 'TEST_CATEGORY' implementing_class = 'TEST_CLASS'
*        remote_enabled = abap_true )
*    ).
*    environment->insert_test_data( chko_content ).
*
*    DATA chko_parameter TYPE TABLE OF chko_parameter.
*    chko_parameter = VALUE #(
*      ( chko_name = name version = 'A' technical_id = 1 name = 'parameter' modifiable = abap_true )
*    ).
*    environment->insert_test_data( chko_parameter ).
*
*    DATA chko_parameterst TYPE TABLE OF chko_parameterst.
*    chko_parameterst = VALUE #(
*      ( chko_name = name version = 'A' technical_id = 1 spras = sy-langu description = 'Parameter description' )
*    ).
*    environment->insert_test_data( chko_parameterst ).
*
*    DATA tadir TYPE TABLE OF tadir.
*    tadir = VALUE #(
*      ( pgmid = 'R3TR' object = 'CHKO' obj_name = name masterlang = 'E' )
*    ).
*    environment->insert_test_data( tadir ).
*  ENDMETHOD.
*
*  METHOD get_chko_as_json.
*    result = VALUE #(
*( `{` )
*( `  "formatVersion": "1",` )
*( `  "header": {` )
*( `    "description": "Test description",` )
*( `    "originalLanguage": "EN"` )
*( `  },` )
*( `  "category": "TEST_CATEGORY",` )
*( `  "implementingClass": "TEST_CLASS",` )
*( `  "parameters": [` )
*( `    {` )
*( `      "technicalId": "1",` )
*( `      "name": "parameter",` )
*( `      "description": "Parameter description",` )
*( `      "hidden": true` )
*( `    }` )
*( `  ]` )
*( `}` ) ).
*  ENDMETHOD.
*
*  METHOD get_chko_as_json_no_params.
*    result = VALUE #(
*( `{` )
*( `  "formatVersion": "1",` )
*( `  "header": {` )
*( `    "description": "Test description",` )
*( `    "originalLanguage": "EN"` )
*( `  },` )
*( `  "category": "TEST_CATEGORY",` )
*( `  "implementingClass": "TEST_CLASS"` )
*( `}` ) ).
*  ENDMETHOD.
*
*ENDCLASS.
