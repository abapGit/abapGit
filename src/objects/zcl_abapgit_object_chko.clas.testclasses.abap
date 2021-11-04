class ltcl_chko definition final for testing
  duration short
  risk level harmless.

  private section.
    class-data:
      environment type ref to if_osql_test_environment.
    class-methods:
      class_setup,
      class_teardown.

    data:
      cut type ref to zif_abapgit_object.
    methods:
      setup,
      insert_chko_in_db importing name type wb_object_name,
      get_chko_as_json returning value(result) type rswsourcet,
      serialize for testing raising cx_static_check,
      deserialize for testing raising cx_static_check.
endclass.


class ltcl_chko implementation.

  method class_setup.
    environment = cl_osql_test_environment=>create( i_dependency_list = value #(
      ( 'TADIR' )
      ( 'CHKO_HEADER' )
      ( 'CHKO_HEADERT' )
      ( 'CHKO_CONTENT' )
      ( 'CHKO_PARAMETER' )
      ( 'CHKO_PARAMETERST' )
     ) ).
  endmethod.

  method class_teardown.
    if environment is bound.
      environment->destroy( ).
    endif.
  endmethod.

  method setup.
    data(item) = value zif_abapgit_definitions=>ty_item(
      obj_name = 'CHKO_TEST'
      obj_type = 'CHKO'
      devclass = '$TMP' ).
    cut = new zcl_abapgit_object_chko( iv_language = sy-langu
                                      is_item = item ).
    cut->mo_files = new zcl_abapgit_objects_files( is_item = item ).
    environment->clear_doubles( ).
  endmethod.

  method serialize.
    insert_chko_in_db( 'CHKO_TEST' ).
    cut->serialize(
      io_xml = new zcl_abapgit_xml_output( ) ).
*      ii_log = new zcl_abapgit_log( ) ).

    data(act_files) = cut->mo_files->get_files( ).

    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( act_files ) ).
    data(json) = cl_abap_codepage=>convert_from( act_files[ 1 ]-data ).
  endmethod.

  method deserialize.
    data(json) = get_chko_as_json( ).
    data(json_as_xstring) = new zcl_abapgit_ajson_cnt_handler( )->serialize( data = json ).
    cut->mo_files->add_raw( iv_ext = 'json' iv_data = json_as_xstring ).

    cut->deserialize(
      iv_package = '$TMP'
      io_xml     = value #( )
      iv_step    = zif_abapgit_object=>gc_step_id-abap
      ii_log     = new zcl_abapgit_log( ) ).

    select from chko_header fields * into table @data(header).
  endmethod.

  method insert_chko_in_db.
    data chko_header type table of chko_header.
    chko_header = value #(
      ( name = name version = 'A' abap_language_version = if_abap_language_version=>gc_version-sap_cloud_platform )
    ).
    environment->insert_test_data( chko_header ).

    data chko_headert type table of chko_headert.
    chko_headert = value #(
      ( name = name version = 'A' spras = sy-langu description = 'Test description' )
    ).
    environment->insert_test_data( chko_headert ).

    data chko_content type table of chko_content.
    chko_content = value #(
      ( name = name version = 'A' category = 'TEST_CATEGORY' implementing_class = 'TEST_CLASS' remote_enabled = abap_true )
    ).
    environment->insert_test_data( chko_content ).

    data chko_parameter type table of chko_parameter.
    chko_parameter = value #(
      ( chko_name = name version = 'A' technical_id = 1 name = 'parameter' modifiable = abap_true )
    ).
    environment->insert_test_data( chko_parameter ).

    data chko_parameterst type table of chko_parameterst.
    chko_parameterst = value #(
      ( chko_name = name version = 'A' technical_id = 1 spras = sy-langu description = 'Parameter description' )
    ).
    environment->insert_test_data( chko_parameterst ).

    data tadir type table of tadir.
    tadir = value #(
      ( pgmid = 'R3TR' object = 'CHKO' obj_name = name masterlang = 'E' )
    ).
    environment->insert_test_data( tadir ).
  endmethod.

  method get_chko_as_json.
    result = value #(
( `{` )
( `  "header": {` )
( `    "formatVersion": "1",` )
( `    "description": "Test description",` )
( `    "masterLanguage": "EN",` )
( `    "abapLanguageVersion": "standard"` )
( `  },` )
( `  "content": {` )
( `    "category": "TEST_CATEGORY",` )
( `    "implementingClass": "TEST_CLASS",` )
( `    "remoteEnabled": true,` )
( `    "parameters": [` )
( `      {` )
( `        "technicalId": "1",` )
( `        "name": "parameter",` )
( `        "description": "Parameter description",` )
( `        "modifiable": true` )
( `      }` )
( `    ]` )
( `  }` )
( `}` ) ).
  endmethod.

endclass.
