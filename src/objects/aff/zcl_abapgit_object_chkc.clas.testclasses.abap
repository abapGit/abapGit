*CLASS ltcl_chkc_handler DEFINITION FINAL FOR TESTING
*  DURATION SHORT
*  RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*    CLASS-DATA:
*      environment TYPE REF TO if_osql_test_environment,
*      chkc_object TYPE trkey.
*    CLASS-METHODS:
*      class_setup,
*      class_teardown.
*
*    DATA:
*      cut         TYPE REF TO zif_abapgit_object.
*
*    METHODS:
*      setup,
*      serialize FOR TESTING RAISING cx_static_check,
*      delete FOR TESTING RAISING cx_static_check,
*      deserialize FOR TESTING RAISING cx_static_check,
*      deserialize_without_parent FOR TESTING RAISING cx_static_check,
*      exsits FOR TESTING RAISING cx_static_check,
*      serialize_without_parent FOR TESTING RAISING cx_static_check,
*      deserialize_additional_props FOR TESTING RAISING cx_static_check,
*      insert_chkc_in_db IMPORTING name       TYPE if_aff_obj=>obj_name
*                                  parent_cat TYPE string OPTIONAL,
*      get_chkc_as_json  IMPORTING parent_cat    TYPE string OPTIONAL
*                        RETURNING VALUE(result) TYPE rswsourcet,
*      get_chkc_additionalProps  RETURNING VALUE(result) TYPE rswsourcet,
*      assert_chkc_exists_on_db IMPORTING chkc       TYPE trkey
*                                         user       TYPE sy-uname
*                                         parent_cat TYPE string OPTIONAL.
*
*ENDCLASS.
*
*
*CLASS ltcl_chkc_handler IMPLEMENTATION.
*
*  METHOD class_setup.
*    environment = cl_osql_test_environment=>create( i_dependency_list = VALUE #(
*      ( 'CHKC_HEADER' )
*      ( 'CHKC_HEADERT' )
*      ( 'CHKC_CONTENT' )
*      ( 'TADIR' )
*     ) ).
*
*    chkc_object-devclass = '$TMP'.
*    chkc_object-obj_type = 'CHKC'.
*    chkc_object-obj_name = 'CHKC_TEST'.
*  ENDMETHOD.
*
*  METHOD class_teardown.
*    IF environment IS BOUND.
*      environment->destroy( ).
*    ENDIF.
*  ENDMETHOD.
*
*  METHOD setup.
*    environment->clear_doubles( ).
*
*    DATA(item) = VALUE zif_abapgit_definitions=>ty_item(
*    obj_name = 'CHKC_TEST'
*    obj_type = 'CHKC'
*    devclass = '$TMP' ).
*
*    cut = NEW zcl_abapgit_object_chkc( iv_language = sy-langu
*                                           is_item = item ).
*    cut->mo_files = NEW zcl_abapgit_objects_files( is_item = item ).
*
*    environment->clear_doubles( ).
*
*  ENDMETHOD.
*
*  METHOD serialize.
*    insert_chkc_in_db( name = CONV #( chkc_object-obj_name ) parent_cat = 'TEST_PARENT_CATEGORY' ).
*
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
*  METHOD serialize_without_parent.
*    insert_chkc_in_db( name = CONV #( chkc_object-obj_name ) ).
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
*  METHOD delete.
*    insert_chkc_in_db( CONV #( chkc_object-obj_name ) ).
*
*    cut->delete( iv_package = '$TMP' ).
*
*    cl_abap_unit_assert=>assert_false( cut->exists( ) ).
*  ENDMETHOD.
*
*  METHOD deserialize.
*
*    DATA(json_table) =  get_chkc_as_json( 'TEST_PARENT_CATEGORY' ).
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
*    assert_chkc_exists_on_db( chkc = chkc_object  parent_cat = 'TEST_PARENT_CATEGORY' user = sy-uname ).
*  ENDMETHOD.
*
*  METHOD deserialize_without_parent.
*
*    DATA(json_table) =  get_chkc_as_json( ).
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
*    assert_chkc_exists_on_db( chkc = chkc_object user = sy-uname ).
*  ENDMETHOD.
*
*  METHOD deserialize_additional_Props.
*
*    DATA(json_table) = get_chkc_additionalProps( ).
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
*    assert_chkc_exists_on_db( chkc = chkc_object  parent_cat = 'TEST_PARENT_CATEGORY' user = sy-uname ).
*  ENDMETHOD.
*
*  METHOD exsits.
*    cl_abap_unit_assert=>assert_false( cut->exists( ) ).
*
*    insert_chkc_in_db( CONV #( chkc_object-obj_name ) ).
*
*    cl_abap_unit_assert=>assert_true( cut->exists( ) ).
*  ENDMETHOD.
*
*  METHOD insert_chkc_in_db.
*    DATA chkc_header TYPE TABLE OF chkc_header.
*    chkc_header = VALUE #(
*      ( name = name version = 'A' abap_language_version = if_abap_language_version=>gc_version-sap_cloud_platform )
*    ).
*    environment->insert_test_data( chkc_header ).
*
*    DATA chkc_headert TYPE TABLE OF chkc_headert.
*    chkc_headert = VALUE #(
*      ( name = name version = 'A' spras = 'E' description = 'Test description' )
*    ).
*    environment->insert_test_data( chkc_headert ).
*
*    DATA chkc_content TYPE TABLE OF chkc_content.
*    chkc_content = VALUE #(
*      ( name = name version = 'A' category = parent_cat )
*    ).
*    environment->insert_test_data( chkc_content ).
*
*    DATA tadir TYPE TABLE OF tadir.
*    tadir = VALUE #(
*      ( pgmid = 'R3TR' object = 'CHKC' obj_name = name masterlang = 'E')
*    ).
*    environment->insert_test_data( tadir ).
*
*  ENDMETHOD.
*
*  METHOD get_chkc_as_json.
*    result = VALUE #(
*    ( `{` )
*    ( |  "formatVersion":"{ cl_chkc_aff_objecthandler=>c_format_version }",|  )
*    ( `  "header": {` )
*    ( `    "description": "Test description",` )
*    ( `    "originalLanguage": "EN",` )
*    ( `    "abapLanguageVersion": "cloudDevelopment"` ) ).
*    IF parent_cat IS NOT INITIAL.
*      APPEND  `  },`  TO result.
*      APPEND `  "parentCategory": "` && parent_cat && `"`  TO result.
*    ELSE.
*      APPEND  `  }`  TO result.
*    ENDIF.
*    APPEND `}` TO result.
*  ENDMETHOD.
*
*  METHOD get_chkc_additionalProps.
*    result = VALUE #(
*( `{` )
*( `  "formatVersion": "1",` )
*( `  "header": {` )
*( `    "description": "Test description",` )
*( `    "originalLanguage": "EN",` )
*( `    "abapLanguageVersion": "cloudDevelopment",` )
*( `    "additionalProps": true` )
*( `  },` )
*( `  "parentCategory": "TEST_PARENT_CATEGORY",` )
*( `  "additionalProps": true` )
*( `}` ) ) .
*  ENDMETHOD.
*
*  METHOD assert_chkc_exists_on_db.
*
*    SELECT SINGLE FROM chkc_header FIELDS * WHERE name = @chkc-obj_name AND version = 'A' INTO @DATA(chkc_header).
*    cl_abap_unit_assert=>assert_equals( exp = user act = chkc_header-changed_by ).
*    cl_abap_unit_assert=>assert_equals( exp = user act = chkc_header-created_by ).
*    cl_abap_unit_assert=>assert_equals( exp = '5' act = chkc_header-abap_language_version ).
*
*    SELECT SINGLE FROM chkc_headert FIELDS * WHERE name = @chkc-obj_name AND version = 'A' AND spras = 'E' INTO @DATA(chkc_headert).
*    cl_abap_unit_assert=>assert_equals( exp = 'Test description' act = chkc_headert-description ).
*
*    SELECT SINGLE FROM chkc_content FIELDS * WHERE name = @chkc-obj_name AND version = 'A' INTO @DATA(chkc_content).
*    IF parent_cat IS SUPPLIED.
*      cl_abap_unit_assert=>assert_equals( exp = parent_cat act = chkc_content-category ).
*    ELSE.
*      cl_abap_unit_assert=>assert_equals( exp = '' act = chkc_content-category ).
*    ENDIF.
*  ENDMETHOD.
*
*ENDCLASS.
