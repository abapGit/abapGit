CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL CRITICAL FINAL.

  PRIVATE SECTION.
    METHODS deserialize_serialize FOR TESTING RAISING cx_static_check.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  METHOD deserialize_serialize.

    DATA ls_item    TYPE zif_abapgit_definitions=>ty_item.
    DATA lo_doma    TYPE REF TO zif_abapgit_object.
    DATA li_xml_in  TYPE REF TO zif_abapgit_xml_input.
    DATA li_xml_out TYPE REF TO zif_abapgit_xml_output.
    DATA lv_xml     TYPE string.
    DATA lv_act     TYPE string.
    DATA lo_log     TYPE REF TO zif_abapgit_log.

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

    lo_doma = zcl_abapgit_objects=>create_object( ls_item ).

    lo_doma->deserialize(
      iv_package   = '$TMP'
      iv_step      = zif_abapgit_object=>gc_step_id-ddic
      ii_log       = lo_log
      iv_transport = ''
      io_xml       = li_xml_in ).

    CREATE OBJECT li_xml_out TYPE zcl_abapgit_xml_output.
    lo_doma->serialize( li_xml_out ).

    lv_act = li_xml_out->render( ).

    " Output should be generated
    cl_abap_unit_assert=>assert_not_initial( lv_act ).

  ENDMETHOD.

ENDCLASS.
