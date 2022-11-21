CLASS ltcl_xml DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS:
      up FOR TESTING RAISING zcx_abapgit_exception,
      empty FOR TESTING RAISING zcx_abapgit_exception,
      input FOR TESTING RAISING zcx_abapgit_exception,
      read_intf FOR TESTING RAISING zcx_abapgit_exception,
      down FOR TESTING RAISING zcx_abapgit_exception.

    TYPES: BEGIN OF ty_old,
             foo TYPE i,
             bar TYPE c LENGTH 1,
           END OF ty_old.

    TYPES: BEGIN OF ty_new,
             foo TYPE i,
             bar TYPE c LENGTH 1,
             moo TYPE f,
           END OF ty_new.

ENDCLASS.


CLASS ltcl_xml IMPLEMENTATION.

  METHOD input.

    DATA lv_xml   TYPE string.
    DATA lo_input TYPE REF TO zcl_abapgit_xml_input.
    DATA ls_data  TYPE ty_old.

    lv_xml = |<?xml version="1.0" encoding="utf-16"?>\n| &&
      |<abapGit version="v1.0.0">\n| &&
      | <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">\n| &&
      |  <asx:values>\n| &&
      |   <DATA>\n| &&
      |    <FOO>2</FOO>\n| &&
      |   </DATA>\n| &&
      |  </asx:values>\n| &&
      | </asx:abap>\n| &&
      |</abapGit>|.

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.

    lo_input->zif_abapgit_xml_input~read( EXPORTING iv_name = 'DATA'
                                          CHANGING cg_data = ls_data ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_data-foo
      exp = 2 ).

  ENDMETHOD.

  METHOD empty.

    DATA: ls_old    TYPE ty_old,
          ls_new    TYPE ty_new,
          lv_xml    TYPE string,
          lo_input  TYPE REF TO zcl_abapgit_xml_input,
          lo_output TYPE REF TO zcl_abapgit_xml_output.


    CLEAR ls_old.

    CREATE OBJECT lo_output.
    lo_output->zif_abapgit_xml_output~add( iv_name = 'DATA'
                                           ig_data = ls_old ).
    lv_xml = lo_output->zif_abapgit_xml_output~render( ).

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.
    lo_input->zif_abapgit_xml_input~read( EXPORTING iv_name = 'DATA'
                                          CHANGING cg_data = ls_new ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_new-foo
      exp = ls_old-foo ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_new-bar
      exp = ls_old-bar ).

  ENDMETHOD.

  METHOD up.

    DATA: ls_old    TYPE ty_old,
          ls_new    TYPE ty_new,
          lv_xml    TYPE string,
          lo_input  TYPE REF TO zcl_abapgit_xml_input,
          lo_output TYPE REF TO zcl_abapgit_xml_output.


    ls_old-foo = 2.
    ls_old-bar = 'A'.

    CREATE OBJECT lo_output.
    lo_output->zif_abapgit_xml_output~add( iv_name = 'DATA'
                                           ig_data = ls_old ).
    lv_xml = lo_output->zif_abapgit_xml_output~render( ).

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.
    lo_input->zif_abapgit_xml_input~read( EXPORTING iv_name = 'DATA'
                                          CHANGING cg_data = ls_new ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_new-foo
      exp = ls_old-foo ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_new-bar
      exp = ls_old-bar ).

  ENDMETHOD.

  METHOD down.

    DATA: ls_old    TYPE ty_old,
          ls_new    TYPE ty_new,
          lv_xml    TYPE string,
          lo_input  TYPE REF TO zcl_abapgit_xml_input,
          lo_output TYPE REF TO zcl_abapgit_xml_output.


    ls_new-foo = 2.
    ls_new-bar = 'A'.
    ls_new-moo = 5.

    CREATE OBJECT lo_output.
    lo_output->zif_abapgit_xml_output~add( iv_name = 'DATA'
                                           ig_data = ls_new ).
    lv_xml = lo_output->zif_abapgit_xml_output~render( ).

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.
    lo_input->zif_abapgit_xml_input~read( EXPORTING iv_name = 'DATA'
                                          CHANGING cg_data = ls_old ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_old-foo
      exp = ls_new-foo ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_old-bar
      exp = ls_new-bar ).

  ENDMETHOD.

  METHOD read_intf.

    DATA ls_vseointerf TYPE vseointerf.
    DATA lv_xml        TYPE string.
    DATA lo_input      TYPE REF TO zcl_abapgit_xml_input.

    lv_xml = |<?xml version="1.0" encoding="utf-8"?>\n| &&
      |<abapGit version="v1.0.0" serializer="LCL_OBJECT_INTF" serializer_version="v1.0.0">\n| &&
      | <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">\n| &&
      |  <asx:values>\n| &&
      |   <VSEOINTERF>\n| &&
      |    <CLSNAME>ZIF_ABAPGIT_UNIT_TEST</CLSNAME>\n| &&
      |    <LANGU>E</LANGU>\n| &&
      |    <DESCRIPT>test</DESCRIPT>\n| &&
      |    <EXPOSURE>2</EXPOSURE>\n| &&
      |    <STATE>1</STATE>\n| &&
      |    <UNICODE>X</UNICODE>\n| &&
      |   </VSEOINTERF>\n| &&
      |  </asx:values>\n| &&
      | </asx:abap>\n| &&
      |</abapGit>|.

    CREATE OBJECT lo_input
      EXPORTING
        iv_xml = lv_xml.

    lo_input->zif_abapgit_xml_input~read( EXPORTING iv_name = 'VSEOINTERF'
                                          CHANGING cg_data = ls_vseointerf ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_vseointerf-langu
      exp = 'E' ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_vseointerf-descript
      exp = 'test' ).

  ENDMETHOD.

ENDCLASS.
