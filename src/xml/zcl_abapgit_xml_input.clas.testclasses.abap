CLASS ltcl_xml DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PUBLIC SECTION.
    METHODS:
      up FOR TESTING
        RAISING zcx_abapgit_exception,
      empty FOR TESTING
        RAISING zcx_abapgit_exception,
      down FOR TESTING
        RAISING zcx_abapgit_exception.

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

ENDCLASS.
