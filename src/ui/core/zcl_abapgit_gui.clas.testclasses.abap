CLASS ltcl_parse_data DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      test_1 FOR TESTING RAISING zcx_abapgit_exception,
      test_2 FOR TESTING RAISING zcx_abapgit_exception,
      test_3 FOR TESTING RAISING zcx_abapgit_exception,
      test_4 FOR TESTING RAISING zcx_abapgit_exception,
      test_5 FOR TESTING RAISING zcx_abapgit_exception,
      test_6 FOR TESTING RAISING zcx_abapgit_exception,
      test_7 FOR TESTING RAISING zcx_abapgit_exception,
      test_8 FOR TESTING RAISING zcx_abapgit_exception.

    DATA:
      mo_gui        TYPE REF TO zcl_abapgit_gui,
      mv_getdata    TYPE c LENGTH 256,
      mt_postdata   TYPE cnht_post_data_tab,
      mo_parameters TYPE REF TO zcl_abapgit_string_map.

ENDCLASS.


CLASS ltcl_parse_data IMPLEMENTATION.

  METHOD setup.
    TRY.
        CREATE OBJECT mo_gui.
      CATCH zcx_abapgit_exception.
        BREAK-POINT.
    ENDTRY.
    CLEAR: mv_getdata, mt_postdata.
  ENDMETHOD.

  METHOD test_1.

    " get data without parameter name
    mv_getdata = '00000001'.

    mo_parameters = mo_gui->parse_data(
      iv_getdata  = mv_getdata
      it_postdata = mt_postdata ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_parameters->get( 'param_0001' )
      exp = '00000001' ).

  ENDMETHOD.

  METHOD test_2.

    " get data with one parameter
    mv_getdata = 'key=00000001'.

    mo_parameters = mo_gui->parse_data(
      iv_getdata  = mv_getdata
      it_postdata = mt_postdata ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_parameters->get( 'key' )
      exp = '00000001' ).

  ENDMETHOD.

  METHOD test_3.

    " get data with several parameters
    mv_getdata = 'p1=hello&p2=world'.

    mo_parameters = mo_gui->parse_data(
      iv_getdata  = mv_getdata
      it_postdata = mt_postdata ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_parameters->get( 'p1' )
      exp = 'hello' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_parameters->get( 'p2' )
      exp = 'world' ).

  ENDMETHOD.

  METHOD test_4.

    " get data with escaped parameter value
    mv_getdata = 'p3=100%25more%3dbest%20result%26happy%20developers'.

    mo_parameters = mo_gui->parse_data(
      iv_getdata  = mv_getdata
      it_postdata = mt_postdata ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_parameters->get( 'p3' )
      exp = '100%more=best result&happy developers' ).

  ENDMETHOD.

  METHOD test_5.

    " just post data
    APPEND 'p1=hello&p2=world' TO mt_postdata.

    mo_parameters = mo_gui->parse_data(
      iv_getdata  = mv_getdata
      it_postdata = mt_postdata ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_parameters->get( 'p1' )
      exp = 'hello' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_parameters->get( 'p2' )
      exp = 'world' ).

  ENDMETHOD.

  METHOD test_6.

    " get & post data
    mv_getdata = 'p3=and&p4=good-bye'.
    APPEND 'p1=hello&p2=world' TO mt_postdata.

    mo_parameters = mo_gui->parse_data(
      iv_getdata  = mv_getdata
      it_postdata = mt_postdata ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_parameters->get( 'p1' )
      exp = 'hello' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_parameters->get( 'p2' )
      exp = 'world' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_parameters->get( 'p3' )
      exp = 'and' ).
    cl_abap_unit_assert=>assert_equals(
      act = mo_parameters->get( 'p4' )
      exp = 'good-bye' ).

  ENDMETHOD.

  METHOD test_7.

    " duplicate parameters
    mv_getdata = 'p1=hello&p1=world'.

    TRY.
        mo_parameters = mo_gui->parse_data(
          iv_getdata  = mv_getdata
          it_postdata = mt_postdata ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_abapgit_exception.
        " no futher check
    ENDTRY.

  ENDMETHOD.

  METHOD test_8.

    " post data with multiple lines and spaces (#3624)
    APPEND 'p1=hello&p2=world' TO mt_postdata.
    APPEND 'and more friends&' TO mt_postdata.
    APPEND 'p3=good-bye' TO mt_postdata.

    mo_parameters = mo_gui->parse_data(
      iv_getdata  = mv_getdata
      it_postdata = mt_postdata ).

    cl_abap_unit_assert=>assert_equals(
      act = mo_parameters->get( 'p1' )
      exp = 'hello' ).
    cl_abap_unit_assert=>assert_equals(
      act = condense( mo_parameters->get( 'p2' ) )
      exp = 'world and more friends' ).
    cl_abap_unit_assert=>assert_equals(
      act = strlen( mo_parameters->get( 'p2' ) )
      exp = 260 ). " includes spaces
    cl_abap_unit_assert=>assert_equals(
      act = mo_parameters->get( 'p3' )
      exp = 'good-bye' ).
    cl_abap_unit_assert=>assert_equals(
      act = strlen( mo_parameters->get( 'p3' ) )
      exp = 8 ). "no spaces

  ENDMETHOD.

ENDCLASS.
