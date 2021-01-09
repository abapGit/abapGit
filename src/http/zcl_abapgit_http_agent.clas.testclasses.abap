
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS create FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD create.

    DATA li_agent TYPE REF TO zif_abapgit_http_agent.
    DATA li_response TYPE REF TO zif_abapgit_http_response.

    li_agent = zcl_abapgit_http_agent=>create( ).
    li_response = li_agent->request( 'https://httpbin.org/get' ).

    cl_abap_unit_assert=>assert_equals(
      act = li_response->is_ok( )
      exp = abap_true ).

  ENDMETHOD.

ENDCLASS.
