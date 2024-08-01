CLASS zcl_abapgit_web_sicf DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_http_extension.
    INTERFACES zif_abapgit_web_request.
    INTERFACES zif_abapgit_web_response.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mi_server TYPE REF TO if_http_server.
ENDCLASS.



CLASS zcl_abapgit_web_sicf IMPLEMENTATION.

  METHOD zif_abapgit_web_request~get_header_field.
    rv_value = mi_server->request->get_header_field( iv_name ).
  ENDMETHOD.

  METHOD zif_abapgit_web_request~get_method.
    rv_method = mi_server->request->get_method( ).
  ENDMETHOD.

  METHOD zif_abapgit_web_request~get_cdata.
    rv_data = mi_server->request->get_cdata( ).
  ENDMETHOD.

  METHOD zif_abapgit_web_response~set_content_type.
    mi_server->response->set_content_type( iv_type ).
  ENDMETHOD.

  METHOD zif_abapgit_web_response~set_cdata.
    mi_server->response->set_cdata( iv_data ).
  ENDMETHOD.

  METHOD zif_abapgit_web_response~set_xdata.
    mi_server->response->set_data( iv_data ).
  ENDMETHOD.

  METHOD if_http_extension~handle_request.

    mi_server = server.

    server->set_session_stateful( ).

    zcl_abapgit_web=>handle(
      ii_request  = me
      ii_response = me ).

  ENDMETHOD.

ENDCLASS.