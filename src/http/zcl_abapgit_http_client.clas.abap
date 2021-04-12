CLASS zcl_abapgit_http_client DEFINITION PUBLIC CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING ii_client TYPE REF TO if_http_client,
      close,
      set_digest
        IMPORTING io_digest TYPE REF TO zcl_abapgit_http_digest,
      send_receive_close
        IMPORTING iv_data        TYPE xstring
        RETURNING VALUE(rv_data) TYPE xstring
        RAISING   zcx_abapgit_exception,
      get_cdata
        RETURNING VALUE(rv_value) TYPE string,
      check_http_200
        RAISING zcx_abapgit_exception,
      check_smart_response
        IMPORTING iv_expected_content_type TYPE string
                  iv_content_regex         TYPE string
        RAISING   zcx_abapgit_exception,
      send_receive
        RAISING zcx_abapgit_exception,
      set_headers
        IMPORTING iv_url     TYPE string
                  iv_service TYPE string
        RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA: mi_client TYPE REF TO if_http_client,
          mo_digest TYPE REF TO zcl_abapgit_http_digest.

ENDCLASS.



CLASS zcl_abapgit_http_client IMPLEMENTATION.


  METHOD check_http_200.

    DATA: lv_code TYPE i,
          lv_text TYPE string.

    mi_client->response->get_status( IMPORTING code = lv_code ).
    CASE lv_code.
      WHEN 200.
        RETURN. " Success, OK
      WHEN 302.
        zcx_abapgit_exception=>raise( 'Resource access temporarily redirected (HTTP 302). Check the URL' ).
      WHEN 401.
        zcx_abapgit_exception=>raise( 'Unauthorized access to resource (HTTP 401). Check your credentials' ).
      WHEN 403.
        zcx_abapgit_exception=>raise( 'Access to resource forbidden (HTTP 403)' ).
      WHEN 404.
        zcx_abapgit_exception=>raise( 'Resource not found (HTTP 404). Check the URL' ).
      WHEN 407.
        zcx_abapgit_exception=>raise( 'Proxy authentication required (HTTP 407). Check your credentials' ).
      WHEN 408.
        zcx_abapgit_exception=>raise( 'Request timeout (HTTP 408)' ).
      WHEN 415.
        zcx_abapgit_exception=>raise( 'Unsupported media type (HTTP 415)' ).
      WHEN 422.
        zcx_abapgit_exception=>raise( 'Unprocessable entity (HTTP 422). Check, if URL has to end with ".git"' ).
      WHEN OTHERS.
        lv_text = mi_client->response->get_cdata( ).
        zcx_abapgit_exception=>raise( |(HTTP { lv_code }) { lv_text }| ).
    ENDCASE.

  ENDMETHOD.


  METHOD check_smart_response.

    DATA: lv_content_type TYPE string.
    DATA: lv_data         TYPE string.

    IF iv_expected_content_type IS NOT INITIAL.
      lv_content_type = mi_client->response->get_content_type( ).
      IF lv_content_type <> iv_expected_content_type.
        zcx_abapgit_exception=>raise( 'Wrong Content-Type sent by server - no fallback to the dumb protocol!' ).
      ENDIF.
    ENDIF.

    IF iv_content_regex IS NOT INITIAL.
      lv_data = mi_client->response->get_cdata( ).
      FIND REGEX iv_content_regex IN lv_data.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Wrong Content sent by server' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD close.
    mi_client->close( ).
  ENDMETHOD.


  METHOD constructor.
    mi_client = ii_client.
  ENDMETHOD.


  METHOD get_cdata.
    rv_value = mi_client->response->get_cdata( ).
  ENDMETHOD.


  METHOD send_receive.

    DATA: lv_text    TYPE string,
          lv_code    TYPE i,
          lv_message TYPE string.

    mi_client->send(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        http_invalid_timeout       = 4
        OTHERS                     = 5 ).

    IF sy-subrc = 0.
      mi_client->receive(
        EXCEPTIONS
          http_communication_failure = 1
          http_invalid_state         = 2
          http_processing_failed     = 3
          OTHERS                     = 4 ).
    ENDIF.

    IF sy-subrc <> 0.
      " in case of HTTP_COMMUNICATION_FAILURE
      " make sure:
      " a) SSL is setup properly in STRUST
      " b) no firewalls
      " check trace file in transaction SMICM

      mi_client->get_last_error(
        IMPORTING
          code    = lv_code
          message = lv_message ).

      lv_text = |HTTP error { lv_code } occured: { lv_message }|.

      zcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

  ENDMETHOD.


  METHOD send_receive_close.

* do not use set_cdata as it modifies the Content-Type header field
    mi_client->request->set_data( iv_data ).
    send_receive( ).
    check_http_200( ).
    rv_data = mi_client->response->get_data( ).
    mi_client->close( ).

  ENDMETHOD.


  METHOD set_digest.
    mo_digest = io_digest.
  ENDMETHOD.


  METHOD set_headers.

    DATA: lv_value TYPE string.


    mi_client->request->set_header_field(
        name  = '~request_method'
        value = 'POST' ).

    lv_value = zcl_abapgit_url=>path_name( iv_url ) &&
      '/git-' &&
      iv_service &&
      '-pack'.
    mi_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_value ).

    lv_value = 'application/x-git-'
                  && iv_service && '-pack-request'.
    mi_client->request->set_header_field(
        name  = 'Content-Type'
        value = lv_value ).

    lv_value = 'application/x-git-'
                  && iv_service && '-pack-result'.
    mi_client->request->set_header_field(
        name  = 'Accept'
        value = lv_value ).

    IF mo_digest IS BOUND.
      mo_digest->run( mi_client ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
