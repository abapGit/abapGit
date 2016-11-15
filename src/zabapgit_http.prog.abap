*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_HTTP
*&---------------------------------------------------------------------*

CLASS lcl_http DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      check_http_200
        IMPORTING ii_client TYPE REF TO if_http_client
        RAISING   lcx_exception,
      send_receive
        IMPORTING ii_client TYPE REF TO if_http_client
        RAISING   lcx_exception,
      get_agent
        RETURNING VALUE(rv_agent) TYPE string,
      create_by_url
        IMPORTING iv_url TYPE string
                  iv_service     TYPE string
        RETURNING VALUE(ri_client) TYPE REF TO if_http_client
        RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS:
      check_auth_requested
        IMPORTING ii_client                TYPE REF TO if_http_client
        RETURNING VALUE(rv_auth_requested) TYPE abap_bool
        RAISING   lcx_exception,
      acquire_login_details
        IMPORTING ii_client TYPE REF TO if_http_client
                  iv_url    TYPE string
        RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_http IMPLEMENTATION.

  METHOD get_agent.

* bitbucket require agent prefix = "git/"
    rv_agent = 'git/abapGit-' && gc_abap_version.

  ENDMETHOD.

  METHOD create_by_url.

    DATA: lv_uri                   TYPE string,
          lv_expect_potentual_auth TYPE abap_bool,
          lo_settings              TYPE REF TO lcl_settings.


    lo_settings = lcl_app=>settings( )->read( ).

    cl_http_client=>create_by_url(
      EXPORTING
        url           = lcl_url=>host( iv_url )
        ssl_id        = 'ANONYM'
        proxy_host    = lo_settings->get_proxy_url( )
        proxy_service = lo_settings->get_proxy_port( )
      IMPORTING
        client        = ri_client ).

    ri_client->request->set_cdata( '' ).
    ri_client->request->set_header_field(
        name  = '~request_method'
        value = 'GET' ).
    ri_client->request->set_header_field(
        name  = 'user-agent'
        value = get_agent( ) ).                             "#EC NOTEXT
    lv_uri = lcl_url=>path_name( iv_url ) &&
             '/info/refs?service=git-' &&
             iv_service &&
             '-pack'.
    ri_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_uri ).

    " Disable internal auth dialog (due to its unclarity)
    ri_client->propertytype_logon_popup = if_http_client=>co_disabled.

    lv_expect_potentual_auth = boolc(
      lcl_login_manager=>load( iv_uri    = iv_url
                               ii_client = ri_client ) IS INITIAL ).

    send_receive( ri_client ).
    IF lv_expect_potentual_auth = abap_true
       AND check_auth_requested( ri_client ) = abap_true.
      acquire_login_details( ii_client = ri_client
                             iv_url    = iv_url ).
      send_receive( ri_client ).
    ENDIF.
    check_http_200( ri_client ).

    lcl_login_manager=>save( iv_uri    = iv_url
                             ii_client = ri_client ).

  ENDMETHOD.

  METHOD send_receive.

    DATA lv_text TYPE string.

    ii_client->send( ).
    ii_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          " make sure:
          " a) SSL is setup properly in STRUST
          " b) no firewalls
          " check trace file in transaction SMICM
          lv_text = 'HTTP Communication Failure'.           "#EC NOTEXT
        WHEN 2.
          lv_text = 'HTTP Invalid State'.                   "#EC NOTEXT
        WHEN 3.
          lv_text = 'HTTP Processing failed'.               "#EC NOTEXT
        WHEN OTHERS.
          lv_text = 'Another error occured'.                "#EC NOTEXT
      ENDCASE.
      lcx_exception=>raise( lv_text ).
    ENDIF.

  ENDMETHOD.  "send_receive

  METHOD check_auth_requested.

    DATA: lv_code TYPE i.

    ii_client->response->get_status(
      IMPORTING
        code   = lv_code ).
    IF lv_code = 401.
      rv_auth_requested = abap_true.
    ENDIF.

  ENDMETHOD.  "check_auth_requested

  METHOD acquire_login_details.
    DATA:
      lv_default_user TYPE string,
      lv_user         TYPE string,
      lv_pass         TYPE string.

    lv_default_user = lcl_app=>user( )->get_repo_username( iv_url = iv_url ).
    lv_user         = lv_default_user.

    lcl_password_dialog=>popup(
      EXPORTING
        iv_repo_url = iv_url
      CHANGING
        cv_user     = lv_user
        cv_pass     = lv_pass ).

    IF lv_user IS INITIAL.
      lcx_exception=>raise( 'HTTP 401, unauthorized' ).
    ENDIF.

    IF lv_user <> lv_default_user.
      lcl_app=>user( )->set_repo_username( iv_url = iv_url iv_username = lv_user ).
    ENDIF.

    ii_client->authenticate(
      username = lv_user
      password = lv_pass ).

  ENDMETHOD.  "acquire_login_details

  METHOD check_http_200.

    DATA: lv_code TYPE i,
          lv_text TYPE string.


    ii_client->response->get_status(
      IMPORTING
        code   = lv_code ).
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 302.
        lcx_exception=>raise( 'HTTP redirect, check URL' ).
      WHEN 401.
        lcx_exception=>raise( 'HTTP 401, unauthorized' ).
      WHEN 403.
        lcx_exception=>raise( 'HTTP 403, forbidden' ).
      WHEN 404.
        lcx_exception=>raise( 'HTTP 404, not found' ).
      WHEN 415.
        lcx_exception=>raise( 'HTTP 415, unsupported media type' ).
      WHEN OTHERS.
        lv_text = ii_client->response->get_cdata( ).
        lcx_exception=>raise( |HTTP error code: { lv_code }, { lv_text }| ).
    ENDCASE.

  ENDMETHOD.                                                "http_200

ENDCLASS.