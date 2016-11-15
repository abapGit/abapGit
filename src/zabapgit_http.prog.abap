*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_HTTP
*&---------------------------------------------------------------------*

CLASS lcl_http_digest DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      run
        IMPORTING
          ii_client TYPE REF TO if_http_client
          iv_username TYPE string
          iv_password TYPE string.

  PRIVATE SECTION.
    CLASS-DATA: gv_nc TYPE n LENGTH 8.

    CLASS-METHODS:
      parse
        IMPORTING
          iv_value  TYPE string
        EXPORTING
          ev_scheme TYPE string
          ev_realm  TYPE string
          ev_qop    TYPE string
          ev_nonce  TYPE string,
      md5
        IMPORTING
          iv_data        TYPE string
        RETURNING
          VALUE(rv_hash) TYPE string,
      hash
        IMPORTING
          iv_qop             TYPE string
          iv_realm           TYPE string
          iv_nonce           TYPE string
          iv_username        TYPE clike
          iv_uri             TYPE string
          iv_method          TYPE string
          iv_cnonse          TYPE string
          iv_password        TYPE clike
        RETURNING
          VALUE(rv_response) TYPE string.

ENDCLASS.

CLASS lcl_http_digest IMPLEMENTATION.

  METHOD hash.

    DATA(lv_ha1) = md5( |{ iv_username }:{ iv_realm }:{ iv_password }| ).
    DATA(lv_ha2) = md5( |{ iv_method }:{ iv_uri }| ).

    rv_response = md5( |{ lv_ha1 }:{ iv_nonce }:{ gv_nc }:{ iv_cnonse }:{ iv_qop }:{ lv_ha2 }| ).

  ENDMETHOD.

  METHOD run.

    DATA: lv_value    TYPE string,
          lv_scheme   TYPE string,
          lv_realm    TYPE string,
          lv_response TYPE string,
          lv_method   TYPE string,
          lv_uri      TYPE string,
          lv_auth     TYPE string,
          lv_cnonce   TYPE string,
          lv_qop      TYPE string,
          lv_nonce    TYPE string.


    lv_value = ii_client->response->get_header_field( 'www-authenticate' ).

    parse(
      EXPORTING
        iv_value  = lv_value
      IMPORTING
        ev_scheme = lv_scheme
        ev_realm  = lv_realm
        ev_qop    = lv_qop
        ev_nonce  = lv_nonce ).

    ASSERT NOT lv_nonce IS INITIAL.

    lv_method = 'GET'.
    lv_uri = ii_client->request->get_header_field( '~request_uri' ).

    CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'
      EXPORTING
        number_chars  = 24
      IMPORTING
        random_string = lv_cnonce.

    lv_response = hash(
      iv_qop      = lv_qop
      iv_realm    = lv_realm
      iv_nonce    = lv_nonce
      iv_username = p_user
      iv_uri      = lv_uri
      iv_method   = lv_method
      iv_cnonse   = lv_cnonce
      iv_password = p_pass ).

* client response
    lv_auth = |Digest username="{ p_user
      }", realm="{ lv_realm
      }", nonce="{ lv_nonce
      }", uri="{ lv_uri
      }", qop={ lv_qop
      }, nc={ gv_nc
      }, cnonce="{ lv_cnonce
      }", response="{ lv_response }"|.

    ii_client->request->set_header_field(
      name  = 'Authorization'
      value = lv_auth ).

  ENDMETHOD.

  METHOD parse.

    CLEAR: ev_scheme,
           ev_realm,
           ev_qop,
           ev_nonce.

    FIND REGEX '^(\w+)' IN iv_value SUBMATCHES ev_scheme.
    FIND REGEX 'realm="([\w ]+)"' IN iv_value SUBMATCHES ev_realm.
    FIND REGEX 'qop="(\w+)"' IN iv_value SUBMATCHES ev_qop.
    FIND REGEX 'nonce="([\w=/+\$]+)"' IN iv_value SUBMATCHES ev_nonce.

  ENDMETHOD.

  METHOD md5.

    DATA: lv_xstr TYPE xstring,
          lv_hash TYPE xstring.


    lv_xstr = lcl_convert=>string_to_xstring_utf8( iv_data ).

    CALL FUNCTION 'CALCULATE_HASH_FOR_RAW'
      EXPORTING
        alg            = 'MD5'
        data           = lv_xstr
      IMPORTING
        hashxstring    = lv_hash
      EXCEPTIONS
        unknown_alg    = 1
        param_error    = 2
        internal_error = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.

    rv_hash = lv_hash.
    TRANSLATE rv_hash TO LOWER CASE.

  ENDMETHOD.

ENDCLASS.

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
        IMPORTING iv_url           TYPE string
                  iv_service       TYPE string
        RETURNING VALUE(ri_client) TYPE REF TO if_http_client
        RAISING   lcx_exception.

  PRIVATE SECTION.
    CLASS-METHODS:
      check_auth_requested
        IMPORTING ii_client                TYPE REF TO if_http_client
        RETURNING VALUE(rv_auth_requested) TYPE abap_bool
        RAISING   lcx_exception,
      is_local_system
        IMPORTING iv_url         TYPE string
        RETURNING VALUE(rv_bool) TYPE abap_bool,
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

    IF is_local_system( iv_url ) = abap_true.
      ri_client->send_sap_logon_ticket( ).
    ENDIF.

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

    lcl_login_manager=>load( iv_uri    = iv_url
                             ii_client = ri_client ).

    send_receive( ri_client ).
    IF check_auth_requested( ri_client ) = abap_true.
      acquire_login_details( ii_client = ri_client
                             iv_url    = iv_url ).
      send_receive( ri_client ).
    ENDIF.
    check_http_200( ri_client ).

    lcl_login_manager=>save( iv_uri    = iv_url
                             ii_client = ri_client ).

  ENDMETHOD.

  METHOD is_local_system.

    DATA: lv_host TYPE string,
          lt_list TYPE STANDARD TABLE OF icm_sinfo2 WITH DEFAULT KEY.


    CALL FUNCTION 'ICM_GET_INFO2'
      TABLES
        servlist           = lt_list
      EXCEPTIONS
        icm_error          = 1
        icm_timeout        = 2
        icm_not_authorized = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    FIND REGEX 'https?://([^/^:]*)' IN iv_url
      SUBMATCHES lv_host.

    READ TABLE lt_list WITH KEY hostname = lv_host TRANSPORTING NO FIELDS.
    rv_bool = boolc( sy-subrc = 0 ).

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

    DATA: lv_default_user TYPE string,
          lv_scheme       TYPE string,
          lv_user         TYPE string,
          lv_pass         TYPE string.


    lv_default_user = lcl_app=>user( )->get_repo_username( iv_url ).
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
      lcl_app=>user( )->set_repo_username( iv_url      = iv_url
                                           iv_username = lv_user ).
    ENDIF.

    lv_scheme = ii_client->response->get_header_field( 'www-authenticate' ).
    FIND REGEX '^(\w+)' IN lv_scheme SUBMATCHES lv_scheme.

    CASE lv_scheme.
      WHEN 'Digest'.
* https://en.wikipedia.org/wiki/Digest_access_authentication
* eg used by https://www.gerritcodereview.com/
        lcl_http_digest=>run(
          ii_client   = ii_client
          iv_username = lv_user
          iv_password = lv_pass ).
      WHEN OTHERS.
* https://en.wikipedia.org/wiki/Basic_access_authentication
        ii_client->authenticate(
          username = lv_user
          password = lv_pass ).
    ENDCASE.

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