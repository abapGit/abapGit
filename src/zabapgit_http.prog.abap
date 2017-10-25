*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_HTTP
*&---------------------------------------------------------------------*

CLASS lcl_proxy_auth DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      run
        IMPORTING ii_client TYPE REF TO if_http_client
        RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    CLASS-DATA: gv_username TYPE string,
                gv_password TYPE string.

    CLASS-METHODS: enter RAISING zcx_abapgit_exception.

ENDCLASS.

CLASS lcl_proxy_auth IMPLEMENTATION.

  METHOD run.

    IF gv_username IS INITIAL OR gv_password IS INITIAL.
      enter( ).
    ENDIF.

    ii_client->authenticate(
      proxy_authentication = abap_true
      username             = gv_username
      password             = gv_password ).

  ENDMETHOD.

  METHOD enter.

    lcl_password_dialog=>popup(
      EXPORTING
        iv_repo_url = 'Proxy Authentication'
      CHANGING
        cv_user     = gv_username
        cv_pass     = gv_password ).

    IF gv_username IS INITIAL OR gv_password IS INITIAL.
      zcx_abapgit_exception=>raise( 'Proxy auth failed' ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_http_digest DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
                  ii_client   TYPE REF TO if_http_client
                  iv_username TYPE string
                  iv_password TYPE string
        RAISING   zcx_abapgit_exception,
      run
        IMPORTING
                  ii_client TYPE REF TO if_http_client
        RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA: mv_ha1      TYPE string,
          mv_username TYPE string,
          mv_realm    TYPE string,
          mv_qop      TYPE string,
          mv_nonce    TYPE string.

    CLASS-DATA: gv_nc TYPE n LENGTH 8.

    CLASS-METHODS:
      md5
        IMPORTING
                  iv_data        TYPE string
        RETURNING
                  VALUE(rv_hash) TYPE string
        RAISING   zcx_abapgit_exception.

    METHODS:
      hash
        IMPORTING
                  iv_qop             TYPE string
                  iv_nonce           TYPE string
                  iv_uri             TYPE string
                  iv_method          TYPE string
                  iv_cnonse          TYPE string
        RETURNING
                  VALUE(rv_response) TYPE string
        RAISING   zcx_abapgit_exception,
      parse
        IMPORTING
          ii_client TYPE REF TO if_http_client.

ENDCLASS.

CLASS lcl_http_client DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING ii_client TYPE REF TO if_http_client,
      close,
      set_digest
        IMPORTING io_digest TYPE REF TO lcl_http_digest,
      send_receive_close
        IMPORTING
                  iv_data        TYPE xstring
        RETURNING
                  VALUE(rv_data) TYPE xstring
        RAISING   zcx_abapgit_exception,
      get_cdata
        RETURNING VALUE(rv_value) TYPE string,
      check_http_200
        RAISING zcx_abapgit_exception,
      send_receive
        RAISING zcx_abapgit_exception,
      set_headers
        IMPORTING iv_url     TYPE string
                  iv_service TYPE string
        RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    DATA: mi_client TYPE REF TO if_http_client,
          mo_digest TYPE REF TO lcl_http_digest.

ENDCLASS.

CLASS lcl_http_client IMPLEMENTATION.

  METHOD constructor.
    mi_client = ii_client.
  ENDMETHOD.

  METHOD set_digest.
    mo_digest = io_digest.
  ENDMETHOD.

  METHOD send_receive_close.

* do not use set_cdata as it modifies the Content-Type header field
    mi_client->request->set_data( iv_data ).
    send_receive( ).
    check_http_200( ).
    rv_data = mi_client->response->get_data( ).
    mi_client->close( ).

  ENDMETHOD.

  METHOD get_cdata.
    rv_value = mi_client->response->get_cdata( ).
  ENDMETHOD.

  METHOD close.
    mi_client->close( ).
  ENDMETHOD.

  METHOD set_headers.

    DATA: lv_value TYPE string.


    mi_client->request->set_header_field(
        name  = '~request_method'
        value = 'POST' ).

    lv_value = lcl_url=>path_name( iv_url ) &&
      '/git-' &&
      iv_service &&
      '-pack'.
    mi_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_value ).

    lv_value = 'application/x-git-'
                  && iv_service && '-pack-request'.         "#EC NOTEXT
    mi_client->request->set_header_field(
        name  = 'Content-Type'
        value = lv_value ).                                 "#EC NOTEXT

    lv_value = 'application/x-git-'
                  && iv_service && '-pack-result'.          "#EC NOTEXT
    mi_client->request->set_header_field(
        name  = 'Accept'
        value = lv_value ).                                 "#EC NOTEXT

    IF mo_digest IS BOUND.
      mo_digest->run( mi_client ).
    ENDIF.

  ENDMETHOD.                    "set_headers

  METHOD send_receive.

    DATA: lv_text    TYPE string,
          lv_code    TYPE i,
          lv_message TYPE string.

    mi_client->send( ).
    mi_client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).

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

  ENDMETHOD.  "send_receive

  METHOD check_http_200.

    DATA: lv_code TYPE i,
          lv_text TYPE string.


    mi_client->response->get_status(
      IMPORTING
        code   = lv_code ).
    CASE lv_code.
      WHEN 200.
        RETURN.
      WHEN 302.
        zcx_abapgit_exception=>raise( 'HTTP redirect, check URL' ).
      WHEN 401.
        zcx_abapgit_exception=>raise( 'HTTP 401, unauthorized' ).
      WHEN 403.
        zcx_abapgit_exception=>raise( 'HTTP 403, forbidden' ).
      WHEN 404.
        zcx_abapgit_exception=>raise( 'HTTP 404, not found' ).
      WHEN 415.
        zcx_abapgit_exception=>raise( 'HTTP 415, unsupported media type' ).
      WHEN OTHERS.
        lv_text = mi_client->response->get_cdata( ).
        zcx_abapgit_exception=>raise( |HTTP error code: { lv_code }, { lv_text }| ).
    ENDCASE.

  ENDMETHOD.                                                "http_200

ENDCLASS.

CLASS lcl_http_digest IMPLEMENTATION.

  METHOD constructor.

    parse( ii_client ).

    mv_ha1 = md5( |{ iv_username }:{ mv_realm }:{ iv_password }| ).

    mv_username = iv_username.

  ENDMETHOD.

  METHOD hash.

    DATA: lv_ha2 TYPE string.


    lv_ha2 = md5( |{ iv_method }:{ iv_uri }| ).

    ASSERT NOT iv_cnonse IS INITIAL.

    rv_response = md5( |{ mv_ha1 }:{ iv_nonce }:{ gv_nc }:{ iv_cnonse }:{ iv_qop }:{ lv_ha2 }| ).

  ENDMETHOD.

  METHOD run.

    DATA: lv_response TYPE string,
          lv_method   TYPE string,
          lv_cnonce   TYPE string,
          lv_uri      TYPE string,
          lv_auth     TYPE string.


    ASSERT NOT mv_nonce IS INITIAL.

    lv_method = ii_client->request->get_header_field( '~request_method' ).
    lv_uri = ii_client->request->get_header_field( '~request_uri' ).

    CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'
      EXPORTING
        number_chars  = 24
      IMPORTING
        random_string = lv_cnonce.

    lv_response = hash(
      iv_qop    = mv_qop
      iv_nonce  = mv_nonce
      iv_uri    = lv_uri
      iv_method = lv_method
      iv_cnonse = lv_cnonce ).

* client response
    lv_auth = |Digest username="{ mv_username
      }", realm="{ mv_realm
      }", nonce="{ mv_nonce
      }", uri="{ lv_uri
      }", qop={ mv_qop
      }, nc={ gv_nc
      }, cnonce="{ lv_cnonce
      }", response="{ lv_response }"|.

    ii_client->request->set_header_field(
      name  = 'Authorization'
      value = lv_auth ).

  ENDMETHOD.

  METHOD parse.

    DATA: lv_value TYPE string.


    lv_value = ii_client->response->get_header_field( 'www-authenticate' ).

    FIND REGEX 'realm="([\w ]+)"' IN lv_value SUBMATCHES mv_realm.
    FIND REGEX 'qop="(\w+)"' IN lv_value SUBMATCHES mv_qop.
    FIND REGEX 'nonce="([\w=/+\$]+)"' IN lv_value SUBMATCHES mv_nonce.

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
      zcx_abapgit_exception=>raise( 'error from CALCULATE_HASH_FOR_RAW' ).
    ENDIF.

    rv_hash = lv_hash.
    TRANSLATE rv_hash TO LOWER CASE.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_http DEFINITION FINAL.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF gc_scheme,
                 digest TYPE string VALUE 'Digest',
               END OF gc_scheme.

    CLASS-METHODS:
      get_agent
        RETURNING VALUE(rv_agent) TYPE string,
      create_by_url
        IMPORTING iv_url           TYPE string
                  iv_service       TYPE string
        RETURNING VALUE(ro_client) TYPE REF TO lcl_http_client
        RAISING   zcx_abapgit_exception.

  PRIVATE SECTION.
    CLASS-METHODS:
      check_auth_requested
        IMPORTING ii_client                TYPE REF TO if_http_client
        RETURNING VALUE(rv_auth_requested) TYPE abap_bool
        RAISING   zcx_abapgit_exception,
      is_local_system
        IMPORTING iv_url         TYPE string
        RETURNING VALUE(rv_bool) TYPE abap_bool,
      acquire_login_details
        IMPORTING ii_client        TYPE REF TO if_http_client
                  io_client        TYPE REF TO lcl_http_client
                  iv_url           TYPE string
        RETURNING VALUE(rv_scheme) TYPE string
        RAISING   zcx_abapgit_exception.

ENDCLASS.

CLASS lcl_http IMPLEMENTATION.

  METHOD get_agent.

* bitbucket require agent prefix = "git/"
    rv_agent = 'git/abapGit-' && zif_abapgit_definitions=>gc_abap_version.

  ENDMETHOD.

  METHOD create_by_url.

    DATA: lv_uri      TYPE string,
          lv_scheme   TYPE string,
          li_client   TYPE REF TO if_http_client,
          lo_settings TYPE REF TO lcl_settings,
          lv_text     TYPE string.


    lo_settings = lcl_app=>settings( )->read( ).

    cl_http_client=>create_by_url(
      EXPORTING
        url           = lcl_url=>host( iv_url )
        ssl_id        = 'ANONYM'
        proxy_host    = lo_settings->get_proxy_url( )
        proxy_service = lo_settings->get_proxy_port( )
      IMPORTING
        client        = li_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active = 2
        internal_error = 3
        OTHERS = 4 ).
    IF sy-subrc <> 0.
      CASE sy-subrc.
        WHEN 1.
          " make sure:
          " a) SSL is setup properly in STRUST
          lv_text = 'HTTPS ARGUMENT_NOT_FOUND | STRUST/SSL Setup correct?'.
        WHEN OTHERS.
          lv_text = 'While creating HTTP Client'.           "#EC NOTEXT

      ENDCASE.
      zcx_abapgit_exception=>raise( lv_text ).
    ENDIF.

    IF lo_settings->get_proxy_authentication( ) = abap_true.
      lcl_proxy_auth=>run( li_client ).
    ENDIF.

    CREATE OBJECT ro_client
      EXPORTING
        ii_client = li_client.

    IF is_local_system( iv_url ) = abap_true.
      li_client->send_sap_logon_ticket( ).
    ENDIF.

    li_client->request->set_cdata( '' ).
    li_client->request->set_header_field(
        name  = '~request_method'
        value = 'GET' ).
    li_client->request->set_header_field(
        name  = 'user-agent'
        value = get_agent( ) ).                             "#EC NOTEXT
    lv_uri = lcl_url=>path_name( iv_url ) &&
             '/info/refs?service=git-' &&
             iv_service &&
             '-pack'.
    li_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_uri ).

    " Disable internal auth dialog (due to its unclarity)
    li_client->propertytype_logon_popup = if_http_client=>co_disabled.

    lcl_login_manager=>load( iv_uri    = iv_url
                             ii_client = li_client ).

    ro_client->send_receive( ).
    IF check_auth_requested( li_client ) = abap_true.
      lv_scheme = acquire_login_details( ii_client = li_client
                                         io_client = ro_client
                                         iv_url    = iv_url ).
      ro_client->send_receive( ).
    ENDIF.
    ro_client->check_http_200( ).

    IF lv_scheme <> gc_scheme-digest.
      lcl_login_manager=>save( iv_uri    = iv_url
                               ii_client = li_client ).
    ENDIF.

  ENDMETHOD.

  METHOD is_local_system.

    DATA: lv_host TYPE string,
          lt_list TYPE zif_abapgit_definitions=>ty_icm_sinfo2_tt,
          li_exit TYPE REF TO lif_exit.

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

    li_exit = lcl_exit=>get_instance( ).
    li_exit->change_local_host( CHANGING ct_hosts = lt_list ).

    FIND REGEX 'https?://([^/^:]*)' IN iv_url
      SUBMATCHES lv_host.

    READ TABLE lt_list WITH KEY hostname = lv_host TRANSPORTING NO FIELDS.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.

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
          lv_user         TYPE string,
          lv_pass         TYPE string,
          lo_digest       TYPE REF TO lcl_http_digest.


    lv_default_user = lcl_app=>user( )->get_repo_login( iv_url ).
    lv_user         = lv_default_user.

    lcl_password_dialog=>popup(
      EXPORTING
        iv_repo_url     = iv_url
      CHANGING
        cv_user         = lv_user
        cv_pass         = lv_pass ).

    IF lv_user IS INITIAL.
      zcx_abapgit_exception=>raise( 'HTTP 401, unauthorized' ).
    ENDIF.

    IF lv_user <> lv_default_user.
      lcl_app=>user( )->set_repo_login( iv_url   = iv_url
                                        iv_login = lv_user ).
    ENDIF.

    " Offer two factor authentication if it is available and required
    lcl_2fa_auth_registry=>use_2fa_if_required(
      EXPORTING
        iv_url      = iv_url
      CHANGING
        cv_username = lv_user
        cv_password = lv_pass ).

    rv_scheme = ii_client->response->get_header_field( 'www-authenticate' ).
    FIND REGEX '^(\w+)' IN rv_scheme SUBMATCHES rv_scheme.

    CASE rv_scheme.
      WHEN gc_scheme-digest.
* https://en.wikipedia.org/wiki/Digest_access_authentication
* e.g. used by https://www.gerritcodereview.com/
        CREATE OBJECT lo_digest
          EXPORTING
            ii_client   = ii_client
            iv_username = lv_user
            iv_password = lv_pass.
        lo_digest->run( ii_client ).
        io_client->set_digest( lo_digest ).
      WHEN OTHERS.
* https://en.wikipedia.org/wiki/Basic_access_authentication
        ii_client->authenticate(
          username = lv_user
          password = lv_pass ).
    ENDCASE.

  ENDMETHOD.  "acquire_login_details

ENDCLASS.
