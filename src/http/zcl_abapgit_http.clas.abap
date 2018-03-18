CLASS zcl_abapgit_http DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF c_scheme,
                 digest TYPE string VALUE 'Digest',
               END OF c_scheme.

    CLASS-METHODS:
      get_agent
        RETURNING VALUE(rv_agent) TYPE string,
      create_by_url
        IMPORTING iv_url           TYPE string
                  iv_service       TYPE string
        RETURNING VALUE(ro_client) TYPE REF TO zcl_abapgit_http_client
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
                  io_client        TYPE REF TO zcl_abapgit_http_client
                  iv_url           TYPE string
        RETURNING VALUE(rv_scheme) TYPE string
        RAISING   zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_HTTP IMPLEMENTATION.


  METHOD acquire_login_details.

    DATA: lv_default_user TYPE string,
          lv_user         TYPE string,
          lv_pass         TYPE string,
          lo_digest       TYPE REF TO zcl_abapgit_http_digest.


    lv_default_user = zcl_abapgit_persistence_user=>get_instance( )->get_repo_login( iv_url ).
    lv_user         = lv_default_user.

    zcl_abapgit_password_dialog=>popup(
      EXPORTING
        iv_repo_url     = iv_url
      CHANGING
        cv_user         = lv_user
        cv_pass         = lv_pass ).

    IF lv_user IS INITIAL.
      zcx_abapgit_exception=>raise( 'HTTP 401, unauthorized' ).
    ENDIF.

    IF lv_user <> lv_default_user.
      zcl_abapgit_persistence_user=>get_instance( )->set_repo_login(
        iv_url   = iv_url
        iv_login = lv_user ).
    ENDIF.

    " Offer two factor authentication if it is available and required
    zcl_abapgit_2fa_auth_registry=>use_2fa_if_required(
      EXPORTING
        iv_url      = iv_url
      CHANGING
        cv_username = lv_user
        cv_password = lv_pass ).

    rv_scheme = ii_client->response->get_header_field( 'www-authenticate' ).
    FIND REGEX '^(\w+)' IN rv_scheme SUBMATCHES rv_scheme.

    CASE rv_scheme.
      WHEN c_scheme-digest.
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


  METHOD check_auth_requested.

    DATA: lv_code TYPE i.

    ii_client->response->get_status(
      IMPORTING
        code   = lv_code ).
    IF lv_code = 401.
      rv_auth_requested = abap_true.
    ENDIF.

  ENDMETHOD.  "check_auth_requested


  METHOD create_by_url.

    DATA: lv_uri                 TYPE string,
          lv_scheme              TYPE string,
          li_client              TYPE REF TO if_http_client,
          lo_proxy_configuration TYPE REF TO zcl_abapgit_proxy_config,
          lv_text                TYPE string.


    CREATE OBJECT lo_proxy_configuration.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = zcl_abapgit_url=>host( iv_url )
        ssl_id             = 'ANONYM'
        proxy_host         = lo_proxy_configuration->get_proxy_url( iv_url )
        proxy_service      = lo_proxy_configuration->get_proxy_port( iv_url )
      IMPORTING
        client             = li_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
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

    IF lo_proxy_configuration->get_proxy_authentication( iv_url ) = abap_true.
      zcl_abapgit_proxy_auth=>run( li_client ).
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
    lv_uri = zcl_abapgit_url=>path_name( iv_url ) &&
             '/info/refs?service=git-' &&
             iv_service &&
             '-pack'.
    li_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_uri ).

    " Disable internal auth dialog (due to its unclarity)
    li_client->propertytype_logon_popup = if_http_client=>co_disabled.

    zcl_abapgit_login_manager=>load( iv_uri    = iv_url
                                     ii_client = li_client ).

    zcl_abapgit_exit=>get_instance( )->http_client( li_client ).

    ro_client->send_receive( ).
    IF check_auth_requested( li_client ) = abap_true.
      lv_scheme = acquire_login_details( ii_client = li_client
                                         io_client = ro_client
                                         iv_url    = iv_url ).
      ro_client->send_receive( ).
    ENDIF.
    ro_client->check_http_200( ).

    IF lv_scheme <> c_scheme-digest.
      zcl_abapgit_login_manager=>save( iv_uri    = iv_url
                                       ii_client = li_client ).
    ENDIF.

  ENDMETHOD.


  METHOD get_agent.

* bitbucket require agent prefix = "git/"
    rv_agent = 'git/abapGit-' && zif_abapgit_definitions=>gc_abap_version.

  ENDMETHOD.


  METHOD is_local_system.

    DATA: lv_host TYPE string,
          lt_list TYPE zif_abapgit_exit=>ty_icm_sinfo2_tt,
          li_exit TYPE REF TO zif_abapgit_exit.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF lt_list.


    CALL FUNCTION 'ICM_GET_INFO2'
      TABLES
        servlist    = lt_list
      EXCEPTIONS
        icm_error   = 1
        icm_timeout = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_list ASSIGNING <ls_list>.
    <ls_list>-hostname = 'localhost'.

    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->change_local_host( CHANGING ct_hosts = lt_list ).

    FIND REGEX 'https?://([^/^:]*)' IN iv_url
      SUBMATCHES lv_host.

    READ TABLE lt_list WITH KEY hostname = lv_host TRANSPORTING NO FIELDS.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
ENDCLASS.
