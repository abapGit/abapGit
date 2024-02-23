CLASS zcl_abapgit_http DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_scheme,
        digest TYPE string VALUE 'Digest',
      END OF c_scheme .

    CLASS-METHODS get_agent
      RETURNING
        VALUE(rv_agent) TYPE string .

    TYPES: BEGIN OF ty_key_value,
             key   TYPE string,
             value TYPE string,
           END OF ty_key_value.
    TYPES ty_headers TYPE STANDARD TABLE OF ty_key_value WITH DEFAULT KEY.

    CLASS-METHODS create_by_url
      IMPORTING
        !iv_url          TYPE string
        !iv_service      TYPE string
        it_headers       TYPE ty_headers OPTIONAL
      RETURNING
        VALUE(ro_client) TYPE REF TO zcl_abapgit_http_client
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS check_connection
      IMPORTING
        !iv_url TYPE string
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.

    CLASS-METHODS check_auth_requested
      IMPORTING
        !ii_client               TYPE REF TO if_http_client
      RETURNING
        VALUE(rv_auth_requested) TYPE abap_bool
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS is_local_system
      IMPORTING
        !iv_url        TYPE string
      RETURNING
        VALUE(rv_bool) TYPE abap_bool .
    CLASS-METHODS acquire_login_details
      IMPORTING
        !ii_client       TYPE REF TO if_http_client
        !io_client       TYPE REF TO zcl_abapgit_http_client
        !iv_url          TYPE string
      RETURNING
        VALUE(rv_scheme) TYPE string
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS get_http_client
      IMPORTING
        !iv_url          TYPE string
      RETURNING
        VALUE(ri_client) TYPE REF TO if_http_client
      RAISING
        zcx_abapgit_exception.

    CLASS-METHODS get_connection_longtext
      IMPORTING
        !iv_host           TYPE string
        !iv_ssl_id         TYPE ssfapplssl
        !iv_proxy_host     TYPE string
        !iv_proxy_service  TYPE string
      RETURNING
        VALUE(rv_longtext) TYPE string.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_abapgit_http IMPLEMENTATION.


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
      zcx_abapgit_exception=>raise( 'Unauthorized access. Check your credentials' ).
    ENDIF.

    IF lv_user <> lv_default_user.
      zcl_abapgit_persistence_user=>get_instance( )->set_repo_login(
        iv_url   = iv_url
        iv_login = lv_user ).
    ENDIF.

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

  ENDMETHOD.


  METHOD check_auth_requested.

    DATA: lv_code TYPE i.

    ii_client->response->get_status( IMPORTING code = lv_code ).
    IF lv_code = 401.
      rv_auth_requested = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD check_connection.
    " Check if a connection from this system to the git host is possible
    " This will validate the general HTTP/HTTPS/SSL configuration and certificates
    get_http_client( iv_url ).
  ENDMETHOD.


  METHOD create_by_url.

    DATA: lv_uri           TYPE string,
          lv_scheme        TYPE string,
          lv_authorization TYPE string,
          li_client        TYPE REF TO if_http_client,
          ls_header        LIKE LINE OF it_headers.

    li_client = get_http_client( iv_url ).

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
        value = get_agent( ) ).
    lv_uri = zcl_abapgit_url=>path_name( iv_url ) &&
             '/info/refs?service=git-' &&
             iv_service &&
             '-pack'.
    li_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_uri ).

    LOOP AT it_headers INTO ls_header.
      li_client->request->set_header_field(
        name  = ls_header-key
        value = ls_header-value ).
    ENDLOOP.

    " Disable internal auth dialog (due to its unclarity)
    li_client->propertytype_logon_popup = if_http_client=>co_disabled.

    lv_authorization = zcl_abapgit_login_manager=>load( iv_url ).
    IF lv_authorization IS NOT INITIAL.
      li_client->request->set_header_field(
        name  = 'authorization'
        value = lv_authorization ).
      li_client->propertytype_logon_popup = li_client->co_disabled.
    ENDIF.

    zcl_abapgit_exit=>get_instance( )->http_client(
      iv_url    = iv_url
      ii_client = li_client ).

    ro_client->send_receive( ).
    IF check_auth_requested( li_client ) = abap_true.
      lv_scheme = acquire_login_details( ii_client = li_client
                                         io_client = ro_client
                                         iv_url    = iv_url ).
      ro_client->send_receive( ).
    ENDIF.
    ro_client->check_http_200( ).

    IF lv_scheme <> c_scheme-digest.
      zcl_abapgit_login_manager=>save(
        iv_uri           = iv_url
        iv_authorization = li_client->request->get_header_field( 'authorization' ) ).
    ENDIF.

  ENDMETHOD.


  METHOD get_agent.

* bitbucket require agent prefix = "git/"
* also see https://github.com/abapGit/abapGit/issues/1432
    rv_agent = |git/2.0 (abapGit { zif_abapgit_version=>c_abap_version })|.

  ENDMETHOD.


  METHOD get_connection_longtext.

    CONSTANTS lc_docs TYPE string VALUE 'https://docs.abapgit.org/user-guide/setup/ssl-setup.html'.

    DATA lv_proxy TYPE string.

    IF iv_proxy_host IS NOT INITIAL.
      lv_proxy = | via proxy <b>{ iv_proxy_host }:{ iv_proxy_service }</b>|.
    ENDIF.

    rv_longtext = |abapGit is trying to connect to <b>{ iv_host }</b> |
      && |using SSL certificates under <b>{ iv_ssl_id }</b>{ lv_proxy }. |
      && |Check system parameters (transaction |
      && zcl_abapgit_html=>create( )->a(
        iv_txt   = 'RZ10'
        iv_act   = |{ zif_abapgit_definitions=>c_action-jump_transaction }?transaction=RZ10|
        iv_class = 'no-pad' )
      && |), SSL setup (transaction |
      && zcl_abapgit_html=>create( )->a(
        iv_txt   = 'STRUST'
        iv_act   = |{ zif_abapgit_definitions=>c_action-jump_transaction }?transaction=STRUST|
        iv_class = 'no-pad' )
      && |), Internet connection monitor (transaction |
      && zcl_abapgit_html=>create( )->a(
        iv_txt   = 'SMICM'
        iv_act   = |{ zif_abapgit_definitions=>c_action-jump_transaction }?transaction=SMICM|
        iv_class = 'no-pad' )
      && |)|.

    IF lv_proxy IS NOT INITIAL.
      rv_longtext = rv_longtext
        && |, and proxy configuration (|
        && zcl_abapgit_html=>create( )->a(
          iv_txt   = 'global settings'
          iv_act   = |{ zif_abapgit_definitions=>c_action-go_settings }|
          iv_class = 'no-pad' )
        && |)|.
    ENDIF.

    rv_longtext = rv_longtext
      && |. It's recommended to get your SAP Basis and network teams involved. |
      && |For more information and troubleshooting, see the |
      && zcl_abapgit_html=>create( )->a(
        iv_txt   = 'abapGit documentation'
        iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ lc_docs }|
        iv_class = 'no-pad' )
      && |.|.

  ENDMETHOD.


  METHOD get_http_client.

    DATA:
      lv_error               TYPE string,
      lv_longtext            TYPE string,
      lv_host                TYPE string,
      lv_ssl_id              TYPE ssfapplssl,
      lv_proxy_host          TYPE string,
      lv_proxy_service       TYPE string,
      lo_proxy_configuration TYPE REF TO zcl_abapgit_proxy_config.

    CREATE OBJECT lo_proxy_configuration.

    ri_client = zcl_abapgit_exit=>get_instance( )->create_http_client( iv_url ).

    IF ri_client IS INITIAL.

      lv_host          = zcl_abapgit_url=>host( iv_url ).
      lv_ssl_id        = zcl_abapgit_exit=>get_instance( )->get_ssl_id( ).
      lv_proxy_host    = lo_proxy_configuration->get_proxy_url( iv_url ).
      lv_proxy_service = lo_proxy_configuration->get_proxy_port( iv_url ).

      lv_longtext = get_connection_longtext(
        iv_host          = lv_host
        iv_ssl_id        = lv_ssl_id
        iv_proxy_host    = lv_proxy_host
        iv_proxy_service = lv_proxy_service ).

      cl_http_client=>create_by_url(
        EXPORTING
          url                = lv_host
          ssl_id             = lv_ssl_id
          proxy_host         = lv_proxy_host
          proxy_service      = lv_proxy_service
        IMPORTING
          client             = ri_client
        EXCEPTIONS
          argument_not_found = 1
          plugin_not_active  = 2
          internal_error     = 3
          OTHERS             = 4 ).
      IF sy-subrc <> 0.
        CASE sy-subrc.
          WHEN 1.
            lv_error = 'ARGUMENT_NOT_FOUND'.
          WHEN 2.
            lv_error = 'PLUGIN_NOT_ACTIVE'.
          WHEN 3.
            lv_error = 'INTERNAL_ERROR'.
          WHEN OTHERS.
            lv_error = |OTHER_ERROR_{ sy-subrc }|.
        ENDCASE.
        zcx_abapgit_exception=>raise(
          iv_text     = |Error { lv_error } creating HTTP connection. Check the configuration|
          iv_longtext = lv_longtext ).
      ENDIF.

    ENDIF.

    IF lo_proxy_configuration->get_proxy_authentication( iv_url ) = abap_true.
      zcl_abapgit_proxy_auth=>run( ri_client ).
    ENDIF.

  ENDMETHOD.


  METHOD is_local_system.

    DATA: lv_host TYPE string,
          lt_list TYPE zif_abapgit_definitions=>ty_string_tt,
          li_exit TYPE REF TO zif_abapgit_exit.


    cl_http_server=>get_location( IMPORTING host = lv_host ).
    APPEND lv_host TO lt_list.

    APPEND 'localhost' TO lt_list.

    li_exit = zcl_abapgit_exit=>get_instance( ).
    li_exit->change_local_host( CHANGING ct_hosts = lt_list ).

    FIND REGEX 'https?://([^/^:]*)' IN iv_url SUBMATCHES lv_host.

    READ TABLE lt_list WITH KEY table_line = lv_host TRANSPORTING NO FIELDS.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
ENDCLASS.
