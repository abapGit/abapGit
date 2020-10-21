CLASS zcl_abapgit_http DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_agent
      RETURNING
        VALUE(rv_agent) TYPE string .
    CLASS-METHODS create_by_url
      IMPORTING
        !iv_url          TYPE string
        !iv_service      TYPE string
      RETURNING
        VALUE(ro_client) TYPE REF TO zcl_abapgit_http_client
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS reset_login
      IMPORTING
        !iv_url TYPE string .
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
    CLASS-METHODS trigger_login
      IMPORTING
        !iv_url    TYPE string
        !iv_digest TYPE string
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_login,
        url   TYPE string,
        count TYPE i,
      END OF ty_login .

    CLASS-DATA:
      gt_logins TYPE HASHED TABLE OF ty_login WITH UNIQUE KEY url .
    CONSTANTS c_max_logins TYPE i VALUE 3.

ENDCLASS.



CLASS zcl_abapgit_http IMPLEMENTATION.


  METHOD check_auth_requested.

    DATA: lv_code TYPE i.

    ii_client->response->get_status( IMPORTING code = lv_code ).
    IF lv_code = 401.
      rv_auth_requested = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD create_by_url.

    DATA: lv_uri                 TYPE string,
          lv_digest              TYPE string,
          li_client              TYPE REF TO if_http_client,
          lo_proxy_configuration TYPE REF TO zcl_abapgit_proxy_config,
          lv_text                TYPE string.


    CREATE OBJECT lo_proxy_configuration.

    li_client = zcl_abapgit_exit=>get_instance( )->create_http_client( iv_url ).

    IF li_client IS NOT BOUND.

      cl_http_client=>create_by_url(
        EXPORTING
          url                = zcl_abapgit_url=>host( iv_url )
          ssl_id             = zcl_abapgit_exit=>get_instance( )->get_ssl_id( )
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
            lv_text = 'While creating HTTP Client'.

        ENDCASE.
        zcx_abapgit_exception=>raise( lv_text ).
      ENDIF.

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
        value = get_agent( ) ).
    lv_uri = zcl_abapgit_url=>path_name( iv_url ) &&
             '/info/refs?service=git-' &&
             iv_service &&
             '-pack'.
    li_client->request->set_header_field(
        name  = '~request_uri'
        value = lv_uri ).

    " Disable internal auth dialog (due to its unclarity)
    li_client->propertytype_logon_popup = if_http_client=>co_disabled.

    zcl_abapgit_login_manager=>load(
      iv_uri    = iv_url
      ii_client = li_client ).

    zcl_abapgit_exit=>get_instance( )->http_client(
      iv_url    = iv_url
      ii_client = li_client ).

    ro_client->send_receive( ).

    IF check_auth_requested( li_client ) = abap_true.
      lv_digest = li_client->response->get_header_field( 'WWW-Authenticate' ).

      " If login count is below maximum, the following will raise an exception
      " that redirects to the login page
      trigger_login(
        iv_url    = iv_url
        iv_digest = lv_digest ).

      " If we get to here, the login was not successful. We reset the count
      " and continue to process the http return code as usual.
      reset_login( iv_url ).
    ENDIF.

    ro_client->check_http_200( ).

  ENDMETHOD.


  METHOD get_agent.

* bitbucket require agent prefix = "git/"
* also see https://github.com/abapGit/abapGit/issues/1432
    rv_agent = |git/2.0 (abapGit { zif_abapgit_version=>gc_abap_version })|.

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


  METHOD reset_login.
    " Reset login count
    DELETE gt_logins WHERE url = iv_url.
  ENDMETHOD.


  METHOD trigger_login.

    DATA ls_login TYPE ty_login.

    FIELD-SYMBOLS: <ls_login> TYPE ty_login.

    " Keep track of how many login attempts have been made
    READ TABLE gt_logins ASSIGNING <ls_login> WITH TABLE KEY url = iv_url.
    IF sy-subrc = 0.
      <ls_login>-count = <ls_login>-count + 1.
    ELSE.
      ls_login-url = iv_url.
      ls_login-count = 1.
      INSERT ls_login INTO TABLE gt_logins ASSIGNING <ls_login>.
    ENDIF.

    " Until maximum number of logins has been reached, we trigger the login page
    " by raising an exception with the URL parameter. The exception is caught
    " in ZCL_ABAPGIT_GUI->HANDLE_ACTION which redirects to the login page.
    IF <ls_login>-count BETWEEN 1 AND c_max_logins.
      zcx_abapgit_exception=>raise_login_error(
        iv_url    = iv_url
        iv_digest = iv_digest
        iv_count  = <ls_login>-count ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
