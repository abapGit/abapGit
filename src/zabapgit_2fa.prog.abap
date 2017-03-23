*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_2FA
*&---------------------------------------------------------------------*

"! Exception base class for two factor authentication related errors
CLASS lcx_2fa_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING is_textid     LIKE textid OPTIONAL
                            ix_previous   LIKE previous OPTIONAL
                            iv_error_text TYPE csequence OPTIONAL,
      get_text REDEFINITION.
    DATA:
      mv_text TYPE string READ-ONLY.
  PROTECTED SECTION.
    METHODS:
      get_default_text RETURNING VALUE(rv_text) TYPE string.
ENDCLASS.

CLASS lcx_2fa_error IMPLEMENTATION.
  METHOD constructor.
    super->constructor( textid = is_textid previous = ix_previous ).
    mv_text = iv_error_text.
  ENDMETHOD.

  METHOD get_text.
    IF mv_text IS NOT INITIAL.
      result = mv_text.
    ELSEIF get_default_text( ) IS NOT INITIAL.
      result = get_default_text( ).
    ELSE.
      result = super->get_text( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_default_text.
    rv_text = 'Error in two factor authentication.' ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.

CLASS lcx_2fa_auth_failed DEFINITION INHERITING FROM lcx_2fa_error FINAL.
  PROTECTED SECTION.
    METHODS:
      get_default_text REDEFINITION.
ENDCLASS.

CLASS lcx_2fa_auth_failed IMPLEMENTATION.
  METHOD get_default_text.
    rv_text = 'Authentication failed using 2FA.' ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.

CLASS lcx_2fa_token_gen_failed DEFINITION INHERITING FROM lcx_2fa_error FINAL.
  PROTECTED SECTION.
    METHODS:
      get_default_text REDEFINITION.
ENDCLASS.

CLASS lcx_2fa_token_gen_failed IMPLEMENTATION.
  METHOD get_default_text.
    rv_text = 'Two factor access token generation failed.' ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.

CLASS lcx_2fa_unsupported DEFINITION INHERITING FROM lcx_2fa_error FINAL.
  PROTECTED SECTION.
    METHODS:
      get_default_text REDEFINITION.
ENDCLASS.

CLASS lcx_2fa_unsupported IMPLEMENTATION.
  METHOD get_default_text.
    rv_text = 'The service is not supported for two factor authentication.' ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.

CLASS lcx_2fa_token_del_failed DEFINITION INHERITING FROM lcx_2fa_error FINAL.
  PROTECTED SECTION.
    METHODS:
      get_default_text REDEFINITION.
ENDCLASS.

CLASS lcx_2fa_token_del_failed IMPLEMENTATION.
  METHOD get_default_text.
    rv_text = 'Deleting previous access tokens failed.' ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.

CLASS lcx_2fa_communication_error DEFINITION INHERITING FROM lcx_2fa_error FINAL.
  PROTECTED SECTION.
    METHODS:
      get_default_text REDEFINITION.
ENDCLASS.

CLASS lcx_2fa_communication_error IMPLEMENTATION.
  METHOD get_default_text.
    rv_text = 'Communication error.' ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.

CLASS lcx_2fa_illegal_state DEFINITION INHERITING FROM lcx_2fa_error FINAL.
  PROTECTED SECTION.
    METHODS:
      get_default_text REDEFINITION.
ENDCLASS.

CLASS lcx_2fa_illegal_state IMPLEMENTATION.
  METHOD get_default_text.
    rv_text = 'Illegal state.' ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.

"! Defines a two factor authentication authenticator
"! <p>
"! Authenticators support one or multiple services and are able to generate access tokens using the
"! service's API using the users username, password and two factor authentication token
"! (app/sms/tokengenerator). With these access tokens the user can be authenticated to the service's
"! implementation of the git http api, just like the "normal" password would.
"! </p>
"! <p>
"! <em>LCL_2FA_AUTHENTICATOR_REGISTRY</em> can be used to find a suitable implementation for a given
"! repository.
"! </p>
"! <p>
"! Using the <em>begin</em> and <em>end</em> methods an internal session can be started and
"! completed in which internal state necessary for multiple methods will be cached. This can be
"! used to avoid having multiple http sessions between <em>authenticate</em> and
"! <em>delete_access_tokens</em>.
"! </p>
INTERFACE lif_2fa_authenticator.
  METHODS:
    "! Generate an access token
    "! @parameter iv_url | Repository url
    "! @parameter iv_username | Username
    "! @parameter iv_password | Password
    "! @parameter iv_2fa_token | Two factor token
    "! @parameter rv_access_token | Generated access token
    "! @raising lcx_2fa_auth_failed | Authentication failed
    "! @raising lcx_2fa_token_gen_failed | Token generation failed
    authenticate IMPORTING iv_url                 TYPE string
                           iv_username            TYPE string
                           iv_password            TYPE string
                           iv_2fa_token           TYPE string
                 RETURNING VALUE(rv_access_token) TYPE string
                 RAISING   lcx_2fa_auth_failed
                           lcx_2fa_token_gen_failed
                           lcx_2fa_communication_error,
    "! Check if this authenticator instance supports the give repository url
    "! @parameter iv_url | Repository url
    "! @parameter rv_supported | Is supported
    supports_url IMPORTING iv_url              TYPE string
                 RETURNING VALUE(rv_supported) TYPE abap_bool,
    "! Get a unique identifier for the service that hosts the repository
    "! @parameter iv_url | Repository url
    "! @parameter rv_id | Service id
    "! @raising lcx_2fa_unsupported | Url is not supported
    get_service_id_from_url IMPORTING iv_url       TYPE string
                            RETURNING VALUE(rv_id) TYPE string
                            RAISING   lcx_2fa_unsupported,
    "! Check if two factor authentication is required
    "! @parameter iv_url | Repository url
    "! @parameter iv_username | Username
    "! @parameter iv_password | Password
    "! @parameter rv_required | 2FA is required
    is_2fa_required IMPORTING iv_url             TYPE string
                              iv_username        TYPE string
                              iv_password        TYPE string
                    RETURNING VALUE(rv_required) TYPE abap_bool
                    RAISING   lcx_2fa_communication_error,
    "! Delete all previously created access tokens for abapGit
    "! @parameter iv_url | Repository url
    "! @parameter iv_username | Username
    "! @parameter iv_password | Password
    "! @parameter iv_2fa_token | Two factor token
    "! @raising lcx_2fa_token_del_failed | Token deletion failed
    "! @raising lcx_2fa_auth_failed | Authentication failed
    delete_access_tokens IMPORTING iv_url       TYPE string
                                   iv_username  TYPE string
                                   iv_password  TYPE string
                                   iv_2fa_token TYPE string
                         RAISING   lcx_2fa_token_del_failed
                                   lcx_2fa_communication_error
                                   lcx_2fa_auth_failed,
    "! Begin an authenticator session that uses internal caching for authorizations
    "! @raising lcx_2fa_illegal_state | Session already started
    begin RAISING lcx_2fa_illegal_state,
    "! End an authenticator session and clear internal caches
    "! @raising lcx_2fa_illegal_state | Session not running
    end RAISING lcx_2fa_illegal_state.
ENDINTERFACE.

"! Default <em>LIF_2FA-AUTHENTICATOR</em> implememtation
CLASS lcl_2fa_authenticator_base DEFINITION
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      lif_2fa_authenticator.
    ALIASES:
      authenticate FOR lif_2fa_authenticator~authenticate,
      supports_url FOR lif_2fa_authenticator~supports_url,
      get_service_id_from_url FOR lif_2fa_authenticator~get_service_id_from_url,
      is_2fa_required FOR lif_2fa_authenticator~is_2fa_required,
      delete_access_tokens FOR lif_2fa_authenticator~delete_access_tokens,
      begin FOR lif_2fa_authenticator~begin,
      end FOR lif_2fa_authenticator~end.
    METHODS:
      "! @parameter iv_supported_url_regex | Regular expression to check if a repository url is
      "!                                     supported, used for default implementation of
      "!                                     <em>SUPPORTS_URL</em>
      constructor IMPORTING iv_supported_url_regex TYPE clike.
  PROTECTED SECTION.
    CLASS-METHODS:
      "! Helper method to raise class based exception after traditional exception was raised
      "! <p>
      "! <em>sy-msg...</em> must be set right before calling!
      "! </p>
      raise_comm_error_from_sy RAISING lcx_2fa_communication_error.
    METHODS:
      "! @parameter rv_running | Internal session is currently active
      is_session_running RETURNING VALUE(rv_running) TYPE abap_bool.
  PRIVATE SECTION.
    DATA:
      mo_url_regex       TYPE REF TO cl_abap_regex,
      mv_session_running TYPE abap_bool.
ENDCLASS.

CLASS lcl_2fa_authenticator_base IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT mo_url_regex
      EXPORTING
        pattern     = iv_supported_url_regex
        ignore_case = abap_true.
  ENDMETHOD.

  METHOD authenticate.
    RAISE EXCEPTION TYPE lcx_2fa_auth_failed. " Needs to be overwritten in subclasses
  ENDMETHOD.

  METHOD supports_url.
    rv_supported = mo_url_regex->create_matcher( text = iv_url )->match( ).
  ENDMETHOD.

  METHOD get_service_id_from_url.
    rv_id = 'UNKNOWN SERVICE'. " Please overwrite in subclasses
  ENDMETHOD.

  METHOD is_2fa_required.
    rv_required = abap_false.
  ENDMETHOD.

  METHOD delete_access_tokens.
    RAISE EXCEPTION TYPE lcx_2fa_token_del_failed. " Needs to be overwritten in subclasses
  ENDMETHOD.

  METHOD raise_comm_error_from_sy.
    DATA: lv_error_msg TYPE string.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO lv_error_msg.
    RAISE EXCEPTION TYPE lcx_2fa_communication_error
      EXPORTING
        iv_error_text = |Communication error: { lv_error_msg }| ##NO_TEXT.
  ENDMETHOD.

  METHOD begin.
    IF mv_session_running = abap_true.
      RAISE EXCEPTION TYPE lcx_2fa_illegal_state.
    ENDIF.

    mv_session_running = abap_true.
  ENDMETHOD.

  METHOD end.
    IF mv_session_running = abap_false.
      RAISE EXCEPTION TYPE lcx_2fa_illegal_state.
    ENDIF.

    mv_session_running = abap_false.
  ENDMETHOD.

  METHOD is_session_running.
    rv_running = mv_session_running.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_2fa_github_authenticator DEFINITION
  INHERITING FROM lcl_2fa_authenticator_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor,
      get_service_id_from_url REDEFINITION,
      authenticate REDEFINITION,
      is_2fa_required REDEFINITION,
      delete_access_tokens REDEFINITION,
      end REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      gc_github_api_url              TYPE string VALUE `https://api.github.com/`,
      gc_otp_header_name             TYPE string VALUE `X-Github-OTP`,
      gc_restendpoint_authorizations TYPE string VALUE `/authorizations`.
    CLASS-METHODS:
      set_new_token_request IMPORTING ii_request   TYPE REF TO if_http_request,
      get_token_from_response IMPORTING ii_response     TYPE REF TO if_http_response
                              RETURNING VALUE(rv_token) TYPE string,
      parse_repo_from_url IMPORTING iv_url              TYPE string
                          RETURNING VALUE(rv_repo_name) TYPE string,
      set_list_token_request IMPORTING ii_request TYPE REF TO if_http_request,
      get_tobedel_tokens_from_resp IMPORTING ii_response   TYPE REF TO if_http_response
                                   RETURNING VALUE(rt_ids) TYPE stringtab,
      set_del_token_request IMPORTING ii_request  TYPE REF TO if_http_request
                                      iv_token_id TYPE string.
    METHODS:
      get_authenticated_client IMPORTING iv_username      TYPE string
                                         iv_password      TYPE string
                                         iv_2fa_token     TYPE string
                               RETURNING VALUE(ri_client) TYPE REF TO if_http_client
                               RAISING   lcx_2fa_auth_failed
                                         lcx_2fa_communication_error.
    DATA:
      mi_authenticated_session TYPE REF TO if_http_client.
ENDCLASS.

CLASS lcl_2fa_github_authenticator IMPLEMENTATION.
  METHOD constructor.
    super->constructor( '^https?://(www\.)?github.com.*$' ).
  ENDMETHOD.

  METHOD authenticate.
    DATA: li_http_client           TYPE REF TO if_http_client,
          lv_http_code             TYPE i,
          lv_http_code_description TYPE string.

    " 1. Try to login to GitHub API
    li_http_client = get_authenticated_client( iv_username  = iv_username
                                               iv_password  = iv_password
                                               iv_2fa_token = iv_2fa_token ).

    " 2. Create an access token which can be used instead of a password
    " https://developer.github.com/v3/oauth_authorizations/#create-a-new-authorization

    set_new_token_request( ii_request = li_http_client->request ).

    li_http_client->send( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      raise_comm_error_from_sy( ).
    ENDIF.

    li_http_client->receive( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      raise_comm_error_from_sy( ).
    ENDIF.

    li_http_client->response->get_status(
      IMPORTING
        code   = lv_http_code
        reason = lv_http_code_description ).
    IF lv_http_code <> 201.
      RAISE EXCEPTION TYPE lcx_2fa_token_gen_failed
        EXPORTING
          iv_error_text = |Token generation failed: { lv_http_code } { lv_http_code_description }|.
    ENDIF.

    rv_access_token = get_token_from_response( li_http_client->response ).
    IF rv_access_token IS INITIAL.
      RAISE EXCEPTION TYPE lcx_2fa_token_gen_failed
        EXPORTING
          iv_error_text = 'Token generation failed: parser error' ##NO_TEXT.
    ENDIF.

    " GitHub might need some time until the new token is ready to use, give it a second
    CALL FUNCTION 'RZL_SLEEP'.
  ENDMETHOD.

  METHOD set_new_token_request.
    DATA: lv_json_string TYPE string.

    lv_json_string = `{"scopes":["repo"],"note":"Generated by abapGit","fingerprint":"abapGit2FA"}`.

    ii_request->set_data( cl_abap_codepage=>convert_to( lv_json_string ) ).
    ii_request->set_header_field( name  = if_http_header_fields_sap=>request_uri
                                  value = gc_restendpoint_authorizations ).
    ii_request->set_method( if_http_request=>co_request_method_post ).
  ENDMETHOD.

  METHOD set_list_token_request.
    ii_request->set_header_field( name  = if_http_header_fields_sap=>request_uri
                                  value = gc_restendpoint_authorizations ).
    ii_request->set_method( if_http_request=>co_request_method_get ).
  ENDMETHOD.

  METHOD set_del_token_request.
    DATA: lv_url TYPE string.

    lv_url = |{ gc_restendpoint_authorizations }/{ iv_token_id }|.

    ii_request->set_header_field( name  = if_http_header_fields_sap=>request_uri
                                  value = lv_url ).
    " Other methods than POST and GET do not have constants unfortunately
    " ii_request->set_method( if_http_request=>co_request_method_delete ).
    ii_request->set_method( 'DELETE' ).
  ENDMETHOD.

  METHOD get_token_from_response.
    CONSTANTS: lc_search_regex TYPE string VALUE `.*"token":"([^"]*).*$`.
    DATA: lv_response TYPE string,
          lo_regex    TYPE REF TO cl_abap_regex,
          lo_matcher  TYPE REF TO cl_abap_matcher.

    lv_response = cl_abap_codepage=>convert_from( ii_response->get_data( ) ).

    CREATE OBJECT lo_regex
      EXPORTING
        pattern = lc_search_regex.

    lo_matcher = lo_regex->create_matcher( text = lv_response ).
    IF lo_matcher->match( ) = abap_true.
      rv_token = lo_matcher->get_submatch( 1 ).
    ENDIF.
  ENDMETHOD.

  METHOD get_tobedel_tokens_from_resp.
    CONSTANTS: lc_search_regex TYPE string
               VALUE `\{"id": ?(\d+)[^\{]*"app":\{[^\{^\}]*\}[^\{]*"fingerprint": ?` &
               `"abapGit2FA"[^\{]*\}`.
    DATA: lv_response TYPE string,
          lo_regex    TYPE REF TO cl_abap_regex,
          lo_matcher  TYPE REF TO cl_abap_matcher.

    lv_response = cl_abap_codepage=>convert_from( ii_response->get_data( ) ).

    CREATE OBJECT lo_regex
      EXPORTING
        pattern = lc_search_regex.

    lo_matcher = lo_regex->create_matcher( text = lv_response ).
    WHILE lo_matcher->find_next( ) = abap_true.
      APPEND lo_matcher->get_submatch( 1 ) TO rt_ids.
    ENDWHILE.
  ENDMETHOD.

  METHOD parse_repo_from_url.
* method not used?
    ASSERT 0 = 1.
*    CONSTANTS: lc_search_regex TYPE string VALUE 'https?:\/\/(www\.)?github.com\/(.*)$'.
*    DATA: lo_regex   TYPE REF TO cl_abap_regex,
*          lo_matcher TYPE REF TO cl_abap_matcher.
*
*    CREATE OBJECT lo_regex
*      EXPORTING
*        pattern = lc_search_regex.
*
*    lo_matcher = lo_regex->create_matcher( text = iv_url ).
*    IF lo_matcher->match( ) = abap_true.
*      rv_repo_name = lo_matcher->get_submatch( 1 ).
*    ELSE.
*      rv_repo_name = '???' ##NO_TEXT.
*    ENDIF.
  ENDMETHOD.

  METHOD get_service_id_from_url.
    rv_id = 'github'.
  ENDMETHOD.

  METHOD is_2fa_required.
    DATA: li_client   TYPE REF TO if_http_client,
          lo_settings TYPE REF TO lcl_settings.

    lo_settings = lcl_app=>settings( )->read( ).

    cl_http_client=>create_by_url(
      EXPORTING
        url                = gc_github_api_url
        ssl_id             = 'ANONYM'
        proxy_host         = lo_settings->get_proxy_url( )
        proxy_service      = lo_settings->get_proxy_port( )
      IMPORTING
        client             = li_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    IF sy-subrc <> 0.
      raise_comm_error_from_sy( ).
    ENDIF.

    li_client->propertytype_logon_popup = if_http_client=>co_disabled.

    " The request needs to use something other than GET and it needs to be send to an endpoint
    " to trigger a SMS.
    li_client->request->set_header_field( name  = if_http_header_fields_sap=>request_uri
                                          value = gc_restendpoint_authorizations ).
    li_client->request->set_method( if_http_request=>co_request_method_post ).

    " Try to authenticate, if 2FA is required there will be a specific response header
    li_client->authenticate( username = iv_username password = iv_password ).

    li_client->send( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      raise_comm_error_from_sy( ).
    ENDIF.

    li_client->receive( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      raise_comm_error_from_sy( ).
    ENDIF.

    " The response will either be UNAUTHORIZED or MALFORMED which is both fine.

    IF li_client->response->get_header_field( gc_otp_header_name ) CP 'required*'.
      rv_required = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD delete_access_tokens.
    DATA: li_http_client           TYPE REF TO if_http_client,
          lv_http_code             TYPE i,
          lv_http_code_description TYPE string,
          lt_tobedeleted_tokens    TYPE stringtab.
    FIELD-SYMBOLS: <lv_id> TYPE string.

    li_http_client = get_authenticated_client( iv_username  = iv_username
                                               iv_password  = iv_password
                                               iv_2fa_token = iv_2fa_token ).

    set_list_token_request( li_http_client->request ).
    li_http_client->send( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      raise_comm_error_from_sy( ).
    ENDIF.

    li_http_client->receive( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      raise_comm_error_from_sy( ).
    ENDIF.

    li_http_client->response->get_status(
      IMPORTING
        code   = lv_http_code
        reason = lv_http_code_description ).
    IF lv_http_code <> 200.
      RAISE EXCEPTION TYPE lcx_2fa_token_del_failed
        EXPORTING
          iv_error_text = |Could not fetch current 2FA authorizations: | &&
                          |{ lv_http_code } { lv_http_code_description }|.
    ENDIF.

    lt_tobedeleted_tokens = get_tobedel_tokens_from_resp( li_http_client->response ).
    LOOP AT lt_tobedeleted_tokens ASSIGNING <lv_id> WHERE table_line IS NOT INITIAL.
      set_del_token_request( ii_request  = li_http_client->request
                             iv_token_id = <lv_id> ).
      li_http_client->send( EXCEPTIONS OTHERS = 1 ).
      IF sy-subrc <> 0.
        raise_comm_error_from_sy( ).
      ENDIF.

      li_http_client->receive( EXCEPTIONS OTHERS = 1 ).
      IF sy-subrc <> 0.
        raise_comm_error_from_sy( ).
      ENDIF.

      li_http_client->response->get_status(
        IMPORTING
          code   = lv_http_code
          reason = lv_http_code_description ).
      IF lv_http_code <> 204.
        RAISE EXCEPTION TYPE lcx_2fa_token_del_failed
          EXPORTING
            iv_error_text = |Could not delete token '{ <lv_id> }': | &&
                            |{ lv_http_code } { lv_http_code_description }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_authenticated_client.
    DATA: lv_http_code             TYPE i,
          lv_http_code_description TYPE string,
          lo_settings              TYPE REF TO lcl_settings.

    " If there is a cached client return it instead
    IF is_session_running( ) = abap_true AND mi_authenticated_session IS BOUND.
      ri_client = mi_authenticated_session.
      RETURN.
    ENDIF.

    " Try to login to GitHub API with username, password and 2fa token

    lo_settings = lcl_app=>settings( )->read( ).

    cl_http_client=>create_by_url(
      EXPORTING
        url                = gc_github_api_url
        ssl_id             = 'ANONYM'
        proxy_host         = lo_settings->get_proxy_url( )
        proxy_service      = lo_settings->get_proxy_port( )
      IMPORTING
        client             = ri_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    IF sy-subrc <> 0.
      raise_comm_error_from_sy( ).
    ENDIF.

    " https://developer.github.com/v3/auth/#working-with-two-factor-authentication
    ri_client->propertytype_accept_cookie = if_http_client=>co_enabled.
    ri_client->request->set_header_field( name = gc_otp_header_name value = iv_2fa_token ).
    ri_client->authenticate( username = iv_username password = iv_password ).
    ri_client->propertytype_logon_popup = if_http_client=>co_disabled.

    ri_client->send( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      raise_comm_error_from_sy( ).
    ENDIF.

    ri_client->receive( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      raise_comm_error_from_sy( ).
    ENDIF.

    " Check if authentication has succeeded
    ri_client->response->get_status(
      IMPORTING
        code   = lv_http_code
        reason = lv_http_code_description ).
    IF lv_http_code <> 200.
      RAISE EXCEPTION TYPE lcx_2fa_auth_failed
        EXPORTING
          iv_error_text = |Authentication failed: { lv_http_code_description }|.
    ENDIF.

    " Cache the authenticated http session / client to avoid unnecessary additional authentication
    IF is_session_running( ) = abap_true.
      mi_authenticated_session = ri_client.
    ENDIF.
  ENDMETHOD.

  METHOD end.
    super->end( ).
    FREE mi_authenticated_session.
  ENDMETHOD.
ENDCLASS.

"! Static registry class to find <em>LIF_2FA_AUTHENTICATOR</em> instances
CLASS lcl_2fa_authenticator_registry DEFINITION
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS:
      class_constructor,
      "! Retrieve an authenticator instance by url
      "! @parameter iv_url | Url of the repository / service
      "! @parameter ro_authenticator | Found authenticator instance
      "! @raising lcx_2fa_unsupported | No authenticator found that supports the service
      get_authenticator_for_url IMPORTING iv_url                  TYPE string
                                RETURNING VALUE(ro_authenticator) TYPE REF TO lif_2fa_authenticator
                                RAISING   lcx_2fa_unsupported,
      "! Check if there is a two factor authenticator available for the url
      "! @parameter iv_url | Url of the repository / service
      "! @parameter rv_supported | 2FA is supported
      is_url_supported IMPORTING iv_url              TYPE string
                       RETURNING VALUE(rv_supported) TYPE abap_bool,
      "! Offer to use two factor authentication if supported and required
      "! <p>
      "! This uses GUI functionality to display a popup to request the user to enter a two factor
      "! token. Also an dummy authentication request might be used to find out if two factor
      "! authentication is required for the account.
      "! </p>
      "! @parameter iv_url | Url of the repository / service
      "! @parameter cv_username | Username
      "! @parameter cv_password | Password, will be replaced by an access token if two factor
      "!                          authentication succeeds
      "! @raising lcx_exception | Error in two factor authentication
      use_2fa_if_required IMPORTING iv_url      TYPE string
                          CHANGING  cv_username TYPE string
                                    cv_password TYPE string
                          RAISING   lcx_exception.
    CLASS-DATA:
      "! All authenticators managed by the registry
      gt_registered_authenticators TYPE HASHED TABLE OF REF TO lif_2fa_authenticator
                                        WITH UNIQUE KEY table_line READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-METHODS:
      popup_token
        RETURNING VALUE(rv_token) TYPE string
        RAISING   lcx_exception.
ENDCLASS.

CLASS lcl_2fa_authenticator_registry IMPLEMENTATION.
  METHOD class_constructor.
    DEFINE register.
      CREATE OBJECT li_authenticator TYPE &1.
      INSERT li_authenticator INTO TABLE gt_registered_authenticators.
    END-OF-DEFINITION.

    DATA: li_authenticator TYPE REF TO lif_2fa_authenticator.

    " If there are new authenticators these need to be added here manually.
    " I do not think there is an equivalent to SEO_INTERFACE_IMPLEM_GET_ALL for local classes
    " without invoking the compiler directly.
    register: lcl_2fa_github_authenticator.
  ENDMETHOD.

  METHOD get_authenticator_for_url.
    FIELD-SYMBOLS: <lo_authenticator> LIKE LINE OF gt_registered_authenticators.

    LOOP AT gt_registered_authenticators ASSIGNING <lo_authenticator>.
      IF <lo_authenticator>->supports_url( iv_url ) = abap_true.
        ro_authenticator = <lo_authenticator>.
        RETURN.
      ENDIF.
    ENDLOOP.

    RAISE EXCEPTION TYPE lcx_2fa_unsupported.
  ENDMETHOD.

  METHOD is_url_supported.
    TRY.
        get_authenticator_for_url( iv_url ).
        rv_supported = abap_true.
      CATCH lcx_2fa_unsupported ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD use_2fa_if_required.
    DATA: li_authenticator TYPE REF TO lif_2fa_authenticator,
          lv_2fa_token     TYPE string,
          lv_access_token  TYPE string,
          lx_ex            TYPE REF TO cx_root.

    IF is_url_supported( iv_url ) = abap_false.
      RETURN.
    ENDIF.

    TRY.
        li_authenticator = get_authenticator_for_url( iv_url ).
        li_authenticator->begin( ).

        " Is two factor authentication required for this account?
        IF li_authenticator->is_2fa_required( iv_url      = iv_url
                                              iv_username = cv_username
                                              iv_password = cv_password ) = abap_true.

          lv_2fa_token = popup_token( ).

          " Delete an old access token if it exists
          li_authenticator->delete_access_tokens( iv_url       = iv_url
                                                  iv_username  = cv_username
                                                  iv_password  = cv_password
                                                  iv_2fa_token = lv_2fa_token ).

          " Get a new access token
          lv_access_token = li_authenticator->authenticate( iv_url       = iv_url
                                                            iv_username  = cv_username
                                                            iv_password  = cv_password
                                                            iv_2fa_token = lv_2fa_token ).

          " Use the access token instead of the password
          cv_password = lv_access_token.
        ENDIF.

        li_authenticator->end( ).

      CATCH lcx_2fa_error INTO lx_ex.
        TRY.
            li_authenticator->end( ).
          CATCH lcx_2fa_illegal_state ##NO_HANDLER.
        ENDTRY.

        RAISE EXCEPTION TYPE lcx_exception
          EXPORTING
            iv_text     = |2FA error: { lx_ex->get_text( ) }|
            ix_previous = lx_ex.
    ENDTRY.
  ENDMETHOD.

  METHOD popup_token.

    DATA: lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname   = 'TADIR'.
    <ls_field>-fieldname = 'OBJ_NAME'.
    <ls_field>-fieldtext = 'Two factor auth. token'.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Two factor auth. token'
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2. "#EC NOTEXT
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'Error from POPUP_GET_VALUES' ).
    ENDIF.

    IF lv_returncode = 'A'.
      lcx_exception=>raise( 'Authentication cancelled' ).
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rv_token = <ls_field>-value.

  ENDMETHOD.

ENDCLASS.
