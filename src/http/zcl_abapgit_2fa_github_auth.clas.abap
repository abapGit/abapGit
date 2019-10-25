CLASS zcl_abapgit_2fa_github_auth DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_2fa_auth_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_override TYPE string OPTIONAL .

    METHODS zif_abapgit_2fa_authenticator~authenticate
        REDEFINITION .
    METHODS zif_abapgit_2fa_authenticator~delete_access_tokens
        REDEFINITION .
    METHODS zif_abapgit_2fa_authenticator~end
        REDEFINITION .
    METHODS zif_abapgit_2fa_authenticator~is_2fa_required
        REDEFINITION .
  PROTECTED SECTION.

    DATA mv_github_api_url TYPE string VALUE `https://api.github.com/` ##NO_TEXT.
  PRIVATE SECTION.

    CONSTANTS c_otp_header_name TYPE string VALUE `X-Github-OTP` ##NO_TEXT.
    CONSTANTS c_restendpoint_authorizations TYPE string VALUE `/authorizations` ##NO_TEXT.
    DATA mi_authenticated_session TYPE REF TO if_http_client .

    CLASS-METHODS set_new_token_request
      IMPORTING
        !ii_request TYPE REF TO if_http_request .
    CLASS-METHODS get_token_from_response
      IMPORTING
        !ii_response    TYPE REF TO if_http_response
      RETURNING
        VALUE(rv_token) TYPE string .
    CLASS-METHODS set_list_token_request
      IMPORTING
        !ii_request TYPE REF TO if_http_request .
    CLASS-METHODS get_tobedel_tokens_from_resp
      IMPORTING
        !ii_response  TYPE REF TO if_http_response
      RETURNING
        VALUE(rt_ids) TYPE string_table .
    CLASS-METHODS set_del_token_request
      IMPORTING
        !ii_request  TYPE REF TO if_http_request
        !iv_token_id TYPE string .
    METHODS get_authenticated_client
      IMPORTING
        !iv_username     TYPE string
        !iv_password     TYPE string
        !iv_2fa_token    TYPE string
      RETURNING
        VALUE(ri_client) TYPE REF TO if_http_client
      RAISING
        zcx_abapgit_2fa_auth_failed
        zcx_abapgit_2fa_comm_error .
ENDCLASS.



CLASS ZCL_ABAPGIT_2FA_GITHUB_AUTH IMPLEMENTATION.


  METHOD constructor.

    DATA: lv_match TYPE string.

    IF iv_override IS SUPPLIED.
      lv_match = iv_override.
    ELSE.
      lv_match = '^https?://(www\.)?github.com.*$'.
    ENDIF.

    super->constructor( lv_match ).

  ENDMETHOD.


  METHOD get_authenticated_client.
    DATA: lv_http_code             TYPE i,
          lv_http_code_description TYPE string.

    " If there is a cached client return it instead
    IF is_session_running( ) = abap_true AND mi_authenticated_session IS BOUND.
      ri_client = mi_authenticated_session.
      RETURN.
    ENDIF.

    " Try to login to GitHub API with username, password and 2fa token
    ri_client = get_http_client_for_url( mv_github_api_url ).

    " https://developer.github.com/v3/auth/#working-with-two-factor-authentication
    ri_client->propertytype_accept_cookie = if_http_client=>co_enabled.
    ri_client->request->set_header_field( name = c_otp_header_name value = iv_2fa_token ).
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
      RAISE EXCEPTION TYPE zcx_abapgit_2fa_auth_failed
        EXPORTING
          mv_text = |Authentication failed: { lv_http_code_description }|.
    ENDIF.

    " Cache the authenticated http session / client to avoid unnecessary additional authentication
    IF is_session_running( ) = abap_true.
      mi_authenticated_session = ri_client.
    ENDIF.
  ENDMETHOD.


  METHOD get_tobedel_tokens_from_resp.
    CONSTANTS: lc_search_regex TYPE string
      VALUE `\{"id": ?(\d+)[^\{]*"app":\{[^\{^\}]*\}[^\{]*"fingerprint": ?"abapGit2FA"[^\{]*\}`.

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


  METHOD set_del_token_request.
    DATA: lv_url TYPE string.

    lv_url = |{ c_restendpoint_authorizations }/{ iv_token_id }|.

    ii_request->set_header_field( name  = if_http_header_fields_sap=>request_uri
                                  value = lv_url ).
    " Other methods than POST and GET do not have constants unfortunately
    " ii_request->set_method( if_http_request=>co_request_method_delete ).
    ii_request->set_method( 'DELETE' ).
  ENDMETHOD.


  METHOD set_list_token_request.
    ii_request->set_header_field( name  = if_http_header_fields_sap=>request_uri
                                  value = c_restendpoint_authorizations ).
    ii_request->set_method( if_http_request=>co_request_method_get ).
  ENDMETHOD.


  METHOD set_new_token_request.
    DATA: lv_json_string TYPE string.

    lv_json_string = `{"scopes":["repo"],"note":"Generated by abapGit","fingerprint":"abapGit2FA"}`.

    ii_request->set_data( cl_abap_codepage=>convert_to( lv_json_string ) ).
    ii_request->set_header_field( name  = if_http_header_fields_sap=>request_uri
                                  value = c_restendpoint_authorizations ).
    ii_request->set_method( if_http_request=>co_request_method_post ).
  ENDMETHOD.


  METHOD zif_abapgit_2fa_authenticator~authenticate.
    DATA: li_http_client           TYPE REF TO if_http_client,
          lv_http_code             TYPE i,
          lv_http_code_description TYPE string.

    " 1. Try to login to GitHub API
    li_http_client = get_authenticated_client( iv_username  = iv_username
                                               iv_password  = iv_password
                                               iv_2fa_token = iv_2fa_token ).

    " 2. Create an access token which can be used instead of a password
    " https://developer.github.com/v3/oauth_authorizations/#create-a-new-authorization

    set_new_token_request( li_http_client->request ).

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
      RAISE EXCEPTION TYPE zcx_abapgit_2fa_gen_failed
        EXPORTING
          mv_text = |Token generation failed: { lv_http_code } { lv_http_code_description }|.
    ENDIF.

    rv_access_token = get_token_from_response( li_http_client->response ).
    IF rv_access_token IS INITIAL.
      RAISE EXCEPTION TYPE zcx_abapgit_2fa_gen_failed
        EXPORTING
          mv_text = 'Token generation failed: parser error' ##NO_TEXT.
    ENDIF.

    " GitHub might need some time until the new token is ready to use, give it a second
    CALL FUNCTION 'RZL_SLEEP'.
  ENDMETHOD.


  METHOD zif_abapgit_2fa_authenticator~delete_access_tokens.

    DATA: li_http_client           TYPE REF TO if_http_client,
          lv_http_code             TYPE i,
          lv_http_code_description TYPE string,
          lt_tobedeleted_tokens    TYPE string_table.

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
      RAISE EXCEPTION TYPE zcx_abapgit_2fa_del_failed
        EXPORTING
          mv_text = |Could not fetch current 2FA authorizations: | &&
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
        RAISE EXCEPTION TYPE zcx_abapgit_2fa_del_failed
          EXPORTING
            mv_text = |Could not delete token '{ <lv_id> }': | &&
                      |{ lv_http_code } { lv_http_code_description }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_abapgit_2fa_authenticator~end.
    super->end( ).
    FREE mi_authenticated_session.
  ENDMETHOD.


  METHOD zif_abapgit_2fa_authenticator~is_2fa_required.

    DATA: li_client TYPE REF TO if_http_client.

    li_client = get_http_client_for_url( mv_github_api_url ).

    li_client->propertytype_logon_popup = if_http_client=>co_disabled.

    " The request needs to use something other than GET and it needs to be send to an endpoint
    " to trigger a SMS.
    li_client->request->set_header_field( name  = if_http_header_fields_sap=>request_uri
                                          value = c_restendpoint_authorizations ).
    li_client->request->set_method( if_http_request=>co_request_method_post ).

    " Try to authenticate, if 2FA is required there will be a specific response header
    li_client->authenticate( username = iv_username password = iv_password ).

    li_client->send( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      raise_comm_error_from_sy( ).
    ENDIF.

    li_client->receive( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
* if the code fails here with a SSL error, make sure STRUST is setup to
* work with https://api.github.com
      raise_comm_error_from_sy( ).
    ENDIF.

    " The response will either be UNAUTHORIZED or MALFORMED which is both fine.

    IF li_client->response->get_header_field( c_otp_header_name ) CP 'required*'.
      rv_required = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
