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

CLASS lcx_2fa_no_cached_token DEFINITION INHERITING FROM lcx_2fa_error FINAL.
  PROTECTED SECTION.
    METHODS:
      get_default_text REDEFINITION.
ENDCLASS.

CLASS lcx_2fa_no_cached_token IMPLEMENTATION.
  METHOD get_default_text.
    rv_text = 'Cached two factor access token requested but not available.' ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.

CLASS lcx_2fa_cache_deletion_failed DEFINITION INHERITING FROM lcx_2fa_error FINAL.
  PROTECTED SECTION.
    METHODS:
      get_default_text REDEFINITION.
ENDCLASS.

CLASS lcx_2fa_cache_deletion_failed IMPLEMENTATION.
  METHOD get_default_text.
    rv_text = 'Cache deletion failed.' ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.

"! Defines a two factor authentication authenticator
"! <p>
"! Authenticators support one or multiple services and are able to generate access tokens using the
"! service's API using the users username, password and two factor authentication token
"! (app/sms/tokengenerator). With these access tokens the user can be authenticated to the service's
"! implementation of the git http api, just like the "normal" password would. The authenticator can
"! also store and retrieve the access token it generated.
"! </p>
"! <p>
"! <em>LCL_2FA_AUTHENTICATOR_REGISTRY</em> can be used to find a suitable implementation for a given
"! repository.
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
                           lcx_2fa_token_gen_failed,
    "! Check if this authenticator instance supports the give repository url
    "! @parameter iv_url | Repository url
    "! @parameter rv_supported | Is supported
    supports_url IMPORTING iv_url              TYPE string
                 RETURNING VALUE(rv_supported) TYPE abap_bool,
    "! Get a unique identifier for the service that hosts the repository
    "! @parameter iv_url | Repository url
    "! @parameter rv_id | Service id
    get_service_id_from_url IMPORTING iv_url       TYPE string
                            RETURNING VALUE(rv_id) TYPE string,
    "! Check if there is a cached access token (for the current user)
    "! @parameter iv_url | Repository url
    "! @parameter rv_available | Token is cached
    is_cached_access_token_avail IMPORTING iv_url              TYPE string
                                 RETURNING VALUE(rv_available) TYPE abap_bool,
    "! Get a cached access token
    "! <p>
    "! Username and password are also parameters to decrypt the token if needed. They must no
    "! necessarily be provided if the used authenticator does not use encryption.
    "! </p>
    "! @parameter iv_url | Repository url
    "! @parameter iv_username | Username
    "! @parameter iv_password | Password
    "! @parameter rv_token | Access token
    "! @raising lcx_2fa_no_cached_token | There is no cached token
    get_cached_access_token IMPORTING iv_url          TYPE string
                                      iv_username     TYPE string OPTIONAL
                                      iv_password     TYPE string OPTIONAL
                            RETURNING VALUE(rv_token) TYPE string
                            RAISING   lcx_2fa_no_cached_token,
    "! Delete a cached token
    "! @parameter iv_url | Repository url
    "! @raising lcx_2fa_cache_deletion_failed | Deletion failed
    delete_cached_access_token IMPORTING iv_url TYPE string
                               RAISING   lcx_2fa_cache_deletion_failed.
ENDINTERFACE.

"! Default <em>LIF_2FA-AUTHENTICATOR</em> implememtation
"! <p>
"! This uses the user settings to store cached access tokens and encrypts / decrypts them as needed.
"! </p>
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
      is_cached_access_token_avail FOR lif_2fa_authenticator~is_cached_access_token_avail,
      get_cached_access_token FOR lif_2fa_authenticator~get_cached_access_token,
      delete_cached_token FOR lif_2fa_authenticator~delete_cached_access_token.
    METHODS:
      "! @parameter iv_supported_url_regex | Regular expression to check if a repository url is
      "!                                     supported, used for default implementation of
      "!                                     <em>SUPPORTS_URL</em>
      constructor IMPORTING iv_supported_url_regex TYPE clike.
  PROTECTED SECTION.
    METHODS:
      "! Subclass implementation of <em>LIF_2FA_AUTHENTICATOR=&gtAUTHENTICATE</em>
      "! <p>
      "! The caller will take care of caching the token.
      "! </p>
      "! @parameter iv_url | Repository url
      "! @parameter iv_username | Username
      "! @parameter iv_password | Password
      "! @parameter iv_2fa_token | Two factor token
      "! @parameter rv_access_token | Generated access token
      "! @raising lcx_2fa_auth_failed | Authentication failed
      "! @raising lcx_2fa_token_gen_failed | Token generation failed
      authenticate_internal ABSTRACT IMPORTING iv_url                 TYPE string
                                               iv_username            TYPE string
                                               iv_password            TYPE string
                                               iv_2fa_token           TYPE string
                                     RETURNING VALUE(rv_access_token) TYPE string
                                     RAISING   lcx_2fa_auth_failed
                                               lcx_2fa_token_gen_failed,
      "! Helper method to raise class based exception after traditional exception was raised
      "! <p>
      "! <em>sy-msg...</em> must be set right before calling!
      "! </p>
      raise_internal_error_from_sy FINAL RAISING lcx_2fa_auth_failed.
  PRIVATE SECTION.
    DATA:
      mo_url_regex TYPE REF TO cl_abap_regex.
ENDCLASS.

CLASS lcl_2fa_authenticator_base IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT mo_url_regex
      EXPORTING
        pattern     = iv_supported_url_regex
        ignore_case = abap_true.
  ENDMETHOD.

  METHOD authenticate.
    DATA: lv_encrypted_token TYPE string.

    rv_access_token = authenticate_internal( iv_url       = iv_url
                                             iv_username  = iv_username
                                             iv_password  = iv_password
                                             iv_2fa_token = iv_2fa_token ).

    " Store the access token, by default in the user settings

    " 1. Encrypt it
*    lv_encrypted_token = cl_encryption_helper=>encrypt_symmetric( iv_text = rv_access_token
*                                                                  iv_key  = iv_password ).
    " TODO: Find something like the above for symmetric encryption
    lv_encrypted_token = rv_access_token.

    " 2. Store it
    TRY.
        lcl_app=>user( )->set_2fa_access_token( iv_service_id = get_service_id_from_url( iv_url )
                                                iv_username   = iv_username
                                                iv_token      = lv_encrypted_token ).
      CATCH lcx_exception ##NO_HANDLER.
        " Not the biggest of deals if caching the token fails
    ENDTRY.
  ENDMETHOD.

  METHOD supports_url.
    rv_supported = mo_url_regex->create_matcher( text = iv_url )->match( ).
  ENDMETHOD.

  METHOD get_service_id_from_url.
    rv_id = 'UNKNOWN SERVICE'. " Please overwrite in subclasses
  ENDMETHOD.

  METHOD is_cached_access_token_avail.
    DATA: lv_service_id TYPE string.

    lv_service_id = get_service_id_from_url( iv_url ).

    " Default storage location is user settings
    TRY.
        rv_available = boolc( lcl_app=>user( )->get_2fa_access_token( lv_service_id )
                              IS NOT INITIAL ).
      CATCH lcx_exception.
        rv_available = abap_false.
    ENDTRY.
  ENDMETHOD.

  METHOD get_cached_access_token.
    DATA: lv_access_token_encrypted TYPE string,
          lx_error                  TYPE REF TO cx_root.

    TRY.
        lv_access_token_encrypted
          = lcl_app=>user( )->get_2fa_access_token( get_service_id_from_url( iv_url ) ).
      CATCH lcx_exception INTO lx_error.
        RAISE EXCEPTION TYPE lcx_2fa_no_cached_token
          EXPORTING
            ix_previous   = lx_error
            iv_error_text = lx_error->get_text( ).
    ENDTRY.

    IF lv_access_token_encrypted IS INITIAL.
      RAISE EXCEPTION TYPE lcx_2fa_no_cached_token.
    ENDIF.

    " TODO: Decryption
*    rv_token = cl_encryption_helper=>decrypt_symmetric( iv_encrypted = rv_access_token
*                                                        iv_key       = iv_password ).
    rv_token = lv_access_token_encrypted.
  ENDMETHOD.

  METHOD delete_cached_token.
    DATA: lx_ex TYPE REF TO cx_root.

    TRY.
        " Default storage location is user settings
        lcl_app=>user( )->delete_2fa_config( get_service_id_from_url( iv_url ) ).
      CATCH lcx_exception INTO lx_ex.
        RAISE EXCEPTION TYPE lcx_2fa_cache_deletion_failed
          EXPORTING
            ix_previous   = lx_ex
            iv_error_text = |Cache deletion failed: { lx_ex->get_text( ) }|.
    ENDTRY.
  ENDMETHOD.

  METHOD raise_internal_error_from_sy.
    DATA: lv_error_msg TYPE string.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO lv_error_msg.
    RAISE EXCEPTION TYPE lcx_2fa_auth_failed
      EXPORTING
        iv_error_text = |Internal error: { lv_error_msg }| ##NO_TEXT.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_2fa_github_authenticator DEFINITION
  INHERITING FROM lcl_2fa_authenticator_base
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor,
      get_service_id_from_url REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      authenticate_internal REDEFINITION.
  PRIVATE SECTION.
    METHODS:
      set_access_token_request IMPORTING ii_request   TYPE REF TO if_http_request
                                         iv_repo_name TYPE string,
      get_token_from_response IMPORTING ii_response     TYPE REF TO if_http_response
                              RETURNING VALUE(rv_token) TYPE string,
      parse_repo_from_url IMPORTING iv_url              TYPE string
                          RETURNING VALUE(rv_repo_name) TYPE string.
ENDCLASS.

CLASS lcl_2fa_github_authenticator IMPLEMENTATION.
  METHOD constructor.
    super->constructor( 'https?:\/\/(www\.)?github.com.*$' ).
  ENDMETHOD.

  METHOD authenticate_internal.
    CONSTANTS: lc_github_api_url              TYPE string VALUE `https://api.github.com/`,
               lc_otp_header_name             TYPE string VALUE `X-Github-OTP`,
               lc_restendpoint_authorizations TYPE string VALUE `/authorizations`.
    DATA: li_http_client           TYPE REF TO if_http_client,
          lv_http_code             TYPE i,
          lv_http_code_description TYPE string,
          lv_binary_response       TYPE xstring,
          BEGIN OF ls_success_response,
            token TYPE string,
          END OF ls_success_response.

    " 1. Try to login to GitHub API with username, password and 2fa token
    cl_http_client=>create_by_url(
      EXPORTING
        url                = lc_github_api_url
      IMPORTING
        client             = li_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    IF sy-subrc <> 0.
      raise_internal_error_from_sy( ).
    ENDIF.

    " https://developer.github.com/v3/auth/#working-with-two-factor-authentication
    li_http_client->propertytype_accept_cookie = if_http_client=>co_enabled.
    li_http_client->request->set_header_field( name = lc_otp_header_name value = iv_2fa_token ).
    li_http_client->authenticate( username = iv_username password = iv_password ).
    li_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

    li_http_client->send( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      raise_internal_error_from_sy( ).
    ENDIF.

    li_http_client->receive( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      raise_internal_error_from_sy( ).
    ENDIF.

    " Check if authentication has succeeded
    li_http_client->response->get_status(
      IMPORTING
        code   = lv_http_code
        reason = lv_http_code_description
    ).
    IF lv_http_code <> 200.
      RAISE EXCEPTION TYPE lcx_2fa_auth_failed
        EXPORTING
          iv_error_text = |Authentication failed: { lv_http_code_description }|.
    ENDIF.


    " 2. Create an access token which can be used instead of a password
    " https://developer.github.com/v3/oauth_authorizations/#create-a-new-authorization

    set_access_token_request( ii_request   = li_http_client->request
                              iv_repo_name = parse_repo_from_url( iv_url ) ).
    li_http_client->request->set_header_field( name  = if_http_header_fields_sap=>request_uri
                                               value = lc_restendpoint_authorizations ).
    li_http_client->request->set_method( if_http_request=>co_request_method_post ).

    li_http_client->send( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      raise_internal_error_from_sy( ).
    ENDIF.

    li_http_client->receive( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      raise_internal_error_from_sy( ).
    ENDIF.

    li_http_client->response->get_status(
      IMPORTING
        code   = lv_http_code
        reason = lv_http_code_description
    ).
    IF lv_http_code <> 201.
      RAISE EXCEPTION TYPE lcx_2fa_token_gen_failed
        EXPORTING
          iv_error_text = |Token generation failed: { lv_http_code } { lv_http_code_description }|.
    ENDIF.

    rv_access_token = get_token_from_response( li_http_client->response ).
    IF rv_access_token IS INITIAL.
      RAISE EXCEPTION TYPE lcx_2fa_token_gen_failed.
    ENDIF.
  ENDMETHOD.

  METHOD set_access_token_request.
    CONSTANTS: BEGIN OF lc_create_access_token_request,
                 scopes TYPE string VALUE 'repo',
                 note   TYPE string VALUE 'abapGit',
               END OF lc_create_access_token_request.
    DATA: lo_json_writer      TYPE REF TO cl_sxml_string_writer,
          lt_scopes           TYPE stringtab,
          lt_rest_parvalues   TYPE abap_trans_srcbind_tab,
          ls_rest_line        LIKE LINE OF lt_rest_parvalues,
          lt_result_parvalues TYPE abap_trans_resbind_tab,
          ls_result_line      LIKE LINE OF lt_result_parvalues,
          lr_data_ref         TYPE REF TO data,
          lv_note             TYPE string,
          lv_fingerprint      TYPE string.

    lo_json_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    APPEND lc_create_access_token_request-scopes TO lt_scopes.

    GET REFERENCE OF lc_create_access_token_request-scopes INTO lr_data_ref.
    ls_rest_line-name = 'scopes'.
    ls_rest_line-value = lr_data_ref.
    APPEND ls_rest_line TO lt_rest_parvalues.

    GET REFERENCE OF lc_create_access_token_request-note INTO lr_data_ref.
    ls_rest_line-name = 'note'.
    ls_rest_line-value = lr_data_ref.
    APPEND ls_rest_line TO lt_rest_parvalues.

    " The fingerprint must be unique, otherwise only one token can be generated, unless the user
    " deletes it in Github's settings. This is problematic if he deletes it in abapGit but keeps it
    " on GitHub.
    lv_fingerprint = |abapGit-{ sy-sysid }-{ sy-uname }-{ sy-datum }-{ sy-uzeit }|.
    GET REFERENCE OF lv_fingerprint INTO lr_data_ref.
    ls_rest_line-name = 'fingerprint'.
    ls_rest_line-value = lr_data_ref.
    APPEND ls_rest_line TO lt_rest_parvalues.

    " Dynamic source table is used because otherwise identifiers will always be written in uppercase
    " which is not supported by the GitHub's API.
    CALL TRANSFORMATION id SOURCE (lt_rest_parvalues)
                           RESULT XML lo_json_writer.

    ii_request->set_data( lo_json_writer->get_output( ) ).
  ENDMETHOD.

  METHOD get_token_from_response.
    CONSTANTS: lc_token_field_name TYPE string VALUE 'token'.
    DATA: lt_result_parvalues TYPE abap_trans_resbind_tab,
          ls_result_line      LIKE LINE OF lt_result_parvalues,
          lr_data_ref         TYPE REF TO data,
          lv_binary_response  TYPE xstring.

    GET REFERENCE OF rv_token INTO lr_data_ref.
    ls_result_line-name = lc_token_field_name.
    ls_result_line-value = lr_data_ref.
    APPEND ls_result_line TO lt_result_parvalues.

    lv_binary_response = ii_response->get_data( ).

    CALL TRANSFORMATION id SOURCE XML lv_binary_response
                           RESULT (lt_result_parvalues).
  ENDMETHOD.

  METHOD parse_repo_from_url.
    DATA: lo_regex   TYPE REF TO cl_abap_regex,
          lo_matcher TYPE REF TO cl_abap_matcher.

    CREATE OBJECT lo_regex
      EXPORTING
        pattern = 'https?:\/\/(www\.)?github.com\/(.*)$'.

    lo_matcher = lo_regex->create_matcher( text = iv_url ).
    IF lo_matcher->match( ) = abap_true.
      rv_repo_name = lo_matcher->get_submatch( 1 ).
    ELSE.
      rv_repo_name = '???' ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get_service_id_from_url.
    rv_id = 'github'.
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
                       RETURNING VALUE(rv_supported) TYPE abap_bool.
    CLASS-DATA:
      "! All authenticators managed by the registry
      gt_registered_authenticators TYPE HASHED TABLE OF REF TO lif_2fa_authenticator
                                        WITH UNIQUE KEY table_line READ-ONLY.
  PROTECTED SECTION.
  PRIVATE SECTION.
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
ENDCLASS.