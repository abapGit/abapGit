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
INTERFACE zif_abapgit_2fa_authenticator PUBLIC.

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
                 RAISING   zcx_abapgit_2fa_auth_failed
                           zcx_abapgit_2fa_gen_failed
                           zcx_abapgit_2fa_comm_error,
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
                            RAISING   zcx_abapgit_2fa_unsupported,
    "! Check if two factor authentication is required
    "! @parameter iv_url | Repository url
    "! @parameter iv_username | Username
    "! @parameter iv_password | Password
    "! @parameter rv_required | 2FA is required
    is_2fa_required IMPORTING iv_url             TYPE string
                              iv_username        TYPE string
                              iv_password        TYPE string
                    RETURNING VALUE(rv_required) TYPE abap_bool
                    RAISING   zcx_abapgit_2fa_comm_error,
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
                         RAISING   zcx_abapgit_2fa_del_failed
                                   zcx_abapgit_2fa_comm_error
                                   zcx_abapgit_2fa_auth_failed,
    "! Begin an authenticator session that uses internal caching for authorizations
    "! @raising lcx_2fa_illegal_state | Session already started
    begin RAISING zcx_abapgit_2fa_illegal_state,
    "! End an authenticator session and clear internal caches
    "! @raising lcx_2fa_illegal_state | Session not running
    end RAISING zcx_abapgit_2fa_illegal_state.
ENDINTERFACE.
