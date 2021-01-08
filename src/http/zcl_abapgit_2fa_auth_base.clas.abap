"! Default {@link ZIF_ABAPGIT_2FA_AUTHENTICATOR} implementation
CLASS zcl_abapgit_2fa_auth_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_2fa_authenticator.
    ALIASES:
      authenticate FOR zif_abapgit_2fa_authenticator~authenticate,
      supports_url FOR zif_abapgit_2fa_authenticator~supports_url,
      is_2fa_required FOR zif_abapgit_2fa_authenticator~is_2fa_required,
      delete_access_tokens FOR zif_abapgit_2fa_authenticator~delete_access_tokens,
      begin FOR zif_abapgit_2fa_authenticator~begin,
      end FOR zif_abapgit_2fa_authenticator~end.
    METHODS:
      "! @parameter iv_supported_url_regex | Regular expression to check if a repository url is
      "!                                     supported, used for default implementation of
      "!                                     {@link .METH:supports_url}
      constructor IMPORTING iv_supported_url_regex TYPE clike.
  PROTECTED SECTION.
    CLASS-METHODS:
      "! Helper method to raise class based exception after traditional exception was raised
      "! <p>
      "! <em>sy-msg...</em> must be set right before calling!
      "! </p>
      raise_comm_error_from_sy RAISING zcx_abapgit_2fa_comm_error.
    METHODS:
      "! @parameter rv_running | Internal session is currently active
      is_session_running RETURNING VALUE(rv_running) TYPE abap_bool,
      "! Returns HTTP client configured with proxy (where required) for the given URL
      get_http_client_for_url
        IMPORTING iv_url           TYPE string
        RETURNING VALUE(ri_client) TYPE REF TO if_http_client
        RAISING   zcx_abapgit_2fa_comm_error.
  PRIVATE SECTION.
    DATA:
      mo_url_regex       TYPE REF TO cl_abap_regex,
      mv_session_running TYPE abap_bool.
ENDCLASS.



CLASS ZCL_ABAPGIT_2FA_AUTH_BASE IMPLEMENTATION.


  METHOD authenticate.
    RAISE EXCEPTION TYPE zcx_abapgit_2fa_auth_failed. " Needs to be overwritten in subclasses
  ENDMETHOD.


  METHOD begin.
    IF mv_session_running = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_2fa_illegal_state.
    ENDIF.

    mv_session_running = abap_true.
  ENDMETHOD.


  METHOD constructor.
    CREATE OBJECT mo_url_regex
      EXPORTING
        pattern     = iv_supported_url_regex
        ignore_case = abap_true.
  ENDMETHOD.


  METHOD delete_access_tokens.
    RAISE EXCEPTION TYPE zcx_abapgit_2fa_del_failed. " Needs to be overwritten in subclasses
  ENDMETHOD.


  METHOD end.
    IF mv_session_running = abap_false.
      RAISE EXCEPTION TYPE zcx_abapgit_2fa_illegal_state.
    ENDIF.

    mv_session_running = abap_false.
  ENDMETHOD.


  METHOD get_http_client_for_url.
    DATA: lo_proxy       TYPE REF TO zcl_abapgit_proxy_config,
          lx_abapgit_exc TYPE REF TO zcx_abapgit_exception,
          lv_error_text  TYPE string.

    CREATE OBJECT lo_proxy.
    cl_http_client=>create_by_url(
      EXPORTING
        url                = iv_url
        ssl_id             = zcl_abapgit_exit=>get_instance( )->get_ssl_id( )
        proxy_host         = lo_proxy->get_proxy_url( iv_url )
        proxy_service      = lo_proxy->get_proxy_port( iv_url )
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

    IF lo_proxy->get_proxy_authentication( iv_url ) = abap_true.
      TRY.
          zcl_abapgit_proxy_auth=>run( ri_client ).
        CATCH zcx_abapgit_exception INTO lx_abapgit_exc.
          lv_error_text = lx_abapgit_exc->get_text( ).
          IF lv_error_text IS INITIAL.
            lv_error_text = `Proxy authentication error`.
          ENDIF.
          RAISE EXCEPTION TYPE zcx_abapgit_2fa_comm_error
            EXPORTING
              mv_text  = lv_error_text
              previous = lx_abapgit_exc.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD is_2fa_required.
    rv_required = abap_false.
  ENDMETHOD.


  METHOD is_session_running.
    rv_running = mv_session_running.
  ENDMETHOD.


  METHOD raise_comm_error_from_sy.
    DATA: lv_error_msg TYPE string.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO lv_error_msg.
    RAISE EXCEPTION TYPE zcx_abapgit_2fa_comm_error
      EXPORTING
        mv_text = |Communication error: { lv_error_msg }|.
  ENDMETHOD.


  METHOD supports_url.
    rv_supported = mo_url_regex->create_matcher( text = iv_url )->match( ).
  ENDMETHOD.
ENDCLASS.
