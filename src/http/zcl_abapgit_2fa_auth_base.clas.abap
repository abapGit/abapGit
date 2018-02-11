"! Default <em>LIF_2FA-AUTHENTICATOR</em> implememtation
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
      get_service_id_from_url FOR zif_abapgit_2fa_authenticator~get_service_id_from_url,
      is_2fa_required FOR zif_abapgit_2fa_authenticator~is_2fa_required,
      delete_access_tokens FOR zif_abapgit_2fa_authenticator~delete_access_tokens,
      begin FOR zif_abapgit_2fa_authenticator~begin,
      end FOR zif_abapgit_2fa_authenticator~end.
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
      raise_comm_error_from_sy RAISING zcx_abapgit_2fa_comm_error.
    METHODS:
      "! @parameter rv_running | Internal session is currently active
      is_session_running RETURNING VALUE(rv_running) TYPE abap_bool.
  PRIVATE SECTION.
    DATA:
      mo_url_regex       TYPE REF TO cl_abap_regex,
      mv_session_running TYPE abap_bool.
ENDCLASS.



CLASS ZCL_ABAPGIT_2FA_AUTH_BASE IMPLEMENTATION.


  METHOD constructor.
    CREATE OBJECT mo_url_regex
      EXPORTING
        pattern     = iv_supported_url_regex
        ignore_case = abap_true.
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
        mv_text = |Communication error: { lv_error_msg }| ##NO_TEXT.
  ENDMETHOD.


  METHOD authenticate.
    RAISE EXCEPTION TYPE zcx_abapgit_2fa_auth_failed. " Needs to be overwritten in subclasses
  ENDMETHOD.


  METHOD begin.
    IF mv_session_running = abap_true.
      RAISE EXCEPTION TYPE zcx_abapgit_2fa_illegal_state.
    ENDIF.

    mv_session_running = abap_true.
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


  METHOD get_service_id_from_url.
    rv_id = 'UNKNOWN SERVICE'. " Please overwrite in subclasses
  ENDMETHOD.


  METHOD is_2fa_required.
    rv_required = abap_false.
  ENDMETHOD.


  METHOD supports_url.
    rv_supported = mo_url_regex->create_matcher( text = iv_url )->match( ).
  ENDMETHOD.
ENDCLASS.
