CLASS zcl_abapgit_proxy_auth DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

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



CLASS ZCL_ABAPGIT_PROXY_AUTH IMPLEMENTATION.


  METHOD enter.

    zcl_abapgit_password_dialog=>popup(
      EXPORTING
        iv_repo_url = 'Proxy Authentication'
      CHANGING
        cv_user     = gv_username
        cv_pass     = gv_password ).

    IF gv_username IS INITIAL OR gv_password IS INITIAL.
      zcx_abapgit_exception=>raise( 'Proxy auth failed' ).
    ENDIF.

  ENDMETHOD.


  METHOD run.

    IF gv_username IS INITIAL OR gv_password IS INITIAL.
      enter( ).
    ENDIF.

    ii_client->authenticate(
      proxy_authentication = abap_true
      username             = gv_username
      password             = gv_password ).

  ENDMETHOD.
ENDCLASS.
