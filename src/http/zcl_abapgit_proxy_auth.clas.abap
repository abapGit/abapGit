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
ENDCLASS.



CLASS zcl_abapgit_proxy_auth IMPLEMENTATION.


  METHOD run.

    DATA lv_auth TYPE string.

    IF gv_username IS INITIAL OR gv_password IS INITIAL.
      lv_auth = zcl_abapgit_login_manager=>get( zcl_abapgit_login_manager=>gc_proxy ).

      IF lv_auth IS INITIAL.
        " If proxy authorization is not set, redirect to "Login Page"
        zcl_abapgit_login_manager=>login( zcl_abapgit_login_manager=>gc_proxy ).
      ELSE.
        SPLIT lv_auth AT ':' INTO gv_username gv_password.
      ENDIF.
    ENDIF.

    ii_client->authenticate(
      proxy_authentication = abap_true
      username             = gv_username
      password             = gv_password ).

  ENDMETHOD.
ENDCLASS.
