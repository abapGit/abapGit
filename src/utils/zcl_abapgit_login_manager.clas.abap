CLASS zcl_abapgit_login_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS gc_proxy TYPE string VALUE 'http://proxy/' ##NO_TEXT. " Identifier for proxy

    CLASS-METHODS login
      IMPORTING
        !iv_url    TYPE string
        !iv_digest TYPE string OPTIONAL
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS load
      IMPORTING
        !iv_uri                 TYPE string
        !ii_client              TYPE REF TO if_http_client OPTIONAL
      RETURNING
        VALUE(rv_authorization) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS save
      IMPORTING
        !iv_uri    TYPE string
        !ii_client TYPE REF TO if_http_client
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS clear .
    CLASS-METHODS set
      IMPORTING
        !iv_uri        TYPE string
        !iv_username   TYPE string
        !iv_password   TYPE string
        !iv_digest     TYPE string OPTIONAL
      RETURNING
        VALUE(rv_auth) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get
      IMPORTING
        !iv_uri        TYPE string
      RETURNING
        VALUE(rv_auth) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS reset_count
      IMPORTING
        !iv_url TYPE string .
    CLASS-METHODS get_default_user
      IMPORTING
        !iv_uri            TYPE string
      RETURNING
        VALUE(rv_username) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS set_default_user
      IMPORTING
        !iv_uri      TYPE string
        !iv_username TYPE string
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS gc_max_logins TYPE i VALUE 3.

    TYPES:
      BEGIN OF ty_auth,
        uri           TYPE string,
        authorization TYPE string,
        digest        TYPE REF TO zcl_abapgit_http_digest,
      END OF ty_auth .

    TYPES:
      BEGIN OF ty_login,
        url   TYPE string,
        count TYPE i,
      END OF ty_login .

    CLASS-DATA:
      gt_auth TYPE HASHED TABLE OF ty_auth WITH UNIQUE KEY uri.
    CLASS-DATA:
      gt_logins TYPE HASHED TABLE OF ty_login WITH UNIQUE KEY url .

    CLASS-METHODS append
      IMPORTING
        !iv_uri    TYPE string
        !iv_auth   TYPE string
        !io_digest TYPE REF TO zcl_abapgit_http_digest OPTIONAL
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS zcl_abapgit_login_manager IMPLEMENTATION.


  METHOD append.

    DATA ls_auth LIKE LINE OF gt_auth.

    READ TABLE gt_auth WITH TABLE KEY uri = zcl_abapgit_url=>host( iv_uri )
      TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      ls_auth-uri           = zcl_abapgit_url=>host( iv_uri ).
      ls_auth-authorization = iv_auth.
      ls_auth-digest        = io_digest.
      INSERT ls_auth INTO TABLE gt_auth.
    ENDIF.

  ENDMETHOD.


  METHOD clear.

    CLEAR gt_auth.

  ENDMETHOD.


  METHOD get.

    DATA ls_auth LIKE LINE OF gt_auth.

    READ TABLE gt_auth INTO ls_auth WITH TABLE KEY uri = zcl_abapgit_url=>host( iv_uri ).
    IF sy-subrc = 0.
      rv_auth = ls_auth-authorization.
    ENDIF.

  ENDMETHOD.


  METHOD get_default_user.

    " No default user for proxy
    IF iv_uri = gc_proxy.
      RETURN.
    ENDIF.

    rv_username = zcl_abapgit_persistence_user=>get_instance( )->get_repo_login( iv_uri ).

  ENDMETHOD.


  METHOD load.

    DATA: ls_auth LIKE LINE OF gt_auth.

    READ TABLE gt_auth INTO ls_auth WITH TABLE KEY uri = zcl_abapgit_url=>host( iv_uri ).
    IF sy-subrc = 0.
      rv_authorization = ls_auth-authorization.

      IF NOT ii_client IS INITIAL.
        IF ls_auth-digest IS BOUND.
          " Digest Authentication
          " https://en.wikipedia.org/wiki/Digest_access_authentication
          " e.g. used by https://www.gerritcodereview.com/
          ls_auth-digest->run( ii_client ).
        ELSE.
          " Basic Authentication
          " https://en.wikipedia.org/wiki/Basic_access_authentication
          ii_client->request->set_header_field(
            name  = 'Authorization'
            value = ls_auth-authorization ).
        ENDIF.
        ii_client->propertytype_logon_popup = ii_client->co_disabled.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD login.

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

    " Until maximum number of logins has been reached, trigger the login page
    " by raising an exception with the URL parameter. The exception is caught
    " in ZCL_ABAPGIT_GUI->HANDLE_ACTION which redirects to the login page.
    IF <ls_login>-count BETWEEN 1 AND gc_max_logins.
      zcx_abapgit_exception=>raise_login(
        iv_url    = iv_url
        iv_digest = iv_digest
        iv_count  = <ls_login>-count ).
    ENDIF.

    " The previous logins were not successful. Reset the count and continue to
    " process as usual. This will typically result in an error message
    " on the target page
    reset_count( iv_url ).

  ENDMETHOD.


  METHOD reset_count.
    DELETE gt_logins WHERE url = iv_url.
  ENDMETHOD.


  METHOD save.

    DATA: lv_auth TYPE string.

    lv_auth = ii_client->request->get_header_field( 'Authorization' ).

    IF NOT lv_auth IS INITIAL.
      append( iv_uri  = iv_uri
              iv_auth = lv_auth ).
    ENDIF.

  ENDMETHOD.


  METHOD set.

    DATA: lv_concat TYPE string,
          lo_digest TYPE REF TO zcl_abapgit_http_digest.

    ASSERT NOT iv_uri IS INITIAL.

    IF iv_username IS INITIAL OR iv_password IS INITIAL.
      RETURN.
    ENDIF.

    CONCATENATE iv_username ':' iv_password INTO lv_concat.

    IF iv_uri = gc_proxy.
      rv_auth = lv_concat.
    ELSE.
      rv_auth = cl_http_utility=>if_http_utility~encode_base64( lv_concat ).

      CONCATENATE 'Basic' rv_auth INTO rv_auth
        SEPARATED BY space.
    ENDIF.

    IF iv_digest CP 'Digest *'.
      CREATE OBJECT lo_digest
        EXPORTING
          iv_digest   = iv_digest
          iv_username = iv_username
          iv_password = iv_password.
    ENDIF.

    append( iv_uri    = iv_uri
            iv_auth   = rv_auth
            io_digest = lo_digest ).

  ENDMETHOD.


  METHOD set_default_user.

    " No default user for proxy
    IF iv_uri = gc_proxy.
      RETURN.
    ENDIF.

    zcl_abapgit_persistence_user=>get_instance( )->set_repo_login(
      iv_url   = iv_uri
      iv_login = iv_username ).

  ENDMETHOD.
ENDCLASS.
