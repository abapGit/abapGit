CLASS zcl_abapgit_login_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

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
    CLASS-METHODS get_digest
      IMPORTING
        !iv_uri          TYPE string
      RETURNING
        VALUE(ro_digest) TYPE REF TO zcl_abapgit_http_digest
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_auth,
        uri           TYPE string,
        authorization TYPE string,
        digest        TYPE REF TO zcl_abapgit_http_digest,
      END OF ty_auth .

    CLASS-DATA:
      gt_auth TYPE HASHED TABLE OF ty_auth WITH UNIQUE KEY uri.

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


  METHOD get_digest.

    DATA ls_auth LIKE LINE OF gt_auth.

    READ TABLE gt_auth INTO ls_auth WITH TABLE KEY uri = zcl_abapgit_url=>host( iv_uri ).
    IF sy-subrc = 0.
      ro_digest = ls_auth-digest.
    ENDIF.

  ENDMETHOD.


  METHOD load.

    DATA: ls_auth LIKE LINE OF gt_auth.

    READ TABLE gt_auth INTO ls_auth WITH TABLE KEY uri = zcl_abapgit_url=>host( iv_uri ).
    IF sy-subrc = 0.
      rv_authorization = ls_auth-authorization.

      IF NOT ii_client IS INITIAL.
        IF ls_auth-digest IS BOUND.
          " Digest Authentification
          " https://en.wikipedia.org/wiki/Digest_access_authentication
          " e.g. used by https://www.gerritcodereview.com/
          ls_auth-digest->run( ii_client ).
        ELSE.
          " Basic Authentification
          " https://en.wikipedia.org/wiki/Basic_access_authentication
          ii_client->request->set_header_field(
            name  = 'Authorization'
            value = ls_auth-authorization ).
        ENDIF.
        ii_client->propertytype_logon_popup = ii_client->co_disabled.
      ENDIF.
    ENDIF.

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

    rv_auth = cl_http_utility=>if_http_utility~encode_base64( lv_concat ).

    CONCATENATE 'Basic' rv_auth INTO rv_auth
      SEPARATED BY space.

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
ENDCLASS.
