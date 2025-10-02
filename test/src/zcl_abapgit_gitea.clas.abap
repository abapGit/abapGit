CLASS zcl_abapgit_gitea DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS create_repo
      IMPORTING
        iv_name       TYPE string
      RETURNING
        VALUE(rv_url) TYPE string
      RAISING
        zcx_abapgit_exception.

  PRIVATE SECTION.
    CONSTANTS c_base TYPE string VALUE 'http://localhost:3050'.
    CONSTANTS c_username TYPE string VALUE 'abapgit'.
    CONSTANTS c_password TYPE string VALUE 'abapgit'.
ENDCLASS.

CLASS zcl_abapgit_gitea IMPLEMENTATION.
  METHOD create_repo.

    DATA li_agent    TYPE REF TO zif_abapgit_http_agent.
    DATA li_response TYPE REF TO zif_abapgit_http_response.
    DATA lv_json     TYPE string.
    DATA lv_url      TYPE string.


    li_agent = zcl_abapgit_http_agent=>create( ).

    li_agent->global_headers( )->set(
      iv_key = 'Accept'
      iv_val = 'application/json' ).

    li_agent->global_headers( )->set(
      iv_key = 'Content-Type'
      iv_val = 'application/json' ).

    lv_url = c_base && '/api/v1/user/repos'.

    zcl_abapgit_login_manager=>set_basic(
      iv_uri      = lv_url
      iv_username = c_username
      iv_password = c_password ).

    li_agent->global_headers( )->set(
      iv_key = 'Authorization'
      iv_val = zcl_abapgit_login_manager=>get( lv_url ) ).

    lv_json = `{` && |\n| &&
      `  "auto_init": true,` && |\n| &&
      `  "default_branch": "main",` && |\n| &&
      `  "description": "description",` && |\n| &&
      `  "license": "MIT",` && |\n| &&
      `  "name": "` && iv_name && `",` && |\n| &&
      `  "private": false,` && |\n| &&
      `  "trust_model": "default"` && |\n| &&
      `}`.

    li_response = li_agent->request(
      iv_url     = lv_url
      iv_method  = 'POST'
      iv_payload = lv_json ).

    IF li_response->code( ) <> 201.
      zcx_abapgit_exception=>raise( |Error creating repository| ).
    ENDIF.

    rv_url = |{ c_base }/abapgit/{ iv_name }|.

  ENDMETHOD.
ENDCLASS.
