INTERFACE zif_abapgit_pr_enum_provider
  PUBLIC .

  TYPES:
    BEGIN OF ty_pull_request,
      base_url        TYPE string,
      number          TYPE string,
      title           TYPE string,
      user            TYPE string,
      head_url        TYPE string,
      head_branch     TYPE string,
      created_at      TYPE string, " TODO change to D after date parsing fixed
      is_for_upstream TYPE abap_bool,
      draft           TYPE abap_bool,
      html_url        TYPE string,
    END OF ty_pull_request.
  TYPES:
    ty_pull_requests TYPE STANDARD TABLE OF ty_pull_request WITH KEY base_url number.

  METHODS list_pull_requests
    RETURNING
      VALUE(rt_pulls) TYPE ty_pull_requests
    RAISING
      zcx_abapgit_exception.

  METHODS create_repository
    IMPORTING
      iv_description TYPE string OPTIONAL
      iv_is_org      TYPE abap_bool DEFAULT abap_true
      iv_private     TYPE abap_bool DEFAULT abap_true
      iv_auto_init   TYPE abap_bool DEFAULT abap_true
    RAISING
      zcx_abapgit_exception.

  METHODS create_initial_branch
    IMPORTING
      iv_readme             TYPE string OPTIONAL
      iv_branch_name        TYPE string DEFAULT 'main'
    RETURNING
      VALUE(rv_branch_name) TYPE string
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
