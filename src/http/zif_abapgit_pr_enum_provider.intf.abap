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
    END OF ty_pull_request.
  TYPES:
    tty_pulls TYPE STANDARD TABLE OF ty_pull_request WITH KEY base_url number.

  METHODS list_pull_requests
    RETURNING
      VALUE(rt_pulls) TYPE tty_pulls
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.
