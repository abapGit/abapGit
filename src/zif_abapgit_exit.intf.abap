INTERFACE zif_abapgit_exit PUBLIC.

  TYPES:
    ty_icm_sinfo2_tt TYPE STANDARD TABLE OF icm_sinfo2 WITH DEFAULT KEY .

  METHODS:
    change_local_host
      CHANGING ct_hosts TYPE ty_icm_sinfo2_tt,
    allow_sap_objects
      RETURNING VALUE(rv_allowed) TYPE abap_bool,
    change_proxy_url
      IMPORTING iv_repo_url TYPE csequence
      CHANGING  c_proxy_url TYPE string,
    change_proxy_port
      IMPORTING iv_repo_url  TYPE csequence
      CHANGING  c_proxy_port TYPE string,
    change_proxy_authentication
      IMPORTING iv_repo_url            TYPE csequence
      CHANGING  c_proxy_authentication TYPE abap_bool,
    http_client
      IMPORTING
        ii_client TYPE REF TO if_http_client.

ENDINTERFACE.
