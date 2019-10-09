INTERFACE zif_abapgit_exit
  PUBLIC .


  TYPES:
    ty_icm_sinfo2_tt TYPE STANDARD TABLE OF icm_sinfo2 WITH DEFAULT KEY .

  METHODS change_local_host
    CHANGING
      !ct_hosts TYPE ty_icm_sinfo2_tt .
  METHODS allow_sap_objects
    RETURNING
      VALUE(rv_allowed) TYPE abap_bool .
  METHODS change_proxy_url
    IMPORTING
      !iv_repo_url  TYPE csequence
    CHANGING
      !cv_proxy_url TYPE string .
  METHODS change_proxy_port
    IMPORTING
      !iv_repo_url   TYPE csequence
    CHANGING
      !cv_proxy_port TYPE string .
  METHODS change_proxy_authentication
    IMPORTING
      !iv_repo_url             TYPE csequence
    CHANGING
      !cv_proxy_authentication TYPE abap_bool .
  METHODS create_http_client
    IMPORTING
      !iv_url          TYPE string
    RETURNING
      VALUE(ri_client) TYPE REF TO if_http_client
    RAISING
      zcx_abapgit_exception .
  METHODS http_client
    IMPORTING
      !iv_url    TYPE string
      !ii_client TYPE REF TO if_http_client .
  METHODS change_tadir
    IMPORTING
      !iv_package TYPE devclass
      !ii_log     TYPE REF TO zif_abapgit_log
    CHANGING
      !ct_tadir   TYPE zif_abapgit_definitions=>ty_tadir_tt .
  METHODS get_ssl_id
    RETURNING
      VALUE(rv_ssl_id) TYPE ssfapplssl .
  METHODS custom_serialize_abap_clif
    IMPORTING
      is_class_key     TYPE seoclskey
    RETURNING
      VALUE(rt_source) TYPE zif_abapgit_definitions=>ty_string_tt
    RAISING
      zcx_abapgit_exception.
ENDINTERFACE.
