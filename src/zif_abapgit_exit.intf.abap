INTERFACE zif_abapgit_exit
  PUBLIC .


  TYPES:
    ty_icm_sinfo2_tt TYPE STANDARD TABLE OF icm_sinfo2 WITH DEFAULT KEY .
  TYPES:
    BEGIN OF ty_ci_repo,
      name      TYPE string,
      clone_url TYPE string,
    END OF ty_ci_repo .
  TYPES:
    ty_ci_repos TYPE TABLE OF ty_ci_repo .

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
      !is_class_key    TYPE seoclskey
    RETURNING
      VALUE(rt_source) TYPE zif_abapgit_definitions=>ty_string_tt
    RAISING
      zcx_abapgit_exception .
  METHODS deserialize_postprocess
    IMPORTING
      !is_step TYPE zif_abapgit_definitions=>ty_step_data
      !ii_log  TYPE REF TO zif_abapgit_log .
  METHODS get_ci_tests
    IMPORTING
      !iv_object   TYPE tadir-object
    CHANGING
      !ct_ci_repos TYPE ty_ci_repos .
  METHODS adjust_display_commit_url
    IMPORTING !iv_repo_url    TYPE zif_abapgit_persistence=>ty_repo-url
              !iv_repo_name   TYPE string
              !iv_repo_key    TYPE zif_abapgit_persistence=>ty_value
              !iv_commit_hash TYPE zif_abapgit_definitions=>ty_sha1
    CHANGING  !cv_display_url TYPE zif_abapgit_persistence=>ty_repo-url
    RAISING   zcx_abapgit_exception .
ENDINTERFACE.
