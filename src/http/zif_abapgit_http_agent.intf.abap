INTERFACE zif_abapgit_http_agent
  PUBLIC .

  CONSTANTS:
    BEGIN OF c_methods,
      get    TYPE string VALUE 'GET',
      post   TYPE string VALUE 'POST',
      put    TYPE string VALUE 'PUT',
      delete TYPE string VALUE 'DELETE',
      patch  TYPE string VALUE 'PATCH',
    END OF c_methods.

  METHODS global_headers
    RETURNING
      VALUE(ro_global_headers) TYPE REF TO zcl_abapgit_string_map.

  METHODS request
    IMPORTING
      !iv_url            TYPE string
      !iv_method         TYPE string DEFAULT c_methods-get
      !io_query          TYPE REF TO zcl_abapgit_string_map OPTIONAL
      !io_headers        TYPE REF TO zcl_abapgit_string_map OPTIONAL
      !iv_payload        TYPE any OPTIONAL " can be string, xstring
    RETURNING
      VALUE(ri_response) TYPE REF TO zif_abapgit_http_response
    RAISING
      zcx_abapgit_exception .

ENDINTERFACE.
