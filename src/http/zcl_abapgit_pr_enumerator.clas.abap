class ZCL_ABAPGIT_PR_ENUMERATOR definition
  public
  final
  create public .

  public section.

    class-methods IS_PLATFORM_SUPPORTED
      importing
        !IV_URL type STRING
      returning
        value(RV_YES) type ABAP_BOOL .

    methods constructor
      importing
        iv_repo_url type string.

    methods get_repo_info
      raising
        zcx_abapgit_exception.


  PROTECTED SECTION.
  PRIVATE SECTION.
    data mv_repo_url type string.
ENDCLASS.



CLASS ZCL_ABAPGIT_PR_ENUMERATOR IMPLEMENTATION.


  method constructor.
    mv_repo_url = to_lower( iv_repo_url ).
  endmethod.


  method get_repo_info.

    data li_agent type ref to zif_abapgit_http_agent.

    li_agent = zcl_abapgit_http_agent=>create( ).

    " get repo name
    data lv_user type string.
    data lv_repo type string.
    FIND ALL OCCURRENCES OF regex 'github\.com\/([^\/]+)\/([^\/]+)' IN mv_repo_url
      submatches lv_user lv_repo.

    " request
    data lo_headers type ref to zcl_abapgit_string_map.
    create object lo_headers.
    lo_headers->set(
      iv_key = 'Accept'
      iv_val = 'application/vnd.github.v3+json' ).

    data li_response type ref to zif_abapgit_http_response.
    data lv_url type string.

    lv_url = |https://api.github.com/repos/{ lv_user }/{ lv_repo }|.

    li_response = li_agent->request(
      iv_url     = lv_url
      io_headers = lo_headers ).

    data lv_resp type string.
    lv_resp = li_response->cdata( ).

  endmethod.


  method is_platform_supported.
  endmethod.
ENDCLASS.
