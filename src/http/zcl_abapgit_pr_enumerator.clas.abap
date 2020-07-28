CLASS zcl_abapgit_pr_enumerator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS is_platform_supported
      IMPORTING
        !iv_url       TYPE string
      RETURNING
        VALUE(rv_yes) TYPE abap_bool .

    METHODS constructor
      IMPORTING
        iv_repo_url TYPE string.

    METHODS get_repo_info
      RAISING
        zcx_abapgit_exception.


  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_repo_url TYPE string.
ENDCLASS.



CLASS ZCL_ABAPGIT_PR_ENUMERATOR IMPLEMENTATION.


  METHOD constructor.
    mv_repo_url = to_lower( iv_repo_url ).
  ENDMETHOD.


  METHOD get_repo_info.

    DATA li_agent TYPE REF TO zif_abapgit_http_agent.

    li_agent = zcl_abapgit_http_agent=>create( ).

    " get repo name
    DATA lv_user TYPE string.
    DATA lv_repo TYPE string.
    FIND ALL OCCURRENCES OF REGEX 'github\.com\/([^\/]+)\/([^\/]+)' IN mv_repo_url
      SUBMATCHES lv_user lv_repo.

  ENDMETHOD.


  METHOD is_platform_supported.
  ENDMETHOD.
ENDCLASS.
