CLASS zcl_abapgit_pr_enumerator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        iv_url TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS get_pulls
      RETURNING
        VALUE(rt_pulls) TYPE zif_abapgit_pr_enum_provider=>ty_pull_requests
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

    CLASS-METHODS new
      IMPORTING
        iv_url             TYPE string
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_abapgit_pr_enumerator
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_repo_url TYPE string.
    DATA mi_enum_provider TYPE REF TO zif_abapgit_pr_enum_provider.

    CLASS-METHODS create_provider
      IMPORTING
        iv_repo_url        TYPE string
      RETURNING
        VALUE(ri_provider) TYPE REF TO zif_abapgit_pr_enum_provider
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_pr_enumerator IMPLEMENTATION.


  METHOD constructor.

    mv_repo_url = iv_url.
    TRY.
        mi_enum_provider = create_provider( mv_repo_url ).
      CATCH zcx_abapgit_exception ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD create_initial_branch.

    IF mi_enum_provider IS NOT BOUND.
      RETURN.
    ENDIF.

    rv_branch_name = mi_enum_provider->create_initial_branch(
      iv_readme      = iv_readme
      iv_branch_name = iv_branch_name ).

  ENDMETHOD.


  METHOD create_provider.

    DATA li_agent TYPE REF TO zif_abapgit_http_agent.
    DATA lv_user TYPE string.
    DATA lv_repo TYPE string.

    li_agent = zcl_abapgit_http_agent=>create( ).

    FIND ALL OCCURRENCES OF REGEX 'github\.com\/([^\/]+)\/([^\/]+)'
      IN iv_repo_url
      SUBMATCHES lv_user lv_repo ##REGEX_POSIX.
    IF sy-subrc = 0.
      lv_repo = replace(
        val = lv_repo
        regex = '\.git$'
        with = '' ) ##REGEX_POSIX.
      CREATE OBJECT ri_provider TYPE zcl_abapgit_pr_enum_github
        EXPORTING
          iv_user_and_repo = |{ lv_user }/{ lv_repo }|
          ii_http_agent    = li_agent.
    ENDIF.

* used in integration testing, see /test/ folder
    FIND ALL OCCURRENCES OF REGEX 'localhost:3050\/([^\/]+)\/([^\/]+)'
      IN iv_repo_url
      SUBMATCHES lv_user lv_repo ##REGEX_POSIX.
    IF sy-subrc = 0.
      CREATE OBJECT ri_provider TYPE zcl_abapgit_pr_enum_gitea
        EXPORTING
          iv_user_and_repo = |{ lv_user }/{ lv_repo }|
          ii_http_agent    = li_agent.
    ENDIF.

    " TODO somewhen more providers

    IF ri_provider IS NOT BOUND.
      zcx_abapgit_exception=>raise( |PR enumeration is not supported for { iv_repo_url }| ).
    ENDIF.

  ENDMETHOD.


  METHOD create_repository.

    IF mi_enum_provider IS NOT BOUND.
      RETURN.
    ENDIF.

    mi_enum_provider->create_repository(
      iv_description = iv_description
      iv_is_org      = iv_is_org
      iv_private     = iv_private
      iv_auto_init   = iv_auto_init ).

  ENDMETHOD.


  METHOD get_pulls.

    IF mi_enum_provider IS NOT BOUND.
      RETURN.
    ENDIF.

    rt_pulls = mi_enum_provider->list_pull_requests( ).

  ENDMETHOD.


  METHOD new.
    CREATE OBJECT ro_instance EXPORTING iv_url = iv_url.
  ENDMETHOD.
ENDCLASS.
