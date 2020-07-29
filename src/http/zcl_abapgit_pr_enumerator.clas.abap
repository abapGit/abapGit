CLASS zcl_abapgit_pr_enumerator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        io_repo TYPE REF TO zcl_abapgit_repo_online
      RAISING
        zcx_abapgit_exception.

    METHODS has_pulls
      RETURNING
        VALUE(rv_yes) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.

    METHODS get_pulls
      RETURNING
        VALUE(rt_pulls) TYPE zif_abapgit_pr_enum_provider=>tty_pulls
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_repo_url TYPE string.
    DATA mi_enum_provider TYPE REF TO zif_abapgit_pr_enum_provider.

    CLASS-METHODS create_provider
      IMPORTING
        iv_repo_url TYPE string
      RETURNING
        VALUE(ri_provider) TYPE REF TO zif_abapgit_pr_enum_provider
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_PR_ENUMERATOR IMPLEMENTATION.


  METHOD constructor.

    mv_repo_url = to_lower( io_repo->get_url( ) ).
    TRY .
      mi_enum_provider = create_provider( mv_repo_url ).
    CATCH zcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.


  METHOD create_provider.

    DATA li_agent TYPE REF TO zif_abapgit_http_agent.
    DATA lv_user TYPE string.
    DATA lv_repo TYPE string.

    li_agent = zcl_abapgit_factory=>get_http_agent( ).

    FIND ALL OCCURRENCES OF REGEX 'github\.com\/([^\/]+)\/([^\/]+)'
      IN iv_repo_url
      SUBMATCHES lv_user lv_repo.
    IF sy-subrc = 0.
      CREATE OBJECT ri_provider TYPE zcl_abapgit_pr_enum_github
        EXPORTING
          iv_user_repo  = |{ lv_user }/{ lv_repo }|
          ii_http_agent = li_agent.
    ELSE.
      zcx_abapgit_exception=>raise( |PR enumeration is not supported for { iv_repo_url }| ).
    ENDIF.

    " TODO somewhen more providers

  ENDMETHOD.


  METHOD get_pulls.

    IF mi_enum_provider IS NOT BOUND.
      RETURN.
    ENDIF.

    rt_pulls = mi_enum_provider->list_pull_requests( ).

    " TODO caching ?

  ENDMETHOD.


  METHOD has_pulls.

    IF mi_enum_provider IS NOT BOUND.
      RETURN. " false
    ENDIF.

    IF get_pulls( ) IS NOT INITIAL.
      rv_yes = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
