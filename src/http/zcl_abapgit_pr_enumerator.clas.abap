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
        iv_repo_url TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS get_repo_info
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

    mv_repo_url = to_lower( iv_repo_url ).
    mi_enum_provider = create_provider( mv_repo_url ).

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


  METHOD get_repo_info.


  ENDMETHOD.


  METHOD is_platform_supported.
  ENDMETHOD.
ENDCLASS.
