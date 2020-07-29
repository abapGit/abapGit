CLASS zcl_abapgit_pr_enum_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS create_provider
      IMPORTING
        iv_repo_url        TYPE string
      RETURNING
        VALUE(ri_provider) TYPE REF TO zif_abapgit_pr_enum_provider
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_ABAPGIT_PR_ENUM_FACTORY IMPLEMENTATION.


  METHOD create_provider.

    DATA li_agent TYPE REF TO zif_abapgit_http_agent.
    DATA lv_repo_url TYPE string.
    DATA lv_user TYPE string.
    DATA lv_repo TYPE string.

    lv_repo_url = to_lower( iv_repo_url ).
    li_agent    = zcl_abapgit_factory=>get_http_agent( ).

    FIND ALL OCCURRENCES OF REGEX 'github\.com\/([^\/]+)\/([^\/]+)'
      IN lv_repo_url
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
ENDCLASS.
