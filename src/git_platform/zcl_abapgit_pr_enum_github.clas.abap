CLASS zcl_abapgit_pr_enum_github DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_pr_enum_provider .

    METHODS constructor
      IMPORTING
        !iv_user_and_repo TYPE string
        !ii_http_agent    TYPE REF TO zif_abapgit_http_agent
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_info,
        repo_json TYPE REF TO zif_abapgit_ajson,
        pulls     TYPE zif_abapgit_pr_enum_provider=>ty_pull_requests,
      END OF ty_info.

    DATA mi_http_agent TYPE REF TO zif_abapgit_http_agent.
    DATA mv_repo_url TYPE string.

    METHODS fetch_repo_by_url
      IMPORTING
        iv_repo_url    TYPE string
      RETURNING
        VALUE(rs_info) TYPE ty_info
      RAISING
        zcx_abapgit_exception.

    METHODS convert_list
      IMPORTING
        ii_json         TYPE REF TO zif_abapgit_ajson
      RETURNING
        VALUE(rt_pulls) TYPE zif_abapgit_pr_enum_provider=>ty_pull_requests.

    METHODS clean_url
      IMPORTING
        iv_url        TYPE string
      RETURNING
        VALUE(rv_url) TYPE string.
ENDCLASS.



CLASS ZCL_ABAPGIT_PR_ENUM_GITHUB IMPLEMENTATION.


  METHOD clean_url.
    rv_url = replace(
      val = iv_url
      regex = '\{.*\}$'
      with = '' ).
  ENDMETHOD.


  METHOD constructor.

    mv_repo_url   = |https://api.github.com/repos/{ iv_user_and_repo }|.
    mi_http_agent = ii_http_agent.
    mi_http_agent->global_headers( )->set(
      iv_key = 'Accept'
      iv_val = 'application/vnd.github.v3+json' ).

    IF zcl_abapgit_login_manager=>get( mv_repo_url ) IS NOT INITIAL.
      mi_http_agent->global_headers( )->set(
        iv_key = 'Authorization'
        iv_val = zcl_abapgit_login_manager=>get( mv_repo_url ) ).
    ENDIF.

  ENDMETHOD.


  METHOD convert_list.

    DATA lt_items TYPE string_table.
    DATA lv_i TYPE string.
    FIELD-SYMBOLS <ls_p> LIKE LINE OF rt_pulls.

    lt_items = ii_json->members( '/' ).

    LOOP AT lt_items INTO lv_i.
      APPEND INITIAL LINE TO rt_pulls ASSIGNING <ls_p>.
      <ls_p>-base_url        = ii_json->get( |/{ lv_i }/base/repo/clone_url| ).
      <ls_p>-number          = ii_json->get( |/{ lv_i }/number| ).
      <ls_p>-title           = ii_json->get( |/{ lv_i }/title| ).
      <ls_p>-user            = ii_json->get( |/{ lv_i }/user/login| ).
      <ls_p>-head_url        = ii_json->get( |/{ lv_i }/head/repo/clone_url| ).
      <ls_p>-head_branch     = ii_json->get( |/{ lv_i }/head/ref| ).
      <ls_p>-created_at      = ii_json->get( |/{ lv_i }/created_at| ).
    ENDLOOP.

  ENDMETHOD.


  METHOD fetch_repo_by_url.

    DATA li_pulls_json TYPE REF TO zif_abapgit_ajson.
    DATA lv_pull_url TYPE string.
    DATA li_response TYPE REF TO zif_abapgit_http_response.
    DATA lx_ajson TYPE REF TO zcx_abapgit_ajson_error.

    li_response = mi_http_agent->request( iv_repo_url ).

    TRY.
        rs_info-repo_json = li_response->json( ).
        li_response->headers( ). " for debug
        lv_pull_url = clean_url( rs_info-repo_json->get( '/pulls_url' ) ).
        IF lv_pull_url IS INITIAL OR rs_info-repo_json->get( '/message' ) = 'Not Found'.
          RETURN.
        ENDIF.
        li_pulls_json = mi_http_agent->request( lv_pull_url )->json( ).
      CATCH zcx_abapgit_ajson_error INTO lx_ajson.
        zcx_abapgit_exception=>raise_with_text( lx_ajson ).
    ENDTRY.

    rs_info-pulls = convert_list( li_pulls_json ).

  ENDMETHOD.


  METHOD zif_abapgit_pr_enum_provider~list_pull_requests.

    DATA lv_upstream_url TYPE string.
    DATA ls_repo_info TYPE ty_info.
    FIELD-SYMBOLS <ls_p> LIKE LINE OF ls_repo_info-pulls.

    ls_repo_info = fetch_repo_by_url( mv_repo_url ).
    APPEND LINES OF ls_repo_info-pulls TO rt_pulls.

    IF ls_repo_info-repo_json->get_boolean( '/fork' ) = abap_true.
      lv_upstream_url = ls_repo_info-repo_json->get( '/source/url' ). " parent ?
      ls_repo_info = fetch_repo_by_url( lv_upstream_url ).
      LOOP AT ls_repo_info-pulls ASSIGNING <ls_p>.
        <ls_p>-is_for_upstream = abap_true.
        APPEND <ls_p> TO rt_pulls.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
