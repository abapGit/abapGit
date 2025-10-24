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

    METHODS create_pull_request
      IMPORTING
        iv_title TYPE clike
        iv_body  TYPE clike OPTIONAL
        iv_head  TYPE string
        iv_base  TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS merge_pull_request
      IMPORTING
        iv_pull_number TYPE i
      RAISING
        zcx_abapgit_exception.

    METHODS update_pull_request_branch
      IMPORTING
        iv_pull_number       TYPE i
        iv_expected_head_sha TYPE zif_abapgit_git_definitions=>ty_sha1
      RAISING
        zcx_abapgit_exception.

    METHODS ready_for_review
      IMPORTING
        iv_pull_number TYPE i
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
    DATA mv_user_and_repo TYPE string.

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



CLASS zcl_abapgit_pr_enum_github IMPLEMENTATION.

  METHOD clean_url.
    rv_url = replace(
      val = iv_url
      regex = '\{.*\}$'
      with = '' ).
  ENDMETHOD.

  METHOD constructor.

    DATA lv_search TYPE string.

    mv_user_and_repo = iv_user_and_repo.
    mv_repo_url   = |https://api.github.com/repos/{ iv_user_and_repo }|.
    mi_http_agent = ii_http_agent.
    mi_http_agent->global_headers( )->set(
      iv_key = 'Accept'
      iv_val = 'application/vnd.github.v3+json' ).

    IF zcl_abapgit_login_manager=>get( mv_repo_url ) IS NOT INITIAL.
      mi_http_agent->global_headers( )->set(
        iv_key = 'Authorization'
        iv_val = zcl_abapgit_login_manager=>get( mv_repo_url ) ).
    ELSE.
* fallback, try searching for the git credentials
      lv_search = mv_repo_url.
      REPLACE FIRST OCCURRENCE OF 'api.github.com/repos' IN lv_search WITH 'github.com'.
      IF zcl_abapgit_login_manager=>get( lv_search ) IS NOT INITIAL.
        mi_http_agent->global_headers( )->set(
          iv_key = 'Authorization'
          iv_val = zcl_abapgit_login_manager=>get( lv_search ) ).
      ENDIF.

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
      <ls_p>-draft           = ii_json->get_boolean( |/{ lv_i }/draft| ).
      <ls_p>-html_url        = ii_json->get( |/{ lv_i }/html_url| ).
    ENDLOOP.

  ENDMETHOD.


  METHOD create_pull_request.
* https://docs.github.com/en/rest/pulls/pulls?apiVersion=2022-11-28#create-a-pull-request

    DATA lv_owner    TYPE string.
    DATA lv_repo     TYPE string.
    DATA lv_url      TYPE string.
    DATA lv_json     TYPE string.
    DATA li_response TYPE REF TO zif_abapgit_http_response.

    lv_url = mv_repo_url && '/pulls'.
    SPLIT mv_user_and_repo AT '/' INTO lv_owner lv_repo.

    lv_json = |\{\n| &&
              |  "owner": "{ lv_owner }",\n| &&
              |  "repo": "{ lv_repo }",\n| &&
              |  "title": "{ iv_title }",\n| &&
              |  "head": "{ iv_head }",\n| &&
              |  "body": "{ iv_body }",\n| &&
              |  "maintainer_can_modify": true,\n| &&
              |  "draft": true,\n| &&
              |  "base": "{ iv_base }"\n| &&
              |\}|.

    li_response = mi_http_agent->request(
      iv_url     = lv_url
      iv_method  = zif_abapgit_http_agent=>c_methods-post
      iv_payload = lv_json ).

    IF li_response->is_ok( ) = abap_false.
      zcx_abapgit_exception=>raise( |Error creating pull request: { li_response->error( ) }| ).
    ENDIF.

  ENDMETHOD.

  METHOD merge_pull_request.
* https://docs.github.com/en/rest/pulls/pulls?apiVersion=2022-11-28#merge-a-pull-request

    DATA lv_url      TYPE string.
    DATA lv_json     TYPE string.
    DATA li_response TYPE REF TO zif_abapgit_http_response.

    lv_url = mv_repo_url && '/pulls/' && iv_pull_number && '/merge'.

    lv_json = |\{\n| &&
              |  "commit_title": "Merge pull request #{ iv_pull_number }",\n| &&
              |  "merge_method": "squash"\n| &&
              |\}|.

    li_response = mi_http_agent->request(
      iv_url     = lv_url
      iv_method  = zif_abapgit_http_agent=>c_methods-put
      iv_payload = lv_json ).

    IF li_response->is_ok( ) = abap_false.
      zcx_abapgit_exception=>raise( |Error merging pull request: { li_response->error( ) }| ).
    ENDIF.

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


  METHOD ready_for_review.
* https://docs.github.com/en/graphql/reference/mutations#markpullrequestreadyforreview
* https://gist.github.com/jeromepl/02e70f3ea4a4e8103da6f96f14eb213c

    DATA lv_url      TYPE string.
    DATA lv_json     TYPE string.
    DATA li_response TYPE REF TO zif_abapgit_http_response.
    DATA lx_ajson    TYPE REF TO zcx_abapgit_ajson_error.
    DATA lv_node_id  TYPE string.

    lv_url = mv_repo_url && '/pulls/' && iv_pull_number.

    li_response = mi_http_agent->request(
      iv_url     = lv_url
      iv_method  = zif_abapgit_http_agent=>c_methods-get ).

    IF li_response->is_ok( ) = abap_false.
      zcx_abapgit_exception=>raise( |Error getting pull request information: { li_response->error( ) }| ).
    ENDIF.

    TRY.
        lv_node_id = li_response->json( )->get( |/node_id| ).
      CATCH zcx_abapgit_ajson_error INTO lx_ajson.
        zcx_abapgit_exception=>raise_with_text( lx_ajson ).
    ENDTRY.

    lv_json = |\{"query": "mutation \{ markPullRequestReadyForReview(input: | &&
      |\{ pullRequestId: \\"{ lv_node_id }\\" \}) \{ pullRequest \{ id \} \} \}" \}|.

    li_response = mi_http_agent->request(
      iv_url     = 'https://api.github.com/graphql'
      iv_method  = zif_abapgit_http_agent=>c_methods-post
      iv_payload = lv_json ).

    IF li_response->is_ok( ) = abap_false.
      zcx_abapgit_exception=>raise( |Error setting to ready: { li_response->error( ) }| ).
    ENDIF.

  ENDMETHOD.


  METHOD update_pull_request_branch.
* https://docs.github.com/en/rest/pulls/pulls?apiVersion=2022-11-28#update-a-pull-request-branch

    DATA lv_owner    TYPE string.
    DATA lv_repo     TYPE string.
    DATA lv_url      TYPE string.
    DATA lv_json     TYPE string.
    DATA li_response TYPE REF TO zif_abapgit_http_response.

    lv_url = mv_repo_url && '/pulls/' && iv_pull_number && '/update-branch'.
    SPLIT mv_user_and_repo AT '/' INTO lv_owner lv_repo.

    lv_json = |\{\n| &&
              |  "expected_head_sha": "{ to_lower( iv_expected_head_sha ) }"\n| &&
              |\}|.

    li_response = mi_http_agent->request(
      iv_url     = lv_url
      iv_method  = zif_abapgit_http_agent=>c_methods-put
      iv_payload = lv_json ).

    IF li_response->is_ok( ) = abap_false.
      zcx_abapgit_exception=>raise( |Error updating pull request branch: { li_response->error( ) }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_pr_enum_provider~create_initial_branch.
* https://docs.github.com/en/rest/repos/contents?apiVersion=2022-11-28#create-or-update-file-contents--parameters

    DATA lv_owner    TYPE string.
    DATA lv_repo     TYPE string.
    DATA lv_url      TYPE string.
    DATA lv_contents TYPE string.
    DATA lv_json     TYPE string.
    DATA li_response TYPE REF TO zif_abapgit_http_response.

    lv_url = mv_repo_url && '/contents/README.md'.
    SPLIT mv_user_and_repo AT '/' INTO lv_owner lv_repo.

    lv_contents = iv_readme.

    IF lv_contents IS INITIAL.
      lv_contents = |# { to_upper( lv_repo ) }|.
    ENDIF.

    lv_json = |\{\n| &&
              |  "message": "Initial commit",\n| &&
              |  "content": "{ cl_http_utility=>encode_base64( lv_contents ) }",\n| &&
              |  "branch": "{ iv_branch_name }"\n| &&
              |\}|.

    li_response = mi_http_agent->request(
      iv_url     = lv_url
      iv_method  = zif_abapgit_http_agent=>c_methods-put
      iv_payload = lv_json ).

    IF li_response->is_ok( ) = abap_false.
      zcx_abapgit_exception=>raise( |Error creating initial { iv_branch_name } branch: { li_response->error( ) }| ).
    ENDIF.

    rv_branch_name = iv_branch_name.

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
