*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_BRANCH_OVERVIEW
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_branch_overview DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_repo TYPE REF TO lcl_repo_online,
      lif_gui_page~render REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo     TYPE REF TO lcl_repo_online,
          mt_branches TYPE lcl_git_transport=>ty_branch_list_tt,
          mt_objects  TYPE ty_objects_tt.

    TYPES: BEGIN OF ty_create,
             name   TYPE string,
             parent TYPE string,
           END OF ty_create.

    TYPES: BEGIN OF ty_commit,
             sha1    TYPE ty_sha1,
             parent  TYPE ty_sha1,
             author  TYPE string,
             email   TYPE string,
             time    TYPE string,
             message TYPE string,
             branch  TYPE string,
             create  TYPE STANDARD TABLE OF ty_create WITH DEFAULT KEY,
           END OF ty_commit.

    DATA: mt_commits TYPE TABLE OF ty_commit.

* todo, split up in UI and logic classes
    METHODS:
      body
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      get_script
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      parse_commits
        RAISING lcx_exception,
      determine_branch
        RAISING lcx_exception,
      get_git_objects
        RAISING lcx_exception.

ENDCLASS.                       "lcl_gui_page_explore DEFINITION

CLASS lcl_gui_page_branch_overview IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_repo = io_repo.
  ENDMETHOD.

  METHOD get_git_objects.

    lcl_progress=>show( iv_key     = 'Get git objects'
                        iv_current = 1
                        iv_total   = 1
                        iv_text    = mo_repo->get_name( ) ) ##NO_TEXT.

* get objects directly from git, mo_repo only contains a shallow clone of only
* the selected branch

    mt_branches = lcl_git_transport=>branches( mo_repo->get_url( ) ).

    lcl_git_transport=>upload_pack( EXPORTING io_repo = mo_repo
                                              iv_deepen = abap_false
                                              it_branches = mt_branches
                                    IMPORTING et_objects = mt_objects ).

    DELETE mt_objects WHERE type = gc_type-blob.

  ENDMETHOD.

  METHOD parse_commits.

    DATA: ls_commit LIKE LINE OF mt_commits,
          ls_raw    TYPE lcl_git_pack=>ty_commit.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF mt_objects.


    LOOP AT mt_objects ASSIGNING <ls_object> WHERE type = gc_type-commit.
      ls_raw = lcl_git_pack=>decode_commit( <ls_object>-data ).

      CLEAR ls_commit.
      ls_commit-sha1 = <ls_object>-sha1.
      ls_commit-parent = ls_raw-parent.
      ls_commit-message = ls_raw-body.
* todo, handle time zones
      FIND REGEX '^([\w\s]+) <(.*)> (\d{10}) .\d{4}$' IN ls_raw-author
        SUBMATCHES
        ls_commit-author
        ls_commit-email
        ls_commit-time ##NO_TEXT.
      IF sy-subrc <> 0.
        BREAK-POINT.
      ENDIF.
      APPEND ls_commit TO mt_commits.

    ENDLOOP.

  ENDMETHOD.

  METHOD determine_branch.

    CONSTANTS: lc_head TYPE string VALUE 'HEAD'.

    DATA: lv_name TYPE string.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF mt_branches,
                   <ls_head>   LIKE LINE OF mt_branches,
                   <ls_commit> LIKE LINE OF mt_commits,
                   <ls_create> LIKE LINE OF <ls_commit>-create.


* exchange HEAD, and make sure the branch determination starts with the HEAD branch
    READ TABLE mt_branches ASSIGNING <ls_head> WITH KEY name = lc_head.
    ASSERT sy-subrc = 0.
    LOOP AT mt_branches ASSIGNING <ls_branch> WHERE sha1 = <ls_head>-sha1 AND name <> lc_head.
      <ls_head>-name = <ls_branch>-name.
      DELETE mt_branches INDEX sy-tabix.
      EXIT.
    ENDLOOP.

* todo, merging?
    LOOP AT mt_branches ASSIGNING <ls_branch>.
      lv_name = <ls_branch>-name+11.
      READ TABLE mt_commits ASSIGNING <ls_commit> WITH KEY sha1 = <ls_branch>-sha1.
      ASSERT sy-subrc = 0.

      DO.
        IF <ls_commit>-branch IS INITIAL.
          <ls_commit>-branch = lv_name.
        ELSE.
          APPEND INITIAL LINE TO <ls_commit>-create ASSIGNING <ls_create>.
          <ls_create>-name = lv_name.
          <ls_create>-parent = <ls_commit>-branch.
          EXIT.
        ENDIF.

        IF <ls_commit>-parent IS INITIAL.
          EXIT.
        ELSE.
          READ TABLE mt_commits ASSIGNING <ls_commit> WITH KEY sha1 = <ls_commit>-parent.
          ASSERT sy-subrc = 0.
        ENDIF.
      ENDDO.

    ENDLOOP.

    SORT mt_commits BY time ASCENDING.

  ENDMETHOD.

  METHOD get_script.

    DATA: li_client TYPE REF TO if_http_client,
          lv_url    TYPE string.

    lv_url = 'https://raw.githubusercontent.com/bpatra/gitgraph.js/develop/src/gitgraph.js'.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = lv_url
        ssl_id             = 'ANONYM'
      IMPORTING
        client             = li_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4 ).
    IF sy-subrc <> 0.
      _raise 'error fetching gitgraph.js script'.
    ENDIF.

    li_client->send( ).
    li_client->receive( ).

    CREATE OBJECT ro_html.
    ro_html->add( li_client->response->get_cdata( ) ).

  ENDMETHOD.

  METHOD body.

    FIELD-SYMBOLS: <ls_commit> LIKE LINE OF mt_commits,
                   <lv_create> LIKE LINE OF <ls_commit>-create,
                   <ls_branch> LIKE LINE OF mt_branches.


    CREATE OBJECT ro_html.

    get_git_objects( ).
    parse_commits( ).
    determine_branch( ).

    _add '<br>'.
    _add 'todo, see https://github.com/larshp/abapGit/issues/272'.

    _add '<canvas id="gitGraph"></canvas>'.

    _add '<script type="text/javascript">'.
* todo, temporary workaround
* see https://github.com/nicoespeon/gitgraph.js/pull/88
* https://github.com/nicoespeon/gitgraph.js/issues/86
    ro_html->add( get_script( ) ).
    _add '</script>'.
*    _add '<script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/gitgraph.js/1.2.2/gitgraph.min.js"></script>'.

    _add '<script type="text/javascript">'.

    _add 'var gitgraph = new GitGraph({'.
    _add '  template: "metro",'.
    _add '  orientation: "vertical-reverse"'.
    _add '});'.

    LOOP AT mt_commits ASSIGNING <ls_commit>.
      IF sy-tabix = 1.
* assumption: all branches are created from master
        ro_html->add( |var var{ <ls_commit>-branch } = gitgraph.branch("{ <ls_commit>-branch }");| ).
      ENDIF.

      ro_html->add( |var{ <ls_commit>-branch }.commit(\{message: "{ <ls_commit>-message }", author: "{ <ls_commit>-author }", sha1: "{ <ls_commit>-sha1(7) }"\});| ).

      LOOP AT <ls_commit>-create ASSIGNING <lv_create>.
        ro_html->add( |var var{ <lv_create>-name } = var{ <lv_create>-parent }.branch("{ <lv_create>-name }");| ).
      ENDLOOP.

    ENDLOOP.

    _add '</script>'.

  ENDMETHOD.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( ) ).
    ro_html->add( title( 'BRANCH_OVERVIEW' ) ).
    _add '<div id="toc">'.
    ro_html->add( body( ) ).
    _add '</div>'.
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.