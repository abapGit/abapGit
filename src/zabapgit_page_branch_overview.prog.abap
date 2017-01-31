*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_BRANCH_OVERVIEW
*&---------------------------------------------------------------------*

CLASS lcl_branch_overview DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_create,
             name   TYPE string,
             parent TYPE string,
           END OF ty_create.

    TYPES: BEGIN OF ty_commit,
             sha1       TYPE ty_sha1,
             parent1    TYPE ty_sha1,
             parent2    TYPE ty_sha1,
             author     TYPE string,
             email      TYPE string,
             time       TYPE string,
             message    TYPE string,
             branch     TYPE string,
             merge      TYPE string,
             create     TYPE STANDARD TABLE OF ty_create WITH DEFAULT KEY,
             compressed TYPE abap_bool,
           END OF ty_commit.

    TYPES: ty_commit_tt TYPE STANDARD TABLE OF ty_commit WITH DEFAULT KEY.

    CLASS-METHODS: run
      IMPORTING io_repo           TYPE REF TO lcl_repo_online
      RETURNING VALUE(rt_commits) TYPE ty_commit_tt
      RAISING   lcx_exception.

    CLASS-METHODS: compress
      IMPORTING it_commits        TYPE ty_commit_tt
      RETURNING VALUE(rt_commits) TYPE ty_commit_tt
      RAISING   lcx_exception.

    CLASS-METHODS: get_branches
      RETURNING VALUE(rt_branches) TYPE lcl_git_branch_list=>ty_git_branch_list_tt.

  PRIVATE SECTION.

    CLASS-METHODS:
      parse_commits
        IMPORTING it_objects TYPE ty_objects_tt
        RAISING   lcx_exception,
      determine_branch
        RAISING lcx_exception,
      determine_merges
        RAISING lcx_exception,
      fixes
        RAISING lcx_exception,
      get_git_objects
        IMPORTING io_repo           TYPE REF TO lcl_repo_online
        RETURNING VALUE(rt_objects) TYPE ty_objects_tt
        RAISING   lcx_exception.

    CLASS-DATA:
      gt_branches TYPE lcl_git_branch_list=>ty_git_branch_list_tt,
      gt_commits  TYPE TABLE OF ty_commit.

ENDCLASS.

CLASS lcl_branch_overview IMPLEMENTATION.

  METHOD get_branches.
    rt_branches = gt_branches.
  ENDMETHOD.

  METHOD compress.

    DEFINE _compress.
      IF lines( lt_temp ) >= 10.
        READ TABLE lt_temp ASSIGNING <ls_temp> INDEX 1.
        ASSERT sy-subrc = 0.
        APPEND INITIAL LINE TO rt_commits ASSIGNING <ls_new>.
        <ls_new>-time       = <ls_temp>-time.
        <ls_new>-message    = |Compressed, { lines( lt_temp ) } commits|.
        <ls_new>-branch     = lv_name.
        <ls_new>-compressed = abap_true.
      ELSE.
        APPEND LINES OF lt_temp TO rt_commits.
      ENDIF.
    END-OF-DEFINITION.

    DATA: lv_previous TYPE i,
          lv_index    TYPE i,
          lv_name     TYPE string,
          lt_temp     LIKE it_commits.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF gt_branches,
                   <ls_new>    LIKE LINE OF rt_commits,
                   <ls_temp>   LIKE LINE OF lt_temp,
                   <ls_commit> LIKE LINE OF it_commits.


    LOOP AT gt_branches ASSIGNING <ls_branch>.

      CLEAR lt_temp.
      lv_name = <ls_branch>-name+11.

      LOOP AT it_commits ASSIGNING <ls_commit>
          WHERE branch = lv_name.
        lv_index = sy-tabix.

        IF NOT <ls_commit>-merge IS INITIAL
            OR NOT <ls_commit>-create IS INITIAL.
* always show these vertices
          lv_previous = -1.
        ENDIF.

        IF lv_previous + 1 <> sy-tabix.
          _compress.
          CLEAR lt_temp.
        ENDIF.

        lv_previous = lv_index.

        APPEND <ls_commit> TO lt_temp.

      ENDLOOP.

      _compress.

    ENDLOOP.

    SORT rt_commits BY time ASCENDING.

  ENDMETHOD.

  METHOD run.

    DATA: lt_objects TYPE ty_objects_tt.


    CLEAR gt_branches.
    CLEAR gt_commits.

    lt_objects = get_git_objects( io_repo ).
    parse_commits( lt_objects ).
    CLEAR lt_objects.

    determine_branch( ).
    determine_merges( ).
    fixes( ).

    SORT gt_commits BY time ASCENDING.

    rt_commits = gt_commits.

  ENDMETHOD.

  METHOD get_git_objects.

    lcl_progress=>show( iv_key     = 'Get git objects'
                        iv_current = 1
                        iv_total   = 1
                        iv_text    = io_repo->get_name( ) ) ##NO_TEXT.

* get objects directly from git, mo_repo only contains a shallow clone of only
* the selected branch

    "TODO refactor
    gt_branches = lcl_git_transport=>branches( io_repo->get_url( ) )->get_branches_only( ).

    lcl_git_transport=>upload_pack( EXPORTING io_repo = io_repo
                                              iv_deepen = abap_false
                                              it_branches = gt_branches
                                    IMPORTING et_objects = rt_objects ).

    DELETE rt_objects WHERE type = gc_type-blob.

  ENDMETHOD.

  METHOD parse_commits.

    DATA: ls_commit LIKE LINE OF gt_commits,
          lv_trash  TYPE string ##NEEDED,
          ls_raw    TYPE lcl_git_pack=>ty_commit.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF it_objects.


    LOOP AT it_objects ASSIGNING <ls_object> WHERE type = gc_type-commit.
      ls_raw = lcl_git_pack=>decode_commit( <ls_object>-data ).

      CLEAR ls_commit.
      ls_commit-sha1 = <ls_object>-sha1.
      ls_commit-parent1 = ls_raw-parent.
      ls_commit-parent2 = ls_raw-parent2.

      SPLIT ls_raw-body AT gc_newline INTO ls_commit-message lv_trash.

* unix time stamps are in same time zone, so ignore the zone,
      FIND REGEX gc_author_regex IN ls_raw-author
        SUBMATCHES
        ls_commit-author
        ls_commit-email
        ls_commit-time ##NO_TEXT.
      ASSERT sy-subrc = 0.
      APPEND ls_commit TO gt_commits.

    ENDLOOP.

  ENDMETHOD.

  METHOD fixes.

    FIELD-SYMBOLS: <ls_commit> LIKE LINE OF gt_commits.


    LOOP AT gt_commits ASSIGNING <ls_commit> WHERE NOT merge IS INITIAL.
* commits from old branches
      IF <ls_commit>-branch = <ls_commit>-merge.
        CLEAR <ls_commit>-merge.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD determine_merges.

    FIELD-SYMBOLS: <ls_merged> LIKE LINE OF gt_commits,
                   <ls_commit> LIKE LINE OF gt_commits.


* important: start with the newest first and propagate branches
    SORT gt_commits BY time DESCENDING.

    LOOP AT gt_commits ASSIGNING <ls_commit> WHERE NOT parent2 IS INITIAL.
      ASSERT NOT <ls_commit>-branch IS INITIAL.

      READ TABLE gt_commits ASSIGNING <ls_merged> WITH KEY sha1 = <ls_commit>-parent2.
      IF sy-subrc = 0.
        <ls_commit>-merge = <ls_merged>-branch.

* orphaned, branch has been deleted after merge
        WHILE <ls_merged>-branch IS INITIAL.
          <ls_merged>-branch = <ls_commit>-branch.
          READ TABLE gt_commits ASSIGNING <ls_merged> WITH KEY sha1 = <ls_merged>-parent1.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
        ENDWHILE.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD determine_branch.

    CONSTANTS: lc_head TYPE string VALUE 'HEAD'.

    DATA: lv_name TYPE string.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF gt_branches,
                   <ls_head>   LIKE LINE OF gt_branches,
                   <ls_commit> LIKE LINE OF gt_commits,
                   <ls_create> LIKE LINE OF <ls_commit>-create.


* exchange HEAD, and make sure the branch determination starts with the HEAD branch
    READ TABLE gt_branches ASSIGNING <ls_head> WITH KEY name = lc_head.
    ASSERT sy-subrc = 0.
    LOOP AT gt_branches ASSIGNING <ls_branch>
        WHERE sha1 = <ls_head>-sha1 AND name <> lc_head.
      <ls_head>-name = <ls_branch>-name.
      DELETE gt_branches INDEX sy-tabix.
      EXIT.
    ENDLOOP.

    LOOP AT gt_branches ASSIGNING <ls_branch>.
      lv_name = <ls_branch>-name+11.
      READ TABLE gt_commits ASSIGNING <ls_commit> WITH KEY sha1 = <ls_branch>-sha1.
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

        IF <ls_commit>-parent1 IS INITIAL.
          EXIT.
        ELSE.
          READ TABLE gt_commits ASSIGNING <ls_commit>
              WITH KEY sha1 = <ls_commit>-parent1.
          ASSERT sy-subrc = 0.
        ENDIF.
      ENDDO.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

***********************

CLASS lcl_gui_page_branch_overview DEFINITION FINAL INHERITING FROM lcl_gui_page.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_repo TYPE REF TO lcl_repo_online
        RAISING   lcx_exception,
      lif_gui_page~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo     TYPE REF TO lcl_repo_online,
          mv_compress TYPE abap_bool VALUE abap_false,
          mt_commits  TYPE lcl_branch_overview=>ty_commit_tt.

    CONSTANTS: BEGIN OF c_actions,
                 uncompress TYPE string VALUE 'uncompress' ##NO_TEXT,
                 compress   TYPE string VALUE 'compress' ##NO_TEXT,
                 refresh    TYPE string VALUE 'refresh' ##NO_TEXT,
                 merge      TYPE string VALUE 'merge' ##NO_TEXT,
               END OF c_actions.

    TYPES: BEGIN OF ty_merge,
             source TYPE string,
             target TYPE string,
           END OF ty_merge.

    METHODS:
      refresh
        RAISING lcx_exception,
      body
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html
        RAISING   lcx_exception,
      form_select
        IMPORTING iv_name        TYPE string
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html,
      render_merge
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html
        RAISING   lcx_exception,
      decode_merge
        IMPORTING it_postdata     TYPE cnht_post_data_tab
        RETURNING VALUE(rs_merge) TYPE ty_merge
        RAISING   lcx_exception,
      build_menu
        RETURNING VALUE(ro_menu) TYPE REF TO lcl_html_toolbar,
      escape_branch
        IMPORTING iv_string        TYPE string
        RETURNING VALUE(rv_string) TYPE string,
      escape_message
        IMPORTING iv_string        TYPE string
        RETURNING VALUE(rv_string) TYPE string.

ENDCLASS.                       "lcl_gui_page_explore DEFINITION

CLASS lcl_gui_page_branch_overview IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'BRANCH_OVERVIEW'.
    mo_repo = io_repo.
    refresh( ).
  ENDMETHOD.

  METHOD refresh.

    mt_commits = lcl_branch_overview=>run( mo_repo ).
    IF mv_compress = abap_true.
      mt_commits = lcl_branch_overview=>compress( mt_commits ).
    ENDIF.

  ENDMETHOD.

  METHOD form_select.

    DATA: lv_name     TYPE string,
          lt_branches TYPE lcl_git_branch_list=>ty_git_branch_list_tt.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF lt_branches.


    CREATE OBJECT ro_html.

    lt_branches = lcl_branch_overview=>get_branches( ).

    ro_html->add( |<select name="{ iv_name }">| ).
    LOOP AT lt_branches ASSIGNING <ls_branch>.
      lv_name = <ls_branch>-name+11.
      ro_html->add( |<option value="{ lv_name }">{ lv_name }</option>| ).
    ENDLOOP.
    ro_html->add( '</select>' ).

  ENDMETHOD.

  METHOD render_merge.

    CREATE OBJECT ro_html.

    ro_html->add( '<form id="commit_form" method="post" action="sapevent:merge">' ).
    ro_html->add( 'Merge' ) ##NO_TEXT.
    ro_html->add( form_select( 'source' ) ) ##NO_TEXT.
    ro_html->add( 'into' ) ##NO_TEXT.
    ro_html->add( form_select( 'target' ) ) ##NO_TEXT.
    ro_html->add( '<input type="submit" value="Submit">' ).
    ro_html->add( '</form>' ).

  ENDMETHOD.

  METHOD body.

    FIELD-SYMBOLS: <ls_commit> LIKE LINE OF mt_commits,
                   <ls_create> LIKE LINE OF <ls_commit>-create.


    CREATE OBJECT ro_html.

    ro_html->add( lcl_gui_chunk_lib=>render_repo_top(
      io_repo         = mo_repo
      iv_show_package = abap_false
      iv_show_branch  = abap_false ) ).
    ro_html->add( '<br>' ).
    ro_html->add( '<br>' ).

    ro_html->add( render_merge( ) ).

    ro_html->add( '<br>' ).
    ro_html->add( build_menu( )->render( ) ).

* see http://stackoverflow.com/questions/6081483/maximum-size-of-a-canvas-element
    _add '<canvas id="gitGraph"></canvas>'.

    ro_html->add( '<script type="text/javascript" src="https://cdnjs.' &&
      'cloudflare.com/ajax/libs/gitgraph.js/1.2.3/gitgraph.min.js">' &&
      '</script>' ) ##NO_TEXT.

    _add '<script type="text/javascript">'.
    _add 'var myTemplateConfig = {'.
    ro_html->add( 'colors: [ "#979797", "#008fb5", "#f1c109", "'
      && '#095256", "#087F8C", "#5AAA95", "#86A873", "#BB9F06" ],' ) ##NO_TEXT.
    _add 'branch: {'.
    _add '  lineWidth: 8,'.
    _add '  spacingX: 50'.
    _add '},'.
    _add 'commit: {'.
    _add '  spacingY: -40,'.
    _add '  dot: { size: 12 },'.
    _add '  message: { font: "normal 14pt Arial" }'.
    _add '}'.
    _add '};'.
    _add 'var gitgraph = new GitGraph({'.
    _add '  template: myTemplateConfig,'.
    _add '  orientation: "vertical-reverse"'.
    _add '});'.

    LOOP AT mt_commits ASSIGNING <ls_commit>.
      IF sy-tabix = 1.
* assumption: all branches are created from master, todo
        ro_html->add( |var {
          escape_branch( <ls_commit>-branch ) } = gitgraph.branch("{
          <ls_commit>-branch }");| ).
      ENDIF.

      IF <ls_commit>-compressed = abap_true.
        ro_html->add( |{ escape_branch( <ls_commit>-branch ) }.commit(\{message: "{
          escape_message( <ls_commit>-message )
          }", dotColor: "black", dotSize: 15, messageHashDisplay: false, messageAuthorDisplay: false\});| ).
      ELSEIF <ls_commit>-merge IS INITIAL.
        ro_html->add( |{ escape_branch( <ls_commit>-branch ) }.commit(\{message: "{
          escape_message( <ls_commit>-message ) }", author: "{
          <ls_commit>-author }", sha1: "{
          <ls_commit>-sha1(7) }"\});| ).
      ELSE.
        ro_html->add( |{ escape_branch( <ls_commit>-merge ) }.merge({
          escape_branch( <ls_commit>-branch ) }, \{message: "{
          escape_message( <ls_commit>-message ) }", author: "{
          <ls_commit>-author }", sha1: "{
          <ls_commit>-sha1(7) }"\});| ).
      ENDIF.

      LOOP AT <ls_commit>-create ASSIGNING <ls_create>.
        ro_html->add( |var { escape_branch( <ls_create>-name ) } = {
          escape_branch( <ls_create>-parent ) }.branch("{
          <ls_create>-name }");| ).
      ENDLOOP.

    ENDLOOP.

    _add '</script>'.

  ENDMETHOD.

  METHOD escape_message.

    rv_string = iv_string.

    REPLACE ALL OCCURRENCES OF '"' IN rv_string WITH '\"'.

  ENDMETHOD.

  METHOD escape_branch.

    rv_string = iv_string.

    TRANSLATE rv_string USING '-_._'.

    rv_string = |branch_{ rv_string }|.

  ENDMETHOD.

  METHOD build_menu.

    CREATE OBJECT ro_menu.

    IF mv_compress = abap_true.
      ro_menu->add(
        iv_txt = 'Uncompress Graph'
        iv_act = c_actions-uncompress ) ##NO_TEXT.
    ELSE.
      ro_menu->add(
        iv_txt = 'Compress Graph'
        iv_act = c_actions-compress ) ##NO_TEXT.
    ENDIF.

    ro_menu->add( iv_txt = 'Refresh' iv_act = c_actions-refresh ) ##NO_TEXT.

  ENDMETHOD.

  METHOD decode_merge.

    DATA: lv_string TYPE string,
          lt_fields TYPE tihttpnvp.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    CONCATENATE LINES OF it_postdata INTO lv_string.

    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'source' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_merge-source = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'target' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    rs_merge-target = <ls_field>-value.

  ENDMETHOD.

  METHOD lif_gui_page~on_event.

    DATA: ls_merge TYPE ty_merge,
          lo_merge TYPE REF TO lcl_gui_page_merge.


    CASE iv_action.
      WHEN c_actions-refresh.
        refresh( ).
        ev_state = gc_event_state-re_render.
      WHEN c_actions-uncompress.
        mv_compress = abap_false.
        refresh( ).
        ev_state = gc_event_state-re_render.
      WHEN c_actions-compress.
        mv_compress = abap_true.
        refresh( ).
        ev_state = gc_event_state-re_render.
      WHEN c_actions-merge.
        ls_merge = decode_merge( it_postdata ).
        CREATE OBJECT lo_merge
          EXPORTING
            io_repo   = mo_repo
            iv_source = ls_merge-source
            iv_target = ls_merge-target.
        ei_page = lo_merge.
        ev_state = gc_event_state-new_page.
    ENDCASE.

  ENDMETHOD.

  METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="toc">' ).
    ro_html->add( body( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_content

ENDCLASS.
