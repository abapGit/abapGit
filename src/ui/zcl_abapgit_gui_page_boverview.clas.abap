CLASS zcl_abapgit_gui_page_boverview DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING io_repo TYPE REF TO zcl_abapgit_repo_online
        RAISING   zcx_abapgit_exception,
      zif_abapgit_gui_event_handler~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_merge,
        source TYPE string,
        target TYPE string,
      END OF ty_merge .

    DATA mo_repo TYPE REF TO zcl_abapgit_repo_online .
    DATA mv_compress TYPE abap_bool VALUE abap_false ##NO_TEXT.
    DATA mt_commits TYPE zif_abapgit_definitions=>ty_commit_tt .
    DATA mi_branch_overview TYPE REF TO zif_abapgit_branch_overview .
    CONSTANTS:
      BEGIN OF c_actions,
        uncompress TYPE string VALUE 'uncompress' ##NO_TEXT,
        compress   TYPE string VALUE 'compress' ##NO_TEXT,
        refresh    TYPE string VALUE 'refresh' ##NO_TEXT,
        merge      TYPE string VALUE 'merge' ##NO_TEXT,
      END OF c_actions .

    METHODS refresh
      RAISING
        zcx_abapgit_exception .
    METHODS body
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS form_select
      IMPORTING
        !iv_name       TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_merge
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS build_menu
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar .
    METHODS escape_branch
      IMPORTING
        !iv_string       TYPE string
      RETURNING
        VALUE(rv_string) TYPE string .
    METHODS escape_message
      IMPORTING
        !iv_string       TYPE string
      RETURNING
        VALUE(rv_string) TYPE string .
    METHODS render_commit_popups
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_BOVERVIEW IMPLEMENTATION.


  METHOD body.

    DATA: lv_tag                 TYPE string,
          lv_branch_display_name TYPE string.

    FIELD-SYMBOLS: <ls_commit> LIKE LINE OF mt_commits,
                   <ls_create> LIKE LINE OF <ls_commit>-create.


    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
      io_repo         = mo_repo
      iv_show_package = abap_false
      iv_show_branch  = abap_false ) ).
    ri_html->add( '<br>' ).
    ri_html->add( '<br>' ).

    ri_html->add( render_merge( ) ).

    ri_html->add( '<br>' ).
    ri_html->add( build_menu( )->render( ) ).


    "CSS gitGraph-scrollWrapper, gitGraph-HTopScroller and gitGraph-Wrapper
    " - Used to manage the Horizonal Scroll bar on top of gitGraph Element
    ri_html->add( '<div class="gitGraph-scrollWrapper" onscroll="GitGraphScroller()">' ).
    "see http://stackoverflow.com/questions/6081483/maximum-size-of-a-canvas-element
    ri_html->add( '<div class="gitGraph-HTopScroller"></div>' ).
    ri_html->add( '</div>' ).

    ri_html->add( '<div class="gitGraph-Wrapper">' ).
    ri_html->add( '<canvas id="gitGraph"></canvas>' ).
    ri_html->add( '</div>' ).

    ri_html->add( '<script type="text/javascript" src="https://cdnjs.' &&
      'cloudflare.com/ajax/libs/gitgraph.js/1.14.0/gitgraph.min.js">' &&
      '</script>' ).

    ri_html->add( '<script type="text/javascript">' ).
    ri_html->add( 'var myTemplateConfig = {' ).
    ri_html->add( 'colors: [ "#979797", "#008fb5", "#f1c109", "'
      && '#095256", "#087F8C", "#5AAA95", "#86A873", "#BB9F06" ],' ).
    ri_html->add( 'branch: {' ).
    ri_html->add( '  lineWidth: 8,' ).
    ri_html->add( '  spacingX: 50' ).
    ri_html->add( '},' ).
    ri_html->add( 'commit: {' ).
    ri_html->add( '  spacingY: -40,' ).
    ri_html->add( '  dot: { size: 12 },' ).
    ri_html->add( '  message: { font: "normal 14pt Arial" }' ).
    ri_html->add( '}' ).
    ri_html->add( '};' ).
    ri_html->add( 'var gitgraph = new GitGraph({' ).
    ri_html->add( '  template: myTemplateConfig,' ).
    ri_html->add( '  orientation: "vertical-reverse"' ).
    ri_html->add( '});' ).
    ri_html->add( 'var gBranchOveriew = new BranchOverview();' ).

    LOOP AT mt_commits ASSIGNING <ls_commit>.

      IF sy-tabix = 1.
        " assumption: all branches are created from master, todo
        ri_html->add( |var {
          escape_branch( <ls_commit>-branch ) } = gitgraph.branch("{
          <ls_commit>-branch }");| ).
      ENDIF.

      IF <ls_commit>-branch IS INITIAL.
        CONTINUE. " we skip orphaned commits
      ENDIF.

      IF <ls_commit>-compressed = abap_true.
        ri_html->add( |{ escape_branch( <ls_commit>-branch ) }.commit(\{message: "{
          escape_message( <ls_commit>-message )
          }", dotColor: "black", dotSize: 15, messageHashDisplay: false, messageAuthorDisplay: false\});| ).
      ELSEIF <ls_commit>-merge IS INITIAL.

        " gitgraph doesn't support multiple tags per commit yet.
        " Therefore we concatenate them.
        " https://github.com/nicoespeon/gitgraph.js/issues/143

        lv_tag = concat_lines_of( table = <ls_commit>-tags
                                  sep   = ` | ` ).

        ri_html->add( |{ escape_branch( <ls_commit>-branch ) }.commit(\{message: "{
          escape_message( <ls_commit>-message ) }", long: "{ escape_message( concat_lines_of( table = <ls_commit>-body
                                                                                              sep   = ` ` ) )
          }", author: "{
          <ls_commit>-author }", sha1: "{
          <ls_commit>-sha1(7) }", tag: "{ lv_tag
          }", onClick:gBranchOveriew.onCommitClick.bind(gBranchOveriew)\});| ).
      ELSE.
        ri_html->add( |{ escape_branch( <ls_commit>-merge ) }.merge({
          escape_branch( <ls_commit>-branch ) }, \{message: "{
          escape_message( <ls_commit>-message ) }", long: "{ escape_message( concat_lines_of( table = <ls_commit>-body
                                                                                              sep   = ` ` ) )
          }", author: "{ <ls_commit>-author }", sha1: "{
          <ls_commit>-sha1(7) }", onClick:gBranchOveriew.onCommitClick.bind(gBranchOveriew)\});| ).
      ENDIF.

      LOOP AT <ls_commit>-create ASSIGNING <ls_create>.
        IF <ls_create>-name CS zcl_abapgit_branch_overview=>c_deleted_branch_name_prefix.
          lv_branch_display_name = ''.
        ELSE.
          lv_branch_display_name = <ls_create>-name.
        ENDIF.

        ri_html->add( |var { escape_branch( <ls_create>-name ) } = {
                      escape_branch( <ls_create>-parent ) }.branch("{
                      lv_branch_display_name }");| ).
      ENDLOOP.

    ENDLOOP.

    ri_html->add(
       |gitGraph.addEventListener( "commit:mouseover", gBranchOveriew.showCommit.bind(gBranchOveriew) );| ).
    ri_html->add(
       |gitGraph.addEventListener( "commit:mouseout",  gBranchOveriew.hideCommit.bind(gBranchOveriew) );| ).

    ri_html->add( '</script>' ).

    ri_html->add( '<script>' ).
    ri_html->add( 'setGitGraphScroller();' ).
    ri_html->add( '</script>' ).

    ri_html->add( render_commit_popups( ) ).

  ENDMETHOD.


  METHOD build_menu.

    CREATE OBJECT ro_menu.

    IF mv_compress = abap_true.
      ro_menu->add(
        iv_txt = 'Uncompress Graph'
        iv_act = c_actions-uncompress ).
    ELSE.
      ro_menu->add(
        iv_txt = 'Compress Graph'
        iv_act = c_actions-compress ).
    ENDIF.

    ro_menu->add( iv_txt = 'Refresh'
                  iv_act = c_actions-refresh ).

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'Branch Overview'.
    mo_repo = io_repo.
    refresh( ).
  ENDMETHOD.


  METHOD escape_branch.

    rv_string = iv_string.

    TRANSLATE rv_string USING '-_._#_'.

    rv_string = |branch_{ rv_string }|.

  ENDMETHOD.


  METHOD escape_message.

    rv_string = iv_string.

    REPLACE ALL OCCURRENCES OF '\' IN rv_string WITH '\\'.
    REPLACE ALL OCCURRENCES OF '"' IN rv_string WITH '\"'.

  ENDMETHOD.


  METHOD form_select.

    DATA: lv_name     TYPE string,
          lt_branches TYPE zif_abapgit_definitions=>ty_git_branch_list_tt.

    FIELD-SYMBOLS: <ls_branch> LIKE LINE OF lt_branches.


    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    lt_branches = mi_branch_overview->get_branches( ).

    ri_html->add( |<select name="{ iv_name }">| ).
    LOOP AT lt_branches ASSIGNING <ls_branch>.
      lv_name = <ls_branch>-name+11.
      ri_html->add( |<option value="{ lv_name }">{ lv_name }</option>| ).
    ENDLOOP.
    ri_html->add( '</select>' ).

  ENDMETHOD.


  METHOD refresh.

    mi_branch_overview = zcl_abapgit_factory=>get_branch_overview( mo_repo ).

    mt_commits = mi_branch_overview->get_commits( ).
    IF mv_compress = abap_true.
      mt_commits = mi_branch_overview->compress( mt_commits ).
    ENDIF.

  ENDMETHOD.


  METHOD render_commit_popups.

    DATA: lv_time    TYPE c LENGTH 10,
          lv_date    TYPE sy-datum,
          lv_content TYPE string.

    FIELD-SYMBOLS: <ls_commit> LIKE LINE OF mt_commits.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    LOOP AT mt_commits ASSIGNING <ls_commit>.

      CLEAR: lv_time, lv_date.

      PERFORM p6_to_date_time_tz IN PROGRAM rstr0400
                                 USING <ls_commit>-time
                                       lv_time
                                       lv_date.

      lv_content = |<table class="commit">|
                && |  <tr>|
                && |    <td class="title">Author</td>|
                && |    <td>{ <ls_commit>-author }</td>|
                && |  </tr>|
                && |  <tr>|
                && |    <td class="title">SHA1</td>|
                && |    <td>{ <ls_commit>-sha1 }</td>|
                && |  </tr>|
                && |  <tr>|
                && |    <td class="title">Date/Time</td>|
                && |    <td>{ lv_date DATE = USER }</td>|
                && |  </tr>|
                && |  <tr>|
                && |    <td class="title">Message</td>|
                && |    <td>{ <ls_commit>-message }</td>|
                && |  </tr>|
                && |  <tr>|.

      IF <ls_commit>-body IS NOT INITIAL.
        lv_content = lv_content
                  && |<td class="title">Body</td>|
                  && |<td>{ concat_lines_of( table = <ls_commit>-body
                                             sep   = |<br/>| ) }</td>|.
      ENDIF.

      lv_content = lv_content
                && |  </tr>|
                && |</table>|.

      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_commit_popup(
        iv_id      = <ls_commit>-sha1(7)
        iv_content = lv_content ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div id="toc">' ).
    ri_html->add( body( ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_merge.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<form id="commit_form" method="post" action="sapevent:merge">' ).
    ri_html->add( 'Merge' ).
    ri_html->add( form_select( 'source' ) ).
    ri_html->add( 'into' ).
    ri_html->add( form_select( 'target' ) ).
    ri_html->add( '<input type="submit" value="Submit">' ).
    ri_html->add( '</form>' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA: ls_merge TYPE ty_merge,
          lo_merge TYPE REF TO zcl_abapgit_gui_page_merge.


    CASE ii_event->mv_action.
      WHEN c_actions-refresh.
        refresh( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_actions-uncompress.
        mv_compress = abap_false.
        refresh( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_actions-compress.
        mv_compress = abap_true.
        refresh( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_actions-merge.
        ls_merge-source = ii_event->form_data( )->get( 'source' ).
        ls_merge-target = ii_event->form_data( )->get( 'target' ).
        CREATE OBJECT lo_merge
          EXPORTING
            io_repo   = mo_repo
            iv_source = ls_merge-source
            iv_target = ls_merge-target.
        rs_handled-page = lo_merge.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN OTHERS.
        rs_handled = super->zif_abapgit_gui_event_handler~on_event( ii_event ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
