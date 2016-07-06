*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_COMMIT
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_commit DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_repo_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS lif_gui_page~render REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo  TYPE REF TO lcl_repo_online,
          mo_stage TYPE REF TO lcl_stage.

    METHODS render_menu
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS render_stage
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS render_form
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS scripts
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_commit IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    mo_repo ?= lcl_app=>repo_srv( )->get( iv_repo_key ).
    mo_stage = lcl_app=>repo_srv( )->get_stage( iv_repo_key ).
  ENDMETHOD.

  METHOD render_stage.

    DATA: lt_stage TYPE lcl_stage=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage.

    CREATE OBJECT ro_html.

    lt_stage = mo_stage->get_all( ).

    ro_html->add( '<table class="stage_tab">' ).
    ro_html->add( '<tr class="title firstrow">').
    ro_html->add( '<td colspan="2">Staged files</td>').
    ro_html->add( '</tr>' ).

    LOOP AT lt_stage ASSIGNING <ls_stage>.
      ro_html->add( '<tr>' ).
      ro_html->add( '<td class="method">' ).
      ro_html->add( lcl_stage=>method_description( <ls_stage>-method ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '<td>' ).
      ro_html->add( <ls_stage>-file-path && <ls_stage>-file-filename ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.

    ro_html->add( '</table>' ).

  ENDMETHOD.    "render_stage

  METHOD render_form.
    DATA: lo_user  TYPE REF TO lcl_persistence_user,
          lv_user  TYPE string,
          lv_key   TYPE string,
          lv_email TYPE string.

* see https://git-scm.com/book/ch5-2.html
* commit messages should be max 50 characters
* body should wrap at 72 characters

    lo_user  = lcl_app=>user( ).
    lv_user  = lo_user->get_username( ).
    lv_email = lo_user->get_email( ).
    lv_key   = mo_repo->get_key( ).

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="form_div">' ).
    ro_html->add( '<form id="commit_form" method="post" action="sapevent:commit_post">' ).
    ro_html->add( |<input name="key" type="hidden" value="{ lv_key }">| ).
    ro_html->add( '<table>' ).

    ro_html->add( '<tr>' ).
    ro_html->add( '<td class="field_name">username</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( |<input name="username" type="text" size="50" value="{ lv_user }">| ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).

    ro_html->add( '<tr>' ).
    ro_html->add( '<td class="field_name">email</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( |<input name="email" type="text" size="50" value="{ lv_email }">| ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).

    ro_html->add( '<tr>' ).
    ro_html->add( '<td class="field_name">comment</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add(
      '<input name="comment" type="text" id="commit_msg" maxlength="50" size="50">' ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).

    ro_html->add( '<tr>' ).
    ro_html->add( '<td class="field_name">body</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( '<textarea name="body" rows="10" cols="50"></textarea>' ).

    ro_html->add( '<input type="submit" class="hidden-submit">' ). "Hmmm ... reconsider

    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).

    ro_html->add( '</table>' ).
    ro_html->add( '</form>' ).

    ro_html->add( '</div>' ).

  ENDMETHOD.    "render_form

  METHOD render_menu.

    DATA lo_toolbar TYPE REF TO lcl_html_toolbar.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    lo_toolbar->add( iv_act = 'submitCommit();'
                     iv_txt = 'Commit'
                     iv_typ = gc_action_type-onclick
                     iv_opt = gc_html_opt-emphas ) ##NO_TEXT.

    lo_toolbar->add( iv_act = 'commit_cancel'
                     iv_txt = 'Cancel'
                     iv_opt = gc_html_opt-cancel ) ##NO_TEXT.

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.      "render_menu

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'COMMIT' ) ).

    ro_html->add( '<div class="repo">' ).
    ro_html->add( render_repo_top( mo_repo ) ).
    ro_html->add( render_menu( ) ).
    ro_html->add( render_form( ) ).
    ro_html->add( render_stage( ) ).
    ro_html->add( '</div>' ).

    ro_html->add( footer( io_include_script = scripts( ) ) ).

  ENDMETHOD.  "lif_gui_page~render

  METHOD styles.

    CREATE OBJECT ro_html.

    _add '/* STAGE */'.
    _add '.stage_tab {'.
    _add '  border: 1px solid #DDD;'.
    _add '  background: #fff;'.
    _add '  margin-top: 0.2em;'.
    _add '}'.
    _add '.stage_tab td {'.
    _add '  border-top: 1px solid #eee;'.
    _add '  color: #333;'.
    _add '  vertical-align: middle;'.
    _add '  padding: 2px 0.5em;'.
    _add '}'.
    _add '.stage_tab td.method {'.
    _add '  color: #ccc;'.
    _add '}'.
    _add '.stage_tab tr.firstrow td { border-top: 0px; } ' .
    _add '.stage_tab tr.title td {'.
    _add '  color: #BBB;'.
    _add '  font-size: 10pt;'.
    _add '  background-color: #edf2f9;'.
    _add '  padding: 4px 0.5em;'.
    _add '  text-align: center;'.
    _add '}'.

    _add '/* COMMIT */'.
    _add 'div.form_div {'.
    _add '  margin: 0.5em 0em;'.
    _add '  background-color: #F8F8F8;'.
    _add '  padding: 1em 1em;'.
    _add '}'.
    _add 'div.form_div td.field_name {'.
    _add '  color: #BBB;'.
    _add '  padding-right: 1em;'.
    _add '}'.

  ENDMETHOD.    "styles

  METHOD scripts.

    CREATE OBJECT ro_html.

    _add 'function setInitialFocus() {'.
    _add '  document.getElementById("commit_msg").focus();'.
    _add '}'.
    _add 'function submitCommit() {'.
    _add '  document.getElementById("commit_form").submit();'.
    _add '}'.
    _add 'setInitialFocus();'.

  ENDMETHOD.    "scripts

ENDCLASS.       "lcl_gui_page_commit