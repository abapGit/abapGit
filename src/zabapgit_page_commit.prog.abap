*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_COMMIT
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_commit DEFINITION FINAL INHERITING FROM lcl_gui_page.

  PUBLIC SECTION.

    CONSTANTS: BEGIN OF c_action,
                 commit_post   TYPE string VALUE 'commit_post',
                 commit_cancel TYPE string VALUE 'commit_cancel',
               END OF c_action.

    METHODS:
      constructor
        IMPORTING io_repo  TYPE REF TO lcl_repo_online
                  io_stage TYPE REF TO lcl_stage
        RAISING   lcx_exception,
      lif_gui_page~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION,
      scripts        REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo  TYPE REF TO lcl_repo_online,
          mo_stage TYPE REF TO lcl_stage.

    METHODS:
      render_menu
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html,
      render_stage
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html
        RAISING   lcx_exception,
      render_form
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html
        RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_gui_page_commit IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    mo_repo   = io_repo.
    mo_stage  = io_stage.

    ms_control-page_title = 'COMMIT'.
  ENDMETHOD.

  METHOD lif_gui_page~on_event.

    DATA: ls_commit TYPE lcl_services_git=>ty_commit_fields.

    CASE iv_action.
      WHEN c_action-commit_post.

        lcl_html_action_utils=>parse_commit_request( EXPORTING it_postdata = it_postdata
                                                     IMPORTING es_fields   = ls_commit ).

        lcl_services_git=>commit( is_commit   = ls_commit
                                  io_repo     = mo_repo
                                  io_stage    = mo_stage ).

        ev_state = gc_event_state-go_back_to_bookmark.

      WHEN c_action-commit_cancel.
        ev_state = gc_event_state-go_back.
    ENDCASE.

  ENDMETHOD.

  METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="repo">' ).
    ro_html->add( lcl_gui_chunk_lib=>render_repo_top(
      io_repo         = mo_repo
      iv_show_package = abap_false
      iv_branch       = mo_stage->get_branch_name( ) ) ).

    ro_html->add( render_menu( ) ).
    ro_html->add( render_form( ) ).
    ro_html->add( render_stage( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_content

  METHOD render_stage.

    DATA: lt_stage TYPE lcl_stage=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage.


    CREATE OBJECT ro_html.

    lt_stage = mo_stage->get_all( ).

    ro_html->add( '<table class="stage_tab">' ).
    ro_html->add( '<thead>' ).
    ro_html->add( '<tr>').
    ro_html->add( '<th colspan="2">Staged files</th>').
    ro_html->add( '</tr>' ).
    ro_html->add( '</thead>' ).

    ro_html->add( '<tbody>' ).
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
    ro_html->add( '</tbody>' ).

    ro_html->add( '</table>' ).

  ENDMETHOD.    "render_stage

  METHOD render_form.

    DATA: lo_user  TYPE REF TO lcl_persistence_user,
          lv_user  TYPE string,
          lv_key   TYPE lcl_persistence_db=>ty_value,
          lv_email TYPE string.

* see https://git-scm.com/book/ch5-2.html
* commit messages should be max 50 characters
* body should wrap at 72 characters

    lo_user  = lcl_app=>user( ).
    lv_key   = mo_repo->get_key( ).

    lv_user  = lo_user->get_repo_username( mo_repo->get_url( ) ).
    IF lv_user IS INITIAL.
      lv_user  = lo_user->get_username( ).
    ENDIF.

    lv_email = lo_user->get_repo_email( mo_repo->get_url( ) ).
    IF lv_email IS INITIAL.
      lv_email = lo_user->get_email( ).
    ENDIF.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="form_div">' ).
    ro_html->add( '<form id="commit_form" method="post" action="sapevent:commit_post">' ).
    ro_html->add( |<input name="repo_key" type="hidden" value="{ lv_key }">| ).
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

    lo_toolbar->add( iv_act = 'submitFormById(''commit_form'');'
                     iv_txt = 'Commit'
                     iv_typ = gc_action_type-onclick
                     iv_opt = gc_html_opt-strong ) ##NO_TEXT.

    lo_toolbar->add( iv_act = 'commit_cancel'
                     iv_txt = 'Cancel'
                     iv_opt = gc_html_opt-cancel ) ##NO_TEXT.

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.      "render_menu

  METHOD scripts.

    CREATE OBJECT ro_html.
    _add 'setInitialFocus("commit_msg");'.

  ENDMETHOD.    "scripts

ENDCLASS.       "lcl_gui_page_commit
