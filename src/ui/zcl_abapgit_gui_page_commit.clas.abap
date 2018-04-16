CLASS zcl_abapgit_gui_page_commit DEFINITION PUBLIC FINAL
    CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.

    CONSTANTS: BEGIN OF c_action,
                 commit_post   TYPE string VALUE 'commit_post',
                 commit_cancel TYPE string VALUE 'commit_cancel',
               END OF c_action.

    METHODS:
      constructor
        IMPORTING io_repo  TYPE REF TO zcl_abapgit_repo_online
                  io_stage TYPE REF TO zcl_abapgit_stage
        RAISING   zcx_abapgit_exception,
      zif_abapgit_gui_page~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION,
      scripts        REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo  TYPE REF TO zcl_abapgit_repo_online,
          mo_stage TYPE REF TO zcl_abapgit_stage.

    METHODS:
      render_menu
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html,
      render_stage
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html
        RAISING   zcx_abapgit_exception,
      render_form
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html
        RAISING   zcx_abapgit_exception,
      render_text_input
        IMPORTING iv_name        TYPE string
                  iv_label       TYPE string
                  iv_value       TYPE string OPTIONAL
                  iv_max_length  TYPE string OPTIONAL
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.

ENDCLASS.



CLASS zcl_abapgit_gui_page_commit IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    mo_repo   = io_repo.
    mo_stage  = io_stage.

    ms_control-page_title = 'COMMIT'.
  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="repo">' ).
    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
      io_repo         = mo_repo
      iv_show_package = abap_false
      iv_branch       = mo_stage->get_branch_name( ) ) ).

    ro_html->add( render_menu( ) ).
    ro_html->add( render_form( ) ).
    ro_html->add( render_stage( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_content


  METHOD render_form.

    CONSTANTS: lc_body_col_max TYPE i VALUE 150.

    DATA: lo_user      TYPE REF TO zcl_abapgit_persistence_user.
    DATA: lv_user      TYPE string.
    DATA: lv_email     TYPE string.
    DATA: lv_s_param   TYPE string.
    DATA: lo_settings  TYPE REF TO zcl_abapgit_settings.
    DATA: lv_body_size TYPE i.

* see https://git-scm.com/book/ch5-2.html
* commit messages should be max 50 characters
* body should wrap at 72 characters

    lo_user  = zcl_abapgit_persistence_user=>get_instance( ).

    lv_user  = lo_user->get_repo_git_user_name( mo_repo->get_url( ) ).
    IF lv_user IS INITIAL.
      lv_user  = lo_user->get_default_git_user_name( ).
    ENDIF.
    IF lv_user IS INITIAL.
      " get default from user master record
      lv_user = zcl_abapgit_user_master_record=>get_instance( sy-uname )->get_name( ).
    ENDIF.

    lv_email = lo_user->get_repo_git_user_email( mo_repo->get_url( ) ).
    IF lv_email IS INITIAL.
      lv_email = lo_user->get_default_git_user_email( ).
    ENDIF.
    IF lv_email IS INITIAL.
      " get default from user master record
      lv_email = zcl_abapgit_user_master_record=>get_instance( sy-uname )->get_email( ).
    ENDIF.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="form-container">' ).
    ro_html->add( '<form id="commit_form" class="aligned-form"'
               && ' method="post" action="sapevent:commit_post">' ).

    ro_html->add( render_text_input( iv_name  = 'committer_name'
                                     iv_label = 'committer name'
                                     iv_value = lv_user ) ).

    ro_html->add( render_text_input( iv_name  = 'committer_email'
                                     iv_label = 'committer e-mail'
                                     iv_value = lv_email ) ).

    lo_settings = zcl_abapgit_persist_settings=>get_instance( )->read( ).

    lv_s_param = lo_settings->get_commitmsg_comment_length( ).

    ro_html->add( render_text_input( iv_name       = 'comment'
                                     iv_label      = 'comment'
                                     iv_max_length = lv_s_param ) ).

    ro_html->add( '<div class="row">' ).
    ro_html->add( '<label for="c-body">body</label>' ).

    lv_body_size = lo_settings->get_commitmsg_body_size( ).
    IF lv_body_size > lc_body_col_max.
      lv_body_size = lc_body_col_max.
    ENDIF.
    ro_html->add( |<textarea id="c-body" name="body" rows="10" cols="| &&
                  |{ lv_body_size }"></textarea>| ).

    ro_html->add( '<input type="submit" class="hidden-submit">' ).
    ro_html->add( '</div>' ).

    ro_html->add( '<div class="row">' ).
    ro_html->add( '<span class="cell"></span>' ).
    ro_html->add( '<span class="cell sub-title">Optionally,'
               && ' specify author (same as committer by default)</span>' ).
    ro_html->add( '</div>' ).

    ro_html->add( render_text_input( iv_name  = 'author_name'
                                     iv_label = 'author name' ) ).

    ro_html->add( render_text_input( iv_name  = 'author_email'
                                     iv_label = 'author e-mail' ) ).

    ro_html->add( '</form>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.    "render_form


  METHOD render_menu.

    DATA lo_toolbar TYPE REF TO zcl_abapgit_html_toolbar.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    lo_toolbar->add( iv_act = 'submitFormById(''commit_form'');'
                     iv_txt = 'Commit'
                     iv_typ = zif_abapgit_definitions=>gc_action_type-onclick
                     iv_opt = zif_abapgit_definitions=>gc_html_opt-strong ) ##NO_TEXT.

    lo_toolbar->add( iv_act = c_action-commit_cancel
                     iv_txt = 'Cancel'
                     iv_opt = zif_abapgit_definitions=>gc_html_opt-cancel ) ##NO_TEXT.

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.      "render_menu


  METHOD render_stage.

    DATA: lt_stage TYPE zcl_abapgit_stage=>ty_stage_tt.

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
      ro_html->add( zcl_abapgit_stage=>method_description( <ls_stage>-method ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '<td>' ).
      ro_html->add( <ls_stage>-file-path && <ls_stage>-file-filename ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.
    ro_html->add( '</tbody>' ).

    ro_html->add( '</table>' ).

  ENDMETHOD.    "render_stage


  METHOD render_text_input.

    DATA lv_attrs TYPE string.

    CREATE OBJECT ro_html.

    IF iv_value IS NOT INITIAL.
      lv_attrs = | value="{ iv_value }"|.
    ENDIF.

    IF iv_max_length IS NOT INITIAL.
      lv_attrs = | maxlength="{ iv_max_length }"|.
    ENDIF.

    ro_html->add( '<div class="row">' ).
    ro_html->add( |<label for="{ iv_name }">{ iv_label }</label>| ).
    ro_html->add( |<input id="{ iv_name }" name="{ iv_name }" type="text"{ lv_attrs }>| ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  " render_text_input


  METHOD scripts.

    CREATE OBJECT ro_html.
    ro_html->add( 'setInitialFocus("comment");' ).

  ENDMETHOD.    "scripts


  METHOD zif_abapgit_gui_page~on_event.

    DATA: ls_commit TYPE zcl_abapgit_services_git=>ty_commit_fields.

    CASE iv_action.
      WHEN c_action-commit_post.

        zcl_abapgit_html_action_utils=>parse_commit_request(
          EXPORTING it_postdata = it_postdata
          IMPORTING es_fields   = ls_commit ).

        ls_commit-repo_key = mo_repo->get_key( ).

        zcl_abapgit_services_git=>commit( is_commit   = ls_commit
                                  io_repo     = mo_repo
                                  io_stage    = mo_stage ).

        ev_state = zif_abapgit_definitions=>gc_event_state-go_back_to_bookmark.

      WHEN c_action-commit_cancel.
        ev_state = zif_abapgit_definitions=>gc_event_state-go_back.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
