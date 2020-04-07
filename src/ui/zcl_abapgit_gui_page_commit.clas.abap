CLASS zcl_abapgit_gui_page_commit DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_gui_page_hotkey.

    CONSTANTS:
      BEGIN OF c_action,
        commit_post   TYPE string VALUE 'commit_post',
        commit_cancel TYPE string VALUE 'commit_cancel',
      END OF c_action .

    METHODS constructor
      IMPORTING
        io_repo  TYPE REF TO zcl_abapgit_repo_online
        io_stage TYPE REF TO zcl_abapgit_stage
      RAISING
        zcx_abapgit_exception.

    METHODS zif_abapgit_gui_event_handler~on_event
        REDEFINITION .
  PROTECTED SECTION.

    CLASS-METHODS parse_commit_request
      IMPORTING
        !it_postdata TYPE cnht_post_data_tab
      EXPORTING
        !eg_fields   TYPE any .

    METHODS render_content
        REDEFINITION .
    METHODS scripts
        REDEFINITION .
  PRIVATE SECTION.

    DATA mo_repo TYPE REF TO zcl_abapgit_repo_online .
    DATA mo_stage TYPE REF TO zcl_abapgit_stage .
    DATA ms_commit TYPE zif_abapgit_services_git=>ty_commit_fields .

    METHODS render_menu
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_stage
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_form
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_text_input
      IMPORTING
        !iv_name       TYPE string
        !iv_label      TYPE string
        !iv_value      TYPE string OPTIONAL
        !iv_max_length TYPE string OPTIONAL
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_COMMIT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    mo_repo   = io_repo.
    mo_stage  = io_stage.

    ms_control-page_title = 'COMMIT'.
  ENDMETHOD.


  METHOD parse_commit_request.

    CONSTANTS: lc_replace TYPE string VALUE '<<new>>'.

    DATA: lv_string TYPE string,
          lt_fields TYPE tihttpnvp.

    FIELD-SYMBOLS <lv_body> TYPE string.

    CLEAR eg_fields.

    CONCATENATE LINES OF it_postdata INTO lv_string.
    REPLACE ALL OCCURRENCES OF zif_abapgit_definitions=>c_crlf    IN lv_string WITH lc_replace.
    REPLACE ALL OCCURRENCES OF zif_abapgit_definitions=>c_newline IN lv_string WITH lc_replace.
    lt_fields = zcl_abapgit_html_action_utils=>parse_fields_upper_case_name( lv_string ).

    zcl_abapgit_html_action_utils=>get_field(
      EXPORTING
        iv_name = 'COMMITTER_NAME'
        it_field = lt_fields
      CHANGING
        cg_field = eg_fields ).
    zcl_abapgit_html_action_utils=>get_field(
      EXPORTING
        iv_name = 'COMMITTER_EMAIL'
        it_field = lt_fields
      CHANGING
        cg_field = eg_fields ).
    zcl_abapgit_html_action_utils=>get_field(
      EXPORTING
        iv_name = 'AUTHOR_NAME'
        it_field = lt_fields
      CHANGING
        cg_field = eg_fields ).
    zcl_abapgit_html_action_utils=>get_field(
      EXPORTING
        iv_name = 'AUTHOR_EMAIL'
        it_field = lt_fields
      CHANGING
      cg_field = eg_fields ).
    zcl_abapgit_html_action_utils=>get_field(
      EXPORTING
        iv_name = 'COMMENT'
        it_field = lt_fields
      CHANGING
      cg_field = eg_fields ).
    zcl_abapgit_html_action_utils=>get_field(
      EXPORTING
        iv_name = 'BODY'
        it_field = lt_fields
      CHANGING
        cg_field = eg_fields ).

    ASSIGN COMPONENT 'BODY' OF STRUCTURE eg_fields TO <lv_body>.
    ASSERT <lv_body> IS ASSIGNED.
    REPLACE ALL OCCURRENCES OF lc_replace IN <lv_body> WITH zif_abapgit_definitions=>c_newline.

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="repo">' ).
    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
      io_repo         = mo_repo
      iv_show_package = abap_false
      iv_branch       = mo_repo->get_branch_name( ) ) ).

    ro_html->add( render_menu( ) ).
    ro_html->add( render_form( ) ).
    ro_html->add( render_stage( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_form.

    CONSTANTS: lc_body_col_max TYPE i VALUE 150.

    DATA: li_user      TYPE REF TO zif_abapgit_persist_user.
    DATA: lv_user      TYPE string.
    DATA: lv_email     TYPE string.
    DATA: lv_s_param   TYPE string.
    DATA: lo_settings  TYPE REF TO zcl_abapgit_settings.
    DATA: lv_body_size TYPE i.
    DATA: lv_comment   TYPE string.
    DATA: lv_body      TYPE string.
    DATA: lv_author_name TYPE string.
    DATA: lv_author_email TYPE string.

* see https://git-scm.com/book/ch5-2.html
* commit messages should be max 50 characters
* body should wrap at 72 characters

    li_user = zcl_abapgit_persistence_user=>get_instance( ).

    lv_user  = li_user->get_repo_git_user_name( mo_repo->get_url( ) ).
    IF lv_user IS INITIAL.
      lv_user  = li_user->get_default_git_user_name( ).
    ENDIF.
    IF lv_user IS INITIAL.
      " get default from user master record
      lv_user = zcl_abapgit_user_master_record=>get_instance( sy-uname )->get_name( ).
    ENDIF.

    lv_email = li_user->get_repo_git_user_email( mo_repo->get_url( ) ).
    IF lv_email IS INITIAL.
      lv_email = li_user->get_default_git_user_email( ).
    ENDIF.
    IF lv_email IS INITIAL.
      " get default from user master record
      lv_email = zcl_abapgit_user_master_record=>get_instance( sy-uname )->get_email( ).
    ENDIF.

    IF ms_commit IS NOT INITIAL.
      lv_user = ms_commit-committer_name.
      lv_email = ms_commit-committer_email.
      lv_comment = ms_commit-comment.
      lv_body = ms_commit-body.
      lv_author_name = ms_commit-author_name.
      lv_author_email = ms_commit-author_email.
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
                                     iv_value      = lv_comment
                                     iv_max_length = lv_s_param ) ).

    ro_html->add( '<div class="row">' ).
    ro_html->add( '<label for="c-body">body</label>' ).

    lv_body_size = lo_settings->get_commitmsg_body_size( ).
    IF lv_body_size > lc_body_col_max.
      lv_body_size = lc_body_col_max.
    ENDIF.
    ro_html->add( |<textarea id="c-body" name="body" rows="10" cols="| &&
                  |{ lv_body_size }">{ lv_body }</textarea>| ).

    ro_html->add( '<input type="submit" class="hidden-submit">' ).
    ro_html->add( '</div>' ).

    ro_html->add( '<div class="row">' ).
    ro_html->add( '<span class="cell"></span>' ).
    ro_html->add( '<span class="cell sub-title">Optionally,'
               && ' specify author (same as committer by default)</span>' ).
    ro_html->add( '</div>' ).

    ro_html->add( render_text_input( iv_name  = 'author_name'
                                     iv_label = 'author name'
                                     iv_value = lv_author_name ) ).

    ro_html->add( render_text_input( iv_name  = 'author_email'
                                     iv_label = 'author e-mail'
                                     iv_value = lv_author_email ) ).

    ro_html->add( '</form>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_menu.

    DATA lo_toolbar TYPE REF TO zcl_abapgit_html_toolbar.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    lo_toolbar->add( iv_act = 'submitFormById(''commit_form'');'
                     iv_txt = 'Commit'
                     iv_typ = zif_abapgit_html=>c_action_type-onclick
                     iv_opt = zif_abapgit_html=>c_html_opt-strong ) ##NO_TEXT.

    lo_toolbar->add( iv_act = c_action-commit_cancel
                     iv_txt = 'Cancel'
                     iv_opt = zif_abapgit_html=>c_html_opt-cancel ) ##NO_TEXT.

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_stage.

    DATA: lt_stage TYPE zcl_abapgit_stage=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage.


    CREATE OBJECT ro_html.

    lt_stage = mo_stage->get_all( ).

    ro_html->add( '<table class="stage_tab">' ).
    ro_html->add( '<thead>' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<th colspan="3">Staged files</th>' ).
    ro_html->add( '</tr>' ).
    ro_html->add( '</thead>' ).

    ro_html->add( '<tbody>' ).
    LOOP AT lt_stage ASSIGNING <ls_stage>.
      ro_html->add( '<tr>' ).
      ro_html->add( '<td>' ).
      ro_html->add( zcl_abapgit_gui_chunk_lib=>render_item_state(
        iv_lstate = <ls_stage>-status-lstate
        iv_rstate = <ls_stage>-status-rstate ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '<td class="method">' ).
      ro_html->add( |<b>{ zcl_abapgit_stage=>method_description( <ls_stage>-method ) }</b>| ).
      ro_html->add( '</td>' ).
      ro_html->add( '<td>' ).
      ro_html->add( <ls_stage>-file-path && <ls_stage>-file-filename ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.
    ro_html->add( '</tbody>' ).

    ro_html->add( '</table>' ).

  ENDMETHOD.


  METHOD render_text_input.

    DATA lv_attrs TYPE string.

    CREATE OBJECT ro_html.

    IF iv_value IS NOT INITIAL AND
       iv_max_length IS NOT INITIAL.
      lv_attrs = | value="{ iv_value }" maxlength="{ iv_max_length }"|.
    ELSEIF iv_value IS NOT INITIAL.
      lv_attrs = | value="{ iv_value }"|.

    ELSEIF iv_max_length IS NOT INITIAL.
      lv_attrs = | maxlength="{ iv_max_length }"|.
    ENDIF.

    ro_html->add( '<div class="row">' ).
    ro_html->add( |<label for="{ iv_name }">{ iv_label }</label>| ).
    ro_html->add( |<input id="{ iv_name }" name="{ iv_name }" type="text"{ lv_attrs }>| ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD scripts.

    ro_html = super->scripts( ).

    ro_html->add( 'setInitialFocus("comment");' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE iv_action.
      WHEN c_action-commit_post.

        parse_commit_request(
          EXPORTING it_postdata = it_postdata
          IMPORTING eg_fields   = ms_commit ).

        ms_commit-repo_key = mo_repo->get_key( ).

        zcl_abapgit_services_git=>commit(
          is_commit = ms_commit
          io_repo   = mo_repo
          io_stage  = mo_stage ).

        MESSAGE 'Commit was successful' TYPE 'S' ##NO_TEXT.

        ev_state = zcl_abapgit_gui=>c_event_state-go_back_to_bookmark.

      WHEN c_action-commit_cancel.
        ev_state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN OTHERS.
        super->zif_abapgit_gui_event_handler~on_event(
          EXPORTING
            iv_action    = iv_action
            iv_getdata   = iv_getdata
            it_postdata  = it_postdata
          IMPORTING
            ei_page      = ei_page
            ev_state     = ev_state ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_hotkey~get_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
