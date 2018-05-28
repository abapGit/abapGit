CLASS zcl_abapgit_gui_page_tag DEFINITION PUBLIC FINAL
    CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.

    CONSTANTS: BEGIN OF c_action,
                 commit_post   TYPE string VALUE 'commit_post',
                 commit_cancel TYPE string VALUE 'commit_cancel',
               END OF c_action.

    METHODS:
      constructor
        IMPORTING io_repo TYPE REF TO zcl_abapgit_repo
        RAISING   zcx_abapgit_exception,

      zif_abapgit_gui_page~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION,
      scripts        REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo_online  TYPE REF TO zcl_abapgit_repo_online.

    METHODS:
      render_menu
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html,

      render_form
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html
        RAISING   zcx_abapgit_exception,

      render_text_input
        IMPORTING iv_name        TYPE string
                  iv_label       TYPE string
                  iv_value       TYPE string OPTIONAL
                  iv_max_length  TYPE string OPTIONAL
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html,

      create_tag
        IMPORTING it_postdata TYPE cnht_post_data_tab
        RAISING   zcx_abapgit_exception,

      parse_tag_request
        IMPORTING !it_postdata TYPE cnht_post_data_tab
        EXPORTING !es_fields   TYPE any.

ENDCLASS.



CLASS zcl_abapgit_gui_page_tag IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    mo_repo_online ?= io_repo.

    ms_control-page_title = 'TAG'.
  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="repo">' ).
    ro_html->add( render_menu( ) ).
    ro_html->add( render_form( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_content


  METHOD render_form.

    CONSTANTS: lc_body_col_max TYPE i VALUE 150.

    DATA: lo_user      TYPE REF TO zcl_abapgit_persistence_user,
          lv_user      TYPE string,
          lv_email     TYPE string,
          lv_s_param   TYPE string,
          lo_settings  TYPE REF TO zcl_abapgit_settings,
          lv_body_size TYPE i.

    lo_user  = zcl_abapgit_persistence_user=>get_instance( ).

    lv_user  = lo_user->get_repo_git_user_name( mo_repo_online->get_url( ) ).
    IF lv_user IS INITIAL.
      lv_user  = lo_user->get_default_git_user_name( ).
    ENDIF.
    IF lv_user IS INITIAL.
      " get default from user master record
      lv_user = zcl_abapgit_user_master_record=>get_instance( sy-uname )->get_name( ).
    ENDIF.

    lv_email = lo_user->get_repo_git_user_email( mo_repo_online->get_url( ) ).
    IF lv_email IS INITIAL.
      lv_email = lo_user->get_default_git_user_email( ).
    ENDIF.
    IF lv_email IS INITIAL.
      " get default from user master record
      lv_email = zcl_abapgit_user_master_record=>get_instance( sy-uname )->get_email( ).
    ENDIF.

    CREATE OBJECT ro_html.

    ro_html->add( |<h3>If comment and body are empty a lightweight tag will be created, |
               && |otherwise an annotated tag.</h3>| ).

    ro_html->add( '<div class="form-container">' ).
    ro_html->add( '<form id="commit_form" class="aligned-form"'
               && ' method="post" action="sapevent:commit_post">' ).

    ro_html->add( render_text_input( iv_name  = 'sha1'
                                     iv_label = 'SHA1'
                                     iv_value = mo_repo_online->get_sha1_local( ) ) ).

    ro_html->add( render_text_input( iv_name  = 'name'
                                     iv_label = 'tag name' ) ).

    ro_html->add( render_text_input( iv_name  = 'tagger_name'
                                     iv_label = 'tagger name'
                                     iv_value = lv_user ) ).

    ro_html->add( render_text_input( iv_name  = 'tagger_email'
                                     iv_label = 'tagger e-mail'
                                     iv_value = lv_email ) ).

    lo_settings = zcl_abapgit_persist_settings=>get_instance( )->read( ).

    lv_s_param = lo_settings->get_commitmsg_comment_length( ).

    ro_html->add( render_text_input( iv_name       = 'message'
                                     iv_label      = 'message'
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

    ro_html->add( '</form>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.    "render_form


  METHOD render_menu.

    DATA lo_toolbar TYPE REF TO zcl_abapgit_html_toolbar.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    lo_toolbar->add( iv_act = 'submitFormById(''commit_form'');'
                     iv_txt = 'Create'
                     iv_typ = zif_abapgit_definitions=>gc_action_type-onclick
                     iv_opt = zif_abapgit_definitions=>gc_html_opt-strong ) ##NO_TEXT.

    lo_toolbar->add( iv_act = c_action-commit_cancel
                     iv_txt = 'Cancel'
                     iv_opt = zif_abapgit_definitions=>gc_html_opt-cancel ) ##NO_TEXT.

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.      "render_menu


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
    ro_html->add( 'setInitialFocus("tag_name");' ).

  ENDMETHOD.    "scripts


  METHOD zif_abapgit_gui_page~on_event.

    CASE iv_action.
      WHEN c_action-commit_post.

        create_tag( it_postdata ).

        ev_state = zif_abapgit_definitions=>gc_event_state-go_back.

      WHEN c_action-commit_cancel.
        ev_state = zif_abapgit_definitions=>gc_event_state-go_back.
    ENDCASE.

  ENDMETHOD.

  METHOD create_tag.

    DATA:
      ls_tag      TYPE zif_abapgit_definitions=>ty_git_tag,
      lx_error    TYPE REF TO zcx_abapgit_exception,
      lv_text     TYPE string,
      lv_tag_type TYPE zif_abapgit_definitions=>ty_git_branch_type.

    parse_tag_request(
      EXPORTING it_postdata = it_postdata
      IMPORTING es_fields   = ls_tag ).

    ls_tag-name = zcl_abapgit_tag=>add_tag_prefix( ls_tag-name ).
    ASSERT ls_tag-name CP 'refs/tags/+*'.

    TRY.
        lv_tag_type = zcl_abapgit_git_porcelain=>create_tag( io_repo = mo_repo_online
                                                             is_tag  = ls_tag ).

      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abapgit_exception=>raise( |Cannot create tag { ls_tag-name }. Error: '{ lx_error->get_text( ) }'| ).
    ENDTRY.

    IF lv_tag_type = zif_abapgit_definitions=>c_git_branch_type-lightweight_tag.
      lv_text = |Lightweight tag { zcl_abapgit_tag=>remove_tag_prefix( ls_tag-name ) } created| ##NO_TEXT.
    ELSE.
      lv_text = |Annotated tag { zcl_abapgit_tag=>remove_tag_prefix( ls_tag-name ) } created| ##NO_TEXT.
    ENDIF.

    MESSAGE lv_text TYPE 'S'.

  ENDMETHOD.


  METHOD parse_tag_request.

    CONSTANTS: lc_replace TYPE string VALUE '<<new>>'.

    DATA: lv_string TYPE string,
          lt_fields TYPE tihttpnvp.

    FIELD-SYMBOLS <lv_body> TYPE string.

    CLEAR es_fields.

    CONCATENATE LINES OF it_postdata INTO lv_string.
    REPLACE ALL OCCURRENCES OF zif_abapgit_definitions=>gc_crlf    IN lv_string WITH lc_replace.
    REPLACE ALL OCCURRENCES OF zif_abapgit_definitions=>gc_newline IN lv_string WITH lc_replace.
    lt_fields = zcl_abapgit_html_action_utils=>parse_fields_upper_case_name( lv_string ).

    zcl_abapgit_html_action_utils=>get_field( EXPORTING name = 'SHA1'         it = lt_fields CHANGING cv = es_fields ).
    zcl_abapgit_html_action_utils=>get_field( EXPORTING name = 'NAME'         it = lt_fields CHANGING cv = es_fields ).
    zcl_abapgit_html_action_utils=>get_field( EXPORTING name = 'TAGGER_NAME'  it = lt_fields CHANGING cv = es_fields ).
    zcl_abapgit_html_action_utils=>get_field( EXPORTING name = 'TAGGER_EMAIL' it = lt_fields CHANGING cv = es_fields ).
    zcl_abapgit_html_action_utils=>get_field( EXPORTING name = 'MESSAGE'      it = lt_fields CHANGING cv = es_fields ).
    zcl_abapgit_html_action_utils=>get_field( EXPORTING name = 'BODY'         it = lt_fields CHANGING cv = es_fields ).

    ASSIGN COMPONENT 'BODY' OF STRUCTURE es_fields TO <lv_body>.
    ASSERT <lv_body> IS ASSIGNED.
    REPLACE ALL OCCURRENCES OF lc_replace IN <lv_body> WITH zif_abapgit_definitions=>gc_newline.

  ENDMETHOD.

ENDCLASS.
