CLASS zcl_abapgit_gui_page_tag DEFINITION PUBLIC FINAL
    CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.

    CONSTANTS: BEGIN OF c_action,
                 commit_post     TYPE string VALUE 'commit_post',
                 commit_cancel   TYPE string VALUE 'commit_cancel',
                 change_tag_type TYPE string VALUE 'change_tag_type',
               END OF c_action.

    METHODS:
      constructor
        IMPORTING io_repo TYPE REF TO zcl_abapgit_repo
        RAISING   zcx_abapgit_exception,

      zif_abapgit_gui_event_handler~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_tag_type,
                 lightweight TYPE string VALUE 'lightweight',
                 annotated   TYPE string VALUE 'annotated',
               END OF c_tag_type.

    DATA: mo_repo_online   TYPE REF TO zcl_abapgit_repo_online,
          mv_selected_type TYPE string.

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
        IMPORTING it_postdata TYPE cnht_post_data_tab
        EXPORTING eg_fields   TYPE any,
      parse_change_tag_type_request
        IMPORTING
          it_postdata TYPE cnht_post_data_tab.

    METHODS render_scripts
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_TAG IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    mo_repo_online ?= io_repo.

    ms_control-page_title = 'TAG'.
    mv_selected_type = c_tag_type-lightweight.

  ENDMETHOD.


  METHOD create_tag.

    DATA:
      ls_tag   TYPE zif_abapgit_definitions=>ty_git_tag,
      lx_error TYPE REF TO zcx_abapgit_exception,
      lv_text  TYPE string.

    parse_tag_request(
      EXPORTING it_postdata = it_postdata
      IMPORTING eg_fields   = ls_tag ).

    IF ls_tag-name IS INITIAL.
      zcx_abapgit_exception=>raise( |Please supply a tag name| ).
    ENDIF.

    ls_tag-name = zcl_abapgit_git_tag=>add_tag_prefix( ls_tag-name ).
    ASSERT ls_tag-name CP 'refs/tags/+*'.

    CASE mv_selected_type.
      WHEN c_tag_type-lightweight.

        ls_tag-type = zif_abapgit_definitions=>c_git_branch_type-lightweight_tag.

      WHEN c_tag_type-annotated.

        ls_tag-type = zif_abapgit_definitions=>c_git_branch_type-annotated_tag.

      WHEN OTHERS.

        zcx_abapgit_exception=>raise( |Invalid tag type: { mv_selected_type }| ).

    ENDCASE.

    TRY.
        zcl_abapgit_git_porcelain=>create_tag( iv_url = mo_repo_online->get_url( )
                                               is_tag = ls_tag ).

      CATCH zcx_abapgit_exception INTO lx_error.
        zcx_abapgit_exception=>raise( |Cannot create tag { ls_tag-name }. Error: '{ lx_error->get_text( ) }'| ).
    ENDTRY.

    IF ls_tag-type = zif_abapgit_definitions=>c_git_branch_type-lightweight_tag.
      lv_text = |Lightweight tag { zcl_abapgit_git_tag=>remove_tag_prefix( ls_tag-name ) } created| ##NO_TEXT.
    ELSEIF ls_tag-type = zif_abapgit_definitions=>c_git_branch_type-annotated_tag.
      lv_text = |Annotated tag { zcl_abapgit_git_tag=>remove_tag_prefix( ls_tag-name ) } created| ##NO_TEXT.
    ENDIF.

    MESSAGE lv_text TYPE 'S'.

  ENDMETHOD.


  METHOD parse_change_tag_type_request.

    FIELD-SYMBOLS: <lv_postdata> TYPE cnht_post_data_line.

    READ TABLE it_postdata ASSIGNING <lv_postdata>
                           INDEX 1.
    IF sy-subrc = 0.
      FIND FIRST OCCURRENCE OF REGEX `type=(.*)`
           IN <lv_postdata>
           SUBMATCHES mv_selected_type.
    ENDIF.

    mv_selected_type = condense( mv_selected_type ).

  ENDMETHOD.


  METHOD parse_tag_request.

    CONSTANTS: lc_replace TYPE string VALUE '<<new>>'.

    DATA: lv_string TYPE string,
          lt_fields TYPE tihttpnvp.

    FIELD-SYMBOLS <lv_body> TYPE string.

    CLEAR eg_fields.

    CONCATENATE LINES OF it_postdata INTO lv_string.
    REPLACE ALL OCCURRENCES OF zif_abapgit_definitions=>c_crlf    IN lv_string WITH lc_replace.
    REPLACE ALL OCCURRENCES OF zif_abapgit_definitions=>c_newline IN lv_string WITH lc_replace.
    lt_fields = zcl_abapgit_html_action_utils=>parse_fields_upper_case_name( lv_string ).

    zcl_abapgit_html_action_utils=>get_field( EXPORTING iv_name = 'SHA1'
                                                        it_field = lt_fields
                                              CHANGING cg_field = eg_fields ).
    zcl_abapgit_html_action_utils=>get_field( EXPORTING iv_name = 'NAME'
                                                        it_field = lt_fields
                                              CHANGING cg_field = eg_fields ).
    zcl_abapgit_html_action_utils=>get_field( EXPORTING iv_name = 'TAGGER_NAME'
                                                        it_field = lt_fields
                                              CHANGING cg_field = eg_fields ).
    zcl_abapgit_html_action_utils=>get_field( EXPORTING iv_name = 'TAGGER_EMAIL'
                                                        it_field = lt_fields
                                              CHANGING cg_field = eg_fields ).
    zcl_abapgit_html_action_utils=>get_field( EXPORTING iv_name = 'MESSAGE'
                                                        it_field = lt_fields
                                              CHANGING cg_field = eg_fields ).
    zcl_abapgit_html_action_utils=>get_field( EXPORTING iv_name = 'BODY'
                                                        it_field = lt_fields
                                              CHANGING cg_field = eg_fields ).

    ASSIGN COMPONENT 'BODY' OF STRUCTURE eg_fields TO <lv_body>.
    ASSERT <lv_body> IS ASSIGNED.
    REPLACE ALL OCCURRENCES OF lc_replace IN <lv_body> WITH zif_abapgit_definitions=>c_newline.

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="repo">' ).
    ro_html->add( render_menu( ) ).
    ro_html->add( render_form( ) ).
    ro_html->add( '</div>' ).

    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.


  METHOD render_form.

    CONSTANTS: lc_body_col_max TYPE i VALUE 150.

    DATA: li_user      TYPE REF TO zif_abapgit_persist_user,
          lv_user      TYPE string,
          lv_email     TYPE string,
          lv_s_param   TYPE string,
          lo_settings  TYPE REF TO zcl_abapgit_settings,
          lv_body_size TYPE i,
          lt_type      TYPE string_table,
          lv_selected  TYPE string.

    FIELD-SYMBOLS: <lv_type> LIKE LINE OF lt_type.


    li_user = zcl_abapgit_persistence_user=>get_instance( ).

    lv_user = li_user->get_repo_git_user_name( mo_repo_online->get_url( ) ).
    IF lv_user IS INITIAL.
      lv_user = li_user->get_default_git_user_name( ).
    ENDIF.
    IF lv_user IS INITIAL.
      " get default from user master record
      lv_user = zcl_abapgit_user_master_record=>get_instance( sy-uname )->get_name( ).
    ENDIF.

    lv_email = li_user->get_repo_git_user_email( mo_repo_online->get_url( ) ).
    IF lv_email IS INITIAL.
      lv_email = li_user->get_default_git_user_email( ).
    ENDIF.
    IF lv_email IS INITIAL.
      " get default from user master record
      lv_email = zcl_abapgit_user_master_record=>get_instance( sy-uname )->get_email( ).
    ENDIF.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="form-container">' ).
    ro_html->add( '<form id="commit_form" class="aligned-form grey70"'
               && ' method="post" action="sapevent:commit_post">' ).

    INSERT c_tag_type-lightweight
           INTO TABLE lt_type.

    INSERT c_tag_type-annotated
           INTO TABLE lt_type.

    ro_html->add( '<div class="row">' ).
    ro_html->add( 'Tag type <select name="folder_logic" onchange="onTagTypeChange(this)">' ).

    LOOP AT lt_type ASSIGNING <lv_type>.

      IF mv_selected_type = <lv_type>.
        lv_selected = 'selected'.
      ELSE.
        CLEAR: lv_selected.
      ENDIF.

      ro_html->add( |<option value="{ <lv_type> }" | && |{ lv_selected }>| && |{ <lv_type> }</option>| ).

    ENDLOOP.

    ro_html->add( '</div>' ).

    ro_html->add( '</select>' ).
    ro_html->add( '<br>' ).
    ro_html->add( '<br>' ).

    ro_html->add( render_text_input( iv_name  = 'sha1'
                                     iv_label = 'SHA1'
                                     iv_value = mo_repo_online->get_sha1_remote( ) ) ).

    ro_html->add( render_text_input( iv_name  = 'name'
                                     iv_label = 'tag name' ) ).

    IF mv_selected_type = c_tag_type-annotated.

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
      ro_html->add( |<textarea id="c-body" name="body" rows="10" cols="| && |{ lv_body_size }"></textarea>| ).

    ENDIF.

    ro_html->add( '<input type="submit" class="hidden-submit">' ).
    ro_html->add( '</div>' ).

    ro_html->add( '</form>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_menu.

    DATA lo_toolbar TYPE REF TO zcl_abapgit_html_toolbar.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    lo_toolbar->add( iv_act = 'submitFormById(''commit_form'');'
                     iv_txt = 'Create'
                     iv_typ = zif_abapgit_html=>c_action_type-onclick
                     iv_opt = zif_abapgit_html=>c_html_opt-strong ) ##NO_TEXT.

    lo_toolbar->add( iv_act = c_action-commit_cancel
                     iv_txt = 'Cancel'
                     iv_opt = zif_abapgit_html=>c_html_opt-cancel ) ##NO_TEXT.

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_scripts.

    CREATE OBJECT ro_html.

    ro_html->zif_abapgit_html~set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ro_html->add( 'setInitialFocus("name");' ).

  ENDMETHOD.


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

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE iv_action.
      WHEN c_action-commit_post.

        create_tag( it_postdata ).

        ev_state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_action-change_tag_type.

        parse_change_tag_type_request( it_postdata ).

        ev_state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-commit_cancel.
        ev_state = zcl_abapgit_gui=>c_event_state-go_back.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
