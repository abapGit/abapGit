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

    CONSTANTS:
      BEGIN OF c_tag_type,
        lightweight TYPE string VALUE 'lightweight',
        annotated   TYPE string VALUE 'annotated',
      END OF c_tag_type .
    DATA mo_repo_online TYPE REF TO zcl_abapgit_repo_online .
    DATA mv_selected_type TYPE string .

    METHODS render_menu
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS render_form
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_text_input
      IMPORTING
        !iv_name       TYPE string
        !iv_label      TYPE string
        !iv_value      TYPE string OPTIONAL
        !iv_max_length TYPE string OPTIONAL
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    METHODS create_tag
      IMPORTING
        !ii_event TYPE REF TO zif_abapgit_gui_event
      RAISING
        zcx_abapgit_exception .
    METHODS parse_tag_request
      IMPORTING
        !ii_event TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_git_tag) TYPE zif_abapgit_definitions=>ty_git_tag
      RAISING
        zcx_abapgit_exception .
    METHODS parse_change_tag_type_request
      IMPORTING
        !it_postdata TYPE cnht_post_data_tab .
    METHODS render_scripts
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_TAG IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    mo_repo_online ?= io_repo.

    ms_control-page_title = 'Tag'.
    mv_selected_type = c_tag_type-lightweight.

  ENDMETHOD.


  METHOD create_tag.

    DATA:
      ls_tag   TYPE zif_abapgit_definitions=>ty_git_tag,
      lx_error TYPE REF TO zcx_abapgit_exception,
      lv_text  TYPE string.

    ls_tag = parse_tag_request( ii_event ).

    IF ls_tag-name IS INITIAL.
      zcx_abapgit_exception=>raise( |Please supply a tag name| ).
    ENDIF.

    ls_tag-name = zcl_abapgit_git_tag=>add_tag_prefix( ls_tag-name ).
    ASSERT ls_tag-name CP zif_abapgit_definitions=>c_git_branch-tags.

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
      lv_text = |Lightweight tag { zcl_abapgit_git_tag=>remove_tag_prefix( ls_tag-name ) } created|.
    ELSEIF ls_tag-type = zif_abapgit_definitions=>c_git_branch_type-annotated_tag.
      lv_text = |Annotated tag { zcl_abapgit_git_tag=>remove_tag_prefix( ls_tag-name ) } created|.
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

    DATA lo_map TYPE REF TO zcl_abapgit_string_map.

    lo_map = ii_event->form_data( ).
    lo_map->strict( abap_false ). " Hack, refactor !
    lo_map->to_abap( CHANGING cs_container = rs_git_tag ).
    REPLACE ALL OCCURRENCES
      OF zif_abapgit_definitions=>c_crlf
      IN rs_git_tag-body
      WITH zif_abapgit_definitions=>c_newline.

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="repo">' ).
    ri_html->add( render_menu( ) ).
    ri_html->add( render_form( ) ).
    ri_html->add( '</div>' ).

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

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="form-container">' ).
    ri_html->add( '<form id="commit_form" class="aligned-form grey70"'
               && ' method="post" action="sapevent:commit_post">' ).

    INSERT c_tag_type-lightweight
           INTO TABLE lt_type.

    INSERT c_tag_type-annotated
           INTO TABLE lt_type.

    ri_html->add( '<div class="row">' ).
    ri_html->add( 'Tag type <select name="folder_logic" onchange="onTagTypeChange(this)">' ).

    LOOP AT lt_type ASSIGNING <lv_type>.

      IF mv_selected_type = <lv_type>.
        lv_selected = 'selected'.
      ELSE.
        CLEAR: lv_selected.
      ENDIF.

      ri_html->add( |<option value="{ <lv_type> }" | && |{ lv_selected }>| && |{ <lv_type> }</option>| ).

    ENDLOOP.

    ri_html->add( '</div>' ).

    ri_html->add( '</select>' ).
    ri_html->add( '<br>' ).
    ri_html->add( '<br>' ).

    ri_html->add( render_text_input( iv_name  = 'sha1'
                                     iv_label = 'SHA1'
                                     iv_value = mo_repo_online->get_sha1_remote( ) ) ).

    ri_html->add( render_text_input( iv_name  = 'name'
                                     iv_label = 'tag name' ) ).

    IF mv_selected_type = c_tag_type-annotated.

      ri_html->add( render_text_input( iv_name  = 'tagger_name'
                                       iv_label = 'tagger name'
                                       iv_value = lv_user ) ).

      ri_html->add( render_text_input( iv_name  = 'tagger_email'
                                       iv_label = 'tagger e-mail'
                                       iv_value = lv_email ) ).

      lo_settings = zcl_abapgit_persist_settings=>get_instance( )->read( ).

      lv_s_param = lo_settings->get_commitmsg_comment_length( ).

      ri_html->add( render_text_input( iv_name       = 'message'
                                       iv_label      = 'message'
                                       iv_max_length = lv_s_param ) ).

      ri_html->add( '<div class="row">' ).
      ri_html->add( '<label for="c-body">body</label>' ).

      lv_body_size = lo_settings->get_commitmsg_body_size( ).
      IF lv_body_size > lc_body_col_max.
        lv_body_size = lc_body_col_max.
      ENDIF.
      ri_html->add( |<textarea id="c-body" name="body" rows="10" cols="{ lv_body_size }"></textarea>| ).

    ENDIF.

    ri_html->add( '<input type="submit" class="hidden-submit">' ).
    ri_html->add( '</div>' ).

    ri_html->add( '</form>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_menu.

    DATA lo_toolbar TYPE REF TO zcl_abapgit_html_toolbar.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    CREATE OBJECT lo_toolbar.

    lo_toolbar->add( iv_act = 'submitFormById(''commit_form'');'
                     iv_txt = 'Create'
                     iv_typ = zif_abapgit_html=>c_action_type-onclick
                     iv_opt = zif_abapgit_html=>c_html_opt-strong ).

    lo_toolbar->add( iv_act = c_action-commit_cancel
                     iv_txt = 'Cancel'
                     iv_opt = zif_abapgit_html=>c_html_opt-cancel ).

    ri_html->add( '<div class="paddings">' ).
    ri_html->add( lo_toolbar->render( ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_scripts.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( 'setInitialFocus("name");' ).

  ENDMETHOD.


  METHOD render_text_input.

    DATA lv_attrs TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF iv_value IS NOT INITIAL.
      lv_attrs = | value="{ iv_value }"|.
    ENDIF.

    IF iv_max_length IS NOT INITIAL.
      lv_attrs = | maxlength="{ iv_max_length }"|.
    ENDIF.

    ri_html->add( '<div class="row">' ).
    ri_html->add( |<label for="{ iv_name }">{ iv_label }</label>| ).
    ri_html->add( |<input id="{ iv_name }" name="{ iv_name }" type="text"{ lv_attrs }>| ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-commit_post.

        create_tag( ii_event ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_action-change_tag_type.

        parse_change_tag_type_request( ii_event->mt_postdata ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_action-commit_cancel.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
