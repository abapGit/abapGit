CLASS zcl_abapgit_gui_page_tags DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        !ii_repo       TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        !ii_repo TYPE REF TO zif_abapgit_repo
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_id,
        tag_group    TYPE string VALUE 'tag_group',
        tag_type     TYPE string VALUE 'tag_type',
        tags         TYPE string VALUE 'tags',
        type         TYPE string VALUE 'type',
        name         TYPE string VALUE 'name',
        sha1         TYPE string VALUE 'sha1',
        anno_group   TYPE string VALUE 'anno_group',
        tagger       TYPE string VALUE 'tagger',
        tagger_name  TYPE string VALUE 'tagger_name',
        tagger_email TYPE string VALUE 'tagger_email',
        message      TYPE string VALUE 'message',
        body         TYPE string VALUE 'body',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_event,
        create        TYPE string VALUE 'create',
        choose_commit TYPE string VALUE 'choose_commit',
        change_type   TYPE string VALUE 'change_type',
      END OF c_event.

    DATA mo_form TYPE REF TO zcl_abapgit_html_form.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_util TYPE REF TO zcl_abapgit_html_form_utils.
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.
    DATA mo_repo TYPE REF TO zcl_abapgit_repo_online.
    DATA mo_settings TYPE REF TO zcl_abapgit_settings.
    DATA ms_tag TYPE zif_abapgit_git_definitions=>ty_git_tag.

    METHODS get_form_schema
      IMPORTING
        io_form_data   TYPE REF TO zcl_abapgit_string_map OPTIONAL
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form
      RAISING
        zcx_abapgit_exception.

    METHODS initialize_form_data
      RAISING
        zcx_abapgit_exception.

    METHODS get_tagger_name
      RETURNING
        VALUE(rv_user) TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS get_tagger_email
      RETURNING
        VALUE(rv_email) TYPE string
      RAISING
        zcx_abapgit_exception.

    METHODS validate_form
      IMPORTING
        !io_form_data            TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

    METHODS choose_commit
      RETURNING
        VALUE(rv_commit) TYPE zif_abapgit_definitions=>ty_commit-sha1
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_TAGS IMPLEMENTATION.


  METHOD choose_commit.

    DATA li_popups TYPE REF TO zif_abapgit_popups.

    li_popups = zcl_abapgit_ui_factory=>get_popups( ).

    rv_commit = li_popups->commit_list_popup(
      iv_repo_url    = mo_repo->get_url( )
      iv_branch_name = mo_repo->get_selected_branch( ) )-sha1.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_form_data.
    CREATE OBJECT mo_validation_log.
    mo_repo ?= ii_repo.

    " Get settings from DB
    mo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).

    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).

    initialize_form_data( ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_tags.

    CREATE OBJECT lo_component
      EXPORTING
        ii_repo = ii_repo.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Create Tag'
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD get_form_schema.

    DATA lv_commitmsg_comment_length TYPE i.

    IF io_form_data IS BOUND AND io_form_data->is_empty( ) = abap_false.
      ms_tag-type = io_form_data->get( c_id-tag_type ).
    ENDIF.

    lv_commitmsg_comment_length =  mo_settings->get_commitmsg_comment_length( ).

    ro_form = zcl_abapgit_html_form=>create(
                iv_form_id   = 'create-tag-form'
                iv_help_page = 'https://docs.abapgit.org/' ). " todo, add docs

    ro_form->start_group(
      iv_name  = c_id-tag_group
      iv_label = 'New Tag'
    )->radio(
      iv_label  = 'Type'
      iv_name   = c_id-tag_type
      iv_action = c_event-change_type
    )->option(
      iv_label = 'Lightweight'
      iv_value = zif_abapgit_definitions=>c_git_branch_type-lightweight_tag
    )->option(
      iv_label = 'Annotated'
      iv_value = zif_abapgit_definitions=>c_git_branch_type-annotated_tag
    )->text(
      iv_name        = c_id-name
      iv_label       = 'Tag Name'
      iv_required    = abap_true
    )->text(
      iv_name        = c_id-sha1
      iv_label       = 'Commit'
      iv_min         = 40
      iv_max         = 40
      iv_condense    = abap_true
      iv_required    = abap_true
      iv_side_action = c_event-choose_commit ).

    IF ms_tag-type = zif_abapgit_definitions=>c_git_branch_type-annotated_tag.
      ro_form->start_group(
        iv_name        = c_id-anno_group
        iv_label       = 'Annotation'
      )->text(
        iv_name        = c_id-message
        iv_label       = 'Comment'
        iv_max         = lv_commitmsg_comment_length
        iv_placeholder = |Add a mandatory comment with max { lv_commitmsg_comment_length } characters|
      )->textarea(
        iv_name        = c_id-body
        iv_label       = 'Body'
        iv_rows        = 6
        iv_cols        = mo_settings->get_commitmsg_body_size( )
        iv_placeholder = 'Add an optional description...'
      )->text(
        iv_name        = c_id-tagger_name
        iv_label       = 'Tagger Name'
      )->text(
        iv_name        = c_id-tagger_email
        iv_label       = 'Tagger Email' ).
    ELSE.
      ro_form->hidden( c_id-message
      )->hidden( c_id-body
      )->hidden( c_id-tagger_name
      )->hidden( c_id-tagger_email ).
    ENDIF.

    ro_form->command(
      iv_label       = 'Create'
      iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-create
    )->command(
      iv_label       = 'Back'
      iv_action      = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD get_tagger_email.

    DATA li_user TYPE REF TO zif_abapgit_persist_user.

    li_user = zcl_abapgit_persistence_user=>get_instance( ).

    rv_email = li_user->get_repo_git_user_email( mo_repo->get_url( ) ).
    IF rv_email IS INITIAL.
      rv_email = li_user->get_default_git_user_email( ).
    ENDIF.
    IF rv_email IS INITIAL.
      " get default from user record
      rv_email = zcl_abapgit_user_record=>get_instance( sy-uname )->get_email( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_tagger_name.

    DATA li_user TYPE REF TO zif_abapgit_persist_user.

    li_user = zcl_abapgit_persistence_user=>get_instance( ).

    rv_user  = li_user->get_repo_git_user_name( mo_repo->get_url( ) ).
    IF rv_user IS INITIAL.
      rv_user  = li_user->get_default_git_user_name( ).
    ENDIF.
    IF rv_user IS INITIAL.
      " get default from user record
      rv_user = zcl_abapgit_user_record=>get_instance( sy-uname )->get_name( ).
    ENDIF.

  ENDMETHOD.


  METHOD initialize_form_data.

    ms_tag-type = zif_abapgit_definitions=>c_git_branch_type-lightweight_tag.

    mo_form_data->set(
      iv_key = c_id-tag_type
      iv_val = ms_tag-type ).

    ms_tag-tagger_name  = get_tagger_name( ).
    ms_tag-tagger_email = get_tagger_email( ).

    mo_form_data->set(
      iv_key = c_id-tagger_name
      iv_val = ms_tag-tagger_name ).
    mo_form_data->set(
      iv_key = c_id-tagger_email
      iv_val = ms_tag-tagger_email ).

    " Set for is_dirty check
    mo_form_util->set_data( mo_form_data ).

  ENDMETHOD.


  METHOD validate_form.

    DATA:
      lt_tags         TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt,
      lv_new_tag_name TYPE string.

    ro_validation_log = mo_form_util->validate( io_form_data ).

    IF zcl_abapgit_utils=>is_valid_email( io_form_data->get( c_id-tagger_email ) ) = abap_false.
      ro_validation_log->set(
        iv_key = c_id-tagger_email
        iv_val = |Invalid email address| ).
    ENDIF.

    lv_new_tag_name = io_form_data->get( c_id-name ).

    IF lv_new_tag_name IS NOT INITIAL.
      " Check if tag already exists
      lt_tags = zcl_abapgit_git_transport=>branches( mo_repo->get_url( ) )->get_tags_only( ).

      READ TABLE lt_tags TRANSPORTING NO FIELDS WITH TABLE KEY name_key
        COMPONENTS name = zcl_abapgit_git_tag=>add_tag_prefix( lv_new_tag_name ).
      IF sy-subrc = 0.
        ro_validation_log->set(
          iv_key = c_id-name
          iv_val = |Tag already exists| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA:
      lx_error  TYPE REF TO zcx_abapgit_exception,
      lv_commit TYPE zif_abapgit_git_definitions=>ty_sha1,
      lv_text   TYPE string.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-go_back.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_event-choose_commit.
        lv_commit = choose_commit( ).

        IF lv_commit IS INITIAL.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          mo_form_data->set(
            iv_key = c_id-sha1
            iv_val = lv_commit ).
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN c_event-change_type.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        mo_validation_log->clear( ).

      WHEN c_event-create.
        " Validate form entries before creating tag
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.

          mo_form_data->strict( abap_false ).
          mo_form_data->to_abap( CHANGING cs_container = ms_tag ).

          REPLACE ALL OCCURRENCES
            OF cl_abap_char_utilities=>cr_lf
            IN ms_tag-body
            WITH cl_abap_char_utilities=>newline.

          ms_tag-name = zcl_abapgit_git_tag=>add_tag_prefix( ms_tag-name ).
          ASSERT ms_tag-name CP zif_abapgit_definitions=>c_git_branch-tags.

          TRY.
              zcl_abapgit_git_porcelain=>create_tag(
                iv_url = mo_repo->get_url( )
                is_tag = ms_tag ).
            CATCH zcx_abapgit_exception INTO lx_error.
              zcx_abapgit_exception=>raise( |Cannot create tag { ms_tag-name }: { lx_error->get_text( ) }| ).
          ENDTRY.

          lv_text = |Tag { zcl_abapgit_git_tag=>remove_tag_prefix( ms_tag-name ) } created|.
          MESSAGE lv_text TYPE 'S'.

          rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

    ENDCASE.

    " If staying on form, initialize it with current settings
    IF rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      mo_form = get_form_schema( mo_form_data ).
      CREATE OBJECT mo_form_util
        EXPORTING
          io_form = mo_form.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).

    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
                    io_repo               = mo_repo
                    iv_show_commit        = abap_false
                    iv_interactive_branch = abap_false ) ).

    ri_html->add( mo_form->render( io_values         = mo_form_data
                                   io_validation_log = mo_validation_log ) ).

    ri_html->add( `</div>` ).

  ENDMETHOD.
ENDCLASS.
