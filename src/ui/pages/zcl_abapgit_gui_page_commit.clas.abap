CLASS zcl_abapgit_gui_page_commit DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.

    CLASS-METHODS create
      IMPORTING
        !io_repo       TYPE REF TO zcl_abapgit_repo_online
        !io_stage      TYPE REF TO zcl_abapgit_stage
        !iv_sci_result TYPE zif_abapgit_definitions=>ty_sci_result DEFAULT zif_abapgit_definitions=>c_sci_result-no_run
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.
    METHODS constructor
      IMPORTING
        !io_repo       TYPE REF TO zcl_abapgit_repo_online
        !io_stage      TYPE REF TO zcl_abapgit_stage
        !iv_sci_result TYPE zif_abapgit_definitions=>ty_sci_result
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_id,
        committer       TYPE string VALUE 'committer',
        committer_name  TYPE string VALUE 'committer_name',
        committer_email TYPE string VALUE 'committer_email',
        message         TYPE string VALUE 'message',
        comment         TYPE string VALUE 'comment',
        body            TYPE string VALUE 'body',
        author          TYPE string VALUE 'author',
        author_name     TYPE string VALUE 'author_name',
        author_email    TYPE string VALUE 'author_email',
        new_branch_name TYPE string VALUE 'new_branch_name',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_event,
        commit TYPE string VALUE 'commit',
      END OF c_event.

    DATA mo_form TYPE REF TO zcl_abapgit_html_form.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_util TYPE REF TO zcl_abapgit_html_form_utils.
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.
    DATA mo_settings TYPE REF TO zcl_abapgit_settings.
    DATA mo_repo TYPE REF TO zcl_abapgit_repo_online.
    DATA mo_stage TYPE REF TO zcl_abapgit_stage.
    DATA mt_stage TYPE zif_abapgit_definitions=>ty_stage_tt.
    DATA ms_commit TYPE zif_abapgit_services_git=>ty_commit_fields.
    DATA mv_sci_result TYPE zif_abapgit_definitions=>ty_sci_result.

    METHODS render_stage_summary
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.
    METHODS render_stage_details
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception.
    METHODS validate_form
      IMPORTING
        !io_form_data            TYPE REF TO zcl_abapgit_string_map
      RETURNING
        VALUE(ro_validation_log) TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.
    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form.
    METHODS get_defaults
      RAISING
        zcx_abapgit_exception.
    METHODS get_committer_name
      RETURNING
        VALUE(rv_user) TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS get_committer_email
      RETURNING
        VALUE(rv_email) TYPE string
      RAISING
        zcx_abapgit_exception.
    METHODS get_comment_default
      RETURNING
        VALUE(rv_text) TYPE string.
    METHODS get_comment_object
      IMPORTING
        !it_stage      TYPE zif_abapgit_definitions=>ty_stage_tt
      RETURNING
        VALUE(rv_text) TYPE string.
    METHODS get_comment_file
      IMPORTING
        !it_stage      TYPE zif_abapgit_definitions=>ty_stage_tt
      RETURNING
        VALUE(rv_text) TYPE string.
    METHODS branch_name_to_internal
      IMPORTING
        iv_branch_name            TYPE string
      RETURNING
        VALUE(rv_new_branch_name) TYPE string.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_COMMIT IMPLEMENTATION.


  METHOD branch_name_to_internal.
    rv_new_branch_name = zcl_abapgit_git_branch_list=>complete_heads_branch_name(
      zcl_abapgit_git_branch_list=>normalize_branch_name( iv_branch_name ) ).
  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    mo_repo       = io_repo.
    mo_stage      = io_stage.
    mt_stage      = mo_stage->get_all( ).
    mv_sci_result = iv_sci_result.

    " Get settings from DB
    mo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).

    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_commit.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo       = io_repo
        io_stage      = io_stage
        iv_sci_result = iv_sci_result.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Commit'
      ii_child_component = lo_component ).

  ENDMETHOD.


  METHOD get_comment_default.

    rv_text = mo_settings->get_commitmsg_comment_default( ).

    IF rv_text IS INITIAL.
      RETURN.
    ENDIF.

    REPLACE '$FILE'   IN rv_text WITH get_comment_file( mt_stage ).
    REPLACE '$OBJECT' IN rv_text WITH get_comment_object( mt_stage ).

  ENDMETHOD.


  METHOD get_comment_file.

    DATA lv_count TYPE i.

    FIELD-SYMBOLS <ls_stage> LIKE LINE OF it_stage.

    lv_count = lines( it_stage ).

    IF lv_count = 1.
      " Just one file so we use the file name
      READ TABLE it_stage ASSIGNING <ls_stage> INDEX 1.
      ASSERT sy-subrc = 0.

      rv_text = <ls_stage>-file-filename.
    ELSE.
      " For multiple file we use the count instead
      rv_text = |{ lv_count } files|.
    ENDIF.

  ENDMETHOD.


  METHOD get_comment_object.

    DATA:
      lv_count TYPE i,
      ls_item  TYPE zif_abapgit_definitions=>ty_item,
      lt_items TYPE zif_abapgit_definitions=>ty_items_tt.

    FIELD-SYMBOLS <ls_stage> LIKE LINE OF it_stage.

    " Get objects
    LOOP AT it_stage ASSIGNING <ls_stage>.
      CLEAR ls_item.
      ls_item-obj_type = <ls_stage>-status-obj_type.
      ls_item-obj_name = <ls_stage>-status-obj_name.
      COLLECT ls_item INTO lt_items.
    ENDLOOP.

    lv_count = lines( lt_items ).

    IF lv_count = 1.
      " Just one object so we use the object name
      READ TABLE lt_items INTO ls_item INDEX 1.
      ASSERT sy-subrc = 0.

      CONCATENATE ls_item-obj_type ls_item-obj_name INTO rv_text SEPARATED BY space.
    ELSE.
      " For multiple objects we use the count instead
      rv_text = |{ lv_count } objects|.
    ENDIF.

  ENDMETHOD.


  METHOD get_committer_email.

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


  METHOD get_committer_name.

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


  METHOD get_defaults.

    ms_commit-committer_name  = get_committer_name( ).
    ms_commit-committer_email = get_committer_email( ).
    ms_commit-comment         = get_comment_default( ).

    " Committer
    mo_form_data->set(
      iv_key = c_id-committer_name
      iv_val = ms_commit-committer_name ).
    mo_form_data->set(
      iv_key = c_id-committer_email
      iv_val = ms_commit-committer_email ).

    " Message
    mo_form_data->set(
      iv_key = c_id-comment
      iv_val = ms_commit-comment ).

  ENDMETHOD.


  METHOD get_form_schema.

    DATA: lv_commitmsg_comment_length TYPE i.

    ro_form = zcl_abapgit_html_form=>create(
      iv_form_id   = 'commit-form'
      iv_help_page = 'https://docs.abapgit.org/guide-stage-commit.html' ).

    lv_commitmsg_comment_length =  mo_settings->get_commitmsg_comment_length( ).

    ro_form->text(
      iv_name        = c_id-comment
      iv_label       = 'Comment'
      iv_required    = abap_true
      iv_max         = lv_commitmsg_comment_length
      iv_placeholder = |Add a mandatory comment with max { lv_commitmsg_comment_length } characters|
    )->textarea(
      iv_name        = c_id-body
      iv_label       = 'Body'
      iv_rows        = 6
      iv_cols        = mo_settings->get_commitmsg_body_size( )
      iv_placeholder = 'Add an optional description...'
    )->text(
      iv_name        = c_id-committer_name
      iv_label       = 'Committer Name'
      iv_required    = abap_true
    )->text(
      iv_name        = c_id-committer_email
      iv_label       = 'Committer Email'
      iv_required    = abap_true ).

    IF mo_settings->get_commitmsg_hide_author( ) IS INITIAL.
      ro_form->text(
        iv_name        = c_id-author_name
        iv_label       = 'Author Name'
        iv_placeholder = 'Optionally, specify an author (same as committer by default)'
      )->text(
        iv_name        = c_id-author_email
        iv_label       = 'Author Email' ).
    ENDIF.

    ro_form->text(
      iv_name        = c_id-new_branch_name
      iv_label       = 'New Branch Name'
      iv_placeholder = 'Optionally, enter a new branch name for this commit'
      iv_condense    = abap_true ).


    ro_form->command(
      iv_label       = 'Commit'
      iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-commit
    )->command(
      iv_label       = 'Back'
      iv_action      = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD render_stage_details.

    FIELD-SYMBOLS <ls_stage> LIKE LINE OF mt_stage.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<table class="stage_tab">' ).
    ri_html->add( '<thead>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<th colspan="3">Staged Files (See <a href="#top">Summary</a> Above)</th>' ).
    ri_html->add( '</tr>' ).
    ri_html->add( '</thead>' ).

    ri_html->add( '<tbody>' ).
    LOOP AT mt_stage ASSIGNING <ls_stage>.
      ri_html->add( '<tr>' ).
      ri_html->add( '<td>' ).
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_item_state(
        iv_lstate = <ls_stage>-status-lstate
        iv_rstate = <ls_stage>-status-rstate ) ).
      ri_html->add( '</td>' ).
      ri_html->add( '<td class="method">' ).
      ri_html->add( zcl_abapgit_stage=>method_description( <ls_stage>-method ) ).
      ri_html->add( '</td>' ).
      ri_html->add( '<td>' ).
      ri_html->add( <ls_stage>-file-path && <ls_stage>-file-filename ).
      ri_html->add( '</td>' ).
      ri_html->add( '</tr>' ).
    ENDLOOP.
    ri_html->add( '</tbody>' ).

    ri_html->add( '</table>' ).

  ENDMETHOD.


  METHOD render_stage_summary.

    DATA:
      BEGIN OF ls_sum,
        method TYPE string,
        count  TYPE i,
      END OF ls_sum,
      lt_sum LIKE STANDARD TABLE OF ls_sum WITH DEFAULT KEY.

    FIELD-SYMBOLS <ls_stage> LIKE LINE OF mt_stage.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    LOOP AT mt_stage ASSIGNING <ls_stage>.
      ls_sum-method = <ls_stage>-method.
      ls_sum-count  = 1.
      COLLECT ls_sum INTO lt_sum.
    ENDLOOP.

    ri_html->add( 'Stage Summary: ' ).

    READ TABLE lt_sum INTO ls_sum WITH TABLE KEY method = zif_abapgit_definitions=>c_method-add.
    IF sy-subrc = 0.
      ri_html->add( |<span class="diff_banner diff_ins" title="add">+ { ls_sum-count }</span>| ).
    ENDIF.
    READ TABLE lt_sum INTO ls_sum WITH TABLE KEY method = zif_abapgit_definitions=>c_method-rm.
    IF sy-subrc = 0.
      ri_html->add( |<span class="diff_banner diff_del" title="remove">- { ls_sum-count }</span>| ).
    ENDIF.
    READ TABLE lt_sum INTO ls_sum WITH TABLE KEY method = zif_abapgit_definitions=>c_method-ignore.
    IF sy-subrc = 0.
      ri_html->add( |<span class="diff_banner diff_upd" title="ignore">~ { ls_sum-count }</span>| ).
    ENDIF.

    IF lines( mt_stage ) = 1.
      ri_html->add( 'file' ).
    ELSE.
      ri_html->add( 'files' ).
    ENDIF.

    ri_html->add( '(See <a href="#stage-details">Details</a> Below)' ).

  ENDMETHOD.


  METHOD validate_form.

    DATA: lt_branches        TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt,
          lv_new_branch_name TYPE string.

    ro_validation_log = mo_form_util->validate( io_form_data ).

    IF zcl_abapgit_utils=>is_valid_email( io_form_data->get( c_id-committer_email ) ) = abap_false.
      ro_validation_log->set(
        iv_key = c_id-committer_email
        iv_val = |Invalid email address| ).
    ENDIF.

    IF zcl_abapgit_utils=>is_valid_email( io_form_data->get( c_id-author_email ) ) = abap_false.
      ro_validation_log->set(
        iv_key = c_id-author_email
        iv_val = |Invalid email address| ).
    ENDIF.

    lv_new_branch_name = io_form_data->get( c_id-new_branch_name ).
    IF lv_new_branch_name IS NOT INITIAL.
      " check if branch already exists
      lt_branches = zcl_abapgit_git_transport=>branches( mo_repo->get_url( ) )->get_branches_only( ).
      READ TABLE lt_branches TRANSPORTING NO FIELDS WITH TABLE KEY name_key
        COMPONENTS name = branch_name_to_internal( lv_new_branch_name ).
      IF sy-subrc = 0.
        ro_validation_log->set(
          iv_key = c_id-new_branch_name
          iv_val = |Branch already exists| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.
    DATA lv_new_branch_name   TYPE string.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN zif_abapgit_definitions=>c_action-go_back.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_event-commit.
        " Validate form entries before committing
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.

          " new branch fields not needed in commit data
          mo_form_data->strict( abap_false ).

          mo_form_data->to_abap( CHANGING cs_container = ms_commit ).

          REPLACE ALL OCCURRENCES
            OF cl_abap_char_utilities=>cr_lf
            IN ms_commit-body
            WITH cl_abap_char_utilities=>newline.

          lv_new_branch_name = mo_form_data->get( c_id-new_branch_name ).
          " create new branch and commit to it if branch name is not empty
          IF lv_new_branch_name IS NOT INITIAL.
            lv_new_branch_name = branch_name_to_internal( lv_new_branch_name ).
            " creates a new branch and automatically switches to it
            mo_repo->create_branch( lv_new_branch_name ).
          ENDIF.

          zcl_abapgit_services_git=>commit(
            is_commit = ms_commit
            io_repo   = mo_repo
            io_stage  = mo_stage ).


          MESSAGE 'Commit was successful' TYPE 'S'.

          rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back_to_bookmark.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    IF mo_form_util->is_empty( mo_form_data ) = abap_true.
      get_defaults( ).
    ENDIF.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="repo">' ).
    ri_html->add( '<div id="top" class="paddings">' ).
    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top( mo_repo ) ).
    ri_html->add( '</div>' ).

    ri_html->add( '<div id="stage-summary" class="dialog w800px paddings">' ).
    ri_html->add( render_stage_summary( ) ).
    ri_html->add( '</div>' ).

    ri_html->add( mo_form->render(
      iv_form_class     = 'w800px'
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).

    ri_html->add( '<div id="stage-details" class="dialog w800px">' ).
    ri_html->add( render_stage_details( ) ).
    ri_html->add( '</div>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
