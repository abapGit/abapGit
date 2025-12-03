CLASS zcl_abapgit_gui_page_flowcons DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .
    INTERFACES zif_abapgit_gui_menu_provider .

    CLASS-METHODS create
      IMPORTING
        ii_repo        TYPE REF TO zif_abapgit_repo_online
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .

    METHODS constructor
      IMPORTING
        ii_repo TYPE REF TO zif_abapgit_repo_online
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        refresh              TYPE string VALUE 'refresh',
        stage_missing_remote TYPE string VALUE 'stage_missing_remote',
        stage_only_remote    TYPE string VALUE 'stage_only_remote',
      END OF c_action .

    DATA mo_repo TYPE REF TO zif_abapgit_repo_online.
    DATA ms_consolidate TYPE zif_abapgit_flow_logic=>ty_consolidate.

    METHODS stage_missing_remote
      RAISING zcx_abapgit_exception.

    METHODS stage_only_remote
      RAISING zcx_abapgit_exception.

    METHODS push
      IMPORTING
        io_stage       TYPE REF TO zcl_abapgit_stage
        iv_sha1        TYPE zif_abapgit_git_definitions=>ty_sha1
        iv_branch_name TYPE string
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS zcl_abapgit_gui_page_flowcons IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    mo_repo = ii_repo.
  ENDMETHOD.

  METHOD stage_missing_remote.
* Stage and commit "ms_consolidate-missing_remote" files to new branch

    DATA lt_branches    TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA ls_main_branch LIKE LINE OF lt_branches.
    DATA lv_branch_name TYPE string.
    DATA lt_filter      TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA ls_filter      LIKE LINE OF lt_filter.
    DATA ls_file        LIKE LINE OF ms_consolidate-missing_remote.
    DATA lv_package     TYPE devclass.
    DATA ls_item        TYPE zif_abapgit_definitions=>ty_item.
    DATA lt_local       TYPE zif_abapgit_definitions=>ty_files_item_tt.
    DATA ls_local       LIKE LINE OF lt_local.
    DATA lo_filter      TYPE REF TO zcl_abapgit_object_filter_obj.
    DATA lo_dot         TYPE REF TO zcl_abapgit_dot_abapgit.
    DATA lo_stage       TYPE REF TO zcl_abapgit_stage.


    lt_branches = zcl_abapgit_git_factory=>get_v2_porcelain( )->list_branches(
      iv_url    = mo_repo->get_url( )
      iv_prefix = zif_abapgit_git_definitions=>c_git_branch-heads_prefix && zif_abapgit_flow_logic=>c_main )->get_all( ).
    ASSERT lines( lt_branches ) = 1.

    READ TABLE lt_branches INDEX 1 INTO ls_main_branch.
    ASSERT sy-subrc = 0.

    lv_branch_name = |consolidate{ sy-datum }{ sy-uzeit }|.
    zcl_abapgit_git_porcelain=>create_branch(
      iv_url  = mo_repo->get_url( )
      iv_name = |{ zif_abapgit_git_definitions=>c_git_branch-heads_prefix }{ lv_branch_name }|
      iv_from = ls_main_branch-sha1 ).

    lv_package = mo_repo->zif_abapgit_repo~get_package( ).
    lo_dot = mo_repo->zif_abapgit_repo~get_dot_abapgit( ).

    LOOP AT ms_consolidate-missing_remote INTO ls_file.
      zcl_abapgit_filename_logic=>file_to_object(
        EXPORTING
          iv_filename = ls_file-filename
          iv_path     = ls_file-path
          iv_devclass = lv_package
          io_dot      = lo_dot
        IMPORTING
          es_item     = ls_item ).
      ls_filter-object = ls_item-obj_type.
      ls_filter-obj_name = ls_item-obj_name.
      INSERT ls_filter INTO TABLE lt_filter.
    ENDLOOP.
    SORT lt_filter BY object obj_name.
    DELETE ADJACENT DUPLICATES FROM lt_filter COMPARING object obj_name.

    CREATE OBJECT lo_filter EXPORTING it_filter = lt_filter.
    lt_local = mo_repo->zif_abapgit_repo~get_files_local_filtered( lo_filter ).

* just add all files, some will match, but its okay
    CREATE OBJECT lo_stage.
    LOOP AT lt_local INTO ls_local.
      lo_stage->add( iv_path     = ls_local-file-path
                     iv_filename = ls_local-file-filename
                     iv_data     = ls_local-file-data ).
    ENDLOOP.

    push(
      io_stage       = lo_stage
      iv_branch_name = lv_branch_name
      iv_sha1        = ls_main_branch-sha1 ).

  ENDMETHOD.

  METHOD stage_only_remote.
* remove and commit "ms_consolidate-only_remote" files to new branch

    DATA lt_branches    TYPE zif_abapgit_git_definitions=>ty_git_branch_list_tt.
    DATA ls_main_branch LIKE LINE OF lt_branches.
    DATA lv_branch_name TYPE string.
    DATA ls_file        LIKE LINE OF ms_consolidate-missing_remote.
    DATA lo_dot         TYPE REF TO zcl_abapgit_dot_abapgit.
    DATA lo_stage       TYPE REF TO zcl_abapgit_stage.


    lt_branches = zcl_abapgit_git_factory=>get_v2_porcelain( )->list_branches(
      iv_url    = mo_repo->get_url( )
      iv_prefix = zif_abapgit_git_definitions=>c_git_branch-heads_prefix && zif_abapgit_flow_logic=>c_main )->get_all( ).
    ASSERT lines( lt_branches ) = 1.

    READ TABLE lt_branches INDEX 1 INTO ls_main_branch.
    ASSERT sy-subrc = 0.

    lv_branch_name = |consolidate{ sy-datum }{ sy-uzeit }|.
    zcl_abapgit_git_porcelain=>create_branch(
      iv_url  = mo_repo->get_url( )
      iv_name = |{ zif_abapgit_git_definitions=>c_git_branch-heads_prefix }{ lv_branch_name }|
      iv_from = ls_main_branch-sha1 ).

    lo_dot = mo_repo->zif_abapgit_repo~get_dot_abapgit( ).

    CREATE OBJECT lo_stage.
    LOOP AT ms_consolidate-only_remote INTO ls_file.
      lo_stage->rm( iv_path     = ls_file-path
                    iv_filename = ls_file-filename ).
    ENDLOOP.

    push(
      io_stage       = lo_stage
      iv_branch_name = lv_branch_name
      iv_sha1        = ls_main_branch-sha1 ).

  ENDMETHOD.

  METHOD push.

    DATA ls_comment TYPE zif_abapgit_git_definitions=>ty_comment.
    DATA lt_sha1    TYPE zif_abapgit_git_definitions=>ty_sha1_tt.
    DATA lt_objects TYPE zif_abapgit_definitions=>ty_objects_tt.

    ls_comment-committer-name  = 'consolidate'.
    ls_comment-committer-email = 'consolidate@localhost'.
    ls_comment-comment         = |Consolidate { sy-datum DATE = ISO } { sy-uzeit TIME = ISO }\n| &&
      |User: { sy-uname }|.

    INSERT iv_sha1 INTO TABLE lt_sha1.
    lt_objects = zcl_abapgit_git_factory=>get_v2_porcelain( )->list_no_blobs_multi(
      iv_url  = mo_repo->get_url( )
      it_sha1 = lt_sha1 ).

    zcl_abapgit_git_porcelain=>push(
      is_comment     = ls_comment
      io_stage       = io_stage
      iv_branch_name = zif_abapgit_git_definitions=>c_git_branch-heads_prefix && iv_branch_name
      iv_url         = mo_repo->get_url( )
      iv_parent      = iv_sha1
      it_old_objects = lt_objects ).

    MESSAGE 'Done, commited' TYPE 'S'.

  ENDMETHOD.

  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_flowcons.

    CREATE OBJECT lo_component EXPORTING ii_repo = ii_repo.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Flow Consolidate'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-stage_missing_remote.
        stage_missing_remote( ).
        CLEAR ms_consolidate.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-stage_only_remote.
        stage_only_remote( ).
        CLEAR ms_consolidate.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-refresh.
        CLEAR ms_consolidate.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN zif_abapgit_definitions=>c_action-go_file_diff.
        rs_handled = zcl_abapgit_flow_page_utils=>call_diff( ii_event ).
      WHEN OTHERS.
* the back button is handled in the default router
        RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    ro_toolbar = zcl_abapgit_html_toolbar=>create( 'toolbar-flow' ).

    ro_toolbar->add(
      iv_txt = 'Refresh'
      iv_act = c_action-refresh ).

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA lv_text    TYPE string.
    DATA lo_timer   TYPE REF TO zcl_abapgit_timer.
    DATA li_repo    TYPE REF TO zif_abapgit_repo.
    DATA lo_toolbar TYPE REF TO zcl_abapgit_html_toolbar.

    register_handlers( ).

    li_repo ?= mo_repo.
    lo_timer = zcl_abapgit_timer=>create( )->start( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( '<div class="repo-overview">' ).

    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
      ii_repo                 = mo_repo
      iv_interactive_favorite = abap_false
      iv_show_commit          = abap_false
      iv_show_branch          = abap_false ) ).

    IF ms_consolidate IS INITIAL.
      ms_consolidate = zcl_abapgit_flow_logic=>consolidate( mo_repo ).
    ENDIF.

    LOOP AT ms_consolidate-errors INTO lv_text.
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_error( iv_error = lv_text ) ).
    ENDLOOP.
    LOOP AT ms_consolidate-warnings INTO lv_text.
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_warning_banner( lv_text ) ).
    ENDLOOP.
    LOOP AT ms_consolidate-success INTO lv_text.
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_success( lv_text ) ).
    ENDLOOP.

    IF lines( ms_consolidate-missing_remote ) > 0.
      ri_html->add( '<h2>Missing Remote Files</h2>' ).
      CREATE OBJECT lo_toolbar EXPORTING iv_id = 'toolbar-flow-cons'.
      lo_toolbar->add( iv_txt = |Stage and commit { lines( ms_consolidate-missing_remote ) } files to new branch|
                       iv_act = c_action-stage_missing_remote
                       iv_opt = zif_abapgit_html=>c_html_opt-strong ).
      ri_html->add( lo_toolbar->render( ) ).

      ri_html->add( zcl_abapgit_flow_page_utils=>render_table(
        it_files    = ms_consolidate-missing_remote
        iv_repo_key = li_repo->get_key( ) ) ).
    ENDIF.

    IF lines( ms_consolidate-only_remote ) > 0.
      ri_html->add( '<h2>Files Only Remote</h2>' ).
      CREATE OBJECT lo_toolbar EXPORTING iv_id = 'toolbar-flow-cons'.
      lo_toolbar->add( iv_txt = |Remove and commit { lines( ms_consolidate-only_remote ) } files to new branch|
                       iv_act = c_action-stage_only_remote
                       iv_opt = zif_abapgit_html=>c_html_opt-strong ).
      ri_html->add( lo_toolbar->render( ) ).

      ri_html->add( zcl_abapgit_flow_page_utils=>render_table(
        it_files    = ms_consolidate-only_remote
        iv_repo_key = li_repo->get_key( ) ) ).
    ENDIF.

    ri_html->add( |<br><br><small>{ lo_timer->end( ) }</small><br>| ).

    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
