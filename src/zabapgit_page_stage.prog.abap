*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_STAGE
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_stage DEFINITION FINAL INHERITING FROM lcl_gui_page.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF c_action,
                 stage_all    TYPE string VALUE 'stage_all',
                 stage_commit TYPE string VALUE 'stage_commit',
               END OF c_action.

    METHODS:
      constructor
        IMPORTING
          io_repo         TYPE REF TO lcl_repo_online
        RAISING   lcx_exception,
      lif_gui_page~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION,
      scripts        REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo  TYPE REF TO lcl_repo_online,
          ms_files TYPE ty_stage_files,
          mo_stage TYPE REF TO lcl_stage,
          mv_ts    TYPE timestamp.

    METHODS:
      render_list
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html,
      render_file
        IMPORTING is_file        TYPE ty_file
                  iv_context     TYPE string
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html,
      render_menu
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html,
      read_last_changed_by
        IMPORTING is_file        TYPE ty_file
        RETURNING VALUE(rv_user) TYPE xubname.

    METHODS process_stage_list
      IMPORTING it_postdata TYPE cnht_post_data_tab
      RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_gui_page_stage IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).
    ms_control-page_title = 'STAGE'.
    mo_repo = io_repo.

    ms_files = lcl_stage_logic=>get( mo_repo ).

    CREATE OBJECT mo_stage
      EXPORTING
        iv_branch_name = io_repo->get_branch_name( )
        iv_branch_sha1 = io_repo->get_sha1_remote( ).

    GET TIME STAMP FIELD mv_ts.

  ENDMETHOD.

  METHOD lif_gui_page~on_event.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ms_files-local.

    CASE iv_action.
      WHEN c_action-stage_all.
        mo_stage->reset_all( ).
        LOOP AT ms_files-local ASSIGNING <ls_file>.
          mo_stage->add( iv_path     = <ls_file>-file-path
                         iv_filename = <ls_file>-file-filename
                         iv_data     = <ls_file>-file-data ).
        ENDLOOP.
      WHEN c_action-stage_commit.
        mo_stage->reset_all( ).
        process_stage_list( it_postdata ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    CREATE OBJECT ei_page TYPE lcl_gui_page_commit
      EXPORTING
        io_repo  = mo_repo
        io_stage = mo_stage.

    ev_state = gc_event_state-new_page.

  ENDMETHOD.

  METHOD process_stage_list.

    DATA: lv_string TYPE string,
          lt_fields TYPE tihttpnvp,
          ls_file   TYPE ty_file.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ms_files-local,
                   <ls_item> LIKE LINE OF lt_fields.

    CONCATENATE LINES OF it_postdata INTO lv_string.
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( |{ lv_string }| ).

    IF lines( lt_fields ) = 0.
      lcx_exception=>raise( 'process_stage_list: empty list' ).
    ENDIF.

    LOOP AT lt_fields ASSIGNING <ls_item>.

      lcl_path=>split_file_location( EXPORTING iv_fullpath = <ls_item>-name
                                     IMPORTING ev_path     = ls_file-path
                                               ev_filename = ls_file-filename ).
      CASE <ls_item>-value.
        WHEN lcl_stage=>c_method-add.
          READ TABLE ms_files-local ASSIGNING <ls_file>
            WITH KEY file-path     = ls_file-path
                     file-filename = ls_file-filename.
          ASSERT sy-subrc = 0.
          mo_stage->add(    iv_path     = <ls_file>-file-path
                            iv_filename = <ls_file>-file-filename
                            iv_data     = <ls_file>-file-data ).
        WHEN lcl_stage=>c_method-ignore.
          mo_stage->ignore( iv_path     = ls_file-path
                            iv_filename = ls_file-filename ).
        WHEN lcl_stage=>c_method-rm.
          mo_stage->rm(     iv_path     = ls_file-path
                            iv_filename = ls_file-filename ).
        WHEN lcl_stage=>c_method-skip.
          " Do nothing
        WHEN OTHERS.
          lcx_exception=>raise( |process_stage_list: unknown method { <ls_item>-value }| ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.        "process_stage_list

  METHOD render_list.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF ms_files-remote,
                   <ls_local>  LIKE LINE OF ms_files-local.

    CREATE OBJECT ro_html.

    ro_html->add( '<table id="stage_tab" class="stage_tab">' ).

    " Local changes
    LOOP AT ms_files-local ASSIGNING <ls_local>.
      AT FIRST.
        ro_html->add('<thead><tr>').
        ro_html->add('<th></th><th colspan="2">LOCAL</th>' ).
        ro_html->add('<th>' ).
        IF lines( ms_files-local ) > 1.
          ro_html->add_a( iv_txt = |{ lines( ms_files-local ) } diffs|
                          iv_act = |{ gc_action-go_diff }?key={ mo_repo->get_key( ) }| ).
        ENDIF.
        ro_html->add('</th>').
        ro_html->add('<th>Last changed by</th>').
        ro_html->add('</tr></thead>').
        ro_html->add('<tbody class="local">').
      ENDAT.

      ro_html->add( render_file( is_file = <ls_local>-file iv_context = 'local' ) ).

      AT LAST.
        ro_html->add('</tbody>').
      ENDAT.
    ENDLOOP.

    " Remote changes
    LOOP AT ms_files-remote ASSIGNING <ls_remote>.
      AT FIRST.
        ro_html->add('<thead><tr>').
        ro_html->add('<th></th><th colspan="3">REMOTE</th>' ).
        ro_html->add('</tr></thead>').
        ro_html->add('<tbody class="remote">').
      ENDAT.

      ro_html->add( render_file( is_file = <ls_remote> iv_context = 'remote' ) ).

      AT LAST.
        ro_html->add('</tbody>').
      ENDAT.
    ENDLOOP.

    ro_html->add( '</table>' ).

  ENDMETHOD.      "render_lines

  METHOD render_file.

    DATA: lv_param TYPE string,
          lv_user  TYPE xubname.

    CREATE OBJECT ro_html.

    ro_html->add( |<tr class="{ iv_context }">| ).
    ro_html->add( |<td class="status" style="color: #CCC">?</td>| ).
    ro_html->add( |<td>{ is_file-path && is_file-filename }</td>| ).

    CASE iv_context.
      WHEN 'local'.
        lv_param = lcl_html_action_utils=>file_encode( iv_key  = mo_repo->get_key( )
                                                       ig_file = is_file ).
        ro_html->add( '<td class="cmd"><a>add</a></td>' ).
        ro_html->add( '<td>' ).
        ro_html->add_a( iv_txt = 'diff' iv_act = |{ gc_action-go_diff }?{ lv_param }| ).
        ro_html->add( '</td>' ).

        lv_user = read_last_changed_by( is_file ).
        ro_html->add( |<td>{ lv_user }</td> | ).
      WHEN 'remote'.
        ro_html->add( '<td class="cmd"><a>ignore</a><a>remove</a></td>' ).
        ro_html->add( |<td><span class="grey">-</span></td>| ).
    ENDCASE.

    ro_html->add( '</tr>' ).

  ENDMETHOD.  "render_file

  METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="repo">' ).
    ro_html->add( lcl_gui_chunk_lib=>render_repo_top( mo_repo ) ).
    ro_html->add( render_menu( ) ).
    ro_html->add( render_list( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.      "render_content

  METHOD render_menu.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="paddings">' ).
    ro_html->add_a( iv_act   = 'gHelper.submit();'
                    iv_typ   = gc_action_type-onclick
                    iv_id    = 'act_commit'
                    iv_style = 'display: none'
                    iv_txt   = 'Commit'
                    iv_opt   = gc_html_opt-strong ) ##NO_TEXT.
    IF lines( ms_files-local ) > 0.
      ro_html->add_a( iv_act = |{ c_action-stage_all }|
                      iv_id  = 'act_commit_all'
                      iv_txt = 'Add all and commit') ##NO_TEXT.
    ENDIF.
    ro_html->add( '</div>' ).

  ENDMETHOD.      "render_menu

  METHOD scripts.

    CREATE OBJECT ro_html.

    ro_html->add( 'var gStageParams = {' ).
    ro_html->add( |  seed:            "stage{ mv_ts }",| ).
    ro_html->add( '  stageTabId:      "stage_tab",' ).
    ro_html->add( '  formAction:      "stage_commit",' ).
    ro_html->add( '  commitNodeId:    "act_commit",' ).
    ro_html->add( '  commitAllNodeId: "act_commit_all"' ).
    ro_html->add( '}' ).
    ro_html->add( 'var gHelper = new StageHelper(gStageParams);' ).

  ENDMETHOD.  "scripts

  METHOD read_last_changed_by.
    DATA: ls_local_file TYPE ty_file_item,
          lt_files_local type ty_files_item_tt.
    TRY.
        lt_files_local = mo_repo->get_files_local( ).
        READ TABLE lt_files_local INTO ls_local_file WITH KEY file = is_file.
        IF sy-subrc = 0.
          rv_user = lcl_objects=>changed_by( ls_local_file-item ).
        ENDIF.
      CATCH lcx_exception.
        CLEAR rv_user. "Should not raise errors if user last changed by was not found
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
