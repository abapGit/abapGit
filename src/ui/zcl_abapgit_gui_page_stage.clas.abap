CLASS zcl_abapgit_gui_page_stage DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF c_action,
                 stage_all    TYPE string VALUE 'stage_all',
                 stage_commit TYPE string VALUE 'stage_commit',
               END OF c_action.

    METHODS:
      constructor
        IMPORTING
                  io_repo TYPE REF TO zcl_abapgit_repo_online
                  iv_seed TYPE string OPTIONAL
        RAISING   zcx_abapgit_exception,
      zif_abapgit_gui_page~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION,
      scripts        REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo  TYPE REF TO zcl_abapgit_repo_online,
          ms_files TYPE zif_abapgit_definitions=>ty_stage_files,
          mv_seed  TYPE string. " Unique page id to bind JS sessionStorage

    METHODS:
      render_list
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html,

      render_file
        IMPORTING iv_context     TYPE string
                  is_file        TYPE zif_abapgit_definitions=>ty_file
                  is_item        TYPE zif_abapgit_definitions=>ty_item OPTIONAL
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html,

      render_actions
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html,

      read_last_changed_by
        IMPORTING is_file        TYPE zif_abapgit_definitions=>ty_file
        RETURNING VALUE(rv_user) TYPE xubname,

      process_stage_list
        IMPORTING it_postdata TYPE cnht_post_data_tab
                  io_stage    TYPE REF TO zcl_abapgit_stage
        RAISING   zcx_abapgit_exception,

      build_menu
        RETURNING VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_STAGE IMPLEMENTATION.


  METHOD build_menu.

    CREATE OBJECT ro_menu.

    IF lines( ms_files-local ) > 0.
      ro_menu->add( iv_txt = |All diffs|
                    iv_act = |{ zif_abapgit_definitions=>gc_action-go_diff }?key={ mo_repo->get_key( ) }| ).
    ENDIF.

  ENDMETHOD. "build_menu


  METHOD constructor.

    DATA lv_ts TYPE timestamp.

    super->constructor( ).

    ms_control-page_title = 'STAGE'.
    mo_repo               = io_repo.
    ms_files              = zcl_abapgit_stage_logic=>get( mo_repo ).
    mv_seed               = iv_seed.

    IF mv_seed IS INITIAL. " Generate based on time unless obtained from diff page
      GET TIME STAMP FIELD lv_ts.
      mv_seed = |stage{ lv_ts }|.
    ENDIF.

    ms_control-page_menu  = build_menu( ).

  ENDMETHOD.


  METHOD process_stage_list.

    DATA: lv_string TYPE string,
          lt_fields TYPE tihttpnvp,
          ls_file   TYPE zif_abapgit_definitions=>ty_file.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ms_files-local,
                   <ls_item> LIKE LINE OF lt_fields.

    CONCATENATE LINES OF it_postdata INTO lv_string.
    lt_fields = zcl_abapgit_html_action_utils=>parse_fields( lv_string ).

    IF lines( lt_fields ) = 0.
      zcx_abapgit_exception=>raise( 'process_stage_list: empty list' ).
    ENDIF.

    LOOP AT lt_fields ASSIGNING <ls_item>.

      zcl_abapgit_path=>split_file_location(
        EXPORTING
          iv_fullpath = <ls_item>-name
        IMPORTING
          ev_path     = ls_file-path
          ev_filename = ls_file-filename ).

      CASE <ls_item>-value.
        WHEN zcl_abapgit_stage=>c_method-add.
          READ TABLE ms_files-local ASSIGNING <ls_file>
            WITH KEY file-path     = ls_file-path
                     file-filename = ls_file-filename.
          ASSERT sy-subrc = 0.
          io_stage->add(    iv_path     = <ls_file>-file-path
                            iv_filename = <ls_file>-file-filename
                            iv_data     = <ls_file>-file-data ).
        WHEN zcl_abapgit_stage=>c_method-ignore.
          io_stage->ignore( iv_path     = ls_file-path
                            iv_filename = ls_file-filename ).
        WHEN zcl_abapgit_stage=>c_method-rm.
          io_stage->rm(     iv_path     = ls_file-path
                            iv_filename = ls_file-filename ).
        WHEN zcl_abapgit_stage=>c_method-skip.
          " Do nothing
        WHEN OTHERS.
          zcx_abapgit_exception=>raise( |process_stage_list: unknown method { <ls_item>-value }| ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.        "process_stage_list


  METHOD read_last_changed_by.
    DATA: ls_local_file  TYPE zif_abapgit_definitions=>ty_file_item,
          lt_files_local TYPE zif_abapgit_definitions=>ty_files_item_tt.
    TRY.
        lt_files_local = mo_repo->get_files_local( ).
        READ TABLE lt_files_local INTO ls_local_file WITH KEY file = is_file.
        IF sy-subrc = 0.
          rv_user = zcl_abapgit_objects=>changed_by( ls_local_file-item ).
        ENDIF.
      CATCH zcx_abapgit_exception.
        CLEAR rv_user. "Should not raise errors if user last changed by was not found
    ENDTRY.

    rv_user = to_lower( rv_user ).
  ENDMETHOD.


  METHOD render_actions.

    DATA: lv_local_count TYPE i,
          lv_add_all_txt TYPE string.

    CREATE OBJECT ro_html.
    lv_local_count = lines( ms_files-local ).
    IF lv_local_count > 0.
      lv_add_all_txt = |Add all and commit ({ lv_local_count })|.
      " Otherwise empty, but the element (id) is preserved for JS
    ENDIF.

    ro_html->add( '<table class="w100 margin-v5"><tr>' ).

    " Action buttons
    ro_html->add( '<td class="indent5em">' ).
    ro_html->add_a( iv_act   = 'errorStub(event)' " Will be reinit by JS
                    iv_typ   = zif_abapgit_definitions=>gc_action_type-onclick
                    iv_id    = 'commitButton'
                    iv_style = 'display: none'
                    iv_txt   = 'Commit (<span id="fileCounter"></span>)'
                    iv_opt   = zif_abapgit_definitions=>gc_html_opt-strong ) ##NO_TEXT.
    ro_html->add_a( iv_act = |{ c_action-stage_all }|
                    iv_id  = 'commitAllButton'
                    iv_txt = lv_add_all_txt ) ##NO_TEXT.
    ro_html->add( '</td>' ).

    " Filter bar
    ro_html->add( '<td class="right">' ).
    ro_html->add( '<input class="stage-filter" id="objectSearch"' &&
                  ' type="search" placeholder="Filter objects">' ).
    ro_html->add( '</td>' ).

    ro_html->add( '</tr></table>' ).

  ENDMETHOD.      "render_actions


  METHOD render_content.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="repo">' ).
    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top( mo_repo ) ).
    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_js_error_banner( ) ).

    ro_html->add( '<div class="stage-container">' ).
    ro_html->add( render_actions( ) ).
    ro_html->add( render_list( ) ).
    ro_html->add( '</div>' ).

    ro_html->add( '</div>' ).

  ENDMETHOD.      "render_content


  METHOD render_file.

    DATA: lv_param    TYPE string,
          lv_filename TYPE string.

    CREATE OBJECT ro_html.

    lv_filename = is_file-path && is_file-filename.
* make sure whitespace is preserved in the DOM
    REPLACE ALL OCCURRENCES OF ` ` IN lv_filename WITH '&nbsp;'.

    ro_html->add( |<tr class="{ iv_context }">| ).

    CASE iv_context.
      WHEN 'local'.
        lv_param = zcl_abapgit_html_action_utils=>file_encode(
          iv_key  = mo_repo->get_key( )
          ig_file = is_file ).

        lv_filename = zcl_abapgit_html=>a(
          iv_txt = lv_filename
          iv_act = |{ zif_abapgit_definitions=>gc_action-go_diff }?{ lv_param }| ).
        ro_html->add( |<td class="type">{ is_item-obj_type }</td>| ).
        ro_html->add( |<td class="name">{ lv_filename }</td>| ).
        ro_html->add( |<td class="user">{ read_last_changed_by( is_file ) }</td>| ).
      WHEN 'remote'.
        ro_html->add( '<td class="type">-</td>' ).  " Dummy for object type
        ro_html->add( |<td class="name">{ lv_filename }</td>| ).
        ro_html->add( '<td></td>' ).                " Dummy for changed-by
    ENDCASE.

    ro_html->add( |<td class="status">?</td>| ).
    ro_html->add( '<td class="cmd"></td>' ). " Command added in JS
    ro_html->add( '</tr>' ).

  ENDMETHOD.  "render_file


  METHOD render_list.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF ms_files-remote,
                   <ls_local>  LIKE LINE OF ms_files-local.

    CREATE OBJECT ro_html.

    ro_html->add( '<table id="stageTab" class="stage_tab w100">' ).

    " Local changes
    LOOP AT ms_files-local ASSIGNING <ls_local>.
      AT FIRST.
        ro_html->add( '<thead><tr class="local">' ).
        ro_html->add( '<th>Type</th>' ).
        ro_html->add( '<th>Files to add (click to see diff)</th>' ).
        ro_html->add( '<th>Changed by</th>' ).
        ro_html->add( '<th></th>' ). " Status
        ro_html->add( '<th class="cmd">' ).
        ro_html->add( '<a>add</a>&#x2193; <a>reset</a>&#x2193;' ).
        ro_html->add( '</th>' ).
        ro_html->add( '</tr></thead>' ).
        ro_html->add( '<tbody>' ).
      ENDAT.

      ro_html->add( render_file(
        iv_context = 'local'
        is_file    = <ls_local>-file
        is_item    = <ls_local>-item ) ). " TODO Refactor, unify structure

      AT LAST.
        ro_html->add('</tbody>').
      ENDAT.
    ENDLOOP.

    " Remote changes
    LOOP AT ms_files-remote ASSIGNING <ls_remote>.
      AT FIRST.
        ro_html->add( '<thead><tr class="remote">' ).
        ro_html->add( '<th></th>' ). " Type
        ro_html->add( '<th colspan="2">Files to remove or non-code</th>' ).
        ro_html->add( '<th></th>' ). " Status
        ro_html->add( '<th class="cmd">' ).
        ro_html->add( '<a>ignore</a>&#x2193; <a>remove</a>&#x2193; <a>reset</a>&#x2193;' ).
        ro_html->add( '</th>' ).
        ro_html->add( '</tr></thead>' ).
        ro_html->add( '<tbody>' ).
      ENDAT.

      ro_html->add( render_file(
        iv_context = 'remote'
        is_file    = <ls_remote> ) ).

      AT LAST.
        ro_html->add('</tbody>').
      ENDAT.
    ENDLOOP.

    ro_html->add( '</table>' ).

  ENDMETHOD.      "render_lines


  METHOD scripts.

    ro_html = super->scripts( ).

    ro_html->add( 'var gStageParams = {' ).
    ro_html->add( |  seed:            "{ mv_seed }",| ). " Unique page id
    ro_html->add( '  formAction:      "stage_commit",' ).

    ro_html->add( '  ids: {' ).
    ro_html->add( '    stageTab:      "stageTab",' ).
    ro_html->add( '    commitBtn:     "commitButton",' ).
    ro_html->add( '    commitAllBtn:  "commitAllButton",' ).
    ro_html->add( '    objectSearch:  "objectSearch",' ).
    ro_html->add( '    fileCounter:   "fileCounter"' ).
    ro_html->add( '  }' ).

    ro_html->add( '}' ).
    ro_html->add( 'var gHelper = new StageHelper(gStageParams);' ).

  ENDMETHOD.  "scripts


  METHOD zif_abapgit_gui_page~on_event.

    DATA lo_stage TYPE REF TO zcl_abapgit_stage.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ms_files-local.

    CREATE OBJECT lo_stage.

    CASE iv_action.
      WHEN c_action-stage_all.
        LOOP AT ms_files-local ASSIGNING <ls_file>.
          lo_stage->add( iv_path     = <ls_file>-file-path
                         iv_filename = <ls_file>-file-filename
                         iv_data     = <ls_file>-file-data ).
        ENDLOOP.
      WHEN c_action-stage_commit.
        process_stage_list( it_postdata = it_postdata io_stage = lo_stage ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_commit
      EXPORTING
        io_repo  = mo_repo
        io_stage = lo_stage.

    ev_state = zif_abapgit_definitions=>gc_event_state-new_page.

  ENDMETHOD.
ENDCLASS.
