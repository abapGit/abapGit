CLASS zcl_abapgit_gui_page_stage DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_gui_page_hotkey.

    CONSTANTS: BEGIN OF c_action,
                 stage_all    TYPE string VALUE 'stage_all',
                 stage_commit TYPE string VALUE 'stage_commit',
                 stage_filter TYPE string VALUE 'stage_filter',
               END OF c_action.

    METHODS:
      constructor
        IMPORTING
                  io_repo TYPE REF TO zcl_abapgit_repo_online
                  iv_seed TYPE string OPTIONAL
        RAISING   zcx_abapgit_exception,
      zif_abapgit_gui_event_handler~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION,
      scripts        REDEFINITION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_changed_by,
        item TYPE zif_abapgit_definitions=>ty_item,
        name TYPE xubname,
      END OF ty_changed_by .
    TYPES:
      ty_changed_by_tt TYPE SORTED TABLE OF ty_changed_by WITH UNIQUE KEY item.
    TYPES:
      BEGIN OF ty_transport,
        item      TYPE zif_abapgit_definitions=>ty_item,
        transport TYPE trkorr,
      END OF ty_transport,
      ty_transport_tt TYPE SORTED TABLE OF ty_transport WITH UNIQUE KEY item.

    DATA mo_repo TYPE REF TO zcl_abapgit_repo_online .
    DATA ms_files TYPE zif_abapgit_definitions=>ty_stage_files .
    DATA mv_seed TYPE string .   " Unique page id to bind JS sessionStorage
    DATA mv_filter_value TYPE string.

    METHODS find_changed_by
      IMPORTING
        !it_local            TYPE zif_abapgit_definitions=>ty_files_item_tt
      RETURNING
        VALUE(rt_changed_by) TYPE ty_changed_by_tt .
    METHODS find_transports
      IMPORTING
        it_local             TYPE zif_abapgit_definitions=>ty_files_item_tt
      RETURNING
        VALUE(rt_transports) TYPE ty_transport_tt.
    METHODS render_list
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_file
      IMPORTING
        !iv_context    TYPE string
        !is_file       TYPE zif_abapgit_definitions=>ty_file
        !is_item       TYPE zif_abapgit_definitions=>ty_item OPTIONAL
        !iv_changed_by TYPE xubname OPTIONAL
        !iv_transport  TYPE trkorr OPTIONAL
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_actions
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS process_stage_list
      IMPORTING
        !it_postdata TYPE cnht_post_data_tab
        !io_stage    TYPE REF TO zcl_abapgit_stage
      RAISING
        zcx_abapgit_exception .
    METHODS build_menu
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar .
    METHODS get_page_patch
      IMPORTING iv_getdata     TYPE clike
                iv_prev_page   TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING   zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_STAGE IMPLEMENTATION.


  METHOD build_menu.

    CREATE OBJECT ro_menu.

    IF lines( ms_files-local ) > 0.
      ro_menu->add( iv_txt = |All diffs|
                    iv_act = |{ zif_abapgit_definitions=>c_action-go_diff }?key={ mo_repo->get_key( ) }| ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    DATA lv_ts TYPE timestamp.

    super->constructor( ).

    ms_control-page_title = 'STAGE'.
    mo_repo               = io_repo.
    ms_files              = zcl_abapgit_factory=>get_stage_logic( )->get( mo_repo ).
    mv_seed               = iv_seed.

    IF mv_seed IS INITIAL. " Generate based on time unless obtained from diff page
      GET TIME STAMP FIELD lv_ts.
      mv_seed = |stage{ lv_ts }|.
    ENDIF.

    ms_control-page_menu  = build_menu( ).

  ENDMETHOD.


  METHOD find_changed_by.

    DATA: ls_local      LIKE LINE OF it_local,
          ls_changed_by LIKE LINE OF rt_changed_by.

    FIELD-SYMBOLS: <ls_changed_by> LIKE LINE OF rt_changed_by.


    LOOP AT it_local INTO ls_local WHERE NOT item IS INITIAL.
      ls_changed_by-item = ls_local-item.
      INSERT ls_changed_by INTO TABLE rt_changed_by.
    ENDLOOP.

    LOOP AT rt_changed_by ASSIGNING <ls_changed_by>.
      TRY.
          <ls_changed_by>-name = to_lower( zcl_abapgit_objects=>changed_by( <ls_changed_by>-item ) ).
        CATCH zcx_abapgit_exception.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_transports.
    DATA: li_cts_api TYPE REF TO zif_abapgit_cts_api,
          ls_new     LIKE LINE OF rt_transports.

    FIELD-SYMBOLS: <ls_local> LIKE LINE OF it_local.

    li_cts_api = zcl_abapgit_factory=>get_cts_api( ).

    TRY.
        LOOP AT it_local ASSIGNING <ls_local> WHERE item IS NOT INITIAL.
          IF <ls_local>-item-obj_type IS NOT INITIAL AND
             <ls_local>-item-obj_name IS NOT INITIAL AND
             <ls_local>-item-devclass IS NOT INITIAL.

            IF li_cts_api->is_chrec_possible_for_package( <ls_local>-item-devclass ) = abap_false.
              EXIT. " Assume all other objects are also in packages without change recording

            ELSEIF li_cts_api->is_object_type_lockable( <ls_local>-item-obj_type ) = abap_true AND
                   li_cts_api->is_object_locked_in_transport( iv_object_type = <ls_local>-item-obj_type
                                                              iv_object_name = <ls_local>-item-obj_name ) = abap_true.

              ls_new-item = <ls_local>-item.

              ls_new-transport = li_cts_api->get_current_transport_for_obj(
                iv_object_type             = <ls_local>-item-obj_type
                iv_object_name             = <ls_local>-item-obj_name
                iv_resolve_task_to_request = abap_false ).

              INSERT ls_new INTO TABLE rt_transports.
            ENDIF.
          ENDIF.
        ENDLOOP.
      CATCH zcx_abapgit_exception.
        ASSERT 1 = 2.
    ENDTRY.
  ENDMETHOD.


  METHOD get_page_patch.

    DATA: lo_page   TYPE REF TO zcl_abapgit_gui_page_diff,
          lv_key    TYPE zif_abapgit_persistence=>ty_repo-key,
          lo_stage  TYPE REF TO zcl_abapgit_stage.

    zcl_abapgit_html_action_utils=>file_obj_decode(
      EXPORTING
        iv_string = iv_getdata
      IMPORTING
        ev_key    = lv_key ).

    CREATE OBJECT lo_stage.

    CREATE OBJECT lo_page
      EXPORTING
        iv_key        = lv_key
        iv_patch_mode = abap_true
        io_stage      = lo_stage.

    ri_page = lo_page.

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

          IF sy-subrc <> 0.
            zcx_abapgit_exception=>raise( |process_stage_list: unknown file { ls_file-path }{ ls_file-filename }| ).
          ENDIF.

          io_stage->add( iv_path     = <ls_file>-file-path
                         iv_filename = <ls_file>-file-filename
                         iv_data     = <ls_file>-file-data ).
        WHEN zcl_abapgit_stage=>c_method-ignore.
          io_stage->ignore( iv_path     = ls_file-path
                            iv_filename = ls_file-filename ).
        WHEN zcl_abapgit_stage=>c_method-rm.
          io_stage->rm( iv_path     = ls_file-path
                        iv_filename = ls_file-filename ).
        WHEN zcl_abapgit_stage=>c_method-skip.
          " Do nothing
        WHEN OTHERS.
          zcx_abapgit_exception=>raise( |process_stage_list: unknown method { <ls_item>-value }| ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD render_actions.

    DATA: lv_local_count TYPE i,
          lv_add_all_txt TYPE string,
          lv_param       TYPE string,
          ls_file        TYPE zif_abapgit_definitions=>ty_file.

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
                    iv_typ   = zif_abapgit_html=>c_action_type-onclick
                    iv_id    = 'commitButton'
                    iv_style = 'display: none'
                    iv_txt   = 'Commit (<span id="fileCounter"></span>)'
                    iv_opt   = zif_abapgit_html=>c_html_opt-strong ) ##NO_TEXT.
    ro_html->add_a( iv_act = |{ c_action-stage_all }|
                    iv_id  = 'commitAllButton'
                    iv_txt = lv_add_all_txt ) ##NO_TEXT.

    lv_param = zcl_abapgit_html_action_utils=>file_encode( iv_key  = mo_repo->get_key( )
                                                           ig_file = ls_file ).


    ro_html->add( '</td>' ).

    ro_html->add( '<td class="pad-sides">' ).
    ro_html->add_a(
      iv_txt = |Patch|
      iv_act = |{ zif_abapgit_definitions=>c_action-go_patch }?{ lv_param }| ).
    ro_html->add( '</td>' ).

    " Filter bar
    ro_html->add( '<td class="right">' ).
    ro_html->add( '<input class="stage-filter" id="objectSearch"' &&
                  ' type="search" placeholder="Filter objects"' &&
                  | value={ mv_filter_value }>| ).
    ro_html->add( '</td>' ).

    ro_html->add( '</tr>' ).
    ro_html->add( '</table>' ).

  ENDMETHOD.


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

  ENDMETHOD.


  METHOD render_file.

    DATA: lv_param            TYPE string,
          lv_filename         TYPE string,
          lv_transport_string TYPE string,
          lv_transport_html   TYPE string.

    CREATE OBJECT ro_html.

    lv_transport_string = iv_transport.

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
          iv_act = |{ zif_abapgit_definitions=>c_action-go_diff }?{ lv_param }| ).

        IF iv_transport IS NOT INITIAL.
          lv_transport_html = zcl_abapgit_html=>a(
            iv_txt = lv_transport_string
            iv_act = |{ zif_abapgit_definitions=>c_action-jump_transport }?{ iv_transport }| ).
        ENDIF.
        ro_html->add( |<td class="type">{ is_item-obj_type }</td>| ).
        ro_html->add( |<td class="name">{ lv_filename }</td>| ).
        ro_html->add( |<td class="user">{ iv_changed_by }</td>| ).
        ro_html->add( |<td class="transport">{ lv_transport_html }</td>| ).
      WHEN 'remote'.
        ro_html->add( '<td class="type">-</td>' ).  " Dummy for object type
        ro_html->add( |<td class="name">{ lv_filename }</td>| ).
        ro_html->add( '<td></td>' ).                " Dummy for changed-by
        ro_html->add( '<td></td>' ).                " Dummy for transport
    ENDCASE.

    ro_html->add( |<td class="status">?</td>| ).
    ro_html->add( '<td class="cmd"></td>' ). " Command added in JS

    ro_html->add( '</tr>' ).

  ENDMETHOD.


  METHOD render_list.

    DATA: lt_changed_by TYPE ty_changed_by_tt,
          ls_changed_by LIKE LINE OF lt_changed_by,
          lt_transports TYPE ty_transport_tt,
          ls_transport  LIKE LINE OF lt_transports.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF ms_files-remote,
                   <ls_local>  LIKE LINE OF ms_files-local.

    CREATE OBJECT ro_html.

    ro_html->add( '<table id="stageTab" class="stage_tab w100">' ).

    lt_changed_by = find_changed_by( ms_files-local ).
    lt_transports = find_transports( ms_files-local ).

    " Local changes
    LOOP AT ms_files-local ASSIGNING <ls_local>.
      AT FIRST.
        ro_html->add( '<thead><tr class="local">' ).
        ro_html->add( '<th>Type</th>' ).
        ro_html->add( '<th>Files to add (click to see diff)</th>' ).
        ro_html->add( '<th>Changed by</th>' ).
        ro_html->add( '<th>Transport</th>' ).
        ro_html->add( '<th></th>' ). " Status
        ro_html->add( '<th class="cmd">' ).
        ro_html->add( '<a>add</a>&#x2193; <a>reset</a>&#x2193;' ).
        ro_html->add( '</th>' ).
        ro_html->add( '</tr></thead>' ).
        ro_html->add( '<tbody>' ).
      ENDAT.

      READ TABLE lt_changed_by INTO ls_changed_by WITH KEY item = <ls_local>-item. "#EC CI_SUBRC
      READ TABLE lt_transports INTO ls_transport WITH KEY item = <ls_local>-item. "#EC CI_SUBRC

      ro_html->add( render_file(
        iv_context = 'local'
        is_file       = <ls_local>-file
        is_item       = <ls_local>-item
        iv_changed_by = ls_changed_by-name
        iv_transport  = ls_transport-transport ) ).

      CLEAR ls_transport.

      AT LAST.
        ro_html->add( '</tbody>' ).
      ENDAT.
    ENDLOOP.

    " Remote changes
    LOOP AT ms_files-remote ASSIGNING <ls_remote>.
      AT FIRST.
        ro_html->add( '<thead><tr class="remote">' ).
        ro_html->add( '<th></th>' ). " Type
        ro_html->add( '<th colspan="3">Files to remove or non-code</th>' ).
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
        ro_html->add( '</tbody>' ).
      ENDAT.
    ENDLOOP.

    ro_html->add( '</table>' ).

  ENDMETHOD.


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

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA: lo_stage  TYPE REF TO zcl_abapgit_stage,
          lv_string TYPE string,
          lt_fields TYPE tihttpnvp.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ms_files-local.


    CREATE OBJECT lo_stage.

    CLEAR: ei_page, ev_state.

    CASE iv_action.
      WHEN c_action-stage_all.

        LOOP AT ms_files-local ASSIGNING <ls_file>.
          lo_stage->add( iv_path     = <ls_file>-file-path
                         iv_filename = <ls_file>-file-filename
                         iv_data     = <ls_file>-file-data ).
        ENDLOOP.

        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_commit
          EXPORTING
            io_repo  = mo_repo
            io_stage = lo_stage.
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.

        ev_state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_action-stage_commit.

        process_stage_list( it_postdata = it_postdata io_stage = lo_stage ).

        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_commit
          EXPORTING
            io_repo  = mo_repo
            io_stage = lo_stage.
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_action-stage_filter.

        CONCATENATE LINES OF it_postdata INTO lv_string.

        lt_fields = zcl_abapgit_html_action_utils=>parse_fields( lv_string ).

        zcl_abapgit_html_action_utils=>get_field( EXPORTING iv_name  = 'filterValue'
                                                            it_field = lt_fields
                                                  CHANGING  cg_field = mv_filter_value ).

        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN zif_abapgit_definitions=>c_action-go_patch.                         " Go Patch page

        ei_page  = get_page_patch(
          iv_getdata   = iv_getdata
          iv_prev_page = iv_prev_page ).
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN OTHERS.
        super->zif_abapgit_gui_event_handler~on_event(
          EXPORTING
            iv_action    = iv_action
            iv_prev_page = iv_prev_page
            iv_getdata   = iv_getdata
            it_postdata  = it_postdata
          IMPORTING
            ei_page      = ei_page
            ev_state     = ev_state ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_hotkey~get_hotkey_actions.

    DATA: ls_hotkey_action TYPE zif_abapgit_gui_page_hotkey=>ty_hotkey_with_name.

    ls_hotkey_action-name   = |Patch|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_patch.
    ls_hotkey_action-hotkey = |p|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
