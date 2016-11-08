*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_VIEW_REPO
*&---------------------------------------------------------------------*

CLASS lcl_repo_content_browser DEFINITION FINAL.

  PUBLIC SECTION.

    CONSTANTS: BEGIN OF c_sortkey,
                 default    TYPE i VALUE 9999,
                 parent_dir TYPE i VALUE 0,
                 dir        TYPE i VALUE 1,
                 orphan     TYPE i VALUE 2,
                 changed    TYPE i VALUE 3,
               END OF c_sortkey.

    TYPES: BEGIN OF ty_repo_item,
             obj_type TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             sortkey  TYPE i,
             path     TYPE string,
             is_dir   TYPE abap_bool,
             changes  TYPE i,
             lstate   TYPE char1,
             rstate   TYPE char1,
             files    TYPE tt_repo_files,
           END OF ty_repo_item.
    TYPES tt_repo_items TYPE STANDARD TABLE OF ty_repo_item WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING io_repo TYPE REF TO lcl_repo.

    METHODS list
      IMPORTING iv_path              TYPE string
                iv_by_folders        TYPE abap_bool
                iv_changes_only      TYPE abap_bool
      RETURNING VALUE(rt_repo_items) TYPE tt_repo_items
      RAISING   lcx_exception.

    METHODS get_log
      RETURNING VALUE(ro_log) TYPE REF TO lcl_log.

  PRIVATE SECTION.
    DATA mo_repo TYPE REF TO lcl_repo.
    DATA mo_log  TYPE REF TO lcl_log.

    METHODS get_local
      RETURNING VALUE(rt_repo_items) TYPE tt_repo_items
      RAISING   lcx_exception.

    METHODS get_remote
      RETURNING VALUE(rt_repo_items) TYPE tt_repo_items
      RAISING   lcx_exception.

    METHODS build_folders
      IMPORTING iv_cur_dir    TYPE string
      CHANGING  ct_repo_items TYPE tt_repo_items
      RAISING   lcx_exception.

    METHODS filter_changes
      CHANGING ct_repo_items TYPE tt_repo_items.

ENDCLASS. "lcl_repo_content_browser

CLASS lcl_repo_content_browser IMPLEMENTATION.

  METHOD constructor.
    mo_repo = io_repo.
  ENDMETHOD.  "constructor

  METHOD get_log.
    ro_log = mo_log.
  ENDMETHOD. "get_log

  METHOD list.

    CREATE OBJECT mo_log.

    IF mo_repo->is_offline( ) = abap_true.
      rt_repo_items = get_local( ).
    ELSE.
      rt_repo_items = get_remote( ).
    ENDIF.

    IF iv_by_folders = abap_true.
      build_folders(
        EXPORTING iv_cur_dir    = iv_path
        CHANGING  ct_repo_items = rt_repo_items ).
    ENDIF.

    IF iv_changes_only = abap_true.
      filter_changes( CHANGING ct_repo_items = rt_repo_items ).
    ENDIF.

    SORT rt_repo_items BY sortkey obj_type obj_name ASCENDING.

  ENDMETHOD.  "list

  DEFINE _reduce_state.
    " &1 - prev, &2 - cur
    IF &1 = &2 OR &2 IS INITIAL.
      ASSERT 1 = 1. " No change
    ELSEIF &1 IS INITIAL.
      &1 = &2.
    ELSE.
      &1 = gc_state-mixed.
    ENDIF.
  END-OF-DEFINITION.

  METHOD build_folders.

    DATA: lv_index    TYPE i,
          lt_subitems LIKE ct_repo_items,
          ls_subitem  LIKE LINE OF ct_repo_items,
          ls_folder   LIKE LINE OF ct_repo_items.

    FIELD-SYMBOLS <item> LIKE LINE OF ct_repo_items.

    LOOP AT ct_repo_items ASSIGNING <item>.
      lv_index = sy-tabix.
      CHECK <item>-path <> iv_cur_dir. " files in target dir - just leave them be

      IF lcl_path=>is_subdir( iv_path = <item>-path  iv_parent = iv_cur_dir ) = abap_true.
        ls_subitem-changes = <item>-changes.
        ls_subitem-path    = <item>-path.
        ls_subitem-lstate  = <item>-lstate.
        ls_subitem-rstate  = <item>-rstate.
        APPEND ls_subitem TO lt_subitems.
      ENDIF.

      DELETE ct_repo_items INDEX lv_index.
    ENDLOOP.

    SORT lt_subitems BY path ASCENDING.

    LOOP AT lt_subitems ASSIGNING <item>.
      AT NEW path.
        CLEAR ls_folder.
        ls_folder-path    = <item>-path.
        ls_folder-sortkey = c_sortkey-dir. " Directory
        ls_folder-is_dir  = abap_true.
      ENDAT.

      ls_folder-changes = ls_folder-changes + <item>-changes.
      _reduce_state ls_folder-lstate <item>-lstate.
      _reduce_state ls_folder-rstate <item>-rstate.

      AT END OF path.
        APPEND ls_folder TO ct_repo_items.
      ENDAT.
    ENDLOOP.

  ENDMETHOD. "build_folders

  METHOD filter_changes.

    DATA lt_repo_temp LIKE ct_repo_items.

    FIELD-SYMBOLS <item> LIKE LINE OF ct_repo_items.

    LOOP AT ct_repo_items ASSIGNING <item>.
      CHECK <item>-changes > 0.
      APPEND <item> TO lt_repo_temp.
    ENDLOOP.

    IF lines( lt_repo_temp ) > 0. " Prevent showing empty package if no changes, show all
      ct_repo_items = lt_repo_temp.
    ENDIF.

  ENDMETHOD. "filter_changes

  METHOD get_local.

    DATA: lt_tadir       TYPE ty_tadir_tt.

    FIELD-SYMBOLS: <ls_repo_item> LIKE LINE OF rt_repo_items,
                   <ls_tadir>     LIKE LINE OF lt_tadir.

    lt_tadir = lcl_tadir=>read( mo_repo->get_package( ) ).
    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      APPEND INITIAL LINE TO rt_repo_items ASSIGNING <ls_repo_item>.
      <ls_repo_item>-obj_type = <ls_tadir>-object.
      <ls_repo_item>-obj_name = <ls_tadir>-obj_name.
      <ls_repo_item>-path     = '/' && <ls_tadir>-path. " Add root anchor
      <ls_repo_item>-sortkey  = c_sortkey-default.      " Default sort key
    ENDLOOP.

  ENDMETHOD.  "get_local

  METHOD get_remote.

    DATA: lo_repo_online TYPE REF TO lcl_repo_online,
          ls_file        TYPE ty_repo_file,
          lt_status      TYPE ty_results_tt.

    FIELD-SYMBOLS: <status>       LIKE LINE OF lt_status,
                   <ls_repo_item> LIKE LINE OF rt_repo_items.

    lo_repo_online ?= mo_repo.
    lt_status       = lo_repo_online->status( mo_log ).

    LOOP AT lt_status ASSIGNING <status>.
      AT NEW obj_name. "obj_type + obj_name
        APPEND INITIAL LINE TO rt_repo_items ASSIGNING <ls_repo_item>.
        <ls_repo_item>-obj_type = <status>-obj_type.
        <ls_repo_item>-obj_name = <status>-obj_name.
        <ls_repo_item>-sortkey  = c_sortkey-default. " Default sort key
        <ls_repo_item>-changes  = 0.
        <ls_repo_item>-path     = <status>-path.
      ENDAT.

      IF <status>-filename IS NOT INITIAL.
        ls_file-path        = <status>-path.
        ls_file-filename    = <status>-filename.
        ls_file-is_changed  = boolc( <status>-match = abap_false ). " TODO refactor
        ls_file-rstate      = <status>-rstate.
        ls_file-lstate      = <status>-lstate.
        APPEND ls_file TO <ls_repo_item>-files.

        IF ls_file-is_changed = abap_true.
          <ls_repo_item>-sortkey = c_sortkey-changed. " Changed files
          <ls_repo_item>-changes = <ls_repo_item>-changes + 1.
          _reduce_state <ls_repo_item>-lstate ls_file-lstate.
          _reduce_state <ls_repo_item>-rstate ls_file-rstate.
        ENDIF.
      ENDIF.

      AT END OF obj_name. "obj_type + obj_name
        IF <ls_repo_item>-obj_type IS INITIAL.
          <ls_repo_item>-sortkey = c_sortkey-orphan. "Virtual objects
        ENDIF.
      ENDAT.
    ENDLOOP.

  ENDMETHOD. "get_remote

ENDCLASS. "lcl_repo_content_browser

**********************************************************************
**********************************************************************

CLASS lcl_gui_view_repo_content DEFINITION FINAL INHERITING FROM lcl_gui_page_super.
  PUBLIC SECTION.

    CONSTANTS: BEGIN OF c_actions,
                 change_dir        TYPE string VALUE 'change_dir' ##NO_TEXT,
                 toggle_hide_files TYPE string VALUE 'toggle_hide_files' ##NO_TEXT,
                 toggle_folders    TYPE string VALUE 'toggle_folders' ##NO_TEXT,
                 toggle_changes    TYPE string VALUE 'toggle_changes' ##NO_TEXT,
               END OF c_actions.

    METHODS: lif_gui_page~render     REDEFINITION,
      lif_gui_page~on_event   REDEFINITION.

    METHODS constructor
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

  PRIVATE SECTION.

    DATA: mo_repo         TYPE REF TO lcl_repo,
          mv_cur_dir      TYPE string,
          mv_hide_files   TYPE abap_bool,
          mv_show_folders TYPE abap_bool,
          mv_changes_only TYPE abap_bool.

    METHODS:
      render_head_menu
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      render_grid_menu
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      render_item
        IMPORTING is_item        TYPE lcl_repo_content_browser=>ty_repo_item
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      render_item_files
        IMPORTING is_item        TYPE lcl_repo_content_browser=>ty_repo_item
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper,
      render_item_command
        IMPORTING is_item        TYPE lcl_repo_content_browser=>ty_repo_item
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper,
      get_item_class
        IMPORTING is_item        TYPE lcl_repo_content_browser=>ty_repo_item
        RETURNING VALUE(rv_html) TYPE string,
      get_item_icon
        IMPORTING is_item        TYPE lcl_repo_content_browser=>ty_repo_item
        RETURNING VALUE(rv_html) TYPE string,
      render_state
        IMPORTING iv1            TYPE char1
                  iv2            TYPE char1
        RETURNING VALUE(rv_html) TYPE string,
      render_empty_package
        RETURNING VALUE(rv_html) TYPE string,
      render_parent_dir
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception.

    METHODS:
      build_obj_jump_link
        IMPORTING is_item        TYPE lcl_repo_content_browser=>ty_repo_item
        RETURNING VALUE(rv_html) TYPE string,
      build_dir_jump_link
        IMPORTING iv_path        TYPE string
        RETURNING VALUE(rv_html) TYPE string.

ENDCLASS. "lcl_gui_view_repo_content

CLASS lcl_gui_view_repo_content IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    mo_repo         = lcl_app=>repo_srv( )->get( iv_key ).
    mv_cur_dir      = '/'. " Root
    mv_hide_files   = lcl_app=>user( )->get_hide_files( ).
    mv_changes_only = lcl_app=>user( )->get_changes_only( ).

  ENDMETHOD. "constructor

  METHOD lif_gui_page~on_event.

    DATA: lv_path TYPE string.

    CASE iv_action.
      WHEN c_actions-toggle_hide_files. " Toggle file diplay
        mv_hide_files   = lcl_app=>user( )->toggle_hide_files( ).
        ev_state        = gc_event_state-re_render.
      WHEN c_actions-change_dir.        " Change dir
        lv_path         = lcl_html_action_utils=>dir_decode( iv_getdata ).
        mv_cur_dir      = lcl_path=>change_dir( iv_cur_dir = mv_cur_dir iv_cd = lv_path ).
        ev_state        = gc_event_state-re_render.
      WHEN c_actions-toggle_folders.    " Toggle folder view
        mv_show_folders = boolc( mv_show_folders <> abap_true ).
        mv_cur_dir      = '/'. 	" Root
        ev_state        = gc_event_state-re_render.
      WHEN c_actions-toggle_changes.    " Toggle changes only view
        mv_changes_only = lcl_app=>user( )->toggle_changes_only( ).
        ev_state        = gc_event_state-re_render.
      WHEN 'update_checksums'.
        mo_repo->refresh_local_checksums( ).
        ev_state = gc_event_state-re_render.
    ENDCASE.

  ENDMETHOD. "lif_gui_page~on_event

  METHOD lif_gui_page~render.

    DATA: lt_repo_items TYPE lcl_repo_content_browser=>tt_repo_items,
          lo_browser    TYPE REF TO lcl_repo_content_browser,
          lx_error      TYPE REF TO lcx_exception,
          lo_log        TYPE REF TO lcl_log.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF lt_repo_items.

    " Reinit, for the case of type change
    mo_repo = lcl_app=>repo_srv( )->get( mo_repo->get_key( ) ).

    CREATE OBJECT ro_html.

    TRY.

        CREATE OBJECT lo_browser EXPORTING io_repo = mo_repo.
        lt_repo_items = lo_browser->list( iv_path         = mv_cur_dir
                                          iv_by_folders   = mv_show_folders
                                          iv_changes_only = mv_changes_only ).

        ro_html->add( render_head_menu( ) ).

        lo_log = lo_browser->get_log( ).
        IF mo_repo->is_offline( ) = abap_false AND lo_log->count( ) > 0.
          ro_html->add( '<div class="log attention">' ).
          ro_html->add( lo_log->to_html( ) ). " shows eg. list of unsupported objects
          ro_html->add( '</div>' ).
        ENDIF.

        ro_html->add( '<div class="repo_container">' ).
        ro_html->add( render_grid_menu( ) ).

        " Repo content table
        ro_html->add( '<table width="100%" class="repo_tab">' ).

        IF lcl_path=>is_root( mv_cur_dir ) = abap_false.
          ro_html->add( render_parent_dir( ) ).
        ENDIF.

        IF lines( lt_repo_items ) = 0.
          ro_html->add( render_empty_package( ) ).
        ELSE.
          LOOP AT lt_repo_items ASSIGNING <ls_item>.
            ro_html->add( render_item( <ls_item> ) ).
          ENDLOOP.
        ENDIF.

        ro_html->add( '</table>' ).
        ro_html->add( '</div>' ).

      CATCH lcx_exception INTO lx_error.
        ro_html->add( render_head_menu( ) ).
        ro_html->add( lcl_gui_page_super=>render_error( lx_error ) ).
    ENDTRY.

  ENDMETHOD.  "lif_gui_page~render

  METHOD render_grid_menu.

    DATA lo_tab_menu TYPE REF TO lcl_html_toolbar.

    CREATE OBJECT lo_tab_menu.

    IF mo_repo->is_offline( ) = abap_false.

      " Show/Hide files
      IF mv_hide_files = abap_true.
        lo_tab_menu->add( iv_txt = 'Show files' iv_act = c_actions-toggle_hide_files ).
      ELSE.
        lo_tab_menu->add( iv_txt = 'Hide files' iv_act = c_actions-toggle_hide_files ).
      ENDIF.

      " Show changes only
      IF mv_changes_only = abap_true.
        lo_tab_menu->add( iv_txt = 'All objects' iv_act = c_actions-toggle_changes ).
      ELSE.
        lo_tab_menu->add( iv_txt = 'Changed only' iv_act = c_actions-toggle_changes ).
      ENDIF.

    ENDIF.

    " Show/Hide folders
    IF mv_show_folders = abap_true.
      lo_tab_menu->add( iv_txt = 'Plain list' iv_act = c_actions-toggle_folders ).
    ELSE.
      lo_tab_menu->add( iv_txt = 'With folders' iv_act = c_actions-toggle_folders ).
    ENDIF.

    ro_html = lo_tab_menu->render( iv_as_angle = abap_true ).

  ENDMETHOD. "render_grid_menu

  METHOD render_head_menu.

    DATA: lo_toolbar     TYPE REF TO lcl_html_toolbar,
          lo_tb_advanced TYPE REF TO lcl_html_toolbar,
          lo_tb_branch   TYPE REF TO lcl_html_toolbar,
          lv_key         TYPE lcl_persistence_db=>ty_value,
          lv_wp_opt      LIKE gc_html_opt-crossout,
          lv_pull_opt    LIKE gc_html_opt-crossout,
          lo_repo_online TYPE REF TO lcl_repo_online.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.
    CREATE OBJECT lo_tb_branch.
    CREATE OBJECT lo_tb_advanced.

    lv_key = mo_repo->get_key( ).
    IF mo_repo->is_offline( ) = abap_false.
      lo_repo_online ?= mo_repo.
    ENDIF.

    IF mo_repo->is_write_protected( ) = abap_true.
      lv_wp_opt   = gc_html_opt-crossout.
      lv_pull_opt = gc_html_opt-crossout.
    ELSE.
      lv_pull_opt = gc_html_opt-emphas.
    ENDIF.

    " Build branch drop-down ========================
    IF mo_repo->is_offline( ) = abap_false. " Online ?
      lo_tb_branch->add( iv_txt = 'Overview'
                         iv_act = |{ gc_action-go_branch_overview }?{ lv_key }| ).
      lo_tb_branch->add( iv_txt = 'Switch'
                         iv_act = |{ gc_action-git_branch_switch }?{ lv_key }|
                         iv_opt = lv_wp_opt ).
      lo_tb_branch->add( iv_txt = 'Create'
                         iv_act = |{ gc_action-git_branch_create }?{ lv_key }| ).
      lo_tb_branch->add( iv_txt = 'Delete'
                         iv_act = |{ gc_action-git_branch_delete }?{ lv_key }| ).
    ENDIF.

    " Build advanced drop-down ========================
    IF mo_repo->is_offline( ) = abap_false. " Online ?
      lo_tb_advanced->add( iv_txt = 'Reset local'
                           iv_act = |{ gc_action-git_reset }?{ lv_key }|
                           iv_opt = lv_wp_opt ).
      lo_tb_advanced->add( iv_txt = 'Background mode'
                           iv_act = |{ gc_action-go_background }?{ lv_key }| ).
      lo_tb_advanced->add( iv_txt = 'Change remote'
                           iv_act = |{ gc_action-repo_remote_change }?{ lv_key }| ).
      lo_tb_advanced->add( iv_txt = 'Make off-line'
                           iv_act = |{ gc_action-repo_remote_detach }?{ lv_key }| ).
      lo_tb_advanced->add( iv_txt = 'Update local checksums'
                           iv_act = |update_checksums| ).
    ELSE.
      lo_tb_advanced->add( iv_txt = 'Make on-line'
                           iv_act = |{ gc_action-repo_remote_attach }?{ lv_key }| ).
    ENDIF.
    lo_tb_advanced->add( iv_txt = 'Remove'
                         iv_act = |{ gc_action-repo_remove }?{ lv_key }| ).
    lo_tb_advanced->add( iv_txt = 'Uninstall'
                         iv_act = |{ gc_action-repo_purge }?{ lv_key }|
                         iv_opt = lv_wp_opt ).

    " Build main toolbar ==============================
    IF mo_repo->is_offline( ) = abap_false. " Online ?
      TRY.
          IF lo_repo_online->get_sha1_remote( ) <> lo_repo_online->get_sha1_local( ).
            lo_toolbar->add( iv_txt = 'Pull'
                             iv_act = |{ gc_action-git_pull }?{ lv_key }|
                             iv_opt = lv_pull_opt ).
          ENDIF.
          IF lcl_stage_logic=>count( lo_repo_online ) > 0.
            lo_toolbar->add( iv_txt = 'Stage'
                             iv_act = |{ gc_action-go_stage }?{ lv_key }|
                             iv_opt = gc_html_opt-emphas ).
          ENDIF.
        CATCH lcx_exception ##NO_HANDLER.
          " authorization error or repository does not exist
          " ignore error
      ENDTRY.
      lo_toolbar->add( iv_txt = 'Branch'
                       io_sub = lo_tb_branch ) ##NO_TEXT.
    ELSE.
      lo_toolbar->add( iv_txt = 'Import ZIP'
                       iv_act = |{ gc_action-zip_import }?{ lv_key }|
                       iv_opt = gc_html_opt-emphas ).
      lo_toolbar->add( iv_txt = 'Export ZIP'
                       iv_act = |{ gc_action-zip_export }?{ lv_key }|
                       iv_opt = gc_html_opt-emphas ).
    ENDIF.

    lo_toolbar->add( iv_txt = 'Advanced'
                     io_sub = lo_tb_advanced ) ##NO_TEXT.
    lo_toolbar->add( iv_txt = 'Refresh'
                     iv_act = |{ gc_action-repo_refresh }?{ lv_key }| ).

    " Render ==========================================
    ro_html->add( '<div class="paddings">' ).
    ro_html->add( '<table width="100%"><tr>' ).

    IF mv_show_folders = abap_true.
      ro_html->add( |<td class="current_dir">{ mv_cur_dir }</td>| ).
    ENDIF.

    ro_html->add( '<td class="right">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</td>' ).
    ro_html->add( '<tr></table>' ).
    ro_html->add( '</div>' ).


  ENDMETHOD.  "render_head_menu

  METHOD get_item_class.

    DATA lt_class TYPE TABLE OF string.

    IF is_item-is_dir = abap_true.
      APPEND 'folder' TO lt_class.
    ELSEIF is_item-changes > 0.
      APPEND 'modified' TO lt_class.
    ELSEIF is_item-obj_name IS INITIAL.
      APPEND 'unsupported' TO lt_class.
    ENDIF.

    IF lines( lt_class ) > 0.
      rv_html = | class="{ concat_lines_of( table = lt_class sep = ` ` ) }"|.
    ENDIF.

  ENDMETHOD. "get_item_class

  METHOD get_item_icon.

    CASE is_item-obj_type.
      WHEN 'PROG' OR 'CLAS' OR 'FUGR'.
        rv_html = |<img src="img/code">|.
      WHEN 'W3MI' OR 'W3HT'.
        rv_html = |<img src="img/bin">|.
      WHEN ''.
        rv_html = space. " no icon
      WHEN OTHERS.
        rv_html = |<img src="img/obj">|.
    ENDCASE.

    IF is_item-is_dir = abap_true.
      rv_html = |<img src="img/dir">|.
    ENDIF.

  ENDMETHOD. "get_item_icon

  METHOD render_item.
    DATA: lv_link TYPE string,
          ls_file LIKE LINE OF is_item-files.

    CREATE OBJECT ro_html.


    ro_html->add( |<tr{ get_item_class( is_item ) }>| ).

    IF is_item-obj_name IS INITIAL AND is_item-is_dir = abap_false.
      ro_html->add( '<td colspan="2"></td>'
                 && '<td class="object">'
                 && '<i class="grey">non-code and meta files</i>'
                 && '</td>' ).
    ELSE.
      ro_html->add( |<td class="icon">{ get_item_icon( is_item ) }</td>| ).

      IF is_item-is_dir = abap_true. " Subdir
        lv_link = build_dir_jump_link( iv_path = is_item-path ).
        ro_html->add( |<td class="dir" colspan="2">{ lv_link }</td>| ).
      ELSE.
        lv_link = build_obj_jump_link( is_item = is_item ).
        ro_html->add( |<td class="type">{ is_item-obj_type }</td>| ).
        ro_html->add( |<td class="object">{ lv_link }</td>| ).
      ENDIF.
    ENDIF.

    IF mo_repo->is_offline( ) = abap_false.

      " Files
      ro_html->add( '<td class="files">' ).
      ro_html->add( render_item_files( is_item ) ).
      ro_html->add( '</td>' ).

      " Command
      ro_html->add( '<td class="cmd">' ).
      ro_html->add( render_item_command( is_item ) ).
      ro_html->add( '</td>' ).

    ENDIF.

    ro_html->add( '</tr>' ).

  ENDMETHOD.  "render_item

  METHOD render_item_files.

    DATA: ls_file     LIKE LINE OF is_item-files.

    CREATE OBJECT ro_html.

    IF mv_hide_files = abap_true AND is_item-obj_type IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT is_item-files INTO ls_file.
      ro_html->add( |<div>{ ls_file-path && ls_file-filename }</div>| ).
    ENDLOOP.

  ENDMETHOD.  "render_item_files

  METHOD render_item_command.

    DATA: lv_difflink TYPE string,
          lv_text     TYPE string,
          ls_file     LIKE LINE OF is_item-files.

    CREATE OBJECT ro_html.

    IF is_item-is_dir = abap_true. " Directory

      ro_html->add( '<div>' ).
      ro_html->add( |<span class="grey">{ is_item-changes } changes</span>| ).
      ro_html->add( render_state( iv1 = is_item-lstate iv2 = is_item-rstate ) ).
      ro_html->add( '</div>' ).

    ELSEIF is_item-changes > 0.

      IF mv_hide_files = abap_true AND is_item-obj_name IS NOT INITIAL.

        lv_difflink = lcl_html_action_utils=>obj_encode(
          iv_key    = mo_repo->get_key( )
          ig_object = is_item ).

        ro_html->add( '<div>' ).
        ro_html->add_anchor( iv_txt = |view diff ({ is_item-changes })|
                             iv_act = |{ gc_action-go_diff }?{ lv_difflink }| ).
        ro_html->add( render_state( iv1 = is_item-lstate iv2 = is_item-rstate ) ).
        ro_html->add( '</div>' ).

      ELSE.
        LOOP AT is_item-files INTO ls_file.

          ro_html->add( '<div>' ).
          IF ls_file-is_changed = abap_true.
            lv_difflink = lcl_html_action_utils=>file_encode(
              iv_key  = mo_repo->get_key( )
              ig_file = ls_file ).
            ro_html->add_anchor(
              iv_txt = 'view diff'
              iv_act = |{ gc_action-go_diff }?{ lv_difflink }| ).
            ro_html->add( render_state( iv1 = ls_file-lstate iv2 = ls_file-rstate ) ).
          ELSE.
            ro_html->add( '&nbsp;' ).
          ENDIF.
          ro_html->add( '</div>' ).

        ENDLOOP.
      ENDIF.

    ENDIF.

  ENDMETHOD.  "render_item_command

  METHOD render_state.

    FIELD-SYMBOLS <state> TYPE char1.

    rv_html = '<span class="state-block">'.

    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          ASSIGN iv1 TO <state>.
        WHEN 2.
          ASSIGN iv2 TO <state>.
      ENDCASE.

      CASE <state>.
        WHEN gc_state-unchanged.  "None or unchanged
          IF iv1 = gc_state-added OR iv2 = gc_state-added.
            rv_html = rv_html && |<span class="none" title="Not exists">&#x00d7;</span>|.
          ELSE.
            rv_html = rv_html && |<span class="none" title="No changes">&nbsp;</span>|.
          ENDIF.
        WHEN gc_state-modified.   "Changed
          rv_html = rv_html && '<span class="changed" title="Modified">M</span>'.
        WHEN gc_state-added.      "Added new
          rv_html = rv_html && '<span class="added" title="Added new">A</span>'.
        WHEN gc_state-mixed.      "Added and changed (multifile)
          rv_html = rv_html && '<span class="mixed" title="Added and modified">~</span>'.
      ENDCASE.
    ENDDO.

    rv_html = rv_html && '</span>'.

  ENDMETHOD. "render_state

  METHOD render_empty_package.

    rv_html = '<tr class="unsupported"><td class="paddings">'
           && '  <center>Empty package</center>'
           && '</td></tr>' ##NO_TEXT.

  ENDMETHOD. "render_empty_package

  METHOD render_parent_dir.

    CREATE OBJECT ro_html.

    ro_html->add( '<tr class="folder">' ).
    ro_html->add( |<td class="icon"><img src="img/dir"></td>| ).
    ro_html->add( |<td class="object" colspan="2">{ build_dir_jump_link( '..' ) }</td>| ).
    IF mo_repo->is_offline( ) = abap_false.
      ro_html->add( |<td colspan="2"></td>| ). " Dummy for online
    ENDIF.
    ro_html->add( '</tr>' ).

  ENDMETHOD. "render_parent_dir

  METHOD build_dir_jump_link.

    DATA: lv_path   TYPE string,
          lv_encode TYPE string,
          lo_html   TYPE REF TO lcl_html_helper.

    lv_path = iv_path.
    REPLACE FIRST OCCURRENCE OF mv_cur_dir IN lv_path WITH ''.
    lv_encode = lcl_html_action_utils=>dir_encode( lv_path ).

    CREATE OBJECT lo_html.
    lo_html->add_anchor( iv_txt = lv_path iv_act = |{ c_actions-change_dir }?{ lv_encode }| ).
    rv_html = lo_html->mv_html.

  ENDMETHOD.  "build_dir_jump_link

  METHOD build_obj_jump_link.

    DATA: lv_encode TYPE string,
          lo_html   TYPE REF TO lcl_html_helper.

    lv_encode = lcl_html_action_utils=>jump_encode( iv_obj_type = is_item-obj_type
                                                    iv_obj_name = is_item-obj_name ).

    CREATE OBJECT lo_html.
    lo_html->add_anchor( iv_txt = |{ is_item-obj_name }|
                         iv_act = |{ gc_action-jump }?{ lv_encode }| ).
    rv_html = lo_html->mv_html.

  ENDMETHOD.  "build_obj_jump_link

ENDCLASS. "lcl_gui_view_repo_content