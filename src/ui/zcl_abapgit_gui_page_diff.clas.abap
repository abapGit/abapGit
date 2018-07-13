CLASS zcl_abapgit_gui_page_diff DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_fstate,
        local  TYPE char1 VALUE 'L',
        remote TYPE char1 VALUE 'R',
        both   TYPE char1 VALUE 'B',
      END OF c_fstate.

    TYPES: BEGIN OF ty_file_diff,
             path       TYPE string,
             filename   TYPE string,
             lstate     TYPE char1,
             rstate     TYPE char1,
             fstate     TYPE char1, " FILE state - Abstraction for shorter ifs
             o_diff     TYPE REF TO zcl_abapgit_diff,
             changed_by TYPE xubname,
             type       TYPE string,
           END OF ty_file_diff,
           tt_file_diff TYPE STANDARD TABLE OF ty_file_diff.

    METHODS:
      constructor
        IMPORTING iv_key           TYPE zif_abapgit_persistence=>ty_repo-key
                  is_file          TYPE zif_abapgit_definitions=>ty_file OPTIONAL
                  is_object        TYPE zif_abapgit_definitions=>ty_item OPTIONAL
                  iv_supress_stage TYPE abap_bool DEFAULT abap_false
        RAISING   zcx_abapgit_exception,
      zif_abapgit_gui_page~on_event REDEFINITION.
  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION,
      scripts REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_actions,
                 toggle_unified TYPE string VALUE 'toggle_unified',
               END OF c_actions.

    DATA: mt_diff_files    TYPE tt_file_diff,
          mt_delayed_lines TYPE zif_abapgit_definitions=>ty_diffs_tt,
          mv_unified       TYPE abap_bool VALUE abap_true,
          mv_repo_key      TYPE zif_abapgit_persistence=>ty_repo-key,
          mv_seed          TYPE string. " Unique page id to bind JS sessionStorage

    METHODS render_diff
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_diff_head
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_table_head
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_lines
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_beacon
      IMPORTING is_diff_line   TYPE zif_abapgit_definitions=>ty_diff
                is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_line_split
      IMPORTING is_diff_line   TYPE zif_abapgit_definitions=>ty_diff
                iv_fstate      TYPE char1
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS render_line_unified
      IMPORTING is_diff_line   TYPE zif_abapgit_definitions=>ty_diff OPTIONAL
      RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    METHODS append_diff
      IMPORTING it_remote TYPE zif_abapgit_definitions=>ty_files_tt
                it_local  TYPE zif_abapgit_definitions=>ty_files_item_tt
                is_status TYPE zif_abapgit_definitions=>ty_result
      RAISING   zcx_abapgit_exception.
    METHODS build_menu
      IMPORTING iv_supress_stage TYPE abap_bool
      RETURNING VALUE(ro_menu)   TYPE REF TO zcl_abapgit_html_toolbar.
    METHODS is_binary
      IMPORTING iv_d1         TYPE xstring
                iv_d2         TYPE xstring
      RETURNING VALUE(rv_yes) TYPE abap_bool.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DIFF IMPLEMENTATION.


  METHOD append_diff.

    DATA:
      lv_offs    TYPE i,
      ls_r_dummy LIKE LINE OF it_remote ##NEEDED,
      ls_l_dummy LIKE LINE OF it_local  ##NEEDED.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF it_remote,
                   <ls_local>  LIKE LINE OF it_local,
                   <ls_diff>   LIKE LINE OF mt_diff_files.


    READ TABLE it_remote ASSIGNING <ls_remote>
      WITH KEY filename = is_status-filename
               path     = is_status-path.
    IF sy-subrc <> 0.
      ASSIGN ls_r_dummy TO <ls_remote>.
    ENDIF.

    READ TABLE it_local ASSIGNING <ls_local>
      WITH KEY file-filename = is_status-filename
               file-path     = is_status-path.
    IF sy-subrc <> 0.
      ASSIGN ls_l_dummy TO <ls_local>.
    ENDIF.

    IF <ls_local> IS INITIAL AND <ls_remote> IS INITIAL.
      zcx_abapgit_exception=>raise( |DIFF: file not found { is_status-filename }| ).
    ENDIF.

    APPEND INITIAL LINE TO mt_diff_files ASSIGNING <ls_diff>.
    <ls_diff>-path     = is_status-path.
    <ls_diff>-filename = is_status-filename.
    <ls_diff>-lstate   = is_status-lstate.
    <ls_diff>-rstate   = is_status-rstate.

    IF <ls_diff>-lstate IS NOT INITIAL AND <ls_diff>-rstate IS NOT INITIAL.
      <ls_diff>-fstate = c_fstate-both.
    ELSEIF <ls_diff>-lstate IS NOT INITIAL.
      <ls_diff>-fstate = c_fstate-local.
    ELSE. "rstate IS NOT INITIAL, lstate = empty.
      <ls_diff>-fstate = c_fstate-remote.
    ENDIF.

    " Changed by
    IF <ls_local>-item-obj_type IS NOT INITIAL.
      <ls_diff>-changed_by = to_lower( zcl_abapgit_objects=>changed_by( <ls_local>-item ) ).
    ENDIF.

    " Extension
    IF <ls_local>-file-filename IS NOT INITIAL.
      <ls_diff>-type = reverse( <ls_local>-file-filename ).
    ELSE.
      <ls_diff>-type = reverse( <ls_remote>-filename ).
    ENDIF.

    FIND FIRST OCCURRENCE OF '.' IN <ls_diff>-type MATCH OFFSET lv_offs.
    <ls_diff>-type = reverse( substring( val = <ls_diff>-type len = lv_offs ) ).
    IF <ls_diff>-type <> 'xml' AND <ls_diff>-type <> 'abap'.
      <ls_diff>-type = 'other'.
    ENDIF.

    IF <ls_diff>-type = 'other'
       AND is_binary( iv_d1 = <ls_remote>-data iv_d2 = <ls_local>-file-data ) = abap_true.
      <ls_diff>-type = 'binary'.
    ENDIF.

    " Diff data
    IF <ls_diff>-type <> 'binary'.
      IF <ls_diff>-fstate = c_fstate-remote. " Remote file leading changes
        CREATE OBJECT <ls_diff>-o_diff
          EXPORTING
            iv_new = <ls_remote>-data
            iv_old = <ls_local>-file-data.
      ELSE.             " Local leading changes or both were modified
        CREATE OBJECT <ls_diff>-o_diff
          EXPORTING
            iv_new = <ls_local>-file-data
            iv_old = <ls_remote>-data.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD build_menu.

    DATA: lo_sub   TYPE REF TO zcl_abapgit_html_toolbar,
          lt_types TYPE string_table,
          lt_users TYPE string_table.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff_files,
                   <lv_i>    TYPE string.

    " Get unique
    LOOP AT mt_diff_files ASSIGNING <ls_diff>.
      APPEND <ls_diff>-type TO lt_types.
      APPEND <ls_diff>-changed_by TO lt_users.
    ENDLOOP.

    SORT: lt_types, lt_users.
    DELETE ADJACENT DUPLICATES FROM: lt_types, lt_users.

    CREATE OBJECT ro_menu.

    IF iv_supress_stage = abap_false.
      ro_menu->add( iv_txt = 'Stage'
                    iv_act = |{ zif_abapgit_definitions=>gc_action-go_stage }?{ mv_repo_key }|
                    iv_id  = 'stage-button'
                    iv_opt = zif_abapgit_definitions=>gc_html_opt-strong ).
    ENDIF.

    IF lines( lt_types ) > 1 OR lines( lt_users ) > 1.
      CREATE OBJECT lo_sub EXPORTING iv_id = 'diff-filter'.

      " File types
      IF lines( lt_types ) > 1.
        lo_sub->add( iv_txt = 'TYPE' iv_typ = zif_abapgit_definitions=>gc_action_type-separator ).
        LOOP AT lt_types ASSIGNING <lv_i>.
          lo_sub->add( iv_txt = <lv_i>
                       iv_typ = zif_abapgit_definitions=>gc_action_type-onclick
                       iv_aux = 'type'
                       iv_chk = abap_true ).
        ENDLOOP.
      ENDIF.

      " Changed by
      IF lines( lt_users ) > 1.
        lo_sub->add( iv_txt = 'CHANGED BY' iv_typ = zif_abapgit_definitions=>gc_action_type-separator ).
        LOOP AT lt_users ASSIGNING <lv_i>.
          lo_sub->add( iv_txt = <lv_i>
                       iv_typ = zif_abapgit_definitions=>gc_action_type-onclick
                       iv_aux = 'changed-by'
                       iv_chk = abap_true ).
        ENDLOOP.
      ENDIF.

      ro_menu->add( iv_txt = 'Filter'
                    io_sub = lo_sub ) ##NO_TEXT.
    ENDIF.

    ro_menu->add( iv_txt = 'Split/Unified view'
                  iv_act = c_actions-toggle_unified ) ##NO_TEXT.

  ENDMETHOD.


  METHOD constructor.

    DATA: lt_remote TYPE zif_abapgit_definitions=>ty_files_tt,
          lt_local  TYPE zif_abapgit_definitions=>ty_files_item_tt,
          lt_status TYPE zif_abapgit_definitions=>ty_results_tt,
          lo_repo   TYPE REF TO zcl_abapgit_repo_online,
          lv_ts     TYPE timestamp.

    FIELD-SYMBOLS: <ls_status> LIKE LINE OF lt_status.

    super->constructor( ).
    ms_control-page_title = 'DIFF'.
    mv_unified            = zcl_abapgit_persistence_user=>get_instance( )->get_diff_unified( ).
    mv_repo_key           = iv_key.

    GET TIME STAMP FIELD lv_ts.
    mv_seed = |diff{ lv_ts }|. " Generate based on time

    ASSERT is_file IS INITIAL OR is_object IS INITIAL. " just one passed

    lo_repo  ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lt_remote = lo_repo->get_files_remote( ).
    lt_local  = lo_repo->get_files_local( ).
    lt_status = lo_repo->status( ).

    IF is_file IS NOT INITIAL.        " Diff for one file

      READ TABLE lt_status ASSIGNING <ls_status>
        WITH KEY path = is_file-path filename = is_file-filename.

      append_diff( it_remote = lt_remote
                   it_local  = lt_local
                   is_status = <ls_status> ).

    ELSEIF is_object IS NOT INITIAL.  " Diff for whole object

      LOOP AT lt_status ASSIGNING <ls_status>
          WHERE obj_type = is_object-obj_type
          AND   obj_name = is_object-obj_name
          AND   match IS INITIAL.
        append_diff( it_remote = lt_remote
                     it_local  = lt_local
                     is_status = <ls_status> ).
      ENDLOOP.

    ELSE.                             " Diff for the whole repo

      LOOP AT lt_status ASSIGNING <ls_status> WHERE match IS INITIAL.
        append_diff( it_remote = lt_remote
                     it_local  = lt_local
                     is_status = <ls_status> ).
      ENDLOOP.

    ENDIF.

    IF lines( mt_diff_files ) = 0.
      zcx_abapgit_exception=>raise( 'PAGE_DIFF ERROR: No diff files found' ).
    ENDIF.

    ms_control-page_menu  = build_menu( iv_supress_stage ).

  ENDMETHOD.


  METHOD is_binary.

    DATA: lv_len TYPE i,
          lv_idx TYPE i,
          lv_x   TYPE x.

    FIELD-SYMBOLS <lv_data> LIKE iv_d1.


    IF iv_d1 IS NOT INITIAL. " One of them might be new and so empty
      ASSIGN iv_d1 TO <lv_data>.
    ELSE.
      ASSIGN iv_d2 TO <lv_data>.
    ENDIF.

    lv_len = xstrlen( <lv_data> ).
    IF lv_len = 0.
      RETURN.
    ENDIF.

    IF lv_len > 100.
      lv_len = 100.
    ENDIF.

    " Simple char range test
    " stackoverflow.com/questions/277521/how-to-identify-the-file-content-as-ascii-or-binary
    DO lv_len TIMES. " I'm sure there is more efficient way ...
      lv_idx = sy-index - 1.
      lv_x = <lv_data>+lv_idx(1).

      IF NOT ( lv_x BETWEEN 9 AND 13 OR lv_x BETWEEN 32 AND 126 ).
        rv_yes = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD render_beacon.

    DATA: lv_beacon  TYPE string.

    CREATE OBJECT ro_html.

    IF is_diff_line-beacon > 0.
      READ TABLE is_diff-o_diff->mt_beacons INTO lv_beacon INDEX is_diff_line-beacon.
    ELSE.
      lv_beacon = '---'.
    ENDIF.


    ro_html->add( '<thead class="nav_line">' ).
    ro_html->add( '<tr>' ).

    ro_html->add( '<th class="num"></th>' ).
    IF mv_unified = abap_true.
      ro_html->add( '<th class="num"></th>' ).
      ro_html->add( |<th>@@ { is_diff_line-new_num } @@ { lv_beacon }</th>| ).
    ELSE.
      ro_html->add( |<th colspan="3">@@ { is_diff_line-new_num } @@ { lv_beacon }</th>| ).
    ENDIF.

    ro_html->add( '</tr>' ).
    ro_html->add( '</thead>' ).

  ENDMETHOD.


  METHOD render_content.

    DATA: ls_diff_file LIKE LINE OF mt_diff_files,
          lo_progress  TYPE REF TO zcl_abapgit_progress.


    CREATE OBJECT ro_html.

    CREATE OBJECT lo_progress
      EXPORTING
        iv_total = lines( mt_diff_files ).

    ro_html->add( |<div id="diff-list" data-repo-key="{ mv_repo_key }">| ).
    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_js_error_banner( ) ).
    LOOP AT mt_diff_files INTO ls_diff_file.
      lo_progress->show(
        iv_current = sy-tabix
        iv_text    = |Render Diff - { ls_diff_file-filename }| ).

      ro_html->add( render_diff( ls_diff_file ) ).
    ENDLOOP.
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_diff.

    CREATE OBJECT ro_html.

    ro_html->add( |<div class="diff" data-type="{ is_diff-type
      }" data-changed-by="{ is_diff-changed_by
      }" data-file="{ is_diff-path && is_diff-filename }">| ). "#EC NOTEXT
    ro_html->add( render_diff_head( is_diff ) ).

    " Content
    IF is_diff-type <> 'binary'.
      ro_html->add( '<div class="diff_content">' ).         "#EC NOTEXT
      ro_html->add( '<table class="diff_tab syntax-hl">' ). "#EC NOTEXT
      ro_html->add( render_table_head( ) ).
      ro_html->add( render_lines( is_diff ) ).
      ro_html->add( '</table>' ).                           "#EC NOTEXT
    ELSE.
      ro_html->add( '<div class="diff_content paddings center grey">' ). "#EC NOTEXT
      ro_html->add( 'The content seems to be binary.' ).    "#EC NOTEXT
      ro_html->add( 'Cannot display as diff.' ).            "#EC NOTEXT
    ENDIF.
    ro_html->add( '</div>' ).                               "#EC NOTEXT

    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.


  METHOD render_diff_head.

    DATA: ls_stats TYPE zif_abapgit_definitions=>ty_count.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="diff_head">' ).              "#EC NOTEXT

    IF is_diff-type <> 'binary'.
      ls_stats = is_diff-o_diff->stats( ).
      IF is_diff-fstate = c_fstate-both. " Merge stats into 'update' if both were changed
        ls_stats-update = ls_stats-update + ls_stats-insert + ls_stats-delete.
        CLEAR: ls_stats-insert, ls_stats-delete.
      ENDIF.

      ro_html->add( |<span class="diff_banner diff_ins">+ { ls_stats-insert }</span>| ).
      ro_html->add( |<span class="diff_banner diff_del">- { ls_stats-delete }</span>| ).
      ro_html->add( |<span class="diff_banner diff_upd">~ { ls_stats-update }</span>| ).
    ENDIF.

    ro_html->add( |<span class="diff_name">{ is_diff-path }{ is_diff-filename }</span>| ). "#EC NOTEXT
    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_item_state(
      iv1 = is_diff-lstate
      iv2 = is_diff-rstate ) ).

    IF is_diff-fstate = c_fstate-both AND mv_unified = abap_true.
      ro_html->add( '<span class="attention pad-sides">Attention: Unified mode'
                 && ' highlighting for MM assumes local file is newer ! </span>' ). "#EC NOTEXT
    ENDIF.

    ro_html->add( |<span class="diff_changed_by">last change by: <span class="user">{
      is_diff-changed_by }</span></span>| ).

    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.


  METHOD render_lines.

    DATA: lo_highlighter TYPE REF TO zcl_abapgit_syntax_highlighter,
          lt_diffs       TYPE zif_abapgit_definitions=>ty_diffs_tt,
          lv_insert_nav  TYPE abap_bool.

    FIELD-SYMBOLS <ls_diff>  LIKE LINE OF lt_diffs.

    lo_highlighter = zcl_abapgit_syntax_highlighter=>create( is_diff-filename ).
    CREATE OBJECT ro_html.

    lt_diffs = is_diff-o_diff->get( ).

    LOOP AT lt_diffs ASSIGNING <ls_diff>.
      IF <ls_diff>-short = abap_false.
        lv_insert_nav = abap_true.
        CONTINUE.
      ENDIF.

      IF lv_insert_nav = abap_true. " Insert separator line with navigation
        ro_html->add( render_beacon( is_diff_line = <ls_diff> is_diff = is_diff ) ).
        lv_insert_nav = abap_false.
      ENDIF.

      IF lo_highlighter IS BOUND.
        <ls_diff>-new = lo_highlighter->process_line( <ls_diff>-new ).
        <ls_diff>-old = lo_highlighter->process_line( <ls_diff>-old ).
      ELSE.
        <ls_diff>-new = escape( val = <ls_diff>-new format = cl_abap_format=>e_html_attr ).
        <ls_diff>-old = escape( val = <ls_diff>-old format = cl_abap_format=>e_html_attr ).
      ENDIF.

      CONDENSE <ls_diff>-new_num. "get rid of leading spaces
      CONDENSE <ls_diff>-old_num.

      IF mv_unified = abap_true.
        ro_html->add( render_line_unified( is_diff_line = <ls_diff> ) ).
      ELSE.
        ro_html->add( render_line_split( is_diff_line = <ls_diff>
                                         iv_fstate    = is_diff-fstate ) ).
      ENDIF.

    ENDLOOP.

    IF mv_unified = abap_true.
      ro_html->add( render_line_unified( ) ). " Release delayed lines
    ENDIF.

  ENDMETHOD.


  METHOD render_line_split.

    DATA: lv_new  TYPE string,
          lv_old  TYPE string,
          lv_mark TYPE string,
          lv_bg   TYPE string.

    CREATE OBJECT ro_html.

    " New line
    lv_mark = ` `.
    IF iv_fstate = c_fstate-both OR is_diff_line-result = zif_abapgit_definitions=>c_diff-update.
      lv_bg = ' diff_upd'.
      lv_mark = `~`.
    ELSEIF is_diff_line-result = zif_abapgit_definitions=>c_diff-insert.
      lv_bg = ' diff_ins'.
      lv_mark = `+`.
    ENDIF.
    lv_new = |<td class="num" line-num="{ is_diff_line-new_num }"></td>|
          && |<td class="code{ lv_bg }">{ lv_mark }{ is_diff_line-new }</td>|.

    " Old line
    CLEAR lv_bg.
    lv_mark = ` `.
    IF iv_fstate = c_fstate-both OR is_diff_line-result = zif_abapgit_definitions=>c_diff-update.
      lv_bg = ' diff_upd'.
      lv_mark = `~`.
    ELSEIF is_diff_line-result = zif_abapgit_definitions=>c_diff-delete.
      lv_bg = ' diff_del'.
      lv_mark = `-`.
    ENDIF.
    lv_old = |<td class="num" line-num="{ is_diff_line-old_num }"></td>|
          && |<td class="code{ lv_bg }">{ lv_mark }{ is_diff_line-old }</td>|.

    " render line, inverse sides if remote is newer
    ro_html->add( '<tr>' ).                                 "#EC NOTEXT
    IF iv_fstate = c_fstate-remote. " Remote file leading changes
      ro_html->add( lv_old ). " local
      ro_html->add( lv_new ). " remote
    ELSE.             " Local leading changes or both were modified
      ro_html->add( lv_new ). " local
      ro_html->add( lv_old ). " remote
    ENDIF.
    ro_html->add( '</tr>' ).                                "#EC NOTEXT

  ENDMETHOD.


  METHOD render_line_unified.

    FIELD-SYMBOLS <ls_diff_line> LIKE LINE OF mt_delayed_lines.

    CREATE OBJECT ro_html.

    " Release delayed subsequent update lines
    IF is_diff_line-result <> zif_abapgit_definitions=>c_diff-update.
      LOOP AT mt_delayed_lines ASSIGNING <ls_diff_line>.
        ro_html->add( '<tr>' ).                             "#EC NOTEXT
        ro_html->add( |<td class="num" line-num="{ <ls_diff_line>-old_num }"></td>|
                   && |<td class="num" line-num=""></td>|
                   && |<td class="code diff_del">-{ <ls_diff_line>-old }</td>| ).
        ro_html->add( '</tr>' ).                            "#EC NOTEXT
      ENDLOOP.
      LOOP AT mt_delayed_lines ASSIGNING <ls_diff_line>.
        ro_html->add( '<tr>' ).                             "#EC NOTEXT
        ro_html->add( |<td class="num" line-num=""></td>|
                   && |<td class="num" line-num="{ <ls_diff_line>-new_num }"></td>|
                   && |<td class="code diff_ins">+{ <ls_diff_line>-new }</td>| ).
        ro_html->add( '</tr>' ).                            "#EC NOTEXT
      ENDLOOP.
      CLEAR mt_delayed_lines.
    ENDIF.

    ro_html->add( '<tr>' ).                                 "#EC NOTEXT
    CASE is_diff_line-result.
      WHEN zif_abapgit_definitions=>c_diff-update.
        APPEND is_diff_line TO mt_delayed_lines. " Delay output of subsequent updates
      WHEN zif_abapgit_definitions=>c_diff-insert.
        ro_html->add( |<td class="num" line-num=""></td>|
                   && |<td class="num" line-num="{ is_diff_line-new_num }"></td>|
                   && |<td class="code diff_ins">+{ is_diff_line-new }</td>| ).
      WHEN zif_abapgit_definitions=>c_diff-delete.
        ro_html->add( |<td class="num" line-num="{ is_diff_line-old_num }"></td>|
                   && |<td class="num" line-num=""></td>|
                   && |<td class="code diff_del">-{ is_diff_line-old }</td>| ).
      WHEN OTHERS. "none
        ro_html->add( |<td class="num" line-num="{ is_diff_line-old_num }"></td>|
                   && |<td class="num" line-num="{ is_diff_line-new_num }"></td>|
                   && |<td class="code"> { is_diff_line-old }</td>| ).
    ENDCASE.
    ro_html->add( '</tr>' ).                                "#EC NOTEXT

  ENDMETHOD.


  METHOD render_table_head.

    CREATE OBJECT ro_html.

    ro_html->add( '<thead class="header">' ).               "#EC NOTEXT
    ro_html->add( '<tr>' ).                                 "#EC NOTEXT

    IF mv_unified = abap_true.
      ro_html->add( '<th class="num">old</th>' ).           "#EC NOTEXT
      ro_html->add( '<th class="num">new</th>' ).           "#EC NOTEXT
      ro_html->add( '<th>code</th>' ).                      "#EC NOTEXT
    ELSE.
      ro_html->add( '<th class="num"></th>' ).              "#EC NOTEXT
      ro_html->add( '<th>LOCAL</th>' ).                     "#EC NOTEXT
      ro_html->add( '<th class="num"></th>' ).              "#EC NOTEXT
      ro_html->add( '<th>REMOTE</th>' ).                    "#EC NOTEXT
    ENDIF.

    ro_html->add( '</tr>' ).                                "#EC NOTEXT
    ro_html->add( '</thead>' ).                             "#EC NOTEXT

  ENDMETHOD.


  METHOD scripts.

    CREATE OBJECT ro_html.

    ro_html->add( 'var gHelper = new DiffHelper({' ).
    ro_html->add( |  seed:        "{ mv_seed }",| ).
    ro_html->add( |  stageAction: "{ zif_abapgit_definitions=>gc_action-go_stage }",| ).
    ro_html->add( '  ids: {' ).
    ro_html->add( '    diffList:    "diff-list",' ).
    ro_html->add( '    filterMenu:  "diff-filter",' ).
    ro_html->add( '    stageButton: "stage-button"' ).
    ro_html->add( '  }' ).
    ro_html->add( '});' ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_page~on_event.

    CASE iv_action.
      WHEN c_actions-toggle_unified. " Toggle file diplay
        mv_unified = zcl_abapgit_persistence_user=>get_instance( )->toggle_diff_unified( ).
        ev_state   = zif_abapgit_definitions=>gc_event_state-re_render.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
