*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_DIFF
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_diff DEFINITION FINAL INHERITING FROM lcl_gui_page.

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_fstate,
        local  TYPE char1 VALUE 'L',
        remote TYPE char1 VALUE 'R',
        both   TYPE char1 VALUE 'B',
      END OF c_fstate.

    TYPES: BEGIN OF ty_file_diff,
             filename TYPE string,
             lstate   TYPE char1,
             rstate   TYPE char1,
             fstate   TYPE char1, " FILE state - Abstraction for shorter ifs
             o_diff   TYPE REF TO lcl_diff,
           END OF ty_file_diff,
           tt_file_diff TYPE STANDARD TABLE OF ty_file_diff.

    METHODS:
      constructor
        IMPORTING iv_key    TYPE lcl_persistence_repo=>ty_repo-key
                  is_file   TYPE ty_file OPTIONAL
                  is_object TYPE ty_item OPTIONAL
        RAISING   lcx_exception,
      lif_gui_page~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_actions,
                 toggle_unified TYPE string VALUE 'toggle_unified',
               END OF c_actions.

    DATA: mt_diff_files    TYPE tt_file_diff,
          mt_delayed_lines TYPE lcl_diff=>ty_diffs_tt,
          mv_unified       TYPE abap_bool VALUE abap_true.

    METHODS render_diff
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html.
    METHODS render_diff_head
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html.
    METHODS render_table_head
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html.
    METHODS render_lines
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html.
    METHODS render_beacon
      IMPORTING is_diff_line   TYPE lcl_diff=>ty_diff
                is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html.
    METHODS render_line_split
      IMPORTING is_diff_line   TYPE lcl_diff=>ty_diff
                iv_fstate      TYPE char1
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html.
    METHODS render_line_unified
      IMPORTING is_diff_line   TYPE lcl_diff=>ty_diff OPTIONAL
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html.
    METHODS append_diff
      IMPORTING it_remote TYPE ty_files_tt
                it_local  TYPE ty_files_item_tt
                is_status TYPE ty_result
      RAISING   lcx_exception.
    METHODS build_menu
      RETURNING VALUE(ro_menu) TYPE REF TO lcl_html_toolbar.

ENDCLASS. "lcl_gui_page_diff

CLASS lcl_gui_page_diff IMPLEMENTATION.

  METHOD constructor.

    DATA: lt_remote TYPE ty_files_tt,
          lt_local  TYPE ty_files_item_tt,
          lt_status TYPE ty_results_tt,
          lo_repo   TYPE REF TO lcl_repo_online.

    FIELD-SYMBOLS: <ls_status> LIKE LINE OF lt_status.

    super->constructor( ).
    ms_control-page_title = 'DIFF'.
    ms_control-page_menu  = build_menu( ).
    mv_unified            = lcl_app=>user( )->get_diff_unified( ).

    ASSERT is_file IS INITIAL OR is_object IS INITIAL. " just one passed

    lo_repo  ?= lcl_app=>repo_srv( )->get( iv_key ).
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
      lcx_exception=>raise( 'PAGE_DIFF ERROR: No diff files found' ).
    ENDIF.

  ENDMETHOD.

  METHOD append_diff.

    DATA:
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
      lcx_exception=>raise( |DIFF: file not found { is_status-filename }| ).
    ENDIF.

    APPEND INITIAL LINE TO mt_diff_files ASSIGNING <ls_diff>.
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

  ENDMETHOD.  "append_diff

  METHOD build_menu.
    CREATE OBJECT ro_menu.
    ro_menu->add( iv_txt = 'Split/Unified view'
                  iv_act = c_actions-toggle_unified ) ##NO_TEXT.
  ENDMETHOD.  " build_menu.

**********************************************************************
* EVENT HANDLING
**********************************************************************

  METHOD lif_gui_page~on_event.

    CASE iv_action.
      WHEN c_actions-toggle_unified. " Toggle file diplay
        mv_unified = lcl_app=>user( )->toggle_diff_unified( ).
        ev_state   = gc_event_state-re_render.
    ENDCASE.

  ENDMETHOD. "lif_gui_page~on_event

**********************************************************************
* RENDER LOGIC
**********************************************************************

  METHOD render_content.

    DATA ls_diff_file LIKE LINE OF mt_diff_files.

    CREATE OBJECT ro_html.

    LOOP AT mt_diff_files INTO ls_diff_file.
      lcl_progress=>show( iv_key     = 'Diff'
                          iv_current = sy-tabix
                          iv_total   = lines( mt_diff_files )
                          iv_text    = |Render Diff - { ls_diff_file-filename }| ).

      ro_html->add( render_diff( ls_diff_file ) ).
    ENDLOOP.

  ENDMETHOD.  "render_content

  METHOD render_diff.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="diff">' ).                   "#EC NOTEXT
    ro_html->add( render_diff_head( is_diff ) ).

    " Content
    ro_html->add( '<div class="diff_content">' ).           "#EC NOTEXT
    ro_html->add( '<table class="diff_tab syntax-hl">' ).   "#EC NOTEXT
    ro_html->add( render_table_head( ) ).
    ro_html->add( render_lines( is_diff ) ).
    ro_html->add( '</table>' ).                             "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT

    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.  " render_diff

**********************************************************************
* CHUNKS
**********************************************************************

  METHOD render_diff_head.

    DATA: ls_stats TYPE lcl_diff=>ty_count.

    CREATE OBJECT ro_html.
    ls_stats = is_diff-o_diff->stats( ).

    IF is_diff-fstate = c_fstate-both. " Merge stats into 'update' if both were changed
      ls_stats-update = ls_stats-update + ls_stats-insert + ls_stats-delete.
      CLEAR: ls_stats-insert, ls_stats-delete.
    ENDIF.

    ro_html->add( '<div class="diff_head">' ).              "#EC NOTEXT

    ro_html->add( |<span class="diff_banner diff_ins">+ { ls_stats-insert }</span>| ).
    ro_html->add( |<span class="diff_banner diff_del">- { ls_stats-delete }</span>| ).
    ro_html->add( |<span class="diff_banner diff_upd">~ { ls_stats-update }</span>| ).
    ro_html->add( |<span class="diff_name">{ is_diff-filename }</span>| ). "#EC NOTEXT
    ro_html->add( lcl_gui_chunk_lib=>render_item_state( iv1 = is_diff-lstate
                                                        iv2 = is_diff-rstate ) ).

    IF is_diff-fstate = c_fstate-both AND mv_unified = abap_true.
      ro_html->add( '<span class="attention pad-sides">Attention: Unified mode'
                 && ' highlighting for MM assumes local file is newer ! </span>' ). "#EC NOTEXT
    ENDIF.

    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.

  METHOD render_table_head.

    CREATE OBJECT ro_html.

    IF mv_unified = abap_true.
      ro_html->add( '<thead class="header">' ).               "#EC NOTEXT
      ro_html->add( '<tr>' ).                                 "#EC NOTEXT
      ro_html->add( '<th class="num">old</th>' ).             "#EC NOTEXT
      ro_html->add( '<th class="num">new</th>' ).             "#EC NOTEXT
      ro_html->add( '<th>code</th>' ).                        "#EC NOTEXT
      ro_html->add( '</tr>' ).                                "#EC NOTEXT
      ro_html->add( '</thead>' ).                             "#EC NOTEXT
    ELSE.
      ro_html->add( '<thead class="header">' ).               "#EC NOTEXT
      ro_html->add( '<tr>' ).                                 "#EC NOTEXT
      ro_html->add( '<th class="num"></th>' ).                "#EC NOTEXT
      ro_html->add( '<th>LOCAL</th>' ).                       "#EC NOTEXT
      ro_html->add( '<th class="num"></th>' ).                "#EC NOTEXT
      ro_html->add( '<th>REMOTE</th>' ).                      "#EC NOTEXT
      ro_html->add( '</tr>' ).                                "#EC NOTEXT
      ro_html->add( '</thead>' ).                             "#EC NOTEXT
    ENDIF.

  ENDMETHOD.  " render_table_head.

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

    IF mv_unified = abap_true.
      ro_html->add( '<th class="num"></th>' ).
      ro_html->add( '<th class="num"></th>' ).
      ro_html->add( |<th>@@ { is_diff_line-new_num } @@ { lv_beacon }</th>| ).
    ELSE.
      ro_html->add( '<th class="num"></th>' ).
      ro_html->add( |<th colspan="3">@@ { is_diff_line-new_num } @@ { lv_beacon }</th>| ).
    ENDIF.

    ro_html->add( '</tr>' ).
    ro_html->add( '</thead>' ).

  ENDMETHOD.  " render_beacon.

  METHOD render_lines.

    DATA: lo_highlighter TYPE REF TO lcl_syntax_highlighter,
          lt_diffs       TYPE lcl_diff=>ty_diffs_tt,
          lv_insert_nav  TYPE abap_bool.

    FIELD-SYMBOLS <ls_diff>  LIKE LINE OF lt_diffs.

    lo_highlighter = lcl_syntax_highlighter=>create( is_diff-filename ).
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

  ENDMETHOD.  "render_lines

  METHOD render_line_split.

    DATA: lv_new  TYPE string,
          lv_old  TYPE string,
          lv_mark TYPE string,
          lv_bg   TYPE string.

    CREATE OBJECT ro_html.

    " New line
    lv_mark = ` `.
    IF iv_fstate = c_fstate-both OR is_diff_line-result = lcl_diff=>c_diff-update.
      lv_bg = ' diff_upd'.
      lv_mark = `~`.
    ELSEIF is_diff_line-result = lcl_diff=>c_diff-insert.
      lv_bg = ' diff_ins'.
      lv_mark = `+`.
    ENDIF.
    lv_new = |<td class="num" line-num="{ is_diff_line-new_num }"></td>|
          && |<td class="code{ lv_bg }">{ lv_mark }{ is_diff_line-new }</td>|.

    " Old line
    CLEAR lv_bg.
    lv_mark = ` `.
    IF iv_fstate = c_fstate-both OR is_diff_line-result = lcl_diff=>c_diff-update.
      lv_bg = ' diff_upd'.
      lv_mark = `~`.
    ELSEIF is_diff_line-result = lcl_diff=>c_diff-delete.
      lv_bg = ' diff_del'.
      lv_mark = `-`.
    ENDIF.
    lv_old = |<td class="num" line-num="{ is_diff_line-old_num }"></td>|
          && |<td class="code{ lv_bg }">{ lv_mark }{ is_diff_line-old }</td>|.

    " render line, inverse sides if remote is newer
    ro_html->add( '<tr>' ).                               "#EC NOTEXT
    IF iv_fstate = c_fstate-remote. " Remote file leading changes
      ro_html->add( lv_old ). " local
      ro_html->add( lv_new ). " remote
    ELSE.             " Local leading changes or both were modified
      ro_html->add( lv_new ). " local
      ro_html->add( lv_old ). " remote
    ENDIF.
    ro_html->add( '</tr>' ).                              "#EC NOTEXT

  ENDMETHOD. "render_line_split

  METHOD render_line_unified.

    DATA lv_line TYPE string.

    FIELD-SYMBOLS <diff_line> LIKE LINE OF mt_delayed_lines.

    CREATE OBJECT ro_html.

    " Release delayed subsequent update lines
    IF is_diff_line-result <> lcl_diff=>c_diff-update.
      LOOP AT mt_delayed_lines ASSIGNING <diff_line>.
        ro_html->add( '<tr>' ).                               "#EC NOTEXT
        ro_html->add( |<td class="num" line-num="{ <diff_line>-old_num }"></td>|
                   && |<td class="num" line-num=""></td>|
                   && |<td class="code diff_del">-{ <diff_line>-old }</td>| ).
        ro_html->add( '</tr>' ).                              "#EC NOTEXT
      ENDLOOP.
      LOOP AT mt_delayed_lines ASSIGNING <diff_line>.
        ro_html->add( '<tr>' ).                               "#EC NOTEXT
        ro_html->add( |<td class="num" line-num=""></td>|
                   && |<td class="num" line-num="{ <diff_line>-new_num }"></td>|
                   && |<td class="code diff_ins">+{ <diff_line>-new }</td>| ).
        ro_html->add( '</tr>' ).                              "#EC NOTEXT
      ENDLOOP.
      CLEAR mt_delayed_lines.
    ENDIF.

    ro_html->add( '<tr>' ).                               "#EC NOTEXT
    CASE is_diff_line-result.
      WHEN lcl_diff=>c_diff-update.
        APPEND is_diff_line TO mt_delayed_lines. " Delay output of subsequent updates
      WHEN lcl_diff=>c_diff-insert.
        ro_html->add( |<td class="num" line-num=""></td>|
                   && |<td class="num" line-num="{ is_diff_line-new_num }"></td>|
                   && |<td class="code diff_ins">+{ is_diff_line-new }</td>| ).
      WHEN lcl_diff=>c_diff-delete.
        ro_html->add( |<td class="num" line-num="{ is_diff_line-old_num }"></td>|
                   && |<td class="num" line-num=""></td>|
                   && |<td class="code diff_del">-{ is_diff_line-old }</td>| ).
      WHEN OTHERS. "none
        ro_html->add( |<td class="num" line-num="{ is_diff_line-old_num }"></td>|
                   && |<td class="num" line-num="{ is_diff_line-new_num }"></td>|
                   && |<td class="code"> { is_diff_line-old }</td>| ).
    ENDCASE.
    ro_html->add( '</tr>' ).                              "#EC NOTEXT

  ENDMETHOD. "render_line_unified

ENDCLASS. "lcl_gui_page_diff
