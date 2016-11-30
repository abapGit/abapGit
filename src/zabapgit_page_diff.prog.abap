*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_DIFF
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_diff DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF c_mod,
        local  TYPE char1 VALUE 'L',
        remote TYPE char1 VALUE 'R',
        both   TYPE char1 VALUE 'B',
      END OF c_mod.

    TYPES: BEGIN OF ty_file_diff,
             filename TYPE string,
             lstate   TYPE char1,
             rstate   TYPE char1,
             mod      TYPE char1, " Abstraction for shorter ifs
             o_diff   TYPE REF TO lcl_diff,
           END OF ty_file_diff,
           tt_file_diff TYPE STANDARD TABLE OF ty_file_diff.

    METHODS: constructor
      IMPORTING iv_key    TYPE lcl_persistence_repo=>ty_repo-key
                is_file   TYPE ty_file OPTIONAL
                is_object TYPE ty_item OPTIONAL
      RAISING   lcx_exception.

    METHODS lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.
    DATA: mt_diff_files TYPE tt_file_diff.

    METHODS render_diff
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_diff_head
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_table_head
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_lines
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_beacon
      IMPORTING is_diff_line   TYPE lcl_diff=>ty_diff
                is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS get_line_hl
      IMPORTING iv_mod    TYPE char1
                iv_result TYPE lcl_diff=>ty_diff-result
      EXPORTING ev_lattr  TYPE string
                ev_rattr  TYPE string.
    METHODS append_diff
      IMPORTING it_remote   TYPE ty_files_tt
                it_local    TYPE ty_files_item_tt
                is_status   TYPE ty_result
      RAISING   lcx_exception.

ENDCLASS. "lcl_gui_page_diff

CLASS lcl_gui_page_diff IMPLEMENTATION.

  METHOD constructor.

    DATA: lt_remote TYPE ty_files_tt,
          lt_local  TYPE ty_files_item_tt,
          lt_status TYPE ty_results_tt,
          lo_repo   TYPE REF TO lcl_repo_online.

    FIELD-SYMBOLS: <ls_status> LIKE LINE OF lt_status.

    super->constructor( ).

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
      ls_r_dummy   LIKE LINE OF it_remote ##NEEDED,
      ls_l_dummy   LIKE LINE OF it_local  ##NEEDED.


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
      <ls_diff>-mod = c_mod-both.
    ELSEIF <ls_diff>-lstate IS NOT INITIAL.
      <ls_diff>-mod = c_mod-local.
    ELSE. "rstate IS NOT INITIAL, lstate = empty.
      <ls_diff>-mod = c_mod-remote.
    ENDIF.

    IF <ls_diff>-mod = c_mod-remote. " Remote file leading changes
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

  METHOD render_diff_head.
    DATA: lo_html  TYPE REF TO lcl_html_helper,
          ls_stats TYPE lcl_diff=>ty_count.

    CREATE OBJECT lo_html.

    ls_stats = is_diff-o_diff->stats( ).

    IF is_diff-mod = c_mod-both. " Merge stats into 'update' if both were changed
      ls_stats-update = ls_stats-update + ls_stats-insert + ls_stats-delete.
      CLEAR: ls_stats-insert, ls_stats-delete.
    ENDIF.

    lo_html->add( '<div class="diff_head">' ).              "#EC NOTEXT
    lo_html->add( |<span class="diff_banner diff_ins">+ { ls_stats-insert }</span>| ).
    lo_html->add( |<span class="diff_banner diff_del">- { ls_stats-delete }</span>| ).
    lo_html->add( |<span class="diff_banner diff_upd">~ { ls_stats-update }</span>| ).
    lo_html->add( |<span class="diff_name">{ is_diff-filename }</span>| ). "#EC NOTEXT
    lo_html->add( render_item_state( iv1 = is_diff-lstate iv2 = is_diff-rstate ) ).
    lo_html->add( '</div>' ).                               "#EC NOTEXT

    ro_html = lo_html.
  ENDMETHOD.

  METHOD render_table_head.

    CREATE OBJECT ro_html.

    ro_html->add( '<thead class="header">' ).             "#EC NOTEXT
    ro_html->add( '<tr>' ).                               "#EC NOTEXT
    ro_html->add( '<th class="num"></th>' ).              "#EC NOTEXT
    ro_html->add( '<th>LOCAL</th>' ).                     "#EC NOTEXT
    ro_html->add( '<th class="num"></th>' ).              "#EC NOTEXT
    ro_html->add( '<th>REMOTE</th>' ).                    "#EC NOTEXT
    ro_html->add( '</tr>' ).                              "#EC NOTEXT
    ro_html->add( '</thead>' ).                           "#EC NOTEXT

  ENDMETHOD.  " render_table_head.

  METHOD render_diff.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="diff">' ).                   "#EC NOTEXT
    ro_html->add( render_diff_head( is_diff ) ).

    " Content
    ro_html->add( '<div class="diff_content">' ).           "#EC NOTEXT
    ro_html->add( '<table width="100%" class="diff_tab">' ). "#EC NOTEXT
    ro_html->add( render_table_head( ) ).
    ro_html->add( render_lines( is_diff ) ).
    ro_html->add( '</table>' ).                             "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT

    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.

  METHOD render_beacon.

    DATA: lv_beacon TYPE string.

    CREATE OBJECT ro_html.

    IF is_diff_line-beacon > 0.
      READ TABLE is_diff-o_diff->mt_beacons INTO lv_beacon INDEX is_diff_line-beacon.
    ELSE.
      lv_beacon = '---'.
    ENDIF.

    ro_html->add( '<thead class="nav_line">' ).
    ro_html->add( '<tr>' ).
    ro_html->add( '<th class="num"></th>' ).
    ro_html->add( |<th colspan="3">@@ { is_diff_line-new_line } @@ { lv_beacon }</th>| ).
    ro_html->add( '</tr>' ).
    ro_html->add( '</thead>' ).

  ENDMETHOD.  " render_beacon.

  METHOD render_lines.

    DATA: lo_highlighter TYPE REF TO lcl_code_highlighter,
          lt_diffs       TYPE lcl_diff=>ty_diffs_tt,
          lv_local       TYPE string,
          lv_remote      TYPE string,
          lv_lattr       TYPE string,
          lv_rattr       TYPE string,
          lv_insert_nav  TYPE abap_bool.

    FIELD-SYMBOLS <ls_diff>  LIKE LINE OF lt_diffs.

    CREATE OBJECT lo_highlighter.
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

      IF is_diff-mod = c_mod-remote. " Remote file leading changes
        lv_local  = lo_highlighter->process_line( <ls_diff>-old ).
        lv_remote = lo_highlighter->process_line( <ls_diff>-new ).
*        lv_local  = escape( val = <ls_diff>-old format = cl_abap_format=>e_html_attr ).
*        lv_remote = escape( val = <ls_diff>-new format = cl_abap_format=>e_html_attr ).
      ELSE.             " Local leading changes or both were modified
        lv_local  = lo_highlighter->process_line( <ls_diff>-new ).
        lv_remote = lo_highlighter->process_line( <ls_diff>-old ).
*        lv_local  = escape( val = <ls_diff>-new format = cl_abap_format=>e_html_attr ).
*        lv_remote = escape( val = <ls_diff>-old format = cl_abap_format=>e_html_attr ).
      ENDIF.

      get_line_hl( EXPORTING iv_mod    = is_diff-mod
                             iv_result = <ls_diff>-result
                   IMPORTING ev_lattr  = lv_lattr
                             ev_rattr  = lv_rattr ).

      ro_html->add( '<tr>' ).                               "#EC NOTEXT
      ro_html->add( |<td class="num">{ <ls_diff>-new_line }</td>| ). "#EC NOTEXT
      ro_html->add( |<td{ lv_lattr }><code>{ lv_local }</code></td>| ). "#EC NOTEXT
      ro_html->add( |<td class="num">{ <ls_diff>-old_line }</td>| ). "#EC NOTEXT
      ro_html->add( |<td{ lv_rattr }><code>{ lv_remote }</code></td>| ). "#EC NOTEXT
      ro_html->add( '</tr>' ).                              "#EC NOTEXT

    ENDLOOP.

  ENDMETHOD.

  METHOD get_line_hl.

    CLEAR: ev_lattr, ev_rattr. " Class for changed lines

    IF iv_result IS INITIAL.
      RETURN.
    ENDIF.

    " Both file changed ? Or line updated ? - All yellow
    IF iv_mod = c_mod-both OR iv_result = lcl_diff=>c_diff-update.
      ev_lattr = ' class="diff_upd"'.             "#EC NOTEXT
      ev_rattr = ' class="diff_upd"'.             "#EC NOTEXT
    ELSEIF iv_mod = c_mod-local. " Changed locally
      CASE iv_result.
        WHEN lcl_diff=>c_diff-insert.
          ev_lattr = ' class="diff_ins"'.            "#EC NOTEXT
        WHEN lcl_diff=>c_diff-delete.
          ev_rattr = ' class="diff_del"'.             "#EC NOTEXT
      ENDCASE.
    ELSEIF iv_mod = c_mod-remote. " Changed remotely - invert sides
      CASE iv_result.
        WHEN lcl_diff=>c_diff-insert.
          ev_rattr = ' class="diff_ins"'.            "#EC NOTEXT
        WHEN lcl_diff=>c_diff-delete.
          ev_lattr = ' class="diff_del"'.             "#EC NOTEXT
      ENDCASE.
    ENDIF.

  ENDMETHOD.  " get_line_hl.

  METHOD lif_gui_page~render.

    DATA ls_diff_file LIKE LINE OF mt_diff_files.

    CREATE OBJECT ro_html.

    ro_html->add( header( ) ).
    ro_html->add( title( 'DIFF' ) ).

    LOOP AT mt_diff_files INTO ls_diff_file.
      ro_html->add( render_diff( ls_diff_file ) ).
    ENDLOOP.

    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS. "lcl_gui_page_diff