*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_DIFF
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_diff DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_file_diff,
             filename TYPE string,
             lstate   TYPE char1,
             rstate   TYPE char1,
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

    METHODS styles       RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_diff
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_head
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_lines
      IMPORTING is_diff        TYPE ty_file_diff
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS get_line_hl
      IMPORTING iv_lstate TYPE char1
                iv_rstate TYPE char1
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

    IF is_file IS NOT INITIAL.

      READ TABLE lt_status ASSIGNING <ls_status>
        WITH KEY path = is_file-path filename = is_file-filename.

      append_diff( it_remote = lt_remote
                   it_local  = lt_local
                   is_status = <ls_status> ).

    ELSEIF is_object IS NOT INITIAL.

      LOOP AT lt_status ASSIGNING <ls_status>
        WHERE obj_type = is_object-obj_type
        AND   obj_name = is_object-obj_name
        AND   match IS INITIAL.

        append_diff( it_remote = lt_remote
                     it_local  = lt_local
                     is_status = <ls_status> ).

      ENDLOOP.

    ELSE. " For the whole repo

      LOOP AT lt_status ASSIGNING <ls_status>
        WHERE match IS INITIAL.

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
      ls_l_dummy   LIKE LINE OF it_local  ##NEEDED,
      ls_diff_file LIKE LINE OF mt_diff_files.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF it_remote,
                   <ls_local>  LIKE LINE OF it_local.

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

    CREATE OBJECT ls_diff_file-o_diff
      EXPORTING
        iv_new = <ls_local>-file-data
        iv_old = <ls_remote>-data.

    ls_diff_file-filename = is_status-filename.
    ls_diff_file-lstate   = is_status-lstate.
    ls_diff_file-rstate   = is_status-rstate.
    APPEND ls_diff_file TO mt_diff_files.

  ENDMETHOD.  "append_diff

  METHOD styles.

    CREATE OBJECT ro_html.

    _add '/* DIFF */'.                           "#EC NOTEXT
    _add 'div.diff {'.                           "#EC NOTEXT
    _add '  background-color: #f2f2f2;'.         "#EC NOTEXT
    _add '  padding: 0.7em    '.                 "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add 'div.diff_head {'.                      "#EC NOTEXT
    _add '  padding-bottom: 0.7em;'.             "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add 'span.diff_name {'.                     "#EC NOTEXT
    _add '  padding-left: 0.5em;'.               "#EC NOTEXT
    _add '  color: grey;'.                       "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add 'span.diff_name strong {'.              "#EC NOTEXT
    _add '  color: #333;'.                       "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add 'span.diff_banner {'.                   "#EC NOTEXT
    _add '  border-style: solid;'.               "#EC NOTEXT
    _add '  border-width: 1px;'.                 "#EC NOTEXT
    _add '  border-radius: 3px;'.                "#EC NOTEXT
    _add '  padding-left: 0.3em;'.               "#EC NOTEXT
    _add '  padding-right: 0.3em;'.              "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add '.diff_ins {'.                          "#EC NOTEXT
    _add '  border-color: #7bea7b;'.             "#EC NOTEXT
    _add '  background-color: #d3f8d3;'.         "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add '.diff_del {'.                          "#EC NOTEXT
    _add '  border-color: #ff667d;'.             "#EC NOTEXT
    _add '  background-color: #ffccd4;'.         "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add '.diff_upd {'.                          "#EC NOTEXT
    _add '  border-color: #dada00;'.             "#EC NOTEXT
    _add '  background-color: #ffffcc;'.         "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add 'div.diff_content {'.                   "#EC NOTEXT
    _add '  background: #fff;'.                  "#EC NOTEXT
    _add '  border-top: 1px solid #DDD;'.        "#EC NOTEXT
    _add '  border-bottom: 1px solid #DDD;'.     "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT

    _add 'div.diff_head span.state-block {'.
    _add '  margin-left: 0.5em;'.
    _add '  font-family: Consolas, Lucida Console, Courier, monospace;'.
    _add '  display: inline-block;'.
    _add '  text-align: center;'.
    _add '}'.
    _add 'div.diff_head span.state-block span {'.
    _add '  display: inline-block;'.
    _add '  padding: 0px 4px;'.
    _add '  border: 1px solid #000;'.
    _add '}'.
    _add 'div.diff_head span.state-block span.added {'.
    _add '  background-color: #69ad74; '.
    _add '  border-color: #579e64;'.
    _add '  color: white;'.
    _add '}'.
    _add 'div.diff_head span.state-block span.changed {'.
    _add '  background-color: #e0c150;'.
    _add '  border-color: #d4af25;'.
    _add '  color: white;'.
    _add '}'.
    _add 'div.diff_head span.state-block span.mixed {'.
    _add '  background-color: #e0c150;'.
    _add '  border-color: #579e64;'.
    _add '  color: #69ad74;'.
    _add '}'.
    _add 'div.diff_head span.state-block span.deleted {'.
    _add '  background-color: #c76861;'.
    _add '  border-color: #b8605a;'.
    _add '  color: white;'.
    _add '}'.
    _add 'div.diff_head span.state-block span.none {'.
    _add '  background-color: #e8e8e8;'.
    _add '  border-color: #dbdbdb;'.
    _add '  color: #c8c8c8;'.
    _add '}'.

    " Table part
    _add '/* DIFF TABLE */'.                     "#EC NOTEXT
    _add 'table.diff_tab {'.                     "#EC NOTEXT
    _add '  font-family: Consolas, Courier, monospace;'. "#EC NOTEXT
    _add '  font-size: 10pt;'.                   "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add 'table.diff_tab td,th {'.                  "#EC NOTEXT
    _add '  color: #444;'.                       "#EC NOTEXT
    _add '  padding-left: 0.5em;'.               "#EC NOTEXT
    _add '  padding-right: 0.5em;'.              "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add 'table.diff_tab th {'.                  "#EC NOTEXT
    _add '  text-align: left;'.                  "#EC NOTEXT
    _add '  font-weight: normal;'.               "#EC NOTEXT
    _add '  padding-top: 3px;'.                  "#EC NOTEXT
    _add '  padding-bottom: 3px;'.               "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add 'table.diff_tab thead.header th {'.     "#EC NOTEXT
    _add '  color: #EEE;'.                       "#EC NOTEXT
    _add '  background-color: #BBB;'.            "#EC NOTEXT
    _add '  text-align: left;'.                  "#EC NOTEXT
    _add '  font-weight: bold;'.                 "#EC NOTEXT
    _add '  padding-left: 0.5em;'.               "#EC NOTEXT
    _add '  font-size: 9pt;'.                    "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add 'table.diff_tab thead.nav_line {'.      "#EC NOTEXT
    _add '  background-color: #edf2f9;'.         "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add 'table.diff_tab thead.nav_line th {'.   "#EC NOTEXT
    _add '  color: #bbb;'.                       "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add 'table.diff_tab td.num, th.num {'.      "#EC NOTEXT
    _add '  text-align: right;'.                 "#EC NOTEXT
    _add '  color: #ccc;'.                       "#EC NOTEXT
    _add '  border-left: 1px solid #eee;'.       "#EC NOTEXT
    _add '  border-right: 1px solid #eee;'.      "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add 'table.diff_tab code {'.                "#EC NOTEXT
    _add '  font-family: inherit;'.              "#EC NOTEXT
    _add '  white-space: pre;'.                  "#EC NOTEXT
    _add '}'.                                    "#EC NOTEXT
    _add 'table.diff_tab tbody tr:first-child td { padding-top: 0.5em; }'.
    _add 'table.diff_tab tbody tr:last-child td { padding-bottom: 0.5em; }'.

  ENDMETHOD.

  METHOD render_head.
    DATA: lo_html  TYPE REF TO lcl_html_helper,
          ls_stats TYPE lcl_diff=>ty_count.

    CREATE OBJECT lo_html.

    ls_stats = is_diff-o_diff->stats( ).

    lo_html->add( '<div class="diff_head">' ).              "#EC NOTEXT
    lo_html->add( |<span class="diff_banner diff_ins">+ { ls_stats-insert }</span>| ).
    lo_html->add( |<span class="diff_banner diff_del">- { ls_stats-delete }</span>| ).
    lo_html->add( |<span class="diff_banner diff_upd">~ { ls_stats-update }</span>| ).
    lo_html->add( '<span class="diff_name">' ).             "#EC NOTEXT
    lo_html->add( |{ is_diff-filename }| ).
    lo_html->add( '</span>' ).                              "#EC NOTEXT
    lo_html->add( render_item_state( iv1 = is_diff-lstate iv2 = is_diff-rstate ) ).
    lo_html->add( '</div>' ).                               "#EC NOTEXT

    ro_html = lo_html.
  ENDMETHOD.

  METHOD render_diff.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="diff">' ).                   "#EC NOTEXT
    ro_html->add( render_head( is_diff ) ).

    " Content
    ro_html->add( '<div class="diff_content">' ).           "#EC NOTEXT
    ro_html->add( '<table width="100%" class="diff_tab">' ). "#EC NOTEXT
    ro_html->add(   '<thead class="header"><tr>' ).         "#EC NOTEXT
    ro_html->add(   '<th class="num"></th>' ).              "#EC NOTEXT
    ro_html->add(   '<th>LOCAL</th>' ).                     "#EC NOTEXT
    ro_html->add(   '<th class="num"></th>' ).              "#EC NOTEXT
    ro_html->add(   '<th>REMOTE</th>' ).                    "#EC NOTEXT
    ro_html->add(   '</tr></thead>' ).                      "#EC NOTEXT
    ro_html->add( render_lines( is_diff ) ).
    ro_html->add( '</table>' ).                             "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT

    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.

  METHOD render_lines.

    DATA: lt_diffs       TYPE lcl_diff=>ty_diffs_tt,
          lv_local       TYPE string,
          lv_remote      TYPE string,
          lv_lattr       TYPE string,
          lv_rattr       TYPE string,
          lv_beacon      TYPE string,
          lv_insert_nav  TYPE abap_bool.

    FIELD-SYMBOLS <ls_diff>  LIKE LINE OF lt_diffs.


    CREATE OBJECT ro_html.
    lt_diffs = is_diff-o_diff->get( ).

    LOOP AT lt_diffs ASSIGNING <ls_diff>.
      IF <ls_diff>-short = abap_false.
        lv_insert_nav = abap_true.
        CONTINUE.
      ENDIF.

      IF lv_insert_nav = abap_true. " Insert separator line with navigation
        IF <ls_diff>-beacon > 0.
          READ TABLE is_diff-o_diff->mt_beacons INTO lv_beacon INDEX <ls_diff>-beacon.
        ELSE.
          lv_beacon = '---'.
        ENDIF.

        ro_html->add( '<thead class="nav_line"><tr>').
        ro_html->add( '<th class="num"></th>' ).
        ro_html->add( |<th colspan="3">@@ { <ls_diff>-new_line } @@ { lv_beacon }</th>| ).
        ro_html->add( '</tr></thead>' ).
        lv_insert_nav = abap_false.
      ENDIF.

      lv_local  = escape( val = <ls_diff>-new format = cl_abap_format=>e_html_attr ).
      lv_remote = escape( val = <ls_diff>-old format = cl_abap_format=>e_html_attr ).

      get_line_hl( EXPORTING iv_lstate = is_diff-lstate
                             iv_rstate = is_diff-rstate
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

    IF iv_lstate IS NOT INITIAL AND iv_rstate IS NOT INITIAL AND iv_result IS NOT INITIAL
      OR iv_result = lcl_diff=>c_diff-update.
      " Both file changed ? Or line updated ? - All yellow
      ev_lattr = ' class="diff_upd"'.             "#EC NOTEXT
      ev_rattr = ' class="diff_upd"'.             "#EC NOTEXT
    ELSEIF iv_lstate IS NOT INITIAL. " Changed locally
      CASE iv_result.
        WHEN lcl_diff=>c_diff-insert.
          ev_lattr = ' class="diff_ins"'.            "#EC NOTEXT
        WHEN lcl_diff=>c_diff-delete.
          ev_rattr = ' class="diff_del"'.             "#EC NOTEXT
      ENDCASE.
    ELSEIF iv_rstate IS NOT INITIAL. " Changed remotely
      CASE iv_result.
        WHEN lcl_diff=>c_diff-insert.
          ev_lattr = ' class="diff_del"'.            "#EC NOTEXT
        WHEN lcl_diff=>c_diff-delete.
          ev_rattr = ' class="diff_ins"'.             "#EC NOTEXT
      ENDCASE.
    ENDIF.

  ENDMETHOD.  " get_line_hl.

  METHOD lif_gui_page~render.

    DATA ls_diff_file LIKE LINE OF mt_diff_files.

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'DIFF' ) ).

    LOOP AT mt_diff_files INTO ls_diff_file.
      ro_html->add( render_diff( ls_diff_file ) ).
    ENDLOOP.

    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS. "lcl_gui_page_diff