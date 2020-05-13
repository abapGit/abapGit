CLASS zcl_abapgit_gui_page_merge_res DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        io_repo       TYPE REF TO zcl_abapgit_repo_online
        io_merge_page TYPE REF TO zcl_abapgit_gui_page_merge
        io_merge      TYPE REF TO zcl_abapgit_merge
      RAISING
        zcx_abapgit_exception.

    METHODS zif_abapgit_gui_event_handler~on_event
         REDEFINITION .
  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_file_diff,
        path       TYPE string,
        filename   TYPE string,
        lstate     TYPE char1,
        rstate     TYPE char1,
        fstate     TYPE char1, " FILE state - Abstraction for shorter ifs
        o_diff     TYPE REF TO zcl_abapgit_diff,
        changed_by TYPE xubname,
        type       TYPE string,
      END OF ty_file_diff .

    CONSTANTS:
      BEGIN OF c_actions,
        toggle_mode  TYPE string VALUE 'toggle_mode' ##NO_TEXT,
        apply_merge  TYPE string VALUE 'apply_merge' ##NO_TEXT,
        apply_source TYPE string VALUE 'apply_source' ##NO_TEXT,
        apply_target TYPE string VALUE 'apply_target' ##NO_TEXT,
        cancel       TYPE string VALUE 'cancel' ##NO_TEXT,
      END OF c_actions .
    CONSTANTS:
      BEGIN OF c_merge_mode,
        selection TYPE string VALUE 'selection' ##NO_TEXT,
        merge     TYPE string VALUE 'merge' ##NO_TEXT,
      END OF c_merge_mode .
    DATA mo_merge TYPE REF TO zcl_abapgit_merge .
    DATA mo_merge_page TYPE REF TO zcl_abapgit_gui_page_merge .
    DATA mo_repo TYPE REF TO zcl_abapgit_repo_online .
    DATA ms_diff_file TYPE ty_file_diff .
    DATA mv_current_conflict_index TYPE sy-tabix .
    DATA mv_merge_mode TYPE string .
    DATA mt_conflicts TYPE zif_abapgit_definitions=>tt_merge_conflict .

    METHODS apply_merged_content
      IMPORTING
        !it_postdata TYPE cnht_post_data_tab
      RAISING
        zcx_abapgit_exception .
    METHODS build_menu
      RETURNING
        VALUE(ro_menu)          TYPE REF TO zcl_abapgit_html_toolbar .
    METHODS is_binary
      IMPORTING
        !iv_d1        TYPE xstring
        !iv_d2        TYPE xstring
      RETURNING
        VALUE(rv_yes) TYPE abap_bool .
    METHODS render_beacon
      IMPORTING
        !is_diff_line  TYPE zif_abapgit_definitions=>ty_diff
        !is_diff       TYPE ty_file_diff
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_diff
      IMPORTING
        !is_diff       TYPE ty_file_diff
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
    METHODS render_diff_head
      IMPORTING
        !is_diff       TYPE ty_file_diff
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_lines
      IMPORTING
        !is_diff       TYPE ty_file_diff
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_line_split
      IMPORTING
        !is_diff_line  TYPE zif_abapgit_definitions=>ty_diff
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS render_table_head
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    METHODS resolve_diff
      RAISING
        zcx_abapgit_exception .
    METHODS toggle_merge_mode .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_MERGE_RES IMPLEMENTATION.


  METHOD apply_merged_content.

    CONSTANTS: lc_replace TYPE string VALUE '<<new>>'.

    DATA: BEGIN OF ls_filedata,
            merge_content TYPE string,
          END OF ls_filedata.

    DATA: lv_string           TYPE string,
          lt_fields           TYPE tihttpnvp,
          lv_new_file_content TYPE xstring.

    FIELD-SYMBOLS: <lv_postdata_line> LIKE LINE OF it_postdata,
                   <ls_conflict>      TYPE zif_abapgit_definitions=>ty_merge_conflict.

    LOOP AT it_postdata ASSIGNING <lv_postdata_line>.
      lv_string = |{ lv_string }{ <lv_postdata_line> }|.
    ENDLOOP.
    REPLACE ALL OCCURRENCES OF zif_abapgit_definitions=>c_crlf    IN lv_string WITH lc_replace.
    REPLACE ALL OCCURRENCES OF zif_abapgit_definitions=>c_newline IN lv_string WITH lc_replace.

    lt_fields = zcl_abapgit_html_action_utils=>parse_fields_upper_case_name( lv_string ).
    zcl_abapgit_html_action_utils=>get_field( EXPORTING iv_name = 'MERGE_CONTENT'
                                                        it_field = lt_fields
                                              CHANGING cg_field = ls_filedata ).
    ls_filedata-merge_content = cl_http_utility=>unescape_url( escaped = ls_filedata-merge_content ).
    REPLACE ALL OCCURRENCES OF lc_replace IN ls_filedata-merge_content WITH zif_abapgit_definitions=>c_newline.

    lv_new_file_content = zcl_abapgit_convert=>string_to_xstring_utf8( ls_filedata-merge_content ).

    READ TABLE mt_conflicts ASSIGNING <ls_conflict> INDEX mv_current_conflict_index.
    <ls_conflict>-result_sha1 = zcl_abapgit_hash=>sha1( iv_type = zif_abapgit_definitions=>c_type-blob
                                                        iv_data = lv_new_file_content ).
    <ls_conflict>-result_data = lv_new_file_content.
    mo_merge->resolve_conflict( <ls_conflict> ).

  ENDMETHOD.


  METHOD build_menu.

    CREATE OBJECT ro_menu.
    ro_menu->add( iv_txt = 'Toggle merge mode' iv_act = c_actions-toggle_mode ) ##NO_TEXT.
    ro_menu->add( iv_txt = 'Cancel' iv_act = c_actions-cancel ) ##NO_TEXT.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    mo_repo = io_repo.
    ms_control-page_title = 'Resolve Conflicts'.
    ms_control-page_menu  = build_menu( ).

    mo_merge_page = io_merge_page.
    mo_merge = io_merge.
    mv_merge_mode = c_merge_mode-selection.
    mv_current_conflict_index = 1.
    mt_conflicts = io_merge->get_conflicts( ).

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

    DATA: lv_beacon  TYPE string,
          lt_beacons TYPE zif_abapgit_definitions=>ty_string_tt.

    CREATE OBJECT ro_html.

    IF is_diff_line-beacon > 0.
      lt_beacons = is_diff-o_diff->get_beacons( ).
      READ TABLE lt_beacons INTO lv_beacon INDEX is_diff_line-beacon.
    ELSE.
      lv_beacon = '---'.
    ENDIF.


    ro_html->add( '<thead class="nav_line">' ).
    ro_html->add( '<tr>' ).

    ro_html->add( '<th class="num"></th>' ).
    ro_html->add( |<th colspan="3">@@ { is_diff_line-new_num } @@ { lv_beacon }</th>| ).

    ro_html->add( '</tr>' ).
    ro_html->add( '</thead>' ).

  ENDMETHOD.


  METHOD render_content.

    resolve_diff( ).
    IF ms_diff_file IS INITIAL.
      zcx_abapgit_exception=>raise( 'no conflict found' ).
    ENDIF.

    CREATE OBJECT ro_html.
    ro_html->add( |<div id="diff-list" data-repo-key="{ mo_repo->get_key( ) }">| ).
    ro_html->add( render_diff( ms_diff_file ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_diff.

    DATA: lv_target_content TYPE string.
    FIELD-SYMBOLS: <ls_conflict> TYPE zif_abapgit_definitions=>ty_merge_conflict.

    CREATE OBJECT ro_html.

    ro_html->add( |<div class="diff" data-type="{ is_diff-type
      }" data-changed-by="{ is_diff-changed_by
      }" data-file="{ is_diff-path && is_diff-filename }">| ). "#EC NOTEXT
    ro_html->add( render_diff_head( is_diff ) ).

    " Content
    IF is_diff-type <> 'binary'.

      IF mv_merge_mode = c_merge_mode-selection.
        ro_html->add( '<div class="diff_content">' ).       "#EC NOTEXT
        ro_html->add( '<table class="diff_tab syntax-hl">' ). "#EC NOTEXT
        ro_html->add( render_table_head( ) ).
        ro_html->add( render_lines( is_diff ) ).
        ro_html->add( '</table>' ).                         "#EC NOTEXT
        ro_html->add( '</div>' ).                           "#EC NOTEXT
      ELSE.

        "Table for Div-Table and textarea
        ro_html->add( '<div class="diff_content">' ).       "#EC NOTEXT
        ro_html->add( '<table class="w100">' ).             "#EC NOTEXT
        ro_html->add( '<thead class="header">' ).           "#EC NOTEXT
        ro_html->add( '<tr>' ).                             "#EC NOTEXT
        ro_html->add( '<th>Code</th>' ).                    "#EC NOTEXT
        ro_html->add( '<th>Merge - ' ).                     "#EC NOTEXT
        ro_html->add_a( iv_act = 'submitFormById(''merge_form'');' "#EC NOTEXT
                        iv_txt = 'Apply'
                        iv_typ = zif_abapgit_html=>c_action_type-onclick
                        iv_opt = zif_abapgit_html=>c_html_opt-strong ).
        ro_html->add( '</th> ' ).                           "#EC NOTEXT
        ro_html->add( '</tr>' ).                            "#EC NOTEXT
        ro_html->add( '</thead>' ).                         "#EC NOTEXT
        ro_html->add( '<td>' ).

        "Diff-Table of source and target file
        ro_html->add( '<table class="diff_tab syntax-hl">' ). "#EC NOTEXT
        ro_html->add( render_table_head( ) ).
        ro_html->add( render_lines( is_diff ) ).
        ro_html->add( '</table>' ).                         "#EC NOTEXT

        READ TABLE mt_conflicts ASSIGNING <ls_conflict> INDEX mv_current_conflict_index.
        IF sy-subrc = 0.
          lv_target_content = zcl_abapgit_convert=>xstring_to_string_utf8( <ls_conflict>-target_data ).
          lv_target_content = escape( val = lv_target_content format = cl_abap_format=>e_html_text ).
        ENDIF.

        ro_html->add( '</td>' ).                            "#EC NOTEXT
        ro_html->add( '<td>' ).                             "#EC NOTEXT
        ro_html->add( '<div class="form-container">' ).
        ro_html->add( |<form id="merge_form" class="aligned-form w100" accept-charset="UTF-8"| ).
        ro_html->add( |method="post" action="sapevent:apply_merge">| ).
        ro_html->add( |<textarea id="merge_content" name="merge_content" class="w100" | ).
        ro_html->add( |rows="{ lines( is_diff-o_diff->get( ) ) }">{ lv_target_content }</textarea>| ).
        ro_html->add( '<input type="submit" class="hidden-submit">' ).
        ro_html->add( '</form>' ).                          "#EC NOTEXT
        ro_html->add( '</div>' ).                           "#EC NOTEXT
        ro_html->add( '</td>' ).                            "#EC NOTEXT
        ro_html->add( '</table>' ).                         "#EC NOTEXT
        ro_html->add( '</div>' ).                           "#EC NOTEXT
      ENDIF.
    ELSE.
      ro_html->add( '<div class="diff_content paddings center grey">' ). "#EC NOTEXT
      ro_html->add( 'The content seems to be binary.' ).    "#EC NOTEXT
      ro_html->add( 'Cannot display as diff.' ).            "#EC NOTEXT
      ro_html->add( '</div>' ).                             "#EC NOTEXT
    ENDIF.

    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.


  METHOD render_diff_head.

    DATA: ls_stats TYPE zif_abapgit_definitions=>ty_count.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="diff_head">' ).              "#EC NOTEXT

    IF is_diff-type <> 'binary' AND is_diff-o_diff IS NOT INITIAL.
      ls_stats = is_diff-o_diff->stats( ).
      ro_html->add( |<span class="diff_banner diff_ins">+ { ls_stats-insert }</span>| ).
      ro_html->add( |<span class="diff_banner diff_del">- { ls_stats-delete }</span>| ).
      ro_html->add( |<span class="diff_banner diff_upd">~ { ls_stats-update }</span>| ).
    ENDIF.

    ro_html->add( |<span class="diff_name">{ is_diff-filename }</span>| ). "#EC NOTEXT
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

      ro_html->add( render_line_split( is_diff_line = <ls_diff> ) ).

    ENDLOOP.

  ENDMETHOD.


  METHOD render_line_split.

    DATA: lv_new  TYPE string,
          lv_old  TYPE string,
          lv_mark TYPE string,
          lv_bg   TYPE string.

    CREATE OBJECT ro_html.

    " New line
    lv_mark = ` `.
    IF is_diff_line-result = zif_abapgit_definitions=>c_diff-update.
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
    IF is_diff_line-result = zif_abapgit_definitions=>c_diff-update.
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
    ro_html->add( lv_old ). " Target
    ro_html->add( lv_new ). " Source
    ro_html->add( '</tr>' ).                                "#EC NOTEXT

  ENDMETHOD.


  METHOD render_table_head.

    CREATE OBJECT ro_html.

    ro_html->add( '<thead class="header">' ).               "#EC NOTEXT
    ro_html->add( '<tr>' ).                                 "#EC NOTEXT
    ro_html->add( '<th class="num"></th>' ).                "#EC NOTEXT

    IF mv_merge_mode = c_merge_mode-selection.
      ro_html->add( '<form id="target_form" method="post" action="sapevent:apply_target">' ). "#EC NOTEXT
      ro_html->add( '<th>Target - ' && mo_repo->get_branch_name( ) && ' - ' ). "#EC NOTEXT
      ro_html->add_a( iv_act = 'submitFormById(''target_form'');' "#EC NOTEXT
                      iv_txt = 'Apply'
                      iv_typ = zif_abapgit_html=>c_action_type-onclick
                      iv_opt = zif_abapgit_html=>c_html_opt-strong ).
      ro_html->add( '</th> ' ).                             "#EC NOTEXT
      ro_html->add( '</form>' ).                            "#EC NOTEXT
      ro_html->add( '<th class="num"></th>' ).              "#EC NOTEXT
      ro_html->add( '<form id="source_form" method="post" action="sapevent:apply_source">' ). "#EC NOTEXT
      ro_html->add( '<th>Source  - ' && mo_merge->get_source_branch( ) && ' - ' ). "#EC NOTEXT
      ro_html->add_a( iv_act = 'submitFormById(''source_form'');' "#EC NOTEXT
                      iv_txt = 'Apply'
                      iv_typ = zif_abapgit_html=>c_action_type-onclick
                      iv_opt = zif_abapgit_html=>c_html_opt-strong ).
      ro_html->add( '</th> ' ).                             "#EC NOTEXT
      ro_html->add( '</form>' ).                            "#EC NOTEXT
    ELSE.
      ro_html->add( '<th>Target - ' && mo_repo->get_branch_name( ) && '</th> ' ). "#EC NOTEXT
      ro_html->add( '<th class="num"></th>' ).              "#EC NOTEXT
      ro_html->add( '<th>Source - ' && mo_merge->get_source_branch( ) && '</th> ' ). "#EC NOTEXT
    ENDIF.

    ro_html->add( '</tr>' ).                                "#EC NOTEXT
    ro_html->add( '</thead>' ).                             "#EC NOTEXT

  ENDMETHOD.


  METHOD resolve_diff.

    DATA: lv_offs TYPE i.
    FIELD-SYMBOLS: <ls_conflict> TYPE zif_abapgit_definitions=>ty_merge_conflict.

    CLEAR ms_diff_file.

    READ TABLE mt_conflicts ASSIGNING <ls_conflict> INDEX mv_current_conflict_index.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ms_diff_file-path     = <ls_conflict>-path.
    ms_diff_file-filename = <ls_conflict>-filename.
    ms_diff_file-type = reverse( <ls_conflict>-filename ).

    FIND FIRST OCCURRENCE OF '.' IN ms_diff_file-type MATCH OFFSET lv_offs.
    ms_diff_file-type = reverse( substring( val = ms_diff_file-type len = lv_offs ) ).
    IF ms_diff_file-type <> 'xml' AND ms_diff_file-type <> 'abap'.
      ms_diff_file-type = 'other'.
    ENDIF.

    IF ms_diff_file-type = 'other'
    AND is_binary( iv_d1 = <ls_conflict>-source_data iv_d2 = <ls_conflict>-target_data ) = abap_true.
      ms_diff_file-type = 'binary'.
    ENDIF.

    IF ms_diff_file-type <> 'binary'.
      CREATE OBJECT ms_diff_file-o_diff
        EXPORTING
          iv_new = <ls_conflict>-source_data
          iv_old = <ls_conflict>-target_data.
    ENDIF.

  ENDMETHOD.


  METHOD toggle_merge_mode.

    IF mv_merge_mode = c_merge_mode-selection.
      mv_merge_mode = c_merge_mode-merge.
    ELSE.
      mv_merge_mode = c_merge_mode-selection.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    FIELD-SYMBOLS: <ls_conflict> TYPE zif_abapgit_definitions=>ty_merge_conflict.

    CASE iv_action.
      WHEN c_actions-apply_merge
        OR c_actions-apply_source
        OR c_actions-apply_target
        OR c_actions-cancel.

        CASE iv_action.
          WHEN c_actions-apply_merge.
            apply_merged_content( it_postdata ).

          WHEN c_actions-apply_source.
            READ TABLE mt_conflicts ASSIGNING <ls_conflict> INDEX mv_current_conflict_index.
            <ls_conflict>-result_sha1 = <ls_conflict>-source_sha1.
            <ls_conflict>-result_data = <ls_conflict>-source_data.
            mo_merge->resolve_conflict( <ls_conflict> ).

          WHEN c_actions-apply_target.
            READ TABLE mt_conflicts ASSIGNING <ls_conflict> INDEX mv_current_conflict_index.
            <ls_conflict>-result_sha1 = <ls_conflict>-target_sha1.
            <ls_conflict>-result_data = <ls_conflict>-target_data.
            mo_merge->resolve_conflict( <ls_conflict> ).

        ENDCASE.

        mv_current_conflict_index = mv_current_conflict_index + 1.
        IF mv_current_conflict_index > lines( mt_conflicts ).
          CLEAR mv_current_conflict_index.
        ENDIF.

        IF mv_current_conflict_index IS NOT INITIAL.
          ev_state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          ei_page = mo_merge_page.
          ev_state = zcl_abapgit_gui=>c_event_state-go_back.
        ENDIF.

      WHEN c_actions-toggle_mode.
        toggle_merge_mode( ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
