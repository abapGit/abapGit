class ZCL_ABAPGIT_GUI_PAGE_MERGE_RES definition
  public
  inheriting from ZCL_ABAPGIT_GUI_PAGE
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_REPO type ref to ZCL_ABAPGIT_REPO_ONLINE
      !IO_MERGE_PAGE type ref to ZCL_ABAPGIT_GUI_PAGE_MERGE
      !IS_MERGE type ZIF_ABAPGIT_DEFINITIONS=>TY_MERGE
    raising
      ZCX_ABAPGIT_EXCEPTION .

  methods ZIF_ABAPGIT_GUI_PAGE~ON_EVENT
    redefinition .
  PROTECTED SECTION.
    METHODS render_content REDEFINITION.

private section.

  types:
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
  types:
    tt_file_diff TYPE STANDARD TABLE OF ty_file_diff .

  data MO_REPO type ref to ZCL_ABAPGIT_REPO_ONLINE .
  data MS_MERGE type ZIF_ABAPGIT_DEFINITIONS=>TY_MERGE .
  constants:
    BEGIN OF c_actions,
        apply  TYPE string VALUE 'apply' ##NO_TEXT,
        cancel TYPE string VALUE 'cancel' ##NO_TEXT,
      END OF c_actions .
  data MO_MERGE_PAGE type ref to ZCL_ABAPGIT_GUI_PAGE_MERGE .
  data MV_CURRENT_CONFLICT_INDEX type SYTABIX .
  data MS_DIFF_FILE type TY_FILE_DIFF .

  methods APPLY_MERGED_CONTENT
    importing
      !IT_POSTDATA type CNHT_POST_DATA_TAB
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods GET_FILE_CONTENT
    importing
      !IV_BRANCH type ZIF_ABAPGIT_PERSISTENCE=>TY_REPO-BRANCH_NAME
      !IV_PATH type STRING
      !IV_FILENAME type STRING
    returning
      value(RV_FILE_CONTENT) type STRING
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods FIND_NEXT_CONFLICT .
  methods RENDER_DIFF
    importing
      !IS_DIFF type TY_FILE_DIFF
    returning
      value(RO_HTML) type ref to ZCL_ABAPGIT_HTML
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods RENDER_DIFF_HEAD
    importing
      !IS_DIFF type TY_FILE_DIFF
    returning
      value(RO_HTML) type ref to ZCL_ABAPGIT_HTML .
  methods BUILD_MENU
    importing
      value(IV_WITH_CONFLICT) type BOOLEAN optional
    returning
      value(RO_MENU) type ref to ZCL_ABAPGIT_HTML_TOOLBAR .
  methods RENDER_BEACON
    importing
      !IS_DIFF_LINE type ZIF_ABAPGIT_DEFINITIONS=>TY_DIFF
      !IS_DIFF type TY_FILE_DIFF
    returning
      value(RO_HTML) type ref to ZCL_ABAPGIT_HTML .
  methods RENDER_TABLE_HEAD
    returning
      value(RO_HTML) type ref to ZCL_ABAPGIT_HTML .
  methods RENDER_LINES
    importing
      !IS_DIFF type TY_FILE_DIFF
    returning
      value(RO_HTML) type ref to ZCL_ABAPGIT_HTML .
  methods RENDER_LINE_SPLIT
    importing
      !IS_DIFF_LINE type ZIF_ABAPGIT_DEFINITIONS=>TY_DIFF
      !IV_FSTATE type CHAR1
    returning
      value(RO_HTML) type ref to ZCL_ABAPGIT_HTML .
  methods RESOLVE_DIFF
    raising
      ZCX_ABAPGIT_EXCEPTION .
  methods IS_BINARY
    importing
      !IV_D1 type XSTRING
      !IV_D2 type XSTRING
    returning
      value(RV_YES) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_MERGE_RES IMPLEMENTATION.


  METHOD apply_merged_content.

    DATA: BEGIN OF filedata,
            merge_content TYPE string,
          END OF filedata.

    DATA: lv_string TYPE string,
          lt_fields TYPE tihttpnvp.

    FIELD-SYMBOLS <postdata_line> LIKE LINE OF it_postdata.

    LOOP AT it_postdata ASSIGNING <postdata_line>.
      lv_string = |{ lv_string }{ <postdata_line> }|.
    ENDLOOP.
    lt_fields = zcl_abapgit_html_action_utils=>parse_fields_upper_case_name( lv_string ).

    zcl_abapgit_html_action_utils=>get_field( EXPORTING name = 'MERGE_CONTENT'  it = lt_fields CHANGING cv = filedata ).

    DATA: new_file_content TYPE xstring.
    new_file_content = zcl_abapgit_convert=>string_to_xstring_utf8( iv_string = filedata-merge_content ).

    ms_merge-stage->add( iv_path     = ms_diff_file-path
                         iv_filename = ms_diff_file-filename
                         iv_data     = new_file_content ).

    FIELD-SYMBOLS: <result> TYPE zif_abapgit_definitions=>ty_expanded.
    READ TABLE ms_merge-result ASSIGNING <result> INDEX mv_current_conflict_index.
    <result>-sha1 = zcl_abapgit_hash=>sha1( iv_type = 'MERGE' iv_data = new_file_content ).

  ENDMETHOD.


  METHOD build_menu.

    CREATE OBJECT ro_menu.
    ro_menu->add( iv_txt = 'Apply' iv_act = 'submitFormById(''merge_form'');' iv_typ = zif_abapgit_definitions=>gc_action_type-onclick
                     iv_opt = zif_abapgit_definitions=>gc_html_opt-strong ) ##NO_TEXT.
    ro_menu->add( iv_txt = 'Cancel' iv_act = c_actions-cancel ) ##NO_TEXT.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    mo_repo = io_repo.
    ms_control-page_title = 'Resolve Conflicts'.
    ms_control-page_menu  = build_menu( ).

    mo_merge_page = io_merge_page.
    ms_merge = is_merge.

    READ TABLE ms_merge-result TRANSPORTING NO FIELDS WITH KEY sha1 = space.
    mv_current_conflict_index = sy-tabix.

    resolve_diff( ).

  ENDMETHOD.


  METHOD find_next_conflict.

    FIELD-SYMBOLS: <result> TYPE zif_abapgit_definitions=>ty_expanded.
    LOOP AT ms_merge-result ASSIGNING <result> FROM mv_current_conflict_index WHERE sha1 IS INITIAL.
      IF sy-tabix NE mv_current_conflict_index
      AND <result>-sha1 IS INITIAL.
        mv_current_conflict_index = sy-tabix.
        EXIT.
      ELSE.
        CLEAR mv_current_conflict_index.
      ENDIF.
    ENDLOOP.
    IF sy-subrc NE 0.
      CLEAR mv_current_conflict_index.
    ENDIF.

  ENDMETHOD.


  METHOD get_file_content.

    DATA: files TYPE zif_abapgit_definitions=>ty_files_tt.
    FIELD-SYMBOLS: <file> TYPE zif_abapgit_definitions=>ty_file.

    ms_merge-repo->set_branch_name( iv_branch ).
    files = ms_merge-repo->get_files_remote( ).
    READ TABLE files ASSIGNING <file> WITH KEY path = iv_path filename = iv_filename.
    CHECK <file> IS ASSIGNED.

    rv_file_content = zcl_abapgit_convert=>xstring_to_string_utf8( <file>-data ).

  ENDMETHOD.


  METHOD IS_BINARY.

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

  ENDMETHOD.  " is_binary.


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
    ro_html->add( |<th colspan="3">@@ { is_diff_line-new_num } @@ { lv_beacon }</th>| ).

    ro_html->add( '</tr>' ).
    ro_html->add( '</thead>' ).

  ENDMETHOD.  " render_beacon.


  METHOD render_content.

    CREATE OBJECT ro_html.
    ro_html->add( |<div id="diff-list" data-repo-key="{ mo_repo->get_key( ) }">| ).
    ro_html->add( render_diff( ms_diff_file ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_content


  METHOD render_diff.

    CREATE OBJECT ro_html.

    ro_html->add( |<div class="diff" data-type="{ is_diff-type
      }" data-changed-by="{ is_diff-changed_by
      }" data-file="{ is_diff-path && is_diff-filename }">| ). "#EC NOTEXT
    ro_html->add( render_diff_head( is_diff ) ).

    " Content
    IF is_diff-type <> 'binary'.
      "Table for Div-Table and textarea
      ro_html->add( '<div class="diff_content">' ).         "#EC NOTEXT
      ro_html->add( '<table>' ).                            "#EC NOTEXT
      ro_html->add( '<thead class="header">' ).             "#EC NOTEXT
      ro_html->add( '<tr>' ).                               "#EC NOTEXT
      ro_html->add( '<th>Code</th>' ).                      "#EC NOTEXT
      ro_html->add( '<th>Merge</th>' ).                     "#EC NOTEXT
      ro_html->add( '</tr>' ).                              "#EC NOTEXT
      ro_html->add( '</thead>' ).                           "#EC NOTEXT
      ro_html->add( '<td>' ).

      "Diff-Table of source and target file
      ro_html->add( '<table class="diff_tab syntax-hl">' ). "#EC NOTEXT
      ro_html->add( render_table_head( ) ).
      ro_html->add( render_lines( is_diff ) ).
      ro_html->add( '</table>' ).                           "#EC NOTEXT

      DATA: target_content TYPE string.
      target_content = get_file_content( iv_branch = ms_merge-target-name iv_path = is_diff-path iv_filename = is_diff-filename ).

      ro_html->add( '</td>' ).                              "#EC NOTEXT
      ro_html->add( '<td>' ).                               "#EC NOTEXT
      ro_html->add( '<div class="form-container">' ).
      ro_html->add( '<form id="merge_form" class="aligned-form" accept-charset="UTF-8" method="post" action="sapevent:apply">' ).
      ro_html->add( |<textarea id="merge_content" name="merge_content" style="width:100%; height:100%;" rows="{ lines( is_diff-o_diff->get( ) ) }">{ target_content }</textarea>| ).
      ro_html->add( '<input type="submit" class="hidden-submit">' ).
      ro_html->add( '</from>' ).                            "#EC NOTEXT
      ro_html->add( '</div>' ).                             "#EC NOTEXT
      ro_html->add( '</td>' ).                              "#EC NOTEXT
      ro_html->add( '</table>' ).                           "#EC NOTEXT
      ro_html->add( '</div>' ).                             "#EC NOTEXT
    ELSE.
      ro_html->add( '<div class="diff_content paddings center grey">' ). "#EC NOTEXT
      ro_html->add( 'The content seems to be binary.' ).    "#EC NOTEXT
      ro_html->add( 'Cannot display as diff.' ).            "#EC NOTEXT
      ro_html->add( '</div>' ).                             "#EC NOTEXT
    ENDIF.

    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.  " render_diff


  METHOD render_diff_head.

    DATA: ls_stats TYPE zif_abapgit_definitions=>ty_count.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="diff_head">' ).              "#EC NOTEXT

    IF is_diff-type <> 'binary'.
      ls_stats = is_diff-o_diff->stats( ).
      ls_stats-update = ls_stats-update + ls_stats-insert + ls_stats-delete.
      CLEAR: ls_stats-insert, ls_stats-delete.

      ro_html->add( |<span class="diff_banner diff_ins">+ { ls_stats-insert }</span>| ).
      ro_html->add( |<span class="diff_banner diff_del">- { ls_stats-delete }</span>| ).
      ro_html->add( |<span class="diff_banner diff_upd">~ { ls_stats-update }</span>| ).
    ENDIF.

    ro_html->add( |<span class="diff_name">{ is_diff-filename }</span>| ). "#EC NOTEXT
    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_item_state( iv1 = is_diff-lstate iv2 = is_diff-rstate ) ).
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

      ro_html->add( render_line_split( is_diff_line = <ls_diff>
                                       iv_fstate    = is_diff-fstate ) ).

    ENDLOOP.

  ENDMETHOD.  "render_lines


  METHOD render_line_split.

    DATA: lv_new   TYPE string,
          lv_old   TYPE string,
          lv_merge TYPE string,
          lv_mark  TYPE string,
          lv_bg    TYPE string.

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
      ro_html->add( lv_new ). " Target
      ro_html->add( lv_old ). " Source
    ro_html->add( '</tr>' ).                                "#EC NOTEXT

  ENDMETHOD. "render_line_split


  METHOD render_table_head.

    CREATE OBJECT ro_html.


      ro_html->add( '<thead class="header">' ).             "#EC NOTEXT
      ro_html->add( '<tr>' ).                               "#EC NOTEXT
      ro_html->add( '<th class="num"></th>' ).              "#EC NOTEXT
      ro_html->add( '<th>Source</th>' ).                     "#EC NOTEXT
      ro_html->add( '<th class="num"></th>' ).              "#EC NOTEXT
      ro_html->add( '<th>Target</th>' ).                    "#EC NOTEXT
      ro_html->add( '</tr>' ).                              "#EC NOTEXT
      ro_html->add( '</thead>' ).                           "#EC NOTEXT

  ENDMETHOD.  " render_table_head.


  METHOD resolve_diff.

    DATA: lv_offs TYPE i.

    FIELD-SYMBOLS: <result> TYPE zif_abapgit_definitions=>ty_expanded,
                   <source> TYPE zif_abapgit_definitions=>ty_expanded,
                   <target> TYPE zif_abapgit_definitions=>ty_expanded.

    "Find current conflict
    READ TABLE ms_merge-result ASSIGNING <result> INDEX mv_current_conflict_index.
    CHECK sy-subrc EQ 0.

    "Find corresponding files in source and target branch
    READ TABLE ms_merge-stree ASSIGNING <source> WITH KEY path = <result>-path.
    READ TABLE ms_merge-ttree ASSIGNING <target> WITH KEY path = <result>-path.
    CHECK <source> IS ASSIGNED AND <target> IS ASSIGNED.

    DATA: source_files TYPE zif_abapgit_definitions=>ty_files_tt,
          target_files TYPE zif_abapgit_definitions=>ty_files_tt.
    FIELD-SYMBOLS: <source_file> TYPE zif_abapgit_definitions=>ty_file,
                   <target_file> TYPE zif_abapgit_definitions=>ty_file.

    ms_merge-repo->set_branch_name( ms_merge-source-name ).
    source_files = ms_merge-repo->get_files_remote( ).
    ms_merge-repo->set_branch_name( ms_merge-target-name ).
    target_files = ms_merge-repo->get_files_remote( ).

    "Read files to get data xstring so diff view can be used
    READ TABLE source_files ASSIGNING <source_file> WITH KEY path = <result>-path filename = <result>-name.
    READ TABLE target_files ASSIGNING <target_file> WITH KEY path = <result>-path filename = <result>-name.
    CHECK <source_file> IS ASSIGNED AND <target_file> IS ASSIGNED.

    ms_diff_file-path     = <result>-path.
    ms_diff_file-filename = <result>-name.

    ms_diff_file-type = reverse( <target_file>-filename ).

    FIND FIRST OCCURRENCE OF '.' IN ms_diff_file-type MATCH OFFSET lv_offs.
    ms_diff_file-type = reverse( substring( val = ms_diff_file-type len = lv_offs ) ).
    IF ms_diff_file-type <> 'xml' AND ms_diff_file-type <> 'abap'.
      ms_diff_file-type = 'other'.
    ENDIF.

    IF ms_diff_file-type = 'other'
    AND is_binary( iv_d1 = <target_file>-data iv_d2 = <source_file>-data ) = abap_true.
      ms_diff_file-type = 'binary'.
    ENDIF.

    IF ms_diff_file-type <> 'binary'.
      CREATE OBJECT ms_diff_file-o_diff
        EXPORTING
          iv_new = <source_file>-data
          iv_old = <target_file>-data.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page~on_event.

    CASE iv_action.
      WHEN c_actions-apply OR c_actions-cancel.
        IF iv_action EQ c_actions-apply.
          apply_merged_content( it_postdata = it_postdata ).
        ENDIF.

        find_next_conflict( ).

        IF mv_current_conflict_index IS NOT INITIAL.
          resolve_diff( ).
          ev_state = zif_abapgit_definitions=>gc_event_state-re_render.
        ELSE.

          FIELD-SYMBOLS: <result> TYPE zif_abapgit_definitions=>ty_expanded.
          READ TABLE ms_merge-result ASSIGNING <result> WITH KEY sha1 = space.
          IF sy-subrc NE 0.
            CLEAR ms_merge-conflict.
          ELSE.
            ms_merge-conflict = |{ <result>-path } merge conflict, changed in source and target branch|.
          ENDIF.

          mo_merge_page->set_merge( is_merge = ms_merge ).
          ei_page = mo_merge_page.
          ev_state = zif_abapgit_definitions=>gc_event_state-go_back.
        ENDIF.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
