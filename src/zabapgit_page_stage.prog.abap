*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_STAGE
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_stage DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF c_action,
                 stage_all    TYPE string VALUE 'stage_all',
                 stage_commit TYPE string VALUE 'stage_commit',
               END OF c_action.

    METHODS:
      constructor
        IMPORTING io_repo TYPE REF TO lcl_repo_online
        RAISING   lcx_exception,
      lif_gui_page~render REDEFINITION,
      lif_gui_page~on_event REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo         TYPE REF TO lcl_repo_online,
          ms_files        TYPE ty_stage_files,
          mt_staged_files TYPE ty_files_item_tt,
          mo_stage        TYPE REF TO lcl_stage,
          mv_ts           TYPE timestamp.

    METHODS:
      render_list
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper,
      render_file
        IMPORTING is_file        TYPE ty_file
                  iv_context     TYPE string
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper,
      render_menu
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper,
      styles
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper,
      scripts
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS process_stage_list
      IMPORTING it_postdata TYPE cnht_post_data_tab
      RAISING   lcx_exception.

    METHODS validate_objects
      RAISING
        lcx_cancel
        lcx_exception.

ENDCLASS.

CLASS lcl_gui_page_stage IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
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

    CLEAR: mt_staged_files.

    CASE iv_action.
      WHEN c_action-stage_all.
        mo_stage->reset_all( ).
        LOOP AT ms_files-local ASSIGNING <ls_file>.
          mo_stage->add( iv_path     = <ls_file>-file-path
                         iv_filename = <ls_file>-file-filename
                         iv_data     = <ls_file>-file-data ).
        ENDLOOP.
        mt_staged_files = ms_files-local.
      WHEN c_action-stage_commit.
        mo_stage->reset_all( ).
        process_stage_list( it_postdata ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    validate_objects( ).

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
          APPEND <ls_file> TO mt_staged_files.
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
        ro_html->add('<th></th><th colspan="2">LOCAL</th><th>' ).
        IF lines( ms_files-local ) > 1.
          ro_html->add_anchor( iv_txt = |{ lines( ms_files-local ) } diffs|
                               iv_act = |{ gc_action-go_diff }?key={ mo_repo->get_key( ) }| ).
        ENDIF.
        ro_html->add('</th></tr></thead>').
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

    DATA lv_param TYPE string.

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
        ro_html->add_anchor( iv_txt = 'diff' iv_act = |{ gc_action-go_diff }?{ lv_param }| ).
        ro_html->add( '</td>' ).
      WHEN 'remote'.
        ro_html->add( '<td class="cmd"><a>ignore</a><a>remove</a></td>' ).
        ro_html->add( |<td><span class="grey">-</span></td>| ).
    ENDCASE.

    ro_html->add( '</tr>' ).

  ENDMETHOD.  "render_file

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'STAGE' ) ).

    ro_html->add( '<div class="repo">' ).
    ro_html->add( render_repo_top( mo_repo ) ).
    ro_html->add( render_menu( ) ).
    ro_html->add( render_list( ) ).
    ro_html->add( '</div>' ).

    ro_html->add( footer( scripts( ) ) ).

  ENDMETHOD.      "lif_gui_page~render

  METHOD render_menu.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="paddings">' ).
    ro_html->add_anchor( iv_act   = |commit('{ c_action-stage_commit }');|
                         iv_typ   = gc_action_type-onclick
                         iv_id    = 'act_commit'
                         iv_style = 'display: none'
                         iv_txt   = 'Commit'
                         iv_opt   = gc_html_opt-emphas ) ##NO_TEXT.
    ro_html->add_anchor( iv_act   = |{ c_action-stage_all }|
                         iv_id    = 'act_commit_all'
                         iv_txt   = 'Add all and commit') ##NO_TEXT.
    ro_html->add( '</div>' ).

  ENDMETHOD.      "render_menu

  METHOD styles.

    CREATE OBJECT ro_html.

    _add '/* STAGE */'.
    _add '.stage_tab {'.
    _add '  border: 1px solid #DDD;'.
    _add '  background: #fff;'.
    _add '  margin-top: 0.2em;'.
    _add '}'.
    _add '.stage_tab td {'.
    _add '  border-top: 1px solid #eee;'.
    _add '  color: #333;'.
    _add '  vertical-align: middle;'.
    _add '  padding: 2px 0.5em;'.
    _add '}'.
    _add '.stage_tab th {'.
    _add '  color: #BBB;'.
    _add '  font-size: 10pt;'.
    _add '  text-align: left;'.
    _add '  font-weight: normal;'.
    _add '  background-color: #edf2f9;'.
    _add '  padding: 4px 0.5em;'.
    _add '}'.
    _add '.stage_tab td.status {'.
    _add '  width: 2em;'.
    _add '  text-align: center;'.
    _add '}'.
    _add '.stage_tab tbody tr:first-child td { padding-top: 0.5em; }'.
    _add '.stage_tab tbody tr:last-child td { padding-bottom: 0.5em; }'.
    _add '.stage_tab td.cmd a { padding: 0px 4px; }'.

  ENDMETHOD.    "styles

  METHOD scripts.

    CREATE OBJECT ro_html.

    " Globals & initialization
    ro_html->add( |var gPageID = "stage{ mv_ts }";| ).
    _add 'var gChoiceCount = 0;'.
    _add 'setHook();'.

    " Hook global click listener on table, global action counter
    _add 'function setHook() {'.
    _add '  var stageTab = document.getElementById("stage_tab");'.
    _add '  if (stageTab.addEventListener) {'.
    _add '    stageTab.addEventListener("click", onEvent);'.
    _add '  } else {'.
    _add '    stageTab.attachEvent("onclick", onEvent);'. " <IE9 crutch
    _add '  }'.
    _add '  window.onbeforeunload = onPageUnload;'.
    _add '  window.onload         = onPageLoad;'.
    _add '}'.

    " Store table state on leaving the page
    _add 'function onPageUnload() {'.
    _add '  var data = collectData();'.
    _add '  window.sessionStorage.setItem(gPageID, JSON.stringify(data));'.
    _add '}'.

    " Re-store table state on entering the page
    _add 'function onPageLoad() {'.
    _add '  var data = JSON.parse(window.sessionStorage.getItem(gPageID));'.
    _add '  var stage = document.getElementById("stage_tab");'.
    _add '  for (var i = stage.rows.length - 1; i >= 0; i--) {'.
    _add '    var tr = stage.rows[i];'.
    _add '    if (tr.parentNode.tagName == "THEAD") continue;'.
    _add '    var context = tr.parentNode.className;'.
    _add '    var cmd     = data[tr.cells[1].innerText];'.
    _add '    if (!cmd) continue;'.
    _add '    formatTR(tr, cmd, context);'.
    _add '    if (countChoiceImpact(cmd) > 0) gChoiceCount++;'.
    _add '  }'.
    _add '  updateMenu();'.
    _add '}'.

    " Event handler, change status
    _add 'function onEvent(event) {'.
    _add '  if (!event.target) {'. " <IE9 crutch
    _add '    if (event.srcElement) event.target = event.srcElement;'.
    _add '    else return;'.
    _add '  }'.
    _add '  if (event.target.tagName != "A") return;'.
    _add '  var td = event.target.parentNode;'.
    _add '  if (!td || td.tagName != "TD" || td.className != "cmd") return;'.
    _add '  var cmd     = event.target.innerText;'.
    _add '  var tr      = td.parentNode;'.
    _add '  var context = tr.parentNode.className;'.
    _add '  switch (cmd) {'.
    _add '    case "add":    cmd = "A"; break;'.
    _add '    case "remove": cmd = "R"; break;'.
    _add '    case "ignore": cmd = "I"; break;'.
    _add '    case "reset":  cmd = "?"; break;'.
    _add '  }'.
    _add '  formatTR(tr, cmd, context);'.
    _add '  gChoiceCount += countChoiceImpact(cmd);'.
    _add '  updateMenu();'.
    _add '}'.

    " Update action counter -> affects menu update after
    _add 'function countChoiceImpact(cmd) {'.
    _add '  if      ("ARI".indexOf(cmd) > -1) return 1;'.
    _add '  else if ("?".indexOf(cmd) > -1)   return -1;'.
    _add '  else alert("Unknown command");'.
    _add '}'.

    " Re-format table line
    _add 'function formatTR(tr, cmd, context) {'.
    _add '  var cmdReset  = "<a>reset</a>"; '.
    _add '  var cmdLocal  = "<a>add</a>"; '.
    _add '  var cmdRemote = "<a>ignore</a><a>remove</a>";'.
    _add '  tr.cells[0].innerText   = cmd;'.
    _add '  tr.cells[0].style.color = (cmd == "?")?"#CCC":"";'.
    _add '  tr.cells[2].innerHTML   = (cmd != "?")?cmdReset'.
    _add '    :(context == "local")?cmdLocal:cmdRemote;'.
    _add '}'.

    " Update menu items visibility
    _add 'function updateMenu() {'.
    _add '  if (gChoiceCount > 0) {'.
    _add '    document.getElementById("act_commit").style.display     = "inline";'.
    _add '    document.getElementById("act_commit_all").style.display = "none";'.
    _add '  } else {'.
    _add '    document.getElementById("act_commit").style.display     = "none";'.
    _add '    document.getElementById("act_commit_all").style.display = "inline";'.
    _add '  }'.
    _add '}'.

    " Commit change to the server
    _add 'function commit(action) {'.
    _add '  var data = collectData();'.
    _add '  submitForm(data, action);'.
    _add '}'.

    " Extract data from the table
    _add 'function collectData() {'.
    _add '  var stage = document.getElementById("stage_tab");'.
    _add '  var data = {};'.
    _add '  for (var i = stage.rows.length - 1; i >= 0; i--) {'.
    _add '    var row = stage.rows[i];'.
    _add '    if (row.parentNode.tagName == "THEAD") continue;'.
    _add '    data[row.cells[1].innerText] = row.cells[0].innerText;'.
    _add '  }'.
    _add '  return data;      '.
    _add '}'.

  ENDMETHOD.  "scripts


  METHOD validate_objects.

    DATA: lt_remote_files            TYPE ty_files_tt,
          ls_remote_file             LIKE LINE OF lt_remote_files,
          lo_object                  TYPE REF TO lif_object,
          ls_staged_file             LIKE LINE OF mt_staged_files,
          lo_previous_remote_version TYPE REF TO lcl_xml_input,
          lo_comparison_result       TYPE REF TO lif_object_comparison_result.

    lt_remote_files = mo_repo->get_files_remote( ).
    LOOP AT mt_staged_files INTO ls_staged_file WHERE file-filename NS '.abap'.
      lcl_objects=>read_object(
        EXPORTING
          is_item     = ls_staged_file-item
          iv_language = mo_repo->get_master_language( )
        RECEIVING
          ri_obj      = lo_object ).

      READ TABLE lt_remote_files
        WITH KEY filename = ls_staged_file-file-filename
        INTO ls_remote_file.
      "if file does not exist in remote, we don't need to validate
      IF sy-subrc = 0.
        CREATE OBJECT lo_previous_remote_version
          EXPORTING
            iv_xml = lcl_convert=>xstring_to_string_utf8( ls_remote_file-data ).
        lo_comparison_result = lo_object->compare_to_previous_version( lo_previous_remote_version ).
        lo_comparison_result->show_confirmation_dialog( ).

        IF lo_comparison_result->is_result_complete_halt( ) = abap_true.
          RAISE EXCEPTION TYPE lcx_cancel.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.