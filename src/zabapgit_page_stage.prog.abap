*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_STAGE
*&---------------------------------------------------------------------*

CLASS lcl_gui_page_stage DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_repo TYPE REF TO lcl_repo_online
        RAISING   lcx_exception,
      lif_gui_page~render REDEFINITION,
      lif_gui_page~on_event REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo  TYPE REF TO lcl_repo_online,
          ms_files TYPE ty_stage_files,
          mo_stage TYPE REF TO lcl_stage.

    METHODS:
      render_list
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper,
      render_menu
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper,
      styles
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS stage_handle_action
      IMPORTING iv_getdata TYPE clike
                iv_action  TYPE clike
      RAISING   lcx_exception.

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

  ENDMETHOD.

  METHOD stage_handle_action.

    DATA: ls_file TYPE ty_file.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF ms_files-local.


    IF iv_action <> 'stage_all'.
      lcl_html_action_utils=>file_decode( EXPORTING iv_string = iv_getdata
                                          IMPORTING eg_file   = ls_file ).
    ENDIF.

    CASE iv_action.
      WHEN 'stage_add'.
        READ TABLE ms_files-local ASSIGNING <ls_file>
          WITH KEY file-path = ls_file-path
          file-filename = ls_file-filename.
        ASSERT sy-subrc = 0.
        mo_stage->add( iv_path     = <ls_file>-file-path
                       iv_filename = <ls_file>-file-filename
                       iv_data     = <ls_file>-file-data ).
      WHEN 'stage_all'.
        LOOP AT ms_files-local ASSIGNING <ls_file>.
          mo_stage->add( iv_path     = <ls_file>-file-path
                         iv_filename = <ls_file>-file-filename
                         iv_data     = <ls_file>-file-data ).
        ENDLOOP.
      WHEN 'stage_reset'.
        mo_stage->reset( iv_path     = ls_file-path
                         iv_filename = ls_file-filename ).
      WHEN 'stage_ignore'.
        mo_stage->ignore( iv_path     = ls_file-path
                          iv_filename = ls_file-filename ).
      WHEN 'stage_rm'.
        mo_stage->rm( iv_path     = ls_file-path
                      iv_filename = ls_file-filename ).
    ENDCASE.

  ENDMETHOD.        "stage_handle_action

  METHOD render_list.

    DATA: lv_method  TYPE lcl_stage=>ty_method,
          lv_param   TYPE string,
          lv_status  TYPE string,
          lo_toolbar TYPE REF TO lcl_html_toolbar.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF ms_files-remote,
                   <ls_local>  LIKE LINE OF ms_files-local.


    CREATE OBJECT ro_html.

    ro_html->add( '<table class="stage_tab">' ).

    LOOP AT ms_files-local ASSIGNING <ls_local>.
      IF sy-tabix = 1.
        ro_html->add('<tr class="separator firstrow">').
        ro_html->add( '<td></td><td colspan="2">LOCAL</td>' ).
        ro_html->add('</tr>').
      ENDIF.

      lv_method = mo_stage->lookup( iv_path     = <ls_local>-file-path
                                    iv_filename = <ls_local>-file-filename ).
      lv_param  = lcl_html_action_utils=>file_encode( iv_key  = mo_repo->get_key( )
                                                      ig_file = <ls_local>-file ).

      CREATE OBJECT lo_toolbar.
      IF lv_method IS NOT INITIAL.
        lo_toolbar->add( iv_txt = 'reset'
          iv_act = 'stage_reset?' && lv_param ) ##NO_TEXT.
      ELSE.
        lo_toolbar->add( iv_txt = 'add'
          iv_act = 'stage_add?' && lv_param ) ##NO_TEXT.
      ENDIF.
      lo_toolbar->add( iv_txt = 'diff'
        iv_act = 'diff?' && lv_param ) ##NO_TEXT.

      IF lv_method IS INITIAL.
        lv_status = '<span class="grey">?</span>'.
      ELSE.
        lv_status = lv_method.
      ENDIF.
      ro_html->add( '<tr>' ).
      ro_html->add( |<td class="status">{ lv_status }</td>| ).
      ro_html->add( |<td>{ <ls_local>-file-path && <ls_local>-file-filename }</td>| ).
      ro_html->add( '<td>' ).
      ro_html->add( lo_toolbar->render( iv_no_separator = abap_true ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.

    LOOP AT ms_files-remote ASSIGNING <ls_remote>.
      IF sy-tabix = 1.
        ro_html->add('<tr class="separator">').
        ro_html->add( '<td></td><td colspan="2">REMOTE</td>' ).
        ro_html->add('</tr>').
      ENDIF.

      lv_method = mo_stage->lookup( iv_path     = <ls_remote>-path
                                    iv_filename = <ls_remote>-filename ).
      lv_param  = lcl_html_action_utils=>file_encode( iv_key  = mo_repo->get_key( )
                                                      ig_file = <ls_remote> ).

      CREATE OBJECT lo_toolbar.
      IF lv_method IS NOT INITIAL.
        lo_toolbar->add( iv_txt = 'reset'  iv_act = 'stage_reset?' && lv_param ) ##NO_TEXT.
      ELSE.
        lo_toolbar->add( iv_txt = 'ignore' iv_act = 'stage_ignore?' && lv_param ) ##NO_TEXT.
        lo_toolbar->add( iv_txt = 'remove' iv_act = 'stage_rm?' && lv_param ) ##NO_TEXT.
      ENDIF.

      IF lv_method IS INITIAL.
        lv_status = '<span class="grey">?</span>'.
      ELSE.
        lv_status = lv_method.
      ENDIF.
      ro_html->add( '<tr>' ).
      ro_html->add( |<td class="status">{ lv_status }</td>| ).
      ro_html->add( |<td>{ <ls_remote>-path && <ls_remote>-filename }</td>| ).
      ro_html->add( '<td>' ).
      ro_html->add( lo_toolbar->render( iv_no_separator = abap_true ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.

    ro_html->add( '</table>' ).

  ENDMETHOD.      "render_lines

  METHOD lif_gui_page~on_event.

    CASE iv_action.
      WHEN 'stage_all'
          OR 'stage_commit'.
        IF iv_action = 'stage_all'.
          stage_handle_action( iv_getdata = iv_getdata iv_action = iv_action ).
        ENDIF.
        CREATE OBJECT ei_page TYPE lcl_gui_page_commit
          EXPORTING
            io_repo  = mo_repo
            io_stage = mo_stage.
        ev_state = gc_event_state-new_page.
      WHEN 'stage_add'
          OR 'stage_reset'
          OR 'stage_ignore'
          OR 'stage_rm'.
        stage_handle_action( iv_getdata = iv_getdata iv_action = iv_action ).
        ev_state = gc_event_state-re_render.
    ENDCASE.

  ENDMETHOD.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'STAGE' ) ).

    ro_html->add( '<div class="repo">' ).
    ro_html->add( render_repo_top( mo_repo ) ).
    ro_html->add( render_menu( ) ).

    ro_html->add( render_list( ) ).

    ro_html->add( '</div>' ).
    ro_html->add( footer( ) ).

  ENDMETHOD.      "lif_gui_page~render

  METHOD render_menu.

    DATA: lo_toolbar TYPE REF TO lcl_html_toolbar,
          lv_action  TYPE string.


    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    lv_action = lcl_html_action_utils=>repo_key_encode( mo_repo->get_key( ) ).

    IF mo_stage->count( ) > 0.
      lo_toolbar->add( iv_act = |stage_commit?{ lv_action }|
                       iv_txt = 'Commit'
                       iv_opt = gc_html_opt-emphas ) ##NO_TEXT.
    ELSEIF lines( ms_files-local ) > 0.
      lo_toolbar->add( iv_act = |stage_all?{ lv_action }|
                       iv_txt = 'Add all and commit') ##NO_TEXT.
    ENDIF.

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( lo_toolbar->render( ) ).
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
    _add '.stage_tab td.status {'.
    _add '  width: 2em;'.
    _add '  text-align: center;'.
    _add '}'.
    _add '.stage_tab tr.separator td {'.
    _add '  color: #BBB;'.
    _add '  font-size: 10pt;'.
    _add '  background-color: #edf2f9;'.
    _add '  padding: 4px 0.5em;'.
    _add '}'.
    _add '.stage_tab tr.firstrow td { border-top: 0px; } '.

  ENDMETHOD.    "styles

ENDCLASS.