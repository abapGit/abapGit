*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_GUI
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_gui_router DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_gui_router DEFINITION FINAL.
  PUBLIC SECTION.

    METHODS on_event
      IMPORTING iv_action   TYPE clike
                iv_getdata  TYPE clike OPTIONAL
                it_postdata TYPE cnht_post_data_tab OPTIONAL
      EXPORTING ei_page     TYPE REF TO lif_gui_page
                ev_state    TYPE i
      RAISING   lcx_exception.

  PRIVATE SECTION.

    METHODS get_page_by_name
      IMPORTING iv_name        TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS get_page_diff
      IMPORTING iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS get_page_branch_overview
      IMPORTING iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS get_page_stage
      IMPORTING iv_key         TYPE lcl_persistence_repo=>ty_repo-key
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS get_page_db_by_name
      IMPORTING iv_name        TYPE clike
                iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS abapgit_installation
      RAISING lcx_exception.

    METHODS repo_purge
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS repo_remove
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS repo_pull
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS reset
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS create_branch
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS db_delete
      IMPORTING iv_getdata TYPE clike
      RAISING   lcx_exception.

    METHODS db_save
      IMPORTING it_postdata TYPE cnht_post_data_tab
      RAISING   lcx_exception.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_gui DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_gui DEFINITION FINAL CREATE PRIVATE FRIENDS lcl_app.

  PUBLIC SECTION.

    METHODS go_home
      RAISING lcx_exception.

    METHODS back
      IMPORTING iv_to_bookmark TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(rv_exit) TYPE xfeld
      RAISING   lcx_exception.

    METHODS on_event FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING action frame getdata postdata query_table.  "#EC NEEDED

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_page_stack,
             page     TYPE REF TO lif_gui_page,
             bookmark TYPE abap_bool,
           END OF ty_page_stack.

    DATA: mi_cur_page    TYPE REF TO lif_gui_page,
          mt_stack       TYPE TABLE OF ty_page_stack,
          mt_assets      TYPE tt_w3urls,
          mo_router      TYPE REF TO lcl_gui_router,
          mo_html_viewer TYPE REF TO cl_gui_html_viewer.

    METHODS constructor
      RAISING lcx_exception.

    METHODS startup
      RAISING lcx_exception.

    METHODS cache_image
      IMPORTING iv_url    TYPE w3url
                iv_base64 TYPE string.

    METHODS cache_html
      IMPORTING iv_html       TYPE string
      RETURNING VALUE(rv_url) TYPE w3url.

    METHODS render
      RAISING lcx_exception.

    METHODS call_page
      IMPORTING ii_page          TYPE REF TO lif_gui_page
                iv_with_bookmark TYPE abap_bool DEFAULT abap_false
      RAISING   lcx_exception.

ENDCLASS.                    "lcl_gui DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_gui IMPLEMENTATION.

  METHOD constructor.

    startup( ).

  ENDMETHOD.            "constructor

  METHOD on_event.

    DATA: lx_exception TYPE REF TO lcx_exception,
          li_page      TYPE REF TO lif_gui_page,
          lv_state     TYPE i.

    TRY.
        IF mi_cur_page IS BOUND.
          mi_cur_page->on_event(
            EXPORTING
              iv_action   = action
              iv_getdata  = getdata
              it_postdata = postdata
            IMPORTING
              ei_page     = li_page
              ev_state    = lv_state ).
        ENDIF.

        IF lv_state IS INITIAL.
          mo_router->on_event(
            EXPORTING
              iv_action   = action
              iv_getdata  = getdata
              it_postdata = postdata
            IMPORTING
              ei_page     = li_page
              ev_state    = lv_state ).
        ENDIF.

        CASE lv_state.
          WHEN gc_event_state-re_render.
            render( ).
          WHEN gc_event_state-new_page.
            call_page( li_page ).
          WHEN gc_event_state-new_page_w_bookmark.
            call_page( ii_page = li_page iv_with_bookmark = abap_true ).
          WHEN gc_event_state-go_back.
            back( ).
          WHEN gc_event_state-go_back_to_bookmark.
            back( iv_to_bookmark = abap_true ).
          WHEN gc_event_state-no_more_act.
            " Do nothing, handling completed
          WHEN OTHERS.
            lcx_exception=>raise( 'Unknown action' ).
        ENDCASE.

      CATCH lcx_exception INTO lx_exception.
        ROLLBACK WORK.
        MESSAGE lx_exception->mv_text TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.                    "on_event

  METHOD back.

    DATA: lv_index TYPE i,
          ls_stack LIKE LINE OF mt_stack.

    lv_index = lines( mt_stack ).

    IF lv_index = 0.
      rv_exit = abap_true.
      RETURN.
    ENDIF.

    DO lv_index TIMES.
      READ TABLE mt_stack INDEX lv_index INTO ls_stack.
      ASSERT sy-subrc = 0.

      DELETE mt_stack INDEX lv_index.
      ASSERT sy-subrc = 0.

      lv_index = lv_index - 1.

      IF iv_to_bookmark = abap_false OR ls_stack-bookmark = abap_true.
        EXIT.
      ENDIF.
    ENDDO.

    mi_cur_page = ls_stack-page. " last page always stays
    render( ).

  ENDMETHOD.                "back

  METHOD call_page.

    DATA: lt_assets TYPE tt_web_assets,
          ls_stack  TYPE ty_page_stack.
    FIELD-SYMBOLS <ls_asset> LIKE LINE OF lt_assets.

    IF NOT mi_cur_page IS INITIAL.
      ls_stack-page     = mi_cur_page.
      ls_stack-bookmark = iv_with_bookmark.
      APPEND ls_stack TO mt_stack.
    ENDIF.

    lt_assets = ii_page->get_assets( ).
    IF lines( lt_assets ) > 0.
      LOOP AT lt_assets ASSIGNING <ls_asset>.
        READ TABLE mt_assets TRANSPORTING NO FIELDS WITH KEY table_line = <ls_asset>-url.
        CHECK sy-subrc IS NOT INITIAL.
        APPEND <ls_asset>-url TO mt_assets.
        cache_image( iv_url = <ls_asset>-url iv_base64 = <ls_asset>-content ).
      ENDLOOP.
    ENDIF.

    mi_cur_page = ii_page.
    render( ).

  ENDMETHOD.                "call_page

  METHOD go_home.

    on_event( action = 'main' ) ##NO_TEXT.

  ENDMETHOD.                "go_home

  METHOD startup.

    DATA: lt_events TYPE cntl_simple_events,
          ls_event  LIKE LINE OF lt_events.

    CREATE OBJECT mo_router.
    CREATE OBJECT mo_html_viewer
      EXPORTING
        query_table_disabled = abap_true
        parent               = cl_gui_container=>screen0.

    CLEAR ls_event.
    ls_event-eventid = mo_html_viewer->m_id_sapevent.
    ls_event-appl_event = abap_true.
    APPEND ls_event TO lt_events.
    mo_html_viewer->set_registered_events( lt_events ).

    SET HANDLER me->on_event FOR mo_html_viewer.

  ENDMETHOD.                    "startup

  METHOD render.

    DATA lv_url TYPE w3url.

    lv_url = cache_html( mi_cur_page->render( )->mv_html ).

    mo_html_viewer->show_url( lv_url ).

  ENDMETHOD.                    "render

  METHOD cache_html.

    DATA: lt_data TYPE TABLE OF text200.

    CALL FUNCTION 'SCMS_STRING_TO_FTEXT'
      EXPORTING
        text      = iv_html
      TABLES
        ftext_tab = lt_data.

    mo_html_viewer->load_data(
      IMPORTING
        assigned_url = rv_url
      CHANGING
        data_table   = lt_data ).

  ENDMETHOD.                    "cache_html

  METHOD cache_image.

    DATA lv_xtmp  TYPE xstring.
    DATA lv_size  TYPE int4.
    DATA lt_xdata TYPE TABLE OF w3_mime. " RAW255

    CALL FUNCTION 'SSFC_BASE64_DECODE'
      EXPORTING
        b64data = iv_base64
      IMPORTING
        bindata = lv_xtmp
      EXCEPTIONS
        OTHERS  = 1.

    ASSERT sy-subrc = 0. " Image data error

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xtmp
      IMPORTING
        output_length = lv_size
      TABLES
        binary_tab    = lt_xdata.

    mo_html_viewer->load_data(
      EXPORTING  type         = 'image'
                 subtype      = 'png'
                 size         = lv_size
                 url          = iv_url
      CHANGING   data_table   = lt_xdata
      EXCEPTIONS OTHERS       = 1 ) ##NO_TEXT.

    ASSERT sy-subrc = 0. " Image data error

  ENDMETHOD.                  "cache_image

ENDCLASS.                     "lcl_gui IMPLEMENTATION

CLASS lcl_gui_page_explore DEFINITION FINAL INHERITING FROM lcl_gui_page_super.
  PUBLIC SECTION.
    METHODS lif_gui_page~render REDEFINITION.

ENDCLASS.                       "lcl_gui_page_explore DEFINITION

CLASS lcl_gui_page_explore IMPLEMENTATION.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.
    ro_html->add( redirect( 'http://larshp.github.io/abapGit/explore.html' ) ).

  ENDMETHOD.

ENDCLASS.                       "lcl_gui_page_explore IMPLEMENTATION

CLASS lcl_gui_page_diff DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS: constructor
      IMPORTING
        is_local  TYPE ty_file
        is_remote TYPE ty_file.

    METHODS lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.
    DATA: mv_filename TYPE string,
          mo_diff     TYPE REF TO lcl_diff.

    METHODS styles       RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_head  RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_diff  RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.
    METHODS render_lines RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_diff IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    mv_filename = is_local-filename.

    CREATE OBJECT mo_diff
      EXPORTING
        iv_local  = is_local-data
        iv_remote = is_remote-data.

  ENDMETHOD.

  METHOD styles.
    DATA lo_html TYPE REF TO lcl_html_helper.
    CREATE OBJECT lo_html.

    lo_html->add( '/* DIFF */' ).                           "#EC NOTEXT
    lo_html->add( 'div.diff {' ).                           "#EC NOTEXT
    lo_html->add( '  background-color: #f2f2f2;' ).         "#EC NOTEXT
    lo_html->add( '  padding: 0.7em    ' ).                 "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'div.diff_head {' ).                      "#EC NOTEXT
    lo_html->add( '  border-bottom: 1px solid #DDD;' ).     "#EC NOTEXT
    lo_html->add( '  padding-bottom: 0.7em;' ).             "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'span.diff_name {' ).                     "#EC NOTEXT
    lo_html->add( '  padding-left: 0.5em;' ).               "#EC NOTEXT
    lo_html->add( '  color: grey;' ).                       "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'span.diff_name strong {' ).              "#EC NOTEXT
    lo_html->add( '  color: #333;' ).                       "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'span.diff_banner {' ).                   "#EC NOTEXT
    lo_html->add( '  border-style: solid;' ).               "#EC NOTEXT
    lo_html->add( '  border-width: 1px;' ).                 "#EC NOTEXT
    lo_html->add( '  border-radius: 3px;' ).                "#EC NOTEXT
    lo_html->add( '  padding-left: 0.3em;' ).               "#EC NOTEXT
    lo_html->add( '  padding-right: 0.3em;' ).              "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( '.diff_ins {' ).                          "#EC NOTEXT
    lo_html->add( '  border-color: #38e038;' ).             "#EC NOTEXT
    lo_html->add( '  background-color: #91ee91 !important;' ). "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( '.diff_del {' ).                          "#EC NOTEXT
    lo_html->add( '  border-color: #ff8093;' ).             "#EC NOTEXT
    lo_html->add( '  background-color: #ffb3be !important;' ). "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( '.diff_upd {' ).                          "#EC NOTEXT
    lo_html->add( '  border-color: #dada00;' ).             "#EC NOTEXT
    lo_html->add( '  background-color: #ffffb3 !important;' ). "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'div.diff_content {' ).                   "#EC NOTEXT
    lo_html->add( '  background: #fff;' ).                  "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT

    " Table part
    lo_html->add( '/* DIFF TABLE */' ).                     "#EC NOTEXT
    lo_html->add( 'table.diff_tab {' ).                     "#EC NOTEXT
    lo_html->add( '  font-family: Consolas, Courier, monospace;' ). "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab th {' ).                  "#EC NOTEXT
    lo_html->add( '  color: grey;' ).                       "#EC NOTEXT
    lo_html->add( '  text-align: left;' ).                  "#EC NOTEXT
    lo_html->add( '  font-weight: normal;' ).               "#EC NOTEXT
    lo_html->add( '  padding: 0.5em;' ).                    "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab td {' ).                  "#EC NOTEXT
    lo_html->add( '  color: #444;' ).                       "#EC NOTEXT
    lo_html->add( '  padding-left: 0.5em;' ).               "#EC NOTEXT
    lo_html->add( '  padding-right: 0.5em;' ).              "#EC NOTEXT
    lo_html->add( '  font-size: 12pt;' ).                   "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab td.num, th.num {' ).      "#EC NOTEXT
    lo_html->add( '  text-align: right;' ).                 "#EC NOTEXT
    lo_html->add( '  color: #ccc;' ).                       "#EC NOTEXT
    lo_html->add( '  border-left: 1px solid #eee;' ).       "#EC NOTEXT
    lo_html->add( '  border-right: 1px solid #eee;' ).      "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab td.cmd, th.cmd {' ).      "#EC NOTEXT
    lo_html->add( '  text-align: center !important;' ).     "#EC NOTEXT
    lo_html->add( '  white-space: nowrap;' ).               "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT
    lo_html->add( 'table.diff_tab tr.diff_nav_line {').     "#EC NOTEXT
    lo_html->add( '  background-color: #edf2f9;').          "#EC NOTEXT
    lo_html->add( '}').                                     "#EC NOTEXT
    lo_html->add( 'table.diff_tab tr.diff_nav_line td {').  "#EC NOTEXT
    lo_html->add( '  color: #ccc;').                        "#EC NOTEXT
    lo_html->add( '}').                                     "#EC NOTEXT
    lo_html->add( 'table.diff_tab code {' ).                "#EC NOTEXT
    lo_html->add( '  font-family: inherit;' ).              "#EC NOTEXT
    lo_html->add( '  white-space: pre;' ).                  "#EC NOTEXT
    lo_html->add( '}' ).                                    "#EC NOTEXT

    ro_html = lo_html.
  ENDMETHOD.

  METHOD render_head.
    DATA: lo_html  TYPE REF TO lcl_html_helper,
          ls_stats TYPE lcl_diff=>ty_count.

    CREATE OBJECT lo_html.

    ls_stats = mo_diff->stats( ).

    lo_html->add( '<div class="diff_head">' ).              "#EC NOTEXT
    lo_html->add( |<span class="diff_banner diff_ins">+ { ls_stats-insert }</span>| ).
    lo_html->add( |<span class="diff_banner diff_del">- { ls_stats-delete }</span>| ).
    lo_html->add( |<span class="diff_banner diff_upd">~ { ls_stats-update }</span>| ).
    lo_html->add( '<span class="diff_name">' ).             "#EC NOTEXT
    lo_html->add( |{ mv_filename }| ).
    lo_html->add( '</span>' ).                              "#EC NOTEXT
    lo_html->add( '</div>' ).                               "#EC NOTEXT

    ro_html = lo_html.
  ENDMETHOD.

  METHOD render_diff.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="diff">' ).                   "#EC NOTEXT
    ro_html->add( render_head( ) ).

    " Content
    ro_html->add( '<div class="diff_content">' ).           "#EC NOTEXT
    ro_html->add( '<table width="100%" class="diff_tab">' ). "#EC NOTEXT
    ro_html->add(   '<tr>' ).                               "#EC NOTEXT
    ro_html->add(   '<th class="num"></th>' ).              "#EC NOTEXT
    ro_html->add(   '<th>@LOCAL</th>' ).                    "#EC NOTEXT
    ro_html->add(   '<th class="num"></th>' ).              "#EC NOTEXT
    ro_html->add(   '<th>@REMOTE</th>' ).                   "#EC NOTEXT
    ro_html->add(   '</tr>' ).                              "#EC NOTEXT
    ro_html->add( render_lines( ) ).
    ro_html->add( '</table>' ).                             "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT

    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.

  METHOD render_lines.

    DATA: lt_diffs       TYPE lcl_diff=>ty_diffs_tt,
          lv_local       TYPE string,
          lv_remote      TYPE string,
          lv_attr_local  TYPE string,
          lv_attr_remote TYPE string,
          lv_beacon      TYPE string,
          lv_insert_nav  TYPE abap_bool.

    FIELD-SYMBOLS <ls_diff>  LIKE LINE OF lt_diffs.


    CREATE OBJECT ro_html.
    lt_diffs = mo_diff->get( ).

    LOOP AT lt_diffs ASSIGNING <ls_diff>.
      IF <ls_diff>-short = abap_false.
        lv_insert_nav = abap_true.
        CONTINUE.
      ENDIF.

      IF lv_insert_nav = abap_true. " Insert separator line with navigation
        IF <ls_diff>-beacon > 0.
          READ TABLE mo_diff->mt_beacons INTO lv_beacon INDEX <ls_diff>-beacon.
        ELSE.
          lv_beacon = '---'.
        ENDIF.

        ro_html->add( '<tr class="diff_nav_line">').
        ro_html->add( '<td class="num"></td>' ).
        ro_html->add( |<td colspan="3">@@ { <ls_diff>-local_line } @@ { lv_beacon }</td>| ).
        ro_html->add( '</tr>' ).
        lv_insert_nav = abap_false.
      ENDIF.

      lv_local  = escape( val = <ls_diff>-local  format = cl_abap_format=>e_html_attr ).
      lv_remote = escape( val = <ls_diff>-remote format = cl_abap_format=>e_html_attr ).

      CLEAR: lv_attr_local, lv_attr_remote. " Class for changed lines
      CASE <ls_diff>-result.
        WHEN lcl_diff=>c_diff-insert.
          lv_attr_local  = ' class="diff_ins"'.             "#EC NOTEXT
        WHEN lcl_diff=>c_diff-delete.
          lv_attr_remote = ' class="diff_del"'.             "#EC NOTEXT
        WHEN lcl_diff=>c_diff-update.
          lv_attr_local  = ' class="diff_upd"'.             "#EC NOTEXT
          lv_attr_remote = ' class="diff_upd"'.             "#EC NOTEXT
      ENDCASE.

      ro_html->add( '<tr>' ).                               "#EC NOTEXT
      ro_html->add( |<td class="num">{ <ls_diff>-local_line }</td>| ). "#EC NOTEXT
      ro_html->add( |<td{ lv_attr_local }><code>{ lv_local }</code></td>| ). "#EC NOTEXT
      ro_html->add( |<td class="num">{ <ls_diff>-remote_line }</td>| ). "#EC NOTEXT
      ro_html->add( |<td{ lv_attr_remote }><code>{ lv_remote }</code></td>| ). "#EC NOTEXT
      ro_html->add( '</tr>' ).                              "#EC NOTEXT

    ENDLOOP.

  ENDMETHOD.

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'DIFF' ) ).
    ro_html->add( render_diff( ) ).
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_gui_router IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_gui_router IMPLEMENTATION.

  METHOD on_event.

    DATA: lv_url  TYPE string,
          lv_key  TYPE lcl_persistence_repo=>ty_repo-key,
          ls_item TYPE ty_item.


    CASE iv_action.
        " General routing
      WHEN 'main'
          OR 'explore'
          OR 'db'
          OR 'background_run'.
        ei_page  = get_page_by_name( iv_action ).
        ev_state = gc_event_state-new_page.
      WHEN 'background'.
        lv_key = iv_getdata.
        CREATE OBJECT ei_page TYPE lcl_gui_page_background
          EXPORTING
            iv_key = lv_key.
        ev_state = gc_event_state-new_page.
      WHEN 'abapgithome'.
        cl_gui_frontend_services=>execute( EXPORTING document = gc_abapgit_homepage
                                           EXCEPTIONS OTHERS = 1 ).
        IF sy-subrc <> 0.
          lcx_exception=>raise( 'Opening page in external browser failed.' ).
        ENDIF.
        ev_state = gc_event_state-no_more_act.
      WHEN 'abapgit_installation'.
        abapgit_installation( ).
        ev_state = gc_event_state-re_render.
      WHEN 'jump'.
        lcl_html_action_utils=>jump_decode( EXPORTING iv_string   = iv_getdata
                                            IMPORTING ev_obj_type = ls_item-obj_type
                                                      ev_obj_name = ls_item-obj_name ).
        lcl_objects=>jump( ls_item ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'diff'.
        ei_page  = get_page_diff( iv_getdata ).
        ev_state = gc_event_state-new_page.

        " DB actions
      WHEN 'db_display' OR 'db_edit'.
        ei_page  = get_page_db_by_name( iv_name = iv_action  iv_getdata = iv_getdata ).
        ev_state = gc_event_state-new_page.
      WHEN 'db_delete'.
        db_delete( iv_getdata = iv_getdata ).
        ev_state = gc_event_state-re_render.
      WHEN 'db_save'.
        db_save( it_postdata ).
        ev_state = gc_event_state-go_back.

        " Repository state actions
      WHEN 'uninstall'.
        lv_key   = iv_getdata.
        repo_purge( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'remove'.
        lv_key   = iv_getdata.
        repo_remove( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'zipimport'.
        lv_key   = iv_getdata.
        lcl_zip=>import( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'zipexport'.
        lv_key   = iv_getdata.
        lcl_zip=>export( lcl_app=>repo_srv( )->get( lv_key ) ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'files_commit'. "TODO refactor name ?
        lv_key   = iv_getdata.
        lcl_zip=>export( io_repo = lcl_app=>repo_srv( )->get( lv_key )
                         iv_zip  = abap_false ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'packagezip'.
        lcl_popups=>repo_package_zip( ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'transportzip'.
        lcl_transport=>zip( ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'refresh'.
        lv_key = iv_getdata.
        lcl_app=>repo_srv( )->get( lv_key )->refresh( ).
        ev_state = gc_event_state-re_render.

        " explore page
      WHEN 'install'.
        lv_url = iv_getdata.
        lcl_popups=>repo_clone( lv_url ).
        ev_state = gc_event_state-re_render.

        " Repository online actions
      WHEN 'pull'.
        lv_key   = iv_getdata.
        repo_pull( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'stage'.
        lv_key   = iv_getdata.
        ei_page  = get_page_stage( lv_key ).
        ev_state = gc_event_state-new_page_w_bookmark.
      WHEN 'reset'.
        lv_key   = iv_getdata.
        reset( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'create_branch'.
        lv_key   = iv_getdata.
        create_branch( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'branch_overview'.
        ei_page  = get_page_branch_overview( iv_getdata ).
        ev_state = gc_event_state-new_page.
      WHEN OTHERS.
        ev_state = gc_event_state-not_handled.
    ENDCASE.
  ENDMETHOD.        " on_event

  METHOD get_page_by_name.

    DATA: lv_page_class TYPE string,
          lv_message    TYPE string.

    lv_page_class = |LCL_GUI_PAGE_{ to_upper( iv_name ) }|.

    TRY.
        CREATE OBJECT ri_page TYPE (lv_page_class).
      CATCH cx_sy_create_object_error.
        lv_message = |Cannot create page class { lv_page_class }|.
        lcx_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.        " get_page_by_name

  METHOD get_page_db_by_name.

    DATA: lv_page_class TYPE string,
          lv_message    TYPE string,
          ls_key        TYPE lcl_persistence_db=>ty_content.

    lv_page_class = |LCL_GUI_PAGE_{ to_upper( iv_name ) }|.
    ls_key        = lcl_html_action_utils=>dbkey_decode( iv_getdata ).

    TRY.
        CREATE OBJECT ri_page TYPE (lv_page_class)
          EXPORTING
            is_key = ls_key.

      CATCH cx_sy_create_object_error.
        lv_message = |Cannot create page class { lv_page_class }|.
        lcx_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.        " get_page_db_by_name

  METHOD get_page_branch_overview.

    DATA: lo_repo TYPE REF TO lcl_repo_online,
          lo_page TYPE REF TO lcl_gui_page_branch_overview,
          lv_key  TYPE lcl_persistence_repo=>ty_repo-key.


    lv_key = iv_getdata.

    lo_repo ?= lcl_app=>repo_srv( )->get( lv_key ).

    CREATE OBJECT lo_page
      EXPORTING
        io_repo = lo_repo.

    ri_page = lo_page.

  ENDMETHOD.

  METHOD get_page_diff.

    DATA: lt_remote TYPE ty_files_tt,
          lt_local  TYPE ty_files_item_tt,
          lo_page   TYPE REF TO lcl_gui_page_diff,
          lo_repo   TYPE REF TO lcl_repo_online,
          ls_file   TYPE ty_repo_file,
          lv_key    TYPE lcl_persistence_repo=>ty_repo-key.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF lt_remote,
                   <ls_local>  LIKE LINE OF lt_local.

    lcl_html_action_utils=>file_decode( EXPORTING iv_string = iv_getdata
                                        IMPORTING ev_key    = lv_key
                                                  eg_file   = ls_file ).

    lo_repo  ?= lcl_app=>repo_srv( )->get( lv_key ).
    lt_remote = lo_repo->get_files_remote( ).
    lt_local  = lo_repo->get_files_local( ).

    READ TABLE lt_remote ASSIGNING <ls_remote>
      WITH KEY filename = ls_file-filename
               path     = ls_file-path.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'file not found remotely' ).
    ENDIF.

    READ TABLE lt_local ASSIGNING <ls_local>
      WITH KEY file-filename = ls_file-filename
               file-path     = ls_file-path.
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'file not found locally' ).
    ENDIF.

    CREATE OBJECT lo_page
      EXPORTING
        is_local  = <ls_local>-file
        is_remote = <ls_remote>.

    ri_page = lo_page.

  ENDMETHOD.

  METHOD abapgit_installation.

    CONSTANTS lc_package_abapgit TYPE devclass VALUE '$ABAPGIT'.
    CONSTANTS lc_package_plugins TYPE devclass VALUE '$ABAPGIT_PLUGINS'.

    DATA lv_text            TYPE c LENGTH 100.
    DATA lv_answer          TYPE c LENGTH 1.
    DATA lo_repo            TYPE REF TO lcl_repo_online.
    DATA lv_url             TYPE string.
    DATA lv_target_package  TYPE devclass.

    lv_text = |Installing current version ABAPGit to package { lc_package_abapgit } |
           && |and plugins to { lc_package_plugins }|.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Install abapGit'
        text_question         = lv_text
        text_button_1         = 'Continue'
        text_button_2         = 'Cancel'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer ##no_text.
    IF lv_answer <> '1'.
      RETURN. ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ENDIF.

    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          lv_url            = 'https://github.com/larshp/abapGit.git'.
          lv_target_package = lc_package_abapgit.
        WHEN 2.
          lv_url            = 'https://github.com/larshp/abapGit-plugins.git' ##no_text.
          lv_target_package = lc_package_plugins.
      ENDCASE.

      IF abap_false = lcl_app=>repo_srv( )->is_repo_installed(
          iv_url              = lv_url
          iv_target_package   = lv_target_package ).

        lcl_sap_package=>create_local( lv_target_package ).

        lo_repo = lcl_app=>repo_srv( )->new_online(
          iv_url         = lv_url
          iv_branch_name = 'refs/heads/master' "TODO replace with HEAD ?
          iv_package     = lv_target_package ) ##NO_TEXT.

        lo_repo->status( ). " check for errors
        lo_repo->deserialize( ).
      ENDIF.
    ENDDO.

    COMMIT WORK.

  ENDMETHOD. "abapgit_installation

  METHOD repo_purge.

    DATA: lt_tadir    TYPE ty_tadir_tt,
          lv_count    TYPE c LENGTH 3,
          lv_answer   TYPE c LENGTH 1,
          lo_repo     TYPE REF TO lcl_repo,
          lv_package  TYPE devclass,
          lv_question TYPE c LENGTH 100.


    lo_repo = lcl_app=>repo_srv( )->get( iv_key ).

    IF lo_repo->is_write_protected( ) = abap_true.
      lcx_exception=>raise( 'Cannot purge. Local code is write-protected by repo config' ).
    ENDIF.

    lv_package = lo_repo->get_package( ).
    lt_tadir   = lcl_tadir=>read( lv_package ).

    IF lines( lt_tadir ) > 0.
      lv_count = lines( lt_tadir ).

      CONCATENATE 'This will delete all objects in package' lv_package
        INTO lv_question
        SEPARATED BY space.                                 "#EC NOTEXT

      CONCATENATE lv_question '(' lv_count 'objects)'
        INTO lv_question
        SEPARATED BY space.                                 "#EC NOTEXT

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Uninstall'
          text_question         = lv_question
          text_button_1         = 'Delete'
          icon_button_1         = 'ICON_DELETE'
          text_button_2         = 'Cancel'
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '2'
          display_cancel_button = abap_false
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.                        "#EC NOTEXT
      IF sy-subrc <> 0.
        lcx_exception=>raise( 'error from POPUP_TO_CONFIRM' ).
      ENDIF.

      IF lv_answer = '2'.
        RETURN.
      ENDIF.

      lcl_objects=>delete( lt_tadir ).

    ENDIF.

    lcl_app=>repo_srv( )->delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.                    "repo_purge

  METHOD repo_remove.

    DATA: lv_answer   TYPE c LENGTH 1,
          lo_repo     TYPE REF TO lcl_repo,
          lv_package  TYPE devclass,
          lv_question TYPE c LENGTH 100.


    lo_repo = lcl_app=>repo_srv( )->get( iv_key ).
    lv_package = lo_repo->get_package( ).

    CONCATENATE 'This will remove the repository reference to the package'
      lv_package
      INTO lv_question
      SEPARATED BY space.                                   "#EC NOTEXT

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Remove'
        text_question         = lv_question
        text_button_1         = 'Remove'
        icon_button_1         = 'ICON_WF_UNLINK'
        text_button_2         = 'Cancel'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.                          "#EC NOTEXT
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from POPUP_TO_CONFIRM' ).
    ENDIF.

    IF lv_answer = '2'.
      RETURN.
    ENDIF.

    lcl_app=>repo_srv( )->delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.                    "repo_remove

  METHOD reset.

    DATA: lo_repo   TYPE REF TO lcl_repo_online,
          lv_answer TYPE c LENGTH 1.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    IF lo_repo->is_write_protected( ) = abap_true.
      lcx_exception=>raise( 'Cannot reset. Local code is write-protected by repo config' ).
    ENDIF.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Warning'
        text_question         = 'Reset local objects?'
        text_button_1         = 'Ok'
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'Cancel'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.                        "#EC NOTEXT
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from POPUP_TO_CONFIRM' ).
    ENDIF.

    IF lv_answer = '2'.
      RETURN.
    ENDIF.

    lo_repo->deserialize( ).

  ENDMETHOD.

  METHOD create_branch.

    DATA: lv_name   TYPE string,
          lv_cancel TYPE abap_bool,
          lo_repo   TYPE REF TO lcl_repo_online.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    lcl_popups=>create_branch_popup(
      IMPORTING
        ev_name   = lv_name
        ev_cancel = lv_cancel ).
    IF lv_cancel = abap_true.
      RETURN.
    ENDIF.

    ASSERT lv_name CP 'refs/heads/+*'.

    lcl_git_porcelain=>create_branch(
      io_repo = lo_repo
      iv_name = lv_name
      iv_from = lo_repo->get_sha1_local( ) ).

* automatically switch to new branch
    lo_repo->set_branch_name( lv_name ).

    MESSAGE 'Switched to new branch' TYPE 'S' ##NO_TEXT.

  ENDMETHOD.

  METHOD repo_pull.

    DATA: lo_repo TYPE REF TO lcl_repo_online.

    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    IF lo_repo->is_write_protected( ) = abap_true.
      lcx_exception=>raise( 'Cannot pull. Local code is write-protected by repo config' ).
    ENDIF.

    lo_repo->refresh( ).
    lo_repo->deserialize( ).

    COMMIT WORK.

  ENDMETHOD.                    "pull

  METHOD get_page_stage.

    DATA: lo_repo       TYPE REF TO lcl_repo_online,
          lo_stage_page TYPE REF TO lcl_gui_page_stage.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    " force refresh on stage, to make sure the latest local and remote files are used
    lo_repo->refresh( ).

    CREATE OBJECT lo_stage_page
      EXPORTING
        io_repo = lo_repo.

    ri_page = lo_stage_page.

  ENDMETHOD.

  METHOD db_delete.

    DATA: lv_answer TYPE c LENGTH 1,
          ls_key    TYPE lcl_persistence_db=>ty_content.


    ls_key = lcl_html_action_utils=>dbkey_decode( iv_getdata ).

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Warning'
        text_question         = 'Delete?'
        text_button_1         = 'Ok'
        icon_button_1         = 'ICON_DELETE'
        text_button_2         = 'Cancel'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.                        "#EC NOTEXT
    IF sy-subrc <> 0.
      lcx_exception=>raise( 'error from POPUP_TO_CONFIRM' ).
    ENDIF.

    IF lv_answer = '2'.
      RETURN.
    ENDIF.

    lcl_app=>db( )->delete(
      iv_type  = ls_key-type
      iv_value = ls_key-value ).

    COMMIT WORK.

  ENDMETHOD.

  METHOD db_save.

    DATA: lv_string  TYPE string,
          ls_content TYPE lcl_persistence_db=>ty_content,
          lt_fields  TYPE tihttpnvp.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    CONCATENATE LINES OF it_postdata INTO lv_string.

    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'type' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    ls_content-type = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'value' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    ls_content-value = <ls_field>-value.

    READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = 'xmldata' ##NO_TEXT.
    ASSERT sy-subrc = 0.
    IF <ls_field>-value(1) <> '<'.
      ls_content-data_str = <ls_field>-value+1. " hmm
    ENDIF.

    lcl_app=>db( )->update(
      iv_type  = ls_content-type
      iv_value = ls_content-value
      iv_data  = ls_content-data_str ).

    COMMIT WORK.

  ENDMETHOD.

ENDCLASS.           " lcl_gui_router