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

    TYPES: BEGIN OF ty_popup,
             url         TYPE string,
             package     TYPE devclass,
             branch_name TYPE string,
             cancel      TYPE abap_bool,
           END OF ty_popup.

    METHODS get_page_by_name
      IMPORTING iv_name        TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS repo_popup
      IMPORTING iv_url          TYPE string
                iv_package      TYPE devclass OPTIONAL
                iv_branch       TYPE string DEFAULT 'refs/heads/master'
      RETURNING VALUE(rs_popup) TYPE ty_popup
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

    METHODS get_page_commit
      IMPORTING iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS get_page_db_by_name
      IMPORTING iv_name        TYPE clike
                iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO lif_gui_page
      RAISING   lcx_exception.

    METHODS abapgit_installation
      RAISING lcx_exception.

    METHODS repo_clone
      IMPORTING iv_url TYPE string
      RAISING   lcx_exception.

    METHODS repo_purge
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS repo_remove
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS repo_new_offline
      RAISING lcx_exception.

    METHODS repo_package_zip
      RAISING lcx_exception.

    METHODS repo_pull
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS switch_branch
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS reset
      IMPORTING iv_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS create_branch_popup
      EXPORTING ev_name   TYPE string
                ev_cancel TYPE abap_bool
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

    METHODS commit_push
      IMPORTING it_postdata TYPE cnht_post_data_tab
      RAISING   lcx_exception.

    METHODS stage_handle_action
      IMPORTING iv_getdata TYPE clike
                iv_action  TYPE clike
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
          lv_state = mi_cur_page->on_event(
            iv_action      = action
            iv_frame       = frame
            iv_getdata     = getdata
            it_postdata    = postdata
            it_query_table = query_table ).
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
            _raise 'Unknown action'.
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

    on_event( action = 'main' ).

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
      EXCEPTIONS OTHERS       = 1 ).

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


CLASS lcl_gui_page_main DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~render     REDEFINITION.
    METHODS lif_gui_page~get_assets REDEFINITION.

    CLASS-METHODS render_repo_top
      IMPORTING io_repo        TYPE REF TO lcl_repo
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_repo_item,
             obj_type TYPE tadir-object,
             obj_name TYPE tadir-obj_name,
             is_first TYPE abap_bool,
             files    TYPE tt_repo_files,
           END OF ty_repo_item.
    TYPES   tt_repo_items TYPE STANDARD TABLE OF ty_repo_item WITH DEFAULT KEY.

    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS render_error
      IMPORTING ix_error       TYPE REF TO lcx_exception
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS render_toc
      IMPORTING it_list        TYPE lcl_repo_srv=>ty_repo_tt
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS build_main_menu
      RETURNING VALUE(ro_menu) TYPE REF TO lcl_html_toolbar.

    METHODS render_repo_menu
      IMPORTING io_repo        TYPE REF TO lcl_repo
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS render_repo
      IMPORTING io_repo        TYPE REF TO lcl_repo
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS extract_repo_content
      IMPORTING io_repo       TYPE REF TO lcl_repo
      EXPORTING et_repo_items TYPE tt_repo_items
                eo_log        TYPE REF TO lcl_log
      RAISING   lcx_exception.

    METHODS render_repo_item
      IMPORTING io_repo        TYPE REF TO lcl_repo
                is_item        TYPE ty_repo_item
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS render_obj_jump_link
      IMPORTING iv_obj_type    TYPE tadir-object
                iv_obj_name    TYPE tadir-obj_name
      RETURNING VALUE(rv_html) TYPE string.

    METHODS render_explore
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS needs_installation
      RETURNING VALUE(rv_not_completely_installed) TYPE abap_bool.

ENDCLASS.

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

CLASS lcl_gui_page_background_run DEFINITION FINAL
    INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      lif_gui_page~on_event REDEFINITION,
      lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.
    DATA: mt_text TYPE TABLE OF string.

    METHODS: run.

ENDCLASS.

CLASS lcl_gui_page_background_run IMPLEMENTATION.

  METHOD lif_gui_page~on_event.
    RETURN.
  ENDMETHOD.

  METHOD run.

    DATA: lx_error TYPE REF TO lcx_exception,
          lv_text  TYPE string,
          lv_line  TYPE i VALUE 1.


    TRY.
        lcl_background=>run( ).

        DO.
          READ LINE lv_line LINE VALUE INTO lv_text.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          APPEND lv_text TO mt_text.
          lv_line = lv_line + 1.
        ENDDO.
      CATCH lcx_exception INTO lx_error.
        APPEND lx_error->mv_text TO mt_text.
    ENDTRY.

  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lv_text LIKE LINE OF mt_text.


    run( ).

    CREATE OBJECT ro_html.

    ro_html->add( header( ) ).
    ro_html->add( title( 'BACKGROUND_RUN' ) ).
    ro_html->add( '<div id="toc">' ).
    LOOP AT mt_text INTO lv_text.
      ro_html->add( '<pre>' && lv_text && '</pre><br>' ).
    ENDLOOP.
    ro_html->add( '</div>' ).
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui_page_background DEFINITION FINAL
    INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      lif_gui_page~on_event REDEFINITION,
      lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.

    METHODS:
      parse_fields
        IMPORTING iv_getdata       TYPE clike
        RETURNING VALUE(rs_fields) TYPE lcl_persistence_background=>ty_background,
      render_data
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      save
        IMPORTING iv_getdata TYPE clike
        RAISING   lcx_exception.

ENDCLASS.

CLASS lcl_gui_page_background IMPLEMENTATION.

  METHOD parse_fields.

    DEFINE _field.
      READ TABLE lt_fields ASSIGNING <ls_field> WITH KEY name = &1.
      IF sy-subrc = 0.
        rs_fields-&2 = <ls_field>-value.
      ENDIF.
    END-OF-DEFINITION.

    DATA: lt_fields TYPE tihttpnvp,
          lv_string TYPE string.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    lv_string = iv_getdata.     " type conversion
    lt_fields = cl_http_utility=>if_http_utility~string_to_fields( lv_string ).

    _field 'key' key.
    _field 'method' method.
    _field 'username' username.
    _field 'password' password.

    ASSERT NOT rs_fields IS INITIAL.

  ENDMETHOD.

  METHOD lif_gui_page~on_event.

    CASE iv_action.
      WHEN 'save'.
        save( iv_getdata ).
        rv_state = gc_event_state-re_render.
    ENDCASE.

  ENDMETHOD.

  METHOD save.

    DATA: ls_fields      TYPE lcl_persistence_background=>ty_background,
          lo_persistence TYPE REF TO lcl_persistence_background.


    ls_fields = parse_fields( iv_getdata ).

    CREATE OBJECT lo_persistence.

    IF ls_fields-method = lcl_persistence_background=>c_method-nothing.
      lo_persistence->delete( ls_fields-key ).
    ELSE.
      lo_persistence->modify( ls_fields ).
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.

  METHOD render_data.

    DATA: lo_repo    TYPE REF TO lcl_repo,
          lo_online  TYPE REF TO lcl_repo_online,
          lo_per     TYPE REF TO lcl_persistence_background,
          lt_per     TYPE lcl_persistence_background=>tt_background,
          ls_per     LIKE LINE OF lt_per,
          lv_nothing TYPE string,
          lv_push    TYPE string,
          lv_pull    TYPE string,
          lt_list    TYPE lcl_repo_srv=>ty_repo_tt.


    CREATE OBJECT ro_html.

    ro_html->add( '<div id="toc">' ).
    ro_html->add( 'Listing online repositories' ) ##NO_TEXT.
    ro_html->add( '<br><br>' ).

    CREATE OBJECT lo_per.
    lt_per = lo_per->list( ).
    lt_list = lcl_app=>repo_srv( )->list( ).

    LOOP AT lt_list INTO lo_repo.
      IF lo_repo->is_offline( ) = abap_false.
        lo_online ?= lo_repo.

        READ TABLE lt_per INTO ls_per WITH KEY key = lo_online->get_key( ).
        IF sy-subrc <> 0.
          CLEAR ls_per.
        ENDIF.

        CLEAR lv_push.
        CLEAR lv_pull.
        CLEAR lv_nothing.
        CASE ls_per-method.
          WHEN lcl_persistence_background=>c_method-push.
            lv_push = ' checked'.
          WHEN lcl_persistence_background=>c_method-pull.
            lv_pull = ' checked'.
          WHEN OTHERS.
            lv_nothing = ' checked'.
        ENDCASE.

        ro_html->add( '<h1>' && lo_online->get_name( ) && '</h1>' ).
        ro_html->add( '<form method="get" action="sapevent:save">' ).
        ro_html->add( '<input type="hidden" name="key" value="' &&
          lo_repo->get_key( ) && '">' ).
        ro_html->add( '<input type="radio" name="method" value="nothing"' &&
          lv_nothing && '>Do nothing<br>' )  ##NO_TEXT.
        ro_html->add( '<input type="radio" name="method" value="push"' &&
          lv_push && '>Automatic push<br>' )  ##NO_TEXT.
        ro_html->add( '<input type="radio" name="method" value="pull"' &&
          lv_pull && '>Automatic pull<br>' )  ##NO_TEXT.
        ro_html->add( '<br>' ).
        ro_html->add( 'Authentication, optional<br>' )  ##NO_TEXT.
        ro_html->add( '(password will be saved in clear text)<br>' )  ##NO_TEXT.
        ro_html->add( '<table>' ).
        ro_html->add( '<tr>' ).
        ro_html->add( '<td>Username:</td>' ).
        ro_html->add( '<td><input type="text" name="username" value="' &&
          ls_per-username && '"></td>' ).
        ro_html->add( '</tr>' ).
        ro_html->add( '<tr>' ).
        ro_html->add( '<td>Password:</td>' ).
        ro_html->add( '<td><input type="text" name="password" value="' &&
          ls_per-password && '"></td>' ).
        ro_html->add( '</tr>' ).
        ro_html->add( '<tr><td colspan="2" align="right">' ).
        ro_html->add( '<input type="submit" value="Save">' ).
        ro_html->add( '</td></tr>' ).
        ro_html->add( '</table>' ).
        ro_html->add( '</form>' ).
        ro_html->add( '<br>' ).
      ENDIF.
    ENDLOOP.

    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA lo_toolbar TYPE REF TO lcl_html_toolbar.

    CREATE OBJECT lo_toolbar.
    CREATE OBJECT ro_html.

    lo_toolbar->add( iv_txt = 'Run background logic'
                     iv_act = 'background_run' ).

    ro_html->add( header( ) ).
    ro_html->add( title( iv_title = 'BACKGROUND' io_menu = lo_toolbar ) ).
    ro_html->add( render_data( ) ).
    ro_html->add( footer( ) ).

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui_page_commit DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_repo_key TYPE lcl_persistence_repo=>ty_repo-key
      RAISING   lcx_exception.

    METHODS lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo  TYPE REF TO lcl_repo_online,
          mo_stage TYPE REF TO lcl_stage.

    METHODS render_menu
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS render_stage
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS render_form
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS scripts
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_commit IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    mo_repo ?= lcl_app=>repo_srv( )->get( iv_repo_key ).
    mo_stage = lcl_app=>repo_srv( )->get_stage( iv_repo_key ).
  ENDMETHOD.

  METHOD render_stage.

    DATA: lt_stage TYPE lcl_stage=>ty_stage_tt.

    FIELD-SYMBOLS: <ls_stage> LIKE LINE OF lt_stage.

    CREATE OBJECT ro_html.

    lt_stage = mo_stage->get_all( ).

    ro_html->add( '<table class="stage_tab">' ).
    ro_html->add( '<tr class="title firstrow">').
    ro_html->add( '<td colspan="2">Staged files</td>').
    ro_html->add( '</tr>' ).

    LOOP AT lt_stage ASSIGNING <ls_stage>.
      ro_html->add( '<tr>' ).
      ro_html->add( '<td class="method">' ).
      ro_html->add( lcl_stage=>method_description( <ls_stage>-method ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '<td>' ).
      ro_html->add( <ls_stage>-file-path && <ls_stage>-file-filename ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.

    ro_html->add( '</table>' ).

  ENDMETHOD.    "render_stage

  METHOD render_form.
    DATA: lo_user  TYPE REF TO lcl_persistence_user,
          lv_user  TYPE string,
          lv_key   TYPE string,
          lv_email TYPE string.

* see https://git-scm.com/book/ch5-2.html
* commit messages should be max 50 characters
* body should wrap at 72 characters

    lo_user  = lcl_app=>user( ).
    lv_user  = lo_user->get_username( ).
    lv_email = lo_user->get_email( ).
    lv_key   = mo_repo->get_key( ).

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="form_div">' ).
    ro_html->add( '<form id="commit_form" method="post" action="sapevent:commit_post">' ).
    ro_html->add( |<input name="key" type="hidden" value="{ lv_key }">| ).
    ro_html->add( '<table>' ).

    ro_html->add( '<tr>' ).
    ro_html->add( '<td class="field_name">username</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( |<input name="username" type="text" size="50" value="{ lv_user }">| ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).

    ro_html->add( '<tr>' ).
    ro_html->add( '<td class="field_name">email</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( |<input name="email" type="text" size="50" value="{ lv_email }">| ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).

    ro_html->add( '<tr>' ).
    ro_html->add( '<td class="field_name">comment</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( '<input name="comment" type="text"' &&
                  ' id="commit_msg" maxlength="50" size="50">' ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).

    ro_html->add( '<tr>' ).
    ro_html->add( '<td class="field_name">body</td>' ).
    ro_html->add( '<td>' ).
    ro_html->add( '<textarea name="body" rows="10" cols="50"></textarea>' ).

    ro_html->add( '<input type="submit" class="hidden-submit">' ). "Hmmm ... reconsider

    ro_html->add( '</td>' ).
    ro_html->add( '</tr>' ).

    ro_html->add( '</table>' ).
    ro_html->add( '</form>' ).

    ro_html->add( '</div>' ).

  ENDMETHOD.    "render_form

  METHOD render_menu.

    DATA lo_toolbar TYPE REF TO lcl_html_toolbar.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    lo_toolbar->add( iv_act = 'submitCommit();'
                     iv_txt = 'Commit'
                     iv_typ = gc_action_type-onclick
                     iv_opt = gc_html_opt-emphas ) ##NO_TEXT.

    lo_toolbar->add( iv_act = 'commit_cancel'
                     iv_txt = 'Cancel'
                     iv_opt = gc_html_opt-cancel ) ##NO_TEXT.

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.      "render_menu

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'COMMIT' ) ).

    ro_html->add( '<div class="repo">' ).
    ro_html->add( lcl_gui_page_main=>render_repo_top( mo_repo ) ).
    ro_html->add( render_menu( ) ).
    ro_html->add( render_form( ) ).
    ro_html->add( render_stage( ) ).
    ro_html->add( '</div>' ).

    ro_html->add( footer( io_include_script = scripts( ) ) ).

  ENDMETHOD.  "lif_gui_page~render

  METHOD styles.
    CREATE OBJECT ro_html.

    ro_html->add('/* REPOSITORY */').
    ro_html->add('div.repo {').
    ro_html->add('  margin-top:       3px;').
    ro_html->add('  background-color: #f2f2f2;').
    ro_html->add('  padding: 0.5em 1em 0.5em 1em;').
    ro_html->add('}').
    ro_html->add('.repo_name span {').
    ro_html->add('  color: #333;').
    ro_html->add('  font-weight: bold;').
    ro_html->add('  font-size: 14pt;').
    ro_html->add('}').
    ro_html->add('.repo_name img {').
    ro_html->add('  vertical-align: baseline;').
    ro_html->add('  margin: 0 5px 0 5px;').
    ro_html->add('}').
    ro_html->add('.repo_attr {').
    ro_html->add('  color: grey;').
    ro_html->add('  font-size: 12pt;').
    ro_html->add('}').
    ro_html->add('.repo_attr span {').
    ro_html->add('  margin-left: 0.2em;').
    ro_html->add('  margin-right: 0.5em;').
    ro_html->add('}').
    ro_html->add('.repo_attr input {').
    ro_html->add('  color: grey;').     " Input wants it personaly
    ro_html->add('  font-size: 12pt;'). " Input wants it personaly
    ro_html->add('  margin-left: 0.5em;').
    ro_html->add('  margin-right: 0.5em;').
    ro_html->add('  background-color: transparent;').
    ro_html->add('  border-style: none;').
    ro_html->add('  text-overflow: ellipsis;').
    ro_html->add('}').

    ro_html->add('/* STAGE */').
    ro_html->add('.stage_tab {').
    ro_html->add('  border: 1px solid #DDD;').
    ro_html->add('  background: #fff;').
    ro_html->add('  margin-top: 0.2em;').
    ro_html->add('}').
    ro_html->add('.stage_tab td {').
    ro_html->add('  border-top: 1px solid #eee;').
    ro_html->add('  color: #333;').
    ro_html->add('  vertical-align: middle;').
    ro_html->add('  padding: 2px 0.5em;').
    ro_html->add('}').
    ro_html->add('.stage_tab td.method {').
    ro_html->add('  color: #ccc;').
    ro_html->add('}').
    ro_html->add('.stage_tab tr.firstrow td { border-top: 0px; } ' ).
    ro_html->add('.stage_tab tr.title td {').
    ro_html->add('  color: #BBB;').
    ro_html->add('  font-size: 10pt;').
    ro_html->add('  background-color: #edf2f9;').
    ro_html->add('  padding: 4px 0.5em;').
    ro_html->add('  text-align: center;').
    ro_html->add('}').

    ro_html->add('/* COMMIT */').
    ro_html->add('div.form_div {').
    ro_html->add('  margin: 0.5em 0em;').
    ro_html->add('  background-color: #F8F8F8;').
    ro_html->add('  padding: 1em 1em;').
    ro_html->add('}').
    ro_html->add('div.form_div td.field_name {').
    ro_html->add('  color: #BBB;').
    ro_html->add('  padding-right: 1em;').
    ro_html->add('}').

  ENDMETHOD.    "styles

  METHOD scripts.

    CREATE OBJECT ro_html.

    ro_html->add( 'function setInitialFocus() {' ).
    ro_html->add( '  document.getElementById("commit_msg").focus();' ).
    ro_html->add( '}' ).
    ro_html->add( 'function submitCommit() {' ).
    ro_html->add( '  document.getElementById("commit_form").submit();' ).
    ro_html->add( '}' ).
    ro_html->add( 'setInitialFocus();' ).

  ENDMETHOD.    "scripts

ENDCLASS.       "lcl_gui_page_commit

CLASS lcl_gui_page_stage DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_repo TYPE REF TO lcl_repo_online
      RAISING   lcx_exception.

    METHODS lif_gui_page~render   REDEFINITION.

  PRIVATE SECTION.
    DATA: mo_repo  TYPE REF TO lcl_repo_online,
          mo_stage TYPE REF TO lcl_stage.

    METHODS render_lines
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS render_menu
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_stage IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).
    mo_repo   = io_repo.
    mo_stage  = lcl_app=>repo_srv( )->get_stage( iv_repo_key = mo_repo->get_key( )
                                                 iv_new      = abap_true ).
  ENDMETHOD.

  METHOD render_lines.

    DATA: lv_method  TYPE lcl_stage=>ty_method,
          lv_param   TYPE string,
          lv_status  TYPE string,
          lo_toolbar TYPE REF TO lcl_html_toolbar.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF mo_stage->mt_workarea.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    LOOP AT mo_stage->mt_workarea ASSIGNING <ls_file>.

      AT NEW type. " Local/remote header line
        IF sy-tabix = 1.
          ro_html->add('<tr class="separator firstrow">').
        ELSE.
          ro_html->add('<tr class="separator">').
        ENDIF.
        IF <ls_file>-type = lcl_stage=>c_wftype-local.
          ro_html->add( '<td></td><td colspan="2">LOCAL</td>' ) ##NO_TEXT.
        ELSE. "c_remote
          ro_html->add( '<td></td><td colspan="2">REMOTE</td>' ) ##NO_TEXT.
        ENDIF.
        ro_html->add('</tr>').
      ENDAT.

      lv_method = mo_stage->lookup( iv_path     = <ls_file>-file-path
                                    iv_filename = <ls_file>-file-filename ).
      lv_param  = lcl_html_action_utils=>file_encode( iv_key  = mo_repo->get_key( )
                                                      ig_file = <ls_file>-file ).

      lo_toolbar->reset( ). " Build line actions
      IF <ls_file>-type = lcl_stage=>c_wftype-local.
        IF lv_method IS NOT INITIAL.
          lo_toolbar->add( iv_txt = 'reset' iv_act = 'stage_reset?' && lv_param ).
        ELSE.
          lo_toolbar->add( iv_txt = 'add'   iv_act = 'stage_add?' && lv_param ).
        ENDIF.
      ELSE. "c_remote
        IF lv_method IS NOT INITIAL.
          lo_toolbar->add( iv_txt = 'reset'  iv_act = 'stage_reset?' && lv_param ).
        ELSE.
          lo_toolbar->add( iv_txt = 'ignore' iv_act = 'stage_ignore?' && lv_param ).
          lo_toolbar->add( iv_txt = 'remove' iv_act = 'stage_rm?' && lv_param ).
        ENDIF.
      ENDIF.

      IF lv_method IS INITIAL.
        lv_status = '<span class="grey">?</span>'.
      ELSE.
        lv_status = lv_method.
      ENDIF.

      ro_html->add( '<tr>' ).
      ro_html->add( |<td class="status">{ lv_status }</td>| ).
      ro_html->add( |<td>{ <ls_file>-file-path && <ls_file>-file-filename }</td>| ).
      ro_html->add( '<td>' ).
      ro_html->add( lo_toolbar->render( iv_no_separator = abap_true ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).

    ENDLOOP.

  ENDMETHOD.      "render_lines

  METHOD lif_gui_page~render.

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'STAGE' ) ).

    ro_html->add( '<div class="repo">' ).
    ro_html->add( lcl_gui_page_main=>render_repo_top( mo_repo ) ).
    ro_html->add( render_menu( ) ).

    ro_html->add( '<table class="stage_tab">' ).
    ro_html->add( render_lines( ) ).
    ro_html->add( '</table>' ).

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
                       iv_opt = gc_html_opt-emphas ).
    ELSEIF mo_stage->mv_local_cnt > 0.
      lo_toolbar->add( iv_act = |stage_all?{ lv_action }|
                       iv_txt = 'Add all and commit').
    ENDIF.

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.      "render_menu

  METHOD styles.
    CREATE OBJECT ro_html.

    ro_html->add('/* REPOSITORY */').
    ro_html->add('div.repo {').
    ro_html->add('  margin-top:       3px;').
    ro_html->add('  background-color: #f2f2f2;').
    ro_html->add('  padding: 0.5em 1em 0.5em 1em;').
    ro_html->add('}').
    ro_html->add('.repo_name span {').
    ro_html->add('  color: #333;').
    ro_html->add('  font-weight: bold;').
    ro_html->add('  font-size: 14pt;').
    ro_html->add('}').
    ro_html->add('.repo_name img {').
    ro_html->add('  vertical-align: baseline;').
    ro_html->add('  margin: 0 5px 0 5px;').
    ro_html->add('}').
    ro_html->add('.repo_attr {').
    ro_html->add('  color: grey;').
    ro_html->add('  font-size: 12pt;').
    ro_html->add('}').
    ro_html->add('.repo_attr span {').
    ro_html->add('  margin-left: 0.2em;').
    ro_html->add('  margin-right: 0.5em;').
    ro_html->add('}').
    ro_html->add('.repo_attr input {').
    ro_html->add('  color: grey;').     " Input wants it personaly
    ro_html->add('  font-size: 12pt;'). " Input wants it personaly
    ro_html->add('  margin-left: 0.5em;').
    ro_html->add('  margin-right: 0.5em;').
    ro_html->add('  background-color: transparent;').
    ro_html->add('  border-style: none;').
    ro_html->add('  text-overflow: ellipsis;').
    ro_html->add('}').

    ro_html->add('/* STAGE */').
    ro_html->add('.stage_tab {').
    ro_html->add('  border: 1px solid #DDD;').
    ro_html->add('  background: #fff;').
    ro_html->add('  margin-top: 0.2em;').
    ro_html->add('}').
    ro_html->add('.stage_tab td {').
    ro_html->add('  border-top: 1px solid #eee;').
    ro_html->add('  color: #333;').
    ro_html->add('  vertical-align: middle;').
    ro_html->add('  padding: 2px 0.5em;').
    ro_html->add('}').
    ro_html->add('.stage_tab td.status {').
    ro_html->add('  width: 2em;').
    ro_html->add('  text-align: center;').
    ro_html->add('}').
    ro_html->add('.stage_tab tr.separator td {').
    ro_html->add('  color: #BBB;').
    ro_html->add('  font-size: 10pt;').
    ro_html->add('  background-color: #edf2f9;').
    ro_html->add('  padding: 4px 0.5em;').
    ro_html->add('}').
    ro_html->add('.stage_tab tr.firstrow td { border-top: 0px; } ' ).

  ENDMETHOD.    "styles

ENDCLASS.

CLASS lcl_gui_page_db DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~render REDEFINITION.

  PRIVATE SECTION.
    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_main IMPLEMENTATION.

  METHOD render_obj_jump_link.

    DATA: lv_encode TYPE string,
          lo_html   TYPE REF TO lcl_html_helper.

    lv_encode = lcl_html_action_utils=>jump_encode( iv_obj_type = iv_obj_type
                                                    iv_obj_name = iv_obj_name ).

    CREATE OBJECT lo_html.
    lo_html->add_anchor( iv_txt = |{ iv_obj_name }| iv_act = |jump?{ lv_encode }| ).
    rv_html = lo_html->mv_html.

  ENDMETHOD.

  METHOD build_main_menu.

    DATA lo_betasub TYPE REF TO lcl_html_toolbar.


    CREATE OBJECT ro_menu.
    CREATE OBJECT lo_betasub.

    lo_betasub->add( iv_txt = 'Database util'    iv_act = 'db' ).
    lo_betasub->add( iv_txt = 'Package to zip'   iv_act = 'packagezip' ).
    lo_betasub->add( iv_txt = 'Background mode'  iv_act = 'background' ).

    ro_menu->add( iv_txt = 'Refresh all'      iv_act = 'refresh' ).
    ro_menu->add( iv_txt = 'Clone'            iv_act = 'install' ).
    ro_menu->add( iv_txt = 'Explore'          iv_act = 'explore' ).
    ro_menu->add( iv_txt = 'New offline repo' iv_act = 'newoffline' ).
    IF needs_installation( ) = abap_true.
      ro_menu->add( iv_txt = 'Get abapGit'    iv_act = 'abapgit_installation' ).
    ENDIF.
    ro_menu->add( iv_txt = '&#x03b2;'         io_sub = lo_betasub ).

  ENDMETHOD.                    "build main_menu

  METHOD styles.
    CREATE OBJECT ro_html.

    ro_html->add('/* REPOSITORY */').
    ro_html->add('div.repo {').
    ro_html->add('  margin-top:       3px;').
    ro_html->add('  background-color: #f2f2f2;').
    ro_html->add('  padding: 0.5em 1em 0.5em 1em;').
    ro_html->add('}').
    ro_html->add('.repo_name span {').
    ro_html->add('  font-weight: bold;').
    ro_html->add('  color: #333;').
    ro_html->add('  font-size: 14pt;').
    ro_html->add('}').
    ro_html->add('.repo_name img {').
    ro_html->add('  vertical-align: baseline;').
    ro_html->add('  margin: 0 5px 0 5px;').
    ro_html->add('}').
    ro_html->add('.repo_attr {').
    ro_html->add('  color: grey;').
    ro_html->add('  font-size: 12pt;').
    ro_html->add('}').
    ro_html->add('.repo_attr span {').
    ro_html->add('  margin-left: 0.2em;').
    ro_html->add('  margin-right: 0.5em;').
    ro_html->add('}').
    ro_html->add('.repo_attr input {').
    ro_html->add('  color: grey;').     " Input wants it personaly
    ro_html->add('  font-size: 12pt;'). " Input wants it personaly
    ro_html->add('  margin-left: 0.5em;').
    ro_html->add('  margin-right: 0.5em;').
    ro_html->add('  background-color: transparent;').
    ro_html->add('  border-style: none;').
    ro_html->add('  text-overflow: ellipsis;').
    ro_html->add('}').

    ro_html->add('/* REPOSITORY TABLE*/').
    ro_html->add('.repo_tab {').
    ro_html->add('  border: 1px solid #DDD;').
    ro_html->add('  border-radius: 3px;').
    ro_html->add('  background: #fff;').
    ro_html->add('  margin-top: 0.5em;').
    ro_html->add('}').
    ro_html->add('.repo_tab td {').
    ro_html->add('  border-top: 1px solid #eee;').
    ro_html->add('  vertical-align: middle;').
    ro_html->add('  color: #333;').
    ro_html->add('  padding-top: 2px;').
    ro_html->add('  padding-bottom: 2px;').
    ro_html->add('}').
    ro_html->add('.repo_tab td.icon {').
    ro_html->add('  width: 32px;').
    ro_html->add('  text-align: center;').
    ro_html->add('}').
    ro_html->add('.repo_tab td.type {').
    ro_html->add('  width: 3em;').
    ro_html->add('}').
    ro_html->add('.repo_tab td.object {').
    ro_html->add('  padding-left: 0.5em;').
    ro_html->add('}').
    ro_html->add('.repo_tab td.files {').
    ro_html->add('  padding-left: 0.5em;').
    ro_html->add('}').
    ro_html->add('.repo_tab td.cmd {').
    ro_html->add('  text-align: right;').
    ro_html->add('  padding-left: 0.5em;').
    ro_html->add('  padding-right: 1em;').
    ro_html->add('}').
    ro_html->add('.repo_tab tr.unsupported { color: lightgrey; }').
    ro_html->add('.repo_tab tr.firstrow td { border-top: 0px; } ' ).
    ro_html->add('.repo_tab td.files span  { display: block; }').
    ro_html->add('.repo_tab td.cmd span    { display: block; }').
    ro_html->add('.repo_tab td.cmd a       { display: block; }').

  ENDMETHOD.

  METHOD render_repo_menu.

    DATA: lo_toolbar     TYPE REF TO lcl_html_toolbar,
          lv_key         TYPE lcl_persistence_db=>ty_value,
          lo_sub         TYPE REF TO lcl_html_toolbar,
          lo_repo_online TYPE REF TO lcl_repo_online.


    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    lv_key = io_repo->get_key( ).

    IF lcl_app=>user( )->is_hidden( lv_key ) = abap_true.
      lo_toolbar->add( iv_txt = 'Show'
                       iv_act = |unhide?{ lv_key }| ).
    ELSE.
      IF io_repo->is_offline( ) = abap_true.
        lo_toolbar->add( iv_txt = 'Import ZIP'
                         iv_act = |zipimport?{ lv_key }|
                         iv_opt = gc_html_opt-emphas ).
        lo_toolbar->add( iv_txt = 'Export ZIP'
                         iv_act = |zipexport?{ lv_key }|
                         iv_opt = gc_html_opt-emphas ).
        lo_toolbar->add( iv_txt = 'Export&amp;Commit'
                         iv_act = |files_commit?{ lv_key }|
                         iv_opt = gc_html_opt-emphas ).
      ELSE.
        lo_repo_online ?= io_repo.
        TRY.
            IF lo_repo_online->get_sha1_remote( ) <> lo_repo_online->get_sha1_local( ).
              lo_toolbar->add( iv_txt = 'Pull'
                               iv_act = |pull?{ lv_key }|
                               iv_opt = gc_html_opt-emphas ).
            ELSEIF lcl_stage_logic=>count( lo_repo_online ) > 0.
              lo_toolbar->add( iv_txt = 'Stage'
                               iv_act = |stage?{ lv_key }|
                               iv_opt = gc_html_opt-emphas ).
            ENDIF.
          CATCH lcx_exception ##NO_HANDLER.
* authorization error or repository does not exist
* ignore error
        ENDTRY.
      ENDIF.

      CREATE OBJECT lo_sub.
      lo_sub->add( iv_txt = 'Remove'
                   iv_act = |remove?{ lv_key }| ).
      lo_sub->add( iv_txt = 'Uninstall'
                   iv_act = |uninstall?{ lv_key }| ).
      lo_sub->add( iv_txt = 'Switch branch'
                   iv_act = |switch_branch?{ lv_key }| ).
      lo_sub->add( iv_txt = 'Reset'
                   iv_act = |reset?{ lv_key }| ).
      lo_sub->add( iv_txt = 'Create branch'
                   iv_act = |create_branch?{ lv_key }| ).
      lo_sub->add( iv_txt = 'Branch overview'
                   iv_act = |branch_overview?{ lv_key }| ).
      lo_toolbar->add( iv_txt = 'Advanced'
                       io_sub = lo_sub ).

      lo_toolbar->add( iv_txt = 'Refresh'
                       iv_act = |refresh?{ lv_key }| ).
      lo_toolbar->add( iv_txt = 'Hide'
                       iv_act = |hide?{ lv_key }| ).
    ENDIF.

    ro_html->add( '<div class="paddings right">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD render_repo_top.
    DATA  lo_repo_online  TYPE REF TO lcl_repo_online.
    DATA  lv_icon         TYPE string.

    CREATE OBJECT ro_html.

    IF io_repo->is_offline( ) = abap_true.
      lv_icon = 'img/repo_offline'.
    ELSE.
      lv_icon = 'img/repo_online'.
    ENDIF.

    ro_html->add( |<a id="repo{ io_repo->get_key( ) }"></a>| ).
    ro_html->add( '<table width="100%"><tr>' ).

    ro_html->add( '<td class="repo_name">' ).
    ro_html->add( |<img src="{ lv_icon }">| ).
    ro_html->add( |<span>{ io_repo->get_name( ) }</span>| ).
    ro_html->add( '</td>' ).

    ro_html->add( '<td class="repo_attr right">' ).
    ro_html->add( '<img src="img/pkg">' ).
    ro_html->add( |<span>{ io_repo->get_package( ) }</span>| ).

    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.
      ro_html->add( '<img src="img/branch">' ).
      ro_html->add( |<span>{ lo_repo_online->get_branch_name( ) }</span>| ).
      ro_html->add( '<img src="img/link">' ).
      ro_html->add( |<input type="text" value="{ lo_repo_online->get_url( ) }" readonly>| ).
    ENDIF.

    ro_html->add( '</td>' ).
    ro_html->add( '</tr></table>' ).

  ENDMETHOD.

  METHOD render_repo.
    DATA: lt_repo_items TYPE tt_repo_items,
          lx_error      TYPE REF TO lcx_exception,
          lo_log        TYPE REF TO lcl_log.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF lt_repo_items.


    CREATE OBJECT ro_html.

    ro_html->add( |<div class="repo" id="repo{ io_repo->get_key( ) }">| ).
    ro_html->add( render_repo_top( io_repo ) ).

    ro_html->add( render_repo_menu( io_repo ) ).

    IF lcl_app=>user( )->is_hidden( io_repo->get_key( ) ) = abap_false.
      TRY.
          extract_repo_content( EXPORTING io_repo       = io_repo
                                IMPORTING et_repo_items = lt_repo_items
                                          eo_log        = lo_log ).

          ro_html->add( '<table width="100%" class="repo_tab">' ).

          IF lines( lt_repo_items ) = 0.
            ro_html->add( '<tr class="unsupported firstrow"><td class="paddings">'
                         && '<center>Empty package</center>'
                         && '</td></tr>' ) ##NO_TEXT.
          ELSE.
            LOOP AT lt_repo_items ASSIGNING <ls_item>.
              ro_html->add( render_repo_item( io_repo = io_repo is_item = <ls_item> ) ).
            ENDLOOP.
          ENDIF.

          ro_html->add( '</table>' ).

          IF io_repo->is_offline( ) = abap_false.
            ro_html->add( '<div class="log">' ). "TODO ??????
            ro_html->add( lo_log->to_html( ) ).
            ro_html->add( '</div>' ).
          ENDIF.
        CATCH lcx_exception INTO lx_error.
          ro_html->add( render_error( lx_error ) ).
      ENDTRY.
    ENDIF. " Hidden

    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD extract_repo_content.

    DATA: lo_repo_online TYPE REF TO lcl_repo_online,
          lt_tadir       TYPE ty_tadir_tt,
          ls_repo_item   TYPE ty_repo_item,
          ls_file        TYPE ty_repo_file,
          lt_results     TYPE ty_results_tt.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_results,
                   <ls_tadir>  LIKE LINE OF lt_tadir.


    CLEAR et_repo_items.

    IF io_repo->is_offline( ) = abap_true.
      lt_tadir = lcl_tadir=>read( io_repo->get_package( ) ).
      LOOP AT lt_tadir ASSIGNING <ls_tadir>.
        CLEAR ls_repo_item.
        IF sy-tabix = 1.
          ls_repo_item-is_first = abap_true.
        ENDIF.
        ls_repo_item-obj_type = <ls_tadir>-object.
        ls_repo_item-obj_name = <ls_tadir>-obj_name.
        APPEND ls_repo_item TO et_repo_items.
      ENDLOOP.

    ELSE.
      CREATE OBJECT eo_log.
      lo_repo_online ?= io_repo.
      lt_results      = lo_repo_online->status( eo_log ).
      LOOP AT lt_results ASSIGNING <ls_result>.
        AT NEW obj_name. "obj_type + obj_name
          CLEAR ls_repo_item.
          IF sy-tabix = 1.
            ls_repo_item-is_first = abap_true.
          ENDIF.
          ls_repo_item-obj_type = <ls_result>-obj_type.
          ls_repo_item-obj_name = <ls_result>-obj_name.
        ENDAT.

        IF <ls_result>-filename IS NOT INITIAL.
          ls_file-path       = <ls_result>-path.
          ls_file-filename   = <ls_result>-filename.
          ls_file-is_changed = boolc( NOT <ls_result>-match = abap_true ).
          APPEND ls_file TO ls_repo_item-files.
        ENDIF.

        AT END OF obj_name. "obj_type + obj_name
          APPEND ls_repo_item TO et_repo_items.
        ENDAT.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD render_repo_item.
    DATA:
      lv_link     TYPE string,
      lv_icon     TYPE string,
      lv_difflink TYPE string,
      ls_file     LIKE LINE OF is_item-files,
      lv_trclass  TYPE string.

    CREATE OBJECT ro_html.

    IF is_item-is_first = abap_true. " TR class
      lv_trclass = 'firstrow'.
    ENDIF.
    IF is_item-obj_name IS INITIAL.
      lv_trclass = lv_trclass && ' unsupported'.
    ENDIF.
    IF lv_trclass IS NOT INITIAL.
      SHIFT lv_trclass LEFT DELETING LEADING space.
      lv_trclass = | class="{ lv_trclass }"|.
    ENDIF.

    ro_html->add( |<tr{ lv_trclass }>| ).

    IF is_item-obj_name IS INITIAL.
      ro_html->add( '<td colspan="3"></td>' ).
    ELSE.
      CASE is_item-obj_type. "TODO ??
        WHEN 'PROG' OR 'CLAS' OR 'FUGR'.
          lv_icon = |<img src="img/code">|.
        WHEN 'W3MI' OR 'W3HT'.
          lv_icon = |<img src="img/bin">|.
        WHEN ''.
          " no icon
        WHEN OTHERS.
          lv_icon = |<img src="img/obj">|.
      ENDCASE.

      lv_link = render_obj_jump_link( iv_obj_name = is_item-obj_name
                                      iv_obj_type = is_item-obj_type ).
      ro_html->add( |<td class="icon">{ lv_icon }</td>| ).
      ro_html->add( |<td class="type">{ is_item-obj_type }</td>| ).
      ro_html->add( |<td class="object">{ lv_link }</td>| ).
    ENDIF.

    IF io_repo->is_offline( ) = abap_false. " Files for online repos only

      ro_html->add( '<td class="files">' ).
      LOOP AT is_item-files INTO ls_file.
        ro_html->add( |<span>{ ls_file-path && ls_file-filename }</span>| ).
      ENDLOOP.
      ro_html->add( '</td>' ).

      ro_html->add( '<td class="cmd">' ).
      IF lines( is_item-files ) = 0.
        ro_html->add( '<span class="grey">new</span>' ).
      ELSE.
        LOOP AT is_item-files INTO ls_file.
          IF ls_file-is_changed = abap_true.
            lv_difflink = lcl_html_action_utils=>file_encode(
              iv_key  = io_repo->get_key( )
              ig_file = ls_file ).
            ro_html->add_anchor(
              iv_txt = 'diff'
              iv_act = |diff?{ lv_difflink }| ).
          ELSE.
            ro_html->add( |<span>&nbsp;</span>| ).
          ENDIF.
        ENDLOOP.
      ENDIF.
      ro_html->add( '</td>' ).

    ENDIF.

    ro_html->add( '</tr>' ).

  ENDMETHOD.

  METHOD needs_installation.

    CONSTANTS:
      lc_abapgit TYPE string VALUE 'https://github.com/larshp/abapGit.git',
      lc_plugins TYPE string VALUE 'https://github.com/larshp/abapGit-plugins.git' ##NO_TEXT.

    TRY.
        IF lcl_app=>repo_srv( )->is_repo_installed( lc_abapgit ) = abap_false
            OR lcl_app=>repo_srv( )->is_repo_installed( lc_plugins ) = abap_false.
          rv_not_completely_installed = abap_true.
        ENDIF.
      CATCH lcx_exception.
        " cannot be installed anyway in this case, e.g. no connection
        rv_not_completely_installed = abap_false.
    ENDTRY.
  ENDMETHOD.                    "needs_installation

  METHOD render_toc.

    DATA: lo_repo    LIKE LINE OF it_list,
          lo_toolbar TYPE REF TO lcl_html_toolbar.


    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    IF lines( it_list ) = 0.
      RETURN.
    ENDIF.

    LOOP AT it_list INTO lo_repo.
      lo_toolbar->add( iv_txt = lo_repo->get_name( )
                       iv_typ = gc_action_type-url
                       iv_act = |#repo{ lo_repo->get_key( ) }| ).
    ENDLOOP.

    ro_html->add( '<div id="toc">' ) ##NO_TEXT.
    ro_html->add( '<img src="img/toc">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD render_error.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="dummydiv attention">' ).
    ro_html->add( |Error: { ix_error->mv_text }| ).
    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD render_explore.

    DATA lo_toolbar TYPE REF TO lcl_html_toolbar.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    lo_toolbar->add( iv_txt = 'Explore new projects'
                     iv_act = 'explore' ).

    ro_html->add( '<div class="dummydiv">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lt_repos TYPE lcl_repo_srv=>ty_repo_tt,
          lx_error TYPE REF TO lcx_exception,
          lo_repo  LIKE LINE OF lt_repos.


    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( iv_title = 'HOME' io_menu = build_main_menu( ) ) ).

    TRY.
        lt_repos = lcl_app=>repo_srv( )->list( ).
      CATCH lcx_exception INTO lx_error.
        ro_html->add( render_error( lx_error ) ).
    ENDTRY.

    ro_html->add( render_toc( lt_repos ) ).

    IF lines( lt_repos ) = 0 AND lx_error IS INITIAL.
      ro_html->add( render_explore( ) ).
    ELSE.
      LOOP AT lt_repos INTO lo_repo.
        lcl_progress=>show( iv_key     = 'Render'
                            iv_current = sy-tabix
                            iv_total   = lines( lt_repos )
                            iv_text    = lo_repo->get_name( ) ) ##NO_TEXT.
        ro_html->add( render_repo( lo_repo ) ).
      ENDLOOP.
    ENDIF.

    ro_html->add( footer( ) ).

  ENDMETHOD.

  METHOD lif_gui_page~get_assets.

    DATA ls_image TYPE ty_web_asset.

    rt_assets = super->lif_gui_page~get_assets( ).

    ls_image-url     = 'img/toc'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAAFVBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgIAO39T0AAAABnRSTlMABBCRlMXJzV0oAAAAN0lEQVQIW2NgwABuaWlB'
      && 'YWlpDgwJDAxiAgxACshgYwAz0tLY2NISSBWBMYAmg4ADyBZhARCJAQBBchGypGCbQgAA'
      && 'AABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/repo_online'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAApVBMVEUAAABQbJxQbJxQ'
      && 'bJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQ'
      && 'bJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQ'
      && 'bJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQbJxQ'
      && 'bJz+TJ01AAAANnRSTlMAAQIDBAcJCgwSFBocHygqMTM1NkRHSU1QUWFiZGlweHuDiImL'
      && 'lZiio6a5vsfT3uTo6e3x9fsxY2JuAAAAgUlEQVQYGXXB6RaBUBSA0e+IEuIiMs9zhlDn'
      && '/R/NZWmt/LA3f1RcoaB50SydCbn20wjedkPu3sKSpMGH21PhLdZ0BATZ+cCXtxtDHGLV'
      && 'pgFW9QqJj2U0wvJvMF+5jiNGI3HK9dMQSouH6sRoFGoWd8l1dEDRWlWPQsFS98KPvvDH'
      && 'C3HLClrWc70ZAAAAAElFTkSuQmCC'.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/repo_offline'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBB'
      && 'ZG9iZSBJbWFnZVJlYWR5ccllPAAAAWNJREFUeNrEkr1KxFAQhe9P/iS6goLWiiB2PoCN'
      && 'lYW9ChbbiFhYRAQ7MaS2SOdT2PkSvoGPINiF1YTNz/WceC+sohDYwoFvZ/Zm78mcmZXG'
      && 'GDFPKDFn/L+AdEWWZUIptRmG4bLWeglHNXjHjGoppUa9CiaoX3ieJEl/z3MCXdfdIKXT'
      && '6bRFju2xYeASJ618338Dl6gf8zw3FOktpGk6QrrFmyPP82J0IgmCHxq1F0URBdbxuzuw'
      && '9nMGR2CRltCBbJpG1HUtmNGZcN/tynfAgbPgBMbWxp/DcmIIDaFdWOjtK7S/hbxnDQu0'
      && 'LGBFBEHQg7YNbAnCZ5xJWZbnRVFsuw7GM4P8hhXkPLgh0batqKqKFmM8O3FbeAanIOAM'
      && 'cJFQWNoBLpAv/e6D4PKEK3UCh+DiN9/sgG8lbhSWCNyDJ2U3MDSOwQa7cfc828rKQIF9'
      && '+x9QsxauwAMYDRA4s/kVXLP4FGAAajajeu7yxJkAAAAASUVORK5CYII='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/pkg'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA30lEQVQoU43OIUuDcRSF'
      && '8fvqhuB0mFwaKLbVBVdkX0GTFss+wYL2H4rJIIgyQQSzZcUPoGHZ9CKCmAwTMS8Y/ga3'
      && 'BWVjT7hwOQ+HEzEbMhU7jrTd69q2KhtFRU2nrvS927dm3pyqPXcuNRVD7sxiRIQlDSc+'
      && 'PGjZUFDWkYekLfdoV2XYua4rSZ61pZBkEUq2XPty41XuXJIiZGNhPDVZiFCYIMSor+Db'
      && '7RQhYnQnCsNvNmGgPFFYMQh1PU9aqrLxyGUNx/p66r9mUc2hFx3JhU9vDtQU4y9KGjaV'
      && '/gXT+AGZVIinhU2EAwAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/branch'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAqFBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA'
      && 'gICAgID/OyosAAAAN3RSTlMAAQIDBAYICQ8TFRweJScoKSo3Oj1FRk1dYWJjZmhzdIaJ'
      && 'j5GVm6CwsrS5vsHDyszV19ne7/X583teZAAAAIFJREFUGFdVytkagVAYheFvFzJlnqc0'
      && 'EEoR+u//zhxI7dbZ9z4LMJ1op9DmjpntdXiBigHbLiAYqukBVr63+YGRSazgCY/iEooP'
      && 'xKZxr0EnSbo14B1Rg4msKzj150fJrQpERPLBv7mIfNxlq+zRbZsu0JYpGlcdwjY9Twfr'
      && 'nAbNsr6IKQxJI/U5CgAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/link'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAXVBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICVwFMKAAAAHnRSTlMAAwQFBgcK'
      && 'FR4gIiMmP0JHSm+RmKDByM/R09rg+/0jN/q+AAAAX0lEQVQYV43Nxw6AIBAE0FGw916Z'
      && '//9MRQ0S4sG5bPZlCxqSCyBGXgFUJKUA4A8PUOKONzuQOxOZIjcLkrMvxGQg3skSCFYL'
      && 'Kl1Ds5LWz+33yyf4rQOSf6CjnV6rHeAA87gJtKzI8ocAAAAASUVORK5CYII='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/code'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOBAMAAADtZjDiAAAAElBMVEUAAACAgICAgICA'
      && 'gICAgICAgIC07w1vAAAABXRSTlMABECUxcOwZQcAAAA1SURBVAhbY2AODQ0NEWBgYGVg'
      && 'YGByhNAMKgIMrKyhAQxMDhA+QwCCZgVqIIUP1Q+yJzTUAAAfUAq+Os55uAAAAABJRU5E'
      && 'rkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/bin'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOBAMAAADtZjDiAAAAElBMVEUAAACAgICAgICA'
      && 'gICAgICAgIC07w1vAAAABXRSTlMABECUxcOwZQcAAABBSURBVAhbXcqxDYAwAMRAK8h9'
      && 'hmAARoANvuD3X4UCiojqZMlsbe8JAuN6ZZ9ozThRCVmsJe9H0HwdXf19W9v2eAA6Fws2'
      && 'RotPsQAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/obj'.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOBAMAAADtZjDiAAAAIVBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgIDcWqnoAAAACnRSTlMABD1AZI+RlcPFIaFe1gAA'
      && 'AEVJREFUCFtjYF+1atVKAQYGLgYGBuaJEJrBUgBCM0+A0AwLgLQIgyOIZmwCSgNptgAG'
      && '1gQQfzKDhgCSPFw9Kg2yZ9WqAgBWJBENLk6V3AAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui_page_db_display DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~render   REDEFINITION.

    METHODS: constructor
      IMPORTING is_key TYPE lcl_persistence_db=>ty_content.

  PRIVATE SECTION.
    DATA: ms_key TYPE lcl_persistence_db=>ty_content.

    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_db_display IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_key = is_key.
  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lv_data TYPE lcl_persistence_db=>ty_content-data_str.

    TRY.
        lv_data = lcl_app=>db( )->read(
          iv_type = ms_key-type
          iv_value = ms_key-value ).
      CATCH lcx_not_found ##NO_HANDLER.
    ENDTRY.

    lv_data = lcl_xml_pretty=>print( lv_data ).

    lv_data = escape( val    = lv_data
                      format = cl_abap_format=>e_html_attr ).

    CREATE OBJECT ro_html.
    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'CONFIG DISPLAY' ) ).

    ro_html->add( '<div class="db_entry">' ).
    ro_html->add( |<table class="tag"><tr><td class="label">Type:</td>| &&
                  |  <td>{ ms_key-type }</td></tr></table>| ).
    ro_html->add( |<table class="tag"><tr><td class="label">Value:</td>| &&
                  |  <td>{ ms_key-value }</td></tr></table>| ).
    ro_html->add( |<pre>{ lv_data }</pre>| ).
    ro_html->add( '</div>' ).

    ro_html->add( footer( ) ).

  ENDMETHOD.

  METHOD styles.
    CREATE OBJECT ro_html.

    ro_html->add('/* DB ENTRY DISPLAY */').
    ro_html->add('div.db_entry {').
    ro_html->add('  background-color: #f2f2f2;').
    ro_html->add('  padding: 0.5em;').
    ro_html->add('}').

    ro_html->add('div.db_entry pre { ').
    ro_html->add('  display: block; ').
    ro_html->add('  overflow: hidden; ').
    ro_html->add('  word-wrap:break-word; ').
    ro_html->add('  white-space: pre-wrap; ').
    ro_html->add('  background-color: #eaeaea;').
    ro_html->add('  padding: 0.5em;').
    ro_html->add('  width: 50em; ').
    ro_html->add('}').

    ro_html->add('table.tag {').
    ro_html->add('  display: inline-block;').
    ro_html->add('  border: 1px #b3c1cc solid;').
    ro_html->add('  background-color: #eee;').
    ro_html->add('  margin-right: 0.5em; ').
    ro_html->add('}').
    ro_html->add('table.tag td { padding: 0.2em 0.5em; }').
    ro_html->add('table.tag td.label { background-color: #b3c1cc; }').

  ENDMETHOD.            "styles

ENDCLASS.

CLASS lcl_gui_page_db_edit DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS lif_gui_page~render   REDEFINITION.

    METHODS: constructor
      IMPORTING is_key TYPE lcl_persistence_db=>ty_content.

  PRIVATE SECTION.
    DATA: ms_key TYPE lcl_persistence_db=>ty_content.

    METHODS styles
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS scripts
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_db_edit IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
    ms_key = is_key.
  ENDMETHOD.

  METHOD lif_gui_page~render.

    DATA: lv_data    TYPE lcl_persistence_db=>ty_content-data_str,
          lo_toolbar TYPE REF TO lcl_html_toolbar.

    TRY.
        lv_data = lcl_app=>db( )->read(
          iv_type  = ms_key-type
          iv_value = ms_key-value ).
      CATCH lcx_not_found ##NO_HANDLER.
    ENDTRY.

    lcl_app=>db( )->lock(
      iv_type  = ms_key-type
      iv_value = ms_key-value ).

    lv_data = lcl_xml_pretty=>print( lv_data ).

    lv_data = escape( val    = lv_data
                      format = cl_abap_format=>e_html_attr ).

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'CONFIG EDIT' ) ).

    ro_html->add( '<div class="db_entry">' ).

    " Banners
    ro_html->add( |<table class="tag"><tr><td class="label">Type:</td>| &&
                  |  <td>{ ms_key-type }</td></tr></table>| ).
    ro_html->add( |<table class="tag"><tr><td class="label">Value:</td>| &&
                  |  <td>{ ms_key-value }</td></tr></table>| ).

    " Form
    ro_html->add( '<form id="db_form" method="post" action="sapevent:db_save">' ).
    ro_html->add( |<input type="hidden" name="type" value="{ ms_key-type }">| ).
    ro_html->add( |<input type="hidden" name="value" value="{ ms_key-value }">| ).
    ro_html->add( |<textarea rows="20" cols="100" name="xmldata">{ lv_data
                     }</textarea>| ).
    ro_html->add( '</form>' ).

    " Menu
    lo_toolbar->add( iv_act = 'submitDBForm();'
                     iv_txt = 'Save'
                     iv_typ = gc_action_type-onclick
                     iv_opt = gc_html_opt-emphas ).

    ro_html->add( '<div class="paddings">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

    ro_html->add( '</div>' ). "db_entry

    ro_html->add( footer( io_include_script = scripts( ) ) ).

  ENDMETHOD.

  METHOD styles.
    CREATE OBJECT ro_html.

    ro_html->add('/* DB ENTRY DISPLAY */').
    ro_html->add('div.db_entry {').
    ro_html->add('  background-color: #f2f2f2;').
    ro_html->add('  padding: 0.5em;').
    ro_html->add('}').
    ro_html->add('div.db_entry textarea { margin: 0.5em 0em; }').
    ro_html->add('table.tag {').
    ro_html->add('  display: inline-block;').
    ro_html->add('  border: 1px #b3c1cc solid;').
    ro_html->add('  background-color: #eee;').
    ro_html->add('  margin-right: 0.5em; ').
    ro_html->add('}').
    ro_html->add('table.tag td { padding: 0.2em 0.5em; }').
    ro_html->add('table.tag td.label { background-color: #b3c1cc; }').

  ENDMETHOD.            "styles

  METHOD scripts.

    CREATE OBJECT ro_html.

    ro_html->add( 'function submitDBForm() {' ).
    ro_html->add( '  document.getElementById("db_form").submit();' ).
    ro_html->add( '}' ).

  ENDMETHOD.    "scripts

ENDCLASS.

CLASS lcl_gui_page_db IMPLEMENTATION.

  METHOD lif_gui_page~render.

    DATA: lt_data    TYPE lcl_persistence_db=>tt_content,
          lv_escaped TYPE string,
          lv_action  TYPE string,
          lv_trclass TYPE string,
          lo_toolbar TYPE REF TO lcl_html_toolbar.

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF lt_data.


    lt_data = lcl_app=>db( )->list( ).

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( 'DATABASE PERSISTENCY' ) ).

    ro_html->add( '<div class="db_list">' ).
    ro_html->add( '<table width="100%" class="db_tab">' ).

    " Header
    ro_html->add( '<tr>' ).
    ro_html->add( '<th>Type</th>' ).
    ro_html->add( '<th>Value</th>' ).
    ro_html->add( '<th>Data</th>' ).
    ro_html->add( '<th></th>' ).
    ro_html->add( '</tr>' ).

    " Lines
    LOOP AT lt_data ASSIGNING <ls_data>.
      CLEAR lv_trclass.
      IF sy-tabix = 1.
        lv_trclass = ' class="firstrow"'.
      ENDIF.

      IF strlen( <ls_data>-data_str ) >= 250.
        lv_escaped = escape( val    = <ls_data>-data_str(250)
                             format = cl_abap_format=>e_html_attr ).
      ELSE.
        lv_escaped = escape( val    = <ls_data>-data_str
                             format = cl_abap_format=>e_html_attr ).
      ENDIF.

      lv_action = lcl_html_action_utils=>dbkey_encode( <ls_data> ).

      CREATE OBJECT lo_toolbar.
      lo_toolbar->add( iv_txt = 'Display' iv_act = |db_display?{ lv_action }| ).
      lo_toolbar->add( iv_txt = 'Edit'    iv_act = |db_edit?{ lv_action }| ).
      lo_toolbar->add( iv_txt = 'Delete'  iv_act = |db_delete?{ lv_action }| ).

      ro_html->add( |<tr{ lv_trclass }>| ).
      ro_html->add( |<td>{ <ls_data>-type }</td>| ).
      ro_html->add( |<td>{ <ls_data>-value }</td>| ).
      ro_html->add( |<td><pre>{ lv_escaped }</pre></td>| ).
      ro_html->add( '<td>' ).
      ro_html->add( lo_toolbar->render( iv_vertical = abap_true ) ).
      ro_html->add( '</td>' ).
      ro_html->add( '</tr>' ).
    ENDLOOP.

    ro_html->add( '</table>' ).
    ro_html->add( '</div>' ).

    ro_html->add( footer( ) ).

  ENDMETHOD.            "lif_gui_page~render

  METHOD styles.
    CREATE OBJECT ro_html.

    ro_html->add('/* DB ENTRIES */').
    ro_html->add('div.db_list {').
    ro_html->add('  background-color: #f2f2f2;').
    ro_html->add('  padding: 0.5em;').
    ro_html->add('}').
    ro_html->add('table.db_tab pre { ').
    ro_html->add('  display: block; ').
    ro_html->add('  overflow: hidden; ').
    ro_html->add('  word-wrap:break-word; ').
    ro_html->add('  white-space: pre-wrap; ').
    ro_html->add('  background-color: #eaeaea;').
    ro_html->add('  padding: 3px;').
    ro_html->add('  width: 50em; ').
    ro_html->add('}').
    ro_html->add('table.db_tab tr.firstrow td { padding-top: 0.5em; } ').
    ro_html->add('table.db_tab th {').
    ro_html->add('  text-align: left;').
    ro_html->add('  color: #888;').
    ro_html->add('  padding: 0.2em;').
    ro_html->add('  border-bottom: 1px #ddd solid;').
    ro_html->add('}').
    ro_html->add('table.db_tab td {').
    ro_html->add('  color: #333;').
    ro_html->add('  padding: 0.2em;').
    ro_html->add('  vertical-align: top;').
    ro_html->add('}').

  ENDMETHOD.            "styles

ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS lcl_gui_router IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_gui_router IMPLEMENTATION.

  DEFINE _add_dialog_fld.
    APPEND INITIAL LINE TO lt_fields ASSIGNING <ls_field>.
    <ls_field>-tabname    = &1.                             "#EC NOTEXT
    <ls_field>-fieldname  = &2.                             "#EC NOTEXT
    <ls_field>-fieldtext  = &3.                             "#EC NOTEXT
    <ls_field>-value      = &4.                             "#EC NOTEXT
    <ls_field>-field_attr = &5.                             "#EC NOTEXT
  END-OF-DEFINITION.

  METHOD on_event.
    DATA: lv_url  TYPE string,
          lv_key  TYPE lcl_persistence_repo=>ty_repo-key,
          ls_item TYPE ty_item.

    CASE iv_action.
        " General routing
      WHEN 'main'
          OR 'explore'
          OR 'db'
          OR 'background'
          OR 'background_run'.
        ei_page  = get_page_by_name( iv_action ).
        ev_state = gc_event_state-new_page.
      WHEN 'abapgithome'.
        cl_gui_frontend_services=>execute( EXPORTING document = gc_abapgit_homepage
                                           EXCEPTIONS OTHERS = 1 ).
        IF sy-subrc <> 0.
          _raise 'Opening page in external browser failed.'.
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
      WHEN 'install'.
        lv_url   = iv_getdata.
        repo_clone( lv_url ).
        ev_state = gc_event_state-re_render.
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
      WHEN 'newoffline'.
        repo_new_offline( ).
        ev_state = gc_event_state-re_render.
      WHEN 'files_commit'. "TODO refactor name ?
        lv_key   = iv_getdata.
        lcl_zip=>export( io_repo = lcl_app=>repo_srv( )->get( lv_key )
                         iv_zip  = abap_false ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'packagezip'. "TODO refactor name ?
        repo_package_zip( ).
        ev_state = gc_event_state-no_more_act.
      WHEN 'hide'.
        lv_key   = iv_getdata.
        lcl_app=>user( )->hide( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'unhide'.
        lv_key   = iv_getdata.
        lcl_app=>user( )->unhide( lv_key ).
        ev_state = gc_event_state-re_render.
      WHEN 'refresh'.
        lv_key = iv_getdata.
        IF lv_key IS INITIAL. " Refresh all or single
          lcl_app=>repo_srv( )->refresh( ).
        ELSE.
          lcl_app=>repo_srv( )->get( lv_key )->refresh( ).
        ENDIF.
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
      WHEN 'switch_branch'.
        lv_key   = iv_getdata.
        switch_branch( lv_key ).
        ev_state = gc_event_state-re_render.
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

        " Stage
      WHEN 'stage_commit'.
        ei_page  = get_page_commit( iv_getdata ).
        ev_state = gc_event_state-new_page.
      WHEN 'stage_all'.
        stage_handle_action( iv_getdata = iv_getdata iv_action = iv_action ).
        ei_page  = get_page_commit( iv_getdata ).
        ev_state = gc_event_state-new_page.
      WHEN 'stage_add' OR 'stage_reset' OR 'stage_ignore' OR 'stage_rm'.
        stage_handle_action( iv_getdata = iv_getdata iv_action = iv_action ).
        ev_state = gc_event_state-re_render.

        " Commit
      WHEN 'commit_post'.
        commit_push( it_postdata ).
        ev_state = gc_event_state-go_back_to_bookmark.
      WHEN 'commit_cancel'.
        ev_state = gc_event_state-go_back.

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
        _raise lv_message.
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
        _raise lv_message.
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
      _raise 'file not found remotely'.
    ENDIF.

    READ TABLE lt_local ASSIGNING <ls_local>
      WITH KEY file-filename = ls_file-filename
               file-path     = ls_file-path.
    IF sy-subrc <> 0.
      _raise 'file not found locally'.
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
          iv_branch_name = 'refs/heads/master'
          iv_package     = lv_target_package ) ##NO_TEXT.

        lo_repo->status( ). " check for errors
        lo_repo->deserialize( ).
      ENDIF.
    ENDDO.

    COMMIT WORK.

  ENDMETHOD. "abapgit_installation

  METHOD repo_popup.

    DATA: lv_returncode TYPE c,
          lv_icon_ok    TYPE icon-name,
          lv_icon_br    TYPE icon-name,
          lt_fields     TYPE TABLE OF sval,
          lv_pattr      TYPE spo_fattr,
          lv_button2    TYPE svalbutton-buttontext,
          lv_icon2      TYPE icon-name.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    IF NOT iv_package IS INITIAL.
      lv_pattr = '05'.
    ELSE.
      lv_button2 = 'Create package'.
      lv_icon2   = icon_msg.
    ENDIF.

*                   TAB           FLD       LABEL            DEF        ATTR
    _add_dialog_fld 'ABAPTXT255' 'LINE'     'Git Clone Url'  iv_url     ''.
    _add_dialog_fld 'TDEVC'      'DEVCLASS' 'Target Package' iv_package lv_pattr.
    _add_dialog_fld 'TEXTL'      'LINE'     'Branch'         iv_branch  '05'.

    lv_icon_ok  = icon_okay.
    lv_icon_br  = icon_workflow_fork.

    CALL FUNCTION 'POPUP_GET_VALUES_USER_BUTTONS'
      EXPORTING
        popup_title       = 'Repository'
        programname       = sy-repid
        formname          = 'BRANCH_POPUP'
        ok_pushbuttontext = 'OK'
        icon_ok_push      = lv_icon_ok
        first_pushbutton  = 'Select branch'
        icon_button_1     = lv_icon_br
        second_pushbutton = lv_button2
        icon_button_2     = lv_icon2
      IMPORTING
        returncode        = lv_returncode
      TABLES
        fields            = lt_fields
      EXCEPTIONS
        error_in_fields   = 1
        OTHERS            = 2.                              "#EC NOTEXT
    IF sy-subrc <> 0.
      _raise 'Error from POPUP_GET_VALUES'.
    ENDIF.
    IF lv_returncode = 'A'.
      rs_popup-cancel = abap_true.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_popup-url = <ls_field>-value.
    lcl_url=>name( rs_popup-url ).         " validate

    READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_popup-package = <ls_field>-value.
    TRANSLATE rs_popup-package TO UPPER CASE.

    READ TABLE lt_fields INDEX 3 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    rs_popup-branch_name = <ls_field>-value.

  ENDMETHOD.

  METHOD repo_clone.

    DATA: ls_popup TYPE ty_popup,
          lo_repo  TYPE REF TO lcl_repo_online.


    ls_popup = repo_popup( iv_url ).
    IF ls_popup-cancel = abap_true.
      RETURN.
    ENDIF.

    lo_repo = lcl_app=>repo_srv( )->new_online(
      iv_url         = ls_popup-url
      iv_branch_name = ls_popup-branch_name
      iv_package     = ls_popup-package ).
    lo_repo->status( ). " check for errors
    lo_repo->deserialize( ).

    COMMIT WORK.

  ENDMETHOD.                    "repo_clone

  METHOD repo_purge.

    DATA: lt_tadir    TYPE ty_tadir_tt,
          lv_count    TYPE c LENGTH 3,
          lv_answer   TYPE c LENGTH 1,
          lo_repo     TYPE REF TO lcl_repo,
          lv_package  TYPE devclass,
          lv_question TYPE c LENGTH 100.


    lo_repo = lcl_app=>repo_srv( )->get( iv_key ).
    lv_package = lo_repo->get_package( ).

    lt_tadir = lcl_tadir=>read( lv_package ).

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
        _raise 'error from POPUP_TO_CONFIRM'.
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
      _raise 'error from POPUP_TO_CONFIRM'.
    ENDIF.

    IF lv_answer = '2'.
      RETURN.
    ENDIF.

    lcl_app=>repo_srv( )->delete( lo_repo ).

    COMMIT WORK.

  ENDMETHOD.                    "repo_remove

  METHOD repo_new_offline.

    DATA: lv_returncode TYPE c,
          lv_url        TYPE string,
          lv_package    TYPE devclass,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.

    "               TAB           FLD       LABEL     DEF                 ATTR
    _add_dialog_fld 'ABAPTXT255' 'LINE'     'Name'    ''                  ''.
    _add_dialog_fld 'TDEVC'      'DEVCLASS' 'Package' ''                  ''.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'New Offline Project'             "#EC NOTEXT
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      _raise 'Error from POPUP_GET_VALUES'.
    ENDIF.
    IF lv_returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    lv_url = <ls_field>-value.

    READ TABLE lt_fields INDEX 2 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    lv_package = <ls_field>-value.
    TRANSLATE lv_package TO UPPER CASE.

    lcl_app=>repo_srv( )->new_offline(
      iv_url     = lv_url
      iv_package = lv_package ).

    COMMIT WORK.

  ENDMETHOD.                    "repo_new_offline

  METHOD repo_package_zip.

    DATA: lo_repo       TYPE REF TO lcl_repo_offline,
          ls_data       TYPE lcl_persistence_repo=>ty_repo,
          lv_returncode TYPE c,
          lt_fields     TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.

    "               TAB           FLD       LABEL     DEF                 ATTR
    _add_dialog_fld 'TDEVC'      'DEVCLASS' 'Package' ''                  ''.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        no_value_check  = abap_true
        popup_title     = 'Export'             "#EC NOTEXT
      IMPORTING
        returncode      = lv_returncode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      _raise 'Error from POPUP_GET_VALUES'.
    ENDIF.
    IF lv_returncode = 'A'.
      RETURN.
    ENDIF.

    READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
    ASSERT sy-subrc = 0.
    TRANSLATE <ls_field>-value TO UPPER CASE.

    ls_data-key             = 'DUMMY'.
    ls_data-package         = <ls_field>-value.
    ls_data-master_language = sy-langu.

    CREATE OBJECT lo_repo
      EXPORTING
        is_data = ls_data.

    lcl_zip=>export( lo_repo ).

  ENDMETHOD.                    "repo_package_zip

  METHOD switch_branch.

    DATA: lo_repo  TYPE REF TO lcl_repo_online,
          ls_popup TYPE ty_popup.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    ls_popup = repo_popup(
      iv_url     = lo_repo->get_url( )
      iv_package = lo_repo->get_package( )
      iv_branch  = lo_repo->get_branch_name( ) ).
    IF ls_popup-cancel = abap_true.
      RETURN.
    ENDIF.

    lo_repo->set_url( ls_popup-url ).
    lo_repo->set_branch_name( ls_popup-branch_name ).

    COMMIT WORK.

    lo_repo->deserialize( ).

  ENDMETHOD.

  METHOD reset.

    DATA: lo_repo   TYPE REF TO lcl_repo_online,
          lv_answer TYPE c LENGTH 1.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

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
      _raise 'error from POPUP_TO_CONFIRM'.
    ENDIF.

    IF lv_answer = '2'.
      RETURN.
    ENDIF.

    lo_repo->deserialize( ).

  ENDMETHOD.

  METHOD create_branch_popup.

    DATA: lv_answer TYPE c LENGTH 1,
          lt_fields TYPE TABLE OF sval.

    FIELD-SYMBOLS: <ls_field> LIKE LINE OF lt_fields.


    CLEAR ev_name.
    CLEAR ev_cancel.

*                   TAB     FLD   LABEL   DEF                       ATTR
    _add_dialog_fld 'TEXTL' 'LINE' 'Name' 'refs/heads/branch_name'  ''.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Create branch'
      IMPORTING
        returncode      = lv_answer
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2 ##NO_TEXT.
    IF sy-subrc <> 0.
      _raise 'error from POPUP_GET_VALUES'.
    ENDIF.

    IF lv_answer = 'A'.
      ev_cancel = abap_true.
    ELSE.
      READ TABLE lt_fields INDEX 1 ASSIGNING <ls_field>.
      ASSERT sy-subrc = 0.
      ev_name = <ls_field>-value.
    ENDIF.

  ENDMETHOD.

  METHOD create_branch.

    DATA: lv_name   TYPE string,
          lv_cancel TYPE abap_bool,
          lo_repo   TYPE REF TO lcl_repo_online.


    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).

    create_branch_popup(
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

    MESSAGE 'Switched to new branch' TYPE 'S'.

  ENDMETHOD.

  METHOD repo_pull.

    DATA: lo_repo TYPE REF TO lcl_repo_online.

    lo_repo ?= lcl_app=>repo_srv( )->get( iv_key ).
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
      _raise 'error from POPUP_TO_CONFIRM'.
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

  METHOD commit_push.

    DATA: ls_fields  TYPE lcl_html_action_utils=>ty_commit_fields,
          ls_comment TYPE ty_comment,
          lo_stage   TYPE REF TO lcl_stage,
          lo_repo    TYPE REF TO lcl_repo_online,
          lo_user    TYPE REF TO lcl_persistence_user.

    ls_fields = lcl_html_action_utils=>parse_commit_request( it_postdata ).

    lo_user = lcl_app=>user( ).   " TODO refactor - password manager
    lo_user->set_username( ls_fields-username ).
    lo_user->set_email( ls_fields-email ).

    IF ls_fields-username IS INITIAL.
      _raise 'empty username'.
    ENDIF.
    IF ls_fields-email IS INITIAL.
      _raise 'empty email'.
    ENDIF.
    IF ls_fields-comment IS INITIAL.
      _raise 'empty comment'.
    ENDIF.

    lo_repo            ?= lcl_app=>repo_srv( )->get( ls_fields-repo_key ).
    lo_stage            = lcl_app=>repo_srv( )->get_stage( ls_fields-repo_key ).
    ls_comment-username = ls_fields-username.
    ls_comment-email    = ls_fields-email.
    ls_comment-comment  = ls_fields-comment.

    IF NOT ls_fields-body IS INITIAL.
      CONCATENATE ls_comment-comment gc_newline ls_fields-body
        INTO ls_comment-comment.
    ENDIF.

    lo_repo->push( is_comment = ls_comment
                   io_stage   = lo_stage ).

    COMMIT WORK.

  ENDMETHOD.      "commit_push

  METHOD get_page_commit.

    DATA: lo_commit_page TYPE REF TO lcl_gui_page_commit,
          lv_key         TYPE lcl_persistence_repo=>ty_repo-key.

    lv_key = lcl_html_action_utils=>repo_key_decode( iv_getdata ).

    CREATE OBJECT lo_commit_page
      EXPORTING
        iv_repo_key = lv_key.

    ri_page ?= lo_commit_page.

  ENDMETHOD.

  METHOD stage_handle_action.

    DATA: ls_file  TYPE ty_file,
          lo_stage TYPE REF TO lcl_stage,
          lv_key   TYPE lcl_persistence_repo=>ty_repo-key.

    FIELD-SYMBOLS: <ls_file> LIKE LINE OF lo_stage->mt_workarea.

    IF iv_action = 'stage_all'.
      lv_key = lcl_html_action_utils=>repo_key_decode( iv_getdata ).
    ELSE.
      lcl_html_action_utils=>file_decode( EXPORTING iv_string = iv_getdata
                                          IMPORTING ev_key    = lv_key
                                                    eg_file   = ls_file ).
    ENDIF.

    lo_stage = lcl_app=>repo_srv( )->get_stage( lv_key ).

    CASE iv_action.
      WHEN 'stage_add'.
        lo_stage->add( iv_path = ls_file-path iv_filename = ls_file-filename ).
      WHEN 'stage_all'.
        LOOP AT lo_stage->mt_workarea ASSIGNING <ls_file>
            WHERE type = lcl_stage=>c_wftype-local.
          lo_stage->add( iv_path     = <ls_file>-file-path
                         iv_filename = <ls_file>-file-filename ).
        ENDLOOP.
      WHEN 'stage_reset'.
        lo_stage->reset( iv_path = ls_file-path iv_filename = ls_file-filename ).
      WHEN 'stage_ignore'.
        lo_stage->ignore( iv_path = ls_file-path iv_filename = ls_file-filename ).
      WHEN 'stage_rm'.
        lo_stage->rm( iv_path = ls_file-path iv_filename = ls_file-filename ).
    ENDCASE.

  ENDMETHOD.        "stage_handle_action

ENDCLASS.           " lcl_gui_router