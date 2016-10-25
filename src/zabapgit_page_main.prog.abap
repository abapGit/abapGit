*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_MAIN
*&---------------------------------------------------------------------*

INCLUDE zabapgit_view_repo.

CLASS lcl_gui_page_main DEFINITION FINAL INHERITING FROM lcl_gui_page_super.

  PUBLIC SECTION.
    METHODS:
      constructor
        RAISING lcx_exception,
      lif_gui_page~render     REDEFINITION,
      lif_gui_page~on_event   REDEFINITION,
      lif_gui_page~get_assets REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_actions,
                 show TYPE string VALUE 'show' ##NO_TEXT,
               END OF c_actions.

    DATA: mv_show         TYPE lcl_persistence_db=>ty_value,
          mo_repo_content TYPE REF TO lcl_gui_view_repo_content.

    METHODS:
      styles
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper,
      retrieve_active_repo
        RAISING lcx_exception,
      render_toc
        IMPORTING it_list        TYPE lcl_repo_srv=>ty_repo_tt
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      render_toc_line
        IMPORTING io_toolbar     TYPE REF TO lcl_html_toolbar
                  iv_image_url   TYPE string
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      build_main_menu
        RETURNING VALUE(ro_menu) TYPE REF TO lcl_html_toolbar,
      render_explore
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception,
      render_repo
        IMPORTING io_repo        TYPE REF TO lcl_repo
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
        RAISING   lcx_exception.

ENDCLASS.


CLASS lcl_gui_page_main IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).
  ENDMETHOD.  " constructor

  METHOD lif_gui_page~on_event.

    DATA: lv_key TYPE lcl_persistence_repo=>ty_repo-key.


    IF NOT mo_repo_content IS INITIAL.
      mo_repo_content->lif_gui_page~on_event(
        EXPORTING
          iv_action    = iv_action
          iv_prev_page = iv_prev_page
          iv_getdata   = iv_getdata
          it_postdata  = it_postdata
        IMPORTING
          ei_page      = ei_page
          ev_state     = ev_state ).

      IF ev_state <> gc_event_state-not_handled.
        RETURN.
      ENDIF.
    ENDIF.

    lv_key = iv_getdata.

    CASE iv_action.
      WHEN c_actions-show.              " Change displayed repo
        lcl_app=>user( )->set_repo_show( lv_key ).
        TRY.
            lcl_app=>repo_srv( )->get( lv_key )->refresh( ).
          CATCH lcx_exception ##NO_HANDLER.
        ENDTRY.

        ev_state = gc_event_state-re_render.
    ENDCASE.

  ENDMETHOD.  "on_event

**********************************************************************
* RENDERING
**********************************************************************

  METHOD lif_gui_page~render.

    DATA: lt_repos TYPE lcl_repo_srv=>ty_repo_tt,
          lx_error TYPE REF TO lcx_exception,
          lo_repo  LIKE LINE OF lt_repos.

    retrieve_active_repo( ). " Get and validate key of user default repo

    CREATE OBJECT ro_html.

    ro_html->add( header( io_include_style = styles( ) ) ).
    ro_html->add( title( iv_title = 'HOME'
                         io_menu  = build_main_menu( ) ) ).

    TRY.
        lt_repos = lcl_app=>repo_srv( )->list( ).
      CATCH lcx_exception INTO lx_error.
        ro_html->add( render_error( lx_error ) ).
    ENDTRY.

    ro_html->add( render_toc( lt_repos ) ).

    IF lines( lt_repos ) = 0 AND lx_error IS INITIAL.
      ro_html->add( render_explore( ) ).
    ELSE.
      lo_repo = lcl_app=>repo_srv( )->get( mv_show ).
      ro_html->add( render_repo( lo_repo ) ).
    ENDIF.

    ro_html->add( footer( ) ).

  ENDMETHOD.  "render

  METHOD retrieve_active_repo.

    DATA: lt_repos    TYPE lcl_repo_srv=>ty_repo_tt,
          lo_repo     LIKE LINE OF lt_repos,
          lv_show_old LIKE mv_show.

    TRY.
        lt_repos = lcl_app=>repo_srv( )->list( ).
      CATCH lcx_exception.
        RETURN.
    ENDTRY.

    lv_show_old = mv_show.
    mv_show     = lcl_app=>user( )->get_repo_show( ). " Get default repo from user cfg

    IF mv_show IS NOT INITIAL.
      TRY. " verify the key exists
          lo_repo = lcl_app=>repo_srv( )->get( mv_show ).
        CATCH lcx_exception.
          CLEAR mv_show.
      ENDTRY.
    ENDIF.

    IF mv_show IS INITIAL. " Fall back to first available repo
      READ TABLE lt_repos INTO lo_repo INDEX 1.
      IF sy-subrc = 0.
        mv_show = lo_repo->get_key( ).
        lcl_app=>user( )->set_repo_show( mv_show ).
      ENDIF.
    ENDIF.

    IF lv_show_old <> mv_show.
      CREATE OBJECT mo_repo_content EXPORTING iv_key = mv_show. " Reinit content state
    ENDIF.

  ENDMETHOD.  "retrieve_active_repo

  METHOD build_main_menu.

    DATA lo_betasub TYPE REF TO lcl_html_toolbar.


    CREATE OBJECT ro_menu.
    CREATE OBJECT lo_betasub.

    lo_betasub->add( iv_txt = 'Database util'    iv_act = gc_action-go_db ) ##NO_TEXT.
    lo_betasub->add( iv_txt = 'Package to zip'   iv_act = gc_action-zip_package ) ##NO_TEXT.
    lo_betasub->add( iv_txt = 'Transport to zip' iv_act = gc_action-zip_transport ) ##NO_TEXT.
    lo_betasub->add( iv_txt = 'Object to files'  iv_act = gc_action-zip_object ) ##NO_TEXT.
    lo_betasub->add( iv_txt = 'Page playground'  iv_act = gc_action-go_playground ) ##NO_TEXT.
    lo_betasub->add( iv_txt = 'Debug info'       iv_act = gc_action-go_debuginfo ) ##NO_TEXT.

    ro_menu->add( iv_txt = 'Clone'            iv_act = gc_action-repo_clone ) ##NO_TEXT.
    ro_menu->add( iv_txt = 'Explore'          iv_act = gc_action-go_explore ) ##NO_TEXT.
    ro_menu->add( iv_txt = 'New offline repo' iv_act = gc_action-repo_newoffline ) ##NO_TEXT.
    IF lcl_services_abapgit=>needs_installation( ) = abap_true.
      ro_menu->add( iv_txt = 'Get abapGit'    iv_act = gc_action-abapgit_install ) ##NO_TEXT.
    ENDIF.
    ro_menu->add( iv_txt = 'Advanced'         io_sub = lo_betasub ) ##NO_TEXT.

  ENDMETHOD.                    "build main_menu

  METHOD render_toc.

    DATA: lo_pback      TYPE REF TO lcl_persistence_background,
          lo_repo       LIKE LINE OF it_list,
          lv_opt        TYPE c LENGTH 1,
          lo_online     TYPE REF TO lcl_html_toolbar,
          lo_offline    TYPE REF TO lcl_html_toolbar,
          lv_repo_title TYPE string.


    CREATE OBJECT ro_html.
    CREATE OBJECT lo_online.
    CREATE OBJECT lo_offline.
    CREATE OBJECT lo_pback.

    IF lines( it_list ) = 0.
      RETURN.
    ENDIF.

    LOOP AT it_list INTO lo_repo.
      IF mv_show = lo_repo->get_key( ).
        lv_opt = gc_html_opt-emphas.
      ELSE.
        CLEAR lv_opt.
      ENDIF.

      IF lo_repo->is_offline( ) = abap_true.
        lo_offline->add( iv_txt = lo_repo->get_name( )
                         iv_act = |{ c_actions-show }?{ lo_repo->get_key( ) }|
                         iv_opt = lv_opt ).
      ELSE.

        lv_repo_title = lo_repo->get_name( ).
        IF lo_pback->exists( lo_repo->get_key( ) ) = abap_true.
          lv_repo_title = lv_repo_title && '<sup>bg</sup>'. " Background marker
        ENDIF.

        lo_online->add( iv_txt = lv_repo_title
                        iv_act = |{ c_actions-show }?{ lo_repo->get_key( ) }|
                        iv_opt = lv_opt ).

      ENDIF.

    ENDLOOP.

    ro_html->add( '<div id="toc"><div class="toc_grid">' ) ##NO_TEXT.

    IF lo_online->count( ) > 0.
      ro_html->add( render_toc_line( io_toolbar   = lo_online
                                     iv_image_url = 'img/repo_online' ) ).
    ENDIF.

    IF lo_offline->count( ) > 0.
      ro_html->add( render_toc_line( io_toolbar   = lo_offline
                                     iv_image_url = 'img/repo_offline' ) ).
    ENDIF.

    ro_html->add( '</div></div>' ).

  ENDMETHOD.  "render_toc

  METHOD render_toc_line.
    CREATE OBJECT ro_html.

    ro_html->add( '<div class="toc_row"><table><tr>' ).

    ro_html->add( '<td>' ).
    ro_html->add( |<img src="{ iv_image_url }">| ).
    ro_html->add( '</td>' ).

    ro_html->add( '<td>' ).
    ro_html->add( io_toolbar->render( iv_sort = abap_true ) ).
    ro_html->add( '</td>' ).

    ro_html->add( '</tr></table></div>' ).

  ENDMETHOD.  "render_toc_line

  METHOD render_explore.

    DATA lo_toolbar TYPE REF TO lcl_html_toolbar.

    CREATE OBJECT ro_html.
    CREATE OBJECT lo_toolbar.

    lo_toolbar->add( iv_txt = 'Explore new projects'
                     iv_act = gc_action-go_explore ) ##NO_TEXT.

    ro_html->add( '<div class="dummydiv">' ).
    ro_html->add( lo_toolbar->render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_explore

  METHOD render_repo.

    CREATE OBJECT ro_html.

    ro_html->add( |<div class="repo" id="repo{ io_repo->get_key( ) }">| ).
    ro_html->add( render_repo_top( io_repo = io_repo iv_interactive_branch = abap_true ) ).
    ro_html->add( mo_repo_content->lif_gui_page~render( ) ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_repo

**********************************************************************
* ASSETS, STYLES, SCRIPTS
**********************************************************************

  METHOD styles.

    CREATE OBJECT ro_html.

    _add '/* REPOSITORY TABLE*/'.
    _add 'div.repo_container {'.
    _add '  position: relative;'.
    _add '}'.
    _add '.repo_tab {'.
    _add '  border: 1px solid #DDD;'.
    _add '  border-radius: 3px;'.
    _add '  background: #fff;'.
    _add '  margin-top: 0.5em;'.
    _add '}'.
    _add '.repo_tab td {'.
    _add '  border-top: 1px solid #eee;'.
    _add '  vertical-align: middle;'.
    _add '  color: #333;'.
    _add '  padding-top: 2px;'.
    _add '  padding-bottom: 2px;'.
    _add '}'.
    _add '.repo_tab td.icon {'.
    _add '  width: 32px;'.
    _add '  text-align: center;'.
    _add '}'.
    _add '.repo_tab td.type {'.
    _add '  width: 3em;'.
    _add '}'.
    _add '.repo_tab td.object {'.
    _add '  padding-left: 0.5em;'.
    _add '}'.
    _add '.repo_tab td.files {'.
    _add '  padding-left: 0.5em;'.
    _add '}'.
    _add '.repo_tab td.cmd {'.
    _add '  text-align: right;'.
    _add '  padding-left: 0.5em;'.
    _add '  padding-right: 1em;'.
    _add '}'.
    _add '.repo_tab tr.unsupported    { color: lightgrey; }'.
    _add '.repo_tab tr.modified       { background: #fbf7e9; }'.
    _add '.repo_tab tr:first-child td { border-top: 0px; }'.
    _add '.repo_tab td.files span     { display: block; }'.
    _add '.repo_tab td.cmd span       { display: block; }'.
    _add '.repo_tab td.cmd a          { display: block; }'.
    _add 'td.current_dir              { color: #ccc; }'.

  ENDMETHOD.  "styles

  METHOD lif_gui_page~get_assets.
* http://fa2png.io/r/octicons/
* colour: #808080
* size: 16
* https://www.base64-image.de/ can be used to convert images to base64

    DATA ls_image TYPE ty_web_asset.

    rt_assets = super->lif_gui_page~get_assets( ).

    ls_image-url     = 'img/sync' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA6ElEQVQYGY3BIWuUAQAG'
      && '4Pc7N72xsbGBYNE8tYpVZKDZX2CcYLEZ9yQxOQSz3D/YmkUsVovRQ2SYNJnlkFfH7VZu'
      && 'wefJgrGHXnjrpQeu5B93smCwr6qqqp54433mDI5Ucds1u577o+p35hyoqe2cMThWVatJ'
      && '7KiZrZxz18SJqqtJPFXPssRgw0oSH9WNXMCQU76qzSxx2cxxTlk3yhKb6mcSQy7kvjpM'
      && 'Ylt98tpjN3POyFTdSuKSqppayxkjE/Uhc36p+m7PhhXr7vmmfhhnzpHPJqqqquqdcRY8'
      && 'spq47sAXMyde2c3/+wvX7Y18BexhBwAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/toc' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAAFVBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgIAO39T0AAAABnRSTlMABBCRlMXJzV0oAAAAN0lEQVQIW2NgwABuaWlB'
      && 'YWlpDgwJDAxiAgxACshgYwAz0tLY2NISSBWBMYAmg4ADyBZhARCJAQBBchGypGCbQgAA'
      && 'AABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/repo_online' ##NO_TEXT.
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

    ls_image-url     = 'img/repo_offline' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAGXRFWHRTb2Z0d2FyZQBB'
      && 'ZG9iZSBJbWFnZVJlYWR5ccllPAAAA3hpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/'
      && 'eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+'
      && 'IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2Jl'
      && 'IFhNUCBDb3JlIDUuNi1jMTMyIDc5LjE1OTI4NCwgMjAxNi8wNC8xOS0xMzoxMzo0MCAg'
      && 'ICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5'
      && 'LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9'
      && 'IiIgeG1sbnM6eG1wTU09Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9tbS8iIHht'
      && 'bG5zOnN0UmVmPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvc1R5cGUvUmVzb3Vy'
      && 'Y2VSZWYjIiB4bWxuczp4bXA9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8iIHht'
      && 'cE1NOk9yaWdpbmFsRG9jdW1lbnRJRD0ieG1wLmRpZDpkOTZmYmU4Yi0yMjEzLTZlNDct'
      && 'ODZiZC05NGE1ZTM1ZmJiMzUiIHhtcE1NOkRvY3VtZW50SUQ9InhtcC5kaWQ6NDQ2RTFD'
      && 'MTA0OTkwMTFFNjlCMzk5MTg2OTAzMDVDRDIiIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5p'
      && 'aWQ6NDQ2RTFDMEY0OTkwMTFFNjlCMzk5MTg2OTAzMDVDRDIiIHhtcDpDcmVhdG9yVG9v'
      && 'bD0iQWRvYmUgUGhvdG9zaG9wIENDIDIwMTUuNSAoV2luZG93cykiPiA8eG1wTU06RGVy'
      && 'aXZlZEZyb20gc3RSZWY6aW5zdGFuY2VJRD0ieG1wLmlpZDpkOTZmYmU4Yi0yMjEzLTZl'
      && 'NDctODZiZC05NGE1ZTM1ZmJiMzUiIHN0UmVmOmRvY3VtZW50SUQ9InhtcC5kaWQ6ZDk2'
      && 'ZmJlOGItMjIxMy02ZTQ3LTg2YmQtOTRhNWUzNWZiYjM1Ii8+IDwvcmRmOkRlc2NyaXB0'
      && 'aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+SXqL'
      && 'ugAAAWVJREFUeNrEk7tKxEAYheeSm0RXUNBaEcTOB7CxsrBXwWIbEQuLiGAnhtQW6XwK'
      && 'O1/CN/ARBLuwmrC5jOfEGVhFIbCFP3z7zyY7J+f8s5HGGDFPKTFnSbfIskwopTbDMFzW'
      && 'Wi/hUg3e4bCWUmqsV8EE6xdeT5Kk3+c5ga7rbtDS6XTaosf2smFhE3Mq3/ffwCXWj3me'
      && 'G4r0EdI0HaHd4smR53kxnEiC4ofG2ouiiALr+N0dWPs5gyOwyEhwIJumEXVdC3Y4E+67'
      && 'HfgOOHARnMDYxvhzWE4MpSG0iwh9fAX7W+h7NrCAZYEoIgiCHtg2iCUI73EmZVmeF0Wx'
      && '7RyMZwb5DSvIefCERNu2oqoqRoxx78SdwjM4BQFngI2EwtIOcIF86XcfBJsnPFIncAgu'
      && 'fstNB3wqcaOwROAePCl7AkPrGGzQjdvnWSsrAwX27X9AzUa4Ag9gNEDgzPZXcN2/C//+'
      && 'Nn4KMABcS6Z2qzb7wwAAAABJRU5ErkJggg=='.

    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/pkg' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAQAAAC1+jfqAAAA30lEQVQoU43OIUuDcRSF'
      && '8fvqhuB0mFwaKLbVBVdkX0GTFss+wYL2H4rJIIgyQQSzZcUPoGHZ9CKCmAwTMS8Y/ga3'
      && 'BWVjT7hwOQ+HEzEbMhU7jrTd69q2KhtFRU2nrvS927dm3pyqPXcuNRVD7sxiRIQlDSc+'
      && 'PGjZUFDWkYekLfdoV2XYua4rSZ61pZBkEUq2XPty41XuXJIiZGNhPDVZiFCYIMSor+Db'
      && '7RQhYnQnCsNvNmGgPFFYMQh1PU9aqrLxyGUNx/p66r9mUc2hFx3JhU9vDtQU4y9KGjaV'
      && '/gXT+AGZVIinhU2EAwAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/branch' ##NO_TEXT.
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

    ls_image-url     = 'img/link' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAXVBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICVwFMKAAAAHnRSTlMAAwQFBgcK'
      && 'FR4gIiMmP0JHSm+RmKDByM/R09rg+/0jN/q+AAAAX0lEQVQYV43Nxw6AIBAE0FGw916Z'
      && '//9MRQ0S4sG5bPZlCxqSCyBGXgFUJKUA4A8PUOKONzuQOxOZIjcLkrMvxGQg3skSCFYL'
      && 'Kl1Ds5LWz+33yyf4rQOSf6CjnV6rHeAA87gJtKzI8ocAAAAASUVORK5CYII='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/code' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOBAMAAADtZjDiAAAAElBMVEUAAACAgICAgICA'
      && 'gICAgICAgIC07w1vAAAABXRSTlMABECUxcOwZQcAAAA1SURBVAhbY2AODQ0NEWBgYGVg'
      && 'YGByhNAMKgIMrKyhAQxMDhA+QwCCZgVqIIUP1Q+yJzTUAAAfUAq+Os55uAAAAABJRU5E'
      && 'rkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/bin' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOBAMAAADtZjDiAAAAElBMVEUAAACAgICAgICA'
      && 'gICAgICAgIC07w1vAAAABXRSTlMABECUxcOwZQcAAABBSURBVAhbXcqxDYAwAMRAK8h9'
      && 'hmAARoANvuD3X4UCiojqZMlsbe8JAuN6ZZ9ozThRCVmsJe9H0HwdXf19W9v2eAA6Fws2'
      && 'RotPsQAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/obj' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOBAMAAADtZjDiAAAAIVBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgIDcWqnoAAAACnRSTlMABD1AZI+RlcPFIaFe1gAA'
      && 'AEVJREFUCFtjYF+1atVKAQYGLgYGBuaJEJrBUgBCM0+A0AwLgLQIgyOIZmwCSgNptgAG'
      && '1gQQfzKDhgCSPFw9Kg2yZ9WqAgBWJBENLk6V3AAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/lock' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAOVBMVEUAAACIiIiIiIiI'
      && 'iIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIiIjNaTNB'
      && 'AAAAEnRSTlMABgdBVXt8iYuRsNXZ3uDi6Pmu6tfUAAAASUlEQVQYV63KSxJAQBAE0TQ0'
      && 'Znym1f0PayE0QdjJ5asCgGTu1hClqjppvaRXB60swBeA2QNUAIq+ICvKx367nqAn/P8Y'
      && 't2jg3Q5rgASaF3KNRwAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/dir' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAASFBMVEUAAABmksxmksxm'
      && 'ksxmksxmksxmksxmksxmksxmksxmksxmksxmksxmksxmksxmksxmksxmksxmksxmksxm'
      && 'ksxmksxmksxmksxMwQo8AAAAF3RSTlMABhIYIy1fZmhpe3+IiYuMkZvD7e/x93sipD4A'
      && 'AAA+SURBVBhXY2BABzwiokAgzAYXEGdiBAIWIYQAPzcQCApzgwEXM4M4KuBDFxAYKAEx'
      && 'VAFeBlYOTiTAzoThewD5hBAcnWM4gwAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

  ENDMETHOD.  "get_assets

ENDCLASS.