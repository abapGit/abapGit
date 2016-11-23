*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE_MAIN
*&---------------------------------------------------------------------*

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
        IMPORTING it_repo_list   TYPE lcl_repo_srv=>ty_repo_tt
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

    IF lv_show_old <> mv_show AND NOT mv_show IS INITIAL.
      CREATE OBJECT mo_repo_content
        EXPORTING
          iv_key = mv_show. " Reinit content state
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
    lo_betasub->add( iv_txt = 'Settings'         iv_act = gc_action-go_settings ) ##NO_TEXT.

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
          lv_opt        TYPE char1,
          lv_key        TYPE lcl_persistence_repo=>ty_repo-key,
          lv_icon       TYPE string,
          lo_repo       LIKE LINE OF it_repo_list,
          lo_favbar     TYPE REF TO lcl_html_toolbar,
          lo_allbar     TYPE REF TO lcl_html_toolbar,
          lt_favorites  TYPE lcl_persistence_user=>tt_favorites,
          lv_repo_title TYPE string.


    CREATE OBJECT ro_html.
    CREATE OBJECT lo_favbar.
    CREATE OBJECT lo_allbar.
    CREATE OBJECT lo_pback.

    lt_favorites = lcl_app=>user( )->get_favorites( ).

    LOOP AT it_repo_list INTO lo_repo.
      lv_key = lo_repo->get_key( ).
      IF lv_key = mv_show.
        lv_opt = gc_html_opt-emphas.
      ELSE.
        CLEAR lv_opt.
      ENDIF.

      lv_repo_title = lo_repo->get_name( ).
      IF lo_pback->exists( lv_key ) = abap_true.
        lv_repo_title = lv_repo_title && '<sup>bg</sup>'. " Background marker
      ENDIF.

      READ TABLE lt_favorites TRANSPORTING NO FIELDS
        WITH KEY table_line = lv_key.

      IF sy-subrc = 0.
        DELETE lt_favorites INDEX sy-tabix. " for later cleanup
        lo_favbar->add( iv_txt = lv_repo_title
                        iv_act = |{ c_actions-show }?{ lv_key }|
                        iv_opt = lv_opt ).
      ENDIF.

      IF lo_repo->is_offline( ) = abap_true.
        lv_icon = '<img src="img/repo_offline">'.
      ELSE.
        lv_icon = '<img src="img/repo_online">'.
      ENDIF.

      lo_allbar->add( iv_txt = lv_repo_title
                      iv_act = |{ c_actions-show }?{ lv_key }|
                      iv_ico = lv_icon
                      iv_opt = lv_opt ).
    ENDLOOP.

    " Cleanup orphan favorites (for removed repos)
    LOOP AT lt_favorites INTO lv_key.
      lcl_app=>user( )->toggle_favorite( lv_key ).
    ENDLOOP.

    " Render HTML
    ro_html->add( '<div id="toc">' )          ##NO_TEXT. " TODO refactor html & css
    ro_html->add( '<div class="toc_grid">' )  ##NO_TEXT.
    ro_html->add( '<div class="toc_row">' )   ##NO_TEXT.

**********************************************************************

    ro_html->add( '<table width="100%"><tr>' ).
    ro_html->add( '<td class="pad-sides"><img src="img/star"></td>' ).

    ro_html->add( '<td class="pad-sides" width="100%">' ). " Maximize width
    IF lo_favbar->count( ) > 0.
      ro_html->add( lo_favbar->render( iv_sort = abap_true ) ).
    ELSE.
      ro_html->add( '<span class="grey">No favorites found. Please'
                 && ' click <img src="img/star-grey"> icon repo toolbar to add'
                 && ' it as favourite. Choose a repo there &#x2192;</span>' ).
    ENDIF.
    ro_html->add( '</td>' ).

    ro_html->add( '<td class="right">' ).
    ro_html->add( lo_allbar->render(
      iv_as_droplist_with_label = '<img class="pad4px" src="img/burger">'
      iv_sort                   = abap_true
      iv_with_icons             = abap_true
      iv_add_minizone           = abap_true ) ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr></table>' ).

**********************************************************************

    ro_html->add( '</div>' ).
    ro_html->add( '</div>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_toc

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
    _add '  padding-right: 0.7em;'.
    _add '}'.
    _add '.repo_tab tr.unsupported    { color: lightgrey; }'.
    _add '.repo_tab tr.modified       { background: #fbf7e9; }'.
    _add '.repo_tab tr:first-child td { border-top: 0px; }'.
    _add '.repo_tab td.current_dir    { color: #ccc; }'.

    " States
    _add '.repo_tab td.cmd span.state-block {'.
    _add '  margin-left: 1em;'.
    _add '  font-family: Consolas, Lucida Console, Courier, monospace;'.
    _add '  font-size: x-small;'.
    _add '  vertical-align: 13%;'.
    _add '  display: inline-block;'.
    _add '  text-align: center;'.
    _add '}'.
    _add '.repo_tab td.cmd span.state-block span {'.
    _add '  display: inline-block;'.
    _add '  padding: 0px 2px;'.
    _add '  border: 1px solid #000;'.
    _add '}'.

    _add '.repo_tab td.cmd span.state-block span.added {'.
    _add '  background-color: #69ad74; '.
    _add '  border-color: #579e64;'.
    _add '  color: white;'.
    _add '}'.
    _add '.repo_tab td.cmd span.state-block span.changed {'.
    _add '  background-color: #e0c150;'.
    _add '  border-color: #d4af25;'.
    _add '  color: white;'.
    _add '}'.
    _add '.repo_tab td.cmd span.state-block span.mixed {'.
    _add '  background-color: #e0c150;'.
    _add '  border-color: #579e64;'.
    _add '  color: #69ad74;'.
    _add '}'.
    _add '.repo_tab td.cmd span.state-block span.deleted {'.
    _add '  background-color: #c76861;'.
    _add '  border-color: #b8605a;'.
    _add '  color: white;'.
    _add '}'.
    _add '.repo_tab td.cmd span.state-block span.none {'.
    _add '  background-color: #e8e8e8;'.
    _add '  border-color: #dbdbdb;'.
    _add '  color: #c8c8c8;'.
    _add '}'.

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
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAVFBMVEUAAACAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICA'
      && 'gICAgICAgICAgICAgICAgICAgICAgICuaWnmAAAAG3RSTlMAAgQFBgsQFxweIiMtN3yI'
      && 'nqOvt9Hp6/Hz9fktMNR/AAAAXElEQVQYV5WO2xJAMAxES1q3ugfF/v9/0qLyyL4k58xk'
      && 'J0p9D7N5oeqZgSwy7fDZnHNdEE1gWK116tksl7hPimGFFPWYl7MU0zksRCl8TStKg1AJ'
      && '0XNC8Zm4/c0BUVQHi0llOUYAAAAASUVORK5CYII='.
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

    ls_image-url     = 'img/burger' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQBAMAAADt3eJSAAAAHlBMVEUAAABtktltktlt'
      && 'ktltktltktltktltktltktltktk7ccVDAAAACXRSTlMAFDBLY2SFoPGv/DFMAAAAJ0lE'
      && 'QVQIW2NggIHKmWAwmaETwpjGoBoKBo4MmIAkxXApuGK4dgwAAJa5IzLs+gRBAAAAAElF'
      && 'TkSuQmCC'.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/star' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAilBMVEUAAABejclejcle'
      && 'jclejclejclejclejclejclejclejclejclejclejclejclejclejclejclejclejcle'
      && 'jclejclejclejclejclejclejclejclejclejclejclejclejclejclejclejclejcle'
      && 'jclejclejclejclejclejclejclejclejcn2yvsVAAAALXRSTlMAAQIFBwkKCw0QERUY'
      && 'HB4jLzEzNjg7PVdYYmRvd3mDm52eub7R0+Tr8fX3+/16wo8zAAAAcElEQVQYGW3BBxKC'
      && 'MABFwYcQETv2hg1UVP79ryeTZBxw3MWL+JGltBgVtGRSSoORVOAE8Xi5zVU7rWfDCOaV'
      && 'Gu59mLz0dTPUBg95eYjVK2VdOzjBW9YZL5FT4i2k5+YoKcY5VPsQkoumOLsu1mjFHx8o'
      && 'ahA3YV7OfwAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

    ls_image-url     = 'img/star-grey' ##NO_TEXT.
    ls_image-content =
         'iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAilBMVEUAAADQ0NDQ0NDQ'
      && '0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ'
      && '0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ'
      && '0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NDQ0NC2QdifAAAALXRSTlMAAQIFBwkKCw0QERUY'
      && 'HB4jLzEzNjg7PVdYYmRvd3mDm52eub7R0+Tr8fX3+/16wo8zAAAAcElEQVQYGW3BBxKC'
      && 'MABFwYcQETv2hg1UVP79ryeTZBxw3MWL+JGltBgVtGRSSoORVOAE8Xi5zVU7rWfDCOaV'
      && 'Gu59mLz0dTPUBg95eYjVK2VdOzjBW9YZL5FT4i2k5+YoKcY5VPsQkoumOLsu1mjFHx8o'
      && 'ahA3YV7OfwAAAABJRU5ErkJggg=='.
    APPEND ls_image TO rt_assets.

  ENDMETHOD.  "get_assets

ENDCLASS.