*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_PAGE
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       INTERFACE lif_gui_page DEFINITION
*----------------------------------------------------------------------*
INTERFACE lif_gui_page.

  METHODS on_event
    IMPORTING iv_action    TYPE clike
              iv_prev_page TYPE clike
              iv_getdata   TYPE clike OPTIONAL
              it_postdata  TYPE cnht_post_data_tab OPTIONAL
    EXPORTING ei_page      TYPE REF TO lif_gui_page
              ev_state     TYPE i
    RAISING   lcx_exception lcx_cancel.

  METHODS render
    RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper
    RAISING   lcx_exception.

ENDINTERFACE.

CLASS lcl_gui_page_super DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_gui_page ABSTRACT METHODS render.

    CLASS-METHODS render_error
        IMPORTING ix_error       TYPE REF TO lcx_exception
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

  PROTECTED SECTION.
    METHODS render_repo_top
      IMPORTING io_repo               TYPE REF TO lcl_repo
                iv_show_package       TYPE abap_bool DEFAULT abap_true
                iv_show_branch        TYPE abap_bool DEFAULT abap_true
                iv_interactive_branch TYPE abap_bool DEFAULT abap_false
                iv_branch             TYPE string OPTIONAL
      RETURNING VALUE(ro_html)        TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS render_item_state
        IMPORTING iv1            TYPE char1
                  iv2            TYPE char1
        RETURNING VALUE(rv_html) TYPE string.

    METHODS render_branch_span
      IMPORTING iv_branch             TYPE string
                io_repo               TYPE REF TO lcl_repo_online
                iv_interactive        TYPE abap_bool
      RETURNING VALUE(ro_html)        TYPE REF TO lcl_html_helper
      RAISING   lcx_exception.

    METHODS header
      RETURNING VALUE(ro_html)   TYPE REF TO lcl_html_helper.

    METHODS footer
      IMPORTING io_include_script TYPE REF TO lcl_html_helper OPTIONAL
      RETURNING VALUE(ro_html)    TYPE REF TO lcl_html_helper.

    METHODS title
      IMPORTING iv_title       TYPE string
                io_menu        TYPE REF TO lcl_html_toolbar OPTIONAL
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

    METHODS redirect
      IMPORTING iv_url         TYPE string
      RETURNING VALUE(ro_html) TYPE REF TO lcl_html_helper.

ENDCLASS.

CLASS lcl_gui_page_super IMPLEMENTATION.

  METHOD render_repo_top.

    DATA: lo_repo_online TYPE REF TO lcl_repo_online,
          lo_pback       TYPE REF TO lcl_persistence_background,
          lv_icon        TYPE string.


    CREATE OBJECT ro_html.
    CREATE OBJECT lo_pback.

    IF io_repo->is_offline( ) = abap_true.
      lv_icon = 'img/repo_offline' ##NO_TEXT.
    ELSE.
      lv_icon = 'img/repo_online' ##NO_TEXT.
    ENDIF.

    ro_html->add( '<table width="100%"><tr>' ).

    ro_html->add( '<td class="repo_name">' ).
    ro_html->add( |<img src="{ lv_icon }">| ).
    ro_html->add( |<span class="name">{ io_repo->get_name( ) }</span>| ).
    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.
      ro_html->add( |<span class="url">{ lo_repo_online->get_url( ) }</span>| ).
    ENDIF.
    ro_html->add( '</td>' ).

    ro_html->add( '<td class="repo_attr right">' ).

    IF abap_true = lcl_app=>user( )->is_favorite_repo( io_repo->get_key( ) ).
      lv_icon = 'img/star' ##NO_TEXT.
    ELSE.
      lv_icon = 'img/star-grey' ##NO_TEXT.
    ENDIF.
    lv_icon = |<img class="pad-sides" src="{ lv_icon }" title="Click to toggle favorite">|.
    ro_html->add_anchor( iv_act = |{ gc_action-repo_toggle_fav }?{ io_repo->get_key( ) }|
                         iv_txt = lv_icon ).

    IF lo_pback->exists( io_repo->get_key( ) ) = abap_true.
      ro_html->add( '<span class="bg_marker" title="background">BG</span>' ).
    ENDIF.

    IF io_repo->is_write_protected( ) = abap_true.
      ro_html->add( '<img src="img/lock" title="locked">' ).
    ENDIF.

    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.
      IF iv_show_branch = abap_true.
        IF iv_branch IS INITIAL.
          ro_html->add( render_branch_span( iv_branch      = lo_repo_online->get_branch_name( )
                                            io_repo        = lo_repo_online
                                            iv_interactive = iv_interactive_branch ) ).
        ELSE.
          ro_html->add( render_branch_span( iv_branch      = iv_branch
                                            io_repo        = lo_repo_online
                                            iv_interactive = iv_interactive_branch ) ).
        ENDIF.
      ENDIF.
    ENDIF.

    IF iv_show_package = abap_true.
      ro_html->add( '<img src="img/pkg">' ).
      ro_html->add( '<span>' ).
      ro_html->add_anchor( iv_txt = io_repo->get_package( )
                           iv_act = |{ gc_action-jump_pkg }?{ io_repo->get_package( ) }| ).
      ro_html->add( '</span>' ).
    ENDIF.

    ro_html->add( '</td>' ).
    ro_html->add( '</tr></table>' ).

  ENDMETHOD.

  METHOD render_item_state.

    DATA: lv_system TYPE string.

    FIELD-SYMBOLS <state> TYPE char1.


    rv_html = '<span class="state-block">'.

    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          ASSIGN iv1 TO <state>.
          lv_system = 'Local:'.
        WHEN 2.
          ASSIGN iv2 TO <state>.
          lv_system = 'Remote:'.
      ENDCASE.

      CASE <state>.
        WHEN gc_state-unchanged.  "None or unchanged
          IF iv1 = gc_state-added OR iv2 = gc_state-added.
            rv_html = rv_html && |<span class="none" title="{ lv_system } Not exists">X</span>|.
          ELSE.
            rv_html = rv_html && |<span class="none" title="{ lv_system } No changes">&nbsp;</span>|.
          ENDIF.
        WHEN gc_state-modified.   "Changed
          rv_html = rv_html && |<span class="changed" title="{ lv_system } Modified">M</span>|.
        WHEN gc_state-added.      "Added new
          rv_html = rv_html && |<span class="added" title="{ lv_system } Added new">A</span>|.
        WHEN gc_state-mixed.      "Multiple changes (multifile)
          rv_html = rv_html && |<span class="mixed" title="{ lv_system } Multiple changes">&#x25A0;</span>|.
        WHEN gc_state-deleted.    "Deleted
          rv_html = rv_html && |<span class="deleted" title="{ lv_system } Deleted">D</span>|.
      ENDCASE.
    ENDDO.

    rv_html = rv_html && '</span>'.

  ENDMETHOD. "render_item_state

  METHOD render_branch_span.
    DATA: lv_text  TYPE string,
          lv_class TYPE string.

    lv_text = lcl_git_branch_list=>get_display_name( iv_branch ).

    IF iv_branch = io_repo->get_head_branch_name( ) OR iv_branch = lcl_git_branch_list=>head_name.
      lv_class = 'branch branch_head'.
    ELSEIF lcl_git_branch_list=>get_type( iv_branch ) = lcl_git_branch_list=>c_type-branch.
      lv_class = 'branch branch_branch'.
    ELSE.
      lv_class = 'branch'.
    ENDIF.

    CREATE OBJECT ro_html.
    ro_html->add( |<span class="{ lv_class }">| ).
    ro_html->add( '<img src="img/branch">' ).
    IF iv_interactive = abap_true.
      ro_html->add_anchor( iv_act = |{ gc_action-git_branch_switch }?{ io_repo->get_key( ) }|
                           iv_txt = lv_text ).
    ELSE.
      ro_html->add( lv_text ).
    ENDIF.
    ro_html->add( '</span>' ).

  ENDMETHOD.  "render_branch_span

  METHOD header.

    CREATE OBJECT ro_html.

    ro_html->add( '<!DOCTYPE html>' ).                      "#EC NOTEXT
    ro_html->add( '<html>' ).                               "#EC NOTEXT
    ro_html->add( '<head>' ).                               "#EC NOTEXT
    ro_html->add( '<title>abapGit</title>' ).               "#EC NOTEXT
    ro_html->add( '<link rel="stylesheet" type="text/css" href="css/common.css">' ).
    ro_html->add( '<meta http-equiv="content-type" content="text/html; charset=utf-8">' ). "#EC NOTEXT
    ro_html->add( '<meta http-equiv="X-UA-Compatible" content="IE=11,10,9,8" />' ). "#EC NOTEXT
    ro_html->add( '<script type="text/javascript" src="js/common.js"></script>' ). "#EC NOTEXT
    ro_html->add( '</head>' ).                              "#EC NOTEXT
    ro_html->add( '<body>' ).                               "#EC NOTEXT

  ENDMETHOD.                    "render html header

  METHOD title.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="header">' ).                    "#EC NOTEXT
    ro_html->add( '<table width="100%"><tr>' ).             "#EC NOTEXT

    ro_html->add( '<td class="logo">' ).                    "#EC NOTEXT
    ro_html->add( |<a href="sapevent:{ gc_action-abapgit_home }">| ). "#EC NOTEXT
    ro_html->add( '<img src="img/logo">' ).                 "#EC NOTEXT
    ro_html->add( '</a>' ).                                 "#EC NOTEXT
    ro_html->add( '</td>' ).                                "#EC NOTEXT

    ro_html->add( '<td class="headpad"><span class="page_title">' ). "#EC NOTEXT
    ro_html->add( |&#x25BA; { iv_title }| ).                "#EC NOTEXT
    ro_html->add( '</span></td>' ).                         "#EC NOTEXT

    IF io_menu IS BOUND.
      ro_html->add( '<td class="headpad right">' ).         "#EC NOTEXT
      ro_html->add( io_menu->render( ) ).
      ro_html->add( '</td>' ).                              "#EC NOTEXT
    ENDIF.

    ro_html->add( '</tr></table>' ).                        "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT

  ENDMETHOD.                    "render page title

  METHOD footer.

    CREATE OBJECT ro_html.

    ro_html->add( '<div id="footer">' ).                    "#EC NOTEXT
    ro_html->add( '<img src="img/logo" >' ).                "#EC NOTEXT
    ro_html->add( '<table width="100%"><tr><td width="40%"></td><td>' ). "#EC NOTEXT
    ro_html->add( |<span class="version">{ gc_abap_version }</span>| ). "#EC NOTEXT
    ro_html->add( '</td><td id="debug-output" width="40%"></td></tr></table>' ). "#EC NOTEXT
    ro_html->add( '</div>' ).                               "#EC NOTEXT
    ro_html->add( '</body>' ).                              "#EC NOTEXT

    IF io_include_script IS BOUND.
      ro_html->add( '<script type="text/javascript">' ).
      ro_html->add( io_include_script ).
      ro_html->add( 'debugOutput("js: OK");' ).
      ro_html->add( '</script>' ).
    ENDIF.

    ro_html->add( '</html>').                               "#EC NOTEXT

  ENDMETHOD.                    "render html footer & logo

  METHOD render_error.

    CREATE OBJECT ro_html.

    ro_html->add( '<div class="dummydiv attention">' ).
    ro_html->add( |Error: { ix_error->mv_text }| ).
    ro_html->add( '</div>' ).

  ENDMETHOD.  "render_error

  METHOD redirect.

    CREATE OBJECT ro_html.

    ro_html->add( '<!DOCTYPE html>' ).                      "#EC NOTEXT
    ro_html->add( '<html><head>' ).                         "#EC NOTEXT
    ro_html->add( |<meta http-equiv="refresh" content="0; url={ iv_url }">| ). "#EC NOTEXT
    ro_html->add( '</head></html>').                        "#EC NOTEXT

  ENDMETHOD.

  METHOD lif_gui_page~on_event.
    ev_state = gc_event_state-not_handled.
  ENDMETHOD.                    "lif_gui_page~on_event

ENDCLASS.