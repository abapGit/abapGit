*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_HTML_CHUNKS
*&---------------------------------------------------------------------*

CLASS lcl_gui_chunk_lib DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS render_error
        IMPORTING ix_error       TYPE REF TO lcx_exception OPTIONAL
                  iv_error       TYPE string OPTIONAL
        RETURNING VALUE(ro_html) TYPE REF TO lcl_html.

    CLASS-METHODS render_repo_top
      IMPORTING io_repo               TYPE REF TO lcl_repo
                iv_show_package       TYPE abap_bool DEFAULT abap_true
                iv_show_branch        TYPE abap_bool DEFAULT abap_true
                iv_interactive_branch TYPE abap_bool DEFAULT abap_false
                iv_branch             TYPE string OPTIONAL
      RETURNING VALUE(ro_html)        TYPE REF TO lcl_html
      RAISING   lcx_exception.

    CLASS-METHODS render_item_state
        IMPORTING iv1            TYPE char1
                  iv2            TYPE char1
        RETURNING VALUE(rv_html) TYPE string.

    CLASS-METHODS render_branch_span
      IMPORTING iv_branch             TYPE string
                io_repo               TYPE REF TO lcl_repo_online
                iv_interactive        TYPE abap_bool
      RETURNING VALUE(ro_html)        TYPE REF TO lcl_html
      RAISING   lcx_exception.

ENDCLASS. "lcl_gui_chunk_lib

CLASS lcl_gui_chunk_lib IMPLEMENTATION.

  METHOD render_repo_top.

    DATA: lo_repo_online TYPE REF TO lcl_repo_online,
          lo_pback       TYPE REF TO lcl_persistence_background,
          lv_hint        TYPE string,
          lv_icon        TYPE string.


    CREATE OBJECT ro_html.
    CREATE OBJECT lo_pback.

    IF io_repo->is_offline( ) = abap_true.
      lv_icon = 'plug/darkgrey' ##NO_TEXT.
      lv_hint = 'Offline repository' ##NO_TEXT.
    ELSE.
      lv_icon = 'cloud-upload/blue' ##NO_TEXT.
      lv_hint = 'On-line repository' ##NO_TEXT.
    ENDIF.

    ro_html->add( '<table class="w100"><tr>' ).

    ro_html->add( '<td class="repo_name">' ).
    ro_html->add_icon( iv_name = lv_icon  iv_hint = lv_hint ).
    ro_html->add( |<span class="name">{ io_repo->get_name( ) }</span>| ).
    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.
      ro_html->add( |<span class="url">{ lo_repo_online->get_url( ) }</span>| ).
    ENDIF.
    ro_html->add( '</td>' ).

    ro_html->add( '<td class="repo_attr right">' ).

    IF abap_true = lcl_app=>user( )->is_favorite_repo( io_repo->get_key( ) ).
      lv_icon = 'star/blue' ##NO_TEXT.
    ELSE.
      lv_icon = 'star/grey' ##NO_TEXT.
    ENDIF.
    ro_html->add_a( iv_act = |{ gc_action-repo_toggle_fav }?{ io_repo->get_key( ) }|
                    iv_txt = lcl_html=>icon( iv_name  = lv_icon
                                             iv_class = 'pad-sides'
                                             iv_hint  = 'Click to toggle favorite' ) ).

    IF lo_pback->exists( io_repo->get_key( ) ) = abap_true.
      ro_html->add( '<span class="bg_marker" title="background">BG</span>' ).
    ENDIF.

    IF io_repo->is_write_protected( ) = abap_true.
      ro_html->add_icon( iv_name = 'lock/darkgrey' iv_hint = 'Locked from pulls' ).
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
      ro_html->add_icon( iv_name = 'package/darkgrey' iv_hint = 'SAP package' ).
      ro_html->add( '<span>' ).
      ro_html->add_a( iv_txt = io_repo->get_package( )
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

    IF iv_branch    = io_repo->get_head_branch_name( )
       OR iv_branch = lcl_git_branch_list=>c_head_name.
      lv_class = 'branch branch_head'.
    ELSEIF lcl_git_branch_list=>get_type( iv_branch ) = lcl_git_branch_list=>c_type-branch.
      lv_class = 'branch branch_branch'.
    ELSE.
      lv_class = 'branch'.
    ENDIF.

    CREATE OBJECT ro_html.
    ro_html->add( |<span class="{ lv_class }">| ).
    ro_html->add_icon( iv_name = 'git-branch/darkgrey' iv_hint = 'Current branch' ).
    IF iv_interactive = abap_true.
      ro_html->add_a( iv_act = |{ gc_action-git_branch_switch }?{ io_repo->get_key( ) }|
                      iv_txt = lv_text ).
    ELSE.
      ro_html->add( lv_text ).
    ENDIF.
    ro_html->add( '</span>' ).

  ENDMETHOD.  "render_branch_span

  METHOD render_error.

    DATA lv_error TYPE string.

    CREATE OBJECT ro_html.

    IF ix_error IS BOUND.
      lv_error = ix_error->mv_text.
    ELSE.
      lv_error = iv_error.
    ENDIF.

    ro_html->add( '<div class="dummydiv attention">' ).
    ro_html->add( |{ lcl_html=>icon( 'alert/red' ) } Error: { lv_error }| ).
    ro_html->add( '</div>' ).

  ENDMETHOD. "render_error

ENDCLASS. "lcl_gui_chunk_lib