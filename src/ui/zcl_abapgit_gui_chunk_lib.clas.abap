CLASS zcl_abapgit_gui_chunk_lib DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS render_error
      IMPORTING
        !ix_error      TYPE REF TO zcx_abapgit_exception OPTIONAL
        !iv_error      TYPE string OPTIONAL
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html .
    CLASS-METHODS render_repo_top
      IMPORTING
        !io_repo               TYPE REF TO zcl_abapgit_repo
        !iv_show_package       TYPE abap_bool DEFAULT abap_true
        !iv_show_branch        TYPE abap_bool DEFAULT abap_true
        !iv_interactive_branch TYPE abap_bool DEFAULT abap_false
        !iv_branch             TYPE string OPTIONAL
        !io_news               TYPE REF TO zcl_abapgit_news OPTIONAL
      RETURNING
        VALUE(ro_html)         TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_item_state
      IMPORTING
        !iv_lstate     TYPE char1
        !iv_rstate     TYPE char1
      RETURNING
        VALUE(rv_html) TYPE string .
    CLASS-METHODS render_js_error_banner
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_news
      IMPORTING
        !io_news       TYPE REF TO zcl_abapgit_news
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_hotkey_overview
      IMPORTING
        !io_page       TYPE REF TO zcl_abapgit_gui_page
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_commit_popup
      IMPORTING
        !iv_content    TYPE csequence
        !iv_id         TYPE csequence
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS render_branch_span
      IMPORTING
        !iv_branch      TYPE string
        !io_repo        TYPE REF TO zcl_abapgit_repo_online
        !iv_interactive TYPE abap_bool
      RETURNING
        VALUE(ro_html)  TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_infopanel
      IMPORTING
        !iv_div_id     TYPE string
        !iv_title      TYPE string
        !iv_hide       TYPE abap_bool DEFAULT abap_true
        !iv_hint       TYPE string OPTIONAL
        !iv_scrollable TYPE abap_bool DEFAULT abap_true
        !io_content    TYPE REF TO zcl_abapgit_html
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_CHUNK_LIB IMPLEMENTATION.


  METHOD render_branch_span.

    DATA: lv_text  TYPE string,
          lv_class TYPE string.

    lv_text = zcl_abapgit_git_branch_list=>get_display_name( iv_branch ).

    IF zcl_abapgit_git_branch_list=>get_type( iv_branch ) = zif_abapgit_definitions=>c_git_branch_type-branch.
      lv_class = 'branch branch_branch'.
    ELSE.
      lv_class = 'branch'.
    ENDIF.

    CREATE OBJECT ro_html.
    ro_html->add( |<span class="{ lv_class }">| ).
    ro_html->add_icon( iv_name = 'code-branch/grey70' iv_hint = 'Current branch' ).
    IF iv_interactive = abap_true.
      ro_html->add_a( iv_act = |{ zif_abapgit_definitions=>c_action-git_branch_switch }?{ io_repo->get_key( ) }|
                      iv_txt = lv_text ).
    ELSE.
      ro_html->add( lv_text ).
    ENDIF.
    ro_html->add( '</span>' ).

  ENDMETHOD.


  METHOD render_commit_popup.

    CREATE OBJECT ro_html.

    ro_html->add( '<ul class="hotkeys">' ).
    ro_html->add( |<li>|
      && |<span>{ iv_content }</span>|
      && |</li>| ).
    ro_html->add( '</ul>' ).

    ro_html = render_infopanel(
      iv_div_id     = |{ iv_id }|
      iv_title      = 'Commit details'
      iv_hide       = abap_true
      iv_scrollable = abap_false
      io_content    = ro_html ).

  ENDMETHOD.


  METHOD render_error.

    DATA lv_error TYPE string.

    CREATE OBJECT ro_html.

    IF ix_error IS BOUND.
      lv_error = ix_error->get_text( ).
    ELSE.
      lv_error = iv_error.
    ENDIF.

    ro_html->add( '<div class="dummydiv error">' ).
    ro_html->add( |{ zcl_abapgit_html=>icon( 'exclamation-circle/red' ) } Error: { lv_error }| ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_hotkey_overview.

    DATA: lv_hint     TYPE string,
          lt_hotkeys  TYPE zif_abapgit_definitions=>tty_hotkey,
          lt_actions  TYPE zif_abapgit_gui_page_hotkey=>tty_hotkey_action,
          lo_settings TYPE REF TO zcl_abapgit_settings.

    FIELD-SYMBOLS: <ls_hotkey> TYPE zif_abapgit_definitions=>ty_hotkey,
                   <ls_action> LIKE LINE OF lt_actions.

    lo_settings = zcl_abapgit_persist_settings=>get_instance( )->read( ).
    lt_hotkeys  = lo_settings->get_hotkeys( ).
    lt_actions  = zcl_abapgit_hotkeys=>get_default_hotkeys_from_pages( io_page ).

    CREATE OBJECT ro_html.

    " Render hotkeys
    ro_html->add( '<ul class="hotkeys">' ).
    LOOP AT lt_hotkeys ASSIGNING <ls_hotkey>.

      READ TABLE lt_actions ASSIGNING <ls_action>
                            WITH TABLE KEY action
                            COMPONENTS action = <ls_hotkey>-action.
      IF sy-subrc = 0.
        ro_html->add( |<li>|
          && |<span class="key-id">{ <ls_hotkey>-sequence }</span>|
          && |<span class="key-descr">{ <ls_action>-name }</span>|
          && |</li>| ).
      ENDIF.

    ENDLOOP.
    ro_html->add( '</ul>' ).

    " Wrap
    READ TABLE lt_hotkeys ASSIGNING <ls_hotkey>
      WITH KEY action = zcl_abapgit_gui_page=>c_global_page_action-showhotkeys.
    IF sy-subrc = 0.
      lv_hint = |Close window with '{ <ls_hotkey>-sequence }' or upper right corner 'X'|.
    ENDIF.

    ro_html = render_infopanel(
      iv_div_id     = 'hotkeys'
      iv_title      = 'Hotkeys'
      iv_hint       = lv_hint
      iv_hide       = abap_true
      iv_scrollable = abap_false
      io_content    = ro_html ).

    IF <ls_hotkey> IS ASSIGNED AND zcl_abapgit_hotkeys=>should_show_hint( ) = abap_true.
      ro_html->add( |<div id="hotkeys-hint" class="corner-hint">|
        && |Press '{ <ls_hotkey>-sequence }' to get keyboard shortcuts list|
        && |</div>| ).
    ENDIF.

  ENDMETHOD.


  METHOD render_infopanel.

    DATA lv_display TYPE string.
    DATA lv_class TYPE string.

    CREATE OBJECT ro_html.

    IF iv_hide = abap_true. " Initially hide
      lv_display = 'display:none'.
    ENDIF.

    lv_class = 'info-panel'.
    IF iv_scrollable = abap_false. " Initially hide
      lv_class = lv_class && ' info-panel-fixed'.
    ENDIF.

    ro_html->add( |<div id="{ iv_div_id }" class="{ lv_class }" style="{ lv_display }">| ).

    ro_html->add( |<div class="info-title">{ iv_title }|
               && '<div class="float-right">'
               && zcl_abapgit_html=>a(
                    iv_txt   = '&#x274c;'
                    iv_typ   = zif_abapgit_html=>c_action_type-onclick
                    iv_act   = |toggleDisplay('{ iv_div_id }')|
                    iv_class = 'close-btn' )
               && '</div></div>' ).

    IF iv_hint IS NOT INITIAL.
      ro_html->add( '<div class="info-hint">'
        && zcl_abapgit_html=>icon( iv_name = 'exclamation-triangle' iv_class = 'pad-right' )
        && iv_hint
        && '</div>' ).
    ENDIF.

    ro_html->add( |<div class="info-list">| ).
    ro_html->add( io_content ).
    ro_html->add( '</div>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_item_state.

    DATA: lv_system TYPE string.

    FIELD-SYMBOLS <lv_state> TYPE char1.


    rv_html = '<span class="state-block">'.

    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          ASSIGN iv_lstate TO <lv_state>.
          lv_system = 'Local:'.
        WHEN 2.
          ASSIGN iv_rstate TO <lv_state>.
          lv_system = 'Remote:'.
      ENDCASE.

      CASE <lv_state>.
        WHEN zif_abapgit_definitions=>c_state-unchanged.  "None or unchanged
          IF iv_lstate = zif_abapgit_definitions=>c_state-added OR iv_rstate = zif_abapgit_definitions=>c_state-added.
            rv_html = rv_html && |<span class="none" title="{ lv_system } Not exists">X</span>|.
          ELSE.
            rv_html = rv_html && |<span class="none" title="{ lv_system } No changes">&nbsp;</span>|.
          ENDIF.
        WHEN zif_abapgit_definitions=>c_state-modified.   "Changed
          rv_html = rv_html && |<span class="changed" title="{ lv_system } Modified">M</span>|.
        WHEN zif_abapgit_definitions=>c_state-added.      "Added new
          rv_html = rv_html && |<span class="added" title="{ lv_system } Added new">A</span>|.
        WHEN zif_abapgit_definitions=>c_state-mixed.      "Multiple changes (multifile)
          rv_html = rv_html && |<span class="mixed" title="{ lv_system } Multiple changes">&#x25A0;</span>|.
        WHEN zif_abapgit_definitions=>c_state-deleted.    "Deleted
          rv_html = rv_html && |<span class="deleted" title="{ lv_system } Deleted">D</span>|.
      ENDCASE.
    ENDDO.

    rv_html = rv_html && '</span>'.

  ENDMETHOD.


  METHOD render_js_error_banner.
    CREATE OBJECT ro_html.
    ro_html->add( '<div id="js-error-banner" class="dummydiv error">' ).
    ro_html->add( |{ zcl_abapgit_html=>icon( 'exclamation-triangle/red' ) }| &&
                  ' If this does not disappear soon,' &&
                  ' then there is a JS init error, please log an issue' ).
    ro_html->add( '</div>' ).
  ENDMETHOD.


  METHOD render_news.

    DATA: lv_text TYPE string,
          lv_hint TYPE string,
          lt_log  TYPE zcl_abapgit_news=>tt_log.

    FIELD-SYMBOLS: <ls_line> LIKE LINE OF lt_log.

    CREATE OBJECT ro_html.

    IF io_news IS NOT BOUND OR io_news->has_news( ) = abap_false.
      RETURN.
    ENDIF.

    lt_log = io_news->get_log( ).

    " Render news
    LOOP AT lt_log ASSIGNING <ls_line>.
      IF <ls_line>-is_header = abap_true.
        IF <ls_line>-pos_to_cur > 0.
          lv_text = <ls_line>-text && '<span class="version-marker update">update</span>'.
        ELSEIF <ls_line>-pos_to_cur = 0.
          lv_text = <ls_line>-text && '<span class="version-marker">current</span>'.
        ELSE. " < 0
          lv_text = <ls_line>-text.
        ENDIF.
        ro_html->add( |<h1>{ lv_text }</h1>| ).
      ELSE.
        ro_html->add( |<li>{ <ls_line>-text }</li>| ).
      ENDIF.
    ENDLOOP.

    " Wrap
    IF io_news->has_important( ) = abap_true.
      lv_hint = 'Please note changes marked with "!"'.
    ENDIF.

    ro_html = render_infopanel(
      iv_div_id  = 'news'
      iv_title   = 'Announcement of the latest changes'
      iv_hint    = lv_hint
      iv_hide    = boolc( io_news->has_unseen( ) = abap_false )
      io_content = ro_html ).

  ENDMETHOD.


  METHOD render_repo_top.

    DATA: lo_repo_online TYPE REF TO zcl_abapgit_repo_online,
          lo_pback       TYPE REF TO zcl_abapgit_persist_background,
          lv_hint        TYPE string,
          lv_icon        TYPE string.


    CREATE OBJECT ro_html.
    CREATE OBJECT lo_pback.

    IF io_repo->is_offline( ) = abap_true.
      lv_icon = 'plug/darkgrey' ##NO_TEXT.
      lv_hint = 'Offline repository' ##NO_TEXT.
    ELSE.
      lv_icon = 'cloud-upload-alt/blue' ##NO_TEXT.
      lv_hint = 'On-line repository' ##NO_TEXT.
    ENDIF.

    ro_html->add( '<table class="w100"><tr>' ).

    ro_html->add( '<td class="repo_name">' ).

    " Repo type and name
    ro_html->add_icon( iv_name = lv_icon  iv_hint = lv_hint ).
    ro_html->add( |<span class="name">{ io_repo->get_name( ) }</span>| ).
    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.

      ro_html->add_a( iv_txt   = lo_repo_online->get_url( )
                      iv_act   = |{ zif_abapgit_definitions=>c_action-url }?|
                              && |{ lo_repo_online->get_url( ) }|
                      iv_class = |url| ).

    ENDIF.

    " News
    IF io_news IS BOUND AND io_news->has_news( ) = abap_true.
      IF io_news->has_updates( ) = abap_true.
        lv_icon = 'arrow-circle-up/warning'.
      ELSE.
        lv_icon = 'arrow-circle-up/grey80'.
      ENDIF.
      ro_html->add_a( iv_act = |toggleDisplay('news')|
                      iv_typ = zif_abapgit_html=>c_action_type-onclick
                      iv_txt = zcl_abapgit_html=>icon( iv_name  = lv_icon
                                                       iv_class = 'pad-sides'
                                                       iv_hint  = 'Display changelog' ) ).
    ENDIF.
    ro_html->add( '</td>' ).

    ro_html->add( '<td class="repo_attr right">' ).

    " Fav
    IF abap_true = zcl_abapgit_persistence_user=>get_instance( )->is_favorite_repo( io_repo->get_key( ) ).
      lv_icon = 'star/blue' ##NO_TEXT.
    ELSE.
      lv_icon = 'star/grey' ##NO_TEXT.
    ENDIF.
    ro_html->add_a( iv_act = |{ zif_abapgit_definitions=>c_action-repo_toggle_fav }?{ io_repo->get_key( ) }|
                    iv_txt = zcl_abapgit_html=>icon( iv_name  = lv_icon
                                                     iv_class = 'pad-sides'
                                                     iv_hint  = 'Click to toggle favorite' ) ).

    " BG
    IF lo_pback->exists( io_repo->get_key( ) ) = abap_true.
      ro_html->add( '<span class="bg_marker" title="background">BG</span>' ).
    ENDIF.

    " Write protect
    IF io_repo->get_local_settings( )-write_protected = abap_true.
      ro_html->add_icon( iv_name = 'lock/grey70' iv_hint = 'Locked from pulls' ).
    ENDIF.

    " Branch
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

    " Package
    IF iv_show_package = abap_true.
      ro_html->add_icon( iv_name = 'box/grey70' iv_hint = 'SAP package' ).
      ro_html->add( '<span>' ).
      ro_html->add_a( iv_txt = io_repo->get_package( )
                      iv_act = |{ zif_abapgit_definitions=>c_action-jump_pkg }?{ io_repo->get_package( ) }| ).
      ro_html->add( '</span>' ).
    ENDIF.

    ro_html->add( '</td>' ).
    ro_html->add( '</tr></table>' ).

  ENDMETHOD.
ENDCLASS.
