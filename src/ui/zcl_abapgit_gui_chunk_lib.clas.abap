CLASS zcl_abapgit_gui_chunk_lib DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_event_signature,
        method TYPE string,
        name   TYPE string,
      END OF  ty_event_signature.

    CLASS-METHODS class_constructor.
    CLASS-METHODS render_error
      IMPORTING
        !ix_error      TYPE REF TO zcx_abapgit_exception OPTIONAL
        !iv_error      TYPE string OPTIONAL
        !iv_extra_style TYPE string OPTIONAL
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
    CLASS-METHODS render_commit_popup
      IMPORTING
        iv_content     TYPE csequence
        iv_id          TYPE csequence
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_error_message_box
      IMPORTING
        ix_error       TYPE REF TO zcx_abapgit_exception
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    CLASS-METHODS parse_change_order_by
      IMPORTING
        iv_query_str       TYPE clike
      RETURNING
        VALUE(rv_order_by) TYPE string.
    CLASS-METHODS parse_direction
      IMPORTING
        iv_query_str               TYPE clike
      RETURNING
        VALUE(rv_order_descending) TYPE abap_bool.
    CLASS-METHODS render_order_by_header_cells
      IMPORTING
        it_col_spec         TYPE zif_abapgit_definitions=>tty_col_spec
        iv_order_by         TYPE string
        iv_order_descending TYPE abap_bool
      RETURNING
        VALUE(ro_html)      TYPE REF TO zcl_abapgit_html.
    CLASS-METHODS render_warning_banner
      IMPORTING
        iv_text        TYPE string
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    CLASS-METHODS render_infopanel
      IMPORTING
        iv_div_id      TYPE string
        iv_title       TYPE string
        iv_hide        TYPE abap_bool DEFAULT abap_true
        iv_hint        TYPE string OPTIONAL
        iv_scrollable  TYPE abap_bool DEFAULT abap_true
        io_content     TYPE REF TO zif_abapgit_html
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_event_as_form
      IMPORTING
        is_event       TYPE ty_event_signature
      RETURNING
        VALUE(ro_html) TYPE REF TO zcl_abapgit_html.
    CLASS-METHODS render_repo_palette
      IMPORTING
        iv_action TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS advanced_submenu
      RETURNING VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.

    CLASS-METHODS help_submenu
      RETURNING VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA gv_time_zone TYPE timezone.

    CLASS-METHODS render_branch_span
      IMPORTING
        !iv_branch      TYPE string
        !io_repo        TYPE REF TO zcl_abapgit_repo_online
        !iv_interactive TYPE abap_bool
      RETURNING
        VALUE(ro_html)  TYPE REF TO zcl_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS get_t100_text
      IMPORTING
        iv_msgid       TYPE scx_t100key-msgid
        iv_msgno       TYPE scx_t100key-msgno
      RETURNING
        VALUE(rv_text) TYPE string.
    CLASS-METHODS normalize_program_name
      IMPORTING
        iv_program_name                   TYPE sy-repid
      RETURNING
        VALUE(rv_normalized_program_name) TYPE string.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_CHUNK_LIB IMPLEMENTATION.


  METHOD advanced_submenu.

    CREATE OBJECT ro_menu.

    ro_menu->add(
      iv_txt = 'Database util'
      iv_act = zif_abapgit_definitions=>c_action-go_db
    )->add(
      iv_txt = 'Package to zip'
      iv_act = zif_abapgit_definitions=>c_action-zip_package
    )->add(
      iv_txt = 'Transport to zip'
      iv_act = zif_abapgit_definitions=>c_action-zip_transport
    )->add(
      iv_txt = 'Object to files'
      iv_act = zif_abapgit_definitions=>c_action-zip_object
    )->add(
      iv_txt = 'Test changed by'
      iv_act = zif_abapgit_definitions=>c_action-changed_by
    )->add(
      iv_txt = 'Debug info'
      iv_act = zif_abapgit_definitions=>c_action-go_debuginfo
    )->add(
      iv_txt = 'Settings'
      iv_act = zif_abapgit_definitions=>c_action-go_settings ).

  ENDMETHOD.


  METHOD class_constructor.

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = gv_time_zone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD get_t100_text.

    SELECT SINGLE text
           FROM t100
           INTO rv_text
           WHERE arbgb = iv_msgid
           AND msgnr = iv_msgno
           AND sprsl = sy-langu.

  ENDMETHOD.


  METHOD help_submenu.

    CREATE OBJECT ro_menu.

    ro_menu->add(
      iv_txt = 'Tutorial'
      iv_act = zif_abapgit_definitions=>c_action-go_tutorial
    )->add(
      iv_txt = 'Documentation'
      iv_act = zif_abapgit_definitions=>c_action-documentation
    )->add(
      iv_txt = 'Explore'
      iv_act = zif_abapgit_definitions=>c_action-go_explore
    )->add(
      iv_txt = 'Changelog'
      iv_act = zif_abapgit_definitions=>c_action-changelog ).

  ENDMETHOD.


  METHOD normalize_program_name.

    rv_normalized_program_name = substring_before(
                                     val   = iv_program_name
                                     regex = `(=+CP)?$` ).

  ENDMETHOD.


  METHOD parse_change_order_by.

    FIND FIRST OCCURRENCE OF REGEX `orderBy=(.*)`
         IN iv_query_str
         SUBMATCHES rv_order_by.

    rv_order_by = condense( rv_order_by ).

  ENDMETHOD.


  METHOD parse_direction.

    DATA: lv_direction TYPE string.

    FIND FIRST OCCURRENCE OF REGEX `direction=(.*)`
         IN iv_query_str
         SUBMATCHES lv_direction.

    rv_order_descending = boolc( condense( lv_direction ) = 'DESCENDING' ).

  ENDMETHOD.


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
    ro_html->add_icon( iv_name = 'code-branch/grey70'
                       iv_hint = 'Current branch' ).
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
    ro_html->add( |<li>| && |<span>{ iv_content }</span>| && |</li>| ).
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
    DATA lv_class TYPE string VALUE 'panel error center'.

    IF iv_extra_style IS NOT INITIAL.
      lv_class = lv_class && ` ` && iv_extra_style.
    ENDIF.

    CREATE OBJECT ro_html.

    IF ix_error IS BOUND.
      lv_error = ix_error->get_text( ).
    ELSE.
      lv_error = iv_error.
    ENDIF.

    ro_html->add( |<div class="{ lv_class }">| ).
    ro_html->add( |{ zcl_abapgit_html=>icon( 'exclamation-circle/red' ) } Error: { lv_error }| ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_error_message_box.

    DATA:
      lv_error_text   TYPE string,
      lv_longtext     TYPE string,
      lv_program_name TYPE sy-repid,
      lv_title        TYPE string,
      lv_text         TYPE string.


    CREATE OBJECT ro_html.

    lv_error_text = ix_error->get_text( ).
    lv_longtext = ix_error->get_longtext( abap_true ).

    REPLACE FIRST OCCURRENCE OF REGEX |(<br>{ zcl_abapgit_message_helper=>gc_section_text-cause }<br>)|
            IN lv_longtext
            WITH |<h3>$1</h3>|.

    REPLACE FIRST OCCURRENCE OF REGEX |(<br>{ zcl_abapgit_message_helper=>gc_section_text-system_response }<br>)|
            IN lv_longtext
            WITH |<h3>$1</h3>|.

    REPLACE FIRST OCCURRENCE OF REGEX |(<br>{ zcl_abapgit_message_helper=>gc_section_text-what_to_do }<br>)|
            IN lv_longtext
            WITH |<h3>$1</h3>|.

    REPLACE FIRST OCCURRENCE OF REGEX |(<br>{ zcl_abapgit_message_helper=>gc_section_text-sys_admin }<br>)|
            IN lv_longtext
            WITH |<h3>$1</h3>|.

    ro_html->add( |<div id="message" class="message-panel">| ).
    ro_html->add( |{ lv_error_text }| ).
    ro_html->add( |<div class="float-right">| ).

    ro_html->add_a(
        iv_txt   = `&#x274c;`
        iv_act   = `toggleDisplay('message')`
        iv_class = `close-btn`
        iv_typ   = zif_abapgit_html=>c_action_type-onclick ).

    ro_html->add( |</div>| ).

    ro_html->add( |<div class="float-right message-panel-commands">| ).

    IF ix_error->if_t100_message~t100key-msgid IS NOT INITIAL.

      lv_title = get_t100_text(
                    iv_msgid = ix_error->if_t100_message~t100key-msgid
                    iv_msgno = ix_error->if_t100_message~t100key-msgno ).

      lv_text = |Message ({ ix_error->if_t100_message~t100key-msgid }/{ ix_error->if_t100_message~t100key-msgno })|.

      ro_html->add_a(
          iv_txt   = lv_text
          iv_typ   = zif_abapgit_html=>c_action_type-sapevent
          iv_act   = zif_abapgit_definitions=>c_action-goto_message
          iv_title = lv_title
          iv_id    = `a_goto_message` ).

    ENDIF.

    ix_error->get_source_position( IMPORTING program_name = lv_program_name ).

    lv_title = normalize_program_name( lv_program_name ).

    ro_html->add_a(
        iv_txt   = `Goto source`
        iv_act   = zif_abapgit_definitions=>c_action-goto_source
        iv_typ   = zif_abapgit_html=>c_action_type-sapevent
        iv_title = lv_title
        iv_id    = `a_goto_source` ).

    ro_html->add_a(
        iv_txt = `Callstack`
        iv_act = zif_abapgit_definitions=>c_action-show_callstack
        iv_typ = zif_abapgit_html=>c_action_type-sapevent
        iv_id  = `a_callstack` ).

    ro_html->add( |</div>| ).
    ro_html->add( |<div class="message-panel-commands">| ).
    ro_html->add( |{ lv_longtext }| ).
    ro_html->add( |</div>| ).
    ro_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_event_as_form.

    CREATE OBJECT ro_html.
    ro_html->add(
      |<form id='form_{ is_event-name }' method={ is_event-method } action='sapevent:{ is_event-name }'></form>| ).

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
        && zcl_abapgit_html=>icon( iv_name = 'exclamation-triangle'
                                   iv_class = 'pad-right' )
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


  METHOD render_order_by_header_cells.

    DATA:
      lt_colspec   TYPE zif_abapgit_definitions=>tty_col_spec,
      lv_tmp       TYPE string,
      lv_disp_name TYPE string.

    FIELD-SYMBOLS <ls_col> LIKE LINE OF lt_colspec.

    CREATE OBJECT ro_html.

    LOOP AT it_col_spec ASSIGNING <ls_col>.
      " e.g. <th class="ro-detail">Created at [{ gv_time_zone }]</th>
      lv_tmp = '<th'.
      IF <ls_col>-css_class IS NOT INITIAL.
        lv_tmp = lv_tmp && | class="{ <ls_col>-css_class }"|.
      ENDIF.
      lv_tmp = lv_tmp && '>'.

      IF <ls_col>-display_name IS NOT INITIAL.
        lv_disp_name = <ls_col>-display_name.
        IF <ls_col>-add_tz = abap_true.
          lv_disp_name = lv_disp_name && | [{ gv_time_zone }]|.
        ENDIF.
        IF <ls_col>-tech_name = iv_order_by.
          IF iv_order_descending = abap_true.
            lv_tmp = lv_tmp && zcl_abapgit_html=>a(
              iv_txt   = lv_disp_name
              iv_act   = |{ zif_abapgit_definitions=>c_action-direction }?direction=ASCENDING|
              iv_title = <ls_col>-title ).
          ELSE.
            lv_tmp = lv_tmp && zcl_abapgit_html=>a(
              iv_txt   = lv_disp_name
              iv_act   = |{ zif_abapgit_definitions=>c_action-direction }?direction=DESCENDING|
              iv_title = <ls_col>-title ).
          ENDIF.
        ELSE.
          lv_tmp = lv_tmp && zcl_abapgit_html=>a(
            iv_txt   = lv_disp_name
            iv_act   = |{ zif_abapgit_definitions=>c_action-change_order_by }?orderBy={ <ls_col>-tech_name }|
            iv_title = <ls_col>-title ).
        ENDIF.
      ENDIF.
      IF <ls_col>-tech_name = iv_order_by
      AND iv_order_by IS NOT INITIAL.
        IF iv_order_descending = abap_true.
          lv_tmp = lv_tmp && | &#x25B4;|. " arrow up
        ELSE.
          lv_tmp = lv_tmp && | &#x25BE;|. " arrow down
        ENDIF.
      ENDIF.

      lv_tmp = lv_tmp && '</th>'.
      ro_html->add( lv_tmp ).
    ENDLOOP.

  ENDMETHOD.


  METHOD render_repo_palette.

    DATA li_repo_srv TYPE REF TO zif_abapgit_repo_srv.
    DATA lt_repo_list TYPE zif_abapgit_persistence=>tt_repo.
    DATA lv_repo_json TYPE string.
    DATA lv_size TYPE i.
    DATA lo_repo TYPE REF TO zcl_abapgit_repo.
    FIELD-SYMBOLS <ls_repo> LIKE LINE OF lt_repo_list.

    li_repo_srv = zcl_abapgit_repo_srv=>get_instance( ).
    lt_repo_list = zcl_abapgit_persist_factory=>get_repo( )->list( ).
    lv_size = lines( lt_repo_list ).

    ri_html = zcl_abapgit_html=>create( ).

    ri_html->add( 'var repoCatalog = [' ). " Maybe separate this into another method if needed in more places
    LOOP AT lt_repo_list ASSIGNING <ls_repo>.
      lo_repo = li_repo_srv->get( <ls_repo>-key ). " inefficient
      lv_repo_json = |\{ key: "{ <ls_repo>-key
        }", isOffline: "{ <ls_repo>-offline
        }", displayName: "{ lo_repo->get_name( ) }"  \}|.
      IF sy-tabix < lv_size.
        lv_repo_json = lv_repo_json && ','.
      ENDIF.
      ri_html->add( lv_repo_json ).
    ENDLOOP.
    ri_html->add( '];' ).

    ri_html->add( |var gGoRepoPalette = new CommandPalette(createRepoCatalogEnumerator(repoCatalog, "{
      iv_action }"), \{| ).
    ri_html->add( '  toggleKey: "F2",' ).
    ri_html->add( '  hotkeyDescription: "Go to repo ..."' ).
    ri_html->add( '});' ).

  ENDMETHOD.


  METHOD render_repo_top.

    DATA: lo_repo_online       TYPE REF TO zcl_abapgit_repo_online,
          lo_pback             TYPE REF TO zcl_abapgit_persist_background,
          lv_hint              TYPE string,
          lv_icon              TYPE string,
          lv_package_jump_data TYPE string.

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
    ro_html->add_icon( iv_name = lv_icon
                       iv_hint = lv_hint ).
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
      ro_html->add_icon( iv_name = 'lock/grey70'
                         iv_hint = 'Locked from pulls' ).
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
      ro_html->add_icon( iv_name = 'box/grey70'
                         iv_hint = 'SAP package' ).
      ro_html->add( '<span>' ).

      lv_package_jump_data = zcl_abapgit_html_action_utils=>jump_encode(
        iv_obj_type = 'DEVC'
        iv_obj_name = io_repo->get_package( ) ).

      ro_html->add_a( iv_txt = io_repo->get_package( )
                      iv_act = |{ zif_abapgit_definitions=>c_action-jump }?{ lv_package_jump_data }| ).
      ro_html->add( '</span>' ).
    ENDIF.

    ro_html->add( '</td>' ).
    ro_html->add( '</tr></table>' ).

  ENDMETHOD.


  METHOD render_warning_banner.

    CREATE OBJECT ro_html.
    ro_html->add( '<div class="dummydiv warning">' ).
    ro_html->add( |{ zcl_abapgit_html=>icon( 'exclamation-triangle/yellow' ) }| && | { iv_text }| ).
    ro_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
