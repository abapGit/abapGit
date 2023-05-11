CLASS zcl_abapgit_gui_chunk_lib DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_event_signature,
        method TYPE string,
        name   TYPE string,
      END OF  ty_event_signature .

    CLASS-METHODS class_constructor .
    CLASS-METHODS render_error
      IMPORTING
        !ix_error       TYPE REF TO zcx_abapgit_exception OPTIONAL
        !iv_error       TYPE string OPTIONAL
        !iv_extra_style TYPE string OPTIONAL
      RETURNING
        VALUE(ri_html)  TYPE REF TO zif_abapgit_html .
    CLASS-METHODS render_repo_top
      IMPORTING
        !io_repo               TYPE REF TO zcl_abapgit_repo
        !iv_show_package       TYPE abap_bool DEFAULT abap_true
        !iv_show_branch        TYPE abap_bool DEFAULT abap_true
        !iv_show_commit        TYPE abap_bool DEFAULT abap_true
        !iv_show_edit          TYPE abap_bool DEFAULT abap_false
        !iv_interactive_branch TYPE abap_bool DEFAULT abap_false
        !io_news               TYPE REF TO zcl_abapgit_news OPTIONAL
      RETURNING
        VALUE(ri_html)         TYPE REF TO zif_abapgit_html
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
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_news
      IMPORTING
        !io_news       TYPE REF TO zcl_abapgit_news
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_commit_popup
      IMPORTING
        !iv_content    TYPE csequence
        !iv_id         TYPE csequence
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_error_message_box
      IMPORTING
        !ix_error      TYPE REF TO zcx_abapgit_exception
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    CLASS-METHODS render_order_by_header_cells
      IMPORTING
        !it_col_spec         TYPE zif_abapgit_definitions=>ty_col_spec_tt
        !iv_order_by         TYPE string
        !iv_order_descending TYPE abap_bool
      RETURNING
        VALUE(ri_html)       TYPE REF TO zif_abapgit_html .
    CLASS-METHODS render_warning_banner
      IMPORTING
        !iv_text       TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    CLASS-METHODS render_infopanel
      IMPORTING
        !iv_div_id     TYPE string
        !iv_title      TYPE string
        !iv_hide       TYPE abap_bool DEFAULT abap_true
        !iv_hint       TYPE string OPTIONAL
        !iv_scrollable TYPE abap_bool DEFAULT abap_true
        !io_content    TYPE REF TO zif_abapgit_html
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_event_as_form
      IMPORTING
        !is_event      TYPE ty_event_signature
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html .
    CLASS-METHODS render_repo_palette
      IMPORTING
        iv_action      TYPE string
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS advanced_submenu
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar .
    CLASS-METHODS help_submenu
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar .
    CLASS-METHODS back_toolbar
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar .
    CLASS-METHODS settings_toolbar
      IMPORTING
        !iv_act        TYPE string
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar .
    CLASS-METHODS settings_repo_toolbar
      IMPORTING
        !iv_key        TYPE zif_abapgit_persistence=>ty_repo-key
        !iv_act        TYPE string
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar .
    CLASS-METHODS render_branch_name
      IMPORTING
        !iv_branch      TYPE string OPTIONAL
        !iv_repo_key    TYPE zif_abapgit_persistence=>ty_value OPTIONAL
        !io_repo        TYPE REF TO zcl_abapgit_repo_online OPTIONAL
        !iv_interactive TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ri_html)  TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_repo_url
      IMPORTING
        iv_url                        TYPE zif_abapgit_persistence=>ty_repo-url
        iv_render_remote_edit_for_key TYPE zif_abapgit_persistence=>ty_repo-key OPTIONAL
      RETURNING
        VALUE(ri_html)                TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_package_name
      IMPORTING
        !iv_package        TYPE devclass
        !iv_interactive    TYPE abap_bool DEFAULT abap_true
        !iv_suppress_title TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_html)     TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_user_name
      IMPORTING
        !iv_username       TYPE syuname
        !iv_interactive    TYPE abap_bool DEFAULT abap_true
        !iv_icon_only      TYPE abap_bool DEFAULT abap_false
        !iv_suppress_title TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_html)     TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_transport
      IMPORTING
        !iv_transport   TYPE trkorr
        !iv_interactive TYPE abap_bool DEFAULT abap_true
        !iv_icon_only   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_html)  TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS render_sci_result
      IMPORTING
        ii_html       TYPE REF TO zif_abapgit_html
        iv_sci_result TYPE zif_abapgit_definitions=>ty_sci_result.

    CLASS-METHODS render_path
      IMPORTING
        !iv_path        TYPE string
        !iv_interactive TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ri_html)  TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .

    CLASS-METHODS render_timestamp
      IMPORTING
        iv_timestamp       TYPE timestampl
      RETURNING
        VALUE(rv_rendered) TYPE string.

    CLASS-METHODS render_text_input
      IMPORTING
        iv_name        TYPE string
        iv_label       TYPE string
        iv_value       TYPE string OPTIONAL
        iv_max_length  TYPE string OPTIONAL
        iv_autofocus   TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html.

    CLASS-METHODS shorten_repo_url
      IMPORTING
        iv_full_url         TYPE string
        iv_max_length       TYPE i DEFAULT 60
      RETURNING
        VALUE(rv_shortened) TYPE string.

    CLASS-METHODS render_label_list
      IMPORTING
        it_labels           TYPE string_table
        io_label_colors     TYPE REF TO zcl_abapgit_string_map
        iv_clickable_action TYPE string OPTIONAL
      RETURNING
        VALUE(rv_html)      TYPE string.

    CLASS-METHODS render_help_hint
      IMPORTING
        iv_text_to_wrap TYPE string
      RETURNING
        VALUE(rv_html)  TYPE string.

    CLASS-METHODS get_item_icon
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_repo_item
      RETURNING
        VALUE(rv_html) TYPE string.

    CLASS-METHODS get_item_link
      IMPORTING
        !is_item       TYPE zif_abapgit_definitions=>ty_repo_item
      RETURNING
        VALUE(rv_html) TYPE string.

  PROTECTED SECTION.

    CLASS-METHODS render_repo_top_commit_hash
      IMPORTING
        !ii_html        TYPE REF TO zif_abapgit_html
        !io_repo_online TYPE REF TO zcl_abapgit_repo_online
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    CLASS-DATA gv_time_zone TYPE timezone .

    CLASS-METHODS get_t100_text
      IMPORTING
        !iv_msgid      TYPE scx_t100key-msgid
        !iv_msgno      TYPE scx_t100key-msgno
      RETURNING
        VALUE(rv_text) TYPE string .
    CLASS-METHODS normalize_program_name
      IMPORTING
        !iv_program_name                  TYPE sy-repid
      RETURNING
        VALUE(rv_normalized_program_name) TYPE string .

ENDCLASS.



CLASS zcl_abapgit_gui_chunk_lib IMPLEMENTATION.


  METHOD advanced_submenu.

    DATA lv_supports_ie_devtools TYPE abap_bool.

    lv_supports_ie_devtools = zcl_abapgit_ui_factory=>get_frontend_services( )->is_sapgui_for_windows( ).

    CREATE OBJECT ro_menu.

    ro_menu->add(
      iv_txt = 'Database Utility'
      iv_act = zif_abapgit_definitions=>c_action-go_db
    )->add(
      iv_txt = 'Package to ZIP'
      iv_act = zif_abapgit_definitions=>c_action-zip_package
    )->add(
      iv_txt = 'Transport to ZIP'
      iv_act = zif_abapgit_definitions=>c_action-zip_transport
    )->add(
      iv_txt = 'Object to Files'
      iv_act = zif_abapgit_definitions=>c_action-zip_object
    )->add(
      iv_txt = 'Debug Info'
      iv_act = zif_abapgit_definitions=>c_action-go_debuginfo ).

    IF lv_supports_ie_devtools = abap_true.
      ro_menu->add(
        iv_txt = 'Open IE DevTools'
        iv_act = zif_abapgit_definitions=>c_action-ie_devtools ).
    ENDIF.

  ENDMETHOD.


  METHOD back_toolbar.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-back'.

    ro_menu->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD class_constructor.

    DATA lv_fm TYPE string.
    lv_fm = 'GET_SYSTEM_TIMEZONE'.

    TRY.
        CALL METHOD ('CL_ABAP_TSTMP')=>get_system_timezone
          RECEIVING
            system_timezone = gv_time_zone.
      CATCH cx_sy_dyn_call_illegal_method.
        CALL FUNCTION lv_fm
          IMPORTING
            timezone            = gv_time_zone
          EXCEPTIONS
            customizing_missing = 1
            OTHERS              = 2.
    ENDTRY.

  ENDMETHOD.


  METHOD get_item_icon.

    CASE is_item-obj_type.
      WHEN 'PROG' OR 'CLAS' OR 'FUGR' OR 'INTF' OR 'TYPE'.
        rv_html = zcl_abapgit_html=>icon( iv_name = 'file-code/darkgrey'
                                          iv_hint = 'Code' ).
      WHEN 'W3MI' OR 'W3HT' OR 'SFPF'.
        rv_html = zcl_abapgit_html=>icon( iv_name = 'file-image/darkgrey'
                                          iv_hint = 'Binary' ).
      WHEN 'DEVC'.
        rv_html = zcl_abapgit_html=>icon( iv_name = 'box/darkgrey'
                                          iv_hint = 'Package' ).
      WHEN ''.
        rv_html = space. " no icon
      WHEN OTHERS.
        rv_html = zcl_abapgit_html=>icon( 'file-alt/darkgrey' ).
    ENDCASE.

    IF is_item-is_dir = abap_true.
      rv_html = zcl_abapgit_html=>icon( iv_name = 'folder/darkgrey'
                                        iv_hint = 'Folder' ).
    ENDIF.

  ENDMETHOD.


  METHOD get_item_link.

    DATA lv_encode TYPE string.
    DATA li_html TYPE REF TO zif_abapgit_html.

    CREATE OBJECT li_html TYPE zcl_abapgit_html.

    lv_encode = zcl_abapgit_html_action_utils=>jump_encode(
      iv_obj_type = is_item-obj_type
      iv_obj_name = is_item-obj_name ).

    rv_html = li_html->a(
      iv_txt = |{ is_item-obj_name }|
      iv_act = |{ zif_abapgit_definitions=>c_action-jump }?{ lv_encode }| ).

  ENDMETHOD.


  METHOD get_t100_text.

    MESSAGE ID iv_msgid TYPE 'S' NUMBER iv_msgno WITH '&1' '&2' '&3' '&4' INTO rv_text.

    " Don't return any generic messages like `&1 &2 &3 &4`
    IF rv_text CO ' 0123456789&'.
      CLEAR rv_text.
    ENDIF.

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
      iv_act = zif_abapgit_definitions=>c_action-changelog
    )->add(
      iv_txt = 'Hotkeys'
      iv_act = zif_abapgit_definitions=>c_action-show_hotkeys ).

  ENDMETHOD.


  METHOD normalize_program_name.

    rv_normalized_program_name = substring_before(
      val   = iv_program_name
      regex = `(=+CP)?$` ).

  ENDMETHOD.


  METHOD render_branch_name.

    DATA:
      lv_key              TYPE string,
      lv_branch           TYPE string,
      lv_selected_commit  TYPE string,
      lv_commit_short_sha TYPE string,
      lv_text             TYPE string,
      lv_icon             TYPE string,
      lv_hint             TYPE string,
      lv_class            TYPE string.

    IF iv_repo_key IS NOT INITIAL.
      lv_key = iv_repo_key.
    ELSEIF io_repo IS BOUND.
      lv_key = io_repo->get_key( ).
    ELSE.
      zcx_abapgit_exception=>raise( 'Either iv_repo_key or io_repo must be supplied' ).
    ENDIF.

    IF iv_branch IS NOT INITIAL.
      lv_branch = iv_branch.
      lv_text = zcl_abapgit_git_branch_list=>get_display_name( lv_branch ).
    ELSEIF io_repo IS BOUND.
      lv_selected_commit = io_repo->get_selected_commit( ).
      IF lv_selected_commit IS NOT INITIAL.
        "Convert to short commit. Example: (ae623b9...)
        lv_commit_short_sha = lv_selected_commit+0(7).
        lv_text = |({ lv_commit_short_sha }...)|.
      ELSE.
        lv_branch = io_repo->get_selected_branch( ).
        lv_text = zcl_abapgit_git_branch_list=>get_display_name( lv_branch ).
      ENDIF.
    ELSE.
      zcx_abapgit_exception=>raise( 'Either iv_branch or io_repo must be supplied' ).
    ENDIF.

    CASE zcl_abapgit_git_branch_list=>get_type( lv_branch ).
      WHEN zif_abapgit_definitions=>c_git_branch_type-branch.
        lv_class = 'branch branch_branch'.
        lv_icon  = 'code-branch/grey70'.
        lv_hint  = 'Current branch'.
      WHEN zif_abapgit_definitions=>c_git_branch_type-annotated_tag
        OR zif_abapgit_definitions=>c_git_branch_type-lightweight_tag.
        lv_class = 'branch'.
        lv_icon  = 'tag-solid/grey70'.
        lv_hint  = 'Current tag'.
      WHEN OTHERS.
        lv_class = 'branch branch_branch'.
        lv_icon  = 'code-branch/grey70'.
        lv_hint  = 'Current commit'.
    ENDCASE.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( |<span class="{ lv_class }">| ).
    ri_html->add_icon( iv_name = lv_icon
                       iv_hint = lv_hint ).
    IF iv_interactive = abap_true.
      ri_html->add_a( iv_act = |{ zif_abapgit_definitions=>c_action-git_branch_switch }?key={ lv_key }|
                      iv_txt = lv_text ).
    ELSE.
      ri_html->add( lv_text ).
    ENDIF.
    ri_html->add( '</span>' ).

  ENDMETHOD.


  METHOD render_commit_popup.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<ul class="hotkeys">' ).
    ri_html->add( |<li>| && |<span>{ iv_content }</span>| && |</li>| ).
    ri_html->add( '</ul>' ).

    ri_html = render_infopanel(
      iv_div_id     = |{ iv_id }|
      iv_title      = 'Commit details'
      iv_hide       = abap_true
      iv_scrollable = abap_false
      io_content    = ri_html ).

  ENDMETHOD.


  METHOD render_error.

    DATA lv_error TYPE string.
    DATA lv_class TYPE string VALUE 'panel error center'.

    IF iv_extra_style IS NOT INITIAL.
      lv_class = lv_class && ` ` && iv_extra_style.
    ENDIF.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF ix_error IS BOUND.
      lv_error = ix_error->get_text( ).
    ELSE.
      lv_error = iv_error.
    ENDIF.

    ri_html->add( |<div class="{ lv_class }">| ).
    ri_html->add( |{ ri_html->icon( 'exclamation-circle/red' ) } { lv_error }| ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_error_message_box.

    DATA:
      lv_error_text          TYPE string,
      lv_longtext            TYPE string,
      lt_longtext_paragraphs TYPE string_table,
      lv_program_name        TYPE sy-repid,
      lv_title               TYPE string,
      lv_text                TYPE string.
    FIELD-SYMBOLS:
      <lv_longtext_paragraph> TYPE string.


    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    lv_error_text = ix_error->get_text( ).
    lv_longtext = ix_error->if_message~get_longtext( abap_true ).

    IF lv_longtext IS NOT INITIAL.
      lv_error_text = |{ lv_error_text } <span class="emphasis">More...</span>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ zcx_abapgit_exception=>c_section_text-cause }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ zcx_abapgit_exception=>c_section_text-system_response }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ zcx_abapgit_exception=>c_section_text-what_to_do }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE FIRST OCCURRENCE OF REGEX
        |({ zcx_abapgit_exception=>c_section_text-sys_admin }{ cl_abap_char_utilities=>newline })|
        IN lv_longtext WITH |<h3>$1</h3>|.

      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
        IN lv_longtext
        WITH cl_abap_char_utilities=>newline.

      SPLIT lv_longtext AT cl_abap_char_utilities=>newline INTO TABLE lt_longtext_paragraphs.
      CLEAR lv_longtext.

      LOOP AT lt_longtext_paragraphs ASSIGNING <lv_longtext_paragraph>.
        CONDENSE <lv_longtext_paragraph>.

        IF <lv_longtext_paragraph> IS INITIAL.
          CONTINUE.
        ENDIF.

        lv_longtext = |{ lv_longtext }<p>{ <lv_longtext_paragraph> }</p>{ cl_abap_char_utilities=>newline }|.
      ENDLOOP.
    ENDIF.

    ri_html->add( |<div id="message" class="message-panel">| ).
    ri_html->add( |{ ri_html->icon( 'exclamation-circle/red' ) } { lv_error_text }| ).
    ri_html->add( |<div class="message-panel-bar">| ).

    ri_html->add_a(
      iv_txt   = `&#x274c;`
      iv_act   = `toggleDisplay('message')`
      iv_class = `close-btn`
      iv_typ   = zif_abapgit_html=>c_action_type-onclick ).

    ri_html->add( |</div>| ).

    ri_html->add( |<div class="message-panel-bar message-panel-commands">| ).

    IF ix_error->if_t100_message~t100key-msgid IS NOT INITIAL.

      lv_title = get_t100_text(
        iv_msgid = ix_error->if_t100_message~t100key-msgid
        iv_msgno = ix_error->if_t100_message~t100key-msgno ).

      IF lv_title IS NOT INITIAL.
        lv_text = |Message ({ ix_error->if_t100_message~t100key-msgid }/{ ix_error->if_t100_message~t100key-msgno })|.

        ri_html->add_a(
          iv_txt   = lv_text
          iv_typ   = zif_abapgit_html=>c_action_type-sapevent
          iv_act   = zif_abapgit_definitions=>c_action-goto_message
          iv_title = lv_title
          iv_id    = `a_goto_message` ).
      ENDIF.
    ENDIF.

    ix_error->get_source_position( IMPORTING program_name = lv_program_name ).

    lv_title = normalize_program_name( lv_program_name ).

    ri_html->add_a(
      iv_txt   = `Goto source`
      iv_act   = zif_abapgit_definitions=>c_action-goto_source
      iv_typ   = zif_abapgit_html=>c_action_type-sapevent
      iv_title = lv_title
      iv_id    = `a_goto_source` ).

    ri_html->add_a(
      iv_txt = `Callstack`
      iv_act = zif_abapgit_definitions=>c_action-show_callstack
      iv_typ = zif_abapgit_html=>c_action_type-sapevent
      iv_id  = `a_callstack` ).

    ri_html->add( |</div>| ).
    ri_html->add( |<div class="message-panel-commands">| ).
    ri_html->add( |{ lv_longtext }| ).
    ri_html->add( |</div>| ).
    ri_html->add( |</div>| ).

  ENDMETHOD.


  METHOD render_event_as_form.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add(
      |<form id="form_{ is_event-name }" method="{ is_event-method }" action="sapevent:{ is_event-name }"></form>| ).

  ENDMETHOD.


  METHOD render_help_hint.

    " TODO potentially move to or integrate with zcl_abapgit_html_form

    DATA lt_fragments TYPE string_table.
    DATA li_html TYPE REF TO zif_abapgit_html.
    li_html = zcl_abapgit_html=>create( ).

    APPEND `<div class="form-field-help-tooltip">` TO lt_fragments.
    APPEND li_html->icon(
      iv_name = 'question-circle-solid'
      iv_class = 'blue' ) TO lt_fragments.
    APPEND `<div class="form-field-help-tooltip-text">` TO lt_fragments.
    APPEND iv_text_to_wrap TO lt_fragments.
    APPEND `</div>` TO lt_fragments.
    APPEND `</div>` TO lt_fragments.

    rv_html = concat_lines_of( table = lt_fragments ).

  ENDMETHOD.


  METHOD render_infopanel.

    DATA lv_display TYPE string.
    DATA lv_class TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF iv_hide = abap_true. " Initially hide
      lv_display = 'display:none'.
    ENDIF.

    lv_class = 'info-panel'.
    IF iv_scrollable = abap_false. " Initially hide
      lv_class = lv_class && ' info-panel-fixed'.
    ENDIF.

    ri_html->add( |<div id="{ iv_div_id }" class="{ lv_class }" style="{ lv_display }">| ).

    ri_html->add( |<div class="info-title">{ iv_title }|
               && '<div class="float-right">'
               && ri_html->a(
                    iv_txt   = '&#x274c;'
                    iv_typ   = zif_abapgit_html=>c_action_type-onclick
                    iv_act   = |toggleDisplay('{ iv_div_id }')|
                    iv_class = 'close-btn' )
               && '</div></div>' ).

    IF iv_hint IS NOT INITIAL.
      ri_html->add( '<div class="info-hint">'
        && ri_html->icon( iv_name = 'exclamation-triangle'
                          iv_class = 'pad-right' )
        && iv_hint
        && '</div>' ).
    ENDIF.

    ri_html->add( |<div class="info-list">| ).
    ri_html->add( io_content ).
    ri_html->add( '</div>' ).
    ri_html->add( '</div>' ).

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
    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( '<div id="js-error-banner" class="dummydiv error">' ).
    ri_html->add( |{ ri_html->icon( 'exclamation-triangle/red' ) }| &&
                  ' If this does not disappear soon,' &&
                  ' then there is a JS init error, please log an issue' ).
    ri_html->add( '</div>' ).
  ENDMETHOD.


  METHOD render_label_list.

    DATA lt_fragments TYPE string_table.
    DATA lv_l TYPE string.
    DATA lv_class TYPE string.
    DATA lv_style TYPE string.
    DATA ls_parsed_color TYPE zcl_abapgit_repo_labels=>ty_color.
    DATA li_html TYPE REF TO zif_abapgit_html.

    IF it_labels IS INITIAL.
      RETURN.
    ENDIF.

    li_html = zcl_abapgit_html=>create( ).

    APPEND `<ul class="repo-labels">` TO lt_fragments.

    LOOP AT it_labels INTO lv_l WHERE table_line IS NOT INITIAL.
      CLEAR lv_class.
      CLEAR lv_style.
      ls_parsed_color = zcl_abapgit_repo_labels=>parse_color( io_label_colors->get( lv_l ) ).
      IF ls_parsed_color-cls IS NOT INITIAL.
        lv_class = | class="rl-{ ls_parsed_color-cls }"|.
      ELSEIF ls_parsed_color-fg IS NOT INITIAL OR ls_parsed_color-bg IS NOT INITIAL.
        lv_style = ` style="`.
        IF ls_parsed_color-fg IS NOT INITIAL.
          lv_style = lv_style && |color:#{ ls_parsed_color-fg };|.
        ENDIF.
        IF ls_parsed_color-bg IS NOT INITIAL.
          lv_style = lv_style && |background-color:#{ ls_parsed_color-bg };|.
          lv_style = lv_style && |border-color:#{ ls_parsed_color-bg };|.
        ENDIF.
        lv_style = lv_style && `"`.
      ENDIF.

      IF iv_clickable_action IS NOT INITIAL.
        lv_l = li_html->a(
          iv_txt = lv_l
          iv_act = |{ iv_clickable_action }|
          iv_class = 'command'
          iv_query = lv_l ).
      ENDIF.
      lv_l = |<li{ lv_class }{ lv_style }>{ lv_l }</li>|.
      APPEND lv_l TO lt_fragments.
    ENDLOOP.

    APPEND `</ul>` TO lt_fragments.

    rv_html = concat_lines_of( table = lt_fragments ).

  ENDMETHOD.


  METHOD render_news.

    DATA: lv_text TYPE string,
          lv_hint TYPE string,
          lv_ul   TYPE abap_bool,
          lt_log  TYPE zcl_abapgit_news=>ty_logs.

    FIELD-SYMBOLS: <ls_line> LIKE LINE OF lt_log.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

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
        IF lv_ul = abap_true.
          ri_html->add( |</ul>| ).
        ENDIF.
        ri_html->add( |<h1>{ lv_text }</h1>| ).
        ri_html->add( |<ul>| ).
        lv_ul = abap_true.
      ELSE.
        <ls_line>-text = escape( val    = <ls_line>-text
                                 format = cl_abap_format=>e_html_text ).
        ri_html->add( |<li>{ <ls_line>-text }</li>| ).
      ENDIF.
    ENDLOOP.
    IF lv_ul = abap_true.
      ri_html->add( |</ul>| ).
    ENDIF.

    " Wrap
    IF io_news->has_important( ) = abap_true.
      lv_hint = 'Please note changes marked with "!"'.
    ENDIF.

    ri_html = render_infopanel(
      iv_div_id  = 'news'
      iv_title   = 'Announcement of the latest changes'
      iv_hint    = lv_hint
      iv_hide    = boolc( io_news->has_unseen( ) = abap_false )
      io_content = ri_html ).

  ENDMETHOD.


  METHOD render_order_by_header_cells.

    DATA:
      lv_tmp       TYPE string,
      lv_disp_name TYPE string.

    FIELD-SYMBOLS <ls_col> LIKE LINE OF it_col_spec.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

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
            lv_tmp = lv_tmp && ri_html->a(
              iv_txt   = lv_disp_name
              iv_act   = |{ zif_abapgit_definitions=>c_action-change_order_by }|
              iv_title = <ls_col>-title ).
          ELSE.
            lv_tmp = lv_tmp && ri_html->a(
              iv_txt   = lv_disp_name
              iv_act   = |{ zif_abapgit_definitions=>c_action-direction }?direction=DESCENDING|
              iv_title = <ls_col>-title ).
          ENDIF.
        ELSEIF <ls_col>-allow_order_by = abap_true.
          lv_tmp = lv_tmp && ri_html->a(
            iv_txt   = lv_disp_name
            iv_act   = |{ zif_abapgit_definitions=>c_action-change_order_by }?orderBy={ <ls_col>-tech_name }|
            iv_title = <ls_col>-title ).
        ELSE.
          lv_tmp = lv_tmp && lv_disp_name.
        ENDIF.
      ENDIF.
      IF <ls_col>-tech_name = iv_order_by
      AND iv_order_by IS NOT INITIAL.
        IF iv_order_descending = abap_true.
          lv_tmp = lv_tmp && | &#x25BE;|. " arrow down
        ELSE.
          lv_tmp = lv_tmp && | &#x25B4;|. " arrow up
        ENDIF.
      ENDIF.

      lv_tmp = lv_tmp && '</th>'.
      ri_html->add( lv_tmp ).
    ENDLOOP.

  ENDMETHOD.


  METHOD render_package_name.

    DATA:
      lv_obj_name TYPE tadir-obj_name,
      lv_jump     TYPE string,
      lv_title    TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF iv_package IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_suppress_title = abap_false.
      lv_title = zcl_abapgit_factory=>get_sap_package( iv_package )->read_description( ).
    ENDIF.

    lv_obj_name = iv_package.
    lv_jump = zcl_abapgit_html_action_utils=>jump_encode(
      iv_obj_type = 'DEVC'
      iv_obj_name = lv_obj_name ).

    ri_html->add( |<span class="package-box">| ).
    ri_html->add_icon( iv_name = 'box/grey70'
                       iv_hint = 'SAP package' ).
    IF iv_interactive = abap_true.
      ri_html->add_a( iv_act   = |{ zif_abapgit_definitions=>c_action-jump }?{ lv_jump }|
                      iv_title = lv_title
                      iv_txt   = |{ iv_package }| ).
    ELSE.
      ri_html->add( iv_package ).
    ENDIF.
    ri_html->add( '</span>' ).

  ENDMETHOD.


  METHOD render_path.

    DATA:
      lv_path    TYPE string,
      lv_jump    TYPE string,
      lv_folder  TYPE string,
      lt_folders TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF iv_path IS INITIAL.
      RETURN.
    ENDIF.

    lv_jump = |{ zcl_abapgit_gui_page_repo_view=>c_actions-change_dir }?PATH=|.

    ri_html->add( |<span class="path-box">| ).

    IF iv_interactive = abap_true.
      SPLIT iv_path AT '/' INTO TABLE lt_folders.

      LOOP AT lt_folders INTO lv_folder.
        IF lv_folder IS INITIAL.
          " root
          lv_path = '/'.
        ELSEIF sy-tabix < lines( lt_folders ).
          lv_path = lv_path && lv_folder && '/'.
          ri_html->add_a( iv_act = lv_jump && lv_path
                          iv_txt = lv_folder ).
        ELSE.
          " no link for current folder
          ri_html->add( | <strong>{ lv_folder }</strong> | ).
        ENDIF.
        ri_html->add( '/' ).
      ENDLOOP.
    ELSE.
      ri_html->add( iv_path ).
    ENDIF.

    ri_html->add( '</span>' ).

  ENDMETHOD.


  METHOD render_repo_palette.

    DATA lt_repo_obj_list TYPE zif_abapgit_repo_srv=>ty_repo_list.
    DATA lt_repo_list TYPE zif_abapgit_persistence=>ty_repos.
    DATA lv_repo_json TYPE string.
    DATA lv_size TYPE i.
    DATA ls_repo_data LIKE LINE OF lt_repo_list.

    FIELD-SYMBOLS:
      <ls_repo>     LIKE LINE OF lt_repo_list,
      <lr_repo_obj> LIKE LINE OF lt_repo_obj_list.

    lt_repo_obj_list = zcl_abapgit_repo_srv=>get_instance( )->list( ).

    LOOP AT lt_repo_obj_list ASSIGNING <lr_repo_obj>.
      ls_repo_data = <lr_repo_obj>->ms_data.
      ls_repo_data-local_settings-display_name = <lr_repo_obj>->get_name( ).
      APPEND ls_repo_data TO lt_repo_list.
    ENDLOOP.

    lv_size = lines( lt_repo_list ).
    SORT lt_repo_list BY local_settings-display_name AS TEXT.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( 'var repoCatalog = [' ). " Maybe separate this into another method if needed in more places
    LOOP AT lt_repo_list ASSIGNING <ls_repo>.
      lv_repo_json = |\{ key: "{ <ls_repo>-key
        }", isOffline: "{ <ls_repo>-offline
        }", displayName: "{ <ls_repo>-local_settings-display_name }"  \}|.
      IF sy-tabix < lv_size.
        lv_repo_json = lv_repo_json && ','.
      ENDIF.
      ri_html->add( lv_repo_json ).
    ENDLOOP.
    ri_html->add( '];' ).

    ri_html->add( |var gGoRepoPalette = new CommandPalette(createRepoCatalogEnumerator(repoCatalog, "{
      iv_action }"), \{| ).
    ri_html->add( '  toggleKey: "F2",' ).
    ri_html->add( '  hotkeyDescription: "Go to Repository ..."' ).
    ri_html->add( '});' ).

  ENDMETHOD.


  METHOD render_repo_top.

    DATA: lo_repo_online TYPE REF TO zcl_abapgit_repo_online,
          lo_pback       TYPE REF TO zcl_abapgit_persist_background,
          lx_error       TYPE REF TO zcx_abapgit_exception,
          lv_hint        TYPE string,
          lv_icon        TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    CREATE OBJECT lo_pback.

    IF io_repo->is_offline( ) = abap_true.
      lv_icon = 'plug/darkgrey'.
      lv_hint = 'Offline Repository'.
    ELSE.
      lv_icon = 'cloud-upload-alt/blue'.
      lv_hint = 'On-line Repository'.
    ENDIF.

    ri_html->add( '<table class="w100"><tr>' ).

    ri_html->add( '<td class="repo_name">' ).

    " Repo type and name
    ri_html->add_icon( iv_name = lv_icon
                       iv_hint = lv_hint ).
    ri_html->add( |<span class="name">{ io_repo->get_name( ) }</span>| ).
    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.

      ri_html->add_a( iv_txt   = lo_repo_online->get_url( )
                      iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url=|
                              && |{ lo_repo_online->get_url( ) }|
                      iv_class = |url| ).
    ENDIF.

    IF iv_show_edit = abap_true.
      ri_html->add_a( iv_txt   = ri_html->icon( iv_name  = 'edit-solid'
                                                iv_class = 'pad-sides'
                                                iv_hint  = 'Change Remote' )
                      iv_act   = |{ zif_abapgit_definitions=>c_action-repo_remote_settings }?| &&
                                 |key={ io_repo->get_key( ) }|
                      iv_class = |url| ).
    ENDIF.

    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.

      ri_html->add_a( iv_txt   = ri_html->icon( iv_name  = 'copy-solid'
                                                iv_class = 'pad-sides'
                                                iv_hint  = 'Copy URL to Clipboard' )
                      iv_act   = |{ zif_abapgit_definitions=>c_action-clipboard }| &&
                                 |?clipboard={ lo_repo_online->get_url( ) }|
                      iv_class = |url| ).
    ENDIF.

    IF io_repo->is_offline( ) = abap_false AND iv_show_commit = abap_true.
      TRY.
          render_repo_top_commit_hash( ii_html        = ri_html
                                       io_repo_online = lo_repo_online ).
        CATCH zcx_abapgit_exception INTO lx_error.
          " In case of missing or wrong credentials, show message in status bar
          lv_hint = lx_error->get_text( ).
          IF lv_hint CS 'credentials'.
            MESSAGE lv_hint TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.
      ENDTRY.
    ENDIF.

    " News
    IF io_news IS BOUND AND io_news->has_news( ) = abap_true.
      IF io_news->has_updates( ) = abap_true.
        lv_icon = 'arrow-circle-up/warning'.
      ELSE.
        lv_icon = 'arrow-circle-up'.
      ENDIF.
      ri_html->add_a( iv_act   = |toggleDisplay('news')|
                      iv_typ   = zif_abapgit_html=>c_action_type-onclick
                      iv_txt   = ri_html->icon( iv_name  = lv_icon
                                                iv_class = 'pad-sides'
                                                iv_hint  = 'Display Changelog' )
                      iv_class = |url| ).
    ENDIF.
    ri_html->add( '</td>' ).

    ri_html->add( '<td class="repo_attr right">' ).

    " Fav
    IF abap_true = zcl_abapgit_persistence_user=>get_instance( )->is_favorite_repo( io_repo->get_key( ) ).
      lv_icon = 'star/blue'.
    ELSE.
      lv_icon = 'star/grey'.
    ENDIF.
    ri_html->add_a( iv_act = |{ zif_abapgit_definitions=>c_action-repo_toggle_fav }?key={ io_repo->get_key( ) }|
                    iv_txt = ri_html->icon( iv_name  = lv_icon
                                            iv_class = 'pad-sides'
                                            iv_hint  = 'Toggle Favorite' ) ).

    " BG
    IF lo_pback->exists( io_repo->get_key( ) ) = abap_true.
      ri_html->add( '<span class="bg_marker" title="background">BG</span>' ).
    ENDIF.

    " Write protect
    IF io_repo->get_local_settings( )-write_protected = abap_true.
      ri_html->add_icon( iv_name = 'lock/grey70'
                         iv_hint = 'Locked from Pulls' ).
    ENDIF.

    " Branch
    IF io_repo->is_offline( ) = abap_false.
      lo_repo_online ?= io_repo.
      IF iv_show_branch = abap_true.
        ri_html->add( render_branch_name( io_repo        = lo_repo_online
                                          iv_interactive = iv_interactive_branch ) ).
      ENDIF.
    ENDIF.

    " Package
    IF iv_show_package = abap_true.
      ri_html->add( render_package_name( io_repo->get_package( ) ) ).
    ENDIF.

    ri_html->add( '</td>' ).
    ri_html->add( '</tr></table>' ).

  ENDMETHOD.


  METHOD render_repo_top_commit_hash.

    DATA: lv_commit_hash       TYPE zif_abapgit_git_definitions=>ty_sha1,
          lv_commit_short_hash TYPE zif_abapgit_git_definitions=>ty_sha1,
          lv_display_url       TYPE zif_abapgit_persistence=>ty_repo-url,
          lo_url               TYPE REF TO zcl_abapgit_git_url,
          lv_icon_commit       TYPE string.

    lv_commit_hash = io_repo_online->get_current_remote( ).
    lv_commit_short_hash = lv_commit_hash(7).

    lv_icon_commit = ii_html->icon( iv_name  = 'code-commit'
                                    iv_class = 'pad-sides'
                                    iv_hint  = 'Commit' ).

    CREATE OBJECT lo_url.

    TRY.
        lv_display_url = lo_url->get_commit_display_url( io_repo_online ).

        ii_html->add_a( iv_txt   = |{ lv_icon_commit }{ lv_commit_short_hash }|
                        iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ lv_display_url }|
                        iv_title = 'Commit'
                        iv_class = |url| ).
      CATCH zcx_abapgit_exception.
        ii_html->add( |<span class="url">{ lv_icon_commit }{ lv_commit_short_hash }</span>| ).
    ENDTRY.

  ENDMETHOD.


  METHOD render_repo_url.

    ri_html = zcl_abapgit_html=>create( )->add_a(
      iv_txt   = shorten_repo_url( iv_url )
      iv_title = iv_url
      iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ iv_url }| ).

    IF iv_render_remote_edit_for_key IS NOT INITIAL.
      ri_html->add_a(
        iv_txt   = ri_html->icon(
          iv_name  = 'edit-solid'
          iv_class = 'pad-sides'
          iv_hint  = 'Change remote' )
        iv_act   = |{ zif_abapgit_definitions=>c_action-repo_remote_settings }?key={ iv_render_remote_edit_for_key }|
        iv_class = |remote_repo| ).
    ENDIF.

  ENDMETHOD.


  METHOD render_sci_result.

    DATA lv_icon TYPE string.

    lv_icon = ii_html->icon(
      iv_name = 'bug-solid'
      iv_hint = 'Code inspector result' ).

    CASE iv_sci_result.
      WHEN zif_abapgit_definitions=>c_sci_result-passed.
        ii_html->add( |<span class="boxed green-filled-set">{ lv_icon }PASSED</span>| ).
      WHEN zif_abapgit_definitions=>c_sci_result-failed.
        ii_html->add( |<span class="boxed red-filled-set">{ lv_icon }FAILED</span>| ).
      WHEN zif_abapgit_definitions=>c_sci_result-warning.
        ii_html->add( |<span class="boxed yellow-filled-set">{ lv_icon }WARN</span>| ).
      WHEN OTHERS. " Including NO_RUN
        RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD render_text_input.

    DATA lv_attrs TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF iv_value IS NOT INITIAL.
      lv_attrs = | value="{ iv_value }"|.
    ENDIF.

    IF iv_max_length IS NOT INITIAL.
      lv_attrs = lv_attrs && | maxlength="{ iv_max_length }"|.
    ENDIF.

    IF iv_autofocus = abap_true.
      lv_attrs = lv_attrs && | autofocus|.
    ENDIF.

    ri_html->add( |<label for="{ iv_name }">{ iv_label }</label>| ).
    ri_html->add( |<input id="{ iv_name }" name="{ iv_name }" type="text"{ lv_attrs }>| ).

  ENDMETHOD.


  METHOD render_timestamp.

    DATA lv_date TYPE d.
    DATA lv_time TYPE t.

    CONVERT TIME STAMP iv_timestamp
      TIME ZONE gv_time_zone
      INTO DATE lv_date
      TIME      lv_time.

    rv_rendered = |{ lv_date DATE = USER } { lv_time TIME = USER }|.

  ENDMETHOD.


  METHOD render_transport.

    DATA:
      lv_title TYPE string,
      lv_jump  TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF iv_transport IS INITIAL.
      RETURN.
    ENDIF.

    lv_title = zcl_abapgit_factory=>get_cts_api( )->read_description( iv_transport ).

    lv_jump = |{ zif_abapgit_definitions=>c_action-jump_transport }?transport={ iv_transport }|.

    IF iv_icon_only = abap_true.
      ri_html->add_a( iv_act   = lv_jump
                      iv_title = |Transport { iv_transport }|
                      iv_txt   = zcl_abapgit_html=>icon( 'truck-solid/darkgrey' ) ).
    ELSE.
      ri_html->add( |<span class="transport-box">| ).

      ri_html->add_icon( iv_name = 'truck-solid/grey70'
                         iv_hint = 'Transport' ).
      IF iv_interactive = abap_true.
        ri_html->add_a( iv_act   = lv_jump
                        iv_title = lv_title
                        iv_txt   = |{ iv_transport }| ).
      ELSE.
        ri_html->add( iv_transport ).
      ENDIF.

      ri_html->add( '</span>' ).
    ENDIF.

  ENDMETHOD.


  METHOD render_user_name.

    DATA:
      lv_title TYPE string,
      lv_jump  TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    IF iv_username IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_username <> zcl_abapgit_objects_super=>c_user_unknown AND iv_suppress_title = abap_false.
      lv_title = zcl_abapgit_user_record=>get_title( iv_username ).
    ENDIF.

    lv_jump = |{ zif_abapgit_definitions=>c_action-jump_user }?user={ iv_username }|.

    IF iv_icon_only = abap_true.
      ri_html->add_a( iv_act   = lv_jump
                      iv_title = lv_title
                      iv_txt   = zcl_abapgit_html=>icon( 'user-solid/darkgrey' ) ).
    ELSE.
      ri_html->add( |<span class="user-box">| ).

      ri_html->add_icon( iv_name = 'user-solid/grey70'
                         iv_hint = 'User name' ).
      IF iv_interactive = abap_true AND iv_username <> zcl_abapgit_objects_super=>c_user_unknown.
        ri_html->add_a( iv_act   = lv_jump
                        iv_title = lv_title
                        iv_txt   = |{ iv_username }| ).
      ELSE.
        ri_html->add( iv_username ).
      ENDIF.

      ri_html->add( '</span>' ).
    ENDIF.

  ENDMETHOD.


  METHOD render_warning_banner.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( '<div class="dummydiv warning">' ).
    ri_html->add( |{ ri_html->icon( 'exclamation-triangle/yellow' ) } { iv_text }| ).
    ri_html->add( '</div>' ).

  ENDMETHOD.


  METHOD settings_repo_toolbar.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-repo-settings'.

    ro_menu->add(
      iv_txt = 'Repository'
      iv_act = |{ zif_abapgit_definitions=>c_action-repo_settings }?key={ iv_key }|
      iv_cur = boolc( iv_act = zif_abapgit_definitions=>c_action-repo_settings )
    )->add(
      iv_txt = 'Local'
      iv_act = |{ zif_abapgit_definitions=>c_action-repo_local_settings }?key={ iv_key }|
      iv_cur = boolc( iv_act = zif_abapgit_definitions=>c_action-repo_local_settings )
    )->add(
      iv_txt = 'Remote'
      iv_act = |{ zif_abapgit_definitions=>c_action-repo_remote_settings }?key={ iv_key }|
      iv_cur = boolc( iv_act = zif_abapgit_definitions=>c_action-repo_remote_settings )
    )->add(
      iv_txt = 'Background'
      iv_act = |{ zif_abapgit_definitions=>c_action-repo_background }?key={ iv_key }|
      iv_cur = boolc( iv_act = zif_abapgit_definitions=>c_action-repo_background )
    )->add(
      iv_txt = 'Stats'
      iv_act = |{ zif_abapgit_definitions=>c_action-repo_infos }?key={ iv_key }|
      iv_cur = boolc( iv_act = zif_abapgit_definitions=>c_action-repo_infos ) ).

    zcl_abapgit_exit=>get_instance(  )->enhance_repo_toolbar(
       io_menu = ro_menu
       iv_key  = iv_key
       iv_act  = iv_act ).

  ENDMETHOD.


  METHOD settings_toolbar.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-settings'.

    ro_menu->add(
      iv_txt = 'Global'
      iv_act = zif_abapgit_definitions=>c_action-go_settings
      iv_cur = boolc( iv_act = zif_abapgit_definitions=>c_action-go_settings )
    )->add(
      iv_txt = 'Personal'
      iv_act = zif_abapgit_definitions=>c_action-go_settings_personal
      iv_cur = boolc( iv_act = zif_abapgit_definitions=>c_action-go_settings_personal ) ).

  ENDMETHOD.


  METHOD shorten_repo_url.
    DATA lv_new_length TYPE i.
    DATA lv_length_to_truncate_to TYPE i.

    rv_shortened = iv_full_url.

    REPLACE FIRST OCCURRENCE OF 'https://' IN rv_shortened WITH ''.
    REPLACE FIRST OCCURRENCE OF 'http://' IN rv_shortened WITH ''.
    IF rv_shortened CP '*.git'.
      lv_new_length = strlen( rv_shortened ) - 4.
      rv_shortened  = rv_shortened(lv_new_length).
    ENDIF.

    IF strlen( rv_shortened ) > iv_max_length.
      lv_length_to_truncate_to = iv_max_length - 3.
      rv_shortened = rv_shortened(lv_length_to_truncate_to) && `...`.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
