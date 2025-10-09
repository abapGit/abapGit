CLASS zcl_abapgit_gui_page_flow DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .
    INTERFACES zif_abapgit_gui_menu_provider .

    CLASS-METHODS create
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .
    METHODS constructor
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        refresh             TYPE string VALUE 'refresh',
        consolidate         TYPE string VALUE 'consolidate',
        pull                TYPE string VALUE 'pull',
        stage_and_commit    TYPE string VALUE 'stage_and_commit',
        only_my_transports  TYPE string VALUE 'only_my_transports',
        hide_full_matches   TYPE string VALUE 'hide_full_matches',
        hide_matching_files TYPE string VALUE 'hide_matching_files',
        hide_conflicts      TYPE string VALUE 'hide_conflicts',
      END OF c_action .
    DATA ms_information TYPE zif_abapgit_flow_logic=>ty_information .
    DATA ms_user_settings TYPE zif_abapgit_persist_user=>ty_flow_settings.

    METHODS refresh
      RAISING
        zcx_abapgit_exception .
    METHODS set_branch
      IMPORTING
        !iv_branch TYPE string
        !iv_key    TYPE zif_abapgit_persistence=>ty_value
      RAISING
        zcx_abapgit_exception .

    METHODS render_toolbar
      IMPORTING
        !iv_index      TYPE i
        !is_feature    TYPE zif_abapgit_flow_logic=>ty_feature
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .

    METHODS call_stage_commit
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception.

    METHODS call_pull
      IMPORTING
        !ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception.

    METHODS call_consolidate
      RETURNING
        VALUE(rs_handled) TYPE zif_abapgit_gui_event_handler=>ty_handling_result
      RAISING
        zcx_abapgit_exception.

    METHODS render_user_settings
      RETURNING
        VALUE(ri_html) TYPE REF TO zif_abapgit_html
      RAISING
        zcx_abapgit_exception .

    METHODS skip_show
      IMPORTING
        is_feature     TYPE zif_abapgit_flow_logic=>ty_feature
      RETURNING
        VALUE(rv_skip) TYPE abap_bool
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_FLOW IMPLEMENTATION.


  METHOD call_consolidate.

    DATA lt_repos        TYPE zcl_abapgit_flow_logic=>ty_repos_tt.
    DATA li_repo         LIKE LINE OF lt_repos.

    lt_repos = zcl_abapgit_flow_logic=>list_repos( abap_false ).
    IF lines( lt_repos ) <> 1.
      MESSAGE 'Todo, repository selection popup' TYPE 'S'.
    ELSE.
      READ TABLE lt_repos INTO li_repo INDEX 1.
      ASSERT sy-subrc = 0.
      rs_handled-page  = zcl_abapgit_gui_page_flowcons=>create( li_repo ).
      rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.
    ENDIF.

  ENDMETHOD.


  METHOD call_pull.

    DATA lv_key         TYPE zif_abapgit_persistence=>ty_value.
    DATA lv_branch      TYPE string.
    DATA lo_filter      TYPE REF TO lcl_filter.
    DATA lt_filter      TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lv_index       TYPE i.
    DATA li_repo_online TYPE REF TO zif_abapgit_repo_online.
    DATA ls_feature     LIKE LINE OF ms_information-features.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF ls_feature-changed_objects.
    FIELD-SYMBOLS <ls_filter> LIKE LINE OF lt_filter.

    lv_key = ii_event->query( )->get( 'KEY' ).
    lv_index = ii_event->query( )->get( 'INDEX' ).
    lv_branch = ii_event->query( )->get( 'BRANCH' ).
    li_repo_online ?= zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).

    READ TABLE ms_information-features INTO ls_feature INDEX lv_index.
    ASSERT sy-subrc = 0.

    LOOP AT ls_feature-changed_objects ASSIGNING <ls_object>.
      APPEND INITIAL LINE TO lt_filter ASSIGNING <ls_filter>.
      <ls_filter>-object = <ls_object>-obj_type.
      <ls_filter>-obj_name = <ls_object>-obj_name.
    ENDLOOP.
    CREATE OBJECT lo_filter EXPORTING it_filter = lt_filter.

    set_branch(
      iv_branch = lv_branch
      iv_key    = lv_key ).

    rs_handled-page = zcl_abapgit_gui_page_pull=>create(
      ii_repo       = li_repo_online
      iv_trkorr     = ls_feature-transport-trkorr
      ii_obj_filter = lo_filter ).

    rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

    refresh( ).

  ENDMETHOD.


  METHOD call_stage_commit.

    DATA lv_key          TYPE zif_abapgit_persistence=>ty_value.
    DATA lv_branch       TYPE string.
    DATA lo_filter       TYPE REF TO lcl_filter.
    DATA lt_filter       TYPE zif_abapgit_definitions=>ty_tadir_tt.
    DATA lv_index        TYPE i.
    DATA li_repo_online  TYPE REF TO zif_abapgit_repo_online.
    DATA ls_feature      LIKE LINE OF ms_information-features.

    FIELD-SYMBOLS <ls_object> LIKE LINE OF ls_feature-changed_objects.
    FIELD-SYMBOLS <ls_filter> LIKE LINE OF lt_filter.

    lv_key = ii_event->query( )->get( 'KEY' ).
    lv_index = ii_event->query( )->get( 'INDEX' ).
    lv_branch = ii_event->query( )->get( 'BRANCH' ).
    li_repo_online ?= zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).

    READ TABLE ms_information-features INTO ls_feature INDEX lv_index.
    ASSERT sy-subrc = 0.

    LOOP AT ls_feature-changed_objects ASSIGNING <ls_object>.
      APPEND INITIAL LINE TO lt_filter ASSIGNING <ls_filter>.
      <ls_filter>-object = <ls_object>-obj_type.
      <ls_filter>-obj_name = <ls_object>-obj_name.
    ENDLOOP.
    CREATE OBJECT lo_filter EXPORTING it_filter = lt_filter.

    set_branch(
      iv_branch = lv_branch
      iv_key    = lv_key ).

    rs_handled-page = zcl_abapgit_gui_page_stage=>create(
      ii_force_refresh = abap_false
      ii_repo_online   = li_repo_online
      ii_obj_filter    = lo_filter ).

    rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page_w_bookmark.

    refresh( ).

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    ms_user_settings = zcl_abapgit_persist_factory=>get_user( )->get_flow_settings( ).
  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_flow.

    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Flow'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD refresh.

    DATA ls_feature LIKE LINE OF ms_information-features.
    DATA li_repo  TYPE REF TO zif_abapgit_repo.


    LOOP AT ms_information-features INTO ls_feature.
      li_repo = zcl_abapgit_repo_srv=>get_instance( )->get( ls_feature-repo-key ).
      li_repo->refresh( ).
    ENDLOOP.

    CLEAR ms_information.

  ENDMETHOD.


  METHOD render_toolbar.

    DATA lo_toolbar TYPE REF TO zcl_abapgit_html_toolbar.
    DATA lv_extra   TYPE string.
    DATA li_repo    TYPE REF TO zif_abapgit_repo.
    DATA lv_opt     TYPE c LENGTH 1.


    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    CREATE OBJECT lo_toolbar EXPORTING iv_id = 'toolbar-flow'.

    li_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( is_feature-repo-key ).
    IF li_repo->get_local_settings( )-write_protected = abap_true.
      lv_opt = zif_abapgit_html=>c_html_opt-crossout.
    ELSE.
      lv_opt = zif_abapgit_html=>c_html_opt-strong.
    ENDIF.

    IF is_feature-full_match = abap_false AND is_feature-branch-display_name IS NOT INITIAL.
      lv_extra = |?index={ iv_index }&key={ is_feature-repo-key }&branch={ is_feature-branch-display_name }|.
      lo_toolbar->add( iv_txt = 'Pull'
                       iv_act = |{ c_action-pull }{ lv_extra }|
                       iv_opt = lv_opt ).
      IF is_feature-transport-trkorr IS NOT INITIAL
          AND ( is_feature-branch-up_to_date = abap_undefined OR is_feature-branch-up_to_date = abap_true ).
* its only remote, so there is no changes to stage
        lo_toolbar->add( iv_txt = 'Stage and Commit'
                         iv_act = |{ c_action-stage_and_commit }{ lv_extra }|
                         iv_opt = zif_abapgit_html=>c_html_opt-strong ).
      ENDIF.
    ENDIF.

    zcl_abapgit_flow_exit=>get_instance( )->toolbar_extras(
      io_toolbar = lo_toolbar
      iv_index   = iv_index
      is_feature = is_feature ).

    ri_html->add( lo_toolbar->render( ) ).

  ENDMETHOD.


  METHOD render_user_settings.

    DATA lv_icon_class TYPE string.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( '<span class="toolbar-light pad-sides">' ).

    IF ms_user_settings-only_my_transports = abap_true.
      lv_icon_class = `blue`.
    ELSE.
      lv_icon_class = `grey`.
    ENDIF.
    ri_html->add( ri_html->a(
      iv_txt   = |<i id="icon-filter-favorite" class="icon icon-check { lv_icon_class }"></i> Only my transports|
      iv_class = 'command'
      iv_act   = |{ c_action-only_my_transports }| ) ).

    IF ms_user_settings-hide_full_matches = abap_true.
      lv_icon_class = `blue`.
    ELSE.
      lv_icon_class = `grey`.
    ENDIF.
    ri_html->add( ri_html->a(
      iv_txt   = |<i id="icon-filter-favorite" class="icon icon-check { lv_icon_class }"></i> Hide full matches|
      iv_class = 'command'
      iv_act   = |{ c_action-hide_full_matches }| ) ).

    IF ms_user_settings-hide_matching_files = abap_true.
      lv_icon_class = `blue`.
    ELSE.
      lv_icon_class = `grey`.
    ENDIF.
    ri_html->add( ri_html->a(
      iv_txt   = |<i id="icon-filter-favorite" class="icon icon-check { lv_icon_class }"></i> Hide matching files|
      iv_class = 'command'
      iv_act   = |{ c_action-hide_matching_files }| ) ).

    IF ms_user_settings-hide_conflicts = abap_true.
      lv_icon_class = `blue`.
    ELSE.
      lv_icon_class = `grey`.
    ENDIF.
    ri_html->add( ri_html->a(
      iv_txt   = |<i id="icon-filter-favorite" class="icon icon-check { lv_icon_class }"></i> Hide transports with conflicts|
      iv_class = 'command'
      iv_act   = |{ c_action-hide_conflicts }| ) ).

    ri_html->add( '</span>' ).

  ENDMETHOD.


  METHOD set_branch.

    DATA lv_branch TYPE string.
    DATA li_repo_online TYPE REF TO zif_abapgit_repo_online.

    IF iv_branch IS NOT INITIAL.
      lv_branch = zif_abapgit_git_definitions=>c_git_branch-heads_prefix && iv_branch.
      li_repo_online ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
      IF li_repo_online->get_selected_branch( ) <> lv_branch.
        li_repo_online->select_branch( lv_branch ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA ls_event_result TYPE zif_abapgit_flow_exit=>ty_event_result.


    CASE ii_event->mv_action.
      WHEN c_action-only_my_transports.
        ms_user_settings-only_my_transports = boolc( ms_user_settings-only_my_transports <> abap_true ).
        zcl_abapgit_persist_factory=>get_user( )->set_flow_settings( ms_user_settings ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-hide_full_matches.
        ms_user_settings-hide_full_matches = boolc( ms_user_settings-hide_full_matches <> abap_true ).
        zcl_abapgit_persist_factory=>get_user( )->set_flow_settings( ms_user_settings ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-hide_conflicts.
        ms_user_settings-hide_conflicts = boolc( ms_user_settings-hide_conflicts <> abap_true ).
        zcl_abapgit_persist_factory=>get_user( )->set_flow_settings( ms_user_settings ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-hide_matching_files.
        ms_user_settings-hide_matching_files = boolc( ms_user_settings-hide_matching_files <> abap_true ).
        zcl_abapgit_persist_factory=>get_user( )->set_flow_settings( ms_user_settings ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-refresh.
        refresh( ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-consolidate.
        rs_handled = call_consolidate( ).
      WHEN zif_abapgit_definitions=>c_action-go_file_diff.
        rs_handled = zcl_abapgit_flow_page_utils=>call_diff( ii_event ).
      WHEN c_action-stage_and_commit.
        rs_handled = call_stage_commit( ii_event ).
      WHEN c_action-pull.
        rs_handled = call_pull( ii_event ).
      WHEN OTHERS.
        ls_event_result = zcl_abapgit_flow_exit=>get_instance( )->on_event(
          ii_event    = ii_event
          it_features = ms_information-features ).

        rs_handled = ls_event_result-handled.

        IF ls_event_result-refresh = abap_true.
          refresh( ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    ro_toolbar = zcl_abapgit_html_toolbar=>create( 'toolbar-flow' ).

    ro_toolbar->add(
      iv_txt = 'Refresh'
      iv_act = c_action-refresh ).

    ro_toolbar->add(
      iv_txt = 'Consolidate'
      iv_act = c_action-consolidate ).

    ro_toolbar->add(
      iv_txt = zcl_abapgit_gui_buttons=>repo_list( )
      iv_act = zif_abapgit_definitions=>c_action-abapgit_home ).

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.

  METHOD skip_show.

    DATA lt_my_transports TYPE zif_abapgit_cts_api=>ty_trkorr_tt.
    DATA lt_user          TYPE zif_abapgit_cts_api=>ty_user_range.
    DATA ls_user          LIKE LINE OF lt_user.
    DATA ls_duplicate     LIKE LINE OF ms_information-transport_duplicates.

    rv_skip = abap_false.

    IF ms_user_settings-only_my_transports = abap_true.
      ls_user-low = sy-uname.
      ls_user-sign = 'I'.
      ls_user-option = 'EW'.
      lt_my_transports = zcl_abapgit_factory=>get_cts_api( )->list_open_requests( it_user = lt_user ).
    ENDIF.

    IF ms_user_settings-hide_full_matches = abap_true
          AND NOT is_feature-transport IS INITIAL
          AND is_feature-full_match = abap_true.
      rv_skip = abap_true.
      RETURN.
    ENDIF.

    IF ms_user_settings-only_my_transports = abap_true AND is_feature-transport-trkorr IS NOT INITIAL.
      READ TABLE lt_my_transports WITH KEY table_line = is_feature-transport-trkorr TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        rv_skip = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF ms_user_settings-hide_conflicts = abap_true.
      LOOP AT ms_information-transport_duplicates INTO ls_duplicate.
        READ TABLE is_feature-changed_objects WITH KEY
            obj_type = ls_duplicate-obj_type
            obj_name = ls_duplicate-obj_name
            TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          rv_skip = abap_true.
          RETURN.
        ENDIF.
      ENDLOOP.

    ENDIF.

    IF lines( is_feature-changed_files ) = 0.
* no changes, eg. only files outside of starting folder changed
      rv_skip = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD zif_abapgit_gui_renderable~render.

    DATA ls_feature       LIKE LINE OF ms_information-features.
    DATA lv_index         TYPE i.
    DATA lv_rendered      TYPE abap_bool.
    DATA lo_timer         TYPE REF TO zcl_abapgit_timer.
    DATA lv_message       LIKE LINE OF ms_information-errors.



    lo_timer = zcl_abapgit_timer=>create( )->start( ).

    register_handlers( ).
    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( '<div class="repo-overview">' ).

    IF ms_information IS INITIAL.
      ms_information = zcl_abapgit_flow_logic=>get( ).
    ENDIF.

    ri_html->add( render_user_settings( ) ).

    ri_html->add( '<br>' ).
    ri_html->add( '<br>' ).

    IF lines( ms_information-errors ) > 0.
      LOOP AT ms_information-errors INTO lv_message.
        ri_html->add( zcl_abapgit_gui_chunk_lib=>render_error( iv_error = lv_message ) ).
      ENDLOOP.
      ri_html->add( '<br>' ).
    ENDIF.

    LOOP AT ms_information-features INTO ls_feature.
      lv_index = sy-tabix.

      IF skip_show( ls_feature ) = abap_true.
        CONTINUE.
      ENDIF.
      lv_rendered = abap_true.

      ri_html->add( '<b><font size="+2">' && ls_feature-repo-name ).
      IF ls_feature-branch-display_name IS NOT INITIAL.
        ri_html->add( | - | ).
        ri_html->add_icon( 'code-branch' ).
        ri_html->add( ls_feature-branch-display_name ).
      ENDIF.
      IF ls_feature-transport-trkorr IS NOT INITIAL.
        ri_html->add( | - | ).
        ri_html->add_icon( 'truck-solid' ).
        ri_html->add( |<tt>{ ls_feature-transport-trkorr }</tt>| ).
      ENDIF.
      ri_html->add( |</font></b><br>| ).

      IF ls_feature-branch-display_name IS INITIAL.
        ri_html->add( |No branch found, comparing with <tt>main</tt>| ).
      ELSEIF ls_feature-pr IS NOT INITIAL.
        ri_html->add( 'Pull Request: ' ).
        ri_html->add_a(
          iv_txt   = ls_feature-pr-title
          iv_act   = |{ zif_abapgit_definitions=>c_action-url }?url={ ls_feature-pr-url }|
          iv_class = |url| ).

        ri_html->add( |<br>| ).
        IF ls_feature-pr-draft = abap_true.
          ri_html->add( 'Status: Draft' ).
        ELSE.
          ri_html->add( 'Status: Ready for Review' ).
        ENDIF.
      ELSE.
        ri_html->add( |No PR found| ).
      ENDIF.
      ri_html->add( |<br>| ).

      IF ls_feature-transport IS NOT INITIAL.
        ri_html->add( |Transport: <tt>{ ls_feature-transport-trkorr }</tt> - { ls_feature-transport-title }<br>| ).
      ELSE.
        ri_html->add( |No corresponding transport found<br>| ).
      ENDIF.

      ri_html->add( render_toolbar(
        iv_index   = lv_index
        is_feature = ls_feature ) ).

      IF ls_feature-branch IS NOT INITIAL AND ls_feature-branch-up_to_date = abap_false.
        ri_html->add( '<b>Branch not up to date</b><br><br>' ).
        CONTINUE.
      ENDIF.

      IF ls_feature-full_match = abap_true.
        ri_html->add( |Full Match, {
          lines( ls_feature-changed_files ) } files, {
          lines( ls_feature-changed_objects ) } objects<br>| ).
      ELSE.
        ri_html->add( zcl_abapgit_flow_page_utils=>render_table(
          it_files                = ls_feature-changed_files
          it_transport_duplicates = ms_information-transport_duplicates
          is_user_settings        = ms_user_settings
          iv_repo_key             = ls_feature-repo-key ) ).
      ENDIF.

* todo      LOOP AT ls_feature-changed_objects INTO ls_item.
* todo       ri_html->add( |<tt><small>{ ls_item-obj_type } { ls_item-obj_name }</small></tt><br>| ).
* todo     ENDLOOP.

      ri_html->add( '<br>' ).
    ENDLOOP.

    IF lines( ms_information-features ) = 0 OR lv_rendered = abap_false.
      ri_html->add( 'Empty, repositories must be favorite + flow enabled<br><br>' ).

      ri_html->add( 'Or nothing in progress<br><br>' ).

      ri_html->add_a(
        iv_txt   = 'abapGit flow documentation'
        iv_act   = |{ zif_abapgit_definitions=>c_action-url
          }?url=https://docs.abapgit.org/user-guide/reference/flow.html|
        iv_class = |url| ).
    ELSE.
      ri_html->add( |<small>{ lines( ms_information-features ) } features in { lo_timer->end( ) }</small>| ).
    ENDIF.

    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
