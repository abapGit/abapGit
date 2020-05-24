CLASS zcl_abapgit_gui_page_main DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_gui_hotkeys.
    METHODS:
      constructor
        RAISING zcx_abapgit_exception,
      zif_abapgit_gui_event_handler~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_actions,
                 show          TYPE string VALUE 'show' ##NO_TEXT,
                 changed_by    TYPE string VALUE 'changed_by',
                 overview      TYPE string VALUE 'overview',
                 documentation TYPE string VALUE 'documentation',
                 changelog     TYPE string VALUE 'changelog',
               END OF c_actions.

    DATA: mv_show         TYPE zif_abapgit_persistence=>ty_value,
          mo_repo_content TYPE REF TO zcl_abapgit_gui_view_repo.

    METHODS:
      test_changed_by
        RAISING zcx_abapgit_exception,
      retrieve_active_repo
        RAISING zcx_abapgit_exception,
      render_toc
        IMPORTING it_repo_list   TYPE zif_abapgit_definitions=>ty_repo_ref_tt
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html
        RAISING   zcx_abapgit_exception,
      build_main_menu
        RETURNING VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar,
      render_repo
        IMPORTING io_repo        TYPE REF TO zcl_abapgit_repo
        RETURNING VALUE(ro_html) TYPE REF TO zcl_abapgit_html
        RAISING   zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_MAIN IMPLEMENTATION.


  METHOD build_main_menu.

    DATA: lo_advsub  TYPE REF TO zcl_abapgit_html_toolbar,
          lo_helpsub TYPE REF TO zcl_abapgit_html_toolbar.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-main'.
    CREATE OBJECT lo_advsub.
    CREATE OBJECT lo_helpsub.

    lo_advsub->add( iv_txt = 'Repository overview'
                    iv_act = zif_abapgit_definitions=>c_action-go_repo_overview ) ##NO_TEXT.
    lo_advsub->add( iv_txt = 'Database util'
                    iv_act = zif_abapgit_definitions=>c_action-go_db ) ##NO_TEXT.
    lo_advsub->add( iv_txt = 'Package to zip'
                    iv_act = zif_abapgit_definitions=>c_action-zip_package ) ##NO_TEXT.
    lo_advsub->add( iv_txt = 'Transport to zip'
                    iv_act = zif_abapgit_definitions=>c_action-zip_transport ) ##NO_TEXT.
    lo_advsub->add( iv_txt = 'Object to files'
                    iv_act = zif_abapgit_definitions=>c_action-zip_object ) ##NO_TEXT.
    lo_advsub->add( iv_txt = 'Test changed by'
                    iv_act = c_actions-changed_by ) ##NO_TEXT.
    lo_advsub->add( iv_txt = 'Debug info'
                    iv_act = zif_abapgit_definitions=>c_action-go_debuginfo ) ##NO_TEXT.
    lo_advsub->add( iv_txt = 'Settings'
                    iv_act = zif_abapgit_definitions=>c_action-go_settings ) ##NO_TEXT.

    lo_helpsub->add( iv_txt = 'Tutorial'
                     iv_act = zif_abapgit_definitions=>c_action-go_tutorial ) ##NO_TEXT.
    lo_helpsub->add( iv_txt = 'Documentation'
                     iv_act = c_actions-documentation ) ##NO_TEXT.
    lo_helpsub->add( iv_txt = 'Explore'
                     iv_act = zif_abapgit_definitions=>c_action-go_explore ) ##NO_TEXT.
    lo_helpsub->add( iv_txt = 'Changelog'
                     iv_act = c_actions-changelog ) ##NO_TEXT.

    ro_menu->add( iv_txt = '+ Online'
                  iv_act = zif_abapgit_definitions=>c_action-repo_newonline ) ##NO_TEXT.
    ro_menu->add( iv_txt = '+ Offline'
                  iv_act = zif_abapgit_definitions=>c_action-repo_newoffline ) ##NO_TEXT.

    ro_menu->add( iv_txt = 'Advanced'
                  io_sub = lo_advsub ) ##NO_TEXT.
    ro_menu->add( iv_txt = 'Help'
                  io_sub = lo_helpsub ) ##NO_TEXT.

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_title = 'HOME'.
    ms_control-page_menu  = build_main_menu( ).
  ENDMETHOD.


  METHOD render_content.

    DATA: lt_repos    TYPE zif_abapgit_definitions=>ty_repo_ref_tt,
          lx_error    TYPE REF TO zcx_abapgit_exception,
          li_tutorial TYPE REF TO zif_abapgit_gui_renderable,
          lo_repo     LIKE LINE OF lt_repos.

    retrieve_active_repo( ). " Get and validate key of user default repo

    CREATE OBJECT ro_html.
    gui_services( )->get_hotkeys_ctl( )->register_hotkeys( me ).

    TRY.
        lt_repos = zcl_abapgit_repo_srv=>get_instance( )->list( ).
      CATCH zcx_abapgit_exception INTO lx_error.
        ro_html->add( zcl_abapgit_gui_chunk_lib=>render_error( ix_error = lx_error ) ).
        RETURN.
    ENDTRY.

    ro_html->add( render_toc( lt_repos ) ).

    IF mv_show IS INITIAL OR lines( lt_repos ) = 0.
      CREATE OBJECT li_tutorial TYPE zcl_abapgit_gui_view_tutorial.
      ro_html->add( li_tutorial->render( ) ).
    ELSE.
      lo_repo = zcl_abapgit_repo_srv=>get_instance( )->get( mv_show ).
      ro_html->add( render_repo( lo_repo ) ).
    ENDIF.

  ENDMETHOD.


  METHOD render_repo.

    DATA lo_news TYPE REF TO zcl_abapgit_news.

    CREATE OBJECT ro_html.

    lo_news = zcl_abapgit_news=>create( io_repo ).

    ro_html->add( |<div class="repo" id="repo{ io_repo->get_key( ) }">| ).
    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
      io_repo               = io_repo
      io_news               = lo_news
      iv_interactive_branch = abap_true ) ).

    ro_html->add( zcl_abapgit_gui_chunk_lib=>render_news( io_news = lo_news ) ).

    IF mo_repo_content IS BOUND.
      ro_html->add( mo_repo_content->zif_abapgit_gui_renderable~render( ) ).
    ENDIF.
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD render_toc.

    DATA: lo_pback      TYPE REF TO zcl_abapgit_persist_background,
          lv_current    TYPE abap_bool,
          lv_key        TYPE zif_abapgit_persistence=>ty_repo-key,
          lv_icon       TYPE string,
          lo_repo       LIKE LINE OF it_repo_list,
          lo_favbar     TYPE REF TO zcl_abapgit_html_toolbar,
          lo_allbar     TYPE REF TO zcl_abapgit_html_toolbar,
          lt_favorites  TYPE zcl_abapgit_persistence_user=>tt_favorites,
          lv_repo_title TYPE string.


    CREATE OBJECT ro_html.
    CREATE OBJECT lo_favbar.
    CREATE OBJECT lo_allbar EXPORTING iv_id = 'toc-all-repos'.
    CREATE OBJECT lo_pback.

    lt_favorites = zcl_abapgit_persistence_user=>get_instance( )->get_favorites( ).

    LOOP AT it_repo_list INTO lo_repo.
      lv_key     = lo_repo->get_key( ).
      lv_current = abap_false.
      IF lv_key = mv_show.
        lv_current = abap_true.
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
                        iv_cur = lv_current ).
      ENDIF.

      IF lo_repo->is_offline( ) = abap_true.
        lv_icon = 'plug/darkgrey'.
      ELSE.
        lv_icon = 'cloud-upload-alt/blue'.
      ENDIF.

      lo_allbar->add( iv_txt = lv_repo_title
                      iv_act = |{ c_actions-show }?{ lv_key }|
                      iv_ico = lv_icon
                      iv_cur = lv_current ).
    ENDLOOP.

    " Cleanup orphan favorites (for removed repos)
    LOOP AT lt_favorites INTO lv_key.
      zcl_abapgit_persistence_user=>get_instance( )->toggle_favorite( lv_key ).
    ENDLOOP.

    " Render HTML
    ro_html->add( '<div id="toc">' )          ##NO_TEXT. " TODO refactor html & css
    ro_html->add( '<div class="toc_grid">' )  ##NO_TEXT.
    ro_html->add( '<div class="toc_row">' )   ##NO_TEXT.

**********************************************************************

    ro_html->add( '<table class="w100"><tr>' ).
    ro_html->add( |<td class="pad-sides">{
                  zcl_abapgit_html=>icon( iv_name = 'star/blue' iv_hint = 'Favorites' )
                  }</td>| ).

    ro_html->add( '<td class="pad-sides w100 favorites">' ). " Maximize width
    IF lo_favbar->count( ) > 0.
      ro_html->add( lo_favbar->render( iv_sort = abap_true ) ).
    ELSE.
      ro_html->add( |<span class="grey">No favorites so far. For more info please check {
                    zcl_abapgit_html=>a( iv_txt = 'tutorial' iv_act = zif_abapgit_definitions=>c_action-go_tutorial )
                    }</span>| ).
    ENDIF.
    ro_html->add( '</td>' ).

    ro_html->add( '<td>' ).
    ro_html->add( lo_allbar->render_as_droplist(
      iv_label  = zcl_abapgit_html=>icon( iv_name = 'bars/blue' )
      iv_action = c_actions-overview
      iv_right  = abap_true
      iv_sort   = abap_true ) ).
    ro_html->add( '</td>' ).
    ro_html->add( '</tr></table>' ).

**********************************************************************

    ro_html->add( '</div>' ).
    ro_html->add( '</div>' ).
    ro_html->add( '</div>' ).

  ENDMETHOD.


  METHOD retrieve_active_repo.

    DATA: lv_show_old LIKE mv_show.

    TRY.
        zcl_abapgit_repo_srv=>get_instance( )->list( ).
      CATCH zcx_abapgit_exception.
        RETURN.
    ENDTRY.

    lv_show_old = mv_show.
    mv_show     = zcl_abapgit_persistence_user=>get_instance( )->get_repo_show( ). " Get default repo from user cfg

    IF mv_show IS NOT INITIAL.
      TRY. " verify the key exists
          zcl_abapgit_repo_srv=>get_instance( )->get( mv_show ).
        CATCH zcx_abapgit_exception.
          CLEAR mv_show.
          zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( mv_show ).
      ENDTRY.
    ENDIF.

    IF lv_show_old <> mv_show AND NOT mv_show IS INITIAL.
      CREATE OBJECT mo_repo_content
        EXPORTING
          iv_key = mv_show. " Reinit content state
    ENDIF.

  ENDMETHOD.


  METHOD test_changed_by.

    DATA: ls_tadir TYPE zif_abapgit_definitions=>ty_tadir,
          lv_user  TYPE xubname,
          ls_item  TYPE zif_abapgit_definitions=>ty_item.


    ls_tadir = zcl_abapgit_ui_factory=>get_popups( )->popup_object( ).
    IF ls_tadir IS INITIAL.
      RETURN.
    ENDIF.

    ls_item-obj_type = ls_tadir-object.
    ls_item-obj_name = ls_tadir-obj_name.

    lv_user = zcl_abapgit_objects=>changed_by( ls_item ).

    MESSAGE lv_user TYPE 'S'.

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA: lv_key           TYPE zif_abapgit_persistence=>ty_repo-key,
          li_repo_overview TYPE REF TO zif_abapgit_gui_renderable.


    IF NOT mo_repo_content IS INITIAL.
      mo_repo_content->zif_abapgit_gui_event_handler~on_event(
        EXPORTING
          iv_action    = iv_action
          iv_getdata   = iv_getdata
          it_postdata  = it_postdata
        IMPORTING
          ei_page      = ei_page
          ev_state     = ev_state ).

      IF ev_state <> zcl_abapgit_gui=>c_event_state-not_handled.
        RETURN.
      ENDIF.
    ENDIF.

    lv_key = iv_getdata.

    CASE iv_action.
      WHEN c_actions-show.
        zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lv_key ).
        TRY.
            zcl_abapgit_repo_srv=>get_instance( )->get( lv_key )->refresh( ).
          CATCH zcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_actions-changed_by.
        test_changed_by( ).
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN c_actions-documentation.
        zcl_abapgit_services_abapgit=>open_abapgit_wikipage( ).
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN zif_abapgit_definitions=>c_action-go_explore.
        zcl_abapgit_services_abapgit=>open_dotabap_homepage( ).
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN c_actions-changelog.
        zcl_abapgit_services_abapgit=>open_abapgit_changelog( ).
        ev_state = zcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN c_actions-overview.
        CREATE OBJECT li_repo_overview TYPE zcl_abapgit_gui_page_repo_over.
        ei_page = li_repo_overview.
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.
      WHEN OTHERS.
        super->zif_abapgit_gui_event_handler~on_event(
          EXPORTING
            iv_action    = iv_action
            iv_getdata   = iv_getdata
            it_postdata  = it_postdata
          IMPORTING
            ei_page      = ei_page
            ev_state     = ev_state ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Main'.

    ls_hotkey_action-description   = |abapGit settings|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_settings.
    ls_hotkey_action-hotkey = |x|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Stage changes|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_stage.
    ls_hotkey_action-hotkey = |s|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Switch branch|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-git_branch_switch.
    ls_hotkey_action-hotkey = |b|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Installed repo list|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_repo_overview.
    ls_hotkey_action-hotkey = |o|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Refresh repository|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_refresh.
    ls_hotkey_action-hotkey = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Pull|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-git_pull.
    ls_hotkey_action-hotkey = |p|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Add online repository|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_newonline.
    ls_hotkey_action-hotkey = |n|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Uninstall repository|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_purge.
    ls_hotkey_action-hotkey = |u|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Diff|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_diff.
    ls_hotkey_action-hotkey = |d|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Run code inspector|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_code_inspector.
    ls_hotkey_action-hotkey = |i|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Show log|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_log.
    ls_hotkey_action-hotkey = |l|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
