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
    CONSTANTS:
      BEGIN OF c_actions,
        show         TYPE string VALUE 'show' ##NO_TEXT,
        overview     TYPE string VALUE 'overview',
        select       TYPE string VALUE 'select',
        apply_filter TYPE string VALUE 'apply_filter',
        abapgit_home TYPE string VALUE 'abapgit_home',
      END OF c_actions.

    DATA: mo_repo_overview TYPE REF TO zcl_abapgit_gui_page_repo_over,
          mv_repo_key      TYPE zif_abapgit_persistence=>ty_value.

    METHODS build_main_menu
      RETURNING VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.

ENDCLASS.



CLASS zcl_abapgit_gui_page_main IMPLEMENTATION.


  METHOD build_main_menu.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-main'.

    ro_menu->add(
      iv_txt = zcl_abapgit_gui_buttons=>new_online( )
      iv_act = zif_abapgit_definitions=>c_action-repo_newonline
    )->add(
      iv_txt = zcl_abapgit_gui_buttons=>new_offline( )
      iv_act = zif_abapgit_definitions=>c_action-repo_newoffline
    )->add(
      iv_txt = zcl_abapgit_gui_buttons=>settings( )
      iv_act = zif_abapgit_definitions=>c_action-go_settings
    )->add(
      iv_txt = zcl_abapgit_gui_buttons=>advanced( )
      iv_title = 'Utilities'
      io_sub = zcl_abapgit_gui_chunk_lib=>advanced_submenu( )
    )->add(
      iv_txt = zcl_abapgit_gui_buttons=>help( )
      iv_title = 'Help'
      io_sub = zcl_abapgit_gui_chunk_lib=>help_submenu( ) ).

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_menu  = build_main_menu( ).
    ms_control-page_title = 'Repository List'.
  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    gui_services( )->get_hotkeys_ctl( )->register_hotkeys( me ).

    IF mo_repo_overview IS INITIAL.
      CREATE OBJECT mo_repo_overview.
    ENDIF.

    ri_html->add( mo_repo_overview->zif_abapgit_gui_renderable~render( ) ).

    register_deferred_script( zcl_abapgit_gui_chunk_lib=>render_repo_palette( c_actions-select ) ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA: lv_key TYPE zif_abapgit_persistence=>ty_value.

    CASE ii_event->mv_action.
      WHEN c_actions-abapgit_home.
        CLEAR mv_repo_key.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_actions-select.

        lv_key = ii_event->query( )->get( 'KEY' ).
        zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lv_key ).

        TRY.
            zcl_abapgit_repo_srv=>get_instance( )->get( lv_key )->refresh( ).
          CATCH zcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.

        mv_repo_key = lv_key.
        CREATE OBJECT rs_handled-page TYPE zcl_abapgit_gui_page_repo_view
          EXPORTING
            iv_key = lv_key.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN zif_abapgit_definitions=>c_action-change_order_by.

        mo_repo_overview->set_order_by( ii_event->query( )->get( 'ORDERBY' ) ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN zif_abapgit_definitions=>c_action-direction.

        mo_repo_overview->set_order_direction(
          boolc( ii_event->query( )->get( 'DIRECTION' ) = 'DESCENDING' ) ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_actions-apply_filter.

        mo_repo_overview->set_filter( ii_event->mt_postdata ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN zif_abapgit_definitions=>c_action-go_patch.

        lv_key = ii_event->query( )->get( 'KEY' ).
        CREATE OBJECT rs_handled-page TYPE zcl_abapgit_gui_page_patch
          EXPORTING
            iv_key = lv_key.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN zif_abapgit_definitions=>c_action-repo_settings.

        lv_key = ii_event->query( )->get( 'KEY' ).
        CREATE OBJECT rs_handled-page TYPE zcl_abapgit_gui_page_repo_sett
          EXPORTING
            io_repo = zcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN OTHERS.

        rs_handled = super->zif_abapgit_gui_event_handler~on_event( ii_event ).

    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Main'.

    ls_hotkey_action-description   = |abapGit settings|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_settings.
    ls_hotkey_action-hotkey = |x|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |New online repository|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_newonline.
    ls_hotkey_action-hotkey = |n|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |New offline repository|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_newoffline.
    ls_hotkey_action-hotkey = |o|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
