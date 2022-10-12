CLASS zcl_abapgit_gui_page_main DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC INHERITING FROM zcl_abapgit_gui_page.

  PUBLIC SECTION.
    INTERFACES: zif_abapgit_gui_hotkeys.
    METHODS:
      constructor
        IMPORTING
          iv_only_favorites TYPE abap_bool
        RAISING
          zcx_abapgit_exception,
      zif_abapgit_gui_event_handler~on_event REDEFINITION.


  PROTECTED SECTION.
    METHODS:
      render_content REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_actions,
        abapgit_home TYPE string VALUE 'abapgit_home',
      END OF c_actions.

    DATA mo_repo_overview  TYPE REF TO zcl_abapgit_gui_page_repo_over.

    METHODS build_main_menu
      RETURNING
        VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_MAIN IMPLEMENTATION.


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

    CREATE OBJECT mo_repo_overview
      EXPORTING
        iv_only_favorites = iv_only_favorites.

  ENDMETHOD.


  METHOD render_content.

    register_hotkeys( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( mo_repo_overview->zif_abapgit_gui_renderable~render( ) ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_actions-abapgit_home.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN OTHERS.
        rs_handled = super->zif_abapgit_gui_event_handler~on_event( ii_event ).
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Main'.

    ls_hotkey_action-description   = |abapGit Settings|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_settings.
    ls_hotkey_action-hotkey = |x|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |New Online Repository|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_newonline.
    ls_hotkey_action-hotkey = |n|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |New Offline Repository|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_newoffline.
    ls_hotkey_action-hotkey = |o|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
