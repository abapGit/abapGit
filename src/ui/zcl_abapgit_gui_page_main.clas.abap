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

    DATA: mo_repo_overview TYPE REF TO zcl_abapgit_gui_repo_over,
          mv_repo_key      TYPE zif_abapgit_persistence=>ty_value.

    METHODS build_main_menu
      RETURNING VALUE(ro_menu) TYPE REF TO zcl_abapgit_html_toolbar.

    METHODS get_patch_page
      IMPORTING iv_getdata     TYPE clike
      RETURNING VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING   zcx_abapgit_exception.

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
      iv_txt = '<i class="icon icon-tools-solid"></i>'
      io_sub = zcl_abapgit_gui_chunk_lib=>advanced_submenu( )
    )->add(
      iv_txt = '<i class="icon icon-question-circle-solid"></i>'
      io_sub = zcl_abapgit_gui_chunk_lib=>help_submenu( ) ).

  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).
    ms_control-page_menu  = build_main_menu( ).
    ms_control-page_title = 'REPOSITORY LIST'.
  ENDMETHOD.


  METHOD render_content.

    DATA: lt_repos    TYPE zif_abapgit_definitions=>ty_repo_ref_tt,
          lx_error    TYPE REF TO zcx_abapgit_exception,
          li_tutorial TYPE REF TO zif_abapgit_gui_renderable,
          lo_repo     LIKE LINE OF lt_repos.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    gui_services( )->get_hotkeys_ctl( )->register_hotkeys( me ).

    IF mo_repo_overview IS INITIAL.
      CREATE OBJECT mo_repo_overview TYPE zcl_abapgit_gui_repo_over.
    ENDIF.

    ri_html->add( mo_repo_overview->zif_abapgit_gui_renderable~render( ) ).

    register_deferred_script( zcl_abapgit_gui_chunk_lib=>render_repo_palette( c_actions-select ) ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA: lv_key           TYPE zif_abapgit_persistence=>ty_value,
          li_repo_overview TYPE REF TO zif_abapgit_gui_renderable,
          li_main_page     TYPE REF TO zcl_abapgit_gui_page_main.

    CASE iv_action.
      WHEN c_actions-abapgit_home.
        CLEAR mv_repo_key.
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_actions-select.

        lv_key = iv_getdata.
        zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( lv_key ).

        TRY.
            zcl_abapgit_repo_srv=>get_instance( )->get( lv_key )->refresh( ).
          CATCH zcx_abapgit_exception ##NO_HANDLER.
        ENDTRY.

        mv_repo_key = lv_key.
        CREATE OBJECT ei_page TYPE zcl_abapgit_gui_page_view_repo
          EXPORTING
            iv_key = lv_key.
        ev_state = zcl_abapgit_gui=>c_event_state-new_page.

      WHEN zif_abapgit_definitions=>c_action-change_order_by.

        mo_repo_overview->set_order_by( zcl_abapgit_gui_chunk_lib=>parse_change_order_by( iv_getdata ) ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN zif_abapgit_definitions=>c_action-direction.

        mo_repo_overview->set_order_direction( zcl_abapgit_gui_chunk_lib=>parse_direction( iv_getdata ) ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_actions-apply_filter.

        mo_repo_overview->set_filter( it_postdata ).
        ev_state = zcl_abapgit_gui=>c_event_state-re_render.

      WHEN zif_abapgit_definitions=>c_action-go_patch.

        ei_page = get_patch_page( iv_getdata ).
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

  METHOD get_patch_page.

    DATA lv_key TYPE zif_abapgit_persistence=>ty_value.

    FIND FIRST OCCURRENCE OF '=' IN iv_getdata.
    IF sy-subrc <> 0. " Not found ? -> just repo key in params
      lv_key = iv_getdata.
    ELSE.
      zcl_abapgit_html_action_utils=>stage_decode(
        EXPORTING iv_getdata = iv_getdata
        IMPORTING ev_key     = lv_key ).
    ENDIF.

    CREATE OBJECT ri_page TYPE zcl_abapgit_gui_page_patch
      EXPORTING
        iv_key = lv_key.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Main'.

    ls_hotkey_action-description   = |abapGit settings|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-go_settings.
    ls_hotkey_action-hotkey = |x|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Add online repository|.
    ls_hotkey_action-action = zif_abapgit_definitions=>c_action-repo_newonline.
    ls_hotkey_action-hotkey = |n|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
ENDCLASS.
