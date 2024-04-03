CLASS zcl_abapgit_gui_page_whereused DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_abapgit_gui_component
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_abapgit_gui_page_title,
      zif_abapgit_gui_event_handler,
      zif_abapgit_gui_hotkeys,
      zif_abapgit_gui_menu_provider,
      zif_abapgit_gui_renderable,
      zif_abapgit_html_table.

    CLASS-METHODS create
      IMPORTING
        iv_package     TYPE devclass
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        iv_package TYPE devclass
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_action,
        refresh TYPE string VALUE 'refresh',
      END OF c_action.

    CONSTANTS c_title TYPE string VALUE 'Where Used'.
    DATA mv_package TYPE devclass.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_WHEREUSED IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mv_package = iv_package.

    IF zcl_abapgit_factory=>get_sap_package( iv_package )->exists( ) = abap_false.
      zcx_abapgit_exception=>raise( |Package { iv_package } does not exist| ).
    ENDIF.

  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_whereused.

    CREATE OBJECT lo_component
      EXPORTING
        iv_package = iv_package.

    ri_page = zcl_abapgit_gui_page_hoc=>create( lo_component ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-refresh.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = c_title.

    ls_hotkey_action-description = |Refresh|.
    ls_hotkey_action-action = c_action-refresh.
    ls_hotkey_action-hotkey = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    ro_toolbar = zcl_abapgit_html_toolbar=>create( )->add(
      iv_txt = 'Refresh'
      iv_act = c_action-refresh ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_title~get_page_title.
    rv_title = c_title.
  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->div(
      iv_class   = 'repo'
      iv_content = |Hello { mv_package }| ).

  ENDMETHOD.


  METHOD zif_abapgit_html_table~get_row_attrs.


  ENDMETHOD.


  METHOD zif_abapgit_html_table~render_cell.

  ENDMETHOD.
ENDCLASS.
