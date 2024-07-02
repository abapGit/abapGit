CLASS zcl_abapgit_gui_page_template DEFINITION
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
      " TODO: remove table interface if not used on the page
      zif_abapgit_html_table.

    CLASS-METHODS create
      " TODO: page parameters, e.g. ref to repo
*      IMPORTING
*        ii_repo TYPE REF TO zif_abapgit_repo
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
*      IMPORTING
*        ii_repo TYPE REF TO zif_abapgit_repo
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        " TODO: List of in-page actions
        refresh TYPE string VALUE 'refresh',
      END OF c_action.

    CONSTANTS c_title TYPE string VALUE 'Page Template'. " TODO: define page title

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_TEMPLATE IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    " ...
  ENDMETHOD.


  METHOD create.

    " TODO: replace with name of class (self)
    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_template.

    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create( lo_component ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      " TODO: action handling, refresh is just an example
      WHEN c_action-refresh.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN 'xyz'.
        ASSERT 1 = 1.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = c_title.

    " TODO: define hotkeys

    ls_hotkey_action-description = 'Refresh'.
    ls_hotkey_action-action      = c_action-refresh.
    ls_hotkey_action-hotkey      = 'r'.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    " TODO: top level menu

    ro_toolbar = zcl_abapgit_html_toolbar=>create( )->add(
      iv_txt = 'Refresh'
      iv_act = c_action-refresh ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_title~get_page_title.
    rv_title = c_title. " Fixed title
  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    ri_html = zcl_abapgit_html=>create( ).

    " TODO: do your rendering

    ri_html->div(
      iv_class   = 'template'
      iv_content = 'Hello!' ).

  ENDMETHOD.


  METHOD zif_abapgit_html_table~get_row_attrs.
    " ... in case of table on the page, otherwise remove the interface at all
  ENDMETHOD.


  METHOD zif_abapgit_html_table~render_cell.
    " ... in case of table on the page, otherwise remove the interface at all
  ENDMETHOD.
ENDCLASS.
