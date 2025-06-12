CLASS zcl_abapgit_gui_page_flowcons DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler .
    INTERFACES zif_abapgit_gui_renderable .
    INTERFACES zif_abapgit_gui_menu_provider .

    CLASS-METHODS create
      IMPORTING
        ii_repo        TYPE REF TO zif_abapgit_repo_online
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception .

    METHODS constructor
      IMPORTING
        ii_repo TYPE REF TO zif_abapgit_repo_online
      RAISING
        zcx_abapgit_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_action,
        refresh             TYPE string VALUE 'refresh',
      END OF c_action .

    DATA mo_repo TYPE REF TO zif_abapgit_repo_online.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_FLOWCONS IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    mo_repo = ii_repo.
  ENDMETHOD.


  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_flowcons.

    CREATE OBJECT lo_component EXPORTING ii_repo = ii_repo.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Flow Consolidate'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-refresh.
* just re-render the page to trigger a refresh
        rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
      WHEN OTHERS.
* the back button is handled in the default router
        RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_menu_provider~get_menu.

    ro_toolbar = zcl_abapgit_html_toolbar=>create( 'toolbar-flow' ).

    ro_toolbar->add(
      iv_txt = 'Refresh'
      iv_act = c_action-refresh ).

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = zif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    DATA ls_consolidate TYPE zif_abapgit_flow_logic=>ty_consolidate.
    DATA lv_text       TYPE string.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( '<div class="repo-overview">' ).

    ri_html->add( zcl_abapgit_gui_chunk_lib=>render_repo_top(
      ii_repo                 = mo_repo
      iv_interactive_favorite = abap_false
      iv_show_commit          = abap_false
      iv_show_branch          = abap_false ) ).

    ls_consolidate = zcl_abapgit_flow_logic=>consolidate( ).

    LOOP AT ls_consolidate-errors INTO lv_text.
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_error( iv_error = lv_text ) ).
    ENDLOOP.
    LOOP AT ls_consolidate-warnings INTO lv_text.
      ri_html->add( zcl_abapgit_gui_chunk_lib=>render_warning_banner( lv_text ) ).
    ENDLOOP.

    ri_html->add( 'todo' ).

    ri_html->add( '</div>' ).

  ENDMETHOD.
ENDCLASS.
