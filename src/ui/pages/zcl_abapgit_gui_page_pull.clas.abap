CLASS zcl_abapgit_gui_page_pull DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_menu_provider.
    INTERFACES zif_abapgit_gui_renderable.

    CONSTANTS: BEGIN OF c_action,
                 back    TYPE string VALUE 'back',
                 refresh TYPE string VALUE 'refresh',
               END OF c_action.

    CLASS-METHODS create
      IMPORTING
        io_repo        TYPE REF TO zcl_abapgit_repo
        ii_obj_filter  TYPE REF TO zif_abapgit_object_filter OPTIONAL
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      IMPORTING
        io_repo       TYPE REF TO zcl_abapgit_repo
        ii_obj_filter TYPE REF TO zif_abapgit_object_filter OPTIONAL
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mo_repo       TYPE REF TO zcl_abapgit_repo.
    DATA mi_obj_filter TYPE REF TO zif_abapgit_object_filter.

ENDCLASS.


CLASS zcl_abapgit_gui_page_pull IMPLEMENTATION.

  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_stage.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo       = io_repo
        ii_obj_filter = ii_obj_filter.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Pull'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.

  METHOD zif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar EXPORTING iv_id = 'toolbar-main'.

    ro_toolbar->add(
      iv_txt = 'Refresh'
      iv_act = c_action-refresh ).

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = c_action-back ).

    ro_toolbar->add(
      iv_txt = zcl_abapgit_gui_buttons=>repo_list( )
      iv_act = zif_abapgit_definitions=>c_action-abapgit_home ).

  ENDMETHOD.

  METHOD constructor.

    super->constructor( ).

    mo_repo       = io_repo.
    mi_obj_filter = ii_obj_filter.

  ENDMETHOD.

  METHOD zif_abapgit_gui_event_handler~on_event.
    BREAK-POINT.
  ENDMETHOD.

  METHOD zif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    ri_html->add( '<div class="repo-overview">' ).
    ri_html->add( 'hello world' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.

ENDCLASS.
