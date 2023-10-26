CLASS zcl_abapgit_gui_page_flow DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.

    CLASS-METHODS create
      RETURNING
        VALUE(ri_page) TYPE REF TO zif_abapgit_gui_renderable
      RAISING
        zcx_abapgit_exception.

    METHODS constructor
      RAISING
        zcx_abapgit_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_abapgit_gui_page_flow IMPLEMENTATION.

  METHOD create.

    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_flow.

    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Flow'
      ii_child_component = lo_component ).

  ENDMETHOD.

  METHOD constructor.
    super->constructor( ).

  ENDMETHOD.

  METHOD zif_abapgit_gui_event_handler~on_event.
    RETURN. " todo, implement method
  ENDMETHOD.

  METHOD zif_abapgit_gui_renderable~render.
    RETURN. " todo, implement method
  ENDMETHOD.

ENDCLASS.
