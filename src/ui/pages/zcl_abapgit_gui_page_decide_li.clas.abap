CLASS zcl_abapgit_gui_page_decide_li DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        ii_list TYPE STANDARD TABLE
      RAISING
        zcx_abapgit_exception.

    METHODS get_selected
      RETURNING
        VALUE(rv_index) TYPE i.

    METHODS zif_abapgit_gui_event_handler~on_event
        REDEFINITION .
  PROTECTED SECTION.

    METHODS render_content
        REDEFINITION .
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_event,
        back   TYPE string VALUE 'back',
        choose TYPE string VALUE 'choose',
      END OF c_event .

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DECIDE_LI IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

* todo

  ENDMETHOD.


  METHOD get_selected.

* todo

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( `hello world` ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_event-back.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
      WHEN c_event-choose.
        BREAK-POINT.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
