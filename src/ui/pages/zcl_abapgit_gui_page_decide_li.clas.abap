CLASS zcl_abapgit_gui_page_decide_li DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_list     TYPE STANDARD TABLE
        ii_callback TYPE REF TO zif_abapgit_gui_page_callback
      RAISING
        zcx_abapgit_exception.

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

    DATA mi_callback TYPE REF TO zif_abapgit_gui_page_callback.
    DATA mr_list TYPE REF TO data.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DECIDE_LI IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    mi_callback = ii_callback.
    GET REFERENCE OF it_list INTO mr_list.

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
* todo: call callback
        BREAK-POINT.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
