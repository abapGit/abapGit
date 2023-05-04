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

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    super->constructor( ).

    mi_callback = ii_callback.

* copy contents of table to local scope
    CREATE DATA mr_list LIKE it_list.
    ASSIGN mr_list->* TO <tab>.
    APPEND LINES OF it_list TO <tab>.

  ENDMETHOD.


  METHOD render_content.

    FIELD-SYMBOLS <list> TYPE ANY TABLE.
    FIELD-SYMBOLS <val> TYPE ANY.
    FIELD-SYMBOLS <row> TYPE ANY.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    DATA(lo_form) = zcl_abapgit_html_form=>create( ).

    lo_form->radio(
      iv_name  = 'list'
      iv_label = 'Choose from list' ).

    ASSIGN mr_list->* TO <list>.
    LOOP AT <list> ASSIGNING <row>.
* todo, component configuration via constructor
      ASSIGN COMPONENT 'TITLE' OF STRUCTURE <row> TO <val>.
      ASSERT sy-subrc = 0.
      lo_form->option(
        iv_label = <val>
        iv_value = |{ sy-tabix }| ).
    ENDLOOP.

    ri_html->add( lo_form->render( io_values = NEW #( ) ) ).

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
