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

    CONSTANTS c_radio_name TYPE string VALUE 'radio'.

    DATA mo_form TYPE REF TO zcl_abapgit_html_form.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_util TYPE REF TO zcl_abapgit_html_form_utils.
    DATA mi_callback TYPE REF TO zif_abapgit_gui_page_callback.
    DATA mr_list TYPE REF TO data.

    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_DECIDE_LI IMPLEMENTATION.


  METHOD constructor.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    super->constructor( ).

* copy contents of table to local scope
    CREATE DATA mr_list LIKE it_list.
    ASSIGN mr_list->* TO <tab>.
    APPEND LINES OF it_list TO <tab>.

    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).

    mi_callback = ii_callback.

  ENDMETHOD.


  METHOD get_form_schema.

    FIELD-SYMBOLS <list> TYPE ANY TABLE.
    FIELD-SYMBOLS <val> TYPE any.
    FIELD-SYMBOLS <row> TYPE any.

    ro_form = zcl_abapgit_html_form=>create( ).

    ro_form->radio(
      iv_name  = c_radio_name
      iv_label = 'Choose from list' ).

    ASSIGN mr_list->* TO <list>.
    LOOP AT <list> ASSIGNING <row>.
* todo, component/title configuration via constructor
      ASSIGN COMPONENT 'TITLE' OF STRUCTURE <row> TO <val>.
      ASSERT sy-subrc = 0.
      ro_form->option(
        iv_label = <val>
        iv_value = |{ sy-tabix }| ).
    ENDLOOP.

    ro_form->command(
      iv_label    = 'Choose'
      iv_cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = c_event-choose
    )->command(
      iv_label    = 'Back'
      iv_action   = c_event-back ).

  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( mo_form->render(
      io_values = mo_form_data ) ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA lv_index TYPE i.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-back.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
      WHEN c_event-choose.
        lv_index = mo_form_data->get( c_radio_name ).
        ASSERT lv_index > 0.
        ASSIGN mr_list->* TO <table>.
        mi_callback->row_selected(
          it_table = <table>
          iv_index = lv_index ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
