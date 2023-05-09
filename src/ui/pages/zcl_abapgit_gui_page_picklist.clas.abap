CLASS zcl_abapgit_gui_page_picklist DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM zcl_abapgit_gui_page
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        it_list TYPE STANDARD TABLE
      RAISING
        zcx_abapgit_exception.

    METHODS zif_abapgit_gui_event_handler~on_event
        REDEFINITION .

    METHODS get_result RETURNING VALUE(rv_index) TYPE i.

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
    DATA mr_list TYPE REF TO data.

    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_PICKLIST IMPLEMENTATION.


  METHOD constructor.

    FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.

    super->constructor( ).

* copy contents of table to local scope
    CREATE DATA mr_list LIKE it_list.
    ASSIGN mr_list->* TO <lt_tab>.
    APPEND LINES OF it_list TO <lt_tab>.

    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).

  ENDMETHOD.


  METHOD get_form_schema.

    FIELD-SYMBOLS <lt_list> TYPE ANY TABLE.
    FIELD-SYMBOLS <lv_val> TYPE any.
    FIELD-SYMBOLS <ls_row> TYPE any.

    ro_form = zcl_abapgit_html_form=>create( ).

    ro_form->radio(
      iv_name  = c_radio_name
      iv_label = 'Choose from list' ).

    ASSIGN mr_list->* TO <lt_list>.
    LOOP AT <lt_list> ASSIGNING <ls_row>.
      ASSIGN COMPONENT 'TITLE' OF STRUCTURE <ls_row> TO <lv_val>.
      ASSERT sy-subrc = 0.
      ro_form->option(
        iv_label = <lv_val>
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


  METHOD get_result.
    ASSERT 1 = 'todo'.
  ENDMETHOD.


  METHOD render_content.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( mo_form->render( io_values = mo_form_data ) ).

  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA lv_index TYPE i.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-back.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
      WHEN c_event-choose.
        lv_index = mo_form_data->get( c_radio_name ).
* todo
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
