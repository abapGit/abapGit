CLASS zcl_abapgit_gui_picklist DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.

    METHODS constructor
      IMPORTING
        !it_list TYPE STANDARD TABLE
        !iv_attr_name TYPE abap_compname OPTIONAL
        !ii_item_renderer TYPE REF TO zif_abapgit_gui_render_item OPTIONAL
      RAISING
        zcx_abapgit_exception.
    METHODS get_result_idx
      RETURNING
        VALUE(rv_index) TYPE i.
    METHODS get_result_item
      CHANGING
        !cs_selected TYPE any.
    METHODS was_cancelled
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF c_event,
        back   TYPE string VALUE 'back',
        choose TYPE string VALUE 'choose',
      END OF c_event.

    CONSTANTS c_radio_name TYPE string VALUE 'radio'.

    DATA mo_form TYPE REF TO zcl_abapgit_html_form.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_util TYPE REF TO zcl_abapgit_html_form_utils.
    DATA mr_list TYPE REF TO data.
    DATA mv_selected TYPE i.
    DATA mv_cancelled TYPE abap_bool.
    DATA mv_attr_name TYPE abap_compname.
    DATA mi_item_renderer TYPE REF TO zif_abapgit_gui_render_item.

    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form
      RAISING
        zcx_abapgit_exception.

ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PICKLIST IMPLEMENTATION.


  METHOD constructor.

    FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.

    super->constructor( ).

    " copy contents of table to local scope
    CREATE DATA mr_list LIKE it_list.
    ASSIGN mr_list->* TO <lt_tab>.
    APPEND LINES OF it_list TO <lt_tab>.

    mv_attr_name = to_upper( iv_attr_name ).
    mi_item_renderer = ii_item_renderer.

    IF mi_item_renderer IS NOT BOUND AND mv_attr_name IS INITIAL.
      zcx_abapgit_exception=>raise( 'Renderer or attr name required' ).
    ENDIF.

    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).

  ENDMETHOD.


  METHOD get_form_schema.

    FIELD-SYMBOLS <lt_list> TYPE ANY TABLE.
    FIELD-SYMBOLS <lv_val> TYPE any.
    FIELD-SYMBOLS <ls_row> TYPE any.
    DATA lv_index TYPE i.
    DATA lv_label TYPE string.

    ro_form = zcl_abapgit_html_form=>create( ).

    ro_form->radio(
      iv_name  = c_radio_name
      iv_label = 'Choose from list' ).

    ASSIGN mr_list->* TO <lt_list>.
    LOOP AT <lt_list> ASSIGNING <ls_row>.
      lv_index = sy-tabix.

      IF mv_attr_name IS NOT INITIAL.
        ASSIGN COMPONENT mv_attr_name OF STRUCTURE <ls_row> TO <lv_val>.
        ASSERT sy-subrc = 0.
        lv_label = <lv_val>.
      ELSEIF mi_item_renderer IS BOUND.
        lv_label = mi_item_renderer->render(
          iv_item  = <ls_row>
          iv_index = lv_index )->render( ).
      ENDIF.

      ro_form->option(
        iv_label = lv_label
        iv_value = |{ lv_index }| ).

    ENDLOOP.

    ro_form->command(
      iv_label    = 'Choose'
      iv_cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = c_event-choose
    )->command(
      iv_label    = 'Back'
      iv_action   = c_event-back ).

  ENDMETHOD.


  METHOD get_result_idx.
    rv_index = mv_selected.
  ENDMETHOD.


  METHOD get_result_item.

    FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.

    CLEAR cs_selected.

    IF mv_selected > 0.
      ASSIGN mr_list->* TO <lt_tab>.
      READ TABLE <lt_tab> INDEX mv_selected INTO cs_selected.
      ASSERT sy-subrc = 0.
    ENDIF.

  ENDMETHOD.


  METHOD was_cancelled.
    rv_yes = mv_cancelled.
  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    DATA lv_index TYPE i.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-back.
        mv_cancelled = abap_true.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
      WHEN c_event-choose.
        mv_selected = mo_form_data->get( c_radio_name ).
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( mo_form->render( io_values = mo_form_data ) ).
    register_handlers( ).

  ENDMETHOD.
ENDCLASS.
