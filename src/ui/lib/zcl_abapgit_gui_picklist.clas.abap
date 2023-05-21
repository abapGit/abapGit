CLASS zcl_abapgit_gui_picklist DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_component
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_abapgit_gui_event_handler.
    INTERFACES zif_abapgit_gui_renderable.
    INTERFACES zif_abapgit_gui_page_title.

    METHODS constructor
      IMPORTING
        !it_list          TYPE STANDARD TABLE
        !iv_id            TYPE string OPTIONAL
        !iv_in_page       TYPE abap_bool DEFAULT abap_false
        !iv_title         TYPE string DEFAULT 'Choose from list'
        !iv_attr_name     TYPE abap_compname OPTIONAL
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
    METHODS is_fulfilled
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.
    METHODS id
      RETURNING
        VALUE(rv_id) TYPE string.
    METHODS is_in_page
      RETURNING
        VALUE(rv_yes) TYPE abap_bool.
    METHODS set_id
      IMPORTING
        iv_id        TYPE string
      RETURNING
        VALUE(ro_me) TYPE REF TO zcl_abapgit_gui_picklist.
    METHODS set_in_page
      IMPORTING
        iv_in_page   TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(ro_me) TYPE REF TO zcl_abapgit_gui_picklist.

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
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.
    DATA mr_list TYPE REF TO data.
    DATA mv_selected TYPE i.
    DATA mv_cancelled TYPE abap_bool.
    DATA mv_fulfilled TYPE abap_bool.
    DATA mv_attr_name TYPE abap_compname.
    DATA mi_item_renderer TYPE REF TO zif_abapgit_gui_render_item.
    DATA mv_in_page TYPE abap_bool.
    DATA mv_id TYPE string.
    DATA mv_title TYPE string.

    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form
      RAISING
        zcx_abapgit_exception.
    METHODS return_state
      RETURNING
        VALUE(rv_state) TYPE zif_abapgit_gui_event_handler=>ty_handling_result-state.

ENDCLASS.



CLASS zcl_abapgit_gui_picklist IMPLEMENTATION.


  METHOD constructor.

    FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.

    super->constructor( ).

    " copy contents of table to local scope
    CREATE DATA mr_list LIKE it_list.
    ASSIGN mr_list->* TO <lt_tab>.
    APPEND LINES OF it_list TO <lt_tab>.

    mv_attr_name = to_upper( iv_attr_name ).
    mi_item_renderer = ii_item_renderer.
    mv_in_page = iv_in_page.
    mv_id      = iv_id.
    mv_title   = iv_title.

    IF mi_item_renderer IS NOT BOUND AND mv_attr_name IS INITIAL.
      zcx_abapgit_exception=>raise( 'Renderer or attr name required' ).
    ENDIF.

    CREATE OBJECT mo_form_data.
    CREATE OBJECT mo_validation_log.
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
      iv_name     = c_radio_name
      iv_label    = mv_title ).

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


  METHOD id.
    rv_id = mv_id.
  ENDMETHOD.


  METHOD is_fulfilled.
    rv_yes = mv_fulfilled.
  ENDMETHOD.


  METHOD is_in_page.
    rv_yes = mv_in_page.
  ENDMETHOD.


  METHOD return_state.
    IF mv_in_page = abap_true.
      rv_state = zcl_abapgit_gui=>c_event_state-re_render.
    ELSE.
      rv_state = zcl_abapgit_gui=>c_event_state-go_back.
    ENDIF.
  ENDMETHOD.


  METHOD set_id.
    mv_id = iv_id.
    ro_me = me.
  ENDMETHOD.


  METHOD set_in_page.
    mv_in_page = iv_in_page.
    ro_me = me.
  ENDMETHOD.


  METHOD was_cancelled.
    rv_yes = mv_cancelled.
  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).
    mo_validation_log->clear( ).

    CASE ii_event->mv_action.
      WHEN c_event-back OR zif_abapgit_definitions=>c_action-go_back.
        " Handle go_back as a "graceful back" - implicit cancel by F3/ESC
        mv_fulfilled = abap_true.
        mv_cancelled = abap_true.
        rs_handled-state = return_state( ).
      WHEN c_event-choose.
        mv_selected = mo_form_data->get( c_radio_name ).
        IF mv_selected = 0.
          mo_validation_log->set(
            iv_key = c_radio_name
            iv_val = 'You have to select one item' ).
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          mv_fulfilled = abap_true.
          rs_handled-state = return_state( ).
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abapgit_gui_page_title~get_page_title.
    rv_title = mv_title.
  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.

    ri_html = zcl_abapgit_html=>create( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).
    register_handlers( ).

  ENDMETHOD.
ENDCLASS.
