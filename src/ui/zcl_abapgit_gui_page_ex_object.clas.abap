CLASS zcl_abapgit_gui_page_ex_object DEFINITION
  PUBLIC
  INHERITING FROM zcl_abapgit_gui_page
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      RAISING
        zcx_abapgit_exception.

    METHODS zif_abapgit_gui_event_handler~on_event REDEFINITION.

  PROTECTED SECTION.
    METHODS render_content REDEFINITION.
  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_id,
        object_type TYPE string VALUE 'object_type',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_event,
        go_back            TYPE string VALUE 'go-back',
        save               TYPE string VALUE 'save',
        choose_object_type TYPE string VALUE 'choose-object-type',
      END OF c_event.


    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
ENDCLASS.



CLASS zcl_abapgit_gui_page_ex_object IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    CREATE OBJECT mo_form_data.
    ms_control-page_title = 'Export Object as ZIP'.
  ENDMETHOD.

  METHOD render_content.

    DATA lo_form TYPE REF TO zcl_abapgit_html_form.

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.
    lo_form = zcl_abapgit_html_form=>create( ).

    lo_form->text(
      iv_name        = c_id-object_type
      iv_required    = abap_true
      iv_upper_case  = abap_true
      iv_side_action = c_event-choose_object_type
      iv_label       = 'Object Type' ).

    lo_form->command(
      iv_label    = 'Save'
      iv_cmd_type = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = c_event-save ).

    lo_form->command(
      iv_label  = 'Back'
      iv_action = c_event-go_back ).

    ri_html->add( lo_form->render( mo_form_data ) ).
  ENDMETHOD.

  METHOD zif_abapgit_gui_event_handler~on_event.
    CASE ii_event->mv_action.
      WHEN c_event-go_back.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_event-save.

      WHEN c_event-choose_object_type.

        mo_form_data->set(
          iv_key = 'object_type'
          iv_val = zcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'TADIR-OBJECT' ) ).

        IF mo_form_data->get( 'object_type' ) IS NOT INITIAL.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
