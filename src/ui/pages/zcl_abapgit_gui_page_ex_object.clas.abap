CLASS zcl_abapgit_gui_page_ex_object DEFINITION
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
    CONSTANTS:
      BEGIN OF c_id,
        object_type TYPE string VALUE 'object_type',
        object_name TYPE string VALUE 'object_name',
        only_main   TYPE string VALUE 'only_main',
      END OF c_id.

    CONSTANTS:
      BEGIN OF c_event,
        go_back            TYPE string VALUE 'go-back',
        export             TYPE string VALUE 'export',
        choose_object_type TYPE string VALUE 'choose-object-type',
      END OF c_event.


    DATA mo_form TYPE REF TO zcl_abapgit_html_form.
    DATA mo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA mo_validation_log TYPE REF TO zcl_abapgit_string_map.
    DATA mo_form_util TYPE REF TO zcl_abapgit_html_form_utils.

    METHODS get_form_schema
      RETURNING
        VALUE(ro_form) TYPE REF TO zcl_abapgit_html_form.

    METHODS export_object
      RAISING
        zcx_abapgit_exception.
ENDCLASS.



CLASS ZCL_ABAPGIT_GUI_PAGE_EX_OBJECT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_validation_log.

    CREATE OBJECT mo_form_data.
    mo_form_data->set(
      iv_key = c_id-only_main
      iv_val = abap_true ).

    mo_form = get_form_schema( ).
    mo_form_util = zcl_abapgit_html_form_utils=>create( mo_form ).
  ENDMETHOD.


  METHOD create.
    DATA lo_component TYPE REF TO zcl_abapgit_gui_page_ex_object.
    CREATE OBJECT lo_component.

    ri_page = zcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Export Objects to Files'
      ii_child_component = lo_component ).
  ENDMETHOD.


  METHOD export_object.
    DATA lv_object_type TYPE trobjtype.
    DATA lt_names TYPE STANDARD TABLE OF sobj_name WITH DEFAULT KEY.
    DATA lv_name LIKE LINE OF lt_names.
    DATA lv_list TYPE string.
    DATA lv_only_main TYPE abap_bool.

    lv_object_type = mo_form_data->get( c_id-object_type ).
    lv_list = mo_form_data->get( c_id-object_name ).
    lv_only_main = mo_form_data->get( c_id-only_main ).

    REPLACE ALL OCCURRENCES OF |\r| IN lv_list WITH ''.
    SPLIT lv_list AT |\n| INTO TABLE lt_names.

    LOOP AT lt_names INTO lv_name.
      IF lv_name IS INITIAL.
        CONTINUE.
      ENDIF.
      zcl_abapgit_zip=>export_object(
        iv_main_language_only = lv_only_main
        iv_object_type        = lv_object_type
        iv_object_name        = lv_name ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_form_schema.
    ro_form = zcl_abapgit_html_form=>create( iv_form_id = 'export-object-to-files' ).

    ro_form->text(
      iv_label       = 'Object Type'
      iv_name        = c_id-object_type
      iv_required    = abap_true
      iv_upper_case  = abap_true
      iv_side_action = c_event-choose_object_type ).

    ro_form->textarea(
      iv_label       = 'Object Names'
      iv_name        = c_id-object_name
      iv_required    = abap_true
      iv_placeholder = 'One object name per line'
      iv_upper_case  = abap_true ).

    ro_form->checkbox(
      iv_label = 'Only Main Language'
      iv_name  = c_id-only_main ).

    ro_form->command(
      iv_label       = 'Export'
      iv_cmd_type    = zif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-export
    )->command(
      iv_label       = 'Back'
      iv_action      = c_event-go_back ).
  ENDMETHOD.


  METHOD zif_abapgit_gui_event_handler~on_event.
    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-go_back.

        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_event-export.

        export_object( ).
        MESSAGE 'Object successfully exported' TYPE 'S'.
        rs_handled-state = zcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_event-choose_object_type.

        mo_form_data->set(
          iv_key = c_id-object_type
          iv_val = zcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'TADIR-OBJECT' ) ).

        IF mo_form_data->get( c_id-object_type ) IS NOT INITIAL.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          rs_handled-state = zcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_abapgit_gui_renderable~render.
    register_handlers( ).

    CREATE OBJECT ri_html TYPE zcl_abapgit_html.

    ri_html->add( '<div class="form-container">' ).
    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).
    ri_html->add( '</div>' ).
  ENDMETHOD.
ENDCLASS.
